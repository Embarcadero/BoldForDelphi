{ Global compiler directives }
{$include bold.inc}
unit BoldCustomCheckListBox;

interface

uses
  BoldListHandleFollower,
  BoldListListControlPack,
  BoldAbstractListHandle,
  BoldControlPack,
  BoldStringControlPack,
  BoldCheckBoxStateControlPack,
  BoldControllerListControlPack,
  BoldElements,
  BoldSystem,
  Classes,
  Controls,
  CheckLst,
  BoldDefs;

const
  CHECKBOXFOLLOWER_INDEX = 0;
  STRINGFOLLOWER_INDEX = 1;

type
  {forward declarations}
  TBoldCustomCheckListBox = class;

  { TBoldCustomCheckListBox }
  TBoldCustomCheckListBox = class(TCheckListBox, IBoldOCLComponent)
  private
    FAlignment: TAlignment;
    fListHandleFollower: TBoldlistHandleFollower;
    fBoldListProperties: TBoldListAsFollowerListController;
    fBoldRowStringProperties: TBoldStringFollowerController;
    fBoldRowCheckBoxProperties: TBoldCheckBoxStateFollowerController;
    fControllerList: TBoldControllerList;
    procedure SetBoldRowStringProperties(const Value: TBoldStringFollowerController);
    function GetSelected(Index: integer): Boolean;
    function GetSelectedCount: integer;
    procedure SetSelected(Index: integer; const Value: Boolean);
    function GetCurrentBoldElement: TBoldElement;
    function GetCurrentBoldObject: TBoldObject;
    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetExpression: TBoldExpression;
    function GetVariableList: TBoldExternalVariablelist;
    function GetBoldlistHandle: TBoldAbstractListHandle;
    procedure SetBoldListHandle(const Value: TBoldAbstractListHandle);
    function GetMutableList: TBoldList;
    procedure SetAlignment(const Value: TAlignment);
    function GetBoldHandleIndexLock: Boolean;
    procedure SetBoldHandleIndexLock(const Value: Boolean);
    procedure SetBoldRowCheckBoxProperties(
      const Value: TBoldCheckBoxStateFollowerController);
    procedure SetBoldListProperties(
      const Value: TBoldListAsFollowerListController);
  protected
    procedure SetItemIndex(const Value: integer); override;
    procedure _DisplayCheckBox(Follower: TBoldFollower);
    procedure _DisplayString(Follower: TBoldFollower);
    procedure Click; override;
    procedure DblClick; override;
    procedure _ListInsertItem(Index: integer; Follower: TBoldFollower);
    procedure _ListDeleteItem(Index: integer; Follower: TBoldFollower);
    procedure _ListBeforeMakeUpToDate(Follower: TBoldFollower);
    procedure _ListAfterMakeUpToDate(Follower: TBoldFollower);
    {Drag & Drop methods}
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure ClickCheck; override;
    property BoldListProperties: TBoldListAsFollowerListController read FBoldListProperties write SetBoldListProperties;
    property BoldListHandle: TBoldAbstractListHandle read GetBoldlistHandle write SetBoldListHandle;
    property BoldRowStringProperties: TBoldStringFollowerController read fBoldRowStringProperties write SetBoldRowStringProperties;
    property BoldRowCheckBoxProperties: TBoldCheckBoxStateFollowerController read fBoldRowCheckBoxProperties write SetBoldRowCheckBoxProperties;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property BoldHandleIndexLock: Boolean read GetBoldHandleIndexLock write SetBoldHandleIndexLock default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: integer); override;
    property CurrentBoldElement: TBoldElement read GetCurrentBoldElement;
    property CurrentBoldObject: TBoldObject read GetCurrentBoldObject;
    property SelectedCount: integer read GetSelectedCount;
    property Selected[Index: integer]: Boolean read GetSelected write SetSelected;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property MutableList: TBoldList read GetMutableList;
  end;

implementation

uses
  BoldControlPackDefs,
  BoldGUI,
  BoldAFP,
  Forms,
  Sysutils;

{ TBoldCustomCheckListBox }

constructor TBoldCustomCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBoldRowStringProperties := TBoldStringFollowerController.Create(self);
  fBoldRowStringProperties.OnGetContextType := GetContextType;
  fBoldRowCheckBoxProperties := TBoldCheckBoxStateFollowerController.Create(self);
  fBoldRowCheckBoxProperties.OnGetContextType := GetContextType;
  fControllerList := TBoldControllerList.Create(self);
  fControllerList.Add(fBoldRowCheckBoxProperties);
  fControllerLIst.Add(fBoldRowStringProperties);

  fBoldRowStringProperties.AfterMakeUptoDate := _DisplayString;
  fBoldRowCheckBoxProperties.AfterMakeUpToDate := _DisplayCheckBox;

  fBoldListProperties := TBoldListAsFollowerListController.Create(self, fControllerList);
  fBoldListProperties.OnGetContextType := GetContextType;
  fBoldListProperties.AfterMakeUptoDate := _ListAfterMakeUpToDate;
  fBoldListProperties.BeforeMakeUptoDate := _ListBeforeMakeUpToDate;
  fBoldlistProperties.OnAfterInsertItem := _ListInsertItem;
  fBoldListProperties.OnAfterDeleteItem := _ListDeleteItem;
  fListHandleFollower := TBoldListHandleFollower.Create(self, fBoldListProperties);
end;

destructor TBoldCustomCheckListBox.Destroy;
begin
  FreeAndNil(fListHandleFollower);
  FreeAndNil(fBoldListProperties);
  FreeAndNil(fControllerList);
  inherited;
end;

procedure TBoldCustomCheckListBox.SetBoldRowStringProperties(const Value: TBoldStringFollowerController);
begin
  fBoldRowStringProperties.Assign(Value);
end;

function TBoldCustomCheckListBox.GetSelected(Index: integer): Boolean;
begin
  Result := inherited Selected[index];
end;

function TBoldCustomCheckListBox.GetSelectedCount: integer;
var
  i: integer;
begin
  Result := 0;
  if not MultiSelect then
    Result := -1
  else
    for i:= 0 to Items.Count - 1 do
      if Selected[i] then
      begin
        Result := 1;
        Break;
      end;
end;

procedure TBoldCustomCheckListBox.SetSelected(Index: integer;
  const Value: Boolean);
var
  Follower: TBoldFollower;
begin
  inherited Selected[Index] := Value;
  Follower := fListHandleFollower.Follower;
  if fBoldListProperties.GetSelected(Follower, Index) <> Value then
  begin
    if Value and not MultiSelect then
      fBoldListProperties.SelectAll(Follower, False);
    fBoldListProperties.SetSelected(Follower, Index, Value);
  end;
end;

procedure TBoldCustomCheckListBox.SetItemIndex(const Value: integer);
begin
  fListHandleFollower.SetFollowerIndex(Value);
  inherited ItemIndex := Value;
end;

function TBoldCustomCheckListBox.GetCurrentBoldElement: TBoldElement;
var
  SubFollower: TBoldFollower;
begin
  SubFollower := fListHandleFollower.Follower.SubFollowers[ItemIndex];
  if Assigned(SubFollower) then
    Result := SubFollower.Element
  else
    Result := nil;
end;

function TBoldCustomCheckListBox.GetCurrentBoldObject: TBoldObject;
begin
{$IFDEF BOLDCOMCLIENT}
  CurrentBoldElement.QueryInterface(IBoldObject, result);
{$ELSE}
  if CurrentBoldElement is TBoldObject then
    Result := CurrentBoldElement as TBoldObject
  else
    Result := nil;
{$ENDIF}
end;

procedure TBoldCustomCheckListBox.Click;
begin
  inherited;
  fListHandleFollower.SetFollowerIndex(ItemIndex);
end;

procedure TBoldCustomCheckListBox.DblClick;
var
  AutoForm: TForm;
begin
  if Assigned(OnDblClick) then
    inherited
  else
    if BoldListProperties.DefaultDblClick and Assigned(CurrentBoldElement) then
    begin
      {$IFDEF BOLDCOMCLIENT}
      Autoform := nil;
      {$ELSE}
      AutoForm := AutoFormProviderRegistry.FormForElement(CurrentBoldElement);
      {$ENDIF}
      if assigned(AutoForm) then
        AutoForm.Show;
    end;
end;

function TBoldCustomCheckListBox.GetExpression: TBoldExpression;
begin
  result := BoldRowStringProperties.Expression;
end;

function TBoldCustomCheckListBox.GetVariableList: TBoldExternalVariablelist;
begin
  Result := BoldListProperties.VariableList;
end;

procedure TBoldCustomCheckListBox.SetExpression(const Value: TBoldExpression);
begin
  BoldRowStringProperties.Expression := Value;
end;

function TBoldCustomCheckListBox.GetBoldlistHandle: TBoldAbstractListHandle;
begin
  Result := fListHandleFollower.BoldHandle;
end;

procedure TBoldCustomCheckListBox.SetBoldListHandle(
  const Value: TBoldAbstractListHandle);
begin
  fListHandleFollower.BoldHandle := Value;
end;

procedure TBoldCustomCheckListBox._ListDeleteItem(Index: Integer; Follower: TBoldFollower);
begin
  Items.Delete(Index);
end;

procedure TBoldCustomCheckListBox._ListInsertItem(Index: integer; Follower: TBoldFollower);
begin
  Items.Insert(Follower.Index, '');
end;

procedure TBoldCustomCheckListBox._DisplayString(Follower: TBoldFollower);
var
  index: integer;
begin
  index := Follower.OwningFollower.index;
  if (index > -1) and (index < Items.Count) then
  begin
    Items[Index] := TBoldStringFollowerController(Follower.Controller).GetCurrentAsString(Follower);
  end;
end;

procedure TBoldCustomCheckListBox._DisplayCheckBox(Follower: TBoldFollower);
var
  index: integer;
begin
  index := Follower.OwningFollower.index;
  if (index > -1) and (index < Items.Count) then
  begin
    State[Index] := TBoldCheckBoxStateFollowerController(Follower.Controller).GetCurrentAsCheckBoxState(Follower);
  end;
end;

procedure TBoldCustomCheckListBox._ListAfterMakeUpToDate(
  Follower: TBoldFollower);
begin
  Items.EndUpdate;
  ItemIndex := Follower.CurrentIndex;
end;

procedure TBoldCustomCheckListBox._ListBeforeMakeUpToDate(
  Follower: TBoldFollower);
begin
  if Assigned(BoldListHandle) and Assigned(BoldListHandle.List) then
    BoldListHandle.List.EnsureRange(0, BoldListHandle.List.Count - 1);
  Items.BeginUpdate;
end;

function TBoldCustomCheckListBox.GetContextType: TBoldElementTypeInfo;
begin
  if Assigned(BoldListHandle) then
    result := BoldListHandle.StaticBoldType
  else
    result := nil;
end;

procedure TBoldCustomCheckListBox.ClickCheck;
var
  CheckBoxFollower: TBoldFollower;
begin
  inherited;
  if not (csDesigning in ComponentState) and (ItemIndex <> - 1) then
  begin
    CheckBoxFollower := fListHandleFollower.Follower.SubFollowers[ItemIndex].SubFollowers[CHECKBOXFOLLOWER_INDEX];
    TBoldCheckBoxStateFollowerController(CheckBoxFollower.Controller).SetAsCheckBoxState(State[ItemIndex], CheckBoxFollower);
  end;
end;

procedure TBoldCustomCheckListBox.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  fBoldListProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomCheckListBox.DoStartDrag(var DragObject: TDragObject);
begin
  fBoldListProperties.StartDrag(fListHandleFollower.Follower);
  inherited DoStartDrag(DragObject);
end;

function TBoldCustomCheckListBox.GetMutableList: TBoldList;
begin
  if Assigned(BoldListHandle) then
    Result := BoldListHandle.MutableList
  else
    Result := nil;
end;

procedure TBoldCustomCheckListBox.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Assigned(OnDragOver)
    or (fBoldListProperties.DropMode = bdpNone)
    or ((Source = Self) and (not fBoldListProperties.InternalDrag)) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := fBoldListProperties.DragOver(fListHandleFollower.Follower, MutableList, ItemAtPos(Point(X, Y), False));
end;

procedure TBoldCustomCheckListBox.DragDrop(Source: TObject; X, Y: integer);
begin
  if Assigned(OnDragDrop) then
  begin
    if BoldGuiHandler.ActivateTargetFormOnDrop then
      BoldGUIHandler.TryToFocusHostingForm(self);
    inherited DragDrop(Source, X, Y);
  end
  else
    fBoldListProperties.DragDrop(fListHandleFollower.Follower, MutableList, ItemAtPos(Point(X, Y), False));
end;

procedure TBoldCustomCheckListBox.SetAlignment(const Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

function TBoldCustomCheckListBox.GetBoldHandleIndexLock: Boolean;
begin
  Result := fListHandleFollower.HandleIndexLock;
end;

procedure TBoldCustomCheckListBox.SetBoldHandleIndexLock(const Value: Boolean);
begin
  fListHandleFollower.HandleIndexLock := Value;
end;

procedure TBoldCustomCheckListBox.SetBoldRowCheckBoxProperties(
  const Value: TBoldCheckBoxStateFollowerController);
begin
  fBoldRowCheckBoxProperties.Assign(Value);
end;

procedure TBoldCustomCheckListBox.SetBoldListProperties(
  const Value: TBoldListAsFollowerListController);
begin
  FBoldListProperties.Assign(Value);
end;

end.