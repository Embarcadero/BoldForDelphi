
{ Global compiler directives }
{$include bold.inc}
unit BoldListBoxCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  // VCL
  Classes,
  Controls,
  Graphics,
  Menus,
  Messages,
  StdCtrls,
  Windows,

  // Bold
  {$IFNDEF BOLDCOMCLIENT}
  BoldComObjectSpace_TLB,
  BoldAFP,
  {$ENDIF}
  BoldAbstractListHandleCom,
  BoldClientElementSupport,
  BoldComObjectSpace_TLB,
  BoldControlPackCom,
  BoldListHandleFollowerCom,
  BoldListListControlPackCom,
  BoldStringControlPackCom;

type
  {Forward declarations of all classes}
  TBoldCustomListBoxCom = class;
  TBoldListBoxCom = class;

  {---TBoldCustomListBoxCom---}
  TBoldCustomListBoxCom = class(TCustomListBox, IBoldOCLComponentCom)
  private
    {Bold stuff}
    FAlignment: TAlignment;
    fHandleFollower: TBoldListHandleFollowerCom;
    fBoldProperties: TBoldListAsFollowerListControllerCom;
    fBoldRowProperties: TBoldStringFollowerControllerCom;

    function GetContextType: IBoldElementTypeInfo;
    procedure SetExpression(Expression: String);
    function GetExpression: String;
    function GetVariableList: IBoldExternalVariableList;

    function GetBoldHandle: TBoldAbstractListHandleCom;
    procedure SetBoldHandle(value: TBoldAbstractListHandleCom);
    function GetFollower: TBoldFollowerCom;
    procedure SetBoldProperties(Value: TBoldListAsFollowerListControllerCom);
    procedure SetRowProperties(Value: TBoldStringFollowerControllerCom);
    function GetCurrentBoldElement: IBoldElement;
    function GetCurrentBoldObject: IBoldObject;
    function GetBoldList: IBoldList;
    function GetBoldHandleIndexLock: Boolean;
    procedure SetBoldHandleIndexLock(Value: Boolean);
    procedure SetSelected(index: Integer; V: Boolean);
    procedure InternalSetSelected(index: integer; v: Boolean);
    procedure SetSelection(aRow: Integer; Shift: TShiftState);
    function GetSelectedCount: Integer;
    procedure SetAlignment(Value: TAlignment);
    procedure _BeforeMakeUptoDate(Follower: TBoldFollowerCom);
    procedure _AfterMakeUptoDate(Follower: TBoldFollowerCom);
    procedure _InsertItem(Follower: TBoldFollowerCom);
    procedure _DeleteItem(index: Integer; OwningFollower: TBoldFollowerCom);
    procedure _RowAfterMakeUptoDate(Follower: TBoldFollowerCom);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    function GetMutableList: IBoldList;
  protected
    function GetItemIndex: Integer; override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure DefaultSetFontAndColor(Index: integer); virtual;
    procedure Click; override;
    procedure DblClick; override;
    function GetSelected(index: Integer): Boolean; {$IFDEF DELPHI6_OR_LATER} override; {$ENDIF}
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DrawItem(index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    function GetPopupMenu: TPopupMenu; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MeasureItem(index: Integer; var Height: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property Follower: TBoldFollowerCom read GetFollower;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property BoldHandle: TBoldAbstractListHandleCom read GetBoldHandle write SetBoldHandle;
    property BoldHandleIndexLock: Boolean read GetBoldHandleIndexLock write SetBoldHandleIndexLock default true;
    property BoldList: IBoldList read GetBoldList;
    property BoldProperties: TBoldListAsFollowerListControllerCom read fBoldProperties write SetBoldProperties;
    property BoldRowProperties: TBoldStringFollowerControllerCom read fBoldRowProperties write SetRowProperties;
    property CurrentBoldObject: IBoldObject read GetCurrentBoldObject;
    property CurrentBoldElement: IBoldElement read GetCurrentBoldElement;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelectedCount: Integer read GetSelectedCount;
    property Selected[index: Integer]: Boolean read GetSelected write SetSelected;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawItem(Index: integer; Rect: TRect); virtual;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    property MutableList: IBoldList read GetMutableList;
  end;

  {---TBoldListBoxCom---}
  TBoldListBoxCom = class(TBoldCustomListBoxCom)
  public
    {$IFNDEF T2H}
    property BoldList;
    property CurrentBoldElement;
    property CurrentBoldObject;
    property ItemIndex;
    property Selected;
    property SelectedCount;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BoldHandleIndexLock;
    property BoldHandle;
    property BoldProperties;
    property BoldRowProperties;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ExtendedSelect;
    property Enabled;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnEndDock;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  Forms,
  {$IFNDEF BOLDCOMCLIENT}
  BoldGui,
  {$ENDIF}
  BoldControlPackDefs,
  BoldListControlPackCom;

{---TBoldCustomListBoxCom---}
constructor TBoldCustomListBoxCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBoldRowProperties := TBoldStringFollowerControllerCom.Create(Self);
  fBoldRowProperties.AfterMakeUptoDate := _RowAfterMakeUptoDate;
  fBoldRowProperties.OnGetContextType := GetContextType;
  fBoldProperties := TBoldListAsFollowerListControllerCom.Create(Self, fBoldRowProperties);
  with fBoldProperties do
  begin
    OnAfterInsertItem := _InsertItem;
    OnAfterDeleteItem := _DeleteItem;
    BeforeMakeUptoDate := _BeforeMakeUptoDate;
    AfterMakeUptoDate := _AfterMakeUptoDate;
  end;
  fHandleFollower := TBoldListHandleFollowerCom.Create(Owner, fBoldProperties);
  DragMode := dmAutomatic;
  Style := lbOwnerDrawVariable;
end;

destructor TBoldCustomListBoxCom.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(fBoldProperties);
  FreeAndNil(fBoldRowProperties);
  inherited Destroy;
end;

procedure TBoldCustomListBoxCom._BeforeMakeUptoDate(Follower: TBoldFollowerCom);
begin
  if assigned(BoldHandle) and assigned(Boldhandle.list) then
    BoldHandle.list.EnsureRange(0, BoldHandle.list.Count-1);
  Items.BeginUpdate;
end;

procedure TBoldCustomListBoxCom._AfterMakeUptoDate(Follower: TBoldFollowerCom);
begin
  ItemIndex := Follower.CurrentIndex;
  if ItemIndex = -1 then
    fBoldProperties.SelectAll(Follower, False)
  else
    internalSetSelected(itemIndex, true);
  Items.EndUpdate;
end;

procedure TBoldCustomListBoxCom.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TBoldCustomListBoxCom.SetBoldHandle(Value: TBoldAbstractListHandleCom);
begin
  fHandleFollower.BoldHandle := value;
end;

function TBoldCustomListBoxCom.GetItemIndex: Integer;
begin
  Result := inherited ItemIndex;
end;

procedure TBoldCustomListBoxCom.SetItemIndex(const Value: Integer);
begin
  fHandleFollower.SetFollowerIndex(value);
  inherited ItemIndex := Value;
end;

procedure TBoldCustomListBoxCom.SetBoldProperties(Value: TBoldListAsFollowerListControllerCom);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomListBoxCom.SetRowProperties(Value: TBoldStringFollowerControllerCom);

begin
  fBoldRowProperties.Assign(Value);
end;

function TBoldCustomListBoxCom.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if not Assigned(Result) then
    Result := BoldProperties.Popup.GetMenu(nil, nil);
end;

procedure TBoldCustomListBoxCom.SetSelection(aRow: Integer; Shift: TShiftState);
begin
  if aRow = -1 then
    Exit;
  if not ((ssShift in Shift) or (ssCtrl in Shift)) or not MultiSelect then
  begin
    fBoldProperties.SelectAll(Follower, False);
    fBoldProperties.SetSelected(Follower, aRow, True);
  end;
  if (ssShift in Shift) and MultiSelect then
    fBoldProperties.SelectRange(Follower, aRow);
  if (ssCtrl in Shift) and MultiSelect then
    fBoldProperties.ToggleSelected(Follower, aRow);
  Invalidate;
end;

procedure TBoldCustomListBoxCom.MouseDown(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetSelection(ItemIndex, Shift);
  inherited;
end;

procedure TBoldCustomListBoxCom.Click;
begin
  inherited Click;
  fHandleFollower.SetFollowerIndex(ItemIndex);
end;

procedure TBoldCustomListBoxCom.DblClick;
var
  autoform: TForm;
begin
  if Assigned(OnDblClick) then
    inherited
  else
    if BoldProperties.DefaultDblClick and Assigned(CurrentBoldElement) then begin
      {$IFDEF BOLDCOMCLIENT}
      Autoform := nil;
      {$ELSE}
      AutoForm := AutoFormProviderRegistry.FormForElement(CurrentBoldElement);
      {$ENDIF}
      if assigned(AutoForm) then
        AutoForm.Show;
    end;
end;

procedure TBoldCustomListBoxCom.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomListBoxCom.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomListBoxCom.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  if Assigned(OnDragOver)
    or (BoldProperties.DropMode = bdpNone)
    or ((Source = Self) and (not BoldProperties.InternalDrag)) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, MutableList, ItemAtPos(Point(X, Y), False));
end;

procedure TBoldCustomListBoxCom.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
  begin
    {$IFNDEF BOLDCOMCLIENT}
    if BoldGuiHandler.ActivateTargetFormOnDrop then
      BoldGUIHandler.TryToFocusHostingForm(self);
    {$ENDIF}
    inherited DragDrop(Source, X, Y);
  end
  else
    BoldProperties.DragDrop(Follower, MutableList, ItemAtPos(Point(X, Y), False));
end;

function TBoldCustomListBoxCom.GetCurrentBoldElement: IBoldElement;
var
  Subfollower: TBoldFollowerCom;
begin
  SubFollower := Follower.SubFollowers[ItemIndex];
  if assigned(SubFollower) then
    Result := Subfollower.Element
  else
    Result := nil;
end;

function TBoldCustomListBoxCom.GetBoldList: IBoldList;
begin
  if Assigned(BoldHandle) then
    Result := BoldHandle.List
  else
    Result := nil;
end;

function TBoldCustomListBoxCom.GetCurrentBoldObject: IBoldObject;
begin
{$IFDEF BOLDCOMCLIENT}
  CurrentBoldElement.QueryInterface(IBoldObject, result);
{$ELSE}
  if CurrentBoldElement is IBoldObject then
    Result := CurrentBoldElement as IBoldObject
  else
    Result := nil;
{$ENDIF}
end;

procedure TBoldCustomListBoxCom._RowAfterMakeUptoDate(Follower: TBoldFollowerCom);
var
  index: Integer;
begin


  index := Follower.index;
  if (index > -1) and (index < Items.Count) then
    Items[index] := TBoldStringFollowerControllerCom(Follower.Controller).GetCurrentAsString(Follower);
end;

procedure TBoldCustomListBoxCom._InsertItem(Follower: TBoldFollowerCom);
begin
  Items.Insert(Follower.Index, '');
end;

procedure TBoldCustomListBoxCom._DeleteItem(index: Integer; OwningFollower: TBoldFollowerCom);
begin
  Items.Delete(index);
end;

procedure TBoldCustomListBoxCom.SetSelected(index: Integer; V: Boolean);
begin
  inherited Selected[index] := V;
  InternalSetSelected(Index, v);
end;

function TBoldCustomListBoxCom.GetSelected(index: Integer): Boolean;
begin
  Result := inherited Selected[index];
end;

function TBoldCustomListBoxCom.GetSelectedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  if MultiSelect then
    Result := SelCount
  else
    for I := 0 to Items.Count - 1 do
      if Selected[I] then
      begin
        Result := 1;
        Break;
      end;
end;

procedure TBoldCustomListBoxCom.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key in [33..40] then
  begin
    Exclude(Shift, ssCtrl);
    SetSelection(ItemIndex, Shift);
  end;
  inherited;
end;

procedure TBoldCustomListBoxCom.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  SignedItemId: integer;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    if Integer(itemID) >= 0 then begin
      DrawItem(itemID, rcItem, State);
      SignedItemId := ItemId;
    end else begin
      Canvas.FillRect(rcItem);
      SignedItemId := -1;
    end;
    if Assigned(BoldHandle) and (SigneditemID = Follower.CurrentIndex) then
      Canvas.DrawFocusRect(rcItem);
  end;
end;

procedure TBoldCustomListBoxCom.DefaultSetFontAndColor(index: Integer);
var
  ec: tColor;
  SubFollower: TBoldFollowerCom;
begin
  BoldRowProperties.SetFont(Canvas.Font, Font, Follower.SubFollowers[index]);
  BoldRowProperties.SetColor(ec, Color, Follower.SubFollowers[index]);
  Canvas.Brush.Color := ec;
  SubFollower := Follower.SubFollowers[index];
  if assigned(Subfollower) and Subfollower.Selected then
    with Canvas do
    begin
      Brush.Color := clHighlight;
      Font.Color := clHighlightText;
    end;
end;

procedure TBoldCustomListBoxCom.DefaultDrawItem(Index: integer; Rect: TRect);
begin
  BoldRowProperties.DrawOnCanvas(Follower.SubFollowers[index], Canvas, Rect, Alignment, Point(2,0));
end;


procedure TBoldCustomListBoxCom.MeasureItem(index: Integer; var Height: Integer);
var
  S: string;
begin
  BoldRowProperties.SetFont(Canvas.Font, Font, Follower.SubFollowers[index]);
  S := '';
  if Assigned(Follower) and
    Assigned(Follower.Controller) then
    S := TBoldStringFollowerControllerCom(Follower.Controller).GetCurrentAsString(Follower.SubFollowers[index]);
  if S = '' then
    Height := 2 + Abs(Canvas.Font.Height)
  else
    Height := Canvas.TextHeight(S);
  inherited;
end;

procedure TBoldCustomListBoxCom.WMSize(var Message: TWMSize);
begin
  inherited;
  if Alignment <> taLeftJustify then
    Invalidate;
end;

procedure TBoldCustomListBoxCom.DrawItem(index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  DefaultSetFontAndColor(Index);
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, index, Rect, State)
  else
    DefaultDrawItem(Index, Rect);
end;

function TBoldCustomListBoxCom.GetBoldHandle: TBoldAbstractListHandleCom;
begin
 Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomListBoxCom.GetFollower: TBoldFollowerCom;
begin
  Result := fHandleFollower.Follower;
end;

function TBoldCustomListBoxCom.GetContextType: IBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomListBoxCom.GetExpression: String;
begin
  result := BoldRowProperties.Expression;
end;

procedure TBoldCustomListBoxCom.SetExpression(Expression: String);
begin
  BoldRowProperties.Expression := Expression;
end;

function TBoldCustomListBoxCom.GetBoldHandleIndexLock: Boolean;
begin
  Result := fHandleFollower.HandleIndexLock;
end;

procedure TBoldCustomListBoxCom.SetBoldHandleIndexLock(Value: Boolean);
begin
  fHandleFollower.HandleIndexLock := Value;
end;


function TBoldCustomListBoxCom.GetMutableList: IBoldList;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.MutableList
  else
    result := nil;
end;

function TBoldCustomListBoxCom.GetVariableList: IBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

procedure TBoldCustomListBoxCom.InternalSetSelected(index: integer; v: Boolean);
begin
  if fBoldProperties.GetSelected(Follower, index) <> V then
  begin
    if V and not MultiSelect then
      fBoldProperties.SelectAll(Follower, False);
    fBoldProperties.SetSelected(Follower, index, V);
  end;
end;

end.
