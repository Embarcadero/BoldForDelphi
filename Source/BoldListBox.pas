unit BoldListBox;

{$UNDEF BOLDCOMCLIENT}
{$INCLUDE bold.inc}

interface

uses
  Classes,
  StdCtrls,
  Controls,
  Windows,
  Menus,
  Graphics,
  Messages,
  BoldEnvironmentVCL, // Make sure VCL environement loaded, and finalized after
  BoldElements,
  {$IFNDEF BOLDCOMCLIENT} // uses
  BoldSystem,
  BoldAFP,
  {$ENDIF}
  BoldAbstractListHandle,
  BoldControlPack,
  BoldListHandleFollower,
  BoldListListControlPack,
  BoldStringControlPack,
  BoldDefs;

// CHECKME is a destroywind needed that saves the extra list.
//         when is DestroyWnd actually called.

type
  {Forward declarations of all classes}
  TBoldCustomListBox = class;
  TBoldListBox = class;

  {---TBoldCustomListBox---}
  TBoldCustomListBox = class(TCustomListBox, IBoldOCLComponent)
  private
    {Bold stuff}
    FAlignment: TAlignment;
    fHandleFollower: TBoldListHandleFollower;
    fBoldProperties: TBoldListAsFollowerListController;
    fBoldRowProperties: TBoldStringFollowerController;

    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetExpression: TBoldExpression;
    function GetVariableList: TBoldExternalVariableList;

    function GetBoldHandle: TBoldAbstractListHandle;
    procedure SetBoldHandle(value: TBoldAbstractListHandle);
    function GetFollower: TBoldFOllower;
    procedure SetBoldProperties(Value: TBoldListAsFollowerListController);
    procedure SetRowProperties(Value: TBoldStringFollowerController);
    function GetCurrentBoldElement: TBoldElement;
    function GetCurrentBoldObject: TBoldObject;
    function GetBoldList: TBoldList;
    function GetItemIndex: Integer; reintroduce;
    function GetBoldHandleIndexLock: Boolean;
    procedure SetBoldHandleIndexLock(Value: Boolean);
    procedure SetItemIndex(Value: Integer); reintroduce;
    procedure SetSelected(index: Integer; V: Boolean);
    procedure InternalSetSelected(index: integer; v: Boolean);
    procedure SetSelection(aRow: Integer; Shift: TShiftState);
    function GetSelectedCount: Integer;
    procedure SetAlignment(Value: TAlignment);
    procedure _BeforeMakeUptoDate(Follower: TBoldFollower);
    procedure _AfterMakeUptoDate(Follower: TBoldFollower);
    procedure _InsertItem(index: Integer; Follower: TBoldFollower);
    procedure _DeleteItem(index: Integer; OwningFollower: TBoldFollower);
    procedure _RowAfterMakeUptoDate(Follower: TBoldFollower);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    function GetMutableList: TBoldList;
  protected
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
    property Follower: TBoldFollower read GetFollower;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property BoldHandle: TBoldAbstractListHandle read GetBoldHandle write SetBoldHandle;
    property BoldHandleIndexLock: Boolean read GetBoldHandleIndexLock write SetBoldHandleIndexLock default true;
    property BoldList: TBoldList read GetBoldList;
    property BoldProperties: TBoldListAsFollowerListController read fBoldProperties write SetBoldProperties;
    property BoldRowProperties: TBoldStringFollowerController read fBoldRowProperties write SetRowProperties;
    property CurrentBoldObject: TBoldObject read GetCurrentBoldObject;
    property CurrentBoldElement: TBoldElement read GetCurrentBoldElement;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelectedCount: Integer read GetSelectedCount;
    property Selected[index: Integer]: Boolean read GetSelected write SetSelected;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawItem(Index: integer; Rect: TRect); virtual;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    property MutableList: TBoldList read GetMutableList;
  end;

  {---TBoldListBox---}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldListBox = class(TBoldCustomListBox)
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
  System.Types,
  {$IFNDEF BOLDCOMCLIENT} // uses
  BoldGui, // IFNDEF BOLDCOMCLIENT
  {$ENDIF}
  BoldControlPackDefs,
  BoldGuiResourceStrings,
  BoldListControlPack;

{---TBoldCustomListBox---}
constructor TBoldCustomListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBoldRowProperties := TBoldStringFollowerController.Create(Self);
  fBoldRowProperties.AfterMakeUptoDate := _RowAfterMakeUptoDate;
  fBoldRowProperties.OnGetContextType := GetContextType;
  fBoldProperties := TBoldListAsFollowerListController.Create(Self, fBoldRowProperties);
  fBoldProperties.OnAfterInsertItem := _InsertItem;
  fBoldProperties.OnAfterDeleteItem := _DeleteItem;
  fBoldProperties.BeforeMakeUptoDate := _BeforeMakeUptoDate;
  fBoldProperties.AfterMakeUptoDate := _AfterMakeUptoDate;
  fHandleFollower := TBoldListHandleFollower.Create(Owner, fBoldProperties);
  DragMode := dmAutomatic;
  Style := lbOwnerDrawVariable;
end;

destructor TBoldCustomListBox.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(fBoldProperties);
  FreeAndNil(fBoldRowProperties);
  inherited Destroy;
end;

procedure TBoldCustomListBox._BeforeMakeUptoDate(Follower: TBoldFollower);
begin
  // Will fetch all
  if assigned(BoldHandle) and assigned(Boldhandle.list) then
    BoldHandle.list.EnsureRange(0, BoldHandle.list.Count-1);
  Items.BeginUpdate;
end;

procedure TBoldCustomListBox._AfterMakeUptoDate(Follower: TBoldFollower);
begin
  ItemIndex := Follower.CurrentIndex;
  if ItemIndex = -1 then
    fBoldProperties.SelectAll(Follower, False)
  else
    internalSetSelected(itemIndex, true);
  Items.EndUpdate;
end;

procedure TBoldCustomListBox.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    // Enough to invalidate drawing surface
    Invalidate;
  end;
end;

procedure TBoldCustomListBox.SetBoldHandle(Value: TBoldAbstractListHandle);
begin
  fHandleFollower.BoldHandle := value;
end;

function TBoldCustomListBox.GetItemIndex: Integer;
begin
  Result := inherited ItemIndex;
end;

procedure TBoldCustomListBox.SetItemIndex(Value: Integer);
begin
  fHandleFollower.SetFollowerIndex(value);
  inherited ItemIndex := Value; // FIXME;
end;

procedure TBoldCustomListBox.SetBoldProperties(Value: TBoldListAsFollowerListController);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomListBox.SetRowProperties(Value: TBoldStringFollowerController);

begin
  fBoldRowProperties.Assign(Value);
end;

function TBoldCustomListBox.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if not Assigned(Result) then
    Result := BoldProperties.Popup.GetMenu(nil, nil);
end;

procedure TBoldCustomListBox.SetSelection(aRow: Integer; Shift: TShiftState);
begin
  if aRow = -1 then
    Exit;
  //  Clear previous selection, Select one item
  if not ((ssShift in Shift) or (ssCtrl in Shift)) or not MultiSelect then
  begin
    fBoldProperties.SelectAll(Follower, False);
    fBoldProperties.SetSelected(Follower, aRow, True);
  end;

  //  Select range from first selected item
  if (ssShift in Shift) and MultiSelect then
    fBoldProperties.SelectRange(Follower, aRow);

  //  Toggle selection on current item
  if (ssCtrl in Shift) and MultiSelect then
    fBoldProperties.ToggleSelected(Follower, aRow);
  Invalidate;
end;

procedure TBoldCustomListBox.MouseDown(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetSelection(ItemIndex, Shift);
  inherited;
end;

procedure TBoldCustomListBox.Click;
begin
  inherited Click;
  fHandleFollower.SetFollowerIndex(ItemIndex);
end;

procedure TBoldCustomListBox.DblClick;
var
  autoform: TForm;
begin
  if Assigned(OnDblClick) then
    inherited
  else
    if BoldProperties.DefaultDblClick and Assigned(CurrentBoldElement) then begin
      {$IFDEF BOLDCOMCLIENT} // autoform
      Autoform := nil;
      {$ELSE}
      AutoForm := AutoFormProviderRegistry.FormForElement(CurrentBoldElement);
      {$ENDIF}
      if assigned(AutoForm) then
        AutoForm.Show;
    end;
end;

procedure TBoldCustomListBox.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomListBox.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomListBox.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  if Assigned(OnDragOver)
    or (BoldProperties.DropMode = bdpNone)
    or ((Source = Self) and (not BoldProperties.InternalDrag)) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, MutableList, ItemAtPos(Point(X, Y), False));
end;

procedure TBoldCustomListBox.DragDrop(Source: TObject; X, Y: Integer);
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

function TBoldCustomListBox.GetCurrentBoldElement: TBoldElement;
var
  Subfollower: TBoldFollower;
begin
  Result := nil;
  if ItemIndex <> -1 then
  begin
    SubFollower := Follower.SubFollowers[ItemIndex];
    if assigned(SubFollower) then
      Result := Subfollower.Element
  end;
end;

function TBoldCustomListBox.GetBoldList: TBoldList;
begin
  //CHECKME We may have to remove this because the list is not necessarily equal with the rendered list!!! /FH
  if Assigned(BoldHandle) then
    Result := BoldHandle.List
  else
    Result := nil;
end;

function TBoldCustomListBox.GetCurrentBoldObject: TBoldObject;
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

procedure TBoldCustomListBox._RowAfterMakeUptoDate(Follower: TBoldFollower);
var
  index: Integer;
begin
// This shouldn't be needed...
//  if Assigned(BoldHandle) then
//    inherited ItemIndex := BoldHandle.CurrentIndex;
  index := Follower.index;
  if (index > -1) and (index < Items.Count) then //FIXME: How come index sometimes is > Items.Count?
    Items[index] := TBoldStringFollowerController(Follower.Controller).GetCurrentAsString(Follower);
end;

procedure TBoldCustomListBox._InsertItem(index: Integer; Follower: TBoldFollower);
begin
  Assert(Assigned(Follower));
  Follower.EnsureDisplayable;
  Items.Insert(Follower.Index, TBoldStringFollowerController(Follower.Controller).GetCurrentAsString(Follower));
end;

procedure TBoldCustomListBox._DeleteItem(index: Integer; OwningFollower: TBoldFollower);
begin
  Items.Delete(index);
end;

procedure TBoldCustomListBox.SetSelected(index: Integer; V: Boolean);
begin
  inherited Selected[index] := V;
  InternalSetSelected(Index, v);
end;

function TBoldCustomListBox.GetSelected(index: Integer): Boolean;
begin
  Result := inherited Selected[index];
end;

function TBoldCustomListBox.GetSelectedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  if MultiSelect then // if not MultiSelect SelCount is always -1!
    Result := SelCount
  else
    for I := 0 to Items.Count - 1 do // Set result to 1 if something is selected
      if Selected[I] then
      begin
        Result := 1;
        Break;
      end;
end;

procedure TBoldCustomListBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key in [33..40] then //PGUP..DOWN
  begin
    Exclude(Shift, ssCtrl); // Cannot make non-consecutive selections with keyboard
    SetSelection(ItemIndex, Shift); // Call setselection with currentrow and shiftstate
  end;
  inherited;
end;

procedure TBoldCustomListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  SignedItemId: integer; // this variable is used to suppress warning from D4 when comparing signed and unsigned values
begin
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
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
      //FIXME Apperens of selected and current...
      Canvas.DrawFocusRect(rcItem);
  end;
  {$WARN UNSAFE_CAST ON}
  {$WARN UNSAFE_CODE ON}
end;

procedure TBoldCustomListBox.DefaultSetFontAndColor(index: Integer);
var
  ec: tColor;
  SubFollower: TBoldFollower;
begin
  BoldRowProperties.SetFont(Canvas.Font, Font, Follower.SubFollowers[index]);
  BoldRowProperties.SetColor(ec, Color, Follower.SubFollowers[index]);
  Canvas.Brush.Color := ec;
  //  Selected state yields default highlight colors
  SubFollower := Follower.SubFollowers[index];
  if assigned(Subfollower) and Subfollower.Selected then begin
    Canvas.Brush.Color := clHighlight;
    Canvas.Font.Color := clHighlightText;
  end;
end;

procedure TBoldCustomListBox.DefaultDrawItem(Index: integer; Rect: TRect);
begin
  BoldRowProperties.DrawOnCanvas(Follower.SubFollowers[index], Canvas, Rect, Alignment, Point(2,0));
end;


procedure TBoldCustomListBox.MeasureItem(index: Integer; var Height: Integer);
var
  S: string;
begin
  // Need to get the font to use
  BoldRowProperties.SetFont(Canvas.Font, Font, Follower.SubFollowers[index]);
  // And measure using current data
  S := '';
  if Assigned(Follower) and
    Assigned(Follower.Controller) then
    S := TBoldStringFollowerController(Follower.Controller).GetCurrentAsString(Follower.SubFollowers[index]);
  if S = '' then
    Height := 2 + Abs(Canvas.Font.Height)
  else
    Height := Canvas.TextHeight(S);

  // Now allow user to remeasure, using updated height-value
  inherited;
end;

procedure TBoldCustomListBox.WMSize(var Message: TWMSize);
begin
  // Redraw when resising if aligment is not taLeftJustify
  inherited;
  if Alignment <> taLeftJustify then
    Invalidate;
end;

procedure TBoldCustomListBox.DrawItem(index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  DefaultSetFontAndColor(Index);
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, index, Rect, State)
  else
    DefaultDrawItem(Index, Rect);
end;

function TBoldCustomListBox.GetBoldHandle: TBoldAbstractListHandle;
begin
 Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomListBox.GetFollower: TBoldFOllower;
begin
  Result := fHandleFollower.Follower;
end;

function TBoldCustomListBox.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomListBox.GetExpression: String;
begin
  result := BoldRowProperties.Expression;
end;

procedure TBoldCustomListBox.SetExpression(const Value: TBoldExpression);
begin
  BoldRowProperties.Expression := Value;
end;

function TBoldCustomListBox.GetBoldHandleIndexLock: Boolean;
begin
  Result := fHandleFollower.HandleIndexLock;
end;

procedure TBoldCustomListBox.SetBoldHandleIndexLock(Value: Boolean);
begin
  fHandleFollower.HandleIndexLock := Value;
end;


function TBoldCustomListBox.GetMutableList: TBoldList;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.MutableList
  else
    result := nil;
end;

function TBoldCustomListBox.GetVariableList: TBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

procedure TBoldCustomListBox.InternalSetSelected(index: integer; v: Boolean);
begin
  if fBoldProperties.GetSelected(Follower, index) <> V then
  begin
    if V and not MultiSelect then
      fBoldProperties.SelectAll(Follower, False);
    fBoldProperties.SetSelected(Follower, index, V);
  end;
end;

end.

