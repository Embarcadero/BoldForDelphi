unit BoldComboBox;

{$UNDEF BOLDCOMCLIENT}
{$INCLUDE bold.inc}

interface

uses
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  SysUtils,
  Menus,
  StdCtrls,
  BoldEnvironmentVCL, // Make sure VCL environement loaded, and finalized after
  BoldDefs,
  BoldControlsDefs,
  BoldControlPackDefs,  
  BoldHandles,
  BoldElements,
  BoldAbstractListHandle,
  BoldElementHandleFollower,
  BoldListHandleFollower,
  BoldComponentValidator,
  BoldControlPack,
  BoldListListControlPack,
  BoldStringControlPack;

{TODO 3 -ofrha -cfeature: WM_PAINT}
{TODO 3 -ofrha -cbug: Alignment not compleatly implemented}
{TODO 3 -ofrha -ccheckme: CB_GETEXTENDEDUI}
{TODO 3 -ofrha -cbug: AfterMakeUptodate when DroppedDown}

type
  {Forward declarations of all classes}
  TBoldCustomComboBox = class;
  TBoldComboBox = class;

  TBoldComboListController = class(TBoldAbstractListAsFollowerListController)
  published
     property DragMode;
     property DropMode;
     property NilElementMode;
  end;

  TBoldCustomComboBox = class(TCustomComboBox, IBoldValidateableComponent, IBoldOCLComponent)
  private
    fAlignment: TAlignment;
    fHandleFollower: TBoldElementHandleFollower;
    fListHandleFollower: TBoldListHandleFollower;
    fBoldProperties: TBoldStringFollowerController;
    fBoldListProperties: TBoldComboListController;
    fBoldRowProperties: TBoldStringFollowerController;
    fBoldSelectChangeAction: TBoldComboSelectChangeAction;
    fBoldSetValueExpression: TBoldExpression;
    fInternalChange: boolean;
    fColor: TColor;
    fEffectiveReadOnly: Boolean;
    fFocused: Boolean;
    fFont: TFont;
    fFontHeight: Integer;
    fFontScaleFlag: Boolean;
    fReadOnly: Boolean;
    fOnSelectChanged: TNotifyEvent;
    fIsEditEvent: boolean;
    fProcessingClick: Integer;    
    fOnSelectChangedIsCalled:Boolean;
    fAutoSearch: boolean;
    FUseMouseWheel: Boolean;
    function GetBoldHandle: TBoldElementHandle;
    procedure SetBoldHandle(value: TBoldElementHandle);
    function GetFollower: TBoldFOllower;
    function GetBoldListHandle: TBoldAbstractListHandle;
    procedure SetBoldListHandle(value: TBoldAbstractListHandle);
    function GetListFollower: TBoldFOllower;
    procedure _InsertItem(Index: Integer; Follower: TBoldFollower);
    procedure _DeleteItem(Index: Integer; OwningFollower: TBoldFollower);
    procedure _ReplaceItem(index: Integer; AFollower: TBoldFollower);
    procedure _RowAfterMakeUptoDate(Follower: TBoldFollower);
    procedure _AfterMakeUptoDate(Follower: TBoldFollower);
    procedure FontChanged(Sender: TObject);
    procedure SetBoldSelectChangeAction(Value: TBoldComboSelectChangeAction);
    procedure SetEffectiveReadOnly(Value: Boolean);
    procedure SetEffectiveText(Value: string);
    procedure SetFocused(Value: Boolean);
    {Messages}
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    {Property handlers}
    function GetEffectiveColor: TColor;
    function GetEffectiveFont: TFont;
    function GetText: string;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    function GetSelectedElement: TBoldElement;
    procedure SetBoldProperties(Value: TBoldStringFollowerController);
    procedure SetBoldListProperties(Value: TBoldComboListController);
    procedure SetRowProperties(Value: TBoldStringFollowerController);
    procedure SetCharCase(const Value: TEditCharCase);
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetReadOnly(Value: Boolean);
    procedure SetText(Value: string);
    function GetContextForBoldProperties: TBoldElementTypeInfo;
    function GetContextForBoldRowProperties: TBoldElementTypeInfo;
    {$IFNDEF BOLDCOMCLIENT}
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean;
    {$ENDIF}
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    {IBoldOCLComponent}
    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetVariableList: TBoldExternalVariableList;
    function GetExpression: TBoldExpression;
  protected
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

    function HandleApplyException(E: Exception; Elem: TBoldElement; var discard: Boolean): boolean;
    procedure Change; override;
    function ComboAllowsTextEditing(BoldProperties: TBoldStringFollowerController; Follower: TBoldFollower): Boolean; virtual;
    procedure ChangeScale(M, D: Integer); override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    procedure CreateWnd; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    function GetPopupMenu: TPopupMenu; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure UpdateEffectiveColor;
    procedure UpdateEffectiveReadOnly;
    procedure UpdateEffectiveFont;
    procedure WndProc(var Message: TMessage); override;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Follower: TBoldFollower read GetFollower;
    property ListFollower: TBoldFollower read GetListFollower;
    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property BoldListHandle: TBoldAbstractListHandle read GetBoldListHandle write SetBoldListHandle;
    property BoldProperties: TBoldStringFollowerController read FBoldProperties write SetBoldProperties;
    property BoldListProperties: TBoldComboListController read fBoldListProperties write SetBoldListProperties;
    property BoldRowProperties: TBoldStringFollowerController read fBoldRowProperties write SetRowProperties;
    property BoldSelectChangeAction: TBoldComboSelectChangeAction read fBoldSelectChangeAction write SetBoldSelectChangeAction;
    property BoldSetValueExpression: TBoldExpression read fBoldSetValueExpression write fBoldSetValueExpression;
    property OnSelectChanged: TNotifyEvent read fOnSelectChanged write fOnSelectChanged;
    property CharCase write SetCharCase;
    property Color: TColor read FColor write SetColor stored IsColorStored default clWindow ;
    property EffectiveColor: TColor read GetEffectiveColor;
    property EffectiveFont: TFont read GetEffectiveFont;
    property EffectiveReadOnly: Boolean read FEffectiveReadOnly;
    property Font: TFont read FFont write SetFont stored IsFontStored;
//    property Items write SetItems;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Text: string read GetText write SetText;
    procedure Click; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    property SelectedElement: TBoldElement read GetSelectedElement;
    property UseMouseWheel: Boolean read FUseMouseWheel write FUseMouseWheel;
  end;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldComboBox = class(TBoldCustomCombobox)
  {$IFNDEF T2H}
  public
    property EffectiveColor;
    property EffectiveFont;
    property EffectiveReadOnly;
    property Text;
  published
    property Alignment;
    property BoldHandle;
    property BoldListHandle;
    property BoldListProperties;
    property BoldProperties;
    property BoldRowProperties;
    property BoldSetValueExpression;
    property BoldSelectChangeAction;
    property OnSelectChanged;
    property CharCase; {Must be published before ReadOnly} //From TBoldCutomCombBox
    property ReadOnly;
    {Properties in standard TComboBox}
    property Style; {Must be published before Items} {TODO 3 -ofrha -cbug: Create own styles!}
    property Anchors;
    //  property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
//    property ImeMode;
//    property ImeName;
    property ItemHeight;
//    property Items;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
//    property Text; Text is only public!
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDock;
    property OnStartDrag;
    property UseMouseWheel default False;
    {$ENDIF}
  end;

implementation

uses
  BoldExceptionHandlers,
  BoldReferenceHandle,
  {$IFNDEF BOLDCOMCLIENT}
  BoldSystem,
  {$ENDIF}
  BoldListControlPack,
  BoldQueue,
  BoldUtils,
  BoldGuiResourceStrings;

{ TBoldCustomComboBox }

type
  TWinControlHack = class(TWinControl) {We need access to Parents Font and Color property}
  end;
  TBoldQueueableAccess = class(TBoldQueueable);
  TBoldElementHandleAccess = class(TBoldElementHandle);

procedure TBoldCustomComboBox._InsertItem(Index: Integer; Follower: TBoldFollower);
begin
  Items.Insert(Index, '');
  if Assigned(Follower) then
  begin
    Follower.EnsureDisplayable;
    Items[index] := TBoldStringFollowerController(Follower.Controller).GetCurrentAsString(Follower);
  end;
end;

procedure TBoldCustomComboBox._DeleteItem(index: Integer; OwningFollower: TBoldFollower);
begin
  Items.Delete(index);
end;

procedure TBoldCustomComboBox._ReplaceItem(index: Integer;
  AFollower: TBoldFollower);
var
  s: string;
begin
  AFollower.EnsureDisplayable;
  s := TBoldStringFollowerController(AFollower.Controller).GetCurrentAsString(AFollower);
  if s <> Items[index] then
    Items[index] := s;
end;

procedure TBoldCustomComboBox._RowAfterMakeUptoDate(Follower: TBoldFollower);
var
  index: Integer;
begin
  index := Follower.index;
  if (index > -1) and (index < Items.Count) then
  begin
    Items[index] := TBoldStringFollowerController(Follower.Controller).GetCurrentAsString(Follower);
  end;
  Invalidate;
  // forces a redisplay of the edit-area, the windows component might go blank if the active row is removed and then reinserted
  fHandleFollower.Follower.MarkValueOutOfDate;
end;

procedure TBoldCustomComboBox._AfterMakeUptoDate(Follower: TBoldFollower);
var
  NewText: string;
begin
//  Code below removed to avoid closeup when moving in dropped down combo. suggested by Hans Karlsen

//  if not (Style = csSimple) and DroppedDown then
//    PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0); //FIXME Bad solution! CloseUp if changed while dropped down!

  if not fInternalChange then
  begin
    UpdateEffectiveColor;
    UpdateEffectiveReadOnly;
    UpdateEffectiveFont;
    NewText := BoldProperties.GetCurrentAsString(Follower);
    if inherited Text <> NewText then
    begin
      fInternalChange := true;
      SetEffectiveText(NewText);
      fInternalChange := false;
    end;
  end;
end;

procedure TBoldCustomComboBox.Change;
begin
  if not (csDesigning in ComponentState) and
    not EffectiveReadOnly and
    (BoldSelectChangeAction = bdscSetText) and
    ComboAllowsTextEditing(BoldProperties, Follower) then
    BoldProperties.MayHaveChanged(text, Follower);
  if not fOnSelectChangedIsCalled then
  begin
    if assigned(OnSelectChanged) then
      fOnSelectChanged(Self);
  end;
  inherited;
end;

procedure TBoldCustomComboBox.ChangeScale(M, D: Integer);
begin
  if (M <> D) and (not (csLoading in ComponentState) or fFontScaleFlag) and not ParentFont then
    Font.Size := MulDiv(Font.Size, M, D);
  fFontScaleFlag := False;
  inherited ChangeScale(M, D);
end;

procedure TBoldCustomComboBox.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
end;

procedure TBoldCustomComboBox.CMExit(var Message: TCMExit);
begin
  if not (csDestroying in ComponentState) and (Follower.Controller.ApplyPolicy = bapExit) then
  begin
    Follower.Apply;
  end;
  SetFocused(False);
  inherited;
end;

procedure TBoldCustomComboBox.CMParentColorChanged(var Message: TMessage);
begin
  if ParentColor then
  begin
    if Message.wParam <> 0 then
      SetColor(TColor(Message.lParam))
    else
      SetColor(TWinControlHack(Parent).Color);
    ParentColor := True;
  end;
end;

procedure TBoldCustomComboBox.CMParentFontChanged(var Message: TMessage);
begin
  if ParentFont then
  begin
    if Message.wParam <> 0 then
{$WARN UNSAFE_CAST OFF}
      SetFont(TFont(Message.lParam))
{$WARN UNSAFE_CAST ON}
    else
      SetFont(TWinControlHack(Parent).Font);
    ParentFont := True;
  end;
end;

procedure TBoldCustomComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        if (Style = csSimple) and (ComboWnd <> EditHandle) then
          if EffectiveReadOnly then Exit;
    end;
  fIsEditEvent := True; // Flag to know we're already in the event
  inherited ComboWndProc(Message, ComboWnd, ComboProc);
  fIsEditEvent := False;
end;

constructor TBoldCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FBoldProperties := TBoldStringFollowerController.Create(Self);
  FBoldProperties.AfterMakeUptoDate := _AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextForBoldProperties;
  fBoldRowProperties := TBoldStringFollowerController.Create(Self);
  fBoldRowProperties.AfterMakeUptoDate := _RowAfterMakeUptoDate;
  fBoldRowProperties.OnGetContextType := GetContextForBoldRowProperties;
  fBoldListProperties := TBoldComboListController.Create(Self, fBoldRowProperties);
  fBoldListProperties.OnAfterInsertItem := _InsertItem;
  fBoldListProperties.OnAfterDeleteItem := _DeleteItem;
  fBoldListProperties.OnReplaceitem := _ReplaceItem;
  fHandleFollower := TBoldElementHandleFollower.Create(Owner, fBoldProperties);
  fListHandleFollower := TBoldListHandleFollower.Create(Owner, fBoldListProperties);
  fHandleFollower.PrioritizedQueuable := fListHandleFollower;
  fHandleFollower.StronglyDependedOfPrioritized := true;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FColor := clWindow;
  FUseMouseWheel := False;
end;

procedure TBoldCustomComboBox.CreateWnd;
begin
  inherited CreateWnd;
  FEffectiveReadOnly := False;
  UpdateEffectiveReadOnly;
end;

destructor TBoldCustomComboBox.Destroy;
begin
  case BoldProperties.ApplyPolicy of
    bapChange: Follower.Apply;
    bapDemand: Follower.DiscardChange;
    bapExit: Follower.Apply;
  end;
  FreeAndNil(fHandleFollower);
  FreeAndNil(fListHandleFollower);
  FreeAndNil(FBoldProperties);
  FreeAndNil(fBoldListProperties);
  FreeAndNil(fBoldRowProperties);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TBoldCustomComboBox.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

function TBoldCustomComboBox.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  if FUseMouseWheel then
  begin
    Result := inherited;
  end else
  begin
    Result := True;
  end;
end;

procedure TBoldCustomComboBox.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomComboBox.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
end;

procedure TBoldCustomComboBox.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, follower.Element, 0);
end;

procedure TBoldCustomComboBox.FontChanged(sender: TObject);
begin
  ParentFont := False;
  if Font.Height <> FFontHeight then
  begin
    FFontScaleFlag := True;
    FFontHeight := Font.Height;
  end;
  UpdateEffectiveFont;
end;

function TBoldCustomComboBox.GetEffectiveColor: TColor;
begin
  Result := inherited Color;
end;

function TBoldCustomComboBox.GetEffectiveFont: TFont;
begin
  Result := inherited Font;
end;

function TBoldCustomComboBox.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (not Assigned(Result)) and Assigned(Follower.Element) then
    Result := BoldProperties.Popup.GetMenu(Self, Follower.Element);
end;

function TBoldCustomComboBox.GetText: string;
var
  I: Integer;
begin
  if Style in [csDropDown, csSimple] then
    Result := inherited Text
  else
  begin
    I := ItemIndex;
    if I < 0 then
    begin
      if BoldListProperties.NilElementMode = neInsertFirst then
      begin
        ItemIndex := 0;
        Result := Items[0];
      end
      else if BoldListProperties.NilElementMode = neAddLast then
      begin
        ItemIndex := Items.Count - 1;
        Result := Items[Items.Count - 1];
      end
      else
        Result := '';
    end
    else
      Result := Items[I];
  end;
end;

function TBoldCustomComboBox.GetContextType: TBoldElementTypeInfo;
begin
  result := GetContextForBoldProperties;
end;

function TBoldCustomComboBox.GetExpression: TBoldExpression;
begin
  result := self.BoldProperties.Expression;
end;

procedure TBoldCustomComboBox.SetExpression(const Value: TBoldExpression);
begin
  BoldProperties.Expression := Value;
end;

function TBoldCustomComboBox.GetVariableList: TBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

function TBoldCustomComboBox.IsColorStored: Boolean;
begin
  Result := not ParentColor;
end;

function TBoldCustomComboBox.IsFontStored: Boolean;
begin
  Result := not ParentFont;
end;

procedure TBoldCustomComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if EffectiveReadOnly and (Key in [VK_UP, VK_DOWN]) then
    Key := 0;
end;

procedure TBoldCustomComboBox.KeyPress(var Key: Char);
var
  Message: TMessage;
//  lWideChar: WideChar;
//  s: string;
//  lKey: Char;
begin
  inherited KeyPress(Key);
  if CharInSet(Key, [#32..#255]) then
  begin
{    lWideChar := GetWideCharFromWMCharMsg(LastWMCHarMessage);
    s := lWideChar;
    lKey := s[1];
    if (Style <> csDropDownList) and
       (BoldSelectChangeAction <> bdcsSetReference) and
       not BoldProperties.ValidateCharacter(lKey, Follower) then
    begin
      MessageBeep(0);
      Key := BOLDNULL;
    end;
}
  end;
  if Key = BOLDESC then
  begin
    Follower.DiscardChange;
    SelectAll;
    Key := BOLDNULL;
  end;
  if (Ord(Key) = 9) and DroppedDown then
  begin
    Message.Msg := CB_SETCURSEL;
{$WARN UNSAFE_CAST OFF}
    TWMCommand(Message).NotifyCode := CBN_SELCHANGE;
{$WARN UNSAFE_CAST ON}
    WndProc(Message);
//    fAutoSearch := true;


//    if ((Message.Msg = CB_SETCURSEL) and (BoldSelectChangeAction = bdcsSetReference)) or (TWMCommand(Message).NotifyCode = CBN_SELCHANGE) or (fAutoSearch) and not DroppedDown then
//    if not (DroppedDown and (Message.Msg = 273) and (TWMCommand(Message).NotifyCode = CBN_SELCHANGE)) then

//    PostMessage(Handle, CBN_CLOSEUP, 0, 0); //FIXME Bad solution! CloseUp if changed while dropped down!
//    CloseUp;
//    Invalidate;
//    Follower.Apply;
//    SelectAll;
  end;
  if Follower.IsInDisplayList then
    Follower.DisplayAll;
end;

procedure TBoldCustomComboBox.SetBoldHandle(Value: TBoldElementHandle);
begin
  if assigned(Value) and (BoldSelectChangeAction = bdcsSetReference) and not Value.CanSetValue then
    raise EBold.CreateFMt('The BoldHandle property must be a TBoldReferenceHandle when BoldSelectChangeAction is bdscSetReference. It is %s', [Value.ClassName]);
  fHandleFollower.BoldHandle := value;
end;

procedure TBoldCustomComboBox.SetBoldListHandle(Value: TBoldAbstractListHandle);
begin
    fListHandleFollower.BoldHandle := value;
end;

procedure TBoldCustomComboBox.SetBoldProperties(Value: TBoldStringFollowerController);
begin
  FBoldProperties.Assign(Value);
end;

procedure TBoldCustomComboBox.SetCharCase(const Value: TEditCharCase);
begin
  if CharCase <> Value then
  begin
    //FIXME Investigate and fix problems with CharCase and non readonly
    inherited CharCase := Value;
  end;
end;

procedure TBoldCustomComboBox.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    ParentColor := False;
    UpdateEffectiveColor;
  end;
end;

procedure TBoldCustomComboBox.SetEffectiveReadOnly(Value: Boolean);
begin
  if Value <> FEffectiveReadOnly then
  begin
    FEffectiveReadOnly := Value;
    if (Style in [csDropDown, csSimple]) and HandleAllocated then
      SendMessage(EditHandle, EM_SETREADONLY, Ord(Value), 0);
  end;
end;

procedure TBoldCustomComboBox.SetEffectiveText(Value: string);
// This mess is needed prevent flickering and allow changes to Text before the controls handle is allocated! /frha
var
  I: Integer;
  Redraw: Boolean;
begin
  if (Value <> GetText) then
  begin
    if Style <> csDropDown then
    begin
      Redraw := (Style <> csSimple) and HandleAllocated;
      if Redraw then
        SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        if Value = '' then
        begin
          if BoldListProperties.NilElementMode = neInsertFirst then
            I := 0
          else if BoldListProperties.NilElementMode = neAddLast then
            I := Items.Count - 1
          else
            I := -1;
        end
        else
          I := Items.IndexOf(Value);
        ItemIndex := I;
      finally
        if Redraw then
        begin
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
          Invalidate;
        end;
      end;
      if I >= 0 then Exit;
    end;
    if Style in [csDropDown, csSimple] then
      inherited Text := Value;
  end;
end;

procedure TBoldCustomComboBox.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then Invalidate;
  end;
end;

procedure TBoldCustomComboBox.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TBoldCustomComboBox.SetReadOnly(Value: Boolean);
begin
  if (FReadOnly <> Value) then
  begin
    FReadOnly := Value;
    UpdateEffectiveReadOnly;
  end;
end;

procedure TBoldCustomComboBox.SetRowProperties(Value: TBoldStringFollowerController);
 begin
  fBoldRowProperties.Assign(Value);
end;

procedure TBoldCustomComboBox.SetText(Value: string);
begin
  if not EffectiveReadOnly then
    SetEffectiveText(Value);
end;

procedure TBoldCustomComboBox.UpdateEffectiveColor;
var
  NewColor: TColor;
begin
  if not ParentColor then
  begin
    NewColor := Color;
    BoldProperties.SetColor(NewColor, Color, Follower); //FIXME FC should only take ONE color and a Follower! /frha
    inherited Color := NewColor;
  end;
end;

procedure TBoldCustomComboBox.UpdateEffectiveFont;
begin
  EffectiveFont.Assign(Font);
  BoldProperties.SetFont(EffectiveFont, Font, Follower); //FIXME FC should only take ONE font and a Follower! /frha
end;

procedure TBoldCustomComboBox.UpdateEffectiveReadOnly;
begin
  if BoldSelectChangeAction in [bdcsNone, bdcsSetValue, bdcsSetReference] then
    SetEffectiveReadOnly(FReadOnly)
  else
    SetEffectiveReadOnly(FReadOnly or not BoldProperties.MayModify(Follower));
end;

procedure TBoldCustomComboBox.WMPaint(var Message: TWMPaint);
begin
  //FIXME Own painting with call to renderers paint isn't done yet...
  inherited;
end;

procedure TBoldCustomComboBox.WndProc(var Message: TMessage);
var
  CallInherited: Boolean;
  ElementToAssignTo: TBoldElement;
  Discard: Boolean;
  LocalSelectedElement: TBoldElement;
begin
{$WARN UNSAFE_CAST OFF}
  CallInherited := True;
  if not (csDesigning in ComponentState) then
  begin
    case Message.Msg of
      CB_SETCURSEL,
      WM_COMMAND,
      305:
      begin
//        CodeSite.Category := 'combo';
//        CodeSite.Send(IntToStr(Message.Msg) + ':' + IntToStr(TWMCommand(Message).NotifyCode));
//        CodeSite.Category := '';

//        ( fAutoSearch and DroppedDown and (Message.Msg = CB_SETCURSEL) and (BoldSelectChangeAction = bdcsSetValue) and (TWMCommand(Message).NotifyCode = 0) )
        if not ((Message.Msg = 334) and (TWMCommand(Message).NotifyCode = 0) and not fAutoSearch) then
        if (
          ( ( (Message.Msg = CB_SETCURSEL) and (BoldSelectChangeAction = bdcsSetReference) ) or (TWMCommand(Message).NotifyCode = CBN_SELCHANGE) or (fAutoSearch) and not DroppedDown)
           or ((Message.Msg = 305) and DroppedDown)
//           or (not (DroppedDown and (Message.Msg = WM_COMMAND) and (TWMCommand(Message).NotifyCode = CBN_SELCHANGE)))
           or ( (Message.Msg = WM_COMMAND) and (TWMCommand(Message).NotifyCode = CBN_SELCHANGE) )
           )
           and not ( (Message.Msg = WM_COMMAND) and ( TWMCommand(Message).NotifyCode = 256 ) )
           and not ( (Message.Msg = WM_COMMAND) and ( TWMCommand(Message).NotifyCode = 1024 ) )
           and not ( (Message.Msg = WM_COMMAND) and ( TWMCommand(Message).NotifyCode = 3 ) )
//           and not ( (Message.Msg = WM_COMMAND) and ( TWMCommand(Message).NotifyCode = 1 ) )
           and not fInternalChange
           then
        begin
//          CodeSite.Send(IntToStr(Message.Msg) + ':' + IntToStr(TWMCommand(Message).NotifyCode));
          if (Message.Msg = CB_SETCURSEL) and assigned(BoldListHandle) and (Cardinal(BoldListHandle.Count) > Message.WParam) then
          begin
            if NativeInt(Message.WParam) = -1 then
              LocalSelectedElement := nil
            else
              LocalSelectedElement := BoldListHandle.List[Message.WParam]
          end
          else
            LocalSelectedElement := SelectedElement;

          case BoldSelectChangeAction of
            bdscSetText:
              CallInherited := not EffectiveReadOnly;
            bdcsSetValue:
            begin
              if Assigned(BoldHandle) and Assigned(BoldHandle.value) then
              begin
                {$IFDEF BOLDCOMCLIENT} // BoldSetValueExpression
                if trim(BoldSetValueExpression) <> '' then
                  ElementToAssignTo := BoldHandle.Value.EvaluateExpression(BoldSetValueExpression)
                else
                  elementToAssignTo := BoldHandle.Value;
                if assigned(ElementToAssignTo) then
                  ElementToAssignTo.AssignElement(LocalSelectedElement);  // checkme take from follwer instead?
                {$ELSE}
                if trim(BoldSetValueExpression) <> '' then
                  ElementToAssignTo := BoldHandle.Value.EvaluateExpressionAsDirectElement(BoldSetValueExpression)
                else
                  elementToAssignTo := BoldHandle.Value;

                if assigned(ElementToAssignTo) and ElementToAssignTo.Mutable and
                  // must check the element (and not the follower) since we might have a BoldSetValueExpression...
                  (not (elementToAssignTo is TBoldMember) or
                   TBoldMember(ElementTOAssignTo).CanModify) then
                  try
                    if elementToAssignTo is TBoldObjectReference then
                    begin
                      ElementToAssignTo.Assign(LocalSelectedElement);
                    end
                    else
                    begin
                      if Assigned(LocalSelectedElement) then
                        BoldProperties.MayHaveChanged(LocalSelectedElement.AsString, Follower)
                      else
                        BoldProperties.MayHaveChanged('', Follower);
                      fInternalChange := true;
                      try
                        TBoldQueueableAccess(Follower).Display;
                      finally
                        fInternalChange := false;
                      end;
                    end;
                  except
                    on E: Exception do
                    begin
                      if not HandleApplyException(E, ElementToAssignTo, Discard) then
                        raise;
                      if Discard then
                        Follower.DiscardChange;
                    end;
                  end;
                {$ENDIF}
              end;
              CallInherited := true;
            end;
            bdcsSetReference:
            begin
              Follower.DiscardChange;
              if assigned(BoldHandle) and BoldHandle.CanSetValue then
                TBoldElementHandleAccess(BoldHandle).SetValue(LocalSelectedElement);
              CallInherited := Message.Msg = CB_SETCURSEL;
            end;
            bdcsSetListIndex:
            begin
              Follower.DiscardChange;
              if assigned(BoldListHandle) then
                BoldListHandle.CurrentIndex := BoldListHandle.List.IndexOf(LocalSelectedElement);
              CallInherited := false;
            end;
            bdcsNone:
            begin
              CallInherited := false;
              Invalidate;
            end;
          end;
          if assigned(OnSelectChanged) then
            fOnSelectChanged(Self);
//        Code below removed to avoid closing dropdown, suggested by Hans Karlsen
          if (not CallInherited) and
             (Style <> csSimple) and
             not fIsEditEvent then
          begin
            if (Style <> csSimple) then
            begin
              PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
              Invalidate;
            end;
            Click;
            fOnSelectChangedIsCalled:=TRUE;
            try
              Change;
            finally
              fOnSelectChangedIsCalled:=FALSE;
            end;
          end;
        end;
      end;
      CB_SHOWDROPDOWN:
        if (Message.WParam=0) and EffectiveReadOnly then
          _AfterMakeUptoDate(Follower); {Restore text} //FIXME Maybe an UpdateEffectiveText?
    end;
  end;
  if CallInherited then
    inherited WndProc(Message);
{$WARN UNSAFE_CAST ON}
end;

procedure TBoldCustomComboBox.SetBoldListProperties(Value: TBoldComboListController);
begin
  fBoldListProperties.Assign(Value);
end;

function TBoldCustomComboBox.GetSelectedElement: TBoldElement;
var
  lFollower: TBoldFollower;
begin
{$WARN UNSAFE_CAST OFF}
  with ListFollower.RendererData as TBoldFollowerList do
  begin
    if (ItemIndex >= 0)and (ItemIndex < Count) then
    begin
      lFollower := Followers[ItemIndex];
      if Assigned(lFollower) then
        Result := lFollower.Element
      else
        result := nil;
    end
    else
      Result := nil;
  end;
{$WARN UNSAFE_CAST ON}
end;

function TBoldCustomComboBox.GetBoldHandle: TBoldElementHandle;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomComboBox.GetFollower: TBoldFOllower;
begin
  Result := fHandleFollower.Follower;
end;

function TBoldCustomComboBox.GetBoldListHandle: TBoldAbstractListHandle;
begin
 Result := fListHandleFollower.BoldHandle;
end;

function TBoldCustomComboBox.GetListFollower: TBoldFOllower;
begin
 Result := fListHandleFollower.Follower;
end;

function TBoldCustomComboBox.GetContextForBoldProperties: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomComboBox.GetContextForBoldRowProperties: TBoldElementTypeInfo;
begin
  if assigned(BoldListHandle) then
    result := BoldListHandle.StaticBoldType
  else
    result := nil;
end;

{$IFNDEF BOLDCOMCLIENT}
function TBoldCustomComboBox.ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean;
begin
  // We want to evaluate everything. Thus suboptimized expressions.
  Result := ComponentValidator.ValidateExpressionInContext(
              BoldProperties.Expression,
              GetContextForBoldProperties,
              format('%s%s.BoldProperties', [NamePrefix, Name])); // Do not localize
  Result := ComponentValidator.ValidateExpressionInContext(
              BoldSetValueExpression,
              GetContextForBoldProperties,
              format('%s%s.BoldSetValueExpression', [NamePrefix, Name])) and Result; // Do not localize
  Result := ComponentValidator.ValidateExpressionInContext(
              BoldRowProperties.Expression,
              GetContextForBoldRowProperties,
              format('%s%s.BoldRowProperties', [NamePrefix, Name])) and Result; // Do not localize
end;
{$ENDIF}

procedure TBoldCustomComboBox.SetBoldSelectChangeAction(Value: TBoldComboSelectChangeAction);
begin
  if (Value = bdcsSetReference) and assigned(BoldHandle) and not BoldHandle.CanSetValue then
    raise EBold.Create('The BoldSelectChangeAction property can not be bdscSetReference when BoldHandle is not a TBoldReferenceHandle');
  fBoldSelectChangeAction := Value;
end;

function TBoldCustomComboBox.ComboAllowsTextEditing(
  BoldProperties: TBoldStringFollowerController;
  Follower: TBoldFollower): Boolean;
begin
  // this method is primarily intended for subclasses to stop editing of string-attributes
  // due to bad combo configuration (needed at SVT)
  result := true;
end;

function TBoldCustomComboBox.HandleApplyException(E: Exception; Elem: TBoldElement; var discard: Boolean): boolean;
var
  ExceptionHandler: TBoldExceptionHandler;
begin
  Discard := False;
  ExceptionHandler := TBoldExceptionHandler.FindExceptionHandler(self);
  Result := assigned(ExceptionHandler);
  if Result then
    ExceptionHandler.HandleApplyException(E, self, Elem, Discard, Result);
end;

procedure TBoldCustomComboBox.Click;
var
  aMsg: TWMCommand;
begin
  Inc(fProcessingClick);
  try
    inherited;
    if fProcessingClick=1 then
    begin
      with aMsg do begin
        Msg := WM_COMMAND;
        NotifyCode := CBN_SELCHANGE;
        ItemID := ItemIndex;
        Ctl := Handle;
        Result := 0;
      end;
{$WARN UNSAFE_CAST OFF}
      WndProc(TMessage(aMsg));
{$WARN UNSAFE_CAST ON}
    end;
  finally
    Dec(fProcessingClick);
  end;
end;

procedure TBoldCustomComboBox.WMChar(var Message: TWMChar);
begin
  fAutoSearch := true;
  inherited;
  fAutoSearch := false;
end;

end.

