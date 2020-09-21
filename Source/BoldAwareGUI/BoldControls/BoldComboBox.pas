unit BoldComboBox;

{$UNDEF BOLDCOMCLIENT}

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
     property NilElementMode;
  end;

  TBoldCustomComboBox = class(TCustomComboBox, IBoldValidateableComponent)
  private
    fAlignment: TAlignment;
    fHandleFollower: TBoldElementHandleFollower;
    fListHandleFollower: TBoldListHandleFollower;
    fBoldProperties: TBoldStringFollowerController;
    fBoldListProperties: TBoldComboListController;
    fBoldRowProperties: TBoldStringFollowerController;
    fBoldSelectChangeAction: TBoldComboSelectChangeAction;
    fBoldSetValueExpression: TBoldExpression;
    fColor: TColor;
    fEffectiveReadOnly: Boolean;
    fFocused: Boolean;
    fFont: TFont;
    fFontHeight: Integer;
    fFontScaleFlag: Boolean;
    fReadOnly: Boolean;
    fOnSelectChanged: TNotifyEvent;
    fIsEditEvent: boolean;
    fOnSelectChangedIsCalled:Boolean;
    function GetBoldHandle: TBoldElementHandle;
    procedure SetBoldHandle(value: TBoldElementHandle);
    function GetFollower: TBoldFOllower;
    function GetBoldListHandle: TBoldAbstractListHandle;
    procedure SetBoldListHandle(value: TBoldAbstractListHandle);
    function GetListFollower: TBoldFOllower;
      procedure _InsertItem(Follower: TBoldFollower);
    procedure _DeleteItem(Index: Integer; OwningFollower: TBoldFollower);
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
  protected
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    property SelectedElement: TBoldElement read GetSelectedElement;
  end;

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
    {$ENDIF}
  end;

implementation

uses
  BoldExceptionHandlers,
  BoldReferenceHandle,
  {$IFNDEF BOLDCOMCLIENT}
  BoldSystem,
  {$ENDIF}
  BoldControlPackDefs,
  BoldListControlPack,
  BoldQueue,
  BoldGuiResourceStrings;

{ TBoldCustomComboBox }

type
  TWinControlHack = class(TWinControl) {We need access to Parents Font and Color property}
  end;

procedure TBoldCustomComboBox._InsertItem(Follower: TBoldFollower);
begin
  Items.Insert(Follower.Index, '');
end;

procedure TBoldCustomComboBox._DeleteItem(index: Integer; OwningFollower: TBoldFollower);
begin
  Items.Delete(index);
end;

procedure TBoldCustomComboBox._RowAfterMakeUptoDate(Follower: TBoldFollower);
var
  index: Integer;
begin
  index := Follower.index;
  if (index > -1) and (index < Items.Count) then
    Items[index] := TBoldStringFollowerController(Follower.Controller).GetCurrentAsString(Follower);
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

  UpdateEffectiveColor;
  UpdateEffectiveReadOnly;
  UpdateEffectiveFont;
  NewText := BoldProperties.GetCurrentAsString(Follower);
  if inherited Text <> NewText then
    SetEffectiveText(NewText);
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
    Follower.Apply;
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
      SetFont(TFont(Message.lParam))
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
  with fBoldListProperties do
  begin
    OnAfterInsertItem := _InsertItem;
    OnAfterDeleteItem := _DeleteItem;
  end;
  fHandleFollower := TBoldElementHandleFollower.Create(Owner, fBoldProperties);
  fListHandleFollower := TBoldListHandleFollower.Create(Owner, fBoldListProperties);
  fHandleFollower.PrioritizedQueuable := fListHandleFollower;
  fHandleFollower.StronglyDependedOfPrioritized := true;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FColor := clWindow;
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
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) then
  begin
    if (Style <> csDropDownList) and
       (BoldSelectChangeAction <> bdcsSetReference) and
       not BoldProperties.ValidateCharacter(Key, Follower) then
    begin
      MessageBeep(0);
      Key := BOLDNULL;
    end;
  end;
  if Key = BOLDESC then
  begin
    Follower.DiscardChange;
    SelectAll;
    Key := BOLDNULL;
  end;
end;

procedure TBoldCustomComboBox.SetBoldHandle(Value: TBoldElementHandle);
begin
  if assigned(Value) and (BoldSelectChangeAction = bdcsSetReference) and
      not (Value is TBoldReferenceHandle) then
    raise EBold.Create(sHandleMustBeReferenceHandle);
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
  CallInherited := True;
  if not (csDesigning in ComponentState) then
  begin
    case Message.Msg of
      CB_SETCURSEL,
      WM_COMMAND:
        if ((Message.Msg = CB_SETCURSEL) and (BoldSelectChangeAction = bdcsSetReference)) or
           (TWMCommand(Message).NotifyCode = CBN_SELCHANGE) then
        begin
          if (Message.Msg = CB_SETCURSEL) and assigned(BoldListHandle) and (BoldListHandle.Count > Message.WParam) then
          begin
            if Message.WParam = -1 then
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
              Follower.DiscardChange;
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
                    ElementToAssignTo.Assign(LocalSelectedElement);  // checkme take from follwer instead?
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
              CallInherited := false;
            end;
            bdcsSetReference:
            begin
              Follower.DiscardChange;
              if assigned(BoldHandle) and (BoldHandle is TBoldReferenceHandle) then
                (BoldHandle as TBoldReferenceHandle).Value := LocalSelectedElement;
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

      CB_SHOWDROPDOWN:
        if (Message.WParam=0) and EffectiveReadOnly then
          _AfterMakeUptoDate(Follower); {Restore text} //FIXME Maybe an UpdateEffectiveText?
    end;
  end;
  if CallInherited then
    inherited WndProc(Message);
end;

procedure TBoldCustomComboBox.SetBoldListProperties(Value: TBoldComboListController);
begin
  fBoldListProperties.Assign(Value);
end;

function TBoldCustomComboBox.GetSelectedElement: TBoldElement;
begin
  with ListFollower.RendererData as TBoldFollowerList do
  begin
    if (ItemIndex >= 0)and (ItemIndex < Count) then
      Result := Followers[ItemIndex].Element
    else
      Result := nil;
  end;
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
  if (Value = bdcsSetReference) and assigned(BoldHandle) and
     not (BoldHandle is TBoldReferenceHandle) then
    raise EBold.Create(sChangeActionCannotBeSetReference);
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

end.
