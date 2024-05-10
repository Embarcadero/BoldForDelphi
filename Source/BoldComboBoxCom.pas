
{ Global compiler directives }
{$include bold.inc}
unit BoldComboBoxCom;

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
  SysUtils,
  Windows,

  // Bold
  BoldAbstractListHandleCom,
  BoldComObjectSpace_TLB,
  BoldComponentValidatorCom,
  BoldControlPackCom,
  BoldControlsDefs,
  BoldDefs,
  BoldElementHandleFollowerCom,
  BoldHandlesCom,
  BoldListHandleFollowerCom,
  BoldListListControlPackCom,
  BoldStringControlPackCom;

type
  {Forward declarations of all classes}
  TBoldCustomComboBoxCom = class;
  TBoldComboBoxCom = class;

  TBoldComboListControllerCom = class(TBoldAbstractListAsFollowerListControllerCom)
  published
     property NilElementMode;
  end;

  TBoldCustomComboBoxCom = class(TCustomComboBox, IBoldValidateableComponentCom)
  private
    fAlignment: TAlignment;
    fHandleFollower: TBoldElementHandleFollowerCom;
    fListHandleFollower: TBoldListHandleFollowerCom;
    fBoldProperties: TBoldStringFollowerControllerCom;
    fBoldListProperties: TBoldComboListControllerCom;
    fBoldRowProperties: TBoldStringFollowerControllerCom;
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
    function GetBoldHandle: TBoldElementHandleCom;
    procedure SetBoldHandle(value: TBoldElementHandleCom);
    function GetFollower: TBoldFollowerCom;
    function GetBoldListHandle: TBoldAbstractListHandleCom;
    procedure SetBoldListHandle(value: TBoldAbstractListHandleCom);
    function GetListFollower: TBoldFollowerCom;
      procedure _InsertItem(Follower: TBoldFollowerCom);
    procedure _DeleteItem(Index: Integer; OwningFollower: TBoldFollowerCom);
    procedure _RowAfterMakeUptoDate(Follower: TBoldFollowerCom);
    procedure _AfterMakeUptoDate(Follower: TBoldFollowerCom);
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
    function GetSelectedElement: IBoldElement;
    procedure SetBoldProperties(Value: TBoldStringFollowerControllerCom);
    procedure SetBoldListProperties(Value: TBoldComboListControllerCom);
    procedure SetRowProperties(Value: TBoldStringFollowerControllerCom);
    procedure SetCharCase(const Value: TEditCharCase);
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetReadOnly(Value: Boolean);
    procedure SetText(Value: string);
    function GetContextForBoldProperties: IBoldElementTypeInfo;
    function GetContextForBoldRowProperties: IBoldElementTypeInfo;
    {$IFNDEF BOLDCOMCLIENT}
    function ValidateComponent(ComponentValidator: TBoldComponentValidatorCom; NamePrefix: String): Boolean;
    {$ENDIF}
  protected
    function HandleApplyException(E: Exception; Elem: IBoldElement; var discard: Boolean): boolean;
    procedure Change; override;
    function ComboAllowsTextEditing(BoldProperties: TBoldStringFollowerControllerCom; Follower: TBoldFollowerCom): Boolean; virtual;
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
    property Follower: TBoldFollowerCom read GetFollower;
    property ListFollower: TBoldFollowerCom read GetListFollower;
    property BoldHandle: TBoldElementHandleCom read GetBoldHandle write SetBoldHandle;
    property BoldListHandleCom: TBoldAbstractListHandleCom read GetBoldListHandle write SetBoldListHandle;
    property BoldProperties: TBoldStringFollowerControllerCom read FBoldProperties write SetBoldProperties;
    property BoldListProperties: TBoldComboListControllerCom read fBoldListProperties write SetBoldListProperties;
    property BoldRowProperties: TBoldStringFollowerControllerCom read fBoldRowProperties write SetRowProperties;
    property BoldSelectChangeAction: TBoldComboSelectChangeAction read fBoldSelectChangeAction write SetBoldSelectChangeAction;
    property BoldSetValueExpression: TBoldExpression read fBoldSetValueExpression write fBoldSetValueExpression;
    property OnSelectChanged: TNotifyEvent read fOnSelectChanged write fOnSelectChanged;
    property CharCase write SetCharCase;
    property Color: TColor read FColor write SetColor stored IsColorStored default clWindow ;
    property EffectiveColor: TColor read GetEffectiveColor;
    property EffectiveFont: TFont read GetEffectiveFont;
    property EffectiveReadOnly: Boolean read FEffectiveReadOnly;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Text: string read GetText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    property SelectedElement: IBoldElement read GetSelectedElement;
  end;

  TBoldComboBoxCom = class(TBoldCustomComboBoxCom)
  {$IFNDEF T2H}
  public
    property EffectiveColor;
    property EffectiveFont;
    property EffectiveReadOnly;
    property Text;
  published
    property Alignment;
    property BoldHandle;
    property BoldListHandleCom;
    property BoldListProperties;
    property BoldProperties;
    property BoldRowProperties;
    property BoldSetValueExpression;
    property BoldSelectChangeAction;
    property OnSelectChanged;
    property CharCase; {Must be published before ReadOnly}
    property ReadOnly;
    {Properties in standard TComboBox}
    property Style; {Must be published before Items} 
    property Anchors;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;

    property ItemHeight;
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
  BoldExceptionHandlersCom,
  BoldReferenceHandleCom,
  {$IFNDEF BOLDCOMCLIENT}
  BoldComObjectSpace_TLB,
  {$ENDIF}
  BoldControlPackDefs,
  BoldListControlPackCom,
  BoldQueue;

{ TBoldCustomComboBoxCom }

type
  TWinControlHack = class(TWinControl) {We need access to Parents Font and Color property}
  end;

procedure TBoldCustomComboBoxCom._InsertItem(Follower: TBoldFollowerCom);
begin
  Items.Insert(Follower.Index, '');
end;

procedure TBoldCustomComboBoxCom._DeleteItem(index: Integer; OwningFollower: TBoldFollowerCom);
begin
  Items.Delete(index);
end;

procedure TBoldCustomComboBoxCom._RowAfterMakeUptoDate(Follower: TBoldFollowerCom);
var
  index: Integer;
begin
  index := Follower.index;
  if (index > -1) and (index < Items.Count) then
    Items[index] := TBoldStringFollowerControllerCom(Follower.Controller).GetCurrentAsString(Follower);
  Invalidate;
  fHandleFollower.Follower.MarkValueOutOfDate;
end;

procedure TBoldCustomComboBoxCom._AfterMakeUptoDate(Follower: TBoldFollowerCom);
var
  NewText: string;
begin



  UpdateEffectiveColor;
  UpdateEffectiveReadOnly;
  UpdateEffectiveFont;
  NewText := BoldProperties.GetCurrentAsString(Follower);
  if inherited Text <> NewText then
    SetEffectiveText(NewText);
end;

procedure TBoldCustomComboBoxCom.Change;
begin
  if not (csDesigning in ComponentState) and
    not EffectiveReadOnly and
    (BoldSelectChangeAction = bdscSetText) and
    ComboAllowsTextEditing(BoldProperties, Follower) then
    BoldProperties.MayHaveChanged(text, Follower);
  inherited;
end;

procedure TBoldCustomComboBoxCom.ChangeScale(M, D: Integer);
begin
  if (M <> D) and (not (csLoading in ComponentState) or fFontScaleFlag) and not ParentFont then
    Font.Size := MulDiv(Font.Size, M, D);
  fFontScaleFlag := False;
  inherited ChangeScale(M, D);
end;

procedure TBoldCustomComboBoxCom.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
end;

procedure TBoldCustomComboBoxCom.CMExit(var Message: TCMExit);
begin
  if not (csDestroying in ComponentState) and (Follower.Controller.ApplyPolicy = bapExit) then
    Follower.Apply;
  SetFocused(False);
  inherited;
end;

procedure TBoldCustomComboBoxCom.CMParentColorChanged(var Message: TMessage);
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

procedure TBoldCustomComboBoxCom.CMParentFontChanged(var Message: TMessage);
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

procedure TBoldCustomComboBoxCom.ComboWndProc(var Message: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        if (Style = csSimple) and (ComboWnd <> EditHandle) then
          if EffectiveReadOnly then Exit;
    end;
  fIsEditEvent := True;
  inherited ComboWndProc(Message, ComboWnd, ComboProc);
  fIsEditEvent := False;
end;

constructor TBoldCustomComboBoxCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FBoldProperties := TBoldStringFollowerControllerCom.Create(Self);
  FBoldProperties.AfterMakeUptoDate := _AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextForBoldProperties;
  fBoldRowProperties := TBoldStringFollowerControllerCom.Create(Self);
  fBoldRowProperties.AfterMakeUptoDate := _RowAfterMakeUptoDate;
  fBoldRowProperties.OnGetContextType := GetContextForBoldRowProperties;
  fBoldListProperties := TBoldComboListControllerCom.Create(Self, fBoldRowProperties);
  with fBoldListProperties do
  begin
    OnAfterInsertItem := _InsertItem;
    OnAfterDeleteItem := _DeleteItem;
  end;
  fHandleFollower := TBoldElementHandleFollowerCom.Create(Owner, fBoldProperties);
  fListHandleFollower := TBoldListHandleFollowerCom.Create(Owner, fBoldListProperties);
  fHandleFollower.PrioritizedQueuable := fListHandleFollower;
  fHandleFollower.StronglyDependedOfPrioritized := true;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FColor := clWindow;
end;

procedure TBoldCustomComboBoxCom.CreateWnd;
begin
  inherited CreateWnd;
  FEffectiveReadOnly := False;
  UpdateEffectiveReadOnly;
end;

destructor TBoldCustomComboBoxCom.Destroy;
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

procedure TBoldCustomComboBoxCom.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomComboBoxCom.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomComboBoxCom.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
end;

procedure TBoldCustomComboBoxCom.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, follower.Element, 0);
end;

procedure TBoldCustomComboBoxCom.FontChanged(sender: TObject);
begin
  ParentFont := False;
  if Font.Height <> FFontHeight then
  begin
    FFontScaleFlag := True;
    FFontHeight := Font.Height;
  end;
  UpdateEffectiveFont;
end;

function TBoldCustomComboBoxCom.GetEffectiveColor: TColor;
begin
  Result := inherited Color;
end;

function TBoldCustomComboBoxCom.GetEffectiveFont: TFont;
begin
  Result := inherited Font;
end;

function TBoldCustomComboBoxCom.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (not Assigned(Result)) and Assigned(Follower.Element) then
    Result := BoldProperties.Popup.GetMenu(Self, Follower.Element);
end;

function TBoldCustomComboBoxCom.GetText: string;
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

function TBoldCustomComboBoxCom.IsColorStored: Boolean;
begin
  Result := not ParentColor;
end;

function TBoldCustomComboBoxCom.IsFontStored: Boolean;
begin
  Result := not ParentFont;
end;

procedure TBoldCustomComboBoxCom.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if EffectiveReadOnly and (Key in [VK_UP, VK_DOWN]) then
    Key := 0;
end;

procedure TBoldCustomComboBoxCom.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) then
  begin
    if (Style <> csDropDownList) and
       (BoldSelectChangeAction <> bdcsSetReference) and
       not BoldProperties.ValidateCharacter(AnsiChar(Key), Follower) then
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

procedure TBoldCustomComboBoxCom.SetBoldHandle(Value: TBoldElementHandleCom);
begin
  if assigned(Value) and (BoldSelectChangeAction = bdcsSetReference) and
      not (Value is TBoldReferenceHandleCom) then
    raise EBold.Create('The BoldHandle property must be a TBoldReferenceHandleCom when BoldSelectChangeAction is bdscSetReference');
  fHandleFollower.BoldHandle := value;
end;

procedure TBoldCustomComboBoxCom.SetBoldListHandle(Value: TBoldAbstractListHandleCom);
begin
    fListHandleFollower.BoldHandle := value;
end;

procedure TBoldCustomComboBoxCom.SetBoldProperties(Value: TBoldStringFollowerControllerCom);
begin
  FBoldProperties.Assign(Value);
end;

procedure TBoldCustomComboBoxCom.SetCharCase(const Value: TEditCharCase);
begin
  if CharCase <> Value then
  begin
    inherited CharCase := Value;
  end;
end;

procedure TBoldCustomComboBoxCom.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    ParentColor := False;
    UpdateEffectiveColor;
  end;
end;

procedure TBoldCustomComboBoxCom.SetEffectiveReadOnly(Value: Boolean);
begin
  if Value <> FEffectiveReadOnly then
  begin
    FEffectiveReadOnly := Value;
    if (Style in [csDropDown, csSimple]) and HandleAllocated then
      SendMessage(EditHandle, EM_SETREADONLY, Ord(Value), 0);
  end;
end;

procedure TBoldCustomComboBoxCom.SetEffectiveText(Value: string);
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

procedure TBoldCustomComboBoxCom.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then Invalidate;
  end;
end;

procedure TBoldCustomComboBoxCom.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;  

procedure TBoldCustomComboBoxCom.SetReadOnly(Value: Boolean);
begin
  if (FReadOnly <> Value) then
  begin
    FReadOnly := Value;
    UpdateEffectiveReadOnly;
  end;
end;

procedure TBoldCustomComboBoxCom.SetRowProperties(Value: TBoldStringFollowerControllerCom);
 begin
  fBoldRowProperties.Assign(Value);
end;

procedure TBoldCustomComboBoxCom.SetText(Value: string);
begin
  if not EffectiveReadOnly then
    SetEffectiveText(Value);
end;

procedure TBoldCustomComboBoxCom.UpdateEffectiveColor;
var
  NewColor: TColor;
begin
  NewColor := Color;
  BoldProperties.SetColor(NewColor, Color, Follower);
  inherited Color := NewColor;
end;

procedure TBoldCustomComboBoxCom.UpdateEffectiveFont;
begin
  EffectiveFont.Assign(Font);
  BoldProperties.SetFont(EffectiveFont, Font, Follower);
end;

procedure TBoldCustomComboBoxCom.UpdateEffectiveReadOnly;
begin
  if BoldSelectChangeAction in [bdcsNone, bdcsSetValue, bdcsSetReference] then
    SetEffectiveReadOnly(FReadOnly)
  else
    SetEffectiveReadOnly(FReadOnly or not BoldProperties.MayModify(Follower));
end;

procedure TBoldCustomComboBoxCom.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

procedure TBoldCustomComboBoxCom.WndProc(var Message: TMessage);
var
  CallInherited: Boolean;
  ElementToAssignTo: IBoldElement;
  Discard: Boolean;
begin
  CallInherited := True;
  if not (csDesigning in ComponentState) then
  begin
    case Message.Msg of
      WM_COMMAND:
        if TWMCommand(Message).NotifyCode = CBN_SELCHANGE then
        begin
          case BoldSelectChangeAction of
            bdscSetText:
              CallInherited := not EffectiveReadOnly;
            bdcsSetValue:
            begin
              Follower.DiscardChange;
              if Assigned(BoldHandle) and Assigned(BoldHandle.value) then
              begin
                {$IFDEF BOLDCOMCLIENT}
                if trim(BoldSetValueExpression) <> '' then
                  ElementToAssignTo := BoldHandle.Value.EvaluateExpression(BoldSetValueExpression)
                else
                  elementToAssignTo := BoldHandle.Value;
                if assigned(ElementToAssignTo) then
                  ElementToAssignTo.AssignElement(SelectedElement);
                {$ELSE}
                if trim(BoldSetValueExpression) <> '' then
                  ElementToAssignTo := BoldHandle.Value.EvaluateExpressionAsDirectElement(BoldSetValueExpression)
                else
                  elementToAssignTo := BoldHandle.Value;

                if assigned(ElementToAssignTo) and ElementToAssignTo.Mutable and
                  (not (elementToAssignTo is IBoldMember) or
                   IBoldMember(ElementTOAssignTo).CanModify) then
                  try
                    ElementToAssignTo.Assign(SelectedElement);
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
              CallInherited := False;
            end;
            bdcsSetReference:
            begin
              Follower.DiscardChange;
              if assigned(BoldHandle) and (BoldHandle is TBoldReferenceHandleCom) then
                (BoldHandle as TBoldReferenceHandleCom).Value := SelectedElement;
              CallInherited := False;
            end;
            bdcsSetListIndex:
            begin
              Follower.DiscardChange;
              if assigned(BoldListHandleCom) then
                BoldListHandleCom.CurrentIndex := BoldListHandleCom.List.IndexOf(SelectedElement);
              CallInherited := False;
            end;
            bdcsNone:
            begin
              CallInherited := False;
              Invalidate;
            end;
          end;
          if assigned(OnSelectChanged) then
            fOnSelectChanged(Self);
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
            Change;
          end;
        end;

      CB_SHOWDROPDOWN:
        if (Message.WParam=0) and EffectiveReadOnly then
          _AfterMakeUptoDate(Follower); {Restore text}
    end;
  end;
  if CallInherited then
    inherited WndProc(Message);
end;

procedure TBoldCustomComboBoxCom.SetBoldListProperties(Value: TBoldComboListControllerCom);
begin
  fBoldListProperties.Assign(Value);
end;

function TBoldCustomComboBoxCom.GetSelectedElement: IBoldElement;
begin
  with ListFollower.RendererData as TBoldFollowerListCom do
  begin
    if (ItemIndex >= 0)and (ItemIndex < Count) then
      Result := Followers[ItemIndex].Element
    else
      Result := nil;
  end;
end;

function TBoldCustomComboBoxCom.GetBoldHandle: TBoldElementHandleCom;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomComboBoxCom.GetFollower: TBoldFollowerCom;
begin
    Result := fHandleFollower.Follower;
end;

function TBoldCustomComboBoxCom.GetBoldListHandle: TBoldAbstractListHandleCom;
begin
 Result := fListHandleFollower.BoldHandle;
end;

function TBoldCustomComboBoxCom.GetListFollower: TBoldFollowerCom;
begin
   Result := fListHandleFollower.Follower;
end;

function TBoldCustomComboBoxCom.GetContextForBoldProperties: IBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomComboBoxCom.GetContextForBoldRowProperties: IBoldElementTypeInfo;
begin
  if assigned(BoldListHandleCom) then
    result := BoldListHandleCom.StaticBoldType
  else
    result := nil;
end;

{$IFNDEF BOLDCOMCLIENT}
function TBoldCustomComboBoxCom.ValidateComponent(ComponentValidator: TBoldComponentValidatorCom; NamePrefix: String): Boolean;
begin
  Result := ComponentValidator.ValidateExpressionInContext(
              BoldProperties.Expression,
              GetContextForBoldProperties,
              format('%s%s.BoldProperties', [NamePrefix, Name]));
  Result := ComponentValidator.ValidateExpressionInContext(
              BoldSetValueExpression,
              GetContextForBoldProperties,
              format('%s%s.BoldSetValueExpression', [NamePrefix, Name])) and Result;
  Result := ComponentValidator.ValidateExpressionInContext(
              BoldRowProperties.Expression,
              GetContextForBoldRowProperties,
              format('%s%s.BoldRowProperties', [NamePrefix, Name])) and Result;
end;
{$ENDIF}

procedure TBoldCustomComboBoxCom.SetBoldSelectChangeAction(Value: TBoldComboSelectChangeAction);
begin
  if (Value = bdcsSetReference) and assigned(BoldHandle) and
     not (BoldHandle is TBoldReferenceHandleCom) then
    raise EBold.Create('The BoldSelectChangeAction property can not be bdscSetReference when BoldHandle is not a TBoldReferenceHandleCom');
  fBoldSelectChangeAction := Value;
end;

function TBoldCustomComboBoxCom.ComboAllowsTextEditing(
  BoldProperties: TBoldStringFollowerControllerCom;
  Follower: TBoldFollowerCom): Boolean;
begin

  result := true;
end;

function TBoldCustomComboBoxCom.HandleApplyException(E: Exception; Elem: IBoldElement; var discard: Boolean): boolean;
var
  ExceptionHandler: TBoldExceptionHandlerCom;
begin
  Discard := False;
  ExceptionHandler := TBoldExceptionHandlerCom.FindExceptionHandler(self);
  Result := assigned(ExceptionHandler);
  if Result then
    ExceptionHandler.HandleApplyException(E, self, Elem, Discard, Result);
end;

end.
