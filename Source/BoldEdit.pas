{ Global compiler directives }
{$include bold.inc}
unit BoldEdit;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  StdCtrls,
  Controls,
  Windows,
  Messages,
  Menus,
  Graphics,
  Buttons,
  BoldEnvironmentVCL,
  BoldControlsDefs,
  BoldHandles,
  BoldElements,
  BoldControlPack,
  BoldElementHandleFollower,
  BoldStringControlPack,
  BoldDefs;

type
  {Forward declarations of all classes}
  TBoldCustomEdit = class;
  TBoldEdit = class;

  {---TBoldCustomEdit---}
  TBoldCustomEdit = class(TCustomEdit, IBoldOCLComponent)
  private
    fHandleFollower: TBoldElementHandleFollower;
    fBoldProperties: TBoldStringFollowerController;
    fMyReadOnly: Boolean;
    fMyFont: TFont;
    fMyColor: TColor;
    fCanvas: TControlCanvas;
    fAlignment: TAlignment;
    fFocused: Boolean;
    fButton: TSpeedButton;
    fBtnControl: TWinControl;
    fButtonStyle: TBoldEditButtonStyle;
    fOnButtonClick: TNotifyEvent;
    fBeepOnInvalidKey: boolean;
    fMaxLength: integer;
    {Bold stuff}
    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetExpression: TBoldExpression;
    function GetVariableList: TBoldExternalVariableList;
    
    function GetBoldHandle: TBoldElementHandle;
    procedure SetBoldHandle(value: TBoldElementHandle);
    function GetFollower: TBoldFOllower;
    procedure SetBoldDisplay(Value: TBoldStringFollowerController);
    procedure SetReadOnly(value: Boolean);
    function GetEffectiveReadOnly: Boolean;
    function GetEffectiveFont: TFont;
    function GetFont: TFont;
    procedure SetFont(value: TFont);
    function GetEffectiveColor: TColor;
    procedure SetEffectiveColor(v: TColor);
    function GetColor: TColor;
    procedure SetColor(value: TColor);
    procedure SetText(value: string);
    function GetText: string;
    function GetTextMargins: TPoint;
    function GetButton: TSpeedButton;
    procedure SetFocused(Value: Boolean);
    procedure SetButtonStyle(const Value: TBoldEditButtonStyle);

    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_EnabledChanged;

    procedure AfterMakeUptoDate(Follower: TBoldFollower);
    procedure _FontChanged(sender: TObject);
  protected
    {Bold Stuff}
    procedure InvalidKey(Key: Char); virtual;
    procedure Change; override;
    function GetPopupMenu: TPopupMenu; override;
    procedure Loaded; override;
    procedure KeyPress(var Key: Char); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure ButtonCreate;
    procedure ButtonDestroy;
    procedure ButtonSetGlyph;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure SetEditRect;
    procedure ButtonClick; virtual;

    property BeepOnInvalidKey: boolean read fBeepOnInvalidKey write fBeepOnInvalidKey default True;
    property Button: TSpeedButton read GetButton;
    property ButtonControl: TWinControl read fBtnControl;
    property EffectiveReadOnly: Boolean read GetEffectiveReadOnly;
    property EffectiveFont: TFont read GetEffectiveFont;
    property EffectiveColor: TColor read GetEffectiveColor write SetEffectiveColor;

    property BoldHandle:TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property Follower: TBoldFollower read GetFollower;
    property BoldProperties: TBoldStringFollowerController read fBoldProperties write SetBoldDisplay;
    property ReadOnly: Boolean read fMyReadOnly write SetReadOnly;
    property Text: string read GetText write SetText;
    property Font: TFont read GetFont write SetFont;
    property Color: TColor read GetColor write SetColor default clWindow;
    property MaxLength: integer read fMaxLength write fMaxLength;
    property Alignment: TAlignment read fAlignment write fAlignment;
    property ButtonStyle: TBoldEditButtonStyle read fButtonStyle write SetButtonStyle;
    property OnButtonClick: TNotifyEvent read fOnButtonClick write fOnButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  end;

  {---TBoldEdit---}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldEdit = class(TBoldCustomEdit)
  public
    {$IFNDEF T2H}
    property Button;
    property EffectiveReadOnly;
    property EffectiveFont;
    property EffectiveColor;
  published
    {Properties from TBoldCustomEdit}
    property BoldHandle;
    property BoldProperties;
    property ReadOnly;
    property Font;
    property Color;
    property Alignment;
    property ButtonStyle;
    property OnButtonClick;
    {Properties from TCustomEdit}
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HideSelection;

    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    {$ENDIF}
  end;

implementation

uses
  BoldControlPackDefs,
  SysUtils,
  BoldUtils,  
  Forms,
  {$IFNDEF BOLDCOMCLIENT}
  BoldSystem,
  BoldGUI,
  BoldReferenceHandle,
  BoldRootedHandles,
  {$ENDIF}
  BoldCommonBitmaps;

{ TBoldComboButton }

type
  TBoldComboButton = class(TSpeedButton)
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;


{---TBoldCustomEdit---}
constructor TBoldCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  fBoldProperties := TBoldStringFollowerController.Create(Self);
  fBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollower.Create(Owner, fBoldProperties);
  fMyFont := TFont.Create;
  fMyFont.OnChange := _FontChanged;
  Color := clWindow;
  fBeepOnInvalidKey := True;
  inherited ReadOnly := True;
end;

destructor TBoldCustomEdit.Destroy;
begin
  case BoldProperties.ApplyPolicy of
    bapChange, bapExit: try
      Follower.Apply;
    except
      Follower.DiscardChange;
    end;
    bapDemand: Follower.DiscardChange;
  end;
  FreeAndNil(fHandleFollower);
  FreeAndNil(fCanvas);
  FreeAndNil(fBoldProperties);
  FreeAndNil(fMyFont);
  inherited Destroy;
end;

procedure TBoldCustomEdit.Loaded;
begin
  inherited;
  effectiveColor := Color;
end;

procedure TBoldCustomEdit.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomEdit.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomEdit.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  {$IFNDEF BOLDCOMCLIENT}
  if (BoldProperties.DropMode = bdpReplace) and
    (assigned(BoldHandle)) and
    (not assigned(BoldHandle.Value) or (BoldHandle.Value is TBoldObject)) and
    (BoldHandle is TBoldRootedHandle) and
    (Not assigned(TBoldRootedHandle(BoldHandle).RootHandle)) and
    (BoldGUIHandler.DraggedObjects.Count = 1) and
    (BoldGUIHandler.DraggedObjects[0] is TBoldObject) then
      accept := not assigned(BoldHandle.DynamicBoldType) or
            ((BoldGUIHandler.DraggedObjects[0] as TBoldObject).Boldtype.ConformsTo(BoldHandle.DynamicBoldType))
  else if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, Follower.element, 0);
  {$ENDIF}
end;

procedure TBoldCustomEdit.DragDrop(Source: TObject; X, Y: Integer);
begin
  {$IFNDEF BOLDCOMCLIENT}
  if (BoldProperties.DropMode = bdpReplace) and
    (assigned(BoldHandle)) and
    (not assigned(BoldHandle.Value) or (BoldHandle.Value is TBoldObject)) and
    (BoldHandle is TBoldReferenceHandle) and
   (BoldGUIHandler.DraggedObjects.Count = 1) and
    (BoldGUIHandler.DraggedObjects[0] is TBoldObject) and
    (((BoldGUIHandler.DraggedObjects[0] as TBoldObject).Boldtype.ConformsTo(BoldHandle.BoldType)) or
      not assigned(BoldHandle.BoldType)) then
    TBoldReferenceHandle(BoldHandle).Value := BoldGuiHandler.DraggedObjects[0]
  else if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
  {$ENDIF}
end;

procedure TBoldCustomEdit.SetBoldHandle(value: TBoldElementHandle);
begin
  fHandleFollower.BoldHandle := value;
end;

procedure TBoldCustomEdit.SetBoldDisplay(value: TBoldStringFollowerController);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomEdit.SetReadOnly(value: Boolean);
begin
  if fMyReadOnly <> value then
  begin
    fMyReadOnly := value;
    Follower.MarkValueOutOfDate;
  end;
end;

function TBoldCustomEdit.GetEffectiveReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TBoldCustomEdit.SetFont(value: TFont);
begin
  if not (csLoading in ComponentState) then
    if fMyFont <> value then
      fMyFont.Assign(value);
end;

procedure TBoldCustomEdit._FontChanged(sender: TObject);
begin
  Follower.MarkValueOutOfDate;
end;

function TBoldCustomEdit.GetFont: TFont;
begin
  Result := fMyFont;
end;

function TBoldCustomEdit.GetEffectiveFont: TFont;
begin
  Result := inherited Font;
end;

procedure TBoldCustomEdit.SetColor(value: TColor);
begin
  if fMyColor <> Value then
    fMyColor := Value;
  if not (csLoading in ComponentState) then
    Follower.MarkValueOutOfDate;
end;

function TBoldCustomEdit.GetColor: TColor;
begin
  Result := fMyColor;
end;

function TBoldCustomEdit.GetEffectiveColor: TColor;
begin
  Result := inherited Color;
end;

procedure TBoldCustomEdit.SetEffectiveColor(v: TColor);
begin
  if EffectiveColor <> v then
    inherited Color := v;
end;

procedure TBoldCustomEdit.SetText(value: string);
begin
  if not (csLoading in ComponentState) then
    if not EffectiveReadOnly then
      inherited Text := value
    else
      raise EBold.CreateFmt('%s.Text: Not modifiable', [ClassName]);
end;

function TBoldCustomEdit.GetText: string;
begin
  Result := inherited Text;
end;

function TBoldCustomEdit.GetPopupmenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (not Assigned(Result)) and assigned(BoldHandle) then
  begin
    Result := BoldProperties.Popup.GetMenu(Self, BoldHandle.Value);
    if assigned(result) and (Result.Items.Count = 0) then
      Result := nil;
  end;
end;

procedure TBoldCustomEdit.InvalidKey(Key: Char);
begin
  if BeepOnInvalidKey then
    MessageBeep(0);
end;

procedure TBoldCustomEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if CharInSet(Key, [#32..#255]) and
     not BoldProperties.ValidateCharacter(Key, Follower) then
  begin
    InvalidKey(Key);
    Key := BOLDNULL;
  end
  else if Key = #1 then
  begin
    SelectAll;
  end
  else if Key = BOLDESC then
  begin
    Follower.DiscardChange;
    SelectAll;
    Key := BOLDNULL;
  end;
end;

procedure TBoldCustomEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then Invalidate;
  end;
end;

procedure TBoldCustomEdit.Change;
begin
  inherited Change;
  if not (csDesigning in ComponentState) and
    not effectiveReadOnly then
    BoldProperties.MayHaveChanged(text, Follower);
end;

procedure TBoldCustomEdit.AfterMakeUptoDate(Follower: TBoldFollower);
var
  newText: string;
  ec    : TColor;
  EffectiveMaxLength: integer;
  RendererDataMaxLength: integer;
begin
  newText := BoldProperties.GetCurrentAsString(Follower);
  if Text <> newText then
    inherited Text := newText;
  inherited ReadOnly := FMyReadOnly or not BoldProperties.MayModify(Follower) or (charCase <> ecNormal);
  BoldProperties.SetFont(EffectiveFont, Font, Follower);

  RendererDataMaxLength := (Follower.RendererData as TBoldStringRendererData).MaxStringLength;

  if RendererDataMaxLength <> -1 then
    EffectiveMaxLength := RendererDataMaxLength
  else
    EffectiveMaxLength := MaxLength;

  if (MaxLength > 0) and (MaxLength < EffectiveMaxLength) then
    EffectiveMaxLength := MaxLength;

  inherited MaxLength := EffectiveMaxLength;

  ec := EffectiveColor;
  BoldProperties.SetColor(ec, Color, Follower);
  EffectiveColor := ec;
end;


procedure TBoldCustomEdit.CMEnter(var message: TCMEnter);
begin
  SetFocused(True);
  inherited;
end;

procedure TBoldCustomEdit.CMExit(var message: TCMExit);
begin
  if (Follower.controller.ApplyPolicy = bapExit) then
    Follower.Apply;
 (* CHECKME if ((fFollower.ApplyPolicy = bapChange) and //NOTE: Added by jesper, mainly to handle date/time with bapChange
    (fFollower.state = bobsDirty)) then
  begin
    fFollower.state := bobsCurrent;
    Change;
  end;     *)
  SetFocused(False);
  inherited;
end;

procedure TBoldCustomEdit.WMPaint(var message: TWMPaint);
var
  R     : TRect;
  DC    : HDC;
  PS    : TPaintStruct;
  S     : string;
begin
  if ((FAlignment = taLeftJustify) or FFocused) and
    not (csPaintCopy in ControlState) then
  begin
    inherited;
    Exit;
  end;
  { Since edit controls do not handle justification unless multi-line (and
  then only poorly) we will draw right and center justify manually unless
  the edit has the focus. }
  if FCanvas = nil then
  begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;
  DC := message.DC;
  if DC = 0 then
    DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := EffectiveFont;
    with FCanvas do
    begin
      R := ClientRect;
      if (fButtonStyle<>bbsNone) then
        R.Right := fBtnControl.Left;
      if not (NewStyleControls and Ctl3D) and (BorderStyle = bsSingle) then
      begin
        Brush.Color := clWindowFrame;
        FrameRect(R);
        InflateRect(R, -1, -1);
      end;
      Brush.Color := EffectiveColor;
      if (csPaintCopy in ControlState) then
      begin
        if Assigned(BoldHandle) then
          S := BoldProperties.GetCurrentAsString(Follower)
        else
          S := '';
        case CharCase of
          ecUpperCase: S := AnsiUpperCase(S);
          ecLowerCase: S := AnsiLowerCase(S);
        end;
      end
      else
        S := Text;
      if PasswordChar <> BOLDNULL then
        FillChar(S[1], Length(S), PasswordChar);
      TBoldAsStringRenderer.DrawStringOnCanvas(fCanvas, R, fAlignment, GetTextMargins, s);
    end;
  finally
    FCanvas.Handle := 0;
    if message.DC = 0 then
      EndPaint(Handle, PS);
  end;
end;

function TBoldCustomEdit.GetTextMargins: TPoint;
var
  DC      : HDC;
  SaveFont: HFont;
  I       : Integer;
  SysMetrics,
    Metrics: TTextMetric;
begin
  if NewStyleControls then
  begin
    if BorderStyle = bsNone then
      I := 0
    else if Ctl3D then
      I := 1
    else
      I := 2;
    Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
    Result.Y := I;
  end else
  begin
    if BorderStyle = bsNone then
      I := 0
    else
    begin
      DC := GetDC(0);
      GetTextMetrics(DC, SysMetrics);
      SaveFont := SelectObject(DC, Font.Handle);
      GetTextMetrics(DC, Metrics);
      SelectObject(DC, SaveFont);
      ReleaseDC(0, DC);
      I := SysMetrics.tmHeight;
      if I > Metrics.tmHeight then
        I := Metrics.tmHeight;
      I := I div 4;
    end;
    Result.X := I;
    Result.Y := I;
  end;
end;

procedure TBoldCustomEdit.ButtonCreate;
begin
  Assert(not Assigned(fBtnControl), 'fBtnControl is already created');
  fBtnControl := TWinControl.Create(Self);
  fBtnControl.Width := 17;
  fBtnControl.Height := 17;
  fBtnControl.Visible := True;
  fBtnControl.Parent := Self;
  fButton := TBoldComboButton.Create(Self);
  fButton.SetBounds(0, 0, FBtnControl.Width, FBtnControl.Height);
  fButton.Visible := True;
  fButton.Parent := fBtnControl;
end;

procedure TBoldCustomEdit.ButtonDestroy;
begin
  FreeAndNil(fButton);
  FreeAndNil(fBtnControl);
end;

procedure TBoldCustomEdit.ButtonSetGlyph;
begin
  case fButtonStyle of
    bbsCombo: fButton.Glyph.Handle := LoadBitmap(0, PChar(32738));
    bbsEllipsis: fButton.Glyph := bmpBoldEdit_Ellipsis;
    bbsCustom: fButton.Glyph := nil;
  end;
end;

procedure TBoldCustomEdit.CMEnabledChanged(var message: TMessage);
begin
  inherited;
  if Assigned(fButton) then
    fButton.Enabled := Enabled;
end;

procedure TBoldCustomEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if (fButtonStyle<>bbsNone) then
    Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TBoldCustomEdit.CreateWnd;
begin
  inherited CreateWnd;
  if (fButtonStyle<>bbsNone) then
  begin
    ButtonCreate;
    ButtonSetGlyph;
    SetEditRect;
  end;
end;

procedure TBoldCustomEdit.DestroyWnd;
begin
  ButtonDestroy;
  inherited DestroyWnd;
end;

procedure TBoldCustomEdit.SetEditRect;
var
  Loc: TRect;
begin
  Loc.Bottom := ClientHeight+1;
  Loc.Right := ClientWidth-1;
  Loc.Top := 0;
  Loc.Left := 0;
  if Assigned(fBtnControl) then
  begin
    if NewStyleControls then
      fBtnControl.SetBounds(ClientWidth - fButton.Width, 0, fButton.Width, ClientHeight)
    else
      fBtnControl.SetBounds(ClientWidth - fButton.Width, 1, fButton.Width, ClientHeight - 1);
    fButton.Height := fBtnControl.Height;
    if (fButtonStyle<>bbsNone) then
      Loc.Right := fBtnControl.Left - 2;
  end;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
end;

procedure TBoldCustomEdit.WMSize(var message: TWMSize);

begin
  inherited;




  if Assigned(fBtnControl) and Assigned(fButton) then
    SetEditRect;
end;

procedure TBoldCustomEdit.SetButtonStyle(const Value: TBoldEditButtonStyle);
begin
  if (Value <> fButtonStyle) then
  begin
    fButtonStyle := Value;
    if HandleAllocated then
    begin
      if (fButtonStyle = bbsNone) then
      begin
        if Assigned(fBtnControl) then
          fBtnControl.Visible := False;
        if (csDesigning in ComponentState) then
          RecreateWnd;
      end
      else
      begin
        if not Assigned(fButton) then
          ButtonCreate;
        ButtonSetGlyph;
        fBtnControl.Visible := True;
      end;
      SetEditRect;
      Invalidate;
    end;
  end;
end;

function TBoldCustomEdit.GetButton: TSpeedButton;
begin
  if not Assigned(fButton) then
  begin
    fButtonStyle := bbsCustom;
    ButtonCreate;
  end;
  Result := fButton;
end;

procedure TBoldCustomEdit.ButtonClick;
begin
  if Assigned(fOnButtonClick) then
    fOnButtonClick(Self);
end;

{ TBoldComboButton }

procedure TBoldComboButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  with TBoldCustomEdit(Parent.Parent) do
    if (Handle <> GetFocus) and CanFocus then
    begin
      SetFocus;
      if GetFocus <> Handle then Exit;
    end;
  inherited MouseDown (Button, Shift, X, Y);
  with TBoldCustomEdit(Parent.Parent) do
    ButtonClick;
end;

procedure TBoldComboButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove (Shift, X, Y);

end;

function TBoldCustomEdit.GetBoldHandle: TBoldElementHandle;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomEdit.GetFollower: TBoldFOllower;
begin
    Result := fHandleFollower.Follower;
end;

function TBoldCustomEdit.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomEdit.GetExpression: TBoldExpression;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldCustomEdit.SetExpression(const Value: TBoldExpression);
begin
  Assert(Assigned(BoldProperties));
  BoldProperties.Expression := Value;
end;

function TBoldCustomEdit.GetVariableList: TBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;
  
end.