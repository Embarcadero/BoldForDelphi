
{ Global compiler directives }
{$include bold.inc}
unit BoldEditCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  // VCL
  Buttons,
  Classes,
  Controls,
  Graphics,
  Menus,
  Messages,
  StdCtrls,
  Windows,

  // Bold
  BoldClientElementSupport,
  BoldComObjectSpace_TLB,
  BoldControlPackCom,
  BoldControlsDefs,
  BoldElementHandleFollowerCom,
  BoldHandlesCom,
  BoldStringControlPackCom;

type
  {Forward declarations of all classes}
  TBoldCustomEditCom = class;
  TBoldEditCom = class;

  {---TBoldCustomEditCom---}
  TBoldCustomEditCom = class(TCustomEdit, IBoldOCLComponentCom)
  private
    fHandleFollower: TBoldElementHandleFollowerCom;
    fBoldProperties: TBoldStringFollowerControllerCom;
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
    function GetContextType: IBoldElementTypeInfo;
    procedure SetExpression(Expression: String);
    function GetExpression: String;
    function GetVariableList: IBoldExternalVariableList;
    
    function GetBoldHandle: TBoldElementHandleCom;
    procedure SetBoldHandle(value: TBoldElementHandleCom);
    function GetFollower: TBoldFollowerCom;
    procedure SetBoldDisplay(Value: TBoldStringFollowerControllerCom);
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

    procedure AfterMakeUptoDate(Follower: TBoldFollowerCom);
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

    property BoldHandle:TBoldElementHandleCom read GetBoldHandle write SetBoldHandle;
    property Follower: TBoldFollowerCom read GetFollower;
    property BoldProperties: TBoldStringFollowerControllerCom read fBoldProperties write SetBoldDisplay;
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

  {---TBoldEditCom---}
  TBoldEditCom = class(TBoldCustomEditCom)
  public
    {$IFNDEF T2H}
    property Button;
    property EffectiveReadOnly;
    property EffectiveFont;
    property EffectiveColor;
  published
    {Properties from TBoldCustomEditCom}
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
  Forms,
  {$IFNDEF BOLDCOMCLIENT}
  BoldComObjectSpace_TLB,
  BoldGUI,
  BoldReferenceHandleCom,
  BoldRootedHandlesCom,
  {$ENDIF}
  BoldDefs,
  BoldCommonBitmaps;

{ TBoldComboButton }

type
  TBoldComboButton = class(TSpeedButton)
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  end;


{---TBoldCustomEditCom---}
constructor TBoldCustomEditCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  fBoldProperties := TBoldStringFollowerControllerCom.Create(Self);
  fBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollowerCom.Create(Owner, fBoldProperties);
  fMyFont := TFont.Create;
  fMyFont.OnChange := _FontChanged;
  Color := clWindow;
  fBeepOnInvalidKey := True;
  inherited ReadOnly := True;
end;

destructor TBoldCustomEditCom.Destroy;
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

procedure TBoldCustomEditCom.Loaded;
begin
  inherited;
  effectiveColor := Color;
end;

procedure TBoldCustomEditCom.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomEditCom.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomEditCom.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  {$IFNDEF BOLDCOMCLIENT}
  if (BoldProperties.DropMode = bdpReplace) and
    (assigned(BoldHandle)) and
    (not assigned(BoldHandle.Value) or (BoldHandle.Value is IBoldObject)) and
    (BoldHandle is TBoldRootedHandleCom) and
    (Not assigned(TBoldRootedHandleCom(BoldHandle).RootHandle)) and
    (BoldGUIHandler.DraggedObjects.Count = 1) and
    (BoldGUIHandler.DraggedObjects[0] is IBoldObject) then
      accept := not assigned(BoldHandle.DynamicBoldType) or
            ((BoldGUIHandler.DraggedObjects[0] as IBoldObject).Boldtype.ConformsTo(BoldHandle.DynamicBoldType))
  else if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, Follower.element, 0);
  {$ENDIF}
end;

procedure TBoldCustomEditCom.DragDrop(Source: TObject; X, Y: Integer);
begin
  {$IFNDEF BOLDCOMCLIENT}
  if (BoldProperties.DropMode = bdpReplace) and
    (assigned(BoldHandle)) and
    (not assigned(BoldHandle.Value) or (BoldHandle.Value is IBoldObject)) and
    (BoldHandle is TBoldReferenceHandleCom) and
   (BoldGUIHandler.DraggedObjects.Count = 1) and
    (BoldGUIHandler.DraggedObjects[0] is IBoldObject) and
    (((BoldGUIHandler.DraggedObjects[0] as IBoldObject).Boldtype.ConformsTo(BoldHandle.BoldType)) or
      not assigned(BoldHandle.BoldType)) then
    TBoldReferenceHandleCom(BoldHandle).Value := BoldGuiHandler.DraggedObjects[0]
  else if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
  {$ENDIF}
end;

procedure TBoldCustomEditCom.SetBoldHandle(value: TBoldElementHandleCom);
begin
  fHandleFollower.BoldHandle := value;
end;

procedure TBoldCustomEditCom.SetBoldDisplay(value: TBoldStringFollowerControllerCom);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomEditCom.SetReadOnly(value: Boolean);
begin
  if fMyReadOnly <> value then
  begin
    fMyReadOnly := value;
    Follower.MarkValueOutOfDate;
  end;
end;

function TBoldCustomEditCom.GetEffectiveReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TBoldCustomEditCom.SetFont(value: TFont);
begin
  if not (csLoading in ComponentState) then
    if fMyFont <> value then
      fMyFont.Assign(value);
end;

procedure TBoldCustomEditCom._FontChanged(sender: TObject);
begin
  Follower.MarkValueOutOfDate;
end;

function TBoldCustomEditCom.GetFont: TFont;
begin
  Result := fMyFont;
end;

function TBoldCustomEditCom.GetEffectiveFont: TFont;
begin
  Result := inherited Font;
end;

procedure TBoldCustomEditCom.SetColor(value: TColor);
begin
  if fMyColor <> Value then
    fMyColor := Value;
  if not (csLoading in ComponentState) then
    Follower.MarkValueOutOfDate;
end;

function TBoldCustomEditCom.GetColor: TColor;
begin
  Result := fMyColor;
end;

function TBoldCustomEditCom.GetEffectiveColor: TColor;
begin
  Result := inherited Color;
end;

procedure TBoldCustomEditCom.SetEffectiveColor(v: TColor);
begin
  if EffectiveColor <> v then
    inherited Color := v;
end;

procedure TBoldCustomEditCom.SetText(value: string);
begin
  if not (csLoading in ComponentState) then
    if not EffectiveReadOnly then
      inherited Text := value
    else
      raise EBold.CreateFmt('%s.Text: Not modifiable', [ClassName]);
end;

function TBoldCustomEditCom.GetText: string;
begin
  Result := inherited Text;
end;

function TBoldCustomEditCom.GetPopupmenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (not Assigned(Result)) and assigned(BoldHandle) then
  begin
    Result := BoldProperties.Popup.GetMenu(Self, BoldHandle.Value);
    if assigned(result) and (Result.Items.Count = 0) then
      Result := nil;
  end;
end;

procedure TBoldCustomEditCom.InvalidKey(Key: Char);
begin
  if BeepOnInvalidKey then
    MessageBeep(0);
end;

procedure TBoldCustomEditCom.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key in [#32..#255]) and
     not BoldProperties.ValidateCharacter(AnsiChar(Key), Follower) then
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

procedure TBoldCustomEditCom.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then Invalidate;
  end;
end;

procedure TBoldCustomEditCom.Change;
begin
  inherited Change;
  if not (csDesigning in ComponentState) and
    not effectiveReadOnly then
    BoldProperties.MayHaveChanged(text, Follower);
end;

procedure TBoldCustomEditCom.AfterMakeUptoDate(Follower: TBoldFollowerCom);
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

  RendererDataMaxLength := (Follower.RendererData as TBoldStringRendererDataCom).MaxStringLength;

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


procedure TBoldCustomEditCom.CMEnter(var message: TCMEnter);
begin
  SetFocused(True);
  inherited;
end;

procedure TBoldCustomEditCom.CMExit(var message: TCMExit);
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

procedure TBoldCustomEditCom.WMPaint(var message: TWMPaint);
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
      TBoldAsStringRendererCom.DrawStringOnCanvas(fCanvas, R, fAlignment, GetTextMargins, s);
    end;
  finally
    FCanvas.Handle := 0;
    if message.DC = 0 then
      EndPaint(Handle, PS);
  end;
end;

function TBoldCustomEditCom.GetTextMargins: TPoint;
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

procedure TBoldCustomEditCom.ButtonCreate;
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

procedure TBoldCustomEditCom.ButtonDestroy;
begin
  FreeAndNil(fButton);
  FreeAndNil(fBtnControl);
end;

procedure TBoldCustomEditCom.ButtonSetGlyph;
begin
  case fButtonStyle of
    bbsCombo: fButton.Glyph.Handle := LoadBitmap(0, PChar(32738));
    bbsEllipsis: fButton.Glyph := bmpBoldEdit_Ellipsis;
    bbsCustom: fButton.Glyph := nil;
  end;
end;

procedure TBoldCustomEditCom.CMEnabledChanged(var message: TMessage);
begin
  inherited;
  if Assigned(fButton) then
    fButton.Enabled := Enabled;
end;

procedure TBoldCustomEditCom.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if (fButtonStyle<>bbsNone) then
    Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TBoldCustomEditCom.CreateWnd;
begin
  inherited CreateWnd;
  if (fButtonStyle<>bbsNone) then
  begin
    ButtonCreate;
    ButtonSetGlyph;
    SetEditRect;
  end;
end;

procedure TBoldCustomEditCom.DestroyWnd;
begin
  ButtonDestroy;
  inherited DestroyWnd;
end;

procedure TBoldCustomEditCom.SetEditRect;
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

procedure TBoldCustomEditCom.WMSize(var message: TWMSize);

begin
  inherited;




  if Assigned(fBtnControl) and Assigned(fButton) then
    SetEditRect;
end;

procedure TBoldCustomEditCom.SetButtonStyle(const Value: TBoldEditButtonStyle);
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

function TBoldCustomEditCom.GetButton: TSpeedButton;
begin
  if not Assigned(fButton) then
  begin
    fButtonStyle := bbsCustom;
    ButtonCreate;
  end;
  Result := fButton;
end;

procedure TBoldCustomEditCom.ButtonClick;
begin
  if Assigned(fOnButtonClick) then
    fOnButtonClick(Self);
end;

{ TBoldComboButton }

procedure TBoldComboButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  with TBoldCustomEditCom(Parent.Parent) do
    if (Handle <> GetFocus) and CanFocus then
    begin
      SetFocus;
      if GetFocus <> Handle then Exit;
    end;
  inherited MouseDown (Button, Shift, X, Y);
  with TBoldCustomEditCom(Parent.Parent) do
    ButtonClick;
end;

procedure TBoldComboButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove (Shift, X, Y);

end;

function TBoldCustomEditCom.GetBoldHandle: TBoldElementHandleCom;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomEditCom.GetFollower: TBoldFollowerCom;
begin
    Result := fHandleFollower.Follower;
end;

function TBoldCustomEditCom.GetContextType: IBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomEditCom.GetExpression: String;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldCustomEditCom.SetExpression(Expression: String);
begin
  Assert(Assigned(BoldProperties));
  BoldProperties.Expression := Expression;
end;

function TBoldCustomEditCom.GetVariableList: IBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

end.
