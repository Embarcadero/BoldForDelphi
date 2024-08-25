{ Global compiler directives }
{$include bold.inc}
unit BoldMemo;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Windows,
  Classes,
  Graphics,
  Controls,
  StdCtrls,
  Menus,
  BoldEnvironmentVCL,
  BoldHandles,
  BoldElements,
  BoldControlPack,
  BoldStringControlPack,
  BoldElementHandleFollower,
  BoldDefs;

type
  TBoldCustomMemo = class;
  TBoldMemo = class;

  {---TBoldCustomMemo---}
  TBoldCustomMemo = class(TCustomMemo, IBoldOCLComponent)
  private
    {Bold stuff}
    fAlignment: TAlignment;
    fBoldProperties: TBoldStringFollowerController;
    fCanvas: TControlCanvas;
    fFocused: Boolean;
    fHandleFollower: TBoldElementHandleFollower;
    fMyColor: TColor;
    fMyFont: TFont;
    fMyReadOnly: Boolean;
    fMaxLength: integer;
    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetExpression: TBoldExpression;
    function GetVariableList: TBoldExternalVariableList;

    procedure _FontChanged(sender: TObject);
    procedure AfterMakeUptoDate(Follower: TBoldFollower);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetBoldHandle: TBoldElementHandle;
    function GetColor: TColor;
    function GetEffectiveColor: TColor;
    function GetEffectiveFont: TFont;
    function GetEffectiveReadOnly: Boolean;
    function GetFollower: TBoldFollower;
    function GetFont: TFont;
    function GetText: string;
    procedure SetBoldDisplay(Value: TBoldStringFollowerController);
    procedure SetBoldHandle(value: TBoldElementHandle);
    procedure SetColor(value: TColor);
    procedure SetEffectiveColor(v: TColor);
    procedure SetFocused(Value: Boolean);
    procedure SetFont(value: TFont);
    procedure SetReadOnly(value: Boolean);
    procedure SetText(value: string);
  protected
    {Bold Stuff}
    procedure Change; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    function GetPopupMenu: TPopupMenu; override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    property Alignment: TAlignment read fAlignment write fAlignment;
    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldStringFollowerController read fBoldProperties write SetBoldDisplay;
    property Color: TColor read GetColor write SetColor default clWindow;
    property EffectiveColor: TColor read GetEffectiveColor write SetEffectiveColor;
    property EffectiveFont: TFont read GetEffectiveFont;
    property EffectiveReadOnly: Boolean read GetEffectiveReadOnly;
    property Follower: TBoldFollower read GetFollower;
    property Font: TFont read GetFont write SetFont;
    property ReadOnly: Boolean read fMyReadOnly write SetReadOnly;
    property Text: string read GetText write SetText;
    property MaxLength: integer read fMaxLength write fMaxLength;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  end;

  {---TBoldMemo---}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldMemo = class(TBoldCustomMemo)
  public
    {$IFNDEF T2H}
    property Text;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BoldHandle;
    property BoldProperties;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property WantReturns;
    property WantTabs;
    property Visible;
    property WordWrap;
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
  SysUtils,
  BoldUtils,
  BoldControlPackDefs;

{---TBoldCustomMemo---}
constructor TBoldCustomMemo.Create(AOwner: TComponent);
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
  inherited ReadOnly := True;
end;

destructor TBoldCustomMemo.Destroy;
begin
  case BoldProperties.ApplyPolicy of
    bapChange,
    bapExit: try
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

procedure TBoldCustomMemo.Loaded;
begin
  inherited;
  EffectiveColor := Color;
end;

procedure TBoldCustomMemo.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomMemo.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomMemo.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, follower.Element, 0);
end;

procedure TBoldCustomMemo.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
end;

procedure TBoldCustomMemo.SetBoldHandle(value: TBoldElementHandle);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldCustomMemo.SetBoldDisplay(value: TBoldStringFollowerController);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomMemo.SetReadOnly(value: Boolean);
begin
  if fMyReadOnly <> value then
  begin
    fMyReadOnly := value;
    Follower.MarkValueOutOfDate;
  end;
end;

function TBoldCustomMemo.GetEffectiveReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
end;

procedure TBoldCustomMemo.SetFont(value: TFont);
begin
  if not (csLoading in ComponentState) then
    if fMyFont <> value then
      fMyFont.Assign(value);
end;

procedure TBoldCustomMemo._FontChanged(sender: TObject);
begin
  Follower.MarkValueOutOfDate;
end;

function TBoldCustomMemo.GetFont: TFont;
begin
  Result := fMyFont;
end;

function TBoldCustomMemo.GetEffectiveFont: TFont;
begin
  Result := inherited Font;
end;

procedure TBoldCustomMemo.SetColor(value: TColor);
begin
  if fMyColor <> value then
    fMyColor := value;
  if not (csLoading in ComponentState) then
    Follower.MarkValueOutOfDate;
end;

function TBoldCustomMemo.GetColor: TColor;
begin
  Result := fMyColor;
end;

function TBoldCustomMemo.GetEffectiveColor: TColor;
begin
  Result := inherited Color;
end;

procedure TBoldCustomMemo.SetEffectiveColor(v: TColor);
begin
  if EffectiveColor <> v then
    inherited Color := v;
end;

procedure TBoldCustomMemo.SetText(value: string);
begin
  if not (csLoading in ComponentState) then
    if not EffectiveReadOnly then
      inherited Text := value
    else
      raise Exception.CreateFmt('%s.Text: Not modifiable', [ClassName]);
end;

function TBoldCustomMemo.GetText: string;
begin
  Result := inherited Text;
end;

function TBoldCustomMemo.GetPopupmenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (not Assigned(Result)) and assigned(BoldHandle) then
    Result := BoldProperties.Popup.GetMenu(Self, BoldHandle.Value);
end;

procedure TBoldCustomMemo.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if CharInSet(Key, [#32..#255]) and
    not BoldProperties.ValidateCharacter(Key, Follower) then
  begin
    MessageBeep(0);
    Key := BOLDNULL;
  end;
  
  if Key = BOLDESC then
  begin
    Follower.DiscardChange;
    SelectAll;
    Key := BOLDNULL;
  end;
end;

procedure TBoldCustomMemo.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then
      Invalidate;
  end;
end;

procedure TBoldCustomMemo.Change;
begin
  inherited Change;
  if not (csDesigning in ComponentState) then
    BoldProperties.MayHaveChanged(text, Follower);
end;

procedure TBoldCustomMemo.AfterMakeUptoDate(Follower: TBoldFollower);
var
  newText: string;
  ec    : TColor;
  EffectiveMaxLength: integer;
  RendererDataMaxLength: integer;
begin
  newText := BoldProperties.GetCurrentAsString(Follower);
  if Text <> newText then
    inherited Text := newText;
  inherited ReadOnly := FMyReadOnly or
    not BoldProperties.MayModify(Follower);
  BoldProperties.SetFont(EffectiveFont, Font, Follower);
  ec := EffectiveColor;
  BoldProperties.SetColor(ec, Color, Follower);
  EffectiveColor := ec;
  EffectiveMaxLength := 0;
  RendererDataMaxLength := (Follower.RendererData as TBoldStringRendererData).MaxStringLength;

  if RendererDataMaxLength <> -1 then
    EffectiveMaxLength := RendererDataMaxLength;

  if (MaxLength > 0) and (MaxLength < EffectiveMaxLength) then
    EffectiveMaxLength := MaxLength;

  inherited MaxLength := EffectiveMaxLength;
end;

procedure TBoldCustomMemo.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
end;

procedure TBoldCustomMemo.CMExit(var Message: TCMExit);
begin
  if (Follower.Controller.ApplyPolicy = bapExit) then
    Follower.Apply; 
  SetFocused(False);
  DoExit;
end;

function TBoldCustomMemo.GetBoldHandle: TBoldElementHandle;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomMemo.GetFollower: TBoldFollower;
begin
  Result := fHandleFollower.Follower;
end;

function TBoldCustomMemo.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    Result := nil;
end;

function TBoldCustomMemo.GetExpression: TBoldExpression;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldCustomMemo.SetExpression(const Value: TBoldExpression);
begin
  BoldProperties.Expression := Value;
end;

function TBoldCustomMemo.GetVariableList: TBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

end.