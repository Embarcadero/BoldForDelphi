{ Global compiler directives }
{$include bold.inc}
unit BoldLabel;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Messages,
  Classes,
  Graphics,
  Controls,
  StdCtrls,
  BoldEnvironmentVCL,
  BoldHandles,
  BoldControlPack,
  BoldElements,
  BoldStringControlPack,
  BoldElementHandleFollower,
  BoldDefs;

type
  {Forward declaration of classes}
  TBoldCustomLabel = class;
  TBoldLabel = class;

  { TBoldCustomLabel }
  TBoldCustomLabel = class(TCustomLabel, IBoldOCLComponent)
  private
    { Private declarations }
    fBoldProperties: TBoldStringFollowerController;
    fHandleFollower: TBoldElementHandleFollower;
    fMyColor: TColor;
    fMyFont: TFont;
    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetExpression: TBoldExpression;
    function GetVariableList: TBoldExternalVariableList;
    procedure _FontChanged(sender: TObject);
    procedure AfterMakeUptoDate(Follower: TBoldFollower);
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    function GetBoldHandle: TBoldElementHandle;
    function GetColor: TColor;
    function GetEffectiveColor: TColor;
    function GetEffectiveFont: TFont;
    function GetFollower: TBoldFollower;
    function GetFont: TFont;
    function GetText: TCaption;
    procedure SetBoldDisplay(Value: TBoldStringFollowerController);
    procedure SetBoldHandle(value: TBoldElementHandle);
    procedure SetColor(value: TColor);
    procedure SetEffectiveColor(v: TColor);
    procedure SetFont(value: TFont);
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;

    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldStringFollowerController read fBoldProperties write SetBoldDisplay;
    property Caption read GetText;
    property Color: TColor read GetColor write SetColor;
    property EffectiveColor: TColor read GetEffectiveColor write SetEffectiveColor;
    property EffectiveFont: TFont read GetEffectiveFont;
    property Font: TFont read GetFont write SetFont;
    property Follower: TBoldFollower read GetFollower;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  end;

  { TBoldLabel }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldLabel = class(TBoldCustomLabel)
  public
    {$IFNDEF T2H}
    property Caption;
  published
    { Published declarations }
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BoldHandle;
    property BoldProperties;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property Layout;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
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
  BoldControlPackDefs;

type
  TExposedControl = class(TControl);

{ TBoldCustomLabel }
constructor TBoldCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBoldProperties := TBoldStringFollowerController.Create(Self);
  fBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollower.Create(Owner, fBoldProperties);
  fMyFont := TFont.Create;
  fMyFont.OnChange := _FontChanged;
  fMyColor := EffectiveColor;
  if (csDesigning in ComponentState) then
    ParentColor := True;
end;

destructor TBoldCustomLabel.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(fBoldProperties);
  FreeAndNil(fMyFont);
  inherited Destroy;
end;

procedure TBoldCustomLabel.Loaded;
begin
  inherited;
  effectiveColor := Color;
end;

procedure TBoldCustomLabel.SetFont(value: TFont);
begin
  if not (csLoading in ComponentState) then
    if fMyFont <> value then
      fMyFont.Assign(value);
end;

procedure TBoldCustomLabel._FontChanged(sender: TObject);
begin
  Follower.MarkValueOutOfDate;
end;

function TBoldCustomLabel.GetFont: TFont;
begin
  Result := fMyFont;
end;

procedure TBoldCustomLabel.CMParentColorChanged(var Message: TMessage);
begin
  inherited;
  if ParentColor then
  begin
    if Message.wParam <> 0 then
      Color := TColor(Message.lParam)
    else
      if Assigned(Parent) and (Parent is TControl) then
        Color := TExposedControl(Parent).Color;
    ParentColor := True;
  end;
end;

function TBoldCustomLabel.GetEffectiveFont: TFont;
begin
  Result := inherited Font;
end;

procedure TBoldCustomLabel.SetColor(value: TColor);
begin
  if fMyColor <> Value then
    fMyColor := Value;
  if not (csLoading in ComponentState) then
    Follower.MarkValueOutOfDate;
end;

function TBoldCustomLabel.GetColor: TColor;
begin
  Result := fMyColor;
end;

function TBoldCustomLabel.GetEffectiveColor: TColor;
begin
  Result := inherited Color;
end;

procedure TBoldCustomLabel.SetEffectiveColor(v: TColor);
begin
  if EffectiveColor <> v then
  begin
    inherited Color := v;
    if (csDesigning in ComponentState) then
      fMyColor := v;
  end;
end;

procedure TBoldCustomLabel.SetBoldHandle(value: TBoldElementHandle);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldCustomLabel.SetBoldDisplay(value: TBoldStringFollowerController);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomLabel.AfterMakeUptoDate(Follower: TBoldFollower);
var
  newText: string;
  ec: TColor;
begin
  if (csDesigning in ComponentState) then
  begin
    with BoldProperties do
      if Assigned(Renderer) then
        NewText := Format('%s.%s', [Renderer.name, Expression])
      else if Expression <> '' then
        NewText := Expression
      else
        NewText := name;
  end
  else
    newText := BoldProperties.GetCurrentAsString(Follower);

  if Text <> newText then
    Text := newText;

  BoldProperties.SetFont(EffectiveFont, Font, Follower);
  ec := EffectiveColor;
  BoldProperties.SetColor(ec, Color, Follower);
  EffectiveColor := ec;
end;  

function TBoldCustomLabel.GetText: TCaption;
begin
  Result := inherited Text;
end;

function TBoldCustomLabel.GetBoldHandle: TBoldElementHandle;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomLabel.GetFollower: TBoldFollower;
begin
  Result := fHandleFollower.Follower;
end;

function TBoldCustomLabel.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomLabel.GetExpression: TBoldExpression;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldCustomLabel.SetExpression(const Value: TBoldExpression);
begin
  BoldProperties.Expression := Value;
end;

function TBoldCustomLabel.GetVariableList: TBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

procedure TBoldCustomLabel.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomLabel.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomLabel.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, Follower.element, 0);
end;

procedure TBoldCustomLabel.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
end;

end.
