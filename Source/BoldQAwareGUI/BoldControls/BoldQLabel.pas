unit BoldQLabel;

{$UNDEF BOLDCOMCLIENT}

interface

uses
//QGUI  Messages,
  Classes,
  QGraphics,
  QControls,
  QStdCtrls,
  QTypes,
  BoldDefs,
  BoldHandles,
  BoldQControlPack,
  BoldQStringControlPack,
  BoldQElementHandleFollower,
  BoldElements;

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
    procedure SetExpression(Expression: String);
    function GetExpression: String;
    function GetVariableList: TBoldExternalVariableList;
    procedure _FontChanged(sender: TObject);
    procedure AfterMakeUptoDate(Follower: TBoldFollower);
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
  end;

  { TBoldLabel }
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
//    property DragCursor;
//    property DragKind;
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
//    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
//    property OnStartDock;
    property OnStartDrag;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldRev,
  BoldUtils;

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
    ParentColor := True; //CHECKME This should not be necesary...
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
    //  caption during design-time
    with BoldProperties do
      if Assigned(Renderer) then
        NewText := Format('%s.%s', [Renderer.name, Expression])
      else if Expression <> '' then
        NewText := Expression
      else
        NewText := name;
  end
  else
    //  Caption at run-time
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

function TBoldCustomLabel.GetExpression: String;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldCustomLabel.SetExpression(Expression: String);
begin
  BoldProperties.Expression := Expression;
end;

function TBoldCustomLabel.GetVariableList: TBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

initialization
end.

