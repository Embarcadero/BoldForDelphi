
{ Global compiler directives }
{$include bold.inc}
unit BoldLabelCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  // VCL
  Classes,
  Controls,
  Graphics,
  Messages,
  StdCtrls,

  // Bold
  BoldClientElementSupport,
  BoldComObjectSpace_TLB,
  BoldControlPackCom,
  BoldElementHandleFollowerCom,
  BoldHandlesCom,
  BoldStringControlPackCom;

type
  {Forward declaration of classes}
  TBoldCustomLabelCom = class;
  TBoldLabelCom = class;

  { TBoldCustomLabelCom }
  TBoldCustomLabelCom = class(TCustomLabel, IBoldOCLComponentCom)
  private
    { Private declarations }
    fBoldProperties: TBoldStringFollowerControllerCom;
    fHandleFollower: TBoldElementHandleFollowerCom;
    fMyColor: TColor;
    fMyFont: TFont;
    function GetContextType: IBoldElementTypeInfo;
    procedure SetExpression(Expression: String);
    function GetExpression: String;
    function GetVariableList: IBoldExternalVariableList;
    procedure _FontChanged(sender: TObject);
    procedure AfterMakeUptoDate(Follower: TBoldFollowerCom);
    procedure CMParentColorChanged(var Message: TMessage); message CM_PARENTCOLORCHANGED;
    function GetBoldHandle: TBoldElementHandleCom;
    function GetColor: TColor;
    function GetEffectiveColor: TColor;
    function GetEffectiveFont: TFont;
    function GetFollower: TBoldFollowerCom;
    function GetFont: TFont;
    function GetText: TCaption;
    procedure SetBoldDisplay(Value: TBoldStringFollowerControllerCom);
    procedure SetBoldHandle(value: TBoldElementHandleCom);
    procedure SetColor(value: TColor);
    procedure SetEffectiveColor(v: TColor);
    procedure SetFont(value: TFont);
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;

    property BoldHandle: TBoldElementHandleCom read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldStringFollowerControllerCom read fBoldProperties write SetBoldDisplay;
    property Caption read GetText;
    property Color: TColor read GetColor write SetColor;
    property EffectiveColor: TColor read GetEffectiveColor write SetEffectiveColor;
    property EffectiveFont: TFont read GetEffectiveFont;
    property Font: TFont read GetFont write SetFont;
    property Follower: TBoldFollowerCom read GetFollower;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  end;

  { TBoldLabelCom }
  TBoldLabelCom = class(TBoldCustomLabelCom)
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

{ TBoldCustomLabelCom }
constructor TBoldCustomLabelCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBoldProperties := TBoldStringFollowerControllerCom.Create(Self);
  fBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollowerCom.Create(Owner, fBoldProperties);
  fMyFont := TFont.Create;
  fMyFont.OnChange := _FontChanged;
  fMyColor := EffectiveColor;
  if (csDesigning in ComponentState) then
    ParentColor := True;
end;

destructor TBoldCustomLabelCom.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(fBoldProperties);
  FreeAndNil(fMyFont);
  inherited Destroy;
end;

procedure TBoldCustomLabelCom.Loaded;
begin
  inherited;
  effectiveColor := Color;
end;

procedure TBoldCustomLabelCom.SetFont(value: TFont);
begin
  if not (csLoading in ComponentState) then
    if fMyFont <> value then
      fMyFont.Assign(value);
end;

procedure TBoldCustomLabelCom._FontChanged(sender: TObject);
begin
  Follower.MarkValueOutOfDate;
end;

function TBoldCustomLabelCom.GetFont: TFont;
begin
  Result := fMyFont;
end;

procedure TBoldCustomLabelCom.CMParentColorChanged(var Message: TMessage);
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

function TBoldCustomLabelCom.GetEffectiveFont: TFont;
begin
  Result := inherited Font;
end;

procedure TBoldCustomLabelCom.SetColor(value: TColor);
begin
  if fMyColor <> Value then
    fMyColor := Value;
  if not (csLoading in ComponentState) then
    Follower.MarkValueOutOfDate;
end;

function TBoldCustomLabelCom.GetColor: TColor;
begin
  Result := fMyColor;
end;

function TBoldCustomLabelCom.GetEffectiveColor: TColor;
begin
  Result := inherited Color;
end;

procedure TBoldCustomLabelCom.SetEffectiveColor(v: TColor);
begin
  if EffectiveColor <> v then
  begin
    inherited Color := v;
    if (csDesigning in ComponentState) then
      fMyColor := v;
  end;
end;

procedure TBoldCustomLabelCom.SetBoldHandle(value: TBoldElementHandleCom);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldCustomLabelCom.SetBoldDisplay(value: TBoldStringFollowerControllerCom);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomLabelCom.AfterMakeUptoDate(Follower: TBoldFollowerCom);
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

function TBoldCustomLabelCom.GetText: TCaption;
begin
  Result := inherited Text;
end;

function TBoldCustomLabelCom.GetBoldHandle: TBoldElementHandleCom;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomLabelCom.GetFollower: TBoldFollowerCom;
begin
  Result := fHandleFollower.Follower;
end;

function TBoldCustomLabelCom.GetContextType: IBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomLabelCom.GetExpression: String;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldCustomLabelCom.SetExpression(Expression: String);
begin
  BoldProperties.Expression := Expression;
end;

function TBoldCustomLabelCom.GetVariableList: IBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

procedure TBoldCustomLabelCom.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomLabelCom.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomLabelCom.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, Follower.element, 0);
end;

procedure TBoldCustomLabelCom.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
end;

end.
