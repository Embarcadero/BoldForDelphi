
{ Global compiler directives }
{$include bold.inc}
unit BoldTrackBar;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Windows,
  Messages,
  Classes,
  Controls,
  ComCtrls,
  CommCtrl,
  Menus,
  BoldDefs,
  BoldEnvironmentVCL,
  BoldControlPackDefs,
  BoldHandles,
  BoldElements,
  BoldControlPack,
  BoldNumericControlPack,
  BoldElementHandleFollower;

type
  { forward declarations }
  TBoldTrackBar = class;

  { TBoldTrackBar }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldTrackBar = class(TTrackBar, IBoldOCLComponent)
  private
    FBoldProperties: TBoldIntegerFollowerController;
    FEffectiveReadOnly: Boolean;
    fHandleFollower: TBoldElementHandleFollower;
    FReadOnly: Boolean;
    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetExpression: TBoldExpression;
    function GetVariableList: TBoldExternalVariableList;
    procedure AfterMakeUptoDate(Follower: TBoldFollower);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
    function GetBoldHandle: TBoldElementHandle;
    function GetFollower: TBoldFollower;
    function GetPosition: Integer;
    procedure SetBoldDisplay(Value: TBoldIntegerFollowerController);
    procedure SetBoldHandle(Value: TBoldElementHandle);
    procedure SetPosition(Value: Integer);
    procedure SetReadOnly(Value: Boolean);
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
  protected
    procedure DoEndDrag(Target: TObject; X: Integer; Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean); override;
    function GetPopupMenu: TPopupMenu; override;
    property EffectiveReadOnly: Boolean read FEffectiveReadOnly;
    property Follower: TBoldFollower read GetFollower;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X: Integer; Y: Integer); override;
  published
    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldIntegerFollowerController read FBoldProperties write SetBoldDisplay;
    property Position: Integer read GetPosition write SetPosition stored False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldGuiResourceStrings,
  BoldControlsDefs;

{ TBoldTrackBar }
constructor TBoldTrackBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoldProperties := TBoldIntegerFollowerController.Create(Self);
  FBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollower.Create(Owner, FBoldProperties);
end;

destructor TBoldTrackBar.Destroy;
begin
  case BoldProperties.ApplyPolicy of
    bapChange,
    bapExit:   Follower.Apply;
    bapDemand: Follower.DiscardChange;
  end;

  FreeAndNil(fHandleFollower);
  FreeAndNil(FBoldProperties);
  inherited Destroy;
end;

procedure TBoldTrackBar.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldTrackBar.DoEndDrag(Target: TObject; X: Integer; Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldTrackBar.DragOver(Source: TObject; X: Integer; Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, follower.Element, 0);
end;

procedure TBoldTrackBar.DragDrop(Source: TObject; X: Integer; Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
end;

procedure TBoldTrackBar.SetBoldHandle(Value: TBoldElementHandle);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldTrackBar.SetBoldDisplay(Value: TBoldIntegerFollowerController);
begin
  FBoldProperties.Assign(Value);
end;

procedure TBoldTrackBar.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Follower.MarkValueOutOfDate;
  end;
end;

function TBoldTrackBar.GetPosition: Integer;
begin
  Result := inherited Position;
end;

procedure TBoldTrackBar.SetPosition(Value: Integer);
begin
  if not EffectiveReadOnly then
  begin
    BoldProperties.MayHaveChanged(Value, Follower);
    inherited Position := Value;
  end
  else
    raise EBold.Create(sValueReadOnly);
end;

function TBoldTrackBar.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (not Assigned(Result)) and Assigned(BoldHandle) then
    Result := BoldProperties.Popup.GetMenu(Self, BoldHandle.Value);
end;

procedure TBoldTrackBar.AfterMakeUptoDate(Follower: TBoldFollower);
var
  NewPosition: Integer;
begin
  NewPosition := BoldProperties.GetCurrentAsInteger(Follower);
  if inherited Position <> NewPosition then
    inherited Position := NewPosition;
  FEffectiveReadOnly := FReadOnly or not BoldProperties.MayModify(Follower);
end;

procedure TBoldTrackBar.CMExit(var Message: TCMExit);
begin
  if not EffectiveReadOnly and (Follower.Controller.ApplyPolicy = bapExit) then
    Follower.Apply;
  inherited;
end;

procedure TBoldTrackBar.CNHScroll(var Message: TWMHScroll);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    BoldProperties.MayHaveChanged(inherited Position, Follower);
end;

procedure TBoldTrackBar.CNVScroll(var Message: TWMVScroll);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    BoldProperties.MayHaveChanged(inherited Position, Follower);
end;

procedure TBoldTrackBar.WMKeyDown(var Message: TWMKeyDown);
begin
  with Message do
  begin
    if EffectiveReadOnly and (CharCode in [VK_END, VK_RIGHT, VK_DOWN,
                                           VK_LEFT, VK_UP, VK_NEXT,
                                           VK_PRIOR, VK_HOME]) then
      Message.Result := 0
    else
      inherited;
  end;
end;

procedure TBoldTrackBar.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if EffectiveReadOnly then
    Message.Result := 0
  else
    inherited;
end;

function TBoldTrackBar.GetBoldHandle: TBoldElementHandle;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldTrackBar.GetFollower: TBoldFollower;
begin
  Result := fHandleFollower.Follower;
end;

function TBoldTrackBar.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHAndle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldTrackBar.GetExpression: TBoldExpression;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldTrackBar.SetExpression(const Value: TBoldExpression);
begin
  BoldProperties.Expression := Value;
end;

function TBoldTrackBar.GetVariableList: TBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

end.
