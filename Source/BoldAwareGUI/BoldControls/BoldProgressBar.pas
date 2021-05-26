
{ Global compiler directives }
{$include bold.inc}
unit BoldProgressBar;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  Controls,
  ComCtrls,
  Menus,
  BoldEnvironmentVCL,
  BoldElements,
  BoldHandles,
  BoldControlPack,
  BoldNumericControlPack,
  BoldElementHandleFollower,
  BoldDefs;

type
  { forward declarations }
  TBoldProgressBar = class;

  { TBoldProgressBar }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldProgressBar = class(TProgressBar, IBoldOCLComponent)
  private
    FEffectiveReadOnly: Boolean;
    FReadOnly: Boolean;
    FBoldProperties: TBoldIntegerFollowerController;
    fHandleFollower: TBoldElementHandleFollower;
    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetExpression: TBoldExpression;
    function GetVariableList: TBoldExternalVariableList;
    procedure AfterMakeUptoDate(Follower: TBoldFollower);
    function GetBoldHandle: TBoldElementHandle;
    function GetFollower: TBoldFollower;
    procedure SetBoldDisplay(Value: TBoldIntegerFollowerController);
    procedure SetBoldHandle(Value: TBoldElementHandle);
    function GetPosition: integer;
    procedure SetPosition(const Value: integer);
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure DoEndDrag(Target: TObject; X: Integer; Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean); override;
    function GetPopupMenu: TPopupMenu; override;
    property Follower: TBoldFollower read GetFollower;
    property EffectiveReadOnly: Boolean read FEffectiveReadOnly;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X: Integer; Y: Integer); override;
  published
    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldIntegerFollowerController read FBoldProperties write SetBoldDisplay;
    property Position: integer read GetPosition write SetPosition stored False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

implementation

uses
  BoldControlPackDefs,
  SysUtils,
  BoldGuiResourceStrings,
  BoldControlsDefs;

{ TBoldProgressBar }
constructor TBoldProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoldProperties := TBoldIntegerFollowerController.Create(Self);
  FBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollower.Create(Owner, FBoldProperties);
end;

destructor TBoldProgressBar.Destroy;
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

procedure TBoldProgressBar.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldProgressBar.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldProgressBar.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, follower.Element, 0);
end;

procedure TBoldProgressBar.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
end;

procedure TBoldProgressBar.SetBoldHandle(Value: TBoldElementHandle);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldProgressBar.SetBoldDisplay(Value: TBoldIntegerFollowerController);
begin
  FBoldProperties.Assign(Value);
end;

function TBoldProgressBar.GetPopupmenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (not Assigned(Result)) and assigned(BoldHandle) then
    Result := BoldProperties.Popup.GetMenu(Self, BoldHandle.Value);
end;

procedure TBoldProgressBar.AfterMakeUptoDate(Follower: TBoldFollower);
var
  NewPosition: Integer;
begin
  NewPosition := BoldProperties.GetCurrentAsInteger(Follower);
  if Position <> NewPosition then
    inherited Position := NewPosition;
  FEffectiveReadOnly := FReadOnly or not BoldProperties.MayModify(Follower);
end;

function TBoldProgressBar.GetBoldHandle: TBoldElementHandle;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldProgressBar.GetFollower: TBoldFollower;
begin
  Result := fHandleFollower.Follower;
end;

function TBoldProgressBar.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldProgressBar.GetExpression: TBoldExpression;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldProgressBar.SetExpression(const Value: TBoldExpression);
begin
  BoldProperties.Expression := Value;
end;

function TBoldProgressBar.GetPosition: integer;
begin
  result := inherited Position;
end;

procedure TBoldProgressBar.SetPosition(const Value: integer);
begin
  if not EffectiveReadOnly then
  begin
    BoldProperties.MayHaveChanged(Value, Follower);
    inherited Position := Value;
  end
  else
    raise EBold.Create(sValueReadOnly);
end;

procedure TBoldProgressBar.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Follower.MarkValueOutOfDate;
  end;
end;

function TBoldProgressBar.GetVariableList: TBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

end.
