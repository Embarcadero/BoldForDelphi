
{ Global compiler directives }
{$include bold.inc}
unit BoldTrackBarCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 14:59:57}

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
  BoldHandlesCom,
  BoldComObjectSpace_TLB, BoldClientElementSupport, BoldComClient,
  BoldControlPackCom,
  BoldNumericControlPackCom,
  BoldElementHandleFollowerCom;

type
  { forward declarations }
  TBoldTrackBarCom = class;

  { TBoldTrackBarCom }
  TBoldTrackBarCom = class(TTrackBar, IBoldOCLComponentCom)
  private
    FBoldProperties: TBoldIntegerFollowerControllerCom;
    FEffectiveReadOnly: Boolean;
    fHandleFollower: TBoldElementHandleFollowerCom;
    FReadOnly: Boolean;
    function GetContextType: IBoldElementTypeInfo;
    procedure SetExpression(Expression: String);
    function GetExpression: String;
    function GetVariableList: IBoldExternalVariableList;
    procedure AfterMakeUptoDate(Follower: TBoldFollowerCom);
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
    function GetBoldHandle: TBoldElementHandleCom;
    function GetFollower: TBoldFollowerCom;
    function GetPosition: Integer;
    procedure SetBoldDisplay(Value: TBoldIntegerFollowerControllerCom);
    procedure SetBoldHandle(Value: TBoldElementHandleCom);
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
    property Follower: TBoldFollowerCom read GetFollower;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X: Integer; Y: Integer); override;
  published
    property BoldHandle: TBoldElementHandleCom read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldIntegerFollowerControllerCom read FBoldProperties write SetBoldDisplay;
    property Position: Integer read GetPosition write SetPosition stored False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldGuiResourceStringsCom,
  BoldControlsDefs;

{ TBoldTrackBarCom }
constructor TBoldTrackBarCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoldProperties := TBoldIntegerFollowerControllerCom.Create(Self);
  FBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollowerCom.Create(Owner, FBoldProperties);
end;

destructor TBoldTrackBarCom.Destroy;
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

procedure TBoldTrackBarCom.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldTrackBarCom.DoEndDrag(Target: TObject; X: Integer; Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldTrackBarCom.DragOver(Source: TObject; X: Integer; Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, follower.Element, 0);
end;

procedure TBoldTrackBarCom.DragDrop(Source: TObject; X: Integer; Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
end;

procedure TBoldTrackBarCom.SetBoldHandle(Value: TBoldElementHandleCom);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldTrackBarCom.SetBoldDisplay(Value: TBoldIntegerFollowerControllerCom);
begin
  FBoldProperties.Assign(Value);
end;

procedure TBoldTrackBarCom.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Follower.MarkValueOutOfDate;
  end;
end;

function TBoldTrackBarCom.GetPosition: Integer;
begin
  Result := inherited Position;
end;

procedure TBoldTrackBarCom.SetPosition(Value: Integer);
begin
  if not EffectiveReadOnly then
  begin
    BoldProperties.MayHaveChanged(Value, Follower);
    inherited Position := Value;
  end
  else
    raise EBold.Create(sValueReadOnly);
end;

function TBoldTrackBarCom.GetPopupMenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (not Assigned(Result)) and Assigned(BoldHandle) then
    Result := BoldProperties.Popup.GetMenu(Self, BoldHandle.Value);
end;

procedure TBoldTrackBarCom.AfterMakeUptoDate(Follower: TBoldFollowerCom);
var
  NewPosition: Integer;
begin
  NewPosition := BoldProperties.GetCurrentAsInteger(Follower);
  if inherited Position <> NewPosition then
    inherited Position := NewPosition;
  FEffectiveReadOnly := FReadOnly or not BoldProperties.MayModify(Follower);
end;

procedure TBoldTrackBarCom.CMExit(var Message: TCMExit);
begin
  if not EffectiveReadOnly and (Follower.Controller.ApplyPolicy = bapExit) then
    Follower.Apply;
  inherited;
end;

procedure TBoldTrackBarCom.CNHScroll(var Message: TWMHScroll);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    BoldProperties.MayHaveChanged(inherited Position, Follower);
end;

procedure TBoldTrackBarCom.CNVScroll(var Message: TWMVScroll);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    BoldProperties.MayHaveChanged(inherited Position, Follower);
end;

procedure TBoldTrackBarCom.WMKeyDown(var Message: TWMKeyDown);
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

procedure TBoldTrackBarCom.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if EffectiveReadOnly then
    Message.Result := 0
  else
    inherited;
end;

function TBoldTrackBarCom.GetBoldHandle: TBoldElementHandleCom;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldTrackBarCom.GetFollower: TBoldFollowerCom;
begin
  Result := fHandleFollower.Follower;
end;

function TBoldTrackBarCom.GetContextType: IBoldElementTypeInfo;
begin
  if assigned(BoldHAndle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldTrackBarCom.GetExpression: String;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldTrackBarCom.SetExpression(Expression: String);
begin
  BoldProperties.Expression := Expression;
end;

function TBoldTrackBarCom.GetVariableList: IBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

end.
