
{ Global compiler directives }
{$include bold.inc}
unit BoldProgressBarCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 14:59:57}

interface

uses
  // VCL
  Classes,
  ComCtrls,
  Controls,
  Menus,

  // Bold
  BoldClientElementSupport,
  BoldComObjectSpace_TLB,
  BoldControlPackCom,
  BoldElementHandleFollowerCom,
  BoldHandlesCom,
  BoldNumericControlPackCom;

type
  { forward declarations }
  TBoldProgressBarCom = class;

  { TBoldProgressBarCom }
  TBoldProgressBarCom = class(TProgressBar, IBoldOCLComponentCom)
  private
    FEffectiveReadOnly: Boolean;
    FReadOnly: Boolean;
    FBoldProperties: TBoldIntegerFollowerControllerCom;
    fHandleFollower: TBoldElementHandleFollowerCom;
    function GetContextType: IBoldElementTypeInfo;
    procedure SetExpression(Expression: String);
    function GetExpression: String;
    function GetVariableList: IBoldExternalVariableList;
    procedure AfterMakeUptoDate(Follower: TBoldFollowerCom);
    function GetBoldHandle: TBoldElementHandleCom;
    function GetFollower: TBoldFollowerCom;
    procedure SetBoldDisplay(Value: TBoldIntegerFollowerControllerCom);
    procedure SetBoldHandle(Value: TBoldElementHandleCom);
    function GetPosition: integer;
    procedure SetPosition(const Value: integer);
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure DoEndDrag(Target: TObject; X: Integer; Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean); override;
    function GetPopupMenu: TPopupMenu; override;
    property Follower: TBoldFollowerCom read GetFollower;
    property EffectiveReadOnly: Boolean read FEffectiveReadOnly;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X: Integer; Y: Integer); override;
  published
    property BoldHandle: TBoldElementHandleCom read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldIntegerFollowerControllerCom read FBoldProperties write SetBoldDisplay;
    property Position: integer read GetPosition write SetPosition stored False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  end;

implementation

uses
  BoldControlPackDefs,
  BoldDefs,
  SysUtils,
  BoldGuiResourceStringsCom;

{ TBoldProgressBarCom }
constructor TBoldProgressBarCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoldProperties := TBoldIntegerFollowerControllerCom.Create(Self);
  FBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollowerCom.Create(Owner, FBoldProperties);
end;

destructor TBoldProgressBarCom.Destroy;
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

procedure TBoldProgressBarCom.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldProgressBarCom.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldProgressBarCom.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
    var Accept: Boolean);
begin
  if (BoldProperties.DropMode = bdpNone) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, follower.Element, 0);
end;

procedure TBoldProgressBarCom.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    inherited DragDrop(Source, X, Y)
  else
    BoldProperties.DragDrop(Follower, follower.Element, 0);
end;

procedure TBoldProgressBarCom.SetBoldHandle(Value: TBoldElementHandleCom);
begin
  fHandleFollower.BoldHandle := Value;
end;

procedure TBoldProgressBarCom.SetBoldDisplay(Value: TBoldIntegerFollowerControllerCom);
begin
  FBoldProperties.Assign(Value);
end;

function TBoldProgressBarCom.GetPopupmenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
  if (not Assigned(Result)) and assigned(BoldHandle) then
    Result := BoldProperties.Popup.GetMenu(Self, BoldHandle.Value);
end;

procedure TBoldProgressBarCom.AfterMakeUptoDate(Follower: TBoldFollowerCom);
var
  NewPosition: Integer;
begin
  NewPosition := BoldProperties.GetCurrentAsInteger(Follower);
  if Position <> NewPosition then
    inherited Position := NewPosition;
  FEffectiveReadOnly := FReadOnly or not BoldProperties.MayModify(Follower);
end;

function TBoldProgressBarCom.GetBoldHandle: TBoldElementHandleCom;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldProgressBarCom.GetFollower: TBoldFollowerCom;
begin
  Result := fHandleFollower.Follower;
end;

function TBoldProgressBarCom.GetContextType: IBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldProgressBarCom.GetExpression: String;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldProgressBarCom.SetExpression(Expression: String);
begin
  BoldProperties.Expression := Expression;
end;

function TBoldProgressBarCom.GetPosition: integer;
begin
  result := inherited Position;
end;

procedure TBoldProgressBarCom.SetPosition(const Value: integer);
begin
  if not EffectiveReadOnly then
  begin
    BoldProperties.MayHaveChanged(Value, Follower);
    inherited Position := Value;
  end
  else
    raise EBold.Create(sValueReadOnly);
end;

procedure TBoldProgressBarCom.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Follower.MarkValueOutOfDate;
  end;
end;

function TBoldProgressBarCom.GetVariableList: IBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

end.
