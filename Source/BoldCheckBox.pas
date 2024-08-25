
{ Global compiler directives }
{$include bold.inc}
unit BoldCheckBox;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Messages,
  Classes,
  Controls,
  StdCtrls,
  BoldEnvironmentVCL,
  BoldHandles,
  BoldElementHandleFollower,
  BoldElements,
  BoldControlPack,
  BoldCheckboxStateControlPack,
  BoldDefs;

type
  TBoldCustomCheckBox = class;
  TBoldCheckBox = class;

  {---TBoldCustomCheckBox---}
  TBoldCustomCheckBox = class(TCustomCheckBox, IBoldOCLComponent)
  private
    { Private declarations }
    fBoldProperties: TBoldCheckBoxStateFollowerController;
    fEffectiveReadOnly: Boolean;
    fHandleFollower: TBoldElementHandleFollower;
    fMyReadOnly: Boolean;
    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetExpression: TBoldExpression;
    function GetVariableList: TBoldExternalVariableList;

    function GetBoldHandle: TBoldElementHandle;
    procedure SetBoldHandle(value: TBoldElementHandle);
    function GetFollower: TBoldFOllower;
    procedure AfterMakeUptoDate(Follower: TBoldFollower);
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMChanged(var Message: TCMChanged); message CM_CHANGED;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetState: TCheckBoxState;
    procedure SetBoldDisplay(Value: TBoldCheckBoxStateFollowerController);
    procedure SetReadOnly(value: Boolean);
    procedure SetState(v: TCheckBoxState);
  protected
    property Follower: TBoldFollower read GetFollower;
   {Bold stuff}
    function GetChecked: boolean; override;
    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldCheckBoxStateFollowerController read fBoldProperties write SetBoldDisplay;
    property Checked: boolean read GetChecked;
    property EffectiveReadOnly: Boolean read fEffectiveReadOnly;
    property ReadOnly: Boolean read fMyReadOnly write SetReadOnly;
    property State: TCheckBoxState read GetState write SetState;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {---TBoldCheckBox---}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldCheckBox = class(TBoldCustomCheckBox)
  public
    {$IFNDEF T2H}
    property Checked;
    property EffectiveReadOnly;
    property State;
  published
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property BoldHandle;
    property BoldProperties;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnEndDock;
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

  BoldCoreConsts,
  BoldControlPackDefs;

{---TBoldCustomCheckBox---}
constructor TBoldCustomCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBoldProperties := TBoldCheckBoxStateFollowerController.Create(Self);
  fBoldProperties.OnGetContextType := GetContextType;
  fBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fHandleFollower := TBoldElementHandleFollower.Create(Owner, fBoldProperties);
end;

destructor TBoldCustomCheckBox.Destroy;
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

  FreeAndNil(fHandleFollower);;
  FreeAndNil(fBoldProperties);
  inherited Destroy;
end;

function TBoldCustomCheckBox.GetChecked: Boolean;
begin
  Result := State = cbChecked;
end;

procedure TBoldCustomCheckBox.SetReadOnly(value: Boolean);
begin
  if fMyReadOnly <> value then
  begin
    fMyReadOnly := value;
    Follower.MarkValueOutOfDate;
  end;
end;

function TBoldCustomCheckBox.GetState: TCheckBoxState;
begin
  Result := inherited State;
end;

procedure TBoldCustomCheckBox.SetState(v: TCheckBoxState);
begin
  if not (csLoading in ComponentState) then
    if (v <> State) then
    begin
      if (not EffectiveReadOnly) then
      begin
        inherited State := v;
        Perform(CM_CHANGED, 0, 0);
      end
      else
        raise EBold.CreateFmt(sStateNotModifiable, [ClassName]);
    end;
end;

procedure TBoldCustomCheckBox.SetBoldHandle(value: TBoldElementHandle);
begin
  fHandleFollower.BoldHandle := value;
end;

procedure TBoldCustomCheckBox.SetBoldDisplay(value: TBoldCheckBoxStateFollowerController);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomCheckBox.AfterMakeUptoDate(Follower: TBoldFollower);
begin
  inherited State := BoldProperties.GetCurrentAsCheckBoxState(Follower);
  fEffectiveReadOnly := FMyReadOnly or
                        not BoldProperties.MayModify(Follower);
end;

procedure TBoldCustomCheckBox.CMChanged(var Message: TCMChanged);
begin
  try
    if not (csDesigning in ComponentState) then
      BoldProperties.MayHaveChanged(state, Follower);
    inherited;
  except
    Follower.DiscardChange;
    raise;
  end;
end;

procedure TBoldCustomCheckBox.CMExit(var Message: TCMExit);
begin
  if (Follower.controller.ApplyPolicy = bapExit) then
    Follower.Apply;
  DoExit;
end;

procedure TBoldCustomCheckBox.CNCommand(var Message: TWMCommand);
begin
  if (not EffectiveReadOnly) and (Message.NotifyCode = BN_CLICKED) then
    Toggle;
end;

function TBoldCustomCheckBox.GetBoldHandle: TBoldElementHandle;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomCheckBox.GetFollower: TBoldFOllower;
begin
     Result := fHandleFollower.Follower;
end;

function TBoldCustomCheckBox.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomCheckBox.GetExpression: TBoldExpression;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldCustomCheckBox.SetExpression(const Value: TBoldExpression);
begin
  BoldProperties.Expression := Value;
end;

function TBoldCustomCheckBox.GetVariableList: TBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

end.
