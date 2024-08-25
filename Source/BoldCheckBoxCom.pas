
{ Global compiler directives }
{$include bold.inc}
unit BoldCheckBoxCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  // VCL
  Classes,
  Controls,
  Messages,
  StdCtrls,

  // Bold
  BoldCheckboxStateControlPackCom,
  BoldClientElementSupport,
  BoldComObjectSpace_TLB,
  BoldControlPackCom,
  BoldElementHandleFollowerCom,
  BoldHandlesCom;

type
  TBoldCustomCheckBoxCom = class;
  TBoldCheckBoxCom = class;

  {---TBoldCustomCheckBoxCom---}
  TBoldCustomCheckBoxCom = class(TCustomCheckBox, IBoldOCLComponentCom)
  private
    { Private declarations }
    fBoldProperties: TBoldCheckBoxStateFollowerControllerCom;
    fEffectiveReadOnly: Boolean;
    fHandleFollower: TBoldElementHandleFollowerCom;
    fMyReadOnly: Boolean;
    function GetContextType: IBoldElementTypeInfo;
    procedure SetExpression(Expression: String);
    function GetExpression: String;
    function GetVariableList: IBoldExternalVariableList;

    function GetBoldHandle: TBoldElementHandleCom;
    procedure SetBoldHandle(value: TBoldElementHandleCom);
    function GetFollower: TBoldFollowerCom;
    procedure AfterMakeUptoDate(Follower: TBoldFollowerCom);
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMChanged(var Message: TCMChanged); message CM_CHANGED;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetState: TCheckBoxState;
    procedure SetBoldDisplay(Value: TBoldCheckBoxStateFollowerControllerCom);
    procedure SetReadOnly(value: Boolean);
    procedure SetState(v: TCheckBoxState);
  protected
    property Follower: TBoldFollowerCom read GetFollower;
   {Bold stuff}
    function GetChecked: boolean; override;
    property BoldHandle: TBoldElementHandleCom read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldCheckBoxStateFollowerControllerCom read fBoldProperties write SetBoldDisplay;
    property Checked: boolean read GetChecked;
    property EffectiveReadOnly: Boolean read fEffectiveReadOnly;
    property ReadOnly: Boolean read fMyReadOnly write SetReadOnly;
    property State: TCheckBoxState read GetState write SetState;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {---TBoldCheckBoxCom---}
  TBoldCheckBoxCom = class(TBoldCustomCheckBoxCom)
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
  BoldDefs,
  BoldControlPackDefs;

{---TBoldCustomCheckBoxCom---}
constructor TBoldCustomCheckBoxCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBoldProperties := TBoldCheckBoxStateFollowerControllerCom.Create(Self);
  fBoldProperties.OnGetContextType := GetContextType;
  fBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fHandleFollower := TBoldElementHandleFollowerCom.Create(Owner, fBoldProperties);
end;

destructor TBoldCustomCheckBoxCom.Destroy;
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

function TBoldCustomCheckBoxCom.GetChecked: Boolean;
begin
  Result := State = cbChecked;
end;

procedure TBoldCustomCheckBoxCom.SetReadOnly(value: Boolean);
begin
  if fMyReadOnly <> value then
  begin
    fMyReadOnly := value;
    Follower.MarkValueOutOfDate;
  end;
end;

function TBoldCustomCheckBoxCom.GetState: TCheckBoxState;
begin
  Result := inherited State;
end;

procedure TBoldCustomCheckBoxCom.SetState(v: TCheckBoxState);
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
        raise EBold.CreateFmt('%s.State: Not modifiable', [ClassName]);
    end;
end;

procedure TBoldCustomCheckBoxCom.SetBoldHandle(value: TBoldElementHandleCom);
begin
  fHandleFollower.BoldHandle := value;
end;

procedure TBoldCustomCheckBoxCom.SetBoldDisplay(value: TBoldCheckBoxStateFollowerControllerCom);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomCheckBoxCom.AfterMakeUptoDate(Follower: TBoldFollowerCom);
begin
  inherited State := BoldProperties.GetCurrentAsCheckBoxState(Follower);
  fEffectiveReadOnly := FMyReadOnly or
                        not BoldProperties.MayModify(Follower);
end;

procedure TBoldCustomCheckBoxCom.CMChanged(var Message: TCMChanged);
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

procedure TBoldCustomCheckBoxCom.CMExit(var Message: TCMExit);
begin
  if (Follower.controller.ApplyPolicy = bapExit) then
    Follower.Apply;
  DoExit;
end;

procedure TBoldCustomCheckBoxCom.CNCommand(var Message: TWMCommand);
begin
  if (not EffectiveReadOnly) and (Message.NotifyCode = BN_CLICKED) then
    Toggle;
end;

function TBoldCustomCheckBoxCom.GetBoldHandle: TBoldElementHandleCom;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomCheckBoxCom.GetFollower: TBoldFollowerCom;
begin
     Result := fHandleFollower.Follower;
end;

function TBoldCustomCheckBoxCom.GetContextType: IBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomCheckBoxCom.GetExpression: String;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldCustomCheckBoxCom.SetExpression(Expression: String);
begin
  BoldProperties.Expression := Expression;
end;

function TBoldCustomCheckBoxCom.GetVariableList: IBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

end.
