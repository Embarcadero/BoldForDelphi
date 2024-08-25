
{ Global compiler directives }
{$include bold.inc}
unit BoldCaptionControllerCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  Classes,
  Controls,
  BoldEnvironmentVCL,
  BoldHandlesCom,
  BoldElementHandleFollowerCom,
  BoldComObjectSpace_TLB, BoldClientElementSupport, BoldComClient,
  BoldControlPackCom,
  BoldStringControlPackCom;

type
  TBoldCustomCaptionControllerCom = class;
  TBoldCaptionControllerCom = class;

  {---TBoldCaptionControllerCom---}
  TBoldCustomCaptionControllerCom = class(TComponent, IBoldOCLComponentCom)
  private
    { Private declarations }
    fBoldProperties: TBoldStringFollowerControllerCom;
    fCaption: TCaption;
    fHandleFollower: TBoldElementHandleFollowerCom;
    fTrackControl: TWinControl;
    function GetContextType: IBoldElementTypeInfo;
    procedure SetExpression(Expression: String);
    function GetExpression: String;
    function GetVariableList: IBoldExternalVariableList;

    function GetBoldHandle: TBoldElementHandleCom;
    procedure SetBoldHandle(value: TBoldElementHandleCom);
    procedure AfterMakeUptoDate(Follower: TBoldFollowerCom);
    function GetTrackedCaption: TCaption;
    procedure SetBoldDisplay(Value: TBoldStringFollowerControllerCom);
    procedure SetCaption(S: TCaption);
    procedure SetTrackControl(Control: TWinControl);
    procedure SetTrackedCaption(S: TCaption);
    property TrackedCaption: TCaption read GetTrackedCaption write SetTrackedCaption;
  protected
    { Protected declarations }
    property Caption: TCaption read fCaption write SetCaption;
    property BoldHandle: TBoldElementHandleCom read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldStringFollowerControllerCom read fBoldProperties write SetBoldDisplay;
    property TrackControl: TWinControl read fTrackControl write SetTrackControl;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {---TBoldCaptionControllerCom---}
  TBoldCaptionControllerCom = class(TBoldCustomCaptionControllerCom)
  published
    {$IFNDEF T2H}
    property BoldHandle;
    property BoldProperties;
    property TrackControl;
    {$ENDIF}
  end;

implementation

uses
  SysUtils;

type
  {---TWinControlWithCaption---}
  {Dummy class to access protected caption property of TWinControl}
  TWinControlWithCaption = class(TwinControl);


{---TBoldCustomCaptionControllerCom---}
constructor TBoldCustomCaptionControllerCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBoldProperties := TBoldStringFollowerControllerCom.Create(Self);
  fBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollowerCom.Create(Owner, fBoldProperties);
  TrackControl := nil;
end;

destructor TBoldCustomCaptionControllerCom.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(fBoldProperties);
  fTrackControl := nil;
  inherited Destroy;
end;

function TBoldCustomCaptionControllerCom.GetTrackedCaption: TCaption;
begin
  if Assigned(fTrackControl) then
    Result := TWinControlWithCaption(fTrackControl).Caption;
end;

procedure TBoldCustomCaptionControllerCom.SetTrackedCaption(s: TCaption);
begin
  if Assigned(fTrackControl) then
    TWinControlWithCaption(fTrackControl).Caption := s;
end;

procedure TBoldCustomCaptionControllerCom.SetCaption(s: TCaption);
begin
  if (s <> fCaption) or
    (s <> TrackedCaption) then
  begin
    fCaption := s;
    TrackedCaption := Caption;
  end;
end;

procedure TBoldCustomCaptionControllerCom.SetTrackControl(Control: TWinControl);
begin
  if Control <> fTrackControl then
  begin
    fTrackControl := Control;
    Caption := Caption;
  end;
end;


procedure TBoldCustomCaptionControllerCom.SetBoldHandle(value: TBoldElementHandleCom);
begin
  fHandleFollower.BoldHandle := value;
end;

procedure TBoldCustomCaptionControllerCom.SetBoldDisplay(value: TBoldStringFollowerControllerCom);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomCaptionControllerCom.AfterMakeUptoDate(Follower: TBoldFollowerCom);
var
  newText: string;
begin
  if (csDesigning in ComponentState) then
  begin
    with BoldProperties do
      if Assigned(Renderer) then
        NewText := Format('%s.%s',[Renderer.name, Expression])
      else if Expression <> '' then
        NewText := Expression
      else
        NewText := name;
  end
  else
    newText := BoldProperties.GetCurrentAsString(Follower);
  Caption := newText;
end;

function TBoldCustomCaptionControllerCom.GetBoldHandle: TBoldElementHandleCom;
begin
  Result := fHandleFollower.BoldHandle;
end;


function TBoldCustomCaptionControllerCom.GetContextType: IBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomCaptionControllerCom.GetExpression: String;
begin
  result := BoldProperties.Expression;

end;

procedure TBoldCustomCaptionControllerCom.SetExpression(Expression: String);
begin
  BoldProperties.Expression := Expression;
end;

function TBoldCustomCaptionControllerCom.GetVariableList: IBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

end.
