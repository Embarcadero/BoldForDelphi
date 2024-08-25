{ Global compiler directives }
{$include bold.inc}
unit BoldCaptionController;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  Controls,
  BoldEnvironmentVCL,
  BoldHandles,
  BoldElementHandleFollower,
  BoldElements,
  BoldControlPack,
  BoldStringControlPack,
  BoldDefs;

type
  TBoldCustomCaptionController = class;
  TBoldCaptionController = class;

  {---TBoldCaptionController---}
  [ComponentPlatforms(pidWin32 or pidWin64)]
  TBoldCustomCaptionController = class(TComponent, IBoldOCLComponent)
  private
    { Private declarations }
    fBoldProperties: TBoldStringFollowerController;
    fCaption: TCaption;
    fHandleFollower: TBoldElementHandleFollower;
    fTrackControl: TWinControl;
    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetExpression: TBoldExpression;
    function GetVariableList: TBoldExternalVariableList;

    function GetBoldHandle: TBoldElementHandle;
    procedure SetBoldHandle(value: TBoldElementHandle);
    procedure AfterMakeUptoDate(Follower: TBoldFollower);
    function GetTrackedCaption: TCaption;
    procedure SetBoldDisplay(Value: TBoldStringFollowerController);
    procedure SetCaption(S: TCaption);
    procedure SetTrackControl(Control: TWinControl);
    procedure SetTrackedCaption(S: TCaption);
    property TrackedCaption: TCaption read GetTrackedCaption write SetTrackedCaption;
  protected
    { Protected declarations }
    property Caption: TCaption read fCaption write SetCaption;
    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldStringFollowerController read fBoldProperties write SetBoldDisplay;
    property TrackControl: TWinControl read fTrackControl write SetTrackControl;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {---TBoldCaptionController---}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldCaptionController = class(TBoldCustomCaptionController)
  published
    property BoldHandle;
    property BoldProperties;
    property TrackControl;
  end;

implementation

uses
  SysUtils;

type
  {---TWinControlWithCaption---}
  {Dummy class to access protected caption property of TWinControl}
  TWinControlWithCaption = class(TwinControl);

{---TBoldCustomCaptionController---}
constructor TBoldCustomCaptionController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBoldProperties := TBoldStringFollowerController.Create(Self);
  fBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollower.Create(Owner, fBoldProperties);
  TrackControl := nil;
end;

destructor TBoldCustomCaptionController.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(fBoldProperties);
  fTrackControl := nil;
  inherited Destroy;
end;

function TBoldCustomCaptionController.GetTrackedCaption: TCaption;
begin
  if Assigned(fTrackControl) then
    Result := TWinControlWithCaption(fTrackControl).Caption;
end;

procedure TBoldCustomCaptionController.SetTrackedCaption(s: TCaption);
begin
  if Assigned(fTrackControl) then
    TWinControlWithCaption(fTrackControl).Caption := s;
end;

procedure TBoldCustomCaptionController.SetCaption(s: TCaption);
begin
  if (s <> fCaption) or
    (s <> TrackedCaption) then
  begin
    fCaption := s;
    TrackedCaption := Caption;
  end;
end;

procedure TBoldCustomCaptionController.SetTrackControl(Control: TWinControl);
begin
  if Control <> fTrackControl then
  begin
    fTrackControl := Control;
    Caption := Caption;
  end;
end;

procedure TBoldCustomCaptionController.SetBoldHandle(value: TBoldElementHandle);
begin
  fHandleFollower.BoldHandle := value;
end;

procedure TBoldCustomCaptionController.SetBoldDisplay(value: TBoldStringFollowerController);
begin
  fBoldProperties.Assign(Value);
end;

procedure TBoldCustomCaptionController.AfterMakeUptoDate(Follower: TBoldFollower);
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

function TBoldCustomCaptionController.GetBoldHandle: TBoldElementHandle;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomCaptionController.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomCaptionController.GetExpression: TBoldExpression;
begin
  result := BoldProperties.Expression;
end;

procedure TBoldCustomCaptionController.SetExpression(const Value: TBoldExpression);
begin
  BoldProperties.Expression := Value;
end;

function TBoldCustomCaptionController.GetVariableList: TBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

end.
