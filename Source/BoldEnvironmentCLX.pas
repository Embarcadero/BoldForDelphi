
{ Global compiler directives }
{$include bold.inc}
unit BoldEnvironmentCLX;

interface

implementation

uses
  SysUtils,
  Classes,
  QControls,
  BoldDefs,
  QDialogs,
  QForms,
  QExtCtrls,
  BoldEnvironment,
  BoldQueue;

type
  TBoldCLXEnvironmentConfiguration = class(TBoldEnvironmentConfiguration)
  protected
    function GetName: string; override;
    function GetRootComponent: TComponent; override;
    function GetQueueClass: TBoldQueueClass; override;    
  public
    procedure HandleDesigntimeException(Sender: TObject); override;
    procedure UpdateDesigner(Sender: TObject);override;
    function IsFormOrDataModule(Sender: TObject): Boolean;override;
    function AskUser(const Text: string): Boolean; override;
    procedure ProcessMessages; override;
    procedure BringToFront; override;
    procedure FocusMainForm; override;
  end;


  TBoldTimerQueue = class(TBoldQueue)
  private
    fBoldQueueActive: Boolean;
    function GetIdleTimer: TTimer;
  private
    fIdleTimer: TTimer;
    procedure IdleTimerEvent(Sender: TObject);
    property IdleTimer: TTimer read GetIdleTimer write fIdleTimer;
  protected
    procedure EnsureDequeing; override;
    function GetIsActive: boolean; override;
  public
    destructor Destroy; override;
    procedure DeActivateDisplayQueue; override;
    procedure ActivateDisplayQueue; override;
  end;

function TBoldCLXEnvironmentConfiguration.AskUser(const Text: string): Boolean;
begin
    Result := MessageDlg(Text, mtWarning, [mbYes, mbNo], 0) = mrYes;
end;

procedure TBoldCLXEnvironmentConfiguration.BringToFront;
begin
  Application.BringToFront
end;

procedure TBoldCLXEnvironmentConfiguration.FocusMainForm;
begin
  if Assigned(Application) and Assigned(Application.MainForm) then
    Application.MainForm.SetFocus;
end;

function TBoldCLXEnvironmentConfiguration.GetName: string;
begin
  Result := 'CLX';
end;

function TBoldCLXEnvironmentConfiguration.GetQueueClass: TBoldQueueClass;
begin
  Result := TBoldTimerQueue;
end;

function TBoldCLXEnvironmentConfiguration.GetRootComponent: TComponent;
begin
  Result := Application;
end;

procedure TBoldCLXEnvironmentConfiguration.HandleDesigntimeException(
  Sender: TObject);
begin
  Application.HandleException(Sender);
end;

function TBoldCLXEnvironmentConfiguration.IsFormOrDataModule(
  Sender: TObject): Boolean;
begin
  Result := (Sender is TForm) or (Sender is TDataModule);
end;

procedure TBoldCLXEnvironmentConfiguration.ProcessMessages;
begin
  Application.ProcessMessages;
end;

procedure TBoldCLXEnvironmentConfiguration.UpdateDesigner(Sender: TObject);
var
  ParentForm: TCustomForm;
  Owner: TComponent;
begin
  if not RunningInIDE then
    raise EBold.CreateFmt(sNotRunningInIDE, ['UpdateDesigner']); // do not localize
  // Don't call inherited
  ParentForm := nil;
  Owner := nil;
  if (Sender is TControl) then
    ParentForm := GetParentForm(TControl(Sender))
  else
  begin
    if Sender is TComponent then
      Owner := TComponent(Sender).Owner;
    if Assigned(Owner) then
    begin
      if (Owner is TCustomFrame) then
        ParentForm := GetParentForm(TCustomFrame(Owner))
      else if (Owner is TCustomForm) then
        ParentForm := TCustomForm(Owner)
      else if (Owner is TDataModule) and Assigned(Owner.Owner) and (Owner.Owner is TCustomForm) then
        ParentForm := TCustomForm(Owner.Owner);
    end;
  end;
  if Assigned(ParentForm) and Assigned(ParentForm.DesignerHook) then
    ParentForm.DesignerHook.Modified;
end;

{ TBoldTimerQueue }

procedure TBoldTimerQueue.EnsureDequeing;
begin
  if fBoldQueueActive then
    IdleTimer.Interval := 1000;
end;

destructor TBoldTimerQueue.Destroy;
begin
  FreeAndNil(fIdleTimer);
  inherited;
end;

function TBoldTimerQueue.GetIdleTimer: TTimer;
begin
  if not Assigned(fIdleTimer) then
  begin
    fIdleTimer := TTimer.Create(nil);
    fIdleTimer.Interval := MaxInt; 
    fIdleTimer.OnTimer := IdleTimerEvent;
  end;
  Result := fIdleTimer;
end;

function TBoldTimerQueue.GetIsActive: boolean;
begin
  result := fBoldQueueActive;
end;

procedure TBoldTimerQueue.IdleTimerEvent(Sender: TObject);
var
  Done: Boolean;
begin
  PerformPreDisplayQueue;
  try
    if fBoldQueueActive then
    begin
      case DisplayMode of
        dmDisplayOne: Done := not DisplayOne;
        dmDisplayAll: Done := not DisplayAll;
        else
          raise EBoldInternal.CreateFmt(sUnknownDisplayMode, [classname]);
      end;
    end;
  except
    Application.HandleException(nil);
  end;
  if Done then
    IdleTimer.Interval := MaxInt
  else
    IdleTimer.Interval := 0;
end;

procedure TBoldTimerQueue.ActivateDisplayQueue;
begin
  fBoldQueueActive := true;
  IdleTimer.Interval := 0;
end;

procedure TBoldTimerQueue.DeActivateDisplayQueue;
begin
  fBoldQueueActive := false;
  IdleTimer.Interval := MaxInt;
end;

initialization
  BoldInternalCLXConfiguration := TBoldCLXEnvironmentConfiguration.Create;
end.
