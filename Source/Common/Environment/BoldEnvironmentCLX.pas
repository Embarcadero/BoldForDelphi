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
  BoldQueue,
  BoldCommonConst;

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

  // Use the fact the a timer with 0 time will be placed last in queue
  // This was communicated by Chuck J, does not seem to be documented, but is used in QForms.Application

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
  public
    destructor Destroy; override;
    procedure DeActivateDisplayQueue; override;
    procedure ActivateDisplayQueue; override;
  end;

function TBoldCLXEnvironmentConfiguration.AskUser(const Text: string): Boolean;
begin
    // Don't call inherited
    Result := MessageDlg(Text, mtWarning, [mbYes, mbNo], 0) = mrYes;
end;

procedure TBoldCLXEnvironmentConfiguration.BringToFront;
begin
  // Don't call inherited
  Application.BringToFront
end;

procedure TBoldCLXEnvironmentConfiguration.FocusMainForm;
begin
  // Don't call inherited
  if Assigned(Application) and Assigned(Application.MainForm) then
    Application.MainForm.SetFocus;
end;

function TBoldCLXEnvironmentConfiguration.GetName: string;
begin
  Result := 'CLX'; // do not localize
end;

function TBoldCLXEnvironmentConfiguration.GetQueueClass: TBoldQueueClass;
begin
  Result := TBoldTimerQueue;
end;

function TBoldCLXEnvironmentConfiguration.GetRootComponent: TComponent;
begin
  // Don't call inherited
  Result := Application;
end;

procedure TBoldCLXEnvironmentConfiguration.HandleDesigntimeException(
  Sender: TObject);
begin
  // Don't call inherited
  Application.HandleException(Sender);
end;

function TBoldCLXEnvironmentConfiguration.IsFormOrDataModule(
  Sender: TObject): Boolean;
begin
  // Don't call inherited
  Result := (Sender is TForm) or (Sender is TDataModule);
end;

procedure TBoldCLXEnvironmentConfiguration.ProcessMessages;
begin
  // Don't call inherited;
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
        // this happens if the model resides in a frame
        ParentForm := GetParentForm(TCustomFrame(Owner))
      else if (Owner is TCustomForm) then
        // this happens if the model resides on a form
        ParentForm := TCustomForm(Owner)
      else if (Owner is TDataModule) and Assigned(Owner.Owner) and (Owner.Owner is TCustomForm) then
        // this happens if the component resides on a Datamodule, the owner.owner is a TDatamoduleDesigner...
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
  // Don't call inherited, abstract in parent
  fBoldQueueActive := true;
  IdleTimer.Interval := 0;
end;

procedure TBoldTimerQueue.DeActivateDisplayQueue;
begin
  // Don't call inherited, abstract in parent
  fBoldQueueActive := false;
  IdleTimer.Interval := MaxInt;
end;

initialization
  BoldInternalCLXConfiguration := TBoldCLXEnvironmentConfiguration.Create; // Freed in finalization of BoldEnvironment
end.
