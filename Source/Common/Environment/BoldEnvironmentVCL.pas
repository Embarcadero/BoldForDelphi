unit BoldEnvironmentVCL;

interface

implementation

uses
  SysUtils,
  Classes,
  Windows,
  Messages,
  AppEvnts,
  Controls,
  Dialogs,
  BoldDefs,
  Forms,
  BoldQueue,
  BoldEnvironment,
  BoldCommonConst;

type
  { forward declarations }
  TBoldAppEventQueue = class;
  TBoldVCLEnvironmentConfiguration = class;

  { TBoldAppEventQueue }
  TBoldAppEventQueue = class(TBoldQueue)
  private
    fAppEvents: TApplicationEvents;
    fBoldQueueActive: Boolean;
    procedure ApplicationEventsOnIdle(Sender: TObject; var Done: Boolean);
    function GetAppEvents: TApplicationEvents;
  protected
    property AppEvents: TApplicationEvents read GetAppEvents;
  public
    destructor Destroy; override;
    procedure DeActivateDisplayQueue; override;
    procedure ActivateDisplayQueue; override;
  end;

  { TBoldVCLEnvironmentConfiguration }
  TBoldVCLEnvironmentConfiguration = class(TBoldEnvironmentConfiguration)
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
    procedure TriggerQueueMechanism; override;
    procedure FocusMainForm; override;
  end;

function TBoldVCLEnvironmentConfiguration.AskUser(const Text: string): Boolean;
begin
  // Don't call inherited
  Result := MessageDlg(Text, mtWarning, [mbYes, mbNo], 0) = mrYes;
end;

procedure TBoldVCLEnvironmentConfiguration.BringToFront;
begin
  // Don't call inherited
  Application.BringToFront
end;

procedure TBoldVCLEnvironmentConfiguration.FocusMainForm;
begin
  // Don't call inherited
  if Assigned(Application) and Assigned(Application.MainForm) then
    Application.MainForm.SetFocus;
end;

function TBoldVCLEnvironmentConfiguration.GetName: string;
begin
  Result := 'VCL'; // do not localize
end;

function TBoldVCLEnvironmentConfiguration.GetQueueClass: TBoldQueueClass;
begin
  Result := TBoldAppEventQueue;
end;

function TBoldVCLEnvironmentConfiguration.GetRootComponent: TComponent;
begin
  // Don't call inherited
  Result := Application;
end;

procedure TBoldVCLEnvironmentConfiguration.HandleDesigntimeException(Sender: TObject);
begin
  // Don't call inherited
  Application.HandleException(Sender);
end;

function TBoldVCLEnvironmentConfiguration.IsFormOrDataModule(Sender: TObject): Boolean;
begin
  // Don't call inherited
  Result := (Sender is TForm) or (Sender is TDataModule);
end;

procedure TBoldVCLEnvironmentConfiguration.ProcessMessages;
begin
  // Don't call inherited;
  Application.ProcessMessages;
end;

procedure TBoldVCLEnvironmentConfiguration.TriggerQueueMechanism;
begin
  // is there a better message to send to get the applicaiton to exit the idlestate?
  // perhaps a timer?
  // remember that it might be called from another thread...
  PostMessage(Application.Handle, WM_PAINT, 0, 0);
end;

procedure TBoldVCLEnvironmentConfiguration.UpdateDesigner(Sender: TObject);
var
  ParentForm: TCustomForm;
  Owner: TComponent;
begin
  // Don't call inherited
  if not RunningInIDE then
    raise EBold.CreateFmt(sNotRunningInIDE, ['UpdateDesigner']); // do not localize
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
  if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
    ParentForm.Designer.Modified;
end;

{ TBoldAppEventQueue }

procedure TBoldAppEventQueue.ActivateDisplayQueue;
begin
  // Don't call inherited, abstract in parent
  fBoldQueueActive := true;
  AppEvents.OnIdle := ApplicationEventsOnIdle;
end;

procedure TBoldAppEventQueue.ApplicationEventsOnIdle(Sender: TObject; var Done: Boolean);
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
end;

procedure TBoldAppEventQueue.DeActivateDisplayQueue;
begin
  // Don't call inherited, abstract in parent
  fBoldQueueActive := false;
  AppEvents.OnIdle := nil;
end;

destructor TBoldAppEventQueue.Destroy;
begin
  FreeAndNil(fAppEvents);
  inherited;
end;

function TBoldAppEventQueue.GetAppEvents: TApplicationEvents;
begin
  if not Assigned(fAppEvents) then
    fAppEvents := TApplicationEvents.create(nil);
  Result := fAppEvents;
end;

initialization
  BoldInternalVCLConfiguration := TBoldVCLEnvironmentConfiguration.Create;

end.
