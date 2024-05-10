
{ Global compiler directives }
{$include bold.inc}
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
  Forms,

  BoldCoreConsts,
  BoldDefs,
  BoldQueue,
  BoldEnvironment,
  BoldRev;

type
  { forward declarations }
  TBoldAppEventQueue = class;
  TBoldVCLEnvironmentConfiguration = class;

  TBoldAppEventQueueDisplayMode = (dmOnMessage, dmOnIdle);

  { TBoldAppEventQueue }
  TBoldAppEventQueue = class(TBoldQueue)
  private
    fAppEvents: TApplicationEvents;
    fBoldQueueActive: Boolean;
    fQueueDisplayMode: TBoldAppEventQueueDisplayMode;
    procedure ApplicationEventsOnIdle(Sender: TObject; var Done: Boolean);
    procedure ApplicationEventsOnMessage(var Msg: TMsg; var Handled: Boolean);
    function GetAppEvents: TApplicationEvents;
  protected
    function GetIsActive: boolean; override;
    property AppEvents: TApplicationEvents read GetAppEvents;
  public
    destructor Destroy; override;
    procedure ActivateDisplayQueue; override;
    procedure DeActivateDisplayQueue; override;
    property QueueDisplayMode: TBoldAppEventQueueDisplayMode read fQueueDisplayMode write fQueueDisplayMode;
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
  Result := MessageDlg(Text, mtWarning, [mbYes, mbNo], 0) = mrYes;
end;

procedure TBoldVCLEnvironmentConfiguration.BringToFront;
begin
  Application.BringToFront
end;

procedure TBoldVCLEnvironmentConfiguration.FocusMainForm;
begin
  if Assigned(Application) and Assigned(Application.MainForm) then
    Application.MainForm.SetFocus;
end;

function TBoldVCLEnvironmentConfiguration.GetName: string;
begin
  Result := 'VCL';
end;

function TBoldVCLEnvironmentConfiguration.GetQueueClass: TBoldQueueClass;
begin
  Result := TBoldAppEventQueue;
end;

function TBoldVCLEnvironmentConfiguration.GetRootComponent: TComponent;
begin
  Result := Application;
end;

procedure TBoldVCLEnvironmentConfiguration.HandleDesigntimeException(Sender: TObject);
begin
  Application.HandleException(Sender);
end;

function TBoldVCLEnvironmentConfiguration.IsFormOrDataModule(Sender: TObject): Boolean;
begin
  Result := (Sender is TForm) or (Sender is TDataModule);
end;

procedure TBoldVCLEnvironmentConfiguration.ProcessMessages;
begin
  Application.ProcessMessages;
end;

procedure TBoldVCLEnvironmentConfiguration.TriggerQueueMechanism;
begin


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
        ParentForm := GetParentForm(TCustomFrame(Owner))
      else if (Owner is TCustomForm) then
        ParentForm := TCustomForm(Owner)
      else if (Owner is TDataModule) and Assigned(Owner.Owner) and (Owner.Owner is TCustomForm) then
        ParentForm := TCustomForm(Owner.Owner);
    end;
  end;
  if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
    ParentForm.Designer.Modified;
end;

{ TBoldAppEventQueue }

procedure TBoldAppEventQueue.ActivateDisplayQueue;
begin
  inherited;
  fBoldQueueActive := true;
  case QueueDisplayMode of
    dmOnIdle: AppEvents.OnIdle := ApplicationEventsOnIdle;
    dmOnMessage: AppEvents.OnMessage := ApplicationEventsOnMessage;
  end;
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
      if Done then
        PerformPostDisplayQueue;
    end;
  except
    Application.HandleException(nil);
  end;
end;

procedure TBoldAppEventQueue.ApplicationEventsOnMessage(var Msg: TMsg;
  var Handled: Boolean);
begin
  if IsActive then
    ApplicationEventsOnIdle(nil, Handled);
  Handled := false;
end;

procedure TBoldAppEventQueue.DeActivateDisplayQueue;
begin
  inherited;
  fBoldQueueActive := false;
  if Assigned(fAppEvents) then
  begin
    AppEvents.OnIdle := nil;
    AppEvents.OnMessage := nil;
  end;
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

function TBoldAppEventQueue.GetIsActive: boolean;
begin
  result := fBoldQueueActive;
end;

initialization
  BoldInternalVCLConfiguration := TBoldVCLEnvironmentConfiguration.Create;

end.
