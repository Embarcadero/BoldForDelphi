
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
  BoldDefs,
  Forms,
  BoldQueue,
  BoldEnvironment,
  BoldRev;

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
  if not RunningInIDE then
    raise EBold.CreateFmt('%s: Not running in IDE', ['UpdateDesigner']);
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
          raise EBoldInternal.CreateFmt('%s.ApplicationEventsOnIdle: Unknown displaymode', [classname]);
      end;
      if Done then
        PerformPostDisplayQueue;
    end;
  except
    Application.HandleException(nil);
  end;

end;

procedure TBoldAppEventQueue.DeActivateDisplayQueue;
begin
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
