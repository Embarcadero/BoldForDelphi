unit BoldEnvironment;

interface

uses
  Classes,
  BoldDefs,
  BoldQueue;

type
  { forward declarations }
  TBoldEnvironmentConfiguration = class;
  TBoldApplicationSubscriber = class;
  TBoldFreestandingEnvironmentConfiguration = class;

  TBoldQueueClass = class of TBoldQueue;

  { TBoldApplicationSubscriber }
  TBoldApplicationSubscriber = class(TComponent)
  private
    fOnApplicationDestroyed: TBoldJustNotifyEvent;
  public
    procedure BeforeDestruction; override;
    property OnApplicationDestroyed: TBoldJustNotifyEvent read fOnApplicationDestroyed write fOnApplicationDestroyed;
  end;

  { TBoldEnvironmentConfiguration }
  TBoldEnvironmentConfiguration = class
  private
    fQueue: TBoldQueue;
    fApplicationSubscriber: TBoldApplicationSubscriber;
    fQueueFinalized: Boolean;
    procedure FinalizeQueue;
// this procedure should be called _ApplicationDestroyed
    procedure DeactivateQueue;

    function GetRunningInIDE: Boolean;
  protected
    function GetQueue: TBoldQueue;
    function GetQueueClass: TBoldQueueClass; virtual;
    procedure SetQueue(NewValue: TBoldQueue);
    function GetName: string; virtual; abstract;
    function GetRootComponent: TComponent; virtual;
  public
    destructor Destroy; override;
    procedure HandleDesigntimeException(Sender: TObject); virtual;
    procedure UpdateDesigner(Sender: TObject); virtual;
    function IsFormOrDataModule(Sender: TObject): Boolean; virtual;
    function AskUser(const Text: string): Boolean; virtual;
    procedure ProcessMessages; virtual;
    procedure BringToFront; virtual;
    procedure FocusMainForm; virtual;
    procedure TriggerQueueMechanism; virtual;
    property RootComponent: TComponent read GetRootComponent;
    property Name: string read GetName;
    property RunningInIDE: Boolean read GetRunningInIDE;
    property QueueFinalized: Boolean read fQueueFinalized;
    property Queue: TBoldQueue read GetQueue;
  end;

  { TBoldFreestandingEnvironmentConfiguration }
  TBoldFreestandingEnvironmentConfiguration = class(TBoldEnvironmentConfiguration)
  protected
    function GetName: string; override;
  public
    procedure BringToFront; override;
    procedure FocusMainForm; override;
  end;

var
  BoldInternalAllowBothUseVCL: Boolean = false;
  BoldInternalAllowBothUseCLX: Boolean = false;
  BoldInternalRunningInIDE: Boolean = false;

  BoldInternalVCLConfiguration: TBoldEnvironmentConfiguration = nil;
  BoldInternalCLXConfiguration: TBoldEnvironmentConfiguration = nil;
  BoldInternalCustomConfiguration: TBoldEnvironmentConfiguration = nil;
  BoldInternalFreestandingConfiguration: TBoldEnvironmentConfiguration = nil;

  function BoldEffectiveEnvironment: TBoldEnvironmentConfiguration;
  function BoldEnvironmentsFinalized: Boolean;

implementation

uses
  SysUtils,
  BoldEnvironmentVCL; //Temporary fix

const
  UnsupportedFunction = '%s: function unsupported in environment configuration %s';

var
  G_EnvironmentsFinalized: Boolean = false;

function BoldEffectiveEnvironment: TBoldEnvironmentConfiguration;
begin
   if Assigned(BoldInternalCustomConfiguration) then
    Result := BoldInternalCustomConfiguration
  else if Assigned(BoldInternalVCLConfiguration) and ((not Assigned(BoldInternalCLXConfiguration)) or BoldInternalAllowBothUseVCL) then
    Result := BoldInternalVCLConfiguration
  else if Assigned(BoldInternalCLXConfiguration) and ((not Assigned(BoldInternalVCLConfiguration)) or BoldInternalAllowBothUseCLX) then
    Result := BoldInternalCLXConfiguration
  else if Assigned(BoldInternalVCLConfiguration) and Assigned(BoldInternalCLXConfiguration) and not
    (BoldInternalAllowBothUseVCL and BoldInternalAllowBothUseCLX) then
    raise EBoldInternal.Create('BoldEffectiveEnvironment, conflict, both CLX and VCL installed and no preference given')
  else if Assigned(BoldInternalFreestandingConfiguration) then
    Result := BoldInternalFreestandingConfiguration
  else
    raise EBoldInternal.Create('BoldEffectiveEnvironment, no environment available. Unit BoldEnvironment either not initialized, or already finalized');
end;

function BoldEnvironmentsFinalized: Boolean;
begin
  result := G_EnvironmentsFinalized;
end;

{ TBoldEnvironmentConfiguration }

function TBoldEnvironmentConfiguration.AskUser(const Text: string): Boolean;
begin
  raise EBoldInternal.CreateFmt(UnsupportedFunction, ['AskUser', Name]);
end;

procedure TBoldEnvironmentConfiguration.BringToFront;
begin
  raise EBoldInternal.CreateFmt(UnsupportedFunction, ['BringToFront', Name]);
end;

function TBoldEnvironmentConfiguration.GetRootComponent: TComponent;
begin
  raise EBoldInternal.CreateFmt(UnsupportedFunction, ['GetRootComponentVCL', Name]);
end;

procedure TBoldEnvironmentConfiguration.HandleDesigntimeException(
  Sender: TObject);
begin
  raise EBoldInternal.CreateFmt(UnsupportedFunction, ['HandleDesigntimeException', Name]);
end;

function TBoldEnvironmentConfiguration.IsFormOrDataModule(Sender: TObject): Boolean;
begin
  raise EBoldInternal.CreateFmt(UnsupportedFunction, ['IsFormOrDataModule', Name]);
end;

procedure TBoldEnvironmentConfiguration.ProcessMessages;
begin
  raise EBoldInternal.CreateFmt(UnsupportedFunction, ['ProcessMessages', Name])
end;

function TBoldEnvironmentConfiguration.GetRunningInIDE: Boolean;
begin
  Result := BoldInternalRunningInIDE;
end;

procedure TBoldEnvironmentConfiguration.UpdateDesigner(Sender: TObject);
begin
  raise EBoldInternal.CreateFmt(UnsupportedFunction, ['UpdateDesigner', Name]);
end;

procedure TBoldEnvironmentConfiguration.FocusMainForm;
begin
  raise EBoldInternal.CreateFmt(UnsupportedFunction, ['FocusMainForm', Name]);
end;

function TBoldEnvironmentConfiguration.GetQueue: TBoldQueue;
begin
  if not Assigned(fQueue) and not QueueFinalized then
  begin
    fQueue := GetQueueClass.Create;
    fApplicationSubscriber := TBoldApplicationSubscriber.Create(RootComponent);
    fApplicationSubscriber.OnApplicationDestroyed := DeactivateQueue;
  end;
  Result := fQueue;
end;

procedure TBoldEnvironmentConfiguration.FinalizeQueue;
begin
  //NOTE: Cannot use FreeAndNil, as it does things in the wrong order.
  if Assigned(fQueue) then
  begin
    fQueue.Free;
    fQueue := nil;
    if assigned(fApplicationSubscriber) then
    begin
      if csDestroying in fApplicationSubscriber.ComponentState then
      begin
        fApplicationSubscriber.OnApplicationDestroyed := nil;
        fApplicationSubscriber := nil
      end
      else
        FreeAndNil(fApplicationSubscriber);
    end;
  end;
  fQueueFinalized := true;
end;

destructor TBoldEnvironmentConfiguration.Destroy;
begin
  if not QueueFinalized then
    FinalizeQueue;
  inherited;
end;

procedure TBoldEnvironmentConfiguration.SetQueue(NewValue: TBoldQueue);
begin
  FQueue := NewValue;
end;

function TBoldEnvironmentConfiguration.GetQueueClass: TBoldQueueClass;
begin
  raise EBoldInternal.CreateFmt(UnsupportedFunction, ['GetQueueClass', Name]);
end;

procedure TBoldEnvironmentConfiguration.TriggerQueueMechanism;
begin
  raise EBoldInternal.CreateFmt(UnsupportedFunction, ['TriggerQueueMechanism', Name]);
end;

// this procedure should be called _ApplicationDestroyed
procedure TBoldEnvironmentConfiguration.DeactivateQueue;
begin
  fApplicationSubscriber := nil;
  if assigned(fQueue) and not QueueFinalized then
    fQueue.DeActivateDisplayQueue;
end;

{ TBoldFreestandingEnvironmentConfiguration }

procedure TBoldFreestandingEnvironmentConfiguration.BringToFront;
begin
  // Don't call inherited
  // Ignore silently
end;

procedure TBoldFreestandingEnvironmentConfiguration.FocusMainForm;
begin
  // Don't call inherited
  // Ignore silently
end;

function TBoldFreestandingEnvironmentConfiguration.GetName: string;
begin
  Result := 'Freestanding'; // do not localize
end;

{ TBoldApplicationSubscriber }


procedure TBoldApplicationSubscriber.BeforeDestruction;
begin
  inherited;
  if Assigned(OnApplicationDestroyed) then
  begin
    OnApplicationDestroyed;
  end;
end;

initialization
  BoldInternalFreestandingConfiguration :=  TBoldFreestandingEnvironmentConfiguration.Create;

finalization
  FreeAndNil(BoldInternalVCLConfiguration);
  FreeAndNil(BoldInternalCLXConfiguration);
  FreeAndNil(BoldInternalCustomConfiguration);
  FreeAndNil(BoldInternalFreestandingConfiguration);
  G_EnvironmentsFinalized := true;

end.
