
{ Global compiler directives }
{$include bold.inc}
unit BoldAdvancedPropagator;

interface

uses
  BoldBase,
  BoldClientHandler,
  BoldEnqueuer,
  BoldSubscriptionHandler,
  BoldOutputQueueHandler,
  BoldClientNotifierHandler,
  BoldPriorityListEnlister,
  BoldPropagatorCleanUp,
  BoldPropagatorUIManager,
  BoldSubscription,
  BoldThreadSafeLog,
  IniFiles,
  BoldPropagatorMainForm,
  Classes;

type
  {forward declarations}
  TBoldAdvancedPropagator = class;

  {TBoldAdvancedPropagator}
  TBoldAdvancedPropagator = class
  private
    fClientHandler: TBoldClientHandler;
    fEnqueuer: TBoldEnqueuer;
    fDequeuer: TBoldDequeuer;
    fClientNotifierHandler: TBoldClientNotifierHandler;
    fPriorityListEnlister: TBoldPriorityListEnlister;
    fCleanUpSubscriber: TBoldCleanUpSubscriber;
    fUIManager: TUIManager;
    fEnableLogging: Boolean;
    fMaxLogFileSize: Integer;
    fClientNotifierPoolSize: integer;
    fLogFileName, fErrorLogFileName, fThreadLogfileName: string;
    fDisconnectClientsOnSendFailure: Boolean;
    fInitialized: Boolean;
    procedure StartDequeue(Sender: TObject);
    procedure DoneDequeue(Sender: TObject);
  protected
    function getClientHandler: TBoldClientHandler; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetConfigurationParams(const cClientNotifierPoolSize: integer; const cEnableLogging: Boolean;
                const cLogFileName, cErrorLogFileName, cThreadLogFileName: string; const cMaxLogFileSize: integer; const cDisconnectClientsOnSendFailure: Boolean);
    procedure Initialize;
    property ClientHandler: TBoldClientHandler read getClientHandler;
    property Enqueuer: TBoldEnqueuer read fEnqueuer;
    property Dequeuer: TBoldDequeuer read fDequeuer;
    property ClientNotifierHandler: TBoldClientNotifierHandler read fClientNotifierHandler;
    property PriorityListEnlister: TBoldPriorityListEnlister read fPriorityListEnlister;
    property CleanUpSubscriber: TBoldCleanUpSubscriber read fCleanUpSubscriber;
    property Initialized: Boolean read FInitialized;
    property DisconnectClientsOnSendFailure: Boolean read fDisconnectClientsOnSendFailure;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldPropagatorConstants,
  BoldDefs,
  BoldPropagatorServer;

  {TBoldAdvancedPropagator}
procedure TBoldAdvancedPropagator.Initialize;
begin
  if fEnableLogging then
    BoldInitLog(fLogFileName, fErrorLogFileName, fThreadLogFileName, fMaxLogFileSize);
  BoldLogError(sInitializing, [ClassName]);
  fClientHandler := TBoldClientHandler.Create;
  fEnqueuer := TBoldEnqueuer.Create(fClientHandler);
  fUIManager := TUIManager.Create;
  fUIManager.ClientHandler := fClientHandler;
  fDequeuer:= TBoldDequeuer.Create(fEnqueuer.InQueue);
  fDequeuer.OnStartDequeue := StartDequeue;
  fDequeuer.OnDoneDequeue := DoneDequeue;
  fDequeuer.OnGlobalInfoChanged := fUIManager.GlobalInfoChanged;
  fDequeuer.OnClientInfoChanged := fUIManager.ClientInfoChanged;
  fPriorityListEnlister:= TBoldPriorityListEnlister.Create;
  fClientNotifierHandler:= TBoldClientNotifierHandler.Create(fClientNotifierPoolSize, fClientHandler, fPriorityListEnlister.PriorityList, fDisconnectClientsOnSendFailure);
  fCleanUpSubscriber := TBoldCleanupSubscriber.Create(fClienthandler);
  fClientNotifierHandler.Resume;
  fClientNotifierHandler.WaitUntilReady(TIMEOUT);
  fDequeuer.Resume;
  fDequeuer.WaitUntilReady(TIMEOUT);
  FInitialized := True;
end;

destructor TBoldAdvancedPropagator.Destroy;
begin
  if fInitialized then
  begin
    fClientHandler.Enabled := false;
    fUIManager.Enabled := false;
    fUIManager.ClientHandler := nil;

    fDequeuer.OnStartDequeue := nil;
    fDequeuer.OnDoneDequeue := nil;
    fDequeuer.OnClientInfoChanged := nil;
    fDequeuer.OnGlobalInfoChanged := nil;
    fEnqueuer.Enabled := false;
    FreeAndNil(fCleanUpsubscriber);
    fClientNotifierHandler.Quit(True);
    FreeAndNil(fClientNotifierHandler);
    fDequeuer.Quit(True);

    FreeAndNil(fDequeuer);
    FreeAndNil(fUIManager);
    FreeAndNil(fEnqueuer);
    FreeAndNil(fPriorityListEnlister);
    FreeAndNil(fClientHandler);
  end;
  BoldLogError(sDestroying, [ClassName]);

  BoldDoneLog;
  inherited;
end;

procedure TBoldAdvancedPropagator.SetConfigurationParams(
  const cClientNotifierPoolSize: integer; const cEnableLogging: Boolean;
  const cLogFileName, cErrorLogFileName, cThreadLogFileName: string;
  const cMaxLogFileSize: integer; const cDisconnectClientsOnSendFailure: Boolean);
begin
  fClientNotifierPoolSize := cClientNotifierPoolSize;
  fEnableLogging := cEnableLogging;
  fMaxLogFileSize := cMaxLogFileSize;
  fLogFileName := cLogFileName;
  fErrorLogFileName := cErrorLogFileName;
  fThreadLogFileName := cThreadLogFileName;
  fDisconnectClientsOnSendFailure := cDisconnectClientsOnSendFailure
end;

constructor TBoldAdvancedPropagator.Create;
begin
  inherited;
  FInitialized := false;
  FEnableLogging := DEFAULT_ENABLELOGGING;
  FMaxLogFileSize := DEFAULT_LOGFILESIZE;
  FClientNotifierPoolSize := DEFAULT_THREADPOOLSIZE;
end;

procedure TBoldAdvancedPropagator.DoneDequeue(Sender: TObject);
begin
  try
    if Assigned(fUIManager) then
      fUIManager.SetDequeueIndicator(false);
  except on
    E: Exception do
      BoldLogError(sQueueError, [ClassName, E.message])
  end;
end;

procedure TBoldAdvancedPropagator.StartDequeue(Sender: TObject);
begin
  if Assigned(fUIManager) then
    fUIManager.SetDequeueIndicator(true);
end;                                          

function TBoldAdvancedPropagator.getClientHandler: TBoldClientHandler;
begin
  Result := fClientHandler;
end;

end.
