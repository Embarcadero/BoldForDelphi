
{ Global compiler directives }
{$include bold.inc}
unit BoldClientNotifierHandler;

interface

uses
  SysUtils,
  BoldUtils,
  extctrls,
  Variants,
  BoldPropagatorInterfaces_TLB,
  BoldThread,
  BoldDefs,
  BoldClientHandler,
  BoldPriorityQueue,
  BoldClientQueue,
  BoldPropagatorConstants,
  BoldThreadSafeLog,
  ActiveX,
  windows,
  messages,
  classes,
  contnrs,
  syncobjs;

type
  {forward declarations}
  TBoldClientNotifierHandler = class;
  TBoldClientNotifier = class;
  TBoldClientNotifierPool = class;

  TBoldClientNotifierHandler = class(TBoldNotifiableThread)
  private
    fTimer: TTimer;
    fClientNotifierPool: TBoldClientNotifierPool;
    fClientNotifierPoolSize: integer;
    fScheduledClients: TQueue;
    fPriorityList: TBoldPriorityQueue;
    fClientHandler: TBoldClienthandler;
    fDisconnectClientsOnSendFailure: Boolean;
    procedure SetTimer(TimePoint: TTimeStamp);
    function getTimer: TTimer;
    function getClientNotifierPool: TBoldClientNotifierPool;
    procedure OnTimer(Sender: TObject);
    procedure SendToClient;
    procedure SetPriorityList(const Value: TboldPriorityQueue);
    function getPriorityList: TBoldPriorityQueue;
  protected
    function getClientHandler: TBoldClientHandler; virtual;
    procedure ProcessPriorityListHead;
    function ProcessScheduledClientsHead: Boolean;
    procedure DisconnectClient(const ClientId: TBoldClientID; const RegistrationTime: TTimeStamp);
    property ClientNotifierPool: TBoldClientNotifierPool read getClientNotifierPool write fClientNotifierPool;
    property Timer: TTimer read getTimer;
    property ClientHandler: TBoldClientHandler read GetClientHandler;
  public
    constructor Create(const ClientNotifierPoolSize: integer; aClientHandler: TBoldClientHandler; aPrioritylist: TBoldPriorityQueue; DisconnectClientsOnSendFailure: Boolean);
    destructor Destroy; override;
    procedure Execute; override;
    procedure OnPriorityChanged(Sender: TObject);
    property PriorityList: TboldPriorityQueue read GetPriorityList write SetPriorityList;
  end;

  TBoldClientNotifierData = class
  private
    FClientID: TBoldClientID;
    FRegistrationTime: TTimeStamp;
    FEvents: variant;
  protected
  public
    constructor CreateInitialized(const ClientID: TBoldClientID; const RegistrationTime: TTimeStamp; Events: variant);
  end;

  TBoldClientNotifier = class (TBoldNotifiableThread)
  private
    fAvailableEvent: TSimpleEvent;
    fOwner: TBoldClientNotifierHandler;
    fClientData: TBoldClientNotifierData;
    function GetAvailable: Boolean;
  protected
    property IsAvailable: Boolean read GetAvailable;
    procedure SetClientData(aData: TBoldClientNotifierData);
    procedure DoWork;
    property Owner: TBoldClientNotifierHandler read fOwner;
  public
    constructor Create(AOwner: TBoldClientNotifierHandler);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TBoldClientNotifierPool = class
  private
    fThreadList: TList;
    CurrentThread: integer;
    fPoolSize: integer;
  protected
    function ScheduleClientNotifier(mData: TBoldClientNotifierData): Boolean;
  public
    constructor Create(ClientHandler: TBoldClientNotifierHandler; const PoolSize: integer);
    destructor Destroy; override;
  end;

implementation

uses
  Comobj,

  BoldCoreConsts,
  BoldGuard,
  BoldPriorityListEnlister,
  BoldPropagatorServer;

{ TBoldClientNotifierHandler }

procedure TBoldClientNotifierHandler.Execute;
var
  rMsg: TMsg;
begin
  try
    EnsureMessageQueue;
    BoldLogThread('ID=ClientNotifierHandler');
    SignalReady;
    while not Terminated do
    begin
      if PeekMessage(rMsg, 0, 0, 0, PM_REMOVE) then
      begin
        if rMsg.Message = WM_QUIT then
          Terminate
        else if rMsg.message = BM_PRIORITY_CHANGED then
          ProcessPriorityListHead
        else
          DispatchMessage(rMsg);
      end
      else if fScheduledClients.Count > 0 then
        ProcessScheduledClientsHead
      else
        WaitMessage;
    end;
  except on E: Exception do
    BoldLogError(sLogError, [ClassName, 'Execute', E.Message]); // do not localize
  end;
  FreeAndNil(fTimer);
end;

function TBoldClientNotifierHandler.getClientHandler: TBoldClientHandler;
begin
  Result := fClientHandler;
end;

function TBoldClientNotifierHandler.getClientNotifierPool: TBoldClientNotifierPool;
begin
  Result := fClientNotifierPool;
end;

function TBoldClientNotifierHandler.getTimer: TTimer;
begin
  Assert(GetCurrentThreadId = self.ThreadID, Format(sTimerAccessed, [ClassName]));
  if not Assigned(fTimer) then
  begin
    fTimer := TTimer.Create(nil);
    fTimer.Enabled := false;
    fTimer.OnTimer := OnTimer;
  end;
  Result := fTimer;
end;

procedure TBoldClientNotifierHandler.OnPriorityChanged(Sender: TObject);
var
  PriorityQueue: TBoldPriorityQueue;
begin
  Assert(Sender is TBoldPriorityQueue, Format(sSenderNotInQueue, [ClassName]));
  PriorityQueue := Sender as TBoldPriorityQueue;
  if Assigned(PriorityQueue.Head) then
    Notify(BM_PRIORITY_CHANGED);
end;

procedure TBoldClientNotifierHandler.OnTimer(Sender: TObject);
begin
  Timer.Enabled := false;
  SendToClient;
end;

procedure TBoldClientNotifierHandler.SendtoClient;
var
  ClientQueueInfo: TBoldClientQueueInfo;
  ClientIdString: string;
  RegistrationTime: TTimeStamp;
  Initialized: Boolean;
  Status: TBoldClientReceiveStatus;
  aData: TBoldClientNotifierData;
begin
  if Assigned(PriorityList) then
  begin
    ClientQueueInfo := TBoldClientQueueInfo(PriorityList.ChopHead);
    try
      if Assigned(ClientQueueInfo) and (ClientQueueInfo.Count <> 0) and
         ClientHandler.HasInfoForClient(ClientQueueInfo.ClientID, ClientIdString, RegistrationTime, Initialized, Status) and Initialized then
        try
          aData := TBoldClientNotifierData.CreateInitialized(ClientQueueInfo.ClientID,
                    RegistrationTime, ClientQueueInfo.QueueAsVarArray);
          fScheduledClients.Push(Pointer(aData));
          while ProcessScheduledClientsHead do;
        except
          raise EBold.CreateFmt(sCouldNotSendEvents, [ClassName]);
        end;
    finally
      FreeAndNil(ClientQueueInfo);
    end;
  end;  
end;

procedure TBoldClientNotifierHandler.ProcessPriorityListHead;
var
  ClientQueueInfo: TBoldClientQueueInfo;
  CurrentTime: TTimeStamp;
begin
  if Assigned(PriorityList.Head) then
  begin
    ClientQueueInfo := TBoldClientQueueInfo(PriorityList.Head);
    if (ClientQueueInfo.Count = 0) then
      PriorityList.RemoveAndFreeHead
    else
    begin
      CurrentTime := DateTimeToTimeStamp(Now);
      if(ClientQueueInfo.TimeOut.Date <= CurrentTime.Date) and
      ((ClientQueueInfo.TimeOut.Time - CurrentTime.Time) <= 1) then
        SendToClient
      else
        SetTimer(ClientQueueInfo.TimeOut);
    end;
  end;
end;

procedure TBoldClientNotifierHandler.SetTimer(TimePoint: TTimeStamp);
begin
  Timer.Enabled := false;
  Timer.Interval := Trunc(TimePoint.Time - DateTimeToTimeStamp(Now).Time);
  Timer.Enabled := true;
end;

constructor TBoldClientNotifierHandler.Create(const ClientNotifierPoolSize: integer; aClientHandler: TBoldClientHandler; aPriorityList: TBoldPriorityQueue; DisconnectClientsOnSendFailure: Boolean);
begin
  inherited Create(True);
  fClientNotifierPoolSize := ClientNotifierPoolSize;
  fClientNotifierPool := TBoldClientNotifierPool.Create(self, fClientNotifierPoolSize);
  fScheduledClients := TQueue.Create;
  fClientHandler := aClienthandler;
  fDisconnectClientsOnSendFailure := DisconnectClientsOnSendFailure;
  PriorityList := aPriorityList;
end;

procedure TBoldClientNotifierHandler.DisconnectClient(
  const ClientId: TBoldClientID; const RegistrationTime: TTimeStamp);
begin
  ClientHandler.DisconnectClient(ClientId, RegistrationTime);
end;

destructor TBoldClientNotifierHandler.Destroy;
begin
  FreeAndNil(fClientNotifierPool);
  FreeAndNil(fScheduledClients);
  inherited;
end;

function TBoldClientNotifierHandler.ProcessScheduledClientsHead: Boolean;
var
  aData: TBoldClientNotifierData;
begin
  Result := fScheduledClients.Count > 0;
  if Result then
  begin
    aData := TBoldClientNotifierData(fScheduledClients.Peek);
    if ClientNotifierPool.ScheduleClientNotifier(aData) then
      fScheduledClients.Pop;
  end;
end;

function TBoldClientNotifierHandler.getPriorityList: TBoldPriorityQueue;
begin
  Result := fPriorityList;
end;

procedure TBoldClientNotifierHandler.SetPriorityList(
  const Value: TboldPriorityQueue);
begin
  if (Value <> fPrioritylist) then
  begin
    if Assigned(fPriorityList) then
      fPriorityList.OnHeadChanged := OnPriorityChanged;
    fPriorityList := Value;
    if Assigned(fPriorityList) then
      fPriorityList.OnHeadChanged := OnPriorityChanged;
  end;
end;

{ TBoldClientNotifier }

constructor TBoldClientNotifier.Create(AOwner: TBoldClientNotifierhandler);
begin
  inherited Create(true);
  fAvailableEvent := TSimpleEvent.Create;
  fOwner := AOwner;
end;

procedure TBoldClientNotifier.Execute;

  function BoolToStr(value: Boolean): string;
  begin
    if Value then
      Result := 'True' // do not localize
    else
      Result := 'False'; // do not localize
  end;
var
  res: integer;
  rMsg: TMsg;
  aListener: IBoldListener;
  ClientIdentifier: string;
  RegistrationTime,
  LeaseTimeout: TTimeStamp;
  RegistrationTimeDT: TDateTime;
  LeaseTimeoutDT: TDateTime;
  DisconnectMsg: string;
  LeaseDuration, PollingInterval: integer;
  Initialized: Boolean;
  Status: TBoldClientReceiveStatus;
begin
  EnsureMessageQueue;
  SignalReady;
  BoldLogThread('ID=ClientNotifier'); // do not localize
  fAvailableEvent.SetEvent;
  while not (Terminated) do
  begin
    res := Integer(getMessage(rMsg, 0, 0, 0));
    if res = -1 then
      Terminate
    else if res = 0 then
      Terminate
    else if rMsg.message = BM_THRD_DOWORK then
    begin
      // Do work here
      CoInitializeEx(nil, CoInitFlags);
      try
        if Owner.ClientHandler.GetListener(fClientData.FclientId,
          fClientData.FRegistrationTime, aListener) then
        begin
          try
            if Assigned(aListener) then
            begin
              aListener.ReceiveEvents(fClientData.FEvents);
              Owner.ClientHandler.MarkHasReceivedEvents(fClientData.FClientID, VarArrayHighBound(fClientData.fEvents, 1)+1);
            end;
          except on E: Exception do
            begin
              Owner.ClientHandler.HasInfoForClient(fClientData.FClientID, clientIdentifier, registrationTime, initialized, Status);
              Owner.ClientHandler.HasInfoForClient(fClientData.FClientID, clientIdentifier, LeaseDuration, pollingInterval, LeaseTimeout, Initialized);
              RegistrationTimeDT := TimeStampToDateTime(RegistrationTime);
              LeaseTimeoutDT := TimeStampToDateTime(LeaseTimeout);
              Owner.ClientHandler.MarkFailedToReceiveEvents(fClientData.FClientID, VarArrayHighBound(fClientData.fEvents, 1)+1);
              if Owner.fDisconnectClientsOnSendFailure then
                DisconnectMsg := ' [Disconnected]'
              else
                DisconnectMsg := '';
              BoldLogError(sExecuteError, [
                ClassName,
                ClientIdentifier,
                fClientData.FClientId,
                DisconnectMsg,
                VarArrayHighBound(fClientData.fEvents, 1)+1, E.Message,
                DateTimeToStr(RegistrationTimeDT),
                TimeToStr(now-registrationTimeDT),
                TimeToStr(LeaseTimeoutDT),
                TimeToStr(now-LeaseTimeOutDT)]);
              if Owner.fDisconnectClientsOnSendFailure then
                // disconnect the client
                Owner.DisconnectClient(fClientData.FClientID, fClientData.fRegistrationTime);
             end;
          end;
        end;
      finally
        aListener := nil;
        FreeAndNil(fClientData);
        CoUninitialize;
        fAvailableEvent.SetEvent;
      end;
    end
    else begin
      DispatchMessage(rMsg);
    end;
  end;
  if Assigned(fClientData) then
    FreeAndNil(fClientData);
end;

destructor TBoldClientNotifier.Destroy;
begin
  FreeAndNil(fAvailableEvent);
  inherited;
end;

function TBoldClientNotifier.GetAvailable: Boolean;
begin
  Result := fAvailableEvent.WaitFor(0) = wrSignaled;
end;

procedure TBoldClientNotifier.SetClientData(
  aData: TBoldClientNotifierData);
begin
  fAvailableEvent.ResetEvent;
  fClientData:= aData;
end;

procedure TBoldClientNotifier.DoWork;
begin
  Notify(BM_THRD_DOWORK);
end;

{ TBoldClientNotifierPool }

constructor TBoldClientNotifierPool.Create(ClientHandler: TBoldClientNotifierHandler; const PoolSize: integer);
var
  i: integer;
  ClientNotifier: TBoldClientNotifier;
begin
  inherited Create;
  fPoolSize := PoolSize;
  fThreadList := TList.Create;
  for i:= 0 to fPoolSize - 1 do
  begin
    ClientNotifier := TBoldClientNotifier.Create(ClientHandler);
    fThreadList.Add(Pointer(ClientNotifier));
    ClientNotifier.Resume;
    ClientNotifier.WaitUntilReady(TIMEOUT*2);
  end;
  CurrentThread := 0;
end;

destructor TBoldClientNotifierPool.Destroy;
var
  i: integer;
  ClientNotifier: TBoldClientNotifier;
  handles: TWOHandleArray;
  nCount: integer;
begin
  nCount := fPoolSize;
  for i:= 0 to fPoolSize - 1 do
  begin
    ClientNotifier := TBoldClientNotifier(fThreadList[i]);
    ClientNotifier.Quit(True);
    handles[i] := ClientNotifier.Handle;
  end;
  SwitchToThread;
  WaitForMultipleObjects(nCount, @handles, True, TIMEOUT* fPoolSize);
  for i := 0 to fPoolSize - 1 do
  begin
    ClientNotifier := TBoldClientNotifier(fThreadList[i]);
    FreeAndNil(ClientNotifier);
  end;
  FreeAndNil(fThreadList);
  inherited;
end;

function TBoldClientNotifierPool.ScheduleClientNotifier(mData: TBoldClientNotifierData): Boolean;
var
  ClientNotifier: TBoldClientNotifier;
begin
  Result := false;
  try
    ClientNotifier := TBoldClientNotifier(fThreadList[CurrentThread]);
    if ClientNotifier.IsAvailable then
    begin
      ClientNotifier.SetClientData(mData);
      ClientNotifier.DoWork;
      Result := true;
    end;
    CurrentThread := (CurrentThread + 1) mod fPoolSize;
  except on E: Exception do
    BoldLogError(sLogError, [ClassName, 'ScheduleClientNotifier', E.Message]); // do not localize
  end;
end;

{ TBoldClientNotifierData }

constructor TBoldClientNotifierData.CreateInitialized(const ClientID: TBoldClientID; const RegistrationTime: TTimeStamp; Events: variant);
begin
  inherited Create;
  FEvents := Events;
  FClientID := ClientID;
  fRegistrationTime := RegistrationTime;
end;

end.
