unit maanClientHandler;

{$INCLUDE Bold.inc}

interface

uses
  {$IFDEF BOLD_DELPHI6_OR_LATER}
  variants,
  {$ENDIF}
  TestSuite,
  sysutils,
  BoldDefs,
  BoldClientHandler,
  BoldPropagatorInterfaces_TLB,
  BoldPropagatorCleanUp,
  classes,
  TestFramework;

type

  TTestableClientHandler = class(TBoldClientHandler)
  protected
    procedure EnqueueRemoveClientQueueEvent(const ClientId: TBoldClientId); override;
  end;

  TTestBoldListener = class(TInterfacedObject,IBoldListener)
  private
    fEvents: TStringList;
    function getEvents: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    {IBoldListener}
    function ReceiveEvents(Events: OleVariant) : integer; safecall;
    property Events: TStringList read getEvents;
  end;

  TMaan_TestCaseClientHandler = class(TBoldTestCase)
  private
    fClientHandler: TTestableClientHandler;
    fClientIdentifierString: string;
    ListenerIntf: IBoldListener;
    fLeaseDuration: integer;
    fPollingInterval: integer;
    fRegistrationTime: TTimeStamp;
    ID: integer;
    IDs: TList;
  public
    ClientID1: TBoldClientID;
    ClientIdentifierString1: string;
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    procedure SetUp; override;
    procedure TearDown; override; // cleanup after tests
    property ClientHandler: TTestableClientHandler read fClientHandler write fClientHandler;
  published
    procedure RegisterClient;
    procedure ExtendLease;
    procedure UnRegisterClient;
  end;


implementation

uses
  BoldUtils,
  BoldRev,
  BoldThreadSafeQueue,
  BoldObjectMarshaler,
  BoldPropagatorServer,
  maan_PropagatorServer,
  windows;

const
  NO_CLIENTS = 120;

{ TMaan_TestCaseClientHandler }

procedure TMaan_TestCaseClientHandler.ExtendLease;
var
  Extended: WordBool;
  ClientIdString: string;
  LeaseTimeOut: TTimeStamp;
  LeaseDuration: integer;
  PollingInterval: integer;
  Initialized :Boolean;
  res: Boolean;
begin
  ClientHandler.ExtendLease(ClientID1, 10, Extended);
  res := ClientHandler.HasInfoForClient(ClientId1, ClientIdString, LeaseDuration, PollingInterval,
        LeaseTimeOut, Initialized);
  Assert(res and (LeaseDuration = 10 ) and (ClientIdString = fClientIdentifierString),
               'Error in ExtendLease');
end;

procedure TMaan_TestCaseClientHandler.RegisterClient;
var
  ClientIdString: string;
  LeaseTimeOut: TTimeStamp;
  LeaseDuration: integer;
  PollingInterval: integer;
  Initialized :Boolean;
  res : Boolean;
  i: integer;
begin
  res := ClientHandler.HasInfoForClient(ClientId1, ClientIdString, LeaseDuration, PollingInterval,
        LeaseTimeOut, Initialized);
  for i:= 0 to NO_CLIENTS do
    Assert(Integer(IDs[i]) = i, Format('Incorrect ClientID %d', [i]));
  Assert(res and (LeaseDuration = fLeaseDuration) and (ClientIDString = fClientIdentifierString) and
         (PollingInterval = fPollingInterval), 'Error in RegisterClient');
end;

procedure TMaan_TestCaseClientHandler.SetUp;
var
  i: integer;
begin
  inherited;
  if not Assigned(IDs) then
    IDs := TList.Create;
  for i:= 0 to NO_CLIENTS do
  begin
    if not Assigned(fClientHandler) then
      fClientHandler := TTestableClientHandler.Create(fClientHandler);
    if not Assigned(ListenerIntf) then
      ListenerIntf := TTestBoldListener.Create;
    fClientidentifierString := Format('TestClient%d', [i]);
    fLeaseDuration := 5;
    fPollingInterval := 100000;
    ClientHandler.RegisterClient(fLeaseDuration, fPollingInterval, ListenerIntf, fClientIdentifierString, ID, fRegistrationTime);
    IDs.Add(Pointer(ID));
    ClientID1 := ID;
    ClientidentifierString1 := fClientIdentifierString;
  end;
end;

class procedure TMaan_TestCaseClientHandler.Suit(ASuite: TBoldTestSuite);
begin
  inherited;
  ASuite.AddTest(CreateWithComment('RegisterClient', ''));
  ASuite.AddTest(CreateWithComment('ExtendLease', ''));
  ASuite.AddTest(CreateWithComment('UnRegisterClient', ''));
end;

class function TMaan_TestCaseClientHandler.Suite: ITestSuite;
begin
  result := inherited Suite;
  SetCommentForTest(result, 'RegisterClient', '');
  SetCommentForTest(result, 'ExtendLease', '');
  SetCommentForTest(result, 'UnRegisterClient', '');
end;

procedure TMaan_TestCaseClientHandler.TearDown;
begin
  FreeAndNil(fClientHandler);
  FreeAndNil(IDs);
  TPropagatorServerTest.FreeSingleton;  
  inherited;
end;

procedure TMaan_TestCaseClientHandler.UnRegisterClient;
var
  Id: integer;
  ClientIdString: string;
  RegistrationTime: TTimeStamp;
  LeaseTimeOut: TTimeStamp;
  LeaseDuration: integer;
  PollingInterval: integer;
  Initialized :Boolean;
  Status: TBoldClientReceiveStatus;
begin
  ClientID1 := 0;
  ClientHandler.HasInfoForClient(ClientId1, ClientIdentifierString1, RegistrationTime, Initialized, Status);
  ClientHandler.UnRegisterClient(ClientID1, RegistrationTime);
  ClientHandler.HasInfoForClient(ClientId1, ClientIdentifierString1, LeaseDuration, PollingInterval,
        LeaseTimeOut, Initialized);
  Assert(not Initialized, 'Error UnRegisterClient');
  ListenerIntf := TTestBoldListener.Create;
  ClientHandler.RegisterClient(55, 55, ListenerIntf,'Test Project',ID, fRegistrationTime);
  ClientHandler.HasInfoForClient(ClientId1, ClientIdString, LeaseDuration, PollingInterval,
        LeaseTimeOut, Initialized);
  Assert(not Initialized and (ClientIdString = 'TestClient0'),
               'Error in RegisterClient');
  Assert(ID <> ClientId1, Format('%s.Error UnregisterClient: error TBoldClientHandler.GetFreeClientIds', [ClassName]));
end;

{ TTestBoldListener }

constructor TTestBoldListener.Create;
begin
  inherited;
  fEvents := TStringList.Create;
end;

destructor TTestBoldListener.Destroy;
begin
  FreeAndNil(fEvents);
  inherited;
end;

function TTestBoldListener.getEvents: TStringList;
begin
  if not Assigned(fEvents) then
    fEvents := TStringList.Create;
  Result := fEvents;
end;

function TTestBoldListener.ReceiveEvents(Events: OleVariant): integer;
var
  i: integer;
begin
  for i := VarArrayLowBound(Events, 1) to VarArrayHighBound(Events, 1) do
    fEvents.Add(Events[i]);
end;

{ TTestableClientHandler }

procedure TTestableClientHandler.EnqueueRemoveClientQueueEvent(const ClientId: TBoldClientId);
begin
  // do nothing
end;

initialization
  TestGlobal.RegisterTestCase(TMaan_TestCaseClientHandler);

finalization
  TestGlobal.UnRegisterTestCase(TMaan_TestCaseClientHandler);

end.
