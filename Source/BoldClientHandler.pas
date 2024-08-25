
{ Global compiler directives }
{$include bold.inc}
unit BoldClientHandler;

interface

uses
  BoldSubscription,
  BoldDefs,
  BoldContainers,
  BoldPropagatorInterfaces_TLB,
  SysUtils,
  BoldGuard,
  BoldUtils,
  BoldThreadSafeLog,
  BoldLoggableCriticalSection,
  syncobjs,
  Classes,
  activex,
  windows;

type
  {forward declarations}
  TBoldClientHandler = class;
  TBoldClientInfoList = class;
  TBoldClientInfo = class;

  TBoldRemoveClientReasonType = (brcrUnregister, brcrConnectionLost, brcrLeaseExpired, brcrAdministrator);
  TBoldClientReceiveStatus = (crsOK, crsNotReceiving, crsRecovered);
  {TBoldClientHandler}
  TBoldClientHandler = class(TBoldPublisher)
  private
    fClientInfoList: TBoldClientInfoList; //ClientInfoList sorted by ClientID
    fNOConnectedClients: integer;
    fEnabled: Boolean;
    fLockTime: TDateTime;
    FClientHandlerLock: TBoldLoggableCriticalSection;
    fLockTimeLock: TCriticalSection;
    fTotalLostEvents: integer;
    fTotalClients: integer;
    fPeakClients: integer;
    function InternalRemoveClient(BoldClientId: TBoldClientID): Boolean;
    procedure AddClient(const LeaseDuration: Integer; PollingInterval: Integer;
                             const BoldClientListener: IBoldListener;
                             const ClientIDString: WideString; out ClientId: integer);
    procedure SetEnabled(Value: Boolean);
    procedure SetNOConnectedClients(const Value: integer);
    function InfoForClient(ClientID: TBoldClientID; out BoldClientInfo: TBoldClientInfo): Boolean;
    procedure NotifyLeaseChanged;
    function GetFirstLeaseTimeOutClient: TBoldClientInfo;
    function GetEnabled: Boolean;
    procedure AcquireLock;
    procedure ReleaseLock;
  protected
    procedure EnqueueRemoveClientQueueEvent(const ClientId: TBoldClientId); virtual; // virtuality is for testcases...
  public
    constructor Create(var APublisherReference);
    destructor Destroy; override;    {IBoldClientHandler}
    function RegisterClient(LeaseDuration: Integer; PollingInterval: Integer;
                             const BoldClientListener: IBoldListener;
                             const ClientIDString: WideString; out BoldClientID: Integer; out RegistrationTime: TTimeStamp): HResult; stdcall;
    function ExtendLease(BoldClientID: Integer; LeaseDuration: Integer; out ExtensionOK: WordBool): HResult; stdcall;
    function UnRegisterClient(BoldClientID: Integer; RegistrationTime: TTimeStamp): HResult; stdcall;

    {thread safe methods}
    function HasInfoForClient(ClientId: TBoldClientId;
              out ClientIdentifierString: string; out RegistrationTime: TTimeStamp;
              out Initialized: Boolean; out Status: TBoldClientReceiveStatus): Boolean; overload;
    function HasInfoForClient(ClientId: TBoldClientId;
              out ClientIdentifierString: string; out LeaseDuration, PollingInterval: integer;
              out LeaseTimeOut: TTimeStamp; out Initialized: Boolean): Boolean; overload;
    function GetRegistrationTime(const ClientID: TBoldClientId; out RegistrationTime: TTimeStamp): Boolean;
    function IsRegistered(const ClientId: TBoldClientId): Boolean;
    function DisconnectClient(const BoldClientId: TBoldClientId; const RegistrationTime: TTimeStamp): Boolean;
    procedure SendDisconnectRequest(const BoldClientId: TBoldClientId; const msg: string; RemainDisconnected: Integer);

    function IsThereAClientTimingOutSoon(out ClientId: TBoldClientId; out RegistrationTime,
              LeaseTimeOut: TTimeStamp): Boolean;
    procedure RemoveExpiredLease(const ClientID: TBoldClientID; const RegistrationTime: TTimeStamp);
    procedure MarkHasReceivedEvents(const ClientID: TBoldClientID; const EventCount: integer);
    procedure MarkFailedToReceiveEvents(const ClientId: TBoldClientID; const EventCount: integer);
    procedure GetRegisteredClientIDs(ClientIds: TStringList);
    procedure GetRegisteredClientInfos(ClientInfo: TStringList);
    {$IFDEF DEBUG}
    procedure DebugLock;
    procedure DebugUnlock;
    function ValidateClientInterface(const ListenerInterface: IBoldListener; const ClientIDString: string): Boolean;
    {$ENDIF}
    function GetListener(const ClientId: TBoldClientId; const RegistrationTime: TTimeStamp; out obj): Boolean;
    function LockTime: TDateTime;
    property NOConnectedClients: integer read fNOConnectedClients write SetNOConnectedClients;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  {TBoldClientInfo}
  TBoldClientInfo = class
  private
    fClientID: TBoldClientID;
    fClientIdentifierString: string;
    fLeaseDuration: Integer;
    fPollingInterval: Integer;
    fLeaseTimeOut: TTimeStamp;
    fListenerCookie: DWORD;
    fRegistrationTime: TTimeStamp;
    fBoldListener: IBoldListener;
    fSuccessfullyReceivedEventsCount: integer;
    fSuccessfullyReceivedCount: integer;
    fLastSuccessfullReceive: TDateTime;
    fLongestIntervalBetweenEvents: TDateTime;
    fLostEvents: integer;
    fClientStatus: TBoldClientReceiveStatus;

    function LeaseIsExpired: Boolean;
    procedure ReInitializeListenerInterface;
    function GetClientStatusString: string;
  public
    fInitialized: Boolean;
    constructor Create(ClientID: TBoldClientID);
    destructor Destroy; override;
    procedure Initialize(LeaseTime: Cardinal; PollingInterval: Cardinal;
                         ClientCallBack: IBoldListener; const ClientIDString: WideString);
    procedure UnInitialize;
    procedure ExtendLease(LeaseDuration: integer);
    property ClientID: TBoldClientID read fClientID;
    property LeaseDuration: Integer read fLeaseDuration;
    property PollingInterval: Integer read fPollingInterval;
    property LeaseTimeOut: TTimeStamp read fLeaseTimeOut;
    property ClientIdentifierString: string read fClientIdentifierString;
    property Initialized: Boolean read fInitialized;
    property RegistrationTime: TTimeStamp read fRegistrationTime;
    property ClientStatusString: string read GetClientStatusString;
  end;

  {TBoldClientInfoList}
  TBoldClientInfoList = class
  private
    fList: TBoldObjectArray;
    fFreeClientIDs: TList;
    function GetItem(Index: Integer): TBoldClientInfo;
    procedure SetItem(Index: Integer; AObject: TBoldClientInfo);
    function getCount: integer;
    procedure EnsureListCapacity(const Index: integer);
  protected
    function GetFreeClientID: TBoldClientID;
    procedure ReturnFreeClientID(const ClientID: TBoldClientID);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AObject: TBoldClientInfo): Integer;
    procedure Clear;
    function Remove(AObject: TBoldClientInfo): Integer;
    function IndexOf(AObject: TBoldClientInfo): Integer;
    procedure Insert(Index: Integer; AObject: TBoldClientInfo);
    function Last: TBoldClientInfo;
    procedure Delete(Index: Integer);
    function GetExistingClientInfo(Index: integer): TBoldClientInfo;
    property Items[Index: Integer]: TBoldClientInfo read GetItem write SetItem; default;
    property Count: Integer read getCount;
  end;

implementation

uses
  Comobj,

  BoldCoreConsts,
  BoldPropagatorConstants,
  BoldPropagatorServer;

type
  {00000323-0000-0000-C000-000000000046}
  {$EXTERNALSYM IGlobalInterfaceTable}
  IGlobalInterfaceTable = interface(IUnknown)
    ['{00000146-0000-0000-C000-000000000046}']
    function RegisterInterfaceInGlobal(const pUnk: IUnknown; const riid: TIID;
                                       out dwCookie: DWORD): HResult; stdcall;
    function RevokeInterfaceFromGlobal(dwCookie: DWORD): HResult; stdcall;
    function GetInterfaceFromGlobal(dwCookie: DWORD; const riid: TIID;
                                    out ppv): HResult; stdcall;
  end;

var
  G_GlobalInterfaceTable: IGlobalInterfaceTable = nil;
  CLSID_StdGlobalInterfaceTable: TGUID = '{00000323-0000-0000-C000-000000000046}';

function GlobalInterfaceTable: IGlobalInterfaceTable;
begin
  if G_GlobalInterfaceTable = nil then
    CoCreateInstance(
    CLSID_StdGlobalInterfaceTable,
    nil, CLSCTX_INPROC_SERVER,
    IGlobalInterfaceTable,
    G_GlobalInterfaceTable);
  Result := G_GlobalInterfaceTable;
end;

{TBoldClientHandler}
constructor TBoldClientHandler.Create(var APublisherReference);
begin
  inherited Create(APublisherReference);
  fClientInfoList := TBoldClientInfoList.Create;
  fClientHandlerLock := TBoldLoggableCriticalSection.Create('CH');
  fLockTimeLock := TCriticalSection.Create;
  fNOConnectedClients := 0;
  fLockTime := 0;
  AcquireLock;
  try
    fEnabled := true;
  finally
    ReleaseLock;
  end;
end;

destructor TBoldClientHandler.Destroy;
begin
  try
    NotifySubscribersAndClearSubscriptions(self);
    AcquireLock;
    try
      fEnabled := false;
      FreeAndNil(fClientInfoList);
    finally
      ReleaseLock;
    end;
    FreeAndNil(fClientHandlerLock);
    FreeAndNil(fLockTimeLock);
  except on E: Exception do
    BoldLogError(sLogError, [ClassName, 'Destroy', E.Message]); // do not localize
  end;
  inherited;
end;

function TBoldClientHandler.InfoForClient(ClientID: TBoldClientID; out BoldClientInfo: TBoldClientInfo): Boolean;
begin
  Result := Enabled;
  try
    if Result then
    begin
      BoldClientInfo := fClientInfoList.GetExistingClientInfo(ClientID);
      Result := Assigned(BoldClientInfo);
    end;
  except on E: Exception do
    BoldLogError(sLogErrorAndID, [ClassName, 'InfoForClient', ClientId, E.message]); // do not localize
  end;
end;

function TBoldClientHandler.RegisterClient(LeaseDuration: Integer; PollingInterval: Integer;
                             const BoldClientListener: IBoldListener;
                             const ClientIDString: WideString; out BoldClientID: Integer; out RegistrationTime: TTimeStamp): HResult; stdcall;
begin
  Result := S_FALSE;
  if not Enabled then
    Exit;

  AcquireLock;
  try
    try
      AddClient(LeaseDuration, PollingInterval, BoldClientListener, ClientIDString, BoldClientID);
      RegistrationTime := fClientInfoList[BoldClientId].RegistrationTime;
      SendExtendedEvent(self, BOLD_PROPAGATOR_CLIENT_REGISTERED, [BoldClientID]);
      NotifyLeaseChanged;
      BoldLog(sLoggingID, [ClientIdString, BoldClientId]);
      inc(fTotalClients);
      if NoConnectedClients > fPeakClients then
        fPeakClients := NoConnectedClients;
      Result := S_OK;
    except on E: Exception do
      BoldLogError(sLogErrorAndID, [ClassName, 'RegisterClient', BOldClientID, E.Message]); // do not localize
    end;
  finally
    ReleaseLock;
  end;
end;

function TBoldClientHandler.ExtendLease(BoldClientID: Integer; LeaseDuration: Integer; out ExtensionOK: WordBool): HResult; stdcall;
var
  ClientInfo: TBoldClientInfo;
begin
  Result := S_FALSE;
  ExtensionOK := false;
  if not Enabled then Exit;
  try
    AcquireLock;
    try
      if InfoForClient(BoldClientID, ClientInfo) and (ClientInfo.Initialized) then
      begin
        ClientInfo.ReInitializeListenerInterface;
        ClientInfo.ExtendLease(LeaseDuration);
        ExtensionOK := true;
        Result := S_OK;
        NotifyLeaseChanged;
      end
      else
        BoldLog(sExtendLeaseFailed, [BoldClientId]);
    finally
      ReleaseLock;
    end;
  except on E: Exception do
    BoldLogError(sLogErrorAndID, [ClassName, 'ExtendLease', BoldClientId, E.Message]); // do not localize
  end;
end;

function TBoldClientHandler.UnRegisterClient(BoldClientID: Integer; RegistrationTime: TTimeStamp): HResult; stdcall;
var
  ClientInfo: TBoldClientInfo;
  IdString: string;
begin
  Result := S_FALSE;
  if not Enabled then Exit;
  try
    AcquireLock;
    try
      if InfoForClient(BoldClientId, ClientInfo) and (ClientInfo.Initialized) and
         (TimeStampComp(ClientInfo.RegistrationTime, RegistrationTime) = 0) then
      begin
        IdString := ClientInfo.ClientIdentifierString;
        if InternalRemoveClient(BoldClientID) then
        begin
          Result := S_OK;
          // notify all subscribers --- cleanup
          SendExtendedEvent(self, BOLD_PROPAGATOR_CLIENT_UNREGISTERED, [BoldClientID]);
          NotifyLeaseChanged;
          BoldLog(sLogOff,
              [IdString, BoldClientId,
              ClientInfo.fSuccessfullyReceivedCount,
              ClientInfo.fSuccessfullyReceivedEventsCount,
              TimetoStr(ClientInfo.fLongestIntervalBetweenEvents),
              DateTimeToStr(TimeStampToDateTime(ClientInfo.RegistrationTime)),
              TimeToStr(now - TimeStampToDateTime(ClientInfo.RegistrationTime)),
              ClientInfo.ClientStatusString]);
        end;
      end;
    finally
      ReleaseLock;
    end;
  except on E: Exception do
    BoldLogError(sLogErrorAndID, [ClassName, 'UnRegisterClient', BoldClientId, E.Message]); // do not localize
  end;
end;

function TBoldClientHandler.InternalRemoveClient(BoldClientId: TBoldClientID): Boolean;
var
  ClientInfo: TBoldClientInfo;
begin
  Result := false;
  if InfoForClient(BoldClientID, ClientInfo) and (ClientInfo.Initialized) then
  begin
    Result := true;
    ClientInfo.UnInitialize;
    fClientInfoList.ReturnFreeClientId(BoldClientId);
    NOConnectedClients := NOConnectedClients - 1;
    SendExtendedEvent(self, BOLD_PROPAGATOR_CLIENT_REMOVED, [BoldClientID]);
    EnqueueRemoveClientQueueEvent(BoldClientId);
  end;
end;

procedure TBoldClientHandler.AddClient(const LeaseDuration: Integer; PollingInterval: Integer;
  const BoldClientListener: IBoldListener; const ClientIDString: WideString; out ClientID: integer);
var
  NewClientInfo: TBoldClientInfo;
  NewClientID: TBoldClientID;
begin
  try
    NewClientID := fClientInfoList.GetFreeClientID;
    NewClientInfo := fClientInfoList[NewClientID];
    if Assigned(NewClientInfo) then
      NewClientInfo.Initialize(LeaseDuration, PollingInterval, BoldClientListener, ClientIDString)
    else
    begin
      fClientInfoList[NewClientID] := TBoldClientInfo.Create(NewClientID);
      fClientInfoList[NewClientID].Initialize(LeaseDuration, PollingInterval, BoldClientListener, ClientIDString);
    end;
    ClientId := NewClientID;
    NOConnectedClients := NOConnectedClients + 1;
  except on E: Exception do
    BoldLogError(sLogError, [ClassName, 'AddClient', E.Message]); // do not localize
  end;
end;

procedure TBoldClientHandler.RemoveExpiredLease(
  const ClientID: TBoldClientID; const RegistrationTime: TTimeStamp);
var
  ClientInfo: TBoldClientInfo;
  IdString: string;
begin
  if not Enabled then Exit;
  AcquireLock;
  try
    InfoForClient(ClientId, ClientInfo);
    if Assigned(ClientInfo) and
      (ClientInfo.Initialized) and
      (TimeStampComp(ClientInfo.RegistrationTime, RegistrationTime) = 0) and
      (ClientInfo.LeaseIsExpired) then
    begin
      IdString := ClientInfo.ClientIdentifierString;
      InternalRemoveClient(ClientID);
      SendExtendedEvent(self, BOLD_PROPAGATOR_CLIENT_LEASE_EXPIRED, [ClientID]);
      BoldLog(sLeaseExpired,
              [IdString, ClientID,
              ClientInfo.fSuccessfullyReceivedCount,
              ClientInfo.fSuccessfullyReceivedEventsCount,
              timetoStr(ClientInfo.fLongestIntervalBetweenEvents),
              TimeToStr(now - ClientInfo.fLastSuccessfullReceive),
              DateTimeToStr(TimeStampToDateTime(ClientInfo.RegistrationTime)),
              TimeToStr(now - TimeStampToDateTime(ClientInfo.RegistrationTime)),
              ClientInfo.ClientStatusString]);
      NotifyLeaseChanged;
    end;
  finally
    ReleaseLock;
  end;
end;

procedure TBoldClientHandler.GetRegisteredClientIDs(ClientIds: TStringList);
var
  i: integer;
begin
  AcquireLock;
  try
    if Assigned(ClientIds) then
      for i:= 0 to fClientInfoList.Count - 1 do
        if Assigned(fClientInfoList[i]) and fClientInfoList[i].Initialized then
          ClientIds.Add(Format('%d=%s', [fClientInfoList[i].ClientId, fClientInfoList[i].ClientIdentifierString]));
  finally
    ReleaseLock;
  end;
end;

function TBoldClientHandler.DisconnectClient(
  const BoldClientId: TBoldClientId;
  const RegistrationTime: TTimeStamp): Boolean;
var
  ClientInfo: TBoldClientInfo;
  IdString: string;
begin
  Result := false;
  if not Enabled then Exit;
  try
    AcquireLock;
    try
      if InfoForClient(BoldClientId, ClientInfo) and
         (ClientInfo.Initialized) and
         (TimeStampComp(ClientInfo.RegistrationTime, RegistrationTime) = 0) then
      begin
        IdString := ClientInfo.ClientIdentifierString;
        if InternalRemoveClient(BoldClientID) then
        begin
          Result := true;
          // notify all subscribers --- cleanup
          SendExtendedEvent(self, BOLD_PROPAGATOR_CLIENT_CONNECTION_LOST, [BoldClientID]);
          NotifyLeaseChanged;
          if ClientInfo.fSuccessfullyReceivedCount = 0 then
            BoldLog(sClientDisconnected, [
              IdString, BoldClientId,
              DateTimeToStr(TimeStampToDateTime(ClientInfo.RegistrationTime)),
              TimeToStr(now - TimeStampToDateTime(ClientInfo.RegistrationTime)),
              ClientInfo.ClientStatusString
            ])
          else
            BoldLog(sClientDisconnected_Long,
              [IdString, BoldClientId,
              ClientInfo.fSuccessfullyReceivedCount,
              ClientInfo.fSuccessfullyReceivedEventsCount,
              timetoStr(ClientInfo.fLongestIntervalBetweenEvents),
              TimeToStr(now - ClientInfo.fLastSuccessfullReceive),
              DateTimeToStr(TimeStampToDateTime(ClientInfo.RegistrationTime)),
              TimeToStr(now - TimeStampToDateTime(ClientInfo.RegistrationTime)),
              ClientInfo.ClientStatusString])
        end;
      end;
    finally
      ReleaseLock;
    end;
  except on E: Exception do
    BoldLogError(sLogErrorAndID, [ClassName, 'DisconnectClient', BoldClientId, E.Message]); // do not localize
  end;
end;

function TBoldClientHandler.GetRegistrationTime(const ClientID: TBoldClientId;
  out RegistrationTime: TTimeStamp): Boolean;
var
  ClientInfo: TBoldClientInfo;
begin
  Result := False;
  try
    AcquireLock;
    try
      Result := InfoForClient(ClientID, ClientInfo) and (ClientInfo.Initialized);
      if Result then
        RegistrationTime := ClientInfo.RegistrationTime;
    finally
      ReleaseLock;
    end;
  except on E: Exception do
    BoldLogError(sLogErrorAndID, [ClassName, 'GetRegistrationTime', ClientID, E.Message]); // do not localize
  end;
end;

{$IFDEF DEBUG}
procedure TBoldClientHandler.DebugLock;
begin
  AcquireLock;
end;

procedure TBoldClientHandler.DebugUnlock;
begin
  ReleaseLock;
end;
{$ENDIF}


procedure TBoldClientHandler.GetRegisteredClientInfos(
  ClientInfo: TStringList);
var
  i: integer;
  ClientInfoItem: TBoldClientInfo;
  Guard: IBoldGuard;
  temp: TStringList;
begin
  if not Enabled then Exit;
  AcquireLock;
  try
    Guard := TBoldGuard.Create(Temp);
    Temp := TStringList.Create;
    Temp.Add('TotalClients='+IntToStr(fTotalClients)); // do not localize
    Temp.Add('PeakClients='+IntToStr(fPeakClients)); // do not localize
    Temp.Add('TotalLostEvents='+IntToStr(fTotalLostEvents)); // do not localize
    ClientInfo.Add(temp.CommaText);
    if Assigned(ClientInfo) then
      for i:= 0 to fClientInfoList.Count - 1 do
        if Assigned(fClientInfoList[i]) and fClientInfoList[i].Initialized then
        begin
          ClientInfoItem := fClientInfoList[i];
          temp.Clear;
          Temp.Add('ID='                +IntToStr(ClientInfoItem.ClientId)); // do not localize
          Temp.Add('IDString='          +ClientInfoItem.ClientIdentifierString); // do not localize
          Temp.Add('RegistrationTime='  +FormatDateTime(BoldDateTimeFormat, TimeStampToDateTime(ClientInfoItem.RegistrationTime))); // do not localize
          Temp.Add('LeaseTimeout='      +FormatDateTime(BoldDateTimeFormat, TimeStampToDateTime(ClientInfoItem.LeaseTimeOut))); // do not localize
          temp.add('Packages='          +intToStr(ClientInfoItem.fSuccessfullyReceivedCount)); // do not localize
          temp.add('Events='            +intToStr(ClientInfoItem.fSuccessfullyReceivedEventsCount)); // do not localize
          temp.add('LastSend='          +FormatDateTime(BoldDateTimeFormat, ClientInfoItem.fLastSuccessfullReceive)); // do not localize
          temp.add('LongestInterval='   +FormatDateTime(BoldDateTimeFormat, ClientInfoItem.fLongestIntervalBetweenEvents)); // do not localize
          temp.add('LostEvents='      +IntToStr(ClientInfoItem.fLostEvents)); // do not localize
          temp.Add('Status='            +ClientInfoItem.ClientStatusString); // do not localize
          ClientInfo.Add(Temp.CommaText);
        end;
  finally
    ReleaseLock;
  end;
end;

procedure TBoldClientHandler.SetEnabled(Value: Boolean);
begin
  AcquireLock;
  try
    if (Value <> fEnabled) then
    begin
      fEnabled := Value;
      if not fEnabled and Assigned(fClientInfoList) then
      begin
        fClientInfoList.Clear;
      end;
    end;
  finally
    ReleaseLock;
  end;
end;

function TBoldClientHandler.IsThereAClientTimingOutSoon(
  out ClientId: TBoldClientId; out RegistrationTime,
  LeaseTimeOut: TTimeStamp): Boolean;
var
  i: integer;
  CurrentClientInfo, aTemp: TBoldClientInfo;
begin
  result := false;
  CurrentClientInfo := nil;
  if not Enabled then Exit;
  try
    AcquireLock;
    try
      if (NoConnectedClients <> 0) then
      begin
        for i:= 0 to fClientInfoList.Count - 1 do
        begin
          aTemp := fClientInfoList[i];
          if Assigned(aTemp) and aTemp.Initialized then
          begin
            if not Assigned(CurrentClientInfo) then
              CurrentClientInfo := aTemp;
            if TimeStampComp(CurrentClientInfo.LeaseTimeOut, aTemp.LeaseTimeOut) = 1 then
              CurrentClientInfo := aTemp;
          end;
        end;
        ClientId := CurrentClientInfo.ClientID;
        RegistrationTime := CurrentClientInfo.RegistrationTime;
        LeaseTimeOut := CurrentClientInfo.LeaseTimeOut;
      end;
    finally
      ReleaseLock;
    end;
  except on E: Exception do
    BoldLogError(sLogError, [ClassName, 'IsThereAClientTimingOutSoon', E.Message]); // do not localize
  end;
end;

procedure TBoldClientHandler.SetNOConnectedClients(const Value: integer);
begin
  AcquireLock;
  try
    if (fNoConnectedClients <> Value) then
      fNoConnectedClients := Value;
  finally
    ReleaseLock;
  end;
end;

function TBoldClientHandler.HasInfoForClient(ClientId: TBoldClientId;
  out ClientIdentifierString: string; out LeaseDuration,
  PollingInterval: integer; out LeaseTimeOut: TTimeStamp;
  out Initialized: Boolean): Boolean;
var
 ClientInfo: TBoldClientInfo;
begin
  Result := false;
  if not Enabled then Exit;
  AcquireLock;
  try
    Result := InfoForClient(ClientId, ClientInfo);
    if Result then
    begin
      ClientIdentifierString := ClientInfo.ClientIdentifierString;
      LeaseDuration := ClientInfo.LeaseDuration;
      PollingInterval := ClientInfo.PollingInterval;
      LeaseTimeout := ClientInfo.LeaseTimeOut;
      Initialized := ClientInfo.Initialized;
    end;
  finally
    ReleaseLock;
  end;
end;

function TBoldClientHandler.HasInfoForClient(ClientId: TBoldClientId;
  out ClientIdentifierString: string; out RegistrationTime: TTimeStamp; out Initialized: Boolean; out Status: TBoldClientReceiveStatus): Boolean;
var
 ClientInfo: TBoldClientInfo;
begin
  Result := false;
  if not Enabled then Exit;
  AcquireLock;
  try
    Result := InfoForClient(ClientId, ClientInfo);
    if Result then
    begin
      ClientIdentifierString := ClientInfo.ClientIdentifierString;
      RegistrationTime := ClientInfo.RegistrationTime;
      Initialized := ClientInfo.Initialized;
      Status := ClientInfo.fClientStatus;
    end;
  finally
    ReleaseLock;
  end;
end;

(*
Is this method uses from anywhere?

function TBoldClientHandler.ExternalRemoveClient(
  const ClientId: TBoldClientId; const RegistrationTime: TTimeStamp; const Reason: TBoldRemoveClientReasonType): Boolean;
  function RemoveClientReasonTypeToString(aReason: TBoldRemoveClientReasonType): string;
  begin
    case aReason of
      brcrUnregister: Result := 'Logged Off';
      brcrConnectionLost: Result := 'Lost Connection';
      brcrLeaseExpired: Result := 'Lease Expired';
      brcrAdministrator: Result := 'Administrator Removed';
    end;
  end;

var
  CanRemove: Boolean;
  ClientInfo: TBoldClientInfo;
begin
  Result := false;
  AcquireLock;
  try
    try
      CanRemove := InfoForClient(ClientId, ClientInfo) and (ClientInfo.Initialized) and
         (TimeStampComp(ClientInfo.RegistrationTime, RegistrationTime) = 0) and
         not ((Reason = brcrLeaseExpired) and not (ClientInfo.LeaseIsExpired));
      if InternalRemoveClient(ClientID) then
      begin
        Result := true;
        NotifyLeaseChanged;
        BoldLog('%s: %s [ID=%d]', [RemoveClientReasonTypeToString(Reason), ClientInfo.ClientIdentifierString, ClientId]);
      end;
    except on E: Exception do
      BoldLogError('%s.UnRegisterClient Error: [ID=%d] %s', [ClassName, ClientID, E.Message]);
    end;
    if Result then
      case Reason of
        brcrUnregister: begin
          SendExtendedEvent(self, BOLD_PROPAGATOR_CLIENT_UNREGISTERED, [ClientID]);
        end;
        brcrConnectionLost: begin
          SendExtendedEvent(self, BOLD_PROPAGATOR_CLIENT_CONNECTION_LOST, [ClientID]);
        end;
        brcrLeaseExpired: begin
        // notify all subscribers --- cleanup
          SendExtendedEvent(self, BOLD_PROPAGATOR_CLIENT_LEASE_EXPIRED, [ClientID]);
        end;
        brcrAdministrator: begin
        // notify all subscribers --- cleanup
//          SendExtendedEvent(self, BOLD_PROPAGATOR_CLIENT_USER_DISCONNECT, [ClientID]);
        end;
      end;//case
  finally
    ReleaseLock;
  end;
end;
*)

procedure TBoldClientHandler.NotifyLeaseChanged;
var
  TimeOutClient: TBoldClientInfo;
  RegistrationTime, LeaseTimeOut: extended;
begin
  TimeOutClient := GetFirstLeaseTimeOutClient;
  if Assigned(TimeOutClient) then
  begin
    RegistrationTime := TimeStampToMSecs(TimeOutClient.RegistrationTime);
    LeaseTimeOut := TimeStampToMSecs(TimeOutClient.LeaseTimeOut);
    SendExtendedEvent(self, BOLD_PROPAGATOR_CLIENT_LEASE_CHANGED,
      [TimeOutClient.ClientID, RegistrationTime, LeaseTimeOut]);
  end;
end;

function TBoldClientHandler.GetFirstLeaseTimeOutClient: TBoldClientInfo;
var
  i: integer;
  aTemp: TBoldClientInfo;
begin
  Result := nil;
  if not Enabled then Exit;
  try
    if (NoConnectedClients <> 0) then
      for i:= 0 to fClientInfoList.Count - 1 do
      begin
        aTemp := fClientInfoList[i];
        if Assigned(aTemp) and aTemp.Initialized then
        begin
          if not Assigned(Result) then
            Result := aTemp;
          if TimeStampComp(Result.LeaseTimeOut, aTemp.LeaseTimeOut) = 1 then
            Result := aTemp;
        end;
      end;
  except on E: Exception do
    BoldLogError(sLogError, [ClassName, 'GetFirstLeaseTimeOutClient', E.Message]); // do not localize
  end;
end;

function TBoldClientHandler.IsRegistered(const ClientId: TBoldClientId): Boolean;
begin
  Result := false;
  if not Enabled then Exit;
  AcquireLock;
  try
   Result := (ClientId >= 0) and (ClientId < fClientInfoList.fList.Count);
   if Result then
   begin
     if Assigned(fClientInfoList.fList[ClientId]) then
       Result := (fClientInfoList.fList[ClientId] as TBoldClientInfo).Initialized
     else
       Result := false;
   end;
  finally
    ReleaseLock;
  end;
end;

procedure TBoldClientHandler.EnqueueRemoveClientQueueEvent(const ClientId: TBoldClientId);
begin
  AcquireLock;
  try
    if fEnabled then
      TBoldPropagatorServer.Instance.AdvancedPropagator.Dequeuer.EnqueueRemoveClientQueueEvent(ClientID);
  finally
    ReleaseLock;
  end;
end;

function TBoldClientHandler.GetListener(const ClientId: TBoldClientId;
  const RegistrationTime: TTimeStamp; out obj): Boolean;
var
  ClientInfo: TBoldClientInfo;
begin
  Result := false;
  if not Enabled then Exit;
  AcquireLock;
  try
    Result := InfoForClient(ClientId, ClientInfo) and (TimeStampComp(ClientInfo.RegistrationTime, RegistrationTime) = 0) and
      ClientInfo.Initialized;
    if Result then
      GlobalInterfaceTable.GetInterfaceFromGlobal(ClientInfo.fListenerCookie,IID_IBoldListener, obj);
  finally
    ReleaseLock;
  end;
end;

function TBoldClientHandler.GetEnabled: Boolean;
begin
  AcquireLock;
  try
    Result := fEnabled;
  finally
    ReleaseLock;
  end;
end;

(* Calling this method at the wrong time can cause a deadlock!
  Use only for debugging purposes. *)
{$IFDEF DEBUG}
function TBoldClientHandler.ValidateClientInterface(
  const ListenerInterface: IBoldListener;
  const ClientIDString: string): Boolean;
var
  Events: TStringList;
  ErrorMsg: string;
begin
  Result := Assigned(ListenerInterface);
  if Result then
  begin
    Events := TStringList.Create;
    Events.Add('L'); {TODO: change this to a ping message}
    try
      OleCheck(ListenerInterface.ReceiveEvents(StringListToVarArray(Events)));
    except on E: Exception do
      begin
        Result := false;
        ErrorMsg := Format(sInvalidListener, [ClassName, ClientIdString, E.Message]);
        if E is EOleSysError then
          ErrorMsg := ErrorMsg + Format(sErrorCode, [(E as EOleSysError).ErrorCode]);
        BoldLogError(ErrorMsg);
      end;
    end;
  end
  else
    BoldLogError(sListenerInterfaceMissing, [ClassName, ClientIdString]);
end;
{$ENDIF}

procedure TBoldClientHandler.MarkHasReceivedEvents(const ClientID: TBoldClientID; const EventCount: integer);
var
  ClientInfo: TBoldClientInfo;
begin
  if not Enabled then Exit;
  AcquireLock;
  try
    ClientInfo := fClientInfoList[ClientID];
    if Assigned(ClientInfo) then
    begin
      ClientInfo.fSuccessfullyReceivedEventsCount := ClientInfo.fSuccessfullyReceivedEventsCount + EventCount;
      ClientInfo.fSuccessfullyReceivedCount := ClientInfo.fSuccessfullyReceivedCount + 1;
      if now-ClientInfo.fLastSuccessfullReceive > ClientInfo.fLongestIntervalBetweenEvents then
        ClientInfo.fLongestIntervalBetweenEvents := now-ClientInfo.fLastSuccessfullReceive;
      fClientInfoList[ClientID].fLastSuccessfullReceive := now;
      if ClientInfo.fClientStatus = crsNotReceiving then
      begin
        ClientInfo.fClientStatus := crsRecovered;
        BoldLogError(sClientHasRecovered, [
          ClientInfo.ClientIdentifierString,
          ClientId, EventCount]);
      end;
    end;
  finally
    ReleaseLock;
  end;
end;

procedure TBoldClientHandler.MarkFailedToReceiveEvents(const ClientId: TBoldClientID; const EventCount: integer);
var
  ClientInfo: TBoldClientInfo;
begin
  if not Enabled then Exit;
  AcquireLock;
  try
    ClientInfo := fClientInfoList[ClientID];
    if Assigned(ClientInfo) then
    begin
      ClientInfo.fLostEvents := ClientInfo.fLostEvents + EventCount;
      ClientInfo.fClientStatus := crsNotReceiving;
      inc(fTotalLostEvents, EventCount); 
    end;
  finally
    ReleaseLock;
  end;
end;

procedure TBoldClientHandler.AcquireLock;
begin
  fClientHandlerLock.Acquire;
  fLockTimeLock.Acquire;
  try
    fLockTime := now;
  finally
    fLockTimeLock.Release;
  end;
end;

procedure TBoldClientHandler.ReleaseLock;
begin
  fLockTimeLock.Acquire;
  try
    fLockTime := 0;
  finally
    fLockTimeLock.Release;
  end;
  fClientHandlerLock.Release;
end;

function TBoldClientHandler.LockTime: TDateTime;
begin
  fLockTimeLock.Acquire;
  try
    if fLockTime = 0 then
      result := 0
    else
      result := now-fLockTime;
  finally
    fLockTimeLock.Release;
  end;
end;

procedure TBoldClientHandler.SendDisconnectRequest(const BoldClientId: TBoldClientId; const msg: string; RemainDisconnected: Integer);
var
  Listener: IBoldListener;
  ListenerAdmin: IBoldListenerAdmin;
  ClientInfo: TBoldClientInfo;
begin
  if not Enabled then Exit;
  try
    AcquireLock;
    try
      if InfoForClient(BoldClientId, ClientInfo) and
        GetListener(BoldClientId, ClientInfo.RegistrationTime, Listener) and
        (Listener.QueryInterface(IBoldListenerAdmin, ListenerAdmin) = S_OK) then
        ListenerAdmin.DisconnectClient(msg, remainDisconnected);
    finally
      ReleaseLock;
    end;
  except on E: Exception do
    BoldLogError(sLogErrorAndID, [ClassName, 'SendDisconnectRequest', BoldClientId, E.Message]); // do not localize
  end;
end;

{TBoldClientInfo}
constructor TBoldClientInfo.Create(ClientID: TBoldClientID);
begin
  inherited Create;
  fClientID := ClientID;
  fClientStatus := crsOK;
end;

destructor TBoldClientInfo.Destroy;
begin
  Uninitialize;
  inherited;
end;

procedure TBoldClientInfo.Initialize(LeaseTime, PollingInterval: Cardinal;
  ClientCallBack: IBoldListener; const ClientIDString: WideString);
begin
  try
    ExtendLease(LeaseTime);
    fPollingInterval := PollingInterval;
    fClientIdentifierString := ClientIdString;
    fBoldListener := ClientCallBack;
    fRegistrationTime := DateTimeToTimeStamp(Now);
    GlobalInterfaceTable.RegisterInterfaceInGlobal(fBoldListener, IID_IBoldListener, fListenerCookie);
    fSuccessfullyReceivedEventsCount := 0;
    fSuccessfullyReceivedCount := 0;
    fLastSuccessfullReceive := now;
    fLongestIntervalBetweenEvents := 0;
    fLostEvents := 0;
    fClientStatus := crsOK;
    fInitialized := true;
  except on E: Exception do
    BoldLogError(sLogErrorAndID, [ClassName, 'Initialize', ClientIdString, E.Message]); // do not localize
  end;
end;

procedure TBoldClientInfo.UnInitialize;
begin
  try
    if Initialized then
    begin
      fLeaseDuration := 0;
      fPollingInterval := 0;
      GlobalInterfaceTable.RevokeInterfaceFromGlobal(fListenerCookie);
      fBoldListener := nil;
      fInitialized := false;
    end
  except on E: Exception do
    BoldLogError(sLogError, [ClassName, 'UnInitialize', E.Message]); // do not localize
  end;
end;


function TBoldClientInfo.LeaseIsExpired: Boolean;
var
  CurrentTime: TTimeStamp;
begin
  Result := True;
  try
    CurrentTime := DateTimetoTimeStamp(Now);
    Result := TimeStampComp(LeaseTimeOut, CurrentTime) <= 0;
  except on E: Exception do
    BoldLogError(sLogErrorAndID, [ClassName, 'LeaseIsExpired', ClientID, E.Message]); // do not localize
  end;
end;

procedure TBoldClientInfo.ReInitializeListenerInterface;
begin
  GlobalInterfaceTable.RevokeInterfaceFromGlobal(fListenerCookie);
  GlobalInterfaceTable.RegisterInterfaceInGlobal(fBoldListener, IID_IBoldListener, fListenerCookie);
end;

function TBoldClientInfo.GetClientStatusString: string;
begin
  case fClientStatus of
    crsOK: result := sOK;
    crsNotReceiving: result := sNotReceiving;
    crsRecovered: result := sRecovered;
    else result := sUnknown;
  end;
end;

procedure TBoldClientInfo.ExtendLease(LeaseDuration: integer);
var
  LeaseLength: TTimeStamp;
begin
  fLeaseDuration := LeaseDuration;

  LeaseLength.Time := LeaseDuration;
  LeaseLength.Date := DateTimeToTimeStamp(0).Date;
  fLeaseTimeOut := DateTimetoTimeStamp(now + TimestampToDateTime(LeaseLength));
end;

{TBoldClientInfoList}
constructor TBoldClientInfoList.Create;
begin
  inherited;
  fList := TBoldObjectArray.Create(MINIMAL_CLIENT_INFO_LIST_GROWTH, [bcoDataOwner, bcoThreadSafe]);
  fFreeClientIDs := TList.Create;
end;

destructor TBoldClientInfoList.Destroy;
begin
  FreeAndNil(fFreeClientIDs);
  FreeAndNil(fList);
  inherited;
end;

function TBoldClientInfoList.Add(AObject: TBoldClientInfo): Integer;
begin
  Result := fList.Add(AObject);
end;

function TBoldClientInfoList.Remove(AObject: TBoldClientInfo): Integer;
begin
  Result := fList.Remove(AObject);
end;

function TBoldClientInfoList.IndexOf(AObject: TBoldClientInfo): Integer;
begin
  Result := fList.IndexOf(AObject);
end;

procedure TBoldClientInfoList.EnsureListCapacity(const Index: integer);
var
  i, cnt: integer;
begin
  if (Index >= fList.Count) then
  begin
    cnt := (Index - fList.Count) + 1;
    if cnt < MINIMAL_CLIENT_INFO_LIST_GROWTH  then
      cnt := MINIMAL_CLIENT_INFO_LIST_GROWTH;
    for i:= 0 to cnt - 1 do
      ReturnFreeClientID(fList.Add(nil));
  end;
end;

function TBoldClientInfoList.GetItem(Index: Integer): TBoldClientInfo;
begin
  try
    EnsureListCapacity(Index);
    Result := TBoldClientInfo(fList[Index]);
  except
    Result := nil;
  end;
end;

procedure TBoldClientInfoList.SetItem(Index: integer; AObject: TBoldClientInfo);
begin
  EnsureListCapacity(Index);
  fList[Index] := AObject;
end;

procedure TBoldClientInfoList.Insert(Index: Integer; AObject: TBoldClientInfo);
begin
  fList.Insert(Index, AObject);
end;

function TBoldClientInfoList.getCount: integer;
begin
  Result := fList.Count;
end;

function TBoldClientInfoList.Last: TBoldClientInfo;
begin
  Result := TBoldClientInfo(fList[fList.Count - 1]);
end;

procedure TBoldClientInfoList.Delete(Index: Integer);
begin
  fList.Delete(Index);
end;

function TBoldClientInfoList.GetExistingClientInfo(
  Index: integer): TBoldClientInfo;
begin
  Result := nil;
  try
    if ((Index >= 0) and (Index < fList.Count)) then
      Result := (fList[Index] as TBoldClientInfo);
  except on E: Exception do
    BoldLogError(sLogErrorAndID, [ClassName, 'GetExistingClientInfo', index, E.message]); // do not localize
  end;
end;

procedure TBoldClientInfoList.Clear;
begin
  fList.Clear;
end;

function TBoldClientInfoList.GetFreeClientID: TBoldClientID;
begin
  EnsureListCapacity(fList.Count + MINIMAL_FREE_CLIENTID_COUNT - fFreeClientIds.Count);
  Result := TBoldClientID(fFreeClientIDs.First);
  fFreeClientIDs.Delete(0);
end;

procedure TBoldClientInfoList.ReturnFreeClientID(
  const ClientID: TBoldClientID);
begin
  fFreeClientIds.Add(Pointer(ClientId));
end;

end.
