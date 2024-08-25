
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleADO;

interface

uses
  Classes,
  ADODB,
  Dialogs,
  BoldDBInterfaces,
  BoldADOInterfaces,
  BoldPersistenceHandleDB,
  BoldPersistenceHandleDB_deprecated;

type
  TBoldNetWorkProtocol = (bnwpLocal, bnwpTcpIP, bnwpNetBEUI, bnwpNovellSPX);

  TBoldPersistenceHandleADO = class(TBoldDBPersistenceHandle)
  private
    fADOConnection: TADOConnection;
    fOwnConnection: TADOConnection;
    fConnectionAdapter: TBoldADOConnection;

    FDataSource: String;
    FProvider: String;
    FPersistSecurityInfo: Boolean;
    FNetWorkProtocol: TBoldNetWorkProtocol;
    FHostName: String;
    FInitialCatalog: String;
    fExtendedProperties: TStringList;
    procedure SetDataSource(const Value: String);
    procedure SetProvider(const Value: String);
    procedure SetPersistSecurityInfo(const Value: Boolean);
    procedure SetNetWorkProtocol(const Value: TBoldNetWorkProtocol);
    procedure SetHostName(const Value: String);
    procedure SetInitialCatalog(const Value: String);
    procedure SetExtendedProperties(const Value: TStringList);
    procedure SetADOConnection(const Value: TADOConnection);
    function getEffectiveConnection: TADOConnection;
  protected
    procedure AddExtendedProperties(result: TStrings); virtual;
    procedure UpdateInternalConnectionString; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property EffectiveConnection: TADOConnection read getEffectiveConnection;
    procedure SetPassword(const Value: string); override;
    procedure SetUserName(const Value: string); override;
    {$IFNDEF T2H}
    procedure InternalTransferproperties(const target: TBoldPersistenceHandleDB); override;
    {$ENDIF}
  public
    constructor create(owner: tComponent); override;
    destructor destroy; override;
    function GetDataBaseInterface: IBoldDatabase; override;
    procedure Assign(Source: TPersistent); override;
  published
    property DataSource: String read FDataSource write SetDataSource;
    property Provider: String read FProvider write SetProvider;
    property PersistSecurityInfo: Boolean read FPersistSecurityInfo write SetPersistSecurityInfo;
    property NetWorkProtocol: TBoldNetWorkProtocol read FNetWorkProtocol write SetNetWorkProtocol;
    property HostName: String read FHostName write SetHostName;
    property InitialCatalog: String read FInitialCatalog write SetInitialCatalog;
    property ExtendedProperties: TStringList read fExtendedProperties write SetExtendedProperties;
    property ADOConnection: TADOConnection read fADOConnection write SetADOConnection;
  end deprecated;


const
  BoldNetWorkProtocolStringRep: array[TBoldNetWorkProtocol] of string = ('<Local>', 'tcpip', 'netbeui', 'spx');

function BoldStringToNetworkProtocol(s: String): TBoldNetworkProtocol;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldDatabaseAdapterAdo,
  BoldUtils;

function BoldStringToNetworkProtocol(s: String): TBoldNetworkProtocol;
var
  i: TBoldNetworkProtocol;
begin
  for i := low(BoldNetWorkProtocolStringRep) to high(BoldNetWorkProtocolStringRep) do
    if CompareText(s, BoldNetWorkProtocolStringRep[i]) = 0 then
    begin
      result := i;
      exit;
    end;
    
  result := bnwpLocal;
end;

{ TBoldPersistenceHandleADO }

procedure TBoldPersistenceHandleADO.AddExtendedProperties(
  Result: TStrings);
begin
  if trim(DataSource) <> '' then
    result.Add('DSN='+trim(DataSource));
  if trim(HostName) <> '' then
    Result.Add('Hostname='+trim(HostName));
  if trim(InitialCatalog) <> '' then
    Result.Add('Initial Catalog='+trim(InitialCatalog));

  result.Add('NetworkProt='+BoldNetWorkProtocolStringRep[NetWorkProtocol]);

  Result.AddStrings(ExtendedProperties);
end;

procedure TBoldPersistenceHandleADO.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TBoldPersistenceHandleADO then
    ExtendedProperties.assign((Source as TBoldPersistenceHandleADO).ExtendedProperties);
end;

procedure TBoldPersistenceHandleADO.UpdateInternalConnectionString;
const
  BoolToStr: array[boolean] of String= ('false', 'true');
var
  LExtendedProperties: TStringList;
  ConnectionString: String;
begin
  if assigned(fOwnConnection) then
  begin
    LExtendedProperties := TStringList.Create;
    AddExtendedProperties(LExtendedProperties);

    ConnectionString := format('Provider=%s;Persist Security Info=%s;Data Source=%s;Extended Properties=%s',
    [Provider,
      BoolToStr[PersistSecurityInfo],
      DataSource,
      BoldSeparateStringList(LExtendedProperties, ';', '"', '"')]);
    LExtendedProperties.Free;
    fOwnConnection.ConnectionString := ConnectionString;
  end;
end;

constructor TBoldPersistenceHandleADO.create(owner: tComponent);
begin
  inherited;
  FHostName := '<Local>';
  fExtendedProperties := TStringList.Create;
end;


destructor TBoldPersistenceHandleADO.destroy;
begin
  Active := false;
  FreeAndNil(fOwnConnection);
  FreeAndNil(fConnectionAdapter);
  inherited;
end;


procedure TBoldPersistenceHandleADO.SetADOConnection(
  const Value: TADOConnection);
begin
  if fADOConnection <> Value then
  begin
    CheckInactive('SetDataBase');
    if assigned(fOwnConnection) then
    begin
      FreeAndNil(fOwnConnection);
      FreeAndNil(fConnectionAdapter);
    end;
    fADOConnection := Value;
    if assigned(fADOConnection) then
      fADOConnection.FreeNotification(self);
  end;
end;

procedure TBoldPersistenceHandleADO.SetDataSource(const Value: String);
begin
  FDataSource := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleADO.SetExtendedProperties(
  const Value: TStringList);
begin
  fExtendedProperties.Assign(Value);
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleADO.SetHostName(const Value: String);
begin
  FHostName := Value;
  if trim(fHostName) = '' then
    FHostName := '<Local>';
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleADO.SetInitialCatalog(const Value: String);
begin
  FInitialCatalog := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleADO.SetNetWorkProtocol(
  const Value: TBoldNetWorkProtocol);
begin
  FNetWorkProtocol := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleADO.SetPersistSecurityInfo(
  const Value: Boolean);
begin
  FPersistSecurityInfo := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleADO.SetProvider(const Value: String);
begin
  FProvider := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleADO.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (aComponent = fADOConnection) and (Operation = opRemove) then
  begin
    if aComponent = EffectiveConnection then
    begin
      Active := false;
      fConnectionAdapter := nil;
    end;
    fADOConnection := nil;
  end;
end;

function TBoldPersistenceHandleADO.getEffectiveConnection: TADOConnection;
begin
  if assigned(fADOConnection) then
    result := fADOConnection
  else
  begin
    if not assigned(fOwnConnection) then
    begin
      fOwnConnection := TADOConnection.Create(nil);
      UpdateInternalConnectionString;
    end;
    result := fOwnConnection;
  end;                      
end;

procedure TBoldPersistenceHandleADO.SetPassword(const Value: string);
begin
  inherited;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleADO.SetUserName(const Value: string);
begin
  inherited;
  UpdateInternalConnectionString;
end;

function TBoldPersistenceHandleADO.GetDataBaseInterface: IBoldDatabase;
begin
  if not assigned(fConnectionAdapter) then
    fConnectionAdapter := TBoldADOConnection.create(EffectiveConnection, SQLDataBaseConfig);
  result := fConnectionAdapter;
end;

procedure TBoldPersistenceHandleADO.InternalTransferproperties(
  const target: TBoldPersistenceHandleDB);
const
  BoolToStr: array[boolean] of String= ('false', 'true');
var
  Adapter: tBoldDatabaseAdapterADO;
  DesInfo: longint;
  LExtendedProperties: TStringList;
  ConnectionString: String;
begin
  inherited;
  DesInfo := Target.DesignInfo;
  if not assigned(Target.DatabaseAdapter) then
  begin
    Target.DatabaseAdapter := TBoldDatabaseAdapterADO.Create(Target.Owner);
    Target.DatabaseAdapter.Name := GetNewComponentName(Target.DatabaseAdapter, 'BoldDatabaseAdapterADO');
    LongRec(DesInfo).Lo := LongRec(DesInfo).lo+16;
    LongRec(DesInfo).Hi := LongRec(DesInfo).hi+16;
    Target.DatabaseAdapter.DesignInfo          := DesInfo;
    showmessage('Created a new DatabaseAdapterADO');
  end
  else if not (target.DatabaseAdapter is TBoldDatabaseAdapterADO) then
    raise Exception.CreateFmt('The persistencehandle is connected to a %s, properties can only be transfered to a TBoldDatabaseAdapterADO', [target.DatabaseAdapter.ClassName] );

  Adapter := target.DatabaseAdapter as tBoldDatabaseAdapterADO;
  if assigned(fADOConnection) then
    Adapter.Connection := ADOConnection;

  if not assigned(Adapter.Connection) then
  begin
    Adapter.Connection := TADOConnection.Create(Target.owner);
    Adapter.Connection.Name := GetNewComponentName(Adapter.Connection, 'ADODatabase');
    showmessage('Created a new ADODatabase');
    LongRec(DesInfo).Lo := LongRec(DesInfo).lo+16;
    LongRec(DesInfo).Hi := LongRec(DesInfo).hi+16;
    Adapter.Connection.DesignInfo          := DesInfo;

    try
      LExtendedProperties := TStringList.Create;
      AddExtendedProperties(LExtendedProperties);

      ConnectionString := format('Provider=%s;Persist Security Info=%s;Data Source=%s;Extended Properties=%s',
      [Provider,
        BoolToStr[PersistSecurityInfo],
        DataSource,
        BoldSeparateStringList(LExtendedProperties, ';', '"', '"')]);
      LExtendedProperties.Free;
      Adapter.Connection.ConnectionString := ConnectionString;
    except
      on e: exception do
      begin
        showmessage('Connection string settings could not be transferred to the new ADO connection: '+BOLDCRLF+BOLDCRLF+
          e.message + BOLDCRLF+BOLDCRLF +
          'Please transfer these manually!');
        Adapter.Connection.ConnectionString := '';

      end;
    end;

  end;
end;

end.
