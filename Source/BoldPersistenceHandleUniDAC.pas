{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleUniDAC;

interface

uses
  Classes,
  DBAccess,
  Uni,
//  UniProvider,
  Dialogs,
  BoldDBInterfaces,
  BoldUniDACInterfaces,
  BoldPersistenceHandleDB,
  BoldPersistenceHandleDB_deprecated;

type
  TBoldNetWorkProtocol = (bnwpLocal, bnwpTcpIP, bnwpNetBEUI, bnwpNovellSPX);

  TBoldPersistenceHandleUniDAC = class(TBoldDBPersistenceHandle)
  private
    fUniConnection: TUniConnection;
    fOwnConnection: TUniConnection;
    fConnectionAdapter: TBoldUniDACConnection;
    fDataSource: string;
    fProvider: string;
    fPersistSecurityInfo: Boolean;
    fNetWorkProtocol: TBoldNetWorkProtocol;
    fHostName: string;
    fInitialCatalog: string;
    fExtendedProperties: TStringList;
    procedure SetDataSource(const Value: string);
    procedure SetProvider(const Value: string);
    procedure SetPersistSecurityInfo(const Value: Boolean);
    procedure SetNetWorkProtocol(const Value: TBoldNetWorkProtocol);
    procedure SetHostName(const Value: string);
    procedure SetInitialCatalog(const Value: string);
    procedure SetExtendedProperties(const Value: TStringList);
    procedure SetUniConnection(const Value: TUniConnection);
    function getEffectiveConnection: TUniConnection;
  protected
    procedure AddExtendedProperties(Result: TStrings); virtual;
    procedure UpdateInternalConnectionString; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property EffectiveConnection: TUniConnection read getEffectiveConnection;
    procedure SetPassword(const Value: string); override;
    procedure SetUserName(const Value: string); override;
    {$IFNDEF T2H}
    procedure InternalTransferproperties(const target: TBoldPersistenceHandleDB); override;
    {$ENDIF}
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    function GetDataBaseInterface: IBoldDatabase; override;
    procedure Assign(Source: TPersistent); override;
  published
    property DataSource: string read FDataSource write SetDataSource;
    property Provider: string read FProvider write SetProvider;
    property PersistSecurityInfo: Boolean read FPersistSecurityInfo write SetPersistSecurityInfo;
    property NetWorkProtocol: TBoldNetWorkProtocol read FNetWorkProtocol write SetNetWorkProtocol;
    property HostName: string read FHostName write SetHostName;
    property InitialCatalog: string read FInitialCatalog write SetInitialCatalog;
    property ExtendedProperties: TStringList read fExtendedProperties write SetExtendedProperties;
    property UniConnection: TUniConnection read fUniConnection write SetUniConnection;
  end deprecated;

const
  BoldNetWorkProtocolStringRep: array[TBoldNetWorkProtocol] of string = ('<Local>', 'tcpip', 'netbeui', 'spx');

function BoldStringToNetworkProtocol(s: string): TBoldNetWorkProtocol;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldDatabaseAdapterUniDAC,
  BoldUtils;

function BoldStringToNetworkProtocol(s: string): TBoldNetWorkProtocol;
var
  i: TBoldNetWorkProtocol;
begin
  for i := low(BoldNetWorkProtocolStringRep) to high(BoldNetWorkProtocolStringRep) do
    if CompareText(s, BoldNetWorkProtocolStringRep[i]) = 0 then
    begin
      Result := i;
      exit;
    end;

  Result := bnwpLocal;
end;

{ TBoldPersistenceHandleUniDAC }

procedure TBoldPersistenceHandleUniDAC.AddExtendedProperties(
  Result: TStrings);
begin
  if trim(DataSource) <> '' then
    Result.Add('DSN=' + trim(DataSource)); // do not localize
  if trim(HostName) <> '' then
    Result.Add('Hostname=' + trim(HostName)); // do not localize
  if trim(InitialCatalog) <> '' then
    Result.Add('Initial Catalog=' + trim(InitialCatalog)); // do not localize

  Result.Add('NetworkProt=' + BoldNetWorkProtocolStringRep[NetWorkProtocol]); // do not localize

  Result.AddStrings(ExtendedProperties);
end;

procedure TBoldPersistenceHandleUniDAC.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TBoldPersistenceHandleUniDAC then
    ExtendedProperties.Assign((Source as TBoldPersistenceHandleUniDAC).ExtendedProperties);
end;

procedure TBoldPersistenceHandleUniDAC.UpdateInternalConnectionString;
const
  BoolToStr: array[Boolean] of string = ('false', 'true');
var
  LExtendedProperties: TStringList;
  ConnectionString: string;
begin
  if Assigned(fOwnConnection) then
  begin
    LExtendedProperties := TStringList.Create;
    AddExtendedProperties(LExtendedProperties);

    ConnectionString := format('Provider=%s;Persist Security Info=%s;Data Source=%s;Extended Properties=%s', // do not localize
      [Provider,
      BoolToStr[PersistSecurityInfo],
        DataSource,
        BoldSeparateStringList(LExtendedProperties, ';', '"', '"')]);
    LExtendedProperties.Free;
    fOwnConnection.ConnectString := ConnectionString;
  end;
end;

constructor TBoldPersistenceHandleUniDAC.Create(owner: TComponent);
begin
  inherited;
  FHostName := '<Local>'; // do not localize
  fExtendedProperties := TStringList.Create;
end;

destructor TBoldPersistenceHandleUniDAC.Destroy;
begin
  Active := False;
  FreeAndNil(fOwnConnection);
  FreeAndNil(fConnectionAdapter);
  inherited;
end;

procedure TBoldPersistenceHandleUniDAC.SetUniConnection(
  const Value: TUniConnection);
begin
  if fUniConnection <> Value then
  begin
    CheckInactive('SetDataBase'); // do not localize
    if Assigned(fOwnConnection) then
    begin
      FreeAndNil(fOwnConnection);
      FreeAndNil(fConnectionAdapter);
    end;
    fUniConnection := Value;
    if Assigned(fUniConnection) then
      fUniConnection.FreeNotification(Self);
  end;
end;

procedure TBoldPersistenceHandleUniDAC.SetDataSource(const Value: string);
begin
  FDataSource := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleUniDAC.SetExtendedProperties(
  const Value: TStringList);
begin
  fExtendedProperties.Assign(Value);
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleUniDAC.SetHostName(const Value: string);
begin
  FHostName := Value;
  if trim(FHostName) = '' then
    FHostName := '<Local>'; // do not localize
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleUniDAC.SetInitialCatalog(const Value: string);
begin
  FInitialCatalog := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleUniDAC.SetNetWorkProtocol(
  const Value: TBoldNetWorkProtocol);
begin
  FNetWorkProtocol := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleUniDAC.SetPersistSecurityInfo(
  const Value: Boolean);
begin
  FPersistSecurityInfo := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleUniDAC.SetProvider(const Value: string);
begin
  FProvider := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleUniDAC.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = fUniConnection) and (Operation = opRemove) then
  begin
    if AComponent = EffectiveConnection then
    begin
      Active := False;
      fConnectionAdapter := nil;
    end;
    fUniConnection := nil;
  end;
end;

function TBoldPersistenceHandleUniDAC.getEffectiveConnection: TUniConnection;
begin
  if Assigned(fUniConnection) then
    Result := fUniConnection
  else
  begin
    if not Assigned(fOwnConnection) then
    begin
      fOwnConnection := TUniConnection.Create(nil);
      UpdateInternalConnectionString;
    end;
    Result := fOwnConnection;
  end;
end;

procedure TBoldPersistenceHandleUniDAC.SetPassword(const Value: string);
begin
  inherited;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleUniDAC.SetUserName(const Value: string);
begin
  inherited;
  UpdateInternalConnectionString;
end;

function TBoldPersistenceHandleUniDAC.GetDataBaseInterface: IBoldDatabase;
begin
  if not Assigned(fConnectionAdapter) then
    fConnectionAdapter := TBoldUniDACConnection.Create(EffectiveConnection, SQLDataBaseConfig);
  Result := fConnectionAdapter;
end;

procedure TBoldPersistenceHandleUniDAC.InternalTransferproperties(
  const target: TBoldPersistenceHandleDB);
const
  BoolToStr: array[Boolean] of string = ('false', 'true');
var
  Adapter: tBoldDatabaseAdapterUniDAC;
  DesInfo: longint;
  LExtendedProperties: TStringList;
  ConnectionString: string;
begin
  inherited;
  DesInfo := target.DesignInfo;
  if not Assigned(target.DatabaseAdapter) then
  begin
    target.DatabaseAdapter := tBoldDatabaseAdapterUniDAC.Create(target.owner);
    target.DatabaseAdapter.Name := GetNewComponentName(target.DatabaseAdapter, 'BoldDatabaseAdapterUniDAC'); // do not localize
    LongRec(DesInfo).Lo := LongRec(DesInfo).Lo + 16; //set Left
    LongRec(DesInfo).Hi := LongRec(DesInfo).Hi + 16; //Set Top;
    target.DatabaseAdapter.DesignInfo := DesInfo;
    showmessage(sCreatedNewAdapter);
  end
  else if not (target.DatabaseAdapter is TBoldDatabaseAdapterUniDAC) then
    raise Exception.CreateFmt(sCanOnlyTransferToUniDACAdapter, [target.DatabaseAdapter.ClassName]);

  Adapter := target.DatabaseAdapter as TBoldDatabaseAdapterUniDAC;
  if Assigned(fUniConnection) then
    Adapter.Connection := UniConnection;

  if not Assigned(Adapter.Connection) then
  begin
    Adapter.Connection := TUniConnection.Create(target.owner);
    Adapter.Connection.Name := GetNewComponentName(Adapter.Connection, 'UniDACDatabase'); // do not localize
    showmessage(sCreatedNewDB);
    LongRec(DesInfo).Lo := LongRec(DesInfo).Lo + 16; //set Left
    LongRec(DesInfo).Hi := LongRec(DesInfo).Hi + 16; //Set Top;
    Adapter.Connection.DesignInfo := DesInfo;

    try
      LExtendedProperties := TStringList.Create;
      AddExtendedProperties(LExtendedProperties);

      ConnectionString := format('Provider=%s;Persist Security Info=%s;Data Source=%s;Extended Properties=%s', // do not localize
        [Provider,
        BoolToStr[PersistSecurityInfo],
          DataSource,
          BoldSeparateStringList(LExtendedProperties, ';', '"', '"')]);
      LExtendedProperties.Free;
      Adapter.Connection.ConnectString := ConnectionString;
    except
      on E: Exception do
      begin
        showmessage(Format(sCouldNotTransferConnectionString, [BOLDCRLF + BOLDCRLF, E.Message, BOLDCRLF + BOLDCRLF]));
        Adapter.Connection.ConnectString := '';
      end;
    end;
  end;
end;

end.

