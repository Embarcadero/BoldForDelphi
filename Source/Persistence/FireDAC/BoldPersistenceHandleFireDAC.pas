{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleFireDAC;

interface

uses
  Classes,
  Dialogs,

  FireDAC.Comp.Client,


  BoldDBInterfaces,
  BoldFireDACInterfaces,
  BoldPersistenceHandleDB,
  BoldPersistenceHandleDB_deprecated;

type
  TBoldNetWorkProtocol = (bnwpLocal, bnwpTcpIP, bnwpNetBEUI, bnwpNovellSPX);

  TBoldPersistenceHandleFireDAC = class(TBoldDBPersistenceHandle)
  private
    fFDConnection: TFDConnection;
    fOwnConnection: TFDConnection;
    fConnectionAdapter: TBoldFireDACConnection;
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
    procedure SetFDConnection(const Value: TFDConnection);
    function getEffectiveConnection: TFDConnection;
  protected
    procedure AddExtendedProperties(Result: TStrings); virtual;
    procedure UpdateInternalConnectionString; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property EffectiveConnection: TFDConnection read getEffectiveConnection;
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
    property FDConnection: TFDConnection read fFDConnection write SetFDConnection;
  end deprecated;

const
  BoldNetWorkProtocolStringRep: array[TBoldNetWorkProtocol] of string = ('<Local>', 'tcpip', 'netbeui', 'spx');

function BoldStringToNetworkProtocol(s: string): TBoldNetWorkProtocol;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldDatabaseAdapterFireDAC,
  BoldUtils,
  BoldFireDACConsts;

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

{ TBoldPersistenceHandleFireDAC }

procedure TBoldPersistenceHandleFireDAC.AddExtendedProperties(
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

procedure TBoldPersistenceHandleFireDAC.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TBoldPersistenceHandleFireDAC then
    ExtendedProperties.Assign((Source as TBoldPersistenceHandleFireDAC).ExtendedProperties);
end;

procedure TBoldPersistenceHandleFireDAC.UpdateInternalConnectionString;
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
    fOwnConnection.ConnectionString := ConnectionString;
  end;
end;

constructor TBoldPersistenceHandleFireDAC.Create(owner: TComponent);
begin
  inherited;
  FHostName := '<Local>'; // do not localize
  fExtendedProperties := TStringList.Create;
end;

destructor TBoldPersistenceHandleFireDAC.Destroy;
begin
  Active := False;
  FreeAndNil(fOwnConnection);
  FreeAndNil(fConnectionAdapter);
  inherited;
end;

procedure TBoldPersistenceHandleFireDAC.SetFDConnection(
  const Value: TFDConnection);
begin
  if fFDConnection <> Value then
  begin
    CheckInactive('SetDataBase'); // do not localize
    if Assigned(fOwnConnection) then
    begin
      FreeAndNil(fOwnConnection);
      FreeAndNil(fConnectionAdapter);
    end;
    fFDConnection := Value;
    if Assigned(fFDConnection) then
      fFDConnection.FreeNotification(Self);
  end;
end;

procedure TBoldPersistenceHandleFireDAC.SetDataSource(const Value: string);
begin
  FDataSource := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleFireDAC.SetExtendedProperties(
  const Value: TStringList);
begin
  fExtendedProperties.Assign(Value);
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleFireDAC.SetHostName(const Value: string);
begin
  FHostName := Value;
  if trim(FHostName) = '' then
    FHostName := '<Local>'; // do not localize
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleFireDAC.SetInitialCatalog(const Value: string);
begin
  FInitialCatalog := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleFireDAC.SetNetWorkProtocol(
  const Value: TBoldNetWorkProtocol);
begin
  FNetWorkProtocol := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleFireDAC.SetPersistSecurityInfo(
  const Value: Boolean);
begin
  FPersistSecurityInfo := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleFireDAC.SetProvider(const Value: string);
begin
  FProvider := Value;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleFireDAC.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = fFDConnection) and (Operation = opRemove) then
  begin
    if AComponent = EffectiveConnection then
    begin
      Active := False;
      fConnectionAdapter := nil;
    end;
    fFDConnection := nil;
  end;
end;

function TBoldPersistenceHandleFireDAC.getEffectiveConnection: TFDConnection;
begin
  if Assigned(fFDConnection) then
    Result := fFDConnection
  else
  begin
    if not Assigned(fOwnConnection) then
    begin
      fOwnConnection := TFDConnection.Create(nil);
      UpdateInternalConnectionString;
    end;
    Result := fOwnConnection;
  end;
end;

procedure TBoldPersistenceHandleFireDAC.SetPassword(const Value: string);
begin
  inherited;
  UpdateInternalConnectionString;
end;

procedure TBoldPersistenceHandleFireDAC.SetUserName(const Value: string);
begin
  inherited;
  UpdateInternalConnectionString;
end;

function TBoldPersistenceHandleFireDAC.GetDataBaseInterface: IBoldDatabase;
begin
  if not Assigned(fConnectionAdapter) then
    fConnectionAdapter := TBoldFireDACConnection.Create(EffectiveConnection, SQLDataBaseConfig);
  Result := fConnectionAdapter;
end;

procedure TBoldPersistenceHandleFireDAC.InternalTransferproperties(
  const target: TBoldPersistenceHandleDB);
const
  BoolToStr: array[Boolean] of string = ('false', 'true');
var
  Adapter: tBoldDatabaseAdapterFireDAC;
  DesInfo: longint;
  LExtendedProperties: TStringList;
  ConnectionString: string;
begin
  inherited;
  DesInfo := target.DesignInfo;
  if not Assigned(target.DatabaseAdapter) then
  begin
    target.DatabaseAdapter := tBoldDatabaseAdapterFireDAC.Create(target.owner);
    target.DatabaseAdapter.Name := GetNewComponentName(target.DatabaseAdapter, 'BoldDatabaseAdapterFireDAC'); // do not localize
    LongRec(DesInfo).Lo := LongRec(DesInfo).Lo + 16; //set Left
    LongRec(DesInfo).Hi := LongRec(DesInfo).Hi + 16; //Set Top;
    target.DatabaseAdapter.DesignInfo := DesInfo;
    showmessage(sCreatedNewAdapter);
  end
  else if not (target.DatabaseAdapter is tBoldDatabaseAdapterFireDAC) then
    raise Exception.CreateFmt(sCanOnlyTransferToFireDACAdapter, [target.DatabaseAdapter.ClassName]);

  Adapter := target.DatabaseAdapter as TBoldDatabaseAdapterFireDAC;
  if Assigned(fFDConnection) then
    Adapter.Connection := FDConnection;

  if not Assigned(Adapter.Connection) then
  begin
    Adapter.Connection := TFDConnection.Create(target.owner);
    Adapter.Connection.Name := GetNewComponentName(Adapter.Connection, 'FireDACDatabase'); // do not localize
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
      Adapter.Connection.ConnectionString := ConnectionString;
    except
      on E: Exception do
      begin
        showmessage(sCouldNotTransferConnectionString + BOLDCRLF + BOLDCRLF +
          E.Message + BOLDCRLF + BOLDCRLF +
          sTransferManually);
        Adapter.Connection.ConnectionString := '';

      end;
    end;

  end;
end;

end.

