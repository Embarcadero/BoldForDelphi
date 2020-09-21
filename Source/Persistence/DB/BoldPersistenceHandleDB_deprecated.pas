unit BoldPersistenceHandleDB_deprecated;

interface

uses
  Classes,
  Dialogs,
  Controls,
  BoldPersistenceHandleDb,
  BoldSQLDatabaseConfig,
  BoldAbstractPersistenceHandleDB;


type
  { forward declarations }
  TBoldDBPersistenceHandle = class;

  { TBoldDBPersistenceHandle }
  TBoldDBPersistenceHandle = class(TBoldAbstractPersistenceHandleDB)
  private
    fUserName: String;
    fPassword: String;
    fSQLDataBaseConfig: TBoldSQLDataBaseConfig;
    fDatabaseEngine: TBoldDataBaseEngine;
    procedure SQLDatabaseConfigChanged(Sender: TObject);
    procedure SetSQLDataBaseConfig(const Value: TBoldSQLDataBaseConfig);
    procedure ReadEmptyStringMarker(Reader: TReader);
    procedure ReadSystemTablePrefix(Reader: TReader);
    function GetNewPersistenceHandle: TBoldPersistenceHandleDB;
    procedure TransferPropertiesToNewPersistenceHandle(const Value: TBoldPersistenceHandleDB);
  protected
    procedure SetDataBaseEngine(const Value: TBoldDataBaseEngine); virtual;
    procedure SetPassword(const Value: string); virtual;
    procedure SetUserName(const Value: string); virtual;
    function GetSQLDatabaseConfig: TBoldSQLDataBaseConfig; override;
    {$IFDEF T2H} // properties is removed, but should still be in doc
    property EmptyStringMarker; // String
    property SystemTablesPrefix; // String
    {$ELSE}
    procedure InternalTransferproperties(const target: TBoldPersistenceHandleDB); virtual;
    function GetNewComponentName(Comp: Tcomponent; BaseName: string): String;
    {$ENDIF}
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property Username: string read fusername write SetUserName;
    property Password: string read fPassword write SetPassword;
    property SQLDataBaseConfig: TBoldSQLDataBaseConfig read GetSQLDataBaseConfig write SetSQLDataBaseConfig;
    property DatabaseEngine: TBoldDataBaseEngine read fDatabaseEngine write SetDataBaseEngine;
    property __TransferPropertiesToNewPersistenceHandle: TBoldPersistenceHandleDB read GetNewPersistenceHandle write TransferPropertiesToNewPersistenceHandle;
  end;

implementation

uses
  SysUtils,
  BoldPersistenceControllerDefault,
  BoldPersistenceHandle;

{ TBoldDBPersistenceHandle }

constructor TBoldDBPersistenceHandle.create(Owner: TComponent);
begin
  inherited;
  fSQLDatabaseConfig := TBoldSQlDatabaseConfig.Create;
end;

procedure TBoldDBPersistenceHandle.SetPassword(const Value: string);
begin
  fPassword := Value;
end;

procedure TBoldDBPersistenceHandle.SetUserName(const Value: string);
begin
  fusername := Value;
end;

destructor TBoldDBPersistenceHandle.destroy;
begin
  FreeAndNil(fSQLDataBaseConfig);
  inherited;
end;

procedure TBoldDBPersistenceHandle.SQLDatabaseConfigChanged(Sender: TObject);
begin
  if HasPersistenceController then
    PersistenceControllerDefault.SQLDataBaseConfig.AssignConfig(SQLDataBaseConfig);
end;

procedure TBoldDBPersistenceHandle.SetSQLDataBaseConfig(const Value: TBoldSQLDataBaseConfig);
begin
  SQLDataBaseConfig.AssignConfig(Value);
end;

procedure TBoldDBPersistenceHandle.SetDataBaseEngine(const Value: TBoldDataBaseEngine);
begin
  if value <> fDatabaseEngine then
  begin
    if not (csLoading in ComponentState) then
      SQLDatabaseConfig.InitializeDbEngineSettings(Value);
    fDatabaseEngine := Value;
    SQLDataBaseConfig.Engine := DatabaseEngine;
  end;
end;

function TBoldDBPersistenceHandle.GetSQLDataBaseConfig: TBoldSQLDataBaseConfig;
begin
  if not assigned(fSQLDataBaseConfig) then
  begin
    fSQLDataBaseConfig := TBoldSQLDataBaseConfig.Create;
    fSQLDataBaseConfig.OnChange := SQLDatabaseConfigChanged;
  end;
  result := fSQLDataBaseConfig;
end;

procedure TBoldDBPersistenceHandle.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  // EmptyStringMarker and SystemtablePrefix moved to SQLDatabaseConfig in v 4.0
  Filer.DefineProperty('EmptyStringMarker', ReadEmptyStringMarker, nil, True);
  Filer.DefineProperty('SystemTablesPrefix', ReadSystemTablePrefix, nil, True);
end;

procedure TBoldDBPersistenceHandle.ReadEmptyStringMarker(Reader: TReader);
begin
  SQLDataBaseConfig.EmptyStringMarker := Reader.ReadString;
end;

procedure TBoldDBPersistenceHandle.ReadSystemTablePrefix(Reader: TReader);
begin
   SQLDataBaseConfig.SystemTablePrefix := Reader.ReadString;
end;

function TBoldDBPersistenceHandle.GetNewPersistenceHandle: TBoldPersistenceHandleDB;
begin
  result := nil;
end;

procedure TBoldDBPersistenceHandle.TransferPropertiesToNewPersistenceHandle(
  const Value: TBoldPersistenceHandleDB);
begin
  if assigned(value) and
    (MessageDlg(format('Do you want to transfer the settings to %s', [value.Name]),
       mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    InternalTransferproperties(value);
    Value.SQLDatabaseConfig.AssignConfig(SQLDataBaseConfig);
    Value.ClockLogGranularity := ClockLogGranularity;
    Value.EvolutionSupport := EvolutionSupport;
    Value.UpgraderHandle := UpgraderHandle;
    Value.OnGetCurrentTime := OnGetCurrentTime;
    Value.BoldModel := BoldModel;
    showmessage('All settings have been transferred to ' + value.Name);
  end;
end;

procedure TBoldDBPersistenceHandle.InternalTransferproperties(const target: TBoldPersistenceHandleDB);
begin
  // do nothing
end;

function TBoldDBPersistenceHandle.GetNewComponentName(Comp: Tcomponent;
  BaseName: string): String;
var
  i: integer;
begin
  i := 1;
  while assigned(comp.Owner.FindComponent(BaseName+IntToStr(i))) do
    inc(i);
  result := BaseName + inttostr(i);
end;

end.
