
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractPersistenceHandleDB;

interface

uses
  Classes,
  BoldDefs,
  BoldPersistenceHandle,
  BoldAbstractModel,
  BoldSubscription,
  BoldAbstractObjectUpgraderHandle,
  BoldPersistenceController,
  BoldSQLDatabaseConfig,
  BoldPersistenceControllerDefault,
  BoldDbInterfaces,
  BoldPSParams, BoldIndexCollection;

type
  { forward declarations }
  TBoldAbstractPersistenceHandleDB = class;

  { TBoldAbstractPersistenceHandleDB }
  TBoldAbstractPersistenceHandleDB = class(TBoldPersistenceHandle)
  private
    fBoldModel: TBoldAbstractModel;
    fOnGetCurrentTime: TBoldGetTimeEvent;
    fClockLogGranularity: TDateTime;
    FEvolutionSupport: Boolean;
    fIgnoreUnknownTables: Boolean;
    fComponentSubscriber: TBoldPassThroughSubscriber;
    FUpgraderHandle: TBoldAbstractObjectUpgraderHandle;
    function GetClockLogGranularity: string;
    procedure SetClockLogGranularity(const Value: string);
    procedure SetEvolutionSupport(const Value: Boolean);
    procedure _ReceiveComponentEvents(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetUpgraderHandle(const Value: TBoldAbstractObjectUpgraderHandle);
    procedure PlaceComponentSubscriptions;
    procedure SetBoldModel(NewModel: TBoldAbstractModel);
    function GetPersistenceControllerDefault: TBoldPersistenceControllerDefault;
    procedure PreparePSParams(PSParams: TBoldPSParams);
  protected
    function CreatePersistenceController: TBoldPersistenceController; override;
    procedure SetActive(Value: Boolean); override;
    function GetSQLDatabaseConfig: TBoldSQLDatabaseConfig; virtual; abstract;
    function GetCustomIndexes: TBoldIndexCollection; virtual; abstract;
    function GetDataBaseInterface: IBoldDatabase; virtual; abstract;
    procedure AssertSQLDatabaseconfig(Context: String); virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property PersistenceControllerDefault: TBoldPersistenceControllerDefault read GetPersistenceControllerDefault;
    procedure CreateDataBaseSchema(IgnoreUnknownTables: Boolean = false);
    procedure CreateDataBase(DropExisting: boolean = true);
    procedure AddModelEvolutionInfoToDatabase;
    property DatabaseInterface: IBoldDatabase read GetDatabaseInterface;
    property SQLDatabaseConfig: TBoldSQLDatabaseConfig read GetSQLDatabaseConfig;
    property CustomIndexes: TBoldIndexCOllection read GetCustomIndexes;
  published
    property BoldModel: TBoldAbstractModel read FBoldModel write SetBoldModel;
    property OnGetCurrentTime: TBoldGetTimeEvent read fOnGetCurrentTime write fOnGetCurrentTime;
    property ClockLogGranularity: string read GetClockLogGranularity write SetClockLogGranularity;
    property EvolutionSupport: Boolean read FEvolutionSupport write SetEvolutionSupport default false;
    property UpgraderHandle: TBoldAbstractObjectUpgraderHandle read FUpgraderHandle write SetUpgraderHandle;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldPSParamsSQL,
  BoldLogHandler,
  BoldPSDescriptionsSQL,
  BoldPMappersDefault,
  BoldPersistenceControllerPassthrough;

const
  breModelChanged = 100;
  breModelDestroying = 101;
  breUpgraderHandleDestroying = 102;

{ TBoldAbstractPersistenceHandleDB }

procedure TBoldAbstractPersistenceHandleDB._ReceiveComponentEvents(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    breModelChanged: if not active then
      ReleasePersistenceController;
    breModelDestroying: BoldModel := nil;
    breUpgraderHandleDestroying: UpgraderHandle := nil;
  end;
end;

procedure TBoldAbstractPersistenceHandleDB.AddModelEvolutionInfoToDatabase;
  procedure CreateTable(TableDescription: TBoldSQLTableDescription);
  var
    Query: IBoldExecQuery;
  begin
    Query := DatabaseInterface.GetExecQuery;
    try
      Query.AssignSQLText(TableDescription.SQLForCreateTable(DatabaseInterface));
      Query.ExecSQL;
    finally
      DatabaseInterface.ReleaseExecQuery(Query);
    end;
  end;

  procedure EnsureTable(TableDescription: TBoldSQLTableDescription);
  var
    MappingTable: IBoldTable;
  begin
    MappingTable := DatabaseInterface.GetTable;
    try
      MappingTable.TableName := TableDescription.SQLName;
      if not MappingTable.Exists then
        CreateTable(TableDescription);
    finally
      DatabaseInterface.ReleaseTable(MappingTable);
    end;
  end;

var
  WasActive: Boolean;
  OldEvolutionSupport: Boolean;

begin
  WasActive := Active;
  OldEvolutionSupport := EvolutionSupport;
  if not WasActive then
  begin
    OldEvolutionSupport := EvolutionSupport;
    EvolutionSupport := false;
    Active := true;
  end;

  EnsureTable(PersistenceControllerDefault.PersistenceMapper.PSSystemDescription.MemberMappingTable);
  EnsureTable(PersistenceControllerDefault.PersistenceMapper.PSSystemDescription.AllInstancesMappingTable);
  EnsureTable(PersistenceControllerDefault.PersistenceMapper.PSSystemDescription.ObjectStorageMappingTable);

  PersistenceControllerDefault.PersistenceMapper.MappingInfo.WriteDataToDB(DatabaseInterface);

  if not WasActive then
  begin
    Active := false;
    EvolutionSupport := OldEvolutionSupport;
  end;
end;

constructor TBoldAbstractPersistenceHandleDB.Create(Owner: TComponent);
begin
  inherited;
  fComponentSubscriber := TBoldPassthroughSubscriber.Create(_ReceiveComponentEvents);
end;

procedure TBoldAbstractPersistenceHandleDB.CreateDataBase(DropExisting: boolean = true);
begin
  DatabaseInterface.CreateDatabase(DropExisting);
end;

procedure TBoldAbstractPersistenceHandleDB.CreateDataBaseSchema(
  IgnoreUnknownTables: Boolean);
var
  Pmapper: TBoldSystemDefaultMapper;
begin
  fIgnoreUnknownTables := IgnoreUnknownTables;
  if Active then
    raise EBold.Create(sCannotGenDBWithActiveHandle);

  if not assigned(BoldModel) then
    raise EBold.CreateFmt(sMissingModelWhenGenDB, [ClassName, Name]);
  AssertSQLDatabaseconfig(sGenerateSchema);

  PMapper := TBoldSystemDefaultMapper.CreateFromMold(BoldModel.RawMoldModel, BoldModel.TypeNameDictionary,
    CustomIndexes, SQLDataBaseConfig, GetDataBaseInterface);
  try
    try
      BoldLog.StartLog(sGenerateSchema);
      PMapper.OpenDatabase(false, false);
      PMapper.OnPreparePSParams := PreparePSParams;
      PMapper.CreatePersistentStorage;
      BoldLog.EndLog;
    except
      on E: Exception do
      begin
        BoldLog.LogFmt(sSchemaGenerationFailed,['', '', E.message], ltError);
        raise;
      end;
    end;
  finally
    PMapper.CloseDataBase;
    PMapper.Free;
  end;
end;

function TBoldAbstractPersistenceHandleDB.CreatePersistenceController: TBoldPersistenceController;
var
  PController: TBoldPersistenceControllerDefault;
begin
  if not assigned(BoldModel) then
    raise EBold.CreateFmt(sMissingModelWhenGetController, [ClassName]);
  if not assigned(SQLDataBaseConfig) then
    raise EBold.CreateFmt(sMissingDBConfigWhenGetController, [ClassName]);
  PController := TBoldPersistenceControllerDefault.CreateFromMold(BoldModel.MoldModel, BoldModel.TypeNameDictionary,
    CustomIndexes, SQLDataBaseConfig, GetDataBaseInterface);

  PController.PersistenceMapper.OnGetCurrentTime := fOnGetCurrentTime;
  PController.PersistenceMapper.ClockLogGranularity := fClockLogGranularity;
  if assigned(UpgraderHandle) then
    PController.PersistenceMapper.ObjectUpgrader := UpgraderHandle.ObjectUpgrader;
  result := PController;
end;

destructor TBoldAbstractPersistenceHandleDB.Destroy;
begin
  FreeAndNil(fComponentSubscriber);
  inherited;
end;

function TBoldAbstractPersistenceHandleDB.GetClockLogGranularity: string;
var
  hrs, mins, secs, msecs: Word;
begin
  DecodeTime(fClockLogGranularity, hrs, mins, secs, msecs);
  result := Format('%d:%d:%d.%d', [hrs, mins, secs, msecs]);
end;

function TBoldAbstractPersistenceHandleDB.GetPersistenceControllerDefault: TBoldPersistenceControllerDefault;
var
  vPersistenceController: TBoldPersistenceController;
begin
  vPersistenceController := PersistenceController;
  while not (vPersistenceController is TBoldPersistenceControllerDefault) and (vPersistenceController is TBoldPersistenceControllerPassthrough) do
    vPersistenceController := (vPersistenceController as TBoldPersistenceControllerPassthrough).NextPersistenceController;
  result := vPersistenceController as TBoldPersistenceControllerDefault;
end;

procedure TBoldAbstractPersistenceHandleDB.PlaceComponentSubscriptions;
begin
  fComponentSubscriber.CancelAllSubscriptions;
  if assigned(fBoldModel) then
  begin
    fBoldModel.AddSmallSubscription(fComponentSubscriber, [beModelChanged], breModelChanged);
    fBoldModel.AddSmallSubscription(fComponentSubscriber, [beDestroying], breModelDestroying);
  end;
  if assigned(fUpgraderHandle) then
    FUpgraderHandle.AddSmallSubscription(fComponentSubscriber, [beDestroying], breUpgraderHandleDestroying);
end;

procedure TBoldAbstractPersistenceHandleDB.PreparePSParams(PSParams: TBoldPSParams);
begin
  (PSParams as TBoldPSSQLParams).IgnoreUnknownTables := fIgnoreUnknownTables;
end;

procedure TBoldAbstractPersistenceHandleDB.SetActive(Value: Boolean);
begin
  if value <> Active then
  begin
    Assert(Assigned(PersistenceControllerDefault));  
    if value then
    begin
      if assigned(UpgraderHandle) and not BoldModel.MoldModel.UseModelVersion then
        raise EBold.CreateFmt(sCannotActivatePersistanceHandle, [classname]);
      PersistenceControllerDefault.OpenDatabase(EvolutionSupport);
    end
    else
      PersistenceControllerDefault.CloseDatabase;
  end;
  inherited;
end;

procedure TBoldAbstractPersistenceHandleDB.SetBoldModel(NewModel: TBoldAbstractModel);
begin
  if FBoldModel <> NewModel then
  begin
    ReleasePersistenceController;
    FBoldModel := NewModel;
    PlaceComponentSubscriptions;
  end;
end;

procedure TBoldAbstractPersistenceHandleDB.SetClockLogGranularity(const Value: string);
var
  hrs, mins, secs, msecs: Word;
  input: string;

  function GetNext(Delimiter: string): Integer;
  var
    p: Integer;
  begin
    if Delimiter <> '' then
    begin
      p := pos(Delimiter, input);
      if p < 2 then
        raise EBold.CreateFmt(sWrongTimeFormat, [classname]);
    end else
      p := length(input) + 1;
    result := StrToIntDef(Copy(input, 1, p - 1), -1);
    if result = -1 then
      raise EBold.CreateFmt(sWrongTimeFormat, [classname]);
    input := Copy(input, p + 1, maxint);
  end;

begin
  input := Value;
  if input = '' then
    fClockLogGranularity := 0
  else
  begin
    hrs := GetNext(':');
    mins := GetNext(':');
    secs := GetNext('.');
    msecs := GetNext('');
    fClockLogGranularity := EncodeTime(hrs, mins, secs, msecs);
  end;
end;

procedure TBoldAbstractPersistenceHandleDB.SetEvolutionSupport(const Value: Boolean);
begin
  if Value <> FEvolutionSupport then
  begin
    if Active then
      raise EBold.CreateFmt(sCannotSetEvolutionSupport, [classname, name]);
    FEvolutionSupport := Value;
  end;
end;

procedure TBoldAbstractPersistenceHandleDB.SetUpgraderHandle(
  const Value: TBoldAbstractObjectUpgraderHandle);
begin
  if FUpgraderHandle <> Value then
  begin
    FUpgraderHandle := Value;
    if HasPersistenceController then
    begin
      if assigned(Value) then
        PersistenceControllerDefault.PersistenceMapper.ObjectUpgrader := Value.ObjectUpgrader
      else
        PersistenceControllerDefault.PersistenceMapper.ObjectUpgrader := nil;
    end;
    PlaceComponentSubscriptions;
  end;
end;

procedure TBoldAbstractPersistenceHandleDB.AssertSQLDatabaseconfig(Context: String);
begin
  if not assigned(SQLDatabaseConfig) then
    raise EBold.CreateFmt(sMissingSQLDatabaseConfig, [classname, Context]);
end;

end.