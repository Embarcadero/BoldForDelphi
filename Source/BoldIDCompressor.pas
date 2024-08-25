unit BoldIDCompressor;

interface

uses
  Classes,
  BoldSystem,
  BoldSystemHandle,
  BoldPersistenceHandle,
  BoldSQLMappingInfo;

type
  TBoldIDCompressorProgressEvent = procedure(APosition, aTotal: integer) of object;

  [ComponentPlatforms(pidWin32 or pidWin64)]
  TBoldIDCompressor = class(TComponent)
  private
    fSystemHandle: TBoldSystemHandle;
    fSqlStrings: TStrings;
    fLogStrings: TStrings;
    FOnProgress: TBoldIDCompressorProgressEvent;
    fIdFetchBlockSize: integer;
    procedure ChangeId(ALocator: TBoldObjectLocator; ANewID: integer);
    procedure SetLogStrings(const Value: TStrings);
    function GetSQL: TStrings;
    function GetBoldSystem: TBoldSystem;
    procedure SetOnProgress(const Value: TBoldIDCompressorProgressEvent);
    procedure GetAllTablesForClass(ExpressionName: String; Mapping: TBoldSQLMappingInfo; Tables: TStringList);
    function GetAllInstanceCount: integer;
    procedure UpdateBoldIDTable;
  protected
    procedure Log(const AFormat: string); overload;
    procedure Log(const AFormat: string; const Args: array of const); overload;
    property BoldSystem: TBoldSystem read GetBoldSystem;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  published
    property LogStrings: TStrings read fLogStrings write SetLogStrings;
    property SystemHandle: TBoldSystemHandle read fSystemHandle write fSystemHandle;
    property SQL: TStrings read GetSQL;
    property OnProgress: TBoldIDCompressorProgressEvent read FOnProgress write SetOnProgress;
    property IdFetchBlockSize: integer read fIdFetchBlockSize write fIdFetchBlockSize default 1000;
  end;

implementation

uses
  Contnrs,
  SysUtils,
  BoldId,
  BoldSystemRT,
  BoldPSDescriptionsSQL,
  BoldPMappersDefault,
  BoldDefs,
  BoldGuard,
  BoldDBInterfaces,
  BoldPMappers,
  BoldPMappersSQL,
  BoldIndexableList,
  BoldBase,
  BoldElements,
  BoldMetaElementList,
  BoldDomainElement,
  BoldIndex,
  BoldDefaultId;

{ TBoldIDCompressor }

procedure TBoldIDCompressor.ChangeId(ALocator: TBoldObjectLocator; ANewID: integer);
var
  Role: TBoldRoleRTInfo;
  Tables: TStringList;
  TableName: string;
  i,m: integer;
  PersistenceMapper: TBoldSystemDefaultMapper;
  MemberPersistenceMappers: TBoldMemberMappingArray;
  MemberMappingInfo: TBoldMemberMappingInfo;
  Column: string;
const
  cUpdateBoldIDSQL = 'UPDATE %s SET %s = %d WHERE %s = %s%s';
begin
  PersistenceMapper := SystemHandle.PersistenceHandleDB.PersistenceControllerDefault.PersistenceMapper;
  Tables:= TStringList.Create;
  try
    GetAllTablesForClass(ALocator.BoldClassTypeInfo.ExpressionName, PersistenceMapper.MappingInfo, Tables);
    for i := 0 to Tables.count - 1 do
    begin
      TableName := Tables[i];
      SQL.Add(format(cUpdateBoldIDSQL, [TableName, IDCOLUMN_NAME, ANewID, IDCOLUMN_NAME, ALocator.BoldObjectID.AsString, SystemHandle.PersistenceHandleDB.SQLDatabaseConfig.SqlScriptTerminator]));
    end;
  finally
    Tables.free;
  end;
  for i := 0 to ALocator.BoldClassTypeInfo.AllRolesCount -1 do
  begin
    Role := ALocator.BoldClassTypeInfo.AllRoles[i] as TBoldRoleRTInfo;
    if role.Persistent then
    begin
      MemberPersistenceMappers := PersistenceMapper.MappingInfo.MemberMappings.MappingsByExpressionNames[role.RoleRTInfoOfOtherEnd.ClassTypeInfo.ExpressionName, role.RoleRTInfoOfOtherEnd.ExpressionName];
      for m := 0 to Length(MemberPersistenceMappers)-1 do
      begin
        MemberMappingInfo := MemberPersistenceMappers[m] as TBoldMemberMappingInfo;
        Column := MemberMappingInfo.ColumnByIndex[0];
        SQL.Add(format(cUpdateBoldIDSQL, [MemberMappingInfo.TableName, Column, ANewID, Column, ALocator.BoldObjectID.AsString, SystemHandle.PersistenceHandleDB.SQLDatabaseConfig.SqlScriptTerminator]));
      end;
    end;
  end;
end;

constructor TBoldIDCompressor.Create(owner: TComponent);
begin
  IdFetchBlockSize := 1000;
  inherited;
end;

destructor TBoldIDCompressor.Destroy;
begin
  FreeAndNil(fSqlStrings);
  inherited;
end;

procedure TBoldIDCompressor.Execute;
var
  i,j: integer;
  BoldID: integer;
  locator: TBoldObjectLocator;
  SourceObjectList: TBoldObjectList;
  SourceIDList: Contnrs.TObjectList;
  NewIDList: TList;
  NewId: integer;
  Bits: TBits;
  HighestUsedID: integer;
  Guard: IBoldGuard;
begin
  if not Assigned(SystemHandle) then
    raise Exception.Create('SystemHandle not set.');
  sql.clear;
  Guard := TBoldGuard.Create(Bits, SourceObjectList, SourceIDList, NewIDList);
  Bits:= TBits.Create;
  SourceObjectList := TBoldObjectList.Create;
  SourceObjectList.DuplicateMode := bldmAllow; // no duplicates, but doesn't search on insert
  SourceIDList := TObjectList.Create(false);
  NewIDList := TList.Create;
  Log('Getting BoldIDs from db.');
  // load all locators, sorted by BoldID
  j := GetAllInstanceCount;
  if Assigned(fOnProgress) then
    fOnProgress(0, j);
  repeat
    i := SourceObjectList.Count;
    SystemHandle.System.GetAllInClassWithSQL(SourceObjectList, TBoldObjectClass(BoldSystem.BoldSystemTypeInfo.RootClassTypeInfo.ObjectClass), '', 'Bold_ID', nil, true, IdFetchBlockSize, i);
    if Assigned(fOnProgress) then
      fOnProgress(SourceObjectList.Count, j);
  until i = SourceObjectList.Count;
  Log('Loaded %d BoldIDs', [SourceObjectList.count]);
  HighestUsedID := SourceObjectList.Locators[SourceObjectList.Count-1].AsString.ToInteger;
  Bits.Size := HighestUsedID;
  Log('Highest used ID = %d', [HighestUsedID]);
  Log('%.7f percent of IDs used', [((HighestUsedID-SourceObjectList.count)/maxint*100)]);
  if HighestUsedID = SourceObjectList.count then
  begin
    Log('No free IDs, nothing to do.');
    exit;
  end;
  for i := 0 to SourceObjectList.Count - 1 do
  begin
    BoldID := SourceObjectList.Locators[i].AsString.ToInteger;
    Bits[BoldID] := true;
  end;
  SourceObjectList.Clear;
  j := Bits.Size-1;
  i := 1; // Skip BoldID 0, start from 1
  while (j > i) do
  begin
    if Bits[j] then
    begin
      Locator := BoldSystem.Locators.LocatorByIdString[j.ToString];
      while (bits[i]) and (i<j) do
        inc(i);
      if bits[i] then
        break;
      SourceIDList.Add(Locator);
      NewIDList.Add(Pointer(i));
      inc(i);
    end;
    dec(j);
  end;
  Assert(SourceIDList.Count = NewIdList.Count);
  j := SourceIDList.Count-1;
  for i := 0 to SourceIDList.Count-1 do
  begin
    Locator := TBoldObjectLocator(SourceIDList[j-i]);
    NewId := Integer(NewIdList[i]);
    Log('%s %s -> %d', [Locator.BoldClassTypeInfo.ExpressionName, Locator.AsString, NewId]);
    ChangeID(locator, NewId);
    if Assigned(fOnProgress) then
      fOnProgress(i, j);
  end;
  UpdateBoldIDTable;
  Log('Succesfully completed.');
end;

function TBoldIDCompressor.GetAllInstanceCount: integer;
var
  Query: IBoldQuery;
  RootTableName: string;
begin
  RootTableName := SystemHandle.PersistenceHandleDB.PersistenceControllerDefault.PersistenceMapper.RootClassObjectPersistenceMapper.MainTable.SQLName;
  Query := BoldSystem.PersistenceController.DatabaseInterface.GetQuery;
  try
    Query.SQLText := 'select count(*) from ' + RootTableName;
    Query.Open;
    result := Query.Fields[0].AsInteger;
    Query.Close;
  finally
    BoldSystem.PersistenceController.DatabaseInterface.ReleaseQuery(Query);
  end;
end;

procedure TBoldIDCompressor.GetAllTablesForClass(ExpressionName: String;
  Mapping: TBoldSQLMappingInfo; Tables: TStringList);
var
  StorageMappings: TBoldObjectStorageMappingArray;
  AllInstances: TBoldAllInstancesMappingArray;
  i: integer;
begin
  Tables.Clear;
  StorageMappings := Mapping.GetObjectStorageMapping(ExpressionName);
  for i := 0 to length(StorageMappings) - 1 do
    Tables.Add(StorageMappings[i].TableName);
  AllInstances := Mapping.GetAllInstancesMapping(ExpressionName);
  for i := 0 to length(AllInstances) - 1 do
    Tables.Add(AllInstances[i].TableName);
end;

function TBoldIDCompressor.GetBoldSystem: TBoldSystem;
begin
  if Assigned(SystemHandle) then
    result := SystemHandle.System
  else
    result := nil;
end;

function TBoldIDCompressor.GetSQL: TStrings;
begin
  if not Assigned(fSqlStrings) then
    fSqlStrings := TStringList.Create;
  result := fSqlStrings;
end;

procedure TBoldIDCompressor.Log(const AFormat: string);
begin
  Log(AFormat, []);
end;

procedure TBoldIDCompressor.Log(const AFormat: string;
  const Args: array of const);
begin
  if Assigned(fLogStrings) then
    LogStrings.Add(Format(AFormat, Args));
end;

procedure TBoldIDCompressor.SetLogStrings(const Value: TStrings);
begin
  fLogStrings := Value;
end;

procedure TBoldIDCompressor.SetOnProgress(
  const Value: TBoldIDCompressorProgressEvent);
begin
  fOnProgress := Value;
end;

procedure TBoldIDCompressor.UpdateBoldIDTable;
var
  vIdTableDescription: TBoldSQLTableDescription;
  vIDTableName: string;
  vIdColumnName: string;
  vRootTableName: string;
  vRooTableIdColumnName: string;
begin
  vRootTableName := SystemHandle.PersistenceHandleDB.PersistenceControllerDefault.PersistenceMapper.PSSystemDescription.RootTable.SQLName;
  vRooTableIdColumnName := SystemHandle.PersistenceHandleDB.PersistenceControllerDefault.PersistenceMapper.PSSystemDescription.RootTable.ColumnsList[0].SQLName;
  vIDTableDescription := SystemHandle.PersistenceHandleDB.PersistenceControllerDefault.PersistenceMapper.PSSystemDescription.IdTable;
  vIDTableName := vIdTableDescription.SQLName;
  vIdColumnName := vIdTableDescription.ColumnsList[0].SQLName;
  SQL.Add(Format('update %s set %s = (select max(%s) from %s)+1%s', [vIDTableName, vIdColumnName, vIdColumnName, vRootTableName, SystemHandle.PersistenceHandleDB.SQLDatabaseConfig.SqlScriptTerminator]));
end;

end.
