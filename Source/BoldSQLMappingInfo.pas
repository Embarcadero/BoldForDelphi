{ Global compiler directives }
{$include bold.inc}
unit BoldSQLMappingInfo;

interface

uses
  Classes,
  BoldDefs,
  BoldBase,
  BoldIndexableList,
  BoldDbInterfaces,
  BoldTaggedValueSupport;

type
  TBoldSQLMappingInfo = class;
  TBoldSQLMappingInfoClass = class of TBoldSQLMappingInfo;

  TBoldClassMappingInfo = class(TBoldMemoryManagedObject)
  private
    fClassExpressionName: string;
  public
    constructor create(const ClassExpressionName: string);
    property ClassExpressionName: String read fClassExpressionName;
  end;

  TBoldMemberMappingInfo = class(TBoldClassMappingInfo)
  private
    FColumnIndex: Boolean;
    fMemberName: String;
    fTableName: string;
    fColumns: string;
    fMapperName: string;
    function GetColumnByIndex(Index: integer): string;
    function GetColumnCount: integer;
  public
    constructor create(const ClassExpressionName, MemberName, TableName, Columns,
        MapperName: string; const ColumnIndex: Boolean);
    function CompareMapping(Mapping: TBoldMemberMappingInfo): Boolean;
    function CompareType(Mapping: TBoldMemberMappingInfo): Boolean;
    property MemberName: String read fMemberName;
    property TableName: string read fTableName;
    property MapperName: string read fMapperName;
    property Columns: string read fColumns;
    property ColumnByIndex[Index: integer]: string read GetColumnByIndex;
    property ColumnIndex: Boolean read FColumnIndex;
    property ColumnCount: integer read GetColumnCount;
  end;

  TBoldDbTypeMappingInfo = class(TBoldClassMappingInfo)
  private
    fDbType: TBoldDbType;
  public
    constructor create(const ClassExpressionName: string; DbType: TBoldDbType);
    property DbType: TBoldDbType read fDbType;
  end;

  TBoldAllInstancesMappingInfo = class(TBoldClassMappingInfo)
  private
    fClassIdRequired: Boolean;
    fTableName: string;
  public
    constructor create(const ClassExpressionName, TableName: String; ClassIDRequired: boolean);
    function CompareMapping(Mapping: TBoldAllInstancesMappingInfo): Boolean;
    property TableName: string read fTableName;
    property ClassIdRequired: Boolean read fClassIdRequired;
  end;

  TBoldObjectStorageMappingInfo = class(TBoldClassMappingInfo)
  private
    fTableName: string;
  public
    constructor create(const ClassExpressionName, TableName: String);
    function CompareMapping(Mapping: TBoldObjectStorageMappingInfo): Boolean;
    property TableName: string read fTableName;
  end;

  TBoldMemberMappingArray = array of TBoldMemberMappingInfo;
  TBoldObjectStorageMappingArray = array of TBoldObjectStorageMappingInfo;
  TBoldAllInstancesMappingArray = array of TBoldAllInstancesMappingInfo;
  TBoldClassMappingArray = array of TBoldClassMappingInfo;

  TBoldMemberMappingList = class(TBoldIndexableList)
  private
    function GetMappingsByExpressionNames(const ClassExpressionName, MemberName: string): TBoldMemberMappingArray;
    function GetItems(index: integer): TBoldMemberMappingInfo;
  public
    constructor Create;
    procedure FillFromList(SourceList: TBoldMemberMappingList);
    property items[index: integer]: TBoldMemberMappingInfo read GetItems; default;
    property MappingsByExpressionNames[const ClassExpressionName, MemberName: string]: TBoldMemberMappingArray read GetMappingsByExpressionNames;
  end;

  TBoldClassMappingList = class(TBoldIndexableList)
  private
    function GetMappingsForClassName(const ClassExpressionName: string): TBoldClassMappingArray;
  public
    constructor Create;
    procedure AddMapping(Mapping: TBoldClassMappingInfo);
    property MappingsForClassName[const ClassExpressionName: string]: TBoldClassMappingArray read GetMappingsForClassName;
  end;

  TBoldAllInstancesMappingList = class(TBoldClassMappingList)
  private
    function GetItems(index: integer): TBoldAllInstancesMappingInfo;
  public
    property items[index: integer]: TBoldAllInstancesMappingInfo read GetItems; default;
  end;

  TBoldObjectStorageMappingList = class(TBoldClassMappingList)
  private
    function GetItems(index: integer): TBoldObjectStorageMappingInfo;
  public
    property items[index: integer]: TBoldObjectStorageMappingInfo read GetItems; default;
  end;

  TBoldDbTypeMappingList = class(TBoldClassMappingList)
  private
    function GetItems(index: integer): TBoldDbTypeMappingInfo;
  public
    property items[index: integer]: TBoldDbTypeMappingInfo read GetItems; default;
  end;


  TBoldSQLMappingInfo = class
  private
    fHighestUsedDbtype: TBoldDbType;
    fMaxDBIdentifierLength: integer;
    fNationalCharConversion: TBoldNationalCharConversion;
    function GetAllInstancesMappingInfo(index: integer): TBoldAllInstancesMappingInfo;
    function GetMemberMappingInfo(index: integer): TBoldMemberMappingInfo;
    function GetObjectStorageMappingInfo(index: integer): TBoldObjectStorageMappingInfo;
    function GetDbTypeMappingInfo(Index: integer): TBoldDbTypeMappingInfo;
  protected
    fSystemTablePrefix: string;
    fMemberMapping: TBoldMemberMappingList;
    fAllInstancesMapping: TBoldAllInstancesMappingList;
    fObjectStorageMapping: TBoldObjectStorageMappingList;
    fDbTypeMapping: TBoldDbTypeMappingList;
    FCurrentDatabase: IBoldDataBase;
    property MemberMappingInfo[index: integer]: TBoldMemberMappingInfo read GetMemberMappingInfo;
    property AllInstancesMappingInfo[index: integer]: TBoldAllInstancesMappingInfo read GetAllInstancesMappingInfo;
    property ObjectStorageMappingInfo[index: integer]: TBoldObjectStorageMappingInfo read GetObjectStorageMappingInfo;
    property DbTypeMapping[Index: integer]: TBoldDbTypeMappingInfo read GetDbTypeMappingInfo;
    procedure ClearDbTypes;
    procedure ClearMappingInfo;
  public
    constructor Create(const SystemTablePrefix: string; MaxDBIdentifierLength: integer; NationalCharConversion: TBoldNationalCharConversion);
    destructor Destroy; override;
    procedure ReadDataFromDB(DataBase: IBoldDataBase; ReadDbTypeFromDB, ReadMappingFromDB: Boolean); virtual; abstract;
    procedure WriteDataToDB(DataBase: IBoldDataBase);
    procedure ScriptForWriteData(DataBase: IBoldDataBase; Script: TStrings; ClearFirst: Boolean;
        Separator: String; Terminator: String); virtual; abstract;
    function CloneWithoutDbType: TBoldSQLMappingInfo;
    procedure AddMemberMapping(const ClassExpressionName, MemberName, TableName,
        ColumnNames, MapperName: String; const ColumnIndex: Boolean);
    procedure AddAllInstancesMapping(const ClassExpressionName, TableName: String; ClassIdRequired: Boolean);
    procedure AddObjectStorageMapping(const ClassExpressionName, TableName: String);
    Procedure AddTypeIdMapping(const ClassExpressionName: String; DbType: TBoldDbType);

    function GetMemberMappings(const ClassExpressionName, MemberName: String): TBoldMemberMappingArray;
    function GetAllInstancesMapping(const ClassExpressionName: String): TBoldAllInstancesMappingArray;
    function GetObjectStorageMapping(const ClassExpressionName: String): TBoldObjectStorageMappingArray;
    function GetDbTypeMapping(const ClassExpressionName: String): TBoldDbType;
    property HighestUsedDbType: TBoldDbType read fHighestUsedDbtype;
    property MaxDBIdentifierLength: integer read fMaxDBIdentifierLength;
    property NationalCharConversion: TBoldNationalCharConversion read fNationalCharConversion;
    property MemberMappings: TBoldMemberMappingList read fMemberMapping;
    property AllInstancesMappings: TBoldAllInstancesMappingList read fAllInstancesMapping;
    property ObjectStorageMappings: TBoldObjectStorageMappingList read fObjectStorageMapping;
    property DbTypeMappings: TBoldDbTypeMappingList read fDbTypeMapping;
  end;

implementation

uses
  StrUtils,
  SysUtils,

  BoldCoreConsts,
  BoldIndex,
  BoldLogHandler,
  BoldUtils,
  BoldHashIndexes,
  BoldPMapperLists,
  BoldPMappers
  ;

type

  TBoldClassMappingIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  TBoldMemberMappingIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
    function KeyStringForExpressionNames(const ClassExpressionName, MemberName: string): string;
    procedure FindAllByExpressionNames(const ClassExpressionName, MemberName: string; aList: TList);
  end;

var
  IX_ClassExpressionNameIndex: integer = -1;
  IX_ClassAndMemberExpressionNameIndex: integer = -1;

{ TBoldAllInstancesMappingList }

function TBoldAllInstancesMappingList.GetItems(index: integer): TBoldAllInstancesMappingInfo;
begin
  result := (inherited items[index]) as TBoldAllInstancesMappingInfo;
end;

{ TBoldObjectStorageMappingList }

function TBoldObjectStorageMappingList.GetItems(index: integer): TBoldObjectStorageMappingInfo;
begin
  result := (inherited items[index]) as TBoldObjectStorageMappingInfo;
end;

{ TBoldDbTypeMappingList }

function TBoldDbTypeMappingList.GetItems(index: integer): TBoldDbTypeMappingInfo;
begin
  result := (inherited items[index]) as TBoldDbTypeMappingInfo;
end;

{ TBoldDBMappingInfo }

procedure TBoldSQLMappingInfo.AddAllInstancesMapping(const ClassExpressionName, TableName: String; ClassIdRequired: Boolean);
var
  OldMappings: TBoldAllInstancesMappingArray;
  i: integer;
begin
  OldMappings := GetAllInstancesMapping(ClassExpressionName);
  for i := 0 to length(OldMappings)-1 do
    if SameText(OldMappings[i].TableName, TableName) then
    begin
      OldMappings[i].fClassIdRequired := OldMappings[i].ClassIdRequired or ClassIdRequired;
      exit;
    end;
  fAllInstancesMapping.Add(TBoldAllInstancesMappingInfo.Create(ClassExpressionName, TableName, ClassIdRequired));
end;

procedure TBoldSQLMappingInfo.AddMemberMapping(const ClassExpressionName,
    MemberName, TableName, ColumnNames, MapperName: String; const ColumnIndex:
    Boolean);
var
  OldMappings: TBoldMemberMappingArray;
  i: integer;
begin
  OldMappings := GetMemberMappings(ClassExpressionName, MemberName);
  for i := 0 to length(OldMappings)-1 do
    if SameText(OldMappings[i].TableName, TableName) and
      SameText(OldMappings[i].Columns, ColumnNames) then
    begin
      exit;
    end;
  fMemberMapping.Add(TBoldMemberMappingInfo.Create(ClassExpressionName,
      MemberName, TableName, ColumnNames, MapperName, ColumnIndex));
end;

procedure TBoldSQLMappingInfo.AddObjectStorageMapping(const ClassExpressionName, TableName: String);
var
  OldMappings: TBoldObjectStorageMappingArray;
  i: integer;
begin
  OldMappings := GetObjectStorageMapping(ClassExpressionName);
  for i := 0 to length(OldMappings)-1 do
    if SameText(OldMappings[i].TableName, TableName) then
      exit;
  fObjectStorageMapping.Add(TBoldObjectStorageMappingInfo.Create(ClassExpressionName, TableName));
end;

procedure TBoldSQLMappingInfo.ClearMappingInfo;
begin
  fMemberMapping.Clear;
  fAllInstancesMapping.Clear;
  fObjectStorageMapping.Clear;
end;

procedure TBoldSQLMappingInfo.ClearDbTypes;
begin
  fDbTypeMapping.Clear;
end;


constructor TBoldSQLMappingInfo.create(const SystemTablePrefix: string; MaxDBIdentifierLength: integer; NationalCharConversion: TBoldNationalCharConversion);
begin
  inherited create;
  fSystemTablePrefix := SystemTablePrefix;
  fMemberMapping := TBoldMemberMappingList.Create;
  fAllInstancesMapping := TBoldAllInstancesMappingList.create;
  fObjectStorageMapping := TBoldObjectStorageMappingList.create;
  fDbTypeMapping := TBoldDbTypeMappingList.Create;
  fHighestUsedDbType := -1;
  fMaxDBIdentifierLength := MaxDBIdentifierLength;
  fNationalCharConversion := NationalCharConversion;
end;

destructor TBoldSQLMappingInfo.destroy;
begin
  FreeAndNil(fMemberMapping);
  FreeAndNil(fAllInstancesMapping);
  FreeAndNil(fObjectStorageMapping);
  FreeAndNil(fDbTypeMapping);
  inherited;
end;

function TBoldSQLMappingInfo.GetAllInstancesMapping(const ClassExpressionName: String): TBoldAllInstancesMappingArray;
begin
  result := TBoldAllInstancesMappingArray(fAllInstancesMapping.MappingsForClassName[ClassExpressionName]);
end;

function TBoldSQLMappingInfo.GetAllInstancesMappingInfo(
  index: integer): TBoldAllInstancesMappingInfo;
begin
  result := fAllInstancesMapping.Items[index];
end;

function TBoldSQLMappingInfo.GetMemberMappings(const ClassExpressionName, MemberName: String): TBoldMemberMappingArray;
begin
  result := fMemberMapping.MappingsByExpressionNames[ClassExpressionName, MemberName];
end;

function TBoldSQLMappingInfo.GetMemberMappingInfo(
  index: integer): TBoldMemberMappingInfo;
begin
  result := fMemberMapping.Items[index];
end;

function TBoldSQLMappingInfo.GetObjectStorageMapping(const ClassExpressionName: String): TBoldObjectStorageMappingArray;
begin
  result := TBoldObjectStorageMappingArray(fObjectStorageMapping.MappingsForClassName[classExpressionName]);
end;

function TBoldSQLMappingInfo.GetObjectStorageMappingInfo(
  index: integer): TBoldObjectStorageMappingInfo;
begin
  result := fObjectStorageMapping.Items[index];
end;

{ TBoldMemberMappingInfo }


function TBoldMemberMappingInfo.CompareMapping(Mapping: TBoldMemberMappingInfo): Boolean;
begin
  Result := (SameText(TableName, Mapping.TableName) and SameText(Columns, Mapping.Columns)) and CompareType(Mapping);
end;

function TBoldMemberMappingInfo.CompareType(
  Mapping: TBoldMemberMappingInfo): Boolean;
const
  CompatibleDateTypes: array [0..2] of string = ('TBoldPMDateTime', 'TBoldPMDate', 'TBoldPMTime');
  CompatibleStringTypes: array [0..1] of string = ('TBoldPMString', 'TBoldPMAnsiString');
begin
  Result := (MapperName = Mapping.MapperName);
  if not Result then begin
    // This is hardcoded case that should consider Date and DateTime as compatible.
    // The generic way to do this properly would be to compare the actual ColumnTypes that are specified in TBoldSQLDataBaseConfig
    // But the sql config is hard to reach from here...
    Result := ((AnsiIndexText(MapperName, CompatibleDateTypes) <> -1) and
               (AnsiIndexText(Mapping.MapperName, CompatibleDateTypes) <> -1));
  end;
  if not Result then begin
    // Same for String/AnsiString
    Result := ((AnsiIndexText(MapperName, CompatibleStringTypes) <> -1) and
               (AnsiIndexText(Mapping.MapperName, CompatibleStringTypes) <> -1));
  end;
  if not Result then
// If new mapper inherits from old mapper we assume they are compatible
    Result := (BoldMemberPersistenceMappers.DescriptorByDelphiName[MapperName].MemberPersistenceMapperClass.IsCompatibleWith(
      BoldMemberPersistenceMappers.DescriptorByDelphiName[Mapping.MapperName].MemberPersistenceMapperClass));
end;

constructor TBoldMemberMappingInfo.create(const ClassExpressionName,
    MemberName, TableName, Columns, MapperName: string; const ColumnIndex:
    Boolean);
begin
  inherited create(ClassExpressionName);
  fMemberName := MemberName;
  fTableName := TableName;
  fColumns := Columns;
  fMapperName := MapperName;
  FColumnIndex := ColumnIndex;
end;

function TBoldMemberMappingInfo.GetColumnByIndex(Index: integer): string;
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    s.CommaText := Columns;
    result := s[index];
  finally
    s.free;
  end;
end;

function TBoldMemberMappingInfo.GetColumnCount: integer;
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    s.CommaText := Columns;
    result := s.Count;
  finally
    s.free;
  end;
end;

{ TBoldAllInstancesMappingInfo }

function TBoldAllInstancesMappingInfo.CompareMapping(Mapping: TBoldAllInstancesMappingInfo): Boolean;
begin
  result :=
    SameText(TableName, Mapping.TableName) and
    (ClassIdRequired = Mapping.ClassIdRequired);

end;

constructor TBoldAllInstancesMappingInfo.create(const ClassExpressionName,
  TableName: String; ClassIDRequired: boolean);
begin
  inherited Create(ClassExpressionName);
  fTableName := TableName;
  fClassIdRequired := ClassIDRequired;
end;


{ TBoldObjectStorageMappingInfo }

function TBoldObjectStorageMappingInfo.CompareMapping(Mapping: TBoldObjectStorageMappingInfo): Boolean;
begin
  result := SameText(TableName, Mapping.TableName);
end;

constructor TBoldObjectStorageMappingInfo.create(const ClassExpressionName,
  TableName: String);
begin
  inherited Create(ClassExpressionName);
  fTableName := TableName;
end;

{ TBoldClassMappingIndex }

function TBoldClassMappingIndex.ItemAsKeyString(Item: TObject): string;
begin
  result := (Item as TBoldClassMappingInfo).ClassExpressionName;
end;

{ TBoldMemberMappingIndex }

procedure TBoldMemberMappingIndex.FindAllByExpressionNames(const ClassExpressionName,
  MemberName: string; aList: TList);
begin
  FindAllByString(KeyStringForExpressionNames(ClassExpressionName, MemberName), aList);
end;

function TBoldMemberMappingIndex.ItemAsKeyString(Item: TObject): string;
var
  MappingInfo: TBoldMemberMappingInfo;
begin
  MappingInfo := (Item as TBoldMemberMappingInfo);
  result := KeyStringForExpressionNames(MappingInfo.ClassExpressionName, MappingInfo.MemberName);
end;

function TBoldMemberMappingIndex.KeyStringForExpressionNames(const ClassExpressionName, MemberName: string): string;
begin
  result := ClassExpressionName + '.' + MemberName;
end;

{ TBoldMemberMappingList }

constructor TBoldMemberMappingList.Create;
begin
  inherited;
  SetIndexVariable(IX_ClassAndMemberExpressionNameIndex, AddIndex(TBoldMemberMappingIndex.Create));
end;

procedure TBoldMemberMappingList.FillFromList(
  SourceList: TBoldMemberMappingList);
var
  i: integer;
begin
  for i := 0 to SourceList.Count-1 do
    Add(TBoldMemberMappingInfo.Create(
      SourceList[i].ClassExpressionName,
      SourceList[i].MemberName,
      SourceList[i].TableName,
      SourceList[i].Columns,
      Sourcelist[i].MapperName,
      SourceList[i].ColumnIndex));
end;

function TBoldMemberMappingList.GetItems(index: integer): TBoldMemberMappingInfo;
begin
  result := (inherited items[index]) as TBoldmemberMappingInfo;
end;

function TBoldMemberMappingList.GetMappingsByExpressionNames(const ClassExpressionName, MemberName: string): TBoldMemberMappingArray;
var
  aList: TList;
  i: Integer;
begin
  aList := TList.Create;
  try
    (Indexes[IX_ClassAndMemberExpressionNameIndex] as TBoldMemberMappingIndex).FindAllByExpressionNames(ClassExpressionName, MemberName, aList);
    SetLength(result, aList.Count);
    for i := 0 to aList.Count-1 do
      result[i] := aList[i];
  finally
    aList.Free;
  end;
end;

{ TBoldClassMappingList }

procedure TBoldClassMappingList.AddMapping(Mapping: TBoldClassMappingInfo);
begin
  Add(Mapping);
end;

constructor TBoldClassMappingList.Create;
begin
  Inherited;
  SetIndexVariable(IX_ClassExpressionNameIndex, AddIndex(TBoldClassMappingIndex.Create));
end;


function TBoldClassMappingList.GetMappingsForClassName(const ClassExpressionName: string): TBoldClassMappingArray;
var
  aList: TList;
  i: Integer;
begin
  aList := TList.Create;
  try
    (Indexes[IX_ClassExpressionNameIndex] as TBoldStringHashIndex).FindAllByString(ClassExpressionName, aList);
    SetLength(result, aList.Count);
    for i := 0 to aList.Count-1 do
      result[i] := aList[i];
  finally
    aList.Free;
  end;
end;

{ TBoldDbTypeMapping }

constructor TBoldDbTypeMappingInfo.create(const ClassExpressionName: string; DbType: TBoldDbType);
begin
  inherited Create(ClassExpressionName);
  fDbType := DbType;
end;

{ TBoldClassMappingInfo }

constructor TBoldClassMappingInfo.create(
  const ClassExpressionName: string);
begin
  Inherited Create;
  fClassExpressionName := ClassexpressionName;
end;

procedure TBoldSQLMappingInfo.AddTypeIdMapping(const ClassExpressionName: String; DbType: TBoldDbType);
var
  OldMapping: TBoldDbType;
begin
  if ClassExpressionName = '' then
    raise EBoldInternal.CreateFmt('%s.AddTypeIdMapping: ClassExpressionName is empty (dbtype: %d)', [ClassName, dbType]);
  OldMapping := GetDbTypeMapping(ClassExpressionName);
  if (OldMapping <> -1) and (OldMapping <> dbtype) then
    raise EBold.CreateFmt(sMultipleDBTypes, [classname, ClassExpressionName, dbtype, oldMapping]);
  if OldMapping = -1 then
  begin
    fDbTypeMapping.AddMapping(TBoldDbTypeMappingInfo.create(ClassExpressionName, DbType));
    if dbType > fHighestUsedDbType then
      fHighestUsedDbtype := DbType;
  end;
end;

function TBoldSQLMappingInfo.GetDbTypeMapping(const ClassExpressionName: String): TBoldDbType;
var
  MappingArray: TBoldClassMappingArray;
begin
  MappingArray := fDbTypeMapping.GetMappingsForClassName(ClassExpressionName);
  if length(MappingArray) > 0 then
    result := TBoldDbTypeMappingInfo(MappingArray[0]).DbType
  else
    result := NO_CLASS;
end;

function TBoldSQLMappingInfo.GetDbTypeMappingInfo(Index: integer): TBoldDbTypeMappingInfo;
begin
  result := fDbTypeMapping.Items[index];
end;


procedure TBoldSQLMappingInfo.WriteDataToDB(DataBase: IBoldDataBase);
var
  i: integer;
  q: IBoldExecQuery;
  Script: TStringList;
begin
  if BoldLog.ProcessInterruption then
    exit;

  BoldLog.Log(sLogWritingMappingToDB);
  if not Database.Connected then
    Database.Open;
  Database.StartTransaction;
  try
    q := DataBase.GetExecQuery;
    Script := TStringList.create;
    try
      q.ParamCheck := false;
      ScriptForWriteData(DataBase, Script, True, '', '');
      BoldLog.ProgressMax := Script.Count;
      q.StartSQLBatch;
      try
        for i := 0 to Script.Count-1 do
        begin
          q.AssignSQLText(Script[i]);
          q.ExecSQL;
          BoldLog.Progress := i;
        end;
        q.EndSQLBatch;
      except
        q.FailSQLBatch;
        raise;
      end;
    finally
      Script.Free;
      DataBase.ReleaseExecQuery(q);
    end;
  finally
    BoldLog.Separator;
    BoldLog.Log('Committing changes to mapping information');
    Database.Commit;
  end;
end;

function TBoldSQLMappingInfo.CloneWithoutDbType: TBoldSQLMappingInfo;
var
  i: integer;
begin
  result := TBoldSQLMappingInfoClass(ClassType).Create(fSystemTablePrefix, fMaxDBIdentifierLength, fNationalCharConversion);
  for i := 0 to fMemberMapping.Count-1 do
    result.AddMemberMapping(
      MemberMappingInfo[i].ClassExpressionName,
      MemberMappingInfo[i].MemberName,
      MemberMappingInfo[i].TableName,
      MemberMappingInfo[i].Columns,
      MemberMappingInfo[i].MapperName,
      MemberMappingInfo[i].ColumnIndex);

  for i := 0 to fAllInstancesMapping.Count-1 do
    result.AddAllInstancesMapping(AllInstancesMappingInfo[i].ClassExpressionName, AllInstancesMappingInfo[i].TableName, AllInstancesMappingInfo[i].ClassIdRequired);

  for i := 0 to fObjectStorageMapping.Count-1 do
    result.AddObjectStorageMapping(ObjectStorageMappingInfo[i].ClassExpressionName, ObjectStorageMappingInfo[i].TableName);
end;

end.
