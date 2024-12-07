{ Global compiler directives }
{$include bold.inc}
unit BoldDbDataValidator;

interface

uses
  Classes,
  System.Generics.Collections,
  BoldDbValidator,
  BoldPSDescriptionsSQL,
  BoldDbInterfaces,
  BoldPMappers,
  BoldPMappersSQL,
  BoldPMappersDefault,
  BoldPMappersLinkDefault;

type
  { forward declarations }
  TBoldDbDataValidator = class;

  TBoldDBDataValidatorTestType =
   (ttExistenceInParentTest,
    ttExistenceInChildTest,
    ttTypeTest,
    ttDuplicateSingleLinkTest,
    ttLinkObjectDupesTest,
    ttLinkObjectTest,
    ttLinkObjectTest2,
    ttStrayObjectsTest,
    ttSingleSingleEmbeddInconsistencyTest,
    ttRelationTest,
    ttNotNullColumns);

  TBoldDBDataValidatorTestTypes = set of TBoldDBDataValidatorTestType;

  TBoldDbDataValidatorCorruptObjectsAction = (caInsert, caDelete);

  TValidateProc =  procedure (ObjectSQLMapper: TBoldObjectSQLMapper) of object;
  TValidateProcList = TDictionary<TValidateProc, TBoldObjectSQLMapper>;

  { TBoldDbDataValidator }
  TBoldDbDataValidator = class(TBoldDbValidator)
  private
    fValidatorTestTypes: TBoldDBDataValidatorTestTypes;
    fClassesToValidate: string;
  protected
//    procedure DeActivate; override;
    function CreateValidatorThread: TBoldDbValidatorThread; override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure Validate; override;
    property ValidatorTestTypes: TBoldDBDataValidatorTestTypes read fValidatorTestTypes write fValidatorTestTypes;
    property ClassesToValidate: string read fClassesToValidate write fClassesToValidate;
  end;

  TBoldDbDataValidatorThread = class(TBoldDbValidatorThread)
  private
    fTypeTestedTables: TStringList;
    fQuery: IBoldQuery;
    fExistenceInParentTestedTables: TStringList;
    fValidatorTestTypes: TBoldDBDataValidatorTestTypes;
    fClassesToValidate: string;
    fCorruptObjectsAction: TBoldDbDataValidatorCorruptObjectsAction;
    fValidateProcList: TValidateProcList;
    function GetQuery: IBoldQuery;
    procedure SuggestTableInsert(table: TBoldSQLTableDescription; IdList, TypeList: TStrings);
    procedure SuggestTableDelete(Tables: TBoldSQLTableDescriptionList; IdList: TStrings; ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure ClearSingleLink(IdList: TStrings; SingleLink: TBoldEmbeddedSingleLinkDefaultMapper; ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure ClearMultiLink(IdList: TStrings; MultiLink: TBoldIndirectMultiLinkDefaultmapper; ObjectSQLMapper: TBoldObjectSQLMapper);
    function ClearRelationIfNeeded(IdList: TStrings; SingleLink: TBoldEmbeddedSingleLinkDefaultMapper; ObjectSQLMapper: TBoldObjectSQLMapper): boolean;
  protected
    function MemberIsInherited(MemberMapper: TBoldMemberPersistenceMapper): Boolean;
    function Prepare2TableTest(SQLTemplate: String; CheckList: TStringList; args: array of const; table1, table2: String; IdList: TStrings; TypeList: TStrings = nil): Boolean;
    procedure AddRemedyForDeleteObjects(Mapper: TBoldObjectSQLMapper; IdList: TStringList);
    procedure UnlinkFromDeleteObject(IdList: TStrings; ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure OpenQuery;
    property Query: IBoldQuery read GetQuery;
    property TypeTestedTables: TStringList read fTypeTestedTables;
    property ExistenceInParentTestedTables: TStringList read fExistenceInParentTestedTables;
  public
    constructor Create(AValidator: TBoldDbValidator); override;
    destructor Destroy; override;
    procedure ValidateExistence(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure ValidateStrayObjects(ObjectDefaultMapper: TBoldObjectDefaultMapper);
    procedure ValidateRelations(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure ValidateNotNullColumns(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure ValidateNotNullForColumn(BoldSQLColumnDescription: TBoldSQLColumnDescription);
    procedure ValidateLinkObjectDupes(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure ValidateLinkObjects(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure Validate; override;
    property ValidatorTestTypes: TBoldDBDataValidatorTestTypes read fValidatorTestTypes write fValidatorTestTypes;
    property CorruptObjectsAction: TBoldDbDataValidatorCorruptObjectsAction read fCorruptObjectsAction write fCorruptObjectsAction;
    property ClassesToValidate: string read fClassesToValidate write fClassesToValidate;
  end;

implementation

uses
  DB,
  SysUtils,

  BoldCoreConsts,
  BoldSQLMappingInfo,
  BoldPSDescriptionsDefault,
  BoldLogHandler,
  BoldDefs,
  BoldUtils,
  BoldMath,
  BoldGuard;

const
  Field_BOLD_ID = 'BOLD_ID';
  Field_BOLD_TYPE = 'BOLD_TYPE';

  ExistenceInParentTest: String =
    'SELECT OWN.BOLD_ID, OWN.BOLD_TYPE ' + BOLDCRLF +
    'FROM %s OWN ' + BOLDCRLF +
    'WHERE (OWN.BOLD_TYPE IN (%d)) AND NOT EXISTS(' + BOLDCRLF +
    '  SELECT PARENT.BOLD_ID ' + BOLDCRLF +
    '  FROM %s PARENT ' + BOLDCRLF +
    '  WHERE (PARENT.BOLD_TYPE IN (%d)) AND (PARENT.BOLD_ID = OWN.BOLD_ID))';
// [Own, Type, Parent]

  ExistenceInChildTest: string =
    'SELECT PARENT.BOLD_ID, PARENT.BOLD_TYPE ' + BOLDCRLF +
    'FROM %s PARENT' + BOLDCRLF +
    'WHERE (PARENT.BOLD_TYPE IN (%d)) AND ' + BOLDCRLF +
    '      NOT EXISTS(' + BOLDCRLF +
    '          SELECT BOLD_ID' + BOLDCRLF +
    '          FROM %s OWN ' + BOLDCRLF +
    '          WHERE (OWN.BOLD_TYPE IN (%d)) AND (OWN.BOLD_ID = PARENT.BOLD_ID))';
// [Parent, Type, Own, Type]


  TypeTest: String =
    'SELECT PARENT.BOLD_ID, PARENT.BOLD_TYPE, OWN.BOLD_TYPE' + BOLDCRLF +
    'FROM %s PARENT, %s OWN' + BOLDCRLF +
    'WHERE (PARENT.BOLD_ID = OWN.BOLD_ID) AND' + BOLDCRLF +
    '      (PARENT.BOLD_TYPE <> OWN.BOLD_TYPE)';
// [Parent, Own]

// find all objects that are related, but the other end does not exist.

  RelationTest: String =
    'SELECT OWN.%s, OWN.BOLD_ID' + BOLDCRLF +
    'FROM %s OWN' + BOLDCRLF +
    'WHERE OWN.%0:s <> -1 AND NOT EXISTS (' + BOLDCRLF +
    '  SELECT RELATED.BOLD_ID' + BOLDCRLF +
    '  FROM %2:s RELATED' + BOLDCRLF +
    '  WHERE RELATED.BOLD_ID = OWN.%0:s)';
// [Link, OwnTable, RelatedTable]

  DuplicateSingleLinkTest: String =
    'SELECT %0:s' + BOLDCRLF +
    'FROM %s' + BOLDCRLF +
    'WHERE %0:s <> -1 ' + BOLDCRLF +
    'GROUP BY %0:s' + BOLDCRLF +
    'HAVING COUNT(*) > 1';

  LinkObjectDupesTest: String =
    'SELECT LINKTABLE.BOLD_ID, LINKTABLE.%s, LINKTABLE.%s, COUNT(*)' + BOLDCRLF +
    'FROM %s LINKTABLE' + BOLDCRLF +
    'GROUP BY LINKTABLE.%0:s, LINKTABLE.%1:s' + BOLDCRLF +
    'HAVING COUNT(*) >= 2' + BOLDCRLF +
    'ORDER BY %0:s, %1:s';
// [Link1, link2, linktable]

// linkobjects with empty ends
  LinkObjectTest: String =
    'SELECT BOLD_ID' + BOLDCRLF +
    'FROM %s' + BOLDCRLF +
    'WHERE %s = -1 or %s = -1';

// Linkobjects pointing to nonexisting objects
  LinkObjectTest2: String =
    'SELECT LT.BOLD_ID' + BOLDCRLF +
    'FROM %s LT' + BOLDCRLF +
    'WHERE NOT EXISTS (SELECT T.BOLD_ID FROM %s T WHERE LT.%s = T.BOLD_ID)';
//[LinkTable, OtherTable, LinkColumn]

  StrayObjectsTest: string =
    'SELECT BOLD_ID, BOLD_TYPE ' + BOLDCRLF +
    'FROM %s' + BOLDCRLF +
    'WHERE NOT BOLD_TYPE IN (%s)';

  SingleSingleEmbeddInconsistencyTest: String =
    'SELECT T1.BOLD_ID, T1.BOLD_TYPE ' + BOLDCRLF +
    'FROM %s T1, %s T2 ' + BOLDCRLF +
    'WHERE (T1.%s = T2.BOLD_ID) and (T2.%s <> T1.BOLD_ID)';
// [Table1, table2, link1, link2Own, Parent]

{ TBoldDbDataValidator }

destructor TBoldDbDataValidatorThread.Destroy;
begin
  FreeAndNil(fTypeTestedTables);
  FreeAndNil(fExistenceInParentTestedTables);
  if assigned(fQuery) then
    Database.ReleaseQuery(fQuery);
  inherited;
end;

function TBoldDbDataValidatorThread.MemberIsInherited(
  MemberMapper: TBoldMemberPersistenceMapper): Boolean;
begin
  result := false;
  if Assigned(MemberMapper.ObjectPersistenceMapper.SuperClass) then
    result := MemberMapper.MemberIndex < MemberMapper.ObjectPersistenceMapper.SuperClass.MemberPersistenceMappers.count;
end;

procedure TBoldDbDataValidatorThread.OpenQuery;
begin
  Query.Open;
end;

function TBoldDbDataValidatorThread.Prepare2TableTest(SQLTemplate: String; CheckList: TStringList; args: array of const; table1, table2: String; IdList: TStrings; TypeList: TStrings = nil): Boolean;
var
  BoldIdField: IBoldField;
  BoldTypeField: IBoldField;
begin
  if not assigned(CheckList) or
  ((CheckList.IndexOf(Table1 + ':' + Table2) = -1) and
   (CheckList.IndexOf(Table2 + ':' + Table1) = -1)) then
  begin
    IdList.Clear;
    if assigned(typelist) then
      typeList.Clear;
    Query.Close;
    Query.AssignSQLText(format(SQLTemplate, args));
    OpenQuery;
    BoldIdField := Query.FieldByName(Field_BOLD_ID);
    BoldTypeField := Query.FieldByName(Field_BOLD_TYPE);
    while not query.eof do
    begin
      IdList.Add(BoldIdField.AsString);
      if assigned(Typelist) then
        TypeList.Add(BoldTypeField.AsString);
      Query.Next;
    end;
    Query.Close;
    if assigned(CheckList) then
      CheckList.Add(Table1 + ':' + Table2);
    result := IdList.Count > 0;
  end
  else
    result := false;
end;

procedure TBoldDbDataValidatorThread.Validate;
var
  ObjectPMapper: TBoldObjectSQLMapper;
  fCurrentTable: IBoldTable;
begin
  fCurrentTable := Database.GetTable;
  try
    repeat
      ObjectPMapper := Validator.TableQueue.Dequeue as TBoldObjectSQLMapper;
      if not Assigned(ObjectPMapper) then
        break;
      if DoCheckStop then exit;
      BoldLog.LogHeader := Format(sCheckingTable, [ObjectPMapper.ExpressionName]);
      BoldLog.ProgressMax := 100;
      BoldLog.Progress := 0;
      ValidateExistence(ObjectPMapper);
      BoldLog.Progress := 20;
      ValidateRelations(ObjectPMapper);
      BoldLog.Progress := 60;
      ValidateNotNullColumns(ObjectPMapper);
      BoldLog.Progress := 80;
      if ObjectPmapper is TBoldObjectDefaultMapper then
        ValidateStrayObjects(ObjectPMapper as TBoldObjectDefaultMapper);
      if ObjectPMapper.IsLinkClass then
      begin
        ValidateLinkObjects(ObjectPmapper);
      end;
    until Validator.TableQueue.Empty;
  finally
    Database.ReleaseTable(fCurrentTable);
  end;
end;

procedure TBoldDbDataValidatorThread.SuggestTableDelete(
  Tables: TBoldSQLTableDescriptionList; IdList: TStrings; ObjectSQLMapper: TBoldObjectSQLMapper);
var
  s: string;
  FetchBlockSize: integer;
  i: integer;
  Block, Start,Stop: integer;
  j: integer;
  BlockIdList: TStringList;
begin
  BoldLog.LogFmt('Clearing links for %d instances of %s', [IdList.Count, ObjectSQLMapper.ExpressionName]);
  FetchBlockSize := SystemSQLMapper.SQLDataBaseConfig.FetchBlockSize;
  BlockIdList := TStringList.Create;
  try
    for Block := 0 to (IdList.Count div FetchBlockSize) do
    begin
      BlockIdList.Clear;
      Start := Block * FetchBlockSize;
      Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), IdList.Count-1]);
      for j := Start to Stop do
        BlockIdList.Add(IdList[j]);
      for i := 0 to Tables.Count - 1 do
      begin
        Query.Close;
        Query.AssignSQLText(format('SELECT * FROM %s WHERE BOLD_ID IN (%s)', [Tables[i].SQLName, BlockIdList.CommaText]));
        OpenQuery;
        if not query.eof then
        begin
          s := format('DELETE FROM %s WHERE BOLD_ID IN (%s);', [Tables[i].SQLName, BlockIdList.CommaText]);
          AddRemedy(s);
        end;
      end;
      UnlinkFromDeleteObject(BlockIdList, ObjectSQLMapper);
    end;
  finally
    BlockIdList.free;
  end;
end;

procedure TBoldDbDataValidatorThread.SuggestTableInsert(table: TBoldSQLTableDescription; IdList, TypeList: TStrings);
var
  j: integer;
  Skip: Boolean;
  remedySQL: String;
  UnsupportedColumns, NotNullColumns, NotNullValues: string;
  Column: TBoldSQLColumnDescription;
begin
  BoldLog.LogFmt('Generating Inserts for %s', [Table.SQlName]);
  NotNullColumns := '';
  NotNullValues := '';
  UnsupportedColumns := '';

  Skip := false;
  for j := 2 to Table.ColumnsList.Count - 1 do
  begin
    Column := Table.ColumnsList[j] as TBoldSQLColumnDescription;
    if Column.Mandatory then
    begin
      NotNullColumns := NotNullColumns + ', ' + Column.SQLName;
      case Column.FieldType of
        ftString, ftMemo, ftFixedChar, ftWideString, ftBlob:
          NotNullValues := NotNullvalues + ', ''''';
        ftLargeint, ftBCD, ftCurrency, ftFloat, ftSmallint, ftInteger, ftWord :
          NotNullvalues := NotNullvalues + ', 0';
        ftBoolean: NotNullValues := NotNullValues + ', false';
        ftDate: NotNullValues := NotNullValues + ', ' + DateToStr(0.0);
        ftTime: NotNullValues := NotNullValues + ', ' + TimeToStr(0.0);
        ftDateTime: NotNullValues := NotNullValues + ', ' + DateTimeToStr(0.0);
        else
        begin
          skip := true;
          NotNullValues := NotNullValues + ', <XXX>';
          if UnsupportedColumns <> '' then
            UnsupportedColumns := UnsupportedColumns + ', ';
          UnsupportedColumns := UnsupportedColumns + Column.SQLName;
        end;
      end;
    end;
  end;

  remedySQL := 'INSERT INTO %s (BOLD_ID, BOLD_TYPE%s) VALUES (%s, %s%s)'+ PersistenceHandle.SQLDataBaseConfig.SqlScriptSeparator;
  if skip then
  begin
    AddRemedy(format(sColumnsHaveUnsupportedType, [UnsupportedColumns]));
    remedySQL := '-- '+ remedySQL;
  end
  else
    AddRemedy(Format(sAddMissingEntries, [Table.SQLName]));

  for j := 0 to idlist.count - 1 do
    AddRemedy(format(remedySQL, [Table.SQLName, NotNullColumns, IdList[j], TypeList[j], NotNullValues]));
end;

procedure TBoldDbDataValidatorThread.ValidateExistence(ObjectSQLMapper: TBoldObjectSQLMapper);
var
  i: integer;
  IdList, TypeList: TStringList;
  SubTable, SuperTable: String;
  OwnMapping, ParentMapping: TBoldAllInstancesMappingArray;
  Tables: TBoldSQLTableDescriptionList;
  TempMapper: TBoldObjectSQLMapper;
begin
  BoldLog.LogHeader := Format('ValidateExistence %s', [ObjectSQLMapper.ExpressionName]);
  SetLength(OwnMapping, 0);
  SetLength(ParentMapping, 0);
  IdList := TStringList.Create;
  TypeList := TStringList.Create;
  Tables := TBoldSQLTableDescriptionList.Create(ObjectSQlMapper.SystemPersistenceMapper.PSSystemDescription);
  try
    Tables.OwnsEntries := false;

    tempMapper := ObjectSQLMapper;
    while assigned(TempMapper) do
    begin
      if (tempMapper.Maintable<>nil) and (Tables.IndexOf(tempMapper.Maintable) = -1) then
        Tables.Add(tempMapper.Maintable);
      TempMapper := TempMapper.Superclass as TBoldObjectSQLMapper;
    end;

    for i := 0 to Tables.count-2 do
    begin
      if Tables[i]=nil then
      begin
        BoldLog.LogFmt('TBoldDbDataValidatorThread.ValidateExistence: %s Tables[%d]=nil', [ObjectSQLMapper.ExpressionName, I]);
        Continue;
      end;
      if Tables[i+1]=nil then
      begin
        BoldLog.LogFmt('TBoldDbDataValidatorThread.ValidateExistence: %s Tables[%d+1]=nil', [ObjectSQLMapper.ExpressionName, I]);
        Continue;
      end;
      SubTable := Tables[i].SQLName;
      SuperTable := Tables[i+1].SQLName;
      if (ttExistenceInParentTest in fValidatorTestTypes) then
      if Prepare2TableTest(ExistenceInParentTest, nil {ExistenceInParentTestedTables},
        [SubTable, ObjectSQLMapper.BoldDbType, SuperTable, ObjectSQLMapper.BoldDbType], SubTable, SuperTable, IdList, TypeList) then
      begin
        BoldLog.LogFmt(sLogObjectsMissingInParentTable, [SubTable, SuperTable], ltWarning);
        Boldlog.Log(IdList.CommaText, ltDetail);
        AddRemedy(Format('-- Corrupt object of class %s missing parent entry in %s, Ids: %s.', [ObjectSQlMapper.ExpressionName, SuperTable, IdList.CommaText]));
        if CorruptObjectsAction = caInsert then
          SuggestTableInsert(Tables[i+1], IdList, TypeList)
        else
          SuggesttableDelete(Tables , IdList, ObjectSQLMapper);
      end;
      if (ttTypeTest in fValidatorTestTypes) then
      if Prepare2TableTest(TypeTest, TypeTestedTables, [SuperTable, SubTable], SuperTable, SubTable, IdList) then
      begin
        BoldLog.LogFmt(sLogObjectsHaveDifferentType, [SuperTable, SubTable], ltWarning);
        Boldlog.Log(IdList.CommaText, ltDetail);
      end;
    end;
{
    if (ObjectSQLMapper is TBoldObjectDefaultMapper) and assigned(ObjectSQlMapper.SuperClass) then
    begin
      OwnMapping := SystemSQLMapper.MappingInfo.GetAllInstancesMapping(ObjectSQlMapper.ExpressionName);
      ParentMapping := SystemSQLMapper.MappingInfo.GetAllInstancesMapping(ObjectSQlMapper.SuperClass.ExpressionName);

      if (length(OwnMapping) = 1) and (length(ParentMapping) = 1) and
        (OwnMapping[0].tableName <> ParentMapping[0].TableName) then
      begin
        SuperTable := ParentMapping[0].TableName;
        SubTable := OwnMapping[0].TableName;
        if (ttExistenceInChildTest in fValidatorTestTypes) then
        if Prepare2TableTest(ExistenceInChildTest, nil, [
            SuperTable,
            ObjectSQLMapper.BoldDbType,
            SubTable,
            ObjectSQLMapper.BoldDbType], SubTable, SuperTable, IdList, TypeList) then
        begin
          BoldLog.LogFmt(sLogObjectsMissingInChildtable, [SuperTable, SubTable], ltWarning);
          Boldlog.Log(IdList.CommaText, ltDetail);
          if CorruptObjectsAction = caInsert then
            SuggesttableInsert(ObjectSQLMapper.MainTable, IdList, TypeList)
          else
            SuggesttableDelete(Tables , IdList, ObjectSQLMapper);
        end;
      end;
    end;
}
    for i:= Tables.Count-1 downto 1 do
    begin
      SuperTable := Tables[i].SQLName;
      SubTable := Tables[i-1].SQLName;
      if (ttExistenceInChildTest in fValidatorTestTypes) then
      if Prepare2TableTest(ExistenceInChildTest, nil, [
          SuperTable,
          ObjectSQLMapper.BoldDbType,
          SubTable,
          ObjectSQLMapper.BoldDbType], SubTable, SuperTable, IdList, TypeList) then
      begin
        BoldLog.LogFmt('The following objects exists in %s, but not in child table %s', [SuperTable, SubTable], ltWarning);
        Boldlog.Log(IdList.CommaText, ltDetail);
        AddRemedy(Format('-- Corrupt object of class %s missing child entry in %s, Ids: %s.', [ObjectSQlMapper.ExpressionName, SubTable, IdList.CommaText]));
        if CorruptObjectsAction = caInsert then
          SuggesttableInsert(ObjectSQLMapper.MainTable, IdList, TypeList)
        else
          SuggesttableDelete(Tables , IdList, ObjectSQLMapper);
      end;
    end;

  finally
    tables.free;
    IdList.Free;
    TypeList.free;
  end;
end;

procedure TBoldDbDataValidatorThread.ValidateLinkObjectDupes(
  ObjectSQLMapper: TBoldObjectSQLMapper);
var
  LastLInk1, LastLInk2, Link1, Link2: Integer;
  LinkTable,
  LinkColumn1, LinkColumn2: String;
  IdList: TStringList;
  Link1Field: IBoldField;
  Link2Field: IBoldField;
  BoldIdField: IBoldField;
begin
  if not (ttLinkObjectDupesTest in fValidatorTestTypes) then
    exit;
  IdList := TStringList.create;
  BoldLog.LogHeader := Format('ValidateLinkObjectDupes %s.%s', [ObjectSQLMapper.ExpressionName]);
  LinkColumn1 := (ObjectSQLMapper.LinkClassRole1 as TBoldEmbeddedSingleLinkDefaultMapper).MainColumnName;
  LinkColumn2 := (ObjectSQLMapper.LinkClassRole2 as TBoldEmbeddedSingleLinkDefaultMapper).MainColumnName;
  LinkTable := ObjectSQLMapper.MainTable.SQLName;
  Query.Close;
  Query.AssignSQLText(format(LinkObjectDupesTest, [LInkColumn1, LinkColumn2, LInkTable]));
  OpenQuery;
  LastLink1 := -Maxint;
  LastLInk2 := -MaxInt;
  Link1Field := Query.FieldByName(LinkColumn1);
  Link2Field := Query.FieldByName(LinkColumn2);
  BoldIdField := Query.FieldByName('BOLD_ID');
  while not query.eof do
  begin
    link1 := Link1Field.AsInteger;
    link2 := Link2Field.AsInteger;
    if (Link1 = LastLink1) and (link2 = lastlink2) then
      IdList.Add(BoldIdField.AsString)
    else
    begin
      LastLInk1 := link1;
      LastLink2 := Link2;
    end;
  end;
  if IdList.Count > 0 then
  begin
    BoldLog.Log(sLogLinkObjectsAreDupes, ltWarning);
    BoldLog.Log(IdList.CommaText, ltDetail);
  end;
  IdList.Free;
end;

procedure TBoldDbDataValidatorThread.ValidateLinkObjects(ObjectSQLMapper: TBoldObjectSQLMapper);
var
  LinkTable,
  LinkColumn1, LinkColumn2: String;
  IdList: TStringList;

  procedure CheckSpacePointers(LinkMApper: TBoldEmbeddedSingleLInkDefaultMapper);
  var
    MainTableName: string;
    TempIdList: TStringList;
    s: string;
    BoldIdField: IBoldField;
  begin
    if LinkMapper=nil then
    begin
      BoldLog.LogFmt('TBoldDbDataValidatorThread.ValidateLinkObjects->CheckSpacePointers: %s: LinkMapper=nil', [ObjectSQLMapper.ExpressionName]);
      Exit;
    end;
    Assert(PersistenceHandle.BoldModel.MoldModel.GetClassByName(LinkMapper.OtherEndObjectMapper.ExpressionName) <> nil, 'Class ' + LinkMapper.ObjectPersistenceMapper.ExpressionName + ' not found.');
    if LInkMapper.OtherEndObjectMapper=nil then
    begin
      BoldLog.LogFmt('TBoldDbDataValidatorThread.ValidateLinkObjects->CheckSpacePointers: %s: LinkMapper.OtherEndObjectMapper=nil', [ObjectSQLMapper.ExpressionName]);
      Exit;
    end;
    if LInkMapper.OtherEndObjectMapper.Maintable<>nil then
      MainTableName := LinkMapper.OtherEndObjectMapper.Maintable.SQLName
    else
    begin
      MainTableName := SystemSQLMapper.RootClassObjectPersistenceMapper.MainTable.SQLName; // it would be more efficient to find first concrete superclass so that we search more concrete table instead of all in Bold_Object
      if not PersistenceHandle.BoldModel.MoldModel.GetClassByName(LinkMapper.OtherEndObjectMapper.ExpressionName).IsAbstract or not LinkMapper.OtherEndObjectMapper.HasSubClasses then
        BoldLog.LogFmt('TBoldDbDataValidatorThread.ValidateLinkObjects->CheckSpacePointers: %s: Link: %s LinkMapper.OtherEndObjectMapper.Maintable=nil, substituting &s', [ObjectSQLMapper.ExpressionName, LinkMapper.MainColumnName, MainTableName]);
    end;
    Query.Close;
    if (ttLinkObjectTest2 in fValidatorTestTypes) then
    begin
      Query.AssignSQLText(format(LinkObjectTest2, [
        LinkTable,
        MainTableName,
        LinkMapper.MainColumnName]));
      OpenQuery;
      BoldIdField := Query.FieldByName('BOLD_ID');
      TempIdList := TStringList.Create;
      try
        while not query.eof do
        begin
          s := BoldIdField.AsString;
          if IdList.IndexOf(s) = -1 then
          begin
            TempIdList.Add(s);
            IdList.Add(s);
          end;
          Query.Next;
        end;
        if TempIdList.Count > 0 then begin
          BoldLog.LogFmt(sLogLinkObjectsLinkUnexistingObjects, [ObjectSQLmapper.ExpressionName], ltWarning);
          BoldLog.Log(TempIdList.CommaText, ltDetail);
          AddRemedy(Format(sCommentRemoveSpaceLinkObjects, [ObjectSQLmapper.ExpressionName]));
          AddRemedyForDeleteObjects(ObjectSQLMapper, IDList);
        end;
      finally
        TempIdList.free;
      end;
    end;
  end;

var
  BoldIdField: IBoldField;
begin
  if not (ttLinkObjectTest in fValidatorTestTypes) then
    exit;
  BoldLog.LogHeader := Format('ValidateLinkObjects %s', [ObjectSQLMapper.ExpressionName]);
  IdList := TStringList.create;
  LinkColumn1 := (ObjectSQLMapper.LinkClassRole1 as TBoldEmbeddedSingleLinkDefaultMapper).MainColumnName;
  LinkColumn2 := (ObjectSQLMapper.LinkClassRole2 as TBoldEmbeddedSingleLinkDefaultMapper).MainColumnName;
  LinkTable := ObjectSQLMapper.MainTable.SQLName;
  Query.Close;
  Query.AssignSQLText(format(LinkObjectTest, [LinkTable, LinkColumn1, LinkColumn2]));
  OpenQuery;
  BoldIdField := Query.FieldByName('BOLD_ID');
  while not query.eof do
  begin
    IdList.Add(BoldIdField.AsString);
    Query.Next;
  end;
  if IdList.Count > 0 then begin
    BoldLog.LogFmt(sLogBrokenLinkObjects, [ObjectSQLmapper.ExpressionName], ltWarning);
    BoldLog.Log(IdList.CommaText, ltDetail);
    AddRemedy(Format(sCommentRemoveBrokenLinkObjects, [ObjectSQLmapper.ExpressionName]));
    AddRemedyForDeleteObjects(ObjectSQLMapper, IdLIst);
  end;

  CheckSpacePointers(ObjectSQLMapper.LinkClassRole1 as TBoldEmbeddedSingleLinkDefaultMapper);
  CheckSpacePointers(ObjectSQLMapper.LinkClassRole2 as TBoldEmbeddedSingleLinkDefaultMapper);

  IdList.Free;
end;

procedure TBoldDbDataValidatorThread.ValidateNotNullForColumn(
  BoldSQLColumnDescription: TBoldSQLColumnDescription);
var
  FieldNames: TStrings;
  TableName: String;
  ColumnName: String;
  NullCount: integer;
begin
  if not (ttNotNullColumns in fValidatorTestTypes) then
    exit;
  BoldLog.LogHeader := Format('ValidateNotNullForColumn %s.%s', [BoldSQLColumnDescription.TableDescription.SQLName, BoldSQLColumnDescription.SQLName]);
  FieldNames := TStringList.Create;
  query.Close;
  TableName := BoldSQLColumnDescription.tableDescription.SQLName;
  ColumnName := BoldSQLColumnDescription.SQLName;
  try
    Query.AssignSQLText(Format('SELECT count(*) FROM %s WHERE %s.%s IS NULL',
                               [TableName,
                                TableName,
                                ColumnName]));
    OpenQuery;
    nullcount := Query.Fields[0].AsInteger;
    Query.Close;
    if NullCount <> 0 then
    begin
      BoldLog.LogFmtIndent(sNullValuesFound, [NullCount, tableName, ColumnName], ltWarning);
      AddRemedy(Format('UPDATE %s SET %s = <initial value> WHERE %1:s IS NULL;',
                        [TableName, ColumnName]));
    end;
  finally
    FieldNames.Free;
  end;
end;

procedure TBoldDbDataValidatorThread.ValidateNotNullColumns(
  ObjectSQLMapper: TBoldObjectSQLMapper);
var
  i, j: integer;
  BoldSQLTableDescription: TBoldSQLTableDescription;
  BoldSQLColumnDescription: TBoldSQLColumnDescription;
begin
  if not (ttNotNullColumns in fValidatorTestTypes) then
    exit;
  for j := 0 to ObjectSQLMapper.AllTables.Count-1 do
  begin
    BoldSQLTableDescription := ObjectSQLMapper.AllTables[j];
    for i := 0 to BoldSQLTableDescription.ColumnsList.count - 1 do
     begin
      BoldSQLColumnDescription := BoldSQLTableDescription.ColumnsList[i] as TBoldSQlColumnDescription;
      if BoldSQLColumnDescription.Mandatory then
        ValidateNotNullForColumn(BoldSQLColumnDescription);
     end;
  end;
end;

procedure TBoldDbDataValidatorThread.ValidateRelations(ObjectSQLMapper: TBoldObjectSQLMapper);
var
  i,j: integer;
  IdList: TStringList;
  SingleLink: TBoldEmbeddedSingleLinkDefaultMapper;
  RelatedTable,
  Linktable: TBoldSQLTableDescription;
  ClassOfOtherEnd: TBoldObjectSQlMapper;
  MemberOfOtherEnd: TBoldMemberSQLMapper;
  SingleRoleOfOtherEnd: TBoldEmbeddedSingleLInkDefaultMapper;
  MemberMappings: TBoldMemberMappingArray;
  BoldIdField: IBoldField;
  FetchBlockSize: integer;
  Block, Start,Stop: integer;
  s: string;
begin
  IdList := TStringList.create;
  LinkTable := ObjectSQLMapper.MainTable;
  MemberMappings := nil;
  BoldLog.ProgressMax := ObjectSQLMapper.MemberPersistenceMappers.count;
  if not assigned(LinkTable) then
    exit;
  for i := 0 to ObjectSQLMapper.MemberPersistenceMappers.count-1 do
  begin
    if ObjectSQLMapper.MemberPersistenceMappers[i] is TBoldEmbeddedSingleLinkDefaultMapper then
    begin
      SingleLink := ObjectSQLMapper.MemberPersistenceMappers[i] as TBoldEmbeddedSingleLinkDefaultMapper;
      MemberMappings := ObjectSQLMapper.SystemPersistenceMapper.MappingInfo.GetMemberMappings(ObjectSQLMapper.ExpressionName, SingleLink.ExpressionName);
      BoldLog.LogHeader := Format('ValidateRelation %s.%s', [ObjectSQLMapper.ExpressionName, SingleLink.ExpressionName]);
      BoldLog.Progress := i;
      if (length(MemberMappings) = 1) and
         SameText(MemberMappings[0].TableName, LinkTable.SQLName) then
      begin
        ClassOfOtherEnd := SystemSQLMapper.ObjectPersistenceMappers[SingleLink.OtherEndObjectPMIndex] as TBoldObjectSQlMapper;
        RelatedTable := ClassOfOtherEnd.MainTable;
        if RelatedTable = nil then  //childmapped abstract class
          RelatedTable := ObjectSQLMapper.SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable;
        if SingleLink=nil then
        begin
          BoldLog.LogFmt('TBoldDbDataValidatorThread.ValidateRelations: %s:%d SingleLink=nil', [ObjectSQLMapper.ExpressionName, I]);
          Continue;
        end;
        if LinkTable=nil then
        begin
          BoldLog.LogFmt('TBoldDbDataValidatorThread.ValidateRelations: %s:%d LinkTable=nil', [ObjectSQLMapper.ExpressionName, I]);
          Continue;
        end;
        if RelatedTable=nil then
        begin
          BoldLog.LogFmt('TBoldDbDataValidatorThread.ValidateRelations: %s:%d RelatedTable=nil', [ObjectSQLMapper.ExpressionName, I]);
          Continue;
        end;
        Query.Close;
        if (ttRelationTest in fValidatorTestTypes) then
        begin
          Query.AssignSQLText(format(RelationTest, [SingleLink.MainColumnName, LInkTable.SQLName, relatedtable.SQLName]));
          OpenQuery;
          BoldIdField := Query.FieldByName('BOLD_ID');
          IdList.Clear;
          while not query.eof do
          begin
            IdList.Add(BoldIdField.AsString);
            Query.Next;
          end;
          Query.Close;
          if IdList.Count > 0 then
          begin
            BoldLog.LogFmt(sLogObjectsWithBrokenLinks, [ObjectSQlMapper.ExpressionName, IdList.Count, SingleLink.ExpressionName], ltWarning);
            FetchBlockSize := SystemSQLMapper.SQLDataBaseConfig.FetchBlockSize;
            for Block := 0 to (IdList.Count div FetchBlockSize) do
            begin
              s := '';
              Start := Block * FetchBlockSize;
              Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), IdList.Count-1]);
              for j := Start to Stop do
              begin
                s := s + IdList[j];
                if j < stop then
                  s := s + ',';
              end;
              BoldLog.Log(s, ltDetail);
              AddRemedy(Format(sCommentCleanRelation, [ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]));
              AddRemedy(format('UPDATE %s SET %s = -1 WHERE BOLD_ID IN (%s);',
                [LinkTable.SQLName, SingleLink.MainColumnName, s]));
            end;
          end;
        end;

        ClassOfOtherEnd := ObjectSQLMapper.SystemPersistenceMapper.ObjectPersistencemappers[SingleLInk.OtherEndObjectPMIndex] as TBoldObjectSQlMapper;
        if assigned(ClassOfOtherEnd) then
        begin
          MemberOfOtherEnd := ClassOfOtherEnd.MemberPersistenceMappers[SingleLink.OtherEndMemberPMIndex] as TBoldMemberSQLMapper;
          if (ttDuplicateSingleLinkTest in fValidatorTestTypes) then
          if (MemberofOtherEnd is TBoldSingleLinkDefaultMapper) and (not ObjectSQlMapper.IsLinkClass) then
          begin
            BoldLog.LogFmt('Validating 1-1 singlelinks for duplicate values: %s.%s',[ObjectSQlMapper.MainTable.SQlName, SingleLink.MainColumnName]);
            Query.Close;
            Query.AssignSQLText(format(DuplicateSingleLinkTest, [
                SingleLink.MainColumnName, ObjectSQlMapper.MainTable.SQlName, SingleLink.MainColumnName,
                SingleLink.MainColumnName]));
            OpenQuery;
            BoldIdField := Query.FieldByName(SingleLink.MainColumnName);
            IdList.Clear;
            while not query.eof do
            begin
              IdList.Add(BoldIdField.AsString);
              Query.Next;
            end;
            Query.Close;
            while not query.eof do
            begin
              IdList.Add(BoldIdField.AsString);
              Query.Next;
            end;
            Query.Close;
            if IdList.Count > 0 then
            begin
              BoldLog.LogFmt('The following %d objects of class %s have duplicate values in singlelinks (%s) :',
               [IdList.Count, ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]);
              FetchBlockSize := SystemSQLMapper.SQLDataBaseConfig.FetchBlockSize;
              for Block := 0 to (IdList.Count div FetchBlockSize) do
              begin
                s := '';
                Start := Block * FetchBlockSize;
                Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), IdList.Count-1]);
                for j := Start to Stop do
                begin
                  s := s + IdList[j];
                  if j < stop then
                    s := s + ',';
                end;
                BoldLog.Log(s);
                AddRemedy(Format('-- Unlink relation (%s.%s) ', [ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]));
                AddRemedy(format('UPDATE %s SET %s = -1 WHERE %s IN (%s);',
                  [ObjectSQlMapper.MainTable.SQlName, SingleLink.MainColumnName, SingleLink.MainColumnName, s]));
              end;
            end;
          end;
          if MemberofOtherEnd is TBoldEmbeddedSingleLinkDefaultMapper then
          if ClassOfOtherEnd.MainTable = nil then
            BoldLog.LogFmt('Can''t validate 1-1 : %s.%s',[ClassOfOtherEnd.ExpressionName, MemberOfOtherEnd.ExpressionName], ltError)
          else
          begin
            BoldLog.LogFmt('Validating 1-1 : %s.%s',[ClassOfOtherEnd.ExpressionName, MemberOfOtherEnd.ExpressionName]);
            SingleRoleOfOtherEnd := MemberOfOtherEnd as TBoldEmbeddedSingleLinkDefaultMapper;
            Query.Close;
            if (ttSingleSingleEmbeddInconsistencyTest in fValidatorTestTypes) then
            begin
              Query.AssignSQLText(format(SingleSingleEmbeddInconsistencyTest, [
                  ObjectSQlMapper.MainTable.SQlName, ClassOfOtherEnd.MainTable.sqlName,
                  SingleLink.MainColumnName, SingleRoleOfOtherEnd.MainColumnName]));
              OpenQuery;
              BoldIdField := Query.FieldByName('BOLD_ID');
              IdList.Clear;
              while not query.eof do
              begin
                IdList.Add(BoldIdField.AsString);
                Query.Next;
              end;
              Query.Close;
              if IdList.Count > 0 then
              begin
                BoldLog.LogFmt(sLogObjectsWithWrongLinks, [IdList.Count, ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName], ltWarning);
                FetchBlockSize := SystemSQLMapper.SQLDataBaseConfig.FetchBlockSize;
                for Block := 0 to (IdList.Count div FetchBlockSize) do
                begin
                  s := '';
                  Start := Block * FetchBlockSize;
                  Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), IdList.Count-1]);
                  for j := Start to Stop do
                  begin
                    s := s + IdList[j];
                    if j < stop then
                      s := s + ',';
                  end;
                  BoldLog.Log(s, ltDetail);
                  AddRemedy(Format(sCommentCleanRelation, [ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]));
                  AddRemedy(format('UPDATE %s SET %s = -1 WHERE BOLD_ID IN (%s);',
                    [ObjectSQlMapper.MainTable.SQlName, SingleLink.MainColumnName, s]));
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  IdList.Free;
end;

procedure TBoldDbDataValidatorThread.ValidateStrayObjects(ObjectDefaultMapper: TBoldObjectDefaultMapper);
var
  i: integer;
  IdList: TStringList;
  TypeList: TStringList;
  TypeId: string;
  OwnMapping: TBoldAllInstancesMappingArray;
  FetchBlockSize: integer;
  Block, Start,Stop: integer;
  s: string;
  BoldIdField: IBoldField;
  BoldTypeField: IBoldField;
begin
  if not (ttStrayObjectsTest in fValidatorTestTypes) then
    exit;
  BoldLog.LogHeader := Format('ValidateStrayObjects %s', [ObjectDefaultMapper.ExpressionName]);
  OwnMapping := SystemSQLMapper.MappingInfo.GetAllInstancesMapping(ObjectdefaultMapper.ExpressionName);
  if (length(OwnMapping)=1) and not OwnMapping[0].ClassIdRequired then
  begin
    IdList := TStringList.Create;
    TypeList := TStringList.Create;
    try
      Query.Close;
      Query.AssignSQLText(format(StrayObjectsTest, [ObjectDefaultMapper.MainTable.SQLName, ObjectDefaultMapper.SubClassesID]));
      OpenQuery;
      BoldIdField := Query.FieldByName(Field_BOLD_ID);
      BoldTypeField := Query.FieldByName(Field_BOLD_TYPE);
      while not query.eof do
      begin
        IdList.Add(BoldIdField.AsString);
        TypeId := BoldTypeField.AsString;
        if not TypeList.Indexof(TypeId) > -1 then
          TypeList.Add(TypeId);
        Query.Next;
      end;
      Query.Close;

      if IdList.Count > 0 then
      begin
        BoldLog.Log(sLogObjectsWithIllegalType, ltWarning);
        BoldLog.Log(TypeList.CommaText, ltDetail);
        FetchBlockSize := SystemSQLMapper.SQLDataBaseConfig.FetchBlockSize;
        for Block := 0 to (IdList.Count div FetchBlockSize) do
        begin
          s := '';
          Start := Block * FetchBlockSize;
          Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), IdList.Count-1]);
          for i := Start to Stop do
          begin
            s := s + IdList[i];
            if i < stop then
              s := s + ',';
          end;
          BoldLog.Log(s, ltDetail);
        end;
        BoldLog.LogFmt('The following %d objects are in table %s, but with an illegal type:', [IdList.Count, ObjectDefaultMapper.MainTable.SQLName], ltWarning);
        AddRemedy(format(sCommentRemoveObjectsWithIllegaltype, [ObjectDefaultMapper.MainTable.SQLName]));
        AddRemedy(format('DELETE FROM %s WHERE BOLD_ID IN (%s);', [ObjectDefaultMapper.MainTable.SQLName, s]));
        Query.AssignSQLText(Format('SELECT * FROM %s WHERE BOLD_ID IN (%s)', [ObjectDefaultMapper.MainTable.SQLName, s]));
      end;
    finally
      FreeAndNil(IdList);
      FreeAndNil(TypeList);
    end;
  end;
end;

function TBoldDbDataValidatorThread.GetQuery: IBoldQuery;
begin
  if not assigned(fQuery) then
  begin
    fQuery := Database.GetQuery;
    fQuery.UseReadTransactions := false;
  end;
  result := fQuery;
end;

constructor TBoldDbDataValidatorThread.Create(AValidator: TBoldDbValidator);
begin
  inherited;
  fTypeTestedTables := TStringList.create;
  fExistenceInParentTestedTables := TStringList.Create;
  ValidatorTestTypes := [ttExistenceInParentTest, ttExistenceInChildTest, ttTypeTest,
                        ttDuplicateSingleLinkTest, ttLinkObjectDupesTest, ttLinkObjectTest,
                        ttLinkObjectTest2, ttStrayObjectsTest, ttSingleSingleEmbeddInconsistencyTest,
                        ttRelationTest];
  fCorruptObjectsAction := caDelete;
end;

{procedure TBoldDbDataValidatorThread.DeActivate;
begin
  if assigned(fQuery) then
    SystemSQLMapper.ReleaseQuery(fQuery);
  inherited;
end;}

procedure TBoldDbDataValidatorThread.AddRemedyForDeleteObjects(Mapper: TBoldObjectSQLMapper; IdList: TStringList);
var
  i,j: integer;
  FetchBlockSize: integer;
  Block, Start,Stop: integer;
  s: string;
begin
  PersistenceHandle.BoldModel.MoldModel.EnsureTopSorted;
  FetchBlockSize := SystemSQLMapper.SQLDataBaseConfig.FetchBlockSize;
  for i := 0 to Mapper.AllTables.Count - 1 do
    if Mapper.Alltables[i] <> (Mapper.SystemPersistenceMapper.PSSystemDescription as TBoldDefaultSystemDescription).XFilestable then
    begin
      for Block := 0 to (IdList.Count div FetchBlockSize) do
      begin
        s := '';
        Start := Block * FetchBlockSize;
        Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), IdList.Count-1]);
        for j := Start to Stop do
        begin
          s := s + IdList[j];
          if j < stop then
            s := s + ',';
        end;
        AddRemedy(format('DELETE FROM %s WHERE BOLD_ID IN (%s);', [Mapper.AllTables[i].SQLName, s]));
      end;
    end;
  UnlinkFromDeleteObject(IdList, Mapper);
end;

type TBoldNonEmbeddedLinkDefaultMapperAccess = Class(TBoldNonEmbeddedLinkDefaultMapper);

procedure TBoldDbDataValidatorThread.UnlinkFromDeleteObject(IdList: TStrings;
  ObjectSQLMapper: TBoldObjectSQLMapper);
var
  i: integer;
  NonEmbeddedLink: TBoldNonEmbeddedLinkDefaultMapper;
  IndirectMultiLink: TBoldIndirectMultiLinkDefaultmapper;
  RelatedTable, Linktable: TBoldSQLTableDescription;
  ClassOfOtherEnd: TBoldObjectSQlMapper;
  MemberOfOtherEnd: TBoldMemberSQLMapper;
  MemberMappings: TBoldMemberMappingArray;
  BoldMemberPersistenceMapper: TBoldMemberPersistenceMapper;
begin
  LinkTable := ObjectSQLMapper.MainTable;
  MemberMappings := nil;
  if not assigned(LinkTable) then
    exit;
  for i := 0 to ObjectSQLMapper.MemberPersistenceMappers.count-1 do
  begin
    BoldMemberPersistenceMapper := ObjectSQLMapper.MemberPersistenceMappers[i];
    if not BoldMemberPersistenceMapper.IsStoredInObject then
    begin
    //TBoldIndirectSingleLinkDefaultmapper
      if BoldMemberPersistenceMapper is TBoldDirectMultiLinkDefaultmapper{TBoldNonEmbeddedLinkDefaultMapper} then
      begin
        NonEmbeddedLink := BoldMemberPersistenceMapper as TBoldNonEmbeddedLinkDefaultMapper;
        ClassOfOtherEnd := SystemSQLMapper.ObjectPersistenceMappers[NonEmbeddedLink.OtherEndObjectMapper.TopSortedIndex] as TBoldObjectSQlMapper;
        MemberOfOtherEnd := ClassOfOtherEnd.MemberPersistenceMappers[NonEmbeddedLink.ClosestOtherEndMemberMapperIndex] as TBoldMemberSQLMapper;
        RelatedTable := ClassOfOtherEnd.MainTable;
        if RelatedTable = nil then  //childmapped abstract class
          RelatedTable := ObjectSQLMapper.SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable;
        if NonEmbeddedLink=nil then
        begin
          BoldLog.LogFmt('TBoldDbDataValidatorThread.ValidateRelations: %s:%d SingleLink=nil', [ObjectSQLMapper.ExpressionName, I]);
          Continue;
        end;
        if LinkTable=nil then
        begin
          BoldLog.LogFmt('TBoldDbDataValidatorThread.ValidateRelations: %s:%d LinkTable=nil', [ObjectSQLMapper.ExpressionName, I]);
          Continue;
        end;
        if RelatedTable=nil then
        begin
          BoldLog.LogFmt('TBoldDbDataValidatorThread.ValidateRelations: %s:%d RelatedTable=nil', [ObjectSQLMapper.ExpressionName, I]);
          Continue;
        end;
        ClearSingleLink(IdList, MemberOfOtherEnd as TBoldEmbeddedSingleLinkDefaultMapper, ClassOfOtherEnd);
      end
      else
      if BoldMemberPersistenceMapper is TBoldIndirectMultiLinkDefaultmapper{TBoldNonEmbeddedLinkDefaultMapper} then
      begin
        IndirectMultiLink := BoldMemberPersistenceMapper as TBoldIndirectMultiLinkDefaultmapper;
        if Assigned(IndirectMultiLink.LinkClassObjectMapper) then
          continue; // skip not implemented link classes
        ClassOfOtherEnd := SystemSQLMapper.ObjectPersistenceMappers[IndirectMultiLink.OtherEndObjectMapper.TopSortedIndex] as TBoldObjectSQlMapper;
        MemberOfOtherEnd := ClassOfOtherEnd.MemberPersistenceMappers[IndirectMultiLink.ClosestOtherEndMemberMapperIndex] as TBoldMemberSQLMapper;
        Assert(MemberOfOtherEnd is TBoldIndirectMultiLinkDefaultmapper, MemberOfOtherEnd.ClassName);
        ClearMultiLink(IdList, MemberOfOtherEnd as TBoldIndirectMultiLinkDefaultmapper, ClassOfOtherEnd);
      end;
    end;
  end;
end;

procedure TBoldDbDataValidatorThread.ClearSingleLink(IdList: TStrings;
  SingleLink: TBoldEmbeddedSingleLinkDefaultMapper; ObjectSQLMapper: TBoldObjectSQLMapper);
var
  FetchBlockSize: integer;
  Block, Start,Stop: integer;
  j: integer;
  header,s : string;
begin
  if IdList.Count > 0 then
  begin
    header := Format('The following (%d) objects of class %s have invalid links in %s:', [IdList.Count, ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]);
    if ClearRelationIfNeeded(IdList, SingleLink, ObjectSQlMapper) then
    begin
      FetchBlockSize := SystemSQLMapper.SQLDataBaseConfig.FetchBlockSize;
      for Block := 0 to (IdList.Count div FetchBlockSize) do
      begin
        begin
          if header <> '' then // only log once, first time
          begin
            BoldLog.Log(header, ltWarning);
            header := '';
          end;
          s := '';
          Start := Block * FetchBlockSize;
          Stop := MinIntValue([Pred(Succ(Block) * FetchBlockSize), IdList.Count-1]);
          for j := Start to Stop do
          begin
            s := s + IdList[j];
            if j < stop then
              s := s + ',';
          end;
          BoldLog.Log(s, ltDetail);
        end;
      end;
    end;
  end;
end;

procedure TBoldDbDataValidatorThread.ClearMultiLink(IdList: TStrings;
  MultiLink: TBoldIndirectMultiLinkDefaultmapper;
  ObjectSQLMapper: TBoldObjectSQLMapper);
begin
//
end;

function TBoldDbDataValidatorThread.ClearRelationIfNeeded(IdList: TStrings;
  SingleLink: TBoldEmbeddedSingleLinkDefaultMapper;
  ObjectSQLMapper: TBoldObjectSQLMapper): boolean;
var
  LinkClassIdList: TStringList;
begin
  result := false;
  if not Assigned(ObjectSQLMapper.MainTable) then
    exit;
  Query.Close;
  Query.AssignSQLText(format('SELECT BOLD_ID FROM %s WHERE %s IN (%s)', [ObjectSQLMapper.MainTable.SQLName, SingleLink.MainColumnName, IdList.CommaText]));
  OpenQuery;
  // add block size
  if not query.eof then
  begin
    result := true;
    if ObjectSQlMapper.IsLinkClass then
    begin
      LinkClassIdList := TStringList.Create;
      try
        while not query.eof do
        begin
          LinkClassIdList.Add(query.Fields[0].AsString);
          query.Next;
        end;
        AddRemedy(Format('-- Delete link class (%s.%s) ', [ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]));
        SuggestTableDelete(SingleLink.ObjectPersistenceMapper.AllTables, LinkClassIdList, ObjectSQlMapper);
      finally
        LinkClassIdList.free;
      end;
    end
    else
    begin
      AddRemedy(Format('-- Clean relation (%s.%s) ', [ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]));
      AddRemedy(format('UPDATE %s SET %s = -1 WHERE %s IN (%s);',
        [ObjectSQlMapper.MainTable.SQLName, SingleLink.MainColumnName, SingleLink.MainColumnName, IdList.CommaText]));
    end;
  end;
  Query.Close;
end;

{ TBoldDbDataValidator }

constructor TBoldDbDataValidator.Create(owner: TComponent);
begin
  inherited;
end;

destructor TBoldDbDataValidator.Destroy;
begin

  inherited;
end;

procedure TBoldDbDataValidator.Validate;
var
  i: integer;
  ObjectPMapper: TBoldObjectSQLMapper;
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.CommaText := UpperCase(ClassesToValidate);
  try
    BoldLog.ProgressMax := SystemSQLMapper.ObjectPersistenceMappers.count - 1;
    for i := SystemSQLMapper.ObjectPersistenceMappers.count - 1 downto 0 do
    begin
      ObjectPMapper := SystemSQLMapper.ObjectPersistenceMappers[i] as TBoldObjectSQLMapper;
      if assigned(ObjectPMapper) then
      begin
        if (sl.Count > 0) and not (sl.IndexOf(UpperCase(ObjectPMapper.ExpressionName)) <> -1) then
          continue;
        TableQueue.Enqueue(ObjectPMapper);
{        BoldLog.LogHeader := Format(sProcessingClass, [ObjectPMapper.ExpressionName]);
        ValidateExistence(ObjectPMapper);
        ValidateRelations(ObjectPMapper);
          ValidateNotNullColumns(ObjectPMapper);
        if ObjectPmapper is TBoldObjectDefaultMapper then
          ValidateStrayObjects(ObjectPMapper as TBoldObjectDefaultMapper);
        if ObjectPMapper.IsLinkClass then
        begin
          ValidateLinkObjects(ObjectPmapper);
        end;}
      end;
//      BoldLog.Progress := SystemSQLMapper.ObjectPersistenceMappers.count-i+1;
      if DoCheckStop then exit;
    end;
  finally
    sl.free;
  end;
  inherited;
end;

function TBoldDbDataValidator.CreateValidatorThread: TBoldDbValidatorThread;
begin
  var thread := TBoldDbDataValidatorThread.Create(self);
  result := thread;
  thread.ValidatorTestTypes := ValidatorTestTypes;
end;

end.



