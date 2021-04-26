
{ Global compiler directives }
{$include bold.inc}
unit BoldDbDataValidator;

interface

uses
  Classes,
  BoldDbValidator,
  BoldPSDescriptionsSQL,
  BoldDbInterfaces,
  BoldPMappers,
  BoldPMappersSQL,
  BoldPMappersDefault;

type
  { forward declarations }
  TBoldDbDataValidator = class;

  { TBoldDbDataValidator }
  TBoldDbDataValidator = class(TBoldDbValidator)
  private
    fTypeTestedTables: TStringList;
    fQuery: IBoldQuery;
    fExistenceInParentTestedTables: TStringList;
    FPauseBetweenQueries: integer;
    function GetQuery: IBoldQuery;
    procedure SuggesttableInsert(table: TBoldSQLTableDescription; IdList,
      TypeList: TStrings);
  protected
    function MemberIsInherited(MemberMapper: TBoldMemberPersistenceMapper): Boolean;
    function Prepare2TableTest(SQLTemplate: String; CheckList: TStringList; args: array of const; table1, table2: String; IdList: TStrings; TypeList: TStrings = nil): Boolean;
    procedure AddRemedyForDeleteObjects(Mapper: TBoldObjectSQLMapper; IdList: TStringList);
    procedure DeActivate; override;
    procedure OpenQuery;
    property Query: IBoldQuery read GetQuery;
    property TypeTestedTables: TStringList read fTypeTestedTables;
    property ExistenceInParentTestedTables: TStringList read fExistenceInParentTestedTables;
  public
    constructor Create(owner: TComponent); override;
    destructor destroy; override;
    procedure ValidateExistence(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure ValidateStrayObjects(ObjectDefaultMapper: TBoldObjectDefaultMapper);
    procedure ValidateRelations(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure ValidateNotNullColumns(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure ValidateNotNullForColumn(BoldSQLColumnDescription: TBoldSQLColumnDescription);
    procedure ValidateLinkObjectDupes(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure ValidateLinkObjects(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure Validate; override;
    property PauseBetweenQueries: integer read FPauseBetweenQueries write FPauseBetweenQueries;
  end;

implementation

uses
  BoldPMappersLinkDefault,
  BoldSQLMappingInfo,
  BoldPSDescriptionsDefault,
  BoldLogHandler,
  db,
  BoldDefs,
  SysUtils,
  BoldUtils,
  BoldMath;

const
  ExistenceInParentTest: String =
    'SELECT OWN.BOLD_ID, OWN.BOLD_TYPE ' + BOLDCRLF +
    'FROM %s OWN ' + BOLDCRLF +
    'WHERE NOT EXISTS(' + BOLDCRLF +
    '  SELECT PARENT.BOLD_ID ' + BOLDCRLF +
    '  FROM %s PARENT ' + BOLDCRLF +
    '  WHERE PARENT.BOLD_ID = OWN.BOLD_ID)';

  ExistenceInChildTest: string =
    'SELECT PARENT.BOLD_ID, PARENT.BOLD_TYPE ' + BOLDCRLF +
    'FROM %s PARENT' + BOLDCRLF +
    'WHERE PARENT.BOLD_TYPE IN (%s) AND' + BOLDCRLF +
    '      NOT EXISTS(' + BOLDCRLF +
    '          SELECT BOLD_ID' + BOLDCRLF +
    '          FROM %s OWN ' + BOLDCRLF +
    '          WHERE OWN.BOLD_ID = PARENT.BOLD_ID)';

  TypeTest: String =
    'SELECT PARENT.BOLD_ID, PARENT.BOLD_TYPE, OWN.BOLD_TYPE' + BOLDCRLF +
    'FROM %s PARENT, %s OWN' + BOLDCRLF +
    'WHERE (PARENT.BOLD_ID = OWN.BOLD_ID) AND' + BOLDCRLF +
    '      (PARENT.BOLD_TYPE <> OWN.BOLD_TYPE)';


  RelationTest: String =
    'SELECT OWN.%s, OWN.BOLD_ID' + BOLDCRLF +
    'FROM %s OWN' + BOLDCRLF +
    'WHERE OWN.%0:s <> -1 AND NOT EXISTS (' + BOLDCRLF +
    '  SELECT RELATED.BOLD_ID' + BOLDCRLF +
    '  FROM %2:s RELATED' + BOLDCRLF +
    '  WHERE RELATED.BOLD_ID = OWN.%0:s)';

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

  LinkObjectTest: String =
    'SELECT BOLD_ID' + BOLDCRLF +
    'FROM %s' + BOLDCRLF +
    'WHERE %s = -1 or %s = -1';
  LinkObjectTest2: String =
    'SELECT LT.BOLD_ID' + BOLDCRLF +
    'FROM %s LT' + BOLDCRLF +
    'WHERE NOT EXISTS (SELECT T.BOLD_ID FROM %s T WHERE LT.%s = T.BOLD_ID)';


  StrayObjectsTest: string =
    'SELECT BOLD_ID, BOLD_TYPE ' + BOLDCRLF +
    'FROM %s' + BOLDCRLF +
    'WHERE NOT BOLD_TYPE IN (%s)';

  SingleSingleEmbeddInconsistencyTest: String =
    'SELECT T1.BOLD_ID, T1.BOLD_TYPE ' + BOLDCRLF +
    'FROM %s T1, %s T2 ' + BOLDCRLF +
    'WHERE (T1.%s = T2.BOLD_ID) and (T2.%s <> T1.BOLD_ID)';

{ TBoldDbDataValidator }

destructor TBoldDbDataValidator.destroy;
begin
  FreeAndNil(fTypeTestedTables);
  FreeAndNil(fExistenceInParentTestedTables);
  if assigned(fQuery) then
    SystemSQLMapper.ReleaseQuery(fQuery);
  inherited;
end;

function TBoldDbDataValidator.MemberIsInherited(
  MemberMapper: TBoldMemberPersistenceMapper): Boolean;
begin
  result := false;
  if Assigned(MemberMapper.ObjectPersistenceMapper.SuperClass) then
    result := MemberMapper.MemberIndex < MemberMapper.ObjectPersistenceMapper.SuperClass.MemberPersistenceMappers.count;
end;

procedure TBoldDbDataValidator.OpenQuery;
begin
  Sleep(1000*PauseBetweenQueries);
  Query.Open;
end;

function TBoldDbDataValidator.Prepare2TableTest(SQLTemplate: String; CheckList: TStringList; args: array of const; table1, table2: String; IdList: TStrings; TypeList: TStrings = nil): Boolean;
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
    BoldIdField := Query.FieldByName('BOLD_ID');
    BoldTypeField := Query.FieldByName('BOLD_TYPE');
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

procedure TBoldDbDataValidator.Validate;
var
  i: integer;
  ObjectPMapper: TBoldObjectSQLMapper;
begin
(*
  if not PersistenceHandle.DatabaseInterface.Connected then
  begin
    BoldLog.Log('Database must be opened before Structurevalidation is performed!');
    exit;
  end;
*)
  BoldLog.ProgressMax := SystemSQLMapper.ObjectPersistenceMappers.count - 1;
  for i := 0 to SystemSQLMapper.ObjectPersistenceMappers.count - 1 do
  begin
    ObjectPMapper := SystemSQLMapper.ObjectPersistenceMappers[i] as TBoldObjectSQLMapper;
    if assigned(ObjectPMapper) then
    begin
      BoldLog.LogHeader := Format('Processing class %d %s', [i, ObjectPMapper.ExpressionName]);
      ValidateExistence(ObjectPMapper);
      ValidateRelations(ObjectPMapper);
//      ValidateNotNullColumns(ObjectPMapper);
      if ObjectPmapper is TBoldObjectDefaultMapper then
        ValidateStrayObjects(ObjectPMapper as TBoldObjectDefaultMapper);
      if ObjectPMapper.IsLinkClass then
      begin
        ValidateLinkObjects(ObjectPmapper);
      end;
    end;
    BoldLog.Progress := i;
  end;
end;

procedure TBoldDbDataValidator.SuggesttableInsert(table: TBoldSQLTableDescription; IdList, TypeList: TStrings);
var
  j: integer;
  Skip: Boolean;
  remedySQL: String;
  UnsupportedColumns, NotNullColumns, NotNullValues: string;
  Column: TBoldSQLColumnDescription;
begin
  BoldLog.Log(Table.SQlName);
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

  remedySQL := 'INSERT INTO %s (BOLD_ID, BOLD_TYPE%s) VALUES (%s, %s%s)';
  if skip then
  begin
    Remedy.add(format('-- Required column(s) %s has unsupported type(s)', [UnsupportedColumns]));
    remedySQL := '-- '+ remedySQL;
  end
  else
    remedy.Add(Format('-- add missing entries into %s', [Table.SQLName]));

  for j := 0 to idlist.count - 1 do
    Remedy.Add(format(remedySQL, [Table.SQLName, NotNullColumns, IdList[j], TypeList[j], NotNullValues]));
end;

procedure TBoldDbDataValidator.ValidateExistence(ObjectSQLMapper: TBoldObjectSQLMapper);
var
  i: integer;
  IdList, TypeList: TStringList;
  SubTable, SuperTable: String;
  OwnMapping, ParentMapping: TBoldAllInstancesMappingArray;
  Tables: TBoldSQLTableDescriptionList;
  TempMapper: TBoldObjectSQLMapper;
begin
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
        BoldLog.LogFmt('TBoldDbDataValidator.ValidateExistence: %s Tables[%d]=nil', [ObjectSQLMapper.ExpressionName, I]);
        Continue;
      end;
      if Tables[i+1]=nil then
      begin
        BoldLog.LogFmt('TBoldDbDataValidator.ValidateExistence: %s Tables[%d+1]=nil', [ObjectSQLMapper.ExpressionName, I]);
        Continue;
      end;
      SubTable := Tables[i].SQLName;
      SuperTable := Tables[i+1].SQLName;
      if Prepare2TableTest(ExistenceInParentTest, ExistenceInParentTestedTables,
        [SubTable, SuperTable], SubTable, SuperTable, IdList, TypeList) then
      begin
        BoldLog.LogFmt('The following objects exists in %s, but not in parent table %s', [SubTable, SuperTable], ltWarning);
        Boldlog.Log(IdList.CommaText, ltDetail);
        SuggestTableInsert(Tables[i+1], IdList, TypeList);
      end;
      if Prepare2TableTest(TypeTest, TypeTestedTables, [SuperTable, SubTable], SuperTable, SubTable, IdList) then
      begin
        BoldLog.LogFmt('The following objects have different type in table %s and %s', [SuperTable, SubTable], ltWarning);
        Boldlog.Log(IdList.CommaText, ltDetail);
      end;
    end;

    if (ObjectSQLMapper is TBoldObjectDefaultMapper) and assigned(ObjectSQlMapper.SuperClass) then
    begin
      OwnMapping := SystemSQLMapper.MappingInfo.GetAllInstancesMapping(ObjectSQlMapper.ExpressionName);
      ParentMapping := SystemSQLMapper.MappingInfo.GetAllInstancesMapping(ObjectSQlMapper.SuperClass.ExpressionName);

      if (length(OwnMapping) = 1) and (length(ParentMapping) = 1) and
        (OwnMapping[0].tableName <> ParentMapping[0].TableName) then
      begin
        SuperTable := ParentMapping[0].TableName;
        SubTable := OwnMapping[0].TableName;
        if Prepare2TableTest(ExistenceInChildTest, nil, [
            SuperTable,
            (ObjectSQLMapper as TBoldObjectDefaultMapper).SubClassesID,
            SubTable], SubTable, SuperTable, IdList, TypeList) then
        begin
          BoldLog.LogFmt('The following objects exists in %s, but not in child table %s', [SuperTable, SubTable], ltWarning);
          Boldlog.Log(IdList.CommaText, ltDetail);
          SuggesttableInsert(ObjectSQLMapper.MainTable, IdList, TypeList);
        end;
      end;
    end;
  finally
    tables.free;
    IdList.Free;
    TypeList.free;
  end;
end;

procedure TBoldDbDataValidator.ValidateLinkObjectDupes(
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
  IdList := TStringList.create;

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
    BoldLog.Log('The following Linkobjects are duplicates:', ltWarning);
    BoldLog.Log(IdList.CommaText, ltDetail);
  end;
  IdList.Free;
end;

procedure TBoldDbDataValidator.ValidateLinkObjects(ObjectSQLMapper: TBoldObjectSQLMapper);
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
      BoldLog.LogFmt('TBoldDbDataValidator.ValidateLinkObjects->CheckSpacePointers: %s: LinkMapper=nil', [ObjectSQLMapper.ExpressionName]);
      Exit;
    end;
    Assert(PersistenceHandle.BoldModel.MoldModel.GetClassByName(LinkMapper.OtherEndObjectMapper.ExpressionName) <> nil, 'Class ' + LinkMapper.ObjectPersistenceMapper.ExpressionName + ' not found.');
    if LInkMapper.OtherEndObjectMapper=nil then
    begin
      BoldLog.LogFmt('TBoldDbDataValidator.ValidateLinkObjects->CheckSpacePointers: %s: LinkMapper.OtherEndObjectMapper=nil', [ObjectSQLMapper.ExpressionName]);
      Exit;
    end;
    if LInkMapper.OtherEndObjectMapper.Maintable<>nil then
      MainTableName := LinkMapper.OtherEndObjectMapper.Maintable.SQLName
    else
    begin
      MainTableName := 'Bold_Object'; // it would be more efficient to find first concrete superclass so that we search more concrete table instead of all in Bold_Object
      if not PersistenceHandle.BoldModel.MoldModel.GetClassByName(LinkMapper.OtherEndObjectMapper.ExpressionName).IsAbstract or not LinkMapper.OtherEndObjectMapper.HasSubClasses then
        BoldLog.LogFmt('TBoldDbDataValidator.ValidateLinkObjects->CheckSpacePointers: %s: Link: %s LinkMapper.OtherEndObjectMapper.Maintable=nil, substituting bold_object', [ObjectSQLMapper.ExpressionName, LinkMapper.MainColumnName]);
    end;
    Query.Close;
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
        BoldLog.LogFmt('The following Linkobjects (class %s) have links to nonexisting objects:', [ObjectSQLmapper.ExpressionName], ltWarning);
        BoldLog.Log(TempIdList.CommaText, ltDetail);
        Remedy.Add(Format('-- Clean Linkobjects (%s) with space pointers', [ObjectSQLmapper.ExpressionName]));
        AddRemedyForDeleteObjects(ObjectSQLMapper, IDList);
      end;
    finally
      TempIdList.free;
    end;
  end;

var
  BoldIdField: IBoldField;
begin
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
    BoldLog.LogFmt('The following Linkobjects (class %s) have empty links in one direction:', [ObjectSQLmapper.ExpressionName], ltWarning);
    BoldLog.Log(IdList.CommaText, ltDetail);
    Remedy.Add(Format('-- Clean Linkobjects (%s) with broken links', [ObjectSQLmapper.ExpressionName]));
    AddRemedyForDeleteObjects(ObjectSQLMapper, IdLIst);
  end;

  CheckSpacePointers(ObjectSQLMapper.LinkClassRole1 as TBoldEmbeddedSingleLinkDefaultMapper);
  CheckSpacePointers(ObjectSQLMapper.LinkClassRole2 as TBoldEmbeddedSingleLinkDefaultMapper);

  IdList.Free;
end;

procedure TBoldDbDataValidator.ValidateNotNullForColumn(
  BoldSQLColumnDescription: TBoldSQLColumnDescription);
var
  FieldNames: TStrings;
  TableName: String;
  ColumnName: String;
  NullCount: integer;
begin
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
      BoldLog.LogFmtIndent('%d Null-values found in Table %s, Column %s: ',
        [NullCount, tableName, ColumnName], ltWarning);
      Remedy.Add(Format('UPDATE %s SET %s = <initial value> WHERE %1:s IS NULL;',
                        [TableName, ColumnName]));
    end;
  finally
    FieldNames.Free;
  end;
end;

procedure TBoldDbDataValidator.ValidateNotNullColumns(
  ObjectSQLMapper: TBoldObjectSQLMapper);
var
  i, j: integer;
  BoldSQLTableDescription: TBoldSQLTableDescription;
  BoldSQLColumnDescription: TBoldSQLColumnDescription;
begin
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

procedure TBoldDbDataValidator.ValidateRelations(ObjectSQLMapper: TBoldObjectSQLMapper);
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
  if not assigned(LinkTable) then
    exit;
  for i := 0 to ObjectSQLMapper.MemberPersistenceMappers.count-1 do
  begin
    if ObjectSQLMapper.MemberPersistenceMappers[i] is TBoldEmbeddedSingleLinkDefaultMapper then
    begin
      SingleLink := ObjectSQLMapper.MemberPersistenceMappers[i] as TBoldEmbeddedSingleLinkDefaultMapper;
      MemberMappings := ObjectSQLMapper.SystemPersistenceMapper.MappingInfo.GetMemberMappings(ObjectSQLMapper.ExpressionName, SingleLink.ExpressionName);
      if (length(MemberMappings) = 1) and
         SameText(MemberMappings[0].TableName, LinkTable.SQLName) then
      begin
        ClassOfOtherEnd := SystemSQLMapper.ObjectPersistenceMappers[SingleLink.OtherEndObjectPMIndex] as TBoldObjectSQlMapper;
        MemberOfOtherEnd := ClassOfOtherEnd.MemberPersistenceMappers[SingleLink.OtherEndMemberPMIndex] as TBoldMemberSQLMapper;
        RelatedTable := ClassOfOtherEnd.MainTable;
        if RelatedTable = nil then  //childmapped abstract class
          RelatedTable := ObjectSQLMapper.SystemPersistenceMapper.RootClassObjectPersistenceMapper.MainTable;
        if SingleLink=nil then
        begin
          BoldLog.LogFmt('TBoldDbDataValidator.ValidateRelations: %s:%d SingleLink=nil', [ObjectSQLMapper.ExpressionName, I]);
          Continue;
        end;
        if LinkTable=nil then
        begin
          BoldLog.LogFmt('TBoldDbDataValidator.ValidateRelations: %s:%d LinkTable=nil', [ObjectSQLMapper.ExpressionName, I]);
          Continue;
        end;
        if RelatedTable=nil then
        begin
          BoldLog.LogFmt('TBoldDbDataValidator.ValidateRelations: %s:%d RelatedTable=nil', [ObjectSQLMapper.ExpressionName, I]);
          Continue;
        end;
        Query.Close;
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
          BoldLog.LogFmt('The following (%d) objects of class %s have invalid links in %s:', [IdList.Count, ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName], ltWarning);
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
            Remedy.Add(Format('-- Clean relation (%s.%s) ', [ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]));
            Remedy.Add(format('UPDATE %s SET %s = -1 WHERE BOLD_ID IN (%s);',
              [LinkTable.SQLName, SingleLink.MainColumnName, s]));
          end;
        end;

        ClassOfOtherEnd := ObjectSQLMapper.SystemPersistenceMapper.ObjectPersistencemappers[SingleLInk.OtherEndObjectPMIndex] as TBoldObjectSQlMapper;
        if assigned(ClassOfOtherEnd) then
        begin
          MemberOfOtherEnd := ClassOfOtherEnd.MemberPersistenceMappers[SingleLink.OtherEndMemberPMIndex] as TBoldMemberSQLMapper;
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
                Remedy.Add(Format('-- Unlink relation (%s.%s) ', [ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]));
                Remedy.Add(format('UPDATE %s SET %s = -1 WHERE %s IN (%s);',
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
              BoldLog.LogFmt('The following %d objects of class %s have singlelinks (%s) pointing to objects that don''t point back (they might point elsewhere):', [IdList.Count, ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]);
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
                Remedy.Add(Format('-- Clean relation (%s.%s) ', [ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]));
                Remedy.Add(format('UPDATE %s SET %s = -1 WHERE BOLD_ID IN (%s);',
                  [ObjectSQlMapper.MainTable.SQlName, SingleLink.MainColumnName, s]));
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  IdList.Free;
end;

procedure TBoldDbDataValidator.ValidateStrayObjects(ObjectDefaultMapper: TBoldObjectDefaultMapper);
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
  OwnMapping := SystemSQLMapper.MappingInfo.GetAllInstancesMapping(ObjectdefaultMapper.ExpressionName);
  if (length(OwnMapping)=1) and not OwnMapping[0].ClassIdRequired then
  begin
    IdList := TStringList.Create;
    TypeList := TStringList.Create;
    try
      Query.Close;
      Query.AssignSQLText(format(StrayObjectsTest, [ObjectDefaultMapper.MainTable.SQLName, ObjectDefaultMapper.SubClassesID]));
      OpenQuery;
      BoldIdField := Query.FieldByName('BOLD_ID');
      BoldTypeField := Query.FieldByName('BOLD_TYPE');
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
        BoldLog.Log('The following invalid types occur in the objects listed below:', ltWarning);
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
        Remedy.Add(format('-- Remove objects with illegal type in table %s', [ObjectDefaultMapper.MainTable.SQLName]));
        Remedy.Add(format('DELETE FROM %s WHERE BOLD_ID IN (%s);', [ObjectDefaultMapper.MainTable.SQLName, s]));
        Query.AssignSQLText(Format('SELECT * FROM %s WHERE BOLD_ID IN (%s)', [ObjectDefaultMapper.MainTable.SQLName, s]));
      end;
    finally
      FreeAndNil(IdList);
      FreeAndNil(TypeList);
    end;
  end;
end;

function TBoldDbDataValidator.GetQuery: IBoldQuery;
begin
  if not assigned(fQuery) then
    fQuery := SystemSQLMapper.GetQuery;
  result := fQuery;
end;

constructor TBoldDbDataValidator.Create(owner: Tcomponent);
begin
  inherited;
  fTypeTestedTables := TStringList.create;
  fExistenceInParentTestedTables := TStringList.Create;
end;

procedure TBoldDbDataValidator.DeActivate;
begin
  if assigned(fQuery) then
    SystemSQLMapper.ReleaseQuery(fQuery);
  inherited;
end;

procedure TBoldDbDataValidator.AddRemedyForDeleteObjects(Mapper: TBoldObjectSQLMapper; IdList: TStringList);
var
  i,j: integer;
  FetchBlockSize: integer;
  Block, Start,Stop: integer;
  s: string;
begin
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
        Remedy.Add(format('DELETE FROM %s WHERE BOLD_ID IN (%s);', [Mapper.AllTables[i].SQLName, s]));
      end;
    end;
end;

initialization

end.
