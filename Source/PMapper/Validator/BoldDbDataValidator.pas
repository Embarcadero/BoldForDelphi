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
    function GetQuery: IBoldQuery;
    procedure SuggesttableInsert(table: TBoldSQLTableDescription; IdList,
      TypeList: TStrings);
  protected
    function MemberIsInherited(MemberMapper: TBoldMemberPersistenceMapper): Boolean;
    function Prepare2TableTest(SQLTemplate: String; CheckList: TStringList; args: array of const; table1, table2: String; IdList: TStrings; TypeList: TStrings = nil): Boolean;
    procedure AddRemedyForDeleteObjects(Mapper: TBoldObjectSQLMapper; IdList: TStringList);
    procedure DeActivate; override;
    property Query: IBoldQuery read GetQuery;
    property TypeTestedTables: TStringList read fTypeTestedTables;
    property ExistenceInParentTestedTables: TStringList read fExistenceInParentTestedTables;
  public
    constructor Create(owner: Tcomponent); override;
    destructor Destroy; override;
    procedure ValidateExistence(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure ValidateStrayObjects(ObjectDefaultMapper: TBoldObjectDefaultMapper);
    procedure ValidateRelations(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure ValidateLinkObjectDupes(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure ValidateLinkObjects(ObjectSQLMapper: TBoldObjectSQLMapper);
    procedure Validate; override;
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
  BoldPMConsts;

const
  Field_BOLD_ID = 'BOLD_ID';
  ExistenceInParentTest: String =
    'SELECT OWN.BOLD_ID, OWN.BOLD_TYPE ' + BOLDCRLF +
    'FROM %s OWN ' + BOLDCRLF +
    'WHERE NOT EXISTS(' + BOLDCRLF +
    '  SELECT PARENT.BOLD_ID ' + BOLDCRLF +
    '  FROM %s PARENT ' + BOLDCRLF +
    '  WHERE PARENT.BOLD_ID = OWN.BOLD_ID)';
    // [Own, Parent]

  ExistenceInChildTest: string =
    'SELECT PARENT.BOLD_ID, PARENT.BOLD_TYPE ' + BOLDCRLF +
    'FROM %s PARENT' + BOLDCRLF +
    'WHERE PARENT.BOLD_TYPE IN (%s) AND' + BOLDCRLF +
    '      NOT EXISTS(' + BOLDCRLF +
    '          SELECT BOLD_ID' + BOLDCRLF +
    '          FROM %s OWN ' + BOLDCRLF +
    '          WHERE OWN.BOLD_ID = PARENT.BOLD_ID)';
    // [parent, types, Own]

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

function TBoldDbDataValidator.Prepare2TableTest(SQLTemplate: String; CheckList: TStringList; args: array of const; table1, table2: String; IdList: TStrings; TypeList: TStrings = nil): Boolean;
begin
  if not assigned(CheckList) or
  ((CheckList.IndexOf(Table1 + ':' + Table2) = -1) and
   (CheckList.IndexOf(Table2 + ':' + Table1) = -1)) then
  begin
    IdList.Clear;
    if assigned(typelist) then
      typeList.Clear;
    Query.AssignSQLText(format(SQLTemplate, args));
    Query.Open;
    while not query.eof do
    begin
      IdList.Add(Query.FieldByName(Field_BOLD_ID).AsString);
      if assigned(Typelist) then
        TypeList.Add(Query.FieldByName('BOLD_TYPE').AsString); // do not localize
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
  if not PersistenceHandle.DatabaseInterface.Connected then
  begin
    Remedy.Add(sDBNotOpened);
    BoldLog.Log(sDBMustBeOpened);
    exit;
  end;

  BoldLog.ProgressMax := SystemSQLMapper.ObjectPersistenceMappers.count - 1;
  for i := 0 to SystemSQLMapper.ObjectPersistenceMappers.count - 1 do
  begin
    ObjectPMapper := SystemSQLMapper.ObjectPersistenceMappers[i] as TBoldObjectSQLMapper;
    if assigned(ObjectPMapper) then
    begin
      BoldLog.LogHeader := Format(sProcessingClass, [ObjectPMapper.ExpressionName]);
      ValidateExistence(ObjectPMapper);
      ValidateRelations(ObjectPMapper);
      if ObjectPmapper is TBoldObjectDefaultMapper then
        ValidateStrayObjects(ObjectPMapper as TBoldObjectDefaultMapper);

      if ObjectPMapper.IsLinkClass then
      begin
  //  The SQL for this is broken... why?
  //
  //      ValidateLinkObjectDupes(ObjectPMapper);
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
  // check all columns except BodlID/BoldType
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
        ftBoolean: NotNullValues := NotNullValues + ', false'; // do not localize
        ftDate: NotNullValues := NotNullValues + ', ' + DateToStr(0.0);
        ftTime: NotNullValues := NotNullValues + ', ' + TimeToStr(0.0);
        ftDateTime: NotNullValues := NotNullValues + ', ' + DateTimeToStr(0.0);
        else
        begin
          // ftBytes, ftVarBytes, ftAutoInc, ftGraphic, ftFmtMemo,
          // ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
          // ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
          // ftVariant, ftInterface, ftIDispatch, ftGuid
          skip := true;
          NotNullValues := NotNullValues + ', <XXX>'; // do not localize

          if UnsupportedColumns <> '' then
            UnsupportedColumns := UnsupportedColumns + ', ';
          UnsupportedColumns := UnsupportedColumns + Column.SQLName;
        end;
      end;
    end;
  end;

  remedySQL := 'INSERT INTO %s (BOLD_ID, BOLD_TYPE%s) VALUES (%s, %s%s)'; // do not localize
  if skip then
  begin
    Remedy.add(format(sColumnsHaveUnsupportedType, [UnsupportedColumns]));
    remedySQL := '// ' + remedySQL;
  end
  else
    remedy.Add(Format(sAddMissingEntries, [Table.SQLName]));

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
      if assigned(tempMapper.Maintable) and (Tables.IndexOf(tempMapper.Maintable) = -1) then
        Tables.Add(tempMapper.Maintable);
      TempMapper := TempMapper.Superclass as TBoldObjectSQLMapper;
    end;

    for i := 0 to Tables.count - 2 do
    begin
      SubTable := Tables[i].SQLName;
      SuperTable := Tables[i + 1].SQLName;
      if Prepare2TableTest(ExistenceInParentTest, ExistenceInParentTestedTables,
        [SubTable, SuperTable], SubTable, SuperTable, IdList, TypeList) then
      begin
        BoldLog.LogFmt(sLogObjectsMissingInParentTable, [SubTable, SuperTable]);
        Boldlog.Log(IdList.CommaText);
        SuggestTableInsert(Tables[i+1], IdList, TypeList);
      end;
      if Prepare2TableTest(TypeTest, TypeTestedTables, [SuperTable, SubTable], SuperTable, SubTable, IdList) then
      begin
        BoldLog.LogFmt(sLogObjectsHaveDifferentType, [SuperTable, SubTable]);
        Boldlog.Log(IdList.CommaText);
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
          BoldLog.LogFmt(sLogObjectsMissingInChildtable, [SuperTable, SubTable]);
          Boldlog.Log(IdList.CommaText);
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
begin
  IdList := TStringList.create;

  LinkColumn1 := (ObjectSQLMapper.LinkClassRole1 as TBoldEmbeddedSingleLinkDefaultMapper).MainColumnName;
  LinkColumn2 := (ObjectSQLMapper.LinkClassRole2 as TBoldEmbeddedSingleLinkDefaultMapper).MainColumnName;
  LinkTable := ObjectSQLMapper.MainTable.SQLName;

  Query.AssignSQLText(format(LinkObjectDupesTest, [LInkColumn1, LinkColumn2, LInkTable]));
  Query.Open;
  LastLink1 := -Maxint;
  LastLInk2 := -MaxInt;
  while not query.eof do
  begin
    link1 := Query.FieldByName(LinkColumn1).AsInteger;
    link2 := Query.FieldByName(LinkColumn2).AsInteger;
    if (Link1 = LastLink1) and (link2 = lastlink2) then
      IdList.Add(Query.FieldByName(Field_BOLD_ID).AsString)
    else
    begin
      LastLInk1 := link1;
      LastLink2 := Link2;
    end;
  end;
  if IdList.Count > 0 then
  begin
    BoldLog.Log(sLogLinkObjectsAreDupes);
    BoldLog.Log(IdList.CommaText);
  end;
  IdList.Free;
end;

procedure TBoldDbDataValidator.ValidateLinkObjects(ObjectSQLMapper: TBoldObjectSQLMapper);
var
  LinkTable,
  LinkColumn1, LinkColumn2: String;
  IdList: TStringList;

procedure CheckSpacePointers(LinkMApper: TBoldEmbeddedSingleLInkDefaultMapper);
begin
  Query.AssignSQLText(format(LinkObjectTest2, [
    LinkTable,
    LInkMapper.OtherEndObjectMapper.Maintable.SQLName,
    LinkMapper.MainColumnName]));
  Query.Open;

  while not query.eof do
  begin
    IdList.Add(Query.FieldByName(Field_BOLD_ID).AsString);
    Query.Next;
  end;

  if IdList.Count > 0 then
  begin
    BoldLog.LogFmt(sLogLinkObjectsLinkUnexistingObjects, [ObjectSQLmapper.ExpressionName]);
    BoldLog.Log(IdList.CommaText);
    Remedy.Add(Format(sCommentRemoveSpaceLinkObjects, [ObjectSQLmapper.ExpressionName]));
    AddRemedyForDeleteObjects(ObjectSQLMapper, IDList);
  end;
end;

begin
  IdList := TStringList.create;
  LinkColumn1 := (ObjectSQLMapper.LinkClassRole1 as TBoldEmbeddedSingleLinkDefaultMapper).MainColumnName;
  LinkColumn2 := (ObjectSQLMapper.LinkClassRole2 as TBoldEmbeddedSingleLinkDefaultMapper).MainColumnName;
  LinkTable := ObjectSQLMapper.MainTable.SQLName;

  Query.AssignSQLText(format(LinkObjectTest, [LinkTable, LinkColumn1, LinkColumn2]));
  Query.Open;
  while not query.eof do
  begin
    IdList.Add(Query.FieldByName(Field_BOLD_ID).AsString);
    Query.Next;
  end;

  if IdList.Count > 0 then
  begin
    BoldLog.LogFmt(sLogBrokenLinkObjects, [ObjectSQLmapper.ExpressionName]);
    BoldLog.Log(IdList.CommaText);
    Remedy.Add(Format(sCommentRemoveBrokenLinkObjects, [ObjectSQLmapper.ExpressionName]));
    AddRemedyForDeleteObjects(ObjectSQLMapper, IdLIst);
  end;

  CheckSpacePointers(ObjectSQLMapper.LinkClassRole1 as TBoldEmbeddedSingleLinkDefaultMapper);
  CheckSpacePointers(ObjectSQLMapper.LinkClassRole2 as TBoldEmbeddedSingleLinkDefaultMapper);

  IdList.Free;
end;

procedure TBoldDbDataValidator.ValidateRelations(ObjectSQLMapper: TBoldObjectSQLMapper);
var
  i: integer;
  IdList: TStringList;
  SingleLink: TBoldEmbeddedSingleLinkDefaultMapper;
  RelatedTable,
  Linktable: TBoldSQLTableDescription;
  ClassOfOtherEnd: TBoldObjectSQlMapper;
  MemberOfOtherEnd: TBoldMemberSQLMapper;
  SingleRoleOfOtherEnd: TBoldEmbeddedSingleLInkDefaultMapper;
  MemberMappings: TBoldMemberMappingArray;
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
      // check if the member is stored in our table.
      if (length(MemberMappings) = 1) and
         SameText(MemberMappings[0].TableName, LinkTable.SQLName) then
      begin
        RelatedTable := (SystemSQLMapper.ObjectPersistenceMappers[SingleLink.OtherEndObjectPMIndex] as TBoldObjectSQLMapper).MainTable;
        Query.AssignSQLText(format(RelationTest, [SingleLink.MainColumnName, LInkTable.SQLName, relatedtable.SQLName]));
        Query.Open;
        IdList.Clear;
        while not query.eof do
        begin
          IdList.Add(Query.FieldByName(Field_BOLD_ID).AsString);
          Query.Next;
        end;
        Query.Close;
        if IdList.Count > 0 then
        begin
          BoldLog.LogFmt(sLogObjectsWithBrokenLinks, [ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]);
          BoldLog.Log(IdList.CommaText);
          Remedy.Add(Format(sCommentCleanRelation, [ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]));

          remedy.Add(format('UPDATE %s SET %s = -1 WHERE BOLD_ID IN (%s);', // do not localize
            [LinkTable.SQLName, SingleLink.MainColumnName, IdList.CommaText]));
        end;

        // find inconsistent single-single embedded links

        ClassOfOtherEnd := ObjectSQLMapper.SystemPersistenceMapper.ObjectPersistencemappers[SingleLInk.OtherEndObjectPMIndex] as TBoldObjectSQlMapper;
        if assigned(ClassOfOtherEnd) then
        begin
          MemberOfOtherEnd := ClassOfOtherEnd.MemberPersistenceMappers[SingleLink.OtherEndMemberPMIndex] as TBoldMemberSQLMapper;
          if MemberofOtherEnd is TBoldEmbeddedSingleLinkDefaultMapper then
          begin
            SingleRoleOfOtherEnd := MemberOfOtherEnd as TBoldEmbeddedSingleLinkDefaultMapper;
            Query.AssignSQLText(format(SingleSingleEmbeddInconsistencyTest, [
                ObjectSQlMapper.MainTable.SQlName, ClassOfOtherEnd.MainTable.sqlName,
                SingleLink.MainColumnName, SingleRoleOfOtherEnd.MainColumnName]));
            Query.Open;
            IdList.Clear;
            while not query.eof do
            begin
              IdList.Add(Query.FieldByName(Field_BOLD_ID).AsString);
              Query.Next;
            end;
            Query.Close;
            if IdList.Count > 0 then
            begin
              BoldLog.LogFmt(sLogObjectsWithWrongLinks, [ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]);
              BoldLog.Log(IdList.CommaText);
              Remedy.Add(Format(sCommentCleanRelation, [ObjectSQlMapper.ExpressionName, SingleLink.ExpressionName]));

              remedy.Add(format('UPDATE %s SET %s = -1 WHERE BOLD_ID IN (%s);', // do not localize
                [ObjectSQlMapper.MainTable.SQlName, SingleLink.MainColumnName, IdList.CommaText]));
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
  IdList: TStringList;
  OwnMapping: TBoldAllInstancesMappingArray;
begin
  OwnMapping := SystemSQLMapper.MappingInfo.GetAllInstancesMapping(ObjectdefaultMapper.ExpressionName);
  if (length(OwnMapping)=1) and not OwnMapping[0].ClassIdRequired then
  begin
    IdList := TStringList.Create;
    try
      Query.AssignSQLText(format(StrayObjectsTest, [ObjectDefaultMapper.MainTable.SQLName, ObjectDefaultMapper.SubClassesID]));
      Query.Open;
      while not query.eof do
      begin
        IdList.Add(Query.FieldByName(Field_BOLD_ID).AsString);
        Query.Next;
      end;
      Query.Close;

      if IdList.Count > 0 then
      begin
        BoldLog.LogFmt(sLogObjectsWithIllegalType, [ObjectDefaultMapper.MainTable.SQLName]);
        BoldLog.Log(IdList.CommaText);
        Remedy.Add(format(sCommentRemoveObjectsWithIllegaltype, [ObjectDefaultMapper.MainTable.SQLName]));
        Remedy.Add(format('DELETE FROM %s WHERE BOLD_ID IN (%s);', [ObjectDefaultMapper.MainTable.SQLName, IdList.CommaText])); // do not localize
        Query.AssignSQLText(Format('SELECT * FROM %s WHERE BOLD_ID IN (%s)', [ObjectDefaultMapper.MainTable.SQLName, IdList.CommaText])); // do not localize
      end;
    finally
      FreeAndNil(IdList);
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
  i: integer;
begin
  for i := 0 to Mapper.AllTables.Count - 1 do
    if Mapper.Alltables[i] <> (Mapper.SystemPersistenceMapper.PSSystemDescription as TBoldDefaultSystemDescription).XFilestable then
      Remedy.Add(format('DELETE FROM %s WHERE BOLD_ID IN (%s);', [Mapper.AllTables[i].SQLName, IdList.CommaText])); // do not localize
end;

end.

