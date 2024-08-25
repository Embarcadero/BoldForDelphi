{ Global compiler directives }
{$include bold.inc}
unit BoldCustomBlobMapper;

interface

uses
  Db,
  BoldDBInterfaces,
  BoldMeta,
  BoldValueSpaceInterfaces,
  BoldValueInterfaces,
  BoldId,
  BoldTypeNameDictionary,
  BoldPMappers,
  BoldPMappersDefault;

type
  TBoldPMBlobWithSeparateTable = class(TBoldMemberDefaultMapper)
  private
    function GetBlobValue(Id: TBoldObjectId; const ValueSpace: IBoldValueSpace): IBoldBlobContent;
    function IdListToString(IdList: TBoldObjectIdList): String;
  protected
    function GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType; override;
    function GetColumnTypeAsSQL(ColumnIndex: Integer): string; override;
    function GetColumnCount: Integer; override;
    function GetColumnSize(ColumnIndex: Integer): Integer; override;
    function GetInitialColumnName(ColumnIndex: Integer): string; override;
    function CompareField(const ObjectContent: IBoldObjectContents; const Field: IBoldField; ColumnIndex: integer; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList): Boolean; override;
    procedure InitializePSDescriptions; override;
  public
    constructor CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; FetchMode: Integer; TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList); override;
    procedure PMCreate(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList); override;
    procedure PMDelete(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList); override;
    class function CanStore(const ContentName: String): Boolean; override;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldPMappersSQL,
  BoldPSDescriptionsSQL,
  BoldDefs,
  BoldPMapperLists,
  BoldMemberTypeDictionary,
  BoldDefaultStreamNames;

{const
  MemberNameColumn = 'MEMBERNAME';
  }

const
  BLOBDATA_TABLENAME = 'BLOBDATATABLE';
  BLOBDATA_DATACOLUMNNAME = 'BLOBDATA';
  BLOBDATA_MEMBERCOLUMNNAME = 'MEMBERNAME';

(*
  Column 1: Bold_ID:integer
  Column 2: MemberName:String
  Column 3: data:blob
  *)

function TBoldPMBlobWithSeparateTable.GetColumnBDEFieldType(ColumnIndex: Integer): TFieldType;
begin
  case ColumnIndex of
    0: result := ftInteger;
    1: result := ftString;
    2: result := ftBlob;
    else result := inherited GetColumnBDEFieldType( ColumnIndex );
  end;
end;

function TBoldPMBlobWithSeparateTable.GetColumnTypeAsSQL(ColumnIndex: Integer): string;
begin
  case ColumnIndex of
    0: result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForInteger;
    1: result := format(SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForString, [ColumnSize[ColumnIndex]] );
    2: result := SystemPersistenceMapper.SQLDataBaseConfig.ColumnTypeForBlob;
    else
      raise EBold.CreateFmt(sIllegalColumnIndex, [classname, 'GetColumnTypeAsSQL', columnIndex] ); // do not localize
  end;
end;

function TBoldPMBlobWithSeparateTable.GetColumnCount: Integer;
begin
  result := 0;
end;

function TBoldPMBlobWithSeparateTable.GetColumnSize(ColumnIndex: Integer): Integer;
begin
  case ColumnIndex of
    0: result := 0;
    1: result := 40;
    2: result := 0;
    else result := inherited GetColumnSize(ColumnIndex);
  end;
end;

function TBoldPMBlobWithSeparateTable.GetInitialColumnName(ColumnIndex: Integer): string;
begin
  case ColumnIndex of
    0: result := IDCOLUMN_NAME;
    1: result := BLOBDATA_MEMBERCOLUMNNAME;
    2: result := BLOBDATA_DATACOLUMNNAME;
    else result := inherited GetInitialColumnName(ColumnIndex);
  end;

end;

constructor TBoldPMBlobWithSeparateTable.CreateFromMold(moldMember: TMoldMember; moldClass: TMoldClass; Owner: TBoldObjectPersistenceMapper; const MemberIndex: Integer; TypeNameDictionary: TBoldTypeNameDictionary);
begin
  inherited;
  if length(ExpressionName) > ColumnSize[1] then
    raise EBold.CreateFmt(sMemberNameTooLong, [ClassName, ExpressionName, ColumnSize[1]] );
  fCustomCreateUpDate := true;
  fCustomFetch := true;

  if MoldClass.Versioned then
    raise EBold.CreateFmt( sVersionedClassesNotSupported, [ClassName, MoldClass.ExpressionName]);
end;

procedure TBoldPMBlobWithSeparateTable.PMFetch(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; FetchMode: Integer; TranslationList: TBoldIdTranslationList; FailureList: TBoldObjectIdList);
var
  q : IBoldQuery;
  Id: TBoldObjectId;
  i: integer;
  ActionList: TBoldObjectIdList;
  Value: IBoldBlobContent;
  sql: String;
begin
  Q := SystemPersistenceMapper.GetQuery;
  ActionList := TBoldObjectIdList.Create;
  try
    ActionList.AddList(ObjectIdlist);

    sql := Format('SELECT %s, %s FROM %s WHERE %s IN (%s) AND UPPER(%s) = ''%s''', [
      IDCOLUMN_NAME, BLOBDATA_DATACOLUMNNAME, BLOBDATA_TABLENAME,
      IDCOLUMN_NAME, IdListToString(ActionList),
      BLOBDATA_MEMBERCOLUMNNAME, UpperCase(ExpressionName) ] );
    q.AssignSQLText(sql);

    q.Open;
    while not q.Eof do
    begin
      // ClassId param -1 was added due to changes in signature of NewIdFromQuery,
      // check if we can replace -1 with known ClassId if possible in order to get ExactId right away - Daniel
      Id := SystemPersistenceMapper.NewIdFromQuery(q, -1, -1, 0, BOLDMAXTIMESTAMP);
      try
        value := GetBlobValue(Id, ValueSpace);
        Value.asBlob := q.Fields[1].AsBlob;
        ActionList.RemoveByIndex(ActionList.IndexByID[Id]);
      finally
        Id.Free;
      end;
      q.next;
    end;
    for i := 0 to ActionList.Count-1 do
    begin
      value := GetBlobValue(ActionList[i], ValueSpace);
      Value.SetContentToNull;
    end;
  finally
    SystemPersistenceMapper.ReleaseQuery(Q);
    ActionList.Free;
  end;
end;

procedure TBoldPMBlobWithSeparateTable.PMCreate(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList);
var
  q: IBoldExecQuery;
  value: IBoldBlobContent;
  i: integer;
  IdParam, DataParam: IBoldParameter;
begin
  Q := SystemPersistenceMapper.GetExecQuery;
  try
    q.ParamCheck := true;
    q.AssignSQLText(Format('INSERT INTO %s (%s, %s, %s) VALUES (:%s, :%s, :%s)',
      [BLOBDATA_TABLENAME,
      IDCOLUMN_NAME, BLOBDATA_MEMBERCOLUMNNAME, BLOBDATA_DATACOLUMNNAME,
      IDCOLUMN_NAME, BLOBDATA_MEMBERCOLUMNNAME, BLOBDATA_DATACOLUMNNAME]));

    q.ParamByName(BLOBDATA_MEMBERCOLUMNNAME).AsString := UpperCase(ExpressionName);

    IdParam := q.ParamByName(IDCOLUMN_NAME);
    DataParam := q.ParamByName(BLOBDATA_DATACOLUMNNAME);
    for i := 0 to ObjectIdList.Count-1 do
    begin
      IdParam.AsInteger := StrToInt(TranslationList.TranslateToNewId[ObjectIdList[i]].AsString);
      Value := GetBlobValue(ObjectIdList[i], ValueSpace);
      DataParam.AsBlob := Value.asBlob;
      q.ExecSQL;
    end;
  finally
    SystemPersistenceMapper.ReleaseExecQuery(Q);
  end;
end;

procedure TBoldPMBlobWithSeparateTable.PMUpdate(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList);
var
  q : IBoldExecQuery;
  i: integer;
  sql: string;
  value: IBoldBlobContent;
  IdParam, DataParam: IBoldParameter;
begin
  Q := SystemPersistenceMapper.GetExecQuery;
  try
    q.ParamCheck := true;
    sql := Format('UPDATE %s SET %s = :%s WHERE %s = :%s AND UPPER(%s) = ''%s''', [
      BLOBDATA_TABLENAME,
      BLOBDATA_DATACOLUMNNAME, BLOBDATA_DATACOLUMNNAME,
      IDCOLUMN_NAME, IDCOLUMN_NAME,
      BLOBDATA_MEMBERCOLUMNNAME, UpperCase(ExpressionName)]);
    q.AssignSQLText(sql);
    IdParam := q.ParamByName(IDCOLUMN_NAME);
    DataParam := q.ParamByName(BLOBDATA_DATACOLUMNNAME);
    for i := 0 to ObjectIdList.Count-1 do
    begin
      Value := GetBlobValue(ObjectIdList[i], ValueSpace);

      if Value.IsNull then
        DataParam.Clear
      else
        DataParam.AsBlob := Value.asBlob;

      IdParam.AsInteger := StrToInt(ObjectIdList[i].AsString);
      q.ExecSQL;
    end;
  finally
    SystemPersistenceMapper.ReleaseExecQuery(Q);
  end;
end;

procedure TBoldPMBlobWithSeparateTable.PMDelete(ObjectIdList: TBoldObjectIdList; const ValueSpace: IBoldValueSpace; TranslationList: TBoldIdTranslationList);
var
  q: IBoldExecQuery;
begin
  Q := SystemPersistenceMapper.GetExecQuery;
  try
    q.AssignSQLText(Format( 'DELETE FROM %s WHERE %s IN (%s) AND UPPER(%s) = ''%s''', [
      BLOBDATA_TABLENAME,
      IDColumn_NAME, IdListToString(ObjectIdList),
      BLOBDATA_MEMBERCOLUMNNAME, UpperCase(ExpressionName)]));
    q.ExecSQL;
  finally
    SystemPersistenceMapper.ReleaseExecQuery(Q);
  end;
end;

class function TBoldPMBlobWithSeparateTable.CanStore(const ContentName: String): Boolean;
begin
  result := ContentName = BoldContentName_Blob;
end;

function TBoldPMBlobWithSeparateTable.GetBlobValue(Id: TBoldObjectId; const ValueSpace: IBoldValueSpace): IBoldBlobContent;
var
  ObjectContents: IBoldObjectContents;
  value: IBoldValue;
  MemberId: TBoldMemberId;
begin
  Objectcontents := ValueSpace.ObjectContentsByObjectId[ID];
  if not assigned(ObjectContents) then
    raise Exception.CreateFmt(sObjectNotInValueSpace, [ClassName, ObjectpersistenceMapper.ExpressionName, ExpressionName, Id.AsString]);

  MemberID := TBoldMemberId.Create(MemberIndex);
  ObjectContents.EnsureMember(MemberID, ContentName);
  MemberID.Free;
  value := ObjectContents.ValueByIndex[MemberIndex];
  if not assigned(value) then
    raise Exception.CreateFmt(sValueNotInValueSpace, [ClassName, ObjectpersistenceMapper.ExpressionName, ExpressionName, Id.AsString]);
  if not value.QueryInterface(IBoldBlobContent, result) = S_OK then
    raise Exception.CreateFmt(sValueNotBlob, [ClassName, ObjectpersistenceMapper.ExpressionName, ExpressionName]);
end;

function TBoldPMBlobWithSeparateTable.IdListToString(IdList: TBoldObjectIdList): String;
var
  i: integer;
begin
  result := '';
  for i := 0 to IdList.Count-1 do
  begin
    if result <> '' then
      result := result + ', ';
    result := result + IdList[i].AsString;
  end;
end;

function TBoldPMBlobWithSeparateTable.CompareField(
  const ObjectContent: IBoldObjectContents; const Field: IBoldField;
  ColumnIndex: integer; const ValueSpace: IBoldValueSpace;
  TranslationList: TBoldIdTranslationList): Boolean;
begin
  raise Exception.CreateFmt(sCustomCompareRequired, [ClassName]);
end;

procedure TBoldPMBlobWithSeparateTable.InitializePSDescriptions;
var
  BlobTable: TBoldSQLTableDescription;
begin
  inherited;
  BlobTable := SystemPersistenceMapper.PSSystemDescription.SQLTablesList.ItemsBySQLName[BLOBDATA_TABLENAME];
  if not assigned(BlobTable) then
  begin
    BlobTable := TBoldSQLTableDescription.Create(SystemPersistenceMapper.PSSystemDescription, ObjectPersistenceMapper.Versioned);
    BlobTable.SQLName := BLOBDATA_TABLENAME;
    BlobTable.AddColumn(InitialColumnName[0], ColumnTypeAsSQL[0], SystemPersistenceMapper.SQLDataBaseConfig.EffectiveSQLForNotNull, ColumnBDEFieldType[0], ColumnSize[0], false, '');
    Blobtable.AddColumn(InitialColumnName[1], ColumnTypeAsSQL[1], SystemPersistenceMapper.SQLDataBaseConfig.EffectiveSQLForNotNull, ColumnBDEFieldType[1], ColumnSize[1], false, '');

    Blobtable.AddColumn(InitialColumnName[2], ColumnTypeAsSQL[2], '', ColumnBDEFieldType[2], ColumnSize[2], true, '');
    BlobTable.EnsureIndex(InitialColumnName[0] + ',' + InitialColumnName[1], true, true, true );
  end;
end;

initialization
  BoldMemberPersistenceMappers.AddDescriptor(TBoldPMBlobWithSeparateTable, alConcrete);

finalization
  if BoldMemberPersistenceMappersAssigned and BoldMemberTypesAssigned then
    BoldMemberPersistenceMappers.RemoveDescriptorByClass(TBoldPMBlobWithSeparateTable);
end.
