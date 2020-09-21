unit BoldMappingInfo;

interface

uses
  Classes,
  BoldNameExpander,
  BoldSQLMappingInfo,
  BoldDbInterfaces;

type
  { TBoldDefaultMappingInfo }
  TBoldDefaultMappingInfo = class(TBoldSQLMappingInfo)
  private
    procedure ScriptForClearData(Script: TStrings; separator: string; terminator: string);
    function AITableName: string;
    function MMTableName: string;
    function OSTableName: string;
    function DbTypeTableName: string;
    function ExpandColumn(ColumnName: String): String;
  public
    procedure ReadDataFromDB(DataBase: IBoldDataBase; ReadDbTypeFromDB, ReadMappingFromDB: Boolean); override;
    procedure ScriptForWriteData(Script: TStrings; Separator: string; ClearFirst: Boolean = true; terminator: string = ''); override;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldDefs;

{ TBoldDeafultMappingInfo }

function TBoldDefaultMappingInfo.AITableName: string;
begin
  result := BoldExpandPrefix(AllInstancesMappingTable_NAME, '', fSystemTablePrefix, MaxDBIdentifierLength, NationalCharConversion)
end;

procedure TBoldDefaultMappingInfo.ScriptForClearData(Script: TStrings; separator: string; terminator: string);

  procedure ClearTable(TableName: string);
  begin
    script.add('DELETE FROM ' + TableName+terminator); // do not localize
    if separator <> '' then
      Script.Add(Separator);
  end;

begin
  ClearTable(AITableName);
  ClearTable(MMTableName);
  ClearTable(OSTableName);
  ClearTable(DbTypeTableName);
end;


function TBoldDefaultMappingInfo.DbTypeTableName: string;
begin
  result := BoldExpandPrefix(TYPETABLE_NAME, '', fSystemTablePrefix, MaxDBIdentifierLength, NationalCharConversion);
end;

function TBoldDefaultMappingInfo.ExpandColumn(ColumnName: String): String;
begin
  result := BoldExpandName(ColumnName, '', xtSQL, MaxDBIdentifierLength, NationalCharConversion);
end;

function TBoldDefaultMappingInfo.MMTableName: string;
begin
  result := BoldExpandPrefix(MemberMappingTable_NAME, '', fSystemTablePrefix, MaxDBIdentifierLength, NationalCharConversion)
end;

function TBoldDefaultMappingInfo.OSTableName: string;
begin
  result := BoldExpandPrefix(ObjectStorageMappingTable_NAME, '', fSystemTablePrefix, MaxDBIdentifierLength, NationalCharConversion)
end;

procedure TBoldDefaultMappingInfo.ReadDataFromDB(DataBase: IBoldDataBase; ReadDbTypeFromDB, ReadMappingFromDB: Boolean);
const
  SELECTFROM = 'SELECT * FROM ';
var
  q: IBoldQuery;
function GetMappernameFromQuery: String;
var
  Field: IBoldField;
begin
  Field := Q.FindField(ExpandColumn(MMT_MAPPERNAME_COLUMN));
  if assigned(Field) then
    result := Field.AsString
  else
    result := '';
end;

begin
  q := DataBase.GetQuery;
  try
    if ReadMappingFromDB then
    begin
      ClearMappingInfo;

      q.AssignSQlText(SELECTFROM + AITableName);
      q.Open;
      while not Q.Eof do
      begin
        AddAllInstancesMapping(q.FieldByName(ExpandColumn(AID_CLASSNAME_COLUMN)).AsString,
                               q.FieldByName(ExpandColumn(AID_TABLENAME_COLUMN)).AsString,
                               q.FieldByName(ExpandColumn(AID_CLASSIDREQUIRED_COLUMN)).AsInteger=1);
        q.Next;
      end;
      q.Close;

      q.AssignSQLText(SELECTFROM + MMTableName);

      q.Open;
      while not Q.Eof do
      begin
        AddMemberMapping(q.FieldByName(ExpandColumn(MMT_CLASSNAME_COLUMN)).AsString,
                               q.FieldByName(ExpandColumn(MMT_MEMBERNAME_COLUMN)).AsString,
                               q.FieldByName(ExpandColumn(MMT_TABLENAME_COLUMN)).AsString,
                               q.FieldByName(ExpandColumn(MMT_COLUMNS_COLUMN)).AsString,
                               GetMapperNameFromQuery);
        q.Next;
      end;
      q.Close;

      q.AssignSQLText(SELECTFROM + OSTableName);

      q.Open;
      while not Q.Eof do
      begin
        AddObjectStorageMapping(q.FieldByName(ExpandColumn(ST_CLASSNAME_COLUMN)).AsString,
                                q.FieldByName(ExpandColumn(ST_TABLENAME_COLUMN)).AsString);
        q.Next;
      end;
      q.Close;
    end;

    if ReadDbTypeFromDB then
    begin
      ClearDbTypes;

      q.AssignSQLText(SELECTFROM + DbTypeTableName);

      q.Open;
      while not Q.Eof do
      begin
        AddTypeIdMapping(q.FieldByName(ExpandColumn(CLASSNAMECOLUMN_NAME)).AsString,
                         q.FieldByName(ExpandColumn(TYPECOLUMN_NAME)).AsInteger);
        q.Next;
      end;
      q.Close;
    end;

  finally
    DataBase.ReleaseQuery(q);
  end;
end;

procedure TBoldDefaultMappingInfo.ScriptForWriteData(Script: TStrings; Separator: string; ClearFirst: Boolean = true; terminator: string = '');
var
  i: integer;
const
  Bool2Int: array[Boolean] of integer=(0, 1);
begin
  if ClearFirst then
    ScriptForClearData(Script, Separator, terminator);

  for i := 0 to fAllInstancesMapping.Count - 1 do
  begin
    Script.Add(format('INSERT INTO %s (%s, %s, %s) VALUES (''%s'', ''%s'', %d)%s', [ // do not localize
      AITableName,
      ExpandColumn(AID_CLASSNAME_COLUMN),
      ExpandColumn(AID_TABLENAME_COLUMN),
      ExpandColumn(AID_CLASSIDREQUIRED_COLUMN),
      AllInstancesMappingInfo[i].ClassExpressionName,
      AllInstancesMappingInfo[i].TableName,
      Bool2Int[AllInstancesMappingInfo[i].classIdrequired],
      terminator]));
    if separator <> '' then
      Script.Add(Separator);
  end;

  for i := 0 to fMemberMapping.Count - 1 do
  begin
    Script.Add(format('INSERT INTO %s (%s, %s, %s, %s, %s) VALUES (''%s'', ''%s'', ''%s'', ''%s'', ''%s'')%s', [ // do not localize
      MMTableName,
      ExpandColumn(MMT_CLASSNAME_COLUMN),
      ExpandColumn(MMT_MEMBERNAME_COLUMN),
      ExpandColumn(MMT_TABLENAME_COLUMN),
      ExpandColumn(MMT_COLUMNS_COLUMN),
      ExpandColumn(MMT_MAPPERNAME_COLUMN),
      MemberMappingInfo[i].ClassExpressionName,
      MemberMappingInfo[i].MemberName,
      MemberMappingInfo[i].TableName,
      MemberMappingInfo[i].Columns,
      MemberMappingInfo[i].MapperName,
      terminator]));
    if separator <> '' then
      Script.Add(Separator);
  end;

  for i := 0 to fObjectStorageMapping.Count - 1 do
  begin
    Script.Add(format('INSERT INTO %s (%s, %s) VALUES (''%s'', ''%s'')%s', [ // do not localize
      OSTableName,
      ExpandColumn(ST_CLASSNAME_COLUMN),
      ExpandColumn(ST_TABLENAME_COLUMN),
      ObjectStorageMappingInfo[i].ClassExpressionName,
      ObjectStorageMappingInfo[i].TableName,
      terminator]));
    if separator <> '' then
      Script.Add(Separator);
  end;

  for i := 0 to fDbTypeMapping.Count - 1 do
  begin
    Script.Add(format('INSERT INTO %s (%s, %s) VALUES (%d, ''%s'')%s', [ // do not localize
      DbTypeTableName,
      ExpandColumn(TYPECOLUMN_NAME),
      ExpandColumn(CLASSNAMECOLUMN_NAME),
      DbTypeMapping[i].DbType,
      DbTypeMapping[i].ClassExpressionName,
      terminator]));
    if separator <> '' then
      Script.Add(Separator);
  end;
end;

end.
