{ Global compiler directives }
{$include bold.inc}
unit BoldMappingInfo;

interface

uses
  Classes,
  BoldNameExpander,
  BoldSQLMappingInfo,
  BoldDbInterfaces;

type

  TBoldDefaultMappingInfo = class(TBoldSQLMappingInfo)
  private
    procedure ScriptForClearData(Script: TStrings; Separator: string; Terminator: string);
    function AITableName: string;
    function MMTableName: string;
    function OSTableName: string;
    function DbTypeTableName: string;
    function ExpandColumn(ColumnName: String): String;
  public
    procedure ReadDataFromDB(DataBase: IBoldDataBase; ReadDbTypeFromDB, ReadMappingFromDB: Boolean); override;
    procedure ScriptForWriteData(DataBase: IBoldDataBase; Script: TStrings; ClearFirst: Boolean;
        Separator: String; Terminator: String); override;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldDefs;

{ TBoldDefaultMappingInfo }

function TBoldDefaultMappingInfo.AITableName: string;
begin
  result := BoldExpandPrefix(AllInstancesMappingTable_NAME, '', fSystemTablePrefix, MaxDBIdentifierLength, NationalCharConversion)
end;

procedure TBoldDefaultMappingInfo.ScriptForClearData(Script: TStrings; Separator: string; Terminator: string);

  procedure ClearTable(TableName: string);
  begin
    script.add('DELETE FROM ' + TableName+Terminator); // do not localize
    if Separator <> '' then
      Script.Add(Separator);
  end;

  procedure AddColumnIndexColumn;
  var
    sScript: string;
  begin
    if (FCurrentDatabase <> nil) and
       (FCurrentDatabase.SQLDatabaseConfig <> nil) then
    begin
      sScript := Format('ALTER TABLE %s ADD %s '+FCurrentDatabase.SQLDatabaseConfig.ColumnTypeForInteger+' DEFAULT 0 '+FCurrentDatabase.SQLDatabaseConfig.SQLForNotNull,
          [MMTableName, MMT_INDEX_COLUMN]);
      sScript := FCurrentDatabase.SQLDatabaseConfig.GetIfColumnNotExistsQuery(
          MMTableName, MMT_INDEX_COLUMN, sScript);
      sScript := sScript + Terminator;
      script.add(sScript);
      if Separator <> '' then
        Script.Add(Separator);
      end;
  end;

begin
  ClearTable(AITableName);
  ClearTable(MMTableName);
  ClearTable(OSTableName);
  ClearTable(DbTypeTableName);
{$IFDEF IndexColumn}
  AddColumnIndexColumn;
{$ENDIF}
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
var
  bColumnIndex: Boolean;
  vClassNameField: IBoldField;
  vTableNameField: IBoldField;
  vMemberNameField: IBoldField;
  vColumnsField: IBoldField;
  vClassIdRequiredField: IBoldField;
  vTypeColumnield: IBoldField;
begin
  q := DataBase.GetQuery;
  try
    if ReadMappingFromDB then
    begin
      ClearMappingInfo;

      q.AssignSQlText(SELECTFROM + AITableName);
      q.Open;
      vClassNameField := q.FieldByName(ExpandColumn(AID_CLASSNAME_COLUMN));
      vTableNameField := q.FieldByName(ExpandColumn(AID_TABLENAME_COLUMN));
      vClassIdRequiredField := q.FieldByName(ExpandColumn(AID_CLASSIDREQUIRED_COLUMN));
      while not Q.Eof do
      begin
        AddAllInstancesMapping(vClassNameField.AsString,
                               vTableNameField.AsString,
                               vClassIdRequiredField.AsInteger=1);
        q.Next;
      end;
      q.Close;

      q.AssignSQLText(SELECTFROM + MMTableName);

      q.Open;
      vClassNameField := q.FieldByName(ExpandColumn(MMT_CLASSNAME_COLUMN));
      vTableNameField := q.FieldByName(ExpandColumn(MMT_TABLENAME_COLUMN));
      vMemberNameField := q.FieldByName(ExpandColumn(MMT_MEMBERNAME_COLUMN));
      vColumnsField := q.FieldByName(ExpandColumn(MMT_COLUMNS_COLUMN));
      while not Q.Eof do
      begin
        //Fallback, if the old MemberMapping was used
        if q.FindField(ExpandColumn(MMT_INDEX_COLUMN)) = nil then begin
          bColumnIndex := False;
        end else begin
          bColumnIndex := q.FieldByName(ExpandColumn(MMT_INDEX_COLUMN)).AsInteger = 1;
        end;
        AddMemberMapping(vClassNameField.AsString,
                               vMemberNameField.AsString,
                               vTableNameField.AsString,                              
                               // Remove linebreaks, to make MappingInfo comparable in TBoldDataBaseEvolutor.MoveDataAction
                               StringReplace(vColumnsField.AsString, #13#10, '', [rfReplaceAll]),
                               GetMapperNameFromQuery,
                               bColumnIndex);
        q.Next;
      end;
      q.Close;

      q.AssignSQLText(SELECTFROM + OSTableName);

      q.Open;
      vClassNameField := q.FieldByName(ExpandColumn(ST_CLASSNAME_COLUMN));
      vTableNameField := q.FieldByName(ExpandColumn(ST_TABLENAME_COLUMN));
      while not Q.Eof do
      begin
        AddObjectStorageMapping(vClassNameField.AsString,
                                vTableNameField.AsString);
        q.Next;
      end;
      q.Close;
    end;

    if ReadDbTypeFromDB then
    begin
      ClearDbTypes;

      q.AssignSQLText(SELECTFROM + DbTypeTableName);

      q.Open;
      vClassNameField := q.FieldByName(ExpandColumn(CLASSNAMECOLUMN_NAME));
      vTypeColumnield := q.FieldByName(ExpandColumn(TYPECOLUMN_NAME));
      while not Q.Eof do
      begin
        AddTypeIdMapping(vClassNameField.AsString,
                         vTypeColumnield.AsInteger);
        q.Next;
      end;
      q.Close;
    end;
  finally
    DataBase.ReleaseQuery(q);
  end;
end;

procedure TBoldDefaultMappingInfo.ScriptForWriteData(DataBase: IBoldDataBase;
    Script: TStrings; ClearFirst: Boolean; Separator: String; Terminator: String);
var
  i,row,Limit: integer;
  vInsertSql: string;
  sl: TStringList;
const
  Bool2Int: array[Boolean] of integer=(0, 1);
begin
  FCurrentDatabase := DataBase;
  sl := TStringList.Create;
  sl.LineBreak := ' ';
  try
    if ClearFirst then
      ScriptForClearData(Script, Separator, Terminator);
    Limit := FCurrentDatabase.SQLDatabaseConfig.MultiRowInsertLimit;
    vInsertSql := format('INSERT INTO %s (%s, %s, %s) VALUES ', [
          AITableName,
          ExpandColumn(AID_CLASSNAME_COLUMN),
          ExpandColumn(AID_TABLENAME_COLUMN),
          ExpandColumn(AID_CLASSIDREQUIRED_COLUMN)
          ]);
    row := 0;
    for i := 0 to fAllInstancesMapping.Count - 1 do
    begin
      sl.Add(format('(''%s'', ''%s'', %d)%s', [ // do not localize
          AllInstancesMappingInfo[i].ClassExpressionName,
          AllInstancesMappingInfo[i].TableName,
          Bool2Int[AllInstancesMappingInfo[i].classIdrequired],
          Terminator]));
      if row = 0 then
        sl[sl.count-1] := vInsertSql + sl[sl.count-1]
      else
        sl[sl.count-1] := ',' + sl[sl.count-1];
      inc(row);
      if (row = limit) or (i = fAllInstancesMapping.Count - 1) then
      begin
        Script.Add(sl.Text);
        sl.clear;
        if Separator <> '' then
          Script.Add(Separator);
        row := 0;
      end;
    end;
    Assert(row = 0);    
    Assert(sl.count = 0);
{$IFDEF IndexColumn}
    vInsertSql := format('INSERT INTO %s (%s, %s, %s, %s, %s, %s) VALUES ',
{$ELSE}
    vInsertSql := format('INSERT INTO %s (%s, %s, %s, %s, %s) VALUES ',
{$ENDIF}
          [MMTableName,
          ExpandColumn(MMT_CLASSNAME_COLUMN),
          ExpandColumn(MMT_MEMBERNAME_COLUMN),
          ExpandColumn(MMT_TABLENAME_COLUMN),
          ExpandColumn(MMT_COLUMNS_COLUMN),
          ExpandColumn(MMT_MAPPERNAME_COLUMN)
{$IFDEF IndexColumn}
          ,ExpandColumn(MMT_INDEX_COLUMN)
{$ENDIF}
    ]);
    for i := 0 to fMemberMapping.Count - 1 do
    begin
      sl.Add(Format(
{$IFDEF IndexColumn}
          '(''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %d)%s', [ // do not localize
{$ELSE}
          '(''%s'', ''%s'', ''%s'', ''%s'', ''%s'')%s', [ // do not localize
{$ENDIF}          
          MemberMappingInfo[i].ClassExpressionName,
          MemberMappingInfo[i].MemberName,
          MemberMappingInfo[i].TableName,
          MemberMappingInfo[i].Columns,
          MemberMappingInfo[i].MapperName,
{$IFDEF IndexColumn}
          Integer(MemberMappingInfo[i].ColumnIndex),
{$ENDIF}
          terminator]));
      if row = 0 then
        sl[sl.count-1] := vInsertSql + sl[sl.count-1]
      else
        sl[sl.count-1] :=  ',' + sl[sl.count-1];
      inc(row);
      if (row = limit) or (i = fMemberMapping.Count - 1) then
      begin
        Script.Add(sl.Text);
        sl.clear;        
        if Separator <> '' then
          Script.Add(Separator);
        row := 0;
      end;
    end;
    Assert(row = 0);
    Assert(sl.count = 0);
    vInsertSql := format('INSERT INTO %s (%s, %s) VALUES ',
          [OSTableName,
          ExpandColumn(ST_CLASSNAME_COLUMN),
          ExpandColumn(ST_TABLENAME_COLUMN)
          ]);
    for i := 0 to fObjectStorageMapping.Count - 1 do
    begin
      sl.Add(format('(''%s'', ''%s'')%s', [ // do not localize
          ObjectStorageMappingInfo[i].ClassExpressionName,
          ObjectStorageMappingInfo[i].TableName,
          Terminator]));
      if row = 0 then
        sl[sl.count-1] := vInsertSql + sl[sl.count-1]
      else
        sl[sl.count-1] :=  ',' + sl[sl.count-1];
      inc(row);        
      if (row = limit) or (i = fObjectStorageMapping.Count - 1) then
      begin
        Script.Add(sl.Text);
        sl.clear;        
        if Separator <> '' then
          Script.Add(Separator);
        row := 0;
      end;
    end;
    Assert(row = 0);
    Assert(sl.count = 0);
    vInsertSql := format('INSERT INTO %s (%s, %s) VALUES ',
          [DbTypeTableName,
          ExpandColumn(TYPECOLUMN_NAME),
          ExpandColumn(CLASSNAMECOLUMN_NAME)]);
    for i := 0 to fDbTypeMapping.Count - 1 do
    begin
      sl.Add(format('(%d, ''%s'')%s', [ // do not localize
          DbTypeMapping[i].DbType,
          DbTypeMapping[i].ClassExpressionName,
          Terminator]));
      if row = 0 then
        sl[sl.count-1] := vInsertSql + sl[sl.count-1]
      else
        sl[sl.count-1] :=  ',' + sl[sl.count-1];
      inc(row);        
      if (row = limit) or (i = fDbTypeMapping.Count - 1) then
      begin
        Script.Add(sl.Text);
        sl.clear;        
        if Separator <> '' then
          Script.Add(Separator);
        row := 0;
      end;
    end;
    Assert(row = 0);    
    Assert(sl.count = 0);
  finally
    FCurrentDatabase := nil;
    sl.free;
  end;
end;

end.
