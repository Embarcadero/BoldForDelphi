unit BoldSQLExecuter;

interface

uses
  Classes,
  db,
  DBTables;

type
  TOnSQLError = procedure(SQL: TStrings; ErrorMessage: String) of object;
  TOnSQLExecute = procedure(SQL: TStrings; Progress, TotalLines: integer) of object;

  TBoldSQLExecuter = class(TComponent)
  private
    fUseTransaction: Boolean;
    fOnSQLError: TOnSQLError;
    fOnSQLExecute: TOnSQLExecute;
  public
    procedure ExecuteSQL(SQLScript: TStrings; Query: TQuery );
  published
    property UseTransaction: Boolean read fUseTransaction write fUseTransaction;
    property OnSQLError: TOnSQLError read fOnSQLError write fOnSQLError;
    property OnSQLExecute: TOnSQLExecute read fOnSQLExecute write fOnSQLExecute;
  end;

procedure Register;

implementation

uses
  SysUtils,
  BoldUtils;
//procedure TBoldSQLExecuter.ExecuteSQL(SQLScript: TStrings; DataBaseName: String);
procedure TBoldSQLExecuter.ExecuteSQL(SQLScript: TStrings; Query: TQuery);
var
  i: integer;
//  Query: TQuery;
  ErrorOccured: Boolean;
  DataBase: TDataBase;
  tempStr: String;

begin
//  Query := TQuery.Create(nil);
//  Query.DataBaseName := DataBaseName;
  ErrorOccured := false;
{
  if UseTransaction then
  begin
    DataBase := TDataBase.Create(nil);
    DataBase.DatabaseName := DataBaseName;
    DataBase.StartTransaction;
  end;
}
  Query.SQL.Clear;

  try
    for i := 0 to SQLScript.Count-1 do begin
      try
        if (SQLScript[i] <> '') and
          (copy(trim(SQLScript[i]), 1, 2) <> '//') then
        begin
          tempStr := trim(SQLScript[i]);
          Query.SQL.Add(TempStr);
          if ((i = SQLScript.Count-1) or (TempStr[Length(TempStr)] = ';')) and (Query.SQL.Count > 0) then begin
            if assigned(OnSQLExecute) then
              OnSQLExecute(Query.SQL, i, SQLScript.Count);
            Query.ExecSQL;
            Query.Close;
            Query.SQL.Clear;
          end;
        end;
      except
        on e:exception do
        begin
          ErrorOccured := true;
          if assigned(OnSQLError) then
            OnSQLError(Query.SQL, e.Message)
          else
            raise;
        end;
      end;
    end;
  finally
{
    if UseTransaction then
    begin
      if ErrorOccured then
        DataBase.RollBack
      else
        DataBase.Commit;
    end;
}
  end;
end;

procedure Register;
begin
  RegisterComponents('Bold', [TBoldSQLExecuter]);
end;

end.


UPDATE BorttagnaFyndTyperFTBorttage33 SET FTBorttagenFran = -1 WHERE BOLD_ID IN (1503);
UPDATE BorttagnaObjektAOBorttagenFran SET AOBorttagenFran = -1 WHERE BOLD_ID IN (1506);
UPDATE OskadeMetodPrio SET OskadeMetod = -1 WHERE BOLD_ID IN (1308);
UPDATE TillagdaFyndTyperFTTillagdTill SET FTTillagdTill = -1 WHERE BOLD_ID IN (1502,1450,1449);
UPDATE TillagdaObjektAOTillagdTill SET AOTillagdTill = -1 WHERE BOLD_ID IN (1505,1504,1451);
