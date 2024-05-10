
{ Global compiler directives }
{$include bold.inc}
unit BoldBDEBatchInterfaces;

interface

uses
  db,
  classes,
  BoldBDEInterfaces;

type
  TBoldBDEBatchQuery = class(TBoldBDEQuery)
  private
    FInBatch: Boolean;
    fParamsInBeginUpdate: Boolean;
    FAccumulatedParams: TParams;
    FAccumulatedSQL: TStringlist;
    function GetAccumulatedParams: TParams;
    function GetAccumulatedSQL: TStringlist;
    function GetHasCachedStatements: boolean;
  protected
    procedure StartSQLBatch; override;
    procedure EndSQLBatch; override;
    procedure FailSQLBatch; override;
    procedure ExecSQL; override;
    procedure BatchExecSQL;
    procedure ExecuteBatch;
    function ParamsContainBlob: Boolean;
    property AccumulatedSQL: TStringlist read GetAccumulatedSQL;
    property AccumulatedParams: TParams read GetAccumulatedParams;
    property InBatch: Boolean read FInBatch write fInBatch;
    property HasCachedStatements: boolean read GetHasCachedStatements;
  public
    destructor destroy; override;
    class procedure InstallBatchQueries;
  end;


var
  BOLDBATCHQUERYMAXSIZE: integer = 30000;
  BOLDBATCHQUERYSEPARATOR: string = ';';

implementation

uses
  SysUtils,
  dbTables,
  BoldDefs;

function ReplaceParamMarkers(const sql: String; Counter: integer; SourceParams, DestParams: TParams): String;
const
  Literals = ['''', '"', '`'];
var
  Name: String;
  Prefix: String;
  tempResult: String;
  CurPos, StartPos: integer;
  oldParam, NewParam: TParam;
  Literal: Boolean;

  function NameDelimiter: Boolean;
  begin
    Result := TempResult[CurPos] in [' ', ',', ';', ')', #13, #10];
  end;

  function IsLiteral: Boolean;
  begin
    Result := TempResult[CurPos] in Literals;
  end;

  function CurrentChar: Char;
  begin
    result := TempResult[CurPos];
  end;


begin
  tempResult := SQL;
  CurPos := 1;
  Literal := False;
  repeat
    while (CurrentChar in LeadBytes) do Inc(CurPos, 2);
    if (CurrentChar = ':') and not Literal and (TempResult[CurPos+1] <> ':') then
    begin
      StartPos := CurPos;
      while (CurrentChar <> #0) and (Literal or not NameDelimiter) do
      begin
        Inc(CurPos);
        while (CurrentChar in LeadBytes) do Inc(CurPos, 2);
        if IsLiteral then
        begin
          Literal := not Literal;
        end;
      end;
      Name := copy(TempResult, StartPos+1, CurPos-(StartPos+1));
      OldParam := SourceParams.ParamByName(Name);

      Prefix := 'P'+IntToStr(DestParams.Count)+'_';

      NewParam := DestParams.CreateParam(OldParam.DataType, prefix+OldParam.Name, OldParam.ParamType);
      NewParam.Assign(OldParam);
      NewParam.Name:=prefix+OldParam.Name;
      if OldParam.IsNull then
        NewParam.Clear
      else
        NewParam.Value := OldParam.value;

      Delete(TempResult, StartPos, length(name)+1);
      Insert('?', TempResult, StartPos);
      CurPos := StartPos;
    end
    else
      if IsLiteral then 
        Literal := not Literal;
    Inc(CurPos);
  until CurPos > Length(TempResult);
  result := TempResult;
end;


{ TBoldBDEBatchQuery }

procedure TBoldBDEBatchQuery.BatchExecSQL;
var
  s: String;
begin
  if ParamsContainBlob then
    ExecuteBatch;
  if not fParamsInBeginUpdate then
  begin
    AccumulatedParams.BeginUpdate;
    fParamsInBeginUpdate:=true;
  end;
  s := ReplaceParamMarkers(Query.SQL.Text, AccumulatedParams.Count, Query.Params, AccumulatedParams);
  s := StringReplace(s, BOLDCRLF, ' ', [rfReplaceAll]);
  s := trim(s) + BOLDBATCHQUERYSEPARATOR;
  AccumulatedSQL.Add(s);
  if ParamsContainBlob or (length(AccumulatedSQL.Text) > BOLDBATCHQUERYMAXSIZE) then
    ExecuteBatch;
end;

destructor TBoldBDEBatchQuery.destroy;
begin
  FreeAndNil(fAccumulatedParams);
  FreeAndNil(fAccumulatedSQL);
  inherited;
end;

procedure TBoldBDEBatchQuery.EndSQLBatch;
begin
  if InBatch and HasCachedStatements then
    ExecuteBatch;
  InBatch := false;
end;

procedure TBoldBDEBatchQuery.ExecSQL;
begin
  if InBatch then
    BatchExecSQL
  else
    inherited;
end;

procedure TBoldBDEBatchQuery.ExecuteBatch;
var
  Driver, DriverMsg: String;
  BatchQuery: TQuery;
begin
  if HasCachedStatements then
  begin
    if fParamsInBeginUpdate then
    begin
      AccumulatedParams.EndUpdate;
      fParamsInBeginUpdate:=false;
    end;
    BatchQuery := TQuery.Create(nil);
    try
      BatchQuery.DatabaseName := Query.DatabaseName;
      BatchQuery.SessionName := Query.SessionName;
      BatchQuery.SQL.AddStrings(AccumulatedSQL);
      BatchQuery.Params.Assign(AccumulatedParams);
      AccumulatedSQL.Clear;
      AccumulatedParams.Clear;
      try
        BatchQuery.ExecSQL;
      except
        on e: exception do
        begin
          DriverMsg := '';
          if assigned(BatchQuery.DBSession) then
          begin
            Driver := Query.DBSession.GetAliasDriverName(query.DatabaseName);
            if pos('INFORMIX', UpperCase(Driver)) = 0 then
              DriverMsg := 'Batch operations has only been tested with Informix' + BOLDCRLF;
          end;
          e.Message := DriverMsg+
            'Batch operation failed: ' + e.MEssage + BOLDCRLF +
            'SQL: '+BatchQuery.SQL.Text;
          raise;
        end;
      end;
    finally
      BatchQuery.Free;
    end;
  end;
end;

procedure TBoldBDEBatchQuery.FailSQLBatch;
begin
  AccumulatedSQL.Clear;
  AccumulatedParams.Clear;
end;

function TBoldBDEBatchQuery.GetAccumulatedParams: TParams;
begin
  if not assigned(fAccumulatedParams) then
    FAccumulatedParams := TParams.Create;
  result := FAccumulatedParams;
end;

function TBoldBDEBatchQuery.GetAccumulatedSQL: TStringlist;
begin
  if not assigned(fAccumulatedSQL) then
    FAccumulatedSQL := TStringList.Create;
  result := FAccumulatedSQL;
end;

function TBoldBDEBatchQuery.GetHasCachedStatements: boolean;
begin
  result := AccumulatedSQL.Count > 0;
end;

class procedure TBoldBDEBatchQuery.InstallBatchQueries;
begin
  BoldBDEQueryClass := TBoldBDEBatchQuery;
end;

function TBoldBDEBatchQuery.ParamsContainBlob: Boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to Query.Params.Count-1 do
    result := result or (Query.Params[i].DataType = ftBlob);
end;

procedure TBoldBDEBatchQuery.StartSQLBatch;
begin
  InBatch := true;
end;

end.
