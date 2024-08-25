{ Global compiler directives }
{$include bold.inc}
unit BoldSQLQuery;

interface

uses
  Db,
  Classes,
  BoldBase,
  BoldSQLDataBaseConfig,
  BoldId,
  BoldContainers,
  BoldPSDescriptionsSQL;

type
  {forward declarations }
  TBoldSQLJoin = class;
  TBoldSQLTableReference = class;
  TBoldSQLColumnReference = class;
  TBoldSqlWCF = class;
  TBoldSQLWCFBinary = class;
  TBoldSQLWCFBinaryInfix = class;
  TBoldSQLWCFBinaryPrefix = class;
  TBoldSQLWCFUnary = class;
  TBoldSQLWCFUnaryPrefix = class;
  TBoldSQLWCFUnaryPostfix = class;
  TBoldSQLWCFString = class;
  TBoldSQLWCFInteger = class;
  TBoldSQLWCFFloat = class;
  TBoldSQLWCFTime = class;
  TBoldSQLWCFDate = class;
  TBoldSQLWCFColumnRef = class;
  TBoldSQLWCFJoin = class;
  TBoldSQLWCFWithQuery = class;
  TBoldSQLWCFExists = class;
  TBoldSQLWCFInQuery = class;
  TBoldSQLWCFGroupFunction = class;
  TBoldSQLWCFSize = class;
  TBoldSQLWCFGroupFunctionWithColumn = class;
  TBoldSQLWCFGenericExpression = class;
  TBoldSQLNameSpace = class;
  TBoldSQLQuery = class;
  TBoldSqlTableReferenceList = class;

  TBoldSQLQueryMode = (qmSelect, qmInsert, qmUpdate, qmDelete);

  { TBoldSQLJoin }
  TBoldSQLJoin = class(TBoldMemoryManagedObject)
  private
    fColumnRef1: TBoldSQLColumnReference;
    fColumnRef2: TBoldSQLColumnReference;
    function getJoinStatement: String;
    function OrderedMatches(ColumnRef1, ColumnRef2: TBoldSqlColumnReference): Boolean;
  public
    constructor Create(ColumnRef1, ColumnRef2: TBoldSqlColumnReference);
    function Matches(ColumnRef1, ColumnRef2: TBoldSqlColumnReference): Boolean;
    property JoinStatement: String read getJoinStatement;
  end;

  { TBoldSQLTableReference }
  TBoldSQLTableReference = class(TBoldMemoryManagedObject)
  private
    fTableDescription: TBoldSQLTableDescription;
    fColumnReferences: TBoldObjectArray;
    fQuery: TBoldSQLQuery;
    fAliasName: String;
    function GetAliasName: String;
    function GetTableAliasDeclaration: String;
    procedure EnsureColumnExists(const ColumnName, Operation: String);
  public
    constructor Create(Query: TBoldSQLQuery);
    destructor Destroy; override;
    function GetColumnReference(const ColumnName: String): TBoldSQLColumnReference;
    property TableDescription: TBoldSQLTableDescription read fTableDescription;
    property AliasName: String read GetAliasName;
    property TableAliasDeclaration: String read GetTableAliasDeclaration;
  end;

  { TBoldSQLColumnReference }
  TBoldSQLColumnReference = class(TBoldMemoryManagedObject)
  private
    fColumnDescription: TBoldSQLColumnDescription;
    fTableReference: TBoldSQLTableReference;
    function GetPrefixedColumnName: String;
  public
    constructor create(TableReference: TBoldSQLTableReference; ColumnDescription: TBoldSQLColumnDescription);
    property TableReference: TBoldSQLTableReference read fTableReference;
    property ColumnDescription: TBoldSQLColumnDescription read fColumnDescription;
    property PrefixedColumnName: String read GetPrefixedColumnName;
  end;

  { TBoldSQLOrderByInfo }
  TBoldSQLOrderByInfo = class(TBoldMemoryManagedObject)
  private
    FColumn: TBoldSQLColumnReference;
    FDescending: Boolean;
  public
    constructor Create(Column: TBoldSQLColumnReference; const Descending: Boolean);
    property Column: TBoldSQLColumnReference read FColumn;
    property Descending: Boolean read FDescending;
  end;

  { TBoldSqlWCF }
  TBoldSqlWCF = class(TBoldMemoryManagedObject)
  public
    function GetAsString(Query: TBoldSQlQuery): String; virtual; abstract;
  end;

  { TBoldSQLWCFBinary }
  TBoldSQLWCFBinary = class(TBoldSqlWCF)
  private
    fArg1: TBoldSqlWCF;
    fArg2: TBoldSqlWCF;
    fSymbol: string;
  public
    constructor Create(arg1, arg2: TBoldSqlWCF; const Symbol: String);
    destructor Destroy; override;
  end;

  { TBoldSQLWCFBinaryInfix }
  TBoldSQLWCFBinaryInfix = class(TBoldSqlWCFBinary)
  public
    class function CreateWCFForIdList(ColumnRef: TBoldSQLColumnReference; IdList: TBoldObjectIdList): TBoldSQLWCFBinaryInfix;
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLWCFBinaryPrefix }
  TBoldSQLWCFBinaryPrefix = class(TBoldSqlWCFBinary)
  public
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLWCFXOR }
  TBoldSQLWCFXOR = class(TBoldSqlWCF)
  private
    fArg1: TBoldSqlWCF;
    fArg2: TBoldSqlWCF;
  public
    constructor Create(arg1, arg2: TBoldSqlWCF);
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLWCFUnary }
  TBoldSQLWCFUnary = class(TBoldSqlWCF)
  private
    fArg1: TBoldSqlWCF;
    fSymbol: string;
  public
    constructor Create(arg1: TBoldSqlWCF; Symbol: String);
    destructor Destroy; override;
  end;

  { TBoldSQLWCFUnaryPrefix }
  TBoldSQLWCFUnaryPrefix = class(TBoldSqlWCFUnary)
  public
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLWCFUnaryTransforLikeString }
  TBoldSQLWCFUnaryTransformLikeString = class(TBoldSqlWCFUnary)
  public
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLWCFUnaryPostfix }
  TBoldSQLWCFUnaryPostfix = class(TBoldSqlWCFUnary)
  public
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLWCFString }
  TBoldSQLWCFString = class(TBoldSqlWCF)
  private
    fStr: String;
  public
    constructor Create(const Value: String);
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLWCFInteger }
  TBoldSQLWCFInteger = class(TBoldSqlWCF)
  private
    fInt: integer;
  public
    constructor Create(Value: integer);
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLWCFFloat }
  TBoldSQLWCFFloat = class(TBoldSqlWCF)
  private
    fFloat: Double;
    fParam:TParam;
  public
    constructor Create(Value: Double);
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLWCFDate }
  TBoldSQLWCFDate = class(TBoldSqlWCF)
  private
    fDate: TDateTime;
    fParam:TParam;
  public
    constructor Create(Value: TDateTime);
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLWCFTime }
  TBoldSQLWCFTime = class(TBoldSqlWCF)
  private
    fTime: TDateTime;
    fParam:TParam;
  public
    constructor Create(Value: TDateTime);
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;



  { TBoldSQLWCFColumnRef }
  TBoldSQLWCFColumnRef = class(TBoldSqlWCF)
  private
    fColumnref: TBoldSqlColumnReference;
  public
    constructor Create(ColumnRef: TBoldSqlColumnReference);
    function GetAsString(Query: TBoldSQlQuery): String; override;
    property ColumnRef: TBoldSQLColumnReference read fColumnRef;
  end;

  { TBoldSQLWCFJoin }
  TBoldSQLWCFJoin = class(TBoldSqlWCF)
  private
    fColumnref1: TBoldSqlColumnReference;
    fColumnref2: TBoldSqlColumnReference;
  public
    constructor Create(ColumnRef1, ColumnRef2: TBoldSqlColumnReference);
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLWCFWithQuery }
  TBoldSQLWCFWithQuery = class(TBoldSqlWCF)
  private
    fQuery: TBoldSQLQuery;
  protected
    function QueryAsString: String;
    procedure CopyParams(Query: TBoldSQlQuery);
  public
    constructor Create(query: TBoldSQLQuery);
    destructor Destroy; override;
    function GetAsString(Query: TBoldSQlQuery): String; override;
    property Query: TBoldSQLQuery read fQuery;
  end;

  { TBoldSQLWCFExists }
  TBoldSQLWCFExists = class(TBoldSQLWCFWithQuery)
  public
    constructor create(query: TBoldSQLQuery; MainTable: TBoldSqlTableReference);
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLWCFInQuery }
  TBoldSQLWCFInQuery = class(TBoldSQLWCFWithQuery)
  private
    fArg1: TBoldSqlWCF;
  public
    constructor Create(Arg1: TBoldSqlWCF; Query: TBoldSQLQuery; MainTable: TBoldSqlTableReference);
    destructor Destroy; override;
    function GetAsString(Query: TBoldSQlQuery): String; override;
    property Arg1: TBoldSqlWCF read fArg1;
  end;

  { TBoldSQLWCFGroupFunction }
  TBoldSQLWCFGroupFunction = class(TBoldSQLWCFWithQuery)
  protected
    procedure PreGetAsString; virtual;
  public
    constructor create(query: TBoldSQLQuery);
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLWCFSize }
  TBoldSQLWCFSize = class(TBoldSQLWCFGroupFunction)
  protected
    procedure PreGetAsString; override;
  end;

  { TBoldSQLWCFGroupFunctionWithColumn }
  TBoldSQLWCFGroupFunctionWithColumn = class(TBoldSQLWCFGroupFunction)
  public
    constructor create(query: TBoldSQLQuery; ColumnRef: TBoldSqlColumnReference; Operation: String);
  end;

  { TBoldSQLWCFGenericExpression }
  TBoldSQLWCFGenericExpression = class(TBoldSqlWCF)
  private
    fExpr: String;
  public
    constructor Create(const Expr: String);
    function GetAsString(Query: TBoldSQlQuery): String; override;
  end;

  { TBoldSQLNameSpace }
  TBoldSQLNameSpace = class(TBoldMemoryManagedObject)
  private
    fUsedNames: TStringList;
    fUsedParams: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetUnusedParamNumber: integer;

    function GetUniqueAlias(const TableName: String): String;
  end;

  { TBoldSQLQuery }
  TBoldSQLQuery = class(TBoldMemoryManagedObject)
  private
    fMode: TBoldSQLQueryMode;
    fJoins: TBoldObjectArray;
    fTableReferences: TBoldSqlTableReferenceList;
    fSystemDescription: TBoldSQLSystemDescription;
    fColumnsToRetrieve: TBoldObjectArray;
    fColumnsToOrderBy: TBoldObjectArray;
    fDistinct: Boolean;
    fLimit: Integer;
    fLimitTop: Boolean;
    fWhereClauseFragments: TBoldObjectArray;
    fParams: TParams;
    fRetrieveCountStar: Boolean;
    fGroupOperation: String;
    fNameSpace: TBoldSQlNameSpace;
    fMainTable: TBoldSqlTableReference;
    fIgnoreHistoricObjects: Boolean;
    fSQLDatabaseConfig: TBoldSQLDatabaseConfig;
    function GetColumnToRetrieve(Index: Integer): TBoldSQLColumnReference;
    function GetJoin(index: integer): TBoldSQLJoin;
    function GetUniqueAlias(const TableName: String): String;
    function GetWCF(index: integer): TBoldSqlWCF;
    procedure GenerateSelect(Strings: TStrings);
    procedure EnsureTableExists(tableName, Operation: String);
    function GetAsString: string;
  protected
    property ColumnToRetrieve[Index: Integer]: TBoldSQLColumnReference read GetColumnToRetrieve;
    property Join[Index: integer]: TBoldSQLJoin read GetJoin;
    property WCF[index: integer]: TBoldSqlWCF read GetWCF;
  public
    constructor Create(Mode: TBoldSQLQueryMode; SystemDescription: TBoldSQLSystemDescription; SQLDatabaseConfig: TBoldSQLDatabaseConfig; NameSpace: TBoldSqlNameSpace);
    destructor Destroy; override;
    function AddJoin(ColumnRef1, ColumnRef2: TBoldSQLColumnReference): TBoldSQLJoin;
    function AddTableReference(const TableName: String): TBoldSQLTableReference;
    procedure AddColumnToRetrieve(ColumnReference: TBoldSQLColumnReference);
    procedure AddColumnToOrderBy(Columnreference: TBoldSQlColumnReference; const
        Descending: Boolean);
    procedure GenerateSQL(Strings: TStrings);
    function AddParam(const name: string=''): TParam;
    function HastableReferenceInList(TableReference: TBoldSQLTablereference): boolean;
    procedure RetrieveCountStar;
    procedure AddWCF(WCF: TBoldSqlWCF);
    procedure ClearColumnsToRetrieve;
    procedure SetLimit(const Top: Boolean = True; const Limit: Integer = 1);
    property Mode: TBoldSQLQueryMode read fMode;
    property SystemDescription: TBoldSQLSystemDescription read fSystemDescription;
    property MainTable: TBoldSQLTableReference read fMaintable;
    property AsString: string read GetAsString;
    property Distinct: Boolean read fDistinct write fDistinct;
    property SQLDatabaseConfig: TBoldSQLDatabaseConfig read fSQLDatabaseConfig;
    property Params: TParams read fParams;
    property IgnoreHistoricObjects: Boolean read fIgnoreHistoricObjects write fIgnoreHistoricObjects;
    property TableReferences: TBoldSQLTableReferenceList read fTableReferences;
  end;

  { TBoldSqlTableReferenceList }
  TBoldSqlTableReferenceList = class(TBoldObjectArray)
  private
    function Get(Index: Integer): TBoldSqlTableReference;
    procedure Put(Index: Integer; const Value: TBoldSqlTableReference);
  public
    function Add(Item: TBoldSqlTableReference): Integer;
    procedure Insert(Index: Integer; Item: TBoldSqlTableReference);
    property Items[Index: Integer]: TBoldSqlTableReference read Get write Put; default;
  end;

implementation

uses
  BoldDefs,
  BoldPSDescriptionsDefault,
  SysUtils,
  BoldIndex,
{$IFNDEF BOLD_UNICODE}
  StringBuilder,
{$ENDIF}
  BoldUtils,
  BoldIndexableList;


{ TBoldSQLQuery }


function TBoldSQLQuery.AddTableReference(const TableName: String): TBoldSQLTableReference;
var
  RootTable: TBoldSQLTableDescription;
  RootTableRef: TBoldSQLTableReference;
  NowTimeWCF: TBoldSQLWCFInteger;
  EndTimeColWCF: TBoldSQLWCFColumnRef;
  MyIdCol, RootIdCol,
  MyStartCol, RootStartCol: TBoldSQLColumnReference;
begin
  EnsureTableExists(TableName, 'AddTableReference');

  result := TBoldSQLTableReference.Create(self);
  Result.fTableDescription := SystemDescription.SQLTablesList.ItemsBySQLName[TableName];
  fTableReferences.Add(result);
  if Result.TableDescription.Versioned and IgnoreHistoricObjects then
  begin
    RootTable := (SystemDescription as TBoldDefaultSystemDescription).RootTable;
    if Result.TableDescription = RootTable then
    begin
      NowTimeWCF := TBoldSQLWCFInteger.Create(BOLDMAXTIMESTAMP);
      EndTimeColWCF := TBoldSQLWCFColumnRef.Create(result.GetColumnReference(TIMESTAMPSTOPCOLUMNNAME));
      AddWCF(TBoldSQLWCFBinaryInfix.Create(NowTimeWCF, EndTimeColWCF, '='));
    end
    else
    begin
      RootTableRef := AddTableReference(RootTable.SQLName);

      MyStartCol := result.GetColumnReference(TIMESTAMPSTARTCOLUMNNAME);
      RootStartCol := RootTableRef.GetColumnReference(TIMESTAMPSTARTCOLUMNNAME);
      AddJoin(MyStartCol, RootStartCol);
      MyIdCol := result.GetColumnReference(IDCOLUMN_NAME);
      RootIdCol := RootTableRef.GetColumnReference(IDCOLUMN_NAME);
      AddJoin(MyIdCol, RootIdCol);
    end;
  end;

end;

constructor TBoldSQLQuery.Create(Mode: TBoldSQLQueryMode; SystemDescription: TBoldSQLSystemDescription; SQLDatabaseConfig: TBoldSQLDatabaseConfig; NameSpace: TBoldSqlNameSpace);
begin
  inherited Create;
  fMode := Mode;
  fJoins := TBoldObjectArray.Create(0, [bcoDataOwner]);
  fTableReferences := TBoldSqlTableReferenceList.Create(0, [bcoDataOwner]);
  fColumnsToRetrieve := TBoldObjectArray.Create(0, []);
  fColumnsToOrderBy := TBoldObjectArray.Create(0, [bcoDataOwner]);
  fSQLDatabaseConfig := SQLDatabaseConfig;
  fWhereClauseFragments := TBoldObjectArray.Create(0, [bcoDataOwner]);
  fSystemDescription := SystemDescription;
  fParams := TParams.Create;
  fNameSpace := nameSpace;
  fIgnoreHistoricObjects := true;
  fDistinct := False;
  fLimit := 0;
  fLimitTop := True;
end;

destructor TBoldSQLQuery.Destroy;
begin
  FreeAndNil(fJoins);
  FreeAndNil(fTableReferences);
  FreeAndNil(fColumnsToRetrieve);
  FreeAndNil(fColumnsToOrderBy);
  FreeAndNil(fWhereClauseFragments);
  FreeAndNil(fParams);
  inherited;
end;

{$HINTS OFF}
procedure TBoldSQLQuery.GenerateSQL(Strings: TStrings);
begin
  case Mode of
    qmSelect: GenerateSelect(Strings)
  end;
  // this code is mainly to avoid GetAsString to be linked out of the project since it is
  // used for debugging only
  if AsString = '' then
    Strings := Strings;
end;
{$HINTS ON}

procedure TBoldSQLQuery.GenerateSelect(Strings: TStrings);

  procedure AddOrderByClause(const LimitTop: Boolean);
  var
    temp: string;
    i: Integer;
    OrderByInfo: TBoldSQLOrderByInfo;
  begin
    temp := '';
    if (fColumnsToOrderBy.Count = 0) and (fLimit > 0) then begin
      // Use default sort on BOLD_ID column for limit, when no sort was spezified
      if Assigned(MainTable) then begin
        temp := MainTable.GetColumnReference(IDCOLUMN_NAME).PrefixedColumnName;
      end else begin
        temp := TableReferences[TableReferences.Count - 1].GetColumnReference(IDCOLUMN_NAME).PrefixedColumnName;
      end;
      if not LimitTop then begin
        temp := temp + ' DESC'; // do not localize
      end;
    end;

    if (fColumnsToOrderBy.Count > 0) then begin
      for i := 0 to fColumnsToOrderBy.Count - 1 do
      begin
        OrderByInfo := TBoldSQLOrderByInfo(fColumnsToOrderBy[i]);
        if temp <> '' then
          temp := temp + ', ';
        temp := temp + OrderByInfo.Column.PrefixedColumnName;
        // If last records are fetched (not LimitTop) then sorting must be reversed.
        if not (OrderByInfo.Descending xor LimitTop) then begin
          temp := temp + ' DESC'; // do not localize
        end;
      end;
    end;

    if temp <> '' then begin
      Strings.Add('ORDER BY ' + temp); // do not localize
    end;
  end;


var
  temp: String;
  i, j: integer;
  TempStringList: TStringList;
  PrefixOfNextWCF: string;
  PrevTable: String;
  tempJoin: TBoldSQLJoin;
  cond: string;
  UnprocessedJoins: TBoldObjectArray;
begin
  tempStringList := TStringList.Create;
  for i := 0 to fColumnsToRetrieve.Count - 1 do
    TempStringlist.Add(ColumnToRetrieve[i].PrefixedColumnName);
  if fGroupOperation <> '' then
    TempStringList[0] := fGroupOperation + '(' + TempStringList[0] + ')';
  if fRetrieveCountStar then begin
    if fDistinct and (TableReferences.Count > 0) then begin
      // Use ID column of last added table als distinct column.
      TempStringList.Add('COUNT(DISTINCT ' +
          TableReferences[TableReferences.Count - 1].GetColumnReference(
              IDCOLUMN_NAME).PrefixedColumnName + ')'); // do not localize
    end else begin
      TempStringList.Add('COUNT(*)'); // do not localize
    end;
  end;
  temp := 'SELECT '; // do not localize
  if fDistinct and (not fRetrieveCountStar) then begin
    temp := temp + 'DISTINCT '; // do not localize
  end;
  if fLimit > 0 then begin
    temp := temp + 'TOP ' + IntToStr(fLimit) + ' '; // do not localize
  end;
  Strings.Add(temp + BoldSeparateStringList(tempStringList, ', ', '', ''));

  PrefixOfNextWCF := 'WHERE'; // do not localize

  if SQLDatabaseConfig.UseSQL92Joins and (TableReferences.Count > 0) then
  begin
    // ensure that the maintable reference is first.
    for i := 0 to TableReferences.Count - 1 do
      if (TableReferences[i] = MainTable) then
        TableReferences.Move(i, 0);

    UnprocessedJoins := TBoldObjectArray.Create(0, []);
    for i := 0 to fJoins.Count - 1 do
      UnprocessedJoins.Add(fJoins[i]);

    temp := 'FROM ' + TableReferences[0].TableAliasDeclaration; // do not localize
    PrevTable := TableReferences[0].AliasName;
    for i := 1 to TableReferences.Count - 1 do
    begin
      cond := '';
      for j := UnprocessedJoins.count - 1 downto 0 do
      begin
        tempJoin := UnprocessedJoins[j] as TBoldSQLJoin;
        if ((TempJoin.fColumnRef1.fTableReference.AliasName = TableReferences[i].AliasName) and
            (TempJoin.fColumnRef2.fTableReference.AliasName = PrevTable)) or
           ((TempJoin.fColumnRef2.fTableReference.AliasName = TableReferences[i].AliasName) and
            (TempJoin.fColumnRef1.fTableReference.AliasName = PrevTable)) then
        begin
          UnprocessedJoins.Delete(j);
          if cond <> '' then
            cond := cond + ' AND '; // do not localize

          cond := cond + TempJoin.JoinStatement;
        end;
      end;
      if cond = '' then
      begin
        temp := temp + ', ' + TableReferences[i].TableAliasDeclaration;
        PrevTable := TableReferences[i].AliasName;
      end
      else
        temp := temp + format(' JOIN %s ON (%s)', [TableReferences[i].TableAliasDeclaration, cond]); // do not localize
    end;
    Strings.Add(temp);
    for i := 0 to UnprocessedJoins.Count - 1 do
    begin
      Strings.Add(PrefixOfNextWCF + ' ' + (UnprocessedJoins[i] as TBoldSQLJoin).JoinStatement);
      PrefixOfNextWCF := ' AND'; // do not localize
    end;
    UnprocessedJoins.Free;
  end else
  begin
    TempStringList.Clear;
    for i := 0 to TableReferences.Count - 1 do
      TempStringList.Add(TableReferences[i].TableAliasDeclaration);
    Strings.Add('FROM ' + BoldSeparateStringList(TempStringList, ', ', '', '')); // do not localize

    for i := 0 to fJoins.Count - 1 do
    begin
      Strings.Add(PrefixOfNextWCF + ' ' + Join[i].JoinStatement);
      PrefixOfNextWCF := ' AND'; // do not localize
    end;
  end;

  for i := 0 to fWhereClauseFragments.Count - 1 do
  begin
    Strings.Add(PrefixOfNextWCF + ' ' + TBoldSqlWCF(fWhereClauseFragments[i]).GetAsString(self));
    PrefixOfNextWCF := '  AND'; // do not localize
  end;

  AddOrderByClause(fLimitTop);

  // The following must be done last!
  // Special case, when multiple last records are to be selected:
  // Through the reversed sort the result set has the wanted records,
  // but in reverse order. (Theoretcal problem, because Limit is 1 on ->first/last)
  // Therefore original order must be restored:
  if (not fLimitTop) and (fLimit > 1) then begin
    Strings.Insert(0, 'SELECT * FROM (');
    // SubSelect must be provided with Alias,
    // otherwise its not possible to resort the outer select.
    Strings.Add(') AS ReverseOrderSelect');
    AddOrderByClause(True);
  end;

  TempStringList.Free;
end;

procedure TBoldSQLQuery.AddColumnToRetrieve(ColumnReference: TBoldSQLColumnReference);
begin
  if fColumnsToRetrieve.IndexOf(ColumnReference) = -1 then
    fColumnsToRetrieve.Add(ColumnReference);
  if fColumnsToRetrieve.Count = 1 then
    fMainTable := ColumnReference.TableReference
  else
    if fMaintable <> ColumnReference.TableReference then
      fMainTable := nil;
end;

function TBoldSQLQuery.GetColumnToRetrieve(Index: Integer): TBoldSQLColumnReference;
begin
  result := TBoldSQLColumnReference(fColumnsToRetrieve[index]);
end;

function TBoldSQLQuery.GetUniqueAlias(const TableName: String): String;
var
  i, Counter: Integer;
  OK: Boolean;
begin
  if assigned(fNameSpace) then
    result := fNameSpace.GetUniqueAlias(TableName)
  else
  begin
    Counter := 0;
    repeat
      OK:= true;
      Inc(Counter);
      result := copy(TableName, 1, 10) + '_' + IntToStr(Counter);
      for i := 0 to TableReferences.Count - 1 do
        OK := OK and (TableReferences[i].fAliasName <> Result);
    until OK;
  end;
end;

function TBoldSQLQuery.GetJoin(index: integer): TBoldSQLJoin;
begin
  result := TBoldSQLJoin(fJoins[index]);
end;

function TBoldSQLQuery.AddJoin(ColumnRef1, ColumnRef2: TBoldSQLColumnReference): TBoldSQLJoin;
var
  i: integer;
  JoinExists: Boolean;
begin
  JoinExists := false;
  Result := nil;
  for i := 0 to fJoins.Count - 1 do
  begin
    if Join[i].Matches(ColumnRef1, ColumnRef2) then
    begin
      JoinExists := true;
      result := Join[i];
    end
  end;

  if not JoinExists then
  begin
    result := TBoldSQLJoin.Create(ColumnRef1, ColumnRef2);
    fJoins.Add(result);
  end;
end;

procedure TBoldSQLQuery.EnsureTableExists(tableName, Operation: String);
begin
  if not assigned(SystemDescription.SQLTablesList.ItemsBySQLName[TableName]) then
    raise EBoldInternal.createFmt('%s: Table %s does not exist', [Operation, TableName]); // do not localize
end;

function TBoldSQLQuery.GetWCF(index: integer): TBoldSqlWCF;
begin
  result := TBoldSqlWCF(fWhereClauseFragments[index]);
end;

function TBoldSQLQuery.AddParam(const name: string=''): TParam;
begin
  result := fParams.Add as tParam;
  if Name = '' then
    result.Name := 'Param' + IntToStr(fNameSpace.GetUnusedParamNumber) // do not localize
  else
    result.Name := Name;
end;

procedure TBoldSQLQuery.AddWCF(WCF: TBoldSqlWCF);
begin
  if assigned(WCF) then
    fWhereClauseFragments.Add(WCF);
end;

procedure TBoldSQLQuery.ClearColumnsToRetrieve;
begin
  fColumnsToRetrieve.Clear;
  fMainTable := nil;
end;

function TBoldSQLQuery.GetAsString: string;
var
  strings: TStringList;
begin
  Strings := TStringlist.create;
  GenerateSelect(Strings);
  result := Strings.Text;
  Strings.Free;
end;

function TBoldSQLQuery.HastableReferenceInList(
  TableReference: TBoldSQLTablereference): boolean;
begin
  result := fTableReferences.IndexOf(TableReference) <> -1;
end;

procedure TBoldSQLQuery.SetLimit(const Top: Boolean = True; const Limit:
    Integer = 1);
begin
  fLimit := Limit;
  fLimitTop := Top;
end;

{ TBoldSQLTableReference }

function TBoldSQLTableReference.GetColumnReference(const ColumnName: String): TBoldSQLColumnReference;
var
  i: integer;
begin
  EnsureColumnExists(ColumnName, 'AddColumnReference'); // do not localize
  for i := 0 to fColumnReferences.Count - 1 do
    if CompareText(TBoldSQLColumnReference(fColumnReferences[i]).ColumnDescription.SQLName, ColumnName) = 0 then
    begin
      result := TBoldSQLColumnReference(fColumnReferences[i]);
      exit;
    end;
  Result := TBoldSQLColumnReference.Create(self, TableDescription.ColumnsList.ItemsBySQLName[ColumnName] as TBoldSQLColumnDescription);
  fColumnReferences.Add(result);
end;

constructor TBoldSQLTableReference.Create(Query: TBoldSQLQuery);
begin
  inherited create;
  fColumnReferences := TBoldObjectArray.Create(0, [bcoDataOwner]);
  fQuery := Query;
end;

destructor TBoldSQLTableReference.Destroy;
begin
  FreeAndNil(fColumnReferences);
  inherited;
end;

procedure TBoldSQLTableReference.EnsureColumnExists(const ColumnName,
  Operation: String);
begin
  if not assigned(TableDescription.ColumnsList.ItemsBySQLName[ColumnName]) then
    raise EBoldInternal.createFmt('%s: Table %s does not contain column %s', [Operation, TableDescription.SQLName, ColumnName]); // do not localize
end;

function TBoldSQLTableReference.GetAliasName: String;
begin
  if fAliasName = '' then
    fAliasName := fQuery.GetUniqueAlias(TableDescription.SQLName);
  result := fAliasName;
end;

function TBoldSQLTableReference.GetTableAliasDeclaration: String;
begin
  result :=  format('%s %s', [TableDescription.SQLName, AliasName]) // do not localize
end;

{ TBoldSQLColumnReference }

constructor TBoldSQLColumnReference.create(TableReference: TBoldSQLTableReference; ColumnDescription: TBoldSQLColumnDescription);
begin
  inherited create;
  fTableReference := TableReference;
  fColumnDescription := ColumnDescription;
end;

function TBoldSQLColumnReference.GetPrefixedColumnName: String;
begin
  result := format('%s.%s', [TableReference.AliasName, ColumnDescription.SQLName]) // do not localize
end;

{ TBoldSQLOrderByInfo }

constructor TBoldSQLOrderByInfo.Create(Column: TBoldSQLColumnReference;
  const Descending: Boolean);
begin
  FColumn := Column;
  FDescending := Descending;
end;

{ TBoldSQLJoin }

constructor TBoldSQLJoin.Create(ColumnRef1, ColumnRef2: TBoldSqlColumnReference);
begin
  inherited Create;
  fColumnRef1 := ColumnRef1;
  fColumnRef2 := ColumnRef2;
end;

function TBoldSQLJoin.getJoinStatement: String;
begin
  result := format('%s.%s = %s.%s', [ // do not localize
    fColumnRef1.TableReference.AliasName, fColumnRef1.ColumnDescription.SQLName,
    fColumnRef2.TableReference.AliasName, fColumnRef2.ColumnDescription.SQLName
    ]);
end;

function TBoldSQLJoin.Matches(ColumnRef1, ColumnRef2: TBoldSqlColumnReference): Boolean;
begin
  result := OrderedMatches(ColumnRef1, ColumnRef2) or
    OrderedMatches(ColumnRef2, ColumnRef1)
end;

function TBoldSQLJoin.OrderedMatches(ColumnRef1, ColumnRef2: TBoldSqlColumnReference): Boolean;
begin
  result := (ColumnRef1.TableReference = fColumnRef1.TableReference) and
    (ColumnRef2.TableReference = fColumnRef2.TableReference) and
    (ColumnRef1.ColumnDescription = fColumnRef1.ColumnDescription) and
    (ColumnRef2.ColumnDescription = fColumnRef2.ColumnDescription);
end;

{ TBoldSqlTableReferenceList }

function TBoldSqlTableReferenceList.Add(
  Item: TBoldSqlTableReference): Integer;
begin
  result := inherited add(item);
end;

function TBoldSqlTableReferenceList.Get(
  Index: Integer): TBoldSqlTableReference;
begin
  result := TBoldSqlTableReference(inherited Items[index])
end;

procedure TBoldSqlTableReferenceList.Insert(Index: Integer;
  Item: TBoldSqlTableReference);
begin
  inherited Insert(index, item);
end;

procedure TBoldSqlTableReferenceList.Put(Index: Integer;
  const Value: TBoldSqlTableReference);
begin
  inherited Items[index] := value;
end;

{ TBoldSQLWCFBinary }

constructor TBoldSQLWCFBinary.Create(arg1, arg2: TBoldSqlWCF; const Symbol: String);
begin
  inherited Create;
  fArg1 := Arg1;
  fArg2 := Arg2;
  fSymbol := Symbol;
end;

destructor TBoldSQLWCFBinary.destroy;
begin
  FreeAndNil(fArg1);
  FreeAndNil(fArg2);
  inherited;
end;

{ TBoldSQLWCFBinaryInfix }

function TBoldSQLWCFBinaryInfix.GetAsString(Query: TBoldSQlQuery): String;
begin
  Result := '(';
  if Assigned(fArg1) then begin
    Result := Result + fArg1.GetAsString(Query) + ' ';
  end;
  if Assigned(fArg1) and Assigned(fArg2) then begin
    Result := Result + fSymbol;
  end;
  if Assigned(fArg2) then begin
    Result := Result + ' ' + fArg2.GetAsString(Query);
  end;
  // Add the escape character to use % and _ ( and [ ) within a search.
  if SameStr(fSymbol, 'LIKE') then begin
    Result := Result + ' ESCAPE ''\''';
  end;
  Result := Result + ')';
end;

class function TBoldSQLWCFBinaryInfix.CreateWCFForIdList(
  ColumnRef: TBoldSQLColumnReference;
  IdList: TBoldObjectIdList): TBoldSQLWCFBinaryInfix;
var
  IdWCF: TBoldSQLWCF;
  IdListWCF: TBoldSQLWCF;
begin
  if IdList.Count = 1 then
  begin
    IdWCF := TBoldSQLWCFColumnRef.Create(ColumnRef);
    IdListWCF := TBoldSQLWCFGenericExpression.Create(IdList.CommaSeparatedIdList);
    result := TBoldSQLWCFBinaryInfix.Create(IdWCF, IdLIstWCF, '=');
  end
  else if IdList.Count > 1 then
  begin
    IdWCF := TBoldSQLWCFColumnRef.Create(ColumnRef);
    IdListWCF := TBoldSQLWCFGenericExpression.Create('(' + IdList.CommaSeparatedIdList + ')');
    result := TBoldSQLWCFBinaryInfix.Create(IdWCF, IdLIstWCF, 'in'); // do not localize
  end
  else
    result := nil;
end;


{ TBoldSQLWCFBinaryPrefix }

function TBoldSQLWCFBinaryPrefix.GetAsString(Query: TBoldSQlQuery): String;
begin
  result := '(' + fSymbol + '(' + fArg1.GetAsString(Query) + ', ' + fArg2.GetAsString(Query) + '))';
end;

{ TBoldSQLWCFXOR }

constructor TBoldSQLWCFXOR.Create(arg1, arg2: TBoldSqlWCF);
begin
  inherited Create;
  fArg1 := Arg1;
  fArg2 := Arg2;
end;

function TBoldSQLWCFXOR.GetAsString(Query: TBoldSQlQuery): String;
begin
  Result := '(';
  if Assigned(fArg1) and Assigned(fArg2) then begin
    Result := Result + Format('(%s AND NOT %s) or (%1:s AND NOT %0:s)',
                              [fArg1.GetAsString(Query),
                               fArg2.GetAsString(Query)]);
  end else begin
    Result := Result + 'FALSE'; // This case does not exist
  end;
  Result := Result + ')';
end;

{ TBoldSQLWCFString }

constructor TBoldSQLWCFString.Create(const Value: String);
begin
  fStr := Value;
end;

function TBoldSQLWCFString.GetAsString(Query: TBoldSQlQuery): String;
var
  temp: string;
  i: integer;
begin
  temp := fStr;
  // Strings in AnsiSQL is quoted with single
  // quotes, and internal quotes has to be quoted (' -> '').
  for i := length(temp) downto 1 do
    if temp[i] = '''' then
      insert('''', temp, i);
  result := Query.SQLDatabaseConfig.UnicodeStringPrefix + '''' + temp + '''';
end;

{ TBoldSQLWCFInteger }

constructor TBoldSQLWCFInteger.Create(Value: integer);
begin
  fInt := Value;
end;

function TBoldSQLWCFInteger.GetAsString(Query: TBoldSQlQuery): String;
begin
  result := IntToStr(fInt);
end;

{ TBoldSQLWCFFloat }

constructor TBoldSQLWCFFloat.Create(Value: Double);
begin
  fFloat := Value;
end;

function TBoldSQLWCFFloat.GetAsString(Query: TBoldSQlQuery): String;
begin
  if not Assigned(fParam) then          //<- Avoid multiple create of Param, and set ptype
    fParam := Query.AddParam;
  fParam.ParamType:=ptInput;
  fParam.AsFloat := fFloat;
  result := ':' + fParam.Name;
end;

{ TBoldSQLWCFColumnRef }

constructor TBoldSQLWCFColumnRef.Create(ColumnRef: TBoldSqlColumnReference);
begin
  fColumnRef := ColumnRef;
end;

function TBoldSQLWCFColumnRef.GetAsString(Query: TBoldSQlQuery): String;
begin
  result := fColumnRef.PrefixedColumnName;
end;

{ TBoldSQLWCFExists }

constructor TBoldSQLWCFExists.create(query: TBoldSQLQuery;
  MainTable: TBoldSqlTableReference);
begin
  inherited Create(Query);
  Query.ClearColumnsToRetrieve;
  Query.AddColumnToRetrieve(MainTable.GetColumnReference(IDCOLUMN_NAME));
end;

function TBoldSQLWCFExists.GetAsString(Query: TBoldSQlQuery): String;
begin
  result := 'EXISTS (' + QueryAsString + ')'; // do not localize
  CopyParams(Query); //<- Copy parameters from exists-statement (HK)
end;

{ TBoldSQLWCFWithQuery }

procedure TBoldSQLWCFWithQuery.CopyParams(Query: TBoldSQlQuery); //<- Copy parameters (HK)
var
  i:integer;
  param,topQueryParam:TParam;
begin
  for i:=0 to self.Query.Params.Count - 1 do
  begin
    param:=self.Query.Params[i];
    topQueryParam:=Query.Params.FindParam(param.Name);
    if not Assigned(topQueryParam) then
      topQueryParam:=Query.AddParam(param.Name);

    topQueryParam.Assign(param);
  end;
end;

constructor TBoldSQLWCFWithQuery.create(query: TBoldSQLQuery);
begin
  inherited Create;
  fQuery := Query;
end;

destructor TBoldSQLWCFWithQuery.destroy;
begin
  FreeAndNil(fQuery);
  inherited;
end;

function TBoldSQLWCFWithQuery.GetAsString(Query: TBoldSQlQuery): String;
begin
  result := '(' + QueryAsString + ')';
  CopyParams(Query); //<- Copy parameters (HK)
end;

function TBoldSQLWCFWithQuery.QueryAsString: String;
var
  Strings: TStrings;
begin
  Strings := TStringList.Create;
  Query.GenerateSQL(Strings);
  result := Strings.Text;
  Strings.Free;
end;

{ TBoldSQLWCFInQuery }

constructor TBoldSQLWCFInQuery.create(Arg1: TBoldSqlWCF;
  Query: TBoldSQLQuery; MainTable: TBoldSqlTableReference);
begin
  inherited Create(Query);
  fArg1 := Arg1;
  Query.ClearColumnsToRetrieve;
  Query.AddColumnToRetrieve(MainTable.GetColumnReference(IDCOLUMN_NAME));
end;

destructor TBoldSQLWCFInQuery.destroy;
begin
  // a workaround, the In-condition does not really own its query
  // -> No, not a workaroung, but a memory leak!
  // -> TBoldSQLWCFInQuery is used only in 2 places, and there the query is not freed.
//  fQuery := nil;

  freeAndNil(fArg1);
  inherited;
  freeAndNil(fArg1);
end;

function TBoldSQLWCFInQuery.GetAsString(Query: TBoldSQlQuery): String;
begin
  result := '(' + Arg1.GetAsString(Query) + ' IN (' + QueryAsString + '))'; // do not localize
  CopyParams(Query); //<- Copy parameters (HK)
end;

{ TBoldSQLWCFGenericExpression }

constructor TBoldSQLWCFGenericExpression.Create(const Expr: String);
begin
  fExpr := Expr;
end;

function TBoldSQLWCFGenericExpression.GetAsString(Query: TBoldSQlQuery): String;
begin
  result := fExpr;
end;

{ TBoldSQLWCFJoin }

constructor TBoldSQLWCFJoin.Create(ColumnRef1,
  ColumnRef2: TBoldSqlColumnReference);
begin
  inherited Create;
  fColumnref1 := ColumnRef1;
  fColumnref2 := ColumnRef2;
end;

function TBoldSQLWCFJoin.GetAsString(Query: TBoldSQlQuery): String;
begin
  result := fColumnref1.PrefixedColumnName + '=' + fColumnref2.PrefixedColumnName;
end;

{ TBoldSQLWCFUnary }

constructor TBoldSQLWCFUnary.Create(arg1: TBoldSqlWCF;
  Symbol: String);
begin
  inherited Create;
  fArg1 := Arg1;
  fSymbol := Symbol;
end;

destructor TBoldSQLWCFUnary.destroy;
begin
  FreeAndNil(fArg1);
  inherited;
end;

function TBoldSQLWCFUnaryPrefix.GetAsString(Query: TBoldSQlQuery): String;
begin
  result := '(' + fSymbol + '(' + fArg1.GetAsString(Query) + '))';
end;

{ TBoldSQLWCFSize }
procedure TBoldSQLWCFSize.PreGetAsString;
begin
  fQuery.RetrieveCountStar;
end;

function TBoldSQLWCFGroupFunction.GetAsString(Query: TBoldSQlQuery): String;
begin
  PreGetAsString;
  result := '(' + QueryAsString + ')';
  CopyParams(Query); //<- Copy parameters (HK)
end;

procedure TBoldSQLQuery.RetrieveCountStar;
begin
  fRetrieveCountStar := true;
end;

{ TBoldSQLWCFUnaryPostfix }

function TBoldSQLWCFUnaryPostfix.GetAsString(Query: TBoldSQlQuery): String;
begin
  result := '((' + fArg1.GetAsString(Query) + ') ' + fSymbol + ')';
end;

{ TBoldSQLWCFGroupFunction }

constructor TBoldSQLWCFGroupFunction.create(query: TBoldSQLQuery);
begin
  inherited;
  Query.ClearColumnsToRetrieve;
end;

procedure TBoldSQLWCFGroupFunction.PreGetAsString;
begin
  // nothing
end;

{ TBoldSQLWCFGroupFunctionWithColumn }

constructor TBoldSQLWCFGroupFunctionWithColumn.create(query: TBoldSQLQuery;
  ColumnRef: TBoldSqlColumnReference; Operation: string);
begin
  inherited Create(Query);
  Query.AddColumnToRetrieve(ColumnRef);
  Query.fGroupOperation := Operation;
end;

{ TBoldSQLNameSpace }

constructor TBoldSQLNameSpace.create;
begin
  inherited;
  fUsedNames := TStringList.Create;
  fUsedParams := 0;
end;

destructor TBoldSQLNameSpace.destroy;
begin
  FreeAndNil(fUsedNames);
  inherited;
end;

function TBoldSQLNameSpace.GetUniqueAlias(const TableName: String): String;
var
  i: integer;
begin
  i := 1;
  repeat
    result := copy(TableName, 1, 10) + '_' + IntToStr(i);
    inc(i);
  until fUsedNames.IndexOf(result) = -1;
  fUsedNames.Add(result);
end;

procedure TBoldSQLQuery.AddColumnToOrderBy(Columnreference:
    TBoldSQlColumnReference; const Descending: Boolean);
begin
  fColumnsToOrderBy.Add(TBoldSQLOrderByInfo.Create(ColumnReference, Descending));
end;

function TBoldSQLNameSpace.GetUnusedParamNumber: integer;
begin
  result := fUsedParams;
  Inc(fUsedParams);
end;

{ TBoldSQLWCFDate }

constructor TBoldSQLWCFDate.Create(Value: TDateTime);
begin
  fDate := Value;
end;

function TBoldSQLWCFDate.GetAsString(Query: TBoldSQlQuery): String;
begin
  if not Assigned(fParam) then          //<- Avoid multiple create of Param, and set ptype
    fParam := Query.AddParam;
  fParam.ParamType:=ptInput;
  fParam.AsDate := fDate;
  result := ':' + fParam.Name;
end;

{ TBoldSQLWCFTime }

constructor TBoldSQLWCFTime.Create(Value: TDateTime);
begin
  fTime := Value;
end;

function TBoldSQLWCFTime.GetAsString(Query: TBoldSQlQuery): String;
begin
  if not Assigned(fParam) then          //<- Avoid multiple create of Param, and set ptype
    fParam := Query.AddParam;
  fParam.ParamType:=ptInput;
  fParam.AsTime := fTime;
  result := ':' + fParam.Name;
end;



{ TBoldSQLWCFUnaryTransforLikeString }

function TBoldSQLWCFUnaryTransformLikeString.GetAsString(
  Query: TBoldSQlQuery): String;
var
  SB: TStringBuilder;
  Ch: Char;
begin
  Result := fArg1.GetAsString(Query);
  if true or Query.SQLDatabaseConfig.QuoteLeftBracketInLike then
  begin
    Sb := TStringBuilder.Create(Length(Result)+10);
    try
      for Ch in Result do
      if Ch = '[' then
        SB.Append('[[]')
      else
        SB.Append(Ch);
      Result := SB.ToString;
    finally
      sb.free;
    end;
  end;
end;

end.
