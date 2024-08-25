
{ Global compiler directives }
{$include bold.inc}
unit BoldSqlSymbols;

interface

uses
  BoldIndexableList,
  BoldSqlNodes,
  System.Classes,
  Data.DB;

type
  TBSS_Symbol = class(TBoldSqlSymbol)
  protected
    function GetName: String; override;
  end;

  {---TBoldSymbolDictionary---}
  TBoldSqlSymbolDictionary = class(TBoldIndexableList)
  private
    function GetSymbol(const Name: string): TBSS_Symbol;
    function GetSymbolByIndex(index: Integer): TBSS_Symbol;
    class var IX_SymbolName: integer;
  public
    constructor Create();
    property SymbolByName[const name: string]: TBSS_Symbol read GetSymbol;
    property Symbols[i: Integer]: TBSS_Symbol read GetSymbolByIndex; default;
  end;

function SqlSymbolDictionary: TBoldSqlSymbolDictionary;

implementation

uses
  SysUtils,

  BoldPMappersSql,
  BoldSQLQuery,
  BoldPMappersDefault,
  BoldDefs,
  BoldContainers,
  BoldPSDescriptionsSQL,
  BoldPMappersLinkDefault,
  BoldHashIndexes,
  BoldIndex,
  BoldCoreConsts;

var
  SqlSymbols: TBoldSqlSymbolDictionary;

type
  {---TSymbolNameIndex---}
  TSymbolNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

function SqlSymbolDictionary: TBoldSqlSymbolDictionary;
begin
  result := SqlSymbols;
end;
  
  {---TSymbolNameIndex---}
function TSymbolNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TBSS_Symbol(Item).Name;
end;

{ TBSS_Symbol }

function TBSS_Symbol.GetName: String;
begin
  result := copy(ClassName, 6, maxint);
end;

{ TBoldSqlSymbolDictionary }

constructor TBoldSqlSymbolDictionary.Create();
begin
  inherited create;
  SetIndexCapacity(1);
  SetIndexVariable(IX_SymbolName, AddIndex(TSymbolNameIndex.Create));
end;

function TBoldSqlSymbolDictionary.GetSymbol(const Name: string): TBSS_Symbol;
begin
  Result := TBSS_Symbol(TBoldStringHashIndex(indexes[IX_SymbolName]).FindByString(Name));
end;

function TBoldSqlSymbolDictionary.GetSymbolByIndex(index: Integer): TBSS_Symbol;
begin
  Result := TBSS_Symbol(Items[index]);
end;

type
  TBSS_BinarySymbol = class(TBSS_Symbol)
  protected
    procedure ConvertQueryToWCF(SourceNode, DestNode: TBoldSqlNode);
    procedure CollectArgWCFs(OpNode: TBoldSQLOperation);
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_BinaryBooleanSymbol = class(TBSS_BinarySymbol)
  public
  end;

  TBSS_Equal = class(TBSS_BinaryBooleanSymbol)
  protected
    function GetName: String; override;
  end;

  TBSS_NotEqual = class(TBSS_BinaryBooleanSymbol)
  protected
    function GetName: String; override;
  end;

  TBSS_Less = class(TBSS_BinaryBooleanSymbol)
  protected
    function GetName: String; override;
  end;

  TBSS_Greater = class(TBSS_BinaryBooleanSymbol)
  protected
    function GetName: String; override;
  end;

  TBSS_LessEQ = class(TBSS_BinaryBooleanSymbol)
  protected
    function GetName: String; override;
  end;

  TBSS_GreaterEQ = class(TBSS_BinaryBooleanSymbol)
  protected
    function GetName: String; override;
  end;

  TBSS_SQLLike = class(TBSS_BinarySymbol)
  protected
    function GetSQLName: String; override;
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_SQLLikeCaseInsensitive = class(TBSS_SQLLike)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_Add = class(TBSS_BinarySymbol)
  protected
    function GetName: String; override;
  end;

  TBSS_Subtract = class(TBSS_BinarySymbol)
  protected
    function GetName: String; override;
  end;

  TBSS_Multiply = class(TBSS_BinarySymbol)
  protected
    function GetName: String; override;
  end;

  TBSS_Divide = class(TBSS_BinarySymbol)
  protected
    function GetName: String; override;
  end;

  TBSS_or = class(TBSS_BinarySymbol)
  end;

  TBSS_and = class(TBSS_BinarySymbol)
  end;

  TBSS_xor = class(TBSS_BinarySymbol)
  end;

  TBSS_implies = class(TBSS_BinarySymbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_UnarySymbol = class(TBSS_Symbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_not = class(TBSS_UnarySymbol)
  end;

  TBSS_UnaryMinus = class(TBSS_UnarySymbol)
  protected
    function GetName: String; override;
    function GetSQLName: String; override;
  end;

  TBSS_isNull = class(TBSS_Symbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_Abs = class(TBSS_UnarySymbol)
  end;

  TBSS_Floor = class(TBSS_UnarySymbol)
  end;

  TBSS_Round = class(TBSS_UnarySymbol)
  end;

  TBSS_ToUpper = class(TBSS_UnarySymbol)
  protected
    function GetSQLName: String; override;
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_toLower = class(TBSS_UnarySymbol)
  protected
    function GetSQLName: String; override;
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_Size = class(TBSS_Symbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_isEmpty = class(TBSS_Symbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_NotEmpty = class(TBSS_Symbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_GroupFunctions = class(TBSS_Symbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_Sum = class(TBSS_GroupFunctions)
  end;

  TBSS_Maxvalue = class(TBSS_GroupFunctions)
  protected
    function GetSQLName: String; override;
  end;

  TBSS_MinValue = class(TBSS_GroupFunctions)
  protected
    function GetSQLName: String; override;
  end;

  TBSS_Average = class(TBSS_GroupFunctions)
  protected
    function GetSQLName: String; override;
  end;

  TBSS_Iteration = class(TBSS_Symbol)
  public
    function ResolveObjectMapper(OperationNode: TBoldSqlOperation): TBoldObjectSqlMapper; override;
  end;

  TBSS_Select = class(TBSS_Iteration)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_reject = class(TBSS_Iteration)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_Exists = class(TBSS_Iteration)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_ForAll = class(TBSS_Iteration)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_orderby = class(TBSS_Iteration)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_orderDescending = class(TBSS_Iteration)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_AllInstances = class(TBSS_Symbol)
  public
    function ResolveObjectMapper(OperationNode: TBoldSqlOperation): TBoldObjectSqlMapper; override;
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_PrefixBinarySymbol = class(TBSS_Symbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_Div = class(TBSS_PrefixBinarySymbol)
  end;
  TBSS_Mod = class(TBSS_PrefixBinarySymbol)
  end;

  TBSS_includes = class(TBSS_Symbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_Length = class(TBSS_UnarySymbol)
  protected
    function GetSQLName: String; override;
  end;

  TBSS_ListOperations = class(TBSS_Symbol)
  protected
    function CreateBoldIdMatchWCF(MainNode: TBoldSQLOperation; ArgNode: TBoldSQLNode): TBoldSQLWCF;
  public
    function ResolveObjectMapper(OperationNode: TBoldSqlOperation): TBoldObjectSqlMapper; override;
  end;

  TBSS_union = class(TBSS_ListOperations)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_Intersection = class(TBSS_ListOperations)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_SymmetricDifference = class(TBSS_ListOperations)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_Difference = class(TBSS_ListOperations)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_OclIsTypeOf = class(TBSS_BinarySymbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_OclIsKindOf = class(TBSS_BinarySymbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_OclAsType = class(TBSS_Symbol)
  public
    function ResolveObjectMapper(OperationNode: TBoldSqlOperation): TBoldObjectSqlMapper; override;
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_SafeCast = class(TBSS_OclAsType);

  TBSS_FilterOnType = class(TBSS_Symbol)
  public
    function ResolveObjectMapper(OperationNode: TBoldSqlOperation): TBoldObjectSqlMapper; override;
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  TBSS_AsSet = class(TBSS_Symbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace:
        TBoldSqlNameSpace); override;
    function ResolveObjectMapper(OperationNode: TBoldSqlOperation):
        TBoldObjectSqlMapper; override;
  end;

  TBSS_First = class(TBSS_Symbol)
  protected
    function IsTopLimit: Boolean; virtual;
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace:
        TBoldSqlNameSpace); override;
  end;

  TBSS_Last = class(TBSS_First)
  protected
    function IsTopLimit: Boolean; override;
  end;

  TBSS_BoldId = class(TBSS_Symbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

type
  TBSS_BoldIDIs = class(TBSS_BinarySymbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

type
  TBSS_BoldIDIn = class(TBSS_BinarySymbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

type
  TBSS_NamedIDIn = class(TBSS_BinarySymbol)
  public
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); override;
  end;

  {
  These operations are available in OCL, but have not yet been implemented in ocl2sql

  TBSS_Min = class(TBSS_Symbol)
  end;
  TBSS_Max = class(TBSS_Symbol)
  end;
  TBSS_concat = class(TBSS_Symbol)
  end;
  TBSS_SubString = class(TBSS_Symbol)
  end;
  TBSS_Pad = class(TBSS_Symbol)
  end;
  TBSS_PostPad = class(TBSS_Symbol)
  end;
  TBSS_FormatNumeric = class(TBSS_Symbol)
  end;
  TBSS_FormatDateTime = class(TBSS_Symbol)
  end;
  TBSS_StrToDate = class(TBSS_Symbol)
  end;
  TBSS_StrToTime = class(TBSS_Symbol)
  end;
  TBSS_StrToDateTime = class(TBSS_Symbol)
  end;
  TBSS_implies = class(TBSS_Symbol)
  end;
  TBSS_if = class(TBSS_Symbol)
  end;
  TBSS_Count = class(TBSS_Symbol)
  end;
  TBSS_IncludesAll = class(TBSS_Symbol)
  end;
  TBSS_difference = class(TBSS_Symbol)
  end;
  TBSS_Including = class(TBSS_Symbol)
  end;
  TBSS_excluding = class(TBSS_Symbol)
  end;
  TBSS_SymmetricDifference = class(TBSS_Symbol)
  end;
  TBSS_collect = class(TBSS_Symbol)
  end;
  TBSS_AsSequence = class(TBSS_Symbol)
  end;
  TBOS_AsBag = class(TBSS_Symbol)
  end;
  TBSS_AsSet = class(TBSS_Symbol)
  end;
  TBSS_Append = class(TBSS_Symbol)
  end;
  TBSS_Prepend = class(TBSS_Symbol)
  end;
  TBSS_SubSequence = class(TBSS_Symbol)
  end;
  TBSS_at = class(TBSS_Symbol)
  end;
  TBSS_asString = class(TBSS_Symbol)
  end;
  TBSS_TypeName = class(TBSS_Symbol)
  end;
  TBSS_Attributes = class(TBSS_Symbol)
  end;
  TBSS_AssociationEnds = class(TBSS_Symbol)
  end;
  TBSS_SuperTypes = class(TBSS_Symbol)
  end;
  TBSS_AllSuperTypes = class(TBSS_Symbol)
  end;
  TBSS_AllSubClasses = class(TBSS_Symbol)
  end;
  TBSS_AllLoadedObjects = class(TBSS_Symbol)
  end;
  TBSS_oclType = class(TBSS_Symbol)
  end;
  TBSS_RegExpMatch = class(TBSS_Symbol)
  end;
  TBSS_InDateRange = class(TBSS_Symbol)
  end;
  TBSS_InTimeRange = class(TBSS_Symbol)
  end;
  TBSS_Constraints = class(TBSS_Symbol)
  end;
  TBSS_AtTime = class(TBSS_Symbol)
  end;
  TBSS_AllInstancesAtTime = class(TBSS_Symbol)
  end;
  TBSS_Existing = class(TBSS_Symbol)
  end;
}

{ TBSS_Select }

procedure TBSS_Select.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  Sel: TBoldSQLIteration;
begin
  Sel := OperationNode as TBoldSQLIteration;
  Sel.Query := Sel.LoopVar.RelinquishQuery;
  Sel.Query.AddWCF(OperationNode.Args[1].RelinquishWCF);
end;

function TBSS_Iteration.ResolveObjectMapper(OperationNode: TBoldSqlOperation): TBoldObjectSqlMapper;
begin
  result := OperationNode.Args[0].ObjectMapper;
end;

{ TBSS_AllInstances }

procedure TBSS_AllInstances.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
begin
  OperationNode.NewQuery(NameSpace);
end;

function TBSS_AllInstances.ResolveObjectMapper(OperationNode: TBoldSqlOperation): TBoldObjectSqlMapper;
begin
  result := OperationNode.Args[0].ObjectMapper;
end;

{ TBSS_Equal }

function TBSS_Equal.GetName: String;
begin
  result := '=';
end;

{ TBSS_NotEqual }

function TBSS_NotEqual.GetName: String;
begin
  result := '<>';
end;

{ TBSS_Add }

function TBSS_Add.GetName: String;
begin
  result := '+';
end;

{ TBSS_Subtract }

function TBSS_Subtract.GetName: String;
begin
  result := '-';
end;

{ TBSS_Multiply }

function TBSS_Multiply.GetName: String;
begin
  result := '*';
end;

{ TBSS_Divide }

function TBSS_Divide.GetName: String;
begin
  result := '/';
end;

{ TBSS_Less }

function TBSS_Less.GetName: String;
begin
  result := '<';
end;

{ TBSS_Greater }

function TBSS_Greater.GetName: String;
begin
  result := '>';
end;

{ TBSS_LessEQ }

function TBSS_LessEQ.GetName: String;
begin
  result := '<=';
end;

{ TBSS_GreaterEQ }

function TBSS_GreaterEQ.GetName: String;
begin
  result := '>=';
end;

{ TBSS_BinarySymbol }

procedure TBSS_BinarySymbol.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
begin
  OperationNode.WCF := TBoldSQLWCFBinaryInfix.Create(
    OperationNode.Args[0].RelinquishWCF,
    OperationNode.Args[1].RelinquishWCF, SQLName);

  CollectArgWCFs(OperationNode);
end;

procedure TBSS_BinarySymbol.CollectArgWCFs(OpNode: TBoldSQLOperation);
begin
  if OpNode.Args[1].HasQuery then
    ConvertQueryToWCF(OpNode.Args[1], OpNode);
  if OpNode.Args[0].HasQuery then
    ConvertQueryToWCF(OpNode.Args[0], OpNode);
end;

procedure TBSS_BinarySymbol.ConvertQueryToWCF(SourceNode, DestNode: TBoldSqlNode);
var
  Query: TBoldSQLQuery;
begin
  Query := SourceNode.RelinquishQuery;
  Query.AddWCF(DestNode.RelinquishWCF);

  if assigned(Query.MainTable) then
    DestNode.WCF := TBoldSQLWCFExists.Create(Query, Query.MainTable)
  else
    DestNode.WCF := TBoldSQLWCFExists.Create(Query, SourceNode.MainTableRef(Query));
end;

{ TBSS_UnaryMinus }

function TBSS_UnaryMinus.GetName: String;
begin
  result := 'unary-';
end;

function TBSS_UnaryMinus.GetSQLName: String;
begin
  result := '-';
end;

{ TBSS_Length }

function TBSS_Length.GetSQLName: String;
begin
  result := 'LEN'; // do not localize
end;

{ TBSS_UnarySymbol }

procedure TBSS_UnarySymbol.BuildWCFOrQuery(
  OperationNode: TBoldSQlOperation; NameSpace: TBoldSqlNameSpace);
var
  WCF: TBoldSQLWCF;
begin
  WCF := TBoldSQLWCFUnaryPrefix.Create(
      OperationNode.Args[0].RelinquishWCF, SQLName);

  if OperationNode.Args[0].HasQuery then
    OperationNode.Query := OperationNode.Args[0].RelinquishQuery;

  OperationNode.WCF := WCF;
end;

{ TBSS_Size }

procedure TBSS_Size.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
begin
  OperationNode.WCF := TBoldSQLWCFSize.Create(OperationNode.Args[0].RelinquishQuery)
end;

{ TBSS_SQLLike }

procedure TBSS_SQLLike.BuildWCFOrQuery(OperationNode: TBoldSQLOperation;
  NameSpace: TBoldSqlNameSpace);
begin
 OperationNode.WCF := TBoldSQLWCFBinaryInfix.Create(
    OperationNode.Args[0].RelinquishWCF,
    TBoldSQLWCFUnaryTransformLikeString.Create(OperationNode.Args[1].RelinquishWCF, ''), SQLname);

  CollectArgWCFs(OperationNode);
end;

function TBSS_SQLLike.GetSQLName: String;
begin
  result := 'LIKE';
end;

{ TBSS_SQLLikeCaseInsensitive }

procedure TBSS_SQLLikeCaseInsensitive.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
begin
  OperationNode.WCF := TBoldSQLWCFBinaryInfix.Create(
    TBoldSQLWCFUnaryPrefix.Create(OperationNode.Args[0].RelinquishWCF, 'UPPER'), // do not localize
    TBoldSQLWCFUnaryPrefix.Create(OperationNode.Args[1].RelinquishWCF, 'UPPER'), SQLname); // do not localize

  CollectArgWCFs(OperationNode);
end;

{ TBSS_implies }

procedure TBSS_implies.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
begin
  OperationNode.WCF := TBoldSQLWCFBinaryInfix.Create(
    TBoldSQLWCFUnaryPrefix.Create(OperationNode.Args[0].RelinquishWCF, 'NOT'), // do not localize
    OperationNode.Args[1].RelinquishWCF, 'OR'); // do not localize
end;

{ TBSS_PrefixSymbol }

procedure TBSS_PrefixBinarySymbol.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
begin
  OperationNode.WCF := TBoldSQLWCFBinaryPrefix.Create(
    OperationNode.Args[0].RelinquishWCF,
    OperationNode.Args[1].RelinquishWCF, SQLName);
end;

{ TBSS_ToUpper }

procedure TBSS_ToUpper.BuildWCFOrQuery(OperationNode: TBoldSQLOperation;
  NameSpace: TBoldSqlNameSpace);
begin
  OperationNode.WCF := TBoldSQLWCFUnaryPrefix.Create(
    OperationNode.Args[0].RelinquishWCF, SQLName);
end;

function TBSS_ToUpper.GetSQLName: String;
begin
  result := 'UPPER';
end;

{ TBSS_toLower }

procedure TBSS_toLower.BuildWCFOrQuery(OperationNode: TBoldSQLOperation;
  NameSpace: TBoldSqlNameSpace);
begin
  OperationNode.WCF := TBoldSQLWCFUnaryPrefix.Create(
    OperationNode.Args[0].RelinquishWCF, SQLName);
end;

function TBSS_toLower.GetSQLName: String;
begin
  result := 'LOWER';
end;

{ TBSS_reject }

procedure TBSS_reject.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  Sel: TBoldSQLIteration;
begin
  Sel := OperationNode as TBoldSQLIteration;
  Sel.Query := Sel.LoopVar.RelinquishQuery;
  Sel.Query.AddWCF(TBoldSQLWCFUnaryPrefix.Create(OperationNode.Args[1].RelinquishWCF, 'NOT'));
end;

{ TBSS_isNull }

procedure TBSS_isNull.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  WCF: TBoldSQLWCF;
  Query: TBoldSQLQuery;
begin
  WCF := TBoldSQLWCFUnaryPostfix.Create(OperationNode.Args[0].RelinquishWCF, 'IS NULL');

  if OperationNode.args[0].HasQuery then
  begin
    OperationNode.Args[0].Query.AddWCF(wcf);
    Query := OperationNode.Args[0].RelinquishQuery;
    WCF := TBoldSQLWCFExists.Create(Query, Query.MainTable);
  end;

  OperationNode.WCF := WCF;
end;

{ TBSS_GroupFunctions }

procedure TBSS_GroupFunctions.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  WCF: TBoldSQLWCF;
begin
  if OperationNode.args[0].HasQuery then
  begin
    WCF := OperationNode.args[0].RelinquishWCF;
    if WCF is TBoldSQLWCFColumnRef then begin
      OperationNode.WCF := TBoldSQLWCFGroupFunctionWithColumn.Create(
          OperationNode.Args[0].RelinquishQuery,
          (WCF as TBoldSQLWCFColumnRef).Columnref, SQLName);
    end
    else
      raise EBoldInternal.CreateFmt('Argument to %s has has no ColumnRef WCF', [Name])

  end
  else
    raise EBoldInternal.CreateFmt('Argument to %s has no Query', [Name])
end;

{ TBSS_includes }

procedure TBSS_includes.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  WCF: TBoldSQLWCF;
  Q1IdColumnRef: TBoldSqlColumnReference;
  Query: TBoldSqlQuery;
  VarReference: TBoldSQlVariableReference;
begin
  if not OperationNode.args[0].HasQuery then
    raise EBoldInternal.Create('Arg 0 for Includes has no Query');

  Q1IdColumnRef := OperationNode.Args[0].MainTableRef.GetColumnReference(IDCOLUMN_NAME);

  if OperationNode.Args[1] is TBoldSQLVariableReference then
  begin
    VarReference := OperationNode.Args[1] as TBoldSQLVariableReference;
    if VarReference.IsExternalVariable then
      WCF := TBoldSQLWCFBinaryInfix.CreateWCFForIdList(Q1IDColumnRef, VarReference.VariableBinding.Context)
    else
      WCF := TBoldSQLWCFBinaryInfix.create(
        TBoldSQLWCFColumnRef.Create(Q1IDColumnRef),
        TBoldSQLWCFColumnRef.Create(OperationNode.Args[1].MainTableRef.GetColumnReference(IDCOLUMN_NAME)) ,'=')
  end
  else
  begin
    Query := OperationNode.Args[1].RelinquishQuery;
    WCF := TBoldSQLWCFInQuery.Create(TBoldSQLWCFColumnRef.Create(Q1IdColumnRef), Query, OperationNode.Args[1].MainTableRef(Query));
  end;

  OperationNode.Args[0].Query.AddWCF(WCF);
  Query := OperationNode.Args[0].RelinquishQuery;
  OperationNode.WCF := TBoldSQLWCFExists.Create(Query, OperationNode.Args[0].MainTableRef(Query));
end;

{ TBSS_Average }

function TBSS_Average.GetSQLName: String;
begin
  result := 'AVG';
end;

{ TBSS_MinValue }

function TBSS_MinValue.GetSQLName: String;
begin
  result := 'MIN';
end;

{ TBSS_Maxvalue }

function TBSS_Maxvalue.GetSQLName: String;
begin
  result := 'MAX';
end;

{ TBSS_Exists }

procedure TBSS_Exists.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  Query: TBoldSqlQuery;
begin
  Query := (OperationNode as tBoldSqlIteration).LoopVar.RelinquishQuery;
  Query.AddWCF(OperationNode.Args[1].RelinquishWCF);
  OperationNode.WCF := TBoldSQLWCFExists.Create(Query, OperationNode.Args[0].MaintableRef(Query));
end;

{ TBSS_ForAll }

procedure TBSS_ForAll.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  Query: TBoldSqlQuery;
  TempWCF: TBoldSqlWCF;
begin
  Query := (OperationNode as TBoldSqlIteration).LoopVar.RelinquishQuery;
  tempWcf := TBoldSQLWCFUnaryPrefix.Create(OperationNode.Args[1].RelinquishWCF, 'NOT');
  Query.AddWCF(TempWcf);
  TempWCF := TBoldSQLWCFExists.Create(Query, OperationNode.Args[0].MaintableRef(Query));
  OperationNode.WCF := TBoldSQLWCFUnaryPrefix.Create(TempWCF, 'NOT')
end;

{ TBSS_orderby }

procedure TBSS_orderby.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  Query: TBoldSqlQuery;
  OrderByMember: TBoldSqlMember;
  OrderByColumnName: String;
  tableDescription: TBoldSQLTableDescription;
  TableRef: TBoldSQlTableReference;
  ColRef: TBoldSqlColumnReference;
  Loopvar: TBoldSqlVariableBinding;
begin
  if not (OperationNode.args[1] is TBoldSqlMember) then
    raise EBold.Create(sArgToOrderByMustBeMember);

  OrderByMember := OperationNode.args[1] as TBoldSqlMember;

  if OrderByMember.MemberMapper.ColumnCount <> 1 then
    raise EBold.Create(sArgToOrderByMustHaveExactlyOneColumn);

  Loopvar := (OperationNode as TBoldSQlIteration).Loopvar;

  Query := Loopvar.RelinquishQuery;

  TableDescription := (OrderByMember.MemberMapper.ColumnDescriptions[0].Owner as TBoldSQLTableDescription);
  TableRef := OperationNode.TableReferenceForTable(TableDescription, Query, false);

  OrderByColumnName := OrderByMember.MemberMapper.ColumnDescriptions[0].SQLName;
  ColRef := TableRef.GetColumnReference(OrderByColumnName);

  Query.AddColumnToOrderBy(ColRef, False);

  OperationNode.WCF := Loopvar.RelinquishWCF;

  OperationNode.Query := Query;
end;

{ TBSS_orderDescending }

procedure TBSS_orderDescending.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  Query: TBoldSqlQuery;
  OrderByMember: TBoldSqlMember;
  OrderByColumnName: String;
  tableDescription: TBoldSQLTableDescription;
  TableRef: TBoldSQlTableReference;
  ColRef: TBoldSqlColumnReference;
  Loopvar: TBoldSqlVariableBinding;
begin
  if not (OperationNode.args[1] is TBoldSqlMember) then
    raise EBold.Create(sArgToOrderByMustBeMember);

  OrderByMember := OperationNode.args[1] as TBoldSqlMember;

  if OrderByMember.MemberMapper.ColumnCount <> 1 then
    raise EBold.Create(sArgToOrderByMustHaveExactlyOneColumn);

  Loopvar := (OperationNode as TBoldSQlIteration).Loopvar;

  Query := Loopvar.RelinquishQuery;

  TableDescription := (OrderByMember.MemberMapper.ColumnDescriptions[0].Owner as TBoldSQLTableDescription);
  TableRef := OperationNode.TableReferenceForTable(TableDescription, Query, false);

  OrderByColumnName := OrderByMember.MemberMapper.ColumnDescriptions[0].SQLName;
  ColRef := TableRef.GetColumnReference(OrderByColumnName);

  Query.AddColumnToOrderBy(ColRef, True);

  OperationNode.WCF := Loopvar.RelinquishWCF;

  OperationNode.Query := Query;
end;

{ TBSS_isEmpty }

procedure TBSS_isEmpty.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  Query: TBoldSqlQuery;
  TempWCF: TBoldSQLWCF;
//  x.IsEmpty -> not exists x
begin
  Query := OperationNode.Args[0].RelinquishQuery;
  TempWCF := TBoldSQLWCFExists.Create(Query, Query.MainTable);
  OperationNode.WCF := TBoldSQLWCFUnaryPrefix.Create(tempWCF, 'NOT'); // do not localize
end;

{ TBSS_NotEmpty }

procedure TBSS_NotEmpty.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  Query: TBoldSqlQuery;
//  x.NotEmpty -> exists x
begin
  Query := OperationNode.Args[0].RelinquishQuery;
  OperationNode.WCF := TBoldSQLWCFExists.Create(Query, Query.MainTable);
end;

{ TBSS_union }

procedure TBSS_union.BuildWCFOrQuery(OperationNode: TBoldSQLOperation;
  NameSpace: TBoldSqlNameSpace);
// a->union(b) >> (id in a) or (id in b)
begin
  OperationNode.NewQuery(nameSpace);
  OperationNode.Query.AddWCF(
    TBoldSQLWCFBinaryInfix.Create(
      CreateBoldIdMatchWCF(OperationNode, OperationNode.args[0]),
      CreateBoldIdMatchWCF(OperationNode, OperationNode.args[1]),
      'OR')); // do not localize
end;

{ TBSS_Intersection }

procedure TBSS_Intersection.BuildWCFOrQuery(
  OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
// a->Intersection(b) >> (id in a) and (id in b)
begin
  OperationNode.NewQuery(nameSpace);
  OperationNode.Query.AddWCF(
    TBoldSQLWCFBinaryInfix.Create(
      CreateBoldIdMatchWCF(OperationNode, OperationNode.args[0]),
      CreateBoldIdMatchWCF(OperationNode, OperationNode.args[1]),
      'AND')); // do not localize
end;

{ TBSS_SymmetricDifference }

procedure TBSS_SymmetricDifference.BuildWCFOrQuery(
  OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
// a->symmetricDifference(b) >> ((id in a) xor (id in b))
// SQL does not support XOR, so we have to use inline version:
// a->symmetricDifference(b) >> (id in a) and (not (id in b)) or
//                              (id in b) and (not (id in a))
begin
  OperationNode.NewQuery(nameSpace);
//  OperationNode.Query.AddWCF(
//    TBoldSQLWCFBinaryInfix.Create(
//      CreateBoldIdMatchWCF(OperationNode, OperationNode.args[0]),
//      CreateBoldIdMatchWCF(OperationNode, OperationNode.args[1]),
//      'XOR')); // do not localize

{ Doesnt work, because queries of the args are relinquished for the first part
  OperationNode.Query.AddWCF(
    TBoldSQLWCFBinaryInfix.Create(
      TBoldSQLWCFBinaryInfix.Create(
        CreateBoldIdMatchWCF(OperationNode, OperationNode.args[0]),
        TBoldSQLWCFUnaryPrefix.Create(
          CreateBoldIdMatchWCF(OperationNode, OperationNode.args[1]),
          'NOT'), // do not localize
        'AND'), // do not localize
      TBoldSQLWCFBinaryInfix.Create(
        CreateBoldIdMatchWCF(OperationNode, OperationNode.args[1]),
        TBoldSQLWCFUnaryPrefix.Create(
          CreateBoldIdMatchWCF(OperationNode, OperationNode.args[0]),
          'NOT'), // do not localize
        'AND'), // do not localize
      'OR')); // do not localize
}
  // Solution: custom WCF for XOR
  OperationNode.Query.AddWCF(
    TBoldSQLWCFXOR.Create(
      CreateBoldIdMatchWCF(OperationNode, OperationNode.args[0]),
      CreateBoldIdMatchWCF(OperationNode, OperationNode.args[1])));
end;

{ TBSS_Difference }

procedure TBSS_Difference.BuildWCFOrQuery(OperationNode: TBoldSQLOperation;
  NameSpace: TBoldSqlNameSpace);
// a->difference(b) >> (id in a) and (not (id in b))
begin
  OperationNode.NewQuery(nameSpace);
  OperationNode.Query.AddWCF(
    TBoldSQLWCFBinaryInfix.Create(
      CreateBoldIdMatchWCF(OperationNode, OperationNode.args[0]),
      TBoldSQLWCFUnaryPrefix.Create(
        CreateBoldIdMatchWCF(OperationNode, OperationNode.args[1]),
        'NOT'), // do not localize
      'AND')); // do not localize
end;

{ TBSS_ListOperations }

function TBSS_ListOperations.CreateBoldIdMatchWCF(MainNode: TBoldSQLOperation; ArgNode: TBoldSQLNode): TBoldSQLWCF;
var
  Query: TBoldSQLQuery;
  MainColumn: TBoldSQLColumnReference;
  VarRef: TBoldSQLVariableReference;
begin
  MainColumn := MainNode.Query.MainTable.GetColumnReference(IDCOLUMN_NAME);

  if Argnode is TBoldSQLVariableReference then
  begin
    VarRef := TBoldSQLVariableReference(ArgNode);
    if VarRef.IsExternalVariable then
      result := TBoldSQLWCFBinaryInfix.CreateWCFForIdList(MainColumn, VarRef.VariableBinding.Context)
    else
      // BoldId = VariabelReference.BoldId
      result := TBoldSQLWCFBinaryInfix.create(
        TBoldSQLWCFColumnRef.Create(MainColumn),
        TBoldSQLWCFColumnRef.Create(ArgNode.MainTableRef.GetColumnReference(IDCOLUMN_NAME)) ,'=');
  end
  else
  begin
    // BoldID in (Arg1.Query)
    Query := ArgNode.RelinquishQuery;
    Result := TBoldSQLWCFInQuery.Create(
      TBoldSQLWCFColumnRef.Create(MainColumn),
      Query,
      ArgNode.MainTableRef(Query));
  end;
end;

function TBSS_ListOperations.ResolveObjectMapper(
  OperationNode: TBoldSqlOperation): TBoldObjectSqlMapper;
var
  Objectmapper1, ObjectMapper2: TBoldObjectSQlMapper;
begin
  ObjectMapper1 := OperationNode.args[0].ObjectMapper;
  ObjectMapper2 := OperationNode.args[1].ObjectMapper;
  while Objectmapper1 <> ObjectMapper2 do
  begin
    if ObjectMapper1.TopSortedIndex > ObjectMapper2.TopSortedIndex then
      ObjectMapper1 := Objectmapper1.SuperClass as tBoldObjectSQLMapper
    else
      ObjectMapper2 := Objectmapper2.SuperClass as tBoldObjectSQLMapper;
  end;
  result := ObjectMapper1;
end;

{ TBSS_OclIsTypeOf }

procedure TBSS_OclIsTypeOf.BuildWCFOrQuery(
  OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  TypeColRef: TBoldSQLWCFColumnRef;
  TypeValue: TBoldSQLWCFInteger;
begin
  TypeColRef := TBoldSQLWCFColumnRef.Create(OperationNode.Args[0].MainTableRef.GetColumnReference(TYPECOLUMN_NAME));
  TypeValue := TBoldSQLWCFInteger.Create(OperationNode.Args[1].ObjectMapper.BoldDbType);
  OperationNode.WCF := TBoldSQLWCFBinaryInfix.Create(TypeColRef, TypeValue, '=');
  CollectArgWCFs(OperationNode);
{  if OPerationNode.Args[0].HasQuery then
    OperationNode.Query := OperationNode.Args[0].RelinquishQuery;
  if assigned(OperationNode.Args[0].WCF) then
    OperationNode.WCF := TBoldSQLWCFBinaryInfix.Create(OperationNode.WCF, OperationNode.Args[0].WCF, 'AND');
    }
end;

{ TBSS_OclIsKindOf }

procedure TBSS_OclIsKindOf.BuildWCFOrQuery(
  OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  TypeColRef: TBoldSQLWCFColumnRef;
  TypeValue: TBoldSQLWCFGenericExpression;
  ObjectMapper: TBoldObjectDefaultMapper;
begin
  ObjectMapper := OperationNode.Args[1].ObjectMapper as TBoldObjectDefaultMapper;
  TypeColRef := TBoldSQLWCFColumnRef.Create(OperationNode.Args[0].MainTableRef.GetColumnReference(TYPECOLUMN_NAME));
  TypeValue := TBoldSQLWCFGenericExpression.Create('(' + ObjectMapper.SubClassesID + ')');
  OperationNode.WCF := TBoldSQLWCFBinaryInfix.Create(TypeColRef, TypeValue, 'in'); // do not localize
  CollectArgWCFs(OperationNode);
end;

{ TBSS_OclAsType }

procedure TBSS_OclAsType.BuildWCFOrQuery(OperationNode: TBoldSQLOperation;
  NameSpace: TBoldSqlNameSpace);
begin
  if (OperationNode.Args[0] is TBoldSQLVariableReference) and
    (OperationNode.Args[0] as TBoldSQLVariableReference).VariableBinding.IsLoopVar then
  begin
    OperationNode.NewQuery(NameSpace);
    OperationNode.EnsureRetrievalOfMainColumns;
    OperationNode.Query.AddJoin(
      OperationNode.MainTableRef.GetColumnReference(IDCOLUMN_NAME),
      OperationNode.Args[0].MainTableRef.GetColumnReference(IDCOLUMN_NAME));
  end
  else
  begin
    OperationNode.Query := OperationNode.Args[0].RelinquishQuery;
    OperationNode.CopyTableReferences(OperationNode.Args[0]);
  end;
end;

function TBSS_OclAsType.ResolveObjectMapper(
  OperationNode: TBoldSqlOperation): TBoldObjectSqlMapper;
begin
  result := OperationNode.Args[1].ObjectMapper;
end;

{ TBSS_FilterOnType }

procedure TBSS_FilterOnType.BuildWCFOrQuery(
  OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  TypeColRef: TBoldSQLWCFColumnRef;
  TypeValue: TBoldSQLWCFGenericExpression;
  ObjectMapper: TBoldObjectDefaultMapper;
  WCF: TBoldSQLWCF;
begin
  ObjectMapper := OperationNode.Args[1].ObjectMapper as TBoldObjectDefaultMapper;
  TypeColRef := TBoldSQLWCFColumnRef.Create(OperationNode.Args[0].MainTableRef.GetColumnReference(TYPECOLUMN_NAME));
  TypeValue := TBoldSQLWCFGenericExpression.Create('('+ObjectMapper.SubClassesID+')');
  WCF := TBoldSQLWCFBinaryInfix.Create(TypeColRef, TypeValue, 'IN');

  if OperationNode.Args[0].HasQuery then
    OperationNode.Query := OperationNode.Args[0].RelinquishQuery;

  if OperationNode.HasQuery then
    OPerationNode.Query.AddWCF(WCF)
  else
  begin
    if assigned(OperationNode.Args[0].WCF) then
      WCF := TBoldSQLWCFBinaryInfix.Create(WCF, OperationNode.Args[0].RelinquishWCF, 'AND');
    OperationNode.WCF := WCF;
  end;

  OperationNode.CopyTableReferences(OPerationNode.Args[0]);
end;

function TBSS_FilterOnType.ResolveObjectMapper(
  OperationNode: TBoldSqlOperation): TBoldObjectSqlMapper;
begin
  result := OperationNode.Args[1].ObjectMapper;
end;

{ TBSS_AsSet }

procedure TBSS_AsSet.BuildWCFOrQuery(OperationNode: TBoldSQLOperation;
    NameSpace: TBoldSqlNameSpace);
var
  Query: TBoldSqlQuery;
begin
  Query := OperationNode.Args[0].RelinquishQuery;

  // Distinct is usually not necessary because inPS evaluation returns no
  // duplicate objects. But if asSet is used in a (Sub-)Select, this is indeed needed.
  Query.Distinct := True;

  // Further simply pass Result without doing anything more
  OperationNode.WCF := operationNode.Args[0].RelinquishWCF;
  OperationNode.Query := Query;
end;

function TBSS_AsSet.ResolveObjectMapper(OperationNode: TBoldSqlOperation):
    TBoldObjectSqlMapper;
begin
  result := OperationNode.Args[0].ObjectMapper;
end;

{ TBSS_First }

procedure TBSS_First.BuildWCFOrQuery(OperationNode: TBoldSQLOperation;
    NameSpace: TBoldSqlNameSpace);
var
  Query: TBoldSqlQuery;
begin
  Query := OperationNode.Args[0].RelinquishQuery;

  // Limit in Query einstellen
  Query.SetLimit(IsTopLimit);

  // Ansonsten simple Weitergabe des Ergebnisses ohne etwas zu machen
  OperationNode.WCF := operationNode.Args[0].RelinquishWCF;
  OperationNode.Query := Query;
end;

function TBSS_First.IsTopLimit: Boolean;
begin
  Result := True;
end;

{ TBSS_Last }

function TBSS_Last.IsTopLimit: Boolean;
begin
  Result := False;
end;

{ TBSS_BoldIDIs }

procedure TBSS_BoldIDIs.BuildWCFOrQuery(
  OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  TypeColRef: TBoldSQLWCFColumnRef;
begin
  TypeColRef := TBoldSQLWCFColumnRef.Create(OperationNode.Args[0].MainTableRef.GetColumnReference(IDCOLUMN_NAME));
  OperationNode.WCF := TBoldSQLWCFBinaryInfix.Create(TypeColRef, OperationNode.Args[1].RelinquishWCF, '=');
  CollectArgWCFs(OperationNode);
end;

{ TBSS_BoldIDIn }

procedure TBSS_BoldIDIn.BuildWCFOrQuery(
  OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  TypeColRef: TBoldSQLWCFColumnRef;
  TypeValue: TBoldSQLWCFGenericExpression;
  aWCF: TBoldSqlWCF;
  sIDs: string;
begin
  TypeColRef := TBoldSQLWCFColumnRef.Create(OperationNode.Args[0].MainTableRef.GetColumnReference(IDCOLUMN_NAME));
  aWCF := OperationNode.Args[1].RelinquishWCF;
  sIDs := aWCF.GetAsString(OperationNode.Args[0].Query);
  aWCF.Free;
  // Übergebenen String in ID-Liste umwandeln -> "" entfernen
  // Außer es ist Paramter (:Param)
  if (Length(sIDs) > 0) then begin
    if (sIDs[1] = '''') then begin
      sIDs := Copy(sIDs, 2, Length(sIDs) - 2);
    end else if (sIDs[1] = ':') then begin
      sIDs := OperationNode.Args[0].Query.Params.ParamValues[Copy(sIDs, 2, MaxInt)];
    end;
  end;
  TypeValue := TBoldSQLWCFGenericExpression.Create('(' + sIDs + ')');
  OperationNode.WCF := TBoldSQLWCFBinaryInfix.Create(TypeColRef, TypeValue, 'in'); // do not localize
  CollectArgWCFs(OperationNode);
end;

{ TBSS_BoldId }

procedure TBSS_BoldId.BuildWCFOrQuery(OperationNode: TBoldSQLOperation;
  NameSpace: TBoldSqlNameSpace);
var
  SQLNode: TBoldSQLNode;
  Query: TBoldSqlQuery;
  SqlMember: TBoldSqlMember;
  TableRefs: TBoldSQLTableReferenceList;
  ColumnName: string;
begin
  Query := nil;
  SQLNode := OperationNode.Args[0];
  if SQLNode is TBoldSqlVariableReference then
  begin
    Query := TBoldSqlVariableReference(SQLNode).Query;
    ColumnName := IDCOLUMN_NAME;
  end
  else
  if (SQLNode is TBoldSqlMember) then
  begin
    SqlMember := TBoldSqlMember(SQLNode);
    if SqlMember.MemberMapper.IsStoredInObject and
      (SqlMember.memberOf is TBoldSqlVariableReference) then
    begin
      ColumnName := SqlMember.MemberMapper.ColumnDescriptions[0].sqlName;
      Query := TBoldSqlVariableReference(SqlMember.memberOf).VariableBinding.Query;
    end;
  end;
  if not Assigned(Query) then
    raise EBoldInternal.Create(className + ': No Query found.');
  TableRefs := Query.TableReferences;
  OperationNode.WCF := TBoldSQLWCFColumnRef.Create(TableRefs[TableRefs.Count-1].GetColumnReference(ColumnName));
end;

{ TBSS_NamedIDIn }

procedure TBSS_NamedIDIn.BuildWCFOrQuery(
  OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
var
  TypeColRef: TBoldSQLWCFColumnRef;
  TypeValue: TBoldSQLWCFGenericExpression;
  aWCF: TBoldSqlWCF;
  sIDs: string;
  aColRef: TBoldSQLColumnReference;
  slHelper: TStringList;
  i: Integer;
  sUnicodePrefix: string;
  oSQLNode: TBoldSQLNode;
begin
  oSQLNode := OperationNode.Args[0];
  aColRef := oSQLNode.MainTableRef.GetColumnReference(TBoldSqlStrLiteral(OperationNode.Args[1]).asString);
  TypeColRef := TBoldSQLWCFColumnRef.Create(aColRef);

  if oSQLNode.HasQuery then
  begin
    sUnicodePrefix := oSQLNode.Query.SQLDataBaseConfig.UnicodeStringPrefix;
  end else
  begin
    sUnicodePrefix := 'N'; // fallback
  end;

  aWCF := OperationNode.Args[2].RelinquishWCF;
  try
    sIDs := aWCF.GetAsString(OperationNode.Args[0].Query);
  finally
    aWCF.Free;
  end;

  // convert passed string to ID list -> remove "".
  // Unless it is a paramter (:Param)
  if (Length(sIDs) > 0) then begin

    // remove leading Unicodeprefix, if existing, to add this later for every single item
    if sIds[1] = sUnicodePrefix then
    begin
      sIDs := Copy(sIDs, 2, Length(sIDs));
    end;

    if (sIDs[1] = '''') then begin
      sIDs := Copy(sIDs, 2, Length(sIDs) - 2);
    end else if (sIDs[1] = ':') then begin
      sIDs := OperationNode.Args[0].Query.Params.ParamValues[Copy(sIDs, 2, MaxInt)];
    end;
  end;


  if sIDs <> '' then begin
    // in case we have a "string" column, every id needs to set in own singlequotes
    if (aColRef.ColumnDescription.FieldType in [ftString, ftMemo, ftFixedChar, ftWideString, ftWideMemo]) then begin
      slHelper := TStringList.Create;
      try
        slHelper.Delimiter := ',';
        slHelper.DelimitedText := sIDs;
        for i := 0 to slHelper.Count - 1 do begin

          // also adding leading unicode prefix for every single item...
          if i = 0 then begin
            sIDs := sUnicodePrefix + '''' + Trim(slHelper[i]) + '''';
          end else begin
            sIDs := sIDs + ', ' + sUnicodePrefix + '''' + Trim(slHelper[i]) + '''';
          end;
        end;
      finally
        slHelper.Free;
      end;
    end;
  end;

  TypeValue := TBoldSQLWCFGenericExpression.Create('(' + sIDs + ')');
  OperationNode.WCF := TBoldSQLWCFBinaryInfix.Create(TypeColRef, TypeValue, 'in'); // do not localize
  CollectArgWCFs(OperationNode);
end;

initialization
  TBoldSqlSymbolDictionary.IX_SymbolName := -1;
  sqlSymbols := TBoldSqlSymbolDictionary.Create;
  sqlSymbols.Add(TBSS_Add.Create);
  sqlSymbols.Add(TBSS_Equal.Create);
  sqlSymbols.Add(TBSS_NotEqual.Create);
  sqlSymbols.Add(TBSS_Add.Create);
  sqlSymbols.Add(TBSS_Subtract.Create);
  sqlSymbols.Add(TBSS_Multiply.Create);
  sqlSymbols.Add(TBSS_Divide.Create);
  sqlSymbols.Add(TBSS_Less.Create);
  sqlSymbols.Add(TBSS_Greater.Create);
  sqlSymbols.Add(TBSS_LessEQ.Create);
  sqlSymbols.Add(TBSS_GreaterEQ.Create);
  sqlSymbols.Add(TBSS_Div.Create);
  sqlSymbols.Add(TBSS_Mod.Create);
  sqlSymbols.Add(TBSS_or.Create);
  sqlSymbols.Add(TBSS_and.Create);
  sqlSymbols.Add(TBSS_xor.Create);
  sqlSymbols.Add(TBSS_implies.Create);
  sqlSymbols.Add(TBSS_not.Create);
  sqlSymbols.Add(TBSS_UnaryMinus.Create);
  sqlSymbols.Add(TBSS_isNull.Create);
  sqlSymbols.Add(TBSS_Abs.Create);
  sqlSymbols.Add(TBSS_Floor.Create);
  sqlSymbols.Add(TBSS_Round.Create);
  sqlSymbols.Add(TBSS_Size.Create);
  sqlSymbols.Add(TBSS_Select.Create);
  sqlSymbols.Add(TBSS_Reject.Create);
  sqlSymbols.Add(TBSS_orderby.Create);
  sqlSymbols.Add(TBSS_orderDescending.Create);
  sqlSymbols.Add(TBSS_AllInstances.Create);
  sqlSymbols.Add(TBSS_SQLLike.Create);
  sqlSymbols.Add(TBSS_SQLLikeCaseInsensitive.Create);
  sqlSymbols.Add(TBSS_includes.Create);
  sqlSymbols.Add(TBSS_Sum.Create);
  sqlSymbols.Add(TBSS_Average.Create);
  sqlSymbols.Add(TBSS_MinValue.Create);
  sqlSymbols.Add(TBSS_MaxValue.Create);
  sqlSymbols.Add(TBSS_Length.Create);
  sqlSymbols.Add(TBSS_Exists.Create);
  sqlSymbols.Add(TBSS_ForAll.Create);
  sqlSymbols.Add(TBSS_NotEmpty.Create);
  sqlSymbols.Add(TBSS_IsEmpty.Create);
  sqlSymbols.Add(TBSS_toUpper.Create);
  sqlSymbols.Add(TBSS_ToLower.Create);
  sqlSymbols.Add(TBSS_Union.Create);
  sqlSymbols.Add(TBSS_Intersection.Create);
  sqlSymbols.Add(TBSS_SymmetricDifference.Create);
  sqlSymbols.Add(TBSS_Difference.Create);
  sqlSymbols.Add(TBSS_oclIsTypeOf.Create);
  sqlSymbols.Add(TBSS_oclIsKindOf.Create);
  sqlSymbols.Add(TBSS_oclAsType.Create);
  sqlSymbols.Add(TBSS_SafeCast.Create);
  sqlSymbols.Add(TBSS_FilterOnType.Create);
  sqlSymbols.Add(TBSS_AsSet.Create);
  sqlSymbols.Add(TBSS_First.Create);
  sqlSymbols.Add(TBSS_Last.Create);
  sqlSymbols.Add(TBSS_BoldId.Create);
  sqlSymbols.Add(TBSS_BoldIDIs.Create);
  sqlSymbols.Add(TBSS_BoldIDIn.Create);
  sqlSymbols.Add(TBSS_NamedIDIn.Create);

finalization
  FreeAndNil(sqlSymbols);

end.
