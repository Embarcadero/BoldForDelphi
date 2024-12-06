{ Global compiler directives }
{$include bold.inc}
unit BoldSqlNodes;

interface

uses
  DB,
  Classes,
  BoldBase,
  BoldPSDescriptionsSQL,
  BoldPMappersSQL,
  BoldSqlQuery,
  BoldId;

type

  TBoldSqlNode = class;
  TBoldSqlTypeNode = class;
  TBoldSqlListCoercion = class;
  TBoldSqlOperation = class;
  TBoldSqlIteration = class;
  TBoldSqlMember = class;
  TBoldSqlVariableBinding = class;
  TBoldSqlVariableReference = class;
  TBoldSqlLiteral = class;
  TBoldSqlStrLiteral = class;
  TBoldSqlDateLiteral = class;
  TBoldSqlTimeLiteral = class;
  TBoldSqlFloatLiteral = class;
  TBoldSqlIntLiteral = class;
  TBoldSqlEnumLiteral = class;

  TBoldSqlNodeList = class;
  TBoldSqlSymbol = class;

  TBoldSqlNodeVisitor = class(TObject)
  protected
    procedure VisitTBoldSqlNode(N: TBoldSqlNode); virtual;
    procedure VisitTBoldSqlListCoercion(N: TBoldSqlListCoercion); virtual;
    procedure VisitTBoldSqlOperation(N: TBoldSqlOperation); virtual;
    procedure VisitTBoldSqlIteration(N: TBoldSqlIteration); virtual;
    procedure VisitTBoldSqlMember(N: TBoldSqlMember); virtual;
    procedure VisitTBoldSqlLiteral(N: TBoldSqlLiteral); virtual;
    procedure VisitTBoldSqlStrLiteral(N: TBoldSqlStrLiteral); virtual;
    procedure VisitTBoldSqlFloatLiteral(N: TBoldSqlFloatLiteral); virtual;
    procedure VisitTBoldSqlEnumLiteral(N: TBoldSqlEnumLiteral); virtual;
    procedure VisitTBoldSqlIntLiteral(N: TBoldSqlIntLiteral); virtual;
    procedure VisitTBoldSqlDateLiteral(N: TBoldSqlDateLiteral); virtual;
    procedure VisitTBoldSqlTimeLiteral(N: TBoldSqlTimeLiteral); virtual;
    procedure VisitTBoldSqlVariableBinding(N: TBoldSqlVariableBinding); virtual;
    procedure VisitTBoldSqlVariableReference(N: TBoldSqlVariableReference); virtual;
    procedure VisitTBoldSqlTypeNode(N: TBoldSqlTypeNode); virtual;
  end;

  TBoldSqlNodeList = class(TList)
  private
    function GetItem(index: Integer): TBoldSqlNode;
    procedure PutItem(index: Integer; Value: TBoldSqlNode);
  public
    destructor Destroy; override;
    function Add(Item: TBoldSqlNode): Integer;
    procedure TraverseList(V: TBoldSqlNodeVisitor); virtual;
    property Items[index: Integer]: TBoldSqlNode read GetItem write PutItem; default;
  end;


  TBoldSqlNode = class(TBoldMemoryManagedObject)
  private
    fPosition: integer;
    fObjectMapper: TBoldObjectSQLMapper;
    fTableReferences: TBoldSqlTableReferenceList;
    fWCF: TBoldSqlWCF;
    fQuery: TBoldSqlQuery;
    function GetHasObjectMapper: Boolean;
    function GetTableReferences: TBoldSqlTableReferenceList; virtual;
  protected
    function GetHasQuery: boolean; virtual;
    function GetQuery: TBoldSqlQuery; virtual;
    function GetObjectMapper: TBoldObjectSQLMapper; virtual;
    procedure SetObjectMapper(const Value: TBoldObjectSQLMapper); virtual;
    procedure SetQuery(const Value: TBoldSqlQuery); virtual;
  public
    constructor Create(Position: integer);
    destructor Destroy; override;
    function TableReferenceForTable(Table: TBoldSqlTableDescription; Query: TBoldSqlQuery; ForceOwntable: Boolean): TBoldSqlTableReference; virtual;
    function RelinquishQuery: TBoldSqlQuery; virtual;
    function RelinquishWCF: TBoldSqlWCF; virtual;
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); virtual;
    procedure NewQuery(NameSpace: tBoldSqlNameSpace);
    procedure EnsureRetrievalOfMainColumns;
    procedure CopyTableReferences(node: TBoldSqlNode);
    property Position: Integer read fPosition;
    property ObjectMapper: TBoldObjectSQLMapper read GetObjectMapper write SetObjectMapper;
    property WCF: TBoldSqlWCF read fWCF write fWCF;
    property Query: TBoldSqlQuery read GetQuery write SetQuery;
    property HasQuery: boolean read GetHasQuery;
    property HasObjectMapper: Boolean read GetHasObjectMapper;
    function MainTableRef(Query: TBoldSQLQuery = nil): TBoldSQLTableReference;
  end;


  TBoldSqlTypeNode = class(TBoldSqlNode)
  private
    fTypeName: String;
    fTopSortedIndex: Integer;
  public
    constructor Create(Position: integer; TypeName: string; TopSortedIndex: integer);
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
    property TypeName: String read fTypeName;
    property TopSortedIndex: Integer read fTopSortedIndex;
  end;

  TBoldSqlListCoercion = class(TBoldSqlNode)
  private
    fChild: TBoldSqlNode;
  public
    constructor Create(Position: integer; Child: TBoldSqlNode);
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
    property Child: TBoldSqlNode read fChild;
  end;

  TBoldSqlOperation = class(TBoldSqlNode)
  private
    fArgs: TBoldSqlNodeList;
    fOperationName: string;
    fSymbol: TBoldSqlSymbol;
  public
    constructor Create(Position: integer; OperationName: String);
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
    property Args: TBoldSqlNodeList read fArgs;
    property OperationName: string read fOperationName;
    property Symbol: TBoldSqlSymbol read fSymbol write fSymbol;
  end;

  TBoldSqlIteration = class(TBoldSqlOperation)
  private
    fLoopVar: TBoldSqlVariableBinding;
  public
    constructor Create(Position: integer; OperationName: String; LoopVar: TBoldSqlVariableBinding);
    destructor Destroy; override;
    function TableReferenceForTable(Table: TBoldSqlTableDescription; Query: TBoldSqlQuery; ForceOwntable: Boolean): TBoldSqlTableReference; override;
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
    property LoopVar: TBoldSqlVariableBinding read fLoopVar;
  end;


  TBoldSqlMember = class(TBoldSqlNode)
  private
    fMemberIndex: Integer;
    fMemberName: string;
    fmemberOf: TBoldSqlNode;
    fQualifier: TBoldSqlNodeList;
    fMemberMapper: TBoldMemberSQLMapper;
    fIsBoolean: Boolean;
  public
    constructor Create(Position: integer; const memberName: String; MemberIndex: Integer; MemberOf: TBoldSqlNode; IsBoolean: Boolean);
    destructor Destroy; override;
    function TableReferenceForTable(Table: TBoldSqlTableDescription; Query: TBoldSqlQuery; ForceOwnTable: Boolean): TBoldSqlTableReference; override;
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
    function QueryOfMemberOfIsEnclosing: Boolean;
    property MemberOf: TBoldSqlNode read fmemberOf;
    property MemberName: string read fMemberName;
    property MemberIndex: Integer read fMemberIndex;
    property Qualifier: TBoldSqlNodeList read fQualifier;
    property MemberMapper: TBoldMemberSQLMapper read fMemberMapper write fMemberMapper;
    property IsBoolean: Boolean read fIsBoolean;
  end;

  TBoldSqlVariableBinding = class(TBoldSqlNode)
  private
    fVariableName: string;
    fIsExternal: Boolean;
    fTopSortedIndex: integer;
    fExternalVarValue: Variant;
    fIsLoopVar: Boolean;
    fRefCount: integer;
    fContext: TBoldObjectIdList;
  public
    constructor Create(Position: integer; VariableName: String; TopSortedIndex: integer);
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
    procedure AddRef;
    procedure DecRef;
    property VariableName: string read fVariableName;
    property TopSortedIndex: integer read fTopSortedIndex;
    property ExternalVarvalue: Variant read fExternalVarValue write fExternalVarValue;
    property IsLoopVar: Boolean read fIsLoopVar write fIsLoopVar;
    property IsExternal: Boolean read fIsExternal write fIsExternal;
    property Context: TBoldObjectIdList read fContext write fContext;
    property RefCount: integer read fRefCount;
  end;

  TBoldSqlVariableReference = class(TBoldSqlNode)
  private
    fVariableBinding: TBoldSqlVariableBinding;
    function GetTableReferences: TBoldSqlTableReferenceList; override;
  protected
    function GetHasQuery: boolean; override;
    function GetQuery: TBoldSqlQuery; override;
    function GetObjectMapper: TBoldObjectSQLMapper; override;
    procedure SetObjectMapper(const Value: TBoldObjectSQLMapper); override;
    procedure SetQuery(const Value: TBoldSqlQuery); override;
  public
    constructor Create(Position: integer; VariableBinding: TBoldSqlVariableBinding);
    function TableReferenceForTable(Table: TBoldSqlTableDescription; Query: TBoldSqlQuery; ForceOwntable: Boolean): TBoldSqlTableReference; override;
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
    function RelinquishQuery: TBoldSqlQuery; override;
    function RelinquishWCF: TBoldSqlWCF; override;
    function IsExternalVariable: Boolean;
    property VariableBinding: TBoldSqlVariableBinding read fVariableBinding;
  end;

  TBoldSqlLiteral = class(TBoldSqlNode)
  protected
    function GetAsString: String; virtual; abstract;
  public
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
    property AsString: String read GetAsString;
  end;

  TBoldSqlStrLiteral = class(TBoldSqlLiteral)
  private
    fStrValue: String;
  protected
    function GetAsString: String; override;
  public
    constructor Create(Position: integer; StrValue: String);
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
    property StrValue: String read fStrValue;
  end;

  TBoldSqlDateLiteral = class(TBoldSqlLiteral)
  private
    fDateValue: TDateTime;
  protected
    function GetAsString: String; override;
  public
    constructor Create(Position: integer; DateValue: TDateTime);
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
    property DateValue: TDateTime read fDateValue;
  end;

  TBoldSqlTimeLiteral = class(TBoldSqlLiteral)
  private
    fTimeValue: TDateTime;
  protected
    function GetAsString: String; override;
  public
    constructor Create(Position: integer; TimeValue: TDateTime);
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
    property TimeValue: TDateTime read fTimeValue;
  end;



  TBoldSqlFloatLiteral = class(TBoldSqlLiteral)
  private
    fFloatValue: Double;
  protected
    function GetAsString: String; override;
  public
    constructor Create(Position: integer; FloatValue: Double);
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
    property FloatValue: Double read fFloatValue;
  end;

  TBoldSqlIntLiteral = class(TBoldSqlLiteral)
  private
    fIntValue: Integer;
  protected
    function GetAsString: String; override;
  public
    constructor Create(Position: integer; IntValue: Integer);
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
    property IntValue: Integer read fIntValue;
  end;

  TBoldSqlEnumLiteral = class(TBoldSqlLiteral)
  private
    fName: string;
    fIntValue: integer;
  protected
    function GetAsString: String; override;
  public
    constructor Create(Position, IntValue: integer; Name: String);
    property Name: string read fName;
    property Intvalue: integer read fIntValue write fIntvalue;
    procedure AcceptVisitor(V: TBoldSqlNodeVisitor); override;
  end;

  TBoldSqlSymbol = class(TBoldMemoryManagedObject)
  protected
    function GetName: String; virtual; abstract;
    function GetSQLName: String; virtual;
  public
    function ResolveObjectMapper(OperationNode: TBoldSqlOperation): TBoldObjectSqlMapper; virtual;
    procedure BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace); virtual;
    property Name: String read GetName;
    property SQLName: String read GetSQLName;
  end;

  TBoldSQLWCFVariable = class(TBoldSqlWCF)
  private
    fParam:TParam;
    fVariableBinding:TBoldSqlVariableBinding;
  public
    constructor Create(VariableBinding: TBoldSqlVariableBinding);
    function GetAsString(Query: TBoldSQlQuery): String; override;
    destructor Destroy;override;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldSQLMappingInfo,
  BoldPMappersDefault;

{ TBoldOCLListCoercion }

procedure TBoldSqlListCoercion.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  inherited;
  v.VisitTBoldSqlListCoercion(self);
end;

{ TBoldOclOperation }

procedure TBoldSqlOperation.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  inherited;
  v.VisitTBoldSqlOperation(self);
end;

constructor TBoldSqlOperation.create(Position: integer; OperationName: String);
begin
  inherited create(position);
  fOperationName := OperationName;
  fArgs := TBoldSqlNodeList.Create;
end;

destructor TBoldSqlOperation.Destroy;
begin
  FreeAndNil(fArgs);
  inherited;          
end;

{ TBoldSqlNode }

procedure TBoldSqlNode.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  v.VisitTBoldSqlNode(self);
end;

constructor TBoldSqlNode.Create(Position: integer);
begin
  inherited create;
  fPosition := Position;
  fTableReferences := TBoldSqlTableReferenceList.Create(0, []);
end;

destructor TBoldSqlNode.destroy;
begin
  FreeAndNil(fTableReferences);
  FreeAndNil(fWCF);
  FreeAndNil(fQuery);
  inherited;
end;

function TBoldSqlNode.GetHasObjectMapper: Boolean;
begin
  result := assigned(fObjectMapper);
end;

function TBoldSqlNode.GetObjectMapper: TBoldObjectSQLMapper;
begin
  assert(assigned(fObjectMapper));
  Result := fObjectMapper;
end;

function TBoldSqlNode.GetQuery: TBoldSqlQuery;
begin
  assert(assigned(fQuery));
  Result := fQuery;
end;

function TBoldSqlNode.MainTableRef(Query: TBoldSQLQuery): TBoldSQLTableReference;
begin
  if not assigned(Query) then
    Query := self.Query;
  assert(assigned(Query));
  result:= TableReferenceForTable(ObjectMapper.MainTable, Query, true);
end;

procedure TBoldSqlNode.NewQuery(NameSpace: tBoldSqlNameSpace);
begin
  Assert(assigned(ObjectMapper));
  Assert(not assigned(fQuery));

  fQuery := TBoldSQLQuery.Create(qmSelect, ObjectMapper.SystemPersistenceMapper.PSSystemDescription, ObjectMapper.SystemPersistenceMapper.SQLDataBaseConfig, NameSpace);
  EnsureRetrievalOfMainColumns;
end;

procedure TBoldSqlNode.EnsureRetrievalOfMainColumns;
var
  MaintableRef: TBoldSQLTableReference;
begin
  Query.ClearColumnsToRetrieve;
  MaintableRef := TableReferenceForTable(ObjectMapper.MainTable, Query, true);
  Query.AddColumnToRetrieve(MainTableRef.GetColumnReference(IDCOLUMN_NAME));
  Query.AddColumnToRetrieve(MainTableRef.GetColumnReference(TYPECOLUMN_NAME));
end;

procedure TBoldSqlNode.CopyTableReferences(node: TBoldSqlNode);
var
  i: integer;
begin
  for i := 0 to Node.GetTableReferences.Count-1 do
    fTableReferences.Add(Node.GetTableReferences[i]);
end;


procedure TBoldSqlNode.SetObjectMapper(const Value: TBoldObjectSQLMapper);
begin
  fObjectmapper := Value;
  if HasObjectMapper and (length(ObjectMapper.SystemPersistenceMapper.MappingInfo.GetAllInstancesMapping(ObjectMapper.ExpressionName)) > 1) then
    raise EBold.CreateFmt(sChildMappedClassesNotSupported, [ObjectMapper.ExpressionName]);
end;

procedure TBoldSqlNode.SetQuery(const Value: TBoldSqlQuery);
begin
  fQuery := Value;
end;


function TBoldSqlNode.TableReferenceForTable(Table: TBoldSQLTableDescription; Query: TBoldSqlQuery; ForceOwntable: Boolean): TBoldSqlTableReference;
var
  i: integer;
  TypeColRefWCF: TBoldSqlWCF;
  TypeIDWCF: TBoldSqlWCF;
  TypeColRef: TBoldSQLColumnReference;
  TypeWCF: TBoldSqlWCF;
  MappingInfos: TBoldAllInstancesMappingArray;
begin
  SetLength(MappingInfos, 0);
  assert(assigned(Table));
  assert(assigned(Query));
  assert(assigned(ObjectMapper), format('Node %s has not ObjectMapper', [ClassName]));


  if assigned(ObjectMapper) and (ObjectMapper.AllTables.IndexOf(Table) = -1) then
    raise EBoldInternal.createFmt('Table %s does not belong to class %s', [Table.SQLName, ObjectMapper.ExpressionName]);

  for i := 0 to fTableReferences.Count-1 do
    if (fTablereferences[i].TableDescription = Table) and
       Query.HastableReferenceInList(fTablereferences[i]) then
    begin
      result := fTablereferences[i];
      exit;
    end;

  result := Query.AddTableReference(Table.SQLName);
  fTablereferences.Add(Result);
  if fTablereferences.Count > 1 then
  begin
    Query.AddJoin(fTablereferences[0].GetColumnReference(IDCOLUMN_NAME), Result.GetColumnReference(IDCOLUMN_NAME));
    if fTableReferences[0].TableDescription.Versioned and Result.TableDescription.versioned then
      Query.AddJoin(fTablereferences[0].GetColumnReference(TIMESTAMPSTARTCOLUMNNAME), Result.GetColumnReference(TIMESTAMPSTARTCOLUMNNAME));
  end;

  if Table = ObjectMapper.MainTable then
  begin
    MappingInfos := ObjectMapper.SystemPersistenceMapper.MappingInfo.GetAllInstancesMapping(ObjectMapper.ExpressionName);
    if MappingInfos[0].ClassIdRequired then
    begin
      TypeIDWCF := TBoldSQLWCFGenericExpression.Create('('+(ObjectMapper as TBoldObjectDefaultMapper).SubClassesID+')');
      TypeColRef := Result.GetColumnReference(TYPECOLUMN_NAME);
      TypeColRefWCF := TBoldSQLWCFColumnRef.Create(TypeColRef);
      TypeWCF := TBoldSQLWCFBinaryInfix.Create(TypeColRefWCF, TypeIDWCF, 'IN');
      Query.AddWCF(TypeWCF);
    end;
  end;
end;

function TBoldSqlNode.GetHasQuery: boolean;
begin
  result := assigned(fQuery);
end;

function TBoldSqlNode.RelinquishQuery: TBoldSqlQuery;
begin
  assert(assigned(fQuery));
  result := fQuery;
  fQuery := nil;
end;

function TBoldSqlNode.RelinquishWCF: TBoldSqlWCF;
var
  QueryMainTable: TBoldSQLTableReference;
begin
  if assigned(fWCF) then
  begin
    result := fWCF;
    fWCF := nil;
  end
  else if assigned(fQuery) then
  begin
    QueryMainTable := fQuery.MainTable;
    fQuery.ClearColumnsToRetrieve;
    fQuery.AddColumnToRetrieve(QueryMainTable.GetColumnReference(IDCOLUMN_NAME));
    result := TBoldSQLWCFWithQuery.Create(fQuery);
    fQuery := nil;
  end
  else
    result := nil;
end;

function TBoldSqlNode.GetTableReferences: TBoldSqlTableReferenceList;
begin
  result := fTableReferences;
end;

{ TBoldSqlTypeNode }

procedure TBoldSqlTypeNode.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  inherited;
  v.VisitTBoldSqlTypeNode(self);
end;

constructor TBoldSqlTypeNode.Create(Position: integer; TypeName: string; TopSortedIndex: integer);
begin
  inherited create(Position);
  fTypeName := TypeName;
  fTopSortedIndex := TopSortedIndex;
end;

{ TBoldSqlIteration }

procedure TBoldSqlIteration.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  inherited;
  v.VisitTBoldSqlIteration(self);
end;

constructor TBoldSqlIteration.Create(Position: integer; OperationName: String; LoopVar: TBoldSqlVariableBinding);
begin
  inherited Create(Position, OperationName);
  fLoopVar := LoopVar;
end;

destructor TBoldSqlIteration.Destroy;
begin
  FreeandNil(fLoopVar);
  inherited;
end;

function TBoldSqlIteration.TableReferenceForTable(
  Table: TBoldSqlTableDescription;
  Query: TBoldSqlQuery; ForceOwntable: Boolean): TBoldSqlTableReference;
begin
  result := LoopVar.TableReferenceForTable(Table, Query, ForceOwntable);
end;


{ TBoldSqlMember }

procedure TBoldSqlMember.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  inherited;
  v.VisitTBoldSqlMember(self);
end;

constructor TBoldSqlMember.Create(Position: integer; const memberName: String; MemberIndex: Integer; MemberOf: TBoldSqlNode; IsBoolean: Boolean);
begin
  inherited Create(Position);
  fMemberName := memberName;
  fMemberIndex := MemberIndex;
  fMemberOf := MemberOf;
  fQualifier := TBoldSqlNodeList.Create;
  fIsBoolean := IsBoolean;
end;

destructor TBoldSqlMember.Destroy;
begin
  FreeAndNil(fMemberOf);
  FreeAndNil(fQualifier);
  inherited;
end;

function TBoldSqlMember.QueryOfMemberOfIsEnclosing: Boolean;
begin
  result := (MemberOf is TBoldSQLVariableReference) and
    (MemberOf as TBoldSQLVariableReference).VariableBinding.IsLoopVar;
end;

function TBoldSqlMember.TableReferenceForTable(Table: TBoldSqlTableDescription; Query: TBoldSqlQuery; ForceOwntable: Boolean): TBoldSqlTableReference;
begin
  if MemberOf is TBoldSqlVariablereference and not ForceOwntable then
    result := MemberOf.TableReferenceForTable(Table, Query, ForceOwntable)
  else
    result := inherited tablereferenceForTable(Table, Query, ForceOwnTable);
end;

{ TBoldSqlNodeList }


function TBoldSqlNodeList.Add(Item: TBoldSqlNode): Integer;
begin
  result := inherited add(item);
end;


destructor TBoldSqlNodeList.Destroy;
var
  i: integer;
begin
  for i := 0 to Count-1 do
    Items[i].Free;
  inherited;
end;

function TBoldSqlNodeList.GetItem(index: Integer): TBoldSqlNode;
begin
  result := TObject(Get(index)) as TBoldSqlNode;
end;

procedure TBoldSqlNodeList.PutItem(index: Integer; Value: TBoldSqlNode);
begin
  Put(index, value);
end;

procedure TBoldSqlNodeList.TraverseList(V: TBoldSqlNodeVisitor);
var
  i: integer;
begin
  for i := 0 to Count-1 do
    items[i].AcceptVisitor(v);
end;

{ TBoldSqlVariableBinding }

procedure TBoldSqlVariableBinding.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  inherited;
  v.VisitTBoldSqlVariableBinding(self);
end;

procedure TBoldSqlVariableBinding.AddRef;
begin
  inc(fRefCount);
  if (fRefCount > 1) and not fIsLoopVar then
    raise EBold.Create(sExternalVarsCanOnlyBereferencedOnce);
end;

procedure TBoldSqlVariableBinding.DecRef;
begin
  if (fRefCount = 0) then
    raise EBold.Create('negative refcount for external variables (or self)');
  dec(fRefCount);
end;

constructor TBoldSqlVariableBinding.Create(Position: integer; VariableName: String; TopSortedIndex: integer);
begin
  inherited Create(Position);
  fVariableName := VariableName;
  fTopSortedIndex := TopSortedIndex;
end;

{ TBoldSqlStrLiteral }

procedure TBoldSqlStrLiteral.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  v.VisitTBoldSqlStrLiteral(self);
end;


constructor TBoldSqlStrLiteral.Create(Position: integer; StrValue: String);
begin
  inherited Create(Position);
  fStrValue := StrValue;
end;

function TBoldSqlStrLiteral.GetAsString: String;
begin
  result := StrValue;
end;

{ TBoldSqlNodeVisitor }

procedure TBoldSqlNodeVisitor.VisitTBoldSqlEnumLiteral(N: TBoldSqlEnumLiteral);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlIntLiteral(N: TBoldSqlIntLiteral);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlIteration(N: TBoldSqlIteration);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlListCoercion(N: TBoldSqlListCoercion);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlLiteral(N: TBoldSqlLiteral);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlMember(N: TBoldSqlMember);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlNode(N: TBoldSqlNode);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlFloatLiteral(N: TBoldSqlFloatLiteral);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlOperation(N: TBoldSqlOperation);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlStrLiteral(N: TBoldSqlStrLiteral);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlTypeNode(N: TBoldSqlTypeNode);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlVariableBinding(N: TBoldSqlVariableBinding);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlVariableReference(N: TBoldSqlVariableReference);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlDateLiteral(
  N: TBoldSqlDateLiteral);
begin

end;

procedure TBoldSqlNodeVisitor.VisitTBoldSqlTimeLiteral(
  N: TBoldSqlTimeLiteral);
begin

end;

{ TBoldSqlVariableReference }

procedure TBoldSqlVariableReference.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  inherited;
  v.VisitTBoldSqlVariableReference(self);
end;

constructor TBoldSqlVariableReference.Create(Position: integer; VariableBinding: TBoldSqlVariableBinding);
begin
  inherited Create(Position);
  fVariableBinding := VariableBinding;
end;

function TBoldSqlVariableReference.GetHasQuery: boolean;
begin
  if VariableBinding.IsLoopVar then begin
    Result := inherited GetHasQuery;
  end else begin
    Result := VariableBinding.HasQuery;
  end;
end;

function TBoldSqlVariableReference.GetObjectMapper: TBoldObjectSQLMapper;
begin
  result := VariableBinding.ObjectMapper;
end;

function TBoldSqlVariableReference.GetQuery: TBoldSqlQuery;
begin
  Result := VariableBinding.Query
end;

function TBoldSqlVariableReference.RelinquishWCF: TBoldSqlWCF;
begin
  result := TBoldSQLWCFVariable.Create(VariableBinding);
end;

function TBoldSqlVariableReference.RelinquishQuery: TBoldSqlQuery;
begin
  result := VariableBinding.RelinquishQuery;
end;

procedure TBoldSqlVariableReference.SetObjectMapper(
  const Value: TBoldObjectSQLMapper);
begin
  raise EBoldInternal.Create('Can not set Objectmapper in a variablereference');
end;

procedure TBoldSqlVariableReference.SetQuery(const Value: TBoldSqlQuery);
begin
  raise EBoldInternal.Create('Can not set Query in a variablereference, try relinquish');
end;

function TBoldSqlVariableReference.TableReferenceForTable(
  Table: TBoldSqlTableDescription;
  Query: TBoldSqlQuery; ForceOwntable: Boolean): TBoldSqlTableReference;
begin
  result := VariableBinding.TableReferenceForTable(Table, Query, ForceOwntable)
end;

function TBoldSqlVariableReference.IsExternalVariable: Boolean;
begin
  result := assigned(VariableBinding) and assigned(VariableBinding.Context);
end;



function TBoldSqlVariableReference.GetTableReferences: TBoldSqlTableReferenceList;
begin
  result := fVariableBinding.GetTableReferences;
end;

{ TBoldSqlEnumLiteral }

procedure TBoldSqlEnumLiteral.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  inherited;
  v.VisitTBoldSqlEnumLiteral(self);
end;

constructor TBoldSqlListCoercion.Create(Position: integer; Child: TBoldSqlNode);
begin
  inherited create(Position);
  fChild := Child;
end;

destructor TBoldSqlListCoercion.destroy;
begin
  FreeAndNil(fChild);
  inherited;
end;

constructor TBoldSqlEnumLiteral.Create(Position, IntValue: integer; Name: String);
begin
  inherited Create(Position);
  fIntValue := IntValue;
  fName := Name;
end;

function TBoldSqlEnumLiteral.GetAsString: String;
begin
  result := Name;
end;

{ TBoldSqlLiteral }

procedure TBoldSqlLiteral.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  inherited;
  v.VisitTBoldSqlLiteral(self);
end;


{ TBoldSqlFloatLiteral }

procedure TBoldSqlFloatLiteral.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  inherited;
  v.VisitTBoldSqlFloatLiteral(self);
end;

constructor TBoldSqlFloatLiteral.Create(Position: integer; FloatValue: Double);
begin
  inherited Create(Position);
  fFloatValue := FloatValue;
end;

function TBoldSqlFloatLiteral.GetAsString: String;
begin
  result := format('%e', [FloatValue]);
end;

{ TBoldSqlIntLiteral }

procedure TBoldSqlIntLiteral.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  inherited;
  v.VisitTBoldSqlIntLiteral(self);
end;

constructor TBoldSqlIntLiteral.Create(Position, IntValue: Integer);
begin
  inherited Create(Position);
  fIntValue := IntValue;
end;

function TBoldSqlIntLiteral.GetAsString: String;
begin
  result := IntToStr(IntValue);
end;

{ TBoldSqlSymbol }

procedure TBoldSqlSymbol.BuildWCFOrQuery(OperationNode: TBoldSQLOperation; NameSpace: TBoldSqlNameSpace);
begin
  raise EBoldInternal.CreateFmt('%s can not build WCFs or Queries...', [name]);
end;

function TBoldSqlSymbol.GetSQLName: String;
begin
  result := Name;
end;

function TBoldSqlSymbol.ResolveObjectMapper(OperationNode: TBoldSqlOperation): TBoldObjectSqlMapper;
begin
  result := nil;
end;

{ TBoldSQLWCFVariable }
constructor TBoldSQLWCFVariable.Create(
  VariableBinding: TBoldSqlVariableBinding);
begin
  fVariableBinding := VariableBinding;
end;

destructor TBoldSQLWCFVariable.Destroy;
begin
  inherited;
end;

function TBoldSQLWCFVariable.GetAsString(Query: TBoldSQlQuery): String;
begin
  if not Assigned(fParam) then
    fParam := Query.AddParam;
  fParam.Name:=fVariableBinding.VariableName;
  fParam.ParamType:=ptInput;
  fParam.Value:=fVariableBinding.ExternalVarValue;
  result := ':'+fParam.Name;
end;


{ TBoldSqlDateLiteral }

procedure TBoldSqlDateLiteral.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  inherited;
  v.VisitTBoldSqlDateLiteral(self);
end;

constructor TBoldSqlDateLiteral.Create(Position: integer;
  DateValue: TDateTime);
begin
  inherited Create(position);
  fDateValue := Datevalue;
end;

function TBoldSqlDateLiteral.GetAsString: String;
begin
  result := DateToStr(fDateValue);
end;

{ TBoldSqlTimeLiteral }

procedure TBoldSqlTimeLiteral.AcceptVisitor(V: TBoldSqlNodeVisitor);
begin
  inherited;
  v.VisitTBoldSqlTimeLiteral(self);
end;

constructor TBoldSqlTimeLiteral.Create(Position: integer;
  TimeValue: TDateTime);
begin
  inherited Create(Position);
  fTimeValue := TimeValue;
end;

function TBoldSqlTimeLiteral.GetAsString: String;
begin
  result := TimeToStr(TimeValue);
end;

end.
