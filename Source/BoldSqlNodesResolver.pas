{ Global compiler directives }
{$include bold.inc}
unit BoldSqlNodesResolver;

interface

uses
  BoldDefs,
  BoldPMappersSQL,
  BoldSqlSymbols,
  BoldSqlNodes;

type
  { forward declarations }
  TBoldSqlNodeResolver = class;

  { TBoldSqlNodeResolver }
  TBoldSqlNodeResolver = class(TBoldSqlNodeVisitor)
  private
    fSystemMapper: TBoldSystemSQLMapper;
    fRootNode: TBoldSQLNode;
    fExternalVariables: TBoldSQLNodeList;
  protected
    function FindSymbolByName(const Name: string): TBSS_Symbol; 
    procedure VisitTBoldSqlNode(N: TBoldSqlNode); override;
    procedure VisitTBoldSqlListCoercion(N: TBoldSqlListCoercion); override;
    procedure VisitTBoldSqlOperation(N: TBoldSqlOperation); override;
    procedure VisitTBoldSqlIteration(N: TBoldSqlIteration); override;
    procedure VisitTBoldSqlMember(N: TBoldSqlMember); override;
    procedure VisitTBoldSqlLiteral(N: TBoldSqlLiteral); override;
    procedure VisitTBoldSqlStrLiteral(N: TBoldSqlStrLiteral); override;
    procedure VisitTBoldSqlFloatLiteral(N: TBoldSqlFloatLiteral); override;
    procedure VisitTBoldSqlEnumLiteral(N: TBoldSqlEnumLiteral); override;
    procedure VisitTBoldSqlDateLiteral(N: TBoldSqlDateLiteral); override;
    procedure VisitTBoldSqlTimeLiteral(N: TBoldSqlTimeLiteral); override;
    procedure VisitTBoldSqlIntLiteral(N: TBoldSqlIntLiteral); override;
    procedure VisitTBoldSqlVariableBinding(N: TBoldSqlVariableBinding); override;
    procedure VisitTBoldSqlVariableReference(N: TBoldSqlVariableReference); override;
    procedure VisitTBoldSqlTypeNode(N: TBoldSqlTypeNode); override;
  public
    constructor Create(SystemMapper: TBoldSystemSQLMapper; RootNode: TBoldSQLNode; ExternalVariables: TBoldSQLNodeList);
    procedure Execute;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldPMappersLinkDefault,
  BoldPMappers,
  BoldIndex,
  BoldIndexableList;

{ TBoldSqlNodeResolver }

constructor TBoldSqlNodeResolver.Create(SystemMapper: TBoldSystemSQLMapper; RootNode: TBoldSQLNode; ExternalVariables: TBoldSQLNodeList);
begin
  inherited Create;
  fRootNode := RootNode;
  fExternalVariables := ExternalVariables;
  fSystemMapper := SystemMapper;
end;

procedure TBoldSqlNodeResolver.Execute;
var
  i: integer;
begin
  if assigned(fExternalVariables) then
    for i := 0 to fExternalVariables.Count - 1 do
      fExternalVariables[i].AcceptVisitor(self);
  if assigned(fRootNode) then
    fRootNode.AcceptVisitor(self);
end;

function TBoldSqlNodeResolver.FindSymbolByName(const Name: string): TBSS_Symbol;
begin
  result := SqlSymbolDictionary.SymbolByName[Name];
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlDateLiteral(
  N: TBoldSqlDateLiteral);
begin
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlEnumLiteral(N: TBoldSqlEnumLiteral);
begin
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlFloatLiteral(N: TBoldSqlFloatLiteral);
begin
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlIntLiteral(N: TBoldSqlIntLiteral);
begin
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlIteration(N: TBoldSqlIteration);
var
  i: integer;
begin
  n.args[0].AcceptVisitor(self);

  n.Symbol := FindSymbolByName(n.OperationName);
  n.ObjectMapper := n.Symbol.ResolveObjectMapper(n);

  n.LoopVar.ObjectMapper := n.ObjectMapper;
  n.LoopVar.AcceptVisitor(self);
  n.LoopVar.IsLoopVar := true;

  for i := 1 to n.args.count - 1 do
    n.args[i].AcceptVisitor(self);
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlListCoercion(N: TBoldSqlListCoercion);
begin
  n.child.acceptVisitor(self);
  n.ObjectMapper  := n.Child.Objectmapper;
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlLiteral(N: TBoldSqlLiteral);
begin
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlMember(N: TBoldSqlMember);
begin
  n.MemberOf.AcceptVisitor(self);
  try
    n.MemberMapper := n.MemberOf.ObjectMapper.MemberPersistenceMappers[n.MemberOf.ObjectMapper.MemberMapperIndexByMemberIndex[n.MemberIndex]] as TBoldMemberSQLMapper;
  except
    on EAssertionFailed do
    begin
      if not n.MemberOf.HasObjectMapper then
        raise EBold.CreateFmt('ObjectMapper not found for member ''%s'', possibly due to unsupported combination of child/parent mapping.', [n.MemberName])
      else
        raise;
    end;
  end;

  if n.MemberMapper is TBoldLinkDefaultMapper then
    n.ObjectMapper := (n.MemberMapper as TBoldLinkDefaultMapper).OtherEndObjectMapper;
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlNode(N: TBoldSqlNode);
begin
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlOperation(N: TBoldSqlOperation);
begin
  if n.ClassType = TBoldSQLOperation then
  begin
    n.Symbol := FindSymbolByName(n.OperationName);
    if not Assigned(n.Symbol) then
      raise EBold.CreateFmt('InPs SQLSymbol ''%s'' not found, possibly not available for InPs evaluation.', [n.OperationName]);
    n.Args.TraverseList(self);
    n.ObjectMapper := n.Symbol.ResolveObjectMapper(n);
    if not n.HasObjectMapper then
      n.ObjectMapper := n.Symbol.ResolveObjectMapper(n);
  end;
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlStrLiteral(N: TBoldSqlStrLiteral);
begin
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlTimeLiteral(N: TBoldSqlTimeLiteral);
begin
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlTypeNode(N: TBoldSqlTypeNode);
begin
  n.ObjectMapper := fSystemMapper.ObjectPersistenceMappers[n.TopSortedIndex] as TBoldObjectSQLMapper;
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlVariableBinding(N: TBoldSqlVariableBinding);
begin
  if not n.HasObjectMapper and (n.TopSortedIndex <> -1) then
    n.ObjectMapper := fSystemMapper.ObjectPersistenceMappers[n.TopSortedIndex] as TBoldObjectSQlMapper;
end;

procedure TBoldSqlNodeResolver.VisitTBoldSqlVariableReference(N: TBoldSqlVariableReference);
begin
  n.VariableBinding.AddRef;
end;

end.
