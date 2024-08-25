
{ Global compiler directives }
{$include bold.inc}
unit BoldSqlQueryGenerator;

interface

uses
  BoldSqlQuery,
  BoldSqlNodes;

type
  { forward declarations }
  TBoldSqlQueryGenerator = class;

  { TBoldSqlQueryGenerator }
  TBoldSqlQueryGenerator = class(TBoldSqlNodeVisitor)
  private
    fNameSpace: TBoldSqlNameSpace;
    procedure HandleRelation(N: TBoldSqlMember);
    procedure HandleAttribute(N: TBoldSqlMember);
  protected
    procedure VisitTBoldSqlNode(N: TBoldSqlNode); override;
    procedure VisitTBoldSqlListCoercion(N: TBoldSqlListCoercion); override;
    procedure VisitTBoldSqlOperation(N: TBoldSqlOperation); override;
    procedure VisitTBoldSqlIteration(N: TBoldSqlIteration); override;
    procedure VisitTBoldSqlMember(N: TBoldSqlMember); override;
    procedure VisitTBoldSqlLiteral(N: TBoldSqlLiteral); override;
    procedure VisitTBoldSqlStrLiteral(N: TBoldSqlStrLiteral); override;
    procedure VisitTBoldSqlFloatLiteral(N: TBoldSqlFloatLiteral); override;
    procedure VisitTBoldSqlDateLiteral(N: TBoldSqlDateLiteral); override;
    procedure VisitTBoldSqlTimeLiteral(N: TBoldSqlTimeLiteral); override;
    procedure VisitTBoldSqlEnumLiteral(N: TBoldSqlEnumLiteral); override;
    procedure VisitTBoldSqlIntLiteral(N: TBoldSqlIntLiteral); override;
    procedure VisitTBoldSqlVariableBinding(N: TBoldSqlVariableBinding); override;
    procedure VisitTBoldSqlVariableReference(N: TBoldSqlVariableReference); override;
    procedure VisitTBoldSqlTypeNode(N: TBoldSqlTypeNode); override;
  public
    constructor Create(NameSpace: TBoldSqlNameSpace);
  end;

implementation

uses
  BoldDefs,
  BoldPSDescriptionsSQL,
  BoldPMappersSQL,
  BoldPmappersDefault,
  BoldPMappersLinkDefault,
  BoldPMappersAttributeDefault;

{ TBoldSqlQueryGenerator }

constructor TBoldSqlQueryGenerator.Create(NameSpace: TBoldSqlNameSpace);
begin
  inherited create;
  fNameSpace := NameSpace;
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlEnumLiteral(
  N: TBoldSqlEnumLiteral);
begin
  n.WCF := TBoldSQLWCFInteger.Create(n.Intvalue);
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlFloatLiteral(
  N: TBoldSqlFloatLiteral);
begin
  n.WCF := TBoldSQLWCFFloat.Create(n.Floatvalue);
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlIntLiteral(
  N: TBoldSqlIntLiteral);
begin
  n.WCF := TBoldSQLWCFInteger.Create(n.Intvalue);
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlIteration(N: TBoldSqlIteration);
var
  i: integer;
begin
  n.Args[0].AcceptVisitor(self);
  n.LoopVar.Acceptvisitor(self);

  n.LoopVar.Query := n.args[0].RelinquishQuery;
  n.LoopVar.WCF := n.args[0].RelinquishWCF;
  n.LoopVar.CopyTableReferences(n.Args[0]);

  for i := 1 to n.Args.Count-1 do
    n.Args[i].AcceptVisitor(self);

  n.CopyTableReferences(n.LoopVar);
  n.Symbol.BuildWCFOrQuery(n, fNameSpace);
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlListCoercion(
  N: TBoldSqlListCoercion);
begin
  n.Child.AcceptVisitor(self);
  n.Query         := n.Child.RelinquishQuery;
  n.WCF           := n.child.RelinquishWCF;
  n.ObjectMapper  := n.Child.Objectmapper;
  n.CopyTableReferences(n.Child);
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlLiteral(N: TBoldSqlLiteral);
begin
end;

procedure TBoldSqlQueryGenerator.HandleRelation(N: TBoldSqlMember);
var
  LastJoinLeftColumn: TBoldSQLColumnDescription;
  TableForLastJoinLeft: TBoldSQLTableReference;
  TableRefForLink: TBoldSQLTableReference;
  LinkMapper: TBoldNonEmbeddedLinkDefaultMapper;
begin
  if n.QueryOfMemberOfIsEnclosing then
    n.NewQuery(fNameSpace)
  else
    n.Query := n.MemberOf.RelinquishQuery;

  n.EnsureRetrievalOfMainColumns;

  if (n.MemberMapper is TBoldEmbeddedSingleLinkDefaultMapper) then
  begin
    LastJoinLeftColumn := n.MemberMapper.ColumnDescriptions[0] as TBoldSQLColumnDescription;
    TableForLastJoinLeft := n.MemberOf.TableReferenceForTable(LastJoinLeftColumn.TableDescription, n.Query, true);
    n.Query.AddJoin(n.MainTableRef.GetColumnReference(IDCOLUMN_NAME), TableForLastJoinLeft.GetColumnReference(LastJoinLeftColumn.SQLName));
  end
  else if (n.MemberMapper is TBoldNonEmbeddedLinkDefaultMapper) then
  begin
    LinkMapper := n.MemberMapper as TBoldNonEmbeddedLinkDefaultMapper;
    if LinkMapper.IsIndirect then
    begin
      TableRefForLink := n.Query.AddTableReference(LinkMapper.LinkClassTableName);

      n.Query.AddJoin(TablerefForlink.GetColumnReference(LInkMapper.ClosestColumnName),
        n.MemberOf.MaintableRef(n.Query).GetColumnReference(IDCOLUMN_NAME));

      n.Query.AddJoin(TablerefForlink.GetColumnReference(LInkMapper.RemoteInnerLinkMapper.MainColumnName),
        n.MaintableRef.GetColumnReference(IDCOLUMN_NAME));
    end
    else
    begin
      n.Query.AddJoin(
        n.MainTableRef.GetColumnReference(LinkMapper.ClosestColumnName),
        n.MemberOf.MainTableRef(n.Query).GetColumnReference(IDCOLUMN_NAME));
    end;
  end;
end;

procedure TBoldSqlQueryGenerator.HandleAttribute(N: TBoldSqlMember);
var
  TableRef: TBoldSQLTableReference;
  MainColumn: TBoldSQlColumnDescription;
  ColumnRef: TBoldSQlColumnReference;
  WCF: TBoldSQLWCF;
  Query: TBoldSQlQuery;
begin
  MainColumn  := n.MemberMapper.ColumnDescriptions[0] as TBoldSQlColumnDescription;
  Tableref    := n.MemberOf.TableReferenceForTable(MainColumn.TableDescription, n.MemberOf.Query, True);
  ColumnRef   := TableRef.GetColumnReference(MainColumn.SQlName);
  if n.isBoolean and (n.MemberMapper is TBoldPMInteger) then
  begin
    WCF := TBoldSQLWCFBinaryInfix.Create(
      TBoldSQLWCFColumnRef.Create(ColumnRef),
      TBoldSQLWCFInteger.Create(1), '=');
      
    if not n.QueryOfMemberOfIsEnclosing then
    begin
      Query := n.MemberOf.RelinquishQuery;
      Query.AddWCF(WCF);
      WCF := TBoldSQLWCFExists.Create(Query, Query.MainTable);
    end;
    n.WCF := WCF;
  end else
  begin
    n.WCF := TBoldSQLWCFColumnRef.Create(ColumnRef);
    if not n.QueryOfMemberOfIsEnclosing then
      n.Query := n.MemberOf.RelinquishQuery;
  end;
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlMember(N: TBoldSqlMember);
begin
  n.MemberOf.AcceptVisitor(self);

  Assert(assigned(n.memberOf.Query));

  if n.MemberMapper is TBoldSingleColumnMember then
    HandleAttribute(n)
  else if (n.MemberMapper is TBoldEmbeddedSingleLinkDefaultMapper) or
          (n.MemberMapper is TBoldNonEmbeddedLinkDefaultMapper) then
    HandleRelation(n);
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlNode(N: TBoldSqlNode);
begin
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlOperation(N: TBoldSqlOperation);
begin
  if n.ClassType = TBoldSQLOperation then
  begin
    n.Args.TraverseList(self);
    n.Symbol.BuildWCFOrQuery(n, fNameSpace);
  end;
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlStrLiteral(N: TBoldSqlStrLiteral);
begin
  n.WCF := TBoldSQLWCFString.Create(n.StrValue);
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlTypeNode(N: TBoldSqlTypeNode);
begin
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlVariableBinding(
  N: TBoldSqlVariableBinding);
begin
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlVariableReference(
  N: TBoldSqlVariableReference);
begin
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlDateLiteral(
  N: TBoldSqlDateLiteral);
begin
  n.WCF := TBoldSQLWCFDate.Create(n.DateValue);
end;

procedure TBoldSqlQueryGenerator.VisitTBoldSqlTimeLiteral(
  N: TBoldSqlTimeLiteral);
begin
  n.WCF := TBoldSQLWCFTime.Create(n.TimeValue);
end;

end.
