
{ Global compiler directives }
{$include bold.inc}
unit BoldSqlNodeMaker;

interface
uses
  BoldSqlNodes,
  BoldOclLightWeightNodes;

type
  TBoldSqlNodeMaker = class(TBoldOLWNodeVisitor)
  private
    fCondition: TBoldOclCondition;
    fSqlVarBindings: TBoldSQlNodelist;
    fOLWVarBindings: TBoldOLWNodeList;
    fRootNode: TBoldSQLNode;
  protected
    property OLWVarBindings: TBoldOLWNodeList read fOLWVarBindings;
  public
    constructor Create(Condition: TBoldOclCondition);
    destructor Destroy; override;
    procedure Execute;
    procedure VisitTBoldOLWNode(N: TBoldOLWNode); override;
    procedure VisitTBoldOLWListCoercion(N: TBoldOLWListCoercion); override;
    procedure VisitTBoldOLWOperation(N: TBoldOLWOperation); override;
    procedure VisitTBoldOLWIteration(N: TBoldOLWIteration); override;
    procedure VisitTBoldOLWMember(N: TBoldOLWMember); override;
    procedure VisitTBoldOLWLiteral(N: TBoldOLWLiteral); override;
    procedure VisitTBoldOLWStrLiteral(N: TBoldOLWStrLiteral); override;
    procedure VisitTBoldOLWFloatLiteral(N: TBoldOLWFloatLiteral); override;
    procedure VisitTBoldOLWEnumLiteral(N: TBoldOLWEnumLiteral); override;
    procedure VisitTBoldOLWDateLiteral(N: TBoldOLWDateLiteral); override;
    procedure VisitTBoldOLWTimeLiteral(N: TBoldOLWTimeLiteral); override;
    procedure VisitTBoldOLWIntLiteral(N: TBoldOLWIntLiteral); override;
    procedure VisitTBoldOLWVariableBinding(N: TBoldOLWVariableBinding); override;
    procedure VisitTBoldOLWVariableReference(N: TBoldOLWVariableReference); override;
    procedure VisitTBoldOLWTypeNode(N: TBoldOLWTypeNode); override;
    function SQLBindingForVarBinding(VarBinding: TBoldOLWVariableBinding): TBoldSQLVariableBinding;
    property RootNode: TBoldSqlNode read fRootNode;
    property SQLVarBindings: TBoldSQLNodelist read fSqlVarBindings;
  end;


implementation

uses
  Classes,
  SysUtils;

{ TBoldSqlNodeMaker }

constructor TBoldSqlNodeMaker.create(Condition: TBoldOclCondition);
begin
  inherited Create;
  fSqlVarBindings := TBoldSQLNodeList.Create;
  fOLWVarBindings := TBoldOLWNodeList.Create;
  fCondition := Condition;
end;

destructor TBoldSqlNodeMaker.destroy;
var
  i: integer;
begin
  for i := fOLWVarBindings.Count-1 downto 0 do
    fOLWVarBindings[i] := nil;
  FreeAndNil(fOLWVarBindings);

  for i := fSQLVarBindings.Count-1 downto 0 do
  begin
    if TBoldSqlVariableBinding(fSQLVarBindings[i]).IsExternal then
    begin
      TBoldSqlVariableBinding(fSQLVarBindings[i]).DecRef;
        if TBoldSqlVariableBinding(fSQLVarBindings[i]).RefCount = 0 then
      TBoldSqlVariableBinding(fSQLVarBindings[i]).Free;
    end;
    fSQLVarBindings[i] := nil;
  end;
  FreeAndNil(fSqlVarbindings);
  
  FreeAndNil(fRootNode);
  inherited;
end;


function TBoldSqlNodeMaker.SQLBindingForVarBinding(
  VarBinding: TBoldOLWVariableBinding): TBoldSQLVariableBinding;
var
  i: integer;
begin
  result := nil;
  for i := 0 to fOLWVarBindings.Count-1 do
    if CompareText((fOLWVarBindings[i] as TBoldOLWVariableBinding).variablename, VarBinding.VariableName) = 0 then
    begin
      result := TBoldSQLVariableBinding(fSqlVarBindings[i]);
      break;
    end;
end;


procedure TBoldSqlNodeMaker.VisitTBoldOLWEnumLiteral(N: TBoldOLWEnumLiteral);
begin
  fRootNode := TBoldSqlEnumLiteral.create(n.Position, n.Intvalue, n.name);
end;

procedure TBoldSqlNodeMaker.VisitTBoldOLWIntLiteral(N: TBoldOLWIntLiteral);
begin
  fRootNode := TBoldSqlIntLiteral.Create(n.Position, n.IntValue);
end;

procedure TBoldSqlNodeMaker.VisitTBoldOLWIteration(N: TBoldOLWIteration);
var
  SqlIteration: TBoldSqlIteration;
  i: integer;
begin
  n.LoopVar.AcceptVisitor(self);
  SqlIteration := TBoldSqlIteration.create(n.Position, n.OperationName, RootNode as TBoldSqlVariableBinding);
  for i := 0 to n.Args.Count-1 do
  begin
    n.Args[i].AcceptVisitor(self);
    SqlIteration.Args.Add(RootNode);
  end;
  fRootNode := SqlIteration;
end;

procedure TBoldSqlNodeMaker.VisitTBoldOLWListCoercion(N: TBoldOLWListCoercion);
begin
  n.Child.AcceptVisitor(self);
  fRootNode := TBoldSqlListCoercion.Create(n.Position, RootNode);
end;

procedure TBoldSqlNodeMaker.VisitTBoldOLWLiteral(N: TBoldOLWLiteral);
begin
end;

procedure TBoldSqlNodeMaker.VisitTBoldOLWMember(N: TBoldOLWMember);
var
  SqlMember: TBoldSqlMember;
  i: integer;
  IsBoolean: Boolean;
begin
  inherited;
  n.MemberOf.AcceptVisitor(self);
  IsBoolean := n.IsBoolean;
  SqlMember := TBoldSqlMember.Create(n.Position, n.MemberName, n.MemberIndex, RootNode, IsBoolean);

  if assigned(n.Qualifier) then
    for i := 0 to n.Qualifier.Count-1 do begin
      n.Qualifier[i].AcceptVisitor(self);
      SqlMember.Qualifier.Add(RootNode);
    end;
  fRootNode := SqlMember;
end;


procedure TBoldSqlNodeMaker.VisitTBoldOLWNode(N: TBoldOLWNode);
begin
end;

procedure TBoldSqlNodeMaker.VisitTBoldOLWFloatLiteral(N: TBoldOLWFloatLiteral);
begin
  fRootNode := TBoldSqlFloatLiteral.Create(n.Position, n.FloatValue);
end;


procedure TBoldSqlNodeMaker.VisitTBoldOLWOperation(N: TBoldOLWOperation);
var
  i: integer;
  SqlOperation: TBoldSqlOperation;
begin
  if n.ClassType = TBoldOLWOperation then
  begin
    SqlOperation := TBoldSqlOperation.create(n.Position, n.OperationName);
    for i := 0 to n.Args.Count-1 do
    begin
      n.Args[i].AcceptVisitor(self);
      SqlOperation.Args.Add(RootNode);
    end;
    fRootNode := SqlOperation;
  end;
end;

procedure TBoldSqlNodeMaker.VisitTBoldOLWStrLiteral(N: TBoldOLWStrLiteral);
begin
  inherited;
  fRootNode := TBoldSqlStrLiteral.Create(n.Position, n.StrValue);
end;

procedure TBoldSqlNodeMaker.VisitTBoldOLWTypeNode(N: TBoldOLWTypeNode);
begin
  fRootNode := TBoldSqlTypeNode.Create(n.Position, N.typeName, (n.TopSortedIndex));
end;

procedure TBoldSqlNodeMaker.VisitTBoldOLWVariableBinding(N: TBoldOLWVariableBinding);
var
  i: integer;
  TopSortedIndex: integer;
  VarBind: TBoldSqlVariableBinding;
begin
  varBind := nil;
  for i := 0 to SQLVarBindings.Count-1 do
    if CompareText((SQLVarBindings[i] as TBoldSqlVariableBinding).VariableName, n.variableName) = 0 then
      VarBind := SQLVarBindings[i] as TBoldSqlVariableBinding;

  if not assigned(VarBind) then
  begin
    TopSortedIndex := n.TopSortedIndex;
    VarBind := TBoldSqlVariableBinding.Create(n.Position, n.VariableName, TopSortedIndex);

    VarBind.ExternalVarvalue := n.ExternalVarvalue;

    OLWVarbindings.Add(n);
    SQLVarBindings.Add(VarBind);
  end;

  fRootNode := VarBind;
end;

procedure TBoldSqlNodeMaker.VisitTBoldOLWVariableReference(N: TBoldOLWVariableReference);
var
  VarBind: TBoldSqlVariableBinding;
begin
  VarBind := SQLBindingForVarBinding(n.VariableBinding);

  if not assigned(VarBind) then
  begin
    n.VariableBinding.AcceptVisitor(self);
    VarBind := RootNode as TBoldSQLVariableBinding;
    varBind.IsExternal := true;

    if SQLVarBindings.IndexOf(VarBind) = -1 then
    begin
      OLWVarBindings.Add(n.VariableBinding);
      SQLVarBindings.Add(VarBind);
    end;
  end;

  fRootNode := TBoldSqlVariableReference.create(n.Position, VarBind)
end;

procedure TBoldSqlNodeMaker.Execute;
var
  i: integer;
begin
  for i := 0 to fCondition.env.Count-1 do
  begin
    fCondition.Env[i].AcceptVisitor(self);
    TBoldSqlVariableBinding(SQLVarBindings[SQLVarBindings.Count-1]).isExternal := true;
  end;
  fCondition.RootNode.AcceptVisitor(self);
end;


procedure TBoldSqlNodeMaker.VisitTBoldOLWDateLiteral(
  N: TBoldOLWDateLiteral);
begin
  fRootNode := TBoldSqlDateLiteral.Create(n.Position, n.DateValue);
end;

procedure TBoldSqlNodeMaker.VisitTBoldOLWTimeLiteral(
  N: TBoldOLWTimeLiteral);
begin
  fRootNode := TBoldSqlTimeLiteral.Create(n.Position, n.TimeValue);
end;

end.
