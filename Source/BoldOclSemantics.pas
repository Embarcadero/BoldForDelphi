{ Global compiler directives }
{$include bold.inc}
unit BoldOclSemantics;

interface

uses
  BoldSystemRT,
  BoldElements,
  BoldOclClasses,
  Classes;

type
  { forward declarations }
  TBoldOclSemanticsVisitor = class;

  { TBoldOclSemanticsVisitor }
  TBoldOclSemanticsVisitor = class(TBoldOclVisitor)
  private
    CurrentSystemTypeInfo: TBoldSystemTypeInfo;
    replacementNode: TBoldOCLNode;
    ReSubscribe: Boolean;
    SymbolTable: TBoldSymbolDictionary;
    fEnv: TBoldOclEnvironment;
    fEvaluator: TBoldEvaluator;
    fIgnoreNelCompatibility: Boolean;
    fReferencedVariables: TList;
    function GetReferencedVariables: TList;
    function GetHasReferencedVariables: boolean;
  public
    constructor Create(Model: TBoldSystemTypeInfo; Evaluator: TBoldEvaluator; SymTab: TBoldSymbolDictionary; Env: TBoldOclEnvironment);
    destructor Destroy; override;
    procedure AddListCoercionOnArgs(N: TBoldOCLOperation);
    procedure CheckArgumentType(actArg: TBoldOCLNode; FormArg: TBoldElementTypeInfo);
    procedure DeduceBoldType(N: TBoldOCLOperation);
    function FindSymbol(const name: string): TBoldOclSymbol;
    function LeastCommonSuperClass(C1, C2: TBoldClassTypeInfo): TBoldClassTypeInfo;
    function LeastCommonSuperType(AttributeTypeInfo1, AttributeTypeInfo2: TBoldAttributetypeInfo): TBoldAttributeTypeInfo;
    procedure PushResubscribe(var ReSubscribe, OldReSubscribe: Boolean);
    function TestForOperation(n: TBoldOclNode): Boolean;
    procedure Traverse(var Node: TBoldOCLNode);
    procedure TraverseList(List: TBoldOCLNodeList; Start, Stop: Integer);
    procedure VisitTBoldOclCollectionLiteral(N: TBoldOclCollectionLiteral); override;
    procedure VisitTBoldOclEnumLiteral(N: TBoldOclEnumLiteral); override;
    procedure VisitTBoldOclIntLiteral(N: TBoldOclIntLiteral); override;
    procedure VisitTBoldOclDateLiteral(N: TBoldOclDateLiteral); override;
    procedure VisitTBoldOclTimeLiteral(N: TBoldOclTimeLiteral); override;
    procedure VisitTBoldOclIteration(N: TBoldOclIteration); override;
    procedure VisitTBoldOclMember(N: TBoldOclMember); override;
    procedure VisitTBoldOclMethod(N: TBoldOclMethod); override;
    procedure VisitTBoldOclNumericLiteral(N: TBoldOclNumericLiteral); override;
    procedure VisitTBoldOclOperation(N: TBoldOCLOperation); override;
    procedure VisitTBoldOclStrLiteral(N: TBoldOclStrLiteral); override;
    procedure VisitTBoldOclTypeNode(N: TBoldOclTypeNode); override;
    procedure VisitTBoldOclVariableReference(N: TBoldOCLVariableReference); override;
    property IgnoreNelCompatibility: Boolean read fIgnoreNelCompatibility write fIgnoreNelCompatibility;
    property ReferencedVariables: TList read GetReferencedVariables;
    property HasReferencedVariables: boolean read GetHasReferencedVariables;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldOclError,
  BoldOcl;

const
  BoldOclDeduceMethodName: array[TBoldOclDeduceMethod] of String = (
    'No DeduceMethod', 'Same type as loopvariable', 'Same type as arg1',
    'Same type as listelement of arg1', 'Same type as arg2', 'Same type as arg3',
    'Least common supertype of arg1 and arg2', 'Least common supertype of arg2 and arg3',
    'Same type as listelement of arg2', 'Type is ObjectList', 'Type is Metatype', 'Type is arg2 (typecast)', 'Type is arg1 ', 'Arg1 as a list', 'a list with the type of Arg2');

constructor TBoldOclSemanticsVisitor.Create(Model: TBoldSystemTypeInfo; Evaluator: TBoldEvaluator; SymTab: TBoldSymbolDictionary; Env: TBoldOclEnvironment);
begin
  inherited Create;
  CurrentSystemTypeInfo := Model;
  fEnv := Env;
  fEvaluator := Evaluator;
  SymbolTable := SymTab;
end;

destructor TBoldOclSemanticsVisitor.Destroy;
begin
  FreeAndNil(fReferencedVariables);
  inherited;
end;

procedure TBoldOclSemanticsVisitor.PushResubscribe(var ReSubscribe, OldReSubscribe: Boolean);
begin
  OldReSubscribe := ReSubscribe;
  ReSubscribe := True;
end;

procedure TBoldOclSemanticsVisitor.CheckArgumentType(actArg: TBoldOCLNode; FormArg: TBoldElementTypeInfo);
begin
  if assigned(FormArg) then
  begin
    if not actArg.BoldType.ConformsTo(FormArg) then
      raise EBoldOCLAbort.CreateFmt(boeNoConform,
      [actArg.Position, actArg.BoldType.AsString, FormArg.AsString]);
  end;
  { else begin
    if actArg.BoldType is TBoldSystemtypeInfo then
      raise EBoldOCLAbort.CreateFmt(boeSystemAsOperationArgument, [actArg.Position]);

  end;
  }
  if not (actArg.BoldType is TBoldListTypeInfo) and
    (FormArg is TBoldListTypeInfo) then
    actArg.NeedsListCoercion := True;
end;

procedure TBoldOclSemanticsVisitor.Traverse(var Node: TBoldOCLNode);
var
  TempNode: TBoldOCLLIstCoercion;
  LocalReSubscribe: Boolean;
begin

  if Node is TBoldOCLLIstCoercion then
  begin
    TempNode := TBoldOCLLIstCoercion(Node);
    Node := TempNode.Child;
    Node.NeedsListCoercion := false;
    TempNode.Child := nil;
    TempNode.Free;
  end;

  Node.ReSubscribe := ReSubscribe;
  Node.SetReferenceValue(nil);

  LocalReSubscribe := ReSubscribe;
  try
    Node.AcceptVisitor(self);
    ReSubscribe := LocalReSubscribe;
  finally
    if assigned(replacementNode) then
    begin
      Node.Free;
      Node := replacementNode;
      ReplacementNode := nil;
    end;
  end;
end;

procedure TBoldOclSemanticsVisitor.TraverseList(List: TBoldOCLNodeList;
    Start, Stop: Integer);
var
  I: Integer;
  TempNode: TBoldOCLLIstCoercion;
  LocalReSubscribe: Boolean;
begin
  for I := Start to Stop do
  begin

    if List[I] is TBoldOCLLIstCoercion then
    begin
      TempNode := TBoldOCLLIstCoercion(List[I]);
      List[I] := TempNode.Child;
      List[i].NeedsListCoercion := false;
      TempNode.Child := nil;
      TempNode.Free;
    end;

    List[I].ReSubscribe := ReSubscribe;
    list[i].SetReferenceValue(nil);

    LocalReSubscribe := ReSubscribe;
    try
      List[I].AcceptVisitor(self);
      ReSubscribe := LocalReSubscribe;
    finally
      if assigned(ReplacementNode) then
      begin
        List[I].Free;
        List[I] := ReplacementNode;
        ReplacementNode := nil;
      end;
    end;
  end;
end;

function TBoldOclSemanticsVisitor.FindSymbol(const name: string): TBoldOclSymbol;
begin
  result := Symboltable.SymbolByName[name];
end;

function TBoldOclSemanticsVisitor.GetHasReferencedVariables: boolean;
begin
  result := Assigned(fReferencedVariables) and (fReferencedVariables.Count > 0);
end;

function TBoldOclSemanticsVisitor.GetReferencedVariables: TList;
begin
  if not Assigned(fReferencedVariables) then
    fReferencedVariables := TList.Create;
  result := fReferencedVariables;
end;

procedure TBoldOclSemanticsVisitor.AddListCoercionOnArgs(N: TBoldOCLOperation);
var
  I: Integer;
  CoercionNode: TBoldOCLLIstCoercion;
begin
  for I := 0 to Length(N.Args) - 1 do
  begin
    if N.Args[I].NeedsListCoercion then
    begin
      CoercionNode := TBoldOCLLIstCoercion.Create;
      CoercionNode.Child := N.Args[I];
      CoercionNode.BoldType := CurrentSystemTypeInfo.ListTypeInfoByElement[N.Args[I].BoldType];
      N.Args[I] := CoercionNode;
    end;
  end;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclOperation(N: TBoldOCLOperation);

procedure FixImplicitCollect;
var
  NewOperation: TBoldOCLOperation;
  TempLIst: TBoldOclNodeList;
  OldNode0: TBoldOclNode;
  VarName: String;
  VarNode: TBoldOCLVariableReference;
  VarBind: TBoldOclVariableBinding;
  CollectNode: tBoldOclIteration;
  i: integer;
begin


  if n is TBoldOclMethod then
  begin
    NewOperation := tBoldOclMethod.Create;
    TBoldOclMethod(NewOperation).MethodOf := TBoldOclMethod(n).MethodOf;
    TBoldOclMethod(NewOperation).MethodOf_AddedToArgs := TBoldOclMethod(n).MethodOf_AddedToArgs;
  end
  else
    NewOperation := TBoldOclOperation.Create;

  OldNode0 := n.args[0];

  VarName := fEnv.MakeGenSymName;
  VarNode := TBoldOClVariableReference.Create;
  VarNode.VariableName := VarName;
  varNode.Position := n.Position;

  NewOperation.OperationName := n.OperationName;
  NewOperation.Position := n.Position;
  SetLength(NewOperation.Args, Length(n.args));
  NewOperation.Args[0] := VarNode;
  for i := 1 to Length(n.args) - 1 do
  begin
    NewOperation.args[i] := n.args[i];
  end;

  SetLength(n.Args, 0);
  CollectNode := TBoldOclIteration.Create;
  collectNode.IteratorSpecifier := OclCollect;
  CollectNode.OperationName := 'Collect';
  collectNode.Position := n.Position;
  SetLength(CollectNode.Args, 2);
  CollectNode.Args[0] := OldNode0;
  CollectNode.Args[1] := NewOperation;

  VarBind := TBoldOCLVariableBinding.Create;
  VarBind.VariableName := VarName;
  CollectNode.LoopVar := VarBind;

  SetLength(TempList, 1);
  TempList[0] := CollectNode;
  try
    TraverseList(TempList, 0, 0);
  finally
    ReplacementNode := TempList[0];
    SetLength(TempList, 0);
  end;
end;

var
  I: Integer;
  OldResubscribe: Boolean;
  Type1, Type2: TBoldElementTypeInfo;
begin
  if not n.IsMethod then
  begin
    N.Symbol := FindSymbol(N.OperationName);
    if not assigned(N.Symbol) then
      raise EBoldOCLAbort.CreateFmt(boeUndefinedOperation, [N.Position, N.OperationName]);
  end;

  N.IsConstant := True;
  try
    if SameText(N.OperationName, 'if') or
      SameText(N.OperationName, 'or') or
      SameText(N.OperationName, 'and') then
    begin
      OldResubscribe := Resubscribe;
      Resubscribe := true;
      TraverseList(N.Args, 0, 0);
      Resubscribe := OldResubscribe;
    end
    else
      TraverseList(N.Args, 0, 0);

    if (n.Args[0].BoldType is TBoldListTypeInfo) and
      not (n.Symbol.FormalArguments[0] is TBoldListTypeInfo) then
    begin
      FixImplicitCollect;
      exit;
    end;

    if Length(N.Args) <> N.Symbol.NumberOfArgs then
      raise EBoldOCLAbort.CreateFmt(boeWrongnumberofargs, [N.Position, N.Symbol.NumberOfArgs, Length(N.Args)]);

    TraverseList(N.Args, 1, Length(N.Args) - 1);

    for I := 0 to Length(N.Args) - 1 do
    begin
      N.IsConstant := N.IsConstant and N.Args[I].IsConstant;
      CheckArgumentType(N.Args[I], N.Symbol.FormalArguments[i]);
    end;
    AddListCoercionOnArgs(N);
    DeduceBoldType(N);
    if n.Symbol.ArgsNeedCommonType then
    begin
      Type1 := n.args[0].BoldType;
      if Type1 is TBoldListTypeINfo then
        type1 := (Type1 as TBoldListTypeINfo).ListElementTypeInfo;
      Type2 := n.args[1].BoldType;
      if Type2 is TBoldListTypeINfo then
        type2 := (Type2 as TBoldListTypeINfo).ListElementTypeInfo;

      if not Type2.ConformsTo(type1) and
        not Type1.ConformsTo(Type2) then
        raise EBoldOCLAbort.CreateFmt(sArgumentsDoNotConform, [n.Position, n.Symbol.SymbolName, Type1.AsString, type2.AsString]);
    end;
  except
    on e: Exception do
    begin
      n.IsConstant := false;
      raise;
    end;
  end;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclIteration(N: TBoldOclIteration);
var
  I: Integer;
  OldReSubscribe: Boolean;
begin
  N.Symbol := FindSymbol(N.OperationName);
  if not assigned(N.Symbol) then
    raise EBoldOCLAbort.CreateFmt(boeUndefinedOperation, [N.Position, N.OperationName]);

  if Length(N.Args) <> N.Symbol.NumberOfArgs then
    raise EBoldOCLAbort.CreateFmt(boeWrongnumberofargs, [N.Position, N.Symbol.NumberOfArgs, Length(N.Args)]);

  PushResubscribe(ReSubscribe, OldReSubscribe);

  TraverseList(N.Args, 0, 0);
  CheckArgumentType(N.Args[0], N.Symbol.FormalArguments[0]);
  N.IsConstant := N.Args[0].IsConstant;
  try
    ReSubscribe := OldReSubscribe;

    if not assigned(N.LoopVar) then
      N.LoopVar := TBoldOclVariableBinding.Create;

    if N.LoopVar.VariableName = '' then
      N.LoopVar.VariableName := fEnv.MakeGenSymName;
    AddListCoercionOnArgs(N);

    n.LoopVar.SetReferenceValue(nil);

    if not assigned(N.LoopVar.BoldType) then
      if n.args[0].BoldType is TBoldListTypeInfo then
        N.LoopVar.BoldType := TBoldListTypeInfo(N.Args[0].BoldType).ListElementTypeInfo
      else
        raise EBoldOCLInternalError.CreateFmt(boeUnKnownTypeOfLoopVar, [N.Position, N.Args[0].BoldType.ClassName]);
    fEnv.PushBinding(N.LoopVar);
    try
      TraverseList(N.Args, 1, Length(N.Args) - 1);
      for I := 1 to Length(N.Args) - 1 do
      begin
        N.IsConstant := N.IsConstant and N.Args[I].IsConstant;
        CheckArgumentType(N.Args[I], N.Symbol.FormalArguments[i]);
      end;
    finally
      fEnv.PopBinding;
    end;
    AddListCoercionOnArgs(N);
    DeduceBoldType(N);
  except
    on e: Exception do
    begin
      n.IsConstant := false;
      raise;
    end;
  end;
end;

function TBoldOclSemanticsVisitor.TestForOperation(n: TBoldOclNode): Boolean;
var
  TempOperation: TBoldOCLOperation;
  nAsMethod: TBoldOclMethod;
  i: integer;
  PossibleOperation: TBoldOclSymbol;
  OpName: String;
begin
  result := false;

  if n is TBoldOclMember then
    OpName := TBoldOclMember(n).MemberName
  else if n is TBoldOclMethod then
    OpName := TBoldOclMethod(n).OperationName
  else
    raise EBoldOclInternalError.CreateFmt('%d:Illegal call for TestForOperation', [n.Position]);

  PossibleOperation := FindSymbol(OpName);

  if Assigned(POssibleOperation) and PossibleOperation.IsDotNotation then
  begin
    TempOperation := TBoldOCLOperation.Create;
    TempOperation.Symbol := PossibleOperation;
    TempOperation.ReSubscribe := Resubscribe;
    TempOperation.OperationName := OpName;
    if n is TBoldOclMember then
    begin
      SetLength(TempOperation.Args, 1);
      TempOperation.args[0] := TBoldOclMember(n).Memberof;
      TBoldOclMember(n).Memberof := nil;
    end
    else if n is TBoldOclMethod then
    begin
      nAsMethod := TBoldOclMethod(n);
      SetLength(TempOperation.Args, Length(nAsMethod.args)+1);
      TempOperation.args[0] := nAsMethod.Methodof;
      for i := 0 to Length(nAsMethod.args) - 1 do
      begin
        TempOperation.args[i+1] := nAsMethod.args[i];
        nAsMethod.args[i] := nil;
      end;
      nAsMethod.Methodof := nil;
    end;
    TempOperation.Position := n.Position;
    try
      TempOperation.AcceptVisitor(self);
    finally
      if not assigned(ReplacementNode) then
        ReplacementNode := TempOperation
      else
        TempOperation.Free;
    end;
    Result := true;
  end;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclMethod(N: TBoldOclMethod);
var
  VarRef: TBoldOCLVariableReference;
  MethodRTInfo: TBoldMethodRTInfo;
  ClassTypeInfo: TBoldClassTypeInfo;
  I, L: Integer;
begin
  if not assigned(N.MethodOf) then
  begin
    VarRef := TBoldOCLVariableReference.Create;
    VarRef.VariableBinding := fEnv.CurrentImplicitVariable;
    VarRef.VariableName := VarRef.VariableBinding.VariableName;
    VarRef.BoldType := VarRef.VariableBinding.BoldType;
    VarRef.Position := N.Position;
    N.MethodOf := VarRef;
  end;

  if not testforOperation(n) then
  begin

    if not assigned(CurrentSystemTypeInfo) then
      raise EBoldOCLAbort.CreateFmt(boeExpressionNeedsContext, [N.Position]);

    Traverse(N.MethodOf);

    if not n.MethodOf_AddedToArgs then
    begin
      L := Length(n.Args);
      SetLength(n.Args, (L+1));
      for I := 0 to L-1 do
        n.Args[i+1] := n.Args[I];
      n.Args[0] := n.MethodOf;
      n.MethodOf_AddedToArgs := true;
    end;

    classTypeInfo := nil;

    if (n.MethodOf.BoldType is TBoldClassTypeInfo) then
      ClassTypeInfo := n.MethodOf.BoldType as TBoldClassTypeInfo
    else if (n.MethodOf.BoldType is TBoldListTypeInfo) and
            (TBoldListTypeInfo(n.MethodOf.BoldType).ListElementTypeInfo is TBoldClassTypeInfo) then
      ClassTypeInfo := TBoldListTypeInfo(n.MethodOf.BoldType).ListElementTypeInfo as TBoldClassTypeInfo;

    if not assigned(classTypeInfo) then
      raise EBoldOclInternalError.CreateFmt(sMethodNotoperatingOnClass, [N.Position, N.OperationName]);

    MethodRTInfo := ClassTypeInfo.Methods.ItemsByExpressionName[n.OperationName];
    if not assigned(MethodRTInfo) then
      raise EBoldOCLAbort.CreateFmt(boeUndefinedOperation, [N.Position, N.OperationName]);

    N.Symbol := nil;

    if not assigned(N.Symbol) then
      raise EBoldOCLAbort.CreateFmt(boeOperationNotOclable, [N.Position, N.OperationName]);

    n.IsMethod := true;

    VisitTBoldOclOperation(n);
  end;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclMember(N: TBoldOclMember);
var
  Binding: TBoldOclVariableBinding;
  ParentType: TBoldElementTypeInfo;
  VarRef: TBoldOCLVariableReference;
  MemberRTInfo: TBoldMemberRTInfo;
  RoleRTInfo: TBoldRoleRTInfo;
  ClassTypeInfo: TBoldClassTypeINfo;
  ElementType: TBoldElementTypeInfo;
  I: Integer;
begin
  ReSubscribe := True;
  if not assigned(CurrentSystemTypeInfo) then
    raise EBoldOCLAbort.CreateFmt(boeExpressionNeedsContext, [N.Position]);

  if not assigned(N.MemberOf) then
  begin
    Binding := fEnv.Lookup(N.Membername);
    if not assigned(Binding) then
    begin
      VarRef := TBoldOCLVariableReference.Create;
      VarRef.VariableBinding := fEnv.CurrentImplicitVariable;
      VarRef.VariableName := VarRef.VariableBinding.VariableName;
      VarRef.BoldType := VarRef.VariableBinding.BoldType;
      VarRef.Position := N.Position;
      if VarRef.VariableBinding.IsConstant then
        VarRef.IsConstant := VarRef.VariableBinding.IsConstant;
      N.MemberOf := VarRef;
    end
    else
    begin
      VarRef := TBoldOCLVariableReference.Create;
      Varref.VariableBinding := Binding;
      Varref.VariableName := N.Membername;
      VarRef.Position := N.Position;
      VarRef.BoldType := Binding.BoldType;
      if Binding.IsConstant then
        Varref.IsConstant := Binding.IsConstant;
      if not assigned(VarRef.BoldType) then
        raise EBoldOCLInternalError.CreateFmt(boeVariableNotAssigned, [N.Position, VarRef.VariableName]);
      ReplacementNode := VarRef;
      if not Varref.IsConstant and (UpperCase(Varref.VariableName) <> 'SELF') then
      begin
        ReferencedVariables.Add(VarRef);
      end;
      exit;
    end;
  end else
    Traverse(N.MemberOf);

  ParentType := N.MemberOf.BoldType;
  if ParentType is TBoldListTypeInfo then
    ElementType := (ParentType as TBoldListTypeInfo).ListElementTypeInfo
  else
    ElementType := ParentType;


  if not assigned(ElementType) then



    raise EBoldOCLAbort.CreateFmt(boeVariableNotAssigned, [N.Position, '']);

  case ElementType.BoldValueType of
    bvtSystem: begin
      if not testforOperation(n) then
      begin
        {-- Member of an RT-model --}
        ClassTypeInfo := CurrentSystemTypeInfo.ClassTypeInfoByExpressionName[N.Membername];
        if not assigned(ClassTypeInfo) then
          raise EBoldOCLAbort.CreateFmt(boeUnknownclass, [N.Position, N.Membername]);
        N.BoldType := CurrentSystemTypeInfo.ListTypeInfoByElement[ClassTypeInfo];
        n.MemberType := nil;
      end;
    end;
    bvtClass: begin
      {-- member of an RT-class --}
      ClassTypeInfo := TBoldClassTypeInfo(ElementType);

      n.MemberIndex := ClassTypeInfo.MemberIndexByExpressionName[n.MemberName];
      if n.MemberIndex <> -1 then
      begin
        n.RTInfo := ClassTypeInfo.AllMembers[n.MemberIndex];
        if n.RTInfo is TBoldRoleRTInfo then
        begin
          if not (n.RTInfo as TBoldRoleRTInfo).IsNavigable then
            n.RTInfo := nil;
        end;
      end else
        n.RTInfo := nil;

      if assigned(n.RTInfo) then
        n.MemberType := n.RTInfo.BoldType
      else
        n.MemberType := nil;


      if not assigned(N.MemberType) and not testForOperation(n) then
        raise EBoldOCLAbort.CreateFmt(boeUnknownMember, [N.Position, N.Membername, ElementType.ExpressionName]);

      if assigned(n.Qualifier) then
      begin
        MemberRTInfo := ClassTypeInfo.MemberRTInfoByExpressionName[n.Membername] as TBoldMemberRTInfo;
        if MemberRTInfo is TBoldRoleRTInfo then
          RoleRTInfo := MemberRTInfo as TBoldRoleRTInfo
        else
          roleRTInfo := nil;

        if not assigned(RoleRTInfo) then
          raise EBoldOclAbort.CreateFmt(sOnlyRolesCanBeQualified, [n.position]);
        if not RoleRTInfo.IsQualified then
          raise EBoldOclAbort.CreateFmt('%d: %s.%s is not a qualified role', [n.position, ClassTypeInfo.ExpressionName, RoleRTInfo.ExpressionName]);
        if Length(N.Qualifier) <> RoleRTInfo.Qualifiers.count then
          raise EBoldOCLAbort.CreateFmt(boeWrongnumberofargs, [N.Position, RoleRTInfo.Qualifiers.count, Length(N.Qualifier)]);

        if RoleRTInfo.IsQualifiedMulti or
          (assigned(n.MemberOf) and (n.MemberOf.BoldType is TBoldListTypeInfo)) then
        begin
          n.BoldType := n.MemberType
        end
        else
          n.BoldType := RoleRTInfo.ClassTypeInfoOfOtherEnd;

        TraverseList(N.qualifier, 0, Length(N.Qualifier) - 1);

        for i := 0 to RoleRTInfo.Qualifiers.count - 1 do
          CheckArgumentType(n.Qualifier[i], RoleRTInfo.Qualifiers[i].BoldType);
      end
      else if not(n.MemberType is TBoldListTypeInfo) and (ParentType is TBoldListTypeInfo) then
        n.BoldType := CurrentSystemTypeInfo.ListTypeInfoByElement[n.MemberType]
      else
        n.BoldType := n.MemberType;

    end;
    bvtAttr: begin
      {-- member of an Attribute --}
      if not testforOperation(n) then
        raise EBoldOCLAbort.CreateFmt(boeMemberOfAttr, [N.Position, ElementType.AsString, n.MemberName]);
    end;
    bvtType: begin
      {-- member of a Type --}
      if not testforOperation(n) then
        raise EBoldOCLAbort.CreateFmt(boeMemberOfType, [N.Position, ElementType.AsString, n.MemberName]);
    end;
  end;

  N.IsConstant := False;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclVariableReference(N: TBoldOCLVariableReference);
begin
  N.VariableBinding := fEnv.Lookup(N.VariableName);
  if not assigned(N.VariableBinding) then
    raise EBoldOCLInternalError.CreateFmt(boeUnknownVariable, [N.Position, N.VariableName]);

  N.BoldType := N.VariableBinding.BoldType;
  if not assigned(N.BoldType) then
    raise EBoldOCLAbort.CreateFmt(boeVariableNotAssigned, [N.Position, n.VariableName]); 
end;

procedure TBoldOCLSemanticsVisitor.VisitTBoldOCLTypeNode(n: TBoldOclTypeNode);
var
  currentType: TBoldElementTypeInfo;
  ClassTypeInfo: TBoldClassTypeInfo;
  ExpressionContext: TBoldElementTypeInfo;
  NewMember: TBoldOclNode;
begin
  n.BoldType := CurrentSystemTypeInfo.BoldType;
  CurrentType := CurrentSystemTypeInfo.ClassTypeInfoByExpressionName[n.TypeName];
  ExpressionContext := fEnv.CurrentImplicitVariable.BoldType;
  if not assigned(CurrentType) then
    CurrentType := CurrentSystemTypeInfo.AttributeTypeInfoByExpressionName[n.TypeName];
  if not assigned(CurrentType) or
    (not IgnoreNelCompatibility and BoldNelCompatibility and (ExpressionContext.BoldValueType in [bvtClass, bvtList])) then
  begin
    ClassTypeInfo := nil;
    if assigned(ExpressionContext) then
    begin
      if ExpressionContext.BoldValueType = bvtClass then
        classTypeInfo := ExpressionContext as TBoldClassTypeInfo;
      if (ExpressionContext.BoldValueType = bvtList) and
        ((ExpressionContext as TBoldLIstTypeInfo).ListElementTypeInfo.BoldValueType = bvtClass) then
        classTypeInfo := (ExpressionContext as TBoldLIstTypeInfo).ListElementTypeInfo as TBoldClassTypeInfo;

      if assigned(ClassTypeInfo) and
         Assigned(ClassTypeInfo.MemberRTInfoByExpressionName[n.TypeName]) then

        if BoldOclAllowCapitalMembers then
        begin
          NewMember := TBoldOclMember.Create;
          TBoldOclMember(NewMember).MemberName := n.TypeName;
          TBoldOclMember(NewMember).Position := n.Position;
          Traverse(NewMember);
          replacementNode := NewMember;
          exit;
        end
        else
          raise EBoldOCLAbort.CreateFmt(boeunknownTypebutMember, [N.Position, n.TypeName]);
    end;
    if not assigned(CurrentType) then
      raise EBoldOCLAbort.CreateFmt(boeunknownType, [N.Position]);
  end;

  n.SetReferenceValue(CurrentType);
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclStrLiteral(N: TBoldOclStrLiteral);
begin
  N.BoldType := TBoldOcl(fEvaluator).StringType;
  N.IsConstant := True;
end;

function TBoldOclSemanticsVisitor.LeastCommonSuperClass(C1, C2: TBoldClassTypeInfo): TBoldClassTypeInfo;
begin
  if (not assigned(C1)) or not assigned(C2) then Result := nil
  else if C1.BoldIsA(C2) then
    Result := C2
  else if C2.BoldIsA(C1) then
    Result := C1
  else
    Result := LeastCommonSuperClass(C1.SuperClassTypeInfo, C2);
end;

function TBoldOclSemanticsVisitor.LeastCommonSuperType(AttributeTypeInfo1, AttributeTypeInfo2: TBoldAttributetypeInfo): TBoldAttributeTypeInfo;
begin
  if (not assigned(AttributeTypeInfo1)) or not assigned(AttributeTypeInfo2) then
    Result := nil
  else if AttributeTypeInfo1.BoldIsA(AttributeTypeInfo2) then
    Result := AttributeTypeInfo2
  else if AttributeTypeInfo2.BoldIsA(AttributeTypeInfo1) then
    Result := AttributeTypeInfo1
  else if AttributeTypeInfo2.AttributeClass=AttributeTypeInfo1.AttributeClass then

    result := AttributeTypeInfo1
  else
    Result := LeastCommonSuperType(AttributeTypeInfo1.SuperAttributeTypeInfo, AttributeTypeInfo2);
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclCollectionLiteral(N: TBoldOclCollectionLiteral);
var
  I: Integer;
  TopType,
  IntType: TBoldAttributetypeInfo;
  TopClass: TBoldClasstypeInfo;
begin
  if N.IsRange then
  begin
    Traverse(N.rangeStart);
    Traverse(N.rangeStop);
    IntType := TBoldOcl(fEvaluator).IntegerType;
    if not N.rangeStart.BoldType.ConformsTo(IntType) then
      raise EBoldOCLAbort.CreateFmt(boeRangeMustBeInt, [N.rangeStart.Position]);
    if not N.rangeStop.BoldType.ConformsTo(IntType) then
      raise EBoldOCLAbort.CreateFmt(boeRangeMustBeInt, [N.rangeStop.Position]);
    N.BoldType := CurrentSystemTypeInfo.ListTypeInfoByElement[IntType];
    N.IsConstant := N.rangeStop.IsConstant and N.rangeStart.IsConstant;
  end
  else
  begin
    if Length(N.Elements) = 0 then
    begin
      N.BoldType := CurrentSystemTypeInfo.ListTypeInfoByElement[nil];
    end
    else
    begin
      TraverseList(N.Elements, 0, Length(N.Elements) - 1);
      N.IsConstant := N.Elements[0].IsConstant;
      if N.Elements[0].BoldType is TBoldClasstypeInfo then
      begin
        TopClass := N.Elements[0].BoldType as TBoldClassTypeInfo;
        for I := 1 to Length(N.Elements) - 1 do
        begin
          N.IsConstant := N.IsConstant and N.Elements[I].IsConstant;
          if not (N.Elements[I].BoldType is TBoldClassTypeInfo) then
            raise EBoldOCLAbort.CreateFmt(boeElementnotConformToCollection, [N.Elements[I].Position]);
          TopClass := LeastCommonSuperClass(TopClass, TBoldClassTypeInfo(N.Elements[I].BoldType));
        end;
        N.BoldType := CurrentSystemTypeInfo.ListTypeInfoByElement[TopClass];
      end
      else
      begin
        TopType := N.Elements[0].BoldType as TBoldAttributetypeInfo;
        for I := 1 to Length(N.Elements) - 1 do
        begin
          if not (N.Elements[I].BoldType is TBoldAttributeTypeInfo) then
            raise EBoldOCLAbort.CreateFmt(boeElementnotConformToCollection, [N.Elements[I].Position]);
          N.IsConstant := N.IsConstant and N.Elements[I].IsConstant;
          TopType := LeastCommonSuperType(TopType, TBoldAttributeTypeInfo(N.Elements[I].BoldType));
          if not assigned(TopType) then
            raise EBoldOCLAbort.CreateFmt(boeElementnotConformToCollection, [N.Elements[I].Position]);
        end;
        N.BoldType := CurrentSystemTypeInfo.ListtypeInfoByElement[TopType];
      end;
    end;
  end;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclNumericLiteral(N: TBoldOclNumericLiteral);
begin
  N.BoldType := TBoldOcl(fEvaluator).FloatType;
  N.IsConstant := True;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclEnumLiteral(N: TBoldOclEnumLiteral);
var
  vElement: TBoldElement;
  vTypeInfo: TBoldElementTypeInfo;
begin
  if not CurrentSystemTypeInfo.FindValueSetAndTypeByName(N.Name, vElement, vTypeInfo) then
    raise EBoldOCLAbort.CreateFmt(boeEnumValueNotFound, [N.Position, N.Name]);
  N.BoldType := vTypeInfo; //CurrentSystemTypeInfo.ValueSetTypeInfo;
  N.IsConstant := True;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclIntLiteral(N: TBoldOclIntLiteral);
begin
  N.BoldType := TBoldOcl(fEvaluator).IntegerType;
  N.IsConstant := True;
end;

procedure TBoldOclSemanticsVisitor.DeduceBoldType(N: TBoldOCLOperation);
var
  IsList: Boolean;
  argno1, argno2: Integer;
  arg1, arg2: TBoldElementTypeInfo;
  i: integer;
  ArgTypes: TStringList;
  ContextStr: String;
begin
  case N.Symbol.DeduceMethod of
    tbodNo: N.BoldType := n.Symbol.resultType;

    tbodCopyArg1: N.BoldType := N.Args[0].BoldType;
    tbodCopyArg2: N.BoldType := N.Args[1].BoldType;
    tbodCopyArg3: N.BoldType := N.Args[2].BoldType;
    tbodArg1AsList :
      if n.args[0].BoldType.BoldValueType = bvtList then
        N.BoldType := N.Args[0].BoldType
      else
        N.BoldType := CurrentSystemTypeInfo.ListTypeInfoByElement[N.Args[0].BoldType];




    tbodListofArg2: begin
      if N.Args[1].BoldType.BoldValueType = bvtList then
        N.BoldType := N.Args[1].BoldType
      else
        N.BoldType := CurrentSystemTypeInfo.ListtypeInfoByElement[N.Args[1].BoldType];
    end;

    tbodCopyArg1Elem: begin
      if not (N.Args[0].BoldType is TBoldListTypeInfo) then
        raise EBoldOCLAbort.CreateFmt(boeNonListArgumentToCopyArg1Elem, [N.Position]);
      N.BoldType := TBoldListTypeInfo(N.Args[0].BoldType).ListElementTypeInfo;
    end;

    tbodCopyLoopVar: begin
      if not (N is TBoldOclIteration) then
        raise EBoldOCLInternalError.CreateFmt(boeOperationWithLoopvar, [N.Position, N.Symbol.SymbolName]);
      N.BoldType := TBoldOclIteration(N).LoopVar.BoldType;
    end;

    tbodLCC, tbodLCC23: begin
      if N.Symbol.DeduceMethod = tbodLCC then argno1 := 0
      else argno1 := 1;
      argno2 := argno1 + 1;

      arg1 := N.Args[argno1].BoldType;
      arg2 := N.Args[argno2].BoldType;
      IsLIst := False;

      if arg1 is TBoldListTypeInfo then
      begin
        arg1 := TBoldListTypeInfo(arg1).ListElementTypeInfo;
        if arg2 is TBoldListTypeInfo then
          arg2 := TBoldListTypeInfo(arg2).ListElementTypeInfo
        else
          N.Args[argno2].NeedsListCoercion := True;
        IsLIst := True;
      end
      else if arg2 is TBoldListTypeInfo then
      begin
        N.Args[argno1].NeedsListCoercion := True;
        arg2 := TBoldListTypeInfo(arg2).ListElementTypeInfo;
        IsLIst := True;
      end;

      if (arg1 is TBoldClasstypeInfo) and (arg2 is TBoldClassTypeInfo) then
        N.BoldType := LeastCommonSuperClass(TBoldClassTypeInfo(arg1), TBoldClasstypeInfo(arg2))
      else if (arg1 is TBoldAttributeTypeInfo) and (arg2 is TBoldAttributeTypeInfo) then
        N.BoldType := LeastCommonSuperType(TBoldAttributeTypeInfo(arg1), TBoldAttributeTypeInfo(arg2))
      else if (arg1 is TBoldTypeTypeInfo) and (arg2 is TBoldTypeTypeInfo) then
        N.BoldType := arg1
      else
        N.BoldType := nil;

      if not assigned(N.BoldType) then
      begin
        if arg1 is TBoldNilTypeInfo then
          N.BoldType := arg2
        else
        if arg2 is TBoldNilTypeInfo then
          N.BoldType := arg1
      end;
      if not assigned(N.BoldType) then
        raise EBoldOCLAbort.CreateFmt(boeNoCommonSuperclass,
        [N.Position, arg1.ExpressionName, arg2.ExpressionName]);

      if IsLIst then
        N.BoldType := CurrentSystemTypeInfo.ListtypeInfoByElement[N.BoldType];
    end;
    tbodType: n.BoldType := N.Args[0].BoldType.BoldType;
    tbodTypeCast: n.BoldType  := n.Args[1].value as TBoldElementTypeInfo;
    tbodArg1Type: n.BoldType  := n.Args[0].value as TBoldElementTypeInfo;    
    tbodObjectList:
      if assigned(n.args[0].Value) then
        n.BoldType := CurrentSystemTypeInfo.ListTypeInfoByElement[n.args[0].Value as TBoldElementTypeInfo];
    tbodListFromArg2: n.BoldType := CurrentSystemTypeInfo.ListTypeInfoByElement[n.args[1].Value as TBoldElementTypeInfo];
  end;
  if not assigned(n.BoldType) then
  begin
    ArgTypes := TStringList.Create;
    for i := 0 to Length(n.args) - 1 do
      ArgTypes.Add(Format('Arg %s: %s', [IntToStr(i + 1), n.args[i].BoldType.AsString]));
    ContextStr := BoldSeparateStringList(ArgTypes, ', ', '', '');
    ArgTypes.Free;

    raise EBoldOclError.CreateFmt(boeunknownExprtypeinDeduce, [n.Position, n.OperationName, ContextStr, BoldOclDeduceMethodName[n.Symbol.Deducemethod]]);
  end;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclDateLiteral(N: TBoldOclDateLiteral);
begin
  N.BoldType := TBoldOcl(fEvaluator).DateType;
  N.IsConstant := True;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclTimeLiteral(N: TBoldOclTimeLiteral);
begin
  N.BoldType := TBoldOcl(fEvaluator).TimeType;
  N.IsConstant := True;
end;

end.
