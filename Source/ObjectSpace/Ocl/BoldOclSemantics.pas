unit BoldOclSemantics;

interface

uses
  Classes,
  BoldSystemRT,
  BoldElements,
  BoldOclClasses;

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
  public
    constructor Create(Model: TBoldSystemTypeInfo; Evaluator: TBoldEvaluator; SymTab: TBoldSymbolDictionary; Env: TBoldOclEnvironment);
    procedure AddListCoercionOnArgs(N: TBoldOCLOperation);
    procedure CheckArgumentType(actArg: TBoldOCLNode; FormArg: TBoldElementTypeInfo);
    procedure DeduceBoldType(N: TBoldOCLOperation);
    function FindSymbol(name: string): TBoldOclSymbol;
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
    'Same type as listelement of arg2', 'Type is ObjectList', 'Type is Metatype', 'Type is arg2 (typecast)', 'Arg1 as a list', 'a list with the type of Arg2');

constructor TBoldOclSemanticsVisitor.Create(Model: TBoldSystemTypeInfo; Evaluator: TBoldEvaluator; SymTab: TBoldSymbolDictionary; Env: TBoldOclEnvironment);
begin
  inherited Create;
  CurrentSystemTypeInfo := Model;
  fEnv := Env;
  fEvaluator := Evaluator;
  SymbolTable := SymTab;
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
      [actArg.Position, actArg.BoldType.ExpressionName, FormArg.ExpressionName]);
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
  // It is not certain the listcoercion will be needed in the current context
  // so we remove it here and then it might be reinserted again later.
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
//  Node.Evaluator := fEvaluator;
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
    // It is not certain the listcoercion will be needed in the current context
    // so we remove it here and then it might be reinserted again later.
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

function TBoldOclSemanticsVisitor.FindSymbol(name: string): TBoldOclSymbol;
begin
  result := Symboltable.SymbolByName[name];
end;

procedure TBoldOclSemanticsVisitor.AddListCoercionOnArgs(N: TBoldOCLOperation);
var
  I: Integer;
  CoercionNode: TBoldOCLLIstCoercion;
begin
  for I := 0 to N.Args.Count - 1 do
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

  // This is an implicit Collect. Change NodeType!

  // We really have to create a new node since the old one will be freed when replaced by the replacementNode!!!

  if n is TBoldOclMethod then
  begin
    NewOperation := tBoldOclMethod.Create;
    TBoldOclMethod(NewOperation).MethodOf := TBoldOclMethod(n).MethodOf;
    TBoldOclMethod(NewOperation).MethodOf_AddedToArgs := TBoldOclMethod(n).MethodOf_AddedToArgs;
  end
  else
    NewOperation := TBoldOclOperation.Create;

  OldNode0 := n.args[0];
  n.args.Delete(0);

  VarName := fEnv.MakeGenSymName;
  VarNode := TBoldOClVariableReference.Create;
  VarNode.VariableName := VarName;
  varNode.Position := n.Position;

  NewOperation.OperationName := n.OperationName;
  NewOperation.Position := n.Position;
  NewOperation.Args := TBoldOclNodeLIst.create;
  NewOperation.Args.add(VarNode);
  for i := 0 to n.args.count - 1 do
  begin
    NewOperation.args.Add(n.args[0]);
    n.Args.Delete(0);
  end;

  CollectNode := TBoldOclIteration.Create;
  collectNode.IteratorSpecifier := OclCollect;
  CollectNode.OperationName := 'Collect'; // do not localize
  collectNode.Position := n.Position;
  CollectNode.Args := TBoldOClNodeList.create;
  CollectNode.Args.Add(OldNode0);
  CollectNode.Args.Add(NewOperation);

  VarBind := TBoldOCLVariableBinding.Create;
  VarBind.VariableName := VarName;
  CollectNode.LoopVar := VarBind;

  // Now Traverse the node and make sure we put the correct node in the replacementNode-variable

  tempList := TBoldOclNodeList.create;
  TempLIst.Add(CollectNode);
  try
    TraverseList(tempLIst, 0, 0);
  finally
    // even if things fail, it is better to have the new node than no node at all.
    ReplacementNode := TempLIst[0];
    TempLIst[0] := nil;
    TempList.Free;
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
    // boolean shortcut must force resubscribe on the shortcutting node
    if SameText(N.OperationName, 'if') or // do not localize
      SameText(N.OperationName, 'or') or // do not localize
      SameText(N.OperationName, 'and') then // do not localize
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

    if N.Args.Count <> N.Symbol.NumberOfArgs then
      raise EBoldOCLAbort.CreateFmt(boeWrongnumberofargs, [N.Position, N.Symbol.NumberOfArgs, N.Args.Count]);

    TraverseList(N.Args, 1, N.Args.Count - 1);

    for I := 0 to N.Args.Count - 1 do
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

  if N.Args.Count <> N.Symbol.NumberOfArgs then
    raise EBoldOCLAbort.CreateFmt(boeWrongnumberofargs, [N.Position, N.Symbol.NumberOfArgs, N.Args.Count]);

  PushResubscribe(ReSubscribe, OldReSubscribe);

  TraverseList(N.Args, 0, 0);
  CheckArgumentType(N.Args[0], N.Symbol.FormalArguments[0]);
  N.IsConstant := N.Args[0].IsConstant;
  try
    ReSubscribe := OldReSubscribe;

    if not assigned(N.LoopVar) then
      //  CHECKME N.LoopVar := TBoldOclVariableBinding.Create(n);
      N.LoopVar := TBoldOclVariableBinding.Create;

    if N.LoopVar.VariableName = '' then
      N.LoopVar.VariableName := fEnv.MakeGenSymName;

    // we must coerce the first arguement before using its type
    AddListCoercionOnArgs(N);

    n.LoopVar.SetReferenceValue(nil);  // reset dynamic boldtype

    if not assigned(N.LoopVar.BoldType) then
      if n.args[0].BoldType is TBoldListTypeInfo then
        N.LoopVar.BoldType := TBoldListTypeInfo(N.Args[0].BoldType).ListElementTypeInfo
      else
        raise EBoldOCLInternalError.CreateFmt(boeUnKnownTypeOfLoopVar, [N.Position, N.Args[0].BoldType.ClassName]);
    fEnv.PushBinding(N.LoopVar);
    try
      TraverseList(N.Args, 1, N.Args.Count - 1);
      for I := 1 to N.Args.Count - 1 do
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
    raise EBoldOclInternalError.CreateFmt('%d:Illegal call for TestForOperation', [n.Position]); // do not localize

  PossibleOperation := FindSymbol(OpName);

  if Assigned(POssibleOperation) and PossibleOperation.IsDotNotation then
  begin
    TempOperation := TBoldOCLOperation.Create;
    TempOperation.Symbol := PossibleOperation;
    TempOperation.ReSubscribe := Resubscribe;
    TempOperation.OperationName := OpName;
    TempOperation.Args := TBoldOCLNodeList.Create;
    if n is TBoldOclMember then
    begin
      TempOperation.args.add(TBoldOclMember(n).Memberof);
      TBoldOclMember(n).Memberof := nil;
    end
    else if n is TBoldOclMethod then
    begin
      TempOperation.args.add(TBoldOclMethod(n).Methodof);
      for i := 0 to TBoldOclMethod(n).args.count - 1 do
      begin
        TempOperation.args.add(TBoldOclMethod(n).args[i]);
        TBoldOclMethod(n).args[i] := nil;
      end;
      TBoldOclMethod(n).Methodof := nil;
    end;
    TempOperation.Position := n.Position;
    try
      TempOperation.AcceptVisitor(self); // HAs to traverse the whole subtree again :-(
    finally
      //Now, publish the operation to be exchanged to.
      if not assigned(ReplacementNode) then
        ReplacementNode := TempOperation
      else
        // this occurs if the operation was also a part of an implicit collect such as
        // Person.allInstances.constraints, in that case, the replacementnode will
        // refer to a copy of the tempoperation, and the copy will have stolen the
        // arguments already (see FixImplicitCollect)
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
      n.Args.Insert(0, n.MethodOf);
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
//    N.Symbol := MethodRTInfo.OclSymbol as TBoldOclSymbol;

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
      N.MemberOf := VarRef;
    end
    else
    begin
      // The Current node will be replaced with a var-reference inside Traverse
      VarRef := TBoldOCLVariableReference.Create;
      Varref.VariableBinding := Binding;
      Varref.VariableName := N.Membername;
      VarRef.Position := N.Position;
      VarRef.BoldType := Binding.BoldType;
      if not assigned(VarRef.BoldType) then
        raise EBoldOCLInternalError.CreateFmt(boeVariableNotAssigned, [N.Position, VarRef.VariableName]);
      ReplacementNode := VarRef;
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
    // this is most likely due to absence of a context. Current member is assumed to have
    // an implicit Self-var-reference, but it has no context.
    // It also occurs when trying to evaluate an empty single-link and is then taken care of by
    // the TBoldOCL.CheckSemantics
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
        n.MemberType := nil; // needed if the expression has been evaluated in another context previously
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
          raise EBoldOclAbort.CreateFmt(sXIsNotAQualifiedRole, [n.position, ClassTypeInfo.ExpressionName, RoleRTInfo.ExpressionName]);
        if N.Qualifier.Count <> RoleRTInfo.Qualifiers.count then
          raise EBoldOCLAbort.CreateFmt(boeWrongnumberofargs, [N.Position, RoleRTInfo.Qualifiers.count, N.Qualifier.Count]);

        if RoleRTInfo.IsQualifiedMulti or
          // qualified access starting from an objectlist
          (assigned(n.MemberOf) and (n.MemberOf.BoldType is TBoldListTypeInfo)) then
        begin
//          raise EBoldOclAbort.CreateFmt('%d: %s.%s - Qualified relations with multiplicity > 1 not supported', [n.position, ClassTypeInfo.ExpressionName, RoleRTInfo.ExpressionName]);
          n.BoldType := n.MemberType
        end
        else
          n.BoldType := RoleRTInfo.ClassTypeInfoOfOtherEnd;

        TraverseList(N.qualifier, 0, N.Qualifier.Count - 1);

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

procedure TBoldOclSemanticsVisitor.VisitTBoldOclVariablereference(N: TBoldOCLVariableReference);
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
  N.BoldType := CurrentSystemTypeInfo.AttributeTypeInfoByExpressionName['String']; // do not localize
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
    // attributes with the same delphitype will conform to each other, but they will not support BoldIsA
    // either attribute will do as good as result from here
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
    IntType := CurrentSystemTypeInfo.AttributeTypeInfoByExpressionName['Integer']; // do not localize
    if not N.rangeStart.BoldType.ConformsTo(IntType) then
      raise EBoldOCLAbort.CreateFmt(boeRangeMustBeInt, [N.rangeStart.Position]);
    if not N.rangeStop.BoldType.ConformsTo(IntType) then
      raise EBoldOCLAbort.CreateFmt(boeRangeMustBeInt, [N.rangeStop.Position]);
    N.BoldType := CurrentSystemTypeInfo.ListTypeInfoByElement[IntType];
    N.IsConstant := N.rangeStop.IsConstant and N.rangeStart.IsConstant;
  end
  else
  begin
    if N.Elements.Count = 0 then
    begin
      N.BoldType := CurrentSystemTypeInfo.ListTypeInfoByElement[nil];
    end
    else
    begin
      TraverseList(N.Elements, 0, N.Elements.Count - 1);
      N.IsConstant := N.Elements[0].IsConstant;
      if N.Elements[0].BoldType is TBoldClasstypeInfo then
      begin
        TopClass := N.Elements[0].BoldType as TBoldClassTypeInfo;
        for I := 1 to N.Elements.Count - 1 do
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
        for I := 1 to N.Elements.Count - 1 do
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
  //  n.IsConstant := False; // Needed as long as values are transfered out of the OCL-nodes
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclNumericLiteral(N: TBoldOclNumericLiteral);
begin
  N.BoldType := CurrentSystemTypeInfo.AttributeTypeInfoByExpressionName['Float']; // do not localize
  N.IsConstant := True;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclEnumLiteral(N: TBoldOclEnumLiteral);
begin
  N.BoldType := CurrentSystemTypeInfo.AttributeTypeInfoByExpressionName['ValueSet']; // do not localize
  N.IsConstant := True;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclIntLiteral(N: TBoldOclIntLiteral);
begin
  N.BoldType := CurrentSystemTypeInfo.AttributeTypeInfoByExpressionName['Integer']; // do not localize
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

//    tbodBoolean: N.BoldType := CurrentModel.AttributeTypeByExpressionName['Boolean'];
//    tbodInteger: N.BoldType := CurrentModel.AttributeTypeByExpressionName['Integer'];
//    tbodreal: N.BoldType := CurrentModel.AttributeTypeByExpressionName['Float'];
//    tbodString: N.BoldType := CurrentModel.AttributeTypeByExpressionName['String'];

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
        raise EBoldOCLAbort.CreateFmt(boeNoCommonSuperclass,
        [N.Position, arg1.ExpressionName, arg2.ExpressionName]);

      if IsLIst then
        N.BoldType := CurrentSystemTypeInfo.ListtypeInfoByElement[N.BoldType];
    end;
    tbodType: n.BoldType := N.Args[0].BoldType.BoldType;
    tbodTypeCast: n.BoldType  := n.Args[1].value as TBoldElementTypeInfo;
    tbodObjectList:
      if assigned(n.args[0].Value) then
        n.BoldType := CurrentSystemTypeInfo.ListTypeInfoByElement[n.args[0].Value as TBoldElementTypeInfo];
    tbodListFromArg2: n.BoldType := CurrentSystemTypeInfo.ListTypeInfoByElement[n.args[1].Value as TBoldElementTypeInfo];
  end;
  if not assigned(n.BoldType) then
  begin
    ArgTypes := TStringList.Create;
    for i := 0 to n.args.count - 1 do
      ArgTypes.Add(Format('Arg %s: %s', [IntToStr(i + 1), n.args[i].BoldType.AsString])); // do not localize
    ContextStr := BoldSeparateStringList(ArgTypes, ', ', '', '');
    ArgTypes.Free;

    raise EBoldOclError.CreateFmt(boeunknownExprtypeinDeduce, [n.Position, n.OperationName, ContextStr, BoldOclDeduceMethodName[n.Symbol.Deducemethod]]);
  end;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclDateLiteral(N: TBoldOclDateLiteral);
begin
  N.BoldType := CurrentSystemTypeInfo.AttributeTypeInfoByExpressionName['Date']; // do not localize
  N.IsConstant := True;
end;

procedure TBoldOclSemanticsVisitor.VisitTBoldOclTimeLiteral(N: TBoldOclTimeLiteral);
begin
  N.BoldType := CurrentSystemTypeInfo.AttributeTypeInfoByExpressionName['Time']; // do not localize
  N.IsConstant := True;
end;

end.
