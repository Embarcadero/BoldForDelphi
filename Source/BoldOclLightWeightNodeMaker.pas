{ Global compiler directives }
{$include bold.inc}
unit BoldOclLightWeightNodeMaker;

interface
uses
  Classes,
  BoldSystem,
  BoldSystemRT,
  BoldOclLightWeightNodes,
  BoldOclClasses;

type
  { forward declaration of classes }
  TBoldOLWNodeMaker = class;

  { TBoldOLWNodeMaker }
  TBoldOLWNodeMaker = class(TBoldOclVisitor)
  private
    fFailed: Boolean;
    fFailurePosition: Integer;
    fFailureReason: String;
    fRootNode: TBoldOLWNode;
    fOLWVarBindings: TList;
    fVarBindings: TList;
    fExternalVarBindings: TList;
    fOclRootNode: TBoldOclNode;
    fEnv: TBoldOclEnvironment;
  protected
    property OLWVarBindings: TList read fOLWVarBindings;
    property VarBindings: TList read fVarBindings;
    procedure SetFailure(Position: integer; const Message: String);
  public
    constructor create(OclRootNode: TBoldOclNode; SystemTypeInfo: TBoldSystemTypeInfo; System: TBoldSystem; Env:TBoldOclEnvironment);
    destructor Destroy; override;
    procedure VisitTBoldOclNode(N: TBoldOclNode); override;
    procedure VisitTBoldOclListCoercion(N: TBoldOclListCoercion); override;
    procedure VisitTBoldOclCollectionLiteral(N: TBoldOclCollectionLIteral); override;
    procedure VisitTBoldOclOperation(N: TBoldOclOperation); override;
    procedure VisitTBoldOclIteration(N: TBoldOclIteration); override;
    procedure VisitTBoldOclMember(N: TBoldOclMember); override;
    procedure VisitTBoldOclMethod(N: TBoldOclMethod); override;
    procedure VisitTBoldOclLiteral(N: TBoldOclLiteral); override;
    procedure VisitTBoldOclStrLiteral(N: TBoldOclStrLiteral); override;
    procedure VisitTBoldOclNumericLiteral(N: TBoldOclNumericLiteral); override;
    procedure VisitTBoldOclEnumLiteral(N: TBoldOclEnumLiteral); override;
    procedure VisitTBoldOclDateLiteral(N: TBoldOclDateLiteral); override;
    procedure VisitTBoldOclTimeLiteral(N: TBoldOclTimeLiteral); override;
    procedure VisitTBoldOclIntLiteral(N: TBoldOclIntLiteral); override;
    procedure VisitTBoldOclVariableBinding(N: TBoldOclVariableBinding); override;
    procedure VisitTBoldOclVariableReference(N: TBoldOclVariableReference); override;
    procedure VisitTBoldOclTypeNode(N: TBoldOclTypeNode); override;
    function OLWBindingForVarBinding(VarBinding: TBoldOclVariableBinding): TBoldOLWVariableBinding;
    property RootNode: TBoldOLWNode read fRootNode;
    property Failed: Boolean read fFailed;
    property FailureReason: String read fFailureReason;
    property FailurePosition: Integer read fFailurePosition;
    property ExternalVarBindings: TList read fExternalvarBindings;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldElements,
  BoldAttributes,
  BoldOcl,
  BoldBase;

{ TBoldOLWNodeMaker }

constructor TBoldOLWNodeMaker.create(OclRootNode: TBoldOclNode; SystemTypeInfo: TBoldSystemTypeInfo; System: TBoldSystem; Env: TBoldOclEnvironment);
begin
  inherited Create;
  fOLWVarBindings := TList.Create;
  fVarBindings := TList.Create;
  fOclRootNode := OclRootNode;
  fExternalVarBindings := TList.Create;
  fEnv := Env;
  if not (OclRootNode.BoldType is TBoldListTypeInfo) or
     not OclRootNode.BoldType.ConformsTo(SystemTypeInfo.RootClassTypeInfo.ListTypeInfo) then
    begin
      if not (OclRootNode.BoldType is TBoldClassTypeInfo) or not OclRootNode.BoldType.ConformsTo(SystemTypeInfo.RootClassTypeInfo) then
        SetFailure(0, format(sPSResultMustBeObjectList, [OclRootNode.BoldType.AsString]));
    end;
end;

destructor TBoldOLWNodeMaker.destroy;
var
  i: integer;
begin
  FreeAndNil(fVarBindings);
  FreeAndNil(fOLWVarbindings);

  // this list will not be empty if there was a failure
  // otherwise it will be sent to the environment of the condition
  for i := 0 to fExternalVarBindings.Count-1 do
    TObject(fExternalvarBindings[i]).Free;
  FreeAndNil(fExternalvarBindings);

  inherited;
end;

procedure TBoldOLWNodeMaker.SetFailure(Position: integer; const Message: String);
begin
  fFailed := true;
  fFailureReason := Message;
  fFailurePosition := Position;
  fOclRootNode := nil;
end;

function TBoldOLWNodeMaker.OLWBindingForVarBinding(
  VarBinding: TBoldOclVariableBinding): TBoldOLWVariableBinding;
var
  p: integer;
begin
  p := fVarBindings.IndexOf(VarBinding);
  if p <> -1 then
    result := TBoldOLWVariableBinding(fOLWVarBindings[p])
  else
    result := nil;
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclCollectionLIteral(N: TBoldOclCollectionLIteral);
begin
  SetFailure(n.Position, sCollectionLiteralsNotSupported);
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclEnumLiteral(N: TBoldOclEnumLiteral);
begin
  fRootNode := TBoldOLWEnumLiteral.create(n.Position, n.name);
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclIntLiteral(N: TBoldOclIntLiteral);
begin
  fRootNode := TBoldOLWIntLiteral.Create(n.Position, n.IntValue);
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclIteration(N: TBoldOclIteration);
var
  OLWIteration: TBoldOLWIteration;
  i: integer;
begin
  n.LoopVar.AcceptVisitor(self);
  OLWIteration := TBoldOLWIteration.create(n.Position,
                                           n.OperationName,
                                           RootNode as TBoldOLWVariableBinding);
  for i := 0 to Length(n.Args)-1 do
  begin
    n.Args[i].AcceptVisitor(self);
    OLWIteration.Args.Add(RootNode);
  end;
  fRootNode := OLWIteration;
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclListCoercion(N: TBoldOclListCoercion);
begin
  n.Child.AcceptVisitor(self);
  fRootNode := TBoldOLWListCoercion.Create(n.Position, RootNode);
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclLiteral(N: TBoldOclLiteral);
begin
  // Abstract class
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclMember(N: TBoldOclMember);
var
  OLWMember: TBoldOLWMember;
  i: integer;
  IsBoolean: Boolean;
  MainRole, RoleRTInfo: TBoldRoleRTinfo;
  EffectivePersistent: Boolean;
begin
  inherited;
  n.MemberOf.AcceptVisitor(self);
  IsBoolean := n.BoldType.ConformsTo(TBoldOCL(n.BoldType.evaluator).BooleanType);
  OLWMember := TBoldOLWMember.Create(n.Position, n.MemberName, n.MemberIndex, RootNode, IsBoolean);

  if not n.RTInfo.Persistent then
  begin
    EffectivePersistent := false;
    if (n.RTInfo is TBoldRoleRTInfo) then
    begin
      RoleRTInfo := n.RTInfo as TBoldRoleRTinfo;
      if (RoleRTinfo.RoleType = rtLinkRole) then
      begin
        MainRole := RoleRTInfo.ClassTypeInfo.AllMembers[RoleRTInfo.IndexOfMainRole] as TBoldRoleRTInfo;
        EffectivePersistent := MainRole.Persistent;
      end;
    end;
    if not EffectivePersistent then
      SetFailure(n.Position, Format(sTransientMembersCannoteUsed, [n.RTInfo.ExpressionName]));
  end;

  if assigned(n.Qualifier) then
    for i := 0 to Length(n.Qualifier)-1 do
    begin
      n.Qualifier[i].AcceptVisitor(self);
      OLWMember.Qualifier.Add(RootNode);
    end;
  fRootNode := OLWMember;
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclMethod(N: TBoldOclMethod);
begin
  SetFailure(n.Position, sMethodsNotSupported);
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclNode(N: TBoldOclNode);
begin
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclNumericLiteral(N: TBoldOclNumericLiteral);
begin
  if n.ClassType = TBoldOclNumericLiteral then
    fRootNode := TBoldOLWFloatLiteral.Create(n.Position, n.FloatValue);
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclOperation(N: TBoldOclOperation);
var
  i: integer;
  OLWOperation: TBoldOLWOperation;
  LiteralName: String;
  Literal: TBoldOLWEnumLiteral;
  LiteralType: TBoldElementTypeInfo;
  BooleanType: TBoldAttributeTypeInfo;
  ValueSet: TBAValueSet;
  ValueSetValue: TBAValueSetValue;
begin
  if n.ClassType = TBoldOclOperation then
  begin
    OLWOperation := TBoldOLWOperation.create(n.Position, n.OperationName);
    for i := 0 to Length(n.Args)-1 do
    begin
      n.Args[i].AcceptVisitor(self);
      OLWOperation.Args.Add(RootNode);
    end;
    fRootNode := OLWOperation;
    if (n.OperationName = '=') or (n.OperationName = '<>') then
    begin
      // if we find an enum literal (#preliminary), we must try to convert it to an integer
      Literal := nil;
      LiteralName := '';
      LiteralType := nil;
      if (n.Args[0] is TBoldOclEnumLiteral) and
         not (n.Args[1] is TBoldOclEnumLiteral) then
      begin
        LiteralName := (n.Args[0] as TBoldOclEnumLiteral).name;
        LiteralType := n.Args[1].BoldType;
        Literal := OLWOperation.args[0] as TBoldOLWEnumLiteral;
      end;

      if (n.Args[1] is TBoldOclEnumLiteral) and
         not (n.Args[0] is TBoldOclEnumLiteral) then
      begin
        LiteralName := (n.Args[1] as TBoldOclEnumLiteral).name;
        LiteralType := n.Args[0].BoldType;
        Literal := OLWOperation.args[1] as TBoldOLWEnumLiteral;
      end;

      BooleanType := TBoldOCL(n.BoldType.evaluator).BooleanType;

      if n.args[0].BoldType.ConformsTo(BooleanType) and
         n.args[1].BoldType.ConformsTo(BooleanType) then
        SetFailure(n.Position, sUseBooleanOperations);

      if assigned(Literal) then
      begin
        if LiteralType.ConformsTo(BooleanType) then
          SetFailure(n.Position, sUseBooleanOperationsWithLiterals);

        if LiteralType.ConformsTo((LiteralType.SystemTypeInfo as TBoldSystemTypeInfo).ValueSetTypeInfo) then
        begin
          ValueSet := TBoldMemberFactory.CreateMemberFromBoldType(LiteralType) as TBAValueSet;
          try
            // Do not compare default representation on first place, because it is not
            // well suited for a comparison. Instead first compare short representation,
            // which should be used to represent the enum value itself, instead of a translation.
            ValueSetValue := ValueSet.Values.FindByString(brShort, LiteralName);
            if ValueSetValue = nil then begin
              ValueSetValue := ValueSet.Values.FindByString(brDefault, LiteralName);
            end;
            if Assigned(ValueSetValue) then begin
              Literal.IntValue := ValueSetValue.AsInteger;
            end 
            else 
            begin
              SetFailure(n.Position, format(sEnumNameNotValid, [LiteralName, LiteralType.ExpressionName]));
            end;
          finally
            ValueSet.Free;
          end;
        end
        else
          SetFailure(n.Position, format(sEnumComparedToNonEnum, [LiteralName, LiteralType.ExpressionName]));
      end;
    end;
  end;
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclStrLiteral(N: TBoldOclStrLiteral);
begin
  inherited;
  fRootNode := TBoldOLWStrLiteral.Create(n.Position, n.StrValue);
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclTypeNode(N: TBoldOclTypeNode);
begin
  if n.Value is TBoldClassTypeInfo then
    fRootNode := TBoldOLWTypeNode.Create(n.Position, N.typeName, (n.Value as TBoldClassTypeInfo).TopSortedIndex)
  else
    SetFailure(n.Position, Format(sTypeNotSupported, [n.TypeName]));
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclVariableBinding(N: TBoldOclVariableBinding);
var
  TopSortedIndex: integer;
  VarBind: TBoldOLWVariableBinding;
  Obj: TBoldObject;
begin
  if not assigned(n.Value) and (n.BoldType is TBoldAttributeTypeInfo) then
  begin
    VarBind := TBoldOLWVariableBinding.Create(N.Position, n.VariableName, -1);
    Varbindings.Add(n);
    OLWvarBindings.Add(VarBind);
    fRootNode := VarBind;
  end
  else
  if assigned(n.Value) or (n.BoldType is TBoldClassTypeInfo) then
  begin
    TopSortedIndex := -1;
    if n.BoldType is TBoldClasstypeInfo then
      TopSortedIndex := (n.BoldType as TBoldClassTypeInfo).TopSortedIndex;

    if (n.BoldType is TBoldListTypeInfo) and
       ((n.BoldType as TBoldListTypeInfo).ListElementTypeInfo is TBoldClassTypeInfo) then
      TopSortedIndex := ((n.BoldType as TBoldListTypeInfo).ListElementTypeInfo as TBoldClassTypeInfo).TopSortedIndex;

    VarBind := TBoldOLWVariableBinding.Create(N.Position, n.VariableName, TopSortedIndex);

    if n.BoldType is TBoldClassTypeInfo then
    begin
      if n.value is TBoldObjectReference then
        Obj := (n.value as TBoldObjectReference).BoldObject
      else if n.value is TBoldObject then
        Obj := (n.value as TBoldObject)
      else
        Obj := nil;
      if assigned(obj) and Obj.BoldObjectLocator.BoldObjectId.IsStorable then
        VarBind.ExternalVarvalue := StrToIntDef(Obj.BoldObjectLocator.BoldObjectId.AsString, -1)
      else
        VarBind.ExternalVarvalue := -1;
    end
    else if n.Value is TBoldAttribute then
      VarBind.ExternalVarvalue := n.Value.AsVariant;

    Varbindings.Add(n);
    OLWvarBindings.Add(VarBind);

    fRootNode := VarBind;
  end
  else
  begin
    fRootNode := TBoldOLWVariableBinding.Create(N.Position, n.VariableName, -1);
    setFailure(n.Position, Format(sLoopVariablesMustBeClassType, [n.BoldType.AsString]));
  end;
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclVariableReference(N: TBoldOclVariableReference);
var
  VarBind: TBoldOLWVariableBinding;
begin
  VarBind := OLWBindingForVarBinding(n.VariableBinding);

  if not assigned(VarBind) then
  begin
    n.VariableBinding.AcceptVisitor(self);
    VarBind := RootNode as TBoldOLWVariableBinding;
    ExternalVarBindings.Add(VarBind);
  end;

  fRootNode := TBoldOLWVariableReference.create(n.Position, VarBind)
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclDateLiteral(
  N: TBoldOclDateLiteral);
begin
  fRootNode := TBoldOLWDateLiteral.Create(n.Position, n.DateValue);
end;

procedure TBoldOLWNodeMaker.VisitTBoldOclTimeLiteral(
  N: TBoldOclTimeLiteral);
begin
  fRootNode := TBoldOLWTimeLiteral.Create(n.Position, n.TimeValue);
end;

end.
