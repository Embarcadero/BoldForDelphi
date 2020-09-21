unit BoldUMLOCLValidator;

interface

uses
  BoldModel,
  BoldOCL,
  BoldElements,
  BoldSystemRT,
  BoldUMLModel,
  BoldTypeNameDictionary,
  BoldUMLModelConverter,
  BoldUMLAttributes;

type
  { forward declarations }
  TOCLValidityChecker = class;

  { TOCLValidityChecker }
  TOCLValidityChecker = class
  private
    fUMLModel: TUMLModel;
    fTypeNameDictionary: TBoldTypeNameDictionary;
    fSystemTypeInfo: TBoldSystemTypeInfo;
    fOclEvaluator: TBoldOCL;
    function ExtractSystemTypeInfo(UMLMOdel: TUMLModel): TBoldSystemTypeInfo;
    function GetOCLEvaluator: TBoldOCL;
    function AttributeTypeInfoFromUMLAttribute(UMLAttribute: TUMLAttribute): TBoldAttributeTypeInfo;
    function ElementTypeInfoFromUMLAssociationEnd(UMLAssociationEnd: TUMLAssociationEnd): TBoldElementTypeInfo;
    procedure ClearViolations;
    procedure AddViolation(Severity: TSeverity; element: TUMLModelElement; Const Msg: string);
  protected
    procedure ValidateDerivationExpressions(UMLClass: TUMLClass; UMLTaggedValue: TUMLTaggedValue);
    procedure ValidateAttribute(UMLAttribute: TUMLAttribute);
    procedure ValidateAssociation(UMLAssociation: TUMLAssociation);
    procedure ValidateAssociationEnd(UMLAssociationEnd: TUMLAssociationEnd);
    procedure ValidateClass(UMLClass: TUMLClass);
    procedure ValidateConstraints(Element, Context: TUMLModelElement; ConstraintList: TUMLConstraintList);
    procedure ValidateExpression(const ScopeName: string; Element, Context: TUMLModelElement; const Expression: string; ElementTypeInfo: TBoldElementTypeInfo);
    property SystemTypeInfo: TBoldSystemTypeInfo read fSystemTypeInfo;
    property OCLEvaluator: TBoldOCL read GetOCLEvaluator;
  public
    constructor Create(aTypeNameDictionary: TBoldTypeNameDictionary);
    procedure ValidateModel(UMLModel: TUMLModel);
    property TypeNameDictionary: TBoldTypeNameDictionary read fTypeNameDictionary;
  end;

implementation

uses
  Classes,
  SysUtils,
  BoldUtils,
  BoldMeta,
  BoldLogHandler,
  BoldDefaultTaggedValues,
  BoldOCLClasses,
  BoldOCLError,
  BoldGuard;

{ TOCLValidityChecker }

constructor TOCLValidityChecker.Create(aTypeNameDictionary: TBoldTypeNameDictionary);
begin
  fTypeNameDictionary := aTypeNameDictionary;
end;

function TOCLValidityChecker.ExtractSystemTypeInfo(UMLModel: TUMLModel): TBoldSystemTypeInfo;
var
  MoldModel: TMoldModel;
begin
  MoldModel := TBoldModelConverter.UMLModelToMold(UMLModel);
  try
    Result := TBoldSystemTypeInfo.Create(MoldModel, false, false, fTypeNameDictionary);
  finally
    MoldModel.Free;
  end;
end;

function TOCLValidityChecker.GetOCLEvaluator: TBoldOCL;
begin
  if not assigned(fOclEvaluator) then
    fOclEvaluator := TBoldOcl.Create(SystemTypeInfo, nil);
  result := fOclEvaluator;
end;

procedure TOCLValidityChecker.ValidateAssociationEnd(UMLAssociationEnd: TUMLAssociationEnd);
begin
  // DerivationExpression
  if UMLAssociationEnd.GetBoldTV(TAG_DERIVATIONOCL) <> '' then
  begin
    if UMLAssociationEnd.Association.Derived then
      ValidateExpression('DerivationOCL', UMLAssociationEnd, UMLAssociationEnd.otherEnd.type_, UMLAssociationEnd.GetBoldTV(TAG_DERIVATIONOCL), ElementTypeInfoFromUMLAssociationEnd(UMLAssociationEnd))
    else
      AddViolation(sHint, UMLAssociationEnd, 'Derivation expression specified for non-derived association');
  end;
  // Constraints
  ValidateConstraints(UMLAssociationEnd, UMLAssociationEnd.type_, UMLAssociationEnd.constraint);
end;

procedure TOCLValidityChecker.ValidateAssociation(UMLAssociation: TUMLAssociation);
var
  Context: TUMLModelElement;
begin
  // Constraints
  if Assigned(UMLAssociation.class_) then
    Context := UMLAssociation.class_
  else
    Context := UMLAssociation;

  ValidateConstraints(UMLAssociation, Context, UMLAssociation.constraint);
end;


procedure TOCLValidityChecker.ValidateAttribute(UMLAttribute: TUMLAttribute);
begin
  // Derivation expression
  if UMLAttribute.GetBoldTV(TAG_DERIVATIONOCL) <> '' then
  begin
    if UMLAttribute.Derived then
      ValidateExpression('DerivationOCL', UMLAttribute, UMLAttribute.owner, UMLAttribute.GetBoldTV(TAG_DERIVATIONOCL), AttributeTypeInfoFromUMLAttribute(UMLAttribute))
    else
      AddViolation(sHint, UMLAttribute, 'Derivation expression specified for non-derived attribute');
  end;
  // Constraints
  ValidateConstraints(UMLAttribute, UMLAttribute.owner, UMLAttribute.constraint);
end;

procedure TOCLValidityChecker.ValidateClass(UMLClass: TUMLClass);
var
  i: integer;
begin
  // Default string representation
  if UMLClass.GetBoldTV(TAG_DEFAULTSTRINGREPRESENTATION) <> '' then
    ValidateExpression('DefaultStringRepresentation', UMLClass, UMLClass, UMLClass.GetBoldTV(TAG_DEFAULTSTRINGREPRESENTATION), OCLEvaluator.SymbolTable.Help.StringType);
  // Members
  for i := 0 to UMLClass.feature.Count - 1 do
  begin
    if UMLClass.Feature[i] is TUMLAttribute then
      ValidateAttribute(TUMLAttribute(UMLClass.Feature[i]));
  end;

  // AssociationEnds
  for i := 0 to UMLClass.associationEnd.Count - 1 do
    ValidateAssociationEnd(TUMLAssociationEnd(UMLClass.associationEnd[i]));

  // Constraints
  ValidateConstraints(UMLCLass, UMLClass, UMLClass.constraint);
  // Derivation expressions
  ValidateDerivationExpressions(UMLClass, UMLClass.taggedValue['Bold.DerivationExpressions']);
end;

procedure TOCLValidityChecker.ValidateConstraints(Element, Context: TUMLModelElement; ConstraintList: TUMLConstraintList);
var
  i: integer;
begin
  for i := 0 to ConstraintList.Count - 1 do
    ValidateExpression('Constraint', Element, Context, ConstraintList[i].body, OCLEvaluator.SymbolTable.Help.BooleanType);
end;

procedure TOCLValidityChecker.ValidateExpression(const ScopeName: string; Element, Context: TUMLModelElement; const Expression: string; ElementTypeInfo: TBoldElementTypeInfo);
var
  ErrorHeader,
  ResultTypeName: string;
  Result,
  ValidationContext: TBoldElementTypeInfo;
  Mapping: TBoldTypeNameMapping;
begin
  ErrorHeader := 'Invalid ' + ScopeName + ': ';
  ValidationContext := nil;
  if context is TUMLClass then
    ValidationContext := SystemTypeInfo.ClassTypeInfoByModelName[Context.Name]
  else if context is TUMLAttribute then
  begin
    Mapping := TypeNameDictionary.MappingForModelName[(Context as TUMLAttribute).typeName];
    if assigned(Mapping) then
      ValidationContext := SystemTypeInfo.AttributeTypeInfoByExpressionName[mapping.ExpressionName];
  end
  else
  begin
    AddViolation(sError, Element, ErrorHeader + Format('Unsupported context type (%s) for OCL-expression', [Context.ClassName]));
    Exit;
  end;

  Result := nil;
  try
    Result := OclEvaluator.ExpressionType(Expression, ValidationContext, true, nil);
  except
    on e: EBoldOclAbort do
      AddViolation(sError, Element, ErrorHeader + e.Message);
    on e: EBoldOclInternalError do
      AddViolation(sError, Element, ErrorHeader + 'Internal error - ' + e.Message);
    on e: EBoldOclError do
      AddViolation(sError, Element, ErrorHeader + 'Error - ' + e.Message);
    on e:Exception do
      AddViolation(sError, Element, ErrorHeader + 'Exception - ' + e.Message);
  end;

  if assigned(result) then
    ResultTypeName := Result.AsString
  else
    ResultTypeName := '(unknown)';

  if Assigned(ElementTypeInfo) and Assigned(Result) then
    if not Result.ConformsTo(ElementTypeInfo) then
      AddViolation(sError, Element, ErrorHeader + Format(' Type mismatch: Expected %s got %s', [ElementTypeInfo.ExpressionName, Result.ExpressionName]));
end;

procedure TOCLValidityChecker.ValidateModel(UMLModel: TUMLModel);
var
  i: integer;
begin
  fUMLModel := UMLModel;
  fSystemTypeInfo := ExtractSystemTypeInfo(UMLModel);
  BoldLog.StartLog('Validating OCL in model');
  ClearViolations;
  for i := 0 to UMLModel.Classes.Count - 1 do
    ValidateClass(UMLModel.Classes[i]);
  for i := 0 to UMLModel.associations.Count - 1 do
    ValidateAssociation(UMLModel.associations[i]);
  BoldLog.EndLog;
end;

procedure TOCLValidityChecker.ValidateDerivationExpressions(UMLClass: TUMLClass; UMLTaggedValue: TUMLTaggedValue);

function LocateModelElement(const FeatureName: string): TUMLModelElement;
var
  i: integer;
  tmpClass: TUMLClassifier;
begin
  Result := nil;
  for i := 0 to UMLClass.allFeature.Count - 1 do
  begin
    if SameText(UMLClass.allFeature[i].Name, FeatureName) then
    begin
      Result := UMLClass.allFeature[i];
      Exit;
    end;
  end;

  tmpClass := UMLClass;
  while Assigned(tmpClass) do
  begin
    for i := 0 to tmpClass.associationEnd.Count - 1 do
    begin
      if SameText(tmpClass.associationEnd[i].Name, FeatureName) then
      begin
        Result := tmpClass.associationEnd[i];
        Exit;
      end;
    end;
    tmpClass := tmpClass.superclass;
  end;
end;

var
  UMLAssociationEnd: TUMLAssociationEnd;
  UMLAttribute: TUMLAttribute;
  UMLModelElement: TUMLModelElement;
  StringList: TStringList;
  bg: IBoldGuard;
  i: integer;
begin
  if not Assigned(UMLTaggedValue) then Exit;
  bg := TBoldGuard.Create(StringList);
  StringList := TStringList.Create;
  StringList.Text := UMLTaggedValue.value;
  for i := 0 to StringList.Count - 1 do
  begin
    UMLModelElement := LocateModelElement(StringList.Names[i]);

    if UMLModelElement is TUMLAttribute then
    begin
      UMLAttribute := TUMLAttribute(UMLModelElement);
      if not UMLAttribute.Derived then
        AddViolation(sHint, UMLAttribute, 'Derivation expression respecified for non-derived attribute')
      else if UMLAttribute.GetBoldTV(TAG_DERIVATIONOCL) = '' then
        AddViolation(sHint, UMLAttribute, 'Derivation expression respecified for code-derived attribute')
      else
        ValidateExpression('DerivationExpression', UMLClass, UMLClass, StringList.Values[StringList.Names[i]], AttributeTypeInfoFromUMLAttribute(UMLAttribute))
    end;

    if UMLModelElement is TUMLAssociationEnd then
    begin
      UMLAssociationEnd := TUMLAssociationEnd(UMLModelElement);
      if not UMLAssociationEnd.association.derived then
        AddViolation(sHint, UMLAssociationEnd, 'Derivation expression respecified for non-derived association')
      else if UMLAssociationEnd.GetBoldTV(TAG_DERIVATIONOCL) = '' then
        AddViolation(sHint, UMLAssociationEnd, 'Derivation expression respecified for code-derived association end')
      else
        ValidateExpression('DerivationExpression', UMLClass, UMLClass, StringList.Values[StringList.Names[i]], ElementTypeInfoFromUMLAssociationEnd(UMLAssociationEnd));
    end;
  end;
end;

function TOCLValidityChecker.AttributeTypeInfoFromUMLAttribute(UMLAttribute: TUMLAttribute): TBoldAttributeTypeInfo;
var
  Mapping: TBoldTypeNameMapping;
begin
  Result := nil;
  Mapping := TypeNameDictionary.MappingForModelName[UMLAttribute.typeName];
  if Assigned(Mapping) then
    Result := SystemTypeInfo.AttributeTypeInfoByExpressionName[Mapping.ExpressionName];
end;

function TOCLValidityChecker.ElementTypeInfoFromUMLAssociationEnd(UMLAssociationEnd: TUMLAssociationEnd): TBoldElementTypeInfo;
var
  ClassTypeInfo: TBoldClassTypeinfo;
begin
  ClassTypeInfo := SystemTypeInfo.ClassTypeInfoByModelName[UMLAssociationEnd.type_.Name];
  if UMLAssociationEnd.multi then
    Result := SystemTypeInfo.ListTypeInfoByElement[ClassTypeInfo]
  else
    Result := ClassTypeInfo;
end;

procedure TOCLValidityChecker.AddViolation(Severity: TSeverity; element: TUMLModelElement; const Msg: string);
var
  Violation: TViolation;
begin
  Violation := fUMLModel.Validator.Violation.AddNew;
  Violation.Severity := Severity;
  Violation.ModelElement := Element;
  Violation.Description := Msg;
end;

procedure TOCLValidityChecker.ClearViolations;
var
  i: integer;
begin
  for i := fUMLModel.Validator.Violation.count - 1 downto 0 do
    fUMLModel.Validator.Violation[i].Delete
end;

end.
