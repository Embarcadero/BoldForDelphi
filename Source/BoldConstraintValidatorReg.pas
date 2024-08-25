{ Global compiler directives }
{$include bold.inc}
unit BoldConstraintValidatorReg;

interface

procedure Register;

implementation

uses
  Classes,
  DesignIntf,

  BoldConstraintValidator,
  BoldCoreConsts,
  BoldDefs,
  BoldElements,
  BoldSystemRt,
  BoldPropertyEditors,
  BoldIDEConsts;

type
  { TBoldTypeNameSelectorForConstraintValidator }
  TBoldTypeNameSelectorForConstraintValidator = class (TBoldTypeNameSelectorForHandles)
  protected
    function GetApprovedTypes: TBoldValueTypes; override;
    function GetContextType(Component: TPersistent): TBoldSystemTypeInfo; override;
  end;

  { TBoldOclVariablesEditor }
  TBoldConstraintValidatorEditor = class(TBoldComponentDblClickEditor)
  protected
    function GetDefaultMethodName: string; override;
  end;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_MISC, [TBoldConstraintValidator]);
  RegisterPropertyEditor(TypeInfo(String), TBoldConstraintItem, 'Context', TBoldTypeNameSelectorForConstraintValidator);
  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldConstraintItem, 'Expression', TBoldOCLExpressionForOCLComponent);
  RegisterPropertyEditor(TypeInfo(TBoldExpression), TBoldConstraintItem, 'ErrorMessage', TBoldOCLExpressionForOCLComponent);
  RegisterComponentEditor(TBoldConstraintValidator, TBoldConstraintValidatorEditor);
end;

{ TBoldTypeNameSelectorForConstraintValidator }

function TBoldTypeNameSelectorForConstraintValidator.GetApprovedTypes: TBoldValueTypes;
begin
  Result := [bvtClass];
end;

function TBoldTypeNameSelectorForConstraintValidator.GetContextType(
  Component: TPersistent): TBoldSystemTypeInfo;
begin
  result := (TBoldConstraintItem(Component).Collection.Owner as TBoldConstraintValidator).StaticSystemTypeInfo;
end;

{ TBoldConstraintValidatorEditor }

function TBoldConstraintValidatorEditor.GetDefaultMethodName: string;
const
  sConstraints = 'Constraints';
begin
  Result := sConstraints;
end;

end.
