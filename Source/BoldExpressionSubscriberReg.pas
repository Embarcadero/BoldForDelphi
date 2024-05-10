unit BoldExpressionSubscriberReg;

interface

procedure Register;

implementation

uses
  Classes,
  DesignIntf,

  BoldExpressionSubscriber,

  BoldDefs,
  BoldElements,
  BoldSystemRt,
  BoldPropertyEditors;

type
  { TBoldTypeNameSelectorForExpressionSubscriber }
  TBoldTypeNameSelectorForExpressionSubscriber = class (TBoldTypeNameSelectorForHandles)
  protected
    function GetApprovedTypes: TBoldValueTypes; override;
    function GetContextType(Component: TPersistent): TBoldSystemTypeInfo; override;
  end;

  { TBoldOclVariablesEditor }
  TExpressionSubscriberEditor = class(TBoldComponentDblClickEditor)
  protected
    function GetDefaultMethodName: string; override;
  end;

procedure Register;
const
  cIdePalletePage = 'AT Bold';
begin
  RegisterComponents(cIdePalletePage, [TExpressionSubscriber]);
  RegisterPropertyEditor(TypeInfo(String), TExpressionDefinition, 'ClassExpressionName', TBoldTypeNameSelectorForExpressionSubscriber);
  RegisterPropertyEditor(TypeInfo(TBoldExpression), TExpressionDefinition, 'Condition', TBoldOCLExpressionForOCLComponent);
  RegisterComponentEditor(TExpressionSubscriber, TExpressionSubscriberEditor);
end;

{ TBoldTypeNameSelectorForExpressionSubscriber }

function TBoldTypeNameSelectorForExpressionSubscriber.GetApprovedTypes: TBoldValueTypes;
begin
  Result := [bvtClass];
end;

function TBoldTypeNameSelectorForExpressionSubscriber.GetContextType(
  Component: TPersistent): TBoldSystemTypeInfo;
begin
  result := (TExpressionDefinition(Component).Collection.Owner as TExpressionSubscriber).StaticSystemTypeInfo;
end;

{ TExpressionSubscriberEditor }

function TExpressionSubscriberEditor.GetDefaultMethodName: string;
begin
  Result := 'Expressions';
end;

end.
