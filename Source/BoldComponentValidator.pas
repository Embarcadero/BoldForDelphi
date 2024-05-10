
{ Global compiler directives }
{$include bold.inc}
unit BoldComponentValidator;

interface

uses
  Classes,
  BoldElements;

type
  { forward declarations }
  IBoldValidateableComponent = interface;
  TBoldComponentValidator = class;

  { IBoldValidateableComponent }
  IBoldValidateableComponent = interface
  ['{29934FB1-E60A-11D3-A340-8CA956000000}']
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean;
  end;

  { TBoldComponentValidator }
  TBoldComponentValidator = class
  private
    fValidateCount: integer;
  protected
    function ValidateValidateableComponent(Component: IBoldValidateableComponent; NamePrefix: String): Boolean;
    procedure InitializeLog;
    procedure CompleteLog;
  public
    function ValidateComponent(Component: TPersistent; NamePrefix: String = ''; TraverseSubComponents: Boolean = false): Boolean;
    function ValidateExpressionInContext(Expression: string; Context: TBoldElementTypeInfo; Name: String; const VariableList: TBoldExternalVariableList = nil): Boolean;
    procedure ValidateApplication;
    function ValidateOCLComponent(Component: IBoldOCLComponent; Name: String): Boolean;
    property ValidateCount: integer read fValidateCount write fValidateCount;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldLogHandler,
  BoldDefs,
  BoldEnvironment;

{ TBoldComponentValidator }

function TBoldComponentValidator.ValidateComponent(Component: TPersistent; NamePrefix: String = ''; TraverseSubComponents: Boolean = false): Boolean;
var
  OCLComponent: IBoldOCLComponent;
  ValidateableComponent: IBoldValidateableComponent;
  i: integer;
  vCounter: integer;
  vIsFormOrDataModule: boolean;
begin
  vCounter := ValidateCount;
  vIsFormOrDataModule := BoldEffectiveEnvironment.IsFormOrDataModule(Component);
  if vIsFormOrDataModule then
    BoldLog.LogFmt(sValidatingHeader, [(Component as TComponent).Name], ltInfo);

  result := true;
  if Supports(Component, IBoldValidateableComponent, ValidateableComponent) then
    result := result and ValidateValidateableComponent(ValidateableComponent, NamePrefix + Component.GetNamePath)
  else
  if Supports(Component, IBoldOCLComponent, OCLComponent) then
    result := result and ValidateOCLComponent(OclComponent, NamePrefix + Component.GetNamePath);

  if TraverseSubComponents and (Component is TComponent) then
    for i := 0 to (Component as TComponent).ComponentCount - 1 do
      result := result and ValidateComponent((Component as TComponent).Components[i], NamePrefix + Component.GetNamePath + '.', TraverseSubComponents);
  if vIsFormOrDataModule then
  begin
    if result then
      BoldLog.LogFmt(sOKCheckMessage, [(Component as TComponent).Name, ValidateCount - vCounter], ltInfo)
    else
      BoldLog.LogFmt(sErrorCheckMessage, [(Component as TComponent).Name, ValidateCount - vCounter], ltWarning);
  end;
end;

function TBoldComponentValidator.ValidateExpressionInContext(Expression: string; Context: TBoldElementTypeInfo; Name: String; const VariableList: TBoldExternalVariableList = nil): Boolean;
begin
  inc(fValidateCount);
  result := true;
  if not assigned(Context) then
  begin
    result := false;
    BoldLog.LogFmt(sNoContext, [Name], ltWarning);
  end
  else
  begin
    try
      Context.Evaluator.ExpressionType(Expression, Context, true, VariableList);
    except
      on e: exception do
      begin
        result := false;
        BoldLog.LogFmt(sValidationError, [Name, e.Message], ltError);
      end;
    end;
    if not Result then
      BoldLog.LogFmt(sValidationExpression, [Expression], ltWarning);
  end;
end;

procedure TBoldComponentValidator.ValidateApplication;
var
  i: integer;
  RootComponent: TComponent;
begin
  BoldLog.Show;
  RootComponent := BoldEffectiveEnvironment.RootComponent;
  for i := 0 to RootComponent.ComponentCount - 1 do
    ValidateComponent(RootComponent.Components[i], '', true);
end;

function TBoldComponentValidator.ValidateOCLComponent(Component: IBoldOCLComponent; Name: String): Boolean;
begin
  result := ValidateExpressionInContext(Component.Expression, Component.ContextType, Name, Component.VariableList);
end;


function TBoldComponentValidator.ValidateValidateableComponent(Component: IBoldValidateableComponent; NamePrefix: String): Boolean;
begin
  result := Component.ValidateComponent(self, NamePrefix);
end;

procedure TBoldComponentValidator.InitializeLog;
begin
  BoldLog.Show;
  BoldLog.StartLog(sComponentValidation);
end;

procedure TBoldComponentValidator.CompleteLog;
begin
  BoldLog.EndLog;
  BoldLog.Hide;
  BoldLog.Show;
end;

end.
