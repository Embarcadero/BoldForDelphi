
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLPluginCallBacks;

interface

uses
  BoldModel,
  BoldTypeNameDictionary;

type
  { forward declarations }
  TUMLOCLValidatorCallBack = class;
  TBoldUMLModelValidatorCallBack = class;

  { TUMLOCLValidatorCallBack }
  TUMLOCLValidatorCallBack = class
  public
    class procedure Validate(Modelhandle: TBoldModel);
  end;

  { TBoldUMLModelValidatorCallBack }
  TBoldUMLModelValidatorCallBack = class
  public
    class procedure Validate(Modelhandle: TBoldModel);
  end;  

implementation

uses
  BoldUMLModelValidator,
  BoldUMLOCLValidator,
  BoldModelOCLValidatorPlugIn,
  BoldUMLModel,
  BoldUMLUtils;

{ TUMLOCLValidatorCallBack }

class procedure TUMLOCLValidatorCallBack.Validate(Modelhandle: TBoldModel);
var
  OCLValidityChecker: TOCLValidityChecker;
begin
  OCLValidityChecker := TOCLValidityChecker.Create(Modelhandle.TypeNameDictionary);
  try
    OCLValidityChecker.ValidateModel(Modelhandle);
  finally
    OCLValidityChecker.Free;
  end;
end;

{ TBoldUMLModelValidatorCallBack }

class procedure TBoldUMLModelValidatorCallBack.Validate(Modelhandle: TBoldModel);
var
  ModelValidator: TBoldUMLModelValidator;
begin
  ModelValidator := TBoldUMLModelValidator.Create(Modelhandle, SQLDataBaseConfigforModel(Modelhandle));
  try
    ModelValidator.Validate(Modelhandle.TypeNameDictionary);
  finally
    ModelValidator.Free;
  end;
end;

end.
