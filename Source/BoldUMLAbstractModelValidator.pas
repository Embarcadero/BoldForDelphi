{ Global compiler directives }
{$include bold.inc}
unit BoldUMLAbstractModelValidator;

interface

uses
  BoldModel,
  BoldUMLAttributes,
  BoldUMLModel,
  BoldSQLDatabaseConfig;

type
  TBoldModelValidatorSourceLanguage = (mvslNone, mvslDelphi, mvslCpp);

  TBoldUMLAbstractModelValidator = class
  private
    fSQLDataBaseConfig: TBoldSQLDataBaseConfig;
    fLanguage: TBoldModelValidatorSourceLanguage;
    fBoldModel: TBoldModel;
    function GetValidator: TValidator;
    procedure AddViolation(severity: TSeverity; const description: String; element: TUMLModelElement);
    function GetUMLModel: TUMLModel;
  protected
    procedure AddError(description: String; args: array of const; element: TUMLModelElement);
    procedure AddHint(description: String; args: array of const; element: TUMLModelElement);
    procedure AddWarning(description: String; args: array of const; element: TUMLModelElement);
    procedure ClearViolations;
    property SQLDataBaseConfig: TBoldSQLDataBaseConfig read fSQLDataBaseConfig;
    property Language: TBoldModelValidatorSourceLanguage read fLanguage;
  public
    constructor Create(ABoldModel: TBoldModel; SQLDataBaseConfig: TBoldSQLDataBaseConfig = nil; const Language: TBoldModelValidatorSourceLanguage = mvslNone);
    property UMLModel: TUMLModel read GetUMLModel;
    property BoldModel: TBoldModel read fBoldModel;
    property Validator: TValidator read GetValidator;
    function HighestSeverity: TSeverity;
  end;

const
  {$IFDEF BOLD_DELPHI}
  BoldDefaultValidatorSourceLanguage = mvslDelphi;
  {$ENDIF}
  {$IFDEF BOLD_BCB}
  BoldDefaultValidatorSourceLanguage = mvslCpp;
  {$ENDIF}

implementation

uses
  SysUtils,
  BoldDefs;

procedure TBoldUMLAbstractModelValidator.AddError(description: String; args: array of const; element: TUMLModelElement);
begin
  AddViolation(sError, format(description, args), element);
end;

procedure TBoldUMLAbstractModelValidator.AddWarning(description: String; args: array of const; element: TUMLModelElement);
begin
  AddViolation(sWarning, format(description, args), element);
end;

procedure TBoldUMLAbstractModelValidator.AddHint(description: String; args: array of const; element: TUMLModelElement);
begin
  AddViolation(sHint, format(description, args), element);
end;

procedure TBoldUMLAbstractModelValidator.AddViolation(severity: TSeverity; const description: String; element: TUMLModelElement);
var
  v : TViolation;
begin
  Validator.BoldSystem.StartTransaction();
  try
  v := Validator.Violation.AddNew;
  v.Severity := severity;
  v.Description := description;
  v.modelElement := element;
    Validator.BoldSystem.CommitTransaction()
  except
    Validator.BoldSystem.RollbackTransaction();
    raise;
  end;
end;

procedure TBoldUMLAbstractModelValidator.ClearViolations;
var
  i: integer;
begin
  if Validator.violation.Empty then
    exit;
  Validator.BoldSystem.StartTransaction();
  try
  for i := Validator.violation.count-1 downto 0 do
      Validator.Violation[i].Delete;
    Validator.BoldSystem.CommitTransaction()
  except
    Validator.BoldSystem.RollbackTransaction();
    raise;
  end;
end;

constructor TBoldUMLAbstractModelValidator.Create(ABoldModel: TBoldModel; SQLDataBaseConfig: TBoldSQLDataBaseConfig; const Language: TBoldModelValidatorSourceLanguage);
begin
  inherited Create;
  fSQLDataBaseConfig := SQLDataBaseConfig;
  fBoldModel := ABoldModel;
  fLanguage := Language;
end;

function TBoldUMLAbstractModelValidator.GetUMLModel: TUMLModel;
begin
  if Assigned(BoldModel) then  
    result := BoldModel.EnsuredUMLModel
  else
    result := nil;
end;

function TBoldUMLAbstractModelValidator.GetValidator: TValidator;
begin
  result := UMLModel.Validator;
end;

function TBoldUMLAbstractModelValidator.HighestSeverity: TSeverity;
var
  i: integer;
begin
  result := sNone;
  for i := 0 to Validator.violation.count-1 do
    if Validator.Violation[i].Severity > result then
      result := Validator.violation[i].Severity;
end;

end.