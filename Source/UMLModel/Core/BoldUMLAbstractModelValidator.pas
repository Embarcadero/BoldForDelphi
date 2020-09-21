unit BoldUMLAbstractModelValidator;

interface

uses
  BoldUMLAttributes,
  BoldUMLModel,
  BoldSQLDatabaseConfig;

type
  TBoldModelValidatorSourceLanguage = (mvslNone, mvslDelphi, mvslCpp);

  TBoldUMLAbstractModelValidator = class
  private
    fSQLDataBaseConfig: TBoldSQLDataBaseConfig;
    fLanguage: TBoldModelValidatorSourceLanguage;
    fUMLModel: TUMLModel;
    function GetValidator: TValidator;
    procedure AddViolation(severity: TSeverity; description: String; element: TUMLModelElement);
  protected
    procedure AddError(description: String; args: array of const; element: TUMLModelElement);
    procedure AddHint(description: String; args: array of const; element: TUMLModelElement);
    procedure AddWarning(description: String; args: array of const; element: TUMLModelElement);
    procedure ClearViolations;
    property SQLDataBaseConfig: TBoldSQLDataBaseConfig read fSQLDataBaseConfig;
    property Language: TBoldModelValidatorSourceLanguage read fLanguage;
  public
    constructor Create(UMLModel: TUMLModel; SQLDataBaseConfig: TBoldSQLDataBaseConfig = nil; const Language: TBoldModelValidatorSourceLanguage = mvslNone);
    property UMLModel: TUMLModel read fUMLModel;
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
  SysUtils;

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

procedure TBoldUMLAbstractModelValidator.AddViolation(severity: TSeverity; description: String; element: TUMLModelElement);
var
  v : TViolation;
begin
  v := Validator.Violation.AddNew;
  v.Severity := severity;
  v.Description := description;
  v.modelElement := element;
end;

procedure TBoldUMLAbstractModelValidator.ClearViolations;
var
  i: integer;
begin
  for i := Validator.violation.count-1 downto 0 do
    Validator.Violation[i].Delete
end;

constructor TBoldUMLAbstractModelValidator.Create(UMLModel: TUMLModel; SQLDataBaseConfig: TBoldSQLDataBaseConfig; const Language: TBoldModelValidatorSourceLanguage);
begin
  inherited Create;
  fSQLDataBaseConfig := SQLDataBaseConfig;
  fUMLModel := UMLModel;
  fLanguage := Language;
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
