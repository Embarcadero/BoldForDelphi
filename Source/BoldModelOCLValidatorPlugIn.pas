
{ Global compiler directives }
{$include bold.inc}
unit BoldModelOCLValidatorPlugIn;

interface

uses
  Graphics,
  BoldUMLPlugins,
  BoldUMLModelEditPlugIn;

type
  { forward declaration of classes }
  TUMLOCLValidator = class;

  { TUMLOCLValidator }
  TUMLOCLValidator = class(TUMLPlugInFunction)
  protected
    function GetMenuItemName: String; override;
    function GetOptions: TBoldUMLPluginOptions; override;
    function GetPlugInType: TPlugInType; override;
    function GetImageResourceName: String; override;
    function GetImageMaskColor: TColor; override;
  public
    procedure Execute(Context: IUMLModelPlugInContext); override;
  end;

implementation

uses
  SysUtils,
  Classes,
  BoldUMLModel,
  BoldModel,
  BoldUMLPlugInCallBacks,
{$IFDEF BoldDevEx}
  BoldUMLModelValidationFormCx,
{$ELSE}
  BoldUMLModelValidationForm,
{$ENDIF}
  BoldUMLOCLValidator;

var
  _UMLOCLValidator: TUMLOCLValidator;

{ TUMLOCLValidator }

procedure TUMLOCLValidator.Execute(Context: IUMLModelPlugInContext);
var
{$IFDEF BoldDevEx}
  frmValidation: TfrmValidationCx;
{$ELSE}
  frmValidation: TfrmValidation;
{$ENDIF}
begin
  frmValidation := EnsureValidationForm(context.GetCurrentModelHandle, G_ValidationFormDefaultOwner, nil);
  frmValidation.ValidationProc := TUMLOCLValidatorCallBack.Validate;
  frmValidation.Validate;
  frmValidation.Show;
end;

function TUMLOCLValidator.GetImageMaskColor: TColor;
begin
  Result := clTeal;
end;

function TUMLOCLValidator.GetImageResourceName: String;
begin
  Result := ClassName;
end;

function TUMLOCLValidator.GetMenuItemName: String;
begin
  Result := 'Validate OCL in model';
end;

function TUMLOCLValidator.GetOptions: TBoldUMLPluginOptions;
begin
  Result := [poRequireBoldified];
end;

function TUMLOCLValidator.GetPlugInType: TPlugInType;
begin
  Result := ptTool;
end;

initialization
  _UMLOCLValidator := TUMLOCLValidator.Create(true);

finalization
  FreeAndNil(_UMLOCLValidator);

end.
