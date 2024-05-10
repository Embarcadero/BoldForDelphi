
{ Global compiler directives }
{$include bold.inc}
unit BoldCodePlugins;

interface

uses
  Graphics,
  BoldMeta,
  BoldGen,
  BoldTypeNameDictionary,
  BoldUMLModelEditPlugIn,
  BoldUMLPlugins;

type
  { forward declarations }
  TUMLGUIDGenerator = class;
  TUMLAbstractCodeGenerator = class;
  TUMLCodeGenerator = class;
  TUMLIDLGenerator = class;
  TUMLMIDLGenerator = class;

  { TUMLGUIDGenerator }
  TUMLGUIDGenerator = class(TUMLPlugInFunction)
  protected
    function GetMenuItemName: String; override;
    function GetImageResourceName: String; override;
    function GetPlugInType: TPlugInType; override;
    function GetImageMaskColor: TColor; override;
    function GetOptions: TBoldUMLPluginOptions; override;
  public
    procedure Execute(context: IUMLModelPlugInContext); override;
  end;

  { TUMLAbstractCodeGenerator }
  TUMLAbstractCodeGenerator = class(TUMLPlugInFunction)
  protected
    function GetPlugInType: TPlugInType; override;
    function GetImageMaskColor: TColor; override;
    procedure GenerateCode(Generator: TBoldGenerator); virtual; abstract;
    procedure Generate(MoldModel: TMoldModel; TypeNameDictionary: TBoldTypeNameDictionary; Path: string);
    function GetOptions: TBoldUMLPluginOptions; override;
  public
    procedure Execute(context: IUMLModelPlugInContext); override;
  end;

  { TUMLCodeGenerator }
  TUMLCodeGenerator = class(TUMLAbstractCodeGenerator)
  protected
    function GetMenuItemName: String; override;
    function GetImageResourceName: String; override;
    procedure GenerateCode(Generator: TBoldGenerator); override;
  end;

  TUMLPersistenceInterfaceGenerator = class(TUMLAbstractCodeGenerator)
  protected
    function GetMenuItemName: String; override;
    function GetImageResourceName: String; override;
    procedure GenerateCode(Generator: TBoldGenerator); override;
  end;

  { TUMLIDLGenerator }
  TUMLIDLGenerator = class(TUMLAbstractCodeGenerator)
  protected
    function GetMenuItemName: String; override;
    function GetImageResourceName: String; override;
    procedure GenerateCode(Generator: TBoldGenerator); override;
  end;

  { TUMLMIDLGenerator }
  TUMLMIDLGenerator = class(TUMLAbstractCodeGenerator)
  protected
    function GetImageResourceName: String; override;
    function GetMenuItemName: String; override;
    procedure GenerateCode(Generator: TBoldGenerator); override;
  end;

implementation

uses
  SysUtils,
  Controls,
  Dialogs,

  BoldCoreConsts,
  BoldGuard,
  BoldDefs,
  BoldLogHandler,
  BoldModel,
  BoldEnvironment,
  BoldUMLAbstractModelValidator,
  BoldUMLModelValidator,
  {To compile without OTA (for runtime-testing), define NO_OTA}
  {$IFNDEF NO_OTA}
  BoldOTASupport,
  {$ENDIF}
  BoldUMLAttributes,
  BoldQueue,
  BoldUMLModel,
  BoldGUIDUtils,
  BoldDefaultTaggedValues;

var
  _CodeGenerator: TUMLCodeGenerator;
  _PersistenceInterfaceGenerator: TUMLPersistenceInterfaceGenerator;
  _GUIDGenerator: TUMLGUIDGenerator;
  _MIDLGenerator: TUMLMIDLGenerator;
  _IDLGenerator: TUMLIDLGenerator;

{ TUMLCodeGenerator }

function TUMLCodeGenerator.GetMenuItemName: String;
begin
  result := sGenerateCode;
end;

function TUMLCodeGenerator.GetImageResourceName: String;
begin
  result := sUMLCodeImage;
end;

procedure TUMLCodeGenerator.GenerateCode(Generator: TBoldGenerator);
begin
  Generator.GenerateBusinessObjectCode;
  Generator.EnsureMethodImplementations;
end;

{ TUMLIDLGenerator }

procedure TUMLIDLGenerator.GenerateCode(Generator: TBoldGenerator);
begin
  generator.UseTypedLists := true;
  Generator.GenerateComInterfaces;
end;

function TUMLIDLGenerator.GetImageResourceName: String;
begin
  result := sUMLIDLImage;
end;

function TUMLIDLGenerator.GetMenuItemName: String;
begin
  result := sUMLBorlandIDL;
end;

{ TUMLMIDLGenerator }

procedure TUMLMIDLGenerator.GenerateCode(Generator: TBoldGenerator);
begin
  Generator.GenerateMIDLCode := true;
  Generator.UseTypedLists := false;
  Generator.GenerateComInterfaces;
end;

function TUMLMIDLGenerator.GetImageResourceName: String;
begin
  result := sUMLGenIDLImage;
end;

function TUMLMIDLGenerator.GetMenuItemName: String;
begin
  result := sUMLGenMSIDL;
end;

{ TUMLAbstractCodeGenerator }

procedure TUMLAbstractCodeGenerator.Execute(context: IUMLModelPlugInContext);
var
  BoldModel: TBoldModel;
  Validator: TBoldUMLModelValidator;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(Validator);
  BoldModel := Context.GetCurrentModelHandle;

  Validator := TBoldUMLModelValidator.Create(Context.GetCurrentModelHandle, nil, BoldDefaultValidatorSourceLanguage);
  Validator.validate(BoldModel.TypeNameDictionary);

  if context.GetCurrentModelHandle.EnsuredUMLModel.Validator.HighestSeverity = sError then
    raise EBold.Create(sModelError);

  BoldLog.StartLog(Format(sCodeGeneration, [MenuItemName]));
  {$IFDEF NO_OTA}
  Generate(BoldModel.MoldModel, BoldModel.TypeNameDictionary, '');
  {$ELSE}
  Generate(BoldModel.MoldModel, BoldModel.TypeNameDictionary, BoldFilePathForComponent(BoldModel));
  {$ENDIF}
  BoldLog.EndLog;
end;

procedure TUMLAbstractCodeGenerator.Generate(MoldModel: TMoldModel;
  TypeNameDictionary: TBoldTypeNameDictionary; Path: string);
var
  Generator: TBoldGenerator;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(Generator);
  Generator := TBoldGenerator.Create(TypenameDictionary);
  Generator.BaseFilePath := Path;
  Generator.UseTypedLists := true;
  Generator.MoldModel := MoldModel;
  GenerateCode(Generator);
end;

function TUMLAbstractCodeGenerator.GetImageMaskColor: TColor;
begin
  result := clTeal;
end;

function TUMLAbstractCodeGenerator.GetOptions: TBoldUMLPluginOptions;
begin
  Result := [poRequireBoldified];
end;

function TUMLAbstractCodeGenerator.GetPlugInType: TPlugInType;
begin
  result := ptTool;
end;

{ TUMLGUIDGenerator }

procedure TUMLGUIDGenerator.Execute(context: IUMLModelPlugInContext);
var
  i: integer;
  OverwriteOld: Boolean;

procedure NewGuid(Element: TUMLModelElement; TaggedValueName: String; OverwriteOld: Boolean);
begin
  if OverwriteOld or (Trim(Element.GetBoldTV(TaggedValuename)) = '') then
    Element.SetBoldTV(TaggedValuename, BoldCreateGUIDAsString(true));
end;

var
  UMLModel: TUMLModel;

begin
  OverwriteOld := MessageDlg('Keep Existing GUIDs?', mtConfirmation, [mbYes, mbNo], 0) = mrNo;
  UMLModel := Context.GetCurrentModelhandle.EnsuredUMLModel;
  NewGuid(UMLModel, TAG_GUID, OverwriteOld);
  for i := 0 to UMLModel.Classes.Count - 1 do
  begin
    NewGuid(UMLModel.Classes[i], TAG_GUID, OverwriteOld);
    NewGuid(UMLModel.Classes[i], TAG_LISTGUID, OverwriteOld);
  end;
end;

function TUMLGUIDGenerator.GetImageMaskColor: TColor;
begin
  result := clTeal;
end;

function TUMLGUIDGenerator.GetImageResourceName: String;
begin
  result := sUMLGenGUIDS;
end;

function TUMLGUIDGenerator.GetMenuItemName: String;
begin
  Result := sUMLGenCOMIDL_GUIDS;
end;

function TUMLGUIDGenerator.GetOptions: TBoldUMLPluginOptions;
begin
  Result := [poRequireBoldified];
end;

function TUMLGUIDGenerator.GetPlugInType: TPlugInType;
begin
  result := ptTool;
end;

{ TUMLPersistenceInterfaceGenerator }

procedure TUMLPersistenceInterfaceGenerator.GenerateCode(Generator: TBoldGenerator);
begin
  Generator.GeneratePersistenceInterfaces;
end;

function TUMLPersistenceInterfaceGenerator.GetImageResourceName: String;
begin
  result := sUMLPluginGenInterface;
end;

function TUMLPersistenceInterfaceGenerator.GetMenuItemName: String;
begin
  result := sUMLGenInterface;
end;

initialization
  _CodeGenerator  := TUMLCodeGenerator.Create(true);
  _PersistenceInterfaceGenerator  := TUMLPersistenceInterfaceGenerator.Create(true);
  _GUIDGenerator  := TUMLGUIDGenerator.Create(true);
  _IDLGenerator  := TUMLIDLGenerator.Create(true);
  _MIDLGenerator  := TUMLMIDLGenerator.Create(true);

finalization
  FreeAndNil(_CodeGenerator);
  FreeAndNil(_PersistenceInterfaceGenerator);
  FreeAndNil(_GUIDGenerator);
  FreeAndNil(_IDLGenerator);
  FreeAndNil(_MIDLGenerator);
end.
