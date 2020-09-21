unit BoldDbPlugins;

interface

uses
  Graphics,
  BoldAbstractPersistenceHandleDB,
  BoldModel,
  BoldUMLModelEditPlugIn,
  BoldUMLPlugins,
  BoldDbValidator;

type
  { forward declarations }
  TUMLGenericDBPlugin = class;
  TUMLDBGenerator = class;
  TBoldUMLDBEvolutorPlugin = class;
  TBoldDbValidatorPlugIn = class;
  TBolddbStructureValidatorPlugin = class;
  TBoldDbDataValidatorPlugin = class;

  { TUMLGenericDBPlugin }
  TUMLGenericDBPlugin = class(TUMLPlugInFunction)
  private
    function GetPersistenceHandle(BoldModel: TBoldModel): TBoldAbstractPersistenceHandleDB;
  protected
    function GetImageMaskColor: TColor; override;
    function GetPlugInType: TPlugInType; override;
    function GetOptions: TBoldUMLPluginOptions; override;
    function GetValidPersistenceHandle(Context: IUMLModelPlugInContext): TBoldAbstractPersistenceHandleDB;
  end;

  { TUMLDBGenerator }
  TUMLDBGenerator = class(TUMLGenericDBPlugin)
  protected
    function GetMenuItemName: String; override;
    function GetImageResourceName: String; override;
  public
    procedure Execute(context: IUMLModelPlugInContext); override;
  end;

  { TBoldUMLDBEvolutorPlugin }
  TBoldUMLDBEvolutorPlugin = class(TUMLGenericDbPlugIn)
  protected
    function GetMenuItemName: String; override;
    function GetImageResourceName: String; override;
  public
    procedure Execute(context: IUMLModelPlugInContext); override;
  end;

  { TBoldDbValidatorPlugIn }
  TBoldDbValidatorPlugIn = class(TUMLGenericDBPlugin)
  protected
    function GetOptions: TBoldUMLPluginOptions; override;
    procedure ExecuteValidator(Validator: TBoldDbValidator; context: IUMLModelPlugInContext);
  end;

  { TBolddbStructureValidatorPlugin }
  TBolddbStructureValidatorPlugin = class(TBoldDbValidatorPlugIn)
  protected
    function GetMenuItemName: String; override;
    function GetImageResourceName: String; override;
  public
    procedure Execute(context: IUMLModelPlugInContext); override;
  end;

  { TBoldDbDataValidatorPlugin }
  TBoldDbDataValidatorPlugin = class(TBoldDbValidatorPlugIn)
  protected
    function GetMenuItemName: String; override;
    function GetImageResourceName: String; override;
  public
    procedure Execute(context: IUMLModelPlugInContext); override;
  end;

implementation

uses
  Dialogs,
  Classes,
  Controls,
  SysUtils,
  BoldDefs,
  BoldHandle,
  BoldDbEvolutorForm,
  BoldDbStructureValidator,
  BoldDbDataValidator,
  BoldUMLAttributes,
  BoldUMLModelValidator,
  BoldGuard;

var
  _DBGenerator: TUMLDBGenerator;
  _dbStructureValidator: TBolddbStructureValidatorPlugin;
  _dbDataValidator: TBolddbDataValidatorPlugin;
  _DbEvolutor: TBoldUMLDBEvolutorPlugin;

{ TUMLDBGenerator }

function TUMLDBGenerator.GetMenuItemName: String;
begin
  result := 'Generate Database';
end;

function TUMLDBGenerator.GetImageResourceName: String;
begin
  result := 'UMLPluginGenDBImage'; // do not localize
end;

procedure TUMLDBGenerator.Execute(context: IUMLModelPlugInContext);
begin
  GetValidPersistenceHandle(Context).CreateDataBaseSchema;
end;

procedure TBolddbStructureValidatorPlugin.Execute(context: IUMLModelPlugInContext);
var
  DbStructureValidator: TBoldDBStructureValidator;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(DbStructureValidator);
  DbStructureValidator := TBoldDBStructureValidator.Create(nil);
  Executevalidator(DbStructureValidator, Context);
end;

function TBolddbStructureValidatorPlugin.GetImageResourceName: String;
begin
  result := 'UMLPluginStructureValidator';
end;

function TBolddbStructureValidatorPlugin.GetMenuItemName: String;
begin
  result := 'Validate database structure';
end;

{ TBoldDbDataValidatorPlugin }

procedure TBoldDbDataValidatorPlugin.Execute(context: IUMLModelPlugInContext);
var
  DbDataValidator: TBoldDBDataValidator;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(DbDataValidator);
  DbDataValidator := TBoldDBDataValidator.Create(nil);
  Executevalidator(DbDataValidator, Context);
end;

function TBoldDbDataValidatorPlugin.GetImageResourceName: String;
begin
   result := 'UMLPluginDataValidator'; // do not localize
end;

function TBoldDbDataValidatorPlugin.GetMenuItemName: String;
begin
  result := 'Validate data integrity in database';
end;

{ TBoldDbValidatorPlugIn }

procedure TBoldDbValidatorPlugIn.ExecuteValidator(Validator: TBoldDbValidator; context: IUMLModelPlugInContext);
begin
  Validator.PersistenceHandle := GetValidPersistenceHandle(context);
  if Validator.Execute then
    ShowMessage('Database validated OK');
end;

function TBoldDbValidatorPlugIn.GetOptions: TBoldUMLPluginOptions;
begin
  Result := [poRequireBoldified];
end;

{ TBoldUMLDBEvolutorPlugin }

procedure TBoldUMLDBEvolutorPlugin.Execute(context: IUMLModelPlugInContext);
var
  PHandle: TBoldAbstractPersistenceHandleDB;
  Res: Word;
begin

  PHandle := GetValidPersistenceHandle(context);
  Res := MessageDlg('Generic script? (generic scripts can be used on other databases with the same schema. Answering "No" gives you a script that will only apply to this database',
    mtConfirmation, [mbYes, mbNo, mbCancel], 0);
  if res in [mrYes, mrNo] then
    TfrmBoldDbEvolutor.EvolveDB(PHandle, res = mrYes);
end;

function TBoldUMLDBEvolutorPlugin.GetImageResourceName: String;
begin
  result := 'UMLPluginDbEvolutor'; // do not localize
end;

function TBoldUMLDBEvolutorPlugin.GetMenuItemName: String;
begin
  result := 'Evolve database';
end;

{ TUMLGenericDBPlugin }


function TUMLGenericDBPlugin.GetImageMaskColor: TColor;
begin
  result := clTeal;
end;

function TUMLGenericDBPlugin.GetOptions: TBoldUMLPluginOptions;
begin
  Result := [poRequireBoldified];
end;

function TUMLGenericDBPlugin.GetPersistenceHandle(BoldModel: TBoldModel): TBoldAbstractPersistenceHandleDB;
var
  i: integer;
  temp: TBoldAbstractPersistenceHandleDB;
begin
  result := nil;
  for i := 0 to BoldHandle.BoldHandleList.Count - 1 do
    if (BoldHandleList[i] is TBoldAbstractPersistenceHandleDB) then
    begin
      temp := BoldHandleList[i] as TBoldAbstractPersistenceHandleDB;
      if (temp.BoldModel = BoldModel) and
        (MessageDlg(format('Use Database settings from %s?', [Temp.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      begin
        result := temp;
        break;
      end;
    end;
  if not assigned(Result) then
    raise Exception.Create('No persistencehandle found. If your persistencehandle is on a datamodule/form that has not been opened, please open the datamodule/form and try again');
end;

function TUMLGenericDBPlugin.GetPlugInType: TPlugInType;
begin
  result := ptTool;
end;

function TUMLGenericDBPlugin.GetValidPersistenceHandle(
  Context: IUMLModelPlugInContext): TBoldAbstractPersistenceHandleDB;
var
  BoldModel: TBoldModel;
  Validator: TBoldUMLModelValidator;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(Validator);
  BoldModel := Context.GetCurrentModelHandle;
  if not assigned(BoldModel) then
    raise EBold.CreateFmt('%s: Unable to find TBoldModel component for this model', [MenuItemName] );
  Result := GetPersistenceHandle(BoldModel);
  if not assigned(Result) then
    raise EBold.CreateFmt('%s: No persistence handle to act on', [MenuItemName]);
  Validator := TBoldUMLModelValidator.Create(Context.GetCurrentModelHandle.EnsuredUMLModel, Result.SQLDataBaseConfig);
  Validator.validate(BoldModel.TypeNameDictionary);

  if Context.GetCurrentModelHandle.EnsuredUMLModel.Validator.HighestSeverity = sError then
    raise EBold.CreateFmt('%s: Errors in model', [MenuItemName]);
end;

initialization
  _DBGenerator    := TUMLDBGenerator.Create(true);
  _dbStructureValidator := TBolddbStructureValidatorPlugin.Create(true);
  _dbDataValidator := TBolddbDataValidatorPlugin.Create(true);
  _DbEvolutor := TBoldUMLDBEvolutorPlugin.Create(true);

finalization
  FreeAndNil(_DBGenerator);
  FreeAndNil(_dbStructureValidator);
  FreeAndNil(_dbDataValidator);
  FreeAndNil(_DbEvolutor);
end.

