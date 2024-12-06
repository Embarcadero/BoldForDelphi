
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLPlugins;

interface

uses
  Graphics,
  windows,
  BoldDefs,
  BoldUMLModelEditPlugIn,
  BoldUMLAttributes,
  BoldUMLModel,
  BoldUMLModelValidator,
  BoldUMLModelLink;

type
  { forward declarations }
  TUMLPlugInFunction = class;
  TUMLLinkImporterExporter = class;
  TUMLLinkImporter = class;
  TUMLLinkExporter = class;
  TUMLModelUpdaterPlugIn = class;

  { TUMLPlugInFunction }
  TUMLPlugInFunction = class
  private
    fPlugIn: TUMLPlugIn;
  protected
    function GetMenuItemName: String; virtual;
    function GetPlugInType: TPlugInType; virtual; abstract;
    function GetInstance: THandle; virtual;
    function GetImageResourceName: String; virtual;
    function GetImageMaskColor: TColor; virtual; abstract;
    function GetOptions: TBoldUMLPluginOptions; virtual;
  public
    constructor Create(InstallPlugin: Boolean);
    destructor Destroy; override;
    procedure Install;
    property MenuItemName: String read GetMenuItemName;
    property PlugInType: TPlugInType read GetPlugInType;
    property Instance: THandle read GetInstance;
    property ImageResourceName: String read GetImageResourceName;
    property ImageMaskColor: TColor read GetImageMaskColor;
    property Options: TBoldUMLPluginOptions read GetOptions;
    procedure Execute(context: IUMLModelPlugInContext); virtual; abstract;
  end;

  { TUMLLinkImporterExporter }
  TUMLLinkImporterExporter = class(TUMLPlugInFunction)
  protected
    procedure Initialize(Context: IUMLModelPlugInContext; var UMLModel: TUMLModel; var UMLLink: TBoldUMLModelLink);
  end;

  { TUMLLinkImporter }
  TUMLLinkImporter = class(TUMLLinkImporterExporter)
  protected
    function GetMenuItemName: String; override;
    function GetPlugInType: TPlugInType; override;
    function GetImageResourceName: String; override;
    function GetImageMaskColor: TColor; override;
  public
    procedure Execute(context: IUMLModelPlugInContext); override;
  end;

  { TUMLLinkExporter }
  TUMLLinkExporter = class(TUMLLinkImporterExporter)
  protected
    function GetMenuItemName: String; override;
    function GetPlugInType: TPlugInType; override;
    function GetImageResourceName: String; override;
    function GetImageMaskColor: TColor; override;
    function GetOptions: TBoldUMLPluginOptions; override;    
  public
    procedure Execute(context: IUMLModelPlugInContext); override;
  end;

  { TUMLModelUpdaterPlugIn }
  TUMLModelUpdaterPlugIn = class(TUMLPlugInFunction)
  protected
    function GetMenuItemName: String; override;
    function GetPlugInType: TPlugInType; override;
    function GetImageResourceName: String; override;
    function GetImageMaskColor: TColor; override;
  public
    procedure Execute(context: IUMLModelPlugInContext); override;
  end;

implementation

{$R BoldUMLPlugins.res}

uses
  SysUtils,
  BoldUtils,
  Dialogs,
  Controls,
  Classes,
  System.Types,
  System.UITypes,
  BoldMeta,
  BoldModel,
  BoldLogHandler,
  BoldUMLModelConverter,
  BoldUMLModelEdit,
  BoldUMLModelSupport,
  BoldUMLModelUpdater,
  BoldUMLUtils,
  BoldGuard;

var
  _LinkExporter: TUMLLinkExporter;
  _LinkImporter: TUMLLinkImporter;

{ TUMLPlugInFunction }

constructor TUMLPlugInFunction.Create(InstallPlugin: Boolean);
begin
  inherited Create;
  if InstallPlugin then
    Install;
end;

destructor TUMLPlugInFunction.Destroy;
begin
  if assigned(fPlugIn) then
  begin
    if UMLModelEditorAssigned then
      UMLModelEditor.PlugInList.Remove(fPlugIn);
    FreeAndNil(fPlugIn);
  end;
  inherited;
end;

function TUMLPlugInFunction.GetImageResourceName: String;
begin
  Result := ClassName;
end;

function TUMLPlugInFunction.GetInstance: THandle;
begin
  result := HInstance;
end;

function TUMLPlugInFunction.GetMenuItemName: String;
begin
  Result := ClassName;
end;

function TUMLPlugInFunction.GetOptions: TBoldUMLPluginOptions;
begin
  result := [];
end;

procedure TUMLPlugInFunction.Install;
begin
  fPlugIn := TUMLPlugIn.Create(MenuItemName, Execute, PlugInType,
    Instance, ImageResourceName, ImageMaskColor,  Options, classname);
  UMLModelEditor.PlugInList.Add(fPlugIn);
end;

{ TUMLLinkImporter }

function TUMLLinkImporter.GetMenuItemName: String;
begin
  result := 'Import via Link';
end;

function TUMLLinkImporter.GetPlugInType: TPlugInType;
begin
  result := ptFile;
end;

function TUMLLinkImporter.GetImageResourceName: String;
begin
  result := 'UMLPluginImportImage';
end;

function TUMLLinkImporter.GetImageMaskColor: TColor;
begin
  result := clTeal;
end;

procedure TUMLLinkImporter.Execute(context: IUMLModelPlugInContext);
var
  UMLLink: TBoldUMLModelLink;
  UMLModel: TUMLModel;
begin
  if MessageDlg('Are you sure you want to import?', mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    Initialize(Context, UMLModel, UMLLink);
    try
      BoldLog.StartLog(Format('Importing Model From %s', [UMLLink.DisplayName]));
      UMLModel.Clear;
      UMLLInk.ImportModel(UMLModel);
      BoldLog.EndLog;
    except
      on E: Exception do
      begin
        BoldLog.Log(E.Message);
        raise;
      end;
    end;
  end;
end;

{ TUMLLinkExporter }

function TUMLLinkExporter.GetMenuItemName: String;
begin
  result := 'Export via Link';
end;

function TUMLLinkExporter.GetPlugInType: TPlugInType;
begin
  result := ptFile;
end;

function TUMLLinkExporter.GetImageResourceName: String;
begin
  result := 'UMLPluginExportImage';
end;

function TUMLLinkExporter.GetImageMaskColor: TColor;
begin
  result := clTeal;
end;

procedure TUMLLinkExporter.Execute(context: IUMLModelPlugInContext);
var
  UMLLink: TBoldUMLModelLink;
  UMLModel: TUMLModel;
begin
  if MessageDlg('Are you sure you want to export?', mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    Initialize(Context, UMLModel, UMLLink);
    try
      BoldLog.StartLog(Format('Exporting Model To %s', [UMLLink.DisplayName]));
      UMLLink.ExportModel(UMLModel);
    except
      on E: Exception do
      begin
        BoldLog.Log(E.Message);
        raise;
      end;
    end;
  end;
end;

function TUMLLinkExporter.GetOptions: TBoldUMLPluginOptions;
begin
  Result := [poRequireUnboldified];
end;

{ TUMLLinkImporterExporter }

procedure TUMLLinkImporterExporter.Initialize(
  Context: IUMLModelPlugInContext; var UMLModel: TUMLModel; var UMLLink: TBoldUMLModelLink);
var
  ModelComponent: TBoldModel;
begin
  ModelComponent := context.GetCurrentModelHandle;
  UMLModel := ModelComponent.EnsuredUMLModel;
  if not assigned(ModelComponent) then
    raise EBold.CreateFmt('%s: Unable to find model component', [ClassName]);

  UMLLink := BoldGetUMLModelLinkForComponent(Modelcomponent);
  if not assigned(UMLLink) then
    raise EBold.CreateFmt('%s: There is no link component for %s', [ClassName, ModelComponent.Name]);
end;

{ TUMLModelUpdaterPlugIn }

procedure TUMLModelUpdaterPlugIn.Execute(context: IUMLModelPlugInContext);
begin
  TBoldUMLModelUpdater.UpdateModel(Context.GetCurrentModelHandle.EnsuredUMLModel);
end;

function TUMLModelUpdaterPlugIn.GetImageMaskColor: TColor;
begin
  result := clTeal;
end;

function TUMLModelUpdaterPlugIn.GetImageResourceName: String;
begin
  result := 'UMLPluginModelUpdater';
end;

function TUMLModelUpdaterPlugIn.GetMenuItemName: String;
begin
  result := 'Update model to latest version';
end;

function TUMLModelUpdaterPlugIn.GetPlugInType: TPlugInType;
begin
  result := ptTool;
end;

initialization
  _LinkExporter   := TUMLLinkExporter.Create(true);
  _LinkImporter   := TUMLLinkImporter.Create(true);

finalization
  FreeAndNil(_LinkExporter);
  FreeAndNil(_LinkImporter);

end.