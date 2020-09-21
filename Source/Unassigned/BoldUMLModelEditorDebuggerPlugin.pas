unit BoldUMLModelEditorDebuggerPlugin;

interface

uses
  Graphics,
  BoldSystemDebuggerForm,
  BoldUMLModel,
  BoldUMLPlugins,
  BoldUMLModelEditPlugIn;

type
  { forward declaration of classes }
  TBoldUMLModelEditorDebuggerPlugin = class;

  { TBoldUMLModelEditorDebuggerPlugin }
  TBoldUMLModelEditorDebuggerPlugin = class(TUMLPlugInFunction)
  protected
    function GetMenuItemName: String; override;
    function GetPlugInType: TPlugInType; override;
    function GetImageResourceName: String; override;
    function GetImageMaskColor: TColor; override;
  public
    procedure Execute(Context: IUMLModelPlugInContext); override;
  end;

implementation

{$R BoldUMLModelEditorDebuggerPlugin.res}

uses
  SysUtils,
  BoldUtils,
  BoldModel;

var
  _UMLModelEditorDebuggerPlugin: TBoldUMLModelEditorDebuggerPlugin = nil;

{ TBoldUMLModelEditorDebuggerPlugin }

// Main method to invoke plug in functionality
procedure TBoldUMLModelEditorDebuggerPlugin.Execute(context: IUMLModelPlugInContext);
var
  UMLModel: TUMLModel;
begin
  UMLModel := Context.GetCurrentModelHandle.EnsuredUMLModel;
  TBoldSystemDebuggerFrm.CreateWithSystem(nil, UMLModel.BoldSystem).Show;
end;

// Mask color for bitmap
function TBoldUMLModelEditorDebuggerPlugin.GetImageMaskColor: TColor;
begin
  Result := clWhite;
end;

// Resource for menu and button icon
function TBoldUMLModelEditorDebuggerPlugin.GetImageResourceName: String;
begin
  result := 'UMLModelEditorDebugger';
end;

// Caption for menu and hint for button
function TBoldUMLModelEditorDebuggerPlugin.GetMenuItemName: String;
begin
  Result := 'Model Editor Debugger';
end;

// Type of tool
function TBoldUMLModelEditorDebuggerPlugin.GetPlugInType: TPlugInType;
begin
  Result := ptTool;
end;

initialization
  _UMLModelEditorDebuggerPlugin := TBoldUMLModelEditorDebuggerPlugin.Create(true);

finalization
  FreeAndNil(_UMLModelEditorDebuggerPlugin);

end.

