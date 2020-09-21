unit BoldUMLNameTrimmer;

interface

uses
  Graphics,
  BoldUMLModel,
  BoldUMLPlugins,
  BoldUMLModelEditPlugIn,
  BoldUMLModelConverter,
  BoldLogHandler;

type
  { forward declaration of classes }
  TUMLNameFixer = class;

  { TUMLNameFixer }
  TUMLNameFixer = class(TUMLPlugInFunction)
  private
    procedure TrimName(UMLElement: TUMLModelElement);
    procedure TrimClassAndMembers(UMLClass: TUMLClass);
  protected
    function GetMenuItemName: String; override;
    function GetPlugInType: TPlugInType; override;
    function GetImageResourceName: String; override;
    function GetImageMaskColor: TColor; override;
  public
    procedure Execute(Context: IUMLModelPlugInContext); override;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

var
  _UMLNameFixer: TUMLNameFixer = nil;

{ TUMLNameFixer }

// Main method to invoke plug in functionality
procedure TUMLNameFixer.Execute(context: IUMLModelPlugInContext);
var
  UMLModel: TUMLModel;
  i: integer;
begin
  UMLModel := Context.GetCurrentModelhandle.EnsuredUMLModel;
  if Assigned(UMLModel) then
  begin
    BoldLog.StartLog('Trimming names');
    for i := 0 to UMLModel.Classes.Count - 1 do
      TrimClassAndMembers(UMLModel.Classes[i]);
    BoldLog.EndLog;
  end;
end;

// Mask color for bitmap
function TUMLNameFixer.GetImageMaskColor: TColor;
begin
  Result := clTeal;
end;

// Resource for menu and button icon
function TUMLNameFixer.GetImageResourceName: String;
begin
  result := 'NameTrimmer';
end;

// Caption for menu and hint for button
function TUMLNameFixer.GetMenuItemName: String;
begin
  Result := 'Name trimmer';
end;

// Type of tool
function TUMLNameFixer.GetPlugInType: TPlugInType;
begin
  Result := ptTool;
end;

// Trims name of class + all its members and association ends
procedure TUMLNameFixer.TrimClassAndMembers(UMLClass: TUMLClass);
var
  i: integer;
begin
  TrimName(UMLClass);

  for i := 0 to UMLClass.Feature.Count - 1 do
    TrimName(UMLClass.Feature[i]);

  for i := 0 to UMLClass.AssociationEnd.Count - 1 do
    TrimName(UMLClass.AssociationEnd[i]);
end;

// Trimmer of names
procedure TUMLNameFixer.TrimName(UMLElement: TUMLModelElement);
begin
  if UMLElement.Name <> Trim(UMLElement.Name) then
  begin
    UMLElement.Name := Trim(UMLElement.Name);
    BoldLog.LogFmt('Trimmed name of %s', [UMLElement.Name]);
  end;
end;

initialization
  _UMLNameFixer := TUMLNameFixer.Create(true);

finalization
  FreeAndNil(_UMLNameFixer);

end.
