unit BoldMMLinkreg;

interface

procedure Register;

implementation

uses
  Classes,
  DesignIntf,
  TypInfo,
  SysUtils,
  BoldIDESupport,
  BoldIDEConsts,
  BoldUtils,
  BoldDefs,
  BoldAbstractPropertyEditors,
  BoldPropertyEditors,
  BoldAbstractModel,
  BoldUMLMMLink;

{$R *.res}

type
  { forward declarations }
  TBoldUMLMMFileNameProperty = class;

  { TBoldUMLMMFileNameProperty }
  TBoldUMLMMFileNameProperty = class(TBoldFileNameProperty)
  protected
    function IsValid: boolean; override;
    function FileFilter: string; override;
  end;

{ TBoldUMLMMFileNameProperty }
function TBoldUMLMMFileNameProperty.FileFilter: string;
begin
  Result := Format('%s (*%s)|*%1:s', [MM_LINKDESC, MM_LINKEXTENSION]); //do not localize
end;

function TBoldUMLMMFileNameProperty.IsValid: boolean;
var
  SelectedPersistent: TPersistent;
  PropertyValue: string;
  i: integer;
begin
  Result := True;
  for i := 0 to PropCount - 1 do
  begin
    SelectedPersistent := GetComponent(i);
    PropertyValue := GetStrProp(SelectedPersistent, GetName);
    Result := Result and (PropertyValue <> '');
  end;
end;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_MISC, [TBoldUMLMMLink]);
end;

procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(String), TBoldUMLMMLink, 'Filename', TBoldUMLMMFileNameProperty); //do not localize
  RegisterPropertyEditor(TypeInfo(TBoldAbstractModel), TBoldUMLMMLink, 'BoldModel', TBoldComponentPropertyIndicateMissing); //do not localize
end;

procedure Register;
begin
  RemovePackageFromDisabledPackagesRegistry(format('BoldMMLink%s', [LIBSUFFIX])); // do not localize
  RegisterComponentsOnPalette;
  RegisterEditors;
end;

end.
