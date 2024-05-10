
{ Global compiler directives }
{$include bold.inc}
unit BoldUMXMILinkreg;

interface                                                       

procedure Register;

implementation

{$R BoldUMXMILinkreg.res}

uses
  SysUtils,
  BoldUtils,
  Classes,
  DesignIntf,
  TypInfo,
  BoldDefs,
  BoldGuard,
  BoldIDEConsts,
  BoldAbstractPropertyEditors,
  BoldPropertyEditors,
  BoldAbstractModel,
  BoldUMLXMILink;

type

{---TBoldRose98FileNameProperty---}
  TBoldUMLXMIFileNameProperty = class(TBoldFileNameProperty)
  protected
    function IsValid: boolean; override;
    function FileFilter: string; override;
  end;

{---TBoldRose98FileNameProperty---}
function TBoldUMLXMIFileNameProperty.FileFilter: string;
begin
  Result := Format('%s (*%s)|*%1:s', [XMI_LINKDESC, XMI_LINKEXTENSION]);
end;

function TBoldUMLXMIFileNameProperty.IsValid: boolean;
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
  RegisterComponents(BOLDPAGENAME_MISC, [TBoldUMLXMILink]);
end;

procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(String), TBoldUMLXMILink, 'Filename', TBoldUMLXMIFileNameProperty);
  RegisterPropertyEditor(TypeInfo(TBoldAbstractModel), TBoldUMLXMILink, 'BoldModel', TBoldComponentPropertyIndicateMissing);
end;

procedure Register;
begin
    RegisterComponentsOnPalette;
    RegisterEditors;
end;

end.
