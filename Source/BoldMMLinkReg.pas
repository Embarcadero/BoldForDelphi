
{ Global compiler directives }
{$include bold.inc}
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
  BoldUtils,
  BoldDefs,
  BoldGuard,
  BoldIDEConsts,
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
  Result := Format('%s (*%s)|*%1:s', [MM_LINKDESC, MM_LINKEXTENSION]);
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
  RegisterPropertyEditor(TypeInfo(String), TBoldUMLMMLink, 'Filename', TBoldUMLMMFileNameProperty);
  RegisterPropertyEditor(TypeInfo(TBoldAbstractModel), TBoldUMLMMLink, 'BoldModel', TBoldComponentPropertyIndicateMissing);
end;

procedure Register;
begin
    RegisterComponentsOnPalette;
    RegisterEditors;
end;

end.
