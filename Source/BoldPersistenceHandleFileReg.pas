
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleFileReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleFile.res}

uses
  SysUtils,
  Classes,
  TypInfo,
  DesignIntf,
  BoldPersistenceHandleFileXML,
  BoldIDEConsts,
  BoldPropertyEditors;

type
{---TBoldRose98FileNameProperty---}
  TBoldXMLFileNameProperty = class(TBoldFileNameProperty)
  protected
    function IsValid: boolean; override;
    function FileFilter: string; override;
  end;

{---TBoldRose98FileNameProperty---}
function TBoldXMLFileNameProperty.FileFilter: string;
begin
  Result := Format('%s (*.%s)|*%1:s', [XML_LINKDESC, XML_LINKEXTENSION]);
end;

function TBoldXMLFileNameProperty.IsValid: boolean;
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

procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(String), TBoldPersistenceHandleFileXML, 'Filename', TBoldXMLFileNameProperty);
end;

procedure Register;
begin
    RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldPersistenceHandleFileXML]);
    RegisterEditors;
end;

end.
