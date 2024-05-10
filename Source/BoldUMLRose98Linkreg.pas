
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldUMLRose98Linkreg;

interface                                                       

procedure Register;

implementation

{$R *.res}

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
  BoldUMLRose98Link;

type

{ TBoldRoseFileNameProperty }
  TBoldUMLRoseFileNameProperty = class(TBoldFileNameProperty)
  protected
    function IsValid: boolean; override;
    function FileFilter: string; override;
  end;

{ TBoldRose98FileNameProperty }
function TBoldUMLRoseFileNameProperty.FileFilter: string;
begin
  Result := Format('%s (*%s)|*%1:s', [ROSE_LINKDESC, ROSE_LINKEXTENSION]);
end;

function TBoldUMLRoseFileNameProperty.IsValid: boolean;
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
  RegisterComponents(BOLDPAGENAME_MISC, [TBoldUMLRoseLink]);
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldUMLRose98Link]);
end;

procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(String), TBoldUMLRoseLink, 'Filename', TBoldUMLRoseFileNameProperty);
  RegisterPropertyEditor(TypeInfo(TBoldAbstractModel), TBoldUMLRoseLink, 'BoldModel', TBoldComponentPropertyIndicateMissing);
end;

procedure Register;
begin
    RegisterComponentsOnPalette;
    RegisterEditors;
end;

end.
