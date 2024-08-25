
{ Global compiler directives }
{$include bold.inc}
unit BoldIDESupport;

interface

procedure RemovePackageFromDisabledPackagesRegistry(PackageName: String);

implementation

uses
  BoldDefs,
  Classes,
  Registry;

procedure RemovePackageFromDisabledPackagesRegistry(PackageName: String);
var
  Regkey: TRegistry;
  i: integer;
  Values: TStringList;
begin
  RegKey := TRegistry.Create;
  if Regkey.OpenKey(BOLD_HOST_IDE_REGISTRYPATH + 'Disabled Packages', false) then
  begin
    Values := TStringList.Create;
    RegKey.GetValueNames(Values);

    for i := 0 to Values.Count - 1 do
      if pos(PackageName, Values[i]) <> 0 then
        RegKey.DeleteValue(Values[i]);
    Values.Free;
  end;
end;

end.
