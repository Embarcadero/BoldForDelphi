unit BoldIDESupport;

interface

procedure RemovePackageFromDisabledPackagesRegistry(PackageName: String);
function IDEBaseRegistryKey: string;

implementation

uses
  Classes,
  Registry,
  ToolsAPI;

function IDEBaseRegistryKey: string;
var
	Service: IOTAServices;
begin
	service := (BorlandIDEServices as IOTAServices);
	Assert(Assigned(Service), 'Service not assigned');
	Result := Service.GetBaseRegistryKey + '\';
end;

procedure RemovePackageFromDisabledPackagesRegistry(PackageName: String);
var
  Regkey: TRegistry;
  i: integer;
  Values: TStringList;
begin
  RegKey := TRegistry.Create;
  try
	  if Regkey.OpenKey(IDEBaseRegistryKey + 'Disabled Packages', false) then // do not localize
	  begin
	    Values := TStringList.Create;
	    try
		    RegKey.GetValueNames(Values);

		    for i := 0 to Values.Count - 1 do
		      if pos(PackageName, Values[i]) <> 0 then
		        RegKey.DeleteValue(Values[i]);
		  finally
	    	Values.Free;
	    end;
	  end;
	finally
	  RegKey.Free;
	end;
end;

end.
