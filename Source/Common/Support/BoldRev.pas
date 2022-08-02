
{ Global compiler directives }
{$include bold.inc}
unit BoldRev;

interface

uses
  Classes;

function BoldBuildVersion: string;
function BoldProductNameLong: string;
function BoldProductNameShort: string;
function BoldProductVersion: string;

function BoldModuleVersions: TStringList;
procedure BoldRegisterModuleVersion(const Version: string);

implementation

uses
  SysUtils;

const
  Release = '<#Version Delimiter=.>';
  Comment = '<#Comments>';
  Timestamp = '<#DateTime "Format=yyyy-mm-dd">';
  ID_BUILD_VERSION = 1;
  ID_PRODUCT_VERSION = 2;
  ID_PRODUCT_NAME_SHORT = 3;
  ID_PRODUCT_NAME_LONG = 4;

var
  G_VersionList: TStringList;

function BoldBuildVersion: string;
begin
  Result := LoadStr(ID_BUILD_VERSION);
end;

function BoldProductNameLong: string;
begin
  Result := LoadStr(ID_PRODUCT_NAME_LONG);
end;

function BoldProductNameShort: string;
begin
  Result := LoadStr(ID_PRODUCT_NAME_SHORT);
end;

function BoldProductVersion: string;
begin
  Result := LoadStr(ID_PRODUCT_VERSION);
end;


function BoldModuleVersions: TStringList;
begin
  if not Assigned(G_VersionList) then
  begin
    G_VersionList := TStringList.Create;
    G_VersionList.Sorted := true;
    G_VersionList.Duplicates := dupError;
  end;
  Result := G_VersionList;
end;

procedure BoldRegisterModuleVersion(const Version: string);
begin
  BoldModuleVersions.Add(Version);
end;

initialization

finalization
  FreeAndNil(G_VersionList);

end.
