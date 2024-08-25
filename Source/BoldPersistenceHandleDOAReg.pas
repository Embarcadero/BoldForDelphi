
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleDOAReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleDOA.res}

uses
  Classes,
  SysUtils,
  BoldPersistenceHandleDOA,
  BoldIDESupport,
  BoldVersionInfo,
  BoldDatabaseAdapterDOA,
  BoldIDEConsts;

procedure Register;
begin
  RemovePackageFromDisabledPackagesRegistry(format('Bold%d%d%sDOA', [
    BoldBuildVersionNumberMajor,
    BoldBuildVersionNumberMinor,
    BoldBuildTarget]));
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleDOA]);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterDOA]);
end;

end.
