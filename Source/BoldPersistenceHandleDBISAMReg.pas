
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleDBISAMReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleDBISAM.res}

uses
  SysUtils,
  Classes,
  BoldIDESupport,
  BoldVersionInfo,
  BoldDatabaseAdapterDBIsam,
  BoldPersistenceHandleDBISAM,
  BoldIDEConsts;  

procedure Register;
begin
  RemovePackageFromDisabledPackagesRegistry(format('Bold%d%d%sDOA', [
    BoldBuildVersionNumberMajor,
    BoldBuildVersionNumberMinor,
    BoldBuildTarget]));
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleDBISAM]);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterDBISAM]);
end;

end.
