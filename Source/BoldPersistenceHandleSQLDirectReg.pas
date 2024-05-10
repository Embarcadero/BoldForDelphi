
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleSQLDirectReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleSQLDirect.res}

uses
  SysUtils,
  Classes,
  BoldIDESupport,
  BoldVersionInfo,
  BoldDatabaseAdapterSQLDirect,
  BoldPersistenceHandleSQLDirect,
  BoldIDEConsts;    

procedure Register;
begin
  RemovePackageFromDisabledPackagesRegistry(format('Bold%d%d%sSQLDirect', [
    BoldBuildVersionNumberMajor,
    BoldBuildVersionNumberMinor,
    BoldBuildTarget]));
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleSQLDirect]);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterSQLDirect]);
end;

initialization

end.
