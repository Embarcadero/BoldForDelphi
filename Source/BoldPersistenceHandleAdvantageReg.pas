
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleAdvantageReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleAdvantage.res}

uses
  SysUtils,
  Classes,
  registry,
  BoldIDESupport,
  BoldVersionInfo,
  BoldDatabaseAdapterAdvantage,
  BoldPersistenceHandleAdvantage,
  BoldIDEConsts;

procedure Register;
begin
  RemovePackageFromDisabledPackagesRegistry(format('Bold%d%d%sAdvantage', [
    BoldBuildVersionNumberMajor,
    BoldBuildVersionNumberMinor,
    BoldBuildTarget]));
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleAdvantage]);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterAdvantage]);
end;

end.
