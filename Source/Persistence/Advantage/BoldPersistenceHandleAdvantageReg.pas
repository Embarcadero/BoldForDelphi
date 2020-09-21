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
  BoldDatabaseAdapterAdvantage,
  BoldPersistenceHandleAdvantage,
  BoldIDEConsts;

procedure Register;
begin
  RemovePackageFromDisabledPackagesRegistry(format('BoldAdvantage%s', [LIBSUFFIX])); // do not localize
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleAdvantage]);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterAdvantage]);
end;

end.
