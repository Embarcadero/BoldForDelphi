unit BoldPersistenceHandleSQLDirectReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleSQLDirect.res}

uses
  SysUtils,
  Classes,
  BoldDatabaseAdapterSQLDirect,
  BoldPersistenceHandleSQLDirect,
  BoldIDESupport,
  BoldIDEConsts;

procedure Register;
begin
  RemovePackageFromDisabledPackagesRegistry(format('BoldSQLDirect%s', [LIBSUFFIX])); // do not localize
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleSQLDirect]);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterSQLDirect]);
end;

end.
