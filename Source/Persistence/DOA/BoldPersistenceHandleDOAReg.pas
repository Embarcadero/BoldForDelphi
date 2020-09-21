unit BoldPersistenceHandleDOAReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleDOA.res}

uses
  Classes,
  SysUtils,
  BoldDatabaseAdapterDOA,
  BoldPersistenceHandleDOA,
  BoldIDESupport,
  BoldIDEConsts;

procedure Register;
begin
  RemovePackageFromDisabledPackagesRegistry(format('BoldDOA%s', [LIBSUFFIX])); // do not localize
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleDOA]);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterDOA]);
end;

end.
