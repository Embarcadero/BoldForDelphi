unit BoldPersistenceHandleDBISAMReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleDBISAM.res}

uses
  SysUtils,
  Classes,
  BoldDatabaseAdapterDBIsam,
  BoldPersistenceHandleDBISAM,
  BoldIDESupport,
  BoldIDEConsts;

procedure Register;
begin
  RemovePackageFromDisabledPackagesRegistry(format('BoldDBISAM%s', [LIBSUFFIX])); // do not localize
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleDBISAM]);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterDBISAM]);
end;

end.
