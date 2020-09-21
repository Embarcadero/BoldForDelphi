unit BoldPersistenceHandleDBXReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleDBX.res}

uses
  Classes,
  BoldPersistenceHandleDBX,
  BoldDatabaseAdapterDBX,
  BoldIDEConsts;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleDBX]);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterDBX]);
end;

end.
