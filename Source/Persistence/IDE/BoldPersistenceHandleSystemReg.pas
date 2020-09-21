unit BoldPersistenceHandleSystemReg;

interface

procedure Register;

implementation
{.$R BoldPersistenceHandleSystem.res}
uses
  SysUtils,
  Classes,
  BoldIdeConsts,
  BoldPersistenceHandleSystem;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldPersistenceHandleSystem]);
end;

end.
