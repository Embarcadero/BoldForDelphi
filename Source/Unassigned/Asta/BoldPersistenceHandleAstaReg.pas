unit BoldPersistenceHandleAstaReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleAsta.res}

uses
  SysUtils,
  BoldUtils,
  DesignIntf,
  Classes,
  BoldAstaInterfaces,
  BoldPropertyEditors,
  BoldDefs,
  BoldGuard,
  BoldPersistenceHandleAsta,
  BoldIDEConsts;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldPersistenceHandleAsta]);
end;

end.
