unit BoldObjectUpgraderHandleReg;

interface

procedure Register;

implementation

{.$R *.res}

uses
  classes,
  BoldObjectUpgraderHandle,
  BoldIDEConsts;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldObjectUpgraderHandle]);
end;

end.

