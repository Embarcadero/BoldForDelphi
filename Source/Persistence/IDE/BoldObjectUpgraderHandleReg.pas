
{ Global compiler directives }
{$include bold.inc}
unit BoldObjectUpgraderHandleReg;

interface

procedure Register;

implementation

uses
  classes,
  BoldObjectUpgraderHandle,
  BoldIDEConsts;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldObjectUpgraderHandle]);
end;

end.
