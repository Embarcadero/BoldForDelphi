
{ Global compiler directives }
{$include bold.inc}
unit BoldObjectUpgraderHandleReg;

interface

procedure Register;

implementation

{$R BoldObjectUpgraderHandleReg.res}

uses
  classes,
  BoldObjectUpgraderHandle,
  BoldIDEConsts;

procedure Register;
begin
    RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldObjectUpgraderHandle]);
end;

end.
