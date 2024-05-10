
{ Global compiler directives }
{$include bold.inc}
unit BoldConcurrencyControlReg;

interface

procedure Register;

implementation

uses
  BoldLockManagerAdminHandleCom,
  BoldLockManagerHandleCom,
  BoldIDEConsts,
  Classes;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_OSS_CMS, [
    TBoldLockManagerAdminHandleCom,
    TBoldLockManagerHandleCom
  ]);
end;

procedure Register;
begin
    RegisterComponentsOnPalette;
end;

end.
