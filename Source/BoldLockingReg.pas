
{ Global compiler directives }
{$include bold.inc}
unit BoldLockingReg;

interface

procedure Register;

implementation

{$R BoldLockingReg.res}

uses
  SysUtils,
  BoldUtils,
  Classes,
  BoldDefs,
  BoldIDEConsts,
  BoldLockingHandles;


procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_OSS_CMS, [TBoldLockingHandle]);
end;

procedure Register;
begin
    RegisterComponentsOnPalette;
end;

end.

