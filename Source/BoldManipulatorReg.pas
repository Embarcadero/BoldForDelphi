
{ Global compiler directives }
{$include bold.inc}
unit BoldManipulatorReg;

interface

procedure Register;

implementation

{$R BoldManipulators.res}

uses
  SysUtils,
  BoldUtils,
  Classes,
  BoldDefs,
  BoldIDEConsts,
  BoldManipulators;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_MISC, [TBoldManipulator]);
end;

procedure Register;
begin
    RegisterComponentsOnPalette;
end;

end.

