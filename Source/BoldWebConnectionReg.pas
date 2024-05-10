
{ Global compiler directives }
{$include bold.inc}
unit BoldWebConnectionReg;

interface

procedure Register;

implementation

{$R BoldWebConnectionReg.res}

uses
  BoldWebConnection,
  BoldIDEConsts,
  Classes;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_MISC, [TBoldWebConnection]);
end;

procedure Register;
begin
    RegisterComponentsOnPalette;
end;

end.

