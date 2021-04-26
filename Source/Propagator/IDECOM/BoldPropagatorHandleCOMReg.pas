
{ Global compiler directives }
{$include bold.inc}
unit BoldPropagatorHandleCOMReg;

interface

procedure Register;

implementation

uses
  Classes,
  BoldGuard,
  BoldPropagatorHandleCOM,
  BoldIDEConsts;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_OSS_CMS, [TBoldPropagatorHandleCom]);
end;

procedure RegisterEditors;
begin
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
  RegisterEditors;
end;

end.
