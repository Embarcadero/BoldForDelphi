
{ Global compiler directives }
{$include bold.inc}
unit BoldHTTPServerPersistenceHandlePassthroughReg;

interface

procedure Register;

implementation

uses
  BoldIDEConsts,
  BoldGuard,
  BoldHTTPServerPersistenceHandlePassthrough,
  Classes;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldHTTPServerPersistenceHandlePassthrough]);
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
end;

end.
