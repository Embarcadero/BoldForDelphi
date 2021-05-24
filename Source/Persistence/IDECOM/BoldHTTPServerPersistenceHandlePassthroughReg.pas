
{ Global compiler directives }
{$include bold.inc}
unit BoldHTTPServerPersistenceHandlePassthroughReg;

interface

procedure Register;

implementation

{$R BoldHTTPServerPersistenceHandlePassthroughReg.res}

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
