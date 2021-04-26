
{ Global compiler directives }
{$include bold.inc}
unit BoldHTTPClientPersistenceHandleReg;

interface

procedure Register;

implementation

uses
  BoldIDEConsts,
  BoldGuard,
  BoldHTTPClientPersistenceHandle,
  Classes;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldHTTPClientPersistenceHandle]);
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
end;

end.
