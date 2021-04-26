
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleSystemReg;

interface

procedure Register;

implementation

uses
  Classes,
  BoldIdeConsts,
  BoldPersistenceHandleSystem;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldPersistenceHandleSystem]);
end;

end.
