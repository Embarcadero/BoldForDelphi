
{ Global compiler directives }
{$include bold.inc}
unit BoldComElementHandleReg;

interface

procedure Register;

implementation

uses
  Classes,
  BoldComServerElementHandles;

procedure Register;
begin
  RegisterComponents('Bold COM',[TBoldComServerElementHandle]);
end;

end.
