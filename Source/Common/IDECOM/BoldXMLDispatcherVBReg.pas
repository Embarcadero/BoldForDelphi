
{ Global compiler directives }
{$include bold.inc}
unit BoldXMLDispatcherVBReg;

interface

procedure Register;

implementation

uses
  BoldXMLDispatcherVB,
  Classes,
  BoldGuard;

procedure Register;
begin
  RegisterComponents('Bold XML', [TBoldXMLDispatcherVB]);
end;

end.
