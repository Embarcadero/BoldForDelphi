
{ Global compiler directives }
{$include bold.inc}
unit BoldXMLReg;

interface
uses
  Classes;

procedure Register;

implementation

uses
  BoldGuard,
  BoldXMLProducers;

procedure Register;
begin
  RegisterComponents('Bold XML', [TBoldXMLProducer]);
end;

end.