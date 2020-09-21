unit BoldXMLReg;

interface
uses
  Classes;

procedure Register;

implementation

uses
  BoldXMLProducers;

procedure Register;
begin
  RegisterComponents('Bold XML', [TBoldXMLProducer]); // do not localize
end;

end.

