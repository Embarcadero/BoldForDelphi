unit BoldXMLDispatcherVBReg;

interface

procedure Register;

implementation

uses
  BoldXMLDispatcherVB,
  Classes;

procedure Register;
begin
  RegisterComponents('Bold XML', [TBoldXMLDispatcherVB]); // do not localize
end;

end.
