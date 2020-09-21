unit BoldXMLDispatcherReg;

interface

procedure Register;

implementation

uses
  BoldXMLDispatcher,
  BoldXMLDispatcherEditor,
  DesignIntf,
  Classes;

procedure Register;
begin
  RegisterComponents('Bold XML', [TBoldXMLDispatcher]); // do not localize
  RegisterComponentEditor(TBoldXMLDispatcher, TBoldXMLDispatcherEditor);
end;

end.
