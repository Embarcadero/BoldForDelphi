
{ Global compiler directives }
{$include bold.inc}
unit BoldXMLDispatcherReg;

interface

procedure Register;

implementation

uses
  BoldXMLDispatcher,
  BoldXMLDispatcherEditor,
  DesignIntf,
  BoldGuard,
  Classes;

procedure Register;
begin
  RegisterComponents('Bold XML', [TBoldXMLDispatcher]);
  RegisterComponentEditor(TBoldXMLDispatcher, TBoldXMLDispatcherEditor);
end;

end.
