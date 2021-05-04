
{ Global compiler directives }
{$include bold.inc}
unit BoldComPersistenceHandleReg;

interface

procedure Register;

implementation

{$R BoldComPersistenceHandleReg.Res}

uses
  Classes,
  DesignIntf,
  BoldIDEConsts,
  BoldSOAPServerPersistenceHandles,
  BoldSOAPClientPersistenceHandles,
  BoldObjectNamePropertyEditor
  ;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE,[
    TBoldSOAPClientPersistenceHandle,
    TBoldSOAPServerPersistenceHandle]);
  RegisterPropertyEditor(TypeInfo(String), TBoldSOAPClientPersistenceHandle, 'Objectname', TBoldObjectNameProperty);
end;

end.
