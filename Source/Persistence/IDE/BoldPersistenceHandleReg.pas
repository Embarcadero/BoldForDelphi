
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleReg;

interface

procedure Register;

implementation

uses
  DesignIntf,
  BoldAbstractPropertyEditors,
  BoldAbstractModel,
  BoldPersistenceHandle,
  BoldPersistenceHandlePassthrough;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TBoldAbstractModel), TBoldPersistenceHandle, 'BoldModel', TBoldComponentPropertyIndicateMissing);
  RegisterPropertyEditor(TypeInfo(TBoldPersistenceHandle), TBoldPersistenceHandlePassthrough, 'NextPersistenceHandle', TBoldComponentPropertyIndicateMissing);
end;

end.
