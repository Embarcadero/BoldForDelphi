unit BoldPersistenceHandleReg;

interface

procedure Register;

implementation

uses
  SysUtils,
  DesignIntf,
  BoldAbstractPropertyEditors,
  BoldAbstractModel,
  BoldPersistenceHandle,
  BoldPersistenceHandlePassthrough;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TBoldAbstractModel), TBoldPersistenceHandle, 'BoldModel', TBoldComponentPropertyIndicateMissing); // do not localize
  RegisterPropertyEditor(TypeInfo(TBoldPersistenceHandle), TBoldPersistenceHandlePassthrough, 'NextPersistenceHandle', TBoldComponentPropertyIndicateMissing); // do not localize
end;

end.
