unit BoldPersistenceNotifierReg;

interface

procedure Register;

implementation

uses
  DesignIntf,
  Classes,
  BoldPersistenceNotifier,
  BoldPersistenceHandle,
  BoldAbstractPropertyEditors,
  BoldIDEConsts;

{.$R *.res}

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE,
  [
      TBoldPersistenceNotifier,
      TBoldPersistenceProgressNotifier
  ]);
end;

procedure RegisterEditors;
begin
  RegisterPropertyEditor(TypeInfo(TBoldPersistenceHandle), TBoldAbstractPersistenceNotifier, 'PersistenceHandle', TBoldComponentPropertyIndicateMissing); // do not localize
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
  RegisterEditors;
end;

end.
