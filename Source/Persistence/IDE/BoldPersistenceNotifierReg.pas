
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceNotifierReg;

interface

procedure Register;

implementation

{$R *.res}

uses
  DesignIntf,
  Classes,
  BoldPersistenceNotifier,
  BoldPersistenceHandle,
  BoldAbstractPropertyEditors,
  BoldIDEConsts;


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
  RegisterPropertyEditor(TypeInfo(TBoldPersistenceHandle), TBoldAbstractPersistenceNotifier, 'PersistenceHandle', TBoldComponentPropertyIndicateMissing);
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
  RegisterEditors;
end;

end.
