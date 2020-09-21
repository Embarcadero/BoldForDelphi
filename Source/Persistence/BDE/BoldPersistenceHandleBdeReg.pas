unit BoldPersistenceHandleBDEReg;

interface

procedure Register;

implementation

{$R BoldPersistenceHandleBDE.res}

uses
  Classes,
  DesignIntf,
  TypInfo,
  BoldPersistenceHandleBDE,
  BoldPersistenceHandleBDEPropertyEditors,
  BoldDatabaseAdapterBDE,
  BoldIDEConsts;

procedure Register;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterBDE]);
  {$WARNINGS OFF}
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleBDE]);
  RegisterPropertyEditor(TypeInfo(string), TBoldPersistenceHandleBDE, 'DatabaseName', TBoldDatabaseNameProperty); // do not localize
  RegisterPropertyEditor(TypeInfo(string), TBoldPersistenceHandleBDE, 'SessionName', TBoldSessionNameProperty); // do not localize
  {$WARNINGS ON}
end;

end.
