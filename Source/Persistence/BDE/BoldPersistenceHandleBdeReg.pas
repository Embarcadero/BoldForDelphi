
{ Global compiler directives }
{$include bold.inc}
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
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleBDE]);
  RegisterPropertyEditor(TypeInfo(string), TBoldPersistenceHandleBDE, 'DatabaseName', TBoldDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TBoldPersistenceHandleBDE, 'SessionName', TBoldSessionNameProperty);
end;

end.
