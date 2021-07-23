
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceHandleDBXReg;

interface

procedure Register;

implementation

uses
  Classes,
  BoldPersistenceHandleDBX,
  BoldAbstractDatabaseAdapter,
  BoldIndexCollection,
  BoldDatabaseAdapterDBX,
  BoldIDEConsts, ColnEdit, DesignIntf;
type

  TBoldIndexCollectionProperty = class(TCollectionProperty)
  end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TBoldIndexCollection), TBoldAbstractDatabaseAdapter, 'CustomIndexes', TBoldIndexCollectionProperty);  // Should really be registered in own unit
  RegisterComponents(BOLDPAGENAME_DEPRECATED, [TBoldPersistenceHandleDBX]);
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [TBoldDatabaseAdapterDBX]);
end;

end.
