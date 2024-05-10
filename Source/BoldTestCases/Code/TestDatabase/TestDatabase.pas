unit TestDatabase;



interface

uses
  BoldSystemHandle,
  BoldAbstractPersistenceHandleDB;

procedure CreateDB(BoldSystemHandle1: TBoldSystemHandle; ABoldPersistenceHandleDB: TBoldAbstractPersistenceHandleDB);

implementation

procedure CreateDB(BoldSystemHandle1: TBoldSystemHandle; ABoldPersistenceHandleDB: TBoldAbstractPersistenceHandleDB);
begin
  ABoldPersistenceHandleDB.CreateDataBaseSchema;
  BoldSystemHandle1.Active := true;
end;


end.
 
