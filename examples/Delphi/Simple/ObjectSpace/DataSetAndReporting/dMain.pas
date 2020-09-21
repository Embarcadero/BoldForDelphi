unit dMain;

interface

uses
  Classes,
  Controls,
  Forms,
  ActnList,
  BoldSubscription,
  BoldHandle,
  BoldHandles,
  BoldSystemHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldModel,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldActions,
  BoldHandleAction,
  BoldDBActions, BoldAbstractModel, BoldIBDatabaseAction, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TdmMain = class(TDataModule)
    bshMain: TBoldSystemHandle;
    stiMain: TBoldSystemTypeInfoHandle;
    bmoMain: TBoldModel;
    BoldUMLRoseLink1: TBoldUMLRoseLink;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmMain: TdmMain;

implementation

{$R *.DFM}

end.
