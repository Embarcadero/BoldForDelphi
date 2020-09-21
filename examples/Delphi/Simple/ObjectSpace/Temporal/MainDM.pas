unit MainDM;

interface

uses
  Classes, 
  Controls, 
  Forms, 
  BoldPersistenceHandle, 
  BoldPersistenceHandleDB, 
  BoldHandle, 
  BoldUMLModelLink, 
  BoldUMLRose98Link, 
  BoldHandles,
  BoldSystemHandle, 
  BoldSubscription, 
  BoldModel, 
  BoldAbstractModel, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB;

type
  TdmMain = class(TDataModule)
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldUMLRoseLink1: TBoldUMLRoseLink;
    BoldModel1: TBoldModel;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
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
