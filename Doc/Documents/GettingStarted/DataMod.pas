unit DataMod;

interface

uses
  SysUtils, Classes, DB, IBDatabase, BoldAFPPluggable, BoldUMLModelLink,
  BoldUMLRose98Link, BoldHandles, BoldAbstractModel, BoldModel,
  BoldSystemHandle, BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldSubscription, BoldHandle, BoldPersistenceHandle,
  BoldAbstractPersistenceHandleDB, BoldPersistenceHandleDB, BoldUMLMMLink;

type
  TDataModule2 = class(TDataModule)
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    IBDatabase1: TIBDatabase;
    BoldUMLMMLink1: TBoldUMLMMLink;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule2: TDataModule2;

implementation

{$R *.dfm}

end.
