unit dMain;

interface

uses
  Classes,
  Controls,
  Forms,
  ActnList,
  BoldHandle,
  BoldHandles,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldModel,
  BoldSubscription,
  BoldSystemHandle,
  BoldUMLModelLink,
  BoldUMLRose98Link,
  BoldHandleAction,
  BoldActions,
  BoldDBActions, BoldAbstractModel, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB, BoldIBDatabaseAction;

type
  TdmMain = class(TDataModule)
    bshMain: TBoldSystemHandle;
    stiMain: TBoldSystemTypeInfoHandle;
    bmoMain: TBoldModel;
    BoldUMLRoseLink1: TBoldUMLRoseLink;
    ActionList1: TActionList;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    procedure BoldIBAliasAction1SchemaGenerated(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmMain: TdmMain;

implementation

{$R *.DFM}

procedure TdmMain.BoldIBAliasAction1SchemaGenerated(Sender: TObject);
begin
  BoldActivateSystemAction1.ExecuteTarget(nil);
end;

end.
