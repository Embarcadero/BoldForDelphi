unit dMain;

interface

uses
  Classes,
  Controls,
  Forms,
  BoldHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldModel,
  BoldHandles,
  BoldSubscription,
  BoldSystemHandle, 
  BoldUMLModelLink, 
  BoldUMLRose98Link, 
  BoldAbstractModel,
  BoldActions,
  BoldDBActions,
  ActnList,
  BoldHandleAction, BoldIBDatabaseAction, DB, IBDatabase,
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
