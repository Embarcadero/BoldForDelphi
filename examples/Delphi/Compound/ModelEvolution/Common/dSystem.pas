unit dSystem;

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
  BoldActions,
  ActnList,
  BoldHandleAction, BoldAbstractObjectUpgraderHandle,
  BoldObjectUpgraderHandle, BoldDBActions, BoldIBDatabaseAction;

type
  TdmSystem = class(TDataModule)
    SystemHandle: TBoldSystemHandle;
    ActionList1: TActionList;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmSystem: TdmSystem;

implementation

uses dSystemTypeInfo, dPersistence;

{$R *.DFM}

end.
