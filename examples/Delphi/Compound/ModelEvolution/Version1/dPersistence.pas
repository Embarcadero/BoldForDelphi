unit dPersistence;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldAbstractObjectUpgraderHandle, BoldObjectUpgraderHandle,
  BoldSubscription, BoldHandle, BoldPersistenceHandle,
  BoldPersistenceHandleDB, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TdmPersistence = class(TDataModule)
    ObjectUpgrader: TBoldObjectUpgraderHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmPersistence: TdmPersistence;

implementation

uses dModel, dSystemTypeInfo;

{$R *.DFM}

end.
