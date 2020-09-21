unit dPersistence;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldAbstractObjectUpgraderHandle, BoldObjectUpgraderHandle,
  BoldSubscription, BoldHandle, BoldPersistenceHandle,
  BoldSystem,
  BoldPersistenceHandleDB, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TdmPersistence = class(TDataModule)
    ObjectUpgrader: TBoldObjectUpgraderHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    procedure ObjectUpgrader_Person_UpgradeObject(Obj: TBoldObject);
    procedure ObjectUpgrader_Order_UpgradeObject(Obj: TBoldObject);
    procedure ObjectUpgrader_Product_UpgradeObject(Obj: TBoldObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmPersistence: TdmPersistence;

implementation

uses dModel, ModelEvClasses, dSystemTypeInfo;

{$R *.DFM}

procedure TdmPersistence.ObjectUpgrader_Person_UpgradeObject(Obj: TBoldObject);
begin
  (Obj as TPerson).Upgrade;
end;

procedure TdmPersistence.ObjectUpgrader_Order_UpgradeObject(Obj: TBoldObject);
begin
  (Obj as TOrder).Upgrade;
end;

procedure TdmPersistence.ObjectUpgrader_Product_UpgradeObject(Obj: TBoldObject);
begin
  (Obj as TProduct).Upgrade;
end;

end.
