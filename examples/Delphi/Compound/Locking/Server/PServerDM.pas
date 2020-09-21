unit PServerDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldPersistenceHandlePassthrough, BoldSnooperHandle,
  BoldPersistenceHandle, BoldPersistenceHandleDB, 
  BoldComServerHandles, BoldSubscription, BoldHandle, BoldServerHandles,
  BoldSOAPServerPersistenceHandles, ModelDM, BoldAbstractPropagatorHandle,
  BoldPropagatorHandleCOM, BoldClientHandles, BoldComClientHandles,
  BoldAbstractLockManagerHandle, BoldLockManagerHandleCom, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB, BoldPersistenceHandlePTWithModel;

type
  TdmPServer = class(TDataModule)
    BoldSOAPServerPersistenceHandle1: TBoldSOAPServerPersistenceHandle;
    BoldComServerHandle1: TBoldComServerHandle;
    BoldSnooperHandle1: TBoldSnooperHandle;
    BoldPropagatorHandleCOM1: TBoldPropagatorHandleCOM;
    BoldLockManagerHandleCom1: TBoldLockManagerHandleCom;
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    procedure IBDatabase1BeforeConnect(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    { Public declarations }
  end;

var
  dmPServer: TdmPServer;

implementation

{$R *.DFM}

procedure TdmPServer.Loaded;
begin
  inherited;
  // Due to timing-problems, it is better to activate the persistencehandles here in Loaded instead of Datamodulecreate.
  // The first client to connect might try to retrieve the com-interface to the persistencehandle before DataModuleCreate.
  BoldPersistenceHandleDB1.Active := True;
  BoldSOAPServerPersistenceHandle1.Active := True;
end;

procedure TdmPServer.IBDatabase1BeforeConnect(Sender: TObject);
begin
  IBDatabase1.DatabaseName := 'localhost:' + ExtractFilePath(Application.ExeName) + ExtractFileName(IBDatabase1.DatabaseName);
end;

end.
