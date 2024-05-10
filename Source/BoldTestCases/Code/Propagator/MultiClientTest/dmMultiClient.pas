unit dmMultiClient;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, BoldHandleAction, BoldActions, BoldDBActions, BoldHandles, BoldSystemHandle,
  BoldRootedHandles, BoldAbstractListHandle, BoldCursorHandle,
  BoldListHandle, BoldIDAdderHandle, BoldListenerHandle,
  BoldAbstractDequeuer, BoldExternalObjectSpaceEventHandler,
  BoldPersistenceHandlePassthrough, BoldSnooperHandle, BoldSubscription,
  BoldHandle, BoldPersistenceHandle, BoldPersistenceHandleDB,
  dmModel1, BoldClientHandles, BoldComClientHandles,
  BoldAbstractPropagatorHandle, BoldPropagatorHandleCOM, DB,
  BoldAbstractDatabaseAdapter,
  BoldAbstractPersistenceHandleDB, BoldPersistenceHandlePTWithModel, BoldDatabaseAdapterUniDAC, DBAccess, Uni,
  System.Actions;

type
  Tdm = class(TDataModule)
    BoldSnooperHandle1: TBoldSnooperHandle;
    BoldExternalObjectSpaceEventHandler1: TBoldExternalObjectSpaceEventHandler;
    BoldListenerHandle1: TBoldListenerHandle;
    BoldIdAdderHandle1: TBoldIdAdderHandle;
    BoldListHandle1: TBoldListHandle;
    BoldSystemHandle1: TBoldSystemHandle;
    ActionList1: TActionList;
    blhClassA: TBoldListHandle;
    blhThing: TBoldListHandle;
    DirectBoldHandle: TBoldSystemHandle;
    blhhitList: TBoldListHandle;
    blhSong: TBoldListHandle;
    BoldPropagatorHandleCOM1: TBoldPropagatorHandleCOM;
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    UniConnection1: TUniConnection;
    BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure EnsureDM;
    class procedure EnsureDB;
  end;

var
  dm: Tdm;

implementation

{$R *.DFM}

class procedure TDm.EnsureDB;
begin
  if not dm.BoldSystemHandle1.Active then
  begin
    dm.BoldIBAliasAction1.ForceRegenerateAliasAndSchema;
    dm.BoldSystemHandle1.Active := True;
  end;
end;

class procedure TDm.EnsureDM;
begin
  Ensuredm_Model;
  if not Assigned(dm) then
    Application.CreateForm(Tdm, dm);
  EnsureDB;
end;

procedure Tdm.DataModuleCreate(Sender: TObject);
begin
//  BoldIBAliasAction1.ServerName := ExtractFilePath(ParamStr(0)) + BoldIBAliasAction1.AliasName + '.gdb';
end;

end.
