unit LockManagerHandlesDataMod;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldClientHandles, BoldComClientHandles, BoldSubscription, BoldHandle,
  BoldAbstractLockManagerHandle, BoldLockManagerHandleCom,
  BoldLockingHandles;

type
  TdmLockManagerHandle = class(TDataModule)
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    LockManagerHandleCom: TBoldLockManagerHandleCom;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
//    LockManagerHandleCOM: TBoldLockManagerHandleCom;
  end;

var
  dmLockManagerHandle: TdmLockManagerHandle;

implementation

{$R *.DFM}

procedure TdmLockManagerHandle.DataModuleCreate(Sender: TObject);
begin
//  LockmanagerHandleCOM := TBoldLockmanagerHandleCom.Create(self);
//  LockmanagerHandleCOM.ConnectionHandle := BoldComConnectionHandle1;
end;

procedure TdmLockManagerHandle.DataModuleDestroy(Sender: TObject);
begin
//  FreeAndNil(LockManagerHandleCOM);
end;

end.
