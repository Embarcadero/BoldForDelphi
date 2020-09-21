unit BoldSOAPClientPersistenceHandles;

interface

uses
  BoldAbstractComClientPersistenceHandles,
  BoldAbstractModel,
  BoldPersistenceController;

type
  TBoldSOAPClientPersistenceHandle = class(TBoldAbstractComClientPersistenceHandle)
  private
    fModel: TBoldAbstractModel;
  protected
    function CreatePersistenceController: TBoldPersistenceController; override;
  published
    property BoldModel: TBoldAbstractModel read fModel write fModel;
  end;

implementation

uses
  BoldSOAPPersistenceControllerProxy;

{ TBoldComClientPersistenceHandle }

function TBoldSOAPClientPersistenceHandle.CreatePersistenceController: TBoldPersistenceController;
var
  Controller: TBoldSOAPPersistenceControllerProxy;
begin
  Controller := TBoldSOAPPersistenceControllerProxy.Create(fModel.MoldModel);
  if Initialized and Active then
    Controller.Connect(ConnectionHandle.BoldProvider, ObjectName);
  result := Controller;
end;

end.
