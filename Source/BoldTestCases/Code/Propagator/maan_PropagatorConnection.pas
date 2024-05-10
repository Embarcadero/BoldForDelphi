unit maan_PropagatorConnection;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  BoldAbstractPropagatorHandle,
  BoldPropagatorHandleCOM,
  BoldSubscription,
  BoldHandle,
  BoldClientHandles,
  BoldComClientHandles;

type
  TdmPropConnection = class(TDataModule)
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    BoldPropagatorHandleCOM1: TBoldPropagatorHandleCOM;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure EnsureDM;
  
var
  dmPropConnection: TdmPropConnection;

implementation

{$R *.DFM}
procedure EnsureDm;
begin
  if not Assigned(dmPropConnection) then
    dmPropConnection := TdmPropConnection.Create(nil);
end;



end.
