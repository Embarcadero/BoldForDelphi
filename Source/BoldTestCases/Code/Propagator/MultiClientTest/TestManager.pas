unit TestManager;

interface

uses
  comobj,
  syncobjs,
  TestClient_TLB,
  BoldPropagatorConstants;

type
  TReceiveEvent = procedure (const Event: string; CliendID: integer) of object;

  TTestManager = class(TAutoIntfObject, IPropagatorTestManager)
  private
    fOnReceiveEvent: TReceiveEvent;
  public
    constructor Create;
    {IPropagatorTestManager}
    function  EventReceived(const Event: WideString; TestClientID: Integer): HResult; stdcall;
    property OnReceiveEvent: TReceiveEvent read fOnReceiveEvent write fOnReceiveEvent;
  end;

implementation

uses
  windows,
  ActiveX,
  BoldDefs;

{ TestManager }

function TTestManager.EventReceived(const Event: WideString;
  TestClientID: Integer): HResult;
begin
  //FIXME!!
  if Assigned(fOnReceiveEvent) then
    fOnReceiveEvent(Event, TestClientID);
  Result := S_OK;
end;


constructor TTestManager.Create;
var
  Tlb: ITypeLib;
begin
  LoadRegTypeLib(LIBID_TestClient, 1, 0, 0, Tlb);
  if Assigned(Tlb) then
    inherited Create(Tlb, IPropagatorTestManager)
  else
    raise EBold.CreateFmt('%s.Create: Type Library unassigned', [ClassName]);
end;

end.
