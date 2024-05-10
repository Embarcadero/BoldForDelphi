
{ Global compiler directives }
{$include bold.inc}
unit BoldSOAPPersistenceControllerStub;

interface

uses
  ActiveX,
  BoldComAdapter,
  BoldPersistenceController,
  BoldMeta,
  BoldSOAP_TLB,
  BoldPersistenceControllerSOAPAdapterCore;

type
  { forward declarations }
  TBoldSOAPPersistenceControllerAdapter = class;

  {-- TBoldSOAPPersistenceControllerAdapter --}
  TBoldSOAPPersistenceControllerAdapter = class(TBoldComAdapter, IBoldSOAPService)
  private
    fAdapterCore: TBoldPersistenceControllerSOAPAdapterCore;
    function GetPersistenceController: TBoldPersistenceController;
    procedure Get(const request: WideString; out reply: WideString); safecall;
  public
    constructor Create(Model: TMoldModel; Adaptee: TObject; Owner: Boolean; const TypeLib: ITypeLib;
      const DispIntf: TGUID); reintroduce;
    destructor Destroy; override;
    property PersistenceController: TBoldPersistenceController read GetPersistenceController;
  end;

implementation

uses
  sysutils,
  BoldComUtils;

{ TBoldSOAPPersistenceControllerAdapter }

constructor TBoldSOAPPersistenceControllerAdapter.Create(Model: TMoldModel; Adaptee: TObject;
  Owner: Boolean; const TypeLib: ITypeLib; const DispIntf: TGUID);
var
  aTypeLibrary: ITypeLib;
begin
  fAdapterCore := TBoldPersistenceControllerSOAPAdapterCore.Create(Model);

  if Failed(LoadRegTypeLib(LIBID_BoldSOAP, BoldSOAPMajorVersion, BoldSOAPMinorVersion, 0, aTypeLibrary)) then
    raise EBoldCom.CreateFmt('%s.Create: Cannot load type library', [classname]);
  inherited Create(Adaptee, Owner, aTypeLibrary, IBoldSOAPService);
end;

destructor TBoldSOAPPersistenceControllerAdapter.Destroy;
begin
  FreeAndNil(fAdapterCore);
  inherited;
end;

procedure TBoldSOAPPersistenceControllerAdapter.Get(
  const request: WideString; out reply: WideString);
begin
  fAdapterCore.Get(request, reply, Adaptee as TBoldPersistenceController);
end;

function TBoldSOAPPersistenceControllerAdapter.GetPersistenceController: TBoldPersistenceController;
begin
  result := Adaptee as TBoldPersistenceController;
end;

end.
