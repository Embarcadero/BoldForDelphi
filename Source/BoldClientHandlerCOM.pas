
{ Global compiler directives }
{$include bold.inc}
unit BoldClientHandlerCOM;

interface

uses
  comObj,
  sysutils,
  BoldUtils,
  BoldThreadSafeLog,
  BoldClientHandler,
  BoldPropagatorInterfaces_TLB,
  BoldThreadedComObjectFactory
  ;

type
  {forward declarations}
  TBoldClientHandlerCOM = class;
  TBoldClientHandlerCOMFactory = class;
  TBoldClientHandlerThreadedCOMFactory = class;

  {TBoldClientHandlerCOM}
  TBoldClientHandlerCOM = class(TBoldComObject, IBoldClientHandler)
  private
    fClientHandler: TBoldClientHandler;
    fRegistrationTime: TTimeStamp;
  public
    destructor Destroy; override;
    {IBoldClientHandler}
    function RegisterClient(LeaseDuration: Integer; PollingInterval: Integer;
                             const BoldClientListener: IBoldListener;
                             const ClientIDString: WideString; out BoldClientID: Integer): HResult; stdcall;
    function ExtendLease(BoldClientID: Integer; LeaseDuration: Integer; out ExtensionOK: WordBool): HResult; stdcall;
    function UnRegisterClient(BoldClientID: Integer): HResult; stdcall;
    procedure Initialize; override;
    property ClientHandler: TBoldClientHandler read fClientHandler write fClientHandler;
  end;

  {TBoldClientHandlerCOMFactory}
  TBoldClientHandlerCOMFactory = class(TComObjectFactory)
  private
    fClientHandler: TBoldClientHandler;
  public
    function CreateComObject(const Controller: IUnknown): TComObject; override;
    property ClientHandler: TBoldClientHandler read fClientHandler write fClientHandler;
  end;

  {TBoldClientHandlerThreadedCOMFactory}
  TBoldClientHandlerThreadedCOMFactory = class(TBoldThreadedComObjectFactory)
  private
    fClientHandler: TBoldClientHandler;
  public
    constructor Create(ComServer: TComServerObject; const ClassID: TGUID; const ClassName, Description: string);
    function CreateComObject(const Controller: IUnknown): TComObject; override;
    property ClientHandler: TBoldClientHandler read fClientHandler write fClientHandler;
  end;

var
  ClientHandlerCOMFactory: TBoldClientHandlerThreadedCOMFactory;
implementation

uses
  BoldApartmentThread,
  BoldPropagatorServer,
  windows
  ;

{TBoldClientHandlerCOM}
function TBoldClientHandlerCOM.RegisterClient(LeaseDuration: Integer; PollingInterval: Integer;
                         const BoldClientListener: IBoldListener;
                         const ClientIDString: WideString; out BoldClientID: Integer): HResult; stdcall;
begin
  BoldLogThread('ID=ClientHandler/RegCli');
  Result := ClientHandler.RegisterClient(LeaseDuration, PollingInterval, BoldClientListener,
                                         ClientIDString, BoldClientID, fRegistrationTime);
end;

function TBoldClientHandlerCOM.ExtendLease(BoldClientID: Integer; LeaseDuration: Integer; out ExtensionOK: WordBool): HResult; stdcall;
begin
  BoldLogThread('ID=ClientHandler/ExtLease');
  Result := ClientHandler.ExtendLease(BoldClientID, LeaseDuration, ExtensionOK);
end;

function TBoldClientHandlerCOM.UnRegisterClient(BoldClientID: Integer): HResult; stdcall;
begin
  BoldLogThread('ID=ClientHandlerH/UnReg');
  Result := ClientHandler.UnRegisterClient(BoldClientID, fRegistrationTime);
end;

destructor TBoldClientHandlerCOM.Destroy;
begin
  TBoldPropagatorServer.Instance.RemoveComObject(Self);
  inherited Destroy;
end;

procedure TBoldClientHandlerCOM.Initialize;
begin
  inherited;
  TBoldPropagatorServer.Instance.AddComObject(self);
end;

{TBoldClientHandlerCOMFactory}

function TBoldClientHandlerCOMFactory.CreateComObject(
  const Controller: IUnknown): TComObject;
begin
  Result := inherited CreateComObject(Controller);
  (Result as TBoldClientHandlerCOM).ClientHandler := ClientHandler;
end;

{ TBoldClientHandlerThreadedCOMFactory }

constructor TBoldClientHandlerThreadedCOMFactory.Create( ComServer: TComServerObject; const ClassID: TGUID;
  const ClassName, Description: string);
begin
  inherited Create(ComServer, TBoldClientHandlerCOM, ClassID, ClassName, Description, ciMultiInstance, batSTA);
end;

function TBoldClientHandlerThreadedCOMFactory.CreateComObject(
  const Controller: IUnknown): TComObject;
begin
  Result := inherited CreateComObject(Controller);
 (Result as TBoldClientHandlerCOM).ClientHandler := ClientHandler;
end;

end.
