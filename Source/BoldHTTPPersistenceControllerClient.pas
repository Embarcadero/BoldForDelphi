
{ Global compiler directives }
{$include bold.inc}
unit BoldHTTPPersistenceControllerClient;

interface

uses
  BoldSOAPPersistenceControllerProxy,
  BoldWebConnection,
  BoldMeta,
  BoldSOAP_TLB,
  ComObj,
  ActiveX
  ;

type
  {forward declarations}
  TBoldHTTPPersistenceControllerClient = class;
  TBoldHTTPSOAPService = class;

  { TBoldHTTPPersistenceControllerClient }
  TBoldHTTPPersistenceControllerClient = class(TBoldSOAPPersistenceControllerProxy)
  private
    FHTTPSoapService: TBoldHTTPSOAPService;
    function getWebConnection: TBoldWebConnection;
    procedure setWebConnection(Value: TBoldWebConnection);
  public
    constructor Create(Model: TMoldModel);
    destructor Destroy; override;
    procedure Disconnect; override;
    property WebConnection: TBoldWebConnection read getWebConnection write setWebConnection;
  end;

  { TBoldHTTPSOAPService }
  TBoldHTTPSOAPService = class(TAutoIntfObject, IBoldSOAPService)
  private
    FWebConnection: TBoldWebConnection;
  public
    constructor Create;
    procedure Get(const request: WideString; out reply: WideString); safecall;
    property WebConnection: TBoldWebConnection read FWebConnection write FWebConnection;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldDataBlock,
  BoldDefs,
  Classes,
  windows
  ;

{ TBoldHTTPSOAPService }

constructor TBoldHTTPSOAPService.Create;
var
  typelib: ITypeLib;
begin
  if (LoadRegTypeLib(LIBID_BoldSOAP, 1, 0, 0, typelib) = S_OK) then
    inherited Create(typelib, IBoldSOAPService)
  else
    raise EBold.CreateFmt('%s.Create: Unable to load type library LIBID_BoldSOAP', [ClassName]);
end;

procedure TBoldHTTPSOAPService.Get(const request: WideString;
  out reply: WideString);
var
  dataSent, dataReceived: TBoldDataBlock;
  StrRequest, StrReply: string;
  size, len, Context: Integer;
begin
  StrRequest:= request;
  len := Length(StrRequest);
  Assert(len > 0);
  dataSent := TBoldDataBlock.Create;
  dataReceived := nil;
  try
    dataSent.Write(StrRequest[1], len);
    Context := WebConnection.Send(datasent);
    dataReceived := WebConnection.Receive(true, Context);
    if assigned(dataReceived) then
    begin
      size := dataReceived.Stream.Position;
      SetString(StrReply, nil, Size);
      dataReceived.Stream.Position := 0;
      dataReceived.Stream.Read(Pointer(StrReply)^, Size);
      Reply := WideString(StrReply);
    end else
      Reply := '';
  finally
    FreeAndNil(datasent);
    FreeAndNil(dataReceived);
  end;
end;


{ TBoldHTTPPersistenceControllerClient }

constructor TBoldHTTPPersistenceControllerClient.Create(Model: TMoldModel);
begin
  inherited Create(Model);
  fhttpSoapService := TBoldHTTPSOAPService.Create;
  fStub := fhttpSoapService as IBoldSOAPService;
end;

destructor TBoldHTTPPersistenceControllerClient.Destroy;
begin
  fStub := nil;
  inherited;
end;

procedure TBoldHTTPPersistenceControllerClient.Disconnect;
begin
end;

function TBoldHTTPPersistenceControllerClient.getWebConnection: TBoldWebConnection;
begin
  Result := fhttpSoapService.WebConnection;
end;

procedure TBoldHTTPPersistenceControllerClient.setWebConnection(
  Value: TBoldWebConnection);
begin
  fhttpSoapService.WebConnection := Value;
end;

end.
