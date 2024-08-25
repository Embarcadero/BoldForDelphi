
{ Global compiler directives }
{$include bold.inc}
unit BoldXMLDispatcherVB;

interface

uses
  Classes,
  BoldDefs,
  BoldSOAP2_TLB,
  BoldXMLRequests,
  comobj,
  BoldXMLDispatcher;
type


 TBoldXMLSOAPService2 = class(TAutoIntfObject, IBoldSOAPService2)
  private
    FOwner: TObject;
  protected
    procedure Get(const request: WideString; out reply: WideString); safecall;
    function  Get2(const request: WideString): WideString; safecall;
  public
    constructor Create(Owner: TObject);
    property Owner: TObject read FOwner;
  end;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldXMLDispatcherVB = class(TBoldXMLDispatcher)
  protected
    function GetComObject: IUnknown; override;
  end;

implementation

uses
  ActiveX,
  Windows,

  BoldCoreConsts;

{ TBoldXMLSOAPService2 }

constructor TBoldXMLSOAPService2.Create(Owner: TObject);
var
  typelib: ITypeLib;
begin
  if (LoadRegTypeLib(LIBID_BoldSOAP2, 1, 0, 0, typelib) = S_OK) then
  begin
    inherited Create(typelib, IBoldSOAPService2);
    FOwner := Owner;
  end
  else
    raise EBold.CreateFmt(sUnableToLoadTypeLibBoldSoap, [ClassName]);
end;

procedure TBoldXMLSOAPService2.Get(const request: WideString;
    out reply: WideString);
var
  ResponseXML: string;
  XMLRequest: TBoldXMLRequest;
begin
  if Assigned((Owner as TBoldXMLDispatcher).OnGetXMLRequest) then
    (Owner as TBoldXMLDispatcher).OnGetXMLRequest(request, XMLRequest)
  else
    XMLRequest := TBoldXMLRequest.CreateFromXML(request);
  if not Assigned(XMLRequest) then
    raise EBold.CreateFmt(sXMLRequestNotAssigned, [ClassName]);
  (Owner as TBoldXMLDispatcher).DispatchAction(XMLRequest, ResponseXML);
  reply := ResponseXML;
end;

function TBoldXMLSOAPService2.Get2(const request: WideString): WideString;
begin
  Get(request, Result);
end;

{ TBoldXMLDispatcherVB }

function TBoldXMLDispatcherVB.GetComObject: IUnknown;
begin
  Result := TBoldXMLSOAPService2.Create(self) as IUnknown;
end;

end.
