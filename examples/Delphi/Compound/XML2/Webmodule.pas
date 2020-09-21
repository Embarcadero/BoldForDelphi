unit Webmodule;

interface

uses
  SysUtils,
  Classes,
  HTTPApp,
  BoldClientHandles,
  BoldComClientHandles,
  BoldSubscription,
  BoldHandle,
  BoldXMLRequests,
  MSXML_TLB,
  BoldSOAP_TLB, HTTPProd;

type
  TWebModule1 = class(TWebModule)
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    bcohDispatcher: TBoldComClientObjectHandle;
    MainPage: TPageProducer;
    procedure WebModule1OrgChartAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    function Dispatcher: IBoldSOAPService;
  end;

var
  WebModule1: TWebModule1;

implementation

{$R *.DFM}

function TWebModule1.Dispatcher: IBoldSOAPService;
begin
  if not BoldComConnectionHandle1.Connected then
    BoldComConnectionHandle1.Connected := True;
  Result := (bcohDispatcher.ComObject as IBoldSOAPService);
end;

procedure TWebModule1.WebModule1OrgChartAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  XMLRequest: TBoldXMLRequest;
  res : WideString;
  XSLDoc, XMLDoc: IXMLDomDocument;
  str: string;
begin
  XMLRequest := TBoldXMLRequest.CreateInitialized;
  str := '';
  try
    //This is where you receive the http request from the browser
    case Request.MethodType of
      mtPost, mtAny, mtGet:
        begin
          if (Request.ContentFields.Count= 0) or (Request.ContentFields.Values['Action'] = 'Home' ) then
          begin
            str := 'NO CONTENTFIELDS';
            Response.content := MainPage.Content;
            Handled := True;
            Exit;
          end
          else if (Request.ContentFields.Values['Action'] = 'EvaluateOCL' ) then
          begin
            str := 'Evaluate OCL';
            //evaluate OCL
            XMLRequest.SetAction('EvaluateOCL');
            XMLRequest.AddParam('OCL', Request.ContentFields.Values['OCL']);
          end
          else if (Request.ContentFields.Values['Action'] = 'FetchID' ) then
          begin
            //fetch object
            str := 'Fetch BOLDID';
            XMLRequest.SetAction('Fetch');
            XMLRequest.AddParam('BOLDID', Request.ContentFields.Values['BOLDID']);
          end
          else if (Request.ContentFields.Values['Action'] = 'Update' ) then
          begin
            str := 'Update';
            //request for an update
            XMLRequest.SetAction('Update');
            XMLRequest.SetIdentifiedValues(Request.ContentFields);
          end;
        end;
    end;
    //Set Response.content to return the result
    try
      Dispatcher.Get(XMLRequest.DomDocument.xml, res);
      Response.Content := res;
      Handled := True;
    except on E: Exception do
      Response.Content := 'Message returned from OrgChart server: ' + E.Message + ' ' + str;
    end;
  finally
    FreeAndNil(XMLRequest);
  end;

end;

end.
