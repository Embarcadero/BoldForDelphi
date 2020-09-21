unit webModule;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  HTTPApp,
  BoldSubscription,
  BoldHandle,
  BoldClientHandles,
  BoldComClientHandles;

type
  TWebModule1 = class(TWebModule)
    bohDispatcher: TBoldComClientObjectHandle;
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    procedure WebModule1BudgetXMLAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModule1: TWebModule1;

implementation

uses
  BoldUtils,
  BoldXMLRequests,
  BoldStringList,
  BoldSOAP_TLB,
  MSXML_TLB,
  BoldDefs,
  comobj
  ;

{$R *.DFM}

procedure TWebModule1.WebModule1BudgetXMLAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  XMLRequest: TBoldXMLRequest;
  res : WideString;
  XSLDoc, XMLDoc: IXMLDomDocument;
begin
  XMLRequest := TBoldXMLRequest.CreateInitialized;
  try
    try
      case Request.MethodType of
        mtPost:
          begin
            XMLRequest.SetAction('UpdateBudget');
            XMLRequest.AddParam('BudgetName', Request.QueryFields.Values['who']);
            XMLRequest.SetIdentifiedValues(Request.ContentFields);
          end;
        mtAny,
        mtPut,
        mtGet,
        mtHead:
          begin
            XMLRequest.SetAction('GetBudget');
            XMLRequest.AddParam('BudgetName', Request.QueryFields.Values['who']);
          end;
      end; //case
      try
        if not BoldComConnectionHandle1.Connected then
          BoldComConnectionHandle1.Connected := true;
        (bohDispatcher.ComObject as IBoldSOAPService).Get(XMLRequest.DomDocument.XML, res);
        //apply xsl
        XSLDoc := CoDOMDocument.Create;
        XMLDoc := CoDomDocument.Create;
        XSLDoc.load(ExtractFilePath(GetModuleFileNameAsString(True)) + 'Budget.xsl');
        XMLDoc.loadXML(res);
        res := XMLDoc.transformNode(XSLDoc);
        Response.Content := res ;
      except on E: Exception do
        Response.Content := Format('Error while getting XML for budget: %s ', [E.Message]);
      end;
    except on E: Exception do
      Response.Content := Format('%s.BudgetXmlAction: %s', [Classname, E.Message]);
    end;
  finally
    XMLRequest.Free;
  end;
  Handled := True;
end;

end.
