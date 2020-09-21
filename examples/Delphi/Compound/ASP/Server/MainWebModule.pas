unit MainWebModule;

interface

uses
  Windows, Messages, SysUtils, Classes, HTTPApp, BoldServerHandles,
  BoldComServerHandles, BoldXMLDispatcher,
  BoldHTTPServerPersistenceHandlePassthrough, BoldPersistenceHandleSystem,
  BoldHandle, BoldPersistenceHandle, BoldPersistenceHandleDB,
  BoldHandles, BoldSubscription, BoldSystemHandle,
  BoldAbstractModel, BoldModel, BoldXMLRequests, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TwmASPServer = class(TWebModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldPersistenceHandleSystem1: TBoldPersistenceHandleSystem;
    BoldHTTPServerPersistenceHandlePassthrough1: TBoldHTTPServerPersistenceHandlePassthrough;
    BoldXMLDispatcher1: TBoldXMLDispatcher;
    BoldModel1: TBoldModel;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    procedure wmASPServerPersistenceAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure wmASPServerSOAPCallsAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure BoldXMLDispatcher1Actions0Action(
      const request: TBoldXMLRequest; out response: String);
    procedure BoldXMLDispatcher1Actions1Action(
      const request: TBoldXMLRequest; out response: String);
    procedure IBDatabase1BeforeConnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  wmASPServer: TwmASPServer;

implementation

uses
  BoldId,
  BoldDefaultId,
  BoldSystem,
  BuildingClasses,
  BoldUtils;

{$R *.DFM}

procedure TwmASPServer.wmASPServerPersistenceAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  reply: WideString;
begin
  BoldHTTPServerPersistenceHandlePassthrough1.Get(Request.Content, reply);
  Response.Content := Reply;
  Handled := true;
end;

procedure TwmASPServer.wmASPServerSOAPCallsAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  XMLRequest: TBoldXMLRequest;
  reply: String;
begin
  XMLRequest := TBoldXMLRequest.CreateFromXML(Request.Content);
  BoldXMLDispatcher1.DispatchAction(XMLRequest, reply);
  Response.Content := reply;
  Handled := true;
end;

procedure TwmASPServer.WebModuleCreate(Sender: TObject);
begin
  BoldSystemHandle1.Active := true;
  BoldPersistenceHandleSystem1.Active := true;
end;

procedure TwmASPServer.BoldXMLDispatcher1Actions0Action(
  const request: TBoldXMLRequest; out response: String);
begin
  try
    BoldSystemHandle1.UpdateDatabase;
    response := 'OK';
  except
    on E: Exception do
      response := E.Message;
  end;
end;

procedure TwmASPServer.BoldXMLDispatcher1Actions1Action(
  const request: TBoldXMLRequest; out response: String);
var
  anObjectId: TBoldDefaultId;
  aLocator: TBoldObjectLocator;
begin
  anObjectId := TBoldDefaultID.CreateWithClassID(0, false);
  anObjectId.AsInteger := StrToInt(request.Params.Values['Building']);
  aLocator := BoldPersistenceHandleSystem1.PersistenceControllerSystem.LocatorById[anObjectId];
  anObjectId.Free;

  if assigned(aLocator) then
  begin
    (aLocator.EnsuredBoldObject as TResidential_Building).ChargeRent;
    response := 'Rent charged';
  end
  else
    response := 'Building not found on server';
end;

procedure TwmASPServer.IBDatabase1BeforeConnect(Sender: TObject);
begin
  IBDatabase1.DatabaseName := 'localhost:' + GetModuleFileNameAsString(True) + ExtractFileName(IBDatabase1.DatabaseName);
end;

end.
