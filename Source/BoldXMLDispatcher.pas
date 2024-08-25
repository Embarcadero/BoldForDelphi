
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldXMLDispatcher;

interface

uses
  SysUtils,
  BoldUtils,
  BoldSOAP_TLB,
  BoldStringList,
  {$IFDEF OXML}OXmlPDOM{$ELSE}Bold_MSXML_TLB{$ENDIF},
  BoldComServerHandles,
  BoldDefs,
  BoldXMLRequests,
  BoldCollections,
  BoldXMLProducers,
  BoldSubscription,
  Classes,
  comobj;

type
  {forward declarations}
  TBoldXMLDispatcher = class;
  TBoldXMLSOAPService = class;
  TBoldXMLActions = class;
  TBoldXMLActionItem = class;

  TBoldXMLSOAPService = class(TAutoIntfObject, IBoldSOAPService)
  private
    FOwner: TObject;
  protected
    procedure Get(const request: WideString; out reply: WideString); safecall;
  public
    constructor Create(Owner: TObject);
    property Owner: TObject read FOwner;
  end;

  TBoldXMLDispatchErrorEvent = procedure (const E: Exception; out response: string) of object;
  
  TBoldGetXMLRequestEvent = procedure (const XML: string; out Request: TBoldXMLRequest) of object;
  TBoldXMLDispatcher = class(TBoldComExportHandle)
  private
    FActions: TBoldXMLActions;
    FOnDispatchError: TBoldXMLDispatchErrorEvent;
    FOnGetXMLRequest: TBoldGetXMLRequestEvent;
    procedure SetActions(Value: TBoldXMLActions);
    function GetAction(Index: Integer): TBoldXMLActionItem;
  protected
    function GetComObject: IUnknown; override;
    function GetHandledObject: TObject; override;
    property Action[Index: Integer]: TBoldXMLActionItem read GetAction;
    procedure HandleDispatchError(const E: Exception; out response: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DispatchAction(const request: TBoldXMLRequest; out response: string);
  published
    property Actions: TBoldXMLActions read FActions write SetActions;
    property OnDispatchError: TBoldXMLDispatchErrorEvent read FOnDispatchError write FOnDispatchError;
    property OnGetXMLRequest: TBoldGetXMLRequestEvent read FOnGetXMLRequest write FOnGetXMLRequest;
  end;

  TBoldXMLActionItem = class(TBoldUniquelyNamedCollectionItemWithNameStorage)
  private
    FProducer: TBoldXMLProducer;
    FDefault: Boolean;
    FOnAction: TBoldXMLMethodEvent;
    FSubscriber: TBoldPassThroughSubscriber;
    procedure SetDefault(Value: Boolean);
    function getActionName: string;
    procedure setActionName(const Value: string);
    procedure setProducer(const Value: TBoldXMLProducer);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure DispatchAction(const request: TBoldXMLRequest; out response: string);
  published
    property Default: Boolean read FDefault write SetDefault default False;
    property ActionName: string read getActionName write setActionName;
    property Producer: TBoldXMLProducer read FProducer write setProducer;
    property OnAction: TBoldXMLMethodEvent read FOnAction write FOnAction;
  end;

  TBoldXMLActions = class(TBoldCollectionWithUniquelyNamedItems)
  private
    FXMLDispatcher: TBoldXMLDispatcher;
    function GetItem(Index: Integer): TBoldXMLActionItem;
    procedure SetItem(Index: Integer; Value: TBoldXMLActionItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(XMLDispatcher: TBoldXMLDispatcher);
    function Add: TBoldXMLActionItem;
    property Items[Index: Integer]: TBoldXMLActionItem read GetItem write SetItem; default;
  end;

implementation

{$R *.res}

uses
  ActiveX,
  Windows,

  BoldCoreConsts;

const
  breProducerDestroying = 100;

{ TBoldXMLSOAPService }

constructor TBoldXMLSOAPService.Create(Owner: TObject);
var
  typelib: ITypeLib;
  Res: HResult;
begin
  Res := LoadRegTypeLib(LIBID_BoldSOAP, 1, 0, 0, typelib);
  if (Res = S_OK) then
  begin
    inherited Create(typelib, IBoldSOAPService);
    FOwner := Owner;
  end
  else
    raise EBold.CreateFmt(sUnableToLoadTypeLibBoldSoap, [ClassName]);
end;

procedure TBoldXMLSOAPService.Get(const request: WideString;
    out reply: WideString);
var
  ResponseXML: string;
  XMLRequest: TBoldXMLRequest;
begin
  if Assigned((Owner as TBoldXMLDispatcher).FOnGetXMLRequest) then
    (Owner as TBoldXMLDispatcher).FOnGetXMLRequest(request, XMLRequest)
  else
    XMLRequest := TBoldXMLRequest.CreateFromXML(request);
  if not Assigned(XMLRequest) then
    raise EBold.CreateFmt(sXMLRequestNotAssigned, [ClassName]);
  (Owner as TBoldXMLDispatcher).DispatchAction(XMLRequest, ResponseXML);
  reply := ResponseXML;
end;

{ TBoldXMLDispatcher }

constructor TBoldXMLDispatcher.Create;
begin
  inherited;
  FActions := TBoldXMLActions.Create(self);
end;

destructor TBoldXMLDispatcher.Destroy;
begin
  FreeAndNil(FActions);
  inherited;
end;

procedure TBoldXMLDispatcher.DispatchAction(const request: TBoldXMLRequest;
    out response: string);
var
  I: Integer;
  Action, Default: TBoldXMLActionItem;
  ActionName: string;
begin
  try
    if not Assigned(request) then
      raise EBold.CreateFmt('%s.DispatchAction: request is nil', [ClassName]);
    I := 0;
    Default := nil;
    ActionName := request.ActionName;
    Action := TBoldXMLActionItem(FActions.ItemByName[ActionName]);
    if Assigned(Action) then
      Action.DispatchAction(Request, Response)
    else
    begin
      while (I < FActions.Count) and not Assigned(Default) do
      begin
        Action := FActions[I];
        if Action.Default then Default := Action;
        Inc(I);
      end;
      if Assigned(Default) then
        Default.DispatchAction(Request, Response);
    end;
  except on E: Exception do
    if Assigned(FOnDispatchError) then
      FOnDispatchError(E, Response)
    else
      HandleDispatchError(E, Response);
  end;
end;

function TBoldXMLDispatcher.GetAction(Index: Integer): TBoldXMLActionItem;
begin
  Result := FActions[Index];
end;

function TBoldXMLDispatcher.GetComObject: IUnknown;
begin
  Result := TBoldXMLSOAPService.Create(self) as IUnknown;
end;

function TBoldXMLDispatcher.GetHandledObject: TObject;
begin
  Result := nil;
end;

procedure TBoldXMLDispatcher.HandleDispatchError(const E: Exception;
  out response: string);
var
  xmlresponse: TBoldXMLRequest;
begin
    xmlresponse := TBoldXMLRequest.CreateInitialized;
  try
    xmlresponse.SetAction('SOAP:Fault');
    xmlresponse.AddParam('SOAP:faultstring', E.Message);
    response := xmlresponse.DomDocument.XML;
  finally
    FreeAndNil(xmlresponse);
  end;
end;

procedure TBoldXMLDispatcher.SetActions(Value: TBoldXMLActions);
begin
  FActions.Assign(Value);
end;

{ TBoldXMLAction }

constructor TBoldXMLActionItem.Create(Collection: TCollection);
var
  i: integer;
  s, aName: string;
begin
  inherited Create(Collection);
  I := 0;
  s := Copy(ClassName, 2, MaxInt);
  repeat
    Inc(I);
    aName := Format('%s%d',[s, I]);
  until not Assigned(self.Collection.ItemByName[aName]);
  ActionName := aName;
  FSubscriber := TBoldPassThroughSubscriber.Create(_Receive);
end;

destructor TBoldXMLActionItem.Destroy;
begin
  FreeAndNil(FSubscriber);
  inherited;
end;

procedure TBoldXMLActionItem.DispatchAction(const request: TBoldXMLRequest;
    out response: string);
begin
  if Assigned(FProducer) then
    response := Producer.getDocumentAsString(request.Params)
  else if Assigned(FOnAction) then
    FOnAction(request, response);
end;

function TBoldXMLActionItem.getActionName: string;
begin
  Result := UniqueName;
end;

{function TBoldXMLActionItem.GetActionNode(const DomDocument: IXMLDomDocument;
  out DomNode: IXMLDomNode): Boolean;
var
  NodesList: IXMLDOMNodeList;
begin
  Result := False;
  NodesList := DomDocument.getElementsByTagName(WideString(ActionName));
  if Assigned(NodesList) and (NodesList.length > 0) then
  begin
    DomNode := NodesList.item[0];
    Result := True;
  end;
end;
}
procedure TBoldXMLActionItem.setActionName(const Value: string);
begin
  UniqueName := Value;
end;

procedure TBoldXMLActionItem.SetDefault(Value: Boolean);
var
  I: Integer;
  Action: TBoldXMLActionItem;
begin
  if (Value <> FDefault) then
  begin
    if Value and Assigned(Collection) then
      for I := 0 to Collection.Count - 1 do
      begin
        Action := TBoldXMLActions(Collection).Items[I];
        if (Action <> Self) then
          Action.Default := False;
      end;
    FDefault := Value;
    Changed(False);
  end;
end;

{ TBoldXMLActions }

function TBoldXMLActions.Add: TBoldXMLActionItem;
begin
  Result := TBoldXMLActionItem(inherited Add);
end;

constructor TBoldXMLActions.Create(XMLDispatcher: TBoldXMLDispatcher);
begin
  inherited Create(self, TBoldXMLActionItem);
  FXMLDispatcher := XMLDispatcher;
end;

function TBoldXMLActions.GetItem(Index: Integer): TBoldXMLActionItem;
begin
  Result := TBoldXMLActionItem(inherited GetItem(Index));
end;

function TBoldXMLActions.GetOwner: TPersistent;
begin
  Result := FXMLDispatcher;
end;

procedure TBoldXMLActions.SetItem(Index: Integer; Value: TBoldXMLActionItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TBoldXMLActionItem.setProducer(const Value: TBoldXMLProducer);
begin
  if (FProducer <> Value) then
  begin
    FSubscriber.CancelAllSubscriptions;
    FProducer := Value;
    if Assigned(FProducer) then
      FProducer.AddSmallSubscription(FSubscriber, [beDestroying], breProducerDestroying);
  end;
end;

procedure TBoldXMLActionItem._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (RequestedEvent = breProducerDestroying) then
    Producer := nil;
end;

end.
