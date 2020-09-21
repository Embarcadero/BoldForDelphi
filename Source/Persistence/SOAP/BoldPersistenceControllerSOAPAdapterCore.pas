unit BoldPersistenceControllerSOAPAdapterCore;

interface

uses
  BoldPersistenceController,
  BoldMeta,
  BoldDefaultXMLStreaming,
  BoldPersistenceOperationXMLStreaming;

type
  { TBoldPersistenceControllerSOAPAdapterCore }
  TBoldPersistenceControllerSOAPAdapterCore = class
  private
    fStreamManager: TBoldDefaultXMLStreamManager;
  public
    constructor Create(Model: TMoldModel);
    destructor Destroy; override;
    procedure Get(const request: WideString; out reply: WideString; PersistenceController: TBoldPersistenceController);
  end;

implementation

uses
  SysUtils,
  MSXML_TLB,
  BoldDefs,
  BoldXMLStreaming,
  BoldComConst;

{ TBoldPersistenceControllerSOAPAdapterCore }

constructor TBoldPersistenceControllerSOAPAdapterCore.Create(Model: TMoldModel);
begin
  fStreamManager := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, Model);
  fStreamManager.PersistenceStatesToBeStreamed := [bvpsModified, bvpsCurrent, bvpsInvalid, bvpsTransient];
  fStreamManager.IgnorePersistenceState := False;
  fStreamManager.PersistenceStatesToOverwrite := [bvpsInvalid, bvpsCurrent];
end;

destructor TBoldPersistenceControllerSOAPAdapterCore.Destroy;
begin
  FreeAndNil(fStreamManager);
  inherited;
end;

procedure TBoldPersistenceControllerSOAPAdapterCore.Get(const request: WideString;
  out reply: WideString; PersistenceController: TBoldPersistenceController);
var
  RequestDoc, ReplyDoc: TDOMDocument;
  RequestBodyNode, ReplyBodyNode: TBoldXMLNode;
  anXMLNode: IXMLDOMNode;
  OpNode: TBoldXMLNode;
  OpName: string;
  PMOperation: TBoldPersistenceOperation;
begin
  RequestDoc := TDOMDocument.Create(nil);
  ReplyDoc := TDOMDocument.Create(nil);
  RequestBodyNode := nil;
  ReplyBodyNode := nil;
  OpNode := nil;
  PMOperation := nil;
  try
    RequestDoc.loadXML(request);
    RequestBodyNode := fStreamManager.GetSOAP(RequestDoc);
    anXMLNode := RequestBodyNode.XMLDomElement.childNodes.nextNode;
    OpNode := TBoldXMLNode.Create(fStreamManager, anXMLNode as IXMLDOMElement, nil);
    OpName := OpNode.Accessor;
    anXMLNode := nil;

    ReplyBodyNode := fStreamManager.NewSOAP(ReplyDoc);

    if OpName = 'PMFetch' then // do not localize
      PMOperation := TBoldPMFetchOperation.Create(fStreamManager)
    else if OpName = 'PMFetchIDListWithCondition' then // do not localize
      PMOperation := TBoldPMFetchIdListOperation.Create(fStreamManager)
    else if OpName = 'PMExactifyIds' then // do not localize
      PMOperation := TBoldPMExactifyIdsOperation.Create(fStreamManager)
    else if OpName = 'PMUpdate' then // do not localize
      PMOperation := TBoldPMUpdateOperation.Create(fStreamManager)
    else if OpName = 'ReserveNewIds' then // do not localize
      PMOperation := TBoldPMReserveNewIdsOperation.Create(fStreamManager)
    else if OpName = 'PMTimeForTimestamp' then // do not localize
      PMOperation := TBoldPMTimeForTimestampOperation.Create(fStreamManager)
    else if OpName = 'PMTimestampForTime' then // do not localize
      PMOperation := TBoldPMTimestampForTimeOperation.Create(fStreamManager)
    else
      raise EBold.CreateFmt(sUnknownOperation, [classname, OpName]);

    PMOperation.ExecuteStreamed(PersistenceController, RequestBodyNode, ReplyBodyNode);
    reply := ReplyDoc.DefaultInterface.xml;
  finally
    RequestBodyNode.Free;
    ReplyBodyNode.Free;
    RequestDoc.Free;
    ReplyDoc.Free;
    OpNode.Free;
    PMOperation.Free;
  end;
end;

end.
