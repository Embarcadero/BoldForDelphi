
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceControllerSOAPAdapterCore;

interface

uses
  BoldPersistenceController,
  BoldMeta,
  BoldDefaultXMLStreaming,
  BoldPersistenceOperationXMLStreaming
  ;

type

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
  {$IFDEF OXML}OXmlPDOM{$ELSE}Bold_MSXML_TLB{$ENDIF},
  BoldDefs,
  BoldXMLStreaming;

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
  RequestDoc, ReplyDoc: {$IFDEF OXML}TXMLDocument{$ELSE}TDOMDocument{$ENDIF};
  RequestBodyNode, ReplyBodyNode: TBoldXMLNode;
  anXMLNode: {$IFDEF OXML}PXMLNode{$ELSE}IXMLDOMNode{$ENDIF};
  OpNode: TBoldXMLNode;
  OpName: string;
  PMOperation: TBoldPersistenceOperation;
begin
  {$IFDEF OXML}
  RequestDoc := TXMLDocument.Create;
  ReplyDoc := TXMLDocument.Create;
  {$ELSE}
  RequestDoc := TDOMDocument.Create(nil);
  ReplyDoc := TDOMDocument.Create(nil);
  {$ENDIF}
  RequestBodyNode := nil;
  ReplyBodyNode := nil;
  OpNode := nil;
  PMOperation := nil;
  try
    {$IFDEF OXML}
    RequestDoc.LoadFromXML(request);
    RequestBodyNode := fStreamManager.GetSOAP(RequestDoc);
    anXMLNode := RequestBodyNode.XMLDomElement.ChildNodes.GetFirst;
    OpNode := TBoldXMLNode.Create(fStreamManager, anXMLNode, nil);
    {$ELSE}
    RequestDoc.loadXML(request);
    RequestBodyNode := fStreamManager.GetSOAP(RequestDoc);
    anXMLNode := RequestBodyNode.XMLDomElement.childNodes.nextNode;
    OpNode := TBoldXMLNode.Create(fStreamManager, anXMLNode as IXMLDOMElement, nil);
    {$ENDIF}
    OpName := OpNode.Accessor;

    ReplyBodyNode := fStreamManager.NewSOAP(ReplyDoc);

    if OpName = 'PMFetch' then
      PMOperation := TBoldPMFetchOperation.Create(fStreamManager)
    else if OpName = 'PMFetchIDListWithCondition' then
      PMOperation := TBoldPMFetchIdListOperation.Create(fStreamManager)
    else if OpName = 'PMExactifyIds' then
      PMOperation := TBoldPMExactifyIdsOperation.Create(fStreamManager)
    else if OpName = 'PMUpdate' then
      PMOperation := TBoldPMUpdateOperation.Create(fStreamManager)
    else if OpName = 'ReserveNewIds' then
      PMOperation := TBoldPMReserveNewIdsOperation.Create(fStreamManager)
    else if OpName = 'PMTimeForTimestamp' then
      PMOperation := TBoldPMTimeForTimestampOperation.Create(fStreamManager)
    else if OpName = 'PMTimestampForTime' then
      PMOperation := TBoldPMTimestampForTimeOperation.Create(fStreamManager)
    else
      raise EBold.CreateFmt('%s.Get: Unrecognized operation %s', [classname, OpName]);

    PMOperation.ExecuteStreamed(PersistenceController, RequestBodyNode, ReplyBodyNode);
    reply := {$IFDEF OXML}ReplyDoc.XML{$ELSE}ReplyDoc.DefaultInterface.xml{$ENDIF};
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
