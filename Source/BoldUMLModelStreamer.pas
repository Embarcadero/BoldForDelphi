
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelStreamer;

interface

uses
  BoldSystem,
  BoldMeta;

type
  TUMLModelStreamer = class
  public
    class procedure FillSystemFromString(BoldSystem: TBoldSystem; const UMLModelAsString: string; MoldModel: TMoldModel);
    class function SystemAsString(BoldSystem: TBoldSystem; MoldModel: TMoldModel): string;
  end;


implementation

uses
  {$IFDEF OXML}OXmlPDOM, OTextReadWrite{$ELSE}Bold_MSXML_TLB{$ENDIF},
  BoldXMLStreaming,
  BoldDefaultXMLStreaming,
  BoldDomainElement,
  BoldGuard,
  BoldDefs,
  BoldValueSpaceInterfaces,
  BoldID;

{ TUMLModelStreamer }

class procedure TUMLModelStreamer.FillSystemFromString(BoldSystem: TBoldSystem;
  const UMLModelAsString: string; MoldModel: TMoldModel);
{$IFDEF OXML}
var
  anXMLDoc: TXMLDocument;
  aMgr: TBoldDefaultXMLStreamManager;
  aNode: TBoldXMLNode;
  ParseError: IOTextParseError;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(aMgr, aNode);
  anXMLDoc := TXMLDocument.Create;
  aMgr := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, MoldModel);

  aMgr.IgnorePersistenceState := True;
  aMgr.PersistenceStatesToOverWrite := [bvpsInvalid, bvpsModified, bvpsTransient, bvpsCurrent];
  anXMLDoc.LoadFromXML(UMLModelAsString);

  ParseError := anXMLDoc.ParseError;
  if Assigned(ParseError) and (ParseError.ErrorCode <> 0) then
    raise EBold.Create('Error reading/parsing XML file');
  aNode := aMgr.GetRootNode(anXMLDoc, 'ValueSpace'); // do not localize
  aMgr.ReadValueSpace(BoldSystem.AsIBoldvalueSpace[bdepPMIn], aNode);
end;
{$ELSE}
var
  anXMLDoc: TDomDocument;
  aMgr: TBoldDefaultXMLStreamManager;
  aNode: TBoldXMLNode;
  ParseError: IXMLDOMParseError;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(anXMLDoc, aMgr, aNode);
  anXMLDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, MoldModel);

  aMgr.IgnorePersistenceState := True;
  aMgr.PersistenceStatesToOverWrite := [bvpsInvalid, bvpsModified, bvpsTransient, bvpsCurrent];

  anXMLDoc.loadXML(UMLModelAsString);

  ParseError := anXMLDoc.parseError;
  if Assigned(ParseError) and (ParseError.errorCode <> 0) then
    raise EBold.Create('Error reading/parsing XML file');
  aNode := aMgr.GetRootNode(anXMLDoc, 'ValueSpace');
  aMgr.ReadValueSpace(BoldSystem.AsIBoldvalueSpace[bdepPMIn], aNode);
end;
{$ENDIF}

class function TUMLModelStreamer.SystemAsString(BoldSystem: TBoldSystem; MoldModel: TMoldModel): string;
{$IFDEF OXML}
var
  anXMLDoc: TXMLDocument;
  aMgr: TBoldDefaultXMLStreamManager;
  aNode: TBoldXMLNode;
  anIdList: TBoldObjectIdList;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(aNode, aMgr, anIDList);
  anXMLDoc := TXMLDocument.Create;
  aMgr := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, MoldModel);
  aMgr.IgnorePersistenceState := True;
  aMgr.PersistenceStatesToBeStreamed := [bvpsInvalid, bvpsModified, bvpsTransient, bvpsCurrent];
  aNode := aMgr.NewRootNode(anXMLDoc, 'ValueSpace'); // do not localize
  anIdList := TBoldObjectIdList.Create;

  BoldSystem.AsIBoldValueSpace[bdepPMOut].AllObjectIds(anIdList, True);
  aMgr.WriteValueSpace(BoldSystem.AsIBoldValueSpace[bdepPMOut], anIdList, nil, aNode);
  Result := anXMLDoc.XML;
end;
{$ELSE}
var
  anXMLDoc: TDomDocument;
  aMgr: TBoldDefaultXMLStreamManager;
  aNode: TBoldXMLNode;
  anIdList: TBoldObjectIdList;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(aNode, aMgr, anXMLDoc, anIDList);
  anXMLDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, MoldModel);
  aMgr.IgnorePersistenceState := True;
  aMgr.PersistenceStatesToBeStreamed := [bvpsInvalid, bvpsModified, bvpsTransient, bvpsCurrent];
  aNode := aMgr.NewRootNode(anXMLDoc, 'ValueSpace');
  anIdList := TBoldObjectIdList.Create;

  BoldSystem.AsIBoldValueSpace[bdepPMOut].AllObjectIds(anIdList, True);
  aMgr.WriteValueSpace(BoldSystem.AsIBoldValueSpace[bdepPMOut], anIdList, nil, aNode);
  Result := anXMLDoc.documentElement.xml;
end;
{$ENDIF}

end.
