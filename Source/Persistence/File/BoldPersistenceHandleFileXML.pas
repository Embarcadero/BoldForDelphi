unit BoldPersistenceHandleFileXML;

interface

uses
  BoldPersistenceController,
  BoldPersistenceHandleFile;

type
  { forward declarations }
  TBoldPersistenceHandleFileXML = class;
  TBoldPersistenceControllerFileXML = class;

  { TBoldPersistenceHandleFileXML }
  TBoldPersistenceHandleFileXML = class(TBoldAbstractPersistenceHandleFile)
  protected
    function CreatePersistenceController: TBoldPersistenceController; override;
  end;

  { TBoldPersistenceControllerFileXML }
  TBoldPersistenceControllerFileXML = class(TBoldPersistenceControllerFile)
  protected
    procedure WriteValueSpace; override;
    procedure ReadValueSpace; override;
  end;

const
  XML_LINKEXTENSION: string = '.xml';
  XML_LINKDESC: string = 'XML file';

implementation

uses
  classes,
  SysUtils,
  BoldDefs,
  MSXML_TLB,
  BoldId,
  BoldXMLStreaming,
  BoldDefaultXMLStreaming,
  BoldGuard,
  PersistenceConsts;

{ TBoldPersistenceHandleFileXML }

function TBoldPersistenceHandleFileXML.CreatePersistenceController: TBoldPersistenceController;
begin
  if not assigned(BoldModel) then
    Raise EBold.CreateFmt(sModelRequired, [ClassName]);
  Result := TBoldPersistenceControllerFileXML.Create(FileName, CacheData, BoldModel.MoldModel);
end;

{ TBoldPersistenceControllerFileXML }

procedure TBoldPersistenceControllerFileXML.ReadValueSpace;
var
  aStringList: TStringList;
  anXMLDoc: TDomDocument;
  aMgr: TBoldDefaultXMLStreamManager;
  aNode: TBoldXMLNode;
  ParseError: IXMLDOMParseError;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(aStringList, aMgr, anXMLDoc, aNode);
  aStringList := TStringList.Create;
  anXMLDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, MoldModel);
  aMgr.IgnorePersistenceState := True;
  aMgr.PersistenceStatesToOverwrite := [bvpsInvalid, bvpsCurrent];
  aMgr.PersistenceStatesToBeStreamed := [bvpsInvalid, bvpsModified, bvpsCurrent];

  aStringList.LoadFromFile(FileName);
  anXMLDoc.loadXML(aStringList.Text);

  ParseError := anXMLDoc.parseError;
  if Assigned(ParseError) and (ParseError.errorCode <> 0) then
    raise EBold.Create(sXMLParseError);
  aNode := aMgr.GetRootNode(anXMLDoc, 'ValueSpace'); //do not localize
  aMgr.ReadValueSpace(LocalValueSpace, aNode);
end;

procedure TBoldPersistenceControllerFileXML.WriteValueSpace;
var
  aStringList: TStringList;
  anXMLDoc: TDomDocument;
  aMgr: TBoldDefaultXMLStreamManager;
  aNode: TBoldXMLNode;
  anIdList: TBoldObjectIdList;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(aStringList, aNode, aMgr, anXMLDoc, anIDList);
  aStringList := TStringList.Create;
  anXMLDoc := TDOMDocument.Create(nil);
  aMgr := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, MoldModel);
  aMgr.IgnorePersistenceState := True;
  aMgr.PersistenceStatesToBeStreamed := [bvpsInvalid, bvpsModified, bvpsCurrent];
  aNode := aMgr.NewRootNode(anXMLDoc, 'ValueSpace'); //do not localize
  anIdList := TBoldObjectIdList.Create;
  LocalValueSpace.AllObjectIds(anIdList, True);
  aMgr.WriteValueSpace(LocalValueSpace, anIdList, nil, aNode);
  aStringList.Text := anXMLDoc.documentElement.xml;
  aStringList.SaveToFile(FileName);
end;

end.
