
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
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
  {$IFDEF OXML}OXmlPDOM, OTextReadWrite{$ELSE}Bold_MSXML_TLB{$ENDIF},
  BoldId,
  BoldXMLStreaming,
  BoldDefaultXMLStreaming,
  BoldGuard;

{ TBoldPersistenceHandleFileXML }

function TBoldPersistenceHandleFileXML.CreatePersistenceController: TBoldPersistenceController;
begin
  if not assigned(BoldModel) then
    Raise EBold.CreateFmt('%s.CreatePersistenceController: Unable to create, model is missing.', [ClassName]);
  Result := TBoldPersistenceControllerFileXML.Create(FileName, CacheData, BoldModel.MoldModel);
end;

{ TBoldPersistenceControllerFileXML }

procedure TBoldPersistenceControllerFileXML.ReadValueSpace;
{$IFDEF OXML}
var
  anXMLDoc: TXMLDocument;
  ParseError: IOTextParseError;
  aMgr: TBoldDefaultXMLStreamManager;
  aNode: TBoldXMLNode;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(aMgr, aNode);
  anXMLDoc := TXMLDocument.Create;
  aMgr := TBoldDefaultXMLStreamManager.Create(
      TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, MoldModel);
  aMgr.IgnorePersistenceState := True;
  aMgr.PersistenceStatesToOverwrite := [bvpsInvalid, bvpsCurrent];
  aMgr.PersistenceStatesToBeStreamed := [bvpsInvalid, bvpsModified, bvpsCurrent];
  anXMLDoc.LoadFromFile(FileName);
  ParseError := anXMLDoc.parseError;
  if Assigned(ParseError) and (ParseError.ErrorCode <> 0) then
    raise EBold.Create('Error reading/parsing XML file');
  aNode := aMgr.GetRootNode(anXMLDoc, 'ValueSpace'); //do not localize
  aMgr.ReadValueSpace(LocalValueSpace, aNode);
end;
{$ELSE}
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
    raise EBold.Create('Error reading/parsing XML file');
  aNode := aMgr.GetRootNode(anXMLDoc, 'ValueSpace');
  aMgr.ReadValueSpace(LocalValueSpace, aNode);
end;
{$ENDIF}

procedure TBoldPersistenceControllerFileXML.WriteValueSpace;
{$IFDEF OXML}
var
  aXML: TXMLDocument;
  aMgr: TBoldDefaultXMLStreamManager;
  aNode: TBoldXMLNode;
  anIdList: TBoldObjectIdList;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(aNode, aMgr, anIDList);
  aXML := TXMLDocument.Create;
  aMgr := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry, MoldModel);
  aMgr.IgnorePersistenceState := True;
  aMgr.PersistenceStatesToBeStreamed := [bvpsInvalid, bvpsModified, bvpsCurrent];
  aNode := aMgr.NewRootNode(aXML, 'ValueSpace'); //do not localize
  anIdList := TBoldObjectIdList.Create;
  LocalValueSpace.AllObjectIds(anIdList, True);
  aMgr.WriteValueSpace(LocalValueSpace, anIdList, nil, aNode);
  aXML.SaveToFile(FileName);
end;
{$ELSE}
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
  aNode := aMgr.NewRootNode(anXMLDoc, 'ValueSpace');
  anIdList := TBoldObjectIdList.Create;
  LocalValueSpace.AllObjectIds(anIdList, True);
  aMgr.WriteValueSpace(LocalValueSpace, anIdList, nil, aNode);
  aStringList.Text := anXMLDoc.documentElement.xml;
  aStringList.SaveToFile(FileName);
end;
{$ENDIF}

end.
