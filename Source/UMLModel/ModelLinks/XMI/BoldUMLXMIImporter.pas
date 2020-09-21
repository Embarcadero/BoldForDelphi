unit BoldUMLXMIImporter;

interface

uses
  Variants,
  BoldContainers,
  BoldUMLModel,
  BoldLogForm,
  BoldSystem,
  BoldUMLXMILink,
  BoldSystemRT,
  BoldUMLXMILinkSupport,
  MSXML_TLB,
  Sysutils, // for TFileName
  Classes;

type

  { types }
  TPass = (PASS1, PASS2);
  TClassAction = (caIMPORT, caTRAVERSE, caSKIP, caUNKNOWN);
  TXMIExporter = (STDEXPORTER, UNISYS_TCR_2);

 { TBoldUMLXMILink }
  TBoldUMLXMIImporter = class
  private
    fTheUMLModel: TUMLModel;
    fTheUMLModelFound: Boolean;
    fDOMDocument: TDOMDocument;
//    fDebugLog: TBoldLogForm;
    fBoldSystem: TBoldSystem;
    fSkipMetaClassList: TStringList;
    fTraverseOnlyMetaClassList: TStringList;
    fImportedElementList: TBoldXMIIObjectList;
    fPass: TPass;
    fRemoveRoseArtefacts: Boolean;
    fOwningLink: TBoldUMLXMILink;
    procedure InitializeLists;
    function FindMemberRt(ClassTypeInfo: TBoldClassTypeInfo; QualifiedName: string): TBoldMemberRtInfo;
    procedure ImportAttributeFromString(BoldAttribute: TBoldAttribute; Attributevalue: string);
  protected
    function GetDOMDocument: TDOMDocument;
    property BoldSystem: TBoldSystem read fBoldSystem;
    procedure LoadAndCheck(const Filename: TFilename);
    procedure ImportMetaAssociationEndFromElement(BoldMember: TBoldMember; XMLElement: IXMLDOMElement; Multi: Boolean);
    property DOMDocument: TDOMDocument read GetDOMDocument;
    class function GetUMLName(const aText: String): String;
//    class function UnqualifiedName(const aText: String): String;
    procedure ImportMetaAttributeFromElement(BoldAttribute: TBoldAttribute; XMLElement: IXMLDOMElement);
    function CreateUMLObjectsForMetaObject(XMLElement: IXMLDOMElement): TBoldUMLElementArray;
    function MetaClassAction(XMLElement: IXMLDOMElement): TClassAction;
    function GetAttributeMember(OwningObject: TUMLElement; const AttributeName: String): TBoldAttribute;
    function ImportMetaObject(XMLElement: IXMLDOMElement): TBoldUMLElementArray;
    function ImportMultiplicityAsString(XMLElement: IXMLDOMELement): string;
    function ImportMultiplicityRangeAsString(XMLElement: IXMLDOMELement): string;
    function ImportExpressionAsString(XMLElement: IXMLDOMELement): string;
    function ImportCdataValue(XMLElement: IXMLDOMELement): string;
    procedure ImportXMIObjectList(List: TUMLELementList; XMLElement: IXMLDOMElement; BoldType: TBoldClassTypeInfo);
    property Pass: TPass read fPass;
    property SkipMetaClassList: TStringList read fSkipMetaClassList;
    property TraverseOnlyMetaClassList: TStringList read fTraverseOnlyMetaClassList;
  public
    constructor Create(OwningLink: TBoldUMLXMILink; UMLModel: TUMLModel);
    destructor Destroy; override;
    procedure RawImport;
    property RemoveRoseArtefacts: Boolean read fRemoveRoseArtefacts write fRemoveRoseArtefacts;
    property OwningLink: TBoldUMLXMILink read fOwningLink;

  end;

implementation

uses
  BoldDefs,
  BoldUtils,
  BoldGuard,
  BoldElements,
  BoldAttributes,
  BoldUMLModelSupport,
  BoldUMLAttributes,
  BoldUMLXMICommon,
  BoldsharedStrings;

const
  XMI_CONTENT_ELEMENT: String = 'XMI.content';
  XMI_CORE_MODELELEMENT_NAME = 'Foundation.Core.ModelElement.name';

  { TBoldUMLXMIImporter }

constructor TBoldUMLXMIImporter.Create(OwningLink: TBoldUMLXMILink; UMLModel: TUMLModel);
begin
  fOwningLink := OwningLink;
  fTheUMLModel := UMLModel;
  fBoldSystem := UMLModel.BoldSystem;
//  fDebugLog := TBoldLogForm.Create(nil);
  fImportedElementList := TBoldXMIIObjectList.Create;
  InitializeLists;
end;

destructor TBoldUMLXMIImporter.Destroy;
begin
  FreeAndNil(fDOMDocument);
//  FreeAndNil(fDebugLog);
  FreeAndNil(fSkipMetaClassList);
  FreeAndNil(fTraverseOnlyMetaClassList);
  FreeAndNil(fImportedElementList);
  inherited;
end;

function TBoldUMLXMIImporter.GetDOMDocument: TDOMDocument;
begin
  if not Assigned(fDOMDocument) then
    fDOMDocument := TDOMDocument.Create(nil);
  Result := fDOMDocument;
end;

procedure TBoldUMLXMIImporter.LoadAndCheck(const Filename: TFilename);
var
  ParseError: IXMLDOMParseError;
begin
  FreeAndNil(fDomDocument);
  DOMDocument.validateOnParse := OwningLink.ValidateInput;
  DOMDocument.load(Filename);

  ParseError := DOMDocument.parseError;
  if Assigned(ParseError) and (ParseError.errorCode <> 0) then
    raise EBold.CreateFmt('Error reading/parsing XMI file, %s', [ParseError.reason]);
end;

procedure TBoldUMLXMIImporter.ImportMetaAssociationEndFromElement(BoldMember: TBoldMember ;XMLElement: IXMLDOMElement; Multi: Boolean);
var
  i: Integer;
  ObjectsToAdd: TUMLElementList;
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  ObjectsToAdd := TUMLElementList.Create;
  if Assigned(BoldMember) then
    ClassTypeInfo := TBoldObjectList(BoldMember).BoldRoleRTInfo.ClassTypeInfoOfOtherEnd
  else
    ClassTypeInfo := nil;
  ImportXMIObjectList(ObjectsToAdd, XMLElement,ClassTypeInfo );
  if (pass = PASS2) and Assigned(BoldMember) then
  begin
    if Multi then
    begin
      if BoldMember is TBoldObjectList then
        for i := 0 to ObjectsToAdd.Count - 1 do
          TBoldObjectList(BoldMember).Add(ObjectsToAdd[i])
      else
        raise Exception.Create('Wrong association type, expected multi.');
    end
    else
    begin
      if BoldMember is TBoldObjectReference then
      begin
        if ObjectsToAdd.Count = 1 then
          TBoldObjectReference(BoldMember).BoldObject := ObjectsToAdd[0]
        else if ObjectsToAdd.Count = 0 then
         TBoldObjectReference(BoldMember).BoldObject := nil
        else
          TBoldObjectReference(BoldMember).BoldObject := ObjectsToAdd[0];
   //       raise EBold.Create('Multiple elements to single reference');
      end
      else
        raise Exception.Create('Wrong association type, expected single.');
    end;
  end;
  FreeAndNil(ObjectsToAdd);
end;

procedure TBoldUMLXMIImporter.ImportXMIObjectList(list: TUMLELementList; XMLElement: IXMLDOMElement; BoldType: TBoldClassTypeInfo);
var
  nodeList: IXMLDOMNodeList;
  childElement: IXMLDOMElement;
  objectsToAdd: TBoldUMLElementArray;
  i: integer;
begin
  if XMLElement = nil then
    raise Ebold.create('xxx');
  nodeList := XMLElement.childNodes;
  childElement := FirstElementFromNodeList(nodeList);
  while Assigned(ChildElement) do
  begin
    if (IsNonBlank(GetXMIIdref(ChildElement))) then // Object Reference
      ObjectsToAdd := (fImportedElementList.UMLElementsById[GetXMIIdref(ChildElement)])
    else
      ObjectsToAdd := ImportMetaObject(childElement);
    if Assigned(ObjectsToAdd) and Assigned(list) then
      for i := 0 to ObjectsToAdd.Count-1 do
        if (not Assigned(BoldType)) or ObjectsToAdd[i].BoldClassTypeInfo.BoldIsA(BoldType) then
          list.add(ObjectsToAdd[i]);
    childElement := NextElementFromNodeList(nodeList);
  end;
end;
{
class function TBoldUMLXMIImporter.UnqualifiedName(const aText: String): String;
begin
  Result := Copy(aText, LastDelimiter(':.', aText) + 1, MAXINT);
end;
}
class function TBoldUMLXMIImporter.GetUMLName(const aText: String): String;
begin
  Result := 'UML' + UnqualifiedName(aText); //do not localize
end;

function TBoldUMLXMIImporter.ImportMetaObject(XMLElement: IXMLDOMElement): TBoldUMLElementArray;
var
  NodeList: IXMLDOMNodeList;
  XMIAttributes: IXMLDOMNamedNodeMap;
  Attributevalue: string;
  Element: IXMLDOMElement;
  Action: TClassAction;
  ClassTypeInfo: array [0..1] of TBoldClassTypeInfo;
  ClassTypeInfoCount: integer;
  RTInfo: TBoldMemberRTInfo;
  BoldMember: TBoldmember;
  i: integer;
  ResultIndex: integer;
  XMIAttributeIndex: integer;
  Found: Boolean;

  function CreateOrLocateMetaObject(DOMElement:  IXMLDOMElement): TBoldUMLElementArray;
  var
    Id: TXMIId;
  begin
    Id := GetXMIId(DOMElement);
    if Pass = PASS1 then
    begin
      if IsNonBlank(Id) then
        Result := CreateUMLObjectsForMetaObject(DOMElement)
      else
        Result := nil
    end
    else  // PASS2, objects refered in previous pass must exist, the rest created now
    begin
      if IsNonBlank(Id) then
      begin
        Result := fImportedElementList.ItemsById[Id].UMLObjects;
        if not Assigned(Result) then
          raise Exception.Create('Modelpart for id ' + FormatId(Id) + ' not found');
      end
      else
        Result := CreateUMLObjectsForMetaObject(DOMElement);
    end;
  end;

begin
  Result := nil;
  Action := MetaClassAction(XMLElement);
  case Action of
    caIMPORT:
      Result := CreateOrLocateMetaObject(XMLElement);
    caTraverse:
       ;
    caSkip:
      begin
//        fDebugLog.AddLog(Format('Skipping metaobject: %s', [XMLElement.NodeName]));
        Exit;
      end;
    caUNKNOWN:
      begin
//        fDebugLog.AddLog(Format('Skipping Unknown metaobject: (%s),(%s)', [XMLElement.ParentNode.Nodename, XMLElement.NodeName]));
        Exit;
      end;
  end;
  // Very hardcoded for max two types, and only AssociationClass having two
  if Assigned(Result) then
  begin
    for i := 0 to Result.Count-1 do
      ClassTypeInfo[i] := Result[i].BoldClassTypeInfo;
    ClassTypeInfoCount := Result.Count;
  end
  else if GetUMLName(XMLElement.tagName) = 'AssociationClass' then //do not localize
  begin
    ClassTypeInfo[0] := BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[GetUMLName('Class')]; //do not localize
    ClassTypeInfo[1] := BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[GetUMLName('Association')]; //do not localize
    ClassTypeInfoCount := 2;
  end
  else
  begin
    ClassTypeInfo[0] := BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[GetUMLName(XMLElement.tagName)];
    ClassTypeInfoCount := 1;
  end;

  XMIAttributes :=  XMLElement.attributes;
  for XMIAttributeIndex := 0 to XMIAttributes.length-1 do
  begin
    if XMIAttributes[XMIAttributeIndex].nodeType = NODE_ATTRIBUTE then
    begin
      for i := 0 to ClassTypeInfoCount - 1 do
      begin
        AttributeValue :=  (XMIAttributes[XMIAttributeIndex] as IXMLDOMAttribute).Value;
        if Assigned(ClassTypeInfo[i]) then
          RTInfo := ClassTypeInfo[i].MemberRTInfoByExpressionName[(XMIAttributes[XMIAttributeIndex] as IXMLDOMAttribute).Name]
        else
          RTInfo := nil;
        if Assigned(Result) and Assigned(RTInfo) and (pass = PASS2) then
        begin
          Boldmember := Result[i].boldMembers[RTInfo.Index];
          if rtInfo.IsAttribute then
            ImportAttributeFromString(TBoldAttribute(BoldMember), Attributevalue)
          else if Assigned(fImportedElementList.UMLElementsById[XMIIdforId(AttributeValue)]) then
            with (fImportedElementList.UMLElementsById[XMIIdforId(AttributeValue)]) do
              for ResultIndex := 0 to Count-1 do
                if Items[ResultIndex].BoldClassTypeInfo.BoldIsA((rtInfo as TBoldRoleRTInfo).ClassTypeInfoOfOtherEnd) then
                begin
                  if rtInfo.IsSingleRole then
                    (BoldMember as TBoldObjectReference).BoldObject := Items[ResultIndex]
                  else
                    (BoldMember as TBoldObjectList).add(Items[ResultIndex]);
                end;
        end;
      end;
    end;
  end;

  NodeList := XMLElement.childNodes;
  Element := FirstElementFromNodeList(NodeList);
  while Assigned(Element) do
  begin
    Found := False;
    for i := 0 to ClassTypeInfoCount - 1 do
    begin
      if Assigned(ClassTypeInfo[i]) then
        RTInfo := FindMemberRt(ClassTypeInfo[i], Element.nodeName)
      else
        RTInfo := nil;
      if Assigned(RTInfo) then
      begin
        Found := True;
        if Assigned(Result)  then
          Boldmember := Result[i].boldMembers[RTInfo.Index]
        else
          BoldMember := nil;
        if rtInfo.IsAttribute then
          ImportMetaAttributeFromElement(Boldmember as TBoldAttribute, Element)
        else
        // don't import associations twice
        if (i = 0) or not assigned(ClassTypeInfo[0].MemberRTInfoByExpressionName[UnqualifiedName(Element.nodeName)]) then
          ImportMetaAssociationEndFromElement(Boldmember, Element, rtinfo.IsMultiRole)
      end
    end;
    if not Found then
      ImportXMIObjectList(nil, Element, nil);
    Element := NextElementFromNodeList(NodeList);
  end;
end;

procedure TBoldUMLXMIImporter.ImportMetaAttributeFromElement(BoldAttribute: TBoldAttribute; XMLElement: IXMLDOMElement);
  function ValueIsMultiplicity(DOMElement: IXMLDOMElement): Boolean;
  begin
    Result := UnqualifiedName(DOMElement.nodeName) = 'multiplicity'; //do not localize
  end;

  function ValueIsXmiValue(DOMElement: IXMLDOMElement): Boolean;
  begin
    Result := assigned(DOMElement.getAttributeNode('xmi.value')); //do not localize
//not (ValueIsMultiplicity(DOMElement) or ValueISCData(DOMElement));
  end;

  function ValueIsExpression(DOMElement: IXMLDOMElement): Boolean;
  begin
    result := (DOMElement.childNodes.length > 0) and
      IsNameOfExpressionClass(UnqualifiedName(DOMElement.firstChild.nodeName));
  end;

  function ValueISCData(DOMElement: IXMLDOMElement): Boolean;
  begin
    Result := not (ValueIsXmiValue(DOMElement) or ValueIsMultiplicity(DOMElement) or
                   ValueIsExpression(DOMElement));
// DOMElement.nodeName = XMI_CORE_MODELELEMENT_NAME;
  end;

var
  anAttribValue: String;
begin
  if (pass = PASS1) then
    Exit; // only set attributes on pass 2
  if ValueIsXmiValue(XMLElement) then
  begin
    anAttribValue := BoldSharedStringManager.GetSharedString(VarToStr(XMLElement.getAttribute('xmi.value'))); //do not localize
    if anAttribValue <> '' then
      ImportAttributeFromString(BoldAttribute, anAttribValue);
  end
  else if ValueISCData(XMLElement) then
  begin
    anAttribValue := BoldSharedStringManager.GetSharedString(ImportCdataValue(XMLElement));
    ImportAttributeFromString(BoldAttribute, anAttribValue);
  end
  else if ValueIsMultiplicity(XMLElement) then
    BoldAttribute.AsString := BoldSharedStringManager.GetSharedString(ImportMultiplicityAsString(XMLElement.firstChild as IXMLDOMElement))
  else if ValueIsExpression(XMLElement) then
    BoldAttribute.AsString := BoldSharedStringManager.GetSharedString(ImportExpressionAsString(XMLElement.firstChild as IXMLDOMElement))
  else
  begin
  { TODO : Further types }
  end;
end;

function TBoldUMLXMIImporter.MetaClassAction(XMLElement: IXMLDOMElement): TClassAction;
begin
  if Assigned(BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[GetUMLName(XMLElement.nodeName)]) then
    Result := caIMPORT
  else if GetUMLName(XMLElement.nodeName) = 'UMLAssociationClass' then //do not localize
    Result := caIMPORT
  else if SkipMetaClassList.IndexOf(XMLElement.nodeName) <> -1 then
    Result := caSkip
  else if TraverseOnlyMetaClassList.IndexOf(XMLElement.nodeName) <> -1 then
    Result := caTraverse
  else
    Result := caUNKNOWN;
end;

function TBoldUMLXMIImporter.GetAttributeMember(OwningObject: TUMLElement; const AttributeName: String): TBoldAttribute;
var
  aMemberRTInfo  : TBoldMemberRTInfo;
begin
  Result := nil;
  aMemberRTInfo := OwningObject.BoldClassTypeInfo.MemberRTInfoByExpressionName[UnqualifiedName(AttributeName)];
  if aMemberRTInfo is TBoldAttributeRTInfo then
  begin
    assert(OwningObject.BoldMembers[aMemberRTInfo.Index] is TBoldAttribute);
    Result := TBoldAttribute(OwningObject.BoldMembers[aMemberRTInfo.Index]);
  end;
end;

// Due to implementation of multiple inheritance more than one UMLObject may be created
function TBoldUMLXMIImporter.CreateUMLObjectsForMetaObject(XMLElement: IXMLDOMElement): TBoldUMLElementArray;
var
  UMLClassName: String;
  XMIId: TXMIId;
  i: integer;
  Element1, Element2: TUMLModelElement;
begin
  XMIId := GetXMIId(XMLElement);
//  fDebugLog.AddLog(Format('Creating model part: %s(%s)', [XMLElement.nodeName, FormatId(XMIId)]));
  if Assigned(fImportedElementList.ItemsById[XMIId]) then
  begin
    raise EBoldInternal.Create('Modelpart already exists');
  end
  else
  begin
    // Handle Model and AssociationClass separately
    UMLClassName := GetUMLName(XMLElement.nodeName);
    if UMLClassName = 'UMLModel' then //do not localize
    begin
      if not fTheUMLModelFound then
      begin
        // We already have a model, just associate it with ID.
        Result := fImportedElementList.Add(XMIId, fTheUMLModel);
        fTheUMLModelFound := True;
      end
      else
      begin
        Element1 := BoldSystem.CreateNewObjectByExpressionName(UMLClassName, False) as TUMLModelElement;
//        TBoldUMLSupport.EnsureBoldTaggedValues(Element1);
        Result := fImportedElementList.Add(XMIId, Element1);
      end;
    end
    else if UMLClassName = 'UMLAssociationClass' then //do not localize
    begin
      Element1 := BoldSystem.CreateNewObjectByExpressionName('UMLClass', False) as TUMLModelElement; //do not localize
//      TBoldUMLSupport.EnsureBoldTaggedValues(Element1);
      Element2 := BoldSystem.CreateNewObjectByExpressionName('UMLAssociation', False) as TUMLModelElement; //do not localize
//      TBoldUMLSupport.EnsureBoldTaggedValues(Element2);
      Result := fImportedElementList.Add(XMIId, Element1, Element2);
      (Result[1] as TUMLAssociation).Class_ := (Result[0] as TUMLClass);
    end
    else
    begin
      Element1 := BoldSystem.CreateNewObjectByExpressionName(UMLClassName, False) as TUMLModelElement;
//      TBoldUMLSupport.EnsureBoldTaggedValues(Element1);
      Result := fImportedElementList.Add(XMIId, Element1);
    end;
  end;
  if XMIID.UUId <> ''then
    for i := 0 to Result.Count-1 do
      if Result[i] is TUMLModelElement then
        TBoldUMLSupport.AddToolId(TUMLModelElement(Result[i]), XMIID.UUId)
end;


procedure TBoldUMLXMIImporter.InitializeLists;
begin
  //  Initialize skip list
  fSkipMetaClassList := TStringList.Create;
  fTraverseOnlyMetaClassList := TStringList.Create;
//  if XMIExporter = UNISYS_TCR_2 then
//    SkipMetaClassList.Add('Diagramming.Diagram');
  // Initialize travsrse list
  SkipMetaClassList.Sorted := True;
  TraverseOnlyMetaClassList.Sorted := True;
end;

function TBoldUMLXMIImporter.ImportMultiplicityAsString(XMLElement: IXMLDOMELement): string;
var
  childElement: IXMLDOMELement;
  nodeList: IXMLDOMNodeList;
  Ranges: TStringList;
begin
  if UnqualifiedName(XMLElement.nodeName) <> 'Multiplicity' then //do not localize
    raise EBold.CreateFmt('Wrong nodename: %s', [XMLElement.nodeName]);

  if (XMLElement.childNodes.length = 0) or (XMLElement.firstChild.childNodes.length = 0) then
  begin
    Result := ImportCdataValue(XMLElement);
    if result = '' then
      result := '0..*'; //do not localize
  end
  else
  begin
    if UnqualifiedName(XMLElement.firstChild.nodeName) <> 'range' then //do not localize
      raise EBold.CreateFmt('Wrong nodename: %s', [XMLElement.nodeName]);

    nodeList := XMLElement.firstChild.childNodes;
    Ranges := TStringList.Create;
    try
      childElement := FirstElementFromNodeList(nodeList);
      while Assigned(ChildElement) do
      begin
        Ranges.Add(ImportMultiplicityRangeAsString(childElement));
        childElement := NextElementFromNodeList(nodeList);
      end;
      Result := BoldSeparateStringList(Ranges, ', ', '', '');
    finally
      FreeAndNil(Ranges);
    end;
  end;
end;

function TBoldUMLXMIImporter.ImportMultiplicityRangeAsString(XMLElement: IXMLDOMELement): string;
var
  Lower, Upper: string;
  childElement: IXMLDOMELement;
  nodeList: IXMLDOMNodeList;
  attr: IXMLDOMAttribute;
begin
  if UnqualifiedName(XMLElement.nodeName) <> 'MultiplicityRange' then //do not localize
    raise EBold.CreateFmt('Wrong nodename: %s', [XMLElement.nodeName]);

  attr := XMLElement.getAttributeNode('lower'); //do not localize
  if assigned(attr) then
    Lower := attr.value;

  attr := XMLElement.getAttributeNode('upper'); //do not localize
  if assigned(attr) then
    Upper := attr.value;

  nodeList := XMLElement.childNodes;
  childElement := FirstElementFromNodeList(nodeList);
  while Assigned(ChildElement) do
  begin
    if UnqualifiedName(childElement.nodeName) = 'lower' then //do not localize
      Lower := ImportCdataValue(childElement)
    else if UnqualifiedName(childElement.nodeName) = 'upper' then //do not localize
      Upper := ImportCdataValue(childElement)
    else
      raise EBoldInternal.Create('Import error');
    childElement := NextElementFromNodeList(nodeList);
  end;

  if lower = '-1' then
    lower := '*';
  if upper = '-1' then
    upper := '*';

  if (Lower = Upper) then
    Result := Lower
  else
    Result := Format('%s..%s', [Lower, Upper]);
end;

function TBoldUMLXMIImporter.ImportCdataValue(XMLElement: IXMLDOMELement): string;
begin
  if XMLElement.childNodes.length = 1 then
  begin
    if VarIsNull(XMLElement.firstChild.nodeValue) then
      result := ''
    else
      Result := XMLElement.firstChild.nodeValue
  end
  else if XMLElement.childNodes.length = 0 then
    Result := ''
  else
    raise EBoldInternal.Create('Node problem');
end;

procedure TBoldUMLXMIImporter.RawImport;
var
  RootElement, ContentElement: IXMLDOMElement;
  AllElements: TUMLModelElementList;
  i: integer;

  function FindContent(XMLElement: IXMLDOMElement): IXMLDOMElement;
  var
    NodeList: IXMLDOMNodeList;
    i: Integer;
  begin
    Result := nil;
    if XMLElement.nodeName = XMI_CONTENT_ELEMENT then
      Result := XMLElement as IXMLDOMElement
    else
    begin
      NodeList := XMLElement.childNodes;
      for i := 0 to NodeList.length - 1 do
      begin
        if Nodelist[i].nodeType = NODE_ELEMENT then
          Result := FindContent(NodeList[i] as IXMLDOMElement);
        if Assigned(Result)then
          Break;
      end;
    end;
  end;

begin
  LoadAndCheck(OwningLink.Filename);
//  fDebugLog.Show;
  fImportedElementList.Clear;

  DOMDocument.preserveWhiteSpace := True;
  RootElement := DOMDocument.documentElement;

  if not Assigned(RootElement) then
    raise EBold.Create('RootElement missing');
  ContentElement := FindContent(RootElement);
  if not Assigned(ContentElement) then
    raise EBold.Create('Content missing');

//  fDebugLog.AddLog('PASS 1');
  fPass := PASS1;
  ImportXMIObjectList(nil, ContentElement, nil);
//  fDebugLog.AddLog('PASS 2');
  fPass := PASS2;
  ImportXMIObjectList(nil, ContentElement, nil);

  AllElements := fBoldSystem.ClassByExpressionName['UMLModelElement'] as TUMLModelElementList; //do not localize
  for i := AllElements.Count-1 downto 0 do
    TBoldUMLSupport.EnsureBoldTaggedValues(AllElements[i]);

//  fDebugLog.AddLog('DONE');
end;

function TBoldUMLXMIImporter.FindMemberRt(
  ClassTypeInfo: TBoldClassTypeInfo;
  QualifiedName: string): TBoldMemberRtInfo;
var
  AlternateRt: TBoldMemberRTInfo;
  UnqualifiedMemberName: string;
  MemberNameWithClass: string;
begin
  UnqualifiedMemberName := UnqualifiedName(QualifiedName);
  result := ClassTypeInfo.MemberRTInfoByExpressionName[UnqualifiedMemberName];
  AlternateRt := ClassTypeInfo.MemberRTInfoByExpressionName[UnqualifiedMemberName + '_'];
  if assigned(AlternateRt) then
  begin
    MemberNameWithClass := FindDefiningClass(AlternateRt).ModelName + '.' + UnqualifiedMemberName;
    if pos(MemberNameWithClass, QualifiedName) > 0 then
      result := AlternateRt;
  end;
end;

function TBoldUMLXMIImporter.ImportExpressionAsString(
  XMLElement: IXMLDOMELement): string;
var
  attr: IXMLDOMAttribute;
  nodeList: IXMLDOMNodeList;
  childElement: IXMLDOMElement;
begin
  attr := XMLElement.getAttributeNode('body'); //do not localize
  if assigned(attr) then
    result := attr.value
  else
  begin
    result := '';
    nodeList := XMLElement.childNodes;
    childElement := FirstElementFromNodeList(nodeList);
    while assigned(childElement) do
    begin
      if UnqualifiedName(childElement.nodeName) = 'body' then //do not localize
        result := ImportCdataValue(childElement);
      childElement := NextElementFromNodeList(nodeList);
    end;
  end;
end;

procedure TBoldUMLXMIImporter.ImportAttributeFromString(
  BoldAttribute: TBoldAttribute; AttributeValue: string);
var
  colonpos: integer;
begin
  if OwningLink.TranslateRoseTaggedValues and
    (BoldAttribute.DisplayName = 'UMLTaggedValue.tag') and //do not localize
    (pos('RationalRose$', AttributeValue) = 1) and //do not localize
    (pos(':', AttributeValue) <> 0) then
  begin
    AttributeValue := Copy(AttributeValue, length('RationalRose$')+1, MAXINT); //do not localize
    colonpos := pos(':', AttributeValue);
    BoldAttribute.AsString := Copy(AttributeValue, 1, colonpos-1) + '.' +
      Copy(AttributeValue, colonpos+1, MAXINT);
  end
  else if BoldAttribute is TBABoolean then
    BoldAttribute.StringRepresentation[3] := AttributeValue
  else
  begin
     if (BoldAttribute is TBAVisibilityKind) and
        (AttributeValue = 'package') then //do not localize
      AttributeValue := 'public'; //do not localize
    BoldAttribute.AsString := AttributeValue;
  end
end;

end.
