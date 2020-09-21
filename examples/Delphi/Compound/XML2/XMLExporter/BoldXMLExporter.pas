// /usr/local/cvsroot/tp/boldfordelphi/examples/Delphi/Compound/XML2/XMLExporter/BoldXMLExporter.pas,v 1.1 2003/01/21 07:24:00 jhogstrom Exp

{$Include Bold.inc}

unit BoldXMLExporter;

interface

uses
  MSXML_TLB,
  BoldManipulators,
  BoldSystem,
  BoldXMLExportInterfaces,
  BoldXMLExportAdapters,
  BoldSubscription,
  classes,
  {$IFNDEF DELPHI6_OR_LATER}
  Variants,
  {$ENDIF}
  Sysutils;

type
  {forward declarations}
  TBoldXMLExporterHandle = class;
  TBoldXMLExporter = class;

  TBoldXMLExporter = class
  private
    fReferencedObjects: TInterfaceList;
    fUseBoldId: Boolean;
    fBoldManipulator: TBoldManipulator;
    fExportLinks: Boolean;
    fExportDerivedAttributes: Boolean;
    function AppendNewElement(ParentElement: IXMLDOMElement; tagname: string): IXMLDOMElement; //move to parent class
    procedure ProduceHeader(XMLElement: IXMLDOMElement); // move to parent class??
    function QualifiedAttributeName(Attribute: IBoldXMLAttribute): string;
    function QualifiedReferenceName(Reference: IBoldXMLReference): string;
    function QualifiedClassName(Obj: IBoldXMLObject): string;
    procedure AddReferencedObject(Obj: IBoldXMLObject);
    procedure ClearReferencedObjects;
    function FindInDocument(Obj: IBoldXMLObject; DocElement: IXMLDOMElement): Boolean;
    function GetAttributeName: string;
  protected
    {XML production methods}
    procedure AttributeAsElement(Attribute: IBoldXMLAttribute; XMLElement: IXMLDOMElement);
    function BooleanAsString(Value: IBoldXMLAttribute): string; //move to parent class
    procedure CompositeAsElement(Composite: IBoldXMLReference; XMLElement: IXMLDOMElement);
    procedure ContentsFromRoot(RootObj: IBoldXMLObject; XMLElement: IXMLDOMElement);
    procedure EmbeddedObject(Attribute: IBoldXMLAttribute; XMLElement: IXMLDOMElement);
    procedure EnumAttribute(Attribute: IBoldXMLAttribute; XMLElement: IXMLDOMElement);
    function  IdOfObject(Obj: IBoldXMLObject): string;
    procedure ObjectAsElement(Obj: IBoldXMLObject; XMLElement: IXMLDOMElement);
    procedure ObjectContents(Obj: IBoldXMLObject; XMLElement: IXMLDOMElement);
    procedure ObjRefOrDataValue(Attribute: IBoldXMLAttribute; XMLElement: IXMLDOMElement);
    procedure OtherLinks(Obj: IBoldXMLObject; XMLElement: IXMLDOMElement);
    procedure ReferenceAsElement(Reference: IBoldXMLReference; XMLElement: IXMLDOMElement);
    procedure ReferencingElement(Obj: IBoldXMLObject; XMLElement: IXMLDOMElement);
    procedure RequiredTypeDefinitions(RootObj: IBoldXMLObject; XMLElement: IXMLDOMElement);
    procedure SvAttributeContents(Attribute: IBoldXMLAttribute; XMLElement: IXMLDOMElement);
    procedure RawExport(RootObj: IBoldXMLObject; const FileName: string);
    procedure ExportReferencedObjects(XMLElement: IXMLDOMElement; ExportedReferencedObjectsClasses: array of string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExportObjects(ObjectList: TBoldObjectList; XMLElement: IXMLDOMElement; ExportedReferencedObjectsClasses: array of string);
    procedure ExportObject(Obj: TBoldObject; XMLElement: IXMLDOMElement; ExportedReferencedObjectsClasses: array of string);
    property BoldManipulator: TBoldManipulator read fBoldManipulator write fBoldManipulator;
    property UseBoldId: Boolean read fUseBoldId write fUseBoldId;
    property ExportLinks: Boolean read fExportLinks write fExportLinks;
    property ExportDerivedAttributes: Boolean read fExportDerivedAttributes write fExportDerivedAttributes;
  end;

  TBoldXMLExporterHandle = class(TBoldSubscribableComponent)
  private
    fXMLExporter: TBoldXMLExporter;
    fUseBoldId: Boolean;
    fBoldManipulator: TBoldManipulator;
    fExportLinks: Boolean;
    fExportDerivedAttributes: Boolean;
    procedure SetBoldManipulator(const Value: TBoldManipulator);
    procedure SetUseBoldId(const Value: Boolean);
    procedure SetExportLinks(const Value: Boolean);
    procedure SetExportDerivedAttributes(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property XMLExporter: TBoldXMLExporter read fXMLExporter;
  published
    property UseBoldId: Boolean read fUseBoldId write SetUseBoldId;
    property BoldManipulator: TBoldManipulator read fBoldManipulator write SetBoldManipulator;
    property ExportLinks: Boolean read fExportLinks write SetExportLinks;
    property ExportDerivedAttributes: Boolean read fExportDerivedAttributes write SetExportDerivedAttributes;
  end;

  procedure Register;

implementation

uses
  BoldRev,
  BoldId;

const
  XML_NODENAME_ROOT = 'XML';
  XML_NODENAME_HEADER = 'XML.header';
  XML_NODENAME_CONTENT = 'XML.content';
  XML_ATTRIBUTENAME_ID = 'XML.id';
  XML_ATTRIBUTENAME_IDREF = 'XML.idref';
  XML_ATTRIBUTENAME_VALUE = 'XML.value';
  XML_ELEMENT_BOLDID = 'XML.boldID';


procedure Register;
begin
  RegisterComponents('Bold XML', [TBoldXMLExporterHandle]);
end;

{ TBoldXMLExporter }

procedure TBoldXMLExporter.AddReferencedObject(Obj: IBoldXMLObject);
begin
  fReferencedObjects.Add(obj);
end;

function TBoldXMLExporter.AppendNewElement(ParentElement: IXMLDOMElement;
  tagname: string): IXMLDOMElement;
begin
  result := ParentElement.ownerDocument.createElement(tagname);
  ParentElement.appendChild(Result);
end;

procedure TBoldXMLExporter.AttributeAsElement(Attribute: IBoldXMLAttribute;
  XMLElement: IXMLDOMElement);
begin
  SvAttributeContents(Attribute, XMLElement);
end;

function TBoldXMLExporter.BooleanAsString(
  Value: IBoldXMLAttribute): string;
begin
  assert(Value.IsBoolean);
  if Value.AsBoolean then
    result := 'true'
  else
    result := 'false';
end;

procedure TBoldXMLExporter.CompositeAsElement(Composite: IBoldXMLReference;
  XMLElement: IXMLDOMElement);
var
  CompositeName: string;
  CompositeNode: IXMLDOMElement;
  i: Integer;
  Objects: IBoldXMLObjectList;
begin
  CompositeName := QualifiedReferenceName(Composite);
  CompositeNode := AppendNewElement(XMLElement, CompositeName);
  if not Composite.IsMulti then
  begin
    if assigned(Composite.SingleObject) then
      ObjectAsElement(Composite.SingleObject, CompositeNode);
  end else
  begin
    Objects := Composite.Objects;
    for i := 0 to Objects.Count-1 do
      ObjectAsElement(Objects.GetObject(i), CompositeNode);
  end;
end;

procedure TBoldXMLExporter.ContentsFromRoot(RootObj: IBoldXMLObject;
  XMLElement: IXMLDOMElement);
var
  ContentElement: IXMLDOMElement;
begin
  ContentElement := AppendNewElement(XMLElement, XML_NODENAME_CONTENT);
  ObjectAsElement(RootObj, ContentElement);
  OtherLinks(RootObj, ContentElement);
  RequiredTypeDefinitions(RootObj, ContentElement);
end;

constructor TBoldXMLExporter.Create;
begin
  inherited Create;
  fReferencedObjects := TInterfaceList.Create;
  fUseBoldId := True;
  fExportLinks := false;
  fExportDerivedAttributes := false;
end;

destructor TBoldXMLExporter.Destroy;
begin
  FreeAndNil(fReferencedObjects);
  inherited;
end;

procedure TBoldXMLExporter.EmbeddedObject(Attribute: IBoldXMLAttribute;
  XMLElement: IXMLDOMElement);
var
  Obj: IBoldXMLObject;
  ObjectNode: IXMLDOMElement;
begin
  assert(Attribute.IsObject);
  Obj := Attribute.AsObject;
  ObjectNode := AppendNewElement(XMLElement, QualifiedClassName(Obj));

  if UseBoldId then
    ObjectNode.setAttribute(XML_ELEMENT_BOLDID, IdOfObject(Obj));

  ObjectContents(Obj, ObjectNode);
end;

procedure TBoldXMLExporter.EnumAttribute(Attribute: IBoldXMLAttribute;
  XMLElement: IXMLDOMElement);
var
  AttrNode: IXMLDOMElement;
begin
  assert(Attribute.IsEnum or Attribute.IsBoolean);
  AttrNode := AppendNewElement(XMLElement, QualifiedAttributeName(Attribute));

  if UseBoldId then
    AttrNode.setAttribute(XML_ELEMENT_BOLDID, Attribute.BoldId(BoldManipulator));
    
  if Attribute.IsBoolean then
    AttrNode.setAttribute(XML_ATTRIBUTENAME_VALUE, BooleanAsString(Attribute))
  else
    AttrNode.setAttribute(XML_ATTRIBUTENAME_VALUE, Attribute.AsString);
end;

procedure TBoldXMLExporter.ExportReferencedObjects(XMLElement: IXMLDOMElement; ExportedReferencedObjectsClasses: array of string);
var
  CurObj: IBoldXMLObject;
  function ExportLinksforClass(ClassName: string): Boolean;
  var
    i: integer;
  begin
    result := false;
    for i:= 0 to High(ExportedReferencedObjectsClasses) do
    begin
      Result := (ClassName = ExportedReferencedObjectsClasses[i]);
      if Result then Break;
    end;
  end;
begin
  while (fReferencedObjects.Count > 0) do
  begin
    CurObj := fReferencedObjects[0] as IBoldXMLObject;
    if ExportLInksforClass(CurObj.QualifiedClassName) and not FindInDocument(CurObj, XMLElement.OwnerDocument.documentElement) then
      ObjectAsElement(CurObj, XMLElement);
    fReferencedObjects.Remove(CurObj);
  end;
end;

function TBoldXMLExporter.FindInDocument(
  Obj: IBoldXMLObject; DocElement: IXMLDOMElement): Boolean;
var
  NodeList: IXMLDOMNodeList;
  CurElement: IXMLDomElement;
  Attr: IXMLDOMAttribute;
  Id: String;
  i: integer;
begin
  Result := false;
  Nodelist := DocElement.getElementsByTagName(Obj.QualifiedClassName);
  for i:= 0 to NodeList.length - 1 do
  begin
    CurElement := NodeList.item[i] as IXMLDOMElement;
    Attr := CurElement.getAttributeNode(GetAttributeName);
    if Assigned(Attr) then
    begin
      id := VarToStr(Attr.value);
      Result := (id = IdOfObject(Obj));
    end;
    if Result then Break;
  end;
end;

function TBoldXMLExporter.IdOfObject(Obj: IBoldXMLObject): string;
begin
  if UseBoldId then
    Result := Obj.BoldId(BoldManipulator)
  else
    result := Obj.LocalId;
end;

procedure TBoldXMLExporter.ObjectAsElement(Obj: IBoldXMLObject;
  XMLElement: IXMLDOMElement);
var
  ObjectNode: IXMLDOMElement;
begin
  ObjectNode := AppendNewElement(XMLElement, QualifiedClassName(Obj));
  ObjectNode.setAttribute(GetAttributeName, IdOfObject(Obj));
  ObjectContents(Obj, ObjectNode);
end;

procedure TBoldXMLExporter.ObjectContents(Obj: IBoldXMLObject;
  XMLElement: IXMLDOMElement);
var
  i: Integer;
  Attribute: IBoldXMLAttribute;
  Reference: IBoldXMLReference;
  AttributeList: IBoldXMLAttributeList;
  ReferenceList: IBoldXMLReferenceList;
begin
  AttributeList := Obj.Attributes;
  for i := 0 to AttributeList.Count-1 do
  begin
    Attribute := AttributeList.Attribute(i);
    if (not Attribute.IsDerived) or (Attribute.IsDerived and ExportDerivedAttributes) then
      AttributeAsElement(Attribute, XMLElement);
  end;
  ReferenceList := Obj.References;
  for i := 0 to ReferenceList.Count-1 do
  begin
    Reference := ReferenceList.Reference(i);
    if not Reference.IsDerived and
      not Reference.IsComposite then
      ReferenceAsElement(Reference, XMLElement);
  end;
  for i := 0 to ReferenceList.Count-1 do
  begin
    Reference := ReferenceList.Reference(i);
    if not Reference.IsDerived and
      Reference.IsComposite then
      CompositeAsElement(Reference, XMLElement);
  end;
end;

procedure TBoldXMLExporter.ObjRefOrDataValue(Attribute: IBoldXMLAttribute;
  XMLElement: IXMLDOMElement);
begin
  XMLElement.appendChild(XMLElement.ownerDocument.createTextNode(Attribute.AsString));
end;

procedure TBoldXMLExporter.OtherLinks(Obj: IBoldXMLObject;
  XMLElement: IXMLDOMElement);
begin

end;

procedure TBoldXMLExporter.ProduceHeader(XMLElement: IXMLDOMElement);
begin
  AppendNewElement(XMLElement, XML_NODENAME_HEADER);
end;

function TBoldXMLExporter.QualifiedAttributeName(
  Attribute: IBoldXMLAttribute): string;
begin
  result := Attribute.QualifiedName;
end;

function TBoldXMLExporter.QualifiedClassName(Obj: IBoldXMLObject): string;
begin
  result := Obj.QualifiedClassName;
end;

function TBoldXMLExporter.QualifiedReferenceName(
  Reference: IBoldXMLReference): string;
begin
  result := Reference.QualifiedName;
end;

procedure TBoldXMLExporter.RawExport(RootObj: IBoldXMLObject; const FileName: string);
var
  aDom: TDOMDocument;
begin
  aDom := TDOMDocument.Create(nil);
  aDom.documentElement := aDom.createElement(XML_NODENAME_ROOT);

  ProduceHeader(aDom.documentElement);
  ContentsFromRoot(RootObj, aDom.documentElement);
  aDom.DefaultInterface.save(FileName);
end;

procedure TBoldXMLExporter.ReferenceAsElement(Reference: IBoldXMLReference;
  XMLElement: IXMLDOMElement);
var
  ReferenceName: string;
  ReferenceNode: IXMLDOMElement;
  i: Integer;
  Objects: IBoldXMLObjectList;
begin
  ReferenceName := QualifiedReferenceName(Reference);
  if not Reference.IsMulti then
  begin
    if assigned(Reference.SingleObject) then
    begin
      ReferenceNode := AppendNewElement(XMLElement, ReferenceName);
      ReferencingElement(Reference.SingleObject, ReferenceNode);
      AddReferencedObject(Reference.SingleObject);
    end;
  end else
  begin
    if Reference.Objects.Count > 0 then
    begin
      ReferenceNode := AppendNewElement(XMLElement, ReferenceName);
      Objects := Reference.Objects;
      for i := 0 to Objects.Count-1 do
      begin
        ReferencingElement(Objects.GetObject(i), ReferenceNode);
        AddReferencedObject(Objects.GetObject(i));
      end;
    end;
  end;
end;

procedure TBoldXMLExporter.ReferencingElement(Obj: IBoldXMLObject;
  XMLElement: IXMLDOMElement);
var
  ElementNode: IXMLDOMElement;
begin
  ElementNode := AppendNewElement(XMLElement, QualifiedClassName(Obj));
  ElementNode.setAttribute(XML_ATTRIBUTENAME_IDREF, IdOfObject(Obj));
end;

procedure TBoldXMLExporter.RequiredTypeDefinitions(RootObj: IBoldXMLObject;
  XMLElement: IXMLDOMElement);
begin

end;

procedure TBoldXMLExporter.SvAttributeContents(
  Attribute: IBoldXMLAttribute; XMLElement: IXMLDOMElement);
var
  ValueElement: IXMLDOMElement;
begin
  if Attribute.IsObject then
  begin
    ValueElement := AppendNewElement(XMLElement, QualifiedAttributeName(Attribute));
    EmbeddedObject(Attribute, ValueElement);
  end
  else
  if Attribute.IsEnum or Attribute.IsBoolean then
    EnumAttribute(Attribute, XMLElement)
  else
  begin
    ValueElement := AppendNewElement(XMLElement, QualifiedAttributeName(Attribute));
    if UseBoldId then
      ValueElement.setAttribute(XML_ELEMENT_BOLDID, Attribute.BoldId(BoldManipulator));

    ObjRefOrDataValue(Attribute, ValueElement);
  end;
end;

procedure TBoldXMLExporter.ExportObjects(ObjectList: TBoldObjectList; XMLElement: IXMLDOMElement;
            ExportedReferencedObjectsClasses: array of string);
var
  i: integer;
  Adapter: TBoldXMLExportObjectAdapter;
begin
  ClearReferencedObjects;
  for i:= 0 to ObjectList.Count - 1 do
  begin
    Adapter := TBoldXMLExportObjectAdapter.Create(ObjectList[i]);
    ObjectAsElement(Adapter, XMLElement);
  end;
  if ExportLinks then
    ExportReferencedObjects(XMLElement, ExportedReferencedObjectsClasses);
end;

function TBoldXMLExporter.GetAttributeName: string;
begin
  if UseBoldId then
    Result := XML_ELEMENT_BOLDID
  else
    Result := XML_ATTRIBUTENAME_ID;
end;

procedure TBoldXMLExporter.ExportObject(Obj: TBoldObject;
  XMLElement: IXMLDOMElement; ExportedReferencedObjectsClasses: array of string);
var
  Adapter: TBoldXMLExportObjectAdapter;
begin
  ClearReferencedObjects;
  Adapter := TBoldXMLExportObjectAdapter.Create(Obj);
  ObjectAsElement(Adapter, XMLElement);
  if ExportLinks then
    ExportReferencedObjects(XMLElement, ExportedReferencedObjectsClasses);
end;

procedure TBoldXMLExporter.ClearReferencedObjects;
begin
  fReferencedObjects.Clear;
end;

{ TBoldXMLExporterHandle }

constructor TBoldXMLExporterHandle.Create(AOwner: TComponent);
begin
  inherited;
  fXMLExporter := TBoldXMLExporter.Create;
end;

destructor TBoldXMLExporterHandle.Destroy;
begin
  FreeAndNil(fXMLExporter);
  inherited;
end;

procedure TBoldXMLExporterHandle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = fBoldManipulator) and (Operation = opRemove) then
    fBoldManipulator := nil;
end;

procedure TBoldXMLExporterHandle.SetBoldManipulator(
  const Value: TBoldManipulator);
begin
  if (fBoldManipulator <> Value) then
  begin
    fBoldManipulator := Value;
    fXMLExporter.BoldManipulator := Value;
  end;
end;

procedure TBoldXMLExporterHandle.SetExportDerivedAttributes(
  const Value: Boolean);
begin
  if (fExportDerivedAttributes <> Value) then
  begin
    fExportDerivedAttributes := Value;
    fXMLExporter.ExportDerivedAttributes := Value;
  end;
end;

procedure TBoldXMLExporterHandle.SetExportLinks(const Value: Boolean);
begin
  if (fExportLinks <> Value) then
  begin
    fExportLinks := Value;
    fXMLExporter.ExportLinks := Value;
  end;
end;

procedure TBoldXMLExporterHandle.SetUseBoldId(const Value: Boolean);
begin
  if (fUseBoldId <> Value) then
  begin
    fUseBoldId := Value;
    fXMLExporter.UseBoldId := Value;
  end;
end;

initialization
  BoldRegisterModuleVersion('$Workfile: BoldXMLExporter.pas $ 1.1 2003/01/21 07:24:00');

end.
