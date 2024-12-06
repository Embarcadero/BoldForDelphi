
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldXMI10Exporter;

interface

uses
  Variants,
  BoldContainers,
  BoldLogForm,

  BoldUMLXMILink,
  BoldMOFInterfaces,
  Bold_MSXML_TLB,
  Sysutils,
  BoldStringList,
  BoldUMLDTDData,
  Classes;

type

  TBoldXMI10Exporter = class
  private
    fOwningLink: TBoldUMLXMILink;
    fProcessedDTD: TBoldUMLDTDReader;
    function AppendNewElement(ParentElement: IXMLDOMElement; tagname: string): IXMLDOMElement;
    procedure ProduceHeader(XMLElement: IXMLDOMElement);
    function QualifiedAttributeName(Attribute: IBoldMOFAttribute): string;
    function QualifiedReferenceName(Reference: IBoldMOFReference): string;
    function QualifiedClassName(Obj: IBoldMOFObject): string;
    {XMI production methods}
    procedure AttributeAsElement(Attribute: IBoldMOFAttribute; XMLElement: IXMLDOMElement);
    function BooleanAsString(Value: IBoldMOFAttribute): string;
    procedure CompositeAsElement(Composite: IBoldMOFReference; XMLElement: IXMLDOMElement);
    procedure ContentsFromRoot(RootObj: IBoldMOFObject; XMLElement: IXMLDOMElement);
    procedure EmbeddedObject(Attribute: IBoldMOFAttribute; XMLElement: IXMLDOMElement);
    procedure EnumAttribute(Attribute: IBoldMOFAttribute; XMLElement: IXMLDOMElement);
    function IdOfObject(Obj: IBoldMOFObject): string;
    procedure ObjectAsElement(Obj: IBoldMOFObject; XMLElement: IXMLDOMElement);
    procedure ObjectContents(Obj: IBoldMOFObject; XMLElement: IXMLDOMElement);
    procedure ObjRefOrDataValue(Attribute: IBoldMOFAttribute; XMLElement: IXMLDOMElement);
    procedure OtherLinks(Obj: IBoldMOFObject; XMLElement: IXMLDOMElement);
    procedure ReferenceAsElement(Reference: IBoldMOFReference; XMLElement: IXMLDOMElement);
    procedure ReferencingElement(Obj: IBoldMOFObject; XMLElement: IXMLDOMElement);
    procedure RequiredTypeDefinitions(RootObj: IBoldMOFObject; XMLElement: IXMLDOMElement);
    procedure SvAttributeContents(Attribute: IBoldMOFAttribute; XMLElement: IXMLDOMElement);
  public
    constructor Create(OwningLink: TBoldUMLXMILink);
    destructor Destroy; override;
    procedure RawExport(RootObj: IBoldMOFObject);
    property OwningLink: TBoldUMLXMILink read fOwningLink;
  end;

implementation

uses
  BoldDefs,
  BoldUtils,
  BoldUMLTypes;

{ TBoldUMLXMIExporter }

const
  XMI_NODENAME_ROOT = 'XMI';
  XMI_NODENAME_HEADER = 'XMI.header';
  XMI_NODENAME_CONTENT = 'XMI.content';
  XMI_NODENAME_METAMODEL = 'XMI.metamodel';
  XMI_ATTRIBUTENAME_ID = 'xmi.id';
  XMI_ATTRIBUTENAME_IDREF = 'xmi.idref';
  XMI_ATTRIBUTENAME_VALUE = 'xmi.value';
  XMI_ATTRIBUTENAME_VERSION = 'xmi.version';
  XMI_ATTRIBUTENAME_NAME = 'xmi.name';

function TBoldXMI10Exporter.AppendNewElement(
  ParentElement: IXMLDOMElement; tagname: string): IXMLDOMElement;
begin
  result := ParentElement.ownerDocument.createElement(tagname);
  ParentElement.appendChild(Result);
end;

procedure TBoldXMI10Exporter.AttributeAsElement(Attribute: IBoldMOFAttribute;
  XMLElement: IXMLDOMElement);
begin
  SvAttributeContents(Attribute, XMLElement);
end;

function TBoldXMI10Exporter.BooleanAsString(Value: IBoldMOFAttribute): string;
begin
  assert(Value.IsBoolean);
  if Value.AsBoolean then
    result := 'true'
  else
    result := 'false';
end;

procedure TBoldXMI10Exporter.CompositeAsElement(Composite: IBoldMOFReference;
  XMLElement: IXMLDOMElement);
var
  CompositeName: string;
  CompositeNode: IXMLDOMElement;
  i: Integer;
  Objects: IBoldMOFObjectList;
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

procedure TBoldXMI10Exporter.ContentsFromRoot(RootObj: IBoldMOFObject;
  XMLElement: IXMLDOMElement);
var
  ContentElement: IXMLDOMElement;
begin
  ContentElement := AppendNewElement(XMLElement, XMI_NODENAME_CONTENT);
  ObjectAsElement(RootObj, ContentElement);
  OtherLinks(RootObj, ContentElement);
  RequiredTypeDefinitions(RootObj, ContentElement);
end;

constructor TBoldXMI10Exporter.Create(OwningLink: TBoldUMLXMILink);
begin
  fOwningLink := OwningLink;
  fProcessedDTD := TBoldUMLDTDReader.Create;
end;

procedure TBoldXMI10Exporter.EnumAttribute(Attribute: IBoldMOFAttribute;
  XMLElement: IXMLDOMElement);
var
  AttrNode: IXMLDOMElement;
begin
  assert(Attribute.IsEnum or Attribute.IsBoolean);
  AttrNode := AppendNewElement(XMLElement, QualifiedAttributeName(Attribute));
  if Attribute.IsBoolean then
    AttrNode.setAttribute(XMI_ATTRIBUTENAME_VALUE, BooleanAsString(Attribute))
  else
    AttrNode.setAttribute(XMI_ATTRIBUTENAME_VALUE, Attribute.AsString);
end;

function TBoldXMI10Exporter.IdOfObject(Obj: IBoldMOFObject): string;
begin
  result := Obj.LocalId;
end;

procedure TBoldXMI10Exporter.ObjectAsElement(Obj: IBoldMOFObject;
  XMLElement: IXMLDOMElement);
var
  ObjectNode: IXMLDOMElement;
  ObjId: string;
begin
  ObjectNode := AppendNewElement(XMLElement, QualifiedClassName(Obj));
  ObjId := IdOfObject(Obj);
  if ObjId <> '' then
    ObjectNode.setAttribute(XMI_ATTRIBUTENAME_ID, ObjId);
  ObjectContents(Obj, ObjectNode);
end;

procedure TBoldXMI10Exporter.ObjectContents(Obj: IBoldMOFObject;
  XMLElement: IXMLDOMElement);
var
  i: Integer;
  Attribute: IBoldMOFAttribute;
  Reference: IBoldMOFReference;
  AttributeList: IBoldMOFAttributeList;
  ReferenceList: IBoldMOFReferenceList;
  FeatureNames: TStrings;

  function FindAttribute(Name: string): IBoldMOFAttribute;
  var
    i: Integer;
    Attr: IBoldMOFAttribute;
  begin
    result := nil;
    for i := 0 to AttributeList.Count-1 do
    begin
      Attr := AttributeList.Attribute(i);
      if Attr.QualifiedName = Name then
      begin
        result := Attr;
        exit;
      end;
    end;
  end;

  function FindReference(Name: string): IBoldMOFReference;
  var
    i: Integer;
    Ref: IBoldMOFReference;
  begin
    result := nil;
    for i := 0 to ReferenceList.Count-1 do
    begin
      Ref := ReferenceList.Reference(i);
      if Ref.QualifiedName = Name then
      begin
        result := Ref;
        exit;
      end;
    end;
  end;

begin
  FeatureNames := TStringList.Create;
  AttributeList := Obj.Attributes;
  ReferenceList := Obj.References;
  try
    FeatureNames.CommaText := fProcessedDTD.MembersOfClass(Obj.QualifiedClassName);
    for i := 0 to FeatureNames.Count-1 do
    begin
      Attribute := FindAttribute(FeatureNames[i]);
      if assigned(Attribute) then
        AttributeAsElement(Attribute, XMLElement)
      else
      begin
        Reference := FindReference(FeatureNames[i]);
        if assigned(Reference) then
        begin
          if Reference.IsComposite then
            CompositeAsElement(Reference, XMLElement)
          else
            ReferenceAsElement(Reference, XMLElement);
        end;
      end;
    end;
  finally
    FeatureNames.Free;
  end;
{
  AttributeList := Obj.Attributes;
  for i := 0 to AttributeList.Count-1 do
  begin
    Attribute := AttributeList.Attribute(i);
    if not Attribute.IsDerived then
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
  }
end;

procedure TBoldXMI10Exporter.ObjRefOrDataValue(Attribute: IBoldMOFAttribute;
  XMLElement: IXMLDOMElement);
begin
  XMLElement.appendChild(XMLElement.ownerDocument.createTextNode(Attribute.AsString));
end;

procedure TBoldXMI10Exporter.OtherLinks(Obj: IBoldMOFObject;
  XMLElement: IXMLDOMElement);
begin
end;

procedure TBoldXMI10Exporter.ProduceHeader(XMLElement: IXMLDOMElement);
var
  HeaderNode: IXMLDOMElement;
  MetamodelNode: IXMLDOMElement;
begin
  HeaderNode := AppendNewElement(XMLElement, XMI_NODENAME_HEADER);
  MetamodelNode := AppendNewElement(HeaderNode, XMI_NODENAME_METAMODEL);
  MetamodelNode.setAttribute(XMI_ATTRIBUTENAME_NAME, '');
end;

function TBoldXMI10Exporter.QualifiedClassName(Obj: IBoldMOFObject): string;
begin
  result := Obj.QualifiedClassName;
end;

function TBoldXMI10Exporter.QualifiedAttributeName(
  Attribute: IBoldMOFAttribute): string;
begin
  result := Attribute.QualifiedName; 
end;

procedure TBoldXMI10Exporter.RawExport(RootObj: IBoldMOFObject);
var
  aDom: TDOMDocument;
  aStringList: TStringList;
begin
  aDom := TDOMDocument.Create(nil);
  aDom.documentElement := aDom.createElement(XMI_NODENAME_ROOT);
  aDom.documentElement.setAttribute(XMI_ATTRIBUTENAME_VERSION, '1.0');

  ProduceHeader(aDom.documentElement);
  ContentsFromRoot(RootObj, aDom.documentElement);

  aStringList := TStringList.Create;
  aStringList.Add('<?xml version = "1.0"?>');
  aStringList.Add('<!DOCTYPE XMI SYSTEM "uml13.dtd">');
  aStringList.Add(aDom.DefaultInterface.xml);
  aStringList.SaveToFile(OwningLink.FileName);

  aDom.Free;
  aStringList.Free;
end;

procedure TBoldXMI10Exporter.ReferenceAsElement(Reference: IBoldMOFReference;
  XMLElement: IXMLDOMElement);
var
  ReferenceName: string;
  ReferenceNode: IXMLDOMElement;
  i: Integer;
  Objects: IBoldMOFObjectList;
begin
  ReferenceName := QualifiedReferenceName(Reference);
  if not Reference.IsMulti then
  begin
    if assigned(Reference.SingleObject) then
    begin
      ReferenceNode := AppendNewElement(XMLElement, ReferenceName);
      ReferencingElement(Reference.SingleObject, ReferenceNode);
    end;
  end else
  begin
    if Reference.Objects.Count > 0 then
    begin
      ReferenceNode := AppendNewElement(XMLElement, ReferenceName);
      Objects := Reference.Objects;
      for i := 0 to Objects.Count-1 do
        ReferencingElement(Objects.GetObject(i), ReferenceNode);
    end;
  end;
end;

procedure TBoldXMI10Exporter.ReferencingElement(Obj: IBoldMOFObject;
  XMLElement: IXMLDOMElement);
var
  ElementNode: IXMLDOMElement;
begin
  ElementNode := AppendNewElement(XMLElement, QualifiedClassName(Obj));
  ElementNode.setAttribute(XMI_ATTRIBUTENAME_IDREF, IdOfObject(Obj));
end;

procedure TBoldXMI10Exporter.RequiredTypeDefinitions(RootObj: IBoldMOFObject;
  XMLElement: IXMLDOMElement);
begin
end;

procedure TBoldXMI10Exporter.SvAttributeContents(
  Attribute: IBoldMOFAttribute; XMLElement: IXMLDOMElement);
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
    ObjRefOrDataValue(Attribute, ValueElement);
  end;
end;

function TBoldXMI10Exporter.QualifiedReferenceName(
  Reference: IBoldMOFReference): string;
begin
  result := Reference.QualifiedName; 
end;

procedure TBoldXMI10Exporter.EmbeddedObject(Attribute: IBoldMOFAttribute;
  XMLElement: IXMLDOMElement);
var
  Obj: IBoldMOFObject;
  ObjectNode: IXMLDOMElement;
begin
  assert(Attribute.IsObject);
  Obj := Attribute.AsObject;
  ObjectNode := AppendNewElement(XMLElement, QualifiedClassName(Obj));
  ObjectContents(Obj, ObjectNode);
end;


destructor TBoldXMI10Exporter.Destroy;
begin
  FreeAndNil(fProcessedDTD);
  inherited;
end;

end.
