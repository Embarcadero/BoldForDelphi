
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldXMLProducers;

interface
uses
  {$IFDEF OXML}OXmlPDOM{$ELSE}Bold_MSXML_TLB{$ENDIF},
  BoldStringList,
  BoldManipulators,
  BoldDefs,
  BoldElements,
  BoldSystem,
  BoldSubscription,
  Classes
  ;

type
  TBoldXMLProducerOption = (xpoIncludeIdString, xpoIncludeValue);
  TBoldXMLProducerOptions = set of TBoldXMLProducerOption;

const
  xpoDefault = [xpoIncludeIdString, xpoIncludeValue];
  DEFAULT_XMLELEMENT_BOLDIDNAME = 'BoldID';

type
  TBoldAbstractXMLProducer = class;
  TBoldXMLProducer = class;

  TBoldProduceEvent = procedure (const paramList: TBoldStringList; const DomDoc: IXMLDomDocument) of object;

  TBoldAbstractXMLProducer = class(TBoldSubscribableComponent)
  private
    FBoldManipulator: TBoldManipulator;
    FXMLElementBoldIDName: string;
    procedure SetBoldManipulator(const Value: TBoldManipulator);
  protected
    procedure Produce(const paramList: TBoldStringList; const DomDoc: IXMLDomDocument); virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function getDocument(const paramList: TBoldStringList): IXMLDomDocument;    
    property BoldManipulator: TBoldManipulator read FBoldManipulator write SetBoldManipulator;
    property XMLElementBoldIDName: string read FXMLElementBoldIDName write FXMLElementBoldIDName;
  public
    constructor Create(AOwner: TComponent); override;
    function getDocumentAsString(const paramList: TBoldStringList): string;
    function AddDomElementForBoldElement(const DomParentElement: IXMLDomElement;
      const BoldElement: TBoldElement; const Mapping: string = ''; const Tag: string = '';
      const Options: TBoldXMLProducerOptions = xpoDefault): IXMLDomElement;
    procedure AddDomElementsForBoldObjectAttributes(const DomParentElement: IXMLDomElement;
      const BoldObject: TBoldObject; const BoldMemberNames: array of string;
      const Options: TBoldXMLProducerOptions = xpoDefault);
    function AddDomElement(const DomParentElement: IXMLDomElement; const XMLTag: string; const XMLValue: string = '';
      const XMLAttributes: TBoldStringList = nil): IXMLDomElement;
  end;

  TBoldXMLProducer = class(TBoldAbstractXMLProducer)
  private
    FOnProduce: TBoldProduceEvent;
  public
    procedure Produce(const paramList: TBoldStringList; const DomDoc: IXMLDomDocument); override;
  published
    property BoldManipulator;
    property OnProduce: TBoldProduceEvent read FOnProduce write FOnProduce;
  end;

implementation

{$R *.res}

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils;

{ TBoldXMLProducer }

procedure TBoldXMLProducer.Produce(const paramList: TBoldStringList;
  const DomDoc: IXMLDomDocument);
begin
  if Assigned(FOnProduce) then
    FOnProduce(paramList, DomDoc);
end;

{ TBoldAbstractXMLProducer }

function TBoldAbstractXMLProducer.AddDomElementForBoldElement(
  const DomParentElement: IXMLDomElement;
  const BoldElement: TBoldElement; const Mapping, Tag: string;
  const Options: TBoldXMLProducerOptions): IXMLDomElement;
var
  IdString, Value, ElementTag: string;
  DomDoc: IXMLDomDocument;
begin
  if not Assigned(BoldManipulator) then
    raise EBold.CreateFmt(sManipulatorNotAssigned, [ClassName, 'AddDomElementForBoldElement']); // do not localize
  Value := BoldManipulator.GetValueAndId(BoldElement, IdString, Mapping);
  if (Trim(Tag) = '') then
    ElementTag := BoldManipulator.DefaultTagForElement(BoldElement)
  else
    ElementTag := Tag;
  DomDoc := DomParentElement.Get_ownerDocument;
  Result := DomDoc.createElement(ElementTag);
  if (xpoIncludeIdString in Options) then
    Result.setAttribute(XMLElementBoldIDName, IdString);
  if (xpoIncludeValue in Options) then
    Result.Set_text(Value);
  DomParentElement.appendChild(Result);
end;

constructor TBoldAbstractXMLProducer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXMLElementBoldIDName := DEFAULT_XMLELEMENT_BOLDIDNAME;
end;

procedure TBoldAbstractXMLProducer.AddDomElementsForBoldObjectAttributes(const DomParentElement: IXMLDomElement;
      const BoldObject: TBoldObject; const BoldMemberNames: array of string;
      const Options: TBoldXMLProducerOptions = xpoDefault);
var
  i: Integer;
  MapperName, MemberName: string;
  position: integer;
begin
  if (Length(BoldMemberNames) = 0) then
  begin
    for i:= 0 to BoldObject.BoldMemberCount - 1 do
      if (BoldObject.BoldMembers[i] is TBoldAttribute) then
       AddDomElementForBoldElement(DomParentElement, BoldObject.BoldMembers[i], '', '', Options);
  end
  else
    for i:= 0 to High(BoldMemberNames) do
    begin
      position := pos('.', BoldMemberNames[i]);
      if (position <> 0) then
      begin
        MapperName := Copy(BoldMemberNames[i], 0, position - 1);
        MemberName := Copy(BoldMemberNames[i], position + 1, MaxInt);
      end
      else
      begin
        MapperName := '';
        MemberName := BoldMemberNames[i];
      end;
      AddDomElementForBoldElement(DomParentElement, BoldObject.BoldMemberByExpressionName[MemberName],
        MapperName, MapperName, Options);
    end;
end;

function TBoldAbstractXMLProducer.getDocument(
  const paramList: TBoldStringList): IXMLDomDocument;
var
  doc: IXMLDomDocument;
  ProcessingInstruction: IXMLDOMProcessingInstruction;
begin
  doc := CoDOMDocument.Create;
  doc.async := false;
  ProcessingInstruction := doc.createProcessingInstruction('xml', 'version="1.0"');
  doc.appendChild(ProcessingInstruction);
  try
    Produce(paramList, doc);
    Result := doc;
  finally
  end;
end;

function TBoldAbstractXMLProducer.getDocumentAsString(
  const paramList: TBoldStringList): string;
begin
  Result := getDocument(paramList).xml;
end;

procedure TBoldAbstractXMLProducer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent=FBoldManipulator) and (Operation=opRemove) then
    FBoldManipulator := nil;
end;

procedure TBoldAbstractXMLProducer.SetBoldManipulator(
  const Value: TBoldManipulator);
begin
  if (Value <> FBoldManipulator) then
  begin
    if Assigned(Value) then
      Value.FreeNotification(Self);
    FBoldManipulator := Value;
  end;
end;

function TBoldAbstractXMLProducer.AddDomElement(
  const DomParentElement: IXMLDomElement; const XMLTag: string; const XMLValue: string;
  const XMLAttributes: TBoldStringList): IXMLDomElement;
var
  DomDoc: IXMLDomDocument;
  i: integer;
begin
  Assert(Assigned(DomParentElement), Format('%s.AddDomElement: cannot pass a nil argument "DomParentElement"', [ClassName]));
  DomDoc := DomParentElement.Get_ownerDocument;
  Result := DomDoc.createElement(XMLTag);
  DomParentElement.appendChild(Result);
  Result.Set_text(XMLValue);
  if Assigned(XMLAttributes) then
    for i:= 0 to XMLAttributes.Count - 1 do
      Result.setAttribute(WideString(XMLAttributes.Names[i]), WideString(XMLAttributes.Strings[i]));
end;

end.