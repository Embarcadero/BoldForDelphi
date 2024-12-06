
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldUMLXMILinkSupport;

interface

uses
  Variants,
  Bold_MSXML_TLB,
  BoldContainers,
  BoldIndexableList,
  BoldUMLModel;

type
  { forward declaration of classes }

  TBoldUMLElementArray = class;
  TBoldXMIObjectItem = class;
  TBoldXMIIObjectList = class;


  TXMIId = record
    Id: string;
    UUId: string;
  end;

  TBoldUMLElementArray = class(TBoldObjectArray)
  private
    function Get(Index: Integer): TUMLElement;
  public
    property Items[Index: Integer]: TUMLElement read Get; default;
  end;

  TBoldXMIObjectItem = class
  private
    fUMLobjects: TBoldUMLElementArray;
    fXMIId: TXMIId;
  public
    constructor Create(const ID: TXMIId);
    destructor Destroy; override;
    property XMIId: TXMIId read fXMIId;
    property UMLobjects: TBoldUMLElementArray read FUMLobjects;
  end;

  TBoldXMIIObjectList = class(TBoldIndexableList)
  private
    function GetItem(index: Integer): TBoldXMIObjectItem;
    function GetItemById(const XMIId: TXMIId): TBoldXMIObjectItem;
    function GetElementsById(const XMIId: TXMIId): TBoldUMLElementArray;
  public
    constructor Create;
    function Add(const Id: TXMIId; Obj1:Tobject; Obj2: TObject=nil): TBoldUMLElementArray;
    property Items[index: Integer]: TBoldXMIObjectItem read GetItem; default;
    property ItemsById[const XMIId: TXMIId]: TBoldXMIObjectItem read GetItemById;
    property UMLElementsById[const XMIId: TXMIId]: TBoldUMLElementArray read GetElementsById;
   end;

function FormatId(const id: TXMIId): string;
function FirstElementFromNodeList(List: IXMLDOMNodeList): IXMLDOMElement;
function NextElementFromNodeList(List: IXMLDOMNodeList): IXMLDOMElement;
function GetXMIId(UMLElementNode: IXMLDOMElement): TXMIId;
function IsNonBlank(const XMIId: TXMIId): Boolean;
function GetXMIIdRef(UMLElementNode: IXMLDOMElement): TXMIId;
function XMIIdforId(const id: String): TXMIId;

implementation

uses
  BoldDefs,
  BoldUtils,
  SysUtils,
  BoldIndex,
  BoldHashIndexes;

var
  IX_ID: integer = -1;
  IX_UUID: integer = -1;

  type
    {---TBOClassNameIndex---}
  TIdIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  TUUIdIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

function FormatId(const id: TXMIId): string;
begin
  if id.id <> '' then
    Result := Format('id:%s ', [id.id]);
  if id.UUid <> '' then
    Result := Format('UUid:%s ', [id.id]);
end;

function GetXMIId(UMLElementNode: IXMLDOMElement): TXMIId;
begin
  if not VarIsNull(UMLElementNode.getAttribute('xmi.id')) then
    Result.Id := UMLElementNode.getAttribute('xmi.id');
  if not VarIsNull(UMLElementNode.getAttribute('xmi.uuid')) then
    Result.UUId := UMLElementNode.getAttribute('xmi.uuid');
end;

function IsNonBlank(const XMIId: TXMIId): boolean;
begin
  Result := (XMIId.id <> '') or (XMIId.UUId <> '')
end;

function GetXMIIdRef(UMLElementNode: IXMLDOMElement): TXMIId;
begin
  if not VarIsNull(UMLElementNode.getAttribute('xmi.idref')) then
    Result.Id := UMLElementNode.getAttribute('xmi.idref');
  if not VarIsNull(UMLElementNode.getAttribute('xmi.uuidref')) then
    Result.UUId := UMLElementNode.getAttribute('xmi.uuidref');
end;

function XMIIdforId(const id: string): TXMIId;
begin
  Result.Id := id;
end; 
function NextElementFromNodeList(List: IXMLDOMNodeList): IXMLDOMElement;
var
  Node: IXMLDOMNode;
begin
  Node := List.NextNode;
  Result := nil;
  while assigned(Node) and (not Assigned(Result)) do
    if Node.NodeType = NODE_ELEMENT then
      Result := Node as IXMLDOMElement
    else
      Node := List.NextNode;
end; 
function FirstElementFromNodeList(List: IXMLDOMNodeList): IXMLDOMElement;
begin
  List.Reset;
  result := NextElementFromNodeList(List);
end;

{ TBoldXMIObjectItem }

constructor TBoldXMIObjectItem.Create(const Id: TXMIId);
begin
  inherited Create;
  fXMIId := Id;
  fUMLObjects := TBoldUMLElementArray.Create(1,[]);
end;

destructor TBoldXMIObjectItem.Destroy;
begin
  FreeAndNil(fUMLObjects);
  inherited;
end;

{ TBoldXMIIObjectList }

function TBoldXMIIObjectList.Add(const Id: TXMIId; Obj1, Obj2: TObject):TBoldUMLElementArray;
var
  newItem: TBoldXMIObjectItem;
begin
  newItem := TBoldXMIObjectItem.Create(id);
  newItem.UMLobjects.add(obj1);
  if Assigned(obj2) then
    newItem.UMLobjects.add(obj2);
  inherited Add(NewItem);
  result := NewItem.UMLobjects;
end;

constructor TBoldXMIIObjectList.Create;
begin
  inherited;
  OwnsEntries := True;
  SetIndexCapacity(3);
  SetIndexVariable(IX_ID, AddIndex(TIDIndex.Create));
  SetIndexVariable(IX_UUID, AddIndex(TUUIDIndex.Create));
end;

function TBoldXMIIObjectList.GetElementsById(
  const XMIId: TXMIId): TBoldUMLElementArray;
begin
  if assigned(ItemsById[XMIId]) then
    Result := ItemsById[XMIID].UMLobjects
  else
    Result := nil;
end;

function TBoldXMIIObjectList.GetItem(index: Integer): TBoldXMIObjectItem;
begin
  Result := TBoldXMIObjectItem(inherited Items[index]);
end;

function TBoldXMIIObjectList.GetItemById(const XMIId: TXMIId): TBoldXMIObjectItem;
begin
  Result := nil;
  if XMIId.ID <> '' then
    Result := TBoldXMIObjectItem(TIDIndex(Indexes[IX_ID]).FindByString(XMIId.ID));
  if not Assigned(Result) and (XMIId.UUid <> '')  then
    Result := TBoldXMIObjectItem(TUUIDIndex(Indexes[IX_UUID]).FindByString(XMIId.UUID));
end;

{ TBoldUMLElementArray }

function TBoldUMLElementArray.Get(Index: Integer): TUMLElement;
begin
  Result := inherited Items[index] as TUMLElement;
end;

{ TIdIndex }

function TIdIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := (Item as TBoldXMIObjectItem).xmiid.id;
end;

{ TUUIdIndex }

function TUUIdIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := (Item as TBoldXMIObjectItem).xmiid.uuid;
end;

end.
