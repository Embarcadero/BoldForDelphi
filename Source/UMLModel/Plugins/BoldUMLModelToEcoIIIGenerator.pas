unit BoldUMLModelToEcoIIIGenerator;

interface

uses
  Graphics,
  Classes,
  Dialogs,
  BoldSystem,
  BoldGuidUtils,
  MSXML_TLB,
  BoldUMLModel,
  BoldUMLTypes,
  BoldUMLPlugins,
  BoldUMLModelEditPlugIn,
  BoldUMLModelConverter,
  BoldLogHandler;

type
  { forward declaration of classes }
  TUMLModelToEcoIIIGenerator = class;

  { TUMLModelToEcoIIIGenerator }
  TUMLModelToEcoIIIGenerator = class(TUMLPlugInFunction)
  private
    procedure ConvertClass(c: TUmlClass; parentNode: IXMLDOMElement; doc: TDOMDocument);
    procedure ConvertAssociation(a: TUMLAssociation; parentNode :IXMLDOMElement; doc: TDOMDocument);
    procedure ConvertAssociationEnd(ae: TUMLAssociationEnd; parentNode :IXMLDOMElement; doc: TDOMDocument);
    procedure ConvertAttribute(attr: TUMLAttribute; parentNode :IXMLDOMElement; doc: TDOMDocument);
    procedure ConvertOperation(op: TUMLOperation; parentNode :IXMLDOMElement; doc: TDOMDocument);
    procedure ConvertPackage(Package: TUmlPackage; path: string);
    function GetId(me: TUMLModelElement): string;
    function AddNode(parent: IXMLDOMElement; Name: string; doc: TDOMDocument): IXMLDOMElement;
    procedure TransferModelElementSettings(me: TUmlModelElement;
      node: IXMLDOMElement; doc: TDOMDocument);
  protected
    function GetMenuItemName: String; override;
    function GetPlugInType: TPlugInType; override;
    function GetImageResourceName: String; override;
    function GetImageMaskColor: TColor; override;
  public
    procedure Execute(Context: IUMLModelPlugInContext); override;  end;

implementation

uses
  Forms,
  Windows,
  ShellAPI,
  ShlObj,
  SysUtils,
  BoldUtils;

var
  _UMLEcoIIIGenerator: TUMLModelToEcoIIIGenerator = nil;

{ TUMLModelToEcoIIIGenerator }

{$REGION 'Helper functions'}
function BrowseDialogCallBack
  (Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): 
  integer stdcall;
var
  wa, rect : TRect;
  dialogPT : TPoint;
begin
  //center in work area
  if uMsg = BFFM_INITIALIZED then
  begin
    wa := Screen.WorkAreaRect;
    GetWindowRect(Wnd, Rect);
    dialogPT.X := ((wa.Right-wa.Left) div 2) - 
                  ((rect.Right-rect.Left) div 2);
    dialogPT.Y := ((wa.Bottom-wa.Top) div 2) - 
                  ((rect.Bottom-rect.Top) div 2);
    MoveWindow(Wnd,
               dialogPT.X,
               dialogPT.Y,
               Rect.Right - Rect.Left,
               Rect.Bottom - Rect.Top,
               True);
  end;

  Result := 0;
end; (*BrowseDialogCallBack*)


function BrowseDialog(const Title: string; const Flag: integer): string;
var
  lpItemID : PItemIDList;
  BrowseInfo : TBrowseInfo;
  DisplayName : array[0..255] of char;
  TempPath : array[0..255] of char;
begin
  Result:='';
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  with BrowseInfo do begin
    hwndOwner := Application.Handle;
    pszDisplayName := @DisplayName;
    lpszTitle := PChar(Title);
    ulFlags := Flag;
    lpfn := BrowseDialogCallBack;
  end;
  lpItemID := SHBrowseForFolder(BrowseInfo);
  if lpItemId <> nil then begin
    SHGetPathFromIDList(lpItemID, TempPath);
    Result := TempPath;
    GlobalFreePtr(lpItemID);
  end;
end;



function TUMLModelToEcoIIIGenerator.GetId(me: TUMLModelElement): string;
var
  comment: TUMLComment;
begin
  if (me.Comment.Count = 0) then
  begin
    comment := TUMLComment.Create(me.BoldSystem);
    comment.Name := BoldCreateGUIDAsString(true);
    me.comment.Add(comment);
    Result := comment.Name;
  end else
    Result := me.comment[0].name;
end;

function TUMLModelToEcoIIIGenerator.AddNode(parent: IXMLDOMElement;
  Name: string; doc: TDOMDocument): IXMLDOMElement;
begin
  Result := doc.CreateElement(Name);
  parent.AppendChild(Result);
end;
{$ENDREGION}

procedure TUMLModelToEcoIIIGenerator.TransferModelElementSettings(me: TUmlModelElement;
  node: IXMLDOMElement; doc: TDOMDocument);
var
  i: integer;
  tv: TUMLTaggedValue;
  tvNode: IXMLDOMElement;
  tvlistNode: IXMLDOMElement;
begin
  node.setAttribute('Name', me.Name);
  node.setAttribute('id', getId(me));
  node.setAttribute('Stereotype', me.stereotypeName);
  if me.M_taggedValue.Count > 0 then
  begin
    tvListNode := AddNode(node, 'TaggedValue', doc);
    for i := 0 to me.M_taggedValue.count-1 do
    begin
      tv := me.M_taggedValue[i];
      tvNode := AddNode(tvListNode, 'TaggedValue', doc);
      tvNode.setAttribute('Tag', tv.tag);
      tvNode.setAttribute('Value', tv.value);
    end;
  end;
end;

procedure TUMLModelToEcoIIIGenerator.ConvertOperation(op: TUMLOperation;
  parentNode: IXMLDOMElement; doc: TDOMDocument);
var
  OpNode,
  ParameterListNode,
  ReturnParameterNode: IXMLDOMElement;
  i: integer;
  Parameter: TUMLParameter;
  procedure ConvertParameter(param: TUMLParameter; parentNode: IXMLDOMElement);
  var
    ParameterNode: IXMLDOMElement;
  begin
    ParameterNode := AddNode(parentNode, 'Parameter', doc);
    ParameterNode.setAttribute('Name', param.Name);
    ParameterNode.setAttribute('Kind', param.M_kind.AsString);
  end;

begin
  OpNode := AddNode(parentNode, 'Method', doc);
  TransferModelElementSettings(op, OpNode, doc);

  ParameterListNode := doc.createElement('Parameter');
  ReturnParameterNode := doc.createElement('ReturnParameter');
  for i := 0 to op.parameter.Count-1 do
  begin
    Parameter := op.parameter[i];
    if parameter.kind = pdReturn then
      ConvertParameter(parameter, ReturnParameterNode)
    else
      ConvertParameter(parameter, ParameterListNode)
  end;
  if ParameterListNode.childNodes.length > 0 then
    opNode.appendChild(ParameterListNode);
  if ReturnParameterNode.childNodes.length > 0 then
    opNode.appendChild(ReturnParameterNode);
end;

procedure TUMLModelToEcoIIIGenerator.ConvertAssociationEnd(
  ae: TUMLAssociationEnd; parentNode: IXMLDOMElement; doc: TDOMDocument);
var
  participantNode, ClassNode, AssocEndNode: IXMLDOMElement;
begin
  AssocEndNode := AddNode(parentNode, 'AssociationEnd', doc);
  TransferModelElementSettings(ae, AssocEndNode, doc);

  if ae.isNavigable then
    AssocEndNode.setAttribute('IsNavigable', 'True');
  AssocEndNode.setAttribute('Ordering', ae.M_ordering.AsString);
  AssocEndNode.setAttribute('Multiplicity', ae.multiplicity);
  AssocEndNode.setAttribute('Visibility', ae.M_visibility.AsString);
  ParticipantNode := AddNode(assocEndNode, 'Participant', doc);
  ClassNode := AddNode(participantNode, 'Class', doc);
  ClassNode.setAttribute('idref', GetId(ae.type_));
end;

procedure TUMLModelToEcoIIIGenerator.ConvertAttribute(attr: TUMLAttribute;
  parentNode: IXMLDOMElement; doc: TDOMDocument);
var
  AttrNode: IXMLDOMElement;
  TypeNode: IXMLDOMElement;
  DataTypeNode: IXMLDOMElement;
begin
  AttrNode := AddNode(parentNode, 'Attribute', doc);
  TransferModelElementSettings(attr, attrNode, doc);

  attrNode.setAttribute('InitialValue', attr.initialValue);
  attrNode.setAttribute('Visibility', attr.M_visibility.AsString);
  typeNode := AddNode(attrNode, 'Type', doc);
  DataTypeNode := AddNode(TypeNode, 'DataType', doc);
  DataTypeNode.setAttribute('idref', attr.typeName);
end;

procedure TUMLModelToEcoIIIGenerator.ConvertAssociation(a: TUMLAssociation;
  parentNode: IXMLDOMElement; doc: TDOMDocument);
var
  AssocNode: IXMLDOMElement;
  AssocEndListNode: IXMLDOMElement;
  i: integer;
begin
  assocNode := AddNode(parentNode, 'Association', doc);
  TransferModelElementSettings(a, assocNode, doc);

  AssocEndListNode := AddNode(assocNode, 'AssociationEnd', doc);
  for i := 0 to a.connection.Count-1 do
    ConvertAssociationEnd(a.connection[i], AssocEndListNode, doc);
end;

procedure TUMLModelToEcoIIIGenerator.ConvertClass(c: TUmlClass;
  parentNode: IXMLDOMElement; doc: TDOMDocument);
var
  ClassNode,
  SuperClassNode,
  SuperClassRefNode,
  FeatureNode: IXMLDOMElement;
  i: integer;
begin
  ClassNode := AddNode(parentNode, 'Class', doc);
  TransferModelElementSettings(c, ClassNode, doc);

  if c.isAbstract then
    ClassNode.setAttribute('IsAbstract', 'True');
  FeatureNode := AddNode(parentNode, 'Feature', doc);
  for i := 0 to c.allFeature.Count-1 do
  begin
    if c.allFeature[i] is TUMLAttribute then
      ConvertAttribute(c.allFeature[i] as TUMLAttribute, FeatureNode, doc);
    if c.allFeature[i] is TUMLOperation then
      ConvertOperation(c.allFeature[i] as TUMLOperation, FeatureNode, doc);
  end;
  if assigned(c.superclass) then
  begin
    SuperClassNode := AddNode(ClassNode, 'SuperClass', doc);
    SuperClassRefNode := AddNode(SuperClassNode, 'Class', doc);
    SuperClassRefNode.setAttribute('idref', GetId(c.superclass));
  end;
end;

procedure TUMLModelToEcoIIIGenerator.ConvertPackage(Package: TUmlPackage; path: string);
var
  Doc: TDOMDocument;
  i: integer;
  OwnedElementNode, PackageNode: IXMLDOMElement;
  aStringList: TStringList;
begin
  Doc := TDOMDocument.Create(nil);
  PackageNode := doc.createElement('Package');
  TransferModelElementSettings(package, packageNode, doc);

  OwnedElementNode := AddNode(PackageNode, 'OwnedElement', Doc);
  doc.documentElement := PackageNode;
  for i := 0 to package.classes.Count-1 do
    ConvertClass(package.classes[i], OwnedElementNode, Doc);

  for i := 0 to package.associations.Count-1 do
    ConvertAssociation(package.associations[i], OwnedElementNode, Doc);

  aStringList := TStringList.Create;
  aStringList.Text := Doc.documentElement.xml;
  aStringList.SaveToFile(Path+'\'+Package.name+'.ecopkg');
end;

// Main method to invoke plug in functionality

procedure TUMLModelToEcoIIIGenerator.Execute(context: IUMLModelPlugInContext);
var
  UMLModel: TUMLModel;
  i: integer;
  sFolder : string;
begin
  UMLModel := Context.GetCurrentModelhandle.EnsuredUMLModel;

  sFolder := BrowseDialog('Select a folder',
                          BIF_RETURNONLYFSDIRS);

  if sFolder <> '' then
  begin
    if Assigned(UMLModel) then
    begin
      for i := 0 to UMLModel.ownedElement.Count - 1 do
      begin
        if (UMLModel.ownedElement[i] is TUMLPackage) then
          ConvertPackage(UMLModel.ownedElement[i] as TUMLPackage, sFolder);
      end;
    end;
  end;
end;

{$REGION 'Plugin support'}
// Mask color for bitmap
function TUMLModelToEcoIIIGenerator.GetImageMaskColor: TColor;
begin
  Result := clTeal;
end;

// Resource for menu and button icon
function TUMLModelToEcoIIIGenerator.GetImageResourceName: String;
begin
  result := 'EcoIII package file generator';
end;

// Caption for menu and hint for button
function TUMLModelToEcoIIIGenerator.GetMenuItemName: String;
begin
  Result := 'EcoIII package file generator';
end;

// Type of tool
function TUMLModelToEcoIIIGenerator.GetPlugInType: TPlugInType;
begin
  Result := ptTool;
end;
{$ENDREGION Plugin support}

initialization
  _UMLEcoIIIGenerator := TUMLModelToEcoIIIGenerator.Create(true);

finalization
  FreeAndNil(_UMLEcoIIIGenerator);

end.
