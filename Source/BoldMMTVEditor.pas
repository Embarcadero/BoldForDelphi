
{ Global compiler directives }
{$include bold.inc}
unit BoldMMTVEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Menus, MMToolsApi, Grids, ValEdit,
  {$IFDEF OXML}OXmlPDOM{$ELSE}Bold_MSXML_TLB{$ENDIF};

type
  TBoldTaggedValueDisplay = class;
  TBoldTaggedValueTabbedDisplay = class;


  TfrmBoldMMTVEdit = class(TForm)
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    Edit1: TMenuItem;
    About1: TMenuItem;
    About2: TMenuItem;
    tvModelElements: TTreeView;
    Splitter1: TSplitter;
    Panel1: TPanel;
    tcTaggedValues: TTabControl;
    vleTaggedValues: TValueListEditor;
    procedure FormCreate(Sender: TObject);
    procedure tvModelElementsChange(Sender: TObject; Node: TTreeNode);
    procedure tvModelElementsChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure tcTaggedValuesChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure tcTaggedValuesChange(Sender: TObject);
    procedure vleTaggedValuesStringsChange(Sender: TObject);
    procedure vleTaggedValuesEditButtonClick(Sender: TObject);
  private
    { Private declarations }
    fMMToolService: IMMToolServices;
    fCurrentElement: IMMModelPart;
    fTagDefinitions: TDOMDocument;
    fClassTagDefs: IXMLDOMElement;
    fAttrTagDefs: IXMLDOMElement;
    fMethodTagDefs: IXMLDOMElement;
    fAssocTagDefs: IXMLDOMElement;
    fModelTagDefs: IXMLDOMElement;
    fTVDisplay: TBoldTaggedValueDisplay;
    fTVTabDisplay: TBoldTaggedValueTabbedDisplay;
    fEnumDefs: TStrings;
    function CurrentElement: IMMModelPart;
    function FindCurrentTagDefs: IXMLDOMElement;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; MMToolService: IMMToolServices); reintroduce;
  end;

  TBoldTaggedValueDisplay = class
  private
    fValueListEditor: TValueListEditor;
    fPrefix: string;
    fCurrentElement: IMMModelPart;
    fDirty: Boolean;
    fEnumDefs: TStrings;
    procedure AddTag(Def: IXMLDOMElement);
    procedure ShowTaggedValue(tag, value: string);
    function GetTaggedValue(tag: string): string;
    function DisplayedTagName(index: integer): string;
  public
    constructor Create(VLE: TValueListEditor; EnumDefs: TStrings);
    procedure SetupTags(Defs: IXMLDOMElement);
    procedure ShowElement(Element: IMMModelPart);
    procedure CommitChanges;
    property Prefix: string read fPrefix write fPrefix;
  end;

  TBoldTaggedValueTabbedDisplay = class
  private
    fTabControl: TTabControl;
    procedure EnsureTab(Prefix: string);
  public
    constructor Create(TC: TTabControl);
    function CurrentDisplayPrefix: string;
    procedure SetupTabs(Defs: IXMLDOMElement);
  end;

var
  frmBoldMMTVEdit: TfrmBoldMMTVEdit;

implementation

{$R *.dfm}

uses
  MMEngineDefs,
  BoldMMTVDefs,
  BoldMMTVMemo;

function FullPrefix(name: string): string;
begin
  result := Copy(name, 1, LastDelimiter('.', name)-1);
end;

function RemoveFullPrefix(name: string): string;
begin
  result := Copy(name, LastDelimiter('.', name)+1, maxint);
end;

function GetXMLAttributeWithDefault(Element: IXMLDOMElement; attribute: string; default: string): string;
var
  attr: IXMLDOMAttribute;
begin
  attr := Element.getAttributeNode(attribute);
  if assigned(attr) then
    result := attr.value
  else
    result := default;
end;

function HasTag(Element: IMMModelPart; tag: string): Boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to Element.TaggedValueCount-1 do
    if Element.TaggedValueNames[i] = tag then
      result := true;
end;

constructor TfrmBoldMMTVEdit.Create(AOwner: TComponent;
  MMToolService: IMMToolServices);
var
  ErrorMessage: string;
  Enums: IXMLDOMElement;
  nodeList: IXMLDOMNodeList;
  node: IXMLDOMNode;
  enumElement: IXMLDOMElement;

  function FindElement(Name: string): IXMLDOMElement;
  var
    NodeList: IXMLDOMNodeList;
    aNode: IXMLDOMNode;
  begin
    NodeList := fTagDefinitions.getElementsByTagName(Name);
    aNode := NodeList.nextNode;
    if assigned(aNode) then
      result := aNode as IXMLDOMElement
    else
    begin
      ErrorMessage := 'No <' + Name + '> element found.';
      result := nil;
    end;
  end;

begin
  inherited Create(AOwner);
  fEnumDefs := TStringList.Create;
  fMMToolService := MMToolService;

  ErrorMessage := '';
  fTagDefinitions := TDOMDocument.Create(nil);
  fTagDefinitions.DefaultInterface.async := false;
  if not fTagDefinitions.load('BoldMMTagDefs.xml') then
  begin
    ErrorMessage := fTagDefinitions.parseError.reason;
  end
  else
  begin
    fClassTagDefs := FindElement(NODENAME_CLASSTAGS);
    fAttrTagDefs := FindElement(NODENAME_ATTRTAGS);
    fMethodTagDefs := FindElement(NODENAME_METHODTAGS);
    fAssocTagDefs := FindElement(NODENAME_ASSOCTAGS);
    fModelTagDefs := FindElement(NODENAME_MODELTAGS);
    Enums := FindElement(NODENAME_ENUMS);
    nodeList := Enums.getElementsByTagName(NODENAME_ENUMDEFINITION);
    node := nodeList.nextNode;
    while assigned(node) do
    begin
      enumElement := node as IXMLDOMElement;
      fEnumDefs.Values[enumElement.getAttribute(ATTRNAME_ENUMNAME)] :=
        enumElement.getAttribute(ATTRNAME_ENUMVALUES);
      node := nodeList.nextNode;
    end;
  end;

  if ErrorMessage <> '' then
    showmessage('Error reading tag definition file (BoldMMTagDefs.xml): ' +  ErrorMessage);
end;

function TfrmBoldMMTVEdit.CurrentElement: IMMModelPart;
var
  Element: IMMModelPart;
  Node: TTreeNode;
begin
  Node := tvModelElements.Selected;
  Element := fMMToolService.CodeModel.GetClassOnID(Integer(Node.Data));
  if not assigned(Element) then
    Element := fMMToolService.CodeModel.GetMemberOnID(Integer(Node.Data));
  result := Element;
end;

procedure TfrmBoldMMTVEdit.FormCreate(Sender: TObject);
var
  i, j: Integer;
  aClassTreeNode: TTreeNode;
  aMemberTreeNode: TTreeNode;
  aClass: IMMClassBase;
  aMember: IMMMember;
  SelectedClassNode: TTreeNode;
  SelectedMemberNode: TTreeNode;
  ModelNode: TTreeNode;
begin
  SelectedClassNode := nil;
  SelectedMemberNode := nil;

  ModelNode := tvModelElements.Items.AddChildObject(nil, 'Model',
                 Pointer(fMMToolService.CodeModel.ClassTreeRoot.ID));
  for i := 0 to fMMToolService.CodeModel.ClassCount-1 do
  begin
    aClass := fMMToolService.CodeModel.Classes[i];
    if not aClass.IsInterface then
    begin
      aClassTreeNode := tvModelElements.Items.AddChildObject(ModelNode, aClass.Name, Pointer(aClass.ID));

      if assigned(fMMToolService.Navigator.CurClass) and
        aClass.IsClass(fMMToolService.Navigator.CurClass) then
        SelectedClassNode := aClassTreeNode;

      for j := 0 to aClass.MemberCount-1 do
      begin
        aMember := aClass.Members[j];
        if aMember.MemberType in [cpMethod, cpProperty] then
        begin
          aMemberTreeNode := tvModelElements.Items.AddChildObject(aClassTreeNode, aMember.Name, Pointer(aMember.ID));
          if assigned(fMMToolService.Navigator.CurMember) and
            (aMember.ID = fMMToolService.Navigator.CurMember.ID) then
            SelectedMemberNode := aMemberTreeNode;
        end;
      end;
    end;
  end;

  fTVDisplay := TBoldTaggedValueDisplay.Create(vleTaggedValues, fEnumDefs);
  fTVTabDisplay := TBoldTaggedValueTabbedDisplay.Create(tcTaggedValues);

  if assigned(SelectedMemberNode) then
    tvModelElements.Selected := SelectedMemberNode
  else if Assigned(SelectedClassNode) then
    tvModelElements.Selected := SelectedClassNode;
end;

procedure TfrmBoldMMTVEdit.tvModelElementsChange(Sender: TObject;
  Node: TTreeNode);
var
  TagDefs: IXMLDOMElement;
begin
  fCurrentElement := CurrentElement;
  TagDefs := FindCurrentTagDefs;
  fTVTabDisplay.SetupTabs(TagDefs);
  fTVDisplay.Prefix := fTVTabDisplay.CurrentDisplayPrefix;
  fTVDisplay.SetupTags(TagDefs);
  fTVDisplay.ShowElement(fCurrentElement);
end;

procedure TfrmBoldMMTVEdit.tvModelElementsChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
begin
  if assigned(fCurrentElement) then
    fTVDisplay.CommitChanges;
end;

procedure TfrmBoldMMTVEdit.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  fTVDisplay.CommitChanges;
  Action := caFree;
end;

function TfrmBoldMMTVEdit.FindCurrentTagDefs: IXMLDOMElement;
var
  aClass: IMMClass;
  aMember: IMMMember;
  index: integer;
  Element: IMMModelPart;
begin
  if not assigned(tvModelElements.Selected.Parent) then
    result := fModelTagDefs
  else
  begin
    Element := CurrentElement;
    if Element.QueryInterface(IMMClass, aClass) = S_OK then
      result := fClassTagDefs
    else if Element.QueryInterface(IMMMember, aMember) = S_OK then
    begin
      if aMember.MemberType = cpMethod then
        result := fMethodTagDefs
      else if aMember.MemberType = cpProperty then
      begin
        if fMMToolService.CodeModel.FindClass(aMember.DataName, index) then
          result := fAssocTagDefs
        else
          result := fAttrTagDefs;
      end else
        result := nil;
    end else
      result := nil;
  end;
end;

procedure TfrmBoldMMTVEdit.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fTVDisplay);
  FreeAndNil(fTVTabDisplay);
  fClassTagDefs := nil;
  fAttrTagDefs := nil;
  fAssocTagDefs := nil;
  fMethodTagDefs := nil;
  FreeAndNil(fTagDefinitions);
  FreeAndNil(fEnumDefs);
end;

procedure TfrmBoldMMTVEdit.tcTaggedValuesChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  fTVDisplay.CommitChanges;
end;

{ TBoldTaggedValueDisplay }

procedure TBoldTaggedValueDisplay.AddTag(Def: IXMLDOMElement);
var
  tagName: string;
  tagPrefix: string;
  keyName: string;
  defaultValue: string;
  typeName: string;
begin
  tagName := Def.getAttribute(ATTRNAME_TAGNAME);
  tagPrefix := FullPrefix(tagName);
  if tagPrefix = Prefix then
  begin
    defaultValue := GetXMLAttributeWithDefault(Def, ATTRNAME_DEFAULT, '');
    keyName := RemoveFullPrefix(tagName);
    fValueListEditor.InsertRow(keyName, defaultValue, true);
    typeName := GetXMLAttributeWithDefault(Def, ATTRNAME_TYPENAME, 'String');
    if fEnumDefs.IndexOfName(typeName) <> -1 then
    begin
      fValueListEditor.ItemProps[keyName].EditStyle := esPickList;
      fValueListEditor.ItemProps[keyName].PickList.CommaText := fEnumDefs.Values[typeName];
    end
    else if typeName = 'Text' then
      fValueListEditor.ItemProps[keyName].EditStyle := esEllipsis
    else if typeName = 'Integer' then
      fValueListEditor.ItemProps[keyName].EditMask := '#999999999999;x; ';
  end;
end;

procedure TBoldTaggedValueDisplay.CommitChanges;
var
  i: Integer;
  tag: string;
  value: string;
begin
  if fDirty then
  begin
    for i := 0 to fValueListEditor.Strings.Count-1 do
    begin
      tag := DisplayedTagName(i);
      value := GetTaggedValue(tag);
      fCurrentElement.TaggedValues[tag] := GetTaggedValue(tag);
    end;
  end;
  fDirty := false;
end;

constructor TBoldTaggedValueDisplay.Create(VLE: TValueListEditor; EnumDefs: TStrings);
begin
  fValueListEditor := VLE;
  fDirty := false;
  fEnumDefs := EnumDefs;
end;

function TBoldTaggedValueDisplay.DisplayedTagName(index: integer): string;
var
  KeyName: string;
begin
  KeyName := fValueListEditor.Strings.Names[index];
  if Prefix = '' then
    result := KeyName
  else
    result := Prefix + '.' + KeyName;
end;

function TBoldTaggedValueDisplay.GetTaggedValue(tag: string): string;
begin
  assert(FullPrefix(tag) = Prefix);
  result := fValueListEditor.Values[RemoveFullPrefix(tag)];
end;

procedure TBoldTaggedValueDisplay.SetupTags(Defs: IXMLDOMElement);
var
  tagList: IXMLDOMNodeList;
  tagNode: IXMLDOMNode;
  tagElement: IXMLDOMElement;
begin
  assert(not fDirty, 'setup tags');
  fValueListEditor.Strings.Clear;
  if not assigned(Defs) then
    exit;

  tagList := Defs.getElementsByTagName(NODENAME_TAGDEFINITION);
  tagNode := tagList.nextNode;
  while assigned(tagNode) do
  begin
    tagElement := tagNode as IXMLDOMElement;
    AddTag(tagElement);
    tagNode := tagList.nextNode;
  end;
  fDirty := false;
end;

procedure TBoldTaggedValueDisplay.ShowElement(Element: IMMModelPart);
var
  i: Integer;
  tag: string;
begin
  assert(not fDirty, 'show element');
  fCurrentElement := Element;
  for i := 0 to fValueListEditor.Strings.Count-1 do
  begin
    tag := DisplayedTagName(i);
    if HasTag(Element, tag) then
      ShowTaggedValue(tag, Element.TaggedValues[tag]);
  end;
  fDirty := false;
end;

procedure TBoldTaggedValueDisplay.ShowTaggedValue(tag, value: string);
begin
  assert(Prefix = FullPrefix(tag));
  fValueListEditor.Values[RemoveFullPrefix(tag)] := value;
end;

{ TBoldTaggedValueTabbedDisplay }

constructor TBoldTaggedValueTabbedDisplay.Create(TC: TTabControl);
begin
  fTabControl := TC;
end;

function TBoldTaggedValueTabbedDisplay.CurrentDisplayPrefix: string;
begin
  result := fTabControl.Tabs[fTabControl.TabIndex];
  if result = 'Std UML' then
    result := '';
end;

procedure TBoldTaggedValueTabbedDisplay.EnsureTab(Prefix: string);
var
  tabName: string;
begin
  if Prefix = '' then
    tabName := 'Std UML'
  else
    tabName := Prefix;

  if fTabControl.Tabs.IndexOf(tabName) = -1 then
  begin
    fTabControl.Tabs.Add(tabName);
  end;
end;

procedure TBoldTaggedValueTabbedDisplay.SetupTabs(Defs: IXMLDOMElement);
var
  tagList: IXMLDOMNodeList;
  tagNode: IXMLDOMNode;
  tagElement: IXMLDOMElement;
  prefix: string;
begin
  fTabControl.Tabs.Clear;
  if not assigned(Defs) then
    exit;

  tagList := Defs.getElementsByTagName(NODENAME_TAGDEFINITION);
  tagNode := tagList.nextNode;
  while assigned(tagNode) do
  begin
    tagElement := tagNode as IXMLDOMElement;

    prefix := FullPrefix(tagElement.getAttribute(ATTRNAME_TAGNAME));
    EnsureTab(prefix);

    tagNode := tagList.nextNode;
  end;
  if fTabControl.Tabs.Count > 0 then
    fTabControl.TabIndex := 0;
end;

procedure TfrmBoldMMTVEdit.tcTaggedValuesChange(Sender: TObject);
begin
  fTVDisplay.Prefix := fTVTabDisplay.CurrentDisplayPrefix;
  fTVDisplay.SetupTags(FindCurrentTagDefs);
  fTVDisplay.ShowElement(fCurrentElement);
end;

procedure TfrmBoldMMTVEdit.vleTaggedValuesStringsChange(Sender: TObject);
begin
  fTVDisplay.fDirty := True;
end;

procedure TfrmBoldMMTVEdit.vleTaggedValuesEditButtonClick(Sender: TObject);
var
  memoform: TfrmMemoEdit;
  key: string;
  value: string;
begin
  key := vleTaggedValues.Keys[vleTaggedValues.Row];
  value := vleTaggedValues.Values[Key];
  memoform := TfrmMemoEdit.Create(self);
  memoform.memoTheMemo.Lines.Text := value;
  memoform.Caption := key + ' - Bold tagged value editor';
  if memoform.ShowModal = mrOK then
    vleTaggedValues.Values[key] := memoform.memoTheMemo.Text;
  memoform.Free;
end;

end.
