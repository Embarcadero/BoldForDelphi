unit BoldTypeNameSelector;

interface

uses
  Classes,
  Graphics,
  Controls,
  Forms,
  ExtCtrls,
  ComCtrls,
  StdCtrls,
  BoldSystemRT,
  BoldElements,
  ImgList, Menus;

type
  TNodeType = (ntClass, ntClasses, ntAttributes, ntList, ntAttribute, ntClassList,
  ntAttributeList, ntAttributeLists, ntClassLists, ntRoot, ntSystem, ntType);

  TfrmBoldTypeNameSelector = class(TForm)
    tvMetaTypes: TTreeView;
    pnButtons: TPanel;
    btCancel: TButton;
    btOK: TButton;
    ilImages: TImageList;
    PopupMenu1: TPopupMenu;
    mnSort: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure tvMetaTypesChange(Sender: TObject; Node: TTreeNode);
    procedure tvMetaTypesDblClick(Sender: TObject);
    procedure mnSortClick(Sender: TObject);
  private
    fSystemTypeInfo: TBoldSystemTypeInfo;
    procedure GenerateNodes(SystemTypeInfo: TBoldSystemTypeInfo; ApprovedTypes: TBoldValueTypes; CurrentStringValue: String);
    procedure SetImageIndex(var Node: TTreeNode; NodeType: TNodeType);
    procedure SelectCurrentNode(CurrentStringValue: String; aNode: TTreeNode);
  public
    function Select(var StringValue: String; SystemTypeInfo: TBoldSystemTypeInfo; ApprovedTypes: TBoldValueTypes): TModalResult;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{$R *.dfm}
{.$R BoldTypeNameSelector.res}

{ TfrmBoldTypeNameSelector }

function TfrmBoldTypeNameSelector.Select(var StringValue: String; SystemTypeInfo: TBoldSystemTypeInfo; ApprovedTypes: TBoldValueTypes): TModalResult;
begin
  Result := mrCancel;
  fSystemTypeInfo := SystemTypeInfo;
  GenerateNodes(SystemTypeInfo, ApprovedTypes, StringValue);
  if mrOK = ShowModal then
  begin
    StringValue := tvMetaTypes.Selected.Text;
    Result := ModalResult;
  end;
end;

procedure TfrmBoldTypeNameSelector.FormCreate(Sender: TObject);
begin
  ilImages.GetInstRes(HInstance, rtBitmap, 'SMALLUMLMODELROOTBITMAP', 16, [], clWhite); // do not localize
  ilImages.GetInstRes(HInstance, rtBitmap, 'SMALLUMLCLASSBITMAP', 16, [], clWhite); // do not localize
  ilImages.GetInstRes(HInstance, rtBitmap, 'SMALLUMLCLASSESBITMAP', 16, [], clWhite); // do not localize
  ilImages.GetInstRes(HInstance, rtBitmap, 'SMALLUMLATTRIBUTEBITMAP', 16, [], clWhite); // do not localize
  ilImages.GetInstRes(HInstance, rtBitmap, 'SMALLUMLATTRIBUTESBITMAP', 16, [], clWhite); // do not localize
  ilImages.GetInstRes(HInstance, rtBitmap, 'SMALLLISTBITMAP', 16, [], clWhite); // do not localize
  ilImages.GetInstRes(HInstance, rtBitmap, 'SMALLUMLCLASSLISTBITMAP', 16, [], clWhite); // do not localize
  ilImages.GetInstRes(HInstance, rtBitmap, 'SMALLUMLATTRIBUTELISTBITMAP', 16, [], clWhite); // do not localize
  ilImages.GetInstRes(HInstance, rtBitmap, 'SMALLSYSTEMBITMAP', 16, [], clWhite); // do not localize
  ilImages.GetInstRes(HInstance, rtBitmap, 'SMALLTYPEBITMAP', 16, [], clWhite); // do not localize
end;

procedure TfrmBoldTypeNameSelector.GenerateNodes(
  SystemTypeInfo: TBoldSystemTypeInfo; ApprovedTypes: TBoldValueTypes; CurrentStringValue: String);
var Index: Integer;
    Node, Node2, ListRoot: TTreeNode;
begin
  with tvMetaTypes do
  begin
    // add the root node
    Node := Items.AddObject(nil, 'Types in the system', nil); // do not localize
    SetImageIndex(Node, ntRoot);

    if bvtClass in ApprovedTypes then
    begin
      if SystemTypeInfo.TopSortedClasses.Count > 0 then
      begin
        // add the classes root node
        Node2 := Items.AddChildObject(Node, 'Classes', nil); // do not localize
        SetImageIndex(Node2, ntClasses);
        // add the classes
        for Index := 0 to SystemTypeInfo.TopSortedClasses.Count - 1 do
        begin
          with SystemTypeInfo.TopSortedClasses[Index] do
          begin
            Node := Items.AddChildObject(Node2, ExpressionName, Pointer(SystemTypeInfo.TopSortedClasses[Index]));
            SetImageIndex(Node, ntClass);
            SelectCurrentNode(CurrentStringValue, Node);
          end;
        end;
        Node2.AlphaSort;
      end;
    end;

    if bvtList in ApprovedTypes then
    begin
      // add the list root node
      Node2 := Items.GetFirstNode;
      ListRoot := Items.AddChildObject(Node2, 'Lists', nil); // do not localize
      SetImageIndex(ListRoot, ntList);
      // add the class list root node
      Node2 := Items.AddChildObject(ListRoot, 'Class lists', nil); // do not localize
      SetImageIndex(Node2, ntClassLists);

      // add the class list nodes
      for Index := 0 to SystemTypeInfo.TopSortedClasses.Count - 1 do
      begin
        Node := Items.AddChildObject(Node2, SystemTypeInfo.ListTypeInfoByElement[SystemTypeInfo.TopSortedClasses[Index]].ExpressionName, Pointer(SystemTypeInfo.ListTypeInfoByElement[SystemTypeInfo.TopSortedClasses[Index]]));
        SetImageIndex(Node, ntClassList);
        SelectCurrentNode(CurrentStringValue, Node);
      end;
      Node2.AlphaSort;

      // add the attribute list root node
      Node2 := Items.AddChildObject(ListRoot, 'Attribute lists', nil); // do not localize
      SetImageIndex(Node2, ntAttributeLists);

      // add the attribute list nodes
      for Index := 0 to SystemTypeInfo.AttributeTypes.Count - 1 do
      begin
        Node := Items.AddChildObject(Node2, SystemTypeInfo.ListTypeInfoByElement[SystemTypeInfo.AttributeTypes[Index]].ExpressionName, Pointer(SystemTypeInfo.ListTypeInfoByElement[SystemTypeInfo.AttributeTypes[Index]]));
        SetImageIndex(Node, ntAttributeList);
        SelectCurrentNode(CurrentStringValue, Node);
      end;
      Node2.AlphaSort;
    end;

    if bvtAttr in ApprovedTypes then
    begin
      // add the attributes node
      Node := Items.GetFirstNode;
      Node2 := Items.AddChildObject(Node, 'Attributes', nil); // do not localize
      SetImageIndex(Node2, ntAttributes);
      for Index := 0 to SystemTypeInfo.AttributeTypes.Count - 1 do
      begin
        // add the attribute nodes
        Node := Items.AddChildObject(Node2, SystemTypeInfo.AttributeTypes[Index].ExpressionName, Pointer(SystemTypeInfo.AttributeTypes[Index]));
        SetImageIndex(Node, ntAttribute);
        SelectCurrentNode(CurrentStringValue, Node);
      end;
      Node2.AlphaSort;
    end;

    if bvtSystem in ApprovedTypes then
    begin
      Node := Items.GetFirstNode;
      Node2 := Items.AddChildObject(Node, SystemTypeInfo.ExpressionName , Pointer(SystemTypeInfo));
      SetImageIndex(Node2, ntSystem);
      SelectCurrentNode(CurrentStringValue, Node2);
    end;

    if bvtType in ApprovedTypes then
    begin
      Node := Items.GetFirstNode;
      Node2 := Items.AddChildObject(Node, SystemTypeInfo.TypeTypeInfo.ExpressionName, Pointer(SystemTypeInfo.TypeTypeInfo));
      SetImageIndex(Node2, ntType);
      SelectCurrentNode(CurrentStringValue, Node2);
    end;
    Items.GetFirstNode.Expand(False);
  end;
end;

procedure TfrmBoldTypeNameSelector.SetImageIndex(var Node: TTreeNode; NodeType: TNodeType);
begin
  case NodeType of
    ntRoot:
      begin
        Node.ImageIndex := 0;
        Node.SelectedIndex := 0;
      end;
    ntClass:
      begin
        Node.ImageIndex := 1;
        Node.SelectedIndex := 1;
      end;
    ntClasses:
      begin
        Node.ImageIndex := 2;
        Node.SelectedIndex := 2;
      end;
    ntAttribute:
      begin
        Node.ImageIndex := 3;
        Node.SelectedIndex := 3;
      end;
    ntAttributes:
      begin
        Node.ImageIndex := 4;
        Node.SelectedIndex := 4;
      end;
    ntList:
      begin
        Node.ImageIndex := 5;
        Node.SelectedIndex := 5;
      end;
    ntClassList:
      begin
        Node.ImageIndex := 6;
        Node.SelectedIndex := 6;
      end;
    ntAttributeList:
      begin
        Node.ImageIndex := 7;
        Node.SelectedIndex := 7;
      end;
    ntAttributeLists:
      begin
        Node.ImageIndex := 7;
        Node.SelectedIndex := 7;
      end;
    ntClassLists:
      begin
        Node.ImageIndex := 6;
        Node.SelectedIndex := 6;
      end;
    ntSystem:
      begin
        Node.ImageIndex := 8;
        Node.SelectedIndex := 8;
      end;
    ntType:
      begin
        Node.ImageIndex := 9;
        Node.SelectedIndex := 9;
      end;
  end;
end;

procedure TfrmBoldTypeNameSelector.btOKClick(Sender: TObject);
begin
  if btOK.Enabled then
    ModalResult := mrOK;
end;

procedure TfrmBoldTypeNameSelector.btCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmBoldTypeNameSelector.tvMetaTypesChange(Sender: TObject; Node: TTreeNode);
begin
  btOK.Enabled := Assigned(Node.Data);
end;

procedure TfrmBoldTypeNameSelector.tvMetaTypesDblClick(Sender: TObject);
begin
  btOKClick(nil);
end;

procedure TfrmBoldTypeNameSelector.SelectCurrentNode(CurrentStringValue: String; aNode: TTreeNode);
begin
  if SameText(aNode.Text, CurrentStringValue) then
    aNode.Selected := True;
end;

procedure TfrmBoldTypeNameSelector.mnSortClick(Sender: TObject);
begin
  if Assigned(tvMetaTypes.Selected) then
    tvMetaTypes.Selected.AlphaSort;
end;

end.
