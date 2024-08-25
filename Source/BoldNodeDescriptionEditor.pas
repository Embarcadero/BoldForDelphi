
{ Global compiler directives }
{$include bold.inc}
unit BoldNodeDescriptionEditor;

interface

uses
  Windows,
  Messages,
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  ComCtrls,
  StdCtrls,
  DesignEditors,
  DesignIntf,
  BoldNodeControlPack,
  Menus,
  BoldSubscription, ImgList, System.ImageList;

type


  TFormNodeEditor = class(TForm)
    TreeView: TTreeView;
    pnlButtons: TPanel;
    btnAddNodeDescription: TButton;
    btnAddListFragment: TButton;
    btnDelete: TButton;
    btnMoveUp: TButton;
    btnMoveDown: TButton;
    cmdSort: TButton;
    Button7: TButton;
    PopupMenu: TPopupMenu;
    popShowButtons: TMenuItem;
    N1: TMenuItem;
    popAddNodeDescription: TMenuItem;
    popAddListFragment: TMenuItem;
    popDelete: TMenuItem;
    popMoveUp: TMenuItem;
    popMoveDown: TMenuItem;
    ImageListTree: TImageList;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PopupMenuPopup(Sender: TObject);
    procedure popShowButtonsClick(Sender: TObject);
    procedure btnAddNodeDescriptionClick(Sender: TObject);
    procedure btnAddListFragmentClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
    procedure cmdSortClick(Sender: TObject);
  private
    fComponent: TComponent;
    fTreeProperties: TBoldTreeFollowerController;
    FSubscriber: TBoldPassthroughSubscriber;
    FUpdateCount: Integer;
    FDesigner: IDesigner;
    procedure Init(Component: TComponent; TreeProperties: TBoldTreeFollowerController; Designer: IDesigner);
    procedure UpdateTree;
    procedure UpdateNode(Node: TTreeNode);
    procedure ResetFocus(NodeData: Pointer);
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    class procedure Edit(Component: TComponent; TreeProperties: TBoldTreeFollowerController; Designer: IDesigner);
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldControlPack,
  BoldGenericListControlPack;

{$R *.dfm}

class procedure TFormNodeEditor.Edit(Component: TComponent; TreeProperties: TBoldTreeFollowerController; Designer: IDesigner);
var
  I: Integer;
  Editor: TFormNodeEditor;
begin
  I := 0;
  Editor := nil;
  while (I < Screen.FormCount) and not Assigned(Editor) do
  begin
    if (Screen.Forms[I] is TFormNodeEditor) and (TFormNodeEditor(Screen.Forms[I]).FComponent = Component) then
      Editor := TFormNodeEditor(Screen.Forms[I]);
    Inc(I);
  end;
  if not Assigned(Editor) then
  begin
    Editor := TFormNodeEditor.Create(nil);
    Editor.Init(Component, TreeProperties, Designer);
  end
  else
  begin
    Editor.BringToFront;
    Editor.WindowState := wsNormal;
  end;
end;

procedure TFormNodeEditor.FormCreate(Sender: TObject);
begin
  FSubscriber := TBoldPassthroughSubscriber.Create(Receive);
end;

procedure TFormNodeEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSubscriber);
end;

procedure TFormNodeEditor.Init(Component: TComponent; TreeProperties: TBoldTreeFollowerController; Designer: IDesigner);
{Initalize form}
begin
  Inc(FUpdateCount);
  try
    FComponent := Component;
    fComponent.FreeNotification(Self);
    fTreeProperties := TreeProperties;
    FDesigner := Designer;
    UpdateTree;
    Show;
  finally
    Dec(FUpdateCount);
  end;
end;

procedure TFormNodeEditor.UpdateTree;
var
  CurrentNode: TTreeNode;

  I, J: Integer;
  RootNode: TTreeNode;
  RootListNode: TTreeNode;
  DescriptionNode: TTreeNode;
  PartNode: TTreeNode;

  procedure StepNode;
  begin
    if Assigned(CurrentNode) then
      CurrentNode := CurrentNode.GetNext;
  end;

  function GetNode(ParentNode: TTreeNode; Ptr: Pointer): TTreeNode;
  var
    DeleteNode: TTreeNode;
    NextNode: TTreeNode;

    function GetNextNode: TTreeNode;
    begin
      Result := CurrentNode;
      while Assigned(Result) and (Result.Data <> Ptr) do
        Result := Result.GetNext;
      if Assigned(Result) then
      begin
        while Assigned(CurrentNode) do
        begin
          if (CurrentNode.Data = Ptr) then
          begin
            Result := CurrentNode;
            Exit;
          end
          else
          begin
            DeleteNode := CurrentNode;
            CurrentNode := CurrentNode.GetNextSibling;
            DeleteNode.Delete;
          end;
        end;
      end;
    end;

  begin
    if Assigned(CurrentNode) and (CurrentNode.Data = Ptr) then
    begin
      Result := CurrentNode;
      StepNode;
    end
    else
    begin
      NextNode := GetNextNode;
      if Assigned(NextNode) then
      begin
        Result := NextNode;
        StepNode;
      end
      else
      begin
        if Assigned(CurrentNode) and (CurrentNode.Parent = ParentNode) then
        begin
          Result := TreeView.Items.InsertObject(CurrentNode, '', Ptr);
          CurrentNode := Result;
          StepNode;
        end
        else
        begin
          if Assigned(ParentNode) then
            Result := TreeView.Items.AddChildObject(ParentNode, '', Ptr)
          else
            Result := TreeView.Items.AddObjectFirst(nil, '', Ptr);
          CurrentNode := Result;
          StepNode;
        end;
      end;
    end;
  end;

  procedure PurgeEnd;
  var
    aNode: TTreeNode;
  begin
    while Assigned(CurrentNode) do
    begin
      aNode := CurrentNode;
      CurrentNode := aNode.GetNextSibling;
      aNode.Delete;
    end;
  end;

begin
  CurrentNode := TreeView.items.GetFirstNode;

  Inc(FUpdateCount);
  try
    RootNode := GetNode(nil, fComponent);
    UpdateNode(RootNode);
    RootListNode := GetNode(RootNode, fTreeProperties);
    UpdateNode(RootListNode);
    for I := 0 to fTreeProperties.Parts.Count - 1 do
    begin
      with fTreeProperties.Parts[I] do
      begin
        PartNode := GetNode(RootListNode, fTreeProperties.Parts[I]);
        UpdateNode(PartNode);
      end;
    end;
    for I := 0 to fTreeProperties.NodeDescriptions.Count - 1 do
    begin
      DescriptionNode := GetNode(RootNode, fTreeProperties.NodeDescriptions[I]);
      UpdateNode(DescriptionNode);
      for J := 0 to fTreeProperties.NodeDescriptions[I].ListController.Parts.Count - 1 do
      begin
        with fTreeProperties.NodeDescriptions[I].ListController.Parts[J] do
        begin
          PartNode := GetNode(DescriptionNode, fTreeProperties.NodeDescriptions[I].ListController.Parts[J]);
          UpdateNode(PartNode);
        end;
      end;
    end;
    PurgeEnd;
    RootNode.Expand(False);
  finally
    Dec(FUpdateCount);
  end;
end;

procedure TFormNodeEditor.UpdateNode(Node: TTreeNode);

  procedure SetTextAndImage(S: string; Idx: Integer);
  begin
    if Node.Text <> S then
      Node.Text := S;
    if Node.ImageIndex <> Idx then
    begin
      Node.ImageIndex := Idx;
      Node.SelectedIndex := Idx+1;
    end;
  end;

  var
    ImageIndex: Integer;

begin
  if Assigned(Node.Data) then
  begin
    {Add subscrption}
    if TObject(Node.Data) is TBoldFollowerController then
    begin
      TBoldFollowerController(Node.Data).AddSmallSubscription(FSubscriber, [beDestroying, beValueChanged], breReEvaluate);
    end;
    {Update caption and icon}
    if TObject(Node.Data) is TComponent then
    begin
      SetTextAndImage(Format('%s', [fComponent.Name]), 6);
    end
    else if TObject(Node.Data) is TBoldTreeFollowerController then
    begin
      TBoldTreeFollowerController(Node.Data).AddSmallSubscription(FSubscriber, [beDestroying, beValueChanged], breReEvaluate);
      SetTextAndImage( 'Root', 8);
    end
    else if TObject(Node.Data) is TBoldNodeDescription then
    begin
      with TBoldNodeDescription(Node.Data) do
      begin
        ListController.AddSmallSubscription(FSubscriber, [beDestroying, beValueChanged], breReEvaluate);
        SetTextAndImage(Format('%s'{ Text: ''%s'' Icon: ''%s'''},
                         [Name, TextController.Expression, IconController.Expression]), 0);
      end;
    end
    else if TObject(Node.Data) is TBoldGenericListPart then
    begin
      with TBoldGenericListPart(Node.Data) do
      begin
        if InterpretAsList then
          ImageIndex := 2
        else
          ImageIndex := 4;
        SetTextAndImage(Format('Parts[%d] Element: ''%s'' Controller: ''%s''', [Index, ElementExpression, ControllerExpression]), ImageIndex);
      end;
    end
    else
      raise EBoldInternal.Create('Unkown type in Node.Data');
  end;
end;

procedure TFormNodeEditor.ResetFocus(NodeData: Pointer);
var
  I: Integer;
begin
  I := TreeView.Items.Count - 1;
  while (I>0) and (TreeView.Items[I].Data<>NodeData) do
    Dec(I);
  TreeView.Selected := TreeView.Items[I];
end;

procedure TFormNodeEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FUpdateCount := 1;
  FSubscriber.CancelAllSubscriptions;
  Action := caFree;
end;

procedure TFormNodeEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = fComponent) and (Operation = opRemove) then
  begin
    fComponent := nil;
    fTreeProperties := nil;
    fDesigner := nil;
    if Assigned(fSubscriber) then
      FSubscriber.CancelAllSubscriptions;
    Release;
  end;
end;

procedure TFormNodeEditor.Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if OriginalEvent = beValueChanged then
    UpdateTree;
end;

procedure TFormNodeEditor.TreeViewClick(Sender: TObject);
begin
  if (FUpdateCount = 0) and
     Assigned(TreeView.Selected.Data) and
     Assigned(FDesigner) and
     (TObject(TreeView.Selected.Data) is TPersistent) then
    FDesigner.SelectComponent(TPersistent(TreeView.Selected.Data));
end;

{Popup menu}

procedure TFormNodeEditor.PopupMenuPopup(Sender: TObject);
begin
  popShowButtons.Checked := pnlButtons.Visible
end;

procedure TFormNodeEditor.btnAddNodeDescriptionClick(Sender: TObject);
begin
  ResetFocus(fTreeProperties.NodeDescriptions.Add);
end;

procedure TFormNodeEditor.btnAddListFragmentClick(Sender: TObject);
begin
  if Assigned(TreeView.Selected) then
  begin
    if (TObject(TreeView.Selected.Data) is TBoldTreeFollowerController) then
    begin
      ResetFocus(fTreeProperties.Parts.Add);
    end
    else if (TObject(TreeView.Selected.Data) is TBoldNodeDescription) then
    begin
      ResetFocus(TBoldNodeDescription(TreeView.Selected.Data).ListController.Parts.Add);
    end
    else if (TObject(TreeView.Selected.Data) is TBoldGenericListPart) then
    begin
      ResetFocus((TBoldGenericListPart(TreeView.Selected.Data).Collection as TBoldGenericListParts).Add);
    end;
  end;
end;

procedure TFormNodeEditor.btnDeleteClick(Sender: TObject);
begin
  if Assigned(TreeView.Selected) and Assigned(TreeView.Selected.Data) then
  begin
    if (TObject(TreeView.Selected.Data) is TBoldNodeDescription) then
    begin
      TBoldNodeDescription(TreeView.Selected.Data).Free;
      UpdateTree;
    end
    else if (TObject(TreeView.Selected.Data) is TBoldGenericListPart) then
    begin
      TBoldGenericListPart(TreeView.Selected.Data).Free;
      UpdateTree;
    end;
  end;
end;

procedure TFormNodeEditor.btnMoveUpClick(Sender: TObject);
var
  NodeDescription: TBoldNodeDescription;
  Part: TBoldGenericListPart;
begin
  if Assigned(TreeView.Selected) and Assigned(TreeView.Selected.Data) then
  begin
    if (TObject(TreeView.Selected.Data) is TBoldNodeDescription) then
    begin
      NodeDescription := TBoldNodeDescription(TreeView.Selected.Data);
      if NodeDescription.Index > 0 then
      begin
        NodeDescription.Index := NodeDescription.Index-1;
        UpdateTree;
      end;
    end
    else if (TObject(TreeView.Selected.Data) is TBoldGenericListPart) then
    begin
      Part := TBoldGenericListPart(TreeView.Selected.Data);
      if Part.Index>0 then
      begin
        Part.Index := Part.Index - 1;
        UpdateTree;
      end;
    end;
  end;
end;

procedure TFormNodeEditor.btnMoveDownClick(Sender: TObject);
var
  NodeDescription: TBoldNodeDescription;
  Part: TBoldGenericListPart;
begin
  if Assigned(TreeView.Selected) and Assigned(TreeView.Selected.Data) then
  begin
    if (TObject(TreeView.Selected.Data) is TBoldNodeDescription) then
    begin
      NodeDescription := TBoldNodeDescription(TreeView.Selected.Data);
      if NodeDescription.Index<NodeDescription.Collection.Count - 1 then
      begin
        NodeDescription.Index := NodeDescription.Index+1;
        UpdateTree;
      end;
    end
    else if (TObject(TreeView.Selected.Data) is TBoldGenericListPart) then
    begin
      Part := TBoldGenericListPart(TreeView.Selected.Data);
      if Part.Index<Part.Collection.Count - 1 then
      begin
        Part.Index := Part.Index + 1;
        UpdateTree;
      end;
    end;
  end;
end;

procedure TFormNodeEditor.popShowButtonsClick(Sender: TObject);
begin
  pnlButtons.Visible := not pnlButtons.Visible;
end;

procedure TFormNodeEditor.cmdSortClick(Sender: TObject);
var
  i,
  j: integer;
begin
  for I := 0 to fTreeProperties.NodeDescriptions.Count - 2 do
    for j := i + 1 to fTreeProperties.NodeDescriptions.Count - 1 do
      if fTreeProperties.NodeDescriptions[i].Name > fTreeProperties.NodeDescriptions[j].Name then
        fTreeProperties.NodeDescriptions[j].Index := fTreeProperties.NodeDescriptions[i].Index;
  UpdateTree;
end;

end.
