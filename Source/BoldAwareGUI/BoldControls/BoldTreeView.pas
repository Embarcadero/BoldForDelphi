unit BoldTreeView;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Windows,
  Messages,
  Classes,
  Graphics,
  Menus,
  Controls,
  ComCtrls,
  CommCtrl,
  BoldEnvironmentVCL, // Make sure VCL environement loaded, and finalized after
  {$IFNDEF BOLDCOMCLIENT} // uses
  BoldSystem, // IFNDEF BOLDCOMCLIENT
  BoldGui,  // IFNDEF BOLDCOMCLIENT
  {$ENDIF}
  BoldElements,
  BoldHandles,
  BoldControlsDefs,
  BoldControlPackDefs,
  BoldElementHandleFollower,
  BoldControlPack,
  BoldNumericControlPack,
  BoldStringControlPack,
  BoldGenericListControlPack,
  BoldComponentValidator,
  BoldNodeControlPack;

type
  {Forward declarations}
  TBoldCustomTreeView = class;
  TBoldTreeView = class;
  TBoldTreeNode = class;

  {---TBoldTreeNode---}
  TBoldTreeNode = class(TTreeNode)
  private
    FFollower: TBoldFollower;
    FNodeDescription: TBoldNodeDescription;
    function GetNodeDescription: TBoldNodeDescription;
    function GetHideNodeIfNoVisibleChildren: Boolean;
    procedure SetSelected(const Value: Boolean);
    procedure UpdateIcon;
    function GetTreeView: TBoldTreeView;
  public
    destructor Destroy; override;
    function ExistsInParent: Boolean;
    property Follower: TBoldFollower read FFollower write FFollower;
    property NodeDescription: TBoldNodeDescription read GetNodeDescription;
    property HideNodeIfNoVisibleChildren: Boolean read GetHideNodeIfNoVisibleChildren;
    property TreeView: TBoldTreeView read GetTreeView;
  end;

  {---TBoldCustomTreeView---}
  TBoldCustomTreeView = class(TCustomTreeView, IBoldValidateableComponent)
  private
    fHandleFollower: TBoldElementHandleFollower;
    FTreeController: TBoldTreeFollowerController;
    FMaxLevels: Integer;
    FAutoExpandLevels: Integer;
    FSelectInserted: Boolean;
    FSelectedIndexDelta: Integer;
    fSelectedImageIndex: integer;
    fStateImageSelected: integer;
    fStateImageUnselected: integer;
    FNodeExpansion: TBoldNodeExpansionMethod;
    FDragFollower: TBoldFollower;
    FEditFollower: TBoldFollower;
    FUpdateCount: Integer;
    fMultiSelect: Boolean;
    fSelectAnchor: TBoldTreeNode;
    fLastSelectedNode: TBoldTreeNode;
    fPopupNode: TBoldTreeNode;
    fMouseDownShiftState: TShiftState;
    fMouseDownNode: TBoldTreeNode;
    fMouseDownPos: TPoint;
    fSelectedNodePreUpdate: TBoldTreeNode;
    fSelectedElementPreUpdate: TBoldElement;
    fSelectedNodeDescriptionPreUpdate: TBoldNodeDescription;
    fNodesHaveBeenDeleted: Boolean;
    procedure _CustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ClearAllSelections;
    procedure SetBoldHandle(Value: TBoldElementHandle);
    function GetBoldHandle: TBoldElementHandle;
    function GetRootFollower: TBoldFOllower;
    procedure SetTreeController(Value: TBoldTreeFollowerController);
    procedure SetMaxLevels(Value: Integer);
    procedure SetAutoExpandLevels(Value: Integer);
    procedure SetSelectedIndexDelta(Value: Integer);
    function GetCurrentFollower: TBoldFollower;
    function GetCurrentElement: TBoldElement;
    function GetSelected: TBoldTreeNode;
    procedure SetSelected(Value: TBoldTreeNode);
    function GetContextType: TBoldElementTypeInfo;
    procedure SetMultiSelect(NewValue: Boolean);
    procedure UpdateMultiSelect(Node: TBoldTreeNode; Shift: TShiftState; MouseDirection: TBoldMouseDirection);
    {$IFNDEF BOLDCOMCLIENT}
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean;
    {$ENDIF}
    property MultiSelect: Boolean read fMultiSelect write SetMultiSelect default false;
    function GetPopupElement: TBoldElement;
  protected
    procedure AfterMakeUptoDate(Follower: TBoldFollower); virtual;
    procedure BeforeMakeUptoDate(Follower: TBoldFollower); virtual;
    function CanChange(Node: TTreeNode): Boolean; override;
    function CanCollapse(Node: TTreeNode): Boolean; override;
    function CanEdit(Node: TTreeNode): Boolean; override;
    function CanExpand(Node: TTreeNode): Boolean; override;
    procedure Change(Node: TTreeNode); override;
    procedure Collapse(Node: TTreeNode); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure SetSelectionInSubtree(Node: TTreeNode; Selected: Boolean);
    function CreateNode: TTreeNode; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DeleteNode(Follower: TBoldFollower); virtual;
    procedure DisplayText(Follower: TBoldFollower); virtual;
    procedure DisplayIcon(Follower: TBoldFollower); virtual;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    function DoInsertVisibleNode(Follower: TBoldFollower): TBoldTreeNode;
    procedure DoInsertHiddenNode(Follower: TBoldFollower);
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure Edit(const Item: TTVItem); override;
    procedure Expand(Node: TTreeNode); override;
    function GetPopupMenu: TPopupMenu; override;
    procedure InsertNode(Follower: TBoldFollower); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetNodeState(Node: TBoldTreeNode);
    procedure BeginUpdate;
    procedure EndUpdate;
    function TreeFollowerControllerClass: TBoldTreeFollowerControllerClass; virtual;
    {To be published}
    property AutoExpandLevels: Integer read FAutoExpandLevels write SetAutoExpandLevels default -1;
    property BoldHandle: TBoldElementHandle read GetBoldHandle  write SetBoldHandle;
    property BoldProperties: TBoldTreeFollowerController read FTreeController write SetTreeController;
    property MaxLevels: Integer read FMaxLevels write SetMaxLevels default -1;
    property NodeExpansion: TBoldNodeExpansionMethod read FNodeExpansion write FNodeExpansion default neDemand;
    property SelectInserted: Boolean read FSelectInserted write FSelectInserted;
    property SelectedIndexDelta: Integer read FSelectedIndexDelta write SetSelectedIndexDelta;
    property SelectedImageIndex: integer read fSelectedImageIndex write fSelectedImageIndex default -1;
    property StateImageSelected: integer read fStateImageSelected write fStateImageSelected default -1;
    property StateImageUnselected: integer read fStateImageUnselected write fStateImageUnselected default -1;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function GetElementAt(X, Y: Integer): TBoldElement;
    function GetFollowerAt(X, Y: Integer): TBoldFollower;
    procedure FillListWithSelectedObjects(List: TBoldObjectList);
    function FindListPartByNames(const NodeDescName, ListPartName: String): TBoldGenericListPart;
    property CurrentFollower: TBoldFollower read GetCurrentFollower;
    property CurrentElement: TBoldElement read GetCurrentElement;
    property PopupElement: TBoldElement read GetPopupElement;
    property Selected: TBoldTreeNode read GetSelected write SetSelected;
    property RootFollower: TBoldFollower read GetRootFollower;
  end;

  {---TBoldTreeView---}
  TBoldTreeView = class(TBoldCustomTreeView)
  public
    {$IFNDEF T2H}
    property Items;
  published
    {Standard porperties}
    property Align;
    property Anchors;
    property AutoExpandLevels;
    property BoldHandle;
    property BoldProperties;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property Images;
    property Indent;
    property MaxLevels;
    property MultiSelect;
    property NodeExpansion;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property SelectedIndexDelta;
    property SelectedImageIndex;
    property StateImageSelected;
    property StateImageUnselected;
    property SelectInserted;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnContextPopup;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{---TBoldTreeNode---}
function TBoldTreeNode.GetNodeDescription: TBoldNodeDescription;
var
  I: Integer;
begin
  // Check if cache is accurate.
  if Assigned(FNodeDescription) and (FNodeDescription.NodeFollowerController = Follower.Controller) then
  begin
    Result := FNodeDescription;
  end
  else
  begin
    I := 0;
    with TBoldTreeView(TreeView).BoldProperties.NodeDescriptions do
    begin
      while (I<Count) and (Items[I].NodeFollowerController <> Follower.Controller) do
        Inc(I);
      if (Items[I].NodeFollowerController = Follower.Controller) then
        FNodeDescription := Items[I]
      else
        FNodeDescription := nil;
      Result := FNodeDescription;
    end;
  end;
end;

function TBoldTreeNode.GetHideNodeIfNoVisibleChildren: Boolean;
begin
  Result := (Follower.Controller as TBoldNodeFollowerController).HideNodeWithNoChildren;
end;

function TBoldTreeNode.ExistsInParent: Boolean;
begin
  Result := Follower.ExistInOwner;
end;

{---TBoldCustomTreeView---}
(*Constructor and destructor*)
constructor TBoldCustomTreeView.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTreeController := TreeFollowerControllerClass.Create(Self);
  FTreeController.OnAfterInsertItem := InsertNode;
  FTreeController.OnBeforeDeleteItem := DeleteNode;
  FTreeController.OnIconChanged := DisplayIcon;
  FTreeController.OnTextChanged := DisplayText;
  FTreeController.OnGetContextType := GetContextType;
  FTreeController.AfterMakeUptoDate := AfterMakeUptoDate;
  FTreeController.BeforeMakeUptoDate := BeforeMakeUptoDate;


  fHandleFollower := TBoldElementHandleFollower.Create(Owner, FTreeController);
  FMaxLevels := -1;
  FAutoExpandLevels := -1;
  FNodeExpansion := neDemand;
  fSelectedImageIndex := -1;
  fStateImageSelected := -1;
  fStateImageUnselected := -1;
  OnCustomDrawItem := _CustomDrawItem;
end;

destructor TBoldCustomTreeView.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(FTreeController);
  inherited Destroy;
end;

(*Methods for properties*)

procedure TBoldCustomTreeView.SetBoldHandle(Value: TBoldElementHandle);
begin
  fHandleFollower.BoldHandle := value
end;

function TBoldCustomTreeView.GetBoldHandle: TBoldElementHandle;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomTreeView.GetRootFollower: TBoldFollower;
begin
  Result := fHandleFollower.Follower;
end;

procedure TBoldCustomTreeView.SetTreeController(Value: TBoldTreeFollowerController);
begin
  FTreeController.Assign(Value);
end;

procedure TBoldCustomTreeView.SetMaxLevels(Value: Integer);

  procedure DoList(Level: Integer; Follower: TBoldFollower);
  var
    I: Integer;
  begin
    for I := 0 to Follower.SubFollowerCount-1 do
    begin
      //Update this node
      if (Follower.SubFollowers[I].Controller as TBoldNodeFollowerController).HideNodeWithNoChildren   then
        DoInsertHiddenNode(Follower.SubFollowers[I])
      else
        SetNodeState(TBoldTreeNode(Follower.SubFollowers[I].ControlData));
      //Recurse through subfollowers
      if Follower.SubFollowers[I].Active and (Follower.SubFollowers[I].SubFollowerCount>=BoldNodeListIndex) and Follower.SubFollowers[I].SubFollowers[BoldNodeListIndex].Active then
          DoList(Level+1, Follower.SubFollowers[I].SubFollowers[BoldNodeListIndex])
    end;
  end;

begin
  RootFollower.EnsureDisplayable;
  if Value<-1 then
    Value := -1;
  if (Value<>FMaxLevels) then
  begin
    FMaxLevels := Value;
    BeginUpdate;
    try
      DoList(0, RootFollower);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TBoldCustomTreeView.SetAutoExpandLevels(Value: Integer);
var
  I: Integer;
  Node: TBoldTreeNode;
begin
  RootFollower.EnsureDisplayable;
  if Value<-1 then
    Value := -1;
  FAutoExpandLevels := Value;
  if (FAutoExpandLevels <> -1) then
  begin
    I := 0;
    BeginUpdate;
    try
      while (I<Items.Count) do
      begin
        Node := TBoldTreeNode(Items[I]);
        Node.Expanded := Node.Level <= FAutoExpandLevels;
        Inc(I);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TBoldCustomTreeView.SetSelectedIndexDelta(Value: Integer);
var
  I: Integer;
  Node: TBoldTreeNode;
begin
  if Value <> FSelectedIndexDelta then
  begin
    FSelectedIndexDelta := Value;
    I := 0;
    BeginUpdate;
    try
      while (I < Items.Count) do
      begin
        Node := TBoldTreeNode(Items[I]);
        Node.SelectedIndex := Node.ImageIndex + FSelectedIndexDelta;
        Inc(I);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

function TBoldCustomTreeView.GetCurrentFollower: TBoldFollower;
begin
  Result := nil;
  if Assigned(Selected) then
    Result := Selected.Follower;
end;

function TBoldCustomTreeView.GetCurrentElement: TBoldElement;
begin
  Result := nil;
  if Assigned(Selected) and Assigned(Selected.Follower) and Assigned(Selected.Follower.Element) then
    Result := Selected.Follower.Element;
end;

procedure TBoldCustomTreeView.CreateWnd;
begin
  inherited createWnd;
  RootFollower.Active := true;
end;

procedure TBoldCustomTreeView.DestroyWnd;
begin
  RootFollower.Active := false;
  Items.Clear;
  Inherited DestroyWnd;
end;

function TBoldCustomTreeView.GetSelected: TBoldTreeNode;
begin
  Result := TBoldTreeNode(inherited Selected);
end;

procedure TBoldCustomTreeView.SetSelected(Value: TBoldTreeNode);
begin
  inherited Selected := Value;
end;

procedure TBoldCustomTreeView.BeginUpdate;
begin
//  Items.BeginUpdate;
  Inc(FUpdateCount);
end;

procedure TBoldCustomTreeView.EndUpdate;
begin
//  Items.EndUpdate;
  Dec(FUpdateCount);
end;

(*Display values add and remove of nodes*)
procedure TBoldCustomTreeView.DisplayText(Follower: TBoldFollower);
var
  Node: TTreeNode;
begin
  Node := TTreeNode(Follower.OwningFollower.ControlData);
  if Assigned(Node) and (Node is TTreeNode) then
  begin
    Node.Text := (Follower.Controller as TBoldStringFollowerController).GetCurrentAsString(Follower);
  end;
end;

procedure TBoldCustomTreeView.DisplayIcon(Follower: TBoldFollower);
var
  Node: TTreeNode;
  ImageIndex: integer;
  SelectedIndex: integer;
begin
  Node := TTreeNode(Follower.OwningFollower.ControlData);
  if Assigned(Node) and (Node is TTreeNode) then
  begin
    if not assigned(OnGetImageIndex) then
    begin
      ImageIndex := (Follower.Controller as TBoldIntegerFollowerController).GetCurrentAsInteger(Follower);

      if SelectedIndexDelta <> 0 then
        SelectedIndex := ImageIndex + SelectedIndexDelta
      else if SelectedImageIndex <> -1 then
        SelectedIndex := SelectedImageIndex
      else
        SelectedIndex := ImageIndex;

      // Make a multiselected node appear selected even thjough the treeview does not know this
      if Follower.Selected and not node.Selected then
        ImageIndex := SelectedIndex;

      Node.SelectedIndex := SelectedIndex;
      Node.ImageIndex := ImageIndex;
    end;

    if Assigned(StateImages) and (StateImageSelected <> -1) then
    begin
      if Follower.Selected or node.Selected then
        Node.StateIndex := StateImageSelected
      else
        Node.StateIndex := StateImageUnSelected;
    end;
  end;
end;

function TBoldCustomTreeView.CreateNode: TTreeNode;
begin
  Result := TBoldTreeNode.Create(Items);
end;

procedure TBoldCustomTreeView.AfterMakeUptoDate(Follower: TBoldFollower);
begin
  // for some reason, the pointer to this event seems to get lost after a while.
  // the variables below might contain space pointers outside of an update operation
  // if this method is not executed.
  fSelectedNodePreUpdate := nil;
  fSelectedElementPreUpdate := nil;
  fSelectedNodeDescriptionPreUpdate := nil;
end;

procedure TBoldCustomTreeView.BeforeMakeUptoDate(Follower: TBoldFollower);
begin
  fSelectedNodePreUpdate := Selected;
  fSelectedElementPreUpdate := nil;
  fSelectedNodeDescriptionPreUpdate := nil;
  fNodesHaveBeenDeleted := false;
end;

function TBoldCustomTreeView.DoInsertVisibleNode(Follower: TBoldFollower): TBoldTreeNode;
var
  ParentNode: TTreeNode;
  PrevSiblingNode: TTreeNode;
  I: Integer;
begin

  ParentNode := nil;
  PrevSiblingNode := nil;
  I := Follower.Index;
  while not Assigned(PrevSiblingNode) and (I<Follower.OwningFollower.SubFollowerCount) do
  begin
    PrevSiblingNode := (Follower.OwningFollower.SubFollowers[I].ControlData as TBoldTreeNode);
    Inc(I);
  end;

  if Assigned(PrevSiblingNode) then
  begin
    Result := TBoldTreeNode(Items.Insert(PrevSiblingNode, ''))
  end
  else
  begin

    if Assigned(Follower.owningFollower.owningFollower) then
    begin
      if Assigned(Follower.owningFollower.owningFollower.ControlData) then
        ParentNode := (Follower.owningFollower.owningFollower.ControlData as TBoldTreeNode)
      else
        ParentNode := DoInsertVisibleNode(Follower.owningFollower.owningFollower);
    end;

    Result := TBoldTreeNode(Items.AddChild(ParentNode, ''));
  end;

  Result.Follower := Follower;
  Follower.ControlData := Result;

  SetNodeState(Result);
end;

procedure TBoldCustomTreeView.DoInsertHiddenNode(Follower: TBoldFollower);
var
  Level: Integer;

  function GetLevel: Integer;
  var
    aFollower: TBoldFollower;
  begin
    Result := 0;
    aFollower := Follower.OwningFollower;
    while Assigned(aFollower) do
    begin
      if aFollower.Controller is TBoldNodeFollowerController then
        Inc(Result);
      aFollower := aFollower.OwningFollower;
    end;
  end;

begin
  Level := GetLevel;
  if ((MaxLevels = -1) or (Level < MaxLevels - 1)) and not Follower.ExistInOwner then
  begin
    BeginUpdate;
    try
      Follower.Active := True;
      (Follower.Controller as TBoldNodeFollowerController).SetActiveRange(Follower, BoldNodeListIndex, BoldNodeListIndex);
      Follower.EnsureDisplayable;
      //If EnsureDisplayable creates childnodes they will create a node to this follower too.
      if (Level <= AutoExpandLevels) and Assigned(Follower.ControlData) then
        (Follower.ControlData as TBoldTreeNode).Expand(False); //CHECKME behövs detta?
    finally
      EndUpdate;
    end;
  end
  else
  begin
    //Dont show it if we can't show any children!
    Follower.Active := False;
    if Assigned(Follower.ControlData) then
    begin
      TBoldTreeNode(Follower.ControlData).Delete;
      Follower.ControlData := nil;
    end;
  end;
end;

procedure TBoldCustomTreeView.SetNodeState(Node: TBoldTreeNode);
var
  Allow: Boolean;
  Follower: TBoldFollower;
  Controller: TBoldNodeFollowerController;
begin
  Follower := Node.Follower;
  Controller := (Follower.Controller as TBoldNodeFollowerController);
  if Controller.HideNodeWithNoChildren then
  begin
    //Nodes with HideNodeWithNoChildren is always fully shown
    Controller.SetActiveRange(Follower, BoldNodeListIndex, BoldNodeTextIndex);
  end
  else
  begin
    if (((MaxLevels = -1) or (Node.Level < MaxLevels)) and not Node.ExistsInParent) then
    begin
      if (NodeExpansion = neAll) or
         (Node.Level <= AutoExpandLevels) or
         ((NodeExpansion = neVisible) and Node.IsVisible) then
      begin
        BeginUpdate;
        try
          Controller.SetActiveRange(Follower, BoldNodeListIndex, BoldNodeTextIndex);
          Follower.EnsureDisplayable;
          if (Node.Level <= AutoExpandLevels) then
          begin
            Allow := true;
            if assigned(OnExpanding) then
              OnExpanding(self, node, allow);
            if Allow then
              Node.Expand(False);
          end;
        finally
          EndUpdate;
        end;
      end
      else
      begin
        if ((Follower.SubFollowerCount = 0) or
            (not Follower.SubFollowers[BoldNodeListIndex].Active)) then
        begin
          //Only set node in "ExpandOnDemand" mode if it's not expanded already!
          Node.HasChildren := (Controller.Items[BoldNodeListIndex] as TBoldGenericListController).CanHaveSubFollowers;
          Controller.SetActiveRange(Follower, BoldNodeIconIndex, BoldNodeTextIndex);
          Follower.EnsureDisplayable;
        end;
      end;
    end
    else
    begin
      //Don't allow expansion
      Node.DeleteChildren;
      (Follower.Controller as TBoldNodeFollowerController).SetActiveRange(Follower, BoldNodeIconIndex, BoldNodeTextIndex);
      Follower.EnsureDisplayable;
    end;
  end;
end;

procedure TBoldCustomTreeView.InsertNode(Follower: TBoldFollower);
var
  NewNode: TBoldTreeNode;
begin
  if not (Follower.Controller as TBoldNodeFollowerController).HideNodeWithNoChildren then
  begin
    NewNode := DoInsertVisibleNode(Follower);
    if not fNodesHaveBeenDeleted and FSelectInserted and (FUpdateCount < 1) then
      Selected := NewNode;
    if assigned(fSelectedNodePreUpdate) and
      (Follower.Element = fSelectedElementPreUpdate) and
      (tBoldTreeNode(follower.ControlData).NodeDescription = fSelectedNodeDescriptionPreUpdate) then
        Selected := tBoldTreeNode(follower.ControlData);
  end
  else
  begin
    DoInsertHiddenNode(Follower);
  end;
end;

procedure TBoldCustomTreeView.DeleteNode(Follower: TBoldFollower);
var
  ParentNode: TBoldTreeNode;
begin
  if Assigned(Follower.ControlData) then
  begin
    ParentNode := (Follower.ControlData as TBoldTreeNode).Parent as TBoldTreeNode;
    if fSelectedNodePreUpdate = (Follower.ControlData as TBoldTreeNode) then
    begin
      fSelectedElementPreUpdate := Follower.Element;
      fSelectedNodeDescriptionPreUpdate := fLastSelectedNode.NodeDescription;
    end;
    fNodesHaveBeenDeleted := true;

    (Follower.ControlData as TBoldTreeNode).Delete;
    Follower.ControlData := nil;
    if Assigned(ParentNode) and
       ParentNode.HideNodeIfNoVisibleChildren and
       not ParentNode.HasChildren then
    begin
      (ParentNode.Follower.Controller as TBoldNodeFollowerController).SetActiveRange(ParentNode.Follower, BoldNodeListIndex, BoldNodeListIndex);
      DeleteNode(ParentNode.Follower);
    end;
  end;
end;


(*Test editing*)
function TBoldCustomTreeView.CanEdit(Node: TTreeNode): Boolean;
begin
  FEditFollower := nil;
  Result := inherited CanEdit(Node);
  if Assigned(TBoldTreeNode(Node).Follower) then
  begin
    Result := Result and TBoldTreeNode(Node).Follower.SubFollowers[BoldNodeTextIndex].Controller.MayModify(TBoldTreeNode(Node).Follower.SubFollowers[BoldNodeTextIndex]);
    if Result then
      FEditFollower := TBoldTreeNode(Node).Follower.SubFollowers[BoldNodeTextIndex];
  end;
end;

procedure TBoldCustomTreeView.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if Assigned(FEditFollower) and (Key > #32) and
     not (FEditFollower.Controller as TBoldStringFollowerController).ValidateCharacter(Key, FEditFollower) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
end;

procedure TBoldCustomTreeView.Edit(const Item: TTVItem);
var
  S: string;
  Node: TTreeNode;

  function GetNodeFromItem(Item: TTVItem):TTreeNode;
  begin
    with Item do
    if (state and TVIF_PARAM) <> 0 then Result := Pointer(lParam)
    else Result := Items.GetNode(hItem);
  end;

begin
  if Item.pszText <> nil then
  begin
    S := Item.pszText;
    Node := GetNodeFromItem(Item);
    if Assigned(OnEdited) then OnEdited(Self, Node, S);
    if Node <> nil then
    begin
      Node.Text := S;
      if Assigned(FEditFollower) then
      begin
        (FEditFollower.Controller as TBoldStringFollowerController).MayHaveChanged(S, FEditFollower);
        FEditFollower.Apply;
      end;
    end;
  end
  else
    if Assigned(FEditFollower) then
      FEditFollower.DiscardChange;
  FEditFollower := nil;
end;

procedure TBoldCustomTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitTests: THitTests;
begin
  HitTests := GetHitTestInfoAt(x, y);
  // skip the MouseUp if this is a drag...
  if abs(x - fMouseDownPos.x) + abs(y - fMouseDownPos.y) < Mouse.DragThreshold then
  begin
    if (Button = mbLeft) and (htOnItem in HitTests) then
      UpdateMultiSelect(fMouseDownNode, fMouseDownShiftState, DirMouseUp);
  end;
  inherited;
end;

procedure TBoldCustomTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitTests: THitTests;
begin
  HitTests := GetHitTestInfoAt(x, y);
  if (Button = mbLeft) and (htOnItem in HitTests) then
    FDragFollower := GetFollowerAt(X, Y)
  else
    FDragFollower := nil;
  fMouseDownNode := GetNodeAt(x, y) as TBoldTreeNode;
  fMouseDownPos := Point(x, y);
  if (Button = mbRight) and (Shift = [ssRight]) and (htOnItem in HitTests) then
  begin
    ClearAllSelections;
    SetSelected(fMouseDownNode);
    fMouseDownNode.SetSelected(true);
  end;

  if (Button = mbLeft) and (htOnItem in HitTests) then
    UpdateMultiSelect(fMouseDownNode, Shift, DirMouseDown);
  // for some reason, the shiftstate is not preserved until mouse up
  fMouseDownShiftState := Shift;
  inherited;
end;

(*Drag and Drop*)

procedure TBoldCustomTreeView.DoStartDrag(var DragObject: TDragObject);
var
  P: TPoint;
begin
  inherited DoStartDrag(DragObject);
  if not assigned(OnStartDrag) then
  begin
    if MultiSelect then
    begin
      {$IFNDEF BOLDCOMCLIENT}
      FillListWithSelectedObjects(BoldGuiHandler.DraggedObjects);
      {$ENDIF}
    end
    else
    begin
      if not Assigned(FDragFollower) then
      begin
        GetCursorPos(P);
        with ScreenToClient(P) do FDragFollower := GetFollowerAt(X, Y);
      end;
      if Assigned(FDragFollower) then
        FDragFollower.Controller.StartDrag(FDragFollower);
    end;
  end;
end;

procedure TBoldCustomTreeView.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  if not assigned(OnStartDrag) then
  begin
    if MultiSelect then
    begin
      {$IFNDEF BOLDCOMCLIENT}
      BoldGuiHandler.DraggedObjects.Clear;
      {$ENDIF}
    end
    else
    begin
      if Assigned(FDragFollower) then
        FDragFollower.Controller.EndDrag;
      FDragFollower := nil;
    end;
  end;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomTreeView.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Follower: TBoldFollower;
begin
  Follower := GetFollowerAt(X, Y);
  if (not Assigned(Follower) or (Follower.Controller.DropMode = bdpNone)) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := Follower.Controller.DragOver(Follower, Follower.Element, 0);
end;

procedure TBoldCustomTreeView.DragDrop(Source: TObject; X, Y: Integer);
var
  Follower: TBoldFollower;
begin
  Follower := GetFollowerAt(X, Y);
  if not Assigned(Follower) or Assigned(OnDragDrop) then
  begin
    {$IFNDEF BOLDCOMCLIENT}
    if BoldGuiHandler.ActivateTargetFormOnDrop then
      BoldGUIHandler.TryToFocusHostingForm(self);
    {$ENDIF}
    inherited DragDrop(Source, X, Y);
  end
  else
    Follower.Controller.DragDrop(Follower, Follower.Element, 0);
end;

(*Popup menu*)
function TBoldCustomTreeView.GetPopupmenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
 (* if Assigned(CurrentFollower) then
    Result := CurrentFollower.Controller.Popup.GetMenu(Self, Follower.Element); *)
end;

(*Expand and collapse*)
function TBoldCustomTreeView.CanExpand(Node: TTreeNode): Boolean;
var
  I: Integer;
  ChildNode: TBoldTreeNode;
begin
  Result := inherited CanExpand(Node) and Assigned(TBoldTreeNode(Node).Follower) and ((MaxLevels=-1) or (Node.Level<MaxLevels)) and not TBoldTreeNode(Node).ExistsInParent;
  if Result then
  begin
    BeginUpdate;
    try
      if ((TBoldTreeNode(Node).Follower.SubFollowerCount = 0) or
          (not TBoldTreeNode(Node).Follower.SubFollowers[BoldNodeListIndex].Active)) then
      begin
        Node.HasChildren := False;
        (TBoldTreeNode(Node).Follower.Controller as TBoldNodeFollowerController).SetActiveRange(TBoldTreeNode(Node).Follower, BoldNodeListIndex, BoldNodeTextIndex);
        TBoldTreeNode(Node).Follower.EnsureDisplayable;
      end;
      if (FNodeExpansion = neVisible) and
         (TBoldTreeNode(Node).Follower.SubFollowerCount > BoldNodeListIndex) then
      begin
        for I := 0 to TBoldTreeNode(Node).Follower.SubFollowers[BoldNodeListIndex].SubFollowerCount-1 do
        begin
          ChildNode := (TBoldTreeNode(Node).Follower.SubFollowers[BoldNodeListIndex].SubFollowers[I].ControlData as TBoldTreeNode);
          if Assigned(ChildNode) and
             ((MaxLevels = -1) or (ChildNode.Level < MaxLevels)) and
             not TBoldTreeNode(ChildNode).ExistsInParent and
             ((ChildNode.Follower.SubFollowerCount = 0) or (not ChildNode.Follower.SubFollowers[BoldNodeListIndex].Active)) then
          begin
            ChildNode.HasChildren := False;
            (ChildNode.Follower.Controller as TBoldNodeFollowerController).SetActiveRange(ChildNode.Follower, BoldNodeListIndex, BoldNodeTextIndex);
            ChildNode.Follower.EnsureDisplayable;
          end;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TBoldCustomTreeView.Expand(Node: TTreeNode);
begin
  inherited Expand(Node);
end;

function TBoldCustomTreeView.CanCollapse(Node: TTreeNode): Boolean;
begin
  Result := inherited CanCollapse(Node);
end;

procedure TBoldCustomTreeView.Collapse(Node: TTreeNode);
begin
  inherited Collapse(Node);
  SetSelectionInSubTree(Node, false);

(*  if Assigned(Node.Data) and (TObject(Node.Data) is TBoldFollower) then
    (TBoldFollower(Node.Data).Controller as TBoldNodeFollowerController).SetActiveRange(TBoldFollower(Node.Data), BoldNodeIconIndex, BoldNodeTextIndex);
  Node.DeleteChildren; *)
end;

(*Other*)
function TBoldCustomTreeView.CanChange(Node: TTreeNode): Boolean;
begin
  Result := inherited CanChange(Node);
end;

procedure TBoldCustomTreeView.Change(Node: TTreeNode);
begin
  inherited Change(Node);
  if assigned(fLastSelectedNode) then
    fLastSelectedNode.UpdateIcon;
  if Node is TBoldTreeNode then
  begin
    (Node as TBoldTreeNode).UpdateIcon;
    fLastSelectedNode := Node as TBoldTreeNode;
  end;
end;

function TBoldCustomTreeView.GetFollowerAt(X, Y: Integer): TBoldFollower;
var
  Node: TBoldTreeNode;
begin
  Result := nil;
  Node := TBoldTreeNode(GetNodeAt(X, Y));
  if Assigned(Node) then
    Result := Node.Follower;
end;

function TBoldCustomTreeView.GetElementAt(X, Y: Integer): TBoldElement;
var
  Follower: TBoldFollower;
begin
  Result := nil;
  Follower := GetFollowerAt(X, Y);
  if Assigned(Follower) then
    Result := Follower.Element;
end;

function TBoldCustomTreeView.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
  begin
    if assigned(BoldHandle.StaticBoldType) then
      result := BoldHandle.StaticBoldType
    else
      result := BoldHandle.StaticSystemTypeInfo
  end
  else
    result := nil;
end;

{$IFNDEF BOLDCOMCLIENT}
function TBoldCustomTreeView.ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean;
var
  j, i: integer;
  BaseName: String;
  Context: TBoldElementTypeInfo;
begin
  // We want to evaluate everything. Thus suboptimized expressions.
  Result := True;
  Context := GetContextType;
  ComponentValidator.ValidateExpressionInContext('', Context, NamePrefix + name);
  if Assigned(Context) then
    for i := 0 to BoldProperties.NodeDescriptions.count - 1 do
    begin
      with BoldProperties.NodeDescriptions[i] do
      begin
        Context := NodeFollowerController.ContextType;
        BaseName:= format('%s%s.Node[%d:%s]', [NamePrefix, self.Name, i, Name]); // do not localize

        Result := ComponentValidator.ValidateExpressionInContext('', Context, BaseName);

        if Assigned(Context) then
        begin
          Result := ComponentValidator.ValidateExpressionInContext(
                      IconController.Expression,
                      Context,
                      BaseName + '.IconController') and Result; // do not localize

          Result := ComponentValidator.ValidateExpressionInContext(
                      TextController.Expression,
                      Context,
                      BaseName + '.TextController') and Result; // do not localize

          for j := 0 to ListController.Parts.Count - 1 do
          begin
            Result := ComponentValidator.ValidateExpressionInContext(
                        ListController.Parts[j].ControllerExpression,
                        Context,
                        BaseName + format('.ListPart[%d]', [j])) and Result; // do not localize
          end;
        end;
      end;
    end;
end;
{$ENDIF}

function TBoldCustomTreeView.TreeFollowerControllerClass: TBoldTreeFollowerControllerClass;
begin
  result := TBoldTreeFollowerController;
end;

procedure TBoldCustomTreeView.SetMultiSelect(NewValue: Boolean);
begin
  fMultiSelect := NewValue;
end;

procedure TBoldCustomTreeView.UpdateMultiSelect(Node: TBoldTreeNode; Shift: TShiftState; MouseDirection: TBoldMouseDirection);
  procedure RestoreLastSelectedNode;
  begin
    // restore the selectedstate of the previous node...
    if assigned(fLastSelectedNode) and (fLastSelectedNode <> Node) then
      fLastSelectedNode.SetSelected(fLastSelectedNode.Follower.Selected);
  end;

var
  Start, Stop, i: Integer;

begin
  if not ((ssShift in Shift) or (ssCtrl in Shift)) or not MultiSelect then
  begin
    fSelectAnchor := Node;
    if (MouseDirection = DirMouseUp) or not Node.Follower.Selected then
      ClearAllSelections;

    if (MouseDirection = DirMouseDown) and Node.Follower.Selected then
    begin
      // a MouseDown occured on a node that was already selected,
      // make sure that the previous node is not unselected until
      // mouseup if this is a drag.
      RestoreLastSelectedNode;
    end;

    if assigned(Node) then
      Node.SetSelected(true);

    if MouseDirection = DirMouseDown then
      fLastSelectedNode := Node;

  end
  else if (ssShift in Shift) and MultiSelect then
  begin
    if not assigned(fSelectAnchor) then
      fSelectAnchor := Node;

    if (MouseDirection = DirmouseDown) and Assigned(fSelectAnchor) and Assigned(Node) then
    begin
      Start := fSelectAnchor.AbsoluteIndex;
      Stop := Node.AbsoluteIndex;
      if (Start > Stop) then
      begin
        Start := Node.AbsoluteIndex;
        Stop := fSelectAnchor.AbsoluteIndex;
      end;
      ClearAllSelections;
      for i := Start to Stop do
      begin
        if assigned(Items[i]) and Items[i].IsVisible then
          (Items[i] as TBoldTreeNode).SetSelected(true);
      end;
      fLastSelectedNode := Node;
    end;
  end
  else if (ssCtrl in Shift) and MultiSelect then
  begin

    RestoreLastSelectedNode;

    fSelectAnchor := Node;

    if (MouseDirection = DirMouseDown) and Assigned(Node) then
    begin
      node.SetSelected(not node.Follower.Selected);
      if node.Selected and not Node.Follower.Selected then
        Node.Selected := false;
      fLastSelectedNode := Node;
    end;
  end;
end;

procedure TBoldCustomTreeView.ClearAllSelections;
var
  i: integer;
begin
  for i := 0 to items.Count - 1 do
    (Items[i] as TBoldTreeNode).SetSelected(false);
end;

procedure TBoldTreeNode.SetSelected(const Value: Boolean);
begin
  Follower.Selected := Value;
  Follower.SubFollowers[1].Selected := Value;
  Updateicon;
end;

procedure TBoldCustomTreeView.FillListWithSelectedObjects(List: TBoldObjectList);
var
  i: integer;
  Follower: TBoldFollower;
begin
  for i := 0 to items.Count - 1 do
  begin
    follower := (Items[i] as TBoldTreeNode).Follower;
    if (Items[i].Selected or Follower.Selected) and (BoldTestType(Follower.Element, TBoldObject)) then
      List.Add(Follower.element as TBoldObject);
  end;
end;

destructor TBoldTreeNode.destroy;
begin
  inherited;
  if TreeView.fLastSelectedNode = self then
    TreeView.fLastSelectedNode := nil;
  if TreeView.fSelectAnchor = self then
    TreeView.fSelectAnchor := nil;
  if TreeView.fPopupNode = self then
    TreeView.fPopupNode := nil;
end;

function TBoldTreeNode.GetTreeView: TBoldTreeView;
begin
  result := inherited TreeView as TBoldTreeView;
end;

procedure TBoldCustomTreeView.SetSelectionInSubtree(Node: TTreeNode; Selected: Boolean);
var
  ChildNode: TBoldTreeNode;
begin
  ChildNode := node.getFirstChild as TBoldTreeNode;
  while Assigned(ChildNode) do
  begin
    ChildNode.SetSelected(Selected);
    SetSelectionInSubtree(ChildNode, Selected);
    ChildNode := Node.GetNextChild(ChildNode) as TBoldTreeNode;
  end;
end;

procedure TBoldTreeNode.UpdateIcon;
begin
  if assigned(Follower) and assigned(Follower.SubFollowers[1]) then
    Follower.SubFollowers[1].MarkValueOutOfDate;
end;

procedure TBoldCustomTreeView.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
begin
  fPopUpNode := Selected as TBoldTreeNode;
  inherited;
end;

function TBoldCustomTreeView.GetPopupElement: TBoldElement;
begin
  if assigned(fPopupNode) and assigned(fPopupNode.Follower) then
    result := fPopupNode.Follower.Element
  else
    result := nil;
end;

procedure TBoldCustomTreeView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key in [VK_UP, VK_DOWN, VK_HOME, VK_END, VK_RIGHT, VK_LEFT] then
    UpdateMultiSelect(Selected as TBoldTreeNode, Shift, dirMouseDown);
end;

function TBoldCustomTreeView.FindListPartByNames(const NodeDescName, ListPartName: String): TBoldGenericListPart;
var
  NodeDesc: TBoldNodeDescription;
begin
  NodeDesc := BoldProperties.NodeDescriptions.FindByName(NodeDescName);
  if assigned(NodeDesc) then
    result := NodeDesc.ListController.FindPartByName(ListPartName)
  else
    result := nil;
end;

procedure TBoldCustomTreeView._CustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Follower: TBoldfollower;
  aColor: TColor;
  TextController: TBoldStringFollowerController;
begin
  Follower := (Node as TBoldTreeNode).Follower;
  if assigned(Follower) then
  begin
    TextController := (Follower.Controller as TBoldNodeFollowerController).TextFollowerController;
    if not (cdsSelected in State) then
    begin
      TextController.SetColor(AColor, Color, Follower);
      Canvas.Brush.Color := aColor;
      TextController.SetFont(Canvas.Font, Font, Follower);
    end;
  end;
end;

end.
