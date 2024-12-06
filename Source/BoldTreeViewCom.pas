
{ Global compiler directives }
{$include bold.inc}
unit BoldTreeViewCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  // VCL
  Classes,
  ComCtrls,
  CommCtrl,
  Controls,
  Menus,
  Windows,

  // Bold
  {$IFNDEF BOLDCOMCLIENT}
  BoldComObjectSpace_TLB,
  BoldGui,
  {$ENDIF}
  BoldComObjectSpace_TLB,
  BoldComponentValidatorCom,
  BoldControlPackCom,
  BoldControlsDefs,
  BoldElementHandleFollowerCom,
  BoldGenericListControlPackCom,
  BoldHandlesCom,
  BoldNodeControlPackCom;

type
  {Forward declarations}
  TBoldCustomTreeViewCom = class;
  TBoldTreeViewCom = class;
  TBoldTreeNodeCom = class;

  {---TBoldTreeNodeCom---}
  TBoldTreeNodeCom = class(TTreeNode)
  private
    FFollower: TBoldFollowerCom;
    FNodeDescription: TBoldNodeDescriptionCom;
    function GetNodeDescription: TBoldNodeDescriptionCom;
    function GetHideNodeIfNoVisibleChildren: Boolean;
    procedure SetSelected(const Value: Boolean);
    procedure UpdateIcon;
    function GetTreeView: TBoldTreeViewCom;
  public
    destructor Destroy; override;
    function ExistsInParent: Boolean;
    property Follower: TBoldFollowerCom read FFollower write FFollower;
    property NodeDescription: TBoldNodeDescriptionCom read GetNodeDescription;
    property HideNodeIfNoVisibleChildren: Boolean read GetHideNodeIfNoVisibleChildren;
    property TreeView: TBoldTreeViewCom read GetTreeView;
  end;

  {---TBoldCustomTreeViewCom---}
  TBoldCustomTreeViewCom = class(TCustomTreeView, IBoldValidateableComponentCom)
  private
    fHandleFollower: TBoldElementHandleFollowerCom;
    FTreeController: TBoldTreeFollowerControllerCom;
    FMaxLevels: Integer;
    FAutoExpandLevels: Integer;
    FSelectInserted: Boolean;
    FSelectedIndexDelta: Integer;
    fSelectedImageIndex: integer;
    fStateImageSelected: integer;
    fStateImageUnselected: integer;
    FNodeExpansion: TBoldNodeExpansionMethod;
    FDragFollower: TBoldFollowerCom;
    FEditFollower: TBoldFollowerCom;
    FUpdateCount: Integer;
    fMultiSelect: Boolean;
    fSelectAnchor: TBoldTreeNodeCom;
    fLastSelectedNode: TBoldTreeNodeCom;
    fPopupNode: TBoldTreeNodeCom;
    fMouseDownShiftState: TShiftState;
    fMouseDownNode: TBoldTreeNodeCom;
    fMouseDownPos: TPoint;
    fSelectedNodePreUpdate: TBoldTreeNodeCom;
    fSelectedElementPreUpdate: IBoldElement;
    fSelectedNodeDescriptionPreUpdate: TBoldNodeDescriptionCom;
    fNodesHaveBeenDeleted: Boolean;
    procedure _CustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ClearAllSelections;
    procedure SetBoldHandle(Value: TBoldElementHandleCom);
    function GetBoldHandle: TBoldElementHandleCom;
    function GetRootFollower: TBoldFollowerCom;
    procedure SetTreeController(Value: TBoldTreeFollowerControllerCom);
    procedure SetMaxLevels(Value: Integer);
    procedure SetAutoExpandLevels(Value: Integer);
    procedure SetSelectedIndexDelta(Value: Integer);
    function GetCurrentFollower: TBoldFollowerCom;
    function GetCurrentElement: IBoldElement;
    function GetSelected: TBoldTreeNodeCom;
    procedure SetSelected(Value: TBoldTreeNodeCom);
    function GetContextType: IBoldElementTypeInfo;
    procedure SetMultiSelect(NewValue: Boolean);
    procedure UpdateMultiSelect(Node: TBoldTreeNodeCom; Shift: TShiftState; MouseDirection: TBoldMouseDirection);
    {$IFNDEF BOLDCOMCLIENT}
    function ValidateComponent(ComponentValidator: TBoldComponentValidatorCom; NamePrefix: String): Boolean;
    {$ENDIF}
    property MultiSelect: Boolean read fMultiSelect write SetMultiSelect default false;
    function GetPopupElement: IBoldElement;
  protected
    procedure AfterMakeUptoDate(Follower: TBoldFollowerCom); virtual;
    procedure BeforeMakeUptoDate(Follower: TBoldFollowerCom); virtual;
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
    procedure DeleteNode(Follower: TBoldFollowerCom); virtual;
    procedure DisplayText(Follower: TBoldFollowerCom); virtual;
    procedure DisplayIcon(Follower: TBoldFollowerCom); virtual;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    function DoInsertVisibleNode(Follower: TBoldFollowerCom): TBoldTreeNodeCom;
    procedure DoInsertHiddenNode(Follower: TBoldFollowerCom);
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure Edit(const Item: TTVItem); override;
    procedure Expand(Node: TTreeNode); override;
    function GetPopupMenu: TPopupMenu; override;
    procedure InsertNode(Follower: TBoldFollowerCom); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetNodeState(Node: TBoldTreeNodeCom);
    procedure BeginUpdate;
    procedure EndUpdate;
    function TreeFollowerControllerClass: TBoldTreeFollowerControllerClassCom; virtual;
    {To be published}
    property AutoExpandLevels: Integer read FAutoExpandLevels write SetAutoExpandLevels default -1;
    property BoldHandle: TBoldElementHandleCom read GetBoldHandle  write SetBoldHandle;
    property BoldProperties: TBoldTreeFollowerControllerCom read FTreeController write SetTreeController;
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
    function GetElementAt(X, Y: Integer): IBoldElement;
    function GetFollowerAt(X, Y: Integer): TBoldFollowerCom;
    procedure FillListWithSelectedObjects(List: IBoldObjectList);
    function FindListPartByNames(const NodeDescName, ListPartName: String): TBoldGenericListPartCom;
    property CurrentFollower: TBoldFollowerCom read GetCurrentFollower;
    property CurrentElement: IBoldElement read GetCurrentElement;
    property PopupElement: IBoldElement read GetPopupElement;
    property Selected: TBoldTreeNodeCom read GetSelected write SetSelected;
    property RootFollower: TBoldFollowerCom read GetRootFollower;
  end;

  {---TBoldTreeViewCom---}
  TBoldTreeViewCom = class(TBoldCustomTreeViewCom)
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
  // VCL
  Graphics,
  SysUtils,

  BoldControlPackDefs,
  BoldNumericControlPackCom,
  BoldStringControlPackCom;

{---TBoldTreeNodeCom---}
function TBoldTreeNodeCom.GetNodeDescription: TBoldNodeDescriptionCom;
var
  I: Integer;
begin
  if Assigned(FNodeDescription) and (FNodeDescription.NodeFollowerController = Follower.Controller) then
  begin
    Result := FNodeDescription;
  end
  else
  begin
    I := 0;
    with TBoldTreeViewCom(TreeView).BoldProperties.NodeDescriptions do
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

function TBoldTreeNodeCom.GetHideNodeIfNoVisibleChildren: Boolean;
begin
  Result := (Follower.Controller as TBoldNodeFollowerControllerCom).HideNodeWithNoChildren;
end;

function TBoldTreeNodeCom.ExistsInParent: Boolean;
begin
  Result := Follower.ExistInOwner;
end;

{---TBoldCustomTreeViewCom---}
(*Constructor and destructor*)
constructor TBoldCustomTreeViewCom.Create(aOwner: TComponent);
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


  fHandleFollower := TBoldElementHandleFollowerCom.Create(Owner, FTreeController);
  FMaxLevels := -1;
  FAutoExpandLevels := -1;
  FNodeExpansion := neDemand;
  fSelectedImageIndex := -1;
  fStateImageSelected := -1;
  fStateImageUnselected := -1;
  OnCustomDrawItem := _CustomDrawItem;
end;

destructor TBoldCustomTreeViewCom.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(FTreeController);
  inherited Destroy;
end;

(*Methods for properties*)

procedure TBoldCustomTreeViewCom.SetBoldHandle(Value: TBoldElementHandleCom);
begin
  fHandleFollower.BoldHandle := value
end;

function TBoldCustomTreeViewCom.GetBoldHandle: TBoldElementHandleCom;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomTreeViewCom.GetRootFollower: TBoldFollowerCom;
begin
  Result := fHandleFollower.Follower;
end;

procedure TBoldCustomTreeViewCom.SetTreeController(Value: TBoldTreeFollowerControllerCom);
begin
  FTreeController.Assign(Value);
end;

procedure TBoldCustomTreeViewCom.SetMaxLevels(Value: Integer);

  procedure DoList(Level: Integer; Follower: TBoldFollowerCom);
  var
    I: Integer;
  begin
    for I := 0 to Follower.SubFollowerCount-1 do
    begin
      if (Follower.SubFollowers[I].Controller as TBoldNodeFollowerControllerCom).HideNodeWithNoChildren   then
        DoInsertHiddenNode(Follower.SubFollowers[I])
      else
        SetNodeState(TBoldTreeNodeCom(Follower.SubFollowers[I].ControlData));
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

procedure TBoldCustomTreeViewCom.SetAutoExpandLevels(Value: Integer);
var
  I: Integer;
  Node: TBoldTreeNodeCom;
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
        Node := TBoldTreeNodeCom(Items[I]);
        Node.Expanded := Node.Level <= FAutoExpandLevels;
        Inc(I);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TBoldCustomTreeViewCom.SetSelectedIndexDelta(Value: Integer);
var
  I: Integer;
  Node: TBoldTreeNodeCom;
begin
  if Value <> FSelectedIndexDelta then
  begin
    FSelectedIndexDelta := Value;
    I := 0;
    BeginUpdate;
    try
      while (I < Items.Count) do
      begin
        Node := TBoldTreeNodeCom(Items[I]);
        Node.SelectedIndex := Node.ImageIndex + FSelectedIndexDelta;
        Inc(I);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

function TBoldCustomTreeViewCom.GetCurrentFollower: TBoldFollowerCom;
begin
  Result := nil;
  if Assigned(Selected) then
    Result := Selected.Follower;
end;

function TBoldCustomTreeViewCom.GetCurrentElement: IBoldElement;
begin
  Result := nil;
  if Assigned(Selected) and Assigned(Selected.Follower) and Assigned(Selected.Follower.Element) then
    Result := Selected.Follower.Element;
end;

procedure TBoldCustomTreeViewCom.CreateWnd;
begin
  inherited createWnd;
  RootFollower.Active := true;
end;

procedure TBoldCustomTreeViewCom.DestroyWnd;
begin
  RootFollower.Active := false;
  Items.Clear;
  Inherited DestroyWnd;
end;

function TBoldCustomTreeViewCom.GetSelected: TBoldTreeNodeCom;
begin
  Result := TBoldTreeNodeCom(inherited Selected);
end;

procedure TBoldCustomTreeViewCom.SetSelected(Value: TBoldTreeNodeCom);
begin
  inherited Selected := Value;
end;

procedure TBoldCustomTreeViewCom.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TBoldCustomTreeViewCom.EndUpdate;
begin
  Dec(FUpdateCount);
end;

(*Display values add and remove of nodes*)
procedure TBoldCustomTreeViewCom.DisplayText(Follower: TBoldFollowerCom);
var
  Node: TTreeNode;
begin
  Node := TTreeNode(Follower.OwningFollower.ControlData);
  if Assigned(Node) and (Node is TTreeNode) then
  begin
    Node.Text := (Follower.Controller as TBoldStringFollowerControllerCom).GetCurrentAsString(Follower);
  end;
end;

procedure TBoldCustomTreeViewCom.DisplayIcon(Follower: TBoldFollowerCom);
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
      ImageIndex := (Follower.Controller as TBoldIntegerFollowerControllerCom).GetCurrentAsInteger(Follower);

      if SelectedIndexDelta <> 0 then
        SelectedIndex := ImageIndex + SelectedIndexDelta
      else if SelectedImageIndex <> -1 then
        SelectedIndex := SelectedImageIndex
      else
        SelectedIndex := ImageIndex;
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

function TBoldCustomTreeViewCom.CreateNode: TTreeNode;
begin
  Result := TBoldTreeNodeCom.Create(Items);
end;

procedure TBoldCustomTreeViewCom.AfterMakeUptoDate(Follower: TBoldFollowerCom);
begin


  fSelectedNodePreUpdate := nil;
  fSelectedElementPreUpdate := nil;
  fSelectedNodeDescriptionPreUpdate := nil;
end;

procedure TBoldCustomTreeViewCom.BeforeMakeUptoDate(Follower: TBoldFollowerCom);
begin
  fSelectedNodePreUpdate := Selected;
  fSelectedElementPreUpdate := nil;
  fSelectedNodeDescriptionPreUpdate := nil;
  fNodesHaveBeenDeleted := false;
end;

function TBoldCustomTreeViewCom.DoInsertVisibleNode(Follower: TBoldFollowerCom): TBoldTreeNodeCom;
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
    PrevSiblingNode := (Follower.OwningFollower.SubFollowers[I].ControlData as TBoldTreeNodeCom);
    Inc(I);
  end;

  if Assigned(PrevSiblingNode) then
  begin
    Result := TBoldTreeNodeCom(Items.Insert(PrevSiblingNode, ''))
  end
  else
  begin

    if Assigned(Follower.owningFollower.owningFollower) then
    begin
      if Assigned(Follower.owningFollower.owningFollower.ControlData) then
        ParentNode := (Follower.owningFollower.owningFollower.ControlData as TBoldTreeNodeCom)
      else
        ParentNode := DoInsertVisibleNode(Follower.owningFollower.owningFollower);
    end;

    Result := TBoldTreeNodeCom(Items.AddChild(ParentNode, ''));
  end;

  Result.Follower := Follower;
  Follower.ControlData := Result;

  SetNodeState(Result);
end;

procedure TBoldCustomTreeViewCom.DoInsertHiddenNode(Follower: TBoldFollowerCom);
var
  Level: Integer;

  function GetLevel: Integer;
  var
    aFollower: TBoldFollowerCom;
  begin
    Result := 0;
    aFollower := Follower.OwningFollower;
    while Assigned(aFollower) do
    begin
      if aFollower.Controller is TBoldNodeFollowerControllerCom then
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
      (Follower.Controller as TBoldNodeFollowerControllerCom).SetActiveRange(Follower, BoldNodeListIndex, BoldNodeListIndex);
      Follower.EnsureDisplayable;
      if (Level <= AutoExpandLevels) and Assigned(Follower.ControlData) then
        (Follower.ControlData as TBoldTreeNodeCom).Expand(False);
    finally
      EndUpdate;
    end;
  end
  else
  begin
    Follower.Active := False;
    if Assigned(Follower.ControlData) then
    begin
      TBoldTreeNodeCom(Follower.ControlData).Delete;
      Follower.ControlData := nil;
    end;
  end;
end;

procedure TBoldCustomTreeViewCom.SetNodeState(Node: TBoldTreeNodeCom);
var
  Allow: Boolean;
  Follower: TBoldFollowerCom;
  Controller: TBoldNodeFollowerControllerCom;
begin
  Follower := Node.Follower;
  Controller := (Follower.Controller as TBoldNodeFollowerControllerCom);
  if Controller.HideNodeWithNoChildren then
  begin
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
          Node.HasChildren := (Controller.Items[BoldNodeListIndex] as TBoldGenericListControllerCom).CanHaveSubFollowers;
          Controller.SetActiveRange(Follower, BoldNodeIconIndex, BoldNodeTextIndex);
          Follower.EnsureDisplayable;
        end;
      end;
    end
    else
    begin
      Node.DeleteChildren;
      (Follower.Controller as TBoldNodeFollowerControllerCom).SetActiveRange(Follower, BoldNodeIconIndex, BoldNodeTextIndex);
      Follower.EnsureDisplayable;
    end;
  end;
end;

procedure TBoldCustomTreeViewCom.InsertNode(Follower: TBoldFollowerCom);
var
  NewNode: TBoldTreeNodeCom;
begin
  if not (Follower.Controller as TBoldNodeFollowerControllerCom).HideNodeWithNoChildren then
  begin
    NewNode := DoInsertVisibleNode(Follower);
    if not fNodesHaveBeenDeleted and FSelectInserted and (FUpdateCount < 1) then
      Selected := NewNode;
    if assigned(fSelectedNodePreUpdate) and
      (Follower.Element = fSelectedElementPreUpdate) and
      (TBoldTreeNodeCom(follower.ControlData).NodeDescription = fSelectedNodeDescriptionPreUpdate) then
        Selected := TBoldTreeNodeCom(follower.ControlData);
  end
  else
  begin
    DoInsertHiddenNode(Follower);
  end;
end;

procedure TBoldCustomTreeViewCom.DeleteNode(Follower: TBoldFollowerCom);
var
  ParentNode: TBoldTreeNodeCom;
begin
  if Assigned(Follower.ControlData) then
  begin
    ParentNode := (Follower.ControlData as TBoldTreeNodeCom).Parent as TBoldTreeNodeCom;
    if fSelectedNodePreUpdate = (Follower.ControlData as TBoldTreeNodeCom) then
    begin
      fSelectedElementPreUpdate := Follower.Element;
      fSelectedNodeDescriptionPreUpdate := fLastSelectedNode.NodeDescription;
    end;
    fNodesHaveBeenDeleted := true;

    (Follower.ControlData as TBoldTreeNodeCom).Delete;
    Follower.ControlData := nil;
    if Assigned(ParentNode) and
       ParentNode.HideNodeIfNoVisibleChildren and
       not ParentNode.HasChildren then
    begin
      (ParentNode.Follower.Controller as TBoldNodeFollowerControllerCom).SetActiveRange(ParentNode.Follower, BoldNodeListIndex, BoldNodeListIndex);
      DeleteNode(ParentNode.Follower);
    end;
  end;
end;


(*Test editing*)
function TBoldCustomTreeViewCom.CanEdit(Node: TTreeNode): Boolean;
begin
  FEditFollower := nil;
  Result := inherited CanEdit(Node);
  if Assigned(TBoldTreeNodeCom(Node).Follower) then
  begin
    Result := Result and TBoldTreeNodeCom(Node).Follower.SubFollowers[BoldNodeTextIndex].Controller.MayModify(TBoldTreeNodeCom(Node).Follower.SubFollowers[BoldNodeTextIndex]);
    if Result then
      FEditFollower := TBoldTreeNodeCom(Node).Follower.SubFollowers[BoldNodeTextIndex];
  end;
end;

procedure TBoldCustomTreeViewCom.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if Assigned(FEditFollower) and (Key > #32) and
     not (FEditFollower.Controller as TBoldStringFollowerControllerCom).ValidateCharacter(AnsiChar(Key), FEditFollower) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
end;

procedure TBoldCustomTreeViewCom.Edit(const Item: TTVItem);
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
        (FEditFollower.Controller as TBoldStringFollowerControllerCom).MayHaveChanged(S, FEditFollower);
        FEditFollower.Apply;
      end;
    end;
  end
  else
    if Assigned(FEditFollower) then
      FEditFollower.DiscardChange;
  FEditFollower := nil;
end;

procedure TBoldCustomTreeViewCom.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitTests: THitTests;
begin
  HitTests := GetHitTestInfoAt(x, y);
  if abs(x - fMouseDownPos.x) + abs(y - fMouseDownPos.y) < Mouse.DragThreshold then
  begin
    if (Button = mbLeft) and (htOnItem in HitTests) then
      UpdateMultiSelect(fMouseDownNode, fMouseDownShiftState, DirMouseUp);
  end;
  inherited;
end;

procedure TBoldCustomTreeViewCom.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitTests: THitTests;
begin
  HitTests := GetHitTestInfoAt(x, y);
  if (Button = mbLeft) and (htOnItem in HitTests) then
    FDragFollower := GetFollowerAt(X, Y)
  else
    FDragFollower := nil;
  fMouseDownNode := GetNodeAt(x, y) as TBoldTreeNodeCom;
  fMouseDownPos := Point(x, y);
  if (Button = mbRight) and (Shift = [ssRight]) and (htOnItem in HitTests) then
  begin
    ClearAllSelections;
    SetSelected(fMouseDownNode);
    fMouseDownNode.SetSelected(true);
  end;

  if (Button = mbLeft) and (htOnItem in HitTests) then
    UpdateMultiSelect(fMouseDownNode, Shift, DirMouseDown);
  fMouseDownShiftState := Shift;
  inherited;
end;

(*Drag and Drop*)

procedure TBoldCustomTreeViewCom.DoStartDrag(var DragObject: TDragObject);
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

procedure TBoldCustomTreeViewCom.DoEndDrag(Target: TObject; X, Y: Integer);
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

procedure TBoldCustomTreeViewCom.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Follower: TBoldFollowerCom;
begin
  Follower := GetFollowerAt(X, Y);
  if (not Assigned(Follower) or (Follower.Controller.DropMode = bdpNone)) or Assigned(OnDragOver) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := Follower.Controller.DragOver(Follower, Follower.Element, 0);
end;

procedure TBoldCustomTreeViewCom.DragDrop(Source: TObject; X, Y: Integer);
var
  Follower: TBoldFollowerCom;
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
function TBoldCustomTreeViewCom.GetPopupmenu: TPopupMenu;
begin
  Result := inherited GetPopupMenu;
 (* if Assigned(CurrentFollower) then
    Result := CurrentFollower.Controller.Popup.GetMenu(Self, Follower.Element); *)
end;

(*Expand and collapse*)
function TBoldCustomTreeViewCom.CanExpand(Node: TTreeNode): Boolean;
var
  I: Integer;
  ChildNode: TBoldTreeNodeCom;
begin
  Result := inherited CanExpand(Node) and Assigned(TBoldTreeNodeCom(Node).Follower) and ((MaxLevels=-1) or (Node.Level<MaxLevels)) and not TBoldTreeNodeCom(Node).ExistsInParent;
  if Result then
  begin
    BeginUpdate;
    try
      if ((TBoldTreeNodeCom(Node).Follower.SubFollowerCount = 0) or
          (not TBoldTreeNodeCom(Node).Follower.SubFollowers[BoldNodeListIndex].Active)) then
      begin
        Node.HasChildren := False;
        (TBoldTreeNodeCom(Node).Follower.Controller as TBoldNodeFollowerControllerCom).SetActiveRange(TBoldTreeNodeCom(Node).Follower, BoldNodeListIndex, BoldNodeTextIndex);
        TBoldTreeNodeCom(Node).Follower.EnsureDisplayable;
      end;
      if (FNodeExpansion = neVisible) and
         (TBoldTreeNodeCom(Node).Follower.SubFollowerCount > BoldNodeListIndex) then
      begin
        for I := 0 to TBoldTreeNodeCom(Node).Follower.SubFollowers[BoldNodeListIndex].SubFollowerCount-1 do
        begin
          ChildNode := (TBoldTreeNodeCom(Node).Follower.SubFollowers[BoldNodeListIndex].SubFollowers[I].ControlData as TBoldTreeNodeCom);
          if Assigned(ChildNode) and
             ((MaxLevels = -1) or (ChildNode.Level < MaxLevels)) and
             not TBoldTreeNodeCom(ChildNode).ExistsInParent and
             ((ChildNode.Follower.SubFollowerCount = 0) or (not ChildNode.Follower.SubFollowers[BoldNodeListIndex].Active)) then
          begin
            ChildNode.HasChildren := False;
            (ChildNode.Follower.Controller as TBoldNodeFollowerControllerCom).SetActiveRange(ChildNode.Follower, BoldNodeListIndex, BoldNodeTextIndex);
            ChildNode.Follower.EnsureDisplayable;
          end;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TBoldCustomTreeViewCom.Expand(Node: TTreeNode);
begin
  inherited Expand(Node);
end;

function TBoldCustomTreeViewCom.CanCollapse(Node: TTreeNode): Boolean;
begin
  Result := inherited CanCollapse(Node);
end;

procedure TBoldCustomTreeViewCom.Collapse(Node: TTreeNode);
begin
  inherited Collapse(Node);
  SetSelectionInSubTree(Node, false);

(*  if Assigned(Node.Data) and (TObject(Node.Data) is TBoldFollowerCom) then
    (TBoldFollowerCom(Node.Data).Controller as TBoldNodeFollowerControllerCom).SetActiveRange(TBoldFollowerCom(Node.Data), BoldNodeIconIndex, BoldNodeTextIndex);
  Node.DeleteChildren; *)
end;

(*Other*)
function TBoldCustomTreeViewCom.CanChange(Node: TTreeNode): Boolean;
begin
  Result := inherited CanChange(Node);
end;

procedure TBoldCustomTreeViewCom.Change(Node: TTreeNode);
begin
  inherited Change(Node);
  if assigned(fLastSelectedNode) then
    fLastSelectedNode.UpdateIcon;
  if Node is TBoldTreeNodeCom then
  begin
    (Node as TBoldTreeNodeCom).UpdateIcon;
    fLastSelectedNode := Node as TBoldTreeNodeCom;
  end;
end;

function TBoldCustomTreeViewCom.GetFollowerAt(X, Y: Integer): TBoldFollowerCom;
var
  Node: TBoldTreeNodeCom;
begin
  Result := nil;
  Node := TBoldTreeNodeCom(GetNodeAt(X, Y));
  if Assigned(Node) then
    Result := Node.Follower;
end;

function TBoldCustomTreeViewCom.GetElementAt(X, Y: Integer): IBoldElement;
var
  Follower: TBoldFollowerCom;
begin
  Result := nil;
  Follower := GetFollowerAt(X, Y);
  if Assigned(Follower) then
    Result := Follower.Element;
end;

function TBoldCustomTreeViewCom.GetContextType: IBoldElementTypeInfo;
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
function TBoldCustomTreeViewCom.ValidateComponent(ComponentValidator: TBoldComponentValidatorCom; NamePrefix: String): Boolean;
var
  j, i: integer;
  BaseName: String;
  Context: IBoldElementTypeInfo;
begin
  Result := True;
  Context := GetContextType;
  ComponentValidator.ValidateExpressionInContext('', Context, NamePrefix + name);
  if Assigned(Context) then
    for i := 0 to BoldProperties.NodeDescriptions.count - 1 do
    begin
      with BoldProperties.NodeDescriptions[i] do
      begin
        Context := NodeFollowerController.ContextType;
        BaseName:= format('%s%s.Node[%d:%s]', [NamePrefix, self.Name, i, Name]);

        Result := ComponentValidator.ValidateExpressionInContext('', Context, BaseName);

        if Assigned(Context) then
        begin
          Result := ComponentValidator.ValidateExpressionInContext(
                      IconController.Expression,
                      Context,
                      BaseName + '.IconController') and Result;

          Result := ComponentValidator.ValidateExpressionInContext(
                      TextController.Expression,
                      Context,
                      BaseName + '.TextController') and Result;

          for j := 0 to ListController.Parts.Count - 1 do
          begin
            Result := ComponentValidator.ValidateExpressionInContext(
                        ListController.Parts[j].ControllerExpression,
                        Context,
                        BaseName + format('.ListPart[%d]', [j])) and Result;
          end;
        end;
      end;
    end;
end;
{$ENDIF}

function TBoldCustomTreeViewCom.TreeFollowerControllerClass: TBoldTreeFollowerControllerClassCom;
begin
  result := TBoldTreeFollowerControllerCom;
end;

procedure TBoldCustomTreeViewCom.SetMultiSelect(NewValue: Boolean);
begin
  fMultiSelect := NewValue;
end;

procedure TBoldCustomTreeViewCom.UpdateMultiSelect(Node: TBoldTreeNodeCom; Shift: TShiftState; MouseDirection: TBoldMouseDirection);
  procedure RestoreLastSelectedNode;
  begin
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
          (Items[i] as TBoldTreeNodeCom).SetSelected(true);
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

procedure TBoldCustomTreeViewCom.ClearAllSelections;
var
  i: integer;
begin
  for i := 0 to items.Count - 1 do
    (Items[i] as TBoldTreeNodeCom).SetSelected(false);
end;

procedure TBoldTreeNodeCom.SetSelected(const Value: Boolean);
begin
  Follower.Selected := Value;
  Follower.SubFollowers[1].Selected := Value;
  Updateicon;
end;

procedure TBoldCustomTreeViewCom.FillListWithSelectedObjects(List: IBoldObjectList);
var
  i: integer;
  Follower: TBoldFollowerCom;
begin
  for i := 0 to items.Count - 1 do
  begin
    follower := (Items[i] as TBoldTreeNodeCom).Follower;
    if (Items[i].Selected or Follower.Selected) and (BoldTestType(Follower.Element, IBoldObject)) then
      List.Add(Follower.element as IBoldObject);
  end;
end;

destructor TBoldTreeNodeCom.Destroy;
begin
  inherited;
  if TreeView.fLastSelectedNode = self then
    TreeView.fLastSelectedNode := nil;
  if TreeView.fSelectAnchor = self then
    TreeView.fSelectAnchor := nil;
  if TreeView.fPopupNode = self then
    TreeView.fPopupNode := nil;
end;

function TBoldTreeNodeCom.GetTreeView: TBoldTreeViewCom;
begin
  result := inherited TreeView as TBoldTreeViewCom;
end;

procedure TBoldCustomTreeViewCom.SetSelectionInSubtree(Node: TTreeNode; Selected: Boolean);
var
  ChildNode: TBoldTreeNodeCom;
begin
  ChildNode := node.getFirstChild as TBoldTreeNodeCom;
  while Assigned(ChildNode) do
  begin
    ChildNode.SetSelected(Selected);
    SetSelectionInSubtree(ChildNode, Selected);
    ChildNode := Node.GetNextChild(ChildNode) as TBoldTreeNodeCom;
  end;
end;

procedure TBoldTreeNodeCom.UpdateIcon;
begin
  if assigned(Follower) and assigned(Follower.SubFollowers[1]) then
    Follower.SubFollowers[1].MarkValueOutOfDate;
end;

procedure TBoldCustomTreeViewCom.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
begin
  fPopUpNode := Selected as TBoldTreeNodeCom;
  inherited;
end;

function TBoldCustomTreeViewCom.GetPopupElement: IBoldElement;
begin
  if assigned(fPopupNode) and assigned(fPopupNode.Follower) then
    result := fPopupNode.Follower.Element
  else
    result := nil;
end;

procedure TBoldCustomTreeViewCom.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key in [VK_UP, VK_DOWN, VK_HOME, VK_END, VK_RIGHT, VK_LEFT] then
    UpdateMultiSelect(Selected as TBoldTreeNodeCom, Shift, dirMouseDown);
end;

function TBoldCustomTreeViewCom.FindListPartByNames(const NodeDescName, ListPartName: String): TBoldGenericListPartCom;
var
  NodeDesc: TBoldNodeDescriptionCom;
begin
  NodeDesc := BoldProperties.NodeDescriptions.FindByName(NodeDescName);
  if assigned(NodeDesc) then
    result := NodeDesc.ListController.FindPartByName(ListPartName)
  else
    result := nil;
end;

procedure TBoldCustomTreeViewCom._CustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Follower: TBoldFollowerCom;
  aColor: TColor;
  TextController: TBoldStringFollowerControllerCom;
begin
  Follower := (Node as TBoldTreeNodeCom).Follower;
  TextController := (Follower.Controller as TBoldNodeFollowerControllerCom).TextFollowerController;
  if not (cdsSelected in State) then
  begin
    TextController.SetColor(AColor, Color, Follower);
    Canvas.Brush.Color := aColor;
    TextController.SetFont(Canvas.Font, Font, Follower);
  end;
end;

end.
