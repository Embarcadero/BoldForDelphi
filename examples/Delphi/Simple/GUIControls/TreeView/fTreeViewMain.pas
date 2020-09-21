unit fTreeViewMain;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  ImgList,
  ActnList,
  StdCtrls,
  ExtCtrls,
  Grids,
  BoldHandle,
  BoldHandles,
  BoldSystemHandle,
  BoldExpressionHandle,
  BoldRootedHandles,
  BoldReferenceHandle,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldSubscription,
  BoldModel,
  BoldNavigator,
  BoldTreeView,
  BoldEdit,
  BoldListBox,
  BoldGrid,
  BoldHandleAction,
  BoldActions,
  BoldDBActions, BoldAbstractModel, BoldNavigatorDefs,
  BoldIBDatabaseAction, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    blhMTB: TBoldListHandle;
    blhAllFrames: TBoldListHandle;
    blhAllBrakes: TBoldListHandle;
    blhAllGears: TBoldListHandle;
    blhAllWheels: TBoldListHandle;
    blhComponents: TBoldListHandle;
    behFrame: TBoldExpressionHandle;
    brhTreeRoot: TBoldReferenceHandle;
    btnShowAll: TButton;
    pgcTreeViews: TPageControl;
    tsBasicTree: TTabSheet;
    btrvBasic: TBoldTreeView;
    tsEnhancedTree: TTabSheet;
    btrvEnhanced: TBoldTreeView;
    imlTreeNodeIcons: TImageList;
    grpFrames: TGroupBox;
    BoldGrid2: TBoldGrid;
    BoldNavigator2: TBoldNavigator;
    grpBrakes: TGroupBox;
    grpGears: TGroupBox;
    GroupBox3: TGroupBox;
    BoldGrid3: TBoldGrid;
    BoldNavigator3: TBoldNavigator;
    BoldGrid4: TBoldGrid;
    BoldNavigator4: TBoldNavigator;
    BoldGrid5: TBoldGrid;
    BoldNavigator5: TBoldNavigator;
    BoldGrid1: TBoldGrid;
    lblFrame: TLabel;
    BoldEdit1: TBoldEdit;
    lblComponents: TLabel;
    BoldListBox1: TBoldListBox;
    BoldNavigator1: TBoldNavigator;
    lblMountainbike: TLabel;
    btnCurrentAsRoot: TButton;
    Panel1: TPanel;
    imgUnlink: TImage;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button1: TButton;
    Button2: TButton;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure btnShowAllClick(Sender: TObject);
    procedure btrvEnhancedDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure btrvEnhancedDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure btnCurrentAsRootClick(Sender: TObject);
    procedure btrvEnhancedStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure pgcTreeViewsChange(Sender: TObject);
    procedure imgUnlinkDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure imgUnlinkDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
    procedure BoldActivateSystemAction1SystemClosed(Sender: TObject);
  private
    { Private declarations }
    procedure PopulateClass(AClassname, APropName: string; AListHandle: TBoldListHandle);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  BoldElements,
  BoldGUI,
  BoldSystem,
  TreeViewExampleClasses;

var
  DragContext: TBoldObject;
  TargetObject: TMTB;
  CurrentTreeView: TBoldTreeView;

{$R *.DFM}

// Show all MTB in treeview.
procedure TForm1.btnShowAllClick(Sender: TObject);
begin
    brhTreeRoot.value := BoldSystemHandle1.System.ClassByExpressionName['MTB'];
end;

// Accept drop if we recieve only one object that is not a MTB.
procedure TForm1.btrvEnhancedDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  TargetElement: TBoldElement;
begin
  Accept := False;
  TargetElement := (sender as TBoldTreeView).GetElementAt(X, Y);
  if (BoldGuiHandler.DraggedObjects.Count = 1) and
     (TargetElement is TMTB) and 
     not (BoldGuiHandler.DraggedObjects[0] is TMTB) then
  begin
    TargetObject := TargetElement as TMTB;
    Accept := (BoldGuiHandler.DraggedObjects[0] is TParts) or
              (BoldGuiHandler.DraggedObjects[0] is TFrame);
  end;
end;

// Replace frame or add new components.
procedure TForm1.btrvEnhancedDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  ObjectToLink: TBoldObject;
begin
  ObjectToLink := BoldGuiHandler.DraggedObjects[0];
  if ObjectToLink is TParts then
    TargetObject.ConsistsOf.Add(ObjectToLink as TParts)
  else
    TargetObject.BuiltAround := ObjectToLink as TFrame;
end;

// Set size of form and set current treeview.
procedure TForm1.FormCreate(Sender: TObject);
begin
  Height := 508;
  Width  := 641;
  pgcTreeViews.ActivePage := tsBasicTree;
  CurrentTreeView := btrvBasic;
end;

// Set current object to root for treviews.
procedure TForm1.btnCurrentAsRootClick(Sender: TObject);
begin
  brhTreeRoot.Value := CurrentTreeView.CurrentElement;
end;

// Set DragContext if the current object has a MTB parent.
procedure TForm1.btrvEnhancedStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  ABoldElement: TBoldElement;
begin
  DragContext := nil;
  if Assigned(CurrentTreeView.Selected) and assigned(CurrentTreeView.Selected.Parent) then
  begin
    ABoldElement := (CurrentTreeView.Selected.Parent as TBoldTreeNode).Follower.Element;
    if ABoldElement is TMTB then
      DragContext := ABoldElement as TBoldObject;
  end;
end;

// Set CurrentTreeView.
procedure TForm1.pgcTreeViewsChange(Sender: TObject);
begin
  if pgcTreeViews.ActivePage = tsBasicTree then
    CurrentTreeView := btrvBasic
  else
    CurrentTreeView := btrvEnhanced;
end;

// Acccept drop if there is only one dragged object and if DragContext is set.
procedure TForm1.imgUnlinkDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  Accept := (BoldGuiHandler.DraggedObjects.Count = 1) and
            (Assigned(DragContext));
end;

// Unlink dragged object from its parent.
procedure TForm1.imgUnlinkDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  ObjectToUnlink: TBoldObject;
begin
  ObjectToUnlink := BoldGuiHandler.DraggedObjects[0];
  if ObjectToUnlink is TFrame then
    (DragContext as TMTB).BuiltAround := nil
  else
    (DragContext as TMTB).ConsistsOf.Remove(ObjectToUnlink);
end;

// Populate a class with a few objects.
procedure TForm1.PopulateClass(AClassName, APropName: string; AListHandle: TBoldListHandle);
var
  NameStrings:  TStrings;
  i: integer;
  AElement: TBoldElement;
begin
  NameStrings := TStringList.Create;
  NameStrings.LoadFromFile(AClassName+ '.txt');
  for i:=0 to NameStrings.Count -1 do
  begin
    AElement := AListHandle.List.AddNew;
    TBoldObject(AElement).BoldMemberByExpressionName[APropName].AsString := NameStrings[i];
  end;
end;

// Check if its OK to close the form.
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if BoldSystemHandle1.Active then
  if BoldSystemHandle1.System.DirtyObjects.Count > 0 then
    case MessageDlg( 'There are dirty objects. save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: BoldSystemHandle1.System.UpdateDatabase;
      mrNo: BoldSystemHandle1.System.Discard;
      mrCancel: CanClose := False;
    end;
end;

// Set root expression for treeviews when system opens.
procedure TForm1.BoldActivateSystemAction1SystemOpened(Sender: TObject);
begin
  if blhAllFrames.List.Count < 1 then
    PopulateClass('Frame', 'Name', blhAllFrames);
  if blhAllGears.List.Count < 1 then
    PopulateClass('Gear', 'Model', blhAllGears);
  if blhAllBrakes.List.Count < 1 then
    PopulateClass('Brake', 'Model', blhAllBrakes);
  if blhAllWheels.List.Count < 1 then
    PopulateClass('Wheel', 'Model', blhAllWheels);
  if blhMTB.List.Count < 1 then
    PopulateClass('MTB', 'Name', blhMTB);
  if BoldSystemHandle1.System.DirtyObjects.Count > 0 then
    BoldSystemHandle1.System.UpdateDatabase;
  brhTreeRoot.Value := BoldSystemHandle1.System.ClassByExpressionName['MTB'];
  btnShowAll.Enabled := BoldSystemHandle1.Active;  
end;

procedure TForm1.BoldActivateSystemAction1SystemClosed(Sender: TObject);
begin
  btnShowAll.Enabled := BoldSystemHandle1.Active;
end;

end.
