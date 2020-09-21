unit main;

interface
uses
  SysUtils,
  Dialogs,  
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Grids,
  ImgList,
  ActnList,
  boldElements,
  boldSubscription,
  BoldHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldRootedHandles,
  BoldListHandle,
  BoldExpressionHandle,
  BoldReferenceHandle,
  BoldControlPack,
  BoldModel,
  BoldNavigator,
  BoldGrid,
  BoldEdit,
  BoldListBox,
  BoldTreeView,
  BoldHandleAction,
  BoldActions,
  BoldDBActions, BoldNavigatorDefs, BoldIBDatabaseAction;

type
  TfrmMain = class(TForm)
    BoldTreeView1: TBoldTreeView;
    ImageList1: TImageList;
    Panel1: TPanel;
    Products: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LblNavigator: TLabel;
    grdProducts: TBoldGrid;
    grdSimpleProducts: TBoldGrid;
    grdAssemblies: TBoldGrid;
    lbAssemblyParts: TBoldListBox;
    lsProductPartOf: TBoldListBox;
    btnUpdateDatabase: TButton;
    NvgShared: TBoldNavigator;
    hdlAllProducts: TBoldListHandle;
    hdlAllSimpleProducts: TBoldListHandle;
    hdlProductPartOf: TBoldListHandle;
    hdlAssemblyParts: TBoldListHandle;
    hdlAllAssemblies: TBoldListHandle;
    Bevel1: TBevel;
    Splitter1: TSplitter;
    btnChangeRoot: TButton;
    btnShowAll: TButton;
    hdlTreeRoot: TBoldReferenceHandle;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button1: TButton;
    Button2: TButton;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure grdProductsEnter(Sender: TObject);
    procedure grdSimpleProductsEnter(Sender: TObject);
    procedure grdAssembliesEnter(Sender: TObject);
    procedure grdAssembliesDblClick(Sender: TObject);
    procedure btnChangeRootClick(Sender: TObject);
    procedure btnShowAllClick(Sender: TObject);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure lbAssemblyPartsKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  BoldGridRTColEditor,
  ProdStructClasses,
  dm1;

{$R *.DFM}

procedure TfrmMain.grdProductsEnter(Sender: TObject);
begin
  {This is an example of how you change the behaviour of a component depending on focus.}
  nvgShared.Boldhandle := hdlAllProducts;
  lblnavigator.Caption := 'Products';
end;

procedure TfrmMain.grdSimpleProductsEnter(Sender: TObject);
begin
  {This is an example of how you change the behaviour of a component depending on focus.}
  nvgShared.Boldhandle := hdlAllSimpleProducts;
  lblnavigator.Caption := 'Simple Products';
end;

procedure TfrmMain.grdAssembliesEnter(Sender: TObject);
begin
  {This is an example of how you change the behaviour of a component depending on focus.}
  nvgShared.Boldhandle := hdlAllAssemblies;
  lblnavigator.Caption := 'Assemblies';
end;

procedure TfrmMain.grdAssembliesDblClick(Sender: TObject);
begin
  {Show the Runtime Column Editor when the user double clicks}
  with TfrmRTColEditor.Create(nil) do
  try
    Execute(Sender as TBoldGrid);
  finally
    Free;
  end;
end;

procedure TfrmMain.btnChangeRootClick(Sender: TObject);
begin
  {This is an example of how you change the root on a TreeView component.
  The Root and BoldHandle property are mutual exclusive.
  Root is set to the element on the active row in the Products Grid.}
  hdlTreeRoot.Value := hdlAllProducts.CurrentElement;
//  BoldTreeView1.AutoExpandLevels := 1;
end;

procedure TfrmMain.btnShowAllClick(Sender: TObject);
begin
  {This is an example of how you change the root on a TreeView component.
  The Root and BoldHandle property are mutual exclusive.
  BoldHandle is set to the entire system and the expression evaluates to a list of all products.
  Assembly and Simple_Product are concreate subclasses of the abstract class Product. }
  hdlTreeRoot.value := dmMain.BoldSystem.System.ClassByExpressionName['Product'];
end;

procedure TfrmMain.BoldActivateSystemAction1SystemOpened(Sender: TObject);
var
  Blade, Handle: TSimple_Product;
  Knife, KnifeKit: TAssembly;
begin

  if hdlAllProducts.Count = 0 then
  begin
    Blade := TSimple_Product.Create(dmMain.BoldSystem.System);
    Blade.Name := 'BigBlade';
    Blade.ProductionCost := 5;

    Handle := TSimple_Product.Create(dmMain.BoldSystem.System);
    Handle.Name := 'Handle';
    Handle.ProductionCost := 2;

    Knife := TAssembly.Create(dmMain.BoldSystem.System);
    Knife.Name := 'BigKnife';
    Knife.AssemblyCost := 1;
    Knife.Parts.Add(Blade);
    Knife.Parts.Add(Handle);

    KnifeKit := TAssembly.Create(dmMain.BoldSystem.System);
    KnifeKit.Name := 'KnifeKit';
    KnifeKit.AssemblyCost := 1;
    KnifeKit.Parts.Add(Knife);

    Blade := TSimple_Product.Create(dmMain.BoldSystem.System);
    Blade.Name := 'SmallBlade';
    Blade.ProductionCost := 4;

    Knife := TAssembly.Create(dmMain.BoldSystem.System);
    Knife.Name := 'SmallKnife';
    Knife.AssemblyCost := 1;
    Knife.Parts.Add(Blade);
    Knife.Parts.Add(Handle);

    KnifeKit.Parts.Add(Knife);
  end;
  hdlTreeRoot.value := dmMain.BoldSystem.System.ClassByExpressionName['Product'];
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if dmMain.BoldSystem.Active then
    if dmMain.BoldSystem.System.DirtyObjects.Count > 0 then
      case MessageDlg( 'There are dirty objects. Save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
        mrYes: dmMain.BoldSystem.System.UpdateDatabase;
        mrNo: dmMain.BoldSystem.System.Discard;
        mrCancel: CanClose := False;
      end;
end;


procedure TfrmMain.lbAssemblyPartsKeyPress(Sender: TObject; var Key: Char);
begin
  {This is an example of how you remove a relation when the user press backspace.}
  if Key = #8 then
    TAssembly(grdAssemblies.CurrentBoldElement).Parts.Remove(lbAssemblyParts.CurrentBoldObject);
end;

end.
