unit fConstraintsMain;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Grids,
  ActnList,
  ExtCtrls,
  BoldHandle,
  BoldHandles,
  BoldSystemHandle,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldSubscription,
  BoldModel,
  BoldGrid,
  BoldExpressionHandle,
  BoldAttributes,
  BoldHandleAction,
  BoldActions,
  BoldNavigator,
  BoldDefs,
  BoldAfp,
  BoldComboBox,
  BoldAFPPluggable,
  BoldDBActions,
  BoldAbstractModel,
  BoldNavigatorDefs, BoldIBDatabaseAction, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldSystemHandle1: TBoldSystemHandle;
    blhCustomers: TBoldListHandle;
    BoldGrid1: TBoldGrid;
    btnSave: TButton;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button1: TButton;
    Button2: TButton;
    blhOrders: TBoldListHandle;
    blhOrderItems: TBoldListHandle;
    BoldNavigator1: TBoldNavigator;
    BoldGrid2: TBoldGrid;
    BoldGrid3: TBoldGrid;
    BoldNavigator2: TBoldNavigator;
    BoldNavigator3: TBoldNavigator;
    blhAllParts: TBoldListHandle;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BoldGrid4: TBoldGrid;
    BoldNavigator5: TBoldNavigator;
    BoldComboBox1: TBoldComboBox;
    Label5: TLabel;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    blhBrokenConstraints: TBoldListHandle;
    BoldGrid5: TBoldGrid;
    Label6: TLabel;
    Label7: TLabel;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure btnSaveClick(Sender: TObject);
    procedure BoldGrid5DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
    procedure BoldActivateSystemAction1SystemClosed(Sender: TObject);
//    procedure BoldAsStringRenderer1SetColor(Element: TBoldElement;
//      var AColor: TColor; Representation: Integer; Expression: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ConstraintExampleClasses;

{$R *.DFM}

// Save all changes to database.
procedure TForm1.btnSaveClick(Sender: TObject);
begin
  BoldSystemHandle1.UpdateDatabase;
end;

procedure TForm1.BoldGrid5DblClick(Sender: TObject);
begin
  // Remap the form shown to the object breaking the constraint
  AutoFormProviderRegistry.FormForElement((blhBrokenConstraints.CurrentElement as TBAConstraint).OwningElement).Show;
end;

{

// if the attributes had constraints, the following piece of code would be meaningful to
// use to mark offending attributes in another color

procedure TForm1.BoldAsStringRenderer1SetColor(Element: TBoldElement;
  var AColor: TColor; Representation: Integer; Expression: String);
var
  ie: TBoldIndirectElement;
  res: string;
begin
  ie := TBoldIndirectElement.Create;
  if assigned( element ) then
  begin
    element.EvaluateExpression( expression, ie );
    if assigned( ie.value ) then
    begin
      res := ie.value.EvaluateExpressionAsString( 'constraints->select(c|not c)->size', brDefault );
      if res <> '0' then
        acolor := clRed;
    end;
  end;
  ie.free;
end;
}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Width := 603;
  height := 582;
end;

procedure TForm1.BoldActivateSystemAction1SystemOpened(Sender: TObject);
begin
  btnSave.Enabled := BoldSystemHandle1.Active;
end;

procedure TForm1.BoldActivateSystemAction1SystemClosed(Sender: TObject);
begin
  btnSave.Enabled := BoldSystemHandle1.Active;
end;

end.


