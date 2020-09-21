unit Mainform;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  BoldVariableHandle,
  BoldSubscription,
  BoldHandles,
  BoldRootedHandles,
  BoldDerivedHandle,
  BoldSystemHandle,
  BoldModel,
  BoldHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  StdCtrls,
  ExtCtrls,
  BoldNavigator,
  Grids,
  BoldGrid,
  BoldTypeNameHandle,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldSystem,
  BoldElements,
  BoldListHandle,
  ComCtrls,
  BoldEdit,
  BoldTrackBar,
  BoldActions,
  ActnList,
  BoldHandleAction,
  BoldDBActions,
  BoldAbstractModel,
  BoldNavigatorDefs, BoldIBDatabaseAction, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    grdAllPersons: TBoldGrid;
    bnAllPersons: TBoldNavigator;
    lblPersons: TLabel;
    blhAllPersons: TBoldListHandle;
    grdRichPersons: TBoldGrid;
    Label2: TLabel;
    UpDown1: TUpDown;
    Label3: TLabel;
    bdhRichPersons: TBoldDerivedHandle;
    bchRichPersons: TBoldCursorHandle;
    bvhRichCount: TBoldVariableHandle;
    edRichPersonCount: TBoldEdit;
    btbRichBreakpoint: TBoldTrackBar;
    bvhAssetBreakpoint: TBoldVariableHandle;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edRichBreakpoint: TBoldEdit;
    GroupBox1: TGroupBox;
    btnUpdate: TButton;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button2: TButton;
    Button3: TButton;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure bdhRichPersonsDeriveAndSubscribe(Sender: TComponent;
      RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
      Subscriber: TBoldSubscriber);
    procedure BoldSystemActivator1SystemActivated(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function RichSorter(Person1, Person2: TBoldElement): Integer;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  DerivedHandleExampleClasses,
  BoldAttributes;

{$R *.DFM}

// Main method, Detailed comments below.
procedure TForm1.bdhRichPersonsDeriveAndSubscribe(Sender: TComponent;
  RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
  Subscriber: TBoldSubscriber);
var
  AllPersons, RichPersons: TPersonList;
  i: Integer;
begin
  // This is the main method, derives and subscribes.
  if Assigned(RootValue) then
  begin
    with RootValue as TBoldSystem do
    begin
      // Create a temp list of all persons
      AllPersons := ClassByExpressionName['Person'] as TPersonList;
      RichPersons := allPersons.Clone as TPersonList;
      // assign the sort method (the list has to be sorted to get the riches persons,
      // not just rich enough)
      RichPersons.Sort(RichSorter);
      for i := RichPersons.Count - 1 downto 0 do
      begin
        // subscribe to M_Assets member of every person
        RichPersons[i].M_Assets.DefaultSubscribe(Subscriber);
        // if the person is not rich enough or the limit is reached, remove the person
        if (RichPersons[i].assets < (bvhAssetBreakpoint.Value as TBACurrency).AsCurrency) or
        (i >= (bvhRichCount.Value as TBAInteger).asInteger) then
          RichPersons.RemoveByIndex(i);
      end;
      // subscribe to the list itself
      AllPersons.DefaultSubscribe(Subscriber, breReSubscribe);
      // set the result of the method
      ResultElement.SetOwnedValue(RichPersons);
    end;
    // subscribe to the rich breakpoint and the person count limit (changeable via GUI).
    bvhRichCount.Value.DefaultSubscribe(Subscriber);
    bvhAssetBreakpoint.Value.DefaultSubscribe(Subscriber);
  end;
end;

// Sort Persons by assets.
function TForm1.RichSorter(Person1, Person2: TBoldElement): Integer;
begin
  result := round((Person2 as TPerson).assets - (Person1 as TPerson).assets);
end;

// Adds some exampel persons if there are none when the
// system gets activated.
procedure TForm1.BoldSystemActivator1SystemActivated(Sender: TObject);
var
  aPerson: TPerson;
  i: Integer;
const
  ExamplePersons: array [1..10, 1..2] of String = (
  ('10000', 'Adam'),
  ('20000', 'Bertil'),
  ('30000', 'Caesar'),
  ('40000', 'David'),
  ('50000', 'Eric'),
  ('60000', 'Fredrik'),
  ('70000', 'Gustaf'),
  ('80000', 'Henrik'),
  ('90000', 'Ivar'),
  ('100000', 'Jacob')
  );

begin
  if blhAllPersons.Count = 0 then
  begin
    for i := 1 to 10 do
    begin
      aPerson := TPerson.Create(nil);
      aPerson.Assets := StrToFloat(ExamplePersons[i, 1]);
      aPerson.firstname := ExamplePersons[i, 2];
      (blhAllPersons.List as TPersonList).Add(aPerson);
    end;
  end;
end;

// Set síze of form on create.
procedure TForm1.FormCreate(Sender: TObject);
begin
  Height := 555;
  Width  := 339;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if BoldSystemHandle1.Active and BoldSystemHandle1.System.BoldDirty then
    case MessageDlg( 'There are dirty objects. Save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: BoldSystemHandle1.System.UpdateDatabase;
      mrNo: BoldSystemHandle1.System.Discard;
      mrCancel: CanClose := False;
    end;
end;

end.
