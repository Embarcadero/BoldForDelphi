unit fMain;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  ActnList,
  StdCtrls,
  BoldModel,
  BoldHandle,
  BoldHandles,
  BoldSystemHandle,
  BoldVariableHandle,
  BoldExpressionHandle,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldSubscription,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldListHandle,
  BoldComboBox,
  BoldEdit,
  BoldListBox,
  BoldActions,
  BoldHandleAction,
  BoldDBActions, BoldAbstractModel, BoldIBDatabaseAction, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TForm1 = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldListBox1: TBoldListBox;
    blhPerson: TBoldListHandle;
    btxtPerson: TBoldEdit;
    bcboPreferredFood: TBoldComboBox;
    bcboMajorsIn: TBoldComboBox;
    bcboSchool: TBoldComboBox;
    bcboFavouriteMusic: TBoldComboBox;
    blhFood: TBoldListHandle;
    bvhMajorTopic: TBoldVariableHandle;
    btnSave: TButton;
    lblPerson: TLabel;
    lblPreferredFood: TLabel;
    lblMajorsIn: TLabel;
    lblFavouriteMusic: TLabel;
    btnAdd: TButton;
    btnRemove: TButton;
    bchMajorTopic: TBoldCursorHandle;
    lblSchool: TLabel;
    gboFood: TGroupBox;
    blstFood: TBoldListBox;
    gboSchool: TGroupBox;
    blstSchool: TBoldListBox;
    btxtSchool: TBoldEdit;
    btxtFood: TBoldEdit;
    btnDelFood: TButton;
    btnAddFood: TButton;
    btnDelSchool: TButton;
    btnAddSchool: TButton;
    blhSchool: TBoldListHandle;
    bvhMusic: TBoldVariableHandle;
    bchMusic: TBoldCursorHandle;
    Bevel1: TBevel;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    Button1: TButton;
    Button2: TButton;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure btnSaveClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnAddFoodClick(Sender: TObject);
    procedure btnDelFoodClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAddSchoolClick(Sender: TObject);
    procedure btnDelSchoolClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
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
  ComboboxClasses,
  BoldElements,
  BoldSystem;

{$R *.DFM}

// Save all changes to database.
procedure TForm1.btnSaveClick(Sender: TObject);
begin
  BoldSystemHandle1.UpdateDatabase;
end;

// Add a new Person.
procedure TForm1.btnAddClick(Sender: TObject);
begin
  TPerson.Create(nil);
end;

// Removes all links and deletes the current person.
procedure TForm1.btnRemoveClick(Sender: TObject);
begin
  with blhPerson.CurrentBoldObject do
  begin
    UnLinkAll;
    Delete;
  end;
end;

// Add a new type of Food.
procedure TForm1.btnAddFoodClick(Sender: TObject);
begin
  blhFood.List.AddNew;
end;

// Remove all links and delete the current food.
procedure TForm1.btnDelFoodClick(Sender: TObject);
begin
  with blhFood.CurrentBoldObject do
  begin
    UnLinkAll;
    Delete;
  end;
end;

// Set size of form before show.
procedure TForm1.FormCreate(Sender: TObject);
begin
  Width := 354;
  Height := 393;
  Randomize;
end;

// Add a new School.
procedure TForm1.btnAddSchoolClick(Sender: TObject);
begin
  blhSchool.List.AddNew;
end;

// Delete the current school.
procedure TForm1.btnDelSchoolClick(Sender: TObject);
begin
  blhSchool.CurrentBoldObject.Delete;
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

// Give the user the opportunity to save changes before close.
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

// Populate classes and save the database if needed.
procedure TForm1.BoldActivateSystemAction1SystemOpened(Sender: TObject);
begin
  if blhFood.List.Count < 1 then
    PopulateClass('Food', 'Name', blhFood);
  if blhSchool.List.Count < 1 then
    PopulateClass('School', 'Name', blhSchool);
  if BoldSystemHandle1.System.DirtyObjects.Count > 0 then
    BoldSystemHandle1.System.UpdateDatabase;
end;

end.
