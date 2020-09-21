unit fMain;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Graphics,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ActnList,
  Grids,
  BoldElements,
  BoldHandle,
  BoldHandles,
  BoldSystemHandle,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldModel,
  BoldControlPack,
  BoldStringControlPack,
  BoldSubscription,
  BoldGrid,
  BoldNavigator,
  BoldEdit,
  BoldHandleAction,
  BoldActions,
  BoldDBActions,
  BoldAbstractModel,
  BoldNavigatorDefs, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB,
  BoldIBDatabaseAction, ComCtrls, BoldTrackBar, BoldExpressionHandle,
  BoldLabel, BoldReferenceHandle;

type
  TForm1 = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    blhPerson: TBoldListHandle;
    BoldGrid1: TBoldGrid;
    bsrSalaryLevel: TBoldAsStringRenderer;
    BoldNavigator1: TBoldNavigator;
    bsrFullName: TBoldAsStringRenderer;
    lblSalaryBreakPoint: TLabel;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    Button1: TButton;
    Button2: TButton;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldTrackBar1: TBoldTrackBar;
    Label1: TLabel;
    BoldTrackBar2: TBoldTrackBar;
    BoldLabel1: TBoldLabel;
    brhGlobals: TBoldReferenceHandle;
    procedure bsrSalaryLevelSetColor(Element: TBoldElement;
      var AColor: TColor; Representation: Integer; Expression: String);
    procedure bsrSalaryLevelSubscribe(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
    function bsrFullNameGetAsString(Element: TBoldElement;
      Representation: Integer; Expression: String): String;
    procedure bsrFullNameSetAsString(Element: TBoldElement;
      NewValue: String; Representation: Integer; Expression: String);
    procedure bsrFullNameSubscribe(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
    function bsrFullNameMayModify(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber): Boolean;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  RendererExampleClasses;

{$R *.DFM}

// If Salary is less than BreakPoint, set color to silver.
procedure TForm1.bsrSalaryLevelSetColor(Element: TBoldElement;
  var AColor: TColor; Representation: Integer; Expression: String);
begin
  if assigned(element) then
    if (element as TPerson).Salary < (brhGlobals.value as TGlobals).SalaryBreakPoint then
      aColor := clSilver;
end;

procedure TForm1.bsrSalaryLevelSubscribe(Element: TBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
begin
  // subscribe to changes in the salary
  Element.SubscribeToExpression('salary', Subscriber, False);
  // Subscribe to changes in BreakPoint value.
  BoldSystemHandle1.System.SubscribeToExpression('Globals.allInstances->first.salaryBreakPoint', Subscriber, False);
end;

// Assemble a FullName from First and Last.
function TForm1.bsrFullNameGetAsString(Element: TBoldElement;
  Representation: Integer; Expression: String): String;
begin
  Result := '';
  if Assigned(Element) then
  begin
    with Element as TPerson do
    begin
      if FirstName <> '' then
        Result := FirstName
      else
        Result := '<FirstName>';
      if LastName <> '' then
        Result := Result + ' ' + LastName
      else
        Result := Result + ' <LastName>';
    end;
  end;
end;

// Break the FullName at the space and set Firstname and LastName.
procedure TForm1.bsrFullNameSetAsString(Element: TBoldElement;
  NewValue: String; Representation: Integer; Expression: String);
begin
  with element as TPerson do
  begin
    FirstName := Copy(NewValue, 0, (Pos(' ', NewValue) -1));
    LastName  := Copy(NewValue, Pos(' ', NewValue), (Length(NewValue) -
                     Pos(' ', NewValue) +1));
  end;
end;

// Subscribe to changes in FirstName and LastName values.
procedure TForm1.bsrFullNameSubscribe(Element: TBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
begin
  with Element as TPerson do
  begin
    SubscribeToExpression('firstName', Subscriber, False);
    SubscribeToExpression('lastName', Subscriber, False);
  end;
end;

// Check if its OK to change FirstName and LastName.
function TForm1.bsrFullNameMayModify(Element: TBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber): Boolean;
begin
  with Element as TPerson do
  begin
    result := EvaluateExpressionAsDirectElement('firstName').ObserverMayModify( subscriber );
    result := result and EvaluateExpressionAsDirectElement('lastName').ObserverMayModify( subscriber );
  end;
end;

// Set size of form before show.
procedure TForm1.FormCreate(Sender: TObject);
begin
  AutoSize := true;
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

procedure TForm1.BoldActivateSystemAction1SystemOpened(Sender: TObject);
var
  Globals: TGlobals;
begin
  if BoldSystemhandle1.System.ClassByExpressionName['Globals'].Count > 0 then
    Globals :=BoldSystemhandle1.System.ClassByExpressionName['Globals'][0] as TGlobals
  else
    Globals := TGlobals.Create(BoldSystemHandle1.System);
  brhGlobals.Value := Globals;
end;

end.


