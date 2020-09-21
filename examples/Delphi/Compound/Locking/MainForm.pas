unit MainForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  MainDM,
  StdCtrls,
  Grids,
  BoldGrid,
  BoldSubscription,
  BoldHandles,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  ExtCtrls,
  BoldNavigator,
  BoldAFPPluggable,
  BoldDefaultTaggedValues,
  BoldAbstractModel,
  BoldVariableHandle,
  BoldListBox,
  BoldExceptionHandlers,
  BoldElements,
  BoldNavigatorDefs,
  ActnList,
  BoldHandleAction,
  BoldActions;

type
  TForm1 = class(TForm)
    blhAllItems: TBoldListHandle;
    BoldGrid1: TBoldGrid;
    BoldNavigator1: TBoldNavigator;
    Label1: TLabel;
    BoldGrid2: TBoldGrid;
    blhAllColours: TBoldListHandle;
    Label2: TLabel;
    BoldNavigator2: TBoldNavigator;
    BoldGrid3: TBoldGrid;
    blhAllOrders: TBoldListHandle;
    BoldGrid4: TBoldGrid;
    blhOrderLines: TBoldListHandle;
    BoldNavigator3: TBoldNavigator;
    BoldNavigator4: TBoldNavigator;
    Label3: TLabel;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    cbPessimisticLocking: TCheckBox;
    cbOptimisticLocking: TCheckBox;
    cbPropagator: TCheckBox;
    Button4: TButton;
    GroupBox1: TGroupBox;
    BoldListBox1: TBoldListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    BoldVariableHandle1: TBoldVariableHandle;
    BoldCursorHandle1: TBoldCursorHandle;
    BoldExceptionHandler1: TBoldExceptionHandler;
    edtServerMachineName: TEdit;
    Label4: TLabel;
    edtUserName: TEdit;
    Label5: TLabel;
    ActionList1: TActionList;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    cmdSave: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure cbPessimisticLockingClick(Sender: TObject);
    procedure cbPropagatorClick(Sender: TObject);
    procedure BoldExceptionHandler1ApplyException(E: Exception;
      Component: TComponent; Elem: TBoldElement; var Discard: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ModelDM,
  BoldDefs,
  BoldLockHandler;

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  dmMain.bshLocking.System.UpdateDatabaseWithList(BoldVariableHandle1.ObjectList);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if not dmMain.bshLocking.System.EnsureEnclosure(BoldVariableHandle1.ObjectList, false) then
    showmessage('Additional objects has to be saved, they have been added to the list');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  BoldVariableHandle1.ObjectList.Clear;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if not dmMain.bshLocking.Active then
  begin
    // Adjust remote persistence connection
    dmMain.bcchPersistence.ServerHost := edtServerMachineName.Text;
    dmMain.bcchPersistence.Connected := True;

    // Adjust Propagator
    dmMain.bcchPropagatorServer.ServerHost := edtServerMachineName.Text;
    dmMain.BoldListenerHandle1.ClientIdentifierString := edtUserName.Text;
    dmMain.bcchPropagatorServer.Connected := cbPropagator.Checked;
    dmMain.BoldPropagatorHandleCOM1.Active := cbPropagator.Checked;
    dmMain.BoldListenerHandle1.SetActive(cbPropagator.Checked);

    // Adjust Pessimistic Locking
    dmMain.bcchPropagatorServer.ServerHost := edtServerMachineName.Text;
    dmMain.BoldLockManagerHandleCom1.Active := cbPessimisticLocking.Checked;
    if cbPessimisticLocking.Checked then
      dmMain.BoldLockingHandle1.SystemHandle := dmMain.bshLocking
    else
      dmMain.BoldLockingHandle1.SystemHandle := nil;

    // Adjust Optimistic Locking
    if cbOptimisticLocking.Checked then
      dmModel.BoldModel1.MoldModel.BoldTVByName[TAG_OPTIMISTICLOCKING] := TV_OPTIMISTICLOCKING_TIMESTAMP
    else
      dmModel.BoldModel1.MoldModel.BoldTVByName[TAG_OPTIMISTICLOCKING] := TV_OPTIMISTICLOCKING_OFF;
    dmModel.BoldModel1.SendEvent(dmModel.BoldModel1, beModelChanged);
  end
  else
  begin
    dmMain.bcchPersistence.Connected := false;

    dmMain.bcchPropagatorServer.Connected := false;
    dmMain.BoldLockManagerHandleCom1.Active := false;

    dmMain.BoldListenerHandle1.SetActive(false);
    dmMain.BoldPropagatorHandleCOM1.Active := false;
  end;

  // Toggle the system
  dmMain.bshLocking.Active := not dmMain.bshLocking.Active;

  // Adjust gui
  cbPessimisticLocking.Enabled := not dmMain.bshLocking.Active;
  cbPropagator.Enabled := not dmMain.bshLocking.Active;
  cbOptimisticLocking.Enabled := not dmMain.bshLocking.Active;
  if dmMain.bshLocking.Active then
    Button4.Caption := 'Stop System'
  else
    Button4.Caption := 'Start System';
end;

procedure TForm1.cbPessimisticLockingClick(Sender: TObject);
begin
  // pessimistic locking requires propagator
  if cbPessimisticLocking.Checked then
    cbPropagator.Checked := true;
end;

procedure TForm1.cbPropagatorClick(Sender: TObject);
begin
  // pessimistic locking requires propagator
  if not cbPropagator.Checked then
    cbPessimisticLocking.Checked := false;
end;

procedure TForm1.BoldExceptionHandler1ApplyException(E: Exception;
  Component: TComponent; Elem: TBoldElement; var Discard: Boolean);
var
  i: Integer;
  ConflictingObjects: string;
begin    
  ConflictingObjects := '';
  for i := 0 to EBoldGetLocksFailed(E).ConflictingRegions.Count - 1 do
    ConflictingObjects := ConflictingObjects +  EBoldGetLocksFailed(E).ConflictingRegions[i].Root.AsString + ' ';


  if E is EBoldGetLocksFailed then
    showmessage(Format('The change is not allowed because one or more objects are locked by other user(s).' + BOLDCRLF +
                       'Objects: %s' + BOLDCRLF +
                       'Users: %s' + BOLDCRLF +
                       'Occured in component %s, when trying to modify the element "%s".',
                       [ConflictingObjects,
                        EBoldGetLocksFailed(E).ClientIds.CommaText,
                        Component.Name,
                        Elem.BoldType.ExpressionName]))
  else
    showmessage('Unknown exception: ' + E.message);
  Discard := true;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowMessage('This example needs a valid license for ObjectSpace Synchronization Server and Concurrency Management Server extensions. If you miss the key request it in the License Manager found in the About box.');
  showmessage(GuidToString(WIN_TRUST_SUBJTYPE_RAW_FILE));
end;

end.


