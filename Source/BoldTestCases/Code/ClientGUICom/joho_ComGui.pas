
unit joho_ComGui;

interface

uses
  Forms,
  Dialogs,
  Windows,
  BoldEdit,
  BoldListBox,
  BoldDefs,
  BoldEditCom,
  BoldListBoxCom,
  BoldNavigatorDefs,
  TestSuite,
  BoldQueue,
  joho_ClientGuiAndHandles,
  BoldLogHandler,
  BoldComObjectSpace_TLB,
  BoldLogHandlerForm,
  TestModel1,
  dmModel1,
  BoldNavigatorCOM,
  BoldNumericControlPackCOM,
  BoldStringControlPackCOM,
  TestFrameWork;

type
  Tjoho_comGui = class(TBoldTestCase)
  private
    procedure EnsureGui;
    procedure CheckAllEditsAndLists;
    procedure OpenConnection;
    procedure CloseConnection;
    procedure RefreshGui;
  public
    class procedure Suit(ASuite: TBoldTestSuite); override;
    class function Suite: ITestSuite; override;
    destructor destroy; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnection;
    procedure TestNavigator;
    procedure ComGui;
    procedure TestGrid;
    procedure TestTrackBar;
    procedure TestProgressbar;
    procedure TestIntegerRenderer;
    procedure TestCOMEdit;
    procedure TestChangePropagation;
    procedure TestObjectCreation;
    procedure TestDynamicHandles;
  end;       

implementation

uses
  SysUtils,
  BoldUtils;

{ Tjoho_comGui }

procedure CheckEditBox(Edit: TBoldEdit; EditCom: TBoldEditCom);
begin
  assert(Edit.Text = editCom.Text, format( 'Edits does not show the same value, %s=[%s] <-> %s=[%s]', [Edit.name, Edit.Text, editCom.Name, editCom.Text]));
end;

procedure CheckListBox( ListBox: TBoldListBox; listBoxCom: TBoldListBoxCom);
var
  i: integer;
begin
  assert(ListBox.BoldList.Count = ListBoxCom.BoldList.Count, 'Not the same amount of elements');
  for i := 0 to ListBox.Boldlist.Count-1 do
    assert(ListBox.Items[i] = ListBoxCom.Items[i], 'Not the same text on row ' + intToStr(i))
end;

procedure Tjoho_comGui.CheckAllEditsAndLists;
begin
  // 2.4.1.1
  assert(gui.BoldEdit1.Text = gui.BoldLabelCom2.Caption, 'Caption does not show the same value');

  // 2.3.2.5, 2.4.1.2
  CheckEditBox(gui.BoldEdit1, gui.BoldEditCom1);
  CheckEditBox(gui.BoldEdit1, gui.BoldEditCom3);
  CheckEditBox(gui.BoldEdit2, gui.BoldEditCom2);
  CheckEditBox(gui.BoldEdit2, gui.BoldEditCom4);

  // 2.3.2.5, 2.4.1.3
  CheckListBox(gui.BoldListBox2, gui.BoldListBoxCom1);
  CheckListBox(gui.BoldLIstBox3, gui.BoldListBoxCom2);
  CheckListBox(gui.BoldListBox2, gui.BoldListBoxCom3);
  CheckListBox(gui.BoldLIstBox3, gui.BoldListBoxCom4);

  // 2.4.1.6
  Assert(gui.Edit1.Text = gui.BoldEdit1.Text, 'Caption controller failed');

  // 2.4.1.7
  Assert(gui.BoldCheckBoxCom1.Checked = (gui.BoldEdit1.Text = 'Bad Touch'), 'Checkbox not correct');

  // 2.4.1.11
  Assert(GUI.BoldMemoCom1.Text = gui.BoldEdit1.Text, 'Memo failed');

  // 2.4.2.1
  Assert(GUI.bceRenderedString.text = gui.BoldEdit1.Text + gui.BoldEdit1.Text, 'String rendering failed');

  // 2.4.2.2
  Assert(GUI.bccRenderedCheckbox.Checked = (gui.BoldEdit2.Text = 'Bad Touch'), 'Checkbox rendering failed');
end;

procedure Tjoho_comGui.CloseConnection;
begin
  RefreshGui;
  // connect the Client
  Gui.BoldComConnectionHandle1.Connected := False;
  RefreshGui;
end;

procedure Tjoho_comGui.ComGui;
begin
  OpenConnection; // 2.3.2.1
  try
    // Check that the GuiComponents Show the same thing as the native ones
    RefreshGui;

    CheckAllEditsAndLists;  //2.3.2.4, 2.4.1.6, 2.4.1.7, 2.4.1.11, 2.4.2.1, 2.4.2.2

    // change the current index in a server handle
    //FIXME: What does this test?
    Gui.blhLongHitLists.Next;
    RefreshGui;
    Gui.blhSongs.Next;
  finally
    CloseConnection;
  end;
end;

destructor Tjoho_comGui.destroy;
begin
  inherited;
end;

procedure Tjoho_comGui.EnsureGui;
begin
  Ensuredm_Model;
end;

procedure Tjoho_comGui.OpenConnection;
begin
  // 2.3.2.1
  RefreshGui;
  // connect the Client
  Gui.BoldComConnectionHandle1.Connected := true;
  RefreshGui;
end;

procedure Tjoho_comGui.RefreshGui;
begin
  TBoldQueueable.DisplayAll;
  Application.ProcessMessages;
  TBoldQueueable.DisplayAll;
  Application.ProcessMessages;
end;

procedure Tjoho_comGui.SetUp;
begin
  EnsureGui;
end;

class procedure Tjoho_comGui.Suit(ASuite: TBoldTestSuite);
begin
  ASuite.AddTest(CreateWithComment('TestConnection', '2.3.1, 2.3.2.1'));
  ASuite.AddTest(CreateWithComment('TestCOMEdit', '2.4.1.2'));
  ASuite.AddTest(CreateWithComment('TestNavigator', '2.4.1.5'));
  ASuite.AddTest(CreateWithComment('TestGrid', '2.4.1.8'));
  ASuite.AddTest(CreateWithComment('TestProgressbar', '2.4.1.9'));
  ASuite.AddTest(CreateWithComment('TestTrackbar', '2.4.1.10'));
  ASuite.AddTest(CreateWithComment('TestIntegerRenderer', '2.4.2.3'));
  ASuite.AddTest(CreateWithComment('TestChangePropagation', ''));
  ASuite.AddTest(CreateWithComment('TestObjectCreation', '2.3.2.5'));
  ASuite.AddTest(CreateWithComment('TestDynamicHandles', '2.3.2.10'));
  ASuite.AddTest(CreateWithComment('ComGui', '2.3.2.4, 2.4.1.6, 2.4.1.7, 2.4.1.11, 2.4.2.1, 2.4.2.2'));
end;

class function Tjoho_comGui.Suite: ITestSuite;
begin
  Result := inherited Suite;
  SetCommentForTest(Result, 'TestConnection', '2.3.1, 2.3.2.1');
  SetCommentForTest(Result,'TestCOMEdit', '2.4.1.2');
  SetCommentForTest(Result,'TestNavigator', '2.4.1.5');
  SetCommentForTest(Result,'TestGrid', '2.4.1.8');
  SetCommentForTest(Result,'TestProgressbar', '2.4.1.9');
  SetCommentForTest(Result,'TestTrackbar', '2.4.1.10');
  SetCommentForTest(Result,'TestIntegerRenderer', '2.4.2.3');
  SetCommentForTest(Result,'TestChangePropagation', '');
  SetCommentForTest(Result,'TestObjectCreation', '2.3.2.5');
  SetCommentForTest(Result,'TestDynamicHandles', '2.3.2.10');
  SetCommentForTest(Result,'ComGui', '2.3.2.4, 2.4.1.6, 2.4.1.7, 2.4.1.11, 2.4.2.1, 2.4.2.2');
end;

procedure Tjoho_comGui.TearDown;
begin
  RefreshGui;
  FreeGUI;
  inherited;
end;

procedure Tjoho_comGui.TestChangePropagation;
begin
  OpenConnection;
  try
    assert(Gui.BoldEdit1.Text = 'Sweden HitList 5', 'Testcase precondition incorrect');

    // Change the value in an object
    GUI.SwedenTop5.Name := 'Sweden HitList 6';
    GUI.SwedenTop5.song.Add( TSong.create(Gui.BoldSystemHandle1.System ));
    RefreshGui;

    assert(Gui.BoldEdit1.Text = 'Sweden HitList 6');
  finally
    CloseConnection;
  end;
end;


procedure Tjoho_comGui.TestCOMEdit;
begin
  OpenConnection;
  try
    // Change the value in a COM-edit (Component has ApplyPolicy Change):
    RefreshGui;
    assert(GUI.SwedenTop5.Name = Gui.BoldEditCom1.text);
    Gui.BoldEditCom1.text := 'Svensktoppen';
    RefreshGui;
    assert(GUI.SwedenTop5.Name = 'Svensktoppen');
  finally
    CloseConnection;
  end;
end;

procedure Tjoho_comGui.TestConnection;
var
  ISystem: IBoldSystem;
begin
  OpenConnection;
  assert( gui.BoldComConnectionHandle1.Connected, 'Connection not open' );
  assert(Assigned(gui.BoldSystemHandleCom1.value), 'SystemHandle has no value');
  assert( gui.BoldSystemHandleCom1.value.QueryInterface(IBoldSystem, ISystem) = S_OK, 'SystemHandle defect (DLLs not registered?)' );
  CloseConnection;
  assert(not gui.BoldComConnectionHandle1.Connected, 'Connection still open' );
  assert(not Assigned(gui.BoldSystemHandleCom1.value), 'SystemHandle has a value');
end;

procedure Tjoho_comGui.TestDynamicHandles;
begin
  OpenConnection;
  try
    assert(gui.bedcDynamicVariableHandle.Text = 'test', 'DynamicVariableHandle');
    assert(gui.bedcStaticVariableHandle.Text = 'test', 'StaticVariableHandle');
    // 2.3.2.10

    // Check only those without own handles on the server
    CheckEditBox( gui.BoldEdit1, gui.BoldEditCom1);
    CheckEditBox( gui.BoldEdit2, gui.BoldEditCom2);

    CheckListBox( gui.BoldListBox2, gui.BoldListBoxCom1 );
    CheckListBox( gui.BoldLIstBox3, gui.BoldListBoxCom2 );
    assert( gui.blhLongHitLists.CurrentIndex = gui.bchcLongHitLists.CurrentIndex, 'Index of Handles out of sync' );


    gui.blhLongHitLists.CurrentIndex := 0;
    Gui.ClientOwnedSongs.CurrentIndex := 0;

    RefreshGui;

    Gui.blhLongHitLists.Next;
    Gui.blhSongs.Next;
    // the handles with their own handle on server should not be affected.

    RefreshGui;
    assert( Gui.ClientOwnedHitLists.CurrentIndex = 0, 'index of owned handles were incorrectly synced' );
    assert( Gui.ClientOwnedSongs.CurrentIndex = 0, 'index of owned handles were incorrectly synced' );
  finally
    CloseConnection;
  end;
end;

procedure Tjoho_comGui.TestGrid;
const
  teststringvalue = 'fridayniteisere';
  testintegervalue = '5';
begin
  OpenConnection;
  // 2.4.1.8
  Gui.blhcAllClassA.CurrentBoldObject.EvaluateExpression('aString').AsString := teststringvalue;
  Gui.blhcAllClassA.CurrentBoldObject.EvaluateExpression('aInteger').AsString := testintegervalue;
  RefreshGui;
  Assert((GUI.BoldGridCom1.CellFollowers[1, 0].RendererData as TBoldStringRendererDataCom).OldStringValue = teststringvalue, 'Grid failed (1)');
  Assert((GUI.BoldGridCom1.CellFollowers[2, 0].RendererData as TBoldStringRendererDataCom).OldStringValue = testintegervalue, 'Grid failed (2)');
  CloseConnection;
end;

procedure Tjoho_comGui.TestIntegerRenderer;
begin
  OpenConnection;
  try
    // 2.4.2.3
    // Progress bar shows the aInteger-attribute, but renders it * 2
    Gui.blhcAllClassA.CurrentBoldObject.Set_BoldMemberValue('aInteger', 9);
    RefreshGui;
    Assert(gui.cpbRenderedInteger.Position = 9 * 2, 'Integer rendering failed');
  finally
    CloseConnection;
  end;
end;

procedure Tjoho_comGui.TestNavigator;
begin
  OpenConnection;
  // 2.4.1.5
  Gui.BoldNavigatorCom1.BtnClick(nbFirst);
  RefreshGui;
  Assert(not Gui.BoldNavigatorCom1.BoldHandle.HasPrior and Gui.BoldNavigatorCom1.BoldHandle.HasNext, 'Navigator did not go to first');
  Gui.BoldNavigatorCom1.BtnClick(nbLast);
  RefreshGui;
  Assert(Gui.BoldNavigatorCom1.BoldHandle.HasPrior and not Gui.BoldNavigatorCom1.BoldHandle.HasNext, 'Navigator did not go to last');
  Gui.BoldNavigatorCom1.BtnClick(nbPrior);
  RefreshGui;
  Assert(Gui.BoldNavigatorCom1.BoldHandle.HasNext, 'Navigator did not go back');
  CloseConnection;
end;

procedure Tjoho_comGui.TestObjectCreation;
var
  OldCount: integer;
begin
  OpenConnection;
  try
    // 2.3.2.5
    // Create objects on client
    OldCount := Gui.blhcAllClassA.count;
    Gui.BoldSystemHandleCom1.System.CreateNewObject( 'ClassA', true );
    RefreshGui;
    assert( Gui.blhcAllClassA.count = OldCount +1, 'ObjectCreation does not get through' );
  finally
    CloseConnection;
  end;
end;

procedure Tjoho_comGui.TestProgressbar;
var
  BoldObject: IBoldObject;
begin
  OpenConnection;
  try
    // 2.4.1.9
    // test Progress bar, listhandle
    Gui.BoldProgressbarCom1.Position := 7;
    RefreshGui;
    assert( Gui.blhcAllClassA.CurrentBoldObject.EvaluateExpressionAsString( 'aInteger', brDefault ) = '7', 'Progressbar Error' );

    BoldObject := Gui.blhcAllClassA.CurrentBoldObject;
    BoldObject.Set_BoldMemberValue('aInteger', 8);
    RefreshGui;
    Assert(Gui.BoldProgressbarCom1.Position = 8, 'Progressbar read error');
  finally
    CloseConnection;
  end;
end;

procedure Tjoho_comGui.TestTrackBar;
var
  BoldObject: IBoldObject;
begin
  OpenConnection;
  try
    // 2.4.1.10
    // test Trackbar, listhandle
    Gui.BoldTrackBarCom1.Position := 7;
    RefreshGui;
    assert( Gui.blhcAllClassA.CurrentBoldObject.EvaluateExpressionAsString( 'aInteger', brDefault ) = '7', 'TrackBar Error' );

    BoldObject := Gui.blhcAllClassA.CurrentBoldObject;
    BoldObject.Set_BoldMemberValue('aInteger', 8);
    RefreshGui;
    Assert(Gui.BoldTrackBarCom1.Position = 8, 'Trackbar read error');
  finally
    CloseConnection;
  end;
end;

initialization
  TestGlobal.RegisterTestCase(Tjoho_comGui);
  
end.
