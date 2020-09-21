unit MainForm;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  Grids,
  ActnList,
  MainDM,
  BoldElements,
  BoldTreeView,
  BoldSubscription,
  BoldHandles,
  BoldRootedHandles,
  BoldExpressionHandle,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldVariableHandle,
  BoldDerivedHandle,
  BoldAFPPluggable,
  BoldVariableDefinition,
  BoldComboBox,
  BoldListBox,
  BoldNavigator,
  BoldEdit,
  BoldMemo,
  BoldGrid,
  BoldCaptionController,
  BoldHandleAction,
  BoldActions,
  BoldDBActions,
  BoldNavigatorDefs, BoldOclVariables, BoldIBDatabaseAction;

type
  TfrmMain = class(TForm)
    behCurrentUser: TBoldExpressionHandle;
    blhAllUsers: TBoldListHandle;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    BoldListBox1: TBoldListBox;
    blhAllProjects: TBoldListHandle;
    Label3: TLabel;
    blhDocuments: TBoldListHandle;
    BoldNavigator1: TBoldNavigator;
    BoldNavigator2: TBoldNavigator;
    blhDocParts: TBoldListHandle;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    btnNewUser: TButton;
    edtNewUserName: TEdit;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    blhDocs: TBoldListHandle;
    Button1: TButton;
    grdOldVersions: TBoldGrid;
    BoldGrid2: TBoldGrid;
    bvhDispOld: TBoldVariableHandle;
    BoldOclVariables1: TBoldOclVariables;
    behDisplayDoc: TBoldExpressionHandle;
    btnPublish: TButton;
    bdhDocVersions: TBoldDerivedHandle;
    bdhPartVersions: TBoldDerivedHandle;
    bvdPartVersions: TBoldOclVariables;
    blhDocVersions: TBoldListHandle;
    behAllHistoricalParts: TBoldExpressionHandle;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    Button3: TButton;
    Button2: TButton;
    Button4: TButton;
    grbADocument: TGroupBox;
    Label4: TLabel;
    BoldListBox3: TBoldListBox;
    BoldNavigator3: TBoldNavigator;
    BoldMemo1: TBoldMemo;
    Label6: TLabel;
    BoldCaptionController1: TBoldCaptionController;
    Label5: TLabel;
    Label8: TLabel;
    GroupBox4: TGroupBox;
    BoldComboBox1: TBoldComboBox;
    Label1: TLabel;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    Label7: TLabel;
    BoldEdit1: TBoldEdit;
    procedure btnNewUserClick(Sender: TObject);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnPublishClick(Sender: TObject);
    procedure bdhDocVersionsDeriveAndSubscribe(Sender: TComponent;
      RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
      Subscriber: TBoldSubscriber);
    procedure bdhPartVersionsDeriveAndSubscribe(Sender: TComponent;
      RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
      Subscriber: TBoldSubscriber);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses
  DocumentClasses,
  BoldId,
  BoldDefs,
  BoldSystem,
  BoldSystemRT,
  BoldAttributes,
  BoldCondition;

procedure TfrmMain.btnNewUserClick(Sender: TObject);
var
  aPerson: TPerson;
begin
  aPerson := TPerson.Create(dmMain.BoldSystemHandle1.System);
  aPerson.Name := edtNewUserName.Text;
end;

procedure TfrmMain.BoldActivateSystemAction1SystemOpened(Sender: TObject);
begin
  TContext.Create(dmMain.BoldSystemHandle1.System);
  if dmMain.BoldSystemHandle1.Active then
    MessageDlg('You must select a Current User. Use the "Create User" button to create a new user', mtWarning, [mbOk], 0);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  DisplayOld: TBABoolean;
begin
  DisplayOld := bvhDispOld.Value as TBABoolean;
  DisplayOld.AsBoolean := not DisplayOld.AsBoolean;
  blhDocVersions.Enabled := DisplayOld.AsBoolean;
  grdOldVersions.Enabled := DisplayOld.AsBoolean;
  btnPublish.Enabled := DisplayOld.AsBoolean;
  if DisplayOld.AsBoolean then
    Button1.Caption := 'View current'
  else
    Button1.Caption := 'View history';
end;

procedure TfrmMain.btnPublishClick(Sender: TObject);
var
  doc: TDocument;
  i: integer;
  NewVersion: TVersion;
begin
  doc := (blhDocVersions.CurrentBoldObject as TDocument);
  for i := 0 to doc.version.Count-1 do
    if doc.version[i].Time = doc.BoldTime then
      exit;

  NewVersion := doc.version.AddNew as TVersion;
  NewVersion.Time := doc.BoldTime;
  NewVersion.IsPublished := True;
end;

procedure TfrmMain.bdhDocVersionsDeriveAndSubscribe(Sender: TComponent;
  RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
  Subscriber: TBoldSubscriber);
var
  aCond: TBoldChangePointCondition;
  DocumentListType: TBoldListTypeInfo;
begin
  if not assigned(RootValue) or (not dmMain.BoldSystemHandle1.Active) then
    exit;

  aCond := TBoldChangePointCondition.Create;
  aCond.IdList := TBoldObjectIdList.Create;
  aCond.IdList.Add((RootValue as TBoldObject).BoldObjectLocator.BoldObjectID);
  aCond.MemberIdList := TBoldMemberIdList.Create;
  aCond.MemberIdList.Add(TBoldMemberId.Create((RootValue as TDocument).documentPart.BoldMemberRTInfo.index));
  aCond.StartTime := 0;
  aCond.EndTime := BOLDMAXTIMESTAMP;

  DocumentListType := dmMain.BoldSystemTypeInfoHandle1.StaticSystemTypeInfo.ListTypeInfoByElement[RootValue.BoldType];
  if not assigned(ResultElement.Value) then
    ResultElement.SetOwnedValue(TBoldMemberFactory.CreateMemberFromBoldType(DocumentListType))
  else
    (ResultElement.Value as TBoldObjectList).Clear;
  dmMain.BoldSystemHandle1.system.GetAllWithCondition(ResultElement.Value as TBoldObjectList, aCond);

  aCond.IdList.Free;
  aCond.MemberIdList.Free;
  aCond.Free;
end;

procedure TfrmMain.bdhPartVersionsDeriveAndSubscribe(Sender: TComponent;
  RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
  Subscriber: TBoldSubscriber);
var
  aCond: TBoldChangePointCondition;
begin
  if (not assigned(RootValue)) or (not dmMain.BoldSystemHandle1.Active) then
    exit;

  aCond := TBoldChangePointCondition.Create;
  aCond.IdList := (RootValue as TBoldObjectList).CreateObjectIdList;
  aCond.StartTime := 0;
  aCond.EndTime := BOLDMAXTIMESTAMP;

  if not assigned(ResultElement.Value) then
    ResultElement.SetOwnedValue(TBoldMemberFactory.CreateMemberFromBoldType(RootValue.BoldType))
  else
    (ResultElement.Value as TBoldObjectList).Clear;
  dmMain.BoldSystemHandle1.system.GetAllWithCondition(ResultElement.Value as TBoldObjectList, aCond);

  RootValue.DefaultSubscribe(Subscriber);

  aCond.IdList.Free;
  aCond.Free;
end;   

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if dmMain.BoldSystemHandle1.Active and dmMain.BoldSystemHandle1.System.BoldDirty then
    case MessageDlg( 'There are dirty objects. Save them before exit?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: dmMain.BoldSystemHandle1.System.UpdateDatabase;
      mrNo: dmMain.BoldSystemHandle1.System.Discard;
      mrCancel: CanClose := False;
    end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ShowMessage('This example needs a deployment key for Object Versioning Extension. If you miss the key request it in the License Manager found in the About box.');
end;

end.
