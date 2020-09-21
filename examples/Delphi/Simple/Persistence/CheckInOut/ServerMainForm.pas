unit ServerMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, MainDM, BoldOLLEHandles, StdCtrls,
  BoldHandle, BoldPersistenceHandle, BoldPersistenceHandleDB,
  BoldSubscription, BoldHandles, BoldSystemHandle,
  BoldListBox, ExtCtrls, BoldNavigatorDefs, BoldNavigator,
  BoldRootedHandles, BoldAbstractListHandle, BoldCursorHandle,
  BoldListHandle, Grids, BoldGrid, BoldDefaultXMLStreaming,
  BoldVariableHandle, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB,
  BoldIBDatabaseAction, ActnList, BoldHandleAction, BoldActions;

type
  TForm1 = class(TForm)
    BoldSystemHandle1: TBoldSystemHandle;
    Button1: TButton;
    BoldOLLEHandle1: TBoldOLLEHandle;
    BoldGrid1: TBoldGrid;
    BoldListHandle1: TBoldListHandle;
    BoldNavigator1: TBoldNavigator;
    BoldListBox1: TBoldListBox;
    BoldListBox2: TBoldListBox;
    BoldListHandle2: TBoldListHandle;
    BoldListHandle3: TBoldListHandle;
    Label1: TLabel;
    Label2: TLabel;
    BoldListHandle4: TBoldListHandle;
    BoldListBox3: TBoldListBox;
    BoldVariableHandle1: TBoldVariableHandle;
    BoldCursorHandle1: TBoldCursorHandle;
    BoldListBox4: TBoldListBox;
    Button2: TButton;
    Button3: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    fStreamManager: TBoldDefaultXMLStreamManager;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  BoldId,
  BoldDefs,
  BoldValueSpaceInterfaces,
  BoldFreestandingValues,
  BoldXMLStreaming,
  BoldDefaultStreamNames,
  MSXML_TLB;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  BoldOLLEHandle1.OLLEController.GenerateDatabase;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  CheckOutList: TBoldObjectIdList;
  aValueSpace: TBoldFreestandingValuespace;
  aDOMDoc: TDOMDocument;
  RootNode: TBoldXMLNode;
  VSNode: TBoldXMLNode;
  aStringList: TStringList;
begin
  CheckOutList := BoldCursorHandle1.ObjectList.CreateObjectIdList;
  aValueSpace := TBoldFreeStandingValueSpace.Create;
  aDOMDoc := TDOMDocument.Create(nil);
  aStringList := TStringList.Create;
  RootNode := nil;
  VSNode := nil;
  try
    BoldOLLEHandle1.OwnObjectHandler.Get(CheckOutList, CheckOutList, 'TheClient', aValueSpace);

    RootNode := fStreamManager.NewRootNode(aDOMDoc, 'TheStuff');
    VSNode := RootNode.NewSubNode('ValueSpace');
    fStreamManager.WriteValueSpace(aValueSpace, CheckOutList, nil, VSNode);
    RootNode.WriteSubNodeObject('IdList', BOLDOBJECTIDLISTNAME, CheckOutList);

    aStringList.Text := aDOMDoc.documentElement.xml;
    aStringList.SaveToFile('CheckOutPackage.xml');
  finally
    CheckOutList.Free;
    aValueSpace.Free;
    aDOMDoc.Free;
    aStringList.Free;
    RootNode.Free;
    VSNode.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fStreamManager := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
                                                        DataModule1.BoldModel1.MoldModel);
  fStreamManager.IgnorePersistenceState := False;
  fStreamManager.PersistenceStatesToOverwrite := [bvpsInvalid, bvpsModified, bvpsCurrent];
  fStreamManager.PersistenceStatesToBeStreamed := [bvpsInvalid, bvpsModified, bvpsCurrent];
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  aStringList: TStringList;
  aDOMDoc: TDOMDocument;
  RootNode: TBoldXMLNode;
  VSNode: TBoldXMLNode;
  CheckInList: TBoldObjectIdList;
  aValueSpace: TBoldFreestandingValuespace;
  NewTimestamp: Integer;
begin
  aStringList := TStringList.Create;
  aDOMDoc := TDOMDocument.Create(nil);
  aValuespace := TBoldFreeStandingValueSpace.Create;
  CheckInList := nil;
  RootNode := nil;
  VSNode := nil;
  try
    aStringList.LoadFromFile('CheckInPackage.xml');
    aDOMDoc.loadXML(aStringList.Text);

    RootNode := fStreamManager.GetRootNode(aDOMDoc, 'TheStuff');
    VSNode := RootNode.GetSubNode('ValueSpace');
    fStreamManager.ReadValueSpace(aValueSpace, VSNode);
    CheckInList := RootNode.ReadSubNodeObject('IdList', BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;
    BoldOLLEHandle1.OwnObjectHandler.CheckIn(aValueSpace, CheckInList, CheckInList, 'TheClient', NewTimestamp);

    aDOMDoc.Free;
    aDOMDoc := TDOMDocument.Create(nil);
    RootNode := fStreamManager.NewRootNode(aDOMDoc, 'TheStuff');
    RootNode.WriteSubNodeInteger('Timestamp', NewTimestamp);

    aStringList.Clear;
    aStringList.Text := aDOMDoc.documentElement.xml;
    aStringList.SaveToFile('CheckInReceipt.xml');
  finally
    aStringList.Free;
    aDOMDoc.Free;
    aValuespace.Free;
    CheckInList.Free;
    RootNode.Free;
    VSNode.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fStreamManager);
end;

end.
