unit ClientMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, MainDM, BoldHandles, BoldRootedHandles, BoldAbstractListHandle,
  BoldCursorHandle, BoldListHandle, BoldOLLEHandles, BoldHandle,
  BoldPersistenceHandle, BoldPersistenceHandleDB, 
  BoldSubscription, BoldSystemHandle, StdCtrls, BoldListBox,
  ExtCtrls, BoldNavigatorDefs, BoldNavigator, Grids, BoldGrid, BoldDefaultXMLStreaming,
  BoldVariableHandle, BoldElements, BoldControlPack,
  BoldStringControlPack, BoldIBDatabaseAction, ActnList, BoldHandleAction,
  BoldActions, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB, BoldAbstractPersistenceHandleDB;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    BoldGrid1: TBoldGrid;
    BoldNavigator1: TBoldNavigator;
    BoldListBox1: TBoldListBox;
    BoldListBox2: TBoldListBox;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldOLLEHandle1: TBoldOLLEHandle;
    BoldListHandle1: TBoldListHandle;
    BoldListHandle2: TBoldListHandle;
    BoldListHandle3: TBoldListHandle;
    Button2: TButton;
    BoldListBox3: TBoldListBox;
    BoldListBox4: TBoldListBox;
    BoldListHandle4: TBoldListHandle;
    BoldVariableHandle1: TBoldVariableHandle;
    BoldCursorHandle1: TBoldCursorHandle;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    BoldAsStringRenderer1: TBoldAsStringRenderer;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    function BoldAsStringRenderer1GetAsString(Element: TBoldElement;
      Representation: Integer; Expression: String): String;
    procedure BoldAsStringRenderer1Subscribe(Element: TBoldElement;
      Representation: Integer; Expression: String;
      Subscriber: TBoldSubscriber);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    fStreamManager: TBoldDefaultXMLStreamManager;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  BoldId,
  BoldDefs,
  BoldValueSpaceInterfaces,
  BoldFreestandingValues,
  BoldXMLStreaming,
  BoldDefaultStreamNames,
  BoldSystem,
  MSXML_TLB;

procedure TForm2.Button2Click(Sender: TObject);
var
  aStringList: TStringList;
  aDOMDoc: TDOMDocument;
  RootNode: TBoldXMLNode;
  VSNode: TBoldXMLNode;
  CheckOutList: TBoldObjectIdList;
  aValueSpace: TBoldFreestandingValuespace;
begin
  aStringList := TStringList.Create;
  aDOMDoc := TDOMDocument.Create(nil);
  aValuespace := TBoldFreeStandingValueSpace.Create;
  VSNode := nil;
  CheckOutList := nil;
  RootNode := nil;
  try
    aStringList.LoadFromFile('CheckOutPackage.xml');
    aDOMDoc.loadXML(aStringList.Text);

    RootNode := fStreamManager.GetRootNode(aDOMDoc, 'TheStuff');
    VSNode := RootNode.GetSubNode('ValueSpace');
    fStreamManager.ReadValueSpace(aValueSpace, VSNode);
    CheckOutList := RootNode.ReadSubNodeObject('IdList', BOLDOBJECTIDLISTNAME) as TBoldObjectIdList;

    BoldOLLEHandle1.ForeignObjectHandler.Put(aValueSpace, CheckOutList, CheckOutList, 'TheServer');
  finally
    aStringList.Free;
    aDOMDoc.Free;
    aValuespace.Free;
    VSNode.Free;
    CheckOutList.Free;
    RootNode.Free;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  fStreamManager := TBoldDefaultXMLStreamManager.Create(TBoldDefaultXMLStreamerRegistry.MainStreamerRegistry,
                                                        DataModule1.BoldModel1.MoldModel);
  fStreamManager.IgnorePersistenceState := True;
  fStreamManager.PersistenceStatesToOverwrite := [bvpsInvalid, bvpsModified, bvpsCurrent];
  fStreamManager.PersistenceStatesToBeStreamed := [bvpsInvalid, bvpsModified, bvpsCurrent];
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  BoldOLLEHandle1.OLLEController.GenerateDatabase;
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  CheckInList: TBoldObjectIdList;
  aValueSpace: TBoldFreestandingValuespace;
  aDOMDoc: TDOMDocument;
  RootNode: TBoldXMLNode;
  VSNode: TBoldXMLNode;
  aStringList: TStringList;
begin
  aValuespace := TBoldFreeStandingValueSpace.Create;
  CheckInList := BoldCursorHandle1.ObjectList.CreateObjectIdList;
  aDOMDoc := TDOMDocument.Create(nil);
  aStringList := TStringList.Create;
  RootNode := nil;
  VSNode := nil;
  try
    BoldOLLEHandle1.ForeignObjectHandler.StartCheckIn(CheckInList, CheckInList, aValueSpace, 'TheServer');

    RootNode := fStreamManager.NewRootNode(aDOMDoc, 'TheStuff');
    VSNode := RootNode.NewSubNode('ValueSpace');
    fStreamManager.WriteValueSpace(aValueSpace, CheckInList, nil, VSNode);
    RootNode.WriteSubNodeObject('IdList', BOLDOBJECTIDLISTNAME, CheckInList);

    aStringList.Text := aDOMDoc.documentElement.xml;
    aStringList.SaveToFile('CheckInPackage.xml');
  finally
    aValuespace.Free;
    CheckInList.Free;
    aDOMDoc.Free;
    aStringList.Free;
    RootNode.Free;
    VSNode.Free;
  end;
end;

procedure TForm2.Button4Click(Sender: TObject);
var
  aDOMDoc: TDOMDocument;
  RootNode: TBoldXMLNode;
  aStringList: TStringList;
  NewTimestamp: Integer;
begin
  aStringList := TStringList.Create;
  aDOMDoc := TDOMDocument.Create(nil);
  RootNode := nil;
  try
    aStringList.LoadFromFile('CheckInReceipt.xml');
    aDOMDoc.loadXML(aStringList.Text);
    RootNode := fStreamManager.GetRootNode(aDOMDoc, 'TheStuff');
    NewTimestamp := RootNode.ReadSubNodeInteger('Timestamp');
    BoldOLLEHandle1.ForeignObjectHandler.AcknowledgeCheckIn('TheServer', NewTimestamp);
  finally
    aStringList.Free;
    aDOMDoc.Free;
    RootNode.Free;
  end;
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  BoldOLLEHandle1.ForeignObjectHandler.FailCheckIn('TheServer');
end;

procedure TForm2.Button6Click(Sender: TObject);
var
  anIdList: TBoldObjectIdList;
begin
  anIdList := TBoldObjectIdList.Create;
  try
    BoldOLLEHandle1.ForeignObjectHandler.HeldObjectsFrom('TheServer', anIdList);
    BoldCursorHandle1.ObjectList.SubscribeToObjectsInList := False;
    BoldCursorHandle1.ObjectList.Clear;
    BoldCursorHandle1.ObjectList.FillFromIDList(anIdList, BoldSystemHandle1.System);
  finally
    anIdList.Free;
  end;    
end;

procedure TForm2.Button7Click(Sender: TObject);
var
  anIdList: TBoldObjectIdList;
begin
  anIdList := TBoldObjectIdList.Create;
  try
    BoldOLLEHandle1.ForeignObjectHandler.ModifiedObjectsFrom('TheServer', anIdList);
    BoldCursorHandle1.ObjectList.SubscribeToObjectsInList := False;
    BoldCursorHandle1.ObjectList.Clear;
    BoldCursorHandle1.ObjectList.FillFromIDList(anIdList, BoldSystemHandle1.System);
  finally
    anIdList.Free;
  end;
end;

function TForm2.BoldAsStringRenderer1GetAsString(Element: TBoldElement;
  Representation: Integer; Expression: String): String;
begin
  Result := '';
  if Assigned(Element) then
  begin
    if (Element as TBoldObject).BoldObjectIsDeleted then
      result := '<Deleted ' + (Element as TBoldObject).BoldClassTypeInfo.ExpressionName + '>'
    else
      result := Element.StringRepresentation[Representation];      
  end;
end;

procedure TForm2.BoldAsStringRenderer1Subscribe(Element: TBoldElement;
  Representation: Integer; Expression: String;
  Subscriber: TBoldSubscriber);
begin
  Element.SubscribeToStringRepresentation(Representation, Subscriber);
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fStreamManager);
end;

end.
