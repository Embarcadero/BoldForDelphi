unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BoldPersistenceHandle, BoldPersistenceHandleDB,
  BoldOLLEHandles, BoldSubscription, BoldHandle,
  BoldDefs,
  BoldId,
  BoldBase,
  BoldValuespaceInterfaces,
  BoldXMLStreaming,
  BoldAbstractModel, BoldModel, ExtCtrls, BoldNavigatorDefs, BoldNavigator,
  Grids, BoldGrid, BoldHandles,
  BoldRootedHandles, BoldAbstractListHandle, BoldCursorHandle,
  BoldListHandle, BoldSystemHandle, BoldAbstractPersistenceHandleDB,
  BoldActions, BoldIBDatabaseAction,
  ActnList, BoldHandleAction, DB, IBDatabase, BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterIB;

type
  TForm1 = class(TForm)
    Button1: TButton;
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldSystemHandle2: TBoldSystemHandle;
    BoldListHandle1: TBoldListHandle;
    BoldListHandle2: TBoldListHandle;
    BoldGrid1: TBoldGrid;
    BoldGrid2: TBoldGrid;
    BoldNavigator1: TBoldNavigator;
    BoldNavigator2: TBoldNavigator;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button3: TButton;
    BoldOLLEHandle1: TBoldOLLEHandle;
    BoldOLLEHandle2: TBoldOLLEHandle;
    ActionList1: TActionList;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldActivateSystemAction2: TBoldActivateSystemAction;
    BoldUpdateDBAction2: TBoldUpdateDBAction;
    BoldIBDatabaseAction2: TBoldIBDatabaseAction;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    BoldPersistenceHandleDB2: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB2: TBoldDatabaseAdapterIB;
    IBDatabase2: TIBDatabase;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  BoldCondition,
  BoldFreestandingValues,
  BoldDefaultXMLStreaming,
  BoldMath,
  BoldGlobalId,
  MSXML_TLB;

procedure TForm1.Button1Click(Sender: TObject);
var
  aFSValuespace: TBoldFreeStandingValueSpace;
  aValueSpace: IBoldValueSpace;
  anIdList: TBoldObjectIdList;
  HoldList: TBoldObjectIdList;
begin
  if not BoldSystemHandle1.Active or not BoldSystemHandle2.Active then
    raise Exception.Create('Both systems must be open before a sync');
  anIdList := TBoldObjectIdList.Create;
  HoldList := TBoldObjectIdList.Create;
  aFSValueSpace := TBoldFreestandingValuespace.Create;
  aValuespace := aFSValuespace;

  BoldOLLEHandle1.OwnObjectHandler.GetSynch('db2', anIdList, aValueSpace);

  BoldOLLEHandle2.ForeignObjectHandler.Put(aValueSpace, anIdList, HoldList, 'db1');
  BoldOLLEHandle1.OwnObjectHandler.AcknowledgeSynch('db2');

  anIdlist.Free;
  HoldList.Free;
  aValueSpace := nil;
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  BoldOLLEHandle1.OLLEController.GenerateDatabase;
  BoldOLLEHandle2.OLLEController.GenerateDatabase;
end;


procedure TForm1.Button3Click(Sender: TObject);
var
  aFSValuespace: TBoldFreeStandingValueSpace;
  aValueSpace: IBoldValueSpace;
  anIdList: TBoldObjectIdList;
  HoldList: TBoldObjectIdList;
begin
  if not BoldSystemHandle1.Active or not BoldSystemHandle2.Active then
    raise Exception.Create('Both systems must be open before a sync');
  anIdList := TBoldObjectIdList.Create;
  HoldList := TBoldObjectIdList.Create;
  aFSValueSpace := TBoldFreestandingValuespace.Create;
  aValuespace := aFSValuespace;

  BoldOLLEHandle2.OwnObjectHandler.GetSynch('db1', anIdList, aValueSpace);

  BoldOLLEHandle1.ForeignObjectHandler.Put(aValueSpace, anIdList, HoldList, 'db2');
  BoldOLLEHandle2.OwnObjectHandler.AcknowledgeSynch('db1');

  anIdlist.Free;
  HoldList.Free;
  aValueSpace := nil;
end;

end.
