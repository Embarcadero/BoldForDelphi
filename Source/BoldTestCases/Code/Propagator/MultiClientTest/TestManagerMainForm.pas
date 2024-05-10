unit TestManagerMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  StdCtrls,
  TestManager,
  BoldSystem,
  BoldListHandle,
  BoldPropagatorConstants,
  syncobjs,
  BoldPersistenceController,
  ExtCtrls, Mask, ComCtrls, BoldSubscription, BoldHandle,
  BoldPersistenceHandle, BoldPersistenceHandlePassthrough,
  BoldDefs,
  BoldIDAdderHandle;

type
  TPropagatorEventtype = record
    Event: string;
    ClientID: TBoldClientID;
  end;

  TestType = (ttLoadTest, ttEventTest, ttThroughPutTest, ttNone);
  TMainForm = class(TForm)
    Timer1: TTimer;
    Panel1: TPanel;
    Label1: TLabel;
    edNumber: TMaskEdit;
    LoadTestTimer: TTimer;
    UpDown1: TUpDown;
    LogMemo: TMemo;
    Label2: TLabel;
    btnTest: TButton;
    rgTest: TRadioGroup;
    ednoclients: TEdit;
    Label3: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure LoadTestTimerTimer(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnTestClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    fNOClients: integer;
    fTestManager: TTestManager;
    fClients: TInterfaceList;
    fDBAlias: string;
    ClassAID, HitListID, SongID: WideString;
    FTestType: TestType;
    StartTime: TTimeStamp;
    BeginTime: TTimeStamp;
    NumberOfEventsReceived: integer;
    NumberOfEventsGenerated: integer;
    fTesting: Boolean;
    EventsReceived: TList;
    EventsSent, Messages: TStringList;
    procedure ReceiveEvent(const Event: string; ClientID: integer);
    procedure CreateObject;
    procedure UpdateEmbeddedState;
    procedure UpdateNonEmbeddedState;
    procedure DeleteObject;
    procedure LoadTest;
    procedure ThroughPutTest;
    procedure AutoTest(const Start: Boolean);
    procedure DisplayResults;
    procedure WriteToLog(const StrFormat: string; const Args: array of const);
    procedure SetTesting(Value: Boolean);
  public
    { Public declarations }
    procedure EventTest;
    procedure InitializeDM;
    procedure InitializeClients;
    property NOClients: integer read fNOClients write fNOClients;
    property TestManager: TTestManager read fTestManager write fTestManager;
    property Clients: TInterfaceList read fClients write fClients;
    property DBAlias: string read fDBAlias write fDBAlias;
    property Testing: Boolean read fTesting write SetTesting;
  end;

var
  MainForm: TMainForm;

const
  NoEventsGenerated = 1000;

implementation

uses
  TestClient_TLB,
  ActiveX,
  comobj,
  dmMultiClient,
  maanDataGen,
  TestModel1;

{$R *.DFM}

function BooleanToStr(Value: Boolean): string;
begin
  if Value then
    Result := 'OK'
  else
    Result := 'Failure';
end;

procedure TMainForm.EventTest;
begin
  FTestType := ttEventTest;
  CreateObject;
  UpdateEmbeddedState;
  UpdateNonEmbeddedState;
  DeleteObject;
end;

procedure TMainForm.CreateObject;
var
  aEvent: ^TPropagatorEventtype;
  EventSent : string;
  ClientId: integer;
begin
  FTestType := ttEventTest;
  // create object
  Sleep(TIMEOUT*3);

  EventSent := 'C:ClassA';
  ClientId := 1;
  (Clients[ClientID - 1] as IPropagatorTestClient).CreateObject(WideString('ClassA'),ClassAID) ; //send
  Sleep(TimeOut*2);
  Application.ProcessMessages;
  try
    aEvent := EventsReceived[0];
    WriteToLog('Create object: %s     %s', ['ClassA', BooleanToStr((aEvent^.Event = EventSent)
                  and (aEvent^.ClientID <> ClientID))]);
    EventsReceived.Clear;
    Dispose(aEvent);
  except
    WriteToLog('Create object: %s     %s', ['ClassA', BooleanToStr((false))]);
  end;

  EventSent := 'C:Song';
  (Clients[ClientID - 1] as IPropagatorTestClient).CreateObject(WideString('Song'),SongID) ; //send
  Sleep(TimeOut*2);
  Application.ProcessMessages;
  try
    aEvent := EventsReceived[0];
    WriteToLog('Create object: %s     %s', ['Song', BooleanToStr((aEvent^.Event = EventSent)
                  and (aEvent^.ClientID <> ClientId))]);
    EventsReceived.Clear;
    Dispose(aEvent);
  except
    WriteToLog('Create object: %s     %s', ['C:Song', BooleanToStr((false))]);
  end;

  EventSent := 'C:HitList';
  (Clients[ClientID - 1] as IPropagatorTestClient).CreateObject(WideString('HitList'),HitListID) ; //send
  Sleep(TimeOut*2);
  Application.ProcessMessages;
  try
    aEvent := EventsReceived[0];
    WriteToLog('Create object: %s     %s', ['HitList', BooleanToStr((aEvent^.Event = EventSent)
                  and (aEvent^.ClientID <> ClientId))]);
    EventsReceived.Clear;
    Dispose(aEvent);
  except
    WriteToLog('Create object: %s     %s', ['HitList', BooleanToStr(false)]);
  end;

  FTestType := ttNone;
end;

procedure TMainForm.UpdateEmbeddedState;
var
  EventSent: string;
  ClientId: TBoldClientID;
  i, count: integer;
  aEvent : ^TPropagatorEventType;
  res: Boolean;
begin
  Sleep(TimeOut);
  FTestType := ttEventTest;
  // update embedded state
  ClientId := 1;
  EventSent := Format('E:%s', [ClassAID]);
  (Clients[ClientId - 1] as IPropagatorTestClient).UpdateEmbeddedState(WideString('ClassA'),ClassAID, WideString('aString'));
  Sleep(TimeOut*4);
  Application.ProcessMessages;
  try
    count := EventsReceived.Count;
    for i:= 0 to count -1 do
    begin
      aEvent := EventsReceived[0];
      res := (aEvent^.Event = EventSent);
      if res then
        Break;
      EventsReceived.Delete(0);
      Dispose(aEvent);
    end;
    WriteToLog('UpdateEmbeddedState: %s     %s', ['ClassA.aString', BooleanToStr( res
                  and (aEvent^.ClientID <> ClientId))]);
    EventsReceived.Clear;
  except
    WriteToLog('UpdateEmbeddedState: %s     %s', ['ClassA.aString', BooleanToStr((false))]);
  end;
  FTestType := ttNone;
end;

procedure TMainForm.UpdateNonEmbeddedState;
var
  EventSent: string;
  ClientId: TBoldClientID;
  i, count: integer;
  aEvent : ^TPropagatorEventType;
  res: Boolean;
begin
  // update nonembedded state
  Sleep(TimeOut);
  FTestType := ttEventTest;
  // update embedded state
  ClientId := 1;
  EventSent := Format('I:hitList:%s', [SongID]);
  (Clients[clientID - 1] as IPropagatorTestClient).UpdateNonEmbeddedState(WideString('Song'), SongID,'HitList');
  Sleep(TimeOut*3);
  Application.ProcessMessages;
  try
    count := EventsReceived.Count;
    for i:= 0 to count -1 do
    begin
      aEvent := EventsReceived[0];
      res := (aEvent^.Event = EventSent);
      if res then
        Break;
      EventsReceived.Delete(0);
      Dispose(aEvent);
    end;
    WriteToLog('UpdateNonEmbeddedState: %s     %s', ['Song.HitList', BooleanToStr( res
                  and (aEvent^.ClientID <> ClientId))]);
    EventsReceived.Clear;
  except
    WriteToLog('UpdateNonEmbeddedState: %s     %s', ['Song.HitList', BooleanToStr((false))]);
  end;
  FTestType := ttNone;
end;

procedure TMainForm.DeleteObject;
var
  EventSent: string;
  ClientId: TBoldClientID;
  i, count: integer;
  aEvent : ^TPropagatorEventType;
  res: Boolean;
begin
  Sleep(TimeOut);
  FTestType := ttEventTest;
  // update embedded state
  ClientId := 1;
  EventSent := Format('C:%s', ['ClassA']);
  (Clients[ClientId - 1] as IPropagatorTestClient).DeleteObject('ClassA', ClassAID);
  Sleep(TimeOut*2);
  Application.ProcessMessages;
  try
    count := EventsReceived.Count;
    for i:= 0 to count -1 do
    begin
      aEvent := EventsReceived[0];
      res := (aEvent^.Event = EventSent);
      if res then
        Break;
      EventsReceived.Delete(0);
      Dispose(aEvent);
    end;
    WriteToLog('Delete Object: %s     %s', ['ClassA', BooleanToStr( res
                  and (aEvent^.ClientID <> ClientId))]);
    EventsReceived.Clear;
  except
    WriteToLog('Delete Object: %s     %s', ['ClassA', BooleanToStr((false))]);
  end;
  FTestType := ttNone;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  EventsReceived := TList.Create;
  EventsSent := TStringlist.Create;
  Messages := TStringList.Create;
  NoClients := 0;
  DBAlias := 'Test';
  NumberOfEventsReceived := 0;
  NumberOfEventsGenerated:= 0;
end;

procedure TMainForm.ReceiveEvent(const Event: string; ClientID: integer);
var
  aEvent: ^TPropagatorEventType;
begin
  case FTestType of
  ttEventTest:
    begin
      New(aEvent);
      aEvent^.Event := Event;
      aEvent^.ClientID := ClientId;
      EventsReceived.Add(aEvent);
    end;
  ttLoadTest:
    begin
      Inc(NumberOfEventsReceived);
      Timer1.Enabled := true;
    end;
  ttThroughPutTest:
    begin
      WriteToLog('Time Elapsed: %d', [DateTimeToTimeStamp(Now).Time - StartTime.Time]);
      FTestType := ttNone;
    end;
  ttNone: ;
  end; //case
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  InitializeDM;
end;

procedure TMainForm.LoadTest;
var
  Number: integer;
begin
  Number := StrToInt(Trim(edNumber.Text));
  if (Number = 0) then
  begin
    showmessage('Number of events not specified');
    LoadTestTimer.Enabled := false;
    Exit;
  end;
  FTestType := ttLoadTest;
  StartTime := DateTimetoTimeStamp(Now);
  (Clients[0] as IPropagatorTestClient).GenerateEvents(Number);
  Inc(NumberOfEventsGenerated, Number);
  WriteToLog('Number of Events generated      %d', [NumberOfEventsGenerated]);
  (Clients[0] as IPropagatorTestClient).UpdateDB;
  StartTime := DateTimeToTimeStamp(Now);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  WriteToLog('Number of events received: %d       Time elapsed:   %d milliseconds',
    [NumberOfEventsReceived, DateTimeToTimeStamp(Now).Time - StartTime.Time])
end;

procedure TMainForm.LoadTestTimerTimer(Sender: TObject);
begin
  LoadTest;
end;

procedure TMainForm.DisplayResults;
begin
  WriteToLog('Results Summary ........',[]);
  WriteToLog('Generated total of        %d events.', [NumberOfEventsGenerated]);
  WriteToLog('Recevied total of         %d events', [NumberOfEventsReceived]);
  WriteToLog('Time elapsed:             %d milliseconds', [DateTimeToTimeStamp(Now).Time - BeginTime.Time]);
end;

procedure TMainForm.InitializeDM;
begin
  TDm.EnsureDM;
  CoInitialize(nil);
  TestManager := TTestManager.Create;
  TestManager.OnReceiveEvent := ReceiveEvent;
  CoUninitialize;
  if not Assigned(fClients) then
    fClients := TInterfaceList.Create;
end;

procedure TMainForm.UpDown1Click(Sender: TObject; Button: TUDBtnType);
var
  Number: integer;
begin
  try
    Number := StrtoInt(Trim(edNumber.Text));
  except
    Exit;
  end;
  case Button of
    btNext: Inc(Number);
    btPrev: Dec(Number);
  end; //case
  edNumber.Text := InttoStr(Number);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(EventsReceived);
  FreeAndNil(EventsSent);
  FreeAndNil(Messages);
end;

procedure TMainForm.ThroughPutTest;
var
  Number: integer;
begin
{  FTestType := ttThroughPutTest;
  Number := StrtoInt(Trim(edNumber.Text));
  (Clients[0] as IPropagatorTestClient).GenerateEvents(Number);
  WriteToLog('Throuhgput test', []);
  WriteToLog('Number of events generated: %d', [Number]);
  StartTime := DateTimetoTimeStamp(Now);
 (Clients[0] as IPropagatorTestClient).UpdateDB; }
end;

procedure TMainForm.AutoTest(const Start: Boolean);
var
  i: integer;
  TestClient: IPropagatorTestClient;
begin
  if Start then
  begin
    InitializeClients;
    for i:= 0 to noClients - 1 do
    begin
      TestClient := Clients[i] as IPropagatorTestClient;
      TestClient.StartAutoTest;
    end;
    WriteToLog('Test Started', []);
    WriteToLog('Number Of Clients: %d', [NoClients]);
    WriteToLog('Start time: %s', [DateTimeToStr(Now)]);
  end
  else
  begin
    for i:= 0 to noClients - 1 do
    begin
      TestClient := Clients[i] as IPropagatorTestClient;
      TestClient.StopAutoTest;
    end;
    WritetoLog('Test Stopped', []);
    WritetoLog('Stop Time: %s', [DateTimetoStr(Now)]);
  end;
end;

procedure TMainForm.btnTestClick(Sender: TObject);
begin
  Testing := not Testing;
end;

procedure TMainForm.WriteToLog(const strFormat: string;
  const Args: array of const);
begin
  LogMemo.Lines.Add(Format(strFormat, Args));
end;

procedure TMainForm.InitializeClients;
var
  i: integer;
  TestClient: IPropagatorTestClient;
  num: integer;
begin
  NoClients := StrToInt(Trim(ednoclients.Text));
  num := noClients - Clients.Count;
  for i:= 1 to num do
  begin
    TestClient := CoPropagatorTestClient.Create;
    DBAlias := 'PropagatorTest';
    TestClient.SetID(i, TestManager, DBAlias);
    Clients.Add(TestClient);
  end;
end;

procedure TMainForm.SetTesting(Value: Boolean);
begin
  if Value <> fTesting then
  begin
    fTesting := Value;
    if Testing then
      btnTest.Caption := '&Stop Test'
    else
      btnTest.Caption := '&Start Test';
    AutoTest(fTesting);  
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  i: integer;
  TestClient: IPropagatorTestClient;
begin
  Testing := false;
  for i:= 0 to fClients.Count - 1 do
  begin
    TestClient := fClients[i] as IPropagatorTestClient;
    TestClient.CloseClient;
  end;
  fClients.Clear;
end;

end.
