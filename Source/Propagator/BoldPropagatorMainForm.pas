
{ Global compiler directives }
{$include bold.inc}
unit BoldPropagatorMainForm;

interface

uses
  Windows,
  Classes,
  ComServ,
  Controls,
  Forms,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  Messages,
  Grids,
  SyncObjs,
  BoldClientHandler,
  BoldPropagatorConstants,
  Menus,
  contnrs;

type
  PPropagatorStateInfo = ^TPropagatorStateInfo;
  TPropagatorStateInfo = record
    Added: integer;
    Sent: integer;
    Events: integer;
    OutQ: integer;
    ClientId: integer;
  end;
  TPropagatorMainForm = class;

  TClientGuiInfo = class
  private
    FGuiForm: TPropagatorMainForm;
    fRow: integer;
    fName: String;
    fRegTime: TDateTime;
    fOutQ: integer;
    fSubscriptions: integer;
    fLeaseExpires: TDateTime;
    fId: integer;
    fPackages: integer;
    fEvents: integer;
    fLastSuccessful: TDateTime;
    fLongestInterval: TDateTime;
    fStatus: string;
    fLostEvents: integer;
  public
    constructor create(form: TPropagatorMainForm; Id: integer; Name: String; RegTime, LeaseExpires: TDateTime; Packages, Events: integer; LastSuccessful, LongestInterval: TDateTime; Status: string; LostEvents: integer);
    procedure RefreshGrid;
    property GuiForm: TPropagatorMainForm read FGuiForm;
    property Id: integer read fId;
    property Name: String read fName;
    property RegTime: TDateTime read fRegTime;
    property LeaseExpires: TDateTime read fLeaseExpires;
    property Subscriptions: integer read fSubscriptions;
    property Packages: integer read fPackages;
    property Events: integer read fEvents;
    property LastSuccessful: TDateTime read fLastSuccessful;
    property LongestInterval: TDateTime read fLongestInterval;
    property Status: string read fStatus;
    property LostEvents: Integer read fLostEvents;
    property OutQ: integer read fOutQ;
    property Row: integer read fRow;
  end;

  TClientGUIInfoList = class
  private
    fClientInfos: TObjectList;
    function GetClientInfoByIndex(Index: integer): TClientGUIInfo;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddClientInfo(ClientGuiInfo: TClientGUIInfo);
    procedure RemoveClientInfo(ClientGuiInfo: TClientGUIInfo);
    procedure Clear;
    property Count: integer read GetCount;
    property Items[Index: integer]: TClientGUIInfo read GetClientInfoByIndex; default;
  end;

  TPropagatorMainForm = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    tsClients: TTabSheet;
    sgClients: TStringGrid;
    tsMemory: TTabSheet;
    mmoMemory: TMemo;
    lbxSystemMemory: TListBox;
    tsStatisticsHistory: TTabSheet;
    lbStatisticsHistory: TListBox;
    TabSheet1: TTabSheet;
    mmoDebugInfo: TMemo;
    tmrDeadLockChecker: TTimer;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnuShutdown: TMenuItem;
    mnuRefreshGUI: TMenuItem;
    mnuCountSubscriptions: TMenuItem;
    mnuOnTop: TMenuItem;
    mnuHangPropagatorDEBUG: TMenuItem;
    mnuUptime: TMenuItem;
    mnuStarted: TMenuItem;
    N1: TMenuItem;
    rbDequeue: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure mnuRefreshClick(Sender: TObject);
    procedure mnuLockClick(Sender: TObject);
    procedure tmrDeadLockCheckerTimer(Sender: TObject);
    procedure mnuDisconnectClientsClick(Sender: TObject);
    procedure mnuStayOnTopClick(Sender: TObject);
    procedure mnuCountSubscriptionsClick(Sender: TObject);
  private
    fStartTime: TDateTime;
    fOutQ: integer;
    fInQ: integer;
    fAdded: integer;
    fSent: integer;
    fSubscriptions: integer;
    fClients: TClientGUIInfoList;
    fInQPeak: integer;
    {$IFDEF DEBUG}
    fLocked: Boolean;
    {$ENDIF}
    fClientHandler: TBoldClientHandler;
    fLastTopRow: integer;
    fLongestLockTime: TDateTime;
    fTotalClients: integer;
    fPeakClients: integer;
    fTotalLostEvents: integer;
    fClientsUpdated: Boolean;
    fClientInfoLock: TCriticalSection;
    function GetUpTime: TDateTime;
    function GetIsDequeueing: boolean;
    procedure SetIsDequeueing(const Value: boolean);
    function ClientByID(Id: integer): TClientGuiInfo;
    function GetClientCount: integer;
  public
    { Public declarations }
    destructor Destroy; override;
    procedure UpdateUptime;
    procedure UpdateStatistics;
    procedure UpdateMemoryStats;
    procedure ClearAllUsers;
    procedure SetClientHandler(ClientHandler: TBoldClientHandler);
    procedure BMUpdateClientInfo(var Msg: TMessage); message BM_UPDATE_CLIENT_INFO;
    procedure BMUpdateGlobalInfo(var Msg: TMessage); message BM_UPDATE_GLOBAL_INFO;
    procedure BMPropagatorStartDequeue(var Msg: TMessage); message BM_PROPAGATOR_DISPLAY_STARTDEQUEUE;
    procedure BMPropagatorDoneDequeue(var Msg: TMessage); message BM_PROPAGATOR_DISPLAY_DONEDEQUEUE;
    procedure AddUser(Id: integer; IdString: string; RegistrationTime, LeaseExpires: TDateTime; Packages, Events: integer; LastSuccessful, LongestInterval: TDateTime; Status: string; LostEvents: integer);
    procedure ClientInfoChanged(Id: integer; ClientSubscriptions, OutQ: integer);
    procedure GlobalInfoChanged(Added, Sent: integer);
    procedure SetClientHandlerStats(TotalClients, PeakClients, TotalLostEvents: integer);
    property Uptime: TDateTime read GetUpTime;
    property StartTime: TDateTime read fStartTime;
    property ClientCount: integer read GetClientCount;
    property Subscriptions: integer read fSubscriptions;
    property InQ: integer read fInQ write fInQ;
    property InQPeak: integer read fInQPeak write fInQPeak;
    property OutQ: integer read fOutQ;
    property Added: integer read fAdded;
    property Sent: integer read fSent;
    property IsDequeueing: boolean read GetIsDequeueing write SetIsDequeueing;
  end;

var
  PropagatorMainForm: TPropagatorMainForm;

implementation

uses
  Sysutils,

  BoldCoreConsts,
  BoldPropagatorServer,
  BoldThreadSafeLog,
  Boldutils;

{$R *.dfm}

function StatToStr(value: integer): string;
var
  temp: Double;
  whole, tenths, hundreds: integer;
begin
  if value = -1 then
    result := '?'
  else
  begin
    if Value >= 1000000 then
    begin
      result := 'M';
      temp := value/1000000;
    end
    else if value >= 1000 then
    begin
      result := 'k';
      temp := value/1000;
    end
    else
    begin
      result := '';
      temp := value;
    end;
    Whole := trunc(temp);
    tenths := trunc(frac(temp)*10);
    hundreds := trunc(frac(temp)*100);

    if result = '' then
      result := intToStr(Whole)
    else if Whole >= 100 then
      result := IntToStr(Whole) + Result
    else if Whole >= 10 then
      result := IntToStr(Whole) + '.' + IntToStr(tenths) + Result
    else
      result := IntToStr(Whole) + '.' + format('%.2d', [hundreds]) + Result
  end;

(*  OldDecimalSeparator := DecimalSeparator;
    if value > 10000000 then
      result := format('%1.1fM', [value/1000000])
    else if value > 1000000 then
      result := format('%1.2fM', [value/1000000])
    else if value > 10000 then
      result := format('%1.1fk', [value/1000])
    else if value > 1000 then
      result := format('%1.2fk', [value/1000])
    else
      result := IntToStr(value);
    DecimalSeparator := OldDecimalSeparator;
*)
end;


procedure TPropagatorMainForm.AddUser(Id: integer; IdString: string; RegistrationTime, LeaseExpires: TDateTime; Packages, Events: integer; LastSuccessful, LongestInterval: TDateTime; Status: string; LostEvents: integer);
begin
  fClientInfoLock.Acquire;
  try
    fClients.AddClientInfo( TClientGuiInfo.Create(self, Id, IdString, RegistrationTime, LeaseExpires, Packages, Events, LastSuccessful, LongestInterval, Status, LostEvents));
    fClientsUpdated := true;
  finally
    fClientInfoLock.Release;
  end;
end;

procedure TPropagatorMainForm.ClearAllUsers;
begin
  fClientINfoLock.Acquire;
  try
    fLastTopRow := sgClients.TopRow;


    fClients.Clear;
    fClientsUpdated := true;
  finally
    fClientInfoLock.Release;
  end;
end;

procedure TPropagatorMainForm.FormCreate(Sender: TObject);
var
  ConfigFile: TStringList;
  fileName: String;
begin
  fClientInfoLock := TCriticalSection.Create;
  BoldLogThread('ID=MainThread/GUI');
  fClients := TClientGUIInfoList.Create;
  fLastTopRow := -1;
  Caption := Format('%s Console', [TBoldPropagatorServer.Instance.ServerName]);
  fStartTime := now;
  PageControl1.ActivePage := tsClients;
  UpdateUptime;
  sgClients.Cells[0, 0] := 'ClientID';
  sgClients.Cells[1, 0] := 'Name';
  sgClients.Cells[2, 0] := 'RegTime';
  sgClients.Cells[3, 0] := 'Timeout';
  sgClients.Cells[4, 0] := 'Subscriptions';
  sgClients.Cells[5, 0] := 'Queue';
  sgClients.Cells[6, 0] := 'Longest Int';
  sgClients.Cells[7, 0] := 'Last (ago)';
  sgClients.Cells[8, 0] := 'Pkg';
  sgClients.Cells[9, 0] := 'Ev';
  sgClients.Cells[10, 0] := 'Status';
  sgClients.Cells[11, 0] := 'Lost';
  mnuCountSubscriptionsClick(self);
  rbDequeue.Parent := StatusBar1;
  rbDeQueue.Height := 13;
  rbDequeue.Top := 4;
  rbDequeue.Left := 2;

  mmoDebugInfo.lines.Clear;
  mmoDebugInfo.lines.add('File: '+paramStr(0));
  mmoDebugInfo.lines.add('');
  {$IFDEF DEBUG}
  mnuHangPropagatorDEBUG.Visible := true;
  formstyle := fsStayOnTop;
  {$ENDIF} 

  FileName := ChangeFileExt(GetModuleFileNameAsString(true), '.ini');
  if not FileExists(FileName) then
  begin
    mmoDebugInfo.lines.add('INI file: N/A');
  end
  else
  begin
    ConfigFile := TStringList.Create;
    ConfigFile.LoadFromFile(FileName);
    mmoDebugInfo.lines.add('INI file:');
    mmoDebugInfo.lines.add('---------------');
    mmoDebugInfo.lines.AddStrings(ConfigFile);
    mmoDebugInfo.lines.add('---------------');
    ConfigFile.Free;
  end;
end;

function TPropagatorMainForm.GetUpTime: TDAteTime;
begin
  result := now - StartTime;
end;

procedure TPropagatorMainForm.UpdateUptime;
begin
  mnuStarted.Caption := format('Started: %s', [DateTimeToStr(startTime)]);
  mnuUptime.Caption := format('Uptime: %d days %s', [trunc(Uptime), TimetoStr(Frac(Uptime))]);
end;

procedure TPropagatorMainForm.mnuRefreshClick(Sender: TObject);
begin
  mnuRefreshGUI.Caption := 'Hang on...';
  UpdateUptime;
  UpdateStatistics;
  UpdateMemoryStats;
end;

function TPropagatorMainForm.GetIsDequeueing: boolean;
begin
  result := rbDequeue.Checked;
end;

procedure TPropagatorMainForm.SetIsDequeueing(const Value: boolean);
begin
  rbDequeue.Checked := value;
end;

procedure TPropagatorMainForm.UpdateStatistics;
var
  s: string;
  i: integer;
  Client: TClientGuiInfo;
  ClientCount: integer;
  AllStatus: String;
  procedure AddStatus(lbl: String; Value: integer);
  begin
    if s <> '' then
      s := s + ' | ';
    s := s + lbl + ': ' + StatToStr(Value);
    if AllStatus <> '' then
      AllStatus := AllStatus + ',';
    AllStatus := AllStatus + lbl + '=' + StatToStr(Value);
  end;
begin
  fOutQ := 0;
  fSubscriptions := 0;
  fClientInfoLock.Acquire;
  try
    ClientCount := fClients.Count;
    for i := 0 to ClientCount-1 do
    begin
      Client := fClients[i];
      if Client.OutQ <> -1 then
        Inc(fOutQ, Client.OutQ);
      if Client.Subscriptions <> -1 then
        Inc(fSubscriptions, Client.Subscriptions);
    end;
  finally
    fClientInfoLock.Release;
  end;
  s := '';
  AllStatus := '';
  AddStatus('Cli', ClientCount);
  AddStatus('Tot', fTotalClients);
  AddStatus('^', fPeakClients);

  StatusBar1.Panels[3].Width := StatusBar1.Canvas.TextWidth(s) + 20;
  StatusBar1.Panels[3].Text := s;
  s := '';

  if mnuCountSubscriptions.Checked then
    AddStatus('Subs', Subscriptions);
  AddStatus('InQ', InQ);
  AddStatus('InQ^', InQPeak);
  AddStatus('OutQ', OutQ);
  AddStatus('Added', Added);
  AddStatus('Sent', sent);
  AddStatus('Lost', fTotalLostEvents);
  StatusBar1.Panels[4].Text := s;
  BoldLog(AllStatus);

  while lbStatisticsHistory.items.Count > 100 do
    lbStatisticsHistory.items.Delete(lbStatisticsHistory.items.Count-1);
  lbStatisticsHistory.items.insert(0, DateTimeToStr(now)+': '+AllStatus);
  if fLastTopRow <> -1 then
  begin
    if fLastTopRow > sgClients.RowCount - sgClients.VisibleRowCount then
      sgClients.TopRow := sgClients.RowCount - sgClients.VisibleRowCount
    else
      sgClients.TopRow := fLastTopRow;
  end;
end;



procedure TPropagatorMainForm.UpdateMemoryStats;
begin
  if Subscriptions > 0 then
  begin
    mmoMemory.Lines.Add('---');
    mmoMemory.Lines.Add(format('%0.2f bytes per subscription', [GetHeapStatus.TotalAllocated/Subscriptions]));
  end;
  lbxSystemMemory.Items.Add(format('Allocated %6.2f Mb', [GetHeapStatus.TotalAllocated/(1024*1024)]));
  lbxSystemMemory.TopIndex := lbxSystemMemory.items.Count - lbxSystemMemory.Height div lbxSystemMemory.ItemHeight;
end;

procedure TPropagatorMainForm.BMUpdateClientInfo(var Msg: TMessage);
var
  pcInfo: PPropagatorStateInfo;
begin
  pcInfo := Pointer(Msg.lParam);
  ClientInfoChanged(pcInfo^.ClientId, pcInfo^.Events, pcInfo^.OutQ);
  Dispose(pcInfo);
end;

procedure TPropagatorMainForm.BMUpdateGlobalInfo(var Msg: TMessage);
var
  pcInfo: PPropagatorStateInfo;
begin
  pcInfo := Pointer(Msg.lParam);
  GlobalInfoChanged(pcInfo^.Added, pcInfo^.Sent);
  Dispose(pcInfo);
end;

procedure TPropagatorMainForm.BMPropagatorDoneDequeue(var Msg: TMessage);
begin
  rbDequeue.Checked := false;
end;

procedure TPropagatorMainForm.BMPropagatorStartDequeue(var Msg: TMessage);
begin
  rbDequeue.Checked := true; 
end;

procedure TPropagatorMainForm.ClientInfoChanged(Id: integer; ClientSubscriptions, OutQ: integer);
var
  Client: TClientGuiInfo;
begin
  client := ClientById(id);
  if Assigned(Client) then
  begin
    Client.fSubscriptions := ClientSubscriptions;
    Client.fOutQ := OutQ;
    Client.RefreshGrid;
  end;
end;

{ TClientGuiInfo }

constructor TClientGuiInfo.create(form: TPropagatorMainForm; Id: integer; Name: String; RegTime, LeaseExpires: TDateTime; Packages, Events: integer; LastSuccessful, LongestInterval: TDateTime; Status: string; LostEvents: integer);
begin
  FGuiForm := Form;
  fId := Id;
  fName := Name;
  fRegTime := RegTime;
  fLeaseExpires := LeaseExpires;
  fSubscriptions := -1;
  fOutQ := -1;
  fEvents := Events;
  fPackages := Packages;
  fLastSuccessful := LastSuccessful;
  fLongestInterval := LongestInterval;
  fStatus := Status;
  fLostEvents := LostEvents;

  fRow := FGuiForm.ClientCount + 1;
end;

procedure TPropagatorMainForm.GlobalInfoChanged(Added, Sent: integer);
begin
  fAdded := Added;
  fSent := Sent;
  UpdateStatistics;
  mnuRefreshGUI.Caption := 'Refresh';
end;

procedure TClientGuiInfo.RefreshGrid;
begin
  GuiForm.sgClients.Cells[0, row] := IntToStr(ID);
  GuiForm.sgClients.Cells[1, row] := Name;
  GuiForm.sgClients.Cells[2, Row] := DateTimeToStr(RegTime);
  if LeaseExpires <> 0 then
    GuiForm.sgClients.Cells[3, Row] := TimeToStr(LeaseExpires)
  else
    GuiForm.sgClients.Cells[3, Row] := '?';
  GuiForm.sgClients.Cells[4, row] := StatToStr(Subscriptions);
  GuiForm.sgClients.Cells[5, row] := StatToStr(OutQ);

  GuiForm.sgClients.Cells[6, row] := TimeToStr(fLongestInterval);
  GuiForm.sgClients.Cells[7, row] := TimeToStr(now-fLastSuccessful);
  GuiForm.sgClients.Cells[8, row] := StatToStr(Packages);
  GuiForm.sgClients.Cells[9, row] := StatToStr(Events);
  GuiForm.sgClients.Cells[10, row] := Status;
  GuiForm.sgClients.Cells[11, row] := StatToStr(LostEvents);
end;

function TPropagatorMainForm.ClientByID(Id: integer): TClientGuiInfo;
var
  i: integer;
begin
  result := nil;
  fClientInfoLock.Acquire;
  try
    for i := 0 to fClients.Count-1 do
    begin
      if fClients[i].id = id then
      begin
        result := fClients[i];
        exit;
      end;
    end;
  finally
    fClientInfoLock.Release;
  end;
end;

function TPropagatorMainForm.GetClientCount: integer;
begin
  fClientInfoLock.Acquire;
  try
    result := fClients.Count;
  finally
    fClientInfoLock.Release;
  end;
end;

destructor TPropagatorMainForm.Destroy;
begin
  FreeAndNil(fClients);
  FreeAndNil(fClientInfoLock);
  inherited;
end;

procedure TPropagatorMainForm.mnuLockClick(Sender: TObject);
begin
  {$IFDEF DEBUG}
  if fLocked then
  begin
    fLocked := false;
    fClientHandler.DebugUnLock;
    mnuHangPropagatorDEBUG.Caption := 'Hang Propagator (DEBUG)';
  end
  else
  begin
    fLocked := true;
    fClientHandler.DebugLock;
    mnuHangPropagatorDEBUG.Caption := 'Release propagator';
  end;
  {$ENDIF}
end;

procedure TPropagatorMainForm.SetClientHandler(ClientHandler: TBoldClientHandler);
begin
  fClientHandler := ClientHandler;
end;

procedure TPropagatorMainForm.tmrDeadLockCheckerTimer(Sender: TObject);
var
  LockTime: TDateTime;
  i: integer;
  h, m, s, ms: Word;
begin
  fClientInfoLock.Acquire;
  try
    if fClientsUpdated then
    begin

      if fClients.Count = 0 then
      begin
        sgClients.RowCount := 2;
        SgClients.Rows[1].Text := '';
      end
      else
      begin
        sgClients.RowCount := fClients.Count + 1;
        for i := 0 to fClients.Count-1 do
          fClients[i].RefreshGrid;
      end;
    end;
    fClientsUpdated := false;
  finally
    fClientInfoLock.Release;
  end;
  tmrDeadLockChecker.Enabled := false;
  if assigned(fClientHandler) then
  begin
    LockTime := fClientHandler.LockTime;
    if LockTime > fLongestLockTime then
    begin
      fLongestLockTime := LockTime;
      if fLongestLockTime < EncodeTime(0, 0, 3, 0) then
      begin
        DecodeTime(fLongestLockTime, h, m, s, ms);
        StatusBar1.Panels[1].Text := IntToStr(s*1000+ms) + 'ms';
      end
      else
        StatusBar1.Panels[1].Text := TimeToStr(fLongestLockTime);
    end;

    if LockTime > 0 then
      StatusBar1.Panels[2].Text := 'DL?'
    else
      StatusBar1.Panels[2].Text := sOK;

    if LockTime > EncodeTime(0, 0, 3, 0) then
    begin
      tmrDeadLockChecker.Enabled := true;
    end;
  end;

  tmrDeadLockChecker.Enabled := true;
end;

procedure TPropagatorMainForm.SetClientHandlerStats(TotalClients,
  PeakClients, TotalLostEvents: integer);
begin
  fTotalClients := TotalClients;
  fPeakClients := PeakClients;
  fTotalLostEvents := TotallostEvents;
end;

procedure TPropagatorMainForm.mnuDisconnectClientsClick(Sender: TObject);
begin
  close;
end;

{ TClientGUIInfoList }

procedure TClientGUIInfoList.AddClientInfo(ClientGuiInfo: TClientGUIInfo);
begin
  fClientInfos.Add(ClientGUIInfo);
end;

procedure TClientGUIInfoList.Clear;
begin
  fClientInfos.Clear;
end;

constructor TClientGUIInfoList.Create;
begin
  fClientInfos := TObjectList.Create(True);
end;

destructor TClientGUIInfoList.Destroy;
begin
  FreeAndNil(fClientInfos);
  inherited;
end;

function TClientGUIInfoList.GetClientInfoByIndex(
  Index: integer): TClientGUIInfo;
begin
  Result := fClientInfos.Items[Index] as TClientGUIInfo;
end;

function TClientGUIInfoList.GetCount: integer;
begin
  Result := fClientInfos.Count;
end;

procedure TClientGUIInfoList.RemoveClientInfo(ClientGuiInfo: TClientGUIInfo);
begin
  fClientInfos.Remove(ClientGuiInfo);
end;

procedure TPropagatorMainForm.mnuStayOnTopClick(Sender: TObject);
begin
  mnuOnTop.Checked := not mnuOnTop.Checked;
  if mnuOnTop.Checked then
    formStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TPropagatorMainForm.mnuCountSubscriptionsClick(
  Sender: TObject);
begin
  mnuCountSubscriptions.Checked := not mnuCountSubscriptions.Checked;
  if mnuCountSubscriptions.Checked then
  begin
    sgClients.ColWidths[4] := 75;
    sgClients.ColWidths[1] := 150;
  end
  else
  begin
    sgClients.ColWidths[4] := -1;
    sgClients.ColWidths[1] := 225;
  end

end;

end.
