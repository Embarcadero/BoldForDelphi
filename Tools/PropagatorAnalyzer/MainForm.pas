unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TThreadAction = (taID, taLock, taUnlock);

  TThreadInfo = class
  private
    FResources: TStringList;
    FThreadName: String;
    fThread: integer;
  public
    constructor Create(Thread: integer);
    destructor destroy; override;
    procedure ProcessLock(Name: String);
    procedure ProcessUnLock(Name: String);
    property Resources: TStringList read FResources;
    property ThreadName: String read FThreadName write fThreadName;
    property Thread: integer read fThread;
  end;

  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Memo1: TMemo;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    fThreadInfos: TStringList;
    fLockInfo: TStringList;
    fDependentLocks: TStringList;
    { Private declarations }
    procedure ClearData(sList: TStringList);
    function GetThreadInfo(Thread: integer): TThreadInfo;
    procedure AddDependentLock(first, second: string);
    function CheckDependency(first, second: string): Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.Button1Click(Sender: TObject);
var
  sList: TStringList;
  i, j: integer;
  s: String;
  Action: TThreadAction;
  Value: string;
  Thread: integer;
  ThreadInfo: TThreadInfo;
begin
  ClearData(fTHreadInfos);
  ClearData(fLockInfo);
  ClearData(fDependentLocks);

  sList := TStringList.create;
  ProgressBar1.Visible := true;
  sLIst.LoadFromFile(Edit1.Text);
  ProgressBar1.Max := sLIst.Count-1;
  for i := 0 to sList.Count-1 do
  begin
    if i mod 100 = 0 then
      ProgressBar1.Position := i; 
    s := copy(SList[i], 10, maxint);
    if pos('U', s) = 1 then
      Action := taUnLock
    else if pos('L', s) = 1 then
      Action := taLock
    else if pos('ID', s) = 1 then
      Action := taId
    else
      raise Exception.Create('unknown action: '+s);
    Value := copy(s, pos('=', s)+1, maxint);
    value := copy(Value, 1, pos(':', Value)-1);
    delete(s, 1, pos(':', s)); 
    if pos('[', value) <> 0 then
      value := copy(value, 1, pos('[', value)-1);
    Thread := strToInt(copy(s, pos('=', s)+1, maxint));
    ThreadInfo := GetThreadInfo(Thread);
    case action of
      taId: begin
        ThreadInfo.ThreadName := Value;
      end;
      taUnlock: ThreadInfo.ProcessUnLock(Value);
      talock: ThreadInfo.Processlock(Value);
    end;
  end;
  SList.Free;
  memo1.Lines.Clear;
  for i := 0 to fDependentLocks.Count-1 do
  begin
    memo1.Lines.Add(fDependentLocks[i]);
    SLIst := TStringList(fDependentLocks.Objects[i]);
    for j := 0 to SList.count-1 do
      if CheckDependency(SLIst[j], fDependentLocks[i]) then
        memo1.Lines.Add('               ! '+SLIst[j])
      else
        memo1.Lines.Add('               * '+SLIst[j]);
  end;
  memo1.Lines.Add('---');

  memo1.Lines.AddStrings(fLockInfo);

  ProgressBar1.Visible := false;

end;

function TForm1.GetThreadInfo(Thread: integer): TThreadInfo;
var
  i: integer;
begin
  i := fThreadInfos.IndexOf(IntToStr(Thread));
  if i = -1 then
  begin
    result := TThreadInfo.Create(Thread);
    fThreadInfos.AddObject(IntToStr(Thread), result);
  end
  else
  begin
    result := TThreadinfo(fThreadInfos.Objects[i]);
  end;
end;

{ TThreadInfo }

constructor TThreadInfo.Create(Thread: integer);
begin
  inherited Create;
  FResources := TStringList.create;
  FThread := Thread;
end;

destructor TThreadInfo.destroy;
begin
  FreeAndNil(fResources);
  inherited;
end;

procedure TThreadInfo.ProcessLock(Name: String);
var
  i: integer;
  s: string;
begin
  if (fResources.Count = 0) or (FResources[FResources.Count-1] <> Name) then
  begin
    FResources.Add(Name);
    if fResources.Count > 0 then
    begin
      s := ThreadName;
      if s = '' then
        s := IntToStr(Thread);
      s := s + ':';
      for i := 0 to fResources.Count-1 do
      begin
        if i > 0 then
          s := s + '->';
        s := s + fResources[i];
      end;
      if Form1.fLockInfo.IndexOf(s) = -1 then
        Form1.fLockInfo.Add(s);
    end;
    
    for i := 0 to fResources.Count-2 do
      Form1.AddDependentLock(fResources[i], FResources[i+1]);
  end;
end;

procedure TThreadInfo.ProcessUnLock(Name: String);
begin
  if fResources.Count > 0 then
  begin
    if FResources[FResources.Count-1] = Name then
      fResources.Delete(fResources.Count-1);
  end
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fLockInfo := TStringList.create;
  fLockInfo.Sorted := true;
  fThreadInfos := TStringList.Create;
  fThreadInfos.Sorted := true;
  fDependentLocks := TStringList.Create;
  Memo1.Lines.Clear;
end;

procedure TForm1.AddDependentLock(first, second: string);
var
  i: integer;
  dependentLocks: TStringList;
begin
  i := fDependentLocks.IndexOf(first);
  if i = -1 then
  begin
    DependentLocks := TStringList.Create;
    dependentLocks.Sorted := true;
    fDependentLocks.AddObject(first, DependentLocks);
  end
  else
    dependentLocks := TStringList(fDependentLocks.Objects[i]);
  if dependentLocks.IndexOf(Second) = -1 then
    dependentLocks.Add(second);
end;

function TForm1.CheckDependency(first, second: string): Boolean;
var
  i: integer;
  dependentLocks: TStringList;
begin
  result := false;
  i := fDependentLocks.IndexOf(first);
  if i <> -1 then
  begin
    dependentLocks := TStringList(fDependentLocks.Objects[i]);
    result := dependentLocks.IndexOf(Second) <> -1;
  end;

end;

procedure TForm1.ClearData(sList: TStringList);
var
  i: integer;
begin
  for i := 0 to SList.Count-1 do
    if assigned(SList.Objects[i]) then
      SList.Objects[i].Free;
  SList.Clear;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ClearData(fTHreadInfos);
  ClearData(fLockInfo);
  ClearData(fDependentLocks);
  fTHreadInfos.Free;
  fLockInfo.Free;
  fDependentLocks.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(Edit1.text);
  OpenDialog1.FileName := ExtractFileName(Edit1.Text);
  if OpenDialog1.Execute then
    Edit1.Text := OpenDialog1.FileName;
end;

end.
