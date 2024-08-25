
{ Global compiler directives }
{$include bold.inc}
unit BoldPerformanceCounter;

interface

uses
  Windows,
  Classes,
  BoldDefs;

type
  { forward declarations }
  TBoldPerformanceCounter = class;
  TBoldPerformanceCounterState = (bpcsStopped, bpcsStarted);
  TBoldPerformanceCounterClass = class of TBoldPerformanceCounter;

  TBoldPerformanceData = record
  private
    fAccumulatedTime: TLargeInteger;
    fLastCallTime: Int64;
    fStart: TLargeInteger;
    fCalls: Int64;
    fKernelTime, fUserTime: TFileTime;
    fAccumulatedCPUTime: TDateTime;
    fLastCallCpuTime: TDateTime;
    function GetLastCallSeconds: double;
    function GetLastCallAsString: string;
    function GetAccumulatedTime: TDateTime;
    function GetSeconds: double;
    function GetAccumulatedCPUTimeMs: string;
    function GetAccumulatedTimeMs: string;
    function GetLastCallCpuTimeMs: string;
  public
    procedure Clear;
    property LastCallSeconds: double read GetLastCallSeconds;
    property LastCallAsString: string read GetLastCallAsString;
//    property Calls: Int64 read fCalls;
    property AccumulatedTime: TDateTime read GetAccumulatedTime;
    property AccumulatedTimeMs: string read GetAccumulatedTimeMs;
    property LastCallCpuTime: TDateTime read fLastCallCpuTime;
    property LastCallCpuTimeMs: string read GetLastCallCpuTimeMs;
    property AccumulatedCpuTime: TDateTime read fAccumulatedCPUTime;
    property AccumulatedCpuTimeMS: string read GetAccumulatedCPUTimeMs;
    property Seconds: Double read GetSeconds;
  end;

  { TBoldPerformanceCounter }
  TBoldPerformanceCounter = class
  private
    fOwnData: TBoldPerformanceData;
    fChildrenData: TBoldPerformanceData;
    fNumberOfChildrenRunning: integer;
    fChildren: TList;
    fOwner: TBoldPerformanceCounter;
    fName: String;
    fTag: integer; // for user convenience
    fActive: boolean;
    function GetChildren(index: integer): TBoldPerformanceCounter;
    function GetChildByName(const AName: String): TBoldPerformanceCounter;
    function GetAsDetailedString: String;
    function GetAsString: String;
    function GetChildCount: integer;
    function GetSeconds: double;
    function GetPercentOfOwner: double;
    procedure StartFromChild;
    procedure StopFromChild;
    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
    function GetSecondsWithoutChildren: Double;
    function GetLastCallSeconds: double;
    function GetLastCallAsString: string;
    function GetAccumulatedTime: TDateTime;
    function GetNamePath: string;
    function GetPercentOfOwnerAsString: String;
    function GetAccumulatedTimeMs: string;
    function GetfAccumulatedCPUTimeMS: string;
  protected
    function NumberOfParents: integer;
    procedure InternalStart;
    procedure InternalStop;
    function FileTime2DateTime(FileTime: TFileTime): TDateTime;
    function HasRuningChildren: boolean;
  public
    constructor Create(Owner: TBoldPerformanceCounter; const Name: String); virtual;
    destructor Destroy; override;
    procedure Reset;
    procedure Start;
    procedure Restart;
    procedure Stop; virtual;
    procedure Clear;
    function WriteToFile(FileName: string): Boolean;
    property Name: string read fName;
    property NamePath: string read GetNamePath;
    property ChildCount: integer read GetChildCount;
    property Children[index: integer]: TBoldPerformanceCounter read GetChildren;
    property ChildByName[const name: String]: TBoldPerformanceCounter read GetChildByName;
    property AsString: String read GetAsString;
    property AsDetailedString: string read GetAsDetailedString;

    property Seconds: Double read GetSeconds;
    property SecondsWithoutChildren: Double read GetSecondsWithoutChildren;
    property LastCallSeconds: double read GetLastCallSeconds;
    property LastCallAsString: string read GetLastCallAsString;
    property PercentOfOwner: Double read GetPercentOfOwner;
    property PercentOfOwnerAsString: String read GetPercentOfOwnerAsString;
    property Active: boolean read GetActive write SetActive;
    property Owner: TBoldPerformanceCounter read fOwner;
    property Calls: Int64 read fOwnData.fCalls;
    property AccumulatedTime: TDateTime read GetAccumulatedTime;
    property AccumulatedTimeMs: string read GetAccumulatedTimeMs;
    property LastCallCpuTime: TDateTime read fOwnData.fLastCallCpuTime;
    property AccumulatedCpuTime: TDateTime read fOwnData.fAccumulatedCPUTime;
    property AccumulatedCpuTimeMs: string read GetfAccumulatedCPUTimeMS;

    property Tag: integer read fTag write fTag;

    property OwnData: TBoldPerformanceData read fOwnData;
    property ChildrenData: TBoldPerformanceData read fChildrenData;
  end;

function BoldMainPerformanceCounter: TBoldPerformanceCounter;

implementation

uses
  SysUtils,
  DateUtils,
  System.Types,

  BoldCoreConsts,
  BoldUtils;

var
  G_MainPerformanceCounter: TBoldPerformanceCounter;
  Frequency: TLargeInteger;

const
  cTimeFormat = 'hh:nn:ss.zzz';

function BoldMainPerformanceCounter: TBoldPerformanceCounter;
begin
  if not assigned(G_MainPerformanceCounter) then
    G_MainPerformanceCounter := TBoldPerformanceCounter.Create(nil, 'BoldMainPerformanceCounter');
  result := G_MainPerformanceCounter;
end;

function TBoldPerformanceCounter.FileTime2DateTime(FileTime: TFileTime): TDateTime;    //Convert then FileTime to TDatetime format
var
  ft1: TFileTime;
  st: TSystemTime;
begin
  if FileTime.dwLowDateTime + FileTime.dwHighDateTime = 0 then
    Result := 0
  else
  begin
    FileTimeToLocalFileTime(FileTime, ft1);
    FileTimeToSystemTime(ft1, st);
    Result := SystemTimeToDateTime(st);
  end;
end;

procedure TBoldPerformanceCounter.Clear;
var
  i: integer;
begin
  for i := ChildCount - 1 downto 0 do
  begin
    Children[i].free;
  end;
  fChildren.Clear;
  Reset;
end;

constructor TBoldPerformanceCounter.Create(Owner: TBoldPerformanceCounter; const Name: String);
begin
  Assert(Name <> '');
  inherited create;
  fNUmberOfChildrenRunning := 0;
  fChildren := TList.Create;
  fOwner := Owner;
  fName := Name;
  if Assigned(Owner) then
    Owner.fChildren.Add(self);
end;

destructor TBoldPerformanceCounter.Destroy;
var
  i: integer;
  BoldPerformanceCounter: TBoldPerformanceCounter;
begin
  if Assigned(fOwner) then
    fOwner.fChildren.Remove(self);
  for i := ChildCount-1 downto 0 do
  begin
    BoldPerformanceCounter := Children[i];
    BoldPerformanceCounter.fOwner := nil;
    BoldPerformanceCounter.Free;
  end;
  fChildren.Free;
  inherited;
end;

function TBoldPerformanceCounter.GetChildCount: integer;
begin
  result := fChildren.Count;
end;

function TBoldPerformanceCounter.GetChildren(index: integer): TBoldPerformanceCounter;
begin
  result := TBoldPerformanceCounter(fChildren[index]);
end;

function TBoldPerformanceCounter.GetfAccumulatedCPUTimeMS: string;
begin
  result := FormatDateTime(cTimeFormat, AccumulatedCPUTime);
end;

function TBoldPerformanceCounter.GetLastCallAsString: string;
begin
  result := Trim(format('%8.3f', [LastCallSeconds]));
end;

function TBoldPerformanceCounter.GetLastCallSeconds: double;
begin
  result := (fOwnData.fLastCallTime / 10000000);
end;

function TBoldPerformanceCounter.GetNamePath: string;
begin
  if Assigned(Owner) and (Owner.NamePath <> '') then
    result := Owner.NamePath + '.' + name
  else
    result := name;
end;

function TBoldPerformanceCounter.GetChildByName(const AName: String): TBoldPerformanceCounter;
var
  i: integer;
begin
//  if there is a need consider using hash instead
  for I := 0 to fChildren.Count - 1 do
    if CompareText(AName, TBoldPerformanceCounter(fChildren[i]).Name) = 0 then
    begin
      result := Children[i];
      exit;
    end;
  result := TBoldPerformanceCounterClass(classType).Create(self,AName)
end;

procedure TBoldPerformanceCounter.Reset;
var
  i: integer;
begin
  fOwnData.Clear;
  for i := 0 to ChildCount-1 do
    Children[i].Reset;
end;

procedure TBoldPerformanceCounter.Restart;
begin
  Reset;
  Start;
end;

procedure TBoldPerformanceCounter.SetActive(const Value: boolean);
begin
  if fActive <> Value then
  begin
    if Value then
      Start
    else
      Stop;
  end;
end;

procedure TBoldPerformanceCounter.InternalStart;
var
  CreationTime, ExitTime: TFileTime;
begin
  if HasRuningChildren then
    QueryPerformanceCounter(fChildrenData.FStart)
  else
    QueryPerformanceCounter(fOwnData.FStart);
  fActive := true;
  if HasRuningChildren then
    GetProcessTimes(GetCurrentProcess, CreationTime, ExitTime, fChildrenData.fKernelTime, fChildrenData.fUserTime)
  else
    GetProcessTimes(GetCurrentProcess, CreationTime, ExitTime, fOwnData.fKernelTime, fOwnData.fUserTime);
end;

procedure TBoldPerformanceCounter.InternalStop;
var
  CurrentTime: TLargeInteger;
  CreationTime, ExitTime, KernelTime, UserTime: TFileTime;
begin
  QueryPerformanceCounter(CurrentTime);
  if HasRuningChildren then
  begin
    fChildrenData.fLastCallTime := CurrentTime - fChildrenData.fStart;
    fChildrenData.fAccumulatedTime := fChildrenData.fAccumulatedTime + fChildrenData.fLastCallTime;
  end
  else
  begin
    fOwnData.fLastCallTime := CurrentTime - fOwnData.fStart;
    fOwnData.fAccumulatedTime := fOwnData.fAccumulatedTime + fOwnData.fLastCallTime;
  end;
  GetProcessTimes(GetCurrentProcess, CreationTime, ExitTime, KernelTime, UserTime);
  if HasRuningChildren then
  begin
    fChildrenData.fLastCallCpuTime := FileTime2DateTime(KernelTime) + FileTime2DateTime(UserTime) - FileTime2DateTime(fChildrenData.fKernelTime) - FileTime2DateTime(fChildrenData.fUserTime);
    fChildrenData.fAccumulatedCpuTime := fChildrenData.fAccumulatedCpuTime + fChildrenData.fLastCallCpuTime;
  end
  else
  begin
    fOwnData.fLastCallCpuTime := FileTime2DateTime(KernelTime) + FileTime2DateTime(UserTime) - FileTime2DateTime(fOwnData.fKernelTime) - FileTime2DateTime(fOwnData.fUserTime);
    fOwnData.fAccumulatedCpuTime := fOwnData.fAccumulatedCpuTime + fOwnData.fLastCallCpuTime;
  end;
  fActive := false;
end;

procedure TBoldPerformanceCounter.Start;
begin
  if active then
    exit;
  if HasRuningChildren then
    inc(fChildrenData.fCalls)
  else
    inc(fOwnData.fCalls);
  if assigned(fOwner) then
    fOwner.StartFromChild;
  if HasRuningChildren then
    QueryPerformanceCounter(fChildrenData.fStart)
  else
    QueryPerformanceCounter(fOwnData.fStart);
  InternalStart;
end;

procedure TBoldPerformanceCounter.Stop;
begin
  if not active then
    exit; // or do we want to raise ?
  InternalStop;
  if assigned(fOwner) then
    fOwner.StopFromChild;
end;

function TBoldPerformanceCounter.GetSeconds: double;
begin
  result := DateUtils.SecondSpan(AccumulatedTime, 0);
end;

function TBoldPerformanceCounter.GetSecondsWithoutChildren: Double;
var
  i: integer;
begin
  result := Seconds;
  for I := 0 to GetChildCount - 1 do
  begin
    result := result - Children[i].Seconds;
  end;
end;

function TBoldPerformanceCounter.HasRuningChildren: boolean;
begin
  result := fNumberOfChildrenRunning > 0;
end;

function TBoldPerformanceCounter.NumberOfParents: integer;
var
  lCounter: TBoldPerformanceCounter;
begin
  result := 0;
  lCounter := fOwner;
  while Assigned(lCounter) do
  begin
    lCounter := lCounter.fOwner;
    inc(result);
  end;
end;

function TBoldPerformanceCounter.GetPercentOfOwner: double;
begin
  if Assigned(fOwner) and (fOwner.ChildrenData.AccumulatedTime <> 0)then
    result := 100 * AccumulatedTime/fOwner.ChildrenData.AccumulatedTime
  else
    result := 100;
end;

function TBoldPerformanceCounter.GetPercentOfOwnerAsString: String;
begin
  result := format('%4.2f', [PercentOfOwner])+'%';
end;

function TBoldPerformanceCounter.GetAccumulatedTime: TDateTime;
var
  CurrentTime: TLargeInteger;
begin
  // for consistency this returns ownData like all other places
  if Active {and HasRuningChildren} then
  begin
    QueryPerformanceCounter(CurrentTime);
    result := (fOwnData.fAccumulatedTime + CurrentTime - fOwnData.fStart) / Frequency / SecsPerDay;
  end
  else
    result := (fOwnData.fAccumulatedTime) / Frequency / SecsPerDay;

// ElapsedMilliseconds / MSecsPerSec / SecsPerDay;
end;

function TBoldPerformanceCounter.GetAccumulatedTimeMs: string;
begin
  result := FormatDateTime(cTimeFormat, AccumulatedTime);
end;

function TBoldPerformanceCounter.GetActive: boolean;
begin
  result := fActive;
end;

function TBoldPerformanceCounter.GetAsDetailedString: String;
var
  i: integer;
  lIndent: string;
begin
  result := '';
  if Calls > 0 then
  begin
    lIndent := StringOfChar(' ', NumberOfParents);

    result := format('%-50s %8.9fs  calls: %5d ', [lIndent+fName, Seconds, Calls]);
    if Assigned(fOwner) then
      result := format('%s %4.2f %% of %s', [result, PercentOfOwner, fOwner.fName]);
    result := result + BOLDCRLF;
  end;
  for i := 0 to ChildCount - 1 do
    result := result + Children[i].AsDetailedString + BOLDCRLF;
end;

function TBoldPerformanceCounter.GetAsString: String;
var
  i: integer;
begin
  result := format(sCallCount, [NamePath, Seconds, Calls]);
  if Assigned(fOwner) then
    result := format(sPercentCount, [result, PercentOfOwner, fOwner.NamePath]);

  result := result+BOLDCRLF;

  for i := 0 to ChildCount - 1 do
    result := result + Children[i].AsString;
end;

procedure TBoldPerformanceCounter.StartFromChild;
begin
  if Active and not HasRuningChildren then
    stop; // stop with own data, start with childdata
  inc(fNumberOfChildrenRunning);
  if fNumberOfChildrenRunning = 1 then
    start;
end;

procedure TBoldPerformanceCounter.StopFromChild;
begin
  if fNumberOfChildrenRunning = 1 then
    stop;
  dec(fNumberOfChildrenRunning);
end;

function TBoldPerformanceCounter.WriteToFile(FileName: string): Boolean;
var
  StringList: TStringList;
begin
  result := true;
  StringList := TStringList.create;
  try
    StringList.Text := AsString;
    Stringlist.SaveToFile(FileName);
  finally
    Stringlist.Free;
  end;
end;

{ TBoldPerformanceData }

procedure TBoldPerformanceData.Clear;
begin
  fAccumulatedTime := 0;
  fLastCallTime := 0;
  fStart := 0;
  fCalls := 0;
  fKernelTime.dwLowDateTime := 0;
  fKernelTime.dwHighDateTime := 0;
  fUserTime.dwLowDateTime := 0;
  fUserTime.dwHighDateTime := 0;
  fAccumulatedCPUTime := 0;
  fLastCallCpuTime := 0;
end;

function TBoldPerformanceData.GetAccumulatedCPUTimeMs: string;
begin
  result := FormatDateTime(cTimeFormat, AccumulatedCPUTime);
end;

function TBoldPerformanceData.GetAccumulatedTime: TDateTime;
begin
{  if Active and HasRuningChildren? then
  begin
    QueryPerformanceCounter(CurrentTime);
    result := (fOwnData.fAccumulatedTime + CurrentTime - fOwnData.fStart) / Frequency / SecsPerDay;
  end
  else}
    result := (fAccumulatedTime) / Frequency / SecsPerDay;
end;

function TBoldPerformanceData.GetAccumulatedTimeMs: string;
begin
  result := FormatDateTime(cTimeFormat, AccumulatedTime);
end;

function TBoldPerformanceData.GetLastCallAsString: string;
begin
  result := Trim(format('%8.3f seconds', [LastCallSeconds]));
end;

function TBoldPerformanceData.GetLastCallCpuTimeMs: string;
begin
  result := FormatDateTime(cTimeFormat, LastCallCpuTime);
end;

function TBoldPerformanceData.GetLastCallSeconds: double;
begin
  result := (fLastCallTime / 10000000);
end;

function TBoldPerformanceData.GetSeconds: double;
begin
  result := DateUtils.SecondSpan(AccumulatedTime, 0);
end;

initialization
  QueryPerformanceFrequency(Frequency);

finalization
  freeAndNil(G_MainPerformanceCounter);
end.
