unit BoldPerformanceCounter;

interface

// ------------------------------------
// this unit was originally by OpenInfo, Hasse&Egil
// ------------------------------------

uses
  Windows,
  Classes,
  BoldDefs;

type
  { forward declarations }
  TBoldPerformanceCounter = class;
  {$IFDEF LINUX}
  TLargeInteger = longint;
  {$ENDIF}
  TBoldPerformanceCounterState = (bpcsStopped, bpcsStarted);

  { TBoldPerformanceCounter }
  TBoldPerformanceCounter = class
  private
    fNumberOfChildrenRunning: integer;
    fChildren: TStringList;
    fOwner: TBoldPerformanceCounter;
    fName: String;
    fAccumulatedTime: TLargeInteger;
    fStart: TLargeInteger;
    fCalls: integer;
    function GetChildren(index: integer): TBoldPerformanceCounter;
    function GetChildByName(Name: String): TBoldPerformanceCounter;
    function GetAsString: String;
    function GetChildCount: integer;
    function GetSeconds: double;
    function GetPercentOfOwner: double;
    procedure StartFromChild;
    procedure StopFromChild;
  public
    constructor Create(Owner: TBoldPerformanceCounter; Name: String);
    destructor Destroy; override;
    function Reset: boolean;
    function Start: boolean;
    function Stop: boolean;
    function WriteToFile(FileName: string): Boolean;
    property ChildCount: integer read GetChildCount;
    property Children[index: integer]: TBoldPerformanceCounter read GetChildren;
    property ChildByName[name: String]: TBoldPerformanceCounter read GetChildByName;
    property AsString: String read GetAsString;
    property Seconds: Double read GetSeconds;
    property PercentOfOwner: Double read GetPercentOfOwner;
  end;

function BoldMainPerformanceCounter: TBoldPerformanceCounter;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldCommonConst;

var
  G_MainPerformanceCounter: TBoldPerformanceCounter;

function BoldMainPerformanceCounter: TBoldPerformanceCounter;
begin
  if not assigned(G_MainPerformanceCounter) then
    G_MainPerformanceCounter := TBoldPerformanceCounter.Create(nil, 'BoldMainPerformanceCounter'); // do not localize
  result := G_MainPerformanceCounter;
end;

{$IFDEF LINUX}
{ TODO : Find performancecoutner for LINUX. }
function QueryPerformanceCounter(var PerformanceCounter: TLargeInteger): boolean;
begin
  PerformanceCounter := 0;
  Result := False;
end;

function QueryPerformanceFrequency(var Frequency: TLargeInteger): boolean;
begin
  Frequency := 0;
  Result := False;
end;
{$ENDIF}

constructor TBoldPerformanceCounter.Create(Owner: TBoldPerformanceCounter; Name: String);
begin
  inherited create;
  fNUmberOfChildrenRunning := 0;
  fChildren := TStringList.Create;
  fOwner := Owner;
  fName := UpperCase(Name);
  if Assigned(Owner) then
    Owner.fChildren.AddObject(Name,self);
end;

destructor TBoldPerformanceCounter.Destroy;
var
  i: integer;
  BoldPerformanceCounter: TBoldPerformanceCounter;
begin
  if Assigned(fOwner) then
  begin
    for i := 0 to fOwner.ChildCount - 1 do
    begin
      if fOwner.Children[i] = self then
      begin
        fOwner.fChildren.Delete(i);
        break;
      end;
    end;
  end;

  for i := ChildCount - 1 downto 0 do
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
  result := fChildren.Objects[index] as TBoldPerformanceCounter;
end;

function TBoldPerformanceCounter.GetChildByName(Name: String): TBoldPerformanceCounter;
var
  i: integer;
begin
  i := fChildren.IndexOf(UpperCase(Name));
  if i = -1 then
    result := TBoldPerformanceCounter.Create(self,name)
  else
    result := Children[i];
end;

function TBoldPerformanceCounter.Reset: boolean;
var
  i: integer;
begin
  fAccumulatedTime := 0;
  fCalls := 0;
  for i := 0 to ChildCount-1 do
    Children[i].Reset;
  result := true;
end;

function TBoldPerformanceCounter.Start: boolean;
begin
  inc(fCalls);
  if assigned(fOwner) then
    fOwner.StartFromChild;
  QueryPerformanceCounter(fStart);
  result := true;
end;

function TBoldPerformanceCounter.Stop: boolean;
var
  CurrentTime: TLargeInteger;
begin
  QueryPerformanceCounter(CurrentTime);
  if assigned(fOwner) then
    fOwner.StopFromChild;
  fAccumulatedTime := fAccumulatedTime + CurrentTime - fStart;
  result := true;
end;

function TBoldPerformanceCounter.GetSeconds: double;
var
  Frequency: TLargeInteger;
begin
  QueryPerformanceFrequency(Frequency);
  result := fAccumulatedTime / Frequency;
end;

function TBoldPerformanceCounter.GetPercentOfOwner: double;
begin
  if Assigned(fOwner)  and (fOwner.Seconds <> 0)then
    result := 100*Seconds/fOwner.Seconds
  else
    result := 100;
end;

function TBoldPerformanceCounter.GetAsString: String;
var
  i: integer;
begin
  result := format(sCallCount, [fName, Seconds, fCalls]);
  if Assigned(fOwner) then
    result := format(sPercentCount, [result, PercentOfOwner, fOwner.fName]);

  result := result+BOLDCRLF;

  for i := 0 to ChildCount - 1 do
    result := result + Children[i].AsString;
end;

procedure TBoldPerformanceCounter.StartFromChild;
begin
  if fNumberOfChildrenRunning = 0 then
    start;
  inc(fNumberOfChildrenRunning);
end;

procedure TBoldPerformanceCounter.StopFromChild;
begin
  dec(fNumberOfChildrenRunning);
  if fNumberOfChildrenRunning = 0 then
    stop;
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

initialization // empty

finalization
  freeAndNil(G_MainPerformanceCounter);
end.
