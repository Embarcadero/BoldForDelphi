
{ Global compiler directives }
{$include bold.inc}
unit BoldLogHandlerSimple;

interface

uses
  Classes,
  BoldLogHandler,
  BoldDefs,
  BoldLogReceiverInterface;

type
  { TBoldLogHandlerReceiver }
  TBoldSimpleLogReceiver = class(TInterfacedObject, IBoldLogReceiver)
  private
    fLogLines: TStringList;
    FEnabled: Boolean;
    function GetLogLines: TStringList;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure SetProgress(const Value: integer);
    procedure SetLogHeader(const Value: string); virtual;
    procedure SetProgressMax(const Value: integer);
    procedure ProcessInterruption;

  public
    destructor Destroy; override;
    procedure Clear;
    procedure Hide;
    procedure Log(const s: string; LogType: TBoldLogType); virtual;
    procedure ProgressStep;
    procedure Show;
    procedure Sync;
    procedure StartLog(const SessionName: string); virtual;
    procedure EndLog;
    property LogLines: TStringList read GetLogLines;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils;

{ TBoldSimpleLogReceiver }

procedure TBoldSimpleLogReceiver.Clear;
begin
  LogLines.Clear;
end;

destructor TBoldSimpleLogReceiver.Destroy;
begin
  FreeAndNil(fLogLines);
  inherited;
end;

procedure TBoldSimpleLogReceiver.EndLog;
begin
end;

function TBoldSimpleLogReceiver.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TBoldSimpleLogReceiver.GetLogLines: TStringList;
begin
  if not Assigned(fLogLines) then
    fLogLines := TStringList.Create;
  Result := fLogLines;
end;


procedure TBoldSimpleLogReceiver.Hide;
begin
end;

procedure TBoldSimpleLogReceiver.Log(const s: string; LogType: TBoldLogType);
begin
  LogLines.Add(s);
end;

procedure TBoldSimpleLogReceiver.ProgressStep;
begin
end;

procedure TBoldSimpleLogReceiver.SetProgress(const Value: integer);
begin
end;

procedure TBoldSimpleLogReceiver.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TBoldSimpleLogReceiver.SetLogHeader(const Value: string);
begin
  Log(Value, ltInfo);
end;

procedure TBoldSimpleLogReceiver.SetProgressMax(const Value: integer);
begin
end;

procedure TBoldSimpleLogReceiver.Show;
begin
end;

procedure TBoldSimpleLogReceiver.StartLog(const SessionName: String);
begin
  LogLines.Add(Format(sSessionStart, [SessionName]));
end;

procedure TBoldSimpleLogReceiver.ProcessInterruption;
begin
end;

procedure TBoldSimpleLogReceiver.Sync;
begin
end;

end.
