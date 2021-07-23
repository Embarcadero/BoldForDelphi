
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
    function GetLogLines: TStringList;
  protected
    procedure SetProgress(const Value: integer);
    procedure SetLogHeader(const Value: string); virtual;
    procedure SetProgressMax(const Value: integer);
    procedure ProcessInterruption;
  public
    destructor destroy; override;
    procedure Clear;
    procedure Hide;
    procedure Log(const s: string; LogType: TBoldLogType); virtual;
    procedure ProgressStep;
    procedure Show;
    procedure Sync;
    procedure StartLog(const SessionName: string); virtual;
    procedure EndLog;
    property LogLines: TStringList read GetLogLines;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldRev;

{ TBoldSimpleLogReceiver }

procedure TBoldSimpleLogReceiver.Clear;
begin
  LogLines.Clear;
end;

destructor TBoldSimpleLogReceiver.destroy;
begin
  FreeAndNil(fLogLines);
  inherited;
end;

procedure TBoldSimpleLogReceiver.EndLog;
begin
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
  LogLines.Add('Session: ' + SessionName);
end;

procedure TBoldSimpleLogReceiver.ProcessInterruption;
begin
end;

procedure TBoldSimpleLogReceiver.Sync;
begin
end;

initialization

end.
