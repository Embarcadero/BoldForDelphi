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
    procedure SetLogHeader(const Value: string);
    procedure SetProgressMax(const Value: integer);
    procedure ProcessInterruption;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure Hide;
    procedure Log(const s: string; LogType: TBoldLogType);
    procedure ProgressStep;
    procedure Show;
    procedure Sync;
    procedure StartLog(const SessionName: string);
    procedure EndLog;
    property LogLines: TStringList read GetLogLines;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldCommonConst;

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
  // intentionally left blank
end;

function TBoldSimpleLogReceiver.GetLogLines: TStringList;
begin
  if not Assigned(fLogLines) then
    fLogLines := TStringList.Create;
  Result := fLogLines;
end;

procedure TBoldSimpleLogReceiver.Hide;
begin
  // intentionally left blank
end;

procedure TBoldSimpleLogReceiver.Log(const s: string; LogType: TBoldLogType);
begin
  LogLines.Add(s);
end;

procedure TBoldSimpleLogReceiver.ProgressStep;
begin
  // intentionally left blank
end;

procedure TBoldSimpleLogReceiver.SetProgress(const Value: integer);
begin
  // intentionally left blank
end;

procedure TBoldSimpleLogReceiver.SetLogHeader(const Value: string);
begin
  Log(Value, ltInfo);
end;

procedure TBoldSimpleLogReceiver.SetProgressMax(const Value: integer);
begin
  // intentionally left blank
end;

procedure TBoldSimpleLogReceiver.Show;
begin
  // intentionally left blank
end;

procedure TBoldSimpleLogReceiver.StartLog(const SessionName: String);
begin
  LogLines.Add(Format(sSessionStart, [SessionName]));
end;

procedure TBoldSimpleLogReceiver.ProcessInterruption;
begin
  // Intentionally left blank
end;

procedure TBoldSimpleLogReceiver.Sync;
begin
  // Intentionally left blank
end;

end.

