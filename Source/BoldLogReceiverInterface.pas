
{ Global compiler directives }
{$include bold.inc}
unit BoldLogReceiverInterface;

interface

uses
  BoldDefs;

type
  { forward declarations }
  IBoldLogReceiver = interface;

  { IBoldLogReceiver }
  IBoldLogReceiver = interface(IUnknown)
  ['{FB3F1CFA-A5BE-4C56-8707-FF969E5C4FBE}']
    procedure SetProgress(const Value: integer);
    procedure SetLogHeader(const Value: string);
    procedure SetProgressMax(const Value: integer);
    procedure Clear;
    procedure Hide;
    procedure Log(const s: string; LogType: TBoldLogType);
    procedure ProgressStep;
    procedure Show;
    procedure StartLog(const SessionName: String);
    procedure EndLog;
    procedure Sync;
    procedure ProcessInterruption;
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    property ProgressMax: integer write SetProgressMax;
    property Progress: integer write SetProgress;
    property LogHeader: string write SetLogHeader;
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;

  TBoldLogReceiver = class(TInterfacedObject, IBoldLogReceiver)
  private
    fEnabled: boolean;
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
  protected
    procedure SetProgress(const Value: integer); virtual;
    procedure SetLogHeader(const Value: string); virtual;
    procedure SetProgressMax(const Value: integer); virtual;
    procedure Clear; virtual;
    procedure Hide; virtual;
    procedure Log(const s: string; LogType: TBoldLogType); virtual;
    procedure ProgressStep; virtual;
    procedure Show; virtual;
    procedure Sync; virtual;
    procedure ProcessInterruption; virtual;
    procedure StartLog(const sessionName: String); virtual;
    procedure EndLog; virtual;
  public
    function LogTypeToString(ALogType: TBoldLogType): string;
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;

implementation

{ TBoldLogReceiver }

procedure TBoldLogReceiver.Clear;
begin
end;

procedure TBoldLogReceiver.EndLog;
begin
end;

function TBoldLogReceiver.GetEnabled: boolean;
begin
  result := fEnabled;
end;

procedure TBoldLogReceiver.Hide;
begin
end;

procedure TBoldLogReceiver.Log(const s: string; LogType: TBoldLogType);
begin
end;

function TBoldLogReceiver.LogTypeToString(ALogType: TBoldLogType): string;
begin
  case ALogType of
   ltInfo: result := 'Info';
   ltDetail: result := 'Detail';
   ltWarning: result := 'Warning';
   ltError: result := 'Error';
   ltSeparator: result := BoldCRLF;
  end;
end;

procedure TBoldLogReceiver.ProcessInterruption;
begin
end;

procedure TBoldLogReceiver.ProgressStep;
begin
end;

procedure TBoldLogReceiver.SetEnabled(const Value: boolean);
begin
  fEnabled := Value;
end;

procedure TBoldLogReceiver.SetLogHeader(const Value: string);
begin
end;

procedure TBoldLogReceiver.SetProgress(const Value: integer);
begin
end;

procedure TBoldLogReceiver.SetProgressMax(const Value: integer);
begin
end;

procedure TBoldLogReceiver.Show;
begin
end;

procedure TBoldLogReceiver.StartLog(const sessionName: String);
begin
end;

procedure TBoldLogReceiver.Sync;
begin
end;

end.

