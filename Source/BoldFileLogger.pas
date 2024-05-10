unit BoldFileLogger;

interface

uses
  Classes,
  SysUtils,
  BoldLogReceiverInterface,
  BoldDefs,
  BoldBase;

type
  TBoldFileLogger = class(TBoldLogReceiver, IBoldLogReceiver)
  private
    fFileStream: TFileStream;
    fWriter: TStreamWriter;
  protected
    procedure SetLogHeader(const Value: string); override;
    procedure Log(const s: string; LogType: TBoldLogType); override;
    procedure StartLog(const SessionName: String); override;
    procedure EndLog; override;
    property ProgressMax: integer write SetProgressMax;
    property Progress: integer write SetProgress;
    property LogHeader: string write SetLogHeader;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
  end;


implementation

uses
  Windows;

{ TBoldFileLogger }

constructor TBoldFileLogger.Create(const AFileName: string);
begin
  fFileStream := TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
  fWriter := TStreamWriter.Create(fFileStream);
end;

destructor TBoldFileLogger.Destroy;
begin
  FreeAndNil(fFileStream);
  FreeAndNil(fWriter);
  inherited;
end;

procedure TBoldFileLogger.EndLog;
begin
  FlushFileBuffers(fFileStream.Handle);
end;

procedure TBoldFileLogger.Log(const s: string; LogType: TBoldLogType);
begin
  fWriter.Write(s); // LogTypeToString(LogType)
end;

procedure TBoldFileLogger.SetLogHeader(const Value: string);
begin
  fWriter.Write(Value);
end;

procedure TBoldFileLogger.StartLog(const SessionName: String);
begin
  fWriter.Write(SessionName);
end;

end.
