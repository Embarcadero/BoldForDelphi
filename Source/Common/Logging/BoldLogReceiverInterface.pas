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
    property ProgressMax: integer write SetProgressMax;
    property Progress: integer write SetProgress;
    property LogHeader: string write SetLogHeader;
  end;

implementation

end.
