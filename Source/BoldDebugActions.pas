
{ Global compiler directives }
{$include bold.inc}
unit BoldDebugActions;

interface

uses
  BoldSystemDebuggerForm,
  Classes,
  ActnList,
  BoldActions,
  BoldLogHandler;

type
  { forward declarations }
  TBoldSystemDebuggerAction = class;
  TBoldLogAction = class;
  TBoldLogSQLAction = class;
  TBoldLogPMAction = class;
  TBoldLogOCLAction = class;
  TBoldLogOSSAction = class;
  TBoldLogQueueAction = class;
  TBoldLogFormAction = class;

  { TBoldSystemDebuggerAction }
  TBoldSystemDebuggerAction = class(TBoldSystemHandleAction)
  private
    fDebugForm: TBoldSystemDebuggerFrm;
    function GetDebugForm: TBoldSystemDebuggerFrm;
  protected
    procedure CloseDebugForm;
    procedure CheckAllowEnable(var EnableAction: boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property DebugForm: TBoldSystemDebuggerFrm read GetDebugForm;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  { TBoldLogAction }
  TBoldLogAction = class(TAction)
  protected
    function GetLogHandler: TBoldLogHandler; virtual; abstract;
    function GetLogType: string; virtual; abstract;
    procedure SetLogHandler(Value: TBoldLogHandler); virtual; abstract;
    property LogType: string read GetLogType;
    property LogHandler: TBoldLogHandler read GetLogHandler write SetLogHandler;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  { TBoldLogOCLAction }
  TBoldLogOCLAction = class(TBoldLogAction)
  protected
    function GetLogHandler: TBoldLogHandler; override;
    function GetLogType: string; override;
    procedure SetLogHandler(Value: TBoldLogHandler); override;
  end;

  { TBoldLogOCLAction }
  TBoldLogSQLAction = class(TBoldLogAction)
  protected
    function GetLogHandler: TBoldLogHandler; override;
    function GetLogType: string; override;
    procedure SetLogHandler(Value: TBoldLogHandler); override;
  end;

  { TBoldLogPMAction }
  TBoldLogPMAction = class(TBoldLogAction)
  protected
    function GetLogHandler: TBoldLogHandler; override;
    function GetLogType: string; override;
    procedure SetLogHandler(Value: TBoldLogHandler); override;
  end;

  { TBoldLogOSSAction }
  TBoldLogOSSAction = class(TBoldLogAction)
  protected
    function GetLogHandler: TBoldLogHandler; override;
    function GetLogType: string; override;
    procedure SetLogHandler(Value: TBoldLogHandler); override;
  end;

  { TBoldLogQueueAction }
  TBoldLogQueueAction = class(TBoldLogAction)
  protected
    function GetLogHandler: TBoldLogHandler; override;
    function GetLogType: string; override;
    procedure SetLogHandler(Value: TBoldLogHandler); override;
  end;

  { TBoldLogFormAction }
  TBoldLogFormAction = class(TAction)
  private
    fShowing: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
  end;

implementation

uses
  SysUtils,
  Menus, // for TextToShortCut
  BoldOCL,
  BoldDBInterfaces,
  BoldPMappers,
  BoldObjectSpaceExternalEvents,
  BoldQueue;

{ TBoldSystemDebuggerAction }

procedure TBoldSystemDebuggerAction.CheckAllowEnable(var EnableAction: boolean);
begin
  inherited;
  EnableAction := EnableAction and BoldSystemHandle.Active;
end;

constructor TBoldSystemDebuggerAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'System debugger';
  ShortCut := TextToShortCut('Ctrl+Shift+D');
end;

procedure TBoldSystemDebuggerAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  if Assigned(fDebugForm) then
    CloseDebugForm
  else
    if Assigned(BoldSystemHandle) then
    begin
      DebugForm.Show;
      DebugForm.BringToFront;
    end;
  Checked := not Checked;
end;

function TBoldSystemDebuggerAction.GetDebugForm: TBoldSystemDebuggerFrm;
begin
  if not assigned(fDebugForm) then
  begin
    fDebugForm := TBoldSystemDebuggerFrm.CreateWithSystem(Self, BoldSystemHandle.System);
    fDebugForm.FreeNotification(Self);
  end;
  Result := fDebugForm;
end;

procedure TBoldSystemDebuggerAction.CloseDebugForm;
begin
  fDebugForm.Close;
end;

procedure TBoldSystemDebuggerAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = fDebugForm) and (Operation = opRemove) then
  begin
    fDebugForm := nil;
    Checked := false;
  end;
end;

{ TBoldLogOCLAction }

function TBoldLogOCLAction.GetLogHandler: TBoldLogHandler;
begin
  Result := BoldOCLLogHandler;
end;

function TBoldLogOCLAction.GetLogType: string;
begin
  Result := 'OCL';
end;

procedure TBoldLogOCLAction.SetLogHandler(Value: TBoldLogHandler);
begin
  BoldOCLLogHandler := Value;
end;

{ TBoldLogSQLAction }

function TBoldLogSQLAction.GetLogHandler: TBoldLogHandler;
begin
  Result := BoldSQLLogHandler;
end;

function TBoldLogSQLAction.GetLogType: string;
begin
  Result := 'SQL';
end;

procedure TBoldLogSQLAction.SetLogHandler(Value: TBoldLogHandler);
begin
  BoldSQLLogHandler := Value;
end;

{ TBoldLogAction }

constructor TBoldLogAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := Format('Toggle %s logs', [LogType]);
end;

procedure TBoldLogAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  if Checked then
  begin
    BoldLog.LogFmt('** Stop logging %s calls', [GetLogType]);
    LogHandler := nil;
  end
  else
  begin
    LogHandler := BoldLog;
    BoldLog.Show;
    BoldLog.LogFmt('** Start logging %s calls', [GetLogType]);
  end;
  Checked := not Checked;
end;

function TBoldLogAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := True;
end;

procedure TBoldLogAction.UpdateTarget(Target: TObject);
begin
  inherited;
  Checked := Assigned(LogHandler)
end;

{ TBoldLogPMAction }

function TBoldLogPMAction.GetLogHandler: TBoldLogHandler;
begin
  Result := BoldPMLogHandler;
end;

function TBoldLogPMAction.GetLogType: string;
begin
  Result := 'PMCalls';
end;

procedure TBoldLogPMAction.SetLogHandler(Value: TBoldLogHandler);
begin
  BoldPMLogHandler := Value;
end;

{ TBoldLogFormAction }

constructor TBoldLogFormAction.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Log view';
  ShortCut := TextToShortCut('Ctrl+L');
end;

procedure TBoldLogFormAction.ExecuteTarget(Target: TObject);
begin
  inherited;
  if fShowing then
    BoldLog.Hide
  else
    BoldLog.Show;
  fShowing := not fShowing;
end;

function TBoldLogFormAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := True;
end;

{ TBoldLogOSSAction }

function TBoldLogOSSAction.GetLogHandler: TBoldLogHandler;
begin
  result := BoldOSSLogHandler;
end;

function TBoldLogOSSAction.GetLogType: string;
begin
  Result := 'OSS traffic';
end;

procedure TBoldLogOSSAction.SetLogHandler(Value: TBoldLogHandler);
begin
  BoldOSSLogHandler := Value;
end;

{ TBoldLogQueueAction }

function TBoldLogQueueAction.GetLogHandler: TBoldLogHandler;
begin
  result := BoldQueueLogHandler;
end;

function TBoldLogQueueAction.GetLogType: string;
begin
  Result := 'Display Queue';
end;

procedure TBoldLogQueueAction.SetLogHandler(Value: TBoldLogHandler);
begin
  BoldQueueLogHandler := Value;
end;

end.
