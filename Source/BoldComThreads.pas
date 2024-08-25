
{ Global compiler directives }
{$include bold.inc}
unit BoldComThreads;

interface

uses
  Windows,
  Classes,
  BoldBase;

type
  { forward declarations }
  TBoldComConnectionThread = class;
  TBoldComWorkerThread = class;

  {--- TBoldComConnectionThread ---}
  TBoldComConnectionThread = class(TBoldMemoryManagedObject)
  private
    FBusy: Boolean;
    FCLSID: TGUID;
    FHostName: string;
    FIID: TGUID;
    FConnectRes: HResult;
  public
    constructor Create(const HostName: string; const CLSID, IID: TGUID);
    destructor Destroy; override;
    function Connect(out Obj): Boolean;
    property Busy: Boolean read FBusy;
    property ConnectRes: HResult read fConnectRes;
  end;

  {--- TBoldComWorkerThread ---}
  TBoldComWorkerThread = class(TThread)
  private
    FInterfaceID: TGUID;
    FInterfaceStream: Pointer;
    FWaitEvent: THandle;
    FWaitTime: Cardinal;
  protected
    FInterface: Pointer;
    procedure DoFinalize; virtual;
    procedure DoIdle; virtual;
    procedure DoInitialize; virtual;
    procedure DoWork; virtual; abstract;
    procedure Execute; override;
  public
    constructor Create(const Unk: IUnknown; const IID: TGUID; WaitTime: Cardinal);
    destructor Destroy; override;
    procedure Release;
  end;

implementation

uses
  SysUtils,
  Messages,
  ActiveX,
  BoldComUtils;

type
  TBoldComCreateObjectThread = class(TThread)
  private
    FInterfaceStream: Pointer;
    FOwner: TBoldComConnectionThread;
    FCreateRes: HResult;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TBoldComConnectionThread; CreateSuspended: Boolean);
    destructor Destroy; override;
    property InterfaceStream: Pointer read FInterfaceStream;
    property CreateRes: HResult read fCreateRes;
  end;

constructor TBoldComCreateObjectThread.Create(Owner: TBoldComConnectionThread;
  CreateSuspended: Boolean);
begin
  FOwner := Owner;
  inherited Create(CreateSuspended);
  FCreateRes := NOERROR;
end;

destructor TBoldComCreateObjectThread.Destroy;
begin
  inherited;
end;

procedure TBoldComCreateObjectThread.Execute;
var
  Unk: IUnknown;
begin
  CoInitialize(nil);
  try
    Unk := nil;
    try
      if FOwner.FHostName <> '' then
        BoldCreateRemoteComObject(FOwner.FHostName,FOwner.FCLSID,FOwner.FIID,Unk, fCreateRes)
      else
        BoldCreateComObject(FOwner.FCLSID,FOwner.FIID,Unk, FCreateRes);
    except
      on Exception do Unk := nil;
    end;
    if Assigned(Unk) then
      CoMarshalInterThreadInterfaceInStream(FOwner.FIID,Unk,IStream(FInterfaceStream));
  finally
    CoUninitialize;
  end;
end;

{-- TBoldComConnectionThread --------------------------------------------------}

constructor TBoldComConnectionThread.Create(const HostName: string;
  const CLSID, IID: TGUID);
begin
  FCLSID := CLSID;
  FHostName := HostName;
  FIID := IID;
  fConnectRes := NOERROR;
end;

destructor TBoldComConnectionThread.Destroy;
begin
  inherited;
end;

function TBoldComConnectionThread.Connect(out Obj): Boolean;
var
  ThreadObject: TBoldComCreateObjectThread;
  Status: Integer;
  Msg: TMsg;
  ThreadHandle: THandle;
begin
  Result := False;
  Pointer(Obj) := nil;
  if FBusy then Exit;
  FBusy := True;
  ThreadObject := TBoldComCreateObjectThread.Create(Self,True);
  try
    ThreadHandle := ThreadObject.Handle;
    ThreadObject.Resume;
    while True do
    begin
      Status := MsgWaitForMultipleObjects(1,ThreadHandle,False,INFINITE,QS_ALLINPUT);
      case Status of
        WAIT_OBJECT_0:
        begin
          if Assigned(ThreadObject.InterfaceStream) then
          begin
            CoGetInterfaceAndReleaseStream(IStream(ThreadObject.InterfaceStream),FIID,Obj);
            if Assigned(Pointer(Obj)) then Result := True;
          end;
          Break;
        end;
        WAIT_OBJECT_0 + 1:
        begin
          while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
          begin
            if Msg.Message = WM_QUIT then Break;
            DispatchMessage(Msg);
          end;
        end;
      else
        Break;
      end;
    end;
  finally
    ThreadObject.Free;
    FBusy := False;
  end;
end;

{-- TBoldComWorkerThread ------------------------------------------------------}

constructor TBoldComWorkerThread.Create(const Unk: IUnknown;
  const IID: TGUID; WaitTime: Cardinal);
begin
  CoMarshalInterThreadInterfaceInStream(IID,Unk,IStream(FInterfaceStream));
  FInterfaceId := IID;
  FWaitEvent := CreateEvent(nil,FALSE,FALSE,nil);
  FWaitTime := WaitTime;
  inherited Create(False);
end;

destructor TBoldComWorkerThread.Destroy;
begin
  Terminate;
  Release;
  CloseHandle(FWaitEvent);
  inherited;
end;

procedure TBoldComWorkerThread.DoFinalize;
begin
end;

procedure TBoldComWorkerThread.DoIdle;
begin
end;

procedure TBoldComWorkerThread.DoInitialize;
begin
end;

procedure TBoldComWorkerThread.Execute;
var
  Status: DWORD;
begin
  CoInitialize(nil);
  try
    if Assigned(FInterfaceStream) then
    begin
      CoGetInterfaceAndReleaseStream(IStream(FInterfaceStream),FInterfaceID,FInterface);
      FInterfaceStream := nil;
    end;
    if Assigned(FInterface) then
    begin
      DoInitialize;
      try
        while not Terminated do
        begin
          Status := WaitForSingleObject(FWaitEvent,FWaitTime);
          if Terminated then Break;
          case Status of
            WAIT_OBJECT_0:
              DoWork;
            WAIT_TIMEOUT:
              DoIdle;
          end;
        end;
      finally
        DoFinalize;
      end;
      IUnknown(FInterface)._Release;
      FInterface := nil;
    end;
  finally
    CoUninitialize;
  end;
end;

procedure TBoldComWorkerThread.Release;
begin
  SetEvent(FWaitEvent);
end;

end.
