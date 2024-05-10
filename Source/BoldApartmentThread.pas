
{ Global compiler directives }
{$include bold.inc}
unit BoldApartmentThread;

interface

uses
  Windows,
  Messages,
  BoldThread;


const
  BM_CREATEOBJECTINTHREAD = WM_USER + $1234;

type
  TBoldApartmentThread = class;

  TCreateInstanceProc = function(const UnkOuter: IUnknown;
    const IID: TGUID; out Obj): HResult of object; stdcall;
  TBoldApartmentType = (batMTA, batSTA);

  PCreateInstanceInfo = ^TCreateInstanceInfo;
  TCreateInstanceInfo = record
    ApartmentThread: TBoldApartmentThread;
  end;

  TBoldApartmentThread = class(TBoldNotifiableThread)
  private
    FCreateInstanceProc: TCreateInstanceProc;
    FApartmentType: TBoldApartmentType;
    FUnkOuter: IUnknown;
    FIID: TGuid;
    FObjectCreatedEvent: THandle;
    FStream: Pointer;
    FCreateResult: HResult;
  protected
    class procedure CreateQueueWindow(var ServerWindow: HWnd); override;
    function ProcessMessage(var Msg: TMsg): Boolean; override;
  public
    constructor Create(const ApartmentType: TBoldApartmentType; CreateInstanceProc: TCreateInstanceProc; UnkOuter: IUnknown; const IID: TGuid);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Init(UnkOuter: IUnknown; const IID: TGuid);
    function MarshalInterface (pUnk: IUnknown): HResult;
    function UnmarshalInterface (out pObject): HResult;
    property ObjectCreatedEvent: THandle read FObjectCreatedEvent;
    property CreateResult: HResult read FCreateResult write FCreateResult;
    property ObjStream: Pointer read FStream;
    property ApartmentType: TBoldApartmentType read FApartmentType write FApartmentType;
    property CreateInstanceProc: TCreateInstanceProc read FCreateInstanceProc;
    property IID: TGuid read FIID;
    property UnkOuter: IUnknown read FUnkOuter;
  end;

implementation

uses
  ActiveX,
  SysUtils,

  BoldCoreConsts,
  BoldThreadSafeLog;

{ apartment handler window }
function ApartmentThreadWndProc (hWndTarget: HWND; iMessage, wParam, lParam: longint): longint; stdcall;
var
  at: TBoldApartmentThread;
  pciInfo: PCreateInstanceInfo absolute lParam;
  punk: IUnknown;
begin
  Result := 0;
  if (UINT (iMessage) = BM_CREATEOBJECTINTHREAD) then
  begin
    try
      Assert(Assigned(pciInfo));
      at := pciInfo^.ApartmentThread;
      at.CreateInstanceProc(at.UnkOuter, at.IID, punk);
      if Succeeded (at.CreateResult) then
        at.CreateResult := at.MarshalInterface (pUnk);
      ReleaseSemaphore(at.ObjectCreatedEvent, 1, nil);
      pUnk := nil;
      Result := 0;
    except
      BoldLogError('ApartmentThreadWndProc: pciInfo not initialized');
    end;
  end
  else
  if (iMessage = WM_QUIT) then
    DestroyWindow (hWndTarget)
  else begin
    Result := DefWindowProc (hWndTarget, iMessage, wParam, lParam);
  end;
end;

var
  cApartmentThreadWindowClass: TWndClass;

{ TBoldApartmentThread }
class procedure TBoldApartmentThread.CreateQueueWindow(var ServerWindow: HWnd);
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  cApartmentThreadWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance,
    cApartmentThreadWindowClass.lpszClassName,TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @ApartmentThreadWndProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(cApartmentThreadWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(cApartmentThreadWindowClass);
  end;
  ServerWindow := CreateWindow(cApartmentThreadWindowClass.lpszClassName, '', 0,
    0, 0, 0, 0, 0, 0, HInstance, nil);
end;

constructor TBoldApartmentThread.Create(const ApartmentType: TBoldApartmentType; CreateInstanceProc: TCreateInstanceProc;
  UnkOuter: IUnknown; const IID: TGuid);
begin
  FApartmentType := ApartmentType;
  FCreateInstanceProc := CreateInstanceProc;
  FUnkOuter := UnkOuter;
  FIID := IID;
  FObjectCreatedEvent := CreateSemaphore(nil, 0, 1, nil);
  FreeOnTerminate := False;
  inherited Create(True);
end;

destructor TBoldApartmentThread.Destroy;
begin
  CloseHandle(FObjectCreatedEvent);
  inherited Destroy;
end;

procedure TBoldApartmentThread.Execute;
var
  rMsg: TMsg;
  res: integer;
begin
  InitServerWindow (TRUE);
  BoldLogThread('ID=AptThread');

  case ApartmentType of
    batSTA: CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
    batMTA: CoInitializeEx(nil, COINIT_MULTITHREADED);
  end;
  EnsureMessageQueue;
  SignalReady;
  try
    while not Terminated do
    begin
      res := Integer(GetMessage(rMsg, 0, 0, 0));
      if res = -1 then
        Terminate
      else if rMsg.Message = WM_QUIT then
        Terminate
      else
        ProcessMessage(rMsg);
    end;
  finally
    FUnkOuter := nil;
    CoUnInitialize;
  end;
end;

procedure TBoldApartmentThread.Init(UnkOuter: IUnknown; const IID: TGuid);
begin
  FIID := IID;
  FUnkOuter := UnkOuter;
end;

function TBoldApartmentThread.MarshalInterface (pUnk: IUnknown): HResult;
begin
  Result := CoMarshalInterThreadInterfaceInStream (FIID, pUnk, IStream (self.FStream));
end;

type
  PInterface =  ^IUnknown;

function TBoldApartmentThread.UnmarshalInterface (out pObject): HResult;
begin
  Result := S_FALSE;
  try
    Assert (FStream <> NIL);
    Result := CoGetInterfaceAndReleaseStream (IStream (FStream), iid, pObject);
    FStream := NIL;
  except on E: Exception do
    BoldLogError('%s.UnmarshalInterface: %s', [ClassName, E.Message]);
  end;
end;

function TBoldApartmentThread.ProcessMessage(var Msg: TMsg): Boolean;
begin
  DispatchMessage(Msg);
  Result := True;
end;

initialization
  cApartmentThreadWindowClass.style := 0;
  cApartmentThreadWindowClass.lpfnWndProc := @ApartmentThreadWndProc;
  cApartmentThreadWindowClass.cbClsExtra := 0;
  cApartmentThreadWindowClass.cbWndExtra := 0;
  cApartmentThreadWindowClass.hInstance := 0;
  cApartmentThreadWindowClass.hIcon := 0;
  cApartmentThreadWindowClass.hCursor := 0;
  cApartmentThreadWindowClass.hbrBackground := 0;
  cApartmentThreadWindowClass.lpszMenuName := NIL;
  cApartmentThreadWindowClass.lpszClassName := 'TBoldApartmentThreadWindow'
end.
