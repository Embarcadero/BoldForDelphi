
{ Global compiler directives }
{$include bold.inc}
unit BoldWebConnection;

interface
uses
  BoldWinInet,
  Classes,
  BoldDataBlock;

resourcestring
  SInvalidURL = 'URL must be in the form "http://server.company.com/scripts/httpsrvr.dll"';
  SDefaultURL = 'http://server.company.com/scripts/httpsrvr.dll';

type

  {forward declarations}
  TBoldWebConnection = class;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldWebConnection = class(TComponent)
  private
    FUserName: string;
    FPassword: string;
    FURL: string;
    FProxy: string;
    FProxyByPass: string;
    FAgent: string;
    FURLHost: string;
    FURLSite: string;
    FURLPort: Integer;
    FInetRoot: HINTERNET;
    FInetConnect: HINTERNET;
    FURLScheme: Integer;
    FBeforeConnect: TNotifyEvent;
    procedure SetURL(const Value: string);
    procedure SetConnected(Value: Boolean);
    function GetConnected: Boolean;
  protected
    procedure Check(Error: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    function Send(const Data: TBoldDataBlock): Integer;
    function Receive(WaitForInput: Boolean; Context: Integer): TBoldDataBlock;
    property Connected: Boolean read GetConnected write SetConnected;
  published
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property URL: string read FURL write SetURL;
    property Agent: string read FAgent write FAgent;
    property Proxy: string read FProxy write FProxy;
    property ProxyByPass: string read FProxyByPass write FProxyByPass;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldDefs,
  windows;

{ TBoldWebConnection }

procedure TBoldWebConnection.Check(Error: Boolean);
var
  ErrCode: Integer;
  S: string;
begin
  ErrCode := GetLastError;
  if Error and (ErrCode <> 0) then
  begin
    SetLength(S, 256);
    FormatMessage(FORMAT_MESSAGE_FROM_HMODULE, Pointer(GetModuleHandle('wininet.dll')),
      ErrCode, 0, PChar(S), Length(S), nil);
    SetLength(S, StrLen(PChar(S)));
    while (Length(S) > 0) and CharInSet(S[Length(S)], [#10, #13]) do
      SetLength(S, Length(S) - 1);
    raise Exception.Create(S);
  end;
end;

constructor TBoldWebConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInetRoot := nil;
  FInetConnect := nil;
  FAgent := 'Bold Application';
  URL := SDefaultURL;
end;

function TBoldWebConnection.GetConnected: Boolean;
begin
  Result := Assigned(FInetConnect);
end;

function TBoldWebConnection.Receive(WaitForInput: Boolean; Context: Integer): TBoldDataBlock;
var
  Size, Downloaded, Status, Len, Index: DWord;
  S: string;
begin
  Len := SizeOf(Status);
  Index := 0;
  if BoldHttpQueryInfo(Pointer(Context), BOLD_HTTP_QUERY_STATUS_CODE or BOLD_HTTP_QUERY_FLAG_NUMBER,
    @Status, Len, Index) and (Status >= 300) then
  begin
    Index := 0;
    SetLength(S, Size);
    if BoldHttpQueryInfo(Pointer(Context), BOLD_HTTP_QUERY_STATUS_TEXT, @S[1], Size, Index) then
    begin
      SetLength(S, Size);
      raise Exception.CreateFmt('%s (%d)', [S, Status]);
    end;
  end;
  Result := TBoldDataBlock.Create;
  repeat
    Check(not BoldInternetQueryDataAvailable(Pointer(Context), Size, 0, 0));
    if Size > 0 then
    begin
      SetLength(S, Size);
      Check(not BoldInternetReadFile(Pointer(Context), @S[1], Size, Downloaded));
      Result.Write(S[1], Downloaded);
    end;
  until Size = 0;
end;

function TBoldWebConnection.Send(const Data: TBoldDataBlock): Integer;
var
  Request: HINTERNET;
  RetVal, Flags: DWord;
  P: Pointer;
  AcceptTypes: PChararr;
begin
  SetLength(AcceptTypes, 2);
  AcceptTypes[0] := PChar('application/octet-stream');
  AcceptTypes[1] := nil;
  Flags := BOLD_INTERNET_FLAG_KEEP_CONNECTION or BOLD_INTERNET_FLAG_NO_CACHE_WRITE;
  if FURLScheme = BOLD_INTERNET_SCHEME_HTTPS then
    Flags := Flags or BOLD_INTERNET_FLAG_SECURE;
  SetConnected(True);
  Request := BoldHttpOpenRequest(FInetConnect, 'POST', FURLSite, '', '', AcceptTypes, Flags, Integer(Self));
  Check(not Assigned(Request));
  while True do
  begin
    Check(not BoldHttpSendRequest(Request, '', Data.Memory, Data.Size));
    RetVal := BoldInternetErrorDlg(GetDesktopWindow(), Request, GetLastError,
      BOLD_FLAGS_ERROR_UI_FILTER_FOR_ERRORS or BOLD_FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS or
      BOLD_FLAGS_ERROR_UI_FLAGS_GENERATE_DATA, P);
    case RetVal of
      ERROR_SUCCESS: break;
      ERROR_CANCELLED: SysUtils.Abort;
      BOLD_ERROR_INTERNET_FORCE_RETRY: {Retry the operation};
    end;
  end;
  Result := Integer(Request)
end;

procedure TBoldWebConnection.SetConnected(Value: Boolean);
var
  AccessType: Integer;
begin
  if Value and not GetConnected then
  begin
    if Assigned(BeforeConnect) then
      BeforeConnect(Self);
    if Length(FProxy) > 0 then
      AccessType := BOLD_INTERNET_OPEN_TYPE_PROXY
    else
      AccessType := BOLD_INTERNET_OPEN_TYPE_PRECONFIG;
    FInetRoot := BoldInternetOpen(Agent, AccessType, FProxy, FProxyByPass, 0);
    if (BoldInternetAttemptConnect(0) <> ERROR_SUCCESS) then
      SysUtils.Abort;
    Check(not Assigned(FInetRoot));
    try
      FInetConnect := BoldInternetConnect(FInetRoot, FURLHost, FURLPort, FUserName, FPassword, BOLD_INTERNET_SERVICE_HTTP, 0, Cardinal(Self));
      Check(not Assigned(FInetConnect));
    except
      BoldInternetCloseHandle(FInetRoot);
    end;
  end
  else if not Value then
  begin
    if Assigned(FInetConnect) then
      BoldInternetCloseHandle(FInetConnect);
    FInetConnect := nil;
    if Assigned(FInetRoot) then
      BoldInternetCloseHandle(FInetRoot);
    FInetRoot := nil;
  end;
end;

procedure TBoldWebConnection.SetURL(const Value: string);
var
  URLComp: TURLComponents;
  P: PChar;
begin
  if (FURL <> Value) then
  begin
    SetConnected(False);
    if Value <> '' then
    begin
      FillChar(URLComp, SizeOf(URLComp), 0);
      URLComp.dwStructSize := SizeOf(URLComp);
      URLComp.dwSchemeLength := 1;
      URLComp.dwHostNameLength := 1;
      URLComp.dwURLPathLength := 1;
      P := PChar(Value);
      BoldInternetCrackUrl(P, 0, 0, URLComp);
      if not (URLComp.nScheme in [BOLD_INTERNET_SCHEME_HTTP, BOLD_INTERNET_SCHEME_HTTPS]) then
        raise Exception.CreateRes(@SInvalidURL);
      FURLScheme := URLComp.nScheme;
      FURLPort := URLComp.nPort;
      FURLHost := Copy(Value, URLComp.lpszHostName - P + 1, URLComp.dwHostNameLength);
      FURLSite := Copy(Value, URLComp.lpszUrlPath - P + 1, URLComp.dwUrlPathLength);
    end
    else
    begin
      FURLPort := 0;
      FURLHost := '';
      FURLSite := '';
      FURLScheme := 0;
    end;
    FURL := Value;
  end;
end;

end.
