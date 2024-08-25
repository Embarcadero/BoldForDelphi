
{ Global compiler directives }
{$include bold.inc}
unit BoldWinINet;

interface

uses
  BoldDefs,
  Windows,
  WinInet;

type
  PCharArr = array of PChar;
  HINTERNET = winINet.HINTERNET;
  TURLComponents = WinINet.TURLComponents;

const
  BOLD_INTERNET_OPEN_TYPE_PRECONFIG = WinINet.INTERNET_OPEN_TYPE_PRECONFIG;
  BOLD_INTERNET_FLAG_RELOAD = WinINet.INTERNET_FLAG_RELOAD;
  BOLD_HTTP_QUERY_STATUS_CODE = WinINet.HTTP_QUERY_STATUS_CODE;
  BOLD_HTTP_QUERY_FLAG_NUMBER = WinINet.HTTP_QUERY_FLAG_NUMBER;
  BOLD_HTTP_QUERY_STATUS_TEXT = WinINet.HTTP_QUERY_STATUS_TEXT;
  BOLD_INTERNET_FLAG_KEEP_CONNECTION = WinINet.INTERNET_FLAG_KEEP_CONNECTION;
  BOLD_INTERNET_FLAG_NO_CACHE_WRITE = WinINet.INTERNET_FLAG_NO_CACHE_WRITE;
  BOLD_INTERNET_SCHEME_HTTPS = WinINet.INTERNET_SCHEME_HTTPS;
  BOLD_INTERNET_FLAG_SECURE = WinINet.INTERNET_FLAG_SECURE;
  BOLD_INTERNET_SCHEME_HTTP = WinINet.INTERNET_SCHEME_HTTP;

  BOLD_FLAGS_ERROR_UI_FILTER_FOR_ERRORS = WinINet.FLAGS_ERROR_UI_FILTER_FOR_ERRORS;
  BOLD_FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS = WinINet.FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS;
  BOLD_FLAGS_ERROR_UI_FLAGS_GENERATE_DATA = WinINet.FLAGS_ERROR_UI_FLAGS_GENERATE_DATA;
  BOLD_ERROR_INTERNET_FORCE_RETRY = WinINet.ERROR_INTERNET_FORCE_RETRY;
  BOLD_INTERNET_OPEN_TYPE_PROXY = WinINet.INTERNET_OPEN_TYPE_PROXY;
  BOLD_INTERNET_SERVICE_HTTP = WinINet.INTERNET_SERVICE_HTTP;

function BoldInternetOpen(Agent: String; AccessType: integer; Proxy: string; ProxyByPass: String; Flags: integer): pointer;
function BoldInternetOpenUrl(iNet: Pointer; URL: string; Headers: String; Flags, Context: cardinal): Pointer;
function BoldInternetReadFile(hFile: Pointer; Buffer: Pointer; NumberOfBytesToRead: Cardinal; var NumberOfBytesRead: Cardinal): LongBool;
function BoldInternetCloseHandle(HINet: Pointer): LongBool;
function BoldHttpQueryInfo(hRequest: Pointer; InfoLevel: Cardinal; Buffer: Pointer; BufferLength: Cardinal; Reserved: Cardinal): LongBool;
function BoldInternetQueryDataAvailable(hFile: Pointer; var NumberOfBytesAvailable: Cardinal; flags: Cardinal; Context: Cardinal): LongBool;
function BoldHttpOpenRequest(hConnect: Pointer; Verb, ObjectName, Version, Referrer: String; AcceptTypes: PCharArr; Flags, Context: Cardinal): Pointer;
function BoldHttpSendRequest(hRequest: Pointer; Headers: string; Optional: Pointer; OptionalLength: Cardinal): LongBool;
function BoldInternetErrorDlg(hWnd: HWND; hRequest: HINTERNET; dwError, dwFlags: DWORD; var lppvData: Pointer): DWORD;
function BoldInternetAttemptConnect(dwReserved: DWORD): DWORD;
function BoldInternetConnect(hInet: HINTERNET; ServerName: string; nServerPort: INTERNET_PORT; Username: string; Password: string; dwService: DWORD; dwFlags: DWORD; dwContext: DWORD): HINTERNET;
function BoldInternetCrackUrl(Url: PChar; UrlLength, dwFlags: DWORD; var lpUrlComponents: TURLComponents): BOOL;

implementation

uses
  SysUtils,
  BoldUtils;

{$IFDEF BOLD_DELPHI}
function BoldInternetOpen(Agent: String; AccessType: integer; Proxy: string; ProxyByPass: String; Flags: integer): pointer;
begin
  result := InternetOpen(PChar(Agent), AccessType, PChar(Proxy), PChar(ProxyByPass), flags);
end;

function BoldInternetOpenUrl(iNet: Pointer; URL: string; Headers: String; Flags, Context: cardinal): Pointer;
begin
  result := InternetOpenURL(iNet, pChar(Url), PChar(Headers), length(Headers), Flags, Context);

end;

function BoldInternetReadFile(hFile: Pointer; Buffer: Pointer; NumberOfBytesToRead: Cardinal; var NumberOfBytesRead: Cardinal): LongBool;
begin
  result := InternetReadFile(hFile, Buffer, NumberOfBytesToRead, NumberOfBytesRead);
end;

function BoldInternetCloseHandle(HINet: Pointer): LongBool;
begin
  result := InternetCloseHandle(hInet);
end;

function BoldHttpQueryInfo(hRequest: Pointer; InfoLevel: Cardinal; Buffer: Pointer; BufferLength: Cardinal; Reserved: Cardinal): LongBool;
begin
  result := HTTPQueryInfo(hRequest, InfoLevel, Buffer, BufferLength, Reserved);
end;

function BoldInternetQueryDataAvailable(hFile: Pointer; var NumberOfBytesAvailable: Cardinal; flags: Cardinal; Context: Cardinal): LongBool;
begin
  result := InternetQueryDataAvailable(hFile, NumberOfBytesAvailable, flags, Context)
end;

function BoldHttpOpenRequest(hConnect: Pointer; Verb, ObjectName, Version, Referrer: String; AcceptTypes: PCharArr; Flags, Context: Cardinal): Pointer;
begin
  result := httpOpenRequest(hConnect, PChar(Verb), PChar(ObjectName), PChar(Version), PChar(Referrer), Pointer(AcceptTypes), Flags, Context)
end;

function BoldHttpSendRequest(hRequest: Pointer; Headers: string; Optional: Pointer; OptionalLength: Cardinal): LongBool;
begin
  result := HttpSendRequest(hRequest, PChar(Headers), length(Headers), Optional, OptionalLength);
end;

function BoldInternetErrorDlg(hWnd: HWND; hRequest: HINTERNET; dwError, dwFlags: DWORD; var lppvData: Pointer): DWORD;
begin
  result := InternetErrorDlg(hWnd, hRequest, dwError, dwFlags, lppvData);
end;

function BoldInternetAttemptConnect(dwReserved: DWORD): DWORD;
begin
  result := InternetAttemptConnect(dwReserved);
end;

function BoldInternetConnect(hInet: HINTERNET; ServerName: string; nServerPort: INTERNET_PORT; Username: string; Password: string; dwService: DWORD; dwFlags: DWORD; dwContext: DWORD): HINTERNET;
begin
  result := InternetConnect(hINet, PChar(ServerName), nServerPort, PChar(UserName), PChar(Password), dwService, dwFlags, dwContext);
end;

function BoldInternetCrackUrl(Url: PChar; UrlLength, dwFlags: DWORD; var lpUrlComponents: TURLComponents): BOOL;
begin
  result := InternetCrackURL(URL, UrlLength, dwFlags, lpUrlComponents);
end;
{$ENDIF}

{$IFDEF BOLD_BCB}
function BoldInternetOpen(Agent: String; AccessType: integer; Proxy: string; ProxyByPass: String; Flags: integer): pointer;
begin
  raise EBoldFeatureNotImplementedYet.Create('BoldInternetOpen now yet implemented in Bold for C++');
end;

function BoldInternetOpenUrl(iNet: Pointer; URL: string; Headers: String; Flags, Context: cardinal): Pointer;
begin
  raise EBoldFeatureNotImplementedYet.Create('BoldInternetOpenUrl now yet implemented in Bold for C++');
end;

function BoldInternetReadFile(hFile: Pointer; Buffer: Pointer; NumberOfBytesToRead: Cardinal; var NumberOfBytesRead: Cardinal): LongBool;
begin
  raise EBoldFeatureNotImplementedYet.Create('BoldInternetReadFile now yet implemented in Bold for C++');
end;

function BoldInternetCloseHandle(HINet: Pointer): LongBool;
begin
  raise EBoldFeatureNotImplementedYet.Create('BoldInternetCloseHandle now yet implemented in Bold for C++');
end;

function BoldHttpQueryInfo(hRequest: Pointer; InfoLevel: Cardinal; Buffer: Pointer; BufferLength: Cardinal; Reserved: Cardinal): LongBool;
begin
  raise EBoldFeatureNotImplementedYet.Create('BoldHttpQueryInfo now yet implemented in Bold for C++');
end;

function BoldInternetQueryDataAvailable(hFile: Pointer; var NumberOfBytesAvailable: Cardinal; flags: Cardinal; Context: Cardinal): LongBool;
begin
  raise EBoldFeatureNotImplementedYet.Create('BoldInternetQueryDataAvailable now yet implemented in Bold for C++');
end;

function BoldHttpOpenRequest(hConnect: Pointer; Verb, ObjectName, Version, Referrer: String; AcceptTypes: PCharArr; Flags, Context: Cardinal): Pointer;
begin
  raise EBoldFeatureNotImplementedYet.Create('BoldHttpOpenRequest now yet implemented in Bold for C++');
end;

function BoldHttpSendRequest(hRequest: Pointer; Headers: string; Optional: Pointer; OptionalLength: Cardinal): LongBool;
begin
  raise EBoldFeatureNotImplementedYet.Create('BoldHttpSendRequest now yet implemented in Bold for C++');
end;

function BoldInternetErrorDlg(hWnd: HWND; hRequest: HINTERNET; dwError, dwFlags: DWORD; var lppvData: Pointer): DWORD;
begin
  raise EBoldFeatureNotImplementedYet.Create('BoldInternetErrorDlg now yet implemented in Bold for C++');
end;

function BoldInternetAttemptConnect(dwReserved: DWORD): DWORD;
begin
  raise EBoldFeatureNotImplementedYet.Create('BoldInternetAttemptConnect now yet implemented in Bold for C++');
end;

function BoldInternetConnect(hInet: HINTERNET; ServerName: string; nServerPort: INTERNET_PORT; Username: string; Password: string; dwService: DWORD; dwFlags: DWORD; dwContext: DWORD): HINTERNET;
begin
  raise EBoldFeatureNotImplementedYet.Create('BoldInternetConnect now yet implemented in Bold for C++');
end;

function BoldInternetCrackUrl(Url: PChar; UrlLength, dwFlags: DWORD; var lpUrlComponents: TURLComponents): BOOL;
begin
  raise EBoldFeatureNotImplementedYet.Create('BoldInternetCrackUrl now yet implemented in Bold for C++');
end;
{$ENDIF}

end.
