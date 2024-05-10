unit BoldGUIDUtils;

interface
function BoldCreateGUIDAsString(StripBrackets: Boolean = false): string;
function BoldCreateGUIDWithBracketsAsString: string;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldDefs,
  ActiveX;

function BoldCreateGUIDWithBracketsAsString: string;
begin
  result := BoldCreateGUIDAsString;
end;

function BoldCreateGUIDAsString(StripBrackets: Boolean): string;
var
  GUID: TGUID;
  P: PWideChar;
begin
  result := '';
  if (CoCreateGuid (GUID) = S_OK) and
    (StringFromCLSID(GUID, P) and $80000000 = 0) then
  begin
    Result := P;
    CoTaskMemFree(P);
  end;
  if StripBrackets then
    result := copy(result, 2, length(result) - 2);
end;

end.