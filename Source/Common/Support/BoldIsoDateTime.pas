unit BoldIsoDateTime;

interface

uses
  BoldDefs;

function ParseISODate(s: string): TDateTime;
function ParseISODateTime(s: string): TDateTime;
function ParseISOTime(str: string): TDateTime;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldSupportConst;

function MatchPattern(pattern, s: string): boolean;
var
  i: integer;
begin
  result := length(pattern) = length(s);
  if result then
    for i := 1 to length(pattern) do
      case pattern[i] of
        '#': result := result and (s[i] in ['0'..'9']);
        else result := result and (s[i] = pattern[i]);
      end;
end;

function ParseISODate(s: string): TDateTime;
var
  y, m, d: integer;
const
  daysPerMonth: array[1..12] of byte = (31, 29, 31, 30, 31, 30,
                                        31, 31, 30, 31, 30, 31);
begin
  if not MatchPattern('####-##-##', s) then
    raise EBold.CreateFmt(sInvalidDateFormatFormat, [s]);
  y := StrToInt(copy(s, 1, 4));
  m := StrToInt(copy(s, 6, 2));
  if m > 12 then
    raise EBold.CreateFmt(sInnvalidDateFormatLargeMonth, [s]);
  if m < 1 then
    raise EBold.CreateFmt(sInvalidDateFormatSmallMonth, [s]);
  d := StrToInt(copy(s, 9, 2));
  if d < 1 then
    raise EBold.CreateFmt(sInvalidDateFormatSmallDay, [s]);
  if d > dayspermonth[m] then
    raise EBold.CreateFmt(sInvalidDateFormatBadDay, [s, daysPerMonth[m], m]);
  result := EncodeDate(y, m, d);
end;

function ParseISODateTime(s: string): TDateTime;
begin
  if not matchPattern('####-##-## ##:##', s) and not matchPattern('####-##-## ##:##:##', s) then
    raise EBold.CreateFmt(sInvalidDateTimeFormat, [s]);
  result := ParseIsoDate(copy(s, 1, 10)) + ParseIsoTime(copy(s, 12, 8));
end;

function ParseISOTime(str: string): TDateTime;
var
  h, m, s: integer;
begin
  if not MatchPattern('##:##:##', str) and not MatchPattern('##:##', str) then
    raise EBold.CreateFmt(sInvalidTimeFormat, [str]);
  h := StrToInt(copy(str, 1, 2));
  if h > 23 then
    raise EBold.CreateFmt(sInvalidTimeFormatLargeHour, [str]);
  m := StrToInt(copy(str, 4, 2));
  if m > 59 then
    raise EBold.CreateFmt(sInvalidTimeFormatLargeMinute, [str]);
  s := StrToInt(copy(str, 7, 2));
  if s > 59 then
    raise EBold.CreateFmt(sInvalidTimeFormatLargeSecond, [str]);
  result := EncodeTime(h, m, s, 0);
end;

end.
