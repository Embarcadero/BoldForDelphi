
{ Global compiler directives }
{$include bold.inc}
unit BoldIsoDateTime;

interface

uses
  BoldDefs,
  Controls; // for TDate

function ParseISODate(const s: string): TDateTime;
function ParseISODateTime(const s: string): TDateTime;
function ParseISOTime(const str: string): TDateTime;

function AsISODateTime(d: TDateTime): string;
function AsISODate(d: TDate): string;
function AsISOTime(t: TTime): string;

function AsISODateTimeMS(d: TDateTime): string;
function AsISOTimeMS(t: TTime): string;

const
  cIsoDateFormat = 'yyyy-mm-dd';
  cIsoTimeFormat = 'hh:mm:ss';
  cIsoTimeFormatMS = 'hh:mm:ss:zzz';
  cIsoDateTimeSeparator = 'T';
  cIsoDateTimeFormat = cIsoDateFormat + '"' + cIsoDateTimeSeparator + '"' + cIsoTimeFormat; // '2018-12-31T11:50:00';
  cIsoDateTimeFormatMS = cIsoDateFormat + '"' + cIsoDateTimeSeparator + '"' + cIsoTimeFormatMS; // '2018-12-31T11:50:00:123';

implementation

uses
  SysUtils,
  BoldUtils;

var
  FormatSettings: TFormatSettings;

function MatchPattern(const pattern, s: string): boolean;
var
  i: integer;
begin
  result := length(pattern) = length(s);
  if result then
    for i := 1 to length(pattern) do
      case pattern[i] of
        '#': result := result and CharInSet(s[i], ['0'..'9']);
        else result := result and (s[i] = pattern[i]);
      end;
end;

function ParseISODate(const s: string): TDateTime;
var
  y, m, d: integer;
const
  daysPerMonth: array[1..12] of byte = (31, 29, 31, 30, 31, 30,
                                        31, 31, 30, 31, 30, 31);
begin
  if not MatchPattern('####-##-##', s) then
    raise EBold.CreateFmt('ParseISODate: Invalid date format %s. Should be YYYY-MM-DD', [s]);
  y := StrToInt(copy(s, 1, 4));
  m := StrToInt(copy(s, 6, 2));
  if m > 12 then
    raise EBold.CreateFmt('ParseISODate: Invalid date format %s. month > 12', [s]);
  if m < 1 then
    raise EBold.CreateFmt('ParseISODate: Invalid date format %s. month < 1', [s]);
  d := StrToInt(copy(s, 9, 2));
  if d < 1 then
    raise EBold.CreateFmt('ParseISODate: Invalid date format %s. date < 1', [s]);
  if d > dayspermonth[m] then
    raise EBold.CreateFmt('ParseISODate: Invalid date format %s. there is only %d days in month %d', [s, daysPerMonth[m], m]);
  result := EncodeDate(y, m, d);
end;

function ParseISODateTime(const s: string): TDateTime;
begin
     if not matchPattern('####-##-## ##:##', s)
    and not matchPattern('####-##-## ##:##:##', s)
    and not matchPattern('####-##-## ##:##:##:###', s)
    and not matchPattern('####-##-##T##:##:##:###', s) then
    raise EBold.CreateFmt('ParseISODateTime: Invalid datatime format %s. Should be YYYY-MM-DD HH:MM[:SS][:ZZZ]', [s]);
  result := ParseIsoDate(copy(s, 1, 10)) + ParseIsoTime(copy(s, 12, MaxInt));
end;

function ParseISOTime(const str: string): TDateTime;
var
  h, m, s, z: integer;
begin
  if not MatchPattern('##:##:##:###', str)
    and not MatchPattern('##:##:##.###', str)
    and not MatchPattern('##:##:##', str)
    and not MatchPattern('##:##', str) then
    raise EBold.CreateFmt('ParseISOTime: Invalid time format %s. Should be HH:MM[:SS:ZZZ]', [str]);
  h := StrToInt(copy(str, 1, 2));
  if h > 23 then
    raise EBold.CreateFmt('ParseISODate: Invalid date format %s. h > 23', [str]);
  m := StrToInt(copy(str, 4, 2));
  if m > 59 then
    raise EBold.CreateFmt('ParseISODate: Invalid date format %s. m > 59', [str]);
  s := StrToInt(copy(str, 7, 2));
  if s > 59 then
    raise EBold.CreateFmt('ParseISODate: Invalid date format %s. s > 59', [str]);
  z := StrToIntDef(copy(str, 10, 3),0);
  result := EncodeTime(h, m, s, z);
end;

function AsISODateTime(d: TDateTime): string;
begin
  result := formatDateTime(cIsoDateTimeFormat, d, FormatSettings);
end;

function AsISODate(d: TDate): string;
begin
  result := formatDateTime(cIsoDateFormat, d, FormatSettings);
end;

function AsISOTime(t: TTime): string;
begin
  result := formatDateTime(cIsoTimeFormat, t, FormatSettings);
end;

function AsISODateTimeMS(d: TDateTime): string;
begin
  result := formatDateTime(cIsoDateTimeFormatMS, d, FormatSettings);
end;

function AsISOTimeMS(t: TTime): string;
begin
  result := formatDateTime(cIsoTimeFormatMS, t, FormatSettings);
end;

initialization
  FormatSettings := TFormatSettings.Create;
  FormatSettings.DateSeparator := '-';
  FormatSettings.TimeSeparator := ':'

end.
