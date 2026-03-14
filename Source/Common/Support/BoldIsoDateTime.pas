unit BoldIsoDateTime;

interface

uses
  BoldDefs,
  BoldUtils;

type
  EBoldIsoDateTimeFormatError = class(EBold);

// Parses a date string in 'YYYY-MM-DD' format.
// Raises EBoldIsoDateTimeFormatError on invalid format or date.
function ParseISODate(s: string): TDateTime;

// Parses a date-time string in 'YYYY-MM-DD HH:NN' or 'YYYY-MM-DD HH:NN:SS' format.
// Raises EBoldIsoDateTimeFormatError on invalid format or date/time.
function ParseISODateTime(s: string): TDateTime;

// Parses a time string in 'HH:NN' or 'HH:NN:SS' format.
// Raises EBoldIsoDateTimeFormatError on invalid format or time.
function ParseISOTime(str: string): TDateTime;

implementation

uses
  SysUtils,
  BoldCoreConsts;

var
  IsoFormatSettings: TFormatSettings;

function ParseISODate(s: string): TDateTime;
begin
  if not TryStrToDate(s, Result, IsoFormatSettings) then
    raise EBoldIsoDateTimeFormatError.CreateFmt(sInvalidDateFormatFormat, [s]);
end;

function ParseISODateTime(s: string): TDateTime;
begin
  if not TryStrToDateTime(s, Result, IsoFormatSettings) then
    raise EBoldIsoDateTimeFormatError.CreateFmt(sInvalidDateTimeFormat, [s]);
end;

function ParseISOTime(str: string): TDateTime;
begin
  if not TryStrToTime(str, Result, IsoFormatSettings) then
    raise EBoldIsoDateTimeFormatError.CreateFmt(sInvalidTimeFormat, [str]);
end;

initialization
  IsoFormatSettings := TFormatSettings.Create;
  // Configure for specific formats expected by the original unit.
  IsoFormatSettings.DateSeparator := '-';
  IsoFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  IsoFormatSettings.LongDateFormat := 'yyyy-mm-dd';
  IsoFormatSettings.TimeSeparator := ':';
  IsoFormatSettings.ShortTimeFormat := 'hh:nn';
  IsoFormatSettings.LongTimeFormat := 'hh:nn:ss';
  // Default DateTimeSeparator (space) is used implicitly.

finalization

end.
