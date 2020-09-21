unit BoldSupportConst;

interface

resourcestring
//BoldISODateTime
  sInvalidDateFormatFormat = 'ParseISODate: Invalid date format %s. Should be YYYY-MM-DD';
  sInnvalidDateFormatLargeMonth = 'ParseISODate: Invalid date format %s. month > 12';
  sInvalidDateFormatSmallMonth = 'ParseISODate: Invalid date format %s. month < 1';
  sInvalidDateFormatSmallDay = 'ParseISODate: Invalid date format %s. date < 1';
  sInvalidDateFormatBadDay = 'ParseISODate: Invalid date format %s. there is only %d days in month %d';
  sInvalidDateTimeFormat = 'ParseISODateTime: Invalid datatime format %s. Should be YYYY-MM-DD HH:MM[:SS]';
  sInvalidTimeFormat = 'ParseISOTime: Invalid time format %s. Should be HH:MM[:SS]';
  sInvalidTimeFormatLargeHour = 'ParseISOTime: Invalid time format %s. h > 23';
  sInvalidTimeFormatLargeMinute = 'ParseISOTime: Invalid time format %s. m > 59';
  sInvalidTimeFormatLargeSecond = 'ParseISOTime: Invalid time format %s. s > 59';

implementation

end.
