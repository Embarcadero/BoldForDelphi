
{ Global compiler directives }
{$include bold.inc}
unit BoldSSExcept;

interface

uses
  SysUtils;

type
  SSExceptionError = (
                     SSExceptionLexFileOpen,
                     SSExceptionLexLexemeLength,
                     SSExceptionOutOfMemory,
                     SSExceptionLexFileRead,
                     SSExceptionLexBadTable,
                     SSExceptionMissingPart,
                     SSExceptionBadList,
                     SSExceptionStackTop,
                     SSExceptionStackPop,
                     SSExceptionLexError,
                     SSExceptionYaccMissingTable,
                     SSExceptionYaccMissingLexer,
                     SSExceptionYaccFileOpen,
                     SSExceptionYaccRead,
                     SSExceptionYaccTableSize,
                     SSExceptionYaccParse,
                     SSExceptionYaccEof,
                     SSExceptionYaccElement,
                     SSExceptionYaccSyncErrToken,
                     SSExceptionYaccSyncErrEof,
                     SSExceptionLexFindResource,
                     SSExceptionYaccFindResource,
                     SSExceptionLexBadResource,
                     SSExceptionYaccBadResource
                    );

type
  SSException = class(EAbort)
  public
    Position: integer;
    Id: SSExceptionError;
    constructor Create(TheId: SSExceptionError; FmtStr: string);
    constructor CreateName(TheId: SSExceptionError; FmtStr: string; Name: PChar);
    constructor CreateLong(TheId: SSExceptionError; TheFmtStr: string; TheLong: Longint);
    constructor CreateLongLongNameLen(TheId: SSExceptionError; FmtStr: string;
                  Long0, Long1: Longint; TheName: PChar; TheLen: Word);
  end;

implementation

constructor SSException.Create(TheId: SSExceptionError; FmtStr: string);
begin
  inherited Create(FmtStr);
  Id := TheId;
end;

constructor SSException.CreateName(TheId: SSExceptionError; FmtStr: string; Name: PChar);
begin
  inherited Create(Format(FmtStr, [string(Name)]));
  Id := TheId;
  Position := 0;
end;

constructor SSException.CreateLong(TheId: SSExceptionError; TheFmtStr: string; TheLong: Integer);
begin
  inherited Create(Format(StringReplace(TheFmtStr, '%ld', '%d', [rfReplaceAll]), [TheLong]));
  Id := TheId;
  Position := 0;
end;

constructor SSException.CreateLongLongNameLen(TheId: SSExceptionError;
  FmtStr: string; Long0, Long1: Integer; TheName: PChar; TheLen: Word);
var
  Name: PChar;
begin
  GetMem(Name, TheLen + 1);
  StrMove(Name, TheName, TheLen);
  inherited Create(Format(StringReplace(FmtStr, '%ld', '%d', [rfReplaceAll]), [Long0, Long1, string(Name)]));
  FreeMem(Name, TheLen + 1);
  Id := TheId;
  Position := Long1;
end;

end.
