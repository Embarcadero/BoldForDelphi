
{ Global compiler directives }
{$include bold.inc}
unit BoldWScanner;

interface

uses
  Classes,
  BoldDefs;

const
  DefaultBufferSize = 8 * 1024; {8K}

  ttNull = #0; {token types}
  ttIdentifier = #1;
  ttString = #2;
  ttInteger = #3;
  ttFloat = #4;
  ttLE = #5;               { <= }
  ttGE = #6;               { >= }
  ttNE = #7;               { <> }
  ttDotDot = #8;           { .. }
  ttAssign = #9;           { := }

type
  { forward declarations }
  TScanner = class;

  TTokenType = Char;

  { TScanner }
  TScanner = class
  private
    fBuffer: PChar;       { input buffer }
    fBufPtr: PChar;       { current read position }
    fBufEnd: PChar;       { end of buffer }
    fBufSize: Cardinal;
    fStream: TStream;
    fToken: string;
    fTokenType: TTokenType;
    fNewLine: Boolean;
    fPosition: LongInt;
    fOnEndOfStream: TNotifyEvent;
    fOnToken: TNotifyEvent;
    procedure SetBufSize(Value: Cardinal);
  protected
    procedure FillBuffer;
    function GetChar(var Ch: Char): Boolean;
    property Stream: TStream read fStream;
    function NextToken: Boolean;
    procedure DoToken; virtual;
    procedure DoEndOfStream; virtual;
    procedure GetNumber(FirstChar: Char);
    procedure CheckSymbol(Ch1: Char);
    procedure GetIdentifier(FirstChar: Char);
    procedure GetString(Delim: Char);
    function SkipWhiteSpace(var Ch: Char): Boolean;
    procedure SkipComment(CommentType: Char);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Scan(Stream: TStream);
    function IsIdentifier(Check: string): Boolean;
    property Position: LongInt read fPosition;
    property NewLine: Boolean read fNewLine;
    property Token: string read fToken;
    property TokenType: TTokenType read fTokenType;
    property BufferSize: Cardinal read fBufSize write SetBufSize default DefaultBufferSize;
    property OnEndOfStream: TNotifyEvent read fOnEndOfStream write fOnEndOfStream;
    property OnToken: TNotifyEvent read fOnToken write fOnToken;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

constructor TScanner.Create;
begin
  inherited Create;
  fBufSize := DefaultBufferSize;
end;

destructor TScanner.Destroy;
begin
  {free buffer}
  FreeMem(fBuffer);
  inherited Destroy;
end;

procedure TScanner.SetBufSize(Value: Cardinal);
begin
  if fBuffer = nil then
    fBufSize := Value
end;

{ Read another buffer from the stream. }
procedure TScanner.FillBuffer;
begin
  fBufEnd := fBuffer + Stream.Read(fBuffer^, BufferSize);
  fBufPtr := fBuffer;
end;

{True for success, False for end of file. }
function TScanner.GetChar(var Ch: Char): Boolean;
begin
  if fBufPtr >= fBufEnd then
    FillBuffer;
  if fBufPtr >= fBufEnd then
    Result := False
  else
  begin
    Ch := fBufPtr^;
    Inc(fBufPtr);
    Result := True;
  end;
end;

procedure TScanner.SkipComment(CommentType: Char);
var
  Ch: Char;
begin
  case CommentType of
    '{':
      while GetChar(ch) and (Ch <> '}') do
        ;
    '/':
      begin
        while GetChar(Ch) and not CharInSet(Ch, [BOLDLF, BOLDCR]) do
          ;
        Dec(fBufptr);
      end;
    '*':
      begin
        while GetChar(Ch) do
          if Ch = '*' then
          begin
            if not GetChar(Ch) then
              Exit
            else if Ch = ')' then
              Exit
            else
              Dec(fBufPtr);
          end;
      end;
    else
      raise Exception.CreateFmt('Cannot happen, CommentType=%s', [CommentType]);
  end;
end;

function TScanner.SkipWhiteSpace(var Ch: Char): Boolean;
var
  Ch2: Char;
begin
  Result := False;

  if not GetChar(ch) then
    Exit;

  while True do
  begin
    case Ch of
    BOLDLF, BOLDCR:
      fNewLine := True;
    BOLDNULL..#9, #11, #12, #14..' ':
      ; { skip the control or space character }
    '{':
      SkipComment(Ch);
    '(':
      begin
        if not GetChar(Ch2) then
          Exit;
        if Ch2 = '*' then
          SkipComment(Ch2)
        else
        begin
          Dec(fBufPtr);
          Break;
        end;
      end;
    '/':
      begin
        if not GetChar(Ch2) then
          Exit;
        if Ch2 = '/' then
          SkipComment(Ch2)
        else
        begin
          Dec(fBufPtr);
          Break;
        end;
      end;
    else
      Break;
    end;
    if not GetChar(Ch) then
      Exit;
  end;
  Result := True;
end;

procedure TScanner.GetString(Delim: Char);
var
  Ch, Ch2: Char;
begin
  fTokenType := ttString;
  {read ahead}
  while GetChar(Ch) do
  begin
    fToken := fToken + Ch;
    if CharInSet(Ch, [BOLDLF, BOLDCR]) then
    begin
      Dec(fBufPtr);
      Exit;
    end;
    {be greedy, eat up all delimeter characters}
    if Ch = Delim then
    begin
      if not GetChar(Ch2) then
        Exit;
      if Ch2 <> Delim then
      begin
        {push character back}
        Dec(fBufPtr);
        Delete(fToken, Length(fToken) - 1, 1);
        Break;
      end;
    end;
  end;
end;

procedure TScanner.GetIdentifier(FirstChar: Char);
var
  Ch: Char;
begin
  fToken := FirstChar;
  fTokenType := ttIdentifier;
  while GetChar(Ch) do
  begin
    if not CharInSet(Ch, ['a'..'z', 'A'..'Z', '0'..'9', '_', '.']) then
    begin
      { put it back}
      Dec(fBufPtr);
      Break;
    end;
    fToken := fToken + Ch;
  end;
end;


type
  TDoubleChar = record
    C1, C2: Char;
    TT: TTokenType;
  end;
const
  DoubleChars: array[1..7] of TDoubleChar =
  (
    (C1: ':'; C2: '='; TT: ttAssign),
    (C1: '<'; C2: '='; TT: ttLE),
    (C1: '<'; C2: '>'; TT: ttNE),
    (C1: '>'; C2: '='; TT: ttGE),
    (C1: '('; C2: '.'; TT: '['),
    (C1: '.'; C2: ')'; TT: ']'),
    (C1: '.'; C2: '.'; TT: ttDotDot)
 );

procedure TScanner.CheckSymbol(Ch1: Char);
var
  Ch2: Char;
  I: Integer;
begin
  if not GetChar(Ch2) then
    Exit;
  for I := Low(DoubleChars) to High(DoubleChars) do
    with DoubleChars[I] do
     if (C1 = Ch1) and (C2 = Ch2) then
     begin
       fTokenType := TT;
       fToken := Ch1 + Ch2;
       Exit;
     end;
  fToken := Ch1;
  fTokenType := Ch1;
  Dec(fBufPtr);  { put back Ch2 }
end;

procedure TScanner.GetNumber(FirstChar: Char);
var
  Ch: Char;
begin
  fToken := FirstChar;
  fTokenType := ttInteger;
  while GetChar(Ch) and CharInSet(Ch, ['0'..'9']) do
    fToken := fToken + Ch;

  if Ch = '.' then
  begin
    fTokenType := ttFloat;
    fToken := fToken + Ch;
    while GetChar(Ch) and CharInSet(Ch, ['0'..'9']) do
      fToken := fToken + Ch;
  end;

  if CharInSet(Ch, ['e', 'E']) then
  begin
    fTokenType := ttFloat;
    fToken := fToken + Ch;
    if GetChar(Ch) and CharInSet(Ch, ['+', '-']) then
      fToken := fToken + Ch;
    while CharInSet(Ch, ['0'..'9']) do
    begin
      fToken := fToken + Ch;
      if not GetChar(Ch) then
        Break;
    end;
  end;
end;

{ True for success, False for end of file. }
function TScanner.NextToken: Boolean;
var
  Ch: Char;
begin
  fNewLine := False;
  Result := False;
  if not SkipWhiteSpace(Ch) then
    Exit;
  fPosition := Stream.Position - (fBufEnd-fBufPtr) - 1;

  case Ch of
    '''', '"':
      GetString(Ch);
    'a'..'z', 'A'..'Z', '_':
      GetIdentifier(Ch);
    '0'..'9':
      GetNumber(Ch);
    '.', ':', '<', '>', '(':
      CheckSymbol(Ch);
    else
    begin
      fToken := Ch;
      fTokenType := Ch;
    end;
  end;
  Result := True;
end;

function TScanner.IsIdentifier(Check: string): Boolean;
begin
  Result := (TokenType = ttIdentifier) and (CompareText(Token, Check) = 0)
end;

procedure TScanner.DoToken;
begin
  if Assigned(fOnToken) then
    fOnToken(Self);
end;

procedure TScanner.DoEndOfStream;
begin
  if Assigned(fOnEndOfStream) then
    fOnEndOfStream(Self);
end;

procedure TScanner.Scan(Stream: TStream);
var
  TmpBuf: PChar;
begin
  fStream := Stream;
  try
    GetMem(fBuffer, BufferSize);
    try
      fNewLine := True;
      fPosition := Stream.Position;
      fToken := '';
      FillBuffer;
      while NextToken do
        DoToken;
      DoEndOfStream;
    finally
      TmpBuf := fBuffer;
      fBuffer := nil;
      FreeMem(TmpBuf);
    end;
  finally
    fStream := nil;
  end;
end;

end.
