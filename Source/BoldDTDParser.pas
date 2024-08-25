
{ Global compiler directives }
{$include bold.inc}
unit BoldDTDParser;

interface

type

  TBoldDTDToken = (dtSpace, dtName, dtEntityDecl, dtElementDecl, dtAttlistDecl,
    dtEndDecl {>}, dtOptional {?}, dtZeroOrMore {*}, dtOneOrMore {+},
    dtChoice {|}, dtComma {,}, dtOpenParen {(}, dtCloseParen {)},
    dtAnyContent {ANY}, dtEmptyContent {EMPTY}, dtPCDataDecl {#PCData},
    dtEOF, dtComment, dtStringLiteral, dtPercent, dtFixedDecl, dtImpliedDecl,
    dtRequiredDecl, dtSemicolon);

  TBoldDTDNameEvent = procedure (Name: string) of object;
  TBoldDTDEvent = procedure of object;

  TBoldDTDScanner = class
  private
    fDTD: string;
    fPos: integer;
    fLastTokenText: string;
    fLastToken: TBoldDTDToken;
    function TryToken(TokenStr: string; Token: TBoldDTDToken): Boolean;
    procedure SpaceToken;
    procedure DeclarationToken;
    procedure NameToken;
    procedure OneCharToken(Token: TBoldDTDToken);
    procedure HashDeclToken;
    procedure EOFToken;
    function TryComment: Boolean;
    procedure StringLiteralToken;
  public
    constructor Create(DTD: string);
    procedure NextToken;
    property LastToken: TBoldDTDToken read fLastToken;
    property LastTokenText: string read fLastTokenText;
  end;

  TBoldDTDParser = class
  private
    fScanner: TBoldDTDScanner;
    fOnStartElement: TBoldDTDNameEvent;
    fOnEndElement: TBoldDTDEvent;
    fOnFoundCpName: TBoldDTDNameEvent;
    procedure AssertToken(Token: TBoldDTDToken);
    procedure AssertNonterminal(Value: Boolean);
    procedure OptionalToken(Token: TBoldDTDToken);
    function ParseToken(Token: TBoldDTDToken): Boolean;
    function Preprocess(DTD: string): string;
    procedure ParseExtSubsetDecl;
    function ParseMarkupDecl: Boolean;
    function ParseElementDecl: Boolean;
    function ParseAttListDecl: Boolean;
    function ParseEntityDecl: Boolean;
    function ParseContentSpec: Boolean;
    function ParseMixedOrChildren: Boolean;
    function ParseMixed2: Boolean;
    function ParseChildren2: Boolean;
    function ParseCp: Boolean;
    function ParseChoiceOrSeq: Boolean;
    procedure StartElement(Name: string);
    procedure EndElement;
    procedure FoundCpName(Name: string);
  public
    procedure Parse(DTD: string);
    property OnStartElement: TBoldDTDNameEvent read fOnStartElement write fOnStartElement;
    property OnEndElement: TBoldDTDEvent read fOnEndElement write fOnEndElement;
    property OnFoundCpName: TBoldDTDNameEvent read fOnFoundCpName write fOnFoundCpName;
  end;

implementation

uses
  Classes,
  SysUtils,
  BoldDefs,
  BoldStringList,
  BoldUtils;

const
  SpaceChars = [#$20, #$9, #$D, #$A];
  NameChars = ['a'..'z', 'A'..'Z', '0'..'9', '.', '_', '-', ':']; 

{ TBoldDTDParser }

procedure TBoldDTDParser.AssertNonterminal(Value: Boolean);
begin
  if not Value then
    raise EBold.Create('Parse error...');
end;

procedure TBoldDTDParser.AssertToken(Token: TBoldDTDToken);
begin
  if fScanner.LastToken <> Token then
    raise EBold.CreateFmt('Unexpected token: "%s"', [fScanner.fLastTokenText]);
  fScanner.NextToken;
end;

procedure TBoldDTDParser.EndElement;
begin
  if assigned(fOnEndElement) then
    fOnEndElement;
end;

procedure TBoldDTDParser.FoundCpName(Name: string);
begin
  if assigned(fOnFoundCpName) then
    fOnFoundCpName(Name);
end;

procedure TBoldDTDParser.OptionalToken(Token: TBoldDTDToken);
begin
  if fScanner.LastToken = Token then
    fScanner.NextToken;
end;

procedure TBoldDTDParser.Parse(DTD: string);
begin
  DTD := Preprocess(DTD);
  fScanner := TBoldDTDScanner.Create(DTD);
  fScanner.NextToken;
  ParseExtSubsetDecl;
  if fScanner.LastToken <> dtEOF then
    raise EBold.CreateFmt('%s: terminated abnormally at: %s', [classname, fScanner.LastTokenText]);
end;


function TBoldDTDParser.ParseAttListDecl: Boolean;
begin
  if ParseToken(dtAttlistDecl) then
  begin
    while fScanner.LastToken <> dtEndDecl do
      fScanner.NextToken;
    fScanner.NextToken;




    result := true;
  end else
    result := false;
end;

function TBoldDTDParser.ParseChildren2: Boolean;
begin





  result := ParseChoiceOrSeq;
  if result then
    if ParseToken(dtOptional) or ParseToken(dtZeroOrMore) or ParseToken(dtOneOrMore) then
      ;
end;

function TBoldDTDParser.ParseChoiceOrSeq: Boolean;
begin



  AssertNonterminal(ParseCp);
  OptionalToken(dtSpace);
  if fScanner.LastToken = dtChoice then
  begin
    while fScanner.LastToken = dtChoice do
    begin
      fScanner.NextToken;
      OptionalToken(dtSpace);
      AssertNonterminal(ParseCp);
      OptionalToken(dtSpace);
    end;
    AssertToken(dtCloseParen);
  end
  else if fScanner.LastToken = dtComma then
  begin
    while fScanner.LastToken = dtComma do
    begin
      fScanner.NextToken;
      OptionalToken(dtSpace);
      AssertNonterminal(ParseCp);
      OptionalToken(dtSpace);
    end;
    AssertToken(dtCloseParen);
  end
  else
  begin
    AssertToken(dtCloseParen);
  end;
  result := true;
end;

function TBoldDTDParser.ParseContentSpec: Boolean;
begin
   result := ParseToken(dtEmptyContent) or
             ParseToken(dtAnyContent) or
             ParseMixedOrChildren;
end;

function TBoldDTDParser.ParseCp: Boolean;
begin
  if fScanner.LastToken = dtName then
  begin
    FoundCpName(fScanner.LastTokenText);
    fScanner.NextToken;
    result := true;
  end
  else if ParseToken(dtOpenParen) then
  begin
    OptionalToken(dtSpace);
    AssertNonterminal(ParseChoiceOrSeq);
    result := true;
  end
  else
    result := false;

  if result then
    if ParseToken(dtOptional) or ParseToken(dtZeroOrMore) or ParseToken(dtOneOrMore) then
      ;
end;

function TBoldDTDParser.ParseElementDecl: Boolean;
begin
  if ParseToken(dtElementDecl) then
  begin
    AssertToken(dtSpace);
    if fScanner.LastToken = dtName then
      StartElement(fScanner.LastTokenText)
    else
      raise EBold.CreateFmt('%s.ParseElementDecl: Unexpected token %s, expected name', [classname, fScanner.LastTokenText]);
    AssertToken(dtName);
    AssertToken(dtSpace);
    AssertNonterminal(ParseContentspec);
    OptionalToken(dtSpace);
    AssertToken(dtEndDecl);
    EndElement;
    result := true;
  end else
    result := false;
end;

function TBoldDTDParser.ParseEntityDecl: Boolean;
begin



  if ParseToken(dtEntityDecl) then
  begin
    while fScanner.LastToken <> dtEndDecl do
      fScanner.NextToken;
    fScanner.NextToken;
  
    result := true;
  end else
    result := false;

end;

procedure TBoldDTDParser.ParseExtSubsetDecl;
begin
  while (ParseMarkupDecl or ParseToken(dtSpace)) do;
end;

function TBoldDTDParser.ParseMarkupDecl: Boolean;
begin
  result := ParseElementDecl or
            ParseAttListDecl or
            ParseEntityDecl or
            ParseToken(dtComment);
end;

function TBoldDTDParser.ParseMixed2: Boolean;
begin
  result := ParseToken(dtPCDataDecl);
  if result then
  begin
    OptionalToken(dtSpace);
    if fScanner.LastToken = dtChoice then
    begin
      while fScanner.LastToken = dtChoice do
      begin
        fScanner.NextToken;
        OptionalToken(dtSpace);
        AssertToken(dtName);
        OptionalToken(dtSpace);
      end;
      AssertToken(dtCloseParen);
      AssertToken(dtZeroOrMore);
    end else
    begin
      AssertToken(dtCloseParen);
    end;
  end;
end;

function TBoldDTDParser.ParseMixedOrChildren: Boolean;
begin
  if ParseToken(dtOpenParen) then
  begin
    OptionalToken(dtSpace);
    AssertNonterminal(ParseMixed2 or
                      ParseChildren2);
    result := true;
  end else
    result := false;
end;

function TBoldDTDParser.ParseToken(Token: TBoldDTDToken): Boolean;
begin
  if fScanner.LastToken = Token then
  begin
    result := True;
    fScanner.NextToken;
  end else
    result := False;
end;

function TBoldDTDParser.Preprocess(DTD: string): string;
var
  DTDStrings: TStrings;
  Scanner: TBoldDTDScanner;
  PEName: string;
  PEText: string;
  PETable: TBoldStringList;
  i, j, k: Integer;
begin
  PETable := TBoldStringList.Create;
  Scanner := TBoldDTDScanner.Create(DTD);
  Scanner.NextToken;
  while Scanner.LastToken <> dtEOF do
  begin
    if Scanner.LastToken = dtEntityDecl then
    begin
      Scanner.NextToken;
      Assert(Scanner.LastToken = dtSpace);
      Scanner.NextToken;
      if Scanner.LastToken = dtPercent then
      begin
        Scanner.NextToken;
        Assert(Scanner.LastToken = dtSpace);
        Scanner.NextToken;
        Assert(Scanner.LastToken = dtName);
        PEName := Scanner.LastTokenText;
        Scanner.NextToken;
        Assert(Scanner.LastToken = dtSpace);
        Scanner.NextToken;
        Assert(Scanner.LastToken = dtStringLiteral);
        PEText := Scanner.LastTokenText;
        PETable.FastValues[PEName] := Copy(PEText, 2, length(PEText)-2);
      end;
    end;
    Scanner.NextToken;
  end;
  PETable.Sort;
  DTDStrings := TStringList.Create;
  DTDStrings.Text := DTD;
  for i := 0 to DTDStrings.Count-1 do
  begin
    j := 1;
    while j <= length(DTDStrings[i]) do
    begin
      if (DTDStrings[i][j] = '%') and not CharInSet(DTDStrings[i][j+1], SpaceChars) then
      begin
        k := j+1;
        while DTDStrings[i][k] <> ';' do
          inc(k);
        DTDStrings[i] := Copy(DTDStrings[i], 1, j-1) + ' ' +
                         PETable.FastValues[Copy(DTDStrings[i], j+1, k-j-1)] + ' ' +
                         Copy(DTDStrings[i], k+1, maxint);
      end else
        inc(j);
    end;
  end;

  result := DTDStrings.Text;
end;

procedure TBoldDTDParser.StartElement(Name: string);
begin
  if assigned(fOnStartElement) then
    fOnStartElement(Name);
end;


{ TBoldDTDScanner }

constructor TBoldDTDScanner.Create(DTD: string);
begin
  fDTD := DTD;
  fPos := 1;
end;

procedure TBoldDTDScanner.DeclarationToken;
begin
  if not (TryToken('<!ELEMENT', dtElementDecl) or
          TryToken('<!ATTLIST', dtAttlistDecl) or
          TryToken('<!ENTITY', dtEntityDecl)) then
    raise EBold.CreateFmt('%s.NextToken: Not a valid token, pos %d', [classname, fPos]);
end;


procedure TBoldDTDScanner.EOFToken;
begin
  if fLastToken = dtEOF then
    raise EBold.CreateFmt('%s: Tried to read past eof', [classname]);
  fLastToken := dtEOF;
  fLastTokenText := '';
end;

procedure TBoldDTDScanner.NameToken;
var
  newpos: integer;
begin
  newpos := fPos;
  while CharInSet(fDTD[newpos], NameChars) do
    inc(newpos);
  if newpos = fPos then
    raise EBold.CreateFmt('%s: Unknown character: ', [classname, fDTD[newpos]]);
  fLastTokenText := copy(fDTD, fPos, newpos-fPos);
  fPos := newpos;
  fLastToken := dtName;
end;

procedure TBoldDTDScanner.NextToken;
begin
  case fDTD[fPos] of
    #0:
      EOFToken;
    #$20, #$9, #$D, #$A:
      SpaceToken;
    '<':
      if not TryComment then
        DeclarationToken;
    '>':
      OneCharToken(dtEndDecl);
    '?':
      OneCharToken(dtOptional);
    '*':
      OneCharToken(dtZeroOrMore);
    '+':
      OneCharToken(dtOneOrMore);
    '|':
      OneCharToken(dtChoice);
    ',':
      OneCharToken(dtComma);
    '(':
      OneCharToken(dtOpenParen);
    ')':
      OneCharToken(dtCloseParen);
    '%':
      OneCharToken(dtPercent);
    ';':
      OneCharToken(dtSemicolon);
    '#':
      HashDeclToken;
    '''', '"':
      StringLiteralToken;
    else
    begin
      if not (TryToken('ANY', dtAnyContent) or
              TryToken('EMPTY', dtEmptyContent)) then
        NameToken;
    end;
  end;
end;

procedure TBoldDTDScanner.OneCharToken(Token: TBoldDTDToken);
begin
  fLastTokenText := Copy(fDTD, fPos, 1);
  inc(fPos);
  fLastToken := Token;
end;

procedure TBoldDTDScanner.HashDeclToken;
begin
  if not (TryToken('#PCDATA', dtPCDataDecl) or
          TryToken('#FIXED', dtFixedDecl) or
          TryToken('#IMPLIED', dtImpliedDecl) or
          TryToken('#REQUIRED', dtRequiredDecl)) then
    raise EBold.CreateFmt('%s.HashDeclToken: Scanner error', [classname]);
end;

procedure TBoldDTDScanner.SpaceToken;
var
  newpos: integer;
begin
  newpos := fPos;
  while CharInSet(fDTD[newpos], SpaceChars) do
    inc(newpos);
  fLastTokenText := copy(fDTD, fPos, newpos-fPos);
  fPos := newpos;
  fLastToken := dtSpace;
end;

procedure TBoldDTDScanner.StringLiteralToken;
var
  Delimiter: char;
  newpos: Integer;
begin
  Delimiter := fDTD[fPos];
  newpos := fPos+1;
  while fDTD[newpos] <> Delimiter do
    inc(newpos);
  inc(newpos);
  fLastTokenText := Copy(fDTD, fPos, newpos-fPos);
  fLastToken := dtStringLiteral;
  fPos := newpos;
end;

function TBoldDTDScanner.TryComment: Boolean;
var
  newpos: Integer;
begin
  if Copy(fDTD, fPos, 4) = '<!--' then
  begin
    newpos := fPos + 4 + 2;
    while not ((fDTD[newpos] = '>') and (fDTD[newpos-1] = '-') and (fDTD[newpos-2] = '-')) do
      inc(newpos);
    inc(newpos);
    fLastToken := dtComment;
    fLastTokenText := Copy(fDTD, fPos, newpos-fPos);
    fPos := newpos;
    result := true;
  end else
    result := false;
end;

function TBoldDTDScanner.TryToken(TokenStr: string;
  Token: TBoldDTDToken): Boolean;
begin
  if Copy(fDTD, fPos, length(TokenStr)) = TokenStr then
  begin
    fLastTokenText := TokenStr;
    fPos := fPos + Length(TokenStr);
    fLastToken := Token;
    result := True;
  end else
    result := False;
end;

end.