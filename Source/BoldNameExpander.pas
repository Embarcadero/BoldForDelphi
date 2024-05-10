
{ Global compiler directives }
{$include bold.inc}
unit BoldNameExpander;

interface

uses
  BoldDefs,
  BoldTaggedValueSupport;

type
  { forward declarations }
  TBoldAbstractNameExpander = class;
  TBoldSQLNameExpander = class;
  TBoldDelphiNameExpander = class;
  TBoldExpressionNameExpander = class;

  TBoldNameExpanderClass = class of TBoldAbstractNameExpander;
  TBoldSQLNameExpanderClass = class of TBoldSQLNameExpander;
  TBoldDelphiNameExpanderClass = class of TBoldDelphiNameExpander;
  TBoldExpressionNameExpanderClass = class of TBoldExpressionNameExpander;

  TExpansionType = (xtDelphi, xtSQL, xtExpression);
  TBoldCharacterSet = set of {$IFDEF BOLD_UNICODE}AnsiChar{$ELSE}Char{$ENDIF};

  { TBoldAbstractNameExpander }
  TBoldAbstractNameExpander = class
  protected
    function GetValidCharacters: TBoldCharacterSet; virtual;
    function GetMapCharacter(ch: char; NationalCharConversion: TBoldNationalCharConversion): char; virtual;
    function GetLanguageIsCaseSensitive: Boolean;
  public
    function ExpandName(const Name, ReplacementName: string): string; virtual;
    function MapCharacters(const Name: String; NationalCharConversion: TBoldNationalCharConversion): string; virtual;
    function TruncateName(const Name: String; MaxIdentifierLength: integer): string; virtual;
    function ValidateName(Name: String; var Reason: String; NationalCharConversion: TBoldNationalCharConversion; MaxIdentifierLength: integer): Boolean; virtual;
    property ValidCharacters: TBoldCharacterSet read GetValidCharacters;
    property MapCharacter[ch: char; NationalCharConversion: TBoldNationalCharConversion]: char read GetMapCharacter;
    property LanguageIsCaseSensitive: Boolean read GetLanguageIsCaseSensitive;
  end;

  { TBoldSQLNameExpander }
  TBoldSQLNameExpander = class(TBoldAbstractNameExpander)
  protected
    function GetValidCharacters: TBoldCharacterSet; override;
  public
    function ExpandName(const Name, ReplacementName: string): string; override;
  end;

  { TBoldDelphiNameExpander }
  TBoldDelphiNameExpander = class(TBoldAbstractNameExpander)
  protected
    function GetValidCharacters: TBoldCharacterSet; override;
  public
    function MapCharacters(const Name: String; NationalCharConversion: TBoldNationalCharConversion): string; override;
    function ValidateName(Name: String; var Reason: String; NationalCharConversion: TBoldNationalCharConversion; MaxIdentifierLength: integer): Boolean; override;
  end;

  { TBoldExpressionNameExpander }
  TBoldExpressionNameExpander = class(TBoldAbstractNameExpander)
  protected
    function GetValidCharacters: TBoldCharacterSet; override;
  public
    function MapCharacters(const Name: String; NationalCharConversion: TBoldNationalCharConversion): string; override;
    function ValidateName(Name: String; var Reason: String; NationalCharConversion: TBoldNationalCharConversion; MaxIdentifierLength: integer): Boolean; override;
  end;

var
  BoldSQLNameExpanderClass: TBoldSQLNameExpanderClass = TBoldSQLNameExpander;
  BoldDelphiNameExpanderClass: TBoldDelphiNameExpanderClass = TBoldDelphiNameExpander;
  BoldExpressionNameExpanderClass: TBoldExpressionNameExpanderClass = TBoldExpressionNameExpander;

  BoldNameExpanderMapAnsiCharacters: Boolean = false;
  BoldSQLNameExpanderNameLimit: integer = 255;
  BoldSQLNameExpanderUpperCaseNames: boolean = false;

function BoldExpandName(const Name, ReplacementName: string; ExpansionType: TExpansionType; MaxIdentifierLength: integer; NationalCharConversion: TBoldNationalCharConversion): string;
function BoldExpandPrefix(const value: String; const ReplacementName, Prefix: String; MaxIdentifierLength: integer; NationalCharConversion: TBoldNationalCharConversion): String;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldSharedStrings,
  BoldDefaultTaggedValues;

var
  BoldSQLNameExpander: TBoldSQLNameExpander = nil;
  BoldDelphiNameExpander: TBoldDelphiNameExpander = nil;
  BoldExpressionNameExpander: TBoldExpressionNameExpander = nil;

function BoldExpandName(const Name, ReplacementName: string;
                        ExpansionType: TExpansionType;
                        MaxIdentifierLength: integer;
                        NationalCharConversion: TBoldNationalCharConversion): string;
var
  CurrentNameExpander: TBoldAbstractNameExpander;
begin
  case expansionType of
    xtSQL:  begin
      if not assigned(BoldSQLNameExpander) or
         not (BoldSQLNameExpander.ClassType = BoldSQLNameExpanderClass) then
      begin
        BoldSQLNameExpander.Free;
        BoldSQLNameExpander := BoldSQLNameExpanderClass.Create;
      end;
      
      if MaxIdentifierLength = -1 then
        MaxIdentifierLength := BoldSQLNameExpanderNameLimit;
        
      CurrentNameExpander := BoldSQLNameExpander;
    end;
    xtDelphi: begin
      if not assigned(BoldDelphiNameExpander) or
         not (BoldDelphiNameExpander.ClassType = BoldDelphiNameExpanderClass) then
      begin
        BoldDelphiNameExpander.Free;
        BoldDelphiNameExpander := BoldDelphiNameExpanderClass.Create;
      end;
      CurrentNameExpander := BoldDelphiNameExpander;
    end;
    xtExpression: begin
      if not assigned(BoldExpressionNameExpander) or
         not (BoldExpressionNameExpander.ClassType = BoldExpressionNameExpanderClass) then
      begin
        BoldExpressionNameExpander.Free;
        BoldExpressionNameExpander := BoldExpressionNameExpanderClass.Create;
      end;
      CurrentNameExpander := BoldExpressionNameExpander;
    end
    else
      CurrentNameExpander := nil;
  end;
  if assigned(CurrentNameExpander) then
    result := CurrentNameExpander.TruncateName(
        CurrentNameExpander.MapCharacters(
        CurrentNameExpander.ExpandName(name, ReplacementName), NationalCharConversion), MaxIdentifierLength)
  else
    result := Name;
  result := BoldSharedStringManager.GetSharedString(Result);
end;

function BoldExpandPrefix(const value: String;
                          const ReplacementName, Prefix: String;
                          MaxIdentifierLength: integer;
                          NationalCharConversion: TBoldNationalCharConversion): String;
var
  p: integer;
begin
  result := value;
  p := pos(Uppercase(TABLEPREFIXTAG), Uppercase(result));
  while p <> 0 do
  begin
    system.Delete(result, p, length(TABLEPREFIXTAG));
    System.Insert(Prefix, result, p);
    p := pos(Uppercase(TABLEPREFIXTAG), Uppercase(result));
  end;
  result := BoldExpandName(Result, ReplacementName, xtSQL, MaxIdentifierLength, NationalCharConversion);
end;

{ TBoldAbstractNameExpander }

function TBoldAbstractNameExpander.ExpandName(const Name, ReplacementName: string): string;
  function DoExpand(const S: string): string;
  var
    I: integer;
  begin
    if (S = TV_NAME) then
      Result := ReplacementName
    else if Length(S) < TV_NAME_Length then
      Result :=  S
    else
    begin
      I := BoldCaseIndependentPos(TV_Name, S);
      if I <> 0 then
        Result := Copy(S, 1, I - 1) +
                  ReplacementName +
                  DoExpand(Copy(S, I + TV_NAME_Length, MaxInt))
      else
        Result := S;
     end;   
   end;
begin
  Result :=  DoExpand(Name);
end;

function TBoldAbstractNameExpander.GetLanguageIsCaseSensitive: Boolean;
begin
  result := false;
end;

function TBoldAbstractNameExpander.GetMapCharacter(ch: char; NationalCharConversion: TBoldNationalCharConversion): char;
begin
  if ((NationalCharConversion = nccDefault) and BoldNameExpanderMapAnsiCharacters) or
    (NationalCharConversion = nccTrue) then
  case ch of
    '�': result := 'a';
    '�': result := 'A';
    '�': result := 'a';
    '�': result := 'a';
    '�': result := 'A';
    '�': result := 'A';
    '�': result := 'A';
    '�': result := 'a';
    '�': result := 'A';
    '�': result := 'a';

    '�': result := 'O';
    '�': result := 'o';
    '�': result := 'O';
    '�': result := 'o';
    '�': result := 'O';
    '�': result := 'o';
    '�': result := 'O';
    '�': result := 'o';

    '�': result := 'E';
    '�': result := 'E';
    '�': result := 'E';
    '�': result := 'E';
    '�': result := 'e';
    '�': result := 'e';
    '�': result := 'e';
    '�': result := 'e';

    '�': result := 'U';
    '�': result := 'u';
    '�': result := 'U';
    '�': result := 'U';
    '�': result := 'u';
    '�': result := 'u';
    '�': result := 'U';
    '�': result := 'u';

    '�': result := 'N';
    '�': result := 'n';

    '�': result := 'I';
    '�': result := 'I';
    '�': result := 'I';
    '�': result := 'I';
    '�': result := 'i';
    '�': result := 'i';
    '�': result := 'i';
    '�': result := 'i';

    '�': result := 'Y';
    '�': result := 'y';
    '�': result := 'y';

    '�': result := 'C';
    '�': result := 'c';

    else result := '_';
  end
  else
    result := '_';
end;

function TBoldAbstractNameExpander.GetValidCharacters: TBoldCharacterSet;
begin
  result := [];
end;

function TBoldAbstractNameExpander.MapCharacters(const Name: String;
                                                 NationalCharConversion: TBoldNationalCharConversion): string;
var
  i: integer;
  validChars: TBoldCharacterSet;
begin
  result := Name;
  validChars := ValidCharacters;
  if validChars <> [] then
    for i := 1 to Length(result) do
      if not CharInSet(result[i], ValidChars) then
        result[i] := MapCharacter[result[i], NationalCharConversion];
end;

function TBoldAbstractNameExpander.TruncateName(const Name: String;
                                                MaxIdentifierLength: integer): string;
var
  CheckSum: WORD;
  i: integer;
const
  Characters: array[0..36] of char =
  ('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','X','Y','Z',
   '0','1','2','3','4','5','6','7','8','9','0','_');
begin
  if (MaxIdentifierLength <> -1) and (length(Name) > MaxIdentifierLength) then
  begin
    Result := copy(Name, 1, MaxIdentifierLength - 3);
    CheckSum := 0;
    for i := 1 to Length(Name) do
    begin
      CheckSum := CheckSum xor ord(Name[i]);
      CheckSum := ((CheckSum shl 3) MOD 65536) + (CheckSum shr 13);
    end;
    While (CheckSum > 0) and (length(Result) < MaxIdentifierLength) do
    begin
      Result := Result + Characters[CheckSum MOD 37];
      CheckSum := CheckSum DIV 37;
    end;
  end
  else
    result := Name;
end;

function TBoldAbstractNameExpander.ValidateName(Name: String;
                                                var Reason: String;
                                                NationalCharConversion: TBoldNationalCharConversion;
                                                MaxIdentifierLength: integer): Boolean;
begin
  result := true;
  if name <> MapCharacters(Name, NationalCharConversion) then
  begin
    result := false;
    reason := 'Name has invalid characters';
  end;
  if name <> truncateName(Name, MaxIdentifierLength) then
  begin
    result := false;
    reason := 'Name is too long';
  end;
  if result then
  begin
    if not LanguageIsCaseSensitive then
      name := UpperCase(Name);
  end;   
end;

{ TBoldSQLNameExpander }

function TBoldSQLNameExpander.ExpandName(const Name, ReplacementName: string): string;
begin
  result := inherited ExpandName(Name, ReplacementName);
  if BoldSQLNameExpanderUpperCaseNames then
    result := upperCase(Result);
end;

function TBoldSQLNameExpander.GetValidCharacters: TBoldCharacterSet;
begin
  result := ['a'..'z', 'A'..'Z', '0'..'9', '"', '''', '_'];
end;

{function TBoldSQLNameExpander.TruncateName(const Name: String): string;
var
  CheckSum: WORD;
  i: integer;
const
  Characters: array[0..36] of char = ('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','X','Y','Z','0','1','2','3','4','5','6','7','8','9','0','_');
begin
  if length(Name) > BoldSQLNameExpanderNameLimit then
  begin
    Result := copy(Name, 1, BoldSQLNameExpanderNameLimit-3);
    CheckSum := 0;
    for i := 1 to Length(Name) do
    begin
      CheckSum := CheckSum xor ord(Name[i]);
      CheckSum := ((CheckSum shl 3) MOD 65536) + (CheckSum shr 13);
    end;
    While (CheckSum > 0) and (length(Result) < BoldSQLNameExpanderNameLimit) do
    begin
      Result := Result + Characters[CheckSum MOD 37];
      CheckSum := CheckSum DIV 37;
    end;
  end
  else
    result := Name;
end;
}
{ TBoldDelphiNameExpander }

function TBoldDelphiNameExpander.GetValidCharacters: TBoldCharacterSet;
begin
  result := ['a'..'z', 'A'..'Z', '0'..'9', '_'];
end;


function TBoldDelphiNameExpander.MapCharacters(const Name: String;
                                               NationalCharConversion: TBoldNationalCharConversion): string;
begin
  result := inherited MapCharacters(Name, NationalCharConversion);
  if (length(result) > 0) and not CharInSet(result[1], ['a'..'z', 'A'..'Z', '_']) then
    result[1] := MapCharacter[result[1], NationalCharConversion];
end;

function TBoldDelphiNameExpander.ValidateName(Name: String;
                                              var Reason: String;
                                              NationalCharConversion: TBoldNationalCharConversion;
                                              MaxIdentifierLength: integer): Boolean;
begin
  result := inherited validateName(Name, reason, NationalCharConversion, MaxIdentifierLength);
  if result and (length(name) = 0) then
  begin
    result := false;
    reason := 'Name can not be empty';
  end;
  if result and not CharInSet(name[1], ['a'..'z', 'A'..'Z', '_']) then
  begin
    result := false;
    reason := 'Name must begin with an alpha-character or underscore';
  end;
end;

{ TBoldExpressionNameExpander }

function TBoldExpressionNameExpander.GetValidCharacters: TBoldCharacterSet;
begin
 result := ['a'..'z', 'A'..'Z', '0'..'9', '_'];
end;

function TBoldExpressionNameExpander.MapCharacters(const Name: String;
                                                   NationalCharConversion: TBoldNationalCharConversion): string;
begin
  result := inherited MapCharacters(Name, NationalCharConversion);
  if (length(result) > 0) and not CharInSet(result[1], ['a'..'z', 'A'..'Z', '_']) then
    result[1] := MapCharacter[result[1], NationalCharConversion];
end;

function TBoldExpressionNameExpander.ValidateName(Name: String;
                                                  var Reason: String;
                                                  NationalCharConversion: TBoldNationalCharConversion;
                                                  MaxIdentifierLength: integer): Boolean;
begin
  result := inherited validateName(Name, reason, NationalCharConversion, MaxIdentifierLength);
  if result and (length(name) = 0) then
  begin
    result := false;
    reason := 'Name can not be empty';
  end;

  if result and not CharInSet(name[1], ['a'..'z', 'A'..'Z', '_']) then
  begin
    result := false;
    reason := 'Name must begin with an alpha-character or underscore';
  end;
end;

initialization

finalization
  FreeAndNil(BoldSQLNameExpander);
  FreeAndNil(BoldDelphiNameExpander);
  FreeAndNil(BoldExpressionNameExpander);

end.
