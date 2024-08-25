
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldRegularExpression;

{*******************************************************************************
*                                                                              *
* Filename: RegularExpression.pas                                             *
*                                                                              *
* Author  : Ian Lane (Email: lanei@ideal.net.au)                              *
*                                                                              *
* Synopsis: This is a regular expression parser component for Borland Delphi. *
*                                                                              *
* See also: IMetaCharacters1.pas, IRegularExpression1.pas, ISource1.pas,      *
*            IToken1.pas and ITokenList1.pas which contain the definitions for *
*            this component.                                                   *
*                                                                              *
*            IMetaCharacters2.pas, IRegularExpression2.pas, ISource2.pas,      *
*            IToken2.pas and ITokenList2.pas which contain the implementation  *
*            of this component                                                 *
*                                                                              *
*                          Copyright (c) 1998 Ian Lane                         *
*                                                                              *
*******************************************************************************}

interface

uses
  BoldDefs,
  Classes,
  BoldBase;

const
  DefaultAnyChar            = '.';
  DefaultCharSetOpen        = '[';
  DefaultCharSetClose       = ']';
  DefaultCharSetComplement  = '~';
  DefaultCharSetRange       = '-';
  DefaultEndOfLine          = '$';
  DefaultQuoteChar          = '\';
  DefaultRepeat0OrMoreTimes = '*';
  DefaultRepeat1OrMoreTimes = '+';
  DefaultStartOfLine        = '^';

type
  TMetaCharacters = class;
  TRegularExpression = class;
  TSource = class;
  TToken = class;
  TTokenList = class;

  EMetaCharacters = EBold;
  TMetaCharacters = class(TPersistent)
  private
    FAnyChar: Char;
    FCharSetClose: Char;
    FCharSetComplement: Char;
    FCharSetOpen: Char;
    FCharSetRange: Char;
    FEndOfLine: Char;
    FQuoteChar: Char;
    FRegularExpression: TRegularExpression;
    FRepeat0OrMoreTimes: Char;
    FRepeat1OrMoreTimes: Char;
    FStartOfLine: Char;
    procedure NotifyRegularExpressionOfChange;
    procedure SetAnyChar(const Value: Char);
    procedure SetCharSetClose(const Value: Char);
    procedure SetCharSetComplement(const Value: Char);
    procedure SetCharSetOpen(const Value: Char);
    procedure SetCharSetRange(const Value: Char);
    procedure SetEndOfLine(const Value: Char);
    procedure SetQuoteChar(const Value: Char);
    procedure SetRepeat0OrMoreTimes(const Value: Char);
    procedure SetRepeat1OrMoreTimes(const Value: Char);
    procedure SetStartOfLine(const Value: Char);
    procedure VerifyMetaCharIsValid(const NewCharacter: Char);
  public
    constructor Create(const RegularExpression: TRegularExpression);
    procedure Assign(Source: TPersistent); override;
    function Equals(const Other: TMetaCharacters): Boolean; reintroduce;
  published
    property AnyChar: Char read FAnyChar write SetAnyChar default DefaultAnyChar;
    property CharSetClose: Char read FCharSetClose write SetCharSetClose
                                 default DefaultCharSetClose;
    property CharSetComplement: Char read FCharSetComplement write SetCharSetComplement
                                      default DefaultCharSetComplement;
    property CharSetOpen: Char read FCharSetOpen write SetCharSetOpen
                                default DefaultCharSetOpen;
    property CharSetRange: Char read FCharSetRange write SetCharSetRange
                                 default DefaultCharSetRange;
    property EndOfLine: Char read FEndOfLine write SetEndOfLine default DefaultEndOfLine;
    property QuoteChar: Char read FQuoteChar write SetQuoteChar default DefaultQuoteChar;
    property Repeat0OrMoreTimes: Char read FRepeat0OrMoreTimes write SetRepeat0OrMoreTimes
                                       default DefaultRepeat0OrMoreTimes;
    property Repeat1OrMoreTimes: Char read FRepeat1OrMoreTimes write SetRepeat1OrMoreTimes
                                       default DefaultRepeat1OrMoreTimes;
    property StartOfLine: Char read FStartOfLine write SetStartOfLine
                                default DefaultStartOfLine;
  end;

  TBuildTokenListEvent = procedure(Sender: TObject; const RegularExpression: String) of object;
  TRegExprStage = (resSearchExpressionStart, resSearchExpressionStep,
                   resSearchExpressionFinish, resSearchExpressionAbort);
  TRegExprProgressEvent = function(Sender: TObject; const Stage: TRegExprStage;
                                   const PercentDone: Byte): Boolean  of Object;
  TRegularExpression = class(TBoldMemoryManagedObject)
  private
    FCaseSensitive: Boolean;
    FMetaCharacters: TMetaCharacters;
    FOnBuildTokenList: TBuildTokenListEvent;
    FOnProgress: TRegExprProgressEvent;
    FRegularExpression: String;
    FTokenList: TTokenList;
    FTokenListRequiresRebuilding: Boolean;
    function GetVersion: String;
    function Progress(const Stage: TRegExprStage;
                      const Total, AmountPerformed: Integer): Boolean;
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetMetaCharacters(const Value: TMetaCharacters);
    procedure SetOnBuildTokenList(const Value: TBuildTokenListEvent);
    procedure SetRegularExpression(const Value: String);
  protected
    function DoSearch(const Source: TSource): Integer;
  public
    constructor Create;
    procedure BuildTokenList;
    procedure BuildTokenListDeferred(const ClearTokensNow: Boolean);
    function Search(const Source: String): Integer;
    function SearchStream(const Source: TStream): Integer;
    function SearchString(const Source: String): Integer;
    destructor Destroy; override;
    property Version: String read GetVersion;
//  published
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive
                                     default False;
    property MetaCharacters: TMetaCharacters read FMetaCharacters write SetMetaCharacters;
    property RegularExpression: String read FRegularExpression write SetRegularExpression;
    property OnBuildTokenList: TBuildTokenListEvent read FOnBuildTokenList
                                                     write SetOnBuildTokenList;
    property OnProgress: TRegExprProgressEvent read FOnProgress write FOnProgress;
  end;

  TSource = class(TBoldMemoryManagedObject)
  protected
    function GetCharacter(const Index: Integer): Char; virtual; abstract;
    function GetLength: Integer; virtual; abstract;
  public
    property Character[const Index: Integer]: Char read GetCharacter; default;
    property Length: Integer read GetLength;
  end;
  TStreamSource = class(TSource)
  private
    FSource: TStream;
  protected
    function GetCharacter(const Index: Integer): Char; override;
    function GetLength: Integer; override;
  public
    constructor Create(const Source: TStream); virtual;
  end;
  TStringSource = class(TSource)
  private
    FSource: String;
  protected
    function GetCharacter(const Index: Integer): Char; override;
    function GetLength: Integer; override;
  public
    constructor Create(const Source: String);
  end;

  TSetOfChar = set of {$IFDEF BOLD_UNICODE}AnsiChar{$ELSE}Char{$ENDIF};
  TTokenType = (ttAnyChar, ttChar, ttCharSet, ttEndOfLine, ttRepeat, ttStartOfLine);
  TToken = class(TBoldMemoryManagedObject)
  private
    FCharacter: Char;
    FCharacterSet: TSetOfChar;
    FFollowingTokens: TTokenList;
    FTokenToRepeat: TToken;
    FTokenType: TTokenType;
    procedure SetTokenType(const Value: TTokenType);
  protected
    procedure Assign(const Source: TToken);
  public
    destructor Destroy; override;
    property Character: Char read FCharacter write FCharacter;
    property FollowingTokens: TTokenList read FFollowingTokens;
    property CharacterSet: TSetOfChar read FCharacterSet write FCharacterSet;
    property TokenToRepeat: TToken read FTokenToRepeat;
    property TokenType: TTokenType read FTokenType write SetTokenType;
  end;
  TTokenList = class(TBoldMemoryManagedObject)
  private
    FList: TList;
  private
    function Add(const TokenType: TTokenType): TToken;
    function GetCount: Integer;
    function GetToken(const Index: Integer): TToken;
  protected
    procedure Assign(const Source: TTokenList);
  public
    constructor Create;
    procedure Build(const RegularExpression: String;
                    const MetaCharacters: TMetaCharacters;
                    const CaseSensitive: Boolean);
    procedure Clear;
    procedure Delete(const Index: Integer);
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Tokens[const Index: Integer]: TToken read GetToken; default;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

const
  CurrentVersion = '1.1';

procedure TMetaCharacters.Assign(Source: TPersistent);

begin
  FAnyChar := TMetaCharacters(Source).FAnyChar;
  FCharSetClose := TMetaCharacters(Source).FCharSetClose;
  FCharSetComplement := TMetaCharacters(Source).FCharSetComplement;
  FCharSetOpen := TMetaCharacters(Source).FCharSetOpen;
  FCharSetRange := TMetaCharacters(Source).FCharSetRange;
  FEndOfLine := TMetaCharacters(Source).FEndOfLine;
  FQuoteChar := TMetaCharacters(Source).FQuoteChar;
  FRegularExpression := TMetaCharacters(Source).FRegularExpression;
  FRepeat0OrMoreTimes := TMetaCharacters(Source).FRepeat0OrMoreTimes;
  FRepeat1OrMoreTimes := TMetaCharacters(Source).FRepeat1OrMoreTimes;
  FStartOfLine := TMetaCharacters(Source).FStartOfLine
end;

constructor TMetaCharacters.Create(const RegularExpression: TRegularExpression);

begin
  Inherited Create;
  FRegularExpression := RegularExpression;
  FAnyChar := DefaultAnyChar;
  FCharSetClose := DefaultCharSetClose;
  FCharSetComplement := DefaultCharSetComplement;
  FCharSetOpen := DefaultCharSetOpen;
  FCharSetRange := DefaultCharSetRange;
  FEndOfLine := DefaultEndOfLine;
  FQuoteChar := DefaultQuoteChar;
  FRepeat0OrMoreTimes := DefaultRepeat0OrMoreTimes;
  FRepeat1OrMoreTimes := DefaultRepeat1OrMoreTimes;
  FStartOfLine := DefaultStartOfLine
end;

function TMetaCharacters.Equals(const Other: TMetaCharacters): Boolean;

begin
  Result := (Other is TMetaCharacters) and
            (FAnyChar            = TMetaCharacters(Other).FAnyChar          ) and
            (FCharSetClose       = TMetaCharacters(Other).FCharSetClose     ) and
            (FCharSetComplement  = TMetaCharacters(Other).FCharSetComplement) and
            (FCharSetOpen        = TMetaCharacters(Other).FCharSetOpen      ) and
            (FCharSetRange       = TMetaCharacters(Other).FCharSetRange     ) and
            (FEndOfLine          = TMetaCharacters(Other).FEndOfLine        ) and
            (FQuoteChar          = TMetaCharacters(Other).FQuoteChar        ) and
            (FRepeat0OrMoreTimes = TMetaCharacters(Other).FRepeat0OrMoreTimes) and
            (FRepeat1OrMoreTimes = TMetaCharacters(Other).FRepeat1OrMoreTimes) and
            (FStartOfLine        = TMetaCharacters(Other).FStartOfLine      )
end;

// This method notifies are owning TRegularExpression component that one of our
// properties has changed.
procedure TMetaCharacters.NotifyRegularExpressionOfChange;

begin
  if Assigned(FRegularExpression) then
    FRegularExpression.BuildTokenListDeferred(False)
end;

procedure TMetaCharacters.SetAnyChar(const Value: Char);

begin
  if Value <> FAnyChar then
  begin
    VerifyMetaCharIsValid(Value);
    FAnyChar := Value;
    NotifyRegularExpressionOfChange
  end
end;

procedure TMetaCharacters.SetCharSetClose(const Value: Char);

begin
  if Value <> FCharSetClose then
  begin
    VerifyMetaCharIsValid(Value);
    FCharSetClose := Value;
    NotifyRegularExpressionOfChange
  end
end;

procedure TMetaCharacters.SetCharSetComplement(const Value: Char);

begin
  if Value <> FCharSetComplement then
  begin
    VerifyMetaCharIsValid(Value);
    FCharSetComplement := Value;
    NotifyRegularExpressionOfChange
  end
end;

procedure TMetaCharacters.SetCharSetOpen(const Value: Char);

begin
  if Value <> FCharSetOpen then
  begin
    VerifyMetaCharIsValid(Value);
    FCharSetOpen := Value;
    NotifyRegularExpressionOfChange
  end
end;

procedure TMetaCharacters.SetCharSetRange(const Value: Char);

begin
  if Value <> FCharSetRange then
  begin
    VerifyMetaCharIsValid(Value);
    FCharSetRange := Value;
    NotifyRegularExpressionOfChange
  end
end;

procedure TMetaCharacters.SetEndOfLine(const Value: Char);

begin
  if Value <> FEndOfLine then
  begin
    VerifyMetaCharIsValid(Value);
    FEndOfLine := Value;
    NotifyRegularExpressionOfChange
  end
end;

procedure TMetaCharacters.SetQuoteChar(const Value: Char);

begin
  if Value <> FQuoteChar then
  begin
    VerifyMetaCharIsValid(Value);
    FQuoteChar := Value;
    NotifyRegularExpressionOfChange
  end
end;

procedure TMetaCharacters.SetRepeat0OrMoreTimes(const Value: Char);

begin
  if Value <> FRepeat0OrMoreTimes then
  begin
    VerifyMetaCharIsValid(Value);
    FRepeat0OrMoreTimes := Value;
    NotifyRegularExpressionOfChange
  end
end;

procedure TMetaCharacters.SetRepeat1OrMoreTimes(const Value: Char);

begin
  if Value <> FRepeat1OrMoreTimes then
  begin
    VerifyMetaCharIsValid(Value);
    FRepeat1OrMoreTimes := Value;
    NotifyRegularExpressionOfChange
  end
end;

procedure TMetaCharacters.SetStartOfLine(const Value: Char);

begin
  if Value <> FStartOfLine then
  begin
    VerifyMetaCharIsValid(Value);
    FStartOfLine := Value;
    NotifyRegularExpressionOfChange
  end
end;

// This method verifies that NewCharacter is not already contained in any property
// of this component. It raises an EMetaCharacters exception on failure.
procedure TMetaCharacters.VerifyMetaCharIsValid(const NewCharacter: Char);

begin
  if (NewCharacter = FAnyChar          ) or
     (NewCharacter = FCharSetClose     ) or
     (NewCharacter = FCharSetComplement) or
     (NewCharacter = FCharSetOpen      ) or
     (NewCharacter = FCharSetRange     ) or
     (NewCharacter = FEndOfLine        ) or
     (NewCharacter = FQuoteChar        ) or
     (NewCharacter = FRepeat0OrMoreTimes) or
     (NewCharacter = FRepeat1OrMoreTimes) or
     (NewCharacter = FStartOfLine      ) then
    raise EMetaCharacters.CreateFmt('MetaCharacter ''%s'' is already in use', [NewCharacter])
end;

procedure TRegularExpression.BuildTokenList;
begin
  if Assigned(FOnBuildTokenList) then
    FOnBuildTokenList(Self, RegularExpression);
  FTokenList.Build(RegularExpression, MetaCharacters, CaseSensitive);
  FTokenListRequiresRebuilding := False
end;


procedure TRegularExpression.BuildTokenListDeferred(const ClearTokensNow: Boolean);

begin
  FTokenListRequiresRebuilding := True;
  if ClearTokensNow then
    FTokenList.Clear
end;

constructor TRegularExpression.Create;

begin
  FMetaCharacters := TMetaCharacters.Create(Self);
  FTokenList := TTokenList.Create
end;

destructor TRegularExpression.Destroy;

begin
  FMetaCharacters.Free;
  FTokenList.Free;
  Inherited Destroy
end;


function TRegularExpression.DoSearch(const Source: TSource): Integer;

var
  Index: Integer;

  function CompareTokenList(const TokenList: TTokenList; Index: Integer): Boolean;

  var
    TokenIndex: Integer;

    function CompareToken(const Token: TToken; var Index: Integer): Boolean; forward;


    function CompareRepeatToken(const Token: TToken; var Index: Integer): Boolean;

    var
      i: Integer;

    begin
      Result := False;
      i := Index;
      while CompareToken(Token.TokenToRepeat, i) do ;
      Dec(i);
      while not Result and (i >= Index) do
      begin
        Result := CompareTokenList(Token.FollowingTokens, i);
        Dec(i)
      end
    end;


    function CompareToken(const Token: TToken; var Index: Integer): Boolean;

    begin
      case Token.TokenType of
        ttAnyChar    : begin
                          Result := Index <= Source.Length;
                          Inc(Index)
                        end;
        ttChar       : if Index <= Source.Length then
                        begin
                          Result := Token.Character = Source[Index];
                          Inc(Index)
                        end
                        else
                          Result := False;
        ttCharSet    : begin
                          Result := (Index <= Source.Length) and
                                    CharInSet(Source[Index], Token.CharacterSet);
                          Inc(Index)
                        end;
        ttEndOfLine  : Result := Index = Source.Length + 1;
        ttRepeat     : Result := CompareRepeatToken(Token, Index);
        ttStartOfLine: Result := Index = 1
        else            Result := False
      end
    end;

  begin { TRegularExpression.Search.CompareTokenList }
    Result := True;
    TokenIndex := 0;
    while Result and (TokenIndex < TokenList.Count) do
    begin
      Result := CompareToken(TokenList[TokenIndex], Index);
      Inc(TokenIndex)
    end
  end;

begin { TRegularExpression.Search }
  Result := 0;
  if FTokenListRequiresRebuilding then
    BuildTokenList;
  Index := 1;
  if not Progress(resSearchExpressionStart, Source.Length, 0) then
    exit;

  while (Result <= 0) and (Index <= Source.Length) do
  begin
    if CompareTokenList(FTokenList, Index) then
      Result := Index
    else
      Inc(Index);
    if not Progress(resSearchExpressionStep, Source.Length, Index) then
      exit
  end;
  Progress(resSearchExpressionFinish, Source.Length, Index)
end;

function TRegularExpression.GetVersion: String;

begin
  Result := CurrentVersion
end;


function TRegularExpression.Progress(const Stage: TRegExprStage;
                                     const Total, AmountPerformed: Integer): Boolean;

begin
  if Assigned(FOnProgress) then
  begin
    Result := FOnProgress(Self, Stage, AmountPerformed * 100 div Total);
    if not Result and Assigned(FOnProgress) then
      if Stage in [resSearchExpressionStart, resSearchExpressionFinish] then
        FOnProgress(Self, resSearchExpressionAbort, AmountPerformed * 100 div Total)
  end
  else
    Result := True
end;


function TRegularExpression.Search(const Source: String): Integer;

begin
  Result := SearchString(Source)
end;


function TRegularExpression.SearchStream(const Source: TStream): Integer;

var
  StreamSource: TStreamSource;

begin
  StreamSource := TStreamSource.Create(Source);
  try
    Result := DoSearch(StreamSource)
  finally
    StreamSource.Free
  end
end;


function TRegularExpression.SearchString(const Source: String): Integer;

var
  StringSource: TStringSource;

begin
  StringSource := TStringSource.Create(Source);
  try
    Result := DoSearch(StringSource)
  finally
    StringSource.Free
  end
end;

procedure TRegularExpression.SetCaseSensitive(const Value: Boolean);

begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    BuildTokenListDeferred(False)
  end
end;

procedure TRegularExpression.SetMetaCharacters(const Value: TMetaCharacters);

begin
  if not Value.Equals(FMetaCharacters) then
  begin
    FMetaCharacters := Value;
    BuildTokenListDeferred(False)
  end
end;

procedure TRegularExpression.SetOnBuildTokenList(const Value: TBuildTokenListEvent);

begin
  if @Value <> @FOnBuildTokenList then
  begin
    FOnBuildTokenList := Value;
    if Assigned(FOnBuildTokenList) then
      BuildTokenListDeferred(False)
  end
end;

procedure TRegularExpression.SetRegularExpression(const Value: String);

begin
  if Value <> FRegularExpression then
  begin
    FRegularExpression := Value;
    BuildTokenListDeferred(False)
  end
end;

constructor TStreamSource.Create(const Source: TStream);

begin
  Inherited Create;
  FSource := Source
end;

function TStreamSource.GetCharacter(const Index: Integer): Char;

begin
  FSource.Position := Index - 1;
  FSource.ReadBuffer(Result, SizeOf(Result))
end;

function TStreamSource.GetLength: Integer;

begin
  Result := FSource.Size
end;

constructor TStringSource.Create(const Source: String);

begin
  Inherited Create;
  FSource := Source
end;

function TStringSource.GetCharacter(const Index: Integer): Char;

begin
  Result := FSource[Index]
end;

function TStringSource.GetLength: Integer;

begin
  Result := System.Length(FSource)
end;
procedure TToken.Assign(const Source: TToken);

begin
  TokenType := Source.TokenType;
  case TokenType of
    ttChar   : Character := Source.Character;
    ttCharSet: CharacterSet := Source.CharacterSet;
    ttRepeat : begin
                  TokenToRepeat.Assign(Source.TokenToRepeat);
                  FollowingTokens.Assign(Source.FollowingTokens)
                end
  end
end;

destructor TToken.Destroy;

begin
  FFollowingTokens.Free;
  FTokenToRepeat.Free;
  Inherited Destroy
end;

procedure TToken.SetTokenType(const Value: TTokenType);

begin
  if Value <> FTokenType then
  begin
    if FTokenType = ttRepeat then
    begin
      FTokenToRepeat.Free;
      FTokenToRepeat := nil;
      FFollowingTokens.Free;
      FFollowingTokens := nil
    end;
    FTokenType := Value;
    if FTokenType = ttRepeat then
    begin
      FTokenToRepeat := TToken.Create;
      FFollowingTokens := TTokenList.Create
    end
  end
end;

function TTokenList.Add(const TokenType: TTokenType): TToken;

begin
  Result := TToken.Create;
  Result.TokenType := TokenType;
  FList.Add(Result)
end;

procedure TTokenList.Assign(const Source: TTokenList);

var
  i: Integer;

begin
  FList.Capacity := Source.Count;
  for i := 0 to Source.Count - 1 do
    Add(Source[i].TokenType).Assign(Source[i])
end;
procedure TTokenList.Build(const RegularExpression: String;
                           const MetaCharacters: TMetaCharacters;
                           const CaseSensitive: Boolean);

var
  i: Integer;

  procedure BuildCharSetToken;

  const
    {$IFDEF BOLD_UNICODE}
    FullCharacterSet = [Low(AnsiChar)..High(AnsiChar)];
    {$ELSE}
    FullCharacterSet = [Low(Char)..High(Char)];
    {$ENDIF}

  var
    c: Char;
    CharacterSet: TSetOfChar;
    ComplementCharSet: Boolean;
    j: Integer;
    s: String;

  begin
    Inc(i);
    ComplementCharSet := (i <= Length(RegularExpression)) and
                         (RegularExpression[i] = MetaCharacters.CharSetComplement);
    if ComplementCharSet then
      Inc(i);
    while (i <= Length(RegularExpression)) and
          (RegularExpression[i] <> MetaCharacters.CharSetClose) do
    begin
      if (RegularExpression[i] = MetaCharacters.CharSetRange) and (Length(s) > 0) and
         (i < Length(RegularExpression)) and
         (RegularExpression[i + 1] <> MetaCharacters.CharSetClose) then
      begin
        Inc(i);
        if (RegularExpression[i] = MetaCharacters.QuoteChar) and
           (i < Length(RegularExpression)) then
          Inc(i);
        for c := Chr(Ord(s[Length(s)]) + 1) to RegularExpression[i] do
          s := s + c;
        Inc(i)
      end
      else
      begin
        if (RegularExpression[i] = MetaCharacters.QuoteChar) and
           (i < Length(RegularExpression)) then
          Inc(i);
        s := s + RegularExpression[i];
        Inc(i)
      end
    end;
    CharacterSet := [];
    for j := 1 to Length(s) do
    begin
      Include(CharacterSet, AnsiChar(s[j]));
      if not CaseSensitive then
      begin
        Include(CharacterSet, AnsiChar(AnsiUpperCase(s[j])[1]));
        Include(CharacterSet, AnsiChar(AnsiLowerCase(s[j])[1]));
      end
    end;
    if ComplementCharSet then
      CharacterSet := FullCharacterSet - CharacterSet;
    if CharacterSet = FullCharacterSet then
      Add(ttAnyChar)
    else
      if CharacterSet <> [] then
        Add(ttCharSet).CharacterSet := CharacterSet
  end;

  procedure BuildRepeatToken;

  var
    NewToken: TToken;

  begin
    NewToken := Add(ttRepeat);
    NewToken.TokenToRepeat.Assign(Tokens[Count - 2]);
    if RegularExpression[i] = MetaCharacters.Repeat0OrMoreTimes then
      Delete(Count - 2);
    NewToken.FollowingTokens.Build(Copy(RegularExpression, i + 1, MaxInt), MetaCharacters,
                                   CaseSensitive);
    i := Length(RegularExpression)
  end;

begin { TTokenList.Build }
  i := 1;
  Clear;
  while i <= Length(RegularExpression) do
  begin
    if (RegularExpression[i] = MetaCharacters.StartOfLine) and (i = 1) then
      Add(ttStartOfLine)
    else
      if (RegularExpression[i] = MetaCharacters.EndOfLine) and
         (i = Length(RegularExpression)) then
        Add(ttEndOfLine)
      else
        if (RegularExpression[i] = MetaCharacters.QuoteChar) and
           (i < Length(RegularExpression)) then
        begin
          Add(ttChar).Character := RegularExpression[i + 1];
          Inc(i)
        end
        else
          if RegularExpression[i] = MetaCharacters.AnyChar then
            Add(ttAnyChar)
          else
            if ((RegularExpression[i] = MetaCharacters.Repeat0OrMoreTimes) or
                (RegularExpression[i] = MetaCharacters.Repeat1OrMoreTimes)) and
               (i > 1) and (Tokens[Count - 1].TokenType <> ttStartOfLine) then
              BuildRepeatToken
            else
              if RegularExpression[i] = MetaCharacters.CharSetOpen then
                BuildCharSetToken
              else
                if CaseSensitive then
                  Add(ttChar).Character := RegularExpression[i]
                else
                  Add(ttCharSet).CharacterSet := [RegularExpression[i],
                                                  AnsiUpperCase(RegularExpression[i])[1],
                                                  AnsiLowerCase(RegularExpression[i])[1]];
    Inc(i)
  end
end;
procedure TTokenList.Clear;

var
  i: Integer;

begin
  for i := 0 to Count - 1 do
    Tokens[i].Free;
  FList.Clear
end;

constructor TTokenList.Create;

begin
  Inherited Create;
  FList := TList.Create
end;

procedure TTokenList.Delete(const Index: Integer);

begin
  Tokens[Index].Free;
  FList.Delete(Index)
end;

destructor TTokenList.Destroy;

begin
  Clear;
  FList.Free;
  Inherited Destroy
end;

function TTokenList.GetCount: Integer;

begin
  Result := FList.Count
end;

function TTokenList.GetToken(const Index: Integer): TToken;

begin
  Result := FList[Index]
end;

end.
