/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldBase64;

// uTBase64 v1.0 - Simple Base64 encoding/decoding class
// Base64 described in RFC2045, Page 24, (w) 1996 Freed & Borenstein
// Delphi implementation (w) 1999 Dennis D. Spreen (dennis@spreendigital.de)
// This unit is freeware. Just drop me a line if this unit is useful for you.
//
// methods:
//  function EncodeData(InputData:string;var OutputData:string):byte;
//   return values:
//    BASE64_OK    = no errors, conversion successful
//    BASE64_ERROR = unknown error (e.g. can't encode octet in input stream) -> error in implementation
//
//  function DecodeData(InputData:string;var OutputData:string):byte;
//   return values:
//    BASE64_OK       = no errors, conversion successful
//    BASE64_INVALID  = invalid characters in input string (may occur only when filterdecodeinput=false)
//    BASE64_LENGTH   = input data length is not a Base64 length (mod 4)
//    BASE64_DATALEFT = too much input data left (receveived 'end of encoded data' but not end of input string)
//    BASE64_PADDING  = wrong padding (input data isn't closed with correct padding characters)
//
//  properties:
//     filterdecodeinput:boolean;  //delete all forbidden characters in input stream: Default=true
//
//  note for Delphi 1 users:
//   as with delphi 1 the max. length of a string can not exceed the length of 255 byte,
//   you may experience some problems (=errors) whilst encoding/decoding such lines.
//   but as base64 is commonly used as internet mail encoding for which the maximum
//   line size is 76 chars (=57 decoded chars) the use of this unit is not problematic.
//
// example:
//  var Base64:TBase64;
//       s1, s2:string;
//  begin
//   Base64 := TBase64.Create;
//   s1 := 'this needs to be Base64-encoded';
//   Base64.EncodeData(s1, s2);
//   ShowMessage('Encoded string:'+s2);
//   s1 := '';
//   Base64.DecodeData(s2, s1);
//   ShowMessage('Decoded string:'+s1);
//   Base64.Free;
//  end;
//

interface

uses
  BoldDefs;

type
  TBase64 = class(TObject)
  private
    ffilterdecodeinput:boolean;
    function ValueToCharacter(value: Byte; var character: char):boolean;
    function CharacterToValue(character: char; var value: byte):boolean;
    function filterLine(const InputData: string): string;
  public
    constructor Create;
    function EncodeData(InputData: TBoldAnsiString; var OutputData: string): Byte;
    function DecodeData(InputData: string; var OutputData: TBoldAnsiString): Byte;
    property FilterdecodeInput: boolean read ffilterdecodeinput write ffilterdecodeinput;
  end;

const
  BASE64_OK       = 0; // no errors, conversion successful
  BASE64_ERROR    = 1; // unknown error (e.g. can't encode octet in input stream) -> error in implementation
  BASE64_INVALID  = 2; // invalid characters in input string (may occur only when filterdecodeinput=false)
  BASE64_LENGTH   = 3; // input data length is not a Base64 length (mod 4)
  BASE64_DATALEFT = 4; // too much input data left (receveived 'end of encoded data' but not end of input string)
  BASE64_PADDING  = 5; // wrong padding (input data isn't closed with correct padding characters)

implementation

const
  AlphabetLength = 64;
  Alphabet: array[1..AlphabetLength] of AnsiChar = (
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
      'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
      'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
      'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
      'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
      'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
      'w', 'x', 'y', 'z', '0', '1', '2', '3',
      '4', '5', '6', '7', '8', '9', '+', '/');
  Pad = '=';


constructor TBase64.Create;
begin
 inherited Create;
 ffilterdecodeinput := true;
end;


//******************************************************************
// converts a value in the range of 0..AlphabetLength-1 to the
// corresponding base64 alphabet representation
// returns true if the value is in the alphabet range
//******************************************************************
function TBase64.ValueToCharacter(value: Byte; var character: char): boolean;
begin
  Result := true;
  if (value > AlphabetLength-1) then
    Result := false
  else
    character := Char(Alphabet[value+1]);
end;


//******************************************************************
// converts a character to a value in the range of 0..AlphabetLength-1
// returns true if the character exists in the alphabet
//******************************************************************
function TBase64.CharacterToValue(character: char; var value: byte): boolean;
begin
  Result := true;
  value := Pos(AnsiChar(character), Alphabet);
  if value = 0 then
    Result := false
  else
    value := value-1;
end;


//******************************************************************
// Encodes a string to its base64 representation in ASCII Format
// returns BASE64_OK if conversion was done without errors
//******************************************************************
function TBase64.EncodeData(InputData: TBoldAnsiString; var OutputData: string): Byte;
var
  i: integer;
  currentb, prevb: Byte;
  c: Byte;
  s: Char;
  InputLength: integer;
begin
  OutPutData := '';
  InputLength := Length(InputData);
  i := 1;
  if (InputLength = 0) then
  begin
    Result := BASE64_OK;
    exit;
  end;

  repeat
    // process first group
    currentb := ord(InputData[i]);
    i := i+1;
    InputLength := InputLength-1;
    c := (currentb shr 2);
    if not ValueToCharacter(c, s) then
    begin
      Result := BASE64_ERROR;
      exit;
    end;
    OutPutData := OutPutData+s;
    prevb := currentb;

    // process second group
    if InputLength = 0 then
      currentb := 0
    else
    begin
      currentb := ord(InputData[i]);
      i := i+1;
    end;
    InputLength := InputLength-1;
    c :=  (prevb and $03) shl 4 + (currentb shr 4);
    if not ValueToCharacter(c, s) then
    begin
      Result := BASE64_ERROR;
      exit;
    end;
    OutPutData := OutPutData+s;
    prevb := currentb;

    // process third group
    if InputLength<0 then
      s := pad
    else
    begin
      if InputLength=0 then
        currentb := 0
      else
      begin
        currentb := ord(InputData[i]);
        i := i+1;
      end;
      InputLength := InputLength-1;
      c := (prevb and $0F) shl 2 + (currentb shr 6);
      if not ValueToCharacter(c, s) then
      begin
        Result := BASE64_ERROR;
        exit;
      end;
    end;
    OutPutData := OutPutData+s;

    // process fourth group
    if InputLength<0 then
      s := pad
    else
    begin
      c := (currentb and $3F);
      if not ValueToCharacter(c, s) then
      begin
        Result := BASE64_ERROR;
        exit;
      end;
    end;
    OutPutData := OutPutData+s;
  until InputLength <= 0;

  result := BASE64_OK;
end;



//******************************************************************
// ignores all characters not in base64 alphabet
// and returns the filtered string
//******************************************************************
function TBase64.filterLine(const InputData: string): string;
var
  f:byte;
  i:integer;
begin
  result := '';
  for i :=  1 to Length(InputData) do
    if CharacterToValue(inputData[i], f) or (InputData[i]=Pad) then
      result := Result+InputData[i];
end;



//******************************************************************
// Decodes a base64 representation in ASCII format into a string
// returns BASE64_OK if conversion was done without errors
//******************************************************************
function TBase64.DecodeData(InputData: string; var OutputData: TBoldAnsiString): Byte;
var
  i: integer;
  InputLength: integer;
  currentb, prevb: Byte;
  c: Byte;
  s: Char;

begin
  if (InputData = '') then
  begin
    result := BASE64_OK;
    exit;
  end;
  OutPutData := '';

  if filterdecodeinput then
    InputData := FilterLine(InputData);

  InputLength := Length(InputData);
  if InputLength mod 4<>0 then
  begin
    Result := BASE64_LENGTH;
    exit;
  end;

  i := 0;
  repeat
    // process first byte
    i := i+1;
    s := InputData[i];
    if not CharacterToValue(s, currentb) then
    begin
      Result := BASE64_INVALID;
      exit;
    end;
    i := i+1;
    s := InputData[i];
    if not CharacterToValue(s, prevb) then
    begin
      Result := BASE64_INVALID;
      exit;
    end;

    c := ((currentb shl 2)+(prevb shr 4)) and 255;
    OutPutData := OutPutData + AnsiChar(Chr(c));

    // process second Byte
    i := i+1;s := InputData[i];
    if s=pad then
    begin
      if (i<>InputLength-1) then
      begin
        Result := BASE64_DATALEFT;
        exit;
      end // too much data left
      else if InputData[i+1]<>pad then
      begin
        Result := BASE64_PADDING;
        exit;
      end; // last char has to be a pad
    end
    else
    begin
      if not CharacterToValue(s, currentb) then
      begin
        Result := BASE64_INVALID;
        exit;
      end;
      c := ((prevb shl 4) + (currentb shr 2)) and 255;
      OutPutData := OutPutData + AnsiChar(Chr(c));
    end;
    // process third Byte
    i := i+1;
    s := InputData[i];
    if s=pad then
    begin
      if (i<>InputLength) then
      begin
        Result := BASE64_DATALEFT;
        exit;
      end; // too much data Left
    end
    else
    begin
      if not CharacterToValue(s, prevb) then
      begin
        Result := BASE64_INVALID;
        exit;
      end;
      c := ((currentb shl 6) + (prevb)) and 255;
      OutPutData := OutPutData + AnsiChar(Chr(c));
     end;
  until (i >= InputLength);
  result := BASE64_OK;
end;

end.
