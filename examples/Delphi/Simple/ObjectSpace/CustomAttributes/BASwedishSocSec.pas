{*******************************}
{   This unit was created by    }
{ the BoldSoft Attribute Wizard }
{      2000-02-28 10:33:37      }
{*******************************}

unit BASwedishSocSec;

interface

uses
  SysUtils,
  BoldMemberTypeDictionary,
  BoldElements,
  BoldValueInterfaces,
  BoldAttributes,
  BoldSystem,
  BoldDefs;

// To install this attribute type in your application, add the following to your
// TypeNameHandle:
//
// ModelName:      SwedishSocSec
// ExpressionName: SwedishSocSec
// DelphiName:     TBASwedishSocSec
// ContentName:    <same as for TBAString>
// PMapper:        <same as for TBAString>
// Accessor:       As<Name>
// NativeType:     <same as for TBAString>
// UnitName:       BASwedishSocSec    // This unit's name, modify if necessary


type
  TBASwedishSocSec = class(TBAString)
  public
    {public declarations}
    function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: AnsiChar; Representation: TBoldRepresentation): Boolean; override;
  end;

implementation

function TBASwedishSocSec.ValidateString(Value: string; Representation: TBoldRepresentation): Boolean;
var
  s:  integer;
  Sum: integer;
  i: integer;
  NewS: string;
begin
{ Validation rule:
  The number looks like: YYMMDD-NNNC
  where
   YYMMDD is date of birth
   NNN is a running number
   C is checksum

  Checksum is calculated as
  Y Y M M D D N N N
  * * * * * * * * *
  2 1 2 1 2 1 2 1 2
  -----------------
  n n n n n n n n n

  if any n is greater than 10, 9 is subtracted from it.
  S = Sum of all n
  Subtract 10 from S until S <10.
  C is now calculated as 10 - S

  More checks could be done, such as validating the date,
  but that is left as an exercise for the reader.}
  //  Only valid if string is 11 characters
  result := true;

  try
    if Length(Value) <> 11 then
    begin
      SetBoldLastFailureReason(TBoldFailureReason.Create('A Swedish social security number must be 11 characters long: YYMMDD-NNNX', self));
      result := false;
      Exit;
    end;

    if StrToInt(Copy(Value, 3, 2)) > 12 then
    begin
      SetBoldLastFailureReason(TBoldFailureReason.Create('Month must be between 1 and 12', Self));
      result := false;
      Exit;
    end;

    if StrToInt(Copy(Value, 5, 2)) > 31 then
    begin
      SetBoldLastFailureReason(TBoldFailureReason.Create('Day must be between 1 and 31', Self));
      result := false;
      Exit;
    end;
  except
    SetBoldLastFailureReason(TBoldFailureReason.Create('Illegal characters in date', Self));
    result := false;
  end;

  if Result then
  begin
    Sum := 0;
    //  7th character is a dash, as specified in ValidatePartial
    NewS := Copy(Value, 1, 6) +
            Copy(Value, 8, 3);
    for i := 1 to 9 do
    begin
      s := StrToInt(NewS[i]) * Succ(Byte(Odd(i)));
      if s > 9 then Dec(s, 9);
      Inc(Sum, s);
    end;
    Result := ((10 - (Sum mod 10)) mod 10) = StrToInt(Value[11]);
    if not result then
      SetBoldLastFailureReason(TBoldFailureReason.Create( 'Checksum incorrect!', self ));
  end;
end;

function TBASwedishSocSec.ValidateCharacter(C: AnsiChar; Representation: TBoldRepresentation): Boolean;
begin
  Result := c in ['0'..'9', '-'];
end;


initialization
  BoldmemberTypes.AddMemberTypeDescriptor( TBASwedishSocSec, alConcrete);

finalization
  if BoldMemberTypesAssigned then
    BoldMemberTypes.RemoveDescriptorByClass(TBASwedishSocSec);
end.











 
 
 
 
 
 