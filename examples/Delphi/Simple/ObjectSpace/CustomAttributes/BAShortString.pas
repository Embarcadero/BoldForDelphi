{*******************************}
{   This unit was created by    }
{ the BoldSoft Attribute Wizard }
{      2000-02-28 10:30:37      }
{*******************************}

unit BAShortString;

interface

uses
  BoldMemberTypeDictionary,
  BoldElements,
  BoldValueInterfaces,
  BoldAttributes,
  BoldSystem,
  BoldDefs;

// To install this attribute type in your application, add the following to your
// TypeNameHandle:
//
// ModelName:      ShortString
// ExpressionName: ShortString
// DelphiName:     TBAShortString
// ContentName:    <same as for TBAString>
// PMapper:        <same as for TBAString>
// Accessor:       <same as for TBAString>
// NativeType:     <same as for TBAString>
// UnitName:       BAShortString    // This unit's name, modify if necessary


type
  TBAShortString = class(TBAString)
  public
    {public declarations}
   function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override;
  end;

implementation

function TBAShortString.ValidateString(Value: string; Representation: TBoldRepresentation): Boolean;
begin
  case representation of
    brDefault: begin
      result := Length(Value) <= 25;
      if not result then
        SetBoldLastFailureReason(TBoldFailureReason.Create('String too long', Self));
    end
    else result := inherited ValidateString( value, Representation );
  end;
end;

initialization
  BoldmemberTypes.AddMemberTypeDescriptor( TBAShortString, alConcrete);

finalization
  if BoldMemberTypesAssigned then
    BoldMemberTypes.RemoveDescriptorByClass(TBAShortString);
end.











 
 
 
 
 
