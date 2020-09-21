{*******************************}
{   This unit was created by    }
{ the BoldSoft Attribute Wizard }
{      2000-02-28 10:28:51      }
{*******************************}

unit BAName;

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
// ModelName:      Name
// ExpressionName: Name
// DelphiName:     TBAName
// ContentName:    <same as for TBAString>
// PMapper:        <same as for TBAString>
// Accessor:       <same as for TBAString>
// NativeType:     <same as for TBAString>
// UnitName:       BAName    // This unit's name, modify if necessary


type
  TBAName = class(TBAString)
  end;

implementation


initialization
  BoldmemberTypes.AddMemberTypeDescriptor( TBAName, alConcrete);

finalization
  if BoldMemberTypesAssigned then
    BoldMemberTypes.RemoveDescriptorByClass(TBAName);
end.











 
 
 
 
 
 