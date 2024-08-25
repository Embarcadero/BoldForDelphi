
{ Global compiler directives }
{$include bold.inc}
unit BoldPSDescriptions;

interface

uses
  BoldBase,
  BoldPSParams;
  
type
  TBoldPSDescriptionElement = class;
  TBoldPSSystemDescription = class;

  {---TBoldPSDescriptionElement---}
  TBoldPSDescriptionElement = class(TBoldMemoryManagedObject)
  private
    fOwner: TBoldPSDescriptionElement;
  public
    constructor Create(aOwner: TBoldPSDescriptionElement); 
    property Owner: TBoldPSDescriptionElement read fOwner;
  end;

  {---TBoldPSSystemDescription---}
  TBoldPSSystemDescription = class(TBoldPSDescriptionElement)
  public
    procedure CreatePersistentStorage(PSParams: TBoldPSParams); virtual; abstract;
  end;

implementation


{---TBoldPSDescriptionElement---}
constructor TBoldPSDescriptionElement.Create(aOwner: TBoldPSDescriptionElement);
begin
  fOwner := aOwner;
end;

end.
