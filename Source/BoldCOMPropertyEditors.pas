
{ Global compiler directives }
{$include bold.inc}
unit BoldCOMPropertyEditors;

interface

uses
  BoldAbstractPropertyEditors;

type
  { forward declarations }
  TBoldElementCOMSubscribeMethodProperty = class;
  TBoldCOMMethodNoPurposeProperty = class;

  { TBoldElementCOMSubscribeMethodProperty }
  TBoldElementCOMSubscribeMethodProperty = class(TBoldOneLinerWithEvalMethodProperty)
  public
    function ImplementationTextToInsert: string; override;
  end;

  { TBoldCOMMethodNoPurposeProperty }
  TBoldCOMMethodNoPurposeProperty = class(TBoldOneLinerWithEvalMethodProperty)
  public
    function ImplementationTextToInsert: string; override;
  end;

implementation


{ TBoldCOMMethodNoPurposeProperty }

function TBoldCOMMethodNoPurposeProperty.ImplementationTextToInsert: string;
begin
  Result := '  // This method server no purpose on COM clients';
end;

{ TBoldElementCOMSubscribeMethodProperty }
function TBoldElementCOMSubscribeMethodProperty.ImplementationTextToInsert: string;
begin
  Result := '  Element.SubscribeToExpression('''', Subscriber.ClientID, Subscriber.SubscriberID, False, True);';
end;

end.
