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
    function TextToInsert: string; override;
  end;

  { TBoldCOMMethodNoPurposeProperty }
  TBoldCOMMethodNoPurposeProperty = class(TBoldOneLinerWithEvalMethodProperty)
  public
    function TextToInsert: string; override;
  end;

implementation

uses
  BoldRev;

{ TBoldCOMMethodNoPurposeProperty }

function TBoldCOMMethodNoPurposeProperty.TextToInsert: string;
begin
  Result := '  // This method server no purpose on COM clients';
end;

{ TBoldElementCOMSubscribeMethodProperty }
function TBoldElementCOMSubscribeMethodProperty.TextToInsert: string;
begin
  Result := '  Element.SubscribeToExpression('''', Subscriber.ClientID, Subscriber.SubscriberID, False, True);';
end;

initialization

end.
