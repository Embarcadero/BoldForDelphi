unit BoldAttributeAutoInc;

interface

uses
  BoldDefs,
  BoldDomainElement,
  BoldElements,
  BoldSubscription,
  BoldAttributes;

type
  TBAAutoInc = class(TBAInteger)
  protected
    procedure CompleteUpdate; override;
    procedure DoSetInitialValue; override;
    procedure InitializeMember(AOwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override;
    function MayModify: Boolean; override;
  end;


implementation
uses
  BoldMemberTypeDictionary,
  BoldValueInterfaces,
  BoldSystem;

{ TBAAutoInc }

procedure TBAAutoInc.CompleteUpdate;
begin
  inherited;
  // The value has been assigned in the database as a part of the update
  // Invalidate to enforce a refetch.
  invalidate;
end;

procedure TBAAutoInc.DoSetInitialValue;
var
  IntegerContent: IBoldIntegerContent;
begin
  AsIBoldValue[bdepContents].QueryInterface(IBoldIntegerContent, IntegerContent);
  IntegerContent.asInteger := -1;
  inherited;
end;

procedure TBAAutoInc.InitializeMember(AOwningElement: TBoldDomainElement;
  ElementTypeInfo: TBoldElementTypeInfo);
begin
  inherited;
  DoSetInitialValue
end;

function TBAAutoInc.MayModify: Boolean;
begin
  result := false;
  SetBoldLastFailureReason(TBoldFailureReason.CreateFmt('AutoInc (%s) attributes can not be modified', [DisplayName], self));
end;

initialization
  BoldMemberTypes.AddMemberTypeDescriptor(TBAAutoInc, alAbstract);

finalization
  if BoldMemberTypesAssigned then
    BoldMemberTypes.RemoveDescriptorByClass(TBAAutoInc);

end.
