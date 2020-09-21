unit MLTestVSAttributes;

interface
uses
  BoldDefs,
  BoldElements,
  BoldSystem,
  BoldAttributes,
  BoldMLAttributes,
  BoldMemberTypeDictionary;

type
  TBAHouseKind = class(TBAMLValueSet)
  protected
    function Getvalues : TBAValueSetValueList; override;
  end;

  TBAPersonCategory = class(TBAMLValueSet)
  protected
    function Getvalues : TBAValueSetValueList; override;
  end;


implementation

var
  _HouseKinds : TBAMLValueSetValueList;
  _PersonCategories : TBAMLValueSetValueList;

function TBAHouseKind.GetValues : TBAValueSetValueList;
begin
  if not assigned( _HouseKinds ) then
    _HouseKinds := TBAMLValueSetValueList.Create(TBoldSystem.DefaultSystem, 'HouseKindClass', 'IntValue', ['Description', 'ShortDescription']);
  result := _HouseKinds;
end;

function TBAPersonCategory.GetValues : TBAValueSetValueList;
begin
  if not assigned( _PersonCategories ) then
    _PersonCategories := TBAMLValueSetValueList.Create(TBoldSystem.DefaultSystem, 'PersonCategoryClass', 'IntValue', ['Description', 'ShortDescription' ]);
  result := _PersonCategories;
end;


initialization
  with BoldMemberTypes do begin
    AddMemberTypeDescriptor( TBAHouseKind, alConcrete);
    AddMemberTypeDescriptor( TBAPersonCategory, alConcrete);
  end;
finalization
  If BoldMemberTypesAssigned then
    with BoldMemberTypes do begin
      RemoveDescriptorByClass(TBAHouseKind);
      RemoveDescriptorByClass(TBAPersonCategory);
    end;
end.
