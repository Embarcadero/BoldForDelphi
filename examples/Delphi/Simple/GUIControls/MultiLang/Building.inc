{$INCLUDE MultilangClasses_Interface.inc}
const
  Streets: array[0..4] of string = ('Bold Drive', '5th Avenue', 'Rose Path', 'Select Avenue', 'Pebble Road');


procedure TBuilding.CompleteCreate;
begin
  Address := IntToStr(Random(300)) + ' ' + Streets[Random(5)];
  TotalRent := random(4000)+2000;
  HouseTypeAttr := random( BoldSystem.ClassByExpressionName['HouseKindClass'].Count );
  M_Description.AsStringByLanguage['English'].AsString := 'English description';
  M_Description.AsStringByLanguage['Swedish'].AsString := 'Svensk beskrivning';
end;





