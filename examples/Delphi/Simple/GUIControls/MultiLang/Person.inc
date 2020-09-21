{$INCLUDE MultilangClasses_Interface.inc}

const
  FirstNames: array[0..4] of string = ('Harry', 'Peter', 'David', 'John', 'James');
  LastNames: array[0..4] of string = ('Jones', 'Flint', 'McGregor', 'Gates', 'Clinton');

procedure TPerson.CompleteCreate;
begin
  FirstName := FirstNames[Random(5)];
  LastName := LastNames[Random(5)];
  Assets := 5000 + Random(10000);
  MotherTounge := random( BoldSystem.ClassByExpressionName[BoldMLLanguageClassName].Count );
  PersonCategoryAttr := random( BoldSystem.ClassByExpressionName['PersonCategoryClass'].Count );

end;





