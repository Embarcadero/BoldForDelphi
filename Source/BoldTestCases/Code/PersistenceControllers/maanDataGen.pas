unit maanDataGen;

interface
uses
  BoldSystem,
  BoldListHandle,
  SysUtils,
  TestModel1;

type
  TClassType = (ctClassA, ctThing, ctHitList, ctSong);

  {class for generating data}
  TDataGen = class
  public
    class function NewClassA(System: TBoldSystem): TClassA;
    class function NewThing(System: TBoldSystem): TThing;
    class function NewLinkClass(System: TBoldSystem): TLinkClass;
    class function NewSong(System: TBoldSystem): TSong;
    class function NewHitList(System: TBoldSystem): THitList;
    class procedure CreateObject(ClassType: TClassType; System: TBoldSystem);
    class function DeleteObject(List: TBoldListhandle): Boolean;
    class function GetRandomObject(List: TBoldListhandle): TBoldObject;
    class function UpdateEmbeddedState(ClassType: tClassType; Obj: TBoldObject): Boolean;
    class function UpdateNonEmbeddedState(ClassType: tClassType; Obj: TBoldObject; Attribute: TBoldObject): Boolean;
  end;

implementation

const
  FLOATCONST1: Real = 1.1;
  INTEGERCONST1: Integer = 1;
  STRINGCONST1: String = 'aa';
  BOOLEANCONST1: Boolean = true;
  CURRENCYCONST1: Currency = 0.1;

class function TDataGen.NewClassA(System: TBoldSystem): TClassA;
begin
  Result := TClassA.Create(System);
  Result.aString := STRINGCONST1;
  Result.aBoolean := BOOLEANCONST1;
  Result.aByte := INTEGERCONST1;
  Result.aCurrency := CURRENCYCONST1;
  Result.aDate := INTEGERCONST1;
  Result.aDateTime := FLOATCONST1;
  Result.aFloat := FLOATCONST1;
  Result.aInteger := INTEGERCONST1;
  Result.aShortInt := INTEGERCONST1;
  Result.aSmallInt := INTEGERCONST1;
  Result.aTime := Frac(FLOATCONST1);
  Result.aWord := INTEGERCONST1;
  Result.aBlob := STRINGCONST1;
  Result.aBlobContent := STRINGCONST1;
end;

class function TDataGen.NewThing(System: TBoldSystem): TThing;
begin
  Result := TThing.Create(System);
end;

class function TDataGen.NewLinkClass(System: TBoldSystem): TLinkClass;
begin
  Result := TLinkClass.Create(System);
end;

class function TDataGen.NewSong(System: TBoldSystem): TSong;
begin
  Result := TSong.Create(System);
  Result.Title := 'One';
end;

class function TDataGen.NewHitList(System: TBoldSystem): THitList;
begin
  Result := THitList.Create(System);
  Result.Name := 'Top10';
end;

class procedure TDataGen.CreateObject(ClassType: TClassType; System: TBoldSystem);
begin
  case ClassType of
    ctClassA: TDataGen.NewClassA(System);
    ctThing: TDataGen.NewThing(System);
    ctSong: TDataGen.NewSong(System);
    ctHitList: TDataGen.NewHitList(System);
  end;//case
end;

class function TDataGen.DeleteObject(List: TBoldListhandle): Boolean;
var
  i: integer;
begin
  if List.Count =  0 then
  begin
    Result := false;
    exit;
  end;
  i := Random(1000) mod List.Count;
  (List.List[i] as TBoldObject).Delete;
  Result := true;
end;

class function TDataGen.GetRandomObject(List: TBoldListhandle): TBoldObject;
var
  i: integer;
begin
  if List.Count =  0 then
  begin
    Result := nil;
    exit;
  end;
  i := Random(1000) mod List.Count;
  Result := (List.List[i] as TBoldObject);
end;

class function TDataGen.UpdateEmbeddedState(ClassType: tClassType; Obj: TBoldObject): Boolean;
var
  ClassAObj: TClassA;
  SongObj : TSong;
begin
  Result := false;
  case ClassType of
    ctClassA:
      begin
        ClassAObj := Obj as TClassA;
        ClassAObj.M_aString.AsString := FloatToStr(Random(100));
        Result := true;
      end;
    ctSong:
      begin
        SongObj := Obj as TSong;
        SongObj.M_Title.AsString := FloatToStr(Random(100));
        Result := true;
      end;
    else ;
  end;//case
end;

class function TDataGen.UpdateNonEmbeddedState(ClassType: tClassType; Obj: TBoldObject; Attribute: TBoldObject): Boolean;
var
  SongObj : TSong;
begin
  Result := false;
  case ClassType of
    ctSong:
      begin
        SongObj := Obj as TSong;
        SongObj.hitList.Add(Attribute as THitList);
        Result := true;
      end;
    else ;
  end;//case
end;


end.
