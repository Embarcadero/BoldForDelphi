unit BoldObjectRetriever;

interface
uses
  SysUtils,
  BoldId,
  BolddefaultId,
  BoldValueSpaceInterfaces,
  BoldSystem;

function BoldRetrieveObjectByIdString(BoldSystem: TBoldSystem; IdString: String): TBoldObject;

implementation

function RetrieveObjectByInexactId(BoldSystem: TBoldSystem; ObjectId: tBoldObjectId): TBoldObject;
var
  TranslationList: TBoldIdTranslationList;
  IdList: TBoldObjectIdList;
  ExactId: TBoldObjectId;
  Locator: TBoldObjectLocator;
begin
  result := nil;
  if assigned(BoldSystem.PersistenceController) then
  begin
    TranslationList := TBoldIdTranslationList.create;
    IdList := TBoldObjectIdList.Create;
    try
      IdList.Add(ObjectId);
      BoldSystem.PersistenceController.PMExactifyIds(IdList, TranslationList);
      ExactId := TranslationList.TranslateToNewId[ObjectId];
      if ExactId.TopSortedIndexExact then
      begin
        Locator := BoldSystem.EnsuredLocatorByID[ExactId];
        if Locator.EnsuredBoldObject.BoldExistenceState = besExisting then
          result := Locator.EnsuredBoldObject;
      end;
    finally
      IdList.free;
      TranslationList.free;
    end;
  end;
end;

function BoldRetrieveObjectByIdString(BoldSystem: TBoldSystem; IdString: String): TBoldObject;
var
  ObjectId: TBoldDefaultID;
  Locator: TBoldObjectLocator;
  IdValue: integer;
begin
  result := nil;
  IdValue := StrToIntDef(IdString, -1);
  if IdValue <> -1 then
  begin
    ObjectId := TBoldDefaultID.CreateWithClassID(0, false);
    ObjectId.AsInteger := IdValue;
    try
      // Check if the locator is already in memory
      Locator := BoldSystem.Locators.LocatorByID[ObjectId];
      if assigned(Locator) then
        result := Locator.EnsuredBoldObject
      else
        result := RetrieveObjectByInexactId(BoldSystem, ObjectId);
    finally
      ObjectId.Free;
    end;
  end;
end;



end.
