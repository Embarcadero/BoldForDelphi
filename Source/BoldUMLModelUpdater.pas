{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelUpdater;

interface

uses
  BoldUMLModel;

type
  { forward declarations }
  TBoldUMLModelUpdater = class;

  { TBoldUMLModelUpdater }
  TBoldUMLModelUpdater = class(TObject)
  private
    class procedure UpdatePersistentTVs(aList: TUMLModelElementList);
  public
    class procedure UpdateModel(ModelToUpdate: TUMLModel);
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldDefs,
  BoldDefaultTaggedValues,
  BoldUMLTaggedValues;

{ TBoldUMLModelUpdater }

class procedure TBoldUMLModelUpdater.UpdateModel(ModelToUpdate: TUMLModel);
var
  aList: TUMLModelElementList;
begin
  aList := ModelToUpdate.BoldSystem.ClassByExpressionName['UMLAssociation'] as TUMLModelElementList;
  UpdatePersistentTVs(aList);
  aList := ModelToUpdate.BoldSystem.ClassByExpressionName['UMLAttribute'] as TUMLModelElementList;
  UpdatePersistentTVs(aList);
end;

class procedure TBoldUMLModelUpdater.UpdatePersistentTVs(aList: TUMLModelElementList);
var
  anElement: TUMLModelElement;
  i: Integer;
  OldPersistentValue: String;
begin
  for i := 0 to aList.Count - 1 do
  begin
    anElement := aList[i];
    OldPersistentValue := anElement.GetBoldTV(TV_PERSISTENT_OLD);
    if OldPersistentValue <> '' then
    begin
      if StringToBoolean(OldPersistentValue) then
        anElement.EnsureTaggedValue(TAG_PERSISTENCE, TV_PERSISTENCE_PERSISTENT)
      else
        anElement.SetTaggedValue(TAG_PERSISTENCE, TV_PERSISTENCE_TRANSIENT);
      anElement.DeleteTaggedValue(BOLDTVPREFIX + TV_PERSISTENT_OLD);
    end;
  end;
end;

end.
