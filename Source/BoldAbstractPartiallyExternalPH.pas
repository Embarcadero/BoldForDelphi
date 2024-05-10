
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractPartiallyExternalPH;

interface
uses
  BoldId,
  BoldMeta,
  BoldValueInterfaces,
  BoldAbstractExternalPersistenceHandle,
  BoldAbstractpartiallyExternalPC;

type
  TBoldAbstractpartiallyExternalPH = class(TBoldAbstractExternalPersistenceHandle)
  private
    function GetDeletedObjects: TBoldObjectIdList;
    function GetNewObjects: TBoldObjectIdList;
    function GetPersistenceController: TBoldAbstractpartiallyExternalPC;
    function GetHasDeletedObjects: Boolean;
    function GetHasNewObjects: Boolean;
    function GetObjectIdByExternalKey(ExpressionName: String; ExternalKey: TBoldObjectId): TBoldObjectId;
  public
    procedure SetMultiLink(MultiLink: IBoldObjectIdListRef; ExternalKeys: TBoldObjectIdList; MoldClassOfOtherEnd: TMoldClass);
    procedure SetSingleLink(SingleLink: IBoldObjectidref; ExternalKey: TBoldObjectId; MoldClassOfOtherEnd: TMoldClass);
    function GetObjectIdByExternalKeyString(ExpressionName: String; ExternalKey: String): TBoldObjectId;
    function GetObjectIdByExternalKeyInteger(ExpressionName: String; ExternalKey: integer): TBoldObjectId;
    property PersistenceController: TBoldAbstractpartiallyExternalPC read GetPersistenceController;
    property DeletedObjects: TBoldObjectIdList read GetDeletedObjects;
    property HasDeletedObjects: Boolean read GetHasDeletedObjects;
    property NewObjects: TBoldObjectIdList read GetNewObjects;
    property HasNewObjects: Boolean read GetHasNewObjects;
  end;

implementation

uses
  BoldCoreConsts,
  BoldStringId,
  BoldDefaultId,
  BoldDefs,
  BoldGuard;

{ TBoldAbstractpartiallyExternalPH }

function TBoldAbstractpartiallyExternalPH.GetDeletedObjects: TBoldObjectIdList;
begin
  if HasPersistenceController then
    result := PersistenceController.DeletedExternalObjects
  else
    result := nil;
end;

function TBoldAbstractpartiallyExternalPH.GetHasDeletedObjects: Boolean;
begin
  result := Assigned(DeletedObjects) and (DeletedObjects.Count > 0)
end;

function TBoldAbstractpartiallyExternalPH.GetHasNewObjects: Boolean;
begin
  result := Assigned(NewObjects) and (NewObjects.Count > 0)
end;

function TBoldAbstractpartiallyExternalPH.GetNewObjects: TBoldObjectIdList;
begin
  if HasPersistenceController then
    result := PersistenceController.NewExternalObjects
  else
    result := nil;
end;

function TBoldAbstractpartiallyExternalPH.GetObjectIdByExternalKey(
  ExpressionName: String; ExternalKey: TBoldObjectId): TBoldObjectId;
var
  MoldClass: TMoldClass;
  ExternalKeys, InternalIds: TBoldObjectIdList;
  Guard: IBoldGuard;
begin
  Guard := TBoldGuard.Create(ExternalKeys, InternalIds);
  MoldClass := BoldModel.MoldModel.Classes.ItemsByExpressionName[ExpressionName];
  if not assigned(MoldClass) then
    raise EBold.CreateFmt(sInvalidClassName, [classname, expressionname]);
  ExternalKeys := TBoldObjectIdLIst.Create;
  InternalIds := TBoldObjectIdLIst.Create;
  ExternalKeys.Add(ExternalKey);
  PersistenceController.TranslateExternalKeysToInternalIds(MoldClass, ExternalKeys, InternalIds);
  if InternalIds.Count > 0 then
    result := InternalIds[0].Clone
  else
    result := nil;
end;

function TBoldAbstractpartiallyExternalPH.GetObjectIdByExternalKeyInteger(
  ExpressionName: String; ExternalKey: integer): TBoldObjectId;
var
  ExternalKeyObj: TBoldDefaultId;
begin
  ExternalKeyObj := TBoldDefaultId.Create;
  ExternalKeyObj.AsInteger := ExternalKey;
  result := GetObjectIdByExternalKey(ExpressionName, ExternalKeyObj);
  ExternalKeyObj.Free;
end;

function TBoldAbstractpartiallyExternalPH.GetObjectIdByExternalKeyString(
  ExpressionName, ExternalKey: String): TBoldObjectId;
var
  ExternalKeyObj: TBoldStringId;
begin
  ExternalKeyObj := TBoldStringId.Create;
  ExternalKeyObj.AsString := ExternalKey;
  result := GetObjectIdByExternalKey(ExpressionName, ExternalKeyObj);
  ExternalKeyObj.Free;
end;

function TBoldAbstractpartiallyExternalPH.GetPersistenceController: TBoldAbstractpartiallyExternalPC;
begin
  result := (inherited PersistenceController) as TBoldAbstractpartiallyExternalPC;
end;

procedure TBoldAbstractpartiallyExternalPH.SetMultiLink(
  MultiLink: IBoldObjectIdListRef; ExternalKeys: TBoldObjectIdList; MoldClassOfOtherEnd: TMoldClass);
begin
  PersistenceController.SetMultiLink(MultiLink, ExternalKeys, MoldClassOfOtherEnd);
end;

procedure TBoldAbstractpartiallyExternalPH.SetSingleLink(SingleLink: IBoldObjectidref; ExternalKey: TBoldObjectId; MoldClassOfOtherEnd: TMoldClass);
begin
  PersistenceController.SetSingleLink(SingleLink, ExternalKey, MoldClassOfOtherEnd);
end;

end.
