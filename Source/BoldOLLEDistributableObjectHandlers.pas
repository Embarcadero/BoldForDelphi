
{ Global compiler directives }
{$include bold.inc}
unit BoldOLLEDistributableObjectHandlers;

interface

uses
  Windows,
  BoldDefs,
  BoldId,
  BoldDefaultId,
  BoldGlobalId,
  DistributableInfo,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldFreeStandingValues,
  BoldElements,
  BoldSystem,
  BoldCondition,
  BoldPersistenceControllerDefault,
  BoldPMappers,
  BoldPMappersDefault,
  BoldPersistenceController,
  BoldDbInterfaces,
  BoldOLLEdmmain;



const
  BOLD_OLL_IDATTRIBUTECOLUMN_NAME = 'LOCALID';
  BOLD_OLL_PSIDATTRIBUTECOLUMN_NAME = 'GLOBALID';
  BOLD_OLL_NAMEOFCLASSATTRIBUTECOLUMN_NAME = 'NAMEOFCLASS';
  
type
  TBoldPSId = string;
  TBoldDistributableObjectHandler = class;
  TBoldForeignObjectHandler = class;
  TBoldOwnObjecthandler = class;
  TBoldBrokenLinkResolver = class;

  TBoldLinkResolveAction = (blraCut, blraAbort, blraFailObject, blraIgnore, blraMissing);

  TBoldBrokenLinkResolver = class
  private
    fHeldObjectAction: TBoldLinkResolveAction;
    fNonheldObjectAction: TBoldLinkResolveAction;
  public
    function ResolveBrokenLink(ObjectContents: IBoldObjectContents; MemberIndex: Integer; Hold: Boolean): Boolean;
    property HeldObjectAction: TBoldLinkResolveAction read fHeldObjectAction write fHeldObjectAction;
    property NonheldObjectAction: TBoldLinkResolveAction read fNonheldObjectAction write fNonheldObjectAction;
  end;

  TBoldDistributableObjectHandler = class
  private
    fPController: TBoldPersistenceControllerDefault;
    fOllSystem: TBoldSystem;
    fBrokenLinkResolver: TBoldBrokenLinkResolver;
    fMyTransaction: Boolean;
    fTheMapping: TMapping;
    function TheMapping: TMapping;
    function LookupInfoByLocalId(LocalId: TBoldDefaultId): TDistributableObjectInfo;
    procedure AddToMapping(anObj: TDistributableObjectInfo);
    procedure GetLocalIdsFor(InfoObjects: TDistributableObjectInfoList; IdList: TBoldObjectIdList);
    function GetForeignPSInfo(PSId: TBoldPSId): TForeignPSInfo;
    procedure ExtractAllIds(IdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; OutIdList: TBoldObjectIdList);
    procedure Fetch(IdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace);
    procedure GetInfoObjectsFor(IdList: TBoldObjectIdList; InfoObjectList: TDistributableObjectInfoList; RemainingIdList: TBoldObjectIdList);
    procedure NewOwnInfoObjectsFor(IdList: TBoldObjectIdList; InfoObjectList: TDistributableObjectInfoList);
    procedure NewForeignInfoObjectsFor(IdList: TBoldObjectIdList; InfoObjectList: TDistributableObjectInfoList; Owner: TForeignPSInfo; Hold: Boolean = false);
    procedure MakeGlobalTranslationListFor(ValueSpace: IBoldValueSpace; IdList: TBoldObjectIdList; GlobalTranslationList: TBoldIdTranslationList);
    procedure MakeLocalizingTranslationList(ValueSpace: IBoldValueSpace; GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
    procedure SearchByOcl(OclExpr: string; IdList: TBoldObjectIdList);
    procedure Update(ValueSpace: IBoldValueSpace; IdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType);
    procedure LockAndFreeObjects(IdList, FreeList: TBoldObjectIdList);
    procedure VerifyAssociations(ValueSpace: IBoldValueSpace; IdList, HoldList: TBoldObjectIdList);
    procedure StartTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;
  public
    property PController: TBoldPersistenceControllerDefault read fPController write fPController;
    property OllSystem: TBoldSystem read fOllSystem write fOllSystem;
    property BrokenLinkResolver: TBoldBrokenLinkResolver read fBrokenLinkResolver write fBrokenLinkResolver;
  end;

  TBoldForeignObjectHandler = class(TBoldDistributableObjectHandler)
  private
    procedure PutObjects(ValueSpace: IBoldValueSpace; IdList, HoldList: TBoldObjectIdList; NewLocalTimeStamp: Integer; Owner: TForeignPSInfo);
    procedure StartCheckInObjects(ValueSpace: IBoldValueSpace; IdList, ReleaseList: TBoldObjectIdList; Owner: TForeignPSInfo);
    procedure ReleaseObjects(ValueSpace: IBoldValueSpace; IdList: TBoldObjectIdList);
    procedure UnReleaseObjects(IdList: TBoldObjectIdList);
  public
    procedure Put(ValueSpace: IBoldValueSpace; IdList: TBoldObjectIdList; HoldList: TBoldObjectIdList; Owner: TBoldPSId);
    procedure StartCheckIn(IdList, ReleaseList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Owner: TBoldPSId);
    procedure AcknowledgeCheckIn(Owner: TBoldPSId; NewTimeStamp: Integer);
    procedure FailCheckIn(Owner: TBoldPSId);
    procedure ObjectsFrom(Owner: TBoldPSId; Objects: TBoldObjectIdList);
    procedure HeldObjectsFrom(Owner: TBoldPSId; Objects: TBoldObjectIdList);
    procedure ModifiedObjectsFrom(Owner: TBoldPSId; Objects: TBoldObjectIdList);
  end;

  TBoldOwnObjectHandler = class(TBoldDistributableObjectHandler)
  private
    procedure GetObjects(ValueSpace: IBoldValueSpace; IdList, HoldList: TBoldObjectIdList; Holder: TForeignPSinfo);
    procedure CheckInObjects(ValueSpace: IBoldValueSpace; IdList, ReleaseList: TBoldObjectIdList; Holder: TForeignPSInfo);
    procedure ReserveObjects(ValueSpace: IBoldValueSpace; HoldList: TBoldObjectIdList);
    procedure UnCheckOutObjects(IdList: TBoldObjectIdList; Holder: TForeignPSInfo);
    procedure InternalGet(IdList, HoldList: TBoldObjectIdList; Holder: TBoldPSId; ValueSpace: IBoldValueSpace);
  public
    procedure GetSynch(ForeignPS: TBoldPSId; IdList: TBoldObjectIdList; Valuespace: IBoldValueSpace);
    procedure AcknowledgeSynch(ForeignPS: TBoldPSId);
    procedure FailSynch(ForeignPS: TBoldPSId);
    procedure Get(IdList, HoldList: TBoldObjectIdList; Holder: TBoldPSId; ValueSpace: IBoldValueSpace);
    procedure CheckIn(ValueSpace: IBoldValueSpace; IdList, ReleaseList: TBoldObjectIdList; Holder: TBoldPSId; var NewTimeStamp: Integer);
    procedure UnCheckOut(IdList: TBoldObjectIdList; Holder: TBoldPSId);
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldDomainElement;

procedure AddObjectToIdList(aDistributableInfo: TDistributableObjectInfo; anIdList: TBoldObjectIdList);
var
  anId: TBoldDefaultId;
begin
  anId := TBoldDefaultId.CreateWithClassId(BUSINESSCLASSESROOT_TOPSORTEDINDEX, false);

  anId.AsInteger := aDistributableInfo.LocalId;
  anIdList.Add(anId);

  anId.Free;
end;


{ TForeignObjectHandler }

procedure TBoldForeignObjectHandler.PutObjects(
  ValueSpace: IBoldValueSpace; IdList, HoldList: TBoldObjectIdList; NewLocalTimeStamp: Integer; Owner: TForeignPSInfo);
var
  InfoObjects: TDistributableObjectInfoList;
  i: Integer;
  anObjectId: TBoldDefaultId;
  anObject: TBoldObject;
  MissingInfos: TBoldObjectIdList;
  aForeignObjectInfo: TForeignObjectInfo;
begin
  InfoObjects := TDistributableObjectInfoList.Create;
  anObjectId := TBoldDefaultId.Create;
  MissingInfos := TBoldObjectIdList.Create;
  try
    GetInfoObjectsFor(IdList, InfoObjects, MissingInfos);
    NewForeignInfoObjectsFor(MissingInfos, InfoObjects, Owner);
    for i := 0 to InfoObjects.Count - 1 do
    begin
      anObject := InfoObjects[i];
      if not (anObject is TForeignObjectInfo) then
        raise EBold.CreateFmt(sObjectNotForeign, [Classname]);
      aForeignObjectInfo := anObject as TForeignObjectInfo;
      if not (aForeignObjectInfo.Owner = Owner) then
        raise EBold.CreateFmt(sWrongOwner, [Classname]);
      anObjectId.AsInteger := aForeignObjectInfo.LocalId;
      aForeignObjectInfo.Put(ValueSpace, HoldList.IdInList[anObjectId], NewLocalTimeStamp);
    end;
{    for i := 0 to MissingInfos.Count - 1 do
    begin
      aForeignObjectInfo := TForeignObjectInfo.Create(OllSystem);
      aForeignObjectInfo.LocalId := (MissingInfos[i] as TBoldDefaultId).AsInteger;
      aForeignObjectInfo.Owner := Owner;
      aForeignObjectInfo.Put(ValueSpace, HoldList.IdInList[MissingInfos[i]], NewLocalTimeStamp);
    end;}
  finally
    InfoObjects.Free;
    anObjectId.Free;
    MissingInfos.Free;
  end;
end;

procedure TBoldForeignObjectHandler.Put(ValueSpace: IBoldValueSpace; IdList: TBoldObjectIdList; HoldList: TBoldObjectIdList; Owner: TBoldPSId);
var
  TranslationList, TranslationList2: TBoldIdTranslationList;
  IdList2, HoldList2: TBoldObjectIdList;
  NewLocalTimeStamp: Integer;
begin
  TranslationList := TBoldIdTranslationList.Create;
  TranslationList2 := TBoldIdTranslationList.Create;
  IdList2 := IdList.Clone;
  HoldList2 := HoldList.Clone;

  try
    StartTransaction;
    try
      MakeLocalizingTranslationList(ValueSpace, IdList, TranslationList);

      ValueSpace.ApplyTranslationList(TranslationList);
      IdList2.ApplyTranslationList(TranslationList);
      HoldList2.ApplyTranslationList(TranslationList);

      VerifyAssociations(ValueSpace, IdList2, HoldList2);
      Update(ValueSpace, IdList2, TranslationList2, NewLocalTimeStamp);
      IdList2.ApplyTranslationList(TranslationList2);
      HoldList2.ApplyTranslationList(TranslationList2);

      LockAndFreeObjects(IdList2, HoldList2);
      PutObjects(ValueSpace, IdList2, HoldList2, NewLocalTimeStamp, GetForeignPSInfo(Owner));
      OllSystem.UpdateDatabase;

      CommitTransaction;
    except
      RollbackTransaction;
      raise;
    end;
  finally
    IdList2.Free;
    HoldList2.Free;
    TranslationList.Free;
    TranslationList2.Free;
  end;
end;

procedure TBoldForeignObjectHandler.ReleaseObjects(ValueSpace: IBoldValueSpace;
  IdList: TBoldObjectIdList);
begin
  PController.PMSetReadonlyness(IdList, nil);
end;

procedure TBoldForeignObjectHandler.StartCheckIn(IdList,
  ReleaseList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Owner: TBoldPSId);
var
  GlobalTranslationList: TBoldIdTranslationList;
  OwnerForeignPSInfo: TForeignPSInfo;
begin
  GlobalTranslationList := TBoldIdTranslationList.Create;
  try
    StartTransaction;
    try
      OwnerForeignPSInfo := GetForeignPSInfo(Owner);
      if OwnerForeignPSInfo.IsCheckingIn then
        raise EBold.CreateFmt(sObjectsAlreadyBeingCheckedIn, [Classname]);
      Fetch(Idlist, ValueSpace);
      StartCheckInObjects(ValueSpace, IdList, ReleaseList, OwnerForeignPSInfo);
      ReleaseObjects(ValueSpace, ReleaseList);

      MakeGlobalTranslationListFor(ValueSpace, IdList, GlobalTranslationList);
      ValueSpace.ApplytranslationList(GlobalTranslationList);
      IdList.ApplyTranslationList(GlobalTranslationList);
      ReleaseList.ApplyTranslationList(GlobalTranslationList);

      OllSystem.UpdateDatabase;

      CommitTransaction;
    except
      RollbackTransaction;
      raise;
    end;
  finally
    GlobalTranslationList.Free;
  end;
end;

procedure TBoldForeignObjectHandler.StartCheckInObjects(
  ValueSpace: IBoldValueSpace; IdList, ReleaseList: TBoldObjectIdList; Owner: TForeignPSinfo);
var
  i: Integer;
  InfoObjectList: TDistributableObjectInfoList;
  NewInfoObjectList: TBoldObjectIdList;
  aDistributableObjectInfo: TDistributableObjectInfo;
  anId: TBoldDefaultId;
begin
  InfoObjectList := TDistributableObjectInfoList.Create;
  NewInfoObjectList := TBoldObjectIdList.Create;
  anId := TBoldDefaultID.Create;
  try
    GetInfoObjectsFor(IdList, InfoObjectList, NewInfoObjectList);
    NewForeignInfoObjectsFor(NewInfoObjectList, InfoObjectList, Owner, True);
    for i := 0 to InfoObjectList.Count - 1 do
    begin
      aDistributableObjectInfo := InfoObjectList[i] as TDistributableObjectInfo;
      anId.AsInteger := aDistributableObjectInfo.LocalId;
      aDistributableObjectInfo.StartCheckIn(ValueSpace, not ReleaseList.IdInList[anId]);
    end;
  finally
    InfoObjectList.Free;
    NewInfoObjectList.Free;
    anId.Free;
  end;
end;

procedure TBoldForeignObjectHandler.AcknowledgeCheckIn(Owner: TBoldPSId;
  NewTimeStamp: Integer);
begin
  GetForeignPSInfo(Owner).AcknowledgeCheckIn(NewTimeStamp);
  OllSystem.UpdateDatabase;
end;

procedure TBoldForeignObjectHandler.FailCheckIn(Owner: TBoldPSId);
var
  FailedReleaseList: TBoldObjectIdList;
begin
  FailedReleaseList := TBoldObjectIdList.Create;
  try
    StartTransaction;
    try
      GetForeignPSInfo(Owner).FailCheckIn(FailedReleaseList);
      UnReleaseObjects(FailedReleaseList);
      OllSystem.UpdateDatabase;

      CommitTransaction;
    except
      RollbackTransaction;
      raise;
    end;
  finally
    FailedReleaseList.Free;
  end;
end;

procedure TBoldForeignObjectHandler.UnReleaseObjects(
  IdList: TBoldObjectIdList);
begin
  PController.PMSetReadonlyness(nil, IdList);
end;

procedure TBoldForeignObjectHandler.HeldObjectsFrom(Owner: TBoldPSId;
  Objects: TBoldObjectIdList);
var
  anObjectList: TForeignObjectInfoList;
  i: Integer;
begin
  Objects.Clear;
  anObjectList := GetForeignPSInfo(Owner).OwnedObjectInfos;
  anObjectList.EnsureObjects;
  for i := 0 to anObjectList.Count-1 do
    if assigned(anObjectList[i].HeldObjectInfo) then
      AddObjectToIdList(anObjectList[i], Objects);
end;

procedure TBoldForeignObjectHandler.ModifiedObjectsFrom(Owner: TBoldPSId;
  Objects: TBoldObjectIdList);
var
  i: Integer;
  anObjectList: TBoldObjectList;
  anElement: TBoldIndirectElement;
  anObjectIdList: TBoldObjectIdList;
  aValueSpace: TBoldFreeStandingValueSpace;

  function DifferentTimeStamp: Boolean;
  var
    anObjectContents: IBOldObjectContents;
  begin
    anObjectContents := (aValueSpace as IBoldValueSpace).GetObjectContentsByObjectId(anObjectIdList[i]);
    result := anObjectContents.TimeStamp <>
              (anObjectList[i] as THeldObjectInfo).OriginalLocalTimeStamp;
  end;

begin
  anElement := TBoldIndirectElement.Create;
  anObjectIdList := TBoldObjectIdList.Create;
  aValueSpace := TBoldFreeStandingValueSpace.Create;
  try
    GetForeignPSInfo(Owner).EvaluateExpression('ownedObjectInfos.heldObjectInfo', anElement);
    anObjectList := anElement.Value as TBoldObjectList;
    anObjectList.EnsureObjects;
    for i := 0 to anObjectList.Count-1 do
      AddObjectToIdList((anObjectList[i] as THeldObjectInfo).ForeignObjectInfo, anObjectIdList);
    Fetch(anObjectIdList, aValueSpace);
    for i := 0 to anObjectList.Count-1 do
      if DifferentTimeStamp then
        Objects.Add(anObjectIdList[i]);
  finally
    anObjectIdList.Free;
    aValueSpace.Free;
    anElement.Free;
  end;
end;

procedure TBoldForeignObjectHandler.ObjectsFrom(Owner: TBoldPSId;
  Objects: TBoldObjectIdList);
var
  anObjectList: TForeignObjectInfoList;
  i: Integer;
begin
  Objects.Clear;
  anObjectList := GetForeignPSInfo(Owner).OwnedObjectInfos;
  anObjectList.EnsureObjects;
  for i := 0 to anObjectList.Count-1 do
    AddObjectToIdList(anObjectList[i], Objects);
end;


{ TOwnObjectHandler }

procedure TBoldOwnObjectHandler.AcknowledgeSynch(ForeignPS: TBoldPSId);
var
  aForeignPS: TForeignPSInfo;
begin
  aForeignPS := GetForeignPSInfo(ForeignPS);
  aForeignPS.LastSynchTimestamp := aForeignPS.OngoingSynchTimestamp;
  aForeignPS.OngoingSynchTimestamp := -1;
  OllSystem.UpdateDatabase;
end;

procedure TBoldOwnObjectHandler.CheckIn(ValueSpace: IBoldValueSpace; IdList,
  ReleaseList: TBoldObjectIdList; Holder: TBoldPSId; var NewTimeStamp: Integer);
var
  TranslationList, TranslationList2: TBoldIdTranslationList;
  IdList2, ReleaseList2: TBoldObjectIdList;
begin
  TranslationList := TBoldIdTranslationList.Create;
  TranslationList2 := TBoldIdTranslationList.Create;
  IdList2 := IdList.Clone;
  ReleaseList2 := ReleaseList.Clone;

  try
    StartTransaction;
    try
      MakeLocalizingTranslationList(ValueSpace, IdList, TranslationList);

      ValueSpace.ApplyTranslationList(TranslationList);
      IdList2.ApplyTranslationList(TranslationList);
      ReleaseList2.ApplyTranslationList(TranslationList);

      VerifyAssociations(ValueSpace, IdList2, IdList2);
      Update(ValueSpace, IdList2, TranslationList2, NewTimeStamp);
      IdList2.ApplyTranslationList(TranslationList2);
      ReleaseList2.ApplyTranslationList(TranslationList2);

      LockAndFreeObjects(IdList2, ReleaseList2);
      CheckInObjects(ValueSpace, IdList2, ReleaseList2, GetForeignPSInfo(Holder));
      OllSystem.UpdateDatabase;

      CommitTransaction;
    except
      RollbackTransaction;
      raise;
    end;
  finally
    IdList2.Free;
    ReleaseList2.Free;
    TranslationList.Free;
    TranslationList2.Free;
  end;
end;

procedure TBoldOwnObjectHandler.CheckInObjects(ValueSpace: IBoldValueSpace;
  IdList, ReleaseList: TBoldObjectIdList; Holder: TForeignPSInfo);
var
  InfoObjects: TDistributableObjectInfoList;
  NewInfoIds: TBoldObjectIdList;
  i: Integer;
  anObject: TBoldObject;
  anOwnObjectInfo: TOwnObjectInfo;
  anObjectId: TBoldDefaultId;
begin
  InfoObjects := TDistributableObjectInfoList.Create;
  NewInfoIds := TBoldObjectIdList.Create;
  anObjectId := TBoldDefaultId.Create;
  try
    GetInfoObjectsFor(IdList, InfoObjects, NewInfoIds);
    for i := 0 to InfoObjects.Count - 1 do
    begin
      anObject := InfoObjects[i];
      if not (anObject is TOwnObjectInfo) then
        raise EBold.CreateFmt(sObjectNotOwned, [Classname, 'CheckInObjects']); // do not localize
      anOwnObjectInfo := anObject as TOwnObjectInfo;
      anObjectId.AsInteger := anOwnObjectInfo.LocalId;
      anOwnObjectInfo.CheckIn(ValueSpace, ReleaseList.IdInList[anObjectId], Holder);
    end;
    for i := 0 to NewInfoIds.Count - 1 do
    begin
      anOwnObjectInfo := TOwnObjectInfo.Create(OllSystem);
      anOwnObjectInfo.LocalId := (NewInfoIds[i] as TBoldDefaultId).AsInteger;
      AddToMapping(anOwnObjectInfo);
      anOwnObjectInfo.CheckedOutObjectInfo := TCheckedOutObjectInfo.Create(OllSystem);
      anOwnObjectInfo.CheckedOutObjectInfo.Holder := Holder;
      anOwnObjectInfo.CheckIn(ValueSpace, ReleaseList.IdInList[NewInfoIds[i]], Holder);
    end;
  finally
    InfoObjects.Free;
    NewInfoIds.Free;
    anObjectId.Free;
  end;
end;

procedure TBoldOwnObjectHandler.FailSynch(ForeignPS: TBoldPSId);
begin
  GetForeignPSInfo(ForeignPS).OngoingSynchTimestamp := -1;
  OllSystem.UpdateDatabase;
end;

procedure TBoldOwnObjectHandler.Get(IdList, HoldList: TBoldObjectIdList;
  Holder: TBoldPSId; ValueSpace: IBoldValueSpace);
begin
  StartTransaction;
  try
    InternalGet(IdList, HoldList, Holder, ValueSpace);

    OllSystem.UpdateDatabase;
    CommitTransaction;
  except
    RollbackTransaction;
    raise;
  end;
end;

procedure TBoldOwnObjectHandler.GetObjects(ValueSpace: IBoldValueSpace; IdList,
  HoldList: TBoldObjectIdList; Holder: TForeignPSInfo);
var
  i: Integer;
  InfoObjectList: TDistributableObjectInfoList;
  NewInfoObjectList: TBoldObjectIdList;
  aDistributableObjectInfo: TDistributableObjectInfo;
  anId: TBoldDefaultId;
begin
  InfoObjectList := TDistributableObjectInfoList.Create;
  NewInfoObjectList := TBoldObjectIdList.Create;
  anId := TBoldDefaultID.Create;
  try
    GetInfoObjectsFor(IdList, InfoObjectList, NewInfoObjectList);
    NewOwnInfoObjectsFor(NewInfoObjectList, InfoObjectList);
    for i := 0 to InfoObjectList.Count - 1 do
    begin
      aDistributableObjectInfo := InfoObjectList[i];
      anId.AsInteger := aDistributableObjectInfo.LocalId;
      aDistributableObjectInfo.Get(ValueSpace, HoldList.IdInList[anId], Holder);
    end;
  finally
    InfoObjectList.Free;
    NewInfoObjectList.Free;
    anId.Free;
  end;
end;

procedure TBoldOwnObjectHandler.GetSynch(ForeignPS: TBoldPSId;
  IdList: TBoldObjectIdList; Valuespace: IBoldValueSpace);
var
  aCond: TBoldTimestampCondition;
  i: Integer;
  HoldList: TBoldObjectIdList;
  MaxTimestamp: Integer;
  aTimestamp: Integer;
  aForeignPS: TForeignPSInfo;
  InfoObjects: TDistributableObjectInfoList;
  ChangedObjects: TBoldObjectIdList;
  MissingIds: TBoldObjectIdList;
begin
  StartTransaction;
  try
    aForeignPS := GetForeignPSInfo(ForeignPS);
    if aForeignPS.IsSynching then
      raise EBold.CreateFmt(sSynchInProgress, [classname]);

    ChangedObjects := TBoldObjectIdList.Create;
    HoldList := TBoldObjectIdList.Create;
    aCond := TBoldTimestampCondition.create;
    InfoObjects := TDistributableObjectInfoList.Create;
    MissingIds := TBoldObjectIdList.Create;
    try
      aCond.Timestamp := aForeignPS.LastSynchTimestamp;
      fPController.PMFetchIDListWithCondition(ChangedObjects, Valuespace, fmDistributable, aCond, -1);
      GetInfoObjectsFor(ChangedObjects, InfoObjects, MissingIds);
      NewOwnInfoObjectsFor(MissingIds, InfoObjects);
      for i := InfoObjects.Count-1 downto 0 do
        if not (InfoObjects[i] is TOwnObjectInfo) then
          InfoObjects.RemoveByIndex(i);

      GetLocalIdsFor(InfoObjects, IdList);
      InternalGet(IdList, HoldList, ForeignPS, ValueSpace);
      MaxTimestamp := 0;
      for i := 0 to IdList.Count-1 do
      begin
        aTimestamp := Valuespace.ObjectContentsByObjectId[IdList[i]].TimeStamp;
        if aTimestamp > MaxTimestamp then
          MaxTimestamp := aTimestamp;
      end;
      aForeignPS.OngoingSynchTimestamp := MaxTimestamp;
    finally
      ChangedObjects.Free;
      HoldList.Free;
      aCond.Free;
      InfoObjects.Free;
    end;

    OllSystem.UpdateDatabase;
    CommitTransaction;
  except
    RollbackTransaction;
    raise;
  end;
end;

procedure TBoldOwnObjectHandler.InternalGet(IdList,
  HoldList: TBoldObjectIdList; Holder: TBoldPSId;
  ValueSpace: IBoldValueSpace);
var
  GlobalTranslationList: TBoldIdTranslationList;
begin
  GlobalTranslationList := TBoldIdTranslationList.Create;
  try
    Fetch(Idlist, ValueSpace);
    GetObjects(ValueSpace, IdList, HoldList, GetForeignPSInfo(Holder));
    ReserveObjects(ValueSpace, HoldList);

    MakeGlobalTranslationListFor(ValueSpace, IdList, GlobalTranslationList);
    ValueSpace.ApplytranslationList(GlobalTranslationList);
    IdList.ApplyTranslationList(GlobalTranslationList);
    HoldList.ApplyTranslationList(GlobalTranslationList);
  finally
    GlobalTranslationList.Free;
  end;
end;

procedure TBoldOwnObjectHandler.ReserveObjects(ValueSpace: IBoldValueSpace;
  HoldList: TBoldObjectIdList);
begin
  PController.PMSetReadonlyness(HoldList, nil);
end;


procedure TBoldOwnObjectHandler.UnCheckOut(IdList: TBoldObjectIdList;
  Holder: TBoldPSId);
var
  EmptyIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList;
begin
  TranslationList := TBoldIDTranslationList.Create;
  EmptyIdList := TBoldObjectIdList.Create;
  try
    StartTransaction;
    try
      PController.PMTranslateToLocalIds(IdList, TranslationList);
      IdList.ApplyTranslationList(TranslationList);
      UnCheckOutObjects(IdList, GetForeignPSInfo(Holder));
      LockAndFreeObjects(EmptyIdList, IdList);
      OllSystem.UpdateDatabase;

      CommitTransaction;
    except
      RollbackTransaction;
      raise;
    end;
  finally
    TranslationList.Free;
    EmptyIdList.Free;
  end;
end;

procedure TBoldOwnObjectHandler.UnCheckOutObjects(IdList: TBoldObjectIdList;
  Holder: TForeignPSInfo);
var
  InfoObjects: TDistributableObjectInfoList;
  i: Integer;
  anOwnObjectInfo: TOwnObjectInfo;
  MissingList: TBoldObjectIdList;
begin
  InfoObjects := TDistributableObjectInfoList.Create;
  MissingList := TBoldObjectIdList.Create;
  try
    GetInfoObjectsFor(IdList, InfoObjects, MissingList);
    for i := 0 to InfoObjects.Count - 1 do
    begin
      if not (InfoObjects[i] is TOwnObjectInfo) then
        raise EBold.CreateFmt(sObjectNotOwned, [Classname, 'UnCheckOutObjects']); // do not localize
      anOwnObjectInfo := InfoObjects[i] as TOwnObjectInfo;
      anOwnObjectInfo.UnCheckOut(Holder);
    end;
  finally
    InfoObjects.Free;
    MissingList.Free;
  end;
end;

{ TDistributableObjectHandler }

function TBoldDistributableObjectHandler.GetForeignPSInfo(
  PSId: TBoldPSId): TForeignPSInfo;
var
  aCondition: TBoldSQLCondition;
  PSInfoObjectIdList: TBoldObjectIdList;
  aLocator: TBoldObjectLocator;
begin
  aCondition := TBoldSQLCondition.Create;
  PSInfoObjectIdList := TBoldObjectIdList.Create;
  try
{    if assigned(OllSystem.PersistenceController) then
    begin
      aCondition.TopSortedIndex := OllSystem.BoldSystemTypeInfo.ClassTypeInfoByModelName['ForeignPSInfo'].TopSortedIndex;
      aCondition.WhereFragment :=  BOLD_OLL_PSIDATTRIBUTECOLUMN_NAME + ' = ''' + PSId + '''';
      OllSystem.PersistenceController.PMFetchIDListWithCondition(PSInfoObjectIdList, OllSystem.AsIBoldvalueSpace[bdepPMIn], fmNormal, aCondition, 0);
    end else}
      SearchByOcl('ForeignPSInfo.allInstances->select(globalId = ''' + PSId + ''')', PSInfoObjectIdList);
    if PSInfoObjectIdList.Count = 0 then
    begin
      result := TForeignPSInfo.Create(OllSystem);
      result.GlobalID := PSId;
    end else
    begin
      assert(PSInfoObjectIdList.Count = 1);
      aLocator := OllSystem.EnsuredLocatorByID[PSInfoObjectIdList[0]];
      aLocator.EnsureBoldObject;
      result := aLocator.BoldObject as TForeignPSInfo;
    end;
  finally
    aCondition.Free;
    PSInfoObjectIdList.Free;
  end;
end;

procedure TBoldDistributableObjectHandler.ExtractAllIds(IdList: TBoldObjectIdList;
  ValueSpace: IBoldValueSpace; OutIdList: TBoldObjectIdList);

  procedure EnsureIdInList(Id: TBoldObjectId; IdList: TBoldObjectIdList);
  begin
    if assigned(Id) and
      not IdList.IdInList[Id] then
      IdList.Add(Id);
  end;

var
  i, j: Integer;
  anObject: IBoldObjectContents;
  aValue: IBoldValue;
  IdRef: IBoldObjectIdRef;
  IdRefPair: IBoldObjectIdRefPair;
begin
  for i := 0 to IdList.Count - 1 do
  begin
    EnsureIdInList(IdList[i], OutIdList);
    anObject := ValueSpace.ObjectContentsByObjectId[IdList[i]];
    if not assigned(anObject) then
      anObject := ValueSpace.ObjectContentsByObjectId[IdList[i]];
    for j := 0 to anObject.MemberCount - 1 do
    begin
      aValue := anObject.ValueByIndex[j];
      if assigned(aValue) then
        if aValue.QueryInterface(IBoldObjectIdRef, IdRef) = S_OK then
          EnsureIdInList(IdRef.Id, OutIdList)
        else if aValue.QueryInterface(IBoldObjectIdRefPair, IdRefPair) = S_OK then
        begin
          EnsureIdInList(IdRefPair.Id1, OutIdList);
          EnsureIdInList(IdRefPair.Id2, OutIdList);
        end;
    end;
  end;
end;

procedure TBoldDistributableObjectHandler.Fetch(IdList: TBoldObjectIdList;
  ValueSpace: IBoldValueSpace);
begin
  PController.PMFetch(IdList, ValueSpace, nil, fmDistributable, 0);
end;

procedure TBoldDistributableObjectHandler.MakeGlobalTranslationListFor(
  ValueSpace: IBoldValueSpace; IdList: TBoldObjectIdList;
  GlobalTranslationList: TBoldIdTranslationList);
var
  anIdList: TBoldObjectIdList;
begin
  anIdList := TBoldObjectIdList.Create;
  try
    ExtractAllIds(IdList, ValueSpace, anIdList);
    PController.PMTranslateToGlobalIds(anIdList, GlobalTranslationList);
  finally
    anIdList.Free;
  end;
end;

procedure TBoldDistributableObjectHandler.GetInfoObjectsFor(IdList: TBoldObjectIdList;
  InfoObjectList: TDistributableObjectInfoList; RemainingIdList: TBoldObjectIdList);

  function IdListToSQL(IdList: TBoldObjectIdList): string;
  var
    i: Integer;
  begin
    result := IdList[0].AsString;
    for i := 1 to IdList.Count - 1 do
      result := result + ', ' + IdList[i].AsString;
  end;

var
  aCondition: TBoldSQLCondition;
  i: Integer;
  InfoObj: TDistributableObjectInfo;
  RemainingIds: TBoldObjectIdList;
  FoundIds: TBoldObjectIdList;
  FetchedInfoObjs: TDistributableObjectInfoList;
begin
  RemainingIds := IdList.Clone;
  FetchedInfoObjs := TDistributableObjectInfoList.Create;
  try
    for i := RemainingIds.Count-1 downto 0 do
    begin
      InfoObj := LookupInfoByLocalId(RemainingIds[i] as TBoldDefaultId);
      if assigned(InfoObj) then
      begin
        InfoObjectList.Add(InfoObj);
        RemainingIds.RemoveByIndex(i);
      end;
    end;

    if (RemainingIds.Count > 0) and
       assigned(OllSystem.PersistenceController) then
    begin
      aCondition := TBoldSQLCondition.Create;
      FoundIds := TBoldObjectIdList.Create;
      try
        aCondition.TopSortedIndex := OllSystem.BoldSystemTypeInfo.ClassTypeInfoByModelName['DistributableObjectInfo'].TopSortedIndex;
        aCondition.WhereFragment :=  BOLD_OLL_IDATTRIBUTECOLUMN_NAME + ' IN (' + IdListToSQL(RemainingIds) + ')';
        OllSystem.GetAllWithCondition(FetchedInfoObjs, aCondition);
        FetchedInfoObjs.EnsureObjects;
        for i := 0 to FetchedInfoObjs.Count-1 do
          AddToMapping(FetchedInfoObjs[i]);
        GetLocalIdsFor(FetchedInfoObjs, FoundIds);
        for i := 0 to FoundIds.Count-1 do
          RemainingIds.Remove(RemainingIds.IDByID[FoundIds[i]]);
        InfoObjectList.AddList(FetchedInfoObjs);
      finally
        aCondition.Free;
        FoundIds.Free;
      end;
    end;

    RemainingIdList.AddList(RemainingIds);

  finally
    RemainingIds.Free;
    FetchedInfoObjs.Free;
  end;
end;

procedure TBoldDistributableObjectHandler.MakeLocalizingTranslationList(
  ValueSpace: IBoldValueSpace;
  GlobalIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
var
  AllIdList: TBoldObjectIdList;
begin
  AllIdList := TBoldObjectIdList.Create;
  try
    ExtractAllIds(GlobalIdList, ValueSpace, AllIdList);
    PController.PMTranslateToLocalIds(AllIdList, TranslationList);
  finally
    AllIdList.Free;
  end;
end;

procedure TBoldDistributableObjectHandler.Update(ValueSpace: IBoldValueSpace;
  IdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList;
  var TimeStamp: TBoldTimeStampType);
var
  anObject: IBoldObjectContents;
  ClientId, i, j: Integer;
  TimeOfLatestUpdate: TDateTime;
begin
  for i := IdList.Count-1 downto 0 do
  begin
    anObject := ValueSpace.ObjectContentsByObjectId[IdList[i]];
    for j := 0 to anObject.MemberCount-1 do
      anObject.ValueByIndex[j].BoldPersistenceState := bvpsModified;
    if IdList[i] is TBoldGlobalId then
    begin
      if anObject.BoldExistenceState = besDeleted then
        IdList.RemoveByIndex(i)
      else
      begin
        anObject.BoldExistenceState := besExisting;
        anObject.BoldPersistenceState := bvpsModified;
      end;
    end else if anObject.BoldExistenceState = besDeleted then
    begin
      anObject.BoldPersistenceState := bvpsModified;
    end;
  end;
  ClientId := 0;
  TimeOfLatestUpdate:= now;
  PController.PMUpdate(IdList, ValueSpace, nil, nil, TranslationList, TimeStamp, TimeOfLatestUpdate, ClientId);
end;

procedure TBoldDistributableObjectHandler.LockAndFreeObjects(IdList,
  FreeList: TBoldObjectIdList);
var
  ReadOnlyList: TBoldObjectIdList;
  i: Integer;
begin
  ReadOnlyList := IdList.Clone;
  try
    for i := 0 to FreeList.Count-1 do
      ReadOnlyList.Remove(ReadOnlyList.IDByID[FreeList[i]]);
    PController.PMSetReadonlyness(ReadOnlyList, FreeList);
  finally
    ReadOnlyList.Free;
  end;
end;

{
function TBoldDistributableObjectHandler.NewLocalClassIdFor(
  ClassId: TBoldClassIdWithExpressionName): TBoldClassId;
var
  i: Integer;
  PMapper: TBoldSystemPersistenceMapper;
begin
  PMapper := (PController as TBoldPersistenceControllerDefault).PersistenceMapper;
  i := 0;
  while (i < PMapper.ObjectPersistenceMappers.Count) and
    not (PMapper.ObjectPersistenceMappers[i].ExpressionName = ClassId.ExpressionName) do
    inc(i);
  if not (PMapper.ObjectPersistenceMappers[i].ExpressionName = ClassId.ExpressionName) then
    raise EBold.CreateFmt('%s.NewLocalClassIdFor: There is no class with expressionname "%s"', [Classname, ClassId.ExpressionName]);
  result := TBoldClassID.CreateWithInfo(True, i);
end;
}
procedure TBoldDistributableObjectHandler.VerifyAssociations(
  ValueSpace: IBoldValueSpace; IdList, HoldList: TBoldObjectIdList);
var
  i: Integer;
  anObject: IBoldObjectContents;

  procedure VerifyObjectRoles(ObjectContents: IBoldObjectContents);

  procedure CascadeToNeighbours;

  procedure VerifyAllInList(anIdList: TBoldObjectIdList);
  var
    i: Integer;
  begin
    for i := 0 to anIdList.Count-1 do
    begin
      if ValueSpace.HasContentsForId[anIdList[i]] then
        VerifyObjectRoles(ValueSpace.ObjectContentsByObjectId[anIdList[i]]);
    end;
  end;

  var
    k: Integer;
    aMember: IBoldValue;
    anIdRef: IBoldObjectIdRef;
    anIdRefPair: IBoldObjectIdRefPair;
    anIdListRef: IBoldObjectIdListRef;
    anIdListRefPair: IBoldObjectIdListRefPair;
  begin
    for k := 0 to ObjectContents.MemberCount-1 do
    begin
      aMember := ObjectContents.ValueByIndex[k];
      if assigned(aMember) then
      begin
        if (aMember.QueryInterface(IBoldObjectIdRef, anIdRef) = S_OK) then
        begin
           if ValueSpace.HasContentsForId[anIdRef.Id] then
             VerifyObjectRoles(ValueSpace.ObjectContentsByObjectId[anIdRef.Id]);
        end else if (aMember.QueryInterface(IBoldObjectIdRefPair, anIdRefPair) = S_OK) then
        begin
          if ValueSpace.HasContentsForId[anIdRefPair.Id1] then
            VerifyObjectRoles(ValueSpace.ObjectContentsByObjectId[anIdRefPair.Id1]);
          if ValueSpace.HasContentsForId[anIdRefPair.Id2] then
            VerifyObjectRoles(ValueSpace.ObjectContentsByObjectId[anIdRefPair.Id2]);
        end else if aMember.QueryInterface(IBoldObjectIdListRef, anIdListRef) = S_OK then
        begin
        end else if aMember.QueryInterface(IBoldObjectIdListRefPair, anIdListRefPair) = S_OK then
        begin

        end;
      end;
    end;
  end;

  var
    j: integer;
    aMember: IBoldValue;
    anIdRef: IBoldObjectIdRef;
    anIdRefPair: IBoldObjectIdRefPair;
  begin
    for j := 0 to ObjectContents.MemberCount-1 do
    begin
      aMember := ObjectContents.ValueByIndex[j];
      if assigned(aMember) then
      begin
        if ((aMember.QueryInterface(IBoldObjectIdRef, anIdRef) = S_OK) and
            (anIdRef.Id is TBoldGlobalId) and
            (not ValueSpace.HasContentsForId[anIdRef.Id])) or
           ((aMember.QueryInterface(IBoldObjectIdRefPair, anIdRefPair) = S_OK) and
            (anIdRefPair.Id1 is TBoldGlobalId) and
            (not ValueSpace.HasContentsForId[anIdRefPair.Id1])) then
          if assigned(BrokenLinkResolver) then
          begin
            if not BrokenLinkResolver.ResolveBrokenLink(ObjectContents, j, HoldList.IdInList[IdList[i]]) then
            begin
              raise EBoldFeatureNotImplementedYet.CreateFmt(sCannotFailIndividualObjects, [classname]);
            end;
          end else
            raise EBold.Create(sUnresolvedLink);
      end;
    end;
  end;

begin
  for i := 0 to IdList.Count-1 do
  begin
    anObject := ValueSpace.ObjectContentsByObjectId[Idlist[i]];
    if assigned(anObject) then
      VerifyObjectRoles(anObject);
  end;
end;


procedure TBoldDistributableObjectHandler.SearchByOcl(OclExpr: string;
  IdList: TBoldObjectIdList);
var
  anElement: TBoldIndirectElement;
  anObjectList: TBoldObjectList;
  i: integer;
begin
  anElement := TBoldIndirectElement.Create;
  try
    OllSystem.EvaluateExpression(OclExpr, anElement);
    anObjectList := anElement.Value as TBoldObjectList;
    for i := 0 to anObjectList.Count-1 do
      IdList.Add(anObjectList.Locators[i].BoldObjectID);
  finally
    anElement.Free;
  end;
end;

procedure TBoldDistributableObjectHandler.CommitTransaction;
begin
  if fMyTransaction then
    PController.CommitTransaction;
  fMyTransaction := false;
end;

procedure TBoldDistributableObjectHandler.RollbackTransaction;
begin
  if fMyTransaction then
    PController.RollbackTransaction;
  fMyTransaction := false;
end;

procedure TBoldDistributableObjectHandler.StartTransaction;
begin
  if not PController.InTransaction then
  begin
    PController.StartTransaction;
    fMyTransaction := true;
  end;
end;
{
function TBoldDistributableObjectHandler.GetDatabase: IBoldDatabase;
begin
  result := ((PController as TBoldPersistenceControllerDefault).PersistenceMapper as TBoldSystemDefaultMapper).Database;
end;
 }
{ TBoldBrokenLinkResolver }

function TBoldBrokenLinkResolver.ResolveBrokenLink(
  ObjectContents: IBoldObjectContents; MemberIndex: Integer;
  Hold: Boolean): Boolean;
var
  ResolveAction: TBoldLinkResolveAction;

  procedure Cut;
  var
    aMember: IBoldValue;
    anIdRef: IBoldObjectIdRef;
    anIdRefPair: IBoldObjectIdrefPair;
  begin
    aMember := ObjectContents.ValueByIndex[MemberIndex];
    if (aMember.QueryInterface(IBoldObjectIdRef, anIdRef) = S_OK) then
      anIdRef.SetFromId(nil, false)
    else if (aMember.QueryInterface(IBoldObjectIdRefPair, anIdRefPair) = S_OK) then
      anIdRefPair.SetFromIds(nil, nil)
    else
      raise EBoldInternal.CreateFmt(sMemberNotSingleLink, [Classname]);
  end;

begin
  result := True;
  if Hold then
    ResolveAction := HeldObjectAction
  else
    ResolveAction := NonheldObjectAction;

  case ResolveAction of
    blraCut: Cut;
    blraAbort: raise EBold.Create(sUnresolvedLink);
    blraFailObject: result := False;
    blraIgnore:;
    blraMissing: raise EBoldFeatureNotImplementedYet.Create(sMissingObjectsNotImplemented);
  end;
end;

procedure TBoldDistributableObjectHandler.GetLocalIdsFor(
  InfoObjects: TDistributableObjectInfoList; IdList: TBoldObjectIdList);
var
  TempId: TBoldDefaultId;
  i: Integer;
begin
  TempId := TBoldDefaultID.CreateWithClassID(0, false);
  try
    for i := 0 to InfoObjects.Count-1 do
    begin
      TempId.AsInteger := InfoObjects[i].LocalId;
      IdList.Add(TempId);
    end;
  finally
    TempId.Free;
  end;   
end;

procedure TBoldDistributableObjectHandler.NewOwnInfoObjectsFor(
  IdList: TBoldObjectIdList; InfoObjectList: TDistributableObjectInfoList);
var
  anOwnObjectInfo: TOwnObjectInfo;
  i: integer;
begin
  for i := IdList.Count-1 downto 0 do
  begin
    anOwnObjectInfo := TOwnObjectInfo.Create(OllSystem);
    anOwnObjectInfo.LocalId := (IdList[i] as TBoldDefaultId).AsInteger;
    AddToMapping(anOwnObjectInfo);
    InfoObjectList.Add(anOwnObjectInfo);
  end;
end;

procedure TBoldDistributableObjectHandler.NewForeignInfoObjectsFor(
  IdList: TBoldObjectIdList; InfoObjectList: TDistributableObjectInfoList; Owner: TForeignPSInfo; Hold: Boolean = false);
var
  i: Integer;
  aForeignObjectInfo: TForeignObjectInfo;
begin
  for i := 0 to IdList.Count - 1 do
  begin
    aForeignObjectInfo := TForeignObjectInfo.Create(OllSystem);
    if Hold then
      aForeignObjectInfo.InitializeHolding(IdList[i], Owner)
    else
    begin
      aForeignObjectInfo.LocalId := (IdList[i] as TBoldDefaultId).AsInteger;
      aForeignObjectInfo.Owner := Owner;
    end;
    AddToMapping(aForeignObjectInfo);
    InfoObjectList.Add(aForeignObjectInfo);
  end;
end;

function TBoldDistributableObjectHandler.LookupInfoByLocalId(
  LocalId: TBoldDefaultId): TDistributableObjectInfo;
begin
  result := TheMapping.ObjectInfo[LocalId.AsInteger];
end;

function TBoldDistributableObjectHandler.TheMapping: TMapping;
var
  Mappings: TBoldObjectList;
begin
  if not assigned(fTheMapping) then
  begin
    Mappings := fOllSystem.ClassByExpressionName['Mapping'];
    if Mappings.Count = 0 then
      fTheMapping := TMapping.Create(fOllSystem)
    else
    begin
      assert(Mappings.Count = 1);
      fTheMapping := Mappings[0] as TMapping;
    end;
  end;
  result := fTheMapping;
end;

procedure TBoldDistributableObjectHandler.AddToMapping(
  anObj: TDistributableObjectInfo);
begin
  TheMapping.M_ObjectInfo.Add(anObj);
end;

end.
