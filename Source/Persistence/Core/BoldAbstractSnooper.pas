unit BoldAbstractSnooper;

interface
uses
  Classes,
  BoldPersistenceControllerPassthrough,
  BoldID,
  BoldValueSpaceInterfaces,
  BoldUpdatePrecondition,
  BoldCondition,
  BoldDefs,
  BoldMeta;

type
  {forward declarations}
  TBoldAbstractSnooper = class;

  TBoldAbstractSnooper = class(TBoldPersistenceControllerPassthrough)
  private
    fMoldModel: TMoldModel;
    fEvents: TStringList;
    fSubscriptions: TStringList;
    fCancelledSubscriptions: TStringList;
    fModelSorted: Boolean;
    FOnPropagatorFailure: TBoldNotifyEventWithErrorMessage;
  protected
    procedure GenerateNonEmbeddedStateChangedEvent(OldID, NewID: TBoldObjectId; const NonEmbeddedLinkName: string);
    procedure SubscribeToNonEmbeddedStateChangedEvent(Id: TBoldObjectId; const NonEmbeddedLinkName: string);
    function ObjectIdByMemberIndex(Object_Content: IBoldObjectContents; MemberIndex: integer): TBoldObjectID;
    function MemberIsEmbeddedSingleLink(MoldMember: TMoldMember; var NonEmbeddedLinkName: string): Boolean;
    function MemberIsNonEmbeddedLink(MoldMember: TMoldMember; var MemberName: string): Boolean;
    function ClassNameFromClassID(const TopSortedIndex: integer): string;
    procedure NonEmbeddedStateOfObjectChanged(Object_Content, NewObject_Content: IBoldObjectContents; MoldClass: TMoldClass);
    procedure ClearEvents;
    procedure EnsureDataBaseLock(const ClientID: TBoldClientID); virtual;
    procedure ReleaseDataBaseLock(const ClientID: TBoldClientID); virtual;
    procedure DoPropagatorFailure(Sender: TObject; const ErrorMessage: string);
    procedure AddClassEvents(TopsortedIndex: integer);
  public
    constructor Create(MoldModel: TMoldModel);
    destructor Destroy; override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; BoldClientID: TBoldClientID); override;
    procedure TransmitEvents(const ClientID: TBoldClientID); virtual; abstract;
    property MoldModel: TMoldModel read fMoldModel;
    property Events: TStringList read fEvents;
    property Subscriptions: TStringList read fSubscriptions;
    property CancelledSubscriptions: TStringList read fCancelledSubscriptions;
    property OnPropagatorFailure: TBoldNotifyEventWithErrorMessage read FOnPropagatorFailure write FOnPropagatorFailure;
  end;

implementation

uses
  Sysutils,
  BoldValueInterfaces,
  BoldFreeStandingValues,
  BoldObjectSpaceExternalEvents;

constructor TBoldAbstractSnooper.Create(MoldModel: TMoldModel);
begin
  inherited Create;
  fModelSorted := false;
  fMoldModel := MoldModel;
  fEvents := TStringList.Create;
  fEvents.Sorted := true;
  fEvents.Duplicates := dupIgnore;
  fSubscriptions := TStringList.Create;
  fCancelledSubscriptions := TStringList.Create;
end;

destructor TBoldAbstractSnooper.Destroy;
begin
  FreeAndNil(fEvents);
  FreeAndNil(fSubscriptions);
  FreeAndNil(fCancelledSubscriptions);
  inherited;
end;

procedure TBoldAbstractSnooper.PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
            MemberIdList: TBoldMemberIdList; FetchMode: Integer;
            BoldClientID: TBoldClientID);
var
  i, j, MemberIndex: integer;
  MoldClass: TMoldClass;
  MemberName: string;
  LoadingEmbedded: Boolean;
  Object_Content: IBoldObjectContents;
begin
  inherited;
  if (BoldClientID = NOTVALIDCLIENTID) or not Assigned(MoldModel) then
    Exit;
  if not fModelSorted then
  begin
    MoldModel.EnsureTopSorted;
    fModelSorted := true;
  end;
  try
    // SubscribeToEmbeddedStateOfObject
    for i:= 0 to ObjectIdList.Count - 1 do
    begin
      LoadingEmbedded := false;
      Object_Content := ValueSpace.ObjectContentsbyObjectId[ObjectIdList[i]];
      // SubscribeToNonEmbeddedStateOfObject
      // explicit MemberIdList

      if Assigned(MemberIdList) then
      begin
        if Assigned(Object_Content) then
        begin
          MoldClass := MoldModel.Classes[ObjectIdList[i].TopSortedIndex];
          for j:= 0 to MemberIdList.Count - 1 do
          begin
            MemberIndex := MemberIdList[j].MemberIndex;
            if MemberIsNonEmbeddedLink(MoldClass.AllBoldMembers[MemberIndex], MemberName) then
              SubscribeToNonEmbeddedStateChangedEvent(ObjectIdList[i], MemberName)
            else
              LoadingEmbedded := true;
          end
        end
      end
      else
      begin
        if Assigned(Object_Content) then   // get not delayed members
        begin
          LoadingEmbedded := true;
          MoldClass := MoldModel.Classes[ObjectIdList[i].TopSortedIndex];
          for j:= 0 to Object_Content.MemberCount - 1 do
            //if member is not delayed
            if not (MoldClass.AllBoldMembers[j].EffectiveDelayedFetch) and
               MemberIsNonEmbeddedLink(MoldClass.AllBoldMembers[j], MemberName) then
                SubscribeToNonEmbeddedStateChangedEvent(ObjectIdList[i], MemberName);
        end;
      end;
      if LoadingEmbedded then
        Subscriptions.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, '', '', '', ObjectIdList[i]));
    end;
    TransmitEvents(BoldClientID);
  finally
    Object_Content:= nil;
  end;
end;

procedure TBoldAbstractSnooper.PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
            FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID);
var
  TopSortedIndex: integer;
begin
  inherited;
  if (BoldClientID = NOTVALIDCLIENTID) then
    Exit;
  // SubscribeToClassChanged
  if (Condition.ClassType = TBoldConditionWithClass) then
  begin
    TopSortedIndex := (Condition as TBoldConditionWithClass).TopSortedIndex;
    Subscriptions.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, ClassNameFromClassId(TopSortedIndex),
             '', '', nil));
  end;
  TransmitEvents(BoldClientID);
end;

procedure TBoldAbstractSnooper.PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
            Old_Values: IBoldValueSpace;
            Precondition: TBoldUpdatePrecondition;
            TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType;
            BoldClientID: TBoldClientID);
var
  i, j: integer;
  LocalOld_Values: TBoldFreeStandingValueSpace;
  LocalObjectIdList: TBoldObjectIdList;
  LocalTranslationList: TBoldIdTranslationList;
  Object_Content, NewObject_Content: IBoldObjectContents;
  ObjectId: TBoldObjectID;
  MoldClass: TMoldClass;
  MemberName: string;
begin
  //make a copy of objectIDList
  assert(assigned(MoldModel), 'Snooper has no Model');
  LocalObjectIdList := ObjectIdList.Clone;
  try
    //get old values
    LocalOld_Values := nil;
    LocalTranslationList := nil;
    if not Assigned(Old_Values) then
    begin
      LocalOld_Values := TBoldFreeStandingValueSpace.Create;
      LocalOld_Values.GetInterface(IBoldValueSpace, Old_Values);
    end;
    if not assigned(TranslationList) then
    begin
      LocalTranslationList := TBoldIdTranslationList.Create;
      TranslationList := LocalTranslationList;
    end;

    ReserveNewIds(ValueSpace, LocalObjectIdList, TranslationList);
    ValueSpace.ApplytranslationList(TranslationList);
    LocalObjectIdList.ApplyTranslationList(TranslationList);

    if (BoldClientID <> NOTVALIDCLIENTID) then
      for i := 0 to LocalObjectIDList.Count - 1 do
      begin
        Object_Content := ValueSpace.ObjectContentsByObjectId[LocalObjectIdList[i]];
        Assert(Assigned(Object_Content), 'Object does not exist');
        if Object_Content.BoldPersistenceState = bvpsModified then
          case Object_Content.BoldExistenceState of
            besExisting:
              begin
                AddClassEvents(LocalObjectIdList[i].TopSortedIndex);
                Subscriptions.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, '', '', '', TranslationList.TranslateToNewId[LocalObjectIdList[i]]));
              end;
            besDeleted:
              begin
                AddClassEvents(LocalObjectIdList[i].TopSortedIndex);
                Events.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsObjectDeleted, '', '', '', LocalObjectIdList[i])) ;
                CancelledSubscriptions.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, '', '', '', LocalObjectIdList[i])) ;
                //Cancel subscriptions to NonEmbeddedStateOfObjectChanged events
                MoldClass := MoldModel.Classes[ObjectIDList[i].TopSortedIndex];
                for j:= 0 to MoldClass.AllBoldMembers.Count - 1 do
                begin
                  MemberName := MoldClass.AllBoldMembers.Items[j].ExpandedExpressionName;
                  if MemberIsNonEmbeddedLink(MoldClass.AllBoldMembers[j], MemberName) then
                    CancelledSubscriptions.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, '', MemberName, '', LocalObjectIdList[i]));
                end;
              end;
          end //case
        else
          Events.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, '', '', '', LocalObjectIdList[i]));
      end; //for

    EnsureDataBaseLock(BoldClientID);
    try
      inherited PMUpdate(LocalObjectIdList, ValueSpace, Old_Values, Precondition, nil, TimeStamp, BoldClientID);
    finally
      ReleaseDataBaseLock(BoldClientID);
    end;
    if (not assigned(Precondition) or (not Precondition.failed)) and
       (BoldClientID <> NOTVALIDCLIENTID) then

    begin
      for i:= LocalObjectIdList.Count - 1 downto 0 do
      begin
        Object_Content := Old_Values.ObjectContentsbyObjectId[LocalObjectIdList[i]];
        ObjectId := LocalObjectIdList[i];
        if Assigned(ObjectId) then
          NewObject_Content := ValueSpace.ObjectContentsByObjectId[ObjectId]
        else
          NewObject_Content := nil;
        NonEmbeddedStateOfObjectChanged(Object_Content, NewObject_Content,
                  MoldModel.Classes[LocalObjectIdList[i].TopSortedIndex]);
      end;
      TransmitEvents(BoldClientID);
    end;
  finally
    // release the interfaces
    Object_Content := nil;
    NewObject_Content := nil;
    if assigned(LocalOld_Values) then
    begin
      // must release the interface before removing the underlying object
      Old_Values := nil;
      FreeAndNil(LocalOld_Values);
    end;
    FreeAndNil(LocalTranslationList);
    LocalObjectIdList.Free;
  end;
end;

procedure TBoldAbstractSnooper.GenerateNonEmbeddedStateChangedEvent(OldID, NewID: TBoldObjectId; const NonEmbeddedLinkName: string);
begin
  if (Assigned(OldID) and Assigned(NewID) and not(OldID.IsEqual[NewID])) then
  begin
    Events.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, '', NonEmbeddedLinkName, '', OldID));
    Events.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, '', NonEmbeddedLinkName, '', NewID));
  end
  else if (Assigned(OldID) and not Assigned(NewID)) then
    Events.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, '', NonEmbeddedLinkName, '', OldID))
  else if (Assigned(NewID) and not Assigned(OldID)) then
    Events.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, '', NonEmbeddedLinkName, '', NewID));
end;

procedure TBoldAbstractSnooper.SubscribeToNonEmbeddedStateChangedEvent(Id: TBoldObjectId; const NonEmbeddedLinkName: string);
begin
 if Assigned(Id) then
    Subscriptions.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, '', NonEmbeddedLinkName, '', Id))
end;

function TBoldAbstractSnooper.ObjectIdByMemberIndex(Object_Content: IBoldObjectContents; MemberIndex: integer): TBoldObjectID;
var
  Value: IBoldValue;
  IDRef: IBoldObjectIDRef;
begin
  Result := nil;
  if Assigned(Object_Content) then
  begin
    Value := Object_Content.ValueByIndex[MemberIndex];
    if Assigned(Value) and (Value.QueryInterface(IBoldObjectIDRef, IDRef) = S_OK) then
        Result := IDRef.Id;
  end;
end;

function TBoldAbstractSnooper.MemberIsEmbeddedSingleLink(MoldMember: TMoldMember; var NonEmbeddedLinkName: string): Boolean;
var
  MoldRole: TMoldRole;
  NonEmbeddedLink: TMoldRole;
begin
  Result := false;
  if (MoldMember is TMoldRole) then
  begin
    MoldRole := MoldMember as TMoldRole;
    Result := MoldRole.EffectiveEmbedded;
    if Result then
    begin
      NonEmbeddedLink := MoldRole.OtherEnd ;
      if (NonEmbeddedLink.RoleType = rtLinkRole) then
        NonEmbeddedLink :=  NonEmbeddedLink.MainRole;
      NonEmbeddedLinkName := NonEmbeddedLink.ExpandedExpressionName;
    end;
  end;
end;

function TBoldAbstractSnooper.MemberIsNonEmbeddedLink(MoldMember: TMoldMember; var MemberName: string): Boolean;
var
  MoldRole: TMoldRole;
begin
  Result := false;
  if (MoldMember is TMoldRole) then
  begin
    MoldRole := (MoldMember as TMoldRole);
    Result := not (MoldRole.EffectiveEmbedded) and (MoldRole.RoleType = rtRole);
    if Result then
      MemberName := MoldRole.ExpandedExpressionName;
  end;
end;

procedure TBoldAbstractSnooper.NonEmbeddedStateOfObjectChanged(Object_Content, NewObject_Content: IBoldObjectContents; MoldClass: TMoldClass);
var
  j: integer;
  MemberCount: integer;
  Id: TBoldObjectID;
  NewId: TBoldObjectID;
  NonEmbeddedLinkName: string;
begin
  MemberCount := 0;
  if Assigned(Object_Content) then
    MemberCount := Object_Content.MemberCount
  else if Assigned(NewObject_Content) then
    MemberCount := NewObject_Content.MemberCount;
  for j:= 0 to MemberCount - 1 do
    if MemberIsEmbeddedSingleLink(MoldClass.AllBoldMembers[j], NonEmbeddedLinkName) then
    begin
      Id := ObjectIdByMemberIndex(Object_Content, j);
      NewId := ObjectIdByMemberIndex(NewObject_Content, j);
      GenerateNonEmbeddedStateChangedEvent(Id, NewId,NonEmbeddedLinkName);
    end;
end;

function TBoldAbstractSnooper.ClassNameFromClassId(const TopSortedIndex: integer): string;
begin
  Result := MoldModel.Classes[TopSortedIndex].ExpandedExpressionName;
end;

procedure TBoldAbstractSnooper.ClearEvents;
begin
  Events.Clear;
  Subscriptions.Clear;
  CancelledSubscriptions.Clear;
end;

procedure TBoldAbstractSnooper.DoPropagatorFailure(Sender: TObject; const ErrorMessage: string);
begin
  if Assigned(FOnPropagatorFailure) then
    FOnPropagatorFailure(Sender, ErrorMessage);
end;

procedure TBoldAbstractSnooper.AddClassEvents(TopsortedIndex: integer);
var
  MoldClass: TMoldClass;
begin
  MoldClass := MoldModel.Classes[TopSortedIndex];
  while assigned(MoldClass) do
  begin
     Events.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, MoldClass.ExpandedExpressionName, '', '', nil));
     MoldClass := MoldClass.SuperClass;
  end;
end;

procedure TBoldAbstractSnooper.EnsureDataBaseLock(const ClientID: TBoldClientID);
begin
  // intentionally left blank
end;

procedure TBoldAbstractSnooper.ReleaseDataBaseLock(const ClientID: TBoldClientID);
begin
  // intentionally left blank
end;

end.
