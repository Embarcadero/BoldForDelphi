
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldExternalObjectSpaceEventHandler;

interface

uses
  Classes,
  SysUtils,
  BoldSystemHandle,
  BoldSystem,
  BoldSubscription,
  BoldAbstractDequeuer,
  BoldDefaultID,
  BoldAbstractPropagatorHandle,
  BoldElementList,
  BoldDomainElement,
  BoldId,
  BoldDefs;

type
  TBoldClassChangedEvent = procedure (TheClass: TBoldObjectList) of object;
  TBoldEmbeddedStateChangedEvent = procedure (BoldObject: TBoldObject) of object;
  TBoldNonEmbeddedStateChangedEvent = procedure (BoldMember: TBoldMember) of object;
  TBoldConflictEvent = procedure (ABoldElement: TBoldDomainElement) of object;
  TBoldLockLostEvent = procedure (LockName: String) of object;
  TBoldDoDisconnectEvent = procedure(aMessage: String; RemainDisconnectedMSec: integer) of object;
  TBoldExternalObjectSpaceEventHandler = class;
  TBoldFailedOSSMessageEvent = procedure(AMessage: String; AException: Exception) of object;
  TBoldInvalidOSSMessageEvent = procedure(const aMsg: String) of object;
  TBoldOSSGetClientsEvent = procedure(const aMsg: String) of object;

  EOSS = class(EBold)
  end;

  EOSSConflict = class(EOSS)
  private
    FList: TBoldList;
  public
    constructor Create(AList: TBoldList);
    destructor Destroy; override;
    property List: TBoldList read FList;
  end;

  TIdListArray = array of TBoldObjectIdList;

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldExternalObjectSpaceEventHandler = class(TBoldStringDequeuer)
  private
    fBoldSystemHandle: TBoldSystemHandle;
    fPropagatorHandle: TBoldAbstractPropagatorHandle;
    fPTSubscriber: TBoldPassthroughSubscriber;
    fConflictingElements: TBoldElementList;
    {*** User events ***}
    fOnConflict: TBoldConflictEvent;
    fOnLockLost: TBoldLockLostEvent;
    fDoDisconnect: TBoldDoDisconnectEvent;
    {*** End user events ***}
    fOnClassChangedEvent: TBoldClassChangedEvent;
    fOnEmbeddedStateChanged: TBoldEmbeddedStateChangedEvent;
    fOnNonEmbeddedStateChanged: TBoldNonEmbeddedStateChangedEvent;
    fOnObjectDeleted: TBoldEmbeddedStateChangedEvent;
    fOnInvalidOSSMessageEvent: TBoldInvalidOSSMessageEvent;
    fBoldOSSGetClientsEvent: TBoldOSSGetClientsEvent;
    fKeepClassesCurrent: boolean;
    fObjectFetchArray: TIdListArray;
    fIdFetchArray: TIdListArray;
    fUseMemberLevelOSS: boolean;
    FOnFailedMessage: TBoldFailedOSSMessageEvent;
    procedure SetPropagatorHandle(Value: TBoldAbstractPropagatorHandle);
    procedure SetBoldSystemHandle(aSystemHandle: TBoldSystemHandle);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetObjectByID(ObjectID: TBoldDefaultID): TBoldObject;
    procedure Subscribe(const DoSubscribe: Boolean);
  protected
    procedure DoInvalidOSSMessage(const aMsg: String);
    procedure HandleMessage(const aMsg: String); override;
//    procedure HandleObjectMessage(const AOSSMessage: TBoldOSSMessage);
    procedure ClassChanged(const AClassName: String); virtual;
    procedure MemberChanged(const AClassName, AMemberName: String; AObjectID: TBoldDefaultID); virtual;
    procedure EmbeddedStateOfObjectChanged(const AClassName: String; AObjectID: TBoldDefaultID); virtual;
    procedure NonEmbeddedStateOfObjectChanged(const AClassName: String; const AMemberName: String; AObjectID: TBoldDefaultID); virtual;
    procedure ObjectCreated(const AClassName: String; AObjectId: TBoldDefaultID); virtual;
    procedure ObjectDeleted(const AClassName: String; AObjectId: TBoldDefaultID); virtual;
    procedure LockLost(const LockName: String); virtual;
    procedure Conflict(AElement: TBoldDomainElement); virtual;
    procedure ClearFetchList;
    procedure FetchObject(ID: TBoldObjectId);
    procedure FetchMember(Member: TBoldMember);
    procedure FetchId(ID: TBoldObjectId);
    procedure FetchLists;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetBoldSystem: TBoldSystem;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property ConflictingElements: TBoldElementList read fConflictingElements;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DequeueAll; override;
  published
    property BoldSystemHandle: TBoldSystemHandle read fBoldSystemHandle write SetBoldSystemHandle;
    property OnClassChanged: TBoldClassChangedEvent read fOnClassChangedEvent write fOnClassChangedEvent;
    property OnEmbeddedStateChanged: TBoldEmbeddedStateChangedEvent read fOnEmbeddedStateChanged write fOnEmbeddedStateChanged;
    property OnObjectDeleted: TBoldEmbeddedStateChangedEvent read fOnObjectDeleted write fOnObjectDeleted;
    property OnNonEmbeddedStateChanged: TBoldNonEmbeddedStateChangedEvent read fOnNonEmbeddedStateChanged write fOnNonEmbeddedStateChanged;
    property OnLockLost: TBoldLockLostEvent read fOnLockLost write fOnLockLost;
    property OnConflict: TBoldConflictEvent read fOnConflict write fOnConflict;
    property OnDoDisconnect: TBoldDoDisconnectEvent read fDoDisconnect write fDoDisconnect;
    property PropagatorHandle: TBoldAbstractPropagatorHandle read fPropagatorHandle write SetPropagatorHandle;
    property KeepClassesCurrent: boolean read fKeepClassesCurrent write fKeepClassesCurrent default true;
    property UseMemberLevelOSS: boolean read fUseMemberLevelOSS write fUseMemberLevelOSS;
    property OnFailedMessage: TBoldFailedOSSMessageEvent read FOnFailedMessage write FOnFailedMessage;
    property OnInvalidOSSMessage: TBoldInvalidOSSMessageEvent read fOnInvalidOSSMessageEvent write fOnInvalidOSSMessageEvent;
    property OnGetClientsEvent: TBoldOSSGetClientsEvent read fBoldOSSGetClientsEvent write fBoldOSSGetClientsEvent;
  end;

implementation

uses
  BoldObjectSpaceExternalEvents,
  BoldValueSpaceInterfaces,
  BoldSystemRT,
  Windows,
  BoldElements,
  BoldMetaElementList,
  BoldIndex,
  BoldIndexableList,
  BoldHashIndexes;

{ TBoldDequeuer }

constructor TBoldExternalObjectSpaceEventHandler.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fPTSubscriber := TBoldPassthroughSubscriber.Create(_Receive);
  fConflictingElements := TBoldElementList.Create;
  fKeepClassesCurrent := true;
end;

procedure TBoldExternalObjectSpaceEventHandler.DequeueAll;
begin
  ClearFetchList;
  try
    inherited DequeueAll;
    FetchLists;
  finally
    ClearFetchList;
  end;
end;

destructor TBoldExternalObjectSpaceEventHandler.Destroy;
begin
  FreeAndNil(fPTSubscriber);
  FreeAndNil(fConflictingElements);
  inherited;
end;

procedure TBoldExternalObjectSpaceEventHandler.SetBoldSystemHandle(aSystemHandle: TBoldSystemHandle);
begin
  if aSystemHandle <> fBoldSystemHandle then
  begin
    Subscribe(False);
    fBoldSystemHandle := aSystemHandle;
    Subscribe(True);
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = fBoldSystemHandle) and (RequestedEvent = beDestroying) then
    BoldSystemHandle := nil;
end;

procedure TBoldExternalObjectSpaceEventHandler.HandleMessage(const aMsg: String);
var
  vClassName, vMemberName, vLockName: String;
  vSubsType: TBoldObjectSpaceSubscriptionType;
  vObjectID, vExactId: TBoldDefaultID;
  vEvent: string;
  vEvents: TStringList;
  i: integer;
begin
  if not assigned(fBoldSystemHandle) then
    raise EBold.CreateFmt('%s.HandleMessage: The Eventhandler (%s) is not connected to a systemhandle. Unable to handle messages', [self.ClassName, name]);
  if not assigned(fBoldSystemHandle.System) then
    raise EBold.CreateFmt('%s.HandleMessage: The systemhandle (%s) is not active. Unable to handle messages', [self.ClassName, fBoldSystemHandle.name]);

  vEvents := TStringList.Create;
  vEvents.CommaText := aMsg;
  vExactId := nil;
  vObjectID := TBoldDefaultID.Create;
  try
    for I := 0 to vEvents.Count - 1 do
    begin
      vEvent := Trim(vEvents[i]);
      begin
        try
          vSubsType := TBoldObjectSpaceExternalEvent.DecodeExternalEvent(vEvent,
                                                                        vClassName,
                                                                        vMemberName,
                                                                        vLockName,
                                                                        vObjectID);
          try
            if (vClassName <> '') then
              vExactId := vObjectID.CloneWithClassId(BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[vClassName].TopSortedIndex, true) as TBoldDefaultID
            else
              vExactId := vObjectID.Clone as TBoldDefaultId;
            case vSubsType of
              bsClassChanged: ClassChanged(vClassName);
              bsMemberChanged: MemberChanged(vClassName, vMemberName, vExactId);
              bsEmbeddedStateOfObjectChanged: EmbeddedStateOfObjectChanged(vClassName, vExactId);
              bsObjectCreated: ObjectCreated(vClassName, vExactId);
              bsObjectDeleted: ObjectDeleted(vClassName, vExactId);
              bsNonEmbeddedStateOfObjectChanged: NonEmbeddedStateOfObjectChanged(vClassName, vMemberName, vExactId);
              bsLockLost: LockLost(vLockName);
            end;
          finally
            FreeAndNil(vExactId);
          end;
        except
          on e: exception do
            FOnFailedMessage(vEvent, e);
        end;
      end;
    end;
  finally
    vEvents.free;
    FreeAndNil(vObjectID);
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler.DoInvalidOSSMessage(
  const aMsg: String);
begin
  if Assigned(OnInvalidOSSMessage) then
    OnInvalidOSSMessage(aMsg);
end;

procedure TBoldExternalObjectSpaceEventHandler.ClassChanged(const AClassName: String);
var
  ClassTypeInfo: TBoldClassTypeInfo;
  ClassList: TBoldObjectList;
begin
  ClassTypeInfo := BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[AClassName];
  if not Assigned(ClassTypeInfo) then
    raise EOSS.CreateFmt('Cannot find the class %s in the system.', [AClassName]);
  if Assigned(fOnClassChangedEvent) then
  begin
    ClassList := BoldSystem.Classes[ClassTypeInfo.TopSortedIndex];
    fOnClassChangedEvent(ClassList);
  end
  else
  repeat
    ClassList := BoldSystem.Classes[ClassTypeInfo.TopSortedIndex];
    ClassList.Invalidate;
    ClassTypeInfo := ClassTypeInfo.SuperClassTypeInfo;
  until not Assigned(ClassTypeInfo);
  SendExtendedEvent(self, boeClassChanged, [AClassName]);
end;

procedure TBoldExternalObjectSpaceEventHandler.EmbeddedStateOfObjectChanged(
  const AClassName: String; AObjectID: TBoldDefaultID);
var
  CurrObj: TBoldObject;
begin
  CurrObj := GetObjectByID(AObjectID);
  if Assigned(fOnEmbeddedStateChanged) then
    fOnEmbeddedStateChanged(CurrObj)
  else
  if Assigned(CurrObj) then
  begin
    if not UseMemberLevelOSS then
    begin
      if (CurrObj.BoldDirty) then
        Conflict(CurrObj)
      else
      begin
        if CurrObj.ObjectHasSubscribers then
          FetchObject(AObjectId);
        CurrObj.Invalidate;
      end;
    end;
  end;
  SendExtendedEvent(self, boeEmbeddedStateOfObjectChanged, [CurrObj, AClassName, AObjectId, CurrObj]);
end;

procedure TBoldExternalObjectSpaceEventHandler.ClearFetchList;
var
  i: integer;
begin
  for I := 0 to high(fObjectFetchArray) do
  begin
    fObjectFetchArray[i].Free;
    fObjectFetchArray[i] := nil;
  end;
  for I := 0 to high(fIdFetchArray) do
  begin
    fIdFetchArray[i].Free;
    fIdFetchArray[i] := nil;
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler.FetchId(ID: TBoldObjectId);
var
  IdList: TBoldObjectIdList;
begin
  Assert(Id.TopSortedIndexExact);
  if (Length(fObjectFetchArray) > Id.TopSortedIndex) and Assigned(fObjectFetchArray[Id.TopSortedIndex]) then
  begin
    if fObjectFetchArray[Id.TopSortedIndex].IdInList[Id] then
      exit;
  end;
  if Id.TopSortedIndex >= Length(fIdFetchArray) then
    SetLength(fIdFetchArray, Id.TopSortedIndex+1);
  IdList := fIdFetchArray[Id.TopSortedIndex];
  if not Assigned(IdList) then
  begin
    IdList := TBoldObjectIdList.Create;
    fIdFetchArray[Id.TopSortedIndex] := IdList;
  end;
  IdList.AddIfNotInList(Id);
end;

procedure TBoldExternalObjectSpaceEventHandler.FetchObject(ID: TBoldObjectId);
var
  IdList: TBoldObjectIdList;
begin
  Assert(Id.TopSortedIndexExact);
  // first remove from IDFetchList if found
  if (Length(fIdFetchArray) > Id.TopSortedIndex) and Assigned(fIdFetchArray[Id.TopSortedIndex]) then
  begin
    if fIdFetchArray[Id.TopSortedIndex].IdInList[Id] then
    begin
      fIdFetchArray[Id.TopSortedIndex].Remove(Id);
    end;
  end;
  if Id.TopSortedIndex >= Length(fObjectFetchArray) then
    SetLength(fObjectFetchArray, Id.TopSortedIndex+1);
  IdList := fObjectFetchArray[Id.TopSortedIndex];
  if not Assigned(IdList) then
  begin
    IdList := TBoldObjectIdList.Create;
    fObjectFetchArray[Id.TopSortedIndex] := IdList;
  end;
  IdList.AddIfNotInList(Id);
end;

procedure TBoldExternalObjectSpaceEventHandler.FetchLists;
var
  i: integer;
  IDList: TBoldObjectIdList;
  vBoldSystem: TBoldSystem;
begin
  vBoldSystem := BoldSystem;
  for I := 0 to high(fIdFetchArray) do
  begin
    if Assigned(fIdFetchArray[i]) then
    begin
      IdList := fIdFetchArray[i];
      vBoldSystem.FetchIdList(IdList, false);
    end;
  end;
  for I := 0 to high(fObjectFetchArray) do
  begin
    if Assigned(fObjectFetchArray[i]) then
    begin
      IdList := fObjectFetchArray[i];
      vBoldSystem.FetchIdList(IdList, true);
    end;
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler.FetchMember(Member: TBoldMember);
begin
//  fMemberFetchList.Add(Member);
end;

procedure TBoldExternalObjectSpaceEventHandler.NonEmbeddedStateOfObjectChanged(
  const AClassName: String; const AMemberName: String; AObjectID: TBoldDefaultID);
var
  CurrObj: TBoldObject;
  CurrMember: TBoldMember;
  i: integer;
begin
  CurrMember := nil;
  CurrObj := GetObjectByID(AObjectID);
  if Assigned(CurrObj) then
  begin
//    if CurrObj.BoldClassTypeInfo.ExpressionName <> AClassName then
//      raise EOSS.CreateFmt('Object %s is %s, but expected type is %s.', [AObjectID.AsString, CurrObj.BoldClassTypeInfo.ExpressionName, AClassName]);
    i := CurrObj.BoldMemberIndexByExpressionName[AMemberName];
    if i = -1 then
      raise EOSS.CreateFmt('Class %s does not have a member "%s", check OSS settings of other clients.', [CurrObj.DisplayName, AMemberName]);
    CurrMember := CurrObj.BoldMemberIfAssigned[i];
  end;
  if Assigned(CurrMember) then
  begin
    if Assigned(fOnNonEmbeddedStateChanged) then
      fOnNonEmbeddedStateChanged(CurrMember)
    else
    if CurrMember.BoldDirty then
      Conflict(CurrMember)
    else
    begin
//      if CurrMember.MemberHasSubscribers then
//        FetchMember(CurrMember);
      CurrMember.Invalidate;
    end;
  end;
  SendExtendedEvent(self, boeNonEmbeddedStateOfObjectChanged, [AObjectID, CurrObj, CurrMember, AClassName, AMemberName]);
end;

procedure TBoldExternalObjectSpaceEventHandler.Conflict(
  AElement: TBoldDomainElement);
begin
  fConflictingElements.Add(AElement);
  if Assigned(fOnConflict) then
    fOnConflict(AElement);
end;

function TBoldExternalObjectSpaceEventHandler.GetBoldSystem: TBoldSystem;
begin
  result := nil;
  if Assigned(fBoldSystemHandle) then
    result := fBoldSystemHandle.System;
end;

function TBoldExternalObjectSpaceEventHandler.GetObjectByID(ObjectID: TBoldDefaultID): TBoldObject;
var
  CurrLocator: TBoldObjectLocator;
begin
  Result := nil;
  if Assigned(BoldSystem) then
  begin
    CurrLocator := fBoldSystemHandle.System.Locators.LocatorByID[ObjectID];
    if Assigned(CurrLocator) then
      Result := CurrLocator.BoldObject;
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler.LockLost(const LockName: String);
begin
  if Assigned(fOnLockLost) then
    fOnLockLost(LockName);
end;

procedure TBoldExternalObjectSpaceEventHandler.MemberChanged(
  const AClassName, AMemberName: String; AObjectID: TBoldDefaultID);
var
  CurrObj: TBoldObject;
  CurrMember: TBoldMember;
  //SizeOfInvalidatedList : Integer;
//  SizeOfInvalidatedListCategory : String;
  sl: TStringList;
  i,j: integer;
  vHasConflicts: boolean;
begin
//  CurrMember := nil;
  CurrObj := GetObjectByID(AObjectID);
  if not Assigned(CurrObj) then
    exit;
  if CurrObj.BoldClassTypeInfo.ExpressionName <> AClassName then
    raise EOSS.CreateFmt('Object %s is %s, but expected type is %s.', [AObjectID.AsString, CurrObj.BoldClassTypeInfo.ExpressionName, AClassName]);
  if Assigned(CurrObj) and UseMemberLevelOSS then
  begin
//    if CurrObj.BoldDirty then
//      Conflict(CurrObj)
//    else
//    begin
//      if CurrObj.ObjectHasSubscribers then
//        FetchObject(ObjectId);
//      CurrObj.Invalidate;

      sl := TStringList.Create;
      try
        sl.CommaText := AMemberName;
        vHasConflicts := false;
        for i := 0 to sl.Count - 1 do
        begin
          j := CurrObj.BoldMemberIndexByExpressionName[sl[i]];
          if j = -1 then
            raise EOSS.CreateFmt('Class %s does not have a member "%s", check OSS settings of other clients.', [CurrObj.DisplayName, AMemberName]);
          CurrMember := CurrObj.BoldMemberIfAssigned[j];
          if Assigned(CurrMember) then
          begin
            sl.Objects[i] := CurrMember;
            if CurrMember.BoldDirty then
            begin
              vHasConflicts := true;
              Conflict(CurrMember);
            end
            else
              CurrMember.Invalidate;
          end;
        end;
        if vHasConflicts then
        begin
          CurrObj.Discard;
          exit;
        end;
{        for I := sl.Count - 1 downto 0 do
        begin
          CurrMember := TBoldMember(sl.Objects[i]);
          if Assigned(CurrMember) then
          begin
            if (CurrMember.BoldPersistenceState in [bvpsModified]) then
            begin
              vHasConflicts := true;
              Conflict(CurrMember); // this is a conflict caused by local objectspace reacting to invalidate, treat it differently ?
            end
            else
            if (CurrMember.BoldPersistenceState in [bvpsCurrent]) then
            begin
              CurrMember.Invalidate;
              continue; // skip the sl.Delete(i) in order to keep invalidated members in list and refetch them
            end;
            sl.Delete(i);
          end;
        end;
}
//        if not vHasConflicts then
//          FetchObject(ObjectId);
//          BoldSystem.FetchMembersWithObject(CurrObj, sl.CommaText);
      finally
        sl.free;
      end;

//    end;
  end;
  SendExtendedEvent(self, boeMemberChanged, [AClassName, AMemberName, AObjectId, CurrObj]);
end;

procedure TBoldExternalObjectSpaceEventHandler.ObjectCreated(const AClassName: String;
  AObjectId: TBoldDefaultID);
var
  ClassTypeInfo: TBoldClassTypeInfo;
  ClassList: TBoldObjectList;
  BoldObject: TBoldObject;
  Handled: boolean;
begin
  ClassTypeInfo := BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[AClassName];
  if not Assigned(ClassTypeInfo) then
    raise EOSS.CreateFmt('Cannot find the class %s in the system.', [AClassName]);
  BoldObject := nil;
  Handled := false;
  repeat
    ClassList := BoldSystem.Classes[ClassTypeInfo.TopSortedIndex];
    if KeepClassesCurrent and (ClassList.BoldPersistenceState <> bvpsInvalid) then
    begin
      if not Handled then
      begin
        case ClassList.BoldPersistenceState of
          bvpsCurrent: FetchObject(AObjectId);
          bvpsTransient: FetchId(AObjectId);
        end;
        Handled := true;
      end;
    end
    else
      ClassList.Invalidate;
    ClassTypeInfo := ClassTypeInfo.SuperClassTypeInfo;
  until not Assigned(ClassTypeInfo);
  SendExtendedEvent(self, boeObjectCreated, [AClassName, AObjectId, BoldObject]);
end;

procedure TBoldExternalObjectSpaceEventHandler.ObjectDeleted(const AClassName: String; AObjectId: TBoldDefaultID);
var
  ClassTypeInfo: TBoldClassTypeInfo;
//  ClassList: TBoldObjectList;
  CurrObj: TBoldObject;
begin
  ClassTypeInfo := BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[AClassName];
  if not Assigned(ClassTypeInfo) then
    raise EOSS.CreateFmt('Cannot find the class %s in the system.', [AClassName]);
  CurrObj := GetObjectByID(AObjectID);
  if Assigned(CurrObj) then
  begin
    if Assigned(fOnObjectDeleted) then
      fOnObjectDeleted(CurrObj)
    else
    if (CurrObj.BoldDirty) then
      Conflict(CurrObj)
    else
    begin
      CurrObj.AsIBoldObjectContents[bdepPMIn].BoldExistenceState := besDeleted;
      if CurrObj.BoldPersistenceState = bvpsInvalid then
        CurrObj.SendEvent(beObjectDeleted);
    end;
  end;
  SendExtendedEvent(self, boeObjectDeleted, [AClassName, AObjectId, CurrObj]);
end;

procedure TBoldExternalObjectSpaceEventHandler.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = fBoldSystemHandle) then
      fBoldSystemHandle := nil
    else if (AComponent = fPropagatorHandle) then
      fPropagatorHandle := nil;
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler.SetPropagatorHandle(
  Value: TBoldAbstractPropagatorHandle);
begin
  if (fPropagatorHandle <> Value) then
  begin
    Subscribe(False);
    fPropagatorHandle := Value;
    Subscribe(True);
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler.Subscribe(
  const DoSubscribe: Boolean);
begin
  if DoSubscribe then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Assigned(fPropagatorHandle) then
        fPropagatorHandle.AddSmallSubscription(fPTSubscriber, [beDestroying], beDestroying);
      if Assigned(fBoldSystemHandle) then
        fBoldSystemHandle.AddSmallSubscription(fPTSubscriber, [beDestroying], beDestroying);
    end;
  end
  else
    fPTSubscriber.CancelAllSubscriptions;
end;

{ EOSSConflict }

constructor EOSSConflict.Create(AList: TBoldList);
begin
  FList := AList.Clone as TBoldList;
  self.message := AList.AsDebugCommaText();
end;

destructor EOSSConflict.Destroy;
begin
  FList.free;
  inherited;
end;

end.
