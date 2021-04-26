
{ Global compiler directives }
{$include bold.inc}
unit BoldExternalObjectSpaceEventHandler;

interface

uses
  Classes,
  BoldSystemHandle,
  BoldSystem,
  BoldSubscription,
  BoldAbstractDequeuer,
  BoldDefaultID,
  BoldAbstractPropagatorHandle,
  BoldElementList,
  BoldDomainElement,
  BoldId,
{$IFDEF UseBoldOSSMessage}
  BoldOSSMessage,
{$ENDIF}
  BoldDefs;

type
  TBoldClassChangedEvent = procedure (TheClass: TBoldObjectList) of object;
  TBoldEmbeddedStateChangedEvent = procedure (BoldObject: TBoldObject) of object;
  TBoldNonEmbeddedStateChangedEvent = procedure (BoldMember: TBoldMember) of object;
  TBoldConflictEvent = procedure (ABoldElement: TBoldDomainElement) of object;
  TBoldLockLostEvent = procedure (LockName: String) of object;
  TBoldDoDisconnectEvent = procedure(aMessage: String; RemainDisconnectedMSec: integer) of object;
  TBoldExternalObjectSpaceEventHandler = class;

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
    fKeepClassesCurrent: boolean;
    fObjectFetchArray: TIdListArray;
    fIdFetchArray: TIdListArray;
    fUseMemberLevelOSS: boolean;
    procedure SetPropagatorHandle(Value: TBoldAbstractPropagatorHandle);
    procedure SetBoldSystemHandle(aSystemHandle: TBoldSystemHandle);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetObjectByID(ObjectID: TBoldDefaultID): TBoldObject;
    procedure Subscribe(const DoSubscribe: Boolean);
  protected
    procedure HandleMessage(const aMsg: String); override;
{$IFDEF UseBoldOSSMessage}
    procedure HandleObjectMessage(const AOSSMessage: TOSSMessage);
{$ENDIF}
    procedure ClassChanged(const ClassName: String); virtual;
    procedure MemberChanged(const ClassName, MemberName: String; ObjectID: TBoldDefaultID); virtual;
    procedure EmbeddedStateOfObjectChanged(const ClassName: String; ObjectID: TBoldDefaultID); virtual;
    procedure NonEmbeddedStateOfObjectChanged(const ClassName: String; const MemberName: String; ObjectID: TBoldDefaultID); virtual;
    procedure ObjectCreated(const ClassName: String; ObjectId: TBoldDefaultID); virtual;
    procedure ObjectDeleted(const ClassName: String;ObjectId: TBoldDefaultID); virtual;
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
  end;

implementation

uses
  SysUtils,
  BoldObjectSpaceExternalEvents,
  BoldValueSpaceInterfaces,
  BoldSystemRT;

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
  ClassName, MemberName, LockName: String;
  SubsType: TBoldObjectSpaceSubscriptionType;
  ObjectID, ExactId: TBoldDefaultID;
  temp: string;
  vEvents: TStringList;
  vEvent: string;
  i: integer;
begin
  if not assigned(fBoldSystemHandle) then
    raise EBold.CreateFmt('%s.HandleMessage: The Eventhandler (%s) is not connected to a systemhandle. Unable to handle messages', [self.ClassName, name]);
  if not assigned(fBoldSystemHandle.System) then
    raise EBold.CreateFmt('%s.HandleMessage: The systemhandle (%s) is not active. Unable to handle messages', [self.ClassName, fBoldSystemHandle.name]);
  vEvents := TStringList.Create;
  vEvents.CommaText := aMsg;
  try
    for I := 0 to vEvents.Count - 1 do
    begin
      vEvent := Trim(vEvents[i]);
      if pos('DISCONNECT:', vEvent) = 1 then
      begin
        if Assigned(fPropagatorHandle) then
          fPropagatorHandle.Connected := false;
        if assigned(OnDoDisconnect) then
        begin
          temp := copy(vEvent, pos(':', vEvent)+1, maxint);
          OnDoDisconnect(
            copy(temp, pos(':', temp)+1, maxint),
            StrToIntDef(copy(temp, 1, pos(':', temp)-1), -1)
          );
        end;
      end
      else
      begin
        ExactId := nil;
        ObjectID := TBoldDefaultID.Create;
        try
          SubsType := TBoldObjectSpaceExternalEvent.DecodeExternalEvent(vEvent,
                                                                        ClassName,
                                                                        MemberName,
                                                                        LockName,
                                                                        ObjectID);
          if (ClassName <> '') then
            ExactId := ObjectID.CloneWithClassId(BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[ClassName].TopSortedIndex, true) as TBoldDefaultID
          else
            ExactId := ObjectID.Clone as TBoldDefaultId;
          case SubsType of
            bsClassChanged: ClassChanged(ClassName);
            bsMemberChanged: MemberChanged(ClassName, MemberName, ExactId);
            bsEmbeddedStateOfObjectChanged: EmbeddedStateOfObjectChanged(ClassName, ExactId);
            bsObjectCreated: ObjectCreated(ClassName, ExactId);
            bsObjectDeleted: ObjectDeleted(ClassName, ExactId);
            bsNonEmbeddedStateOfObjectChanged: NonEmbeddedStateOfObjectChanged(ClassName, MemberName, ExactId);
            bsLockLost: LockLost(LockName);
          end;
        finally
          FreeAndNil(ObjectID);
          FreeAndNil(ExactId);
        end;
      end;
    end;
  finally
    vEvents.free;
  end;
end;

{$IFDEF UseBoldOSSMessage}
procedure TBoldExternalObjectSpaceEventHandler.HandleObjectMessage(
  const AOSSMessage: TOSSMessage);
var
  vEvents: TStringList;
  vEvent: string;
  i: integer;
begin
  if not assigned(fBoldSystemHandle) then
    raise EBold.CreateFmt('%s.HandleMessage: The Eventhandler (%s) is not connected to a systemhandle. Unable to handle messages', [self.ClassName, name]);
  if not assigned(fBoldSystemHandle.System) then
    raise EBold.CreateFmt('%s.HandleMessage: The systemhandle (%s) is not active. Unable to handle messages', [self.ClassName, fBoldSystemHandle.name]);
  vEvents := TStringList.Create;
  vEvents.Text := AOSSMessage.Events;
  try
    case AOSSMessage.MessageType of
      mtFail:; // TODO: anything to do here ? maybe at least call event OnFail
      mtSync :
        begin
          for I := 0 to vEvents.count-1 do
          begin
            vEvent := vEvents[i];
            HandleMessage(vEvent);
          end;
        end;
    end;
  finally
    vEvents.free;
  end;
end;
{$ENDIF}

procedure TBoldExternalObjectSpaceEventHandler.ClassChanged(const ClassName: String);
var
  ClassTypeInfo: TBoldClassTypeInfo;
  ClassList: TBoldObjectList;
begin
  ClassTypeInfo := BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[ClassName];
  if not Assigned(ClassTypeInfo) then
    raise EOSS.CreateFmt('Cannot find the class %s in the system.', [ClassName]);
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
  SendExtendedEvent(self, boeClassChanged, [ClassName]);
end;

procedure TBoldExternalObjectSpaceEventHandler.EmbeddedStateOfObjectChanged(
  const ClassName: String; ObjectID: TBoldDefaultID);
var
  CurrObj: TBoldObject;
begin
  CurrObj := GetObjectByID(ObjectID);
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
          FetchObject(ObjectId);
        CurrObj.Invalidate;
      end;
    end;
  end;
  SendExtendedEvent(self, boeEmbeddedStateOfObjectChanged, [CurrObj, className, ObjectId, CurrObj]);
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
  ClassTypeInfo: TBoldClassTypeInfo;
  IDList: TBoldObjectIdList;
  List: TBoldObjectList;
  ClassList: TBoldObjectList;
  TopSortedClasses: TBoldClassTypeInfoList;
  vBoldSystem: TBoldSystem;
begin
  vBoldSystem := BoldSystem;
  TopSortedClasses := vBoldSystem.BoldSystemTypeInfo.TopSortedClasses;
  for I := 0 to high(fIdFetchArray) do
  begin
    if Assigned(fIdFetchArray[i]) then
    begin
      IdList := fIdFetchArray[i];
      BoldSystem.FetchIdList(IdList, false);
    end;
  end;
  for I := 0 to high(fObjectFetchArray) do
  begin
    if Assigned(fObjectFetchArray[i]) then
    begin
      IdList := fObjectFetchArray[i];
      BoldSystem.FetchIdList(IdList, true);
    end;
  end;
end;

procedure TBoldExternalObjectSpaceEventHandler.FetchMember(Member: TBoldMember);
begin
//  fMemberFetchList.Add(Member);
end;

procedure TBoldExternalObjectSpaceEventHandler.NonEmbeddedStateOfObjectChanged(
  const ClassName: String; const MemberName: String; ObjectID: TBoldDefaultID);
var
  CurrObj: TBoldObject;
  CurrMember: TBoldMember;
  i: integer;
begin
  CurrMember := nil;
  CurrObj := GetObjectByID(ObjectID);
  if Assigned(CurrObj) then
  begin
    i := CurrObj.BoldMemberIndexByExpressionName[MemberName];
    if i = -1 then
      raise EOSS.CreateFmt('Class %s does not have a member "%s", check OSS settings of other clients.', [CurrObj.DisplayName, MemberName]);
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
  SendExtendedEvent(self, boeNonEmbeddedStateOfObjectChanged, [ObjectID, CurrObj, CurrMember, ClassName, MemberName]);
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
  CurrLocator := fBoldSystemHandle.System.Locators.LocatorByID[ObjectID];
  if Assigned(CurrLocator) then
    Result := CurrLocator.BoldObject;
end;

procedure TBoldExternalObjectSpaceEventHandler.LockLost(const LockName: String);
begin
  if Assigned(fOnLockLost) then
    fOnLockLost(LockName);
end;

procedure TBoldExternalObjectSpaceEventHandler.MemberChanged(
  const ClassName, MemberName: String; ObjectID: TBoldDefaultID);
var
  CurrObj: TBoldObject;
  CurrMember: TBoldMember;
  SizeOfInvalidatedList : Integer;
  SizeOfInvalidatedListCategory : String;
  sl: TStringList;
  i,j: integer;
  vHasConflicts: boolean;
begin
  CurrMember := nil;
  CurrObj := GetObjectByID(ObjectID);
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
        sl.CommaText := MemberName;
        vHasConflicts := false;
        for i := 0 to sl.Count - 1 do
        begin
          j := CurrObj.BoldMemberIndexByExpressionName[sl[i]];
          if j = -1 then
            raise EOSS.CreateFmt('Class %s does not have a member "%s", check OSS settings of other clients.', [CurrObj.DisplayName, MemberName]);
          CurrMember := CurrObj.BoldMemberIfAssigned[j];
          if Assigned(CurrMember) then
          begin
            sl.Objects[i] := CurrMember;
            if CurrMember.BoldDirty then
            begin
              vHasConflicts := true;
              Conflict(CurrMember);
            end;
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
        if not vHasConflicts then
          FetchObject(ObjectId);
//          BoldSystem.FetchMembersWithObject(CurrObj, sl.CommaText);
      finally
        sl.free;
      end;

//    end;
  end;
  SendExtendedEvent(self, boeMemberChanged, [ClassName, MemberName, ObjectId, CurrObj]);
end;

procedure TBoldExternalObjectSpaceEventHandler.ObjectCreated(const ClassName: String;
  ObjectId: TBoldDefaultID);
var
  ClassTypeInfo: TBoldClassTypeInfo;
  ClassList: TBoldObjectList;
  BoldObject: TBoldObject;
  Handled: boolean;
begin
  ClassTypeInfo := BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[ClassName];
  if not Assigned(ClassTypeInfo) then
    raise EOSS.CreateFmt('Cannot find the class %s in the system.', [ClassName]);
  BoldObject := nil;
  Handled := false;
  repeat
    ClassList := BoldSystem.Classes[ClassTypeInfo.TopSortedIndex];
    if KeepClassesCurrent and (ClassList.BoldPersistenceState <> bvpsInvalid) then
    begin
      if not Handled then
      begin
        case ClassList.BoldPersistenceState of
          bvpsCurrent: FetchObject(ObjectId);
          bvpsTransient: FetchId(ObjectId);
        end;
        Handled := true;
      end;
    end
    else
      ClassList.Invalidate;
    ClassTypeInfo := ClassTypeInfo.SuperClassTypeInfo;
  until not Assigned(ClassTypeInfo);
  SendExtendedEvent(self, boeObjectCreated, [ClassName, ObjectId, BoldObject]);
end;

procedure TBoldExternalObjectSpaceEventHandler.ObjectDeleted(const ClassName: String; ObjectId: TBoldDefaultID);
var
  ClassTypeInfo: TBoldClassTypeInfo;
  ClassList: TBoldObjectList;
  CurrObj: TBoldObject;
begin
  ClassTypeInfo := BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[ClassName];
  if not Assigned(ClassTypeInfo) then
    raise EOSS.CreateFmt('Cannot find the class %s in the system.', [ClassName]);
  CurrObj := GetObjectByID(ObjectID);
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
  SendExtendedEvent(self, boeObjectDeleted, [ClassName, ObjectId, CurrObj]);
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

initialization

end.
