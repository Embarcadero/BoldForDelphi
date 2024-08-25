{ Global compiler directives }
{$include bold.inc}
unit BoldSystemOldValuehandler;

interface

uses
  BoldDefs,
  BoldValueSpaceInterfaces,
  BoldValueInterfaces,
  BoldFreeStandingValues,
  BoldId,
  BoldSystem;
type

  TBoldOldValueHandler = class(TBoldAbstractOldValueHandler)
  private
    fOldValues: TBoldFreeStandingValueSpace;
  protected
    function GetOldValues: IBoldValueSpace; override;
    function GetIsEmpty: Boolean; override;
    procedure PurgeEqualValues; override;
  public
    constructor Create(System: TBoldSystem); override;
    destructor Destroy; override;
    procedure MemberValuePreChange(BoldMember: TBoldMember); override;
    procedure MemberPersistenceStatePreChange(BoldMember: TBoldMember; NewState: TBoldValuePersistenceState); override;
    procedure ObjectExistenceChange(BoldObject: TBoldObject); override;
    procedure ObjectExistencePersistenceStateChange(BoldObject: TBoldObject; NewState: TBoldValuePersistenceState); override;
  end;

implementation

uses
  SysUtils;

{ TBoldOldValueHandler }

constructor TBoldOldValueHandler.Create(System: TBoldSystem);
begin
  inherited Create(System);
  fOldValues := TBoldFreeStandingValueSpace.Create;
end;

destructor TBoldOldValueHandler.Destroy;
begin
  FreeAndNil(fOldValues);
  inherited;
end;

function TBoldOldValueHandler.GetIsEmpty: Boolean;
var
  i: integer;
  m: integer;
  ObjectIdList: TBoldObjectIdList;
  ObjectContents: TBoldFreeStandingObjectContents;
  value: TBoldFreeStandingValue;
begin
  result := fOldValues.IdCount = 0;
  if result then
    exit;
  ObjectIdList := TBoldObjectIdList.Create;
  try
    fOldValues.AllObjectIds(objectIdList, false); // do we really need ALL objects ?
    for i := 0 to ObjectIdList.Count-1 do
    begin
      ObjectContents := fOldValues.GetFSObjectContentsByObjectId(ObjectIdList[i]);
      if assigned(objectContents) then
        for m := 0 to ObjectContents.MemberCount do
        begin
          value := ObjectContents.FSValueByIndex[m];
          if assigned(value) then
          begin
            result := false;
            exit;
          end;
        end;
    end;
  finally
    ObjectIdList.Free;
  end;
end;

function TBoldOldValueHandler.GetOldValues: IBoldValueSpace;
begin
  Result := fOldValues;
end;

procedure TBoldOldValueHandler.MemberPersistenceStatePreChange(
  BoldMember: TBoldMember; NewState: TBoldValuePersistenceState);
var
  aFSObjectContent: TBoldFreeStandingObjectContents;
begin
  if fOldValues.IsEmpty then
    exit;
  if (BoldMember.BoldPersistenceState in [bvpsModified, bvpsCurrent]) and (NewState in [bvpsCurrent, bvpsInvalid]) then
  begin
    aFSObjectContent := fOldValues.GetFSObjectContentsByObjectId(BoldMember.OwningObject.BoldObjectLocator.BoldObjectID);
    if assigned(aFSObjectContent) then
    begin
      aFSObjectContent.RemoveMemberByIndex(BoldMember.BoldMemberRTInfo.Index);
      if aFSObjectContent.IsEmpty then
        fOldValues.RemoveFSObjectContents(aFSObjectContent);
    end;
  end;
end;

procedure TBoldOldValueHandler.MemberValuePreChange(
  BoldMember: TBoldMember);
begin
  if not BoldMember.OwningObject.BoldObjectIsNew and
     (BoldMember.BoldPersistenceState = bvpsCurrent) and
     BoldMember.BoldMemberRTInfo.CanHaveOldValue then
  begin
    CopyMemberToValueSpace(BoldMember, OldValues);
  end;
end;

procedure TBoldOldValueHandler.ObjectExistenceChange(
  BoldObject: TBoldObject);
begin

end;

procedure TBoldOldValueHandler.ObjectExistencePersistenceStateChange(
  BoldObject: TBoldObject; NewState: TBoldValuePersistenceState);
begin

end;

procedure TBoldOldValueHandler.PurgeEqualValues;
var
  i: integer;
  m: integer;
  ObjectIdList: TBoldObjectIdList;
  ObjectContents: TBoldFreeStandingObjectContents;
  Locator: TBoldObjectLocator;
  value: TBoldFreeStandingValue;
  Id: TBoldObjectId;
  Member: TBoldMember;
  bEqual: Boolean;

  function IdListPairEqual(IdList: TBFSObjectIdListrefPair; ObjectList: TBoldObjectList): Boolean;
  var
    i: integer;
  begin
    result := IdList.Count = ObjectList.Count;
    if result then
      for i := 0 to IdList.Count-1 do
        result := result and ObjectList.LocatorInList(System.Locators.LocatorByID[IdList.IdList2[i]]);
  end;

  function IdListEqual(IdList: IBoldObjectIdListRef; ObjectList: TBoldObjectList): Boolean;
  var
    i: integer;
  begin
    result := IdList.Count = ObjectList.Count;
    if result then
      for i := 0 to IdList.Count-1 do
        result := result and ObjectList.LocatorInList(System.Locators.LocatorByID[IdList.IdList[i]]);
  end;

  function IdPairEqual(IdRef: TBFSObjectIdRefPair; ObjectRef: TBoldObjectReference): Boolean;
  begin
    if assigned(IdRef.Id2) then
      result := assigned(ObjectRef.Locator) and IdRef.Id2.IsEqual[ObjectRef.Locator.BoldObjectID]
    else
      result := not assigned(ObjectRef.Locator)
  end;

  function IdEqual(IdRef: TBFSObjectIdRef; ObjectRef: TBoldObjectReference): Boolean;
  begin
    if Assigned(IdRef.Id) then
      result := assigned(ObjectRef.Locator) and IdRef.Id.IsEqual[ObjectRef.Locator.BoldObjectID]
    else
      result := not assigned(ObjectRef.Locator)
  end;

begin
  ObjectIdList := TBoldObjectIdList.Create;
  try
    fOldValues.AllObjectIds(objectIdList, false);
    for i := 0 to ObjectIdList.Count-1 do
    begin
      Id := ObjectIdList[i];
      Locator := System.Locators.LocatorByID[Id];
      // do we ever keep oldvalues for objects that are not loaded?
      if not assigned(Locator) or not assigned(Locator.BoldObject) then
        fOldValues.RemoveFSObjectContentsByObjectId(ObjectIdList[i])
      else
      begin
        ObjectContents := fOldValues.GetFSObjectContentsByObjectId(Id);
        if assigned(ObjectContents) then
        begin
          for m := 0 to ObjectContents.MemberCount do
          begin
            value := ObjectContents.FSValueByIndex[m];
            if assigned(value) then
            begin
              if not Locator.BoldObject.BoldMemberAssigned[m] then begin
                ObjectContents.RemoveMemberByIndex(m);
              end else begin
                Member := Locator.BoldObject.BoldMembers[m];
                if Member.BoldPersistenceState = bvpsInvalid then begin
                  ObjectContents.RemoveMemberByIndex(m); // invalidate should have handled this case already...
                end else if (value is TBFSObjectIdListrefPair) then begin
                  if IdListPairEqual(TBFSObjectIdListRefPair(value),
                                     Member as TBoldObjectList) then
                  begin
                    ObjectContents.RemoveMemberByIndex(m);
                  end;
                end else if (value is TBFSObjectIdListRef) then begin
                  if IdListEqual(TBFSObjectIdListRef(value),
                                 Member as TBoldObjectList) then
                  begin
                    ObjectContents.RemoveMemberByIndex(m);
                  end;
                end else if (value is TBFSObjectIdRefPair) then begin
                  if IdPairEqual(TBFSObjectIdRefPair(value),
                                 Member as TBoldObjectReference) then
                  begin
                    ObjectContents.RemoveMemberByIndex(m);
                  end;
                end else if (value is TBFSObjectIdRef) then begin
                  if IdEqual(TBFSObjectIdRef(value),
                             Member as TBoldObjectReference) then
                  begin
                    ObjectContents.RemoveMemberByIndex(m);
                  end;
                end else if (Member is TBoldAttribute) and
                            Member.IsEqualToValue(value) then
                begin
                  ObjectContents.RemoveMemberByIndex(m);
                end;
              end;
            end;
          end;

          bEqual := True;
          // Is there no longer any difference, OldValue can be completely removed
          for m := 0 to ObjectContents.MemberCount do begin
            value := ObjectContents.FSValueByIndex[m];
            if Assigned(value) then begin
              bEqual := False;
              Break;
            end;
          end;
          if bEqual then begin
            fOldValues.RemoveFSObjectContentsByObjectId(Id);
          end;
        end;
      end;
    end;
  finally
    ObjectIdList.Free;
  end;

  fOldValues.ClearWhenObjectContentsEmpty;
end;

end.
