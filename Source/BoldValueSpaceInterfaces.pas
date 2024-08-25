{ Global compiler directives }
{$include bold.inc}
unit BoldValueSpaceInterfaces;

interface

uses
  BoldValueInterfaces,
  BoldId,
  BoldDefs;

const
  BUSINESSCLASSESROOT_TOPSORTEDINDEX = 0;

type
  TBoldExistenceState = (besNotCreated, besExisting, besDeleted);

  IBoldValueSpace = interface;
  IBoldObjectContents = interface;

  IBoldValueSpace = interface
    ['{A90FA286-018A-4032-8392-72EA9213F3F5}']
    procedure AllObjectIds(resultList: TBoldObjectIdList; OnlyLoaded: Boolean);
    procedure ApplytranslationList(IdTranslationList: TBoldIdTranslationList);
    procedure ApplyValueSpace(const ValueSpace: IBoldValueSpace; IgnorePersistenceState: Boolean);
    procedure EnsureObjectContents(ObjectId: TBoldObjectId);
    procedure EnsureObjectId(ObjectId: TBoldObjectId);
    procedure ExactifyIDs(TranslationList: TBoldIdTranslationList);
    function GetHasContentsForId(ObjectId: TBoldObjectId): boolean;
    function GetObjectContentsByObjectId(ObjectId: TBoldObjectId): IBoldObjectContents;
    function GetEnsuredObjectContentsByObjectId(ObjectId: TBoldObjectId): IBoldObjectContents;
    function GetEnsuredObjectContentsByObjectIdAndCheckIfCreated(ObjectId: TBoldObjectId; out aBoldObjectContents: IBoldObjectContents): boolean;
    property HasContentsForId[ObjectId: TBoldObjectId]: boolean read GetHasContentsForId;
    property ObjectContentsByObjectId[ObjectId: TBoldObjectId]: IBoldObjectContents read GetObjectContentsByObjectId;
    property EnsuredObjectContentsByObjectId[ObjectId: TBoldObjectId]: IBoldObjectContents read GetEnsuredObjectContentsByObjectId;
    function IdCount: integer;
    function IsEmpty: boolean;
   end;

  IBoldObjectContents = interface
    ['{67C57AC5-621B-11D2-AFF7-006008F62CFF}']
    procedure EnsureMember(MemberId: TBoldMemberId; const ContentName: string);
    function EnsureMemberAndGetValueByIndex(MemberIndex: Integer; const ContentName: string): IBoldValue;
    function GetBoldExistenceState: TBoldExistenceState;
    function GetBoldMemberCount: Integer;
    function GetBoldPersistenceState: TBoldValuePersistenceState;
    function GetGlobalId: string;
    function GetIsModified: Boolean;
    function GetIsReadOnly: Boolean;
    function GetObjectId: TBoldObjectId;
    function GetValueByIndex(I: Integer): IBoldValue;
    function GetValueByMemberId(MemberId: TBoldMemberID):IBoldValue;
    function GetTimeStamp: TBoldTimeStampType;
    procedure SetBoldExistenceState(Value: TBoldExistenceState);
    procedure SetBoldPersistenceState(Value: TBoldValuePersistenceState);
    procedure SetGlobalId(const NewValue: string);
    procedure SetIsReadOnly(NewValue: Boolean);
    procedure SetTimeStamp(NewValue: TBoldTimeStampType);
    property BoldExistenceState: TBoldExistenceState read GetBoldExistenceState write SetBoldExistenceState;
    property BoldPersistenceState: TBoldValuePersistenceState read GetBoldPersistenceState write SetBoldPersistenceState;
    property GlobalId: string read GetGlobalId write SetGlobalId;
    property IsModified: Boolean read GetIsModified;
    property IsReadOnly: Boolean read GetIsReadOnly write SetIsReadOnly;
    property ValueByIndex[I: Integer]: IBoldValue read GetValueByIndex;
    property ValueByMemberId[MemberId: TBoldMemberID]:IBoldValue read GetValueByMemberId;
    property MemberCount: Integer read GetBoldMemberCount;
    property TimeStamp: TBoldTimeStampType read GetTimeStamp write SetTimeStamp;
    property ObjectId: TBoldObjectId read GetObjectId;
   end;


procedure BoldApplyPartialValueSpace(const DestVS, SourceVS: IBoldValueSpace; ObjectidList: TBoldObjectIdList; MemberIdList: TBoldMemberIdList;
  ForceCurrent: Boolean; PersistenceStatesToIgnore: TBoldValuePersistenceStateSet = [bvpsInvalid]);

implementation

uses
  SysUtils,
  BoldGuard;

procedure BoldApplyPartialValueSpace(const DestVS, SourceVS: IBoldValueSpace; ObjectidList: TBoldObjectIdList; MemberIdList: TBoldMemberIdList; ForceCurrent: Boolean; PersistenceStatesToIgnore: TBoldValuePersistenceStateSet = [bvpsInvalid]);
var
  O, M: integer;
  SourceObjectContents,
  DestObjectContents: IBoldObjectContents;
  MemberId: TBoldMemberId;
  ObjectId: TBoldObjectId;
  SourceValue,
  DestValue: IBoldValue;
  OwnIdList: TBoldObjectIdList;
  ListToCopy: TBoldObjectIdList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(OwnIdList);
  if Assigned(ObjectIdList) then
    ListToCopy := ObjectIdList
  else
  begin
    OwnIdList := TBoldObjectIdList.Create;
    SourceVS.AllObjectIds(OwnIdList, false);
    ListToCopy := OwnIdList;
  end;
  for O := 0 to ListToCopy.Count-1 do
  begin
    ObjectId := ListToCopy[O];
    SourceObjectContents := SourceVS.ObjectContentsByObjectId[ObjectId];
    if assigned(SourceObjectContents) then
    begin
      DestVS.EnsureObjectId(ObjectId);
      DestObjectContents := DestVS.EnsuredObjectContentsByObjectId[ObjectId];
      DestObjectContents.BoldExistenceState := SourceObjectContents.BoldExistenceState;
      if ForceCurrent then
        DestObjectContents.BoldPersistenceState := bvpsCurrent
      else
        DestObjectContents.BoldPersistenceState := SourceObjectContents.BoldPersistenceState;
      if assigned(MemberIdList) then
      begin
        for M := 0 to MemberIdList.Count-1 do
        begin
          MemberId := MemberIdList[M];
          SourceValue := SourceObjectContents.ValueByMemberId[MemberId];
          if assigned(SourceValue) and not (SourceValue.BoldPersistenceState in PersistenceStatesToIgnore) then
          begin
            DestValue := DestObjectContents.EnsureMemberAndGetValueByIndex(MemberId.MemberIndex, SourceValue.ContentName);;
            DestValue.AssignContent(SourceValue);
            if ForceCurrent then
              DestValue.BoldPersistenceState := bvpsCurrent;
          end;
        end;
      end
      else
      begin
        for M := 0 to SourceObjectContents.MemberCount-1 do
        begin
          SourceValue := SourceObjectContents.ValueByIndex[M];
          if assigned(SourceValue) and not (SourceValue.BoldPersistenceState in PersistenceStatesToIgnore) then
          begin
            DestValue := DestObjectContents.EnsureMemberAndGetValueByIndex(M, SourceValue.ContentName);
            DestValue.AssignContent(SourceValue);
            if ForceCurrent then
              DestValue.BoldPersistenceState := bvpsCurrent;
          end;
        end;
      end;
    end;
  end;
end;

end.
