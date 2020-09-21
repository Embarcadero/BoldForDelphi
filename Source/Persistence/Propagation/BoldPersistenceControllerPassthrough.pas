unit BoldPersistenceControllerPassthrough;

interface

uses
  BoldPersistenceController,
  BoldID,
  BoldCondition,
  BoldSubscription,
  BoldValueSpaceInterfaces,
  BoldUpdatePrecondition,
  BoldDefs;

type
  {forward declarations}
  TBoldPersistenceControllerPassthrough = class;

  { TBoldPersistenceControllerPassthrough }
  TBoldPersistenceControllerPassthrough = class(TBoldPersistenceController)
  private
    fNextPersistenceController: TBoldPersistenceController;
    function getNextPersistenceController: TBoldPersistenceController;
  public
    constructor Create;
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; BoldClientID: TBoldClientID); override;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMSetReadOnlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); override;
    procedure SubscribeToPeristenceEvents(Subscriber: TBoldSubscriber); override;
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
                  TranslationList: TBoldIdTranslationList); override;
    procedure PMTimestampForTime(ClockTime: TDateTime; var Timestamp: TBoldTimestampType); override;
    procedure PMTimeForTimestamp(Timestamp: TBoldTimestampType; var ClockTime: TDateTime); override;
    property NextPersistenceController: TBoldPersistenceController read getNextPersistenceController
              write fNextPersistenceController;
  end;

implementation

uses
  SysUtils,
  BoldPMConsts;

  { TBoldPersistenceControllerPassthrough }

constructor TBoldPersistenceControllerPassthrough.Create;
begin
  inherited;
end;

function TBoldPersistenceControllerPassthrough.getNextPersistenceController: TBoldPersistenceController;
begin
  if Assigned(fNextPersistenceController) then
    Result := fNextPersistenceController
  else
    raise EBold.CreateFmt(sNextControllerMissing, [ClassName]);
end;

procedure TBoldPersistenceControllerPassthrough.PMExactifyIds(
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  NextPersistenceController.PMExactifyIds(ObjectIdList, TranslationList);
end;

procedure TBoldPersistenceControllerPassthrough.PMFetch(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  MemberIdList: TBoldMemberIdList; FetchMode: Integer;
  BoldClientID: TBoldClientID);
begin
  NextPersistenceController.PMFetch(ObjectIdList, ValueSpace, MemberIdList, FetchMode, BoldClientID);
end;

procedure TBoldPersistenceControllerPassthrough.PMFetchIDListWithCondition(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  FetchMode: Integer; Condition: TBoldCondition;
  BoldClientID: TBoldClientID);
begin
  NextPersistenceController.PMFetchIDListWithCondition(ObjectIdList, ValueSpace, FetchMode, Condition, BoldClientID);
end;

procedure TBoldPersistenceControllerPassthrough.PMSetReadOnlyness(
  ReadOnlyList, WriteableList: TBoldObjectIdList);
begin
  NextPersistenceController.PMSetReadOnlyness(ReadOnlyList, WriteableList);
end;

procedure TBoldPersistenceControllerPassthrough.PMTranslateToGlobalIds(
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  NextPersistenceController.PMTranslateToGlobalIds(ObjectIdList, TranslationList);
end;

procedure TBoldPersistenceControllerPassthrough.PMTranslateToLocalIds(
  GlobalIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  NextPersistenceController.PMTranslateToLocalIds(GlobalIdList, TranslationList);
end;

procedure TBoldPersistenceControllerPassthrough.PMUpdate(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  Old_Values: IBoldValueSpace;
  Precondition: TBoldUpdatePrecondition;
  TranslationList: TBoldIdTranslationList;
  var TimeStamp: TBoldTimeStampType; BoldClientID: TBoldClientID);
begin
  NextPersistenceController.PMUpdate(ObjectIdList, ValueSpace, Old_Values,
    Precondition,
    TranslationList, TimeStamp, BoldClientID);
end;

procedure TBoldPersistenceControllerPassthrough.SubscribeToPeristenceEvents(
  Subscriber: TBoldSubscriber);
begin
  NextPersistenceController.SubscribeToPeristenceEvents(Subscriber);
end;

procedure TBoldPersistenceControllerPassthrough.ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
             TranslationList: TBoldIdTranslationList);
begin
  NextPersistenceController.ReserveNewIds(ValueSpace, ObjectIdList, TranslationList);
end;

procedure TBoldPersistenceControllerPassthrough.PMTimeForTimestamp(
  Timestamp: TBoldTimestampType; var ClockTime: TDateTime);
begin
  NextPersistenceController.PMTimeForTimestamp(Timestamp, ClockTime);
end;

procedure TBoldPersistenceControllerPassthrough.PMTimestampForTime(
  ClockTime: TDateTime; var Timestamp: TBoldTimestampType);
begin
  NextPersistenceController.PMTimestampForTime(ClockTime, Timestamp);
end;

end.
