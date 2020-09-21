unit BoldPersistenceController;

interface

uses
  BoldSubscription,
  BoldCondition,
  BoldId,
  BoldUpdatePrecondition,
  BoldValueSpaceInterfaces,
  BoldDefs;

type
  { forward declarations }
  TBoldPersistenceController = class;

  {-- TBoldPersistenceController --}
  TBoldPersistenceController = class(TBoldSubscribableObject)
  public
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); virtual; abstract;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); virtual; abstract;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); virtual; abstract;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; BoldClientID: TBoldClientID); virtual; abstract;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); virtual; abstract;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); virtual; abstract;
    procedure PMSetReadOnlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); virtual; abstract;
    procedure SubscribeToPeristenceEvents(Subscriber: TBoldSubscriber); virtual;
    // this info should be stored in separate Mapping model
    function MultilinksAreStoredInObject: Boolean; virtual;
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
                    TranslationList: TBoldIdTranslationList); virtual; abstract;
    procedure PMTimestampForTime(ClockTime: TDateTime; var Timestamp: TBoldTimestampType); virtual;
    procedure PMTimeForTimestamp(Timestamp: TBoldTimestampType; var ClockTime: TDateTime); virtual;
  end;

const
  {TBoldClientID - used for an invalid/nonexisting client}
  NOTVALIDCLIENTID = -1;

implementation

uses
  SysUtils,
  PersistenceConsts;

{ TBoldPersistenceController }

function TBoldPersistenceController.MultilinksAreStoredInObject: Boolean;
begin
  result := false;
end;

procedure TBoldPersistenceController.PMTimeForTimestamp(
  Timestamp: TBoldTimestampType; var ClockTime: TDateTime);
begin
  raise EBoldFeatureNotImplementedYet.CreateFmt(sPMTimeForTimeStampNotSupported, [classname]);
end;

procedure TBoldPersistenceController.PMTimestampForTime(
  ClockTime: TDateTime; var Timestamp: TBoldTimestampType);
begin
  raise EBoldFeatureNotImplementedYet.CreateFmt(sPMTimeStampForTimeNotSupported, [classname]);
end;

procedure TBoldPersistenceController.SubscribeToPeristenceEvents(
  Subscriber: TBoldSubscriber);
begin
  AddSubscription(Subscriber, bpeStartFetch, bpeStartFetch);
  AddSubscription(Subscriber, bpeEndFetch, bpeEndFetch);

  AddSubscription(Subscriber, bpeStartUpdate, bpeStartUpdate);
  AddSubscription(Subscriber, bpeEndUpdate, bpeEndUpdate);

  AddSubscription(Subscriber, bpeStartFetchId, bpeStartFetchId);
  AddSubscription(Subscriber, bpeEndFetchId, bpeEndFetchId);
end;

end.


