
{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceController;

interface

uses
  BoldSubscription,
  BoldCondition,
  BoldId,
  BoldUpdatePrecondition,
  BoldValueSpaceInterfaces,
  BoldDefs,
  BoldElements,
  BoldDBInterfaces;

type
  { forward declarations }
  TBoldPersistenceController = class;

  {-- TBoldPersistenceController --}
  TBoldPersistenceController = class(TBoldSubscribableObject)
  public
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean); virtual; abstract;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); virtual; abstract;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); virtual; abstract;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID); virtual; abstract;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); virtual; abstract;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); virtual; abstract;
    procedure PMSetReadOnlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); virtual; abstract;
    procedure SubscribeToPersistenceEvents(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet = []); virtual;
    function MultilinksAreStoredInObject: Boolean; virtual;
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
                    TranslationList: TBoldIdTranslationList); virtual; abstract;
    procedure PMTimestampForTime(ClockTime: TDateTime; var Timestamp: TBoldTimestampType); virtual;
    procedure PMTimeForTimestamp(Timestamp: TBoldTimestampType; var ClockTime: TDateTime); virtual;
    // The BoldSystem is passed as TBoldElement,
    // because include of BoldSystem.pas would cause recursive dependency.
    function CanEvaluateInPS(sOCL: string; aSystem: TBoldElement; aContext: TBoldElementTypeInfo = nil; const aVariableList: TBoldExternalVariableList = nil): Boolean; virtual; abstract;
    procedure StartTransaction; virtual;
    procedure CommitTransaction; virtual;
    procedure RollbackTransaction; virtual;
    function DatabaseInterface: IBoldDatabase; virtual;
  end;

const
  {TBoldClientID - used for an invalid/nonexisting client}
  NOTVALIDCLIENTID = -1;

implementation

uses
  SysUtils;

{ TBoldPersistenceController }

procedure TBoldPersistenceController.StartTransaction;
begin
 // Can be overriden but not mandatory
end;

procedure TBoldPersistenceController.CommitTransaction;
begin
 // Can be overriden but not mandatory
end;

procedure TBoldPersistenceController.RollbackTransaction;
begin
 // If Rollback is ever called, then it has to be overriden so we raise an exception
  raise EBoldFeatureNotImplementedYet.CreateFmt('RollbackTransaction not supported by %s', [classname]);
end;

function TBoldPersistenceController.DatabaseInterface: IBoldDatabase;
begin
  result := nil;
end;

function TBoldPersistenceController.MultilinksAreStoredInObject: Boolean;
begin
  result := false;
end;

procedure TBoldPersistenceController.PMTimeForTimestamp(
  Timestamp: TBoldTimestampType; var ClockTime: TDateTime);
begin
  raise EBoldFeatureNotImplementedYet.CreateFmt('PMTimeForTimestamp not supported by %s', [classname]);
end;

procedure TBoldPersistenceController.PMTimestampForTime(
  ClockTime: TDateTime; var Timestamp: TBoldTimestampType);
begin
  raise EBoldFeatureNotImplementedYet.CreateFmt('PMTimestampForTime not supported by %s', [classname]);
end;

procedure TBoldPersistenceController.SubscribeToPersistenceEvents(
  Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet);
begin
  if Events = [] then
    AddSmallSubscription(Subscriber, bePersistenceEvents)
  else
    AddSmallSubscription(Subscriber, Events);
end;

end.
