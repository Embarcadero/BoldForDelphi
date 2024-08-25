{ Global compiler directives }
{$include bold.inc}
unit BoldPersistenceControllerPassthrough;

interface

uses
  BoldPersistenceController,
  BoldID,
  BoldCondition,
  BoldSubscription,
  BoldValueSpaceInterfaces,
  BoldUpdatePrecondition,
  BoldDefs,
  BoldElements,
  BoldDbInterfaces;

type
  {forward declarations}
  TBoldPersistenceControllerPassthrough = class;

  { TBoldPersistenceControllerPassthrough }
  TBoldPersistenceControllerPassthrough = class(TBoldPersistenceController)
  private
    fSubscriber: TBoldPassthroughSubscriber;
    fNextPersistenceController: TBoldPersistenceController;
    function GetNextPersistenceController: TBoldPersistenceController;
    procedure SetNextPersistenceController(const Value: TBoldPersistenceController);
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID); override;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMSetReadOnlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); override;
    procedure SubscribeToPersistenceEvents(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet = []); override;
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
                  TranslationList: TBoldIdTranslationList); override;
    procedure PMTimestampForTime(ClockTime: TDateTime; var Timestamp: TBoldTimestampType); override;
    procedure PMTimeForTimestamp(Timestamp: TBoldTimestampType; var ClockTime: TDateTime); override;
    function CanEvaluateInPS(sOCL: string; aSystem: TBoldElement; aContext: TBoldElementTypeInfo = nil; const aVariableList: TBoldExternalVariableList = nil): Boolean; override;
    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    function DatabaseInterface: IBoldDatabase; override;
    property NextPersistenceController: TBoldPersistenceController read GetNextPersistenceController write SetNextPersistenceController;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts;

  { TBoldPersistenceControllerPassthrough }

function TBoldPersistenceControllerPassthrough.CanEvaluateInPS(sOCL: string;
  aSystem: TBoldElement; aContext: TBoldElementTypeInfo;
  const aVariableList: TBoldExternalVariableList): Boolean;
begin
  Result := NextPersistenceController.CanEvaluateInPS(sOCL, aSystem, aContext, aVariableList);
end;

procedure TBoldPersistenceControllerPassthrough.SetNextPersistenceController(const Value: TBoldPersistenceController);
begin
  if fNextPersistenceController <> value then
  begin
    fNextPersistenceController := Value;
    FreeAndNil(fSubscriber);
    fSubscriber := TBoldPassthroughSubscriber.Create(Receive);
    fNextPersistenceController.AddSubscription(fSubscriber, beDestroying);
  end;
end;

procedure TBoldPersistenceControllerPassthrough.StartTransaction;
begin
  NextPersistenceController.StartTransaction;
end;

procedure TBoldPersistenceControllerPassthrough.CommitTransaction;
begin
  NextPersistenceController.CommitTransaction;
end;

procedure TBoldPersistenceControllerPassthrough.RollbackTransaction;
begin
  NextPersistenceController.RollbackTransaction;
end;

constructor TBoldPersistenceControllerPassthrough.Create;
begin
  inherited;
end;

function TBoldPersistenceControllerPassthrough.DatabaseInterface: IBoldDatabase;
begin
  if Assigned(fNextPersistenceController) then
    Result := fNextPersistenceController.DatabaseInterface
  else
    result := nil;
end;

destructor TBoldPersistenceControllerPassthrough.Destroy;
begin
  FreeAndNil(fSubscriber);
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
  TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean);
begin
  NextPersistenceController.PMExactifyIds(ObjectIdList, TranslationList, HandleNonExisting);
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
  var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID);
begin
  NextPersistenceController.PMUpdate(ObjectIdList, ValueSpace, Old_Values,
    Precondition,
    TranslationList, TimeStamp, TimeOfLatestUpdate, BoldClientID);
end;

procedure TBoldPersistenceControllerPassthrough.SubscribeToPersistenceEvents(
  Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet);
begin
  NextPersistenceController.SubscribeToPersistenceEvents(Subscriber, Events);
end;

procedure TBoldPersistenceControllerPassthrough.Receive(Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  if Originator = fNextPersistenceController then
    fNextPersistenceController := nil;
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
