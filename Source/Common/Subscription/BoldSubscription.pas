
{ Global compiler directives }
{$include bold.inc}
unit BoldSubscription;

interface

uses
  Classes,
  BoldBase;

const
  beMinSmallReserved = 0;
  beMaxSmallReserved = 27;
  beMinSmallUser = 28;
  beMaxSmallUser = 30;
  beMaxEvent = 32767;
  beMinUser = 4096;
  beMaxUser = beMaxEvent;
  bqMinQuery = beMaxEvent+1;
  bpeMinReserved = 1024;
  bqMaxQuery = 100000;

  beDestroying = 0; {General message for all subscribables}
  beMemberChanged = 1; beObjectDeleted = 2; beObjectCreated = 3; {Object events}
  beItemAdded = 4; beItemDeleted = 5; beItemReplaced = 6; beOrderChanged = 7; {ObjectList events}
  beValueChanged = 8; {value of attribute or ObjectReference changed}
  beValueIdentityChanged = 9;  {Actual identity of anElement.value changed}
  breReSubscribe = 10;
  beDirtyListInvalidOrItemDeleted = 11;
  beValueInvalid = 12;
  beLocatorDestroying = 13;
  beObjectTimestampChanged = beValueIdentityChanged; // or perhaps use a separate integer ?
  beQualifierChanged = 14;
  beDeactivating = 15;
  beRolledBack = 16;
  bePostUpdateId = 17;
  bePreUpdateId = 18;
  beObjectFetched = 19;
  beObjectUnloaded = 20; // sent by TBoldSystem before an object gets unloaded from memory
  breReEvaluate = beValueChanged; {backwards compatibility}

  beDefaultRequestedEvent = breReEvaluate;

  beServerSubscriberRemoved = 21;

  beBeginUpdate = 22; // sent by TBoldObjectList before loops
  beEndUpdate = 23; // sent by TBoldObjectList after loops
  beObjectBecomingClean = 24;
  beObjectBecomingDirty = 25;
  beMemberBecomingClean = 26;
  beMemberBecomingDirty = 27;
  beDirtyListEvents = [beObjectBecomingDirty, beObjectBecomingClean, beMemberBecomingDirty, beMemberBecomingClean];

  beValueEvents = [beItemAdded, beItemDeleted, beItemReplaced, beOrderChanged, beValueChanged, beValueInvalid];
  bpeStartFetch = bpeMinReserved + 0;
  bpeEndFetch = bpeMinReserved + 1;
  bpeStartUpdate = bpeMinReserved + 2;
  bpeEndUpdate = bpeMinReserved + 3;
  bpeFetchObject = bpeMinReserved + 4;
  bpeFetchMember = bpeMinReserved + 5;
  bpeUpdateObject = bpeMinReserved + 6;
  bpeDeleteObject = bpeMinReserved + 7;
  bpeCreateObject = bpeMinReserved + 8;
  bpeStartFetchID = bpeMinReserved + 9;
  bpeEndFetchID = bpeMinReserved + 10;
  bpeFetchId = bpeMinReserved + 11;
  bpeProgressStart = bpeMinReserved + 12;
  bpeProgressEnd = bpeMinReserved + 13;
  bpeMaxReserved = bpeMinReserved + 13;

  // additional persistence events
  bpeStartFetchMember = 42;
  bpeEndFetchMember = 43;
  bpeStartFetchObjectById = 44;
  bpeEndFetchObjectById = 45;
  bpeStartFetchClass = 46;
  bpeEndFetchClass = 47;
  bpeStartFetchAllInClassWithRawSQL = 48;
  bpeEndFetchAllInClassWithRawSQL = 49;
  bpeStartFetchAllInClassWithSQL = 50;
  bpeEndFetchAllInClassWithSQL = 51;

  // OSS Events
  boeClassChanged = 52;
  boeEmbeddedStateOfObjectChanged = 53;
  boeObjectCreated = 54;
  boeObjectDeleted = 55;
  boeNonEmbeddedStateOfObjectChanged = 56;
  boeMemberChanged = 57;
  beOssEvents = [boeClassChanged, boeEmbeddedStateOfObjectChanged, boeObjectCreated, boeObjectDeleted, boeNonEmbeddedStateOfObjectChanged, boeMemberChanged];

  bePrepareModify = 38;
  beCompleteModify = 39;
  bePrepareDelete = 41;
 {$IFNDEF BOLD_NO_QUERIES}
  bqMayUpdate = bqMinQuery + 1;
  bqMayModify = bqMinQuery + 2;
  bqMayDelete = bqMinQuery + 3;
  bqMayCommit = bqMinQuery + 4;
 {$ENDIF}
  bqMaxSubscription = bqMinQuery + 6;
  const
    beBigEventFlag = 1 shl 30;

{$IFDEF BoldSystemBroadcastMemberEvents}
  beBroadcastMemberEvents = beValueEvents + [beCompleteModify] + beDirtyListEvents;
{$ENDIF}

  { Subscription Statistics }
var
  PublisherCount: Integer = 0;
  SubscriberCount: Integer = 0;
  ActiveSubscriptionCount: Integer = 0;
  _SendEventMatch: Int64 = 0;
  _SendExtendedEvent: Int64 = 0;
{$IFNDEF BOLD_NO_QUERIES}
  _SendQuery: Int64 = 0;
  _QueryMatch: Int64 = 0;
{$ENDIF}

  {Forward declarations of all classes}
type
  TBoldPublisher = class;
  TBoldSubscriber = class;
  TBoldPassthroughSubscriber = class;
  TBoldSubscribableObject = class;
  TBoldSubscribableComponent = class;
  TBoldSubscribablePersistent = class;

  {TBoldEvent itself is a Smallint. TBoldEventset is a set of of small values (0..31) It is possible
   to subscribe either to one particular event, or to a set of small events. Of the small
   events, 0..23 are reserved for Bold and 24..30 are available for users. Of the larger events,
   32..64 are reserved for the future use of Bold, the rest are available for user programming}

  {TBoldEvent}
  TBoldEvent = 0..bqMaxQuery;
  TBoldRequestedEvent = Integer;

  TBoldSmallEvent = beMinSmallReserved..beMaxSmallUser;
  TBoldSmallEventSet = set of TBoldSmallEvent;

  TBoldEventHandler = procedure (Originator: TObject; OriginalEvent: TBoldEvent;
    RequestedEvent: TBoldRequestedEvent) of object;

  TBoldExtendedEventHandler = procedure (Originator: TObject; OriginalEvent: TBoldEvent;
    RequestedEvent: TBoldRequestedEvent; const Args: array of const) of object;

  TBoldQueryHandler = function (Originator: TObject; OriginalEvent: TBoldEvent;
    RequestedEvent: TBoldRequestedEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean of object;

  TBoldPublisherFlag = (bpfNeedsPacking);
  TBoldPublisherFlags = set of TBoldPublisherFlag;


  {---TBoldSubscription---}
  TBoldSubscription = record
  public
    Subscriber: TBoldSubscriber;
    IndexInSubscriber: integer;
    RequestedEvent: TBoldRequestedEvent;
    MatchCondition: Integer;
    function GetIsSmallEventSubscription: Boolean; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure ClearEntry; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure ReIndexsSubscriber(NewPublisherIndex: integer); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function IsMatchingEvent(OriginalEvent: TBoldEvent): Boolean; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure ExtendEvents(Events: TBoldSmallEventSet); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    property IsSmallEventSubscription: Boolean read GetIsSmallEventSubscription;
  end;

  TBoldPublisherReference = record
  public
    Publisher: TBoldPublisher;
    Index: integer; // index in the publishers subscriptionarray
  end;

  TBoldPublisherReferenceArray = array of TBoldPublisherReference;

  TBoldSubscriptionArray = array of TBoldSubscription;

  {---TBoldPublisher---}
  TBoldPublisher = class(TBoldMemoryManagedObject)
  strict private
    fSubscriptionArray: TBoldSubscriptionArray;
    fSubscriptionCount: integer;
    fPublisherFlags: TBoldPublisherFlags;
    fSubscribableObject: TBoldSubscribableObject;
    fHoleCount: Integer;
    function GetHasSubscribers: Boolean; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure EnsureFreeSpace; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetNeedsPacking: Boolean; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SetNeedsPacking(Value: Boolean);
    class procedure RemoveFromPostNotificationQueue(Receiver: TObject);
    procedure PackSubscriptions(dummy: TObject);
    property NeedsPacking: Boolean read GetNeedsPacking write SetNeedsPacking;  {Set by TBoldSubscription}
    class var G_NotificationNesting: integer;
    class var G_InPostNotification: Boolean;
  private // actually unit internal
    class procedure DelayTillAfterNotification(Event: TNotifyEvent; Sender: TObject; Receiver: TObject);
    procedure ClearEntry(Subscriber: TBoldSubscriber; index: integer); // Do not use inline due to D2007 bug
    function GetSubscriptionsAsText: string;
  strict protected
    procedure SetPublisherFlag(Flag: TBoldPublisherFlag; Value: Boolean); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetPublisherFlag(Flag: TBoldPublisherFlag): Boolean; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetContextString: string; virtual;
    function GetDebugInfo: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    class procedure StartNotify; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    class procedure EndNotify;
    procedure BoldForcedDequeuePostNotify;    
    procedure NotifySubscribersAndClearSubscriptions(Originator: TObject); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure AddSmallSubscription(Subscriber: TBoldSubscriber;
      Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent = beDefaultRequestedEvent); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure AddSubscription(Subscriber: TBoldSubscriber;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent = beDefaultRequestedEvent); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SendExtendedEvent(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const);
{$IFNDEF BOLD_NO_QUERIES}
    function SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
{$ENDIF}
    function HasMatchingSubscription(Subscriber: TBoldSubscriber): boolean; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure CancelSubscriptionTo(Subscriber: TBoldSubscriber);
    property HasSubscribers: Boolean read GetHasSubscribers;
    property SubscribableObject : TBoldSubscribableObject read fSubscribableObject write fSubscribableObject;
    property SubscriptionCount: integer read fSubscriptionCount;
    property ContextString: string read GetContextString;
    property SubscriptionsAsText: string read GetSubscriptionsAsText;
  end;

  {---TBoldSubscriber---}
  TBoldSubscriber = class(TBoldMemoryManagedObject)
  strict private
    fSubscriptionArray: TBoldPublisherReferenceArray;
    fSubscriptionCount: integer;
  private  // unit internal
    procedure AddToSubscriptions(Publisher: TBoldPublisher; Index: integer); {$IFDEF BOLD_INLINE} inline; {$ENDIF} {called by TBoldSubscription}
    procedure ClearEntry(Index: integer); {$IFDEF BOLD_INLINE} inline; {$ENDIF} {called by TBoldSubscription}
    function GetSubscriptionsAsText: string;
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent); virtual; abstract;
    procedure ReceiveExtended(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
 {$IFNDEF BOLD_NO_QUERIES}
    function Answer(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; virtual;
{$ENDIF}
    function GetContextString: string; virtual;
    function GetHandlesExtendedEvents: Boolean; virtual;
    function GetDebugInfo: string; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure CancelAllSubscriptions; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function HasMatchingSubscription(APublisher: TBoldPublisher): boolean; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure CancelSubscriptionTo(APublisher: TBoldPublisher);
    property ContextString: string read GetContextString;
    property HandlesExtendedEvents: Boolean read GetHandlesExtendedEvents;
    property SubscriptionCount: integer read fSubscriptionCount;
    property Subscriptions: TBoldPublisherReferenceArray read fSubscriptionArray;
    property SubscriptionsAsText: string read GetSubscriptionsAsText;
  end;

  {---TBoldPassthroughSubscriber---}
  TBoldPassthroughSubscriber = class(TBoldSubscriber)
  strict private
    fReceiveFunc: TBoldEventHandler;
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent); override;
    function GetHandlesExtendedEvents: Boolean; override;
    function GetContextString: string; override;
  public
    constructor Create(AReceiveFunc: TBoldEventHandler);
    property receiveFunc: TBoldEventHandler read fReceiveFunc write fReceiveFunc;
  end;

  {---TBoldPassthroughSubscriber---}
  TBoldExtendedPassthroughSubscriber = class(TBoldPassthroughSubscriber)
  strict private
    fExtendedReceiveFunc: TBoldExtendedEventHandler;
 {$IFNDEF BOLD_NO_QUERIES}
    fAnswerFunc: TBoldQueryHandler;
{$ENDIF}
  protected
    procedure ReceiveExtended(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
 {$IFNDEF BOLD_NO_QUERIES}
    function Answer(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; override;
{$ENDIF}
    function GetHandlesExtendedEvents: Boolean; override;
  public
    constructor CreateWithExtendedReceive(AExtendedReceiveFunc: TBoldExtendedEventHandler);
{$IFNDEF BOLD_NO_QUERIES}
    constructor CreateWithReceiveAndAnswer(AReceiveFunc: TBoldEventHandler;
                                           AAnswerFunc: TBoldQueryHandler);
{$ENDIF}
  end;

  {---TBoldSubscribableObject---}
  TBoldSubscribableObject = class(TBoldFlaggedObject)
 strict  private
    fPublisher: TBoldPublisher;
    function GetHasSubscribers: Boolean; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetPublisher: TBoldPublisher; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetSubscriptionsAsText: string;
  protected
    function GetDebugInfo: string; override;
    procedure FreePublisher; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetContextString: string; virtual;
    property HasSubscribers: Boolean read GetHasSubscribers;
    property Publisher: TBoldPublisher read GetPublisher;
  public
    destructor Destroy; override;
    procedure AddSmallSubscription(Subscriber: TBoldSubscriber;
      Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent = beDefaultRequestedEvent); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure AddSubscription(Subscriber: TBoldSubscriber;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent = beDefaultRequestedEvent); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SendEvent(OriginalEvent: TBoldEvent); virtual;
    procedure SendExtendedEvent(OriginalEvent: TBoldEvent; const Args: array of const); virtual;
{$IFNDEF BOLD_NO_QUERIES}
    function SendQuery(OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber; Originator: TObject = nil): Boolean; virtual;
{$ENDIF}
    property ContextString: string read GetContextString;
    property SubscriptionsAsText: string read GetSubscriptionsAsText;
end;

  {---TBoldSubscribableComponent---}
  TBoldSubscribableComponent = class(TComponent)
  strict private
    fPublisher: TBoldPublisher;
    function GetPublisher: TBoldPublisher; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetSubscriptionsAsText: string;
  strict protected
    function GetHasSubscribers: Boolean; virtual;
    property Publisher: TBoldPublisher read GetPublisher;
    procedure FreePublisher; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  public
    destructor Destroy; override;
    procedure AddSmallSubscription(Subscriber: TBoldSubscriber;
      Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent = beDefaultRequestedEvent); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure AddSubscription(Subscriber: TBoldSubscriber;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent = beDefaultRequestedEvent); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SendEvent(Originator: TObject; OriginalEvent: TBoldEvent); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SendExtendedEvent(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const);
    {$IFNDEF BOLD_NO_QUERIES}
    function SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
    {$ENDIF}
    property HasSubscribers: Boolean read GetHasSubscribers;
    property SubscriptionsAsText: string read GetSubscriptionsAsText;
  end;

  {--- TBoldSubscribablePersistent ---}
  TBoldSubscribablePersistent = class(TPersistent)
  strict private
    fPublisher: TBoldPublisher;
    function GetPublisher: TBoldPublisher; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetSubscriptionsAsText: string;
  strict protected
    function GetHasSubscribers: Boolean; virtual;
    property Publisher: TBoldPublisher read GetPublisher;
  protected
    procedure FreePublisher; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  public
    destructor Destroy; override;
    procedure AddSmallSubscription(Subscriber: TBoldSubscriber;
      Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent = beDefaultRequestedEvent); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure AddSubscription(Subscriber: TBoldSubscriber;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent = beDefaultRequestedEvent); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SendEvent(Originator: TObject; OriginalEvent: TBoldEvent); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SendExtendedEvent(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const);
{$IFNDEF BOLD_NO_QUERIES}
    function SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
{$ENDIF}
    property HasSubscribers: Boolean read GetHasSubscribers;
    property SubscriptionsAsText: string read GetSubscriptionsAsText;
  end;
  
  procedure BoldAddEventToPostNotifyQueue(Event: TNotifyEvent; Sender: TObject; Receiver: TObject);
  function BoldEventToString(aEvent: integer): string;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldEventQueue;

function BoldEventToString(aEvent: integer): string;
begin
  case aEvent of
    beDestroying: result := 'beDestroying';
    beMemberChanged: result := 'beMemberChanged';
    beObjectDeleted: result := 'beObjectDeleted';
    beObjectCreated: result := 'beObjectCreated';
    beItemAdded: result := 'beItemAdded';
    beItemDeleted: result := 'beItemDeleted';
    beItemReplaced: result := 'beItemReplaced';
    beOrderChanged: result := 'beOrderChanged';
    beValueChanged: result := 'beValueChanged';
    beValueIdentityChanged: result := 'beValueIdentityChanged';
    beValueInvalid: result := 'beValueInvalid';
//    beDerivedSoonDestroyed: result := 'beDerivedSoonDestroyed';
    beLocatorDestroying: result := 'beLocatorDestroying';
    beDirtyListInvalidOrItemDeleted: result := 'beDirtyListInvalidOrItemDeleted';
    beQualifierChanged: result := 'beQualifierChanged';
    beDeactivating: result := 'beDeactivating';
    beRolledBack: result := 'beRolledBack';
    bePostUpdateId: result := 'bePostUpdateId';
    bePreUpdateId: result := 'bePreUpdateId';
    beObjectFetched: result := 'beObjectFetched';
    bePrepareModify: result := 'bePrepareModify';
    beCompleteModify: result := 'beCompleteModify';
    bePrepareDelete: result := 'bePrepareDelete';
    breReSubscribe: result := 'breReSubscribe';
    beServerSubscriberRemoved: result := 'beServerSubscriberRemoved';
    bpeStartFetch: result := 'bpeStartFetch';
    bpeEndFetch: result := 'bpeEndFetch';
    bpeStartUpdate: result := 'bpeStartUpdate';
    bpeEndUpdate: result := 'bpeEndUpdate';
    bpeFetchObject: result := 'bpeFetchObject';
    bpeFetchMember: result := 'bpeFetchMember';
    bpeUpdateObject: result := 'bpeUpdateObject';
    bpeDeleteObject: result := 'bpeDeleteObject';
    bpeCreateObject: result := 'bpeCreateObject';
    bpeStartFetchID: result := 'bpeStartFetchID';
    bpeEndFetchID: result := 'bpeEndFetchID';
    bpeFetchId: result := 'bpeFetchId';
    bpeProgressStart: result := 'bpeProgressStart';
    bpeProgressEnd: result := 'bpeProgressEnd';
 {$IFNDEF BOLD_NO_QUERIES}
    bqMayUpdate: result := 'bqMayUpdate';
    bqMayModify: result := 'bqMayModify';
    bqMayDelete: result := 'bqMayDelete';
    bqMayCommit: result := 'bqMayCommit';
 {$ENDIF}
  else
  case aEvent of
    beMinSmallReserved..beMaxSmallReserved : result := 'Unknown SmallReserved event: ' + IntToStr(aEvent);
    beMinSmallUser..beMaxSmallUser : result := 'Unknown SmallUser event: ' + IntToStr(aEvent);
    beMinUser..beMaxEvent : result := 'Unknown UserEvent event: ' + IntToStr(aEvent);
    bqMinQuery..bqMaxQuery : result := 'Unknown Query event: ' + IntToStr(aEvent);
  else
    result:= 'Unknown event: ' + IntToStr(aEvent);
  end;
  end;
end;

var
  G_PostNotifyQueue: TboldEventQueue = nil;

function GetNewLength(oldLength: integer): integer; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
begin
  if oldLength > 64 then
    Result := oldLength + oldLength div 4
  else  if oldLength > 8 then
    Result := oldLength + 16
  else
    Result := oldLength + 4;
end;

procedure TBoldPublisher.BoldForcedDequeuePostNotify;
var
  OldInPostNotification: Boolean;
begin
  if assigned(G_PostNotifyQueue) then
  begin
    OldInPostNotification := G_InPostNotification;
    G_InPostNotification := True;
    G_PostNotifyQueue.DequeueAll;
    G_InPostNotification := False;
    G_InPostNotification := OldInPostNotification;
  end;
end;

procedure BoldAddEventToPostNotifyQueue(Event: TNotifyEvent; Sender: TObject; Receiver: TObject);
begin
  TBoldPublisher.DelayTillAfterNotification(Event, Sender, Receiver);
end;

class procedure TBoldPublisher.StartNotify;
begin
  Inc(G_NotificationNesting);
end;

class procedure TBoldPublisher.EndNotify;
begin
  Dec(G_NotificationNesting);
  if (G_NotificationNesting = 0) and Assigned(G_PostNotifyQueue) and (not G_InPostNotification) then
  begin
    G_InPostNotification := True;
    G_PostNotifyQueue.DequeueAll; {may add and delete entries, Eventqueue can handle it}
    G_InPostNotification := False;
  end;
end;

{---TBoldSubscription---}

function TBoldSubscription.GetIsSmallEventSubscription: Boolean;
begin
  Result := (MatchCondition and beBigEventFlag) = 0;
end;

function TBoldSubscription.IsMatchingEvent(OriginalEvent: TBoldEvent): Boolean;
begin
  if (OriginalEvent <= beMaxSmallUser) then
    result := (OriginalEvent in TBoldSmallEventSet(MatchCondition)) and ((MatchCondition and beBigEventFlag) = 0)
  else
    Result := (OriginalEvent or beBigEventFlag) = MatchCondition;
end;

procedure TBoldSubscription.ReIndexsSubscriber(NewPublisherIndex: integer);
begin
  Subscriber.Subscriptions[IndexInSubscriber].Index := NewPublisherIndex;
end;

procedure TBoldSubscription.ExtendEvents(Events: TBoldSmallEventSet);
begin
  if (MatchCondition and beBigEventFlag) <> 0 then
    raise EBoldInternal.Create('TBoldSubscription.ExtendEvents called for big event');
  MatchCondition := Integer(TBoldSmallEventSet(MatchCondition) + Events);
end;

procedure TBoldSubscriber.ClearEntry(Index: integer);
begin
  Subscriptions[Index].Publisher := nil;
  // Attempt to reuse empty places
  if fSubscriptionCount-1 = index then
  begin
    while (fSubscriptionCount > 0) and (Subscriptions[fSubscriptionCount-1].Publisher = nil) do
      Dec(fSubscriptionCount);
    if (fSubscriptionCount < length(fSubscriptionArray) div 2) and (length(fSubscriptionArray) > 4) then
      SetLength(fSubscriptionArray, fSubscriptionCount);
  end;
end;

procedure TBoldSubscriber.AddToSubscriptions(Publisher: TBoldPublisher; Index: integer);
begin
  if fSubscriptionCount = Length(fSubscriptionArray) then
    SetLength(fSubscriptionArray, GetNewLength(fSubscriptionCount));
  fSubscriptionArray[fSubscriptionCount].Publisher := Publisher;
  fSubscriptionArray[fSubscriptionCount].Index := Index;
  Inc(fSubscriptionCount);
  Inc(ActiveSubscriptionCount);
end;

procedure TBoldSubscription.ClearEntry;
begin
  if Assigned(subscriber) then
  begin
    Subscriber.ClearEntry(IndexInSubscriber);
    Subscriber := nil;
    Dec(ActiveSubscriptionCount);
  end;
end;

{---TBoldPublisher---}

function TBoldPublisher.GetContextString: string;
begin
  if Assigned(fSubscribableObject) then
    result := fSubscribableObject.ClassName
  else
    result := ClassName;  
end;

function TBoldPublisher.GetDebugInfo: string;
begin
  result := GetContextString;
end;

function TBoldPublisher.GetHasSubscribers: Boolean;
begin
  result := fSubscriptionCount > 0;
end;

function TBoldPublisher.GetPublisherFlag(Flag: TBoldPublisherFlag): Boolean;
begin
  result := Flag in fPublisherFlags;
end;

function TBoldPublisher.GetSubscriptionsAsText: string;
var
  i,j: integer;
begin
  result := '';
  j := 0;
  for I := 0 to Length(fSubscriptionArray) - 1 do
  begin
    if Assigned(fSubscriptionArray[i].Subscriber) then
    begin
      result := result + IntToStr(j) + ':' + fSubscriptionArray[i].Subscriber.ContextString+ #13#10;
      inc(j);
    end;
  end;
end;

function TBoldPublisher.HasMatchingSubscription(
  Subscriber: TBoldSubscriber): boolean;
var
  i: integer;
begin
  if fSubscriptionCount < Subscriber.SubscriptionCount then
  begin
    for I := fSubscriptionCount - 1 downto 0 do
      if (fSubscriptionArray[I].Subscriber = Subscriber) then
      begin
        result := true;
        exit;
      end;
  end
  else
  for I := Subscriber.SubscriptionCount - 1 downto 0 do
    if (Subscriber.Subscriptions[I].Publisher = Self) then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

procedure TBoldPublisher.CancelSubscriptionTo(Subscriber: TBoldSubscriber);
var
  i: integer;
begin
  StartNotify;
  try
    if fSubscriptionCount <= Subscriber.SubscriptionCount then
    begin
      for I := 0 to SubscriptionCount-1 do
        if (fSubscriptionArray[I].Subscriber = Subscriber) then
          ClearEntry(Subscriber, i);
    end
    else
    for I := 0 to Subscriber.SubscriptionCount - 1 do
      if (Subscriber.Subscriptions[I].Publisher = Self) then
        ClearEntry(Subscriber, Subscriber.Subscriptions[I].Index);
  finally
    EndNotify;
  end;
end;

procedure TBoldPublisher.SetPublisherFlag(Flag: TBoldPublisherFlag;
  Value: Boolean);
begin
  if Value then
    Include(fPublisherFlags, Flag)
  else
    Exclude(fPublisherFlags, Flag);
end;

function TBoldPublisher.GetNeedsPacking: Boolean;
begin
  result := GetPublisherFlag(bpfNeedsPacking);
end;

procedure TBoldPublisher.SetNeedsPacking(Value: Boolean);
begin
  if NeedsPacking <> Value then
  begin
    SetPublisherFlag(bpfNeedsPacking, Value);
    // the postnotify queue will be gone if we are in finalization,
    // but then we don't need to pack since we will be destroyed soon anyway...
    if Value and assigned(G_PostNotifyQueue) then
      DelayTillAfterNotification(PackSubscriptions, nil, Self);
  end;
end;

procedure TBoldPublisher.EnsureFreeSpace;
begin
  if fSubscriptionCount = Length(fSubscriptionArray) then
    SetLength(fSubscriptionArray, GetNewLength(fSubscriptionCount));
end;

procedure TBoldPublisher.ClearEntry(Subscriber: TboldSubscriber; index: integer);
begin
  if fSubscriptionArray[index].Subscriber <> Subscriber then
  begin
    if Assigned(fSubscriptionArray[index].Subscriber) and Assigned(Subscriber) then
      Assert(false, Format('TBoldPublisher.ClearEntry: %s <> %s; index = %d', [fSubscriptionArray[index].Subscriber.ContextString, Subscriber.ContextString, Index]))
    else
      Assert(false, Format('TBoldPublisher.ClearEntry: %s; index = %d', [Subscriber.ContextString, Index]));
  end;
  fSubscriptionArray[index].ClearEntry;
  // attempt to rewind empty slots
  if fSubscriptionCount - 1 = index then
  begin
    Dec(fSubscriptionCount);
    while (fSubscriptionCount <> 0) and (fSubscriptionArray[fSubscriptionCount-1].Subscriber = nil) do
    begin
      Dec(fSubscriptionCount);
      Dec(fHoleCount);
    end;
  end
  else
    Inc(fHoleCount);
  if (fSubscriptionCount-fHoleCount < Length(fSubscriptionArray) div 2) then
    NeedsPacking := True;
end;

procedure TBoldPublisher.AddSmallSubscription(Subscriber: TBoldSubscriber;
    Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
var
  I, index: Integer;
begin
  if not assigned(Subscriber) then
    exit;
  if fSubscriptionCount <= Subscriber.SubscriptionCount then
  begin
    for I := 0 to fSubscriptionCount - 1 do
    begin
      if (fSubscriptionArray[I].Subscriber = Subscriber) and
        (fSubscriptionArray[I].RequestedEvent = RequestedEvent) and
        (fSubscriptionArray[I].isSmallEventSubscription) then
      begin
        fSubscriptionArray[I].ExtendEvents(Events);
        Exit;
      end;
    end;
  end
  else
  begin
    for I := 0 to Subscriber.SubscriptionCount - 1 do
    begin
      if (Subscriber.Subscriptions[I].Publisher = Self) then
      begin
        Index := Subscriber.Subscriptions[I].Index;
        if (fSubscriptionArray[Index].RequestedEvent = RequestedEvent) and
          fSubscriptionArray[Index].isSmallEventSubscription then
        begin
          fSubscriptionArray[Index].ExtendEvents(Events);
          Exit;
        end;
      end;
    end;
  end;
  if Events = [] then
    Raise EBold.CreateFmt('%s.AddSmallSubscription: Events is empty set, probably event is a not small event.', [ClassName]);
  EnsureFreeSpace;
  fSubscriptionArray[fSubscriptionCount].Subscriber := Subscriber;
  fSubscriptionArray[fSubscriptionCount].IndexInSubscriber := Subscriber.SubscriptionCount;
  fSubscriptionArray[fSubscriptionCount].RequestedEvent := RequestedEvent;
  fSubscriptionArray[fSubscriptionCount].MatchCondition := Integer(Events);
  Subscriber.AddToSubscriptions(self, fSubscriptionCount);
  Inc(fSubscriptionCount);
end;

procedure TBoldPublisher.AddSubscription(Subscriber: TBoldSubscriber;
    OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
var
  I, index: Integer;
begin
  if not assigned(Subscriber) then
    exit;
  if (OriginalEvent <= beMaxSmallUser) then
  begin
    AddSmallSubscription(Subscriber, [OriginalEvent], RequestedEvent);
    Exit;
  end;
  if fSubscriptionCount <= Subscriber.SubscriptionCount then
  begin
    for I := 0 to fSubscriptionCount - 1 do
    begin
      if (fSubscriptionArray[I].Subscriber = Subscriber) and
         (fSubscriptionArray[I].RequestedEvent = RequestedEvent) and
         fSubscriptionArray[I].IsMatchingEvent(OriginalEvent) then
        Exit;
    end;
  end
  else
  begin
    for I := 0 to Subscriber.SubscriptionCount - 1 do
    begin
      if (Subscriber.Subscriptions[I].Publisher = Self) then
      begin
        Index := Subscriber.Subscriptions[I].Index;
        if (fSubscriptionArray[Index].RequestedEvent = RequestedEvent) and
          fSubscriptionArray[Index].IsMatchingEvent(OriginalEvent) then
          Exit;
      end;
    end;
  end;
  EnsureFreeSpace;
  fSubscriptionArray[fSubscriptionCount].Subscriber := Subscriber;
  fSubscriptionArray[fSubscriptionCount].IndexInSubscriber := Subscriber.SubscriptionCount;
  fSubscriptionArray[fSubscriptionCount].RequestedEvent := RequestedEvent;
  fSubscriptionArray[fSubscriptionCount].MatchCondition :=  OriginalEvent or beBigEventFlag;
  Subscriber.AddToSubscriptions(self, fSubscriptionCount);
  Inc(fSubscriptionCount);
end;

procedure TBoldPublisher.SendExtendedEvent(Originator: TObject;
                                           OriginalEvent: TBoldEvent;
                                           const Args: array of const);
var
  I: Integer;
  Subscriber: TBoldSubscriber;
begin
  if fSubscriptionCount = 0 then
    Exit;
  Inc(_SendExtendedEvent);
  StartNotify;
  for I := 0 to fSubscriptionCount - 1 do
  begin
    Subscriber := fSubscriptionArray[I].Subscriber;
    if Assigned(Subscriber) and fSubscriptionArray[I].IsMatchingEvent(OriginalEvent) then
    begin
      if Subscriber.HandlesExtendedEvents then
        Subscriber.ReceiveExtended(Originator, OriginalEvent, fSubscriptionArray[I].RequestedEvent, Args)
      else
        Subscriber.Receive(Originator, OriginalEvent, fSubscriptionArray[I].RequestedEvent);
      Inc(_SendEventMatch);
    end;
  end;
  EndNotify;
end;

{$IFNDEF BOLD_NO_QUERIES}
function TBoldPublisher.SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
var
  I: Integer;
begin
  if fSubscriptionCount > 0 then
  begin
    Inc(_SendQuery);
    StartNotify;
    for I := 0 to fSubscriptionCount - 1 do
    begin
      if Assigned( fSubscriptionArray[I].Subscriber) and fSubscriptionArray[I].IsMatchingEvent(OriginalEvent) then
        if not  fSubscriptionArray[I].Subscriber.Answer(Originator, OriginalEvent, fSubscriptionArray[I].RequestedEvent, Args, Subscriber) then
        begin
          Inc(_QueryMatch);
          result := false;
          Exit;
        end;
    end;
    EndNotify;
  end;
  result := true;
end;
{$ENDIF}

procedure TBoldPublisher.NotifySubscribersAndClearSubscriptions(Originator: TObject);
var
  I: Integer;
begin
  if fSubscriptionCount = 0 then
    Exit;
  SendExtendedEvent(Originator, beDestroying, []);
  for I := 0 to fSubscriptionCount - 1 do
    fSubscriptionArray[I].ClearEntry;
  SetLength(fSubscriptionArray, 0);
  fSubscriptionCount := 0;
end;

constructor TBoldPublisher.Create;
begin
  Inc(PublisherCount);
end;

destructor TBoldPublisher.Destroy;
begin
  Assert(fSubscriptionCount = 0);
  if NeedsPacking then
    RemoveFromPostNotificationQueue(self);
  Dec(PublisherCount);
end;

procedure TBoldSubscriber.CancelAllSubscriptions;
var
  I: Integer;
  Publisher: TBoldPublisher;
begin
  if fSubscriptionCount = 0 then
    Exit;
  TBoldPublisher.StartNotify;
  try
    for I := 0 to fSubscriptionCount - 1 do
    begin
      Publisher := fSubscriptionArray[I].Publisher;
      if Assigned(Publisher) then
        Publisher.ClearEntry(self, fSubscriptionArray[I].Index);
    end;
    fSubscriptionCount := 0; {remove entires but retain size}
  finally
    TBoldPublisher.EndNotify;
  end;
end;

{---TBoldPassthroughSubscriber---}

constructor TBoldPassthroughSubscriber.Create(AReceiveFunc: TBoldEventHandler);
begin
  inherited Create;
  fReceiveFunc := AReceiveFunc;
end;

function TBoldPassthroughSubscriber.GetContextString: string;
var
  vObject: TObject;
begin
  vObject := TObject(TMethod(fReceiveFunc).Data);
{$IFNDEF BOLD_DISABLEMEMORYMANAGER}
  if vObject is TBoldMemoryManagedObject then
    result := TBoldMemoryManagedObject(vObject).DebugInfo
  else
{$ENDIF}
  if vObject is TComponent then
    result := TComponent(vObject).Name
  else
    Result := vObject.ClassName;
end;

function TBoldPassthroughSubscriber.GetHandlesExtendedEvents: Boolean;
begin
  Result := false;
end;

procedure TBoldPassthroughSubscriber.Receive(Originator: TObject; OriginalEvent: TBoldEvent;
    RequestedEvent: TBoldRequestedEvent);
begin
  if Assigned(fReceiveFunc) then
    fReceiveFunc(Originator, OriginalEvent, RequestedEvent);
end;

{---TBoldExtendedPassthroughSubscriber---}

{$IFNDEF BOLD_NO_QUERIES}
constructor TBoldExtendedPassthroughSubscriber.CreateWithReceiveAndAnswer(AReceiveFunc: TBoldEventHandler;
                                                                  AAnswerFunc: TBoldQueryHandler);
begin
  inherited Create(nil);
  ReceiveFunc := AReceiveFunc;
  fAnswerFunc := AAnswerFunc;
end;

function TBoldExtendedPassthroughSubscriber.Answer(Originator: TObject; OriginalEvent: TBoldEvent;
    RequestedEvent: TBoldRequestedEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  if Assigned(fAnswerFunc) then
    result := fAnswerFunc(Originator, OriginalEvent, RequestedEvent, Args, Subscriber)
  else
    result := true;
end;
{$ENDIF}

{--- TBoldSubscribableObject ---}

function TBoldSubscribableObject.GetPublisher: TBoldPublisher;
begin
  if not Assigned(fPublisher) then
  begin
    fPublisher := TBoldPublisher.Create;
    fPublisher.SubscribableObject := self;
  end;
  Result := fPublisher
end;

function TBoldSubscribableObject.GetSubscriptionsAsText: string;
begin
  result := Publisher.SubscriptionsAsText;
end;

procedure TBoldSubscribableObject.FreePublisher;
begin
  if Assigned(fPublisher) then
  begin
    fPublisher.NotifySubscribersAndClearSubscriptions(Self);
    FreeAndNil(fPublisher);
  end;
end;

procedure TBoldSubscribableObject.AddSmallSubscription(Subscriber: TBoldSubscriber;
    Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
begin
  Publisher.AddSmallSubscription(Subscriber, Events, RequestedEvent);
end;

procedure TBoldSubscribableObject.AddSubscription(Subscriber: TBoldSubscriber;
    OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Publisher.AddSubscription(Subscriber, OriginalEvent, RequestedEvent);
end;

procedure TBoldSubscribableObject.SendEvent(OriginalEvent: TBoldEvent);
begin
  if Assigned(fPublisher) then
    fPublisher.SendExtendedEvent(self, OriginalEvent, []);
end;

destructor TBoldSubscribableObject.Destroy;
begin
  FreePublisher;
end;

{$IFNDEF BOLD_NO_QUERIES}
function TBoldSubscribableObject.SendQuery(OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber; Originator: TObject = nil): Boolean;
begin
  if Assigned(Originator) then
    result := not Assigned(fPublisher) or fPublisher.SendQuery(Originator, OriginalEvent, Args, Subscriber)
  else
    result := not Assigned(fPublisher) or fPublisher.SendQuery(self, OriginalEvent, Args, Subscriber);
end;
{$ENDIF}

function TBoldSubscribableObject.GetContextString: string;
begin
  result := ClassName;
end;

function TBoldSubscribableObject.GetDebugInfo: string;
begin
  result := ContextString;
end;

function TBoldSubscribableObject.GetHasSubscribers: Boolean;
begin
  result := assigned(fPublisher) and Publisher.HasSubscribers;
end;

{--- TBoldSubscribableComponent ---}

function TBoldSubscribableComponent.GetPublisher: TBoldPublisher;
begin
  if not assigned(fPublisher) then
    fPublisher := TBoldPublisher.Create;
  result := fPublisher;
end;

function TBoldSubscribableComponent.GetSubscriptionsAsText: string;
begin
  result := Publisher.SubscriptionsAsText;
end;

function TBoldSubscribableComponent.GetHasSubscribers: Boolean;
begin
  result := assigned(fPublisher) and Publisher.HasSubscribers;
end;

procedure TBoldSubscribableComponent.FreePublisher;
begin
  if Assigned(fPublisher) then
  begin
    fPublisher.NotifySubscribersAndClearSubscriptions(Self);
    FreeAndNil(fPublisher);
  end;
end;

destructor TBoldSubscribableComponent.Destroy;
begin
  FreePublisher;
  inherited;
end;

procedure TBoldSubscribableComponent.AddSmallSubscription(Subscriber: TBoldSubscriber;
    Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
begin
  Publisher.AddSmallSubscription(Subscriber, Events, RequestedEvent);
end;

procedure TBoldSubscribableComponent.AddSubscription(Subscriber: TBoldSubscriber;
    OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Publisher.AddSubscription(Subscriber, OriginalEvent, RequestedEvent);
end;

procedure TBoldSubscribableComponent.SendEvent(Originator: TObject; OriginalEvent: TBoldEvent);
begin
  if Assigned(fPublisher) then
    fPublisher.SendExtendedEvent(Originator, OriginalEvent, []);
end;

{$IFNDEF BOLD_NO_QUERIES}
function TBoldSubscribableComponent.SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  result := not Assigned(fPublisher) or fPublisher.SendQuery(Originator, OriginalEvent, Args, Subscriber);
end;
{$ENDIF}

{---TBoldSubscribablePersistent---}

function TBoldSubscribablePersistent.GetPublisher: TBoldPublisher;
begin
  if not assigned(fPublisher) then
    fPublisher := TBoldPublisher.Create;
  result := fPublisher;
end;

function TBoldSubscribablePersistent.GetSubscriptionsAsText: string;
begin
  if assigned(fPublisher) then
    result := fPublisher.SubscriptionsAsText
  else
    result := '';
end;

function TBoldSubscribablePersistent.GetHasSubscribers: Boolean;
begin
  result := assigned(fPublisher) and Publisher.HasSubscribers;
end;

procedure TBoldSubscribablePersistent.FreePublisher;
begin
  if Assigned(fPublisher) then
  begin
    fPublisher.NotifySubscribersAndClearSubscriptions(Self);
    FreeAndNil(fPublisher);
  end;
end;

destructor TBoldSubscribablePersistent.Destroy;
begin
  FreePublisher;
  inherited;
end;

procedure TBoldSubscribablePersistent.AddSmallSubscription(Subscriber: TBoldSubscriber;
    Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
begin
  Publisher.AddSmallSubscription(Subscriber, Events, RequestedEvent);
end;

procedure TBoldSubscribablePersistent.AddSubscription(Subscriber: TBoldSubscriber;
    OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Publisher.AddSubscription(Subscriber, OriginalEvent, RequestedEvent);
end;

procedure TBoldSubscribablePersistent.SendEvent(Originator: TObject; OriginalEvent: TBoldEvent);
begin
  if Assigned(fPublisher) then
    fPublisher.SendExtendedEvent(Originator, OriginalEvent, []);
end;

{$IFNDEF BOLD_NO_QUERIES}
function TBoldSubscribablePersistent.SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  result := not Assigned(fPublisher) or fPublisher.SendQuery(Originator, OriginalEvent, Args, Subscriber);
end;

function TBoldSubscriber.Answer(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  raise EBold.CreateFmt('%s.Answer: You have subscribed to a query without implementing the virtual Answer method... (triggered by: %s)', [classname, Originator.Classname]);
end;
{$ENDIF}

function TBoldSubscriber.GetContextString: string;
begin
  Result := ClassName;
end;

function TBoldSubscriber.GetDebugInfo: string;
begin
  result := ContextString;
end;

procedure TBoldSubscriber.ReceiveExtended(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
end;

procedure TBoldExtendedPassthroughSubscriber.ReceiveExtended(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  if Assigned(fExtendedReceiveFunc) then
    fExtendedReceiveFunc(Originator, OriginalEvent, RequestedEvent, Args);
end;

function TBoldSubscriber.GetHandlesExtendedEvents: Boolean;
begin
  result := false;
end;

function TBoldSubscriber.GetSubscriptionsAsText: string;
var
  i,j: integer;
begin
  result := '';
  j := 0;
  for I := 0 to Length(fSubscriptionArray) - 1 do
  if Assigned(fSubscriptionArray[i].Publisher) and Assigned(fSubscriptionArray[i].Publisher.SubscribableObject) then
  begin
    result := result + IntToStr(j) + ':' + fSubscriptionArray[i].Publisher.SubscribableObject.ContextString + #13#10;
    inc(j);
  end;
end;

function TBoldSubscriber.HasMatchingSubscription(
  APublisher: TBoldPublisher): boolean;
begin
  result := APublisher.HasMatchingSubscription(self);
end;

procedure TBoldSubscriber.CancelSubscriptionTo(APublisher: TBoldPublisher);
begin
  APublisher.CancelSubscriptionTo(self);
end;

constructor TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(
  AExtendedReceiveFunc: TBoldExtendedEventHandler);
begin
  inherited Create(nil);
  fExtendedReceiveFunc := AExtendedReceiveFunc;
end;

function TBoldExtendedPassthroughSubscriber.GetHandlesExtendedEvents: Boolean;
begin
  result := assigned(fExtendedReceiveFunc);
end;

procedure TBoldSubscribableObject.SendExtendedEvent(OriginalEvent: TBoldEvent;
                                                    const Args: array of const);
begin
  if Assigned(fPublisher) then
    fPublisher.SendExtendedEvent(self, OriginalEvent, Args);
end;

procedure TBoldSubscribableComponent.SendExtendedEvent(Originator: TObject;
                                                       OriginalEvent: TBoldEvent;
                                                       const Args: array of const);
begin
  if Assigned(fPublisher) then
    Publisher.SendExtendedEvent(Originator, OriginalEvent, args);
end;

procedure TBoldSubscribablePersistent.SendExtendedEvent(Originator: TObject;
                                                        OriginalEvent: TBoldEvent;
                                                        const Args: array of const);
begin
  Publisher.SendExtendedEvent(Originator, OriginalEvent, Args);
end;

procedure TBoldSubscriber.AfterConstruction;
begin
  inherited;
  Inc(SubscriberCount);
end;

procedure TBoldSubscriber.BeforeDestruction;
begin
  Dec(SubscriberCount);
  CancelAllSubscriptions;
  inherited;
end;

class procedure TBoldPublisher.DelayTillAfterNotification(
  Event: TNotifyEvent; Sender: TObject; Receiver: TObject);
begin
  if G_NotificationNesting = 0 then
    Event(Sender)
  else if Assigned(G_PostNotifyQueue) then
    G_PostNotifyQueue.Add(Event, Sender, Receiver)
  else
    Raise EBold.CreateFmt('%s.DelayTillAfterNotification: Queue not allocated', [ClassName]);
end;

procedure TBoldPublisher.PackSubscriptions(dummy: TObject);
var
  OldCount, i, Gap: integer;
begin
  OldCount := fSubscriptionCount;
  Gap := 0;
  for i := 0 to OldCount-1 do
  begin
    if FSubscriptionArray[i].Subscriber = nil then
    begin
      Inc(Gap);
    end
    else if gap > 0 then
    begin
      Assert((fSubscriptionArray[i].Subscriber.Subscriptions[fSubscriptionArray[i].IndexInSubscriber].Publisher = Self) and (fSubscriptionArray[i].Subscriber.Subscriptions[fSubscriptionArray[i].IndexInSubscriber].Index = i));
      fSubscriptionArray[i].ReIndexsSubscriber(i-gap);
      fSubscriptionArray[i-gap] := fSubscriptionArray[i];
      fSubscriptionArray[i].Subscriber := nil;
    end;
  end;
  fSubscriptionCount := fSubscriptionCount-Gap;
  if fSubscriptionCount = 0 then
    SetLength(FSubscriptionArray, 0)
  else
  // do not bother if it's less than 9 records
  if (Length(FSubscriptionArray) > 8) and (fSubscriptionCount < Length(FSubscriptionArray) div 2) then
    SetLength(FSubscriptionArray, fSubscriptionCount);
  NeedsPacking := false;
  fHoleCount := 0;
end;

class procedure TBoldPublisher.RemoveFromPostNotificationQueue(Receiver: TObject);
begin
  if Assigned(G_PostNotifyQueue) then
    G_PostNotifyQueue.RemoveAllForReceiver(Receiver);
end;

procedure InitDebugMethods;
begin
  exit; // intentionally do nothing, but code bellow forces compiler to include these debug methods so they can be inspected
  TBoldPublisher.Create.SubscriptionsAsText;
  TBoldPassthroughSubscriber.Create(nil).SubscriptionsAsText;
  TBoldSubscribableObject.Create.SubscriptionsAsText;
  TBoldSubscribablePersistent.Create.SubscriptionsAsText;
  TBoldSubscribableComponent.Create(nil).SubscriptionsAsText;
end;

initialization
  G_PostNotifyQueue := TboldEventQueue.Create;
  InitDebugMethods;

finalization
  FreeAndNil(G_PostNotifyQueue);
  if (DebugHook <> 0) then
    if (ActiveSubscriptionCount + PublisherCount + SubscriberCount) > 0 then
      Assert(false, Format('ActiveSubscriptionCount = %d, PublisherCount = %d, SubscriberCount = %d', [ActiveSubscriptionCount, PublisherCount, SubscriberCount]));
end.
