unit BoldSubscription;

interface

uses
  Classes,
  BoldBase;

const
  beMinSmallReserved = 0;
  beMaxSmallReserved = 23;
  beMinSmallUser = 24;
  beMaxSmallUser = 31;
  beMaxEvent = 32767;
  beMinUser = 4096;
  beMaxUser = beMaxEvent;
  bqMinQuery = beMaxEvent+1;
  bpeMinReserved = 1024;
  beDestroying = 0; {General message for all subscribables}
  beMemberChanged = 2; beObjectDeleted = 3; beObjectCreated = 11; {Object events}
  beItemAdded = 4; beItemDeleted = 5; beItemReplaced = 6; beOrderChanged = 7; {ObjectList events}
  beValueChanged = 8; {value of attribute or ObjectReference changed}
  beValueIdentityChanged = 9;  {Actual identity of anElement.value changed}
  beValueInvalid = 12;
  beDerivedSoonDestroyed = 13;
  beLocatorDestroying = 14;
  beDirtyListInvalidOrItemDeleted = 15;
  beQualifierChanged = 16;
  beDeactivating = 17; // sent by persistencehandles
  beRolledBack = 18; // sent by TBoldSystem
  bePostUpdateId = 19; // send by TBoldObject when the ID is updated to allow regions to rehash themselves
  bePreUpdateId = 20; // send by TBoldObject when the ID is updated to allow regions to rehash themselves
  beObjectFetched = 21; // sent by TBoldObject when it has been recreated in memory
  breReEvaluate = beValueChanged; {backwards compatibility}
  breReSubscribe = 10;

  beServerSubscriberRemoved = 22;

  beValueEvents = [beItemAdded, beItemDeleted, beItemReplaced, beOrderChanged, beValueChanged, beValueInvalid];

  // BoldPersistenceEvents
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

  bePrepareModify = 38;
  beCompleteModify = 39;
  bePrepareDelete = 41;

  bqMayUpdate = bqMinQuery + 1;
  bqMayModify = bqMinQuery + 2;
  bqMayDelete = bqMinQuery + 3;
  bqMayCommit = bqMinQuery + 4;

  bqMaxSubscription = bqMinQuery + 6;

  {Forward declarations of all classes}
type
  TBoldPublisher = class;
  TBoldSubscription = class;
  TBoldSmallEventSubscription = class;
  TBoldSubscriber = class;
  TBoldPassthroughSubscriber = class;
  TBoldSubscribableObject = class;
  TBoldSubscribableComponent = class;
  TBoldSubscribablePersistent = class;

  {TBoldEvent itself is a Smallint. TBoldEventset is a set of of small values (0..31) It is possible
   to subscribe either to one particular event, or to a set of small events. Of the small
   events, 0..23 are reserved for Bold and 24..31 are available for users. Of the larger events,
   32..64 are reserved for the future use of Bold, the rest are available for user programming}

  {TBoldEvent}
  TBoldEvent = Integer;
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

  TBoldSubscriptionList = TList;

  {---TBoldPublisher---}
  TBoldPublisher = class(TBoldMemoryManagedObject)
  private
    fSubscriptions: TBoldSubscriptionList;
    fPublisherFlags: TBoldPublisherFlags;
    property Subscriptions: TBoldSubscriptionList read fSubscriptions;
    function GetHasSubscribers: Boolean;
    procedure AddToSubscriptions(Subscription: TBoldSubscription);  {called by TBoldSubcrition}
    function GetNeedsPacking: Boolean;
    procedure SetNeedsPacking(Value: Boolean);
    class procedure DelayTillAfterNotification(Event: TNotifyEvent; Sender: TObject; Receiver: TObject);
    class procedure RemoveFromPostNotificationQueue(Receiver: TObject);
    procedure PackSubscriptions(dummy: TObject);
    property NeedsPacking: Boolean read GetNeedsPacking write SetNeedsPacking;  {Set by TBoldSubscription}
  protected
    procedure SetPublisherFlag(Flag: TBoldPublisherFlag; Value: Boolean);
    function GetPublisherFlag(Flag: TBoldPublisherFlag): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    class procedure StartNotify;
    class procedure EndNotify;
    procedure NotifySubscribersAndClearSubscriptions(Originator: TObject);
    procedure AddSmallSubscription(Subscriber: TBoldSubscriber;
      Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
    procedure AddSubscription(Subscriber: TBoldSubscriber;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SendExtendedEvent(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const);
   function SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
    property HasSubscribers: Boolean read GetHasSubscribers;
  end;

  {---TBoldSubscription---}
  TBoldSubscription = class(TBoldMemoryManagedObject)
  private
    fPublisher: TBoldPublisher;
    fRequestedEvent: TBoldRequestedEvent;
    fSubscriber: TBoldSubscriber;
  protected
    constructor Create(Publisher: TBoldPublisher;
      Subscriber: TBoldSubscriber; RequestedEvent: TBoldRequestedEvent);
    function IsMatchingEvent(OriginalEvent: TBoldEvent): Boolean; virtual; abstract;
    procedure CloneTo(Subscriber: TBoldSubscriber; NewRequestedEvent: TBoldRequestedEvent); virtual; abstract;
    procedure UnlinkFromSubscriber;
    procedure UnlinkFromPublisher;
    property Publisher: TBoldPublisher read fPublisher;
    property RequestedEvent: TBoldRequestedEvent read fRequestedEvent;
    property Subscriber: TBoldSubscriber read fSubscriber;
  end;

  {---TBoldEventSubscription---}
  TBoldEventSubscription = class(TBoldSubscription)
  private
    fEvent: TBoldEvent;
  protected
    function IsMatchingEvent(OriginalEvent: TBoldEvent): Boolean; override;
    procedure CloneTo(Subscriber: TBoldSubscriber; NewRequestedEvent: TBoldRequestedEvent); override;
  public
    constructor Create(Publisher: TBoldPublisher; Subscriber: TBoldSubscriber;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  end;

  {---TBoldSmallEventSubscription---}
  TBoldSmallEventSubscription = class(TBoldSubscription)
  private
    fEvents: TBoldSmallEventSet;
  protected
    procedure ExtendEvents(Events: TBoldSmallEventSet);
    function IsMatchingEvent(OriginalEvent: TBoldEvent): Boolean; override;
    procedure CloneTo(Subscriber: TBoldSubscriber; NewRequestedEvent: TBoldRequestedEvent); override;
  public
    constructor Create(Publisher: TBoldPublisher; Subscriber: TBoldSubscriber;
      Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
  end;

  {---TBoldSubscriber---}
  TBoldSubscriber = class(TBoldMemoryManagedObject)
  private
    fSubscriptions: TList;
    function GetSubscriptions: TList;
    property Subscriptions: TList read GetSubscriptions;
    procedure AddToSubscriptions(Subscription: TBoldSubscription); {called by TBoldSubcrition}
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent); virtual; abstract;
    procedure ReceiveExtended(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const); virtual;
    function Answer(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; virtual;
    function GetContextString: string; virtual;
    function GetHandlesExtendedEvents: Boolean; virtual;
  public
    destructor Destroy; override;
    procedure CancelAllSubscriptions;
    procedure CloneSubscriptions(Subscriber: TBoldSubscriber; OldRequestedEvent: TBoldRequestedEvent; NewRequestedEvent: TBoldRequestedEvent);
    property ContextString: string read GetContextString;
    property HandlesExtendedEvents: Boolean read GetHandlesExtendedEvents;
  end;

  {---TBoldPassthroughSubscriber---}
  TBoldPassthroughSubscriber = class(TBoldSubscriber)
  private
    fReceiveFunc: TBoldEventHandler;
    fExtendedReceiveFunc: TBoldExtendedEventHandler;
    fAnswerFunc: TBoldQueryHandler;
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent); override;
    procedure ReceiveExtended(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    function Answer(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; override;
    function GetHandlesExtendedEvents: Boolean; override;
  public
    constructor Create(receiveFunc: TBoldEventHandler);
    constructor CreateWithExtendedReceive(ExtendedReceiveFunc: TBoldExtendedEventHandler);
    constructor CreateWithReceiveAndAnswer(ReceiveFunc: TBoldEventHandler;
                                           AnswerFunc: TBoldQueryHandler);
    property receiveFunc: TBoldEventHandler read fReceiveFunc write fReceiveFunc;
  end;

  {---TBoldSubscribableObject---}
  TBoldSubscribableObject = class(TBoldFlaggedObject)
  private
    fPublisher: TBoldPublisher;
    function GetHasSubscribers: Boolean;
    function GetPublisher: TBoldPublisher;
  protected
    procedure FreePublisher;
    property HasSubscribers: Boolean read GetHasSubscribers;
    property Publisher: TBoldPublisher read GetPublisher;
  public
    destructor Destroy; override;
    procedure AddSmallSubscription(Subscriber: TBoldSubscriber;
      Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
    procedure AddSubscription(Subscriber: TBoldSubscriber;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SendEvent(OriginalEvent: TBoldEvent); virtual;
    procedure SendExtendedEvent(OriginalEvent: TBoldEvent; const Args: array of const); virtual;
    function SendQuery(OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; virtual;
  end;

  {---TBoldSubscribableComponent---}
  TBoldSubscribableComponent = class(TComponent)
  private
    fPublisher: TBoldPublisher;
    function GetPublisher: TBoldPublisher;
  protected
    function GetHasSubscribers: Boolean; virtual;
    property Publisher: TBoldPublisher read GetPublisher;
    procedure FreePublisher;
  public
    destructor Destroy; override;
    procedure AddSmallSubscription(Subscriber: TBoldSubscriber;
      Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
    procedure AddSubscription(Subscriber: TBoldSubscriber;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SendEvent(Originator: TObject; OriginalEvent: TBoldEvent);
    procedure SendExtendedEvent(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const);
    function SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
    property HasSubscribers: Boolean read GetHasSubscribers;
  end;

  {--- TBoldSubscribablePersistent ---}
  TBoldSubscribablePersistent = class(TPersistent)
  private
    fPublisher: TBoldPublisher;
    function GetPublisher: TBoldPublisher;
  protected
    function GetHasSubscribers: Boolean; virtual;
    property Publisher: TBoldPublisher read GetPublisher;
    procedure FreePublisher;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSmallSubscription(Subscriber: TBoldSubscriber;
      Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
    procedure AddSubscription(Subscriber: TBoldSubscriber;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SendEvent(Originator: TObject; OriginalEvent: TBoldEvent);
    procedure SendExtendedEvent(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const);
    function SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
    property HasSubscribers: Boolean read GetHasSubscribers;
  end;

  procedure BoldForcedDequeuePostNotify;
  procedure BoldAddEventToPostNotifyQueue(Event: TNotifyEvent; Sender: TObject; Receiver: TObject);

implementation

uses
  SysUtils,
  BoldDefs,
  BoldEventQueue,
  BoldCommonConst;

var
  G_NotificationNesting: integer = 0;
  G_PostNotifyQueue: TboldEventQueue = nil;
  G_InPostNotification: Boolean = false;

procedure BoldForcedDequeuePostNotify;
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

{---TBoldPublisher---}

constructor TBoldPublisher.Create;
begin
  inherited;
  fSubscriptions := TBoldSubscriptionList.Create;
end;

destructor TBoldPublisher.Destroy;
begin
  Assert(not Assigned(fSubscriptions));
  if NeedsPacking then
    RemoveFromPostNotificationQueue(self);
  inherited;
end;

procedure TBoldPublisher.NotifySubscribersAndClearSubscriptions(Originator: TObject);
var
  I: Integer;
begin
  if not Assigned(fSubscriptions) then
    Exit;
  SendExtendedEvent(Originator, beDestroying, []);
  for I := 0 to Subscriptions.Count - 1 do
    TBoldSubscription(Subscriptions[I]).UnlinkFromPublisher;
  FreeAndNil(fSubscriptions);
end;

procedure TBoldPublisher.SendExtendedEvent(Originator: TObject;
                                           OriginalEvent: TBoldEvent;
                                           const Args: array of const);
var
  I: Integer;
  Subscription: TBoldSubscription;
begin
  if Assigned(Subscriptions) then
  begin
    StartNotify;
    for I := 0 to Subscriptions.Count - 1 do
    begin
      Subscription := TBoldSubscription(fSubscriptions.Items[I]);
      if Assigned(Subscription.Subscriber) and Subscription.IsMatchingEvent(OriginalEvent) then
        if Subscription.Subscriber.HandlesExtendedEvents then
          Subscription.Subscriber.ReceiveExtended(Originator, OriginalEvent, Subscription.RequestedEvent, Args)
        else
          Subscription.Subscriber.Receive(Originator, OriginalEvent, Subscription.RequestedEvent);
    end;
    EndNotify;
  end;
end;

procedure TBoldPublisher.AddToSubscriptions(Subscription: TBoldSubscription);
begin
  Subscriptions.Add(Subscription);
end;

procedure TBoldPublisher.AddSmallSubscription(Subscriber: TBoldSubscriber;
    Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
var
  I: Integer;
  Subscription: TBoldSubscription;
  LocalSubscriptions: TBoldSubscriptionList;
begin
  if not assigned(Subscriber) then
    exit;
  if Subscriptions.Count < Subscriber.Subscriptions.Count then
  begin
    localSubscriptions := Subscriptions;
    for I := 0 to localSubscriptions.Count - 1 do
    begin
      Subscription := TBoldSubscription(LocalSubscriptions.Items[I]);
      if (Subscription.Subscriber = Subscriber) and
        (Subscription.RequestedEvent = RequestedEvent) and
        (Subscription is TBoldSmallEventSubscription) then
      begin
        TBoldSmallEventSubscription(Subscription).ExtendEvents(Events);
        Exit;
      end;
    end;
  end
  else
   begin
    localSubscriptions := Subscriber.Subscriptions;
    for I := 0 to localSubscriptions.Count - 1 do
    begin
      Subscription := TBoldSubscription(LocalSubscriptions.Items[I]);
      if (Subscription.Publisher = Self) and
        (Subscription.RequestedEvent = RequestedEvent) and
        (Subscription is TBoldSmallEventSubscription) then
      begin
        TBoldSmallEventSubscription(Subscription).ExtendEvents(Events);
        Exit;
      end;
    end;
  end;
  TBoldSmallEventSubscription.Create(Self, Subscriber, Events, RequestedEvent);
end;

procedure TBoldPublisher.AddSubscription(Subscriber: TBoldSubscriber;
    OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
var
  I: Integer;
  Subscription: TBoldSubscription;
  LocalSubscriptions: TBoldSubscriptionList;
begin
  if not assigned(Subscriber) then
    exit;
  if Subscriptions.Count < Subscriber.Subscriptions.Count then
  begin
    LocalSubscriptions := Subscriptions;
    for I := 0 to LocalSubscriptions.Count - 1 do
    begin
      Subscription := TBoldSubscription(LocalSubscriptions.Items[I]);
      if (Subscription.Subscriber = Subscriber) and
         (Subscription.RequestedEvent = RequestedEvent) and
         Subscription.IsMatchingEvent(OriginalEvent) then
        Exit;
    end;
  end
  else
  begin
    LocalSubscriptions := Subscriber.Subscriptions;
    for I := 0 to localSubscriptions.Count - 1 do
    begin
      Subscription := TBoldSubscription(localSubscriptions.Items[I]);
      if (Subscription.Publisher = Self) and
         (Subscription.RequestedEvent = RequestedEvent) and
         Subscription.IsMatchingEvent(OriginalEvent) then
        Exit;
    end;
  end;
  TBoldEventSubscription.Create(Self, Subscriber, OriginalEvent, RequestedEvent);
end;

function TBoldPublisher.SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
var
  I: Integer;
  Subscription: TBoldSubscription;
begin
  if Assigned(Subscriptions) then
  begin
    StartNotify;
    for I := 0 to Subscriptions.Count - 1 do
    begin
      Subscription := TBoldSubscription(Subscriptions.Items[I]);
      if Assigned(Subscription.Subscriber) and Subscription.IsMatchingEvent(OriginalEvent) then
        if not Subscription.Subscriber.Answer(Originator, OriginalEvent, Subscription.RequestedEvent, Args, Subscriber) then
        begin
          result := false;
          Exit;
        end;
    end;
    EndNotify;
  end;
  result := true;
end;

function TBoldPublisher.GetHasSubscribers: Boolean;
begin
  result := assigned(fSubscriptions) and (Subscriptions.Count > 0);
end;

{---TBoldSubscription---}

constructor TBoldSubscription.Create(Publisher: TBoldPublisher; Subscriber: TBoldSubscriber; RequestedEvent: TBoldRequestedEvent);
begin
  inherited Create;
  fPublisher := Publisher;
  fSubscriber := Subscriber;
  fRequestedEvent := RequestedEvent;
  Publisher.AddToSubscriptions(Self);
  Subscriber.AddToSubscriptions(Self);
end;

procedure TBoldSubscription.UnlinkFromSubscriber;
begin
  fSubscriber := nil;
  if Assigned(Publisher) then
    Publisher.NeedsPacking := True
  else
    Free; {commit suicide when both links gone}
end;

procedure TBoldSubscription.UnlinkFromPublisher;
begin
  fPublisher := nil;
  if not Assigned(Subscriber) then
    Free; {commit suicide when both links gone}
end;

{---TBoldEventSubscription---}

procedure TBoldEventSubscription.CloneTo(Subscriber: TBoldSubscriber;
  NewRequestedEvent: TBoldRequestedEvent);
begin
  inherited;
  Publisher.AddSubscription(Subscriber,fEvent, NewRequestedEvent);
end;

constructor TBoldEventSubscription.Create(Publisher: TBoldPublisher;
    Subscriber: TBoldSubscriber; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  inherited Create(Publisher, Subscriber, RequestedEvent);
  fEvent := OriginalEvent;
end;

function TBoldEventSubscription.IsMatchingEvent(OriginalEvent: TBoldEvent): Boolean;
begin
  Result := OriginalEvent = fEvent;
end;

{---TBoldSmallEventSubscription---}

procedure TBoldSmallEventSubscription.CloneTo(Subscriber: TBoldSubscriber; NewRequestedEvent: TBoldRequestedEvent);
begin
  inherited;
  Publisher.AddSmallSubscription(Subscriber,fEvents, NewRequestedEvent);
end;

constructor TBoldSmallEventSubscription.Create(Publisher: TBoldPublisher;
    Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
begin
  inherited Create(Publisher, Subscriber, RequestedEvent);
  fEvents := Events;
end;

procedure TBoldSmallEventSubscription.ExtendEvents(Events: TBoldSmallEventSet);
begin
  fEvents := fEvents + Events;
end;

function TBoldSmallEventSubscription.IsMatchingEvent(OriginalEvent: TBoldEvent): Boolean;
begin
  Result := (OriginalEvent < 32) and (OriginalEvent in fEvents);
end;

{---TBoldSubscriber---}

destructor TBoldSubscriber.Destroy;
begin
  if Assigned(fSubscriptions) then
  begin
    CancelAllSubscriptions;
    FreeAndNil(fSubscriptions);
  end;
  inherited Destroy;
end;

function TBoldSubscriber.GetSubscriptions: TList;
begin
  if not Assigned(fSubscriptions) then
    fSubscriptions := TList.Create;
  Result := fSubscriptions;
end;

procedure TBoldSubscriber.AddToSubscriptions(Subscription: TBoldSubscription);
begin
  Subscriptions.Add(Subscription);
end;

procedure TBoldSubscriber.CancelAllSubscriptions;
var
  I: Integer;
begin
  if Subscriptions.Count = 0 then
    Exit;
  TBoldPublisher.StartNotify;
  try
    for I := 0 to Subscriptions.Count - 1 do
      TBoldSubscription(Subscriptions[I]).UnlinkFromSubscriber;
    fSubscriptions.Count := 0; {remove entires but retain size}
  finally
    TBoldPublisher.EndNotify;
  end;
end;

{---TBoldPassthroughSubscriber---}

constructor TBoldPassthroughSubscriber.Create(receiveFunc: TBoldEventHandler);
begin
  inherited Create;
  fReceiveFunc := receiveFunc;
end;

constructor TBoldPassthroughSubscriber.CreateWithReceiveAndAnswer(ReceiveFunc: TBoldEventHandler;
                                                                  AnswerFunc: TBoldQueryHandler);
begin
  inherited Create;
  fReceiveFunc := ReceiveFunc;
  fAnswerFunc := AnswerFunc;
end;

procedure TBoldPassthroughSubscriber.Receive(Originator: TObject; OriginalEvent: TBoldEvent;
    RequestedEvent: TBoldRequestedEvent);
begin
  if Assigned(fReceiveFunc) then
    fReceiveFunc(Originator, OriginalEvent, RequestedEvent);
end;

function TBoldPassthroughSubscriber.Answer(Originator: TObject; OriginalEvent: TBoldEvent;
    RequestedEvent: TBoldRequestedEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  if Assigned(fAnswerFunc) then
    result := fAnswerFunc(Originator, OriginalEvent, RequestedEvent, Args, Subscriber)
  else
    result := true;
end;

{--- TBoldSubscribableObject ---}

function TBoldSubscribableObject.GetPublisher: TBoldPublisher;
begin
  if not Assigned(fPublisher) then
    fPublisher := TBoldPublisher.Create;
  Result := fPublisher
end;

procedure TBoldSubscribableObject.FreePublisher;
begin
  if Assigned(fPublisher) then
  begin
    fPublisher.NotifySubscribersAndClearSubscriptions(Self);
    FreeAndNil(fPublisher);
  end;
end;

destructor TBoldSubscribableObject.Destroy;
begin
  FreePublisher;
  inherited;
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

function TBoldSubscribableObject.SendQuery(OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  result := not Assigned(fPublisher) or fPublisher.SendQuery(self, OriginalEvent, Args, Subscriber);
end;

function TBoldSubscribableObject.GetHasSubscribers: Boolean;
begin
  result := assigned(fPublisher) and Publisher.HasSubscribers;
end;

{--- TBoldSubscribableComponent ---}

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

function TBoldSubscribableComponent.SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  result := not Assigned(fPublisher) or fPublisher.SendQuery(Originator, OriginalEvent, Args, Subscriber);
end;

{---TBoldSubscribablePersistent---}

constructor TBoldSubscribablePersistent.Create;
begin
  inherited;
  fPublisher := TBoldPublisher.Create;
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

function TBoldSubscribablePersistent.SendQuery(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  result := not Assigned(fPublisher) or fPublisher.SendQuery(Originator, OriginalEvent, Args, Subscriber);
end;

function TBoldSubscriber.Answer(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  raise EBold.CreateFmt(sAnswerNotImplemented, [classname, Originator.Classname]);
end;

function TBoldSubscriber.GetContextString: string;
begin
  Result := '';
end;

procedure TBoldSubscriber.ReceiveExtended(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent;
  const Args: array of const);
begin
  // do nothing
end;

procedure TBoldPassthroughSubscriber.ReceiveExtended(Originator: TObject;
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

constructor TBoldPassthroughSubscriber.CreateWithExtendedReceive(
  ExtendedReceiveFunc: TBoldExtendedEventHandler);
begin
  fExtendedReceiveFunc := ExtendedReceiveFunc;
end;

function TBoldPassthroughSubscriber.GetHandlesExtendedEvents: Boolean;
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

function TBoldPublisher.GetPublisherFlag(Flag: TBoldPublisherFlag): Boolean;
begin
  result := Flag in fPublisherFlags;
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
  if not NeedsPacking then
  begin
    SetPublisherFlag(bpfNeedsPacking, Value);
    // the postnotify queue will be gone if we are in finalization,
    // but then we don't need to pack since we will be destroyed soon anyway...
    if assigned(G_PostNotifyQueue) then
      DelayTillAfterNotification(PackSubscriptions, nil, Self);
  end
  else
    SetPublisherFlag(bpfNeedsPacking, Value);
end;

function TBoldSubscribablePersistent.GetHasSubscribers: Boolean;
begin
  result := assigned(fPublisher) and Publisher.HasSubscribers;
end;

function TBoldSubscribableComponent.GetHasSubscribers: Boolean;
begin
 result := assigned(fPublisher) and Publisher.HasSubscribers;
end;

procedure TBoldSubscriber.CloneSubscriptions(Subscriber: TBoldSubscriber;
  OldRequestedEvent: TBoldRequestedEvent;
  NewRequestedEvent: TBoldRequestedEvent);
var
  i: integer;
begin
  for i := 0 to Subscriptions.Count - 1 do
    if TBoldSubscription(Subscriptions[i]).RequestedEvent = OldRequestedEvent then
      TBoldSubscription(Subscriptions[i]).CloneTo(Subscriber, newRequestedEvent);
end;

class procedure TBoldPublisher.DelayTillAfterNotification(
  Event: TNotifyEvent; Sender: TObject; Receiver: TObject);
begin
  if G_NotificationNesting = 0 then
    Event(Sender)
  else if Assigned(G_PostNotifyQueue) then
    G_PostNotifyQueue.Add(Event, Sender, Receiver)
  else
    raise EBold.CreateFmt(sQueueNotAllocated, [ClassName]);
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

class procedure TBoldPublisher.StartNotify;
begin
  Inc(G_NotificationNesting);
end;

procedure TBoldPublisher.PackSubscriptions(dummy: TObject);
var
  i: integer;
begin
  for i := Subscriptions.Count - 1 downto 0 do
    if TBoldSubscription(Subscriptions[i]).Subscriber = nil then
    begin
      TBoldSubscription(Subscriptions[i]).UnlinkFromPublisher;
      Subscriptions[i] := Subscriptions.Last;
      Subscriptions.Delete(Subscriptions.Count - 1);
    end;
  NeedsPacking := false;
end;

class procedure TBoldPublisher.RemoveFromPostNotificationQueue(Receiver: TObject);
begin
  if Assigned(G_PostNotifyQueue) then
    G_PostNotifyQueue.RemoveAllForReceiver(Receiver);
end;

function TBoldSubscribableComponent.GetPublisher: TBoldPublisher;
begin
  if not assigned(fPublisher) then
    fPublisher := TBoldPublisher.Create;
  result := fPublisher;
end;

function TBoldSubscribablePersistent.GetPublisher: TBoldPublisher;
begin
  if not assigned(fPublisher) then
    fPublisher := TBoldPublisher.Create;
  result := fPublisher;
end;

initialization
  G_PostNotifyQueue := TboldEventQueue.Create;

finalization
  FreeAndNil(G_PostNotifyQueue);

end.
