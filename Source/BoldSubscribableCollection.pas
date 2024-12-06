{ Global compiler directives }
{$include bold.inc}
unit BoldSubscribableCollection;

interface

uses
  Classes,
  BoldSubscription;

  {Forward declarations of all classes}
type

  TBoldSubscribableCollection = class;

  {--- TBoldSubscribablePersistent ---}
  TBoldSubscribableCollection = class(TCollection)
  private
    fPublisher: TBoldPublisher;
  protected
    function GetHasSubscribers: Boolean; virtual;
    property Publisher: TBoldPublisher read fPublisher;
    procedure FreePublisher;
  public
    constructor Create(ItemClass: TCollectionItemClass); virtual;
    destructor Destroy; override;
    procedure AddSmallSubscription(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
    procedure AddSubscription(Subscriber: TBoldSubscriber; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SendEvent(OriginalEvent: TBoldEvent);
    procedure SendExtendedEvent(OriginalEvent: TBoldEvent; const Args: array of const);
    function SendQuery(OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
    property HasSubscribers: Boolean read GetHasSubscribers;
  end;

  {--- TBoldSubscribableCollectionItem ---}
  TBoldSubscribableCollectionItem = class(TCollectionItem)
  private
    fPublisher: TBoldPublisher;
  protected
    function GetHasSubscribers: Boolean; virtual;
    property Publisher: TBoldPublisher read fPublisher;
    procedure FreePublisher;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure AddSmallSubscription(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
    procedure AddSubscription(Subscriber: TBoldSubscriber; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SendEvent(OriginalEvent: TBoldEvent);
    procedure SendExtendedEvent(OriginalEvent: TBoldEvent; const Args: array of const);
    function SendQuery(OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
    property HasSubscribers: Boolean read GetHasSubscribers;
  end;

implementation

uses
  SysUtils;

{---TBoldSubscribableCollection---}

constructor TBoldSubscribableCollection.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  fPublisher := TBoldPublisher.Create(fPublisher);
end;

procedure TBoldSubscribableCollection.FreePublisher;
begin
  if Assigned(fPublisher) then
  begin
    fPublisher.NotifySubscribersAndClearSubscriptions(Self);
    FreeAndNil(fPublisher);
  end;
end;

destructor TBoldSubscribableCollection.Destroy;
begin
  FreePublisher;
  inherited;
end;

procedure TBoldSubscribableCollection.AddSmallSubscription(Subscriber: TBoldSubscriber;
    Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
begin
  Publisher.AddSmallSubscription(Subscriber, Events, RequestedEvent);
end;

procedure TBoldSubscribableCollection.AddSubscription(Subscriber: TBoldSubscriber;
    OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Publisher.AddSubscription(Subscriber, OriginalEvent, RequestedEvent);
end;

procedure TBoldSubscribableCollection.SendEvent(OriginalEvent: TBoldEvent);
begin
  if Assigned(fPublisher) then
    fPublisher.SendExtendedEvent(Self, OriginalEvent, []);
end;

function TBoldSubscribableCollection.SendQuery(OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  result := not Assigned(fPublisher) or fPublisher.SendQuery(Self, OriginalEvent, Args, Subscriber);
end;

procedure TBoldSubscribableCollection.SendExtendedEvent(
  OriginalEvent: TBoldEvent;
  const Args: array of const);
begin
  Publisher.SendExtendedEvent(Self, OriginalEvent, Args);
end;

function TBoldSubscribableCollection.GetHasSubscribers: Boolean;
begin
  result := assigned(fPublisher) and fPublisher.HasSubscribers;
end;

{---TBoldSubscribableCollectionItem---}

constructor TBoldSubscribableCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  fPublisher := TBoldPublisher.Create(fPublisher);
end;

procedure TBoldSubscribableCollectionItem.FreePublisher;
begin
  if Assigned(fPublisher) then
  begin
    fPublisher.NotifySubscribersAndClearSubscriptions(Self);
    FreeAndNil(fPublisher);
  end;
end;

destructor TBoldSubscribableCollectionItem.Destroy;
begin
  FreePublisher;
  inherited;
end;

procedure TBoldSubscribableCollectionItem.AddSmallSubscription(Subscriber: TBoldSubscriber;
    Events: TBoldSmallEventSet; RequestedEvent: TBoldRequestedEvent);
begin
  Publisher.AddSmallSubscription(Subscriber, Events, RequestedEvent);
end;

procedure TBoldSubscribableCollectionItem.AddSubscription(Subscriber: TBoldSubscriber;
    OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  Publisher.AddSubscription(Subscriber, OriginalEvent, RequestedEvent);
end;

procedure TBoldSubscribableCollectionItem.SendEvent(OriginalEvent: TBoldEvent);
begin
  if Assigned(fPublisher) then
    fPublisher.SendExtendedEvent(Self, OriginalEvent, []);
end;

function TBoldSubscribableCollectionItem.SendQuery(OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean;
begin
  result := not Assigned(fPublisher) or fPublisher.SendQuery(Self, OriginalEvent, Args, Subscriber);
end;

procedure TBoldSubscribableCollectionItem.SendExtendedEvent(
  OriginalEvent: TBoldEvent;
  const Args: array of const);
begin
  Publisher.SendExtendedEvent(Self, OriginalEvent, Args);
end;

function TBoldSubscribableCollectionItem.GetHasSubscribers: Boolean;
begin
  result := assigned(fPublisher) and fPublisher.HasSubscribers;
end;

end.