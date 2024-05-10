unit BoldTestFrameworkUtilities;

interface

uses
  BoldSubscription,
  BoldDomainElement,
  BoldSystemHandle,
  BoldPersistenceHandleDB,
  BoldDatabaseAdapterIB,
  SysUtils,
  Classes;

type
  {forward declarations}
  TLoggingSubscriber = class;

  TLoggingSubscriber = class(TBoldSubscriber)
  private
    FEvents: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent); override;
    procedure SubscribeToElement(Element: TBoldDomainElement);
    procedure ClearEvents;
    procedure VerifyNoEventsReceived(const Element: TBoldDomainElement);
    procedure VerifyEventReceived(const EventType: TBoldEvent; const Element: TBoldDomainElement);
  end;

  {routines}
  function EventTypeToString(const EventType: TBoldEvent): string;

  procedure ForceRegenerateAliasAndSchema(BoldSystemHandle: TBoldSystemHandle);

implementation

uses
  BoldSystem;
  
{ TLoggingSubscriber }

constructor TLoggingSubscriber.Create;
begin
  inherited;
  FEvents := TStringList.Create;
end;

destructor TLoggingSubscriber.Destroy;
begin
  FreeAndNil(FEvents);
  inherited;
end;

procedure TLoggingSubscriber.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
var
  ObjId, ObjectName, MemberName: string;
begin
  if (Originator is TBoldObject) then
  begin
    ObjectName := (Originator as TBoldObject).BoldClassTypeInfo.ExpressionName;
    ObjId := (Originator as TBoldObject).BoldObjectLocator.BoldObjectID.AsString;
    memberName := '';
  end;
  if (Originator is TBoldMember) then
  begin
    ObjectName := (Originator as TBoldMember).OwningObject.BoldClassTypeInfo.ExpressionName;
    ObjId := (Originator as TBoldMember).OwningObject.BoldObjectLocator.BoldObjectID.AsString;
    MemberName := (Originator as TBoldMember).BoldMemberRTInfo.ExpressionName;
  end;
  FEvents.Add(Format('%s%s:%s:%s', [ObjectName, ObjId, MemberName, EventTypeToString(OriginalEvent)]));
end;

procedure TLoggingSubscriber.ClearEvents;
begin
  FEvents.Clear;
end;

procedure TLoggingSubscriber.SubscribeToElement(Element: TBoldDomainElement);
begin
  Element.AddSubscription(self, beValueChanged, beValueChanged);
  Element.AddSubscription(self, beValueInvalid, beValueInvalid);
  Element.AddSubscription(self, beValueIdentityChanged, beValueIdentityChanged);
  Element.AddSubscription(self, beMemberChanged, beMemberChanged);
  Element.AddSubscription(self, beItemAdded, beItemAdded);
  Element.AddSubscription(self, beItemDeleted, beItemDeleted);
  Element.AddSubscription(self, beItemReplaced, beItemReplaced);
  Element.AddSubscription(self, beDirtyListInvalidOrItemDeleted, beDirtyListInvalidOrItemDeleted);
end;

procedure TLoggingSubscriber.VerifyNoEventsReceived(const Element: TBoldDomainElement);
var
  ObjectName, oid, MemberName: string;
  i: integer;
  s: string;
  Found: Boolean;
begin
  Found := false;
  if (Element is TBoldObject) then
  begin
    ObjectName := (Element as TBoldObject).BoldClassTypeInfo.ExpressionName;
    oid := (Element as TBoldObject).BoldObjectLocator.BoldObjectID.AsString;
    memberName := '';
  end;
  if (Element is TBoldMember) then
  begin
    ObjectName := (Element as TBoldMember).OwningObject.BoldClassTypeInfo.ExpressionName;
    oid := (Element as TBoldMember).OwningObject.BoldObjectLocator.BoldObjectID.AsString;
    MemberName := (Element as TBoldMember).BoldMemberRTInfo.ExpressionName;
  end;
  s := Format('%s%s:%s', [ObjectName, oid, MemberName]);
  for i:= 0 to FEvents.Count - 1 do
  begin
    Found := (Pos(s, fEvents[i]) = 0);
    if Found then Break;
  end;
  Assert(Found, 'VerifyNoEventsReceived failed');
end;

procedure TLoggingSubscriber.VerifyEventReceived(const EventType: TBoldEvent;
  const Element: TBoldDomainElement);
var
  ObjectName, oid, MemberName: string;
  s: string;
begin
  if (Element is TBoldObject) then
  begin
    ObjectName := (Element as TBoldObject).BoldClassTypeInfo.ExpressionName;
    oid := (Element as TBoldObject).BoldObjectLocator.BoldObjectID.AsString;
    memberName := '';
  end;
  if (Element is TBoldMember) then
  begin
    ObjectName := (Element as TBoldMember).OwningObject.BoldClassTypeInfo.ExpressionName;
    oid := (Element as TBoldMember).OwningObject.BoldObjectLocator.BoldObjectID.AsString;
    MemberName := (Element as TBoldMember).BoldMemberRTInfo.ExpressionName;
  end;
  s := Format('%s%s:%s:%s', [ObjectName, oid, MemberName, EventTypeToString(EventType)]);
  Assert((fEvents.IndexOf(s) <> -1), Format('VerifyEventReceived %s failed', [s]));
end;

  {routines}
function EventTypeToString(const EventType: TBoldEvent): string;
begin
  if (EventType = beValueChanged) then
    Result := 'beValueChanged'
  else if (EventType = beValueInvalid) then
    Result := 'beValueInvalid'
  else if (EventType = beValueIdentityChanged) then
    Result := 'beValueIdentityChanged'
  else if (EventType = beMemberChanged) then
    Result := 'beMemberChanged'
  else if (EventType = beItemAdded) then
    Result := 'beItemAdded'
  else if (EventType = beItemDeleted) then
    Result := 'beItemDeleted'
  else if (EventType = beItemReplaced) then
    Result := 'beItemReplaced'
  else if (EventType = beDirtyListInvalidOrItemDeleted) then
    Result := 'beDirtyListInvalidOrItemDeleted'
  else
    Result := Format('Missing: Event=%s(Add it to the function EventTypeToString)', [IntToStr(EventType)]);
end;

procedure ForceRegenerateAliasAndSchema(BoldSystemHandle: TBoldSystemHandle);
begin
  (BoldSystemHandle.PersistenceHandle as TBoldPersistenceHandleDB).DatabaseAdapter.CreateDatabase;
  (BoldSystemHandle.PersistenceHandle as TBoldPersistenceHandleDB).CreateDataBaseSchema;
end;

end.
