unit BoldPropagatorUtils;

interface

uses
  BoldSystemHandle,
  BoldComClientHandles,
  BoldSnooperHandle,
  BoldListenerHandle;

function ReConnectToPropagator(
  SystemHandle: TBoldSystemHandle;
  ListenerHandle: TBoldListenerHandle;
  ComConnectionHandle: TBoldComConnectionHandle;
  SnooperHandle: TBoldSnooperHandle): Boolean;

implementation

uses
  Classes,
  BoldDefs,
  BoldSnooper,
  BoldSystemRT,
  BoldId,
  BoldObjectSpaceExternalEvents,
  BoldGuard,
  BoldSystem;

procedure GetActiveObjectSubscriptions(Obj: TBoldObject; Subscriptions: TStringList);
var
  i: integer;
  MemberRTInfo: TBoldMemberRTInfo;
  Id: TBoldObjectId;
begin
  if Obj.BoldPersistent then
  begin
    Id := Obj.BoldObjectLocator.BoldObjectId;
    // Add a subscription for the embedded state changes
    Subscriptions.Add(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, '', '', '', Id));
    // and one subscription for each nonembedded member that is current/modified in memory
    for i := 0 to obj.BoldMemberCount - 1 do
    begin
      MemberRTInfo := obj.BoldClassTypeInfo.AllMembers[i];
      if not MemberRTInfo.IsStoredInObject then
      begin
        if obj.BoldMemberAssigned[i] and
           (obj.BoldMembers[i].BoldPersistenceState in [bvpsCurrent, bvpsmodified]) then
        begin
          Subscriptions.Add(
            TBoldObjectSpaceExternalEvent.EncodeExternalEvent(
              bsNonEmbeddedStateOfObjectChanged, '', MemberRTInfo.ExpressionName, '', Id));
        end;
      end;
    end;
  end;
end;


procedure GetActiveSubscriptions(System: TBoldSystem; Subscriptions: TStringList);
var
  Guard: IBoldGuard;
  Traverser: TBoldLocatorListTraverser;
  i: integer;
  TopSortedClasses: TBoldClassTypeInfoList;
begin
  Guard := TBoldGuard.Create(Traverser);
  Traverser := system.Locators.CreateTraverser;
  // add subscriptions for all objects in memory
  while not Traverser.EndOfList do
  begin
    if assigned(Traverser.locator.BoldObject) then
      GetActiveObjectSubscriptions(Traverser.locator.BoldObject, Subscriptions);
    Traverser.Next;
  end;
  // Add subscriptions to classes (Why is the classlist transient?)
  TopSortedClasses := system.BoldSystemTypeInfo.TopSortedClasses;
  for i := 0 to TopSortedClasses.Count-1 do
    if system.Classes[i].BoldPersistenceState in [bvpsTransient, bvpsCurrent, bvpsmodified] then
      Subscriptions.Add(
        TBoldObjectSpaceExternalEvent.EncodeExternalEvent(
          bsClassChanged, TopSortedClasses[i].ExpressionName, '', '', nil));

end;


procedure SendSubscriptions(Subscriptions: TStringList; Snooper: TBoldSnooper; ClientId: TBoldClientId);
begin
  Snooper.Subscriptions.AddStrings(Subscriptions);
  Snooper.TransmitEvents(ClientId);
end;

function ReConnectToPropagator(
  SystemHandle: TBoldSystemHandle;
  ListenerHandle: TBoldListenerHandle;
  ComConnectionHandle: TBoldComConnectionHandle;
  SnooperHandle: TBoldSnooperHandle): Boolean;
var
  Snooper: TBoldSnooper;
  Guard: IBoldGuard;
  Subscriptions: TStringList;
begin
  result := false;
  Guard := TBoldGuard.Create(Subscriptions);
  // disconnect and reconnect the COM-interface
  ComConnectionHandle.Connected := false;
  ComConnectionHandle.Connected := true;
  if ComConnectionHandle.Connected then
  begin
    ListenerHandle.StartListenerThread;
    if ListenerHandle.Connected then
    begin
      Subscriptions := TStringList.create;
      GetActiveSubscriptions(SystemHandle.System, Subscriptions);
      Snooper := SnooperHandle.PersistenceController as TBoldSnooper;
      SendSubscriptions(Subscriptions, Snooper, ListenerHandle.BoldClientID);
      result := true;
    end;
  end;
end;

end.
