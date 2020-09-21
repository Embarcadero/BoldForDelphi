unit BoldObjectSpaceExternalEvents;

interface

uses
  BoldID,
  BoldDefaultID;

type
  TBoldEventType = Char;
  TBoldExternalEvent = String;
  TBoldObjectSpaceExternalEvent = class;

  TBoldObjectSpaceSubscriptionType = (bsClassChanged, bsEmbeddedStateOfObjectChanged,
                                      bsNonEmbeddedStateOfObjectChanged, bsGotLocks, bsLockLost{, bsDBLock}, bsObjectDeleted);

  TBoldObjectSpaceExternalEvent = class
  private
    class procedure GetID(Event: TBoldExternalEvent; ObjectID: TBoldDefaultID);
    class function GetParameter(Event: tBoldExternalEvent): String;
    class function GetEventType(Event: TBoldExternalEvent): TBoldObjectSpaceSubscriptionType;
  public
    class function EncodeExternalEvent(SubscriptionType: TBoldObjectSpaceSubscriptionType; ClassName,MemberName, LockName: string;
                       ObjectID: TBoldObjectID): String;
    class function DecodeExternalEvent(Event: TBoldExternalEvent;
                                  var Classname, MemberName, LockName: String;
                                  ObjectID: TBoldDefaultID): TBoldObjectSpaceSubscriptionType;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  ValueSpaceConst;

const
  SUBSCRIPTION_DELIMITER_CHAR = ':';
  EXTERNAL_EVENT_CLASSCHANGED = 'C';
  EXTERNAL_EVENT_EMBEDDEDSTATEOFOBJECTCHANGED = 'E';
  EXTERNAL_EVENT_NONEMBEDDEDSTATEOFOBJECTCHANGED = 'I';
  EXTERNAL_EVENT_EMBEDDEDSTATE_OBJECTDELETED = 'D';
  EXTERNAL_EVENT_LOCKLOST = 'L';
  EXTERNAL_EVENT_GOTLOCK = 'GotLocks';
//  EXTERNAL_EVENT_DBLOCK = 'DBLock';

class function TBoldObjectSpaceExternalEvent.GetEventType(Event: TBoldExternalEvent): TBoldObjectSpaceSubscriptionType;
var
  ExternalEvent: char;
begin
  ExternalEvent := ' ';
  if length(Event) > 0 then
    ExternalEvent := Event[1];
  case ExternalEvent of
    EXTERNAL_EVENT_CLASSCHANGED: Result := bsClassChanged;
    EXTERNAL_EVENT_EMBEDDEDSTATEOFOBJECTCHANGED:
    begin
      if GetParameter(Event) = EXTERNAL_EVENT_EMBEDDEDSTATE_OBJECTDELETED then
        result := bsObjectDeleted
      else
        Result := bsEmbeddedStateOfObjectChanged;
    end;
    EXTERNAL_EVENT_NONEMBEDDEDSTATEOFOBJECTCHANGED: Result := bsNonEmbeddedStateOfObjectChanged;
    EXTERNAL_EVENT_LOCKLOST: Result := bsLockLost;
    else
      if Pos(EXTERNAL_EVENT_GOTLOCK, Event) = 1 then
        Result := bsGotLocks
//      else if Pos(EXTERNAL_EVENT_DBLock, Event) = 1 then
//        Result :=  bsDBLock
      else
        raise EBold.CreateFmt(sInvalidEvent, [Event]);
  end
end;

class procedure TBoldObjectSpaceExternalEvent.GetID(Event: TBoldExternalEvent; ObjectID: TBoldDefaultID);
var
  p, IDAsInt: Integer;
  IDAsString: String;
begin
  if assigned(ObjectId) then
  begin
    case GetEventType(Event) of
      bsObjectdeleted, bsEmbeddedStateOfObjectChanged:
      begin
        p := pos(PROPAGATOR_PARAMETER_DELIMITER_CHAR, Event);
        if p <> 0 then
          Delete(Event, p, maxint);
        IDAsString := Copy(Event, 3, MaxInt);
      end;
      bsNonEmbeddedStateOfObjectChanged:
        begin
          Delete(Event, 1, Pos(SUBSCRIPTION_DELIMITER_CHAR, Event));
          Delete(Event, 1, Pos(SUBSCRIPTION_DELIMITER_CHAR, Event));
          IDAsString := Event;
        end;
    end;
    try
      IDAsInt := StrToInt(IDAsString);
      ObjectID.AsInteger := IDAsInt;
    except
      raise EBold.CreateFmt(sInvalidID, [IDAsString]);
    end;
  end;
end;

class function TBoldObjectSpaceExternalEvent.EncodeExternalEvent(SubscriptionType: TBoldObjectSpaceSubscriptionType;
                  ClassName,MemberName, LockName: string; ObjectID: TBoldObjectID): String;
begin
  case SubscriptionType of
    bsClassChanged: Result := EXTERNAL_EVENT_CLASSCHANGED +
                              SUBSCRIPTION_DELIMITER_CHAR +
                              ClassName;
    bsEmbeddedStateOfObjectChanged: Result := EXTERNAL_EVENT_EMBEDDEDSTATEOFOBJECTCHANGED +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              ObjectID.AsString;
    bsNonEmbeddedStateOfObjectChanged: Result := EXTERNAL_EVENT_NONEMBEDDEDSTATEOFOBJECTCHANGED +
                                                  SUBSCRIPTION_DELIMITER_CHAR +
                                                  MemberName +
                                                  SUBSCRIPTION_DELIMITER_CHAR +
                                                  ObjectID.AsString;
    bsLockLost: Result := EXTERNAL_EVENT_LOCKLOST +
                          SUBSCRIPTION_DELIMITER_CHAR +
                          LockName;
    bsGotLocks: Result := EXTERNAL_EVENT_GOTLOCK;
    bsObjectDeleted: Result := EXTERNAL_EVENT_EMBEDDEDSTATEOFOBJECTCHANGED +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              ObjectID.AsString +
                                              PROPAGATOR_PARAMETER_DELIMITER_CHAR +
                                              EXTERNAL_EVENT_EMBEDDEDSTATE_OBJECTDELETED
//    bsDBLock: Result := EXTERNAL_EVENT_DBLOCK;
    else
      Result := '';
  end;
end;

class function TBoldObjectSpaceExternalEvent.DecodeExternalEvent(Event: TBoldExternalEvent;
                                                            var Classname, MemberName, LockName: String;
                                                            ObjectID: TBoldDefaultID): TBoldObjectSpaceSubscriptionType;
begin
  Result := GetEventType(Event);

  case Result of
    bsClassChanged: ClassName := Copy(Event, Length(EXTERNAL_EVENT_CLASSCHANGED + SUBSCRIPTION_DELIMITER_CHAR) + 1, MaxInt);
    bsObjectdeleted, bsEmbeddedStateOfObjectChanged:
    begin
      GetID(Event, ObjectID);
    end;
    bsNonEmbeddedStateOfObjectChanged:
      begin
        MemberName := Copy(Event, pos(SUBSCRIPTION_DELIMITER_CHAR, Event) + 1, Pos(SUBSCRIPTION_DELIMITER_CHAR, Copy(Event, pos(SUBSCRIPTION_DELIMITER_CHAR, Event) + 1, maxint)) - 1);
        GetID(Event, ObjectID);
      end;
    bsLockLost: LockName := Copy(Event, Length(EXTERNAL_EVENT_LOCKLOST + SUBSCRIPTION_DELIMITER_CHAR) + 1, MaxInt);
    bsGotLocks{, bsDBLock}: ;
  end;
end;


class function TBoldObjectSpaceExternalEvent.GetParameter(Event: tBoldExternalEvent): String;
var
  p: integer;
begin
  p := pos(PROPAGATOR_PARAMETER_DELIMITER_CHAR, Event);
  if p <> 0 then
    result := copy(Event, p+1, maxint)
  else
    result := '';
end;

end.
