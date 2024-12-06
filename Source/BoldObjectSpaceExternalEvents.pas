{ Global compiler directives }
{$include bold.inc}
unit BoldObjectSpaceExternalEvents;

interface

uses
  BoldID,
  BoldDefaultID,
  BoldLogHandler;

type
  TBoldEventType = Char;
  TBoldExternalEvent = String;
  TBoldObjectSpaceExternalEvent = class;

  TBoldObjectSpaceSubscriptionType =
    (bsClassChanged,
    bsEmbeddedStateOfObjectChanged,
    bsNonEmbeddedStateOfObjectChanged,
    bsGotLocks,
    bsLockLost{, bsDBLock},
    bsObjectCreated,
    bsObjectDeleted,
    bsMemberChanged);

  TBoldObjectSpaceExternalEvent = class
  private
    class procedure GetID(const Event: TBoldExternalEvent; ObjectID: TBoldDefaultID; AClassName: string = '');
    class function GetParameter(const Event: TBoldExternalEvent): String;
    class function GetEventType(const Event: TBoldExternalEvent): TBoldObjectSpaceSubscriptionType;
  public
    class function EncodeExternalEvent(SubscriptionType: TBoldObjectSpaceSubscriptionType; const AClassName, AMemberName, ALockName: string;
                       ObjectID: TBoldObjectID): String;
    class function DecodeExternalEvent(const Event: TBoldExternalEvent;
                                  var AClassname, MemberName, LockName: String;
                                  ObjectID: TBoldDefaultID): TBoldObjectSpaceSubscriptionType;
  end;

procedure BoldOSSLog(const s: string);
procedure BoldOSSLogFmt(const s: string; const Args: array of const);

var
  BoldOSSLogHandler: TBoldLogHandler;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldIsoDateTime;

const
  SUBSCRIPTION_DELIMITER_CHAR = ':';
  CLASS_MEMBER_SEPARATOR = '.';
  EXTERNAL_EVENT_CLASSCHANGED = 'C';
  EXTERNAL_EVENT_MEMBERCHANGED = 'M';
  EXTERNAL_EVENT_EMBEDDEDSTATEOFOBJECTCHANGED = 'E';
  EXTERNAL_EVENT_NONEMBEDDEDSTATEOFOBJECTCHANGED = 'I';
  EXTERNAL_EVENT_OBJECTCREATED = 'N';
  EXTERNAL_EVENT_EMBEDDEDSTATE_OBJECTDELETED = 'D';
  EXTERNAL_EVENT_LOCKLOST = 'L';
  EXTERNAL_EVENT_GOTLOCK = 'GotLocks';

procedure BoldOSSLog(const s: string);
begin
  if assigned(BoldOSSLogHandler) then
    BoldOSSLogHandler.Log(AsISODateTimeMS(now)+':'+trim(s));
end;

procedure BoldOSSLogFmt(const s: string; const Args: array of const);
begin
  if assigned(BoldOSSLogHandler) then
    BoldOSSLogHandler.LogFmt(AsIsoDateTimeMs(now)+':'+ trim(s), Args);
end;

class function TBoldObjectSpaceExternalEvent.GetEventType(const Event: TBoldExternalEvent): TBoldObjectSpaceSubscriptionType;
var
  ExternalEvent: char;
begin
  ExternalEvent := ' ';
  if length(Event) > 0 then
    ExternalEvent := Event[1];
  case ExternalEvent of
    EXTERNAL_EVENT_CLASSCHANGED: Result := bsClassChanged;
    EXTERNAL_EVENT_MEMBERCHANGED: Result := bsMemberChanged;
    EXTERNAL_EVENT_EMBEDDEDSTATEOFOBJECTCHANGED:
    begin
      if GetParameter(Event) = EXTERNAL_EVENT_EMBEDDEDSTATE_OBJECTDELETED then
        result := bsObjectDeleted
      else
        Result := bsEmbeddedStateOfObjectChanged;
    end;
    EXTERNAL_EVENT_NONEMBEDDEDSTATEOFOBJECTCHANGED: Result := bsNonEmbeddedStateOfObjectChanged;
    EXTERNAL_EVENT_OBJECTCREATED: Result := bsObjectCreated;
    EXTERNAL_EVENT_LOCKLOST: Result := bsLockLost;
    else
    begin
      if Pos(EXTERNAL_EVENT_GOTLOCK, Event) = 1 then
        Result := bsGotLocks
      else
        raise EBold.CreateFmt(sInvalidEvent, [Event]);
    end;
  end
end;

class procedure TBoldObjectSpaceExternalEvent.GetID(const Event: TBoldExternalEvent;
 ObjectID: TBoldDefaultID; AClassName: string);
var
  IDAsInt: Integer;
  IDAsString: String;
begin
  if assigned(ObjectId) then
  begin
    case GetEventType(Event) of
      bsObjectDeleted, bsEmbeddedStateOfObjectChanged, bsObjectCreated, bsNonEmbeddedStateOfObjectChanged, bsMemberChanged:
      begin
        IDAsString := Event;
        Delete(IDAsString, 1, Pos(SUBSCRIPTION_DELIMITER_CHAR, IDAsString));
        Delete(IDAsString, 1, Pos(SUBSCRIPTION_DELIMITER_CHAR, IDAsString));
        Delete(IDAsString, Pos(PROPAGATOR_PARAMETER_DELIMITER_CHAR,IDAsString), MaxInt);
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
                  const AClassName, AMemberName, ALockName: string; ObjectID: TBoldObjectID): String;
begin
  case SubscriptionType of
    bsClassChanged: Result := EXTERNAL_EVENT_CLASSCHANGED +
                              SUBSCRIPTION_DELIMITER_CHAR +
                              AClassName;
    bsMemberChanged: Result := EXTERNAL_EVENT_MEMBERCHANGED +
                               SUBSCRIPTION_DELIMITER_CHAR +
                               AClassName +
                               CLASS_MEMBER_SEPARATOR +
                               AMemberName +
                               SUBSCRIPTION_DELIMITER_CHAR +
                               ObjectID.AsString;
    bsEmbeddedStateOfObjectChanged: Result := EXTERNAL_EVENT_EMBEDDEDSTATEOFOBJECTCHANGED +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              AClassName +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              ObjectID.AsString;
    bsNonEmbeddedStateOfObjectChanged: Result := EXTERNAL_EVENT_NONEMBEDDEDSTATEOFOBJECTCHANGED +
                                                  SUBSCRIPTION_DELIMITER_CHAR +
                                                  AClassName +
                                                  CLASS_MEMBER_SEPARATOR +
                                                  AMemberName +
                                                  SUBSCRIPTION_DELIMITER_CHAR +
                                                  ObjectID.AsString;
    bsLockLost: Result := EXTERNAL_EVENT_LOCKLOST +
                          SUBSCRIPTION_DELIMITER_CHAR +
                          ALockName;
    bsGotLocks: Result := EXTERNAL_EVENT_GOTLOCK;
    bsObjectCreated: Result := EXTERNAL_EVENT_OBJECTCREATED +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              AClassName +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              ObjectID.AsString;

    bsObjectDeleted: Result := EXTERNAL_EVENT_EMBEDDEDSTATEOFOBJECTCHANGED +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              AClassName +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              ObjectID.AsString +
                                              PROPAGATOR_PARAMETER_DELIMITER_CHAR +
                                              EXTERNAL_EVENT_EMBEDDEDSTATE_OBJECTDELETED
    else
      Result := '';
  end;
end;

class function TBoldObjectSpaceExternalEvent.DecodeExternalEvent(const Event: TBoldExternalEvent;
                                                            var AClassname, MemberName, LockName: String;
                                                            ObjectID: TBoldDefaultID): TBoldObjectSpaceSubscriptionType;
var
  s: string;
  i: integer;
begin
  Result := GetEventType(Event);
  case Result of
    bsClassChanged: AClassName := Copy(Event, Length(EXTERNAL_EVENT_CLASSCHANGED + SUBSCRIPTION_DELIMITER_CHAR) + 1, MaxInt);
    bsObjectCreated:
      begin
        AClassName := Copy(Event, Length(EXTERNAL_EVENT_OBJECTCREATED + SUBSCRIPTION_DELIMITER_CHAR) + 1, MaxInt);
        AClassName := Copy(AClassName, 1, Pos(SUBSCRIPTION_DELIMITER_CHAR,AClassName) - 1);
        GetID(Event, ObjectID, AClassName);
        BoldOSSLogFmt('Object %s.%s created', [AClassName, ObjectID.AsString]);
      end;
    bsMemberChanged:
      begin
        s := Copy(Event, pos(SUBSCRIPTION_DELIMITER_CHAR, Event) + 1, maxint);
        i := Pos(CLASS_MEMBER_SEPARATOR, s);
        AClassName := Copy(s, 1, i-1);
        MemberName := Copy(s, i+1, Pos(SUBSCRIPTION_DELIMITER_CHAR, s)-i-1);
        s := Copy(s, Pos(SUBSCRIPTION_DELIMITER_CHAR, s) + 1, MaxInt);
        ObjectID.AsInteger := StrToInt(s);
        BoldOSSLogFmt('Member modified %s.%s.%s', [AClassName, MemberName, ObjectID.AsString]);
      end;
      bsObjectDeleted, bsEmbeddedStateOfObjectChanged:
      begin
        i := pos(SUBSCRIPTION_DELIMITER_CHAR, Event) + 1;
        AClassName := Copy(Event, i, Pos(SUBSCRIPTION_DELIMITER_CHAR, Copy(Event, i, maxint)) - 1);
        GetID(Event, ObjectID, AClassName);
        case bsObjectDeleted of
          bsObjectDeleted: BoldOSSLogFmt('Object Deleted %s.%s', [AClassName, ObjectID.AsString]);
          bsEmbeddedStateOfObjectChanged: BoldOSSLogFmt('EmbeddedStateOfObjectChanged %s.%s', [AClassName, ObjectID.AsString]);
        end;
        BoldOSSLogFmt('Object Deleted %s.%s', [AClassName, ObjectID.AsString]);
      end;
    bsNonEmbeddedStateOfObjectChanged:
      begin
        i := pos(SUBSCRIPTION_DELIMITER_CHAR, Event) + 1;
        s := Copy(Event, i, Pos(SUBSCRIPTION_DELIMITER_CHAR, Copy(Event, i, maxint)) - 1);
        AClassName := Copy(s, 1, Pos(CLASS_MEMBER_SEPARATOR, s) - 1);
        MemberName := Copy(s, Pos(CLASS_MEMBER_SEPARATOR, s) +1, maxint);
        GetID(Event, ObjectID, AClassName);
        BoldOSSLogFmt('Non Embedded state of object changed: Deleted %s.%s.%s', [AClassName, ObjectID.AsString, MemberName]);
      end;
    bsLockLost:
      begin
        LockName := Copy(Event, Length(EXTERNAL_EVENT_LOCKLOST + SUBSCRIPTION_DELIMITER_CHAR) + 1, MaxInt);
        BoldOSSLogFmt('Lock Lost %s', [LockName]);
      end;
    bsGotLocks:
      begin
        LockName := Copy(Event, Length(EXTERNAL_EVENT_LOCKLOST + SUBSCRIPTION_DELIMITER_CHAR) + 1, MaxInt);
        BoldOSSLogFmt('Locks placed %s', [LockName]);
      end;
  end;
end;


class function TBoldObjectSpaceExternalEvent.GetParameter(const Event: TBoldExternalEvent): String;
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

