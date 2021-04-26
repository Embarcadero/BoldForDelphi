
{ Global compiler directives }
{$include bold.inc}
unit BoldObjectSpaceExternalEvents;

interface

uses
  BoldID,
  BoldDefaultID;

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
    class function EncodeExternalEvent(SubscriptionType: TBoldObjectSpaceSubscriptionType; const ClassName,MemberName, LockName: string;
                       ObjectID: TBoldObjectID): String;
    class function DecodeExternalEvent(const Event: TBoldExternalEvent;
                                  var Classname, MemberName, LockName: String;
                                  ObjectID: TBoldDefaultID): TBoldObjectSpaceSubscriptionType;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldRev;

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
      if Pos(EXTERNAL_EVENT_GOTLOCK, Event) = 1 then
        Result := bsGotLocks

      else
        raise EBold.CreateFmt('Invalid event: %s', [Event]);
  end
end;

class procedure TBoldObjectSpaceExternalEvent.GetID(const Event: TBoldExternalEvent;
 ObjectID: TBoldDefaultID; AClassName: string);
var
  p, IDAsInt: Integer;
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
      raise EBold.CreateFmt('Invalid ID, %s is not an valid integer.', [IDAsString]);
    end;
  end;
end;

class function TBoldObjectSpaceExternalEvent.EncodeExternalEvent(SubscriptionType: TBoldObjectSpaceSubscriptionType;
                  const ClassName,MemberName, LockName: string; ObjectID: TBoldObjectID): String;
begin
  case SubscriptionType of
    bsClassChanged: Result := EXTERNAL_EVENT_CLASSCHANGED +
                              SUBSCRIPTION_DELIMITER_CHAR +
                              ClassName;
    bsMemberChanged: Result := EXTERNAL_EVENT_MEMBERCHANGED +
                               SUBSCRIPTION_DELIMITER_CHAR +
                               ClassName +
                               CLASS_MEMBER_SEPARATOR +
                               MemberName +
                               SUBSCRIPTION_DELIMITER_CHAR +
                               ObjectID.AsString;
    bsEmbeddedStateOfObjectChanged: Result := EXTERNAL_EVENT_EMBEDDEDSTATEOFOBJECTCHANGED +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              ClassName +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              ObjectID.AsString;
    bsNonEmbeddedStateOfObjectChanged: Result := EXTERNAL_EVENT_NONEMBEDDEDSTATEOFOBJECTCHANGED +
                                                  SUBSCRIPTION_DELIMITER_CHAR +
                                                  ClassName +
                                                  CLASS_MEMBER_SEPARATOR +
                                                  MemberName +
                                                  SUBSCRIPTION_DELIMITER_CHAR +
                                                  ObjectID.AsString;
    bsLockLost: Result := EXTERNAL_EVENT_LOCKLOST +
                          SUBSCRIPTION_DELIMITER_CHAR +
                          LockName;
    bsGotLocks: Result := EXTERNAL_EVENT_GOTLOCK;
    bsObjectCreated: Result := EXTERNAL_EVENT_OBJECTCREATED +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              ClassName +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              ObjectID.AsString;

    bsObjectDeleted: Result := EXTERNAL_EVENT_EMBEDDEDSTATEOFOBJECTCHANGED +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              ClassName +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              ObjectID.AsString +
                                              PROPAGATOR_PARAMETER_DELIMITER_CHAR +
                                              EXTERNAL_EVENT_EMBEDDEDSTATE_OBJECTDELETED
    else
      Result := '';
  end;
end;

class function TBoldObjectSpaceExternalEvent.DecodeExternalEvent(const Event: TBoldExternalEvent;
                                                            var Classname, MemberName, LockName: String;
                                                            ObjectID: TBoldDefaultID): TBoldObjectSpaceSubscriptionType;
var
  s: string;
  i: integer;
begin
  Result := GetEventType(Event);
  case Result of
    bsClassChanged: ClassName := Copy(Event, Length(EXTERNAL_EVENT_CLASSCHANGED + SUBSCRIPTION_DELIMITER_CHAR) + 1, MaxInt);
    bsObjectCreated:
      begin
        ClassName := Copy(Event, Length(EXTERNAL_EVENT_OBJECTCREATED + SUBSCRIPTION_DELIMITER_CHAR) + 1, MaxInt);
        ClassName := Copy(ClassName, 1, Pos(SUBSCRIPTION_DELIMITER_CHAR,ClassName) - 1);
        GetID(Event, ObjectID, ClassName);
      end;
    bsMemberChanged:
      begin
        s := Copy(Event, pos(SUBSCRIPTION_DELIMITER_CHAR, Event) + 1, maxint);
        i := Pos(CLASS_MEMBER_SEPARATOR, s);
        ClassName := Copy(s, 1, i-1);
        MemberName := Copy(s, i+1, Pos(SUBSCRIPTION_DELIMITER_CHAR, s)-i-1);
        s := Copy(s, Pos(SUBSCRIPTION_DELIMITER_CHAR, s) + 1, MaxInt);
        ObjectID.AsInteger := StrToInt(s);
      end;
      bsObjectDeleted, bsEmbeddedStateOfObjectChanged:
      begin
        i := pos(SUBSCRIPTION_DELIMITER_CHAR, Event) + 1;
        ClassName := Copy(Event, i, Pos(SUBSCRIPTION_DELIMITER_CHAR, Copy(Event, i, maxint)) - 1);
        GetID(Event, ObjectID, ClassName);
      end;
    bsNonEmbeddedStateOfObjectChanged:
      begin
        i := pos(SUBSCRIPTION_DELIMITER_CHAR, Event) + 1;
        s := Copy(Event, i, Pos(SUBSCRIPTION_DELIMITER_CHAR, Copy(Event, i, maxint)) - 1);
        ClassName := Copy(s, 1, Pos(CLASS_MEMBER_SEPARATOR, s) - 1);
        MemberName := Copy(s, Pos(CLASS_MEMBER_SEPARATOR, s) +1, maxint);
        GetID(Event, ObjectID, ClassName);
      end;
    bsLockLost: LockName := Copy(Event, Length(EXTERNAL_EVENT_LOCKLOST + SUBSCRIPTION_DELIMITER_CHAR) + 1, MaxInt);
    bsGotLocks{, bsDBLock}: ;
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

initialization

end.

