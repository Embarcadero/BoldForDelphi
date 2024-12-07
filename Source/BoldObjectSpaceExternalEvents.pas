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
    class procedure GetID(const AEvent: TBoldExternalEvent; AObjectID: TBoldDefaultID; AClassName: string = '');
    class function GetParameter(const AEvent: TBoldExternalEvent): String;
    class function GetEventType(const AEvent: TBoldExternalEvent): TBoldObjectSpaceSubscriptionType;
  public
    class function EncodeExternalEvent(ASubscriptionType: TBoldObjectSpaceSubscriptionType; const AClassName, AMemberName, ALockName: string;
                       AObjectID: TBoldObjectID): String;
    class function DecodeExternalEvent(const AEvent: TBoldExternalEvent;
                                  var AClassname, AMemberName, ALockName: String;
                                  AObjectID: TBoldDefaultID): TBoldObjectSpaceSubscriptionType;
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

class function TBoldObjectSpaceExternalEvent.GetEventType(const AEvent: TBoldExternalEvent): TBoldObjectSpaceSubscriptionType;
var
  ExternalEvent: char;
begin
  ExternalEvent := ' ';
  if length(AEvent) > 0 then
    ExternalEvent := AEvent[1];
  case ExternalEvent of
    EXTERNAL_EVENT_CLASSCHANGED: Result := bsClassChanged;
    EXTERNAL_EVENT_MEMBERCHANGED: Result := bsMemberChanged;
    EXTERNAL_EVENT_EMBEDDEDSTATEOFOBJECTCHANGED:
    begin
      if GetParameter(AEvent) = EXTERNAL_EVENT_EMBEDDEDSTATE_OBJECTDELETED then
        result := bsObjectDeleted
      else
        Result := bsEmbeddedStateOfObjectChanged;
    end;
    EXTERNAL_EVENT_NONEMBEDDEDSTATEOFOBJECTCHANGED: Result := bsNonEmbeddedStateOfObjectChanged;
    EXTERNAL_EVENT_OBJECTCREATED: Result := bsObjectCreated;
    EXTERNAL_EVENT_LOCKLOST: Result := bsLockLost;
    else
    begin
      if Pos(EXTERNAL_EVENT_GOTLOCK, AEvent) = 1 then
        Result := bsGotLocks
      else
        raise EBold.CreateFmt(sInvalidEvent, [AEvent]);
    end;
  end
end;

class procedure TBoldObjectSpaceExternalEvent.GetID(const AEvent: TBoldExternalEvent;
 AObjectID: TBoldDefaultID; AClassName: string);
var
  IDAsInt: Integer;
  IDAsString: String;
begin
  if assigned(AObjectId) then
  begin
    case GetEventType(AEvent) of
      bsObjectDeleted, bsEmbeddedStateOfObjectChanged, bsObjectCreated, bsNonEmbeddedStateOfObjectChanged, bsMemberChanged:
      begin
        IDAsString := AEvent;
        Delete(IDAsString, 1, Pos(SUBSCRIPTION_DELIMITER_CHAR, IDAsString));
        Delete(IDAsString, 1, Pos(SUBSCRIPTION_DELIMITER_CHAR, IDAsString));
        Delete(IDAsString, Pos(PROPAGATOR_PARAMETER_DELIMITER_CHAR,IDAsString), MaxInt);
      end;
    end;
    try
      IDAsInt := StrToInt(IDAsString);
      AObjectID.AsInteger := IDAsInt;
    except
      raise EBold.CreateFmt(sInvalidID, [IDAsString]);
    end;
  end;
end;

class function TBoldObjectSpaceExternalEvent.EncodeExternalEvent(ASubscriptionType: TBoldObjectSpaceSubscriptionType;
                  const AClassName, AMemberName, ALockName: string; AObjectID: TBoldObjectID): String;
begin
  case ASubscriptionType of
    bsClassChanged: Result := EXTERNAL_EVENT_CLASSCHANGED +
                              SUBSCRIPTION_DELIMITER_CHAR +
                              AClassName;
    bsMemberChanged: Result := EXTERNAL_EVENT_MEMBERCHANGED +
                               SUBSCRIPTION_DELIMITER_CHAR +
                               AClassName +
                               CLASS_MEMBER_SEPARATOR +
                               AMemberName +
                               SUBSCRIPTION_DELIMITER_CHAR +
                               AObjectID.AsString;
    bsEmbeddedStateOfObjectChanged: Result := EXTERNAL_EVENT_EMBEDDEDSTATEOFOBJECTCHANGED +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              AClassName +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              AObjectID.AsString;
    bsNonEmbeddedStateOfObjectChanged: Result := EXTERNAL_EVENT_NONEMBEDDEDSTATEOFOBJECTCHANGED +
                                                  SUBSCRIPTION_DELIMITER_CHAR +
                                                  AClassName +
                                                  CLASS_MEMBER_SEPARATOR +
                                                  AMemberName +
                                                  SUBSCRIPTION_DELIMITER_CHAR +
                                                  AObjectID.AsString;
    bsLockLost: Result := EXTERNAL_EVENT_LOCKLOST +
                          SUBSCRIPTION_DELIMITER_CHAR +
                          ALockName;
    bsGotLocks: Result := EXTERNAL_EVENT_GOTLOCK;
    bsObjectCreated: Result := EXTERNAL_EVENT_OBJECTCREATED +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              AClassName +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              AObjectID.AsString;

    bsObjectDeleted: Result := EXTERNAL_EVENT_EMBEDDEDSTATEOFOBJECTCHANGED +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              AClassName +
                                              SUBSCRIPTION_DELIMITER_CHAR +
                                              AObjectID.AsString +
                                              PROPAGATOR_PARAMETER_DELIMITER_CHAR +
                                              EXTERNAL_EVENT_EMBEDDEDSTATE_OBJECTDELETED
    else
      Result := '';
  end;
end;

class function TBoldObjectSpaceExternalEvent.DecodeExternalEvent(const AEvent: TBoldExternalEvent;
                                                            var AClassname, AMemberName, ALockName: String;
                                                            AObjectID: TBoldDefaultID): TBoldObjectSpaceSubscriptionType;
var
  s: string;
  i: integer;
begin
  Result := GetEventType(AEvent);
  case Result of
    bsClassChanged: AClassName := Copy(AEvent, Length(EXTERNAL_EVENT_CLASSCHANGED + SUBSCRIPTION_DELIMITER_CHAR) + 1, MaxInt);
    bsObjectCreated:
      begin
        AClassName := Copy(AEvent, Length(EXTERNAL_EVENT_OBJECTCREATED + SUBSCRIPTION_DELIMITER_CHAR) + 1, MaxInt);
        AClassName := Copy(AClassName, 1, Pos(SUBSCRIPTION_DELIMITER_CHAR,AClassName) - 1);
        GetID(AEvent, AObjectID, AClassName);
        BoldOSSLogFmt('Object %s.%s created', [AClassName, AObjectID.AsString]);
      end;
    bsMemberChanged:
      begin
        s := Copy(AEvent, pos(SUBSCRIPTION_DELIMITER_CHAR, AEvent) + 1, maxint);
        i := Pos(CLASS_MEMBER_SEPARATOR, s);
        AClassName := Copy(s, 1, i-1);
        AMemberName := Copy(s, i+1, Pos(SUBSCRIPTION_DELIMITER_CHAR, s)-i-1);
        s := Copy(s, Pos(SUBSCRIPTION_DELIMITER_CHAR, s) + 1, MaxInt);
        AObjectID.AsInteger := StrToInt(s);
        BoldOSSLogFmt('Member modified %s.%s.%s', [AClassName, AMemberName, AObjectID.AsString]);
      end;
      bsObjectDeleted, bsEmbeddedStateOfObjectChanged:
      begin
        i := pos(SUBSCRIPTION_DELIMITER_CHAR, AEvent) + 1;
        AClassName := Copy(AEvent, i, Pos(SUBSCRIPTION_DELIMITER_CHAR, Copy(AEvent, i, maxint)) - 1);
        GetID(AEvent, AObjectID, AClassName);
        case bsObjectDeleted of
          bsObjectDeleted: BoldOSSLogFmt('Object Deleted %s.%s', [AClassName, AObjectID.AsString]);
          bsEmbeddedStateOfObjectChanged: BoldOSSLogFmt('EmbeddedStateOfObjectChanged %s.%s', [AClassName, AObjectID.AsString]);
        end;
        BoldOSSLogFmt('Object Deleted %s.%s', [AClassName, AObjectID.AsString]);
      end;
    bsNonEmbeddedStateOfObjectChanged:
      begin
        i := pos(SUBSCRIPTION_DELIMITER_CHAR, AEvent) + 1;
        s := Copy(AEvent, i, Pos(SUBSCRIPTION_DELIMITER_CHAR, Copy(AEvent, i, maxint)) - 1);
        AClassName := Copy(s, 1, Pos(CLASS_MEMBER_SEPARATOR, s) - 1);
        AMemberName := Copy(s, Pos(CLASS_MEMBER_SEPARATOR, s) +1, maxint);
        GetID(AEvent, AObjectID, AClassName);
        BoldOSSLogFmt('Non Embedded state of object changed: Deleted %s.%s.%s', [AClassName, AObjectID.AsString, AMemberName]);
      end;
    bsLockLost:
      begin
        ALockName := Copy(AEvent, Length(EXTERNAL_EVENT_LOCKLOST + SUBSCRIPTION_DELIMITER_CHAR) + 1, MaxInt);
        BoldOSSLogFmt('Lock Lost %s', [ALockName]);
      end;
    bsGotLocks:
      begin
        ALockName := Copy(AEvent, Length(EXTERNAL_EVENT_LOCKLOST + SUBSCRIPTION_DELIMITER_CHAR) + 1, MaxInt);
        BoldOSSLogFmt('Locks placed %s', [ALockName]);
      end;
  end;
end;


class function TBoldObjectSpaceExternalEvent.GetParameter(const AEvent: TBoldExternalEvent): String;
var
  p: integer;
begin
  p := pos(PROPAGATOR_PARAMETER_DELIMITER_CHAR, AEvent);
  if p <> 0 then
    result := copy(AEvent, p+1, maxint)
  else
    result := '';
end;

end.

