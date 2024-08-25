{ Global compiler directives }
{$include bold.inc}
unit BoldMLAttributes;

interface

uses
  Classes,
  BoldDefs,
  BoldElements,
  BoldSystem,
  BoldSubscription,
  BoldDerivedValueSet,
  BoldDomainElement,
  BoldValueInterfaces,
  BoldAttributes;

const
  brMLString = 0;

type
  {--- Forward declarations ---}
  TBAMLValueSetValue = class;
  TBAMLValueSetValueList = class;
  TBAMLValueSet = class;
  TBALanguage = class;
  TBAMLString = class;
  TBAMLSubString = class;

  TBAStringClass = class of TBAString;

  {--- TBAMLValueSetValue ---}
  TBAMLValueSetValue = class(TBAValueSetValue)
  private
    FStringRepresentations: TBoldMemberList;
  protected
    function GetStringRepresentationCount: Integer; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    procedure AddMLString(Value: TBAMLString);
    function GetMLString(Representation: integer): TBAMLString;
  public
    constructor Create(List: TBAValueSetValueList);
    procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    destructor Destroy; override;
  end;

  {--- TBAMLValueSetValueList ---}
  TBAMLValueSetValueList = class(TBADerivedValueSetValueList)
  protected
    function FindByStringAndLanguage(Representation: integer; Value, Language: String): TBAValueSetValue;
    procedure AddMembers(Int: Integer; Members: Array of TBoldMember); override;
  public
  end;

  {--- TBAMLValueSet ---}
  TBAMLValueSet = class(TBAValueSet)
  protected
    function GetStringRepresentationByLanguage(Representation: integer; Languagename: String): String;
    procedure SetStringRepresentationByLanguage(Representation: integer; Languagename: String; NewValue: String);
    function GetStringBylanguage(representation: integer; LanguageName: String): TBAString;
    procedure Initialize; override;
  public
//    class function Getvalues: TBAValueSetValueList; override;  
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; requestedEvent: TBoldEvent = breReEvaluate); override;
    procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure SubscribeToLanguage(representation: integer; Language: String; Subscriber: TBoldSubscriber; requestedEvent: TBoldEvent);
    function CompareToEnumLiteral(const str: String): Boolean; override;
    property StringRepresentationByLanguage [representation: integer; Language: String]: String read GetStringRepresentationByLanguage write SetStringRepresentationByLanguage;
  end;

  {--- TBALanguage ---}
  TBALanguage = class(TBAValueSet)
  protected
    procedure Initialize; override;
  public
    class function GetValues: TBAValueSetValueList; override;
  end;

  {--- TBAMLString ---}
  TBAMLString = class(TBAString)
  private
    fMLStrings: TStringList;
    procedure InternalSetDataValue(Representation:TBoldRepresentation; Value: String);
    function GetContentAsBlob: TBoldAnsiString;
    procedure SetContentAsBlob(const NewValue: TBoldAnsiString);
    function GetStringBylanguage(LanguageName: String): TBAString;
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    procedure ReceiveEventFromOwned(originator: TObject; originalEvent: TBoldEvent; const Args: array of const); override;
    function ReceiveQueryFromOwned(Originator: TObject; OriginalEvent: TBoldEvent; const Args: array of const; Subscriber: TBoldSubscriber): Boolean; override;
    function StringClass: TBAStringClass; virtual;
    procedure Initialize; override;
  public
    destructor Destroy; override;
    function GetStreamName: string; override;
    function ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    procedure Assign(Source: TBoldElement); override;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; requestedEvent: TBoldEvent = breReEvaluate); override;
    procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure SubscribeToLanguage(Language: String; Subscriber: TBoldSubscriber; requestedEvent: TBoldEvent);
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    property AsStringByLanguage[Language: String]: TBAString read GetStringByLanguage;
  end;

  { TBAMLString_Proxy }
  TBAMLString_Proxy = class(TBAString_Proxy, IBoldBlobContent)
  private
    function GetProxedMLString: TBAMLString;
  protected
    property ProxedMLString: TBAMLString read GetProxedMLString implements IBoldBlobContent;
  end;

  { TBAMLSubString }
  TBAMLSubString = class(TBAString)
  protected
    function StartModify: Boolean; override;
  end;

function BoldPrimaryLanguage(BoldSystem: TBoldSystem): TBALanguage;
procedure BoldSetPrimaryLanguageByName(BoldSystem: TBoldSystem; LanguageName: String);

function BoldSecondaryLanguage(BoldSystem: TBoldSystem): TBALanguage;
procedure BoldSetSecondaryLanguageByName(BoldSystem: TBoldSystem; LanguageName: String);
function BoldLanguageList: TBAValueSetValueList;

var
  BoldMLStringSecondaryFallBack: string = '< %s: %s >';
  BoldMLStringUnknownFallback: string = '<< %s: %s >>';

  BoldMLLanguageClassName: String = 'LanguageClass';
  BoldMLLanguageNameAttributeName: String = 'LanguageName';
  BoldMLLanguageNumberAttributeName: String = 'LanguageNumber';

implementation

uses
  SysUtils,
  BoldCoreConsts,
  BoldUtils,
  BoldNameExpander,
  BoldTaggedValueSupport,
  BoldSystemRT,
  BoldMemberTypeDictionary,
  BoldDefaultStreamNames;

var
  _BoldLanguageList: TBADerivedValueSetValueList;
  _Systems,
  _SecondaryLanguages,
  _PrimaryLanguages: TList;

  LoadingOfLanguages: Boolean = false;

{--- global functions ---}

procedure EnsureLanguageList;
var
  LanguageTypeInfo: TBoldClassTypeINfo;
  MemberRTInfo: TBoldMemberRTInfo;
begin
  if loadingOfLanguages then
    raise EBold.create(sBootStrapProblem);

  if not assigned(_BoldLanguageList) then
  begin
    LoadingOfLanguages := true;
    LanguageTypeInfo := TBoldSystem.DefaultSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[BoldMLLanguageClassName];
    if not assigned(LanguageTypeInfo) then
      raise EBold.CreateFmt(sNeedClassCalledX, [BoldMLLanguageClassName]);
    MemberRTinfo := LanguageTypeInfo.MemberRTInfoByExpressionName[BoldMLLanguageNameAttributeName];
    if not assigned(memberRTInfo) then
      raise EBold.CreateFmt(sNeedMemberCalledX, [BoldMLLanguageClassName, BoldMLLanguageNameAttributeName]);
    if not memberRTInfo.MemberClass.InheritsFrom(TBAString) then
      raise EBold.CreateFmt(sMustBeTBAString, [BoldMLLanguageNameAttributeName, memberRTInfo.MemberClass.ClassName]);
    memberRTInfo := LanguageTypeInfo.MemberRTInfoByExpressionName[BoldMLLanguageNumberAttributeName];
    if not assigned(memberRTInfo) then
      raise EBold.CreateFmt(sNeedIntegerMemberX, [BoldMLLanguageClassName, BoldMLLanguageNumberAttributeName]);
    if not memberRTInfo.MemberClass.InheritsFrom(TBAInteger) then
      raise EBold.CreateFmt(sMemberXMustBeInteger, [BoldMLLanguageNumberAttributeName, memberRTInfo.MemberClass.ClassName]);

    _BoldLanguageList := TBADerivedValueSetValueList.Create(TBoldSystem.DefaultSystem, BoldMLLanguageClassName, BoldMLLanguageNumberAttributeName, [BoldMLLanguageNameAttributeName]);
    LoadingOfLanguages := false;
  end;
end;

function BoldLanguageList: TBAValueSetValueList;
begin
  EnsureLanguageList;
  result := _BoldLanguageList;
end;

procedure ValidateLanguage(LanguageName, Location: String);
begin
  EnsureLanguageList;
  if (Languagename <> '') and
     not loadingOfLanguages and
     not assigned(_BoldLanguageList.FindByText(brDefault, LanguageName)) then
    raise EBold.CreateFmt(sInvalidLanguageName, [Location, languagename])
end;

function EnsureValuesForElement(BoldSystem: TBoldSystem): TBoldSystem;
begin
  if Assigned(BoldSystem) then
    result := BoldSystem
  else
    result := TBoldSystem.DefaultSystem;

  if not assigned(_Systems) then
  begin
    _Systems := TList.create;
    _PrimaryLanguages := TList.Create;
    _SecondaryLanguages := TList.Create;
  end;
  if _Systems.IndexOf(Result) = -1 then
  begin
    _Systems.Add(result);
    _PrimaryLanguages.Add(TBoldMemberfactory.CreateMemberFromBoldType(result.BoldSystemTypeInfo.AttributeTypeInfoByDelphiName['TBALanguage']));
    _SecondaryLanguages.Add(TBoldMemberfactory.CreateMemberFromBoldType(result.BoldSystemTypeInfo.AttributeTypeInfoByDelphiName['TBALanguage']));
  end;
end;

function BoldPrimaryLanguage(BoldSystem: tBoldSystem): TBALanguage;
begin
  Boldsystem := EnsureValuesForElement(BoldSystem);
  result := TBALanguage(_PrimaryLanguages[_Systems.IndexOf(BoldSystem)])
end;

function BoldSecondaryLanguage(BoldSystem: TBoldSystem): TBALanguage;
begin
  Boldsystem := EnsureValuesForElement(BoldSystem);
  result := TBALanguage(_SecondaryLanguages[_Systems.IndexOf(BoldSystem)])
end;

procedure BoldSetPrimaryLanguageByName(BoldSystem: TBoldSystem; LanguageName: String);
begin
  ValidateLanguage(LanguageName, 'BoldSetPrimaryLanguageByName');
  BoldPrimaryLanguage(BoldSystem).AsString := LanguageName;
end;

procedure BoldSetSecondaryLanguageByName(BoldSystem: TBoldSystem; LanguageName: String);
begin
  ValidateLanguage(LanguageName, 'BoldSetSecondaryLanguageByName');
  BoldSecondaryLanguage(BoldSystem).AsString := LanguageName;
end;

{--- TBAMLValueSetValue ---}

constructor TBAMLValueSetValue.Create(List: TBAValueSetValueList);
begin
  inherited Create;
  fStringRepresentations := TBoldMemberList.Create;
  list.AddValue(self);
end;

destructor TBAMLValueSetValue.Destroy;
begin
  FreeAndNil(fStringRepresentations);
  inherited;
end;

function TBAMLValueSetValue.GetStringRepresentationCount: Integer;
begin
  result := FStringRepresentations.Count;
end;

function TBAMLValueSetValue.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  if representation in [1..StringRepresentationCount] then
    result := fStringRepresentations[Representation - 1].AsString
  else
    raise EBold.CreateFmt('%s: representation not supported %d', [ClassName, representation]);
end;

procedure TBAMLValueSetValue.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if representation in [1..StringRepresentationCount] then
    fStringRepresentations[Representation - 1].AsString := Value
  else
    raise EBold.CreateFmt('%s: Representation not supported %d', [ClassName, representation]);
end;

function TBAMLValueSetValue.GetMLString(Representation: integer): TBAMLString;
begin
  if representation in [1..StringRepresentationCount] then
    result := FStringRepresentations[Representation - 1] as TBAMLString
  else
    raise EBold.CreateFmt('%s: Representation not supported %d', [ClassName, representation]);
end;

procedure TBAMLValueSetValue.AddMLString(Value: TBAMLString);
begin
  fStringRepresentations.Add(Value);
end;

{--- TBAMLValueSetValueList ---}

procedure TBAMLValueSetValueList.AddMembers(Int: Integer; Members: Array of TBoldMember);
var
  i: integer;
begin
  with TBAMLValueSetValue.Create(self) do
  begin
    AsInteger := Int;
    for i := 0 to High(Members) do
    begin
      if not (members[i] is TBAMLString) then
        raise EBold.CreateFmt(sMLValueSetsRequireMLStrings, [Members[i].Classname]);
      AddMLString(Members[i] as TBAMLString);
    end;
  end;
end;

function TBAMLValueSetValueList.FindByStringAndLanguage(Representation: integer; Value, Language: String): TBAValueSetValue;
var
  i: integer;
begin
  result := nil;
  for i := 0 to FValueList.count - 1 do
  begin
    if (TObject(FValueList[i]) as TBAMLValueSetValue).GetMLString(representation).GetStringBylanguage(Language).AsString = Value then
    begin
      result := TBAMLValueSetValue(FValueList[i]);
      break;
    end;
  end;
end;

{--- TBAMLValueSet ---}

procedure TBAMLValueSet.DefaultSubscribe(Subscriber: TBoldSubscriber; requestedEvent: TBoldEvent = breReEvaluate);
begin
  inherited;
  BoldPrimaryLanguage(BoldSystem).DefaultSubscribe(Subscriber, requestedEvent);
end;

procedure TBAMLValueSet.SubscribeToLanguage(representation: integer; Language: String; Subscriber: TBoldSubscriber; requestedEvent: TBoldEvent);
begin
  ValidateLanguage(Language, ClassName + '.SubscribeToLanguage');
  GetStringBylanguage(Representation, Language).DefaultSubscribe(Subscriber, requestedEvent);
end;

function TBAMLValueSet.GetStringBylanguage(representation: integer; LanguageName: String): TBAString;
var
  temp: TBAValueSetValue;
  TempML: TBAMLString;
begin
  Result := nil;
  ValidateLanguage(LanguageName, ClassName + '.GetStringByLanguage');

  temp := Values.FindByInteger(AsInteger);
  if temp is TBAMLValueSetValue then
  begin
    tempML := (Temp as TBAMLvalueSetValue).GetMLString(Representation);
    result := TempMl.AsStringByLanguage[LanguageName];
  end;
end;

procedure TBAMLValueSet.Initialize;
var
  x: TBAvalueSetValue;
begin
  inherited;

  if assigned(Values) then
  begin
    x := Values.GetFirstvalue;
    if assigned(x) then
      ContentAsInteger := x.AsInteger
    else
      SetContentToNull;
  end
  else
    SetContentToNull;
end;

function TBAMLValueSet.GetStringRepresentationByLanguage(Representation: integer; LAnguagename: String): String;
begin
  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Result := GetStringbylanguage(Representation, LanguageName).AsString;
end;

procedure TBAMLValueSet.SetStringRepresentationByLanguage(Representation: integer; LAnguagename: String; NewValue: String);
var
  MLValueSetValueList: TBAMLValueSetValueList;
  MLValue: TBAValueSetValue;
begin
  MLValueSetValueList := Values as TBAMLValueSetValueList;
  MLValue := MLValueSetValueList.FindByStringAndLanguage(Representation, NewValue, LanguageName);
  if assigned(MLValue) then
    AsInteger := MLValue.AsInteger
  else
    raise EBold.CreateFmt(sInvalidValue, [ClassName, NewValue]);
end;

{--- TBALanguage ---}

class function TBALanguage.GetValues: TBAValueSetValueList;
begin
  result := nil;
  if TBoldSystem.DefaultSystem <> nil then
  if TBoldSystem.DefaultSystem.BoldSystemTypeInfo.ClassTypeInfoByExpressionName[BoldMLLanguageClassName] <> nil then
  begin
    EnsureLanguageList;
    result := _BoldLanguageList;
  end;
end;
{--- TBAMLString ---}

destructor TBAMLString.Destroy;
begin
  PrepareToDestroy;
  inherited;
  while fMLStrings.count > 0 do
  begin
    fMLStrings.Objects[0].Free;
    fMlStrings.Delete(0);
  end;
  fMlStrings.Free;
end;

procedure TBAMLString.Assign(Source: TBoldElement);
begin
  if (source is TBAMLString) then
    StringRepresentation[brMLString] := Source.StringRepresentation[brMLString]
  else
    AsString := Source.AsString;
end;

function TBAMLString.GetContentAsBlob: TBoldAnsiString;
begin
  result := TBoldAnsiString(StringRepresentation[brMLString]);
end;

procedure TBAMLString.SetContentAsBlob(const NewValue: TBoldAnsiString);
begin
  StringRepresentation[brMLString] := String(NewValue);
end;


procedure TBAMLString.ReceiveEventFromOwned(originator: TObject; originalEvent: TBoldEvent; const Args: array of const);
begin
  if originalEvent in beValueEvents then
    SendEvent(OriginalEvent);

  if OriginalEvent = bePrepareModify then
  begin
    StartModify;
    PreChange;
  end;

  if OriginalEvent = beCompleteModify then
    EndModify;
end;

function TBAMLString.GetStringBylanguage(LanguageName: String): TBAString;
var
  StrPos: integer;
  BoldString: TBAString;
begin
  ValidateLanguage(LanguageName, ClassName + '.GetStringByLanguage');
  if LanguageName = '' then
    LanguageName := BoldPrimaryLanguage(BoldSystem).AsString;
  StrPos := fMLStrings.IndexOf(LanguageName);
  if StrPos = -1 then
  begin
    BoldString := TBoldDomainElementClass(StringClass).CreateWithOwner(self) as TBAString;
    StrPos := fMLStrings.AddObject(languagename, BoldString);
  end;
  Result := TBAString(fMLStrings.Objects[StrPos])
end;

function TBAMLString.GetStringRepresentation(Representation: TBoldRepresentation): string;
var
  i: integer;
  DisplayLanguage: TBALanguage;
begin
  case Representation of
    brMLString: begin
      Result := '';
      for i := 0 to fMLStrings.count - 1 do
        result := result + fMLStrings[i] + #255 + TBAString(fMLStrings.Objects[i]).AsString + #255;
    end;

    brDefault: begin
      if IsNull then
        result := ''
      else
      begin
        DisplayLanguage := BoldPrimaryLanguage(BoldSystem);
        result := GetStringByLanguage(DisplayLanguage.AsString).AsString;
        if result = '' then
        begin
          DisplayLanguage := BoldSecondaryLanguage(BoldSystem);
          result := GetStringByLanguage(DisplayLanguage.AsString).AsString;
          if result <> '' then
            result := format(BoldMLStringSecondaryFallBack, [DisplayLanguage.AsString, Result])
          else
          begin
            for i := 0 to BoldLanguageList.count - 1 do
            begin
              result := GetStringByLAnguage(BoldLanguageList[i].AsString).AsString;
              if result <> '' then
              begin
                result := format(BoldMLStringUnknownFallback, [BoldLanguageList[i].AsString, result]);
                break;
              end;
            end;
          end;
        end;
      end;
    end;

    else
      result := inherited GetStringRepresentation(Representation);
  end;
end;

procedure TBAMLString.InternalSetDataValue(Representation:TBoldRepresentation; Value: String);
var
  Lang, LangValue: String;
  StringContent: IBoldStringContent;
begin
  SetToNonNull;
  case Representation of
    brMLString: begin
      if not assigned(fMLStrings) then
        fMLStrings := TStringList.create;

      while fMLStrings.Count > 0 do
      begin
        fMLStrings.Objects[0].Free;
        fMLStrings.Delete(0);
      end;

      while Pos(#255, Value) <> 0 do
      begin
        Lang := copy(Value, 1, pos(#255, Value) - 1);
        Delete(Value, 1, Pos(#255, Value));
        LangValue := copy(Value, 1, pos(#255, Value) - 1);
        Delete(Value, 1, Pos(#255, Value));
        GetStringBylanguage(Lang).ProxyInterface(IBoldStringContent, bdepContents, StringContent);
        StringContent.asString := LangValue;
      end;
      Changed(beValueChanged, []);
    end;
    brDefault:
      GetStringByLanguage(BoldPrimaryLanguage(BoldSystem).AsString).AsString := Value
  end;
end;

procedure TBAMLString.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if representation in [brMLString, brDefault] then
    InternalSetDataValue(representation, Value)
  else
    inherited;
end;

procedure TBAMLString.DefaultSubscribe(Subscriber: TBoldSubscriber; requestedEvent: TBoldEvent = breReEvaluate);
begin
  inherited;
  BoldPrimaryLanguage(BoldSystem).DefaultSubscribe(Subscriber, requestedEvent);
  BoldSecondaryLanguage(BoldSystem).DefaultSubscribe(Subscriber, requestedEvent);
end;

procedure TBAMLString.SubscribeToLanguage(Language: String; Subscriber: TBoldSubscriber; requestedEvent: TBoldEvent);
begin
  ValidateLanguage(Language, ClassName + '.SubscribeToLanguage');
  GetStringBylanguage(Language).DefaultSubscribe(Subscriber, requestedEvent);
end;

type
  charArray = array of char;

function TBAMLString.GetStreamName: string;
begin
  Result := BoldContentName_Blob;
end;

procedure TBAMLString.SubscribeToStringRepresentation(
  Representation: TBoldRepresentation; Subscriber: TBoldSubscriber;
  RequestedEvent: TBoldEvent = breReEvaluate);
var
  Language: TBALanguage;
function SubscribeAndTestEmpty: Boolean;
var
  temp: TBAString;
begin
  temp := GetStringByLanguage(Language.AsString);
  temp.DefaultSubscribe(subscriber, requestedEvent);
  Language.DefaultSubscribe(subscriber, requestedEvent);
  result := temp.AsString = '';
end;

begin
  case Representation of
    brDefault: begin
      DefaultSubscribe(Subscriber, RequestedEvent);
      Language := BoldPrimaryLanguage(self.BoldSystem);
      if subscribeandTestEmpty then
      begin
        Language := BoldSecondaryLanguage(self.BoldSystem);
        SubscribeAndTestEmpty;
      end;
    end
    else
      inherited;
  end;
end;

function TBAMLString.StringClass: TBAStringClass;
begin
  result := TBAMLSubString;
end;

function TBAMLString.ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean;
begin
  if fMLStrings.count > 0 then
    result := (fMlStrings.Objects[0] as TBAString).ValidateCharacter(c, representation)
  else
    result := inherited ValidateCharacter(c, representation);
end;

function TBAMLString.ValidateString(const Value: string;
  Representation: TBoldRepresentation): Boolean;
begin
  if fMLStrings.count > 0 then
    result := (fMlStrings.Objects[0] as TBAString).ValidateString(Value, representation)
  else
    result := inherited ValidateString(value, representation);
end;

procedure TBALanguage.Initialize;
var
  y: TBAvalueSetValueList;
  x: TBAvalueSetValue;
begin
  inherited;
  y := Values;

  if assigned(y) then
  begin
    x := y.FindByInteger(0);
    if not assigned(x) then
      x := y.GetFirstvalue;
    if assigned(x) then
      ContentAsInteger := x.AsInteger
    else
      SetContentToNull;
  end else
    SetContentToNull;
end;

procedure TBAMLString.Initialize;
begin
  inherited;
  fMlStrings := TStringList.create;
end;

procedure TBAMLValueSetValue.SubscribeToStringRepresentation(
  Representation: TBoldRepresentation; Subscriber: TBoldSubscriber;
  RequestedEvent: TBoldEvent);
begin
  if representation in [1..StringRepresentationCount] then
    fStringRepresentations[Representation - 1].SubscribeToStringRepresentation(brDefault, subscriber, requestedEvent)
  else
    raise EBold.CreateFmt('%s: Representation not supported %d', [ClassName]);
end;

procedure TBAMLValueSet.SubscribeToStringRepresentation(
  Representation: TBoldRepresentation; Subscriber: TBoldSubscriber;
  RequestedEvent: TBoldEvent);
begin
  Values[AsInteger].SubscribeToStringRepresentation(Representation, subscriber, requestedEvent);
end;

function TBAMLString.ReceiveQueryFromOwned(Originator: TObject;
  OriginalEvent: TBoldEvent; const Args: array of const;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := true;
  if OriginalEvent = bqMayModify then
    result := CanModify;
end;

procedure TBAMLString.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldBlobContent;
begin
  if source.QueryInterface(IBoldBlobContent, S) = S_OK then
  begin
    if s.IsNull then
      SetContentToNull
    else
      StringRepresentation[brMLString] := String(s.asBlob);
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, 'AssignContentValue']); // do not localize
end;

{ TBoldMLString_Proxy }

function TBAMLString_Proxy.GetProxedMLString: TBAMLString;
begin
  result := ProxedMember as TBAMLString;
end;

function TBAMLString.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldBlobContent) then
  begin
    result := TBAString_Proxy.MakeProxy(self, Mode).GetInterface(IID, obj);
    if not result then
      raise EBoldInternal.CreateFmt(sProxyClassDidntImplementInterface, [ClassName]);
  end
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

{ TBAMLSubString }

function TBAMLSubString.StartModify: Boolean;
var
  OwningMember: TBoldMember;
begin
  if (OwningElement is TBoldMember) then
  begin
    if not (BoldPersistenceState in [bvpsCurrent, bvpsModified, bvpsTransient, bvpsInvalid]) then
      StateError('StartModify');
    result := CanModify;
    OwningMember := (OwningElement as TBoldMember);
    if result and assigned(OwningMember.BoldSystem) and assigned(OwningMember.OwningObject) and
       assigned(OwningMember.BoldSystem.PessimisticLockHandler) and
       not OwningMember.BoldMemberRTInfo.IsDerived then
      result := OwningMember.BoldSystem.PessimisticLockHandler.LockElement(OwningMember);
    if result then
    begin
      if assigned(OwningMember.OwningObject) and not OwningMember.BoldSystem.InTransaction and StoreInUndo then
        OwningMember.BoldSystem.UndoHandler.HandleMember(OwningMember.OwningObject.AsIBoldObjectContents[bdepContents],
                                                                        OwningMember.BoldMemberRTInfo.Index, OwningMember.AsIBoldValue[bdepContents]);
      DoStartModify;
    end;
  end
  else
    result := inherited StartModify;
end;

function TBAMLValueSet.CompareToEnumLiteral(const str: String): Boolean;
var
  i: integer;
  LangRepr: String;
begin
  result := false;
  for i := 0 to BoldLanguageList.Count-1 do
  begin
    LangRepr := StringRepresentationByLanguage[brDefault, BoldLanguageList[i].AsString];
    LangRepr := BoldExpandName(LangRepr, '', xtExpression, -1, nccTrue);
    result := result or SameText(LangRepr, Str);
    if Result then
      exit;
  end;
end;

initialization
  with BoldMemberTypes do
  begin
    AddMemberTypeDescriptor(TBALanguage, alConcrete);
    AddMemberTypeDescriptor(TBAMLValueSet, alConcrete);
    AddMemberTypeDescriptor(TBAMLString, alConcrete);
    AddMemberTypeDescriptor(TBAMLSubString, alConcrete);
  end;

finalization
  if BoldMemberTypesAssigned then
    with BoldMemberTypes do
    begin
      RemoveDescriptorByClass(TBALanguage);
      RemoveDescriptorByClass(TBAMLValueSet);
      RemoveDescriptorByClass(TBAMLString);
      RemoveDescriptorByClass(TBAMLSubString);
    end;
  _Systems.Free;
  if assigned(_PrimaryLanguages) then
    while _PrimaryLanguages.Count > 0 do
    begin
      TObject(_PrimaryLanguages[0]).Free;
      _PrimaryLanguages.Delete(0);
    end;
  _PrimaryLanguages.Free;

  if assigned(_SecondaryLanguages) then
    while _SecondaryLanguages.Count > 0 do
    begin
      TObject(_SecondaryLanguages[0]).Free;
      _SecondaryLanguages.Delete(0);
    end;
  _SecondaryLanguages.Free;

end.
