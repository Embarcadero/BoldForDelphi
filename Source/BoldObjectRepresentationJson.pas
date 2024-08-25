unit BoldObjectRepresentationJson;

{$INCLUDE bold.inc}

interface

uses
  Classes,
  BoldSystem,
  BoldElements,
  System.JSON;

const
  DefaultEmbeddedDepth = 20;

type
  TBoldResourceJSONFieldsConfiguration = class(TObject)
  private
    FFieldsConfigurationAsJSON : TJSONObject;
  public
    constructor CreateWithJSON(const aJSONConf: String);
    destructor Destroy; override;
    function IsFieldEmbbeded(const aResource, aFieldName : String) : Boolean;
    function IsFieldIncluded(const aResource, aFieldName : String) : Boolean;
    //property FieldsConfigurationAsJSON : String write SetFieldConfigurationAsJSON;
  end;

  TBoldElementJsonConverter = class
  private
    FBoldElement: TBoldElement;
    FJsonString: String;
    procedure ApplyJsonStringToBoldElement;
    procedure ApplyJsonDateTimeToBoldMember(aMember: TBoldMember; aJsonValue: TJSONValue);
//    function TrimLetterTFromDateTime(const aDateTime: String) : String;
    function CreateJsonForBoldObjectRef(aBoldObject: TBoldObject): TJSONObject;
    function CreateJsonForBoldObject(aBoldObject : TBoldObject;anEmbeddedDepth : Integer = DefaultEmbeddedDepth) : TJSONObject;
    function CreateJsonForBoldAttribute(aBoldAttribute : TBoldAttribute) : TJSONValue;
    function CreateJsonForBoldSingleLink(aBoldSingleLink : TBoldObjectReference; anEmbbedDepth : Integer = DefaultEmbeddedDepth) : TJSONValue;
    function CreateJsonForBoldMultiLink(aBoldMultiLink : TBoldObjectList; anEmbbedDepth : Integer = DefaultEmbeddedDepth) : TJSONValue;
    function ElementForId(aBoldSystem: TBoldSystem; anId : Integer) : TBoldElement;
    function GetJsonName(Member: TBoldMember) : String;
    procedure SetSingleLink(aBoldObject: TBoldObject; const SingleLinkMemberExpressionName: String; js: TJSONPair);overload;
//    procedure SetSingleLink(SingleLinkMember : TBoldObjectReference; js: TJSONPair; jsAttribute: TBoldMember); overload;
    procedure SetBoldElementAsVariant(ABoldElement: TBoldElement; aVariant: Variant);
    function CreateJson: TJSONValue;
    function GetJsonString: String;
    function IsMemberIncluded(aBoldObject : TBoldObject; const aMember : String) : Boolean;
    function IsMemberEmbedded(aBoldObject : TBoldObject; const aMember : String) : Boolean;
    function GetBoldResourceJSONFieldsConfiguration: TBoldResourceJSONFieldsConfiguration;
  public
    constructor CreateWithBoldElement(aBoldElement : TBoldElement);
    constructor CreateWithJsonString(const aJsonString : String);
    destructor Destroy; override;

    class procedure SetResourceFieldsConfiguration(const aConfiguration: String);
    function ReceivedJsonValue(js: TJSONObject; jsAttribute: TBoldMember; var Value: TJSONValue) : Boolean; overload;
    function ReceivedJsonValue(js: TJSONObject; jsAttribute: TBoldMember) : Boolean; overload;
    function ReceivedJsonValue(js: TJSONObject; jsAttribute: TBoldMember; var Value: Variant): Boolean; overload;

    property JsonString  : String read GetJsonString write FJsonString;
    property BoldElement : TBoldElement read FBoldElement write FBoldElement;
    property BoldResourceJSONFieldsConfiguration : TBoldResourceJSONFieldsConfiguration read GetBoldResourceJSONFieldsConfiguration;
  end;

function BoldElementToJsonString(aBoldElement : TBoldElement) : String;
function BoldElementToJson(aBoldElement : TBoldElement) : TJSONValue;
procedure JsonToBoldObject(const aJsonString: String; aBoldObject : TBoldObject);
function BoldObjectStringRepresentation(aBoldObject: TBoldObject) : String;

const
  UNASSIGNED_ID : Integer = 0;

implementation

uses
  SysUtils,
  DateUtils,
  System.Generics.Collections,

  //BoldExpressSecuredObject,
  BoldCoreConsts,
  BoldURI,
  BoldSystemRT,
  BoldAttributes,
  BoldDefaultId,
  BoldUMLTypes,
  BoldIndex,
  BoldDefs,

  Variants,
  XSBuiltIns;

{const
  DATE_SUFFIX : String = '_Date';
  TIME_SUFFIX : String = '_Time';}
var
  G_ResourceFieldsConfiguration : TBoldResourceJSONFieldsConfiguration = nil;

{ TBoldResourceJSONFieldsConfiguration }

constructor TBoldResourceJSONFieldsConfiguration.CreateWithJSON(const aJSONConf: String);
begin
  FFieldsConfigurationAsJSON := TJSonObject.ParseJSONValue(aJSONConf) as TJSONObject;

  if FFieldsConfigurationASJSON = nil then
    FFieldsConfigurationAsJSON := TJSONObject.Create;
end;

destructor TBoldResourceJSONFieldsConfiguration.Destroy;
begin
  FreeAndNil(FFieldsConfigurationAsJSON);
  inherited;
end;

function TBoldResourceJSONFieldsConfiguration.IsFieldEmbbeded(const aResource, aFieldName: String): Boolean;
var
  vResource : TJSONArray;
  vString, s : String;
  I : Integer;
  vFoundEmbedded, vFoundExclude : Boolean;
begin
  vFoundEmbedded := False;
  vFoundExclude := False;
  Result := False; // by default we don't embbed
  vResource := FFieldsConfigurationAsJSON.Values[aResource] as TJSONArray;

  if Assigned(vResource) then
  begin
    for I := 0 to vResource.Count - 1 do
    begin
      vString := Uppercase(vResource[i].Value);
      s := Uppercase(aFieldName);
      if vString = '-'+s then vFoundExclude := True;
      if vString = '*'+s then vFoundEmbedded := True;
    end;
    Result := not vFoundExclude and vFoundEmbedded;
  end;
end;

function TBoldResourceJSONFieldsConfiguration.IsFieldIncluded(const aResource,aFieldName: String): Boolean;
var
  vResource : TJSONArray;
  I : Integer;
  vString, s : String;
  vFoundInclude, vFoundExclude : Boolean;
begin
  vFoundInclude := False;
  vFoundExclude := False;
  vResource := FFieldsConfigurationAsJSON.Values[aResource] as TJSONArray;
  Result := (not Assigned(vResource));
  if not Result then
  begin
    for I := 0  to vResource.Count - 1 do
    begin
      vString := UpperCase(vResource[i].Value);
      s := Uppercase(aFieldName);
      if vString = '-'+s then vFoundExclude := True;
      if vString = s then vFoundInclude := True;
      if vString = '*'+s then vFoundInclude := True;
    end;

    Result := not vFoundExclude and vFoundInclude;
  end;
end;


function BoldObjectStringRepresentation(aBoldObject: TBoldObject) : String;
begin
  Result := '';
  if Assigned(aBoldObject) then
  try
    //TBoldExpressSecuredObject.StartImpersonation(aBoldObject.BoldSystem,'System');
    Result := aBoldObject.AsString;
  finally
    //TBoldExpressSecuredObject.EndImpersonation(aBoldObject.BoldSystem);
  end;
end;

function BoldElementToJson(aBoldElement : TBoldElement) : TJSONValue;
begin
  with TBoldElementJsonConverter.CreateWithBoldElement(aBoldElement) do
  begin
    try
      Result := CreateJson;
    finally
      Free;
    end;
  end;
end;

function BoldElementToJsonString(aBoldElement : TBoldElement) : String;
begin
  with TBoldElementJsonConverter.CreateWithBoldElement(aBoldElement) do
  begin
    try
      Result := JsonString;
    finally
      Free;
    end;
  end;
end;

procedure JsonToBoldObject(const aJsonString: String; aBoldObject : TBoldObject);
begin
  with TBoldElementJsonConverter.CreateWithBoldElement(aBoldObject) do
  try
    JsonString := aJsonString;
    ApplyJsonStringToBoldElement;
  finally
    Free;
  end;
end;

constructor TBoldElementJsonConverter.CreateWithBoldElement(aBoldElement: TBoldElement);
begin
  Create;
  BoldElement := aBoldElement;
end;

constructor TBoldElementJsonConverter.CreateWithJsonString(const aJsonString: String);
begin
  Create;
  JsonString := aJsonString;
end;

destructor TBoldElementJsonConverter.Destroy;
begin
  FBoldElement := nil;
  FJsonString := '';
  inherited;
end;

function TBoldElementJsonConverter.ElementForId(aBoldSystem: TBoldSystem; anId : Integer) : TBoldElement;
var
  vObjectID: TBoldDefaultID;
begin
  vObjectID := TBoldDefaultId.CreateWithClassID(0, false);
  try
    (vObjectID as TBoldDefaultId).AsInteger := anId;
    if vObjectID.AsInteger = UNASSIGNED_ID then
      Result := nil
    else
      Result := aBoldSystem.EnsuredLocatorByID[vObjectID].EnsuredBoldObject;
  finally
    FreeAndNil(vObjectID);
  end;
end;

class procedure TBoldElementJsonConverter.SetResourceFieldsConfiguration(const aConfiguration: String);
begin
  if Assigned(G_ResourceFieldsConfiguration) then
    G_ResourceFieldsConfiguration.Free;

  G_ResourceFieldsConfiguration := TBoldResourceJSONFieldsConfiguration.CreateWithJSON(aConfiguration);
end;

function TBoldElementJsonConverter.GetBoldResourceJSONFieldsConfiguration: TBoldResourceJSONFieldsConfiguration;
begin
  Result := G_ResourceFieldsConfiguration;
end;

function TBoldElementJsonConverter.GetJsonName(Member: TBoldMember) : String;
begin
  Result := Member.BoldMemberRTInfo.ExpressionName;
end;

function TBoldElementJsonConverter.GetJsonString: String;
var
  vJson  : TJSONValue;
begin
  vJson := CreateJson;
  Result := '';
  try
    if Assigned(vJSon) then
      Result := vJSon.ToJson;
  finally
    vJson.Free;
  end;
end;

function TBoldElementJsonConverter.IsMemberEmbedded(aBoldObject: TBoldObject; const aMember: String): Boolean;
begin
  Result := BoldResourceJSONFieldsConfiguration.IsFieldEmbbeded(aBoldObject.BoldClassTypeInfo.ExpressionName,aMember);
end;

function TBoldElementJsonConverter.IsMemberIncluded(aBoldObject: TBoldObject; const aMember: String): Boolean;
begin
  Result := BoldResourceJSONFieldsConfiguration.IsFieldIncluded(aBoldObject.BoldClassTypeInfo.ExpressionName,aMember);
end;

function TBoldElementJsonConverter.CreateJson: TJSONValue;
begin
  Result := nil;
  if BoldElement = nil then
    Result := TJSONNull.Create
  else
  if BoldElement is TBoldObject then
    Result := CreateJsonForBoldObject(TBoldObject(BoldElement))
  else
    case BoldElement.BoldType.BoldValueType of
      bvtList: result := CreateJsonForBoldMultiLink(TBoldObjectList(BoldElement));
      bvtClass: result := CreateJsonForBoldSingleLink(TBoldObjectReference(BoldElement));
      bvtAttr: result := CreateJsonForBoldAttribute(TBoldAttribute(BoldElement));
      bvtSystem, bvtType: Assert(false, 'not supported');
    end;
end;

(*function TBoldElementJsonConverter.GetSingleLinkJsonName(SingleLinkMemberExpressionName : String) : String;
begin
  Result := SingleLinkMemberExpressionName + ID_SUFFIX;
end;

function TBoldElementJsonConverter.GetSingleLinkAsStringJsonName(SingleLinkMemberExpressionName : String) : String;
begin
  Result := SingleLinkMemberExpressionName + ASSTRING_SUFFIX;
end;
  *)

{procedure TBoldElementJsonConverter.SetSingleLink(SingleLinkMember : TBoldObjectReference; js: TJSONPair; jsAttribute: TBoldMember);
begin
  SetSingleLink(SingleLinkMember.OwningObject,SingleLinkMember.BoldMemberRTInfo.ExpressionName, js);
end;}

function TBoldElementJsonConverter.ReceivedJsonValue(js: TJSONObject; jsAttribute: TBoldMember; var Value: TJSONValue): Boolean;
var
  JsonName : String;
begin
  JsonName := getJsonName(jsAttribute);
  Value := js.Values[JsonName];
  Result := Assigned(Value);
end;

function TBoldElementJsonConverter.ReceivedJsonValue(js: TJSONObject; jsAttribute: TBoldMember; var Value: Variant): Boolean;
var
 vJsonValue: TJSONValue;
begin
  Result := ReceivedJsonValue(js,jsAttribute,vJsonValue);
  if Result then Value := vJsonValue.Value;
end;

function TBoldElementJsonConverter.ReceivedJsonValue(js: TJSONObject; jsAttribute: TBoldMember): Boolean;
begin
  Result := Assigned(js.Values[getJsonName(jsAttribute)]);
end;

procedure TBoldElementJsonConverter.SetSingleLink(aBoldObject: TBoldObject; const SingleLinkMemberExpressionName: String; js: TJSONPair);
var
  vBoldObject: TBoldObject;
begin
  // navigable single links
  if not Assigned(aBoldObject) then
    raise Exception.Create('BoldJson.SetSingleLink called with nil aBoldObject for ' + SingleLinkMemberExpressionName);
  if js.JsonValue is TJSONNumber then
    vBoldObject := ElementForId(aBoldObject.BoldSystem,TJSONNumber(js.JsonValue).AsInt) as TBoldObject
  else
    vBoldObject := URIBoldElement(aBoldObject.BoldSystem, js.JsonValue.Value).Value as TBoldObject;
  TBoldObjectReference(aBoldOBject.BoldMemberByExpressionName[SingleLinkMemberExpressionName]).BoldObject := vBoldObject;
  if (js.JsonValue.Value <> '') and (vBoldObject = nil) then
    raise Exception.Create('Invalid URI for ' + js.JsonString.Value + ': ' + js.JsonValue.Value);
end;

procedure TBoldElementJsonConverter.SetBoldElementAsVariant(ABoldElement: TBoldElement; aVariant : Variant);
begin
  if aBoldElement.AsVariant <> aVariant then
    aBoldElement.AsVariant := aVariant;
end;

procedure TBoldElementJsonConverter.ApplyJsonDateTimeToBoldMember(aMember: TBoldMember; aJsonValue: TJSONValue);
begin
  if aJsonValue.Null then // Null date and time -> sets to 0 date
    SetBoldElementAsVariant(aMember, Null)
  else
  begin
    SetBoldElementAsVariant(aMember, ISO8601ToDate(aJSonValue.Value));
  end;
{  else
  begin //Date and Time separate
    vValue := aJsonObject.Values[(aDateTimeAsString + DATE_SUFFIX)];
    if (vValue <> nil) then
      if vValue is TJSONNull then
        SetBoldElementAsVariant(aMember,0)
      else
        SetBoldElementAsVariant(aMember,
          Int(StrToDateTime(TrimLetterTFromDateTime(vValue.Value), vDateFormatSettings) +
             TBADateTime(aMember).AsTime));

    vValue := aJsonObject.Values[(aDateTimeAsString + TIME_SUFFIX)];
    if Assigned(vValue) then // _Date and _Time send from client
      if not (vValue is TJSONNull) then
          SetBoldElementAsVariant(aMember,TBADateTime(aMember).AsDate +
            Frac(StrToDateTime(TrimLetterTFromDateTime(vValue.Value), vDateFormatSettings)))
      else
        SetBoldElementAsVariant(aMember, TBADateTime(aMember).AsDate)
  end;}
end;

{function TBoldElementJsonConverter.TrimLetterTFromDateTime(const aDateTime: String) : String;
var
  vTPos : Integer;
begin
  vTPos := Pos('T',aDateTime);
  if vTPos = 0 then
    Result := aDateTime
  else
    Result := Copy(aDateTime,1,vTPos-1) + ' ' + Copy(aDateTime,vTPos+1,MaxInt);
end;}

procedure TBoldElementJsonConverter.ApplyJsonStringToBoldElement;
var
  js, jsBase             : TJSONObject;
  i, j                   : Integer;
  vBoldObject            : TBoldObject;
  vBoldMember            : TBoldMember;
  vMemberNameAsString    : string;
//  vMemberValue           : Variant;
  vBooleanValue          : Boolean;
  vPair, vInnerPair: TJSONPair;
begin
  js := TJSonObject.ParseJSONValue(FJsonString) as TJSONObject;
  if js = nil then
    raise Exception.Create('Invalid JSON');

  try
    // Follow the model and look up the values for the object's members from the json
    if FBoldElement is TBoldObject then
    begin
      vBoldObject := TBoldObject(FBoldElement);
      for i := 0 to js.Count - 1 do
      begin
        vPair := js.Pairs[i];
        vMemberNameAsString := vPair.JsonString.Value;
        vBoldMember := vBoldObject.BoldMemberByExpressionName[vMemberNameAsString];
        if Assigned(vBoldMember) then
        begin
          if vPair.JsonValue is TJSONNumber then
          begin
            if (vBoldMember is TBANumeric) then
              SetBoldElementAsVariant(vBoldMember, vPair.JsonValue.Value)
            else
              raise Exception.CreateFmt('JSON was number but member %s was %s', [vMemberNameAsString, vBoldMember.ClassName]);
          end
          else
          if vPair.JsonValue is TJSONBool then
          begin
            if vBoldMember is TBABoolean then
              SetBoldElementAsVariant(vBoldMember, vPair.JsonValue.Value)
            else
              raise Exception.CreateFmt('JSON was Boolean but member %s was %s', [vMemberNameAsString, vBoldMember.ClassName]);
          end
          else
          if vPair.JsonValue is TJSONObject then
            begin
            jsBase := TJSONObject(vPair.JsonValue);
            for j := 0 to jsBase.Count - 1 do
            begin
              vInnerPair := jsBase.Pairs[j];
              vMemberNameAsString := vInnerPair.JsonString.Value;
              if AnsiCompareText(vMemberNameAsString, 'string') = 0 then
                SetBoldElementAsVariant(vBoldMember, vInnerPair.JsonValue.Value)
              else if AnsiCompareText(vMemberNameAsString, 'datetime') = 0 then
              begin
                ApplyJsonDateTimeToBoldMember(vBoldMember, vInnerPair.JsonValue)
              end
              else if AnsiCompareText(vMemberNameAsString, 'boolean') = 0 then
                SetBoldElementAsVariant(vBoldMember, (vInnerPair.JsonValue as TJSONBool).AsBoolean)
              else if AnsiCompareText(vMemberNameAsString, 'int') = 0 then
                SetBoldElementAsVariant(vBoldMember, (vInnerPair.JsonValue as TJSONNumber).AsInt64)
              else if AnsiCompareText(vMemberNameAsString, 'float') = 0 then
                SetBoldElementAsVariant(vBoldMember, (vInnerPair.JsonValue as TJSONNumber).AsDouble);
            end;
          end
          else
          if vPair.JsonValue is TJSONNull then
          begin
            if vBoldMember is TBoldAttribute then
              TBoldAttribute(vBoldMember).SetToNull
            else
              raise Exception.CreateFmt('Attempt to set %s to null, but it was not attribute', [vMemberNameAsString]);
          end
          else
          if vPair.JsonValue is TJSONString then
          begin
            if vBoldMember is TBAString  then
              SetBoldElementAsVariant(vBoldMember, vPair.JsonValue.Value)
            else if vBoldMember is TBAMoment then
              ApplyJsonDateTimeToBoldMember(vBoldMember, vPair.JsonValue)
            else if (vBoldMember is TBABoolean) and TryStrToBool(vPair.JsonValue.Value, vBooleanValue) then
              TBABoolean(vBoldMember).AsBoolean := vBooleanValue
            else
              raise Exception.CreateFmt('JSON was string but member %s was %s', [vMemberNameAsString, vBoldMember.ClassName]);
          end
          else if vBoldMember.BoldMemberRTInfo.IsSingleRole then
            SetSingleLink(vBoldObject, vMemberNameAsString, vPair)
          else
            raise Exception.CreateFmt('Unknown type for %s', [vMemberNameAsString]);
        end;
      end;
    end;
  finally
    FreeAndNil(js);
  end;
end;

function TBoldElementJsonConverter.CreateJsonForBoldAttribute(aBoldAttribute: TBoldAttribute): TJSONValue;
var
  vMemberValueAsVariant : Variant;
  vDateFormatSettings   : TFormatSettings;
  vDateTime             : Variant;
  vInt: Int64;
  vBool: Boolean;
begin
  vMemberValueAsVariant := aBoldAttribute.AsVariant;
  if VarIsNull(vMemberValueAsVariant) then
    Result := TJSONNull.Create
  else if aBoldAttribute is TBAString then
    Result := TJSONString.Create(vMemberValueAsVariant)
  else if (aBoldAttribute is TBAMoment) then
  begin
    vDateFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
    vDateFormatSettings.ShortTimeFormat := 'hh:nn:ss';
    vDateFormatSettings.DateSeparator := '-';
    vDateFormatSettings.TimeSeparator := ':';

    vDateTime := vMemberValueAsVariant;
    if vDateTime = 0 then
      Result := TJSONNull.Create
    else begin
      Result := TJSONObject.Create;
      with Result as TJSONObject do
      begin
        AddPair('datetime', (FormatDateTime(BoldDateTimeFormat,vDateTime,vDateFormatSettings)));
        AddPair('date', FormatDateTime('yyyy-mm-dd', vDateTime,vDateFormatSettings));
        AddPair('time', FormatDateTime('hh:nn:ss', vDateTime,vDateFormatSettings));
      end;
    end
  end
  else if (aBoldAttribute is TBANumeric) then
  begin
    if (aBoldAttribute is TBAFloat) or  (aBoldAttribute is TBACurrency) then
      Result := TJSONNumber.Create(TBANumeric(aBoldAttribute).AsFloat)
    else
    begin
      vInt := vMemberValueAsVariant;
      Result := TJSONNumber.Create(vInt);
    end;
  end
  else if aBoldAttribute is TBABoolean then
  begin
    vBool := vMemberValueAsVariant;
    case vBool of
      true: result := TJSONTrue.Create;
      false: result := TJSONFalse.Create;
    end
  end
  else
    Result := TJSONNull.Create;
end;

function TBoldElementJsonConverter.CreateJsonForBoldMultiLink(aBoldMultiLink: TBoldObjectList;anEmbbedDepth : Integer): TJSONValue;
var
  vLocator : TBoldObjectLocator;
  I : Integer;
begin
  // Fixme
  //vLocator := aBoldSingleLink.Locator;
  Result := TJSONArray.Create;
  aBoldMultiLink.EnsureObjects;
  for I := 0 to aBoldMultiLink.Count -1 do
  begin
    vLocator := aBoldMultiLink.Locators[I];
    if anEmbbedDepth > 0 then
      TJSONArray(Result).Add(CreateJsonForBoldObject(vLocator.EnsuredBoldObject,anEmbbedDepth - 1))
    else
      TJSONArray(Result).Add(CreateJsonForBoldObjectRef(vLocator.EnsuredBoldObject));
  end;
end;

function TBoldElementJsonConverter.CreateJsonForBoldObjectRef(aBoldObject : TBoldObject): TJSONObject;
var
  vSelfURI                 : string;
begin
  Result := TJSONObject.Create;
  if Assigned(aBoldObject) then
  begin
    //Removed as we use GUIDs. Result.Add('boldid',StrToInt(aBoldObject.BoldObjectLocator.BoldObjectID.AsString));

    try
      // Removed for Perf reason as Attracs string reps unreliable.  Result.Add(STRING_REP,BoldObjectStringRepresentation(aBoldObject));
    except
      on E:Exception do
      begin
        Result.AddPair('error', 'asString:' + E.Message);
      end;
    end;
    vSelfURI := BoldElementURI(aBoldObject);
    Result.AddPair('self', vSelfURI);
  end;
end;

function TBoldElementJsonConverter.CreateJsonForBoldObject(aBoldObject: TBoldObject; anEmbeddedDepth : Integer): TJSONObject;
var
  I                        : Integer;
  vMemberRTInfo            : TBoldMemberRTInfo;
  vBoldMember              : TBoldMember;
  vJsonFieldName           : String;
  vNewEmbbedDepth          : Integer;
  vBoldIDString            : String;
begin
  Result := CreateJsonForBoldObjectRef(aBoldObject);
  if Assigned(aBoldObject) then
    vBoldIDString := aBoldObject.BoldObjectLocator.BoldObjectID.AsString;
  if not Assigned(aBoldObject) or (anEmbeddedDepth = 0) then
    exit;

  //js.Add('id',StrToInt(aBoldObject.BoldObjectLocator.BoldObjectID.AsString));
  //Result.Add('id',aBoldObject.BoldMemberByExpressionName['objectGUID'].AsString);

  for I := 0 to aBoldObject.BoldClassTypeInfo.AllMembersCount - 1 do
  try
    vBoldMember := aBoldObject.BoldMembers[i];
    vMemberRTInfo := vBoldMember.BoldMemberRTInfo;
    vJsonFieldName := GetJsonName(vBoldMember);

    if (*(vMemberRTInfo.Visibility = BoldUMLTypes.vkPublic) and *) IsMemberIncluded(aBoldObject,vJsonFieldName) then
    begin
      if isMemberEmbedded(aBoldObject,vJsonFieldName) then
        vNewEmbbedDepth := anEmbeddedDepth - 1
      else
        vNewEmbbedDepth := 0;

      if vMemberRTInfo.IsMultiRole and (vMemberRTInfo as TBoldRoleRTInfo).IsNavigable then
      begin // Multi-links
         Result.AddPair(vJsonFieldName, CreateJsonForBoldMultiLink(TBoldObjectList(vBoldMember),vNewEmbbedDepth));
      end
      else if vMemberRTInfo.IsSingleRole and (vMemberRTInfo as TBoldRoleRTInfo).IsNavigable then
      begin

        Result.AddPair(vJsonFieldName,CreateJsonForBoldSingleLink(TBoldObjectReference(vBoldMember),vNewEmbbedDepth));
        //Result.AddPair(vJsonFieldName+'.href', BoldElementURI(aBoldSingleLink));
      end
      else if vMemberRTInfo.IsAttribute {and (UpperCase(vMemberRTInfo.ModelName) <> 'OBJECTGUID')} then
        if (vMemberRTInfo.MemberClass = TBATypedBlob) or (vMemberRTInfo.MemberClass = TBABlob) then
          //TODO
        else
            Result.AddPair(vJsonFieldName,CreateJsonForBoldAttribute(TBoldAttribute(vBoldMember)));
    end; // MemberRTInfo.Visibility = BoldUMLTypes.vkPublic

  except
    On E:Exception do
    begin
      E.Message := Format('%s: %s', [vJsonFieldName, E.Message]);
      raise;
    end;
  end;
end;

function TBoldElementJsonConverter.CreateJsonForBoldSingleLink(aBoldSingleLink: TBoldObjectReference;anEmbbedDepth : Integer): TJSONValue;
var
  vBoldObject : TBoldObject;
begin
  Result := nil;
  if Assigned(aBoldSingleLink) then
    vBoldObject := aBoldSingleLink.BoldObject
  else
    vBoldObject := nil;

  try
    if anEmbbedDepth = 0 then
      Result :=  CreateJsonForBoldObjectRef(vBoldObject)
    else
      Result := CreateJsonForBoldObject(vBoldObject, anEmbbedDepth -1);
  except
    //Might not be allowed
  end;

end;

initialization
  TBoldElementJsonConverter.SetResourceFieldsConfiguration('{}');

finalization
  FreeAndNil(G_ResourceFieldsConfiguration);
  
end.

