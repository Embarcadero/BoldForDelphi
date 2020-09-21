unit BoldAttributes;

interface

uses
  Classes,
  BoldSystem,
  BoldSystemRT,
  BoldDefs,
  BoldBase,
  BoldSubscription,
  BoldElements,
  BoldDomainElement,
  BoldValueInterfaces;

const
  {Query events}
  bqBaseAttributes = bqMaxSystem + 1;
  bqMaySetContentType = bqBaseAttributes + 0;
  bqMaxAttributes = bqBaseAttributes + 0;

type
  {Declare New Exceptions Types}

  {Forward declarations}
  TBAString = class;
  TBANumeric = class;
  TBAInteger = class;
  TBAByte = class;
  TBAShortInt = class;
  TBASmallInt = class;
  TBAWord = class;
  TBAFloat = class;
  TBACurrency = class;
  TBABoolean = class;
  TBABlob = class;
  TBATypedBlob = class;
  TBABlobImageJPEG = class;
  TBABlobImageBMP = class;
  TBAConstraint = class;
  TBAValueSetValue = class;
  TBAValueSetValueList = class;
  TBAValueSet = class;

  {---TBAString---}
  TBAString = class(TBoldAttribute)
  private
    fValue: string;
    procedure SetDataValue(NewValue: string);
    procedure SetContent(NewValue: string);
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    procedure FreeContent; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
    function MaySetValue(NewValue: String; Subscriber: TBoldSubscriber): Boolean; virtual;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure Assign(Source: TBoldElement); override;
    procedure AssignValue(Source: IBoldValue); override;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override;
    function CanSetValue(NewValue: string; Subscriber: TBoldSubscriber): Boolean;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure SetEmptyValue; override;
  end;

  {$IFNDEF T2H}
  TBAString_Proxy = class(TBoldAttribute_Proxy, IBoldStringContent)
  private
    function GetProxedString: TBAString;
    procedure SetContentAsString(const NewValue: String);
    function GetContentAsString: String;
  protected
    property ProxedString: TBAString read GetProxedString ;
  end;
  {$ENDIF}


  {---TBATrimmedString---}
  TBATrimmedString = class(TBAString)
  protected
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
  end;

  {---TBANumeric---}
  TBANumeric = class(TBoldAttribute)
  protected
    function GetAsFloat: Double; virtual; abstract;
    procedure SetAsInteger(Value: integer); virtual; abstract;
  public
    procedure SetEmptyValue; override;
    property AsFloat: Double read GetAsFloat;
    property AsInteger: Integer write SetAsInteger;
  end;

  {---TBAInteger---}
  TBAInteger = class(TBANumeric)
  private
    FValue: integer;
    procedure SetDataValue(NewValue: Integer);
    procedure SetContent(NewValue: Integer);
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    function CheckRangeWithBounds(Value, Min, Max: integer): boolean;
    function GetAsInteger: integer; virtual;
    function GetAsFloat: Double; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetAsInteger(Value: integer); override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
    function MaySetValue(NewValue: integer; Subscriber: TBoldSubscriber): Boolean; virtual;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure Assign(Source: TBoldElement); override;
    procedure AssignValue(Source: IBoldValue); override;
    function CanSetValue(Value: integer; Subscriber: TBoldSubscriber): Boolean;
    function CheckRange(Value: integer): Boolean; virtual;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure SetAsVariant(const Value: Variant); override;
    function GetAsVariant: Variant; override;
    function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    property AsInteger: integer read GetAsInteger write SetAsInteger;
  end;

  {---TBASmallInt---}
  TBASmallInt = class(TBAInteger)
  private
    function GetAsSmallInt: SmallInt;
    procedure SetAsSmallInt(const Value: SmallInt);
  public
    function CheckRange(Value: integer): Boolean; override;
    property AsSmallInt: SmallInt read GetAsSmallInt write SetAsSmallInt;
  end;

  {---TBAShortInt---}
  TBAShortInt = class(TBASmallInt)
  private
    function GetAsShortInt: ShortInt;
    procedure SetAsShortInt(const Value: ShortInt);
  public
    function CheckRange(Value: integer): Boolean; override;
    property AsShortInt: ShortInt read GetAsShortInt write SetAsShortInt;
  end;

  {---TBAWord---}
  TBAWord = class(TBAInteger)
  private
    function GetAsWord: Word;
    procedure SetAsWord(const Value: Word);
  public
    function CheckRange(Value: integer): Boolean; override;
    property AsWord: Word read GetAsWord write SetAsWord;
  end;

  {---TBAByte---}
  TBAByte = class(TBAWord)
  private
    function GetAsByte: Byte;
    procedure SetAsByte(const Value: Byte);
  public
    function CheckRange(Value: integer): Boolean; override;
    property AsByte: Byte read GetAsByte write SetAsByte;
  end;

  {---TBAFloat---}
  TBAFloat = class(TBANumeric)
  private
    fValue: Double;
    procedure SetContent(NewValue: Double);
    procedure SetDataValue(NewValue: Double);
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    function GetAsFloat: Double; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetAsFloat(Value: Double); virtual;
    procedure SetAsInteger(Value: integer); override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
    function MaySetValue(NewValue: Double; Subscriber: TBoldSubscriber): Boolean; virtual;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure SetAsVariant(const Value: Variant); override;
    function GetAsVariant: Variant; override;
    function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    procedure Assign(Source: TBoldElement); override;
    procedure AssignValue(Source: IBoldValue); override;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    property AsInteger: Integer write SetAsInteger;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    function CanSetValue(NewValue: Double; Subscriber: TBoldSubscriber): Boolean;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
  end;

  {---TBACurrency---}
  TBACurrency = class(TBANumeric)
  private
    FValue: Currency;
    procedure SetContent(NewValue: Currency);
    procedure SetDataValue(NewValue: Currency);
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetAsCurrency(Value: Currency);
    procedure SetAsFloat(Value: Double); virtual;
    procedure SetAsInteger(Value: integer); override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
    function MaySetValue(NewValue: Currency; Subscriber: TBoldSubscriber): Boolean; virtual;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure SetAsVariant(const Value: Variant); override;
    function GetAsVariant: Variant; override;
    function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    procedure Assign(Source: TBoldElement); override;
    procedure AssignValue(Source: IBoldValue); override;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer write SetAsInteger;
    function CanSetValue(NewValue: Currency; Subscriber: TBoldSubscriber): Boolean;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
  end;

  {-- TBoldBlobStream --}

  TBoldBlobStreamMode = (bmRead, bmWrite, bmReadWrite);

  { TBoldBlobStream }
  TBoldBlobStream = class(TStream)
  private
    fBlobAttr: TBABlob;
    fMode: TBoldBlobStreamMode;
    fPosition: Integer;
    function GetBlobSize: Integer;
    procedure StartModifyOfBlob(Operation: String);
    procedure EndModifyOfBlob;
    procedure FailModifyOfBlob;
    procedure InternalSetSize(NewSize: Integer; BlobEvents: Boolean);
  public
    constructor Create(BlobAttr: TBABlob; Mode: TBoldBlobStreamMode);
    function Read(var Buffer; Count: integer): integer; override;
    function Write(const Buffer; Count: integer): integer; override;
    function Seek(Offset: integer; Origin: Word): integer; override;
    procedure Clear;
    procedure Truncate;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SetSize(NewSize: integer); override;
  end;

  {---TBABlob---}
  TBABlob = class(TBoldAttribute)
  private
    FValue: string;
    procedure SetContent(NewValue: string);
    procedure SetDataValue(NewValue: string);
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    procedure FreeContent; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
    function GetAsBlob: String;
    procedure SetAsBlob(NewValue: String);
    function MaySetValue(NewValue: String; Subscriber: TBoldSubscriber): Boolean; virtual;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure SetAsVariant(const Value: Variant); override;
    function GetAsVariant: Variant; override;
    function CreateBlobStream(Mode: TBoldBlobStreamMode): TBoldBlobStream;
    procedure SetToNull; override;
    procedure Assign(Source: TBoldElement); override;
    procedure AssignValue(Source: IBoldValue); override;
    property ContentType: string index brShort read GetStringRepresentation write SetStringRepresentation;
    function CanSetValue(NewValue: string; Subscriber: TBoldSubscriber): Boolean;
    function CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure SetEmptyValue; override;
  end;

  {-- TBATypedBlob --}
  TBATypedBlob = class(TBABlob)
  private
    FContentType: string;
    procedure SetContentType2(Value: string);
    procedure SetContentTypeContent(NewValue: String);
  protected
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
    function GetContentTypeContent: String;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure AssignValue(Source: IBoldValue); override;
    procedure SetToNull; override;
    function CanSetContentType(Value: string; Subscriber: TBoldSubscriber): Boolean;
    function CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
  end;

  {-- TBABlobImageJPEG --}
  TBABlobImageJPEG = class(TBABlob)
  protected
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
  end;

  {-- TBABlobImageBMP --}
  TBABlobImageBMP = class(TBABlob)
  protected
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
  end;

  {---TBAMoment---}
  TBAMoment = class(TBoldAttribute)
  private
    FValue: TDateTime;
    procedure SetDataValue(NewValue: TDateTime);
    procedure SetContent(NewValue: TDateTime);
    function GetDays: Word;
    function GetHours: Word;
    function GetMinutes: Word;
    function GetMonths: Word;
    function GetSeconds: Word;
    function GetYears: Word;
  protected
    function GetAsDate: TDateTime;
    function GetAsDateTime: TDateTime;
    function GetAsTime: TDateTime;
    procedure SetAsDate(Value: TDateTime);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsTime(Value: TDateTime);
    function MaySetValue(NewValue: TDateTime; Subscriber: TBoldSubscriber): Boolean; virtual;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property Seconds: Word read GetSeconds;
    property Minutes: Word read GetMinutes;
    property Hours: Word read GetHours;
    property Days: Word read GetDays;
    property Months: Word read GetMonths;
    property Years: Word read GetYears;
  public
    function GetAsVariant: Variant; override;
    procedure SetAsVariant(const Value: Variant); override;
    procedure Assign(Source: TBoldElement); override;
    function CanSetValue(NewValue: TDateTime; Subscriber: TBoldSubscriber): Boolean;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    procedure SetEmptyValue; override;
  end;

  {---TBADateTime---}
  TBADateTime = class(TBAMoment)
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure AssignValue(Source: IBoldValue); override;
    property AsDateTime;
    property AsDate;
    property AsTime;
    property Seconds;
    property Minutes;
    property Hours;
    property Days;
    property Months;
    property Years;
    function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
  end;

  {---TBADate---}
  TBADate = class(TBAMoment)
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure AssignValue(Source: IBoldValue); override;
    function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    property AsDate;
    property Days;
    property Months;
    property Years;
  end;

  {---TBATime---}
  TBATime = class(TBAMoment)
  private
    function GetAsSeconds: cardinal;
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure AssignValue(Source: IBoldValue); override;
    function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    property AsTime;
    property AsSeconds: cardinal read GetAsSeconds;
    property Seconds;
    property Minutes;
    property Hours;
  end;

  {---TBAValueSetValue---}
  TBAValueSetValue = class(TBoldElement)
  private
    FAsInteger: Integer;
    FStringRepresentations: TStringList;
  protected
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
    function GetAsInteger: Integer;
    procedure SetAsInteger(Value: Integer);
    procedure AddString(Value: string);
    function GetStringRepresentationCount: Integer; virtual;
    function GetBoldType: TBoldElementTypeInfo; override;
  public
    constructor InternalCreate(StringValues: Integer; List: TBAValueSetValueList);
    destructor Destroy; override;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property StringRepresentationCount: Integer read GetStringRepresentationCount;
  end;

  {TBAValueSetValueList}
  TBAValueSetValueList = class(TBoldMemoryManagedObject)
  private
    function GetCount: integer;
    function GetValueSetValue(index: integer): TBAValueSetValue;
  protected
    FValueList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetFirstValue: TBAValueSetValue;
    procedure AddValue(Value: TBAValueSetValue);
    function FindByInteger(Value: Integer): TBAValueSetValue;
    function FindByString(Representation: TBoldRepresentation; Value: string): TBAValueSetValue;
    function FindByText(Representation: TBoldRepresentation; Value: string): TBAValueSetValue;
    procedure ToStrings(Representation: TBoldRepresentation; theStrings: TStrings);
    procedure ToStringsWithNil(Representation: TBoldRepresentation; theStrings: TStrings; nilstring: string);
    procedure Add(intvalue: Integer; arrString: array of string);
    property Count: integer read GetCount;
    property ValueSetValues[index: integer]: TBAValueSetValue read GetValueSetValue; default;
  end;

  {TBAValueSet}
  TBAValueSet = class(TBoldAttribute)
  private
    FValue: TBAValueSetValue;
    procedure CheckIllegalValue;
    function GetAsInteger: Integer;
    procedure SetAsInteger(Value: Integer);
    procedure SetContent(NewValue: TBAValueSetValue);
    procedure SetContentAsInteger(NewValue: Integer);
    procedure SetDataValue(NewValue: TBAValueSetValue);
  protected
    function GetContentAsInteger: Integer; virtual;
    procedure AssignContentValue(Source: IBoldValue); override;
    procedure InitializeMember(OwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override;
    function GetValues: TBAValueSetValueList; virtual; abstract;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override;
    function MaySetValue(NewValue: TBAValueSetValue; Subscriber: TBoldSubscriber): Boolean; virtual;
    property ContentAsInteger: Integer read GetContentAsInteger write SetContentAsInteger;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure SetAsVariant(const Value: Variant); override;
    function GetAsVariant: Variant; override;
    function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    procedure Assign(Source: TBoldElement); override;
    function CompareToEnumLiteral(const str: String): Boolean; virtual;
    procedure AssignValue(Source: IBoldValue); override;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    property Values: TBAValueSetValueList read GetValues;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    function CanSetValue(NewValue: TBAValueSetValue; Subscriber: TBoldSubscriber): Boolean;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure SetEmptyValue; override;
  end;



  {---TBABoolean---}
  TBABoolean = class(TBAValueSet)
  protected
    procedure AssignContentValue(Source: IBoldValue); override;
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(Value: Boolean);
    function GetValues: TBAValueSetValueList; override;
    function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure SetAsVariant(const Value: Variant); override;
    function GetAsVariant: Variant; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
  end;

  TBAConstraint = class(TBABoolean)
  private
    fConstraint: TBoldConstraintRTInfo;
    fOwningElement: TBoldElement;
    fElementSubscriber: TBoldPassThroughSubscriber;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure CalculateConstraint;
  protected
    function GetContentAsInteger: Integer; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure InitializeMember(OwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TBoldElement); override;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure Initialize(Constraint: TBoldConstraintRTInfo; OwningElement: TBoldElement);
    property Constraint: TBoldConstraintRTInfo read fConstraint;
    property OwningElement: TBoldElement read fOwningElement;
  end;

implementation

uses
  SysUtils,
  BoldNameExpander,
  BoldTaggedValueSupport,
  Variants,
  BoldUtils,
  BoldCoreConsts,
  BoldMemberTypeDictionary;

var
  _BooleanValues: TBAValueSetValueList;

const
  DEFAULTNOW = '<NOW>';
  Meth_GetStringRepresentation = 'GetStringRepresentation';
  Meth_SetStringRepresentation = 'SetStringRepresentation';
  Meth_AssignContentValue = 'AssignContentValue';
  Meth_AssignValue = 'AssignValue';

type
  TBAInteger_Proxy = class(TBoldAttribute_Proxy, IBoldIntegerContent)
  private
    function GetProxedInteger: TBAInteger;
    function GetContentAsInteger: Integer;
    procedure SetContentAsInteger(NewValue: Integer);
  protected
    property ProxedInteger: TBAInteger read GetProxedInteger;
  end;

  TBAFloat_Proxy = class(TBoldAttribute_Proxy, IBoldFloatContent)
  private
    function GetProxedFloat: TBAFloat;
    function GetContentAsFloat: Double;
    procedure SetContentAsFloat(NewValue: Double);
  protected
    property ProxedFloat: TBAFloat read GetProxedFloat;
  end;

  TBABlob_Proxy = class(TBoldAttribute_Proxy, IBoldBlobContent)
  private
    function GetProxedBlob: TBABlob;
    function GetContentAsBlob: String;
    procedure SetContentAsBlob(const NewValue: String);
  protected
    property ProxedBlob: TBABlob read GetProxedBlob;
  end;

  { TBACurrency_Proxy }
  TBACurrency_Proxy = class(TBoldAttribute_Proxy, IBoldCurrencyContent)
  private
    function GetProxedCurrency: TBACurrency;
    function GetContentAsCurrency: Currency;
    procedure SetContentAsCurrency(NewValue: Currency);
  protected
    property ProxedCurrency: TBACurrency read GetProxedCurrency;
  end;

  TBATypedBlob_Proxy = class(TBABlob_Proxy, IBoldTypedBlob)
  private
    function GetProxedTypedBlob: TBATypedBlob;
    function GetContentTypeContent: String;
    procedure SetContentTypeContent(const NewValue: String);
  protected
    property ProxedTypedBlob: TBATypedBlob read GetProxedTypedBlob;
  end;

  TBAMoment_Proxy = class(TBoldAttribute_Proxy)
  protected
    function GetProxedMoment: TBAMoment;
    function GetContentAsDateTime: TDateTime;
    procedure SetContentAsDateTime(NewValue: TDateTime);
    function GetContentAsDate: TDateTime;
    procedure SetContentAsDate(NewValue: TDateTime);
    function GetContentAsTime: TDateTime;
    procedure SetContentAsTime(NewValue: TDateTime);
    property ProxedMoment: TBAMoment read GetProxedMoment;
  end;

  TBADateTime_Proxy = class(TBAMoment_Proxy, IBoldDateTimeContent, IBoldTimeContent, IBoldDateContent)
  end;

  TBADate_Proxy = class(TBAMoment_Proxy, IBoldDateContent)
  end;

  TBATime_Proxy = class(TBAMoment_Proxy, IBoldTimeContent)
  end;

  TBAValueSet_Proxy = class(TBoldAttribute_Proxy, IBoldIntegerContent, IBoldStringContent)
  private
    function GetProxedvalueSet: TBAValueSet;
    function GetContentAsInteger: Integer;
    procedure SetContentAsInteger(NewValue: Integer);
    procedure SetContentAsString(const NewValue: String);
    function GetContentAsString: String;
  protected
    property ProxedValueSet: TBAValueSet read GetProxedValueSet;
  end;

  TBABoolean_Proxy = class(TBAValueSet_Proxy, IBoldBooleanContent)
  private
    function GetContentAsBoolean: Boolean;
    procedure SetContentAsBoolean(NewValue: Boolean);
  end;

{ TBAString }
procedure TBAString.SetDataValue(NewValue: string);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize

  if IsNull or (FValue <> NewValue) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize
    try
      SetContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBAString.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);

  EnsureValidString(Value, Representation);
  if Representation = brDefault then
    SetDataValue(Value)
  else
    inherited SetStringRepresentation(Representation, Value);
end;

function TBAString.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, Meth_GetStringRepresentation, '');
  if Representation <> brDefault then
    inherited GetStringRepresentation(Representation);

  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Result := FValue;
end;

function TBAString.ValidateString(Value: string; Representation: TBoldRepresentation): Boolean;
begin
  if Assigned(BoldAttributeRtInfo) then
  begin
    Result := (BoldAttributeRtInfo.Length = -1) or
      (Length(Value) <= BoldAttributeRtInfo.Length);
    if not result then
      SetBoldLastFailureReason(TBoldFailureReason.Create(sStringTooLong, self));
  end
  else
    Result := True;
  end;

procedure TBAString.Assign(Source: TBoldElement);
begin
  if Source is TBAString then
  begin
    if TBAString(Source).IsNull then
      SetToNull
    else
      AsString := Source.AsString;
  end
  else
    inherited;
end;

function TBAString.CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer;
begin
  if BoldElement is TBAString then
  begin
    if EitherIsNull(Self, TBAString(BoldElement)) then
      Result := NullSmallest(BoldElement)
    else
      case CompType of
        ctDefault:
          Result := AnsiCompareText(AsString, TBAString(BoldElement).AsString);
        ctAsString:
          Result := CompareStr(AsString, TBAString(BoldElement).AsString);
        ctAsText:
          Result := CompareText(AsString, TBAString(BoldElement).AsString);
        ctAsAnsiString:
          Result := AnsiCompareStr(AsString, TBAString(BoldElement).AsString);
        ctAsAnsiText:
          Result := AnsiCompareText(AsString, TBAString(BoldElement).AsString);
      else
        Result := inherited CompareToAs(CompType, BoldElement);
      end
  end
  else
    Result := inherited CompareToAs(CompType, BoldElement);
end;

function TBAString.CanSetValue(NewValue: string; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetValue(NewValue, Subscriber) and
            SendQuery(bqMaySetValue, [NewValue], Subscriber)
end;

function TBAString.MaySetValue(NewValue: String;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := true;
end;


procedure TBAString.AssignValue(Source: IBoldValue);
var
  s: IBoldStringContent;
begin
  if source.QueryInterface(IBoldStringContent, S) = S_OK then
    SetDataValue(s.AsString)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignValue]);
end;

procedure TBAString.SetContent(NewValue: string);
begin
  if (BoldPersistenceState = bvpsInvalid) or
     ContentIsNull or (FValue <> NewValue) then
  begin
    PreChange;
    FValue := NewValue;
    SetToNonNull;
    Changed(beValueChanged, [NewValue]);
  end;
end;

function TBAString_Proxy.GetContentAsString: string;
begin
  result := ProxedString.FValue;
end;

procedure TBAString_Proxy.SetContentAsString(const NewValue: string);
begin
  ProxedString.SetContent(NewValue);
end;

procedure TBAString.AssignContentValue(Source: IBoldValue);
var
  s: IBoldStringContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
    SetContentToNull
  else if not assigned(source) then
    SetContent('')
  else if source.QueryInterface(IBoldStringContent, S) = S_OK then
  begin
    if s.IsNull then
      SetContentToNull
    else
      SetContent(s.AsString)
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignContentValue]);
end;

function TBAString.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBAString_Proxy;
end;

function TBAString.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldStringContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldStringContent') // do not localize
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

procedure TBAString.FreeContent;
begin
  inherited;
  FValue := '';
end;

procedure TBAString.SetEmptyValue;
begin
  asString := '';
end;

{ TBATrimmedString }

procedure TBATrimmedString.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  inherited SetStringRepresentation(Representation, trim(Value));
end;

{ TBANumeric }

procedure TBANumeric.SetEmptyValue;
begin
  AsInteger := 0;
end;

{ TBAInteger }

procedure TBAInteger.SetDataValue(NewValue: Integer);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize
  if not CheckRange(NewValue) then
    raise EBoldInternal.CreateFmt('%s: %s', [DisplayName, GetBoldLastFailureReason.Reason]); // do not localize

  if IsNull or (FValue <> NewValue) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize
    try
      SetContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

function TBAInteger.CheckRange(Value: integer): Boolean;
begin
  result := CheckRangeWithBounds(Value, Low(integer), High(integer));
end;

procedure TBAInteger.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);
  if Representation = brDefault then
    if Value = '' then
      SetToNull
    else
      SetDataValue(StrToInt(Value))
  else
    inherited SetStringRepresentation(Representation, Value);
end;

function TBAInteger.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, Meth_GetStringRepresentation, '');
  if Representation <> brDefault then
    inherited GetStringRepresentation(Representation);
  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Str(GetAsInteger, Result);
end;

function TBAInteger.GetAsInteger: integer;
begin
  BoldClearLastFailure;
  if not canRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsInteger', ''); // do not localize
  EnsureNotNull; {ensures current}
  Result := FValue;
end;

function TBAInteger.GetAsFloat: Double;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsFloat', ''); // do not localize
  EnsureNotNull; {ensures current}
  Result := FValue;
end;

procedure TBAInteger.SetAsInteger(Value: integer);
begin
  SetDataValue(Value);
end;

function TBAInteger.ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean;
begin
  Result := C in ['0'..'9', '-', '+'];
end;

function TBAInteger.ValidateString(Value: string; Representation: TBoldRepresentation): Boolean;
begin
  if value = '' then
    result := CanSetToNull(nil)
  else
    result := CheckRange(StrToInt(trim(Value)));
end;

procedure TBAInteger.Assign(Source: TBoldElement);
begin
  if (Source is TBAInteger) then
  begin
    if TBAInteger(Source).IsNull then
      SetToNull
    else
      AsInteger := TBAInteger(Source).AsInteger;
  end
  else
    inherited;
end;

function TBAInteger.CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer;
begin
  if BoldElement is TBAInteger then
  begin
    if EitherIsNull(Self, TBoldAttribute(BoldElement)) then
      Result := NullSmallest(BoldElement)
    else
      case CompType of
        ctDefault:
          if AsInteger = TBAInteger(BoldElement).AsInteger then
            Result := 0
          else if AsInteger > TBAInteger(BoldElement).AsInteger then
            Result := 1
          else
            Result := -1;
      else
        Result := inherited CompareToAs(CompType, BoldElement);
      end;
  end else if BoldElement is TBANumeric then
  begin
    if EitherIsNull(Self, TBoldAttribute(BoldElement)) then
      Result := NullSmallest(BoldElement)
    else
      case CompType of
        ctDefault:
          if AsInteger = TBANumeric(BoldElement).AsFloat then
            Result := 0
          else if AsInteger > TBANumeric(BoldElement).AsFloat then
            Result := 1
          else
            Result := -1;
      else
        Result := inherited CompareToAs(CompType, BoldElement);
      end;
  end else
    Result := inherited CompareToAs(CompType, BoldElement);
end;

function TBAInteger.CanSetValue(Value: Integer; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetValue(Value, Subscriber) and
            SendQuery(bqMaySetValue, [Value], Subscriber);
end;

function TBAInteger.MaySetValue(NewValue: integer;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := CheckRange(NewValue);
end;

function TBAInteger.CheckRangeWithBounds(Value, Min,
  Max: integer): boolean;
begin
  if (Value < Min) or (Value > MAX) then
  begin
    result := false;
    SetBoldLastFailureReason(
      TBoldFailureReason.createFmt(
        sRangeError,
        [ClassName, Min, MAX, Value], self));
  end
  else
    result := true;
end;

{---TBAByte---}
function TBAByte.CheckRange(Value: integer): Boolean;
begin
  result := CheckRangeWithBounds(Value, Low(Byte), High(Byte));
end;

function TBAByte.GetAsByte: Byte;
begin
  result := AsInteger;
end;

procedure TBAByte.SetAsByte(const Value: Byte);
begin
  SetDataValue(Value);
end;

{---TBAShortint---}
function TBAShortInt.CheckRange(Value: integer): Boolean;
begin
  result := CheckRangeWithBounds(Value, Low(ShortInt), High(ShortInt));
end;

function TBAShortInt.GetAsShortInt: ShortInt;
begin
  result := AsInteger;
end;

procedure TBAShortInt.SetAsShortInt(const Value: ShortInt);
begin
  SetDataValue(Value);
end;

{---TBASmallint---}
function TBASmallInt.CheckRange(Value: integer): Boolean;
begin
  result := CheckRangeWithBounds(Value, Low(SmallInt), High(SmallInt));
end;

function TBASmallInt.GetAsSmallInt: SmallInt;
begin
  result := AsInteger;
end;

procedure TBASmallInt.SetAsSmallInt(const Value: SmallInt);
begin
  SetDataValue(value);
end;

{---TBAWord---}
function TBAWord.CheckRange(Value: integer): Boolean;
begin
  result := CheckRangeWithBounds(Value, Low(word), High(Word));
end;

function TBAWord.GetAsWord: Word;
begin
  result := AsInteger;
end;

procedure TBAWord.SetAsWord(const Value: Word);
begin
  SetDataValue(Value);
end;

procedure TBAInteger.AssignValue(Source: IBoldValue);
var
  s: IBoldIntegerContent;
begin
  if source.QueryInterface(IBoldIntegerContent, S) = S_OK then
    SetDataValue(s.AsInteger)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignValue]);
end;

procedure TBAInteger.SetContent(NewValue: Integer);
begin
  if (BoldPersistenceState = bvpsInvalid) or
    ContentIsNull or (FValue <> NewValue) then
  begin
    PreChange;
    FValue := NewValue;
    SetToNonNull;
    Changed(beValueChanged, [NewValue]);
  end;
end;

procedure TBAInteger_Proxy.SetContentAsInteger(NewValue: Integer);
begin
  ProxedInteger.SetContent(NewValue);
end;

function TBAInteger_Proxy.GetContentAsInteger: Integer;
begin
  result := ProxedInteger.fValue;
end;

procedure TBAInteger.AssignContentValue(Source: IBoldValue);
var
  s: IBoldIntegerContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
    SetContentToNull
  else if not assigned(source) then
    SetContent(0)
  else if source.QueryInterface(IBoldIntegerContent, S) = S_OK then
  begin
    if s.IsNull then
      SetContentToNull
    else
      SetContent(s.AsInteger)
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignContentValue]);
end;

function TBAInteger.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldIntegerContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldIntegerContent') // do not localize
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBAInteger.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBAInteger_Proxy;
end;

{ TBAFloat }

procedure TBAFloat.SetDataValue(NewValue: Double);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize

  if IsNull or (FValue <> NewValue) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize

    try
      SetContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBAFloat.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);
  if Representation = brDefault then
    if Value = '' then
      SetToNull
    else
      SetDataValue(StrToFloat(Value))
  else
    inherited SetStringRepresentation(Representation, Value);
end;

function TBAFloat.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, Meth_GetStringRepresentation, '');
  if Representation <> brDefault then
    inherited GetStringRepresentation(Representation);
  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Result := FloatToStr(GetAsFloat);
end;

function TBAFloat.GetAsFloat: Double;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsFloat', ''); // do not localize
  EnsureNotNull; {ensures current}
  Result := FValue;
end;

procedure TBAFloat.SetAsFloat(Value: Double);
begin
  SetDataValue(Value);
end;

procedure TBAFloat.SetAsInteger(Value: Integer);
begin
  SetAsfloat(Value);
end;

function TBAFloat.ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean;
begin
  Result := C in ['0'..'9', '-', '+', 'e', 'E', FormatSettings.DecimalSeparator];
end;

function TBAFloat.ValidateString(Value: string; Representation: TBoldRepresentation): Boolean;
var
  temp: Extended;
begin
  if value = '' then
    result := CanSetToNull(nil)
  else
  begin
    result := TextToFloat(PChar(Value), temp, fvExtended);
    if not result then
      FormatFailure(value, 'float'); // do not localize
  end;
end;

procedure TBAFloat.Assign(Source: TBoldElement);
begin
  if Source is TBANumeric then
  begin
    if TBANumeric(Source).IsNull then
      SetToNull
    else
      AsFloat := TBANumeric(Source).AsFloat
  end
  else
    inherited;
end;

function TBAFloat.CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer;
var
  CompareValue: Double;
begin
  if BoldElement is TBANumeric then
  begin
    if EitherIsNull(Self, TBoldAttribute(BoldElement)) then
      Result := NullSmallest(BoldElement)
    else
    begin
      CompareValue := TBANumeric(BoldElement).AsFloat;

      case CompType of
        ctDefault:
          if AsFloat = CompareValue then
            Result := 0
          else if AsFloat > CompareValue then
            Result := 1
          else
            Result := -1;
      else
        Result := inherited CompareToAs(CompType, BoldElement);
      end;
    end;
  end
  else
    Result := inherited CompareToAs(CompType, BoldElement);
end;

function TBAFloat.CanSetValue(NewValue: Double; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetValue(NewValue, Subscriber) and
            SendQuery(bqMaySetValue, [NewValue], Subscriber);
end;

function TBAFloat.MaySetValue(NewValue: Double;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := True;
end;

procedure TBAFloat.AssignValue(Source: IBoldValue);
var
  s: IBoldFloatContent;
begin
  if source.QueryInterface(IBoldFloatContent, S) = S_OK then
    SetDataValue(s.AsFloat)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignValue]);
end;

procedure TBAFloat.SetContent(NewValue: Double);
begin
  if ContentIsNull or (FValue <> NewValue) then
  begin
    PreChange;
    FValue := NewValue;
    SetToNonNull;
    Changed(beValueChanged, [NewValue]);
  end;
end;

function TBAFloat_Proxy.GetContentAsFloat: Double;
begin
  result := ProxedFloat.fValue;
end;

procedure TBAFloat_Proxy.SetContentAsFloat(NewValue: Double);
begin
  ProxedFloat.SetContent(NewValue);
end;

procedure TBAFloat.AssignContentValue(Source: IBoldValue);
var
  FloatContent: IBoldFloatContent;
  CurrencyContent: IBoldCurrencyContent;
  IntegerContent: IBoldIntegerContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
    SetContentToNull
  else if not assigned(source) then
    SetContent(0)
  else if source.QueryInterface(IBoldFloatContent, FloatContent) = S_OK  then
  begin
    if FloatContent.IsNull then
      SetContentToNull
    else
      SetContent(FloatContent.AsFloat)
  end
  else if source.QueryInterface(IBoldCurrencyContent, CurrencyContent) = S_OK then
  begin
    if CurrencyContent.IsNull then
      SetContentToNull
    else
      SetContent(CurrencyContent.asCurrency)
  end
  else if source.QueryInterface(IBoldIntegerContent, IntegerContent) = S_OK then
  begin
    if IntegerContent.IsNull then
      SetContentToNull
    else
      SetContent(IntegerContent.asInteger)
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignContentValue]);
end;

function TBAFloat.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldFloatContent) then
    Result := RetrieveProxyInterface(IBoldFloatContent, Mode, obj, 'IBoldFloatContent') // do not localize
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBAFloat.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBAFloat_Proxy;
end;

{ TBACurrency }

procedure TBACurrency.SetDataValue(NewValue: Currency);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize

  if IsNull or (FValue <> NewValue) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize
    try
      SetContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBACurrency.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);
  if Representation = brDefault then
  begin
    if Value = '' then
      SetToNull
    else
    begin
      if Value[Length(Value)] = FormatSettings.DecimalSeparator then
        Value := Concat(Value, '0');
      SetDataValue(StrToCurr(Value));
    end;
  end else
    inherited SetStringRepresentation(Representation, Value);
end;

function TBACurrency.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, Meth_GetStringRepresentation, '');
  if Representation <> brDefault then
    inherited GetStringRepresentation(Representation);
  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Result := CurrToStr(GetAsCurrency);
end;

procedure TBACurrency.SetAsCurrency(Value: Currency);
begin
  SetDataValue(Value);
end;

function TBACurrency.GetAsCurrency: Currency;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsCurrency', ''); // do not localize
  EnsureNotNull; {ensures current}
  Result := FValue;
end;

function TBACurrency.GetAsFloat: Double;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsFloat', ''); // do not localize
  EnsureNotNull; {ensures current}
  Result := FValue;
end;

procedure TBACurrency.SetAsFloat(Value: Double);
begin
  SetDataValue(Value);
end;

procedure TBACurrency.SetAsInteger(Value: integer);
begin
  SetAsFloat(Value);
end;

function TBACurrency.ValidateString(Value: string; Representation: TBoldRepresentation): Boolean;
var
  Temp: Currency;
begin
  if value = '' then
    result := CanSetToNull(nil)
  else
  begin
    result := TextToFloat(PChar(Value), temp, fvCurrency);
    if not result then
      FormatFailure(value, 'currency'); // do not localize
  end;
end;

function TBACurrency.ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean;
begin
  Result := C in ['0'..'9', '-', '+', 'e', 'E', FormatSettings.DecimalSeparator];
end;

procedure TBACurrency.Assign(Source: TBoldElement);
begin
  if (Source is TBANumeric) then
  begin
    if TBANumeric(Source).IsNull then
      SetToNull
    else if Source is TBACurrency then
      AsCurrency := TBACurrency(Source).AsCurrency
    else if Source is TBANumeric then
      AsCurrency := TBANumeric(Source).AsFloat;
  end
  else
    inherited;
end;

function TBACurrency.CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer;
var
  CompareValue: Currency;
begin
  if BoldElement is TBANumeric then
  begin
    if EitherIsNull(Self, TBoldAttribute(BoldElement)) then
      Result := NullSmallest(BoldElement)
    else
    begin
      if BoldElement is TBACurrency then
        CompareValue := TBACurrency(BoldElement).AsCurrency
      else
        CompareValue := TBANumeric(BoldElement).AsFloat;

      case CompType of
        ctDefault:          if AsCurrency = CompareValue then
            Result := 0
          else if AsCurrency > CompareValue then
            Result := 1
          else
            Result := -1;
      else
        Result := inherited CompareToAs(CompType, BoldElement);
      end;
    end;
  end
  else
    Result := inherited CompareToAs(CompType, BoldElement);
end;

function TBACurrency.CanSetValue(NewValue: Currency; Subscriber: TBoldSubscriber): Boolean;
begin
  Result := MaySetValue(NewValue, Subscriber) and
            SendQuery(bqMaySetValue, [NewValue], Subscriber);
end;

function TBACurrency.MaySetValue(NewValue: Currency;
  Subscriber: TBoldSubscriber): Boolean;
begin
  Result := True;
end;

procedure TBACurrency.AssignValue(Source: IBoldValue);
var
  s: IBoldCurrencyContent;
begin
  if source.QueryInterface(IBoldCurrencyContent, S) = S_OK then
    SetDataValue(s.AsCurrency)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignValue]);
end;

procedure TBACurrency.SetContent(NewValue: Currency);
begin
  if ContentIsNull or (FValue <> NewValue) then
  begin
    PreChange;
    FValue := NewValue;
    SetToNonNull;
    Changed(beValueChanged, [NewValue]);
  end;
end;

procedure TBACurrency_Proxy.SetContentAsCurrency(NewValue: Currency);
begin
  ProxedCurrency.SetContent(NewValue);
end;

function TBACurrency_Proxy.GetContentAsCurrency: Currency;
begin
  result := ProxedCurrency.fValue;
end;

procedure TBACurrency.AssignContentValue(Source: IBoldValue);
var
  s: IBoldCurrencyContent;
  FloatContent: IBoldFloatContent;
  IntegerContent: IBoldIntegerContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
    SetContentToNull
  else if not assigned(source) then
    SetContent(0)
  else if (source.QueryInterface(IBoldCurrencyContent, S) = S_OK) then
  begin
    if s.IsNull then
      SetContentToNull
    else
      SetContent(s.AsCurrency)
  end
  else if (source.QueryInterface(IBoldFloatContent, FloatContent) = S_OK) then
  begin
    if FloatContent.IsNull then
      SetContentToNull
    else
      SetContent(FloatContent.AsFloat)
  end
  else if (source.QueryInterface(IBoldIntegerContent, IntegerContent) = S_OK) then
  begin
    if IntegerContent.IsNull then
      SetContentToNull
    else
      SetContent(IntegerContent.AsInteger)
  end  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignContentValue]);
end;

function TBACurrency.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldCurrencyContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldCurrencyContent') // do not localize
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBACurrency.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBACurrency_Proxy;
end;

{ TBoldBlobStream }

constructor TBoldBlobStream.Create(BlobAttr: TBABlob; Mode: TBoldBlobStreamMode);
begin
  inherited Create;
  fBlobAttr := BlobAttr;
  fMode := Mode;
  if (Mode in [bmReadWrite, bmWrite]) and not BlobAttr.Mutable then
    BlobAttr.MutableError('Binary Large Object'); //FIXME Better representation of BLOBs!!! // do not localize
  if Mode in [bmRead, bmReadWrite] then
    fBlobAttr.EnsureNotNull;
  if (Mode = bmWrite) then
    Truncate;
end;

function TBoldBlobStream.GetBlobSize: Integer;
begin
  Result := Length(fBlobAttr.fValue);
end;

function TBoldBlobStream.Read(var Buffer; Count: integer): integer;
begin
  if Count > Size - fPosition then
    Result := Size - fPosition else
    Result := Count;
  if Result > 0 then
  begin
    Move(PChar(fBlobAttr.fValue)[fPosition], Buffer, Result);
    Inc(FPosition, Result);
  end;
end;

function TBoldBlobStream.Write(const Buffer; Count: integer): integer;
var
  EndPos: integer;
begin
  result := 0;
  if (fPosition >= 0) and (Count >= 0) then
  begin
    EndPos := fPosition + Count;
    if EndPos > 0 then
    begin
      try
        StartModifyOfBlob('Write'); // do not localize
        if EndPos > Size then
        begin
          SetLength(fBlobAttr.fValue, EndPos);
        end;
        System.Move(Buffer, PChar(fBlobAttr.fValue)[fPosition], Count);
        fPosition := EndPos;
        Result := Count;
        EndModifyOfBlob;
      except
        FailModifyOfBlob;
      end;
    end;
  end;
end;

function TBoldBlobStream.Seek(Offset: integer; Origin: Word): integer;
begin
  case Origin of
    0: FPosition := Offset;
    1: Inc(FPosition, Offset);
    2: FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

procedure TBoldBlobStream.Clear;
begin
  SetSize(0);
end;

procedure TBoldBlobStream.Truncate;
begin
  SetSize(fPosition);
end;

procedure TBoldBlobStream.LoadFromStream(Stream: TStream);
var
  Count: Integer;
begin
  Stream.Position := 0;
  Count := Stream.Size;
  InternalSetSize(Count, Count = 0); // dont send any events if we are going to set the stream-value below
  if Count <> 0 then
  begin
    try
      StartModifyOfBlob('LoadFromStream'); // do not localize
      Stream.ReadBuffer(PChar(fBlobAttr.fValue)[0], Count);
      EndModifyOfBlob;
    except
      FailModifyOfBlob;
    end;
  end;
end;

procedure TBoldBlobStream.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBoldBlobStream.SaveToStream(Stream: TStream);
begin
  if Size <> 0 then
    Stream.WriteBuffer(PChar(fBlobAttr.fValue)[0], Size);
end;

procedure TBoldBlobStream.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBoldBlobStream.SetSize(NewSize: integer);
begin
  InternalSetSize(NewSize, true);
end;

procedure TBoldBlobStream.EndModifyOfBlob;
begin
  // we can not call SetToNull since that will cause recursive Modification. that must be done by InternalSetSize
  if (Size<>0) then
    fBlobAttr.SetToNonNull;
  fBlobAttr.Changed(beValueChanged, [fBlobAttr.FValue]);
  fBlobAttr.EndModify;
end;

procedure TBoldBlobStream.FailModifyOfBlob;
begin
  fBlobAttr.FailModify;
end;

procedure TBoldBlobStream.StartModifyOfBlob(Operation: String);
begin
  if not fBlobAttr.StartModify then
      BoldRaiseLastFailure(fBlobAttr, 'BlobStream: ' + Operation, ''); // do not localize
  fBlobAttr.PreChange;
end;

procedure TBoldBlobStream.InternalSetSize(NewSize: Integer;  BlobEvents: Boolean);
var
  OldPosition: Integer;
begin
  OldPosition := fPosition;
  if (NewSize = 0) and
    (not assigned(fBlobAttr.BoldAttributeRTInfo) or fBlobAttr.BoldAttributeRTInfo.AllowNull) then
    fBlobAttr.SetToNull
  else
    try
      if BlobEvents then
        StartModifyOfBlob('SetSize'); // do not localize
      SetLength(fBlobAttr.fValue, NewSize);
      if BlobEvents then
        EndModifyOfBlob;
    except
      if BlobEvents then
        FailModifyOfBlob;
    end;
  if OldPosition > NewSize then
    Seek(0, soFromEnd);

end;

{ TBABlob }
procedure TBABlob.SetDataValue(NewValue: string);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize

  if IsNull or (FValue <> NewValue) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize
    try
      SetContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBABlob.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);
  case Representation of
    brDefault: SetDataValue(Value);
    brShort: {Content type is lost when assigned to a Blob};
  else
    inherited SetStringRepresentation(Representation, Value);
  end;
end;

function TBABlob.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, Meth_GetStringRepresentation, '');
  case Representation of
    brDefault: begin
      if IsNull then {IsNull ensures current}
        Result := ''
      else
        Result := FValue;
    end;
    brShort: begin
      Result := '';
    end;
  else
    Result := inherited GetStringRepresentation(Representation);
  end;
end;

function TBABlob.GetAsBlob: String;
begin
  result := GetStringRepresentation(brDefault);
end;

procedure TBABlob.SetAsBlob(NewValue: String);
begin
  SetStringRepresentation(brDefault, NewValue);
end;

procedure TBABlob.SetToNull;
begin
  inherited;
  //	save memory by "disposing" of blob data
  FValue := '';
end;

procedure TBABlob.Assign(Source: TBoldElement);
begin
  if (Source is TBABlob) then
  begin
    if TBABlob(Source).IsNull then
      SetToNull
    else
    begin
      AsString := TBABlob(Source).AsString;
      ContentType := TBABlob(Source).ContentType;
    end;
    Exit;
  end
  else
    inherited;
end;

function TBABlob.CreateBlobStream(Mode: TBoldBlobStreamMode): TBoldBlobStream;
begin
  Result := TBoldBlobStream.Create(Self, Mode);
end;

function TBABlob.CanSetValue(NewValue: string;  Subscriber: TBoldSubscriber): Boolean;
begin
  Result := MaySetValue(NewValue, Subscriber) and
            SendQuery(bqMaySetValue, [NewValue], Subscriber);
end;

function TBABlob.MaySetValue(NewValue: String;
  Subscriber: TBoldSubscriber): Boolean;
begin
  Result := True;
end;

procedure TBABlob.AssignValue(Source: IBoldValue);
var
  s: IBoldBlobContent;
begin
  if source.QueryInterface(IBoldBlobContent, S) = S_OK then
    SetDataValue(s.AsBlob)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignValue]);
end;

procedure TBABlob.SetContent(NewValue: string);
begin
  if ContentIsNull or (FValue <> NewValue) then
  begin
    PreChange;
    FValue := NewValue;
    SetToNonNull;
    Changed(beValueChanged, [NewValue]);
  end;
end;

function TBABlob_Proxy.GetContentAsBlob: string;
begin
  result := ProxedBlob.fValue;
end;

procedure TBABlob_Proxy.SetContentAsBlob(const NewValue: string);
begin
  ProxedBlob.SetContent(NewValue);
end;

procedure TBABlob.AssignContentValue(Source: IBoldValue);
var
  s: IBoldBlobContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
    SetContentToNull
  else if not assigned(source) then
    SetContent('')
  else if source.QueryInterface(IBoldBlobContent, S) = S_OK then
  begin
    if s.IsNull then
      SetContentToNull
    else
      SetContent(s.AsBlob)
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignContentValue]);
end;

function TBABlob.CompareToAs(CompareType: TBoldCompareType;
  BoldElement: TBoldElement): Integer;
var
  CompareBlob: TBABlob;
begin
  if BoldElement is TBABlob then
  begin
    CompareBlob := BoldElement as TBABlob;
    if EitherIsNull(Self, CompareBlob) then
      Result := NullSmallest(CompareBlob)
    else
    begin
      case CompareType of
        ctDefault, ctAsString:
          // will this really compare binary values with #0 inside? /joho
          Result := CompareStr(fValue, CompareBlob.fValue);
        ctAsText:
          Result := CompareText(fValue, CompareBlob.fValue);
        ctAsAnsiString:
          Result := AnsiCompareStr(fValue, CompareBlob.fValue);
        ctAsAnsiText:
          Result := AnsiCompareText(fValue, CompareBlob.fValue);
        else
          result := inherited CompareToAs(CompareType, BoldElement);
      end;
    end;
  end
  else
    Result := inherited CompareToAs(CompareType, BoldElement);
end;

function TBABlob.IsEqualAs(CompareType: TBoldCompareType;
  BoldElement: TBoldElement): Boolean;
var
  CompareBlob: TBABlob;
begin
  if BoldElement is TBABlob then
  begin
    CompareBlob := BoldElement as TBABlob;
    if EitherIsNull(self, CompareBlob) then
      result := CompareBlob.IsNull and IsNull
    else
      result := CompareBlob.fValue = fValue; // will this really compare binary values with #0 inside? /joho
  end
  else
    Result := inherited IsEqualAs(CompareType, BoldElement);
end;

function TBABlob.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldBlobContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldBlobContent') // do not localize
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBABlob.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBABlob_Proxy;
end;

procedure TBABlob.FreeContent;
begin
  inherited;
  FValue := '';
end;

procedure TBABlob.SetEmptyValue;
begin
  asString := '';
end;

{ TBATypedBlob }
procedure TBATypedBlob.SetContentType2(Value: string);
begin
  BoldClearLastFailure;
  if not CanSetContentType(Value, nil) then
    BoldRaiseLastFailure(self, 'SetContentType', ''); // do not localize

  if ContentIsNull or (FContentType <> Value) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetContentType', ''); // do not localize
    try
      SetContentTypeContent(Value);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end
end;

function TBATypedBlob.GetContentTypeContent: String;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetContentType', ''); // do not localize
  result := ContentType;
end;

function TBATypedBlob.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  case Representation of
    brShort: begin
      if not CanRead(nil) then
        BoldRaiseLastFailure(self, Meth_GetStringRepresentation, '');
      Result := FContentType;
    end;
  else
    Result := inherited GetStringRepresentation(Representation);
  end;
end;

procedure TBATypedBlob.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  case Representation of
    brShort: begin
      if not ValidateString(Value, Representation) then
        BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);
      SetContentType2(Value);
    end;
  else
    inherited SetStringRepresentation(Representation, Value);
  end;
end;

procedure TBATypedBlob.SetToNull;
begin
  inherited;
  FContentType := '';
end;

function TBATypedBlob.CanSetContentType(Value: string; Subscriber: TBoldSubscriber): Boolean;
begin
  Result := SendQuery(bqMaySetContentType, [Value], Subscriber);
end;

procedure TBATypedBlob.AssignValue(Source: IBoldValue);
var
  s: IBoldTypedBlob;
  s2: IBoldBlobContent;
begin
  if (source.QueryInterface(IBoldTypedBlob, S) = S_OK) and
   (source.QueryInterface(IBoldBlobContent, S2) = S_OK)
  then
  begin
    FContentType := s.ContentTypeContent;
    SetDataValue(s2.AsBlob);
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignValue]);
end;

function TBATypedBlob.CompareToAs(CompareType: TBoldCompareType;
  BoldElement: TBoldElement): Integer;
begin
  if Boldelement is TBATypedBlob then
  begin
    result := CompareStr((Boldelement as TBATypedBlob).ContentType, ContentType);
    if result = 0 then
      result := inherited CompareToAs(CompareType, BoldElement)
  end
  else
    result := inherited CompareToAs(CompareType, BoldElement);
end;

function TBATypedBlob.IsEqualAs(CompareType: TBoldCompareType;
  BoldElement: TBoldElement): Boolean;
begin
  if Boldelement is TBATypedBlob then
  begin
    result := ((Boldelement as TBATypedBlob).ContentType = ContentType) and
        inherited IsEqualAs(CompareType, BoldElement);
  end
  else
    result := inherited IsEqualAs(CompareType, BoldElement);
end;

{-- TBABlobImageJPEG --}
procedure TBABlobImageJPEG.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  case Representation of
    brShort: begin
      if (Value <> '') and (Value <> ContentType) then
        raise EBold.CreateFmt(sCannotAssignXtoY, [ClassName, Value, ContentType]);
    end;
  else
    inherited SetStringRepresentation(Representation, Value);
  end;
end;

function TBABlobImageJPEG.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  case Representation of
    brShort: begin
      Result := 'image/jpeg'; // do not localize
    end;
  else
    inherited GetStringRepresentation(Representation);
  end;
end;

{-- TBABlobImageBMP --}
procedure TBABlobImageBMP.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  case Representation of
    brShort: begin
      if (Value <> '') and (Value <> ContentType) then
        raise EBold.CreateFmt(sCannotAssignXtoY, [ClassName, Value, ContentType]);
    end;
  else
    inherited SetStringRepresentation(Representation, Value);
  end;
end;

function TBABlobImageBMP.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  case Representation of
    brShort: begin
      Result := 'image/bitmap'; // do not localize
    end;
  else
    inherited GetStringRepresentation(Representation);
  end;
end;

function TBATypedBlob.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldTypedBlob) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldTypedBlob') // do not localize
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBATypedBlob.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBATypedBlob_Proxy;
end;

procedure TBATypedBlob.SetContentTypeContent(NewValue: String);
begin
  PreChange;
  FContentType := NewValue;
  SetToNonNull;
  Changed(beValueChanged, [NewValue]);
end;

{ TBAMoment }

procedure TBAMoment.SetDataValue(NewValue: TDateTime);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize

  if IsNull or (FValue <> NewValue) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize
    try
      SetContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

function TBAMoment.CanSetValue(NewValue: TDateTime; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetValue(NewValue, Subscriber) and
            SendQuery(bqMaySetValue, [NewValue], Subscriber);
end;

function TBAMoment.MaySetValue(NewValue: TDateTime;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := True;
end;

function TBAMoment.GetAsDateTime: TDateTime;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsDateTime', ''); // do not localize
  EnsureNotNull; {ensures current}
  Result := FValue;
end;

function TBAMoment.GetAsDate: TDateTime;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsDate', ''); // do not localize
  EnsureNotNull; {ensures current}
  Result := Int(fValue);;
end;

function TBAMoment.GetAsTime: TDateTime;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsTime', ''); // do not localize
  EnsureNotNull; {ensures current}
  Result := frac(fValue);
end;

procedure TBAMoment.SetAsDateTime(Value: TDateTime);
begin
  SetDataValue(Value);
end;

procedure TBAMoment.SetAsDate(Value: TDateTime);
begin
  if IsNull then
    SetAsDateTime(Int(Value))
  else
    SetAsDateTime(Int(Value) + AsTime);
end;

procedure TBAMoment.SetAsTime(Value: TDateTime);
begin
  if IsNull then
    SetAsDateTime(frac(Value))
  else
    SetAsDateTime(frac(Value) + AsDate);
end;

procedure TBAMoment.Assign(Source: TBoldElement);
begin
  if (Source is TBAMoment) then
  begin
    if TBAMoment(Source).IsNull then
      SetToNull
    else
      SetAsDateTime(TBAMoment(Source).GetAsDateTime);
  end
  else
    inherited;
end;

function TBAMoment.CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer;
var
  MyValue,
  CompareValue: TDateTime;
begin
  if BoldElement is TBAMoment then
  begin
    if EitherIsNull(Self, TBoldAttribute(BoldElement)) then
      Result := NullSmallest(BoldElement)
    else
    begin
      case CompType of
        ctAsDate: begin
          MyValue := GetAsDate;
          CompareValue := TBAMoment(BoldElement).GetAsDate;
        end;
        ctAsTime: begin
          MyValue := GetAsTime;
          CompareValue := TBAMoment(BoldElement).GetAsTime;
        end;
      else
      begin
        MyValue := GetAsDateTime;
        CompareValue := TBAMoment(BoldElement).GetAsDateTime;
      end;
    end;
    if CompType in [ctDefault, ctAsTime, ctAsDate] then
    begin
      if MyValue = CompareValue then
        Result := 0
      else if MyValue > CompareValue then
        Result := 1
      else
      begin
        Result := -1;
      end
    end else
      Result := inherited CompareToAs(CompType, BoldElement);
    end;
  end else
    Result := inherited CompareToAs(CompType, BoldElement);
end;

procedure TBAMoment.SetContent(NewValue: TDateTime);
begin
  if ContentIsNull or (FValue <> NewValue) then
  begin
    PreChange;
    FValue := NewValue;
    SetToNonNull;
    Changed(beValueChanged, [NewValue]);
  end;
end;

function TBAMoment_Proxy.GetContentAsDate: TDateTime;
begin
  result := Int(ProxedMoment.fValue);
end;

procedure TBAMoment_Proxy.SetContentAsDate(NewValue: TDateTime);
begin
  ProxedMoment.SetContent(Int(NewValue) + GetContentAsTime);
end;

function TBAMoment_Proxy.GetContentAsTime: TDateTime;
begin
  result := frac(ProxedMoment.fValue);
end;

function TBAMoment_Proxy.GetContentAsDateTime: TDateTime;
begin
  result := ProxedMoment.fValue;
end;

procedure TBAMoment_Proxy.SetContentAsDateTime(NewValue: TDateTime);
begin
  ProxedMoment.SetContent(NewValue);
end;

procedure TBAMoment_Proxy.SetContentAsTime(NewValue: TDateTime);
begin
  ProxedMoment.SetContent(frac(NewValue) + GetContentAsDate);
end;

procedure TBAMoment.SetEmptyValue;
begin
  SetAsDateTime(0);
end;

function TBAMoment.GetDays: Word;
var Year, Month, Day: Word;
begin
  DecodeDate(AsDate, Year, Month, Day);
  Result := Day;
end;

function TBAMoment.GetHours: Word;
var Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(AsTime, Hour, Min, Sec, MSec);
  Result := Hour;
end;

function TBAMoment.GetMinutes: Word;
var Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(AsTime, Hour, Min, Sec, MSec);
  Result := Min;
end;

function TBAMoment.GetMonths: Word;
var Year, Month, Day: Word;
begin
  DecodeDate(AsDate, Year, Month, Day);
  Result := Month;
end;

function TBAMoment.GetSeconds: Word;
var Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(AsTime, Hour, Min, Sec, MSec);
  Result := Sec;
end;

function TBAMoment.GetYears: Word;
var Year, Month, Day: Word;
begin
  DecodeDate(AsDate, Year, Month, Day);
  Result := Year;
end;

{ TBADateTime }

procedure TBADateTime.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);
  if Representation = brDefault then
    //	FIXME: What if only date supplied?
    if Value = '' then
      SetToNull
    else if upperCase(Value) = DEFAULTNOW then
      SetDataValue(now)
    else
      SetDataValue(StrToDateTime(Value))
  else
    inherited SetStringRepresentation(Representation, Value);
end;

function TBADateTime.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  if Representation <> brDefault then
    inherited GetStringRepresentation(Representation);
  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Result := DateTimeToStr(GetAsDateTime);
end;

function TBADateTime.ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean;
begin
  Result := C in ['0'..'9', ' ', FormatSettings.TimeSeparator, FormatSettings.DateSeparator];
end;

function TBADateTime.ValidateString(Value: string; Representation: TBoldRepresentation): Boolean;
begin
  try
    if value = '' then
      result := CanSetToNull(nil)
    else if upperCase(Value) = DEFAULTNOW then
      result := true
    else
    begin
      StrToDateTime(Value);
      Result := True;
    end;
  except
    Result := False;
    FormatFailure(value, 'datetime'); // do not localize
  end;
end;

{---TBADate---}
procedure TBADate.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);
  if Representation = brDefault then
    if Value = '' then
      SetToNull
    else if upperCase(Value) = DEFAULTNOW then
      SetDataValue(now)
    else
      try
        SetDataValue(StrToDate(Value));
      except
      end
  else
    inherited SetStringRepresentation(Representation, Value);
end;

function TBADate.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  if Representation <> brDefault then
    inherited GetStringRepresentation(Representation);
  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Result := DateToStr(GetAsDateTime);
end;

function TBADate.ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean;
begin
  Result := C in ['0'..'9', FormatSettings.DateSeparator];
end;

function TBADate.ValidateString(Value: string; Representation: TBoldRepresentation): Boolean;
begin
  try
    if value = '' then
      result := CanSetToNull(nil)
    else if upperCase(Value) = DEFAULTNOW then
      result := true
    else
    begin
      StrToDate(Value);
      Result := True;
    end;
  except
    Result := False;
    FormatFailure(value, 'date'); // do not localize
  end;
end;

{---TBATime---}
procedure TBATime.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);
  if Representation = brDefault then
    if Value = '' then
      SetToNull
    else if upperCase(Value) = DEFAULTNOW then
      SetDataValue(now)
    else
      SetDataValue(StrToTime(Value))
  else
    inherited SetStringRepresentation(Representation, Value);
end;

function TBATime.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  if Representation <> brDefault then
    inherited GetStringRepresentation(Representation);
  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Result := TimeToStr(GetAsDateTime);
end;

function TBATime.ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean;
begin
  Result := C in ['0'..'9', FormatSettings.TimeSeparator];
end;

function TBATime.ValidateString(Value: string; Representation: TBoldRepresentation): Boolean;
begin
  try
    if value = '' then
      result := CanSetToNull(nil)
    else if upperCase(Value) = DEFAULTNOW then
      result := true
    else
    begin
      StrToTime(Value);
      Result := True;
    end;
  except
    Result := False;
    FormatFailure(value, 'time'); // do not localize
  end;
end;

{---TBAValueSetValue---}
constructor TBAValueSetValue.InternalCreate(StringValues: Integer; List: TBAValueSetValueList);
var
  I: Integer;
begin
  inherited Create;
  FStringRepresentations := TStringList.Create;
  for I := 0 to StringValues do
    FStringRepresentations.Add('');
  List.AddValue(Self);
end;

destructor TBAValueSetValue.Destroy;
begin
  FreeAndNil(FStringRepresentations);
  inherited;
end;

function TBAValueSetValue.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  Result := FStringRepresentations.Strings[Representation];
end;

procedure TBAValueSetValue.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
begin
  EnsureValidString(Value, Representation);
  FStringRepresentations.Strings[Representation] := Value;
end;

function TBAValueSetValue.GetAsInteger: Integer;
begin
  Result := FAsInteger;
end;

procedure TBAValueSetValue.SetAsInteger(Value: Integer);
begin
  FAsInteger := Value;
end;

procedure TBAValueSetValue.AddString(Value: string);
begin
  FStringRepresentations.Add(Value);
end;

function TBAValueSetValue.GetStringRepresentationCount: Integer;
begin
  Result := FStringRepresentations.Count;
end;

procedure TBAValueSetValue.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate);
begin
  if mutable then
    AddSmallSubscription(Subscriber, [beItemAdded, beItemDeleted, beItemReplaced, beOrderChanged, beValueInvalid], requestedEvent);
end;

procedure TBAValueSetValue.GetAsList(ResultList: TBoldIndirectElement);
begin
  ResultList.SetReferenceValue(Self);
end;

{---TBAValueSetValueList---}
constructor TBAValueSetValueList.Create;
begin
  inherited;
  FValueList := TList.Create;
end;

destructor TBAValueSetValueList.Destroy;
var
  I: Integer;
begin
  for I := 0 to FValueList.Count - 1 do
    TBAValueSetValue(FValueList[I]).Free;
  FreeAndNil(FValueList);
  inherited;
end;

procedure TBAValueSetValueList.AddValue(Value: TBAValueSetValue);
begin
  FValueList.Add(Value);
end;

procedure TBAValueSetValueList.Add(intvalue: Integer; arrString: array of string);
var
  RepresentationIdx,
  ArrSize: Integer;
begin
  ArrSize := High(arrString) + 1;
  with TBAValueSetValue.InternalCreate(ArrSize, Self) do
  begin
    AsInteger := intvalue;
    for RepresentationIdx := 0 to ArrSize - 1 do
      StringRepresentation[RepresentationIdx + 1] := arrString[RepresentationIdx  ];
  end;
end;

function TBAValueSetValueList.FindByInteger(Value: Integer): TBAValueSetValue;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FValueList.Count - 1 do
    if TBAValueSetValue(FValueList.Items[I]).AsInteger = Value
      then Result := FValueList.Items[I];
end;

function TBAValueSetValueList.GetFirstValue: TBAValueSetValue;
begin
  if FValueList.Count = 0 then
    Result := nil
  else
    Result := FValueList.Items[0];
end;

function TBAValueSetValueList.FindByString(Representation: TBoldRepresentation; Value: string): TBAValueSetValue;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FValueList.Count - 1 do
    if CompareText(TBAValueSetValue(FValueList.Items[I]).StringRepresentation[Representation], Value) = 0 then
    begin
      Result := FValueList.Items[I];
      break;
    end;
end;

function TBAValueSetValueList.FindByText(Representation: TBoldRepresentation; Value: string): TBAValueSetValue;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FValueList.Count - 1 do
    if AnsiCompareText(TBAValueSetValue(FValueList.Items[I]).StringRepresentation[Representation], Value) = 0 then
    begin
      Result := FValueList.Items[I];
      break;
    end;
end;

procedure TBAValueSetValueList.ToStrings(Representation: TBoldRepresentation; theStrings: TStrings);
var
  I: Integer;
begin
  for I := 0 to FValueList.Count - 1 do
    theStrings.AddObject(TBAValueSetValue(FValueList.Items[I]).StringRepresentation[Representation], FValueList.Items[I]);
end;

procedure TBAValueSetValueList.ToStringsWithNil
    (Representation: TBoldRepresentation; theStrings: TStrings; nilstring: string);
begin
  theStrings.AddObject(nilstring, nil);
  ToStrings(Representation, theStrings);
end;


procedure TBADateTime.AssignValue(Source: IBoldValue);
var
  s: IBoldDateTimeContent;
begin
  if source.QueryInterface(IBoldDateTimeContent, S) = S_OK then
    SetDataValue(s.AsDateTime)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignValue]);
end;

procedure TBADateTime.AssignContentValue(Source: IBoldValue);
var
  s: IBoldDateTimeContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
    SetContentToNull
  else if not assigned(source) then
    SetContent(0)
  else if source.QueryInterface(IBoldDateTimeContent, S) = S_OK then
  begin
    if s.IsNull then
      SetContentToNull
    else
      SetContent(s.AsDateTime)
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignContentValue]);
end;

function TBADateTime.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldDateTimeContent) or
     IsEqualGuid(IID, IBoldDateContent) or
     IsEqualGuid(IID, IBoldTimeContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBold[Date][Time]Content') // do not localize
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBADateTime.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBADateTime_Proxy;
end;

{ TBAValueSet }

procedure TBAValueSet.SetDataValue(NewValue: TBAValueSetValue);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize
  if not Assigned(NewValue) then
    raise EBold.Create(sInvalidValue);

  if IsNull or (FValue <> NewValue) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize
    try
      SetContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

function TBAValueSet.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  CheckIllegalValue;
  if IsNull then {IsNull ensures current} {IsNull checks MayRead}
    Result := '' //FIXME: raise Exception?
  else
    Result := FValue.StringRepresentation[Representation];
end;

procedure TBAValueSet.SetStringRepresentation(Representation: TBoldRepresentation; Value: string);
var
  ValueSetValue: TBAValueSetValue;
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);
  ValueSetValue := Values.FindByString(Representation, Value);
  assert(assigned(ValueSetValue), sCannotFindValueSetValue);
  SetDataValue(ValueSetValue)
end;

function TBAValueSet.GetAsInteger: Integer;
begin
  BoldClearLastFailure;
  CheckIllegalValue;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsInteger', ''); // do not localize
  EnsureNotNull; {ensures current}
  Result := FValue.AsInteger;
end;

procedure TBAValueSet.SetAsInteger(Value: Integer);
begin
  assert(Assigned(Self), 'TBAValueSet.SetAsInteger: Self = nil');
  SetDataValue(Values.FindByInteger(Value));
end;

function TBAValueSet.ValidateString(Value: string; Representation: TBoldRepresentation): Boolean;
var
  ValueSetvalue: TBAValueSetValue;
  strList: TStringList;
  Str: String;
begin
  ValueSetValue := Values.FindByString(Representation, Value);
  Result := Assigned(ValueSetvalue);

  if not Result then
  begin
    strList := TStringList.create;
    try
      Values.ToStrings(Representation, strList);
      Str := BoldSeparateStringList(StrList, ', ', '(', ')');
      SetBoldLastFailureReason(TBoldFailureReason.CreateFmt(sUnknownStringValue, [Value, Str], self));
    finally
      StrList.Free;
    end;
  end;
end;

function TBAValueSet.ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean;
var
  i: integer;
  Str: String;
begin
  Str := '';
  for i := 0 to Values.Count - 1 do
    str := str + values[i].StringRepresentation[Representation];
  result := pos(c, str) <> 0;
end;


procedure TBAValueSet.Assign(Source: TBoldElement);
begin
  if (Source is TBAValueSet) then
  begin
    if TBAValueSet(Source).IsNull then
      SetToNull
    else
      AsInteger := TBAValueSet(Source).AsInteger;
  end
  else
    inherited;
end;

function TBAValueSet.CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer;
begin
  if BoldElement is TBAValueSet then
  begin
    if EitherIsNull(Self, TBoldAttribute(BoldElement)) then
      Result := NullSmallest(BoldElement)
    else
      case CompType of
        ctDefault:
          if AsInteger = TBAValueSet(BoldElement).AsInteger then
            Result := 0
          else if AsInteger > TBAValueSet(BoldElement).AsInteger then
            Result := 1
          else
            Result := -1;
      else
        Result := inherited CompareToAs(CompType, BoldElement);
      end;
  end
  else
    Result := inherited CompareToAs(CompType, BoldElement);
end;

function TBAValueSet.CanSetValue(NewValue: TBAValueSetValue; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetValue(NewValue, Subscriber) and
            SendQuery(bqMaySetValue, [NewValue], Subscriber);
end;

function TBAValueSet.MaySetValue(NewValue: TBAValueSetValue;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := True;
end;

procedure TBAValueSet.InitializeMember(OwningElement: TBoldDomainElement;
  ElementTypeInfo: TBoldElementTypeInfo);
begin
  inherited;
  if not Assigned(Values) then
    raise EBold.CreateFmt(sValuesNotInitialized, [ClassName]);
  if assigned(values.FValueList) and (values.fValueList.count <> 0) then
    FValue := Values.GetFirstValue
  else
    setToNull;
end;

procedure TBAValueSet.AssignValue(Source: IBoldValue);
var
  s: IBoldIntegerContent;
begin
  if source.QueryInterface(IBoldIntegerContent, S) = S_OK then
    SetDataValue(Values.FindByInteger(s.AsInteger))
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignValue]);
end;

procedure TBAValueSet.SetContent(NewValue: TBAValueSetValue);
begin
  if ContentIsNull or (FValue <> NewValue) then
  begin
    PreChange;
    FValue := NewValue;
    SetToNonNull;
    Changed(beValueChanged, [NewValue]);
  end;
end;

function TBAValueSet_Proxy.GetContentAsString: string;
begin
  result := ProxedValueSet.fValue.StringRepresentation[brDefault];
end;

procedure TBAValueSet_Proxy.SetContentAsString(const NewValue: String);
var
  ValueSetValue: TBAValueSetValue;
begin
  ValueSetValue := ProxedValueSet.Values.FindByString(brDefault, NewValue);
  if assigned(ValueSetValue) then
    ProxedValueSet.SetContent(ValueSetValue);
end;

function TBAValueSet_Proxy.GetContentAsInteger: Integer;
begin
  result := ProxedValueSet.GetContentAsInteger;
end;

function TBAValueSet.GetContentAsInteger: Integer;
begin
  result := FValue.AsInteger;
end;

procedure TBAValueSet.SetContentAsInteger(NewValue: Integer);
begin
  SetContent(Values.FindByInteger(NewValue));
end;

procedure TBAValueSet_Proxy.SetContentAsInteger(NewValue: Integer);
begin
  ProxedValueSet.ContentAsInteger:= NewValue;
end;

procedure TBAValueSet.AssignContentValue(Source: IBoldValue);
var
  s: IBoldIntegerContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
    SetContentToNull
  else if not assigned(source) then
    SetContent(Values.GetFirstValue)
  else if source.QueryInterface(IBoldIntegerContent, S) = S_OK then
  begin
    if s.IsNull then
      SetContentToNull
    else
      SetContent(Values.FindByInteger(s.AsInteger))
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignContentValue]);
end;

function TBAValueSet.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldIntegerContent) or
     IsEqualGuid(IID, IBoldStringContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldString/IntegerContent') // do not localize
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBAValueSet.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBAValueSet_Proxy;
end;

procedure TBAValueSet.SetEmptyValue;
begin
  if Values.Count > 0 then
    AsInteger := Values[0].AsInteger
  else if not assigned(BoldAttributeRTInfo) or BoldAttributeRTInfo.AllowNull then
    SetToNull
  else
    raise EBold.CreateFmt(sNoLegalValuesAvailable, [classname, '']);  //FIXME: Missing parameter
end;

procedure TBAValueSet.CheckIllegalValue;
begin
  if not IsNull and not Assigned(FValue) then
  begin
    if BoldPersistenceState = bvpsCurrent then
      raise EBold.Create(sBadDataInDB)
    else
      raise EBold.Create(sCannotRead)
  end;
end;

function TBAValueSet.CompareToEnumLiteral(const str: String): Boolean;
begin
  result := SameText(
    Str,
    BoldExpandName(AsString, '', xtExpression, -1, nccTrue))
end;

{ TBABoolean }
function TBABoolean.GetAsBoolean: Boolean;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsBoolean', ''); // do not localize
  EnsureNotNull; {ensures current}
  Result := Boolean(GetContentAsInteger);
end;

procedure TBABoolean.SetAsBoolean(Value: Boolean);
begin
  SetAsInteger(Integer(Value));
end;

function TBABoolean.GetValues: TBAValueSetValueList;
begin
  if not Assigned(_BooleanValues) then
  begin
    _BooleanValues := TBAValueSetValueList.Create;
    _BooleanValues.Add(Ord(False), ['N', 'F', 'False']); // do not localize
    _BooleanValues.Add(Ord(True), ['Y', 'T', 'True']); // do not localize
  end;
  Result := _BooleanValues;
end;

function TBAInteger.GetAsVariant: Variant;
begin
  Result := AsInteger;
end;

procedure TBAInteger.SetAsVariant(const Value: Variant);
begin
  AsInteger := Value;
end;

function TBAFloat.GetAsVariant: Variant;
begin
  Result := AsFloat;
end;

procedure TBAFloat.SetAsVariant(const Value: Variant);
begin
  AsFloat := Value;
end;

function TBACurrency.GetAsVariant: Variant;
begin
  Result := AsCurrency;
end;

procedure TBACurrency.SetAsVariant(const Value: Variant);
begin
  AsCurrency := Value;
end;

function TBABlob.GetAsVariant: Variant;
var
  P: Pointer;
  Data: string;
  Size: Integer;
begin
  // return byte array
  Data := GetAsBlob;
  Size := Length(Data);
  if Size > 0 then
  begin
    Result := VarArrayCreate([0, Size], varByte);
    P := VarArrayLock(Result);
    try
      Move(Pointer(Data)^, P^, Size);
    finally
      VarArrayUnlock(Result);
    end;
  end
  else
    Result := Null;
end;

procedure TBABlob.SetAsVariant(const Value: Variant);
var
  P: Pointer;
  Data: string;
  Size: Integer;
begin
  if VarIsArray(Value) and ((VarType(Value) and varTypeMask) = varByte) and
    (VarArrayDimCount(Value) = 1) then
  begin
    Size := VarArrayHighBound(Value, 1);
    SetLength(Data, Size);
    P := VarArrayLock(Value);
    try
      Move(P^, Pointer(Data)^, Size);
    finally
      VarArrayUnlock(Value);
    end;
    SetAsBlob(Data);
  end
  else if VarIsNull(Value) then
  begin
    SetAsBlob('');
  end
  else
  begin
    SetAsBlob(Value);
  end;
end;

function TBAMoment.GetAsVariant: Variant;
begin
  Result := GetAsDateTime;
end;

procedure TBAMoment.SetAsVariant(const Value: Variant);
begin
  SetAsDateTime(Value);
end;

function TBAValueSet.GetAsVariant: Variant;
begin
  Result := AsInteger;
end;

procedure TBAValueSet.SetAsVariant(const Value: Variant);
begin
  AsInteger := Value;
end;

function TBABoolean.GetAsVariant: Variant;
begin
  Result := AsBoolean;
end;

procedure TBABoolean.SetAsVariant(const Value: Variant);
begin
  AsBoolean := Value;
end;

function TBAValueSetValueList.GetCount: integer;
begin
  result := FValueList.Count;
end;

function TBAValueSetValueList.GetValueSetValue(Index: integer): TBAValueSetValue;
begin
  assert((index >= 0) and (index < count), 'TBAValueSetValueList.GetValueSetValue: IndexOutOfRange');
  result := TObject(FValueList[index]) as TBAValueSetValue;
end;

function TBAValueSetValue.GetBoldType: TBoldElementTypeInfo;
begin
  result := nil;
end;

procedure TBADate.AssignValue(Source: IBoldValue);
var
  s: IBoldDateContent;
begin
  if source.QueryInterface(IBoldDateContent, S) = S_OK then
    SetDataValue(s.AsDate)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignValue]);
end;

procedure TBATime.AssignValue(Source: IBoldValue);
var
  s: IBoldTimeContent;
begin
  if source.QueryInterface(IBoldTimeContent, S) = S_OK then
    SetDataValue(s.AsTime)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignValue]);
end;

function TBABoolean_Proxy.GetContentAsBoolean: Boolean;
begin
  Result := Boolean(GetContentAsInteger);
end;

procedure TBABoolean_Proxy.SetContentAsBoolean(NewValue: Boolean);
begin
  SetContentAsInteger(Integer(NewValue));
end;

procedure TBADate.AssignContentValue(Source: IBoldValue);
var
  s: IBoldDateContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
    SetContentToNull
  else if not assigned(source) then
    SetContent(0)
  else if source.QueryInterface(IBoldDateContent, S) = S_OK then
  begin
    if s.IsNull then
      SetContentToNull
    else
      SetContent(s.AsDate)
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignContentValue]);
end;

procedure TBATime.AssignContentValue(Source: IBoldValue);
var
  s: IBoldTimeContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
    SetContentToNull
  else if not assigned(source) then
    SetContent(0)
  else if source.QueryInterface(IBoldTimeContent, S) = S_OK then
  begin
    if s.IsNull then
      SetContentToNull
    else
      SetContent(s.AsTime)
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, Meth_AssignContentValue]);
end;

procedure TBABoolean.AssignContentValue(Source: IBoldValue);
var
  s: IBoldBooleanContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
    SetContentToNull
  else if not assigned(source) then
    SetContentAsInteger(Integer(false))
  else if source.QueryInterface(IBoldBooleanContent, S) = S_OK then
  begin
    if s.IsNull then
      SetContentToNull
    else
      SetContentAsInteger(Integer(s.AsBoolean))
  end
  else
    inherited;
end;

function TBABoolean.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldBooleanContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldBooleanContent') // do not localize
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBABoolean.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBABoolean_Proxy;
end;

{ TBAConstraint }

procedure TBAConstraint.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate);
begin
  if assigned(OwningElement) and assigned(Constraint) then
    owningElement.SubscribeToExpression(constraint.ConstraintExpression, subscriber, RequestedEvent = breReSubscribe);
end;

procedure TBAConstraint.CalculateConstraint;
var
  ie: TBoldIndirectElement;
begin
  if assigned(Constraint) and assigned(OwningElement) then
  begin
    ie := TBoldIndirectElement.Create;
    try
      Owningelement.EvaluateExpression(constraint.ConstraintExpression, ie);
      if (ie.value is TBABoolean) then
        assign(ie.Value)
      else
        asBoolean := false;
    finally
      ie.free;
    end;
  end
  else
    asBoolean := false;
end;

destructor TBAConstraint.Destroy;
begin
  FreeAndNil(fElementSubscriber);
  inherited;
end;

function TBAConstraint.GetContentAsInteger: Integer;
begin
  calculateConstraint;
  result := inherited GetContentAsInteger;
end;

function TBAConstraint.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  result := '';
  case representation of
    10: if assigned(constraint) then
      result := Constraint.ModelName;
    11: if assigned(constraint) then
      result := Constraint.ConstraintMessage;
    12: if assigned(constraint) then
      result := Constraint.ConstraintExpression;
    13: if Owningelement is TBoldDomainElement then
      result := TBoldDomainElement(Owningelement).DisplayName;
    14: if assigned(Owningelement) then
      result := Owningelement.AsString;
    else
      result := inherited GetStringRepresentation(Representation);
  end;
end;

procedure TBAConstraint.Initialize(Constraint: TBoldConstraintRTInfo; OwningElement: TBoldElement);
var
  ResType: TBoldElementTypeInfo;
begin
  fOwningElement := OwningElement;
  if assigned(fOwningElement) then
    fOwningElement.AddSmallSubscription(fElementSubscriber, [beDestroying, beObjectDeleted], beDestroying);
  fConstraint := Constraint;
  if assigned(Constraint) and assigned(OwningElement) then
  begin
    try
      resType := Owningelement.Evaluator.ExpressionType(Constraint.ConstraintExpression, OwningElement.BoldType, true);
    except
      on e: Exception do
        raise EBold.CreateFmt(sInvalidConstraint, [Constraint.ConstraintExpression, BOLDCRLF, e.message]);
    end;

    if not assigned(ResType) then
      raise EBold.CreateFmt(sUnknownConstraintType, [Constraint.ConstraintExpression, OwningElement.BoldType.AsString]);

    if not ResType.ConformsTo((BoldType.SystemTypeInfo as TboldSystemTypeInfo).AttributeTypeInfoByDelphiName[TBABoolean.ClassName]) then
      raise EBold.CreateFmt(sConstraintNotBoolean, [Constraint.ConstraintExpression, OwningElement.BoldType.AsString]);
  end;
end;

procedure TBAConstraint.InitializeMember(OwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo);
begin
  inherited;
  fElementSubscriber := TBoldPassthroughSubscriber.Create(Receive);
end;

procedure TBAConstraint.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = beDestroying then
  begin
    fOwningElement := nil;
    fElementSubscriber.CancelAllSubscriptions;
  end;
end;

procedure TBAConstraint.SubscribeToStringRepresentation(
  Representation: TBoldRepresentation; Subscriber: TBoldSubscriber;
  RequestedEvent: TBoldEvent = breReEvaluate);
begin
  case Representation of
    10..13: ; // do nothing, static values...
    14: if assigned(OwningElement) then
      OwningElement.SubscribeToStringRepresentation(brDefault, Subscriber, RequestedEvent);
    else inherited;
  end;
end;

procedure TBAConstraint.Assign(Source: TBoldElement);
begin
  inherited;
  if Source is TBAConstraint then
    Initialize(TBAConstraint(Source).Constraint, TBAConstraint(Source).OwningElement);
end;

{ TBAString_Proxy }

function TBAString_Proxy.GetProxedString: TBAString;
begin
  result := ProxedElement as TBAString;
end;

{ TBAInteger_Proxy }

function TBAInteger_Proxy.GetProxedInteger: TBAInteger;
begin
  result := ProxedElement as TBAInteger;
end;

{ TBAFloat_Proxy }

function TBAFloat_Proxy.GetProxedFloat: TBAFloat;
begin
  result := ProxedElement as TBAFloat;
end;

{ TBACurrency_Proxy }

function TBACurrency_Proxy.GetProxedCurrency: TBACurrency;
begin
  result := ProxedElement as TBACurrency;
end;

{ TBABlob_Proxy }

function TBABlob_Proxy.GetProxedBlob: TBABlob;
begin
  result := ProxedElement as TBABlob;
end;

{ TBAValueSet_Proxy }

function TBAValueSet_Proxy.GetProxedvalueSet: TBAValueSet;
begin
  result := ProxedElement as TBAValueSet;
end;

{ TBABoolean_Proxy }

function TBADate.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldDateContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldDateContent') // do not localize
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBATime.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldTimeContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldTimeContent') // do not localize
  else
    result := inherited ProxyInterface(IId, Mode, Obj);
end;

function TBADate.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBADate_Proxy;
end;

function TBATime.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBATime_Proxy;
end;

{ TBATypedBlob_Proxy }

function TBATypedBlob_Proxy.GetContentTypeContent: String;
begin
  Result := ProxedTypedBlob.GetContentTypeContent;
end;

function TBATypedBlob_Proxy.GetProxedTypedBlob: TBATypedBlob;
begin
  result := ProxedElement as TBATypedBlob;
end;

procedure TBATypedBlob_Proxy.SetContentTypeContent(const NewValue: String);
begin
  ProxedTypedBlob.SetContentTypeContent(NewValue);
end;

function TBAMoment_Proxy.GetProxedMoment: TBAMoment;
begin
  result := ProxedElement as TBAMoment;
end;

function TBATime.GetAsSeconds: cardinal;
begin
  Result := Seconds + (Minutes * 60) + (Hours * 3600);
end;

initialization
  with BoldMemberTypes do
  begin
    AddMemberTypeDescriptor(TBoldAttribute, alAbstract);
    AddMemberTypeDescriptor(TBAString, alConcrete);
    AddMemberTypeDescriptor(TBATrimmedString, alConcrete);
    AddMemberTypeDescriptor(TBANumeric, alAbstract);
    AddMemberTypeDescriptor(TBAInteger, alConcrete);
    AddMemberTypeDescriptor(TBAWord, alConcrete);
    AddMemberTypeDescriptor(TBASmallInt, alConcrete);
    AddMemberTypeDescriptor(TBAByte, alConcrete);
    AddMemberTypeDescriptor(TBAShortInt, alConcrete);
    AddMemberTypeDescriptor(TBAFloat, alConcrete);
    AddMemberTypeDescriptor(TBACurrency, alConcrete);
    AddMemberTypeDescriptor(TBABlob, alConcrete);
    AddMemberTypeDescriptor(TBATypedBlob, alConcrete);
    AddMemberTypeDescriptor(TBAMoment, alAbstract);
    AddMemberTypeDescriptor(TBADateTime, alConcrete);
    AddMemberTypeDescriptor(TBADate, alConcrete);
    AddMemberTypeDescriptor(TBATime, alConcrete);
    AddMemberTypeDescriptor(TBAValueSet, alAbstract);
    AddMemberTypeDescriptor(TBABoolean, alConcrete);
    AddMemberTypeDescriptor(TBAConstraint, alConcrete);
    AddMemberTypeDescriptor(TBABlobImageJPEG, alConcrete);
    AddMemberTypeDescriptor(TBABlobImageBMP, alConcrete);
  end;

finalization
  FreeAndNil(_BooleanValues);
  if BoldMemberTypesAssigned then
    with BoldMemberTypes do
    begin
      RemoveDescriptorByClass(TBoldAttribute);
      RemoveDescriptorByClass(TBAString);
      RemoveDescriptorByClass(TBATrimmedString);
      RemoveDescriptorByClass(TBANumeric);
      RemoveDescriptorByClass(TBAInteger);
      RemoveDescriptorByClass(TBAWord);
      RemoveDescriptorByClass(TBASmallInt);
      RemoveDescriptorByClass(TBAByte);
      RemoveDescriptorByClass(TBAShortInt);
      RemoveDescriptorByClass(TBAFloat);
      RemoveDescriptorByClass(TBACurrency);
      RemoveDescriptorByClass(TBABlob);
      RemoveDescriptorByClass(TBATypedBlob);
      RemoveDescriptorByClass(TBAMoment);
      RemoveDescriptorByClass(TBADateTime);
      RemoveDescriptorByClass(TBADate);
      RemoveDescriptorByClass(TBATime);
      RemoveDescriptorByClass(TBAValueSet);
      RemoveDescriptorByClass(TBABoolean);
      RemoveDescriptorByClass(TBAConstraint);
      RemoveDescriptorByClass(TBABlobImageJPEG);
      RemoveDescriptorByClass(TBABlobImageBMP);
    end;

end.

