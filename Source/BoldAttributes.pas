
{ Global compiler directives }
{$include bold.inc}
unit BoldAttributes;

interface

uses
  Classes,
  SysUtils,
  BoldSystem,
  BoldSystemRT,
  BoldDefs,
  BoldBase,
  BoldSubscription,
  BoldElements,
  BoldDomainElement,
  BoldValueInterfaces,
  BoldFreeStandingValues;

 {$IFNDEF BOLD_NO_QUERIES}
const
  {Query events}
  bqBaseAttributes = bqMaxSystem + 1;
  bqMaySetContentType = bqBaseAttributes + 0;
  bqMaxAttributes = bqBaseAttributes + 0;
  {$ENDIF}
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

  TBAValueSetClass = class of TBAValueSet;

  {---TBAString---}
  { Base class for all string attributes. Before Unicode (Delphi 2009) this class
    constains an AnsiString, after Unicode (Delphi 2009 and up) a UnicodeString.

    The descendants TBAAnsiString and TBAUnicodeString only contains implementation
    and an additional data field, if it is a different string kind compared to
    this class in the respective Delphi version.
    This ensures that TBAString can be used as a base class for all string attributes
    which helps in type checks and comparation and on the other hand it can be
    instanciated and then uses the default string type of the Delphi version. }
  TBAString = class(TBoldAttribute)
  strict private
    FValue: string;
  private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
    procedure SetDataValue(const NewValue: string);
    procedure SetContent(const NewValue: string);
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    procedure FreeContent; override;
    function GetAsAnsiString: TBoldAnsiString; virtual;
    function GetAsUnicodeString: TBoldUnicodeString; virtual;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    function GetValue: string; virtual;
    function MaySetValue(NewValue: String; Subscriber: TBoldSubscriber): Boolean; virtual;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    procedure SetAsAnsiString(const Value: TBoldAnsiString); virtual;
    procedure SetAsUnicodeString(const Value: TBoldUnicodeString); virtual;
    procedure SetValue(const Value: string); virtual;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
    property Value: string read GetValue write SetValue;
  public
    constructor CreateWithValue(const Value: string);
    procedure Assign(Source: TBoldElement); override;
    procedure AssignValue(const Source: IBoldValue); override;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    function ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean; override;
    function CanSetValue(NewValue: string; Subscriber: TBoldSubscriber): Boolean;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure SetEmptyValue; override;
    function IsNullOrEmpty: Boolean;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    property AsAnsiString: TBoldAnsiString read GetAsAnsiString write
        SetAsAnsiString;
    property AsUnicodeString: TBoldUnicodeString read GetAsUnicodeString write
        SetAsUnicodeString;
  end;

  {$IFNDEF T2H}
  TBAString_Proxy = class(TBoldAttribute_Proxy, IBoldStringContent)
  private
    class var fLastUsed: array[TBoldDomainElementProxyMode] of TBoldMember_Proxy;
    class var fLastUsedAsInterface: array[TBoldDomainElementProxyMode] of IBoldValue;
    function GetContentAsAnsiString: TBoldAnsiString;
    function GetContentAsUnicodeString: TBoldUnicodeString;
    function GetProxedString: TBAString;
    procedure SetContentAsString(const NewValue: String);
    function GetContentAsString: String; override;
    procedure SetContentAsAnsiString(const NewValue: TBoldAnsiString);
    procedure SetContentAsUnicodeString(const NewValue: TBoldUnicodeString);
  protected
    property ProxedString: TBAString read GetProxedString;
    class function MakeProxy(ProxedMember: TBoldMember; Mode:  TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
  end;
  {$ENDIF}

  {---TBAAnsiString---}
  TBAAnsiString = class(TBAString)
  private
    function ProxyClass: TBoldMember_ProxyClass;
  {$IFDEF BOLD_UNICODE}
  private
    fValue: TBoldAnsiString;
    procedure SetContent(NewValue: TBoldAnsiString);
    procedure SetDataValue(NewValue: TBoldAnsiString);
  {$ENDIF}
  protected
  {$IFDEF BOLD_UNICODE}
    procedure FreeContent; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAsAnsiString: TBoldAnsiString; override;
    procedure SetAsAnsiString(const Value: TBoldAnsiString); override;
  {$ENDIF}
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
  public
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode;
        out Obj): Boolean; override;
  {$IFDEF BOLD_UNICODE}
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation):
        Boolean; override;
    function ValidateString(const Value: string; Representation: TBoldRepresentation):
        Boolean; override;
  {$ENDIF}
  end;

  {---TBAUnicodeString---}
  TBAUnicodeString = class(TBAString)
  private
  {$IFNDEF BOLD_UNICODE}
    fValue: TBoldUnicodeString;
    procedure SetContent(NewValue: TBoldUnicodeString);
    procedure SetDataValue(NewValue: TBoldUnicodeString);
  {$ENDIF}
    function ProxyClass: TBoldMember_ProxyClass;
  protected
  {$IFNDEF BOLD_UNICODE}
    procedure FreeContent; override;
    function GetAsUnicodeString: TBoldUnicodeString; override;
    function GetValue: string; override;
    procedure SetAsUnicodeString(const Value: TBoldUnicodeString); override;
    procedure SetValue(const Value: string); override;
  {$ENDIF}
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
  public
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode;
        out Obj): Boolean; override;
  end;

  {---TBAText---}
  TBAText = class(TBAAnsiString)
  protected
  public
    function ValidateString(const Value: string; Representation: TBoldRepresentation):
        Boolean; override;
  end;

  {---TBAUnicodeText---}
  TBAUnicodeText = class(TBAUnicodeString)
  public
    function ValidateString(const Value: string; Representation: TBoldRepresentation):
        Boolean; override;
  end;

  {---TBATrimmedString---}
  TBATrimmedString = class(TBAString)
  protected
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
  end;

  {---TBANumeric---}
  TBANumeric = class(TBoldAttribute)
  private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  protected
    function GetAsFloat: Double; virtual; abstract;
    procedure SetAsInteger(Value: integer); virtual; abstract;
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
  public
    procedure SetEmptyValue; override;
    function IsVariantTypeCompatible(const Value: Variant): Boolean; override;
    function IsNullOrZero: boolean; virtual;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
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
    procedure FreeContent; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    function CheckRangeWithBounds(Value, Min, Max: integer): boolean;
    function GetAsInteger: integer; virtual;
    function GetAsFloat: Double; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetAsInteger(Value: integer); override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    function MaySetValue(NewValue: integer; Subscriber: TBoldSubscriber): Boolean; virtual;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
  public
    constructor CreateWithValue(Value: integer);
    procedure Assign(Source: TBoldElement); override;
    procedure AssignValue(const Source: IBoldValue); override;
    function CanSetValue(Value: integer; Subscriber: TBoldSubscriber): Boolean;
    function CheckRange(Value: integer): Boolean; virtual;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure SetEmptyValue; override;
    procedure SetAsVariant(const Value: Variant); override;
    function GetAsVariant: Variant; override;
    function ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    function IsVariantTypeCompatible(const Value: Variant): Boolean; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
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
    procedure FreeContent; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetAsFloat: Double; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetAsFloat(Value: Double); virtual;
    procedure SetAsInteger(Value: integer); override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    function MaySetValue(NewValue: Double; Subscriber: TBoldSubscriber): Boolean; virtual;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
  public
    constructor CreateWithValue(Value: double);
    procedure SetAsVariant(const Value: Variant); override;
    function GetAsVariant: Variant; override;
    function ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    function IsVariantTypeCompatible(const Value: Variant): Boolean; override;
    procedure SetEmptyValue; override;
    procedure Assign(Source: TBoldElement); override;
    procedure AssignValue(const Source: IBoldValue); override;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
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
    procedure FreeContent; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetAsCurrency: Currency;
    function GetAsFloat: Double; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetAsCurrency(Value: Currency);
    procedure SetAsFloat(Value: Double); virtual;
    procedure SetAsInteger(Value: integer); override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    function MaySetValue(NewValue: Currency; Subscriber: TBoldSubscriber): Boolean; virtual;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
  public
    constructor CreateWithValue(Value: currency);
    procedure SetAsVariant(const Value: Variant); override;
    function GetAsVariant: Variant; override;
    function ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    function IsVariantTypeCompatible(const Value: Variant): Boolean; override;
    procedure SetEmptyValue; override;
    procedure Assign(Source: TBoldElement); override;
    procedure AssignValue(const Source: IBoldValue); override;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer write SetAsInteger;
    function CanSetValue(NewValue: Currency; Subscriber: TBoldSubscriber): Boolean;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
  end;

  { TBoldBlobStream }
  TBoldBlobStream = class(TStream)
  private
    fData: TBytes;
    fBlobAttr: TBABlob;
    fPosition: Int64;
    function GetBlobSize: Int64;
    procedure InternalSetSize(NewSize: Int64; BlobEvents: Boolean);
  public
    constructor Create(BlobAttr: TBABlob);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Clear;
    procedure Truncate;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const aFileName: string; aMode: Word = fmShareDenyNone);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SetSize(NewSize: Longint); override; deprecated;
    procedure SetSize(const NewSize: Int64); override;
    function IsDataSame(AData: Pointer; ASize: Int64): boolean;
  end;

  {---TBABlob---}
  TBABlob = class(TBoldAttribute)
  private
    fStream: TBoldBlobStream;
    fSupressEventCount: integer;
    procedure SetContent(NewValue: TBoldAnsiString);
    procedure SetDataValue(NewValue: TBoldAnsiString);
    function GetBlobSize: Int64;
    function GetAsStream: TBoldBlobStream;
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
    procedure FreeContent; override;
    procedure SetAsVariant(const Value: Variant); override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    function GetAsBlob: TBoldAnsiString;
    procedure SetAsBlob(NewValue: TBoldAnsiString);
    function MaySetValue(NewValue: TBoldAnsiString; Subscriber: TBoldSubscriber): Boolean; virtual;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
    function SupressEvents: Boolean;
    procedure BeginSupressEvents;
    procedure EndSupressEvents;
    procedure StartModifyOfBlob(const Operation: String);
    procedure EndModifyOfBlob;
    procedure FailModifyOfBlob;
  public
    destructor Destroy; override;
    procedure Initialize; override;
    procedure SetToNull; override;
    procedure Assign(Source: TBoldElement); override;
    procedure AssignValue(const Source: IBoldValue); override;
    property ContentType: string index brShort read GetStringRepresentation write SetStringRepresentation;
    function CanSetValue(NewValue: TBoldAnsiString; Subscriber: TBoldSubscriber): Boolean;
    function CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure SetEmptyValue; override;
    property BlobSize: Int64 read GetBlobSize;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const aFileName: string; aMode: Word = fmShareDenyNone);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    property AsStream: TBoldBlobStream read GetAsStream;
  end;

  {-- TBATypedBlob --}
  TBATypedBlob = class(TBABlob)
  private
    FContentType: string;
    procedure SetContentType2(Value: string);
    procedure SetContentTypeContent(NewValue: String);
  protected
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    function GetContentTypeContent: String;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;    
  public
    procedure AssignValue(const Source: IBoldValue); override;
    procedure SetToNull; override;
    function CanSetContentType(Value: string; Subscriber: TBoldSubscriber): Boolean;
    function CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
  end;

  {-- TBABlobImageJPEG --}
  TBABlobImageJPEG = class(TBABlob)
  protected
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
  end;

  {-- TBABlobImageBMP --}
  TBABlobImageBMP = class(TBABlob)
  protected
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
  end;

  {---TBAMoment---}
  TBAMoment = class(TBANumeric)
  private
    FValue: TDateTime;
    class var AttributeTypeInfo: TBoldElementTypeInfo;
    procedure SetDataValue(NewValue: TDateTime);
    procedure SetDateTimeContent(NewValue: TDateTime);
    function GetDays: Word;
    function GetHours: Word;
    function GetMinutes: Word;
    function GetMonths: Word;
    function GetSeconds: Word;
    function GetYears: Word;
  protected
    procedure FreeContent; override;
    function GetAsFloat: Double; override;
    procedure SetAsInteger(Value: integer); override;
    function GetAsDate: TDateTime;
    function GetAsDateTime: TDateTime;
    function GetAsTime: TDateTime;
    procedure SetAsDate(Value: TDateTime);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsTime(Value: TDateTime);
    function MaySetValue(NewValue: TDateTime; Subscriber: TBoldSubscriber): Boolean; virtual;
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
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
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;
    function IsVariantTypeCompatible(const Value: Variant): Boolean; override;
    procedure SetEmptyValue; override;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    function IsNullOrZero: boolean; override;
  end;

  {---TBADateTime---}
  TBADateTime = class(TBAMoment)
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
  public
    constructor CreateWithValue(Value: TDateTime);
    procedure AssignValue(const Source: IBoldValue); override;
    property AsDateTime;
    property AsDate;
    property AsTime;
    property Seconds;
    property Minutes;
    property Hours;
    property Days;
    property Months;
    property Years;
    function ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
  end;

  {---TBADate---}
  TBADate = class(TBAMoment)
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
  public
    constructor CreateWithValue(Value: TDateTime);
    procedure AssignValue(const Source: IBoldValue); override;
    function ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean; override;
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
    procedure AssignContentValue(const Source: IBoldValue); override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
  public
    constructor CreateWithValue(Value: TDateTime);
    procedure AssignValue(const Source: IBoldValue); override;
    function ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean; override;
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
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
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
    function CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
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
    class var AttributeTypeInfo: TBoldElementTypeInfo;
    procedure CheckIllegalValue;
    function GetAsInteger: Integer;
    procedure SetAsInteger(Value: Integer);
    procedure SetContent(NewValue: TBAValueSetValue);
    procedure SetContentAsInteger(NewValue: Integer);
    procedure SetDataValue(NewValue: TBAValueSetValue);
  protected
    procedure FreeContent; override;
    function GetContentAsInteger: Integer; virtual;
    procedure AssignContentValue(const Source: IBoldValue); override;
    procedure Initialize; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    function MaySetValue(NewValue: TBAValueSetValue; Subscriber: TBoldSubscriber): Boolean; virtual;
    property ContentAsInteger: Integer read GetContentAsInteger write SetContentAsInteger;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
  public
    class function GetValues: TBAValueSetValueList; virtual;
    procedure SetAsVariant(const Value: Variant); override;
    function GetAsVariant: Variant; override;
    function ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean; override;
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    procedure Assign(Source: TBoldElement); override;
    function CompareToEnumLiteral(const str: String): Boolean; virtual;
    procedure AssignValue(const Source: IBoldValue); override;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property Values: TBAValueSetValueList read GetValues;
    function CanSetValue(NewValue: TBAValueSetValue; Subscriber: TBoldSubscriber): Boolean;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure SetEmptyValue; override;
  end;

  {---TBABoolean---}
  TBABoolean = class(TBAValueSet)
  private
    class var _BooleanValues: TBAValueSetValueList;
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(Value: Boolean);
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
  public
    constructor CreateWithValue(Value: Boolean);
    class function GetValues: TBAValueSetValueList; override;
    procedure SetAsVariant(const Value: Variant); override;
    function GetAsVariant: Variant; override;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    function IsVariantTypeCompatible(const Value: Variant): Boolean; override;
    function IsEqualToValue(const Value: IBoldValue): Boolean; override;    
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
    procedure Initialize; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TBoldElement); override;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure InitializeConstraint(Constraint: TBoldConstraintRTInfo; OwningElement: TBoldElement);
    property Constraint: TBoldConstraintRTInfo read fConstraint;
    property OwningElement: TBoldElement read fOwningElement;
  end;

function VarRecToBoldAttribute(const Value: TVarRec): TBoldAttribute;
function VarArrayToBoldMemberList(const Values: array of const): TBoldMemberList;

implementation

uses
  {$IFDEF BOLD_UNICODE}AnsiStrings,{$ENDIF}
  BoldNameExpander,
  BoldTaggedValueSupport,
  Variants,
  BoldUtils,
  BoldCoreConsts,
  BoldMemberTypeDictionary;

type
  TBAAnsiString_Proxy = class(TBAString_Proxy, IBoldAnsiStringContent)
  end;


  TBoldSystemSubscriber = class(TBoldPassthroughSubscriber)
  protected
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent;
      RequestedEvent: TBoldRequestedEvent); override;
  end;

var
  _SystemSubscriber: TBoldSystemSubscriber;

procedure SubscribeToSystem;
begin
  if not Assigned(_SystemSubscriber) then
    _SystemSubscriber := TBoldSystemSubscriber.Create(nil);
  TBoldSystem.DefaultSystem.AddSmallSubscription(_SystemSubscriber, [beDestroying], beDestroying);
end;

{ TBoldSystemSubscriber }

procedure TBoldSystemSubscriber.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  TBAString.AttributeTypeInfo := nil;
  TBANumeric.AttributeTypeInfo := nil;
  TBAMoment.AttributeTypeInfo := nil;
  TBAValueSet.AttributeTypeInfo := nil;
  FreeAndNil(_SystemSubscriber);
end;

type
  TBAUnicodeString_Proxy = class(TBAString_Proxy, IBoldUnicodeStringContent)
  end;
  
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

  TBABlob_Proxy = class(TBoldAttribute_Proxy, IBoldBlobContent, IBoldBlobStreamContent)
  private
    function GetProxedBlob: TBABlob;
    function GetContentAsBlob: TBoldAnsiString;
    procedure SetContentAsBlob(const NewValue: TBoldAnsiString);
    function GetBlobAsStream: TStream;
    procedure BeginSupressEvents;
    procedure EndSupressEvents;
    function SupressEvents: boolean;
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
    function GetContentAsString: String; override;
  protected
    property ProxedValueSet: TBAValueSet read GetProxedValueSet;
  end;

  TBABoolean_Proxy = class(TBAValueSet_Proxy, IBoldBooleanContent)
  private
    function GetContentAsBoolean: Boolean;
    procedure SetContentAsBoolean(NewValue: Boolean);
  end;

function VarRecToBoldAttribute(Const Value: TVarRec): TBoldAttribute;
begin
  case Value.VType of
    vtInteger:      Result := TBAInteger.CreateWithValue(value.VInteger);
    vtInt64:        Result := TBAInteger.CreateWithValue(value.VInt64^);
    vtBoolean:      Result := TBABoolean.CreateWithValue(value.VBoolean);
    vtExtended:     Result := TBAFloat.CreateWithValue(value.VExtended^);
    vtString:       Result := TBAString.CreateWithValue(value.VString^);
    vtAnsiString:   Result := TBAString.CreateWithValue(String(value.VAnsiString));
{$IFDEF BOLD_UNICODE}
    vtUnicodeString: Result := TBAString.CreateWithValue(String(value.vUnicodeString));
{$ENDIF}
    vtCurrency:     Result := TBACurrency.CreateWithValue(value.VCurrency^);
    vtWideString:   Result := TBAString.CreateWithValue(PWideString(value.VWideString)^);
  else
    raise EBold.Create('Unsupported Variant type: ' + IntToStr(Value.Vtype));
  end;
end;

function VarArrayToBoldMemberList(const Values: array of const): TBoldMemberList;
var
  i: integer;
begin
  result := TBoldMemberList.Create;
  result.CloneMembers := false;
  for I := 0 to length(Values) - 1 do
    result.Add(VarRecToBoldAttribute(Values[i]));
end;

{ TBAString }
procedure TBAString.SetDataValue(const NewValue: string);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', '');

  if IsNull or (FValue <> NewValue) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', '');
    try
      SetContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBAString.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, 'SetStringRepresentation', 'String validation failed');

  if Representation = brDefault then
    SetDataValue(Value)
  else
    inherited SetStringRepresentation(Representation, Value);
end;

function TBAString.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetStringRepresentation', '');
  if Representation <> brDefault then
    inherited GetStringRepresentation(Representation);

  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Result := FValue;
end;

function TBAString.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
begin
  if Assigned(BoldAttributeRtInfo) then
  begin
    Result := (BoldAttributeRtInfo.Length = -1) or
      (Length(Value) <= BoldAttributeRtInfo.Length);
    if not result then
      SetBoldLastFailureReason(TBoldFailureReason.CreateFmt('String too long. Max allowed: %d, actual: %d.', [BoldAttributeRtInfo.Length, Length(Value)] , self));
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
      Result := StringCompare(CompType, AsString, BoldElement.AsString);
  end
  else
  if Assigned(BoldElement) then
    Result := StringCompare(CompType, AsString, BoldElement.AsString)
  else
    result := -1;
end;

constructor TBAString.CreateWithValue(const Value: string);
begin
  inherited Create;
  asString := value;
end;

function TBAString.CanSetValue(NewValue: string; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetValue(NewValue, Subscriber);
{$IFNDEF BOLD_NO_QUERIES}
   result := result and   SendQuery(bqMaySetValue, [NewValue], Subscriber)
{$ENDIF}
end;

function TBAString.MaySetValue(NewValue: String;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := true;
end;


procedure TBAString.AssignValue(const Source: IBoldValue);
var
  s: IBoldStringContent;
begin
  if source.QueryInterface(IBoldStringContent, S) = S_OK then
    SetDataValue(s.AsString)
  else
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
end;

procedure TBAString.SetContent(const NewValue: string);
var
  bContentIsNull: Boolean;
  sOldValue: string;
begin
  bContentIsNull := ContentIsNull;
  if (BoldPersistenceState = bvpsInvalid) or
     bContentIsNull or (fValue <> NewValue) then
  begin
    PreChange;
    sOldValue := fValue;
    fValue := NewValue;
    SetToNonNull;
    if bContentIsNull then begin
      Changed(beValueChanged, [NewValue]);
    end else begin
      Changed(beValueChanged, [NewValue, sOldValue]);
    end;
  end;
end;

function TBAString_Proxy.GetContentAsString: string;
begin
  result := ProxedString.Value;
end;

function TBAString_Proxy.GetContentAsAnsiString: TBoldAnsiString;
begin
  result := ProxedString.AsAnsiString;
end;

function TBAString_Proxy.GetContentAsUnicodeString: TBoldUnicodeString;
begin
  result := ProxedString.AsUnicodeString;
end;

procedure TBAString_Proxy.SetContentAsString(const NewValue: string);
begin
  ProxedString.SetContent(NewValue);
end;

procedure TBAString_Proxy.SetContentAsAnsiString(const NewValue:
    TBoldAnsiString);
begin
  if ProxedString is TBAAnsiString then begin
    TBAAnsiString(ProxedString).SetContent(NewValue);
  end else begin
    ProxedString.SetContent(string(NewValue));
  end;
end;

procedure TBAString_Proxy.SetContentAsUnicodeString(const NewValue:
    TBoldUnicodeString);
begin
  if ProxedString is TBAUnicodeString then begin
    TBAUnicodeString(ProxedString).SetContent(NewValue);
  end else begin
    ProxedString.SetContent(string(NewValue));
  end;
end;

procedure TBAString.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldStringContent;
  sr: IBoldStringRepresentable;
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
  else if source.QueryInterface(IBoldStringRepresentable, sr) = S_OK then
  begin
    if sr.IsNull then
      SetContentToNull
    else
      SetContent(sr.AsString)
  end
  else
    raise EBold.CreateFmt('%s.AssignContentValue: unknown type of source', [classname]);
end;

function TBAString.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBAString_Proxy.MakeProxy(self, mode);
end;

function TBAString.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldStringContent) then begin
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldStringContent') // do not localize
  end else if IsEqualGUID(IID, IBoldAnsiStringContent) then begin
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldAnsiStringContent') // do not localize
  end else if IsEqualGUID(IID, IBoldUnicodeStringContent) then begin
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldUnicodeStringContent') // do not localize
  end else begin
    Result := inherited ProxyInterface(IID, Mode, Obj);
  end;
end;

procedure TBAString.FreeContent;
begin
  inherited;
  FValue := '';
end;

function TBAString.GetAsAnsiString: TBoldAnsiString;
begin
  Result := TBoldAnsiString(AsString);
end;

function TBAString.GetAsUnicodeString: TBoldUnicodeString;
begin
  Result := TBoldUnicodeString(AsString);
end;

function TBAString.GetValue: string;
begin
  Result := FValue;
end;

function TBAString.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  vString: IBoldStringContent;
begin
  Assert(Assigned(Value), ClassName + '.IsEqualToValue: Value can not be nil.');
  if Value.QueryInterface(IBoldStringContent, vString) = S_OK then
  begin
    if IsNull and vString.IsNull then
      result := true
    else
    if IsNull or vString.IsNull then
      result := false
    else
      result := StringCompare(ctCaseSensitive, Self.AsString, vString.asString) = 0
  end
  else
    result := inherited IsEqualToValue(Value);
end;

function TBAString.IsNullOrEmpty: Boolean;
begin
  Result := IsNull or (Value = '');
end;

procedure TBAString.SetAsAnsiString(const Value: TBoldAnsiString);
begin
  AsString := string(Value);
end;

procedure TBAString.SetAsUnicodeString(const Value: TBoldUnicodeString);
begin
  AsString := string(Value);
end;

procedure TBAString.SetEmptyValue;
begin
  if FValue <> '' then
    asString := '';
end;

procedure TBAString.SetValue(const Value: string);
begin
  FValue := Value;
end;

function TBAString.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
end;

function TBAString.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSString;
end;

{ TBAAnsiString }

{$IFDEF BOLD_UNICODE}
procedure TBAAnsiString.FreeContent;
begin
  inherited;
  fValue := '';
end;

function TBAAnsiString.GetAsAnsiString: TBoldAnsiString;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsAnsiString', ''); // do not localize

  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Result := fValue;
end;

function TBAAnsiString.GetValue: string;
begin
  Result := String(fValue);
end;

procedure TBAAnsiString.SetAsAnsiString(const Value: TBoldAnsiString);
begin
  SetDataValue(Value);
end;

procedure TBAAnsiString.SetContent(NewValue: TBoldAnsiString);
var
  bContentIsNull: Boolean;
  sOldValue: TBoldAnsiString;
begin
  bContentIsNull := ContentIsNull;
  if (BoldPersistenceState = bvpsInvalid) or
     bContentIsNull or (fValue <> NewValue) then
  begin
    PreChange;
    sOldValue := fValue;
    fValue := NewValue;
    SetToNonNull;
    if bContentIsNull then begin
      Changed(beValueChanged, [NewValue]);
    end else begin
      Changed(beValueChanged, [NewValue, sOldValue]);
    end;
  end;
end;

procedure TBAAnsiString.SetDataValue(NewValue: TBoldAnsiString);
begin
  BoldClearLastFailure;
  if not CanSetValue(string(NewValue), nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize

  if IsNull or (fValue <> NewValue) then
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

procedure TBAAnsiString.SetValue(const Value: string);
begin
  fValue := TBoldAnsiString(Value);
end;

function TBAAnsiString.ValidateCharacter(C: Char; Representation:
    TBoldRepresentation): Boolean;
begin
  Result := inherited ValidateCharacter(C, Representation);
  Result := Result and (Ord(C) < 256);
end;

function TBAAnsiString.ValidateString(const Value: string; Representation:
    TBoldRepresentation): Boolean;
begin
  Result := inherited ValidateString(Value, Representation);
  if Result and (String(AnsiString(Value)) <> Value) then begin
    Result := False;
    SetBoldLastFailureReason(TBoldFailureReason.Create(sStringIsNotAnsiString, Self));
  end;
end;
{$ENDIF}

function TBAAnsiString.GetProxy(
  Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBAAnsiString_Proxy.MakeProxy(self, mode);
end;

function TBAAnsiString.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBAAnsiString_Proxy;
end;

function TBAAnsiString.ProxyInterface(const IId: TGUID; Mode:
    TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGUID(IID, IBoldAnsiStringContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldAnsiStringContent') // do not localize
  else
    Result := inherited ProxyInterface(IID, Mode, Obj);
end;

{ TBAUnicodeString }

function TBAUnicodeString.GetProxy(
  Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBAUnicodeString_Proxy.MakeProxy(self, mode);
end;

{$IFNDEF BOLD_UNICODE}
procedure TBAUnicodeString.FreeContent;
begin
  inherited;
  FValue := '';
end;

function TBAUnicodeString.GetAsUnicodeString: TBoldUnicodeString;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsUnicodeString', ''); // do not localize

  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Result := fValue;
end;

function TBAUnicodeString.GetValue: string;
begin
  Result := String(FValue);
end;

procedure TBAUnicodeString.SetAsUnicodeString(const Value: TBoldUnicodeString);
begin
  SetDataValue(Value);
end;

procedure TBAUnicodeString.SetContent(NewValue: TBoldUnicodeString);
var
  bContentIsNull: Boolean;
  sOldValue: TBoldUnicodeString;
begin
  bContentIsNull := ContentIsNull;
  if (BoldPersistenceState = bvpsInvalid) or
     bContentIsNull or (fValue <> NewValue) then
  begin
    PreChange;
    sOldValue := fValue;
    fValue := NewValue;
    SetToNonNull;
    if bContentIsNull then begin
      Changed(beValueChanged, [NewValue]);
    end else begin
      Changed(beValueChanged, [NewValue, sOldValue]);
    end;
  end;
end;

procedure TBAUnicodeString.SetDataValue(NewValue: TBoldUnicodeString);
begin
  BoldClearLastFailure;
  if not CanSetValue(string(NewValue), nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', ''); // do not localize

  if IsNull or (fValue <> NewValue) then
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

procedure TBAUnicodeString.SetValue(const Value: string);
begin
  FValue := TBoldUnicodeString(Value);
end;
{$ENDIF}

function TBAUnicodeString.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBAUnicodeString_Proxy;
end;

function TBAUnicodeString.ProxyInterface(const IId: TGUID; Mode:
    TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGUID(IID, IBoldUnicodeStringContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldUnicodeStringContent') // do not localize
  else
    Result := inherited ProxyInterface(IID, Mode, Obj);
end;

{ TBAText }

function TBAText.ValidateString(const Value: string; Representation:
    TBoldRepresentation): Boolean;
begin
  // no inherited (inherited checks length)
  Result := True;
  // but we need AnsiString-Check
  {$IFDEF BOLD_UNICODE}
  if (String(AnsiString(Value)) <> Value) then begin
    Result := False;
    SetBoldLastFailureReason(TBoldFailureReason.Create(sStringIsNotAnsiString, Self));
  end;
  {$ENDIF}
end;

{ TBAUnicodeText }

function TBAUnicodeText.ValidateString(const Value: string; Representation:
    TBoldRepresentation): Boolean;
begin
  // no inherited (inherited checks length)
  Result := True;
end;

{ TBATrimmedString }

procedure TBATrimmedString.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  inherited SetStringRepresentation(Representation, trim(Value));
end;

{ TBANumeric }

function TBANumeric.CompareToAs(CompType: TBoldCompareType;
  BoldElement: TBoldElement): Integer;
begin
  if BoldElement is TBANumeric then
  begin
    if EitherIsNull(Self, TBoldAttribute(BoldElement)) then
      Result := NullSmallest(BoldElement)
    else
      case CompType of
        ctDefault:
          if AsFloat = TBANumeric(BoldElement).AsFloat then
            Result := 0
          else if AsFloat > TBANumeric(BoldElement).AsFloat then
            Result := 1
          else
            Result := -1;
      else
        Result := inherited CompareToAs(CompType, BoldElement);
      end;
  end else
    Result := inherited CompareToAs(CompType, BoldElement);
end;

function TBANumeric.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
end;

function TBANumeric.IsNullOrZero: boolean;
begin
  result := isNull or (AsFloat = 0);
end;

function TBANumeric.IsVariantTypeCompatible(const Value: Variant): Boolean;
begin
  result := VarType(Value) in [varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64];
end;

procedure TBANumeric.SetEmptyValue;
begin
  if AsFloat <> 0 then  
    AsInteger := 0;
end;

{ TBAInteger }

procedure TBAInteger.SetDataValue(NewValue: Integer);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', '');
  if not CheckRange(NewValue) then
    raise EBoldInternal.CreateFmt('%s: %s', [DisplayName, GetBoldLastFailureReason.Reason]);

  if IsNull or (FValue <> NewValue) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', '');
    try
      SetContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBAInteger.SetEmptyValue;
begin
  if fValue <> 0 then
    inherited;
end;

function TBAInteger.CheckRange(Value: integer): Boolean;
begin
  result := CheckRangeWithBounds(Value, Low(integer), High(integer));
end;

procedure TBAInteger.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, 'SetStringRepresentation', 'String validation failed');
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
    BoldRaiseLastFailure(self, 'GetStringRepresentation', '');
  if Representation <> brDefault then
    inherited GetStringRepresentation(Representation);
  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Result := IntToStr(GetAsInteger);
end;

function TBAInteger.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  vInteger: IBoldIntegerContent;
begin
  Assert(Assigned(Value), ClassName + '.IsEqualToValue: Value can not be nil.');
  if Value.QueryInterface(IBoldIntegerContent, vInteger) = S_OK then
  begin
    if IsNull and vInteger.IsNull then
      result := true
    else
    if IsNull or vInteger.IsNull then
      result := false
    else
      result := Self.asInteger = vInteger.asInteger
  end
  else
    result := inherited IsEqualToValue(Value);
end;

function TBAInteger.IsVariantTypeCompatible(const Value: Variant): Boolean;
begin
  result := inherited IsVariantTypeCompatible(Value) and CheckRange(Value);
end;

function TBAInteger.GetAsInteger: integer;
begin
  BoldClearLastFailure;
  if not canRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsInteger', '');
  EnsureNotNull; {ensures current}
  Result := FValue;
end;

function TBAInteger.GetAsFloat: Double;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsFloat', '');
  EnsureNotNull; {ensures current}
  Result := FValue;
end;

procedure TBAInteger.SetAsInteger(Value: integer);
begin
  SetDataValue(Value);
end;

function TBAInteger.ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean;
begin
  Result := CharInSet(C, ['0'..'9', '-', '+']);
end;

function TBAInteger.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
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
  end
  else
    Result := inherited CompareToAs(CompType, BoldElement);
end;

constructor TBAInteger.CreateWithValue(Value: integer);
begin
  inherited Create;
  asInteger := value;
end;

procedure TBAInteger.FreeContent;
begin
  inherited;
  fValue := 0;
end;

function TBAInteger.CanSetValue(Value: Integer; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetValue(Value, Subscriber);
{$IFNDEF BOLD_NO_QUERIES}
   result := result and   SendQuery(bqMaySetValue, [Value], Subscriber)
{$ENDIF}
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
        'Value outside range, range is %d to %d. Attempted to set %d',
        [Min, MAX, Value], self));
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

procedure TBAInteger.AssignValue(const Source: IBoldValue);
var
  s: IBoldIntegerContent;
begin
  if source.QueryInterface(IBoldIntegerContent, S) = S_OK then
    SetDataValue(s.AsInteger)
  else
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
end;

procedure TBAInteger.SetContent(NewValue: Integer);
var
  bContentIsNull: Boolean;
  sOldValue: Integer;
begin
  bContentIsNull := ContentIsNull;
  if (BoldPersistenceState = bvpsInvalid) or
    ContentIsNull or (FValue <> NewValue) then
  begin
    PreChange;
    sOldValue := fValue;
    FValue := NewValue;
    SetToNonNull;
    if bContentIsNull then begin
      Changed(beValueChanged, [NewValue]);
    end else begin
      Changed(beValueChanged, [NewValue, sOldValue]);
    end;
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

procedure TBAInteger.AssignContentValue(const Source: IBoldValue);
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
    raise EBold.CreateFmt('%s.AssignContentValue: unknown type of source', [classname]);
end;

function TBAInteger.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldIntegerContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldIntegerContent')
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBAInteger.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBAInteger_Proxy.MakeProxy(self, mode);
end;

{ TBAFloat }

procedure TBAFloat.SetDataValue(NewValue: Double);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', '');

  if IsNull or (FValue <> NewValue) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', '');

    try
      SetContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBAFloat.SetEmptyValue;
begin
  if FValue <> 0 then
    inherited;
end;

procedure TBAFloat.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, 'SetStringRepresentation', 'String validation failed');
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
    BoldRaiseLastFailure(self, 'GetStringRepresentation', '');
  if Representation <> brDefault then
    inherited GetStringRepresentation(Representation);
  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Result := FloatToStr(GetAsFloat);
end;

function TBAFloat.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  vFloat: IBoldFloatContent;
begin
  Assert(Assigned(Value), ClassName + '.IsEqualToValue: Value can not be nil.');
  if Value.QueryInterface(IBoldFloatContent, vFloat) = S_OK then
  begin
    if IsNull and vFloat.IsNull then
      result := true
    else
    if IsNull or vFloat.IsNull then
      result := false
    else
      result := Self.asFloat = vFloat.asFloat
  end
  else
    result := inherited IsEqualToValue(Value);
end;

function TBAFloat.IsVariantTypeCompatible(const Value: Variant): Boolean;
begin
  result := (VarType(Value) in [varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varSingle, varDouble, varCurrency])
end;

function TBAFloat.GetAsFloat: Double;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsFloat', '');
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
  Result := CharInSet(C, ['0'..'9', '-', '+', 'e', 'E', {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DecimalSeparator]);
end;

function TBAFloat.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
var
  temp: Extended;
begin
  if value = '' then
    result := CanSetToNull(nil)
  else
  begin
    result := TextToFloat(PChar(Value), temp, fvExtended);
    if not result then
      FormatFailure(value, 'float');
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

constructor TBAFloat.CreateWithValue(Value: double);
begin
  inherited Create;
  asFloat := Value;
end;

procedure TBAFloat.FreeContent;
begin
  inherited;
  fValue := 0;
end;

function TBAFloat.CanSetValue(NewValue: Double; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetValue(NewValue, Subscriber);
{$IFNDEF BOLD_NO_QUERIES}
   result := result and SendQuery(bqMaySetValue, [NewValue], Subscriber)
{$ENDIF}
end;

function TBAFloat.MaySetValue(NewValue: Double;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := True;
end;

procedure TBAFloat.AssignValue(const Source: IBoldValue);
var
  s: IBoldFloatContent;
begin
  if source.QueryInterface(IBoldFloatContent, S) = S_OK then
    SetDataValue(s.AsFloat)
  else
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
end;

procedure TBAFloat.SetContent(NewValue: Double);
var
  bContentIsNull: Boolean;
  sOldValue: Double;
begin
  bContentIsNull := ContentIsNull;
  if (BoldPersistenceState = bvpsInvalid) or
    ContentIsNull or (FValue <> NewValue) then
  begin
    PreChange;
    sOldValue := fValue;
    FValue := NewValue;
    SetToNonNull;
    if bContentIsNull then begin
      Changed(beValueChanged, [NewValue]);
    end else begin
      Changed(beValueChanged, [NewValue, sOldValue]);
    end;
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

procedure TBAFloat.AssignContentValue(const Source: IBoldValue);
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
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
end;

function TBAFloat.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldFloatContent) then
    Result := RetrieveProxyInterface(IBoldFloatContent, Mode, obj, 'IBoldFloatContent')
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBAFloat.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBAFloat_Proxy.MakeProxy(self, mode);
end;

{ TBACurrency }

procedure TBACurrency.SetDataValue(NewValue: Currency);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', '');

  if IsNull or (FValue <> NewValue) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', '');
    try
      SetContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBACurrency.SetEmptyValue;
begin
  if fValue <> 0 then  
    inherited;
end;

procedure TBACurrency.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, 'SetStringRepresentation', 'String validation failed');
  if Representation = brDefault then
  begin
    if Value = '' then
      SetToNull
    else
    begin
      if Value[Length(Value)] = {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DecimalSeparator then
        SetDataValue(StrToCurr(Concat(Value, '0')))
      else
        SetDataValue(StrToCurr(Value));
    end;
  end else
    inherited SetStringRepresentation(Representation, Value);
end;

function TBACurrency.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetStringRepresentation', '');
  if Representation <> brDefault then
    inherited GetStringRepresentation(Representation);
  if IsNull then {IsNull ensures current}
    Result := ''
  else
    Result := CurrToStr(GetAsCurrency);
end;

function TBACurrency.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  vCurrency: IBoldCurrencyContent;
begin
  Assert(Assigned(Value), ClassName + '.IsEqualToValue: Value can not be nil.');
  if Value.QueryInterface(IBoldCurrencyContent, vCurrency) = S_OK then
  begin
    if IsNull and vCurrency.IsNull then
      result := true
    else
    if IsNull or vCurrency.IsNull then
      result := false
    else
      result := Self.asCurrency = vCurrency.asCurrency
  end
  else
    result := inherited IsEqualToValue(Value);
end;

function TBACurrency.IsVariantTypeCompatible(const Value: Variant): Boolean;
begin
  result := (VarType(Value) in [varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varSingle, varDouble, varCurrency])
end;

procedure TBACurrency.SetAsCurrency(Value: Currency);
begin
  SetDataValue(Value);
end;

function TBACurrency.GetAsCurrency: Currency;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsCurrency', '');
  EnsureNotNull; {ensures current}
  Result := FValue;
end;

function TBACurrency.GetAsFloat: Double;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsFloat', '');
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

function TBACurrency.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
var
  Temp: Currency;
begin
  if value = '' then
    result := CanSetToNull(nil)
  else
  begin
    result := TextToFloat(PChar(Value), temp, fvCurrency);
    if not result then
      FormatFailure(value, 'currency');
  end;
end;

function TBACurrency.ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean;
begin
  Result := CharInSet(C, ['0'..'9', '-', '+', 'e', 'E', {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DecimalSeparator]);
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

constructor TBACurrency.CreateWithValue(Value: currency);
begin
  inherited Create;
  asCurrency := Value;
end;

procedure TBACurrency.FreeContent;
begin
  inherited;
  fValue := 0;
end;

function TBACurrency.CanSetValue(NewValue: Currency; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetValue(NewValue, Subscriber);
{$IFNDEF BOLD_NO_QUERIES}
   result := result and   SendQuery(bqMaySetValue, [NewValue], Subscriber)
{$ENDIF}
end;

function TBACurrency.MaySetValue(NewValue: Currency;
  Subscriber: TBoldSubscriber): Boolean;
begin
  Result := True;
end;

procedure TBACurrency.AssignValue(const Source: IBoldValue);
var
  s: IBoldCurrencyContent;
begin
  if source.QueryInterface(IBoldCurrencyContent, S) = S_OK then
    SetDataValue(s.AsCurrency)
  else
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
end;

procedure TBACurrency.SetContent(NewValue: Currency);
var
  bContentIsNull: Boolean;
  sOldValue: Currency;
begin
  bContentIsNull := ContentIsNull;
  if (BoldPersistenceState = bvpsInvalid) or
    ContentIsNull or (FValue <> NewValue) then
  begin
    PreChange;
    sOldValue := fValue;
    FValue := NewValue;
    SetToNonNull;
    if bContentIsNull then begin
      Changed(beValueChanged, [NewValue]);
    end else begin
      Changed(beValueChanged, [NewValue, sOldValue]);
    end;
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

procedure TBACurrency.AssignContentValue(const Source: IBoldValue);
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
    raise EBold.CreateFmt('%s.AssignContentValue: unknown type of source', [classname]);
end;

function TBACurrency.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldCurrencyContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldCurrencyContent')
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBACurrency.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBACurrency_Proxy.MakeProxy(self, mode);
end;

{ TBoldBlobStream }

constructor TBoldBlobStream.Create(BlobAttr: TBABlob);
begin
  inherited Create;
  fBlobAttr := BlobAttr;
end;

function TBoldBlobStream.GetBlobSize: Int64;
begin
  Result := Length(fData);
end;

function TBoldBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count > Size - fPosition then
    Result := Size - fPosition else
    Result := Count;
  if Result > 0 then
  begin
    Move(PAnsiChar(fBlobAttr.AsStream.fData)[fPosition], Buffer, Result);
    Inc(FPosition, Result);
  end;
end;

function TBoldBlobStream.Write(const Buffer; Count: Longint): Longint;
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
        if not fBlobAttr.SupressEvents then
          fBlobAttr.StartModifyOfBlob('Write');
        if EndPos > Size then
        begin
          SetLength(fData, EndPos);
        end;
        System.Move(Buffer, PAnsiChar(fBlobAttr.AsStream.fData)[fPosition], Count);
        fPosition := EndPos;
        Result := Count;
        if not fBlobAttr.SupressEvents then
          fBlobAttr.EndModifyOfBlob;
      except
        if not fBlobAttr.SupressEvents then
          fBlobAttr.FailModifyOfBlob;
        raise;
      end;
    end;
  end;
end;

function TBoldBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    0: FPosition := Offset;
    1: Inc(FPosition, Offset);
    2: FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

procedure TBoldBlobStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
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
  try
    fBlobAttr.StartModifyOfBlob('LoadFromStream');
    Count := Stream.Size;
    InternalSetSize(Count, Count = 0);
    if Count <> 0 then
    begin
      Stream.ReadBuffer(fBlobAttr.AsStream.fData, Count);
    end;
    fBlobAttr.EndModifyOfBlob;
  except
    fBlobAttr.FailModifyOfBlob;
    raise;
  end;
end;

procedure TBoldBlobStream.LoadFromFile(const aFileName: string; aMode: Word = fmShareDenyNone);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(aFileName, aMode);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBoldBlobStream.SaveToStream(Stream: TStream);
begin
  fBlobAttr.EnsureContentsCurrent;
  if Size <> 0 then
    Stream.WriteBuffer(fBlobAttr.AsStream.fData, Size);
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

procedure TBoldBlobStream.SetSize(const NewSize: Int64);
begin
  if NewSize <> GetBlobSize then
    InternalSetSize(NewSize, not fBlobAttr.SupressEvents);
end;

procedure TBoldBlobStream.InternalSetSize(NewSize: Int64; BlobEvents: Boolean);
var
  OldPosition: Int64;
begin
  if NewSize = Size then
    exit;
  OldPosition := fPosition;
  if (NewSize = 0) and
    (not assigned(fBlobAttr.BoldAttributeRTInfo) or fBlobAttr.BoldAttributeRTInfo.AllowNull) then
    begin
      SetLength(fData, 0);
      if not fBlobAttr.SupressEvents and not fBlobAttr.BoldPersistenceStateIsInvalid then
        fBlobAttr.SetToNull;
    end
  else
    try
      if BlobEvents then
        fBlobAttr.StartModifyOfBlob('SetSize');
      SetLength(fData, NewSize);
      if BlobEvents then
        fBlobAttr.EndModifyOfBlob;
    except
      if BlobEvents then
        fBlobAttr.FailModifyOfBlob;
      raise
    end;
  if OldPosition > NewSize then
    Seek(0, soFromEnd);
end;

function TBoldBlobStream.IsDataSame(AData: Pointer; ASize: Int64): boolean;
begin
  result := false;
  if ASize = Length(fData) then
    result := CompareMem(AData, fData, ASize);
end;

{ TBABlob }

procedure TBABlob.SetDataValue(NewValue: TBoldAnsiString);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', '');

  if IsNull or not AsStream.IsDataSame(PAnsiChar(NewValue), Length(NewValue)) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', '');
    try
      SetContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBABlob.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, 'SetStringRepresentation', 'String validation failed');
  case Representation of
    brDefault: SetDataValue(TBoldAnsiString(Value));
    brShort: {Content type is lost when assigned to a Blob};
  else
    inherited SetStringRepresentation(Representation, Value);
  end;
end;

function TBABlob.SupressEvents: Boolean;
begin
  result := fSupressEventCount > 0;
end;

procedure TBABlob.BeginSupressEvents;
begin
  Inc(fSupressEventCount);
  StartModifyOfBlob('Write');
end;

procedure TBABlob.EndSupressEvents;
begin
  EndModifyOfBlob;
  Dec(fSupressEventCount);
  Assert(fSupressEventCount >= 0);
end;

function TBABlob.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetStringRepresentation', '');
  case Representation of
    brDefault: begin
      if IsNull then {IsNull ensures current}
        Result := ''
      else
        Result := TEncoding.Default.GetString(AsStream.fData);
    end;
    brShort: begin
      Result := '';
    end;
  else
    Result := inherited GetStringRepresentation(Representation);
  end;
end;

function TBABlob.GetAsBlob: TBoldAnsiString;
begin
  result := TBoldAnsiString(TEncoding.Default.GetString(AsStream.fData));
end;

function TBABlob.GetAsStream: TBoldBlobStream;
begin
  if not SupressEvents then
    EnsureContentsCurrent;
  if not Assigned(fStream) then
    fStream := TBoldBlobStream.Create(self);
  result := fStream;
end;

function TBABlob.GetBlobSize: Int64;
begin
  result := AsStream.GetBlobSize;
end;

function TBABlob.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSBlob;
end;

procedure TBABlob.SetAsBlob(NewValue: TBoldAnsiString);
begin
  SetStringRepresentation(brDefault, String(NewValue));
end;

procedure TBABlob.SetAsVariant(const Value: Variant);
var
  p: Pointer;
begin
  if VarIsArray(Value) then
  begin
    if VarType(Value) <> VarArray + VarByte then
      raise EBold.CreateFmt('%s.SetAsVariant: Unsupported type of variant array, type: %d.', [classname, VarType(Value)]);
    AsStream.Clear;
    p := VarArrayLock(Value);
    try
      AsStream.Write(p ^, VarArrayHighBound(Value, 1));
    finally
      VarArrayUnlock(Value);
    end;
  end
  else
    inherited SetAsVariant(Value);
end;

procedure TBABlob.SetToNull;
begin
  inherited;
  AsStream.Clear;
end;

procedure TBABlob.StartModifyOfBlob(const Operation: String);
begin
  if not SupressEvents and not StartModify then
    BoldRaiseLastFailure(self, 'BlobStream: '+Operation, '');
  PreChange;
end;

procedure TBABlob.EndModifyOfBlob;
begin
  if (GetBlobSize<>0) then
    SetToNonNull;
  // Old value is not passed in as parameter when Blob is set via Stream
  // TODO: If Old value is needed store it in StartModifyOfBlob
  Changed(beValueChanged, [fStream.fData]);
  if not SupressEvents then
    EndModify;
end;

procedure TBABlob.FailModifyOfBlob;
begin
  if SupressEvents then
    exit;
  FailModify;
end;

procedure TBABlob.Assign(Source: TBoldElement);
begin
  if (Source is TBABlob) then
  begin
    if TBABlob(Source).IsNull then
      SetToNull
    else
    begin
      TBABlob(Source).AsStream.Position:=0;
      LoadFromStream(TBABlob(Source).AsStream);
      ContentType := TBABlob(Source).ContentType;
    end;
    Exit;
  end
  else
    inherited;
end;

function TBABlob.CanSetValue(NewValue: TBoldAnsiString;  Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetValue(NewValue, Subscriber);
{$IFNDEF BOLD_NO_QUERIES}
   result := result and   SendQuery(bqMaySetValue, [NewValue], Subscriber)
{$ENDIF}
end;

function TBABlob.MaySetValue(NewValue: TBoldAnsiString;
  Subscriber: TBoldSubscriber): Boolean;
begin
  Result := True;
end;

procedure TBABlob.AssignValue(const Source: IBoldValue);
var
  s: IBoldBlobContent;
begin
  if source.QueryInterface(IBoldBlobContent, S) = S_OK then
    SetDataValue(s.AsBlob)
  else
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
end;

procedure TBABlob.SetContent(NewValue: TBoldAnsiString);
var
  bContentIsNull: Boolean;
  aOldValue: TBytes;
begin
  bContentIsNull := ContentIsNull;
  if bContentIsNull or not AsStream.IsDataSame(PAnsiChar(NewValue), Length(NewValue)) then
  begin
    PreChange;
    aOldValue := Copy(AsStream.fData, 0, BlobSize);
    if NewValue = '' then
    begin
      AsStream.Clear;
      AsStream.Seek(0, soFromBeginning);
    end
    else
    begin
      AsStream.fData := BytesOf(NewValue);
      AsStream.Seek(0, soFromEnd);
    end;
    SetToNonNull;
    if bContentIsNull then begin
      Changed(beValueChanged, [AsStream.fData]);
    end else begin
      Changed(beValueChanged, [AsStream.fData, aOldValue]);
    end;
  end;
end;

procedure TBABlob_Proxy.BeginSupressEvents;
begin
  ProxedBlob.BeginSupressEvents;
end;

procedure TBABlob_Proxy.EndSupressEvents;
begin
  ProxedBlob.EndSupressEvents;
end;

function TBABlob_Proxy.GetBlobAsStream: TStream;
begin
  result := ProxedBlob.AsStream;
end;

function TBABlob_Proxy.GetContentAsBlob: TBoldAnsiString;
begin
  SetString(result, PAnsiChar(ProxedBlob.AsStream.fData), ProxedBlob.BlobSize);
end;

procedure TBABlob_Proxy.SetContentAsBlob(const NewValue: TBoldAnsiString);
begin
  ProxedBlob.SetContent(NewValue);
end;

procedure TBABlob.AssignContentValue(const Source: IBoldValue);
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
    raise EBold.CreateFmt('%s.AssignContentValue: unknown type of source', [classname]);
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
      if CompareBlob.BlobSize > MaxInt then
      begin // memory compare, 2 results
        if AsStream.IsDataSame(CompareBlob.AsStream.fData, CompareBlob.BlobSize) then
          result := 0
        else
          result := -1;
      end
      else // string compare, 3 results
        Result := StringCompare(CompareType, AsString, CompareBlob.AsString);
    end;
  end
  else
    Result := inherited CompareToAs(CompareType, BoldElement);
end;

destructor TBABlob.Destroy;
begin
  FreeAndNil(fStream);
  inherited;
end;

procedure TBABlob.Initialize;
begin
  inherited;
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
      result := CompareBlob.AsStream.fData = AsStream.fData;
  end
  else
    Result := inherited IsEqualAs(CompareType, BoldElement);
end;

function TBABlob.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  vBlob: IBoldBlobContent;
begin
  Assert(Assigned(Value), ClassName + '.IsEqualToValue: Value can not be nil.');
  if Value.QueryInterface(IBoldBlobContent, vBlob) = S_OK then
  begin
    if IsNull and vBlob.IsNull then
      result := true
    else
    if IsNull or vBlob.IsNull then
      result := false
    else
      result := Self.GetAsBlob = vBlob.asBlob
  end
  else
    result := inherited IsEqualToValue(Value);
end;

function TBABlob.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldBlobContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldBlobContent')
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBABlob.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBABlob_Proxy.MakeProxy(self, mode);
end;

procedure TBABlob.FreeContent;
begin
  inherited;
  if assigned(fStream) then
    fStream.Clear;
end;

procedure TBABlob.SetEmptyValue;
begin
  if Assigned(BoldAttributeRTInfo) and not BoldAttributeRTInfo.AllowNull then
    asString := ''
  else
    SetContentToNull;
end;

procedure TBABlob.LoadFromFile(const aFileName: string; aMode: Word);
begin
  AsStream.LoadFromFile(aFilename, aMode);
end;

procedure TBABlob.LoadFromStream(Stream: TStream);
begin
  AsStream.LoadFromStream(Stream);
end;

procedure TBABlob.SaveToFile(const FileName: string);
begin
  AsStream.SaveToFile(FileName);
end;

procedure TBABlob.SaveToStream(Stream: TStream);
begin
  AsStream.SaveToStream(Stream);
end;

{ TBATypedBlob }
procedure TBATypedBlob.SetContentType2(Value: string);
begin
  BoldClearLastFailure;
  if not CanSetContentType(Value, nil) then
    BoldRaiseLastFailure(self, 'SetContentType', '');

  if ContentIsNull or (FContentType <> Value) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetContentType', '');
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
    BoldRaiseLastFailure(self, 'GetContentType', '');
  result := ContentType;
end;

function TBATypedBlob.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSTypedBlob;
end;

function TBATypedBlob.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  case Representation of
    brShort: begin
      if not CanRead(nil) then
        BoldRaiseLastFailure(self, 'GetStringRepresentation', '');
      Result := FContentType;
    end;
  else
    Result := inherited GetStringRepresentation(Representation);
  end;
end;

procedure TBATypedBlob.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  case Representation of
    brShort: begin
      if not ValidateString(Value, Representation) then
        BoldRaiseLastFailure(self, 'SetStringRepresentation', 'String validation failed');
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
{$IFDEF BOLD_NO_QUERIES}
   result := True;
{$ELSE}
  Result := SendQuery(bqMaySetContentType, [Value], Subscriber);
{$ENDIF}
end;

procedure TBATypedBlob.AssignValue(const Source: IBoldValue);
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
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
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

function TBATypedBlob.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  vTypedBlob: IBoldTypedBlob;
begin
  Assert(Assigned(Value), ClassName + '.IsEqualToValue: Value can not be nil.');
  if Value.QueryInterface(IBoldTypedBlob, vTypedBlob) = S_OK then
    result := (Self.ContentType = vTypedBlob.ContentTypeContent) and inherited IsEqualToValue(Value)
  else
    result := inherited IsEqualToValue(Value);
end;

{-- TBABlobImageJPEG --}
procedure TBABlobImageJPEG.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  case Representation of
    brShort: begin
      if (Value<>'') and (Value<>ContentType) then
        raise EBold.CreateFmt('%s.SetStringRepresentation: Can not assign a ''%s'' to a ''%s''', [ClassName, Value, ContentType]);
    end;
  else
    inherited SetStringRepresentation(Representation, Value);
  end;
end;

function TBABlobImageJPEG.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  case Representation of
    brShort: begin
      Result := 'image/jpeg';
    end;
  else
    inherited GetStringRepresentation(Representation);
  end;
end;

{-- TBABlobImageBMP --}
procedure TBABlobImageBMP.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  case Representation of
    brShort: begin
      if (Value <> '') and (Value <> ContentType) then
        raise EBold.CreateFmt('%s.SetStringRepresentation: Can not assign a ''%s'' to a ''%s''', [ClassName, Value, ContentType]);
    end;
  else
    inherited SetStringRepresentation(Representation, Value);
  end;
end;

function TBABlobImageBMP.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  case Representation of
    brShort: begin
      Result := 'image/bitmap';
    end;
  else
    inherited GetStringRepresentation(Representation);
  end;
end;

function TBATypedBlob.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldTypedBlob) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldTypedBlob')
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBATypedBlob.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBATypedBlob_Proxy.MakeProxy(self, mode);
end;

procedure TBATypedBlob.SetContentTypeContent(NewValue: String);
begin
  PreChange;
  FContentType := NewValue;
  SetToNonNull;
  Changed(beValueChanged, [AsStream.fData]);
end;

{ TBAMoment }

procedure TBAMoment.SetDataValue(NewValue: TDateTime);
begin
  BoldClearLastFailure;
{$IFDEF NoNegativeDates}
  if IsPartOfSystem then
  begin
  if (NewValue < 0) then
    BoldRaiseLastFailure(self, 'SetDataValue', 'Attempt to set date before 1899-12-30');
  if (NewValue >= 1000*365) then
    BoldRaiseLastFailure(self, 'SetDataValue', 'Attempt to set date after 2899-12-30');
  end;
{$ENDIF}
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', '');

  if IsNull or (FValue <> NewValue) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', '');
    try
      SetDateTimeContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

function TBAMoment.CanSetValue(NewValue: TDateTime; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetValue(NewValue, Subscriber);
{$IFNDEF BOLD_NO_QUERIES}
   result := result and   SendQuery(bqMaySetValue, [NewValue], Subscriber)
{$ENDIF}
end;

function TBAMoment.MaySetValue(NewValue: TDateTime;
  Subscriber: TBoldSubscriber): Boolean;
begin
{$IFDEF NoNegativeDates}
  result := not IsPartOfSystem or ((NewValue >= 0) and (NewValue < 1000*365000));
{$ELSE}
  result := true;
{$ENDIF}
end;

function TBAMoment.GetAsDateTime: TDateTime;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsDateTime', '');
  EnsureNotNull; {ensures current}
  Result := FValue;
end;

function TBAMoment.GetAsFloat: Double;
begin
  Result := GetAsDateTime;
end;

function TBAMoment.GetAsDate: TDateTime;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsDate', '');
  EnsureNotNull; {ensures current}
  Result := Int(fValue);;
end;

function TBAMoment.GetAsTime: TDateTime;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsTime', '');
  EnsureNotNull; {ensures current}
  Result := frac(fValue);
end;

procedure TBAMoment.SetAsDateTime(Value: TDateTime);
begin
  SetDataValue(Value);
end;

procedure TBAMoment.SetAsInteger(Value: integer);
begin
  SetAsDate(Value);
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

procedure TBAMoment.FreeContent;
begin
  inherited;
  FValue := 0;
end;

procedure TBAMoment.SetDateTimeContent(NewValue: TDateTime);
var
  bContentIsNull: Boolean;
  sOldValue: TDateTime;
begin
{$IFDEF NoNegativeDates}
  if IsPartOfSystem then
  begin
  if NewValue < 0 then
    raise EBoldInternal.Create('TBAMoment.SetContent setting a negative value. Should have been prevented by MaySetValue');
  if NewValue >=1000*365 then
    raise EBoldInternal.Create('TBAMoment.SetContent setting a too big value. Should have been prevented by MaySetValue');
  end;
{$ENDIF}
  bContentIsNull := ContentIsNull;
  if (BoldPersistenceState = bvpsInvalid) or
    ContentIsNull or (FValue <> NewValue) then
  begin
    PreChange;
    sOldValue := fValue;
    FValue := NewValue;
    SetToNonNull;
    if bContentIsNull then begin
      Changed(beValueChanged, [NewValue]);
    end else begin
      Changed(beValueChanged, [NewValue, sOldValue]);
    end;
  end;
end;

function TBAMoment_Proxy.GetContentAsDate: TDateTime;
begin
  result := Int(ProxedMoment.fValue);
end;

procedure TBAMoment_Proxy.SetContentAsDate(NewValue: TDateTime);
begin
  ProxedMoment.SetDateTimeContent(Int(NewValue) + GetContentAsTime);
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
  ProxedMoment.SetDateTimeContent(NewValue);
end;

procedure TBAMoment_Proxy.SetContentAsTime(NewValue: TDateTime);
begin
  ProxedMoment.SetDateTimeContent(frac(NewValue) + GetContentAsDate);
end;

procedure TBAMoment.SetEmptyValue;
begin
  if Assigned(BoldAttributeRTInfo) and not BoldAttributeRTInfo.AllowNull then
    SetAsDateTime(0)
  else
    SetContentToNull;
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

function TBAMoment.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  vDateTime: IBoldDateTimeContent;
  vDate: IBoldDateContent;
  vTime: IBoldTimeContent;
begin
  Assert(Assigned(Value), ClassName + '.IsEqualToValue: Value can not be nil.');
  if Value.QueryInterface(IBoldDateTimeContent, vDateTime) = S_OK then
  begin
    if Self.IsNull and vDateTime.IsNull then
      result := true
    else
    if Self.IsNull or vDateTime.IsNull then
      result := false
    else
      result := Self.asDateTime = vDateTime.asDateTime
  end
  else
  if Value.QueryInterface(IBoldDateContent, vDate) = S_OK then
  begin
    if Self.IsNull and vDate.IsNull then
      result := true
    else
    if Self.IsNull or vDate.IsNull then
      result := false
    else
      result := Self.asDate = vDate.asDate
  end
  else
  if Value.QueryInterface(IBoldTimeContent, vTime) = S_OK then
  begin
    if Self.IsNull and vTime.IsNull then
      result := true
    else
    if Self.IsNull or vTime.IsNull then
      result := false
    else
      result := Self.asTime = vTime.asTime
  end
  else
    result := inherited IsEqualToValue(Value);
end;

function TBAMoment.IsNullOrZero: boolean;
begin
  result := isNull or (GetAsDateTime = 0);
end;

function TBAMoment.IsVariantTypeCompatible(const Value: Variant): Boolean;
begin
  result := (VarType(Value) in [varDate, varDouble, varInteger, varInt64]);
end;

{ TBADateTime }

procedure TBADateTime.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, 'SetStringRepresentation', 'String validation failed');
  if Representation = brDefault then
    if Value = '' then
      SetToNull
    else if upperCase(Value) = '<NOW>' then
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
  Result := CharInSet(C, ['0'..'9', ' ', {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}TimeSeparator, {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DateSeparator]);
end;

function TBADateTime.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
begin
  try
    if value = '' then
      result := CanSetToNull(nil)
    else if upperCase(Value) = '<NOW>' then
      result := true
    else
    begin
      StrToDateTime(Value);
      Result := True;
    end;
  except
    Result := False;
    FormatFailure(value, 'datetime');
  end;
end;

{---TBADate---}
procedure TBADate.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, 'SetStringRepresentation', 'String validation failed');
  if Representation = brDefault then
    if Value = '' then
      SetToNull
    else if upperCase(Value) = '<NOW>' then
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
  Result := CharInSet(C, ['0'..'9', {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DateSeparator]);
end;

function TBADate.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
begin
  try
    if value = '' then
      result := CanSetToNull(nil)
    else if upperCase(Value) = '<NOW>' then
      result := true
    else
    begin
      StrToDate(Value);
      Result := True;
    end;
  except
    Result := False;
    FormatFailure(value, 'date');
  end;
end;

{---TBATime---}
procedure TBATime.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, 'SetStringRepresentation', 'String validation failed');
  if Representation = brDefault then
    if Value = '' then
      SetToNull
    else if upperCase(Value) = '<NOW>' then
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
  Result := CharInSet(C, ['0'..'9', {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}TimeSeparator]);
end;

function TBATime.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
begin
  try
    if value = '' then
      result := CanSetToNull(nil)
    else if upperCase(Value) = '<NOW>' then
      result := true
    else
    begin
      StrToTime(Value);
      Result := True;
    end;
  except
    Result := False;
    FormatFailure(value, 'time');
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

procedure TBAValueSetValue.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(nil, 'SetStringRepresentation', 'String validation failed');
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

function TBAValueSetValue.CompareToAs(CompareType: TBoldCompareType;
  BoldElement: TBoldElement): Integer;
begin
  if BoldElement is TBAValueSetValue then
  begin
    if (TBAValueSetValue(BoldElement).AsInteger = AsInteger) then
      Result := 0
    else
    if AsInteger > TBAValueSetValue(BoldElement).AsInteger then
      Result := 1
    else
      Result := -1;
  end
  else
    Result := inherited CompareToAs(CompareType, BoldElement);
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
  inherited Create;
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
  i, j: Integer;
  aValue: TBAValueSetValue;
begin
  Result := nil;
  for I := 0 to FValueList.Count - 1 do
  begin
    aValue := TBAValueSetValue(FValueList.Items[I]);
    for j := 0 to aValue.StringRepresentationCount - 1 do
      if SameText(aValue.StringRepresentation[j], Value) then
      begin
        Result := FValueList.Items[I];
        exit;
      end;
  end;
end;

function TBAValueSetValueList.FindByText(Representation: TBoldRepresentation; Value: string): TBAValueSetValue;
var
  i, j: Integer;
  aValue: TBAValueSetValue;
begin
  Result := nil;
  for I := 0 to FValueList.Count - 1 do
  begin
    aValue := TBAValueSetValue(FValueList.Items[I]);
    for j := 0 to aValue.StringRepresentationCount - 1 do
      if AnsiSameText(aValue.StringRepresentation[j], Value) then
      begin
        Result := FValueList.Items[I];
        exit;
      end;
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


procedure TBADateTime.AssignValue(const Source: IBoldValue);
var
  s: IBoldDateTimeContent;
begin
  if source.QueryInterface(IBoldDateTimeContent, S) = S_OK then
    SetDataValue(s.AsDateTime)
  else
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
end;

constructor TBADateTime.CreateWithValue(Value: TDateTime);
begin
  inherited Create;
  asDateTime := Value;
end;

procedure TBADateTime.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldDateTimeContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
    SetContentToNull
  else if not assigned(source) then
    SetDateTimeContent(0)
  else if source.QueryInterface(IBoldDateTimeContent, S) = S_OK then
  begin
    if s.IsNull then
      SetContentToNull
    else
      SetDateTimeContent(s.AsDateTime)
  end
  else
    raise EBold.CreateFmt('%s.AssignContentValue: unknown type of source', [classname]);
end;

function TBADateTime.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldDateTimeContent) or
     IsEqualGuid(IID, IBoldDateContent) or
     IsEqualGuid(IID, IBoldTimeContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBold[Date][Time]Content')
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBADateTime.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSDateTime;
end;

function TBADateTime.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBADateTime_Proxy.MakeProxy(self, mode);
end;

{ TBAValueSet }

procedure TBAValueSet.SetDataValue(NewValue: TBAValueSetValue);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', '');
  if not Assigned(NewValue) then
    raise EBold.CreateFmt('%s: Invalid value', [ClassName]);

  if IsNull or not (FValue.IsEqual(NewValue)) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', '');
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
    Result := ''
  else
    Result := FValue.StringRepresentation[Representation];
end;

class function TBAValueSet.GetValues: TBAValueSetValueList;
begin
  result := nil;
end;

procedure TBAValueSet.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
var
  ValueSetValue: TBAValueSetValue;
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, 'SetStringRepresentation', 'String validation failed');
  ValueSetValue := Values.FindByString(Representation, Value);
  assert(assigned(ValueSetValue), 'ValidateString said OK, but SetStringRepresentation can not find a valuesetvalue');
  SetDataValue(ValueSetValue)
end;

function TBAValueSet.GetAsInteger: Integer;
begin
  BoldClearLastFailure;
  CheckIllegalValue;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsInteger', '');
  EnsureNotNull; {ensures current}
  Result := FValue.AsInteger;
end;

procedure TBAValueSet.SetAsInteger(Value: Integer);
begin
  assert(Assigned(Self), 'TBAValueSet.SetAsInteger: Self = nil');
  SetDataValue(Values.FindByInteger(Value));
end;

function TBAValueSet.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
var
  ValueSetvalue: TBAValueSetValue;
  strList: TStringList;
  Str: String;
begin
  result := false;
  if Value = '' then
    exit;
  ValueSetValue := Values.FindByString(Representation, Value);
  Result := Assigned(ValueSetvalue);

  if not Result then
  begin
    strList := TStringList.create;
    try
      Values.ToStrings(Representation, strList);
      Str := BoldSeparateStringList(StrList, ', ', '(', ')');
      SetBoldLastFailureReason(TBoldFailureReason.CreateFmt('Unknown stringvalue ''%s'', Allowed values: %s', [Value, Str], self));
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
  if BoldElement is TBAValueSetValue then
  begin
    // Since TBAValueSetValue has no BoldType we compare both Integer and String values to make sure they are the same 
    if (AsInteger = TBAValueSetValue(BoldElement).AsInteger) and (TBAValueSetValue(BoldElement).AsString = AsString) then
      Result := 0
    else
      Result := -1;
  end
  else
  if BoldElement is TBAInteger then
  begin
    // Since TBAValueSetValue has no BoldType we compare both Integer and String values to make sure they are the same
    if (AsInteger = TBAInteger(BoldElement).AsInteger) {and (TBAValueSetValue(BoldElement).AsString = AsString)} then
      Result := 0
    else
      Result := -1;
  end
  else
  if BoldElement is TBAString then
  begin
    if (AsString = TBAString(BoldElement).AsString) then
      Result := 0
    else
      Result := -1;
  end
  else
    Result := inherited CompareToAs(CompType, BoldElement);
end;

function TBAValueSet.CanSetValue(NewValue: TBAValueSetValue; Subscriber: TBoldSubscriber): Boolean;
begin
  result := MaySetValue(NewValue, Subscriber);
{$IFNDEF BOLD_NO_QUERIES}
   result := result and SendQuery(bqMaySetValue, [NewValue], Subscriber)
{$ENDIF}
end;

function TBAValueSet.MaySetValue(NewValue: TBAValueSetValue;
  Subscriber: TBoldSubscriber): Boolean;
begin
  result := True;
end;

procedure TBAValueSet.Initialize;
begin
  inherited;
  if not Assigned(Values) then
    raise EBold.CreateFmt('%s: Values not properly initalized', [ClassName]);
  if assigned(values.FValueList) and (values.fValueList.count <> 0) then
    FValue := Values.GetFirstValue
  else
    setToNull;
end;

procedure TBAValueSet.AssignValue(const Source: IBoldValue);
var
  s: IBoldIntegerContent;
begin
  if source.QueryInterface(IBoldIntegerContent, S) = S_OK then
    SetDataValue(Values.FindByInteger(s.AsInteger))
  else
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
end;

procedure TBAValueSet.SetContent(NewValue: TBAValueSetValue);
var
  bContentIsNull: Boolean;
  sOldValue: TBAValueSetValue;
begin
  bContentIsNull := ContentIsNull;
  if (BoldPersistenceState = bvpsInvalid) or
     bContentIsNull or not (fValue.IsEqual(NewValue)) then
  begin
    PreChange;
    sOldValue := fValue;
    fValue := NewValue;
    SetToNonNull;
    if bContentIsNull then begin
      Changed(beValueChanged, [NewValue]);
    end else begin
      Changed(beValueChanged, [NewValue, sOldValue]);
    end;
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

function TBAValueSet.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSInteger;
end;

procedure TBAValueSet.SetContentAsInteger(NewValue: Integer);
begin
  SetContent(Values.FindByInteger(NewValue));
end;

procedure TBAValueSet_Proxy.SetContentAsInteger(NewValue: Integer);
begin
  ProxedValueSet.ContentAsInteger:= NewValue;
end;

procedure TBAValueSet.AssignContentValue(const Source: IBoldValue);
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
    raise EBold.CreateFmt('%s.AssignContentValue: unknown type of source', [classname]);
end;

function TBAValueSet.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldIntegerContent) or
     IsEqualGuid(IID, IBoldStringContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldString/IntegerContent')
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBAValueSet.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBAValueSet_Proxy.MakeProxy(self, mode);
end;

procedure TBAValueSet.SetEmptyValue;
begin
  if Values.Count > 0 then
  begin
    if AsInteger <> Values.GetFirstValue.AsInteger then
      AsInteger := Values.GetFirstValue.AsInteger
  end
  else if not assigned(BoldAttributeRTInfo) or BoldAttributeRTInfo.AllowNull then
    SetToNull
  else
    raise EBold.CreateFmt('%s.SetEmptyValue: There are no legal values, and null is not allowed for this attribute (%s)', [classname, '']);
end;

procedure TBAValueSet.CheckIllegalValue;
begin
  if not IsNull and not Assigned(FValue) then
  begin
    if BoldPersistenceState = bvpsCurrent then
      raise EBold.Create('Illegal value in valueset. Bad data in database?')
    else
      raise EBold.Create('Illegal value in valueset. Cannot read.')
  end;
end;

function TBAValueSet.CompareToEnumLiteral(const str: String): Boolean;
begin
  result := SameText(
    Str,
    BoldExpandName(AsString, '', xtExpression, -1, nccTrue))
end;

procedure TBAValueSet.FreeContent;
begin
  inherited;
  if Values.Count > 0 then
    fValue := Values.GetFirstValue;
end;

{ TBABoolean }
constructor TBABoolean.CreateWithValue(Value: Boolean);
begin
  inherited Create;
  AsBoolean := Value;
end;

function TBABoolean.GetAsBoolean: Boolean;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetAsBoolean', '');
  EnsureNotNull; {ensures current}
  Result := Boolean(GetContentAsInteger);
end;

procedure TBABoolean.SetAsBoolean(Value: Boolean);
begin
  SetAsInteger(Integer(Value));
end;

class function TBABoolean.GetValues: TBAValueSetValueList;
begin
  if not Assigned(_BooleanValues) then
  begin
    _BooleanValues := TBAValueSetValueList.Create;
       //representation              1       2      3
       //---------------------------------------------
    _BooleanValues.Add(Ord(False), ['N', 'F', 'False']); // do not localize
    _BooleanValues.Add(Ord(True), ['Y', 'T', 'True']); // do not localize
{
    _BooleanValues.Add(Ord(False), ['F',    'F', 'False']);
    _BooleanValues.Add(Ord(False), ['False','F', 'False']);
    _BooleanValues.Add(Ord(False), ['N',    'F', 'False']);
    _BooleanValues.Add(Ord(True),  ['T',    'T', 'True']);
    _BooleanValues.Add(Ord(True),  ['True', 'T', 'True']);
    _BooleanValues.Add(Ord(True),  ['Y',    'T', 'True']);
}
  end;
  Result := _BooleanValues;
end;

function TBABoolean.IsEqualToValue(const Value: IBoldValue): Boolean;
var
  vBoolean: IBoldBooleanContent;
begin
  Assert(Assigned(Value), ClassName + '.IsEqualToValue: Value can not be nil.');
  if Value.QueryInterface(IBoldBooleanContent, vBoolean) = S_OK then
  begin
    if IsNull and vBoolean.IsNull then
      result := true
    else
    if IsNull or vBoolean.IsNull then
      result := false
    else
      result := Self.AsBoolean = vBoolean.asBoolean;
  end
  else
    result := inherited IsEqualToValue(Value);
end;

function TBABoolean.IsVariantTypeCompatible(const Value: Variant): Boolean;
begin
  result := (VarType(Value) in [varBoolean]);
end;

function TBAInteger.GetAsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := AsInteger;
end;

function TBAInteger.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSInteger;
end;

procedure TBAInteger.SetAsVariant(const Value: Variant);
begin
  if IsVariantTypeCompatible(Value) then
    AsInteger := Value
  else
    inherited SetAsVariant(Value);
end;

function TBAFloat.GetAsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := AsFloat;
end;

function TBAFloat.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSFloat;
end;

procedure TBAFloat.SetAsVariant(const Value: Variant);
begin
  if IsVariantTypeCompatible(Value) then
    AsFloat := Value
  else
    inherited SetAsVariant(Value);
end;

function TBACurrency.GetAsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := AsCurrency;
end;

function TBACurrency.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSCurrency;
end;

procedure TBACurrency.SetAsVariant(const Value: Variant);
begin
  if IsVariantTypeCompatible(Value) then
    AsCurrency := Value
  else
    inherited SetAsVariant(Value);
end;

function TBAMoment.GetAsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := GetAsDateTime;
end;

function TBAMoment.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
end;

procedure TBAMoment.SetAsVariant(const Value: Variant);
begin
  if IsVariantTypeCompatible(Value) then
    SetAsDateTime(Value)
  else
    inherited SetAsVariant(Value);
end;

function TBAValueSet.GetAsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := AsString;
end;

function TBAValueSet.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
end;

procedure TBAValueSet.SetAsVariant(const Value: Variant);
begin
  AsString := Value;
end;

function TBABoolean.GetAsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := AsBoolean;
end;

function TBABoolean.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSBoolean;
end;

procedure TBABoolean.SetAsVariant(const Value: Variant);
begin
  if IsVariantTypeCompatible(Value) then
    AsBoolean := Value
  else
    inherited SetAsVariant(Value);
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

procedure TBADate.AssignValue(const Source: IBoldValue);
var
  s: IBoldDateContent;
begin
  if source.QueryInterface(IBoldDateContent, S) = S_OK then
    SetDataValue(s.AsDate)
  else
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
end;

constructor TBADate.CreateWithValue(Value: TDateTime);
begin
  inherited Create;
  asDate := Value;
end;

procedure TBATime.AssignValue(const Source: IBoldValue);
var
  s: IBoldTimeContent;
begin
  if source.QueryInterface(IBoldTimeContent, S) = S_OK then
    SetDataValue(s.AsTime)
  else
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
end;

constructor TBATime.CreateWithValue(Value: TDateTime);
begin
  inherited Create;
  AsTime := Value;
end;

function TBABoolean_Proxy.GetContentAsBoolean: Boolean;
begin
  Result := Boolean(GetContentAsInteger);
end;

procedure TBABoolean_Proxy.SetContentAsBoolean(NewValue: Boolean);
begin
  SetContentAsInteger(Integer(NewValue));
end;

procedure TBADate.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldDateContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
    SetContentToNull
  else if not assigned(source) then
    SetDateTimeContent(0)
  else if source.QueryInterface(IBoldDateContent, S) = S_OK then
  begin
    if s.IsNull then
      SetContentToNull
    else
      SetDateTimeContent(s.AsDate)
  end
  else
    raise EBold.CreateFmt('%s.AssignContentValue: unknown type of source', [classname]);
end;

procedure TBATime.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldTimeContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
    SetContentToNull
  else if not assigned(source) then
    SetDateTimeContent(0)
  else if source.QueryInterface(IBoldTimeContent, S) = S_OK then
  begin
    if s.IsNull then
      SetContentToNull
    else
      SetDateTimeContent(s.AsTime)
  end
  else
    raise EBold.CreateFmt('%s.AssignContentValue: unknown type of source', [classname]);
end;

procedure TBABoolean.AssignContentValue(const Source: IBoldValue);
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
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldBooleanContent')
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBABoolean.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBABoolean_Proxy.MakeProxy(self, mode);
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

destructor TBAConstraint.destroy;
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
        begin
        {$IFDEF OCLConstraintMessages}
          if (OwningElement is TBoldDomainElement) then
          begin
            if Assigned(OwningElement.Evaluator.ExpressionType(Constraint.ConstraintMessage, OwningElement.BoldType, false)) then
              result := TBoldDomainElement(OwningElement).EvaluateExpressionAsString(Constraint.ConstraintMessage, brDefault)
            else
              result := Constraint.ConstraintMessage;
          end
          else
        {$ENDIF}
          begin
            result := Constraint.ConstraintMessage;
          end;
        end;
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

procedure TBAConstraint.InitializeConstraint(Constraint: TBoldConstraintRTInfo; OwningElement: TBoldElement);
var
  ResType: TBoldElementTypeInfo;
const
  sInvalidConstraint = 'Invalid Constraint: %s %s%s';
  sInvalidConstraintMessage = 'Invalid Constraint message: %s %s%s';
  sUnknownConstraintType = 'unknown type of constraint expression: %s (in context: %s)';
  sUnknownConstraintMessageType = 'unknown type of constraint message: %s (in context: %s)';
  sConstraintNotBoolean = 'Constraint is not Boolean: %s (in context %s)';
  sConstraintMessageString = 'Constraint message is not String: %s (in context %s)';
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
        raise EBold.CreateFmt('Invalid Constraint: %s ' + BOLDCRLF + '%s', [Constraint.ConstraintExpression, e.message]);
    end;

    if not assigned(ResType) then
      raise EBold.CreateFmt('unknown type of constraint expression: %s (in context: %s)', [Constraint.ConstraintExpression, OwningElement.BoldType.AsString]);

    if not ResType.ConformsTo((BoldType.SystemTypeInfo as TboldSystemTypeInfo).AttributeTypeInfoByDelphiName[TBABoolean.ClassName]) then
      raise EBold.CreateFmt('Constraint is not Boolean: %s (in context %s)', [Constraint.ConstraintExpression, OwningElement.BoldType.AsString]);

  {$IFDEF OCLConstraintMessages}
    if Constraint.ConstraintMessage <> '' then
    begin
      try
        resType := Owningelement.Evaluator.ExpressionType(Constraint.ConstraintMessage, OwningElement.BoldType, false);
        if not Assigned(resType) then
          resType := Owningelement.Evaluator.ExpressionType(QuotedStr(Constraint.ConstraintMessage), OwningElement.BoldType, false);
      except
        on e: Exception do
          raise EBold.CreateFmt(sInvalidConstraintMessage, [Constraint.ConstraintMessage, TBoldDomainElement(OwningElement).DisplayName, e.message]);
      end;

      if not assigned(ResType) then
        raise EBold.CreateFmt(sUnknownConstraintMessageType, [Constraint.ConstraintMessage, OwningElement.BoldType.AsString]);

      if not ResType.ConformsTo((BoldType.SystemTypeInfo as TboldSystemTypeInfo).AttributeTypeInfoByDelphiName[TBAString.ClassName]) then
        raise EBold.CreateFmt(sConstraintMessageString, [Constraint.ConstraintMessage, OwningElement.BoldType.AsString]);
    end;
  {$ENDIF}
  end;
end;

procedure TBAConstraint.Initialize;
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
    10: ; // do nothing, static values...
    11:
      {$IFDEF OCLConstraintMessages}
      if assigned(OwningElement) and (Constraint.ConstraintMessage <> '') then
      begin
        if Assigned(OwningElement.Evaluator.ExpressionType(Constraint.ConstraintMessage, OwningElement.BoldType, false)) then
          OwningElement.SubscribeToExpression(Constraint.ConstraintMessage, Subscriber, RequestedEvent = breReSubscribe);
      end
      {$ENDIF};
    12..13: ; // do nothing, static values...
    14: if assigned(OwningElement) then
      OwningElement.SubscribeToStringRepresentation(brDefault, Subscriber, RequestedEvent);
    else inherited;
  end;
end;

procedure TBAConstraint.Assign(Source: TBoldElement);
begin
  inherited;
  if Source is TBAConstraint then
    InitializeConstraint(TBAConstraint(Source).Constraint, TBAConstraint(Source).OwningElement);
end;

{ TBAString_Proxy }

function TBAString_Proxy.GetProxedString: TBAString;
begin
  result := ProxedMember as TBAString;
end;

class function TBAString_Proxy.MakeProxy(ProxedMember: TBoldMember;
  Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  Result := fLastUsed[Mode];
  // Reuse proxy if we hold only reference
  if Assigned(Result) and (Result.RefCount =1) then
  begin
    Result.Retarget(ProxedMember, Mode);
  end
  else
  begin
    Result := Create(ProxedMember, Mode);
    fLastUsed[Mode] := Result;
    fLastUsedAsInterface[Mode] := Result;  // Inc refcount
  end;
end;

{ TBAInteger_Proxy }

function TBAInteger_Proxy.GetProxedInteger: TBAInteger;
begin
  result := ProxedMember as TBAInteger;
end;

{ TBAFloat_Proxy }

function TBAFloat_Proxy.GetProxedFloat: TBAFloat;
begin
  result := ProxedMember as TBAFloat;
end;

{ TBACurrency_Proxy }

function TBACurrency_Proxy.GetProxedCurrency: TBACurrency;
begin
  result := ProxedMember as TBACurrency;
end;

{ TBABlob_Proxy }

function TBABlob_Proxy.GetProxedBlob: TBABlob;
begin
  result := ProxedMember as TBABlob;
end;

function TBABlob_Proxy.SupressEvents: Boolean;
begin
  result := ProxedBlob.SupressEvents;
end;

{ TBAValueSet_Proxy }

function TBAValueSet_Proxy.GetProxedvalueSet: TBAValueSet;
begin
  result := ProxedMember as TBAValueSet;
end;

{ TBABoolean_Proxy }

function TBADate.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldDateContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldDateContent')
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBATime.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldTimeContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldTimeContent')
  else
    result := inherited ProxyInterface(IId, Mode, Obj);
end;

function TBADate.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSDate;
end;

function TBADate.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBADate_Proxy.MakeProxy(self, mode);
end;

function TBATime.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBATime_Proxy.MakeProxy(self, mode);
end;

{ TBATypedBlob_Proxy }

function TBATypedBlob_Proxy.GetContentTypeContent: String;
begin
  Result := ProxedTypedBlob.GetContentTypeContent;
end;

function TBATypedBlob_Proxy.GetProxedTypedBlob: TBATypedBlob;
begin
  result := ProxedMember as TBATypedBlob;
end;

procedure TBATypedBlob_Proxy.SetContentTypeContent(const NewValue: String);
begin
  ProxedTypedBlob.SetContentTypeContent(NewValue);
end;

function TBAMoment_Proxy.GetProxedMoment: TBAMoment;
begin
  result := ProxedMember as TBAMoment;
end;

function TBATime.GetAsSeconds: cardinal;
begin
  Result := Seconds + (Minutes * 60) + (Hours * 3600);
end;

function TBATime.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSTime;
end;

initialization
  with BoldMemberTypes do
  begin
    AddMemberTypeDescriptor(TBoldAttribute, alAbstract);
    AddMemberTypeDescriptor(TBAString, alConcrete);
    AddMemberTypeDescriptor(TBAAnsiString, alConcrete);
    AddMemberTypeDescriptor(TBAUnicodeString, alConcrete);
    AddMemberTypeDescriptor(TBAText, alConcrete);
    AddMemberTypeDescriptor(TBAUnicodeText, alConcrete);
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
  FreeAndNil(TBABoolean._BooleanValues);
  FreeAndNil(_SystemSubscriber);
  if BoldMemberTypesAssigned then
    with BoldMemberTypes do
    begin
      RemoveDescriptorByClass(TBoldAttribute);
      RemoveDescriptorByClass(TBAString);
      RemoveDescriptorByClass(TBAAnsiString);
      RemoveDescriptorByClass(TBAUnicodeString);
      RemoveDescriptorByClass(TBAText);
      RemoveDescriptorByClass(TBAUnicodeText);      
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
