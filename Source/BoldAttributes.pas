
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
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
    procedure SetDataValue(const NewValue: string);
    function SetContent(const NewValue: string): boolean;
    function GetValue: string; virtual;
    procedure SetValue(const Value: string); virtual;
    property Value: string read GetValue write SetValue;
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    procedure AssignCloneValue(AClone: TBoldMember); override;
    procedure FreeContent; override;
    function GetAsAnsiString: TBoldAnsiString; virtual;
    function GetAsUnicodeString: TBoldUnicodeString; virtual;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    function MaySetValue(NewValue: String; Subscriber: TBoldSubscriber): Boolean; virtual;
    function GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
    procedure SetAsAnsiString(const Value: TBoldAnsiString); virtual;
    procedure SetAsUnicodeString(const Value: TBoldUnicodeString); virtual;
    function GetFreeStandingClass: TBoldFreeStandingElementClass; override;
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
    procedure SetContentAsAnsiString(const NewValue: TBoldAnsiString);
    procedure SetContentAsUnicodeString(const NewValue: TBoldUnicodeString);
  protected
    function GetContentAsString: String; override;
    property ProxedString: TBAString read GetProxedString;
  public
    class function MakeProxy(ProxedMember: TBoldMember; Mode:  TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
  end;
  {$ENDIF}

  {---TBAAnsiString---}
  TBAAnsiString = class(TBAString)
  {$IFDEF BOLD_UNICODE}
  private
    fValue: TBoldAnsiString;
    function SetContent(NewValue: TBoldAnsiString): boolean;
    procedure SetDataValue(NewValue: TBoldAnsiString);
    procedure SetValue(const Value: string); override;
  {$ENDIF}
  protected
  {$IFDEF BOLD_UNICODE}
    procedure FreeContent; override;
    function GetValue: string; override;
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
    function SetContent(NewValue: TBoldUnicodeString): boolean;;
    procedure SetDataValue(NewValue: TBoldUnicodeString);
    procedure SetValue(const Value: string); override;
  {$ENDIF}
  protected
  {$IFNDEF BOLD_UNICODE}
    procedure FreeContent; override;
    function GetAsUnicodeString: TBoldUnicodeString; override;
    function GetValue: string; override;
    procedure SetAsUnicodeString(const Value: TBoldUnicodeString); override;
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
  protected
    function GetAsFloat: Double; virtual; abstract;
    procedure SetAsInteger(Value: integer); virtual; abstract;
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
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  private
    FValue: integer;
    procedure SetDataValue(NewValue: Integer);
    function SetContent(NewValue: Integer): boolean;
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
    procedure FreeContent; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    procedure AssignCloneValue(AClone: TBoldMember); override;
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
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  private
    function GetAsSmallInt: SmallInt;
    procedure SetAsSmallInt(const Value: SmallInt);
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
  public
    function CheckRange(Value: integer): Boolean; override;
    property AsSmallInt: SmallInt read GetAsSmallInt write SetAsSmallInt;
  end;

  {---TBAShortInt---}
  TBAShortInt = class(TBASmallInt)
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  private
    function GetAsShortInt: ShortInt;
    procedure SetAsShortInt(const Value: ShortInt);
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
  public
    function CheckRange(Value: integer): Boolean; override;
    property AsShortInt: ShortInt read GetAsShortInt write SetAsShortInt;
  end;

  {---TBAWord---}
  TBAWord = class(TBAInteger)
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  private
    function GetAsWord: Word;
    procedure SetAsWord(const Value: Word);
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
  public
    function CheckRange(Value: integer): Boolean; override;
    property AsWord: Word read GetAsWord write SetAsWord;
  end;

  {---TBAByte---}
  TBAByte = class(TBAWord)
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  private
    function GetAsByte: Byte;
    procedure SetAsByte(const Value: Byte);
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
  public
    function CheckRange(Value: integer): Boolean; override;
    property AsByte: Byte read GetAsByte write SetAsByte;
  end;

  {---TBAFloat---}
  TBAFloat = class(TBANumeric)
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  private
    fValue: Double;
    function SetContent(NewValue: Double): boolean;
    procedure SetDataValue(NewValue: Double);
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
    procedure FreeContent; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    procedure AssignCloneValue(AClone: TBoldMember); override;
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
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  private
    FValue: Currency;
    function SetContent(NewValue: Currency): boolean;
    procedure SetDataValue(NewValue: Currency);
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
    procedure FreeContent; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    procedure AssignCloneValue(AClone: TBoldMember); override;
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
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  private
    fStream: TBoldBlobStream;
    fSupressEventCount: integer;
    function SetContent(NewValue: TBoldAnsiString): boolean;
    procedure SetDataValue(NewValue: TBoldAnsiString);
    function GetBlobSize: Int64;
    function GetAsStream: TBoldBlobStream;
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
    procedure FreeContent; override;
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
    procedure SetAsVariant(const Value: Variant); override;
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
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  private
    FContentType: string;
    procedure SetContentType2(Value: string);
    procedure SetContentTypeContent(NewValue: String);
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
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
    procedure SetAsDate(Value: TDateTime); virtual; abstract;
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsTime(Value: TDateTime);
    function IsSameValue(Value: TDateTime): boolean; virtual; abstract;
    function MaySetValue(NewValue: TDateTime; Subscriber: TBoldSubscriber): Boolean; virtual;
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
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
    function IsSameValue(Value: TDateTime): boolean; override;
    procedure SetAsDate(Value: TDateTime); override;
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
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
    procedure SetAsDate(Value: TDateTime); override;
    function IsSameValue(Value: TDateTime): boolean; override;
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
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  private
    function GetAsSeconds: cardinal;
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
    procedure SetAsDate(Value: TDateTime); override;
    function IsSameValue(Value: TDateTime): boolean; override;
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
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  private
    FValue: TBAValueSetValue;
    procedure CheckIllegalValue;
    function GetAsInteger: Integer;
    procedure SetAsInteger(Value: Integer);
    function SetContent(NewValue: TBAValueSetValue): boolean;
    procedure SetContentAsInteger(NewValue: Integer);
    procedure SetDataValue(NewValue: TBAValueSetValue);
  protected
    procedure FreeContent; override;
    function GetContentAsInteger: Integer; virtual;
    procedure AssignContentValue(const Source: IBoldValue); override;
    procedure AssignCloneValue(AClone: TBoldMember); override;
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
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  private
    class var _BooleanValues: TBAValueSetValueList;
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
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
  strict private
    class var AttributeTypeInfo: TBoldElementTypeInfo;
  private
    class procedure ClearAttributeTypeInfo;
  private
    fConstraint: TBoldConstraintRTInfo;
    fOwningElement: TBoldElement;
    fElementSubscriber: TBoldPassThroughSubscriber;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure CalculateConstraint;
  protected
    function GetAttributeTypeInfoForType: TBoldElementTypeInfo; override;
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
  Math,
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
  TBAString.ClearAttributeTypeInfo;
  TBAAnsiString.ClearAttributeTypeInfo;
  TBAUnicodeString.ClearAttributeTypeInfo;
  TBAText.ClearAttributeTypeInfo;
  TBAUnicodeText.ClearAttributeTypeInfo;
  TBATrimmedString.ClearAttributeTypeInfo;
  TBAInteger.ClearAttributeTypeInfo;
  TBAWord.ClearAttributeTypeInfo;
  TBASmallInt.ClearAttributeTypeInfo;
  TBAByte.ClearAttributeTypeInfo;
  TBAShortInt.ClearAttributeTypeInfo;
  TBAFloat.ClearAttributeTypeInfo;
  TBACurrency.ClearAttributeTypeInfo;
  TBABlob.ClearAttributeTypeInfo;
  TBATypedBlob.ClearAttributeTypeInfo;
  TBADateTime.ClearAttributeTypeInfo;
  TBADate.ClearAttributeTypeInfo;
  TBATime.ClearAttributeTypeInfo;
  TBAValueSet.ClearAttributeTypeInfo;
  TBABoolean.ClearAttributeTypeInfo;
  TBAConstraint.ClearAttributeTypeInfo;
  TBABlobImageJPEG.ClearAttributeTypeInfo;
  TBABlobImageBMP.ClearAttributeTypeInfo;
  FreeAndNil(_SystemSubscriber);
end;

const
  DEFAULTNOW = '<NOW>';
  Meth_GetStringRepresentation = 'GetStringRepresentation';
  Meth_SetStringRepresentation = 'SetStringRepresentation';

type
  TBAUnicodeString_Proxy = class(TBAString_Proxy, IBoldUnicodeStringContent)
  private
    class var fLastUsed: array[TBoldDomainElementProxyMode] of TBoldMember_Proxy;
    class var fLastUsedAsInterface: array[TBoldDomainElementProxyMode] of IBoldValue;
  protected
    class function MakeProxy(ProxedMember: TBoldMember; Mode:  TBoldDomainElementProxyMode): TBoldMember_Proxy; override;
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
    vtString:       Result := TBAString.CreateWithValue(String(value.VString^));
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
var
  bContentIsNull: Boolean;
  sOldValue: String;
begin
  if IsNull or (FValue <> NewValue) then
  begin
    BoldClearLastFailure;
    if not CanSetValue(NewValue, nil) then
      BoldRaiseLastFailure(self, sSetDataValue, '');
    if not StartModify then
      BoldRaiseLastFailure(self, sSetDataValue, '');
    bContentIsNull := ContentIsNull;
    sOldValue := fValue;
    try
      SetContent(NewValue);
      EndModify;
      if bContentIsNull then
        Changed(beValueChanged, [NewValue])
      else
        Changed(beValueChanged, [NewValue, sOldValue]);
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBAString.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);

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

function TBAString.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
begin
  if Assigned(BoldAttributeRtInfo) then
  begin
    Result := (BoldAttributeRtInfo.Length = -1) or
      (Length(Value) <= BoldAttributeRtInfo.Length);
    if not result then
      SetBoldLastFailureReason(TBoldFailureReason.CreateFmt(sStringTooLong, [BoldAttributeRtInfo.Length, Length(Value)] , self));
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
  begin
    if (BoldElement is TBoldAttribute) and EitherIsNull(Self, TBoldAttribute(BoldElement)) then
      Result := NullSmallest(BoldElement)
    else
      Result := StringCompare(CompType, AsString, BoldElement.AsString);
  end
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
   result := result and SendQuery(bqMaySetValue, [NewValue], Subscriber)
{$ENDIF}
end;

class procedure TBAString.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
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
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignValue]);
end;

function TBAString.SetContent(const NewValue: string): boolean;
begin
  result := false;
  if (BoldPersistenceState = bvpsInvalid) or
     ContentIsNull or (fValue <> NewValue) then
  begin
    PreChange;
    fValue := NewValue;
    SetToNonNull;
    result := true;
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
var
  bContentIsNull: Boolean;
  sOldValue: string;
begin
  bContentIsNull := ProxedString.ContentIsNull;
  sOldValue := ProxedString.Value;
  if ProxedString.SetContent(NewValue) then
    if bContentIsNull then
      ProxedString.Changed(beValueChanged, [NewValue])
    else
      ProxedString.Changed(beValueChanged, [NewValue, sOldValue]);
end;

procedure TBAString_Proxy.SetContentAsAnsiString(const NewValue:
    TBoldAnsiString);
var
  bContentIsNull: Boolean;
  sOldValue: string;
  bHasChanged: Boolean;
begin
  bContentIsNull := ProxedString.ContentIsNull;
  sOldValue := ProxedString.Value;
  if ProxedString is TBAAnsiString then
    bHasChanged := TBAAnsiString(ProxedString).SetContent(NewValue)
  else
    bHasChanged := ProxedString.SetContent(string(NewValue));
  if bHasChanged then
    if bContentIsNull then
      ProxedString.Changed(beValueChanged, [NewValue])
    else
      ProxedString.Changed(beValueChanged, [NewValue, sOldValue]);
end;

procedure TBAString_Proxy.SetContentAsUnicodeString(const NewValue:
    TBoldUnicodeString);
var
  bContentIsNull: Boolean;
  sOldValue: String;
  bHasChanged: Boolean;
begin
  bContentIsNull := ProxedString.ContentIsNull;
  sOldValue := ProxedString.Value;
  if ProxedString is TBAUnicodeString then begin
    bHasChanged := TBAUnicodeString(ProxedString).SetContent(NewValue);
  end else begin
    bHasChanged := ProxedString.SetContent(string(NewValue));
  end;
  if bHasChanged then
    if bContentIsNull then
      ProxedString.Changed(beValueChanged, [NewValue])
    else
      ProxedString.Changed(beValueChanged, [NewValue, sOldValue]);
end;

procedure TBAString.AssignCloneValue(AClone: TBoldMember);
begin
  EnsureContentsCurrent;
  if ContentIsNull then
    (AClone as TBAString).SetContentToNull
  else
    (AClone as TBAString).SetToNonNull;
  (AClone as TBAString).fValue := FValue;
end;

procedure TBAString.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldStringContent;
  sr: IBoldStringRepresentable;
  NullableValue: IBoldNullableValue;
begin
  if not assigned(source) or ((source.QueryInterface(IBoldNullableValue, NullableValue) = S_OK) and NullableValue.IsNull) then
  begin
    if CanSetToNull(nil) then
      SetContentToNull
    else
      SetContent('')
  end
  else if source.QueryInterface(IBoldStringContent, S) = S_OK then
    SetContent(s.AsString)
  else if source.QueryInterface(IBoldStringRepresentable, sr) = S_OK then
    SetContent(sr.AsString)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
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

function TBAAnsiString.SetContent(NewValue: TBoldAnsiString): boolean;
begin
  result := false;
  if (BoldPersistenceState = bvpsInvalid) or
     ContentIsNull or (fValue <> NewValue) then
  begin
    PreChange;
    fValue := NewValue;
    SetToNonNull;
    result := true;
  end;
end;

procedure TBAAnsiString.SetDataValue(NewValue: TBoldAnsiString);
var
  bContentIsNull: Boolean;
  sOldValue: TBoldAnsiString;
begin
  if IsNull or (fValue <> NewValue) then
  begin
    BoldClearLastFailure;
    if not CanSetValue(string(NewValue), nil) then
      BoldRaiseLastFailure(self, sSetDataValue, ''); // do not localize
    if not StartModify then
      BoldRaiseLastFailure(self, sSetDataValue, ''); // do not localize
    bContentIsNull := ContentIsNull;
    sOldValue := fValue;
    try
      SetContent(NewValue);
      EndModify;
      if bContentIsNull then
        Changed(beValueChanged, [NewValue])
      else
        Changed(beValueChanged, [NewValue, sOldValue]);
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

function TBAUnicodeString.SetContent(NewValue: TBoldUnicodeString): boolean;
begin
  result := false;
  if (BoldPersistenceState = bvpsInvalid) or
     ContentIsNull or (fValue <> NewValue) then
  begin
    PreChange;
    fValue := NewValue;
    SetToNonNull;
    result := true;
  end;
end;

procedure TBAUnicodeString.SetDataValue(NewValue: TBoldUnicodeString);
var
  bContentIsNull: Boolean;
  sOldValue: TBoldUnicodeString;
begin
  if IsNull or (fValue <> NewValue) then
  begin
    BoldClearLastFailure;
    if not CanSetValue(string(NewValue), nil) then
      BoldRaiseLastFailure(self, sSetDataValue, ''); // do not localize
    if not StartModify then
      BoldRaiseLastFailure(self, sSetDataValue, ''); // do not localize
    bContentIsNull := ContentIsNull;
    sOldValue := fValue;
    try
      SetContent(NewValue);
      EndModify;
      if bContentIsNull then
        Changed(beValueChanged, [NewValue])
      else
        Changed(beValueChanged, [NewValue, sOldValue]);
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
var
  bContentIsNull: Boolean;
  sOldValue: Integer;
begin
  if IsNull or (FValue <> NewValue) then
  begin
    BoldClearLastFailure;
    if not CanSetValue(NewValue, nil) then
      BoldRaiseLastFailure(self, sSetDataValue, '');
    if not CheckRange(NewValue) then
      raise EBoldInternal.CreateFmt('%s: %s', [DisplayName, GetBoldLastFailureReason.Reason]);
    if not StartModify then
      BoldRaiseLastFailure(self, sSetDataValue, '');
    bContentIsNull := ContentIsNull;
    sOldValue := fValue;
    try
      SetContent(NewValue);
      EndModify;
      if bContentIsNull then
        Changed(beValueChanged, [NewValue])
      else
        Changed(beValueChanged, [NewValue, sOldValue]);
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
   result := result and SendQuery(bqMaySetValue, [Value], Subscriber)
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
      TBoldFailureReason.createFmt(sRangeError, [Min, MAX, Value], self));
  end
  else
    result := true;
end;

class procedure TBAInteger.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
end;

{---TBAByte---}
function TBAByte.CheckRange(Value: integer): Boolean;
begin
  result := CheckRangeWithBounds(Value, Low(Byte), High(Byte));
end;

class procedure TBAByte.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
end;

function TBAByte.GetAsByte: Byte;
begin
  result := AsInteger;
end;

function TBAByte.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
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


class procedure TBAShortInt.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
end;

function TBAShortInt.GetAsShortInt: ShortInt;
begin
  result := AsInteger;
end;

function TBAShortInt.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
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

class procedure TBASmallInt.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
end;

function TBASmallInt.GetAsSmallInt: SmallInt;
begin
  result := AsInteger;
end;

function TBASmallInt.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
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

class procedure TBAWord.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
end;

function TBAWord.GetAsWord: Word;
begin
  result := AsInteger;
end;

function TBAWord.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
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
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignValue]);
end;

function TBAInteger.SetContent(NewValue: Integer): boolean;
begin
  result := false;
  if (BoldPersistenceState = bvpsInvalid) or
    ContentIsNull or (FValue <> NewValue) then
  begin
    PreChange;
    FValue := NewValue;
    SetToNonNull;
    result := true;
  end;
end;

procedure TBAInteger_Proxy.SetContentAsInteger(NewValue: Integer);
var
  bContentIsNull: Boolean;
  sOldValue: Integer;
begin
  bContentIsNull := ProxedInteger.ContentIsNull;
  sOldValue := ProxedInteger.fValue;
  if ProxedInteger.SetContent(NewValue) then
    if bContentIsNull then
      ProxedInteger.Changed(beValueChanged, [NewValue])
    else
      ProxedInteger.Changed(beValueChanged, [NewValue, sOldValue]);
end;

function TBAInteger_Proxy.GetContentAsInteger: Integer;
begin
  result := ProxedInteger.fValue;
end;

procedure TBAInteger.AssignCloneValue(AClone: TBoldMember);
begin
  EnsureContentsCurrent;
  if ContentIsNull  then
    (AClone as TBAInteger).SetContentToNull
  else
    (AClone as TBAInteger).SetToNonNull;
  (AClone as TBAInteger).fValue := FValue;
end;

procedure TBAInteger.AssignContentValue(const Source: IBoldValue);
var
  i: IBoldIntegerContent;
  s: IBoldStringContent;
  NullableValue: IBoldNullableValue;
begin
  if not assigned(source) or ((source.QueryInterface(IBoldNullableValue, NullableValue) = S_OK) and NullableValue.IsNull) then
  begin
    if CanSetToNull(nil) then
      SetContentToNull
    else
      SetContent(0);
  end
  else if source.QueryInterface(IBoldIntegerContent, i) = S_OK then
    SetContent(i.AsInteger)
  else if source.QueryInterface(IBoldStringContent, s) = S_OK then
    SetContent(s.asString.ToInteger)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
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
var
  bContentIsNull: Boolean;
  sOldValue: Double;
begin
  if IsNull or not (SameValue(FValue, NewValue)) then
  begin
    BoldClearLastFailure;
    if not CanSetValue(NewValue, nil) then
      BoldRaiseLastFailure(self, sSetDataValue, '');
    if not StartModify then
      BoldRaiseLastFailure(self, sSetDataValue, '');
    bContentIsNull := ContentIsNull;
    sOldValue := fValue;
    try
      SetContent(NewValue);
      EndModify;
      if bContentIsNull then
        Changed(beValueChanged, [NewValue])
      else
        Changed(beValueChanged, [NewValue, sOldValue]);
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
      result := SameValue(Self.asFloat, vFloat.asFloat);
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
          if SameValue(AsFloat, CompareValue) then
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

class procedure TBAFloat.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
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
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignValue]);
end;

function TBAFloat.SetContent(NewValue: Double): boolean;
begin
  result := false;
  if (BoldPersistenceState = bvpsInvalid) or
    ContentIsNull or not (SameValue(FValue, NewValue)) then
  begin
    PreChange;
    FValue := NewValue;
    SetToNonNull;
    result := true;
  end;
end;

function TBAFloat_Proxy.GetContentAsFloat: Double;
begin
  result := ProxedFloat.fValue;
end;

procedure TBAFloat_Proxy.SetContentAsFloat(NewValue: Double);
var
  bContentIsNull: Boolean;
  sOldValue: Double;
begin
  bContentIsNull := ProxedFloat.ContentIsNull;
  sOldValue := ProxedFloat.fValue;
  if ProxedFloat.SetContent(NewValue) then
    if bContentIsNull then
      ProxedFloat.Changed(beValueChanged, [NewValue])
    else
      ProxedFloat.Changed(beValueChanged, [NewValue, sOldValue]);
end;

procedure TBAFloat.AssignCloneValue(AClone: TBoldMember);
begin
  EnsureContentsCurrent;
  if ContentIsNull  then
    (AClone as TBAFloat).SetContentToNull
  else
    (AClone as TBAFloat).SetToNonNull;
  (AClone as TBAFloat).fValue := FValue;
end;

procedure TBAFloat.AssignContentValue(const Source: IBoldValue);
var
  FloatContent: IBoldFloatContent;
  CurrencyContent: IBoldCurrencyContent;
  IntegerContent: IBoldIntegerContent;
  StringContent: IBoldStringContent;
  NullableValue: IBoldNullableValue;
begin
  if not assigned(source) or ((source.QueryInterface(IBoldNullableValue, NullableValue) = S_OK) and NullableValue.IsNull) then
  begin
    if CanSetToNull(nil) then
      SetContentToNull
    else
      SetContent(0);
  end
  else if source.QueryInterface(IBoldFloatContent, FloatContent) = S_OK  then
    SetContent(FloatContent.AsFloat)
  else if source.QueryInterface(IBoldCurrencyContent, CurrencyContent) = S_OK then
    SetContent(CurrencyContent.asCurrency)
  else if source.QueryInterface(IBoldIntegerContent, IntegerContent) = S_OK then
    SetContent(IntegerContent.asInteger)
  else if source.QueryInterface(IBoldStringContent, StringContent) = S_OK then
    SetContent(StrToFloat(StringContent.asString))
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

function TBAFloat.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldFloatContent) then
    Result := RetrieveProxyInterface(IBoldFloatContent, Mode, obj, 'IBoldFloatContent') // do not localize
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;

function TBAFloat.GetProxy(Mode: TBoldDomainElementProxyMode): TBoldMember_Proxy;
begin
  result := TBAFloat_Proxy.MakeProxy(self, mode);
end;

{ TBACurrency }

procedure TBACurrency.SetDataValue(NewValue: Currency);
var
  bContentIsNull: Boolean;
  sOldValue: Currency;
begin
  if IsNull or not (SameValue(FValue, NewValue)) then
  begin
    BoldClearLastFailure;
    if not CanSetValue(NewValue, nil) then
      BoldRaiseLastFailure(self, sSetDataValue, '');
    if not StartModify then
      BoldRaiseLastFailure(self, sSetDataValue, ''); // do not localize
    bContentIsNull := ContentIsNull;
    sOldValue := fValue;
    try
      SetContent(NewValue);
      EndModify;
      if bContentIsNull then
        Changed(beValueChanged, [NewValue])
      else
        Changed(beValueChanged, [NewValue, sOldValue]);
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
    BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);
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
    BoldRaiseLastFailure(self, Meth_GetStringRepresentation, '');
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
      result := (SameValue(Self.asCurrency, vCurrency.asCurrency));
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
      FormatFailure(value, 'currency'); // do not localize
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
        ctDefault:
          if SameValue(AsCurrency, CompareValue) then
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
   result := result and SendQuery(bqMaySetValue, [NewValue], Subscriber)
{$ENDIF}
end;

class procedure TBACurrency.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
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
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignValue]);
end;

function TBACurrency.SetContent(NewValue: Currency): boolean;
begin
  result := false;
  if (BoldPersistenceState = bvpsInvalid) or
    ContentIsNull or not (SameValue(fValue, NewValue)) then
  begin
    PreChange;
    FValue := NewValue;
    SetToNonNull;
    result := true;
  end;
end;

procedure TBACurrency_Proxy.SetContentAsCurrency(NewValue: Currency);
var
  bContentIsNull: Boolean;
  sOldValue: Currency;
begin
  bContentIsNull := ProxedCurrency.ContentIsNull;
  sOldValue := ProxedCurrency.FValue;
  if ProxedCurrency.SetContent(NewValue) then
    if bContentIsNull then
      ProxedCurrency.Changed(beValueChanged, [NewValue])
    else
      ProxedCurrency.Changed(beValueChanged, [NewValue, sOldValue]);
end;

function TBACurrency_Proxy.GetContentAsCurrency: Currency;
begin
  result := ProxedCurrency.fValue;
end;

procedure TBACurrency.AssignCloneValue(AClone: TBoldMember);
begin
  EnsureContentsCurrent;
  if ContentIsNull  then
    (AClone as TBACurrency).SetContentToNull
  else
    (AClone as TBACurrency).SetToNonNull;
  (AClone as TBACurrency).fValue := FValue;
end;

procedure TBACurrency.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldCurrencyContent;
  FloatContent: IBoldFloatContent;
  IntegerContent: IBoldIntegerContent;
  NullableValue: IBoldNullableValue;
begin
  if not assigned(source) or ((source.QueryInterface(IBoldNullableValue, NullableValue) = S_OK) and NullableValue.IsNull) then
  begin
    if CanSetToNull(nil) then
      SetContentToNull
    else
      SetContent(0);
  end
  else if (source.QueryInterface(IBoldCurrencyContent, S) = S_OK) then
    SetContent(s.AsCurrency)
  else if (source.QueryInterface(IBoldFloatContent, FloatContent) = S_OK) then
    SetContent(FloatContent.AsFloat)
  else if (source.QueryInterface(IBoldIntegerContent, IntegerContent) = S_OK) then
    SetContent(IntegerContent.AsInteger)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

function TBACurrency.ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldCurrencyContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldCurrencyContent') // do not localize
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
  SetSize(Int64(0));
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
var
  bContentIsNull: Boolean;
  sOldValue: AnsiString;
begin
  if IsNull or not AsStream.IsDataSame(PAnsiChar(NewValue), Length(NewValue)) then
  begin
    BoldClearLastFailure;
    if not CanSetValue(NewValue, nil) then
      BoldRaiseLastFailure(self, sSetDataValue, '');
    if not StartModify then
      BoldRaiseLastFailure(self, sSetDataValue, '');
    bContentIsNull := ContentIsNull;
    sOldValue := self.GetAsBlob;
    try
      SetContent(NewValue);
      EndModify;
      if bContentIsNull then
        Changed(beValueChanged, [NewValue])
      else
        Changed(beValueChanged, [NewValue, sOldValue]);
    except
      FailModify;
      raise;
    end;
  end;
end;

procedure TBABlob.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);
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
    BoldRaiseLastFailure(self, Meth_GetStringRepresentation, '');
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

function TBABlob.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
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
      AsStream.Write(p ^, VarArrayHighBound(Value, 1)+1);
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
  // We can not call SetToNull since that will cause recursive Modification. that must be done by InternalSetSize
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
   result := result and SendQuery(bqMaySetValue, [NewValue], Subscriber)
{$ENDIF}
end;

class procedure TBABlob.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
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
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignValue]);
end;

function TBABlob.SetContent(NewValue: TBoldAnsiString): boolean;
begin
  result := false;
  if ContentIsNull or not AsStream.IsDataSame(PAnsiChar(NewValue), Length(NewValue)) then
  begin
    PreChange;
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
    result := true;
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
var
  bContentIsNull: Boolean;
  sOldValue: TBoldAnsiString;
begin
  bContentIsNull := ProxedBlob.ContentIsNull;
  sOldValue := ProxedBlob.GetAsBlob;
  if ProxedBlob.SetContent(NewValue) then
    if bContentIsNull then
      ProxedBlob.Changed(beValueChanged, [NewValue])
    else
      ProxedBlob.Changed(beValueChanged, [NewValue, sOldValue]);
end;

procedure TBABlob.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldBlobContent;
  NullableValue: IBoldNullableValue;
begin
  if not assigned(source) or ((source.QueryInterface(IBoldNullableValue, NullableValue) = S_OK) and NullableValue.IsNull) then
  begin
    if CanSetToNull(nil) then
      SetContentToNull
    else
      SetContent('');
  end
  else if source.QueryInterface(IBoldBlobContent, S) = S_OK then
    SetContent(s.AsBlob)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
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
  BLobLength: integer;
begin
  if BoldElement is TBABlob then
  begin
    CompareBlob := BoldElement as TBABlob;
    if EitherIsNull(self, CompareBlob) then
      result := CompareBlob.IsNull and IsNull
    else
    begin
      BLobLength := Length(CompareBlob.AsStream.fData);
      result := (Length(AsStream.fData) = BLobLength) and CompareMem(@CompareBlob.AsStream.fData[0], @AsStream.fData[0], BLobLength);
    end;
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
  begin
    BeginSupressEvents;
    try
    fStream.Clear;
    finally
      EndSupressEvents;
    end;
  end;
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
var
  bContentIsNull: Boolean;
  sOldValue: AnsiString;
begin
  BoldClearLastFailure;
  if not CanSetContentType(Value, nil) then
    BoldRaiseLastFailure(self, 'SetContentType', '');

  if ContentIsNull or (FContentType <> Value) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetContentType', '');
    try
      bContentIsNull := ContentIsNull;
      sOldValue := GetAsBlob;
      SetContentTypeContent(Value);
      EndModify;
      if bContentIsNull then
        Changed(beValueChanged, [Value])
      else
        Changed(beValueChanged, [Value, sOldValue]);
    except
      FailModify;
      raise;
    end;
  end
end;

function TBATypedBlob.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
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
        BoldRaiseLastFailure(self, Meth_GetStringRepresentation, '');
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
{$IFDEF BOLD_NO_QUERIES}
   result := True;
{$ELSE}
  Result := SendQuery(bqMaySetContentType, [Value], Subscriber);
{$ENDIF}
end;

class procedure TBATypedBlob.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
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
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignValue]);
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
end;

{ TBAMoment }

procedure TBAMoment.SetDataValue(NewValue: TDateTime);
var
  bContentIsNull: Boolean;
  sOldValue: TDateTime;
begin
  if IsNull or not IsSameValue(NewValue) then
  begin
    BoldClearLastFailure;
  {$IFDEF NoNegativeDates}
    if IsPartOfSystem then
    begin
    if (NewValue < 0) then
      BoldRaiseLastFailure(self, sSetDataValue, 'Attempt to set date before 1899-12-30');
    if (NewValue >= 1000*365) then
      BoldRaiseLastFailure(self, sSetDataValue, 'Attempt to set date after 2899-12-30');
    end;
  {$ENDIF}
    if not CanSetValue(NewValue, nil) then
      BoldRaiseLastFailure(self, sSetDataValue, '');
    if not StartModify then
      BoldRaiseLastFailure(self, sSetDataValue, '');
    bContentIsNull := ContentIsNull;
    sOldValue := fValue;
    try
      SetDateTimeContent(NewValue);
      EndModify;
      if bContentIsNull then
        Changed(beValueChanged, [NewValue])
      else
        Changed(beValueChanged, [NewValue, sOldValue]);
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
   result := result and SendQuery(bqMaySetValue, [NewValue], Subscriber)
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
  Result := Int(fValue);
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
    if bContentIsNull then
      Changed(beValueChanged, [NewValue])
    else
      Changed(beValueChanged, [NewValue, sOldValue]);
  end;
end;

function TBAMoment_Proxy.GetContentAsDate: TDateTime;
begin
  result := Int(ProxedMoment.fValue);
end;

procedure TBAMoment_Proxy.SetContentAsDate(NewValue: TDateTime);
var
  bContentIsNull: Boolean;
  sOldValue: TDateTime;
begin
  bContentIsNull := ProxedMoment.ContentIsNull;
  sOldValue := ProxedMoment.FValue;
  ProxedMoment.SetDateTimeContent(Int(NewValue) + GetContentAsTime);
  if bContentIsNull then
    ProxedMoment.Changed(beValueChanged, [NewValue])
  else
    ProxedMoment.Changed(beValueChanged, [NewValue, sOldValue]);
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
var
  bContentIsNull: Boolean;
  sOldValue: TDateTime;
begin
  bContentIsNull := ProxedMoment.ContentIsNull;
  sOldValue := ProxedMoment.FValue;
  ProxedMoment.SetDateTimeContent(NewValue);
  if bContentIsNull then
    ProxedMoment.Changed(beValueChanged, [NewValue])
  else
    ProxedMoment.Changed(beValueChanged, [NewValue, sOldValue]);
end;

procedure TBAMoment_Proxy.SetContentAsTime(NewValue: TDateTime);
var
  bContentIsNull: Boolean;
  sOldValue: TDateTime;
begin
  bContentIsNull := ProxedMoment.ContentIsNull;
  sOldValue := ProxedMoment.FValue;
  ProxedMoment.SetDateTimeContent(frac(NewValue) + GetContentAsDate);
  if bContentIsNull then
    ProxedMoment.Changed(beValueChanged, [NewValue])
  else
    ProxedMoment.Changed(beValueChanged, [NewValue, sOldValue]);
end;

procedure TBAMoment.SetEmptyValue;
begin
  if Assigned(BoldAttributeRTInfo) and not BoldAttributeRTInfo.AllowNull and (fValue <> 0) then
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
    BoldRaiseLastFailure(self, Meth_SetStringRepresentation, sStringValidationFailed);
  if Representation = brDefault then
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
  Result := CharInSet(C, ['0'..'9', ' ', {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}TimeSeparator, {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DateSeparator]);
end;

function TBADateTime.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
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
procedure TBADate.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
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
  Result := CharInSet(C, ['0'..'9', {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DateSeparator]);
end;

function TBADate.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
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
procedure TBATime.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
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
  Result := CharInSet(C, ['0'..'9', {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}TimeSeparator]);
end;

function TBATime.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
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

procedure TBAValueSetValue.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  if not ValidateString(Value, Representation) then
    BoldRaiseLastFailure(nil, 'SetStringRepresentation', sStringValidationFailed);
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

procedure TBAValueSetValue.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
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
    if TBAValueSetValue(FValueList.Items[I]).AsInteger = Value then
    begin
      Result := FValueList.Items[I];
      exit;
    end;
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
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignValue]);
end;

class procedure TBADateTime.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
end;

constructor TBADateTime.CreateWithValue(Value: TDateTime);
begin
  inherited Create;
  asDateTime := Value;
end;

procedure TBADateTime.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldDateTimeContent;
  NullableValue: IBoldNullableValue;
begin
  if not assigned(source) or ((source.QueryInterface(IBoldNullableValue, NullableValue) = S_OK) and NullableValue.IsNull) then
  begin
    if CanSetToNull(nil) then
      SetContentToNull
    else
      SetDateTimeContent(0)
  end
  else if source.QueryInterface(IBoldDateTimeContent, S) = S_OK then
    SetDateTimeContent(s.AsDateTime)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
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

procedure TBADateTime.SetAsDate(Value: TDateTime);
begin
  SetDataValue(Value);
end;

function TBADateTime.IsSameValue(Value: TDateTime): boolean;
begin
  result := SameValue(Value, fValue);
end;

function TBADateTime.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
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
var
  bContentIsNull: Boolean;
  sOldValue: TBAValueSetValue;
begin
  if IsNull or not (FValue.IsEqual(NewValue)) then
  begin
    BoldClearLastFailure;
    if not CanSetValue(NewValue, nil) then
      BoldRaiseLastFailure(self, sSetDataValue, '');
    if not Assigned(NewValue) then
      raise EBold.CreateFmt(sValueNotSet, [ClassName]);
    if not StartModify then
      BoldRaiseLastFailure(self, sSetDataValue, '');
    bContentIsNull := ContentIsNull;
    sOldValue := fValue;
    try
      SetContent(NewValue);
      EndModify;
      if bContentIsNull then
        Changed(beValueChanged, [NewValue])
      else
        Changed(beValueChanged, [NewValue, sOldValue]);
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
    raise EBold.CreateFmt(sValuesNotInitialized, [ClassName]);
  if assigned(values.FValueList) and (values.fValueList.count <> 0) then
    FValue := Values.GetFirstValue
  else
    setToNull;
end;

procedure TBAValueSet.AssignValue(const Source: IBoldValue);
var
  i: IBoldIntegerContent;
  s: IBoldStringContent;
  value: TBAValueSetValue;
begin
  if source.QueryInterface(IBoldIntegerContent, i) = S_OK then
  begin
    Value := Values.FindByInteger(i.AsInteger);
    if Assigned(Value) then
      SetDataValue(Value)
    else
      raise EBold.CreateFmt(sUnknownIntRepr, [classname, sAssignValue, i.asString]);
  end
  else if source.QueryInterface(IBoldStringContent, s) = S_OK then
  begin
    Value := Values.FindByString(brDefault, s.asString);
    if Assigned(Value) then
      SetDataValue(Value)
    else
      raise EBold.CreateFmt(sUnknownStringRepr, [classname, sAssignValue, s.asString]);
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignValue]);
end;

function TBAValueSet.SetContent(NewValue: TBAValueSetValue): boolean;
begin
  result := false;
  if (BoldPersistenceState = bvpsInvalid) or
     ContentIsNull or not (fValue.IsEqual(NewValue)) then
  begin
    PreChange;
    fValue := NewValue;
    SetToNonNull;
    result := true;
  end;
end;

function TBAValueSet_Proxy.GetContentAsString: string;
begin
  result := ProxedValueSet.fValue.StringRepresentation[brDefault];
end;

procedure TBAValueSet_Proxy.SetContentAsString(const NewValue: String);
var
  bContentIsNull: Boolean;
  sOldValue: String;
  ValueSetValue: TBAValueSetValue;
begin
  ValueSetValue := ProxedValueSet.Values.FindByString(brDefault, NewValue);
  if assigned(ValueSetValue) then
  begin
    bContentIsNull := ProxedValueSet.ContentIsNull;
    sOldValue := ProxedValueSet.FValue.AsString;
    if ProxedValueSet.SetContent(ValueSetValue) then
      if bContentIsNull then
        ProxedValueSet.Changed(beValueChanged, [NewValue])
      else
        ProxedValueSet.Changed(beValueChanged, [NewValue, sOldValue]);
  end;
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
var
  bContentIsNull: Boolean;
  sOldValue: Integer;
begin
  bContentIsNull := ProxedValueSet.ContentIsNull;
  sOldValue := ProxedValueSet.FValue.FAsInteger;
  ProxedValueSet.ContentAsInteger:= NewValue;
  if bContentIsNull then
    ProxedValueSet.Changed(beValueChanged, [NewValue])
  else
    ProxedValueSet.Changed(beValueChanged, [NewValue, sOldValue]);
end;

procedure TBAValueSet.AssignCloneValue(AClone: TBoldMember);
begin
  EnsureContentsCurrent;
  if ContentIsNull  then
    (AClone as TBAValueSet).SetContentToNull
  else
    (AClone as TBAValueSet).SetToNonNull;
  (AClone as TBAValueSet).fValue := FValue;
end;

procedure TBAValueSet.AssignContentValue(const Source: IBoldValue);
var
  i: IBoldIntegerContent;
  s: IBoldStringContent;
  value: TBAValueSetValue;
  NullableValue: IBoldNullableValue;
begin
  if not assigned(source) or ((source.QueryInterface(IBoldNullableValue, NullableValue) = S_OK) and NullableValue.IsNull) then
  begin
    if CanSetToNull(nil) then
      SetContentToNull
    else
      SetContent(Values.GetFirstValue)
  end
  else if source.QueryInterface(IBoldIntegerContent, i) = S_OK then
  begin
    value := Values.FindByInteger(i.AsInteger);
    if Assigned(Value) then
      SetContent(Value)
    else
      raise EBold.CreateFmt(sUnknownIntRepr, [classname, sAssignContentValue, i.asString]);
  end
  else if source.QueryInterface(IBoldStringContent, S) = S_OK then
  begin
    Value := Values.FindByString(brDefault, s.asString);
    if Assigned(Value) then
      SetContent(Value)
    else
      raise EBold.CreateFmt(sUnknownStringRepr, [classname, sAssignContentValue, s.asString]);
  end
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
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

class procedure TBAValueSet.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
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
class procedure TBABoolean.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
end;

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

function TBAInteger.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
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

function TBAFloat.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
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

function TBACurrency.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
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

function TBABoolean.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
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
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignValue]);
end;

class procedure TBADate.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
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
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignValue]);
end;

class procedure TBATime.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
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
  NullableValue: IBoldNullableValue;
begin
  if not assigned(source) or ((source.QueryInterface(IBoldNullableValue, NullableValue) = S_OK) and NullableValue.IsNull) then
  begin
    if CanSetToNull(nil) then
      SetContentToNull
    else
    SetDateTimeContent(0)
  end
  else if source.QueryInterface(IBoldDateContent, S) = S_OK then
    SetDateTimeContent(s.AsDate)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

procedure TBATime.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldTimeContent;
  NullableValue: IBoldNullableValue;
begin
  if not assigned(source) or ((source.QueryInterface(IBoldNullableValue, NullableValue) = S_OK) and NullableValue.IsNull) then
  begin
    if CanSetToNull(nil) then
      SetContentToNull
    else
    SetDateTimeContent(0)
  end
  else if source.QueryInterface(IBoldTimeContent, S) = S_OK then
    SetDateTimeContent(s.AsTime)
  else
    raise EBold.CreateFmt(sUnknownTypeOfSource, [classname, sAssignContentValue]);
end;

procedure TBABoolean.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldBooleanContent;
  NullableValue: IBoldNullableValue;
begin
  if not assigned(source) or ((source.QueryInterface(IBoldNullableValue, NullableValue) = S_OK) and NullableValue.IsNull) then
  begin
    if CanSetToNull(nil) then
      SetContentToNull
    else
      SetContentAsInteger(Integer(false))
  end
  else if source.QueryInterface(IBoldBooleanContent, S) = S_OK then
    SetContentAsInteger(Integer(s.AsBoolean))
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

procedure TBAConstraint.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
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

class procedure TBAConstraint.ClearAttributeTypeInfo;
begin
  AttributeTypeInfo := nil;
end;

destructor TBAConstraint.destroy;
begin
  FreeAndNil(fElementSubscriber);
  inherited;
end;

function TBAConstraint.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
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
        raise EBold.CreateFmt(sInvalidConstraint, [Constraint.ConstraintExpression, BOLDCRLF, e.message]);
    end;

    if not assigned(ResType) then
      raise EBold.CreateFmt(sUnknownConstraintType, [Constraint.ConstraintExpression, OwningElement.BoldType.AsString]);

    if not ResType.ConformsTo((BoldType.SystemTypeInfo as TboldSystemTypeInfo).AttributeTypeInfoByDelphiName[TBABoolean.ClassName]) then
      raise EBold.CreateFmt(sConstraintNotBoolean, [Constraint.ConstraintExpression, OwningElement.BoldType.AsString]);

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

procedure TBADate.SetAsDate(Value: TDateTime);
begin
  SetDataValue(Trunc(Value));
end;

function TBADate.IsSameValue(Value: TDateTime): boolean;
begin
  result := Trunc(Value) = fValue;
end;

function TBADate.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
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
var
  bContentIsNull: Boolean;
  sOldValue: AnsiString;
begin
  bContentIsNull := ProxedTypedBlob.ContentIsNull;
  sOldValue := ProxedTypedBlob.GetAsBlob;
  ProxedTypedBlob.SetContentTypeContent(NewValue);
  if bContentIsNull then
    ProxedTypedBlob.Changed(beValueChanged, [NewValue])
  else
    ProxedTypedBlob.Changed(beValueChanged, [NewValue, sOldValue]);
end;

function TBAMoment_Proxy.GetProxedMoment: TBAMoment;
begin
  result := ProxedMember as TBAMoment;
end;

function TBATime.GetAsSeconds: cardinal;
begin
  Result := Seconds + (Minutes * 60) + (Hours * 3600);
end;

procedure TBATime.SetAsDate(Value: TDateTime);
begin
  SetDataValue(Frac(Value));
end;

function TBATime.IsSameValue(Value: TDateTime): boolean;
begin
  result := SameValue(Frac(Value), fValue);
end;

function TBATime.GetAttributeTypeInfoForType: TBoldElementTypeInfo;
begin
  if not Assigned(AttributeTypeInfo) then
  begin
    AttributeTypeInfo := inherited GetAttributeTypeInfoForType;
    SubscribeToSystem;
  end;
  result := AttributeTypeInfo;
end;

function TBATime.GetFreeStandingClass: TBoldFreeStandingElementClass;
begin
  result := TBFSTime;
end;

{ TBAUnicodeString_Proxy }

class function TBAUnicodeString_Proxy.MakeProxy(ProxedMember: TBoldMember;
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

