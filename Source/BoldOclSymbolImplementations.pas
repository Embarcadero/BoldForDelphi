{ Global compiler directives }
{$include bold.inc}
unit BoldOclSymbolImplementations;

interface

uses
  BoldOclClasses;

procedure InitializeSymbolTable(SymTab: TBoldSymbolDictionary);
procedure RegisterOCLOperation(OperationClass: TBoldOclSymbolClass);

implementation

uses
  SysUtils,
  DateUtils,
  Classes,
  Variants,
  Math,
  System.RegularExpressions,
  System.RegularExpressionsCore,

  BoldCoreConsts,
  BoldElements,
  BoldAttributes,
  BoldOclError,
  BoldDefs,
  BoldMath,
  BoldTypeList,
  BoldSystemRT,
  BoldSystem,
  BoldSubscription,
  BoldValueSpaceInterfaces,
  BoldIsoDateTime;

var
  G_OCLOperations: TList = nil;

function OCLOperations: TList;
begin
  if not assigned(G_OCLOperations) then
    G_OClOperations := TList.Create;
  result := G_OCLOperations;
end;

procedure RegisterOCLOperation(OperationClass: TBoldOclSymbolClass);
begin
  if OCLOPerations.IndexOf(OperationClass) = -1 then
    OCLOperations.Add(OperationClass);
end;

function ExtractDateTimeFromMoment(moment: TBAMoment): TDateTime;
begin
  if Moment is TBADateTime then
    Result := (Moment as TBADateTime).asDateTime
  else if Moment is TBADate then
    Result := (Moment as TBADate).asDate
  else if Moment is TBATime then
    Result := (Moment as TBATime).asTime
  else
    Result := 0;
end;

type

  TBOS_AbstractCompare = class(TBoldOclSymbol)
  protected
    function CompareEnumLiterals(const Params: TBoldOclSymbolParameters): Boolean;
  end;

  TBOS_Equal = class(TBOS_AbstractCompare)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_NotEqual = class(TBOS_AbstractCompare)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Add = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Subtract = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_UnaryMinus = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Multiply = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Divide = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Abs = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Floor = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Round = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_SimpleRound = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_SimpleRoundTo = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_strToInt = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_strToFloat = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Min = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Max = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Less = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Greater = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_LessEQ = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_GreaterEQ = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Div = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_SafeDivZero = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Mod = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Length = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_concat = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_ToUpper = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_toLower = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_SubString = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Contains = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Pad = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_PostPad = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Format = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_FormatNumeric = class(TBOS_Format)
  protected
    procedure Init; override;
  end;

  TBOS_FormatFloat = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_FormatDateTime = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_AsISODateTime = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_AsISODate = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_StrToDate = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_StrToTime = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_StrToDateTime = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_DayOfDate = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_MonthOfDate = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_YearOfDate = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_WeekOfDate = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_DayOfWeekOfDate = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_HoursBetween = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_MinutesBetween = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_SecondsBetween = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_or = class(TBoldOclSymbol)
  protected
    procedure Init; override;
    function GetShortCircuitType: ShortCircuitType; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_and = class(TBoldOclSymbol)
  protected
    procedure Init; override;
    function GetShortCircuitType: ShortCircuitType; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_not = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_xor = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_implies = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_if = class(TBoldOclSymbol)
  protected
    procedure Init; override;
    function GetShortCircuitType: ShortCircuitType; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Size = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_includes = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Count = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_IncludesAll = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_isEmpty = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_NotEmpty = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_isNull = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Sum = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;


  TBOS_Maxvalue = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_MinValue = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Average = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Exists = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_ForAll = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_union = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Intersection = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_difference = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Including = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_excluding = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_SymmetricDifference = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Select = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_reject = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_collect = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS__ListCopier = class(TBoldOclSymbol)
  protected
    procedure CopyListToResult(const Params: TBoldOclSymbolParameters);
  end;

  TBOS_AsSequence = class(TBOS__ListCopier)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_AsBag = class(TBOS__ListCopier)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_AsSet = class(TBOS__ListCopier)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Append = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Prepend = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_SubSequence = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_at = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_first = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_last = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_GenericOrder = class(TBoldOclSymbol)
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_orderby = class(TBOS_GenericOrder)
  protected
    procedure Init; override;
  end;

  TBOS_orderDescending = class(TBOS_GenericOrder)
  protected
    procedure Init; override;
  end;

  TBOS_asString = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_AsFloat = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_FloatAsDateTime = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_datePart = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_asDateTime = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;


  TBOS_StringRepresentation = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_TaggedValue = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;


  TBOS_TypeName = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Attributes = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_AssociationEnds = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

{  TBOS_Operations = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;
}
  TBOS_SuperTypes = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_AllSuperTypes = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_AllSubClasses = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;


  TBOS_AllInstances = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_NullValue = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;


  TBOS_AllLoadedObjects = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_EmptyLIst = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;


  TBOS_oclType = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_oclIsKindOf = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_OclIsTypeOf = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_OclAsType = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_safeCast = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;


  TBOS_SQLLike = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_SQLLikeCaseInsensitive = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_RegExpMatch = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_InDateRange = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_InTimeRange = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Constraints = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_AtTime = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_ObjectTime = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_ObjectTimeStamp = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_AllInstancesAtTime = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Existing = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_FilterOnType = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_BoldTime = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_TimeStampToTime = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_TimeToTimeStamp = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_IndexOf = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_ReverseCollection = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_asCommaText = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_separate = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_CommaSeparatedStringToCollection = class(TBoldOclSymbol)
  protected
    function GetListTypeInfo: TBoldListTypeInfo; virtual; abstract;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_CommaSeparatedStringToStringCollection = class(TBOS_CommaSeparatedStringToCollection)
  protected
    procedure Init; override;
    function GetListTypeInfo: TBoldListTypeInfo; override;
  end;

  TBOS_CommaSeparatedStringToIntegerCollection = class(TBOS_CommaSeparatedStringToCollection)
  protected
    procedure Init; override;
    function GetListTypeInfo: TBoldListTypeInfo; override;
  end;

  TBOS_Trim = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_HasDuplicates = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_BoldId = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Power = class(TBoldOCLSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_Sqrt = class(TBoldOCLSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_BoldIDIn = class(TBoldOclSymbol)
  protected
    procedure Init; override;
    function GetShortCircuitType: ShortCircuitType; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

type
  TBOS_NamedIdIn = class(TBoldOclSymbol)
  protected
    procedure Init; override;
    function GetShortCircuitType: ShortCircuitType; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;


procedure TBOS_Equal.Init;
begin
  InternalInit('=', [nil, nil], tbodNo, HELP.BooleanType, False, 100, true);
end;

procedure TBOS_NotEqual.Init;
begin
  InternalInit('<>', [nil, nil], tbodNo, HELP.BooleanType, False, 101, true);
end;

procedure TBOS_Add.Init;
begin
  InternalInit('+', [nil, nil], tbodLCC, nil, False, 102);
end;

procedure TBOS_Subtract.Init;
begin
  InternalInit('-',[HELP.NumericType, HELP.NumericType], tbodLCC, nil, False, 103);
end;

procedure TBOS_UnaryMinus.Init;
begin
  InternalInit('unary-', [HELP.NumericType], tbodCopyArg1, nil, False, 104);
end;

procedure TBOS_Multiply.Init;
begin
  InternalInit('*', [HELP.NumericType, HELP.NumericType], tbodLCC, nil, False, 105);
end;

procedure TBOS_Divide.Init;
begin
  InternalInit('/', [HELP.NumericType, HELP.NumericType], tbodNo, HELP.RealType, False, 106);
end;

procedure TBOS_SafeDivZero.Init;
begin
  InternalInit('safediv', [Help.NumericType, Help.NumericType], tbodNo, Help.NumericType, True, 106);
end;

procedure TBOS_Abs.Init;
begin
  InternalInit('abs', [HELP.NumericType], tbodCopyArg1, nil, True, 107);
end;

procedure TBOS_Floor.Init;
begin
  InternalInit('floor', [HELP.NumericType], tbodNo, HELP.IntegerType, True, 108);
end;

procedure TBOS_Round.Init;
begin
  InternalInit('round', [HELP.NumericType], tbodNo, HELP.IntegerType, True, 109);
end;

procedure TBOS_SimpleRound.Init;
begin
  InternalInit('simpleRound', [HELP.NumericType], tbodNo, HELP.IntegerType, True, 109);
end;

procedure TBOS_SimpleRoundTo.Init;
begin
  InternalInit('simpleRoundTo', [HELP.NumericType, HELP.IntegerType], tbodNo, HELP.IntegerType, True, 109);
end;

procedure TBOS_strToInt.Init;
begin
  InternalInit('strToInt', [HELP.stringType], tbodNo, HELP.IntegerType, True, 109);
end;

procedure TBOS_strToFloat.Init;
begin
  InternalInit('strToFloat', [HELP.stringType], tbodNo, HELP.RealType, True, 109);
end;

procedure TBOS_Max.Init;
begin
  InternalInit('max', [HELP.NumericType, HELP.NumericType], tbodLCC, nil, True, 110);
end;

procedure TBOS_Min.Init;
begin
  InternalInit('min', [HELP.NumericType, HELP.NumericType], tbodLCC, nil, True, 111);
end;

procedure TBOS_Less.Init;
begin
  InternalInit('<', [nil, nil], tbodNo, HELP.BooleanType, False, 112);
end;

procedure TBOS_Greater.Init;
begin
  InternalInit('>', [nil, nil], tbodNo, HELP.BooleanType, False, 113);
end;

procedure TBOS_LessEQ.Init;
begin
  InternalInit('<=', [nil, nil], tbodNo, HELP.BooleanType, False, 114);
end;

procedure TBOS_GreaterEQ.Init;
begin
  InternalInit('>=', [nil, nil], tbodNo, HELP.BooleanType, False, 115);
end;

procedure TBOS_Div.Init;
begin
  InternalInit('div', [HELP.IntegerType, HELP.IntegerType], tbodNo, HELP.IntegerType, False, 116);
end;

procedure TBOS_Mod.Init;
begin
  InternalInit('mod', [HELP.IntegerType, HELP.IntegerType], tbodNo, HELP.IntegerType, False, 117);
end;

procedure TBOS_Length.Init;
begin
  InternalInit('length', [HELP.StringType], tbodNo, HELP.IntegerType, True, 118);
end;

procedure TBOS_concat.Init;
begin
  InternalInit('concat', [HELP.StringType, HELP.StringType], tbodNo, HELP.StringType, True, 119);
end;

procedure TBOS_ToUpper.Init;
begin
  InternalInit('toUpper', [HELP.StringType], tbodNo, HELP.StringType, True, 120);
end;

procedure TBOS_toLower.Init;
begin
  InternalInit('toLower', [HELP.StringType], tbodNo, HELP.StringType, True, 121);
end;

procedure TBOS_SubString.Init;
begin
  InternalInit('subString', [HELP.StringType, HELP.IntegerType, HELP.IntegerType], tbodNo, HELP.StringType, True, 122);
end;

procedure TBOS_Contains.Init;
begin
  InternalInit('contains', [HELP.StringType, HELP.StringType], tbodNo, HELP.BooleanType, True, 122);
end;

procedure TBOS_Pad.Init;
begin
  InternalInit('pad', [HELP.StringType, HELP.IntegerType, HELP.StringType], tbodNo, HELP.StringType, True, 123);
end;

procedure TBOS_PostPad.Init;
begin
  InternalInit('postPad', [HELP.StringType, HELP.IntegerType, HELP.StringType], tbodNo, HELP.StringType, True, 124);
end;

procedure TBOS_format.Init;
begin
  InternalInit('format', [HELP.NumericType, HELP.StringType], tbodNo, HELP.StringType, True, 124);
end;

procedure TBOS_FormatFloat.Init;
begin
  InternalInit('formatFloat', [HELP.NumericType, HELP.StringType], tbodNo, HELP.StringType, True, 124);
end;

procedure TBOS_formatNumeric.Init;
begin
  InternalInit('formatNumeric', [HELP.NumericType, HELP.StringType], tbodNo, HELP.StringType, True, 124);
end;

procedure TBOS_formatDateTime.Init;
begin
  InternalInit('formatDateTime', [HELP.MomentType, HELP.StringType], tbodNo, HELP.StringType, True, 124);
end;

procedure TBOS_AsISODateTime.Init;
begin
  InternalInit('asISODateTime', [HELP.MomentType], tbodNo, HELP.StringType, True, 124);
end;

procedure TBOS_AsISODate.Init;
begin
  InternalInit('asISODate', [HELP.MomentType], tbodNo, HELP.StringType, True, 124);
end;

procedure TBOS_StrToDate.Init;
begin
  InternalInit('strToDate', [HELP.StringType], tbodNo, HELP.DateType, True, 124);
end;

procedure TBOS_StrToTime.Init;
begin
  InternalInit('strToTime', [HELP.StringType], tbodNo, HELP.TimeType, True, 124);
end;

procedure TBOS_StrToDateTime.Init;
begin
  InternalInit('strToDateTime', [HELP.StringType], tbodNo, HELP.DateTimeType, True, 124);
end;

function TBOS_or.GetShortCircuitType: ShortCircuitType;
begin
  Result := csOr;
end;

procedure TBOS_or.Init;
begin
  InternalInit('or', [HELP.BooleanType, HELP.BooleanType], tbodNo, HELP.BooleanType, True, 125);
end;

function TBOS_and.GetShortCircuitType: ShortCircuitType;
begin
  Result := csAnd;
end;

procedure TBOS_and.Init;
begin
  InternalInit('and', [HELP.BooleanType, HELP.BooleanType], tbodNo, HELP.BooleanType, True, 126);
end;

procedure TBOS_xor.Init;
begin
  InternalInit('xor', [HELP.BooleanType, HELP.BooleanType], tbodNo, HELP.BooleanType, True, 127);
end;

procedure TBOS_implies.Init;
begin
  InternalInit('implies', [HELP.BooleanType, HELP.BooleanType], tbodNo, HELP.BooleanType, True, 128);
end;

procedure TBOS_not.Init;
begin
  InternalInit('not', [HELP.BooleanType], tbodNo, HELP.BooleanType, True, 129);
end;

function TBOS_if.GetShortCircuitType: ShortCircuitType;
begin
  Result := csIf;
end;

procedure TBOS_if.Init;
begin
  InternalInit('if', [HELP.BooleanType, nil, nil], tbodLCC23, nil, True, 130);
end;

procedure TBOS_Size.Init;
begin
  InternalInit('size', [HELP.ListType], tbodNo, HELP.IntegerType, True, 131);
end;

procedure TBOS_includes.Init;
begin
  InternalInit('includes', [HELP.ListType, nil], tbodNo, HELP.BooleanType, True, 132, true);
end;

procedure TBOS_Count.Init;
begin
  InternalInit('count', [HELP.ListType, nil], tbodNo, HELP.IntegerType, True, 133, true);
end;

procedure TBOS_IncludesAll.Init;
begin
  InternalInit('includesAll', [HELP.ListType, HELP.ListType], tbodNo, HELP.BooleanType, True, 134, true);
end;

procedure TBOS_isEmpty.Init;
begin
  InternalInit('isEmpty', [HELP.ListType], tbodNo, HELP.BooleanType, True, 135);
end;

procedure TBOS_NotEmpty.Init;
begin
  InternalInit('notEmpty', [HELP.ListType], tbodNo, HELP.BooleanType, True, 136);
end;

procedure TBOS_Sum.Init;
begin
  InternalInit('sum', [HELP.NumericListType], tbodCopyArg1Elem, nil, True, 137);
end;

procedure TBOS_MinValue.Init;
begin
  InternalInit('minValue', [HELP.NumericListType], tbodCopyArg1Elem, nil, True, 138);
end;

procedure TBOS_Maxvalue.Init;
begin
  InternalInit('maxValue', [HELP.NumericListType], tbodCopyArg1Elem, nil, True, 139);
end;

procedure TBOS_Average.Init;
begin
  InternalInit('average', [HELP.NumericListType], tbodNo, HELP.RealType, True, 140);
end;

procedure TBOS_Exists.Init;
begin
  InternalInit('exists', [HELP.ListType, HELP.BooleanType], tbodNo, HELP.BooleanType, True, 141);
end;

procedure TBOS_ForAll.Init;
begin
  InternalInit('forAll', [HELP.ListType, HELP.BooleanType], tbodNo, HELP.BooleanType, True, 142);
end;

procedure TBOS_union.Init;
begin
  InternalInit('union', [HELP.ListType, HELP.ListType], tbodLCC, nil, True, 143);
end;

procedure TBOS_Intersection.Init;
begin
  InternalInit('intersection', [HELP.ListType, HELP.ListType], tbodLCC, nil, True, 144, true);
end;

procedure TBOS_difference.Init;
begin
  InternalInit('difference', [HELP.ListType, HELP.ListType], tbodCopyArg1, nil, True, 145, true);
end;

procedure TBOS_Including.Init;
begin
  InternalInit('including', [HELP.ListType, HELP.ObjectType], tbodLCC, nil, True, 146);
end;

procedure TBOS_excluding.Init;
begin
  InternalInit('excluding', [HELP.ListType, HELP.ObjectType], tbodCopyArg1, nil, True, 147);
end;

procedure TBOS_SymmetricDifference.Init;
begin
  InternalInit('symmetricDifference', [HELP.ListType, HELP.ListType], tbodLCC, nil, True, 148);
end;

procedure TBOS_Select.Init;
begin
  InternalInit('select', [HELP.ListType, HELP.BooleanType], tbodCopyArg1, nil, True, 149);
end;

procedure TBOS_reject.Init;
begin
  InternalInit('reject', [HELP.ListType, HELP.BooleanType], tbodCopyArg1, nil, True, 150);
end;

procedure TBOS_collect.Init;
begin
  InternalInit('collect', [HELP.ListType, nil], tbodListofArg2, nil, True, 151);
end;

procedure TBOS_AsSequence.Init;
begin
  InternalInit('asSequence', [HELP.ListType], tbodArg1AsList, nil, True, 152);
end;

procedure TBOS_AsBag.Init;
begin
  InternalInit('asBag', [HELP.ListType], tbodArg1AsList, nil, True, 153);
end;

procedure TBOS_AsSet.Init;
begin
  InternalInit('asSet', [HELP.ListType], tbodArg1AsList, nil, True, 154);
end;

procedure TBOS_Append.Init;
begin
  InternalInit('append', [HELP.ListType, HELP.ObjectType], tbodLCC, nil, True, 155);
end;

procedure TBOS_Prepend.Init;
begin
  InternalInit('prepend', [HELP.ListType, HELP.ObjectType], tbodLCC, nil, True, 156);
end;

procedure TBOS_SubSequence.Init;
begin
  InternalInit('subSequence', [HELP.ListType, HELP.IntegerType, HELP.IntegerType], tbodCopyArg1, nil, True, 157);
end;

procedure TBOS_at.Init;
begin
  InternalInit('at', [HELP.ListType, HELP.IntegerType], tbodCopyArg1Elem, nil, True, 158);
end;

procedure TBOS_first.Init;
begin
  InternalInit('first', [HELP.ListType], tbodCopyArg1Elem, nil, True, 159);
end;

procedure TBOS_last.Init;
begin
  InternalInit('last', [HELP.ListType], tbodCopyArg1Elem, nil, True, 160);
end;

procedure TBOS_orderby.Init;
begin
  InternalInit('orderby', [HELP.ListType, nil], tbodCopyArg1, nil, True, 161);
end;

procedure TBOS_orderDescending.Init;
begin
  InternalInit('orderdescending', [HELP.ListType, nil], tbodCopyArg1, nil, True, 162);
end;

procedure TBOS_asString.Init;
begin
  InternalInit('asString', [nil], tbodNo, HELP.StringType, True, 163);
end;

procedure TBOS_datePart.Init;
begin
  InternalInit('datePart', [HELP.MomentType], tbodNo, HELP.DateType, True, 163);
end;

procedure TBOS_asDateTime.Init;
begin
  InternalInit('asDateTime', [HELP.MomentType], tbodNo, HELP.DateTimeType, True, 163);
end;

procedure TBOS_TypeName.Init;
begin
  InternalInit('typename', [HELP.TypeType], tbodNo, HELP.StringType, True, 164);
end;

procedure TBOS_Attributes.Init;
begin
  InternalInit('attributes', [HELP.TypeType], tbodNo, HELP.StringListType, True, 165);
end;

procedure TBOS_AssociationEnds.Init;
begin
  InternalInit('associationEnds', [HELP.TypeType], tbodNo, HELP.StringListType, True, 166);
end;

{procedure TBOS_Operations.Init;
begin
  InternalInit('operations', [HELP.TypeType], tbodNo, HELP.StringListType, True, 167);
end;
}

procedure TBOS_SuperTypes.Init;
begin
  InternalInit('superTypes', [HELP.TypeType], tbodNo, HELP.TypeListType, True, 168);
end;

procedure TBOS_AllSuperTypes.Init;
begin
  InternalInit('allSuperTypes', [HELP.TypeType], tbodNo, HELP.TypeListType, True, 169);
end;

procedure TBOS_AllSubClasses.Init;
begin
  InternalInit('allSubClasses', [HELP.TypeType], tbodNo, HELP.TypeListType, True, 169);
end;

procedure TBOS_AllInstances.Init;
begin
  InternalInit('allInstances', [HELP.TypeType], tbodObjectList, nil, True, 170);
end;

procedure TBOS_AllLoadedObjects.Init;
begin
  InternalInit('allLoadedObjects', [HELP.TypeType], tbodObjectList, nil, True, 170);
end;

procedure TBOS_emptyList.Init;
begin
  InternalInit('emptyList', [HELP.TypeType], tbodObjectList, nil, True, 170);
end;

procedure TBOS_oclType.Init;
begin
  InternalInit('oclType', [nil], tbodNo, HELP.TypeType, True, 171);
end;

procedure TBOS_oclIsKindOf.Init;
begin
  InternalInit('oclIsKindOf', [nil, Help.TypeType], tbodNo, HELP.BooleanType, True, 172);
end;

procedure TBOS_OclIsTypeOf.Init;
begin
  InternalInit('oclIsTypeOf', [nil, Help.TypeType], tbodNo, HELP.BooleanType, True, 173);
end;

procedure TBOS_OclAsType.Init;
begin
  InternalInit('oclAsType', [nil, Help.TypeType], tbodTypeCast, nil, True, 174);
end;

procedure TBOS_SafeCast.Init;
begin
  InternalInit('safeCast', [nil, Help.TypeType], tbodTypeCast, nil, True, 174);
end;

procedure TBOS_sqlLike.Init;
begin
  InternalInit('sqlLike', [help.StringType, help.StringType], tbodNo, Help.BooleanType, True, 175);
end;

procedure TBOS_SqlLikeCaseInsensitive.Init;
begin
  InternalInit('sqlLikeCaseInsensitive', [help.StringType, help.StringType], tbodNo, Help.BooleanType, True, 176);
end;

procedure TBOS_RegExpMatch.Init;
begin
  InternalInit('regExpMatch', [help.StringType, help.StringType], tbodNo, Help.BooleanType, True, 177);
end;

procedure TBOS_InDateRange.Init;
begin
  InternalInit('inDateRange', [help.MomentType, help.NumericType, help.NumericType], tbodNo, Help.BooleanType, True, 178);
end;

procedure TBOS_InTimeRange.Init;
begin
  InternalInit('inTimeRange', [help.MomentType, help.NumericType, help.NumericType], tbodNo, Help.BooleanType, True, 179);
end;

procedure TBOS_isNull.Init;
begin
  InternalInit('isNull', [nil], tbodNo, HELP.BooleanType, True, 180);
end;

procedure TBOS_Constraints.Init;
var
  ConstraintListTypeInfo: TBoldListTypeInfo;
begin
  ConstraintListTypeInfo := Help.SystemTypeInfo.ListTypeInfoByElement[Help.ConstraintType];
  InternalInit('constraints', [nil], tbodNo, ConstraintListTypeInfo, True, 181);
end;

procedure TBOS_AtTime.Init;
begin
  InternalInit('atTime', [help.ObjectType, help.IntegerType], tbodCopyArg1, nil, True, 182);
end;

procedure TBOS_ObjectTimeStamp.Init;
begin
  InternalInit('objectTimeStamp', [help.ObjectType], tbodNo, Help.IntegerType, True, 182);
end;

procedure TBOS_ObjectTime.Init;
begin
  InternalInit('boldTime', [help.ObjectType], tbodNo, Help.IntegerType, True, 182);
end;

procedure TBOS_allInstancesAtTime.Init;
begin
  InternalInit('allInstancesAtTime', [help.TypeType, help.IntegerType], tbodObjectList, nil, True, 183);
end;

procedure TBOS_existing.Init;
begin
  InternalInit('existing', [help.ObjectType], tbodNo, HELP.BooleanType, True, 184);
end;

procedure TBOS_FilterOnType.Init;
begin
  InternalInit('filterOnType', [help.ListType, help.TypeType], tbodListFromArg2, nil, True, 185);
end;

procedure TBOS_BoldTime.Init;
begin
  InternalInit('boldTime', [help.ObjectType], tbodno, help.integerType, True, 186);
end;

procedure TBOS_TimeStampToTime.Init;
begin
  InternalInit('timeStampToTime', [help.IntegerType], tbodno, help.DateTimeType, True, 187);
end;

procedure TBOS_TimeToTimeStamp.Init;
begin
  InternalInit('timeToTimeStamp', [help.DateTimeType], tbodno, help.IntegerType, True, 188);
end;

procedure TBOS_StringRepresentation.Init;
begin
  InternalInit('stringRepresentation', [nil, help.integerType], tbodNo, HELP.StringType, True, 190);
end;

procedure TBOS_TaggedValue.Init;
begin
  InternalInit('taggedValue', [help.ObjectType, help.StringType], tbodNo, HELP.StringType, True, 190);
end;

procedure TBOS_IndexOf.Init;
begin
  InternalInit('indexOf', [HELP.ListType, nil{HELP.ObjectType}], tbodNo, HELP.IntegerType, True, 191, true);
end;

procedure TBOS_ReverseCollection.Init;
begin
  InternalInit('reverseCollection', [HELP.ListType], tbodCopyArg1, nil, True, 192);
end;

procedure TBOS_HasDuplicates.Init;
begin
  InternalInit('hasDuplicates', [HELP.ListType], tbodNo, HELP.BooleanType, True, 193);
end;

procedure TBOS_BoldId.Init;
begin
  InternalInit('boldID', [Help.ObjectType], tbodNo, Help.IntegerType, True, 194);
end;

procedure TBOS_Power.Init;
begin
  InternalInit('power', [Help.NumericType, Help.NumericType], tbodNo, Help.RealType, True, 0);
end;

procedure TBOS_Sqrt.Init;
begin
  InternalInit('sqrt', [Help.NumericType], tbodNo, Help.RealType, True, 0);
end;

{-- SymbolImplementations --}

function TBOS_AbstractCompare.CompareEnumLiterals(const Params: TBoldOclSymbolParameters): Boolean;
var
  str: String;
  valueSet: TBAValueSet;
begin
  Str := '';
  ValueSet := nil;
  if Params.nodes[0] is TBoldOclEnumLiteral then
    str := (Params.nodes[0] as TBoldOClEnumLiteral).Name
  else if Params.Values[0] is TBAValueSet then
    ValueSet := Params.values[0] as TBAValueSet;

  if Params.nodes[1] is TBoldOclEnumLiteral then
  begin
    if Str = '' then
      str := (Params.nodes[1] as TBoldOClEnumLiteral).Name
    else
      raise EBoldOclRunTimeError.CreateFmt(sCannotCompareEnumLiterals,
        [0, str, (Params.nodes[1] as TBoldOClEnumLiteral).Name]);
  end
  else if Params.Values[1] is TBAValueSet then
    ValueSet := Params.values[0] as TBAValueSet;

  Result := assigned(ValueSet) and ValueSet.CompareToEnumLiteral(Str);
end;

procedure TBOS_Equal.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if (Params.nodes[0] is TBoldOclEnumLiteral) or (Params.nodes[1] is TBoldOclEnumLiteral) then
    HELP.MakeNewBoolean(Params.Result, CompareEnumLiterals(Params))
  else if not assigned(Params.values[0]) then
    Help.MakeNewBoolean(Params.Result, not Assigned(Params.values[1]))
  else if not Assigned(Params.values[1]) then
    Help.MakeNewBoolean(Params.Result, false)
  else
    HELP.MakeNewBoolean(Params.Result, Params.values[0].IsEqual(Params.values[1]));
end;

procedure TBOS_NotEqual.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if (Params.nodes[1] is TBoldOclEnumLiteral) or (Params.nodes[0] is TBoldOclEnumLiteral) then
    HELP.MakeNewBoolean(Params.Result, not CompareEnumLiterals(Params))
  else if not assigned(Params.values[0]) then
    Help.MakeNewBoolean(Params.Result, Assigned(Params.values[1]))
  else if not Assigned(Params.values[1]) then
    Help.MakeNewBoolean(Params.Result, true)
  else
    HELP.MakeNewBoolean(Params.Result, not Params.values[0].IsEqual(Params.values[1]));
end;


{-- Real operations --}

procedure TBOS_Add.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if Params.Result.BoldType.ConformsTo(HELP.StringType) then
    HELP.MakeNewString(Params.Result, XString(Params.values[0]) + XString(Params.values[1]))
  else if Params.Result.BoldType.ConformsTo(HELP.IntegerType) then
    HELP.MakeNewInteger(Params.Result, XInteger(Params.values[0]) + XInteger(Params.values[1]))
  else if Params.Result.BoldType.ConformsTo(HELP.CurrencyType) then
    HELP.MakeNewCurrency(Params.Result, XCurrency(Params.values[0]) + Xcurrency(Params.values[1]))
  else if Params.Result.BoldType.ConformsTo(HELP.NumericType) then
    HELP.MakeNewNumeric(Params.Result, XNumeric(Params.values[0]) + XNumeric(Params.values[1]))
  else if Params.Result.BoldType.ConformsTo(HELP.MomentType) then
    HELP.MakeNewDateTime(Params.Result, XDateTime(Params.values[0]) + XDateTime(Params.values[1]))
  else
  begin
    raise EBoldOclRunTimeError.CreateFmt(borteOperationNotDefinedOnArgTypes,
      [0, Params.values[0].BoldType.AsString, '+', Params.values[1].BoldType.AsString]);
  end;
end;

procedure TBOS_Subtract.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if Params.Result.BoldType.ConformsTo(HELP.IntegerType) then
    HELP.MakeNewInteger(Params.Result, XInteger(Params.values[0]) - XInteger(Params.values[1]))
  else if Params.Result.BoldType.ConformsTo(HELP.CurrencyType) then
    HELP.MakeNewCurrency(Params.Result, XCurrency(Params.values[0]) - XCurrency(Params.values[1]))
  else
    HELP.MakeNewNumeric(Params.Result, XNumeric(Params.values[0]) - XNumeric(Params.values[1]));
end;

procedure TBOS_UnaryMinus.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if Params.Result.BoldType.ConformsTo(HELP.IntegerType) then
    HELP.MakeNewInteger(Params.Result, -XInteger(Params.values[0]))
  else if Params.Result.BoldType.ConformsTo(HELP.CurrencyType) then
    HELP.MakeNewCurrency(Params.Result, -XCurrency(Params.values[0]))
  else
    HELP.MakeNewNumeric(Params.Result, -XNumeric(Params.values[0]));
end;

procedure TBOS_Multiply.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if Params.Result.BoldType.ConformsTo(HELP.IntegerType) then
    HELP.MakeNewInteger(Params.Result, XInteger(Params.values[0]) * XInteger(Params.values[1]))
  else if Params.Result.BoldType.ConformsTo(HELP.CurrencyType) then
    HELP.MakeNewCurrency(Params.Result, XCurrency(Params.values[0]) * XCurrency(Params.values[1]))
  else
    HELP.MakeNewNumeric(Params.Result, XNumeric(Params.values[0]) * XNumeric(Params.values[1]));
end;

procedure TBOS_Divide.Evaluate(const Params: TBoldOclSymbolParameters);

  procedure RaiseError;
  begin
    raise EBoldOclRuntimeError.CreateFmt(borteDivisionByZero, [0]);
  end;

begin
  try
    HELP.MakeNewNumeric(Params.Result, XNumeric(Params.values[0]) / XNumeric(Params.values[1]));
  except
    on e: EInvalidOp do
      RaiseError;
    on e: EDivByZero do
      RaiseError;
    on e: EZeroDivide do
      RaiseError;
  end;
end;

{ TBOS_SafeDivZero }

procedure TBOS_SafeDivZero.Evaluate(
  const Params: TBoldOclSymbolParameters);
begin
  try
    HELP.MakeNewNumeric(Params.Result, XNumeric(Params.values[0]) / XNumeric(Params.values[1]));
  except
    on e: EInvalidOp do;
    on e: EDivByZero do;
    on e: EZeroDivide do;
  end;
end;

procedure TBOS_Abs.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if Params.Result.BoldType.ConformsTo(HELP.integerType) then
    HELP.MakeNewInteger(Params.Result, abs(XInteger(Params.values[0])))
  else if Params.Result.BoldType.ConformsTo(HELP.CurrencyType) then
    HELP.MakeNewCurrency(Params.Result, abs(XCurrency(Params.values[0])))
  else
    HELP.MakeNewNumeric(Params.Result, abs(XNumeric(Params.values[0])));
end;

procedure TBOS_Floor.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewInteger(Params.Result, Floor(XNumeric(Params.values[0])));
end;

procedure TBOS_Round.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewInteger(Params.Result, Round(XNumeric(Params.values[0])));
end;

procedure TBOS_SimpleRound.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewNumeric(Params.Result, math.Floor((XNumeric(Params.values[0])+0.5)));
end;

procedure TBOS_SimpleRoundTo.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Value: double;
  Digits: TRoundToRange;
begin
  Value := XNumeric(Params.values[0]);
  Digits := Trunc(XNumeric(Params.values[1]));
  HELP.MakeNewNumeric(Params.Result, math.SimpleRoundTo( Value, Digits ));
end;

procedure TBOS_StrToInt.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewInteger(Params.Result, StrToInt(XString(Params.values[0])));
end;

procedure TBOS_strToFloat.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewNumeric(Params.Result, StrToFloat(XString(Params.values[0])));
end;

procedure TBOS_Max.Evaluate(const Params: TBoldOclSymbolParameters);
var
  result: TBoldElement;
begin
  result := Params.values[0];
  if Assigned(result) and Assigned(Params.values[1]) then
  begin
    if result.CompareTo(Params.Values[1]) = -1 then
      result := Params.values[1];
  end
  else
  if not Assigned(result) then
    result := Params.values[1];
  if not Assigned(result) then
    HELP.MakeNewNull(Params.Result, Params.Result.BoldType)
  else
  begin
    HELP.MakeNew(Params.Result, Params.Result.BoldType);
    Params.Result.Value.Assign(Result);
  end;
end;

procedure TBOS_Min.Evaluate(const Params: TBoldOclSymbolParameters);
var
  result: TBoldElement;
begin
  result := Params.values[0];
  if Assigned(result) and Assigned(Params.values[1]) then
  begin
    if result.CompareTo(Params.Values[1]) = 1 then
      result := Params.values[1];
  end
  else
  if not Assigned(result) then
    result := Params.values[1];
  if not Assigned(result) then
    HELP.MakeNewNull(Params.Result, Params.Result.BoldType)
  else
  begin
    HELP.MakeNew(Params.Result, Params.Result.BoldType);
    Params.Result.Value.Assign(Result);
  end;
end;

procedure TBOS_Less.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if assigned(Params.values[0]) and assigned(Params.values[1]) then
    HELP.MakeNewBoolean(Params.Result, Params.values[0].CompareTo(Params.values[1]) < 0)
  else
    Help.MakenewBoolean(Params.Result, false);
end;

procedure TBOS_Greater.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if assigned(Params.values[0]) and assigned(Params.values[1]) then
    HELP.MakeNewBoolean(Params.Result, Params.values[0].CompareTo(Params.values[1]) > 0)
  else
    Help.MakenewBoolean(Params.Result, false);
end;

procedure TBOS_LessEQ.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if assigned(Params.values[0]) and assigned(Params.values[1]) then
    HELP.MakeNewBoolean(Params.Result, Params.values[0].CompareTo(Params.values[1]) <= 0)
  else
    Help.MakenewBoolean(Params.Result, false);
end;

procedure TBOS_GreaterEQ.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if assigned(Params.values[0]) and assigned(Params.values[1]) then
    HELP.MakeNewBoolean(Params.Result, Params.values[0].CompareTo(Params.values[1]) >= 0)
  else
    Help.MakenewBoolean(Params.Result, false);
end;

{-- integer operations --}

procedure TBOS_Div.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  try
    HELP.MakeNewInteger(Params.Result, XInteger(Params.values[0]) div XInteger(Params.values[1]));
  except
    on e: EDivByZero do
      raise EBoldOclRuntimeError.CreateFmt(borteDivisionByZero, [0]);
    on e: EZeroDivide do
      raise EBoldOclRuntimeError.CreateFmt(borteDivisionByZero, [0]);
  end;
end;

procedure TBOS_Mod.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewInteger(Params.Result, XInteger(Params.values[0]) mod XInteger(Params.values[1]));
end;

{-- String operations --}

procedure TBOS_Length.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewInteger(Params.Result, Length(XString(Params.values[0])));
end;

procedure TBOS_concat.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewString(Params.Result, XString(Params.values[0]) + XString(Params.values[1]));
end;

procedure TBOS_ToUpper.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewString(Params.Result, XString(Params.values[0]).ToUpper);
end;

procedure TBOS_toLower.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewString(Params.Result, XString(Params.values[0]).ToLower);
end;

procedure TBOS_SubString.Evaluate(const Params: TBoldOclSymbolParameters);
var
  S         : string;
  lower, upper: Integer;
begin
  lower := XInteger(Params.values[1]);
  upper := XInteger(Params.values[2]);
  S := Copy(XString(Params.values[0]), lower, upper - lower + 1);
  HELP.MakeNewString(Params.Result, S);
end;

procedure TBOS_Contains.Evaluate(const Params: TBoldOclSymbolParameters);
var
  s, subs : string;
begin
  S := XString(Params.values[0]);
  Subs := XString(Params.values[1]);
  HELP.MakeNewBoolean(Params.Result, Pos(Subs, S) > 0);
end;

procedure TBOS_Pad.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Finallength: Integer;
  padder: string;
  PaddedStr: String;
begin
  PaddedStr := xString(Params.values[0]);
  FinalLength := xInteger(Params.values[1]);
  if length(PaddedStr) < finalLength then
  begin
    Padder := xString(Params.values[2]);
    if Padder = '' then
      raise EBoldOclRuntimeError.CreateFmt(bortePadStringEmpty, [0]);

    while length(PaddedStr) < Finallength do
      PaddedStr := Padder + PaddedStr;

    if length(PaddedStr) <> FinalLength then
      PaddedStr := copy(PaddedStr, length(PaddedStr) - FinalLength + 1, FinalLength);
  end;

  Help.MakeNewString(Params.Result, PaddedStr);
end;

procedure TBOS_PostPad.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Finallength: Integer;
  Padder: string;
  PaddedStr: String;
begin
  PaddedStr := xString(Params.values[0]);
  FinalLength := xInteger(Params.values[1]);
  if length(PaddedStr) < finalLength then
  begin
    Padder := xString(Params.values[2]);
    if Padder = '' then
      raise EBoldOclRuntimeError.CreateFmt(bortePadStringEmpty, [0]);

    while length(PaddedStr) < Finallength do
      PaddedStr := PaddedStr + Padder;

    if length(PaddedStr) <> FinalLength then
      PaddedStr := copy(PaddedStr, 1, FinalLength);
  end;

  Help.MakeNewString(Params.Result, PaddedStr);
end;

procedure TBOS_Format.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  help.MakeNewString(params.result, format(XString(Params.values[1]), [XNumeric(Params.values[0])]));
end;

procedure TBOS_FormatFloat.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  help.MakeNewString(params.result, formatFloat(XString(Params.values[1]), XNumeric(Params.values[0])));
end;

procedure TBOS_AsISODateTime.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Moment: TBAMoment;
begin
  Moment := Params.values[0] as TBAMoment;
  if not Assigned(moment) or moment.IsNull then
    HELP.MakeNewNull(Params.Result, Params.Result.BoldType)
  else
    help.MakeNewString(params.result, formatDateTime(cIsoDateTimeFormat, ExtractDateTimeFromMoment(Moment)));
end;

procedure TBOS_AsISODate.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Moment: TBAMoment;
begin
  Moment := Params.values[0] as TBAMoment;
  if not Assigned(moment) or moment.IsNull then
    HELP.MakeNewNull(Params.Result, Params.Result.BoldType)
  else
    help.MakeNewString(params.result, formatDateTime(cIsoDateFormat, ExtractDateTimeFromMoment(Moment)));
end;

procedure TBOS_FormatDateTime.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Moment: TBAMoment;
begin
  Moment := Params.values[0] as TBAMoment;
  if not Assigned(moment) or moment.IsNull then
  begin
    help.MakeNewString(params.result, '');
  end
  else
  begin
    help.MakeNewString(params.result, formatDateTime(XString(Params.values[1]), ExtractDateTimeFromMoment(Moment)));
  end;
end;

procedure TBOS_StrToDateTime.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  help.MakeNewDateTime(params.result, StrToDateTime(xString(Params.values[0])));
end;

procedure TBOS_StrToTime.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  help.MakeNewTime(params.result, StrToTime(xString(Params.values[0])));
end;

procedure TBOS_StrToDate.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  help.MakeNewDate(params.result, StrToDate(xString(Params.values[0])));
end;

{-- Boolean Operations --}

procedure TBOS_or.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewBoolean(Params.Result, XBoolean(Params.values[0]) or XBoolean(Params.values[1]));
end;

procedure TBOS_xor.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewBoolean(Params.Result, XBoolean(Params.values[0]) xor XBoolean(Params.values[1]));
end;

procedure TBOS_and.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewBoolean(Params.Result, XBoolean(Params.values[0]) and XBoolean(Params.values[1]));
end;

procedure TBOS_not.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewBoolean(Params.Result, not XBoolean(Params.values[0]));
end;

procedure TBOS_implies.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewBoolean(Params.Result, not XBoolean(Params.values[0]) or XBoolean(Params.values[1]));
end;

procedure TBOS_if.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if XBoolean(Params.values[0]) then
    Params.nodes[1].TransferValue(Params.Result)
  else
    Params.nodes[2].TransferValue(Params.Result);
end;

procedure TBOS_Size.Evaluate(const Params: TBoldOclSymbolParameters);
var
  list: TBoldLIst;
begin
  List := XList(Params.values[0]);
  if not assigned(list) then
    Help.MakeNewInteger(Params.Result, 0)
  else
    HELP.MakeNewInteger(Params.Result, List.Count);
end;

procedure TBOS_Count.Evaluate(const Params: TBoldOclSymbolParameters);
var
  i  : Integer;
  Count: Integer;
  List: TBoldList;
  Elem: TBoldElement;
begin
  Count := 0;
  List := XList(Params.values[0]);
  If not assigned(list) then
    Help.MakeNewInteger(Params.Result, 0)
  else
  begin
    Elem := Params.values[1];
    List.EnsureRange(0, List.Count - 1);
    for i := 0 to List.Count - 1 do
      if List[i].IsEqual(Elem) then inc(Count);
    HELP.MakeNewInteger(Params.Result, Count);
  end;
end;

procedure TBOS_IncludesAll.Evaluate(const Params: TBoldOclSymbolParameters);
var
  i         : Integer;
  list1, list2: TBoldList;
  IncludesAll: Boolean;
begin
  i := 0;
  list1 := XList(Params.values[0]);
  list2 := XList(Params.values[1]);

  if not assigned(List2) or (list2.count = 0) then
    HELP.MakeNewBoolean(Params.Result, true)
  else if not assigned(list1) then
    HELP.MakeNewBoolean(Params.Result, false)
  else
  begin
    IncludesAll := True;

    while (i < list1.Count) and IncludesAll do
    begin
      IncludesAll := IncludesAll and list2.Includes(list1[i]);
      inc(i);
    end;
    HELP.MakeNewBoolean(Params.Result, IncludesAll);
  end;
end;

procedure TBOS_isEmpty.Evaluate(const Params: TBoldOclSymbolParameters);
var
  List: TBoldList;
begin
  List := XList(Params.values[0]);

  if not assigned(list) then
    HELP.MakeNewBoolean(Params.Result, true)
  else
    HELP.MakeNewBoolean(Params.Result, List.Count = 0);
end;

procedure TBOS_NotEmpty.Evaluate(const Params: TBoldOclSymbolParameters);
var
  List: TBoldList;
begin
  List := XList(Params.values[0]);

  if not assigned(list) then
    HELP.MakeNewBoolean(Params.Result, false)
  else
    HELP.MakeNewBoolean(Params.Result, List.Count <> 0);
end;

procedure TBOS_IsNull.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if Params.values[0] is TBoldAttribute then
    HELP.MakeNewBoolean(Params.Result, (Params.values[0] as TBoldAttribute).IsNull)
  else
    HELP.MakeNewBoolean(Params.Result, Params.values[0] = nil);
end;

procedure TBOS_Select.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if Assigned(XList(Params.Result.value)) and XBoolean(Params.values[0]) then
    XList(Params.Result.value).Add(Params.values[1]);
end;

procedure TBOS_reject.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if not (Assigned(XList(Params.Result.value)) and XBoolean(Params.values[0])) then
    XList(Params.Result.value).Add(Params.values[1]);
end;

procedure TBOS_collect.Evaluate(const Params: TBoldOclSymbolParameters);
var
  ResultList: TBoldList;
  SourceList: TBoldList;
begin
  ResultList := XList(Params.Result.value);
  if assigned(Resultlist) then
  begin
    sourceList := XList(Params.values[0]);
    if assigned(SourceList) then
    begin
      ResultList.AddList(SourceList);
    end
    else
    begin
      if Assigned(Params.values[0]) then
        resultList.Add(Params.values[0]);
    end;
  end;
end;

procedure TBOS_GenericOrder.Evaluate(const Params: TBoldOclSymbolParameters);
var
  SortList: TBoldMemberList;
  DummyValue: TBoldAttribute;
begin
  Sortlist := XList(Params.Result.value) as TBoldMemberList;
  if Assigned(SortList) then
  begin
    if assigned(Params.values[0]) then
      SortList.Add(Params.values[0] as TBoldMember)
    else
    begin
      DummyValue := TBoldMemberFactory.CreateMemberFromBoldType(params.nodes[0].BoldType) as TBoldAttribute;
      DummyValue.SetToNull;
      sortlist.add(dummyValue);
      if Sortlist.CloneMembers then
        DummyValue.Free;
    end;
  end;
end;

{procedure TBOS_orderdescending.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if Assigned(XList(Params.Result.value)) and assigned(Params.values[0]) then
    XList(Params.Result.value).Add(Params.values[0]);
end;
}

procedure TBOS_Sum.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Sum          : Double;
  i            : Integer;
  List         : TBoldList;
  ListElementType: TBoldElementTypeInfo;
begin
  Sum := 0;
  List := XList(Params.values[0]);

  ListElementType := Params.Result.BoldType;

  if not assigned(list) then
    Sum := 0
  else
    for i := 0 to List.Count - 1 do
      if not TBoldAttribute(List[i]).IsNull then
      Sum := Sum + XNumeric(List[i]);

  if ListElementType.ConformsTo(HELP.IntegerType) then
    HELP.MakeNewInteger(Params.Result, Round(Sum))
  else if ListElementType.ConformsTo(HELP.CurrencyType) then
    HELP.MakeNewCurrency(Params.Result, Sum)
  else if ListElementType.ConformsTo(HELP.TimeType) then
    HELP.MakeNewDateTime(Params.Result, Sum)
  else
    HELP.MakeNewNumeric(Params.Result, Sum);
end;

procedure TBOS_Maxvalue.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Max          : Variant;
  i            : Integer;
  List         : TBoldList;
  ListElementType: TBoldElementTypeInfo;
begin
  Max := Null;
  List := XList(Params.values[0]);
  ListElementType := Params.Result.BoldType;
  if assigned(list) then
  for i := 0 to List.Count - 1 do
  begin
    if not TBoldAttribute(List[i]).IsNull then
    begin
      if VarIsNull(Max) or (Max < XNumeric(List[i])) then
        Max := XNumeric(List[i]);
    end;
  end;
  if VarIsNull(Max) then
    HELP.MakeNewNull(Params.Result, Params.Result.BoldType)
  else if ListElementType.ConformsTo(HELP.IntegerType) then
    HELP.MakeNewInteger(Params.Result, Round(Max))
  else if ListElementType.ConformsTo(HELP.CurrencyType) then
    HELP.MakeNewCurrency(Params.Result, Max)
  else if ListElementType.ConformsTo(HELP.MomentType) then
    HELP.MakeNewDateTime(Params.Result, Max)
  else
    HELP.MakeNewNumeric(Params.Result, Max);
end;

procedure TBOS_MinValue.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Min          : Variant;
  i            : Integer;
  List         : TBoldList;
  ListElementType: TBoldElementTypeInfo;
begin
  Min := Null;
  List := XList(Params.values[0]);
  ListElementType := Params.Result.BoldType;
  if assigned(list) then
  for i := 0 to List.Count - 1 do
  begin
    if not TBoldAttribute(List[i]).IsNull then
  begin
      if VarIsNull(Min) or (Min > XNumeric(List[i])) then
        Min := XNumeric(List[i]);
  end;
  end;
  if VarIsNull(Min) then
    HELP.MakeNewNull(Params.Result, Params.Result.BoldType)
  else if ListElementType.ConformsTo(HELP.IntegerType) then
    HELP.MakeNewInteger(Params.Result, Round(Min))
  else if ListElementType.ConformsTo(HELP.CurrencyType) then
    HELP.MakeNewCurrency(Params.Result, Min)
  else if ListElementType.ConformsTo(HELP.MomentType) then
    HELP.MakeNewDateTime(Params.Result, Min)
  else
    HELP.MakeNewNumeric(Params.Result, Min);
end;

procedure TBOS_Average.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Sum: Double;
  i : Integer;
  List: TBoldList;
begin
  Sum := 0;
  List := XList(Params.values[0]);
  if not assigned(list) or (List.Count = 0) then
    HELP.MakeNewNumeric(Params.Result, 0)
  else
  begin
    for i := 0 to List.Count - 1 do
      if not TBoldAttribute(List[i]).IsNull then
      Sum := Sum + XNumeric(List[i]);
    HELP.MakeNewNumeric(Params.Result, Sum / List.Count);
  end;
end;

procedure TBOS_ForAll.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  (Params.Result.Value as TBABoolean).AsBoolean := XBoolean(Params.Result.value) and xBoolean(Params.values[0]);
end;

procedure TBOS_Exists.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  (Params.Result.Value as TBABoolean).AsBoolean := XBoolean(Params.Result.value) or XBoolean(Params.values[0]);
end;

procedure TBOS_includes.Evaluate(const Params: TBoldOclSymbolParameters);
var
  List: TBoldList;
  Element: TBoldElement;
  aObj: TBoldObject;
  i : Integer;
  temp: Boolean;
begin
  temp := False;
  List := XList(Params.values[0]);
  if Assigned(List) and (List.Count > 0) then begin
    Element := Params.Values[1];
    if List is TBoldObjectList then begin
      // Optimization for ObjectLists: check for locator.
      // No need to ensure objects for equality check
      aObj := nil;
      if Element is TBoldObject then begin
        aObj := TBoldObject(Element);
      end else if Element is TBoldObjectReference then begin
        aObj := TBoldObjectReference(Element).BoldObject;
      end;
      if Assigned(aObj) then begin
        temp := TBoldObjectList(List).LocatorInList(aObj.BoldObjectLocator);
      end;
    end else begin
      List.EnsureRange(0, List.Count - 1);
      for i := 0 to List.Count - 1 do begin
        if List[i].IsEqual(Element) then begin
          temp := True;
          Break;
        end;
      end;
    end;
  end;
  HELP.MakeNewBoolean(Params.Result, temp);
end;

procedure TBOS_union.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if not assigned(xlist(Params.values[0])) then
    Help.TransferOrClone(Params.nodes[1], Params.Result)
  else
  begin
    if assigned(Params.nodes[0].BoldType) and assigned(Params.nodes[1].BoldType) and
      not Params.nodes[1].BoldType.conformsto(Params.nodes[0].BoldType) then
    begin
      params.result.SetOwnedValue(help.CreateNewMember(params.result.BoldType));
      XList(Params.Result.value).AddList(XList(Params.values[0]));
    end else
      Help.TransferOrClone(Params.nodes[0], Params.Result);

    if assigned(xlist(Params.values[1])) then
      XList(Params.Result.value).AddList(XList(Params.values[1]));
  end;
end;

procedure TBOS_Intersection.Evaluate(const Params: TBoldOclSymbolParameters);
var
  i                  : Integer;
  resList, list1, list2: TBoldList;
aResListObject, aList1Object, aList2Object: TBoldObjectList;
begin
  list1 := XList(Params.values[0]);
  list2 := XList(Params.values[1]);
  if not assigned(List1) then
    Help.TransferOrClone(Params.nodes[1], Params.Result)
  else if not assigned(List2) then
    Help.TransferOrClone(Params.nodes[0], Params.Result)
  else
  begin
    if list2.Count < list1.Count then
    begin
      resList := list1;
      list1 := list2;
      list2 := resList;
    end;

    resList := help.CreateNewMember(Params.Result.BoldType) as TBoldList;

    // Optimization for ObjectLists
    // Thus, the objects no longer need to be fechted during OCL evaluation
    if resList is TBoldObjectList then begin
      aList1Object := TBoldObjectList(list1);
      aList2Object := TBoldObjectList(list2);
      aResListObject := TBoldObjectList(resList);
      for i := 0 to aList1Object.Count - 1 do
        if aList2Object.LocatorInList(aList1Object.Locators[i]) then
          aResListObject.AddLocator(aList1Object.Locators[i]);
    end else begin
      for i := 0 to list1.Count - 1 do
        if list2.Includes(list1[i]) then
          resList.Add(list1[i]);
    end;
    Params.Result.SetOwnedValue(resList);
  end;
end;

procedure TBOS_difference.Evaluate(const Params: TBoldOclSymbolParameters);
var
  i, p: Integer;
  list1, list2: TBoldList;
  aList1Object, aList2Object: TBoldObjectList;
begin
  if not assigned(XList(Params.values[0])) then
    Params.Result.SetReferenceValue(nil)
  else if not assigned(XList(Params.values[1])) then
    Help.TransferOrClone(Params.nodes[0], Params.Result)
  else
  begin
    HELP.TransferOrClone(Params.nodes[0], Params.Result);
    list2 := XList(Params.values[1]);
    list1 := XList(Params.Result.value);
    if (list1 is TBoldObjectList) and (list2 is TBoldObjectList) then begin
      aList1Object := TBoldObjectList(list1);
      aList2Object := TBoldObjectList(list2);
      for i := 0 to aList2Object.Count - 1 do
      begin
        p := aList1Object.IndexOfLocator(aList2Object.Locators[i]);
        if p <> -1 then
          aList1Object.RemoveByIndex(p);
      end;
    end else begin
      for i := 0 to list2.Count - 1 do
      begin
        p := list1.IndexOf(List2[i]);
        if p <> -1 then
          list1.RemoveByIndex(p);
      end;
    end;
  end;
end;

procedure TBOS_Including.Evaluate(const Params: TBoldOclSymbolParameters);
var
  ResList: TBoldList;
begin
  resList := help.CreateNewMember(Params.Result.BoldType) as TBoldList;
  Params.result.SetOwnedValue(ResList);
  resList.AddList(xlist(Params.values[0]));
  if assigned(Params.Values[1]) then
    ResList.Add(Params.Values[1]);
{
  HELP.TransferOrClone(Params.nodes[0], Params.Result);
  if assigned(xlist(Params.values[1])) then
    XList(Params.Result.value).Add(Params.values[1]);}
end;

procedure TBOS_excluding.Evaluate(const Params: TBoldOclSymbolParameters);
var
  OrgList: TBoldList;
begin
  HELP.TransferOrClone(Params.nodes[0], Params.Result);
  OrgList := xList(Params.Result.value);
  if assigned(OrgList) and OrgList.Includes(Params.values[1]) then
    orgList.Remove(Params.values[1]);
end;

procedure TBOS_SymmetricDifference.Evaluate(const Params: TBoldOclSymbolParameters);
var
  i                  : Integer;
  resList, list1, list2: TBoldList;
  aResListObject, aList1Object, aList2Object: TBoldObjectList;
begin
  list1 := XList(Params.values[0]);
  list2 := XList(Params.values[1]);
  if not assigned(list1) then
    HELP.TransferOrClone(Params.nodes[0], Params.Result)
  else if not assigned(List2) then
    HELP.TransferOrClone(Params.nodes[1], Params.Result)
  else
  begin
    resList := help.CreateNewMember(Params.Result.BoldType) as TBoldList;
    // Optimization for ObjectLists
    if resList is TBoldObjectList then begin
      aResListObject := TBoldObjectList(resList);
      aList1Object := TBoldObjectList(list1);
      aList2Object := TBoldObjectList(list2);

      for i := 0 to aList1Object.Count - 1 do
        if not aList2Object.LocatorInList(aList1Object.Locators[i]) then
            aResListObject.AddLocator(aList1Object.Locators[i]);

      for i := 0 to aList2Object.Count - 1 do
        if not aList1Object.LocatorInList(aList2Object.Locators[i]) then
            aResListObject.AddLocator(aList2Object.Locators[i]);
    end else begin
      for i := 0 to list1.Count - 1 do
        if not list2.Includes(list1[i]) then resList.Add(list1[i]);

      for i := 0 to list2.Count - 1 do
        if not list1.Includes(list2[i]) then resList.Add(list2[i]);
    end;
    Params.Result.SetOwnedValue(ResList);
  end;
end;

procedure TBOS__ListCopier.CopyListToResult(const Params: TBoldOclSymbolParameters);
var
  temp: TBoldIndirectElement;
begin
  if assigned(Params.values[0]) then
  begin
    temp := TBoldIndirectElement.Create;
    Params.values[0].GetAsList(temp);
    HELP.TransferOrClone(temp, Params.Result);
    temp.Free;
  end
  else
    params.result.SetOwnedValue(help.CreateNewMember(params.result.BoldType));
end;

procedure TBOS_AsSequence.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  CopyListToResult(params);

  if assigned(xlist(Params.Result.value)) then
    XList(Params.Result.value).DuplicateMode := bldmAllow;
end;

procedure TBOS_AsBag.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  CopyListToResult(params);

  if assigned(xlist(Params.Result.value)) then
    XList(Params.Result.value).DuplicateMode := bldmAllow;
end;

procedure TBOS_AsSet.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if not assigned(XList(Params.values[0])) then
    Params.Result.SetReferenceValue(nil)
  else
  if xlist(params.Values[0]).DuplicateMode = bldmAllow then
  begin
    Params.Result.SetOwnedValue(TBoldMemberFactory.CreateMemberFromBoldType(params.Values[0].BoldType));
    xlist(Params.Result.value).DuplicateMode := bldmMerge;
    xlist(Params.Result.value).AddList(XList(params.Values[0]));
  end
  else
    CopyListToResult(params);
end;

procedure TBOS_Append.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.TransferOrClone(Params.nodes[0], Params.Result);
  if assigned(xlist(Params.Result.value)) then
    XList(Params.Result.value).Add(Params.values[1]);
end;

procedure TBOS_Prepend.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.TransferOrClone(Params.nodes[0], Params.Result);
  if assigned(xlist(Params.Result.value)) then
    XList(Params.Result.value).Insert(0, Params.values[0]);
end;

procedure TBOS_SubSequence.Evaluate(const Params: TBoldOclSymbolParameters);
var
  i          : Integer;
  Start, Stop: Integer;
  resList, List: TBoldList;
  aResListObject, aListObject: TBoldObjectList;
begin
  List := XList(Params.values[0]);
  resList := help.CreateNewMember(Params.Result.BoldType) as TBoldList;
  Params.Result.SetOwnedValue(resList);
  if not assigned(List) then
    exit;

  Start := XInteger(Params.values[1])- 1;
  Stop := XInteger(Params.values[2])- 1;

  if Start < 0 then Start := 0;
  if Stop > List.Count - 1 then Stop := List.Count - 1;

  // Optimization for ObjectLists
  // Thus, the objects no longer need to be fechted during OCL evaluation
  if resList is TBoldObjectList then begin
    aListObject := TBoldObjectList(List);
    aResListObject := TBoldObjectList(resList);
    for i := Start to Stop do
      aResListObject.AddLocator(aListObject.Locators[i]);
  end else begin
    for i := Start to Stop do
      resList.Add(List[i]);
  end;
end;

procedure TBOS_at.Evaluate(const Params: TBoldOclSymbolParameters);
var
  List: TBoldList;
  P : Integer;
  EL: TBoldElement;
begin
  EL := nil;
  List := XList(Params.values[0]);
  if not assigned(list) then
    Params.Result.SetReferenceValue(nil)
  else
  begin
    P := XInteger(Params.values[1]) - 1;
    try
      if (P >= 0) and (P < List.Count) then begin
        EL := List[P];
      end;
    except
      on e:EListError do
        raise EBoldOclRunTimeError.CreateFmt(borteAtIndexOutOfBounds, [0, p, list.Count]);
    end;
    if Assigned(EL) then begin
      EL.GetAsValue(Params.Result);
    end else begin
      Params.Result.SetOwnedValue(nil);
    end;
  end;
end;

procedure TBOS_first.Evaluate(const Params: TBoldOclSymbolParameters);
var
  EL: TBoldElement;
  List: tBoldList;
begin
  List := XList(Params.values[0]);
  if not assigned(List) or (list.count = 0) then
    Params.Result.SetReferenceValue(nil)
  else
  begin
    EL := List[0];
    if assigned(el) then
      EL.GetAsValue(Params.Result)
    else
      Params.Result.SetReferenceValue(nil);
  end;
end;

procedure TBOS_last.Evaluate(const Params: TBoldOclSymbolParameters);
var
  EL: TBoldElement;
  List: tBoldList;
begin
  List := XList(Params.values[0]);
  if not assigned(List) or (list.count = 0) then
    Params.Result.SetReferenceValue(nil)
  else
  begin
    EL := List[List.Count - 1];
    if assigned(el) then
      EL.GetAsValue(Params.Result)
    else
      Params.Result.SetReferenceValue(nil);
  end;
end;

procedure TBOS_asString.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewString(Params.Result, XString(Params.values[0]));
  if assigned(Params.values[0]) then
    Params.values[0].SubscribeToStringRepresentation(brDefault, Params.subscriber, breReEvaluate);
end;

procedure TBOS_datePart.Evaluate(const Params: TBoldOclSymbolParameters);
var
  vMoment: TBAMoment;
begin
  vMoment := Params.values[0] as TBAMoment;
  if Assigned(vMoment) and not vMoment.IsNull then
    HELP.MakeNewDateTime(Params.Result, INT(XDateTime(Params.values[0])))
  else
    Params.Result.SetReferenceValue(nil);
end;

procedure TBOS_asDateTime.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if Assigned(Params.values[0]) then
    HELP.MakeNewDateTime(Params.Result, XDateTime(Params.values[0]))
  else
    Params.Result.SetReferenceValue(nil);
end;

procedure TBOS_StringRepresentation.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Rep: integer;
begin
  if assigned(Params.values[1]) then
    Rep := XInteger(Params.values[1])
  else
    rep := brDefault;

  if assigned(Params.values[0]) then
  begin
    HELP.MakeNewString(Params.Result, Params.values[0].StringRepresentation[rep]);
    Params.values[0].SubscribeToStringRepresentation(rep, Params.subscriber, breReEvaluate);
  end
  else
    HELP.MakeNewString(Params.Result, '');
end;

procedure TBOS_TaggedValue.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Obj: TBoldObject;
  Tag: string;
  value: string;
begin
  if Params.values[0] is TBoldObject then
  begin
    obj := Params.values[0] as TBoldObject;
    tag := XString(params.values[1]);
    value := obj.BoldClassTypeInfo.TaggedValues[Tag];
  end else
    value := '';

  HELP.MakeNewString(Params.Result, value);
end;


procedure TBOS_TypeName.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewString(Params.Result, XString(Params.values[0]));
end;

procedure TBOS_oclType.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if assigned(Params.values[0]) then
    Params.Result.SetReferenceValue(Params.values[0].BoldType)
  else
    Params.Result.SetReferenceValue(nil);
end;

procedure TBOS_OclAsType.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Parm1Type: TBoldElementTypeInfo;
begin
  Parm1Type :=  xtype(Params.values[1]);
  if not assigned(Parm1Type) then
    raise EBoldOclRunTimeError.CreateFmt(boeArgrtIsNottype, [0, '->oclAsType']);

  if not assigned(Params.values[0]) or Params.values[0].BoldType.ConformsTo(Parm1Type) then
    Params.Result.SetReferenceValue(Params.values[0])
  else
    raise EBoldOclRunTimeError.CreateFmt(borteInvalidCast, [0, Params.values[0].BoldType.AsString, Params.values[1].AsString]);
end;

procedure TBOS_SafeCast.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if not assigned(xtype(Params.values[1])) then
    raise EBoldOclRunTimeError.CreateFmt(boeArgrtIsNottype, [0, '.safeCast']);

  if not assigned(Params.values[0]) or Params.values[0].BoldType.ConformsTo(XType(Params.values[1])) then
    Params.Result.SetReferenceValue(Params.values[0])
  else
    Params.Result.SetReferenceValue(nil)
end;


procedure TBOS_oclIsKindOf.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Parm1Type: TBoldElementTypeInfo;
begin
  Parm1Type :=  xtype(Params.values[1]);
  if not assigned(Parm1Type) then
    raise EBoldOclRunTimeError.CreateFmt(boeArgrtIsNottype, [0, '->oclIsKindOf']);

  HELP.MakeNewBoolean(Params.Result, assigned(Params.values[0]) and Params.values[0].BoldType.ConformsTo(Parm1Type));
end;

procedure TBOS_OclIsTypeOf.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Parm1Type: TBoldElementTypeInfo;
begin
  Parm1Type :=  xtype(Params.values[1]);
  if not assigned(Parm1Type) then
    raise EBoldOclRunTimeError.CreateFmt(boeArgrtIsNottype, [0, '->oclIsTypeOf']);

  HELP.MakeNewBoolean(Params.Result, assigned(Params.values[0]) and (Params.values[0].BoldType = Parm1Type));
end;

procedure TBOS_AllInstances.Evaluate(const Params: TBoldOclSymbolParameters);
var
  ClassTypeInfo: TBoldClassTypeInfo;
  AttributeTypeInfo: TBoldAttributeTypeInfo;
  i: integer;
  StrList: TStringList;
  ValueSetlist: TBoldMemberList;
begin
  if Params.values[0] is TBoldClassTypeInfo then
  begin
    ClassTypeInfo := Params.values[0] as TBoldClassTypeInfo;

    if assigned(Params.System) then
      Params.Result.SetReferenceValue(Params.System.Classes[ClassTypeInfo.TopSortedIndex])
    else
      raise EBoldOclRunTimeError.Create(sUnableToGetAllInstances);

  end else if Params.values[0] is TBoldAttributeTypeInfo then
  begin
    AttributeTypeInfo := Params.values[0] as TBoldAttributeTypeInfo;

    if AttributeTypeInfo.ConformsTo(Params.SystemTypeInfo.ValueSetTypeInfo) then
    begin
      With Params.SystemTypeInfo do
      begin
        ValueSetlist := help.CreateNewMember(ListTypeInfoByElement[AttributeTypeInfo]) as TBoldMemberList;

        StrList := TStringList.Create;
        with help.CreateNewMember(AttributeTypeInfo) as TBAValueSet do
        begin
          Values.ToStrings(brDefault, StrList);
          Free;
        end;

        for i := 0 to StrList.Count - 1 do
          ValueSetlist.AddNew.StringRepresentation[brDefault] := StrList[i];
        StrList.Free;

        Params.Result.SetOwnedValue(ValueSetlist);
      end;
    end;
  end;
end;

procedure TBOS_AllLoadedObjects.Evaluate(const Params: TBoldOclSymbolParameters);
var
  ClassTypeInfo: TBoldClassTypeInfo;
  ClassList,
  Objectlist: TBoldObjectList;
  Traverser: TBoldLocatorListTraverser;
begin
  ClassTypeInfo := Params.values[0] as TBoldClassTypeInfo;

  if assigned(Params.System) then
  begin
    ClassList := Params.System.Classes[ClassTypeInfo.TopSortedIndex];

    if assigned(Params.Subscriber) then
      ClassList.AddSmallSubscription(params.subscriber, [beItemAdded, beItemDeleted, beObjectFetched, beObjectUnloaded], breReEvaluate);

    if ClassList.BoldPersistenceState <> bvpsCurrent then
    begin
      ObjectList := help.CreateNewMember(Params.Result.BoldType) as TBoldObjectList;
      ObjectList.DuplicateMode := bldmAllow;
      Params.Result.SetOwnedValue(ObjectList);
      Traverser := Params.System.Locators.CreateTraverser;
      while Traverser.MoveNext do
      with Traverser.Locator do
      begin
        if assigned(BoldObject) and Boldobject.BoldType.ConformsTo(ClassTypeInfo) then
          ObjectList.AddLocator(Traverser.Locator);
      end;
      Traverser.Free;
    end else
      Params.Result.SetReferenceValue(ClassList);
  end
  else
    raise EBoldOclRunTimeError.Create(sUnableToGetAllLoadedObjects);
end;

procedure TBOS_EmptyList.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Objectlist: TBoldObjectList;
begin
  ObjectList := help.CreateNewMember(Params.Result.BoldType) as TBoldObjectList;
  Params.Result.SetOwnedValue(ObjectList);
end;

procedure TBOS_Attributes.Evaluate(const Params: TBoldOclSymbolParameters);
var
  ClassInfo: tBoldClassTypeInfo;
  MemberRTInfo: TBoldMemberRTInfo;
  i: integer;
  Name: TBAString;
begin
  Params.Result.SetOwnedValue(help.CreateNewMember(Params.Result.BoldType));
  if Params.values[0] is TBoldClassTypeInfo then
  begin
    ClassInfo := Params.values[0] as TBoldClassTypeInfo;
    Name := help.CreateNewMember(Help.StringType) as TBAString;
    for i := 0 to ClassInfo.AllMembersCount - 1 do
    begin
      MemberRTInfo := ClassInfo.AllMembers[i];
      if MemberRTInfo.IsAttribute then
      begin
        Name.AsString := MemberRTInfo.ExpressionName;
        (Params.Result.Value as TBoldList).Add(Name);
      end;
    end;
    Name.free;
  end else
    raise EBoldOclRuntimeError.createFmt(borteNonClassAttribute, [0, Params.values[0].asString]);
end;

procedure TBOS_AssociationEnds.Evaluate(const Params: TBoldOclSymbolParameters);
var
  ClassInfo: tBoldClassTypeInfo;
  MemberRTInfo: TBoldMemberRTInfo;
  i: integer;
  Name: TBAString;
begin
  Params.Result.SetOwnedValue(help.CreateNewMember(Params.Result.BoldType));
  if Params.values[0] is TBoldClassTypeInfo then
  begin
    ClassInfo := Params.values[0] as TBoldClassTypeInfo;
    Name := help.CreateNewMember(Help.StringType) as TBAString;
    for i := 0 to ClassInfo.AllMembersCount - 1 do
    begin
      MemberRTInfo := ClassInfo.AllMembers[i];
      if MemberRTInfo.IsRole then
      begin
        Name.AsString := MemberRTInfo.ExpressionName;
        (Params.Result.Value as TBoldList).Add(Name);
      end;
    end;
    Name.free;
  end else
    raise EBoldOclRuntimeError.createFmt(borteNonClassAttribute, [0, Params.values[0].asString]);
end;

{
procedure TBOS_Operations.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  raise EBoldFeatureNotImplementedYet.CreateFmt('Operation %s is not yet implemented...', [ClassName]);
end;
}

procedure TBOS_SuperTypes.Evaluate(const Params: TBoldOclSymbolParameters);
var
  List: TBoldTypeList;
  ClassTypeInfo: TBoldClassTypeInfo;
  AttrTypeInfo: TBoldAttributeTypeInfo;
begin
  List := TBoldTypeListFactory.CreateList(params.result.BoldType);
  Params.Result.SetOwnedValue(list);
  if assigned(Params.values[0]) then
  begin
    case TBoldElementTypeInfo(Params.values[0]).BoldvalueType of
      bvtClass: begin
        ClassTypeInfo := Params.values[0] as TBoldClasstypeInfo;
        if assigned(ClasstypeINfo.SuperClassTypeInfo) then
          list.Add(ClasstypeInfo.SuperClassTypeInfo);
      end;
      bvtAttr: begin
        AttrTypeInfo := Params.values[0] as TBoldAttributeTypeInfo;
        if assigned(AttrTypeInfo.SuperAttributeTypeInfo) then
          list.Add(AttrTypeInfo.SuperAttributeTypeInfo);
      end;
    end;
  end;
end;

procedure TBOS_AllSuperTypes.Evaluate(const Params: TBoldOclSymbolParameters);
var
  List: TBoldTypeList;
  ClassTypeInfo: TBoldClassTypeInfo;
  AttrTypeInfo: TBoldAttributeTypeInfo;
begin
  List := TBoldTypeListFactory.CreateList(params.result.BoldType);
  Params.Result.SetOwnedValue(list);
  if assigned(Params.values[0]) then
  begin
    case TBoldElementTypeInfo(Params.values[0]).BoldvalueType of
      bvtClass: begin
        ClassTypeInfo := Params.values[0] as TBoldClasstypeInfo;
        while assigned(ClasstypeINfo.SuperClassTypeInfo) do
        begin
          list.Add(ClasstypeInfo.SuperClassTypeInfo);
          ClassTypeInfo := ClasstypeInfo.SuperClassTypeInfo;
        end;
      end;
      bvtAttr: begin
        AttrTypeInfo := Params.values[0] as TBoldAttributeTypeInfo;
        while assigned(AttrTypeInfo.SuperAttributeTypeInfo) do
        begin
          list.Add(AttrTypeInfo.SuperAttributeTypeInfo);
          AttrTypeInfo := AttrTypeInfo.SuperAttributeTypeInfo;
        end;
      end;
    end;
  end;
end;

procedure TBOS_AllSubClasses.Evaluate(const Params: TBoldOclSymbolParameters);
var
  List: TBoldTypeList;
  i: integer;
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  List := TBoldTypeListFactory.CreateList(params.result.BoldType);
  Params.Result.SetOwnedValue(list);
  if Params.values[0] is TBoldClasstypeInfo then
  begin
    ClassTypeInfo := Params.values[0] as TBoldClasstypeInfo;
    for i := 0 to Params.SystemTypeInfo.TopSortedClasses.Count - 1 do
      if Params.SystemTypeInfo.TopSortedClasses[i].ConformsTo(ClassTypeINfo) then
        list.Add(Params.SystemTypeInfo.TopSortedClasses[i]);
  end;
end;

function SQLLikeToRegEx(const AExpression: string): string;
var
  l: integer;
  Starts, Ends: boolean;
begin
  l := Length(AExpression);
  if l > 1 then
  begin
    Starts := Copy(AExpression, 1, 1) = '%';
    Ends := Copy(AExpression, l, 1) = '%';
    if Starts and Ends then
      result := '^.*' + Copy(AExpression, 2, l-2) + '.*$'
    else
    if Starts then
      result := '^.*' + Copy(AExpression, 2, MaxInt) + '$'
    else
    if Ends then
      result := '^' + Copy(AExpression, 1, l-1) + '.*$'
    else
      result := '^' + Copy(AExpression, 1, l) + '$'
  end
  else
  if AExpression = '%' then
    result := '.*'
  else
    result := '^' + AExpression + '$'
end;

function EscapeRegEx(const ASource: string): string;
begin
  result := TPerlRegEx.EscapeRegExChars(ASource);
  result := SQLLikeToRegEx(result);
end;

procedure TBOS_SQLLike.Evaluate(const Params: TBoldOclSymbolParameters);
var
  s: string;
begin
  s := XString(Params.values[1]);
  s := EscapeRegEx(s);
  if s = '' then
    Help.MakeNewBoolean(Params.Result, false)
  else
    Help.MakeNewBoolean(Params.Result, TRegEx.IsMatch(XString(Params.values[0]), s));
end;

procedure TBOS_SQLLikeCaseInSensitive.Evaluate(const Params: TBoldOclSymbolParameters);
var
  s: string;
begin
  s := XString(Params.values[1]);
  s := EscapeRegEx(s);
  if s = '' then
    Help.MakeNewBoolean(Params.Result, false)
  else
    Help.MakeNewBoolean(Params.Result, TRegEx.IsMatch(XString(Params.values[0]), s, [roIgnoreCase]));
end;

procedure TBOS_RegExpMatch.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  Help.MakeNewBoolean(Params.Result, TRegEx.IsMatch(XString(Params.values[0]), XString(Params.values[1]), []));
end;

procedure TBOS_InDateRange.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Moment: TBAMoment;
  Start, Stop, val: TDateTime;
begin
  Moment := Params.values[0] as TBAMoment;
  if not Assigned(Moment) or (Moment.IsNull or (Params.values[1] as TBoldAttribute).IsNull or (Params.values[2] as TBoldAttribute).IsNull) then
  begin
    help.MakeNewBoolean(params.result, false);
  end
  else
  begin
    Val := ExtractDateTimeFromMoment(Moment);

    val := trunc(val);

    Start := trunc((Params.values[1] as TBANumeric).AsFloat);
    Stop := trunc((Params.values[2] as TBANumeric).AsFloat);
    Help.MakeNewBoolean(Params.Result, (val >= start) and (val <= stop));
  end;
end;

procedure TBOS_InTimeRange.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Moment: TBAMoment;
  Start, Stop, val: TDateTime;
begin
  Moment := Params.values[0] as TBAMoment;
  if not Assigned(Moment) or (moment.IsNull or (Params.values[1] as TBoldAttribute).IsNull or (Params.values[2] as TBoldAttribute).IsNull) then
  begin
    help.MakeNewBoolean(params.result, false);
  end
  else
  begin
    Val := ExtractDateTimeFromMoment(Moment);

    Val := Frac(val);

    Start := frac((Params.values[1] as TBANumeric).AsFloat);
    Stop := frac((Params.values[2] as TBANumeric).AsFloat);
    Help.MakeNewBoolean(Params.Result, (CompareTime(val,Start)>=0) and (CompareTime(val,stop)<=0));
  end;
end;

procedure TBOS_Constraints.Evaluate(const Params: TBoldOclSymbolParameters);
var
  ResList: TBoldMemberList;

procedure AddConstraint(IncomingConstraint: TBoldConstraintRTInfo; element: TBoldElement);
begin
  with ResList.AddNew as TBAConstraint do
    InitializeConstraint(IncomingConstraint, element);
end;

var
  ClassTypeInfo: TBoldClassTypeINfo;
  i: integer;
  Member: TBoldMember;
begin
  ResList := help.CreateNewMember(params.Result.BoldType) as TBoldMemberList;
  params.Result.SetOwnedValue(ResList);

  if Params.values[0] is TBoldMember then
  begin
    Member := Params.values[0] as TBoldMember;
      for i := 0 to Member.BoldMemberRTinfo.ConstraintCount - 1 do
        AddConstraint(Member.BoldMemberRTinfo.ConstraintByIndex[i], member)
  end
  else if Params.values[0] is TBoldObject then
  begin
    ClassTypeInfo := (Params.values[0] as TBoldObject).BoldClassTypeInfo;
    while assigned(ClassTypeInfo) do
    begin
      for i := 0 to ClassTypeInfo.ConstraintCount - 1 do
        AddConstraint(ClassTypeInfo.ConstraintByIndex[i], Params.values[0]);
      ClassTypeInfo := ClassTypeInfo.SuperClassTypeInfo;
    end;
  end
  else if Params.values[0] is TBoldSystem then
  begin
    with Params.values[0] as TBoldSystem do
    begin
      for i := 0 to BoldSystemTypeInfo.ConstraintCount - 1 do
        AddConstraint(BoldSystemTypeInfo.ConstraintByIndex[i], Params.values[0]);
    end;
  end;
end;

procedure TBOS_AtTime.Evaluate(const Params: TBoldOclSymbolParameters);
var
  OldObject: TBoldObject;
  NewTime: Integer;
begin
  OldObject := Params.values[0] as TBoldObject;
  if (Params.values[1] as TBAInteger).IsNull then
    NewTime := maxInt
  else
  NewTime := (Params.values[1] as TBAInteger).AsInteger;
  Params.Result.SetReferenceValue(OldObject.AtTime(NewTime));
end;

procedure TBOS_ObjectTimeStamp.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Obj: TBoldObject;
begin
  Obj := Params.values[0] as TBoldObject;
  Help.MakeNewInteger(Params.Result, Obj.BoldTimeStamp);
  Obj.AddSubscription(Params.Subscriber, beObjectTimestampChanged, breReEvaluate);
end;

procedure TBOS_ObjectTime.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Obj: TBoldObject;
begin
  Obj := Params.values[0] as TBoldObject;
  Help.MakeNewInteger(Params.Result, Obj.BoldObjectLocator.BoldObjectId.TimeStamp);
end;

procedure TBOS_AllInstancesAtTime.Evaluate(const Params: TBoldOclSymbolParameters);
var
  NewTime: Integer;
  ClassTypeInfo: TBoldClassTypeInfo;
begin
  NewTime := (Params.values[1] as TBAInteger).AsInteger;
  if Params.values[0] is TBoldClassTypeInfo then
  begin
    ClassTypeInfo := Params.values[0] as TBoldClassTypeInfo;

    if assigned(Params.System) then
      Params.Result.SetReferenceValue(Params.System.Classes[ClassTypeInfo.TopSortedIndex].atTime(NewTime))
    else
      raise EBoldOclRunTimeError.Create(sUnableToGetAllInstances);
  end
  else
    raise EBoldOclRunTimeError.Create(sAllInstancesAtTimeOnlyAllowedOnClasses);
end;

procedure TBOS_Existing.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  Help.MakeNewBoolean(Params.Result, (Params.values[0] as TBoldObject).BoldExistenceState = besExisting);
end;

procedure TBOS_FilterOnType.Evaluate(const Params: TBoldOclSymbolParameters);
var
  IncomingList: TBoldList;
  i: integer;
  OutList: TBoldList;
  aIncommingListObject,
  aOutListObject: TBoldObjectList;
  iTopSortedIndex: Integer;
  Locator: TBoldObjectLocator;
  FilterType: TBoldElementTypeInfo;
begin
  IncomingList := XList(Params.values[0]);
  OutList := TBoldMemberFactory.CreateMemberFromBoldType(params.Result.BoldType) as TBoldList;
  FilterType := Params.values[1] as TBoldElementTypeInfo;
  if Assigned(IncomingList) then
  begin
    // Optimization for ObjectLists
    if OutList is TBoldObjectList then begin
      aIncommingListObject := TBoldObjectList(IncomingList);
      aOutListObject := TBoldObjectList(OutList);
      iTopSortedIndex := -1;
      aIncommingListObject.EnsureObjects;
      for i := 0 to aIncommingListObject.Count - 1 do begin
        Locator := aIncommingListObject.Locators[i];
        if Locator.BoldObjectID.TopSortedIndexExact then
        begin
          if (iTopSortedIndex = Locator.BoldObjectID.TopSortedIndex) or (Locator.BoldClassTypeInfo.ConformsTo(FilterType)) then
          begin
            aOutListObject.AddLocator(Locator);
            iTopSortedIndex := Locator.BoldObjectID.TopSortedIndex; // this is to skip ConformsTo with objects of repeated type
          end;
        end
        else
          if Locator.EnsuredBoldObject.BoldType.ConformsTo(FilterType) then
            aOutListObject.AddLocator(Locator);
      end;
    end else begin
      //Prefetch
      if Assigned(IncomingList) then begin
        IncomingList.EnsureRange(0, IncomingList.Count - 1);
        for i := 0 to IncomingList.Count - 1 do begin
          if IncomingList[i].BoldType.ConformsTo(FilterType) then
          begin
            OutList.Add(IncomingList[i]);
          end;
        end;
      end;
    end;
  end;
  params.result.SetOwnedValue(OutList);
end;

procedure TBOS_BoldTime.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  help.MakeNewInteger(Params.Result, (Params.values[0] as TBoldObject).BoldTime);
end;

procedure TBOS_TimeStampToTime.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  help.MakeNewDateTime(Params.Result, Params.System.TimeForTimestamp[(Params.values[0] as TBAInteger).AsInteger]);
end;

procedure TBOS_TimeToTimeStamp.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  help.MakeNewInteger(Params.Result, Params.System.TimestampForTime[(Params.values[0] as TBAdateTime).AsDateTime]);
end;

procedure TBOS_indexOf.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Index: Integer;
  List: TBoldList;
  aObj: TBoldObject;
  i: Integer;
begin
  Index := -1;
  List := XList(Params.values[0]);
  if List is TBoldObjectList then begin
    // Optimization for ObjectLists: check for locator.
    // No need to ensure objects for equality check
    aObj := nil;
    if Params.values[1] is TBoldObject then begin
      aObj := TBoldObject(Params.values[1]);
    end else if Params.values[1] is TBoldObjectReference then begin
      aObj := TBoldObjectReference(Params.values[1]).BoldObject;
    end;
    if Assigned(aObj) then begin
      Index := TBoldObjectList(List).IndexOfLocator(aObj.BoldObjectLocator);
    end;
  end else if Assigned(List) then begin
    List.EnsureRange(0, List.Count - 1);
    for i := 0 to List.Count - 1 do begin
      if List[i].IsEqual(Params.values[1]) then begin
        Index := i;
        Break;
      end;
    end;
  end;
  HELP.MakeNewInteger(Params.Result, Index);
end;

procedure TBOS_reverseCollection.Evaluate(const Params: TBoldOclSymbolParameters);
var
  List, resList: TBoldLIst;
  i: Integer;
begin
  List := XList(Params.values[0]);
  resList := help.CreateNewMember(Params.Result.BoldType) as TBoldList;
  resList.Capacity := List.Count;
  if List is TBoldObjectList then begin
    // Optimization for ObjectLists: check for locator.
    // No need to ensure objects for equality check
    for i := List.Count - 1 downto 0 do begin
      TBoldObjectList(resList).AddLocator(TBoldObjectList(List).Locators[i]);
    end;
  end else if Assigned(List) then begin
    for i := List.Count - 1 downto 0 do begin
      resList.Add(List[i]);
    end;
  end;

  Params.Result.SetOwnedValue(resList);
end;

procedure InitializeSymbolTable(SymTab: TBoldSymbolDictionary);
var
  i: integer;
begin
  SymTab.Capacity := SymTab.Count + OCLOperations.Count;
  for i := 0 to OCLOperations.Count - 1 do
    SymTab.Add(TBoldOclSymbolClass(OCLOperations[i]).Create(SymTab.Help));
end;

{ TBOS_CommaText }

procedure TBOS_asCommaText.Init;
begin
  InternalInit('asCommaText', [HELP.{String}ListType], tbodNo, HELP.StringType, True, 193);
end;

procedure TBOS_asCommaText.Evaluate(const Params: TBoldOclSymbolParameters);
var
  List : TBoldList;
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    List := XList(Params.values[0]);
    List.EnsureRange();
    for i := 0 to List.Count - 1 do
      sl.Add(XString(List[i]));
    HELP.MakeNewString(Params.Result, sl.CommaText);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TBOS_separate.Init;
begin
  InternalInit('separate', [HELP.StringListType, HELP.StringType], tbodNo, HELP.StringType, True, 194);
end;

procedure TBOS_separate.Evaluate(const Params: TBoldOclSymbolParameters);
var
  List : TBoldList;
  ResultString: string;
  Separator: string;
  i: Integer;
begin
  ResultString := '';
  List := XList(Params.values[0]);
  Separator := XString(Params.values[1]);
  for i := 0 to List.Count - 1 do
  begin
    if i > 0 then
      ResultString := ResultString + Separator;
    ResultString := ResultString + XString(List[i]);
  end;
  HELP.MakeNewString(Params.Result, ResultString);
end;

{ TBOS_CommaSeparatedStringToCollection }

procedure TBOS_CommaSeparatedStringToCollection.Evaluate(
  const Params: TBoldOclSymbolParameters);
var
  lStringList: TStringList;
  lBoldMember: TBoldMember;
  lBoldMemberList: TBoldMemberList;
  lIndexStringList: integer;
  lElementTypeInfo : TBoldElementTypeInfo;
begin
  lStringList := TStringList.Create;
  try
    lStringList.StrictDelimiter := true;
    lStringList.CommaText := (Params.Values[0].AsString);
    lBoldMemberList := Help.CreateNewMember(GetListTypeInfo) as TBoldMemberList;
    lElementTypeInfo := GetListTypeInfo.ListElementTypeInfo;
    for lIndexStringList := 0 to lStringList.Count - 1 do
    begin
      lBoldMember := Help.CreateNewMember(lElementTypeInfo) as TBoldMember;
      lBoldMember.AsString := lStringList[lIndexStringList];
      lBoldMemberList.Add(lBoldMember);
      lBoldMember.free;
    end;
    Params.Result.SetOwnedValue(lBoldMemberList);
  finally
    lStringList.free;
  end;
end;

{ TBOS_CommaSeparatedStringToStringCollection }

procedure TBOS_CommaSeparatedStringToStringCollection.Init;
begin
  InternalInit('toStringCollection', [Help.StringType], tbodNo, GetListTypeInfo, True, 0);
end;

function TBOS_CommaSeparatedStringToStringCollection.GetListTypeInfo: TBoldListTypeInfo;
begin
  result := Help.StringListType;
end;

{ TBOS_CommaSeparatedStringToIntegerCollection }

procedure TBOS_CommaSeparatedStringToIntegerCollection.Init;
begin
  InternalInit('toIntegerCollection', [Help.StringType], tbodNo, GetListTypeInfo, True, 0);
end;


function TBOS_CommaSeparatedStringToIntegerCollection.GetListTypeInfo: TBoldListTypeInfo;
begin
  result := Help.IntegerListType;
end;

{ TBOS_intAsFloat }

procedure TBOS_AsFloat.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewNumeric(Params.Result, XNumeric(Params.values[0]));
end;

procedure TBOS_AsFloat.Init;
begin
  InternalInit('asFloat', [HELP.NumericType], tbodNo, HELP.RealType, True, 163);
end;

{ TBOS_FloatAsDateTime }

procedure TBOS_FloatAsDateTime.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewDateTime(Params.Result, XNumeric(Params.Values[0]));
end;

procedure TBOS_FloatAsDateTime.Init;
begin
  InternalInit('floatAsDateTime', [HELP.NumericType], tbodNo, HELP.DateTimeType, True, 163);
end;

{ TBOS_NullValue }

procedure TBOS_NullValue.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewNull(Params.Result, XType(Params.values[0]));
end;

procedure TBOS_NullValue.Init;
begin
  InternalInit('nullValue', [HELP.TypeType], tbodArg1Type, nil, True, 170);
end;

{ TBOS_DayOfDate }

procedure TBOS_DayOfDate.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Moment: TBAMoment;
  LYear, LMonth, LDay: Word;
begin
    Moment := Params.values[0] as TBAMoment;
    if not Assigned(Moment) then
      Params.Result.SetReferenceValue(nil)
    else
    begin
      if Moment.IsNull then
        LYear := 0
      else
        DecodeDate(ExtractDateTimeFromMoment(Moment), LYear, LMonth, LDay);
      HELP.MakeNewInteger(Params.Result, LDay);
    end;
end;

procedure TBOS_DayOfDate.Init;
begin
  InternalInit('day', [HELP.MomentType], tbodNo, HELP.IntegerType, True, 124);

end;

procedure TBOS_MonthOfDate.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Moment: TBAMoment;
  LYear, LMonth, LDay: Word;
begin
    Moment := Params.values[0] as TBAMoment;
    if not Assigned(Moment) then
      Params.Result.SetReferenceValue(nil)
    else
    begin
      if Moment.IsNull then
        LMonth := 0
      else
        DecodeDate(ExtractDateTimeFromMoment(Moment), LYear, LMonth, LDay);
      HELP.MakeNewInteger(Params.Result, LMonth);
    end;
end;

procedure TBOS_MonthOfDate.Init;
begin
  InternalInit('month', [HELP.MomentType], tbodNo, HELP.IntegerType, True, 124);
end;

procedure TBOS_YearOfDate.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Moment: TBAMoment;
  LYear, LMonth, LDay: Word;
begin
    Moment := Params.values[0] as TBAMoment;
    if not Assigned(Moment) then
      Params.Result.SetReferenceValue(nil)
    else
    begin
      if Moment.IsNull then
        LYear := 0
      else
        DecodeDate(ExtractDateTimeFromMoment(Moment), LYear, LMonth, LDay);
      HELP.MakeNewInteger(Params.Result, LYear);
    end;
end;

procedure TBOS_YearOfDate.Init;
begin
  InternalInit('year', [HELP.MomentType], tbodNo, HELP.IntegerType, True, 124);
end;

procedure TBOS_WeekOfDate.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Moment: TBAMoment;
  LYear, LWeek, LDOW: Word;
begin
    Moment := Params.values[0] as TBAMoment;
    if not Assigned(Moment) then
      Params.Result.SetReferenceValue(nil)
    else
    begin
      if Moment.IsNull then
        LWeek := 0
      else
        DecodeDateWeek(ExtractDateTimeFromMoment(Moment), LYear, LWeek, LDOW);
      HELP.MakeNewInteger(Params.Result, LWeek);
    end;
end;

procedure TBOS_WeekOfDate.Init;
begin
  InternalInit('week', [HELP.MomentType], tbodNo, HELP.IntegerType, True, 124);
end;

procedure TBOS_DayOfWeekOfDate.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Moment: TBAMoment;
  LYear, LWeek, LDOW: Word;
begin
    Moment := Params.values[0] as TBAMoment;
    if not Assigned(Moment) then
      Params.Result.SetReferenceValue(nil)
    else
    begin
      if Moment.IsNull then
        LDOW := 0
      else
        DecodeDateWeek(ExtractDateTimeFromMoment(Moment), LYear, LWeek, LDOW);
      HELP.MakeNewInteger(Params.Result, LDOW);
    end;
end;

procedure TBOS_DayOfWeekOfDate.Init;
begin
  InternalInit('dayOfWeek', [HELP.MomentType], tbodNo, HELP.IntegerType, True, 124);
end;

{ TBOS_Trim }

procedure TBOS_Trim.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  Help.MakeNewString(Params.Result, StringReplace(Trim(XString(Params.Values[0])), BOLDCRLF, ' ', [rfReplaceAll]));
end;

procedure TBOS_Trim.Init;
begin
  InternalInit('trim', [Help.StringType], tbodNo, Help.StringType, True, 124);
end;

{ TBOS_HasDuplicates }

procedure TBOS_HasDuplicates.Evaluate(const Params: TBoldOclSymbolParameters);
var
  list: TBoldLIst;
begin
  List := XList(Params.values[0]);
  if not assigned(list) then
    Help.MakeNewBoolean(Params.Result, false)
  else
    HELP.MakeNewBoolean(Params.Result, List.HasDuplicates);
end;

{ TBOS_BoldId }

procedure TBOS_BoldId.Evaluate(const Params: TBoldOclSymbolParameters);
var
  S: String;
  Locator: TBoldObjectLocator;
begin
  if assigned(Params.Values[0]) then
  begin
    Locator := (Params.Values[0] as TBoldObject).BoldObjectLocator;
    s := Locator.BoldObjectID.AsString;
    if not Locator.BoldObjectID.IsStorable then
       Locator.BoldObject.AddSmallSubscription(Params.subscriber, [bePostUpdateID ], breResubscribe);
    Help.MakeNewInteger(Params.Result,StrToInt(s));
  end
  else
    HELP.MakeNewNull(Params.Result, Params.Result.BoldType)
end;

{ TBOS_SecondsBetween }

procedure TBOS_SecondsBetween.Evaluate(
  const Params: TBoldOclSymbolParameters);
var
  i:integer;
begin
  if (Params.values[0] = nil) or (Params.values[0] as TBoldAttribute).IsNull
  or (Params.values[1] = nil) or (Params.values[1] as TBoldAttribute).IsNull then
    Params.Result.SetReferenceValue(nil)
  else
  begin
  i := SecondsBetween(XDateTime(Params.Values[0]),XDateTime(Params.Values[1]));
  Help.MakeNewInteger(Params.Result,i);
  end;
end;

procedure TBOS_SecondsBetween.Init;
begin
  InternalInit('secondsBetween',[Help.MomentType,Help.MomentType],tbodNo,Help.IntegerType,True,0);
end;

function HoursBetween(const ANow, AThen: TDateTime): Int64;
begin
  Result := Round(HourSpan(ANow, AThen));
  if ANow < AThen then
    Result := -Result;
end;

function MinutesBetween(const ANow, AThen: TDateTime): Int64;
begin
  Result := Round(MinuteSpan(ANow, AThen));
  if ANow < AThen then
    Result := -Result;
end;

function SecondsBetween(const ANow, AThen: TDateTime): Int64;
begin
  Result := Round(SecondSpan(ANow, AThen));
  if ANow < AThen then
    Result := -Result;
end;

{ TBOS_MinutesBetween }

procedure TBOS_MinutesBetween.Evaluate(
  const Params: TBoldOclSymbolParameters);
var
  i:integer;
begin
  if (Params.values[0] = nil) or (Params.values[0] as TBoldAttribute).IsNull
  or (Params.values[1] = nil) or (Params.values[1] as TBoldAttribute).IsNull then
    Params.Result.SetReferenceValue(nil)
  else
  begin
  i := MinutesBetween(XDateTime(Params.Values[0]),XDateTime(Params.Values[1]));
  Help.MakeNewInteger(Params.Result,i);
  end;
end;

procedure TBOS_MinutesBetween.Init;
begin
  InternalInit('minutesBetween',[Help.MomentType,Help.MomentType],tbodNo,Help.IntegerType,True,0);
end;

{ TBOS_HoursBetween }

procedure TBOS_HoursBetween.Evaluate(
  const Params: TBoldOclSymbolParameters);
var
  i:integer;
begin
  if (Params.values[0] = nil) or (Params.values[0] as TBoldAttribute).IsNull
  or (Params.values[1] = nil) or (Params.values[1] as TBoldAttribute).IsNull then
    Params.Result.SetReferenceValue(nil)
  else
  begin
  i := HoursBetween(XDateTime(Params.Values[0]),XDateTime(Params.Values[1]));
  Help.MakeNewInteger(Params.Result,i);
  end;
end;

procedure TBOS_HoursBetween.Init;
begin
  InternalInit('hoursBetween',[Help.MomentType, Help.MomentType],tbodNo,Help.IntegerType,True,0);
end;

{ TBOS_Power }

procedure TBOS_Power.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if (Params.values[0] = nil) or (Params.values[1] = nil) then
    Params.Result.SetReferenceValue(nil)
  else
    Help.MakeNewNumeric(Params.Result, Power(XNumeric(Params.values[0]), XNumeric(Params.values[1])));
end;

{ TBOS_Sqrt }

procedure TBOS_Sqrt.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if (Params.values[0] = nil) then
    Params.Result.SetReferenceValue(nil)
  else
    Help.MakeNewNumeric(Params.Result, Sqrt(XNumeric(Params.values[0])));
end;

{ TBOS_BoldIDIn }

procedure TBOS_BoldIDIn.Evaluate(const Params: TBoldOclSymbolParameters);
var
  sIDList: string;
  aObj: TBoldObject;
  sBoldID: string;
  bResult: Boolean;
  aIDList: TStringList;
  i: Integer;
begin
  sIDList := XString(Params.values[1]);

  sBoldID := '-1';
  if Params.values[0] is TBoldObject then begin
    aObj := TBoldObject(Params.values[0]);
    if Assigned(aObj.BoldObjectLocator) then begin
      sBoldID := aObj.BoldObjectLocator.AsString;
    end;
  end;

  bResult := False;
  if sBoldID <> '-1' then begin
    aIDList := TStringList.Create;
    try
      aIDList.Delimiter := ',';
      aIDList.DelimitedText := sIDList;
      for i := 0 to aIDList.Count - 1 do begin
        if sBoldID = Trim(aIDList[i]) then begin
          bResult := True;
          Break;
        end;
      end;
    finally
      aIDList.Free;
    end;
  end;

  HELP.MakeNewBoolean(Params.Result, bResult);

end;

function TBOS_BoldIDIn.GetShortCircuitType: ShortCircuitType;
begin
  Result := csBoldIDIn;
end;

procedure TBOS_BoldIDIn.Init;
begin
  InternalInit('boldIDIn', [Help.ObjectType, Help.StringType], tbodNo, HELP.BooleanType, True, 274);//, 'Returns true if the object has one of the given ids.'); // do not localize
end;

{ TBOS_NamedIdIn }

procedure TBOS_NamedIdIn.Evaluate(const Params: TBoldOclSymbolParameters);
var
  sIDList: string;
  aObj: TBoldObject;
  sNamedIDValue: string;
  bResult: Boolean;
  aIDList: TStringList;
  i: Integer;
begin
  sIDList := XString(Params.values[2]);

  sNamedIDValue := '-1';
  if Params.values[0] is TBoldObject then begin
    aObj := TBoldObject(Params.values[0]);
    sNamedIDValue := aObj.EvaluateExpressionAsString(XString(Params.Values[1]));
  end;

  bResult := False;
  if sNamedIDValue <> '-1' then begin
    aIDList := TStringList.Create;
    try
      aIDList.Delimiter := ',';
      aIDList.DelimitedText := sIDList;
      for i := 0 to aIDList.Count - 1 do begin
        if sNamedIDValue = Trim(aIDList[i]) then begin
          bResult := True;
          Break;
        end;
      end;
    finally
      aIDList.Free;
    end;
  end;

  HELP.MakeNewBoolean(Params.Result, bResult);

end;

function TBOS_NamedIdIn.GetShortCircuitType: ShortCircuitType;
begin
  Result := csNamedIDIn;
end;

procedure TBOS_NamedIdIn.Init;
begin
  InternalInit('namedIDIn', [Help.ObjectType, Help.StringType, Help.StringType], tbodNo, HELP.BooleanType, True, 275);//, 'Returns true if the object has one of the given ids.'); // do not localize
end;

initialization
  RegisterOclOperation(TBOS_Equal);
  RegisterOclOperation(TBOS_NotEqual);
  RegisterOclOperation(TBOS_Add);
  RegisterOclOperation(TBOS_Subtract);
  RegisterOclOperation(TBOS_UnaryMinus);
  RegisterOclOperation(TBOS_Multiply);
  RegisterOclOperation(TBOS_Divide);
  RegisterOclOperation(TBOS_SafeDivZero);
  RegisterOclOperation(TBOS_Abs);
  RegisterOclOperation(TBOS_Floor);
  RegisterOclOperation(TBOS_Round);
  RegisterOclOperation(TBOS_SimpleRound);
  RegisterOclOperation(TBOS_SimpleRoundTo);
  RegisterOclOperation(TBOS_strToInt);
  RegisterOclOperation(TBOS_strToFloat);
  RegisterOclOperation(TBOS_Min);
  RegisterOclOperation(TBOS_Max);
  RegisterOclOperation(TBOS_Less);
  RegisterOclOperation(TBOS_Greater);
  RegisterOclOperation(TBOS_LessEQ);
  RegisterOclOperation(TBOS_GreaterEQ);
  RegisterOclOperation(TBOS_Div);
  RegisterOclOperation(TBOS_Mod);
  RegisterOclOperation(TBOS_Length);
  RegisterOclOperation(TBOS_concat);
  RegisterOclOperation(TBOS_ToUpper);
  RegisterOclOperation(TBOS_toLower);
  RegisterOclOperation(TBOS_SubString);
  RegisterOclOperation(TBOS_Contains);
  RegisterOclOperation(TBOS_Pad);
  RegisterOclOperation(TBOS_PostPad);
  RegisterOclOperation(TBOS_or);
  RegisterOclOperation(TBOS_and);
  RegisterOclOperation(TBOS_not);
  RegisterOclOperation(TBOS_xor);
  RegisterOclOperation(TBOS_implies);
  RegisterOclOperation(TBOS_if);
  RegisterOclOperation(TBOS_Size);
  RegisterOclOperation(TBOS_includes);
  RegisterOclOperation(TBOS_Count);
  RegisterOclOperation(TBOS_IncludesAll);
  RegisterOclOperation(TBOS_isEmpty);
  RegisterOclOperation(TBOS_isNull);
  RegisterOclOperation(TBOS_NotEmpty);
  RegisterOclOperation(TBOS_Sum);
  RegisterOclOperation(TBOS_Maxvalue);
  RegisterOclOperation(TBOS_MinValue);
  RegisterOclOperation(TBOS_Average);
  RegisterOclOperation(TBOS_Exists);
  RegisterOclOperation(TBOS_ForAll);
  RegisterOclOperation(TBOS_union);
  RegisterOclOperation(TBOS_Intersection);
  RegisterOclOperation(TBOS_difference);
  RegisterOclOperation(TBOS_Including);
  RegisterOclOperation(TBOS_excluding);
  RegisterOclOperation(TBOS_SymmetricDifference);
  RegisterOclOperation(TBOS_Select);
  RegisterOclOperation(TBOS_reject);
  RegisterOclOperation(TBOS_collect);
  RegisterOclOperation(TBOS_AsSequence);
  RegisterOclOperation(TBOS_AsBag);
  RegisterOclOperation(TBOS_AsSet);
  RegisterOclOperation(TBOS_Append);
  RegisterOclOperation(TBOS_Prepend);
  RegisterOclOperation(TBOS_SubSequence);
  RegisterOclOperation(TBOS_at);
  RegisterOclOperation(TBOS_first);
  RegisterOclOperation(TBOS_last);
  RegisterOclOperation(TBOS_orderby);
  RegisterOclOperation(TBOS_orderDescending);
  RegisterOclOperation(TBOS_asString);
  RegisterOclOperation(TBOS_AsFloat);
  RegisterOclOperation(TBOS_FloatAsDateTime);
  RegisterOclOperation(TBOS_datePart);
  RegisterOclOperation(TBOS_asDateTime);
  RegisterOclOperation(TBOS_stringRepresentation);
  RegisterOclOperation(TBOS_TaggedValue);
  RegisterOclOperation(TBOS_TypeName);
  RegisterOclOperation(TBOS_Attributes);
  RegisterOclOperation(TBOS_AssociationEnds);
  RegisterOclOperation(TBOS_NullValue);
{
  RegisterOclOperation(TBOS_Operations);
}
  RegisterOclOperation(TBOS_SuperTypes);
  RegisterOclOperation(TBOS_AllSuperTypes);
  RegisterOclOperation(TBOS_AllSubClasses);

  RegisterOclOperation(TBOS_AllInstances);
  RegisterOclOperation(TBOS_AllLoadedObjects);
  RegisterOclOperation(TBOS_EmptyList);
  RegisterOclOperation(TBOS_oclType);
  RegisterOclOperation(TBOS_oclIsKindOf);
  RegisterOclOperation(TBOS_OclIsTypeOf);
  RegisterOclOperation(TBOS_OclAsType);
  RegisterOclOperation(TBOS_SafeCast);
  RegisterOclOperation(TBOS_SQLLike);
  RegisterOclOperation(TBOS_SQLLikeCaseInsensitive);
  RegisterOclOperation(TBOS_RegExpMatch);
  RegisterOclOperation(TBOS_InDateRange);
  RegisterOclOperation(TBOS_InTimeRange);
  RegisterOclOperation(TBOS_Constraints);
  RegisterOclOperation(TBOS_Format);
  RegisterOclOperation(TBOS_FormatNumeric);
  RegisterOclOperation(TBOS_FormatFloat);
  RegisterOclOperation(TBOS_FormatDateTime);
  RegisterOclOperation(TBOS_AsISODate);
  RegisterOclOperation(TBOS_AsISODateTime);
  RegisterOclOperation(TBOS_StrToDateTime);
  RegisterOclOperation(TBOS_StrToDate);
  RegisterOclOperation(TBOS_StrToTime);
  RegisterOclOperation(TBOS_AtTime);
  RegisterOclOperation(TBOS_ObjectTime);
  RegisterOclOperation(TBOS_AllInstancesAtTime);
  RegisterOclOperation(TBOS_ObjectTimeStamp);
  RegisterOclOperation(TBOS_Existing);
  RegisterOclOperation(TBOS_FilterOnType);
  RegisterOclOperation(TBOS_BoldTime);
  RegisterOclOperation(TBOS_TimeStampToTime);
  RegisterOclOperation(TBOS_TimeToTimeStamp);
  RegisterOclOperation(TBOS_IndexOf);
  RegisterOclOperation(TBOS_ReverseCollection);
  RegisterOclOperation(TBOS_asCommaText);
  RegisterOclOperation(TBOS_separate);
  RegisterOclOperation(TBOS_DayOfDate);
  RegisterOclOperation(TBOS_MonthOfDate);
  RegisterOclOperation(TBOS_YearOfDate);
  RegisterOclOperation(TBOS_DayOfWeekOfDate);
  RegisterOclOperation(TBOS_WeekOfDate);
  RegisterOCLOperation(TBOS_HoursBetween);
  RegisterOCLOperation(TBOS_MinutesBetween);
  RegisterOCLOperation(TBOS_SecondsBetween);
  RegisterOclOperation(TBOS_Trim);
  RegisterOclOperation(TBOS_HasDuplicates);
  RegisterOclOperation(TBOS_CommaSeparatedStringToStringCollection);
  RegisterOclOperation(TBOS_CommaSeparatedStringToIntegerCollection);
  RegisterOclOperation(TBOS_BoldId);
  RegisterOclOperation(TBOS_Power);
  RegisterOclOperation(TBOS_Sqrt);
  RegisterOclOperation(TBOS_BoldIDIn);
  RegisterOclOperation(TBOS_NamedIDIn);

finalization
  FreeAndNil(G_OCLOperations);

end.
