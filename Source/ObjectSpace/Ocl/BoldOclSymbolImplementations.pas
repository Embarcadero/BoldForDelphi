unit BoldOclSymbolImplementations;

interface

uses
  BoldOclClasses;

procedure InitializeSymbolTable(SymTab: TBoldSymbolDictionary);
procedure RegisterOCLOperation(OperationClass: TBoldOclSymbolClass);

implementation

uses
  SysUtils,
  BoldElements,
  BoldAttributes,
  BoldOclError,
  BoldDefs,
  BoldMath,
  BoldTypeList,
  BoldSystemRT,
  BoldSystem,
  Classes,
  BoldSubscription,
  BoldValueSpaceInterfaces, // besExisting is defined here...
  BoldRegularExpression,
  BoldCoreConsts;

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

  TBOS_Except = class(TBoldOclSymbol)
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

  TBOS_strToInt = class(TBoldOclSymbol)
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

  TBOS_FormatNumeric = class(TBoldOclSymbol)
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

  TBOS_or = class(TBoldOclSymbol)
  protected
    procedure Init; override;
  public
    procedure Evaluate(const Params: TBoldOclSymbolParameters); override;
  end;

  TBOS_and = class(TBoldOclSymbol)
  protected
    procedure Init; override;
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

  TBOS_SumTime = class(TBoldOclSymbol)
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

  TBOS_dateTimeAsFloat = class(TBoldOclSymbol)
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

procedure TBOS_Equal.Init;
begin
  InternalInit('=', [nil, nil], tbodNo, HELP.BooleanType, False, 100);
end;

procedure TBOS_NotEqual.Init;
begin
  InternalInit('<>', [nil, nil], tbodNo, HELP.BooleanType, False, 101); // do not localize
end;

procedure TBOS_Except.Init;
begin
  InternalInit('except', [nil, nil], tbodLCC, nil, true, 101); // do not localize
end;

procedure TBOS_Add.Init;
begin
  InternalInit('+', [nil, nil], tbodLCC, nil, False, 102); // do not localize
end;

procedure TBOS_Subtract.Init;
begin
  InternalInit('-',[HELP.NumericType, HELP.NumericType], tbodLCC, nil, False, 103); // do not localize
end;

procedure TBOS_UnaryMinus.Init;
begin
  InternalInit('unary-', [HELP.NumericType], tbodCopyArg1, nil, False, 104); // do not localize
end;

procedure TBOS_Multiply.Init;
begin
  InternalInit('*', [HELP.NumericType, HELP.NumericType], tbodLCC, nil, False, 105); // do not localize
end;

procedure TBOS_Divide.Init;
begin
  InternalInit('/', [HELP.NumericType, HELP.NumericType], tbodNo, HELP.RealType, False, 106); // do not localize
end;

procedure TBOS_Abs.Init;
begin
  InternalInit('abs', [HELP.NumericType], tbodCopyArg1, nil, True, 107); // do not localize
end;

procedure TBOS_Floor.Init;
begin
  InternalInit('floor', [HELP.NumericType], tbodNo, HELP.IntegerType, True, 108); // do not localize
end;

procedure TBOS_Round.Init;
begin
  InternalInit('round', [HELP.NumericType], tbodNo, HELP.IntegerType, True, 109); // do not localize
end;

procedure TBOS_strToInt.Init;
begin
  InternalInit('strToInt', [HELP.stringType], tbodNo, HELP.IntegerType, True, 109); // do not localize
end;

procedure TBOS_Max.Init;
begin
  InternalInit('max', [HELP.NumericType, HELP.NumericType], tbodLCC, nil, True, 110); // do not localize
end;
procedure TBOS_Min.Init;
begin
  InternalInit('min', [HELP.NumericType, HELP.NumericType], tbodLCC, nil, True, 111); // do not localize
end;

procedure TBOS_Less.Init;
begin
  InternalInit('<', [nil, nil], tbodNo, HELP.BooleanType, False, 112); // do not localize
end;
procedure TBOS_Greater.Init;
begin
  InternalInit('>', [nil, nil], tbodNo, HELP.BooleanType, False, 113); // do not localize
end;
procedure TBOS_LessEQ.Init;
begin
  InternalInit('<=', [nil, nil], tbodNo, HELP.BooleanType, False, 114); // do not localize
end;
procedure TBOS_GreaterEQ.Init;
begin
  InternalInit('>=', [nil, nil], tbodNo, HELP.BooleanType, False, 115); // do not localize
end;

procedure TBOS_Div.Init;
begin
  InternalInit('div', [HELP.IntegerType, HELP.IntegerType], tbodNo, HELP.IntegerType, False, 116); // do not localize
end;
procedure TBOS_Mod.Init;
begin
  InternalInit('mod', [HELP.IntegerType, HELP.IntegerType], tbodNo, HELP.IntegerType, False, 117); // do not localize
end;

procedure TBOS_Length.Init;
begin
  InternalInit('length', [HELP.StringType], tbodNo, HELP.IntegerType, True, 118); // do not localize
end;
procedure TBOS_concat.Init;
begin
  InternalInit('concat', [HELP.StringType, HELP.StringType], tbodNo, HELP.StringType, True, 119); // do not localize
end;
procedure TBOS_ToUpper.Init;
begin
  InternalInit('toUpper', [HELP.StringType], tbodNo, HELP.StringType, True, 120); // do not localize
end;
procedure TBOS_toLower.Init;
begin
  InternalInit('toLower', [HELP.StringType], tbodNo, HELP.StringType, True, 121); // do not localize
end;
procedure TBOS_SubString.Init;
begin
  InternalInit('subString', [HELP.StringType, HELP.IntegerType, HELP.IntegerType], tbodNo, HELP.StringType, True, 122); // do not localize
end;

procedure TBOS_Pad.Init;
begin
  InternalInit('pad', [HELP.StringType, HELP.IntegerType, HELP.StringType], tbodNo, HELP.StringType, True, 123); // do not localize
end;

procedure TBOS_PostPad.Init;
begin
  InternalInit('postPad', [HELP.StringType, HELP.IntegerType, HELP.StringType], tbodNo, HELP.StringType, True, 124); // do not localize
end;

procedure TBOS_formatNumeric.Init;
begin
  InternalInit('formatNumeric', [HELP.NumericType, HELP.StringType], tbodNo, HELP.StringType, True, 124); // do not localize
end;

procedure TBOS_formatDateTime.Init;
begin
  InternalInit('formatDateTime', [HELP.MomentType, HELP.StringType], tbodNo, HELP.StringType, True, 124); // do not localize
end;

procedure TBOS_StrToDate.Init;
begin
  InternalInit('strToDate', [HELP.StringType], tbodNo, HELP.DateType, True, 124); // do not localize
end;

procedure TBOS_StrToTime.Init;
begin
  InternalInit('strToTime', [HELP.StringType], tbodNo, HELP.TimeType, True, 124); // do not localize
end;

procedure TBOS_StrToDateTime.Init;
begin
  InternalInit('strToDateTime', [HELP.StringType], tbodNo, HELP.DateTimeType, True, 124); // do not localize
end;

procedure TBOS_or.Init;
begin
  InternalInit('or', [HELP.BooleanType, HELP.BooleanType], tbodNo, HELP.BooleanType, True, 125); // do not localize
end;
procedure TBOS_and.Init;
begin
  InternalInit('and', [HELP.BooleanType, HELP.BooleanType], tbodNo, HELP.BooleanType, True, 126); // do not localize
end;
procedure TBOS_xor.Init;
begin
  InternalInit('xor', [HELP.BooleanType, HELP.BooleanType], tbodNo, HELP.BooleanType, True, 127); // do not localize
end;
procedure TBOS_implies.Init;
begin
  InternalInit('implies', [HELP.BooleanType, HELP.BooleanType], tbodNo, HELP.BooleanType, True, 128); // do not localize
end;
procedure TBOS_not.Init;
begin
  InternalInit('not', [HELP.BooleanType], tbodNo, HELP.BooleanType, True, 129); // do not localize
end;

procedure TBOS_if.Init;
begin
  InternalInit('if', [HELP.BooleanType, nil, nil], tbodLCC23, nil, True, 130); // do not localize
end;

procedure TBOS_Size.Init;
begin
  InternalInit('size', [HELP.ListType], tbodNo, HELP.IntegerType, True, 131); // do not localize
end;
procedure TBOS_includes.Init;
begin
  InternalInit('includes', [HELP.ListType, nil], tbodNo, HELP.BooleanType, True, 132, true); // do not localize
end;
procedure TBOS_Count.Init;
begin
  InternalInit('count', [HELP.ListType, nil], tbodNo, HELP.IntegerType, True, 133, true); // do not localize
end;
procedure TBOS_IncludesAll.Init;
begin
  InternalInit('includesAll', [HELP.ListType, HELP.ListType], tbodNo, HELP.BooleanType, True, 134, true); // do not localize
end;
procedure TBOS_isEmpty.Init;
begin
  InternalInit('isEmpty', [HELP.ListType], tbodNo, HELP.BooleanType, True, 135); // do not localize
end;

procedure TBOS_NotEmpty.Init;
begin
  InternalInit('notEmpty', [HELP.ListType], tbodNo, HELP.BooleanType, True, 136); // do not localize
end;
procedure TBOS_Sum.Init;
begin
  InternalInit('sum', [HELP.NumericListType], tbodCopyArg1Elem, nil, True, 137); // do not localize
end;

procedure TBOS_MinValue.Init;
begin
  InternalInit('minValue', [HELP.NumericListType], tbodCopyArg1Elem, nil, True, 138); // do not localize
end;
procedure TBOS_Maxvalue.Init;
begin
  InternalInit('maxValue', [HELP.NumericListType], tbodCopyArg1Elem, nil, True, 139); // do not localize
end;
procedure TBOS_Average.Init;
begin
  InternalInit('average', [HELP.NumericListType], tbodNo, HELP.RealType, True, 140); // do not localize
end;
procedure TBOS_Exists.Init;
begin
  InternalInit('exists', [HELP.ListType, HELP.BooleanType], tbodNo, HELP.BooleanType, True, 141); // do not localize
end;
procedure TBOS_ForAll.Init;
begin
  InternalInit('forAll', [HELP.ListType, HELP.BooleanType], tbodNo, HELP.BooleanType, True, 142); // do not localize
end;
procedure TBOS_union.Init;
begin
  InternalInit('union', [HELP.ListType, HELP.ListType], tbodLCC, nil, True, 143); // do not localize
end;
procedure TBOS_Intersection.Init;
begin
  InternalInit('intersection', [HELP.ListType, HELP.ListType], tbodLCC, nil, True, 144, true); // do not localize
end;
procedure TBOS_difference.Init;
begin
  InternalInit('difference', [HELP.ListType, HELP.ListType], tbodCopyArg1, nil, True, 145, true); // do not localize
end;
procedure TBOS_Including.Init;
begin
  InternalInit('including', [HELP.ListType, HELP.ObjectType], tbodLCC, nil, True, 146); // do not localize
end;
procedure TBOS_excluding.Init;
begin
  InternalInit('excluding', [HELP.ListType, HELP.ObjectType], tbodCopyArg1, nil, True, 147); // do not localize
end;
procedure TBOS_SymmetricDifference.Init;
begin
  InternalInit('symmetricDifference', [HELP.ListType, HELP.ListType], tbodLCC, nil, True, 148); // do not localize
end;
procedure TBOS_Select.Init;
begin
  InternalInit('select', [HELP.ListType, HELP.BooleanType], tbodCopyArg1, nil, True, 149); // do not localize
end;
procedure TBOS_reject.Init;
begin
  InternalInit('reject', [HELP.ListType, HELP.BooleanType], tbodCopyArg1, nil, True, 150); // do not localize
end;
procedure TBOS_collect.Init;
begin
  InternalInit('collect', [HELP.ListType, nil], tbodListofArg2, nil, True, 151); // do not localize
end;
procedure TBOS_AsSequence.Init;
begin
  InternalInit('asSequence', [HELP.ListType], tbodArg1AsList, nil, True, 152); // do not localize
end;
procedure TBOS_AsBag.Init;
begin
  InternalInit('asBag', [HELP.ListType], tbodArg1AsList, nil, True, 153); // do not localize
end;
procedure TBOS_AsSet.Init;
begin
  InternalInit('asSet', [HELP.ListType], tbodArg1AsList, nil, True, 154); // do not localize
end;
procedure TBOS_Append.Init;
begin
  InternalInit('append', [HELP.ListType, HELP.ObjectType], tbodLCC, nil, True, 155); // do not localize
end;
procedure TBOS_Prepend.Init;
begin
  InternalInit('prepend', [HELP.ListType, HELP.ObjectType], tbodLCC, nil, True, 156); // do not localize
end;
procedure TBOS_SubSequence.Init;
begin
  InternalInit('subSequence', [HELP.ListType, HELP.IntegerType, HELP.IntegerType], tbodCopyArg1, nil, True, 157); // do not localize
end;
procedure TBOS_at.Init;
begin
  InternalInit('at', [HELP.ListType, HELP.IntegerType], tbodCopyArg1Elem, nil, True, 158); // do not localize
end;
procedure TBOS_first.Init;
begin
  InternalInit('first', [HELP.ListType], tbodCopyArg1Elem, nil, True, 159); // do not localize
end;
procedure TBOS_last.Init;
begin
  InternalInit('last', [HELP.ListType], tbodCopyArg1Elem, nil, True, 160); // do not localize
end;

procedure TBOS_orderby.Init;
begin
  InternalInit('orderby', [HELP.ListType, nil], tbodCopyArg1, nil, True, 161); // do not localize
end;
procedure TBOS_orderDescending.Init;
begin
  InternalInit('orderdescending', [HELP.ListType, nil], tbodCopyArg1, nil, True, 162); // do not localize
end;



procedure TBOS_asString.Init;
begin
  InternalInit('asString', [nil], tbodNo, HELP.StringType, True, 163); // do not localize
end;

procedure TBOS_dateTimeAsFloat.Init;
begin
  InternalInit('dateTimeAsFloat', [HELP.MomentType], tbodNo, HELP.RealType, True, 163); // do not localize
end;

procedure TBOS_TypeName.Init;
begin
  InternalInit('typename', [HELP.TypeType], tbodNo, HELP.StringType, True, 164); // do not localize
end;

procedure TBOS_Attributes.Init;
begin
  InternalInit('attributes', [HELP.TypeType], tbodNo, HELP.StringListType, True, 165); // do not localize
end;

procedure TBOS_AssociationEnds.Init;
begin
  InternalInit('associationEnds', [HELP.TypeType], tbodNo, HELP.StringListType, True, 166); // do not localize
end;

{procedure TBOS_Operations.Init;
begin
  InternalInit('operations', [HELP.TypeType], tbodNo, HELP.StringListType, True, 167); // do not localize
end;
}

procedure TBOS_SuperTypes.Init;
begin
  InternalInit('superTypes', [HELP.TypeType], tbodNo, HELP.TypeListType, True, 168); // do not localize
end;

procedure TBOS_AllSuperTypes.Init;
begin
  InternalInit('allSuperTypes', [HELP.TypeType], tbodNo, HELP.TypeListType, True, 169); // do not localize
end;

procedure TBOS_AllSubClasses.Init;
begin
  InternalInit('allSubClasses', [HELP.TypeType], tbodNo, HELP.TypeListType, True, 169); // do not localize
end;

procedure TBOS_AllInstances.Init;
begin
  InternalInit('allInstances', [HELP.TypeType], tbodObjectList, nil, True, 170); // do not localize
end;

procedure TBOS_AllLoadedObjects.Init;
begin
  InternalInit('allLoadedObjects', [HELP.TypeType], tbodObjectList, nil, True, 170); // do not localize
end;

procedure TBOS_emptyList.Init;
begin
  InternalInit('emptyList', [HELP.TypeType], tbodObjectList, nil, True, 170); // do not localize
end;

procedure TBOS_oclType.Init;
begin
  InternalInit('oclType', [nil], tbodNo, HELP.TypeType, True, 171); // do not localize
end;

procedure TBOS_oclIsKindOf.Init;
begin
  InternalInit('oclIsKindOf', [nil, Help.TypeType], tbodNo, HELP.BooleanType, True, 172); // do not localize
end;

procedure TBOS_OclIsTypeOf.Init;
begin
  InternalInit('oclIsTypeOf', [nil, Help.TypeType], tbodNo, HELP.BooleanType, True, 173); // do not localize
end;

procedure TBOS_OclAsType.Init;
begin
  InternalInit('oclAsType', [nil, Help.TypeType], tbodTypeCast, nil, True, 174); // do not localize
end;

procedure TBOS_SafeCast.Init;
begin
  InternalInit('safeCast', [nil, Help.TypeType], tbodTypeCast, nil, True, 174); // do not localize
end;

procedure TBOS_sqlLike.Init;
begin
  InternalInit('sqlLike', [help.StringType, help.StringType], tbodNo, Help.BooleanType, True, 175); // do not localize
end;

procedure TBOS_SqlLikeCaseInsensitive.Init;
begin
  InternalInit('sqlLikeCaseInsensitive', [help.StringType, help.StringType], tbodNo, Help.BooleanType, True, 176); // do not localize
end;

procedure TBOS_RegExpMatch.Init;
begin
  InternalInit('regExpMatch', [help.StringType, help.StringType], tbodNo, Help.BooleanType, True, 177); // do not localize
end;

procedure TBOS_InDateRange.Init;
begin
  InternalInit('inDateRange', [help.MomentType, help.NumericType, help.NumericType], tbodNo, Help.BooleanType, True, 178); // do not localize
end;

procedure TBOS_InTimeRange.Init;
begin
  InternalInit('inTimeRange', [help.MomentType, help.NumericType, help.NumericType], tbodNo, Help.BooleanType, True, 179); // do not localize
end;

procedure TBOS_isNull.Init;
begin
  InternalInit('isNull', [nil], tbodNo, HELP.BooleanType, True, 180); // do not localize
end;

procedure TBOS_Constraints.Init;
var
  ConstraintListTypeInfo: TBoldListTypeInfo;
begin
  ConstraintListTypeInfo := Help.SystemTypeInfo.ListTypeInfoByElement[Help.ConstraintType];
  InternalInit('constraints', [nil], tbodNo, ConstraintListTypeInfo, True, 181); // do not localize
end;

procedure TBOS_AtTime.Init;
begin
  InternalInit('atTime', [help.ObjectType, help.IntegerType], tbodCopyArg1, nil, True, 182); // do not localize
end;

procedure TBOS_ObjectTimeStamp.Init;
begin
  InternalInit('objectTimeStamp', [help.ObjectType], tbodNo, Help.IntegerType, True, 182); // do not localize
end;


procedure TBOS_allInstancesAtTime.Init;
begin
  InternalInit('allInstancesAtTime', [help.TypeType, help.IntegerType], tbodObjectList, nil, True, 183); // do not localize
end;

procedure TBOS_existing.Init;
begin
  InternalInit('existing', [help.ObjectType], tbodNo, HELP.BooleanType, True, 184); // do not localize
end;

procedure TBOS_FilterOnType.Init;
begin
  InternalInit('filterOnType', [help.ListType, help.TypeType], tbodListFromArg2, nil, True, 185); // do not localize
end;

procedure TBOS_BoldTime.Init;
begin
  InternalInit('boldTime', [help.ObjectType], tbodno, help.integerType, True, 186); // do not localize
end;

procedure TBOS_TimeStampToTime.Init;
begin
  InternalInit('timeStampToTime', [help.IntegerType], tbodno, help.DateTimeType, True, 187); // do not localize
end;

procedure TBOS_TimeToTimeStamp.Init;
begin
  InternalInit('timeToTimeStamp', [help.DateTimeType], tbodno, help.IntegerType, True, 188); // do not localize
end;

procedure TBOS_SumTime.Init;
begin
  InternalInit('sumTime', [HELP.MomentListType], tbodNo, Help.DateTimeType, True, 189); // do not localize
end;

procedure TBOS_StringRepresentation.Init;
begin
  InternalInit('stringRepresentation', [nil, help.integerType], tbodNo, HELP.StringType, True, 190); // do not localize
end;

procedure TBOS_TaggedValue.Init;
begin
  InternalInit('taggedValue', [help.ObjectType, help.StringType], tbodNo, HELP.StringType, True, 190); // do not localize
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
  else
    HELP.MakeNewBoolean(Params.Result, Params.values[0].IsEqual(Params.values[1]));
end;

procedure TBOS_NotEqual.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if (Params.nodes[1] is TBoldOclEnumLiteral) or (Params.nodes[0] is TBoldOclEnumLiteral) then
    HELP.MakeNewBoolean(Params.Result, not CompareEnumLiterals(Params))
  else if not assigned(Params.values[0]) then
    Help.MakeNewBoolean(Params.Result, Assigned(Params.values[1]))
  else
    HELP.MakeNewBoolean(Params.Result, not Params.values[0].IsEqual(Params.values[1]));
end;

procedure TBOS_Except.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  // this operation does not need to do anything as it is all handled by the evaluator
  // this method is never called...
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
begin
  try
    HELP.MakeNewNumeric(Params.Result, XNumeric(Params.values[0]) / XNumeric(Params.values[1]));
  except
    on e: EDivByZero do
      raise EBoldOclRuntimeError.CreateFmt(borteDivisionByZero, [0]);
    on e: EZeroDivide do
      raise EBoldOclRuntimeError.CreateFmt(borteDivisionByZero, [0]);
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

procedure TBOS_StrToInt.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewInteger(Params.Result, StrToIntDef(XString(Params.values[0]), 0));
end;

procedure TBOS_Max.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if Params.Result.BoldType.ConformsTo(HELP.integerType) then
    HELP.MakeNewInteger(Params.Result, MaxIntValue([XInteger(Params.values[0]),XInteger(Params.values[1])]))
  else
    HELP.MakeNewNumeric(Params.Result, maxValue([XNumeric(Params.values[0]),XNumeric(Params.values[1])]));
end;

procedure TBOS_Min.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if Params.Result.BoldType.ConformsTo(HELP.integerType) then
    HELP.MakeNewInteger(Params.Result, MinIntValue([XInteger(Params.values[0]),XInteger(Params.values[1])]))
  else
    HELP.MakeNewNumeric(Params.Result, MinValue([XNumeric(Params.values[0]),XNumeric(Params.values[1])]));
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
  HELP.MakeNewString(Params.Result, AnsiUpperCase(XString(Params.values[0])));
end;

procedure TBOS_toLower.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  HELP.MakeNewString(Params.Result, AnsiLowerCase(XString(Params.values[0])));
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

procedure TBOS_FormatNumeric.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  help.MakeNewString(params.result, format(XString(Params.values[1]), [XNumeric(Params.values[0])]));
end;

procedure TBOS_FormatDateTime.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Moment: TBAMoment;
  val: TDateTime;
begin
  Moment := Params.values[0] as TBAMoment;
  if moment.IsNull then
  begin
    help.MakeNewString(params.result, '');
  end
  else
  begin
    if Moment is TBADateTime then
      val := (Moment as TBADateTime).asDateTime
    else if Moment is TBADate then
      val := (Moment as TBADate).asDate
    else if Moment is TBATime then
      val := (Moment as TBATime).asTime
    else
      val := 0;
    help.MakeNewString(params.result, formatDateTime(XString(Params.values[1]), val));
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

    while (i < list2.Count) and IncludesAll do
    begin
      IncludesAll := IncludesAll and list1.Includes(list2[i]);
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
    HELP.MakeNewBoolean(Params.Result, true)
  else
    HELP.MakeNewBoolean(Params.Result, List.Count <> 0);
end;

procedure TBOS_IsNull.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if Params.values[0] is TBoldAttribute then
    HELP.MakeNewBoolean(Params.Result, (Params.values[0] as TBoldAttribute).IsNull)
  else
    HELP.MakeNewBoolean(Params.Result, false);
end;

procedure TBOS_Select.Evaluate(const Params: TBoldOclSymbolParameters);
  // First argument is the evaluated expression, the second argument is the object
begin
  if Assigned(XList(Params.Result.value)) and XBoolean(Params.values[0]) then
    XList(Params.Result.value).Add(Params.values[1]);
end;

procedure TBOS_reject.Evaluate(const Params: TBoldOclSymbolParameters);
  // First argument is the evaluated expression, the second argument is the object
begin
  if Assigned(XList(Params.Result.value)) and not XBoolean(Params.values[0]) then
    XList(Params.Result.value).Add(Params.values[1]);
end;

procedure TBOS_collect.Evaluate(const Params: TBoldOclSymbolParameters);
  // First argument is the evaluated expression, the second argument is the object
var
  ResultList: TBoldList;
  SourceList: TBoldList;
  i: integer;
begin
  ResultList := XList(Params.Result.value);
  if assigned(Resultlist) then
  begin
    sourceList := XList(Params.values[0]);
    if assigned(SourceList) then
    begin
      for i := 0 to SourceList.Count - 1 do
        ResultList.add(SourceList[i]);
    end
    else
    begin
      resultList.Add(Params.values[0]);
    end;
  end;
end;

procedure TBOS_GenericOrder.Evaluate(const Params: TBoldOclSymbolParameters);
var
  SortList: TBoldMemberList;
  DummyValue: TBoldAttribute;
begin
  Sortlist := XList(Params.Result.value) as tBoldMemberList;
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
      Sum := Sum + XNumeric(List[i]);

  if ListElementType.ConformsTo(HELP.IntegerType) then
    HELP.MakeNewInteger(Params.Result, Round(Sum))
  else if ListElementType.ConformsTo(HELP.CurrencyType) then
    HELP.MakeNewCurrency(Params.Result, Sum)
  else
    HELP.MakeNewNumeric(Params.Result, Sum);
end;


procedure TBOS_SumTime.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Sum          : TDateTime;
  i            : Integer;
  List         : TBoldList;
begin
  Sum := 0;
  List := XList(Params.values[0]);

  if assigned(list) then
    for i := 0 to List.Count - 1 do
      Sum := Sum + XDateTime(List[i]);

  HELP.MakeNewDateTime(Params.Result, Sum)
end;


procedure TBOS_Maxvalue.Evaluate(const Params: TBoldOclSymbolParameters);
var
  MAX          : Double;
  i            : Integer;
  List         : TBoldList;
  ListElementType: TBoldElementTypeInfo;
begin
  List := XList(Params.values[0]);
  ListElementType := Params.Result.BoldType;
  if not assigned(list) or (list.Count = 0) then
    Max := 0
  else
  begin
    MAX := xNumeric(List[0]);
    for i := 1 to List.Count - 1 do
    begin
      if MAX < XNumeric(List[i]) then
        MAX := XNumeric(List[i])
    end;
  end;
  if ListElementType.ConformsTo(HELP.IntegerType) then
    HELP.MakeNewInteger(Params.Result, Round(MAX))
  else if ListElementType.ConformsTo(HELP.CurrencyType) then
    HELP.MakeNewCurrency(Params.Result, MAX)
  else
    HELP.MakeNewNumeric(Params.Result, MAX);
end;

procedure TBOS_MinValue.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Min          : Double;
  i            : Integer;
  List         : TBoldList;
  ListElementType: TBoldElementTypeInfo;
begin
  List := XList(Params.values[0]);
  ListElementType := Params.Result.Boldtype;
  if not assigned(list) or (List.Count = 0) then
    min := 0
  else
  begin
    Min := XNumeric(List[0]);
    for i := 1 to List.Count - 1 do
      if Min > XNumeric(List[i]) then Min := XNumeric(List[i]);
  end;

  if ListElementType.ConformsTo(HELP.IntegerType) then
    HELP.MakeNewInteger(Params.Result, Round(Min))
  else if ListElementType.ConformsTo(HELP.CurrencyType) then
    HELP.MakeNewCurrency(Params.Result, Min)
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
    begin
      Sum := Sum + XNumeric(List[i]);
    end;
    HELP.MakeNewNumeric(Params.Result, Sum / List.Count);
  end;
end;

procedure TBOS_ForAll.Evaluate(const Params: TBoldOclSymbolParameters);
  // First argument is the evaluated expression, the second argument is the object
begin
  (Params.Result.Value as TBABoolean).AsBoolean := XBoolean(Params.Result.value) and xBoolean(Params.values[0]);
end;

procedure TBOS_Exists.Evaluate(const Params: TBoldOclSymbolParameters);
  // First argument is the evaluated expression, the second argument is the object
begin
  (Params.Result.Value as TBABoolean).AsBoolean := XBoolean(Params.Result.value) or XBoolean(Params.values[0]);
end;

procedure TBOS_includes.Evaluate(const Params: TBoldOclSymbolParameters);
var
  List: TBoldList;
  i : Integer;
  temp: Boolean;
begin
  temp := False;
  List := XList(Params.values[0]);
  If Assigned(List) then
    for i := 0 to List.Count - 1 do
      temp := temp or (List[i].IsEqual(Params.values[1]));
  HELP.MakeNewBoolean(Params.Result, temp);
end;

procedure TBOS_union.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if not assigned(xlist(Params.values[0])) then
    Help.TransferOrClone(Params.nodes[1], Params.Result)
  else
  begin
    // if the result is a supertype of the two parameters, then we can not reuse either list.
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
      // Make sure we loop over the shortes list.
      resList := list1;
      list1 := list2;
      list2 := resList;
    end;

    resList := help.CreateNewMember(Params.Result.BoldType) as TBoldList;
    for i := 0 to list1.Count - 1 do
      if list2.Includes(list1[i]) then
        resList.Add(list1[i]);
    Params.Result.SetOwnedValue(resList);
  end;
end;

procedure TBOS_difference.Evaluate(const Params: TBoldOclSymbolParameters);
var
  i, p         : Integer;
  list1, list2: TBoldList;
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
    for i := 0 to list2.Count - 1 do
    begin
      p := list1.IndexOf(List2[i]);
      if p <> -1 then
        list1.RemoveByIndex(p);
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
    for i := 0 to list1.Count - 1 do
      if not list2.Includes(list1[i]) then resList.Add(list1[i]);

    for i := 0 to list2.Count - 1 do
      if not list1.Includes(list2[i]) then resList.Add(list2[i]);
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
  CopyListToResult(params);

  if assigned(xlist(Params.Result.value)) then
    XList(Params.Result.value).DuplicateMode := bldmMerge;
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
begin
  if not assigned(xlist(Params.values[0])) then
    raise EBoldOclRunTimeError.CreateFmt(boeArgrtIsNotList, [0, '->subSequence']); // do not localize

  List := XList(Params.values[0]);
  Start := XInteger(Params.values[1])- 1;
  Stop := XInteger(Params.values[2])- 1;

  resList := help.CreateNewMember(Params.Result.BoldType) as TBoldList;

  if Start < 0 then Start := 0;
  if Stop > List.Count - 1 then Stop := List.Count - 1;
  for i := Start to Stop do
    resList.Add(List[i]);

  Params.Result.SetOwnedValue(resList);
end;

procedure TBOS_at.Evaluate(const Params: TBoldOclSymbolParameters);
var
  List: TBoldList;
  P : Integer;
  EL: TBoldElement;
begin
  List := XList(Params.values[0]);
  if not assigned(list) then
    Params.Result.SetReferenceValue(nil)
  else
  begin
    P := XInteger(Params.values[1]) - 1;
    try
      EL := List[P];
    except
      on e:EListError do
        raise EBoldOclRunTimeError.CreateFmt(borteAtIndexOutOfBounds, [0, p, list.Count]);
    end;
    EL.GetAsValue(Params.Result);
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
  // First argument is the evaluated expression, the second argument is the object          ; Subscriber: TBoldSubscriber
begin
  HELP.MakeNewString(Params.Result, XString(Params.values[0]));
  if assigned(Params.values[0]) then
    Params.values[0].SubscribeToStringRepresentation(brDefault, Params.subscriber, breReEvaluate);
end;

procedure TBOS_dateTimeAsFloat.Evaluate(const Params: TBoldOclSymbolParameters);
  // First argument is the evaluated expression, the second argument is the object          ; Subscriber: TBoldSubscriber
begin
  HELP.MakeNewNumeric(Params.Result, XDateTime(Params.values[0]));
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
begin
  if not assigned(xtype(Params.values[1])) then
    raise EBoldOclRunTimeError.CreateFmt(boeArgrtIsNottype, [0, '->oclAsType']); // do not localize

  if not assigned(Params.values[0]) or Params.values[0].BoldType.ConformsTo(XType(Params.values[1])) then
    Params.Result.SetReferenceValue(Params.values[0])
  else
    raise EBoldOclRunTimeError.CreateFmt(borteInvalidCast, [0, Params.values[0].BoldType.AsString, Params.values[1].AsString]);
end;

procedure TBOS_SafeCast.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if not assigned(xtype(Params.values[1])) then
    raise EBoldOclRunTimeError.CreateFmt(boeArgrtIsNottype, [0, '.safeCast']); // do not localize

  if not assigned(Params.values[0]) or Params.values[0].BoldType.ConformsTo(XType(Params.values[1])) then
    Params.Result.SetReferenceValue(Params.values[0])
  else
    Params.Result.SetReferenceValue(nil)
end;


procedure TBOS_oclIsKindOf.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if not assigned(xtype(Params.values[1])) then
    raise EBoldOclRunTimeError.CreateFmt(boeArgrtIsNottype, [0, '->oclIsKindOf']); // do not localize

  HELP.MakeNewBoolean(Params.Result, assigned(Params.values[0]) and Params.values[0].BoldType.ConformsTo(Xtype(Params.values[1])));
end;

procedure TBOS_OclIsTypeOf.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  if not assigned(xtype(Params.values[1])) then
    raise EBoldOclRunTimeError.CreateFmt(boeArgrtIsNottype, [0, '->oclIsTypeOf']); // do not localize

  HELP.MakeNewBoolean(Params.Result, assigned(Params.values[0]) and (Params.values[0].BoldType = Xtype(Params.values[1])));
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

    if AttributeTypeInfo.ConformsTo(Params.SystemTypeInfo.AttributeTypeInfoByExpressionName['ValueSet']) then // do not localize
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

//      StrList.Sort;

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
      ClassList.AddSmallSubscription(params.subscriber, [beItemAdded, beItemDeleted, beObjectFetched], breReEvaluate);

    if ClassList.BoldPersistenceState <> bvpsCurrent then
    begin
      ObjectList := help.CreateNewMember(Params.Result.BoldType) as TBoldObjectList;
      Params.Result.SetOwnedValue(ObjectList);
      Traverser := Params.System.Locators.CreateTraverser;
      while not Traverser.EndOfList do
      begin
        if assigned(Traverser.Locator.BoldObject) and
          Traverser.Locator.Boldobject.BoldType.ConformsTo(ClassTypeInfo) then
          ObjectList.Add(Traverser.Locator.Boldobject);
        Traverser.Next;
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
    for i := 0 to ClassInfo.AllMembers.Count - 1 do
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
    for i := 0 to ClassInfo.AllMembers.Count - 1 do
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

var
  RegExp: TRegularExpression;
  SQLRegExp: TRegularExpression;

function FixSQLRegExp(S: String): String;
var
  i: integer;
begin
  Result := #11;
  for i := 1 to Length(S) do
    case S[i] of
      '%': Result := Result + #1 + #8;
      '_': Result := Result + #1;
      else Result := Result + S[i];
    end;
  Result := Result + #6;
end;

Procedure InitSQLRegExp;
begin
  if not assigned(SQLRegExp) then
  begin
    SQLRegExp := TRegularExpression.Create;
    with SQLRegExp.MetaCharacters do
    begin
      AnyChar := #1;
      CharSetClose := #2;
      CharSetComplement := #3;
      CharSetOpen := #4;
      CharSetRange := #5;
      EndOfLine := #6;
      QuoteChar := #7;
      Repeat0OrMoreTimes := #8;
      Repeat1OrMoreTimes := #9;
      StartOfLine := #11;
    end;
  end;
end;

procedure InitRegExp;
begin
  if not assigned(RegExp) then
    RegExp := TRegularExpression.Create;
end;

procedure TBOS_SQLLike.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  InitSQLRegExp;
  SQlRegExp.RegularExpression := FixSqlRegExp(XString(Params.values[1]));
  SqlRegExp.CaseSensitive := true;
  Help.MakeNewBoolean(Params.Result, SQLRegExp.SearchString(XString(Params.values[0])) <> 0);
end;

procedure TBOS_SQLLikeCaseInSensitive.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  InitSQLRegExp;
  SQlRegExp.RegularExpression := FixSqlRegExp(XString(Params.values[1]));
  SqlRegExp.CaseSensitive := False;
  Help.MakeNewBoolean(Params.Result, SQLRegExp.SearchString(XString(Params.values[0])) <> 0);
end;

procedure TBOS_RegExpMatch.Evaluate(const Params: TBoldOclSymbolParameters);
begin
  InitRegExp;
  RegExp.RegularExpression := XString(Params.values[1]);
  Help.MakeNewBoolean(Params.Result, RegExp.SearchString(XString(Params.values[0])) <> 0);
end;

procedure TBOS_InDateRange.Evaluate(const Params: TBoldOclSymbolParameters);
var
  Moment: TBAMoment;
  Start, Stop, val: TDateTime;
begin
  Moment := Params.values[0] as TBAMoment;
  if moment.IsNull then
  begin
    help.MakeNewBoolean(params.result, false);
  end
  else
  begin
    if Moment is TBADateTime then
      val := (Moment as TBADateTime).asDateTime
    else if Moment is TBADate then
      val := (Moment as TBADate).asDate
    else if Moment is TBATime then
      val := (Moment as TBATime).asTime
    else
      val := 0;

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
  if Moment is TBADateTime then
    val :=(Moment as TBADateTime).asDateTime
  else if Moment is TBADate then
    val :=(Moment as TBADate).asDate
  else if Moment is TBATime then
    val :=(Moment as TBATime).asTime
  else
    val := 0;

  Val := Frac(val);

  Start := frac((Params.values[1] as TBANumeric).AsFloat);
  Stop := frac((Params.values[2] as TBANumeric).AsFloat);
  Help.MakeNewBoolean(Params.Result, (val >= start) and (val <= stop));
end;

procedure TBOS_Constraints.Evaluate(const Params: TBoldOclSymbolParameters);
var
  ResList: TBoldMemberList;

procedure AddConstraint(IncomingConstraint: TBoldConstraintRTInfo; element: TBoldElement);
begin
  with ResList.AddNew as TBAConstraint do
    Initialize(IncomingConstraint, element);
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
  NewTime := (Params.values[1] as TBAInteger).AsInteger;
  Params.Result.SetReferenceValue(OldObject.AtTime(NewTime));
end;

procedure TBOS_ObjectTimeStamp.Evaluate(const Params: TBoldOclSymbolParameters);
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
begin
  IncomingList := XList(Params.values[0]);
  OutList := TBoldMemberFactory.CreateMemberFromBoldType(params.Result.BoldType) as TBoldList;
  for i := 0 to IncomingList.Count - 1 do
    if IncomingList[i].BoldType.ConformsTo(Params.values[1] as TBoldElementTypeInfo) then
      OutList.Add(IncomingList[i]);
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

procedure InitializeSymbolTable(SymTab: TBoldSymbolDictionary);
var
  i: integer;
begin
  for i := 0 to OCLOperations.Count - 1 do
    SymTab.Add(TBoldOclSymbolClass(OCLOperations[i]).Create(SymTab.Help));
end;

initialization
  SQLRegExp := nil;
  RegExp := nil;
  RegisterOclOperation(TBOS_Equal);
  RegisterOclOperation(TBOS_NotEqual);
  RegisterOclOperation(TBOS_Except);
  RegisterOclOperation(TBOS_Add);
  RegisterOclOperation(TBOS_Subtract);
  RegisterOclOperation(TBOS_UnaryMinus);
  RegisterOclOperation(TBOS_Multiply);
  RegisterOclOperation(TBOS_Divide);
  RegisterOclOperation(TBOS_Abs);
  RegisterOclOperation(TBOS_Floor);
  RegisterOclOperation(TBOS_Round);
  RegisterOclOperation(TBOS_strToInt);
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
  RegisterOclOperation(TBOS_SumTime);
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
  RegisterOclOperation(TBOS_dateTimeAsFloat);
  RegisterOclOperation(TBOS_stringRepresentation);
  RegisterOclOperation(TBOS_TaggedValue);
  RegisterOclOperation(TBOS_TypeName);
  RegisterOclOperation(TBOS_Attributes);
  RegisterOclOperation(TBOS_AssociationEnds);
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
  RegisterOclOperation(TBOS_FormatNumeric);
  RegisterOclOperation(TBOS_FormatDateTime);
  RegisterOclOperation(TBOS_StrToDateTime);
  RegisterOclOperation(TBOS_StrToDate);
  RegisterOclOperation(TBOS_StrToTime);
  RegisterOclOperation(TBOS_AtTime);
  RegisterOclOperation(TBOS_ObjectTimeStamp);
  RegisterOclOperation(TBOS_AllInstancesAtTime);
  RegisterOclOperation(TBOS_Existing);
  RegisterOclOperation(TBOS_FilterOnType);
  RegisterOclOperation(TBOS_BoldTime);
  RegisterOclOperation(TBOS_TimeStampToTime);
  RegisterOclOperation(TBOS_TimeToTimeStamp);

finalization
  SqlRegExp.Free;
  RegExp.Free;
  FreeAndNil(G_OCLOperations);

end.
