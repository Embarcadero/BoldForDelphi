
{ Global compiler directives }
{$include bold.inc}
unit BoldOclError;

interface

uses
  SysUtils,
  BoldDefs;

resourceString

  boeUserSpecifiedCollections      = '%d:User specified collections are not implemented';
  boeTimeExpressionsUnIMpl         = '%d:Timeexpressions aren''t implemented';
  boeUserSpecifiedEnumsUnImpl      = '%d:User specified enumerations aren''t implemented';
  boePackageSpecUnImpl             = '%d:Package specification in types aren''t implemented';
  boeConversionError               = '%d:Strange... Conversion failed in OCL parser (tried to convert %s to %s)';
  boeInvalidMemberName             = '%d:Member names may not be type path-names, only single names';
  boeInvalidMethodName             = '%d:Method names may not be type path-names, only single names';
  boeInvalidLoopVariable           = '%d:This function (%s) may not define a loop variable';
  boeOperationWithOutContext       = '%d:Strange indeed! an operation without a context...';
  boeInvalidQualifier              = '%d:Qualifiers aren''t legal here' ;
  boeInvalidTimeSpecifier          = '%d:Timespecifiers aren''t legal here';
  boeOperationNotDefinedOnArgTypes = '%d:%s %s %s is not defined';
  boeNoConform                     = '%d:%s does not conform to %s';
  boeSystemAsOperationArgument     = '%d:The entire system can not be argument to an operation';
  boeUndefinedOperation            = '%d:Undefined operation: %s';
  boeWrongNumberOfArgs             = '%d:%d parameters expected, %d found...';
  boeInvalidOperationOnAttr        = '%d:%s is not an operation on the type %s';
  boeMethodsNotImplemented         = '%d:Method calls on classes are not yet implemented';
  boeUnknownClass                  = '%d:%s is not a classname in the model';
  boeUnknownMember                 = '%d:%s is not a member of %s';
  boeUnknownMemberType             = '%d:This member is neither an attribute or a role (%s)';
  boeNotAnOperation                = '%d:%s is not an operation on %s';
  boeUnknownVariable               = '%d:Can''t find variable %s in environment';
  boeOperationWithLoopvar          = '%d:Only iterations may use CopyLoopVar-semantics! (tried with %s)';
  boeNonListArgumentToCopyArg1Elem = '%d:Can''t take ListElement-type of a non-list';
  boeNoCommonSuperClass            = '%d:The parameters of this operation have no common superclass (%s and %s)';
  boeNoRootElement                 = '%d:OCL expression has no root element' ;
  boeUnKnownRoot                   = '%d:Unknown type of root: %s';
  boeExpressionNeedsContext        = '%d:This expression needs a context/model';
  boeExpressionNeedsRoot           = '%d:This expression needs a root element';
  boeVariableNotAssigned           = '%d:This variable (%s) has no value or type';
  boeSubscribeBeforeEval           = '%d:You can not subscribe to an expression that has not been evaluated';
  boeCollectionNotValidLiteral     = '%d:You can not create a literal of type "Collection", try Set, Sequence or Bag instead';
  boeElementnotConformToCollection = '%d:All elements in a collection literal must conform to a common type, this one doesn''t';
  boeRangeMustBeInt                = '%d:Ranges can only consist of integers';
  boeInvalidCharacter              = '%d:Expression contains an invalid character';
  boeUnMatchedParentesis           = '%d:Parenthesis are not in balance';
  boeunterminatedQoute             = '%d:Unterminated string in expression';
  boeUnKnownIterator               = '%d:Semantics for this iterator has not been implemented';
  boeUnknownContext                = '%d:Unknown type (%s) of Context in SemanticCheck';
  boeUnKnownTypeOfLoopVar          = '%d:Unknown type (%s) of loop variable';
  boeModelIsNoModel                = '%d:For some Reason, the model of the expression is no model...(%s)';
  boeInvalidTypeNameName           = '%d:This Multiple parts in typenames are not implemented';
  boeunknownType                   = '%d:This is not a typename';
  boeunknownTypeButMember          = '%d:%s is not a typename (but it is a member of the current class context, use lowercase!)';
  boeMemberofType                  = '%d:Types can not have members (tried %s.%s)';
  boeMemberofAttr                  = '%d:Attributes can not have members (tried %s.%s)';
  boeEnvSizeError                  = '%d:Started with %d vars, ended up with %d...';
  boeEnumValueNotFound             = '%d:Enum value (%s) not found in any registed ValueSet';
{//}  boeunknownExprtypeinDeduce       = '%d:Unable to deduce type of expression ' + BOLDCRLF +
{//}                                        'Operation: %s ' + BOLDCRLF +
{//}                                        '%s' + BOLDCRLF +
{//}                                        'Using deduce method: %s';

  boeOperationNotOclable           = '%d:Operation %s is not OCLCompatible';


  borteAtIndexOutOfBounds          = '%d:Tried to take element #%d of a list with %d elements';
  borteDivisionByZero              = '%d:Division By Zero';
  borteInvalidCast                 = '%d:Invalid OCL-cast, tried to cast a %s to a %s';
  borteOperationNotDefinedOnArgTypes = '%d:%s %s %s is not defined';
  borteNonClassAttribute           = '%d:Can not take attributes of a non-class (%s)';
  bortePadStringEmpty              = '%d:Trying to pad with an empty string doesn''t really get me anywhere...';
  boeArgrtIsNotList                = '%d:Argument of ''%s'' is not a list';
  boeArgrtIsNottype                = '%d:Argument of ''%s'' is not a type';
  boertRangeNotAssigned            = '%d:Invalid value of range';

type

  EBoldOCLAbort = class(EAbort)
  public
    Position: integer;
    Ocl: String;
    ErrorFixed: Boolean;
    procedure FixError;
    function ErrorPointer: String;
  end;

  EBoldOCLError = class(EBold)
  public
    Position: integer;
    Ocl: String;
    ErrorFixed: Boolean;
    procedure FixError;
    function ErrorPointer: String;
  end;

  EBoldOCLInternalError = class(EBoldOclError);

  EBoldOclRunTimeError = class(EBoldOclError);


implementation


procedure EBoldOClAbort.FixError;
begin
  if not errorFixed then
  begin
    try
      if pos(':', Message) <> -1 then
        Position := StrToInt(Copy(message, 1, pos(':', Message) - 1))
      else
        Position := 0;
    except
      on e: EConvertError do
        Position := 0;
    end;
  end;
  ErrorFixed := true;
end;

function EBoldOClAbort.ErrorPointer: String;
begin
  result := StringOfChar(' ', Position) + '^';
end;

procedure EBoldOClError.FixError;
begin
  if not errorFixed then
  begin
    try
      Position := StrToInt(Copy(message, 1, pos(':', Message) - 1));
    except
      on e: EConvertError do
        Position := 0;
    end;
  end;
  ErrorFixed := true;
end;

function EBoldOClError.ErrorPointer: String;
begin
  result := StringOfChar(' ', Position) + '^';
end;

end.
