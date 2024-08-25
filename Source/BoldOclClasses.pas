
{ Global compiler directives }
{$include bold.inc}
unit BoldOclClasses;

interface

uses
  Classes,
  BoldIndexableList,
  BoldBase,
  BoldSubscription,
  BoldElements,
  BoldSystem,
  BoldSystemRT;

type
  TBOCollectionKind = (OCLSet, OCLBag, OCLSequence);
  TBoldOCLIteratorSpecifier = (OclNoIterator, OclSelect, OclReject, OCLCollect, OclIterate, OclExists, OclForAll, OclOrderBy, OclOrderDescending, OclUnique);

  TBoldOclDeduceMethod = (tbodNo, tbodCopyLoopVar, tbodCopyArg1, tbodCopyArg1Elem,
    tbodCopyArg2, tbodCopyArg3, tbodLCC, tbodLCC23, tbodListofArg2, TbodObjectlist,
    tbodType, tbodTypecast, tbodArg1Type, tbodArg1AsList, tbodListFromArg2);

  TBoldOCL_Operation = procedure (Args: array of TBoldIndirectElement; Result: TBoldIndirectElement);

  TBoldOclNode = class;
  TBoldOclTypeNode = class;
  TBoldOclLIstCoercion = class;
  TBoldOclOperation = class;
  TBoldOclIteration = class;
  TBoldOclMember = class;
  TBoldOclMethod = class;
  TBoldOclVariableReference = class;
  TBoldOclVariableBinding = class;
  TBoldOclLiteral = class;
  TBoldOClCollectionLiteral = class;
  TBoldOclNumericLiteral = class;
  TBoldOclIntLiteral = class;
  TBoldOclStrLiteral = class;
  TBoldOCLMomentLiteral = class;
  TBoldOCLDateLiteral = class;
  TBoldOCLTimeLiteral = class;
  TBoldOclEnumLiteral = class;
  TBoldOclSymbol = class;
  TBoldOClSymbolClass = class of TBoldOclSymbol;
  TBoldSymbolDictionary = class;
  TBoldOclSymbolHelp = class;


{//}  TBoldOclSymbolParameters = record
{//}    Nodes: array[0..10] of TBoldOCLNode;
{//}    Values: array[0..10] of TBoldElement;
{//}    Result: TBoldOCLNode;
{//}    Subscriber: TBoldSubscriber;
{//}    System: TBoldSystem;
{//}    SystemTypeInfo: TBoldSystemTypeInfo;
{//}  end;

  TBoldOclNodeList = array of TBoldOclNode;

  { TBoldOclEnvironment }
  TBoldOclEnvironment = class(TBoldMemoryManagedObject)
  private
    GenSymCounter: Integer;
    fOuterScope: TBoldOclEnvironment;
    fList: TList;
    function GetCount: integer;
    function GetBindings(Index: integer): TBoldOclVariableBinding;
    function GetBindingsAsCommaText: string;
  public
    constructor Create(OuterScope: TBoldOclEnvironment);
    property Count: integer read GetCount;
    destructor Destroy; override;
    procedure pushBinding(B: TBoldOclVariableBinding);
    function popBinding: TBoldOclVariableBinding;
    procedure ReplaceBinding(name: string; Binding: TBoldOclVariableBinding);
    procedure RemoveBinding(Binding: TBoldOclVariableBinding);
    procedure RemoveVariable(Variable: TBoldExternalVariable);
    function Lookup(const S: string): TBoldOclVariableBinding;
    function lookupSelf: TBoldOclVariableBinding;
    function Find(const S: string): TBoldOclVariableBinding;
    function CurrentImplicitVariable: TBoldOclVariableBinding;
    function MakeGenSymName: string;
    property Bindings[Index: integer]: TBoldOclVariableBinding read GetBindings; default;
    property BindingsAsCommaText: string read GetBindingsAsCommaText;
  end;

  { TBoldOclVisitor }
  TBoldOclVisitor = class(TBoldMemoryManagedObject)
  public
    procedure VisitTBoldOclNode(N: TBoldOclNode); virtual;
    procedure VisitTBoldOclListCoercion(N: TBoldOclListCoercion); virtual;
    procedure VisitTBoldOclCollectionLiteral(N: TBoldOclCollectionLIteral); virtual;
    procedure VisitTBoldOclOperation(N: TBoldOclOperation); virtual;
    procedure VisitTBoldOclIteration(N: TBoldOclIteration); virtual;
    procedure VisitTBoldOclMember(N: TBoldOclMember); virtual;
    procedure VisitTBoldOclMethod(N: TBoldOclMethod); virtual;
    procedure VisitTBoldOclLiteral(N: TBoldOclLiteral); virtual;
    procedure VisitTBoldOclStrLiteral(N: TBoldOclStrLiteral); virtual;
    procedure VisitTBoldOclNumericLiteral(N: TBoldOclNumericLiteral); virtual;
    procedure VisitTBoldOclEnumLiteral(N: TBoldOclEnumLiteral); virtual;
    procedure VisitTBoldOclIntLiteral(N: TBoldOclIntLiteral); virtual;
    procedure VisitTBoldOclMomentLiteral(N: TBoldOclMomentLiteral); virtual;
    procedure VisitTBoldOclDateLiteral(N: TBoldOclDateLiteral); virtual;
    procedure VisitTBoldOclTimeLiteral(N: TBoldOclTimeLiteral); virtual;
    procedure VisitTBoldOclVariableBinding(N: TBoldOclVariableBinding); virtual;
    procedure VisitTBoldOclVariableReference(N: TBoldOclVariableReference); virtual;
    procedure VisitTBoldOclTypeNode(N: TBoldOclTypeNode); virtual;
  end;

  { TBoldOclNode }
  TBoldOclNode = class(TBoldIndirectElement)
  protected
    fBoldType: TBoldElementTypeInfo;
    procedure SetBoldType(NewType: TBoldElementTypeInfo);
    function GetBoldType: TBoldElementTypeInfo; virtual;
  public
    Position: Integer;
    constructor Create; virtual;
    procedure AcceptVisitor(V: TBoldOclVisitor); virtual;
    property BoldType: TBoldElementTypeInfo read GetBoldType write SetBoldType;
    property NeedsListCoercion: Boolean index befNeedsListCoercion read GetElementFlag write SetElementFlag;
    property IsConstant: Boolean index befIsConstant read GetElementFlag write SetElementFlag;
    property Resubscribe: boolean index befResubscribe read GetElementFlag write SetElementFlag;
    property HasTemporaryDummyValue: boolean index befHastemporaryDummyValue read GetElementFlag write SetElementFlag;
  end;

  { TBoldOclTypeNode }
  TBoldOclTypeNode = class(TBoldOclNode)
  public
    typeName: String;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOCLListCoercion }
  TBoldOCLListCoercion = class(TBoldOCLNode)
  public
    Child: TBoldOCLNode;
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOclOperation }
  TBoldOclOperation = class(TBoldOclNode)
  public
    IteratorSpecifier: TBoldOclIteratorSpecifier;
    IsMethod: Boolean;
    Args: TBoldOclNodeList;
    Symbol: TBoldOCLSymbol;
    OperationName: string;
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOclIteration }
  TBoldOclIteration = class(TBoldOclOperation)
  public
    LoopVar: TBoldOclVariableBinding;
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOclMember }
  TBoldOclMember = class(TBoldOclNode)
  public
    MemberOf: TBoldOclNode;
    MemberName: string;
    MemberType: TBoldElementTypeInfo;
    MemberIndex: Integer;
    Qualifier: TBoldOclNodeList;
    RTInfo: TBoldMemberRTInfo;
    constructor Create; override;
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOclMethod }
  TBoldOclMethod = class(TBoldOclOperation)
  public
    MethodOf: TBoldOclNode;
    MethodOf_AddedToArgs: Boolean;
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOclVariableBinding }
  TBoldOclVariableBinding = class(TBoldOclNode)
  public
    VariableName: string;
    TypeNameList: TStringList;
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  TBoldOclVariableBindingExternal = class(TBoldOclVariableBinding)
  private
    fExternalVariable: TBoldExternalVariable;
    procedure SetExternalVariable(const Value: TBoldExternalVariable);
  protected
    function GetBoldType: TBoldElementTypeInfo; override;
  public
    destructor Destroy; override;
    property ExternalVariable: TBoldExternalVariable read fExternalVariable write SetExternalVariable;
  end;

  { TBoldOclVariableReference }
  TBoldOclVariableReference = class(TBoldOclNode)
  private
    fSubscriber: TBoldSubscriber;
    fVariableBinding: TBoldOclVariableBinding;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetVariableBinding(const Value: TBoldOclVariableBinding);
  public
    VariableName: string;
    property VariableBinding: TBoldOclVariableBinding read fVariableBinding write SetVariableBinding;
    constructor Create; override;
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOclLiteral }
  TBoldOclLiteral = class(TBoldOclNode)
  public
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOclCollectionLiteral }
  TBoldOclCollectionLiteral = class(TBoldOclLiteral)
  public
    CollectionKind: TBOCollectionKind;
    IsRange: Boolean;
    RangeStart, RangeStop: TBoldOClNode;
    Elements: TBoldOClNodeList;
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOclStrLiteral }
  TBoldOclStrLiteral = class(TBoldOclLiteral)
  public
    StrValue: String;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOclNumericLiteral }
  TBoldOclNumericLiteral = class(TBoldOclLiteral)
  public
    FloatValue: Double;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOclIntLiteral }
  TBoldOclIntLiteral = class(TBoldOclNumericLiteral)
  public
    IntValue: Integer;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOCLMomentLiteral }
  TBoldOCLMomentLiteral = class(TBoldOclLiteral)
  private
    fDateTimeValue: TDateTime;
  public
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOclDateLiteral }
  TBoldOclDateLiteral = class(TBoldOCLMomentLiteral)
  private
    function GetDateTimeValue: TDateTime;
    procedure SetDateTimeValue(const Value: TDateTime);
  public
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
    property DateValue: TDateTime read GetDateTimeValue write SetDateTimeValue;
  end;

  { TBoldOclTimeLiteral }
  TBoldOclTimeLiteral = class(TBoldOCLMomentLiteral)
  private
    function GetDateTimeValue: TDateTime;
    procedure SetDateTimeValue(const Value: TDateTime);
  public
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
    property TimeValue: TDateTime read GetDateTimeValue write SetDateTimeValue;
  end;

  { TBoldOclEnumLiteral }
  TBoldOclEnumLiteral = class(TBoldOclLiteral)
  public
    name: string;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  {---TBoldSymbolDictionary---}
  TBoldSymbolDictionary = class(TBoldIndexableList)
  private
    fhelp: TBoldOclSymbolHelp;
    function GetSymbol(const Name: string): TBoldOclSymbol;
    function GetSymbolByIndex(index: Integer): TBoldOclSymbol;
    class var IX_SymbolName: integer;
  public
    constructor Create(SystemTypeInfo: TBoldSystemTypeInfo; BoldSystem: TBoldSystem; var ErrorsEncountered: Boolean);
    destructor Destroy; override;
    property help: TBoldOclSymbolHelp read fHelp;
    property SymbolByName[const name: string]: TBoldOclSymbol read GetSymbol;
    property Symbols[i: Integer]: TBoldOclSymbol read GetSymbolByIndex;
  end;

  { TBoldOclSymbolHelp }
  TBoldOclSymbolHelp = class(TBoldMemoryManagedObject)
  private
    fNumericType,
    fIntegerType,
    fBooleanType,
    fRealType,
    fDateType,
    fDateTimeType,
    fTimeType,
    fCurrencyType,
    fMomentType,
    fConstraintType,
    fStringType: TBoldAttributeTypeInfo;
    fTypeType: TBoldTypeTypeInfo;
    fObjectType: TBoldClassTypeInfo;
    fListType,
    fObjectListType,
    fNumericListType,
    fMomentListType,
    fStringListType,
    fIntegerListType,
    fTypeListType: TBoldListTypeInfo;
    fSystemTypeInfo: tBoldSystemTypeInfo;
  private
    fEmptyString: TBoldMember;
    fBooleanFalse: TBoldMember;
    fBooleanTrue: TBoldMember;
    fIntegerZero: TBoldMember;
    fCurrencyZero: TBoldMember;
    fFloatZero: TBoldMember;
  public
    constructor Create(ASystemTypeInfo: TBoldSystemTypeInfo; ABoldSystem: TBoldSystem; var AErrorsEncountered: Boolean);
    destructor Destroy; override;
    procedure MakeNew(el: TBoldOCLNode; NewType: TBoldElementTypeInfo);
    function CreateNewMember(BoldType: TBoldElementTypeInfo): TBoldMember;
    property SystemTypeInfo: TBoldSystemTypeInfo read fSystemTypeInfo;
    property NumericType: TBoldAttributeTypeInfo read fNumericType;
    property BooleanType: TBoldAttributeTypeInfo read fBooleanType;
    property IntegerType: TBoldAttributeTypeInfo read fIntegerType;
    property RealType: TBoldAttributeTypeInfo read fRealType;
    property DateType: TBoldAttributeTypeInfo read fDateType;
    property TimeType: TBoldAttributeTypeInfo read fTimeType;
    property DateTimeType: TBoldAttributeTypeInfo read fDateTimeType;
    property CurrencyType: TBoldAttributeTypeInfo read fCurrencyType;
    property MomentType: TBoldAttributeTypeInfo read fMomentType;
    property ConstraintType: TBoldAttributeTypeInfo read fConstraintType;
    property StringType: TBoldAttributeTypeInfo read fStringType;
    property ListType: TBoldListTypeInfo read fListType;
    property ObjectType: TBoldClassTypeInfo read fObjectType;
    property TypeType: TBoldTypeTypeInfo read fTypeType;
    property NumericListType: TBoldListTypeInfo read fNumericListType;
    property MomentListType: TBoldListTypeInfo read fMomentListType;
    property StringListType: TBoldListTypeInfo read fStringListType;
    property IntegerListType: TBoldListTypeInfo read fIntegerListType;
    property TypeListType: TBoldListTypeInfo read fTypeListType;
    property ObjectListType: TBoldListTypeInfo read fObjectListType;
    procedure MakeNewNumeric(El: TBoldOCLNode; value: Double);
    procedure MakeNewTime(El: TBoldOCLNode; value: TDateTime);
    procedure MakeNewDateTime(El: TBoldOCLNode; value: TDateTime);
    procedure MakeNewDate(El: TBoldOCLNode; value: TDateTime);
    procedure MakeNewBoolean(El: TBoldOCLNode; value: Boolean);
    procedure MakeNewInteger(El: TBoldOCLNode; value: integer);
    procedure MakeNewString(El: TBoldOCLNode; const value: string);
    procedure MakeNewCurrency(El: TBoldOCLNode; value: currency);
    procedure MakeNewNull(el: TBoldOCLNode; NewType: TBoldElementTypeInfo);
    procedure TransferOrClone(source, dest: TBoldIndirectElement);
  end;

   ShortCircuitType = (csNone, csAnd, csOr, csIf, csBoldIDIn, csNamedIDIn);

  { TBoldOclSymbol }
  TBoldOclSymbol = class(TBoldMemoryManagedObject)
  private
    fHelp: TBoldOCLSymbolHelp;
    fSymbolName: String;
    fFormalArguments: array of TBoldElementTypeInfo;
    fDeduceMethod: TBoldOclDeduceMethod;
    fResultType: TBoldElementTypeInfo;
    fIsDotNotation: Boolean;
    fIsPostFix: Boolean;
    fHelpContext: Integer;
    fArgsNeedCommonType: Boolean;
  protected
    procedure InternalInit(const Name: string;
                                       Args: array of TBoldElementTypeInfo;
                                       DeduceMethod: TBoldOclDeduceMethod;
                                       resultType: TBoldElementTypeInfo;
                                       IsPostfix: Boolean;
                                       HelpContext: integer;
                                       ArgsNeedCommonType: Boolean = false);
    function GetFormalArguments(index: integer): TBoldElementTypeInfo;
    function GetNumberOfArgs: integer;
    procedure Init; virtual; abstract;
    class function XBoolean(Elem: TBoldElement): Boolean; static;
    class function XCurrency(Elem: TBoldElement): Currency; static;
    class function XInteger(Elem: TBoldElement): Integer; static;
    class function XList(Elem: TBoldElement): tBoldList; static;
    class function XDateTime(Elem: TBoldElement): TDateTime; static;
    class function XNumeric(Elem: TBoldElement): Double; static;
    class function XString(Elem: TBoldElement): String; static;
    class function XType(Elem: TBoldElement): TBoldElementTypeInfo; static;
    property Help: TBoldOclSymbolHelp read fHelp;
  public
    constructor Create(Help: TBoldOclSymbolHelp);
    destructor Destroy; override;
    function GetShortCircuitType: ShortCircuitType; virtual;
    procedure Evaluate(const Params: TBoldOclSymbolParameters); virtual; abstract;
    procedure SQL(const Args: Array of String; var result: String); virtual;
    property DeduceMethod: tBoldOclDeduceMethod read fDeduceMethod;
    property FormalArguments[Index: integer]: TBoldElementTypeInfo read GetFormalArguments;
    property HelpContext: integer read fHelpContext;
    property isDotNotation: Boolean read fIsDotNotation;
    property IsPostfix: Boolean read fIsPostFix;
    property NumberOfArgs: INteger read GetNumberOfArgs;
    property ResultType: TBoldElementTypeInfo read fResultType;
    property SymbolName: string read fSymbolName;
    property ArgsNeedCommonType: Boolean read fArgsNeedCommonType;
  end;

implementation

uses
  System.Types,
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldOclError,
  BoldAttributes,
  BoldHashIndexes;

procedure TBoldOclNode.AcceptVisitor(V: TBoldOclVisitor);
begin
end;

constructor TBoldOclNode.Create;
begin
  inherited create;
  IsConstant := false;
end;

destructor TBoldOclMember.Destroy;
var
  i: Integer;
begin
  FreeAndNil(MemberOf);
  for i := 0 to Length(Qualifier)- 1 do
    Qualifier[i].Free;
  inherited;
end;

destructor TBoldOCLCollectionLiteral.Destroy;
var
  i: Integer;
begin
  FreeAndNil(RangeStart);
  FreeAndNil(RangeStop);
  for i := 0 to Length(Elements) - 1 do
    Elements[i].Free;
  inherited;
end;


destructor TBoldOclOperation.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Args) - 1 do
    Args[I].Free;
  inherited;
end;

destructor TBoldOclVariableBinding.Destroy;
begin
  FreeAndNil(TypeNameList);
  inherited;
end;

destructor TBoldOclIteration.Destroy;
begin
  FreeAndNil(LoopVar);
  inherited Destroy;
end;

destructor TBoldOclMethod.Destroy;
begin
  if not MethodOf_AddedToArgs then
    MethodOf.Free;
  Methodof := nil;
  inherited;
end;

constructor TBoldOclMember.Create;
begin
  inherited;
  RTInfo := nil;
end;



procedure TBoldOclTypeNode.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclTypeNode(self);
end;

procedure TBoldOclListCoercion.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclListCoercion(self);
end;

procedure TBoldOclCollectionLiteral.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclCollectionLIteral(self);
end;

procedure TBoldOclOperation.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclOperation(self);
end;

procedure TBoldOclIteration.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclIteration(self);
end;

procedure TBoldOclMember.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclMember(self);
end;

procedure TBoldOclMethod.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclMethod(self);
end;

procedure TBoldOclLiteral.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclLiteral(self);
end;

procedure TBoldOclStrLiteral.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclStrLiteral(self);
end;

procedure TBoldOclNumericLiteral.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclNumericLiteral(self);
end;

procedure TBoldOclEnumLiteral.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclENumLiteral(self);
end;

procedure TBoldOclIntLiteral.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclIntLiteral(self);
end;

procedure TBoldOclVariableBinding.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclVariableBinding(self);
end;

procedure TBoldOclVariableReference.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclVariableReference(self);
end;

procedure TBoldOclVisitor.VisitTBoldOclNode(N: TBoldOclNode); begin end;
procedure TBoldOclVisitor.VisitTBoldOclTypeNode(N: TBoldOclTypeNode); begin end;
procedure TBoldOclVisitor.VisitTBoldOclListCoercion(N: TBoldOclListCoercion); begin end;
procedure TBoldOclVisitor.VisitTBoldOclCollectionLiteral(N: TBoldOclCollectionLiteral); begin end;
procedure TBoldOclVisitor.VisitTBoldOclOperation(N: TBoldOclOperation); begin end;
procedure TBoldOclVisitor.VisitTBoldOclIteration(N: TBoldOclIteration); begin end;
procedure TBoldOclVisitor.VisitTBoldOclMember(N: TBoldOclMember); begin end;
procedure TBoldOclVisitor.VisitTBoldOclMethod(N: TBoldOclMethod); begin end;
procedure TBoldOclVisitor.VisitTBoldOclLiteral(N: TBoldOclLiteral); begin end;
procedure TBoldOclVisitor.VisitTBoldOclStrLiteral(N: TBoldOclStrLiteral); begin end;
procedure TBoldOclVisitor.VisitTBoldOclNumericLiteral(N: TBoldOclNumericLiteral); begin end;
procedure TBoldOclVisitor.VisitTBoldOclEnumLiteral(N: TBoldOclEnumLiteral); begin end;
procedure TBoldOclVisitor.VisitTBoldOclIntLiteral(N: TBoldOclIntLiteral); begin end;
procedure TBoldOclVisitor.VisitTBoldOclVariableReference(N: TBoldOclVariableReference); begin end;
procedure TBoldOclVisitor.VisitTBoldOclVariableBinding(N: TBoldOclVariableBinding); begin end;
procedure TBoldOclVisitor.VisitTBoldOclDateLiteral(N: TBoldOclDateLiteral); begin end;
procedure TBoldOclVisitor.VisitTBoldOclMomentLiteral(N: TBoldOclMomentLiteral); begin end;
procedure TBoldOclVisitor.VisitTBoldOclTimeLiteral(N: TBoldOclTimeLiteral); begin end;



constructor TBoldOclEnvironment.Create(OuterScope: TBoldOclEnvironment);
begin
  inherited Create;
  GenSymCounter := 0;
  fOuterScope := OuterScope;
  fList := TList.Create;
end;

destructor TBoldOCLEnvironment.Destroy;
var
  tempBinding: TBoldOclVariableBinding;
begin
  while flist.Count <> 0 do
  begin
    TempBinding := PopBinding;
    TempBinding.Free;
  end;
  FreeAndNil(fList);
  inherited;
end;

function TBoldOCLEnvironment.GetCount: integer;
begin
  result := fList.Count;
end;

procedure TBoldOclEnvironment.pushBinding(B: TBoldOclVariableBinding);
begin
  fList.Add(B);
end;

function TBoldOclEnvironment.popBinding: TBoldOclVariableBinding;
begin
  result := Bindings[Count - 1];
  fList.Delete(Count - 1);
end;

procedure TBoldOclEnvironment.ReplaceBinding(name: string;
  Binding: TBoldOclVariableBinding);
var
  i: integer;
begin
  I := fList.Count - 1;
  while I >= 0 do
  begin
    if CompareText(Bindings[I].VariableName, name) = 0 then
    begin
      TObject(fList.Items[i]).free;
      fList.Items[i] := Binding;
      exit;
    end;
    Dec(I);
  end;
end;

procedure TBoldOclEnvironment.RemoveBinding(Binding: TBoldOclVariableBinding);
begin
  fList.Remove(Binding);
end;

procedure TBoldOclEnvironment.RemoveVariable(Variable: TBoldExternalVariable);
var
  i: integer;
begin
  I := fList.Count - 1;
  while I >= 0 do
  begin
    if Bindings[I] is TBoldOclVariableBindingExternal and (TBoldOclVariableBindingExternal(Bindings[I]).ExternalVariable = Variable) then
    begin
      Variable.Evaluator := nil;
      TObject(fList[i]).free;
      fList.Delete(i);
      exit;
    end;
    Dec(I);
  end;
end;

function TBoldOclEnvironment.Find(const S: string): TBoldOclVariableBinding;
var
  I: Integer;
begin
  I := fList.Count - 1;
  while I >= 0 do
  begin
    if CompareText(Bindings[I].VariableName, s) = 0 then
    begin
      Result := Bindings[i];
      exit;
    end;
    Dec(I);
  end;
  if assigned(fOuterScope) then
    result := fOuterScope.Lookup(S)
  else
    Result := nil;
end;

function TBoldOclEnvironment.Lookup(const S: string): TBoldOclVariableBinding;
var
  I: Integer;
  ExternalVariable: TBoldExternalVariable;
begin
  I := fList.Count - 1;
  while I >= 0 do
  begin
    if CompareText(Bindings[I].VariableName, s) = 0 then
    begin
      Result := Bindings[i];
      if result is TBoldOclVariableBindingExternal then
      begin
        ExternalVariable := TBoldOclVariableBindingExternal(result).ExternalVariable;
        if Assigned(ExternalVariable) then
          result.SetReferenceValue(ExternalVariable.Value)
        else
          result.SetReferenceValue(nil);
      end;
      exit;
    end;
    Dec(I);
  end;
  if assigned(fOuterScope) then
    result := fOuterScope.Lookup(S)
  else
    Result := nil;
end;

function TBoldOclEnvironment.lookupSelf: TBoldOclVariableBinding;
begin
  Result := Lookup('SELF');
end;

function TBoldOclEnvironment.CurrentImplicitVariable: TBoldOclVariableBinding;
begin
  Result := Bindings[Count - 1];
end;

function TBoldOclEnvironment.MakeGenSymName: string;
begin
  Result := IntToStr(GenSymCounter) + '#GenSym';
  Inc(GenSymCounter);
end;

function TBoldOclNode.GetBoldType: TBoldElementTypeInfo;
begin
  result := fBoldType;
end;

procedure TBoldOclNode.SetBoldType(NewType: TBoldElementTypeInfo);
begin
  fBoldType := NewType;
end;

constructor TBoldOclSymbolHelp.Create(ASystemTypeInfo: TBoldSystemTypeInfo; ABoldSystem: TBoldSystem; var AErrorsEncountered: Boolean);

  procedure SignalError(const Message: String; const args: array of const);
  begin
    if assigned(ABoldSystem) then
      raise EBoldOclError.CreateFmt(Message, Args);
    AErrorsEncountered := true;
  end;

  procedure InstallAttribute(const Name: String; var AttrTypeInfo: TBoldAttributeTypeInfo; AttrClass: TClass; Exact: Boolean);

  begin
  AttrTypeInfo := SystemTypeInfo.AttributeTypeInfoByExpressionName[Name];
  if not assigned(AttrTypeINfo) then
      SignalError(sMissingOCLType, [Name]);

  if not assigned(AttrTypeInfo.AttributeClass) then
      SignalError(sMissingDelphiType, [Name, AttrTypeInfo.DelphiName])
    else
    begin
    case exact of
      true: if AttrTypeInfo.AttributeClass <> AttrClass then
          SignalError(sTypeMustBeX, [Name, AttrClass.ClassName, AttrTypeInfo.AttributeClass.ClassName]);
      false: if not AttrTypeInfo.AttributeClass.InheritsFrom(AttrClass) then
          SignalError(sTypeMustInheritFromX, [Name, AttrClass.ClassName, AttrTypeInfo.AttributeClass.ClassName]);
    end;
  end;
  end;

begin
  inherited Create;
  fSystemTypeInfo := ASystemTypeInfo;
  fTypeType := SystemTypeInfo.BoldType as TBoldTypeTypeInfo;
  InstallAttribute('Numeric', fNumericType, TBANumeric, true);
  InstallAttribute('Float', fRealType, TBAFloat, false);
  InstallAttribute('String', fStringType, TBAString, false);
  InstallAttribute('Integer', fIntegerType, TBAInteger, false);
  InstallAttribute('Boolean', fBooleanType, TBABoolean, false);
  InstallAttribute('Currency', fCurrencyType, TBACurrency, false);
  InstallAttribute('Moment', fMomentType, TBAMoment, false);
  InstallAttribute('Constraint', fConstraintType, TBAConstraint, false);
  InstallAttribute('Date', fDateType, TBADate, false);
  InstallAttribute('DateTime', fDateTimeType, TBADateTime, false);
  InstallAttribute('Time', fTimeType, TBATime, false);

  fObjectType := SystemTypeInfo.RootClassTypeInfo;
  fListType := SystemTypeInfo.ListTypeInfoByElement[nil];
  fNumericListType := SystemTypeInfo.ListTypeInfoByElement[NumericType];
  fMomentListType := SystemTypeInfo.ListTypeInfoByElement[MomentType];
  fTypeListType := SystemTypeInfo.ListTypeInfoByElement[TypeType];
  fObjectListType := SystemTypeInfo.ListTypeInfoByElement[ObjectType];
  fStringListType := SystemTypeInfo.ListTypeInfoByElement[StringType];
  fIntegerListType := SystemTypeInfo.ListTypeInfoByElement[IntegerType];

  fEmptyString := CreateNewMember(StringType);
  fEmptyString.AsVariant := '';
  fBooleanFalse := CreateNewMember(BooleanType);
  fBooleanFalse.AsVariant := false;
  fBooleanTrue := CreateNewMember(BooleanType);
  fBooleanTrue.AsVariant := true;
  fIntegerZero := CreateNewMember(IntegerType);
  fIntegerZero.AsVariant := 0;
  fCurrencyZero := CreateNewMember(CurrencyType);
  fCurrencyZero.AsVariant := 0;
  fFloatZero := CreateNewMember(RealType);
  fFloatZero.AsVariant := 0;
end;

procedure TBoldOclSymbolHelp.MakeNew(el: TBoldOCLNode; NewType: TBoldElementTypeInfo);
begin
  if el.OwnsValue and assigned(el.Value) then
    TBoldAttribute(el.Value).RecycleValue
  else
    el.SetOwnedValue(CreateNewMember(NewType));
end;

procedure TBoldOclSymbolHelp.MakeNewNull(el: TBoldOCLNode;
  NewType: TBoldElementTypeInfo);
begin
  MakeNew(el, NewType);
  if (el.Value is TBoldAttribute) then
    (el.Value as TBoldAttribute).SetToNull;
end;

procedure TBoldOclSymbolHelp.MakeNewNumeric(El: TBoldOCLNode; value: Double);
begin
  El.SetOwnedValue(fFloatZero.Clone);
  if Value <> 0 then
  TBAFloat(El.Value).AsFloat := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewCurrency(El: TBoldOCLNode; value: currency);
begin
  El.SetOwnedValue(fCurrencyZero.Clone);
  if Value <> 0 then
  TBACurrency(El.Value).Ascurrency := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewString(El: TBoldOCLNode; const value: String);
begin
  El.SetOwnedValue(fEmptyString.Clone);
  if value <> '' then
  TBAString(El.Value).AsString := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewInteger(El: TBoldOCLNode; value: integer);
begin
  El.SetOwnedValue(fIntegerZero.Clone);
  if Value <> 0 then
  TBAInteger(El.Value).AsInteger := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewTime(El: TBoldOCLNode; value: TDateTime);
begin
  MakeNew(el, TimeType);
  Assert(El.Value is TBATime);
  TBATime(El.Value).AsTime := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewDateTime(El: TBoldOCLNode; value: TDateTime);
begin
  MakeNew(el, DateTimeType);
  Assert(El.Value is TBADateTime);
  TBADateTime(El.Value).AsDateTime := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewDate(El: TBoldOCLNode; value: TDateTime);
begin
  MakeNew(el, DateType);
  Assert(El.Value is TBADate);
  TBADate(El.Value).AsDate := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewBoolean(El: TBoldOCLNode; value: Boolean);
begin
  if Value then
    El.SetOwnedValue(fBooleanTrue.Clone)
  else
    El.SetOwnedValue(fBooleanFalse.Clone);
end;

procedure TBoldOclSymbolHelp.TransferorClone(source, dest: TBoldIndirectElement);
begin
  if not assigned(Source.Value) then
    Dest.SetReferenceValue(nil)
  else if source.OwnsValue and Source.Value.Mutable then
    Source.transferValue(Dest)
  else if Source.Value is TBoldMember then
    Dest.SetOwnedValue(TBoldMember(Source.Value).Clone)
  else
    raise EBoldInternal.CreateFmt('TransferOrClone: Unable to clone Object of type %s', [Source.Value.ClassName]);
end;

{-- TBoldOclSymbol -- }

procedure TBoldOclSymbol.InternalInit(const Name: string;
                                       Args: array of TBoldElementTypeInfo;
                                       DeduceMethod: TBoldOclDeduceMethod;
                                       resultType: TBoldElementTypeInfo;
                                       IsPostfix: Boolean;
                                       HelpContext: Integer;
                                       ArgsNeedCommonType: Boolean = false);
var
  i:Integer;
begin
  fSymbolName := Name;
  fHelpContext := HelpContext;
  SetLength(fFormalArguments, Length(args));
  for i := 0 to Length(args) - 1 do
    fFormalArguments[i] := args[i];
  if assigned(ResultType) then
    fResultType := ResultType
  else
    fDeduceMethod := DeduceMethod;
  fIsPostfix := IsPostfix;
  fArgsNeedCommonType := ArgsNeedCommonType;
end;

function TBoldOclSymbol.GetFormalArguments(index: integer): TBoldElementTypeInfo;
begin
  result := fFormalArguments[index];
end;

function TBoldOclSymbol.GetNumberOfArgs: integer;
begin
  result := Length(fFormalArguments);
end;

function TBoldOclSymbol.GetShortCircuitType: ShortCircuitType;
begin
  Result := csNone;
end;

procedure TBoldOclSymbol.SQL(const Args: Array of String; var result: String);
begin
  result := '';
end;

constructor TBoldOclSymbol.Create(Help: TBoldOclSymbolHelp);
begin
  Inherited create;
  fHelp := help;
  Init;
  fIsDotNotation := isPostFix and
                    (not assigned(FormalArguments[0]) or
                     (FormalArguments[0].BoldValueType <> bvtList));
end;

destructor TBoldOclSymbol.Destroy;
begin
  inherited;
end;

class function TBoldOclSymbol.XBoolean(Elem: TBoldElement): Boolean;
begin
  if elem is TBABoolean  then
    result := TBABoolean(Elem).AsBoolean
  else
    result := false;
end;

class function TBoldOclSymbol.XInteger(Elem: TBoldElement): Integer;
begin
  if (elem is TBAInteger) and not TBAInteger(elem).IsNull then
    result := TBAInteger(Elem).AsInteger
  else
    result := 0;
end;

class function TBoldOclSymbol.XDateTime(Elem: TBoldElement): TDateTime;
begin
  if elem is TBADate then
    result := TBADate(Elem).AsDate
  else if elem is TBADateTime then
    result := TBADateTime(Elem).AsDateTime
  else if elem is TBATime then
    result := TBATime(Elem).AsTime
  else
    result := 0;
end;

class function TBoldOclSymbol.XNumeric(Elem: TBoldElement): Double;
begin
  if elem is TBANumeric then
    result := TBANumeric(Elem).AsFloat
  else
    result := 0;
end;

class function TBoldOclSymbol.Xcurrency(Elem: TBoldElement): currency;
begin
  if elem is TBACurrency then
    result := TBACurrency(Elem).Ascurrency
  else
    result := 0;
end;

class function TBoldOclSymbol.XString(Elem: TBoldElement): String;
begin
  if assigned(elem) then
    result := Elem.AsString
  else
    result := '';
end;

class function TBoldOclSymbol.XType(Elem: TBoldElement): TBoldElementTypeInfo;
begin
  if elem is TBoldElementTypeInfo then
    result := TBoldElementTypeInfo(Elem)
  else
    result := nil;
end;

class function TBoldOclSymbol.XList(Elem: TBoldElement): tBoldList;
begin
  if elem is TBoldList then
    result := TBoldList(Elem)
  else
    result := nil;
end;

type
  {---TSymbolNameIndex---}
  TSymbolNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  {---TSymbolNameIndex---}
function TSymbolNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TBoldOclSymbol(Item).Symbolname;
end;

{---TBoldSymbolDictionary---}
constructor TBoldSymbolDictionary.Create(SystemTypeInfo: TBoldSystemTypeInfo; BoldSystem: TBoldSystem; var ErrorsEncountered: Boolean);
begin
  inherited create;
  SetIndexCapacity(1);
  fHelp := TBoldOclSymbolHelp.Create(SystemTypeInfo, BoldSystem, ErrorsEncountered);
  SetIndexVariable(IX_SymbolName, AddIndex(TSymbolNameIndex.Create));
end;

destructor TBoldSymbolDictionary.Destroy;
begin
  FreeAndNil(fHelp);
  inherited;
end;

function TBoldSymbolDictionary.GetSymbol(const Name: string): TBoldOclSymbol;
begin
  Result := TBoldOclSymbol(TBoldStringHashIndex(indexes[IX_SymbolName]).FindByString(Name));
end;

function TBoldSymbolDictionary.GetSymbolByIndex(index: Integer): TBoldOclSymbol;
begin
  Result := TBoldOclSymbol(Items[index]);
end;

function TBoldOclSymbolHelp.CreateNewMember(
  BoldType: TBoldElementTypeInfo): TBoldMember;
begin
  result := TBoldMemberFactory.CreateMemberFromBoldType(BoldType);
  if (result is TBoldObjectList) then
     TBoldObjectList(result).SubscribeToObjectsInList := false;
end;

destructor TBoldOclSymbolHelp.Destroy;
begin
  FreeAndNil(fEmptyString);
  FreeAndNil(fBooleanFalse);
  FreeAndNil(fBooleanTrue);
  FreeAndNil(fIntegerZero);
  FreeAndNil(fCurrencyZero);
  FreeAndNil(fFloatZero);
  inherited;
end;

function TBoldOclEnvironment.GetBindings(
  Index: integer): TBoldOclVariableBinding;
begin
  result := TBoldOclVariableBinding(fList[index])
end;

function TBoldOclEnvironment.GetBindingsAsCommaText: string;
var
  i: integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  for I := 0 to Count - 1 do
    Sl.Add(Bindings[i].VariableName);
  result := sl.commaText;
  sl.free;
end;

{ TBoldOCLMomentLiteral }

procedure TBoldOCLMomentLiteral.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclMomentLiteral(self);
end;

{ TBoldOclDateLiteral }

procedure TBoldOclDateLiteral.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclDateLiteral(self);
end;

function TBoldOclDateLiteral.GetDateTimeValue: TDateTime;
begin
  result := fDateTimeValue;
end;

procedure TBoldOclDateLiteral.SetDateTimeValue(const Value: TDateTime);
begin
  fDateTimeValue := value;
end;

{ TBoldOclTimeLiteral }

procedure TBoldOclTimeLiteral.AcceptVisitor(V: TBoldOclVisitor);
begin
  V.VisitTBoldOclTimeLiteral(self);
end;

function TBoldOclTimeLiteral.GetDateTimeValue: TDateTime;
begin
  result := fDateTimeValue;
end;

procedure TBoldOclTimeLiteral.SetDateTimeValue(const Value: TDateTime);
begin
  fDateTimeValue := Value;
end;

destructor TBoldOCLListCoercion.Destroy;
begin
  FreeAndNil(Child);
  inherited;
end;

{ TBoldOclVariableBindingExternal }

destructor TBoldOclVariableBindingExternal.Destroy;
begin
  inherited;
end;

function TBoldOclVariableBindingExternal.GetBoldType: TBoldElementTypeInfo;
begin
  if Assigned(ExternalVariable) then  
    result := ExternalVariable.ValueType
  else
    result := nil;
end;

procedure TBoldOclVariableBindingExternal.SetExternalVariable(
  const Value: TBoldExternalVariable);
begin
  fExternalVariable := Value;
end;

{$WARNINGS OFF}
procedure InitDebugMethods;
var
  env: TBoldOclEnvironment;
begin
  Exit;
  env.BindingsAsCommaText;     // This is used to force compiler to include BindingsAsCommaText
end;
{$WARNINGS ON}

constructor TBoldOclVariableReference.Create;
begin
  inherited;
  fSubscriber := TBoldPassthroughSubscriber.Create(Receive);
end;

destructor TBoldOclVariableReference.Destroy;
begin
  FreeAndNil(fSubscriber);
  inherited;
end;

procedure TBoldOclVariableReference.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if Originator = VariableBinding then
    FreeAndNil(VariableBinding);
end;

procedure TBoldOclVariableReference.SetVariableBinding(
  const Value: TBoldOclVariableBinding);
begin
  fVariableBinding := Value;
end;

initialization
  TBoldSymbolDictionary.IX_SymbolName := -1;
  InitDebugMethods;

end.
