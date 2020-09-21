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
    tbodType, tbodTypecast, tbodArg1AsList, tbodListFromArg2);

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


{ TBoldOclNodeList }
  TBoldOclNodeList = class(TBoldMemoryManagedObject)
  private
    FList: TList;
    function GetItem(index: Integer): TBoldOclNode;
    procedure PutItem(index: Integer; Value: TBoldOclNode);
  public
    property Items[index: Integer]: TBoldOclNode read GetItem write PutItem; default;
    constructor Create;
    destructor Destroy; override;
    function Add(Item: TBoldOclNode): Integer;
    procedure Clear;
    procedure ClearAndFree;
    procedure Delete(index: Integer);
    procedure Insert(index: Integer; Item: TBoldOclNode);
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: TBoldOclNode): Integer;
    function Count: Integer;
  end;

  { TBoldOclEnvironment }
  TBoldOclEnvironment = class(TBoldMemoryManagedObject)
  private
    GenSymCounter: Integer;
    fOuterScope: TBoldOclEnvironment;
    fList: TList;
    function GetCount: integer;
    function GetBindings(Index: integer): TBoldOclVariableBinding;
  public
    constructor Create(OuterScope: TBoldOclEnvironment);
    property Count: integer read GetCount;
    destructor Destroy; override;
    procedure pushBinding(B: TBoldOclVariableBinding);
    function popBinding: TBoldOclVariableBinding;
    function Lookup(S: string): TBoldOclVariableBinding;
    function lookupSelf: TBoldOclVariableBinding;
    function CurrentImplicitVariable: TBoldOclVariableBinding;
    function MakeGenSymName: string;
    property Bindings[Index: integer]: TBoldOclVariableBinding read GetBindings; default;
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
    function GetBoldType: TBoldElementTypeInfo;
  public
    Position: Integer;
    constructor Create;
    procedure AcceptVisitor(V: TBoldOclVisitor); virtual;
    property BoldType: TBoldElementTypeInfo read GetBoldType write SetBoldType;
    property NeedsListCoercion: Boolean index befNeedsListCoercion read GetElementFlag write SetElementFlag;
    property IsConstant: Boolean index befIsConstant read GetElementFlag write SetElementFlag;
    property Resubscribe: boolean index befResubscribe read GetElementFlag write SetElementFlag;
    property HastemporaryDummyValue: boolean index befHastemporaryDummyValue read GetElementFlag write SetElementFlag;
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
    constructor Create;
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
    TypeNameList: TStringList; //For temporary use only. The parser stores the type
    //info from a select(i:Person|i<>self.employer)
    //but this must be moved to the oclNode.ExpressionType
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldOclVisitor); override;
  end;

  { TBoldOclVariableReference }
  TBoldOclVariableReference = class(TBoldOclNode)
  public
    VariableName: string;
    VariableBinding: TBoldOclVariableBinding; // Not to be traversed...
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
    fTypeListType: TBoldListTypeInfo;
    fSystemTypeInfo: tBoldSystemTypeInfo;
    procedure fMakeNew(el: TBoldIndirectElement; NewType: TBoldElementTypeInfo);
  public
    constructor create(SystemTypeInfo: tBoldSystemTypeInfo; BoldSystem: TBoldSystem; var ErrorsEncountered: Boolean);
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
    property TypeListType: TBoldListTypeInfo read fTypeListType;
    property ObjectListType: TBoldListTypeInfo read fObjectListType;
    procedure MakeNewNumeric(El: TBoldIndirectElement; value: Double);
    procedure MakeNewTime(El: TBoldIndirectElement; value: TDateTime);
    procedure MakeNewDateTime(El: TBoldIndirectElement; value: TDateTime);
    procedure MakeNewDate(El: TBoldIndirectElement; value: TDateTime);
    procedure MakeNewBoolean(El: TBoldIndirectElement; value: Boolean);
    procedure MakeNewInteger(El: TBoldIndirectElement; value: integer);
    procedure MakeNewString(El: TBoldIndirectElement; const value: string);
    procedure MakeNewCurrency(El: TBoldIndirectElement; value: currency);
    procedure TransferOrClone(source, dest: TBoldIndirectElement);
  end;

  { TBoldOclSymbol }
  TBoldOclSymbol = class(TBoldMemoryManagedObject)
  private
    fHelp: TBoldOCLSymbolHelp;
    fSymbolName: String;
    fFormalArguments: TList;
    fDeduceMethod: TBoldOclDeduceMethod;
    fResultType: TBoldElementTypeInfo;
    fIsDotNotation: Boolean;
    fIsPostFix: Boolean;
    fHelpContext: Integer;
    fArgsNeedCommonType: Boolean;
  protected
    procedure InternalInit(const Name: string;
                                       const Args: array of TBoldElementTypeInfo;
                                       DeduceMethod: TBoldOclDeduceMethod;
                                       resultType: TBoldElementTypeInfo;
                                       IsPostfix: Boolean;
                                       HelpContext: integer;
                                       ArgsNeedCommonType: Boolean = false);
    function GetFormalArguments(index: integer): TBoldElementTypeInfo;
    function GetNumberOfArgs: integer;
    procedure Init; virtual; abstract;
    function XBoolean(Elem: TBoldElement): Boolean;
    function XCurrency(Elem: TBoldElement): Currency;
    function XInteger(Elem: TBoldElement): Integer;
    function XList(Elem: TBoldElement): tBoldList;
    function XDateTime(Elem: TBoldElement): TDateTime;
    function XNumeric(Elem: TBoldElement): Double;
    function XString(Elem: TBoldElement): String;
    function XType(Elem: TBoldElement): TBoldElementTypeInfo;
    property Help: TBoldOclSymbolHelp read fHelp;
  public
    constructor Create(Help: TBoldOclSymbolHelp);
    destructor Destroy; override;
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
  SysUtils,
  BoldHashIndexes,
  BoldDefs,
  BoldOclError,
  BoldAttributes,
  BoldCoreConsts;

var
  IX_SymbolName: integer = -1;

constructor TBoldOclNodeList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TBoldOclNodeList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TBoldOclNodeList.GetItem(index: Integer): TBoldOclNode;
begin
  Assert((not Assigned(FList[index])) or (tObject(FList[index]) is TBoldOCLNode));
  Result := TBoldOCLNode(FList[index]);
end;

procedure TBoldOclNodeList.PutItem(index: Integer; Value: TBoldOclNode);
begin
  FList[index] := Value;
end;

function TBoldOclNodeList.Add(Item: TBoldOclNode): Integer;
begin
  Result := FList.Add(Item);
end;

procedure TBoldOclNodeList.Clear;
begin
  FList.Clear;
end;

procedure TBoldOclNodeList.ClearAndFree;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    Items[I].Free;
    items[i] := nil;
  end;
  FList.Clear;
end;

procedure TBoldOclNodeList.Delete(index: Integer);
begin
  FList.Delete(index);
  FList.Pack;
end;

procedure TBoldOclNodeList.Insert(index: Integer; Item: TBoldOclNode);
begin
  FList.Insert(index, Item);
end;

procedure TBoldOclNodeList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

function TBoldOclNodeList.Remove(Item: TBoldOclNode): Integer;
begin
  Result := FList.Remove(Item);
end;

function TBoldOclNodeList.Count: Integer;
begin
  Result := FList.Count
end;

//=======================================================================
//== TBoldOCLNodes and decendants constructors and destructors
//=======================================================================

procedure TBoldOclNode.AcceptVisitor(V: TBoldOclVisitor);
begin
  // Do nothing
end;

constructor TBoldOclNode.Create;
begin
  inherited create;
  IsConstant := false;
end;

destructor TBoldOclMember.Destroy;
begin
  FreeAndNil(MemberOf);
  if assigned(Qualifier) then
    Qualifier.Clearandfree;
  FreeAndNil(Qualifier);
  inherited;
end;

destructor TBoldOCLCollectionLiteral.Destroy;
begin
  FreeAndNil(RangeStart);
  FreeAndNil(RangeStop);
  if assigned(elements) then
  begin
    Elements.Clearandfree;
    FreeAndNil(Elements)
  end;
  inherited;
end;


destructor TBoldOclOperation.Destroy;
var I: Integer;
begin
  if assigned(Args) then
  begin
    for I := 0 to Args.Count - 1 do
    begin
      TBoldOclNode(Args[I]).Free;
      Args[I] := nil;
    end;
    FreeAndNil(Args);
  end;
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

//=======================================================================
//== TBoldOCLNode and descendants Visitor mechanism
//=======================================================================

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
procedure TBoldOclVisitor.VisitTBoldOclENumLiteral(N: TBoldOclEnumLiteral); begin end;
procedure TBoldOclVisitor.VisitTBoldOclIntLiteral(N: TBoldOclIntLiteral); begin end;
procedure TBoldOclVisitor.VisitTBoldOclVariableReference(N: TBoldOclVariableReference); begin end;
procedure TBoldOclVisitor.VisitTBoldOclVariableBinding(N: TBoldOclVariableBinding); begin end;
procedure TBoldOclVisitor.VisitTBoldOclDateLiteral(N: TBoldOclDateLiteral); begin end;
procedure TBoldOclVisitor.VisitTBoldOclMomentLiteral(N: TBoldOclMomentLiteral); begin end;
procedure TBoldOclVisitor.VisitTBoldOclTimeLiteral(N: TBoldOclTimeLiteral); begin end;


//=======================================================================
//== TBoldOCLEnvironment
//=======================================================================

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
  b.variableName := uppercase(b.variableName);
  fList.Add(B);
end;

function TBoldOclEnvironment.popBinding: TBoldOclVariableBinding;
begin
  result := Bindings[Count - 1];
  fList.Delete(Count - 1);
end;

function TBoldOclEnvironment.Lookup(S: string): TBoldOclVariableBinding;
var I: Integer;
begin
  I := fList.Count - 1;
  s := uppercase(s);
  while I >= 0 do
  begin
    if Bindings[I].VariableName = S then
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

function TBoldOclEnvironment.lookupSelf: TBoldOclVariableBinding;
begin
  Result := Lookup('SELF'); // do not localize
end;

function TBoldOclEnvironment.CurrentImplicitVariable: TBoldOclVariableBinding;
begin
  Result := Bindings[Count - 1];
end;

function TBoldOclEnvironment.MakeGenSymName: string;
begin
  Result := IntToStr(GenSymCounter) + '#GenSym'; // do not localize
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

constructor TBoldOclSymbolHelp.create(SystemTypeInfo: tBoldSystemTypeInfo; BoldSystem: TBoldSystem; var ErrorsEncountered: Boolean);

procedure SignalError(const Message: String; const args: array of const);
begin
  if assigned(BoldSystem) then
    raise EBoldOclError.CreateFmt(Message, Args);
  ErrorsEncountered := true;
end;

procedure InstallAttribute(const Name: String; var AttrTypeInfo: TBoldAttributeTypeInfo; AttrClass: TClass; Exact: Boolean);

begin
  AttrTypeInfo := SystemTypeInfo.AttributeTypeInfoByExpressionName[Name];
  if not assigned(AttrTypeINfo) then
    SignalError(sMissingOCLType, [Name]);

  if not assigned(AttrTypeInfo.AttributeClass) then
    SignalError(sMissingDelphiType, [Name, AttrTypeInfo.DelphiName])
  else begin
    case exact of
      true: if AttrTypeInfo.AttributeClass <> AttrClass then
        SignalError(sTypeMustBeX, [Name, AttrClass.ClassName, AttrTypeInfo.AttributeClass.ClassName]);
      false: if not AttrTypeInfo.AttributeClass.InheritsFrom(AttrClass) then
        SignalError(sTypeMustInheritFromX, [Name, AttrClass.ClassName, AttrTypeInfo.AttributeClass.ClassName]);
    end;
  end;
end;

begin
  inherited create;
  fSystemTypeInfo := SystemTypeInfo;
  fTypeType := SystemTypeInfo.BoldType as TBoldTypeTypeInfo;
  InstallAttribute('Numeric', fNumericType, TBANumeric, true); // do not localize
  InstallAttribute('Float', fRealType, TBAFloat, false); // do not localize
  InstallAttribute('String', fStringType, TBAString, false); // do not localize
  InstallAttribute('Integer', fIntegerType, TBAInteger, false); // do not localize
  InstallAttribute('Boolean', fBooleanType, TBABoolean, false); // do not localize
  InstallAttribute('Currency', fCurrencyType, TBACurrency, false); // do not localize
  InstallAttribute('Moment', fMomentType, TBAMoment, false); // do not localize
  InstallAttribute('Constraint', fConstraintType, TBAConstraint, false); // do not localize
  InstallAttribute('Date', fDateType, TBADate, false); // do not localize
  InstallAttribute('DateTime', fDateTimeType, TBADateTime, false); // do not localize
  InstallAttribute('Time', fTimeType, TBATime, false); // do not localize

  fObjectType := SystemTypeInfo.RootClassTypeInfo;
  fListType := SystemTypeInfo.ListTypeInfoByElement[nil];
  fNumericListType := SystemTypeInfo.ListTypeInfoByElement[NumericType];
  fMomentListType := SystemTypeInfo.ListTypeInfoByElement[MomentType];
  fTypeListType := SystemTypeInfo.ListTypeInfoByElement[TypeType];
  fObjectListType := SystemTypeInfo.ListTypeInfoByElement[ObjectType];
  fStringListType := SystemTypeInfo.ListTypeInfoByElement[StringType];
end;

procedure TBoldOclSymbolHelp.fMakeNew(el: TBoldIndirectElement; NewType: TBoldElementTypeInfo);
begin
  // if we do not own it, or it is not there, then create new element
  if not el.OwnsValue or not assigned(el.Value) then
  begin
    if (el is TBoldOCLNode) and
      assigned(TBoldOclNode(el).BoldType) and
      TBoldOclNode(el).BoldType.ConformsTo(NewType) then
      el.SetOwnedValue(CreateNewMember(TBoldOclNode(el).BoldType))
    else
      el.SetOwnedValue(CreateNewMember(NewType))
  end
  else
    // always recycle the value, we must tell the rest of the world that this is now a new vlaue.
    if el.value is TBoldAttribute then
      TBoldAttribute(el.Value).RecycleValue;
end;

procedure TBoldOclSymbolHelp.MakeNewNumeric(El: TBoldIndirectElement; value: Double);
begin
  fMakeNew(el, RealType);
  Assert(El.Value is TBAFloat);
  TBAFloat(El.Value).AsFloat := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewCurrency(El: TBoldIndirectElement; value: currency);
begin
  fMakeNew(el, CurrencyType);
  Assert(El.Value is TBACurrency);
  TBACurrency(El.Value).Ascurrency := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewString(El: TBoldIndirectElement; const value: String);
begin
  fMakeNew(el, StringType);
  Assert(El.Value is TBAString);
  TBAString(El.Value).AsString := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewInteger(El: TBoldIndirectElement; value: integer);
begin
  fMakeNew(el, Integertype);
  Assert(El.Value is TBAInteger);
  TBAInteger(El.Value).AsInteger := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewTime(El: TBoldIndirectElement; value: TDateTime);
begin
  fMakeNew(el, TimeType);
  Assert(El.Value is TBATime);
  TBATime(El.Value).AsTime := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewDateTime(El: TBoldIndirectElement; value: TDateTime);
begin
  fMakeNew(el, DateTimeType);
  Assert(El.Value is TBADateTime);
  TBADateTime(El.Value).AsDateTime := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewDate(El: TBoldIndirectElement; value: TDateTime);
begin
  fMakeNew(el, DateType);
  Assert(El.Value is TBADate);
  TBADate(El.Value).AsDate := Value;
end;

procedure TBoldOclSymbolHelp.MakeNewBoolean(El: TBoldIndirectElement; value: Boolean);
begin
  fMakeNew(el, BooleanType);
  Assert(El.Value is TBABoolean);
  TBABoolean(El.Value).AsBoolean := Value;
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
                                       const Args: array of TBoldElementTypeInfo;
                                       DeduceMethod: TBoldOclDeduceMethod;
                                       resultType: TBoldElementTypeInfo;
                                       IsPostfix: Boolean;
                                       HelpContext: Integer;
                                       ArgsNeedCommonType: Boolean = false);
var
  i: integer;
begin
  fSymbolName := Name;
  fHelpContext := HelpContext;
  for i := 0 to High(Args) do
    fFormalArguments.Add(Args[i]);

  if assigned(ResultType) then
    fResultType := ResultType
  else
    fDeduceMethod := DeduceMethod;
  fIsPostfix := IsPostfix;
  fArgsNeedCommonType := ArgsNeedCommonType;
end;

function TBoldOclSymbol.GetFormalArguments(index: integer): TBoldElementTypeInfo;
begin
  result := TBoldElementTypeInfo(fFormalArguments[index]);
end;

function TBoldOclSymbol.GetNumberOfArgs: integer;
begin
  result := fFormalArguments.Count;
end;

procedure TBoldOclSymbol.SQL(const Args: Array of String; var result: String);
begin
  result := '';
end;

constructor TBoldOclSymbol.Create(Help: TBoldOclSymbolHelp);
begin
  Inherited create;
  fHelp := help;
  fFormalArguments := TList.create;
  Init;
  fIsDotNotation := isPostFix and
                    (not assigned(FormalArguments[0]) or
                     (FormalArguments[0].BoldValueType <> bvtList));
end;

destructor TBoldOclSymbol.Destroy;
begin
  FreeAndNil(fFormalArguments);
  inherited;
end;

function TBoldOclSymbol.XBoolean(Elem: TBoldElement): Boolean;
begin
  if elem is TBABoolean  then
    result := TBABoolean(Elem).AsBoolean
  else
    result := false;
end;

function TBoldOclSymbol.XInteger(Elem: TBoldElement): Integer;
begin
  if (elem is TBAInteger) and not TBAInteger(elem).IsNull then
    result := TBAInteger(Elem).AsInteger
  else
    result := 0;
end;

function TBoldOclSymbol.XDateTime(Elem: TBoldElement): TDateTime;
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

function TBoldOclSymbol.XNumeric(Elem: TBoldElement): Double;
begin
  if elem is TBANumeric then
    result := TBANumeric(Elem).AsFloat
  else
    result := 0;
end;

function TBoldOclSymbol.Xcurrency(Elem: TBoldElement): currency;
begin
  if elem is TBACurrency then
    result := TBACurrency(Elem).Ascurrency
  else
    result := 0;
end;

function TBoldOclSymbol.XString(Elem: TBoldElement): String;
begin
  if assigned(elem) then
    result := Elem.AsString
  else
    result := '';
end;

function TBoldOclSymbol.XType(Elem: TBoldElement): TBoldElementTypeInfo;
begin
  if elem is TBoldElementTypeInfo then
    result := TBoldElementTypeInfo(Elem)
  else
    result := nil;
end;

function TBoldOclSymbol.XList(Elem: TBoldElement): tBoldList;
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
  Result := TBoldOclSymbol(TSymbolNameIndex(indexes[IX_SymbolName]).FindByString(Name));;
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

function TBoldOclEnvironment.GetBindings(
  Index: integer): TBoldOclVariableBinding;
begin
  result := TBoldOclVariableBinding(fList[index])
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

end.
