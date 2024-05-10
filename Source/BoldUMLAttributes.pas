
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLAttributes;

interface

uses
  Classes, 
  BoldAttributes,
  BoldDefs,
  BoldUMLTypes;

type

  TSeverity = (sNone, sHint, sWarning, sError);

  TBAChangeableKind = class(TBAValueSet)
  private
    function GetAsChangeableKind: TChangeableKind;
    procedure SetAsChangeableKind(ChangeableKind: TChangeableKind);
  public
    class function GetValues: TBAValueSetValueList; override;
    property AsChangeableKind: TChangeableKind read GetAsChangeableKind write SetAsChangeableKind;
  end;



  TBAVisibilityKind = class(TBAValueSet)
  private
    function GetAsVisibilityKind: TVisibilityKind;
    procedure SetAsVisibilityKind(VisibilityKind: TVisibilityKind);
  public
    class function GetValues: TBAValueSetValueList; override;
    property AsVisibilityKind: TVisibilityKind read GetAsVisibilityKind write SetAsVisibilityKind;
  end;

  TBAAggregationKind = class(TBAValueSet)
  private
    function GetAsAggregationKind: TAggregationKind;
    procedure SetAsAggregationKind(AggregationKind: TAggregationKind);
  public
    class function GetValues: TBAValueSetValueList; override;
    property AsAggregationKind: TAggregationKind read GetAsAggregationKind write SetAsAggregationKind;
  end;

  TBAScopeKind = class(TBAValueSet)
  private
    function getAsScopeKind: TScopeKind;
    procedure setAsScopeKind(ScopeKind: TScopeKind);
  public
    class function GetValues: TBAValueSetValueList; override;
    property AsScopeKind: TScopeKind read getAsScopeKind write setAsScopeKind;
  end;

  TBAParameterDirectionKind = class(TBAValueSet)
  private
    function getAsParameterDirectionKind: TBoldParameterDirectionKind;
    procedure setAsParameterDirectionKind(ParameterDirectionKind: TBoldParameterDirectionKind);
  public
    class function GetValues: TBAValueSetValueList; override;
    property AsParameterDirectionKInd: TBoldParameterDirectionKind read getAsParameterDirectionKind write setAsParameterDirectionKind;
  end;

  TBASeverity = class(TBAValueSet)
  private
    function getAsSeverity: TSeverity;
    procedure setAsSeverity(Severity: TSeverity);
  public
    class function GetValues: TBAValueSetValueList; override;
    property AsSeverity: TSeverity read getAsSeverity write setAsSeverity;
  end;

  TBAOrderingKind = class(TBAValueSet)
  private
    function GetAsOrderingKind: TOrderingKind;
    procedure SetAsOrderingKind(OrderingKind: TOrderingKind);
  public
    class function GetValues: TBAValueSetValueList; override;
    property AsOrderingKind: TOrderingKind read getAsOrderingKind write setAsOrderingKind;
  end;

  TBAPseudostateKind = class(TBAValueSet)
  private
    function GetAsPseudostateKind: TPseudostateKind;
    procedure SetAsPseudostateKind(PseudostateKind: TPseudostateKind);
  public
    class function GetValues: TBAValueSetValueList; override;
    property AsPseudostateKind: TPseudostateKind read getAsPseudostateKind write setAsPseudostateKind;
  end;

  TBACallConcurrencyKind = class(TBAValueSet)
  private
    function GetAsCallConcurrencyKind: TCallConcurrencyKind;
    procedure SetAsCallConcurrencyKind(CallConcurrencyKind: TCallConcurrencyKind);
  public
    class function GetValues: TBAValueSetValueList; override;
    property AsCallConcurrencyKind: TCallConcurrencyKind read getAsCallConcurrencyKind write setAsCallConcurrencyKind;
  end;

  TBAMessageDirectionKind = class(TBAValueSet)
  private
    function GetAsMessageDirectionKind: TMessageDirectionKind;
    procedure SetAsMessageDirectionKind(MessageDirectionKind: TMessageDirectionKind);
  public
    class function GetValues: TBAValueSetValueList; override;
    property AsMessageDirectionKind: TMessageDirectionKind read getAsMessageDirectionKind write setAsMessageDirectionKind;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldMemberTypeDictionary;

var
  _ScopeKindValues: TBAValueSetValueList;
  _ParameterDirectionKindValues: TBAValueSetValueList;
  _SeverityValues: TBAValueSetValueList;
  _AggregationKindValues: TBAVAlueSetValueList;
  _VisibilityKindValues: TBAVAlueSetValueList;
  _ChangeableKindValues: TBAValueSetValueList;
  _OrderingKindValues: TBAValueSetValueList;
  _PseudostateKindValues: TBAValueSetValueList;
  _CallConcurrencyKindValues: TBAValueSetValueList;
  _MessageDirectionKindValues: TBAValueSetValueList;


class function TBAScopeKind.GetValues: TBAValueSetValueList;
begin
  if not Assigned(_ScopeKindValues) then
  begin
    _ScopeKindValues := TBAValueSetValueList.Create;
    _ScopeKindValues.Add(1, ['instance']);
    _ScopeKindValues.Add(2, ['classifier']);
  end;
  Result := _ScopeKindValues;
end;

function TBAScopeKind.GetAsScopeKind: TScopeKind;
begin
  case AsInteger of
    1: result := skInstance;
    2: result := skClassifier;
    else raise EBold.CreateFmt(sWrongValue, [ClassName, 'GetAsScopeKind']); // do not localize
  end;
end;

procedure TBAScopeKind.SetAsScopeKind(ScopeKind: TScopeKind);
begin
  case ScopeKind of
    skInstance: AsInteger := 1;
    skClassifier: AsInteger := 2;
  end;
end;

class function TBAParameterDirectionKind.GetValues: TBAValueSetValueList;
begin
  if not Assigned(_ParameterDirectionKindValues) then
  begin
    _ParameterDirectionKindValues := TBAValueSetValueList.Create;
    _ParameterDirectionKindValues.Add(1, ['in']);
    _ParameterDirectionKindValues.Add(2, ['out']);
    _ParameterDirectionKindValues.Add(3, ['inout']);
    _ParameterDirectionKindValues.Add(4, ['return']);
  end;
  Result := _ParameterDirectionKindValues;
end;

function TBAParameterDirectionKind.GetAsParameterDirectionKind: TBoldParameterDirectionKind;
begin
  case AsInteger of
    1: result := pdIn;
    2: result := pdOut;
    3: Result := pdInout;
    4: Result := pdReturn;
    else raise EBold.CreateFmt(sWrongValue, [ClassName, 'getAsParameterDirectionKind']); // do not localize
  end;
end;

procedure TBAParameterDirectionKind.SetAsParameterDirectionKind(ParameterDirectionKind: TBoldParameterDirectionKind);
begin
  case ParameterDirectionKind of
    pdIn: AsInteger := 1;
    pdOut: AsInteger := 2;
    pdInout: AsInteger := 3;
    pdReturn: AsInteger := 4;
  end;
end;


class function TBASeverity.GetValues: TBAValueSetValueList;
begin
  if not Assigned(_SeverityValues) then
  begin
    _SeverityValues := TBAValueSetValueList.Create;
    _SeverityValues.Add(0, ['None']);
    _SeverityValues.Add(1, ['Hint']);
    _SeverityValues.Add(2, ['Warning']);
    _SeverityValues.Add(3, ['Error']);
  end;
  Result := _SeverityValues;
end;

function TBASeverity.GetAsSeverity: TSeverity;
begin
  case AsInteger of
    0: Result := sNone;
    1: Result := sHint;
    2: result := sWarning;
    3: result := sError;
    else raise EBold.CreateFmt(sWrongValue, [ClassName, 'getAsSeverity']); // do not localize
  end;
end;

procedure TBASeverity.SetAsSeverity(Severity: TSeverity);
begin
  case Severity of
    sNone: AsInteger := 0;
    sHint: AsInteger := 1;
    sWarning: AsInteger := 2;
    sError: AsInteger := 3;
  end;
end;

{ TBAAggregationKind }

function TBAAggregationKind.GetAsAggregationKind: TAggregationKind;
begin
  case AsInteger of
    1: Result := akNone;
    2: Result := akAggregate;
    3: Result := akComposite;
    else raise EBold.CreateFmt(sWrongValue, [ClassName, 'GetAsAggregationKind']); // do not localize
  end;
end;

class function TBAAggregationKind.GetValues: TBAValueSetValueList;
begin
  if not Assigned(_AggregationKindValues) then
  begin
    _AggregationKindValues := TBAValueSetValueList.Create;
    _AggregationKindValues.Add(1, ['none']);
    _AggregationKindValues.Add(2, ['aggregate']);
    _AggregationKindValues.Add(3, ['composite']);
  end;
  Result := _AggregationKindValues;
end;

procedure TBAAggregationKind.SetAsAggregationKind(
  AggregationKind: TAggregationKind);
begin
  case AggregationKind of
    akNone: AsInteger := 1;
    akAggregate: AsInteger := 2;
    akComposite: AsInteger := 3;
  end;
end;

{ TBAVisibilityKind }

function TBAVisibilityKind.GetAsVisibilityKind: TVisibilityKind;
begin
  case AsInteger of
    1: Result := vkPrivate;
    2: Result := vkProtected;
    3: Result := vkPublic;
    else raise EBold.CreateFmt(sWrongValue, [ClassName, 'GetAsVisibilityKind']); // do not localize
  end;
end;

class function TBAVisibilityKind.GetValues: TBAValueSetValueList;
begin
  if not Assigned(_VisibilityKindValues) then
  begin
    _VisibilityKindValues := TBAValueSetValueList.Create;
    _VisibilityKindValues.Add(1, ['private']);
    _VisibilityKindValues.Add(2, ['protected']);
    _VisibilityKindValues.Add(3, ['public']);
  end;
  Result := _VisibilityKindValues;
end;

procedure TBAVisibilityKind.SetAsVisibilityKind(VisibilityKind: TVisibilityKind);
begin
  case VisibilityKind  of
    vkPrivate: AsInteger := 1;
    vkProtected: AsInteger := 2;
    vkPublic: AsInteger := 3;
  end;
end;

{ TBAChangeableKind }

function TBAChangeableKind.GetAsChangeableKind: TChangeableKind;
begin
  case AsInteger of
    1: Result := ckChangeable;
    2: Result := ckFrozen;
    3: Result := ckAddOnly;
    else raise EBold.CreateFmt(sWrongValue, [ClassName, 'GetAsChangeableKind']); // do not localize
  end;
end;

class function TBAChangeableKind.GetValues: TBAValueSetValueList;
begin
  if not Assigned(_ChangeableKindValues) then
  begin
    _ChangeableKindValues := TBAValueSetValueList.Create;
    _ChangeableKindValues.Add(1, ['changeable']);
    _ChangeableKindValues.Add(2, ['frozen']);
    _ChangeableKindValues.Add(3, ['addOnly']);
  end;
  Result := _ChangeableKindValues;
end;

procedure TBAChangeableKind.SetAsChangeableKind(ChangeableKind: TChangeableKind);
begin
  case ChangeableKind of
    ckChangeable: AsInteger := 1;
    ckFrozen: AsInteger := 2;
    ckAddOnly: AsInteger := 3;
  end;
end;

{ TBAOrderingKind }

function TBAOrderingKind.GetAsOrderingKind: TOrderingKind;
begin
  case AsInteger of
    0: Result := okUnordered;
    1: Result := okOrdered;
    else raise EBold.CreateFmt(sWrongValue, [ClassName, 'getAsOrderingKind']); // do not localize
  end;
end;

class function TBAOrderingKind.GetValues: TBAValueSetValueList;
begin
  if not Assigned(_OrderingKindValues) then
  begin
    _OrderingKindValues := TBAValueSetValueList.Create;
    _OrderingKindValues.Add(0, ['unordered']); // do not localize
    _OrderingKindValues.Add(1, ['ordered']); // do not localize
  end;
  Result := _OrderingKindValues;
end;

procedure TBAOrderingKind.SetAsOrderingKind(OrderingKind: TOrderingKind);
begin
  case OrderingKind of
    okUnordered: AsInteger := 0;
    okOrdered: AsInteger := 1;
  end;
end;

{ TBAPseudostateKindKind }

function TBAPseudostateKind.GetAsPseudostateKind: TPseudostateKind;
begin
  case AsInteger of
    0: Result := pkInitial;
    1: Result := pkDeepHistory;
    2: Result := pkShallowHistory;
    3: Result := pkJoin;
    4: Result := pkFork;
    5: Result := pkBranch;
    6: Result := pkJunction;
    7: Result := pkFinal;
    else raise EBold.CreateFmt(sWrongValue, [ClassName, 'getAsPseudostateKind']); // do not localize
  end;
end;

class function TBAPseudostateKind.GetValues: TBAValueSetValueList;
begin
  if not Assigned(_PseudostateKindValues) then
  begin
    _PseudostateKindValues := TBAValueSetValueList.Create;
    _PseudostateKindValues.Add(0, ['initial']); // do not localize
    _PseudostateKindValues.Add(1, ['deepHistory']); // do not localize
    _PseudostateKindValues.Add(2, ['shallowHistory']); // do not localize
    _PseudostateKindValues.Add(3, ['join']); // do not localize
    _PseudostateKindValues.Add(4, ['fork']); // do not localize
    _PseudostateKindValues.Add(5, ['branch']); // do not localize
    _PseudostateKindValues.Add(6, ['junction']); // do not localize
    _PseudostateKindValues.Add(7, ['final']); // do not localize
  end;
  Result := _PseudostateKindValues;
end;

procedure TBAPseudostateKind.SetAsPseudostateKind(
  PseudostateKind: TPseudostateKind);
begin
  case PseudostateKind of
    pkInitial: AsInteger := 0;
    pkDeepHistory: AsInteger := 1;
    pkShallowHistory: AsInteger := 2;
    pkJoin: AsInteger := 3;
    pkFork: AsInteger := 4;
    pkBranch: AsInteger := 5;
    pkJunction: AsInteger := 6;
    pkFinal: AsInteger := 7;
  end;
end;

{ TBACallConcurrencyKind }

function TBACallConcurrencyKind.GetAsCallConcurrencyKind: TCallConcurrencyKind;
begin
  case AsInteger of
    0: Result := cckSequential;
    1: Result := cckGuarded;
    2: Result := cckConcurrent;
    else raise EBold.CreateFmt(sWrongValue, [ClassName, 'getAsCallConcurrencyKind']); // do not localize
  end;
end;

class function TBACallConcurrencyKind.GetValues: TBAValueSetValueList;
begin
  if not Assigned(_CallConcurrencyKindValues) then
  begin
    _CallConcurrencyKindValues := TBAValueSetValueList.Create;
    _CallConcurrencyKindValues.Add(0, ['sequential']);
    _CallConcurrencyKindValues.Add(1, ['guarded']);
    _CallConcurrencyKindValues.Add(2, ['concurrent']);
  end;
  Result := _CallConcurrencyKindValues;
end;

procedure TBACallConcurrencyKind.SetAsCallConcurrencyKind(
  CallConcurrencyKind: TCallConcurrencyKind);
begin
  case CallConcurrencyKind of
    cckSequential: AsInteger := 0;
    cckGuarded: AsInteger := 1;
    cckConcurrent: AsInteger := 2;
  end;
end;

{ TBAMessageDirectionKind }

function TBAMessageDirectionKind.GetAsMessageDirectionKind: TMessageDirectionKind;
begin
  case AsInteger of
    0: Result := mdkActivation;
    1: Result := mdkReturn;
    else raise EBold.CreateFmt(sWrongValue, [ClassName, 'getAsMessageDirectionKind']); // do not localize
  end;
end;

class function TBAMessageDirectionKind.GetValues: TBAValueSetValueList;
begin
  if not Assigned(_MessageDirectionKindValues) then
  begin
    _MessageDirectionKindValues := TBAValueSetValueList.Create;
    _MessageDirectionKindValues.Add(0, ['activation']);
    _MessageDirectionKindValues.Add(1, ['return']);
  end;
  Result := _MessageDirectionKindValues;
end;

procedure TBAMessageDirectionKind.SetAsMessageDirectionKind(
  MessageDirectionKind: TMessageDirectionKind);
begin
  case MessageDirectionKind of
    mdkActivation: AsInteger := 0;
    mdkReturn: AsInteger := 1;
  end;
end;

initialization
  BoldmemberTypes.AddMemberTypeDescriptor(TBAScopeKind, alConcrete);
  BoldmemberTypes.AddMemberTypeDescriptor(TBAParameterDirectionKind, alConcrete);
  BoldmemberTypes.AddMemberTypeDescriptor(TBASeverity, alConcrete);
  BoldmemberTypes.AddMemberTypeDescriptor(TBAAggregationKind, alConcrete);
  BoldmemberTypes.AddMemberTypeDescriptor(TBAVisibilityKind, alConcrete);
  BoldmemberTypes.AddMemberTypeDescriptor(TBAChangeableKind, alConcrete);
  BoldmemberTypes.AddMemberTypeDescriptor(TBAOrderingKind, alConcrete);
  BoldmemberTypes.AddMemberTypeDescriptor(TBAPseudostateKind, alConcrete);
  BoldmemberTypes.AddMemberTypeDescriptor(TBACallConcurrencyKind, alConcrete);
  BoldmemberTypes.AddMemberTypeDescriptor(TBAMessageDirectionKind, alConcrete);

finalization
  FreeAndNil(_ScopeKindValues);
  FreeAndNil(_ParameterDirectionKindValues);
  FreeAndNil(_SeverityValues);
  FreeAndNil(_AggregationKindValues);
  FreeAndNil(_VisibilityKindValues);
  FreeAndNil(_ChangeableKindValues);
  FreeAndNil(_OrderingKindValues);
  FreeAndNil(_PseudostateKindValues);
  FreeAndNil(_CallConcurrencyKindValues);
  FreeAndNil(_MessageDirectionKindValues);
  if BoldMemberTypesAssigned then
  begin
    BoldMemberTypes.RemoveDescriptorByClass(TBAScopeKind);
    BoldMemberTypes.RemoveDescriptorByClass(TBAParameterDirectionKind);
    BoldMemberTypes.RemoveDescriptorByClass(TBASeverity);
    BoldMemberTypes.RemoveDescriptorByClass(TBAAggregationKind);
    BoldMemberTypes.RemoveDescriptorByClass(TBAVisibilityKind);
    BoldMemberTypes.RemoveDescriptorByClass(TBAChangeableKind);
    BoldMemberTypes.RemoveDescriptorByClass(TBAOrderingKind);
    BoldMemberTypes.RemoveDescriptorByClass(TBAPseudostateKind);
    BoldMemberTypes.RemoveDescriptorByClass(TBACallConcurrencyKind);
    BoldMemberTypes.RemoveDescriptorByClass(TBAMessageDirectionKind);
  end;
end.
