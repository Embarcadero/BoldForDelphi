
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLTypes;

interface

uses
  BoldBase,
  BoldContainers;

type
  { forward declarations }
  TBoldUMLRange = class;
  TBoldUMLMultiplicity = class; 
  TBoldParameterDirectionKind = (pdIn, pdOut, pdInout, pdReturn);
  TAggregationKind = (akNone, akAggregate, akComposite);
  TVisibilityKind = (vkPrivate, vkProtected, vkPublic);
  TChangeableKind = (ckChangeable, ckFrozen, ckAddOnly);
  TScopeKind = (skInstance, skClassifier);
  TOrderingKind = (okUnordered, okOrdered);
  TPseudostateKind = (pkInitial, pkDeepHistory, pkShallowHistory, pkJoin, pkFork,
        pkBranch, pkJunction, pkFinal);
  TCallConcurrencyKind = (cckSequential, cckGuarded, cckConcurrent);
  TMessageDirectionKind = (mdkActivation, mdkReturn);

  TBoldUMLRange = class(TBoldMemoryManagedObject)
  private
    fUpper: integer;
    fUpperUnlimited: boolean;
    fLower: integer;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    procedure SetUpper(const Value: integer);
    procedure SetUpperUnlimited(const Value: Boolean);
  public
    function FormatAsString(UnlimitedString: string='n'): string;
    function InRange(value: integer): Boolean;
    property AsString: string read GetAsString write SetAsString;
    property Lower: integer read fLower write fLower;
    property Upper: integer read fUpper write SetUpper;
    property UpperUnlimited: boolean read fUpperUnlimited write SetUpperUnlimited;
  end;

  TBoldUMLMultiplicity = class(TBoldObjectArray)
  private
    function GetRange(index: integer): TBoldUMLRange;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  public
    constructor Create;
    function FormatAsString(UnlimitedString: string='n'): string;
    function InRange(value: integer): Boolean;
    property AsString: string read GetAsString write SetAsString;
    function AddRange: TBoldUMLRange;
    property Range[index: integer]: TBoldUMLRange read GetRange;
  end;

implementation

uses
  SysUtils;

{ TBoldUMLRange }

function TBoldUMLRange.FormatAsString(UnlimitedString: string): string;
begin
  if UpperUnlimited then
    if lower = 0 then
      result := UnlimitedString
    else
      Result := Format('%d..%s', [lower, UnlimitedString])
  else
    if Upper=Lower then
      Result := IntToStr(Lower)
    else
      Result := Format('%d..%d', [lower, upper])
end;

function TBoldUMLRange.GetAsString: string;
begin
  Result := FormatAsString;
end;

function TBoldUMLRange.InRange(value: integer): Boolean;
begin
  Result := (value >= Lower) and (UpperUnlimited or (value <= Upper));
end;

procedure TBoldUMLRange.SetAsString(const Value: string);
var
  UpperString: string;
  LowerString: string;
  DotDotPos: integer;
begin
  DotDotPos := pos('..', Value);
  if DotDotPos = 0 then
  begin
    LowerString := trim(Value);
    if (LowerString = 'n') or (LowerString = '*') then
    begin
      lower := 0;
      UpperUnlimited := true;
    end
    else
    begin
      Lower := StrToInt(LowerString);
      Upper := lower;
    end
  end
  else
  begin
    LowerString := trim(copy(value, 1, dotdotpos-1));
    UpperString := trim(copy(value, DotDotPos+2, MaxInt));
    lower := StrToInt(LowerString);
    Upper := StrToIntDef(UpperString, -1);
    if upper < 0 then
      UpperUnlimited := True;
  end;
end;

procedure TBoldUMLRange.SetUpper(const Value: integer);
begin
  fUpper := Value;
  fUpperUnlimited := false;
end;

procedure TBoldUMLRange.SetUpperUnlimited(const Value: Boolean);
begin
  fUpperUnlimited := Value;
  if Value = true then
    fUpper := -1;
end;

{ TBoldUMLMultiplicity }

function TBoldUMLMultiplicity.AddRange: TBoldUMLRange;
begin
  Result := TBoldUMLRange.Create;
  Add(result);
end;

constructor TBoldUMLMultiplicity.Create;
begin
  inherited Create(1, [bcoDataOwner]);
end;

function TBoldUMLMultiplicity.FormatAsString(
  UnlimitedString: string): string;
var
  i: integer;
begin
  for i := 0 to Count-1 do
  begin
    if length(Result) > 0 then
      Result := Result + ',';
    Result := result + Range[i].FormatAsString(UnlimitedString);
  end;  
end;

function TBoldUMLMultiplicity.GetAsString: string;
begin
  result := FormatAsString;
end;

function TBoldUMLMultiplicity.GetRange(index: integer): TBoldUMLRange;
begin
  result := Items[index] as  TBoldUMLRange;
end;

function TBoldUMLMultiplicity.InRange(value: integer): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count-1 do
    Result := Result or Range[i].InRange(value);
end;

procedure TBoldUMLMultiplicity.SetAsString(const Value: string);
var
  s: string;
  EndPos: integer;
  CommaPos: integer;
begin
  Clear;
  s := value;
  while length(s) > 0 do
  begin
    CommaPos := Pos(',', s);
    if CommaPos = 0 then
      EndPos := Length(s)
    else
      EndPos := CommaPos-1;
    AddRange.AsString := Copy(s, 1, EndPos);
    if CommaPos = 0 then
      s := ''
    else
      s := Copy(s, CommaPos+1, MaxInt);
  end;
end;

end.
