
{ Global compiler directives }
{$include bold.inc}
unit BoldOclRepository;

interface

uses
  Classes,
  BoldElements,
  BoldSystemRT,
  BoldSystemHandle,
  BoldSubscription,
  BoldHandles,
  BoldComponentvalidator,
  BoldDefs;

type
  TBoldOclDefinitions = class;
  TBoldOclDefinition = class;
  TBoldOclRepository = class;

  TBoldOclRepository = class(TBoldNonSystemHandle, IBoldValidateableComponent)
  private
    FOclDefinitions: TBoldOclDefinitions;
    procedure SetOclDefinitions(const Value: TBoldOclDefinitions);
//    procedure SetStaticSystemHandle(Value: TBoldSystemHandle);
  protected
//    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetStaticSystemHandle(Value: TBoldAbstractSystemHandle); override;
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean; override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    function LookUpOclDefinition(Name: string): string;
  published
    property OclDefinitions: TBoldOclDefinitions read FOclDefinitions write SetOclDefinitions;
  end;

  TBoldOclDefinitions = class(TCollection)
  private
    fOwningRepository: TBoldOclRepository;
    function GetItems(Index: integer): TBoldOclDefinition;
  protected
    function GetOwner: TPersistent; override;
    property OwningRepository: TBoldOclRepository read fOwningRepository;
    function NameIsUnique(Name: String): Boolean;
  public
    constructor create(OwningRepository: TBoldOclRepository);
    function GetUniqueName: String;
    function LookUpOclDefinition(Name: string): string;
    property Items[Index: integer]: TBoldOclDefinition read GetItems; default;
  end;

  TBoldOclDefinition = class(TCollectionItem, IBoldOclComponent)
  private
    fName: String;
    fExpression: String;
    fContext: String;
    procedure SetExpression(const Value: TBoldExpression);
    function GetExpression: TBoldExpression;
    function GetVariableList: TBoldExternalVariableList;
    function GetDefinitions: TBoldOclDefinitions;
    function QueryInterface(const IId: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function GetSystemTypeInfo: TBoldSystemTypeInfo;
    procedure SetName(const Value: String);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor destroy; override;
    function GetContextType: TBoldElementTypeInfo;
    property Definitions: TBoldOclDefinitions read GetDefinitions;
    property SystemTypeInfo: TBoldSystemTypeInfo read GetSystemTypeInfo;
  published
    property Name: String read FName write SetName;
    property Expression: String read GetExpression write SetExpression;
    property Context: String read FContext write fContext;
  end;


implementation

uses
  SysUtils,
  BoldLogHandler;

{ TBoldOclRepository }

constructor TBoldOclRepository.Create(owner: TComponent);
begin
  inherited;
  FOclDefinitions := TBoldOclDefinitions.Create(self);
end;

destructor TBoldOclRepository.Destroy;
begin
  FreeAndNil(FOclDefinitions);
  inherited;
end;

function TBoldOclRepository.LookUpOclDefinition(Name: string): string;
begin
  result := OclDefinitions.LookUpOclDefinition(Name);
end;

procedure TBoldOclRepository.SetOclDefinitions(
  const Value: TBoldOclDefinitions);
begin
  FOclDefinitions := Value;
end;

procedure TBoldOclRepository.SetStaticSystemHandle(Value: TBoldAbstractSystemHandle);
begin
  if StaticSystemHandle = Value then
   exit;
  if assigned(StaticSystemHandle) then
  begin
    (StaticSystemHandle as TBoldSystemHandle).InstallOclDefinitionLookUp(nil);
    StaticSystemHandle.RemoveFreeNotification(self);
  end;
  if assigned(Value) then
  begin
    (Value as TBoldSystemHandle).InstallOclDefinitionLookUp(LookUpOclDefinition);
    Value.FreeNotification(Self);
  end;
  inherited SetStaticSystemHandle(Value);
end;

function TBoldOclRepository.ValidateComponent(
  ComponentValidator: TBoldComponentValidator;
  NamePrefix: String): Boolean;
var
  i: integer;
  Context: TBoldElementTypeInfo;
begin
  result := inherited ValidateComponent(ComponentValidator, NamePrefix);
  if not assigned(StaticSystemHandle) then
    BoldLog.LogFmt('*** OclRepository %s%s has no StaticSystemHandle', [NamePrefix, Name])
  else if not assigned(StaticSystemHandle.StaticSystemTypeInfo) then
    BoldLog.LogFmt('*** StaticSystemHandle of OclRepository %s%s has no TypeInfo', [NamePrefix, Name])
  else
  begin
    for i := 0 to OclDefinitions.count-1 do begin
      Context := StaticSystemHandle.StaticSystemTypeInfo.ElementTypeInfoByExpressionName[OclDefinitions[i].Context];
      result := ComponentValidator.ValidateExpressionInContext(
        OclDefinitions[i].Expression,
        Context,
        NamePrefix+Name+ '.'+OclDefinitions[i].Name) and result;
    end;
  end;
end;

{ TBoldOclDefinition }

constructor TBoldOclDefinition.Create(Collection: TCollection);
begin
  inherited;
  Name := (Collection as TBoldOclDefinitions).GetUniqueName;
end;

destructor TBoldOclDefinition.destroy;
begin
  inherited;

end;

function TBoldOclDefinition.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(SystemTypeInfo) then
    result := SystemTypeInfo.ElementTypeInfoByExpressionName[Context]
  else
    result := nil;
end;

function TBoldOclDefinition.GetDefinitions: TBoldOclDefinitions;
begin
  result := Collection as TBoldOclDefinitions;
end;

function TBoldOclDefinition.GetDisplayName: string;
begin
  result:= '%' + name + ': '+Expression;
end;


function TBoldOclDefinition.GetExpression: TBoldExpression;
begin
  result := fExpression;
end;

function TBoldOclDefinition.GetSystemTypeInfo: TBoldSystemTypeInfo;
begin
  if assigned(Definitions.OwningRepository.StaticSystemHandle) then
    result := Definitions.OwningRepository.StaticSystemHandle.StaticSystemTypeInfo
  else
    result := nil;
end;

function TBoldOclDefinition.GetVariableList: TBoldExternalVariableList;
begin
  result := nil;
end;

function TBoldOclDefinition.QueryInterface(const IId: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TBoldOclDefinition.SetExpression(const Value: TBoldExpression);
begin
  fExpression := Value;
end;

procedure TBoldOclDefinition.SetName(const Value: String);
begin
  if value <> Name then
  begin
    if TBoldOclDefinitions(Collection).NameIsUnique(Value) then
      FName := Value
    else
      raise EBold.CreateFmt('Invalid Name: %s Not Unique', [Value]);
  end;
end;

function TBoldOclDefinition._AddRef: Integer;
begin
  result := -1;
end;

function TBoldOclDefinition._Release: Integer;
begin
  result := -1;
end;

{ TBoldOclDefinitions }

constructor TBoldOclDefinitions.create(OwningRepository: TBoldOclRepository);
begin
  inherited Create(TBoldOclDefinition);
  fOwningRepository := OwningRepository;
end;

function TBoldOclDefinitions.GetItems(Index: integer): TBoldOclDefinition;
begin
  result := TBoldOclDefinition(inherited items[index]);
end;

function TBoldOclDefinitions.GetOwner: TPersistent;
begin
  result := OwningRepository;
end;

function TBoldOclDefinitions.GetUniqueName: String;
var
  i: integer;
begin
  i := 1;
  repeat
    result := 'Ocl'+IntToStr(i);
    Inc(i);
  until NameIsUnique(result);
end;

function TBoldOclDefinitions.LookUpOclDefinition(Name: string): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to Count -1 do
    if Items[i].Name = Name then
    begin
      result := items[i].Expression;
      exit;
    end;
end;

function TBoldOclDefinitions.NameIsUnique(Name: String): Boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to Count-1 do
    if CompareStr(Name, Items[i].Name) = 0 then
    begin
      result := false;
      exit;
    end;
end;

initialization
end.
