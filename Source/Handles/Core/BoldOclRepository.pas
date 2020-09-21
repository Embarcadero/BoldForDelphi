unit BoldOclRepository;

interface

uses
  Classes,
  BoldElements,
  BoldSystemRT,
  BoldSystemHandle,
  BoldSubscription,
  BoldComponentvalidator;

type
  TBoldOclDefinitions = class;
  TBoldOclDefinition = class;
  TBoldOclRepository = class;

  TBoldOclRepository = class(TBoldSubscribableComponent, IBoldValidateableComponent)
  private
    FOclDefinitions: TBoldOclDefinitions;
    FSystemHandle: TBoldSystemHandle;
    procedure SetOclDefinitions(const Value: TBoldOclDefinitions);
    procedure SetSystemHandle(const Value: TBoldSystemHandle);
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean;
  public
    constructor Create(owner: TComponent); override;
    function LookUpOclDefinition(Name: string): string;
  published
    property OclDefinitions: TBoldOclDefinitions read FOclDefinitions write SetOclDefinitions;
    property SystemHandle: TBoldSystemHandle read FSystemHandle write SetSystemHandle;
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
    constructor Create(OwningRepository: TBoldOclRepository);
    function GetUniqueName: String;
    function LookUpOclDefinition(Name: string): string;
    property Items[Index: integer]: TBoldOclDefinition read GetItems; default;
  end;

  TBoldOclDefinition = class(TCollectionItem, IBoldOclComponent)
  private
    fName: String;
    fExpression: String;
    fContext: String;
    procedure SetExpression(Expression: String);
    function GetExpression: String;
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
    destructor Destroy; override;
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
  BoldDefs,
  BoldHandles,
  BoldLogHandler,
  HandlesConst;

{ TBoldOclRepository }

constructor TBoldOclRepository.create(owner: TComponent);
begin
  inherited;
  FOclDefinitions := TBoldOclDefinitions.Create(self);
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

procedure TBoldOclRepository.SetSystemHandle(const Value: TBoldSystemHandle);
begin
  if assigned(SystemHandle) then
    SystemHandle.InstallOclDefinitionLookUp(nil);

  FSystemHandle := Value;
  if assigned(SystemHandle) then
    SystemHandle.InstallOclDefinitionLookUp(LookUpOclDefinition);
end;

function TBoldOclRepository.ValidateComponent(
  ComponentValidator: TBoldComponentValidator;
  NamePrefix: String): Boolean;
var
  i: integer;
  Context: TBoldElementTypeInfo;
begin
  result := true;
  if not assigned(SystemHandle) then
    BoldLog.LogFmt(sRepositoryHasNoSystemHandle, [NamePrefix, Name])
  else if not assigned(SystemHandle.StaticSystemTypeInfo) then
    BoldLog.LogFmt(sSystemHandleHasNoTypeInfo, [NamePrefix, Name])
  else
  begin
    for i := 0 to OclDefinitions.count - 1 do
    begin
      Context := SystemHandle.StaticSystemTypeInfo.ElementTypeInfoByExpressionName[OclDefinitions[i].Context];
      result := ComponentValidator.ValidateExpressionInContext(
        OclDefinitions[i].Expression,
        Context,
        NamePrefix + Name + '.' + OclDefinitions[i].Name) and result;
    end;
  end;
end;

{ TBoldOclDefinition }

constructor TBoldOclDefinition.Create(Collection: TCollection);
begin
  inherited;
  Name := (Collection as TBoldOclDefinitions).GetUniqueName;
end;

destructor TBoldOclDefinition.Destroy;
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
  result:= '%' + name + ': ' + Expression;
end;


function TBoldOclDefinition.GetExpression: String;
begin
  result := fExpression;
end;

function TBoldOclDefinition.GetSystemTypeInfo: TBoldSystemTypeInfo;
begin
  if assigned(Definitions.OwningRepository.SystemHandle) then
    result := Definitions.OwningRepository.SystemHandle.StaticSystemTypeInfo
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

procedure TBoldOclDefinition.SetExpression(Expression: String);
begin
  fExpression := Expression;
end;

procedure TBoldOclDefinition.SetName(const Value: String);
begin
  if value <> Name then
  begin
    if TBoldOclDefinitions(Collection).NameIsUnique(Value) then
      FName := Value
    else
      raise EBold.CreateFmt(sNameNotUnique, [Value]);
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
    result := 'Ocl' + IntToStr(i); // do not translate
    Inc(i);
  until NameIsUnique(result);
end;

function TBoldOclDefinitions.LookUpOclDefinition(Name: string): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to Count - 1 do
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
    if AnsiCompareStr(Name, Items[i].Name) = 0 then
    begin
      result := false;
      exit;
    end;
end;

end.

