
{ Global compiler directives }
{$include bold.inc}
unit BoldVariableDefinitionCom;

interface

uses
  Classes,
  BoldSubscription,
  BoldHandlesCom;

type

  TBoldVariableDefinitionCom = class;
  TBoldVariableTupleListCom = class;
  TBoldVariableTupleCom = class;

  TBoldVariableDefinitionCom = class(TBoldSubscribableComponent)
  private
    fVariableTupleList: TBoldVariableTupleListCom;
    procedure SetVariableTupleList(const Value: TBoldVariableTupleListCom);
    procedure VariablesChanged;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property Variables: TBoldVariableTupleListCom read fVariableTupleList write SetVariableTupleList;
  end;

  TBoldVariableTupleListCom = class(TCollection)
  private
    fOwner: TBoldVariableDefinitionCom;
    function GetItems(Index: integer): TBoldVariableTupleCom;
  protected
    function GetOwner: TPersistent; override;
    property Owner: TBoldVariableDefinitionCom read fOwner;
  public
    constructor create(Owner: TBoldVariableDefinitionCom);
    function NameIsUnique(Name: String): Boolean;
    function NameIsValid(Name: String): Boolean;
    function GetUniqueName: String;
    property Items[Index: integer]: TBoldVariableTupleCom read GetItems; default;
  end;

  TBoldVariableTupleCom = class(TCollectionItem)
  private
    FVariableName: String;
    FBoldHandle: TBoldElementHandleCom;
    fBoldHandleSubscriber: TBoldPassThroughSubscriber;
    FUseListElement: Boolean;
    procedure _ReceiveHandleEvent(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetBoldHandle(const Value: TBoldElementHandleCom);
    procedure SetVariableName(const Value: String);
    procedure SetUseListElement(const Value: Boolean);
    procedure Changed;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property BoldHandle: TBoldElementHandleCom read FBoldHandle write SetBoldHandle;
    property VariableName: String read FVariableName write SetVariableName;
    property UseListElement: Boolean read FUseListElement write SetUseListElement;
  end;


implementation

uses
  SysUtils,
  BoldDefs;

{ TBoldVariableDefinitionCom }
constructor TBoldVariableDefinitionCom.create(Owner: TComponent);
begin
  inherited;
  fVariableTupleList := TBoldVariableTupleListCom.Create(self);
end;

destructor TBoldVariableDefinitionCom.Destroy;
begin
  FreeAndNil(fVariableTupleList);
  inherited;
end;

procedure TBoldVariableDefinitionCom.SetVariableTupleList(
  const Value: TBoldVariableTupleListCom);
begin
  fVariableTupleList := Value;
end;

{ TBoldVariableTupleList }

constructor TBoldVariableTupleListCom.Create(Owner: TBoldVariableDefinitionCom);
begin
  inherited Create(TBoldVariableTupleCom);
  fOwner := Owner;
end;


function TBoldVariableTupleListCom.GetItems(
  Index: integer): TBoldVariableTupleCom;
begin
  result := TBoldVariableTupleCom(inherited items[index]);
end;

function TBoldVariableTupleListCom.GetOwner: TPersistent;
begin
  result := fOwner;
end;

function TBoldVariableTupleListCom.GetUniqueName: String;
var
  i: integer;
begin
  i := 1;
  repeat
    result := 'Variable'+IntToStr(i);
    Inc(i);
  until NameIsUnique(result);
end;

function TBoldVariableTupleListCom.NameIsUnique(Name: String): Boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to Count-1 do
    if AnsiCompareStr(Name, Items[i].VariableName) = 0 then
    begin
      result := false;
      exit;
    end;
end;

function TBoldVariableTupleListCom.NameIsValid(Name: String): Boolean;
var
  i: integer;
begin
  result := true;
  for i := 1 to length(name) do
    if not (name[i] in ['A'..'Z', 'a'..'z', '_', '0'..'9']) then
    begin
      result := false;
      exit;
    end;
end;

{ TBoldVariableTuple }

procedure TBoldVariableTupleCom.Changed;
begin
  (Collection as TBoldVariableTupleListCom).Owner.VariablesChanged;
end;

constructor TBoldVariableTupleCom.Create(Collection: TCollection);
begin
  inherited;
  Changed;
  VariableName := (Collection as TBoldVariableTupleListCom).GetUniqueName;
  fBoldHandleSubscriber := TBoldPassthroughSubscriber.Create(_ReceiveHandleEvent);
end;

destructor TBoldVariableTupleCom.Destroy;
begin
  FreeAndNil(fBoldHandleSubscriber);
  inherited;
end;

function TBoldVariableTupleCom.GetDisplayName: string;
begin
  result := VariableName;
  if assigned(BoldHandle) then
    Result := result + ': ' + BoldHandle.Name
  else
    result := result + ': Not Connected';
  if UseListElement then
    result := result + ' (list)';
end;

procedure TBoldVariableTupleCom.SetBoldHandle(const Value: TBoldElementHandleCom);
begin
  FBoldHandle := Value;
  fBoldHandleSubscriber.CancelAllSubscriptions;
  if assigned(value) then
    Value.AddSmallSubscription(fBoldHandleSubscriber, [beDestroying], beDestroying); 

  Changed;

end;

procedure TBoldVariableTupleCom.SetUseListElement(const Value: Boolean);
begin 
  Changed;
end;

procedure TBoldVariableTupleCom.SetVariableName(const Value: String);
begin
  if FVariableName <> Value then begin
    if not (Collection as TBoldVariableTupleListCom).NameIsUnique(Value) then
      raise EBold.CreateFmt('Can''t rename variable to "%s", name already exists', [Value]);
    if not (Collection as TBoldVariableTupleListCom).NameIsValid(Value) then
      raise EBold.Create('Invalid variable name, only alphanum characters and underscore valid');
    FVariableName := Value;
    Changed;
  end;
end;


procedure TBoldVariableTupleCom._ReceiveHandleEvent(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (originator = BoldHandle) and (requestedEvent = beDestroying) then
    BoldHandle := nil;
end;

procedure TBoldVariableDefinitionCom.VariablesChanged;
begin 
end;

end.
