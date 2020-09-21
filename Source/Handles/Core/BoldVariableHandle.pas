unit BoldVariableHandle;

interface

uses
  Classes,
  BoldElements,
  BoldSystem,
  BoldHandles;

type
  TBoldVariableHandle = class(TBoldNonSystemHandle)
  private
    fValueTypeName: String;
    fValue: TBoldElement;
    fInitialValues: TStringList;
    procedure InitialvaluesChanged(Sender: TObject);
    procedure SetValueTypeName(const Value: String);
    procedure SetInitialValues(const Value: TStringList);
    procedure CreateVariableElement;
    function GetObjectList: TBoldObjectList;
  protected
    function GetValue: TBoldElement; override;
    function GetStaticBoldType: TBoldElementTypeInfo; override;
    procedure StaticBoldTypeChanged; override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    property ObjectList: TBoldObjectList read GetObjectList;
  published
    property ValueTypeName: String read fValueTypeName write SetValueTypeName;
    property InitialValues: TStringList read fInitialValues write SetInitialValues;
  end;

implementation

uses
  SysUtils,
  BoldSubscription,
  BoldDefs,
  BoldSystemRT,
  HandlesConst;

{ TBoldVariableHandle }

constructor TBoldVariableHandle.Create(owner: TComponent);
var
  s: TStringList;
begin
  inherited;
  s := TStringList.Create;
  s.OnChange := InitialvaluesChanged;
  fInitialValues := s;
end;

procedure TBoldVariableHandle.CreateVariableElement;
var
  i: integer;
begin
  Assert(not Assigned(fValue));
  if assigned(StaticBoldType) then
  begin
    fValue := TBoldMemberFactory.CreateMemberFromBoldType(StaticBoldType);
    if Initialvalues.Count>0 then
    begin
      if fValue is TBoldAttribute then
        fValue.AsString := InitialValues[0]
      else if fValue is TBoldMemberList then
        with TBoldMemberList(Fvalue) do
          for i := 0 to InitialValues.Count-1 do
            AddNew.AsString := InitialValues[i];
    end;
  end;
end;

destructor TBoldVariableHandle.Destroy;
begin
  FreePublisher;
  FreeAndNil(fInitialValues);
  FreeAndNil(fValue);
  inherited;
end;

function TBoldVariableHandle.GetStaticBoldType: TBoldElementTypeInfo;
begin
  if assigned(StaticSystemHandle) then
  begin
    result := StaticSystemHandle.StaticSystemTypeInfo.ElementTypeInfoByExpressionName[ValueTypeName];
    if assigned(result) and not (result.BoldValueType in [bvtAttr, bvtList]) then
      raise EBold.CreateFmt(sOnlyListsAndAttributeTypesAllowed, [ClassName, ValueTypeName]);
  end
  else
    result := nil;
end;

function TBoldVariableHandle.GetValue: TBoldElement;
begin
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    if not assigned(StaticSystemHandle) then
      raise EBold.CreateFmt(sNoSystemHandle, [classname, 'GetValue', name]); // do not localize
    if not assigned(StaticBoldType) then
      raise EBold.CreateFmt(sValueTypeNameInvalid, [classname, name, ValueTypeName]);
  end;

  if {not (csDesigning in ComponentState) and}
    not assigned(fValue) and
    assigned(StaticBoldType) then
    CreateVariableElement;
  result := fValue;
end;

procedure TBoldVariableHandle.InitialvaluesChanged(Sender: TObject);
begin
  if Assigned(FValue) {and (csDesigning in ComponentState)} then
  begin
    FreeAndNil(fValue);
    SendEvent(Self, beValueIdentityChanged);
  end;
end;

procedure TBoldVariableHandle.SetValueTypeName(const Value: String);
begin
  if value <> fValueTypeName then
  begin
    fValueTypeName := Value;
    StaticBoldTypeChanged;
  end;
end;

procedure TBoldVariableHandle.SetInitialValues(const Value: TStringList);
begin
  if FInitialValues <> Value then
    FInitialValues.Assign(Value);
end;

procedure TBoldVariableHandle.StaticBoldTypeChanged;
begin
  FreeAndNil(FValue);
  inherited;  {Send message after clearing value}
end;

function TBoldVariableHandle.GetObjectList: TBoldObjectList;
begin
  if value is TBoldObjectList then
    result := value as TBoldObjectList
  else
    result := nil;
end;

end.
