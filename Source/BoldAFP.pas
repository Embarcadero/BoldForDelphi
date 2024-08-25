unit BoldAFP;

interface

uses
  Forms,
  Controls,
  BoldBase,
  BoldElements,
  BoldReferenceHandle,
  BoldIndexableList;

type
  // Note: May also be made into a component!! Events for ensuring components etc.!
  {---Forward declarations---}
  TBoldAutoFormProviderRegistry = class;
  TBoldAutoFormProvider = class;
  TBoldAutoFormProviderClass = class of TBoldAutoFormProvider;
  TBoldAutoFormProviderHolder = class;

  {---TBoldAutoFormProvider---}
  TBoldAutoFormProvider = class(TBoldMemoryManagedObject)
  private
    fBoldHandle: TBoldReferenceHandle;        // Later
    fElement: TBoldElement;
    function GetElementIsList: boolean;
    function GetHandleIsList: boolean;
    function GetElementTypeInfo: TBoldElementTypeInfo;
  protected
    procedure DoEnsureComponents; virtual;
    procedure DoGenerateForm; virtual;
    procedure EnsureComponents; virtual; abstract;
    procedure EnsureForm; virtual;
    procedure EnsureHandle; virtual; abstract;
    function GetForm: TForm; virtual; abstract;
    function GetFormClass: TFormClass; virtual; abstract;
    function GetTarget: TWinControl; virtual; abstract;
    function ModelNameOrDefault(const DefaultName: string): string;
    procedure PostEnsureComponents; virtual;
    procedure PostGenerateAutoForm; virtual;
    procedure PreEnsureComponents; virtual;
    procedure PreGenerateAutoForm; virtual;
    procedure SetForm(Value: TForm); virtual; abstract;
    property Element: TBoldElement read fElement;
    property ElementIsList: boolean read GetElementIsList;
    property Form: TForm read GetForm write SetForm;
    property HandleIsList: boolean read GetHandleIsList;
    property Target: TWinControl read GetTarget;
    property ElementTypeInfo: TBoldElementTypeInfo read GetElementTypeInfo;
  public
    constructor Create(BoldElement: TBoldElement); virtual;
    function GenerateAutoForm: TForm; virtual;
    property BoldHandle: TBoldReferenceHandle read fBoldHandle write fBoldHandle;
    property FormClass: TFormClass read GetFormClass;
  end;

  {---TBoldAutoFormProviderHolder---}
  TBoldAutoFormProviderHolder = class(TBoldMemoryManagedObject)
  private
    fClass: TBoldAutoFormProviderClass;
    fValueType: TBoldValueTypeSet;
    fElementClass: TBoldElementClass;
    fIsListProvider: boolean;
  public
    constructor Create(ValueType: TBoldValueTypeSet; ElementClass: TBoldElementClass; ProviderClass: TBoldAutoFormProviderClass; IsList: boolean);
    property ProviderClass: TBoldAutoFormProviderClass read fClass;
    property ElementClass: TBoldElementClass read fElementClass;
    property ValueType: TBoldValueTypeSet read fValueType;
    property IsListProvider: boolean read fIsListProvider;
  end;

  {---TBoldAutoFormProviderRegistry---}
  TBoldAutoFormProviderRegistry = class(TBoldIndexableList)
  private
    function GetHolders(const index: integer): TBoldAutoFormProviderHolder;
  protected
    function GetMatchingProvider(BoldElement: TBoldElement): TBoldAutoFormProviderHolder;
  public
    constructor Create;
    function FormForElement(BoldElement: TBoldElement): TForm;
    procedure RegisterListProvider(ValueType: TBoldValueTypeSet; ElementClass: TBoldElementClass; ProviderClass: TBoldAutoFormProviderClass);
    procedure RegisterProvider(ValueType: TBoldValueTypeSet; ElementClass: TBoldElementClass; ProviderClass: TBoldAutoFormProviderClass);
    procedure UnregisterProvider(ProviderClass: TBoldAutoFormProviderClass);
    property Count;
    property Holders[const index: integer]: TBoldAutoFormProviderHolder read GetHolders;
  end;

function AutoFormProviderRegistry: TBoldAutoFormProviderRegistry;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldSystemRT,
  BoldSystem,
  BoldHandles,
  BoldHashIndexes;

var
  IX_ProviderClass: integer = -1;

type
{---TProviderClassIndex---}
  TProviderClassIndex = class(TBoldClassHashIndex)
  protected
    function ItemAsKeyClass(Item: TObject): TClass; override;
  end;

var
  G_AutoFormProviderRegistry: TBoldAutoFormProviderRegistry;

{---global access methods---}
function AutoFormProviderRegistry: TBoldAutoFormProviderRegistry;
begin
  if not Assigned(G_AutoFormProviderRegistry) then
    G_AutoFormProviderRegistry := TBoldAutoFormProviderRegistry.Create;
  Result := G_AutoFormProviderRegistry;
end;

{---TProviderClassIndex---}
function TProviderClassIndex.ItemAsKeyClass(Item: TObject): TClass;
begin
  Result := TBoldAutoFormProviderHolder(Item).ProviderClass;
end;

{---TBoldAutoFormProviderRegistry---}
constructor TBoldAutoFormProviderRegistry.Create;
begin
  inherited;
  SetIndexCapacity(1);
  SetIndexVariable(IX_ProviderClass, AddIndex(TProviderClassIndex.Create));
end;

function TBoldAutoFormProviderRegistry.GetHolders(const index: integer): TBoldAutoFormProviderHolder;
begin
  Result := Items[index] as TBoldAutoFormProviderHolder;
end;

function TBoldAutoFormProviderRegistry.GetMatchingProvider(BoldElement: TBoldElement): TBoldAutoFormProviderHolder;
var
  i: integer;
  MatchType: TBoldValueTypeSet;
  LookForListProvider: boolean;
function GetSpan(AFPHolder: TBoldAutoFormProviderHolder; BoldElement: TBoldElement): integer;
var
  ElementClass: TClass;
begin
  Result := 0;
  ElementClass := BoldElement.ClassType;
  while ElementClass <> AFPHolder.ElementClass do
  begin
    Inc(Result);
    ElementClass := ElementClass.ClassParent;
  end;
end;

function ClosestSpan(AFPHolder1, AFPHolder2: TBoldAutoFormProviderHolder; BoldElement: TBoldElement): TBoldAutoFormProviderHolder;
begin
  if GetSpan(AFPHolder1, BoldElement) < GetSpan(AFPHolder2, BoldElement) then
    Result := AFPHolder1
  else
    Result := AFPHolder2;
end;

begin
  Result := nil;
  MatchType := bvtSystem;  // Set to avoid compiler warning

  LookForListProvider := BoldElement.BoldType.BoldValueType = bvtList;
  if LookForListProvider then
    MatchType := TBoldListTypeInfo(BoldElement.BoldType).ListElementTypeInfo.BoldValueType;

  // Match on classtype
  for i := Count - 1  downto 0 do
    if (BoldElement is Holders[i].ElementClass) and (Holders[i].IsListProvider = LookForListProvider) then
    begin
      Result := Holders[i];
      Break;
      if not Assigned(Result) then
        Result := Holders[i]
      else
        Result := ClosestSpan(Result, Holders[i], BoldElement);
    end;

  // Match on BoldValueType
  if not Assigned(Result) then
    for i := Count -1  downto 0 do
      if (BoldElement.BoldType.BoldValueType = Holders[i].ValueType) or (LookForListProvider and (MatchType = Holders[i].ValueType)) then
      begin
        Result := Holders[i];
        Break;
      end;
end;

function TBoldAutoFormProviderRegistry.FormForElement(BoldElement: TBoldElement): TForm;
var
  Holder: TBoldAutoFormProviderHolder;
begin
  Result := nil;
  Holder := GetMatchingProvider(BoldElement);
  if Assigned(Holder) then
    with Holder.ProviderClass.Create(BoldElement) do
    try
      Result := GenerateAutoForm;
    finally
      Free;
    end;
end;

procedure TBoldAutoFormProviderRegistry.RegisterProvider(ValueType: TBoldValueTypeSet; ElementClass: TBoldElementClass; ProviderClass: TBoldAutoFormProviderClass);
begin
  Add(TBoldAutoFormProviderHolder.Create(ValueType, ElementClass, ProviderClass, False));
end;

procedure TBoldAutoFormProviderRegistry.RegisterListProvider(ValueType: TBoldValueTypeSet; ElementClass: TBoldElementClass; ProviderClass: TBoldAutoFormProviderClass);
begin
  Add(TBoldAutoFormProviderHolder.Create(ValueType, ElementClass, ProviderClass, True));
end;

procedure TBoldAutoFormProviderRegistry.UnregisterProvider(ProviderClass: TBoldAutoFormProviderClass);
begin
  Remove(TBoldAutoFormProviderHolder(TProviderClassIndex(Indexes[IX_ProviderClass]).FindByClass(ProviderClass)))
end;

{---TBoldAutoFormProviderHolder---}
constructor TBoldAutoFormProviderHolder.Create(ValueType: TBoldValueTypeSet; ElementClass: TBoldElementClass; ProviderClass: TBoldAutoFormProviderClass; IsList: boolean);
begin
  inherited Create;
  fClass := ProviderClass;
  fValueType := ValueType;
  fElementClass := ElementClass;
  fIsListProvider := IsList;
end;

{---TBoldAutoFormProvider---}
constructor TBoldAutoFormProvider.Create(BoldElement: TBoldElement);
begin
  inherited Create;
  fElement := BoldElement;
end;

function TBoldAutoFormProvider.ModelNameOrDefault(const DefaultName: string): string;
begin
  if Element is TBoldMember and
    assigned(TBoldMember(Element).BoldMemberRTInfo) then
    result := TBoldMember(Element).BoldMemberRTInfo.Modelname
  else if element is TBoldObject then
    result := TBoldObject(Element).BoldClassTypeInfo.Modelname
  else if element is TBoldSystem then
    result := TBoldSystem(Element).BoldSystemTypeInfo.Modelname
  else
    Result := DefaultName;
end;

function TBoldAutoFormProvider.GetElementIsList: boolean;
begin
  Result := ElementTypeInfo.BoldValueType = bvtList;
end;

function TBoldAutoFormProvider.GetElementTypeInfo: TBoldElementTypeInfo;
begin
  Result := Element.BoldType;
end;

function TBoldAutoFormProvider.GetHandleIsList: boolean;
begin
  Result := assigned(BoldHandle.StaticBoldType) and
    (BoldHandle.StaticBoldType.BoldValueType = bvtList);
end;

procedure TBoldAutoFormProvider.DoEnsureComponents;
begin
  PreEnsureComponents; //Empty!
  EnsureComponents;
  PostEnsureComponents;
end;

procedure TBoldAutoFormProvider.PreEnsureComponents;
begin
end;

procedure TBoldAutoFormProvider.PostEnsureComponents;
begin
  if Assigned(BoldHandle) then
    BoldHandle.Value := Element
  else
  begin
    // Cannot use FreeAndNil on property - there's no variable to access.
    Form.Free;
    Form := nil;
    raise EBoldInternal.CreateFmt(sMissingBoldHandle, [ClassName]);
  end;
end;

procedure TBoldAutoFormProvider.EnsureForm;
begin
  if Assigned(FormClass) then
    Form := FormClass.Create(Application)
  else
    Form := nil;
end;

procedure TBoldAutoFormProvider.DoGenerateForm;
begin
  EnsureForm;
  if Assigned(Form) then
  begin
    EnsureHandle;
    DoEnsureComponents;
  end;
end;

function TBoldAutoFormProvider.GenerateAutoForm: TForm;
begin
  PreGenerateAutoForm;
  DoGenerateForm;
  if Assigned(Form) then
    PostGenerateAutoForm;
  Result := Form;
end;

procedure TBoldAutoFormProvider.PreGenerateAutoForm;
begin
  // Intentionally left blank
end;

procedure TBoldAutoFormProvider.PostGenerateAutoForm;
begin
  // Intentionally left blank
end;

initialization

finalization
  FreeAndNil(G_AutoFormProviderRegistry);

end.

