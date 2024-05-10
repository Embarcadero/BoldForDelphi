
{ Global compiler directives }
{$include bold.inc}
unit BoldPropertyMapper;

interface

uses
  Classes,
  BoldEnvironmentVCL,
  BoldElements,
  BoldHandles,
  BoldControlPack,
  BoldVariantControlPack,
  BoldElementHandleFollower,
  BoldOclVariables;

type
  { forward declarations }
  TBoldPropertyMapper = class;
  TBoldPropertyMapping = class;
  TBoldPropertyMappingCollection = class;

  { TBoldPropertyMappingCollection }
  TBoldPropertyMappingCollection = class(TCollection)
  private
    FPropertyMapper: TBoldPropertyMapper;
    function GetItem(Index: Integer): TBoldPropertyMapping;
    procedure SetItem(Index: Integer; Value: TBoldPropertyMapping);
  protected
    function GetOwner: TPersistent; override;
    property PropertyMapper: TBoldPropertyMapper read fPropertyMapper;
  public
    constructor Create(PropertyMapper: TBoldPropertyMapper);
    function Add: TBoldPropertyMapping;
    property Items[Index: Integer]: TBoldPropertyMapping read GetItem write SetItem; default;
  end;

  { TBoldPropertyMapping }
  TBoldPropertyMapping = class(TCollectionItem)
  private
    FVCLProperty: String;
    FVCLComponent: TComponent;
    FOnExit: TNotifyEvent;
    FReadOnly: Boolean;
    FHandleFollower: TBoldElementHandleFollower;
    FBoldProperties: TBoldVariantFollowerController;
    fMappingCollection: TBoldPropertyMappingCollection;
    function GetPropertyMapper: TBoldPropertyMapper;
    procedure ConvertRelativeProp(StartInstance: TObject; PropNamePath: String; var LastObject: TObject; var PropName: String);
    procedure SetRelativePropValue(StartInstance: TObject; PropNamePath: string; Value: TBoldElement);
    procedure SetBoldProperties(const Value: TBoldVariantFollowerController);
    function GetContextType: TBoldElementTypeInfo;
  protected
    function GetBoldHandle: TBoldElementHandle;
    procedure SetBoldHandle(Value: TBoldElementHandle);
    procedure SetPropertyValue(Follower: TBoldFollower); virtual;
    procedure EnsureValidVCLProperty; virtual;
    procedure HookOnExit; virtual;
    procedure UnhookOnExit; virtual;
    procedure DoOnExit(Sender: TObject); virtual;
    function GetDisplayName: string; override;
    procedure SetReadOnly(const Value: Boolean); virtual;
    procedure SetVCLComponent(const Value: TComponent); virtual;
    procedure SetVCLProperty(const Value: String); virtual;
    property MappingCollection: TBoldPropertyMappingCollection read fMappingCollection;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property PropertyMapper: TBoldPropertyMapper read GetPropertyMapper;
    property HandleFollower: TBoldElementHandleFollower read FHandleFollower;
  published
    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldVariantFollowerController read FBoldProperties write SetBoldProperties;
    property VCLComponent: TComponent read FVCLComponent write SetVCLComponent;
    property VCLProperty: String read FVCLProperty write SetVCLProperty;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default true;
  end;

  { TBoldPropertyMapper }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldPropertyMapper = class(TComponent)
  private
    FMappingCollection: TBoldPropertyMappingCollection;
    fEnabled: Boolean;
    procedure SetMappingCollection(const Value: TBoldPropertyMappingCollection);
    procedure SetEnabled(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property MappingCollection: TBoldPropertyMappingCollection read FMappingCollection write SetMappingCollection;
    property Enabled: Boolean read fEnabled write SetEnabled;
  end;

  function CreatePropertyControllerMapping(aBoldHandle: TBoldElementHandle; aExpression: string;
    aVCLComponent: TComponent; aVCLProperty: string; aReadOnly: boolean = true; aBoldVariables: TBoldOclVariables = nil): TBoldPropertyMapper;

implementation

uses
  SysUtils,
  TypInfo,
  BoldControlPackDefs,
  BoldControlsDefs,
  BoldSystem,
  Variants,
  BoldGuard;

function CreatePropertyControllerMapping(aBoldHandle: TBoldElementHandle; aExpression: string;
  aVCLComponent: TComponent; aVCLProperty: string; aReadOnly: boolean; aBoldVariables: TBoldOclVariables): TBoldPropertyMapper;
var
  lBoldPropertyMapping: TBoldPropertyMapping;
begin
  result := TBoldPropertyMapper.Create(aVCLComponent);
  lBoldPropertyMapping:= result.MappingCollection.Add;
  lBoldPropertyMapping.BoldHandle:= aBoldHandle;
  lBoldPropertyMapping.BoldProperties.Expression:= aExpression;
  lBoldPropertyMapping.BoldProperties.Variables := aBoldVariables;
  lBoldPropertyMapping.VCLComponent:= aVCLComponent;
  lBoldPropertyMapping.VCLProperty:= aVCLProperty;
  lBoldPropertyMapping.ReadOnly:= aReadOnly;
end;

{ TBoldPropertyMapper }

constructor TBoldPropertyMapper.Create(AOwner: TComponent);
begin
  inherited;
  FMappingCollection := TBoldPropertyMappingCollection.Create(Self);
  fEnabled:= true;
end;

destructor TBoldPropertyMapper.Destroy;
begin
  FreeAndNil(FMappingCollection);
  inherited;
end;

procedure TBoldPropertyMapper.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if Assigned(MappingCollection) and (not (csDestroying in ComponentState)) then
    for I := 0 to MappingCollection.Count - 1 do
      with MappingCollection[I] do
        if (AComponent = VCLComponent) and (Operation = opRemove) then
          VCLComponent := nil;
end;

procedure TBoldPropertyMapper.SetEnabled(const Value: Boolean);
var
  i: integer;
begin
  if value <> fEnabled then
  begin
    fEnabled := Value;
    if Value then    
      for I := 0 to MappingCollection.Count-1  do
        MappingCollection[i].HandleFollower.Follower.MarkValueOutOfDate      
  end;
end;

procedure TBoldPropertyMapper.SetMappingCollection(const Value: TBoldPropertyMappingCollection);
begin
  FMappingCollection.Assign(Value);
end;

{ TBoldPropertyMapping }

constructor TBoldPropertyMapping.Create(Collection: TCollection);
begin
  inherited;
  FBoldProperties := TBoldVariantFollowerController.Create(nil);
  FBoldProperties.AfterMakeUptoDate := SetPropertyValue;
  fBoldProperties.OnGetContextType := GetContextType;
  FHandleFollower := TBoldElementHandleFollower.Create(nil, FBoldProperties);
  fMappingCollection := Collection as TBoldPropertyMappingCollection;
  FOnExit := nil;
  FReadOnly := true;
end;

procedure TBoldPropertyMapping.EnsureValidVCLProperty;
var
  PropList: TPropList;
  Count, I: Integer;
  Found: Boolean;
begin
  if not Assigned(VCLComponent) then
    VCLProperty := '';
  exit;
  Found := False;
  I := 0;
  if Assigned(VCLComponent) then
  begin
    Count := GetPropList(VCLComponent.ClassInfo, BoldPropertiesController_SupportedPropertyTypes, @PropList);
    while (I < Count) and (not Found) do
    begin
      Found := String(PropList[I]^.Name) = VCLProperty;
      Inc(I);
    end;
  end;
  if not Found then
    VCLProperty := '';
end;

procedure TBoldPropertyMapping.SetVCLComponent(const Value: TComponent);
var
  AllowHookUnHook: Boolean;
begin
  AllowHookUnHook := assigned(value) and
                     not ((csDesigning in Value.ComponentState) or
                     (Collection.Count > 1));

  if AllowHookUnHook then
    UnHookOnExit;
  FVCLComponent := Value;
  if AllowHookUnHook then
    HookOnExit;
  if Assigned(Value) then
    (Collection as TBoldPropertyMappingCollection).PropertyMapper.FreeNotification(Value);
  EnsureValidVCLProperty;
end;

procedure TBoldPropertyMapping.SetBoldHandle(Value: TBoldElementHandle);
begin
  FHandleFollower.BoldHandle := Value;
end;

procedure TBoldPropertyMapping.SetBoldProperties(
  const Value: TBoldVariantFollowerController);
begin
  FBoldProperties := Value;
end;

procedure TBoldPropertyMapping.SetVCLProperty(const Value: String);
begin
  FVCLProperty := Value;
end;

procedure TBoldPropertyMapping.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

destructor TBoldPropertyMapping.Destroy;
begin
  FreeAndNil(FHandleFollower);
  FreeAndNil(FBoldProperties);
  inherited;
end;

procedure TBoldPropertyMapping.DoOnExit(Sender: TObject);
begin
  if (not ReadOnly) and BoldProperties.MayModify(HandleFollower.Follower) then
  begin
    BoldProperties.MayHaveChanged(GetPropValue(VCLComponent, VCLProperty, True), HandleFollower.Follower);
    if BoldProperties.ApplyPolicy = bapExit then
      HandleFollower.Follower.Apply;
  end;
  if Assigned(FOnExit) then
    FOnExit(Sender);
end;

procedure TBoldPropertyMapping.HookOnExit;
var
  DoOnExitMethod: TNotifyEvent;
begin
  if Assigned(VCLComponent) and Assigned(GetPropInfo(VCLComponent.ClassInfo, 'OnExit')) then
  begin
    FOnExit := TNotifyEvent(Typinfo.GetMethodProp(VCLComponent, 'OnExit'));
    DoOnExitMethod := DoOnExit;
    Typinfo.SetMethodProp(VCLComponent, 'OnExit', TMethod(DoOnExitMethod));
  end;
end;

procedure TBoldPropertyMapping.UnhookOnExit;
begin
  if Assigned(VCLComponent) and Assigned(GetPropInfo(VCLComponent.ClassInfo, 'OnExit')) then
    Typinfo.SetMethodProp(VCLComponent, 'OnExit', TMethod(FOnExit));
end;

procedure TBoldPropertyMapping.SetPropertyValue(Follower: TBoldFollower);
var
  ie: TBoldIndirectElement;
  BoldGuard: IBoldGuard;
  sendElement: TBoldElement;
begin
  if (csDesigning in PropertyMapper.ComponentState) then
    exit;
  if not PropertyMapper.Enabled then
    exit;
  BoldGuard := TBoldGuard.Create(ie);
  ie := TBoldIndirectElement.Create;
  if assigned(BoldHandle) and
     assigned(Follower.element) then
  begin
    Follower.Element.EvaluateExpression(BoldProperties.Expression, ie, false, BoldProperties.VariableList);
    SendElement := ie.Value;
  end
  else
    SendElement := nil;
  if Assigned(MappingCollection) then
    SetRelativePropValue(VCLComponent, VCLProperty, SendElement);
end;

function TBoldPropertyMapping.GetPropertyMapper: TBoldPropertyMapper;
begin
  Result := MappingCollection.PropertyMapper;
end;

procedure TBoldPropertyMapping.ConvertRelativeProp(StartInstance: TObject;
  PropNamePath: String; var LastObject: TObject; var PropName: String);
var
  I, ColIndex,
  OpenBracketPos: Integer;
  Path: TStringList;
  PathItem: String;
  BoldGuard: IBoldGuard;
begin
  BoldGuard := TBoldGuard.Create(Path);
  Path := TStringList.Create;
  Path.CommaText := StringReplace(PropNamePath, '.', ',', [rfReplaceAll]);

  LastObject := StartInstance;
  for I := 0 to Path.Count - 1 do
  begin
    if not Assigned(LastObject) then
      Exit;
    PathItem := Path[I];
    OpenBracketPos := Pos('[', PathItem);
    if OpenBracketPos = 0 then
    begin
      if (I < Path.Count - 1)

      and (GetPropInfo(LastObject.ClassInfo, PathItem)^.PropType^.Kind = tkClass) then
      begin
        LastObject := TObject(Typinfo.GetOrdProp(LastObject, PathItem))
      end;
    end
    else
    begin
      LastObject := TCollection(Typinfo.GetOrdProp(LastObject, Copy(PathItem, 1, OpenBracketPos - 1)));
      ColIndex := StrToInt(Copy(PathItem, OpenBracketPos + 1, Length(PathItem) - OpenBracketPos - 1));
      LastObject := TCollection(LastObject).Items[ColIndex];
    end;
  end;
  PropName := Path[Path.Count - 1];
end;

procedure TBoldPropertyMapping.SetRelativePropValue(StartInstance: TObject;
  PropNamePath: string;  Value: TBoldElement);
var
  LastObject,
  PropertyObj: TObject;
  PropName: String;
  ObjPoint: LongInt;
  VarValue: Variant;
  i: integer;
  List: TBoldList;
  Strings: TStrings;
  TypeKind: TTypeKind;
  PropInfo: PPropInfo;
begin
  if PropNamePath = '' then
    Exit;

  ConvertRelativeProp(StartInstance, PropNamePath, LastObject, PropName);
  if not Assigned(LastObject) then
    Exit;
  PropInfo := GetPropInfo(LastObject.ClassInfo, PropName);
  if not Assigned(PropInfo) then
    Exit;
  TypeKind := PropInfo^.PropType^.Kind;

  if Assigned(Value) then
  begin
    VarValue := Value.AsVariant;
  end
  else
    case TypeKind of
      tkEnumeration: VarValue := 0;
      tkInteger: VarValue := 0;
      else VarValue := BoldProperties.NilRepresentation;
    end;
  if VarType(VarValue) = varBoolean then
  begin
    if VarValue then
      VarValue := 'True'
    else
      VarValue := 'False';
  end;

  if TypeKind = tkClass then
  begin
    PropertyObj := TObject(Typinfo.GetOrdProp(LastObject, PropName));
    if PropertyObj is TStrings then
    begin
      Strings := (PropertyObj as TStrings);
      Strings.Clear;
      if BoldTestType(value, TBoldList) then
      begin
        List := (value as TBoldList);
        for i := 0 to List.Count - 1 do
          Strings.Add(List.Elements[i].Stringrepresentation[BoldProperties.Representation]);
      end
      else
        Strings.Text := VarValue;
    end
    else
    begin
      ObjPoint := LongInt(PropertyMapper.Owner.FindComponent(VarValue));
      SetOrdProp(LastObject, GetPropInfo(LastObject.ClassInfo, VCLProperty), ObjPoint);
    end;
  end
  else if TypeKind = tkInteger then
    try
     if not VarIsNull(VarValue) then
      SetOrdProp(LastObject, PropName, VarValue)
    except
      on E: Exception do
        raise Exception.CreateFmt('Could not set the integer %s property to value %s. (%s)', [PropNamePath, VarValue, e.Message]);
    end
  else if TypeKind = tkFloat then
    try
     if VarIsFloat(VarValue) then
       SetPropValue(LastObject, PropName, VarValue);
    except
      on E: Exception do
        raise Exception.CreateFmt('Could not set the float %s property to value %s. (%s)', [PropNamePath, VarValue, e.Message]);
    end
  else
    try
      SetPropValue(LastObject, PropName, VarValue);
    except
      on E: Exception do
        raise Exception.CreateFmt('Could not set the %s property to value %s. (%s)', [PropNamePath, VarValue, e.Message]);
    end;
end;

procedure TBoldPropertyMapping.Assign(Source: TPersistent);
begin
  if source is TBoldPropertyMapping then
  begin
    VCLComponent := (Source as TBoldPropertyMapping).VCLComponent;
    VCLProperty := (Source as TBoldPropertyMapping).VCLProperty;
    ReadOnly := (Source as TBoldPropertyMapping).ReadOnly;
    BoldProperties.Assign((Source as TBoldPropertyMapping).BoldProperties);
    BoldHandle.Assign((Source as TBoldPropertyMapping).BoldHandle);
  end
  else
    inherited;
end;

function TBoldPropertyMapping.GetBoldHandle: TBoldElementHandle;
begin
  Result := FHandleFollower.BoldHandle;
end;

function TBoldPropertyMapping.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldPropertyMapping.GetDisplayName: string;
begin
  if assigned(VCLComponent) then
    result := VCLComponent.Name
  else
    result := '<No Comp>';

  if trim(VCLProperty) <> '' then
    result := result + '.' + trim(VCLProperty)
  else
    result := result + '.<No Prop>';

  if ReadOnly then
    result := result + ' (RO)'
  else
    result := result + ' (RW)';
end;

{ TBoldPropertyMappingCollection }

constructor TBoldPropertyMappingCollection.Create(PropertyMapper: TBoldPropertyMapper);
begin
  inherited Create(TBoldPropertyMapping);
  FPropertyMapper := PropertyMapper;
end;

function TBoldPropertyMappingCollection.Add: TBoldPropertyMapping;
begin
  Result := TBoldPropertyMapping(inherited Add);
end;

function TBoldPropertyMappingCollection.GetItem(Index: Integer): TBoldPropertyMapping;
begin
  Result := TBoldPropertyMapping(inherited GetItem(Index));
end;

function TBoldPropertyMappingCollection.GetOwner: TPersistent;
begin
  Result := FPropertyMapper;
end;

procedure TBoldPropertyMappingCollection.SetItem(Index: Integer; Value: TBoldPropertyMapping);
begin
  inherited SetItem(Index, Value);
end;

end.
