
{ Global compiler directives }
{$include bold.inc}
unit BoldPropertiesController;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
  BoldEnvironmentVCL,
  BoldElements,
  BoldHandles,
  BoldControlPack,
  BoldStringControlPack,
  BoldElementHandleFollower,
  BoldOclVariables;

type
  { forward declarations }
  TBoldPropertiesController = class;
  TBoldDrivenProperty = class;
  TBoldDrivenPropertyCollection = class;

  { TBoldDrivenPropertyCollection }
  TBoldDrivenPropertyCollection = class(TCollection)
  private
    FPropertiesController: TBoldPropertiesController;
    function GetItem(Index: Integer): TBoldDrivenProperty;
    procedure SetItem(Index: Integer; Value: TBoldDrivenProperty);
  protected
    function GetOwner: TPersistent; override;
    property PropertiesController: TBoldPropertiesController read fPropertiesController;
  public
    constructor Create(PropertiesController: TBoldPropertiesController);
    function Add: TBoldDrivenProperty;
    property Items[Index: Integer]: TBoldDrivenProperty read GetItem write SetItem; default;
  end;

  { TBoldDrivenProperty }
  TBoldDrivenProperty = class(TCollectionItem)
  private
    FPropertyName: String;
    FVCLComponent: TComponent;
    FOnExit: TNotifyEvent;
    FReadOnly: Boolean;
    fDrivenProperties: TBoldDrivenPropertyCollection;
    function GetPropertiesController: TBoldPropertiesController;
    procedure ConvertRelativeProp(StartInstance: TObject; PropNamePath: String; var LastObject: TObject; var PropName: String);
    procedure SetRelativePropValue(StartInstance: TObject; PropNamePath: string; Value: TBoldElement);
  protected
    procedure SetPropertyValue(Follower: TBoldFollower); virtual;
    procedure EnsureValidPropertyName; virtual;
    procedure HookOnExit; virtual;
    procedure UnhookOnExit; virtual;
    procedure DoOnExit(Sender: TObject); virtual;
    function GetDisplayName: string; override;
    procedure SetReadOnly(const Value: Boolean); virtual;
    procedure SetVCLComponent(const Value: TComponent); virtual;
    procedure SetPropertyName(const Value: String); virtual;
    property DrivenProperties: TBoldDrivenPropertyCollection read fDrivenProperties;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property PropertiesController: TBoldPropertiesController read GetPropertiesController;
  published
    property VCLComponent: TComponent read FVCLComponent write SetVCLComponent;
    property PropertyName: String read FPropertyName write SetPropertyName;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default true;
  end;

  { TBoldPropertiesController }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldPropertiesController = class(TComponent)
  private
    FHandleFollower: TBoldElementHandleFollower;
    FBoldProperties: TBoldStringFollowerController;
    FDrivenProperties: TBoldDrivenPropertyCollection;
    procedure SetDrivenProperties(const Value: TBoldDrivenPropertyCollection);
    function GetContextType: TBoldElementTypeInfo;
  protected
    { Bold Awareness }
    function  GetBoldHandle: TBoldElementHandle;
    procedure SetBoldHandle(Value: TBoldElementHandle);
    procedure SetBoldProperties(Value: TBoldStringFollowerController);
    procedure SetPropertyValue(Follower: TBoldFollower); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property HandleFollower: TBoldElementHandleFollower read FHandleFollower;
  published
    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldStringFollowerController read FBoldProperties write SetBoldProperties;
    property DrivenProperties: TBoldDrivenPropertyCollection read FDrivenProperties write SetDrivenProperties;
  end;

  procedure CreatePropertyControllerMapping(aBoldHandle: TBoldElementHandle; aExpression: string;
    aVCLComponent: TComponent; aPropertyName: string; aReadOnly: boolean = true; aBoldVariables: TBoldOclVariables = nil);

implementation

uses
  SysUtils,
  TypInfo,
  BoldControlPackDefs,
  BoldControlsDefs,
  {$IFNDEF BOLDCOMCLIENT}
  BoldSystem,
  {$ENDIF}
  Variants,
  BoldGuard;

procedure CreatePropertyControllerMapping(aBoldHandle: TBoldElementHandle; aExpression: string;
  aVCLComponent: TComponent; aPropertyName: string; aReadOnly: boolean; aBoldVariables: TBoldOclVariables);
var
  lBoldPropertiesController: TBoldPropertiesController;
  lBoldDrivenProperty: TBoldDrivenProperty;
begin
  lBoldPropertiesController:= TBoldPropertiesController.Create(aVCLComponent);
  lBoldPropertiesController.BoldHandle:= aBoldHandle;
  lBoldPropertiesController.BoldProperties.Expression:= aExpression;
  lBoldPropertiesController.BoldProperties.Variables := aBoldVariables;
  lBoldDrivenProperty:= lBoldPropertiesController.DrivenProperties.Add;
  lBoldDrivenProperty.VCLComponent:= aVCLComponent;
  lBoldDrivenProperty.PropertyName:= aPropertyName;
  lBoldDrivenProperty.ReadOnly:= aReadOnly;
end;

{ TBoldPropertiesController }

constructor TBoldPropertiesController.Create(AOwner: TComponent);
begin
  inherited;
  FBoldProperties := TBoldStringFollowerController.Create(Self);
  FHandleFollower := TBoldElementHandleFollower.Create(Owner, FBoldProperties);

  FBoldProperties.AfterMakeUptoDate := SetPropertyValue;
  fBoldProperties.OnGetContextType := GetContextType;
  FDrivenProperties := TBoldDrivenPropertyCollection.Create(Self);
end;

destructor TBoldPropertiesController.Destroy;
begin
  FreeAndNil(FHandleFollower);
  FreeAndNil(FDrivenProperties);
  FreeAndNil(FBoldProperties);
  inherited;
end;

procedure TBoldPropertiesController.SetBoldProperties(value: TBoldStringFollowerController);
begin
  FBoldProperties.Assign(Value);
end;

function TBoldPropertiesController.GetBoldHandle: TBoldElementHandle;
begin
  Result := FHandleFollower.BoldHandle;
end;

procedure TBoldPropertiesController.SetBoldHandle(Value: TBoldElementHandle);
begin
  FHandleFollower.BoldHandle := Value;
end;

procedure TBoldPropertiesController.SetPropertyValue(Follower: TBoldFollower);
var
  I: Integer;
begin
  for I := 0 to DrivenProperties.Count-1 do
    DrivenProperties[I].SetPropertyValue(Follower);
end;

procedure TBoldPropertiesController.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;
  if Assigned(DrivenProperties) and (not (csDestroying in ComponentState)) then
    for I := 0 to DrivenProperties.Count - 1 do
      with DrivenProperties[I] do
        if (AComponent = VCLComponent) and (Operation = opRemove) then
          VCLComponent := nil;
end;

procedure TBoldPropertiesController.SetDrivenProperties(const Value: TBoldDrivenPropertyCollection);
begin
  FDrivenProperties.Assign(Value);
end;

{ TBoldDrivenProperty }

constructor TBoldDrivenProperty.Create(Collection: TCollection);
begin
  inherited;
  fDrivenProperties := Collection as TBoldDrivenPropertyCollection;
  FOnExit := nil;
  FReadOnly := true;
end;

procedure TBoldDrivenProperty.EnsureValidPropertyName;



var
  PropList: TPropList;
  Count, I: Integer;
  Found: Boolean;
begin
  if not Assigned(VCLComponent) then
    PropertyName := '';
  exit;
  Found := False;
  I := 0;
  if Assigned(VCLComponent) then
  begin
    Count := GetPropList(VCLComponent.ClassInfo, BoldPropertiesController_SupportedPropertyTypes, @PropList);
    while (I < Count) and (not Found) do
    begin
      Found := String(PropList[I]^.Name) = PropertyName;
      Inc(I);
    end;
  end;
  if not Found then
    PropertyName := '';
end;

procedure TBoldDrivenProperty.SetVCLComponent(const Value: TComponent);
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
    (Collection as TBoldDrivenPropertyCollection).PropertiesController.FreeNotification(Value);
  EnsureValidPropertyName;
end;

procedure TBoldDrivenProperty.SetPropertyName(const Value: String);
begin
  FPropertyName := Value;
end;

procedure TBoldDrivenProperty.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TBoldDrivenProperty.DoOnExit(Sender: TObject);
begin
  if (not ReadOnly) and PropertiesController.BoldProperties.MayModify(PropertiesController.HandleFollower.Follower) then
  begin
    PropertiesController.BoldProperties.MayHaveChanged(GetPropValue(VCLComponent, PropertyName, True), PropertiesController.HandleFollower.Follower);
    if PropertiesController.BoldProperties.ApplyPolicy = bapExit then
      PropertiesController.HandleFollower.Follower.Apply;
  end;
  if Assigned(FOnExit) then
    FOnExit(Sender);
end;

procedure TBoldDrivenProperty.HookOnExit;
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

procedure TBoldDrivenProperty.UnhookOnExit;
begin
  if Assigned(VCLComponent) and Assigned(GetPropInfo(VCLComponent.ClassInfo, 'OnExit')) then
    Typinfo.SetMethodProp(VCLComponent, 'OnExit', TMethod(FOnExit));
end;

procedure TBoldDrivenProperty.SetPropertyValue(Follower: TBoldFollower);
{$IFDEF BOLDCOMCLIENT}
var
  value: IBoldElement;
begin
  if assigned(PropertiesController.BoldHandle) and
     assigned(Follower.element) then
  begin
    value := Follower.Element.EvaluateExpression(PropertiesController.BoldProperties.Expression);
    SetRelativePropValue(VCLComponent, PropertyName, value);
  end;
end;
{$ELSE}
var
  ie: TBoldIndirectElement;
  BoldGuard: IBoldGuard;
  sendElement: TBoldElement;
begin
  BoldGuard := TBoldGuard.Create(ie);
  ie := TBoldIndirectElement.Create;
  if assigned(PropertiesController.BoldHandle) and
     assigned(Follower.element) then
  begin
    Follower.Element.EvaluateExpression(PropertiesController.BoldProperties.Expression, ie, false, PropertiesController.BoldProperties.VariableList);
    SendElement := ie.Value;
  end
  else
    SendElement := nil;
  SetRelativePropValue(VCLComponent, PropertyName, SendElement);
end;
{$ENDIF}

function TBoldDrivenProperty.GetPropertiesController: TBoldPropertiesController;
begin
  Result := DrivenProperties.PropertiesController;
end;

procedure TBoldDrivenProperty.ConvertRelativeProp(StartInstance: TObject;
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

procedure TBoldDrivenProperty.SetRelativePropValue(StartInstance: TObject;
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
    {$IFDEF BOLDCOMCLIENT}
    VarValue := Value.AsVariant;
    {$ELSE}
    VarValue := Value.AsVariant;
    {$ENDIF}
  end
  else
    case TypeKind of
      tkEnumeration: VarValue := 0;
      tkInteger: VarValue := 0;
      else VarValue := PropertiesController.BoldProperties.NilStringRepresentation;
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
          Strings.Add(List.Elements[i].Stringrepresentation[PropertiesController.BoldProperties.Representation]);
      end
      else
        Strings.Text := VarValue;
    end
    else
    begin
      ObjPoint := LongInt(PropertiesController.Owner.FindComponent(VarValue));
      SetOrdProp(LastObject, GetPropInfo(LastObject.ClassInfo, PropertyName), ObjPoint);
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

procedure TBoldDrivenProperty.Assign(Source: TPersistent);
begin
  if source is TBoldDrivenProperty then
  begin
    VCLComponent := (Source as TBoldDrivenProperty).VCLComponent;
    PropertyName := (Source as TBoldDrivenProperty).PropertyName;
    ReadOnly := (Source as TBoldDrivenProperty).ReadOnly;
  end
  else
    inherited;
end;

function TBoldDrivenProperty.GetDisplayName: string;
begin
  if assigned(VCLComponent) then
    result := VCLComponent.Name
  else
    result := '<No Comp>';

  if trim(propertyName) <> '' then
    result := result + '.' + trim(PropertyName)
  else
    result := result + '.<No Prop>';

  if ReadOnly then
    result := result + ' (RO)'
  else
    result := result + ' (RW)';
end;

{ TBoldDrivenPropertyCollection }

constructor TBoldDrivenPropertyCollection.Create(PropertiesController: TBoldPropertiesController);
begin
  inherited Create(TBoldDrivenProperty);
  FPropertiesController := PropertiesController;
end;

function TBoldDrivenPropertyCollection.Add: TBoldDrivenProperty;
begin
  Result := TBoldDrivenProperty(inherited Add);
end;

function TBoldDrivenPropertyCollection.GetItem(Index: Integer): TBoldDrivenProperty;
begin
  Result := TBoldDrivenProperty(inherited GetItem(Index));
end;

function TBoldDrivenPropertyCollection.GetOwner: TPersistent;
begin
  Result := FPropertiesController;
end;

procedure TBoldDrivenPropertyCollection.SetItem(Index: Integer; Value: TBoldDrivenProperty);
begin
  inherited SetItem(Index, Value);
end;

function TBoldPropertiesController.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

end.
