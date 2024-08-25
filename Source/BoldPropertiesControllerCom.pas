
{ Global compiler directives }
{$include bold.inc}
unit BoldPropertiesControllerCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  Classes,
  BoldEnvironmentVCL,
  BoldComObjectSpace_TLB, BoldClientElementSupport, BoldComClient,
  BoldHandlesCom,
  BoldControlPackCom,
  BoldStringControlPackCom,
  BoldElementHandleFollowerCom;

type
  { forward declarations }
  TBoldPropertiesControllerCom = class;
  TBoldDrivenPropertyCom = class;
  TBoldDrivenPropertyCollectionCom = class;

  { TBoldDrivenPropertyCollectionCom }
  TBoldDrivenPropertyCollectionCom = class(TCollection)
  private
    FPropertiesController: TBoldPropertiesControllerCom;
    function GetItem(Index: Integer): TBoldDrivenPropertyCom;
    procedure SetItem(Index: Integer; Value: TBoldDrivenPropertyCom);
  protected
    function GetOwner: TPersistent; override;
    property PropertiesController: TBoldPropertiesControllerCom read fPropertiesController;
  public
    constructor Create(PropertiesController: TBoldPropertiesControllerCom);
    function Add: TBoldDrivenPropertyCom;
    property Items[Index: Integer]: TBoldDrivenPropertyCom read GetItem write SetItem; default;
  end;

  { TBoldDrivenPropertyCom }
  TBoldDrivenPropertyCom = class(TCollectionItem)
  private
    FPropertyName: String;
    FVCLComponent: TComponent;
    FOnExit: TNotifyEvent;
    FReadOnly: Boolean;
    fDrivenProperties: TBoldDrivenPropertyCollectionCom;
    function GetPropertiesController: TBoldPropertiesControllerCom;
    procedure ConvertRelativeProp(StartInstance: TObject; PropNamePath: String; var LastObject: TObject; var PropName: String);
    procedure SetRelativePropValue(StartInstance: TObject; PropNamePath: string; Value: IBoldElement);
  protected
    procedure SetPropertyValue(Follower: TBoldFollowerCom); virtual;
    procedure EnsureValidPropertyName; virtual;
    procedure HookOnExit; virtual;
    procedure UnhookOnExit; virtual;
    procedure DoOnExit(Sender: TObject); virtual;
    function GetDisplayName: string; override;
    procedure SetReadOnly(const Value: Boolean); virtual;
    procedure SetVCLComponent(const Value: TComponent); virtual;
    procedure SetPropertyName(const Value: String); virtual;
    property DrivenProperties: TBoldDrivenPropertyCollectionCom read fDrivenProperties;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property PropertiesController: TBoldPropertiesControllerCom read GetPropertiesController;
  published
    property VCLComponent: TComponent read FVCLComponent write SetVCLComponent;
    property PropertyName: String read FPropertyName write SetPropertyName;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default true;
  end;

  { TBoldPropertiesControllerCom }
  TBoldPropertiesControllerCom = class(TComponent)
  private
    FHandleFollower: TBoldElementHandleFollowerCom;
    FBoldProperties: TBoldStringFollowerControllerCom;
    FDrivenProperties: TBoldDrivenPropertyCollectionCom;
    procedure SetDrivenProperties(const Value: TBoldDrivenPropertyCollectionCom);
    function GetContextType: IBoldElementTypeInfo;
  protected
    { Bold Awareness }
    function  GetBoldHandle: TBoldElementHandleCom;
    procedure SetBoldHandle(Value: TBoldElementHandleCom);
    procedure SetBoldProperties(Value: TBoldStringFollowerControllerCom);
    procedure SetPropertyValue(Follower: TBoldFollowerCom); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property HandleFollower: TBoldElementHandleFollowerCom read FHandleFollower;
  published
    property BoldHandle: TBoldElementHandleCom read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldStringFollowerControllerCom read FBoldProperties write SetBoldProperties;
    property DrivenProperties: TBoldDrivenPropertyCollectionCom read FDrivenProperties write SetDrivenProperties;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  BoldControlPackDefs,
  BoldControlsDefs,
  {$IFNDEF BOLDCOMCLIENT}
  BoldComObjectSpace_TLB,
  {$ENDIF}
  Variants,
  BoldGuard;

{ TBoldPropertiesControllerCom }

constructor TBoldPropertiesControllerCom.Create(AOwner: TComponent);
begin
  inherited;
  FBoldProperties := TBoldStringFollowerControllerCom.Create(Self);
  FHandleFollower := TBoldElementHandleFollowerCom.Create(Owner, FBoldProperties);

  FBoldProperties.AfterMakeUptoDate := SetPropertyValue;
  fBoldProperties.OnGetContextType := GetContextType;
  FDrivenProperties := TBoldDrivenPropertyCollectionCom.Create(Self);
end;

destructor TBoldPropertiesControllerCom.Destroy;
begin
  FreeAndNil(FHandleFollower);
  FreeAndNil(FDrivenProperties);
  FreeAndNil(FBoldProperties);
  inherited;
end;

procedure TBoldPropertiesControllerCom.SetBoldProperties(value: TBoldStringFollowerControllerCom);
begin
  FBoldProperties.Assign(Value);
end;

function TBoldPropertiesControllerCom.GetBoldHandle: TBoldElementHandleCom;
begin
  Result := FHandleFollower.BoldHandle;
end;

procedure TBoldPropertiesControllerCom.SetBoldHandle(Value: TBoldElementHandleCom);
begin
  FHandleFollower.BoldHandle := Value;
end;

procedure TBoldPropertiesControllerCom.SetPropertyValue(Follower: TBoldFollowerCom);
var
  I: Integer;
begin
  for I := 0 to DrivenProperties.Count-1 do
    DrivenProperties[I].SetPropertyValue(Follower);
end;

procedure TBoldPropertiesControllerCom.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TBoldPropertiesControllerCom.SetDrivenProperties(const Value: TBoldDrivenPropertyCollectionCom);
begin
  FDrivenProperties.Assign(Value);
end;

{ TBoldDrivenPropertyCom }

constructor TBoldDrivenPropertyCom.Create(Collection: TCollection);
begin
  inherited;
  fDrivenProperties := Collection as TBoldDrivenPropertyCollectionCom;
  FOnExit := nil;
  FReadOnly := true;
end;

procedure TBoldDrivenPropertyCom.EnsureValidPropertyName;



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
      Found := PropList[I]^.Name = PropertyName;
      Inc(I);
    end;
  end;
  if not Found then
    PropertyName := '';
end;

procedure TBoldDrivenPropertyCom.SetVCLComponent(const Value: TComponent);
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
    (Collection as TBoldDrivenPropertyCollectionCom).PropertiesController.FreeNotification(Value);
  EnsureValidPropertyName;
end;

procedure TBoldDrivenPropertyCom.SetPropertyName(const Value: String);
begin
  FPropertyName := Value;
end;

procedure TBoldDrivenPropertyCom.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TBoldDrivenPropertyCom.DoOnExit(Sender: TObject);
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

procedure TBoldDrivenPropertyCom.HookOnExit;
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

procedure TBoldDrivenPropertyCom.UnhookOnExit;
begin
  if Assigned(VCLComponent) and Assigned(GetPropInfo(VCLComponent.ClassInfo, 'OnExit')) then
    Typinfo.SetMethodProp(VCLComponent, 'OnExit', TMethod(FOnExit));
end;

procedure TBoldDrivenPropertyCom.SetPropertyValue(Follower: TBoldFollowerCom);
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
  sendElement: IBoldElement;
begin
  BoldGuard := TBoldGuard.Create(ie);
  ie := TBoldIndirectElement.Create;
  if assigned(PropertiesController.BoldHandle) and
     assigned(Follower.element) then
  begin
    Follower.Element.EvaluateExpression(PropertiesController.BoldProperties.Expression, ie);
    SendElement := ie.Value;
  end
  else
    SendElement := nil;
  SetRelativePropValue(VCLComponent, PropertyName, SendElement);
end;
{$ENDIF}

function TBoldDrivenPropertyCom.GetPropertiesController: TBoldPropertiesControllerCom;
begin
  Result := DrivenProperties.PropertiesController;
end;

procedure TBoldDrivenPropertyCom.ConvertRelativeProp(StartInstance: TObject;
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

procedure TBoldDrivenPropertyCom.SetRelativePropValue(StartInstance: TObject;
  PropNamePath: string;  Value: IBoldElement);
var
  LastObject,
  PropertyObj: TObject;
  PropName: String;
  ObjPoint: LongInt;
  VarValue: Variant;
  i: integer;
  List: IBoldList;
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
    VarValue := Value.GetAsVariant;
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
      if BoldTestType(value, IBoldList) then
      begin
        List := (value as IBoldList);
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
      SetOrdProp(LastObject, PropName, VarValue)
    except
      on E: Exception do
        raise Exception.CreateFmt('Could not set the integer %s property to value %s. (%s)', [PropNamePath, VarValue, e.Message]);
    end
  else
    try
      SetPropValue(LastObject, PropName, VarValue);
    except
      on E: Exception do
        raise Exception.CreateFmt('Could not set the %s property to value %s. (%s)', [PropNamePath, VarValue, e.Message]);
    end;
end;

procedure TBoldDrivenPropertyCom.Assign(Source: TPersistent);
begin
  if source is TBoldDrivenPropertyCom then
  begin
    VCLComponent := (Source as TBoldDrivenPropertyCom).VCLComponent;
    PropertyName := (Source as TBoldDrivenPropertyCom).PropertyName;
    ReadOnly := (Source as TBoldDrivenPropertyCom).ReadOnly;
  end
  else
    inherited;
end;

function TBoldDrivenPropertyCom.GetDisplayName: string;
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

{ TBoldDrivenPropertyCollectionCom }

constructor TBoldDrivenPropertyCollectionCom.Create(PropertiesController: TBoldPropertiesControllerCom);
begin
  inherited Create(TBoldDrivenPropertyCom);
  FPropertiesController := PropertiesController;
end;

function TBoldDrivenPropertyCollectionCom.Add: TBoldDrivenPropertyCom;
begin
  Result := TBoldDrivenPropertyCom(inherited Add);
end;

function TBoldDrivenPropertyCollectionCom.GetItem(Index: Integer): TBoldDrivenPropertyCom;
begin
  Result := TBoldDrivenPropertyCom(inherited GetItem(Index));
end;

function TBoldDrivenPropertyCollectionCom.GetOwner: TPersistent;
begin
  Result := FPropertiesController;
end;

procedure TBoldDrivenPropertyCollectionCom.SetItem(Index: Integer; Value: TBoldDrivenPropertyCom);
begin
  inherited SetItem(Index, Value);
end;

function TBoldPropertiesControllerCom.GetContextType: IBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

end.
