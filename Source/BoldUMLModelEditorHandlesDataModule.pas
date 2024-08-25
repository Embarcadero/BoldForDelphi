unit BoldUMLModelEditorHandlesDataModule;

interface

uses
  System.SysUtils, System.Classes,
  VCL.Graphics,
  BoldStringControlPack, BoldElements,
  BoldControlPack, BoldCheckboxStateControlPack, BoldReferenceHandle,
  BoldVariableHandle, BoldDerivedHandle, BoldListHandle, BoldExpressionHandle,
  BoldSubscription, BoldHandles, BoldRootedHandles, BoldAbstractListHandle,
  BoldCursorHandle,
  BoldUMLModel,
  BoldModel,
  BoldTypeNameDictionary, BoldVariantControlPack,
  Vcl.StdCtrls;

type
  TdmBoldUMLModelEditorHandles = class(TDataModule)
    bchAttributePMapperNames: TBoldCursorHandle;
    behAllDataTypes: TBoldExpressionHandle;
    blhClassAssociations: TBoldListHandle;
    blhOclExpressions: TBoldListHandle;
    blhTypesForAttribute: TBoldListHandle;
    bdhTypesForAttribute: TBoldDerivedHandle;
    bchTypesForAttribute: TBoldCursorHandle;
    brhRoot: TBoldReferenceHandle;
    behBoldified: TBoldExpressionHandle;
    brhCopyCut: TBoldReferenceHandle;
    brhCurrentElement: TBoldReferenceHandle;
    bvhOptimisticLocking: TBoldVariableHandle;
    behModel: TBoldExpressionHandle;
    blhModelAssociations: TBoldListHandle;
    blhModelClasses: TBoldListHandle;
    behClass: TBoldExpressionHandle;
    behClassSuperClass: TBoldExpressionHandle;
    bvhTableMapping: TBoldVariableHandle;
    bcrTableMapping: TBoldCursorHandle;
    blhClassOperations: TBoldListHandle;
    blhAllSuperclasses: TBoldListHandle;
    behClassIsRootClass: TBoldExpressionHandle;
    bcrAutoCreated: TBoldAsCheckBoxStateRenderer;
    blhClassAssociationEnds: TBoldListHandle;
    blhClassAttributes: TBoldListHandle;
    blhClassMembers: TBoldListHandle;
    behAttribute: TBoldExpressionHandle;
    blhAllDataTypes: TBoldListHandle;
    bvhDelphiProperty: TBoldVariableHandle;
    bchDelphiProperty: TBoldCursorHandle;
    bvhAttributeKind: TBoldVariableHandle;
    bdhAttributePMapperNames: TBoldDerivedHandle;
    bchDelphiFunctionType: TBoldCursorHandle;
    bvhDelphiFunctionType: TBoldVariableHandle;
    blhAllParameterKind: TBoldListHandle;
    blhAllOwnerScope: TBoldListHandle;
    blhParameters: TBoldListHandle;
    behOperation: TBoldExpressionHandle;
    behAssociation: TBoldExpressionHandle;
    blhAssociationAssociationEnds: TBoldListHandle;
    bchMultiplicityValues: TBoldCursorHandle;
    bvhMultiplicityValues: TBoldVariableHandle;
    blhAssociationEndQualifiers: TBoldListHandle;
    behAssociationEnd: TBoldExpressionHandle;
    behAssociationEndType: TBoldExpressionHandle;
    bvhDeleteActions: TBoldVariableHandle;
    bchDeleteActions: TBoldCursorHandle;
    blhAllAggregationKind: TBoldListHandle;
    blhAllChangeabilityKind: TBoldListHandle;
    behParameter: TBoldExpressionHandle;
    brhPackage: TBoldExpressionHandle;
    blhPackageClasses: TBoldListHandle;
    blhPackageAssociations: TBoldListHandle;
    behHighestSeverity: TBoldExpressionHandle;
    brhDataType: TBoldExpressionHandle;
    blhAllVisibilityKind: TBoldListHandle;
    blhAllClasses: TBoldListHandle;
    bsrRedOnAutocreated: TBoldAsStringRenderer;
    bchAttributeKind: TBoldCursorHandle;
    BoldAsVariantRenderer1: TBoldAsVariantRenderer;
    blhPackages: TBoldListHandle;
    blhAssociations: TBoldListHandle;
    behModelClasses: TBoldExpressionHandle;
    behModelAssociations: TBoldExpressionHandle;
    behClassFeatures: TBoldExpressionHandle;
    bchClassFeatures: TBoldCursorHandle;
    bcrBooleanToCheckBox: TBoldAsCheckBoxStateRenderer;
    bchOptimisticLocking: TBoldCursorHandle;
    behOclExpressions: TBoldExpressionHandle;
    behClassOperations: TBoldExpressionHandle;
    bsrNiceCRRenderer: TBoldAsStringRenderer;
    rDataTypePMapper: TBoldAsVariantRenderer;
    behClassAssociations: TBoldExpressionHandle;
    behClassAssociationEnds: TBoldExpressionHandle;
    behClassAttributes: TBoldExpressionHandle;
    bdhSuperClasses: TBoldDerivedHandle;
    blhSuperClasses: TBoldListHandle;
    behDataTypes: TBoldExpressionHandle;
    bchDataTypes: TBoldCursorHandle;
    behPackages: TBoldExpressionHandle;
    behAssociations: TBoldExpressionHandle;
    rDataTypeAttributeType: TBoldAsVariantRenderer;
    blhConcreteTypesForAttribute: TBoldListHandle;
    procedure bdhTypesForAttributeDeriveAndSubscribe(Sender: TComponent;
      RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
      Subscriber: TBoldSubscriber);
    procedure bdhAttributePMapperNamesDeriveAndSubscribe(Sender: TComponent;
      RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
      Subscriber: TBoldSubscriber);
    procedure BoldAsVariantRenderer1SetColor(aFollower: TBoldFollower;
      var AColor: TColor);
    procedure BoldAsVariantRenderer1SetFont(aFollower: TBoldFollower;
      AFont: TFont);
    procedure DataModuleCreate(Sender: TObject);
    function bcrBooleanToCheckBoxGetAsCheckBoxState(
      aFollower: TBoldFollower): TCheckBoxState;
    function bcrBooleanToCheckBoxMayModify(aFollower: TBoldFollower): Boolean;
    procedure bcrBooleanToCheckBoxSetAsCheckBoxState(aFollower: TBoldFollower;
      newValue: TCheckBoxState);
    procedure bcrBooleanToCheckBoxSubscribe(aFollower: TBoldFollower;
      Subscriber: TBoldSubscriber);
    function bsrNiceCRRendererGetAsString(aFollower: TBoldFollower): string;
    procedure bsrNiceCRRendererSubscribe(aFollower: TBoldFollower;
      Subscriber: TBoldSubscriber);
    function rDataTypePMapperGetAsVariant(AFollower: TBoldFollower): Variant;
    function bcrAutoCreatedGetAsCheckBoxState(
      aFollower: TBoldFollower): TCheckBoxState;
    procedure bcrAutoCreatedSetAsCheckBoxState(aFollower: TBoldFollower;
      newValue: TCheckBoxState);
    procedure bdhSuperClassesDeriveAndSubscribe(Sender: TComponent;
      RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
      Subscriber: TBoldSubscriber);
    function rDataTypeAttributeTypeGetAsVariant(
      AFollower: TBoldFollower): Variant;
  private
    fModelHandle: TBoldModel;
    fIgnoreModelChanges: Boolean;
    function GetCurrentTypeNameDictionary: TBoldTypeNameDictionary;
    function GetCurrentModelIsBoldified: Boolean;
    procedure SetModelHandle(const Value: TBoldModel);
    procedure SetInitialValues;
    procedure SetIgnoreModelChanges(const Value: Boolean);
  protected
    function GetCurrentModel: TUMLModel;
    function GetCurrentModelHandle: TBoldModel;
    function GetCurrentElement: TUMLModelElement;
    procedure EnsureUnFlattenedAndUnBoldified;
    procedure EnsureFlattenedAndBoldified;
    property IgnoreModelChanges: Boolean read FIgnoreModelChanges write SetIgnoreModelChanges;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AfterConstruction; override;
    procedure EditOclExpression(Element: TUMLModelElement; TaggedValue: String; Context: TUMLModelElement);
    property ModelHandle: TBoldModel read FModelHandle write SetModelHandle;
    property CurrentModel: TUMLModel read GetCurrentModel;
    property CurrentTypeNameDictionary: TBoldTypeNameDictionary read GetCurrentTypeNameDictionary;
    property CurrentModelIsBoldified: Boolean read GetCurrentModelIsBoldified;
  end;

var
  dmBoldUMLModelEditorHandles: TdmBoldUMLModelEditorHandles;

implementation

uses
  System.UITypes,
  BoldSystem, BoldDefs, BoldPMapperLists, BoldAttributes,
  BoldUMLModelDataModule, BoldDefaultTaggedValues, BoldUtils,
  BoldTaggedValueSupport, Variants, BoldUMLModelSupport, BoldUMLOCLEditor,
  Winapi.Windows, BoldAbstractModel, BoldSystemHandle, RTLConsts;

{$R *.dfm}

procedure TdmBoldUMLModelEditorHandles.AfterConstruction;
begin
  inherited;
  Assert((dmBoldUMLModelEditorHandles = nil) or (dmBoldUMLModelEditorHandles = self));
end;

function TdmBoldUMLModelEditorHandles.bcrAutoCreatedGetAsCheckBoxState(
  aFollower: TBoldFollower): TCheckBoxState;
begin
  if Assigned(aFollower.Element) and TBoldUMLBoldify.IsAutocreated(aFollower.Element as TUMLModelElement) then
    Result := cbChecked
  else
    Result := cbUnchecked
end;

procedure TdmBoldUMLModelEditorHandles.bcrAutoCreatedSetAsCheckBoxState(
  aFollower: TBoldFollower; newValue: TCheckBoxState);
begin
  if assigned(aFollower.Element) and (newValue= cbUnchecked) then
    TBoldUMLBoldify.RemoveBoldifyTaggedValue(aFollower.Element as TUMLModelElement, TAG_AUTOCREATED);
end;

function TdmBoldUMLModelEditorHandles.bcrBooleanToCheckBoxGetAsCheckBoxState(
  aFollower: TBoldFollower): TCheckBoxState;
var
  anElement: TBoldElement;
begin
  Result := cbGrayed;
  if Assigned(aFollower.Element) then
  begin
    anElement := aFollower.Value;
    if Assigned(anElement) then
    begin
      if TVIsTrue(anElement.AsString)then
        Result := cbChecked
      else
        Result := cbUnchecked;
    end;
  end;
end;

function TdmBoldUMLModelEditorHandles.bcrBooleanToCheckBoxMayModify(
  aFollower: TBoldFollower): Boolean;
var
  ValueElement: TBoldElement;
begin
  ValueElement := aFollower.Value;
  if Assigned(ValueElement) then
    Result := ValueElement.ObserverMayModify(aFollower.subscriber)
  else
    Result := false;
end;

procedure TdmBoldUMLModelEditorHandles.bcrBooleanToCheckBoxSetAsCheckBoxState(
  aFollower: TBoldFollower; newValue: TCheckBoxState);
begin
  aFollower.Value.AsString := BooleanToString(NewValue = cbChecked);
end;

procedure TdmBoldUMLModelEditorHandles.bcrBooleanToCheckBoxSubscribe(
  aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
begin
  aFollower.Element.SubscribeToExpression(aFollower.Controller.Expression, Subscriber, False);
end;

procedure TdmBoldUMLModelEditorHandles.bdhAttributePMapperNamesDeriveAndSubscribe(
  Sender: TComponent; RootValue: TBoldElement;
  ResultElement: TBoldIndirectElement; Subscriber: TBoldSubscriber);
var
  BoldName, strStreamName: String;
  Index: Integer;
  Mapping : TBoldTypeNameMapping;
begin
  if not Assigned(RootValue) then
  begin
    resultElement.SetOwnedValue(nil);
    Exit;
  end;
  ResultElement.SetOwnedValue(TBoldMemberList.CreateWithTypeInfo(CurrentModel.BoldSystem.BoldSystemTypeInfo.ListTypeInfoByElement[CurrentModel.BoldSystem.BoldSystemTypeInfo.AttributeTypeInfoByExpressionName['String']]));
  BoldName := (Rootvalue as TUMLAttribute).typeName;
  Mapping := CurrentTypeNameDictionary.MappingForModelName[BoldName];
  if assigned(Mapping) then
    strStreamName := Mapping.ExpandedContentsName;

  TBoldMemberList(ResultElement.Value).AddNew.AsString := DEFAULTNAME;


  for Index := 0 to BoldMemberPersistenceMappers.Count - 1 do
    if BoldMemberPersistenceMappers.Descriptors[Index].CanStore(strStreamName) then
      TBoldMemberList(ResultElement.Value).AddNew.AsString := BoldMemberPersistenceMappers.Descriptors[Index].Name;

  if Assigned(subscriber) then
    RootValue.SubscribeToExpression('typeName', Subscriber);
end;

procedure TdmBoldUMLModelEditorHandles.bdhSuperClassesDeriveAndSubscribe(
  Sender: TComponent; RootValue: TBoldElement;
  ResultElement: TBoldIndirectElement; Subscriber: TBoldSubscriber);
var
  List: TBoldObjectList;
  Bo: TUMLClass;
begin
  List := TUMLClassList.CreateWithTypeInfo(CurrentModel.BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByClass[TUMLClass]);
  Bo := RootValue as TUMLClass;
  while Assigned(BO) do
  begin
    List.Add(BO);
    if Assigned(Subscriber) then
      BO.SubscribeToExpression('', Subscriber);
    BO := bo.superclass as TUMLClass
  end;
  ResultElement.SetOwnedValue(List);
  if Assigned(subscriber) then
  begin
    RootValue.SubscribeToExpression('', Subscriber);
    ModelHandle.AddSubscription(Subscriber, beModelChanged);
  end;
end;

procedure TdmBoldUMLModelEditorHandles.bdhTypesForAttributeDeriveAndSubscribe(
  Sender: TComponent; RootValue: TBoldElement;
  ResultElement: TBoldIndirectElement; Subscriber: TBoldSubscriber);
begin
  if not Assigned(ModelHandle) then
  begin
    ResultElement.SetReferenceValue(nil);
    exit;
  end;
  ResultElement.SetReferenceValue(ModelHandle.DataTypes);
  if Assigned(subscriber) then
  begin
    ModelHandle.AddSubscription(Subscriber, beModelChanged);
    ModelHandle.DataTypes.SubscribeToExpression('', Subscriber);
    (ResultElement.Value as TBoldList).DefaultSubscribe(Subscriber);
    ResultElement.Value.AddSmallSubscription(Subscriber, [beDestroying]);
    ModelHandle.DataTypes.AddSmallSubscription(Subscriber,  [beDestroying]);
    ModelHandle.DataTypes.DefaultSubscribe(Subscriber{, breReSubscribe});
  end;
end;

procedure TdmBoldUMLModelEditorHandles.BoldAsVariantRenderer1SetColor(
  aFollower: TBoldFollower; var AColor: TColor);
begin
//  AColor := (AFollower.Value as TBoldElement).IsPartOfSystem;
end;

procedure TdmBoldUMLModelEditorHandles.BoldAsVariantRenderer1SetFont(
  aFollower: TBoldFollower; AFont: TFont);
begin
  if (AFollower.Value as TBoldElement).IsPartOfSystem then
    AFont.Style := AFont.Style + [fsBold]
  else
    AFont.Style := AFont.Style - [fsBold];
end;

function TdmBoldUMLModelEditorHandles.bsrNiceCRRendererGetAsString(
  aFollower: TBoldFollower): string;
begin
  if Assigned(aFollower.Value) then
  begin
    Result := aFollower.Value.AsString;
    while Pos(BOLDCRLF, Result) > 0 do
      Delete(Result, Pos(BOLDCRLF, Result), 2);
  end
  else
    Result := '';
end;

procedure TdmBoldUMLModelEditorHandles.bsrNiceCRRendererSubscribe(
  aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
begin
  aFollower.Element.SubscribeToExpression(aFollower.Controller.Expression, Subscriber, False);
end;

constructor TdmBoldUMLModelEditorHandles.Create(AOwner: TComponent);
begin
  inherited;
  if (csDesigning in ComponentState) then
    InitInheritedComponent(Self, TDataModule);
end;

procedure TdmBoldUMLModelEditorHandles.DataModuleCreate(Sender: TObject);
begin
  SetInitialValues;
end;

procedure TdmBoldUMLModelEditorHandles.EditOclExpression(
  Element: TUMLModelElement; TaggedValue: String; Context: TUMLModelElement);
var
  res: String;
  Tag: TUMLTaggedValue;
  Value: string;
begin
  EnsureFlattenedAndBoldified;
  Tag := Element.taggedValue[BOLDTVPREFIX+TaggedValue];
  if Assigned(Tag) then
    Value := Tag.value
  else
  if Assigned(Element.FindBoldMemberByExpressionName(TaggedValue)) then
    Value := Element.BoldMemberByExpressionName[TaggedValue].AsString;
  try
    res := BoldUMLOclEditor_.EditOcl(
      ModelHandle,
      Context,
      Value
      );
  finally
    if Assigned(Tag) then
      Element.SetBoldTV(TaggedValue, res)
    else
      Element.BoldMemberByExpressionName[TaggedValue].AsString := res;
  end;
end;

procedure TdmBoldUMLModelEditorHandles.EnsureFlattenedAndBoldified;
var
  Boldifier: TBoldUMLBoldify;
begin
  if CurrentModel = nil then
    Exit;
  Boldifier := ModelHandle.Boldify;

  if not TBoldUMLSupport.IsFlattened(CurrentModel) or not Boldifier.IsBoldified(CurrentModel) then
  begin
    IgnoreModelChanges := True;
    try
      CurrentModel.BoldSystem.StartTransaction;
      try
        if not TBoldUMLSupport.IsFlattened(CurrentModel) then
          TBoldUMLSupport.Flatten(CurrentModel);

        if not Boldifier.IsBoldified(CurrentModel) then
          Boldifier.Boldify(CurrentModel);
         CurrentModel.BoldSystem.CommitTransaction;
      except
        CurrentModel.BoldSystem.RollbackTransaction;
        raise;
      end;
    finally
      IgnoreModelChanges := False;
    end;
  end;
end;

procedure TdmBoldUMLModelEditorHandles.EnsureUnFlattenedAndUnBoldified;
var
  Boldifier: TBoldUMLBoldify;
begin
  if CurrentModel = nil then
    Exit;
  Boldifier := ModelHandle.Boldify;

  IgnoreModelChanges := True;
  try
    CurrentModel.BoldSystem.StartTransaction;
    try
      if TBoldUMLSupport.IsFlattened(CurrentModel) then
        TBoldUMLSupport.UnFlatten(CurrentModel);

      if  Boldifier.IsBoldified(CurrentModel) then
        Boldifier.UnBoldify(CurrentModel);
       CurrentModel.BoldSystem.CommitTransaction;
    except
      CurrentModel.BoldSystem.RollbackTransaction;
      raise;
    end;
  finally
    IgnoreModelChanges := False;
  end;
end;

function TdmBoldUMLModelEditorHandles.GetCurrentElement: TUMLModelElement;
begin
  result := brhCurrentElement.Value as TUMLModelElement;
end;

function TdmBoldUMLModelEditorHandles.GetCurrentModel: TUMLModel;
begin
  if assigned(brhRoot.Value) then
    result := brhRoot.Value as TUMLModel
  else
    result := nil;
end;

function TdmBoldUMLModelEditorHandles.GetCurrentModelHandle: TBoldModel;
begin
  Result := fModelHandle;
end;

function TdmBoldUMLModelEditorHandles.GetCurrentModelIsBoldified: Boolean;
begin
  Result := (behBoldified.Value as TBABoolean).AsBoolean = true;
end;

function TdmBoldUMLModelEditorHandles.GetCurrentTypeNameDictionary: TBoldTypeNameDictionary;
begin
  if assigned(ModelHandle) then
    result := ModelHandle.TypeNameDictionary
  else
    result := nil;
end;

procedure TdmBoldUMLModelEditorHandles.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = fModelHandle) and (Operation = opRemove) then
    fModelHandle := nil;
end;

procedure TdmBoldUMLModelEditorHandles.SetModelHandle(const Value: TBoldModel);
var
  index: integer;
begin
  if FModelHandle = Value then
    exit;
  if Assigned(FModelHandle) then
    FModelHandle.RemoveFreeNotification(self);
  FModelHandle := Value;
  if Assigned(FModelHandle) then
    FModelHandle.FreeNotification(self);

  for index := 0 to ComponentCount-1 do
    if Components[index] is TBoldNonSystemHandle then
      (Components[index] as TBoldNonSystemHandle).StaticSystemHandle := Value.SystemHandle;

  for index := 0 to ComponentCount-1 do
    if Components[index] is TBoldRootedHandle then
      if (Components[index] as TBoldRootedHandle).RootHandle = nil then
// (Components[index] as TBoldRootedHandle).RootHandle := behModel;
        raise Exception.CreateFmt('Error Message' , [TBoldRootedHandle(Components[index]).Name]);
end;

procedure TdmBoldUMLModelEditorHandles.SetIgnoreModelChanges(
  const Value: Boolean);
begin
  if value <> fIgnoreModelChanges then
  begin
    FIgnoreModelChanges := Value;
{    if Value then
      fModelChangedSubscriber.CancelAllSubscriptions
    else
      SubscribeToModelChanges;}
  end;
end;

procedure TdmBoldUMLModelEditorHandles.SetInitialValues;
begin
  bvhTableMapping.InitialValues.Clear;
  TBoldTaggedValueSupport.AddTableMappings(bvhTableMapping.InitialValues);

  bvhAttributeKind.InitialValues.Clear;
  TBoldTaggedValueSupport.AddAttributeKinds(bvhAttributeKind.InitialValues);

  bvhDelphiProperty.InitialValues.Clear;
  TBoldTaggedValueSupport.AddPropertyAccessKinds(bvhDelphiProperty.InitialValues);

  bvhDelphiFunctionType.InitialValues.Clear;
  TBoldTaggedValueSupport.AddDelphiFunctionTypes(bvhDelphiFunctionType.InitialValues);

  bvhDeleteActions.InitialValues.Clear;
  TBoldTaggedValueSupport.AddDeleteActions(bvhDeleteActions.InitialValues);

end;

function TdmBoldUMLModelEditorHandles.rDataTypeAttributeTypeGetAsVariant(
  AFollower: TBoldFollower): Variant;
var
  TypeNameMapping: TBoldTypeNameMapping;
  UMLDataType: TUMLDataType;
begin
  Result := Null;
  if Assigned(AFollower.Element) then
  begin
    UMLDataType := AFollower.Element as TUMLDataType;
    TypeNameMapping := ModelHandle.TypeNameDictionary.ExactMappingForModelName[UMLDataType.name];
    if Assigned(TypeNameMapping) then
      Result := TypeNameMapping.ExpandedDelphiName
  end;
end;

function TdmBoldUMLModelEditorHandles.rDataTypePMapperGetAsVariant(
  AFollower: TBoldFollower): Variant;
var
  TypeNameMapping: TBoldTypeNameMapping;
  UMLDataType: TUMLDataType;
begin
  Result := Null;
  if Assigned(AFollower.Element) then
  begin
    UMLDataType := AFollower.Element as TUMLDataType;
    TypeNameMapping := ModelHandle.TypeNameDictionary.ExactMappingForModelName[UMLDataType.name];
    if Assigned(TypeNameMapping) then
      Result := TypeNameMapping.ExpandedMapperName;
  end;
end;

end.
