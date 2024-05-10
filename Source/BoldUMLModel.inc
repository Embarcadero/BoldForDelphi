    {**********************************************************}
    {                                                          }
    {       Bold for Delphi Stub File                          }
    {                                                          }
    {       Autogenerated file for method implementations      }
    {                                                          }
    {**********************************************************}

{$INCLUDE BoldUMLModel_Interface.inc}

procedure TUMLModel.Clear;
begin
  BoldSystem.StartTransaction();
  try
    Validator.Violation.DeleteObjects;
    M_ownedElement.DeleteObjects;
    M_taggedValue.DeleteObjects;
    BoldSystem.CommitTransaction();
  except
    BoldSystem.RollbackTransaction();
    raise;
  end;
end;

function TUMLAssociationEnd.GetOtherEnd: TUMLAssociationEnd;
begin
  if Association.Connection[0] <> Self then
    Result := Association.Connection[0]
  else
    Result := Association.Connection[1];
end;

function TUMLAttribute.IsQualifier: Boolean;
begin
  Result := not Assigned(Owner);
end;

function TUMLAssociationEnd.GetQualifierByName(InName: String): TUMLAttribute;
var Index: Integer;
begin
  Result := nil;
  for Index := 0 to Qualifier.Count - 1 do
  begin
    if Qualifier[Index].Name = InName then
    begin
      Result := Qualifier[Index];
      Exit;
    end;
  end;
end;

function TUMLFeature.ExpandedExpressionName: String;
begin
  Result := inherited ExpandedExpressionName;
  if Length(Result) > 0 then
    Result[1] := LowerCase(Result[1])[1];
end;

function TUMLClass.ExpandedExpressionName: String;
begin
  Result := inherited ExpandedExpressionName;
  if Length(Result) > 0 then
    Result[1] := UpperCase(Result[1])[1];
end;

function TUMLModelElement.ExpandedExpressionName: String;
var
  ExpressionName, NameInModel: String;
begin
  ExpressionName := GetBoldTV(TAG_EXPRESSIONNAME);
  NameInModel := Name;
  Result := BoldExpandName(ExpressionName, NameInModel, xtExpression, -1, TBoldTaggedValueSupport.StringToNationalCharConversion(model.GetBoldTV(TAG_NATIONALCHARCONVERSION)));
end;

procedure TUMLAssociationEnd._Multi_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
begin
  M_Multi.AsBoolean := (GetUpperLimitForMultiplicity(Multiplicity) > 1) or (qualifier.Count > 0);
  if Assigned(Subscriber) then
  begin
    m_Multiplicity.DefaultSubscribe(Subscriber, breReEvaluate);
    M_qualifier.DefaultSubscribe(Subscriber, breReEvaluate);
  end;
end;

procedure TUMLAssociationEnd._Mandatory_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
begin
  M_Mandatory.AsBoolean := GetLowerLimitForMultiplicity(Multiplicity) > 0;

  if Assigned(Subscriber) then
    m_Multiplicity.DefaultSubscribe(Subscriber, breReEvaluate);
end;

function TUMLModelElement.FindTaggedValue(TagName: String): TUMLTaggedValue;
begin
  Result := TaggedValue[TagName];
end;

function TUMLModelElement.GetTaggedValue(TagName: String): String;
var
  aTaggedValue: TUMLTaggedValue;
begin
  aTaggedValue := FindTaggedValue( TagName );
  if Assigned(aTaggedValue) then
    Result := aTaggedValue.Value
  else
    Result := '';
end;

procedure TUMLModelElement.SetTaggedValue(TagName: String; Value: String);
var
  aTaggedValue: TUMLTaggedValue;
begin
  aTaggedValue := TaggedValue[TagName];
  if Assigned(aTaggedValue) then
    aTaggedValue.Value := Value
  else
  begin
    aTaggedValue := M_TaggedValue.AddNew;
    aTaggedValue.Tag := TagName;
    aTaggedValue.Value := Value;
  end;
end;

procedure TUMLModelElement.DeleteTaggedValue(TagName: String);
var
  aTaggedValue: TUMLTaggedValue;
begin
  aTaggedValue := TaggedValue[TagName];
  if Assigned(aTaggedValue) then
  begin
    aTaggedValue.UnlinkAll;
    aTaggedValue.Delete;
  end;
end;

procedure TUMLModelElement.EnsureTaggedValue(TagName: String; DefaultValue: String);
var
  aTaggedValue: TUMLTaggedValue;
begin
  aTaggedValue := TaggedValue[TagName];
  if not Assigned(aTaggedValue) then
  begin
//    aTaggedValue := TUMLTaggedValue.Create((Self as TUMLModelElement).BoldSystem);
    aTaggedValue := M_TaggedValue.AddNew;
    aTaggedValue.Tag := TagName;
    aTaggedValue.Value := DefaultValue;
  end;
end;

procedure TUMLModel.CompleteCreate;
begin
  inherited CompleteCreate;
  Validator := TValidator.Create(BoldSystem);
end;

function TUMLAttribute.EffectivePersistent: Boolean;
begin
  Result := Persistent and Owner.Persistent;
end;

function TUMLAssociation.EffectivePersistent: Boolean;
begin
  Result := Persistent and
            assigned(Connection[0].Type_) and Connection[0].Type_.Persistent and
            assigned(Connection[1].Type_) and Connection[1].Type_.Persistent;
end;

function TUMLModelElement.GetBoldTV(const TagName: String): String;
var
  aTaggedValue: TUMLTaggedValue;
begin
  aTaggedValue := FindTaggedValue(BOLDTVPREFIX + TagName );
  if Assigned(aTaggedValue) then
    Result := aTaggedValue.Value
  else
    Result := '';
end;

procedure TUMLModelElement.SetBoldTV(const TagName: String; const Value: String);
var
  aTaggedValue: TUMLTaggedValue;
begin
  aTaggedValue := FindTaggedValue(BOLDTVPREFIX + TagName );
  if Assigned(aTaggedValue) then
    aTaggedValue.Value := Value
  else
  begin
//    aTaggedValue := TUMLTaggedValue.Create((Self as TUMLModelElement).BoldSystem);
    aTaggedValue := M_TaggedValue.AddNew;
    aTaggedValue.Tag := BOLDTVPREFIX + TagName;
    aTaggedValue.Value := Value;
  end;
end;

function TUMLAssociationEnd.ExpandedExpressionName: String;
begin
  Result := inherited ExpandedExpressionName;
  if Length(Result) > 0 then
    Result[1] := LowerCase(Result[1])[1];
end;

procedure TUMLClassifier.GetAllOverrideableMethods(Methods: TList);

  procedure GetSuperclassMethods(aClass: TUMLClassifier; SuperMethods: TList);
  var
    Index: Integer;
    UMLOperation: TUMLOperation;
  begin
    if Assigned(aClass.SuperClass) then
      GetSuperclassMethods(aClass.SuperClass, SuperMethods);
    for Index := 0 to aClass.Feature.Count - 1 do
    begin
      if aClass.Feature[Index] is TUMLOperation then
      begin
        UMLOperation := (aClass.Feature[Index] as TUMLOperation);
        if (UMLOperation.GetBoldTV(TAG_DELPHIOPERATIONKIND) = TV_DELPHIOPERATIONKIND_VIRTUAL) or
           (UMLOperation.GetBoldTV(TAG_DELPHIOPERATIONKIND) = TV_DELPHIOPERATIONKIND_DYNAMIC) or
           (UMLOperation.GetBoldTV(TAG_DELPHIOPERATIONKIND) = TV_DELPHIOPERATIONKIND_ABSTRACTVIRTUAL) then

          Methods.Add(aClass.Feature[Index]);
      end;
    end;
  end;

begin
  GetSuperclassMethods(Self, Methods);
end;

procedure TUMLClassifier._Persistent_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
var
  value: string;
begin
  // default value when this tagged value is ensured is to make it persistent,
  // so empty values must be treated as persistent too.
  value := GetTaggedValue(TAG_PERSISTENCE);
  M_Persistent.AsBoolean := (value = '') or SameText(value, TV_PERSISTENCE_PERSISTENT);
  SubscribeToExpression('taggedValue[''' + TAG_PERSISTENCE + '''].value', Subscriber);
end;

procedure TUMLClassifier._Persistent_ReverseDerive(DerivedObject: TObject);
var
  NewValue: String;
begin
  if Persistent then
    NewValue := TV_PERSISTENCE_PERSISTENT
  else
    NewValue := TV_PERSISTENCE_TRANSIENT;
  SetTaggedValue(TAG_PERSISTENCE, NewValue);
end;

procedure TUMLAssociation._Persistent_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
begin
  M_Persistent.AsBoolean := SameText(GetTaggedValue(TAG_PERSISTENCE), TV_PERSISTENCE_PERSISTENT);
  SubscribeToExpression('taggedValue[''' + TAG_PERSISTENCE + '''].value', Subscriber);
end;

procedure TUMLAssociation._Persistent_ReverseDerive(DerivedObject: TObject);
var
  NewValue: String;
begin
  if Persistent then
    NewValue := TV_PERSISTENCE_PERSISTENT
  else
    NewValue := TV_PERSISTENCE_TRANSIENT;
  SetTaggedValue(TAG_PERSISTENCE, NewValue);
end;

procedure TUMLAttribute._Persistent_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
begin
  M_Persistent.AsBoolean := SameText(GetTaggedValue(TAG_PERSISTENCE), TV_PERSISTENCE_PERSISTENT);
  SubscribeToExpression('taggedValue[''' + TAG_PERSISTENCE + '''].value', Subscriber);
end;

procedure TUMLAttribute._Persistent_ReverseDerive(DerivedObject: TObject);
var
  NewValue: String;
begin
  if Persistent then
    NewValue := TV_PERSISTENCE_PERSISTENT
  else
    NewValue := TV_PERSISTENCE_TRANSIENT;
  SetTaggedValue(TAG_PERSISTENCE, NewValue);
end;

procedure TUMLModelElement._StereotypeName_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
begin
  if Assigned(stereotype) then
    M_StereotypeName.AsString := stereotype.Name
  else
    M_StereotypeName.AsString := '';
  SubscribeToExpression('stereotype.Name', Subscriber);
end;

function GetElementByName(UMLModel: TUMLModel; Name: String; UMLClass: TBoldObjectClass): TUMLModelElement;
var
  ElementList: TUMLModelElementList;
  i: integer;
begin
  result := nil;
  ElementList := UMLModel.BoldSystem.ClassByObjectClass[UMLClass] as TUMLModelElementList;
  for i := 0 to ElementList.Count-1 do
  begin
    if (SameText(ElementList[i].Name, name)) and (ElementList[i].Model = UMLModel) then
    begin
      result := ElementList[i];
      exit;
    end;
  end;
end;

procedure TUMLModelElement._Stereotypename_ReverseDerive(DerivedObject: TObject);
var
  aStereotype: TUMLStereotype;
  NewName: string;
begin
  NewName := (DerivedObject as TBAString).AsString;
  if NewName = '' then
    stereotype := nil
  else
  begin
    if not assigned(model) then
      raise EBold.Create('Can''t set StereotypeName in for Element not belonging to model');
    aStereoType := GetElementByName(Model, Newname, TUMLStereotype) as TUMLStereoType;
//    model.EvaluateExpressionAsDirectElement
//    (
//      Format('allOwnedElement->filterOnType(UMLStereotype)->select(name=''%s'')->first', [NewName])
//    ) as TUMLStereotype;
    if not Assigned(aStereotype) then
    begin
      aStereotype := TUMLStereotype.Create(BoldSystem);
      aStereotype.namespace_ := model;
      aStereotype.Name := newName;
    end;
    stereotype := aStereotype;
  end;
end;

class function TUMLStereotype.FindStereotypeByName(aName: String; aSystem: TBoldSystem): TUMLStereotype;
var
  AllStereotypes: TUMLStereotypeList;
  i: Integer;
begin
  Result := nil;
  AllStereotypes := aSystem.ClassByObjectClass[TUMLStereotype] as TUMLStereotypeList;
  for i := 0 to AllStereotypes.Count - 1 do
  begin
    if AllStereotypes[i].Name = aName then
    begin
      Result := AllStereotypes[i];
      Exit;
    end;
  end;
end;


procedure TUMLModelElement._documentation_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
begin
  // FIXME implement

end;

procedure TUMLClassifier.SetFirstParent(parent: TUMLClassifier);
var
  NewGeneralization: TUMLGeneralization;
begin
  if generalization.Count = 0 then
  begin
    NewGeneralization := TUMLGeneralization.Create(BoldSystem);
    NewGeneralization.namespace_ := Self;
    NewGeneralization.Child := Self;
  end;
  if parent = nil then
    Generalization[0].Delete
  else
    generalization[0].parent := parent;
end;


procedure TUMLParameter._typeName_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
begin
  if Assigned(type_) then
    M_typeName.AsString := type_.name
  else
    M_typeName.AsString := '';
  SubscribeToExpression('type.name', Subscriber);
end;

procedure TUMLParameter._typeName_ReverseDerive(DerivedObject: TObject);
var
  aType: TUMLClassifier;
  NewName: string;
begin
  NewName := (DerivedObject as TBAString).AsString;
  if NewName = '' then
    type_ := nil
  else
  begin
    if not assigned(model) then
      raise EBold.Create('Can''t set typeName for Parameter not belonging to model');
    aType := GetElementByName(Model, Newname, TUMLDataType) as TUMLClassifier;
    if not Assigned(aType) then
      aType := GetElementByName(Model, Newname, TUMLGeneralizableElement) as TUMLClassifier;
    if not Assigned(aType) then
    begin
      aType := TUMLDataType.Create(BoldSystem);
      aType.namespace_ := model;
      aType.Name := newName;
    end;
    type_ := aType;
  end;
end;

procedure TUMLStructuralFeature._typeName_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
begin
  if Assigned(type_) then
    M_typeName.AsString := type_.name
  else
    M_typeName.AsString := '';
  SubscribeToExpression('type.name', Subscriber);
end;

procedure TUMLStructuralFeature._typeName_ReverseDerive(DerivedObject: TObject);
var
//  aType: TUMLGeneralizableElement;
  aType: TUMLClassifier;
  NewName: string;
begin
  NewName := (DerivedObject as TBAString).AsString;
  if NewName = '' then
    type_ := nil
  else
  begin
    if not assigned(model) then
      raise EBold.Create('Can''t set typeName for StructuralFeature not belonging to model');
    aType := GetElementByName(Model, Newname, TUMLDataType) as TUMLClassifier;
    if not Assigned(aType) then
    begin
      aType := TUMLDataType.Create(BoldSystem);
      aType.namespace_ := model;
      aType.Name := newName;
    end;
    type_ := aType;
  end;
end;

procedure TUMLModelElement._derived_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
begin
  M_Derived.AsBoolean := TVIsTrue(GetTaggedValue(TAG_DERIVED));
  SubscribeToExpression('taggedValue[''' + TAG_DERIVED + '''].value', Subscriber);
end;

procedure TUMLModelElement._derived_ReverseDerive(DerivedObject: TObject);
var
  NewValue: String;
begin
  if Derived then
    NewValue := TV_TRUE
  else
    NewValue := TV_FALSE;
  SetTaggedValue(TAG_DERIVED, NewValue);
end;

procedure TUMLAssociationEnd._isOrdered_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
begin
  M_isOrdered.AsBoolean := Ordering = okOrdered;
  M_ordering.AddSmallSubscription(Subscriber, beValueEvents, breReEvaluate);
end;

procedure TUMLAssociationEnd._isOrdered_ReverseDerive(DerivedObject: TObject);
begin
  if IsOrdered then
    Ordering := okOrdered
  else
    Ordering := okUnordered;
end;

procedure TUMLClassifier._superclasses_DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber);
var
  vClass: TUMLClassifier;
begin
  vClass := self;
  repeat
    vClass := vClass.SuperClass;
    if Assigned(vClass) then
      M_superclasses.Insert(0, vClass);
  until vClass = nil ;
end;
