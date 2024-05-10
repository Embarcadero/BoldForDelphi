
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelConverter;

interface

uses
  BoldUMLModel,
  BoldMeta,
  BoldSystem,
  BoldContainers;

type
  TBoldModelConverter = class
  private
    class procedure UMLElementToMoldElement(UMLElement: TUMLModelElement; MoldElement: TMoldElement);
    class procedure UMLClassToMoldClass(UMLClass: TUMLClass; MoldClass: TMoldClass);
    class procedure UMLAssociationToMoldAssociation(UMLAssociation: TUMLAssociation; MoldAssociation: TMoldAssociation);
    class procedure UMLAttributeToMoldAttribute(UMLAttribute: TUMLAttribute; MoldAttribute: TMoldAttribute);
    class procedure UMLOperationToMoldMethod(UMLOperation: TUMLOperation; MoldMethod: TMoldMethod);
    class procedure UMLAssociationEndToMoldRole(UMLAssociationEnd: TUMLAssociationEnd; MoldRole: TMoldRole);
    class procedure UMLAttributeToMoldQualifier(UMLAttribute: TUMLAttribute; MoldQualifier: TMoldQualifier);
    class procedure UMLParametersToMoldParameters(UMLParameters: TBoldObjectList; MoldMethod: TMoldMethod);
    class function UMLModelGetUniqueRootClass(UMLModel: TUMLModel): TUMLClass;

    class procedure MoldElementToUMLElement(MoldElement: TMoldElement; UMLElement: TUMLModelElement);
    class procedure MoldClassToUMLClass(MoldClass: TMoldClass; UMLClass: TUMLClass);
    class procedure MoldAssociationToUMLAssociation(MoldAssociation: TMoldAssociation; UMLAssociation: TUMLAssociation);
    class procedure MoldAttributeToUMLAttribute(MoldAttribute: TMoldAttribute; UMLAttribute: TUMLAttribute);
    class procedure MoldMethodToUMLOperation(MoldMethod: TMoldMethod; UMLOperation: TUMLOperation);
    class procedure MoldRoleToUMLAssociationEnd(MoldRole: TMoldRole; UMLAssociationEnd: TUMLAssociationEnd);
    class procedure MoldQualifierToUMLAttribute(MoldQualifier: TMoldQualifier; UMLAttribute: TUMLAttribute);
    class procedure MoldParametersToUMLParameters(MoldParameters: TBoldObjectArray; UMLParameters: TBoldObjectList; BoldSystem: TBoldSystem);
    class function GetUMLClassByName(const name: string; UMLModel: TUMLModel): TUMLClass;
    class function GetUMLAssociationByName(const name: string; UMLModel: TUMLModel): TUMLAssociation;
    class procedure MoldModelToUMLModel(MoldModel: TMoldModel; UMLModel: TUMLModel);
  public
    class function UMLModelToMold(UMLModel: TUMLModel): TMoldModel; virtual;
    class procedure MoldModelToUML(MoldModel: TMoldModel; TargetUMLModel: TUMLModel);
  end;

implementation

uses
  Classes,
  SysUtils,
  BoldLogHandler,
  BoldQueue,
  BoldUtils,
  BoldGuard,
  BoldMetaSupport,
  BoldDefaultTaggedValues,
  BoldUMLModelSupport,
  BoldUMLTypes;

class function TBoldModelConverter.UMLModelToMold(UMLModel: TUMLModel): TMoldModel;
var
  MoldModel: TMoldModel;
  MoldClass: TMoldClass;
  MoldAssociation: TMoldAssociation;
  i: integer;
  UMLRootClass: TUMLClass;
begin
  MoldModel := TMoldModel.Create(nil, UMLModel.Name);
  UMLElementToMoldElement(UMLModel, MoldModel);
  BoldLog.StartLog('Converting UMLModel to Mold');
  BoldLog.ProgressMax := UMLModel.Classes.Count + UMLModel.Associations.Count;

  UMLRootClass := UMLModelGetUniqueRootClass(UMLModel);
  if UMLRootClass <> nil then
    MoldModel.RootClass.name := UMLRootClass.Name;
  for i := 0 to UMLModel.Classes.Count - 1 do
  begin
    MoldClass := MoldModel.GetClassByName((UMLModel.Classes[i]).Name);
    UMLClassToMoldClass(UMLModel.Classes[i], MoldClass);
    BoldLog.ProgressStep;
  end;
  for i := 0 to UMLModel.Associations.Count - 1 do
  begin
    if (UMLModel.associations[i].connection.Count = 2) and
      (UMLModel.associations[i].connection[0].type_ is TUMLClass) and
      (UMLModel.associations[i].connection[1].type_ is TUMLClass) then
    begin
      MoldAssociation := TMoldAssociation.Create(MoldModel, UMLModel.Associations[i].name);
      UMLAssociationToMoldAssociation(UMLModel.Associations[i],
                                       MoldAssociation);
    end;
    BoldLog.ProgressStep;
  end;
  result := MoldModel;
  BoldLog.EndLog;
end;

class procedure TBoldModelConverter.UMLElementToMoldElement(UMLElement: TUMLModelElement; MoldElement: TMoldElement);
var
  Index: Integer;
begin
  MoldElement.Stereotype := UMLElement.stereotypeName;
  for Index := 0 to UMLElement.constraint.Count - 1 do
    with UMLElement.constraint[Index] do
      MoldElement.Constraints.values[Name] := Body;
  for Index := 0 to UMLElement.M_TaggedValue.Count-1 do
    MoldElement.TVByName[UMLElement.M_TaggedValue[Index].Tag] := UMLElement.M_TaggedValue[Index].Value;
end;

class procedure TBoldModelConverter.UMLClassToMoldClass(UMLClass: TUMLClass; MoldClass: TMoldClass);
var
  i: Integer;
  MoldAttribute: TMoldAttribute;
  MoldMethod: TMoldMethod;
begin
  UMLElementToMoldElement(UMLClass, MoldClass);
  with MoldClass do
  begin
    IsAbstract := UMLClass.isAbstract;
    Persistent := UMLClass.Persistent;
    for i := 0 to UMLClass.Feature.Count-1 do
    begin
      if UMLClass.Feature[i] is TUMLAttribute then
      begin
        MoldAttribute := TMoldAttribute.Create(MoldClass, UMLClass.Feature[i].name);
        UMLAttributeToMoldAttribute(UMLClass.Feature[i] as TUMLAttribute, MoldAttribute);
      end
      else
      if UMLClass.Feature[i] is TUMLOperation then
      begin
        MoldMethod := TMoldMethod.Create(MoldClass, UMLClass.Feature[i].name);
        UMLOperationToMoldMethod(UMLClass.Feature[i] as TUMLOperation, MoldMethod);
      end;
    end;
    if assigned(UMLClass.Superclass) then
      SuperClass := Model.GetClassByName((UMLClass.Superclass).Name);
  end;
end;

class procedure TBoldModelConverter.UMLAssociationToMoldAssociation(UMLAssociation: TUMLAssociation; MoldAssociation: TMoldAssociation);
var
  i: Integer;
  MoldRole: TMoldRole;
begin
  UMLElementToMoldElement(UMLAssociation, MoldAssociation);
  with MoldAssociation do
  begin
    Derived := UMLAssociation.Derived;
    if assigned(UMLAssociation.Class_) then
      LinkClass := Model.GetClassByName((UMLAssociation.Class_ as TUMLClass).Name);
    for i := 0 to 1 do
    begin
      MoldRole := TMoldRole.Create(MoldAssociation, UMLAssociation.Connection[i].name);
      UMLAssociationEndToMoldRole(UMLAssociation.Connection[i], MoldRole);
    end;
  end;
end;

class procedure TBoldModelConverter.UMLAssociationEndToMoldRole(UMLAssociationEnd: TUMLAssociationEnd; MoldRole: TMoldRole);
var
  OtherAssociationEnd: TUMLAssociationEnd;
  OtherEndClass: TUMLClass;
  i: Integer;
  MoldQualifier: TMoldQualifier;
begin
  UMLElementToMoldElement(UMLAssociationEnd, MoldRole);
  with MoldRole do
  begin
    if UMLAssociationEnd.Association.Connection[0] = UMLAssociationEnd then
      OtherAssociationEnd := UMLAssociationEnd.Association.Connection[1] as TUMLAssociationEnd
    else
      OtherAssociationEnd := UMLAssociationEnd.Association.Connection[0] as TUMLAssociationEnd;
    OtherEndClass := OtherAssociationEnd.Type_ as TUMLClass;
    if Assigned(OtherEndClass) then
      MoldClass := Model.GetClassByName(OtherEndClass.Name)
    else
      MoldClass := nil;
    Multiplicity := UMLAssociationEnd.Multiplicity;
    Navigable := UMLAssociationEnd.isNavigable;
    Ordered := UMLAssociationEnd.isOrdered;
    Aggregation := UMLAssociationEnd.Aggregation;
    Visibility := UMLAssociationEnd.Visibility;
    Changeability := UMLAssociationEnd.Changeability;

  end;
  for i := 0 to {OtherAssociationEnd.}UMLAssociationEnd.Qualifier.Count - 1 do
  begin
    MoldQualifier := TMoldQualifier.Create(MoldRole, UMLAssociationEnd.Qualifier[i].name);
    UMLAttributeToMoldQualifier({OtherAssociationEnd.}UMLAssociationEnd.Qualifier[i], MoldQualifier);
  end;
end;

class procedure TBoldModelConverter.UMLAttributeToMoldQualifier(UMLAttribute: TUMLAttribute; MoldQualifier: TMoldQualifier);
begin
  UMLElementToMoldElement(UMLAttribute, MoldQualifier);
  with MoldQualifier do
  begin
    BoldType := UMLAttribute.typeName;
  end;
end;

class procedure TBoldModelConverter.UMLAttributeToMoldAttribute(UMLAttribute: TUMLAttribute; MoldAttribute: TMoldAttribute);
begin
  UMLElementToMoldElement(UMLAttribute, MoldAttribute);
  with MoldAttribute do
  begin
    BoldType := UMLAttribute.typeName;
    Derived := UMLAttribute.Derived;
    Visibility := UMLAttribute.visibility;
    InitialValue := UMLAttribute.InitialValue;
  end;
end;

class procedure TBoldModelConverter.UMLOperationToMoldMethod(UMLOperation: TUMLOperation; MoldMethod: TMoldMethod);
var
  i: Integer;
begin
  UMLElementToMoldElement(UMLOperation, MoldMethod);
  with MoldMethod do
  begin
    IsClassMethod := (UMLOperation.ownerScope = skClassifier);
    ReturnType := '';
    for i := 0 to UMLOperation.Parameter.Count-1 do
      if (UMLOperation.Parameter[i] as TUMLParameter).kind = pdReturn then
      begin
        ReturnType := (UMLOperation.Parameter[i] as TUMLParameter).typeName;
        break;
      end;
    UMLParametersToMoldParameters(UMLOperation.Parameter, MoldMethod);
    Visibility := UMLOperation.Visibility;
  end;
end;

class procedure TBoldModelConverter.UMLParametersToMoldParameters(UMLParameters: TBoldObjectList; MoldMethod: TMoldMethod);
var Index: Integer;
    MoldParameter: TMoldParameter;
begin
  for Index := 0 to UMLParameters.Count - 1 do
  begin
    if (UMLParameters[Index] as TUMLParameter).Kind <> pdReturn then
    begin
      MoldParameter := TMoldParameter.Create(MoldMethod);
      MoldParameter.ParameterName := (UMLParameters[Index] as TUMLParameter).Name;
      MoldParameter.ParameterType := (UMLParameters[Index] as TUMLParameter).typeName;
      MoldParameter.ParameterKind := (UMLParameters[Index] as TUMLParameter).kind;
      MoldParameter.IsConst := StringToBoolean((UMLParameters[Index] as TUMLParameter).GetBoldTV(TAG_ISCONST));
      MoldMethod.Parameters.Add(MoldParameter);
    end;
  end;
end;

class function TBoldModelConverter.UMLModelGetUniqueRootClass(UMLModel: TUMLModel): TUMLClass;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to UMLModel.Classes.Count-1 do
  begin
    with UMLModel.Classes[i] as TUMLClass do
    begin
      if Superclass = nil then
      begin
        if result <> nil then
        begin
          result := nil;
          break;
        end
        else
        begin
          result := UMLModel.Classes[i] as TUMLClass;
        end;
      end;
    end;
  end;
end;

class procedure TBoldModelConverter.MoldModelToUML(MoldModel: TMoldModel; TargetUMLModel: TUMLModel);
begin
  TargetUMLModel.BoldSystem.StartTransaction;
  try
    TargetUMLModel.Clear;
    MoldModelToUMLModel(MoldModel, TargetUMLModel);
    TargetUMLModel.BoldSystem.CommitTransaction;
  except
    TargetUMLModel.BoldSystem.RollBackTransaction;
  end;
end;

class procedure TBoldModelConverter.MoldModelToUMLModel(MoldModel: TMoldModel; UMLModel: TUMLModel);
var
  i: Integer;
  UMLClass: TUMLClass;
  UMLAssociation: TUMLAssociation;
begin
  MoldElementToUMLElement(MoldModel, UMLModel);
  BoldLog.StartLog('Converting MoldModel to UML');
  BoldLog.ProgressMax := MoldModel.Classes.Count + MoldModel.Associations.Count;
  BoldInstalledQueue.DeactivateDisplayQueue;
  TBoldUMLBoldify.SetRootClassName(UMLModel, MoldModel.RootClass.Name);
  try
    for i := 0 to MoldModel.Classes.Count-1 do
    begin
      UMLClass := GetUMLClassByName(MoldModel.Classes[i].name, UMLModel);
      MoldClassToUMLClass(MoldModel.Classes[i], UMLClass);
      BoldLog.ProgressStep;
      BoldLog.Sync;
    end;

    for i := 0 to MoldModel.Associations.Count-1 do
    begin
      UMLAssociation := GetUMLAssociationByName(MoldModel.Associations[i].name, UMLModel);
      MoldAssociationToUMLAssociation(MoldModel.Associations[i], UMLAssociation);
      BoldLog.ProgressStep;
      BoldLog.Sync;
    end;
  finally
    BoldInstalledQueue.ActivateDisplayQueue;
  end;
  BoldLog.EndLog;
end;

class procedure TBoldModelConverter.MoldElementToUMLElement(MoldElement: TMoldElement; UMLElement: TUMLModelElement);
var
  I: Integer;
  G: IBoldGuard;
  Tags, Values: TStringList;
begin
  G := TBoldGuard.Create(Tags, Values);
  Tags := TStringList.Create;
  Values := TStringList.Create;
  with UMLElement do
  begin
    Name := MoldElement.name;
    StereotypeName := MoldElement.Stereotype;
    for i := 0 to MoldElement.Constraints.Count-1 do
      with Constraint.AddNew as TUMLConstraint do
      begin
        Name := MoldElement.Constraints.Names[i];
        Body := MoldElement.Constraints.Values[Name];
      end;
    MoldElement.AddAllTaggedValues(Tags, Values);
    for i := 0 to Tags.Count-1 do
    begin
      with M_TaggedValue.AddNew do
      begin
        tag := Tags[i];
        value := Values[i];
      end;
    end;
  end;
end;

class procedure TBoldModelConverter.MoldClassToUMLClass(MoldClass: TMoldClass; UMLClass: TUMLClass);
var
  i: Integer;
  UMLAttribute: TUMLAttribute;
  UMLOperation: TUMLOperation;
begin
  MoldElementToUMLElement(MoldClass, UMLClass);
  with UMLClass do
  begin
    isAbstract := MoldClass.IsAbstract;
    Persistent := MoldClass.Persistent;
    isRoot := False;
    isLeaf := False;
    for i := 0 to MoldClass.Attributes.Count-1 do
    begin
      UMLAttribute := TUMLAttribute.Create(UMLClass.BoldSystem);
      UMLClass.Feature.Add(UMLAttribute);
      MoldAttributeToUMLAttribute(MoldClass.Attributes[i], UMLAttribute);
    end;
    for i := 0 to MoldClass.Methods.Count-1 do
    begin
      UMLOperation := TUMLOperation.Create(UMLClass.BoldSystem);
      UMLClass.Feature.Add(UMLOperation);
      MoldMethodToUMLOperation(MoldClass.Methods[i], UMLOperation);
    end;
    if assigned(MoldClass.SuperClass) then
      SetFirstParent(GetUMLClassByName(MoldClass.Superclass.name, UMLClass.model));
  end;
end;

class procedure TBoldModelConverter.MoldAssociationToUMLAssociation(MoldAssociation: TMoldAssociation; UMLAssociation: TUMLAssociation);
var
  i: Integer;
  UMLAssociationEnd: TUMLAssociationEnd;
begin
  MoldElementToUMLElement(MoldAssociation, UMLAssociation);
  with UMLAssociation do
  begin
    Derived := MoldAssociation.Derived;
    if assigned(MoldAssociation.LinkClass) then
      Class_ := GetUMLClassByName(MoldAssociation.LinkClass.name, model);
    for i := 0 to 1 do
    begin
      UMLAssociationEnd := TUMLAssociationEnd.Create(model.BoldSystem);
      UMLAssociation.Connection.Add(UMLAssociationEnd);
      MoldRoleToUMLAssociationEnd(MoldAssociation.Roles[i], UMLAssociationEnd);
    end;
  end;
end;

class procedure TBoldModelConverter.MoldAttributeToUMLAttribute(MoldAttribute: TMoldAttribute; UMLAttribute: TUMLAttribute);
begin
  MoldElementToUMLElement(MoldAttribute, UMLAttribute);
  with UMLAttribute do
  begin
    typeName := MoldAttribute.BoldType;
    Derived := MoldAttribute.Derived;
    Visibility := MoldAttribute.Visibility;
    InitialValue := MoldAttribute.InitialValue;
  end;
end;

class procedure TBoldModelConverter.MoldMethodToUMLOperation(MoldMethod: TMoldMethod; UMLOperation: TUMLOperation);
var
  ReturnUMLParameter: TUMLParameter;
begin
  MoldElementToUMLElement(MoldMethod, UMLOperation);
  with UMLOperation do
  begin
    if MoldMethod.IsClassMethod then
      ownerScope := skClassifier
    else
      ownerScope := skInstance;
    Visibility := MoldMethod.Visibility;
    MoldParametersToUMLParameters(MoldMethod.Parameters, Parameter, UMLOperation.BoldSystem);
    if MoldMethod.ReturnType <> '' then
    begin
      ReturnUMLParameter := TUMLParameter.Create(UMLOperation.BoldSystem);
      Parameter.Add(ReturnUMLParameter);
      ReturnUMLParameter.Name := 'return';
      ReturnUMLParameter.kind := pdReturn;
      ReturnUMLParameter.typeName := MoldMethod.ReturnType;
      ReturnUMLParameter.SetBoldTV(TAG_DELPHINAME, TV_NAME);
      ReturnUMLParameter.SetBoldTV(TAG_EXPRESSIONNAME, TV_NAME);
    end;
  end;
end;

class procedure TBoldModelConverter.MoldRoleToUMLAssociationEnd(MoldRole: TMoldRole; UMLAssociationEnd: TUMLAssociationEnd);
var
  i: Integer;
  UMLAttribute: TUMLAttribute;
begin
  MoldElementToUMLElement(MoldRole, UMLAssociationEnd);
  with UMLAssociationEnd do
  begin
    if Assigned(MoldRole.OtherEnd.MoldClass) then
      Type_ := GetUMLClassByName(MoldRole.OtherEnd.MoldClass.name, Association.model)
    else
      Type_ := nil;
    isNavigable := MoldRole.Navigable;
    isOrdered := MoldRole.Ordered;
    Aggregation := MoldRole.Aggregation;
    Visibility := MoldRole.Visibility;
    Changeability := MoldRole.Changeability;
    Multiplicity := MoldRole.Multiplicity;
  end;
  for i := 0 to MoldRole.{OtherEnd.}Qualifiers.Count-1 do
  begin
    UMLAttribute := TUMLAttribute.Create(UMLAssociationEnd.BoldSystem);
    UMLAssociationEnd.Qualifier.Add(UMLAttribute);
    MoldQualifierToUMLAttribute(MoldRole.{OtherEnd.}Qualifiers[i], UMLAttribute);
  end;
end;

class procedure TBoldModelConverter.MoldQualifierToUMLAttribute(MoldQualifier: TMoldQualifier; UMLAttribute: TUMLAttribute);
begin
  MoldElementToUMLElement(MoldQualifier, UMLAttribute);
  with UMLAttribute do
  begin
    typeName := MoldQualifier.BoldType;
  end;
end;

class procedure TBoldModelConverter.MoldParametersToUMLParameters(MoldParameters: TBoldObjectArray; UMLParameters: TBoldObjectList; BoldSystem: TBoldSystem);
var Index: Integer;
    UMLParameter: TUMLParameter;
begin
  for Index := 0 to MoldParameters.Count - 1 do
  begin
    if (MoldParameters[Index] as TMoldParameter).ParameterKind <> pdReturn then
    begin
      UMLParameter := TUMLParameter.Create(BoldSystem);
      UMLParameters.Add(UMLParameter);
      UMLParameter.Name := (MoldParameters[Index] as TMoldParameter).ParameterName;
      UMLParameter.typeName := (MoldParameters[Index] as TMoldParameter).ParameterType;
      UMLParameter.kind := (MoldParameters[Index] as TMoldParameter).ParameterKind;
      UMLParameter.SetBoldTV(TAG_ISCONST, BooleanToString((MoldParameters[Index] as TMoldParameter).IsConst));
      UMLParameter.SetBoldTV(TAG_DELPHINAME, TV_NAME);
      UMLParameter.SetBoldTV(TAG_EXPRESSIONNAME, TV_NAME);
    end;
  end;
end;

class function TBoldModelConverter.GetUMLClassByName(const Name: string; UMLModel: TUMLModel): TUMLClass;
var
  UMLClass: TUMLClass;
  i: Integer;
begin
  UMLClass := nil;
  for i := 0 to UMLModel.OwnedElement.Count-1 do
  begin
    if (UMLModel.OwnedElement[i] is TUMLClass) and
      (TUMLClass(UMLModel.OwnedElement[i]).Name = Name) then
    begin
      UMLClass := TUMLClass(UMLModel.OwnedElement[i]);
      break;
    end;
  end;
  if not assigned(UMLClass) then
  begin
    UMLClass := TUMLClass.Create(UMLModel.BoldSystem);
    UMLClass.Name := Name;
    UMLModel.OwnedElement.Add(UMLClass);
  end;
  result := UMLClass;
end;

class function TBoldModelConverter.GetUMLAssociationByName(const name: string; UMLModel: TUMLModel): TUMLAssociation;
begin
  Result := TUMLAssociation.Create(UMLModel.BoldSystem);
  Result.Name := name;
  UMLModel.OwnedElement.Add(Result);
end;

initialization

end.
