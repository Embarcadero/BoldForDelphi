
{ Global compiler directives }
{$include bold.inc}
unit BoldMMImporter;

interface

uses
  BoldUMLModel,
  BoldUMLModelSupport,
  BoldSystem,
  MMToolsAPI;
                        
const
  aeSource = 1;
  aeTarget = 2;

type
 TPass = (PASS1, PASS2);

  TMMModelImporter = class
  private
    fPass: TPass;
    fUMLModel: TUMLModel;
    fBoldSystem: TBoldSystem;
    fMMToolServices: IMMToolServices;
    procedure ImportClass(MMClass: IMMClass; UMLPackage: TUMLPackage);
    procedure ImportAttributeProperty(MMProperty: IMMProperty; UMLClass: TUMLClass);
    procedure ImportAssociationProperty(MMProperty: IMMProperty; UMLClass: TUMLClass; UMLClassOfOtherEnd: TUMLClass);
    procedure ImportProperty(MMProperty: IMMProperty; UMLClass: TUMLClass);
    procedure ImportMethod(MMethod: IMMMethod; UMLClass: TUMLClass);
    procedure GetTaggedValuesAndConstraints(MMModelPart: IMMModelPart; UMLModelElement: TUMLModelElement);
    procedure ImportModel(MMModel: IMMCodeModel);
  public
    constructor Create(UMLModel: TUMLModel; MMToolServices: IMMToolServices);
    property Pass: TPass read fPass write fPass;
    procedure RawImport;
  end;

implementation
uses
  SysUtils,
  MMEngineDefs,
  BoldDefs,
  BoldUtils,
  BoldMMTVDefs,
  BoldDefaultTaggedValues,
  BoldTaggedValueSupport,
  BoldUMLTypes,
  BoldUMLDelphiSupport,
  BoldUMLModelLinkSupport
  ;

function Prefix(name: string): string;
begin
  result := Copy(name, 1, pos('.', name));
end;

function RemovePrefix(name: string): string;
begin
  result := Copy(name, pos('.', name)+1, maxint);
end;

function UMLVisibility(MMVisibility: TVisibility): TVisibilityKind;
begin
  case MMVisibility of
    scPrivate:
      Result := vkPrivate;
    scProtected:
      Result := vkProtected;
    else
      Result := vkPublic;
  end;
end;

procedure DelphiParameterToUMLParameters(Parameters: string; UMLParameters: TUMLParameterList);
var
  Chunk: string;
  ParameterName: string;
  ParameterType: string;
  SemiColonPos, CommaPos: integer;
  IsConst: Boolean;
  Kind: TBoldParameterDirectionKind;
  UMLParameter: TUMLParameter;
begin
  while Parameters <> '' do
  begin
    SemiColonPos := Pos(';', Parameters);
    if SemiColonPos = 0 then
    begin
     Chunk := Parameters;
     Parameters := '';
    end
    else
    begin
      Chunk := Copy(Parameters, 1, SemiColonPos-1);
      Parameters := Copy(Parameters, SemiColonPos+1, MaxInt);
    end;
    IsConst := TBoldUMLDelphiSupport.ExtractIsConst(Chunk);
    Kind := TBoldUMLDelphiSupport.ExtractKind(Chunk);
    ParameterType := TBoldUMLDelphiSupport.ExtractType(Chunk);
    while Chunk <> '' do
    begin
      CommaPos := Pos(',', Chunk);
      if CommaPos = 0 then
      begin
       ParameterName := BoldTrim(Chunk);
       Chunk := '';
      end
      else
      begin
        ParameterName := BoldTrim(Copy(Chunk, 1, CommaPos-1));
        Chunk := BoldTrim(Copy(Chunk, CommaPos+1, MaxInt));
      end;
      UMLParameter := UMLParameters.AddNew;
      TBoldUMLSupport.EnsureBoldTaggedValues(UMLParameter);
      UMLParameter.name := ParameterName;
      UMLParameter.typeName := ParameterType;
      if IsConst then
        UMLParameter.SetBoldTV(TAG_ISCONST, TV_TRUE)
      else
        UMLParameter.SetBoldTV(TAG_ISCONST, TV_FALSE);
      UMLParameter.kind := Kind;
    end;
  end;
end;

procedure QualifierStringToUMLQualifiers(QualifierString: string; UMLQualifiers: TUMLAttributeList);
var
  Chunk: string;
  QualifierName: string;
  QualifierType: string;
  SemiColonPos, CommaPos: integer;
  IsConst: Boolean;
  UMLAttribute: TUMLAttribute;
begin
  if QualifierString = '<auto>' then
    Exit;
  while QualifierString <> '' do
  begin
    SemiColonPos := Pos(';', QualifierString);
    if SemiColonPos = 0 then
    begin
     Chunk := QualifierString;
     QualifierString := '';
    end
    else
    begin
      Chunk := Copy(QualifierString, 1, SemiColonPos-1);
      QualifierString := Copy(QualifierString, SemiColonPos+1, MaxInt);
    end;
    IsConst := TBoldUMLDelphiSupport.ExtractIsConst(Chunk);
    QualifierType := TBoldUMLDelphiSupport.ExtractType(Chunk);
    while Chunk <> '' do
    begin
      CommaPos := Pos(',', Chunk);
      if CommaPos = 0 then
      begin
       QualifierName := BoldTrim(Chunk);
       Chunk := '';
      end
      else
      begin
        QualifierName := BoldTrim(Copy(Chunk, 1, CommaPos-1));
        Chunk := BoldTrim(Copy(Chunk, CommaPos+1, MaxInt));
      end;
      UMLAttribute := UMLQualifiers.AddNew;
      TBoldUMLSupport.EnsureBoldTaggedValues(UMLAttribute);
      UMLAttribute.name := QualifierName;
      UMLAttribute.typeName := QualifierType;
      if IsConst then
        UMLAttribute.SetBoldTV(TAG_ISCONST, TV_TRUE)
      else
        UMLAttribute.SetBoldTV(TAG_ISCONST, TV_FALSE);
    end;
  end;
end;

constructor TMMModelImporter.Create(UMLModel: TUMLModel; MMToolServices: IMMToolServices);
begin
  fUMLModel := UMLModel;
  fBoldSystem := UMLModel.BoldSystem;
  fMMToolServices := MMToolServices;
end;

procedure TMMModelImporter.GetTaggedValuesAndConstraints(MMModelPart: IMMModelPart;
  UMLModelElement: TUMLModelElement);
var
  i: integer;
  Tag, Value: string;
  ActualModelElement: TUMLModelElement;
begin
  for I := 0 to MMModelPart.TaggedValueCount -1 do
  begin
    Tag := MMModelPart.TaggedValueNames[i];
    Value := MMModelPart.TaggedValues[Tag];

    if (Prefix(Tag) = PREFIX_SOURCE_ASSOC_END) and
      (UMLModelElement is TUMLAssociation) then
    begin
      ActualModelElement := TUMLAssociation(UMLModelElement).connection[0];
      Tag := RemovePrefix(Tag);
    end
    else if (Prefix(Tag) = PREFIX_TARGET_ASSOC_END) and
      (UMLModelElement is TUMLAssociation) then
    begin
      ActualModelElement := TUMLAssociation(UMLModelElement).connection[1];
      Tag := RemovePrefix(Tag);
    end
    else if Prefix(Tag) = PREFIX_MODEL then
    begin
      ActualModelElement := UMLModelElement.model;
      Tag := RemovePrefix(Tag);
    end else
      ActualModelElement := UMLModelElement;

    if Tag = BOLDTVPREFIX + TAG_CONSTRAINTS then
      TBoldUMLModelLinkSupport.StringToConstraints(ActualModelElement, Value)
    else
      ActualModelElement.SetTaggedValue(Tag, Value);
   end;
end;

procedure TMMModelImporter.ImportAssociationProperty(
  MMProperty: IMMProperty; UMLClass: TUMLClass; UMLClassOfOtherEnd: TUMLClass);
var
  UMLAssociation: TUMLAssociation;
  SourceUMLAssociationEnd: TUMLAssociationEnd;
  TargetUMLAssociationEnd: TUMLAssociationEnd;
  MMMemberVisualization: IMMMemberVisualization;
  Name: string;
begin
  if pass = PASS2 then
  begin
    UMLAssociation := TUMLAssociation.Create(UMLClass.BoldSystem);
    UMLAssociation.namespace_ := UMLClass.namespace_;
    TBoldUMLSupport.EnsureBoldTaggedValues(UMLAssociation);
    TBoldUMLSupport.AddToolId(UMLAssociation, IntToStr(MMProperty.ID));
    Name := MMProperty.Name;
    UMLAssociation.name := Name;
    UMLAssociation.stereotypeName := MMProperty.Category; 
    UMLAssociation.visibility := UMLVisibility(MMProperty.Visibility);

    MMMemberVisualization := MMProperty as IMMMemberVisualization;
    SourceUMLAssociationEnd := UMLAssociation.connection.AddNew;
    TargetUMLAssociationEnd := UMLAssociation.connection.AddNew;
    GetTaggedValuesAndConstraints(MMProperty as IMMModelPart, UMLAssociation);
    TBoldUMLSupport.EnsureBoldTaggedValues(SourceUMLAssociationEnd);
    TBoldUMLSupport.EnsureBoldTaggedValues(TargetUMLAssociationEnd);
    SourceUMLAssociationEnd.type_ := UMLClass;
    TargetUMLAssociationEnd.type_ :=  UMLClassOfOtherEnd;
    if MMMemberVisualization.RoleName[aeTarget] <> '' then
      TargetUMLAssociationEnd.Name := MMMemberVisualization.RoleName[aeTarget]
    else
    begin
      TargetUMLAssociationEnd.Name := UMLAssociation.name;
      UMLAssociation.Name := '';
    end;
    SourceUMLAssociationEnd.Name := MMMemberVisualization.RoleName[aeSource];
    SourceUMLAssociationEnd.multiplicity := MMMemberVisualization.Multiplicity[aeSource];
    TargetUMLAssociationEnd.multiplicity := MMMemberVisualization.Multiplicity[aeTarget];
    QualifierStringToUMLQualifiers(MMMemberVisualization.Qualifier[aeTarget], SourceUMLAssociationEnd.qualifier);
    QualifierStringToUMLQualifiers(MMMemberVisualization.Qualifier[aeSource], TargetUMLAssociationEnd.qualifier);
    SourceUMLAssociationEnd.isNavigable := True;
    TargetUMLAssociationEnd.isNavigable := True;
    SourceUMLAssociationEnd.isOrdered := StringToBoolean(MMProperty.TaggedValues[PREFIX_SOURCE_ASSOC_END + BOLDTVPREFIX + TAG_ORDERED]);
    TargetUMLAssociationEnd.isOrdered := StringToBoolean(MMProperty.TaggedValues[PREFIX_TARGET_ASSOC_END + BOLDTVPREFIX + TAG_ORDERED]);
    case MMMemberVisualization.AssocStyle[aeTarget] of
      masAuto:
      begin
        SourceUMLAssociationEnd.isNavigable := False;
        SourceUMLAssociationEnd.aggregation := akNone;
      end;
      masNavigable:
      begin
        SourceUMLAssociationEnd.isNavigable := False;
        SourceUMLAssociationEnd.aggregation := akNone;
      end;
      masUndefined:
        SourceUMLAssociationEnd.aggregation := akNone;
      masAggregation:
        SourceUMLAssociationEnd.aggregation := akAggregate;
      masComposition:
        SourceUMLAssociationEnd.aggregation := akComposite;
    end;

    case MMMemberVisualization.AssocStyle[aeSource] of
      masAuto:
      begin
        TargetUMLAssociationEnd.isNavigable := True;
        TargetUMLAssociationEnd.aggregation := akAggregate;
      end;
      masNavigable:
      begin
        TargetUMLAssociationEnd.isNavigable := False;
        TargetUMLAssociationEnd.aggregation := akNone;
      end;
       masUndefined:
        TargetUMLAssociationEnd.aggregation := akNone;
      masAggregation:
        TargetUMLAssociationEnd.aggregation := akAggregate;
      masComposition:
        TargetUMLAssociationEnd.aggregation := akComposite;
    end;
    if UMLAssociation.Persistent and UMLAssociation.Derived then
      UMLAssociation.Persistent := false;
  end;
end;

procedure TMMModelImporter.ImportAttributeProperty(MMProperty: IMMProperty;
  UMLClass: TUMLClass);
var
  UMLAttribute: TUMLAttribute;
  Name: string;
begin
  if pass = PASS2 then
  begin
    UMLAttribute := TUMLAttribute.Create(UMLClass.BoldSystem);
    UMLAttribute.owner := UMLClass;
    TBoldUMLSupport.EnsureBoldTaggedValues(UMLAttribute);
    TBoldUMLSupport.AddToolId(UMLAttribute, IntToStr(MMProperty.ID));
    Name := MMProperty.Name;
    UMLAttribute.name := Name;
    UMLAttribute.stereotypeName := MMProperty.Category;
    GetTaggedValuesAndConstraints(MMProperty as IMMModelPart, UMLAttribute); 
    UMLAttribute.typeName := MMProperty.DataName;
    if MMProperty.DefaultSpec =  dsDefault then
      UMLAttribute.initialValue := MMProperty.DefaultSpecStr;
    UMLAttribute.visibility := UMLVisibility(MMProperty.Visibility);
    if UMLAttribute.Persistent and UMLAttribute.Derived then
      UMLAttribute.Persistent := false;
  end;
end;

procedure TMMModelImporter.ImportClass(MMClass: IMMClass;
  UMLPackage: TUMLPackage);
var
  i: integer;
  UMLClass: TUMLClass;
  MMMember: IMMMember;
  name: string;
begin

  if pass = PASS1 then
  begin
    UMLClass := TUMLClass.Create(UMLPackage.BoldSystem);
    UMLClass.namespace_ := UMLPackage;
    TBoldUMLSupport.EnsureBoldTaggedValues(UMLClass);
    TBoldUMLSupport.AddToolId(UMLClass, IntToStr(MMClass.ID));
    UMLClass.name := MMClass.Name;
    if UMLClass.name = 'TObject' then
    begin
      UMLClass.name := 'BusinessClassesRoot';
    end;
    UMLClass.stereotypeName := MMClass.Category;
    UMLClass.isAbstract := MMClass.Options[classAbstract];

    GetTaggedValuesAndConstraints(MMClass as IMMModelPart, UMLClass);
    end
    else
    begin
      name := MMclass.Name;
      if name = 'TObject' then
      begin
        name := 'BusinessClassesRoot';
      end;

      UMLClass := UMLPackage.EvaluateExpressionAsDirectElement(Format('classes->select(name=''%s'')->first', [Name])) as TUMLClass; 
      Assert(Assigned(UMLClass));
      if MMClass.Ancestor <> nil then
        UMLClass.SetFirstParent(UMLPackage.EvaluateExpressionAsDirectElement(Format('classes->select(name=''%s'')->first', [MMClass.Ancestor.Name])) as TUMLClass); 
      for I := 0 to MMClass.MemberCount-1 do
      begin
        MMMember := MMClass.Members[i];
        case MMMember.MemberType of
          cpResClause:
            ;
          cpField:
            ;
          cpMethod:
            ImportMethod(MMMember as IMMMethod, UMLClass);
          cpProperty:
            ImportProperty(MMMember as IMMProperty, UMLClass);
          cpEvent:
            ;
          else
            raise EBold.Create('Unknown Membertype');
        end;
      end;
    end;
   end;

procedure TMMModelImporter.ImportMethod(MMethod: IMMMethod;
  UMLClass: TUMLClass);
var
  UMLOperation: TUMLOperation;
  ReturnParameter: TUMLParameter;
begin
  if (MMethod.MethodKind in [mkConstructor, mkDestructor]) or
  (MMethod.BindingKind = bkMessage) then
    Exit;
  UMLOperation := TUMLOperation.Create(UMLClass.BoldSystem);
  UMLOperation.owner := UMLClass;
  TBoldUMLSupport.EnsureBoldTaggedValues(UMLOperation);
  TBoldUMLSupport.AddToolId(UMLOperation, IntToStr(MMethod.ID));
  UMLOperation.name := MMethod.Name;
  UMLOperation.stereotypeName := MMethod.Category;
  UMLOperation.visibility := UMLVisibility(MMethod.Visibility);
  if MMethod.IsAbstract then
      UMLOperation.SetBoldTV(TAG_DELPHIOPERATIONKIND, TV_DELPHIOPERATIONKIND_ABSTRACTVIRTUAL)
  else
    case MMethod.BindingKind of
      bkStatic:
        UMLOperation.SetBoldTV(TAG_DELPHIOPERATIONKIND, TV_DELPHIOPERATIONKIND_NORMAL);
      bkVirtual:
        UMLOperation.SetBoldTV(TAG_DELPHIOPERATIONKIND, TV_DELPHIOPERATIONKIND_VIRTUAL);
      bkOverride:
        UMLOperation.SetBoldTV(TAG_DELPHIOPERATIONKIND, TV_DELPHIOPERATIONKIND_OVERRIDE);
      bkDynamic:
        UMLOperation.SetBoldTV(TAG_DELPHIOPERATIONKIND, TV_DELPHIOPERATIONKIND_DYNAMIC);
      else
        raise EBoldInternal.Create('Internal error');
    end;

  if MMethod.Options[methodClassMethod] then
    UMLOperation.ownerScope := skClassifier
  else
    UMLOperation.ownerScope := skInstance;

  GetTaggedValuesAndConstraints(MMethod as IMMModelPart, UMLOperation);
  DelphiParameterToUMLParameters(MMethod.Parameters, UMLOperation.parameter);
  if MMethod.MethodKind = mkFunction then
    begin
      ReturnParameter := UMLOperation.Parameter.AddNew;
      TBoldUMLSupport.EnsureBoldTaggedValues(ReturnParameter);
      ReturnParameter.name := 'return';
      ReturnParameter.kind := pdReturn;
      ReturnParameter.typeName := MMethod.DataName;
    end;
end;

procedure TMMModelImporter.ImportProperty(MMProperty: IMMProperty;
  UMLClass: TUMLClass);
var
  TypeAsUMLClass: TUMLClass;
begin
  if Pass = PASS2 then
  begin
    TypeAsUMLClass :=  UMLClass.namespace_.EvaluateExpressionAsDirectElement(Format('classes->select(name=''%s'')->first', [MMProperty.DataName])) as TUMLClass; ;
    if Assigned(TypeAsUMLClass) then
      ImportAssociationProperty(MMProperty, UMLClass, TypeAsUMLClass)
    else
      ImportAttributeProperty(MMProperty, UMLClass);
  end;
end;

procedure TMMModelImporter.ImportModel(MMModel: IMMCodeModel);
var
  i: integer;
  ClassBase: IMMClassBase;
begin
 for i := 0 to MMToolServices.CodeModel.ClassCount -1 do
  begin
    ClassBase := MMModel.Classes[i];
    if ClassBase.IsInterface then
    else
       ImportClass(ClassBase as IMMClass, fUMLModel)
  end;
end;

procedure TMMModelImporter.RawImport;
begin
  fUMLModel.BoldSystem.StartTransaction;
  try
    fUMLModel.name := ChangeFileExt(ExtractFileName(MMToolServices.ProjectManager.ProjectName), '');
    pass := PASS1;
    ImportModel(MMToolServices.CodeModel);
    pass := PASS2;
    ImportModel(MMToolServices.CodeModel);
    fUMLModel.BoldSystem.CommitTransaction;
  except
    fUMLModel.BoldSystem.RollbackTransaction;
    raise;
  end;
end;

end.
