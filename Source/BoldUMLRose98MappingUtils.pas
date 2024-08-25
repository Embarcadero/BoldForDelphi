
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLRose98MappingUtils;

interface 

uses
  Classes,
  BoldUMLModelLink,
  RationalRose98_TLB,
  BoldAbstractModel,
  BoldUMLModel,
  BoldDefs,
  BoldQueue,
  BoldRose98Support,
  BoldTaggedValueSupport,
  BoldUMLTypes,
  BoldSystem;

type
  { forward declarations }
  TBoldUMLRose98MappingUtils = class;

  { TBoldUMLRose98MappingUtils }
  TBoldUMLRose98MappingUtils = class
  private
    fLogicalPackages: TStrings;
    fIncludeSubPackages: Boolean;
    fUMLModel: TUMLModel;
    fCachedClasses: TStringList;
    fCachedAssociations: TStringList;
    fCachedElements: TSTringList;
    fUMLModelreadOnly: Boolean;
    procedure SetLogicalPackages(Value: TStrings); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SetUMLModel(const Value: TUMLModel); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function AssociationInLogicalPackages(RoseAssociation: IRoseAssociation): Boolean;
    function ClassInLogicalPackages(RoseClass: IRoseClass): Boolean;
    function RoseClassForUMLClassifier(RoseModel: IRoseModel; UMLClassifier: TUMLClassifier): IRoseClass;
    function RoseAssociationForUMLAssociation(RoseModel: IRoseModel; UMLAssociation: TUMLAssociation): IRoseAssociation;
    function FindClass(const UniqueId: String): TUMLClass;
    function FindAssocation(const UniqueId: String): TUMLASsociation;
    function FindElement(const UniqueId: String): TUMLElement;
    function FindInCache(Cache: TStringList; const UniqueId: string): TUMLElement;
    procedure RefreshCache(var Cache: TStringList; const FilterType: string);
    procedure ClearCaches;
    function RoseAttributeForUMLAttribute(RoseClass: IRoseClass; UMLAttribute: TUMLAttribute): IRoseAttribute;
    function RoseOperationForUMLOperation(RoseClass: IRoseClass; UMLOperation: TUMLOperation): IRoseOperation;
    function UMLAssociationForRoseASsociation(UMLModel: TUMLModel; RoseAssociation: IRoseAssociation): TUMLAssociation;
    function UMLClassifierForRoseClass(UMLModel: TUMLModel; RoseClass: IRoseClass): TUMLClassifier;
    function UMLAttributeForRoseAttribute(UMLClass: TUMLClass; RoseAttribute: IRoseAttribute): TUMLAttribute;
    function UMLOperationForRoseOperation(UMLClass: TUMLClass; RoseOperation: IRoseOperation): TUMLOperation;
    function AssociationRolesMatch(UMLAssociation: TUMLAssociation; RoseAssociation: IRoseAssociation): Boolean;
    function AssociationReverseRolesMatch(UMLAssociation: TUMLAssociation; RoseAssociation: IRoseAssociation): Boolean;
    function RoleMatch(UMLAssociationEnd: TUMLAssociationEnd; RoseRole: IRoseRole): Boolean;

    property LogicalPackages: TStrings read fLogicalPackages write SetLogicalPackages;
    property IncludeSubPackages: Boolean read fIncludeSubPackages write fIncludeSubPackages default False;
    property UMLModel: TUMLModel read FUMLModel write SetUMLModel;
    property UMLModelreadOnly: Boolean read fUMLModelreadOnly write fUMLModelreadOnly;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  Dialogs,
  BoldLogHandler,
  BoldUMLRose98Support,
  BoldUMLAttributes,
  BoldUMLModelSupport,
  BoldUMLModelLinkSupport,
  BoldNameExpander,
  BoldPerformanceCounter,
  BoldDefaultTaggedValues,
  BoldRose98TaggedValues;

function TBoldUMLRose98MappingUtils.RoseClassForUMLClassifier(RoseModel: IRoseModel; UMLClassifier: TUMLClassifier): IRoseClass;
var
  RoseClassCollection: IRoseClassCollection;
  RoseClass: IRoseClass;
  I: Integer;
begin
  Result := nil;
  if TBoldUMLSupport.GetToolId(UMLClassifier) <> '' then
    Result := RoseModel.FindClassWithID(TBoldUMLSupport.GetToolId(UMLClassifier))
  else
  begin
    RoseClassCollection := RoseModel.FindClasses(UMLClassifier.Name);
    for I := 1 to RoseClassCollection.Count do
    begin
      RoseClass := RoseClassCollection.GetAt(I);
      if ClassInLogicalPackages(RoseClass) then
      begin
        if Assigned(Result) then
          raise EBoldImport.CreateFmt('Found multiple classes with name %s, can''t determine which one to use', [UMLClassifier.Name])
        else
          Result := RoseClass;
      end;
    end;
  end;
end;

function TBoldUMLRose98MappingUtils.UMLClassifierForRoseClass(UMLModel: TUMLModel; RoseClass: IRoseClass): TUMLClassifier;
begin
  result := nil;
  if assigned(RoseClass) then
  begin
    Result := FindClass(RoseClass.GetUniqueID);
    if not Assigned(Result) then
    begin
      Result :=
       UMLModel.EvaluateExpressionAsDirectElement
      (
      Format('UMLClassifier.allInstances->select(model=self)->select(name=''%s'')->first',
        [RoseClass.Name])
      ) as TUMLClassifier;
      if Assigned(Result) and (TBoldUMLSupport.GetToolId(Result) <> '') then
        Result := nil;
    end;
  end;
end;

function TBoldUMLRose98MappingUtils.UMLAssociationForRoseAssociation(UMLModel: TUMLModel; RoseAssociation: IRoseAssociation): TUMLAssociation;
var
  UMLClassifier1: TUMLClassifier;
  i: integer;
begin
  Result := FindAssocation(RoseAssociation.GetUniqueID);
  if not Assigned(Result) then
  begin
    Result :=
     UMLModel.EvaluateExpressionAsDirectElement
    (
    Format('UMLAssociation.allInstances->select(model=self)->select(name=''%s'')->first',
      [RoseAssociation.Name])
    ) as TUMLAssociation;
    if not Assigned(Result) then
    begin
      UMLClassifier1 := UMLClassifierForRoseClass(UMLModel, RoseAssociation.Role1.Class_);
      if Assigned(UMLClassifier1) then
      begin
        for i := 0 to UMLClassifier1.associationEnd.Count - 1 do
        begin
          with UMLClassifier1.associationEnd[i].association do
          begin
            if AssociationRolesMatch(UMLClassifier1.associationEnd[i].association, RoseAssociation) or
               AssociationReverseRolesMatch(UMLClassifier1.associationEnd[i].association, RoseAssociation) then
            begin
              Result := UMLClassifier1.associationEnd[i].association;
              break;
            end;
          end;
        end;
      end;
    end;
    if Assigned(Result) and (TBoldUMLSupport.GetToolId(Result) <> '') then
      Result := nil;
  end;
end;

function TBoldUMLRose98MappingUtils.RoseAssociationForUMLAssociation(
  RoseModel: IRoseModel;
  UMLAssociation: TUMLAssociation): IRoseAssociation;
var
  RoseAssociationCollection: IRoseAssociationCollection;
  RoseAssociation: IRoseAssociation;
  I: Integer;
begin
  Result := nil;
  if TBoldUMLSupport.GetToolId(UMLAssociation) <> '' then
    Result := RoseModel.GetAllAssociations.GetWithUniqueID(TBoldUMLSupport.GetToolId(UMLAssociation))
  else
  begin
    RoseAssociationCollection := RoseModel.GetAllAssociations;
    for I := 1 to RoseAssociationCollection.Count do
    begin
      RoseAssociation := RoseAssociationCollection.GetAt(I);
      if AssociationInLogicalPackages(RoseAssociation) then
      begin
        if RoseAssociation.Name = UMLAssociation.name then
        begin
          if Assigned(Result) then
            raise EBoldImport.CreateFmt('Found multiple associations with name %s, can''t determine which one to use', [UMLAssociation.name])
          else
            Result := RoseAssociation;
        end;
      end;
    end;
  end;
end;

function TBoldUMLRose98MappingUtils.UMLAttributeForRoseAttribute(
  UMLClass: TUMLClass; RoseAttribute: IRoseAttribute): TUMLAttribute;
begin
  Result := FindElement(RoseAttribute.GetUniqueID) as TUMLAttribute;
  if not Assigned(Result) or (Result.Owner <> UMLClass) then
  begin
    Result :=
     UMLClass.EvaluateExpressionAsDirectElement
    (
    Format('feature->select((name=''%s'') and oclIsKindOf(UMLAttribute))->first',
      [RoseAttribute.Name])
    ) as TUMLAttribute;
    if Assigned(Result) and (TBoldUMLSupport.GetToolId(Result) <> '') then
      Result := nil;
  end;
end;

function TBoldUMLRose98MappingUtils.RoseAttributeForUMLAttribute(
  RoseClass: IRoseClass; UMLAttribute: TUMLAttribute): IRoseAttribute;
begin
  Result := nil;
  if TBoldUMLSupport.GetToolId(UMLAttribute) <> '' then
    Result := RoseClass.Attributes.GetWithUniqueID(TBoldUMLSupport.GetToolId(UMLAttribute))
  else
    Result := RoseClass.Attributes.GetFirst(UMLAttribute.Name);
end;

function TBoldUMLRose98MappingUtils.RoseOperationForUMLOperation(
  RoseClass: IRoseClass; UMLOperation: TUMLOperation): IRoseOperation;
begin
  Result := nil;
  if TBoldUMLSupport.GetToolId(UMLOperation) <> '' then
    Result := RoseClass.Operations.GetWithUniqueID(TBoldUMLSupport.GetToolId(UMLOperation))
  else
    Result := RoseClass.Operations.GetFirst(UMLOperation.Name);
end;

function TBoldUMLRose98MappingUtils.UMLOperationForRoseOperation(
  UMLClass: TUMLClass; RoseOperation: IRoseOperation): TUMLOperation;
begin
  Result := FindElement(RoseOperation.GetUniqueID) as TUMLOperation;
  if not Assigned(Result) or (Result.Owner <> UMLClass) then
  begin
    Result :=
     UMLClass.EvaluateExpressionAsDirectElement
    (
    Format('feature->select((name=''%s'') and oclIsKindOf(UMLOperation))->first',
      [RoseOperation.Name])
    ) as TUMLOperation;
    if Assigned(Result) and (TBoldUMLSupport.GetToolId(Result) <> '') then
      Result := nil;
   end;
end;

constructor TBoldUMLRose98MappingUtils.Create;
begin
  inherited;
  fLogicalPackages := TStringList.Create;
  fIncludeSubPackages := False;
end;

destructor TBoldUMLRose98MappingUtils.Destroy;
begin
  FreeAndNil(fLogicalPackages);
  ClearCaches;
  inherited;
end;

procedure TBoldUMLRose98MappingUtils.SetLogicalPackages(Value: TStrings);
begin
  fLogicalPackages.Assign(Value);
end;

function TBoldUMLRose98MappingUtils.AssociationInLogicalPackages(
  RoseAssociation: IRoseAssociation): Boolean;
var
  RoseRole1, RoseRole2: IRoseRole;
begin
  Result := False;
  if not Assigned(RoseAssociation) then Exit;
  RoseRole1 := RoseAssociation.Role1;
  RoseRole2 := RoseAssociation.Role2;
  if Assigned(RoseRole1) and Assigned(RoseRole2) then
  begin
    if Assigned(RoseRole1.Class_) then
      Result := ClassInLogicalPackages(RoseRole1.Class_)
    else
    begin
      if not Assigned(RoseRole1.UseCase) then
        BoldLog.LogFmt('Warning: Incomplete association %s ignored', [RoseAssociation.Name], ltWarning);
    end;
    if Result then
    begin
      if Assigned(RoseRole2.Class_) then
        Result := Result and ClassInLogicalPackages(RoseRole2.Class_)
      else
      begin
        if not Assigned(RoseRole2.UseCase) then
          BoldLog.LogFmt('Warning: Incomplete association %s ignored', [RoseAssociation.Name], ltWarning);
      end;
    end;
  end;
end;

function TBoldUMLRose98MappingUtils.ClassInLogicalPackages(
  RoseClass: IRoseClass): Boolean;
var
  Cat: IRoseCategory;
begin
  if not Assigned(RoseClass) then
  begin
    Result := False;
    Exit;
  end;
  if LogicalPackages.Count = 0 then
  begin
    cat := RoseClass.ParentCategory;
    while assigned(cat) and not Cat.TopLevel do
      Cat := cat.ParentCategory;
    Result := assigned(Cat) and (Cat = RoseClass.Model.RootCategory);
  end
  else
  begin
    Cat := RoseClass.ParentCategory;
    if Cat.TopLevel then
      Result := LogicalPackages.IndexOf('<Root Package>') <> -1
    else
    begin
      Result := LogicalPackages.IndexOf(Cat.Name) <> -1;
      if not Result and IncludeSubPackages then
      begin
        while not Result and not Cat.TopLevel do
        begin
          Cat := Cat.ParentCategory;
          Result := LogicalPackages.IndexOf(Cat.Name) <> -1;
        end;
      end;
    end;
  end;
end;

function TBoldUMLRose98MappingUtils.AssociationReverseRolesMatch(
  UMLAssociation: TUMLAssociation;
  RoseAssociation: IRoseAssociation): Boolean;
begin
  Result :=
    (UMLAssociation.connection.Count = 2) and
    Rolematch(UMLAssociation.connection[1], RoseAssociation.Role1) and
    Rolematch(UMLAssociation.connection[0], RoseAssociation.Role2);
end;

function TBoldUMLRose98MappingUtils.AssociationRolesMatch(
  UMLAssociation: TUMLAssociation;
  RoseAssociation: IRoseAssociation): Boolean;
begin
  Result :=
    (UMLAssociation.connection.Count = 2) and
    Rolematch(UMLAssociation.connection[0], RoseAssociation.Role1) and
    Rolematch(UMLAssociation.connection[1], RoseAssociation.Role2);
end;

function TBoldUMLRose98MappingUtils.RoleMatch(
  UMLAssociationEnd: TUMLAssociationEnd; RoseRole: IRoseRole): Boolean;
begin
  if TBoldUMLSupport.GetToolId(UMLAssociationEnd) <> '' then
    Result := TBoldUMLSupport.GetToolId(UMLAssociationEnd) = RoseRole.GetUniqueID
  else
    Result :=
      (UMLAssociationEnd.name = RoseRole.Name) and
      (UMLClassifierForRoseClass(UMLAssociationEnd.Model, RoseRole.Class_) = UMLAssociationEnd.type_);
end;

procedure TBoldUMLRose98MappingUtils.SetUMLModel(const Value: TUMLModel);
begin
  FUMLModel := Value;
  ClearCaches;
end;

procedure TBoldUMLRose98MappingUtils.ClearCaches;
begin
  FreeAndNil(fCachedClasses);
  FreeAndNil(fCachedAssociations);
  FreeAndNil(fCachedElements);
end;

function TBoldUMLRose98MappingUtils.FindClass(const UniqueId: String): TUMLClass;
begin
  Result := FindInCache(fCachedClasses, UniqueId) as TUMLClass;
  if not Assigned(Result) then
  begin
    RefreshCache(fCachedClasses, 'Class');
    Result := FindInCache(fCachedClasses, UniqueId) as TUMLClass;
  end;
end;

function TBoldUMLRose98MappingUtils.FindInCache(Cache: TStringList;
  const UniqueId: string): TUMLElement;
var
  pos: integer;
begin
  result := nil;
  if Assigned(Cache) then
  begin
    pos := Cache.IndexOf(UniqueId);
    if pos <> -1 then
      result := Cache.Objects[pos] as TUMLElement;
  end;
end;

procedure TBoldUMLRose98MappingUtils.RefreshCache(var Cache: TStringList;
  const FilterType: string);
var
  i: integer;
  ToolId: string;
  Elementlist: TUMLModelElementList;
begin
  if Assigned(Cache) and UMLModelReadOnly then
    Exit;
  FreeAndNil(Cache);
  Cache := TStringList.Create;
  Elementlist := UMLModel.EvaluateExpressionAsNewElement(
    Format('UML%s.allInstances->reject(oclIsKindOf(UMLTaggedValue))->select(model=self)', [FilterType])) as TUMLModelElementList;
  try
    for i := 0 to Elementlist.Count-1 do
    begin
      ToolId :=  TBoldUMLSupport.GetToolId(Elementlist[i]);
      if ToolId <> '' then
        Cache.AddObject(ToolId, Elementlist[i]);
    end;
  finally
    Elementlist.Free;
  end;
  Cache.Sorted := true;
end;

function TBoldUMLRose98MappingUtils.FindAssocation(const UniqueId: String): TUMLASsociation;
var
  temp: TUMLElement;
begin
  temp := FindInCache(fCachedAssociations, UniqueId);
  if temp is TUMLAssociation then
    Result := temp as TUMLASsociation
  else
    result := nil;
  if not Assigned(Result) then
  begin
    RefreshCache(fCachedAssociations, 'Association');
    Result := FindInCache(fCachedAssociations, UniqueId) as TUMLASsociation;
  end;
end;

function TBoldUMLRose98MappingUtils.FindElement(const UniqueId: String): TUMLElement;
begin
  Result := FindInCache(fCachedElements , UniqueId) as TUMLElement;
  if not Assigned(Result) then
  begin
    RefreshCache(fCachedElements, 'ModelElement');
    Result := FindInCache(fCachedElements, UniqueId) as TUMLElement;
  end;
end;

end.
