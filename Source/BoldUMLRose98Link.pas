
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLRose98Link;  
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Forms,
  Classes,
  BoldUMLModelLink,
  RationalRose98_TLB,
  BoldAbstractModel,
  BoldUMLModel,
  BoldDefs,
  BoldQueue,
  BoldRose98Support,
  BoldTaggedValueSupport,
  BoldUMLRose98MappingUtils,
  BoldUMLTypes,
  BoldSystem;

type
  { forward declareation of classes }
  TBoldUMLRose98Link = class;

  TRoseLinkPass = (Pass1, Pass2);

  { TBoldRoseLink }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldUMLRoseLink = class(TBoldUMLModelLink)
  private
    fFileName: string;
    fTools: TStrings;
    fRoseModel: IRoseModel;
    fMakeClassesPersistent: Boolean;
    fUMLModel: TUMLModel;
    fCachedTypes: TStringList;
    fCachedPackages: TStringList;
    fIgnoreBrokenAssociations: Boolean;
    fIgnoreUseCaseAssociations: Boolean;
    fAdjustEmbedFlag: Boolean;
    fImplicitRolesUMLCompliant: Boolean;
    fMapping: TBoldUMLRose98MappingUtils;
    fGetToolIdOnExport: Boolean;
    function DefaultLogicalPackage: IRoseCategory; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure EnsureModel;
    procedure ExportAssociation(UMLAssociation: TUMLAssociation; RoseAssociation : IRoseAssociation);
    procedure EnsureandExportAssociations(TheModel: TUMLModel);
    procedure ExportAttribute(UMLAttribute: TUMLAttribute; RoseClass: IRoseClass);
    procedure ExportClass(UMLClass: TUMLClass);
    procedure ExportOperation(UMLOperation: TUMLOperation; RoseClass: IRoseClass);
    procedure ExportSignature(UMLOperation: TUMLOperation; RoseOperation: IRoseOperation);
    procedure ExportRole(UMLAssociationEnd: TUMLAssociationEnd; RoseRole: IRoseRole; OtherRoseRole: IRoseRole);
    procedure ExportQualifier(UMLQualifier: TUMLAttribute; RoseRole: IRoseRole);
    procedure ExportInheritance(UMLClass: TUMLClass);
    procedure ExportConstraints(RoseItem: IRoseItem; UMLElement: TUMLModelElement);
    function GetEnsuredUMLDataType(const Name: string): TUMLDataType;
    procedure ImportAssociation(RoseAssociation: IRoseAssociation; UMLAssociation: TUMLAssociation);
    procedure ImportAttribute(RoseAttribute: IRoseAttribute; UMLAttribute: TUMLAttribute);
    procedure ImportClass(RoseClass: IRoseClass; UMLClass: TUMLClass; pass: TRoseLinkPass);
    procedure ImportPackage(RosePackage: IRoseCategory; UMLPackage: TUMLPackage; pass: TRoseLinkPass);
    procedure ImportOperation(RoseOperation: IRoseOperation; UMLOperation: TUMLOperation);
    procedure ImportRole(RoseRole: IRoseRole; OtherRole: IRoseRole; UMLAssociationEnd: TUMLAssociationEnd);
    procedure ImportQualifier(RoseAttribute: IRoseAttribute; UMLAssociationEnd: TUMLAssociationEnd);
    procedure ImportSignature(RoseOperation: IRoseOperation; UMLOperation: TUMLOperation);
    procedure ImportConstraints(RoseItem: IRoseItem; UMLElement: TUMLModelElement);
    procedure SetLogicalPackages(Value: TStrings); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SetTools(Value: TStrings); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    property RoseModel: IRoseModel read fRoseModel;
    function GetBoldSystem: TBoldSystem; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function FindPackage(const UniqueId: String): TUMLPackage;
    procedure RefreshCache(var Cache: TStringList; FilterType: TClass);
    function FindInCache(Cache: TStringList; UniqueId: string): TUMLElement;
    procedure ReadObsoleteProperty(Reader: TReader; const PropertyName, NewPropertyName: string);
    procedure ReadObsoletePluralSuffix(Reader: TReader); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure ReadObsoletMultiplicityForRoles(Reader: TReader); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure ReadObsoleteMultiplicityForNonNavigableRoles(Reader: TReader); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetLogicalPackages: TStrings; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetIncludeSubPackages: Boolean; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SetIncludeSubPackages(const Value: Boolean); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  protected
    function GetCanExport: Boolean; override;
    function GetDisplayName: string; override;
    function GetFileName: string; override;
    procedure SetFileName(const Value: string); override;
    procedure DefineProperties(Filer: TFiler); override;
    property UMLModel: TUMLModel read fUMLModel;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    function ExportModel(UMLModel: TUMLModel): Boolean; override;
    function ImportModel(UMLModel: TUMLModel): Boolean; override;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property AdjustEmbedFlag: Boolean read fAdjustEmbedFlag write fAdjustEmbedFlag;
  published
    property FileName;
    property IncludeSubPackages: Boolean read GetIncludeSubPackages write SetIncludeSubPackages default False;
    property LogicalPackages: TStrings read GetLogicalPackages write SetLogicalPackages;
    property Tools: TStrings read fTools write SetTools;
    property MakeClassesPersistent: Boolean read fMakeClassesPersistent write fMakeClassesPersistent default True;
    property BoldModel;
    property ImplicitRolesUMLCompliant: Boolean read fImplicitRolesUMLCompliant write fImplicitRolesUMLCompliant default false;
    property IgnoreBrokenAssociations: Boolean read fIgnoreBrokenAssociations write fIgnoreBrokenAssociations default true;
    property IgnoreUseCaseAssociations: Boolean read fIgnoreUseCaseAssociations write fIgnoreUseCaseAssociations default true;
    property GetToolIdOnExport: Boolean read fGetToolIdOnExport write fGetToolIdOnExport default true;
  end;

 { TBoldUMLRose98Link }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
 TBoldUMLRose98Link = class(TBoldUMLRoseLink)
 end;

const
  ROSE_LINKEXTENSION: string = '.mdl';
  ROSE_LINKDESC: string = 'Rational Rose model';

implementation

uses
  SysUtils,
  BoldUtils,
  Dialogs,
  System.UITypes,
  BoldLogHandler,
  BoldUMLDelphiSupport,
  BoldUMLRose98Support,
  BoldUMLAttributes,
  BoldUMLModelSupport,
  BoldUMLModelLinkSupport,
  BoldUMLTaggedValues,
  BoldNameExpander,
  BoldPerformanceCounter,
  BoldDefaultTaggedValues,
  BoldRose98TaggedValues;

var
  RoseProp: TBoldUMLRose98Properties;

{ TBoldRoseLink }

constructor TBoldUMLRoseLink.Create;
begin
  inherited;
  fMapping := TBoldUMLRose98MappingUtils.Create;
  fTools := TStringList.Create;
  fMakeClassesPersistent := True;
  fAdjustEmbedFlag := true;
  fIgnoreBrokenAssociations := true;
  fIgnoreUseCaseAssociations := true;
  fGetToolIdOnExport := true;
end;

destructor TBoldUMLRoseLink.Destroy;
begin
  FreeAndNil(fMapping);
  FreeAndNil(fTools);
  FreeAndNil(fCachedPackages);
  FreeAndNil(fCachedTypes);
  inherited;
end;

function TBoldUMLRoseLink.GetCanExport: Boolean;
begin
  Result := True;
end;

function TBoldUMLRoseLink.GetDisplayName: string;
begin
  Result := FFileName;
end;

function TBoldUMLRoseLink.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TBoldUMLRoseLink.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TBoldUMLRoseLink.SetLogicalPackages(Value: TStrings);
begin
  fMapping.LogicalPackages := Value;
end;

function TBoldUMLRoseLink.DefaultLogicalPackage: IRoseCategory;
begin
  Result := RoseModel.RootCategory;
end;

procedure TBoldUMLRoseLink.EnsureModel;
var
  Model: IRoseModel;
  RoseApplication: IRoseApplication;
  AllCategorys: IRoseCategoryCollection;
  Category: IRoseCategory;
  i: Integer;
 begin
  fRoseModel := nil;
  RoseApplication := TBoldUMLRose98Support.GetApplication;
  if Assigned(RoseApplication) then
  begin
    if Trim(FileName) <> '' then
    begin
      if AnsiCompareFileName(FileName, RoseApplication.CurrentModel.Name) <> 0 then
      begin
        Model := RoseApplication.OpenModel(FileName);
        if not Assigned(Model) then
          raise EBoldImport.CreateFmt('Failed to open model %s', [FileName]);
      end;
      fRoseModel := RoseApplication.CurrentModel;
    end
    else
      raise EBoldImport.Create('No model file name specified');
  end
  else
    raise EBoldImport.Create('Failed to launch Rational Rose');
  AllCategorys := fRoseModel.GetAllCategories;
  for i := 1 to AllCategorys.Count do
  begin
    Category := AllCategorys.GetAt(i);
    if Category.IsControlled and (not Category.IsLoaded) then
    begin
      fRoseModel := nil;
      raise EBoldImport.CreateFmt('Model contains the non-loaded controlled unit:%s', [Category.Name])
    end
  end;
end;

procedure TBoldUMLRoseLink.ImportAssociation(RoseAssociation: IRoseAssociation; UMLAssociation: TUMLAssociation);
var
  RoseLinkClass: IRoseClass;
  RoseRole1, RoseRole2: IRoseRole;
  UseCasename, RoseClassName: string;
  RoseItem: IRoseItem;
  UMLAssociationEnd: TUMLAssociationEnd;
  TVPersistence: TUMLTaggedValue;
begin
  RoseItem := RoseAssociation as IRoseItem;
  RoseRole1 := RoseAssociation.Role1;
  RoseRole2 := RoseAssociation.Role2;
  if IgnoreUseCaseAssociations and (assigned(RoseRole1.UseCase) or assigned(RoseRole2.UseCase)) then
  begin
    if assigned(RoseRole1.UseCase) then
      usecasename := RoseRole1.UseCase.Name
    else
      usecasename := RoseRole2.UseCase.Name;

    BoldLog.LogFmt('Ignoring association "%s" in use case %s', [RoseAssociation.Name, usecasename]);
    UMLAssociation.Delete;
    exit;
  end;
  if IgnoreBrokenAssociations and (not assigned(roseRole1.class_) or not assigned(roserole2.class_)) then
  begin
    if assigned(RoseRole1.Class_) then
      RoseClassName := RoseRole1.Class_.Name
    else if assigned(RoseRole2.Class_) then
      RoseClassName := RoseRole2.Class_.Name
    else
      roseClassName := '';
    BoldLog.LogFmt('Ignoring broken association "%s" (roles: "%s", "%s", one end: "%s")', [RoseAssociation.Name, RoseRole1.Name, RoseRole2.Name, RoseClassName], ltWarning);
    UMLAssociation.Delete;
    exit;
  end;

  with UMLAssociation do
  begin
    Name := RoseAssociation.Name;
    BoldLog.Log(Format('Association: %s', [qualifiedName]));
    TBoldUMLSupport.AddToolId(UMLAssociation, RoseAssociation.getUniqueId);

    RoseProp.GetTaggedValues(RoseItem, (UMLAssociation as TUMLModelElement), Tools);
    TVPersistence := taggedValue[BOLDSTDUMLTOOLNAME + '.' + TAG_PERSISTENCE ];
    if not assigned(TVPersistence) then
      TVPersistence := taggedValue[ BOLDSTDUMLTOOLNAME + '.' + 'Persistence' ];
    if assigned(TVPersistence) then
      TVPersistence.tag := TAG_PERSISTENCE;
    TVPersistence := taggedValue[ BOLDSTDUMLTOOLNAME + '.' + 'Persistence' ];
    if assigned(TVPersistence) then
      TVPersistence.Delete;

    TVPersistence := taggedValue[BOLDSTDUMLTOOLNAME + '.' + 'PersistenceSet'];
    if assigned(TVPersistence) then
      TVPersistence.Delete;

    StereotypeName := RoseItem.Stereotype;
    Derived := RoseAssociation.Derived;
    ImportConstraints(RoseItem, UMLAssociation);

    UMLAssociationEnd := TUMLAssociationEnd.Create(UMLAssociation.BoldSystem);
    UMLAssociationEnd.association := UMLAssociation;
    ImportRole(RoseRole1, RoseRole2, UMLAssociationEnd);
    UMLAssociationEnd := TUMLAssociationEnd.Create(UMLAssociation.BoldSystem);
    UMLAssociationEnd.association := UMLAssociation;
    ImportRole(RoseRole2, RoseRole1, UMLAssociationEnd);
    RoseLinkClass := RoseAssociation.LinkClass;
    if Assigned(RoseLinkClass) then
      UMLAssociation.Class_ := fMapping.FindClass(RoseLinkClass.GetUniqueID);
    if Persistent and Derived then
    Persistent := false;
  end;
end;

procedure TBoldUMLRoseLink.ImportAttribute(RoseAttribute: IRoseAttribute; UMLAttribute: TUMLAttribute);
var
  RoseItem: IRoseItem;
  TVPersistence: TUMLTaggedValue;
begin
  RoseItem := RoseAttribute as IRoseItem;
  with UMLAttribute do
  begin
    Name := RoseAttribute.Name;
    TBoldUMLSupport.AddToolId(UMLAttribute, RoseAttribute.getUniqueId);
    RoseProp.GetTaggedValues(RoseItem, (UMLAttribute as TUMLModelElement), Tools);
    TVPersistence := taggedValue[ BOLDSTDUMLTOOLNAME + '.' + TAG_PERSISTENCE ];
    if not assigned(TVPersistence) then
      TVPersistence := taggedValue[ BOLDSTDUMLTOOLNAME + '.' + 'Persistence' ];
    if assigned(TVPersistence) then
      TVPersistence.tag := TAG_PERSISTENCE;
    TVPersistence := taggedValue[ BOLDSTDUMLTOOLNAME + '.' + 'Persistence' ];
    if assigned(TVPersistence) then
      TVPersistence.Delete;

    TVPersistence := taggedValue[BOLDSTDUMLTOOLNAME + '.' + 'PersistenceSet'];
    if assigned(TVPersistence) then
      TVPersistence.Delete;

    ImportConstraints(RoseItem, UMLAttribute);
    Derived := RoseAttribute.Derived;
    type_ := GetEnsuredUMLDataType(RoseAttribute.Type_);
    StereotypeName := RoseAttribute.Stereotype;
    InitialValue := RoseAttribute.InitValue;
    Visibility := TBoldUMLRose98Support.RoseExportControlToVisibility(RoseAttribute.ExportControl);
    if UMLAttribute.Persistent and UMLAttribute.Derived then
      UMLAttribute.Persistent := false;
  end;
end;

procedure TBoldUMLRoseLink.ImportClass(RoseClass: IRoseClass; UMLClass: TUMLClass; pass: TRoseLinkPass);
var
  I: Integer;
  RoseItem: IRoseItem;
  RoseAttribute: IRoseAttribute;
  RoseOperation: IRoseOperation;
  TableMappingString: string;
  UMLAttribute: TUMLAttribute;
  UMLOperation: TUMLOperation;
begin
  RoseItem := RoseClass as IRoseItem;
  with UMLClass do
  begin
    if pass = pass1 then
    begin
      Name := RoseItem.Name;
      BoldLog.Log(Format('Class: %s', [qualifiedName]));
      TBoldUMLSupport.AddToolId(UMLClass, RoseClass.getUniqueId);

      RoseProp.GetTaggedValues(RoseItem, UMLClass, Tools);
      StereotypeName := RoseItem.Stereotype;
      ImportConstraints(RoseItem, UMLClass);

      isAbstract := RoseClass.Abstract;
      if (MakeClassesPersistent) then
        Persistent := True
      else
        Persistent := RoseClass.Persistence;

      TableMappingString := RoseProp.GetString(RoseItem, TAG_TABLEMAPPING, TV_TABLEMAPPING_OWN);

      if (TableMappingString <> TV_TABLEMAPPING_OWN) and
         (TableMappingString <> TV_TABLEMAPPING_PARENT) and
         (TableMappingString <> TV_TABLEMAPPING_CHILDREN) and
         (TableMappingString <> TV_TABLEMAPPING_IMPORTED) then
      begin
        TableMappingString := TV_TABLEMAPPING_OWN;
        BoldLog.LogFmt('Warning: Unknown table mapping %s, ''OWN'' assumed', [TableMappingString], ltWarning);
      end;
      SetBoldTV(TAG_TABLEMAPPING, TableMappingString);

      for I := 1 to RoseClass.Attributes.Count do
      begin
        RoseAttribute := RoseClass.Attributes.GetAt(i);
        UMLAttribute := TUMLAttribute.Create(UMLClass.BoldSystem);
        UMLAttribute.owner := UMLClass;
        ImportAttribute(RoseAttribute, UMLAttribute);
      end;

      for I := 1 to RoseClass.Operations.Count do
      begin
        RoseOperation:= RoseClass.Operations.GetAt(i);
        UMLOperation := TUMLOperation.Create(UMLClass.BoldSystem);
        UMLOperation.owner := UMLClass;
        ImportOperation(RoseOperation, UMLOperation);
      end;
     end
     else
      case RoseClass.GetSuperClasses.Count of
      0:
        ;
      1:
        SetFirstParent(fMapping.FindClass(RoseClass.GetSuperClasses.GetAt(1).GetUniqueID));
      else
        raise EBoldImport.CreateFmt('Class %s has multiple super classes', [Name]);
      end;
   end;
end;

procedure TBoldUMLRoseLink.ImportOperation(RoseOperation: IRoseOperation; UMLOperation: TUMLOperation);
var
  RoseItem: IRoseItem;
  NewParameter: TUMLParameter;
begin
  RoseItem := RoseOperation as IRoseItem;

  with UMLOperation do
  begin
    Name := RoseOperation.Name;
    TBoldUMLSupport.AddToolId(UMLOperation, RoseOperation.getUniqueId);
    RoseProp.GetTaggedValues(RoseItem, (UMLOperation as TUMLModelElement), Tools);
    StereotypeName := RoseOperation.Stereotype;
    Visibility := TBoldUMLRose98Support.RoseExportControlToVisibility(RoseOperation.ExportControl);
    ImportConstraints(RoseItem, UMLOperation);

    if RoseProp.GetBoolean(RoseItem, TAG_ISCLASSMETHOD, False) then
      UMLOperation.ownerScope := skClassifier
    else
      UMLOperation.ownerScope := skInstance;

    ImportSignature(RoseOperation, UMLOperation);

    if RoseOperation.ReturnType <> '' then
    begin
      NewParameter := UMLOperation.Parameter.AddNew;
      TBoldUMLSupport.EnsureBoldTaggedValues(NewParameter);
      with NewParameter do
      begin
        name := 'return';
        kind := pdReturn;
        SetBoldTV(TAG_EXPRESSIONNAME, RoseProp.GetString(RoseItem, TAG_EXPRESSIONNAME, TV_NAME));
        type_ := GetEnsuredUMLDataType(RoseOperation.ReturnType);
      end;
    end;
  end;
end;

procedure TBoldUMLRoseLink.ImportSignature(RoseOperation: IRoseOperation; UMLOperation: TUMLOperation);
var
    Index: Integer;
    RoseParams: IRoseParameterCollection;
    UMLParameter: TUMLParameter;
    ParamName: String;
    RoseParam: IRoseParameter;
begin
  RoseParams := RoseOperation.Parameters;
  for Index := 1 to RoseParams.Count do
  begin
    RoseParam := RoseParams.GetAt(Index) as IRoseParameter;
    UMLParameter := UMLOperation.parameter.AddNew;
    UMLParameter.name := RoseParam.Name;

    RoseProp.GetTaggedValues(RoseParam as IRoseItem, (UMLParameter as TUMLModelElement), Tools);

    TBoldUMLSupport.AddToolId(UMLParameter, RoseParam.getUniqueId);
    UMLParameter.StereotypeName := RoseParam.Stereotype;
    UMLParameter.type_ := GetEnsuredUMLDataType(RoseParam.Type_);
    ParamName := UMLParameter.Name;
    if Pos(UpperCase('var '), UpperCase(ParamName)) > 0 then
    begin
      UMLParameter.SetBoldTV(TAG_ISCONST, TV_FALSE);
      UMLParameter.kind := pdInOut;
      Delete(ParamName, Pos(UpperCase('var '), UpperCase(ParamName)), Length('var '));
      UMLParameter.Name := ParamName;
    end
    else if Pos(UpperCase('out '), UpperCase(ParamName)) > 0 then
    begin
      UMLParameter.SetBoldTV(TAG_ISCONST, TV_FALSE);
      UMLParameter.kind := pdOut;
      Delete(ParamName, Pos(UpperCase('out '), UpperCase(ParamName)), Length('out '));
      UMLParameter.Name := ParamName;
    end
    else if Pos(UpperCase('const '), UpperCase(ParamName)) > 0 then
    begin
      UMLParameter.SetBoldTV(TAG_ISCONST, TV_TRUE);
      Delete(ParamName, Pos(UpperCase('const '), UpperCase(ParamName)), Length('const '));
      UMLParameter.Name := ParamName;
    end;
  end;
end;

function TBoldUMLRoseLink.ImportModel(UMLModel: TUMLModel): Boolean;
procedure SplitVersion(Version: string; var Major, Minor: string);
var
  dotPos: integer;
begin
  dotPos := Pos('.', Version);
  if dotPos > 0 then
  begin
    Major := Copy(Version, 1, DotPos - 1);
    Minor := Copy(Version, DotPos + 1, maxint);
  end
  else
  begin
    Major := Version;
    Minor := '0';
  end;
end;

var
  RoseItem: IRoseItem;
  ModelPTYVersionMajor,
  ModelPTYVersionMinor,
  ModelPTYVersion: string;
begin
  Result := False;
  FreeAndNil(fCachedPackages);
  FreeAndNil(fCachedTypes);
  if not Assigned(UMLModel) then
    raise EBoldImport.CreateFmt('%s.ImportModel: Must have an UMLModel to import to.', [className]);
  fUMLModel := UMLModel;
  fMapping.UMLModel := UMLModel;
  TBoldQueueable.DeActivateDisplayQueue;

  try
    UMLModel.BoldSystem.StartTransaction;
    try
      BoldLog.StartLog('Importing model from: ' + FileName);
      EnsureModel;
      RoseProp.ClearEffectiveDefaults;
      ModelPTYVersion := RoseModel.GetPropertyValue(BOLDTOOLNAME, TAG_PTY_VERSION);
      SplitVersion(ModelPTYVersion, ModelPTYVersionMajor, ModelPTYVersionMinor);

      if ModelPTYVersionMajor <> BOLDTVREV_MAJOR then
        raise EBold.CreateFmt('You must update the model properties of your model with a new BfD.pty' + BOLDCRLF + BOLDCRLF +
          'Please read the online help (Tasks->Updating Rose PTY-files) for further instructions ' + BOLDCRLF + BOLDCRLF +
          'Your version: %s' + BOLDCRLF +
          'Current version: %s', [ModelPTYVersion, BOLDTVREV]);

      if ModelPTYVersionMinor <> BOLDTVREV_MINOR then
        BoldLog.LogFmt('There has been changes in the BfD.PTY file that you cannot use with the PTY-file you have.' + BOLDCRLF + BOLDCRLF +
          'Please read the online help (Tasks->Updating Rose PTY-files) for further instructions ' + BOLDCRLF + BOLDCRLF +
          'Your version: %s' + BOLDCRLF +
          'Current version: %s', [ModelPTYVersion, BOLDTVREV]);

      UMLModel.Clear;
      TBoldUMLSupport.EnsureBoldTaggedValues(UMLModel);
      Application.BringToFront;
      RoseItem := RoseModel as IRoseItem;


      Assert(UMLModel.Associations.Count = 0);
      BoldLog.ProgressMax := 2 * RoseModel.GetAllClasses.Count + RoseModel.GetAllAssociations.Count + RoseModel.GetAllCategories.Count;
      BoldLog.Progress := 0;
      RoseProp.GetTaggedValues(RoseItem, UMLModel, Tools);
      ImportPackage(RoseModel.RootCategory, UMLModel, pass1);

      RoseProp.GetTaggedValues(RoseItem, UMLModel, Tools);
      if not BoldLog.ProcessInterruption then
        ImportPackage(RoseModel.RootCategory, UMLModel, pass2);

      if not BoldLog.ProcessInterruption then
        UMLModel.Name := RoseProp.GetString(RoseItem, TAG_MODELNAME, Rose98TaggedValueList.DefaultForClassAndTag['Model', TAG_MODELNAME]);

      Result := True;
      if BoldLog.ProcessInterruption then
      begin
        BoldLog.Log('Import was aborted, old model restored', ltError);
        UMLModel.BoldSystem.RollbackTransaction
      end
      else
        UMLModel.BoldSystem.CommitTransaction;
      BoldLog.EndLog;
    except
      on E: Exception do
      begin
        UMLModel.BoldSystem.RollbackTransaction;
        BoldLog.LogFmt('Error: %s', [E.Message], ltError);
      end;
    end;
  finally
    TBoldQueueable.ActivateDisplayQueue;
    fRoseModel := nil;
  end;
end;

procedure TBoldUMLRoseLink.ImportRole(RoseRole: IRoseRole; OtherRole: IRoseRole; UMLAssociationEnd: TUMLAssociationEnd);
var
  RoseItem: IRoseItem;
  i : Integer;
begin
  RoseItem := RoseRole as IRoseItem;
  with UMLAssociationEnd{.GetOtherEnd} do
  begin
    Name := RoseRole.Name;
    TBoldUMLSupport.AddToolId(UMLAssociationEnd, RoseRole.getUniqueId);
    RoseProp.GetTaggedValues(RoseItem, (UMLAssociationEnd as TUMLModelElement), Tools);
    if assigned(RoseRole.Class_) then
      Type_ :=  fMapping.FindClass(RoseRole.Class_.GetUniqueID);
    isNavigable := RoseRole.Navigable;
    isOrdered := RoseProp.GetBoolean(RoseItem, TAG_ORDERED, False);
    ImportConstraints(RoseItem, UMLAssociationEnd);

    Aggregation := TBoldUMLRose98Support.GetContainment(RoseRole, OtherRole);
    Visibility := TBoldUMLRose98Support.RoseExportControlToVisibility(RoseRole.ExportControl);
    Changeability := TBoldRose98TaggedValueSupport.StringToChangeableKind(RoseProp.GetString(RoseItem, TAG_CHANGEABILITY, Rose98TaggedValueList.DefaultForClassAndTag['AssociationEnd', TAG_CHANGEABILITY]));
    Multiplicity := RoseRole.Cardinality;
    if AdjustEmbedFlag and (UMLAssociationEnd.Multi or assigned(UMLAssociationEnd.Association.Class_)) then
      UMLAssociationEnd.SetBoldTV(TAG_EMBED, TV_FALSE);

    with RoseRole.Keys do
      for i := 1 to Count do ImportQualifier(GetAt(I), UMLAssociationEnd);
  end;
end;

procedure TBoldUMLRoseLink.ImportQualifier(RoseAttribute: IRoseAttribute; UMLAssociationEnd: TUMLAssociationEnd);
var
  RoseItem: IRoseItem;
  UMLQualifier: TUMLAttribute;
begin
  RoseItem := RoseAttribute as IRoseItem;
  UMLQualifier := UMLAssociationEnd.qualifier.AddNew;
  UMLQualifier.Name := RoseAttribute.Name;
  TBoldUMLSupport.AddToolId(UMLQualifier, RoseAttribute.getUniqueId);
  ImportConstraints(RoseItem, UMLQualifier);
  RoseProp.GetTaggedValues(RoseItem, UMLQualifier, Tools);
  UMLQualifier.type_ := GetEnsuredUMLDataType(RoseAttribute.Type_);
end;

procedure TBoldUMLRoseLink.EnsureandExportAssociations(TheModel: TUMLModel);
var
  RoseAssociation: IRoseAssociation;
  i : Integer;
  UMLAssociation : TUMLAssociation;
begin
  for i := 0 to UMLModel.associations.Count - 1 do
  begin
    UMLAssociation := UMLModel.Associations[i];
    RoseAssociation := fMapping.RoseAssociationForUMLAssociation(RoseModel, UMLAssociation);
    if not Assigned(RoseASsociation) then
    begin
      BoldLog.LogFmt('Adding association %s', [UMLAssociation.Name]);
      RoseAssociation := fMapping.RoseClassForUMLClassifier(RoseModel, UMLAssociation.Connection[1].Type_)
        .AddAssociation(UMLAssociation.Connection[0].Name, UMLAssociation.Connection[0].Type_.Name);
      if GetToolIdOnExport then
        TBoldUMLSupport.AddToolId(UMLAssociation, RoseAssociation.GetUniqueID);
    end;
    ExportAssociation(UMLASsociation, RoseAssociation);
    BoldLog.ProgressStep;
  end;
end;

procedure TBoldUMLRoseLink.ExportAssociation(UMLAssociation: TUMLAssociation; RoseAssociation : IRoseAssociation);
var
  RoseRole1: IRoseRole;
  RoseRole2: IRoseRole;
  RoseItem: IRoseItem;
  LogName: String;
  LinkClass: IRoseClass;
  RoseLinkClassName: String;
  UMLAssoEnd1, UMLAssoEnd2: TUMLAssociationEnd;
  function RolesOK(UMLAssociationEnd: TUMLAssociationEnd; RoseRole: IRoseRole): Boolean;
  begin
    Result := (RoseRole.Class_.Name = UMLAssociationEnd.Type_.Name)
  end;

begin
  if GetToolIdOnExport then
    TBoldUMLSupport.AddToolId(UMLAssociation, RoseAssociation.getUniqueId);
  LogName := TBoldUMLRose98Properties.LogName(UMLAssociation);

  UMLAssoEnd1 := UMLAssociation.Connection[0];
  UMLAssoEnd2 := UMLAssociation.Connection[1];

  RoseRole1 := RoseAssociation.Role1;
  RoseRole2 := RoseAssociation.Role2;
  if RolesOK(UMLAssoEnd1, RoseRole1) then
  begin
    ExportRole(UMLAssoEnd1, RoseRole1, RoseRole2);
    ExportRole(UMLAssoEnd2, RoseRole2, RoseRole1);
  end
  else
  begin
    ExportRole(UMLAssoEnd1, RoseRole2, RoseRole1);
    ExportRole(UMLAssoEnd2, RoseRole1, RoseRole2);
  end;

  if CompareText(RoseAssociation.Name, UMLAssociation.Name) <> 0 then
  begin
    BoldLog.LogFmt('Setting association name %s to %s', [RoseAssociation.Name, LogName]);
    RoseAssociation.Name := UMLAssociation.Name;
  end;

  RoseItem := RoseAssociation as IRoseItem;
  if Assigned(UMLAssociation.Class_) then
  begin
    LinkClass := fMapping.RoseClassForUMLClassifier(RoseModel, UMLAssociation.Class_);
    if not Assigned(LinkClass) then
    begin
      RoseLinkClassName := RoseProp.GetString(RoseItem, TAG_LINKCLASSNAME, TV_NAME);
      if CompareText(BoldExpandName(RoseLinkClassName, RoseAssociation.Name, xtDelphi, -1,
                TBoldTaggedValueSupport.StringToNationalCharConversion(UMLAssociation.model.GetBoldTV(TAG_NATIONALCHARCONVERSION))), UMLAssociation.Class_.Name) <> 0 then
        RoseProp.SetString(RoseItem, 'LinkClassName', TV_NAME, UMLAssociation.Class_.Name, LogName);
    end
    else
      RoseAssociation.LinkClass := LinkClass;
  end;
  RoseProp.SetTaggedValues(RoseItem, UMLAssociation, Tools);
  RoseItem.Stereotype := UMLAssociation.StereotypeName;
  RoseAssociation.Derived := UMLAssociation.Derived;
end;

procedure TBoldUMLRoseLink.ExportAttribute(UMLAttribute: TUMLAttribute; RoseClass: IRoseClass);
var
  RoseItem: IRoseItem;
  RoseAttribute: IRoseAttribute;
begin
  RoseAttribute := fMapping.RoseAttributeForUMLAttribute(RoseClass, UMLAttribute);
  if not Assigned(RoseAttribute) then
  begin
    BoldLog.LogFmt('Adding Attribute %s', [UMLAttribute.qualifiedName]);
    RoseAttribute := RoseClass.AddAttribute(UMLAttribute.Name, UMLAttribute.typeName, '');
  end;

  if GetToolIdOnExport then
    TBoldUMLSupport.AddToolId(UMLAttribute, RoseAttribute.getUniqueId);

 RoseItem := RoseAttribute as IRoseItem;
  with UMLAttribute do
  begin
    if RoseAttribute.Name <> name then
    begin
      BoldLog.LogFmt('Changing name of Attribute %s to %s', [RoseAttribute.Name, UMLAttribute.qualifiedName]);
      RoseAttribute.Name := name;
    end;
    if RoseAttribute.Stereotype <> StereotypeName then
    begin
      BoldLog.Log(Format('Setting %s.Stereotype to %s', [UMLAttribute.qualifiedName,StereotypeName]));
      RoseAttribute.Stereotype := StereotypeName;
    end;
    TBoldUMLRose98Support.SetExportControl(visibility, RoseAttribute.ExportControl, UMLAttribute.qualifiedName);
    ExportConstraints(RoseItem, UMLAttribute);
    if RoseAttribute.InitValue <> InitialValue then
    begin
      BoldLog.Log(Format('Setting %s.InitialValue to %s', [UMLAttribute.InitialValue,InitialValue]));
      RoseAttribute.InitValue := InitialValue;
    end;
    RoseProp.SetTaggedValues(RoseItem, UMLAttribute, Tools);
    if Derived <> RoseAttribute.Derived then
    begin
      BoldLog.Log(Format('Setting %s.Derived to %s', [UMLAttribute.qualifiedName,BooleanToString(Derived)]));
      RoseAttribute.Derived := Derived;
    end;
    if CompareText(typeName, RoseAttribute.Type_) <> 0 then
    begin
      BoldLog.Log(Format('Setting %s.Type to %s', [UMLAttribute.qualifiedName, typeName]));
      RoseAttribute.Type_ := typeName;
    end;
  end;
end;

procedure TBoldUMLRoseLink.ExportClass(UMLClass: TUMLClass);
var
  RoseClass: IRoseClass;
  RoseItem: IRoseItem;
  RoseAttribute: IRoseAttribute;
  RoseAttributes: IRoseAttributeCollection;
  RoseOperation: IRoseOperation;
  RoseOperations: IRoseOperationCollection;
  LogName: string;
  I: Integer;
begin
  LogName := TBoldUMLRose98Properties.LogName(UMLClass);
  RoseClass := fMapping.RoseClassForUMLClassifier(RoseModel, UMLClass);
  if not Assigned(RoseClass) then
  begin
    BoldLog.LogFmt('Adding class %s', [LogName]);
    RoseClass := DefaultLogicalPackage.AddClass(UMLClass.Name);
    if not Assigned(RoseClass) then
    begin
      BoldLog.LogFmt('Failed to add class %s', [LogName], ltError);
      Exit;
    end;
  end;
  if GetToolIdOnExport then
    TBoldUMLSupport.AddToolId(UMLClass, RoseClass.getUniqueId);
  RoseItem := RoseClass as IRoseItem;
  with UMLClass do
  begin
    RoseProp.SetTaggedValues(RoseItem, UMLClass, Tools);
    ExportConstraints(RoseItem, UMLClass);
    if RoseClass.Stereotype <> StereotypeName then
    begin
      BoldLog.LogFmt('Setting %s.Stereotype to %s', [LogName, StereotypeName]);
      RoseClass.Stereotype := StereotypeName;
    end;
    if RoseClass.Name <> name then
    begin
      BoldLog.LogFmt('Changing name of Class %s to %s', [RoseClass.Name, name]);
      RoseClass.Name := name;
    end;
    if IsAbstract <> RoseClass.Abstract then
    begin
      BoldLog.LogFmt('Setting %s.Abstract to %s', [LogName, BooleanToString(IsAbstract)]);
      RoseClass.Abstract := IsAbstract;
    end;
    if Persistent <> (MakeClassesPersistent or RoseClass.Persistence) then
    begin
      BoldLog.LogFmt('Setting %s.Persistence to %s', [LogName, BooleanToString(Persistent)]);
      RoseClass.Persistence := Persistent;
    end;
    RoseAttributes := RoseClass.Attributes;
    I := 1;
    while I <= RoseAttributes.Count do
    begin
      RoseAttribute := RoseAttributes.GetAt(I);
      if Assigned(fMapping.UMLAttributeForRoseAttribute(UMLClass, RoseAttribute)) then
        Inc(I)
      else
      begin
        BoldLog.LogFmt('Deleting attribute %s.%s', [LogName, RoseAttribute.Name]);
        RoseClass.DeleteAttribute(RoseAttribute);
        RoseAttributes := RoseClass.Attributes;
        I := 1;
      end;
    end;
    RoseOperations := RoseClass.Operations;
    I := 1;
    while I <= RoseOperations.Count do
    begin
      RoseOperation := RoseOperations.GetAt(I);
      if Assigned(fMapping.UMLOperationForRoseOperation(UMLClass, RoseOperation)) then
        Inc(I)
      else
      begin
        BoldLog.LogFmt('Deleting operation %s.%s', [LogName, RoseOperation.Name]);
        RoseClass.DeleteOperation(RoseOperation);
        RoseOperations := RoseClass.Operations;
        I := 1;
      end;
    end;
    for I := 0 to Feature.Count - 1 do
    begin
      if Feature[I] is TUMLAttribute then
        ExportAttribute(Feature[I] as TUMLAttribute, RoseClass)
      else if Feature[I] is TUMLOperation then
        ExportOperation(Feature[I] as TUMLOperation, RoseClass);
    end;
  end;
end;

procedure TBoldUMLRoseLink.ExportOperation(UMLOperation: TUMLOperation; RoseClass: IRoseClass);
var
  RoseItem: IRoseItem;
  RoseOperation: IRoseOperation;
  LogName, ReturnTypeName: string;
  UMLReturnType: TUMLClassifier;
begin
  LogName := TBoldUMLRose98Properties.LogName(UMLOperation);
  UMLReturnType := UMLOperation.EvaluateExpressionAsDirectElement('parameter->select(kind=#return)->first.type->first') as TUMLClassifier;
  if ASsigned(UMLReturnType) then
    ReturnTypeName := UMLReturnType.name
  else
    ReturnTypeName := '';
  RoseOperation := fMapping.RoseOperationForUMLOperation(RoseClass, UMLOperation);
  if not Assigned(RoseOperation) then
  begin
    BoldLog.LogFmt('Adding Operation %s', [LogName]);
    RoseOperation := RoseClass.Addoperation(UMLOperation.Name, ReturnTypeName);
  end;
  if GetToolIdOnExport then
    TBoldUMLSupport.AddToolId(UMLOperation, RoseOperation.getUniqueId);

  RoseItem := RoseOperation as IRoseItem;
  if RoseOperation.Name <> UMLOperation.name then
  begin
    BoldLog.LogFmt('Changing name of Operation %s to %s', [RoseOperation.name, UMLOperation.qualifiedName]);
    RoseOperation.Name := UMLOperation.name;
  end;
  RoseItem.Stereotype := UMLOperation.StereotypeName;
  ExportConstraints(RoseItem, UMLOperation);
  TBoldUMLRose98Support.SetExportControl(UMLOperation.Visibility, RoseOperation.ExportControl, UMLOperation.qualifiedName);
  RoseProp.SetTaggedValues(RoseItem, UMLOperation, Tools);
  RoseProp.SetBoolean(RoseItem, 'IsClassMethod', False, UMLOperation.ownerScope = skClassifier, LogName);
   if CompareText(RoseOperation.ReturnType, ReturnTypeName) <> 0 then
  begin
    BoldLog.LogFmt('Setting %s.Type to %s', [LogName, ReturnTypeName]);
    RoseOperation.ReturnType := ReturnTypeName;
  end;
  ExportSignature(UMLOperation, RoseOperation);
end;

function TBoldUMLRoseLink.ExportModel(UMLModel: TUMLModel): Boolean;

var
  RoseItem: IRoseItem;
  RoseClasses: IRoseClassCollection;
  RoseClass: IRoseClass;
  RoseAssociations: IRoseAssociationCollection;
  RoseAssociation: IRoseAssociation;
  I: Integer;
  ModelPTYVersion: string;
begin
  Result := False;
  fUMLModel := UMLModel;
  fMapping.UMLModel := UMLModel;
  fMapping.ClearCaches;
  try
    try
      if not assigned(UMLModel) then
        raise EBoldInternal.Create('Can not export model, no UMLModel object provided');
      BoldLog.StartLog('Exporting model to: ' + FileName);
      EnsureModel;
      RoseProp.ClearEffectiveDefaults;
      Application.BringToFront;
      RoseItem := RoseModel as IRoseItem;
      ModelPTYVersion := RoseModel.GetPropertyValue(BOLDTOOLNAME, TAG_PTY_VERSION);

      if ModelPTYVersion <> BOLDTVREV then
        raise EBold.CreateFmt('You must update the model properties of the target model with a new BfD.pty' + BOLDCRLF + BOLDCRLF +
          'Please read the online help (Tasks->Updating Rose PTY-files) for further instructions ' + BOLDCRLF + BOLDCRLF +
          'Your version: %s' + BOLDCRLF +
          'Current version: %s', [ModelPTYVersion, BOLDTVREV]);

      if GetToolIdOnExport and (TBoldRose98Support.GetVersion > 4.5 {Rose98 does not support getting the ID for the model})then
        TBoldUMLSupport.AddToolId(UMLModel, RoseModel.getUniqueId);

      RoseProp.SetTaggedValuesAsDefaultProps(RoseItem, fUMLModel, Tools);

      if RoseModel.Stereotype <> UMLModel.StereotypeName then
      begin
        RoseModel.Stereotype := UMLModel.StereotypeName;
      end;
      ExportConstraints(RoseItem, UMLModel);
      RoseClasses := RoseModel.GetAllClasses;
      I := 1;
      BoldLog.ProgressMax := RoseClasses.Count - 1;
      BoldLog.LogHeader := '(1/5) Removing classes';
      BoldLog.Progress := 0;

      while I <= RoseClasses.Count do
      begin
        RoseClass := RoseClasses.GetAt(I);
        if fMapping.ClassInLogicalPackages(RoseClass) then
        begin
          if Assigned(fMapping.UMLClassifierForRoseClass(UMLModel,RoseClass)) then
          begin
            Inc(I);
            BoldLog.Progress := i;
          end
          else
          begin
            BoldLog.LogFmt('Deleting class %s', [RoseClass.Name]);
            RoseAssociations := RoseClass.GetAssociations;
            while roseAssociations.count > 0 do
            begin
              RoseClass.DeleteAssociation(RoseAssociations.GetAt(1));
              RoseAssociations := RoseClass.GetAssociations;
            end;
            RoseClass.ParentCategory.DeleteClass(RoseClass);
            RoseClasses := RoseModel.GetAllClasses;
            BoldLog.Progress := i;
            I := 1;
          end;
        end
        else
          inc(i);
      end;
      RoseAssociations := RoseModel.GetAllAssociations;
      I := 1;
      BoldLog.ProgressMax := RoseAssociations.Count - 1;
      BoldLog.LogHeader := '(2/5) Removing associations';
      BoldLog.Progress := 0;

      while I <= RoseAssociations.Count do
      begin
        RoseAssociation := RoseAssociations.GetAt(I);
        if fMapping.AssociationInLogicalPackages(RoseAssociation) then
        begin
          if Assigned(fMapping.UMLAssociationForRoseASsociation(UMLModel, RoseAssociation)) then
          begin
            Inc(I);
            BoldLog.Progress := i;
          end
          else
          begin
            BoldLog.LogFmt('Removing association: %s', [RoseAssociation.Name]);
            RoseClass := RoseAssociation.Role1.Class_;
            if not Assigned(RoseClass) then
              RoseClass := RoseAssociation.Role2.Class_;
            if Assigned(RoseClass) then
              RoseClass.DeleteAssociation(RoseAssociation);
            RoseAssociations := RoseModel.GetAllAssociations;
            BoldLog.Progress := i;
            I := 1;
          end;
        end
        else
          inc(i);
      end;

      with UMLModel do
      begin
        BoldLog.ProgressMax := Classes.Count - 1;
        BoldLog.LogHeader := '(3/5) Adding classes';
        for I := 0 to Classes.Count - 1 do
        begin
          BoldLog.Progress := i;
          ExportClass(Classes[I]);
        end;
        BoldLog.LogHeader := '(4/5) Adding superclasses';
        for I := 0 to Classes.Count - 1 do
        begin
          BoldLog.Progress := i;
          ExportInheritance(Classes[I]);
        end;
        BoldLog.ProgressMax := Associations.Count - 1;
        BoldLog.LogHeader := '(5/5) Adding associations';
        EnsureandExportAssociations(UMLModel);
        BoldLog.Progress := 0;
      end;

      BoldLog.EndLog;

      Result := True;

    except
      on E: Exception do BoldLog.LogFmt('Error: %s', [E.Message], ltError);
    end;
  finally
    fRoseModel := nil;
  end;  
end;

procedure TBoldUMLRoseLink.ExportSignature(UMLOperation: TUMLOperation; RoseOperation: IRoseOperation);
var
  RoseParameter: IRoseParameter;
  RoseParameters: IRoseParameterCollection;
  UMLParameter: TUMLParameter;
  FullDelphiName: string;
  Index: Integer;
  LogName: string;
begin
  LogName := Format('%s.%s', [UMLOperation.Owner.Name, UMLOperation.Name]);

  RoseParameters := RoseOperation.Parameters;
  for Index := 0 to UMLOperation.parameter.Count - 1 do
  begin
    UMLParameter := UMLOperation.parameter[Index];
    if UMLParameter.kind <> pdReturn then
    begin
      FullDelphiName :=
        TBoldUMLDelphiSupport.ParameterModifier(UMLParameter.kind, TVIsTrue(UMLParameter.GetBoldTV(TAG_ISCONST)))
        + UMLParameter.Name;
      if Index >= RoseParameters.Count then
      begin
        BoldLog.LogFmt('Adding parameter %s to %s', [FullDelphiName, LogName]);
        RoseParameter := RoseOperation.AddParameter(FullDelphiName, UMLParameter.typeName, '', Index);
        RoseParameters := RoseOperation.Parameters;
      end
      else
        RoseParameter := RoseParameters.GetAt(Index + 1) as IRoseParameter;
      if Assigned(RoseParameter) then
      begin
        if GetToolIdOnExport then
          TBoldUMLSupport.AddToolId(UMLParameter, RoseParameter.getUniqueId);
        if CompareText(FullDelphiName, RoseParameter.Name) <> 0 then
        begin
          BoldLog.LogFmt('Setting parameter name %s of %s to %s', [RoseParameter.Name, LogName, FullDelphiName]);
          RoseParameter.Name := FullDelphiName;
        end;
        if CompareText(UMLParameter.typeName, RoseParameter.Type_) <> 0 then
        begin
          BoldLog.LogFmt('Setting parameter type %s of %s to %s', [RoseParameter.Name, LogName, UMLParameter.typeName]);
          RoseParameter.Type_ := UMLParameter.typeName;
        end;
      end;
    end;
  end;
  if Assigned(RoseParameter) then
  begin
    while RoseParameters.Count > UMLOperation.parameter.Count do
    begin
      RoseParameter := RoseParameters.GetAt(RoseParameters.Count);
      BoldLog.LogFmt('Deleting parameter %s from %s', [RoseParameter.Name, LogName]);
      RoseOperation.DeleteParameter(RoseParameter);
      RoseParameters := RoseOperation.Parameters;
    end;
  end;
end;

procedure TBoldUMLRoseLink.ExportRole(UMLAssociationEnd: TUMLAssociationEnd; RoseRole: IRoseRole; OtherRoseRole: IRoseRole);
var
  RoseItem: IRoseItem;
  LogName{, RoseName}: string;
  RoseAttribute: IRoseAttribute;
  RoseAttributes: IRoseAttributeCollection;
  i : Integer;
begin
  LogName := TBoldUMLRose98Properties.LogName(UMLAssociationEnd);
  RoseItem := RoseRole as IRoseItem;
  if GetToolIdOnExport then
    TBoldUMLSupport.AddToolId(UMLAssociationEnd, RoseRole.getUniqueId);

  if RoseRole.Class_.Name <> UMLAssociationEnd.Type_.Name then
  BoldLog.LogFmt('Warning! Unable to move associationend "%s" in association "%s". Should point to class %s. Please fix manually', [
    RoseRole.Name, UMLAssociationEnd.Association.Name, UMLAssociationEnd.Type_.Name], ltWarning);

  if UMLAssociationEnd.name <> RoseRole.Name then
  begin
    BoldLog.LogFmt('Setting role name %s to %s', [LogName, UMLAssociationEnd.name]);
    RoseRole.Name := UMLAssociationEnd.Name;
  end;

  if (RoseItem.Stereotype <> UMLAssociationEnd.StereotypeName) then
  begin
    BoldLog.LogFmt('Setting %s.stereotype to %s', [LogName, UMLAssociationEnd.StereotypeName]);
    RoseItem.Stereotype := UMLAssociationEnd.StereotypeName;
  end;

  if (RoseRole.Cardinality <> UMLAssociationEnd.Multiplicity) then
  begin
    BoldLog.LogFmt('Setting %s.Cardinality to %s', [LogName, UMLAssociationEnd.Multiplicity]);
    RoseRole.Cardinality := UMLAssociationEnd.Multiplicity;
  end;

  ExportConstraints(RoseItem, UMLAssociationEnd);
  TBoldUMLRose98Support.SetContainment(UMLAssociationEnd.Aggregation, RoseRole, OtherRoseRole, UMLAssociationEnd.qualifiedName);
  TBoldUMLRose98Support.SetExportControl(UMLAssociationEnd.visibility, RoseRole.ExportControl, UMLAssociationEnd.qualifiedName);

  RoseProp.SetString(RoseItem, 'Changeability', 'Changeable', TBoldRose98TaggedValueSupport.ChangeableKindToString(UMLAssociationEnd.Changeability), LogName);
  RoseProp.SetTaggedValues(RoseItem, UMLAssociationEnd, Tools);
  RoseProp.SetBoolean(RoseItem, TAG_ORDERED, False, UMLAssociationEnd.isOrdered, LogName);

  if UMLAssociationEnd.isNavigable <> RoseRole.Navigable then
  begin
    BoldLog.LogFmt('Setting %s.Navigable to %s', [LogName, BooleanToString(UMLAssociationEnd.isNavigable)]);
    RoseRole.Navigable := UMLAssociationEnd.isNavigable;
  end;
  RoseAttributes := RoseRole.Keys;
  I := 1;
  while I <= RoseAttributes.Count do
  begin
    RoseAttribute := RoseAttributes.GetAt(I);
    if Assigned(UMLAssociationEnd.GetQualifierByName(RoseAttribute.Name)) then
      Inc(I)
    else
    begin
      BoldLog.LogFmt('Deleting qualifier %s.%s', [LogName, RoseAttribute.Name]);
      RoseRole.DeleteKey(RoseAttribute);
      RoseAttributes := RoseRole.Keys;
      I := 1;
    end;
  end;
  for I := 0 to UMLAssociationEnd.Qualifier.Count - 1 do
    ExportQualifier(UMLAssociationEnd.Qualifier[I], RoseRole);
end;

procedure TBoldUMLRoseLink.ExportQualifier(UMLQualifier: TUMLAttribute; RoseRole: IRoseRole);
var
  Index: Integer;
  RoseItem: IRoseItem;
  RoseAttribute: IRoseAttribute;
  LogName: string;
begin
  LogName := Format('%s.%s', [TBoldUMLRose98Properties.LogName(UMLQualifier.AssociationEnd), UMLQualifier.Name]);
  Index := RoseRole.Keys.FindFirst(UMLQualifier.Name);
  if Index = 0 then
  begin
    BoldLog.Log(Format('Adding qualifier %s', [LogName]));
    RoseAttribute := RoseRole.AddKey(UMLQualifier.Name, UMLQualifier.typeName);
  end
  else
    RoseAttribute := RoseRole.Keys.GetAt(Index);

  RoseItem := RoseAttribute as IRoseItem;

  RoseItem.Stereotype := UMLQualifier.StereotypeName;
  RoseProp.SetTaggedValues(RoseItem, UMLQualifier, Tools);
  if GetToolIdOnExport then
      TBoldUMLSupport.AddToolId(UMLQualifier, RoseAttribute.getUniqueId);
  if CompareText(UMLQualifier.typeName, RoseAttribute.Type_) <> 0 then
  begin
    BoldLog.Log(Format('Setting %s.Type to %s', [LogName, UMLQualifier.typeName]));
    RoseAttribute.Type_ := UMLQualifier.typeName;
  end;
end;

procedure TBoldUMLRoseLink.ExportInheritance(UMLClass: TUMLClass);
var
  RoseClass: IRoseClass;
  RoseSuper: IRoseClass;
  UMLSuper: TUMLClassifier;
  RoseInheritRelations: IRoseInheritRelationCollection;
  RoseInheritRelation: IRoseInheritRelation;
  I: Integer;

  function UMLInheritsFrom(UMLClass, UMLSuper: TUMLClassifier): Boolean;
  var
    i: integer;
  begin
    Result := false;
    for i := 0 to UMLClass.generalization.Count - 1 do
      if UMLClass.generalization[i].parent = UMLSuper then
        Result := true;
  end;

begin
  RoseClass := fMapping.RoseClassForUMLClassifier(RoseModel, UMLClass);

  RoseInheritRelations := RoseClass.GetInheritRelations;
  I := 1;
  {Delete all in Rose model but not in UML model}
  while I <= RoseInheritRelations.Count do
  begin
    RoseInheritRelation := RoseInheritRelations.GetAt(I);
    RoseSuper := RoseInheritRelation.GetSupplierClass;
    if assigned(RoseSuper) then
    begin
      UMLSuper := fMapping.UMLClassifierForRoseClass(UMLModel, RoseSuper);
      if UMLInheritsFrom(UMLClass, UMLSuper) then
        Inc(I)
      else
      begin
        BoldLog.LogFmt('Deleting superclass %s from %s', [RoseInheritRelation.SupplierName, RoseClass.Name]);
        RoseClass.DeleteInheritRel(RoseInheritRelation);
        RoseInheritRelations := RoseClass.GetInheritRelations;
        I := 1;
      end;
    end
    else
    begin
      BoldLog.LogFmt('Broken inheritance found in class %s: "%s" (No supplier)', [RoseClass.name, RoseInheritRelation.Name]);
      inc(i);
    end;

  end;

  for i := 0 to UMLClass.generalization.Count - 1 do
  begin
    UMLSuper := UMLClass.generalization[i].parent as TUMLClassifier;
    if assigned(UMLSuper) then
    begin
      RoseSuper := fMapping.RoseClassForUMLClassifier(RoseModel, UMLSuper);
      if RoseClass.GetSuperclasses.IndexOf(RoseSuper) = 0 then
      begin
        BoldLog.LogFmt('Adding superclass %s to %s', [RoseSuper.Name, RoseClass.Name]);
        RoseClass.AddInheritRel('', RoseSuper.Name);
      end
    end
    else
      BoldLog.LogFmt('Warning: Class %s has a generalization with missing parent', [UMLClass.name], ltWarning);
  end;
end;

function TBoldUMLRoseLink.GetBoldSystem: TBoldSystem;
begin
  if assigned(UMLModel) then
    result := UMLModel.BoldSystem
  else
    result := nil;
end;

procedure TBoldUMLRoseLink.SetTools(Value: TStrings);
begin
  Tools.Assign(Value);
end;

procedure TBoldUMLRoseLink.ImportConstraints(RoseItem: IRoseItem; UMLElement: TUMLModelElement);
begin
  TBoldUMLModelLinkSupport.StringToConstraints(UMLElement, RoseProp.GetText(RoseItem, TAG_CONSTRAINTS, ''));
end;

procedure TBoldUMLRoseLink.ExportConstraints(RoseItem: IRoseItem; UMLElement: TUMLModelElement);
var
  ConstrString, RoseConstr: String;
begin
  ConstrString := TBoldUMLModelLinkSupport.ConstraintsAsString(UMLElement);
  RoseConstr := RoseProp.GetString(RoseItem, TAG_CONSTRAINTS, '');

  if  (RoseConstr <> '')  and (Copy(RoseConstr, Length(RoseConstr) - 2, MaxInt) <> BOLDCRLF) then
    RoseConstr := RoseConstr + BOLDCRLF;
  if ConstrString <> RoseConstr then
    RoseProp.SetString(RoseItem, TAG_CONSTRAINTS, '', ConstrString, UMLElement.qualifiedName);
end;

function TBoldUMLRoseLink.GetEnsuredUMLDataType(const Name: string): TUMLDataType;
  procedure RefreshCachedTypes;
  var
    i: integer;
  begin
    FreeAndNil(fCachedTypes);
    fCachedTypes := TStringList.Create;
    for i := 0 to UMLModel.AllOwnedElement.count - 1 do
      if UMLModel.AllOwnedElement[i] is TUMLDataType then
        fCachedTypes.AddObject(UMLModel.AllOwnedElement[i].Name, UMLModel.AllOwnedElement[i]);
    fCachedTypes.Sorted := true;
  end;

  function FindTypeInCache(const Name: string): TUMLDataType;
  var
    pos: integer;
  begin
    result := nil;
    if Assigned(fCachedTypes) then
    begin
      pos := fCachedTypes.IndexOf(Name);
      if Pos <> -1 then
        result := fCachedTypes.Objects[pos] as TUMLDataType;
    end;
  end;

begin
  if Name = '' then
  begin
    Result := nil;
    Exit;
  end;
  Result :=  FindTypeInCache(Name);
  if not Assigned(Result) then
  begin
    RefreshCachedTypes;
    Result :=  FindTypeInCache(Name);
  end;
  if not Assigned(Result) then
  begin
    Result := TUMLDataType.Create(BoldSystem);
    fCachedTypes.AddObject(Result.name, Result);
    Result.namespace_ := UMLmodel;
    Result.name := Name;
  end;
end;

procedure TBoldUMLRoseLink.ImportPackage(RosePackage: IRoseCategory;
  UMLPackage: TUMLPackage; pass: TRoseLinkPass);
var
  RoseItem: IRoseItem;
  i: integer;
  RoseClasses: IRoseClassCollection;
  RoseClass: IRoseClass;
  RoseAssociations: IRoseAssociationCollection;
  RoseAssociation: IRoseAssociation;
  RoseSubPackages: IRoseCategoryCollection;
  RoseSubPackage: IRoseCategory;
  UMLAssociation: TUMLAssociation;
  UMLSubPackage: TUMLPackage;
  UMLClass: TUMLClass;
begin

  RoseItem := RosePackage as IRoseItem;
  if pass = pass1 then
    with UMLPackage do
    begin
      Name := RoseItem.Name;
      BoldLog.Log(Format('Package: %s', [qualifiedName]));
      BoldLog.ProgressStep;
      TBoldUMLSupport.AddToolId(UMLPackage, RosePackage.getUniqueId);
      RoseProp.GetTaggedValues(RoseItem, UMLPackage, Tools);
      StereotypeName := RoseItem.Stereotype;
      ImportConstraints(RoseItem, UMLPackage);
    end;

  RoseClasses := RosePackage.Classes;
  RoseAssociations := RosePackage.Associations;

  BoldLog.LogHeader := 'Importing classes from ' + RosePackage.Name;


  for i := 1 to RoseClasses.Count do
  begin

    RoseClass := RoseClasses.GetAt(I);
    if fMapping.ClassInLogicalPackages(RoseClass) then
    begin
      if pass = pass1 then
      begin
        BoldLog.ProgressStep;
        BoldLog.ProgressStep;
        UMLClass := TUMLClass.Create(UMLPackage.BoldSystem);
        UMLClass.namespace_ := UMLPackage;
      end
      else
         UMLClass := fMapping.FindClass(RoseClass.GetUniqueId);
      ImportClass(RoseClass, UMLClass, pass);
    end;
    if BoldLog.ProcessInterruption then
       exit;
    end;

  BoldLog.LogHeader := 'Importing associations from ' + RosePackage.Name;
  if pass = pass2 then
    for I := 1 to RoseAssociations.Count do
    begin
      BoldLog.ProgressStep;
      RoseAssociation := RoseAssociations.GetAt(I);
      if fMapping.AssociationInLogicalPackages(RoseAssociation) then
      begin
        UMLAssociation := TUMLAssociation.Create(UMLModel.BoldSystem);
        UMLAssociation.namespace_ := UMLPackage;
        ImportAssociation(RoseAssociation, UMLAssociation);
      end;
    if BoldLog.ProcessInterruption then
      exit;
    end;

  RoseSubPackages := RosePackage.Categories;

  for i := 1 to RoseSubPackages.Count do
  begin
    RoseSubPackage := RoseSubPackages.GetAt(I);
    if pass = pass1 then
    begin
      UMLSubPackage := TUMLPackage.Create(UMLPackage.BoldSystem);
      UMLSubPackage.namespace_ := UMLPackage;
    end
    else
      UMLSubPackage := FindPackage(RoseSubPackage.GetUniqueID);
    ImportPackage(RoseSubPackage, UMLSubPackage, pass);
    if BoldLog.ProcessInterruption then
       exit;
  end;
end;

function TBoldUMLRoseLink.FindPackage(const UniqueId: String): TUMLPackage;
begin
  Result := FindInCache(fCachedPackages, UniqueId) as TUMLPackage;
  if not Assigned(Result) then
  begin
    RefreshCache(fCachedPackages, TUMLPackage);
    Result := FindInCache(fCachedPackages, UniqueId) as TUMLPackage;
  end;
end;


procedure TBoldUMLRoseLink.RefreshCache(var Cache: TStringList; FilterType: TClass);
var
  i: integer;
begin
  FreeAndNil(Cache);
  Cache := TStringList.Create;
  for i := 0 to UMLModel.AllOwnedElement.count - 1 do
    if UMLModel.AllOwnedElement[i] is FilterType then
      Cache.AddObject(UMLModel.AllOwnedElement[i].TaggedValue[BOLDINTERALTVPREFIX + TAG_TOOLID].value, UMLModel.AllOwnedElement[i]);
  Cache.Sorted := true;
end;

function TBoldUMLRoseLink.FindInCache(Cache: TStringList; UniqueId: string): TUMLElement;
var
  pos: integer;
begin
  result := nil;
  if Assigned(Cache) then
  begin
    pos := Cache.IndexOf(UniqueId);
    if pos <> -1 then
      result := Cache.Objects[pos] as TUMLElement;
    if not Assigned(Result) then
      raise EBoldInternal.Create('Internal error');
  end;
end;

procedure TBoldUMLRoseLink.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('PluralSuffix', ReadObsoletePluralSuffix, nil, False);
  Filer.DefineProperty('DefaultMultiplicityForRoles', ReadObsoletMultiplicityForRoles, nil, False);
  Filer.DefineProperty('DefaultMultiplicityForNonNavigableRoles', ReadObsoleteMultiplicityForNonNavigableRoles, nil, False);
end;

procedure TBoldUMLRoseLink.ReadObsoletePluralSuffix(Reader: TReader);
begin
  ReadObsoleteProperty(Reader, 'PluralSuffix', 'PluralSuffix');
end;

procedure TBoldUMLRoseLink.ReadObsoleteMultiplicityForNonNavigableRoles(Reader: TReader);
begin
  ReadObsoleteProperty(Reader, 'DefaultMultiplicityForNonNavigableRoles', 'DefaultNonNavigableMultiplicity');
end;

procedure TBoldUMLRoseLink.ReadObsoletMultiplicityForRoles(Reader: TReader);
begin
  ReadObsoleteProperty(Reader, 'DefaultMultiplicityForRoles', 'DefaultNavigableMultiplicity');
end;

procedure TBoldUMLRoseLink.ReadObsoleteProperty(Reader: TReader; const PropertyName, NewPropertyName: string);
var
  OldPropValue: string;
begin
  if Reader.NextValue = vaString then
    OldPropValue := Reader.ReadString
  else
    OldPropValue := Reader.ReadIdent;

  if OldPropValue <> '' then
    MessageDlg(Format('%s.%s has been moved to model component (Boldify.%s). Old value was "%s"',
                      [ClassName, PropertyName, NewPropertyName, OldPropValue]), mtWarning, [mbOK], 0);
end;

function TBoldUMLRoseLink.GetLogicalPackages: TStrings;
begin
  Result := fMapping.LogicalPackages;
end;

function TBoldUMLRoseLink.GetIncludeSubPackages: Boolean;
begin
  Result := fMapping.IncludeSubPackages;
end;

procedure TBoldUMLRoseLink.SetIncludeSubPackages(const Value: Boolean);
begin
  fMapping.IncludeSubPackages := Value;
end;


initialization
  BoldUMLModelLinkList.AddLink(ROSE_LINKEXTENSION, ROSE_LINKDESC, TBoldUMLRoseLink);
  RoseProp := TBoldUMLRose98Properties.Create(BOLDTOOLNAME);

finalization
  RoseProp.Free;
  if BoldUMLModelLinkListAssigned then
    BoldUMLModelLinkList.RemoveLink(TBoldUMLRoseLink);

end.
