{ Global compiler directives }
{$include bold.inc}
unit BoldModel;

interface

uses
  Classes,
  BoldContainers,
  BoldSubscription,
  BoldSystemHandle,
  BoldMeta,
  BoldUMLModelSupport,
  BoldAbstractModel,
  BoldUMLModel,
  BoldTypeNameDictionary;

type
  { forward declarations }
  TBoldModel = class;
  TBoldModelList = class;

  TUMLModelMode = (ummNone, ummDesignTime, ummRunTime);

  { TBoldModel }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldModel = class(TBoldAbstractModel)
  private
    FUMLModelExposed: Boolean;
    fBoldify: TBoldUMLBoldify;
    fUMLModel: TUMLModel;
    fUMLModelMode: TUMLModelMode;
    fUMLModelAsString: string;
    fSystemHandle: TBoldSystemHandle;
    fDataTypes: TUMLDataTypeList;
    fModelChangedSubscriber: TBoldPassthroughSubscriber;
    fEditableModel: boolean;
    fValidateNesting: integer;
    procedure SetBoldify(const Value: TBoldUMLBoldify);
    procedure EnsureUMLModel;
    procedure AssertDesignTime;
    procedure ReadUMLModelAsString(Reader: TReader);
    procedure WriteUMLModelAsString(Writer: TWriter);
    procedure UMLModelFromString(const ModelAsString: string);
    function GetEnsuredUMLModel: TUMLModel;
    function GetUMLModel: TUMLModel;
    procedure UMLModelChangedRecieve(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetIsValidating: boolean;
    function GetDataTypes: TUMLDataTypeList;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure SubscribeToUMLModel;
    procedure UnSubscribeToUMLModel;
    procedure MarkUMLModelExposed;
    function GetUMLModelMoreCurrent: Boolean;
    procedure EnsureMoldModelCurrent; override;
    property UMLModelExposed: Boolean read FUMLModelExposed;
    property UMLModelMoreCurrent: Boolean read GetUMLModelMoreCurrent;
    procedure StartValidation;
    procedure EndValidation;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure EnsureTypes;
    function UMLModelToString: string;
    property UMLModel: TUMLModel read GetUMLModel;
    property EnsuredUMLModel: TUMLModel read GetEnsuredUMLModel;
    property IsValidating: boolean read GetIsValidating;
    property SystemHandle: TBoldSystemHandle read fSystemHandle;
    property DataTypes: TUMLDataTypeList read GetDataTypes;
  published
    property UMLModelMode: TUMLModelMode read fUMLModelMode write fUMLModelMode;
    property Boldify: TBoldUMLBoldify read FBoldify write SetBoldify;
    property EditableModel: boolean read fEditableModel write fEditableModel stored False;
  end;

  { TBoldModelList }
  TBoldModelList = class(TObject)
  private
    fEntries: TBoldObjectArray;
    function GetItem(Index: Integer): TBoldModel;
    function GetCount: Integer;
  protected
    procedure AddModel(BoldModel: TBoldModel);
    procedure RemoveModel(BoldModel: TBoldModel);
  public
    constructor Create;
    destructor Destroy; override;
    property Item[Index: Integer]: TBoldModel read GetItem; default;
    property Count: Integer read GetCount;
  end;

function TheModelList: TBoldModelList;

implementation

uses
  SysUtils,
  BoldUMLModelDataModule,
  BoldLogHandler,
  BoldUMLModelStreamer,
  BoldDefs,
  BoldDefaultTaggedValues,
  BoldMemberTypeDictionary,
  BoldUMLAttributes,
  BoldUMLModelConverter,
  BoldUMLModelValidator,
  BoldUMLUtils,
  BoldHandles,
  BoldGuard,
  BoldSystem;

var
  G_ModelList: TBoldModelList = nil;

function TheModelList: TBoldModelList;
begin
  if not assigned(G_ModelList) then
    G_ModelList := TBoldModelList.Create;
  result := G_ModelList;
end;

{ TBoldModel }

procedure TBoldModel.Assign(Source: TPersistent);
begin
  if Source is TBoldModel then
  begin
    EnsureUMLModel;
    self.UMLModelFromString(TBoldModel(Source).UMLModelToString)
  end
  else
    inherited;
end;

constructor TBoldModel.Create(owner: TComponent);
begin
  inherited;
  TheModelList.AddModel(Self);
  fBoldify := TBoldUMLBoldify.Create;
end;

procedure TBoldModel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('UMLModelAsString', ReadUMLModelAsString, WriteUMLModelAsString, UMLModelMode <> ummNone);
end;

destructor TBoldModel.Destroy;
begin
  if assigned(G_ModelList) then
    TheModelList.RemoveModel(Self);
  if Assigned(fSystemhandle) then
    fSystemHandle.Active := False;
  FreeAndNil(fDataTypes);
  FreeAndNil(fBoldify);
  FreeAndNil(fSystemHandle);
  FreeAndNil(fModelChangedSubscriber);
  inherited;
end;

procedure TBoldModel.AssertDesignTime;
begin
  if not (csDesigning in ComponentState ) then
    raise EBold.Create('UMLModel not accessible in run time');
end;

procedure TBoldModel.EnsureTypes;
var
  Index: Integer;
  BoldMemberTypeList: TBoldMemberTypeList;
  UMLDataTypes: TUMLDataTypeList;

  procedure EnsureDataType(const modelName: string);
  var
    aType: TUMLDataType;
    i: integer;
  begin
    aType := nil;
    for i := 0 to UMLDataTypes.Count - 1 do
    begin
      if SameText(UMLDataTypes[i].name, modelName) then
        aType := UMLDataTypes[i];
    end;
    if not Assigned(aType) then
    begin
      aType := TUMLDataType.Create(EnsuredUMLModel.BoldSystem);
      TBoldUMLSupport.EnsureBoldTaggedValues(aType);
      aType.namespace_ := EnsuredUMLModel;
      aType.Name := modelName;
      if Assigned(EnsuredUMLModel.BoldSystem.BoldSystemTypeInfo.AttributeTypeInfoByExpressionName[modelName]) then
      begin
        aType.isAbstract := EnsuredUMLModel.BoldSystem.BoldSystemTypeInfo.AttributeTypeInfoByExpressionName[modelName].IsAbstract;
      end;
    end;
    fDataTypes.Add(aType as TUMLDataType);
  end;

begin
  BoldMemberTypeList := BoldMemberTypes;
  if not assigned(fDataTypes) then
    fDataTypes := TUMLDataTypeList.CreateWithTypeInfo(EnsuredUMLModel.BoldSystem.BoldSystemTypeInfo.ClassTypeInfoByClass[TUMLDataType])
  else
    fDataTypes.Clear;
  EnsuredUMLModel.BoldSystem.StartTransaction();
  try
    UMLDataTypes := EnsuredUMLModel.BoldSystem.ClassByObjectClass[TUMLDataType] as TUMLDataTypeList;
    with TypeNameDictionary do
    begin
      for Index := 0 to Count - 1 do
      begin
        if Mapping[index].ModelName = DEFAULTNAME then
        else if Assigned(BoldMemberTypeList.DescriptorByDelphiName[Mapping[Index].ExpandedDelphiName]) then
        begin
          if BoldMemberTypeList.DescriptorByDelphiName[Mapping[index].ExpandedDelphiName].AbstractionLevel = alConcrete then
            EnsureDataType(Mapping[index].ModelName);
        end
        else
          EnsureDataType(Mapping[index].ModelName);
      end;
      EnsuredUMLModel.BoldSystem.CommitTransaction();
    end;
  except
    EnsuredUMLModel.BoldSystem.RollbackTransaction();
    raise;
  end;
end;

function TBoldModel.GetDataTypes: TUMLDataTypeList;
begin
  if not Assigned(fDataTypes) or fDataTypes.Empty then
    EnsureTypes;
  result := fDataTypes;
end;

function TBoldModel.GetEnsuredUMLModel: TUMLModel;
begin
  EnsureUMLModel;
  Result := fUMLModel;
  MarkUMLModelExposed;
end;

function TBoldModel.GetIsValidating: boolean;
begin
  result := fValidateNesting > 0;
end;

procedure TBoldModel.ReadUMLModelAsString(Reader: TReader);
begin
  fUMLModelAsString := Reader.ReadString;
end;

procedure TBoldModel.EnsureUMLModel;
var
  dmModelEdit: TdmModelEdit;
begin
  if not Assigned(fUMLModel) then
  begin
    BoldLog.StartLog('Initializing UML meta model');
    BoldLog.Sync;
    dmModelEdit := TdmModelEdit.Create(self);
    fSystemHandle := TBoldSystemHandle.Create(nil);
    fSystemHandle.AutoActivate := False;
    fSystemHandle.SystemTypeInfoHandle := dmModelEdit.BoldSystemTypeInfoHandle1;
    fSystemHandle.Active := True;
    BoldLog.EndLog;
    fSystemHandle.System.StartTransaction();
    try
      case UMLModelMode of
        ummNone:
        begin
          fUMLModel := TUMLModel.Create(fSystemHandle.System);
          TBoldModelConverter.MoldModelToUML(RawMoldModel, fUMLModel);
        end;
        ummDesignTime:
        begin
          AssertDesignTime;
          UMLModelFromString(fUMLModelAsString);
        end;
        ummRunTime: UMLModelFromString(fUMLModelAsString);
      end;
      fUMLModelAsString := '';
      fSystemHandle.System.CommitTransaction();
    except
      fSystemHandle.System.RollbackTransaction();
      raise;
    end;
  end;
end;

procedure TBoldModel.SetBoldify(const Value: TBoldUMLBoldify);
begin
  Boldify.Assign(Value);
end;

procedure TBoldModel.StartValidation;
begin
  Inc(fValidateNesting);
end;

procedure TBoldModel.EndValidation;
begin
  Dec(fValidateNesting);
end;

procedure TBoldModel.UMLModelFromString(const ModelAsString: string);
begin
  if ModelAsString <> '' then
  begin
    if assigned(fUMLModel) then
    begin
      fUMLModel.Delete;
      fUMLModel := nil;
    end;
    TUMLModelStreamer.FillSystemFromString(fSystemHandle.System, ModelAsString, fSystemHandle.SystemTypeInfoHandle.BoldModel.MoldModel);
    fUMLModel := fSystemHandle.System.EvaluateExpressionAsDirectElement('UMLModel.allInstances->first') as TUMLModel;
    if not Assigned(fUMLModel) then
      raise EBold.Create('Bad string format for UMLModel');
  end;
end;

function TBoldModel.UMLModelToString: string;
begin
  EnsureUMLModel;
  Result := TUMLModelStreamer.SystemAsString(fSystemHandle.System, fSystemHandle.SystemTypeInfoHandle.BoldModel.MoldModel);
end;

procedure TBoldModel.WriteUMLModelAsString(Writer: TWriter);
begin
  fUMLModelAsString := UMLModelToString;
  Writer.WriteString(fUMLModelAsString);
end;

constructor TBoldModelList.Create;
begin
  fEntries := TBoldObjectArray.Create(8, []);
end;

procedure TBoldModelList.AddModel(BoldModel: TBoldModel);
begin
  fEntries.Add(BoldModel);
end;

destructor TBoldModelList.Destroy;
begin
  FreeAndNil(fEntries);
  inherited;
end;

function TBoldModelList.GetItem(Index: Integer): TBoldModel;
begin
  Result := TBoldModel(fEntries[Index]);
end;

function TBoldModelList.GetCount: Integer;
begin
  Result := fEntries.Count;
end;

procedure TBoldModelList.RemoveModel(BoldModel: TBoldModel);
begin
  fEntries.remove(BoldModel);
end;

procedure TBoldModel.EnsureMoldModelCurrent;
var
  MoldModel: TMoldModel;
  Validator: TBoldUMLModelValidator;
  Errors: TStringList;
  FlattenState, BoldifyState: Boolean;
  i: integer;
  G: IBoldGuard;
  TransactionHandler: IBoldTransactionHandler;
begin
  G := TBoldGuard.Create(Validator, Errors);
  if UMLModelMoreCurrent then
  begin
    TransactionHandler := EnsuredUMLModel.BoldSystem.CreateTransactionHandler;
    FlattenState := TBoldUMLSupport.IsFlattened(UMLModel);
    BoldifyState := Boldify.IsBoldified(UMLModel);
    if not FlattenState then
      TBoldUMLSupport.Flatten(UMLModel);

    if not BoldifyState then
      Boldify.Boldify(UMLModel);

    Errors := TStringList.Create;

    Validator := TBoldUMLModelValidator.Create(self, SQLDataBaseConfigforModel(self));

    Validator.Validate(TypeNameDictionary);

    for i := 0 to UMLModel.Validator.Violation.Count - 1 do
      if UMLModel.Validator.Violation[i].Severity = sError then
        Errors.Add(UMLModel.Validator.Violation[i].Description);

    MoldModel :=  TBoldModelConverter.UMLModelToMold(UMLModel);
    MoldModel.TVByName[BOLDINTERALTVPREFIX + TV_MODELERRORS] := Errors.CommaText;

    SetFromModel(MoldModel);

    if not FlattenState then
      TBoldUMLSupport.UnFlatten(UMLModel);

    if not BoldifyState then
      Boldify.UnBoldify(UMLModel);

    SubscribeToUMLModel;
    TransactionHandler.Commit;
  end;
end;

function TBoldModel.GetUMLModel: TUMLModel;
begin
  Result := fUMLModel;
  if Assigned(fUMLModel) then
    MarkUMLModelExposed;
end;

procedure TBoldModel.UMLModelChangedRecieve(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if IsValidating then
    exit;
  UnSubscribeToUMLModel;
  SendEvent(self, beModelChanged);
  UpdateDesigner;
end;

procedure TBoldModel.SubscribeToUMLModel;
begin
  Assert(Assigned(fUMLModel));
  Assert(not Assigned(fModelChangedSubscriber));
  fModelChangedSubscriber := TBoldPassthroughSubscriber.Create(UMLModelChangedRecieve);
  TBoldUMLSupport.SubscribeToEntireModel(fUMLModel, fModelChangedSubscriber);
end;

procedure TBoldModel.UnSubscribeToUMLModel;
begin
  FreeAndNil(fModelChangedSubscriber);
end;

procedure TBoldModel.MarkUMLModelExposed;
begin
  if (not FUMLModelExposed ) then
  begin
    FUMLModelExposed := True;
    SubscribeToUMLModel;
  end;
end;

function TBoldModel.GetUMLModelMoreCurrent: Boolean;
begin
   Result := UMLModelExposed and not Assigned(fModelChangedSubscriber);
end;

initialization

finalization
  FreeAndNil(G_ModelList);

end.
