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
  TBoldModel = class(TBoldAbstractModel)
  private
    FUMLModelExposed: Boolean;
    fBoldify: TBoldUMLBoldify;
    fUMLModel: TUMLModel;
    fUMLModelMode: TUMLModelMode;
    fUMLModelAsString: string;
    fSystemHandle: TBoldSystemHandle;
    fModelChangedSubscriber: TBoldPassthroughSubscriber;
    fEditableModel: boolean;
    procedure SetBoldify(const Value: TBoldUMLBoldify);
    procedure EnsureUMLModel;
    procedure AssertDesignTime;
    procedure ReadUMLModelAsString(Reader: TReader);
    procedure WriteUMLModelAsString(Writer: TWriter);
    function UMLModelToString: string;
    procedure UMLModelFromString(ModelAsString: string);
    function GetEnsuredUMLModel: TUMLModel;
    function GetUMLModel: TUMLModel;
    procedure UMLModelChangedRecieve(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure SubscribeToUMLModel;
    procedure UnSubscribeToUMLModel;
    procedure MarkUMLModelExposed;
    function GetUMLModelMoreCurrent: Boolean;
    procedure EnsureMoldModelCurrent; override;
    property UMLModelExposed: Boolean read FUMLModelExposed;
    property UMLModelMoreCurrent: Boolean read GetUMLModelMoreCurrent;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure EnsureTypes;
    property UMLModel: TUMLModel read GetUMLModel;
    property EnsuredUMLModel: TUMLModel read GetEnsuredUMLModel;
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
  BoldUMLModelDataModule, // Circular dependency
  SysUtils,
  BoldLogHandler,
  BoldUMLModelStreamer,
  BoldDefs,
  BoldDefaultTaggedValues,
  BoldMemberTypeDictionary,
  BoldUMLAttributes,
  BoldUMLModelConverter,
  BoldUMLModelValidator,
  BoldGuard;

var
  G_ModelList: TBoldModelList = nil;

function TheModelList: TBoldModelList;
begin
  if not assigned(G_ModelList) then
    G_ModelList := TBoldModelList.Create;
  result := G_ModelList;
end;

{ TBoldModel }

constructor TBoldModel.Create(owner: TComponent);
begin
  inherited;
  TheModelList.AddModel(Self);
  fBoldify := TBoldUMLBoldify.Create;
end;

procedure TBoldModel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('UMLModelAsString', ReadUMLModelAsString, WriteUMLModelAsString, UMLModelMode <> ummNone); // do not localize
end;

destructor TBoldModel.Destroy;
begin
  if assigned(G_ModelList) then
    TheModelList.RemoveModel(Self);
  if Assigned(fSystemhandle) then
    fSystemHandle.Active := False;
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

  procedure EnsureDataType(const modelName: string);
  var
    aType: TUMLGeneralizableElement;
  begin
    aType := EnsuredUMLModel.EvaluateExpressionAsDirectElement
    (
      Format('allOwnedElement->select(OCLIsKindOf(UMLGeneralizableElement) and (name=''%s''))->first', [modelName])
    ) as TUMLGeneralizableElement;
    if not Assigned(aType) then
    begin
      aType := TUMLDataType.Create(EnsuredUMLModel.BoldSystem);
      TBoldUMLSupport.EnsureBoldTaggedValues(aType);
      aType.namespace_ := EnsuredUMLModel;
      aType.Name := modelName;
    end;
  end;

begin
  BoldMemberTypeList := BoldMemberTypes;

  with TypeNameDictionary do
  begin
    for Index := 0 to Count - 1 do
    begin
      if Mapping[index].ModelName = DEFAULTNAME then
        // ignore
      else if Assigned(BoldMemberTypeList.DescriptorByDelphiName[Mapping[Index].ExpandedDelphiName]) then
      begin
        if BoldMemberTypeList.DescriptorByDelphiName[Mapping[index].ExpandedDelphiName].AbstractionLevel = alConcrete then
          EnsureDataType(Mapping[index].ModelName);
      end
      else
        EnsureDataType(Mapping[index].ModelName);
    end;
  end;
end;

function TBoldModel.GetEnsuredUMLModel: TUMLModel;
begin
  EnsureUMLModel;
  Result := fUMLModel;
  MarkUMLModelExposed;
end;

procedure TBoldModel.ReadUMLModelAsString(Reader: TReader);
begin
  fUMLModelAsString := Reader.ReadString;
end;

procedure TBoldModel.EnsureUMLModel;
begin
  if not Assigned(fUMLModel) then
  begin
    BoldLog.StartLog('Initializing UML meta model');
    BoldLog.Sync;
    EnsureModelEditDataModule;
    fSystemHandle := TBoldSystemHandle.Create(nil);
    fSystemHandle.AutoActivate := False;
    fSystemHandle.SystemTypeInfoHandle := dmModelEdit.BoldSystemTypeInfoHandle1;
    fSystemHandle.Active := True;
    BoldLog.EndLog;
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
    fUMLModelAsString := ''; // Reclaim memory. Saving will be straight from UMLModel
  end;
end;

procedure TBoldModel.SetBoldify(const Value: TBoldUMLBoldify);
begin
  Boldify.Assign(Value);
end;

procedure TBoldModel.UMLModelFromString(ModelAsString: string);
begin
  if ModelAsString <> '' then
  begin
    if assigned(fUMLModel) then
    begin
      fUMLModel.Delete;
      fUMLModel := nil;
    end;
    // Model always freshly created at this point.
    TUMLModelStreamer.FillSystemFromString(fSystemHandle.System, ModelAsString, fSystemHandle.SystemTypeInfoHandle.BoldModel.MoldModel);
    fUMLModel := fSystemHandle.System.EvaluateExpressionAsDirectElement('UMLModel.allInstances->first') as TUMLModel; // do not localize
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
begin
  G := TBoldGuard.Create(Validator, Errors);
  if UMLModelMoreCurrent then
  begin
    FlattenState := TBoldUMLSupport.IsFlattened(UMLModel);
    BoldifyState := Boldify.IsBoldified(UMLModel);
    if not FlattenState then
      TBoldUMLSupport.Flatten(UMLModel);

    if not BoldifyState then
      Boldify.Boldify(UMLModel);

    Errors := TStringList.Create;

    Validator := TBoldUMLModelValidator.Create(UMLModel);

    Validator.Validate(TypeNameDictionary);

    for i := 0 to UMLModel.Validator.Violation.Count - 1 do
      if UMLModel.Validator.Violation[i].Severity = sError then
        Errors.Add(UMLModel.Validator.Violation[i].Description);

    MoldModel :=  TBoldModelConverter.UMLModelToMold(UMLModel);
    MoldModel.TVByName[BOLDINTERALTVPREFIX + TV_MODELERRORS] := Errors.CommaText;

    SetFromModel(MoldModel); // hands over ownership of MoldModel

    // Restore  UML model state
    if not FlattenState then
      TBoldUMLSupport.UnFlatten(UMLModel);

    if not BoldifyState then
      Boldify.UnBoldify(UMLModel);

    SubscribeToUMLModel;
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
  UnSubscribeToUMLModel;
  SendEvent(self, beModelChanged);
  UpdateDesigner;
end;

procedure TBoldModel.SubscribeToUMLModel;
begin
  Assert(Assigned(fUMLModel));
  Assert(not Assigned(fModelChangedSubscriber));
  fModelChangedSubscriber := TBoldPassthroughSubscriber.Create(UMLModelChangedRecieve);
  TBoldUMLSupport.SubscribeToEntireModel(fUMLModel, fModelChangedSubscriber);  // note fUMLModel to stop loops
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
