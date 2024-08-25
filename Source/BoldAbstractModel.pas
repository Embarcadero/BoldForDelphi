
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractModel;

interface

uses
  Classes,
  BoldMeta,
  BoldTypeNameHandle,
  BoldTypeNameDictionary,
  BoldSubscription;

const
  beModelChanged = 25;

type
  TBoldAbstractModel = class;

  { TBoldAbstractModel }
  TBoldAbstractModel = class(TBoldSubscribableComponent)
  private
    FMoldModel: TMoldModel;
    FRawMoldModel: TMoldModel;
    FTypeNameHandle: TBoldTypeNameHandle;
    fImplicitTypeNameDictionary: TBoldTypeNameDictionary;
    FToBeRemovedInfoFileName: String;
    procedure ReadModel(Reader: TReader);
    procedure WriteModel(Writer: TWriter);
    function GetMoldModel: TMoldModel;
    function GetRawMoldModel: TMoldModel;
    procedure SetTypeNameHandle(const Value: TBoldTypeNameHandle);
    function GetTypeNameDictionary: TBoldTypeNameDictionary;
    procedure ReadToBeRemovedInfoFromFile(MoldModel: TMoldModel);
    procedure SetToBeRemovedInfoFileName(const Value: String);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure EnsureMoldModelCurrent; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFromModel(model: TMoldModel);
    procedure SetFromModelAsString(const ModelAsStrings: TStrings);
    procedure UpdateDesigner;
    procedure WriteToBeRemovedInfoToFile(FileName: String);
    property MoldModel: TMoldModel read GetMoldModel;
    property RawMoldModel: TMoldModel read GetRawMoldModel;
    property TypeNameDictionary: TBoldTypeNameDictionary read GetTypeNameDictionary;
  published
    property TypeNameHandle: TBoldTypeNameHandle read fTypeNameHandle write SetTypeNameHandle;
    property ToBeRemovedInfoFileName: String read FToBeRemovedInfoFileName write SetToBeRemovedInfoFileName;
  end;

implementation

uses
  SysUtils,
  BoldCoreConsts,
  BoldBld,
  BoldGuard,
  BoldEnvironment,
  BoldTaggedValueSupport,
  BoldDefaultTaggedValues;

{---TBoldAbstractModel---}
constructor TBoldAbstractModel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetFromModel(nil);
end;

destructor TBoldAbstractModel.Destroy;
begin
  FreePublisher;
  if fMoldModel <> FRawMoldModel then
    FreeAndNil(fMoldModel);
  FreeAndNil(fRawMoldModel);
  FreeAndNil(fImplicitTypeNameDictionary);
  inherited Destroy;
end;

function TBoldAbstractModel.GetMoldModel;
begin
  EnsureMoldModelCurrent;
  Result := FMoldModel;
end;

procedure TBoldAbstractModel.DefineProperties(Filer: TFiler);

  function DoWriteModel: Boolean;
  var
    ModelAsStrings, AncestorModelAsStrings: TStringList;
    G: IBoldGuard;
  begin
    G := TBoldGuard.Create(ModelAsStrings, AncestorModelAsStrings);
    Result := true;
    if Filer.Ancestor is TBoldAbstractModel then
    begin
      ModelAsStrings := TStringList.Create;
      AncestorModelAsStrings := TStringList.Create;
      TMoldBLDRW.ModelToStrings(RawMoldModel, ModelAsStrings);
      TMoldBLDRW.ModelToStrings((Filer.Ancestor as TBoldAbstractModel).RawMoldModel,AncestorModelAsStrings);
      Result := not ModelAsStrings.Equals(AncestorModelAsStrings);
    end
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty(sModel, ReadModel, WriteModel, DoWriteModel);
end;

procedure TBoldAbstractModel.ReadModel(Reader: TReader);
var
  ModelAsStrings: TStringList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(ModelAsStrings);
  ModelAsStrings := TStringList.Create;
  Reader.ReadListBegin;
  while not Reader.EndOfList do
    ModelAsStrings.Add(Reader.ReadString);
  Reader.ReadListEnd;
  SetFromModelAsString(ModelAsStrings);
end;

procedure TBoldAbstractModel.WriteModel(Writer: TWriter);
var
  I: Integer;
  ModelAsStrings: TStringList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(ModelAsStrings);
  ModelAsStrings := TStringList.Create;
  TMoldBLDRW.ModelToStrings(RawMoldModel, ModelAsStrings);
  Writer.WriteListBegin;
  for I := 0 to ModelAsStrings.Count - 1 do
    Writer.WriteString(ModelAsStrings.Strings[I]);
  Writer.WriteListEnd;
end;

procedure TBoldAbstractModel.Notification(AComponent: TComponent; Operation: TOperation);
begin
 inherited Notification(AComponent, Operation);
 if (Operation = opRemove) and (AComponent = TypeNameHandle) then
   TypeNameHandle := nil;
end;

procedure TBoldAbstractModel.SetFromModel(Model: TMoldModel);
var
  ModelAsStrings: TStringList;
  G: IBoldGuard;
begin
  G := TBoldGuard.Create(ModelAsStrings);
  ModelAsStrings := TStringList.Create;
  if not Assigned(model) then
    Model := TMoldBLDRW.StringsToModel(ModelAsStrings);
  if fMoldModel <> FRawMoldModel then
    FreeAndNil(fMoldModel);
  FreeAndNil(FRawMoldModel);
  fMoldModel := Model;
  if fMoldModel.TrimRemoved then // if nothing is trimmed then RawModel and MoldModel are same and can share same instance.
  begin
    TMoldBLDRW.ModelToStrings(Model, ModelAsStrings);
    fRawMoldModel := TMoldBLDRW.StringsToModel(ModelAsStrings)
  end
  else
    FRawMoldModel := Model;
  FRawMoldModel.TypeNameDictionary := TypeNameDictionary;
  fMoldModel.UpdateMemberIndexes;
  ReadToBeRemovedInfoFromFile(FMoldModel);
  fMoldModel.TypeNameDictionary := TypeNameDictionary;
end;

procedure TBoldAbstractModel.SetFromModelAsString(const ModelAsStrings: TStrings);
begin
  if fMoldModel <> FRawMoldModel then
    FreeAndNil(fMoldModel);
  FreeAndNil(FRawMoldModel);
  FMoldModel := TMoldBLDRW.StringsToModel(ModelAsStrings);
  if fMoldModel.TrimRemoved then  // if nothing is trimmed then RawModel and MoldModel are same and can share same instance
  begin
    fRawMoldModel := TMoldBLDRW.StringsToModel(ModelAsStrings);
    FRawMoldModel.TypeNameDictionary := TypeNameDictionary;
  end
  else
    fRawMoldModel := fMoldModel;
  fMoldModel.UpdateMemberIndexes;
  ReadToBeRemovedInfoFromFile(FMoldModel);
  fMoldModel.TypeNameDictionary := TypeNameDictionary;
end;

procedure TBoldAbstractModel.SetTypeNameHandle(
  const Value: TBoldTypeNameHandle);
begin
  FreeAndNil(fImplicitTypeNameDictionary);
  FTypeNameHandle := Value;
  if assigned(Value) then
    Value.FreeNotification(Self);
  if assigned(fMoldModel) then
  begin
    fMoldModel.TypeNameDictionary := TypeNameDictionary;
    fRawMoldModel.TypeNameDictionary := TypeNameDictionary;
  end;
end;

function TBoldAbstractModel.GetTypeNameDictionary: TBoldTypeNameDictionary;
begin
  if not assigned(fTypeNameHandle) then
  begin
    if not assigned(fImplicitTypeNameDictionary) then
    begin
      FImplicitTypeNameDictionary := TBoldCurrentTypeNameDictionaryClass.Create(self);
      fImplicitTypeNameDictionary.AddDefaultMappings;
    end;
    Result := fImplicitTypeNameDictionary;
  end
  else
    Result := FTypeNameHandle.Dictionary;
end;

procedure TBoldAbstractModel.UpdateDesigner;
begin
  if (csDesigning in ComponentState) and
    not (csUpdating in ComponentState) then
    BoldEffectiveEnvironment.UpdateDesigner(Self);
end;

function TBoldAbstractModel.GetRawMoldModel: TMoldModel;
begin
  EnsureMoldModelCurrent;
  Result := FRawMoldModel;
end;

procedure TBoldAbstractModel.SetToBeRemovedInfoFileName(const Value: String);
begin
  FToBeRemovedInfoFileName := Trim(Value);
end;

procedure TBoldAbstractModel.ReadToBeRemovedInfoFromFile(MoldModel: TMoldModel);
var
  i, p: integer;
  Clsname: String;
  MemberName: String;
  MoldClass: TMoldClass;
  MoldAssoc: TMoldAssociation;
  MoldAttribute: TMoldAttribute;
  Info: TStringLIst;
begin
  if (ToBeRemovedInfoFileName <> '') and fileexists(ToBeRemovedInfoFileName) then
  begin
    Info := TStringList.Create;
    try
      Info.LoadFromFile(ToBeRemovedInfoFileName);
      for i := 0 to Info.count-1 do
      begin
        p := pos('.', Info[i]);
        if p <> 0 then
        begin
          clsName := copy(Info[i], 1, p - 1);
          MemberName := copy(Info[i], p + 1, maxint);
          MoldClass := MoldModel.Classes.ItemsByName[ClsName];
          if assigned(MoldClass) then
          begin
            MoldAttribute := MoldClass.Attributes.ItemsByName[MemberName];
            if Assigned(MoldAttribute) and (MoldAttribute.EvolutionState = esNormal) then
              MoldAttribute.BoldTVByName[TAG_EVOLUTIONSTATE] := TV_EVOLUTIONSTATE_TOBEREMOVED;
          end;
        end
        else
        begin
          MoldClass := MoldModel.Classes.ItemsByName[Info[i]];
          if assigned(MoldClass) then
            MoldClass.BoldTVByName[TAG_EVOLUTIONSTATE] := TV_EVOLUTIONSTATE_TOBEREMOVED;
          MoldAssoc := MoldModel.Associations.ItemsByName[Info[i]];
          if assigned(MoldAssoc) then
            MoldAssoc.BoldTVByName[TAG_EVOLUTIONSTATE] := TV_EVOLUTIONSTATE_TOBEREMOVED;
        end;
      end;
    finally
      Info.Free;
    end;
  end;
end;

procedure TBoldAbstractModel.WriteToBeRemovedInfoToFile(FileName: String);
var
  i, j: integer;
  MoldClass: TMoldClass;
  info: TStringList;
begin
  Info := TStringList.create;
  try
    for i := 0 to MoldModel.Classes.Count - 1 do
    begin
      MoldClass := MoldModel.Classes[i];
      if MoldClass.EvolutionState = esToBeRemoved then
        Info.Add(MoldClass.Name);

      for j := 0 to MoldClass.Attributes.Count-1 do
        if MoldClass.Attributes[j].EvolutionState = esToBeRemoved then
          Info.Add(format('%s.%s', [MoldClass.Name, MoldClass.Attributes[j].Name]));
    end;
    for i := 0 to MoldModel.Associations.Count - 1 do
      if MoldModel.Associations[i].EvolutionState = esToBeremoved then
        Info.Add(MoldModel.Associations[i].Name);

    Info.SaveToFile(FileName);
  finally
    Info.free;
  end;
end;


procedure TBoldAbstractModel.EnsureMoldModelCurrent;
begin

end;

end.