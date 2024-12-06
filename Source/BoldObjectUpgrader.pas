
{ Global compiler directives }
{$include bold.inc}
unit BoldObjectUpgrader;

interface

uses
  Classes,

  BoldCoreConsts,
  BoldId,
  BoldSystem,
  BoldSystemRT,
  BoldDbInterfaces,
  BoldValueSpaceInterfaces,
  BoldAbstractObjectUpgrader,
  BoldPMappersDefault,
  BoldPersistenceControllerDefault;

type
  { forward declarations }
  TBoldObjectUpgraderConfigurationItemWithEvent = class;
  TBoldObjectUpgraderConfigurationWithEvent = class;
  TBoldObjectUpgrader = class;

  TBoldUpgradeEvent = procedure (Obj: TBoldObject) of object;
  TBoldGetSystemTypeInfoEvent = function: TBoldSystemTypeInfo of object;
  TBoldGetPersistenceControllerEvent = function: TBoldPersistenceControllerDefault of object;

  { TBoldObjectUpgraderConfigurationItemWithEvent }
  TBoldObjectUpgraderConfigurationItemWithEvent = class(TBoldObjectUpgraderConfigurationItem)
  private
    fOnUpgradeObject: TBoldUpgradeEvent;
  public
    function GetNamePath: String; override;
    procedure Assign(source: TPersistent); override;
  published
    property OnUpgradeObject: TBoldUpgradeEvent read fOnUpgradeObject write fOnUpgradeObject;
  end;

  { TBoldObjectUpgraderConfigurationWithEvent }
  TBoldObjectUpgraderConfigurationWithEvent = class(TBoldObjectUpgraderConfiguration)
  protected
    function GetItemClass: TBoldObjectUpgraderConfigItemClass; override;
  end;

  { TBoldObjectUpgrader }
  TBoldObjectUpgrader = class(TBoldAbstractObjectUpgrader)
  private
    fNestingLevel: Integer;
    fBoldSystem: TBoldSystem;
    fOnUpgradeObject: TBoldUpgradeEvent;
    fGetSystemTypeInfo: TBoldGetSystemTypeInfoEvent;
    fGetPersistenceController: TBoldGetPersistenceControllerEvent;
    function GetBoldSystem: TBoldSystem;
    function GetValueSpacePMIn: IBoldValueSpace;
    function ObjectMapperForId(ObjectId: TBoldObjectId): TBoldObjectDefaultMapper;
    function GetPersistenceController: TBoldPersistenceControllerDefault;
    function GetSystemTypeInfo: TBoldSystemTypeInfo;
    procedure FetchObjectForId(ObjectId: TBoldObjectId; Query: IBoldQuery);
  public
    constructor create(Config: TBoldObjectUpgraderConfiguration; GetSystemTypeInfo: TBoldGetSystemTypeInfoEvent; GetPersistenceController: TBoldGetPersistenceControllerEvent);
    destructor Destroy; override;
    procedure UpgradeObjectById(ObjectId: TBoldObjectId; Query: IBoldQuery); override;
    procedure UpgradeObject(Obj: TBoldObject);
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    procedure FailTransaction; override;
    procedure ReleaseBoldSystem;
    procedure GenerateAutoUpgradeScript(Script: TStrings);
    property OnUpgradeObject: TBoldUpgradeEvent read fOnUpgradeObject write fOnUpgradeObject;
    property SystemTypeInfo: TBoldSystemTypeInfo read GetSystemTypeInfo;
    property BoldSystem: TBoldSystem read GetBoldSystem;
    property ValueSpacePmIn: IBoldValueSpace read GetValueSpacePmIn;
    property PersistenceController: TBoldPersistenceControllerDefault read GetPersistenceController;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldPMappers,
  BoldPMappersSQL,
  BoldUtils,
  BoldGuard,
  BoldDomainElement;

{ TBoldObjectUpgrader }

constructor TBoldObjectUpgrader.create(Config: TBoldObjectUpgraderConfiguration; GetSystemTypeInfo: TBoldGetSystemTypeInfoEvent; GetPersistenceController: TBoldGetPersistenceControllerEvent);
begin
  inherited Create(Config);
  fGetSystemTypeInfo := GetSystemTypeInfo;
  fGetPersistenceController := GetPersistenceController;
end;

destructor TBoldObjectUpgrader.Destroy;
begin
  ReleaseBoldSystem;
  inherited;
end;

procedure TBoldObjectUpgrader.EndTransaction;
begin
  if fNestinglevel = 1 then
  begin
    if BoldSystem.BoldDirty then
      BoldSystem.UpdateDatabase;
    if BoldSystem.Locators.Count > 1000 then
      ReleaseBoldSystem;
  end;
  dec(fNestingLevel);
end;

procedure TBoldObjectUpgrader.FailTransaction;
begin
  Dec(fNestingLevel);
  if fNEstingLevel = 0 then
  begin
    BoldSystem.Discard;
    ReleaseBoldSystem;
  end;
end;

type
  TBoldExposedSystem = class(TBoldSystem);

procedure TBoldObjectUpgrader.FetchObjectForId(ObjectId: TBoldObjectId; Query: IBoldQuery);
var
  IdList: TBoldObjectIdList;
  MemberPMList: TBoldMemberPersistenceMapperList;
  ObjectMapper: TBoldObjectDefaultMapper;
  Guard: IBoldGuard;
  ObjectList: TBoldObjectlist;
begin
  Guard := TBoldGuard.Create(Objectlist, IdList, MemberPMList);
  
  MemberPMList := TBoldMemberPersistenceMapperList.Create;
  MemberPMList.OwnsEntries := false;
  IdList := TBoldObjectIdList.Create;
  Objectlist := TBoldObjectList.Create;

  IdList.Add(ObjectId);
  ObjectMapper := ObjectMapperForId(ObjectId);
  ObjectMapper.BuildMemberFetchLists(nil, MemberPMList, nil, fmNormal);
  ObjectMapper.HandleFetchData(ObjectId, ValueSpacePmIn, nil, query, MemberPMList, fmNormal, nil);

  Objectlist.Add(BoldSystem.EnsuredLocatorByID[ObjectId].BoldObject);
  
  TBoldExposedSystem(BoldSystem).SystemPersistenceHandler.EndFetchForAll(ObjectList, nil);
end;

procedure TBoldObjectUpgrader.GenerateAutoUpgradeScript(Script: TStrings);
var
  i: integer;
  ConfigItem: TBoldObjectUpgraderConfigurationItem;
  ObjectMapper: TBoldObjectDefaultMapper;
  RootTable: String;
  CurrentVersion: integer;
  VersionColumn: String;
  UpgradeWholeClass: TStringList;
begin
  CurrentVersion := PersistenceController.PersistenceMapper.ModelVersion;
  ObjectMapper := PersistenceController.PersistenceMapper.ObjectPersistenceMappers[0] as TBoldObjectDefaultMapper;
  RootTable := ObjectMapper.Maintable.SQLName;
  VersionColumn := ObjectMapper.ModelVersionMember.ColumnDescriptions[0].SQLName;
  UpgradeWholeClass := TStringList.Create;
  for i := 0 to PersistenceController.PersistenceMapper.ObjectPersistenceMappers.count - 1 do
  begin
    ObjectMapper := PersistenceController.PersistenceMapper.ObjectPersistenceMappers[i] as TBoldObjectDefaultMapper;
    if assigned(ObjectMapper) then
    begin
      configItem := Config.ItemByName[ObjectMapper.ExpressionName];
      if not assigned(configItem) then
        UpgradeWholeClass.Add(IntToStr(ObjectMapper.BoldDbType))
      else if ConfigItem.UpgradeOlderThanVersion < CurrentVersion then
      begin
        Script.Add(format('UPDATE %s SET %s = %d WHERE (%s = %d) AND (%s >= %d) AND (%s <> %d)', [
            RootTable,
            VersionColumn, CurrentVersion,
            TYPECOLUMN_NAME, ObjectMapper.BoldDbType,
            VersionColumn, ConfigItem.UpgradeOlderThanVersion,
            VersionColumn, CurrentVersion]));
      end;
    end;
  end;
  if UpgradeWholeClass.Count <> 0 then
    Script.Add(format('UPDATE %s SET %s = %d WHERE %s IN (%s) AND (%s <> %d)', [
            RootTable,
            VersionColumn, CurrentVersion,
            TYPECOLUMN_NAME, BoldSeparateStringList(UpgradeWholeClass, ', ', '', ''),
            VersionColumn, CurrentVersion]));
  UpgradeWholeClass.Free;
end;

function TBoldObjectUpgrader.GetBoldSystem: TBoldSystem;
var
  aSystemTypeInfo: TBoldSystemTypeInfo;
  aPController: TBoldPersistenceControllerDefault;
begin
  if not assigned(fBoldSystem) then
  begin
    aSystemTypeInfo := SystemTypeInfo;
    aPController := PersistenceController;
    if not assigned(aSystemTypeInfo) then
      raise EBold.CreateFmt(sMissingTypeInfo, [classname]);
    if not assigned(aPController) then
      raise EBold.CreateFmt(sMissingPersistenceController, [classname]);
    fBoldSystem := TBoldSystem.CreateWithTypeInfo(nil, aSystemTypeInfo, aPController)
  end;
  result := fBoldSystem;
end;

function TBoldObjectUpgrader.GetPersistenceController: TBoldPersistenceControllerDefault;
begin
  result := fGetPersistenceController;
end;

function TBoldObjectUpgrader.GetSystemTypeInfo: TBoldSystemTypeInfo;
begin
  result := fGetSystemTypeInfo;
end;

function TBoldObjectUpgrader.GetValueSpacePmIn: IBoldValueSpace;
begin
  result := BoldSystem.AsIBoldvalueSpace[bdepPMIn];
end;

function TBoldObjectUpgrader.ObjectMapperForId(ObjectId: TBoldObjectId): TBoldObjectDefaultMapper;
begin
  result := PersistenceController.PersistenceMapper.ObjectPersistenceMappers[ObjectId.TopSortedIndex] as TBoldObjectDefaultMapper;
end;

procedure TBoldObjectUpgrader.ReleaseBoldSystem;
begin
  if (fNestingLevel <> 0) then
    raise EBold.CreateFmt(sCannotReleaseInOperation, [classname]);
  FreeAndNil(fBoldSystem);
end;

procedure TBoldObjectUpgrader.StartTransaction;
begin
  inc(fNestingLevel);
end;

procedure TBoldObjectUpgrader.UpgradeObject(Obj: TBoldObject);
var
  ConfigItem: TBoldObjectUpgraderConfigurationItemWithEvent;
begin
  ConfigItem := Config.ItemByName[Obj.BoldClassTypeInfo.Expressionname] as TBoldObjectUpgraderConfigurationItemWithEvent;
{$IFNDEF CompareToOldValues}
  Obj.MarkObjectDirty;
{$ENDIF}
  if assigned(ConfigItem) and assigned(ConfigItem.OnUpgradeObject) then
    ConfigItem.OnUpgradeObject(Obj)
  else if assigned(OnUpgradeObject) then
    OnUpgradeObject(Obj);
{$IFNDEF CompareToOldValues}
  Obj.MarkObjectDirty;
{$ENDIF}
end;

procedure TBoldObjectUpgrader.UpgradeObjectById(ObjectId: TBoldObjectId; Query: IBoldQuery);
var
  Obj: TBoldObject;
begin
  StartTransaction;
  try
    FetchObjectForId(ObjectId, Query);
    Obj := BoldSystem.EnsuredLocatorByID[ObjectId].BoldObject;
    if not assigned(Obj) then
      raise EBoldInternal.CreateFmt('%s.UpgradeObject: The object is not in the system', [classname]);
    UpgradeObject(Obj);
    EndTransaction;
  except
    FailTransaction;
    raise;
  end;
end;

{ TBoldObjectUpgraderConfiguration }

function TBoldObjectUpgraderConfigurationWithEvent.GetItemClass: TBoldObjectUpgraderConfigItemClass;
begin
  result := TBoldObjectUpgraderConfigurationItemWithEvent;
end;

{ TBoldObjectUpgraderConfigurationItem }

procedure TBoldObjectUpgraderConfigurationItemWithEvent.Assign(source: TPersistent);
begin
  inherited;
  if Source is TBoldObjectUpgraderConfigurationItemWithEvent then
    OnUpgradeObject := (Source as TBoldObjectUpgraderConfigurationItemWithEvent).OnUpgradeObject;
end;

function TBoldObjectUpgraderConfigurationItemWithEvent.GetNamePath: String;
begin
  result := Config.ConfigOwner.GetNamePath +'._'+ExpressionName+'_';
end;

end.
