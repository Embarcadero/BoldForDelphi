unit BoldPersistenceHandleFile;

interface

uses
  BoldPersistenceHandle,
  BoldPersistenceController,
  BoldId,
  BoldDefs,
  BoldAbstractModel,
  BoldMeta,
  BoldUpdatePrecondition,
  BoldCondition,
  BoldFreeStandingValues,
  BoldValueSpaceInterfaces;

type
  { forward declarations }
  TBoldAbstractPersistenceHandleFile = class;
  TBoldPersistenceControllerFile = class;

  { TBoldAbstractPersistenceHandleFile }
  {$MESSAGE WARN 'BoldModel should have FreeNotification!'}
  TBoldAbstractPersistenceHandleFile = class(TBoldPersistenceHandle)
  private
    FFileName: String;
    FCacheData: Boolean;
    FBoldModel: TBoldAbstractModel;
    procedure SetFileName(const Value: String);
    procedure SetCacheData(const Value: Boolean);
    procedure SetBoldModel(const Value: TBoldAbstractModel);
  published
    property FileName: String read FFileName write SetFileName;
    property CacheData: Boolean read FCacheData write SetCacheData;
    property BoldModel: TBoldAbstractModel read FBoldModel write SetBoldModel;
  end;

  { TBoldPersistenceControllerFile }
  TBoldPersistenceControllerFile = class(TBoldPersistenceController)
  private
    fFreeStandingValueSpace: TBoldFreeStandingValueSpace;
    fFileName: String;
    fFileTimeStamp: TDateTime;
    fCacheData: Boolean;
    fMoldModel: TMoldModel;
    procedure EnsureValueSpace;
    procedure UnloadLocalValueSpace;
    function GetValueSpace: IBoldValueSpace;
  protected
    procedure WriteValueSpace; virtual; abstract;
    procedure ReadValueSpace; virtual; abstract;
  public
    constructor Create(filename: string; CacheData: Boolean; MoldModel: TMoldModel);
    destructor Destroy; override;
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; BoldClientID: TBoldClientID); override;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMSetReadOnlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); override;
    function MultilinksAreStoredInObject: Boolean; override;
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
                    TranslationList: TBoldIdTranslationList); override;
    property FileName: String read fFileName;
    property LocalValueSpace: IBoldValueSpace read GetValueSpace;
    property CacheData: Boolean read fCacheData;
    property MoldModel: TMoldModel read fMoldModel;
  end;

implementation

uses
  Dialogs,
  SysUtils,
  BoldDefaultId,
  PersistenceConsts,
  BoldCoreConsts;

{ TBoldPersistenceControllerFile }

constructor TBoldPersistenceControllerFile.create(filename: string; CacheData: Boolean; MoldModel: TMoldModel);
begin
  Inherited Create;
  fFileName := FileName;
  fCacheData:= CacheData;
  fMoldModel := MoldModel;
end;

function TBoldPersistenceControllerFile.GetValueSpace: IBoldValueSpace;
begin
  EnsureValueSpace;
  result := fFreeStandingValueSpace;
end;

procedure TBoldPersistenceControllerFile.EnsureValueSpace;
  procedure FixupValueSpace; // make sure that valuspaces in old formats can be handled
  begin
  // Make sure persistacestate is current for everything
    fFreeStandingValueSpace.MarkAllObjectsAndMembersCurrent;
  end;
begin
  if not assigned(fFreeStandingValueSpace) then
  begin
    fFreeStandingValueSpace := TBoldFreeStandingValueSpace.create;
    if fileexists(Filename) then
    begin
      ReadValueSpace;
      FixupValueSpace;
      fFileTimestamp := FileDateToDateTime(FileAge(FileName))
    end
    else
      fFileTimeStamp := 0;
  end;
end;

procedure TBoldPersistenceControllerFile.PMExactifyIds(
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  raise EBold.Create(sNotImplemented);
end;

procedure TBoldPersistenceControllerFile.PMFetch(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  MemberIdList: TBoldMemberIdList; FetchMode: Integer;
  BoldClientID: TBoldClientID);
var
  i: integer;
begin
  EnsureValueSpace;
  for i := 0 to ObjectIdList.Count - 1 do
    if not LocalValueSpace.HasContentsForId[ObjectIdList[i]] then
      raise EBold.CreateFmt(sObjectNotInFile, [ClassName, ObjectIdList[i].AsString]);
  BoldApplyPartialValueSpace(ValueSpace, LocalValueSpace, ObjectIdList, MemberIdList, false);

  if not cacheData then
    UnloadLocalValueSpace;
end;

procedure TBoldPersistenceControllerFile.PMFetchIDListWithCondition(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  FetchMode: Integer; Condition: TBoldCondition;
  BoldClientID: TBoldClientID);
var
  AllObjectIdList: TBoldObjectIdList;
  i: integer;
  SearchClass: TMoldClass;
  CurrentClass: TMoldClass;
  NewCondition: TBoldConditionWithClass;
begin
  if Condition is TBoldConditionWithClass then
  begin
    EnsureValueSpace;
    AllObjectIdList := TBoldObjectIdList.Create;
    LocalValueSpace.AllObjectIds(AllObjectIdList, false);
    SearchClass := MoldModel.Classes[TBoldConditionWithClass(Condition).TopSortedIndex];
    for i := 0 to AllObjectIdList.Count - 1 do
    begin
      CurrentClass := MoldModel.Classes[AllObjectIdList[i].TopSortedIndex];
      if CurrentClass.ChildTo(SearchClass) then
       ObjectIdList.Add(AllObjectIdList[i]);
    end;
    AllObjectIdlist.Free;
    if not cacheData then
      UnloadLocalValueSpace;
  end
  else if Condition is TBoldSQLCondition then
  begin
    ShowMessage(sSQLNotSpokenHere);
    NewCondition := TBoldConditionWithClass.Create;
    NewCondition.TopSortedIndex := TBoldSQLCondition(Condition).TopSortedIndex;
    PMFetchIDListWithCondition(ObjectIdList, ValueSpace, FetchMode, NewCondition, NOTVALIDCLIENTID);
    NewCondition.free;
  end
  else
    raise EBold.CreateFmt(sUnknownConditionType, [Condition.Classname]);
end;

procedure TBoldPersistenceControllerFile.PMSetReadonlyness(ReadOnlyList,
  WriteableList: TBoldObjectIdList);
begin
  raise EBold.Create(sNotImplemented);
end;

procedure TBoldPersistenceControllerFile.PMTranslateToGlobalIds(
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  raise EBold.Create(sNotImplemented);
end;

procedure TBoldPersistenceControllerFile.PMTranslateToLocalIds(
  GlobalIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  raise EBold.Create(sNotImplemented);
end;

procedure TBoldPersistenceControllerFile.PMUpdate(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  Old_Values: IBoldValueSpace;
  Precondition: TBoldUpdatePrecondition;
  TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; BoldClientID: TBoldClientID);
var
  NewTimeStamp: TDateTime;
  LocalTranslationList: TBoldIdTranslationList;
begin
  if assigned(Precondition) then
    raise EBold.CreateFmt(sPreconditionsNotSupported, [Classname, Precondition.Classname]);

  EnsureValueSpace;

  if fileexists(Filename) then
    NewTimeStamp := FileDateToDateTime(FileAge(FileName))
  else
    NewTimeStamp := 0;

  if (NewTimeStamp <> fFileTimeStamp) then
  begin
{    if MessageDlg('The datafile has been written since you last accessed it... Go ahead?',
        mtConfirmation, [mbYes, mbNo], 0) = mrNo then
      exit
    else}
      UnloadLocalValueSpace;
  end;


  if not assigned(TranslationList) then
    LocalTranslationList := TBoldIdTranslationList.Create
  else
    LocalTranslationList := TranslationList;
  ReserveNewIds(LocalValueSpace, ObjectIdList, LocalTranslationList);

  BoldApplyPartialValueSpace(LocalValueSpace,  ValueSpace, ObjectIdList, nil, True, [bvpsInvalid, bvpsTransient]);
  LocalValueSpace.ApplytranslationList(LocalTranslationList);
  fFreeStandingValueSpace.RemoveDeletedObjects;

  ValueSpace.ApplytranslationList(LocalTranslationList);

  if not assigned(TranslationList) then
    LocalTranslationList.Free;

  WriteValueSpace;

  fFileTimestamp := FileDateToDateTime(FileAge(FileName));
end;

procedure TBoldPersistenceControllerFile.UnloadLocalValueSpace;
begin
  // does not work.
{  fFreeStandingValueSpace.Free;
  fFreeStandingValueSpace := nil;
  }
end;

function TBoldPersistenceControllerFile.MultilinksAreStoredInObject: Boolean;
begin
  result := true;
end;

procedure TBoldPersistenceControllerFile.ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
              TranslationList: TBoldIdTranslationList);
var
  i: integer;
  NewId: TBoldDefaultId;
begin
  for i := 0 to ObjectIdList.Count - 1 do
  begin
    if not ObjectIdLIst[i].IsStorable then
    begin
      NewId := TBoldDefaultId.CreateWithClassID(ObjectIdList[i].TopSortedIndex,
                                                ObjectIdList[i].TopSortedIndexExact);
      repeat
        NewId.AsInteger := random(maxint);
      until not assigned(ValueSpace.ObjectContentsByObjectId[NewId]) and
        (TranslationList.TranslateToOldId[NewId] = NewId);
      TranslationList.AddTranslation(ObjectIdList[i], NewId);
      NewId.Free;
    end;
  end;
end;

destructor TBoldPersistenceControllerFile.destroy;
begin
  FreeAndNil(fFreeStandingValueSpace);
  inherited;
end;

{ TBoldAbstractPersistenceHandleFile }

procedure TBoldAbstractPersistenceHandleFile.SetBoldModel(const Value: TBoldAbstractModel);
begin
  FBoldModel := Value;
end;

procedure TBoldAbstractPersistenceHandleFile.SetCacheData(const Value: Boolean);
begin
  FCacheData := Value;
end;

procedure TBoldAbstractPersistenceHandleFile.SetFileName(const Value: String);
begin
  FFileName := Value;
end;

end.
