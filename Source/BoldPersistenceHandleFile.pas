
{ Global compiler directives }
{$include bold.inc}
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
  BoldValueSpaceInterfaces,
  BoldElements;

type
  { forward declarations }
  TBoldAbstractPersistenceHandleFile = class;
  TBoldPersistenceControllerFile = class;

  { TBoldAbstractPersistenceHandleFile }
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
    constructor create(filename: string; CacheData: Boolean; MoldModel: TMoldModel);
    destructor Destroy; override;
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID); override;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMSetReadOnlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); override;
    function MultilinksAreStoredInObject: Boolean; override;
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList;
                    TranslationList: TBoldIdTranslationList); override;
    function CanEvaluateInPS(sOCL: string; aSystem: TBoldElement; aContext: TBoldElementTypeInfo = nil; const aVariableList: TBoldExternalVariableList = nil): Boolean; override;
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
  BoldCoreConsts;

{ TBoldPersistenceControllerFile }

function TBoldPersistenceControllerFile.CanEvaluateInPS(sOCL: string;
  aSystem: TBoldElement; aContext: TBoldElementTypeInfo;
  const aVariableList: TBoldExternalVariableList): Boolean;
const
  sMethodNotImplemented = '%s.%s: not supported/implemented';
begin
  raise EBold.CreateFmt(sMethodNotImplemented, [ClassName, 'CanEvaluateInPS']); // do not localize
end;

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
  procedure FixupValueSpace;
  begin
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
      FileAge(FileName, fFileTimestamp);
    end
    else
      fFileTimeStamp := 0;
  end;
end;

procedure TBoldPersistenceControllerFile.PMExactifyIds(
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean);
begin
  raise EBold.CreateFmt(sNotImplemented, [ClassName, 'PMExactifyIds']);
end;

procedure TBoldPersistenceControllerFile.PMFetch(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  MemberIdList: TBoldMemberIdList; FetchMode: Integer;
  BoldClientID: TBoldClientID);
begin
  EnsureValueSpace;
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
    ShowMessage('This filehandler does not understand SQL, ignoring condition and orderby...');
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
  raise EBold.CreateFmt(sNotImplemented, [ClassName, 'PMSetReadonlyness']);
end;

procedure TBoldPersistenceControllerFile.PMTranslateToGlobalIds(
  ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  raise EBold.CreateFmt(sNotImplemented, [ClassName, 'PMTranslateToGlobalIds']);
end;

procedure TBoldPersistenceControllerFile.PMTranslateToLocalIds(
  GlobalIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList);
begin
  raise EBold.CreateFmt(sNotImplemented, [ClassName, 'PMTranslateToLocalIds']);
end;

procedure TBoldPersistenceControllerFile.PMUpdate(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
  Old_Values: IBoldValueSpace;
  Precondition: TBoldUpdatePrecondition;
  TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID);
var
  NewTimeStamp: TDateTime;
  LocalTranslationList: TBoldIdTranslationList;
begin
  if assigned(Precondition) then
    raise EBold.CreateFmt('%s.PMUpdate: Preconditions (%s) not supported in this component', [Classname, Precondition.Classname]);

  EnsureValueSpace;

  if fileexists(Filename) then
    FileAge(FileName, NewTimeStamp)
  else
    NewTimeStamp := 0;
  TimeOfLatestUpdate := now;
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
  
  FileAge(FileName, fFileTimestamp);
end;

procedure TBoldPersistenceControllerFile.UnloadLocalValueSpace;
begin
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
