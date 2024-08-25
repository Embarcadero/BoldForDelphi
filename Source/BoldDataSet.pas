
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldDataSet;

interface

uses
  Classes,
  DB,
  BoldEnvironmentVCL,
  BoldSystem,
  BoldSystemRT,
  BoldElements,
  BoldSubscription,
  BoldAbstractListHandle,
  BoldListListControlPack,
  BoldControlPack,
  BoldControllerListControlPack,
  BoldStringControlPack,
  BoldListHandleFollower, ExtCtrls;

type
  TBDSBookmarkInfo = record
    Flag: TBookmarkFlag;
    Element: TBoldElement;
  end;

  PBDSBookmarkInfo = ^TBDSBookmarkInfo;

  TBoldAbstractDataSet = class;
  TBoldDataSet = class;
  TBoldDataSetFieldDescription = class;
  TBoldDataSetFieldDescriptions = class;

  { TBoldDataSetFieldDescription }
  TBoldDataSetFieldDescription = class(TCollectionItem)
  private
    fRTInfo: TBoldMemberRTInfo;
    fBoldProperties: TBoldStringFollowerController;
    fBufferSize: integer;
    fFieldName: string;
    fFieldOfs: integer;
    fFieldSize: integer;
    fFieldType: TFieldType;
    fRequired: boolean;
    fUseFieldSize: boolean;
    fInvalid: Boolean;
    function GetDataSet: TBoldAbstractDataSet;
    function GetExpression: string;
    function GetFieldDescriptions: TBoldDataSetFieldDescriptions;
    function GetListElementType: TBoldElementTypeInfo;
    procedure SetBoldProperties(AValue: TBoldStringFollowerController);
    procedure SetExpression(AValue: string);
    function GetFieldOfs: integer;
    function GetFieldSize: integer;
    function GetFieldType: TFieldType;
    function GetRequired: boolean;
    function GetRTInfo: TBoldMemberRTInfo;
    function GetUseFieldSize: boolean;
    procedure SetFieldName(const Value: String);
    function GetEffectiveFieldName: String;
    procedure Invalidate;
    procedure EnsureValid;
    procedure AfterMakeUptoDate(Follower: TBoldFollower);
    procedure DefineFieldType(aClass: TClass);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(AOwner: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property BufferSize: integer read FBuffersize;
    property EffectiveFieldName: String read GetEffectiveFieldName;
    property FieldSize: integer read GetFieldSize;
    property FieldType: TFieldType read GetFieldType;
    property FieldOfs: integer read GetFieldOfs;
    property ListelementType: TBoldElementTypeInfo read GetListelementType;
    property Required: boolean read GetRequired;
    property RTInfo: TBoldMemberRTInfo read GetRTInfo;
    property UseFieldSize: boolean read GetUseFieldSize;
    property Dataset: TBoldAbstractDataSet read GetDataset;
    property FieldDescriptions: TBoldDataSetFieldDescriptions read GetFieldDescriptions;
    property Expression: string read GetExpression write SetExpression;
  published
    property BoldProperties: TBoldStringFollowerController read FBoldProperties write SetBoldProperties;
    property FieldName: String read fFieldName write SetFieldName;
  end;

  { TBoldDataSetFieldDescriptions }
  TBoldDataSetFieldDescriptions = class(TCollection)
  private
    FDataset: TBoldAbstractDataSet;
    FFieldFollowers: TBoldControllerList;
    function GetItem(AIndex: Integer): TBoldDataSetFieldDescription;
  protected
    function GetOwner: TPersistent; override;
    property FieldFollowers: TBoldControllerList read FFieldFollowers;
  public
    constructor Create(AOwner: TBoldAbstractDataSet);
    destructor Destroy; override;
    function Add: TBoldDataSetFieldDescription;
    procedure InvalidateFields;
    property Items[index: Integer]: TBoldDataSetFieldDescription read GetItem; default;
    property Dataset: TBoldAbstractDataSet read FDataset;
  end;

  { TBoldAbstractDataSet }
  TBoldAbstractDataSet = class(TDataSet)
  private
    FTimer: TTimer;
    FAutoOpen: boolean;
    FBookmarkOfs: integer;
    FContextSubscriber: TBoldPassthroughSubscriber;
    FFieldDescriptions: TBoldDataSetFieldDescriptions;
    FLayoutChanged: boolean;
    FListFollowerController: TBoldListAsFollowerListController;
    FListFollower: TBoldListHandleFollower;
    FRecordPos: integer;
    FBufferSize: integer;
    FRecordSize: integer;
    fLastKnownType: TBoldElementTypeInfo;
    fInternalCursorReady: Boolean;
    function GetIndexForBookmark(Bookmark: Pointer): integer;
    procedure InsertElement(ABuffer: PChar); virtual;
    procedure _ReceiveContext(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure BoldToBuffer(ABuffer: PChar);
    procedure _InsertRow(Follower: TBoldFollower);
    procedure _DeleteRow(index: Integer; owningFollower: TBoldFollower);
    procedure BufferToBoldElement(AIndex: integer; ABuffer: PChar);
    procedure BoldElementToBuffer(AIndex: integer; ABuffer: PChar);
    function GetBookmarkInfo(Buffer: PChar): PBDSBookmarkInfo;
    function GetActiveBoldElement: TBoldElement;
    function GetActiveBoldObject: TBoldObject;
    procedure SetAutoOpen(AValue: boolean);
    function GetBoldRTInfo(AFieldName: string): TBoldMemberRTInfo;
    function GetBoldHandle: TBoldAbstractListHandle;
    procedure SetBoldHandle(NewValue: TBoldAbstractListHandle);
    procedure SetFieldDescriptions(const Value: TBoldDataSetFieldDescriptions);
    procedure EnsureRecordPosInValidRange;
    procedure TimerTrigger(Sender: TObject);
    procedure DoTimer;
  protected
    procedure Loaded; override;
    function CreateNewElement: TBoldElement; virtual;
    procedure TypeMayHaveChanged;
    procedure TypeHasChanged; virtual;
    procedure LayoutChanged;
    procedure ClearCalcFields(Buffer: PChar); override;
    procedure DoOnNewRecord; override;
    function GetActiveRecBuf(var RecBuf: PChar): Boolean;
    function GetListElementType: TBoldElementTypeInfo; virtual;
    procedure DeleteAllFields;
    { Mandatory overrides }
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure InternalInitRecord(Buffer: PChar); override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    procedure SetRecNo(Value: Integer); override;    
    function GetRecordSize: Word; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
    function LocateRecord(const KeyFields: string;
      const KeyValues: Variant; Options: TLocateOptions;
      SyncCursor: Boolean): Boolean;
    function ValuesInFields(AFields: TList; const KeyValues: Variant; Options: TLocateOptions): Boolean;
    { Optional overrides }
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure DoAfterScroll; override;
    function GetCanModify: boolean; override;
    procedure InternalEdit; override;
    procedure InternalCancel; override;
    function GetList: TBoldList;
    property List: TBoldList read GetList;
    property ListElementType: TBoldElementTypeInfo read GetListElementType;
    property BoldHandle: TBoldAbstractListHandle read GetBoldHandle write SetBoldHandle;
    property AutoOpen: boolean read FAutoOpen write SetAutoOpen;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    procedure CreateDefaultFields;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
    procedure TryToOpen;
    property ActiveBoldElement: TBoldElement read GetActiveBoldElement ;
    property ActiveBoldObject: TBoldObject read GetActiveBoldObject;
    property BoldRTInfo[AFieldName: string]: TBoldMemberRTInfo read GetBoldRTInfo;
  published
    property FieldDescriptions: TBoldDataSetFieldDescriptions read fFieldDescriptions write SetFieldDescriptions;
  end;

  { TBoldDataSet }
  TBoldDataSet = class(TBoldAbstractDataSet)
  published
    {$IFNDEF T2H}
    property Active;
    property AutoOpen;
    property BoldHandle;
    property BeforePost;
    property BeforeClose;
    property BeforeInsert;
    property AfterInsert;
    property AfterOpen;
    property BeforeScroll;
    property AfterScroll;
    property AfterCancel;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldAttributes,
  DBConsts,
  Variants,
  BoldEnvironment;


type
  TBoldConvInfo = record
    FBoldType: TClass;
    FFieldType: TFieldType;
    FBufferSize: integer;
    FUseSize: boolean;
    FUseDecimals: boolean;
  end;

const
  arConvInfo: array [0..14] of TBoldConvInfo =
    (
    (FBoldType: TBAString; FFieldType: ftString; FBufferSize: 255; FUseSize: True; FUseDecimals: False),
    (FBoldType: TBANumeric; FFieldType: ftInteger; FBufferSize: sizeof(integer); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBAInteger; FFieldType: ftInteger; FBufferSize: sizeof(integer); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBASmallInt; FFieldType: ftSmallint; FBufferSize: sizeof(smallint); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBAShortInt; FFieldType: ftSmallint; FBufferSize: sizeof(smallint); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBAWord; FFieldType: ftWord; FBufferSize: sizeof(word); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBAByte; FFieldType: ftSmallint; FBufferSize: sizeof(smallint); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBAFloat; FFieldType: ftFloat; FBufferSize: sizeof(double); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBACurrency; FFieldType: ftCurrency; FBufferSize: sizeof(double); FUseSize: False; FUseDecimals: True),
    (FBoldType: TBABlob; FFieldType: ftBlob; FBufferSize: 40; FUseSize: true; FUseDecimals: False),
    (FBoldType: TBADateTime; FFieldType: ftDateTime; FBufferSize: sizeof(TDateTime); FUseSize: False; FUseDecimals: False),
    (FBoldType: TBADate; FFieldType: ftDate; FBufferSize: SizeOf(Longint); FUseSize: False; FUseDecimals: False),
    (FBoldType: TBATime; FFieldType: ftTime; FBufferSize: SizeOf(Longint); FUseSize: False; FUseDecimals: False),
    (FBoldType: TBAValueSet; FFieldType: ftInteger; FBufferSize: sizeof(integer); FUseSize: false; FUseDecimals: False),
    (FBoldType: TBABoolean; FFieldType: ftBoolean; FBufferSize: sizeof(wordbool); FUseSize: false; FUseDecimals: False)
   );


{****************************************************************************
  TBoldDataSetFieldDescription
 ****************************************************************************}

constructor TBoldDataSetFieldDescription.Create(AOwner: TCollection);
begin
  Assert(AOwner is TBoldDataSetFieldDescriptions);
  inherited Create(AOwner);
  FBoldProperties := TBoldStringFollowerController.Create(nil);
  FBoldProperties.AfterMakeUptoDate := self.AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetListElementType;
  FieldDescriptions.FieldFollowers.Add(FBoldProperties);
  Invalidate;
end;

destructor TBoldDataSetFieldDescription.Destroy;
begin
  FieldDescriptions.FieldFollowers.Remove(FBoldProperties);
  FreeAndNil(FBoldProperties);
  inherited Destroy;
end;

procedure TBoldDataSetFieldDescription.DefineFieldType(aClass: TClass);
var
  i: integer;
begin
  FFieldType := ftString;
  FFieldSize := 255;
  FBufferSize := 255;
  FRequired := False;

  for i := high(arConvInfo) downto Low(arConvInfo) do    
  begin
    with arConvInfo[i] do
    begin
      if AClass.inheritsfrom(arConvInfo[i].FBoldType) then
      begin
        self.FFieldType := arConvInfo[i].FFieldType;
        self.FBufferSize := arConvInfo[i].FBufferSize;
        self.FUseFieldSize := arConvInfo[i].FUseSize;
        if FUseSize then
          self.FFieldSize := arConvInfo[i].FBufferSize
        else
          self.FFieldSize := 0;
        self.FRequired := False;
        break;
      end;
    end;
  end;
end;

procedure TBoldDataSetFieldDescription.AfterMakeUptoDate(Follower: TBoldFollower);
var
  LField: TField;
  ActiveBuf: PChar;

  function FindFieldNo(ANo: integer): TField;
  var
    FieldIx: integer;
  begin
    result := nil;
    for FieldIx := 0 to Dataset.FieldCount - 1 do
    begin
      if Dataset.Fields[FieldIx].FieldNo = ANo then
      begin
        result := Dataset.Fields[FieldIx];
        Exit;
      end;
    end;
  end;

begin
  with Dataset do
  begin
    if not Active then
      Exit;

    if not (Dataset.State in dsWriteModes) then
    begin
      DoTimer;
      //UpdateCursorPos;
      //Resync([]);
    end
    else
    begin
      if GetActiveRecBuf(ActiveBuf) and (GetBookmarkInfo(ActiveBuf)^.Element = Follower.OwningFollower.Element) then
      begin
        BoldElementToBuffer(Follower.Index, ActiveBuf);
        if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
        begin
          LField := FindFieldNo(Follower.Index + 1);
          if Assigned(LField) then
            DataEvent(deFieldChange, LongInt(LField));
        end;
        SetModified(True);
      end
    end
  end;
end;

function TBoldDataSetFieldDescription.GetEffectiveFieldName: String;
var
  i: integer;
begin
  if FieldName <> '' then
    result := FieldName
  else
  begin
    result := BoldProperties.Expression;
    for i := 1 to length(Result) do
      if Result[i] in ['.', '(', '-', '>', ')'] then
        result[i] := '_'
  end;
end;

procedure TBoldDataSetFieldDescription.EnsureValid;
var
  LFieldType: TBoldElementTypeInfo;
begin
  if fInvalid then
  begin
    if not Assigned(ListElementType) then
      raise EBold.CreateFmt('Cannot initialize fields - dataset %s has no context', [Dataset.Name]);

    //LFieldType := ListElementType.Evaluator.ExpressionType(Expression, ListElementType, false);
    LFieldType := ListElementType.Evaluator.ExpressionType(Expression,
      ListElementType, false, fBoldProperties.VariableList);
    if not Assigned(LFieldType) then
      raise Exception.CreateFmt('Invalid field expression %s', [Expression]);

    if ListElementType.Evaluator is TBoldRTEvaluator then
      fRTInfo := (ListElementType.Evaluator as TBoldRTEvaluator).RTInfo(Expression, ListElementType, false)
    else
      fRTInfo := nil;

    if LFieldType is TBoldAttributeTypeInfo then
    begin
      DefineFieldType(TBoldAttributeTypeInfo(LFieldType).AttributeClass);
      if (fRTInfo is TBoldAttributeRTInfo) and
         (TBoldAttributeRTInfo(FRTInfo).Length > 0) and
         FUseFieldSize then
      begin
        FBufferSize := TBoldAttributeRTInfo(FRTInfo).Length + 1;
        FFieldSize := FBufferSize - 1;
      end;
    end
    else if LFieldType is TBoldClassTypeInfo then
    begin
      FFieldType := ftInteger;
      FFieldSize := 0;
      FBufferSize := sizeof(integer);
      FRequired := False;
    end
    else
    begin
      FFieldType := ftString;
      FFieldSize := 40;
      FBufferSize := 40;
      FRequired := False;
    end;
    fInvalid := false;
  end;
end;

function TBoldDataSetFieldDescription.GetFieldDescriptions: TBoldDataSetFieldDescriptions;
begin
  result := TBoldDataSetFieldDescriptions(Collection);
end;

function TBoldDataSetFieldDescription.GetDataset: TBoldAbstractDataSet;
begin
  result := FieldDescriptions.Dataset;
end;

function TBoldDataSetFieldDescription.GetListElementType: TBoldElementTypeInfo;
begin
  result := Dataset.ListelementType;
end;

function TBoldDataSetFieldDescription.GetExpression: string;
begin
  result := FBoldProperties.Expression;
end;

procedure TBoldDataSetFieldDescription.SetExpression(AValue: string);
begin
  fBoldProperties.Expression := aValue;
  Invalidate;
end;

procedure TBoldDataSetFieldDescription.SetBoldProperties(AValue: TBoldStringFollowerController);
begin
  if (AValue <> FBoldProperties) and Assigned(AValue) then
    FBoldProperties.Assign(AValue);
  invalidate;
end;

procedure TBoldDataSetFieldDescription.Assign(Source: TPersistent);
begin
  if source is TBoldDataSetFieldDescription then
  begin
    self.BoldProperties.Assign(TBoldDataSetFieldDescription(Source).BoldProperties);
    self.FieldName := TBoldDataSetFieldDescription(Source).FieldName;
    Invalidate;
  end;
end;

procedure TBoldDataSetFieldDescription.Invalidate;
begin
  fInvalid := true;
end;

function TBoldDataSetFieldDescription.GetFieldOfs: integer;
begin
  EnsureValid;
  Result := FFieldOfs;
end;

function TBoldDataSetFieldDescription.GetFieldSize: integer;
begin
  EnsureValid;
  REsult := fFieldSize;
end;

function TBoldDataSetFieldDescription.GetFieldType: TFieldType;
begin
  EnsureValid;
  result := fFieldType;
end;

function TBoldDataSetFieldDescription.GetRequired: boolean;
begin
  Ensurevalid;
  result := fRequired;
end;

function TBoldDataSetFieldDescription.GetRTInfo: TBoldMemberRTInfo;
begin
  EnsureValid;
  result := fRTInfo;
end;

function TBoldDataSetFieldDescription.GetUseFieldSize: boolean;
begin
  EnsureValid;
  result := fUseFieldSize;
end;

procedure TBoldDataSetFieldDescription.SetFieldName(const Value: String);
begin
  fFieldName := Value;
  Invalidate;
end;

function TBoldDataSetFieldDescriptions.GetOwner: TPersistent;
begin
  result := DataSet;
end;

function TBoldDataSetFieldDescription.GetDisplayName: string;
begin
  result := EffectiveFieldName + ' = ' + Expression;
end;

{****************************************************************************
  TBoldDataSetFieldDescriptions
****************************************************************************}

constructor TBoldDataSetFieldDescriptions.Create(AOwner: TBoldAbstractDataSet);
begin
  inherited Create(TBoldDataSetFieldDescription);
  FFieldFollowers := TBoldControllerList.Create(nil);
  FDataset := AOwner;
end;

destructor TBoldDataSetFieldDescriptions.Destroy;
begin
  Clear;
  FreeAndNil(FFieldFollowers);
  inherited Destroy;
end;

function TBoldDataSetFieldDescriptions.Add: TBoldDataSetFieldDescription;
begin
  result := TBoldDataSetFieldDescription(inherited Add);




end;

function TBoldDataSetFieldDescriptions.GetItem(AIndex: Integer): TBoldDataSetFieldDescription;
begin
  result := TBoldDataSetFieldDescription(inherited Items[AIndex]);
end;

procedure TBoldDataSetFieldDescriptions.InvalidateFields;
var
  i: integer;
begin
  for i := 0 to count - 1 do
    items[i].Invalidate;
end;

{****************************************************************************
  TBoldAbstractDataSet
 ****************************************************************************}

constructor TBoldAbstractDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTimer:= TTimer.Create(nil);
  fTimer.Interval := 100;
  fTimer.Enabled:= False;
  fTimer.OnTimer:= TimerTrigger;
  fLastKnownType := nil;
  fInternalCursorReady := false;
  FContextSubscriber := TBoldPassthroughSubscriber.Create(_ReceiveContext);

  FFieldDescriptions := TBoldDataSetFieldDescriptions.Create(self);
  FLayoutChanged := True;

  FListFollowerController := TBoldListAsFollowerListController.Create(self, FFieldDescriptions.FieldFollowers);
  FListFollowerController.OnAfterInsertItem := _InsertRow;
  FListFollowerController.OnAfterDeleteItem := _DeleteRow;
  FListFollowerController.OnGetContextType := GetListElementType;

  FListFollower := TBoldListHandleFollower.Create(Owner, FListFollowerController);
  FRecordSize := 0;
  FRecordPos := -1;
end;

destructor TBoldAbstractDataSet.Destroy;
begin
  Close;
  FreeAndNil(fTimer);
  FRecordPos := -1;
  FreeAndNil(FContextSubscriber);
  FreeAndNil(FListFollower);
  FreeAndNil(FListFollowerController);
  FreeAndNil(FFieldDescriptions);
  inherited Destroy;
end;

function TBoldAbstractDataSet.GetListElementType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

procedure TBoldAbstractDataSet.SetAutoOpen(AValue: boolean);
begin
  if (FAutoOpen <> AValue) then
  begin
    FAutoOpen := AValue;
    if FAutoOpen then
      TryToOpen;
  end;    
end;

procedure TBoldAbstractDataSet.TryToOpen;
begin
  if not (csLoading in ComponentState) and
     not (csDestroying in ComponentState) and 
     not Active then
  try
    Open;
  except
  end;
end;

procedure TBoldAbstractDataSet.DoOnNewRecord;
begin
  InsertElement(ActiveBuffer);
  inherited DoOnNewRecord;
end;

function TBoldAbstractDataSet.CreateNewElement: TBoldElement;
begin
  result := nil;
  if assigned(BoldHandle) and
     assigned(BoldHandle.List) and
     BoldHandle.List.CanCreateNew then
    result := BoldHandle.List.AddNew;
end;

procedure TBoldAbstractDataSet.InsertElement(ABuffer: PChar);
var
  LElement: TBoldElement;
begin
  lElement := CreateNewElement;
  if not Assigned(LElement) then
    Abort;

  fRecordPos := List.IndexOf(LElement);

  with GetBookmarkInfo(ABuffer)^ do
  begin
    Flag := bfInserted;
    Element := LElement;
  end;
  BoldToBuffer(ABuffer);
end;

function TBoldAbstractDataSet.GetBoldRTInfo(AFieldName: string): TBoldMemberRTInfo;
var
 FieldDesc: TBoldDataSetFieldDescription;
 i: integer;
begin
  InternalInitFieldDefs;
  FieldDesc := nil;
  for i := 0 to FieldDescriptions.Count - 1 do
  begin
    if FieldDescriptions[i].EffectiveFieldName = AFieldName then
    begin
      FieldDesc := FieldDescriptions.Items[i];
      break;
    end;
  end;

  if Assigned(FieldDesc) then
    result := FieldDesc.RTInfo
  else
    raise Exception.CreateFmt('Invalid fieldname %s', [AFieldName]);
end;

procedure TBoldAbstractDataSet.EnsureRecordPosInValidRange;
begin
  if assigned(list) then
  begin
    if fRecordPos > List.Count-1 then
      fRecordPos := List.Count-1
  end
  else
    fRecordPos := -1;
end;

procedure TBoldAbstractDataSet._ReceiveContext(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    beDestroying: Close;
    beValueIdentityChanged:
    begin
      TypeMayHaveChanged;
      EnsureRecordPosInValidRange;
    end;
    beItemDeleted: EnsureRecordPosInValidRange;
  end; 
end;

function TBoldAbstractDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
var
  BoldDataSetFieldDescription: TBoldDataSetFieldDescription;
  BlobValue: TBABlob;
  IndirectElement: TBoldIndirectElement;
begin
  if IsEmpty then
    Result := TMemoryStream.Create
  else
  begin
    IndirectElement := TBoldIndirectElement.Create;
    try
      BoldDataSetFieldDescription := FieldDescriptions[Field.FieldNo - 1];
      //List[RecNo - 1].EvaluateExpression(BoldDataSetFieldDescription.Expression, IndirectElement);
      List[RecNo - 1].EvaluateExpression(BoldDataSetFieldDescription.Expression,
        IndirectElement, false, BoldDataSetFieldDescription.fBoldProperties.VariableList);
      BlobValue := (IndirectElement.Value as TBABlob);
      if not assigned(BlobValue) or BlobValue.IsNull then
        result := TMemoryStream.Create
      else
        result := BlobValue.CreateBlobStream(TBoldBlobStreamMode(Mode));
    finally
      IndirectElement.Free;
    end;
  end;
end;

procedure TBoldAbstractDataSet.InternalEdit;
begin
  inherited InternalEdit;
end;

function TBoldAbstractDataSet.GetCanModify: boolean;
begin
  result := True;
end;

procedure TBoldAbstractDataSet.InternalCancel;
begin
  if Assigned(ActiveBoldObject) then
    ActiveBoldObject.Discard;
{CHECKME Is this needed???

  if State = dsInsert then
  begin
    if SQL then
      self.FListHandle.RemoveCurrentElement
  end;
}
  inherited;
end;

function TBoldAbstractDataSet.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
  Result := null;
  if LocateRecord(KeyFields, KeyValues, [loCaseInsensitive], false) then
  begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TempBuffer);
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

procedure TBoldAbstractDataSet.DoAfterScroll;
begin
  if (FListFollower.Follower.CurrentIndex <> RecNo - 1) then
    fListFollower.SetFollowerIndex(RecNo - 1);  
  inherited ;
end;

function TBoldAbstractDataSet.ValuesInFields(AFields: TList; const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  i: Integer;

  function CompareField(AField: TField; AValue: variant): boolean;
  begin
    if (loPartialKey in Options) then
    begin
      if (loCaseInsensitive in Options) then
        Result := AnsiStrLIComp(PChar(AField.AsString),
                                PChar(VarToStr(AValue)),
                                Length(AValue)) = 0
      else
        Result := AnsiStrLComp(PChar(AField.AsString),
                               PChar(VarToStr(AValue)),
                               Length(AValue)) = 0
    end
    else
    begin
      if loCaseInsensitive  in Options then
        Result := AnsiStrIComp(PChar(AField.AsString),
                               PChar(VarToStr(AValue))) = 0
      else
        Result := AnsiStrComp(PChar(AField.AsString),
                              PChar(VarToStr(AValue))) = 0
    end;
  end;

begin
  if AFields.count = 1 then
    result := CompareField(TField(AFields[0]), KeyValues)
  else
  begin
    Result := True;
    for i := 0 to AFields.count - 1 do
    begin
      Result := Result and CompareField(TField(AFields[i]), KeyValues[i]);
      if not Result then
        Exit;
    end;
  end;
end;
function TBoldAbstractDataSet.LocateRecord(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions;
  SyncCursor: Boolean): Boolean;
var
  LFields: TList;
  Locator: TBoldObjectLocator;
  LocatorPtr: pointer;
  LElement: TBoldElement;
  i: Integer;
begin
  result := false;
  if BoldHandle.Count = 0 then
    Exit;

  CheckBrowseMode;
  CursorPosChanged;
  SetTempState(dsFilter);
  try
    if UpperCase(KeyFields) = 'SELF' then
    begin
      LocatorPtr := Pointer(Integer(KeyValues));
      if Assigned(LocatorPtr) then
      begin
        try
          Locator := TObject(LocatorPtr) as TBoldObjectLocator;
          LElement := Locator.BoldObject;
          with GetBookmarkInfo(TempBuffer)^ do
          begin
            Element := LElement;
            Flag := bfCurrent;
          end;
          BoldToBuffer(TempBuffer);
          FRecordPos := List.IndexOf(LElement);
          Result := FRecordPos <> -1;
        except
        end
      end
    end
    else
    begin
      LFields := TList.Create;
      try
        GetFieldList(LFields, KeyFields);
        for i := 0 to List.Count - 1 do
        begin
          with GetBookmarkInfo(TempBuffer)^ do
          begin
            Element := List[i];
            Flag := bfCurrent;
          end;

          BoldToBuffer(TempBuffer);
          if ValuesInFields(LFields, KeyValues, Options) then
          begin
            FRecordPos := i;
            Result := True;
            break;
          end;
        end;
      finally
        LFields.Free;
      end;
    end;
  finally
    RestoreState(dsBrowse);
  end;
end;

function TBoldAbstractDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := LocateRecord(KeyFields, KeyValues, Options, True);
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TBoldAbstractDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := GetIndexForBookmark(Bookmark) <> -1;
end;

procedure TBoldAbstractDataSet.BoldToBuffer(ABuffer: PChar);
var
  i: integer;
begin
  for i := 0 to FieldDescriptions.Count - 1 do
    BoldElementToBuffer(i, ABuffer);

  GetCalcFields(ABuffer);
end;

procedure TBoldAbstractDataSet._DeleteRow(index: Integer; owningFollower: TBoldFollower);
begin
  if not Active then
    Exit;

  if State = dsInsert then
    Exit;
  UpdateCursorPos;  
  if BoldHandle.CurrentElement <> ActiveBoldElement then
    if (State in dsWriteModes) then
      Cancel
    else
      Resync([])
  else
    if not (State in dsWriteModes) then
      Resync([]);

end;

procedure TBoldAbstractDataSet._InsertRow(Follower: TBoldFollower);
begin
  if State in [dsInsert, dsInactive] then
    Exit;
  if (State in dsWriteModes) then
    UpdateRecord;
  DoTimer;
  //UpdateCursorPos;
  //Resync([]);
end;

function TBoldAbstractDataSet.GetList: TBoldList;
begin
  if Assigned(BoldHandle) then
    result := BoldHandle.List
  else
    Result := nil;
end;

procedure TBoldAbstractDataSet.BufferToBoldElement(AIndex: integer; ABuffer: PChar);
var
  Locator: TBoldObjectLocator;
  LocatorPointer: pointer;
  LFieldAdr: pointer;
  LFieldDescr: TBoldDataSetFieldDescription;
  IndirectElement: TBoldIndirectElement;
  LValue: TBoldElement;
  LActiveElement: TBoldElement;
begin
  LFieldDescr := FieldDescriptions[AIndex];

  with LFieldDescr do
  begin
    LFieldAdr := ABuffer + FieldOfs;

    IndirectElement := TBoldIndirectElement.Create;
    try
      LValue := nil;
      try
        LActiveElement := GetBookmarkInfo(ABuffer)^.Element;

        //LActiveElement.EvaluateExpression(Expression, IndirectElement);
        LActiveElement.EvaluateExpression(Expression, IndirectElement, False,
          fBoldProperties.VariableList);
        LValue := IndirectElement.Value;
      except
      end;

      if Assigned(LValue) and LValue.Mutable then
      case FieldType of
        ftBoolean: begin
          if Boolean(LFieldAdr^) then
            LValue.AsString := 'Y'
          else
            LValue.AsString := 'N'
        end;

        ftString: begin
          LValue.AsString := Copy(PChar(LFieldAdr), 1, BufferSize - 1);
        end;

        ftInteger: begin
          if LValue is TBoldObjectReference then
          begin
            if Integer(LFieldAdr^) <> 0 then
            begin
              LocatorPointer := Pointer(LFieldAdr^);
              if Assigned(LocatorPointer) then
              begin
                Locator := TObject(LocatorPointer) as TBoldObjectLocator;
                Locator.EnsureBoldObject;
                TBoldObjectReference(LValue).BoldObject := Locator.BoldObject;
              end
              else
                TBoldObjectReference(LValue).BoldObject := nil;
            end
            else
              TBoldObjectReference(LValue).BoldObject := nil;
          end
          else if lValue is TBAValueSet then
            (lValue as TBAValueSet).AsInteger := Integer(LFieldAdr^)
          else
            LValue.AsString := IntToStr(Integer(LFieldAdr^));
        end;

        ftCurrency,
        ftFloat: begin
          LValue.AsString := FloatToStr(Double(LFieldAdr^));
        end;

        ftTime:
          if Double(LFieldAdr^) <> 0 then
            LValue.AsString := TimeToStr(TimeStampToDateTime(MSecsToTimeStamp(Double(LFieldAdr^))))
          else
            LValue.AsString := '';

        ftDate:
          if Double(LFieldAdr^) <> 0 then
            LValue.AsString := DateToStr(TimeStampToDateTime(MSecsToTimeStamp(Double(LFieldAdr^))))
          else
            LValue.AsString := '';

        ftDateTime:
          if Double(LFieldAdr^) <> 0 then
            LValue.AsString := DateTimeToStr(TimeStampToDateTime(MSecsToTimeStamp(Double(LFieldAdr^))))
          else
            LValue.AsString := '';
      end;
    finally
      IndirectElement.Free;
    end;
  end;
end;

procedure TBoldAbstractDataSet.BoldElementToBuffer(AIndex: integer; ABuffer: PChar);
var
  LDateTime: TDateTime;
  LTimeStamp: TTimeStamp;
  LInteger: integer;
  LBoolean: boolean;
  LFloat: double;
  LFieldAdr: pointer;
  IndirectElement: TBoldIndirectElement;
  LActiveElement: TBoldElement;
  LFieldDescr: TBoldDataSetFieldDescription;
  Locator: TBoldObjectLocator;
  LString: string;
begin
  LFieldDescr := FieldDescriptions[AIndex];

  with LFieldDescr do
  begin
    LFieldAdr := ABuffer + FieldOfs;

    IndirectElement := TBoldIndirectElement.Create;
    try
      FillChar(LFieldAdr^, BufferSize, 0);
      try
        LActiveElement := GetBookmarkInfo(ABuffer)^.Element;
        //LActiveElement.EvaluateExpression(Expression, IndirectElement);
        LActiveElement.EvaluateExpression(Expression, IndirectElement, False,
          fBoldProperties.VariableList);
        if Assigned(IndirectElement.Value) then
          case FieldType of
            ftBoolean: begin
              if IndirectElement.Value.AsString = 'Y' then
                LBoolean := True
              else
                LBoolean := False;
              Move(LBoolean, LFieldAdr^, SizeOf(LBoolean));
            end;

            ftString: begin
              StrLCopy(LFieldAdr, PChar(IndirectElement.Value.AsString), BufferSize - 1);
            end;

            ftInteger: begin
              if IndirectElement.Value is TBoldObjectReference then
              begin
                Locator := TBoldObjectReference(IndirectElement.Value).Locator;
                Move(Integer(Pointer(Locator)), LFieldAdr^, BufferSize);
              end
              else if IndirectElement.Value is TBoldObject then
              begin
                Locator := TBoldObject(IndirectElement.Value).BoldObjectLocator;
                Move(Integer(Pointer(Locator)), LFieldAdr^, BufferSize);
              end
              else if IndirectElement.Value is TBAValueSet then
              begin
                if (IndirectElement.Value as TBAValueSet).IsNull then
                  LInteger := 0
                else
                  lInteger := (IndirectElement.Value as TBAValueSet).AsInteger;
                Move(LInteger, LFieldAdr^, BufferSize);
              end
              else
              begin
                if IndirectElement.Value.AsString = '' then
                  LInteger := 0
                else
                  LInteger := StrToInt(IndirectElement.Value.AsString);
                Move(LInteger, LFieldAdr^, BufferSize);
              end;
            end;

            ftCurrency,
            ftFloat: begin
              LFloat := StrToFloat(IndirectElement.Value.AsString);
              Move(LFloat, LFieldAdr^, BufferSize);
            end;

            ftDate: begin
              LTimeStamp.Date := 0;
              if IndirectElement.Value is TBADate then
                LTimeStamp := DateTimeToTimeStamp((IndirectElement.value as TBADate).asDate)
              else
              begin
                LString := Trim(IndirectElement.Value.AsString);
                if LString <> '' then
                  LTimeStamp := DateTimeToTimeStamp(StrToDate(IndirectElement.Value.AsString));
              end;
              Move(LTimeStamp.Date, LFieldAdr^, BufferSize);
            end;

            ftTime: begin
              LTimeStamp.Time := 0;
              if IndirectElement.value is TBATime then
                lTimeStamp := DateTimeToTimeStamp((IndirectElement.value as TBATime).asTime)
              else
              begin
                LString := Trim(IndirectElement.Value.AsString);
                if LString <> '' then
                  LTimestamp := DateTimeToTimeStamp(StrToTime(IndirectElement.Value.AsString));
              end;
              Move(LTimeStamp.Time, LFieldAdr^, BufferSize);
            end;

            ftDateTime: begin
              lFloat := 0;
              if IndirectElement.Value is TBADateTime then
              begin
                lDateTime := (IndirectElement.value as TBADateTime).asDateTime;
                LFloat := TimeStampToMSecs(DateTimeToTimeStamp(LDateTime));
              end
              else
              begin
                LString := Trim(IndirectElement.Value.AsString);
                if LString <> '' then
                begin
                  LDateTime := StrToDateTime(IndirectElement.Value.AsString);
                  LFloat := TimeStampToMSecs(DateTimeToTimeStamp(LDateTime));
                end
              end;
              Move(LFloat, LFieldAdr^, BufferSize);
            end;

            ftBlob,
            ftMemo: begin
              StrLCopy(LFieldAdr, PChar(IndirectElement.Value.AsString), BufferSize - 1);
            end;
          end;
      except
      end;
    finally
      IndirectElement.Free;
    end;
  end;
end;

function TBoldAbstractDataSet.GetActiveBoldElement: TBoldElement;
var
  ActiveBuf: PChar;
begin
  if Active and GetActiveRecBuf(ActiveBuf) then
    result := GetBookmarkInfo(ActiveBuf)^.Element
  else
    result := nil;
end;

function TBoldAbstractDataSet.GetActiveBoldObject: TBoldObject;
begin
  result := (ActiveBoldElement as TBoldObject);
end;

function TBoldAbstractDataSet.GetBookmarkInfo(Buffer: PChar): PBDSBookmarkInfo;
begin
  result := PBDSBookmarkInfo(Buffer + FBookmarkOfs);
end;

function TBoldAbstractDataSet.AllocRecordBuffer: PChar;
begin
  GetMem(Result, FBufferSize);
end;

procedure TBoldAbstractDataSet.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeMem(Buffer);
end;


procedure TBoldAbstractDataSet.InternalInitRecord(Buffer: PChar);
begin
  FillChar(Buffer^, FBufferSize, 0);
end;















function TBoldAbstractDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  Result := grError;
  if not Assigned(List) then
  begin
    Result := grEOF;
    exit;
  end;

  if RecordCount = 0 then
    Exit;

  result := grOk;
  case GetMode of
    gmCurrent: begin
      if (FRecordPos < 0) or (FRecordPos >= RecordCount) then
        result := grError;
    end;

    gmNext: begin
      if FRecordPos >= RecordCount - 1 then
      begin
        result := grEof;
      end
      else
        Inc(FRecordPos, 1);
    end;

    gmPrior: if FRecordPos <= 0 then
      begin
        result := grBof;
        FRecordPos := -1;
      end
      else
        Dec(FRecordPos, 1);
  end;

  if result = grOk then
  begin
    GetCalcFields(Buffer);
    with GetBookmarkInfo(Buffer)^ do
    begin
      Element := List[FRecordPos];
      Flag := bfCurrent;
    end;
    BoldToBuffer(Buffer);
  end
  else
    if (result = grError) and (DoCheck) then
      DatabaseError('No records found!');
end;



function TBoldAbstractDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TBoldAbstractDataSet.GetActiveRecBuf(var RecBuf: PChar): Boolean;
begin
  case State of
    dsBrowse: if IsEmpty then
      RecBuf := nil
    else
      RecBuf := ActiveBuffer;

    dsEdit,
    dsInsert: RecBuf := ActiveBuffer;

    dsSetKey: RecBuf := nil;

    dsCalcFields: RecBuf := CalcBuffer;

    dsFilter: RecBuf := TempBuffer;

    dsNewValue: RecBuf := nil;

    dsOldValue: RecBuf := nil;
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

procedure TBoldAbstractDataSet.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[RecordSize], CalcFieldsSize, 0);
end;

function TBoldAbstractDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  LActiveElement: TBoldElement;
  IndirectElement: TBoldIndirectElement;
  LRecBuf: PChar;
  Attr: TBoldAttribute;
begin
  Result := False;
  if not GetActiveRecBuf(LRecBuf) then
    Exit;

  with Field do
  begin
    if FieldNo > 0 then
    begin
      with FieldDescriptions[Field.FieldNo - 1] do
      begin
        IndirectElement := TBoldIndirectElement.Create;
        try
          LActiveElement := GetBookmarkInfo(LRecBuf)^.Element;
          //LActiveElement.EvaluateExpression(Expression, IndirectElement);
          LActiveElement.EvaluateExpression(Expression, IndirectElement, False,
            fBoldProperties.VariableList);
          if Assigned(IndirectElement.Value) and (IndirectElement.Value is TBoldAttribute) then
          begin
            Attr := TBoldAttribute(IndirectElement.Value);
            result := not Attr.IsNull;
            if Assigned(Buffer) and (Field.DataType=ftVariant) then
              Variant(Buffer^) := IndirectElement.Value.GetAsVariant;
            Field.readOnly := not Attr.CanModify;
          end
          else
            result := True;
        finally
          IndirectElement.Free;
        end;
        if Assigned(Buffer) then
        begin
          Move((LRecBuf + FieldOfs)^, Buffer^ , Field.Datasize);
        end;
      end;
    end
    else
    begin
      if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
      begin
        Inc(LRecBuf, FRecordSize + Offset);
        Result := Boolean(LRecBuf[0]);
        if Result and (Buffer <> nil) then
          Move(LRecBuf[1], Buffer^, DataSize);
      end;
    end;
    if (DataType = ftString) and (fRecordSize > 30) then
      DisplayWidth := 30;
  end;
end;

procedure TBoldAbstractDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  LRecBuf: PChar;
begin
  with Field do
  begin
    if not (State in dsWriteModes) then
      DatabaseError(SNotEditing);

    if not GetActiveRecBuf(LRecBuf) then
      Exit;

    if FieldNo > 0 then
    begin
      if State = dsCalcFields then
        DatabaseError(SNotEditing);
      if ReadOnly and not (State in [dsSetKey, dsFilter]) then
        DatabaseErrorFmt(SFieldReadOnly, [DisplayName]);
      Validate(Buffer);
      if FieldKind <> fkInternalCalc then
      begin
        with FieldDescriptions[Field.FieldNo - 1] do
        begin
          if not Assigned(Buffer) then
            FillChar((LRecBuf + FieldOfs)^, BufferSize, #0)
          else
            Move(Buffer^, (LRecBuf + FieldOfs)^, BufferSize);
        end;
        BufferToBoldElement(Field.FieldNo - 1, LRecBuf);
      end;
    end
    else {fkCalculated, fkLookup}
    begin
      Inc(LRecBuf, FRecordSize + Offset);
      Boolean(LRecBuf[0]) := LongBool(Buffer);
      if Boolean(LRecBuf[0]) then
        Move(Buffer^, LRecBuf[1], DataSize);
    end;
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Longint(Field));
  end;
end;


procedure TBoldAbstractDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  TBoldElement(Data^) := GetBookmarkInfo(Buffer)^.Element;
end;








function TBoldAbstractDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := GetBookmarkInfo(Buffer)^.Flag;
end;
procedure TBoldAbstractDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  GetBookmarkInfo(Buffer)^.Flag := Value;
end;

function TBoldAbstractDataSet.GetIndexForBookmark(Bookmark: Pointer): integer;
begin
  //if Assigned(Bookmark) then
  if Assigned(Bookmark) and Assigned(List) then
    try
      result := List.IndexOf(TBoldElement(Bookmark^))
    except
      result := -1;
    end
  else
    result := -1;
end;
procedure TBoldAbstractDataSet.InternalGotoBookmark(Bookmark: Pointer);
var
  LIndex: integer;
begin
  LIndex := GetIndexForBookmark(Bookmark);
  if LIndex >= 0 then
    FRecordPos := LIndex;  
end;

procedure TBoldAbstractDataSet.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(@GetBookmarkInfo(Buffer)^.Element); 
end;

procedure TBoldAbstractDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  GetBookmarkInfo(Buffer)^.Element := Data;
end;
procedure TBoldAbstractDataSet.InternalFirst;
begin
  FRecordPos := -1;
end;
procedure TBoldAbstractDataSet.InternalLast;
begin
  FRecordPos := List.Count;
end;



procedure TBoldAbstractDataSet.InternalClose;
begin
  BindFields(False);
  if DefaultFields then DestroyFields;
  fInternalCursorReady := false;
end;

procedure TBoldAbstractDataSet.InternalHandleException;
begin
  BoldEffectiveEnvironment.HandleDesigntimeException(Self);
end;
procedure TBoldAbstractDataSet.InternalDelete;
begin
  ActiveBoldObject.Delete;
  if List.Count = 0 then
    FRecordPos := -1;  
end;



procedure TBoldAbstractDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
  RecPos: Integer;
begin
  if Append then
  begin
    List.AddNew;
    InternalLast;
  end
  else
  begin
    if FRecordPos = -1 then
      RecPos := 0
    else
      RecPos := FRecordPos;
    List.InsertNew(RecPos);
  end;
end;





procedure TBoldAbstractDataSet.InternalOpen;
begin
  fInternalCursorReady := Assigned(BoldHandle) and BoldHandle.Enabled;
  if not fInternalCursorReady then Exit;
  FRecordPos := -1;

  if Assigned(BoldHandle.ListElementType) then
    InternalInitFieldDefs;
  if DefaultFields then CreateFields;

  BindFields(True);

  BookmarkSize := SizeOf(TBDSBookmarkInfo);
  FBookmarkOfs := FRecordSize + CalcFieldsSize;
  FBufferSize := FBookmarkOfs + BookmarkSize;
end;

procedure TBoldAbstractDataSet.InternalPost;
begin
  FRecordPos := List.IndexOf(ActiveBoldElement);
  if (FRecordPos <> FListFollower.Follower.CurrentIndex) then
  begin
    FListFollower.SetFollowerIndex(FRecordPos);
    CursorPosChanged;
  end;
end;



function TBoldAbstractDataSet.IsCursorOpen: Boolean;
begin
  Result := fInternalCursorReady ;
end;








function TBoldAbstractDataSet.GetRecordCount: Integer;
begin
  if not Assigned(List) then
    result := 0
  else
    Result := List.Count;
end;

function TBoldAbstractDataSet.GetRecNo: Integer;
var
  ActiveBuf: PChar;
  LIndex: integer;
begin
  Result := 0;
  if GetActiveRecBuf(ActiveBuf) then
  begin
    LIndex := List.IndexOf(GetBookmarkInfo(ActiveBuf)^.Element);
    if (LIndex >= 0) and (LIndex < RecordCount) then
      Result :=  LIndex + 1;
  end;
end;

procedure TBoldAbstractDataSet.InternalInitFieldDefs;
var
  i: integer;
begin
  FRecordSize := 0;
  FieldDefs.Clear;

  for i := 0 to FieldDescriptions.Count - 1 do
  begin
    with FieldDescriptions[i] do
    begin
      TFieldDef.Create(FieldDefs, EffectiveFieldName, FieldType, FieldSize, Required, i + 1);
      fFieldOfs := fRecordSize;
      fRecordSize := fRecordSize + BufferSize;
    end;
  end;
end;

procedure TBoldAbstractDataSet.LayoutChanged;
var
  WasActive: boolean;
begin
  WasActive := Active;
  FLayoutChanged := True;
  if WasActive then
    Close;

  FieldDescriptions.InvalidateFields;

  if (WasActive or AutoOpen) and not (csDestroying in componentState) then
    TryToOpen;
end;

function TBoldAbstractDataSet.GetBoldHandle: TBoldAbstractListHandle;
begin
  result := FListFollower.BoldHandle;
end;

procedure TBoldAbstractDataSet.SetBoldHandle(NewValue: TBoldAbstractListHandle);
begin
  if fListFollower.BoldHandle <> NewValue then
  begin
    fListFollower.BoldHandle := NewValue;
    fContextSubscriber.CancelAllSubscriptions;
    if assigned(NewValue) then
    begin
      NewValue.addsubscription(fContextSubscriber, beDestroying, beDestroying);
      NewValue.addsubscription(fContextSubscriber, beValueIdentityChanged, beValueIdentityChanged);
      if assigned(NewValue.List) then
        NewValue.List.addsubscription(fContextSubscriber, beItemDeleted, beItemDeleted);
    end;
  end;
end;

procedure TBoldAbstractDataSet.SetFieldDescriptions(const Value: TBoldDataSetFieldDescriptions);
begin
  FieldDescriptions.Assign(Value);
end;

procedure TBoldAbstractDataSet.TypeMayHaveChanged;
var
  NewType: TBoldElementTypeInfo;
begin
  NewType := ListElementType;
  if (NewType <> fLastKnownType) then
  begin
    fLastKnownType := NewType;
    TypeHasChanged;
  end;
end;

procedure TBoldAbstractDataSet.TypeHasChanged;
begin
  LayOutChanged;
end;

procedure TBoldAbstractDataSet.Loaded;
begin
  if not (csdesigning in ComponentState) and (FieldDescriptions.Count = 0) then
    CreateDefaultFields; 
  inherited;
  if AutoOpen and not Active then
    TryToOpen;
end;

procedure TBoldAbstractDataSet.CreateDefaultFields;
var
  i: integer;
  NewField: TBoldDataSetFieldDescription;
begin
  try
    DeleteAllFields;
    if Assigned(ListElementType) then
    begin
      if (ListElementType is TBoldClassTypeInfo) then
      begin
        with ListElementType as TBoldClassTypeInfo do
        begin
          for i := 0 to AllMembers.Count - 1 do
            if AllMembers[i].IsAttribute then
            begin
              NewField := FieldDescriptions.Add;
              NewField.Boldproperties.Expression := AllMembers[i].ExpressionName;
            end;
        end;
      end
      else if (ListElementType is TBoldAttributeTypeInfo) then
      begin
        FieldDescriptions.Add;
      end
      else if (ListElementType is TBoldListTypeInfo) then
      begin
        FieldDescriptions.Add;
      end;
    end;
  finally
    LayoutChanged;
  end;
end;

procedure TBoldAbstractDataSet.DeleteAllFields;
begin
  while FieldDescriptions.Count > 0 do
    FieldDescriptions.Delete(0);
end;

procedure TBoldAbstractDataSet.SetRecNo(Value: Integer);
begin
end;

procedure TBoldAbstractDataSet.DoTimer;
begin
  if not fTimer.Enabled then
  begin
    fTimer.Tag:= 0;
    TimerTrigger(nil);
    fTimer.Tag:= 1;
  end
  else
    fTimer.Tag:= 0;
  fTimer.Enabled:= false;
  fTimer.Enabled:= true;
end;

procedure TBoldAbstractDataSet.TimerTrigger(Sender: TObject);
begin
  fTimer.Enabled:= False;
  if fTimer.Tag = 1 then Exit;
  UpdateCursorPos;
  Resync([]);
end;


end.