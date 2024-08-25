
{ Global compiler directives }
{$include bold.inc}
unit BoldExternalPersistenceControllerDataSet;

interface

uses
  DB,
  Classes,
  BoldSubscription,
  BoldId,
  BoldValueSpaceInterfaces,
  BoldTypeNameDictionary,
  BoldMeta,
  BoldAbstractPartiallyExternalPC,
  BoldExternalPersistenceConfigItemDataSet;

type
  TBoldExternalPersistenceControllerDataSet = class(TBoldAbstractPartiallyExternalPC)
  private
    FConfig: TBoldExternalPersistenceConfigDataSetItems;
    fMaxFetchBlockSize: integer;
  protected
    function LocateInDB(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents): TDataSet;
    procedure PrepareFetchExternal(ExternalKeys: TBoldObjectIdList;
      ValueSpace: IBoldValueSpace; MoldClass: TMoldClass;
      MemberIdList: TBoldMemberIdList; var FetchContext: TObject); override;
    procedure PostFetch(FetchContext: TObject; MoldClass: TMoldClass); override;
    function ConfigItemByObjectContents(ObjectContents: IBoldObjectContents): TBoldExternalPersistenceConfigDataSetItem;
    procedure FetchObject(ObjectContents: IBoldObjectContents;
      MemberIdList: TBoldMemberIdList; FetchContext: TObject;
      ValueSpace: IBoldValueSpace); override;
    function HandlesClass(MoldClass: TMoldClass): Boolean; override;
    function ExternalKeyExistsInExternalStorage(MoldClass:
      TMoldClass; ExternalKey: TBoldObjectId): Boolean; override;
    procedure AssignKeyToObject(MoldClass: TMoldClass;
      ObjectContents: IBoldObjectContents; ExternalKey: TBoldObjectId;
      ValueSpace: IBoldValueSpace); override;
    function ExternalKeysToInternalSQL(MoldClass: TMoldClass;
      ExternalKeys: TBoldObjectIdList): String; override;
    procedure GetExternalKeys(MoldClass: TMoldClass;
      ExternalKeys: TBoldObjectIdList); override;
    function GetExternalKeyFromObject(ObjectContents: IBoldObjectContents;
      ValueSpace: IBoldValueSpace): TBoldObjectId; override;
    procedure CreateObjects(ObjectIdList: TBoldObjectIdList;
      ValueSpace: IBoldValueSpace); override;
    procedure DeleteObjects(ObjectIdList: TBoldObjectIdList;
      ValueSpace: IBoldValueSpace); override;
    procedure UpdateObjects(ObjectIdList: TBoldObjectIdList;
      ValueSpace: IBoldValueSpace); override;
    function GetMaxFetchBlockSize: integer; override;
  public
    constructor Create(MoldModel: TMoldModel;
      Config: TBoldExternalPersistenceConfigDataSetItems;
      TypeNameDictionary: TBoldTypeNameDictionary;
      OnStartUpdates, OnEndUpdates, OnFailUpdates: TNotifyEvent; MaxFetchBlockSize: integer);
    procedure SubscribeToPersistenceEvents(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet = []); override;
    property Config: TBoldExternalPersistenceConfigDataSetItems read fConfig;
  end;

implementation

uses
  SysUtils,
  Variants,

  BoldCoreConsts,
  BoldDefs,
  BoldNameExpander,
  BoldValueInterfaces,
  BoldStringId,
  BoldDefaultId;

function MemberIndexByName(MoldClass: TMoldClass; MemberName: String): Integer;
begin
  for Result := 0 to MoldClass.AllBoldMembers.Count-1 do
    if SameText(MoldClass.AllBoldMembers[Result].ExpandedExpressionName, MemberName) then
      exit;
  Result := -1;
end;

function FindExternalKey(MoldClass: TMoldClass): Integer;
begin
  for Result := 0 to MoldClass.AllBoldMembers.Count-1 do
    if MoldClass.AllBoldMembers[Result].Storage = bsExternalKey then
      exit;
  Result := -1;
end;

function RemovePreAt(Member: TMoldMember): String;
begin
  result := BoldExpandName(Member.ColumnName, Member.name, xtSQL, -1, Member.MoldClass.Model.NationalCharConversion);
  if (Length(result) > 0) and (result[1] = '@') then
    Result := Copy(result, 2, Length(result))
end;

function FindExternalKeyColumns(MoldClass: TMoldClass): String;
var
  i: integer;
begin
  Result := '';
  for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
      Result := Result + RemovePreAt(MoldClass.AllBoldMembers[i]) + ';';

  if Length(Result) > 0 then
    SetLength(Result, Length(Result)-1);
end;

function GetCharCount(C: Char; S: String): Integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to Length(S) do
    if S[i] = C then
      Inc(Result);
end;

function GetNextWord(var S: String; Splitter: Char): String;
begin
  if Pos(Splitter, S) > 0 then
  begin
    Result := Copy(S, 1, Pos(Splitter, S)-1);
    Delete(S, 1, Pos(Splitter, S));
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;


function GetDataValue(DataSet: TDataSet; FieldNames: String): Variant;
var
  FieldCount: Integer;
  i: integer;
  S: String;
begin
  FieldCount := GetCharCount(';', FieldNames) + 1;
  if FieldCount = 1 then
    Result := DataSet.FieldByName(FieldNames).Value
  else if FieldCount > 0 then
  begin
    S := FieldNames;
    Result := VarArrayCreate([0, FieldCount-1], varVariant);
    for i := 0 to FieldCount-1 do
      Result[i] := DataSet.FieldByName(GetNextWord(S, ';')).Value;
  end;
end;

function VarArrayToStr(V: Variant): String;
var
  i: integer;
begin
  Result := '';
  for i := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
    Result := Result + VarToStr(V[i]) + ';';
  if Length(Result) > 0 then
    SetLength(Result, Length(Result)-1);
end;

function NormalizeVariant(V: Variant): Variant;
begin
  if VarType(V) and varArray <> 0 then
    Result := VarArrayToStr(V)
  else
    Result := V;
end;

function BoldValueToVariant(B: IBoldValue): Variant;
var
  Nullable: IBoldNullableValue;
  S: IBoldStringContent;
  I: IBoldIntegerContent;
  F: IBoldFloatContent;
  C: IBoldCurrencyContent;
  Bool: IBoldBooleanContent;
  D: IBoldDateContent;
  T: IBoldTimeContent;
  DT: IBoldDateTimeContent;
  BL: IBoldBlobContent;
begin
  if (B.QueryInterface(IBoldNullableValue, Nullable) = S_OK) and
     (Nullable.IsNull) then
    Result := Null
  else if B.QueryInterface(IBoldStringContent, S) = S_OK then
    Result := S.asString
  else if B.QueryInterface(IBoldIntegerContent, I) = S_OK then
    Result := I.asInteger
  else if B.QueryInterface(IBoldFloatContent, F) = S_OK then
    Result := F.asFloat
  else if B.QueryInterface(IBoldCurrencyContent, C) = S_OK then
    Result := C.asCurrency
  else if B.QueryInterface(IBoldBooleanContent, Bool) = S_OK then
    Result := Bool.asBoolean
  else if B.QueryInterface(IBoldDateContent, D) = S_OK then
    Result := D.asDate
  else if B.QueryInterface(IBoldTimeContent, T) = S_OK then
    Result := T.asTime
  else if B.QueryInterface(IBoldDateTimeContent, DT) = S_OK then
    Result := DT.asDateTime
  else if B.QueryInterface(IBoldBlobContent, BL) = S_OK then
    Result := BL.asBlob
  else raise Exception.Create(sUnknownDataType);
end;

procedure VariantToBoldValue(B: IBoldValue; Value: Variant);
var
  Nullable: IBoldNullableValue;
  S: IBoldStringContent;
  I: IBoldIntegerContent;
  F: IBoldFloatContent;
  C: IBoldCurrencyContent;
  Bool: IBoldBooleanContent;
  D: IBoldDateContent;
  T: IBoldTimeContent;
  DT: IBoldDateTimeContent;
  BL: IBoldBlobContent;
begin
  if (B.QueryInterface(IBoldNullableValue, Nullable) = S_OK) and
     (VarIsNull(Value)) then
    Nullable.SetContentToNull
  else if B.QueryInterface(IBoldStringContent, S) = S_OK then
    S.asString := Value
  else if B.QueryInterface(IBoldIntegerContent, I) = S_OK then
    I.asInteger := Value
  else if B.QueryInterface(IBoldFloatContent, F) = S_OK then
    F.asFloat := Value
  else if B.QueryInterface(IBoldCurrencyContent, C) = S_OK then
    C.asCurrency := Value
  else if B.QueryInterface(IBoldBooleanContent, Bool) = S_OK then
    Bool.asBoolean := Value
  else if B.QueryInterface(IBoldDateContent, D) = S_OK then
    D.asDate := Value
  else if B.QueryInterface(IBoldTimeContent, T) = S_OK then
    T.asTime := Value
  else if B.QueryInterface(IBoldDateTimeContent, DT) = S_OK then
    DT.asDateTime := Value
  else if B.QueryInterface(IBoldBlobContent, BL) = S_OK then
    BL.asBlob := AnsiString(Value)
  else raise Exception.Create(sUnknownDataType);
end;

procedure SetBoldValueToNull(B: IBoldValue);
var
  Nullable: IBoldNullableValue;
  S: IBoldStringContent;
  I: IBoldIntegerContent;
  F: IBoldFloatContent;
  C: IBoldCurrencyContent;
  Bool: IBoldBooleanContent;
  D: IBoldDateContent;
  T: IBoldTimeContent;
  DT: IBoldDateTimeContent;
  BL: IBoldBlobContent;
begin
  if B.QueryInterface(IBoldNullableValue, Nullable) = S_OK then
    Nullable.SetContentToNull
  else if B.QueryInterface(IBoldStringContent, S) = S_OK then
    S.asString := ''
  else if B.QueryInterface(IBoldIntegerContent, I) = S_OK then
    I.asInteger := 0
  else if B.QueryInterface(IBoldFloatContent, F) = S_OK then
    F.asFloat := 0
  else if B.QueryInterface(IBoldCurrencyContent, C) = S_OK then
    C.asCurrency := 0
  else if B.QueryInterface(IBoldBooleanContent, Bool) = S_OK then
    Bool.asBoolean := False
  else if B.QueryInterface(IBoldDateContent, D) = S_OK then
    D.asDate := 0
  else if B.QueryInterface(IBoldTimeContent, T) = S_OK then
    T.asTime := 0
  else if B.QueryInterface(IBoldDateTimeContent, DT) = S_OK then
    DT.asDateTime := 0
  else if B.QueryInterface(IBoldBlobContent, BL) = S_OK then
    BL.asBlob := ''
  else raise Exception.Create(sUnknownDataType);
end;

function GetKeyCount(MoldClass: TMoldClass): Integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
      Inc(Result);
end;

function GetObjectKeys(MoldClass: TMoldClass; ObjectContents: IBoldObjectContents): Variant;
var
  i: integer;
  KeyCount: Integer;
  CurrKey: Integer;
begin
  Result := Unassigned;

  KeyCount := GetKeyCount(MoldClass);
  if KeyCount = 0 then
    exit
  else if KeyCount > 1 then
    Result := VarArrayCreate([0, KeyCount-1], varVariant);

  CurrKey := 0;

  for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
      if KeyCount = 1 then
      begin
        Result := BoldValueToVariant(ObjectContents.ValueByIndex[i]);
        exit;
      end
      else
      begin
        Result[CurrKey] := BoldValueToVariant(ObjectContents.ValueByIndex[i]);
        Inc(CurrKey);
      end;
end;

{ TBoldExternalPersistenceControllerEventDriven }

constructor TBoldExternalPersistenceControllerDataSet.Create(MoldModel: TMoldModel; Config: TBoldExternalPersistenceConfigDataSetItems; TypeNameDictionary: TBoldTypeNameDictionary; OnStartUpdates, OnEndUpdates, OnFailUpdates: TNotifyEvent; MaxFetchBlockSize: integer);
begin
  inherited Create(MoldModel, TypeNameDictionary, OnStartUpdates, OnEndUpdates, OnFailUpdates, UpdateBoldDatabaseFirst);
  FConfig := Config;
  FMaxFetchBlockSize := MaxFetchBlockSize;
end;

procedure TBoldExternalPersistenceControllerDataSet.CreateObjects(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace);
var
  i: integer;
  j: integer;
  ConfigItem: TBoldExternalPersistenceConfigDataSetItem;
  ObjectContents: IBoldObjectContents;
  MoldClass: TMoldClass;
  BoldValue: IBoldValue;
  ColumnName: String;
begin
  for i := 0 to ObjectIdList.count-1 do
  begin
    ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectIdList[i]];

    MoldClass := MoldModel.Classes[ObjectidList[i].TopSortedIndex];
    ConfigItem := ConfigItemByObjectContents(ObjectContents);

    if Assigned(ConfigItem) then
    begin
      ConfigItem.DataSet.Insert;
      for j := 0 to MoldClass.AllBoldMembers.Count-1 do
        if MoldClass.AllBoldMembers[j].Storage in [bsExternal, bsExternalKey] then
        begin
          { Do not store derived attributes }
          if MoldClass.AllBoldMembers[j].Derived then
            Continue;

          { Do not store multi links and non-embedded single links }
          if (MoldClass.AllBoldMembers[j] is TMoldRole) and
             (TMoldRole(MoldClass.AllBoldMembers[j]).Multi or
             not TMoldRole(MoldClass.AllBoldMembers[j]).Embed) then
            Continue;

          BoldValue := ObjectContents.ValueByIndex[j];

          ColumnName := BoldExpandName(MoldClass.AllBoldMembers[j].ColumnName, MoldClass.AllBoldMembers[j].name, xtSQL, -1, MoldClass.Model.NationalCharConversion);

          ConfigItem.DataSet[ColumnName] := BoldValueToVariant(BoldValue);
        end;
      ConfigItem.DataSet.Post;
    end;
  end;
end;

procedure TBoldExternalPersistenceControllerDataSet.DeleteObjects(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace);
var
  i: integer;
  ObjectContents: IBoldObjectContents;
  MoldClass: TMoldClass;
  DataSet: TDataSet;
begin
  for i := 0 to ObjectIdList.count-1 do
  begin
    ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectIdList[i]];
    MoldClass := MoldModel.Classes[ObjectidList[i].TopSortedIndex];

    DataSet := LocateInDB(MoldClass, ObjectContents);
    if Assigned(DataSet) then
      DataSet.Delete;
  end;
end;

procedure TBoldExternalPersistenceControllerDataSet.FetchObject(
  ObjectContents: IBoldObjectContents;
  MemberIdList: TBoldMemberIdList; FetchContext: TObject; ValueSpace: IBoldValueSpace);

  procedure SyncMember(ConfigItem: TBoldExternalPersistenceConfigDataSetItem;
    MoldClass: TMoldClass; MemberIndex: Integer; DataSet: TDataSet);
  var
    DBFieldName: String;
    Value: IBoldValue;
    MoldRole: TMoldRole;
    DBValue: Variant;
    ExternalKey: TBoldObjectId;
    IDRef: IBoldObjectIdRef;
    IDRefList: IBoldObjectIdListRef;
    MultiLinkConfigItem: TBoldExternalPersistenceConfigDataSetItem;
    MultiLinkClass: TMoldClass;
    MultiLinkDataSet: TDataSet;
    MultiLinkKeyName: String;
    MultiLinkList: TBoldObjectidList;
    i: integer;
    A: String;
    B: String;
    S: String;
  begin
    Value := ObjectContents.ValueByIndex[MemberIndex];
    DBFieldName := RemovePreAt(MoldClass.AllBoldMembers[MemberIndex]);

    if MoldClass.AllBoldMembers[MemberIndex] is TMoldRole then
    begin
      MoldRole := TMoldRole(MoldClass.AllBoldMembers[MemberIndex]);

      if MoldRole.Multi then
      begin
        MultiLinkClass := MoldRole.OtherEnd.MoldClass;

        MultiLinkConfigItem := Config.FindExpressionName(MultiLinkClass.ExpandedExpressionName);
        if not Assigned(MultiLinkConfigItem) then
          raise Exception.CreateFmt(sLinkToUnconfiguredTable, [MultiLinkClass.ExpandedExpressionName]);

        MultiLinkDataSet := MultiLinkConfigItem.DataSet;
        MultiLinkKeyName := RemovePreAt(MoldRole.OtherEnd);

        DBFieldName := FindExternalKeyColumns(MoldClass);
        A := DBFieldName;
        B := MultiLinkKeyName;
        S := '';
        for i := 0 to GetCharCount(';', MultiLinkKeyName) do
        begin
          if S <> '' then
            S := S + ' and '; // do not localize
          S := S + '(' + GetNextWord(B, ';') + ' = ' + ConfigItem.DataSet.FieldByName(GetNextWord(A, ';')).AsString + ')';
        end;

        MultiLinkDataSet.Filter := S;

        MultiLinkDataSet.Filtered := True;
        try
          MultiLinkList := TBoldObjectIdList.Create;
          try
            MultiLinkDataSet.First;
            while not MultiLinkDataSet.Eof do
            begin
              DBValue := NormalizeVariant(GetDataValue(MultiLinkDataSet, FindExternalKeyColumns(MultiLinkClass)));

              if not VarIsNull(DBValue) then
              begin
                 // the type of the field MUST match the type of the internal ID
                if VarType(DBValue) in [varInteger, varSmallint, varSingle, varDouble] then
                begin
                  ExternalKey := TBoldDefaultId.Create;
                  TBoldDefaultId(ExternalKey).AsInteger := DBValue;
                end
                else if varType(dbValue) = varString then
                begin
                  ExternalKey := TBoldStringId.Create;
                  TBoldStringId(ExternalKey).AsString := DBValue;
                end
                else
                  raise Exception.CreateFmt(sUnknownVarTypeLoadingID, [MoldClass.name, MoldRole.name]);

                MultiLinkList.Add(ExternalKey);
                ExternalKey.Free;
              end;
              MultiLinkDataSet.Next;
            end;
            if Value.QueryInterface(IBoldObjectIdListRef, IDRefList) = S_OK then
              SetMultiLink(IDRefList, MultiLinkList, MoldRole.OtherEnd.MoldClass);
          finally
            MultiLinkList.Free;
          end;
        finally
          MultiLinkDataSet.Filtered := False;
        end;
      end
      else
      begin
        DBValue := NormalizeVariant(GetDataValue(ConfigItem.DataSet, DBFieldName));

        if not VarIsNull(DBValue) then
        begin
          if VarType(DBValue) in [varInteger, varSmallint, varSingle, varDouble] then
          begin
            ExternalKey := TBoldDefaultId.Create;
            TBoldDefaultId(ExternalKey).AsInteger := DBValue;
          end
          else if varType(dbValue) = varString then
          begin
            ExternalKey := TBoldStringId.Create;
            TBoldStringId(ExternalKey).AsString := DBValue;
          end
          else
            raise Exception.CreateFmt(sUnknownVarTypeLoadingSingleID, [MoldClass.name, MoldRole.name]);

          if Value.QueryInterface(IBoldObjectIdRef, IDRef) = S_OK then
            SetSingleLink(IDRef, ExternalKey, MoldRole.OtherEnd.MoldClass);
          ExternalKey.Free;
        end;
      end
    end
    else
    begin
      if ConfigItem.DataSet.FieldByName(DBFieldName).IsNull then
        SetBoldValueToNull(Value)
      else
        VariantToBoldValue(Value, ConfigItem.DataSet.FieldByName(DBFieldName).Value);
    end;
  end;

var
  ConfigItem: TBoldExternalPersistenceConfigDataSetItem;
  i: integer;
  MoldClass: TMoldClass;
  DataSet: TDataSet;
begin
  ConfigItem := ConfigItemByObjectContents(ObjectContents);
  MoldClass := MoldModel.Classes[ObjectContents.ObjectId.TopSortedIndex];
  DataSet := LocateInDB(MoldClass, ObjectContents);

  if Assigned(DataSet) and Assigned(ConfigItem) then
  begin
    if Assigned(MemberIdList) then
    begin
      for i := 0 to MemberIdList.Count-1 do
        if MoldClass.AllBoldMembers[MemberIdList[i].MemberIndex].Storage in [bsExternal, bsExternalKey] then
          SyncMember(ConfigItem, MoldClass, MemberIdList[i].MemberIndex, DataSet);
    end
    else
      for i := 0 to MoldClass.AllBoldMembers.Count-1 do
        if MoldClass.AllBoldMembers[i].Storage in [bsExternal, bsExternalKey] then
        begin
          { Do not fetch derived attributes }
          if MoldClass.AllBoldMembers[i].Derived then
            Continue;

          { Do not fetch multi links and non-embedded single links }
          if (MoldClass.AllBoldMembers[i] is TMoldRole) and
             (TMoldRole(MoldClass.AllBoldMembers[i]).Multi or
             not TMoldRole(MoldClass.AllBoldMembers[i]).Embed) then
            Continue;

          SyncMember(ConfigItem, MoldClass, i, DataSet);
      end;
    if not DataSet.CanModify then
      ObjectContents.IsReadOnly := true;
  end;
end;

procedure TBoldExternalPersistenceControllerDataSet.GetExternalKeys(MoldClass: TMoldClass; ExternalKeys: TBoldObjectIdList);
var
  ConfigItem: TBoldExternalPersistenceConfigDataSetItem;
  DBFieldName: String;
  ExternalID: TBoldObjectId;
  DBValue: Variant;
begin
  ConfigItem := Config.FindExpressionName(MoldClass.ExpandedExpressionName);
  ConfigItem.DataSet.First;

  DBFieldName := FindExternalKeyColumns(MoldClass);

  while not ConfigItem.DataSet.Eof do
  begin
    DBValue := NormalizeVariant(GetDataValue(ConfigItem.DataSet, DBFieldName));
    if VarType(DBValue) in [varInteger, varSmallint, varSingle, varDouble] then
    begin
      ExternalId := TBoldDefaultId.Create;
      TBoldDefaultId(ExternalId).AsInteger := DBValue;
    end
    else if VarType(DBValue) = varString then
    begin
      ExternalId := TBoldStringId.Create;
      TBoldStringId(ExternalId).AsString := DBValue;
    end
    else
      raise Exception.CreateFmt(sUnknownVarTypeLoadingObject, [MoldClass.name]);
    ExternalKeys.Add(ExternalId);
    ExternalId.Free;
    ConfigItem.DataSet.Next;
  end;
end;

function TBoldExternalPersistenceControllerDataSet.HandlesClass(MoldClass: TMoldClass): Boolean;
begin
  result := true;
end;

function TBoldExternalPersistenceControllerDataSet.ExternalKeyExistsInExternalStorage(MoldClass: TMoldClass; ExternalKey: TBoldObjectId): Boolean;
var
  ConfigItem: TBoldExternalPersistenceConfigDataSetItem;
begin
  ConfigItem := Config.FindExpressionName(MoldClass.ExpandedExpressionName);
  Result := ConfigItem.DataSet.Locate(FindExternalKeyColumns(MoldClass),
    ExternalKey.AsString, []);
end;

function TBoldExternalPersistenceControllerDataSet.ConfigItemByObjectContents(
  ObjectContents: IBoldObjectContents): TBoldExternalPersistenceConfigDataSetItem;
var
  MoldClass: TMoldClass;
begin
  MoldClass := MoldModel.Classes[ObjectContents.ObjectId.TopSortedIndex];
  result := Config.FindExpressionName(MoldClass.ExpandedExpressionName);
end;

procedure TBoldExternalPersistenceControllerDataSet.SubscribeToPersistenceEvents(Subscriber: TBoldSubscriber; Events: TBoldSmallEventSet);
begin
  inherited;
end;

procedure TBoldExternalPersistenceControllerDataSet.UpdateObjects(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace);
var
  i, j: integer;
  ConfigItem: TBoldExternalPersistenceConfigDataSetItem;
  ObjectContents: IBoldObjectContents;
  DataSet: TDataSet;
  MoldClass: TMoldClass;
  Value: IBoldValue;
  ColumnName: String;
begin
  for i := 0 to ObjectIdList.count-1 do
  begin
    ObjectContents := ValueSpace.ObjectContentsByObjectId[ObjectIdList[i]];
    MoldClass := MoldModel.Classes[ObjectidList[i].TopSortedIndex];
    ConfigItem := ConfigItemByObjectContents(ObjectContents);
    if Assigned(ConfigItem) then
    begin
      DataSet := LocateInDB(MoldClass, ObjectContents);
      if Assigned(DataSet) then
      begin
        for j := 0 to MoldClass.AllBoldMembers.Count-1 do
        begin
          { Do not store derived attributes }
          if MoldClass.AllBoldMembers[j].Derived then
            Continue;

          { Do not store multi links and non-embedded single links }
          if (MoldClass.AllBoldMembers[j] is TMoldRole) and
             (TMoldRole(MoldClass.AllBoldMembers[j]).Multi or
             not TMoldRole(MoldClass.AllBoldMembers[j]).Embed) then
            Continue;

          Value := ObjectContents.ValueByIndex[j];
          ColumnName := BoldExpandName(MoldClass.AllBoldMembers[j].ColumnName, MoldClass.AllBoldMembers[j].name, xtSQL, -1, MoldClass.Model.NationalCharConversion);

          DataSet[ColumnName] := BoldValueToVariant(Value);
        end;
      end;
    end;
  end;
end;

function TBoldExternalPersistenceControllerDataSet.GetExternalKeyFromObject(
  ObjectContents: IBoldObjectContents; ValueSpace: IBoldValueSpace): TBoldObjectId;
var
  MoldClass: TMoldClass;
  DBValue: Variant;
begin
  MoldClass := MoldModel.Classes[ObjectContents.ObjectId.TopSortedIndex];

  DBValue := NormalizeVariant(GetObjectKeys(MoldClass, ObjectContents));

  if VarType(DBValue) in [varInteger, varSmallint, varSingle, varDouble] then
  begin
    Result := TBoldDefaultId.Create;
    TBoldDefaultId(Result).AsInteger := DBValue;
  end
  else if VarType(DBValue) = varString then
  begin
    Result := TBoldStringId.Create;
    TBoldStringId(Result).AsString := DBValue;
  end
  else
    raise Exception.CreateFmt(sUnknownVarTypeLoadingObject, [MoldClass.name]);
end;


procedure TBoldExternalPersistenceControllerDataSet.PostFetch(
  FetchContext: TObject; MoldClass: TMoldClass);
begin
  inherited;
end;

procedure TBoldExternalPersistenceControllerDataSet.PrepareFetchExternal(ExternalKeys: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MoldClass: TMoldClass; MemberIdList: TBoldMemberIdList; var FetchContext: TObject);
begin
  inherited;
end;

function TBoldExternalPersistenceControllerDataSet.GetMaxFetchBlockSize: integer;
begin
  result := fMaxFetchBlockSize;
end;

function TBoldExternalPersistenceControllerDataSet.ExternalKeysToInternalSQL(MoldClass: TMoldClass; ExternalKeys: TBoldObjectIdList): String;
var
  i, j: integer;
  KeyCount: Integer;
  SQL: String;
  ExternalColumns: String;
  ExternalKey: String;
  S: String;
  T: String;
  v, c: Integer;
begin
  ExternalColumns := FindExternalKeyColumns(MoldClass);
  KeyCount := GetCharCount(';', ExternalColumns) + 1;

  if KeyCount = 1 then
  begin
    Result := inherited ExternalKeysToInternalSQL(MoldClass, ExternalKeys);
    Exit;
  end;

  Result := '';
  for i := 1 to ExternalKeys.Count-1 do
  begin
    SQL := '(';
    ExternalKey := ExternalKeys[i].AsString;
    S := ExternalColumns;
    for j := 1 to GetCharCount(';', ExternalKey) + 1 do
    begin
      T := GetNextWord(ExternalKey, ';');
      Val(T, v, c);
      if c <> 0 then
        T := '''' + T + '''';
      SQL := SQL + '(' + GetNextWord(S, ';') + ' = ' + T + ') AND '; // do not localize
    end;
    if Length(SQL) > 1 then
      SetLength(SQL, Length(SQL)-5);
    SQL := SQL + ')';

    Result := Result + SQL + ' OR '; // do not localize
  end;

  if Length(Result) > 0 then
    SetLength(Result, Length(Result)-4);
end;

procedure TBoldExternalPersistenceControllerDataSet.AssignKeyToObject(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents;
  ExternalKey: TBoldObjectId; ValueSpace: IBoldValueSpace);
var
  i: integer;
  KeyValue: IBoldValue;
  StrContent: IBoldStringContent;
  IntContent: IBoldIntegerContent;
  S: String;
begin
  begin
    Keyvalue := nil;
    S := ExternalKey.AsString;
    for i := 0 to MoldClass.AllBoldMembers.Count-1 do
    begin
      if MoldClass.AllBoldMembers[i].Storage = bsExternalKey then
      begin
        KeyValue := ObjectContents.ValueByIndex[i];
        if KeyValue.QueryInterface(IBoldStringContent, StrContent) = S_OK then
          StrContent.AsString := GetNextWord(S, ';')
        else if KeyValue.QueryInterface(IBoldIntegerContent, IntContent) = S_OK then
          IntContent.AsInteger := StrToInt(GetNextWord(S, ';'))
        else
          raise EBold.createFmt(sKeyTypeNotAutoHandled, [MoldClass.Name, MoldClass.AllBoldMembers[i].Name]);
      end;
    end;
  end;
end;

function TBoldExternalPersistenceControllerDataSet.LocateInDB(
  MoldClass: TMoldClass; ObjectContents: IBoldObjectContents): TDataSet;
var
  ExternalKeys: String;
  ExternalKeyValues: Variant;
  ConfigItem: TBoldExternalPersistenceConfigDataSetItem;
begin
  Result := nil;
  ExternalKeys := FindExternalKeyColumns(MoldClass);
  ConfigItem := ConfigItemByObjectContents(ObjectContents);
  if Assigned(ConfigItem) and (ExternalKeys <> '') then
  begin
    ExternalKeyValues := GetObjectKeys(MoldClass, ObjectContents);
    if ConfigItem.DataSet.Locate(ExternalKeys, ExternalKeyValues, []) then
        Result := ConfigItem.DataSet;
  end;
end;

end.
