unit cxBoldLookupEdit;

interface

//  v2.03 - 25 Jan 2011  2007-2011 Daniel Mauric

uses
  classes,
  cxLookupEdit,
  cxDataStorage,
  cxClasses,
  cxEdit,
  cxContainer,
  cxBoldEditors,
  cxGridBoldSupportUnit,
  dxCoreClasses,

  BoldElements,
  BoldControlPack,
  BoldControlsDefs,
  BoldDefs,
  BoldHandles,
  BoldComponentValidator;

type
  TcxCustomBoldLookupEditProperties = class;

  { TcxCustomBoldLookupEditLookupData }

  TcxCustomBoldLookupEditLookupData = class(TcxCustomLookupEditLookupData)
  private
//    fCurrentElement: TBoldElement; // ?
    function GetDataController: TcxBoldDataController;
    function GetProperties: TcxCustomBoldLookupEditProperties;
  protected
    procedure DoSetCurrentKey(ARecordIndex: Integer); override;
    procedure DoSyncGrid; override;
    property DataController: TcxBoldDataController read GetDataController;
    property Properties: TcxCustomBoldLookupEditProperties read GetProperties;
  end;

  { TcxCustomBoldLookupEditProperties }

  TcxCustomBoldLookupEditProperties = class(TcxCustomLookupEditProperties, IcxBoldEditProperties, IBoldValidateableComponent)
  private
//    FCachedLookupSource: TDataSource;
    FCaseSensitiveSearch: Boolean;
    FLockGridModeCount: Integer;
//    FLookupField: TField;
//    FLookupList: TcxLookupList;
//    FLookupSource: TDataSource;
//    FSyncLookup: Boolean;
    fBoldSelectChangeAction: TBoldComboSelectChangeAction;
    fBoldSetValueExpression: TBoldExpression;
//    function GetIsUseLookupList: Boolean;
//    function GetKeyFieldNames: string;
//    function GetListField: TField;
    function GetListFieldIndex: Integer;
//    function GetListFieldNames: string;
//    procedure SetIsUseLookupList(Value: Boolean);
    procedure SetBoldSelectChangeAction(
      const Value: TBoldComboSelectChangeAction);
//    procedure SetKeyFieldNames(const Value: string);
    procedure SetListFieldIndex(Value: Integer);
    procedure SetBoldSetValueExpression(const Value: TBoldExpression);
//    procedure SetListFieldNames(const Value: string);
  protected
    // IcxBoldEditProperties
    procedure SetStoredValue(aValue: Variant; aBoldHandle: TBoldElementHandle; aEdit: TcxCustomEdit; aFollower: TBoldFollower; var aDone: boolean); virtual;
    function BoldElementToEditValue(aFollower: TBoldFollower; aElement: TBoldElement; aEdit: TcxCustomEdit): variant;
    function CanEdit(aBoldHandle: TBoldElementHandle; aFollower: TBoldFollower): boolean; virtual;
    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;
    // BoldLookupGrid methods
    procedure BoldLookupGridBeginUpdate; virtual;
//    procedure BoldLookupGridCheckColumnByFieldName(const AFieldName: string); virtual; // if a column does not exist, then create it with zero index
//    procedure BoldLookupGridCreateColumnsByFieldNames(const AFieldNames: string); virtual;
    procedure BoldLookupGridEndUpdate; virtual;
//    function GetBoldLookupGridColumnField(AIndex: Integer): TField; virtual;
//    function GetBoldLookupGridColumnFieldName(AIndex: Integer): string; virtual;
//    function GetBoldLookupGridColumnIndexByFieldName(const AFieldName: string): Integer; virtual;
    function GetBoldLookupGridDataController: TcxBoldDataController; virtual;

    function CanDisplayArbitraryEditValue: Boolean;
    procedure CheckLookup; virtual;
    procedure CheckLookupColumn; virtual;
//    procedure CheckLookupList;
    procedure DefaultValuesProviderDestroyed; override;
    procedure DefineByLookupError;
    procedure DoChanged; override;
    function GetAlwaysPostEditValue: Boolean; override;
    function FindByText(AItemIndex: Integer; const AText: string; APartialCompare: Boolean): Integer; override;
    function GetDisplayColumnIndex: Integer; override;
    function GetDisplayLookupText(const AKey: TcxEditValue): string; override;
    function GetDefaultHorzAlignment: TAlignment; override;
    function GetDefaultMaxLength: Integer; override;
    function GetIncrementalFiltering: Boolean; override;
    function GetKeyByRecordIndex(ARecordIndex: Integer): Variant;
    class function GetLookupDataClass: TcxInterfacedPersistentClass; override;
//    function GetLookupResultFieldName: string;
    function GetNullKey: Variant; override;
    function GetRecordIndexByKey(const AKey: Variant): Integer;
    function IsPickMode: Boolean; override;
    procedure LockDataChanged; override;
    procedure LookupSourceFreeNotification(Sender: TComponent); virtual;
    procedure SetDisplayColumnIndex(Value: Integer); override;
//    procedure SetLookupField(ALookupField: TField);
    procedure UnlockDataChanged; override;
//    property InSyncLookup: Boolean read FSyncLookup;
//    property IsUseLookupList: Boolean read GetIsUseLookupList write SetIsUseLookupList;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
//    function GetDataField: TField;
//    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
//    function GetLookupField: TField;
//    function IsLookupField: Boolean; override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue;
      var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;
    property CaseSensitiveSearch: Boolean read FCaseSensitiveSearch
      write FCaseSensitiveSearch default False;
    property DataController: TcxBoldDataController read GetBoldLookupGridDataController;
//    property KeyFieldNames: string read GetKeyFieldNames write SetKeyFieldNames;
//    property ListField: TField read GetListField;
//    property ListFieldNames: string read GetListFieldNames write SetListFieldNames stored False;
    property ListFieldIndex: Integer read GetListFieldIndex write SetListFieldIndex default 0;

//    property BoldRowProperties: TBoldStringFollowerController read fBoldRowProperties write SetRowProperties;
    property BoldSelectChangeAction: TBoldComboSelectChangeAction read fBoldSelectChangeAction write SetBoldSelectChangeAction default bdcsSetValue;
    property BoldSetValueExpression: TBoldExpression read fBoldSetValueExpression write SetBoldSetValueExpression;
  end;

  { TcxCustomBoldLookupEdit }

  TcxCustomBoldLookupEdit = class(TcxCustomLookupEdit)
  private
    function GetProperties: TcxCustomBoldLookupEditProperties;
    function GetActiveProperties: TcxCustomBoldLookupEditProperties;
    procedure SetProperties(Value: TcxCustomBoldLookupEditProperties);
  protected
    function GetClearValue: TcxEditValue; override;
    function IsValidChar(AChar: Char): Boolean; override;
    function ItemIndexToLookupKey(AItemIndex: Integer): TcxEditValue; override;
    function LookupKeyToEditValue(const AKey: TcxEditValue): TcxEditValue; override;
//    function LookupKeyToItemIndex(const AKey: TcxEditValue): Integer; override;
    procedure PopupWindowClosed(Sender: TObject); override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue;
      var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCustomBoldLookupEditProperties
      read GetActiveProperties;
    property Properties: TcxCustomBoldLookupEditProperties read GetProperties
      write SetProperties;
  end;

  { TcxBoldLookupEditDataBinding }

  TcxBoldLookupEditDataBinding = class(TcxBoldTextEditDataBinding)
  protected
    function IsLookupControl: Boolean; override;
  end;

implementation

uses
{$IFDEF DELPHI6}
  VDBConsts,
{$ENDIF}
  Contnrs,
  cxDropDownEdit,
  Variants,
  SysUtils,

  BoldSystem;

{ TcxCustomBoldLookupEditLookupData }

procedure TcxCustomBoldLookupEditLookupData.DoSetCurrentKey(ARecordIndex: Integer);
begin
  FCurrentKey := Properties.GetKeyByRecordIndex(ARecordIndex);
end;

type
  TcxCustomEditAccess = Class(TcxCustomEdit)
  end;

procedure TcxCustomBoldLookupEditLookupData.DoSyncGrid;
var
  lcxBoldEditDataBinding: TcxBoldEditDataBinding;
  lBoldElement: TBoldElement;
begin
  if Edit is TcxCustomEdit and (TcxCustomEditAccess(Edit).DataBinding is TcxBoldEditDataBinding) then
  begin
    lcxBoldEditDataBinding := TcxCustomEditAccess(Edit).DataBinding as TcxBoldEditDataBinding;
    lBoldElement := lcxBoldEditDataBinding.Follower.Element;
    if Assigned(DataController) and Assigned(lBoldElement) and (DataController.Follower.Element is TBoldList) then
    begin
      Properties.LockDataChanged;
      try
        DataController.RecNo := (DataController.Follower.Element as TBoldList).IndexOf(lBoldElement);
      finally
        Properties.UnlockDataChanged;
      end;
    end;
  end;
//  DataController.Follower.Element.
{  if DataController <> nil then
    try
      Properties.LockDataChanged;
      try
        DataController.LocateByKey(GetCurrentKey);
      finally
        Properties.UnlockDataChanged;
      end;
    except
      on EVariantError do;
      on EDatabaseError do;
    end;
}
end;

function TcxCustomBoldLookupEditLookupData.GetDataController: TcxBoldDataController;
begin
  Result := Properties.DataController;
end;

function TcxCustomBoldLookupEditLookupData.GetProperties: TcxCustomBoldLookupEditProperties;
begin
  Result := TcxCustomBoldLookupEditProperties(inherited Properties);
end;

{ TcxCustomBoldLookupEditProperties }

constructor TcxCustomBoldLookupEditProperties.Create(AOwner: TPersistent);
begin
  inherited;
  BoldSelectChangeAction := bdcsSetValue;
end;

destructor TcxCustomBoldLookupEditProperties.Destroy;
begin
//  SetLookupField(nil);
//  FLookupList.Free;
//  FLookupList := nil;
//  FreeAndNil(FCachedLookupSource);
  inherited Destroy;
end;

procedure TcxCustomBoldLookupEditProperties.Assign(Source: TPersistent);
begin
  if Source is TcxCustomBoldLookupEditProperties then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      CaseSensitiveSearch := TcxCustomBoldLookupEditProperties(Source).CaseSensitiveSearch;
//      if not IsDefinedByLookup then
//        KeyFieldNames := TcxCustomBoldLookupEditProperties(Source).KeyFieldNames;
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

class function TcxCustomBoldLookupEditProperties.GetContainerClass: TcxContainerClass;
begin
//  result := inherited GetContainerClass;
  Result := TcxCustomBoldLookupEdit;
end;
{
function TcxCustomBoldLookupEditProperties.GetDataField: TField;
var
  ADefaultValuesProvider: TcxCustomEditDefaultValuesProvider;
begin
  Result := nil;
  if IDefaultValuesProvider <> nil then
  begin
    ADefaultValuesProvider := TcxCustomEditDefaultValuesProvider(IDefaultValuesProvider.GetInstance);
    if ADefaultValuesProvider is TcxCustomDBEditDefaultValuesProvider then
      Result := TcxCustomDBEditDefaultValuesProvider(ADefaultValuesProvider).Field;
  end;
end;
}
{
function TcxCustomBoldLookupEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  if GetLookupField <> nil then
  begin
    if AEditFocused then
      Result := evsKey
    else
      Result := evsText;
  end
  else
    Result := inherited GetEditValueSource(AEditFocused);
end;
}
{
function TcxCustomBoldLookupEditProperties.GetLookupField: TField;
begin
  Result := GetDataField;
  if (Result <> nil) and (not Result.Lookup or (csDestroying in Result.ComponentState)) then
    Result := nil;
end;

function TcxCustomBoldLookupEditProperties.IsLookupField: Boolean;
begin
  Result := GetLookupField <> nil;
end;
}
procedure TcxCustomBoldLookupEditProperties.PrepareDisplayValue(
  const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue;
  AEditFocused: Boolean);
var
  lRecordIndex, lItemIndex: integer;
begin
  lRecordIndex := GetRecordIndexByKey(AEditValue);
  lItemIndex := GetListIndex;
  if (lItemIndex <> -1) and (lRecordIndex <> -1) then
  begin
    DisplayValue := DataController.Values[lRecordIndex, lItemIndex];
    if VarIsNull(DisplayValue) then
    begin
      DisplayValue := DataController.Values[lRecordIndex, lItemIndex];
      if VarIsNull(DisplayValue) then
        DisplayValue := '';
    end;
  end
  else
    inherited PrepareDisplayValue(AEditValue, DisplayValue, AEditFocused);
end;

procedure TcxCustomBoldLookupEditProperties.BoldLookupGridBeginUpdate;
begin
end;
{
procedure TcxCustomBoldLookupEditProperties.BoldLookupGridCheckColumnByFieldName(const AFieldName: string);
begin
end;

procedure TcxCustomBoldLookupEditProperties.BoldLookupGridCreateColumnsByFieldNames(const AFieldNames: string);
begin
end;
}
procedure TcxCustomBoldLookupEditProperties.BoldLookupGridEndUpdate;
begin
end;
{
function TcxCustomBoldLookupEditProperties.GetBoldLookupGridColumnField(AIndex: Integer): TField;
begin
  Result := nil;
end;
}
{
function TcxCustomBoldLookupEditProperties.GetBoldLookupGridColumnFieldName(AIndex: Integer): string;
begin
  Result := '';
end;

function TcxCustomBoldLookupEditProperties.GetBoldLookupGridColumnIndexByFieldName(const AFieldName: string): Integer;
begin
  Result := -1;
end;
}
function TcxCustomBoldLookupEditProperties.GetAlwaysPostEditValue: Boolean;
begin
  result := true;
end;

function TcxCustomBoldLookupEditProperties.GetBoldLookupGridDataController: TcxBoldDataController;
begin
  Result := nil;
end;

function TcxCustomBoldLookupEditProperties.CanDisplayArbitraryEditValue: Boolean;
//var
//  AKeyField: TField;
begin
  Result := DropDownListStyle = lsEditList;
{
  Result := False; // TODO: method in DataController?
  if (KeyFieldNames <> '') and not IsMultipleFieldNames(KeyFieldNames) and
    (DataController <> nil) and (DataController.DataSet <> nil) then
  begin
    AKeyField := DataController.DataSet.FindField(KeyFieldNames);
    if AKeyField <> nil then
      Result := (AKeyField = GetListField) and
        ((DropDownListStyle = lsEditList) or (AKeyField is TStringField));
  end;
}
end;

procedure TcxCustomBoldLookupEditProperties.CheckLookup;
begin
//  Assert(false);
//  SetLookupField(GetLookupField);
//  CheckListSource;
end;

procedure TcxCustomBoldLookupEditProperties.CheckLookupColumn;
//var
//  AFieldName: string;
begin
//  Assert(false);
//  AFieldName := GetLookupResultFieldName;
//  if AFieldName <> '' then
//    BoldLookupGridCheckColumnByFieldName(AFieldName);
end;

{procedure TcxCustomBoldLookupEditProperties.CheckLookupList;
begin
  if FLookupList <> nil then
    FLookupList.Clear;
//  if (DataController <> nil) then
//    DataController.DataModeController.GridMode := IsUseLookupList;
end;}

procedure TcxCustomBoldLookupEditProperties.DefaultValuesProviderDestroyed;
begin
  inherited DefaultValuesProviderDestroyed;
  BeginUpdate;
  try
    Changed;
  finally
    EndUpdate(False);
  end;
end;

procedure TcxCustomBoldLookupEditProperties.DefineByLookupError;
begin
  Assert(false);
//  DatabaseError(SPropDefByLookup);
end;

procedure TcxCustomBoldLookupEditProperties.DoChanged;
begin
//  CheckLookupList;
  CheckLookup;
  CheckLookupColumn;
  inherited;

  if (owner is TcxCustomEdit) and TcxCustomEdit(owner).IsDesigning and not TcxCustomEdit(owner).IsLoading and Assigned(TcxCustomEditAccess(owner).FDataBinding) then
  begin
    _ValidateEdit(TcxCustomEdit(owner));
  end;
end;

function TcxCustomBoldLookupEditProperties.IsPickMode: Boolean;
begin
  Result := (DropDownListStyle = lsEditList) and CanDisplayArbitraryEditValue;
end;

procedure TcxCustomBoldLookupEditProperties.LockDataChanged;
begin
  inherited LockDataChanged;
  // TODO: if GridMode
  if (DataController <> nil) and DataController.IsGridMode then
    Inc(FLockGridModeCount);
  if FLockGridModeCount <> 0 then
    DataController.LockGridModeNotify;
end;

procedure TcxCustomBoldLookupEditProperties.LookupSourceFreeNotification(Sender: TComponent);
begin
  CheckLookup;
end;

procedure TcxCustomBoldLookupEditProperties.SetDisplayColumnIndex(Value: Integer);
begin
  if IsDefinedByLookup then
    DefineByLookupError;
  inherited SetDisplayColumnIndex(Value);
end;

procedure TcxCustomBoldLookupEditProperties.UnlockDataChanged;
begin
  if FLockGridModeCount <> 0 then
  begin
    if DataController <> nil then
      DataController.UnlockGridModeNotify;
    Dec(FLockGridModeCount);
  end;
  inherited UnlockDataChanged;
end;

function TcxCustomBoldLookupEditProperties.FindByText(AItemIndex: Integer;
  const AText: string; APartialCompare: Boolean): Integer;

{  function GetLocateOptions: TLocateOptions;
  begin
    Result := [];
    if not CaseSensitiveSearch then
      Include(Result, loCaseInsensitive);
    if APartialCompare then
      Result := Result + [loPartialKey];
  end;

  function GetLocateValue: Variant;
  begin
    Result := AText;
    // TDataSet.Locate does not work with empty strings passed as key values for numeric fields
    if (AText = '') and not (DataController.GetItemField(AItemIndex) is TStringField) then
      Result := Null;
  end;
}
//var
//  ADataSet: TDataSet;
//  AListFieldName: string;
begin
//  if not IsUseLookupList then
  begin
    Result := inherited FindByText(AItemIndex, AText, APartialCompare);
    Exit;
  end;
  Result := DataController.GetFocusedRecordIndex;
{
  Result := -1;
  LockDataChanged;
  try
//    ADataSet := DataController.DataSet;
    AListFieldName := DataController.GetItemFieldName(AItemIndex);
    try
//      if (ADataSet <> nil) and ADataSet.Active and (AItemIndex <> -1) and
//        ADataSet.Locate(AListFieldName, GetLocateValue, GetLocateOptions) then
          Result := DataController.GetFocusedRecordIndex;
    except
      on EDatabaseError do;
      on EVariantError do;
    end;
  finally
    UnlockDataChanged;
  end;
}
end;

function TcxCustomBoldLookupEditProperties.GetDisplayColumnIndex: Integer;
begin
  Result := inherited GetDisplayColumnIndex;
end;

function TcxCustomBoldLookupEditProperties.GetDisplayLookupText(const AKey: TcxEditValue): string;
var
  ARecordIndex: Integer;
  AItemIndex: Integer;
begin
  Result := '';
  AItemIndex := GetListIndex;
  if (AItemIndex <> -1) and (DataController <> nil) then
  begin
    ARecordIndex := GetRecordIndexByKey(AKey);
    if (ARecordIndex <> -1) then
      Result := DataController.DisplayTexts[ARecordIndex, AItemIndex]
  end
  else
  begin
    if VarIsStr(AKey) then
      Result := AKey
    else
      Result := inherited GetDisplayLookupText(AKey);
  end;
end;

function TcxCustomBoldLookupEditProperties.GetDefaultHorzAlignment: TAlignment;
begin
  Result := inherited GetDefaultHorzAlignment;
end;

function TcxCustomBoldLookupEditProperties.GetDefaultMaxLength: Integer;
begin
  Result := inherited GetDefaultMaxLength;
end;

function TcxCustomBoldLookupEditProperties.GetIncrementalFiltering: Boolean;
begin
  Result := inherited GetIncrementalFiltering;
end;

class function TcxCustomBoldLookupEditProperties.GetLookupDataClass: TcxInterfacedPersistentClass;
begin
  Result := TcxCustomBoldLookupEditLookupData;
end;

function TcxCustomBoldLookupEditProperties.GetNullKey: Variant;
begin
  Result := Null;
end;

function TcxCustomBoldLookupEditProperties.GetRecordIndexByKey(const AKey: Variant): Integer;
begin
  if VarIsNull(AKey) or VarIsEmpty(AKey) or not VarIsType(AKey, varInteger) or (AKey >= DataController.RecordCount) then
//  or VarIsType(AKey, varSmallint) or VarIsType(AKey, varShortInt) or VarIsType(AKey, varInt64)) then
    result := -1
  else
    Result := AKey;
end;

{function TcxCustomBoldLookupEditProperties.GetIsUseLookupList: Boolean;
begin
  Result := FLookupList <> nil;
end;}

{procedure TcxCustomBoldLookupEditProperties.SetIsUseLookupList(Value: Boolean);
begin
  if (FLookupList <> nil) <> Value then
  begin
    if Value then
    begin
      FLookupList := TcxLookupList.Create;
    end
    else
    begin
      FLookupList.Free;
      FLookupList := nil;
    end;
    Changed;
  end;
end;}

function TcxCustomBoldLookupEditProperties.GetKeyByRecordIndex(
  ARecordIndex: Integer): Variant;
begin
  if (ARecordIndex <> -1) and (DataController <> nil) then
    Result := DataController.GetRecordId(ARecordIndex)
  else
    Result := Null;
end;

procedure TcxCustomBoldLookupEditProperties.SetStoredValue(
  aValue: Variant; aBoldHandle: TBoldElementHandle; aEdit: TcxCustomEdit; aFollower: TBoldFollower; var aDone: boolean);
begin
// TODO: nothing to do here, subclasse will override, perhaps even make this abstract ?
end;

procedure TcxCustomBoldLookupEditProperties.SetBoldSelectChangeAction(
  const Value: TBoldComboSelectChangeAction);
begin
  fBoldSelectChangeAction := Value;
end;

function TcxCustomBoldLookupEditProperties.GetListFieldIndex: Integer;
begin
  Result := inherited DisplayColumnIndex;
end;

procedure TcxCustomBoldLookupEditProperties.SetListFieldIndex(
  Value: Integer);
begin
  inherited DisplayColumnIndex := Value;
end;

function TcxCustomBoldLookupEditProperties.BoldElementToEditValue(
  aFollower: TBoldFollower; aElement: TBoldElement; aEdit: TcxCustomEdit): variant;
var
  lBoldList: TBoldList;
begin
  result := Null; // used to be -1
  if Assigned(DataController) and Assigned(DataController.BoldHandle) and Assigned(aElement) then
  begin
    lBoldList := DataController.BoldHandle.List;
    if Assigned(lBoldList) then
    begin
      if aElement is TBoldObjectReference then
        result := lBoldList.IndexOf( TBoldObjectReference(aElement).BoldObject )
      else
        result := lBoldList.IndexOf(aElement);
    end;
  end;
end;

type
  TcxBoldEditDataBindingAccess = class(TcxBoldEditDataBinding);
  TBoldFollowerControllerAccess = class(TBoldFollowerController);

function TcxCustomBoldLookupEditProperties.ValidateComponent(
  ComponentValidator: TBoldComponentValidator;
  NamePrefix: string): Boolean;
var
  lContext: TBoldElementTypeInfo;
  lName: string;
  lcxBoldEditDataBinding: TcxBoldEditDataBinding;
begin
  if (Owner is TComponent) and (TComponent(Owner).Name <> '') then
    lName := TComponent(Owner).Name
  else
    lName := ClassName;

  result := Assigned(DataController);
  if Assigned(DataController) then
  begin
    lContext := DataController.GetHandleStaticType;
    if assigned(lContext) then
    begin
      result := ComponentValidator.ValidateExpressionInContext(
        TBoldFollowerControllerAccess(DataController.BoldProperties).Expression,
        lContext,
        format('%s %s.BoldProperties.Expression', [NamePrefix, lName]), DataController.BoldProperties.VariableList) and result; // do not localize
      if (BoldSelectChangeAction = bdcsSetValue) and (Owner is TcxCustomEdit) then
      begin
        lcxBoldEditDataBinding := TcxCustomEditAccess(TcxCustomEdit(Owner)).DataBinding as TcxBoldEditDataBinding;
        lContext := TcxBoldEditDataBindingAccess(lcxBoldEditDataBinding).GetContextType;
        result := ComponentValidator.ValidateExpressionInContext(
          BoldSetValueExpression,
          lContext,
          format('%s %s.BoldSetValueExpression', [NamePrefix, lName]),
          lcxBoldEditDataBinding.BoldProperties.VariableList) and result; // do not localize
      end;
    end;
  end;
end;

function TcxCustomBoldLookupEditProperties.CanEdit(
  aBoldHandle: TBoldElementHandle; aFollower: TBoldFollower): boolean;
begin
  result := false;
end;

procedure TcxCustomBoldLookupEditProperties.SetBoldSetValueExpression(
  const Value: TBoldExpression);
begin
  fBoldSetValueExpression := Value;
  if Owner is TcxCustomEdit and (TcxCustomEdit(Owner).IsDesigning) and not (TcxCustomEdit(Owner).IsLoading) then
  begin
    _ValidateEdit(TcxCustomEdit(Owner));
  end;  
end;

{ TcxCustomBoldLookupEdit }

class function TcxCustomBoldLookupEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomBoldLookupEditProperties;
end;

function TcxCustomBoldLookupEdit.GetClearValue: TcxEditValue;
begin
  Result := inherited GetClearValue;
end;

function TcxCustomBoldLookupEdit.IsValidChar(AChar: Char): Boolean;
begin
  Result := True;
end;

function TcxCustomBoldLookupEdit.ItemIndexToLookupKey(AItemIndex: Integer): TcxEditValue;
begin
  Assert(false);
end;

function TcxCustomBoldLookupEdit.LookupKeyToEditValue(const AKey: TcxEditValue): TcxEditValue;
begin
  Result := AKey;
end;

{function TcxCustomBoldLookupEdit.LookupKeyToItemIndex(const AKey: TcxEditValue): Integer;
begin
  Assert(false);
end;}

procedure TcxCustomBoldLookupEdit.PopupWindowClosed(Sender: TObject);
begin
//  if ActiveProperties.DataController.DataModeController.SyncMode then
    ILookupData.CurrentKey := EditValue;
  inherited PopupWindowClosed(Sender);
end;

procedure TcxCustomBoldLookupEdit.PrepareDisplayValue(
  const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue;
  AEditFocused: Boolean);
begin
  if (ActiveProperties.DropDownListStyle <> lsEditList) and not Focused and
    ActiveProperties.CanDisplayArbitraryEditValue then
      DisplayValue := VarToStr(AEditValue)
  else
    ActiveProperties.PrepareDisplayValue(AEditValue, DisplayValue, AEditFocused);
end;

function TcxCustomBoldLookupEdit.GetProperties: TcxCustomBoldLookupEditProperties;
begin
  Result := TcxCustomBoldLookupEditProperties(FProperties);
end;

function TcxCustomBoldLookupEdit.GetActiveProperties: TcxCustomBoldLookupEditProperties;
begin
  Result := TcxCustomBoldLookupEditProperties(InternalGetActiveProperties);
end;

procedure TcxCustomBoldLookupEdit.SetProperties(Value: TcxCustomBoldLookupEditProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxBoldLookupEditDataBinding }

function TcxBoldLookupEditDataBinding.IsLookupControl: Boolean;
begin
  Result := True;
end;

end.
