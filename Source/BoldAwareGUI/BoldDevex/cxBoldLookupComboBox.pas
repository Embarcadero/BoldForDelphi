unit cxBoldLookupComboBox;

{$I cxVer.inc}

//  v2.03 - 25 Jan 2011  2007-2011 Daniel Mauric

interface

uses
{$IFDEF DELPHI6}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Controls, Graphics,
  cxClasses, cxContainer, cxCustomData, cxDataStorage,
  cxLookAndFeels, cxEdit,
  cxEditConsts, cxDropDownEdit,
  cxLookupEdit,
  cxBoldLookupEdit,
  cxLookupGrid,
  cxLookupBoldGrid,
  cxFilterControlUtils,

  cxGridBoldSupportUnit,
  cxBoldEditors,

  BoldListHandleFollower,
  BoldComboBox, // TODO: it's only neede for TBoldComboListController, perhaps we can replace it
  BoldControlPack,
//  BoldStringControlPack,
  BoldAbstractListHandle,
  BoldElements,
  BoldHandles,
  BoldComponentValidator;

type
  { TcxBoldLookupComboBoxProperties }

  TcxBoldLookupComboBoxProperties = class(TcxCustomBoldLookupEditProperties)
  private
    FGrid: TcxCustomLookupBoldGrid;
    function GetBoldListHandle: TBoldAbstractListHandle;
    function GetGrid: TcxCustomLookupBoldGrid;
//    function GetGridMode: Boolean;
    function GetListColumns: TcxLookupBoldGridColumns;
    function GetListOptions: TcxLookupBoldGridOptions;
//    function GetListSource: TDataSource;
    function GetOnSortingChanged: TNotifyEvent;
//    procedure SetGridMode(Value: Boolean);
    procedure SetListColumns(Value: TcxLookupBoldGridColumns);
    procedure SetListOptions(Value: TcxLookupBoldGridOptions);
//    procedure SetListSource(Value: TDataSource);
    procedure SetOnSortingChanged(Value: TNotifyEvent);
    procedure SetBoldListHandle(const Value: TBoldAbstractListHandle);
  protected
    function GetLookupGridClass: TcxCustomLookupBoldGridClass; virtual;
    procedure ListOptionsChanged(Sender: TObject); virtual;
    // LookupGrid methods
    function GetLookupGridColumnCount: Integer; override;
    function GetLookupGridControl: TWinControl; override;
    function GetLookupGridDataController: TcxCustomDataController; override;
    function GetLookupGridVisualAreaPreferredWidth: Integer; override;
    function GetLookupGridNearestPopupHeight(AHeight: Integer): Integer; override;
    function GetLookupGridPopupHeight(ADropDownRowCount: Integer): Integer; override;
    function IsLookupGridMouseOverList(const P: TPoint): Boolean; override;
    procedure LookupGridInitEvents(AOnClick, AOnFocusedRowChanged: TNotifyEvent;
      AOnCloseUp: cxLookupEdit.TcxLookupGridCloseUpEvent); override;
    procedure LookupGridInitLookAndFeel(ALookAndFeel: TcxLookAndFeel; AColor: TColor; AFont: TFont); override;
    procedure LookupGridLockMouseMove; override;
    procedure LookupGridMakeFocusedRowVisible; override;
    procedure LookupGridUnlockMouseMove; override;
    // BoldLookupGrid methods
    procedure BoldLookupGridBeginUpdate; override;
//    procedure BoldLookupGridCheckColumnByFieldName(const AFieldName: string); override;
//    procedure BoldLookupGridCreateColumnsByFieldNames(const AFieldNames: string); override;
    procedure BoldLookupGridEndUpdate; override;
//    function GetBoldLookupGridColumnField(AIndex: Integer): TField; override;
//    function GetBoldLookupGridColumnFieldName(AIndex: Integer): string; override;
//    function GetBoldLookupGridColumnIndexByFieldName(const AFieldName: string): Integer; override;
    function GetBoldLookupGridDataController: TcxBoldDataController; override;
    procedure SetStoredValue(aValue: Variant; aBoldHandle: TBoldElementHandle; aEdit: TcxCustomEdit; aFollower: TBoldFollower; var aDone: boolean); override;
    function CanEdit(aBoldHandle: TBoldElementHandle; aFollower: TBoldFollower): boolean; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    property Grid: TcxCustomLookupBoldGrid read GetGrid;
//    property LookupListFollower: TBoldFollower read GetListFollower;
  published
    property BoldLookupListHandle: TBoldAbstractListHandle read GetBoldListHandle write SetBoldListHandle;
//    property BoldLookupListProperties: TBoldComboListController read fBoldListProperties write SetBoldListProperties;
//    property BoldRowProperties: TBoldStringFollowerController read fBoldRowProperties write SetRowProperties;

    property BoldSelectChangeAction;
    property BoldSetValueExpression;

    property Alignment;
    property AutoSelect;
    property AssignedValues;
    property ButtonGlyph;
    property CaseSensitiveSearch;
    property CharCase;
    property ClearKey;
    property DropDownAutoSize;
    property DropDownHeight;
    property DropDownListStyle;
    property DropDownRows;
    property DropDownSizeable;
    property DropDownWidth;
//    property GridMode: Boolean read GetGridMode write SetGridMode default False;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property ImmediateDropDown;
//    property ImmediatePost;
    property IncrementalFiltering;
    property IncrementalFilteringOptions;
//    property KeyFieldNames;
    property ListColumns: TcxLookupBoldGridColumns read GetListColumns write SetListColumns;
//    property ListFieldNames;
    property ListFieldIndex;
    property ListOptions: TcxLookupBoldGridOptions read GetListOptions write SetListOptions;
//    property ListSource: TDataSource read GetListSource write SetListSource;
    property MaxLength;
    property OEMConvert;
    property PopupAlignment;
    property PostPopupValueOnTab;
    property ReadOnly;
    property Revertable;
    property UseLeftAlignmentOnEditing;
    property ValidateOnEnter;
    property OnChange;
    property OnCloseUp;
    property OnEditValueChanged;
    property OnInitPopup;
    property OnNewLookupDisplayText;
    property OnPopup;
    property OnSortingChanged: TNotifyEvent read GetOnSortingChanged write SetOnSortingChanged;
    property OnValidate;
  end;

  { TcxCustomBoldLookupComboBox }

  TcxCustomBoldLookupComboBox = class(TcxCustomBoldLookupEdit)
  private
    function GetProperties: TcxBoldLookupComboBoxProperties;
    function GetActiveProperties: TcxBoldLookupComboBoxProperties;
    procedure SetProperties(Value: TcxBoldLookupComboBoxProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxBoldLookupComboBoxProperties
      read GetActiveProperties;
    property EditValue;
    property Properties: TcxBoldLookupComboBoxProperties read GetProperties
      write SetProperties;
    property Text;
  end;

  { TcxBoldLookupComboBox }

  TcxBoldNBLookupComboBox = class(TcxCustomBoldLookupComboBox)
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties;
    property EditValue;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
{$IFDEF DELPHI5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TcxBoldLookupComboBox }

  TcxBoldLookupComboBox = class(TcxCustomBoldLookupComboBox, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetDataBinding: TcxBoldTextEditDataBinding;
    procedure SetDataBinding(Value: TcxBoldTextEditDataBinding);
//    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure Initialize; override;
    procedure Paint; override;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DataBinding: TcxBoldTextEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
{$IFDEF DELPHI5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TcxBoldFilterLookupComboBoxHelper }

  TcxBoldFilterLookupComboBoxHelper = class(TcxFilterComboBoxHelper)
  protected
    class function IsIDefaultValuesProviderNeeded(
      AEditProperties: TcxCustomEditProperties): Boolean; override;
  public
    class function GetFilterEditClass: TcxCustomEditClass; override;
    class procedure GetFilterValue(AEdit: TcxCustomEdit; AEditProperties: TcxCustomEditProperties;
      var V: Variant; var S: TCaption); override;
    class function GetSupportedFilterOperators(
      AProperties: TcxCustomEditProperties;
      AValueTypeClass: TcxValueTypeClass;
      AExtendedSet: Boolean = False): TcxFilterControlOperators; override;
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
    class function IsValueValid(AValueTypeClass: TcxValueTypeClass;
      var Value: Variant): Boolean; override;
  end;

implementation

uses
  cxTextEdit,

  BoldSystem,
  BoldQueue;

type
  TControlAccess = class(TControl);

{ TcxBoldLookupComboBoxProperties }

constructor TcxBoldLookupComboBoxProperties.Create(AOwner: TPersistent);
//var
//  lMatchObject: TComponent;
begin
  inherited Create(AOwner);
{  if aOwner is TComponent then
    lMatchObject := aOwner as TComponent
  else
    lMatchObject := nil;
}    
  FGrid := GetLookupGridClass.Create(nil);
  FGrid.IsPopupControl := True;
  FGrid.Options.OnChanged := ListOptionsChanged;
  InitializeDataController;
end;

destructor TcxBoldLookupComboBoxProperties.Destroy;
begin
  DeinitializeDataController;
  FGrid.Free;
  FGrid := nil;
  inherited Destroy;
end;

procedure TcxBoldLookupComboBoxProperties.Assign(Source: TPersistent);
begin
  if Source is TcxBoldLookupComboBoxProperties then
  begin
    BeginUpdate;
    try
//      GridMode := TcxBoldLookupComboBoxProperties(Source).GridMode;
      ListOptions := TcxBoldLookupComboBoxProperties(Source).ListOptions;
      if not IsDefinedByLookup then
      begin
        BoldSetValueExpression := TcxBoldLookupComboBoxProperties(Source).BoldSetValueExpression;
        BoldSelectChangeAction := TcxBoldLookupComboBoxProperties(Source).BoldSelectChangeAction;
        BoldLookupListHandle := TcxBoldLookupComboBoxProperties(Source).BoldLookupListHandle;
//        ListSource := TcxBoldLookupComboBoxProperties(Source).ListSource;
        ListColumns := TcxBoldLookupComboBoxProperties(Source).ListColumns;
      end;
      OnSortingChanged := TcxBoldLookupComboBoxProperties(Source).OnSortingChanged;
      inherited Assign(Source);
      if IsDefinedByLookup then
        ListColumns := TcxBoldLookupComboBoxProperties(Source).ListColumns;
      // DisplayAll is needed to make sure DataController is up to date,
      // otherwise it will get updated in next onIdle and by then it will be too late and it will be interpreted as a modification
      TBoldQueueable.DisplayAll;
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

class function TcxBoldLookupComboBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxBoldLookupComboBox;
end;

function TcxBoldLookupComboBoxProperties.GetLookupGridClass: TcxCustomLookupBoldGridClass;
begin
  Result := TcxCustomLookupBoldGrid;
end;

procedure TcxBoldLookupComboBoxProperties.ListOptionsChanged(Sender: TObject);
begin
  Changed;
end;

// LookupGrid

function TcxBoldLookupComboBoxProperties.GetLookupGridColumnCount: Integer;
begin
  Result := ListColumns.Count;
end;

function TcxBoldLookupComboBoxProperties.GetLookupGridControl: TWinControl;
begin
  Result := Grid;
end;

function TcxBoldLookupComboBoxProperties.GetLookupGridDataController: TcxCustomDataController;
begin
  Result := Grid.DataController;
end;

function TcxBoldLookupComboBoxProperties.GetLookupGridVisualAreaPreferredWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ListColumns.Count - 1 do
    Inc(Result, ListColumns[I].Width);
end;

function TcxBoldLookupComboBoxProperties.GetLookupGridNearestPopupHeight(
  AHeight: Integer): Integer;
begin
  Result := Grid.GetNearestPopupHeight(AHeight);
end;

function TcxBoldLookupComboBoxProperties.GetLookupGridPopupHeight(ADropDownRowCount: Integer): Integer;
begin
  Result := Grid.GetPopupHeight(ADropDownRowCount);
end;

function TcxBoldLookupComboBoxProperties.IsLookupGridMouseOverList(const P: TPoint): Boolean;
begin
  Result := Grid.IsMouseOverList(P);
end;

procedure TcxBoldLookupComboBoxProperties.LookupGridInitEvents(AOnClick, AOnFocusedRowChanged: TNotifyEvent;
  AOnCloseUp: cxLookupEdit.TcxLookupGridCloseUpEvent);
begin
  Grid.OnClick := AOnClick;
  Grid.OnFocusedRowChanged := AOnFocusedRowChanged;
  Grid.OnCloseUp := AOnCloseUp;
end;

procedure TcxBoldLookupComboBoxProperties.LookupGridInitLookAndFeel(ALookAndFeel: TcxLookAndFeel;
  AColor: TColor; AFont: TFont);
begin
  Grid.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  Grid.Color := AColor;
  Grid.Font := AFont;
end;

procedure TcxBoldLookupComboBoxProperties.LookupGridLockMouseMove;
begin
  Grid.LockPopupMouseMove;
end;

procedure TcxBoldLookupComboBoxProperties.LookupGridMakeFocusedRowVisible;
begin
  Grid.MakeFocusedRowVisible;
end;

procedure TcxBoldLookupComboBoxProperties.LookupGridUnlockMouseMove;
begin
  TControlAccess(Grid).MouseCapture := False;
end;

// BoldLookupGrid

procedure TcxBoldLookupComboBoxProperties.BoldLookupGridBeginUpdate;
begin
  Grid.BeginUpdate;
end;

procedure TcxBoldLookupComboBoxProperties.BoldLookupGridEndUpdate;
begin
  Grid.EndUpdate;
end;

function TcxBoldLookupComboBoxProperties.GetBoldLookupGridDataController: TcxBoldDataController;
begin
  if Grid <> nil then
    Result := Grid.DataController
  else
    Result := nil;
end;

function TcxBoldLookupComboBoxProperties.GetGrid: TcxCustomLookupBoldGrid;
begin
  Result := FGrid;
end;

{function TcxBoldLookupComboBoxProperties.GetGridMode: Boolean;
begin
  Result := inherited IsUseLookupList;
end;}

function TcxBoldLookupComboBoxProperties.GetListColumns: TcxLookupBoldGridColumns;
begin
  Result := Grid.Columns;
end;

function TcxBoldLookupComboBoxProperties.GetListOptions: TcxLookupBoldGridOptions;
begin
  Result := Grid.Options;
end;

function TcxBoldLookupComboBoxProperties.GetOnSortingChanged: TNotifyEvent;
begin
  Result := Grid.DataController.OnSortingChanged;
end;

{procedure TcxBoldLookupComboBoxProperties.SetGridMode(Value: Boolean);
begin
  inherited IsUseLookupList := Value;
end;}

procedure TcxBoldLookupComboBoxProperties.SetListColumns(Value: TcxLookupBoldGridColumns);
begin
  Grid.Columns := Value; // TODO: recreate?
  CheckLookupColumn;
  CheckDisplayColumnIndex;
end;

procedure TcxBoldLookupComboBoxProperties.SetListOptions(Value: TcxLookupBoldGridOptions);
begin
  Grid.Options := Value;
end;

procedure TcxBoldLookupComboBoxProperties.SetOnSortingChanged(Value: TNotifyEvent);
begin
  Grid.DataController.OnSortingChanged := Value;
end;

function TcxBoldLookupComboBoxProperties.GetBoldListHandle: TBoldAbstractListHandle;
begin
  result := DataController.BoldHandle;
end;

procedure TcxBoldLookupComboBoxProperties.SetBoldListHandle(
  const Value: TBoldAbstractListHandle);
begin
  DataController.BoldHandle := Value;
end;

procedure TcxBoldLookupComboBoxProperties.SetStoredValue(
  aValue: Variant; aBoldHandle: TBoldElementHandle; aEdit: TcxCustomEdit; aFollower: TBoldFollower; var aDone: boolean);
var
  lItemIndex: Integer;
  lSelectedElement: TBoldElement;
begin
//  inherited;

//  if TcxCustomComboBox(aEdit).ILookupData.CurrentKey <> null then
  if (not VarIsNull(aValue)) and (aValue <> -1) then
  begin
    lItemIndex := aValue; //(aEdit as TcxCustomComboBox).ItemIndex;
    lSelectedElement := DataController.BoldHandle.List[lItemIndex]; //(DataController.Follower.Element as TBoldList)[lItemIndex];

    InternalComboSetValue(aBoldHandle, aFollower, lSelectedElement, BoldSelectChangeAction, BoldSetValueExpression, BoldLookupListHandle, AValue);
  //  aEdit.EditModified := false
  end;
  aDone := true;
end;

function TcxBoldLookupComboBoxProperties.CanEdit(
  aBoldHandle: TBoldElementHandle; aFollower: TBoldFollower): boolean;
begin
  result := Assigned(BoldLookupListHandle) and (BoldLookupListHandle.Count > 0);
end;

{ TcxCustomBoldLookupComboBox }

class function TcxCustomBoldLookupComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxBoldLookupComboBoxProperties;
end;

function TcxCustomBoldLookupComboBox.GetProperties: TcxBoldLookupComboBoxProperties;
begin
  Result := TcxBoldLookupComboBoxProperties(FProperties);
end;

function TcxCustomBoldLookupComboBox.GetActiveProperties: TcxBoldLookupComboBoxProperties;
begin
  Result := TcxBoldLookupComboBoxProperties(InternalGetActiveProperties);
end;

procedure TcxCustomBoldLookupComboBox.SetProperties(Value: TcxBoldLookupComboBoxProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxBoldLookupComboBox }

class function TcxBoldLookupComboBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxBoldLookupEditDataBinding;
end;

function TcxBoldLookupComboBox.GetDataBinding: TcxBoldTextEditDataBinding;
begin
  Result := TcxBoldTextEditDataBinding(FDataBinding);
end;

procedure TcxBoldLookupComboBox.SetDataBinding(Value: TcxBoldTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

{procedure TcxBoldLookupComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(GetcxDBEditDataLink(Self));
end;}

type
  TcxBoldTextEditDataBindingAccess = class(TcxBoldTextEditDataBinding);

procedure TcxBoldLookupComboBox.Paint;
begin
  inherited Paint;
  if TcxBoldTextEditDataBindingAccess(DataBinding).ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

procedure TcxBoldLookupComboBox.Initialize;
begin
  inherited;
  if IsDesigning and not IsLoading then
  begin
    _ValidateEdit(self);
  end;
end;

{ TcxBoldFilterLookupComboBoxHelper }

class function TcxBoldFilterLookupComboBoxHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := TcxBoldLookupComboBox;
end;

class procedure TcxBoldFilterLookupComboBoxHelper.GetFilterValue(AEdit: TcxCustomEdit;
  AEditProperties: TcxCustomEditProperties; var V: Variant; var S: TCaption);
begin
  V := AEdit.EditValue;
  S := TcxCustomTextEdit(AEdit).ILookupData.GetDisplayText(V);
end;

class function TcxBoldFilterLookupComboBoxHelper.GetSupportedFilterOperators(
  AProperties: TcxCustomEditProperties;
  AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  Result := [fcoEqual, fcoNotEqual, fcoBlanks, fcoNonBlanks];
end;

class procedure TcxBoldFilterLookupComboBoxHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  with TcxCustomLookupEditProperties(AProperties) do
  begin
    DropDownAutoSize := True;
    DropDownListStyle := lsFixedList;
    DropDownSizeable := True;
    IncrementalFiltering := True;
  end;
end;

class function TcxBoldFilterLookupComboBoxHelper.IsValueValid(AValueTypeClass: TcxValueTypeClass;
  var Value: Variant): Boolean;
begin
  Result := True;
end;

class function TcxBoldFilterLookupComboBoxHelper.IsIDefaultValuesProviderNeeded(
  AEditProperties: TcxCustomEditProperties): Boolean;
begin
  Result := TcxCustomBoldLookupEditProperties(AEditProperties).IsDefinedByLookup;
end;

initialization
//  scxSEditRepositoryLookupComboBoxItem = 'LookupComboBox|Represents a lookup combo box control';
//  InternalAdd('scxSEditRepositoryLookupComboBoxItem', @scxSEditRepositoryLookupComboBoxItem);
// TODO: extract sting to a resourcestring like above
  GetRegisteredEditProperties.Register(TcxBoldLookupComboBoxProperties, 'BoldLookupComboBox|Represents a bold aware lookup combo box control');
  FilterEditsController.Register(TcxBoldLookupComboBoxProperties, TcxBoldFilterLookupComboBoxHelper);

finalization
  FilterEditsController.Unregister(TcxBoldLookupComboBoxProperties, TcxBoldFilterLookupComboBoxHelper);

end.
