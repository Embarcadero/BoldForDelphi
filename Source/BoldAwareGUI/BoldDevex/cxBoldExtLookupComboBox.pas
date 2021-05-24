unit cxBoldExtLookupComboBox;

{$I cxVer.inc}

//  v2.03 - 25 Jan 2011  2007-2011 Daniel Mauric

interface

uses
{$IFDEF DELPHI6}
  Variants,
{$ENDIF}
  Windows, Classes, Controls,
  Graphics, Messages, SysUtils, cxClasses,
  cxContainer, cxControls, cxCustomData,
  cxBoldLookupEdit,
  cxEditConsts, cxGrid, cxGridCustomTableView, cxEdit,
  cxGridCustomView, cxGridStrs, cxGridTableView, cxLookAndFeels, cxLookupEdit,

  BoldElements,
  cxBoldEditors,
  cxGridBoldSupportUnit,
  BoldControlPack,
  BoldHandles,
  BoldComponentValidator;

type
  { TcxBoldExtLookupGrid }

  TcxBoldExtLookupGrid = class(TcxGrid)
  private
    FEditable: Boolean;
    FMousePos: TPoint;
    FPopupMouseMoveLocked: Boolean;
    FPrevOnKeyDown: TKeyEvent;
    FPrevOnMouseDown: TMouseEvent;
    FPrevOnMouseMove: TMouseMoveEvent;
    FPrevOnMouseUp: TMouseEvent;
    FRowPressed: Boolean;
    FOnCloseUp: TcxLookupGridCloseUpEvent;
    function GetView: TcxCustomGridTableView;
    procedure ViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    procedure DoCancelMode; override;
    procedure DoCloseUp(AAccept: Boolean); virtual;
    function IsDataRow(AHitTest: TcxCustomGridHitTest): Boolean;
    property Editable: Boolean read FEditable write FEditable;
    property PopupMouseMoveLocked: Boolean read FPopupMouseMoveLocked write FPopupMouseMoveLocked;
    property OnCloseUp: TcxLookupGridCloseUpEvent read FOnCloseUp write FOnCloseUp;
  public
    property View: TcxCustomGridTableView read GetView;
  end;

  { TcxCustomBoldExtLookupComboBoxProperties }

  TcxCustomBoldExtLookupComboBoxProperties = class(TcxCustomBoldLookupEditProperties);

  { TcxBoldExtLookupComboBoxProperties }

  TcxBoldExtLookupComboBoxProperties = class(TcxCustomBoldExtLookupComboBoxProperties)
  private
    FAutoSearchOnPopup: Boolean;
    FDestroying: Boolean;
    FFocusPopup: Boolean;
    FGrid: TcxBoldExtLookupGrid;
    FInCheckListFieldItem: Boolean;
    FListFieldItem: TcxCustomGridTableItem;
    FPrevColumnFiltering: Boolean;
    FPrevColumnsQuickCustomization: Boolean;
    FPrevPullFocusing: Boolean;
    FPrevImmediateEditor: Boolean;
    FPrevIncSearch: Boolean;
    FPrevMultiSelect: Boolean;
    FView: TcxCustomGridTableView;
    function GetGrid: TcxBoldExtLookupGrid;
//    function GetGridMode: Boolean;
    function GetListFieldIndex: Integer;
    function GetListFieldItem: TcxCustomGridTableItem;
//    procedure SetGridMode(Value: Boolean);
    procedure SetListFieldItem(Value: TcxCustomGridTableItem);
    procedure SetView(Value: TcxCustomGridTableView);
  protected
    // IcxBoldEditProperties
    procedure SetStoredValue(aValue: Variant; aBoldHandle: TBoldElementHandle; aEdit: TcxCustomEdit; aFollower: TBoldFollower; var aDone: boolean); override;
    function CanEdit(aBoldHandle: TBoldElementHandle; aFollower: TBoldFollower): boolean; override;

    procedure CheckListFieldItem;
    procedure DeinitializeDataController; override;
    procedure FreeNotification(Sender: TComponent); override;
    function GetIncrementalFiltering: Boolean; override;
    function GetListIndex: Integer; override;
    procedure InitializeDataController; override;
    procedure LinkView(AView: TcxCustomGridTableView);
    function PopupWindowCapturesFocus: Boolean; override;
    procedure UnlinkView(AView: TcxCustomGridTableView);
    // LookupGrid methods
    function GetLookupGridActiveControl: TWinControl; override;
    function GetLookupGridCanResize: Boolean; override;
    function GetLookupGridColumnCount: Integer; override;
    function GetLookupGridControl: TWinControl; override;
    function GetLookupGridDataController: TcxCustomDataController; override;
    function GetLookupGridVisualAreaPreferredWidth: Integer; override;
    function GetLookupGridNearestPopupHeight(AHeight: Integer): Integer; override;
    function GetLookupGridPopupHeight(ADropDownRowCount: Integer): Integer; override;
    function IsLookupGridMouseOverList(const P: TPoint): Boolean; override;
    procedure LookupGridDeinitialize; override;
    procedure LookupGridDroppedDown(const AFindStr: string); override;
    procedure LookupGridInitEvents(AOnClick, AOnFocusedRowChanged: TNotifyEvent;
      AOnCloseUp: TcxLookupGridCloseUpEvent); override;
    procedure LookupGridInitialize; override;
    procedure LookupGridInitLookAndFeel(ALookAndFeel: TcxLookAndFeel;
      AColor: TColor; AFont: TFont); override;
    procedure LookupGridLockMouseMove; override;
    procedure LookupGridMakeFocusedRowVisible; override;
    procedure LookupGridUnlockMouseMove; override;

    procedure BoldLookupGridBeginUpdate; override;
    procedure BoldLookupGridEndUpdate; override;

    // DBLookupGrid methods
{
    procedure DBLookupGridBeginUpdate; override;
    procedure DBLookupGridCheckColumnByFieldName(const AFieldName: string); override;
    procedure DBLookupGridCreateColumnsByFieldNames(const AFieldNames: string); override;
    procedure DBLookupGridEndUpdate; override;
    function GetDBLookupGridColumnField(AIndex: Integer): TField; override;
    function GetDBLookupGridColumnFieldName(AIndex: Integer): string; override;
    function GetDBLookupGridColumnIndexByFieldName(const AFieldName: string): Integer; override;
}
    function GetBoldLookupGridDataController: TcxBoldDataController; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    class function IsViewSupported(Value: TcxCustomGridTableView): Boolean;
    property Grid: TcxBoldExtLookupGrid read GetGrid;
    property ListFieldIndex: Integer read GetListFieldIndex;
  published
    property BoldSelectChangeAction;
    property BoldSetValueExpression;

    property Alignment;
    property AssignedValues;
    property AutoSearchOnPopup: Boolean read FAutoSearchOnPopup write FAutoSearchOnPopup default True;
    property AutoSelect;
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
    property FocusPopup: Boolean read FFocusPopup write FFocusPopup default False;
//    property GridMode: Boolean read GetGridMode write SetGridMode default False;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property ImmediateDropDown;
//    property ImmediatePost;
    property IncrementalFiltering;
    property IncrementalFilteringOptions;
    property View: TcxCustomGridTableView read FView write SetView; // before
//    property KeyFieldNames;
    property ListFieldItem: TcxCustomGridTableItem read GetListFieldItem write SetListFieldItem;
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
    property OnValidate;
  end;

  { TcxCustomBoldExtLookupComboBox }

  TcxCustomBoldExtLookupComboBox = class(TcxCustomBoldLookupEdit)
  private
    function GetActiveProperties: TcxBoldExtLookupComboBoxProperties;
    function GetProperties: TcxBoldExtLookupComboBoxProperties;
    procedure SetProperties(Value: TcxBoldExtLookupComboBoxProperties);
  protected
    function CanDropDown: Boolean; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxBoldExtLookupComboBoxProperties
      read GetActiveProperties;
    property EditValue;
    property Properties: TcxBoldExtLookupComboBoxProperties read GetProperties
      write SetProperties;
    property Text;
  end;

  { TcxBoldNBExtLookupComboBox }

  TcxBoldNBExtLookupComboBox = class(TcxCustomBoldExtLookupComboBox)
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
    property OnContextPopup;
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

  { TcxBoldExtLookupComboBox }

  TcxBoldExtLookupComboBox = class(TcxCustomBoldExtLookupComboBox, IBoldValidateableComponent, IBoldOCLComponent)
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
    property OnContextPopup;
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

  { TcxEditRepositoryExtLookupComboBoxItem }

  TcxEditRepositoryExtLookupComboBoxItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxBoldExtLookupComboBoxProperties;
    procedure SetProperties(Value: TcxBoldExtLookupComboBoxProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxBoldExtLookupComboBoxProperties read GetProperties write SetProperties;
  end;

implementation

uses
  Types,
  cxGridFilterHelpers,
  BoldSystem,
  cxDropDownEdit;

type
  TcxCustomGridTableOptionsBehaviorAccess = class(TcxCustomGridTableOptionsBehavior);
  TcxCustomGridTableOptionsViewAccess = class(TcxCustomGridTableOptionsView);

{ TcxBoldExtLookupGrid }

procedure TcxBoldExtLookupGrid.DoCancelMode;
begin
  FRowPressed := False;
  inherited;
end;

procedure TcxBoldExtLookupGrid.DoCloseUp(AAccept: Boolean);
begin
  if AAccept then
    View.DataController.SyncSelected(True);
  if Assigned(FOnCloseUp) then FOnCloseUp(Self, AAccept);
end;

function TcxBoldExtLookupGrid.IsDataRow(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := (AHitTest is TcxGridRecordHitTest) and
    TcxGridRecordHitTest(AHitTest).GridRecord.IsData; 
end;

function TcxBoldExtLookupGrid.GetView: TcxCustomGridTableView;
begin
  Result := Levels[0].GridView as TcxCustomGridTableView;
end;

procedure TcxBoldExtLookupGrid.ViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FPrevOnKeyDown) then
    FPrevOnKeyDown(Self, Key, Shift);
  if (View = nil) then Exit;
  case Key of
    VK_RETURN:
      if not Editable or not View.OptionsData.Editing or (ssCtrl in Shift) then
      begin
        if View.DataController.IsEditing then
          View.DataController.Post;
        DoCloseUp(View.DataController.FocusedRowIndex <> -1);
      end;
    VK_ESCAPE:
      if Editable and not View.DataController.IsEditing then
        DoCloseUp(False);
  end;
end;

procedure TcxBoldExtLookupGrid.ViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  if Assigned(FPrevOnMouseDown) then
    FPrevOnMouseDown(Sender, Button, Shift, X, Y);
  AHitTest := View.ViewInfo.GetHitTest(X, Y);
  if (Button = mbLeft) and IsDataRow(AHitTest) then
  begin
    if Editable then
    begin
      if ssDouble in Shift then
        DoCloseUp(True);
    end
    else
      FRowPressed := True;
  end;
end;

procedure TcxBoldExtLookupGrid.ViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  if Assigned(FPrevOnMouseMove) then
    FPrevOnMouseMove(Sender, Shift, X, Y);
  if not MouseCapture and PopupMouseMoveLocked then
  begin
    PopupMouseMoveLocked := False;
    Exit;
  end;
  // Hot Track
  if (View = nil) or Editable then Exit;
  AHitTest := View.ViewInfo.GetHitTest(X, Y);
  if IsDataRow(AHitTest) and ((FMousePos.X <> X) or (FMousePos.Y <> Y)) then
  begin
    FMousePos.X := X;
    FMousePos.Y := Y;
    TcxGridRecordHitTest(AHitTest).GridRecord.Focused := True;
  end;
end;

procedure TcxBoldExtLookupGrid.ViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TcxCustomGridHitTest;
begin
  if Assigned(FPrevOnMouseUp) then
    FPrevOnMouseUp(Sender, Button, Shift, X, Y);
  AHitTest := View.ViewInfo.GetHitTest(X, Y);
  if (Button = mbLeft) and FRowPressed then
    DoCloseUp(IsDataRow(AHitTest));
  FRowPressed := False;
end;

{ TcxBoldExtLookupComboBoxProperties }

constructor TcxBoldExtLookupComboBoxProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAutoSearchOnPopup := True;
end;

destructor TcxBoldExtLookupComboBoxProperties.Destroy;
begin
  FDestroying := True;
  ListFieldItem := nil;
  View := nil;
  FreeAndNil(FGrid);
  inherited Destroy;
end;

procedure TcxBoldExtLookupComboBoxProperties.Assign(Source: TPersistent);
begin
  if Source is TcxBoldExtLookupComboBoxProperties then
  begin
    BeginUpdate;
    try
      AutoSearchOnPopup := TcxBoldExtLookupComboBoxProperties(Source).AutoSearchOnPopup;
      FocusPopup := TcxBoldExtLookupComboBoxProperties(Source).FocusPopup;
      View := nil; //?
//      GridMode := TcxBoldExtLookupComboBoxProperties(Source).GridMode;
      View := TcxBoldExtLookupComboBoxProperties(Source).View;
      inherited Assign(Source);
      ListFieldItem := TcxBoldExtLookupComboBoxProperties(Source).ListFieldItem;
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

class function TcxBoldExtLookupComboBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxBoldNBExtLookupComboBox;
end;

class function TcxBoldExtLookupComboBoxProperties.IsViewSupported(Value: TcxCustomGridTableView): Boolean;
begin
  Result := Value.CanBeLookupList and
    (TcxCustomGridView(Value).DataController is TcxBoldDataController);
end;

procedure TcxBoldExtLookupComboBoxProperties.CheckListFieldItem;
begin
  FInCheckListFieldItem := True;
  try
    if (View <> nil) and (ListFieldItem <> nil) and
      (View.IndexOfItem(ListFieldItem) = -1) then
      ListFieldItem := nil;
  finally
    FInCheckListFieldItem := False;
  end;
end;

procedure TcxBoldExtLookupComboBoxProperties.DeinitializeDataController;
begin
  inherited DeinitializeDataController;
  if DataController <> nil then
    DataController.RemoveDataChangeRefCount;
end;

procedure TcxBoldExtLookupComboBoxProperties.FreeNotification(Sender: TComponent);
begin
  inherited FreeNotification(Sender);
  if Sender = ListFieldItem then
    ListFieldItem := nil;
  if Sender = View then
    View := nil;
end;

function TcxBoldExtLookupComboBoxProperties.GetIncrementalFiltering: Boolean;
begin
  if FocusPopup then
    Result := False
  else
    Result := inherited GetIncrementalFiltering;
end;

function TcxBoldExtLookupComboBoxProperties.GetListIndex: Integer;
begin
  Result := Self.ListFieldIndex;
end;

procedure TcxBoldExtLookupComboBoxProperties.InitializeDataController;
begin
  inherited InitializeDataController;
  if DataController <> nil then
    DataController.AddDataChangeRefCount;
end;

procedure TcxBoldExtLookupComboBoxProperties.LinkView(AView: TcxCustomGridTableView);
begin
  CheckListFieldItem;
  FreeNotificator.AddSender(AView);
  InitializeDataController;
end;

function TcxBoldExtLookupComboBoxProperties.PopupWindowCapturesFocus: Boolean;
begin
  Result := FocusPopup;
end;

procedure TcxBoldExtLookupComboBoxProperties.UnlinkView(AView: TcxCustomGridTableView);
begin
  DeinitializeDataController;
  FreeNotificator.RemoveSender(AView);
end;

// LookupGrid methods

function TcxBoldExtLookupComboBoxProperties.GetLookupGridActiveControl: TWinControl;
begin
  if View <> nil then
    Result := View.Site
  else
    Result := inherited GetLookupGridActiveControl;
end;

function TcxBoldExtLookupComboBoxProperties.GetLookupGridCanResize: Boolean;
begin
  if View <> nil then
    Result := not TcxCustomGridTableOptionsViewAccess(View.OptionsView).CellAutoHeight
  else
    Result := False;
end;

function TcxBoldExtLookupComboBoxProperties.GetLookupGridColumnCount: Integer;
begin
  if View <> nil then
    Result := View.ItemCount
  else
    Result := 0;
end;

function TcxBoldExtLookupComboBoxProperties.GetLookupGridControl: TWinControl;
begin
  Result := Grid;
end;

function TcxBoldExtLookupComboBoxProperties.GetLookupGridDataController: TcxCustomDataController;
begin
  if View <> nil then
    Result := View.DataController
  else
    Result := nil;
end;

function TcxBoldExtLookupComboBoxProperties.GetLookupGridVisualAreaPreferredWidth: Integer;
begin
  Result := 0;
  if View <> nil then
    View.ViewInfo.GetWidth(Point(MaxInt, MaxInt), Result);
end;

function TcxBoldExtLookupComboBoxProperties.GetLookupGridNearestPopupHeight(AHeight: Integer): Integer;
begin
  if View <> nil then
    Result := View.ViewInfo.GetNearestPopupHeight(AHeight, FocusPopup)
  else
    Result := AHeight;
end;

function TcxBoldExtLookupComboBoxProperties.GetLookupGridPopupHeight(ADropDownRowCount: Integer): Integer;
begin
  if View <> nil then
  begin
    if FocusPopup and (ADropDownRowCount < 2) then // TODO: Check New Item Row 
      ADropDownRowCount := 2;
    Result := View.ViewInfo.GetPopupHeight(ADropDownRowCount);
  end
  else
    Result := 0;
end;

function TcxBoldExtLookupComboBoxProperties.IsLookupGridMouseOverList(const P: TPoint): Boolean;
var
  AHitTest: TcxCustomGridHitTest;
begin
  Result := False;
  if View <> nil then
  begin
    AHitTest := View.ViewInfo.GetHitTest(P);
    Result := AHitTest is TcxGridRecordHitTest;
  end;
end;

procedure TcxBoldExtLookupComboBoxProperties.LookupGridDeinitialize;
begin
  Grid.Levels[0].GridView := nil;
  // restore options
  if (View <> nil) and not (csDestroying in View.ComponentState) then
  begin
    TcxCustomGridTableOptionsBehaviorAccess(View.OptionsBehavior).PullFocusing := FPrevPullFocusing;
    View.OptionsSelection.MultiSelect := FPrevMultiSelect;
    View.OptionsBehavior.ImmediateEditor := FPrevImmediateEditor;
    if View is TcxGridTableView then
    begin
      TcxGridTableView(View).OptionsCustomize.ColumnFiltering := FPrevColumnFiltering;
      TcxGridTableView(View).OptionsCustomize.ColumnsQuickCustomization := FPrevColumnsQuickCustomization;
    end;
    View.OptionsBehavior.IncSearch := FPrevIncSearch;
    View.OnKeyDown := Grid.FPrevOnKeyDown;
    View.OnMouseDown := Grid.FPrevOnMouseDown;
    View.OnMouseMove := Grid.FPrevOnMouseMove;
    View.OnMouseUp := Grid.FPrevOnMouseUp;
  end;
end;

procedure TcxBoldExtLookupComboBoxProperties.LookupGridDroppedDown(const AFindStr: string);
begin
  // Init Inc Search
  // TODO: !!!
  if FocusPopup and AutoSearchOnPopup and (ListFieldItem <> nil) then
  begin
    ListFieldItem.Focused := True;
    View.DataController.Search.Locate(ListFieldItem.Index, AFindStr);
  end;  
end;

procedure TcxBoldExtLookupComboBoxProperties.LookupGridInitEvents(AOnClick, AOnFocusedRowChanged: TNotifyEvent;
  AOnCloseUp: cxLookupEdit.TcxLookupGridCloseUpEvent);
begin
  Grid.OnClick := AOnClick; // not impl
  if View <> nil then
  begin
//    View.OnFocusedRecordChanged := AOnFocusedRowChanged;
    Grid.OnCloseUp := AOnCloseUp; 
  end;
end;

procedure TcxBoldExtLookupComboBoxProperties.LookupGridInitialize;
begin
  if View = nil then
    Exit;
  // save options
  FPrevPullFocusing := TcxCustomGridTableOptionsBehaviorAccess(View.OptionsBehavior).PullFocusing;
  TcxCustomGridTableOptionsBehaviorAccess(View.OptionsBehavior).PullFocusing := True;
  FPrevMultiSelect := View.OptionsSelection.MultiSelect;
  View.OptionsSelection.MultiSelect := False;
  FPrevImmediateEditor := View.OptionsBehavior.ImmediateEditor;
  View.OptionsBehavior.ImmediateEditor := False;
  if View is TcxGridTableView then
  begin
    FPrevColumnFiltering := TcxGridTableView(View).OptionsCustomize.ColumnFiltering;
    FPrevColumnsQuickCustomization := TcxGridTableView(View).OptionsCustomize.ColumnsQuickCustomization;
    if not FocusPopup then
    begin
      TcxGridTableView(View).OptionsCustomize.ColumnFiltering := False;
      TcxGridTableView(View).OptionsCustomize.ColumnsQuickCustomization := False;
    end;
  end;

  Grid.FPrevOnKeyDown := View.OnKeyDown;
  View.OnKeyDown := Grid.ViewKeyDown;

  Grid.FPrevOnMouseDown := View.OnMouseDown;
  View.OnMouseDown := Grid.ViewMouseDown;

  Grid.FMousePos := Point(-1, -1);
  Grid.FPrevOnMouseMove := View.OnMouseMove;
  View.OnMouseMove := Grid.ViewMouseMove;

  Grid.FPrevOnMouseUp := View.OnMouseUp;
  View.OnMouseUp := Grid.ViewMouseUp;

  Grid.Editable := FocusPopup;
  Grid.Levels[0].GridView := View;

  FPrevIncSearch := View.OptionsBehavior.IncSearch;
  if FocusPopup and AutoSearchOnPopup then
    View.OptionsBehavior.IncSearch := True;
  View.DataController.Search.Cancel;  
end;

procedure TcxBoldExtLookupComboBoxProperties.LookupGridInitLookAndFeel(ALookAndFeel: TcxLookAndFeel;
  AColor: TColor; AFont: TFont);
begin
  Grid.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  Grid.Color := AColor;
  Grid.Font := AFont;
end;

procedure TcxBoldExtLookupComboBoxProperties.LookupGridLockMouseMove;
begin
  Grid.PopupMouseMoveLocked := True;
end;

procedure TcxBoldExtLookupComboBoxProperties.LookupGridMakeFocusedRowVisible;
begin
  if View <> nil then
    View.Controller.MakeFocusedRecordVisible;
end;

procedure TcxBoldExtLookupComboBoxProperties.LookupGridUnlockMouseMove;
begin
  Grid.MouseCapture := False;
  Grid.PopupMouseMoveLocked := False;
end;

// DBLookupGrid methods
{
procedure TcxBoldExtLookupComboBoxProperties.DBLookupGridBeginUpdate;
begin
  if View <> nil then View.BeginUpdate;
end;

procedure TcxBoldExtLookupComboBoxProperties.DBLookupGridCheckColumnByFieldName(const AFieldName: string);
begin
  if (View <> nil) and (DataController <> nil) then
  begin
    if (AFieldName <> '') and (DataController.GetItemByFieldName(AFieldName) = nil) then
      with View.CreateItem do
      begin
        Index := 0;
        DataController.ChangeFieldName(Index, AFieldName);
      end;
  end;
end;

procedure TcxBoldExtLookupComboBoxProperties.DBLookupGridCreateColumnsByFieldNames(const AFieldNames: string);
var
  I: Integer;
  AFieldNamesList: TStrings;
begin
  if View <> nil then
  begin
    View.ClearItems;
    AFieldNamesList := TStringList.Create;
    try
      GetFieldNames(AFieldNames, AFieldNamesList);
      View.BeginUpdate;
      try
        for I := 0 to AFieldNamesList.Count - 1 do
          DataController.ChangeFieldName(View.CreateItem.Index, AFieldNamesList[I]);
      finally
        View.EndUpdate;
      end;
    finally
      AFieldNamesList.Free;
    end;
  end;
end;

procedure TcxBoldExtLookupComboBoxProperties.DBLookupGridEndUpdate;
begin
  if View <> nil then View.EndUpdate;
end;

function TcxBoldExtLookupComboBoxProperties.GetDBLookupGridColumnField(AIndex: Integer): TField;
begin
  if DataController <> nil then
    Result := DataController.GetItemField(AIndex)
  else
    Result := nil;
end;

function TcxBoldExtLookupComboBoxProperties.GetDBLookupGridColumnFieldName(AIndex: Integer): string;
begin
  if DataController <> nil then
    Result := DataController.GetItemFieldName(AIndex)
  else
    Result := '';
end;

function TcxBoldExtLookupComboBoxProperties.GetDBLookupGridColumnIndexByFieldName(const AFieldName: string): Integer;
var
  AItem: TcxCustomGridTableItem;
begin
  if DataController <> nil then
  begin
    AItem := TcxCustomGridTableItem(DataController.GetItemByFieldName(AFieldName));
    Result := AItem.Index;
  end
  else
    Result := -1;
end;

function TcxBoldExtLookupComboBoxProperties.GetDBLookupGridDataController: TcxDBDataController;
begin
  Result := TcxDBDataController(GetLookupGridDataController);
end;
}
function TcxBoldExtLookupComboBoxProperties.GetGrid: TcxBoldExtLookupGrid;

  procedure CreateGrid;
  begin
    FGrid := TcxBoldExtLookupGrid.Create(nil);
    FGrid.IsPopupControl := True;
    FGrid.BorderStyle := cxcbsNone;
    FGrid.Levels.Add;
  end;

begin
  if (FGrid = nil) and not FDestroying then
    CreateGrid;
  Result := FGrid;
end;

{function TcxBoldExtLookupComboBoxProperties.GetGridMode: Boolean;
begin
  Result := inherited IsUseLookupList;
end;}

function TcxBoldExtLookupComboBoxProperties.GetListFieldIndex: Integer;
var
  AItem: TcxCustomGridTableItem;
begin
  if IsDefinedByLookup then
    Result := GetDisplayColumnIndex
  else
  begin
    AItem := ListFieldItem;
    if AItem <> nil then
      Result := AItem.Index
    else
      Result := -1;
  end;  
end;

function TcxBoldExtLookupComboBoxProperties.GetListFieldItem: TcxCustomGridTableItem;
begin
  if IsDefinedByLookup then
    Result := nil
  else
    Result := FListFieldItem;
end;

{procedure TcxBoldExtLookupComboBoxProperties.SetGridMode(Value: Boolean);
begin
  inherited IsUseLookupList := Value;
end;}

procedure TcxBoldExtLookupComboBoxProperties.SetListFieldItem(Value: TcxCustomGridTableItem);
begin
  if (View <> nil) and (View.IndexOfItem(Value) = -1) then
    Value := nil;
  if FListFieldItem <> Value then
  begin
    if FListFieldItem <> nil then
      FreeNotificator.RemoveSender(FListFieldItem);
    FListFieldItem := Value;
    if FListFieldItem <> nil then
      FreeNotificator.AddSender(FListFieldItem);
    if not FInCheckListFieldItem then
      Changed;
  end;
end;

procedure TcxBoldExtLookupComboBoxProperties.SetView(Value: TcxCustomGridTableView);
begin
  if (Value <> nil) and not IsViewSupported(Value) then Exit;
  if FView <> Value then
  begin
    if FView <> nil then
      UnlinkView(FView);
    FView := Value;
    if FView <> nil then
      LinkView(FView);
    Changed;
  end;
end;

function TcxBoldExtLookupComboBoxProperties.GetBoldLookupGridDataController: TcxBoldDataController;
begin
  Result := TcxBoldDataController(GetLookupGridDataController);
end;

procedure TcxBoldExtLookupComboBoxProperties.SetStoredValue(
  aValue: Variant; aBoldHandle: TBoldElementHandle; aEdit: TcxCustomEdit; aFollower: TBoldFollower; var aDone: boolean);
var
  lItemIndex: Integer;
  lSelectedElement: TBoldElement;
begin
//  inherited;
//  Assert(aEdit is TcxCustomComboBox, 'TcxBoldLookupComboBoxProperties.SetStoredValue: aEdit is not TcxCustomComboBox;' + aEdit.classname);

//  lItemIndex := (aEdit as TcxCustomComboBox).ItemIndex;
//  if TcxCustomComboBox(aEdit).ILookupData.CurrentKey <> null then
  if (not VarIsNull(aValue)) and (aValue <> -1) then
  begin
    lItemIndex := aValue; //TcxCustomComboBox(aEdit).ILookupData.CurrentKey;

    lSelectedElement := GetBoldLookupGridDataController.BoldHandle.List[lItemIndex];
    InternalComboSetValue(aBoldHandle, aFollower, lSelectedElement, BoldSelectChangeAction, BoldSetValueExpression, DataController.BoldHandle, aValue);
{
  Assert(aEdit.EditingValue = aEdit.EditValue);
  i := aEdit.EditingValue;
  Assert(DataController.CurrentIndex = i);
//  (DataController.Follower.Element as TBoldList)[i].
  aFollower.Element.Assign(DataController.CurrentBoldObject);
}
  end;
  aDone := true;
end;

procedure TcxBoldExtLookupComboBoxProperties.BoldLookupGridBeginUpdate;
begin
  if View <> nil then View.BeginUpdate;
end;

procedure TcxBoldExtLookupComboBoxProperties.BoldLookupGridEndUpdate;
begin
  if View <> nil then View.EndUpdate;
end;

function TcxBoldExtLookupComboBoxProperties.CanEdit(
  aBoldHandle: TBoldElementHandle; aFollower: TBoldFollower): boolean;
begin
  result := GetBoldLookupGridDataController.RecordCount > 0;
end;

{ TcxCustomBoldExtLookupComboBox }

class function TcxCustomBoldExtLookupComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxBoldExtLookupComboBoxProperties;
end;

function TcxCustomBoldExtLookupComboBox.CanDropDown: Boolean;
begin
  if ActiveProperties.FocusPopup then
    Result := True
  else
   Result := inherited CanDropDown;
end;

function TcxCustomBoldExtLookupComboBox.GetActiveProperties: TcxBoldExtLookupComboBoxProperties;
begin
  Result := TcxBoldExtLookupComboBoxProperties(InternalGetActiveProperties);
end;

function TcxCustomBoldExtLookupComboBox.GetProperties: TcxBoldExtLookupComboBoxProperties;
begin
  Result := TcxBoldExtLookupComboBoxProperties(FProperties);
end;

procedure TcxCustomBoldExtLookupComboBox.SetProperties(Value: TcxBoldExtLookupComboBoxProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxBoldExtLookupComboBox }

class function TcxBoldExtLookupComboBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxBoldLookupEditDataBinding;
end;

function TcxBoldExtLookupComboBox.GetDataBinding: TcxBoldTextEditDataBinding;
begin
  Result := TcxBoldTextEditDataBinding(FDataBinding);
end;

procedure TcxBoldExtLookupComboBox.SetDataBinding(Value: TcxBoldTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;
{
procedure TcxBoldExtLookupComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(GetcxDBEditDataLink(Self));
end;
}

type
  TcxBoldTextEditDataBindingAccess = class(TcxBoldTextEditDataBinding);

procedure TcxBoldExtLookupComboBox.Paint;
begin
  inherited Paint;
  if TcxBoldTextEditDataBindingAccess(DataBinding).ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

procedure TcxBoldExtLookupComboBox.Initialize;
begin
  inherited;
  if IsDesigning and not IsLoading then
  begin
    _ValidateEdit(self);
  end;
end;

{ TcxEditRepositoryExtLookupComboBoxItem }

class function TcxEditRepositoryExtLookupComboBoxItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxBoldExtLookupComboBoxProperties;
end;

function TcxEditRepositoryExtLookupComboBoxItem.GetProperties: TcxBoldExtLookupComboBoxProperties;
begin
  Result := inherited Properties as TcxBoldExtLookupComboBoxProperties;
end;

procedure TcxEditRepositoryExtLookupComboBoxItem.SetProperties(Value: TcxBoldExtLookupComboBoxProperties);
begin
  inherited Properties := Value;
end;

// TODO: rename TcxEditRepositoryExtLookupComboBoxItem to something unique
{
initialization
  RegisterClasses([TcxEditRepositoryExtLookupComboBoxItem]);
  GetRegisteredEditProperties.Register(TcxBoldExtLookupComboBoxProperties,
    cxSEditRepositoryExtLookupComboBoxItem);

finalization
  GetRegisteredEditProperties.Unregister(TcxBoldExtLookupComboBoxProperties);
}
end.
