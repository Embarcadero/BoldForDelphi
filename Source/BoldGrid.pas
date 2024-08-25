
{ Global compiler directives }
{$include bold.inc}
unit BoldGrid;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  {$IFDEF DELPHI6_OR_LATER}
  Types,
  {$ELSE}
  Windows,
  {$ENDIF}
  Messages,
  Graphics,
  Controls,
  Grids,
  Menus,
  StdCtrls,
  Classes,
  BoldEnvironmentVCL,
  {$IFNDEF BOLDCOMCLIENT}
  BoldSystem,
  {$ENDIF}
  BoldCommonBitmaps,
  BoldControlPackDefs,
  BoldControlsDefs,
  BoldElements,
  BoldAbstractListHandle,
  BoldListHandleFollower,
  BoldControlPack,
  BoldControllerListControlPack,
  BoldListListControlPack,
  BoldComponentvalidator,
  BoldSubscription,
  BoldStringControlPack;

type
  TBoldFirstColumnRenderer = class;
  TBoldCustomGrid = class;
  TBoldGrid = class;
  TBoldGridColumns = class;
  TBoldGridColumn = class;
  TBoldColumnTitle = class;
  TBoldColumnClass = class of TBoldGridColumn;
  TBoldInplaceEdit = class;

  {$IFNDEF BOLDCOMCLIENT}
  TBoldLookupChange = procedure(DestElement: TBoldElement; NewValue: TBoldElement) of object;
  {$ENDIF}

  { TBoldFirstColumnRenderer }
  TBoldFirstColumnRenderer = class(TBoldAsStringRenderer)
  public
    procedure DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); override;
  end;

  {$IFNDEF BOLDCOMCLIENT}
  {  TBoldGridCheckBoxPainterRenderer  }
  TBoldGridCheckBoxPainterRenderer = class(TBoldAsStringRenderer)
  private
    fColumn: TBoldGridColumn;
    procedure GetCurrentElement(Follower: TBoldFollower; ie: TBoldIndirectElement);
    function GetCurrentCheckBoxState(Follower: TBoldFollower): TCheckBoxState;
    function GetCheckBoxRect(rect: TRect; Alignment: TAlignment): TRect;
    procedure ToggleValue(GridCoord: TGridCoord; Grid: TBoldCustomGrid);
  public
    procedure DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); override;
    procedure CheckBoxClick(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer; Grid: TBoldCustomGrid; Alignment: TAlignment);
    procedure KeyPress(var Key: Word; Shift: TShiftState; Grid: TBoldCustomGrid);
  end;
  {$ENDIF}

  TBoldConstraintRenderer = class(TBoldAsStringRenderer)
    {Override draw in Controllers since we need access to follower}
    procedure DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); override;
  end;


  { TBoldGridColumns }
  TBoldGridColumns = class(TCollection)
  private
    fGrid: TBoldCustomGrid;
    function GetColumn(index: Integer): TBoldGridColumn;
    procedure MoveColumn(FromIndex, ToIndex: Longint);
    procedure SetColumn(index: Integer; Value: TBoldGridColumn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(theGrid: TBoldCustomGrid; ColumnClass: TBoldColumnClass);
    function Add: TBoldGridColumn;
    procedure Update(Item: TCollectionItem); override;
    property Grid: TBoldCustomGrid read fGrid;
    property Items[index: Integer]: TBoldGridColumn read GetColumn write SetColumn; default;
  end;

  { TBoldColumnTitle }
  TBoldColumnTitle = class(TPersistent)
  private
    fAlignment: TAlignment;
    fCaption: TCaption;
    FColor: TColor;
    fColumn: TBoldGridColumn;
    fFont: TFont;
    fPopupMenu: TPopupMenu;
    procedure _FontChanged(Sender: TObject);
    function GetAlignment: TAlignment;
    function GetCaption: string;
    function GetColor: TColor;
    function GetFont: TFont;
    function IsAlignmentStored: Boolean;
    function IsCaptionStored: Boolean;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaption(const Value: string); virtual;
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetPopupMenu(Value: TPopupMenu);
  protected
    procedure RefreshDefaultFont;
    procedure Changed;
  public
    constructor Create(Column: TBoldGridColumn);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultAlignment: TAlignment;
    function DefaultCaption: string;
    function DefaultColor: TColor;
    function DefaultFont: TFont;
    procedure RestoreDefaults; virtual;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
  end;

  { TBoldGridColumn }
  TBoldGridColumn = class(TCollectionItem)
  private
    {$IFNDEF BOLDCOMCLIENT}
    fCheckBoxPainterRenderer: TBoldGridCheckBoxPainterRenderer;
    fAllowCheckBox: Boolean;
    fLookupHandle: TBoldAbstractListHandle;
    fNilElementMode: TBoldNilElementMode;
    fLookUpProperties: TBoldStringFollowerController;
    fOnLookupChange: TBoldLookupChange;
    fLookupHandleSubscriber: TBoldPassThroughSubscriber;
    {$ENDIF}
    fAlignment: TAlignment;
    fAssignedValues: TBoldColumnValues;
    fBoldProperties: TBoldStringFollowerController;
    fColor: TColor;
    fCWAdjust: TBoldCWAdjustSet;
    fDefaultPopupMenu: Boolean;
    fFont: TFont;
    fGrid: TBoldCustomGrid;
    fReadOnly: Boolean;
    fTitle: TBoldColumnTitle;
    fUserDraw: Boolean;
    {$IFNDEF BOLDCOMCLIENT}
    procedure SetLookupHandle(const Value: TBoldAbstractListHandle);
    procedure SetLookUpProperties(const Value: TBoldStringFollowerController);
    function GetLookupContext: TBoldElementTypeInfo;
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    {$ENDIF}
    procedure _FontChanged(Sender: TObject);
    function GetWidth: Integer;
    procedure SetBoldProperties(Value: TBoldStringFollowerController);
    procedure SetColor(C: TColor);
    procedure SetCWAdjust(V: TBoldCWAdjustSet);
    procedure SetFont(Value: TFont);
    procedure SetTitle(V: TBoldColumnTitle);
    procedure SetWidth(Value: Integer);
    function GetFont: TFont;
    function AreFontsEqual(Font1, Font2: TFont): Boolean;
  protected
    function GetDisplayName: string; override;
    procedure RefreshDefaultFont;
    procedure SetIndex(Value: Integer); override;
    {$IFNDEF BOLDCOMCLIENT}
    procedure SetCheckBoxRendererIfAppropriate;
    function ColumnHasCheckBoxOverrides: Boolean; virtual;
    function GetCurrentCheckBoxState(Follower: TBoldFollower): TCheckBoxState; virtual;
    procedure SetCurrentCheckBoxState(Follower: TBoldFollower; NewValue: TCheckBoxState); virtual;
    {$ENDIF}
  public
    constructor Create(theCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultFont: TFont;
    procedure RestoreDefaults; virtual;
    property AssignedValues: TBoldColumnValues read fAssignedValues;
    property Grid: TBoldCustomGrid read fGrid;
  published
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property BoldProperties: TBoldStringFollowerController read fBoldProperties write SetBoldProperties;
    property Color: TColor read FColor write SetColor default clWindow;
    property ColReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property CWAdjust: TBoldCWAdjustSet read fCWAdjust write SetCWAdjust default[];
    property DefaultTitlePopupMenu: Boolean read fDefaultPopupMenu write fDefaultPopupMenu default True;
    property Font: TFont read GetFont write SetFont;
    property Title: TBoldColumnTitle read FTitle write SetTitle;
    property UserDraw: Boolean read fUserDraw write fUserDraw default False;
    property Width: Integer read GetWidth write SetWidth stored False;
    {$IFNDEF BOLDCOMCLIENT}
    property AllowCheckBox: Boolean read fAllowCheckBox write fAllowCheckBox default false;
    property LookUpProperties: TBoldStringFollowerController read fLookUpProperties write SetLookUpProperties;
    property NilElementMode: TBoldNilElementMode read fNilElementMode write fNilElementMode default neNone;
    property LookupHandle: TBoldAbstractListHandle read fLookupHandle write SetLookupHandle;
    property OnLookupChange: TBoldLookupChange read fOnLookupChange write fOnLookUpChange;
    {$ENDIF}
  end;

  { TBoldCustomGrid }
  TBoldCustomGrid = class(TCustomGrid, IBoldValidateableComponent)
  private
    {Bold stuff}
    fFirstColumnRenderer: TBoldFirstColumnRenderer;
    fAddNewAtEnd: Boolean;
    fAlwaysShowSelected: Boolean;
    fAnchor: Integer;
    fBoldColumnsProperties: TBoldControllerList;
    fBoldProperties: TBoldListAsFollowerListController;
    FColumns: TBoldGridColumns;
    fCurrentListElementType: TBoldElementTypeInfo;
    fEnableColAdjust: boolean;
    fHandleFollower: TBoldListHandleFollower;
    fBoldAutoColumns: Boolean;
    fOndrawCell: TBoldDrawCellEvent;
    fOnTopLeftChanged: TNotifyEvent;
    fRangeSelect: Boolean;
    fTitleFont: TFont;
    fLastMouseDownScreenCoord: TPoint;
    fLastMouseDownGridCoord: TGridCoord;
    fBoldShowConstraints: Boolean;
    fInvalidateFrom: integer;
    fMakingListUpToDate: Boolean;
    fHasGhostRow: Boolean;
    FOnSelectCell: TSelectCellEvent;
    fIsEnsuringFixedCol: Boolean;
    {$IFNDEF BOLDCOMCLIENT}
    fLookUpEditorActive: Boolean;
    {$ENDIF}
    fFixedColumn: TBoldGridColumn;
    fAutoSelectNewRows: Boolean;
    fSubFollowerCountBeforeMakeUpToDate: integer;
    fLastInsertedRowIndex: integer;
    fEditedElementBeforeMakeUpToDate: TBoldElement;
    fBoldDragAnywhere: Boolean;
    fIsDragging: Boolean;
    fIsMultiSelecting: Boolean;
    fLastMouseDownShiftState: TShiftState;
    fPostDisplayEventSet: boolean;
    procedure EnsureOneFixedCol;
    function GetBoldHandle: TBoldAbstractListHandle;
    function GetBoldList: TBoldList;
    function GetCurrentBoldElement: TBoldElement;
    function GetFollower: TBoldFOllower;
    function GetOptions: TGridOptions;
    function GetSelected(DataRow: integer): Boolean;
    procedure SetBoldHandle(value: TBoldAbstractListHandle);
    procedure SetColumns(Value: TBoldGridColumns);
    procedure SetController(Value: TBoldListAsFollowerListController);
    procedure SetOptions(val: TGridOptions);
    procedure SetSelection(aRow: Integer; Shift: TShiftState; ForceClearOfOtherRows: Boolean; IgnoreToggles: Boolean);
    procedure TypeMayHaveChanged;
    function CellFont(Column: TBoldGridColumn): TFont;
    function GetString(GridCol, DataRow: Integer): string;
    function HighlightCell(AState: TGridDrawState; aRow: integer): Boolean;
    procedure _AfterMakeCellUptoDate(Follower: TBoldFollower);
    procedure _DeleteRow(index: Integer; owningFollower: TBoldFollower);
    procedure _InsertRow(index: Integer; OwningFollower: TBoldFollower);
    procedure _ReplaceRow(index: Integer; AFollower: TBoldFollower);
    procedure AdjustCol(Col: Integer);
    function DefaultTitlePopup(Col: Integer): TPopupMenu;
    procedure DefaultTitlePopupOnClick(Sender: TObject);
    function GetCellFollower(ListCol, DataRow: Integer): TBoldFollower;
    function GetCurrentCellFollower: TBoldFollower;
    function GetMultiSelect: Boolean;
    function GetRowFollower(DataRow: Integer): TBoldFollower;
    procedure InvalidateFromRow(DisplayDataRow: Longint);
    procedure SetCurrentRow(DataRow: Integer);
    procedure SetMultiSelect(V: Boolean);
    procedure TitleMenuPopup(GridCoord: TGridCoord; X, Y: Integer);
    procedure SetTitleFont(const Value: TFont);
    function GetBoldHandleIndexLock: Boolean;
    procedure SetBoldHandleIndexLock(Value: Boolean);
    {$IFNDEF BOLDCOMCLIENT}
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean;
    function ColumnIsCheckBox(col: integer): Boolean;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    {$ENDIF}
    function GetMutableList: TBoldList;
    function GetShowTitleRow: Boolean;
    procedure SetShowTitleRow(const Value: Boolean);
    function GetTitleRow: integer;
    procedure GetActiveRange(var FirstActive, LastActive: integer);
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect;
    procedure EnsureRowActive(DataRow: integer);
    procedure DisplayAvailableFollowers;
    function GetCellText(col, row: integer): string;
    procedure _FontChanged(Sender: TObject);
    procedure PostDisplayEvent(Sender: TObject);
    procedure SetPostDisplayEvent;
  protected
    { Protected declarations }
    procedure _AfterMakeListUptoDate(Follower: TBoldFollower); virtual;
    procedure _BeforeMakeListUpToDate(Follower: TBoldFollower); virtual;
    procedure AutoAdjustCol(Col: Integer);
    function CanEditAcceptKey(KEY: Char): Boolean; override;
    function CanEditModify: Boolean; override;
    function CanEditShow: Boolean; override;
    function CanEditShowForCustomEditors: Boolean;
    procedure Click; override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure ColWidthsChanged; override;
    function CreateColumns: TBoldGridColumns; dynamic;
    procedure CreateDefaultColumns; virtual;
    function DataRow(GridRow: Integer): Integer;
    function GridRow(Datarow: Integer): Integer;
    procedure DblClick; override;
    procedure DefaultColumns;
    procedure DeleteAllColumns;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure KeyDown(var KEY: Word; Shift: TShiftState); override;
    procedure KeyPress(var KEY: Char); override;
    procedure KeyUp(var KEY: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure TopLeftChanged; override;
    procedure Resize; override;
    procedure EnsureConstraintColumn;
    procedure EditStop;
    function GetEditLimit: Integer; override;
    function ColumnClass: TBoldColumnClass; virtual;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    function GetEditor: TBoldInplaceEdit;
    procedure EnsureActiveCellFollowerExpressions;
    {$IFNDEF BOLDCOMCLIENT}
    procedure LookUpChange(sender: Tobject; DestElement: TBoldElement; EditColumn: TBoldGridColumn);
    {$ENDIF}
    property BoldDragAnywhere: Boolean read fBoldDragAnywhere write fBoldDragAnywhere default False;
    property AddNewAtEnd: Boolean read fAddNewAtEnd write fAddNewAtEnd;
    property AlwaysShowSelected: Boolean read fAlwaysShowSelected write fAlwaysShowSelected default True;
    property BoldHandleIndexLock: Boolean read GetBoldHandleIndexLock write SetBoldHandleIndexLock default true;
    property BoldAutoColumns: Boolean read fBoldAutoColumns write fBoldAutoColumns;
    property BoldShowConstraints: Boolean read fBoldShowConstraints write fBoldShowConstraints;
    property BoldHandle: TBoldAbstractListHandle read GetBoldHandle write SetBoldHandle;
    property BoldList: TBoldList read GetBoldList;
    property BoldProperties: TBoldListAsFollowerListController read fBoldProperties write SetController;
    property CellFollowers[GridCol, DataRow: Integer]: TBoldFollower read GetCellFollower;
    property Columns: TBoldGridColumns read FColumns write SetColumns;
    property CurrentBoldElement: TBoldElement read GetCurrentBoldElement;
    property CurrentCellFollower: TBoldFollower read GetCurrentCellFollower;
    property EnableColAdjust: boolean read fEnableColAdjust write fEnableColAdjust;
    property Follower: TBoldFollower read GetFollower;
    property OnDrawCell: TBoldDrawCellEvent read fOndrawCell write fOndrawCell;
    property OnTopLeftChanged: TNotifyEvent read fOnTopLeftChanged write fOnTopLeftChanged;
    property Options: TGridOptions read GetOptions write SetOptions default [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goColMoving, goColSizing];
    property RowFollowers[DataRow: Integer]: TBoldFollower read GetRowFollower;
    property Selected[DataRow: integer]:Boolean read GetSelected;
    property TitleFont: TFont read fTitleFont write SetTitleFont;
    property HasGhostRow: Boolean read fHasGhostRow;
    property ShowTitleRow: Boolean read GetShowTitleRow write SetShowTitleRow default True;
    property TitleRow: integer read GetTitleRow;
    property OnSelectCell: TSelectCellEvent read FOnSelectCell write FOnSelectCell;
    {$IFNDEF BOLDCOMCLIENT}
    property LookUpEditorActive: Boolean read fLookUpEditorActive write fLookUpEditorActive;
    {$ENDIF}
    property AutoSelectNewRows: Boolean read fAutoSelectNewRows write fAutoSelectNewRows default false;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddColumn: TBoldGridColumn;
    procedure AdjustActiveRange;
    function CreateEditor: TInplaceEdit; override;
    procedure DeleteColumn(ACol: Integer); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure DrawCell(ACol, aRow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure EditColumns;
    function GetHandleStaticType: TBoldElementTypeInfo;
    function GetHandleListElementType: TBoldElementTypeInfo;
    function GetEditText(GridCol, GridRow: Longint): string; override;
    procedure MoveColumn(FromIndex, ToIndex: Longint);
    procedure SetEditText(GridCol, GridRow: Longint; const Value: string); override;
    procedure ReallyInvalidateCol(Column: integer);
    procedure DisplayAllCells;
    function AsClipBoardText: String;
    procedure ActivateAllCells;
    property ColCount;
    property CellText[col, row: integer]: string read GetCellText;
    property MutableList: TBoldList read GetMutableList;
  end;

  { TBoldGrid }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldGrid = class(TBoldCustomGrid)
  public
    {$IFNDEF T2H}
    {Properties from TBoldCustomGrid}
    property BoldList;
    property CellFollowers;
    property CurrentBoldElement;
    property Selected;
    {Properties from TCustomGrid}
    property Col;
    property ColWidths;
    property LeftCol;
    property Row;
    property RowCount;
    property RowHeights;
    property TopRow;
    property VisibleColCount;
    property VisibleRowCount;
  published
    property AddNewAtEnd;
    property Align;
    property AlwaysShowSelected;
    property Anchors;
    property BoldHandleIndexLock;
    property BoldAutoColumns;
    property BoldShowConstraints;
    property BoldHandle;
    property BoldProperties;
    property BoldDragAnywhere;
    property Color;
    property Constraints;
    property Columns;
    {$IFNDEF BCB}
    property Ctl3d;
    {$ENDIF}
    property DefaultColWidth;
    property DefaultRowHeight;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EnableColAdjust;
    property Enabled;
    property FixedColor;
    property FixedCols;
    property Font;
    property GridLineWidth;
    property Options;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property ShowTitleRow;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDrawCell;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
    property OnSelectCell;
    {$ENDIF}
  end;

  { TBoldInplaceEdit }
  TBoldInplaceEdit = class(TInplaceEdit)
  private
    {$IFNDEF BOLDCOMCLIENT}
    fCombo: TCombobox;
    fInitialComboIndex: integer;
    fDestElement: TBoldElement;
    fDestElementSubscriber: TBoldPassThroughSubscriber;
    fComboAborting: Boolean;
    {$ENDIF}
    fEditColumn: TBoldGridColumn;
    function GetGrid: TBoldCustomGrid;
    {$IFNDEF BOLDCOMCLIENT}
    procedure HideCombo;
    function GetCombo: TComboBox;
    function GetDestElement(CellFollower: TBoldFollower; Column: TBoldGridColumn): TBoldElement;
    procedure ComboChange(sender: TObject);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    {$ENDIF}
  protected
    procedure KeyPress(var Key: Char); override;
    procedure BoundsChanged; override;
    {$IFNDEF BOLDCOMCLIENT}
    procedure ChangedPos(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure InitCombo(var Message: TWMWindowPosChanged);
    procedure ComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Grid: TBoldCustomGrid read GetGrid;
    {$IFNDEF BOLDCOMCLIENT}
    property Combo: TCombobox read GetCombo;
    {$ENDIF}
  end;


{$IFNDEF BOLDCOMCLIENT}
var
  BoldAllowCheckBoxInGrids: Boolean = false;
{$ENDIF}

implementation

uses
  SysUtils,
  Forms,
  System.Types,
  System.UITypes,
  {$IFNDEF BOLDCOMCLIENT}
  BoldAttributes,
  BoldSystemRT,
  BoldAFP,
  BoldGUI,
  {$ENDIF}
  BoldEnvironment,
  BoldDefs,
  BoldListControlPack,
  BoldUtils,
  TypInfo,
  BoldMath,
  BoldQueue;

const
  ColumnTitleValues = [cvTitleColor..cvTitleFont];
  breDestElementDestroyed = 100;
  breColumnLookUpHandleDestroyed = 101;

var
  TheDefaultTitlePopup: TPopupMenu;

{ TBoldInplaceEdit }

{$IFNDEF BOLDCOMCLIENT}

type
  TBoldInplaceCombo = class(TCombobox)
  private
    fInplaceEdit: TBoldInplaceEdit;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor CreateWithInplaceEditor(InplaceEdit: TBoldInplaceEdit);
  end;

{ TBoldInplaceCombo }

constructor TBoldInplaceCombo.CreateWithInplaceEditor(InplaceEdit: TBoldInplaceEdit);
begin
  inherited Create(InplaceEdit);
  fInplaceEdit := InplaceEdit;
end;

procedure TBoldInplaceCombo.WndProc(var Message: TMessage);
var
  CallInherited: Boolean;
begin
  CallInherited := True;
  if not (csDesigning in ComponentState) then
  begin
    case Message.Msg of
      WM_COMMAND: if TWMCommand(Message).NotifyCode = CBN_SELCHANGE then
        if fInplaceEdit.fEditColumn.BoldProperties.ApplyPolicy = bapChange then
          Click;
    end;
  end;
  if CallInherited then
    inherited;
end;


procedure TBoldInplaceEdit.ChangedPos(var Message: TWMWindowPosChanged);
begin
  if not IsWindowVisible(Handle) then
  begin
    if fComboAborting then
      Grid.CurrentCellFollower.MarkValueOutOfDate
    else
      Grid.EditStop;

    Combo.Visible := False;
    Grid.LookUpEditorActive := False;
    if Grid.CanFocus then
      Grid.SetFocus;
  end
  else
  begin
    InitCombo(Message);
  end;
end;
{$ENDIF}

procedure TBoldInplaceEdit.BoundsChanged;
var
  R: TRect;
begin
  Assert(Assigned(Grid));
  R := Rect(2, 2, TBoldCustomGrid(Grid).Columns[TBoldCustomGrid(Grid).Col].Width - 2, Height);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TBoldInplaceEdit.KeyPress(var Key: Char);
var
  Grid: TBoldCustomGrid;
begin
  inherited KeyPress(Key);
  Grid := TBoldCustomGrid(Owner);
  Grid.SetSelection(grid.DataRow(grid.Row), [], true, false);
  if CharInSet(Key, [#32..#255]) and
    not Grid.Columns[Grid.Col].BoldProperties.ValidateCharacter(Key, Grid.CurrentCellFollower) then
  begin
    MessageBeep(0);
    Key := BOLDNULL;
  end;

  if Key = BOLDESC then
  begin
    Grid.CurrentCellFollower.DiscardChange;
    SelectAll;
    Key := BOLDNULL;
  end;
end;

{ TBoldInplaceEdit }

constructor TBoldInplaceEdit.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF BOLDCOMCLIENT}
  fDestElementSubscriber := TBoldPassthroughSubscriber.Create(_receive);
  {$ENDIF}
end;

destructor TBoldInplaceEdit.Destroy;
begin
  {$IFNDEF BOLDCOMCLIENT}
  FreeAndNil(fCombo);
  FreeAndNil(fDestElementSubscriber);
  fDestElement := nil;
  {$ENDIF}
  fEditColumn := nil;
  inherited;
end;

function TBoldInplaceEdit.GetGrid: TBoldCustomGrid;
begin
  result := (inherited Grid) as TBoldCustomGrid;
end;

type
  TExposedBoldAsStringRenderer = class(TBoldAsStringRenderer);
  TExposedFollowerController = class(TBoldFollowerController);

{ TBoldColumnTitle }
constructor TBoldColumnTitle.Create(Column: TBoldGridColumn);
begin
  inherited Create;
  fColumn := Column;
  fFont := TFont.Create;
  fFont.Assign(DefaultFont);
  fFont.OnChange := _FontChanged;
end;

destructor TBoldColumnTitle.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TBoldColumnTitle.Assign(Source: TPersistent);
var
  S: TBoldColumnTitle;
  AssignedValues: TBoldColumnValues;
begin
  if Source is TBoldColumnTitle then
  begin
    S := Source as TBoldColumnTitle;
    AssignedValues := S.fColumn.fAssignedValues;
    if cvTitleAlignment in AssignedValues then
      Alignment := S.Alignment;
    if cvTitleColor in AssignedValues then
      Color := S.Color;
    if cvTitleCaption in AssignedValues then
      Caption := S.Caption;
    if cvTitleFont in AssignedValues then
      Font := S.Font;
  end
  else
    inherited Assign(Source);
end;

function TBoldGridColumn.AreFontsEqual(Font1, Font2: TFont): Boolean;
begin
  Result := (Font1.Charset = Font2.Charset) and
            (Font1.Color = Font2.Color) and
            (Font1.Height = Font2.Height) and
            (Font1.Name = Font2.Name) and
            (Font1.Pitch = Font2.Pitch) and
            (Font1.Size = Font2.Size) and
            (Font1.Style = Font2.Style);
end;

procedure TBoldColumnTitle.Changed;
begin
  fColumn.fGrid.InvalidateCell(fColumn.index, 0);
end;

function TBoldColumnTitle.DefaultAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TBoldColumnTitle.DefaultColor: TColor;
var
  Grid: TBoldCustomGrid;
begin
  Grid := fColumn.fGrid;
  if Assigned(Grid) then
    Result := Grid.FixedColor
  else
    Result := clBtnFace;
end;

function TBoldColumnTitle.DefaultFont: TFont;
var
  Grid: TBoldCustomGrid;
begin
  Grid := fColumn.fGrid;
  if Assigned(Grid) then
    Result := Grid.TitleFont
  else
    Result := fColumn.Font;
end;

function TBoldColumnTitle.DefaultCaption: string;
begin
  if Assigned(fColumn.BoldProperties) then
    Result := fColumn.BoldProperties.Expression
  else
    Result := '';
end;

procedure TBoldColumnTitle._FontChanged(Sender: TObject);
begin
  if fColumn.AreFontsEqual(TFont(Sender), DefaultFont) then
    Exclude(fColumn.fAssignedValues, cvTitleFont)
  else
    Include(fColumn.fAssignedValues, cvTitleFont);
  Changed;
end;

function TBoldColumnTitle.GetAlignment: TAlignment;
begin
  if cvTitleAlignment in fColumn.fAssignedValues then
    Result := FAlignment
  else
    Result := DefaultAlignment;
end;

function TBoldColumnTitle.GetColor: TColor;
begin
  if cvTitleColor in fColumn.fAssignedValues then
    Result := FColor
  else
    Result := DefaultColor;
end;

function TBoldColumnTitle.GetCaption: string;
begin
  if cvTitleCaption in fColumn.fAssignedValues then
    Result := fCaption
  else
    Result := DefaultCaption;
end;

function TBoldColumnTitle.GetFont: TFont;
var
  SavedOnChange: TNotifyEvent;
begin
  if not (cvTitleFont in fColumn.fAssignedValues) then
  begin
    if not fColumn.AreFontsEqual(DefaultFont, fFont) then
    begin
      SavedOnChange := FFont.OnChange;
      FFont.OnChange := nil;
      FFont.Assign(DefaultFont);
      FFont.OnChange := SavedOnChange;
    end;
  end;
  Result := FFont;
end;

function TBoldColumnTitle.IsAlignmentStored: Boolean;
begin
  Result := (cvTitleAlignment in fColumn.fAssignedValues) and
            (FAlignment <> DefaultAlignment);
end;

function TBoldColumnTitle.IsColorStored: Boolean;
begin
  Result := (cvTitleColor in fColumn.fAssignedValues) and
            (FColor <> DefaultColor);
end;

function TBoldColumnTitle.IsFontStored: Boolean;
begin
  Result := (cvTitleFont in fColumn.fAssignedValues);
end;

function TBoldColumnTitle.IsCaptionStored: Boolean;
begin
  Result := (cvTitleCaption in fColumn.fAssignedValues) and
            (fCaption <> DefaultCaption);
end;

procedure TBoldColumnTitle.RefreshDefaultFont;
var
  SavedOnChange: TNotifyEvent;
begin
  if (cvTitleFont in fColumn.fAssignedValues) then
    Exit;
  SavedOnChange := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Assign(DefaultFont);
  finally
    FFont.OnChange := SavedOnChange;
  end;
end;

procedure TBoldColumnTitle.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := cvTitleFont in fColumn.fAssignedValues;
  fColumn.fAssignedValues := fColumn.fAssignedValues - ColumnTitleValues;
  fCaption := '';
  RefreshDefaultFont;
  { If font was assigned, changing it back to default may affect grid title
    height, and title height changes require layout and redraw of the grid. }
  fColumn.Changed(FontAssigned);
end;

procedure TBoldColumnTitle.SetAlignment(Value: TAlignment);
begin
  if Value = DefaultAlignment then
    Exclude(fColumn.fAssignedValues, cvTitleAlignment)
  else
    Include(fColumn.fAssignedValues, cvTitleAlignment);
  FAlignment := Value;
  Changed;
end;

procedure TBoldColumnTitle.SetColor(Value: TColor);
begin
  if Value = DefaultColor then
    Exclude(fColumn.fAssignedValues, cvTitleColor)
  else
    Include(fColumn.fAssignedValues, cvTitleColor);
  FColor := Value;
  Changed;
end;

procedure TBoldColumnTitle.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TBoldColumnTitle.SetCaption(const Value: string);
begin
  if Value = DefaultCaption then
    Exclude(fColumn.fAssignedValues, cvTitleCaption)
  else
    Include(fColumn.fAssignedValues, cvTitleCaption);
  fCaption := Value;
  Changed;
end;

procedure TBoldColumnTitle.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Assigned(Value) then
    Value.FreeNotification(fColumn.fGrid);
end;

{ TBoldGridColumn }
constructor TBoldGridColumn.Create(theCollection: TCollection);
begin
  inherited Create(theCollection);
  if not (theCollection is TBoldGridColumns) then
    raise EBold.CreateFmt('%s.Create: Cannot create TBoldGridColumn outside a TBoldGridColumns', [ClassName]);
  fGrid := (theCollection as TBoldGridColumns).Grid;
  FBoldProperties := TBoldStringFollowerController.Create(fGrid);
  fColor := fGrid.Color;
  FBoldProperties.AfterMakeUptoDate := fGrid._AfterMakeCellUptoDate;
  fBoldProperties.OnGetContextType := fGrid.GetHandleStaticType;
  fGrid.fBoldColumnsProperties.Add(FBoldProperties);
  FTitle := TBoldColumnTitle.Create(self);
  FFont := TFont.Create;
  FFont.OnChange := _FontChanged;
  DefaultTitlePopupMenu := True;
  {$IFNDEF BOLDCOMCLIENT}
  fLookUpProperties := TBoldStringFollowerController.Create(grid);
  fLookUpProperties.OnGetContextType := GetLookupContext;
  fOnLookupChange := nil;
  fNilElementMode := neNone;
  fLookupHandleSubscriber := TBoldPassThroughSubscriber.Create(_Receive);
  {$ENDIF}
end;

destructor TBoldGridColumn.Destroy;
begin
  if fGrid.fFixedColumn = self then
    fGrid.fFixedColumn := nil;
  fGrid.fBoldColumnsProperties.Delete(index);
  BoldProperties := nil;
  FreeAndNil(FTitle);
  FreeAndNil(FFont);
  {$IFNDEF BOLDCOMCLIENT}
  FreeAndNil(fLookUpProperties);
  FreeAndNil(fLookupHandleSubscriber);
  {$ENDIF}
  inherited;
end;



procedure TBoldGridColumn.Assign(Source: TPersistent);
var
  SourceCol: TBoldGridColumn;
begin
  if Source is TBoldGridColumn then
  begin
    SourceCol := Source as TBoldGridColumn;
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      RestoreDefaults;
      BoldProperties := SourceCol.BoldProperties;
      if cvColor in SourceCol.AssignedValues then
        Color := SourceCol.Color;
      if cvWidth in SourceCol.AssignedValues then
        Width := SourceCol.Width;
      if cvFont in SourceCol.AssignedValues then
        Font := SourceCol.Font;
      if cvAlignment in SourceCol.AssignedValues then
        Alignment := SourceCol.Alignment;
      Title := SourceCol.Title;





    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TBoldGridColumn.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  Include(FAssignedValues, cvFont);
  Changed(False);
end;

procedure TBoldGridColumn.SetBoldProperties(Value: TBoldStringFollowerController);
begin
  if Assigned(Value) then
    fBoldProperties.Assign(Value);
end;

procedure TBoldGridColumn.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := cvFont in FAssignedValues;
  FTitle.RestoreDefaults;
  FAssignedValues := [];
  RefreshDefaultFont;

  Changed(FontAssigned);
end;

procedure TBoldGridColumn.RefreshDefaultFont;
var
  SavedOnChange: TNotifyEvent;
begin
  if cvFont in FAssignedValues then
    Exit;
  SavedOnChange := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Assign(DefaultFont);
  finally
    FFont.OnChange := SavedOnChange;
  end;
end;

function TBoldGridColumn.DefaultFont: TFont;
begin
  if Assigned(Grid) then
    Result := Grid.Font
  else
    Result := FFont;
end;

function TBoldGridColumn.GetDisplayName: string;
begin
  Result := FTitle.Caption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TBoldGridColumn.SetTitle(V: TBoldColumnTitle);
begin
  if V <> FTitle then
  begin
    FTitle.Assign(V);
    fGrid.InvalidateCell(index, 0);
  end;
end;

procedure TBoldGridColumn.SetColor(C: TColor);
begin
  if Color <> C then
  begin
    FColor := C;
    Changed(True);
  end;
end;

procedure TBoldGridColumn._FontChanged(Sender: TObject);
begin
  if AreFontsEqual(TFont(Sender), DefaultFont) then
  begin
    Exclude(fAssignedValues, cvTitleFont);
  end
  else
  begin
    Include(fAssignedValues, cvTitleFont);
  end;
  Font.Assign(TFont(Sender));
  fGrid.ReallyInvalidateCol(index);
end;

procedure TBoldGridColumn.SetWidth(Value: Integer);
begin
  fGrid.ColWidths[index] := Value;
end;

function TBoldGridColumn.GetWidth: Integer;
begin
  Result := fGrid.ColWidths[index];
end;

procedure TBoldGridColumn.SetCWAdjust(V: TBoldCWAdjustSet);
begin
  if V <> fCWAdjust then
  begin
    fCWAdjust := V;
    fGrid.AdjustCol(index);
  end;
end;

{ TBoldGridColumns }
function TBoldGridColumns.GetOwner: TPersistent;
begin
  Result := fGrid;
end;

constructor TBoldGridColumns.Create(theGrid: TBoldCustomGrid; ColumnClass: TBoldColumnClass);
begin
  inherited Create(ColumnClass);
  fGrid := theGrid;
end;

procedure TBoldGridColumns.MoveColumn(FromIndex, ToIndex: Longint);
begin
  inherited;
  Items[FromIndex].index := ToIndex;
end;

function TBoldGridColumns.Add: TBoldGridColumn;
begin
  BeginUpdate;
  Result := TBoldGridColumn(inherited Add);
  EndUpdate;
end;

function TBoldGridColumns.GetColumn(index: Integer): TBoldGridColumn;
begin
  Result := TBoldGridColumn(inherited Items[index]);
end;

procedure TBoldGridColumns.SetColumn(index: Integer; Value: TBoldGridColumn);
begin
  Items[index].Assign(Value);
end;

procedure TBoldGridColumns.Update(Item: TCollectionItem);
begin
  if Assigned(fGrid) then
  begin
    if (fGrid.ColCount <> Count) then
      fGrid.ColCount := MaxIntValue([1, Count]);
    if Assigned(Item) then
      fGrid.ReallyInvalidateCol(Item.index)
    else
      fGrid.Refresh;
  end;
end;

{ TBoldCustomGrid }
constructor TBoldCustomGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fFirstColumnRenderer := TBoldFirstColumnRenderer.Create(Self);
  fBoldColumnsProperties := TBoldControllerList.Create(Self);
  fBoldProperties := TBoldListAsFollowerListController.Create(self, fBoldColumnsProperties);
  fBoldProperties.OnAfterInsertItem := _InsertRow;
  fBoldProperties.OnAfterDeleteItem := _DeleteRow;
  fBoldProperties.OnReplaceitem := _ReplaceRow;
  fBoldProperties.AfterMakeUptoDate := _AfterMakeListUptoDate;
  fBoldProperties.BeforeMakeUptoDate := _BeforeMakeListUptoDate;
  fBoldProperties.OnGetContextType := GetHandleStaticType;
  fHandleFollower     := TBoldListHandleFollower.Create(Owner, fBoldProperties);
  FColumns            := CreateColumns;
  fAnchor             := 0;
  Options             := [goFixedVertLine, goFixedHorzLine, goVertLine,
                          goHorzLine, goRangeSelect, goEditing,
                          goColMoving, goColSizing];
  AlwaysShowSelected  := True;
  fTitleFont          := TFont.Create;
  fTitleFont.OnChange := _FontChanged;

  DefaultDrawing      := False;
  DefaultRowHeight    := 17;
  RowCount            := 2;
  fIsEnsuringFixedCol := false;
  fLastMouseDownScreenCoord := Point(-1, -1);
  fLastMouseDownGridCoord.x := -1;
  fLastMouseDownGridCoord.y := -1;

  if (csDesigning in ComponentState) and not (csReading in ComponentState) then
  begin
    AddColumn;
    AddColumn;
  end;
  EnsureOneFixedCol;
  fHasGhostRow := true;
end;

destructor TBoldCustomGrid.Destroy;
begin
  if EditorMode then
    EditStop;
  fFixedColumn := nil;
  RowCount := 0;
  FreeAndNil(fHandleFollower);
  FreeAndNil(FColumns);
  FreeAndNil(fBoldProperties);
  FreeAndNil(fBoldColumnsProperties);
  FreeAndNil(fTitleFont);
  FreeAndNil(fFirstColumnRenderer);
  inherited Destroy;
end;

procedure TBoldCustomGrid.TypeMayHaveChanged;
var
  NewListElementType: TBoldElementTypeInfo;
  {$IFNDEF BOLDCOMCLIENT}
  i: integer;
  {$ENDIF}
begin
  if BoldEffectiveEnvironment.RunningInIDE and (not Assigned(BoldHandle) or not Assigned(BoldHandle.List) or (BoldHandle.List.Count = 0)) then
    Exit;
  NewListElementType := GetHandleListElementType;

  if (NewListElementType <> fCurrentListElementType) then
  begin
    fCurrentListElementType := NewListElementType;
    if BoldAutoColumns then
      CreateDefaultColumns;
  end;
  {$IFNDEF BOLDCOMCLIENT}
  for i := 0 to Columns.Count - 1 do
    Columns[i].SetCheckBoxRendererIfAppropriate;
  {$ENDIF}
end;

procedure TBoldCustomGrid.Loaded;
begin
  inherited;

  EnsureOneFixedCol;

  if not (csDesigning in ComponentState) then
    TypeMayHaveChanged;

  EnsureConstraintColumn;
end;

procedure TBoldCustomGrid.DeleteAllColumns;
begin
  while Columns.Count > 1 do
    Columns[ColCount - 1].Free;
  if columns.count = 0 then
    AddColumn;
  EnsureOneFixedCol;
end;

procedure TBoldCustomGrid.CreateDefaultColumns;
{$IFNDEF BOLDCOMCLIENT}
var
  i: integer;
  ListElementType: TBoldElementTypeInfo;
  ClasstypeInfo: TBoldClassTypeInfo;
  UsedFirstCol: Boolean;
  column: TBoldGridColumn;

  function GetEmptyCol: TBoldGridColumn;
  begin
    if UsedFirstCol then
      result := AddColumn
    else
      result := Columns[ColCount - 1];
    UsedFirstCol := true;
  end;
{$ENDIF}

begin
  {$IFNDEF BOLDCOMCLIENT}
  ListElementType := GetHandleListElementType;
  DeleteAllColumns;
  UsedFirstCol := false;
  EnsureConstraintColumn;
  if Assigned(ListElementType) then
  begin
    if (ListElementType is TBoldClassTypeInfo) then
    begin
      ClassTypeInfo := ListElementType as TBoldClassTypeInfo;
      if ClassTypeInfo.DefaultStringRepresentation <> '' then
      begin
        Column := GetEmptyCol;
        Column.Boldproperties.Expression := '';
        Column.Title.Caption := ClassTypeInfo.ModelName;
      end;
      for i := 0 to ClassTypeInfo.AllMembers.Count - 1 do
      begin
        if ClassTypeInfo.AllMembers[i].IsAttribute then
        begin
          Column := GetEmptyCol;
          column.Boldproperties.Expression := ClassTypeInfo.AllMembers[i].ExpressionName;
          Column.Title.Caption := ClassTypeInfo.AllMembers[i].ModelName;
        end;
      end;
    end
    else if (ListElementType is TBoldAttributeTypeInfo) then
    begin
      GetEmptyCol.Title.Caption := TBoldAttributeTypeInfo(ListElementType).ModelName;
    end
    else if (ListElementType is TBoldListTypeInfo) then
    begin
      GetEmptyCol.Title.Caption := 'ClassName';
    end;
  end;
  if Columns.Count = 2 then
  begin
    AddColumn;
    Columns[Columns.Count - 1].Free;
  end;
  EnsureOneFixedCol;
  {$ENDIF}
  if Columns.count = 1 then
  begin
    AddColumn;
    with Columns[ColCount - 1] do
    begin
       Boldproperties.Expression := 'self.oclType';
       Title.Caption := 'Type';
    end;
    AddColumn;
    with Columns[ColCount - 1] do
    begin
       Boldproperties.Expression := '';
       Title.Caption := 'AsString';
    end;
  end;
  if BoldShowConstraints then
    Col := 2
  else
    Col := 1;
  {
  // special stuff
  AddColumn;
  with Columns[ColCount - 1] do
  begin
    BoldProperties.Expression := 'constraints->select(a|not a)->size > 0';
    Title.Caption := 'Violates constraint?';
  end;
  }
end;

procedure TBoldCustomGrid.EditColumns;
begin
(*  with TfrmRTColEditor.Create(nil) do
  try
    Execute(Self);
  finally
    Free;
  end;
  *)
end;

procedure TBoldCustomGrid.DefaultColumns;
begin
  CreateDefaultColumns;
end;

function TBoldCustomGrid.CreateColumns: TBoldGridColumns;
begin
  Result := TBoldGridColumns.Create(self, ColumnClass);
end;

procedure TBoldCustomGrid.SetColumns(Value: TBoldGridColumns);
begin
  Columns.Assign(Value);
end;

procedure TBoldCustomGrid.ColumnMoved(FromIndex, ToIndex: Longint);
var
  Col: Integer;
begin
  Columns.MoveColumn(FromIndex, ToIndex);
  inherited ColumnMoved(FromIndex, ToIndex);
  for Col := MinIntValue([FromIndex, ToIndex]) to MaxIntValue([FromIndex, ToIndex]) do
    Columns.Update(Columns[Col]);
end;

procedure TBoldCustomGrid.SetMultiSelect(V: Boolean);
begin
  if V then
    Options := Options + [goRangeSelect]
  else
    Options := Options - [goRangeSelect];
end;

function TBoldCustomGrid.GetMultiSelect: Boolean;
begin
  Result := goRangeSelect in Options;
end;

procedure TBoldCustomGrid.EnsureOneFixedCol;
  function FirstIsOK: Boolean;
  begin
    result := (Columns.Count > 0) and
      (not assigned(Columns[0].BoldProperties.Renderer) or
       (Columns[0].BoldProperties.Renderer = fFirstColumnRenderer)) and
      ((Columns[0].BoldProperties.Expression = '') or
       (Columns[0].BoldProperties.Expression = ''''''));
  end;

begin
  if FixedCols = 0 then
    fFixedColumn := nil;
  if not (csReading in componentstate) and
     not assigned(fFixedColumn) and
     not fIsEnsuringFixedCol then
  begin
    fIsEnsuringFixedCol := true;
    while Columns.Count < 2 do
      if not FirstIsOK then
        Columns.Insert(0)
      else
        Columns.Add;
    if not FirstIsOk then
      Columns.Insert(0);

    fFixedColumn := Columns[0];
    fFixedColumn.BoldProperties.Expression := '';
    fFixedColumn.Title.Caption := '';
    fFixedColumn.Color := Self.FixedColor;
    if not (csDesigning in componentstate) then
      fFixedColumn.BoldProperties.Renderer := fFirstColumnRenderer;
    FixedCols := 1;
    ColWidths[0] := 17;

    fIsEnsuringFixedCol := false;
  end;
end;

function TBoldCustomGrid.AddColumn: TBoldgridColumn;
begin
  Columns.BeginUpdate;
  try
    result := Columns.Add;
  finally
    Columns.EndUpdate;
  end;
end;

procedure TBoldCustomGrid.DeleteColumn(ACol: Integer);
begin
  if ColCount > 2 then
    inherited DeleteColumn(ACol);
end;

procedure TBoldCustomGrid.MoveColumn(FromIndex, ToIndex: Longint);
begin
  inherited MoveColumn(FromIndex, ToIndex);
end;

procedure TBoldCustomGrid.PostDisplayEvent(Sender: TObject);
begin
  Invalidate;
  fPostDisplayEventSet := false;
end;

function TBoldCustomGrid.CreateEditor: TInplaceEdit;
begin
  Result := TBoldInplaceEdit.Create(self);
  TBoldInplaceEdit(Result).Font.Assign(CellFont(Columns[Col]));
  result.width := Columns[col].Width;
end;

procedure TBoldCustomGrid.EditStop;
var
  CellFollower: TBoldFollower;
begin
  CellFollower := CurrentCellFollower;

  if assigned(CellFollower) and
    (CellFollower.Controller.ApplyPolicy = bapExit) then
    CellFollower.Apply;
end;

function TBoldCustomGrid.GetEditText(GridCol, GridRow: Longint): string;
var
  Editor: TInplaceEdit;
begin
  Editor := InplaceEditor;
  if (Editor is TBoldInplaceEdit) then
  begin
    TBoldInplaceEdit(Editor).Font.Assign(CellFont(Columns[Col]));
    Editor.Width := Columns[Col].Width;
  end;
  Result := GetString(GridCol, DataRow(GridRow));
end;

procedure TBoldCustomGrid.SetEditText(GridCol, GridRow: Longint; const Value: string);
begin
  if not (csDesigning in ComponentState) and Editormode and assigned(CurrentCellFollower) then
    TBoldStringFollowerController(CurrentCellFollower.Controller).MayHaveChanged(Value, CurrentCellFollower)
end;

procedure TBoldCustomGrid.Click;
begin
  fHandleFollower.SetFollowerIndex(DataRow(row));
  inherited;
end;

procedure TBoldCustomGrid.DblClick;
var
  autoform: TForm;
begin

  if (fLastMouseDownGridCoord.Y >= FixedRows) then
  begin
    if Assigned(OnDblClick) then
      inherited
    else
      if BoldProperties.DefaultDblClick and Assigned(CurrentBoldElement) then
      begin
        {$IFDEF BOLDCOMCLIENT}
        AutoForm := nil;
        {$ELSE}
        AutoForm := AutoFormProviderRegistry.FormForElement(CurrentBoldElement);
        {$ENDIF}
        if assigned(AutoForm) then
          AutoForm.Show;
      end;
  end;
end;

procedure TBoldCustomGrid.KeyPress(var KEY: Char);
begin
  if KEY = BOLDESC then
  begin
    if assigned(CurrentCellFollower) then
      CurrentCellFollower.DiscardChange;
    HideEditor;
    KEY := BOLDNULL;
  end;
  inherited KeyPress(KEY);
end;

procedure TBoldCustomGrid.SetBoldHandle(Value: TBoldAbstractListHandle);
begin
  fHandleFollower.BoldHandle := value;
end;

function TBoldCustomGrid.GetCurrentBoldElement: TBoldElement;
begin
  if Assigned(CurrentCellFollower) then
    Result := CurrentCellFollower.Element
  else
    Result := nil;
end;

function TBoldCustomGrid.GetBoldList: TBoldList;
begin
  if Assigned(BoldHandle) then
    Result := BoldHandle.List
  else
    Result := nil;
end;

procedure TBoldCustomGrid.SetController(Value: TBoldListAsFollowerListController);
begin
  fBoldProperties.Assign(Value);
end;

function TBoldCustomGrid.GetOptions: TGridOptions;
begin
  Result := inherited Options;
  if fRangeSelect then
    Include(Result, goRangeSelect);
end;

procedure TBoldCustomGrid.SetOptions(val: TGridOptions);
begin
  fRangeSelect := goRangeSelect in val;
  inherited Options := val - [goRangeSelect];
end;

procedure TBoldCustomGrid.SetPostDisplayEvent;
begin
  if fPostDisplayEventSet then
    exit;
  fPostDisplayEventSet := true;
  BoldInstalledQueue.AddEventToPostDisplayQueue(PostDisplayEvent, nil, self)
end;

procedure TBoldCustomGrid.SetSelection(aRow: Integer; Shift: TShiftState; ForceClearOfOtherRows: Boolean; IgnoreToggles: Boolean);
begin
  if aRow = -1 then
    Exit;

  fIsMultiSelecting := MultiSelect and ((ssShift in Shift) or (ssCtrl in Shift));
  if not ((ssShift in Shift) or (ssCtrl in Shift)) or not MultiSelect then
  begin
    if (not Follower.SubFollowers[aRow].Selected) or ForceClearOfOtherRows then
    begin
      fBoldProperties.SelectAll(Follower, False);
      fBoldProperties.SetSelected(Follower, aRow, True);
    end;
  end;
  if (ssShift in Shift) and MultiSelect then
  begin
    fBoldProperties.SelectRange(Follower, aRow);
  end;
  if (ssCtrl in Shift) and MultiSelect and (not IgnoreToggles) then
  begin
    fBoldProperties.ToggleSelected(Follower, aRow);
  end;


  Invalidate;
  AdjustActiveRange;
end;

procedure TBoldCustomGrid.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
  begin
    {$IFNDEF BOLDCOMCLIENT}
    if BoldGuiHandler.ActivateTargetFormOnDrop then
      BoldGUIHandler.TryToFocusHostingForm(self);
    {$ENDIF}
    inherited DragDrop(Source, X, Y);
  end
  else
    BoldProperties.DragDrop(Follower, MutableList, MouseCoord(X, Y).Y);
end;

procedure TBoldCustomGrid.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomGrid.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomGrid.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Assigned(OnDragOver)
    or (BoldProperties.DropMode = bdpNone)
    or ((Source = self) and (not BoldProperties.InternalDrag)) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, MutableList, MouseCoord(X, Y).Y);
end;

procedure TBoldCustomGrid.DefaultTitlePopupOnClick(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    TMenuItem(Sender).checked := not TMenuItem(Sender).checked;

    if Pos('__mnuBoldGridCWA', TMenuItem(Sender).name) = 1 then
      with Columns[TMenuItem(Sender).Owner.Tag] do
        if TMenuItem(Sender).checked then
          CWAdjust := CWAdjust + [TBoldCWAdjust(TMenuItem(Sender).Tag)]
        else
          CWAdjust := CWAdjust - [TBoldCWAdjust(TMenuItem(Sender).Tag)];
  end;
end;

function TBoldCustomGrid.DefaultTitlePopup(Col: Integer): TPopupMenu;
var
  I: Integer;
  M: TMenuItem;
begin
  Result := nil;
  if not Columns[Col].DefaultTitlePopupMenu then
    Exit;

  if not Assigned(TheDefaultTitlePopup) then
  begin
    TheDefaultTitlePopup := TPopupMenu.Create(self);
    with TheDefaultTitlePopup do
    begin
      for I := Byte(Low(TBoldCWAdjust)) to Byte(High(TBoldCWAdjust)) do
      begin
        M := TMenuItem.Create(TheDefaultTitlePopup);
        M.Caption := GetEnumName(TypeInfo(TBoldCWAdjust), I);
        M.OnClick := DefaultTitlePopupOnClick;
        M.name := '__mnuBoldGridCWA' + GetEnumName(TypeInfo(TBoldCWAdjust), I);
        M.Tag := I;
        M.RadioItem := False;
        Items.Add(M);
      end;
      M := TMenuItem.Create(TheDefaultTitlePopup);
      M.Caption := '-';
      M.name := '__mnuBoldGridSeparator';
      Items.Add(M);
      M := TMenuItem.Create(TheDefaultTitlePopup);
      M.Caption := '&Close Popup';
      M.name := '__mnuBoldGridCancel';
      Items.Add(M);


    end;
  end;
  Result := TheDefaultTitlePopup;
  with Result do
  begin
    Tag := Col;
    for I := 0 to Items.Count - 1 do
      Items[I].checked := TBoldCWAdjust(I) in Columns[Col].CWAdjust;
  end;
end;

procedure TBoldCustomGrid.TitleMenuPopup(GridCoord: TGridCoord; X, Y: Integer);
var
  P: TPoint;
  ThePopupMenu: TPopupMenu;
begin
  if Assigned(Columns[GridCoord.X].Title.PopupMenu) then
    ThePopupMenu := Columns[GridCoord.X].Title.PopupMenu
  else
    ThePopupMenu := DefaultTitlePopup(GridCoord.X);
  if Assigned(ThePopupMenu) then
  begin
    P := ClientToScreen(Point(X, Y));
    ThePopupMenu.Popup(P.X, P.Y);
  end;
end;

procedure TBoldCustomGrid.AutoAdjustCol(Col: Integer);
var
  CWA: TBoldCWAdjustSet;
begin
  if (Col > 0) and (Col < Columns.Count) then
  begin
    CWA := Columns[Col].CWAdjust;
    Columns[Col].CWAdjust := [caAllowGrow, caAllowShrink, caIncludeTitle];
    Columns[Col].CWAdjust := CWA;
  end;
end;

procedure TBoldCustomGrid.MouseUp(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if fEnableColAdjust and (fLastMouseDownScreenCoord.x = X) then
  begin
    AutoAdjustCol(MouseCoord(X, Y).X);
  end;

  if (Button = mbLeft) then
  begin
    if not fIsDragging then
      SetSelection(DataRow(Row), Shift, true, false)
    else
    begin

      if not selected[DataRow(Row)] and (ssCtrl in fLastMouseDownShiftState) then
        SetSelection(DataRow(Row), fLastMouseDownShiftState, true, false)
    end;
  end;

  fLastMouseDownScreenCoord := Point(-1, -1);
end;

procedure TBoldCustomGrid.MouseDown(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fLastMouseDownGridCoord := MouseCoord(X, Y);
  fLastMouseDownScreenCoord := Point(X, Y);

  {$IFNDEF BOLDCOMCLIENT}
  if (goEditing in Options) and
    (Button = mbLeft) and
    (fLastMouseDownGridCoord.x >= 0) and
    (Columns.Count > fLastMouseDownGridCoord.x) and
    assigned(Columns[fLastMouseDownGridCoord.x].fCheckBoxPainterRenderer) then
  begin
    if (fLastMouseDownGridCoord.Y >= FixedRows) then
    begin
      if (ssDouble in Shift) then
        DblClick
      else
      begin
        if not Columns[fLastMouseDownGridCoord.x].ColReadOnly then
          Columns[fLastMouseDownGridCoord.x].fCheckBoxPainterRenderer.CheckBoxClick(Button, shift, x, y, self, Columns[fLastMouseDownGridCoord.x].Alignment);
      end;
    end;
    exit;
  end;
  {$ENDIF}

  inherited;

  if (fLastMouseDownGridCoord.y <> -1) and (fLastMouseDownGridCoord.Y = TitleRow) then
  begin
    if fLastMouseDownGridCoord.X = Pred(FixedCols) then
    begin
      fBoldProperties.SelectAll(Follower, True);
      ReallyInvalidateCol(Pred(FixedCols));
      AdjustActiveRange;
    end
    else
      if BUTTON = mbRight then
      begin
        TitleMenuPopup(fLastMouseDownGridCoord, X, Y);
        fLastMouseDownScreenCoord := Point(-1, -1);
      end;
  end
  else
  begin
    if (Button = mbLeft) and (fLastMouseDownGridCoord.Y >= FixedRows) then
    begin
      Row := fLastMouseDownGridCoord.Y;
      if not ((fLastMouseDownGridCoord.x = pred(FixedCols)) and Selected[fLastMouseDownGridCoord.y]) then
        SetSelection(DataRow(Row), Shift, false, true)
    end;
  end;
  if (Button = mbLeft) and (fLastMouseDownGridCoord.X = Pred(FixedCols)) then
  begin
    try
      fIsDragging := true;
      BeginDrag(False);
    finally
      fIsDragging := false;
    end;
  end;
  fLastMouseDownShiftState := Shift;
end;

procedure TBoldCustomGrid.KeyDown(var KEY: Word; Shift: TShiftState);
const
  RowMovementKeys = [VK_UP, VK_PRIOR, VK_DOWN, VK_NEXT, VK_HOME, VK_END, VK_TAB];
begin
  if Key in Rowmovementkeys then
  begin
    HideEditor;
    if (ssShift in Shift) and MultiSelect then
      fIsMultiSelecting := true;
  end;

  if (Key = VK_DELETE) and (Shift = []) then
  begin
    if (goEditing in Options) and not (BoldHandle.List.Count = 0) then begin
      ShowEditor;
      InplaceEditor.Text := '';
      SetEditText(Col, Row, '');
    end;
  end;
  
  if (Row = RowCount - 1) and (KEY = 40) then {40 = KeyDown}
  begin
    if AddNewAtEnd and (fBoldProperties.NilElementMode<>neAddLast) then
    begin
      BoldHandle.List.AddNew;
      Follower.EnsureDisplayable;
    end
    else
      KEY := 0;
  end;
  {$IFNDEF BOLDCOMCLIENT}
  if not (Key in RowMovementKeys) and not (Key in [VK_LEFT, VK_RIGHT]) and ColumnIsCheckBox(Col) then
    Columns[Col].fCheckBoxPainterRenderer.KeyPress(Key, Shift, self)
  else
  {$ENDIF}
    inherited;
end;

procedure TBoldCustomGrid.KeyUp(var KEY: Word; Shift: TShiftState);
begin
  if KEY in [33..40] then
  begin


    Exclude(Shift, ssCtrl);
    SetSelection(DataRow(Row), Shift, true, true);
  end;
  inherited;
end;

function TBoldCustomGrid.GetString(GridCol, DataRow: Integer): string;
begin
  if not assigned(BoldHandle) or (DataRow >= BoldHandle.Count) then
    result := ''
  else
  begin
    EnsureRowActive(DataRow);
    if Assigned(CellFollowers[GridCol, DataRow]) and Assigned(CellFollowers[GridCol, DataRow].Controller) then
    begin
      Follower.EnsureDisplayable;
      Result := TBoldStringFollowerController(CellFollowers[GridCol, DataRow].Controller).GetCurrentAsString(CellFollowers[GridCol, DataRow]);
    end;
  end;
end;

function TBoldCustomGrid.HighlightCell(AState: TGridDrawState; aRow: integer): Boolean;
var
  RowSelected: Boolean;
begin
  RowSelected := Follower.SubFollowers[DataRow(aRow)].Selected;

  if fIsMultiSelecting then
    Result := Follower.SubFollowers[DataRow(aRow)].Selected
  else
    Result := (gdSelected in AState) and RowSelected and (AlwaysShowSelected or Focused);
end;

function TBoldCustomGrid.CellFont(Column: TBoldGridColumn): TFont;
begin
  if Assigned(Column.Font) then
    Result := Column.Font
  else
    Result := self.Font;
end;

procedure TBoldCustomGrid.AdjustCol(Col: Integer);
var
  TextW,
  I: Integer;
begin
  if Columns[Col].CWAdjust <> [] then
  begin
    TextW := 0;
    if not HasGhostRow then
      for I := TopRow to TopRow + MinIntValue([VisibleRowCount, Follower.SubFollowerCount]) - 1 do
        TextW := MaxIntValue([TextW, Canvas.TextWidth(GetString(Col, DataRow(I))) + 4]);
    if caIncludeTitle in Columns[Col].CWAdjust then
      TextW := MaxIntValue([TextW, Canvas.TextWidth(Columns[Col].Title.Caption) + 4]);

    if caAllowGrow in Columns[Col].CWAdjust then
      if TextW > ColWidths[Col] then
        ColWidths[Col] := TextW;

    if caAllowShrink in Columns[Col].CWAdjust then
      if TextW < ColWidths[Col] then
        ColWidths[Col] := TextW;
  end;
end;

procedure TBoldCustomGrid.ColWidthsChanged;
begin
  inherited;


end;

function TBoldCustomGrid.CanEditAcceptKey(KEY: Char): Boolean;
begin
  Result := Assigned(CurrentCellFollower) and
            TBoldStringFollowerController(CurrentCellFollower.Controller).ValidateCharacter(KEY, CurrentCellFollower);
end;

function TBoldCustomGrid.CanEditModify: Boolean;
begin
  if not Assigned(CurrentCellFollower) then
    Result := False
  else
    Result := CurrentCellFollower.MayModify and
    not Columns[Col].ColReadOnly;
end;

function TBoldCustomGrid.CanEditShow: Boolean;
begin
  Result := (inherited CanEditShow) and Assigned(CurrentCellFollower) and
            not Columns[Col].ColReadOnly;
  result := result and
    (CurrentCellFollower.MayModify
    {$IFNDEF BOLDCOMCLIENT} or assigned(Columns[Col].LookupHandle)
    {$ENDIF});

  {$IFNDEF BOLDCOMCLIENT}
  LookUpEditorActive := result and assigned(Columns[Col].LookupHandle);
  {$ENDIF}
end;

function TBoldCustomGrid.CanEditShowForCustomEditors: Boolean;
begin
  Result := (inherited CanEditShow) and Assigned(CurrentCellFollower);
end;

procedure TBoldCustomGrid.TopLeftChanged;
var
  Col: Integer;
begin
  inherited;
  AdjustActiveRange;
  for Col := FixedCols to Columns.Count - 1 do
    AdjustCol(Col);
  if Assigned(FOnTopLeftChanged) then
    FOnTopLeftChanged(Self);
end;

function TBoldCustomGrid.GetRowFollower(DataRow: Integer): TBoldFollower;
begin
  if (DataRow >= 0) and (datarow < Follower.SubFollowerCount) then
  begin
    Result := Follower.SubFollowers[DataRow];
  end
  else
    result := nil;
end;

function TBoldCustomGrid.GetCellFollower(ListCol, DataRow: Integer): TBoldFollower;
var
  RowFollower: TBoldFollower;
begin
  RowFollower := GetRowFollower(DataRow);
  if assigned(RowFollower) and
    (ListCol >= 0) and
    (listCol < RowFollower.SubFollowerCount) then
    begin
      Result := RowFollower.SubFollowers[ListCol];
    end
  else
    result := nil;
end;

procedure TBoldCustomGrid.DrawCell(ACol, aRow: Longint; ARect: TRect; AState: TGridDrawState);
var
  aListRow: Integer;
  DrawColumn: TBoldGridColumn;
  Align: TAlignment;
  cl: TColor;
  TempRect: TRect;
  FrameFlags1, FrameFlags2: DWORD;
  CellFollower: TBoldFollower;
begin
  if Follower.IsInDisplayList then exit;

  aListRow := DataRow(aRow);
  if (ACol > Columns.Count - 1) then
    Exit;
  FrameFlags1 := 0;
  FrameFlags2 := 0;
  if goFixedVertLine in Options then
  begin
    FrameFlags1 := BF_RIGHT;
    FrameFlags2 := BF_LEFT;
  end;
  if goFixedHorzLine in Options then
  begin
    FrameFlags1 := FrameFlags1 or BF_BOTTOM;
    FrameFlags2 := FrameFlags2 or BF_TOP;
  end;
  with Canvas do
  begin
    DrawColumn := Columns[ACol];
    if (((aListRow >= Follower.SubFollowerCount) or (aListRow < 0)) and not (aRow = TitleRow)) then
    begin
      ARect.Right := ARect.Right + 1;
      ARect.Bottom := ARect.Bottom + 1;
      Brush.Color := Color;
      FillRect(ARect);
      Exit;
    end
    else
    begin
      if (aRow = TitleRow) then {Title row}
      begin
        if Assigned(DrawColumn.Title) then
        begin
          Font.Assign(DrawColumn.Title.Font);
          Align := DrawColumn.Title.Alignment;
          Brush.Color := DrawColumn.Title.Color;
          TBoldAsStringRenderer.DrawStringOnCanvas(Canvas, ARect, Align, Point(1, 1), DrawColumn.Title.Caption);
        end
      end
      else
      begin
        if not Assigned(RowFollowers[aListRow]) or RowFollowers[aListRow].IsInDisplayList then
        begin
          SetPostDisplayEvent;
          exit;
        end;
        CellFollower := CellFollowers[ACol, aListRow];
        if not Assigned(CellFollower) or CellFollower.IsInDisplayList then
        begin
          SetPostDisplayEvent;
          exit;
        end;
        with Columns[ACol].BoldProperties do
        begin
          SetFont(Canvas.Font, CellFont(DrawColumn), CellFollower);
          SetColor(cl, DrawColumn.Color, CellFollower)
        end;

        if HighlightCell(AState, aRow) then
        begin
          Brush.Color := clHighlight;
          Canvas.Font.Color := clHighlightText;
        end
        else
          Brush.Color := cl;

        if DrawColumn.UserDraw then
        begin
          if Assigned(OnDrawCell) then
            OnDrawCell(DrawColumn, Canvas, ACol, aRow, ARect, AState);
        end
        else
        begin
          Columns[ACol].BoldProperties.DrawOnCanvas(CellFollower, Canvas, ARect, DrawColumn.Alignment, Point(2, 2));
        end;
      end;
      if (gdFixed in AState) and Ctl3D and
        ((FrameFlags1 or FrameFlags2) <> 0) then
      begin
        TempRect := ARect;
        if (FrameFlags1 and BF_RIGHT) = 0 then
          Inc(TempRect.Right, GridLineWidth)
        else if (FrameFlags1 and BF_BOTTOM) = 0 then
          Inc(TempRect.Bottom, GridLineWidth);
        DrawEdge(Canvas.Handle, TempRect, BDR_RAISEDINNER, FrameFlags1);
        DrawEdge(Canvas.Handle, TempRect, BDR_RAISEDINNER, FrameFlags2);
      end;
      if not (csDesigning in ComponentState) and
        (gdSelected in AState) and
        ([goEditing, goAlwaysShowEditor] * Options <>
         [goEditing, goAlwaysShowEditor])
        and not (goRowSelect in Options) then
        Canvas.DrawFocusRect(ARect);
    end;
  end;
end;

function TBoldCustomGrid.GridRow(Datarow: Integer): Integer;
begin
  Result := Datarow + FixedRows;
end;

function TBoldCustomGrid.DataRow(GridRow: Integer): Integer;
begin
  if HasGhostRow then
    Result := -1
  else
    Result := GridRow - FixedRows;
end;

procedure TBoldCustomGrid._AfterMakeCellUptoDate(Follower: TBoldFollower);
var
  DisplayGridRow: Integer;
  DisplayCol: Integer;
begin
  DisplayGridRow := GridRow(Follower.OwningFollower.index);
  DisplayCol := Follower.index;
  if Assigned(InplaceEditor) and InplaceEditor.Visible and (Row = DisplayGridRow)  and (Col = DisplayCol) then
    InplaceEditor.Text := (Follower.Controller as TBoldStringFollowerController).GetCurrentAsString(Follower);
  if (DisplayGridRow >= TopRow) and (DisplayGridRow <= (TopRow + VisibleRowCount + 1)) then
    InvalidateCell(DisplayCol, DisplayGridRow);
end;

procedure TBoldCustomGrid.InvalidateFromRow(DisplayDataRow: Longint);
var
  I: Integer;
begin
  for I := GridRow(DisplayDataRow) to MinIntValue([RowCount - 1, TopRow + VisibleRowCount + 1]) do
    InvalidateRow(I);
end;

procedure TBoldCustomGrid._InsertRow(index: Integer; OwningFollower: TBoldFollower);
begin
  if Index < fInvalidateFrom then
    fInvalidateFrom := Index;
  fLastInsertedRowIndex := Index;
end;

procedure TBoldCustomGrid._ReplaceRow(index: Integer;
  AFollower: TBoldFollower);
begin
  if Index < fInvalidateFrom then
    fInvalidateFrom := Index;
end;

procedure TBoldCustomGrid._DeleteRow(index: Integer; owningFollower: TBoldFollower);
begin
  if Index < fInvalidateFrom then
    fInvalidateFrom := Index;
end;

procedure TBoldCustomGrid.SetCurrentRow(DataRow: Integer);
begin
  Row := GridRow(MaxIntValue([0, DataRow]));
  ReallyInvalidateCol(0);
end;

function TBoldCustomGrid.GetCurrentCellFollower;
begin
  if (Col < FixedCols)  then
    Result := nil
  else
    Result := CellFollowers[Col, DataRow(Row)];
end;

function TBoldCustomGrid.GetSelected(DataRow: integer): Boolean;
var
  lBoldFollower: TBoldFollower;
begin
  Result := false;
  if (DataRow >= 0) and (DataRow < Follower.SubFollowerCount) then
  begin
    lBoldFollower := RowFollowers[DataRow];
    if Assigned(lBoldFollower) then
      Result := RowFollowers[DataRow].Selected;
  end;
end;

procedure TBoldCustomGrid._AfterMakeListUptoDate(Follower: TBoldFollower);
var
  OldLeftCol: integer;
begin

  if fInvalidateFrom <= DataRow(Row) then
    EditorMode := False;
  fHasGhostRow := Follower.SubFollowerCount = 0;
  if HasGhostRow then
    RowCount :=  FixedRows + 1
  else
    RowCount := Follower.SubFollowerCount + FixedRows;
  if AutoSelectNewRows and
    (fSubFollowerCountBeforeMakeUpToDate = Follower.SubFollowerCount - 1) then
  begin
    BoldHandle.CurrentIndex := fLastInsertedRowIndex;
    Follower.CurrentIndex := fLastInsertedRowIndex;
    SetCurrentRow(Follower.CurrentIndex);
  end;

  if Assigned(fEditedElementBeforeMakeUpToDate) and
    ((Follower.CurrentIndex = -1) or
      not assigned(RowFollowers[Follower.CurrentIndex]) or
      (fEditedElementBeforeMakeUpToDate <> RowFollowers[Follower.CurrentIndex].Element)) then
    EditorMode := false;
{
  if (fSubFollowerCountBeforeMakeUpToDate = 0) and
     (Follower.SubFollowerCount > 0) and
     (Follower.CurrentIndex <> -1) then
  begin
    Follower.SubFollowers[Follower.CurrentIndex].Selected := true;
  end;
}
  AdjustActiveRange;
  if fInvalidateFrom <> MAXINT then
    InvalidateFromRow(fInvalidateFrom);
  if GridRow(Follower.CurrentIndex) < RowCount then
  begin
    OldLeftCol := LeftCol;
    SetCurrentRow(Follower.CurrentIndex);
    LeftCol := OldLeftCol;
  end;

  if not fIsMultiSelecting and (Follower.CurrentIndex <> -1) and (Follower.SubFollowerCount > 0) and
    assigned(Follower.SubFollowers[Follower.CurrentIndex]) and
    not Follower.SubFollowers[Follower.CurrentIndex].Selected then
  begin
    fBoldProperties.SelectAll(Follower, False);
    fBoldProperties.SetSelected(Follower, Follower.CurrentIndex, True);
  end;
  fMakingListUpToDate := False;
end;

function TBoldCustomGrid.GetBoldHandle: TBoldAbstractListHandle;
begin
  if not assigned(fHandleFollower) then
    result := nil
  else
    Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomGrid.GetFollower: TBoldFOllower;
begin
  Assert(Assigned(fHandleFollower));
  Result := fHandleFollower.Follower;
end;

procedure TBoldCustomGrid._BeforeMakeListUpToDate(Follower: TBoldFollower);
begin
  TypeMayHaveChanged;
  fMakingListUpToDate := True;
  AdjustActiveRange;
  fInvalidateFrom := MAXINT;
  fSubFollowerCountBeforeMakeUpToDate := Follower.SubfollowerCount;
  if Assigned(InplaceEditor) and InplaceEditor.Visible then
    fEditedElementBeforeMakeUpToDate := RowFollowers[DataRow(Row)].Element
  else
    fEditedElementBeforeMakeUpToDate := nil;
end;

function TBoldCustomGrid.GetHandleStaticType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomGrid.GetHandleListElementType: TBoldElementTypeInfo;
begin
  if Assigned(BoldHandle) then
    Result := BoldHandle.ListElementType
  else
    Result := nil;
end;

procedure TBoldCustomGrid.SetTitleFont(const Value: TFont);
begin
  fTitleFont.Assign(Value);
end;

function TBoldCustomGrid.GetBoldHandleIndexLock: Boolean;
begin
  Result := fHandleFollower.HandleIndexLock;
end;

procedure TBoldCustomGrid.SetBoldHandleIndexLock(Value: Boolean);
begin
  fHandleFollower.HandleIndexLock := Value;
end;

{$IFNDEF BOLDCOMCLIENT}

procedure TBoldInplaceEdit.ComboChange(sender: TObject);
begin
  if not fComboAborting then
    Grid.LookUpChange(sender, fDestElement, fEditColumn);
  HideCombo;
end;

procedure TBoldInplaceEdit._Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = breDestElementDestroyed then
    fDestElement := nil;
end;

procedure TBoldInplaceEdit.HideCombo;
begin
  Combo.visible := false;
end;

function TBoldInplaceEdit.GetDestElement(CellFollower: TBoldFollower; Column: TBoldGridColumn): TBoldElement;
var
  ie: TBoldIndirectElement;
begin
  result := nil;
  if assigned(cellFollower) and assigned(CellFollower.Element) then
  begin

    if assigned(Column.OnLookupChange) then
      result := CellFollower.Element
    else
    begin
      ie := TBoldIndirectElement.Create;
      try
        CellFollower.Element.EvaluateExpression(Column.BoldProperties.Expression, ie, false, Column.BoldProperties.VariableList);
        if assigned(ie.value) and not ie.OwnsValue then
          result := ie.Value
      finally
        ie.Free;
      end;
    end;
  end;
end;

function TBoldInplaceEdit.GetCombo: TComboBox;
begin
  if not assigned(fCombo) then
  begin
    fCombo := TBoldInplaceCombo.CreateWithInplaceEditor(self);
    fCombo.Style := csDropDownList;
    fCombo.Visible := False;
    fCombo.OnChange := ComboChange;
    fCombo.OnClick := ComboChange;
    fCombo.OnKeyDown := ComboKeyDown;
    fCombo.OnExit := ComboChange;
  end;
  result := fCombo;
end;

procedure TBoldInplaceEdit.WMKeyDown(var Message: TWMKeyDown);
begin
  if Combo.Visible then
  begin
    if (Message.CharCode = VK_ESCAPE) then
    begin
      HideCombo;
      KeyDown(Message.CharCode, []);
      Grid.SetFocus;
    end
    else
    begin
      Combo.SetFocus;
      Combo.Perform(WM_CHAR, TMessage(Message).WParam, TMessage(Message).LParam);
    end;
  end
  else
  begin
    inherited;
  end;
end;

procedure TBoldInplaceEdit.InitCombo(var Message: TWMWindowPosChanged);
var
  i: Integer;
  CellFollower: TBoldFollower;
  ListElement: TBoldElement;
  CellValue: TBoldElement;
begin
  fEditColumn := Grid.Columns[Grid.Col] as TBoldGridColumn;
  if Grid.LookUpEditorActive then
  begin
    Combo.Parent := Parent;
    Combo.Clear;
    CellFollower := Grid.CellFollowers[Grid.Col, Grid.Row-Grid.FixedRows];
    fDestElement := GetDestElement(CellFollower, fEditColumn);
    fDestElementSubscriber.CancelAllSubscriptions;
    if assigned(fDestElement) then
      fDestElement.AddSmallSubscription(fDestElementSubscriber, [beDestroying], breDestElementDestroyed);

    CellValue := fDestElement;
    if CellValue is TBoldObjectReference then
      CellValue := (CellValue as TBoldObjectReference).BoldObject;

    fInitialComboIndex := -1;
    if fEditColumn.NilElementMode = neInsertFirst then
    begin
      fInitialComboIndex := 0;
      Combo.Items.AddObject(fEditColumn.LookUpProperties.NilStringRepresentation, nil);
    end;

    if assigned(fEditColumn.LookupHandle.List) then
      fEditColumn.LookupHandle.List.EnsureRange(0, fEditColumn.LookupHandle.Count - 1);
    for i := 0 to fEditColumn.LookupHandle.Count - 1 do
    begin
      ListElement := fEditColumn.LookupHandle.List[i];

      Combo.Items.AddObject(
        ListElement.EvaluateExpressionAsString(fEditColumn.LookUpProperties.Expression, fEditColumn.LookUpProperties.Representation),
        ListElement);

      if (CellValue = ListElement) or
        ((CellValue is TBoldAttribute) and CellValue.IsEqual(ListElement)) then
        fInitialComboIndex := Combo.Items.Count - 1;
    end;

    if fEditColumn.NilElementMode = neAddLast then
    begin
      if fInitialComboIndex = -1 then
        fInitialComboIndex := Combo.Items.Count;
      Combo.Items.AddObject(fEditColumn.LookUpProperties.NilStringRepresentation, nil);
    end;

    Combo.Top := Message.WindowPos.y - 1;
    Combo.Left := Message.WindowPos.x - 1;
    Combo.Height := Message.WindowPos.cy + Combo.ItemHeight * Combo.DropDownCount;
    Combo.Width := Message.WindowPos.cx + 3;
    Combo.ItemIndex := fInitialComboIndex;
    Combo.Visible := True;
    Combo.BringToFront;
  end;
end;

procedure TBoldInplaceEdit.ComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Sender = Combo then
  begin
    case Key of
      VK_RETURN:
      begin
        ComboChange(self);
        KeyDown(Key, Shift);
        Grid.SetFocus;
      end;
      VK_ESCAPE:
      begin
        Combo.ItemIndex := fInitialComboIndex;
        fComboAborting := true;
        try
          HideCombo;
        finally
          fComboAborting := false;
        end;
        KeyDown(Key, Shift);
        Grid.SetFocus;
      end;
    end;
  end;
end;

{$ENDIF}

procedure TBoldCustomGrid._FontChanged(Sender: TObject);
begin
  ParentFont := false;
end;

function TBoldCustomGrid.GetEditLimit: Integer;
var
  El: TBoldElement;
begin
  result := 0;
  {$IFNDEF BOLDCOMCLIENT}
  El := TBoldInplaceEdit(InplaceEditor).GetDestElement(CurrentCellFollower, Columns[Col]);
  if (el is TBAString) and assigned((el as TBAString).BoldAttributeRTInfo) then
    Result := (el as TBAString).BoldAttributeRTInfo.Length;
  if result < 0 then
    result := 0;
  {$ENDIF}
end;

{ TBoldFirstColumnRenderer }

procedure TBoldFirstColumnRenderer.DrawOnCanvas(Follower: TBoldFollower;
  Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint);
begin
  inherited;
  with Follower do
  begin
    Canvas.Brush.Color := (Owner as TBoldCustomGrid).FixedColor;
    Canvas.FillRect(Rect);
    if OwningFollower.OwningFollower.CurrentIndex = OwningFollower.index then
      Canvas.Draw(Rect.Left + 8, Rect.Top + 4, bmpBoldGridCurrent);
    if OwningFollower.Selected then
      Canvas.Draw(Rect.Left, Rect.Top + 4, bmpBoldGridSelected);
  end;
end;

{$IFNDEF BOLDCOMCLIENT}

procedure TBoldGridCheckBoxPainterRenderer.GetCurrentElement(Follower: TBoldFollower; ie: TBoldIndirectElement);
var
  StringController: TBoldStringFollowerController;
begin
  StringController := Follower.AssertedController as TBoldStringFollowerController;
  Follower.element.EvaluateExpression(StringController.Expression, ie, false, StringController.VariableList);
end;

function TBoldGridCheckBoxPainterRenderer.GetCurrentCheckBoxState(Follower: TBoldFollower): TCheckBoxState;
var
  BoolValue: TBABoolean;
  ie: TBoldIndirectElement;
begin
  if fColumn.ColumnHasCheckBoxOverrides then
    result := fColumn.GetCurrentCheckBoxState(follower)
  else
  begin
    ie := TBoldIndirectElement.Create;
    try
      if assigned(follower) then
        GetCurrentElement(Follower, ie);
      if ie.value is TBABoolean then
      begin
        BoolValue := ie.value as TBABoolean;
        if BoolValue.IsNull then
          result := cbGrayed
        else if BoolValue.AsBoolean then
          Result := cbChecked
        else
          Result := cbUnChecked
      end
      else
        Result := cbGrayed
    finally
      ie.Free;
    end;
  end;
end;

function TBoldGridCheckBoxPainterRenderer.GetCheckBoxRect(rect: TRect; Alignment: TAlignment): TRect;
const
  CheckBoxSize = 12;
begin
  Result.Bottom := Rect.Bottom - 3;
  Result.Top := Result.Bottom - CheckBoxSize;

  case alignment of
    taCenter: Result.left := Rect.left + (Rect.right - Rect.left - CheckBoxSize) div 2;
    taLeftJustify: Result.Left := Rect.Left + 3;
    taRightJustify: Result.Left := rect.Right - 3 - CheckBoxSize;
  end;

  Result.Right := Result.Left + CheckBoxSize;
end;

procedure TBoldGridCheckBoxPainterRenderer.CheckBoxClick(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer; Grid: TBoldCustomGrid; Alignment: TAlignment);
var
  GridCoord: TGridCoord;
  CellRect, CheckBoxRect: TRect;
  i: integer;
begin
  GridCoord := GRid.MouseCoord(X, Y);
  CellRect.Left := 0;
  for i := 0 to Grid.FixedCols - 1 do
    CellRect.Left := CellRect.Left + Grid.Columns[i].Width + Grid.GridLineWidth;
  for i := GRid.LeftCol to GridCoord.x - 1 do
    CellRect.Left := CellRect.Left + Grid.Columns[i].Width + Grid.GridLineWidth;
  CellRect.right := CellRect.Left + GRid.Columns[GridCoord.x].Width;
  CheckBoxRect := GetCheckBoxRect(CellRect, alignment);

  if (GridCoord.y >= Grid.FixedRows) and (gridCoord.y < grid.RowCount) then
  begin
    if (x >= CheckBoxRect.Left) and (x <= CheckBoxRect.Right) then
      ToggleValue(GridCoord, grid);

    if (GridCoord.x > 0) and (GridCoord.x < Grid.Columns.Count) then
    begin
      Grid.Col := GridCoord.x;
      Grid.Row := GridCoord.y;
    end;
  end;
end;

procedure TBoldGridCheckBoxPainterRenderer.KeyPress(var Key: word; Shift: TShiftState; Grid: TBoldCustomGrid);
var
  GridCoord: TGridCoord;
begin
  GridCoord.x := Grid.Col;
  GridCoord.y := Grid.Row;
  if (Key = 32) and (shift = []) then
  begin
    ToggleValue(GridCoord, grid);
    Key := 0;
  end;
end;

procedure TBoldGridCheckBoxPainterRenderer.ToggleValue(GridCoord: TGridCoord; Grid: TBoldCustomGrid);
var
  CheckBoxState: TCheckBoxState;
  CurrentFollower: TBoldFollower;
  BoolValue: TBABoolean;
  ie: TBoldIndirectElement;
begin
  CurrentFollower := Grid.CellFollowers[GridCoord.x, GridCoord.y-Grid.FixedRows];
  if CurrentFollower.MayModify then
  begin
    if fColumn.ColumnHasCheckBoxOverrides then
    begin
      CheckBoxState := fCOlumn.GetCurrentCheckBoxState(CurrentFollower);
      case CheckBoxState of
        cbGrayed: fColumn.SetCurrentCheckBoxState(CurrentFollower, cbChecked);
        cbChecked: fColumn.SetCurrentCheckBoxState(CurrentFollower, cbUnChecked);
        cbUnChecked: fColumn.SetCurrentCheckBoxState(CurrentFollower, cbChecked);
      end;
    end
    else
    begin
      CheckBoxState := GetCurrentCheckBoxState(CurrentFollower);
      ie := TBoldIndirectElement.Create;
      try
        GetCurrentElement(CurrentFollower, ie);
        if ie.value is TBABoolean then
        begin
          BoolValue := ie.value as TBABoolean;
          if BoolValue.CanModify then
          begin
            case CheckBoxState of
              cbGrayed: BoolValue.AsBoolean := false;
              cbUnchecked: BoolValue.AsBoolean := true;
              cbChecked: begin
                if BoolValue.CanSetToNull(nil) then
                  BoolValue.SetToNull
                else
                  BoolValue.AsBoolean := False;
              end;
            end;
          end;
        end;
      finally
        ie.Free;
      end;
    end;
  end;
end;

procedure TBoldGridCheckBoxPainterRenderer.DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint);
var
  OrgPenColor, OrgBrushColor: TColor;
  GridCoord: TGridCoord;
  CheckBoxRect, GrayRect: TRect;
  i: integer;
  checkBoxState: TCheckBoxState;
  Grid: TBoldCustomGrid;
begin
  CheckBoxState := GetCurrentCheckBoxState(Follower);
  OrgPenColor := Canvas.Pen.Color;
  OrgBrushColor := Canvas.Brush.Color;

  GRid := TExposedFollowerController(Follower.AssertedController).OwningComponent as TBoldCustomGrid;
  GridCoord := Grid.MouseCoord(rect.left + 1, rect.top + 1);
  if (GRidCoord.x = Grid.Col) and (GridCoord.y = GRid.Row) then
    Canvas.Brush.Color := clHighlight
  else
    Canvas.Brush.Color := Grid.Color;

  Canvas.FillRect(rect);

  CheckBoxRect := GetCheckBoxRect(rect, Alignment);
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(CheckBoxRect);
  Canvas.Pen.Color := clDkGray;
  Canvas.MoveTo(CheckBoxRect.Left, CheckBoxRect.Bottom);
  Canvas.LineTo(CheckBoxRect.left, CheckBoxRect.Top);
  Canvas.LineTo(CheckBoxRect.Right, CheckBoxRect.Top);

  Canvas.Pen.Color := clBlack;
  Canvas.MoveTo(CheckBoxRect.Left + 1, CheckBoxRect.Bottom - 1);
  Canvas.LineTo(CheckBoxRect.left + 1, CheckBoxRect.Top + 1);
  Canvas.LineTo(CheckBoxRect.Right - 1, CheckBoxRect.Top + 1);

  Canvas.Pen.Color := clltGray;
  Canvas.MoveTo(CheckBoxRect.Left + 1, CheckBoxRect.Bottom);
  Canvas.LineTo(CheckBoxRect.Right - 1, CheckBoxRect.Bottom);
  Canvas.LineTo(CheckBoxRect.Right - 1, CheckBoxRect.Top);

  case CheckBoxState of
    cbChecked:
    begin
      Canvas.Pen.Color := clBlack;
      for i := 0 to 2 do
      begin
        Canvas.MoveTo(CheckBoxRect.Left + 3, CheckBoxRect.Top + 5 + i);
        Canvas.LineTo(CheckBoxRect.Left + 5, CheckBoxRect.Top + 7 + i);
        Canvas.LineTo(CheckBoxRect.Left + 10, CheckBoxRect.Top + 2 + i);
      end;
    end;
    cbGrayed:
    begin
      Canvas.Brush.Color := cl3DLight;
      GrayRect.Top := CheckBoxRect.Top + 2;
      GrayRect.Bottom := CheckBoxRect.Bottom - 1;
      GrayRect.Left := CheckBoxRect.Left + 2;
      GrayRect.Right := CheckBoxRect.Right - 1;
      Canvas.FillRect(GrayRect);
    end;
  end;

  Canvas.Pen.Color := OrgPenColor;
  Canvas.Brush.Color := OrgBrushColor;
end;

{$ENDIF}

{ TBoldConstraintRenderer }

procedure TBoldConstraintRenderer.DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect;
  Alignment: TAlignment; Margins: TPoint);
var
  RectMiddle, IncValue: Integer;
begin
  with Follower do
  begin
    Canvas.FillRect(Rect);
    RectMiddle := (Rect.Bottom - Rect.Top) div 2;
    if (RendererData as TBoldStringRendererData).CurrentStringValue = 'Y' then
    begin
      IncValue := RectMiddle - (bmpBoldGridConstraint_false.Height div 2);
      Canvas.Draw(Rect.Left + 1, Rect.Top + IncValue, bmpBoldGridConstraint_false);
    end
    else
    if (RendererData as TBoldStringRendererData).CurrentStringValue = 'N' then
    begin
      IncValue := RectMiddle - (bmpBoldGridConstraint_true.Height div 2);
      Canvas.Draw(Rect.Left + 1, Rect.Top + IncValue, bmpBoldGridConstraint_true);
    end;
  end;
end;

function TBoldGridColumn.GetFont: TFont;
var
  SavedOnChange: TNotifyEvent;
begin
  if not (cvTitleFont in fAssignedValues) then
  begin
    if not AreFontsEqual(DefaultFont, fFont) then
    begin
      SavedOnChange := FFont.OnChange;
      FFont.OnChange := nil;
      FFont.Assign(DefaultFont);
      FFont.OnChange := SavedOnChange;
    end;
  end;
  Result := FFont;
end;

procedure TBoldCustomGrid.EnsureConstraintColumn;
begin
  if BoldShowConstraints and not (csdesigning in componentState) then
  begin
    with AddColumn do
    begin
      SetIndex(1);
      BoldProperties.Expression := 'constraints->exists(c|not c)';
      Width := bmpBoldGridSelected.Width + 3;
      Title.Caption := '�';
      BoldProperties.Renderer := TBoldConstraintRenderer.Create(Self);
      ColReadOnly := True;
    end;
  end;
end;

procedure TBoldGridColumn.SetIndex(Value: Integer);
begin
  fGrid.fBoldColumnsProperties.Move(index, value);
  inherited;
  fGrid.Invalidate;
end;

procedure TBoldCustomGrid.ReallyInvalidateCol(Column: integer);
{var
  Rect: TGridRect;
}
begin
  Invalidate;
{  // InvalidateRect is private, so we can not call it... otherwise the code below would be better.
  if not HandleAllocated then Exit;
  Rect.Top := TopRow;
  Rect.Left := Column;
  Rect.Bottom := TopRow + VisibleRowCount + 1;
  Rect.Right := Column;
  InvalidateRect(Rect);
}
end;

function TBoldCustomGrid.GetMutableList: TBoldList;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.MutableList
  else
    result := nil;
end;

procedure TBoldCustomGrid.AdjustActiveRange;
var
  firstActive, LastActive: Integer;
begin
  if Assigned(Follower) then
  begin
    GetActiveRange(FirstActive, LastActive);
    BoldProperties.SetActiveRange(Follower, firstActive, lastActive, 10);
//    EnsureActiveCellFollowerExpressions;
  end;
end;

procedure TBoldCustomGrid.Resize;
begin
  inherited;
  if not fMakingListUpToDate then
    AdjustActiveRange;
end;

function TBoldCustomGrid.GetShowTitleRow: Boolean;
begin
  result := fixedRows = 1;
end;

procedure TBoldCustomGrid.SetShowTitleRow(const Value: Boolean);
begin
  if Value then
    FixedRows := 1
  else
    FixedRows := 0;
end;

function TBoldCustomGrid.GetTitleRow: integer;
begin
  if ShowTitleRow then
    Result := 0
  else
    Result := -1;
end;

{$IFNDEF BOLDCOMCLIENT}
function TBoldCustomGrid.ColumnIsCheckBox(Col: integer): Boolean;
begin
  result := (Col >= 0) and (Col < Columns.Count) and assigned(Columns[Col].fCheckBoxPainterRenderer);
end;

procedure TBoldCustomGrid.WMChar(var Msg: TWMChar);
begin
  if not ColumnIsCheckBox(col) then
    inherited;
end;

function TBoldCustomGrid.ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean;
var
  i: integer;
  Context: TBoldElementTypeInfo;
begin
  Context := GetHandleStaticType;
  result := ComponentValidator.ValidateExpressionInContext(
      '', Context, format('%s%s', [NamePrefix, Name]));
  if assigned(context) then
    for i := 0 to Columns.Count - 1 do
      result := ComponentValidator.ValidateExpressionInContext(
        Columns[i].BoldProperties.Expression,
        Context,
        format('%s%s.Column[%d]', [NamePrefix, Name, i])) and result;
end;

procedure TBoldCustomGrid.LookUpChange(sender: Tobject; DestElement: TBoldElement; EditColumn: TBoldGridColumn);
var
  aIndex: Integer;
begin
  aIndex := -1;
  if LookUpEditorActive then
    aIndex := GetEditor.Combo.ItemIndex;

  LookUpEditorActive := True;
  if (aIndex > -1) and assigned(DestElement) then
  begin
    if assigned(EditColumn) and assigned(EditColumn.OnLookupChange) then
      EditColumn.OnLookUpChange(DestElement, GetEditor.Combo.Items.Objects[aIndex] as TBoldElement)
    else
      DestElement.Assign(GetEditor.Combo.Items.Objects[aIndex] as TBoldElement);
  end;
end;
{$ENDIF}

function TBoldCustomGrid.ColumnClass: TBoldColumnClass;
begin
  result := TBoldGridColumn;
end;

function TBoldCustomGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnSelectCell) then
    FOnSelectCell(Self, ACol, ARow, Result);
end;

function TBoldCustomGrid.GetEditor: TBoldInplaceEdit;
begin
  result := InplaceEditor as TBoldInplaceEdit;
end;

function TBoldCustomGrid.AsClipBoardText: String;
var
  Col, Row: integer;
begin
  ActivateAllCells;
  Result := '';
  for row := 0 to RowCount - 1 do
  begin
    for col := FixedCols to ColCount - 1 do
      Result := Result + CellText[Col, Row] + #9;
    Result := Result + BOLDCRLF;
  end;
end;

{$IFNDEF BOLDCOMCLIENT}

procedure TBoldGridColumn._Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = breColumnLookUpHandleDestroyed then
    LookupHandle := nil;
end;

procedure TBoldGridColumn.SetCheckBoxRendererIfAppropriate;
var
  SystemTypeinfo: TBoldSystemTypeInfo;
  ListElementTypeInfo: TBoldElementTypeInfo;
  BooleanTypeInfo: TBoldAttributeTypeInfo;
  ResultTypeInfo: TBoldElementTypeInfo;
begin
  if not assigned(BoldProperties.Renderer) and
    assigned(Grid.BoldHandle) and
    assigned(Grid.BoldHandle.ListElementType) and
    (AllowCheckBox or BoldAllowCheckBoxInGrids) then
  begin
    ListElementTypeInfo := Grid.BoldHandle.ListElementType;
    SystemTypeInfo := ListElementTypeInfo.SystemTypeInfo as TBoldSystemTypeInfo;
    BooleanTypeInfo := SystemTypeinfo.AttributeTypeInfoByExpressionName['Boolean'];
    ResultTypeInfo := SystemTypeinfo.Evaluator.ExpressionType(BoldProperties.Expression, ListElementTypeInfo, false, BoldProperties.VariableList);
    if (assigned(ResultTypeInfo) and ResultTypeInfo.ConformsTo(BooleanTypeInfo)) or ColumnHasCheckBoxOverrides then
    begin
      fCheckBoxPainterRenderer := TBoldGridCheckBoxPainterRenderer.Create(Grid);
      fCheckBoxPainterRenderer.fColumn := self;
      BoldProperties.Renderer := fCheckBoxPainterRenderer;
    end;
  end;
end;

function TBoldGridColumn.ColumnHasCheckBoxOverrides: Boolean;
begin
  result := false;
end;

function TBoldGridColumn.GetCurrentCheckBoxState(
  Follower: TBoldFollower): TCheckBoxState;
begin
  result := cbGrayed;
end;

procedure TBoldGridColumn.SetCurrentCheckBoxState(Follower: TBoldFollower; NewValue: TCheckBoxState);
begin
end;

function TBoldGridColumn.GetLookupContext: TBoldElementTypeInfo;
begin
  if assigned(LookupHandle) then
    result := LookupHandle.StaticBoldType
  else
    result := nil;
end;

procedure TBoldGridColumn.SetLookupHandle(const Value: TBoldAbstractListHandle);
begin
  if value <> LookupHandle then
  begin
    fLookupHandleSubscriber.CancelAllSubscriptions;
    fLookupHandle := Value;
    if assigned(fLookUpHandle) then
      fLookupHandle.AddSmallSubscription(fLookUpHandleSubscriber, [beDestroying], breColumnLookUpHandleDestroyed);
  end;
end;

procedure TBoldGridColumn.SetLookUpProperties(const Value: TBoldStringFollowerController);
begin
  if assigned(value) then
    fLookUpProperties.Assign(Value);
end;

{$ENDIF}

procedure TBoldCustomGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if BoldDragAnywhere and (ssLeft in Shift) and
    (fLastMouseDownScreenCoord.X <> -1) and (
    (abs(fLastMouseDownScreenCoord.X - X) > mouse.DragThreshold) or
    (abs(fLastMouseDownScreenCoord.Y - Y) > mouse.DragThreshold)) then
  begin
    if fLastMouseDownGridCoord.y >= FixedRows then
    begin
      try
        fIsDragging := true;
        BeginDrag(False);
      finally
        fIsDragging := false;
      end;
    end;
  end;
end;

procedure TBoldCustomGrid.ActivateAllCells;
begin
  BoldProperties.SetActiveRange(Follower, 0, RowCount - 1);
  DisplayAvailableFollowers;
end;

procedure TBoldCustomGrid.GetActiveRange(var FirstActive, LastActive: integer);
var
  i: integer;
begin
  firstActive := DataRow(TopRow) - 1;
  LastActive := DataRow(TopRow + VisibleRowCount) + 1;
  for i := 0 to FirstActive - 1 do
    if Selected[i] then
    begin
      FirstActive := i;
      break;
    end;
  for i := DataRow(RowCount - 1) downto LastActive + 1 do
    if Selected[i] then
    begin
      LastActive := i;
      break;
    end;
end;

procedure TBoldCustomGrid.EnsureRowActive(DataRow: integer);
var
  firstActive, LastActive: Integer;
begin
  if Assigned(Follower) and (DataRow >= 0) and
     assigned(BoldHandle) and (DataRow < BoldHandle.Count) then
  begin
    if not assigned(RowFollowers[DataRow]) or not RowFollowers[DataRow].Active then
    begin
      GetActiveRange(FirstActive, LastActive);
      if DataRow < FirstActive then
        FirstActive := DataRow
      else if DataRow > LastActive then
        LastActive := DataRow;
      BoldProperties.SetActiveRange(Follower, firstActive, lastActive, 10);
      Follower.EnsureDisplayable;
    end;
  end;
end;

procedure TBoldCustomGrid.DisplayAvailableFollowers;
begin
  Follower.EnsureDisplayable;
end;

function TBoldCustomGrid.GetCellText(col, row: integer): string;
begin
  if row < FixedRows then
    Result := Columns[Col].Title.Caption
  else
    result := GetString(col, DataRow(Row));
end;

procedure TBoldCustomGrid.DisplayAllCells;
begin
  BoldProperties.SelectAll(Follower, true);
  Invalidate;
  AdjustActiveRange;
  EnsureActiveCellFollowerExpressions;
  Follower.EnsureDisplayable;
  BoldProperties.SelectAll(Follower, false);
  BoldProperties.SetSelected(Follower, DataRow(Row), true);
end;

procedure TBoldCustomGrid.EnsureActiveCellFollowerExpressions;
var
  i: integer;
begin
  if Follower.SubFollowerCount > 0 then
  begin
    for i := FixedCols to ColCount - 1 do
      if Assigned(Columns[i].BoldProperties) and Columns[i].BoldProperties.SupportsMultiEnsure then
        if assigned(CellFollowers[i, TopRow]) then
          CellFollowers[i, TopRow].EnsureMulti;
  end;
end;

end.
