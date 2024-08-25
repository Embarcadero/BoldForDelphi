
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldGridCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  // VCL
  {$IFDEF DELPHI6_OR_LATER}
  Types,
  {$ELSE}
  Windows,
  {$ENDIF}
  Classes,
  Controls,
  Graphics,
  Grids,
  Menus,

  // Bold
  {$IFNDEF BOLDCOMCLIENT}
  BoldComObjectSpace_TLB,
  {$ENDIF}
  BoldAbstractListHandleCom,
  BoldComClient,
  BoldComObjectSpace_TLB,
  BoldComponentValidatorCom,
  BoldControllerListControlPackCom,
  BoldControlPackCom,
  BoldControlsDefs,
  BoldListHandleFollowerCom,
  BoldListListControlPackCom,
  BoldStringControlPackCom;

type
  TBoldFirstColumnRendererCom = class;
  TBoldCustomGridCom = class;
  TBoldGridCom = class;
  TBoldGridColumnsCom = class;
  TBoldGridColumnCom = class;
  TBoldColumnTitleCom = class;
  TBoldColumnClassCom = class of TBoldGridColumnCom;
  TBoldInplaceEditCom = class;

  {$IFNDEF BOLDCOMCLIENT}
  TBoldLookupChange = procedure(DestElement: IBoldElement; NewValue: IBoldElement) of object;
  {$ENDIF}

  { TBoldFirstColumnRendererCom }
  TBoldFirstColumnRendererCom = class(TBoldAsStringRendererCom)
  public
    procedure DrawOnCanvas(Follower: TBoldFollowerCom; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); override;
  end;

  {$IFNDEF BOLDCOMCLIENT}
  {  TBoldGridCheckBoxPainterRenderer  }
  TBoldGridCheckBoxPainterRenderer = class(TBoldAsStringRendererCom)
  private
    fColumn: TBoldGridColumnCom;
    procedure GetCurrentElement(Follower: TBoldFollowerCom; ie: TBoldIndirectElement);
    function GetCurrentCheckBoxState(Follower: TBoldFollowerCom): TCheckBoxState;
    function GetCheckBoxRect(rect: TRect; Alignment: TAlignment): TRect;
    procedure ToggleValue(GridCoord: TGridCoord; Grid: TBoldCustomGridCom);
  public
    procedure DrawOnCanvas(Follower: TBoldFollowerCom; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); override;
    procedure CheckBoxClick(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer; Grid: TBoldCustomGridCom; Alignment: TAlignment);
    procedure KeyPress(var Key: Word; Shift: TShiftState; Grid: TBoldCustomGridCom);
  end;
  {$ENDIF}

  TBoldConstraintRendererCom = class(TBoldAsStringRendererCom)
    {Override draw in Controllers since we need access to follower}
    procedure DefaultMakeUptodateAndSetMayModifyAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldStringFollowerControllerCom; Subscriber: TBoldComClientSubscriber); override;
    procedure DrawOnCanvas(Follower: TBoldFollowerCom; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); override;
  end;


  { TBoldGridColumnsCom }
  TBoldGridColumnsCom = class(TCollection)
  private
    fGrid: TBoldCustomGridCom;
    function GetColumn(index: Integer): TBoldGridColumnCom;
    procedure MoveColumn(FromIndex, ToIndex: Longint);
    procedure SetColumn(index: Integer; Value: TBoldGridColumnCom);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(theGrid: TBoldCustomGridCom; ColumnClass: TBoldColumnClassCom);
    function Add: TBoldGridColumnCom;
    procedure Update(Item: TCollectionItem); override;
    property Grid: TBoldCustomGridCom read fGrid;
    property Items[index: Integer]: TBoldGridColumnCom read GetColumn write SetColumn; default;
  end;

  { TBoldColumnTitleCom }
  TBoldColumnTitleCom = class(TPersistent)
  private
    fAlignment: TAlignment;
    fCaption: TCaption;
    FColor: TColor;
    fColumn: TBoldGridColumnCom;
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
    constructor Create(Column: TBoldGridColumnCom);
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

  { TBoldGridColumnCom }
  TBoldGridColumnCom = class(TCollectionItem)
  private
    {$IFNDEF BOLDCOMCLIENT}
    fCheckBoxPainterRenderer: TBoldGridCheckBoxPainterRenderer;
    fAllowCheckBox: Boolean;
    fLookupHandle: TBoldAbstractListHandleCom;
    fNilElementMode: TBoldNilElementMode;
    fLookUpProperties: TBoldStringFollowerControllerCom;
    fOnLookupChange: TBoldLookupChange;
    fLookupHandleSubscriber: TBoldComClientPassthroughSubscriber;
    {$ENDIF}
    fAlignment: TAlignment;
    fAssignedValues: TBoldColumnValues;
    fBoldProperties: TBoldStringFollowerControllerCom;
    fColor: TColor;
    fCWAdjust: TBoldCWAdjustSet;
    fDefaultPopupMenu: Boolean;
    fFont: TFont;
    fGrid: TBoldCustomGridCom;
    fReadOnly: Boolean;
    fTitle: TBoldColumnTitleCom;
    fUserDraw: Boolean;
    {$IFNDEF BOLDCOMCLIENT}
    procedure SetLookupHandle(const Value: TBoldAbstractListHandleCom);
    procedure SetLookUpProperties(const Value: TBoldStringFollowerControllerCom);
    function GetLookupContext: IBoldElementTypeInfo;
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    {$ENDIF}
    procedure _FontChanged(Sender: TObject);
    function GetWidth: Integer;
    procedure SetBoldProperties(Value: TBoldStringFollowerControllerCom);
    procedure SetColor(C: TColor);
    procedure SetCWAdjust(V: TBoldCWAdjustSet);
    procedure SetFont(Value: TFont);
    procedure SetTitle(V: TBoldColumnTitleCom);
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
    function GetCurrentCheckBoxState(Follower: TBoldFollowerCom): TCheckBoxState; virtual;
    procedure SetCurrentCheckBoxState(Follower: TBoldFollowerCom; NewValue: TCheckBoxState); virtual;
    {$ENDIF}
  public
    constructor Create(theCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultFont: TFont;
    procedure RestoreDefaults; virtual;
    property AssignedValues: TBoldColumnValues read fAssignedValues;
    property Grid: TBoldCustomGridCom read fGrid;
  published
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property BoldProperties: TBoldStringFollowerControllerCom read fBoldProperties write SetBoldProperties;
    property Color: TColor read FColor write SetColor default clWindow;
    property ColReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property CWAdjust: TBoldCWAdjustSet read fCWAdjust write SetCWAdjust default[];
    property DefaultTitlePopupMenu: Boolean read fDefaultPopupMenu write fDefaultPopupMenu default True;
    property Font: TFont read GetFont write SetFont;
    property Title: TBoldColumnTitleCom read FTitle write SetTitle;
    property UserDraw: Boolean read fUserDraw write fUserDraw default False;
    property Width: Integer read GetWidth write SetWidth stored False;
    {$IFNDEF BOLDCOMCLIENT}
    property AllowCheckBox: Boolean read fAllowCheckBox write fAllowCheckBox default false;
    property LookUpProperties: TBoldStringFollowerControllerCom read fLookUpProperties write SetLookUpProperties;
    property NilElementMode: TBoldNilElementMode read fNilElementMode write fNilElementMode default neNone;
    property LookupHandle: TBoldAbstractListHandleCom read fLookupHandle write SetLookupHandle;
    property OnLookupChange: TBoldLookupChange read fOnLookupChange write fOnLookUpChange;
    {$ENDIF}
  end;

  { TBoldCustomGridCom }
  TBoldCustomGridCom = class(TCustomGrid, IBoldValidateableComponentCom)
  private
    {Bold stuff}
    fFirstColumnRenderer: TBoldFirstColumnRendererCom;
    fAddNewAtEnd: Boolean;
    fAlwaysShowSelected: Boolean;
    fAnchor: Integer;
    fBoldColumnsProperties: TBoldControllerListCom;
    fBoldProperties: TBoldListAsFollowerListControllerCom;
    FColumns: TBoldGridColumnsCom;
    fCurrentListElementType: IBoldElementTypeInfo;
    fEnableColAdjust: boolean;
    fHandleFollower: TBoldListHandleFollowerCom;
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
    fFixedColumn: TBoldGridColumnCom;
    fAutoSelectNewRows: Boolean;
    fSubFollowerCountBeforeMakeUpToDate: integer;
    fLastInsertedRowIndex: integer;
    fEditedElementBeforeMakeUpToDate: IBoldElement;
    fBoldDragAnywhere: Boolean;
    fIsDragging: Boolean;
    fIsMultiSelecting: Boolean;
    fLastMouseDownShiftState: TShiftState;
    procedure EnsureOneFixedCol;
    function GetBoldHandle: TBoldAbstractListHandleCom;
    function GetBoldList: IBoldList;
    function GetCurrentBoldElement: IBoldElement;
    function GetFollower: TBoldFollowerCom;
    function GetOptions: TGridOptions;
    function GetSelected(DataRow: integer): Boolean;
    procedure SetBoldHandle(value: TBoldAbstractListHandleCom);
    procedure SetColumns(Value: TBoldGridColumnsCom);
    procedure SetController(Value: TBoldListAsFollowerListControllerCom);
    procedure SetOptions(val: TGridOptions);
    procedure SetSelection(aRow: Integer; Shift: TShiftState; ForceClearOfOtherRows: Boolean; IgnoreToggles: Boolean);
    procedure TypeMayHaveChanged;
    function CellFont(Column: TBoldGridColumnCom): TFont;
    function GetString(GridCol, DataRow: Integer): string;
    function HighlightCell(AState: TGridDrawState; aRow: integer): Boolean;
    procedure _AfterMakeCellUptoDate(Follower: TBoldFollowerCom);
    procedure _DeleteRow(index: Integer; owningFollower: TBoldFollowerCom);
    procedure _InsertRow(Follower: TBoldFollowerCom);
    procedure AdjustCol(Col: Integer);
    function DefaultTitlePopup(Col: Integer): TPopupMenu;
    procedure DefaultTitlePopupOnClick(Sender: TObject);
    function GetCellFollower(ListCol, DataRow: Integer): TBoldFollowerCom;
    function GetCurrentCellFollower: TBoldFollowerCom;
    function GetMultiSelect: Boolean;
    function GetRowFollower(DataRow: Integer): TBoldFollowerCom;
    procedure InvalidateFromRow(DisplayDataRow: Longint);
    procedure SetCurrentRow(DataRow: Integer);
    procedure SetMultiSelect(V: Boolean);
    procedure TitleMenuPopup(GridCoord: TGridCoord; X, Y: Integer);
    procedure SetTitleFont(const Value: TFont);
    function GetBoldHandleIndexLock: Boolean;
    procedure SetBoldHandleIndexLock(Value: Boolean);
    {$IFNDEF BOLDCOMCLIENT}
    function ValidateComponent(ComponentValidator: TBoldComponentValidatorCom; NamePrefix: String): Boolean;
    function ColumnIsCheckBox(col: integer): Boolean;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    {$ENDIF}
    function GetMutableList: IBoldList;
    function GetShowTitleRow: Boolean;
    procedure SetShowTitleRow(const Value: Boolean);
    function GetTitleRow: integer;
    procedure GetActiveRange(var FirstActive, LastActive: integer);
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect;
    procedure EnsureRowActive(DataRow: integer);
    procedure DisplayAvailableFollowers;
    function GetCellText(col, row: integer): string;
    procedure _FontChanged(Sender: TObject);
  protected
    { Protected declarations }
    procedure _AfterMakeListUptoDate(Follower: TBoldFollowerCom); virtual;
    procedure _BeforeMakeListUpToDate(Follower: TBoldFollowerCom); virtual;
    procedure AutoAdjustCol(Col: Integer);
    function CanEditAcceptKey(KEY: Char): Boolean; override;
    function CanEditModify: Boolean; override;
    function CanEditShow: Boolean; override;
    function CanEditShowForCustomEditors: Boolean;
    procedure Click; override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure ColWidthsChanged; override;
    function CreateColumns: TBoldGridColumnsCom; dynamic;
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
    function ColumnClass: TBoldColumnClassCom; virtual;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    function GetEditor: TBoldInplaceEditCom;
    procedure EnsureActiveCellFollowerExpressions;
    {$IFNDEF BOLDCOMCLIENT}
    procedure LookUpChange(sender: Tobject; DestElement: IBoldElement; EditColumn: TBoldGridColumnCom);
    {$ENDIF}
    property BoldDragAnywhere: Boolean read fBoldDragAnywhere write fBoldDragAnywhere default False;
    property AddNewAtEnd: Boolean read fAddNewAtEnd write fAddNewAtEnd;
    property AlwaysShowSelected: Boolean read fAlwaysShowSelected write fAlwaysShowSelected default True;
    property BoldHandleIndexLock: Boolean read GetBoldHandleIndexLock write SetBoldHandleIndexLock default true;
    property BoldAutoColumns: Boolean read fBoldAutoColumns write fBoldAutoColumns;
    property BoldShowConstraints: Boolean read fBoldShowConstraints write fBoldShowConstraints;
    property BoldHandle: TBoldAbstractListHandleCom read GetBoldHandle write SetBoldHandle;
    property BoldList: IBoldList read GetBoldList;
    property BoldProperties: TBoldListAsFollowerListControllerCom read fBoldProperties write SetController;
    property CellFollowers[GridCol, DataRow: Integer]: TBoldFollowerCom read GetCellFollower;
    property Columns: TBoldGridColumnsCom read FColumns write SetColumns;
    property CurrentBoldElement: IBoldElement read GetCurrentBoldElement;
    property CurrentCellFollower: TBoldFollowerCom read GetCurrentCellFollower;
    property EnableColAdjust: boolean read fEnableColAdjust write fEnableColAdjust;
    property Follower: TBoldFollowerCom read GetFollower;
    property OnDrawCell: TBoldDrawCellEvent read fOndrawCell write fOndrawCell;
    property OnTopLeftChanged: TNotifyEvent read fOnTopLeftChanged write fOnTopLeftChanged;
    property Options: TGridOptions read GetOptions write SetOptions default [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goColMoving, goColSizing];
    property RowFollowers[DataRow: Integer]: TBoldFollowerCom read GetRowFollower;
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
    function AddColumn: TBoldGridColumnCom;
    procedure AdjustActiveRange;
    function CreateEditor: TInplaceEdit; override;
    procedure DeleteColumn(ACol: Integer); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure DrawCell(ACol, aRow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure EditColumns;
    function GetHandleStaticType: IBoldElementTypeInfo;
    function GetHandleListElementType: IBoldElementTypeInfo;
    function GetEditText(GridCol, GridRow: Longint): string; override;
    procedure MoveColumn(FromIndex, ToIndex: Longint);
    procedure SetEditText(GridCol, GridRow: Longint; const Value: string); override;
    procedure ReallyInvalidateCol(Column: integer);
    procedure DisplayAllCells;
    function AsClipBoardText: String;
    procedure ActivateAllCells;
    property ColCount;
    property CellText[col, row: integer]: string read GetCellText;
    property MutableList: IBoldList read GetMutableList;
  end;

  { TBoldGridCom }
  TBoldGridCom = class(TBoldCustomGridCom)
  public
    {$IFNDEF T2H}
    {Properties from TBoldCustomGridCom}
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

  { TBoldInplaceEditCom }
  TBoldInplaceEditCom = class(TInplaceEdit)
  private
    {$IFNDEF BOLDCOMCLIENT}
    fCombo: TCombobox;
    fInitialComboIndex: integer;
    fDestElement: IBoldElement;
    fDestElementSubscriber: TBoldComClientPassthroughSubscriber;
    fComboAborting: Boolean;
    {$ENDIF}
    fEditColumn: TBoldGridColumnCom;
    function GetGrid: TBoldCustomGridCom;
    {$IFNDEF BOLDCOMCLIENT}
    procedure HideCombo;
    function GetCombo: TComboBox;
    function GetDestElement(CellFollower: TBoldFollowerCom; Column: TBoldGridColumnCom): IBoldElement;
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
    property Grid: TBoldCustomGridCom read GetGrid;
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
  Messages,
  StdCtrls,
  SysUtils,
  Forms,
  {$IFNDEF BOLDCOMCLIENT}
  {!! DO NOT REMOVE !! BoldAttributes ,}
  {!! DO NOT REMOVE !! BoldSystemRT ,}
  BoldAFP,
  BoldGUI,
  {$ENDIF}
  BoldCommonBitmaps,
  BoldControlPackDefs,
  BoldDefs,
  BoldEnvironment,
  BoldListControlPackCom,
  BoldMath,
  TypInfo;

const
  ColumnTitleValues = [cvTitleColor..cvTitleFont];
  breDestElementDestroyed = 100;
  breColumnLookUpHandleDestroyed = 101;

var
  TheDefaultTitlePopup: TPopupMenu;

{ TBoldInplaceEditCom }

{$IFNDEF BOLDCOMCLIENT}

type
  TBoldInplaceCombo = class(TCombobox)
  private
    fInplaceEdit: TBoldInplaceEditCom;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor createWithInplaceEditor(InplaceEdit: TBoldInplaceEditCom);
  end;

{ TBoldInplaceCombo }

constructor TBoldInplaceCombo.createWithInplaceEditor(InplaceEdit: TBoldInplaceEditCom);
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


procedure TBoldInplaceEditCom.ChangedPos(var Message: TWMWindowPosChanged);
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

procedure TBoldInplaceEditCom.BoundsChanged;
var
  R: TRect;
begin
  Assert(Assigned(Grid));
  R := Rect(2, 2, TBoldCustomGridCom(Grid).Columns[TBoldCustomGridCom(Grid).Col].Width - 2, Height);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TBoldInplaceEditCom.KeyPress(var Key: Char);
var
  Grid: TBoldCustomGridCom;
begin
  inherited KeyPress(Key);
  Grid := TBoldCustomGridCom(Owner);
  Grid.SetSelection(grid.DataRow(grid.Row), [], true, false);
  if (Key in [#32..#255]) and
    not Grid.Columns[Grid.Col].BoldProperties.ValidateCharacter(AnsiChar(Key), Grid.CurrentCellFollower) then
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

{ TBoldInplaceEditCom }

constructor TBoldInplaceEditCom.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF BOLDCOMCLIENT}
  fDestElementSubscriber := TBoldComClientPassthroughSubscriber.Create(_receive);
  {$ENDIF}
end;

destructor TBoldInplaceEditCom.Destroy;
begin
  {$IFNDEF BOLDCOMCLIENT}
  FreeAndNil(fCombo);
  FreeAndNil(fDestElementSubscriber);
  fDestElement := nil;
  {$ENDIF}
  fEditColumn := nil;
  inherited;
end;

function TBoldInplaceEditCom.GetGrid: TBoldCustomGridCom;
begin
  result := (inherited Grid) as TBoldCustomGridCom;
end;

type
  TExposedBoldAsStringRenderer = class(TBoldAsStringRendererCom);
  TExposedFollowerController = class(TBoldFollowerControllerCom);

{ TBoldColumnTitleCom }
constructor TBoldColumnTitleCom.Create(Column: TBoldGridColumnCom);
begin
  inherited Create;
  fColumn := Column;
  fFont := TFont.Create;
  fFont.Assign(DefaultFont);
  fFont.OnChange := _FontChanged;
end;

destructor TBoldColumnTitleCom.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TBoldColumnTitleCom.Assign(Source: TPersistent);
var
  S: TBoldColumnTitleCom;
  AssignedValues: TBoldColumnValues;
begin
  if Source is TBoldColumnTitleCom then
  begin
    S := Source as TBoldColumnTitleCom;
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

function TBoldGridColumnCom.AreFontsEqual(Font1, Font2: TFont): Boolean;
begin
  Result := (Font1.Charset = Font2.Charset) and
            (Font1.Color = Font2.Color) and
            (Font1.Height = Font2.Height) and
            (Font1.Name = Font2.Name) and
            (Font1.Pitch = Font2.Pitch) and
            (Font1.Size = Font2.Size) and
            (Font1.Style = Font2.Style);
end;

procedure TBoldColumnTitleCom.Changed;
begin
  fColumn.fGrid.InvalidateCell(fColumn.index, 0);
end;

function TBoldColumnTitleCom.DefaultAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TBoldColumnTitleCom.DefaultColor: TColor;
var
  Grid: TBoldCustomGridCom;
begin
  Grid := fColumn.fGrid;
  if Assigned(Grid) then
    Result := Grid.FixedColor
  else
    Result := clBtnFace;
end;

function TBoldColumnTitleCom.DefaultFont: TFont;
var
  Grid: TBoldCustomGridCom;
begin
  Grid := fColumn.fGrid;
  if Assigned(Grid) then
    Result := Grid.TitleFont
  else
    Result := fColumn.Font;
end;

function TBoldColumnTitleCom.DefaultCaption: string;
begin
  if Assigned(fColumn.BoldProperties) then
    Result := fColumn.BoldProperties.Expression
  else
    Result := '';
end;

procedure TBoldColumnTitleCom._FontChanged(Sender: TObject);
begin
  if fColumn.AreFontsEqual(TFont(Sender), DefaultFont) then
    Exclude(fColumn.fAssignedValues, cvTitleFont)
  else
    Include(fColumn.fAssignedValues, cvTitleFont);
  Changed;
end;

function TBoldColumnTitleCom.GetAlignment: TAlignment;
begin
  if cvTitleAlignment in fColumn.fAssignedValues then
    Result := FAlignment
  else
    Result := DefaultAlignment;
end;

function TBoldColumnTitleCom.GetColor: TColor;
begin
  if cvTitleColor in fColumn.fAssignedValues then
    Result := FColor
  else
    Result := DefaultColor;
end;

function TBoldColumnTitleCom.GetCaption: string;
begin
  if cvTitleCaption in fColumn.fAssignedValues then
    Result := fCaption
  else
    Result := DefaultCaption;
end;

function TBoldColumnTitleCom.GetFont: TFont;
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

function TBoldColumnTitleCom.IsAlignmentStored: Boolean;
begin
  Result := (cvTitleAlignment in fColumn.fAssignedValues) and
            (FAlignment <> DefaultAlignment);
end;

function TBoldColumnTitleCom.IsColorStored: Boolean;
begin
  Result := (cvTitleColor in fColumn.fAssignedValues) and
            (FColor <> DefaultColor);
end;

function TBoldColumnTitleCom.IsFontStored: Boolean;
begin
  Result := (cvTitleFont in fColumn.fAssignedValues);
end;

function TBoldColumnTitleCom.IsCaptionStored: Boolean;
begin
  Result := (cvTitleCaption in fColumn.fAssignedValues) and
            (fCaption <> DefaultCaption);
end;

procedure TBoldColumnTitleCom.RefreshDefaultFont;
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

procedure TBoldColumnTitleCom.RestoreDefaults;
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

procedure TBoldColumnTitleCom.SetAlignment(Value: TAlignment);
begin
  if Value = DefaultAlignment then
    Exclude(fColumn.fAssignedValues, cvTitleAlignment)
  else
    Include(fColumn.fAssignedValues, cvTitleAlignment);
  FAlignment := Value;
  Changed;
end;

procedure TBoldColumnTitleCom.SetColor(Value: TColor);
begin
  if Value = DefaultColor then
    Exclude(fColumn.fAssignedValues, cvTitleColor)
  else
    Include(fColumn.fAssignedValues, cvTitleColor);
  FColor := Value;
  Changed;
end;

procedure TBoldColumnTitleCom.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TBoldColumnTitleCom.SetCaption(const Value: string);
begin
  if Value = DefaultCaption then
    Exclude(fColumn.fAssignedValues, cvTitleCaption)
  else
    Include(fColumn.fAssignedValues, cvTitleCaption);
  fCaption := Value;
  Changed;
end;

procedure TBoldColumnTitleCom.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Assigned(Value) then
    Value.FreeNotification(fColumn.fGrid);
end;

{ TBoldGridColumnCom }
constructor TBoldGridColumnCom.Create(theCollection: TCollection);
begin
  inherited Create(theCollection);
  if not (theCollection is TBoldGridColumnsCom) then
    raise EBold.CreateFmt('%s.Create: Cannot create TBoldGridColumnCom outside a TBoldGridColumnsCom', [ClassName]);
  fGrid := (theCollection as TBoldGridColumnsCom).Grid;
  FBoldProperties := TBoldStringFollowerControllerCom.Create(fGrid);
  fColor := fGrid.Color;
  FBoldProperties.AfterMakeUptoDate := fGrid._AfterMakeCellUptoDate;
  fBoldProperties.OnGetContextType := fGrid.GetHandleStaticType;
  fGrid.fBoldColumnsProperties.Add(FBoldProperties);
  FTitle := TBoldColumnTitleCom.Create(self);
  FFont := TFont.Create;
  FFont.OnChange := _FontChanged;
  DefaultTitlePopupMenu := True;
  {$IFNDEF BOLDCOMCLIENT}
  fLookUpProperties := TBoldStringFollowerControllerCom.Create(grid);
  fLookUpProperties.OnGetContextType := GetLookupContext;
  fOnLookupChange := nil;
  fNilElementMode := neNone;
  fLookupHandleSubscriber := TBoldComClientPassthroughSubscriber.Create(_Receive);
  {$ENDIF}
end;

destructor TBoldGridColumnCom.Destroy;
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



procedure TBoldGridColumnCom.Assign(Source: TPersistent);
var
  SourceCol: TBoldGridColumnCom;
begin
  if Source is TBoldGridColumnCom then
  begin
    SourceCol := Source as TBoldGridColumnCom;
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

procedure TBoldGridColumnCom.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  Include(FAssignedValues, cvFont);
  Changed(False);
end;

procedure TBoldGridColumnCom.SetBoldProperties(Value: TBoldStringFollowerControllerCom);
begin
  if Assigned(Value) then
    fBoldProperties.Assign(Value);
end;

procedure TBoldGridColumnCom.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := cvFont in FAssignedValues;
  FTitle.RestoreDefaults;
  FAssignedValues := [];
  RefreshDefaultFont;

  Changed(FontAssigned);
end;

procedure TBoldGridColumnCom.RefreshDefaultFont;
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

function TBoldGridColumnCom.DefaultFont: TFont;
begin
  if Assigned(Grid) then
    Result := Grid.Font
  else
    Result := FFont;
end;

function TBoldGridColumnCom.GetDisplayName: string;
begin
  Result := FTitle.Caption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TBoldGridColumnCom.SetTitle(V: TBoldColumnTitleCom);
begin
  if V <> FTitle then
  begin
    FTitle.Assign(V);
    fGrid.InvalidateCell(index, 0);
  end;
end;

procedure TBoldGridColumnCom.SetColor(C: TColor);
begin
  if Color <> C then
  begin
    FColor := C;
    Changed(True);
  end;
end;

procedure TBoldGridColumnCom._FontChanged(Sender: TObject);
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

procedure TBoldGridColumnCom.SetWidth(Value: Integer);
begin
  fGrid.ColWidths[index] := Value;
end;

function TBoldGridColumnCom.GetWidth: Integer;
begin
  Result := fGrid.ColWidths[index];
end;

procedure TBoldGridColumnCom.SetCWAdjust(V: TBoldCWAdjustSet);
begin
  if V <> fCWAdjust then
  begin
    fCWAdjust := V;
    fGrid.AdjustCol(index);
  end;
end;

{ TBoldGridColumnsCom }
function TBoldGridColumnsCom.GetOwner: TPersistent;
begin
  Result := fGrid;
end;

constructor TBoldGridColumnsCom.Create(theGrid: TBoldCustomGridCom; ColumnClass: TBoldColumnClassCom);
begin
  inherited Create(ColumnClass);
  fGrid := theGrid;
end;

procedure TBoldGridColumnsCom.MoveColumn(FromIndex, ToIndex: Longint);
begin
  inherited;
  Items[FromIndex].index := ToIndex;
end;

function TBoldGridColumnsCom.Add: TBoldGridColumnCom;
begin
  BeginUpdate;
  Result := TBoldGridColumnCom(inherited Add);
  EndUpdate;
end;

function TBoldGridColumnsCom.GetColumn(index: Integer): TBoldGridColumnCom;
begin
  Result := TBoldGridColumnCom(inherited Items[index]);
end;

procedure TBoldGridColumnsCom.SetColumn(index: Integer; Value: TBoldGridColumnCom);
begin
  Items[index].Assign(Value);
end;

procedure TBoldGridColumnsCom.Update(Item: TCollectionItem);
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

{ TBoldCustomGridCom }
constructor TBoldCustomGridCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fFirstColumnRenderer := TBoldFirstColumnRendererCom.Create(Self);
  fBoldColumnsProperties := TBoldControllerListCom.Create(Self);
  fBoldProperties := TBoldListAsFollowerListControllerCom.Create(self, fBoldColumnsProperties);
  fBoldProperties.OnAfterInsertItem := _InsertRow;
  fBoldProperties.OnAfterDeleteItem := _DeleteRow;
  fBoldProperties.AfterMakeUptoDate := _AfterMakeListUptoDate;
  fBoldProperties.BeforeMakeUptoDate := _BeforeMakeListUptoDate;
  fBoldProperties.OnGetContextType := GetHandleStaticType;
  fHandleFollower     := TBoldListHandleFollowerCom.Create(Owner, fBoldProperties);
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

destructor TBoldCustomGridCom.Destroy;
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

procedure TBoldCustomGridCom.TypeMayHaveChanged;
var
  NewListElementType: IBoldElementTypeInfo;
  {$IFNDEF BOLDCOMCLIENT}
  i: integer;
  {$ENDIF}
begin
  if BoldEffectiveEnvironment.RunningInIDE and (not Assigned(BoldHandle.List) or (BoldHandle.List.Count = 0)) then
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

procedure TBoldCustomGridCom.Loaded;
begin
  inherited;

  EnsureOneFixedCol;

  if not (csDesigning in ComponentState) then
    TypeMayHaveChanged;

  EnsureConstraintColumn;
end;

procedure TBoldCustomGridCom.DeleteAllColumns;
begin
  while Columns.Count > 1 do
    Columns[ColCount - 1].Free;
  if columns.count = 0 then
    AddColumn;
  EnsureOneFixedCol;
end;

procedure TBoldCustomGridCom.CreateDefaultColumns;
{$IFNDEF BOLDCOMCLIENT}
var
  i: integer;
  ListElementType: IBoldElementTypeInfo;
  ClasstypeInfo: IBoldClassTypeInfo;
  UsedFirstCol: Boolean;
  column: TBoldGridColumnCom;

  function GetEmptyCol: TBoldGridColumnCom;
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
    if (ListElementType is IBoldClassTypeInfo) then
    begin
      ClassTypeInfo := ListElementType as IBoldClassTypeInfo;
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
    else if (ListElementType is IBoldAttributeTypeInfo) then
    begin
      GetEmptyCol.Title.Caption := IBoldAttributeTypeInfo(ListElementType).ModelName;
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

procedure TBoldCustomGridCom.EditColumns;
begin
(*  with TfrmRTColEditorCom.Create(nil) do
  try
    Execute(Self);
  finally
    Free;
  end;
  *)
end;

procedure TBoldCustomGridCom.DefaultColumns;
begin
  CreateDefaultColumns;
end;

function TBoldCustomGridCom.CreateColumns: TBoldGridColumnsCom;
begin
  Result := TBoldGridColumnsCom.Create(self, ColumnClass);
end;

procedure TBoldCustomGridCom.SetColumns(Value: TBoldGridColumnsCom);
begin
  Columns.Assign(Value);
end;

procedure TBoldCustomGridCom.ColumnMoved(FromIndex, ToIndex: Longint);
var
  Col: Integer;
begin
  Columns.MoveColumn(FromIndex, ToIndex);
  inherited ColumnMoved(FromIndex, ToIndex);
  for Col := MinIntValue([FromIndex, ToIndex]) to MaxIntValue([FromIndex, ToIndex]) do
    Columns.Update(Columns[Col]);
end;

procedure TBoldCustomGridCom.SetMultiSelect(V: Boolean);
begin
  if V then
    Options := Options + [goRangeSelect]
  else
    Options := Options - [goRangeSelect];
end;

function TBoldCustomGridCom.GetMultiSelect: Boolean;
begin
  Result := goRangeSelect in Options;
end;

procedure TBoldCustomGridCom.EnsureOneFixedCol;
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

function TBoldCustomGridCom.AddColumn: TBoldGridColumnCom;
begin
  Columns.BeginUpdate;
  try
    result := Columns.Add;
  finally
    Columns.EndUpdate;
  end;
end;

procedure TBoldCustomGridCom.DeleteColumn(ACol: Integer);
begin
  if ColCount > 2 then
    inherited DeleteColumn(ACol);
end;

procedure TBoldCustomGridCom.MoveColumn(FromIndex, ToIndex: Longint);
begin
  inherited MoveColumn(FromIndex, ToIndex);
end;

function TBoldCustomGridCom.CreateEditor: TInplaceEdit;
begin
  Result := TBoldInplaceEditCom.Create(self);
  TBoldInplaceEditCom(Result).Font.Assign(CellFont(Columns[Col]));
  result.width := Columns[col].Width;
end;

procedure TBoldCustomGridCom.EditStop;
var
  CellFollower: TBoldFollowerCom;
begin
  CellFollower := CurrentCellFollower;

  if assigned(CellFollower) and
    (CellFollower.Controller.ApplyPolicy = bapExit) then
    CellFollower.Apply;
end;

function TBoldCustomGridCom.GetEditText(GridCol, GridRow: Longint): string;
var
  Editor: TInplaceEdit;
begin
  Editor := InplaceEditor;
  if (Editor is TBoldInplaceEditCom) then
  begin
    TBoldInplaceEditCom(Editor).Font.Assign(CellFont(Columns[Col]));
    Editor.Width := Columns[Col].Width;
  end;
  Result := GetString(GridCol, DataRow(GridRow));
end;

procedure TBoldCustomGridCom.SetEditText(GridCol, GridRow: Longint; const Value: string);
begin
  if not (csDesigning in ComponentState) and Editormode and assigned(CurrentCellFollower) then
    TBoldStringFollowerControllerCom(CurrentCellFollower.Controller).MayHaveChanged(Value, CurrentCellFollower)
end;

procedure TBoldCustomGridCom.Click;
begin
  fHandleFollower.SetFollowerIndex(DataRow(row));
  inherited;
end;

procedure TBoldCustomGridCom.DblClick;
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

procedure TBoldCustomGridCom.KeyPress(var KEY: Char);
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

procedure TBoldCustomGridCom.SetBoldHandle(Value: TBoldAbstractListHandleCom);
begin
  fHandleFollower.BoldHandle := value;
end;

function TBoldCustomGridCom.DataRow(GridRow: Integer): Integer;
begin
  if HasGhostRow then
    Result := -1
  else
    Result := GridRow - FixedRows;
end;

function TBoldCustomGridCom.GetCurrentBoldElement: IBoldElement;
begin
  if Assigned(CurrentCellFollower) then
    Result := CurrentCellFollower.Element
  else
    Result := nil;
end;

function TBoldCustomGridCom.GetBoldList: IBoldList;
begin
  if Assigned(BoldHandle) then
    Result := BoldHandle.List
  else
    Result := nil;
end;

procedure TBoldCustomGridCom.SetController(Value: TBoldListAsFollowerListControllerCom);
begin
  fBoldProperties.Assign(Value);
end;

function TBoldCustomGridCom.GetOptions: TGridOptions;
begin
  Result := inherited Options;
  if fRangeSelect then
    Include(Result, goRangeSelect);
end;

procedure TBoldCustomGridCom.SetOptions(val: TGridOptions);
begin
  fRangeSelect := goRangeSelect in val;
  inherited Options := val - [goRangeSelect];
end;

procedure TBoldCustomGridCom.SetSelection(aRow: Integer; Shift: TShiftState; ForceClearOfOtherRows: Boolean; IgnoreToggles: Boolean);
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

procedure TBoldCustomGridCom.DragDrop(Source: TObject; X, Y: Integer);
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

procedure TBoldCustomGridCom.DoStartDrag(var DragObject: TDragObject);
begin
  BoldProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TBoldCustomGridCom.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TBoldCustomGridCom.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Assigned(OnDragOver)
    or (BoldProperties.DropMode = bdpNone)
    or ((Source = self) and (not BoldProperties.InternalDrag)) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldProperties.DragOver(Follower, MutableList, MouseCoord(X, Y).Y);
end;

procedure TBoldCustomGridCom.DefaultTitlePopupOnClick(Sender: TObject);
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

function TBoldCustomGridCom.DefaultTitlePopup(Col: Integer): TPopupMenu;
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

procedure TBoldCustomGridCom.TitleMenuPopup(GridCoord: TGridCoord; X, Y: Integer);
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

procedure TBoldCustomGridCom.AutoAdjustCol(Col: Integer);
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

procedure TBoldCustomGridCom.MouseUp(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TBoldCustomGridCom.MouseDown(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TBoldCustomGridCom.KeyDown(var KEY: Word; Shift: TShiftState);
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

procedure TBoldCustomGridCom.KeyUp(var KEY: Word; Shift: TShiftState);
begin
  if KEY in [33..40] then
  begin


    Exclude(Shift, ssCtrl);
    SetSelection(DataRow(Row), Shift, true, true);
  end;
  inherited;
end;

function TBoldCustomGridCom.GetString(GridCol, DataRow: Integer): string;
begin
  if not assigned(BoldHandle) or (DataRow >= BoldHandle.Count) then
    result := ''
  else
  begin
    EnsureRowActive(DataRow);
    if Assigned(CellFollowers[GridCol, DataRow]) and Assigned(CellFollowers[GridCol, DataRow].Controller) then
      Result := TBoldStringFollowerControllerCom(CellFollowers[GridCol, DataRow].Controller).GetCurrentAsString(CellFollowers[GridCol, DataRow]);
  end;
end;

function TBoldCustomGridCom.HighlightCell(AState: TGridDrawState; aRow: integer): Boolean;
var
  RowSelected: Boolean;
begin
  RowSelected := Follower.SubFollowers[DataRow(aRow)].Selected;

  if fIsMultiSelecting then
    Result := Follower.SubFollowers[DataRow(aRow)].Selected
  else
    Result := (gdSelected in AState) and RowSelected and (AlwaysShowSelected or Focused);
end;

function TBoldCustomGridCom.CellFont(Column: TBoldGridColumnCom): TFont;
begin
  if Assigned(Column.Font) then
    Result := Column.Font
  else
    Result := self.Font;
end;

procedure TBoldCustomGridCom.AdjustCol(Col: Integer);
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

procedure TBoldCustomGridCom.ColWidthsChanged;
begin
  inherited;


end;

function TBoldCustomGridCom.CanEditAcceptKey(KEY: Char): Boolean;
begin
  Result := Assigned(CurrentCellFollower) and
            TBoldStringFollowerControllerCom(CurrentCellFollower.Controller).ValidateCharacter(AnsiChar(KEY), CurrentCellFollower);
end;

function TBoldCustomGridCom.CanEditModify: Boolean;
begin
  if not Assigned(CurrentCellFollower) then
    Result := False
  else
    Result := CurrentCellFollower.Controller.MayModify(CurrentCellFollower) and
    not Columns[Col].ColReadOnly;
end;

function TBoldCustomGridCom.CanEditShow: Boolean;
begin
  Result := (inherited CanEditShow) and Assigned(CurrentCellFollower) and
            not Columns[Col].ColReadOnly;
  result := result and
    (CurrentCellFollower.RendererData.MayModify
    {$IFNDEF BOLDCOMCLIENT} or assigned(Columns[Col].LookupHandle)
    {$ENDIF});

  {$IFNDEF BOLDCOMCLIENT}
  LookUpEditorActive := result and assigned(Columns[Col].LookupHandle);
  {$ENDIF}
end;

function TBoldCustomGridCom.CanEditShowForCustomEditors: Boolean;
begin
  Result := (inherited CanEditShow) and Assigned(CurrentCellFollower);
end;

procedure TBoldCustomGridCom.TopLeftChanged;
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

procedure TBoldCustomGridCom.DrawCell(ACol, aRow: Longint; ARect: TRect; AState: TGridDrawState);
var
  aListRow: Integer;
  DrawColumn: TBoldGridColumnCom;
  Align: TAlignment;
  cl: TColor;
  TempRect: TRect;
  FrameFlags1, FrameFlags2: DWORD;

begin


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
          TBoldAsStringRendererCom.DrawStringOnCanvas(Canvas, ARect, Align, Point(1, 1), DrawColumn.Title.Caption);
        end
      end
      else if RowFollowers[aListRow].Displayable then
      begin
        with Columns[ACol].BoldProperties do
        begin
          SetFont(Canvas.Font, CellFont(DrawColumn), CellFollowers[ACol, aListRow]);
          SetColor(cl, DrawColumn.Color, CellFollowers[ACol, aListRow])
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
          Columns[ACol].BoldProperties.DrawOnCanvas(CellFollowers[ACol, aListRow], Canvas, ARect, DrawColumn.Alignment, Point(2, 2));
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

procedure TBoldCustomGridCom._AfterMakeCellUptoDate(Follower: TBoldFollowerCom);
var
  DisplayGridRow: Integer;
  DisplayCol: Integer;
begin
  DisplayGridRow := GridRow(Follower.OwningFollower.index);
  DisplayCol := Follower.index;
  if Assigned(InplaceEditor) and InplaceEditor.Visible and (Row = DisplayGridRow)  and (Col = DisplayCol) then
    InplaceEditor.Text := (Follower.Controller as TBoldStringFollowerControllerCom).GetCurrentAsString(Follower);
  if (DisplayGridRow >= TopRow) and (DisplayGridRow <= (TopRow + VisibleRowCount + 1)) then
    InvalidateCell(DisplayCol, DisplayGridRow);
end;

procedure TBoldCustomGridCom.InvalidateFromRow(DisplayDataRow: Longint);
var
  I: Integer;
begin
  for I := GridRow(DisplayDataRow) to MinIntValue([RowCount - 1, TopRow + VisibleRowCount + 1]) do
    InvalidateRow(I);
end;

procedure TBoldCustomGridCom._InsertRow(Follower: TBoldFollowerCom);
begin
  if Follower.Index < fInvalidateFrom then
    fInvalidateFrom := Follower.Index;
  fLastInsertedRowIndex := Follower.Index;
end;

procedure TBoldCustomGridCom._DeleteRow(index: Integer; owningFollower: TBoldFollowerCom);
begin
  if Index < fInvalidateFrom then
    fInvalidateFrom := Index;
end;

procedure TBoldCustomGridCom.SetCurrentRow(DataRow: Integer);
begin
  Row := GridRow(MaxIntValue([0, DataRow]));
  ReallyInvalidateCol(0);
end;

function TBoldCustomGridCom.GetRowFollower(DataRow: Integer): TBoldFollowerCom;
begin
  if datarow < Follower.SubFollowerCount then
    Result := Follower.SubFollowers[DataRow]
  else
    result := nil;
end;

function TBoldCustomGridCom.GetCellFollower(ListCol, DataRow: Integer): TBoldFollowerCom;
var
  RowFollower: TBoldFollowerCom;
begin
  RowFollower := GetRowFollower(DataRow);
  if assigned(RowFollower) and
    (ListCol >= 0) and
    (listCol < RowFollower.SubFollowerCount) then
    Result := RowFollower.SubFollowers[ListCol]
  else
    result := nil;
end;

function TBoldCustomGridCom.GetCurrentCellFollower;
begin
  if (Col < FixedCols)  then
    Result := nil
  else
    Result := CellFollowers[Col, DataRow(Row)];
end;

function TBoldCustomGridCom.GetSelected(DataRow: integer): Boolean;
begin
  if (DataRow >= 0) and (DataRow < Follower.SubFollowerCount) then
    Result := RowFollowers[DataRow].Selected
  else
    Result := false;
end;

procedure TBoldCustomGridCom._AfterMakeListUptoDate(Follower: TBoldFollowerCom);
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

  if (fSubFollowerCountBeforeMakeUpToDate = 0) and
     (Follower.SubFollowerCount > 0) and
     (Follower.CurrentIndex <> -1) then
  begin
    Follower.SubFollowers[Follower.CurrentIndex].Selected := true;
  end;

  AdjustActiveRange;
  if fInvalidateFrom <> MAXINT then
    InvalidateFromRow(fInvalidateFrom);
  if GridRow(Follower.CurrentIndex) < RowCount then
  begin
    OldLeftCol := LeftCol;
    SetCurrentRow(Follower.CurrentIndex);
    LeftCol := OldLeftCol;
  end;

  if not fIsMultiSelecting and
    assigned(Follower.SubFollowers[Follower.CurrentIndex]) and
    not Follower.SubFollowers[Follower.CurrentIndex].Selected then
  begin
    fBoldProperties.SelectAll(Follower, False);
    fBoldProperties.SetSelected(Follower, Follower.CurrentIndex, True);
  end;
  fMakingListUpToDate := False;
end;

function TBoldCustomGridCom.GetBoldHandle: TBoldAbstractListHandleCom;
begin
  if not assigned(fHandleFollower) then
    result := nil
  else
    Result := fHandleFollower.BoldHandle;
end;

function TBoldCustomGridCom.GetFollower: TBoldFollowerCom;
begin
  Assert(Assigned(fHandleFollower));
  Result := fHandleFollower.Follower;
end;

procedure TBoldCustomGridCom._BeforeMakeListUpToDate(Follower: TBoldFollowerCom);
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

function TBoldCustomGridCom.GetHandleStaticType: IBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TBoldCustomGridCom.GetHandleListElementType: IBoldElementTypeInfo;
begin
  if Assigned(BoldHandle) then
    Result := BoldHandle.ListElementType
  else
    Result := nil;
end;

procedure TBoldCustomGridCom.SetTitleFont(const Value: TFont);
begin
  fTitleFont.Assign(Value);
end;

function TBoldCustomGridCom.GetBoldHandleIndexLock: Boolean;
begin
  Result := fHandleFollower.HandleIndexLock;
end;

procedure TBoldCustomGridCom.SetBoldHandleIndexLock(Value: Boolean);
begin
  fHandleFollower.HandleIndexLock := Value;
end;

{$IFNDEF BOLDCOMCLIENT}

procedure TBoldInplaceEditCom.ComboChange(sender: TObject);
begin
  if not fComboAborting then
    Grid.LookUpChange(sender, fDestElement, fEditColumn);
  HideCombo;
end;

procedure TBoldInplaceEditCom._Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = breDestElementDestroyed then
    fDestElement := nil;
end;

procedure TBoldInplaceEditCom.HideCombo;
begin
  Combo.visible := false;
end;

function TBoldInplaceEditCom.GetDestElement(CellFollower: TBoldFollowerCom; Column: TBoldGridColumnCom): IBoldElement;
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

function TBoldInplaceEditCom.GetCombo: TComboBox;
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

procedure TBoldInplaceEditCom.WMKeyDown(var Message: TWMKeyDown);
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

procedure TBoldInplaceEditCom.InitCombo(var Message: TWMWindowPosChanged);
var
  i: Integer;
  CellFollower: TBoldFollowerCom;
  ListElement: IBoldElement;
  CellValue: IBoldElement;
  FollowerController: TExposedFollowerController;
  Renderer: TExposedBoldAsStringRenderer;
begin
  fEditColumn := Grid.Columns[Grid.Col] as TBoldGridColumnCom;
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
    if CellValue is IBoldObjectReference then
      CellValue := (CellValue as IBoldObjectReference).BoldObject;

    fInitialComboIndex := -1;
    if fEditColumn.NilElementMode = neInsertFirst then
    begin
      fInitialComboIndex := 0;
      Combo.Items.AddObject(fEditColumn.LookUpProperties.NilStringRepresentation, nil);
    end;

    FollowerController := TExposedFollowerController(fEditColumn.LookUpProperties);
    renderer := TExposedBoldAsStringRenderer(FollowerController.EffectiveRenderer);
    if assigned(fEditColumn.LookupHandle.List) then
      fEditColumn.LookupHandle.List.EnsureRange(0, fEditColumn.LookupHandle.Count - 1);
    for i := 0 to fEditColumn.LookupHandle.Count - 1 do
    begin
      ListElement := fEditColumn.LookupHandle.List[i];

      Combo.Items.AddObject(
        Renderer.GetAsStringAndSubscribe(ListElement, fEditColumn.LookUpProperties, nil),
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

procedure TBoldInplaceEditCom.ComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TBoldCustomGridCom._FontChanged(Sender: TObject);
begin
  ParentFont := false;
end;

function TBoldCustomGridCom.GetEditLimit: Integer;
var
  El: IBoldElement;
begin
  result := 0;
  {$IFNDEF BOLDCOMCLIENT}
  El := TBoldInplaceEditCom(InplaceEditor).GetDestElement(CurrentCellFollower, Columns[Col]);
  if (el is TBAString) and assigned((el as TBAString).BoldAttributeRTInfo) then
    Result := (el as TBAString).BoldAttributeRTInfo.Length;
  if result < 0 then
    result := 0;
  {$ENDIF}
end;

{ TBoldFirstColumnRendererCom }

procedure TBoldFirstColumnRendererCom.DrawOnCanvas(Follower: TBoldFollowerCom;
  Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint);
begin
  inherited;
  with Follower do
  begin
    Canvas.Brush.Color := (Owner as TBoldCustomGridCom).FixedColor;
    Canvas.FillRect(Rect);
    if OwningFollower.OwningFollower.CurrentIndex = OwningFollower.index then
      Canvas.Draw(Rect.Left + 8, Rect.Top + 4, bmpBoldGridCurrent);
    if OwningFollower.Selected then
      Canvas.Draw(Rect.Left, Rect.Top + 4, bmpBoldGridSelected);
  end;
end;

{$IFNDEF BOLDCOMCLIENT}

procedure TBoldGridCheckBoxPainterRenderer.GetCurrentElement(Follower: TBoldFollowerCom; ie: TBoldIndirectElement);
var
  StringController: TBoldStringFollowerControllerCom;
begin
  StringController := Follower.AssertedController as TBoldStringFollowerControllerCom;
  Follower.element.EvaluateExpression(StringController.Expression, ie, false, StringController.VariableList);
end;

function TBoldGridCheckBoxPainterRenderer.GetCurrentCheckBoxState(Follower: TBoldFollowerCom): TCheckBoxState;
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

procedure TBoldGridCheckBoxPainterRenderer.CheckBoxClick(BUTTON: TMouseButton; Shift: TShiftState; X, Y: Integer; Grid: TBoldCustomGridCom; Alignment: TAlignment);
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

procedure TBoldGridCheckBoxPainterRenderer.KeyPress(var Key: word; Shift: TShiftState; Grid: TBoldCustomGridCom);
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

procedure TBoldGridCheckBoxPainterRenderer.ToggleValue(GridCoord: TGridCoord; Grid: TBoldCustomGridCom);
var
  CheckBoxState: TCheckBoxState;
  CurrentFollower: TBoldFollowerCom;
  BoolValue: TBABoolean;
  ie: TBoldIndirectElement;
begin
  CurrentFollower := Grid.CellFollowers[GridCoord.x, GridCoord.y-Grid.FixedRows];
  if CurrentFollower.rendererData.MayModify then
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

procedure TBoldGridCheckBoxPainterRenderer.DrawOnCanvas(Follower: TBoldFollowerCom; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint);
var
  OrgPenColor, OrgBrushColor: TColor;
  GridCoord: TGridCoord;
  CheckBoxRect, GrayRect: TRect;
  i: integer;
  checkBoxState: TCheckBoxState;
  Grid: TBoldCustomGridCom;
begin
  CheckBoxState := GetCurrentCheckBoxState(Follower);
  OrgPenColor := Canvas.Pen.Color;
  OrgBrushColor := Canvas.Brush.Color;

  GRid := TExposedFollowerController(Follower.AssertedController).OwningComponent as TBoldCustomGridCom;
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

{ TBoldConstraintRendererCom }

procedure TBoldConstraintRendererCom.DefaultMakeUptodateAndSetMayModifyAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldStringFollowerControllerCom; Subscriber: TBoldComClientSubscriber);
begin
  inherited DefaultMakeUptoDateAndSetMayModifyAndSubscribe(Element, RendererData, FollowerController, Subscriber);
  RendererData.MayModify := false;
end;

procedure TBoldConstraintRendererCom.DrawOnCanvas(Follower: TBoldFollowerCom; Canvas: TCanvas; Rect: TRect;
  Alignment: TAlignment; Margins: TPoint);
var
  RectMiddle, IncValue: Integer;
begin
  with Follower do
  begin
    Canvas.FillRect(Rect);
    RectMiddle := (Rect.Bottom - Rect.Top) div 2;
    if (RendererData as TBoldStringRendererDataCom).CurrentStringValue = 'Y' then
    begin
      IncValue := RectMiddle - (bmpBoldGridConstraint_false.Height div 2);
      Canvas.Draw(Rect.Left + 1, Rect.Top + IncValue, bmpBoldGridConstraint_false);
    end
    else
    if (RendererData as TBoldStringRendererDataCom).CurrentStringValue = 'N' then
    begin
      IncValue := RectMiddle - (bmpBoldGridConstraint_true.Height div 2);
      Canvas.Draw(Rect.Left + 1, Rect.Top + IncValue, bmpBoldGridConstraint_true);
    end;
  end;
end;

function TBoldGridColumnCom.GetFont: TFont;
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

procedure TBoldCustomGridCom.EnsureConstraintColumn;
begin
  if BoldShowConstraints and not (csdesigning in componentState) then
  begin
    with AddColumn do
    begin
      SetIndex(1);
      BoldProperties.Expression := 'constraints->exists(c|not c)';
      Width := bmpBoldGridSelected.Width + 3;
      Title.Caption := '§';
      BoldProperties.Renderer := TBoldConstraintRendererCom.Create(Self);
      ColReadOnly := True;
    end;
  end;
end;

procedure TBoldGridColumnCom.SetIndex(Value: Integer);
begin
  fGrid.fBoldColumnsProperties.Move(index, value);
  inherited;
  fGrid.Invalidate;
end;

procedure TBoldCustomGridCom.ReallyInvalidateCol(Column: integer);
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

function TBoldCustomGridCom.GetMutableList: IBoldList;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.MutableList
  else
    result := nil;
end;

procedure TBoldCustomGridCom.AdjustActiveRange;
var
  firstActive, LastActive: Integer;
begin
  if Assigned(Follower) then
  begin
    GetActiveRange(FirstActive, LastActive);
    BoldProperties.SetActiveRange(Follower, firstActive, lastActive, 10);
    EnsureActiveCellFollowerExpressions;
  end;
end;

procedure TBoldCustomGridCom.Resize;
begin
  inherited;
  if not fMakingListUpToDate then
    AdjustActiveRange;
end;

function TBoldCustomGridCom.GridRow(Datarow: Integer): Integer;
begin
  Result := Datarow + FixedRows;
end;

function TBoldCustomGridCom.GetShowTitleRow: Boolean;
begin
  result := fixedRows = 1;
end;

procedure TBoldCustomGridCom.SetShowTitleRow(const Value: Boolean);
begin
  if Value then
    FixedRows := 1
  else
    FixedRows := 0;
end;

function TBoldCustomGridCom.GetTitleRow: integer;
begin
  if ShowTitleRow then
    Result := 0
  else
    Result := -1;
end;

{$IFNDEF BOLDCOMCLIENT}
function TBoldCustomGridCom.ColumnIsCheckBox(Col: integer): Boolean;
begin
  result := (Col >= 0) and (Col < Columns.Count) and assigned(Columns[Col].fCheckBoxPainterRenderer);
end;

procedure TBoldCustomGridCom.WMChar(var Msg: TWMChar);
begin
  if not ColumnIsCheckBox(col) then
    inherited;
end;

function TBoldCustomGridCom.ValidateComponent(ComponentValidator: TBoldComponentValidatorCom; NamePrefix: String): Boolean;
var
  i: integer;
  Context: IBoldElementTypeInfo;
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

procedure TBoldCustomGridCom.LookUpChange(sender: Tobject; DestElement: IBoldElement; EditColumn: TBoldGridColumnCom);
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
      EditColumn.OnLookUpChange(DestElement, GetEditor.Combo.Items.Objects[aIndex] as IBoldElement)
    else
      DestElement.Assign(GetEditor.Combo.Items.Objects[aIndex] as IBoldElement);
  end;
end;
{$ENDIF}

function TBoldCustomGridCom.ColumnClass: TBoldColumnClassCom;
begin
  result := TBoldGridColumnCom;
end;

function TBoldCustomGridCom.SelectCell(ACol, ARow: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnSelectCell) then
    FOnSelectCell(Self, ACol, ARow, Result);
end;

function TBoldCustomGridCom.GetEditor: TBoldInplaceEditCom;
begin
  result := InplaceEditor as TBoldInplaceEditCom;
end;

function TBoldCustomGridCom.AsClipBoardText: String;
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

procedure TBoldGridColumnCom._Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = breColumnLookUpHandleDestroyed then
    LookupHandle := nil;
end;

procedure TBoldGridColumnCom.SetCheckBoxRendererIfAppropriate;
var
  SystemTypeinfo: IBoldSystemTypeInfo;
  ListElementTypeInfo: IBoldElementTypeInfo;
  BooleanTypeInfo: IBoldAttributeTypeInfo;
  ResultTypeInfo: IBoldElementTypeInfo;
begin
  if not assigned(BoldProperties.Renderer) and
    assigned(Grid.BoldHandle) and
    assigned(Grid.BoldHandle.ListElementType) and
    (AllowCheckBox or BoldAllowCheckBoxInGrids) then
  begin
    ListElementTypeInfo := Grid.BoldHandle.ListElementType;
    SystemTypeInfo := ListElementTypeInfo.SystemTypeInfo as IBoldSystemTypeInfo;
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

function TBoldGridColumnCom.ColumnHasCheckBoxOverrides: Boolean;
begin
  result := false;
end;

function TBoldGridColumnCom.GetCurrentCheckBoxState(
  Follower: TBoldFollowerCom): TCheckBoxState;
begin
  result := cbGrayed;
end;

procedure TBoldGridColumnCom.SetCurrentCheckBoxState(Follower: TBoldFollowerCom; NewValue: TCheckBoxState);
begin
end;

function TBoldGridColumnCom.GetLookupContext: IBoldElementTypeInfo;
begin
  if assigned(LookupHandle) then
    result := LookupHandle.StaticBoldType
  else
    result := nil;
end;

procedure TBoldGridColumnCom.SetLookupHandle(const Value: TBoldAbstractListHandleCom);
begin
  if value <> LookupHandle then
  begin
    fLookupHandleSubscriber.CancelAllSubscriptions;
    fLookupHandle := Value;
    if assigned(fLookUpHandle) then
      fLookupHandle.AddSmallSubscription(fLookUpHandleSubscriber, [beDestroying], breColumnLookUpHandleDestroyed);
  end;
end;

procedure TBoldGridColumnCom.SetLookUpProperties(const Value: TBoldStringFollowerControllerCom);
begin
  if assigned(value) then
    fLookUpProperties.Assign(Value);
end;

{$ENDIF}

procedure TBoldCustomGridCom.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TBoldCustomGridCom.ActivateAllCells;
begin
  BoldProperties.SetActiveRange(Follower, 0, RowCount - 1);
  DisplayAvailableFollowers;
end;

procedure TBoldCustomGridCom.GetActiveRange(var FirstActive, LastActive: integer);
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

procedure TBoldCustomGridCom.EnsureRowActive(DataRow: integer);
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

procedure TBoldCustomGridCom.DisplayAvailableFollowers;
begin
  Follower.EnsureDisplayable;
end;

function TBoldCustomGridCom.GetCellText(col, row: integer): string;
begin
  if row < FixedRows then
    Result := Columns[Col].Title.Caption
  else
    result := GetString(col, DataRow(Row));
end;

procedure TBoldCustomGridCom.DisplayAllCells;
begin
  BoldProperties.SelectAll(Follower, true);
  Invalidate;
  AdjustActiveRange;
  EnsureActiveCellFollowerExpressions; 
  Follower.EnsureDisplayable;
  BoldProperties.SelectAll(Follower, false);
  BoldProperties.SetSelected(Follower, DataRow(Row), true);
end;

procedure TBoldCustomGridCom.EnsureActiveCellFollowerExpressions;
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