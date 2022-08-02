unit cxGridBoldSupportUnit;


{$ASSERTIONS ON}
{$INCLUDE Bold.inc}

{$DEFINE DelayOnFocusedRecordChange}

{.$DEFINE IEJpegImage}
{.$DEFINE DefaultDragMode}
{.$DEFINE FireAfterLoadOnChangeOnly}
{$DEFINE CenterResultOnIncSearch}

{.$DEFINE BoldDevExLog}

(*
  cxGrid Bold aware components v2.7 - Oct 2018
  for ExpressQuantumGrid 6

  2007-2018 Daniel Mauric

  features
    - on demand fetching
    - appropriate column properties for each BoldAttribute, including ComboBoxProperties with possible values for TBAValueSet
    - smart drag & drop support, across views and within single view if list is ordered, etc...
    - constraint column with hints that contain broken constraints messages
    - implements IBoldValidateableComponent
    - global var cxBoldDataSourceClass allows to plugin a custom TcxBoldDataSource subclass

    non bold related features
    - ctrl+home and ctrl+end go to top and bottom of the list, respectively
    - ctrl+numeric plus toggles ApplyBestFit for the visible range of records

  known issues
    - Drag drop is currently not supported in the CardView
    - Master detail views only supported when connected to BoldListHandles

*)

interface

uses
  Classes,
  Controls,
  Types,
  Messages,
  Contnrs,

  cxGridCustomTableView,
  cxCustomData,
  cxGridCustomView,
  cxGridTableView,
  cxStorage,
  cxDataStorage,
  cxData,
  cxDataUtils,
  cxEdit,
  cxDropDownEdit,
  cxGridCardView,
//  cxGridChartView,
  cxGridBandedTableView,
  cxGridLayoutView,
  cxFilter,
  cxGraphics,

  BoldSystem,
  BoldEnvironmentVCL, // Make sure VCL environement loaded, and finalized after
  BoldComponentvalidator,
  BoldSystemRT,
  BoldControlPack,
//  BoldStringControlPack,
  BoldVariantControlPack,
  BoldListHandleFollower,
  BoldListListControlPack,
  BoldControllerListControlPack,
  BoldAbstractListHandle,
  BoldElements,
  BoldSubscription,

  cxBoldEditors; // for IcxBoldEditProperties, perhaps extract that class to another unit later on and use that instead

type
  TcxGridBoldTableView = class;
  TcxGridBoldColumn = class;
  TcxGridItemBoldDataBinding = class;
  TcxBoldDataController = class;
  TcxGridBoldDataController = class;
  TcxBoldDataSource = class;
  TcxGridBoldTableController = class;
  TcxGridBoldCardView = class;
(*
  TcxBoldGridChartView = class;
  TcxGridBoldChartDataController = class;
  TcxGridBoldChartCategories = class;
  TcxGridBoldChartDataGroup = class;
  TcxGridBoldChartSeries = class;
*)
  TcxGridBoldBandedTableView = class;
  TcxGridBoldBandedColumn = class;
  TcxGridBoldBandedTableController = class;
  TcxBoldDataControllerSearch = class;
  TcxBoldCustomDataProvider = class;
  TcxGridBoldEditingController = class;

  TcxGridBoldLayoutView = class;
  TcxGridBoldLayoutViewItem = class;

  TBoldCxGridVariantFollowerController = class;

  IBoldAwareViewItem = interface
    ['{187C2B47-FD11-4A01-9340-6BC608B6FF38}']
    function GetBoldProperties: TBoldVariantFollowerController;
    property BoldProperties: TBoldVariantFollowerController read GetBoldProperties;
  end;

  IBoldAwareView = interface
    ['{51A80761-FCA5-4D4E-8585-907B8C08C404}']
    function GetDataController: TcxGridBoldDataController;
    procedure SetDataController(Value: TcxGridBoldDataController);
    property DataController: TcxGridBoldDataController read GetDataController write SetDataController;
    function GetItemCount: Integer;
    property ItemCount: Integer read GetItemCount;
    function GetItem(Index: Integer): IBoldAwareViewItem;
    property Items[Index: Integer]: IBoldAwareViewItem read GetItem; default;
    function GetSelection: TBoldList;
    property Selection: TBoldList read GetSelection;
    procedure DoSelectionChanged;
    procedure ClearItems;

    function GetCurrentBoldObject: TBoldObject;
    function GetCurrentIndex: integer;
    function GetCurrentElement: TBoldElement;

    property CurrentBoldObject: TBoldObject read GetCurrentBoldObject;
    property CurrentElement: TBoldElement read GetCurrentElement;
    property CurrentIndex: integer read GetCurrentIndex;
  end;

  TBoldCxGridVariantFollowerController = class(TBoldVariantFollowerController)
  protected
    fcxGridItemBoldDataBinding: TcxGridItemBoldDataBinding;
  public
    function SubFollowersActive: boolean; override;
    constructor Create(aOwningComponent: TComponent); reintroduce;
  end;

  TcxGridBoldDefaultValuesProvider = class(TcxCustomBoldEditDefaultValuesProvider)
  public
    function DefaultCanModify: Boolean; override;
    function IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean; override;
  end;


  TcxBoldDataSource = class(TcxCustomDataSource)
  private
    fBoldDataController: TcxBoldDataController;
    fIsBoldInitiatedChange: boolean;
  protected
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
    function GetItemHandle(AItemIndex: Integer): TcxDataItemHandle; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle; override;
    function IsRecordIdSupported: Boolean; override;
    function GetRecordId(ARecordHandle: TcxDataRecordHandle): Variant; override;
    function GetDetailHasChildren(ARecordIndex, ARelationIndex: Integer): Boolean; override;
    procedure LoadRecordHandles; override;
    procedure CustomSort; override;
    function CustomSortElementCompare(Item1, Item2: TBoldElement): integer;
    function IsCustomSorting: Boolean; override;
  public
    constructor Create(aBoldDataController: TcxBoldDataController); virtual;
    destructor Destroy; override;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
    function GetRecordHandleByIndex(ARecordIndex: Integer): TcxDataRecordHandle; override;
  end;

  TcxBoldCustomDataControllerInfo = class(TcxCustomDataControllerInfo)
  protected
    procedure DoSort; override;
    procedure DoFilter; override;
  end;

  TcxBoldDataSummary = class(TcxDataSummary)
  protected
{$IFDEF BOLD_DELPHI16_OR_LATER}
    procedure CalculateSummary(ASummaryItems: TcxDataSummaryItems; ABeginIndex, AEndIndex: Integer;
      var ACountValues: TcxDataSummaryCountValues; var ASummaryValues: TcxDataSummaryValues); override;
{$ELSE}
    procedure CalculateSummary(ASummaryItems: TcxDataSummaryItems; ABeginIndex, AEndIndex: Integer;
      var ACountValues: TcxDataSummaryCountValues; var ASummaryValues: TcxDataSummaryValues; var SummaryValues: Variant); override;
{$ENDIF}
  end;

  TcxGridUserQueryEvent = procedure (Sender: TObject; var Allow: boolean) of object;

  TcxBoldDataController = class(TcxCustomDataController)
  private
    fBoldHandleFollower: TBoldListHandleFollower;
    fBoldProperties: TBoldListAsFollowerListController;
    fBoldColumnsProperties: TBoldControllerList;
    fSubscriber: TBoldPassthroughSubscriber;
    FSkipMakeCellUptoDate: integer;
    FSkipSyncFocusedRecord: integer;
    fInDelayScrollUpdate: boolean;
    fCurrentListElementType: TBoldElementTypeInfo;
    fSelection: TBoldList;
    fBoldAutoColumns: Boolean;
    fDataChanged: boolean;
    fInvalidating: boolean;
    fFetchedAll: boolean;
    fInternalLoading: boolean;
//    fBeforeLoad: TNotifyEvent;
    fAfterLoad: TNotifyEvent;
    fLoadAll: boolean;
    fSkipCancel: boolean;
    fOnDelete: TNotifyEvent;
    fOnInsert: TNotifyEvent;
    fCanInsert: TcxGridUserQueryEvent;
    fCanDelete: TcxGridUserQueryEvent;
    FUseDelayedScrollUpdate: boolean;
    fClearColumnsOnTypeChange: boolean;
    fStoredRecordIndex: integer;
    fRedrawGrid: boolean;
    fRecalcSummary: boolean;
{$IFDEF DelayOnFocusedRecordChange}
    fFocusChanged: boolean;
    fPrevFocusedRecordIndex: integer;
    fFocusedRecordIndex: integer;
    fPrevFocusedDataRecordIndex: integer;
    fFocusedDataRecordIndex: integer;
    fNewItemRecordFocusingChanged: boolean;
{$ENDIF}
    function GetRecNo: Integer;  {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SetRecNo(const Value: Integer);
    function GetBoldHandle: TBoldAbstractListHandle;  {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetBoldHandleIndexLock: Boolean;
    procedure SetBoldHandle(const Value: TBoldAbstractListHandle);
    procedure SetBoldHandleIndexLock(const Value: Boolean);
    procedure SetController(const Value: TBoldListAsFollowerListController);
    function GetRowFollower(DataRow: Integer): TBoldFollower;  {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetFollower: TBoldFollower;  {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetCellFollower(ARecordIndex, AItemIndex: Integer): TBoldFollower;  {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetSelection: TBoldList; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    function GetBoldList: TBoldList; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure SetDataChanged(const Value: boolean);
    procedure FindMinMaxIndex(ListA, ListB: TBoldList; var AFrom, ATo: integer);
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetHasCellFollower(ARecordIndex, AItemIndex: Integer): boolean;
    procedure EnsureEventQueued;
  protected
    function GetDataProviderClass: TcxCustomDataProviderClass; override;
    function GetSearchClass: TcxDataControllerSearchClass; override;
    function CreateDataControllerInfo: TcxCustomDataControllerInfo; override;
    function IsDataLinked: Boolean;
    function IsSmartRefresh: Boolean; override;
    function GetCurrentBoldObject: TBoldObject;
    function GetCurrentIndex: integer;
    function GetCurrentElement: TBoldElement;
    procedure ProcessQueueEvent(Sender: TObject); virtual;
    procedure BeforeSorting; override;
    function BoldSetValue(AItemHandle: TcxDataItemHandle; ACellFollower: TBoldFollower; const AValue: variant): boolean; virtual;
    function RequiresAllRecords: boolean; overload; virtual;
    function RequiresAllRecords(AItem: TObject): boolean; overload; virtual;
    procedure SelectionChanged; virtual;
    procedure FilterChanged; override;
    function FindItemByData(AData: Integer): TObject;
    function GetItemData(AItem: TObject): Integer; virtual; abstract;
    function BoldPropertiesFromItem(aIndex: integer): TBoldVariantFollowerController;
    procedure _InsertRow(index: Integer; Follower: TBoldFollower); virtual;
    procedure _DeleteRow(index: Integer; owningFollower: TBoldFollower); virtual;
    procedure _ReplaceRow(index: Integer; AFollower: TBoldFollower); virtual;
    procedure _BeforeMakeListUpToDate(Follower: TBoldFollower); virtual;
    procedure _AfterMakeListUptoDate(Follower: TBoldFollower); virtual;
    procedure _AfterMakeCellUptoDate(Follower: TBoldFollower); virtual;
    function GetHandleListElementType: TBoldElementTypeInfo;
    function TypeMayHaveChanged: boolean;
    procedure TypeChanged(aNewType, aOldType: TBoldElementTypeInfo); virtual;
    procedure BeginDelayScrollUpdate;
    procedure EndDelayScrollUpdate;
    procedure DisplayFollowers; virtual;
    function MainFollowerNeedsDisplay: boolean;
    function EnsureFollower(ARecordIndex, AItemIndex: integer): boolean;
    property BoldHandleIndexLock: Boolean read GetBoldHandleIndexLock write SetBoldHandleIndexLock default true;
    property HasCellFollower[ARecordIndex, AItemIndex: Integer]: boolean read GetHasCellFollower;
    property BoldAutoColumns: Boolean read fBoldAutoColumns write fBoldAutoColumns default false;
    property BoldColumnsProperties: TBoldControllerList read fBoldColumnsProperties;
    property DataHasChanged: boolean read fDataChanged write SetDataChanged;
    property SkipMakeCellUptoDate: integer read FSkipMakeCellUptoDate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InDelayScrollUpdate: boolean;
    function IsProviderMode: Boolean; override;
    function GetRecordCount: Integer; override;
    procedure Cancel; override;
    procedure AdjustActiveRange(aList: TBoldList = nil; aItem: integer = -1); overload; virtual;
    procedure AdjustActiveRange(aRecordIndex: integer; aItem: integer = -1); overload;
    procedure PreFetchColumns(aList: TBoldList = nil; aItem: integer = -1); virtual;
    function GetHandleStaticType: TBoldElementTypeInfo;
    function GetCurrentDetailDataController(ARelationIndex: integer = 0): TcxBoldDataController;
    function CreateList: TBoldList;
    property BoldProperties: TBoldListAsFollowerListController read fBoldProperties write SetController;
    property BoldHandle: TBoldAbstractListHandle read GetBoldHandle write SetBoldHandle;
    property BoldHandleFollower: TBoldListHandleFollower read fBoldHandleFollower;
    property RecNo: Integer read GetRecNo write SetRecNo; // Sequenced
    property Follower: TBoldFollower read GetFollower;
    property CurrentBoldObject: TBoldObject read GetCurrentBoldObject;
    property CurrentElement: TBoldElement read GetCurrentElement;
    property CurrentIndex: integer read GetCurrentIndex;
//    property OnBeforeLoad: TNotifyEvent read fBeforeLoad write fBeforeLoad;
    property OnAfterLoad: TNotifyEvent read fAfterLoad write fAfterLoad;
    property LoadAll: boolean read fLoadAll write fLoadAll default false;
    property Selection: TBoldList read GetSelection;
    property BoldList: TBoldList read GetBoldList;
    property CellFollowers[ARecordIndex, AItemIndex: Integer]: TBoldFollower read GetCellFollower;    
  published
    property OnInsert: TNotifyEvent read fOnInsert write fOnInsert;
    property OnDelete: TNotifyEvent read fOnDelete write fOnDelete;
    property CanInsert: TcxGridUserQueryEvent read fCanInsert write fCanInsert;
    property CanDelete: TcxGridUserQueryEvent read fCanDelete write fCanDelete;
    property UseDelayedScrollUpdate: boolean read FUseDelayedScrollUpdate write FUseDelayedScrollUpdate default true;
    property ClearColumnsOnTypeChange: boolean read fClearColumnsOnTypeChange write fClearColumnsOnTypeChange default true;
  end;

  TcxGridBoldDataController = class(TcxBoldDataController, IcxCustomGridDataController, IcxGridDataController)
  private
    FPrevScrollBarPos: Integer;
    fCreatingColumns: boolean;
    fInternalChange: boolean;
    fTriggerAfterLoad: boolean;
    function GetController: TcxCustomGridTableController;
    function GetGridViewValue: TcxCustomGridTableView;
    procedure ConstraintColumnCustomDrawCell(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure ColumnCustomDrawCell(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
  protected
    function GetSummaryItemClass: TcxDataSummaryItemClass; override;
    function GetSummaryGroupItemLinkClass: TcxDataSummaryGroupItemLinkClass; override;

    procedure CheckDataSetCurrent; override; // used to get CurrentIndex (ie. current record after change)
    procedure GetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
      ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint;
      var AHintText: TCaption; var AIsHintMultiLine: Boolean; var AHintTextRect: TRect);
    procedure SelectionChanged; override;
    procedure ProcessQueueEvent(Sender: TObject); override;
    procedure DisplayFollowers; override;
    function DoEditing(AItem: TcxCustomGridTableItem): Boolean;
    function BoldSetValue(AItemHandle: TcxDataItemHandle; ACellFollower: TBoldFollower; const AValue: variant): boolean; override;

    function GetOwnerOrView: TComponent;
    { IcxCustomGridDataController }
    procedure AssignData(ADataController: TcxCustomDataController);
    procedure DeleteAllItems;
    procedure GetFakeComponentLinks(AList: TList);
    function GetGridView: TcxCustomGridView;
    function HasAllItems: Boolean;
    function IsDataChangeable: Boolean;
    function IsDataLinked: Boolean;
    function SupportsCreateAllItems: Boolean;

    { IcxGridDataController }
    procedure CheckGridModeBufferCount;
    function DoScroll(AForward: Boolean): Boolean;
    function DoScrollPage(AForward: Boolean): Boolean;
    //function GetFilterPropertyValue(const AName: string; var AValue: Variant): Boolean;
    function GetItemDataBindingClass: TcxGridItemDataBindingClass;
    function GetItemDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
    function GetNavigatorIsBof: Boolean;
    function GetNavigatorIsEof: Boolean;
    function GetScrollBarPos: Integer;
    function GetScrollBarRecordCount: Integer;
    //function SetFilterPropertyValue(const AName: string; const AValue: Variant): Boolean;
    function SetScrollBarPos(Value: Integer): Boolean;
    function SupportsScrollBarParams: Boolean; virtual;
    function GetItemData(AItem: TObject): Integer; override;
    function RequiresAllRecords: boolean; overload; override;
    function RequiresAllRecords(AItem: TObject): boolean; overload; override;
    function CanSelectRow(ARowIndex: Integer): Boolean; override;
    function CompareByField(ARecordIndex1, ARecordIndex2: Integer;
      AField: TcxCustomDataField; AMode: TcxDataControllerComparisonMode): Integer; override;
    procedure DoValueTypeClassChanged(AItemIndex: Integer); override;
    procedure FilterChanged; override;
    function GetDefaultActiveRelationIndex: Integer; override;
    function GetFilterDisplayText(ARecordIndex, AItemIndex: Integer): string; override;
    function GetItemID(AItem: TObject): Integer; override;
//    function GetItemData(AItem: TObject): Integer; virtual;
    function GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass; override;
    function GetSummaryClass: TcxDataSummaryClass; override;
    { Bold methods }
    procedure _BeforeMakeListUpToDate(Follower: TBoldFollower); override;
    procedure _AfterMakeListUptoDate(Follower: TBoldFollower); override;
    procedure _AfterMakeCellUptoDate(Follower: TBoldFollower); override;
    procedure TypeChanged(aNewType, aOldType: TBoldElementTypeInfo); override;
    procedure ConnectEvents(AConnect: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateAllItems(AMissingItemsOnly: Boolean);
    procedure EnsureConstraintColumn;
    function GetItemByExpression(const AExpression: string): TObject;
    function GetItem(Index: Integer): TObject; override;
    procedure ChangeValueTypeClass(AItemIndex: Integer; AValueTypeClass: TcxValueTypeClass); override;
    procedure DoStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DoEndDrag(Sender, Target: TObject; X, Y: Integer);
    function CreateItem(aGridView: TcxCustomGridTableView; const aExpression, aCaption, aValueType, aName: string): TcxCustomGridTableItem;
    procedure BeginFullUpdate; override;
    procedure EndFullUpdate; override;
    function CreateDetailLinkObject(ARelation: TcxCustomDataRelation;
      ARecordIndex: Integer): TObject; override;
    procedure FocusControl(AItemIndex: Integer; var Done: Boolean); override;
    function GetDetailDataControllerByLinkObject(ALinkObject: TObject): TcxCustomDataController; override;
    function GetDisplayText(ARecordIndex, AItemIndex: Integer): string; override;
    function GetFilterDataValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant; override;
    function GetFilterItemFieldCaption(AItem: TObject): string; override;
    function GetItemSortByDisplayText(AItemIndex: Integer; ASortByDisplayText: Boolean): Boolean; override;
    function GetItemValueSource(AItemIndex: Integer): TcxDataEditValueSource; override;
    procedure UpdateData; override;
    procedure ReloadStorage;
    procedure AdjustActiveRange(aList: TBoldList = nil; aItem: integer = -1); override;
    procedure CollectVisibleRecords(var aList: TBoldList);
    procedure PreFetchColumns(AList: TBoldList; AItem: integer = -1); override;
//    procedure DoGroupingChanged; override;
//    procedure DoSortingChanged; override;
    // Master-Detail: Grid Notifications
    procedure SetMasterRelation(AMasterRelation: TcxCustomDataRelation; AMasterRecordIndex: Integer); override;
    procedure SetValueTypeAndProperties(aMember: TBoldMemberRtInfo; aItem: TcxCustomGridTableItem; aChangeProperties: boolean = true); overload;
    procedure SetValueTypeAndProperties(aElementTypeInfo: TBoldElementTypeInfo; aItem: TcxCustomGridTableItem; aChangeProperties: boolean = true); overload;
    procedure ForEachRow(ASelectedRows: Boolean; AProc: TcxDataControllerEachRowProc); override;
    property GridView: TcxCustomGridTableView read GetGridViewValue;
    property Controller: TcxCustomGridTableController read GetController;
  published
    property BoldProperties;
    property BoldHandle;
    property BoldHandleIndexLock;
    property BoldAutoColumns;
//    property OnBeforeLoad;
    property OnAfterLoad;
    property LoadAll;

    property Filter;
    property Options;
    property Summary;
    property OnAfterCancel;
    property OnAfterDelete;
    property OnAfterInsert;
    property OnAfterPost;
    property OnBeforeCancel;
    property OnBeforeDelete;
    property OnBeforeInsert;
    property OnBeforePost;
    property OnNewRecord;
    property OnCompare;
    property OnDataChanged;
    property OnDetailCollapsing;
    property OnDetailCollapsed;
    property OnDetailExpanding;
    property OnDetailExpanded;
    property OnFilterRecord;
    property OnGroupingChanged;
    property OnRecordChanged;
    property OnSortingChanged;
  end;

  TcxGridItemBoldDataBinding = class(TcxGridItemDataBinding)
  private
    fBoldProperties: TBoldVariantFollowerController;
    fSubscriber: TBoldPassthroughSubscriber;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    function GetDataController: TcxGridBoldDataController;
    function GetBoldProperties: TBoldVariantFollowerController;
    procedure SetBoldProperties(Value: TBoldVariantFollowerController);
  protected
    function GetDefaultValueTypeClass: TcxValueTypeClass; override;
    procedure Init; override;
    procedure Remove;
  public
    constructor Create(AItem: TcxCustomGridTableItem); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DataController: TcxGridBoldDataController read GetDataController;
  published
    property BoldProperties: TBoldVariantFollowerController read GetBoldProperties write SetBoldProperties;
  end;

  TcxGridBoldColumn = class(TcxGridColumn, IBoldAwareViewItem, IcxStoredObject)
  private
    function GetDataBinding: TcxGridItemBoldDataBinding;
    procedure SetDataBinding(Value: TcxGridItemBoldDataBinding);
  protected
    // IcxStoredObject
    function GetProperties(AProperties: TStrings): Boolean;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;
    function CalculateBestFitWidth: Integer; override;
    procedure VisibleChanged; override;
  public
    destructor Destroy; override;
  published
    property DataBinding: TcxGridItemBoldDataBinding read GetDataBinding write SetDataBinding implements IBoldAwareViewItem;
  end;

  TcxBoldDataControllerSearch = class(TcxDataControllerSearch)
  public
  // the sole purpose of these overrides is to ensure range (fetch in 1 pass)
{$IFDEF BOLD_DELPHI25_OR_LATER}
    function Locate(AItemIndex: Integer; const ASubText: string; AIsAnywhere: Boolean = False; ASyncSelection: Boolean = True): Boolean; override;
    function LocateNext(AForward: Boolean; AIsAnywhere: Boolean = False; ASyncSelection: Boolean = True): Boolean; override;
{$ELSE}
    function Locate(AItemIndex: Integer; const ASubText: string; AIsAnywhere: Boolean = False): Boolean; override;
    function LocateNext(AForward: Boolean; AIsAnywhere: Boolean = False): Boolean; override;
{$ENDIF}
  end;

  TcxGridBoldCardsViewInfo = class(TcxGridCardsViewInfo)
  protected
  end;

  TcxGridBoldCardViewViewInfo = class(TcxGridCardViewViewInfo)
  protected
    function GetSiteClass: TcxGridSiteClass; override;
    function GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass; override;
  end;

  TcxGridBoldCardView = class(TcxGridCardView, IBoldAwareView, IBoldValidateableComponent)
  private
    function GetDataController: TcxGridBoldDataController;
    procedure SetDataController(Value: TcxGridBoldDataController);
    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;
    // IBoldAwareView
    function GetItemCount: Integer;
    function GetItem(Index: Integer): IBoldAwareViewItem;
    function GetSelection: TBoldList;
    function GetCurrentBoldObject: TBoldObject;
    function GetCurrentIndex: integer;
    function GetCurrentElement: TBoldElement;
  protected
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetControllerClass: TcxCustomGridControllerClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
    function DoEditing(AItem: TcxCustomGridTableItem): Boolean; override;
    function DoCellDblClick(ACellViewInfo: TcxGridTableDataCellViewInfo;
      AButton: TMouseButton; AShift: TShiftState): Boolean; override;
    procedure DoSelectionChanged; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items[Index: Integer]: IBoldAwareViewItem read GetItem; default;
    property ItemCount: Integer read GetItemCount;
    property Selection: TBoldList read GetSelection;
    property CurrentBoldObject: TBoldObject read GetCurrentBoldObject;
    property CurrentElement: TBoldElement read GetCurrentElement;
    property CurrentIndex: integer read GetCurrentIndex;
  published
    property DataController: TcxGridBoldDataController read GetDataController write SetDataController;
  end;

  TcxGridBoldCardViewRow = class(TcxGridCardViewRow, IBoldAwareViewItem)
  private
    function GetDataBinding: TcxGridItemBoldDataBinding;
    procedure SetDataBinding(Value: TcxGridItemBoldDataBinding);
  protected
    // IcxStoredObject
    {function GetStoredProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;}
    function CalculateBestFitWidth: Integer; override;
    procedure VisibleChanged; override;
  public
    destructor Destroy; override;
  published
    property DataBinding: TcxGridItemBoldDataBinding read GetDataBinding write SetDataBinding implements IBoldAwareViewItem;
  end;
(*
  TcxGridBoldChartDataController = class(TcxBoldDataController, {TcxGridBoldDataController} IcxCustomGridDataController,
    IcxGridChartViewItemsProvider)
  private
    { IcxGridChartViewItemsProvider }
    function IcxGridChartViewItemsProvider.GetItem = GetChartItem;
    function GetChartItem(AItemClass: TcxGridChartItemClass; AIndex: Integer): TcxGridChartItem;
    procedure GetItemCaptions(AItemClass: TcxGridChartItemClass; ACaptions: TStringList);
    procedure InitItem(AItem: TcxGridChartItem; AIndex: Integer);
    procedure GetValidValueFields(AItemClass: TcxGridChartItemClass; AFields: TList);
    { IcxCustomGridDataController }
    procedure AssignData(ADataController: TcxCustomDataController);
    procedure CreateAllItems(AMissingItemsOnly: Boolean);
    procedure DeleteAllItems;
    procedure GetFakeComponentLinks(AList: TList);
    function HasAllItems: Boolean;
    function IsDataChangeable: Boolean;
    function SupportsCreateAllItems: Boolean;
  published
    property Options;
//    property OnAfterSummary: TcxAfterSummaryEvent read GetOnAfterSummary write SetOnAfterSummary;
    property OnCompare;
    property OnDataChanged;
    property OnFilterRecord;
//    property OnSummary: TcxSummaryEvent read GetOnSummary write SetOnSummary;
  end;

  TcxGridBoldChartItemDataBinding = class(TcxGridChartItemDataBinding)
  private
    fBoldProperties: TBoldVariantFollowerController;
    procedure SetBoldProperties(Value: TBoldVariantFollowerController);
    function GetDataController: TcxGridBoldChartDataController;
  public
    constructor Create(AGridView: TcxGridChartView; AIsValue: Boolean;
      ADefaultValueTypeClass: TcxValueTypeClass); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DataController: TcxGridBoldChartDataController read GetDataController;
    property BoldProperties: TBoldVariantFollowerController read fBoldProperties write SetBoldProperties;
  end;

  TcxGridBoldChartCategories = class(TcxGridChartCategories)
  private
    function GetDataBinding: TcxGridBoldChartItemDataBinding;
    procedure SetDataBinding(Value: TcxGridBoldChartItemDataBinding);
  published
    property DataBinding: TcxGridBoldChartItemDataBinding read GetDataBinding write SetDataBinding;
  end;

  TcxGridBoldChartDataGroup = class(TcxGridChartDataGroup)
  private
    function GetDataBinding: TcxGridBoldChartItemDataBinding;
    procedure SetDataBinding(Value: TcxGridBoldChartItemDataBinding);
  published
    property DataBinding: TcxGridBoldChartItemDataBinding read GetDataBinding write SetDataBinding;
  end;

  TcxGridBoldChartSeries = class(TcxGridChartSeries)
  private
    function GetDataBinding: TcxGridBoldChartItemDataBinding;
    procedure SetDataBinding(Value: TcxGridBoldChartItemDataBinding);
  published
    property DataBinding: TcxGridBoldChartItemDataBinding read GetDataBinding write SetDataBinding;
  end;

  TcxBoldGridChartView = class(TcxGridChartView, IBoldAwareView, IBoldValidateableComponent)
  private
    function GetCategories: TcxGridBoldChartCategories;
    function GetDataController: TcxGridBoldChartDataController;
    function GetDataGroup(Index: Integer): TcxGridBoldChartDataGroup;
    function GetSeries(Index: Integer): TcxGridBoldChartSeries;
    procedure SetCategories(Value: TcxGridBoldChartCategories);
    procedure SetDataController(Value: TcxGridBoldChartDataController);
    procedure SetDataGroup(Index: Integer; Value: TcxGridBoldChartDataGroup);
    procedure SetSeries(Index: Integer; Value: TcxGridBoldChartSeries);
    procedure ClearItems;
    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean;
    // IBoldAwareView
    function GetItemCount: Integer;
    function GetItem(Index: Integer): IBoldAwareViewItem;
    function GetSelection: TBoldList;
    property Selection: TBoldList read GetSelection;
  protected
    function GetCategoriesClass: TcxGridChartCategoriesClass; override;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetItemDataBindingClass: TcxGridChartItemDataBindingClass; override;

    function FindItemByFieldName(AItemClass: TcxGridChartItemClass; const AFieldName: string): TcxGridChartItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items[Index: Integer]: IBoldAwareViewItem read GetItem; default;
    property ItemCount: Integer read GetItemCount;

    function CreateDataGroup: TcxGridBoldChartDataGroup;
    function FindDataGroupByFieldName(const AFieldName: string): TcxGridBoldChartDataGroup;
    function GetDataGroupClass: TcxGridChartDataGroupClass; override;
    property DataGroups[Index: Integer]: TcxGridBoldChartDataGroup read GetDataGroup write SetDataGroup;

    function CreateSeries: TcxGridBoldChartSeries;
    function FindSeriesByFieldName(const AFieldName: string): TcxGridBoldChartSeries;
    function GetSeriesClass: TcxGridChartSeriesClass; override;
    property Series[Index: Integer]: TcxGridBoldChartSeries read GetSeries write SetSeries;
  published
    property Categories: TcxGridBoldChartCategories read GetCategories write SetCategories;
    property DataController: TcxGridBoldChartDataController read GetDataController write SetDataController;
  end;
*)

  TcxGridBoldBandedColumn = class(TcxGridBandedColumn, IBoldAwareViewItem {,IcxStoredObject})
  private
    function GetDataBinding: TcxGridItemBoldDataBinding;
    procedure SetDataBinding(Value: TcxGridItemBoldDataBinding);
//    procedure HyperLinkClick(Sender: TObject);
  protected
    // IcxStoredObject
//    function GetProperties(AProperties: TStrings): Boolean;
//    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
//    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;
    function CalculateBestFitWidth: Integer; override;
    procedure VisibleChanged; override;
  public
    destructor Destroy; override;
  published
    property DataBinding: TcxGridItemBoldDataBinding read GetDataBinding write SetDataBinding implements IBoldAwareViewItem;
  end;

  TcxGridBoldBandedRowsViewInfo = class(TcxGridBandedRowsViewInfo)
  protected
  end;

  TcxGridBoldBandedTableViewInfo = class(TcxGridBandedTableViewInfo)
  protected
    function GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass; override;
    function GetSiteClass: TcxGridSiteClass; override;
    procedure Calculate; override;
  end;

  TcxGridBoldBandedTableView = class(TcxGridBandedTableView, IBoldAwareView, IBoldValidateableComponent)
  private
    FPrevFocusedRecordIndex: integer;
    FPrevFocusedDataRecordIndex: integer;  
    procedure HookDragDrop;
    function GetDataController: TcxGridBoldDataController;
    procedure SetDataController(Value: TcxGridBoldDataController);
    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;
    // IBoldAwareView
    function GetItemCount: Integer;
    function GetItem(Index: Integer): IBoldAwareViewItem;
    function GetSelection: TBoldList;
    function GetCurrentBoldObject: TBoldObject;
    function GetCurrentIndex: integer;
    function GetCurrentElement: TBoldElement;
  protected
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetControllerClass: TcxCustomGridControllerClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
    function DoCellDblClick(ACellViewInfo: TcxGridTableDataCellViewInfo;
      AButton: TMouseButton; AShift: TShiftState): Boolean; override;
    function DoEditing(AItem: TcxCustomGridTableItem): Boolean; override;
    procedure DoSelectionChanged; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;
    procedure DoChanged(AChangeKind: TcxGridViewChangeKind); override;
    procedure DoFocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex,
      APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
      ANewItemRecordFocusingChanged: Boolean); override;    
{$IFDEF DelayOnFocusedRecordChange}
    procedure InheritedDoFocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex, // a bit ugly
      APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
      ANewItemRecordFocusingChanged: Boolean);
{$ENDIF}    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items[Index: Integer]: IBoldAwareViewItem read GetItem; default;
    property ItemCount: Integer read GetItemCount;
    property Selection: TBoldList read GetSelection;
    property CurrentBoldObject: TBoldObject read GetCurrentBoldObject;
    property CurrentElement: TBoldElement read GetCurrentElement;
    property CurrentIndex: integer read GetCurrentIndex;
  published
    property DataController: TcxGridBoldDataController read GetDataController write SetDataController;
  end;

  TcxBoldGridSite = class(TcxGridSite)
  protected
    procedure WndProc(var Message: TMessage); override;
  end;

  TLinkClickEvent = procedure(Sender: TObject; aElement: TBoldElement) of object;

  TcxBoldGridRowsViewInfo = class(TcxGridRowsViewInfo)
  protected
  end;

  TcxGridBoldTableViewInfo = class(TcxGridTableViewInfo)
  protected
    function GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass; override;
    function GetSiteClass: TcxGridSiteClass; override;
    procedure Calculate; override;
  end;

  TcxGridBoldTableView = class(TcxGridTableView, IBoldAwareView, IBoldValidateableComponent)
  private
    fOnLinkClick: TLinkClickEvent;
    FPrevFocusedRecordIndex: integer;
    FPrevFocusedDataRecordIndex: integer;
    procedure HookDragDrop;
    function GetDataController: TcxGridBoldDataController;
    procedure SetDataController(Value: TcxGridBoldDataController);
    function GetSelection: TBoldList;
    function GetCurrentBoldObject: TBoldObject;
    function GetCurrentIndex: integer;
    function GetCurrentElement: TBoldElement;
    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;
    // IBoldAwareView
    function GetItemCount: Integer;
    function GetItem(Index: Integer): IBoldAwareViewItem;
    function GetFake: TNotifyEvent;
    procedure SetFake(const Value: TNotifyEvent);
  protected
    // IcxStoredObject
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;

    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetControllerClass: TcxCustomGridControllerClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
    function DoEditing(AItem: TcxCustomGridTableItem): Boolean; override;
    procedure DoEditKeyPress(AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit;
      var Key: Char); override;
    function DoCellDblClick(ACellViewInfo: TcxGridTableDataCellViewInfo;
      AButton: TMouseButton; AShift: TShiftState): Boolean; override;
    procedure DoSelectionChanged; override;
    procedure DoChanged(AChangeKind: TcxGridViewChangeKind); override;
    procedure DoFocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex,
      APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
      ANewItemRecordFocusingChanged: Boolean); override;
{$IFDEF DelayOnFocusedRecordChange}
    procedure InheritedDoFocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex, // a bit ugly
      APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
      ANewItemRecordFocusingChanged: Boolean);
{$ENDIF}
    procedure DoItemsAssigned; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;
    procedure DoCustomDrawCell(ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
      var ADone: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items[Index: Integer]: IBoldAwareViewItem read GetItem; default;
    property ItemCount: Integer read GetItemCount;
    property Selection: TBoldList read GetSelection;
    property CurrentBoldObject: TBoldObject read GetCurrentBoldObject;
    property CurrentElement: TBoldElement read GetCurrentElement;
    property CurrentIndex: integer read GetCurrentIndex;
  published
    property DataController: TcxGridBoldDataController read GetDataController write SetDataController;
    property DragMode;
    property OnDelete: TNotifyEvent read GetFake write SetFake;
    property OnInsert: TNotifyEvent read GetFake write SetFake;

    property OnLinkClick: TLinkClickEvent read fOnLinkClick write fOnLinkClick;
  end;

  TcxGridBoldLayoutView = class(TcxGridLayoutView, IBoldAwareView, IBoldValidateableComponent)
  private
    function GetDataController: TcxGridBoldDataController;
//    function GetItem(Index: Integer): TcxGridBoldLayoutViewItem;
    procedure SetDataController(Value: TcxGridBoldDataController);
//    procedure SetItem(Index: Integer; Value: TcxGridBoldLayoutViewItem);
    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;
    // IBoldAwareView
    function GetItemCount: Integer;
    function GetItem(Index: Integer): IBoldAwareViewItem;
    function GetSelection: TBoldList;
    function GetCurrentBoldObject: TBoldObject;
    function GetCurrentIndex: integer;
    function GetCurrentElement: TBoldElement;
  protected
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateItem: TcxGridBoldLayoutViewItem;
//    property Items[Index: Integer]: TcxGridBoldLayoutViewItem read GetItem write SetItem;
    property Items[Index: Integer]: IBoldAwareViewItem read GetItem; default;
    property ItemCount: Integer read GetItemCount;
    property Selection: TBoldList read GetSelection;
    property CurrentBoldObject: TBoldObject read GetCurrentBoldObject;
    property CurrentElement: TBoldElement read GetCurrentElement;
    property CurrentIndex: integer read GetCurrentIndex;
  published
    property DataController: TcxGridBoldDataController read GetDataController write SetDataController;
  end;

  TcxGridBoldLayoutViewItem = class(TcxGridLayoutViewItem, IBoldAwareViewItem)
  private
    function GetDataBinding: TcxGridItemBoldDataBinding;
    procedure SetDataBinding(Value: TcxGridItemBoldDataBinding);
  public
    destructor Destroy; override;
  published
    property DataBinding: TcxGridItemBoldDataBinding read GetDataBinding write SetDataBinding implements IBoldAwareViewItem;
  end;


  TcxBoldCustomDataProvider = class(TcxCustomDataProvider)
  protected
    function GetValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant; override;
    procedure SetValue(ARecordIndex: Integer; AField: TcxCustomDataField; const Value: Variant); override;
    function CanInsert: Boolean; override;
    function CanDelete: Boolean; override;
    procedure DeleteRecords(AList: TList); override;
    function SetEditValue(ARecordIndex: Integer; AField: TcxCustomDataField; const AValue: Variant; AEditValueSource: TcxDataEditValueSource): Boolean; override;
    function IsActiveDataSet: Boolean; override;
  end;

  TcxGridBoldTableController = class(TcxGridTableController)
  protected
{    procedure FocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex,
      APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
      ANewItemRecordFocusingChanged: Boolean); override;
}
    function GetEditingControllerClass: TcxGridEditingControllerClass; override;
  public
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  end;

  TcxGridBoldBandedTableController = class(TcxGridBandedTableController)
  protected
    function GetEditingControllerClass: TcxGridEditingControllerClass; override;
  public
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  end;

  TcxGridBoldEditingController = class(TcxGridTableEditingController)
  protected
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure EditChanged(Sender: TObject); override;
    procedure EditExit(Sender: TObject); override;
  public
    procedure HideEdit(Accept: Boolean); override;
  end;

  TcxGridBoldCardViewController = class(TcxGridCardViewController)
  protected
    function GetEditingControllerClass: TcxGridEditingControllerClass; override;
//    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
//    procedure EditChanged(Sender: TObject); override;
  public
  end;

  TcxGridBoldCardEditingController = class(TcxGridEditingController)
  protected
//    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure EditChanged(Sender: TObject); override;
//    procedure EditExit(Sender: TObject); override;
  public
//    procedure HideEdit(Accept: Boolean); override;
  end;


  TcxBoldDataSourceClass = class of TcxBoldDataSource;
{$IFDEF BoldDevExLog}
  TcxBoldGridLogProc = procedure(aMessage: string; aCategory: string = '') of object;
{$ENDIF}

var
  cxBoldDataSourceClass: TcxBoldDataSourceClass = TcxBoldDataSource;
{$IFDEF BoldDevExLog}
  cxBoldGridLogProc: TcxBoldGridLogProc;
{$ENDIF}

{$IFDEF BoldDevExLog}
procedure _Log(aMessage: string; aCategory: string = '');
{$ENDIF}

implementation

uses
  Dialogs,
  Forms,
  Graphics,
  Math,
  SysUtils,
  Variants,
  Windows,
{$IFDEF BOLD_DELPHI16_OR_LATER}UiTypes,{$ENDIF}

  BoldAFP,
  BoldAttributes,
  BoldBase,
  BoldCommonBitmaps,
  BoldControlPackDefs,
  BoldDefs,
  BoldDerivedValueSet,
  BoldElementList,
  BoldEnvironment,
  BoldGui,
  BoldId,
  BoldListControlPack,
  BoldListHandle,
  BoldMLAttributes,
  BoldOcl,
  BoldQueue,
  BoldReferenceHandle,
  BoldSystemPersistenceHandler,
  BoldValueSpaceInterfaces,

  cxCalendar,
  cxCheckBox,
  cxClasses,
  cxControls,
  cxCurrencyEdit,
  cxFilterConsts,
  cxFilterControlUtils,
  cxGridCommon,
  cxGridLevel,
  cxGridRows,
  cxHyperLinkEdit,
  cxImage,
  cxSpinEdit,
  cxTextEdit,
  cxTimeEdit,

{$IFDEF SpanFetch}
  AttracsSpanFetchManager,
{$ENDIF}
  BoldGuard, TypInfo;

const
  cOCLConstraint = 'constraints->select(c|not c)->size = 0';
  beSystemDestroying = 100;
  beSelectionDestroying = 101;

type
  EcxGridBoldSupport = class(Exception);
  TcxCustomDataControllerAccess = class(TcxCustomDataController);
  TcxGridTableControllerAccess = class(TcxGridTableController);
  TBoldListHandleFollowerAccess = class(TBoldListHandleFollower);
  TBoldFollowerAccess = class(TBoldFollower);
  TBoldFollowerControllerAccess = class(TBoldFollowerController);
  TcxCustomGridTableItemAccess = class(TcxCustomGridTableItem);
  TcxGridLevelAccess = class(TcxGridLevel);
  TcxCustomDataProviderAccess = class(TcxCustomDataProvider);
  TcxCustomGridRecordAccess = class(TcxCustomGridRecord);
  TcxCustomGridTableViewAccess = class(TcxCustomGridTableView);
  TcxCustomDataControllerInfoAccess = class(TcxCustomDataControllerInfo);
  TBoldQueueableAccess = class(TBoldQueueable);

{$IFDEF BoldDevExLog}
procedure _Log(aMessage: string; aCategory: string = '');
begin
  if Assigned(cxBoldGridLogProc) then
    cxBoldGridLogProc(aMessage, aCategory);
end;
{$ENDIF}

function InternalSetValue(aFollower: TBoldFollower; const AValue: Variant): boolean;
var
  lController: TBoldVariantFollowerController;
begin
  result := false;
  lController := aFollower.Controller as TBoldVariantFollowerController;
  if VarIsNull(aValue) then
    lController.MayHaveChanged('', aFollower)
  else
    lController.MayHaveChanged(aValue, aFollower);
end;

{ TcxGridBoldDataController }

procedure TcxGridBoldDataController.SetValueTypeAndProperties(
  aElementTypeInfo: TBoldElementTypeInfo; aItem: TcxCustomGridTableItem;
  aChangeProperties: boolean);
var
  lAttributeClass: TClass;
  lValueType: string;
  lBAValueSet: TBAValueSet;
  lBoldAttributeTypeInfo: TBoldAttributeTypeInfo;
  i: integer;
begin
  lValueType := 'String';
  if aElementTypeInfo is TBoldAttributeTypeInfo then
  begin
    lBoldAttributeTypeInfo := TBoldAttributeTypeInfo(aElementTypeInfo);
    lAttributeClass := lBoldAttributeTypeInfo.AttributeClass;
    if not Assigned(lAttributeClass) then
    begin
      raise EcxGridBoldSupport.Create('Custom attribute ' + aElementTypeInfo.ModelName + ' is not installed in IDE.');
    end;
    // Blob, ValueSet and Associations map to string
    if lAttributeClass.InheritsFrom(TBAString) then
    begin
      lValueType := 'String';
      if aChangeProperties then
        aItem.PropertiesClass := TcxTextEditProperties;
    end
    else
      if lAttributeClass.InheritsFrom(TBATime) then
      begin
        lValueType := 'DateTime';
        if aChangeProperties then
        begin
          aItem.PropertiesClass := TcxTimeEditProperties;
          (aItem.Properties as TcxTimeEditProperties).Alignment.Horz := taRightJustify;
        end;
      end
      else
        if lAttributeClass.InheritsFrom(TBAMoment) then
        begin
          lValueType := 'DateTime';
          if aChangeProperties then
          begin
            aItem.PropertiesClass := TcxDateEditProperties;
            (aItem.Properties as TcxDateEditProperties).Alignment.Horz := taRightJustify;
          end;
        end
        else
          if lAttributeClass.InheritsFrom(TBABoolean) then
          begin
            lValueType := 'Boolean';
            if aChangeProperties then
              aItem.PropertiesClass := TcxCheckBoxProperties;
          end
          else
            if lAttributeClass.InheritsFrom(TBACurrency) then
            begin
              lValueType := 'Currency';
              if aChangeProperties then
              begin
                aItem.PropertiesClass := TcxCurrencyEditProperties;
                (aItem.Properties as TcxCurrencyEditProperties).Alignment.Horz := taRightJustify;
              end;
            end
            else
              if lAttributeClass.InheritsFrom(TBANumeric) then
              begin
                if aChangeProperties then
                begin
                  aItem.PropertiesClass := TcxSpinEditProperties;
                  (aItem.properties as TcxSpinEditProperties).SpinButtons.Visible := false;
                  (aItem.Properties as TcxSpinEditProperties).Alignment.Horz := taRightJustify;
                  (aItem.Properties as TcxSpinEditProperties).Increment := 0;
                  (aItem.Properties as TcxSpinEditProperties).LargeIncrement := 0;                                    
                end;
                if lAttributeClass.InheritsFrom(TBAFloat) then
                begin
                  lValueType := 'Float';
                  if aItem.properties is TcxSpinEditProperties then
                    (aItem.properties as TcxSpinEditProperties).ValueType := vtFloat;
                end
                else
                  if lAttributeClass.InheritsFrom(TBASMallInt) then
                  begin
                    lValueType := 'Smallint';
                    if aItem.properties is TcxSpinEditProperties then
                      (aItem.properties as TcxSpinEditProperties).ValueType := vtInt;
                  end
                  else
                    if lAttributeClass.InheritsFrom(TBAWord) then
                    begin
                      lValueType := 'Word';
                      if aItem.properties is TcxSpinEditProperties then
                        (aItem.properties as TcxSpinEditProperties).ValueType := vtInt;
                    end
                    else
                      if lAttributeClass.InheritsFrom(TBAInteger) then
                      begin
                        lValueType := 'Integer';
                        if aItem.properties is TcxSpinEditProperties then
                          (aItem.properties as TcxSpinEditProperties).ValueType := vtInt;
                      end;
              end
              else
                {$IFDEF IEJpegImage}
                if lAttributeClass.InheritsFrom(TBABlobImageJPEG) then
                begin
                  lValueType := '';
                  if aChangeProperties then
                  begin
                    aItem.PropertiesClass := TcxImageProperties;
                    (aItem.properties as TcxImageProperties).GraphicClassName := 'TIEJpegImage';
                  end;
                end
                else
                  {$ENDIF}
                  if lAttributeClass.InheritsFrom(TBABlobImageBMP) or lAttributeClass.InheritsFrom(TBABlobImageJPEG) then
                  begin
                    lValueType := '';
                    if aChangeProperties then
                      aItem.PropertiesClass := TcxImageProperties;
                  end
                  else
                    if aChangeProperties and lAttributeClass.InheritsFrom(TBAValueSet) then
                    begin
                      if lAttributeClass.InheritsFrom(TBALanguage) or lAttributeClass.InheritsFrom(TBADerivedValueSetValueList) and GridView.IsDesigning then
                      begin
                        MessageDlg(Format('Combo values for ''%s'' can only be fetched at run time', [aElementTypeInfo.expressionName]), mtError, [mbOk], 0);
                      end
                      else
                      begin
                        lBAValueSet := TBoldMemberFactory.CreateMemberFromBoldType(aElementTypeInfo) as TBAValueSet;
                        try
                          aItem.PropertiesClass := TcxComboBoxProperties;
                          (aItem.Properties as TcxComboBoxProperties).Items.Clear;
                          for i := 0 to lBAValueSet.Values.Count - 1 do
                          begin
                            (aItem.Properties as TcxComboBoxProperties).Items.Add(lBAValueSet.Values[i].AsString);
                          end;
                          (aItem.Properties as TcxComboBoxProperties).DropDownListStyle := lsEditFixedList;
                          (aItem.Properties as TcxComboBoxProperties).DropDownRows := lBAValueSet.Values.Count;
                        finally
                          lBAValueSet.Free;
                        end;
                      end;
                    end;
  end;
  aItem.DataBinding.ValueType := lValueType;
end;

procedure TcxGridBoldDataController.SetValueTypeAndProperties(aMember: TBoldMemberRtInfo; aItem: TcxCustomGridTableItem; aChangeProperties: boolean);
begin
  SetValueTypeAndProperties(aMember.boldType, aItem);
end;

procedure TcxGridBoldDataController.ChangeValueTypeClass(AItemIndex: Integer;
  AValueTypeClass: TcxValueTypeClass);
begin
  // this code is copied from inherited TcxCustomDataController.ChangeValueTypeClass and RestructData is commented out
  // Commenting out RestructData causes other problems.
  // Probably it has to be only called once, not for each item. That should be implemented
  CheckItemRange(AItemIndex);
  if GetItemValueTypeClass(AItemIndex) <> AValueTypeClass then
  begin
    Fields[AItemIndex].ValueTypeClass := AValueTypeClass;
//    if {IsProviderMode and} not TcxCustomGridTableViewAccess(GridView).IsAssigningItems then
//      RestructData;
    DataControllerInfo.UpdateField(Fields[AItemIndex]);
    DoValueTypeClassChanged(AItemIndex);
  end;
end;

procedure TcxGridBoldDataController.CheckDataSetCurrent;
var
  i: integer;
  lChanged: boolean;
  lState : TKeyboardState;
begin
  inherited;
  if Assigned(BoldHandle) and Assigned(CustomDataSource) and (FSkipSyncFocusedRecord = 0) and not (csDestroying in GridView.ComponentState) then
  begin
    i := FocusedRecordIndex;
    if (i >= RecordCount) then
    begin
      FocusedRecordIndex := -1;
      i := -1;
    end;
    lChanged := (i <> Follower.CurrentIndex) {or (i <> BoldHandle.CurrentIndex)};
    if not lChanged and (i <> BoldHandle.CurrentIndex) then
      lChanged := true;

    if lChanged then
    begin
      {$IFDEF BoldDevExLog}
      if GridView <> nil then
        _Log(GridView.Name + ':CheckDataSetCurrent', className)
      else
        _Log(':CheckDataSetCurrent', className);
      {$ENDIF}
      ConnectEvents(false);
      inc(FSkipMakeCellUptoDate);
      BeginUpdate;
      try
//        TBoldQueueable.DisplayAll;
        fBoldHandleFollower.SetFollowerIndex(i);
        GetKeyboardState(lState);
        if i <> -1 then
        begin
          i := GetRowIndexByRecordIndex(i, false);
        end;
        if not (((lState[VK_CONTROL] and 128) <> 0) or ((lState[VK_SHIFT] and 128) <> 0)) and ((i = -1) or not IsRowSelected(i)) then
        begin
          Controller.ClearSelection;
          if Assigned(Controller.FocusedRecord) and not MultiSelect then
            Controller.FocusedRecord.Selected := true;
        end;
      finally
        DisplayFollowers;
        ConnectEvents(True);
        dec(FSkipMakeCellUptoDate);
        SelectionChanged;
        EndUpdate;
      end;
    end;
  end;
end;

procedure TcxGridBoldDataController.ConnectEvents(AConnect: boolean);
var
  i: integer;
begin
  for i := 0 to ItemCount - 1 do
  begin
    if AConnect then
      BoldPropertiesFromItem(i).AfterMakeUptoDate := _AfterMakeCellUptoDate
    else
      BoldPropertiesFromItem(i).AfterMakeUptoDate := nil;
  end;
  if AConnect then
  begin
    fBoldProperties.AfterMakeUptoDate := _AfterMakeListUptoDate;
    fBoldProperties.BeforeMakeUptoDate := _BeforeMakeListUptoDate;
    fBoldProperties.OnAfterInsertItem := _InsertRow;
    fBoldProperties.OnAfterDeleteItem := _DeleteRow;
    fBoldProperties.OnReplaceitem := _ReplaceRow
  end
  else
  begin
    fBoldProperties.AfterMakeUptoDate := nil;
    fBoldProperties.BeforeMakeUptoDate := nil;
    fBoldProperties.OnAfterInsertItem := nil;
    fBoldProperties.OnAfterDeleteItem := nil;
    fBoldProperties.OnReplaceitem := nil;
  end;
end;

procedure TcxGridBoldDataController.ConstraintColumnCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
  ARect: TRect;
  lConstraintBitmap: Graphics.TBitmap;
  lValue: Variant;
begin
  if AViewInfo is TcxGridCardRowCaptionViewInfo then exit;
  with AViewInfo do
  begin
    ARect := Bounds;
//    ACanvas.Brush.Style := bsClear;
    ACanvas.FillRect(ARect, ACanvas.Brush.Color);
    if AViewInfo.RecordViewInfo.Index = -1 then
    begin
      // draw nothing
    end
    else
    begin
      lValue := Value;
      if not VarIsNull(lValue) and lValue then
        lConstraintBitmap := bmpBoldGridConstraint_true
      else
        lConstraintBitmap := bmpBoldGridConstraint_false;
      lConstraintBitmap.Transparent := true;
      ACanvas.Draw(ARect.Left + 4, ARect.Top + 4, lConstraintBitmap);
    end;
  end;
  ADone := true;
end;

procedure TcxGridBoldDataController.ColumnCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
  lBoldVariantFollowerController: TBoldVariantFollowerController;
  lColor: TColor;
  lCellFollower: TBoldFollower;
begin
  inc(FSkipMakeCellUptoDate);
  try
    if not AViewInfo.RecordViewInfo.Focused then
    begin
      lCellFollower := CellFollowers[AViewInfo.RecordViewInfo.GridRecord.RecordIndex, AViewInfo.Item.Id];
      if Assigned(lCellFollower) then
      begin
        lBoldVariantFollowerController := lCellFollower.Controller as TBoldVariantFollowerController;
        lBoldVariantFollowerController.SetColor(lColor, ACanvas.Brush.Color, lCellFollower);
        lBoldVariantFollowerController.SetFont(ACanvas.Font, ACanvas.Font, lCellFollower);
        if LColor > -1 then
          ACanvas.Brush.Color := lColor;
      end;
    end;
  finally
    dec(FSkipMakeCellUptoDate);
  end;
end;

procedure TcxGridBoldDataController.GetCellHint(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  ACellViewInfo: TcxGridTableDataCellViewInfo; const AMousePos: TPoint;
  var AHintText: TCaption; var AIsHintMultiLine: Boolean; var AHintTextRect: TRect);
var
  lIE: TBoldIndirectElement;
begin
  if VarIsType(ACellViewInfo.Value, varBoolean) and (not ACellViewInfo.Value) then
//  if (not VarIsNull(ACellViewInfo.Value)) and (not ACellViewInfo.Value) then
  begin
    lIE := TBoldIndirectElement.Create;
    try
      Follower.SubFollowers[ARecord.RecordIndex].Element.EvaluateExpression('constraints->select(c|not c)', lIE);
      AHintText := (lIE.Value as TBoldList).AsCommaText(false, 11);
    finally
      lIE.free;
    end;
  end;
end;

procedure TcxGridBoldDataController.CreateAllItems(
  AMissingItemsOnly: Boolean);
  
function ClassTypeHasConstraints(aBoldClassTypeInfo: TBoldClassTypeInfo): boolean;
var
  lBoldClassTypeInfo: TBoldClassTypeInfo;
begin
  lBoldClassTypeInfo := aBoldClassTypeInfo;
  repeat
  // BoldClassTypeInfo.ConstraintCount doesn't include inherited constraints so we have to iterate
    result := lBoldClassTypeInfo.ConstraintCount > 0;
    lBoldClassTypeInfo := lBoldClassTypeInfo.SuperClassTypeInfo;
  until result or (lBoldClassTypeInfo = nil);
end;

var
  I: Integer;
  lListElementType: TBoldElementTypeInfo;
  lClasstypeInfo: TBoldClassTypeInfo;
  lMember: TBoldMemberRtInfo;
  lcxCustomGridTableItem: TcxCustomGridTableItem;
begin
  if (BoldHandle = nil) or (BoldHandle.ListElementType = nil) then Exit;
  ShowHourglassCursor;
  fCreatingColumns := true;
  try
    GridView.BeginUpdate;
    BeginUpdateFields;
    try
      lListElementType := BoldHandle.ListElementType;
      if (lListElementType is TBoldClassTypeInfo) then
      begin
        lClassTypeInfo := lListElementType as TBoldClassTypeInfo;
        if ClassTypeHasConstraints(lClassTypeInfo) and (not AMissingItemsOnly or (GetItemByExpression(cOCLConstraint) = nil)) then
        begin
          // create constraint column
          EnsureConstraintColumn;
        end;
        if (lClassTypeInfo.DefaultStringRepresentation <> '') and (lClassTypeInfo.AllMembers.Count = 0) then
        begin
          if not AMissingItemsOnly or (GetItemByExpression('') = nil) then
          begin
            CreateItem(GridView, '', lClassTypeInfo.ModelName, 'String', 'DefaultStringRepresentation');
          end;
        end;
        for i := 0 to lClassTypeInfo.AllMembers.Count - 1 do
        begin
          lMember := lClassTypeInfo.AllMembers[I];
          if (lMember.IsAttribute or (lMember.IsSingleRole and TBoldRoleRTInfo(lMember).IsNavigable)) and not lMember.DelayedFetch then
          begin
            if not AMissingItemsOnly or (GetItemByExpression(lMember.ExpressionName) = nil) then
            begin
              lcxCustomGridTableItem := CreateItem(GridView, lMember.ExpressionName, lMember.ModelName, 'String', lMember.ModelName);
              if lMember.IsSingleRole then
              begin
                lcxCustomGridTableItem.DataBinding.ValueType := 'String';
              end
              else
              begin
                SetValueTypeAndProperties(lMember, lcxCustomGridTableItem);
              end;
            end;
          end;
        end;
      end
      else if (lListElementType is TBoldAttributeTypeInfo) then
      begin
        if not AMissingItemsOnly or (GetItemByExpression('') = nil) then
        begin
          CreateItem(GridView, '', TBoldAttributeTypeInfo(lListElementType).ModelName, 'String', TBoldAttributeTypeInfo(lListElementType).ModelName);
        end;
      end
      else if (lListElementType is TBoldListTypeInfo) then
      begin
        if not AMissingItemsOnly or (GetItemByExpression('') = nil) then
        begin
          CreateItem(GridView, '', 'ClassName', 'String', 'ClassName');
        end;
      end;
      if (GridView.ItemCount = 0) or ((GridView.ItemCount = 1) and ((GridView.Items[0] as IBoldAwareViewItem).BoldProperties.expression = cOCLConstraint)) then
      begin
        CreateItem(GridView, '', lListElementType.asString, 'String', lListElementType.asString);
      end;
    finally
      EndUpdateFields;
      GridView.EndUpdate;
    end;
  finally
    HideHourglassCursor;
    fCreatingColumns := false;
  end;
end;

{
procedure TcxGridBoldDataController.DoGroupingChanged;
begin
  inherited;

end;

procedure TcxGridBoldDataController.DoSortingChanged;
begin
  inherited;

end;
}

function TcxGridBoldDataController.GetController: TcxCustomGridTableController;
begin
  Result := GridView.Controller;
end;

function TcxGridBoldDataController.GetGridViewValue: TcxCustomGridTableView;
begin
  result := TcxCustomGridTableView(GetGridView);
end;

function TcxGridBoldDataController.GetItem(Index: Integer): TObject;
begin
  Result := GridView.Items[Index];
end;

function TcxGridBoldDataController.GetItemByExpression(
  const aExpression: string): TObject;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ItemCount - 1 do
  begin
    if AnsiCompareText(BoldPropertiesFromItem(i).Expression, aExpression) = 0 then
    begin
      Result := GetItem(I);
      Break;
    end;
  end;
end;

function TcxGridBoldDataController.GetItemData(AItem: TObject): Integer;
begin
  if AItem is TcxCustomGridTableItem then
    Result := Integer(TcxCustomGridTableItem(AItem).DataBinding.Data)
  else
    Result := -1;
end;

function TcxGridBoldDataController.GetItemDataBindingClass: TcxGridItemDataBindingClass;
begin
  Result := TcxGridItemBoldDataBinding;
end;

function TcxGridBoldDataController.HasAllItems: Boolean;
begin
  result := false;
end;

function TcxGridBoldDataController.SupportsCreateAllItems: Boolean;
begin
  result := true;
end;

procedure TcxGridBoldDataController.ForEachRow(ASelectedRows: Boolean;
  AProc: TcxDataControllerEachRowProc);
var
  i: integer;
  lList: TBoldList;
  IsObjectList: boolean;
  lWholeList: TBoldList;
  lGuard: IBoldGuard;
begin
  if ASelectedRows then
  begin
    if GetSelectedCount > 0 then
    begin
      lWholeList := BoldList;
      lGuard := TBoldGuard.Create(lList);
      lList := CreateList;
      lList.DuplicateMode := bldmAllow;
      lList.Capacity := GetSelectedCount;
      IsObjectList := (lList is TBoldObjectList) and (lWholeList is TBoldObjectList);
      for i := 0 to GetSelectedCount - 1 do // or Controller.SelectedRecordCount ?
      begin
        if IsObjectList then
          TBoldObjectList(lList).AddLocator( TBoldObjectList(lWholeList).Locators[Controller.SelectedRecords[i].RecordIndex] )
        else
          lList.Add(lWholeList[Controller.SelectedRecords[i].RecordIndex]);
      end;
      AdjustActiveRange(lList);
    end;
  end
  else
  begin
    AdjustActiveRange;
  end;
  inherited;
end;

procedure TcxGridBoldDataController.SetMasterRelation(
  AMasterRelation: TcxCustomDataRelation; AMasterRecordIndex: Integer);
var
  lReferenceHandle: TBoldReferenceHandle;
  lListHandle: TBoldListHandle;
  lGridLevel: TcxGridLevel;
  lPatternView: IBoldAwareView;
  lMasterElement: TBoldElement;
  lBoldAwareView: IBoldAwareView;
begin
  if Assigned(AMasterRelation) and (AMasterRelation.Item is TcxGridLevel) then
  begin
    lGridLevel := (AMasterRelation.Item as TcxGridLevel);
    if lGridLevel.GridView.IsPattern and (lGridLevel.GridView <> GridView) then
    begin
      lBoldAwareView := lGridLevel.GridView.MasterGridView as IBoldAwareView;
      Assert(lBoldAwareView.DataController.BoldHandle.List.count > AMasterRecordIndex);
      lBoldAwareView.DataController.BoldHandle.CurrentIndex := AMasterRecordIndex;
      lMasterElement := lBoldAwareView.DataController.BoldHandle.Value;
      lPatternView := (lGridLevel.GridView as IBoldAwareView);
      lReferenceHandle := TBoldReferenceHandle.Create(GetOwnerOrView);
      lReferenceHandle.Value := lMasterElement;
      lListHandle := TBoldListHandle.Create(GetOwnerOrView);
      lListHandle.RootHandle := lReferenceHandle;
      if Assigned(lPatternView.DataController.BoldHandle) and (lPatternView.DataController.BoldHandle is TBoldListHandle) then
      begin
        lListHandle.expression := (lPatternView.DataController.BoldHandle as TBoldListHandle).Expression;
        lListHandle.mutableListexpression := (lPatternView.DataController.BoldHandle as TBoldListHandle).mutableListexpression;
        lListHandle.name := CreateUniqueName(GetOwnerOrView, GridView, lListHandle, '', 'md_' + lPatternView.DataController.BoldHandle.name);
      end;
      BoldAutoColumns := false; // lPatternView.DataController.BoldAutoColumns;
      BoldHandle := lListHandle;
      GridView.Name := CreateUniqueName(GetOwnerOrView, GetOwnerOrView, GridView, '', lGridLevel.GridView.Name);
    end;
  end;
  inherited;
end;

type
  TcxCustomGridTableControllerHack = class(TcxCustomGridTableController);

procedure TcxGridBoldDataController.SelectionChanged;
var
  i, j: integer;
  lSelectedIndex: integer;
  lFollower: TBoldFollower;
  lCount: integer;
  lList: TBoldList;
  lIndex: integer;
//  lBoldAwareView: IBoldAwareView;
  lSelection: TBoldList;
  lRecordCount: integer;
begin
  {$IFDEF BoldDevExLog}
  _Log(GridView.Name + ':DoSelectionChanged', className);
  {$ENDIF}
//  GridView.GetInterface(IBoldAwareView, lBoldAwareView);
  lSelection := fSelection;// lBoldAwareView.Selection;
  if Assigned(lSelection) then
    lSelection.Clear;
  lFollower := Follower;
  j := Controller.SelectedRecordCount;
  if Assigned(BoldHandle) and Assigned(Follower.Element) and (lFollower.Element is TBoldObjectList) then
  begin
    lList := BoldList;
    if (j > 0) and (j >= lList.Count) then
    begin
      if Assigned(lSelection) then
        lSelection.AddList(lList);
      BoldProperties.SelectAll(lFollower, true);
    end
    else
    begin
      lCount := lList.count;
      lRecordCount := TcxCustomGridTableControllerHack(GridView.Controller).ViewData.RecordCount;
      BoldProperties.SelectAll(lFollower, false);
      for i := 0 to j - 1 do
      begin
        lIndex := GetSelectedRowIndex(i);
        if lIndex < lRecordCount then
        begin
          lSelectedIndex := Controller.SelectedRecords[i].RecordIndex;
          if lSelectedIndex < lCount then
          begin
            if Assigned(lSelection) then
              lSelection.Add(lList[lSelectedIndex]);
            BoldProperties.SetSelected(lFollower, lSelectedIndex, true);
          end;
        end;
      end;
    end;
    if not MultiSelect and (Follower.SubFollowerCount > 0) and (Follower.CurrentIndex <> -1) and (Follower.CurrentIndex < Follower.SubFollowerCount) then
    begin
      if Assigned(lSelection) and Assigned(Follower.CurrentSubFollower) then
        lSelection.Add(Follower.CurrentSubFollower.Element);
      BoldProperties.SetSelected(lFollower, Follower.CurrentIndex, true);
    end;
  end
  else
  begin
    if Assigned(lSelection) then
      lSelection.Clear;
    BoldProperties.SelectAll(lFollower, false);
  end;
  {$IFDEF DisplayAll}
//  TBoldQueueable.DisplayAll;
  {$ENDIF}
end;

function TcxGridBoldDataController.DoEditing(AItem: TcxCustomGridTableItem): Boolean;
var
  lRecord: integer;
  lFollower: TBoldFollower;
  lIcxBoldEditProperties: IcxBoldEditProperties;
begin
  lRecord := RecNo;
  Assert(lRecord <> -1, 'lRecord <> -1');
  lFollower := CellFollowers[lRecord, AItem.ID];
  if not Assigned(lFollower) then
  begin
    TBoldQueueable.DisplayAll;
    lFollower := CellFollowers[lRecord, AItem.ID];
//    Assert(Assigned(lFollower));
  end;
  result := Assigned(lFollower);
  if Assigned(lFollower) then
  begin
    lFollower.EnsureDisplayable;
    result := lFollower.Controller.MayModify(lFollower);
    Assert(AItem.GetProperties <> nil);
    if Supports(AItem.GetProperties, IcxBoldEditProperties, lIcxBoldEditProperties) then
    begin
      result := lIcxBoldEditProperties.CanEdit(BoldHandle, lFollower);
    end
  end;
end;

procedure TcxGridBoldDataController.AssignData(
  ADataController: TcxCustomDataController);
begin
end;

procedure TcxGridBoldDataController.CheckGridModeBufferCount;
begin
//  UpdateGridModeBufferCount;
end;

procedure TcxGridBoldDataController.DeleteAllItems;
begin
  GridView.ClearItems;
end;

function TcxGridBoldDataController.DoScroll(AForward: Boolean): Boolean;
begin
  Result := SupportsScrollBarParams;
  if Result then
    if AForward then
      Controller.GoToNext(False, False)
    else
      Controller.GoToPrev(False, False);
end;

function TcxGridBoldDataController.DoScrollPage(
  AForward: Boolean): Boolean;
begin
  Result := SupportsScrollBarParams;
  if Result then
    if AForward then
      TcxCustomGridTableControllerAccess.FocusNextPage(Controller, False)
    else
      TcxCustomGridTableControllerAccess.FocusPrevPage(Controller, False);
end;

procedure TcxGridBoldDataController.GetFakeComponentLinks(AList: TList);
begin
  if (BoldHandle <> nil) and (BoldHandle.Owner <> GetOwnerOrView) and
    (AList.IndexOf(BoldHandle.Owner) = -1) then
    AList.Add(BoldHandle.Owner);
end;

function TcxGridBoldDataController.GetGridView: TcxCustomGridView;
begin
  Result := TcxCustomGridView(GetOwner);
end;

function TcxGridBoldDataController.GetItemDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
//  Result := TcxGridDefaultValuesProvider;
  Result := TcxGridBoldDefaultValuesProvider;
end;

function TcxGridBoldDataController.GetNavigatorIsBof: Boolean;
begin
  Result := GridView.Controller.IsStart;
end;

function TcxGridBoldDataController.GetNavigatorIsEof: Boolean;
begin
  Result := GridView.Controller.IsFinish;
end;

function TcxGridBoldDataController.GetScrollBarPos: Integer;
begin
  if SupportsScrollBarParams then
    if dceInsert in EditState then
      Result := FPrevScrollBarPos
    else
      Result := RecNo - 1
  else
    Result := -1;
  FPrevScrollBarPos := Result;
end;

function TcxGridBoldDataController.GetScrollBarRecordCount: Integer;
begin
// TODO see how to properly replace DataSetRecordCount, perhaps add BoldHandleRecordCount
  if SupportsScrollBarParams then
    Result := {DataSetRecordCount +} GridView.ViewInfo.VisibleRecordCount - 1
  else
    Result := -1;
end;

function TcxGridBoldDataController.CreateItem(
  aGridView: TcxCustomGridTableView; const aExpression, aCaption, aValueType,
  aName: string): TcxCustomGridTableItem;
begin
  result := aGridView.CreateItem;
  (result as IBoldAwareViewItem).BoldProperties.Expression := aExpression;
  result.DataBinding.ValueType := aValueType;
  result.Caption := aCaption;
  result.Name := CreateUniqueName(GetOwnerOrView, GridView, result, ScxGridPrefixName, aName);
end;

function TcxGridBoldDataController.IsDataChangeable: Boolean;
begin
  Result := False;
end;

function TcxGridBoldDataController.IsDataLinked: Boolean;
begin
  Result := BoldHandle <> nil;
end;

procedure TcxGridBoldDataController.PreFetchColumns(AList: TBoldList; aItem: integer);

  procedure Prefetch(FetchList: TBoldList; const AOcl: String; AVariableList: TBoldExternalVariableList);
{$IFNDEF SpanFetch}
  var
    IE: TBoldIndirectElement;
  const
    cCollectOcl = 'self->collect(%s)';
{$ENDIF}
  begin
    try
  {$IFDEF SpanFetch}
      FetchOclSpan(FetchList, AOcl, AVariableList);
  {$ELSE}
      IE := TBoldIndirectElement.Create;
      try
        with BoldHandle.StaticSystemHandle.System.Evaluator do
        begin
          if Assigned(ExpressionType(AOCL, FetchList.BoldType, false, AVariableList)) then
            Evaluate(AOcl, FetchList, nil, false, IE, false, AVariableList)
          else
          if Assigned(ExpressionType(Format(cCollectOcl, [AOcl]), FetchList.BoldType, false, AVariableList)) then
            Evaluate(Format(cCollectOcl, [AOcl]), FetchList, nil, false, IE, false, AVariableList);
        end;
      finally
        IE.free;
      end;
{$ENDIF}      
    except
      // ignore all exceptions during prefetch, otherwise the grid view infos will be messed up
    end;
  end;

var
  i,j: integer;
  lOcl: string;
  lBoldAwareViewItem: IBoldAwareViewItem;
  lItem: TcxCustomGridTableItem;
  FetchList: TBoldList;
  lWholeList: TBoldList;
  lMainFollower: TBoldFollower;
  lRowFollower: TBoldFollower;
  lCellFollower: TBoldFollower;
  locator: TBoldObjectLocator;
  IsObjectList: boolean;
  lRowIndex: integer;
  lSameCount: boolean;
  lGuard: IBoldGuard;
  lTableViewInfo: TcxGridTableViewInfo;
  lRowsViewInfo: TcxGridRowsViewInfo;
  lVisibleList: TBoldList;
const
  cCollectOcl = 'self->collect(%s)';
begin
  lGuard := TBoldGuard.Create(FetchList, lVisibleList);
  if not Assigned(AList) then
  begin
    lVisibleList := CreateList;
    CollectVisibleRecords(lVisibleList);
    AList := lVisibleList;
//    lTableViewInfo := (Owner as TcxGridBoldTableView).ViewInfo;
//    for := lTableViewInfo.FirstRecordIndex to lTableViewInfo.FirstRecordIndex + lTableViewInfo.VisibleColumnCount
//    (Owner as TcxGridBoldTableView).ViewInfo.RecordsViewInfo[0]. VisibleColumnCount;
//    (Owner as TcxGridBoldTableView).GridView.ViewInfo.
  end;

  if AList.Empty then
    exit;
  lMainFollower := Follower;
  lWholeList := BoldList;

  FetchList := CreateList;
  FetchList.DuplicateMode := bldmAllow;
  FetchList.Capacity := AList.Count;
  IsObjectList := (AList is TBoldObjectList) and (lWholeList is TBoldObjectList);
  lSameCount := AList.Count = lWholeList.Count;
  Locator := nil;
  for i := 0 to AList.Count - 1 do
  begin
{    if lSameCount then
      j := i
    else}
    if IsObjectList then
    begin
      locator := TBoldObjectList(aList).Locators[i];
      if TBoldObjectList(lWholeList).Locators[i] = locator then
        j := i
      else
        j := TBoldObjectList(lWholeList).IndexOfLocator( locator );
    end
    else
    begin
      if lWholeList[i] = aList[i] then
        j := i
      else
        j := lWholeList.IndexOf(aList[i]);
    end;
    if j = -1 then
    begin
      FetchList.AddList(AList);
      break;
    end;
    lRowFollower := lMainFollower.SubFollowers[j];
    if not Assigned(lRowFollower) or not lRowFollower.Displayable {or not Assigned(lRowFollower.Element)} then
    begin
      if IsObjectList then
        TBoldObjectList(FetchList).AddLocator(locator)
      else
        FetchList.Add(aList[i])
    end
    else
    begin
      for j := 0 to lRowFollower.SubFollowerCount - 1 do
      begin
        lItem := TcxCustomGridTableItem(FindItemByData(j));
        if (aItem = j) or ((AItem = -1) and lItem.ActuallyVisible) {or RequiresAllRecords(lItem)} then
          if not lRowFollower.SubFollowerAssigned[j] then
          begin
            if IsObjectList then
              TBoldObjectList(FetchList).AddLocator(locator)
            else
              FetchList.Add(aList[j]);
            break;
          end;
      end;
    end;
  end;
//  if FetchList.Empty then
//    exit;
  ConnectEvents(false);
  inc(FSkipMakeCellUptoDate);
  try
    if not FetchList.Empty then
    for I := 0 to ItemCount - 1 do
    begin
      lItem := TcxCustomGridTableItem(FindItemByData(i));
      if (aItem = i) or ((aItem = -1) and lItem.ActuallyVisible) {or RequiresAllRecords(lItem)} then
      begin
        lItem.GetInterface(IBoldAwareViewItem, lBoldAwareViewItem);
        lOcl := lBoldAwareViewItem.BoldProperties.Expression;
        Prefetch(FetchList, lOcl, lBoldAwareViewItem.BoldProperties.VariableList);
      end;
    end;
    DisplayFollowers;
    lSameCount := (AList.Count = lWholeList.Count) {or ((Assigned(Filter) and Filter.Active and (Filter.Root.Count > 0)))};
    if lSameCount then
    begin
      i := 0;
      j := lWholeList.Count-1;
    end
    else
      FindMinMaxIndex(AList, lWholeList, i, j);
    with TBoldFollowerList(lMainFollower.RendererData) do
      if (FirstActive <> i) or (LastActive <> j) then
        BoldProperties.SetActiveRange(lMainFollower, i, j, 0);

    for i := 0 to AList.Count - 1 do
    begin
      if lSameCount and (lWholeList[i] = AList[i]) then
        lRowIndex := i
      else
      if isObjectList then
        lRowIndex := TBoldObjectList(lWholeList).IndexOfLocator(TBoldObjectList(AList).Locators[i])
      else
        lRowIndex := lWholeList.IndexOf(AList[i]);
      if lRowIndex <> -1 then
      begin
        lRowFollower := lMainFollower.EnsuredSubFollowers[lRowIndex];
        Assert(Assigned(lRowFollower));
        lRowFollower.SetElementAndMakeCurrent(AList[i], true);
        for j := 0 to ItemCount - 1 do
        begin
          lItem := TcxCustomGridTableItem(FindItemByData(j));
          if (aItem = j) or ((aItem = -1) and lItem.ActuallyVisible) {or RequiresAllRecords(lItem)} then
          begin
            lCellFollower := lRowFollower.SubFollowers[j];
            Assert(Assigned(lCellFollower));
            lCellFollower.Active := true;
          end;
        end;
      end;
    end;

  finally
    dec(FSkipMakeCellUptoDate);
    ConnectEvents(true);
  end;
end;

procedure TcxGridBoldDataController.ProcessQueueEvent(Sender: TObject);
begin
  if not IsDestroying then
    if not GridView.IsDestroying then
    begin
      if fDataChanged then
      begin
        DataHasChanged := false;
        AdjustActiveRange();
        DataChanged(dcTotal, -1, -1);
      //  Follower.MarkValueOutOfDate;
      //  self.Refresh;
        CheckDataSetCurrent;
      end;
{$IFDEF DelayOnFocusedRecordChange}
      if fFocusChanged then
      begin
        if GridView is TcxGridBoldTableView then
          (GridView as TcxGridBoldTableView).InheritedDoFocusedRecordChanged(FPrevFocusedRecordIndex, FFocusedRecordIndex,
            FPrevFocusedDataRecordIndex, FFocusedDataRecordIndex, FNewItemRecordFocusingChanged)
        else
        if (GridView is TcxGridBoldBandedTableView) then
          (GridView as TcxGridBoldBandedTableView).InheritedDoFocusedRecordChanged(FPrevFocusedRecordIndex, FFocusedRecordIndex,
            FPrevFocusedDataRecordIndex, FFocusedDataRecordIndex, FNewItemRecordFocusingChanged)
        else
          Assert(false, 'Unsupported GridView: ' + GridView.ClassName);
        fFocusChanged := false;
      end;
{$ENDIF}
      if fRedrawGrid then
      begin
        GridView.Invalidate(true);
        if fRecalcSummary then
          GridView.DataController.Summary.Recalculate;
        fRedrawGrid := false;
        fRecalcSummary := false;
      end;
    end;
end;

function TcxGridBoldDataController.RequiresAllRecords(AItem: TObject): boolean;
var
  lItem: TcxCustomGridTableItemAccess;
begin
  result := false;
  if (AItem is TcxCustomGridTableItem) then
  begin
    lItem := TcxCustomGridTableItemAccess(AItem);
    result := (lItem.SortIndex <> -1) or (lItem.GroupIndex <> -1) or lItem.Filtered or
           ((AItem is TcxGridColumn) and
           (
           (TcxGridColumn(AItem).Summary.FooterKind <> skNone ) or
           (TcxGridColumn(AItem).Summary.GroupFooterKind  <> skNone ) or
           (TcxGridColumn(AItem).Summary.GroupKind <> skNone )
           ));
  end;
end;

function TcxGridBoldDataController.RequiresAllRecords: boolean;
var
  lcxCustomGridTableView: TcxCustomGridTableView;
  i: integer;
begin
  lcxCustomGridTableView := GridView as TcxCustomGridTableView;
  result := (lcxCustomGridTableView.SortedItemCount > 0)
    or (lcxCustomGridTableView.GroupedItemCount > 0)
    or (Summary.FooterSummaryItems.Count > 0)
    or (Assigned(Filter) and Filter.Active and (Filter.Root.Count > 0));
  if not result  then    
  for I := 0 to lcxCustomGridTableView.ItemCount - 1 do
    result := result or RequiresAllRecords(lcxCustomGridTableView.Items[i]);
end;

function TcxGridBoldDataController.GetOwnerOrView: TComponent;
begin
  if Assigned(GridView.Owner) then
    result := GridView.Owner
  else
    result := GridView;
end;

function TcxGridBoldDataController.SetScrollBarPos(
  Value: Integer): Boolean;
begin
  Result := SupportsScrollBarParams;
  if Result then
    RecNo := Value + 1;
end;

function TcxGridBoldDataController.SupportsScrollBarParams: Boolean;
begin
  Result := IsGridMode and TcxCustomGridTableViewAccess(GridView).IsEqualHeightRecords;
end;

constructor TcxGridBoldDataController.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TcxGridBoldDataController.Destroy;
begin
  BoldInstalledQueue.RemoveFromPostDisplayQueue(self);
  inherited;
end;

procedure TcxGridBoldDataController.DisplayFollowers;
var
  Queueable: TBoldQueueable;
begin
  if Assigned(Follower) and (Follower.IsInDisplayList {or not Follower.Displayable}) and not (GridView.IsLoading or GridView.IsDestroying) then
  repeat
    Queueable := TBoldFollowerAccess(Follower).MostPrioritizedQueuableOrSelf;
    if Assigned(Queueable) then
      TBoldQueueableAccess(Queueable).display;
  until not Assigned(Queueable);
end;

procedure TcxGridBoldDataController.CollectVisibleRecords(var aList: TBoldList);
var
  i,j: integer;
  lFollower: TBoldFollower;
  lRecordIndex: integer;
  lList: TBoldList;
  lGuard: IBoldGuard;
  lWholeList: TBoldList;
  lRecord: TcxCustomGridRecord;
  IsObjectList: boolean;
begin
  lWholeList := BoldList;
  IsObjectList := (lWholeList is TBoldObjectList);
//  lGuard := TBoldGuard.Create(lList);
//  lList := CreateList;
  lList := AList;
  i := GridView.Controller.TopRecordIndex;
  if i = -1 then
    exit;
  if i <> -1 then
  begin
    j := i + GridView.ViewInfo.VisibleRecordCount+2; // there can be at most 2 partially visible records
    i := (i div 10) * 10;
    j := (j div 10 +1) * 10;
  end
  else
  begin
    i := 0;
    j := 10;
  end;
  if j >= TcxCustomDataControllerInfoAccess(DataControllerInfo).RecordList.Count then
    j := TcxCustomDataControllerInfoAccess(DataControllerInfo).RecordList.Count -1;
  lList.Capacity := j-i+1;
  lList.DuplicateMode := bldmAllow;
  for i := i to j do
  begin
    lRecord := GridView.ViewData.GetRecordByIndex(i);
    if Assigned(lRecord) then
    begin
      lRecordIndex := lRecord.RecordIndex;
      if (lRecordIndex > -1) and (lRecordIndex < lWholeList.count) then
      begin
        if IsObjectList then
          TBoldObjectList(lList).AddLocator(TBoldObjectList(lWholeList).Locators[lRecordIndex])
        else
          lList.Add(lWholeList[lRecordIndex]);
      end;
    end;
  end;
end;

procedure TcxGridBoldDataController.AdjustActiveRange(
  aList: TBoldList = nil; aItem: integer = -1);
var
  i,j: integer;
  lFollower: TBoldFollower;
  lRecordIndex: integer;
  lList: TBoldList;
  lGuard: IBoldGuard;
  lWholeList: TBoldList;
  lRecord: TcxCustomGridRecord;
  IsObjectList: boolean;
begin
  lFollower := Follower;
  lList := aList;
  lWholeList := BoldList;
  if not Assigned(lFollower) or not Assigned(lWholeList) then
    exit;
   i := GridView.Controller.TopRecordIndex;
   if i = -1 then
     exit;
  if not Assigned(aList) and not LoadAll then
  begin
    IsObjectList := (lWholeList is TBoldObjectList);
    lGuard := TBoldGuard.Create(lList);
    lList := CreateList;
    if i <> -1 then
    begin
      j := i + GridView.ViewInfo.VisibleRecordCount+2; // there can be at most 2 partially visible records
      i := (i div 10) * 10;
      j := (j div 10 +1) * 10;
    end
    else
    begin
      i := 0;
      j := 10;
    end;
    if j >= TcxCustomDataControllerInfoAccess(DataControllerInfo).RecordList.Count then
      j := TcxCustomDataControllerInfoAccess(DataControllerInfo).RecordList.Count -1;
    lList.Capacity := j-i+1;
    lList.DuplicateMode := bldmAllow;
    for i := i to j do
    begin
      lRecord := GridView.ViewData.GetRecordByIndex(i);
      if Assigned(lRecord) then
      begin
        lRecordIndex := lRecord.RecordIndex;
        if (lRecordIndex > -1) and (lRecordIndex < lWholeList.count) then
        begin
          if IsObjectList then
            TBoldObjectList(lList).AddLocator(TBoldObjectList(lWholeList).Locators[lRecordIndex])
          else
            lList.Add(lWholeList[lRecordIndex]);
        end;
      end;
    end;
  end;
  if LoadAll then
    aItem := -1;
  inherited AdjustActiveRange(lList, aItem);
end;

procedure TcxGridBoldDataController._BeforeMakeListUpToDate(
  Follower: TBoldFollower);
//var
//  lFirstLoad: boolean;
begin
//  lFirstLoad := CustomDataSource = nil;
  if GridView.IsDestroying then
  begin
    Assert(Assigned(self));
    exit;
  end;
  inherited;
{  if lFirstLoad then
  begin
    if Assigned(OnBeforeLoad) then
      OnBeforeLoad(GridView);
  end;
}
end;

procedure TcxGridBoldDataController.ReloadStorage;
begin
  if not Assigned(CustomDataSource) then
    exit;
  if CustomDataSource.Provider = nil then
    TcxBoldDataSource(CustomDataSource).CurrentProvider := Provider;
  if MainFollowerNeedsDisplay then
    DisplayFollowers;
  DataStorage.BeginLoad;
  fInternalLoading := true;
  try
    TcxBoldDataSource(CustomDataSource).LoadRecordHandles;
    if DataControllerInfo.LockCount = 0 then
      Refresh;
    Assert(Follower.SubFollowerCount = DataStorage.RecordCount, 'Follower.SubFollowerCount = DataStorage.RecordCount' + IntToStr(Follower.SubFollowerCount) + ',' + IntToStr(DataStorage.RecordCount) );
  finally
    fInternalLoading := false;
    DataStorage.EndLoad;
  end;
end;

procedure TcxGridBoldDataController._AfterMakeListUptoDate(
  Follower: TBoldFollower);
var
  i: integer;
  lcxBoldDataSource: TcxBoldDataSource;
  lBoldAwareView: IBoldAwareView;
  lFirstLoad: boolean;
  lDataChanged: boolean;
  lTypeChanged: boolean;
begin
  if GridView.IsDestroying then
  begin
    Assert(Assigned(self));
    exit;
  end;
  {$IFDEF BoldDevExLog}
  _Log((GetOwner as TComponent).Name + ':_AfterMakeListUpToDate:' +IntToStr(FSkipMakeCellUptoDate), className);
  {$ENDIF}

  lTypeChanged := false;
  lBoldAwareView := GridView as IBoldAwareView;
  lFirstLoad := (CustomDataSource = nil);
  if not lFirstLoad and (DataStorage.RecordCount <> Follower.SubfollowerCount) then
    DataHasChanged := true;
  try
    if GridView.IsDesigning {or isPattern} then // isPattern needs to be tested for detail views
    begin
      exit;
    end;
//    TypeMayHaveChanged;
    if lFirstLoad then
    begin
      fFetchedAll := false;
      lcxBoldDataSource := cxBoldDataSourceClass.Create(self as TcxGridBoldDataController);

      if {BoldAutoColumns and} (GetHandleListElementType <> fCurrentListElementType) and Assigned(fCurrentListElementType) and (GetHandleListElementType <> nil) then
      if ClearColumnsOnTypeChange and not GetHandleListElementType.ConformsTo(fCurrentListElementType) then
        lBoldAwareView.ClearItems;
      CustomDataSource := lcxBoldDataSource;

      if (Follower.Element = nil) or Follower.ElementValid then
      begin
        lTypeChanged := TypeMayHaveChanged;
      end;
      for i := 0 to ItemCount - 1 do
      begin
        BoldPropertiesFromItem(i).AfterMakeUptoDate := nil;
      end;
    end
    else
    begin
      TypeMayHaveChanged;
  //    EndUpdate;
    end;
  finally
    lDataChanged := DataHasChanged;
//    if lDataChanged or lFirstLoad or lTypeChanged then
//      AdjustActiveRange();
    if not lDataChanged then
      dec(FSkipMakeCellUptoDate);
    if lDataChanged and (FSkipMakeCellUptoDate = 1) and Assigned(CustomDataSource) then
    begin
      if CustomDataSource.Provider = nil then
        TcxBoldDataSource(CustomDataSource).CurrentProvider := Provider;
      CustomDataSource.DataChanged;
      TcxBoldDataSource(CustomDataSource).DataController.DataControllerInfo.Refresh;

      DataHasChanged := false;
    end;
    if lDataChanged then
      dec(FSkipMakeCellUptoDate);
    ConnectEvents(true);
    if {not lFirstLoad and not lDataChanged and} (Follower.SubFollowerCount <> DataStorage.RecordCount) then
      ReloadStorage;
    inc(FSkipSyncFocusedRecord);
    try
      if CustomDataSource.Provider = nil then
        TcxBoldDataSource(CustomDataSource).CurrentProvider := Provider;
      Assert(Assigned(CustomDataSource.Provider));
      EndFullUpdate;
    finally
      dec(FSkipSyncFocusedRecord);
    end;
    if not ((Follower.SubFollowerCount = DataStorage.RecordCount) or (DetailMode = dcdmPattern) or lFirstLoad) then
    begin
      if CustomDataSource.Provider = nil then
        TcxBoldDataSource(CustomDataSource).CurrentProvider := Provider;
      DataStorage.BeginLoad;
      try
        TcxBoldDataSource(CustomDataSource).LoadRecordHandles;
        Assert(Follower.SubFollowerCount = DataStorage.RecordCount, 'Follower.SubFollowerCount = DataStorage.RecordCount' + IntToStr(Follower.SubFollowerCount) + ',' + IntToStr(DataStorage.RecordCount) );
      finally
        DataStorage.EndLoad;
      end;
    end;
    if (DetailMode <> dcdmPattern) and (Groups.GroupingItemCount = 0) and (not Filter.Active) and (GridView.ViewData.RecordCount <> {RowCount} Follower.SubFollowerCount) then
    begin
      TcxCustomGridTableViewAccess(GridView).ViewInfoCache.Count := Follower.SubFollowerCount;
      TcxCustomDataControllerInfoAccess(DataControllerInfo).Update;
      TcxCustomGridTableViewAccess(GridView).ViewData.Refresh(Follower.SubFollowerCount);
      Assert(GridView.ViewData.RecordCount = Follower.SubFollowerCount);
    end;
    if Assigned(CustomDataSource) then
      TcxBoldDataSource(CustomDataSource).fIsBoldInitiatedChange := false;
    fBoldProperties.OnAfterInsertItem := _InsertRow;
    fBoldProperties.OnAfterDeleteItem := _DeleteRow;
    fBoldProperties.OnReplaceitem := _ReplaceRow;

    BeginUpdate;
    try
      FNearestRecordIndex := -1;
      if (Follower.CurrentIndex <> FocusedRecordIndex) and (FSkipMakeCellUptoDate < 2) then
      begin
        Controller.ClearSelection;
        if {(Follower.CurrentIndex <> -1) and} (Follower.CurrentIndex < RecordCount) then
          FocusedRecordIndex := Follower.CurrentIndex
        else
        if RecordCount > 0 then
        begin
          Follower.CurrentIndex := 0;
          FocusedRecordIndex := 0;
        end;
        if Assigned(Controller.FocusedRecord) then
          Controller.FocusedRecord.Selected := true;
      end;
      Change([dccSelection]);
    finally
      EndUpdate;
    end;
    if (DataControllerInfo.LockCount = 0) and ((DataControllerInfo.DataRowCount <> RowCount) and ((DataControllerInfo.DataGroups.Count>0) and (RecordCount <> DataControllerInfo.DataRowCount))) then
    begin
      Refresh;
    end;
    if lTypeChanged and BoldAutoColumns {(GridView.OptionsView is TcxGridTableOptionsView) and TcxGridTableOptionsView(GridView.OptionsView).ColumnAutoWidth} then
    begin
//      TcxGridTableOptionsView(GridView.OptionsView).ColumnAutoWidth := false;
      ShowHourglassCursor;
      GridView.BeginUpdate;
      try
        GridView.ApplyBestFit(nil, true, false);
      finally
        GridView.EndUpdate;
        HideHourglassCursor;
//        TcxGridTableOptionsView(GridView.OptionsView).ColumnAutoWidth := true
      end;
    end;
    ConnectEvents(True);
    if lDataChanged or lFirstLoad or lTypeChanged then
    begin
      {$IFDEF FireAfterLoadOnChangeOnly}
      if Assigned(OnAfterLoad) then
      begin
        if (DataControllerInfo.LockCount = 0) then
        begin
          fTriggerAfterLoad := false;
          OnAfterLoad(GridView)
        end
        else
          fTriggerAfterLoad := true;
      end;

      {$ENDIF}
    end;
    {$IFNDEF FireAfterLoadOnChangeOnly}
    if Assigned(OnAfterLoad) then
    begin
      if (DataControllerInfo.LockCount = 0) then
      begin
        fTriggerAfterLoad := false;
        OnAfterLoad(GridView);
      end
      else
        fTriggerAfterLoad := true;
    end;
    {$ENDIF}
  end;
end;



procedure TcxGridBoldDataController.TypeChanged(aNewType, aOldType: TBoldElementTypeInfo);
begin
  inherited;
  if not Assigned(fCurrentListElementType) then
  begin
    if BoldAutoColumns then
    begin
      TcxCustomGridTableView(GridView).ClearItems;
      fFetchedAll := false;
    end;
  end
  else
  if Assigned(aOldType) and BoldAutoColumns or (TcxCustomGridTableView(GridView).ItemCount = 0) then
  begin
    TcxCustomGridTableView(GridView).ClearItems;
    TcxGridBoldDataController(self).CreateAllItems(false);
    fFetchedAll := false;
  end;
end;

function TcxGridBoldDataController.GetSummaryClass: TcxDataSummaryClass;
begin
  Result := TcxBoldDataSummary;
end;

function TcxGridBoldDataController.GetSummaryGroupItemLinkClass: TcxDataSummaryGroupItemLinkClass;
begin
  Result := TcxGridTableSummaryGroupItemLink;
end;

function TcxGridBoldDataController.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := TcxGridTableSummaryItem;
end;

procedure TcxGridBoldDataController._AfterMakeCellUptoDate(
  Follower: TBoldFollower);
var
  lItem: TcxCustomGridTableItemAccess;
begin
  if DataHasChanged then
    exit;
  lItem := TcxCustomGridTableItemAccess(GetItem(Follower.index));
  if (FSkipMakeCellUptoDate = 0) and not ((lItem.GroupIndex = -1) and (lItem.SortIndex = -1) and (not lItem.Filtered) and (Follower.OwningFollower.index <= RecordCount)) then
    DataHasChanged := true
  else
  begin
    EnsureEventQueued;
    fRedrawGrid := true;
    fRecalcSummary := fRecalcSummary or (Summary.FooterSummaryItems.IndexOfItemLink(lItem) <> -1) or (Assigned(Summary.SummaryGroups.FindByItemLink(lItem)));
  end;
end;

procedure TcxGridBoldDataController.BeginFullUpdate;
begin
  GridView.BeginUpdate;
  inherited;
end;

procedure TcxGridBoldDataController.EndFullUpdate;
begin
  inherited;
  if not (IsDestroying or GridView.IsDestroying) then
  begin
    if DataHasChanged and (LockCount <= 1) then
    begin

      //DataChangedEvent(nil);
        DataHasChanged := false;
        AdjustActiveRange();
        DataChanged(dcTotal, -1, -1);
      //  Follower.MarkValueOutOfDate;
      //  self.Refresh;
        CheckDataSetCurrent;


    end;
    if ((LockCount <= 1)) and (Follower.SubFollowerCount <> DataStorage.RecordCount) then
      ReloadStorage;
    GridView.EndUpdate
  end;
  if fTriggerAfterLoad then
  begin
    fTriggerAfterLoad := false;
    OnAfterLoad(GridView)
  end;  
end;

function TcxGridBoldDataController.BoldSetValue(AItemHandle: TcxDataItemHandle;
  ACellFollower: TBoldFollower; const AValue: variant): boolean;
var
  lcxCustomGridTableItem: TcxCustomGridTableItem;
  lcxBoldEditProperties: IcxBoldEditProperties;
  lEdit: TcxCustomEdit;
begin
  result := false;
  lcxCustomGridTableItem := GetItem(Integer(AItemHandle)) as TcxCustomGridTableItem;
  if Supports(lcxCustomGridTableItem.GetProperties, IcxBoldEditProperties, lcxBoldEditProperties) then
  begin
    lEdit := Controller.EditingController.Edit;
    lcxBoldEditProperties.SetStoredValue(AValue, nil, lEdit, ACellFollower, result);
  end;
end;

procedure TcxGridBoldDataController.EnsureConstraintColumn;
var
  lItem: TcxCustomGridTableItemAccess;
begin
  lItem := TcxCustomGridTableItemAccess(GetItemByExpression(cOCLConstraint));
  if not Assigned(lItem) then
    lItem := TcxCustomGridTableItemAccess(CreateItem(GridView, cOCLConstraint, '§', 'Boolean', 'Constraints'));
  lItem.OnCustomDrawCell := ConstraintColumnCustomDrawCell;
  lItem.Index := 0;
  lItem.BestFitMaxWidth := 16;
  lItem.Width := 16;
  lItem.MinWidth := 16;
  lItem.Options.Focusing := false;
  lItem.Options.Editing := false;
  lItem.Options.IncSearch := false;
  if lItem.Options is TcxGridColumnOptions then
  begin
    TcxGridColumnOptions(lItem.Options).HorzSizing := false;
    TcxGridColumnOptions(lItem.Options).Moving := false;
  end;
end;

function TcxGridBoldDataController.CanSelectRow(
  ARowIndex: Integer): Boolean;
begin
  Result := TcxCustomGridTableViewAccess(GridView).CanSelectRecord(ARowIndex);
end;

function TcxGridBoldDataController.CompareByField(ARecordIndex1,
  ARecordIndex2: Integer; AField: TcxCustomDataField;
  AMode: TcxDataControllerComparisonMode): Integer;
begin
  if GridView.ViewData.NeedsCustomDataComparison(AField, AMode) then
    Result := GridView.ViewData.CustomCompareDataValues(AField,
      GetComparedValue(ARecordIndex1, AField), GetComparedValue(ARecordIndex2, AField), AMode)
  else
    Result := inherited CompareByField(ARecordIndex1, ARecordIndex2, AField, AMode);
end;

function TcxGridBoldDataController.CreateDetailLinkObject(
  ARelation: TcxCustomDataRelation; ARecordIndex: Integer): TObject;
begin
  Result := TcxGridLevelAccess(ARelation.Item).CreateLinkObject(ARelation, ARecordIndex);
end;

procedure TcxGridBoldDataController.DoValueTypeClassChanged(
  AItemIndex: Integer);
begin
  inherited;
  TcxCustomGridTableViewAccess(GridView).ItemValueTypeClassChanged(AItemIndex);
end;

procedure TcxGridBoldDataController.FilterChanged;
begin
  DataControllerInfo.BeginUpdate;
  try
    if MainFollowerNeedsDisplay then
      DisplayFollowers;
    inherited;
    TcxCustomGridTableViewAccess(GridView).FilterChanged;
  finally
    TcxCustomDataControllerInfoAccess(DataControllerInfo).CorrectFocusedRow(FocusedRowIndex);
    DataControllerInfo.EndUpdate;
  end;
end;

procedure TcxGridBoldDataController.FocusControl(AItemIndex: Integer;
  var Done: Boolean);
begin
  inherited;
  TcxCustomGridTableViewAccess(GridView).FocusEdit(AItemIndex, Done);
end;

function TcxGridBoldDataController.GetDefaultActiveRelationIndex: Integer;
begin
  Result := TcxCustomGridTableViewAccess(GridView).GetDefaultActiveDetailIndex;
end;

function TcxGridBoldDataController.GetDetailDataControllerByLinkObject(
  ALinkObject: TObject): TcxCustomDataController;
begin
  Result := TcxCustomGridView(ALinkObject).DataController;
end;

function TcxGridBoldDataController.GetDisplayText(ARecordIndex,
  AItemIndex: Integer): string;
var
  lCellFollower: TBoldFollower;
begin
{  if not GridView.ViewData.GetDisplayText(ARecordIndex, AItemIndex, Result) then
    Result := inherited GetDisplayText(ARecordIndex, AItemIndex);
  TcxCustomGridTableItemAccess(GridView.Items[AItemIndex]).DoGetDataText(ARecordIndex, Result);
}
  AItemIndex := GridView.Items[AItemIndex].ID;
  EnsureFollower(ARecordIndex, AItemIndex);
  lCellFollower := CellFollowers[ARecordIndex, AItemIndex];
  result := VarToStr((lCellFollower.Controller as TBoldVariantFollowerController).GetCurrentAsVariant(lCellFollower));
end;

function TcxGridBoldDataController.GetFilterDataValue(
  ARecordIndex: Integer; AField: TcxCustomDataField): Variant;
begin
  if Assigned(BoldList) and not HasCellFollower[ARecordIndex, GetItemData(aField.Item)] then
    PreFetchColumns(BoldList, GetItemData(aField.Item));
  Result := inherited GetFilterDataValue(ARecordIndex, AField);
  if GridView.ViewData.HasCustomDataHandling(AField, doFiltering) then
    Result := GridView.ViewData.GetCustomDataValue(AField, Result, doFiltering);
end;

function TcxGridBoldDataController.GetFilterDisplayText(ARecordIndex,
  AItemIndex: Integer): string;
var
  lCellFollower: TBoldFollower;
begin
{  if GridView.ViewData.HasCustomDataHandling(Fields[AItemIndex], doFiltering) then
    Result := GridView.ViewData.GetCustomDataDisplayText(ARecordIndex, AItemIndex, doFiltering)
  else
    Result := inherited GetFilterDisplayText(ARecordIndex, AItemIndex);
}
  AItemIndex := GridView.Items[AItemIndex].ID;
  EnsureFollower(ARecordIndex, AItemIndex);
  lCellFollower := CellFollowers[ARecordIndex, AItemIndex];
  result := VarToStr((lCellFollower.Controller as TBoldVariantFollowerController).GetCurrentAsVariant(lCellFollower));
end;

function TcxGridBoldDataController.GetFilterItemFieldCaption(
  AItem: TObject): string;
begin
  Result := TcxCustomGridTableItemAccess(AItem).FilterCaption;
end;

function TcxGridBoldDataController.GetItemID(AItem: TObject): Integer;
begin
  if AItem is TcxCustomGridTableItem then
    Result := TcxCustomGridTableItem(AItem).ID
  else
    Result := -1;
end;

function TcxGridBoldDataController.GetItemSortByDisplayText(
  AItemIndex: Integer; ASortByDisplayText: Boolean): Boolean;
begin
  Result := TcxCustomGridTableViewAccess(GridView).GetItemSortByDisplayText(AItemIndex, ASortByDisplayText);
end;

function TcxGridBoldDataController.GetItemValueSource(
  AItemIndex: Integer): TcxDataEditValueSource;
begin
  Result := TcxCustomGridTableViewAccess(GridView).GetItemValueSource(AItemIndex);
end;

function TcxGridBoldDataController.GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass;
begin
  Result := GridView.ViewData.GetSortingBySummaryEngineClass;
end;

procedure TcxGridBoldDataController.UpdateData;
begin
  inherited;
  TcxCustomGridTableViewAccess(GridView).UpdateRecord;
end;


procedure TcxGridBoldDataController.DoDragOver(
  Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  lcxGridSite: TcxGridSite;
  lcxCustomGridHitTest: TcxCustomGridHitTest;
  lcxGridRecordCellHitTest: TcxGridRecordCellHitTest;
  lBoldRoleRTInfo: TBoldRoleRTInfo;
  lBoldList: TBoldList;
begin
  Accept := false;
  lcxGridSite := TcxGridSite(Sender);
  lcxCustomGridHitTest := lcxGridSite.ViewInfo.GetHitTest(X, Y);
  lBoldList := BoldHandle.MutableList;
  if Assigned(lBoldList) then
  begin
    lBoldRoleRTInfo := lBoldList.BoldMemberRTInfo as TBoldRoleRTInfo;
    // not allowed for association classes
    Accept := not Assigned(lBoldRoleRTInfo) or not (lBoldRoleRTInfo.RoleType = rtLinkRole);
    // do not allow drag in a single grid if the list is a member and is not ordered
    Accept := Accept and not ((TcxDragControlObject(Source).Control = Sender) and (Assigned(lBoldRoleRTInfo) and not lBoldRoleRTInfo.IsOrdered));
    // do not allow drop in the system owned class list (ie all instances)
    Accept := Accept and not (lBoldList.OwningElement is TBoldSystem);
    // accept drop only where applicable
    Accept := Accept and ((lcxCustomGridHitTest is TcxGridViewNoneHitTest) or (lcxCustomGridHitTest is TcxGridRecordCellHitTest));
    // do not allow drop after the last row of the grid if the dragged item is already last in the list
    if Accept and (lcxCustomGridHitTest is TcxGridViewNoneHitTest) and (TcxDragControlObject(Source).Control = Sender) then
    begin
      Accept := Follower.CurrentIndex <> Follower.SubFollowerCount - 1;
    end
    else
      if Accept and (lcxCustomGridHitTest is TcxGridRecordCellHitTest) and (TcxDragControlObject(Source).Control = Sender) then
      begin
        lcxGridRecordCellHitTest := TcxGridRecordCellHitTest(lcxCustomGridHitTest);
      // do not allow source and desination row to be same if drag within single grid
        Accept := Accept and (lcxGridRecordCellHitTest.GridRecord.RecordIndex <> FocusedRecordIndex);
      end;
    // check if destination already contains all source objects (and the desination is not ordered)
    if Accept and Assigned(lBoldRoleRTInfo) and not lBoldRoleRTInfo.IsOrdered and lBoldList.IncludesAll(BoldGUIHandler.DraggedObjects) then
      Accept := false;
  end;
  Accept := Accept and BoldProperties.DragOver(Follower, BoldHandle.MutableList, Y);
end;

procedure TcxGridBoldDataController.DoDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  i: integer;
  lIndex: integer;
  lcxCustomGridHitTest: TcxCustomGridHitTest;
  lcxGridSite: TcxGridSite;
begin
  lcxGridSite := TcxGridSite(Sender);
  lcxCustomGridHitTest := lcxGridSite.ViewInfo.GetHitTest(X, Y);

  lIndex := -1;
  if lcxCustomGridHitTest is TcxGridRecordCellHitTest then
  begin
    lIndex := TcxGridRecordCellHitTest(lcxCustomGridHitTest).GridRecord.RecordIndex;
  end;

  BoldProperties.DragDrop(Follower, BoldHandle.MutableList, lIndex);
  TcxBoldDataSource(CustomDataSource).fIsBoldInitiatedChange := true;
  try
    {$IFDEF DisplayAll}
    TBoldQueueable.DisplayAll;
    {$ENDIF}
    CustomDataSource.DataChanged;
    BeginUpdate;
    try
      ClearSelection;
      for I := 0 to BoldGUIHandler.DraggedObjects.Count - 1 do
      begin
        lIndex := BoldHandle.List.IndexOf(BoldGUIHandler.DraggedObjects[i]);
        if lIndex < RowCount then
          ChangeRowSelection(lIndex, True);
      end;
    finally
      EndUpdate;
    end;
  finally
    TcxBoldDataSource(CustomDataSource).fIsBoldInitiatedChange := false
  end;
end;

procedure TcxGridBoldDataController.DoEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  BoldProperties.EndDrag;
end;

procedure TcxGridBoldDataController.DoStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  {$IFDEF DisplayAll}
  TBoldQueueable.DisplayAll;
  {$ENDIF}
  SelectionChanged; // make sure the follower selection is updated
  BoldProperties.StartDrag(Follower);
end;

{ TcxGridItemBoldDataBinding }

procedure TcxGridItemBoldDataBinding.Assign(Source: TPersistent);
begin
  if Source is TcxGridItemBoldDataBinding then
    fBoldProperties.Assign(TcxGridItemBoldDataBinding(Source).BoldProperties);
  inherited;
end;

constructor TcxGridItemBoldDataBinding.Create(
  AItem: TcxCustomGridTableItem);
begin
  inherited Create(AItem);
  fSubscriber := TBoldPassthroughSubscriber.Create(Receive);
  fBoldProperties := TBoldCxGridVariantFollowerController.Create(DataController.GetOwnerOrView);
  (fBoldProperties as TBoldCxGridVariantFollowerController).fcxGridItemBoldDataBinding := self;
  fBoldProperties.ApplyPolicy := bapExit;
  DataController.fBoldColumnsProperties.Add(fBoldProperties);
  fBoldProperties.OnGetContextType := DataController.GetHandleStaticType;
  fBoldProperties.AddSmallSubscription(fSubscriber, [beValueChanged], beValueChanged);
  FBoldProperties.AfterMakeUptoDate := DataController._AfterMakeCellUptoDate;
  (DefaultValuesProvider as TcxGridBoldDefaultValuesProvider).BoldHandleFollower := DataController.BoldHandleFollower;
  (DefaultValuesProvider as TcxGridBoldDefaultValuesProvider).BoldProperties := fBoldProperties;
  Data := TObject(DataController.fBoldColumnsProperties.Count-1);
end;

destructor TcxGridItemBoldDataBinding.Destroy;
begin
  FreeAndNil(FBoldProperties);
  FreeAndNil(fSubscriber);
  inherited;
end;

function TcxGridItemBoldDataBinding.GetDataController: TcxGridBoldDataController;
begin
  Result := TcxGridBoldDataController(inherited DataController);
end;

function TcxGridItemBoldDataBinding.GetDefaultValueTypeClass: TcxValueTypeClass;
begin
  Result := TcxStringValueType;
end;

procedure TcxGridItemBoldDataBinding.Init;
begin
  inherited;
  with Item do
  if BoldProperties.Expression = cOCLConstraint then
  begin
    OnCustomDrawCell := DataController.ConstraintColumnCustomDrawCell;
    OnGetCellHint := DataController.GetCellHint;
  end
  else
  begin
    if not Assigned(OnCustomDrawCell) then
      OnCustomDrawCell := DataController.ColumnCustomDrawCell;
  end;
end;

procedure TcxGridItemBoldDataBinding.SetBoldProperties(
  Value: TBoldVariantFollowerController);
begin
  if Assigned(Value) then
    fBoldProperties.Assign(Value);
end;

procedure TcxGridItemBoldDataBinding.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
var
  lContextType: TBoldElementTypeInfo;
  lResultType: TBoldElementTypeInfo;
  lBoldMemberRTInfo: TBoldMemberRTInfo;
  lEvaluator: TBoldOcl;
begin
  if not GridView.IsLoading and not DataController.fCreatingColumns then
    if Assigned(DataController.BoldHandle) and Assigned(DataController.BoldHandle.ListElementType) then
    begin
      lContextType := DataController.BoldHandle.ListElementType;
      lEvaluator := lContextType.Evaluator as TBoldOcl;
      Assert(Assigned(lEvaluator));
      lResultType := lEvaluator.ExpressionType(BoldProperties.Expression, lContextType, false, BoldProperties.VariableList);
      if Assigned(lResultType) then
      begin
        DataController.SetValueTypeAndProperties(lResultType, Item, (Item.RepositoryItem = nil) and (Item.PropertiesClassName = ''));
      end;
      if (Item.Caption = '') then
      begin
        lBoldMemberRTInfo := lEvaluator.RTInfo(BoldProperties.Expression, lContextType, false, BoldProperties.VariableList);
        if Assigned(lBoldMemberRTInfo) then
        begin
          Item.Caption := lBoldMemberRTInfo.ModelName;
        end;
      end;
    end;
end;

procedure TcxGridItemBoldDataBinding.Remove;
var
  i: integer;
  lBoldColumnsProperties: TBoldControllerList;
  lcxGridItemBoldDataBinding: TcxGridItemBoldDataBinding;
begin
  DataController.fBoldColumnsProperties.Remove(fBoldProperties);
  if (not GridView.IsDestroying) then
  begin
    lBoldColumnsProperties := DataController.fBoldColumnsProperties;
    for I := 0 to DataController.ItemCount - 1 do
    begin
      lcxGridItemBoldDataBinding := ((DataController.GetItem(i) as TcxCustomGridTableItem).DataBinding as TcxGridItemBoldDataBinding);
      lcxGridItemBoldDataBinding.Data := TObject(lBoldColumnsProperties.IndexOf(lcxGridItemBoldDataBinding.fBoldProperties));
    end;
  end;
  inherited;
end;

function TcxGridItemBoldDataBinding.GetBoldProperties: TBoldVariantFollowerController;
begin
  result := fBoldProperties;
end;

{ TcxBoldDataController }

function TcxBoldDataController.GetBoldHandle: TBoldAbstractListHandle;
begin
  if not assigned(fBoldHandleFollower) then
    result := nil
  else
    Result := fBoldHandleFollower.BoldHandle;
end;

procedure TcxBoldDataController.SetBoldHandle(
  const Value: TBoldAbstractListHandle);
begin
  if fBoldHandleFollower.BoldHandle <> Value then
  begin
    CustomDataSource.free;
    CustomDataSource := nil;
    fBoldHandleFollower.BoldHandle := value;
  end;
end;

function TcxBoldDataController.GetBoldHandleIndexLock: Boolean;
begin
  Result := fBoldHandleFollower.HandleIndexLock;
end;

procedure TcxBoldDataController.SetBoldHandleIndexLock(
  const Value: Boolean);
begin
  fBoldHandleFollower.HandleIndexLock := Value;
end;

procedure TcxBoldDataController.SetController(
  const Value: TBoldListAsFollowerListController);
begin
  fBoldProperties.Assign(Value);
end;

procedure TcxBoldDataController.SetDataChanged(const Value: boolean);
begin
  if fDataChanged = value then
    Exit;
  fDataChanged := Value;
  if Value then
    EnsureEventQueued;
end;

procedure TcxBoldDataController.SetRecNo(const Value: Integer);
begin
  fBoldHandleFollower.SetFollowerIndex(Value);
end;

constructor TcxBoldDataController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUseDelayedScrollUpdate := true;
  fClearColumnsOnTypeChange := true;  
  fBoldColumnsProperties := TBoldControllerList.Create(AOwner{GridView});
  fBoldProperties := TBoldListAsFollowerListController.Create(AOwner{GridView}, fBoldColumnsProperties);
  fBoldHandleFollower := TBoldListHandleFollower.Create(AOwner.Owner{Form}, fBoldProperties);
  Options := Options + [dcoImmediatePost];
  fSubscriber := TBoldPassthroughSubscriber.Create(Receive);

  if GetOwner is TcxComponent and TcxComponent(GetOwner).IsDesigning then
  begin
    // nothing needed at design time
  end
  else
  begin
//  don't need insert before we hook up datasource in _AfterMakeListUptoDate, so will set it there
//    fBoldProperties.OnAfterInsertItem := _InsertRow;
    fBoldProperties.OnAfterDeleteItem := _DeleteRow;
    fBoldProperties.OnReplaceitem := _ReplaceRow;
    fBoldProperties.AfterMakeUptoDate := _AfterMakeListUptoDate;
    fBoldProperties.BeforeMakeUptoDate := _BeforeMakeListUptoDate;
    fBoldProperties.OnGetContextType := GetHandleStaticType;
  end;
  fBoldProperties.OnGetContextType := GetHandleStaticType;
end;

function TcxBoldDataController.CreateDataControllerInfo: TcxCustomDataControllerInfo;
begin
  result := TcxBoldCustomDataControllerInfo.Create(Self);
end;

function TcxBoldDataController.CreateList: TBoldList;
begin
  if Assigned(fCurrentListElementType) then
    result := TBoldMemberFactory.CreateMemberFromBoldType(fCurrentListElementType.ListTypeInfo) as TBoldList
  else
    result := TBoldElementList.Create;
end;

procedure TcxBoldDataController.ProcessQueueEvent(Sender: TObject);
begin
//
end;

procedure TcxBoldDataController.BeforeSorting;
begin
  inherited;
//  DataControllerInfo.
end;

procedure TcxBoldDataController.BeginDelayScrollUpdate;
begin
  if InDelayScrollUpdate then
    exit;
  fInDelayScrollUpdate := true;
  inc(FSkipSyncFocusedRecord);
  fStoredRecordIndex := CurrentIndex;
end;

procedure TcxBoldDataController.EndDelayScrollUpdate;
begin
  if not InDelayScrollUpdate then
    exit;
  fInDelayScrollUpdate := false;
  dec(FSkipSyncFocusedRecord);
  if (FocusedRecordIndex <> Follower.CurrentIndex) then
  begin
    Follower.CurrentIndex := FocusedRecordIndex;
    CheckDataSetCurrent;
    if self is TcxGridBoldDataController then
    begin
      if TcxGridBoldDataController(self).GridView is TcxGridBoldTableView then
        with TcxCustomGridTableViewAccess(TcxGridBoldDataController(self).GridView), TcxGridBoldTableView(TcxGridBoldDataController(self).GridView) do
          DoFocusedRecordChanged(FPrevFocusedRecordIndex, FocusedRecordIndex, FPrevFocusedDataRecordIndex, FocusedDataRowIndex, false)
      else
      if TcxGridBoldDataController(self).GridView is TcxGridBoldBandedTableView then
        with TcxCustomGridTableViewAccess(TcxGridBoldDataController(self).GridView), TcxGridBoldBandedTableView(TcxGridBoldDataController(self).GridView) do
          DoFocusedRecordChanged(FPrevFocusedRecordIndex, FocusedRecordIndex, FPrevFocusedDataRecordIndex, FocusedDataRowIndex, false)
    end;
  end;
end;

procedure TcxBoldDataController.DisplayFollowers;
var
  Queueable: TBoldQueueable;
begin
  if Assigned(Follower) and (Follower.IsInDisplayList or not Follower.Displayable) then
  repeat
    Queueable := TBoldFollowerAccess(Follower).MostPrioritizedQueuableOrSelf;
    if Assigned(Queueable) then
      TBoldQueueableAccess(Queueable).display;
  until not Assigned(Queueable);
end;

function TcxBoldDataController.MainFollowerNeedsDisplay: boolean;
begin
  result := Assigned(Follower) and (Follower.IsInDisplayList or not Follower.Displayable);
end;

procedure TcxBoldDataController.EnsureEventQueued;
begin
  BoldInstalledQueue.AddEventToPostDisplayQueue(ProcessQueueEvent, nil, self)
end;

function TcxBoldDataController.EnsureFollower(ARecordIndex, AItemIndex: integer): boolean;
var
  lMainFollower: TBoldFollower;
  lRowFollower: TBoldFollower;
  lCellFollower: TBoldFollower;
  lResultElement: TBoldElement;
  lItem: TObject;
  lBoldAwareViewItem: IBoldAwareViewItem;
  lIcxBoldEditProperties: IcxBoldEditProperties;
begin
  result := false;
  lCellFollower := nil;
  lMainFollower := Follower;
  lItem := FindItemByData(AItemIndex);
  Assert(Assigned(lItem), 'lItem not found for index '+ IntToStr(AItemIndex));
  Assert(lItem.GetInterface(IBoldAwareViewItem, lBoldAwareViewItem), 'lItem.GetInterface(IBoldAwareViewItem, lBoldAwareViewItem)1');
  if not lMainFollower.Displayable then
  begin
//    Assert(IsDetailExpanding);
    DisplayFollowers;
  end;
  inc(FSkipMakeCellUptoDate);
  try
  //  if not lMainFollower.Displayable then
  //    DisplayFollowers;
    with TBoldFollowerList(lMainFollower.RendererData) do
    begin
      if (ARecordIndex < FirstActive) or (ARecordIndex > LastActive) then
      begin
        PreFetchColumns(nil, -1)
      end;//  AdjustActiveRange(ARecordIndex, -1);
    end;
    if ARecordIndex >= lMainFollower.SubFollowerCount then
    begin
      PreFetchColumns(nil, AItemIndex);
      AdjustActiveRange(nil, -1);
    end;
    if ARecordIndex >= lMainFollower.SubFollowerCount then
      Assert(false, Format('RecordIndex = %d Out of bounds, count = %d', [ARecordIndex, lMainFollower.SubFollowerCount]));
    lRowFollower := lMainFollower.SubFollowers[ARecordIndex];
    if not Assigned(lRowFollower) then
    begin
      PreFetchColumns(nil, -1);
      AdjustActiveRange(nil, AItemIndex);
      lRowFollower := lMainFollower.SubFollowers[ARecordIndex];
    end;
    if not Assigned(lRowFollower) then
      exit;
    if not lRowFollower.ElementValid then //  if not lRowFollower.Displayable then
      AdjustActiveRange(nil, AItemIndex);
    Assert(lRowFollower.ElementValid, 'lRowFollower.ElementValid');
    if not lRowFollower.Displayable then
      lRowFollower.EnsureDisplayable;
    Assert(lRowFollower.Active, 'lRowFollower.Active');
    if not lRowFollower.SubFollowerAssigned[AItemIndex] then
    begin
  //    AdjustActiveRange(nil, AItemIndex);
//      if not lRowFollower.SubFollowerAssigned[AItemIndex] then
        result := false;
//        AdjustActiveRange(nil, AItemIndex);//      Assert(false);
  //      lCellFollower := lRowFollower.SubFollowers[AItemIndex];
  //    Assert(lRowFollower.SubFollowerAssigned[AItemIndex], 'SubFollowerAssigned not assigned');
    end;
    try
      lRowFollower.EnsureDisplayable;
    except
      exit;
    end;
    lCellFollower := lRowFollower.SubFollowers[AItemIndex];
    Assert(Assigned(lCellFollower), 'lCellFollower not assigned');
    if not lCellFollower.Displayable then
      lCellFollower.EnsureDisplayable;
    Assert(lCellFollower.Displayable, ' lCellFollower not Displayable');
  finally
    result := Assigned(lCellFollower) and lCellFollower.Displayable;
    dec(FSkipMakeCellUptoDate);
  end;
end;

destructor TcxBoldDataController.Destroy;
begin
  BoldInstalledQueue.RemoveFromPostDisplayQueue(Self);
  fBoldProperties.OnAfterInsertItem := nil;
  fBoldProperties.OnAfterDeleteItem := nil;
  fBoldProperties.OnReplaceitem := nil;  
  FreeAndNil(fBoldHandleFollower);
  FreeAndNil(fBoldProperties);
  CustomDataSource.Free;
  CustomDataSource := nil;
  FreeAndNil(fBoldColumnsProperties);
  FreeAndNil(fSubscriber);
  FreeAndNil(fSelection);
  inherited Destroy;
end;

procedure TcxBoldDataController._BeforeMakeListUpToDate(
  Follower: TBoldFollower);
var
  i: integer;
begin
  inc(FSkipMakeCellUptoDate);
  BeginFullUpdate;

  if GetOwner is TcxComponent and TcxComponent(GetOwner).IsDesigning then exit;
//  if isPattern then exit;

  if (Assigned(CustomDataSource) and (GetHandleListElementType <> fCurrentListElementType)) or (not Assigned(CustomDataSource) and Assigned(fCurrentListElementType)) then
  begin
    fFetchedAll := false;
    CustomDataSource.free;
    fBoldProperties.OnAfterInsertItem := nil;
    fBoldProperties.OnAfterDeleteItem := nil;
    fBoldProperties.OnReplaceitem := nil;
  end;
  if (CustomDataSource = nil) then
  begin
// TODO:
//    if Assigned(OnBeforeLoad) then
//      OnBeforeLoad(GridView);
  end;
  if (CustomDataSource = nil) or (Follower.SubFollowerCount = 0) then
  begin
//    if not LoadAll then
    // we need to set ActiveRange here, otherwise all followers will be active and all objects will be fetched
      BoldProperties.SetActiveRange(Follower, 0, -1, 0);
  end;
  if Assigned(CustomDataSource) then
    TcxBoldDataSource(CustomDataSource).fIsBoldInitiatedChange := true;
  for i := 0 to ItemCount - 1 do
  begin
    BoldPropertiesFromItem(i).AfterMakeUptoDate := nil;
  end;
end;

procedure TcxBoldDataController._AfterMakeListUptoDate(
  Follower: TBoldFollower);
var
  lcxBoldDataSource: TcxBoldDataSource;
  lFirstLoad: boolean;
  i: integer;
begin
  {$IFDEF BoldDevExLog}
  _Log((GetOwner as TComponent).Name + ':_AfterMakeListUpToDate:' + IntToStr(FSkipMakeCellUptoDate), className);
  {$ENDIF}

  lFirstLoad := (CustomDataSource = nil);
  if lFirstLoad then
  begin
    lcxBoldDataSource := cxBoldDataSourceClass.Create(self);
    CustomDataSource := lcxBoldDataSource;
    TypeMayHaveChanged;
  end;
  dec(FSkipMakeCellUptoDate);

    CustomDataSource.DataChanged;
    for i := 0 to ItemCount - 1 do
    begin
      BoldPropertiesFromItem(i).AfterMakeUptoDate := _AfterMakeCellUptoDate;
    end;
  PreFetchColumns();
  EndFullUpdate;
  if Assigned(CustomDataSource) then
    TcxBoldDataSource(CustomDataSource).fIsBoldInitiatedChange := false;
end;

procedure TcxBoldDataController._InsertRow(index: Integer; Follower: TBoldFollower);
begin
  DataHasChanged := true;
end;

procedure TcxBoldDataController._ReplaceRow(index: Integer;
  AFollower: TBoldFollower);
begin
  DataHasChanged := true;
end;

procedure TcxBoldDataController._DeleteRow(index: Integer;
  owningFollower: TBoldFollower);
var
  vRowIndex: integer;
  vRecordIndex: integer;
begin
  DataHasChanged := true;
  if fInDeleteSelection then
    exit;
  with DataControllerInfo.Selection do
    if (DataControllerInfo.Selection.Count > 1) and IsRecordSelected(Index) then
      DataControllerInfo.Selection.Delete(FindByRecordIndex(Index));
end;

function TcxBoldDataController.GetHandleStaticType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

procedure TcxBoldDataController._AfterMakeCellUptoDate(
  Follower: TBoldFollower);
begin
// TODO: ?
end;

function TcxBoldDataController.GetFollower: TBoldFollower;
begin
  Result := nil;
  if Assigned(fBoldHandleFollower) then
    Result := fBoldHandleFollower.Follower;
end;

function TcxBoldDataController.GetCellFollower(ARecordIndex, AItemIndex: Integer): TBoldFollower;
var
  lRowFollower: TBoldFollower;
begin
  lRowFollower := GetRowFollower(ARecordIndex);
  if assigned(lRowFollower) and
    (AItemIndex >= 0) and
    (AItemIndex < lRowFollower.SubFollowerCount) then
    Result := lRowFollower.SubFollowers[AItemIndex]
  else
    result := nil;
end;

function TcxBoldDataController.GetHasCellFollower(ARecordIndex, AItemIndex: Integer): boolean;
var
  lRowFollower: TBoldFollower;
begin
  lRowFollower := GetRowFollower(ARecordIndex);
  result := assigned(lRowFollower) and lRowFollower.SubFollowerAssigned[AItemIndex];
end;

function TcxBoldDataController.GetDataProviderClass: TcxCustomDataProviderClass;
begin
  Result := TcxBoldCustomDataProvider;
end;

function TcxBoldDataController.GetRecNo: Integer;
begin
  result := Follower.CurrentIndex;
end;

function TcxBoldDataController.GetRecordCount: Integer;
begin
  if (DetailMode = dcdmPattern) or not Assigned(TcxCustomDataProviderAccess(provider).CustomDataSource) then
    result := inherited GetRecordCount
  else
  begin
    Result := TcxBoldDataSource(TcxCustomDataProviderAccess(provider).CustomDataSource).GetRecordCount;
  end;
end;

function TcxBoldDataController.GetRowFollower(
  DataRow: Integer): TBoldFollower;
var
  lFollower: TBoldFollower;
begin
  lFollower := Follower;
  if datarow < lFollower.SubFollowerCount then
    Result := lFollower.SubFollowers[DataRow]
  else
    result := nil;
end;

function TcxBoldDataController.GetSearchClass: TcxDataControllerSearchClass;
begin
  result := TcxBoldDataControllerSearch;
end;

function TcxBoldDataController.InDelayScrollUpdate: boolean;
begin
  result := FUseDelayedScrollUpdate and fInDelayScrollUpdate;
end;

function TcxBoldDataController.IsDataLinked: Boolean;
begin
  Result := BoldHandle <> nil;
end;

function TcxBoldDataController.IsSmartRefresh: Boolean;
begin
  result := false;
end;

procedure TcxBoldDataController.AdjustActiveRange(aList: TBoldList = nil; aItem: integer = -1);
var
  lFollower: TBoldFollower;
  i,j: integer;
begin
  lFollower := Follower;
  if Assigned(lFollower) and Assigned(lFollower.element) and (lFollower.Element is TBoldList) then
  begin
    if RequiresAllRecords or LoadAll then
      BoldProperties.SetActiveRange(lFollower, 0, lFollower.SubFollowerCount-1, 0)
    else
    begin
      FindMinMaxIndex(aList, BoldList, i, j);
      BoldProperties.SetActiveRange(lFollower, i, j, 0);
    end;
{    if aList = nil then
      PreFetchColumns(BoldList, aItem)
    else
      PreFetchColumns(aList, aItem);}
  end;
end;

procedure TcxBoldDataController.AdjustActiveRange(aRecordIndex: integer; aItem: integer = -1);
var
  lList: TBoldObjectList;
begin
  if (Follower.Element is TBoldObjectList) and (aRecordIndex >= 0) and (aRecordIndex < TBoldObjectList(Follower.Element).Count) then
  begin
    lList := TBoldObjectList.Create;
    try
      lList.Add( TBoldObjectList(Follower.Element)[aRecordIndex]);
      AdjustActiveRange(lList, aItem);
    finally
      lList.free;
    end;
  end;
end;

function TcxBoldDataController.TypeMayHaveChanged: boolean;
var
  lNewListElementType: TBoldElementTypeInfo;
  lOldListElementType: TBoldElementTypeInfo;
begin
  result := false;
  if not Assigned(BoldHandle) or not Assigned(BoldHandle.List) then
  begin
    if Assigned(fCurrentListElementType) then
    begin
      lOldListElementType := fCurrentListElementType;
      fCurrentListElementType := nil;
      TypeChanged(nil, lOldListElementType);
      result := true;
    end;
  end
  else
  begin
    lNewListElementType := GetHandleListElementType;
    if (lNewListElementType <> fCurrentListElementType) then
    begin
      {$IFDEF BoldDevExLog}
      if Assigned(fCurrentListElementType) then
      begin
        if Assigned(lNewListElementType) then
          _Log((GetOwner as TComponent).Name + ':TypeMayHaveChanged:' + fCurrentListElementType.AsString + '->' + lNewListElementType.AsString, className)
        else
          _Log((GetOwner as TComponent).Name + ':TypeMayHaveChanged:' + fCurrentListElementType.AsString + '-> nil', className);
      end
      else
        _Log((GetOwner as TComponent).Name + ':TypeMayHaveChanged:' + lNewListElementType.AsString, className);
      {$ENDIF}
      lOldListElementType := fCurrentListElementType;
      fCurrentListElementType := lNewListElementType;
      TypeChanged(lNewListElementType, lOldListElementType);
      result := true;
    end;
  end;
end;

function TcxBoldDataController.GetHandleListElementType: TBoldElementTypeInfo;
begin
  if Assigned(BoldHandle) then
    Result := BoldHandle.ListElementType //BoldType
  else
    Result := nil;
end;

function TcxBoldDataController.BoldPropertiesFromItem(
  aIndex: integer): TBoldVariantFollowerController;
var
  lBoldAwareViewItem: IBoldAwareViewItem;
begin
  if GetItem(aIndex).GetInterface(IBoldAwareViewItem, lBoldAwareViewItem) then
    result := lBoldAwareViewItem.BoldProperties
  else
    result := nil;
end;

function TcxBoldDataController.BoldSetValue(AItemHandle: TcxDataItemHandle;
  ACellFollower: TBoldFollower; const AValue: variant): boolean;
begin
  result := false;
end;

function TcxBoldDataController.GetCurrentBoldObject: TBoldObject;
begin
  if GetCurrentElement is TBoldObject then
    result := TBoldObject(GetCurrentElement)
  else
    result := nil;
end;

function TcxBoldDataController.GetCurrentElement: TBoldElement;
var
  lFollower: TBoldFollower;
begin
  if CurrentIndex = -1 then
    result := nil
  else
  begin
    lFollower := Follower.CurrentSubFollower;
    if Assigned(lFollower) then
      result := lFollower.Element
    else
      result := nil;
  end;
end;

function TcxBoldDataController.GetCurrentIndex: integer;
begin
  if InDelayScrollUpdate then
    result := fStoredRecordIndex
  else
    result := Follower.CurrentIndex;
end;


procedure TcxBoldDataController.Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    beSystemDestroying:
    begin
      fCurrentListElementType := nil;
    end;
    beSelectionDestroying:
    begin
      fSelection := nil;
      raise EcxGridBoldSupport.Create('Grid Selection destroyed, do not free the grid selection !');
    end;
  end;
end;

function TcxBoldDataController.RequiresAllRecords(AItem: TObject): boolean;
begin
  result := true;
end;

function TcxBoldDataController.RequiresAllRecords: boolean;
begin
  result := true;
end;

procedure TcxBoldDataController.TypeChanged(aNewType, aOldType: TBoldElementTypeInfo);
begin
  fSubscriber.CancelAllSubscriptions;
  FreeAndNil(fSelection);
  if Assigned(aNewType) then
    aNewType.SystemTypeInfo.AddSubscription(fSubscriber, beDestroying, beSystemDestroying);
end;

function TcxBoldDataController.IsProviderMode: Boolean;
begin
  result := true;
end;

procedure TcxBoldDataController.PreFetchColumns(aList: TBoldList; aItem: integer);
begin
end;

function TcxBoldDataController.GetCurrentDetailDataController(ARelationIndex: integer = 0): TcxBoldDataController;
begin
  if CurrentIndex = -1 then
    result := nil
  else
    result := GetDetailDataController(CurrentIndex, ARelationIndex) as TcxBoldDataController;
end;

procedure TcxBoldDataController.FilterChanged;
begin
  fSkipCancel := true;
  try
    inherited;
  finally
    fSkipCancel := false;
  end;
end;

function TcxBoldDataController.FindItemByData(AData: Integer): TObject;
var
  I: Integer;
begin
  if (AData > -1) and (AData < ItemCount) then
  begin
    Result := GetItem(AData);
    if GetItemData(Result) = AData then
      Exit;
  end;
  for I := 0 to ItemCount - 1 do
  begin
    Result := GetItem(I);
    if GetItemData(Result) = AData then
      Exit;
  end;
  Result := nil;
end;

procedure TcxBoldDataController.FindMinMaxIndex(ListA, ListB: TBoldList;
  var AFrom, ATo: integer);
var
  i,j: integer;
  vCount: integer;
begin
  vCount := ListB.Count;
  if (ListA is TBoldObjectList) and (ListB is TBoldObjectList) then
  begin
    AFrom := maxInt;
    ATo := -1;
    for I := 0 to ListA.Count - 1 do
    begin
      if TBoldObjectList(ListB).Locators[i] = TBoldObjectList(ListA).Locators[i] then
        j := i
      else
        j := TBoldObjectList(ListB).IndexOfLocator(TBoldObjectList(ListA).Locators[i]);
      AFrom := Min(AFrom, j);
      ATo := max(ATo, j);
      if (AFrom = 0) and (ATo = vCount-1) then
        exit;
    end;
  end
  else
  begin
    AFrom := maxInt;
    ATo := -1;
    for I := 0 to ListA.Count - 1 do
    begin
      j := ListB.IndexOf(ListA[i]);
      AFrom := Min(AFrom, j);
      ATo := max(ATo, j);
      if (AFrom = 0) and (ATo = vCount-1) then
        exit;
    end;
    if (ATo = -1) and (ListA.Count > 0) and (ListB.Count > 0) then
    begin
      AFrom := 0;
      ATo := ListB.Count -1;
    end;
  end;
end;

function TcxBoldDataController.GetSelection: TBoldList;
var
  ListType: TBoldElementTypeInfo;
begin
  if not Assigned(fSelection) then
  begin
    fSelection := CreateList;
    fSelection.AddSubscription(fSubscriber, beDestroying, beSelectionDestroying);
    SelectionChanged;
  end;
  result := fSelection;
end;

procedure TcxBoldDataController.Cancel;
var
  i: integer;
  lRowFollower: TBoldFollower;
begin
  if fSkipCancel then
    exit;
  if FocusedRecordIndex = -1 then
    inherited
  else
  begin
    lRowFollower := Follower.SubFollowers[FocusedRecordIndex];
    for i := 0 to lRowFollower.SubFollowerCount - 1 do
    begin
      lRowFollower.SubFollowers[i].DiscardChange;
    end;
  end;
end;

procedure TcxBoldDataController.SelectionChanged;
begin
// do nothing
end;

function TcxBoldDataController.GetBoldList: TBoldList;
begin
  if Assigned(BoldHandle) then
    result := BoldHandle.List //Follower.Element as TBoldList;
  else
    result := nil;
end;

{ TcxBoldDataSource }

constructor TcxBoldDataSource.Create(
  aBoldDataController: TcxBoldDataController);
begin
  inherited Create;
  fBoldDataController := aBoldDataController;
end;

function TcxBoldDataSource.GetRecordCount: Integer;
begin
  result := fBoldDataController.Follower.SubFollowerCount;
  if not fBoldDataController.fInternalLoading then
    if (fBoldDataController.Follower.state <> bfsCurrent) and  (result <> fBoldDataController.DataStorage.RecordCount) then
      result := fBoldDataController.DataStorage.RecordCount;
end;

function TcxBoldDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var
  lMainFollower: TBoldFollower;
  lCellFollower: TBoldFollower;
  lItemIndex: integer;
  lRecordIndex: integer;
  lResultElement: TBoldElement;
  lItem: TObject;
  lcxBoldDataController: TcxBoldDataController;
  lBoldAwareViewItem: IBoldAwareViewItem;
  lIcxBoldEditProperties: IcxBoldEditProperties;
begin
  result := null;
  lItemIndex := Integer(AItemHandle);
  lRecordIndex := Integer(ARecordHandle);
  lcxBoldDataController := DataController as TcxBoldDataController;
  if not lcxBoldDataController.EnsureFollower(lRecordIndex, lItemIndex) then
    exit;
  lMainFollower := lcxBoldDataController.Follower;
  lCellFollower := lMainFollower.SubFollowers[lRecordIndex].SubFollowers[lItemIndex];
  lItem := lcxBoldDataController.FindItemByData(lItemIndex);
  if (lItem is TcxCustomGridTableItem) and Supports(TcxCustomGridTableItemAccess(lItem).GetProperties, IcxBoldEditProperties, lIcxBoldEditProperties) then
  begin
    lResultElement := lCellFollower.Value;
    if Assigned(lResultElement) then
      result := lIcxBoldEditProperties.BoldElementToEditValue(lCellFollower, lResultElement, nil)
    else
      result := (lCellFollower.Controller as TBoldVariantFollowerController).GetCurrentAsVariant(lCellFollower);
  end
  else
    result := (lCellFollower.Controller as TBoldVariantFollowerController).GetCurrentAsVariant(lCellFollower);
end;

procedure TcxBoldDataSource.SetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle; const AValue: Variant);
var
  lRowFollower: TBoldFollower;
  lCellFollower: TBoldFollower;
  lItemIndex: integer;
  lDone: boolean;
begin
  lItemIndex := Integer(AItemHandle);

  lRowFollower := fBoldDataController.Follower.SubFollowers[Integer(ARecordHandle)];
  Assert(Assigned(lRowFollower));

  inc(fBoldDataController.FSkipMakeCellUptoDate);
  try
    lRowFollower.EnsureDisplayable;
  finally
    dec(fBoldDataController.FSkipMakeCellUptoDate);
  end;
  lCellFollower := lRowFollower.SubFollowers[lItemIndex];

  lDone := fBoldDataController.BoldSetValue(AItemHandle, lCellFollower, AValue);

  if not lDone then
  begin
    (lCellFollower.Controller as TBoldVariantFollowerController).MayHaveChanged(aValue, lCellFollower);
  end;
  if (lCellFollower.State = bfsDirty) and (lCellFollower.Controller.ApplyPolicy <> bapDemand) then
  begin
    lCellFollower.Apply;
    {$IFDEF DisplayAll}
    inc(fBoldDataController.FSkipMakeCellUptoDate);
    try
      TBoldQueueable.DisplayAll;
    finally
      dec(fBoldDataController.FSkipMakeCellUptoDate);
    end;
    {$ENDIF}
  end;
end;

function TcxBoldDataSource.IsCustomSorting: Boolean;
var
  i: integer;
begin
  result := false;

  with TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo) do
  for I := 0 to RecordList.Count - 1 do
    Assert(Integer(RecordList.Items[i]) < DataController.RecordCount);
end;

procedure TcxBoldDataSource.CustomSort;
var
  i,j: integer;
  lOcl: string;
  SourceList: TBoldList;
begin
  SourceList := fBoldDataController.BoldList;
  for I := 0 to TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).TotalSortingFieldList.Count - 1 do
  begin
    j := TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).TotalSortingFieldList[i].Field.Index;
    lOcl := fBoldDataController.BoldColumnsProperties[j].Expression;
//    FetchOclSpan(SourceList, lOcl);
  end;
  SourceList.Sort(CustomSortElementCompare);
end;

function TcxBoldDataSource.CustomSortElementCompare(Item1,
  Item2: TBoldElement): integer;
var
  i,j: integer;
  IE1, IE2: TBoldIndirectElement;
begin
  result := 0;
  IE1 := TBoldIndirectElement.Create;
  IE2 := TBoldIndirectElement.Create;
  try
    for I := 0 to TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).TotalSortingFieldList.Count - 1 do
    begin
      j := TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).TotalSortingFieldList[i].Field.Index;
      Item1.EvaluateExpression(fBoldDataController.BoldColumnsProperties[j].Expression, ie1);
      Item2.EvaluateExpression(fBoldDataController.BoldColumnsProperties[j].Expression, ie2);
      if Assigned(IE1.Value) then
        result := IE1.Value.CompareTo(IE2.Value);
    end;
  finally
    ie1.free;
    ie2.free;
  end;
end;

procedure TcxBoldDataSource.DeleteRecord(
  ARecordHandle: TcxDataRecordHandle);
var
  lRowFollower: TBoldFollower;
  lIndex: integer;
  lBoldObject: TBoldObject;
begin
  if not fIsBoldInitiatedChange then
  begin
    lIndex := Integer(ARecordHandle);
    lRowFollower := fBoldDataController.Follower.SubFollowers[lIndex];
    Assert(Assigned(lRowFollower));
    Assert(lIndex = lRowFollower.index);
    fIsBoldInitiatedChange := true;
    try
      {$IFDEF DisplayAll}
      TBoldQueueable.DisplayAll;
      {$ENDIF}
//      Assert(lRowFollower.Element = fBoldDataController.Follower.SubFollowers[lIndex].Element, 'lRowFollower.Element = fBoldDataController.Follower.SubFollowers[lIndex].Element');
      lBoldObject := (lRowFollower.Element as TBoldObject);
      if Assigned(lBoldObject) then
        Assert(lBoldObject = fBoldDataController.BoldHandle.ObjectList[lIndex], 'lBoldObject = fBoldDataController.BoldHandle.ObjectList[lIndex]')
      else
        lBoldObject := fBoldDataController.BoldHandle.ObjectList[lIndex];

      if lBoldObject.BoldClassTypeInfo.IsLinkClass then
      begin
        if lBoldObject.BoldObjectExists then // not already deleted
          lBoldObject.delete;
      end
      else
      begin
        fBoldDataController.BoldHandle.MutableList.Remove(lBoldObject);
      end;
{
      if lBoldObject.BoldObjectExists then // not already deleted
        lBoldObject.delete;
}
//      TBoldQueueable.DisplayAll;
      {$IFDEF BoldDevExLog}
      _Log((TcxBoldDataController(DataController).GetOwner as TComponent).Name + ':DataChanged4', ClassName);
      {$ENDIF}
      TcxBoldDataController(DataController).DataHasChanged := true; //DataChanged;
{
      if (lIndex < GetRecordCount) and (GetRecordCount > 0)then
      begin
       (fBoldDataController.GridView.Controller as TcxGridTableController).FocusedRow.Selected := true;
      end;
}
    finally
      fIsBoldInitiatedChange := false;
    end;
  end;
end;

function TcxBoldDataSource.GetDetailHasChildren(ARecordIndex,
  ARelationIndex: Integer): Boolean;
var
  lcxCustomDataController: TcxCustomDataController;
  lFollower: TBoldFollower;
  lOcl: string;
  Ie: TBoldIndirectElement;
  lDetailObject: TcxDetailObject;
  lObject: TObject;
  lGridLevel: TcxGridLevel;
  lcxCustomDataRelation: TcxCustomDataRelation;
  lPatternView: IBoldAwareView;
  lcxCustomDataProviderAccess: TcxCustomDataProviderAccess;
begin
  lcxCustomDataProviderAccess := TcxCustomDataProviderAccess(CurrentProvider);
  Assert(fBoldDataController = lcxCustomDataProviderAccess.DataController);
  result := false;

  if not (lcxCustomDataProviderAccess.DataController is TcxGridBoldDataController) then
    exit;
//  lDetailObject := fBoldDataController.GetDetailLinkObject(ARecordIndex, ARelationIndex) as TcxDetailObject;

  lDetailObject := lcxCustomDataProviderAccess.DataController.Relations.GetDetailObject(ARecordIndex);
  lObject := lDetailObject.LinkObjects[ARelationIndex];

  if Assigned(lObject) then
  begin
    lcxCustomDataController := lcxCustomDataProviderAccess.DataController.GetDetailDataController(ARecordIndex, ARelationIndex);
    result := lcxCustomDataController.RecordCount > 0;
  end
  else
  begin
    lcxCustomDataRelation := lcxCustomDataProviderAccess.DataController.Relations[ARelationIndex];
    lGridLevel := lcxCustomDataRelation.Item as TcxGridLevel;
    if lGridLevel.GridView.IsPattern and (lGridLevel.GridView <> TcxGridBoldDataController(lcxCustomDataProviderAccess.DataController).GridView) then
    begin
      lPatternView := (lGridLevel.GridView as IBoldAwareView);
      if Assigned(lPatternView.DataController.BoldHandle) and (lPatternView.DataController.BoldHandle is TBoldListHandle) then
      begin
        TBoldQueueable.DisplayAll;
        lOcl := (lPatternView.DataController.BoldHandle as TBoldListHandle).Expression;
        Ie := TBoldIndirectElement.Create;
        try
          TcxBoldDataController(lcxCustomDataProviderAccess.DataController).AdjustActiveRange();
          lFollower := TcxBoldDataController(lcxCustomDataProviderAccess.DataController).Follower.SubFollowers[ARecordIndex];
          if not Assigned(lFollower) or not Assigned(lFollower.Element) then
            exit;
          if (lPatternView.DataController.BoldHandle as TBoldListHandle).Variables <> nil then
            lFollower.Element.EvaluateExpression(lOcl, Ie, false, (lPatternView.DataController.BoldHandle as TBoldListHandle).Variables.VariableList)
          else
            lFollower.Element.EvaluateExpression(lOcl, Ie);
          if Ie.Value is TBoldList then
          begin
            result := TBoldList(Ie.Value).Count > 0;
          end
          else
            result := false; //Assigned(Ie.Value);
        finally
          Ie.free;
        end;
      end;
    end;
  end;
end;

function TcxBoldDataSource.GetItemHandle(
  AItemIndex: Integer): TcxDataItemHandle;
var
  lItem: TObject;
begin
  lItem := TcxCustomDataProviderAccess(CurrentProvider).DataController.GetItem(AItemIndex);
  if lItem is TcxCustomGridTableItem then
    result := TcxCustomGridTableItem(lItem).DataBinding.Data
  else
    result := TcxDataItemHandle(AItemIndex); // this handles cxLookupGrid which doesn't allow column moving so indexes are static
end;

function TcxBoldDataSource.GetRecordHandle(
  ARecordIndex: Integer): TcxDataRecordHandle;
begin
  result := TcxDataRecordHandle(ARecordIndex);
end;

function TcxBoldDataSource.GetRecordHandleByIndex(
  ARecordIndex: Integer): TcxDataRecordHandle;
begin
  Result := TcxDataRecordHandle(ARecordIndex);
end;

destructor TcxBoldDataSource.Destroy;
begin
  inherited;
end;

function TcxBoldDataSource.IsRecordIdSupported: Boolean;
begin
  result := true;
end;

procedure TcxBoldDataSource.LoadRecordHandles;
begin
{$IFDEF BoldDevExLog}
  if Assigned(fBoldDataController.BoldHandle) then
    _Log(fBoldDataController.BoldHandle.Name + ':' + IntToStr(GetRecordCount), 'recordhandles');
{$ENDIF}
  inherited;
end;

function TcxBoldDataSource.GetRecordId(
  ARecordHandle: TcxDataRecordHandle): Variant;
begin
  result := Integer(ARecordHandle);
end;

{ TcxGridBoldColumn }

function TcxGridBoldColumn.CalculateBestFitWidth: Integer;
begin
  GridView.OptionsBehavior.BestFitMaxRecordCount := GridView.ViewInfo.VisibleRecordCount;
  result := inherited CalculateBestFitWidth;
end;

destructor TcxGridBoldColumn.Destroy;
begin
  DataBinding.Remove;
  inherited;
end;

function TcxGridBoldColumn.GetDataBinding: TcxGridItemBoldDataBinding;
begin
  Result := TcxGridItemBoldDataBinding(inherited DataBinding);
end;

function TcxGridBoldColumn.GetProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('OnCustomDrawCell');
  AProperties.Add('OnGetFilterValues');
  AProperties.Add('OnUserFilteringEx');
  AProperties.Add('OnGetCellHint');
  AProperties.Add('Sorting');
  AProperties.Add('Grouping');
  AProperties.Add('Filtering');
  AProperties.Add('Caption');
  AProperties.Add('IncSearch');
  AProperties.Add('Expression');
  AProperties.Add('Renderer');
  Result := inherited GetStoredProperties(AProperties);
end;

procedure TcxGridBoldColumn.GetPropertyValue(const AName: string;
  var AValue: Variant);
var
  method: TMethod;
  Renderer: TBoldRenderer;
begin
  if Assigned(OnCustomDrawCell) and (AName = 'OnCustomDrawCell') then
  begin
    method := TMethod(OnCustomDrawCell);
    if method.Data <> DataController then
      AValue := TObject(Method.Data).MethodName(Method.Code);
  end
  else
  if Assigned(OnGetFilterValues) and (AName = 'OnGetFilterValues') then
  begin
    method := TMethod(OnGetFilterValues);
    if method.Data <> DataController then
      AValue := TObject(Method.Data).MethodName(Method.Code);
  end
  else
  if Assigned(OnUserFilteringEx) and (AName = 'OnUserFilteringEx') then
  begin
    method := TMethod(OnUserFilteringEx);
    if method.Data <> DataController then
      AValue := TObject(Method.Data).MethodName(Method.Code);
  end
  else
  if Assigned(OnGetCellHint) and (AName = 'OnGetCellHint') then
  begin
    method := TMethod(OnGetCellHint);
    if method.Data <> DataController then
      AValue := TObject(Method.Data).MethodName(Method.Code);
  end
  else
  if AName = 'Sorting' then
    AValue := Options.Sorting
  else
  if AName = 'Grouping' then
    AValue := Options.Grouping
  else
  if AName = 'Filtering' then
    AValue := Options.Filtering
  else
  if AName = 'Caption' then
    AValue := Caption
  else
  if AName = 'IncSearch' then
    AValue := Options.IncSearch
  else
  if AName = 'Expression' then
    AValue := DataBinding.BoldProperties.expression
  else
  if AName = 'Renderer' then
  begin
    if Assigned(DataBinding.BoldProperties.Renderer) then
    begin
      Renderer := DataBinding.BoldProperties.Renderer;
      if Assigned(Renderer.Owner) and (Renderer.Owner <> Owner.Owner) then
      begin
        AValue := Renderer.Owner.Name + '.'+ Renderer.Name;
      end
      else
        AValue := Renderer.Name;
    end;
  end
  else
    inherited;
end;

procedure TcxGridBoldColumn.SetDataBinding(
  Value: TcxGridItemBoldDataBinding);
begin
  inherited DataBinding := Value;
end;

procedure TcxGridBoldColumn.SetPropertyValue(const AName: string;
  const AValue: Variant);

  function FindEvent(const AEventName: string): TMethod;
  var
    vObject: TObject;
  begin
    result.Code := nil;
    result.Data := nil;
    vObject := self;
    repeat
      if Integer(vObject.MethodAddress(AEventName)) <> 0 then
      begin
        result.Code := vObject.MethodAddress(AEventName);
        result.Data := vObject;
      end
      else
      if vObject is TComponent then
        vObject := TComponent(vObject).Owner;
    until (Integer(result.Code) <> 0) or not Assigned(vObject);
  end;

var
  Renderer: TBoldCustomAsVariantRenderer;
  s: string;
  i: integer;
  Component: TComponent;
begin
  if AName = 'OnCustomDrawCell' then
  begin
    if AValue = '' then
      OnCustomDrawCell := nil
    else
      OnCustomDrawCell := TcxGridTableDataCellCustomDrawEvent(FindEvent(AValue));
  end
  else
  if AName = 'OnGetFilterValues' then
  begin
    if AValue = '' then
      OnGetFilterValues := nil
    else
      OnGetFilterValues := TcxGridGetFilterValuesEvent(FindEvent(AValue));
  end
  else
  if AName = 'OnUserFilteringEx' then
  begin
    if AValue = '' then
      OnUserFilteringEx := nil
    else
      OnUserFilteringEx := TcxGridUserFilteringExEvent(FindEvent(AValue));
  end
  else
  if AName = 'OnGetCellHint' then
  begin
    if AValue = '' then
      OnGetCellHint := nil
    else
      OnGetCellHint := TcxGridGetCellHintEvent(FindEvent(AValue));
  end
  else
  if AName = 'Sorting' then
    Options.Sorting := AValue
  else
  if AName = 'Grouping' then
    Options.Grouping := AValue
  else
  if AName = 'Filtering' then
    Options.Filtering := AValue
  else
  if AName = 'Caption' then
    Caption := AValue
  else
  if AName = 'IncSearch' then
  begin
    Options.IncSearch := AValue;
    if Options.IncSearch then
      GridView.OptionsBehavior.IncSearchItem := self;
  end
  else
  if AName = 'Expression' then
    DataBinding.BoldProperties.expression := AValue
  else
  if AName = 'Renderer' then
  begin
    i := Pos('.', AValue);
    if i > 1 then
    begin
      s := Copy(aValue, 1, i-1);
      Component := Application.FindComponent(s);
      s := Copy(aValue, i+1, maxint);
      Renderer := Component.FindComponent(s) as TBoldCustomAsVariantRenderer;
    end
    else
    begin
      Renderer := FindComponent(AValue) as TBoldCustomAsVariantRenderer;
      if not Assigned(Renderer) and Assigned(GridView.Owner) then
        Renderer := GridView.Owner.FindComponent(AValue) as TBoldCustomAsVariantRenderer;
    end;
    DataBinding.BoldProperties.Renderer := Renderer;
  end
  else
    inherited;
end;

procedure TcxGridBoldColumn.VisibleChanged;
begin
  inherited;
//  if Visible and not IsLoading then
//    (DataController as TcxGridBoldDataController).AdjustActiveRange();
end;

{ TcxGridBoldTableView }

function TcxGridBoldTableView.DoCellDblClick(
  ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
  AShift: TShiftState): Boolean;
var
  lAutoForm: TForm;
  lElement: TBoldElement;
begin
  result := false;
  if DataController.BoldProperties.DefaultDblClick and not Controller.IsSpecialRowFocused and Assigned(DataController.Follower.CurrentSubFollower) then
  begin
    lElement := DataController.Follower.CurrentSubFollower.Element;
    lAutoForm := AutoFormProviderRegistry.FormForElement(lElement);
    if assigned(lAutoForm) then
    begin
      result := true;
      lAutoForm.Show;
    end
  end;
  if not result then
  begin
    result := inherited DoCellDblClick(ACellViewInfo, AButton, AShift);
  end;
end;

//{$IFNDEF UseBoldEditors}
//type
//  IcxBoldEditProperties = Interface
//  ['{D50859F1-F550-4CE6-84DE-5074921512E5}']
//    procedure SetStoredValue(aEdit: TcxCustomEdit; aFollower: TBoldFollower; var aDone: boolean);
//  end;
//{$ENDIF}

function TcxGridBoldTableView.DoEditing(
  AItem: TcxCustomGridTableItem): Boolean;
begin
  if Controller.IsSpecialRowFocused then
  begin
    result := inherited DoEditing(aItem);
  end
  else
  begin
    result := DataController.DoEditing(AItem) and inherited DoEditing(aItem);
  end;
end;

procedure TcxGridBoldTableView.DoEditKeyPress(
  AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit; var Key: Char);
var
  lRecord: integer;
  lFollower: TBoldFollower;
begin
  lRecord := DataController.RecNo;
  if (lRecord <> -1) and (Key <> #8) and not Controller.IsSpecialRowFocused then
  begin
    lFollower := DataController.CellFollowers[lRecord, AItem.ID];
    if not (lFollower.AssertedController as TBoldVariantFollowerController).ValidateCharacter(key, lFollower) then
    begin
      key := #0;
    end;
  end;
  inherited;
end;

{$IFDEF DelayOnFocusedRecordChange}
procedure TcxGridBoldTableView.InheritedDoFocusedRecordChanged(APrevFocusedRecordIndex,
  AFocusedRecordIndex, APrevFocusedDataRecordIndex,
  AFocusedDataRecordIndex: Integer; ANewItemRecordFocusingChanged: Boolean);
begin
  if AFocusedRecordIndex >= TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).RecordList.Count then
  begin
    DataController.DataControllerInfo.Refresh;
    Assert(DataController.FilteredRecordCount = TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).RecordList.Count, Format('DataController.FilteredRecordCount = %d <> DataController.DataControllerInfo.GetRowCount = %d', [DataController.FilteredRecordCount, TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).RecordList.Count]));
  end;
  inherited DoFocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex, APrevFocusedDataRecordIndex, AFocusedDataRecordIndex, ANewItemRecordFocusingChanged);
end;
{$ENDIF}

procedure TcxGridBoldTableView.DoFocusedRecordChanged(APrevFocusedRecordIndex,
  AFocusedRecordIndex, APrevFocusedDataRecordIndex,
  AFocusedDataRecordIndex: Integer; ANewItemRecordFocusingChanged: Boolean);
begin
  if DataController.InDelayScrollUpdate then
  begin
    if FPrevFocusedRecordIndex = MaxInt then // MaxInt used as flag to only store Prev values once
    begin
      FPrevFocusedRecordIndex := APrevFocusedRecordIndex;
      FPrevFocusedDataRecordIndex := APrevFocusedDataRecordIndex;
    end;
  end
  else
{$IFDEF DelayOnFocusedRecordChange}
  if Assigned(OnFocusedRecordChanged)then
  begin
    DataController.EnsureEventQueued;
    DataController.fFocusChanged := true;
    DataController.fPrevFocusedRecordIndex := APrevFocusedRecordIndex;
    DataController.fFocusedRecordIndex := AFocusedRecordIndex;
    DataController.fPrevFocusedDataRecordIndex := APrevFocusedDataRecordIndex;
    DataController.fFocusedDataRecordIndex := AFocusedDataRecordIndex;
    DataController.fNewItemRecordFocusingChanged := ANewItemRecordFocusingChanged;
  end
  else
{$ENDIF}  
  begin
//    if not TBoldQueueable.IsDisplayQueueEmpty and not TBoldQueueable.IsDisplaying then
//      TBoldQueueable.DisplayAll;
    inherited;
    FPrevFocusedRecordIndex := MaxInt;
  end;
end;

procedure TcxGridBoldTableView.DoChanged(AChangeKind: TcxGridViewChangeKind);
begin
  if Assigned(DataController) and Assigned(DataController.CustomDataSource) and DataController.MainFollowerNeedsDisplay then
    if DataController.LockCount = 0 then
      DataController.DisplayFollowers;
  inherited;
end;

procedure TcxGridBoldTableView.DoCustomDrawCell(ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
  CellFollower: TBoldFollower;
  Controller: TBoldVariantFollowerController;
  Color: TColor;
begin
  Color := ACanvas.Brush.Color;
  inherited DoCustomDrawCell(ACanvas, AViewInfo, ADone);
  if ADone then
    exit;
  CellFollower := (AViewInfo.GridView as IBoldAwareView).DataController.CellFollowers[AViewInfo.RecordViewInfo.GridRecord.RecordIndex, AViewInfo.Item.ID];
  if Assigned(CellFollower) then
  begin
    Controller := CellFollower.Controller as TBoldVariantFollowerController;
    Controller.SetColor(Color, Color, CellFollower);
    if Color > -1 then
      ACanvas.Brush.Color := Color;
  end;
end;

procedure TcxGridBoldTableView.DoItemsAssigned;
begin
  DataController.RestructData;
  inherited;
end;

function TcxGridBoldTableView.GetSelection: TBoldList;
begin
  result := DataController.Selection;
end;

procedure TcxGridBoldTableView.DoSelectionChanged;
begin
  DataController.SelectionChanged;
  inherited;
end;

function TcxGridBoldTableView.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  result := TcxGridBoldTableViewInfo;
end;

function TcxGridBoldTableView.GetControllerClass: TcxCustomGridControllerClass;
begin
  result := TcxGridBoldTableController;
end;

function TcxGridBoldTableView.GetDataController: TcxGridBoldDataController;
begin
  Result := TcxGridBoldDataController(FDataController);
end;

function TcxGridBoldTableView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridBoldDataController;
end;

function TcxGridBoldTableView.GetFake: TNotifyEvent;
begin
  result := nil;
end;

procedure TcxGridBoldTableView.SetFake(const Value: TNotifyEvent);
begin

end;

function TcxGridBoldTableView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridBoldColumn;
end;

procedure TcxGridBoldTableView.HookDragDrop;
begin
  OnDragDrop := DataController.DoDragDrop;
  OnStartDrag := DataController.DoStartDrag;
  OnEndDrag := DataController.DoEndDrag;
  OnDragOver := DataController.DoDragOver;
end;

procedure TcxGridBoldTableView.SetDataController(
  Value: TcxGridBoldDataController);
begin
  FDataController.Assign(Value);
end;

function TcxGridBoldTableView.ValidateComponent(
  ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;
var
  i: integer;
  lContext: TBoldElementTypeInfo;
  lBoldValidateableComponent: IBoldValidateableComponent;
begin
  lContext := DataController.GetHandleStaticType;
  result := ComponentValidator.ValidateExpressionInContext(
    '', lContext, format('%s%s', [NamePrefix, Name])); // do not localize
  if assigned(lContext) then
    for i := 0 to ItemCount - 1 do
    begin
      result := ComponentValidator.ValidateExpressionInContext(
        Items[i].BoldProperties.Expression,
        lContext,
        format('%s%s.Column[%d]', [NamePrefix, Name, i]),
        Items[i].BoldProperties.VariableList) and result; // do not localize
      if Supports((DataController.GetItem(i) as TcxCustomGridTableItem).GetProperties, IBoldValidateableComponent, lBoldValidateableComponent) then
        result := lBoldValidateableComponent.ValidateComponent(ComponentValidator, namePrefix) and result;
    end;
end;

constructor TcxGridBoldTableView.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF DefaultDragMode}
  DragMode := dmAutomatic;
{$ENDIF}
  hookDragDrop;
end;

{ TcxGridBoldBandedTableView }

constructor TcxGridBoldBandedTableView.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF DefaultDragMode}
  DragMode := dmAutomatic;
{$ENDIF}
  hookDragDrop;
end;

destructor TcxGridBoldBandedTableView.Destroy;
begin
  inherited;
end;

{$IFDEF DelayOnFocusedRecordChange}
procedure TcxGridBoldBandedTableView.InheritedDoFocusedRecordChanged(APrevFocusedRecordIndex,
  AFocusedRecordIndex, APrevFocusedDataRecordIndex,
  AFocusedDataRecordIndex: Integer; ANewItemRecordFocusingChanged: Boolean);
begin
  if AFocusedRecordIndex >= TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).RecordList.Count then
  begin
    DataController.DataControllerInfo.Refresh;
    Assert(DataController.FilteredRecordCount = TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).RecordList.Count, Format('DataController.FilteredRecordCount = %d <> DataController.DataControllerInfo.GetRowCount = %d', [DataController.FilteredRecordCount, TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).RecordList.Count]));
  end;
  inherited DoFocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex, APrevFocusedDataRecordIndex, AFocusedDataRecordIndex, ANewItemRecordFocusingChanged);
end;
{$ENDIF}

procedure TcxGridBoldBandedTableView.DoFocusedRecordChanged(APrevFocusedRecordIndex,
  AFocusedRecordIndex, APrevFocusedDataRecordIndex,
  AFocusedDataRecordIndex: Integer; ANewItemRecordFocusingChanged: Boolean);
begin
  if DataController.InDelayScrollUpdate then
  begin
    if FPrevFocusedRecordIndex = MaxInt then // MaxInt used as flag to only store Prev values once
    begin
      FPrevFocusedRecordIndex := APrevFocusedRecordIndex;
      FPrevFocusedDataRecordIndex := APrevFocusedDataRecordIndex;
    end;
  end
  else
{$IFDEF DelayOnFocusedRecordChange}
  if Assigned(OnFocusedRecordChanged)then
  begin
    DataController.EnsureEventQueued;
    DataController.fFocusChanged := true;
    DataController.fPrevFocusedRecordIndex := APrevFocusedRecordIndex;
    DataController.fFocusedRecordIndex := AFocusedRecordIndex;
    DataController.fPrevFocusedDataRecordIndex := APrevFocusedDataRecordIndex;
    DataController.fFocusedDataRecordIndex := AFocusedDataRecordIndex;
    DataController.fNewItemRecordFocusingChanged := ANewItemRecordFocusingChanged;
  end
  else
{$ENDIF}  
  begin
//    if not TBoldQueueable.IsDisplayQueueEmpty and not TBoldQueueable.IsDisplaying then
//      TBoldQueueable.DisplayAll;
    inherited;
    FPrevFocusedRecordIndex := MaxInt;
  end;
end;

procedure TcxGridBoldBandedTableView.DoChanged(AChangeKind: TcxGridViewChangeKind);
begin
  if Assigned(DataController) and Assigned(DataController.CustomDataSource) and DataController.MainFollowerNeedsDisplay then
    DataController.DisplayFollowers;
  inherited;
end;

function TcxGridBoldBandedTableView.DoCellDblClick(
  ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
  AShift: TShiftState): Boolean;
var
  lAutoForm: TForm;
  lElement: TBoldElement;
begin
  result := false;
  if DataController.BoldProperties.DefaultDblClick and not Controller.IsSpecialRowFocused and Assigned(DataController.Follower.CurrentSubFollower) then
  begin
    lElement := DataController.Follower.CurrentSubFollower.Element;
    lAutoForm := AutoFormProviderRegistry.FormForElement(lElement);
    if assigned(lAutoForm) then
    begin
      result := true;
      lAutoForm.Show;
    end
  end;
  if not result then
  begin
    result := inherited DoCellDblClick(ACellViewInfo, AButton, AShift);
  end;
end;

function TcxGridBoldBandedTableView.DoEditing(
  AItem: TcxCustomGridTableItem): Boolean;
begin
  if Controller.IsSpecialRowFocused then
  begin
    result := inherited DoEditing(aItem);
  end
  else
  begin
    result := DataController.DoEditing(AItem) and inherited DoEditing(aItem);
  end;
end;

procedure TcxGridBoldBandedTableView.DoSelectionChanged;
begin
  DataController.SelectionChanged;
  inherited;
end;

function TcxGridBoldBandedTableView.GetControllerClass: TcxCustomGridControllerClass;
begin
  result := TcxGridBoldBandedTableController;
end;

function TcxGridBoldBandedTableView.GetCurrentBoldObject: TBoldObject;
begin
  result := DataController.CurrentBoldObject;
end;

function TcxGridBoldBandedTableView.GetCurrentElement: TBoldElement;
begin
  result := DataController.CurrentElement;
end;

function TcxGridBoldBandedTableView.GetCurrentIndex: integer;
begin
  result := DataController.CurrentIndex;
end;

function TcxGridBoldBandedTableView.GetDataController: TcxGridBoldDataController;
begin
  Result := TcxGridBoldDataController(FDataController);
end;

function TcxGridBoldBandedTableView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridBoldDataController;
end;

function TcxGridBoldBandedTableView.GetItem(Index: Integer): IBoldAwareViewItem;
begin
  result := inherited Items[Index] as IBoldAwareViewItem;
end;

function TcxGridBoldBandedTableView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridBoldBandedColumn;
end;

function TcxGridBoldBandedTableView.GetItemCount: Integer;
begin
  result := inherited ItemCount;
end;

function TcxGridBoldBandedTableView.GetSelection: TBoldList;
begin
  result := DataController.Selection;
end;

function TcxGridBoldBandedTableView.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  Result := TcxGridBoldBandedTableViewInfo;
end;

procedure TcxGridBoldBandedTableView.HookDragDrop;
begin
  OnDragDrop := DataController.DoDragDrop;
  OnStartDrag := DataController.DoStartDrag;
  OnEndDrag := DataController.DoEndDrag;
  OnDragOver := DataController.DoDragOver;
end;

procedure TcxGridBoldBandedTableView.SetDataController(
  Value: TcxGridBoldDataController);
begin
  FDataController.Assign(Value);
end;

function TcxGridBoldBandedTableView.ValidateComponent(
  ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;
var
  i: integer;
  lContext: TBoldElementTypeInfo;
  lBoldValidateableComponent: IBoldValidateableComponent;
begin
  lContext := DataController.GetHandleStaticType;
  result := ComponentValidator.ValidateExpressionInContext(
    '', lContext, format('%s%s', [NamePrefix, Name])); // do not localize
  if assigned(lContext) then
  begin
    for i := 0 to ItemCount - 1 do
    begin
      result := ComponentValidator.ValidateExpressionInContext(
        Items[i].BoldProperties.Expression,
        lContext,
        format('%s%s.Column[%d]', [NamePrefix, Name, i]),
        Items[i].BoldProperties.VariableList) and result; // do not localize
      if Supports((DataController.GetItem(i) as TcxCustomGridTableItem).GetProperties, IBoldValidateableComponent, lBoldValidateableComponent) then
        result := lBoldValidateableComponent.ValidateComponent(ComponentValidator, namePrefix) and result;
    end;
  end;
end;

destructor TcxGridBoldTableView.Destroy;
begin
  TBoldFollowerControllerAccess(DataController.fBoldColumnsProperties).FreePublisher;
  inherited;
end;

function TcxGridBoldTableView.GetCurrentBoldObject: TBoldObject;
begin
  result := DataController.CurrentBoldObject;
end;

function TcxGridBoldTableView.GetCurrentIndex: integer;
begin
  result := DataController.CurrentIndex;
end;

function TcxGridBoldTableView.GetCurrentElement: TBoldElement;
begin
  result := DataController.CurrentElement;
end;

function TcxGridBoldTableView.GetItem(Index: Integer): IBoldAwareViewItem;
begin
  result := inherited Items[Index] as IBoldAwareViewItem;
end;

function TcxGridBoldTableView.GetItemCount: Integer;
begin
  result := inherited ItemCount;
end;

function TcxGridBoldTableView.GetProperties(AProperties: TStrings): Boolean;
begin
  with AProperties do
  begin
    Add('IncSearch');
  end;
  Result := inherited GetProperties(AProperties);
end;

procedure TcxGridBoldTableView.SetPropertyValue(const AName: string;
  const AValue: Variant);
begin
  if AName = 'IncSearch' then
    OptionsBehavior.IncSearch := AValue
  else
    inherited;
end;

procedure TcxGridBoldTableView.GetPropertyValue(const AName: string;
  var AValue: Variant);
begin
  if AName = 'IncSearch' then
    AValue := OptionsBehavior.IncSearch
  else
    inherited;
end;

{ TcxGridBoldCardView }

constructor TcxGridBoldCardView.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF DefaultDragMode}
// Drag drop is currently not supported in the CardView

//  DragMode := dmAutomatic;
//  hookDragDrop;
{$ENDIF}
end;

destructor TcxGridBoldCardView.Destroy;
begin
  inherited;
end;

function TcxGridBoldCardView.DoCellDblClick(
  ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
  AShift: TShiftState): Boolean;
var
  lAutoForm: TForm;
  lElement: TBoldElement;
begin
  result := false;
  if DataController.BoldProperties.DefaultDblClick and Assigned(DataController.Follower.CurrentSubFollower) then
  begin
    lElement := DataController.Follower.CurrentSubFollower.Element;
    lAutoForm := AutoFormProviderRegistry.FormForElement(lElement);
    if assigned(lAutoForm) then
    begin
      result := true;
      lAutoForm.Show;
    end
  end;
  if not result then
  begin
    result := inherited DoCellDblClick(ACellViewInfo, AButton, AShift);
  end;
end;

function TcxGridBoldCardView.DoEditing(
  AItem: TcxCustomGridTableItem): Boolean;
begin
  result := DataController.DoEditing(AItem) and inherited DoEditing(aItem);
end;

procedure TcxGridBoldCardView.DoSelectionChanged;
begin
  DataController.SelectionChanged;
  inherited;
end;

function TcxGridBoldCardView.GetControllerClass: TcxCustomGridControllerClass;
begin
  result := TcxGridBoldCardViewController;
end;

function TcxGridBoldCardView.GetCurrentBoldObject: TBoldObject;
begin
  result := DataController.CurrentBoldObject;
end;

function TcxGridBoldCardView.GetCurrentElement: TBoldElement;
begin
  result := DataController.CurrentElement;
end;

function TcxGridBoldCardView.GetCurrentIndex: integer;
begin
  result := DataController.CurrentIndex;
end;

function TcxGridBoldCardView.GetDataController: TcxGridBoldDataController;
begin
  Result := TcxGridBoldDataController(FDataController);
end;

function TcxGridBoldCardView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridBoldDataController;
end;

function TcxGridBoldCardView.GetItem(Index: Integer): IBoldAwareViewItem;
begin
  result := inherited Items[Index] as IBoldAwareViewItem;
end;

function TcxGridBoldCardView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridBoldCardViewRow;
end;

function TcxGridBoldCardView.GetItemCount: Integer;
begin
  result := inherited ItemCount;
end;

function TcxGridBoldCardView.GetSelection: TBoldList;
begin
  result := DataController.Selection;
end;

function TcxGridBoldCardView.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  Result := TcxGridBoldCardViewViewInfo;
end;

procedure TcxGridBoldCardView.SetDataController(
  Value: TcxGridBoldDataController);
begin
  FDataController.Assign(Value);
end;

function TcxGridBoldCardView.ValidateComponent(
  ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;
var
  i: integer;
  lContext: TBoldElementTypeInfo;
  lBoldValidateableComponent: IBoldValidateableComponent;
begin
  lContext := DataController.GetHandleStaticType;
  result := ComponentValidator.ValidateExpressionInContext(
    '', lContext, format('%s%s', [NamePrefix, Name])); // do not localize
  if assigned(lContext) then
  begin
    for i := 0 to ItemCount - 1 do
    begin
      result := ComponentValidator.ValidateExpressionInContext(
        Items[i].BoldProperties.Expression,
        lContext,
        format('%s%s.Column[%d]', [NamePrefix, Name, i]),
        Items[i].BoldProperties.VariableList) and result; // do not localize
      if Supports((DataController.GetItem(i) as TcxCustomGridTableItem).GetProperties, IBoldValidateableComponent, lBoldValidateableComponent) then
        result := lBoldValidateableComponent.ValidateComponent(ComponentValidator, namePrefix) and result;
    end;
  end;
end;

{ TcxBoldCustomDataProvider }

function TcxBoldCustomDataProvider.GetValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant;
var
  ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle;
  lBoldDataSource: TcxBoldDataSource;
begin
  Result := Null;
  lBoldDataSource := CustomDataSource as TcxBoldDataSource;
  if Assigned(lBoldDataSource) then
  begin
    lBoldDataSource.CurrentProvider := Self;
    ARecordHandle := TcxDataRecordHandle(ARecordIndex);
    AItemHandle := lBoldDataSource.GetItemHandle(AField.Index);
    Result := lBoldDataSource.GetValue(ARecordHandle, AItemHandle);
  end;
end;

function TcxBoldCustomDataProvider.IsActiveDataSet: Boolean;
begin
  result := Assigned(CustomDataSource);
end;

function TcxBoldCustomDataProvider.SetEditValue(ARecordIndex: Integer;
  AField: TcxCustomDataField; const AValue: Variant;
  AEditValueSource: TcxDataEditValueSource): Boolean;
begin
  DataController.SetValue(ARecordIndex, AField.Index, AValue);
  SetModified;
  Result := True;
end;

procedure TcxBoldCustomDataProvider.SetValue(ARecordIndex: Integer; AField: TcxCustomDataField; const Value: Variant);
var
  ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle;
  lBoldDataSource: TcxBoldDataSource;
begin
  lBoldDataSource := CustomDataSource as TcxBoldDataSource;
  if Assigned(lBoldDataSource) then
  begin
    lBoldDataSource.CurrentProvider := Self;
    ARecordHandle := TcxDataRecordHandle(ARecordIndex);
    AItemHandle := lBoldDataSource.GetItemHandle(AField.Index);
    lBoldDataSource.SetValue(ARecordHandle, AItemHandle, Value);
  end;
end;

function TcxBoldCustomDataProvider.CanDelete: Boolean;
var
  lcxBoldDataController: TcxBoldDataController;
begin
  lcxBoldDataController := DataController as TcxBoldDataController;
  result := Assigned(lcxBoldDataController) and Assigned(lcxBoldDataController.BoldHandle);
  if result then
  begin
    if Assigned(lcxBoldDataController.fOnDelete) and Assigned(lcxBoldDataController.fCanDelete) then
    begin
      lcxBoldDataController.fCanDelete(lcxBoldDataController, result)
    end
    else
      result := Assigned(lcxBoldDataController.BoldHandle.MutableList);
{
      for I := 0 to lcxBoldDataController.Selection.Count - 1 do
        if not lcxBoldDataController.BoldHandle.MutableList.CanRemove( lcxBoldDataController.Selection[i] ) then
        begin
          result := false;
          exit;
        end;
}
  end;
end;

function TcxBoldCustomDataProvider.CanInsert: Boolean;
var
  lcxBoldDataController: TcxBoldDataController;
begin
  lcxBoldDataController := DataController as TcxBoldDataController;
  result := Assigned(lcxBoldDataController) and Assigned(lcxBoldDataController.BoldHandle);
  if result then
  begin
    if Assigned(lcxBoldDataController.fOnInsert) and Assigned(lcxBoldDataController.fCanInsert) then
    begin
      lcxBoldDataController.fCanInsert(lcxBoldDataController, result)
    end
    else
      if Assigned(lcxBoldDataController.fCanInsert) then
        lcxBoldDataController.fCanInsert(lcxBoldDataController, result);
      result := result and Assigned(lcxBoldDataController.BoldHandle.MutableList) and lcxBoldDataController.BoldHandle.MutableList.CanCreateNew;
  end;
end;

type TcxCustomDataSourceAccess = class(TcxCustomDataSource);

procedure TcxBoldCustomDataProvider.DeleteRecords(AList: TList);
var
  i, {j,} ARecordIndex: Integer;
  lListToDelete: TBoldObjectList;
//  lOriginalList: TBoldObjectList;
  lMutableList: TBoldObjectList;
  lObjectToDelete: TBoldObject;
  lFollower: TBoldFollower;
begin
  DataController.BeginFullUpdate;
  lListToDelete := TBoldObjectList.Create;
  inc(TcxBoldDataController(DataController).fSkipMakeCellUptoDate);
  try
//    lOriginalList := TcxBoldDataController(DataController).Follower.Element as TBoldObjectList;
    for I := AList.Count - 1 downto 0 do
    begin
      ARecordIndex := Integer(AList[I]);
      with DataController.DataControllerInfo.Selection do
        if (DataController.DataControllerInfo.Selection.Count > 1) and IsRecordSelected(ARecordIndex) then
          DataController.DataControllerInfo.Selection.Delete(FindByRecordIndex(ARecordIndex));

      lFollower := TcxBoldDataController(DataController).Follower.Subfollowers[ARecordIndex];
      lObjectToDelete := lFollower.element as TBoldObject;
      if Assigned(lObjectToDelete) and not lObjectToDelete.BoldObjectIsDeleted then
        lListToDelete.Add(lObjectToDelete);
    end;
    lMutableList := TcxBoldDataController(DataController).BoldHandle.MutableObjectList;
    if Assigned(lMutableList.BoldRoleRTInfo) and (lMutableList.BoldRoleRTInfo.RoleType = rtLinkRole) then
    begin
      for I := lListToDelete.Count - 1 downto 0 do
      begin
        if lListToDelete[i].BoldObjectExists then // not already deleted
          lListToDelete[i].Delete;
      end;
    end
    else
      lMutableList.RemoveList(lListToDelete);
    if TcxCustomDataControllerAccess(DataController).FInDeleteSelection then
      DataController.ClearSelection;
    TcxGridBoldDataController(DataController).DataHasChanged := true;
  finally
    lListToDelete.free;
    DataController.EndFullUpdate;
    TcxCustomDataControllerAccess(DataController).CheckNearestFocusRow;
    dec(TcxBoldDataController(DataController).fSkipMakeCellUptoDate);
  end;
end;

{ TcxGridBoldTableController }

procedure TcxGridBoldTableController.DoKeyDown(var Key: Word;
  Shift: TShiftState);
var
  lColumnAutoWidth: boolean;
  lVisibleCount: integer;
begin
  if (Key = VK_DOWN) or (Key = VK_UP) or (Key = VK_NEXT) or (Key = VK_PRIOR) then
    (DataController as TcxBoldDataController).BeginDelayScrollUpdate;
  if (key = VK_ADD) and (shift = [ssCtrl]) then
  begin
    GridView.BeginUpdate;
    try
      GridView.OptionsView.ColumnAutoWidth := not GridView.OptionsView.ColumnAutoWidth;
      lColumnAutoWidth := GridView.OptionsView.ColumnAutoWidth;
      if not lColumnAutoWidth then
      begin
        lVisibleCount := GridView.ViewInfo.VisibleRecordCount;
        if lVisibleCount <> GridView.OptionsBehavior.BestFitMaxRecordCount then
          GridView.OptionsBehavior.BestFitMaxRecordCount := lVisibleCount;
        ViewInfo.GridView.ApplyBestFit(nil, true, true);
        if lColumnAutoWidth then
          GridView.OptionsView.ColumnAutoWidth := true;
      end;
    finally
      GridView.EndUpdate;
    end;
  end;
  inherited;
end;
{
procedure TcxGridBoldTableController.FocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex,
  APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
  ANewItemRecordFocusingChanged: Boolean);
begin
//  CodeSite.Send('FocusedRecordChanged:' + IntToStr(AFocusedRecordIndex));
  inherited;
end;
}
function TcxGridBoldTableController.GetEditingControllerClass: TcxGridEditingControllerClass;
begin
  result := TcxGridBoldEditingController;
end;

procedure TcxGridBoldTableController.KeyDown(var Key: Word;
  Shift: TShiftState);
var
  lIndex: integer;
  lBoldHandle: TBoldAbstractListHandle;
  lBoldList: TBoldList;
  lHandled: boolean;
  lAllowed: boolean;
  lcxBoldDataController: TcxBoldDataController;
begin
  lcxBoldDataController := (DataController as TcxBoldDataController);
  if DataController.FilteredRecordCount <> TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).RecordList.Count then
  begin
    DataController.DataControllerInfo.Refresh;
    Assert(DataController.FilteredRecordCount = TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).RecordList.Count, Format('DataController.RecordCount = %d <> DataController.DataControllerInfo.GetRowCount = %d', [DataController.RecordCount, TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).RecordList.Count]));
  end;
  if not BlockRecordKeyboardHandling and (FocusedRecord <> nil) then
    TcxCustomGridRecordAccess(FocusedRecord).KeyDown(Key, Shift);
  case Key of
    VK_INSERT:
      if (Shift = []) then
      begin
        Key := 0;
        lHandled := false;
        if Assigned(lcxBoldDataController.fOnInsert) then
        begin
          lAllowed := CanInsert(true);
          if lAllowed then
            lcxBoldDataController.fOnInsert(lcxBoldDataController);
          lHandled := true;
        end
        else
          lAllowed := Assigned(lcxBoldDataController.BoldHandle) and Assigned(lcxBoldDataController.BoldHandle.MutableList);
        if not lHandled and lAllowed then
        begin
          lBoldHandle := TcxBoldDataController(DataController).BoldHandle;
          lBoldList := lBoldHandle.MutableList;
          if Assigned(lBoldList.BoldMemberRTInfo) and TBoldRoleRTInfo(lBoldList.BoldMemberRTInfo).IsOrdered and ((DataController as TcxBoldDataController).Follower.CurrentIndex <> -1) then
          begin
            lIndex := TcxBoldDataController(DataController).Follower.CurrentIndex;
            lBoldList.InsertNew(lIndex);
          end
          else
          begin
            lIndex := lBoldList.IndexOf(lBoldList.AddNew);
          end;
          TBoldQueueable.DisplayAll;
          (DataController as TcxBoldDataController).Follower.CurrentIndex := lIndex;
        end;
      end
      else
        if (Shift = [ssCtrl]) and not IsEditing then
          GridView.CopyToClipboard(False);
    VK_DELETE:
      if ((Shift = []) or (Shift = [ssCtrl])) and (SelectedRecordCount > 0) then
      begin
        Key := 0;
        lHandled := false;
        lAllowed := CanDelete(true);
        if Assigned(lcxBoldDataController.fOnDelete)  then
        begin
          if lAllowed then
            lcxBoldDataController.fOnDelete(lcxBoldDataController);
          lHandled := true;
        end;
        if not lHandled and lAllowed then
        begin
          DeleteSelection;
        end;
        TBoldQueueable.DisplayAll;
      end;
    VK_HOME:
      begin
        if ([ssCtrl] = Shift) {or not FocusedRecordHasCells(True)} then
        begin
          GoToFirst(True)
        end
        else
          inherited; //FocusNextItem(-1, True, False, False, true);
      end;
    VK_END:
      begin
        if ([ssCtrl] = Shift) {or not FocusedRecordHasCells(True)} then
        begin
          GoToLast(False, True)
        end
        else
          inherited; // FocusNextItem(-1, False, True, False, true)
      end;
  else
    inherited
  end;
end;

procedure TcxGridBoldTableController.KeyUp(var Key: Word; Shift: TShiftState);
{$IFDEF CenterResultOnIncSearch}
var
  AVisibleRecordCount: Integer;
{$ENDIF}
begin
  inherited;
  with TcxBoldDataController(DataController) do
    EndDelayScrollUpdate;
{$IFDEF CenterResultOnIncSearch}
  if (FocusedRecord <> nil) and IsIncSearching then
  begin
    AVisibleRecordCount := ViewInfo.VisibleRecordCount;
    GridView.Controller.TopRowIndex := GridView.Controller.FocusedRecordIndex - (AVisibleRecordCount div 2);
  end;
{$ENDIF}
end;

procedure TcxBoldGridSite.WndProc(var Message: TMessage);
var
  vDataController: TcxBoldDataController;
begin
  try
    if Controller is TcxGridTableController then
    begin
      vDataController := TcxGridTableControllerAccess(Controller).DataController as TcxBoldDataController;
      if Assigned(vDataController) and (vDataController.LockCount = 0) then
      begin
        Case Message.Msg of WM_PAINT, WM_SETFOCUS, WM_KILLFOCUS, WM_WINDOWPOSCHANGING, WM_MOVE, WM_MOUSEMOVE:
          if Assigned(vDataController.CustomDataSource) and vDataController.MainFollowerNeedsDisplay then
            vDataController.DisplayFollowers;
        end;
      end;
    end;
  finally
    inherited;
  end;
end;

{ TcxGridBoldBandedTableController }

procedure TcxGridBoldBandedTableController.DoKeyDown(var Key: Word;
  Shift: TShiftState);
var
  lColumnAutoWidth: boolean;
  lVisibleCount: integer;
begin
  if (Key = VK_DOWN) or (Key = VK_UP) or (Key = VK_NEXT) or (Key = VK_PRIOR) then
    (DataController as TcxBoldDataController).BeginDelayScrollUpdate;
  if (key = VK_ADD) and (shift = [ssCtrl]) then
  begin
    GridView.BeginUpdate;
    try
      GridView.OptionsView.ColumnAutoWidth := not GridView.OptionsView.ColumnAutoWidth;
      lColumnAutoWidth := GridView.OptionsView.ColumnAutoWidth;
      if not lColumnAutoWidth then
      begin
        lVisibleCount := GridView.ViewInfo.VisibleRecordCount;
        if lVisibleCount <> GridView.OptionsBehavior.BestFitMaxRecordCount then
          GridView.OptionsBehavior.BestFitMaxRecordCount := lVisibleCount;
        ViewInfo.GridView.ApplyBestFit(nil, true, true);
        if lColumnAutoWidth then
          GridView.OptionsView.ColumnAutoWidth := true;
      end;
    finally
      GridView.EndUpdate;
    end;
  end;
  inherited;
end;

function TcxGridBoldBandedTableController.GetEditingControllerClass: TcxGridEditingControllerClass;
begin
  result := TcxGridBoldEditingController;
end;

procedure TcxGridBoldBandedTableController.KeyDown(var Key: Word;
  Shift: TShiftState);
var
  lIndex: integer;
  lBoldHandle: TBoldAbstractListHandle;
  lBoldList: TBoldList;
  lHandled: boolean;
  lAllowed: boolean;
  lcxBoldDataController: TcxBoldDataController;
begin
  lcxBoldDataController := (DataController as TcxBoldDataController);
  if DataController.FilteredRecordCount <> TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).RecordList.Count then
  begin
    DataController.DataControllerInfo.Refresh;
    Assert(DataController.FilteredRecordCount = TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).RecordList.Count, Format('DataController.FilteredRecordCount = %d <> DataController.DataControllerInfo.GetRowCount = %d', [DataController.FilteredRecordCount, TcxCustomDataControllerInfoAccess(DataController.DataControllerInfo).RecordList.Count]));
  end;
  if not BlockRecordKeyboardHandling and (FocusedRecord <> nil) then
    TcxCustomGridRecordAccess(FocusedRecord).KeyDown(Key, Shift);
  case Key of
    VK_INSERT:
      if (Shift = []) then
      begin
        Key := 0;
        lHandled := false;
        if Assigned(lcxBoldDataController.fOnInsert) then
        begin
          lAllowed := CanInsert(true);
          if lAllowed then
            lcxBoldDataController.fOnInsert(lcxBoldDataController);
          lHandled := true;
        end
        else
          lAllowed := Assigned(lcxBoldDataController.BoldHandle) and Assigned(lcxBoldDataController.BoldHandle.MutableList);
        if not lHandled and lAllowed then
        begin
          lBoldHandle := TcxBoldDataController(DataController).BoldHandle;
          lBoldList := lBoldHandle.MutableList;
          if Assigned(lBoldList.BoldMemberRTInfo) and TBoldRoleRTInfo(lBoldList.BoldMemberRTInfo).IsOrdered and ((DataController as TcxBoldDataController).Follower.CurrentIndex <> -1) then
          begin
            lIndex := TcxBoldDataController(DataController).Follower.CurrentIndex;
            lBoldList.InsertNew(lIndex);
          end
          else
          begin
            lIndex := lBoldList.IndexOf(lBoldList.AddNew);
          end;
          TBoldQueueable.DisplayAll;
          (DataController as TcxBoldDataController).Follower.CurrentIndex := lIndex;
        end;
      end
      else
        if (Shift = [ssCtrl]) and not IsEditing then
          GridView.CopyToClipboard(False);
    VK_DELETE:
      if ((Shift = []) or (Shift = [ssCtrl])) and (SelectedRecordCount > 0) then
      begin
        Key := 0;
        lHandled := false;
        lAllowed := true;
        if Assigned(lcxBoldDataController.fOnDelete)  then
        begin
          lAllowed := CanDelete(true);
          if lAllowed then
            lcxBoldDataController.fOnDelete(lcxBoldDataController);
          lHandled := true;
        end;
        if not lHandled and lAllowed then
        begin
          DeleteSelection;
        end;
        TBoldQueueable.DisplayAll;
      end;
    VK_HOME:
      begin
        if ([ssCtrl] = Shift) {or not FocusedRecordHasCells(True)} then
        begin
          GoToFirst(True)
        end
        else
          inherited; //FocusNextItem(-1, True, False, False, true);
      end;
    VK_END:
      begin
        if ([ssCtrl] = Shift) {or not FocusedRecordHasCells(True)} then
        begin
          GoToLast(False, True)
        end
        else
          inherited; // FocusNextItem(-1, False, True, False, true)
      end;
  else
    inherited
  end;
end;

procedure TcxGridBoldBandedTableController.KeyUp(var Key: Word;
  Shift: TShiftState);
{$IFDEF CenterResultOnIncSearch}
var
  AVisibleRecordCount: Integer;
{$ENDIF}
begin
  inherited;
  with TcxBoldDataController(DataController) do
    EndDelayScrollUpdate;
{$IFDEF CenterResultOnIncSearch}
  if (FocusedRecord <> nil) and IsIncSearching then
  begin
    AVisibleRecordCount := ViewInfo.VisibleRecordCount;
    GridView.Controller.TopRowIndex := GridView.Controller.FocusedRecordIndex - (AVisibleRecordCount div 2);
  end;
{$ENDIF}
end;

{ TcxBoldDataControllerSearch }

function TcxBoldDataControllerSearch.Locate(AItemIndex: Integer; const ASubText: string; AIsAnywhere: Boolean = False {$IFDEF BOLD_DELPHI25_OR_LATER}; ASyncSelection: Boolean = True{$ENDIF}): Boolean;
begin
  with DataController as TcxBoldDataController do
    AdjustActiveRange(BoldList, AItemIndex);
  result := inherited Locate(AItemIndex, ASubText {$IFDEF BOLD_DELPHI25_OR_LATER}, AIsAnywhere, ASyncSelection{$ENDIF});
end;

function TcxBoldDataControllerSearch.LocateNext(AForward: Boolean; AIsAnywhere: Boolean = False {$IFDEF BOLD_DELPHI25_OR_LATER}; ASyncSelection: Boolean = True{$ENDIF}): Boolean;
begin
  (DataController as TcxBoldDataController).AdjustActiveRange();
  result := inherited LocateNext(AForward {$IFDEF BOLD_DELPHI25_OR_LATER}, AIsAnywhere, ASyncSelection{$ENDIF});
end;

{ TcxGridBoldCardViewRow }

function TcxGridBoldCardViewRow.CalculateBestFitWidth: Integer;
begin
  GridView.OptionsBehavior.BestFitMaxRecordCount := GridView.ViewInfo.VisibleRecordCount;
  result := inherited CalculateBestFitWidth;
end;

destructor TcxGridBoldCardViewRow.Destroy;
begin
  DataBinding.Remove;
  inherited;
end;

function TcxGridBoldCardViewRow.GetDataBinding: TcxGridItemBoldDataBinding;
begin
  Result := TcxGridItemBoldDataBinding(inherited DataBinding);
end;

procedure TcxGridBoldCardViewRow.SetDataBinding(
  Value: TcxGridItemBoldDataBinding);
begin
  inherited DataBinding := Value;
end;

procedure TcxGridBoldCardViewRow.VisibleChanged;
begin
  inherited;
//  if Visible and not IsLoading then
//    (DataController as TcxGridBoldDataController).AdjustActiveRange();
end;

{ TcxGridBoldEditingController }

procedure TcxGridBoldEditingController.DoEditKeyDown(var Key: Word;
  Shift: TShiftState);
var
  lWasEditing: boolean;
//  lController: TcxGridTableController;
  lHideFilterRowOnEnter: boolean;
begin
//  lController := nil;
  lHideFilterRowOnEnter := false;
//  if Controller is TcxGridTableController then
  begin
//    lController := Controller as TcxGridTableController;
    lWasEditing := (EditingItem <> nil) and EditingItem.Editing;
    if lWasEditing and (Key = VK_ESCAPE) then
    begin
      Key := VK_ESCAPE;
    end
    else
    if Controller.IsFilterRowFocused then
    begin
      if (Key = VK_RETURN) and lWasEditing then
      begin
        lHideFilterRowOnEnter := true;
      end;
    end;
  end;
  inherited;
  if lHideFilterRowOnEnter and (EditingItem = nil) and (GridView.DataController.FilteredRecordCount > 0) then
    Controller.GridView.FilterRow.Visible := false
end;

procedure TcxGridBoldEditingController.EditChanged(Sender: TObject);
var
  lEdit: TcxCustomEdit;
  lFollower: TBoldFollower;
  lDataController: TcxGridBoldDataController;
  lIcxBoldEditProperties: IcxBoldEditProperties;
  lDone: Boolean;
begin
//  inherited; // moved to the end of the method, coz it fires OnChange event and we don't want that to happen before we make the change
{
  Here we basically ignore ApplyPolicy. We want to mark follower dirty as soon as possible (ie here)
  But we don't want to apply changes to ObjectSpace yet as changes may cause reload of data,
  especially if the view is grouped/sorted and user is editing a grouped/sorted column.
  So if this happens editing loses focus and view reloads data, so we want to avoid this.
}
  if (EditingItem <> nil) {EditingItem is TcxGridBoldColumn} and not Controller.IsSpecialRowFocused then
  begin
    lEdit := Sender as TcxCustomEdit;
    lDataController := EditingItem.DataBinding.DataController as TcxGridBoldDataController;
    lFollower := lDataController.Follower.SubFollowers[lDataController.FocusedRecordIndex];
    if lFollower.Active then
    begin
      lFollower := lFollower.SubFollowers[Integer(EditingItem.DataBinding.Data)];
      lDone := false;
      Assert(EditingItem.GetProperties <> nil);
      if Supports(EditingItem.GetProperties, IcxBoldEditProperties, lIcxBoldEditProperties) then
      begin
        lIcxBoldEditProperties.SetStoredValue(Null, lDataController.BoldHandle, lEdit, lFollower, lDone);
      end;
      if not lDone then
      begin
        lDone := (lFollower.Controller as TBoldVariantFollowerController).MayHaveChanged(Edit.EditingValue, lFollower);
      end;
      if lDone then
      begin
        lDataController.fInternalChange := true;
        try
          TBoldQueueable.DisplayAll;
        finally
          lDataController.fInternalChange := false;
          inherited;
        end;
      end;
    end;
  end
  else
    inherited;
end;

procedure TcxGridBoldEditingController.EditExit(Sender: TObject);
begin
//  self.EditChanged(Sender);
  inherited;
end;

procedure TcxGridBoldEditingController.HideEdit(Accept: Boolean);
var
  lcxBoldDataController: TcxBoldDataController;
  lFollower: TBoldFollower;
begin
  if not Accept and Assigned(Edit) and Edit.ModifiedAfterEnter then
  begin
    lcxBoldDataController := EditingItem.DataBinding.DataController as TcxBoldDataController;
    if (lcxBoldDataController.FocusedRecordIndex <> -1) then
    begin
      lFollower := lcxBoldDataController.Follower.SubFollowers[lcxBoldDataController.FocusedRecordIndex];
      lFollower := lFollower.SubFollowers[Integer(EditingItem.DataBinding.Data)];
      begin
        lFollower.DiscardChange;
      end;
    end;
  end;
//  if Assigned(Edit) and Edit.ModifiedAfterEnter then
//    self.EditChanged(Edit);
  inherited;
end;

{ TcxGridBoldBandedColumn }

function TcxGridBoldBandedColumn.CalculateBestFitWidth: Integer;
begin
  GridView.OptionsBehavior.BestFitMaxRecordCount := GridView.ViewInfo.VisibleRecordCount;
  result := inherited CalculateBestFitWidth;
end;

destructor TcxGridBoldBandedColumn.Destroy;
begin
  DataBinding.Remove;
  inherited;
end;

function TcxGridBoldBandedColumn.GetDataBinding: TcxGridItemBoldDataBinding;
begin
  Result := TcxGridItemBoldDataBinding(inherited DataBinding);
end;

procedure TcxGridBoldBandedColumn.SetDataBinding(
  Value: TcxGridItemBoldDataBinding);
begin
  inherited DataBinding := Value;
end;

procedure TcxGridBoldBandedColumn.VisibleChanged;
begin
  inherited;
//  if Visible and not IsLoading then
//    (DataController as TcxGridBoldDataController).AdjustActiveRange();
end;

{ TcxGridBoldChartDataController }
(*
procedure TcxGridBoldChartDataController.AssignData(
  ADataController: TcxCustomDataController);
begin
end;

procedure TcxGridBoldChartDataController.CreateAllItems(
  AMissingItemsOnly: Boolean);
begin
end;

procedure TcxGridBoldChartDataController.DeleteAllItems;
begin
end;

function TcxGridBoldChartDataController.GetChartItem(
  AItemClass: TcxGridChartItemClass; AIndex: Integer): TcxGridChartItem;
var
  AFields: TList;
begin
  AFields := TList.Create;
  try
    GetValidValueFields(AItemClass, AFields);
//    Result := GridView.FindItemByFieldName(AItemClass, TField(AFields[AIndex]).FieldName);
  finally
    AFields.Free;
  end;
end;

procedure TcxGridBoldChartDataController.GetFakeComponentLinks(AList: TList);
begin
  if (BoldHandle <> nil) and (BoldHandle.Owner <> GridView.Component) and
    (AList.IndexOf(BoldHandle.Owner) = -1) then
    AList.Add(BoldHandle.Owner);
end;

procedure TcxGridBoldChartDataController.GetItemCaptions(
  AItemClass: TcxGridChartItemClass; ACaptions: TStringList);
var
  AFields: TList;
  I: Integer;
begin
  AFields := TList.Create;
  try
    GetValidValueFields(AItemClass, AFields);
//    for I := 0 to AFields.Count - 1 do
//      ACaptions.Add(TField(AFields[I]).DisplayName);
  finally
    AFields.Free;
  end;
end;

procedure TcxGridBoldChartDataController.GetValidValueFields(
  AItemClass: TcxGridChartItemClass; AFields: TList);
var
  I: Integer;
//  AField: TField;
begin
{
  if DataSet = nil then Exit;
  for I := 0 to DataSet.FieldCount - 1 do
  begin
    AField := DataSet.Fields[I];
    if not AItemClass.IsValue or
      IsValueTypeClassValid(GetValueTypeClassByField(AField)) then
      AFields.Add(AField);
  end;
  AFields.Sort(CompareFields);
}
end;

function TcxGridBoldChartDataController.HasAllItems: Boolean;
begin
  Result := True;
end;

procedure TcxGridBoldChartDataController.InitItem(AItem: TcxGridChartItem;
  AIndex: Integer);
var
  AFields: TList;
begin
  AFields := TList.Create;
  try
    GetValidValueFields(TcxGridChartItemClass(AItem.ClassType), AFields);
//    TcxGridBoldChartItemDataBinding(AItem.DataBinding).FieldName := TField(AFields[AIndex]).FieldName;
  finally
    AFields.Free;
  end;
end;

function TcxGridBoldChartDataController.IsDataChangeable: Boolean;
begin
  Result := False;
end;

function TcxGridBoldChartDataController.SupportsCreateAllItems: Boolean;
begin
  Result := False;
end;

{ TcxGridBoldChartItemDataBinding }

procedure TcxGridBoldChartItemDataBinding.Assign(Source: TPersistent);
begin
//  if Source is TcxGridBoldChartItemDataBinding then
//    FieldName := TcxGridBoldChartItemDataBinding(Source).FieldName;
  inherited;
end;

constructor TcxGridBoldChartItemDataBinding.Create(AGridView: TcxGridChartView;
  AIsValue: Boolean; ADefaultValueTypeClass: TcxValueTypeClass);
begin
  inherited Create(AGridView, AIsValue, ADefaultValueTypeClass);
  fBoldProperties := TBoldVariantFollowerController.Create(AGridView.Component);

//  DataController.fBoldColumnsProperties.Add(fBoldProperties);
//  fBoldProperties.OnGetContextType := DataController.GetHandleStaticType;
//  FBoldProperties.AfterMakeUptoDate := DataController._AfterMakeCellUptoDate;
end;

destructor TcxGridBoldChartItemDataBinding.Destroy;
begin
  FreeAndNil(FBoldProperties);
  inherited;
end;

function TcxGridBoldChartItemDataBinding.GetDataController: TcxGridBoldChartDataController;
begin
  Result := TcxGridBoldChartDataController(inherited DataController);
end;

procedure TcxGridBoldChartItemDataBinding.SetBoldProperties(
  Value: TBoldVariantFollowerController);
begin
  if Assigned(Value) then
    fBoldProperties.Assign(Value);
end;

{ TcxGridBoldChartCategories }

function TcxGridBoldChartCategories.GetDataBinding: TcxGridBoldChartItemDataBinding;
begin
  Result := TcxGridBoldChartItemDataBinding(inherited DataBinding);
end;

procedure TcxGridBoldChartCategories.SetDataBinding(
  Value: TcxGridBoldChartItemDataBinding);
begin
  inherited DataBinding := Value;
end;

{ TcxGridBoldChartDataGroup }

function TcxGridBoldChartDataGroup.GetDataBinding: TcxGridBoldChartItemDataBinding;
begin
  Result := TcxGridBoldChartItemDataBinding(inherited DataBinding);
end;

procedure TcxGridBoldChartDataGroup.SetDataBinding(
  Value: TcxGridBoldChartItemDataBinding);
begin
  inherited DataBinding := Value;
end;

{ TcxGridBoldChartSeries }

function TcxGridBoldChartSeries.GetDataBinding: TcxGridBoldChartItemDataBinding;
begin
  Result := TcxGridBoldChartItemDataBinding(inherited DataBinding);
end;

procedure TcxGridBoldChartSeries.SetDataBinding(
  Value: TcxGridBoldChartItemDataBinding);
begin
  inherited DataBinding := Value;
end;

{ TcxBoldGridChartView }

procedure TcxBoldGridChartView.ClearItems;
begin
  ClearSeries;
end;

constructor TcxBoldGridChartView.Create(AOwner: TComponent);
begin
  inherited;
end;

function TcxBoldGridChartView.CreateDataGroup: TcxGridBoldChartDataGroup;
begin
  Result := TcxGridBoldChartDataGroup(inherited CreateDataGroup);
end;

function TcxBoldGridChartView.CreateSeries: TcxGridBoldChartSeries;
begin
  Result := TcxGridBoldChartSeries(inherited CreateSeries);
end;

destructor TcxBoldGridChartView.Destroy;
begin
  inherited;
end;

function TcxBoldGridChartView.FindDataGroupByFieldName(
  const AFieldName: string): TcxGridBoldChartDataGroup;
begin
  Result := TcxGridBoldChartDataGroup(FindItemByFieldName(GetDataGroupClass, AFieldName));
end;

function TcxBoldGridChartView.FindItemByFieldName(
  AItemClass: TcxGridChartItemClass;
  const AFieldName: string): TcxGridChartItem;
var
  AItems: TList;
  I: Integer;
begin
  AItems := GetItemList(AItemClass);
  for I := 0 to AItems.Count - 1 do
  begin
    Result := TcxGridChartItem(AItems[I]);
//    if SameText(TcxGridBoldChartItemDataBinding(Result.DataBinding).FieldName, AFieldName) then Exit;
  end;
  Result := nil;
end;

function TcxBoldGridChartView.FindSeriesByFieldName(
  const AFieldName: string): TcxGridBoldChartSeries;
begin
  Result := TcxGridBoldChartSeries(FindItemByFieldName(GetSeriesClass, AFieldName));
end;

function TcxBoldGridChartView.GetCategories: TcxGridBoldChartCategories;
begin
  Result := TcxGridBoldChartCategories(inherited Categories);
end;

function TcxBoldGridChartView.GetCategoriesClass: TcxGridChartCategoriesClass;
begin
  Result := TcxGridBoldChartCategories;
end;

function TcxBoldGridChartView.GetDataController: TcxGridBoldChartDataController;
begin
  Result := TcxGridBoldChartDataController(inherited DataController);
end;

function TcxBoldGridChartView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridBoldChartDataController;
end;

function TcxBoldGridChartView.GetDataGroup(
  Index: Integer): TcxGridBoldChartDataGroup;
begin
  Result := TcxGridBoldChartDataGroup(inherited DataGroups[Index]);
end;

function TcxBoldGridChartView.GetDataGroupClass: TcxGridChartDataGroupClass;
begin
  Result := TcxGridBoldChartDataGroup;
end;

function TcxBoldGridChartView.GetItem(Index: Integer): IBoldAwareViewItem;
begin
  result := inherited Items[Index] as IBoldAwareViewItem;
end;

function TcxBoldGridChartView.GetItemCount: Integer;
begin
  result := inherited SeriesCount;
end;

function TcxBoldGridChartView.GetItemDataBindingClass: TcxGridChartItemDataBindingClass;
begin
  Result := TcxGridBoldChartItemDataBinding;
end;

function TcxBoldGridChartView.GetSelection: TBoldList;
begin
  result := fSelection;
end;

function TcxBoldGridChartView.GetSeries(Index: Integer): TcxGridBoldChartSeries;
begin
  Result := TcxGridBoldChartSeries(inherited Series[Index]);
end;

function TcxBoldGridChartView.GetSeriesClass: TcxGridChartSeriesClass;
begin
  Result := TcxGridBoldChartSeries;
end;

procedure TcxBoldGridChartView.SetCategories(Value: TcxGridBoldChartCategories);
begin
  inherited Categories := Value;
end;

procedure TcxBoldGridChartView.SetDataController(
  Value: TcxGridBoldChartDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxBoldGridChartView.SetDataGroup(Index: Integer;
  Value: TcxGridBoldChartDataGroup);
begin
  inherited DataGroups[Index] := Value;
end;

procedure TcxBoldGridChartView.SetSeries(Index: Integer;
  Value: TcxGridBoldChartSeries);
begin
  inherited Series[Index] := Value;
end;

function TcxBoldGridChartView.ValidateComponent(
  ComponentValidator: TBoldComponentValidator; NamePrefix: String): Boolean;
var
  i: integer;
  lContext: TBoldElementTypeInfo;
begin
  lContext := DataController.GetHandleStaticType;
  result := ComponentValidator.ValidateExpressionInContext(
      '', lContext, format('%s%s', [NamePrefix, Name])); // do not localize
  if assigned(lContext) then
    for i := 0 to ItemCount - 1 do
      result := ComponentValidator.ValidateExpressionInContext(
        Items[i].DataBinding.BoldProperties.Expression,
        lContext,
        format('%s%s.Column[%d]', [NamePrefix, Name, i])) and result; // do not localize
end;
*)

{ TcxGridBoldCardViewController }

function TcxGridBoldCardViewController.GetEditingControllerClass: TcxGridEditingControllerClass;
begin
  result := TcxGridBoldCardEditingController; //TcxGridBoldEditingController;
end;

{ TcxGridBoldCardEditingController }

procedure TcxGridBoldCardEditingController.EditChanged(Sender: TObject);
var
  lEdit: TcxCustomEdit;
  lFollower: TBoldFollower;
  lDataController: TcxGridBoldDataController;
  lIcxBoldEditProperties: IcxBoldEditProperties;
  lDone: Boolean;
begin
//  inherited; // moved to the end of the method, coz it fires OnChange event and we don't want that to happen before we make the change
{
  Here we basically ignore ApplyPolicy. We want to mark follower dirty as soon as possible (ie here)
  But we don't want to apply changes to ObjectSpace yet as changes may cause reload of data,
  especially if the view is grouped/sorted and user is editing a grouped/sorted column.
  So if this happens editing loses focus and view reloads data, so we want to avoid this.
}
  if EditingItem is TcxGridBoldCardViewRow then
  begin
    lEdit := Sender as TcxCustomEdit;
    lDataController := EditingItem.DataBinding.DataController as TcxGridBoldDataController;
    lFollower := lDataController.Follower.SubFollowers[lDataController.FocusedRecordIndex];
    if lFollower.Active then
    begin
      lFollower := lFollower.SubFollowers[Integer(EditingItem.DataBinding.Data)];
      lDone := false;
      Assert(EditingItem.GetProperties <> nil);
      if Supports(EditingItem.GetProperties, IcxBoldEditProperties, lIcxBoldEditProperties) then
      begin
        lIcxBoldEditProperties.SetStoredValue(Null, lDataController.BoldHandle, lEdit, lFollower, lDone);
      end;
      if not lDone then
      begin
        lDone := (lFollower.Controller as TBoldVariantFollowerController).MayHaveChanged(Edit.EditingValue, lFollower);
      end;
      if lDone then
      begin
        lDataController.fInternalChange := true;
        try
          TBoldQueueable.DisplayAll;
        finally
          lDataController.fInternalChange := false;
          inherited;
        end;
      end;
    end;
  end
  else
    inherited;
end;

{ TcxGridBoldDefaultValuesProvider }

function TcxGridBoldDefaultValuesProvider.DefaultCanModify: Boolean;
begin
  Result := inherited DefaultCanModify {and Follower.MayModify};
end;

function TcxGridBoldDefaultValuesProvider.IsDisplayFormatDefined(
  AIsCurrencyValueAccepted: Boolean): Boolean;
begin
  with Owner as TcxGridItemBoldDataBinding do
    Result := IsDisplayFormatDefined(AIsCurrencyValueAccepted) or Assigned(BoldProperties.Renderer);
end;

{ TcxGridBoldTableViewInfo }

function TcxGridBoldTableViewInfo.GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass;
begin
  result := TcxBoldGridRowsViewInfo;
end;

function TcxGridBoldTableViewInfo.GetSiteClass: TcxGridSiteClass;
begin
  Result := TcxBoldGridSite;
end;

procedure TcxGridBoldTableViewInfo.Calculate;
var
  vDataController: TcxBoldDataController;
begin
  vDataController := DataController as TcxBoldDataController;
  if Assigned(vDataController) then
  begin
    with vDataController as TcxBoldDataController do
    begin
      if Assigned(vDataController.CustomDataSource) then
        if MainFollowerNeedsDisplay then
          DisplayFollowers;
    end;
  end;
  inherited;
end;

{ TcxGridBoldBandedTableViewInfo }

function TcxGridBoldBandedTableViewInfo.GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass;
begin
  Result := TcxGridBoldBandedRowsViewInfo;
end;

{ TcxGridBoldBandedTableViewInfo }

function TcxGridBoldBandedTableViewInfo.GetSiteClass: TcxGridSiteClass;
begin
  Result := TcxBoldGridSite;
end;

procedure TcxGridBoldBandedTableViewInfo.Calculate;
var
  vDataController: TcxBoldDataController;
begin
  vDataController := DataController as TcxBoldDataController;
  if Assigned(vDataController) then
  begin
    with vDataController as TcxBoldDataController do
    begin
      if Assigned(vDataController.CustomDataSource) then
        if MainFollowerNeedsDisplay then
          DisplayFollowers;
    end;
  end;
  inherited;
end;

{ TcxGridBoldCardViewViewInfo }

function TcxGridBoldCardViewViewInfo.GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass;
begin
  Result := TcxGridBoldCardsViewInfo;
end;

function TcxGridBoldCardViewViewInfo.GetSiteClass: TcxGridSiteClass;
begin
  Result := TcxBoldGridSite;
end;


{ TcxGridBoldLayoutView }

constructor TcxGridBoldLayoutView.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TcxGridBoldLayoutView.Destroy;
begin

  inherited;
end;

function TcxGridBoldLayoutView.CreateItem: TcxGridBoldLayoutViewItem;
begin
  Result := TcxGridBoldLayoutViewItem(inherited CreateItem);
end;

function TcxGridBoldLayoutView.GetCurrentBoldObject: TBoldObject;
begin
  result := DataController.CurrentBoldObject;
end;

function TcxGridBoldLayoutView.GetCurrentElement: TBoldElement;
begin
  result := DataController.CurrentElement;
end;

function TcxGridBoldLayoutView.GetCurrentIndex: integer;
begin
  result := DataController.CurrentIndex;
end;

function TcxGridBoldLayoutView.GetDataController: TcxGridBoldDataController;
begin
  Result := TcxGridBoldDataController(FDataController);
end;

function TcxGridBoldLayoutView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridBoldDataController;
end;

function TcxGridBoldLayoutView.GetItem(Index: Integer): IBoldAwareViewItem;
begin
  result := inherited Items[Index] as IBoldAwareViewItem;
end;

function TcxGridBoldLayoutView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridBoldLayoutViewItem;
end;

function TcxGridBoldLayoutView.GetItemCount: Integer;
begin
  result := inherited ItemCount;
end;

function TcxGridBoldLayoutView.GetSelection: TBoldList;
begin
  result := DataController.Selection;
end;

procedure TcxGridBoldLayoutView.SetDataController(
  Value: TcxGridBoldDataController);
begin
  FDataController.Assign(Value);
end;

function TcxGridBoldLayoutView.ValidateComponent(
  ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;
var
  i: integer;
  lContext: TBoldElementTypeInfo;
  lBoldValidateableComponent: IBoldValidateableComponent;
begin
  lContext := DataController.GetHandleStaticType;
  result := ComponentValidator.ValidateExpressionInContext(
    '', lContext, format('%s%s', [NamePrefix, Name])); // do not localize
  if assigned(lContext) then
  begin
    for i := 0 to ItemCount - 1 do
    begin
      result := ComponentValidator.ValidateExpressionInContext(
        Items[i].BoldProperties.Expression,
        lContext,
        format('%s%s.Column[%d]', [NamePrefix, Name, i]),
        Items[i].BoldProperties.VariableList) and result; // do not localize
      if Supports((self.DataController.GetItem(i) as TcxCustomGridTableItem).GetProperties, IBoldValidateableComponent, lBoldValidateableComponent) then
        result := lBoldValidateableComponent.ValidateComponent(ComponentValidator, namePrefix) and result;
    end;
  end;
end;

{ TcxGridBoldLayoutViewItem }

destructor TcxGridBoldLayoutViewItem.Destroy;
begin
  DataBinding.Remove;
  inherited;
end;

function TcxGridBoldLayoutViewItem.GetDataBinding: TcxGridItemBoldDataBinding;
begin
  Result := TcxGridItemBoldDataBinding(inherited DataBinding);
end;

procedure TcxGridBoldLayoutViewItem.SetDataBinding(
  Value: TcxGridItemBoldDataBinding);
begin
  inherited DataBinding := Value;
end;

{ TBoldCxGridVariantFollowerController }

constructor TBoldCxGridVariantFollowerController.Create(
  aOwningComponent: TComponent);
begin
  inherited Create(aOwningComponent);
end;

function TBoldCxGridVariantFollowerController.SubFollowersActive: boolean;
begin
  result := false; //cxGridItemBoldDataBinding.Item.ActuallyVisible;
end;

{ TcxBoldCustomDataControllerInfo }

procedure TcxBoldCustomDataControllerInfo.DoFilter;
var
  i: integer;
  RootFilter: TcxFilterCriteriaItemList;
  lList: TBoldList;
  lDataController: TcxBoldDataController;
begin
  try
    lDataController := (DataController as TcxBoldDataController);
    lList := lDataController.BoldList;
    RootFilter := (DataController.Filter as TcxFilterCriteria).Root;
    if not (not Assigned(lList) or lList.Empty or (RootFilter.Count = 0)) then
      for I := 0 to RootFilter.Count - 1 do
        if RootFilter.Items[i] is TcxDataFilterCriteriaItem then
          lDataController.PreFetchColumns(lList, lDataController.GetItemData(TcxDataFilterCriteriaItem(RootFilter.Items[i]).Field.Item));
  finally
    inherited;
  //  SelectionChanged; // ?  
  end;
end;

procedure TcxBoldCustomDataControllerInfo.DoSort;
var
  i: integer;
  lList: TBoldList;
  lWholeList: TBoldList;
  lGuard: IBoldGuard;
  IsObjectList: boolean;
begin
  try
    GetTotalSortingFields;
    if (TotalSortingFieldList.Count = 0) or (RecordList.Count = 0) then
      exit;
    lWholeList := TcxBoldDataController(DataController).BoldList;
    if not Assigned(lWholeList) then
      exit;
    if RecordList.Count = lWholeList.Count then
      lList := lWholeList as TBoldList
    else
    begin
      lGuard := TBoldGuard.Create(lList);
      lList := TcxBoldDataController(DataController).CreateList;
      lList.DuplicateMode := bldmAllow;
      lList.Capacity := RecordList.Count;
      IsObjectList := (lList is TBoldObjectList) and (lWholeList is TBoldObjectList);
      for I := 0  to RecordList.Count -1 do
      begin
        if Integer(RecordList[i]) < lWholeList.Count then
          if IsObjectList then
            TBoldObjectList(lList).AddLocator( TBoldObjectList(lWholeList).Locators[Integer(RecordList[i])] )
          else
            lList.Add( lWholeList[Integer(RecordList[i])] );
      end;
    end;
    for I := 0 to TotalSortingFieldList.Count - 1 do
    begin
      TcxBoldDataController(DataController).PreFetchColumns(lList, TcxBoldDataController(DataController).GetItemData(TotalSortingFieldList[i].Field.Item));
    end;
  finally
    inherited;
  end;
end;

{ TcxBoldDataSummary }

procedure TcxBoldDataSummary.CalculateSummary(
{$IFDEF BOLD_DELPHI16_OR_LATER}
  ASummaryItems: TcxDataSummaryItems; ABeginIndex, AEndIndex: Integer;
  var ACountValues: TcxDataSummaryCountValues; var ASummaryValues: TcxDataSummaryValues
{$ELSE}
  ASummaryItems: TcxDataSummaryItems; ABeginIndex, AEndIndex: Integer;
  var ACountValues: TcxDataSummaryCountValues; var ASummaryValues: TcxDataSummaryValues; var SummaryValues: Variant
{$ENDIF}
);
var
  I: Integer;
  lList: TBoldList;
  lWholeList: TBoldList;
  lGuard: IBoldGuard;
  ARecordIndex: Integer;
  IsObjectList: boolean;
begin
  lWholeList := TcxBoldDataController(DataController).BoldList;
  if (ASummaryItems.Count = 0) or not Assigned(lWholeList) then
    exit;
  if lWholeList.Count = 0 then
    exit;
  if (ABeginIndex = 0) and (AEndIndex = lWholeList.Count-1) then
  begin
    lList := lWholeList;
  end
  else
  begin
    lGuard := TBoldGuard.Create(lList);
    lList := TcxBoldDataController(DataController).CreateList;
    lList.DuplicateMode := bldmAllow;
    lList.Capacity := AEndIndex - ABeginIndex;
    IsObjectList := (lList is TBoldObjectList) and (lWholeList is TBoldObjectList);
    for I := ABeginIndex to AEndIndex do
    begin
      ARecordIndex := GetRecordIndex(I);
      if ARecordIndex <> -1 then
      begin
        if IsObjectList then
          TBoldObjectList(lList).AddLocator(TBoldObjectList(lWholeList).Locators[ARecordIndex])
        else
          lList.Add(lWholeList[ARecordIndex]);
      end
    end;
  end;
  for I := 0 to ASummaryItems.Count - 1 do
    if Assigned(ASummaryItems[i].Field) then
      with TcxBoldDataController(DataController) do
        PreFetchColumns(lList, GetItemData(ASummaryItems[i].Field.Item));
  inherited;        
end;

initialization
  cxGridRegisteredViews.Register(TcxGridBoldTableView, 'Bold Table');
  cxGridRegisteredViews.Register(TcxGridBoldCardView, 'Bold Card');
//  cxGridRegisteredViews.Register(TcxBoldGridChartView, 'Bold Chart');
  cxGridRegisteredViews.Register(TcxGridBoldBandedTableView, 'Bold Banded Table');
  cxGridRegisteredViews.Register(TcxGridBoldLayoutView, 'Bold Layout');
  Classes.RegisterClasses([TcxGridBoldColumn, TcxGridItemBoldDataBinding, TcxGridBoldBandedColumn, TcxGridBoldCardViewRow, TcxGridBoldLayoutViewItem]);

finalization
  cxGridRegisteredViews.Unregister(TcxGridBoldTableView);
  cxGridRegisteredViews.Unregister(TcxGridBoldCardView);
//  cxGridRegisteredViews.Unregister(TcxBoldGridChartView);
  cxGridRegisteredViews.Unregister(TcxGridBoldBandedTableView);
  cxGridRegisteredViews.Unregister(TcxGridBoldLayoutView);
  Classes.UnRegisterClasses([TcxGridBoldColumn, TcxGridItemBoldDataBinding, TcxGridBoldBandedColumn, TcxGridBoldCardViewRow, TcxGridBoldLayoutViewItem]);
//  FilterEditsController.Unregister(TcxSingleLinkEditProperties, TcxFilterSingleLinkEditHelper);

end.
