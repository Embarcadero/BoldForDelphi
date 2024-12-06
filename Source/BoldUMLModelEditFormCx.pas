
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelEditFormCx;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  BoldSystemHandle,
  BoldListHandle,
  Grids,
  BoldHandles,
  BoldModel,
  BoldGrid,
  BoldUMLModel,
  BoldUMLTypes,
  StdCtrls,
  ComCtrls,
  BoldXCVTreeView,
  BoldSubscription,
  BoldElements,
  BoldControlPack,
  BoldStringControlPack,
  BoldEdit,
  BoldCheckBox,
  Menus,
  ToolWin,
  BoldUMLModelEdit,
  BoldUMLModelEditPlugIn,
  ImgList,
  BoldComboBox,
  ActnList,
  ClipBrd,
  BoldReferenceHandle,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldRootedHandles,
  BoldExpressionHandle,
  BoldCheckboxStateControlPack,
  BoldFilteredHandle,
  BoldLabel,
  BoldTypeNameDictionary,
  BoldVariableHandle,
  BoldSystem,
  Commctrl,
  BoldSmallLogFrame,
  BoldDragObject,
  BoldDerivedHandle,
  BoldAbstractPersistenceHandleDB,
  BoldPropertiesController,
  BoldUMLModelEditorHandlesDataModule,
  AppEvnts, System.ImageList, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData,
  cxDataStorage, cxEdit, cxNavigator, dxDateRanges, dxScrollbarAnnotations,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridBoldSupportUnit, cxClasses, cxGridLevel, cxGrid, Data.DB, cxDBData,
  cxGridDBTableView, BoldTypeNameHandle, BoldAbstractModel, cxTextEdit,
  cxDropDownEdit, cxCheckBox, BoldVariantControlPack, cxContainer, cxBoldEditors,
  cxMemo, System.Actions, BoldUndoActions,
  BoldHandleAction, BoldActions, cxMaskEdit, cxGroupBox, cxCheckGroup,
  dxNumericWheelPicker, cxListView,
  BoldUndoForm, dxBar, cxSpinEdit, cxLookupEdit, cxBoldLookupEdit,
  cxBoldLookupComboBox, cxBoldExtLookupComboBox, cxLabel, cxHyperLinkEdit,
  BoldPropertyMapper,
  BoldUMLModelValidationFormCx, Vcl.StdActns, cxImageList, BoldAction,
  BoldPlaceableSubscriber,
  BoldUMLModelDataModule;

type
  CutOrCopyKind = (cckCut, cckCopy, cckNone);

type
  TUMLPlugInToolButton = class(TdxBarButton)
  private
    FPlugIn: TUMLPlugIn;
  public
    property PlugIn: TUMLPlugIn read FPlugIn write FPlugIn;
  end;

  TBoldModelEditFrmCx = class(TForm, IUnknown, IUMLModelPlugInContext, IBoldModelEditForm)
    Flatten: TMenuItem;
    Boldifymodel1: TMenuItem;
    Loggform1: TMenuItem;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    Edit1: TMenuItem;
    Tools1: TMenuItem;
    Clear1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Undo1: TMenuItem;
    mnuConsistencycheck: TMenuItem;
    N3: TMenuItem;
    N8: TMenuItem;
    SaveFileAs1: TMenuItem;
    popTree: TPopupMenu;
    DeleteXxx1: TMenuItem;
    N9: TMenuItem;
    NewAttribute1: TMenuItem;
    NewOperation1: TMenuItem;
    N10: TMenuItem;
    NewClass1: TMenuItem;
    NewAssociation1: TMenuItem;
    NewParameter1: TMenuItem;
    Model1: TMenuItem;
    Deletexxx2: TMenuItem;
    N11: TMenuItem;
    NewClass2: TMenuItem;
    NewAssociation2: TMenuItem;
    NewAttribute2: TMenuItem;
    NewOperation2: TMenuItem;
    NewParameter2: TMenuItem;
    StatusBar1: TStatusBar;
    N2: TMenuItem;
    ilTreeView: TImageList;
    ilMenu: TImageList;
    N4: TMenuItem;
    NewQualifier1: TMenuItem;
    NewQualifier2: TMenuItem;
    OpenFile1: TMenuItem;
    mnuOverrideFrameworkMethods: TMenuItem;
    mnuOverrideModelMethods: TMenuItem;
    mnuOverrideInAllSubclasses: TMenuItem;
    NewDatatype1: TMenuItem;
    FetchDatatypes: TMenuItem;
    NewPackage1: TMenuItem;
    InsertSuperclass1: TMenuItem;
    NewDataType2: TMenuItem;
    InsertSuperclass2: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    AddSubclass1: TMenuItem;
    AddSubclass2: TMenuItem;
    AppEvents: TApplicationEvents;
    pcLeft: TPageControl;
    tsClasses: TTabSheet;
    cxGridClasses: TcxGrid;
    tvClasses: TcxGridBoldTableView;
    cxLevelClasses: TcxGridLevel;
    cxGridAttributes: TcxGrid;
    cxGridAttributesLevel: TcxGridLevel;
    tvAttributes: TcxGridBoldTableView;
    cxGridAssociationEnds: TcxGrid;
    tvAssociationEnds: TcxGridBoldTableView;
    cxGridLevelAssociationEnds: TcxGridLevel;
    tvAttributesConstraints: TcxGridBoldColumn;
    tvAttributesname: TcxGridBoldColumn;
    tvAttributesvisibility: TcxGridBoldColumn;
    tvAttributesqualifiedName: TcxGridBoldColumn;
    tvAttributesderived: TcxGridBoldColumn;
    tvAttributesmodel: TcxGridBoldColumn;
    tvAttributesowner: TcxGridBoldColumn;
    tvAttributestype: TcxGridBoldColumn;
    tvAttributesinitialValue: TcxGridBoldColumn;
    tvAttributespersistent: TcxGridBoldColumn;
    tvAssociationEndsConstraints: TcxGridBoldColumn;
    tvAssociationEndsname: TcxGridBoldColumn;
    tvAssociationEndsvisibility: TcxGridBoldColumn;
    tvAssociationEndsqualifiedName: TcxGridBoldColumn;
    tvAssociationEndsderived: TcxGridBoldColumn;
    tvAssociationEndsmodel: TcxGridBoldColumn;
    tvAssociationEndsqualifyingOwner: TcxGridBoldColumn;
    tvAssociationEndsisNavigable: TcxGridBoldColumn;
    tvAssociationEndsaggregation: TcxGridBoldColumn;
    tvAssociationEndsmultiplicity: TcxGridBoldColumn;
    tvAssociationEndsmulti: TcxGridBoldColumn;
    tvAssociationEndsmandatory: TcxGridBoldColumn;
    tvAssociationEndsisOrdered: TcxGridBoldColumn;
    tvAssociationEndstype: TcxGridBoldColumn;
    tvAssociationEndsotherEnd: TcxGridBoldColumn;
    tvAssociationEndsassociation: TcxGridBoldColumn;
    tvClassesConstraints: TcxGridBoldColumn;
    tvClassesname: TcxGridBoldColumn;
    tvClassesisAbstract: TcxGridBoldColumn;
    tvClassespersistent: TcxGridBoldColumn;
    tvClassessuperclass: TcxGridBoldColumn;
    tvClassesisAssociationClass: TcxGridBoldColumn;
    tvClassesassociation: TcxGridBoldColumn;
    tvClassesmodel: TcxGridBoldColumn;
    Splitter9: TSplitter;
    Splitter10: TSplitter;
    pgClient: TPageControl;
    tabAttributes: TTabSheet;
    tabAssociationEnds: TTabSheet;
    tabClientOperations: TTabSheet;
    cxGridOperations: TcxGrid;
    tvOperations: TcxGridBoldTableView;
    cxLevelOperations: TcxGridLevel;
    tvOperationsname: TcxGridBoldColumn;
    tvOperationsvisibility: TcxGridBoldColumn;
    tabAssociations: TTabSheet;
    cxGridClassAssociations: TcxGrid;
    tvClassAssociations: TcxGridBoldTableView;
    cxGridLevelClassAssociations: TcxGridLevel;
    tvClassAssociationsConstraints: TcxGridBoldColumn;
    tvClassAssociationsname: TcxGridBoldColumn;
    tvClassAssociationsderived: TcxGridBoldColumn;
    tvClassAssociationsisNavigable: TcxGridBoldColumn;
    tvClassAssociationsordering: TcxGridBoldColumn;
    tvClassAssociationsaggregation: TcxGridBoldColumn;
    tvClassAssociationsmultiplicity: TcxGridBoldColumn;
    tvClassAssociationsmulti: TcxGridBoldColumn;
    tvClassAssociationsmandatory: TcxGridBoldColumn;
    tvClassAssociationsisOrdered: TcxGridBoldColumn;
    tvClassAssociationstype: TcxGridBoldColumn;
    tvClassAssociationsassociation: TcxGridBoldColumn;
    OpenOtherEnd: TMenuItem;
    TabOCL: TTabSheet;
    cxGridOclExpressions: TcxGrid;
    tvOclExpressions: TcxGridBoldTableView;
    tvOclExpressionsName: TcxGridBoldColumn;
    cxLevelOclExpressions: TcxGridLevel;
    tvOclExpressionsExpression: TcxGridBoldColumn;
    tvAttributesOCLExpression: TcxGridBoldColumn;
    tvAssociationEndsOclExpression: TcxGridBoldColumn;
    tvClassAssociationsOCLExpression: TcxGridBoldColumn;
    cxStyleRepository1: TcxStyleRepository;
    cxBoldStyle: TcxStyle;
    ActionList1: TActionList;
    actFind: TAction;
    Find1: TMenuItem;
    BoldUndoAction1: TBoldUndoAction;
    BoldRedoAction1: TBoldRedoAction;
    Redo1: TMenuItem;
    pnlLeft: TPanel;
    pnlClassFilter: TPanel;
    cxCheckGroup1: TcxCheckGroup;
    tvOperationsisOwnerscope: TcxGridBoldColumn;
    tvOperationsDelphiFunctionType: TcxGridBoldColumn;
    tvOperationsDelphiName: TcxGridBoldColumn;
    tvOperationsisExpressionName: TcxGridBoldColumn;
    tvAttributesLength: TcxGridBoldColumn;
    tvAttributesAllowNull: TcxGridBoldColumn;
    tvAttributesDelayedFetch: TcxGridBoldColumn;
    tvAttributesGetMethod: TcxGridBoldColumn;
    tvAttributesSetMethod: TcxGridBoldColumn;
    tvAttributesLocalVariable: TcxGridBoldColumn;
    tvAttributesstereotype: TcxGridBoldColumn;
    tvAttributesDelphiName: TcxGridBoldColumn;
    tvAttributesExpressionName: TcxGridBoldColumn;
    tvAttributesPMapperName: TcxGridBoldColumn;
    tvAttributesColumnName: TcxGridBoldColumn;
    tvAttributesDefaultDBValue: TcxGridBoldColumn;
    tvAssociationEndsstereotype: TcxGridBoldColumn;
    tvAssociationEndsEmbed: TcxGridBoldColumn;
    pnlBottom: TPanel;
    pcBottom: TPageControl;
    tsValidation: TTabSheet;
    dxBarManager1: TdxBarManager;
    dxBarManager1Bar1: TdxBar;
    dxComboDatabase: TdxBarCombo;
    dxBarManager1Bar2: TdxBar;
    dxBarButton1: TdxBarButton;
    actAddDb: TAction;
    tsModel: TTabSheet;
    tsDataTypes2: TTabSheet;
    cxGrid2: TcxGrid;
    tvDataTypes: TcxGridBoldTableView;
    cxGridBoldColumn3: TcxGridBoldColumn;
    cxGridBoldColumn4: TcxGridBoldColumn;
    cxGridBoldColumn5: TcxGridBoldColumn;
    cxGridLevel2: TcxGridLevel;
    tsPackages: TTabSheet;
    lblModelName: TLabel;
    lblModelDelphiName: TLabel;
    lblModelExpressionName: TLabel;
    lblModelPMapperName: TLabel;
    lblModelInterfaceUses: TLabel;
    lblModelImplementationUses: TLabel;
    Label1: TLabel;
    lblModelStereotype: TLabel;
    lblModelConstraints: TLabel;
    lblModelGUID: TLabel;
    lblModelTypeLibVersion: TLabel;
    lblModelOptimisticLocking: TLabel;
    tbxModelName: TBoldEdit;
    tbxModelDelhpiName: TBoldEdit;
    tbxModelExpressionName: TBoldEdit;
    tbxModelPMapperName: TBoldEdit;
    tbxModelInterfaceUses: TBoldEdit;
    tbxModelImplementationUses: TBoldEdit;
    btInterfaceUses: TButton;
    btImplementationUses: TButton;
    bcbModelUseXFiles: TBoldCheckBox;
    bcbModelUseTimestamp: TBoldCheckBox;
    bcbUseGlobalId: TBoldCheckBox;
    bcbUseReadOnly: TBoldCheckBox;
    tbxModelStereotype: TBoldEdit;
    tbxModelConstraints: TBoldEdit;
    btModelConstraintEditor: TButton;
    edModelGUID: TBoldEdit;
    edModelTypeLibVersion: TBoldEdit;
    bcbUseClockLog: TBoldCheckBox;
    bcbGenerateMultiplicityConstraints: TBoldCheckBox;
    cmbModelOptimisticLocking: TBoldComboBox;
    tbxModelRootClass: TBoldEdit;
    cxGridPackages: TcxGrid;
    tvPackages: TcxGridBoldTableView;
    lvlPackages: TcxGridLevel;
    tvPackagesname: TcxGridBoldColumn;
    tvPackagesqualifiedName: TcxGridBoldColumn;
    tvPackagesnamespace: TcxGridBoldColumn;
    tvPackagesmodel: TcxGridBoldColumn;
    tvPackagesqualifyingOwner: TcxGridBoldColumn;
    tsClass: TTabSheet;
    lblClassName: TLabel;
    lblClassDelphiName: TLabel;
    lblClassExpressionName: TLabel;
    lblClassPMapperName: TLabel;
    lblClassTableName: TLabel;
    lblClassFileName: TLabel;
    lblClassTableMapping: TLabel;
    lblClassSuperclass: TLabel;
    blbClassInfo: TBoldLabel;
    Label4: TLabel;
    lblClassStereotype: TLabel;
    lblClassConstraint: TLabel;
    lblClassDerivationExpressions: TLabel;
    Label12: TLabel;
    lblClassDefaultStringRep: TLabel;
    lblClassGUID: TLabel;
    lblClassOptimisticLocking: TLabel;
    tbxClassName: TBoldEdit;
    tbxClassDelphiName: TBoldEdit;
    tbxClassExpressionName: TBoldEdit;
    tbxClassTableName: TBoldEdit;
    tbxClassFileName: TBoldEdit;
    bcbClassPersistent: TBoldCheckBox;
    bcbClassAbstract: TBoldCheckBox;
    bcbClassImported: TBoldCheckBox;
    cmbClassPMapperName: TBoldComboBox;
    cmbTableMapping: TBoldComboBox;
    tbxClassStereotype: TBoldEdit;
    tbxClassConstraint: TBoldEdit;
    btClassConstraintEditor: TButton;
    tbxClassDerivationExpressions: TBoldEdit;
    btShowDerivationExpressionsEditor: TButton;
    tbxClassUnitName: TBoldEdit;
    edClassDefaultStringRep: TBoldEdit;
    btClassDefaultStringRep: TButton;
    edClassGUID: TBoldEdit;
    cmbClassOptimisticLocking: TBoldComboBox;
    bcbRemoveSuperOnUnboldify: TBoldCheckBox;
    bcbRemoveOnUnboldify: TBoldCheckBox;
    bcbIsRootClass: TBoldCheckBox;
    tsAssociations: TTabSheet;
    gridAssociations: TcxGrid;
    tvAssociations: TcxGridBoldTableView;
    lvAssociations: TcxGridLevel;
    tvAssociationsConstraints: TcxGridBoldColumn;
    tvAssociationsname: TcxGridBoldColumn;
    tvAssociationsstereotypeName: TcxGridBoldColumn;
    tvAssociationsderived: TcxGridBoldColumn;
    tvAssociationspersistent: TcxGridBoldColumn;
    tvAssociationsisAssociationClass: TcxGridBoldColumn;
    tvAssociationsclass: TcxGridBoldColumn;
    tsClassStats: TTabSheet;
    cxGrid4: TcxGrid;
    cxGridBoldTableView3: TcxGridBoldTableView;
    cxGridLevel4: TcxGridLevel;
    cxGridBoldTableView3Constraints: TcxGridBoldColumn;
    cxGridBoldTableView3name: TcxGridBoldColumn;
    cxGridBoldTableView3Column1: TcxGridBoldColumn;
    cxGridBoldTableView3Column2: TcxGridBoldColumn;
    cxGridBoldTableView3Column4: TcxGridBoldColumn;
    cxGridBoldTableView3Column8: TcxGridBoldColumn;
    cxGridBoldTableView3Column3: TcxGridBoldColumn;
    cxGridBoldTableView3Column5: TcxGridBoldColumn;
    cxGridBoldTableView3Column6: TcxGridBoldColumn;
    tvAttributesqualifyingOwner: TcxGridBoldColumn;
    tabFeatures: TTabSheet;
    cxGrid5: TcxGrid;
    cxGridBoldTableView4: TcxGridBoldTableView;
    cxGridBoldColumn6: TcxGridBoldColumn;
    cxGridBoldColumn7: TcxGridBoldColumn;
    cxGridBoldColumn8: TcxGridBoldColumn;
    cxGridBoldColumn9: TcxGridBoldColumn;
    cxGridBoldColumn14: TcxGridBoldColumn;
    cxGridBoldColumn15: TcxGridBoldColumn;
    cxGridBoldColumn17: TcxGridBoldColumn;
    cxGridLevel5: TcxGridLevel;
    cxGridBoldTableView4Column1: TcxGridBoldColumn;
    cxCheckBox1: TcxCheckBox;
    cxCheckBox2: TcxCheckBox;
    cxCheckBox3: TcxCheckBox;
    cxCheckBox4: TcxCheckBox;
    cxCheckBox6: TcxCheckBox;
    tvOperationsqualifyingOwner: TcxGridBoldColumn;
    cxCheckBox5: TcxCheckBox;
    cxCheckBox7: TcxCheckBox;
    tvClassAssociationsqualifyingOwner: TcxGridBoldColumn;
    btnGotoSuperClass: TButton;
    tvOclExpressionsqualifiedName: TcxGridBoldColumn;
    tvOclExpressionsType: TcxGridBoldColumn;
    cxCheckBox8: TcxCheckBox;
    tvOclExpressionsqualifyingOwner: TcxGridBoldColumn;
    BoldPropertyMapper1: TBoldPropertyMapper;
    tvAssociationsColumn1: TcxGridBoldColumn;
    tvAssociationsColumn2: TcxGridBoldColumn;
    tvAssociationsColumn3: TcxGridBoldColumn;
    tvAssociationsColumn4: TcxGridBoldColumn;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    cxImageList1: TcxImageList;
    dxBarButton2: TdxBarButton;
    dxBarButton3: TdxBarButton;
    dxBarButton4: TdxBarButton;
    dxBarButton5: TdxBarButton;
    dxBarManager1Bar3: TdxBar;
    dxBarManager1Bar4: TdxBar;
    actConsistencyCheck: TAction;
    actTagEdit: TAction;
    dxBarButton6: TdxBarButton;
    dxBarButton7: TdxBarButton;
    behViolations: TBoldExpressionHandle;
    actDelete: TBoldAction;
    actNewOperation: TBoldAction;
    actNewClass: TBoldAction;
    actNewAssociation: TBoldAction;
    actNewDataType: TBoldAction;
    actAddSubclass: TBoldAction;
    actNewAttribute: TBoldAction;
    actNewParameter: TBoldAction;
    actNewQualifier: TBoldAction;
    actInsertSuperClass: TBoldAction;
    Panel2: TPanel;
    cmbSuperClass: TcxBoldComboBox;
    actSelectSuperClass: TAction;
    N7: TMenuItem;
    N12: TMenuItem;
    cxCheckBox9: TcxCheckBox;
    cxGridBoldTableView3Column7: TcxGridBoldColumn;
    cxGridBoldTableView3Column9: TcxGridBoldColumn;
    cxGridBoldTableView4Column2: TcxGridBoldColumn;
    cxGridBoldTableView4Column3: TcxGridBoldColumn;
    tvAssociationsColumn5: TcxGridBoldColumn;
    tvAssociationEndsColumn1: TcxGridBoldColumn;
    tvClassAssociationsColumn1: TcxGridBoldColumn;
    tvDataTypesColumn1: TcxGridBoldColumn;
    tvDataTypesColumn2: TcxGridBoldColumn;
    BoldEdit1: TBoldEdit;
    Label2: TLabel;
    cxGridBoldTableView4Column4: TcxGridBoldColumn;
    tvAttributesReverseDerived: TcxGridBoldColumn;
    tvAssociationsColumn6: TcxGridBoldColumn;
    tvClassAssociationsColumn2: TcxGridBoldColumn;
    procedure Boldifymodel1Click(Sender: TObject);
    procedure FlattenClick(Sender: TObject);
    procedure Loggform1Click(Sender: TObject);
    procedure Tools1Click(Sender: TObject);
    procedure cmbSuperClassSelectChanged(Sender: TObject);
    procedure bdhAttributePMapperNamesDeriveAndSubscribe(Sender: TComponent;
      RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
      Subscriber: TBoldSubscriber);
    procedure Clear1Click(Sender: TObject);
    procedure popTreePopup(Sender: TObject);
    procedure DeleteXxx1Click(Sender: TObject);
    procedure NewClass1Click(Sender: TObject);
    procedure NewAssociation1Click(Sender: TObject);
    procedure NewAttribute1Click(Sender: TObject);
    procedure NewOperation1Click(Sender: TObject);
    procedure NewParameter1Click(Sender: TObject);
    procedure NewQualifier1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure SaveFileAs1Click(Sender: TObject);
    procedure mnuOverrideInAllSubclassesClick(Sender: TObject);
    procedure btInterfaceUsesClick(Sender: TObject);
    procedure btImplementationUsesClick(Sender: TObject);
    procedure Splitter2CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    function bcrGetSetGetAsCheckBoxState(aFollower: TBoldFollower): TCheckBoxState;
    procedure bcrGetSetSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
    function bcrGetSetMayModify(aFollower: TBoldFollower): Boolean;
    procedure bcrBooleanToCheckBoxSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
    function bcrBooleanToCheckBoxGetAsCheckBoxState(aFollower: TBoldFollower): TCheckBoxState;
    procedure bcrBooleanToCheckBoxSetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
    procedure bcrGetSetSetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
    procedure btModelConstraintEditorClick(Sender: TObject);
    procedure btShowDerivationExpressionsEditorClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btAttributeShowDerivExprEditorClick(Sender: TObject);
    procedure btAssoEndShowDeriExprEditorClick(Sender: TObject);
    procedure BoldTreeView1Paste(Sender: TObject);
    procedure btClassDefaultStringRepClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure EnsureTypesFromTypeNameHandle(Sender: TObject);
    procedure NewDatatype1Click(Sender: TObject);
    procedure NewPackage1Click(Sender: TObject);
    procedure InsertSuperclass1Click(Sender: TObject);
    function bcrBooleanToCheckBoxMayModify(aFollower: TBoldFollower): Boolean;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure AddSubclass1Click(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure AppEventsHint(Sender: TObject);
    procedure OpenOtherEndClick(Sender: TObject);
    function rDataTypePMapperGetAsVariant(AFollower: TBoldFollower): Variant;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure tvDataControllerAfterLoad(Sender: TObject);
    procedure tvGridViewKeyPress(Sender: TObject; var Key: Char);
    procedure tvGridDblClick(Sender: TObject);
    procedure dxComboDatabaseChange(Sender: TObject);
    procedure actAddDbExecute(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure cxCheckBox3Click(Sender: TObject);
    procedure cxCheckBox4Click(Sender: TObject);
    procedure cxCheckBox6Click(Sender: TObject);
    procedure cxCheckBox7Click(Sender: TObject);
    procedure cxCheckBox5Click(Sender: TObject);
    procedure cxCheckBox8Click(Sender: TObject);
    procedure tvOclExpressionsDblClick(Sender: TObject);
    procedure AppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure FileOpen1BeforeExecute(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure actConsistencyCheckExecute(Sender: TObject);
    procedure actTagEditExecute(Sender: TObject);
    procedure tvNameCustomDrawCell(
      Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure tvDefaultCustomDrawCell(
      Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure tvAggregationCustomDrawCell(
      Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
      AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FileSaveAs1BeforeExecute(Sender: TObject);
    procedure actSelectSuperClassUpdate(Sender: TObject);
    procedure actSelectSuperClassExecute(Sender: TObject);
    procedure cxCheckBox9Click(Sender: TObject);
    procedure actEvaluateOCLExecute(Sender: TObject);
    procedure cxCheckGroup1PropertiesChange(Sender: TObject);
    procedure tvDataTypesDblClick(Sender: TObject);
    procedure actTagEditUpdate(Sender: TObject);
  private
    { Private declarations }
//    fdmBoldUMLModelEditorHandles: TdmBoldUMLModelEditorHandles;
//    fdmModelEdit: TdmModelEdit;
    fStartup: TDateTime;
    fValidationForm: TfrmValidationCx;
    fModelChangedSubscriber: TBoldPassthroughSubscriber;
    fModelSubscriptionsValid: Boolean;
    fIgnoreModelChanges: Boolean;
    fShowAttribKindFeaturesSubscriber: TBoldPassThroughSubscriber;
    fPlugInList: TList;
    CutOrCopy: CutOrCopyKind;
    GreyNode: TTreeNode;
    FrameworkMethodMenuItems: TList;
    ModelMethods: TList;
    fLogframe: TBoldLogFrame;
    fIsExecutingPlugin: Boolean;
    fModelNeedsValidation: Boolean;
    fModelHandle: TBoldModel;
    fUndoForm: TfrmBoldUndo;
    fSelectionHistory: TBoldObjectList;
    fSelectionHistoryIndex: integer;
    fLoadedFrom: string;
    fCodeGenerator: TObject;
//    procedure FindComponentInstanceEvent(Reader: TReader; const Name: string; var Instance: Pointer);
    procedure LoadFormsettingsFromRegistry;
    procedure SaveFormsettingsToRegistry;
    procedure SetCheckBoxHints;
    procedure EditOclExpression(Element: TUMLModelElement; TaggedValue: String; Context: TUMLModelElement);
    function GetUMLObjectToCopyOrMove: TUMLModelElement;
    procedure SetUMLObjectToCopyOrMove(const Value: TUMLModelElement);
    function GetCurrentModelIsBoldified: Boolean;
    procedure EnsureUnFlattenedAndUnBoldified;
    procedure SetModelNeedsValidation(const Value: Boolean);
    procedure SetIgnoreModelChanges(const Value: Boolean);
    procedure SubscribeToModelChanges;
    function GetIsExecutingPlugin: boolean;
    procedure SetIsExecutingPlugin(Value: boolean);
    procedure OnOverrideMenuItemClick(Sender: TObject);
    procedure OverrideFrameworkMethod(MethodName: String);
    procedure OverrideModelMethod(MethodName: String);
    procedure FreeRefsInList(List: TList);
    procedure CreateModelMethodItems(UMLClass: TUMLClass);
    procedure CreateFrameworkMethodItems(UMLClass: TUMLClass);
    procedure ClearMenuItems(var MenuItem: TMenuItem);
    procedure GetParamNames(Method: String; ResultList: TStringList);
    procedure GetParamTypes(Method: String; ResultList: TStringList);
    function GetReturnType(Method: String): String;
    procedure DroppedOnUMLClass(Dropped: TUMLModelElement; Target: TUMLClass);
    procedure DroppedOnUMLOperation(Dropped: TUMLParameter; Target: TUMLOperation);
    procedure DroppedOnUMLPackage(Dropped: TUMLModelElement; Target: TUMLPackage);

    function DropOnUMLClass(ToBeDropped: TUMLModelElement): Boolean;
    function DropOnUMLPackage(ToBeDropped: TUMLModelElement): Boolean;
    function DropOnUMLAttribute(ToBeDropped: TUMLModelElement): Boolean;
    function DropOnUMLOperation(ToBeDropped: TUMLModelElement): Boolean;
    function DropOnUMLParameter(ToBeDropped: TUMLModelElement): Boolean;
    function DropOnUMLAssociation(ToBeDropped: TUMLModelElement): Boolean;
    function DropOnUMLAssociationEnd(ToBeDropped: TUMLModelElement): Boolean;

    procedure ModelChangedRecieve(Originator: TObject;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure ShowAttribKindFeaturesRecieve(Originator: TObject;
      OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
    procedure SetCurrentElement(Element: TUMLModelElement);
    procedure AddMenuTool(MenuItem: TMenuItem);
    procedure AddMenuFiler(MenuItem: TMenuItem);
    procedure AddButtonTool(ToolButton: TUMLPlugInToolButton);
    procedure AddButtonFiler(ToolButton: TUMLPlugInToolButton);
    procedure AddTool(PlugIn: TUMLPlugIn);
    procedure AddFiler(PlugIn: TUMLPlugIn);
    procedure GetCopyPasteRef;
    function NewPlugInMenuItem(PlugIn: TUMLPlugIn): TUMLPlugInMenuItem;
    function NewPlugInToolButton(PlugIn: TUMLPlugIn): TUMLPlugInToolButton;
    function GetPlugInList: TList;
    procedure SetPlugInList(const Value: TList);
    procedure InstallPlugIn(PlugIn: TUMLPlugIn);
    procedure PasteAssociation;
    procedure PasteAssociationEnd;
    procedure PasteFeature;
    procedure PasteClass;
    procedure PasteParameter;
    procedure PlugInMenuClick(sender: TObject);
    procedure PlugInButtonClick(sender: TObject);
    procedure FreeItemsInMenu(InMenu: TMenuItem);
    function FindFrameworkMethod(MethodName: String): String;
    function FindModelMethod(MethodName: String): TUMLOperation;
    procedure DroppedOnUMLParameter(Dropped: TUMLModelElement; Target: TUMLParameter);
    procedure ExecPlugIn(PlugIn: TUMLPlugin);
    procedure CreateParamsFromStringList(ParamNames, ParamTypes: TStringList; NewOperation: TUMLOperation; ReturnType: String);
    function DumpHandles(AHandle: TBoldElementHandle): string;
    procedure SetLoadedFrom(const Value: string);
    property UMLObjectToCopyOrMove: TUMLModelElement read GetUMLObjectToCopyOrMove write SetUMLObjectToCopyOrMove;
    procedure SetBoldifyFlattenCaptions;
    procedure ApplyGUI;
    function GetElementToPaste: TUMLModelElement;
    procedure UpdateStatusbar;
    function GetCurrentClass: TUMLClass;
    function GetCurrentPackage: TUMLNameSpace;
    function GetCurrentTypeNameDictionary: TBoldTypeNameDictionary;
    function GetCurrentModelHandle: TBoldModel;
    procedure SetModelHandle(const Value: TBoldModel);
    procedure SearchModel(const Text: string);
    procedure SyncSelection;
    procedure WMAppCommand(var Msg: TMessage); message WM_APPCOMMAND;
    procedure ClearClassFilter;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetCurrentModel: TUMLModel;
    function GetCurrentElement: TUMLModelElement;
    property IgnoreModelChanges: Boolean read FIgnoreModelChanges write SetIgnoreModelChanges;
  public
    constructor Create(AOnwer: TComponent); override;
    property CurrentElement: TUMLModelElement read GetCurrentElement write SetCurrentElement;
    property ModelHandle: TBoldModel read GetCurrentModelHandle write SetModelHandle;
    property CurrentModel: TUMLModel read GetCurrentModel;
    property CurrentTypeNameDictionary: TBoldTypeNameDictionary read GetCurrentTypeNameDictionary;
    property CurrentModelIsBoldified: Boolean read GetCurrentModelIsBoldified;
    property PlugInlist: TList read FPlugInlist write SetPlugInlist;
    procedure JumpToElement(element: TUMLModelElement);
    function CloseThisForm: TCloseAction;
    procedure LoadModelFromFile;
    procedure SaveModelToFile(const AFileName: string);
    property ModelNeedsValidation: Boolean read FModelNeedsValidation write SetModelNeedsValidation;
    procedure EnsureFlattenedAndBoldified;
    property LoadedFrom: string write SetLoadedFrom;
  end;

//var
//  BoldModelEditFrm: TBoldModelEditFrm;

implementation

uses
  BoldDefaultTaggedValues,
  BoldUMLAttributes,
  BoldUMLModelConverter,
  BoldUtils,
  BoldGuard,
  BoldCursorGuard,
  BoldAttributes,
  BoldUMLModelValidator,
  BoldUMLModelLink,
  BoldLogHandler,
  //BoldSystemDebuggerForm,
  BoldPMapperLists,
  BoldDefs,
//  BoldDefsDT,
  BoldGUI,
  BoldQueue,
  BoldUMLUsesEditorForm,
 // BoldMeta,
  BoldTaggedValueSupport,
  BoldUMLConstraintEditorCx,
//  BoldUMLTaggedValuesEditor,
  BoldDerivationExpressionsEditorCx,
  BoldUMLOCLEditor,
  BoldUMLModelSupport,
  BoldUMLPluginCallBacks,
  BoldUMLAbstractModelValidator,
  BoldLogHandlerForm,
  System.Variants,
  System.IOUtils,
  System.UITypes,
  BoldQueryUserDlg,
  BoldUMLAttributeEditor,
  BoldUMLAssociationEditor,
  BoldSystemRT,
  BoldUMLOperationEditor,
  BoldUMLTaggedValuesEditorCx,
  BoldUMLModelPersistenceDataModule,
  FireDAC.Comp.Client,
  FireDAC.VCLUI.ConnEdit,
  BoldHandle,
  BoldOclPropEditor,
  BoldRegistry,
  BoldUMLPlugins,
  BoldGen,
  {$IFDEF AttracsModelEditor}
  DMAttracsPMappers, AttracsAttributes, BAWeekDay,
  {$ENDIF}
  BoldEnvironment,
  BoldMemberTypeDictionary,
  BoldUndoHandler;

{$R *.dfm}

var
  DefaultCodeOutputDirName: string;

function FindGlobalComponent(const Name: string): TComponent;
begin
  if AnsiSameText(Name, 'dmBoldUMLModelEditorHandles') then
    Result := dmBoldUMLModelEditorHandles
  else
  if AnsiSameText(Name, 'dmModelEdit') then
    result := nil
  else
    Result := nil;
end;

type
  TStandAloneCodeGenerator = class(TUMLPlugInFunction)
  protected
    function GetMenuItemName: String; override;
    function GetImageResourceName: String; override;
    function GetPlugInType: TPlugInType; override;
    function GetImageMaskColor: TColor; override;
  public
    procedure Execute(context: IUMLModelPlugInContext); override;
  end;

function TStandAloneCodeGenerator.GetMenuItemName: String;
begin
  result := 'Generate Code';
end;

function TStandAloneCodeGenerator.GetImageResourceName: String;
begin
  result := 'UMLPluginGenCodeImage';
end;

function TStandAloneCodeGenerator.GetImageMaskColor: TColor;
begin
  result := clTeal;
end;

function TStandAloneCodeGenerator.GetPlugInType: TPlugInType;
begin
  result := ptTool;
end;

Procedure TStandAloneCodeGenerator.Execute(context: IUMLModelPlugInContext);
var
  vGenerator: TBoldGenerator;
begin
  vGenerator := TBoldGenerator.Create(context.GetCurrentModelHandle.TypeNameDictionary);
  try
    vGenerator.BaseFilePath := DefaultCodeOutputDirName;
    vGenerator.UseTypedLists := true;
    vGenerator.MoldModel := Context.GetCurrentModelHandle.MoldModel;
    vGenerator.GenerateBusinessObjectCode;
    vGenerator.EnsureMethodImplementations;
  finally
    vGenerator.Free;
  end;
end;

procedure TBoldModelEditFrmCx.SetCurrentElement(Element: TUMLModelElement);
var
  i: integer;
begin
  ApplyGUI;
  dmBoldUMLModelEditorHandles.brhCurrentElement.Value := Element;
  inc(fSelectionHistoryIndex);
  for i := fSelectionHistory.Count-1 downto fSelectionHistoryIndex do
    fSelectionHistory.RemoveByIndex(i);
  fSelectionHistory.Add(Element);
//  fSelectionHistoryIndex := fSelectionHistory.Count-1;
end;

procedure TBoldModelEditFrmCx.popTreePopup(Sender: TObject);
var
  i: integer;
  ActiveGrid: TcxCustomGridView;
  CurrentClass: TUMLClass;
begin
  if activecontrol is TcxBoldGridSite then
    ActiveGrid := TcxBoldGridSite(activecontrol).GridView
  else
    ActiveGrid := nil;
  CurrentClass := nil;
  DeleteXxx1.Visible := False;
  N10.Visible := False;
  NewClass1.Visible := False;
  NewAssociation1.Visible := False;
  NewAttribute1.Visible := False;
  NewOperation1.Visible := False;
  NewParameter1.Visible := False;
  NewQualifier1.Visible := False;
  NewDataType1.Visible := False;
  InsertSuperclass1.Visible := False;
  AddSubclass1.Visible := False;
  NewPackage1.Visible := False;
  N9.Visible := False;
  mnuOverrideFrameworkMethods.Visible := False;
  mnuOverrideModelMethods.Visible := False;
  mnuOverrideInAllSubclasses.Visible := False;
  OpenOtherEnd.Visible := false;
  if CurrentElement = nil then
    exit;
  if (CurrentElement is TUMLPackage) or (ActiveGrid = tvPackages) then
  begin
    if not (CurrentElement is TUMLModel) and not (CurrentElement is TUMLClass) then
    begin
      DeleteXxx1.Caption := 'Delete Package ' + CurrentElement.Name;
      DeleteXxx1.Visible := True;
    end;
    NewClass1.Visible := true;
    NewAssociation1.Visible := true;
    NewDatatype1.Visible := true;
    NewPackage1.Visible := true;
    N9.Visible := true;
  end
  else if (CurrentElement is TUMLClass) and (ActiveGrid = tvClasses)  then
  begin
    DeleteXxx1.Caption := 'Delete Class ' + CurrentElement.Name;
    DeleteXxx1.Visible := True;
    N10.Visible := True;
    NewClass1.Visible := true;
    InsertSuperclass1.Visible := True;
    AddSubclass1.Visible := true;
//    NewAttribute1.Visible := True;
//    NewOperation1.Visible := True;
    N9.Visible := true;
//    NewAssociation1.Visible := True;
    mnuOverrideFrameworkMethods.Visible := True;
    mnuOverrideModelMethods.Visible := True;
    CreateFrameworkMethodItems(CurrentElement as TUMLClass);
    CreateModelMethodItems(CurrentElement as TUMLClass);
  end
  else if (CurrentElement is TUMLAttribute) or (ActiveGrid = tvAttributes) then
  begin
    if (CurrentElement is TUMLAttribute) then
    begin
      DeleteXxx1.Caption := 'Delete Attribute ' + CurrentElement.Name;
      DeleteXxx1.Visible := True;
    end;
    NewAttribute1.Visible := True;
  end
  else if (CurrentElement is TUMLOperation) or (ActiveGrid = tvOperations) then
  begin
    if (CurrentElement is TUMLOperation) then
    begin
      DeleteXxx1.Caption := 'Delete Operation ' + CurrentElement.Name;
      DeleteXxx1.Visible := True;
    end;
    N10.Visible := True;
//    NewAttribute1.Visible := True;
//    AddSubclass1.Visible := true;
    NewOperation1.Visible := True;
//    NewParameter1.Visible := True;
//    NewClass1.Visible := true;
    mnuOverrideFrameworkMethods.Visible := True;
    mnuOverrideModelMethods.Visible := True;
    CurrentClass := dmBoldUMLModelEditorHandles.blhModelClasses.CurrentBoldObject as TUMLClass;
{    if CurrentElement is TUMLClass then
      CurrentClass := CurrentElement as TUMLClass
    else if CurrentElement is TUMLOperation then
      CurrentClass := (CurrentElement as TUMLOperation).Owner as TUMLClass;}
    if Assigned(CurrentClass) then
    begin
      CreateFrameworkMethodItems(CurrentClass);
      CreateModelMethodItems(CurrentClass);
    end;
    if (CurrentElement is TUMLOperation) and ((CurrentElement as TUMLOperation).Owner.Subclasses.Count > 0) and
       ((CurrentElement.GetBoldTV(TAG_DELPHIOPERATIONKIND) = TV_DELPHIOPERATIONKIND_VIRTUAL) or
       (CurrentElement.GetBoldTV(TAG_DELPHIOPERATIONKIND) = TV_DELPHIOPERATIONKIND_DYNAMIC) or
       (CurrentElement.GetBoldTV(TAG_DELPHIOPERATIONKIND) = TV_DELPHIOPERATIONKIND_ABSTRACTVIRTUAL)) then
      mnuOverrideInAllSubclasses.Visible := True
    else
      mnuOverrideInAllSubclasses.Visible := False;
  end
  else if (CurrentElement is TUMLAssociation) or (ActiveGrid = tvClassAssociations) then
  begin
    if (CurrentElement is TUMLAssociationEnd) then
    begin
      DeleteXxx1.Caption := 'Delete Association ' + (CurrentElement as TUMLAssociationEnd).association.name;
      DeleteXxx1.Visible := True;
    end
    else
    if (CurrentElement is TUMLAssociation) then
    begin
      DeleteXxx1.Caption := 'Delete Association ' + CurrentElement.Name;
      DeleteXxx1.Visible := True;
    end
    else
      DeleteXxx1.Visible := false;
    NewAssociation1.Visible := true;
  end
  else if (CurrentElement is TUMLAssociationEnd) or (ActiveGrid = tvAssociationEnds) then
  begin
//    NewQualifier1.Visible := True;
    OpenOtherEnd.Visible := (CurrentElement is TUMLAssociationEnd);
    NewAssociation1.Visible := true;
  end
  else if CurrentElement is TUMLParameter then
  begin
    DeleteXxx1.Caption := 'Delete Parameter ' + CurrentElement.Name;
    DeleteXxx1.Visible := True;
  end;
  DeleteXxx2.Caption := DeleteXxx1.Caption;
  DeleteXxx2.Visible := DeleteXxx1.Visible;
  N11.Visible := N10.Visible;
  NewClass2.Visible := NewClass1.Visible;
  InsertSuperclass2.Visible := InsertSuperClass1.Visible;
  AddSubclass2.Visible := AddSubclass1.Visible;
  NewAssociation2.Visible := NewAssociation1.Visible;
  NewAttribute2.Visible := NewAttribute1.Visible;
  NewOperation2.Visible := NewOperation1.Visible;
//  NewParameter2.Visible := NewParameter1.Visible;
//  NewQualifier2.Visible := NewQualifier1.Visible;
  NewDataType2.Visible :=  NewDataType1.Visible;

  if pgClient.ActivePage = tabAssociations then
    OpenOtherEnd.Visible := false;

  // Disable the menu shortcuts that are not visible
  for i := 0 to ComponentCount-1 do
    if Components[i] is TMenuItem then
      (Components[i] as TMenuItem).Enabled := (Components[i] as TMenuItem).visible
end;

procedure TBoldModelEditFrmCx.DeleteXxx1Click(Sender: TObject);
var
  QueryRes: TBoldQueryResult;
begin
  if MessageDlg('Delete ' + CurrentElement.BoldType.ModelName + BoldCRLF +
    format('Do you want to delete %s:%s ?', [CurrentElement.BoldType.ModelName, CurrentElement.expandedExpressionName])
  , mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    if CurrentElement is TUMLClass then
    begin
      if not (CurrentModelIsBoldified and (TBoldUMLBoldify.GetRootClass(CurrentModel) = CurrentElement)) then
      begin
        TBoldUMLSupport.RelinkSpecializations(TUMLClass(CurrentElement));
        CurrentElement.Delete;
      end;
    end
    else if not (CurrentElement is TUMLAssociationEnd) then
      CurrentElement.Delete;
end;

procedure TBoldModelEditFrmCx.NewClass1Click(Sender: TObject);
var
  NewClass: TUMLClass;
  NameSpace: TUMLNameSpace;
//  ClassEditForm: TClassEditForm;
begin
  NameSpace := GetCurrentPackage;
  NewClass := TUMLClass.Create(NameSpace.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewClass);
  NewClass.namespace_ := NameSpace;
  NewClass.name := TBoldUMLSupport.UniqueName(NewClass, 'NewClass');
  if CurrentModelIsBoldified then
    NewClass.SetFirstParent(TBoldUMLBoldify.GetRootClass(CurrentModel));
  NewClass.persistent := true;
  pgClient.ActivePage := tsClass;
  pcLeft.ActivePage := tsClasses;
  dmBoldUMLModelEditorHandles.blhModelClasses.CurrentElement := NewClass;
{  ClassEditForm := TClassEditForm.Create(nil);
  ClassEditForm.brhClass.Value := NewClass;
  ClassEditForm.ShowModal;}
end;

procedure TBoldModelEditFrmCx.NewAssociation1Click(Sender: TObject);
var
  NewAssociation: TUMLAssociation;
  NewAssociationEnd: TUMLASsociationEnd;
  NameSpace: TUMLNameSpace;
  i: integer;
  BlockName: string;
  AssociationEditForm: TBoldUMLAssociationEditForm;
begin
  BlockName := CurrentModel.BoldSystem.UndoHandlerInterface.SetCheckPoint('New association');
  NameSpace := GetCurrentPackage;
  NewAssociation := TUMLAssociation.Create(NameSpace.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewAssociation);
  NewAssociation.namespace_ := NameSpace;
  NewAssociation.name := TBoldUMLSupport.UniqueName(NewAssociation,'NewAssociation');
  NewAssociation.persistent := true;

  for i := 0 to 1 do
  begin
    NewAssociationEnd := TUMLAssociationEnd.Create(NameSpace.BoldSystem);
    TBoldUMLSupport.EnsureBoldTaggedValues(NewAssociationEnd);
    with NewAssociationEnd do
    begin
      association := NewAssociation;
      isNavigable := true;
      name := Format('AssociationEnd%d', [i]);
    end;
  end;
  NewAssociationEnd.type_ := GetCurrentClass;
  dmBoldUMLModelEditorHandles.blhClassAssociations.CurrentElement := NewAssociation;

  AssociationEditForm := TBoldUMLAssociationEditForm.Create(nil);
  AssociationEditForm.brhAssociation.Value := NewAssociation;
  AssociationEditForm.ShowModal;
  if AssociationEditForm.ModalResult = mrCancel then
  begin
    CurrentModel.BoldSystem.UndoHandlerInterface.UndoBlock(BlockName);
    if CurrentModel.BoldSystem.UndoHandlerInterface.RedoList.IndexOf(BlockName) <> -1 then
      CurrentModel.BoldSystem.UndoHandlerInterface.RedoList.RemoveBlock(BlockName);
  end;
end;

procedure TBoldModelEditFrmCx.NewAttribute1Click(Sender: TObject);
var
  NewAttribute: TUMLAttribute;
  Class_: TUMLClass;
  AttributeEditForm: TBoldUMLAttributeEditForm;
  BlockName: string;
begin
  BlockName := CurrentModel.BoldSystem.UndoHandlerInterface.SetCheckPoint('New attribute');
  Class_ := GetCurrentClass;
  NewAttribute := TUMLAttribute.Create(Class_.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewAttribute);
  NewAttribute.owner := Class_;
  NewAttribute.name := TBoldUMLSupport.UniqueName(NewAttribute, 'NewAttribute');
  NewAttribute.typeName := 'String';
  NewAttribute.persistent := Class_.persistent;
  dmBoldUMLModelEditorHandles.blhClassAttributes.CurrentElement := NewAttribute;
  AttributeEditForm := TBoldUMLAttributeEditForm.Create(nil);
  AttributeEditForm.brhAttribute.Value := NewAttribute;
  AttributeEditForm.ShowModal;
  if AttributeEditForm.ModalResult = mrCancel then
  begin
    CurrentModel.BoldSystem.UndoHandlerInterface.UndoBlock(BlockName);
    CurrentModel.BoldSystem.UndoHandlerInterface.RedoList.Clear;
  end;
end;

procedure TBoldModelEditFrmCx.NewOperation1Click(Sender: TObject);
var
  NewOperation: TUMLOperation;
  OperationEditForm: TBoldUMLOperationEditForm;
  Class_: TUMLClass;
  BlockName: string;
begin
  Blockname := CurrentModel.BoldSystem.UndoHandlerInterface.SetCheckPoint('New Operation');
  Class_ := GetCurrentClass;
  NewOperation := TUMLOperation.Create(Class_.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewOperation);
  NewOperation.owner := Class_;
  NewOperation.name := TBoldUMLSupport.UniqueName(NewOperation, 'NewOperation');
  dmBoldUMLModelEditorHandles.blhClassOperations.CurrentElement := NewOperation;
  OperationEditForm := TBoldUMLOperationEditForm.Create(nil);
  OperationEditForm.brhOperation.Value := NewOperation;
  OperationEditForm.ShowModal;
  if OperationEditForm.ModalResult = mrCancel then
  begin
    CurrentModel.BoldSystem.UndoHandlerInterface.UndoBlock(BlockName);
    CurrentModel.BoldSystem.UndoHandlerInterface.RedoList.Clear;
  end;
end;

procedure TBoldModelEditFrmCx.NewParameter1Click(Sender: TObject);
var
  NewParameter: TUMLParameter;
  BehavioralFeature: TUMLBehavioralFeature;
begin
  BehavioralFeature := CurrentElement as TUMLBehavioralFeature;
  NewParameter := TUMLParameter.Create(BehavioralFeature.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewParameter);
  NewParameter.behavioralFeature := BehavioralFeature;
  NewParameter.name := TBoldUMLSupport.UniqueName(NewParameter, 'NewParameter');
end;

procedure TBoldModelEditFrmCx.NewQualifier1Click(Sender: TObject);
var
  NewQualifier: TUMLAttribute;
  AssociationEnd: TUMLAssociationEnd;
begin
  AssociationEnd := CurrentElement as TUMLAssociationEnd;
  NewQualifier := TUMLAttribute.Create(AssociationEnd.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewQualifier);
  NewQualifier.AssociationEnd := AssociationEnd;
  NewQualifier.name := TBoldUMLSupport.UniqueName(NewQualifier, 'NewQualifier');
end;

function TBoldModelEditFrmCx.GetCurrentModel: TUMLModel;
begin
  if assigned(dmBoldUMLModelEditorHandles.brhRoot.Value) then
    result := dmBoldUMLModelEditorHandles.brhRoot.Value as TUMLModel
  else
    result := nil;
end;

procedure TBoldModelEditFrmCx.JumpToElement(element: TUMLModelElement);
var
  i: integer;
begin
  if Element is TUMLModel then
  begin
    pcLeft.ActivePage := tsModel;
  end
  else
  if Element is TUMLOperation then
  begin
    dmBoldUMLModelEditorHandles.blhModelClasses.CurrentElement := TUMLOperation (Element).qualifyingOwner;
    dmBoldUMLModelEditorHandles.blhClassOperations.CurrentElement := Element;
    i := tvClasses.DataController.BoldList.IndexOf(TUMLOperation (Element).qualifyingOwner);
    if tvClasses.DataController.FilteredIndexByRecordIndex[i] = -1 then
      ClearClassFilter;
    pgClient.ActivePage := tabClientOperations;
    pcLeft.ActivePage := tsClasses;
  end
  else
  if Element is TUMLClassifier then
  begin
    dmBoldUMLModelEditorHandles.blhModelClasses.CurrentElement := Element;
    i := tvClasses.DataController.BoldList.IndexOf(Element);
    if tvClasses.DataController.FilteredIndexByRecordIndex[i] = -1 then
      ClearClassFilter;
    pcLeft.ActivePage := tsClasses;
    pgClient.ActivePage := tsClass;
  end
  else
  if Element is TUMLAttribute then
  begin
    dmBoldUMLModelEditorHandles.blhModelClasses.CurrentElement := TUMLAttribute(Element).qualifyingOwner;
    dmBoldUMLModelEditorHandles.blhClassAttributes.CurrentElement := Element;
    i := tvClasses.DataController.BoldList.IndexOf(TUMLAttribute(Element).qualifyingOwner);
    if tvClasses.DataController.FilteredIndexByRecordIndex[i] = -1 then
      ClearClassFilter;
    pgClient.ActivePage := tabAttributes;
    pcLeft.ActivePage := tsClasses;
  end
  else
  if Element is TUMLAssociationEnd then
  begin
//      (Dropped as TUMLAssociationEnd).OtherEnd.Type_ := Target;
    if TUMLAssociationEnd(Element).qualifyingOwner is TUMLAssociation then
    begin
      dmBoldUMLModelEditorHandles.blhModelClasses.CurrentElement := TUMLAssociationEnd(Element).type_;
      i := tvClasses.DataController.BoldList.IndexOf(TUMLAssociationEnd(Element).type_);
      if tvClasses.DataController.FilteredIndexByRecordIndex[i] = -1 then
        ClearClassFilter;
    end
    else
    begin
      dmBoldUMLModelEditorHandles.blhModelClasses.CurrentElement := TUMLAssociationEnd(Element).type_;
      Assert(dmBoldUMLModelEditorHandles.blhModelClasses.CurrentElement = TUMLAssociationEnd(Element).type_);
    end;
    dmBoldUMLModelEditorHandles.blhClassAssociationEnds.CurrentElement := TUMLAssociationEnd(Element);
    if (pgClient.ActivePage <> tabAssociations) then
      pgClient.ActivePage := tabAssociationEnds;
    pcLeft.ActivePage := tsClasses;
  end
  else
  if Element is TUMLAssociation then
  begin
    if (pgClient.ActivePage <> tabAssociationEnds) then
      pgClient.ActivePage := tabAssociations;
    pcLeft.ActivePage := tsClasses;
    dmBoldUMLModelEditorHandles.blhModelClasses.CurrentElement := TUMLAssociation(Element).M_Connection[0].type_;
    dmBoldUMLModelEditorHandles.blhClassAssociationEnds.CurrentElement := TUMLAssociation(Element).M_Connection[0];
  end
  else
    Assert(false, 'Unsupported type in JumpToElement: ' + Element.ClassName);
end;


procedure TBoldModelEditFrmCx.AddMenuTool(menuItem: TMenuItem);
begin
  if Assigned(MenuItem) then
    Tools1.Insert(4, menuItem);
end;

procedure TBoldModelEditFrmCx.AddMenuFiler(menuItem: TMenuItem);
begin
  if Assigned(MenuItem) then
  begin
    mnuFile.Insert(5, menuItem);
  end;
end;

procedure TBoldModelEditFrmCx.AddButtonTool(ToolButton: TUMLPlugInToolButton);
begin
  if Assigned(ToolButton) then
  begin
    dxBarManager1Bar4.ItemLinks.BeginUpdate;
    dxBarManager1Bar4.ItemLinks.Add.Item := ToolButton;
    dxBarManager1Bar4.ItemLinks.EndUpdate;
  end;
end;

procedure TBoldModelEditFrmCx.actAddDbExecute(Sender: TObject);
var
  ConnectionName: string;
begin
  DumpHandles(tvClasses.DataController.BoldHandle);
  exit;
  with FireDAC.VCLUI.ConnEdit.TfrmFDGUIxFormsConnEdit.Create(self)do
  begin
    if Execute(dmBoldUMLModelPersistence.FDConnection1, 'Select database') then
    begin
      FillConnectionInfo(dmBoldUMLModelPersistence.FDConnection1, true);
      ConnectionName := dmBoldUMLModelPersistence.FDConnection1.ConnectionName;
      if ConnectionName = '' then
        InputQuery('Save connection definition', 'Save connection as', ConnectionName);
      if ConnectionName <> '' then
      FDManager.AddConnectionDef(ConnectionName,
                                 dmBoldUMLModelPersistence.FDConnection1.DriverName,
                                 dmBoldUMLModelPersistence.FDConnection1.ResultConnectionDef.Params,
                                 true);
      FDManager.SaveConnectionDefFile;
    end;
    free;
  end;
end;

procedure TBoldModelEditFrmCx.actConsistencyCheckExecute(Sender: TObject);
begin
  ApplyGUI;
  CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := false;
  try
    EnsureFlattenedAndBoldified;
    fValidationForm.Validate;
  finally
    CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := true;
  end;
end;

procedure TBoldModelEditFrmCx.actEvaluateOCLExecute(Sender: TObject);
begin
  BoldUMLOclEditor_.EditOcl(ModelHandle, CurrentElement, '');
end;

procedure TBoldModelEditFrmCx.actSelectSuperClassExecute(Sender: TObject);
var
  UMLClass: TUMLClass;
  i: integer;
begin
  UMLClass := TUMLClass(dmBoldUMLModelEditorHandles.blhModelClasses.CurrentElement);
  if Assigned(UMLClass.superclass) then
  begin
    i := tvClasses.DataController.BoldList.IndexOf(UMLClass.superclass);
    if tvClasses.DataController.FilteredIndexByRecordIndex[i] = -1 then
      ClearClassFilter;
    dmBoldUMLModelEditorHandles.blhModelClasses.CurrentElement := UMLClass.superclass;
  end;
end;

procedure TBoldModelEditFrmCx.actSelectSuperClassUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(dmBoldUMLModelEditorHandles.blhModelClasses.CurrentElement) and
    Assigned(TUMLClass(dmBoldUMLModelEditorHandles.blhModelClasses.CurrentElement).superclass);
end;

procedure TBoldModelEditFrmCx.actTagEditExecute(Sender: TObject);
var
  TVEditor: TfrmBoldUMLTaggedValuesEditorCx;
begin
  TVEditor := TfrmBoldUMLTaggedValuesEditorCx.Create(self);
  TVEditor.behRoot.RootHandle := dmBoldUMLModelEditorHandles.brhCurrentElement;
  TVEditor.Show;
end;

procedure TBoldModelEditFrmCx.actTagEditUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(dmBoldUMLModelEditorHandles.brhCurrentElement.Value);
end;

procedure TBoldModelEditFrmCx.AddButtonFiler(ToolButton: TUMLPlugInToolButton);
begin

end;

function TBoldModelEditFrmCx.GetPlugInList: TList;
begin
  result := FPlugInList;
end;

procedure TBoldModelEditFrmCx.SetPlugInList(const Value: TList);
var
  i: Integer;
begin
  FPlugInList := Value;
  for i := 0 to Value.Count - 1 do
    InstallPlugIn(TObject(Value.Items[i]) as TUMLPlugIn);
end;

procedure TBoldModelEditFrmCx.InstallPlugIn(PlugIn: TUMLPlugIn);
begin
  dxBarRegisterItem(TUMLPlugInToolButton, TdxBarButtonControl, true);
  if PlugIn.PlugInType = ptTool then
    AddTool(PlugIn)
  else
    AddFiler(PlugIn);
end;

procedure TBoldModelEditFrmCx.AddTool(PlugIn: TUMLPlugIn);
begin
  AddMenuTool(NewPlugInMenuItem(PlugIn));
  AddButtonTool(NewPlugInToolButton(PlugIn));
end;

procedure TBoldModelEditFrmCx.AddFiler(PlugIn: TUMLPlugIn);
begin
  AddMenuFiler(NewPlugInMenuItem(PlugIn));
  AddButtonFiler(NewPlugInToolButton(PlugIn));
end;

function TBoldModelEditFrmCx.NewPlugInMenuItem(PlugIn: TUMLPlugIn): TUMLPlugInMenuItem;
begin
  if PlugIn.MenuItemName = '' then
    Result := nil
  else
  begin
    Result            := TUMLPlugInMenuItem.Create(self);
    Result.Caption    := PlugIn.MenuItemName;
    Result.PlugIn     := PlugIn;
    Result.OnClick    := PlugInMenuClick;
    Result.ImageIndex := ilMenu.Count;  // not -1 since the bitmap does not exist until NewPlugInToolButton is executed /kala
  end;
end;

function TBoldModelEditFrmCx.NewPlugInToolButton(PlugIn: TUMLPlugIn): TUMLPlugInToolButton;
begin
  if PlugIn.MenuItemName = '' then
    Result := nil
  else
  begin
    result := TUMLPlugInToolButton.Create(self);
    if not ilMenu.ResInstLoad(PlugIn.Instance, rtBitmap, PlugIn.ImageResourceName, PlugIn.ImageMaskColor) then
      if not ilMenu.ResourceLoad(rtBitmap, PlugIn.ImageResourceName, PlugIn.ImageMaskColor) then
        ShowMessage('Unable to locate bitmap resource ' + PlugIn.ImageResourceName);
    result.ImageIndex := ilMenu.Count - 1;
    result.OnClick    := PlugInButtonClick;
    result.PlugIn     := PlugIn;
    result.Hint       := PlugIn.MenuItemName;
  end;
end;

procedure TBoldModelEditFrmCx.ExecPlugIn(PlugIn: TUMLPlugin);
var
  CursorGuard: IBoldCursorGuard;
begin
  ApplyGUI;
  CursorGuard := TBoldCursorGuard.Create;
  CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := false;
  try
    if poRequireBoldified in PlugIn.Options then
      EnsureFlattenedAndBoldified;

    if poRequireUnBoldified in PlugIn.Options then
      EnsureUnFlattenedAndUnBoldified;
    PlugIn.GuardedExecute(Self);
  finally
    CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := true;
  end;
end;

procedure TBoldModelEditFrmCx.PlugInMenuClick(sender: TObject);
var PlugIn: TUMLPlugIn;
begin
  if (Sender is TUMLPlugInMenuItem) then
  begin
    PlugIn := TUMLPlugInMenuItem(Sender).PlugIn;
    ExecPlugin(PlugIn);
  end;
end;

procedure TBoldModelEditFrmCx.PlugInButtonClick(sender: TObject);
var PlugIn: TUMLPlugIn;
begin
  if (Sender is TUMLPlugInToolButton) then
  begin
    PlugIn := TUMLPlugInToolButton(Sender).PlugIn;
    ExecPlugin(PlugIn);
  end;
end;

function TBoldModelEditFrmCx.GetCurrentElement: TUMLModelElement;
begin
  result := dmBoldUMLModelEditorHandles.brhCurrentElement.Value as TUMLModelElement;
  if not Assigned(Result) then
  begin
    result := dmBoldUMLModelEditorHandles.blhModelClasses.Value as TUMLModelElement;
    SetCurrentElement(Result);
  end;
end;

procedure TBoldModelEditFrmCx.Paste1Click(Sender: TObject);
begin
  If Assigned(ActiveControl) then
    ActiveControl.Perform(WM_PASTE, 0, 0);
end;

procedure TBoldModelEditFrmCx.FormCreate(Sender: TObject);
var
  Index: Integer;
begin
//  dmBoldUMLModelEditorHandles := TdmBoldUMLModelEditorHandles.Create(self);
  fStartup := Now;
  pcLeft.TabIndex := 0;
  pgClient.ActivePage := tsClass;
  SetCheckBoxHints;
  popTree.AutoHotkeys := maManual;
  CutOrCopy := cckNone;
  fModelChangedSubscriber := TBoldPassthroughSubscriber.Create(ModelChangedRecieve);
  FShowAttribKindFeaturesSubscriber := TBoldPassthroughSubscriber.Create(ShowAttribKindFeaturesRecieve);

  //PopulateComboBoxes;
  fLogframe := TBoldLogFrame.create(self);
  fLogFrame.Parent := self;

  fUndoForm := TfrmBoldUndo.Create(self);
  fUndoForm.Parent := pnlBottom;
  fUndoForm.HideEmptyBlocks := true;
  fUndoForm.Align := alRight;

  FDManager.ConnectionDefFileName := ExtractFilePath(Application.ExeName) + Application.Name + 'dbdef.ini';
  FDManager.ConnectionDefFileAutoLoad := True;
  FDManager.GetConnectionNames(dxComboDatabase.Items);

  dxComboDatabase.ItemIndex := 0;
  fSelectionHistory := TBoldObjectList.Create;
  fSelectionHistory.DuplicateMode := bldmAllow;
  If not BoldInternalRunningInIDE then
  begin
    fCodeGenerator  := TStandAloneCodeGenerator.Create(true);
  end;
end;

procedure TBoldModelEditFrmCx.EnsureTypesFromTypeNameHandle(Sender: TObject);
begin
  IgnoreModelChanges := True;
  ModelHandle.EnsureTypes;
{  if cmbAttributeBoldType.Items.IndexOf(cmbAttributeBoldType.Text) = -1 then
    cmbAttributeBoldType.Text := '';}
  IgnoreModelChanges := False;
end;

procedure TBoldModelEditFrmCx.GetCopyPasteRef;
begin
  if UMLObjectToCopyOrMove <> TUMLModelElement(CurrentElement) then
    UMLObjectToCopyOrMove := TUMLModelElement(CurrentElement);
end;

procedure TBoldModelEditFrmCx.Copy1Click(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    ActiveControl.Perform(WM_COPY, 0, 0);
end;

procedure TBoldModelEditFrmCx.Cut1Click(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    ActiveControl.Perform(WM_CUT, 0, 0);
end;

procedure TBoldModelEditFrmCx.cxCheckBox3Click(Sender: TObject);
begin
  tvAssociationEnds.Preview.Visible := (Sender as TcxCheckBox).Checked;
end;

procedure TBoldModelEditFrmCx.cxCheckBox4Click(Sender: TObject);
begin
  if (Sender as TcxCheckBox).Checked then
  begin
    dmBoldUMLModelEditorHandles.behClassAssociationEnds.Expression := 'superclasses.associationEnd->union(associationEnd)'
  end
  else
  begin
    dmBoldUMLModelEditorHandles.behClassAssociationEnds.Expression := 'associationEnd';
  end;
end;

procedure TBoldModelEditFrmCx.cxCheckBox5Click(Sender: TObject);
begin
  tvClassAssociations.Preview.Visible := (Sender as TcxCheckBox).Checked;
end;

procedure TBoldModelEditFrmCx.cxCheckBox6Click(Sender: TObject);
begin
  if (Sender as TcxCheckBox).Checked then
  begin
    tvOperationsqualifyingOwner.Visible := true;
    dmBoldUMLModelEditorHandles.behClassOperations.Expression := 'allfeature->select(oclIsTypeOf(UMLOperation))'
  end
  else
  begin
    tvOperationsqualifyingOwner.Visible := false;
    dmBoldUMLModelEditorHandles.behClassOperations.Expression := 'feature->select(oclIsTypeOf(UMLOperation))';
  end;
end;

procedure TBoldModelEditFrmCx.cxCheckBox7Click(Sender: TObject);
begin
  if (Sender as TcxCheckBox).Checked then
    dmBoldUMLModelEditorHandles.behClassAssociations.Expression := 'superclasses.associationEnd->union(superclasses.associationEnd.otherEnd)->union(associationEnd->union(associationEnd.otherEnd))'
  else
    dmBoldUMLModelEditorHandles.behClassAssociations.Expression := 'associationEnd->union(associationEnd.otherEnd)';
end;

procedure TBoldModelEditFrmCx.cxCheckBox8Click(Sender: TObject);
begin
  if (Sender as TcxCheckBox).Checked then
  begin
    tvOclExpressionsqualifyingOwner.Visible := true;
    dmBoldUMLModelEditorHandles.behOclExpressions.Expression := 'allFeature->select(derived)->union(associations.connection->select(derived))->union(constraint)'
  end
  else
  begin
    tvOclExpressionsqualifyingOwner.Visible := false;
    dmBoldUMLModelEditorHandles.behOclExpressions.Expression := 'feature->select(derived)->union(associations.connection->select(derived))->union(constraint)';
  end;
end;

procedure TBoldModelEditFrmCx.cxCheckBox9Click(Sender: TObject);
begin
  if (Sender as TcxCheckBox).Checked then
    dmBoldUMLModelEditorHandles.behDataTypes.RootHandle := dmBoldUMLModelEditorHandles.behAllDataTypes
  else
    dmBoldUMLModelEditorHandles.behDataTypes.RootHandle := dmBoldUMLModelEditorHandles.bdhTypesForAttribute;
end;

procedure TBoldModelEditFrmCx.cxCheckGroup1PropertiesChange(Sender: TObject);
var
  AItemList: TcxFilterCriteriaItemList;
begin
  tvClasses.BeginFilteringUpdate;
  tvClasses.DataController.Filter.Clear;
  if cxCheckGroup1.States[0] = cbsChecked  then
  tvClasses.DataController.Filter.Root.AddItem(tvClassespersistent, foEqual, True, 'True');
  if cxCheckGroup1.States[1] = cbsChecked  then
    tvClasses.DataController.Filter.Root.AddItem(tvClassespersistent, foNotEqual, True, 'True');
  if cxCheckGroup1.States[2] = cbsChecked  then
    tvClasses.DataController.Filter.Root.AddItem(tvClassesisAbstract, foEqual, True, 'True');
  if cxCheckGroup1.States[3] = cbsChecked  then
  tvClasses.DataController.Filter.Root.AddItem(tvClassesisAssociationClass, foEqual, True, 'True');
  tvClasses.EndFilteringUpdate;
  tvClasses.DataController.Filter.Active := true;
end;

procedure TBoldModelEditFrmCx.tvDataTypesDblClick(Sender: TObject);
var
  AttributeDescriptor: TBoldMemberTypeDescriptor;
  sl: TStringList;
  s: string;
  i: integer;
  ValueSetValueList: TBAValueSetValueList;
  v: Variant;
begin
  v := tvDataTypes.DataController.Values[tvDataTypes.CurrentIndex, tvDataTypesColumn1.Index];
  if VarIsNull(v) then
    exit;
  s := v;
  Attributedescriptor := BoldMemberTypes.DescriptorByDelphiName[s];
  if Assigned(AttributeDescriptor) and Assigned(AttributeDescriptor.MemberClass) then
    if AttributeDescriptor.MemberClass.InheritsFrom(TBAValueSet)  and (Attributedescriptor.AbstractionLevel = alConcrete) then
    begin
      sl := TStringList.Create;
      try
        ValueSetValueList := TBAValueSetClass(AttributeDescriptor.MemberClass).GetValues;
        if Assigned(ValueSetValueList) then
          ValueSetValueList.ToStrings(brDefault, sl);
        if sl.Text <> '' then
          ShowMessage(sl.Text);
      finally
        sl.Free;
      end;
  end;
end;

procedure TBoldModelEditFrmCx.tvOclExpressionsDblClick(Sender: TObject);
var
  Element: TUMLModelElement;
  Context: TUMLModelElement;
begin
  Element := tvOclExpressions.CurrentElement as TUMLModelElement;
  if Element is TUMLConstraint then
    Context := (Element as TUMLConstraint).constrainedElement.First as TUMLModelElement
  else
    Context := (Element as TUMLModelElement).qualifyingOwner;
  EditOclExpression(Element, TAG_DERIVATIONOCL, Context);
end;

procedure TBoldModelEditFrmCx.tvGridDblClick(Sender: TObject);
var
  BlockName: string;
  BO: TUMLModelElement;
  Form: TForm;
begin
  Form := nil;
  if CurrentElement is TUMLAttribute then
  begin
    Form := TBoldUMLAttributeEditForm.Create(self);
    TBoldUMLAttributeEditForm(Form).brhAttribute.Value := CurrentElement;;
  end
  else
  if CurrentElement is TUMLAssociation then
  begin
    Form := TBoldUMLAssociationEditForm.Create(self);
    TBoldUMLAssociationEditForm(Form).brhAssociation.Value := CurrentElement;
  end
  else
  if CurrentElement is TUMLAssociationEnd then
  begin
    Form := TBoldUMLAssociationEditForm.Create(self);
    TBoldUMLAssociationEditForm(Form).brhAssociation.Value := (CurrentElement as TUMLAssociationEnd).association;
    TBoldUMLAssociationEditForm(Form).PageControl1.ActivePageIndex := (CurrentElement as TUMLAssociationEnd).association.M_connection.IndexOf(CurrentElement as TUMLAssociationEnd);
  end
  else
  if CurrentElement is TUMLOperation then
  begin
    Form := TBoldUMLOperationEditForm.Create(self);
    TBoldUMLOperationEditForm(Form).brhOperation.Value := CurrentElement;
  end;
  if Assigned(Form) then
  begin
    BlockName := CurrentModel.BoldSystem.UndoHandlerInterface.SetCheckPoint('Edit ' + CurrentElement.AsString);
    if Form.ShowModal = mrCancel then
    begin
      CurrentModel.BoldSystem.UndoHandlerInterface.UndoBlock(BlockName);
      if CurrentModel.BoldSystem.UndoHandlerInterface.RedoList.IndexOf(BlockName) <> -1 then
        CurrentModel.BoldSystem.UndoHandlerInterface.RedoList.RemoveBlock(BlockName);
    end;
  end;
end;

procedure TBoldModelEditFrmCx.tvAggregationCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
begin
  ADone := false;
  if not AViewInfo.Selected then
  if CompareText(AViewInfo.Value, 'none') = 0 then
    ACanvas.Font.Color := clGray
  else
    ACanvas.Font.Color := clWindowText;
end;

procedure TBoldModelEditFrmCx.tvNameCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
begin
  ADone := false;
  if not AViewInfo.Selected then
  begin
    if CompareText(AViewInfo.Value, '<Name>') = 0 then
      ACanvas.Font.Color := clGray
    else
      ACanvas.Font.Color := clWindowText;
  end;
end;

procedure TBoldModelEditFrmCx.tvDefaultCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
begin
  ADone := false;
  if not AViewInfo.Selected then
  if CompareText(AViewInfo.Value, '<Default>') = 0 then
    ACanvas.Font.Color := clGray
  else
    ACanvas.Font.Color := clWindowText;
end;

procedure TBoldModelEditFrmCx.tvDataControllerAfterLoad(
  Sender: TObject);
var
  View: TcxGridBoldTableView;
  lVisibleCount: integer;
begin
  View := (Sender as TcxGridBoldTableView);
  View.DataController.Groups.FullExpand;
  lVisibleCount := View.ViewInfo.VisibleRecordCount;
  if lVisibleCount <> View.OptionsBehavior.BestFitMaxRecordCount then
    View.OptionsBehavior.BestFitMaxRecordCount := lVisibleCount;
//  View.ApplyBestFit(nil, true, true);
end;

procedure TBoldModelEditFrmCx.tvGridViewKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Ord(Key) = VK_RETURN then
    (Sender as TcxBoldGridSite).GridView.OnDblClick(Sender);
end;

procedure TBoldModelEditFrmCx.PasteClass;
var
  UMLClass: TUMLClass;
begin
  UMLClass := GetElementToPaste as TUMLClass;
  if CurrentElement is TUMLClass then
  begin
    UMLClass.SetFirstParent(TUMLClass(CurrentElement));
    UMLClass.namespace_ := TUMLClass(CurrentElement).namespace_;
  end
  else // if the user pastes with no class as superclass (selected), the models superclass defaults.
  begin
    if CurrentModelIsBoldified then
      UMLClass.SetFirstParent(TBoldUMLBoldify.GetRootClass(CurrentModel));
    if CurrentElement is TUMLPackage then
      UMLClass.namespace_ := TUMLPackage(CurrentElement)
    else
      UMLClass.namespace_ := CurrentModel;
  end;
   UMLClass.Name := TBoldUMLSupport.UniqueName(UMLClass, UMLObjectToCopyOrMove.Name);
end;

procedure TBoldModelEditFrmCx.PasteFeature;
var
  newFeature: TUMLFeature;
begin
  if CurrentElement is TUMLClass then
  begin
    newFeature := GetElementToPaste as TUMLFeature;
    newFeature.owner := TUMLCLass(CurrentElement);
    newFeature.Name := TBoldUMLSupport.UniqueName(newFeature,UMLObjectToCopyOrMove.name);
  end;
end;

procedure TBoldModelEditFrmCx.PasteAssociation;
var
  newAssociation: TUMLAssociation;
  oldAssociation: TUMLAssociation;
  i: integer;
begin
  if CurrentElement is TUMLPackage then
  begin
    newAssociation := GetElementToPaste as TUMLAssociation;
    oldAssociation :=  UMLObjectToCopyOrMove as TUMLAssociation;
    newAssociation.namespace_ := TUMLPackage(CurrentElement);
    newAssociation.Name := TBoldUMLSupport.UniqueName(newAssociation,UMLObjectToCopyOrMove.name);
    for i := 0 to oldAssociation.connection.count - 1 do
      newAssociation.connection[i].name := TBoldUMLSupport.UniqueName(newAssociation.connection[i], oldAssociation.connection[i].name);
  end;
end;

procedure TBoldModelEditFrmCx.PasteAssociationEnd;
var
  oldAssociation: TUMLAssociation;
  newAssociation: TUMLAssociation;
  i: integer;
begin
  if CurrentElement is TUMLClass then
  begin
    newAssociation := nil;
    oldAssociation :=  UMLObjectToCopyOrMove as TUMLAssociation;
    case CutOrCopy of
      cckCopy:
        newAssociation :=  TBoldCopyAndClone.BoldClone(UMLObjectToCopyOrMove, bcmDeep) as TUMLAssociation;
      cckCut:
        newAssociation := oldAssociation;
    end;
    newAssociation.namespace_ := TUMLCLass(CurrentElement).namespace_;
    newAssociation.name := TBoldUMLSupport.UniqueName(newAssociation,oldAssociation.name);
    for i := 0 to oldAssociation.connection.count - 1 do
    begin
      if oldAssociation.connection[i] = UMLObjectToCopyOrMove then
        newAssociation.connection[i].type_ := TUMLClass(CurrentElement);
      newAssociation.connection[i].name := TBoldUMLSupport.UniqueName(newAssociation.connection[i], oldAssociation.connection[i].name);
    end;
  end;
end;

procedure TBoldModelEditFrmCx.PasteParameter;
var
  newParameter: TUMLParameter;
begin
  if CurrentElement is TUMLOperation then
  begin
    newParameter := GetElementToPaste as TUMLParameter;
    newParameter.behavioralFeature := TUMLOperation(CurrentElement);
    newParameter.Name := TBoldUMLSupport.UniqueName(NewParameter, UMLObjectToCopyOrMove.Name);
  end;
end;

procedure TBoldModelEditFrmCx.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(UMLObjectToCopyOrMove) then
  begin
    if Key = VK_ESCAPE then
    begin
      GreyNode.Cut := False;
      UMLObjectToCopyOrMove := nil;
      CutOrCopy := cckNone;
    end;
  end;
end;

procedure TBoldModelEditFrmCx.ModelChangedRecieve(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if GetCurrentModelHandle.IsValidating then
    exit;
  fModelSubscriptionsValid := false;
  if not fIgnoreModelChanges then
  begin
    ModelNeedsValidation := True;
    fModelChangedSubscriber.CancelAllSubscriptions;
  end;
end;

procedure TBoldModelEditFrmCx.FormShow(Sender: TObject);
var
  Index: Integer;
begin
  ShowAttribKindFeaturesRecieve(dmBoldUMLModelEditorHandles.behAttribute, beValueChanged, breResubscribe);
  for Index := 0 to BoldObjectPersistenceMappers.Count - 1 do
    cmbClassPMapperName.Items.Add(BoldObjectPersistenceMappers.Descriptors[Index].Name);

  fValidationForm := EnsureValidationForm(GetCurrentModelHandle, self, JumpToElement);
  fValidationForm.Parent := tsValidation;
  fValidationForm.BorderStyle := bsNone;
  fValidationForm.Align := alClient;
  fValidationForm.ValidationProc := TBoldUMLModelValidatorCallBack.Validate;
  fValidationForm.Show;
end;

procedure TBoldModelEditFrmCx.OpenOtherEndClick(Sender: TObject);
var
  AssociationEnd: TUMLAssociationEnd;
begin
  AssociationEnd := CurrentElement as TUMLAssociationEnd;
  JumpToElement(AssociationEnd.GetOtherEnd);
end;

procedure TBoldModelEditFrmCx.SaveFileAs1Click(Sender: TObject);
var
  UMLLinkClass: TBoldUMLModelLinkClass;
begin
  ApplyGUI;
  if fIsExecutingPlugin then
  begin
    ShowMessage('Already executing a plugin');
    Exit;
  end;

  FileSaveAs1.Dialog.Filter := Copy(BoldUMLModelLinkList.FileFilter, 1, Length(BoldUMLModelLinkList.ExportableFileFilter) - Length(ALLFILESFILTER)) ;
  if FileSaveAs1.Dialog.Execute then
  begin
    SaveModelTofile(FileSaveAs1.Dialog.FileName);
  end;
end;

procedure TBoldModelEditFrmCx.SaveFormsettingsToRegistry;
var
  BoldRegistry: TBoldRegistry;
  BoldGuard: IBoldGuard;
begin
exit;
  BoldGuard := TBoldGuard.Create(BoldRegistry);
  BoldRegistry := TBoldRegistry.Create;
  try
    with BoldRegistry do
    begin
      OpenKey('\UMLModel');
      WriteInteger('Width', Width);
      WriteInteger('Height', Height);
      WriteInteger('Left', Left);
      WriteInteger('Top', Top);
    end;
  except
  end;
end;

procedure TBoldModelEditFrmCx.SaveModelToFile(const AFileName: string);
var
  UMLLinkClass: TBoldUMLModelLinkClass;
  CursorGuard: IBoldCursorGuard;
begin
  if fIsExecutingPlugin then
  begin
    ShowMessage('Already executing a plugin');
    Exit;
  end;
  fIsExecutingPlugin := true;
  CursorGuard := TBoldCursorGuard.Create;
  try
    fIsExecutingPlugin := true;
    CursorGuard := TBoldCursorGuard.Create;
    try
      UMLLinkClass := BoldUMLModelLinkList.LinkClasses[-1, ExtractFileExt(AFileName)];
      if Assigned(UMLLinkClass) then
      begin
        with UMLLinkClass.Create(nil) do
        try
          FileName := AFileName;
          ExportModel(CurrentModel);
        finally
          Free;
        end;
      end;
    finally
      fIsExecutingPlugin := false;
    end;
  finally
    fIsExecutingPlugin := false;
    CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := true;
  end;
end;

procedure TBoldModelEditFrmCx.LoadFormsettingsFromRegistry;
var
  BoldRegistry: TBoldRegistry;
  BoldGuard: IBoldGuard;
begin
exit;
  BoldGuard := TBoldGuard.Create(BoldRegistry);
  try
    BoldRegistry := TBoldRegistry.Create;
    with BoldRegistry do
    begin
      OpenKey('\UMLModel');
      Width := ReadInteger('Width', 800);
      Height := ReadInteger('Height', 600);
      Left := ReadInteger('Left', 0);
      Top := ReadInteger('Top', 0);
    end;
  except
  end;
end;

procedure TBoldModelEditFrmCx.Exit1Click(Sender: TObject);
begin
  ApplyGUI;
  Close;
end;

function TBoldModelEditFrmCx.CloseThisForm: TCloseAction;
begin
  ApplyGUI;
  Result := caHide;
end;

function TBoldModelEditFrmCx.DropOnUMLClass(ToBeDropped: TUMLModelElement): Boolean;
begin
end;

function TBoldModelEditFrmCx.DropOnUMLAssociation(ToBeDropped: TUMLModelElement): Boolean;
begin
end;

function TBoldModelEditFrmCx.DropOnUMLAssociationEnd(ToBeDropped: TUMLModelElement): Boolean;
begin
end;

function TBoldModelEditFrmCx.DropOnUMLAttribute(ToBeDropped: TUMLModelElement): Boolean;
begin
end;

function TBoldModelEditFrmCx.DropOnUMLPackage(ToBeDropped: TUMLModelElement): Boolean;
begin
end;

function TBoldModelEditFrmCx.DropOnUMLOperation(ToBeDropped: TUMLModelElement): Boolean;
begin
end;

function TBoldModelEditFrmCx.DropOnUMLParameter(ToBeDropped: TUMLModelElement): Boolean;
begin
end;

procedure TBoldModelEditFrmCx.DroppedOnUMLParameter(Dropped: TUMLModelElement; Target: TUMLParameter);
begin
end;

function TBoldModelEditFrmCx.DumpHandles(AHandle: TBoldElementHandle): string;
var
  st: string;
begin
  result := 'ModelHandle:' + FModelHandle.Name + '='+FModelHandle.UMLModel.AsString + ';SystemHandle='+ FModelHandle.SystemHandle.Name+BoldCRLF;
  st := '';
  repeat
    if (aHandle is TBoldNonSystemHandle) then
    begin
      if Assigned(TBoldNonSystemHandle(AHandle).StaticSystemHandle) then
      begin
        if Assigned(TBoldNonSystemHandle(AHandle).StaticSystemHandle.Value) then
          st := TBoldNonSystemHandle(AHandle).StaticSystemHandle.Name + TBoldNonSystemHandle(AHandle).StaticSystemHandle.Value.AsString
        else
          st := TBoldNonSystemHandle(AHandle).StaticSystemHandle.Name + ' value=nil';
      end
      else
        st := 'StaticSystemHandle=nil';
    end;
    if Assigned(aHandle.Value) then
      result := result + AHandle.Name + ':' + aHandle.Value.AsString + ':' + st + BoldCRLF
    else
      result := result + AHandle.Name + ':nil'  + ':' + st + BoldCRLF;
    if AHandle is TBoldRootedHandle then
      AHandle := TBoldRootedHandle(AHandle).RootHandle
    else
      AHandle := nil;
  until AHandle = nil;
  ShowMessage(result);
//  dmBoldUMLModelEditorHandles.brhRoot.Value := ModelHandle.SystemHandle.System.ClassByExpressionNa//me['UMLModel'].first;
  tvClasses.DataController.BoldHandle := dmBoldUMLModelEditorHandles.blhModelClasses;
  BoldPropertyMapper1.MappingCollection[7].BoldHandle := dmBoldUMLModelEditorHandles.behDataTypes;
//  ShowMessage(ModelHandle.SystemHandle.System.ClassByExpressionName['UMLModel'].first.AsString);
//  ModelHandle.SystemHandle.Active := true;

end;

procedure TBoldModelEditFrmCx.dxComboDatabaseChange(Sender: TObject);
begin
  if Assigned(dmBoldUMLModelPersistence) then
    dmBoldUMLModelPersistence.FDConnection1.ConnectionDefName := dxComboDatabase.Text;
end;

procedure TBoldModelEditFrmCx.DroppedOnUMLClass(Dropped: TUMLModelElement; Target: TUMLClass);
begin
end;

procedure TBoldModelEditFrmCx.DroppedOnUMLOperation(Dropped: TUMLParameter; Target: TUMLOperation);
begin
end;

procedure TBoldModelEditFrmCx.GetParamNames(Method: String; ResultList: TStringList);
var StartPos, EndPos, FinalPos: Integer;
    NameWithKind: String;
begin
  EndPos := 0;
  FinalPos := Pos(')', Method);
  while(Pos(':', Method) > 0) and (Pos(':', Method) < FinalPos) do
  begin
    if Pos('(', Method) > 0 then
    begin
      StartPos := Pos('(', Method) + 1;
      Method[Pos('(', Method)] := 'X';
    end
    else
    begin
      StartPos := Pos(';', Method) + 2;
      Method[Pos(';', Method)] := 'X';
    end;

    if Pos(':', Method) > 0 then
    begin
      EndPos := Pos(':', Method) - 1;
      Method[Pos(':', Method)] := 'X';  //Replace the ':' with a 'X' to avoid finding it again...
    end;
    NameWithKind := Copy(Method, StartPos, EndPos - StartPos + 1);
    ResultList.Add(Trim(NameWithKind));
  end;
end;

procedure TBoldModelEditFrmCx.GetParamTypes(Method: String; ResultList: TStringList);
var StartPos, EndPos, FinalPos: Integer;
begin
  FinalPos := Pos(')', Method);
  while(Pos(':', Method) > 0) and (Pos(':', Method) < FinalPos) do
  begin
    StartPos := Pos(':', Method) + 2;
    Method[Pos(':', Method)] := 'X';

    if (Pos(';', Method) > 0) and (Pos(';', Method) < FinalPos) then
    begin
      EndPos := Pos(';', Method) - 1;
      Method[Pos(';', Method)] := 'X';  //Replace the ':' with a 'X' to avoid finding it again...
    end
    else
    begin
      EndPos := FinalPos - 1;
    end;
    ResultList.Add(Trim(Copy(Method, StartPos, EndPos - StartPos + 1)));
  end;
end;

function TBoldModelEditFrmCx.GetReturnType(Method: String): String;
var
  StartPos, EndPos: Integer;
  TempString: String;
begin
  for StartPos := Length(Method) downto 1 do
    if Method[StartPos] = ':' then Break;
  if StartPos = 0 then
    TempString := ''
  else
    TempString := Trim(Copy(Method, StartPos + 1, Length(Method) - StartPos));
  if Pos(')', TempString) <> 0 then
    Result := ''
  else
  begin
    EndPos := Pos(';', TempString) - 1;
    Result := Trim(Copy(TempString, 1, EndPos));
  end;
end;

procedure TBoldModelEditFrmCx.ClearMenuItems(var MenuItem: TMenuItem);
begin
  while MenuItem.Count > 0 do
    MenuItem.Remove(MenuItem.Items[0]);
end;

constructor TBoldModelEditFrmCx.Create(AOnwer: TComponent);
begin
//  fdmModelEdit := TdmModelEdit.Create(self);
//  dmBoldUMLModelEditorHandles := TdmBoldUMLModelEditorHandles.Create(self);
  if (csDesigning in ComponentState) then
    RegisterFindGlobalComponentProc(FindGlobalComponent);
  inherited;
  if (csDesigning in ComponentState) then
    InitInheritedComponent(Self, TForm);
end;

procedure TBoldModelEditFrmCx.CreateFrameworkMethodItems(UMLClass: TUMLClass);
var
  Index: Integer;
  MenuItem: TMenuItem;
  TypeList: TStringList;
begin
  if not Assigned(FrameworkMethodMenuItems) then
    FrameworkMethodMenuItems := TList.Create
  else
    FreeRefsInList(FrameworkMethodMenuItems);
  FrameworkMethodMenuItems.Clear;
  for Index := 0 to Length(FrameworkMethods) - 1 do
  begin
    TypeList := TStringList.Create;
    GetParamTypes(FrameworkMethods[Index], TypeList);
    MenuItem := TMenuItem.Create(popTree);
    MenuItem.Caption := TBoldUMLModelValidator.GetMethodName(FrameworkMethods[Index]);
    MenuItem.OnClick := OnOverrideMenuItemClick;
    FrameworkMethodMenuItems.Add(MenuItem);
    if TBoldUMLOperationSupport.ClassHasOperation(UMLClass, TBoldUMLModelValidator.GetMethodName(FrameworkMethods[Index]), TypeList) then
    begin
      MenuItem.Enabled := false;
      MenuItem.Checked := true;
    end;
    TypeList.Free;
  end;
  with mnuOverrideFrameworkMethods do
  begin
    ClearMenuItems(mnuOverrideFrameworkMethods);
    for Index := 0 to FrameworkMethodMenuItems.Count - 1 do
    begin
      Add(TMenuItem(FrameworkMethodMenuItems.Items[Index]));
    end;
  end;
end;

procedure TBoldModelEditFrmCx.FreeRefsInList(List: TList);
var
  Index: Integer;
begin
  for Index := 0 to List.Count - 1 do
    TMenuItem(List.Items[Index]).Free;
end;

procedure TBoldModelEditFrmCx.FreeItemsInMenu(InMenu: TMenuItem);
begin
  while InMenu.Count > 0 do
    InMenu[0].Free;
end;

procedure TBoldModelEditFrmCx.CreateModelMethodItems(UMLClass: TUMLClass);
var
  Index: Integer;
  TypeList: TStringList;
  MenuItem: TMenuItem;
begin
  if not Assigned(ModelMethods) then
    ModelMethods := TList.Create;

  FreeItemsInMenu(mnuOverrideModelMethods);
  ClearMenuItems(mnuOverrideModelMethods);
  ModelMethods.Clear;
  UMLClass.GetAllOverrideableMethods(ModelMethods);

  TypeList := TStringList.Create;

  try
    for Index := 0 to ModelMethods.Count - 1 do
    begin
      TypeList.Clear;
      TBoldUMLOperationSupport.ParameterTypesToList(TUMLOperation(ModelMethods[Index]),TypeList);
      MenuItem := TMenuItem.Create(popTree);
      MenuItem.Caption := TUMLOperation(ModelMethods[Index]).Name;
      MenuItem.OnClick := OnOverrideMenuItemClick;
      mnuOverrideModelMethods.Add(MenuItem);
      if TBoldUMLOperationSupport.ClassHasOperation(UMLClass, TUMLOperation(ModelMethods[Index]).Name, TypeList) then
      begin
        MenuItem.Enabled := false;
        MenuItem.Checked := true;
      end;
    end;
  finally
    TypeList.Free;
  end;
end;

procedure TBoldModelEditFrmCx.OverrideFrameworkMethod(MethodName: String);
var Method, ReturnType: String;
    NewMethod: TUMLOperation;
    ParamNames, ParamTypes: TStringList;
    Class_: TUMLClass;
begin
  Class_ := GetCurrentClass;
  Method := FindFrameworkMethod(MethodName);
  if Method <> '' then
  begin
    NewMethod := TUMLOperation.Create(Class_.BoldSystem);
    TBoldUMLSupport.EnsureBoldTaggedValues(NewMethod);
    NewMethod.name := TBoldUMLModelValidator.GetMethodName(Method);
    NewMethod.owner := Class_;
    NewMethod.SetBoldTV(TAG_DELPHIOPERATIONKIND, TV_DELPHIOPERATIONKIND_OVERRIDE);
    ParamNames := TStringList.Create;
    ParamTypes := TStringList.Create;
    GetParamNames(Method, ParamNames);
    GetParamTypes(Method, ParamTypes);
    ReturnType := GetReturnType(Method);
    CreateParamsFromStringList(ParamNames, ParamTypes, NewMethod, ReturnType);
    ParamNames.Free;
    ParamTypes.Free;
    JumpToElement(NewMethod);
  end
  else
    MessageDlg('Error: Could not find operation!', mtError, [mbOK], 0);
end;

procedure TBoldModelEditFrmCx.CreateParamsFromStringList(ParamNames, ParamTypes: TStringList; NewOperation: TUMLOperation; ReturnType: String);
var
  Index: Integer;
  CurrentParamName: String;
  UMLParameter: TUMLParameter;
begin
  for Index := 0 to ParamNames.Count - 1 do
  begin
    CurrentParamName := ParamNames[Index];
    UMLParameter := TUMLParameter.Create(NewOperation.BoldSystem);
    TBoldUMLSupport.EnsureBoldTaggedValues(UMLParameter);
    UMLParameter.behavioralFeature := NewOperation;
    if Pos('const ', CurrentParamName) > 0 then
    begin
      UMLParameter.SetBoldTV(TAG_ISCONST, TV_TRUE);
      Delete(CurrentParamName, Pos('const ', CurrentParamName), Length('const '));
    end
    else
      UMLParameter.SetBoldTV(TAG_ISCONST, TV_FALSE);
    if Pos('var ', CurrentParamName) > 0 then
    begin
      UMLParameter.kind := pdInOut;
      Delete(CurrentParamName, Pos('var ', CurrentParamName), Length('var '));
    end
    else
      UMLParameter.kind := pdIn;

    UMLParameter.Name := CurrentParamName;
    UMLParameter.typeName := ParamTypes[Index];
  end;
  if ReturnType <> '' then
  begin
    UMLParameter := TUMLParameter.Create(NewOperation.BoldSystem);
    TBoldUMLSupport.EnsureBoldTaggedValues(UMLParameter);
    UMLParameter.behavioralFeature := NewOperation;
    UMLParameter.name := ('return');
    UMLParameter.typeName := ReturnType;
    UMLParameter.kind := pdReturn;
  end;
end;

procedure TBoldModelEditFrmCx.OverrideModelMethod(MethodName: String);
var
  OldOperation, NewOperation: TUMLOperation;
  OldParameter, NewParameter: TUMLParameter;
  Index: Integer;
  Class_: TUMLClass;
begin
  OldOperation := FindModelMethod(MethodName);
  if Assigned(OldOperation) then
  begin
    Class_ := dmBoldUMLModelEditorHandles.blhModelClasses.CurrentBoldObject as TUMLClass;
    Assert(OldOperation.owner <> Class_);
    NewOperation := TUMLOperation.Create(Class_.BoldSystem);
    TBoldUMLSupport.EnsureBoldTaggedValues(NewOperation);
    NewOperation.owner := Class_;
    NewOperation.name := OldOperation.Name;
    NewOperation.visibility := OldOperation.Visibility;
    NewOperation.SetBoldTV(TAG_DELPHIOPERATIONKIND, TV_DELPHIOPERATIONKIND_OVERRIDE);
    for Index := 0 to OldOperation.Parameter.Count - 1 do
    begin
      OldParameter := OldOperation.Parameter[Index];
      NewParameter := TUMLParameter.Create(NewOperation.BoldSystem);
      TBoldUMLSupport.EnsureBoldTaggedValues(NewOperation);
      with NewParameter do
      begin
        behavioralFeature := NewOperation;
        name := OldParameter.Name;
        typeName := OldParameter.typeName;
        SetBoldTV(TAG_ISCONST, OldParameter.GetBoldTV(TAG_ISCONST));
        kind := OldParameter.kind;
      end;
    end;
    JumpToElement(NewOperation);
  end
  else
    MessageDlg('Error: Could not find operation!', mtError, [mbOK], 0);
end;

procedure TBoldModelEditFrmCx.OnOverrideMenuItemClick(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    if (Sender as TMenuItem).Parent.Name = 'mnuOverrideFrameworkMethods' then
      OverrideFrameworkMethod((Sender as TMenuItem).Caption)
    else if (Sender as TMenuItem).Parent.Name = 'mnuOverrideModelMethods' then
      OverrideModelMethod((Sender as TMenuItem).Caption)
  end;
end;

procedure TBoldModelEditFrmCx.FileOpen1Accept(Sender: TObject);
begin
  fLoadedFrom := FileOpen1.Dialog.FileName;
  LoadModelFromFile;
end;

procedure TBoldModelEditFrmCx.FileOpen1BeforeExecute(Sender: TObject);
begin
  ApplyGUI;
  if fIsExecutingPlugin then
  begin
    ShowMessage('Already executing a plugin');
    Exit;
  end;
  FileOpen1.Dialog.Filter := Copy(BoldUMLModelLinkList.FileFilter, 1, Length(BoldUMLModelLinkList.FileFilter) - Length(ALLFILESFILTER)) ;
end;

procedure TBoldModelEditFrmCx.FileSaveAs1Accept(Sender: TObject);
var
  UMLLinkClass: TBoldUMLModelLinkClass;
  CursorGuard: IBoldCursorGuard;
begin
  fIsExecutingPlugin := true;
  CursorGuard := TBoldCursorGuard.Create;
  try
    UMLLinkClass := BoldUMLModelLinkList.LinkClasses[-1, ExtractFileExt(FileSaveAs1.Dialog.FileName)];
    if Assigned(UMLLinkClass) then
    begin
      with UMLLinkClass.Create(nil) do
      try
        FileName := FileSaveAs1.Dialog.FileName;
        ExportModel(CurrentModel);
        CurrentModel.BoldSystem.UndoHandlerInterface.UndoList.Clear;
        CurrentModel.BoldSystem.UndoHandlerInterface.RedoList.Clear;
      finally
        Free;
      end;
    end;
  finally
    fIsExecutingPlugin := false;
  end;
end;

procedure TBoldModelEditFrmCx.FileSaveAs1BeforeExecute(Sender: TObject);
begin
  FileSaveAs1.Dialog.FileName := fLoadedFrom;
  FileSaveAs1.Dialog.Filter := Copy(BoldUMLModelLinkList.FileFilter, 1, Length(BoldUMLModelLinkList.ExportableFileFilter) - Length(ALLFILESFILTER)) ;
end;

{procedure TBoldModelEditFrmCx.FindComponentInstanceEvent(Reader: TReader;
  const Name: string; var Instance: Pointer);
var
  i: integer;
begin
  i := pos('.', Name);
  if pos('dmBoldUMLModelEditorHandles', Name) > 0 then
    Instance := dmBoldUMLModelEditorHandles.FindComponent(Copy(name, i+1, maxInt))
  else
  if pos('dmModelEdit', Name) > 0 then
    Instance := dmModelEdit.FindComponent(Copy(name, i+1, maxInt))
  else
    Instance := nil;
end;}

function TBoldModelEditFrmCx.FindFrameworkMethod(MethodName: String): String;
var Index: Integer;
begin
  Result := '';
  for Index := 0 to Length(FrameworkMethods) - 1 do
  begin
    if TBoldUMLModelValidator.GetMethodName(FrameworkMethods[Index]) = MethodName then
    begin
      Result := FrameworkMethods[Index];
      Exit;
    end;
  end;
end;

function TBoldModelEditFrmCx.FindModelMethod(MethodName: String): TUMLOperation;
var Index: Integer;
begin
  Result := nil;
  for Index := 0 to ModelMethods.Count - 1 do
  begin
    if TUMLOperation(ModelMethods[Index]).Name = MethodName then
    begin
      Result := TUMLOperation(ModelMethods[Index]);
      Exit;
    end;
  end;
end;

procedure TBoldModelEditFrmCx.mnuOverrideInAllSubclassesClick(
  Sender: TObject);
var
  UMLOperation: TUMLOperation;
begin
  UMLOperation :=  CurrentElement as TUMLOperation;
  TBoldUMLOperationSupport.OverrideInAllSubclasses(UMLOperation.owner, UMLOperation);
end;

procedure TBoldModelEditFrmCx.btInterfaceUsesClick(Sender: TObject);
begin
  TfrmUsesEditor.CreateEditorWithParams('Interface Uses Editor', dmBoldUMLModelEditorHandles.behModel.Value, tbxModelInterfaceUses.BoldProperties.Expression);
end;

procedure TBoldModelEditFrmCx.btImplementationUsesClick(Sender: TObject);
begin
  TfrmUsesEditor.CreateEditorWithParams('Implementation Uses Editor', dmBoldUMLModelEditorHandles.behModel.Value, tbxModelImplementationUses.BoldProperties.Expression);
end;

procedure TBoldModelEditFrmCx.Splitter2CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept := True;
end;

function TBoldModelEditFrmCx.bcrGetSetGetAsCheckBoxState(aFollower: TBoldFollower): TCheckBoxState;
begin
  Result := cbGrayed;
  if Assigned(aFollower.Element) then
    Result := bcrBooleanToCheckBoxGetAsCheckBoxState(aFollower);
end;

procedure TBoldModelEditFrmCx.bcrGetSetSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
begin
  (aFollower.Element as TUMLAttribute).M_StereotypeName.DefaultSubscribe(Subscriber, breReEvaluate);
  aFollower.Element.SubscribeToExpression(aFollower.Controller.Expression, Subscriber, False);
end;

function TBoldModelEditFrmCx.bcrGetSetMayModify(aFollower: TBoldFollower): Boolean;
begin
  Result := (aFollower.Element as TUMLAttribute).GetBoldTV(TAG_ATTRIBUTEKIND) = TV_ATTRIBUTEKIND_DELPHI;
end;

procedure TBoldModelEditFrmCx.bcrBooleanToCheckBoxSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
begin
  aFollower.Element.SubscribeToExpression(aFollower.Controller.Expression, Subscriber, False);
end;

function TBoldModelEditFrmCx.bcrBooleanToCheckBoxGetAsCheckBoxState(aFollower: TBoldFollower): TCheckBoxState;
var
  anElement: TBoldElement;
begin
  Result := cbGrayed;
  if Assigned(aFollower.Element) then
  begin
    anElement := aFollower.Value;
    if Assigned(anElement) then
    begin
      if TVIsTrue(anElement.AsString)then
        Result := cbChecked
      else
        Result := cbUnchecked;
    end;
  end;
end;

procedure TBoldModelEditFrmCx.bcrBooleanToCheckBoxSetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
begin
  aFollower.Value.AsString := BooleanToString(NewValue = cbChecked);
end;

procedure TBoldModelEditFrmCx.bcrGetSetSetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
begin
  bcrBooleanToCheckBoxSetAsCheckBoxState(aFollower, newValue);
end;

procedure TBoldModelEditFrmCx.ShowAttribKindFeaturesRecieve(Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = breResubscribe then
  begin
    FShowAttribKindFeaturesSubscriber.CancelAllSubscriptions;
    dmBoldUMLModelEditorHandles.behAttribute.AddSmallSubscription(FShowAttribKindFeaturesSubscriber, [beValueIdentityChanged ], breResubscribe);
  end;
end;

procedure TBoldModelEditFrmCx.btModelConstraintEditorClick(Sender: TObject);
begin
  ShowTheConstraintEditor(dmBoldUMLModelEditorHandles.blhModelClasses.Value as TUMLModelElement, Self);
end;

procedure TBoldModelEditFrmCx.btShowDerivationExpressionsEditorClick(Sender: TObject);
begin
  ShowDerivationExpressionsEditor(dmBoldUMLModelEditorHandles.blhModelClasses.Value.EvaluateExpressionAsDirectElement('taggedValue[''Bold.DerivationExpressions'']') as TUMLTaggedValue);
end;

procedure TBoldModelEditFrmCx.FormDestroy(Sender: TObject);
begin
  if (csDesigning in ComponentState) then
    UnregisterFindGlobalComponentProc(FindGlobalComponent);
  SaveFormsettingsToRegistry;
  if Assigned(FrameworkMethodMenuItems) then
  begin
    FreeRefsInList(FrameworkMethodMenuItems);
    FreeAndNil(FrameworkMethodMenuItems)
  end;
  if Assigned(ModelMethods) then
    FreeAndNil(ModelMethods);
  FreeItemsInMenu(mnuOverrideFrameworkMethods);
  FreeItemsInMenu(mnuOverrideModelMethods);
  EnsureFreeConstraintEditor;
  FreeAndNil(fModelChangedSubscriber);
  FreeAndNil(FShowAttribKindFeaturesSubscriber);
  FreeAndNil(fSelectionHistory);
  FreeAndNil(fCodeGenerator);
end;

procedure TBoldModelEditFrmCx.btAttributeShowDerivExprEditorClick(Sender: TObject);
var
  Attr: TUMLAttribute;
begin
  Attr := CurrentElement as TUMLAttribute;
  EditOclExpression(CurrentElement, TAG_DERIVATIONOCL, Attr.Owner);
end;

procedure TBoldModelEditFrmCx.btAssoEndShowDeriExprEditorClick(Sender: TObject);
var
  AssoEnd: TUMLAssociationEnd;
begin
  AssoEnd := CurrentElement as TUMLAssociationEnd;
  if Assigned(AssoEnd.OtherEnd.Type_) then
  begin
    EditOclExpression(AssoEnd, TAG_DERIVATIONOCL, AssoEnd.OtherEnd.Type_);
  end
  else
    showmessage('This association is not complete... the other end is not connected');
end;

procedure TBoldModelEditFrmCx.BoldTreeView1Paste(Sender: TObject);
begin
//  with BoldTreeView1 do
  begin
    if Assigned(UMLObjectToCopyOrMove) then
    begin
      if UMLObjectToCopyOrMove is TUMLClass then
      begin
        if UMLObjectToCopyOrMove <> CurrentElement then
          PasteClass;
      end
      else if UMLObjectToCopyOrMove is TUMLFeature then
        PasteFeature
      else if UMLObjectToCopyOrMove is TUMLParameter then
        PasteParameter
      else if UMLObjectToCopyOrMove is TUMLAssociation then
        PasteAssociation
      else if UMLObjectToCopyOrMove is TUMLAssociationEnd then
        PasteAssociationEnd;
        GreyNode := nil;
    end;
  end;
  if Assigned(GreyNode) then
    GreyNode.Cut := False;
end;

procedure TBoldModelEditFrmCx.EditOclExpression(Element: TUMLModelElement; TaggedValue: String; Context: TUMLModelElement);
var
  res: String;
  lPrevIgnoreModelChanges: Boolean;
begin
  EnsureFlattenedAndBoldified;
  lPrevIgnoreModelChanges := IgnoreModelChanges;
  IgnoreModelChanges := true;
  try
    res := BoldUMLOclEditor_.EditOcl(
      ModelHandle,
      Context,
      Element.GetBoldTV(TaggedValue));
  finally
    IgnoreModelChanges := lPrevIgnoreModelChanges;
    Element.SetBoldTV(TaggedValue, res);
  end;
end;

procedure TBoldModelEditFrmCx.btClassDefaultStringRepClick(Sender: TObject);
begin
  EditOclExpression(CurrentElement, TAG_DEFAULTSTRINGREPRESENTATION, CurrentElement);
end;

procedure TBoldModelEditFrmCx.ToolButton1Click(Sender: TObject);
begin
  //TBoldSystemDebuggerFrm.CreateWithSystem(nil, (behModel.Value as TBoldObject).BoldSystem).show;
end;

procedure TBoldModelEditFrmCx.UpdateStatusbar;
var
  InfoText: string;
  HighestSeverity: TSeverity;
begin
  InfoText := '';
  if assigned(dmBoldUMLModelEditorHandles.behHighestSeverity.Value) then
    HighestSeverity := (dmBoldUMLModelEditorHandles.behHighestSeverity.Value as TBASeverity).AsSeverity
  else
    HighestSeverity := sNone;
  case HighestSeverity of
    sNone: begin
      if ModelNeedsValidation then
        InfoText := 'Model not validated'
      else
        InfoText := 'No errors in model';
    end;
    sHint: InfoText := 'There are Hints in this model, Select "Tools"->"Check Consistency" for further details.';
    sWarning: InfoText := 'There are Warnings in this model, Select "Tools"->"Check Consistency" for further details.';
    sError: InfoText := 'There are Errors in this model, Select "Tools"->"Check Consistency" for further details.';
  end;
//  imgHint.Visible := HighestSeverity = sHint;
//  imgWarning.Visible := HighestSeverity in [sWarning, sError];
  Statusbar1.Panels[1].Text := InfoText;
end;

procedure TBoldModelEditFrmCx.WMAppCommand(var Msg: TMessage);
begin
  case GET_APPCOMMAND_LPARAM(Msg.LParam) of
    APPCOMMAND_BROWSER_FORWARD:
    begin
      if fSelectionHistoryIndex = fSelectionHistory.Count-1 then
        exit;
      inc(fSelectionHistoryIndex);
      JumpToElement(fSelectionHistory[fSelectionHistoryIndex] as TUMLModelElement);
        dmBoldUMLModelEditorHandles.brhCurrentElement.Value := fSelectionHistory[fSelectionHistoryIndex] as TUMLModelElement;
      Msg.Result := 1;
    end;
    APPCOMMAND_BROWSER_BACKWARD:
    begin
      if fSelectionHistoryIndex = 0 then
        exit;
      dec(fSelectionHistoryIndex);
      if fSelectionHistoryIndex < fSelectionHistory.Count then
      begin
        JumpToElement(fSelectionHistory[fSelectionHistoryIndex] as TUMLModelElement);
        dmBoldUMLModelEditorHandles.brhCurrentElement.Value := fSelectionHistory[fSelectionHistoryIndex] as TUMLModelElement;
        Msg.Result := 1;
      end;
    end;
  end;
end;

procedure TBoldModelEditFrmCx.NewDatatype1Click(Sender: TObject);
var
  NewDataType: TUMLDataType;
  NameSpace: TUMLNameSpace;
begin
  if (CurrentElement is TUMLPackage) then
    NameSpace := CurrentElement as TUMLNameSpace
  else
    NameSpace := CurrentModel;
  NewDataType := TUMLDataType.Create(NameSpace.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewDataType);
  NewDataType.namespace_ := NameSpace;
  NewDataType.name := TBoldUMLSupport.UniqueName(NewDataType, 'NewDataType');
  NewDataType.persistent := true;
end;

procedure TBoldModelEditFrmCx.NewPackage1Click(Sender: TObject);
var
  NewPackage: TUMLPackage;
  NameSpace: TUMLNameSpace;
begin
  if (CurrentElement is TUMLClass) then
    NameSpace := (CurrentElement as TUMLClass).model as TUMLNameSpace
  else
    NameSpace := CurrentElement as TUMLNameSpace;
  NewPackage := TUMLPackage.Create(NameSpace.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewPackage);
  NewPackage.namespace_ := NameSpace;
  NewPackage.name := TBoldUMLSupport.UniqueName(NewPackage, 'NewPackage');
end;

procedure TBoldModelEditFrmCx.DroppedOnUMLPackage(Dropped: TUMLModelElement; Target: TUMLPackage);
begin
  if (Dropped is TUMLClassifier) or (Dropped is TUMLPackage) then
   Dropped.namespace_ := Target;
end;

procedure TBoldModelEditFrmCx.InsertSuperclass1Click(Sender: TObject);
var
  NewClass: TUMLClass;
  NameSpace: TUMLNameSpace;
begin
  NameSpace := CurrentElement.namespace_;
  NewClass := TUMLClass.Create(NameSpace.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewClass);
  NewClass.namespace_ := NameSpace;
  NewClass.name := TBoldUMLSupport.UniqueName(NewClass,CurrentElement.name);
  NewClass.SetFirstParent(GetCurrentClass.SuperClass);
  GetCurrentClass.SetFirstParent(NewClass);
  NewClass.persistent := true;
  JumpToElement(NewClass);
end;

function TBoldModelEditFrmCx.GetUMLObjectToCopyOrMove: TUMLModelElement;
begin

end;

procedure TBoldModelEditFrmCx.SetUMLObjectToCopyOrMove(
  const Value: TUMLModelElement);
begin

end;

function TBoldModelEditFrmCx.GetCurrentModelIsBoldified: Boolean;
begin
  Result := (dmBoldUMLModelEditorHandles.behBoldified.Value as TBABoolean).AsBoolean = true;
end;

function TBoldModelEditFrmCx.GetIsExecutingPlugin: boolean;
begin
  result := fIsExecutingPlugin;
end;

procedure TBoldModelEditFrmCx.SetIsExecutingPlugin(Value: boolean);
begin
  fIsExecutingPlugIn := value;
end;

procedure TBoldModelEditFrmCx.SetLoadedFrom(const Value: string);
begin
  fLoadedFrom := Value;
end;

function TBoldModelEditFrmCx.rDataTypePMapperGetAsVariant(
  AFollower: TBoldFollower): Variant;
var
  TypeNameMapping: TBoldTypeNameMapping;
  UMLDataType: TUMLDataType;
begin
  Result := Null;
  if Assigned(AFollower.Element) then
  begin
    UMLDataType := AFollower.Element as TUMLDataType;
    TypeNameMapping := ModelHandle.TypeNameDictionary.MappingForModelName[UMLDataType.name];
    Result := TypeNameMapping.ExpandedMapperName;
  end;
end;

procedure TBoldModelEditFrmCx.Boldifymodel1Click(Sender: TObject);
var
  CursorGuard: IBoldCursorGuard;
begin
  IgnoreModelChanges := True;
  CursorGuard := TBoldCursorGuard.Create;
  CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := false;
  CurrentModel.BoldSystem.StartTransaction;
  try
    try
      with ModelHandle.Boldify do
        if IsBoldified(CurrentModel) then
          UnBoldify(CurrentModel)
       else
          Boldify(CurrentModel);
       CurrentModel.BoldSystem.CommitTransaction;
    except
      CurrentModel.BoldSystem.RollbackTransaction;
    end;
  finally
    IgnoreModelChanges := False;
    CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := true;
  end;
end;

procedure TBoldModelEditFrmCx.SearchModel(const Text: string);
var
  Element: TUMLModelElement;
const
  cFindbyName = 'self->select(name = ''%s'')->first';
begin
  Element := CurrentModel.allOwnedElement.EvaluateExpressionAsDirectElement(Format(cFindbyName, [Text])) as TUMLModelElement;
  CurrentElement := Element;
end;

procedure TBoldModelEditFrmCx.SetBoldifyFlattenCaptions;
begin
  if not Assigned(CurrentModel) then
    Exit;
  if TBoldUMLBoldify.IsBoldified(CurrentModel) then
    Boldifymodel1.Caption := 'UnBoldify model'
  else
    Boldifymodel1.Caption := 'Boldify model';
  if TBoldUMLSupport.IsFlattened(CurrentModel) then
    Flatten.Caption := 'UnFlatten package structure'
  else
    Flatten.Caption := 'Flatten package structure';
end;

procedure TBoldModelEditFrmCx.FlattenClick(Sender: TObject);
begin
  IgnoreModelChanges := True;
  CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := false;
  CurrentModel.BoldSystem.StartTransaction;
  try
    try
      if TBoldUMLSupport.IsFlattened(CurrentModel) then
        TBoldUMLSupport.UnFlatten(CurrentModel)
      else
        TBoldUMLSupport.Flatten(CurrentModel);
      CurrentModel.BoldSystem.CommitTransaction;
    except
      CurrentModel.BoldSystem.RollbackTransaction;
    end;
  finally
    IgnoreModelChanges := False;
    CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := true;
  end;
end;

procedure TBoldModelEditFrmCx.LoadModelFromFile;
var
  CursorGuard: IBoldCursorGuard;
  UMLLinkClass: TBoldUMLModelLinkClass;
begin
  if fIsExecutingPlugin then
  begin
    ShowMessage('Already executing a plugin');
    Exit;
  end;
  fIsExecutingPlugin := true;
  CursorGuard := TBoldCursorGuard.Create;
  TBoldQueueable.DeActivateDisplayQueue;
  try
    if Assigned(CurrentModel) then
    begin
      CurrentModel.BoldSystem.UndoHandlerInterface.ClearAllUndoBlocks;
      CurrentModel.BoldSystem.UndoHandlerInterface.RedoList.Clear;
      CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := false;
    end;
    UMLLinkClass := BoldUMLModelLinkList.LinkClasses[-1, ExtractFileExt(fLoadedFrom)];
    if Assigned(UMLLinkClass) then
    begin
      with UMLLinkClass.create(nil) do
      try
        FileName := fLoadedFrom;
        ImportModel(CurrentModel);
        DefaultCodeOutputDirName := ExtractFilePath(fLoadedFrom);
        FileSaveAs1.Dialog.InitialDir := DefaultCodeOutputDirName;
        {$IFDEF AttracsModelEditor}
        If not BoldInternalRunningInIDE then
          InstallAttracsAttributeTypes(GetCurrentModelHandle.TypeNameDictionary);
        {$ENDIF}
      finally
        Free;
      end;
    end;
  finally
    fIsExecutingPlugin := false;
    ModelHandle.DataTypes;
    ModelHandle.MoldModel;
    CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := true;
    Caption := 'Bold Model Editor - ' + fLoadedFrom;
    BoldPropertyMapper1.Enabled := true;
    fSelectionHistory.Clear;
    fSelectionHistoryIndex := -1;
    TBoldQueueable.ActivateDisplayQueue;
  end;
end;

procedure TBoldModelEditFrmCx.Loggform1Click(Sender: TObject);
begin
  BoldLogForm.Show;
end;

procedure TBoldModelEditFrmCx.Tools1Click(Sender: TObject);
begin
  SetBoldifyFlattenCaptions;
end;

procedure TBoldModelEditFrmCx.cmbSuperClassSelectChanged(Sender: TObject);
begin
  with Sender as TBoldComboBox do
  begin
    if BoldHandle.Value is TUMLClassifier then
       TUMLClassifier(BoldHandle.Value).setfirstParent(SelectedElement as TUMLClassifier);
  end;
end;

procedure TBoldModelEditFrmCx.EnsureFlattenedAndBoldified;
var
  Boldifier: TBoldUMLBoldify;
begin
  if CurrentModel = nil then
    Exit;
  Boldifier := ModelHandle.Boldify;

  if not TBoldUMLSupport.IsFlattened(CurrentModel) or not Boldifier.IsBoldified(CurrentModel) then
  begin
    IgnoreModelChanges := True;
    CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := false;
    try
      CurrentModel.BoldSystem.StartTransaction;
      try
        if not TBoldUMLSupport.IsFlattened(CurrentModel) then
          TBoldUMLSupport.Flatten(CurrentModel);

        if not Boldifier.IsBoldified(CurrentModel) then
          Boldifier.Boldify(CurrentModel);
         CurrentModel.BoldSystem.CommitTransaction;
      except
        CurrentModel.BoldSystem.RollbackTransaction;
        raise;
      end;
    finally
      IgnoreModelChanges := False;
      CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := true;
    end;
  end;
end;

procedure TBoldModelEditFrmCx.EnsureUnFlattenedAndUnBoldified;
var
  Boldifier: TBoldUMLBoldify;
begin
  if CurrentModel = nil then
    Exit;
  Boldifier := ModelHandle.Boldify;

  IgnoreModelChanges := True;
  try
    CurrentModel.BoldSystem.StartTransaction;
    try
      if TBoldUMLSupport.IsFlattened(CurrentModel) then
        TBoldUMLSupport.UnFlatten(CurrentModel);

      if  Boldifier.IsBoldified(CurrentModel) then
        Boldifier.UnBoldify(CurrentModel);
       CurrentModel.BoldSystem.CommitTransaction;
    except
      CurrentModel.BoldSystem.RollbackTransaction;
      raise;
    end;
  finally
    IgnoreModelChanges := False;
  end;
end;

procedure TBoldModelEditFrmCx.bdhAttributePMapperNamesDeriveAndSubscribe(
  Sender: TComponent; RootValue: TBoldElement;
  ResultElement: TBoldIndirectElement; Subscriber: TBoldSubscriber);
var
  BoldName, strStreamName: String;
  Index: Integer;
  Mapping : TBoldTypeNameMapping;
begin
  if not Assigned(RootValue) then
  begin
    resultElement.SetOwnedValue(nil);
    Exit;
  end;
  ResultElement.SetOwnedValue(TBoldMemberList.CreateWithTypeInfo(CurrentModel.BoldSystem.BoldSystemTypeInfo.ListTypeInfoByElement[CurrentModel.BoldSystem.BoldSystemTypeInfo.AttributeTypeInfoByExpressionName['String']]));
  BoldName := (Rootvalue as TUMLAttribute).typeName;
  Mapping := CurrentTypeNameDictionary.MappingForModelName[BoldName];
  if assigned(Mapping) then
    strStreamName := Mapping.ExpandedContentsName;

  TBoldMemberList(ResultElement.Value).AddNew.AsString := DEFAULTNAME;


  for Index := 0 to BoldMemberPersistenceMappers.Count - 1 do
    if BoldMemberPersistenceMappers.Descriptors[Index].CanStore(strStreamName) then
      TBoldMemberList(ResultElement.Value).AddNew.AsString := BoldMemberPersistenceMappers.Descriptors[Index].Name;

  if Assigned(subscriber) then
    RootValue.SubscribeToExpression('typeName', Subscriber);
end;

procedure TBoldModelEditFrmCx.CheckBox1Click(Sender: TObject);
begin
  if (Sender as TcxCheckBox).Checked then
  begin
    tvAttributesqualifyingOwner.Visible := true;
    dmBoldUMLModelEditorHandles.behClassAttributes.Expression := 'allfeature->filterOnType(UMLAttribute)'
  end
  else
  begin
    tvAttributesqualifyingOwner.Visible := false;
    dmBoldUMLModelEditorHandles.behClassAttributes.Expression := 'feature->filterOnType(UMLAttribute)'
  end;
end;

procedure TBoldModelEditFrmCx.CheckBox2Click(Sender: TObject);
begin
  tvAttributes.Preview.Visible := (Sender as TcxCheckBox).Checked;
end;

procedure TBoldModelEditFrmCx.Clear1Click(Sender: TObject);
var
  CursorGuard: IBoldCursorGuard;
begin
  if Assigned(CurrentModel) and
    (MessageDlg('This will clear your model, are you sure?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    ApplyGUI;
    CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := false;
    CursorGuard := TBoldCursorGuard.Create;
    CurrentModel.Clear;
    fLoadedFrom := '';
  end;
end;

procedure TBoldModelEditFrmCx.ClearClassFilter;
var
  i: integer;
begin
  for I := 0 to cxCheckGroup1.Properties.Items.Count-1 do
    cxCheckGroup1.States[i] := cbsUnchecked;
  tvClasses.DataController.Filter.Clear;
  tvClasses.DataController.FindCriteria.Text := '';
end;

procedure TBoldModelEditFrmCx.SubscribeToModelChanges;
begin
  if not fModelSubscriptionsValid then
  begin
    fModelChangedSubscriber.CancelAllSubscriptions;
    if (not ModelNeedsValidation) then
    begin
      TBoldUMLSupport.SubscribeToEntireModel(CurrentModel, fModelChangedSubscriber);
      fModelSubscriptionsValid := True;
    end;
  end;
end;

procedure TBoldModelEditFrmCx.SetModelNeedsValidation(const Value: Boolean);
begin
  if value <> fModelNeedsValidation then
  begin
    FModelNeedsValidation := Value;
    UpdateStatusBar;
    SubscribeToModelChanges;
  end;
end;

procedure TBoldModelEditFrmCx.SetIgnoreModelChanges(const Value: Boolean);
begin
  if value <> fIgnoreModelChanges then
  begin
    FIgnoreModelChanges := Value;
    if Value then
      fModelChangedSubscriber.CancelAllSubscriptions
    else
      SubscribeToModelChanges;
  end;
end;

procedure TBoldModelEditFrmCx.ApplyGUI;
begin
  TBoldQueueable.ApplyAll;
end;

function TBoldModelEditFrmCx.bcrBooleanToCheckBoxMayModify(aFollower: TBoldFollower): Boolean;
var
  ValueElement: TBoldElement;
begin
  ValueElement := aFollower.Value;
  if Assigned(ValueElement) then
    Result := ValueElement.ObserverMayModify(aFollower.subscriber)
  else
    Result := false;
end;

function TBoldModelEditFrmCx.GetElementToPaste: TUMLModelElement;
begin
  Result := nil;
  case CutOrCopy of
    cckCopy:
      Result := TBoldCopyAndClone.BoldClone(UMLObjectToCopyOrMove, bcmDeep) as TUMLModelElement;
    cckCut:
      Result := UMLObjectToCopyOrMove;
  end;
end;

procedure TBoldModelEditFrmCx.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ApplyGUI;
  Action := caFree;
end;

procedure TBoldModelEditFrmCx.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  s: string;
  res: integer;
begin
  CanClose := true;
  if not CurrentModel.BoldSystem.UndoHandlerInterface.UndoList.ContainsChanges then
    exit;
  if BoldInternalRunningInIDE then
    s := 'Save changes to component?'
  else
  if (fLoadedFrom <> '') then
    s := Format('Save changes to %s ?', [fLoadedFrom]);
  res := Application.MessageBox(Pchar(s), 'Confirm', MB_YESNOCANCEL or MB_ICONQUESTION);
  case res of
    mrCancel: CanClose := false;
    mrYes:
      begin
        CanClose := true;
        SaveModelToFile(fLoadedFrom);
      end;
    mrNo: ;
  end;
end;

procedure TBoldModelEditFrmCx.FormActivate(Sender: TObject);
begin
  G_ValidationFormDefaultOwner := Self;
  AppEvents.OnHint := AppEventsHint;
end;

procedure TBoldModelEditFrmCx.AddSubclass1Click(Sender: TObject);
var
  SuperClass: TUMLClass;
  NewClass: TUMLClass;
begin
  SuperClass := GetCurrentClass;
  if not assigned(SuperClass) then
    raise Exception.Create('Unable to understand which class to use as superclass');
  NewClass := TUMLClass.Create(SuperClass.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewClass);
  NewClass.namespace_ := SuperClass.NameSpace_;
  NewClass.name := TBoldUMLSupport.UniqueName(NewClass, SuperClass.name);
  NewClass.SetFirstParent(SuperClass);
  NewClass.persistent := true;
  JumpToElement(NewClass);
end;

function TBoldModelEditFrmCx.GetCurrentClass: TUMLClass;
begin
  if CurrentElement is TUMLClass then
    result := CurrentElement as TUMLClass
  else if (CurrentElement is TUMLFeature) and ((CurrentElement as TUMLfeature).owner is TUMLClass) then
    result := (CurrentElement as TUMLfeature).owner as TUMLClass
  else
    result := dmBoldUMLModelEditorHandles.blhModelClasses.CurrentBoldObject as TUMLClass;
end;

function TBoldModelEditFrmCx.GetCurrentPackage: TUMLNameSpace;
begin
  if CurrentElement is TUMLClass then
    result := (CurrentElement as TUMLClass).namespace_
  else if CurrentElement is TUMLAssociationEnd then
    result := (CurrentElement as TUMLAssociationEnd).qualifyingOwner.namespace_
  else if CurrentElement is TUMLAssociation then
    result := (CurrentElement as TUMLAssociation).namespace_
  else if CurrentElement is TUMLAttribute then
    result := (CurrentElement as TUMLAttribute).owner.namespace_
  else if CurrentElement is TUMLNameSpace then
    result := CurrentElement as TUMLNameSpace
  else
    result := nil;
end;

procedure TBoldModelEditFrmCx.FormDeactivate(Sender: TObject);
begin
  TBoldQueueable.ApplyAllMatching(self);
  AppEvents.OnHint := nil;
end;

function TBoldModelEditFrmCx.GetCurrentTypeNameDictionary: TBoldTypeNameDictionary;
begin
  if assigned(ModelHandle) then
    result := ModelHandle.TypeNameDictionary
  else
    result := nil;
end;

procedure TBoldModelEditFrmCx.SetModelHandle(const Value: TBoldModel);
var
  vSystemHandle: TBoldSystemHandle;
  BoldAbstractPersistenceHandleDB: TBoldAbstractPersistenceHandleDB;
  i: integer;
begin
  if FModelHandle <> Value then
  try
    TBoldQueueable.DeActivateDisplayQueue;
    if Assigned(Value) then
      Value.FreeNotification(Self);
    fModelHandle := Value;
    dmBoldUMLModelEditorHandles.brhRoot.Value := ModelHandle.EnsuredUMLModel;
    dmBoldUMLModelEditorHandles.ModelHandle := Value;
    vSystemHandle := ModelHandle.SystemHandle;
    BoldUndoAction1.BoldSystemHandle := vSystemHandle;
    BoldRedoAction1.BoldSystemHandle := vSystemHandle;
    for i := 0 to ComponentCount-1 do
      if Components[i] is TBoldNonSystemHandle then
        (Components[i] as TBoldNonSystemHandle).StaticSystemHandle := vSystemHandle;

    if assigned(CurrentModel) then
    begin
      EnsureTypesFromTypeNameHandle(nil);
      TBoldUMLSupport.EnsureBoldTaggedVAlues(CurrentModel);
    end
    else
      raise EBoldInternal.CreateFmt('%s.SetRoot: Root must be part of Model', [ClassName]);
    fUndoForm.BoldSystemHandle := vSystemHandle;
    fModelHandle.DataTypes;
    fModelHandle.MoldModel;
    ModelNeedsValidation := True;
    SubscribeToModelChanges;
    if Assigned(CurrentModel) and Assigned(CurrentModel.BoldSystem) then
      TBoldUndoHandler(CurrentModel.BoldSystem.UndoHandler).ReuseEmptyBlocks := false;

    for i := 0 to BoldHandle.BoldHandleList.Count - 1 do
      if (BoldHandleList[i] is TBoldAbstractPersistenceHandleDB) then
      begin
        BoldAbstractPersistenceHandleDB := (BoldHandleList[i] as TBoldAbstractPersistenceHandleDB);
        if (BoldAbstractPersistenceHandleDB.BoldModel = Value) then
          dxComboDatabase.Items.AddObject(BoldAbstractPersistenceHandleDB.Name, BoldAbstractPersistenceHandleDB);
      end;
    dxComboDatabase.ItemIndex := 0;
    fSelectionHistory.Clear;
    fSelectionHistoryIndex := -1;
  finally
    TBoldQueueable.ActivateDisplayQueue;
    if Assigned(CurrentModel) and Assigned(CurrentModel.BoldSystem) then
      CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := true;
  end;
end;

procedure TBoldModelEditFrmCx.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (aComponent = fModelHandle) and (Operation = opRemove) then
    Free;
end;

function TBoldModelEditFrmCx.GetCurrentModelHandle: TBoldModel;
begin
  Result := fModelHandle;
end;

procedure TBoldModelEditFrmCx.AppEventsHint(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := GetLongHint(Application.Hint);
end;

procedure TBoldModelEditFrmCx.AppEventsIdle(Sender: TObject;
  var Done: Boolean);
var
  vBoldSystem: TBoldSystem;
begin
  if fIsExecutingPlugin then
    exit;
  if not BoldInstalledQueue.Empty then
    exit;
  if Assigned(ModelHandle) and Assigned(ModelHandle.SystemHandle) and ModelHandle.SystemHandle.Active then
  begin
    vBoldSystem := ModelHandle.SystemHandle.System;
    if vBoldSystem.UndoHandlerInterface.Enabled
      and vBoldSystem.UndoHandlerInterface.CurrentUndoBlockHasChanges
//      and (vBoldSystem.UndoHandlerInterface.RedoList.Count = 0)
    then
      if Application.ModalLevel = 0 then
      vBoldSystem.UndoHandlerInterface.SetCheckPoint;
    SyncSelection;
  end;
end;

procedure TBoldModelEditFrmCx.SyncSelection;
var
  BoldAwareView: IBoldAwareView;
begin
  if (Screen.ActiveForm = self) and (Screen.ActiveControl is TcxBoldGridSite) and Supports(TcxBoldGridSite(Screen.ActiveControl).GridView, IBoldAwareView, BoldAwareView) then
  begin
    CurrentElement;
    if not BoldInstalledQueue.Empty then
    exit;
    if (CurrentElement <> BoldAwareView.CurrentBoldObject) and (BoldAwareView.CurrentBoldObject is TUMLModelElement) then
    begin
      SetCurrentElement(BoldAwareView.CurrentBoldObject as TUMLModelElement);
    end;
  end;
end;

procedure TBoldModelEditFrmCx.SetCheckBoxHints;
var
  i: integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TBoldCheckBox then
      TBoldCheckBox(Components[i]).Hint := StripHotkey(TBoldCheckBox(Components[i]).Caption);
end;

initialization
  BoldUMLModelEdit.BoldModelEditFrmClass := TBoldModelEditFrmCx;

end.
