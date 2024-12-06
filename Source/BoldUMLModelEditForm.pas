{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelEditForm;

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
  ImageList,
  BoldXCVTreeView,
  BoldSubscription,
  BoldElements,
  BoldControlPack,
  BoldStringControlPack,
  BoldEdit,
  BoldCheckBox,
  Menus,
  ToolWin,
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
  BoldTreeView,
  Commctrl,
  BoldSmallLogFrame,
  BoldUMLModelEdit,
  BoldDragObject,
  BoldDerivedHandle,
  BoldPropertiesController,
  AppEvnts;

type
  CutOrCopyKind = (cckCut, cckCopy, cckNone);

type
  TUMLPlugInToolButton = class(TToolButton)
  private
    FPlugIn: TUMLPlugIn;
  public
    property PlugIn: TUMLPlugIn read FPlugIn write FPlugIn;
  end;

  TBoldModelEditFrm = class(TForm, IUnknown, IUMLModelPlugInContext, IBoldModelEditForm)
    behBoldified: TBoldExpressionHandle;
    tbxModelRootClass: TBoldEdit;
    bcbRemoveSuperOnUnboldify: TBoldCheckBox;
    bcrAutoCreated: TBoldAsCheckBoxStateRenderer;
    bcbRemoveOnUnboldify: TBoldCheckBox;
    bsrRedOnAutocreated: TBoldAsStringRenderer;
    Flatten: TMenuItem;
    Boldifymodel1: TMenuItem;
    Loggform1: TMenuItem;
    behClassIsRootClass: TBoldExpressionHandle;
    bphClassIsRootClass: TBoldPropertiesController;
    bcbIsRootClass: TBoldCheckBox;
    BoldPropertiesController1: TBoldPropertiesController;
    bdhAttributePMapperNames: TBoldDerivedHandle;
    bchAttributePMapperNames: TBoldCursorHandle;
    pcModelEdit: TPageControl;
    tabModel: TTabSheet;
    tabClass: TTabSheet;
    tabAttribute: TTabSheet;
    tabOperation: TTabSheet;
    tabAssociation: TTabSheet;
    tabAssociationEnd: TTabSheet;
    behModel: TBoldExpressionHandle;
    behClass: TBoldExpressionhandle;
    lblAttributeName: TLabel;
    tbxAttributeName: TBoldEdit;
    lblAttributeDelphiName: TLabel;
    lblAttributeExpressionName: TLabel;
    lblAttributePMapperName: TLabel;
    lblAttributeColumnName: TLabel;
    tbxAttributeDelphiName: TBoldEdit;
    tbxAttributeExpressionName: TBoldEdit;
    tbxAttributeColumnName: TBoldEdit;
    behAttribute: TBoldExpressionhandle;
    behOperation: TBoldExpressionhandle;
    behAssociation: TBoldExpressionhandle;
    behAssociationEnd: TBoldExpressionhandle;
    lblAttributeBoldType: TLabel;
    bcbAttributePersistent: TBoldCheckBox;
    bcbAttributeDelayedFetch: TBoldCheckBox;
    bcbAttributeAllowNull: TBoldCheckBox;
    bcbAttributeDerived: TBoldCheckBox;
    lblAttributeLength: TLabel;
    tbxAttributeLength: TBoldEdit;
    behClassSuperClass: TBoldExpressionHandle;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    Edit1: TMenuItem;
    Tools1: TMenuItem;
    Help1: TMenuItem;
    Clear1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Undo1: TMenuItem;
    mnuConsistencycheck: TMenuItem;
    N3: TMenuItem;
    Contents1: TMenuItem;
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
    tabParameter: TTabSheet;
    lblParameterName: TLabel;
    tbxParameterName: TBoldEdit;
    behParameter: TBoldExpressionhandle;
    tbxParameterType: TBoldEdit;
    lblParameterType: TLabel;
    lblParameterKind: TLabel;
    NewParameter1: TMenuItem;
    Model1: TMenuItem;
    Deletexxx2: TMenuItem;
    N11: TMenuItem;
    NewClass2: TMenuItem;
    NewAssociation2: TMenuItem;
    NewAttribute2: TMenuItem;
    NewOperation2: TMenuItem;
    NewParameter2: TMenuItem;
    brhTreeRoot: TBoldReferenceHandle;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    sbModel: TScrollBox;
    lblModelName: TLabel;
    lblModelDelphiName: TLabel;
    lblModelExpressionName: TLabel;
    lblModelPMapperName: TLabel;
    lblModelInterfaceUses: TLabel;
    lblModelImplementationUses: TLabel;
    tbxModelName: TBoldEdit;
    tbxModelDelhpiName: TBoldEdit;
    tbxModelExpressionName: TBoldEdit;
    tbxModelPMapperName: TBoldEdit;
    tbxModelInterfaceUses: TBoldEdit;
    tbxModelImplementationUses: TBoldEdit;
    pcModelTabs: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Splitter2: TSplitter;
    BoldGrid1: TBoldGrid;
    BoldGrid2: TBoldGrid;
    blhModelClasses: TBoldListHandle;
    blhModelAssociations: TBoldListHandle;
    sbClass: TScrollBox;
    lblClassName: TLabel;
    lblClassDelphiName: TLabel;
    lblClassExpressionName: TLabel;
    lblClassPMapperName: TLabel;
    lblClassTableName: TLabel;
    lblClassFileName: TLabel;
    lblClassTableMapping: TLabel;
    lblClassSuperclass: TLabel;
    tbxClassName: TBoldEdit;
    tbxClassDelphiName: TBoldEdit;
    tbxClassExpressionName: TBoldEdit;
    tbxClassTableName: TBoldEdit;
    tbxClassFileName: TBoldEdit;
    bcbClassPersistent: TBoldCheckBox;
    bcbClassAbstract: TBoldCheckBox;
    bcbClassImported: TBoldCheckBox;
    Splitter3: TSplitter;
    pcClassTabs: TPageControl;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    BoldGrid3: TBoldGrid;
    BoldGrid4: TBoldGrid;
    blhClassAttributes: TBoldListHandle;
    blhClassOperations: TBoldListHandle;
    sbOperation: TScrollBox;
    lblOperationName: TLabel;
    lblOperationDelphiName: TLabel;
    lblOperationExpressionName: TLabel;
    lblOperationOwnerScope: TLabel;
    lblOperationDelphiFunctionType: TLabel;
    tbxOperationName: TBoldEdit;
    tbxOperationDelphiName: TBoldEdit;
    tbxOperationExpressionName: TBoldEdit;
    Splitter4: TSplitter;
    pcOperationTabs: TPageControl;
    TabSheet5: TTabSheet;
    BoldGrid5: TBoldGrid;
    blhParameters: TBoldListHandle;
    sbAssociation: TScrollBox;
    lblAssociationName: TLabel;
    lblAssociationClass: TLabel;
    tbxAssociationName: TBoldEdit;
    Splitter5: TSplitter;
    pcAssociationTabs: TPageControl;
    TabSheet6: TTabSheet;
    BoldGrid6: TBoldGrid;
    blhAssociationAssociationEnds: TBoldListHandle;
    blhAllClasses: TBoldListHandle;
    tbxParameterExpressionName: TBoldEdit;
    lblParameterExpressionName: TLabel;
    N2: TMenuItem;
    ilTreeView: TImageList;
    ilMenu: TImageList;
    N4: TMenuItem;
    NewQualifier1: TMenuItem;
    NewQualifier2: TMenuItem;
    sbAssociationEnd: TScrollBox;
    lblAssociationEndName: TLabel;
    lblAssociationEndDelphiName: TLabel;
    lblAssociationEndExpressionName: TLabel;
    lblAssociationEndColumnName: TLabel;
    lblAssociationEndClass: TLabel;
    tbxAssociationEndName: TBoldEdit;
    tbxAssociationEndDelphiName: TBoldEdit;
    tbxAssociationEndExpressionName: TBoldEdit;
    tbxAssociationEndColumnName: TBoldEdit;
    bcbAssociationEndNavigable: TBoldCheckBox;
    bcbAssociationEndMulti: TBoldCheckBox;
    bcbAssociationEndOrdered: TBoldCheckBox;
    bcbAssociationEndMandatory: TBoldCheckBox;
    bcbAssociationEndEmbed: TBoldCheckBox;
    Splitter6: TSplitter;
    pcAssociationEndTabs: TPageControl;
    TabSheet7: TTabSheet;
    BoldGrid7: TBoldGrid;
    blhAssociationEndQualifiers: TBoldListHandle;
    BoldTreeView1: TBoldXCVTreeView;
    View1: TMenuItem;
    mnuShowAttributes: TMenuItem;
    mnuShowRoles: TMenuItem;
    mnuShowOperations: TMenuItem;
    ToolBar1: TToolBar;
    Separator1: TToolButton;
    Separator2: TToolButton;
    tbnFilterAttributes: TToolButton;
    tbnFilterRoles: TToolButton;
    tbnFilterOperations: TToolButton;
    mnuBreak1: TMenuItem;
    mnuBreak2: TMenuItem;
    mnuAdvanced: TMenuItem;
    odOpenModel: TOpenDialog;
    OpenFile1: TMenuItem;
    sdSaveModel: TSaveDialog;
    cmbAttributeBoldType: TBoldComboBox;
    cmbAttributePMapperName: TBoldComboBox;
    cmbClassPMapperName: TBoldComboBox;
    mnuOverrideFrameworkMethods: TMenuItem;
    mnuOverrideModelMethods: TMenuItem;
    bcbIsConst: TBoldCheckBox;
    mnuOverrideInAllSubclasses: TMenuItem;
    btInterfaceUses: TButton;
    btImplementationUses: TButton;
    bsrNiceCRRenderer: TBoldAsStringRenderer;
    cmbSuperClass: TBoldComboBox;
    Label1: TLabel;
    cmbTableMapping: TBoldComboBox;
    cmbOwnerScope: TBoldComboBox;
    cmbAssociationLinkClass: TBoldComboBox;
    cmbAssoEndClass: TBoldComboBox;
    cmbParamKind: TBoldComboBox;
    cmbDelphiFunctionType: TBoldComboBox;
    TabSheet9: TTabSheet;
    bcbModelUseXFiles: TBoldCheckBox;
    bcbModelUseTimestamp: TBoldCheckBox;
    bcbUseGlobalId: TBoldCheckBox;
    bcbUseReadOnly: TBoldCheckBox;
    lblAttributeStereotype: TLabel;
    blhAllParameterKind: TBoldListHandle;
    blhAllOwnerScope: TBoldListHandle;
    behAssociationEndType: TBoldExpressionHandle;
    bcrGetSet: TBoldAsCheckBoxStateRenderer;
    blhAllSuperclasses: TBoldListHandle;
    blbModelInfo: TBoldLabel;
    blbClassInfo: TBoldLabel;
    blbAttributeInfo: TBoldLabel;
    blbOperationInfo: TBoldLabel;
    blbAssociationInfo: TBoldLabel;
    blbAssociationEndInfo: TBoldLabel;
    blbParameterInfo: TBoldLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    bcrBooleanToCheckBox: TBoldAsCheckBoxStateRenderer;
    cmbTVAttributeKind: TBoldComboBox;
    lblAttributeKind: TLabel;
    gbDelphiFeatures: TGroupBox;
    bcbDPHasField: TBoldCheckBox;
    cmbTVDPRead: TBoldComboBox;
    cmbTVDPWrite: TBoldComboBox;
    lblDPRead: TLabel;
    lblDPWrite: TLabel;
    tbxAttributeDerivationOCL: TBoldEdit;
    lblAttributeDerivationOCL: TLabel;
    tbxAttributeStereotype: TBoldEdit;
    tbxModelStereotype: TBoldEdit;
    lblModelStereotype: TLabel;
    tbxClassStereotype: TBoldEdit;
    lblClassStereotype: TLabel;
    tbxOperationStereotype: TBoldEdit;
    lblOperationStereotype: TLabel;
    tbxAssociationStereotype: TBoldEdit;
    lblAssociationStereotype: TLabel;
    tbxAssociationEndStereotype: TBoldEdit;
    lblStereotype: TLabel;
    tbxParameterStereotype: TBoldEdit;
    lblParameterStereotype: TLabel;
    blhAllAggregationKind: TBoldListHandle;
    Label2: TLabel;
    cmbAggregationKind: TBoldComboBox;
    cmbAssoEndVisibility: TBoldComboBox;
    lblAssoEndVisibility: TLabel;
    blhAllVisibilityKind: TBoldListHandle;
    lblAttributeVisibility: TLabel;
    cmbAttributeVisibility: TBoldComboBox;
    cmbOperationVisibility: TBoldComboBox;
    lblOperationVisibility: TLabel;
    lblAssoEndChangeability: TLabel;
    cmbAssoEndChangeability: TBoldComboBox;
    blhAllChangeabilityKind: TBoldListHandle;
    tbxAttributeInitialValue: TBoldEdit;
    lblAttributeInitialValue: TLabel;
    lblModelConstraints: TLabel;
    tbxModelConstraints: TBoldEdit;
    btModelConstraintEditor: TButton;
    brhCurrentElement: TBoldReferenceHandle;
    lblClassConstraint: TLabel;
    tbxClassConstraint: TBoldEdit;
    btClassConstraintEditor: TButton;
    lblAttributeConstraint: TLabel;
    tbxAttributeConstraint: TBoldEdit;
    btAttributeConstraintEditor: TButton;
    lblOperationConstraint: TLabel;
    tbxOperationConstraint: TBoldEdit;
    btOperationConstraintEditor: TButton;
    lblAssociationConstraint: TLabel;
    tbxAssociationConstraint: TBoldEdit;
    btAssociationConstraintEditor: TButton;
    lblAssociationEndConstraint: TLabel;
    tbxAssociationEndConstraint: TBoldEdit;
    btAssociationEndConstraintEditor: TButton;
    lblParameterConstraint: TLabel;
    tbxParameterConstraint: TBoldEdit;
    btParameterConstraintEditor: TButton;
    lbMultiplicity: TLabel;
    bvhTableMapping: TBoldVariableHandle;
    bcrTableMapping: TBoldCursorhandle;
    bchAttributeKind: TBoldCursorhandle;
    bvhAttributeKind: TBoldVariableHandle;
    bvhDelphiProperty: TBoldVariableHandle;
    bchDelphiProperty: TBoldCursorhandle;
    bchDelphiFunctionType: TBoldCursorhandle;
    bvhDelphiFunctionType: TBoldVariableHandle;
    cbOperationOverrideInAllSubclasses: TBoldCheckBox;
    bchMultiplicityValues: TBoldCursorhandle;
    bvhMultiplicityValues: TBoldVariableHandle;
    cmbMultiplicity: TBoldComboBox;
    lblClassDerivationExpressions: TLabel;
    tbxClassDerivationExpressions: TBoldEdit;
    btShowDerivationExpressionsEditor: TButton;
    bcbAttributeReverseDerive: TBoldCheckBox;
    cmbAssoEndDeleteAction: TBoldComboBox;
    lblAssoEndDeleteAction: TLabel;
    bchDeleteActions: TBoldCursorHandle;
    bvhDeleteActions: TBoldVariableHandle;
    bcbAssociationDerived: TBoldCheckBox;
    lblAssoEndDerivationOCL: TLabel;
    tbxAssoEndDerivationOCL: TBoldEdit;
    Label12: TLabel;
    tbxClassUnitName: TBoldEdit;
    btAttributeShowDerivExprEditor: TButton;
    btAssoEndShowDeriExprEditor: TButton;
    bcbPersistent: TBoldCheckBox;
    lblClassDefaultStringRep: TLabel;
    edClassDefaultStringRep: TBoldEdit;
    btClassDefaultStringRep: TButton;
    TabSheet8: TTabSheet;
    BoldGrid9: TBoldGrid;
    blhClassAssociationEnds: TBoldListHandle;
    edModelGUID: TBoldEdit;
    edModelTypeLibVersion: TBoldEdit;
    lblModelGUID: TLabel;
    lblModelTypeLibVersion: TLabel;
    edClassGUID: TBoldEdit;
    lblClassGUID: TLabel;
    ToolButton1: TToolButton;
    bcbUseClockLog: TBoldCheckBox;
    bcbGenerateMultiplicityConstraints: TBoldCheckBox;
    bvhOptimisticLocking: TBoldVariableHandle;
    bchOptimisticLocking: TBoldCursorHandle;
    cmbClassOptimisticLocking: TBoldComboBox;
    cmbModelOptimisticLocking: TBoldComboBox;
    lblModelOptimisticLocking: TLabel;
    lblClassOptimisticLocking: TLabel;
    behHighestSeverity: TBoldExpressionHandle;
    tabPackage: TTabSheet;
    sbPaCKAGE: TScrollBox;
    lbPackageName: TLabel;
    lbPackageExpressionname: TLabel;
    blbPackageInfo: TBoldLabel;
    Label22: TLabel;
    lbPackageStereotyp: TLabel;
    tbxPackageName: TBoldEdit;
    tbxPackageExpressionName: TBoldEdit;
    tbxStereotypeName: TBoldEdit;
    Splitter7: TSplitter;
    pcPackageTabs: TPageControl;
    TabSheet11: TTabSheet;
    BoldGrid8: TBoldGrid;
    TabSheet12: TTabSheet;
    BoldGrid10: TBoldGrid;
    TabSheet13: TTabSheet;
    brhPackage: TBoldExpressionhandle;
    blhPackageAssociations: TBoldListHandle;
    blhPackageClasses: TBoldListHandle;
    blhAllDataTypes: TBoldListHandle;
    tabDataType: TTabSheet;
    Label15: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    BoldLabel2: TBoldLabel;
    Label21: TLabel;
    BoldEdit5: TBoldEdit;
    Label23: TLabel;
    BoldEdit6: TBoldEdit;
    Label24: TLabel;
    Label25: TLabel;
    BoldEdit7: TBoldEdit;
    Label26: TLabel;
    BoldEdit8: TBoldEdit;
    brhDataType: TBoldExpressionhandle;
    NewDatatype1: TMenuItem;
    FetchDatatypes: TMenuItem;
    NewPackage1: TMenuItem;
    InsertSuperclass1: TMenuItem;
    NewDataType2: TMenuItem;
    InsertSuperclass2: TMenuItem;
    brhCopyCut: TBoldReferenceHandle;
    tabQualifier: TTabSheet;
    BoldEdit1: TBoldEdit;
    BoldEdit3: TBoldEdit;
    BoldComboBox1: TBoldComboBox;
    BoldEdit4: TBoldEdit;
    BoldEdit9: TBoldEdit;
    Label27: TLabel;
    BoldLabel1: TBoldLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    bvhAdvancedMode: TBoldVariableHandle;
    bpcAdvancedView: TBoldPropertiesController;
    N5: TMenuItem;
    N6: TMenuItem;
    imgWarning: TImage;
    imgHint: TImage;
    bpcAttributeDerivedVisibility: TBoldPropertiesController;
    bpcAttributeReverseDerivedEnabled: TBoldPropertiesController;
    bpcAttributeDelayedFetchEnabled: TBoldPropertiesController;
    bpcAssociationEndDerivationVisible: TBoldPropertiesController;
    AddSubclass1: TMenuItem;
    AddSubclass2: TMenuItem;
    apeHintCatcher: TApplicationEvents;
    function bcrAutoCreatedGetAsCheckBoxState(aFollower: TBoldFollower): TCheckBoxState;
    procedure bcrAutoCreatedSetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
    procedure bsrRedOnAutocreatedSetColor(aFollower: TBoldFollower; var AColor: TColor);
    procedure Boldifymodel1Click(Sender: TObject);
    procedure FlattenClick(Sender: TObject);
    procedure Loggform1Click(Sender: TObject);
    procedure Tools1Click(Sender: TObject);
    procedure cmbSuperClassSelectChanged(Sender: TObject);
    procedure bdhAttributePMapperNamesDeriveAndSubscribe(Sender: TComponent;
      RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
      Subscriber: TBoldSubscriber);
    procedure Clear1Click(Sender: TObject);
    procedure BoldTreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure popTreePopup(Sender: TObject);
    procedure DeleteXxx1Click(Sender: TObject);
    procedure NewClass1Click(Sender: TObject);
    procedure NewAssociation1Click(Sender: TObject);
    procedure NewAttribute1Click(Sender: TObject);
    procedure NewOperation1Click(Sender: TObject);
    procedure NewParameter1Click(Sender: TObject);
    procedure mnuConsistencycheckClick(Sender: TObject);
    procedure tbnFilterRolesClick(Sender: TObject);
    procedure tbnFilterOperationsClick(Sender: TObject);
    procedure NewQualifier1Click(Sender: TObject);
    procedure mnuShowAttributesClick(Sender: TObject);
    procedure tbnFilterAttributesClick(Sender: TObject);
    procedure mnuShowRolesClick(Sender: TObject);
    procedure mnuShowOperationsClick(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure mnuAdvancedClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pcModelEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OpenFile1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure BoldTreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure BoldTreeView1DragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure SaveFileAs1Click(Sender: TObject);
    procedure mnuOverrideInAllSubclassesClick(Sender: TObject);
    procedure btInterfaceUsesClick(Sender: TObject);
    procedure btImplementationUsesClick(Sender: TObject);
    function bsrNiceCRRendererGetAsString(aFollower: TBoldFollower): String;
    procedure bsrNiceCRRendererSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
    procedure Splitter2CanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure BoldTreeView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Contents1Click(Sender: TObject);
    procedure BoldTreeView1Expanded(Sender: TObject; Node: TTreeNode);
    function bcrGetSetGetAsCheckBoxState(aFollower: TBoldFollower): TCheckBoxState;
    procedure bcrGetSetSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
    function bcrGetSetMayModify(aFollower: TBoldFollower): Boolean;
    procedure bcrBooleanToCheckBoxSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
    function bcrBooleanToCheckBoxGetAsCheckBoxState(aFollower: TBoldFollower): TCheckBoxState;
    procedure bcrBooleanToCheckBoxSetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
    procedure bcrGetSetSetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
    procedure btModelConstraintEditorClick(Sender: TObject);
    procedure btShowDerivationExpressionsEditorClick(Sender: TObject);
    procedure N3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btAttributeShowDerivExprEditorClick(Sender: TObject);
    procedure btAssoEndShowDeriExprEditorClick(Sender: TObject);
    procedure BoldTreeView1Cut(Sender: TObject);
    procedure BoldTreeView1Copy(Sender: TObject);
    procedure BoldTreeView1Paste(Sender: TObject);
    procedure BoldTreeView1StartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure btClassDefaultStringRepClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure EnsureTypesFromTypeNameHandle(Sender: TObject);
    procedure NewDatatype1Click(Sender: TObject);
    procedure NewPackage1Click(Sender: TObject);
    procedure InsertSuperclass1Click(Sender: TObject);
    function bcrBooleanToCheckBoxMayModify(aFollower: TBoldFollower): Boolean;
    procedure BoldTreeView1EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure AddSubclass1Click(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure apeHintCatcherHint(Sender: TObject);
  private
    { Private declarations }
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
    fLoadedFrom: string;
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
    procedure SetIsAdvancedMode(advanced: Boolean);
    procedure AddMenuTool(MenuItem: TMenuItem);
    procedure AddMenuFiler(MenuItem: TMenuItem);
    procedure AddButtonTool(ToolButton: TToolButton);
    procedure AddButtonFiler(ToolButton: TToolButton);
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
    procedure EnsureOwnerExpanded(element: TUMLModelElement);
    function GetNodeForElement(element: TUMLModelElement; Caption: String = ''): TBoldTreeNode;
    procedure PopulateComboBoxes;
    procedure SetLoadedFrom(const Value: string);
    property UMLObjectToCopyOrMove: TUMLModelElement read GetUMLObjectToCopyOrMove write SetUMLObjectToCopyOrMove;
    procedure SetBoldifyFlattenCaptions;
    procedure ApplyGUI;
    function GetElementToPaste: TUMLModelElement;
    procedure UpdateStatusbar;
    function GetCurrentClass: TUMLClass;
    function GetCurrentPackage: TUMLNameSpace;
    function GetCurrentTypeNameDictionary: TBoldTypeNameDictionary;
    procedure SetModelHandle(const Value: TBoldModel);
    procedure LoadModelFromFile;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function QueryInterface(const IId: TGUID; out Obj): HResult; override; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
    function GetCurrentModel: TUMLModel;
    function GetCurrentModelHandle: TBoldModel;
    function GetCurrentElement: TUMLModelElement;
    property IgnoreModelChanges: Boolean read FIgnoreModelChanges write SetIgnoreModelChanges;
  public
    constructor Create(AOnwer: TComponent); override;
    function CloseThisForm: TCloseAction;
    procedure EnsureFlattenedAndBoldified;
    property CurrentElement: TUMLModelElement read GetCurrentElement write SetCurrentElement;
    property ModelHandle: TBoldModel read FModelHandle write SetModelHandle;
    property IsAdvancedMode: Boolean  write SetIsAdvancedMode;
    property CurrentModel: TUMLModel read GetCurrentModel;
    property CurrentTypeNameDictionary: TBoldTypeNameDictionary read GetCurrentTypeNameDictionary;
    property CurrentModelIsBoldified: Boolean read GetCurrentModelIsBoldified;
    property PlugInlist: TList read FPlugInlist write SetPlugInlist;
    procedure JumpToElement(sender: TComponent; element: TUMLModelElement);
    property ModelNeedsValidation: Boolean read FModelNeedsValidation write SetModelNeedsValidation;
    property LoadedFrom: string write SetLoadedFrom;
  end;


implementation

uses
  System.UITypes,
  BoldDefaultTaggedValues,
  BoldUMLAttributes,
  BoldUMLModelConverter,
  BoldUMLModelValidationForm,
  BoldUtils,
  BoldGuard,
  BoldCursorGuard,
  BoldSystem,
  BoldAttributes,
  BoldUMLModelValidator,
  BoldUMLModelLink,
  BoldLogHandler,
  BoldPMapperLists,
  BoldDefs,
  BoldGUI,
  BoldQueue,
  BoldUMLUsesEditorForm,
  BoldTaggedValueSupport,
  BoldUMLConstraintEditor,
  BoldUMLModelDataModule,
  BoldUMLTaggedValuesEditor,
  BoldDerivationExpressionsEditor,
  BoldUMLOCLEditor,
  BoldUMLModelSupport,
  BoldUMLPluginCallBacks,
  BoldUMLAbstractModelValidator,
  BoldRegistry,
  BoldUMLModelEditorHandlesDataModule,
  BoldLogHandlerForm;

{$R *.dfm}

function FindGlobalComponent(const Name: string): TComponent;
begin
  if AnsiSameText(Name, 'dmBoldUMLModelEditorHandles') then
    Result := BoldUMLModelEditorHandlesDataModule.dmBoldUMLModelEditorHandles
  else
  if AnsiSameText(Name, 'dmModelEdit') then
  begin
    EnsureModelEditDataModule;
    result := dmModelEdit;
  end
  else
    Result := nil;
end;

procedure TBoldModelEditFrm.BoldTreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if BoldTreeView1.CurrentElement is TUMLModelElement then
    CurrentElement := TUMLModelElement(BoldTreeView1.CurrentElement)
  else
    CurrentElement := nil;
end;

procedure TBoldModelEditFrm.SetCurrentElement(Element: TUMLModelElement);
begin
  ApplyGUI;
  brhCurrentElement.Value := Element;
  if not Assigned(Element) then
    pcModelEdit.ActivePage := nil
  else if Element is TUMLModel then
    pcModelEdit.ActivePage := tabModel
  else if Element is TUMLClass then
    pcModelEdit.ActivePage := tabClass
  else if Element is TUMLAttribute then
  begin
    if TUMLAttribute(Element).IsQualifier then
      pcModelEdit.ActivePage := tabQualifier
    else
      pcModelEdit.ActivePage := tabAttribute
  end
  else if Element is TUMLOperation then
    pcModelEdit.ActivePage := tabOperation
  else if Element is TUMLAssociation then
    pcModelEdit.ActivePage := tabAssociation
  else if Element is TUMLAssociationEnd then
    pcModelEdit.ActivePage := tabAssociationEnd
  else if Element is TUMLParameter then
    pcModelEdit.ActivePage := tabParameter
  else if Element is TUMLPackage then
    pcModelEdit.ActivePage := tabPackage
  else if Element is TUMLDataType then
    pcModelEdit.ActivePage := tabDataType;
end;

procedure TBoldModelEditFrm.popTreePopup(Sender: TObject);
var
  i: integer;
begin
  BoldTreeView1.Selected := BoldTreeView1.Selected; // Fix for right-click-select

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

  if CurrentElement is TUMLPackage then
  begin
    if not (CurrentElement is TUMLModel) then
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
  else if CurrentElement is TUMLClass then
  begin
    DeleteXxx1.Caption := 'Delete Class ' + CurrentElement.Name;
    DeleteXxx1.Visible := True;
    N10.Visible := True;
    NewClass1.Visible := true;
    InsertSuperclass1.Visible := True;
    AddSubclass1.Visible := true;
    NewAttribute1.Visible := True;
    NewOperation1.Visible := True;
    N9.Visible := true;
    mnuOverrideFrameworkMethods.Visible := True;
    mnuOverrideModelMethods.Visible := True;
    CreateFrameworkMethodItems(CurrentElement as TUMLClass);
    CreateModelMethodItems(CurrentElement as TUMLClass);
  end
  else if CurrentElement is TUMLAttribute then
  begin
    DeleteXxx1.Caption := 'Delete Attribute ' + CurrentElement.Name;
    DeleteXxx1.Visible := True;
    // this check ignores qualifiers
    if assigned((CurrentElement as TUMLAttribute).owner) then
    begin
      NewAttribute1.Visible := True;
      AddSubclass1.Visible := true;
      NewOperation1.Visible := True;
      NewClass1.Visible := true;
    end;
  end
  else if CurrentElement is TUMLOperation then
  begin
    DeleteXxx1.Caption := 'Delete Operation ' + CurrentElement.Name;
    DeleteXxx1.Visible := True;
    N10.Visible := True;
    NewAttribute1.Visible := True;
    AddSubclass1.Visible := true;
    NewOperation1.Visible := True;
    NewParameter1.Visible := True;
    NewClass1.Visible := true;
    if ((CurrentElement as TUMLOperation).Owner.Subclasses.Count > 0) and
       ((CurrentElement.GetBoldTV(TAG_DELPHIOPERATIONKIND) = TV_DELPHIOPERATIONKIND_VIRTUAL) or
       (CurrentElement.GetBoldTV(TAG_DELPHIOPERATIONKIND) = TV_DELPHIOPERATIONKIND_DYNAMIC) or
       (CurrentElement.GetBoldTV(TAG_DELPHIOPERATIONKIND) = TV_DELPHIOPERATIONKIND_ABSTRACTVIRTUAL)) then
      mnuOverrideInAllSubclasses.Visible := True
    else
      mnuOverrideInAllSubclasses.Visible := False;
  end
  else if CurrentElement is TUMLAssociation then
  begin
    DeleteXxx1.Caption := 'Delete Association ' + CurrentElement.Name;
    DeleteXxx1.Visible := True;
  end
  else if CurrentElement is TUMLAssociationEnd then
  begin
    NewQualifier1.Visible := True;
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
  NewParameter2.Visible := NewParameter1.Visible;
  NewQualifier2.Visible := NewQualifier1.Visible;
  NewDataType2.Visible :=  NewDataType1.Visible;
  // Disable the menu shortcuts that are not visible
  for i := 0 to ComponentCount-1 do
    if Components[i] is TMenuItem then
      (Components[i] as TMenuItem).Enabled := (Components[i] as TMenuItem).visible

end;

procedure TBoldModelEditFrm.DeleteXxx1Click(Sender: TObject);
begin
  if not BoldTreeView1.IsEditing then
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

procedure TBoldModelEditFrm.NewClass1Click(Sender: TObject);
var
  NewClass: TUMLClass;
  NameSpace: TUMLNameSpace;
begin
  NameSpace := GetCurrentPackage;
  NewClass := TUMLClass.Create(NameSpace.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewClass);
  NewClass.namespace_ := NameSpace;
  NewClass.name := TBoldUMLSupport.UniqueName(NewClass, 'NewClass');
  if CurrentModelIsBoldified then
    NewClass.SetFirstParent(TBoldUMLBoldify.GetRootClass(CurrentModel));
  NewClass.persistent := true;
end;

procedure TBoldModelEditFrm.NewAssociation1Click(Sender: TObject);
var
  NewAssociation: TUMLAssociation;
  NewAssociationEnd: TUMLASsociationEnd;
  NameSpace: TUMLNameSpace;
  i: integer;
begin
  NameSpace := CurrentElement as TUMLNameSpace;
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
end;

procedure TBoldModelEditFrm.NewAttribute1Click(Sender: TObject);
var
  NewAttribute: TUMLAttribute;
  Class_: TUMLClass;
begin
  Class_ := GetCurrentClass;
  NewAttribute := TUMLAttribute.Create(Class_.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewAttribute);
  NewAttribute.owner := Class_;
  NewAttribute.name := TBoldUMLSupport.UniqueName(NewAttribute, 'NewAttribute');
  NewAttribute.typeName := 'String';
  NewAttribute.persistent := Class_.persistent;
end;

procedure TBoldModelEditFrm.NewOperation1Click(Sender: TObject);
var
  NewOperation: TUMLOperation;
  Class_: TUMLClass;
begin
  Class_ := GetCurrentClass;
  NewOperation := TUMLOperation.Create(Class_.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewOperation);
  NewOperation.owner := Class_;
  NewOperation.name := TBoldUMLSupport.UniqueName(NewOperation, 'NewOperation');
end;

procedure TBoldModelEditFrm.NewParameter1Click(Sender: TObject);
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

procedure TBoldModelEditFrm.NewQualifier1Click(Sender: TObject);
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

procedure TBoldModelEditFrm.SetIsAdvancedMode(advanced: Boolean);
begin
 (bvhAdvancedMode.Value as TBABoolean).asBoolean := advanced;
end;

procedure TBoldModelEditFrm.mnuConsistencycheckClick(Sender: TObject);
var
  ValidationForm: TfrmValidation;
begin
  ApplyGUI;
  EnsureFlattenedAndBoldified;
  ValidationForm := EnsureValidationForm(GetCurrentModelHandle, self, JumpToElement);
  ValidationForm.ValidationProc := TBoldUMLModelValidatorCallBack.Validate;
  ValidationForm.Show;
  ValidationForm.Validate;
end;

function TBoldModelEditFrm.GetCurrentModel: TUMLModel;
begin
  if assigned(brhTreeRoot.Value) then
    result := (brhTreeRoot.Value as TUMLModelElement).model
  else
    result := nil;
end;

function TBoldModelEditFrm.GetNodeForElement(element: TUMLModelElement; Caption: String): TBoldTreeNode;
var
  node: TBoldTreeNode;
begin
  result := nil;
  node := BoldTreeView1.Items.GetFirstNode as TBoldTreeNode;
  while assigned(node) do
  begin
    if (node.Follower.Element = element) and
      ((Caption = '') or (Caption = node.Text)) then
    begin
      result := node;
      break;
    end;
    node := node.GetNext as TBoldTreeNode;
  end;
end;

procedure TBoldModelEditFrm.JumpToElement(sender: TComponent; element: TUMLModelElement);
var
  node: TBoldTreeNode;
begin
  EnsureOwnerExpanded(element);
  node := GetNodeForElement(Element);
  if assigned(node) then
    BoldTreeView1.Selected := node
end;

procedure TBoldModelEditFrm.EnsureOwnerExpanded(element: TUMLModelElement);

  procedure ExpandNodeForElementWithCaption(element: TUMLModelElement; Caption: string = '');
  var
    node: TBoldTreeNode;
  begin
    node := GetNodeForElement(element, Caption);
    if assigned(node) and not node.expanded then
      node.Expand(false);
  end;

var
  owner: TUMLModelElement;
begin
  owner := element.qualifyingOwner;
  if assigned(owner) then
  begin
    EnsureOwnerExpanded(Owner);
    // Empty caption always expands the topmost one in tree
    ExpandNodeForElementWithCaption(owner);
    // Note: Names here must match names in treeview.
    if owner is TUMLModel then
      ExpandNodeForElementWithCaption(owner, 'Logical View');
    if element is TUMLAssociation then
      ExpandNodeForElementWithCaption(owner, 'Associations')
    else if element is TUMLDataType then
      ExpandNodeForElementWithCaption(owner, 'DataTypes');
  end;
end;

procedure TBoldModelEditFrm.AddMenuTool(menuItem: TMenuItem);
begin
  if Assigned(MenuItem) then
    Tools1.Insert(4, menuItem);
end;

procedure TBoldModelEditFrm.AddMenuFiler(menuItem: TMenuItem);
begin
  if Assigned(MenuItem) then
  begin
    mnuFile.Insert(2, menuItem);
  end;
end;

procedure TBoldModelEditFrm.AddButtonTool(ToolButton: TToolButton);
begin
  if Assigned(ToolButton) then
  begin
    ToolButton.Parent := ToolBar1;
    ToolButton.Left := Separator2.Left;
  end;
end;

procedure TBoldModelEditFrm.AddButtonFiler(ToolButton: TToolButton);
begin
  if Assigned(ToolButton) then
  begin
    ToolButton.Parent := ToolBar1;
    ToolButton.Left := Separator1.Left;
  end;
end;

procedure TBoldModelEditFrm.SetPlugInList(const Value: TList);
var
  i: Integer;
begin
  FPlugInList := Value;
  for i := 0 to Value.Count - 1 do
    InstallPlugIn(TObject(Value.Items[i]) as TUMLPlugIn);
end;

procedure TBoldModelEditFrm.InstallPlugIn(PlugIn: TUMLPlugIn);
begin
  if PlugIn.PlugInType = ptTool then
    AddTool(PlugIn)
  else
    AddFiler(PlugIn);
end;

procedure TBoldModelEditFrm.AddTool(PlugIn: TUMLPlugIn);
begin
  AddMenuTool(NewPlugInMenuItem(PlugIn));
  AddButtonTool(NewPlugInToolButton(PlugIn));
end;

procedure TBoldModelEditFrm.AddFiler(PlugIn: TUMLPlugIn);
begin
  AddMenuFiler(NewPlugInMenuItem(PlugIn));
  AddButtonFiler(NewPlugInToolButton(PlugIn));
end;

function TBoldModelEditFrm.NewPlugInMenuItem(PlugIn: TUMLPlugIn): TUMLPlugInMenuItem;
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

function TBoldModelEditFrm.NewPlugInToolButton(PlugIn: TUMLPlugIn): TUMLPlugInToolButton;
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

procedure TBoldModelEditFrm.ExecPlugIn(PlugIn: TUMLPlugin);
var
  CursorGuard: IBoldCursorGuard;
begin
  ApplyGUI;
  CursorGuard := TBoldCursorGuard.Create;
  if poRequireBoldified in PlugIn.Options then
    EnsureFlattenedAndBoldified;

  if poRequireUnBoldified in PlugIn.Options then
    EnsureUnFlattenedAndUnBoldified;

  PlugIn.GuardedExecute(Self);
end;

procedure TBoldModelEditFrm.PlugInMenuClick(sender: TObject);
var PlugIn: TUMLPlugIn;
begin
  if (Sender is TUMLPlugInMenuItem) then
  begin
    PlugIn := TUMLPlugInMenuItem(Sender).PlugIn;
    ExecPlugin(PlugIn);
  end;
end;

procedure TBoldModelEditFrm.PlugInButtonClick(sender: TObject);
var PlugIn: TUMLPlugIn;
begin
  if (Sender is TUMLPlugInToolButton) then
  begin
    TUMLPlugInToolButton(Sender).Parent.SetFocus;
    PlugIn := TUMLPlugInToolButton(Sender).PlugIn;
    ExecPlugin(PlugIn);
  end;
end;

function TBoldModelEditFrm.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TBoldModelEditFrm._AddRef: Integer;
begin
  result := -1;
end;

function TBoldModelEditFrm._Release: Integer;
begin
  result := -1;
end;

function TBoldModelEditFrm.GetCurrentElement: TUMLModelElement;
begin
  result :=   brhCurrentElement.Value as TUMLModelElement;
end;

procedure TBoldModelEditFrm.tbnFilterRolesClick(Sender: TObject);
begin
  (Sender as TToolButton).Parent.SetFocus;
  mnuShowRolesClick(mnuShowRoles);
end;

procedure TBoldModelEditFrm.tbnFilterOperationsClick(Sender: TObject);
begin
  (Sender as TToolButton).Parent.SetFocus;
  mnuShowOperationsClick(mnuShowOperations);
end;

procedure TBoldModelEditFrm.mnuShowAttributesClick(Sender: TObject);
begin
  mnuShowAttributes.Checked := not mnuShowAttributes.Checked;
  BoldTreeView1.FindListPartByNames('UMLClass', 'Attributes').Enabled := mnuShowAttributes.Checked;
  tbnFilterAttributes.Down := mnuShowAttributes.Checked;
end;

procedure TBoldModelEditFrm.tbnFilterAttributesClick(Sender: TObject);
begin
  (Sender as TToolButton).Parent.SetFocus;
  mnuShowAttributesClick(mnuShowAttributes);
end;

procedure TBoldModelEditFrm.mnuShowRolesClick(Sender: TObject);
begin
  mnuShowRoles.Checked := not mnuShowRoles.Checked;
  BoldTreeView1.FindListPartByNames('UMLClass', 'AssociationEnds').Enabled := mnuShowRoles.Checked;
  BoldTreeView1.FindListPartByNames('UMLAssociation', 'AssociationEnds').Enabled := mnuShowRoles.Checked;
  tbnFilterRoles.Down := mnuShowRoles.Checked;
end;

procedure TBoldModelEditFrm.mnuShowOperationsClick(Sender: TObject);
begin
  mnuShowOperations.Checked := not mnuShowOperations.Checked;
  BoldTreeView1.FindListPartByNames('UMLClass', 'Operations').Enabled := mnuShowOperations.Checked;
  tbnFilterOperations.Down := mnuShowOperations.Checked;
end;

procedure TBoldModelEditFrm.Paste1Click(Sender: TObject);
begin
  If Assigned(ActiveControl) then
    ActiveControl.Perform(WM_PASTE, 0, 0);
end;

procedure TBoldModelEditFrm.FormCreate(Sender: TObject);
var
  Index: Integer;
  dmModelEdit: TdmModelEdit;
begin
  dmModelEdit := TdmModelEdit.Create(self);
  SetCheckBoxHints;
  popTree.AutoHotkeys := maManual;

//  HelpFile := MODELEDITORHELPFILE;

  CutOrCopy := cckNone;

  fModelChangedSubscriber := TBoldPassthroughSubscriber.Create(ModelChangedRecieve);
  FShowAttribKindFeaturesSubscriber := TBoldPassthroughSubscriber.Create(ShowAttribKindFeaturesRecieve);

  for Index := 0 to pcModelEdit.PageCount - 1 do
    pcModelEdit.Pages[Index].TabVisible := False;

  PopulateComboBoxes;
  fLogframe := TBoldLogFrame.create(self);
  fLogFrame.Parent := self;
  imgWarning.Visible := false;
  imgWarning.Parent := Statusbar1;
  imgWarning.top := 3;
  imgWarning.Left := 7;
  imgHint.Visible := false;
  imgHint.Parent := Statusbar1;
  imgHint.top := 3;
  imgHint.Left := 7;
  // set up the static systemhandle for all handles,
  dmModelEdit.bshUMLModel.Active := true;
  for index := 0 to ComponentCount-1 do
    if Components[index] is TBoldNonSystemHandle then
      (Components[index] as TBoldNonSystemHandle).StaticSystemHandle := dmModelEdit.bshUMLModel;
  // this will not normally be the correct systemhandle, but since these components only
  // need "anEnum.allInstances" it is OK if they have the correct model
  blhAllAggregationKind.RootHandle := dmModelEdit.bshUMLModel;
  blhAllChangeabilityKind.RootHandle := dmModelEdit.bshUMLModel;
  blhAllOwnerScope.RootHandle := dmModelEdit.bshUMLModel;
  blhAllParameterKind.RootHandle := dmModelEdit.bshUMLModel;
  blhAllVisibilityKind.RootHandle := dmModelEdit.bshUMLModel;

  for index := 0 to ComponentCount-1 do
    if Components[index] is TBoldRootedHandle then
      if (Components[index] as TBoldRootedHandle).RootHandle = nil then
        raise Exception.Create(TBoldRootedHandle(Components[index]).Name);
end;

procedure TBoldModelEditFrm.PopulateComboBoxes;
begin
  bvhTableMapping.InitialValues.Clear;
  TBoldTaggedValueSupport.AddTableMappings(bvhTableMapping.InitialValues);

  bvhAttributeKind.InitialValues.Clear;
  TBoldTaggedValueSupport.AddAttributeKinds(bvhAttributeKind.InitialValues);

  bvhDelphiProperty.InitialValues.Clear;
  TBoldTaggedValueSupport.AddPropertyAccessKinds(bvhDelphiProperty.InitialValues);

  bvhDelphiFunctionType.InitialValues.Clear;
  TBoldTaggedValueSupport.AddDelphiFunctionTypes(bvhDelphiFunctionType.InitialValues);

  bvhDeleteActions.InitialValues.Clear;
  TBoldTaggedValueSupport.AddDeleteActions(bvhDeleteActions.InitialValues);
end;

procedure TBoldModelEditFrm.EnsureTypesFromTypeNameHandle(Sender: TObject);
begin
  IgnoreModelChanges := True;
  ModelHandle.EnsureTypes;
  if cmbAttributeBoldType.Items.IndexOf(cmbAttributeBoldType.Text) = -1 then
    cmbAttributeBoldType.Text := '';
  IgnoreModelChanges := False;
end;

procedure TBoldModelEditFrm.GetCopyPasteRef;
begin
  if UMLObjectToCopyOrMove <> TUMLModelElement(BoldTreeView1.CurrentElement) then
    UMLObjectToCopyOrMove := TUMLModelElement(BoldTreeView1.CurrentElement);
end;

procedure TBoldModelEditFrm.Copy1Click(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    ActiveControl.Perform(WM_COPY, 0, 0);
end;

procedure TBoldModelEditFrm.Cut1Click(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    ActiveControl.Perform(WM_CUT, 0, 0);
end;

procedure TBoldModelEditFrm.mnuAdvancedClick(Sender: TObject);
begin
  mnuAdvanced.Checked := not mnuAdvanced.Checked;
  IsAdvancedMode := mnuAdvanced.Checked;
end;

procedure TBoldModelEditFrm.PasteClass;
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

procedure TBoldModelEditFrm.PasteFeature;
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

procedure TBoldModelEditFrm.PasteAssociation;
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

procedure TBoldModelEditFrm.PasteAssociationEnd;
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

procedure TBoldModelEditFrm.PasteParameter;
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

procedure TBoldModelEditFrm.FormKeyDown(Sender: TObject; var Key: Word;
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

procedure TBoldModelEditFrm.pcModelEditChange(Sender: TObject);
begin
  if pcModelEdit.ActivePage = tabClass then
  begin
    if not Assigned((BoldTreeView1.CurrentElement as TUMLClass).Superclass) then
      cmbSuperClass.Enabled := False
    else
      cmbSuperClass.Enabled := True;
  end;
end;

procedure TBoldModelEditFrm.ModelChangedRecieve(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if GetCurrentModelHandle.IsValidating then
    exit;
  fModelSubscriptionsValid := false;
  if not fIgnoreModelChanges then
    ModelNeedsValidation := True;
end;

procedure TBoldModelEditFrm.FormShow(Sender: TObject);
var
  Index: Integer;
begin
  ShowAttribKindFeaturesRecieve(behAttribute, beValueChanged, breResubscribe);

  for Index := 0 to BoldObjectPersistenceMappers.Count - 1 do
    cmbClassPMapperName.Items.Add(BoldObjectPersistenceMappers.Descriptors[Index].Name);
end;

procedure TBoldModelEditFrm.OpenFile1Click(Sender: TObject);
begin
  ApplyGUI;
  if fIsExecutingPlugin then
  begin
    ShowMessage('Already executing a plugin');
    Exit;
  end;
  odOpenModel.Filter := Copy(BoldUMLModelLinkList.FileFilter, 1, Length(BoldUMLModelLinkList.FileFilter) - Length(ALLFILESFILTER)) ;
  if odOpenModel.Execute then
  begin
    fLoadedFrom := odOpenModel.FileName;
    LoadModelFromFile;
  end;
end;

procedure TBoldModelEditFrm.SaveFileAs1Click(Sender: TObject);
var
  UMLLinkClass: TBoldUMLModelLinkClass;
  CursorGuard: IBoldCursorGuard;
begin
  ApplyGUI;
  if fIsExecutingPlugin then
  begin
    ShowMessage('Already executing a plugin');
    Exit;
  end;

  sdSaveModel.FileName := fLoadedFrom;
  sdSaveModel.Filter := Copy(BoldUMLModelLinkList.FileFilter, 1, Length(BoldUMLModelLinkList.ExportableFileFilter) - Length(ALLFILESFILTER)) ;
  if sdSaveModel.Execute then
  begin
    fIsExecutingPlugin := true;
    CursorGuard := TBoldCursorGuard.Create;
    try
      UMLLinkClass := BoldUMLModelLinkList.LinkClasses[-1, ExtractFileExt(sdSaveModel.FileName)];
      if Assigned(UMLLinkClass) then
      begin
        with UMLLinkClass.Create(nil) do
        try
          FileName := sdSaveModel.FileName;
          ExportModel(CurrentModel);
        finally
          Free;
        end;
      end;
    finally
      fIsExecutingPlugin := false;
    end;
  end;
end;

procedure TBoldModelEditFrm.SaveFormsettingsToRegistry;
var
  BoldRegistry: TBoldRegistry;
  BoldGuard: IBoldGuard;
begin
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
      WriteInteger('TreeViewWidth', BoldTreeView1.Width);
      WriteInteger('ModelHeight', sbModel.Height);
      WriteInteger('ClassHeight', sbClass.Height);
      WriteInteger('OperationHeight', sbOperation.Height);
      WriteInteger('AssociationHeight', sbAssociation.Height);
      WriteInteger('AssociationEndHeight', sbAssociationEnd.Height);
    end;
  except
  end;
end;

procedure TBoldModelEditFrm.LoadFormsettingsFromRegistry;
var
  BoldRegistry: TBoldRegistry;
  BoldGuard: IBoldGuard;
begin
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
      BoldTreeView1.Width := ReadInteger('TreeViewWidth', BoldTreeView1.Width);
      sbModel.Height := ReadInteger('ModelHeight', sbModel.Height);
      sbClass.Height := ReadInteger('ClassHeight', sbClass.Height);
      sbOperation.Height := ReadInteger('OperationHeight', sbOperation.Height);
      sbAssociation.Height := ReadInteger('AssociationHeight', sbAssociation.Height);
      sbAssociationEnd.Height := ReadInteger('AssociationEndHeight', sbAssociationEnd.Height);
    end;
  except
  end;
end;

procedure TBoldModelEditFrm.LoadModelFromFile;
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
  try
    if Assigned(CurrentModel) then
      CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := false;
    UMLLinkClass := BoldUMLModelLinkList.LinkClasses[-1, ExtractFileExt(fLoadedFrom)];
    if Assigned(UMLLinkClass) then
    begin
      with UMLLinkClass.create(nil) do
      try
        FileName := fLoadedFrom;
        ImportModel(CurrentModel);
      finally
        Free;
      end;
    end;
  finally
    fIsExecutingPlugin := false;
    ModelHandle.DataTypes;
    ModelHandle.MoldModel;
    CurrentModel.BoldSystem.UndoHandlerInterface.Enabled := true;
  end;
end;

procedure TBoldModelEditFrm.Exit1Click(Sender: TObject);
begin
  ApplyGUI;
  Close;
end;

function TBoldModelEditFrm.CloseThisForm: TCloseAction;
begin
  ApplyGUI;
  Result := caHide;
end;

procedure TBoldModelEditFrm.BoldTreeView1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var DropTarget: TUMLModelElement;
  DraggedElement: TUMLModelElement;
begin
  Accept := False;
  Assert(Sender is TBoldTreeView);
  if BoldGUIHandler.DraggedObjects.Count = 1  then
  begin
    DraggedElement := (BoldGUIHandler.DraggedObjects[0] as TUMLModelElement);
    DropTarget := (Sender as TBoldTreeView).GetElementAt(X, Y) as TUMLModelElement;
    if DropTarget is TUMLClass then
      Accept := DropOnUMLClass(DraggedElement)
    else if DropTarget is TUMLAttribute then
      Accept := DropOnUMLAttribute(DraggedElement)
    else if DropTarget is TUMLOperation then
      Accept := DropOnUMLOperation(DraggedElement)
    else if DropTarget is TUMLParameter then
      Accept := DropOnUMLParameter(DraggedElement)
    else if DropTarget is TUMLAssociation then
      Accept := DropOnUMLAssociation(DraggedElement)
    else if DropTarget is TUMLAssociationEnd then
      Accept := DropOnUMLAssociationEnd(DraggedElement)
    else if DropTarget is TUMLPackage then
      Accept := DropOnUMLPackage(DraggedElement)
  end;
end;

function TBoldModelEditFrm.DropOnUMLClass(ToBeDropped: TUMLModelElement): Boolean;
begin
  Result := (ToBeDropped is TUMLOperation) or
            (ToBeDropped is TUMLAttribute) or
            (ToBeDropped is TUMLAssociationEnd) or
            (ToBeDropped is TUMLClass);
  if (ToBeDropped is TUMLClass) then
    if CurrentModelIsBoldified and (TUMLClass(ToBeDropped) = TBoldUMLBoldify.GetRootClass(CurrentModel))then
      Result := False;
end;

function TBoldModelEditFrm.DropOnUMLAssociation(ToBeDropped: TUMLModelElement): Boolean;
begin
  Result := False;
end;

function TBoldModelEditFrm.DropOnUMLAssociationEnd(ToBeDropped: TUMLModelElement): Boolean;
begin
  Result := False;
end;

function TBoldModelEditFrm.DropOnUMLAttribute(ToBeDropped: TUMLModelElement): Boolean;
begin
  Result := False;
end;

function TBoldModelEditFrm.DropOnUMLPackage(ToBeDropped: TUMLModelElement): Boolean;
begin
   Result := (ToBeDropped is TUMLClass)
            or
            ((ToBeDropped is TUMLPackage) and
              (not (ToBeDropped is TUMLModel)));
end;

function TBoldModelEditFrm.DropOnUMLOperation(ToBeDropped: TUMLModelElement): Boolean;
begin
  Result := (ToBeDropped is TUMLParameter)
end;

function TBoldModelEditFrm.DropOnUMLParameter(ToBeDropped: TUMLModelElement): Boolean;
begin
  Result := (ToBeDropped is TUMLParameter);
end;

procedure TBoldModelEditFrm.BoldTreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DropTarget: TUMLModelElement;
  DroppedElement: TUMLModelElement;
begin
  if Sender is TBoldTreeView then
  begin
    DropTarget := (Sender as TBoldTreeView).GetElementAt(X, Y) as TUMLModelElement;
    DroppedElement := (BoldGUIHandler.DraggedObjects[0] as TUMLModelElement);
    if DropTarget is TUMLClass then
      DroppedOnUMLClass(DroppedElement, DropTarget as TUMLClass)
    else if DropTarget is TUMLOperation then
      DroppedOnUMLOperation(DroppedElement as TUMLParameter, DropTarget as TUMLOperation)
    else if DropTarget is TUMLParameter then
      DroppedOnUMLParameter(DroppedElement, DropTarget as TUMLParameter)
    else if DropTarget is TUMLPackage then
      DroppedOnUMLPackage(DroppedElement, DropTarget as TUMLPackage)
  end;
end;

procedure TBoldModelEditFrm.DroppedOnUMLParameter(Dropped: TUMLModelElement; Target: TUMLParameter);
begin
  if (Dropped is TUMLParameter) and ((Dropped as TUMLParameter).behavioralFeature.Owner = Target.behavioralFeature.Owner) then
  begin
    with (Dropped as TUMLParameter).behavioralFeature do
      Parameter.Move(Parameter.IndexOf(Dropped as TUMLParameter), Parameter.IndexOf(Target as TUMLParameter));
  end;
end;

procedure TBoldModelEditFrm.DroppedOnUMLClass(Dropped: TUMLModelElement; Target: TUMLClass);
begin
  if Dropped is TUMLAttribute then
    (Target as TUMLClass).Feature.Add(Dropped as TUMLAttribute)
  else if Dropped is TUMLClass then
  begin
    if Target <> Dropped then
      (Dropped as TUMLClass).SetFirstParent(Target);
  end
  else if Dropped is TUMLOperation then
    Target.Feature.Add(Dropped as TUMLOperation)
  else if Dropped is TUMLAssociationEnd then
    (Dropped as TUMLAssociationEnd).OtherEnd.Type_ := Target;
end;

procedure TBoldModelEditFrm.DroppedOnUMLOperation(Dropped: TUMLParameter; Target: TUMLOperation);
begin
  Target.Parameter.Add(Dropped);
end;

procedure TBoldModelEditFrm.GetParamNames(Method: String; ResultList: TStringList);
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

procedure TBoldModelEditFrm.GetParamTypes(Method: String; ResultList: TStringList);
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

function TBoldModelEditFrm.GetPlugInList: TList;
begin
  result := FPlugInList;
end;

function TBoldModelEditFrm.GetReturnType(Method: String): String;
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

procedure TBoldModelEditFrm.ClearMenuItems(var MenuItem: TMenuItem);
begin
  while MenuItem.Count > 0 do
    MenuItem.Remove(MenuItem.Items[0]);
end;

constructor TBoldModelEditFrm.Create(AOnwer: TComponent);
begin
  if (csDesigning in ComponentState) then
    RegisterFindGlobalComponentProc(FindGlobalComponent);
  inherited;
  if (csDesigning in ComponentState) then
    InitInheritedComponent(Self, TForm);
end;

procedure TBoldModelEditFrm.CreateFrameworkMethodItems(UMLClass: TUMLClass);
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
    if not TBoldUMLOperationSupport.ClassHasOperation(UMLClass, TBoldUMLModelValidator.GetMethodName(FrameworkMethods[Index]), TypeList) then
    begin
      MenuItem := TMenuItem.Create(popTree);
      MenuItem.Caption := TBoldUMLModelValidator.GetMethodName(FrameworkMethods[Index]);
      MenuItem.OnClick := OnOverrideMenuItemClick;
      FrameworkMethodMenuItems.Add(MenuItem);
      //MenuItem := nil;
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

procedure TBoldModelEditFrm.FreeRefsInList(List: TList);
var
  Index: Integer;
begin
  for Index := 0 to List.Count - 1 do
    TMenuItem(List.Items[Index]).Free;
end;

procedure TBoldModelEditFrm.FreeItemsInMenu(InMenu: TMenuItem);
begin
  while InMenu.Count > 0 do
    InMenu[0].Free;
end;

procedure TBoldModelEditFrm.CreateModelMethodItems(UMLClass: TUMLClass);
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
      if not TBoldUMLOperationSupport.ClassHasOperation(UMLClass, TUMLOperation(ModelMethods[Index]).Name, TypeList) then
      begin
        MenuItem := TMenuItem.Create(popTree);
        MenuItem.Caption := TUMLOperation(ModelMethods[Index]).Name;
        MenuItem.OnClick := OnOverrideMenuItemClick;
        mnuOverrideModelMethods.Add(MenuItem);
      end;
    end;
  finally
    TypeList.Free;
  end;
end;

procedure TBoldModelEditFrm.OverrideFrameworkMethod(MethodName: String);
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
  end
  else
    MessageDlg('Error: Could not find operation!', mtError, [mbOK], 0);
end;

procedure TBoldModelEditFrm.CreateParamsFromStringList(ParamNames, ParamTypes: TStringList; NewOperation: TUMLOperation; ReturnType: String);
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

procedure TBoldModelEditFrm.OverrideModelMethod(MethodName: String);
var
  OldOperation, NewOperation: TUMLOperation;
  OldParameter, NewParameter: TUMLParameter;
  Index: Integer;
  Class_: TUMLClass;
begin
  OldOperation := FindModelMethod(MethodName);
  if Assigned(OldOperation) then
  begin
    Class_ := GetCurrentClass;
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
  end
  else
    MessageDlg('Error: Could not find operation!', mtError, [mbOK], 0);
end;

procedure TBoldModelEditFrm.OnOverrideMenuItemClick(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    if (Sender as TMenuItem).Parent.Name = 'mnuOverrideFrameworkMethods' then
      OverrideFrameworkMethod((Sender as TMenuItem).Caption)
    else if (Sender as TMenuItem).Parent.Name = 'mnuOverrideModelMethods' then
      OverrideModelMethod((Sender as TMenuItem).Caption)
  end;
end;

function TBoldModelEditFrm.FindFrameworkMethod(MethodName: String): String;
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

function TBoldModelEditFrm.FindModelMethod(MethodName: String): TUMLOperation;
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

procedure TBoldModelEditFrm.mnuOverrideInAllSubclassesClick(
  Sender: TObject);
var
  UMLOperation: TUMLOperation;
begin
  UMLOperation :=  BoldTreeView1.CurrentElement as TUMLOperation;
  TBoldUMLOperationSupport.OverrideInAllSubclasses(UMLOperation.owner, UMLOperation);
end;

procedure TBoldModelEditFrm.btInterfaceUsesClick(Sender: TObject);
begin
  TfrmUsesEditor.CreateEditorWithParams('Interface Uses Editor', behModel.Value, tbxModelInterfaceUses.BoldProperties.Expression);
end;

procedure TBoldModelEditFrm.btImplementationUsesClick(Sender: TObject);
begin
  TfrmUsesEditor.CreateEditorWithParams('Implementation Uses Editor', behModel.Value, tbxModelImplementationUses.BoldProperties.Expression);
end;

function TBoldModelEditFrm.bsrNiceCRRendererGetAsString(aFollower: TBoldFollower): String;
begin
  if Assigned(aFollower.Value) then
  begin
    Result := aFollower.Value.AsString;
    while Pos(BOLDCRLF, Result) > 0 do
      Delete(Result, Pos(BOLDCRLF, Result), 2);
  end
  else
    Result := '';
end;

procedure TBoldModelEditFrm.bsrNiceCRRendererSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
begin
  aFollower.Element.SubscribeToExpression(aFollower.Controller.Expression, Subscriber, False);
end;

procedure TBoldModelEditFrm.Splitter2CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TBoldModelEditFrm.BoldTreeView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and not BoldTreeView1.IsEditing then
    DeleteXxx1Click(Sender);

  if (Key = VK_F2) and not BoldTreeView1.IsEditing and
     assigned(BoldTreeView1.Selected) then
  begin
    if not BoldTreeView1.Selected.EditText then
      Beep;
  end;
end;

procedure TBoldModelEditFrm.Contents1Click(Sender: TObject);
begin
  Application.HelpContext(0);
end;

procedure TBoldModelEditFrm.BoldTreeView1Expanded(Sender: TObject;
  Node: TTreeNode);
begin
  // to avoid having a empty rigth-side of ModelEdit when started.
  if not Assigned(BoldTreeView1.Selected) then
    BoldTreeView1.Items[0].Selected := True;
end;

function TBoldModelEditFrm.bcrGetSetGetAsCheckBoxState(aFollower: TBoldFollower): TCheckBoxState;
begin
  Result := cbGrayed;
  if Assigned(aFollower.Element) then
    Result := bcrBooleanToCheckBoxGetAsCheckBoxState(aFollower);
end;

procedure TBoldModelEditFrm.bcrGetSetSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
begin
  (aFollower.Element as TUMLAttribute).M_StereotypeName.DefaultSubscribe(Subscriber, breReEvaluate);
  aFollower.Element.SubscribeToExpression(aFollower.Controller.Expression, Subscriber, False);
end;

function TBoldModelEditFrm.bcrGetSetMayModify(aFollower: TBoldFollower): Boolean;
begin
  Result := (aFollower.Element as TUMLAttribute).GetBoldTV(TAG_ATTRIBUTEKIND) = TV_ATTRIBUTEKIND_DELPHI;
end;

procedure TBoldModelEditFrm.bcrBooleanToCheckBoxSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
begin
  aFollower.Element.SubscribeToExpression(aFollower.Controller.Expression, Subscriber, False);
end;

function TBoldModelEditFrm.bcrBooleanToCheckBoxGetAsCheckBoxState(aFollower: TBoldFollower): TCheckBoxState;
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

procedure TBoldModelEditFrm.bcrBooleanToCheckBoxSetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
begin
  aFollower.Value.AsString := BooleanToString(NewValue = cbChecked);
end;

procedure TBoldModelEditFrm.bcrGetSetSetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
begin
  bcrBooleanToCheckBoxSetAsCheckBoxState(aFollower, newValue);
end;

procedure TBoldModelEditFrm.ShowAttribKindFeaturesRecieve(Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  if RequestedEvent = breResubscribe then
  begin
    FShowAttribKindFeaturesSubscriber.CancelAllSubscriptions;
    behAttribute.AddSmallSubscription(FShowAttribKindFeaturesSubscriber, [beValueIdentityChanged ], breResubscribe);
    if Assigned(behAttribute.Value) then
    begin
      if behAttribute.Value is TUMLAttribute then
        TUMLAttribute(behAttribute.Value).SubscribeToExpression('taggedValue[''Bold.AttributeKind''].value', FShowAttribKindFeaturesSubscriber, False);
    end;
  end;
  if behAttribute.Value is TUMLAttribute then
    gbDelphiFeatures.Visible := TUMLAttribute(behAttribute.Value).GetBoldTV(TAG_ATTRIBUTEKIND) = TV_ATTRIBUTEKIND_DELPHI;
end;

procedure TBoldModelEditFrm.btModelConstraintEditorClick(Sender: TObject);
begin
  ShowTheConstraintEditor(brhCurrentElement.Value as TUMLModelElement, Self);
end;

procedure TBoldModelEditFrm.btShowDerivationExpressionsEditorClick(Sender: TObject);
begin
  ShowDerivationExpressionsEditor(behClass.Value.EvaluateExpressionAsDirectElement('taggedValue[''Bold.DerivationExpressions'']') as TUMLTaggedValue);
end;

procedure TBoldModelEditFrm.N3Click(Sender: TObject);
var
  TVEditor: TfrmBoldUMLTaggedValuesEditor;
begin
  TVEditor := TfrmBoldUMLTaggedValuesEditor.Create(self);
  TVEditor.behRoot.RootHandle := brhCurrentElement;
  TVEditor.Show;
end;

procedure TBoldModelEditFrm.FormDestroy(Sender: TObject);
begin
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
end;

procedure TBoldModelEditFrm.btAttributeShowDerivExprEditorClick(Sender: TObject);
var
  Attr: TUMLAttribute;
begin
  Attr := CurrentElement as TUMLAttribute;
  EditOclExpression(CurrentElement, TAG_DERIVATIONOCL, Attr.Owner);
end;

procedure TBoldModelEditFrm.btAssoEndShowDeriExprEditorClick(Sender: TObject);
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

procedure TBoldModelEditFrm.BoldTreeView1Cut(Sender: TObject);
begin
  if (not (BoldTreeView1.CurrentElement is TUMLAssociation)) and
     (not (BoldTreeView1.CurrentElement is TUMLModel)) then
  begin
    if (BoldTreeView1.CurrentElement is TUMLClass) then
    begin
      if CurrentModelIsBoldified and ((BoldTreeView1.CurrentElement as TUMLClass) = TBoldUMLBoldify.GetRootClass(CurrentModel)) then
        Exit;
    end;
    CutOrCopy := cckCut;
    GetCopyPasteRef;
    GreyNode := BoldTreeView1.Selected;
    GreyNode.Cut := True;
  end;
end;

procedure TBoldModelEditFrm.BoldTreeView1Copy(Sender: TObject);
begin
  CutOrCopy := cckCopy;
  GetCopyPasteRef;
end;

procedure TBoldModelEditFrm.BoldTreeView1Paste(Sender: TObject);
begin
  with BoldTreeView1 do
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

procedure TBoldModelEditFrm.BoldTreeView1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  DragObject := TBoldDTDragObject.Create(Sender as TControl);
  if BoldTreeView1.CurrentElement is TBoldObject then
    BoldGuiHandler.DraggedObjects.Add(BoldTreeView1.CurrentElement as TBoldObject);
end;

procedure TBoldModelEditFrm.EditOclExpression(Element: TUMLModelElement; TaggedValue: String; Context: TUMLModelElement);
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

procedure TBoldModelEditFrm.btClassDefaultStringRepClick(Sender: TObject);
begin
  EditOclExpression(CurrentElement, TAG_DEFAULTSTRINGREPRESENTATION, CurrentElement);
end;

procedure TBoldModelEditFrm.ToolButton1Click(Sender: TObject);
begin
  //TBoldSystemDebuggerFrm.CreateWithSystem(nil, (behModel.Value as TBoldObject).BoldSystem).show;
end;

procedure TBoldModelEditFrm.UpdateStatusbar;
var
  InfoText: string;
  HighestSeverity: TSeverity;
begin
  InfoText := '';
  if assigned(behHighestSeverity.Value) then
    HighestSeverity := (behHighestSeverity.Value as TBASeverity).AsSeverity
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
  imgHint.Visible := HighestSeverity = sHint;
  imgWarning.Visible := HighestSeverity in [sWarning, sError];
  Statusbar1.Panels[1].Text := InfoText;
end;

procedure TBoldModelEditFrm.NewDatatype1Click(Sender: TObject);
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

procedure TBoldModelEditFrm.NewPackage1Click(Sender: TObject);
var
  NewPackage: TUMLPackage;
  NameSpace: TUMLNameSpace;
begin
  NameSpace := CurrentElement as TUMLNameSpace;
  NewPackage := TUMLPackage.Create(NameSpace.BoldSystem);
  TBoldUMLSupport.EnsureBoldTaggedValues(NewPackage);
  NewPackage.namespace_ := NameSpace;
  NewPackage.name := TBoldUMLSupport.UniqueName(NewPackage, 'NewPackage');
end;

procedure TBoldModelEditFrm.DroppedOnUMLPackage(Dropped: TUMLModelElement; Target: TUMLPackage);
begin
  if (Dropped is TUMLClassifier) or (Dropped is TUMLPackage) then
   Dropped.namespace_ := Target;
end;

procedure TBoldModelEditFrm.InsertSuperclass1Click(Sender: TObject);
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
end;

function TBoldModelEditFrm.GetUMLObjectToCopyOrMove: TUMLModelElement;
begin
  Result := brhCopyCut.Value as TUMLModelElement;
end;

procedure TBoldModelEditFrm.SetUMLObjectToCopyOrMove(
  const Value: TUMLModelElement);
begin
   brhCopyCut.Value := Value;
end;

function TBoldModelEditFrm.GetCurrentModelIsBoldified: Boolean;
begin
  Result := (behBoldified.Value as TBABoolean).AsBoolean = true;
end;

function TBoldModelEditFrm.bcrAutoCreatedGetAsCheckBoxState(aFollower: TBoldFollower): TCheckBoxState;
begin
  if Assigned(aFollower.Element) and TBoldUMLBoldify.IsAutocreated(aFollower.Element as TUMLModelElement) then
    Result := cbChecked
  else
    Result := cbUnchecked
end;

procedure TBoldModelEditFrm.bcrAutoCreatedSetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
begin
  if assigned(aFollower.Element) and (newValue= cbUnchecked) then
    TBoldUMLBoldify.RemoveBoldifyTaggedValue(aFollower.Element as TUMLModelElement, TAG_AUTOCREATED);
end;

procedure TBoldModelEditFrm.bsrRedOnAutocreatedSetColor(aFollower: TBoldFollower; var AColor: TColor);
begin
  if Assigned(aFollower.Element) and TBoldUMLBoldify.IsAutocreated(aFollower.Element as TUMLModelElement) then
    aColor := clRed;
end;

function TBoldModelEditFrm.GetIsExecutingPlugin: boolean;
begin
  result := fIsExecutingPlugin;
end;

procedure TBoldModelEditFrm.SetIsExecutingPlugin(Value: boolean);
begin
  fIsExecutingPlugIn := value;
end;

procedure TBoldModelEditFrm.SetLoadedFrom(const Value: string);
begin
  fLoadedFrom := Value;
end;

procedure TBoldModelEditFrm.Boldifymodel1Click(Sender: TObject);
var
  CursorGuard: IBoldCursorGuard;
begin
  IgnoreModelChanges := True;
  CursorGuard := TBoldCursorGuard.Create;
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
  end;
end;

procedure TBoldModelEditFrm.SetBoldifyFlattenCaptions;
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

procedure TBoldModelEditFrm.FlattenClick(Sender: TObject);
begin
  IgnoreModelChanges := True;
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
  end;
end;

procedure TBoldModelEditFrm.Loggform1Click(Sender: TObject);
begin
  BoldLogForm.Show;
end;

procedure TBoldModelEditFrm.Tools1Click(Sender: TObject);
begin
  SetBoldifyFlattenCaptions;
end;

procedure TBoldModelEditFrm.cmbSuperClassSelectChanged(Sender: TObject);
begin
  with Sender as TBoldComboBox do
  begin
    if BoldHandle.Value is TUMLClassifier then
       TUMLClassifier(BoldHandle.Value).setfirstParent(SelectedElement as TUMLClassifier);
  end;
end;

procedure TBoldModelEditFrm.EnsureFlattenedAndBoldified;
var
  Boldifier: TBoldUMLBoldify;
begin
  if CurrentModel = nil then
    Exit;
  Boldifier := ModelHandle.Boldify;

  if not TBoldUMLSupport.IsFlattened(CurrentModel) or not Boldifier.IsBoldified(CurrentModel) then
  begin
    IgnoreModelChanges := True;
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
    end;
  end;
end;

procedure TBoldModelEditFrm.EnsureUnFlattenedAndUnBoldified;
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

procedure TBoldModelEditFrm.bdhAttributePMapperNamesDeriveAndSubscribe(
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

procedure TBoldModelEditFrm.Clear1Click(Sender: TObject);
begin
  if Assigned(CurrentModel) and
    (MessageDlg('This will remove everything from your model, are you sure?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    CurrentModel.Clear;
end;

procedure TBoldModelEditFrm.SubscribeToModelChanges;
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

procedure TBoldModelEditFrm.SetModelNeedsValidation(const Value: Boolean);
begin
  if value <> fModelNeedsValidation then
  begin
    FModelNeedsValidation := Value;
    UpdateStatusBar;
    SubscribeToModelChanges;
  end;
end;

procedure TBoldModelEditFrm.SetIgnoreModelChanges(const Value: Boolean);
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

procedure TBoldModelEditFrm.ApplyGUI;
begin
  TBoldQueueable.ApplyAll;
end;

function TBoldModelEditFrm.bcrBooleanToCheckBoxMayModify(aFollower: TBoldFollower): Boolean;
var
  ValueElement: TBoldElement;
begin
  ValueElement := aFollower.Value;
  if Assigned(ValueElement) then
    Result := ValueElement.ObserverMayModify(aFollower.subscriber)
  else
    Result := false;
end;

function TBoldModelEditFrm.GetElementToPaste: TUMLModelElement;
begin
  Result := nil;
  case CutOrCopy of
    cckCopy:
      Result := TBoldCopyAndClone.BoldClone(UMLObjectToCopyOrMove, bcmDeep) as TUMLModelElement;
    cckCut:
      Result := UMLObjectToCopyOrMove;
  end;
end;

procedure TBoldModelEditFrm.BoldTreeView1EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  BoldGuiHandler.DraggedObjects.Clear;
end;

procedure TBoldModelEditFrm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ApplyGUI;
  Action := caFree;
end;

procedure TBoldModelEditFrm.FormActivate(Sender: TObject);
begin
  G_ValidationFormDefaultOwner := Self;
  apeHintCatcher.OnHint := apeHintCatcherHint;
end;

procedure TBoldModelEditFrm.AddSubclass1Click(Sender: TObject);
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
end;

function TBoldModelEditFrm.GetCurrentClass: TUMLClass;
begin
  if CurrentElement is TUMLClass then
    result := CurrentElement as TUMLClass
  else if (CurrentElement is TUMLFeature) and ((CurrentElement as TUMLfeature).owner is TUMLClass) then
    result := (CurrentElement as TUMLfeature).owner as TUMLClass
  else
    result := nil;
end;

function TBoldModelEditFrm.GetCurrentPackage: TUMLNameSpace;
begin
  if CurrentElement is TUMLClass then
    result := (CurrentElement as TUMLClass).namespace_
  else if CurrentElement is TUMLAssociation then
    result := (CurrentElement as TUMLAssociation).namespace_
  else if CurrentElement is TUMLAttribute then
    result := (CurrentElement as TUMLAttribute).owner.namespace_
  else if CurrentElement is TUMLNameSpace then
    result := CurrentElement as TUMLNameSpace
  else
    result := nil;
end;

procedure TBoldModelEditFrm.FormDeactivate(Sender: TObject);
begin
  TBoldQueueable.ApplyAllMatching(self);
  apeHintCatcher.OnHint := nil;
end;

function TBoldModelEditFrm.GetCurrentTypeNameDictionary: TBoldTypeNameDictionary;
begin
  if assigned(ModelHandle) then
    result := ModelHandle.TypeNameDictionary
  else
    result := nil;
end;

procedure TBoldModelEditFrm.SetModelHandle(const Value: TBoldModel);
begin
  if FModelHandle <> Value then
  begin
    TBoldQueueable.DeActivateDisplayQueue;
    Value.FreeNotification(Self);
    fModelHandle := Value;
    brhTreeRoot.Value := ModelHandle.EnsuredUMLModel;
    if assigned(CurrentModel) then
    begin
      EnsureTypesFromTypeNameHandle(nil);
      TBoldUMLSupport.EnsureBoldTaggedVAlues(CurrentModel);
    end
    else
      raise EBoldInternal.CreateFmt('%s.SetRoot: Root must be part of Model', [ClassName]);
    ModelNeedsValidation := True;
    EnsureValidationForm(GetCurrentModelHandle, self, JumpToElement);
    TBoldQueueable.ActivateDisplayQueue;
  end;
end;

procedure TBoldModelEditFrm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (aComponent = fModelHandle) and (Operation = opRemove) then
    Free;
end;

function TBoldModelEditFrm.GetCurrentModelHandle: TBoldModel;
begin
  Result := fModelHandle;
end;

procedure TBoldModelEditFrm.apeHintCatcherHint(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := GetLongHint(Application.Hint);

end;

procedure TBoldModelEditFrm.SetCheckBoxHints;
var
  i: integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TBoldCheckBox then
      TBoldCheckBox(Components[i]).Hint := StripHotkey(TBoldCheckBox(Components[i]).Caption);
end;

initialization
  BoldUMLModelEdit.BoldModelEditFrmClass := TBoldModelEditFrm;

end.
