unit cxBoldEditors;

{$ASSERTIONS ON}
{$INCLUDE Bold.inc}

{$DEFINE Constraints}

{.$DEFINE BoldDevExLog}

{.$DEFINE LOGCHANGES}

(*
  cxBoldEditors v2.60 - 26 July 2017
  2007-2017 Daniel Mauric
*)

interface

uses
  Classes,
  SysUtils,

  cxClasses,
  cxControls,
  cxEdit,
  cxTextEdit,
  cxDropDownEdit,
  cxCalendar,
  cxTimeEdit,
  cxMemo,
  cxCurrencyEdit,
  cxMaskEdit,
  cxCheckBox,
  cxSpinEdit,
  cxButtonEdit,
  cxHyperLinkEdit,
  cxProgressBar,
  cxBarEditItem,
  dxBar,
  cxContainer,
  {$IFDEF DevExScheduler}
  cxDateNavigator,
  {$ENDIF}
  cxLabel,
  cxImage,
  cxRichEdit,
  cxCheckListBox,
  cxListBox,
  cxListView, ComCtrls,
  cxDataUtils,

  cxLookAndFeelPainters, // for  TcxCheckBoxState = (cbsUnchecked, cbsChecked, cbsGrayed);
  StdCtrls, // for    TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);
  cxGraphics,
  Controls,// for TCaption
  Messages, // for TMessage
  Windows,

  BoldSubscription,
  BoldVariantControlPack,
  BoldCheckboxStateControlPack,
  BoldControllerListControlPack,
  BoldElementHandleFollower,
  BoldControlPack,
  BoldHandles,
  BoldElements,
  BoldControlPackDefs,
  BoldSystem,
  BoldSystemRT,
  BoldListHandleFollower,
  BoldListListControlPack,
  BoldControlsDefs,
  BoldDefs,
  BoldAbstractListHandle,
  BoldComponentvalidator;

type
  TcxCustomBoldEditDefaultValuesProvider = class;
  TcxBoldEditDataBinding = class;
  TBoldComboListController = class;
  TcxCustomBoldTextEditProperties = class;
  TcxBoldTextEditProperties = class;
  TcxBoldTextEdit = class;
  TcxBoldDateEdit = class;

  IcxBoldEditProperties = Interface
  ['{D50859F1-F550-4CE6-84DE-5074921512E5}']
    procedure SetStoredValue(aValue: Variant; aBoldHandle: TBoldElementHandle; aEdit: TcxCustomEdit; aFollower: TBoldFollower; var aDone: boolean);
    function BoldElementToEditValue(aFollower: TBoldFollower; aElement: TBoldElement; aEdit: TcxCustomEdit): variant;
    function CanEdit(aBoldHandle: TBoldElementHandle; aFollower: TBoldFollower): boolean;
  end;

  TBoldComboListController = class(TBoldAbstractListAsFollowerListController)
  published
     property NilElementMode;
  end;

  TcxCustomBoldTextEditProperties = class(TcxCustomTextEditProperties, IcxBoldEditProperties, IBoldValidateableComponent)
  private
    fListHandleFollower: TBoldListHandleFollower;
    fBoldListProperties: TBoldAbstractListAsFollowerListController; // TBoldComboListController;
    fBoldRowProperties: TBoldVariantFollowerController;
    fBoldSelectChangeAction: TBoldComboSelectChangeAction;
    fBoldSetValueExpression: TBoldExpression;
    function GetBoldListHandle: TBoldAbstractListHandle;
    function GetListFollower: TBoldFollower;
    procedure SetBoldListHandle(const Value: TBoldAbstractListHandle);
    procedure SetBoldListProperties(const Value: TBoldAbstractListAsFollowerListController);
    procedure SetRowProperties(const Value: TBoldVariantFollowerController);
    function GetContextForBoldRowProperties: TBoldElementTypeInfo;
    procedure SetBoldSelectChangeAction(Value: TBoldComboSelectChangeAction);
    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;
    // IcxBoldEditProperties
    procedure SetStoredValue(aValue: Variant; aBoldHandle: TBoldElementHandle; aEdit: TcxCustomEdit; aFollower: TBoldFollower; var aDone: boolean);
    function BoldElementToEditValue(aFollower: TBoldFollower; aElement: TBoldElement; aEdit: TcxCustomEdit): variant;
    function CanEdit(aBoldHandle: TBoldElementHandle; aFollower: TBoldFollower): boolean;
    procedure SetBoldSetValueExpression(const Value: TBoldExpression);
  protected
    function IsNilRepresentation(AValue: Variant): boolean;  
    procedure _InsertItem(Index: Integer; Follower: TBoldFollower);
    procedure _ReplaceItem(Index: Integer; Follower: TBoldFollower);
    procedure _DeleteItem(Index: Integer; OwningFollower: TBoldFollower);
    procedure _RowAfterMakeUptoDate(Follower: TBoldFollower);
    procedure _BeforeMakeUptoDate(Follower: TBoldFollower);
    procedure _AfterMakeUptoDate(Follower: TBoldFollower);
    property BoldLookupListHandle: TBoldAbstractListHandle read GetBoldListHandle write SetBoldListHandle;
    property BoldLookupListProperties: TBoldAbstractListAsFollowerListController read fBoldListProperties write SetBoldListProperties;
    property BoldRowProperties: TBoldVariantFollowerController read fBoldRowProperties write SetRowProperties;
    property BoldSelectChangeAction: TBoldComboSelectChangeAction read fBoldSelectChangeAction write SetBoldSelectChangeAction default bdcsSetValue;
    property BoldSetValueExpression: TBoldExpression read fBoldSetValueExpression write SetBoldSetValueExpression;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    class function GetContainerClass: TcxContainerClass; override;
    function IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    property LookupListFollower: TBoldFollower read GetListFollower;
  end;

  TcxBoldComboBoxProperties = class(TcxCustomComboBoxProperties, IcxBoldEditProperties, IBoldValidateableComponent)
  private
    fListHandleFollower: TBoldListHandleFollower;
    fBoldListProperties: TBoldComboListController;
    fBoldRowProperties: TBoldVariantFollowerController;
    fBoldSelectChangeAction: TBoldComboSelectChangeAction;
    fBoldSetValueExpression: TBoldExpression;
    function GetBoldListHandle: TBoldAbstractListHandle;
    function GetListFollower: TBoldFollower;
    procedure SetBoldListHandle(const Value: TBoldAbstractListHandle);
    procedure SetBoldListProperties(const Value: TBoldComboListController);
    procedure SetRowProperties(const Value: TBoldVariantFollowerController);
    function GetContextForBoldRowProperties: TBoldElementTypeInfo;
    procedure SetBoldSelectChangeAction(Value: TBoldComboSelectChangeAction);
    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;
    // IcxBoldEditProperties
    procedure SetStoredValue(aValue: Variant; aBoldHandle: TBoldElementHandle; aEdit: TcxCustomEdit; aFollower: TBoldFollower; var aDone: boolean);
    function BoldElementToEditValue(aFollower: TBoldFollower; aElement: TBoldElement; aEdit: TcxCustomEdit): variant;
    function CanEdit(aBoldHandle: TBoldElementHandle; aFollower: TBoldFollower): boolean;
    procedure SetBoldSetValueExpression(const Value: TBoldExpression);
  protected
    procedure _InsertItem(Index: Integer; Follower: TBoldFollower);
    procedure _ReplaceItem(Index: Integer; Follower: TBoldFollower);
    procedure _DeleteItem(Index: Integer; OwningFollower: TBoldFollower);
    procedure _RowAfterMakeUptoDate(Follower: TBoldFollower);
    procedure _BeforeMakeUptoDate(Follower: TBoldFollower);
    procedure _AfterMakeUptoDate(Follower: TBoldFollower);
    function GetAlwaysPostEditValue: Boolean; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    function IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    function IsDisplayValueValid(var DisplayValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    property LookupListFollower: TBoldFollower read GetListFollower;
  published
    property BoldLookupListHandle: TBoldAbstractListHandle read GetBoldListHandle write SetBoldListHandle;
    property BoldLookupListProperties: TBoldComboListController read fBoldListProperties write SetBoldListProperties;
    property BoldRowProperties: TBoldVariantFollowerController read fBoldRowProperties write SetRowProperties;
    property BoldSelectChangeAction: TBoldComboSelectChangeAction read fBoldSelectChangeAction write SetBoldSelectChangeAction default bdcsSetValue;
    property BoldSetValueExpression: TBoldExpression read fBoldSetValueExpression write SetBoldSetValueExpression;

    property Alignment;
    property AssignedValues;
    property AutoSelect;
    property BeepOnError;
    property ButtonGlyph;
    property CaseInsensitive;
    property CharCase;
    property ClearKey;
    property DropDownAutoWidth;
    property DropDownListStyle;
    property DropDownRows;
    property DropDownSizeable;
    property DropDownWidth;
    property HideSelection;
    property IgnoreMaskBlank;
    property ImeMode;
    property ImeName;
    property ImmediateDropDown;
//    property ImmediatePost;
    property ImmediateUpdateText;
    property IncrementalSearch;
    property ItemHeight;
    property MaskKind;
    property EditMask;
    property MaxLength;
    property OEMConvert;
    property PopupAlignment;
    property PostPopupValueOnTab;
    property ReadOnly;
    property Revertable;
//    property Sorted;
    property UseLeftAlignmentOnEditing;
    property ValidateOnEnter;
    property ValidationOptions;
    property OnChange;
    property OnCloseUp;
    property OnDrawItem;
    property OnEditValueChanged;
    property OnInitPopup;
    property OnMeasureItem;
    property OnNewLookupDisplayText;
    property OnPopup;
    property OnValidate;
  end;

  TcxSingleLinkEditProperties = class(TcxCustomHyperLinkEditProperties {TcxCustomHyperLinkEditProperties})
  published
    property Alignment;
    property AssignedValues;
//    property AutoComplete; // deprecated
    property AutoSelect;
    property ClearKey;
    property ImeMode;
    property ImeName;
    property IncrementalSearch;
    property LinkColor;
    property LookupItems;
//    property LookupItemsSorted;
//    property Prefix;
    property ReadOnly;
    property StartKey;
    property SingleClick;
    property UseLeftAlignmentOnEditing;
//    property UsePrefix;
    property ValidateOnEnter;
    property ValidationOptions;    
    property OnChange;
    property OnEditValueChanged;
    property OnStartClick;
    property OnValidate;
  end;


  TcxBoldTextEditProperties = class(TcxCustomBoldTextEditProperties)
  published
    property BoldLookupListHandle;
    property BoldLookupListProperties;
    property BoldRowProperties;
    property BoldSelectChangeAction;
    property BoldSetValueExpression;
    property Alignment;
    property AssignedValues;
    property AutoSelect;
    property BeepOnError;
    property CharCase;
    property ClearKey;
    property EchoMode;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property IncrementalSearch;
    property OEMConvert;
    property PasswordChar;
    property ReadOnly;
    property UseLeftAlignmentOnEditing;

    property ValidateOnEnter;
    property ValidationOptions;    
    property OnChange;
    property OnEditValueChanged;
    property OnNewLookupDisplayText;
    property OnValidate;
  end;

  TcxCustomBoldEditDefaultValuesProvider = class(TcxCustomEditDefaultValuesProvider)
  private
    fBoldHandleFollower: TBoldAbstractHandleFollower; // handle follower is needed if we end up needing to access follower
    fBoldProperties: TBoldFollowerController;
//    fcxBoldEditDataBinding: TcxBoldEditDataBinding;

// TODO: Place subscriptions instead of FreeNotification
//    procedure FreeNotification(Sender: TComponent);

    function GetFollower: TBoldFollower;
//    procedure SetBoldHandle(const Value: TBoldElementHandle);
    procedure SetBoldProperties(const Value: TBoldFollowerController);
    procedure SetHandleFollower(const Value: TBoldAbstractHandleFollower);
    function GetBoldHandle: TBoldElementHandle;
  protected
    function GetBoldElementTypeInfo: TBoldElementTypeInfo;
//    property DataBinding: TcxBoldEditDataBinding read fcxBoldEditDataBinding;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
//    function CanSetEditMode: Boolean; override;
    function DefaultAlignment: TAlignment; override;
    function DefaultBlobKind: TcxBlobKind; override;
    function DefaultCanModify: Boolean; override;
//    function DefaultDisplayFormat: string; override;
//    function DefaultEditFormat: string; override;
//    function DefaultEditMask: string; override;
    function DefaultIsFloatValue: Boolean; override;
    function DefaultMaxLength: Integer; override;
//    function DefaultMaxValue: Double; override;
//    function DefaultMinValue: Double; override;
//    function DefaultPrecision: Integer; override;
//    function DefaultReadOnly: Boolean; override;
//    function DefaultRequired: Boolean; override;
    function IsBlob: Boolean; override;
    function IsCurrency: Boolean; override;
    function IsDataAvailable: Boolean; override;
//    function IsDataStorage: Boolean; override;
//    function IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean; override;
//    function IsOnGetTextAssigned: Boolean; override;
//    function IsOnSetTextAssigned: Boolean; override;
    function IsValidChar(AChar: Char): Boolean; override;

    property Follower: TBoldFollower read GetFollower;
    property BoldHandle: TBoldElementHandle read GetBoldHandle{ write SetBoldHandle};
    property BoldProperties: TBoldFollowerController read fBoldProperties write SetBoldProperties;
    property BoldHandleFollower: TBoldAbstractHandleFollower read fBoldHandleFollower write SetHandleFollower;
  end;

  TcxBoldEditDataBinding = class(TcxEditDataBinding)
  private
    fInternalChange: integer;
    fBoldHandleFollower: TBoldElementHandleFollower;
    fCurrentElementType: TBoldElementTypeInfo;
    fBoldFollowerController: TBoldFollowerController;
    fValueOrDefinitionInvalid: boolean;
    fBrokenConstraints: TStringList;
    fShowHintIfCaptionDoesntFit: boolean;
    procedure SetBoldHandle(const Value: TBoldElementHandle);
    function GetFollower: TBoldFollower;
    function GetBoldHandle: TBoldElementHandle;
    function GetDefaultValuesProvider: TcxCustomBoldEditDefaultValuesProvider;

    procedure SetBoldProperties(const Value: TBoldVariantFollowerController);
    function GetBoldProperties: TBoldVariantFollowerController;

{$IFDEF Constraints}
    procedure SubscribeToConstraints(aElement: TBoldElement);
{$ENDIF}
    function GetValueOrDefinitionInvalid: boolean;
    procedure SetValueOrDefinitionInvalid(const Value: boolean);
    function GetIsInInternalChange: boolean;
    function GetBrokenConstraints: TStringList;
    function GetHasBrokenConstraints: boolean;
  protected
    // IBoldOCLComponent
    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetVariableList: TBoldExternalVariableList;
    function GetExpression: TBoldExpression;

    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;

    procedure _AfterMakeUptoDate(Follower: TBoldFollower); virtual;

    function ValidateTypeConforms(aExpressionType: TBoldElementTypeInfo): string; virtual;

    function MayModify: boolean; virtual;
    procedure TypeMayHaveChanged;
    procedure DoChanged; virtual;

    property BoldFollowerController: TBoldFollowerController read fBoldFollowerController;

    procedure InternalSetValue(const aValue: TcxEditValue); virtual;
    function InternalGetValue(Follower: TBoldFollower): Variant;
    function ImmediatePost: boolean; virtual;

    procedure ValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean);

    function HandleApplyException(E: Exception; Elem: TBoldElement; var Discard: Boolean): Boolean;
    procedure DefaultValuesChanged; override;
    function GetModified: Boolean; override;
    function GetStoredValue: TcxEditValue; override;
    function IsRefreshDisabled: Boolean;
    procedure Reset; override;
    procedure SetStoredValue(const Value: TcxEditValue); override;
    procedure DataChanged; virtual;
    procedure DataSetChange; virtual;
    procedure EditingChanged; virtual;
    function IsLookupControl: Boolean; virtual;
    procedure UpdateData; virtual;
    property DefaultValuesProvider: TcxCustomBoldEditDefaultValuesProvider read GetDefaultValuesProvider;
    procedure DoEnter; virtual;
    procedure DoExit; virtual;
    property ValueOrDefinitionInvalid: boolean read GetValueOrDefinitionInvalid write SetValueOrDefinitionInvalid;
    property BrokenConstraints: TStringList read GetBrokenConstraints;
    property HasBrokenConstraints: boolean read GetHasBrokenConstraints;
  public
    constructor Create(AEdit: TcxCustomEdit); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CanCheckEditorValue: Boolean; override;
    function CanModify: Boolean; override;
    function CanPostEditorValue: Boolean; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    class function GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass; override;
    procedure SetModified; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure UpdateDisplayValue; override;
    property Follower: TBoldFollower read GetFollower;
    property IsInInternalChange: boolean read GetIsInInternalChange;
  published
    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
//    property BoldHandleFollower: TBoldElementHandleFollower read fBoldHandleFollower write SetBoldHandleFollower;
//    property BoldProperties: TBoldVariantFollowerController read fBoldProperties write SetBoldProperties;
    property BoldProperties: TBoldVariantFollowerController read GetBoldProperties write SetBoldProperties;
    property ShowHintIfCaptionDoesntFit: boolean read fShowHintIfCaptionDoesntFit write fShowHintIfCaptionDoesntFit default false;
  end;

  TcxBoldTextEditDataBinding = class(TcxBoldEditDataBinding)
  end;

  TcxBoldDateTimeEditDataBinding = class(TcxBoldEditDataBinding)
  protected
    function ValidateTypeConforms(aExpressionType: TBoldElementTypeInfo): string; override;
  end;

  TcxBoldTimeEditDataBinding = class(TcxBoldEditDataBinding)
  protected
    function ValidateTypeConforms(aExpressionType: TBoldElementTypeInfo): string; override;
  end;

  TcxBoldCheckBoxEditDataBinding = class(TcxBoldEditDataBinding)
  protected
    function ValidateTypeConforms(aExpressionType: TBoldElementTypeInfo): string; override;
    function ImmediatePost: boolean; override;
  public
    function MayModify: boolean; override; // TODO: move this up to TcxBoldEditDataBinding
  end;

  TcxBoldNumericEditDataBinding = class(TcxBoldEditDataBinding)
  protected
    function ValidateTypeConforms(aExpressionType: TBoldElementTypeInfo): string; override;
  end;

  TcxBoldFloatEditDataBinding = class(TcxBoldEditDataBinding)
  protected
    function ValidateTypeConforms(aExpressionType: TBoldElementTypeInfo): string; override;
  end;

  TcxBoldCurrencyEditDataBinding = class(TcxBoldEditDataBinding)
  protected
    function ValidateTypeConforms(aExpressionType: TBoldElementTypeInfo): string; override;
  end;

  TcxBoldBlobEditDataBinding = class(TcxBoldEditDataBinding)
  protected
    function ValidateTypeConforms(aExpressionType: TBoldElementTypeInfo): string; override;
  end;

  TcxBoldComboBoxEditDataBinding = class(TcxBoldTextEditDataBinding)
  protected
    function GetModified: Boolean; override;
    function ImmediatePost: boolean; override;
  public
    constructor Create(AEdit: TcxCustomEdit); override;  
  end;

  TcxBoldTextEdit = class(TcxCustomTextEdit, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxBoldTextEditProperties;
    function GetProperties: TcxBoldTextEditProperties;
    procedure SetProperties(Value: TcxBoldTextEditProperties);
    function GetDataBinding: TcxBoldTextEditDataBinding;
    procedure SetDataBinding(Value: TcxBoldTextEditDataBinding);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    function ValidateKeyDown(var Key: Word; Shift: TShiftState): Boolean; override;
    function ValidateKeyPress(var Key: Char): Boolean; override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean); override;
    procedure DoOnChange; override;
    procedure DoChange; override;
    procedure Paint; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxBoldTextEditProperties read GetActiveProperties;
    procedure DoEnter; override;
    procedure DoExit; override;
  published
    property DataBinding: TcxBoldTextEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
    property Properties: TcxBoldTextEditProperties read GetProperties write SetProperties;
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
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
{$IFDEF DELPHI12}
    property TextHint;
{$ENDIF}
    property Visible;
    property OnClick;
{$IFDEF DELPHI5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;
{
  TcxCustomBoldDateEditProperties = class(TcxCustomDateEditProperties)
  end;

  TcxBoldDateEditProperties = class(TcxCustomBoldDateEditProperties)
  end;
}

  TcxBoldDateEdit = class(TcxCustomDateEdit, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxDateEditProperties;
    function GetProperties: TcxDateEditProperties;
    procedure SetProperties(Value: TcxDateEditProperties);
    function GetDataBinding: TcxBoldDateTimeEditDataBinding;
    procedure SetDataBinding(Value: TcxBoldDateTimeEditDataBinding);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    function ValidateKeyDown(var Key: Word; Shift: TShiftState): Boolean; override;
    function ValidateKeyPress(var Key: Char): Boolean; override;
    procedure DoChange; override;
    procedure DoOnChange; override;
    function CanDropDown: Boolean; override;
    procedure Paint; override;
    procedure HidePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason); override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean); override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxDateEditProperties read GetActiveProperties;
    procedure DoEnter; override;    
    procedure DoExit; override;
  published
    property DataBinding: TcxBoldDateTimeEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
    property Properties: TcxDateEditProperties read GetProperties write SetProperties;
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
//    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
//    property Date;
    property DragMode;
//    property EditValue;
    property Enabled;
    property ImeMode;
    property ImeName;
//    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop default True;
{$IFDEF DELPHI12}
    property TextHint;
{$ENDIF}
    property Visible;
    property OnClick;
  {$IFDEF DELPHI5}
    property OnContextPopup;
  {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
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
    property OnStartDrag;
    property OnStartDock;
  end;

  TcxBoldMemo = class(TcxCustomMemo, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxMemoProperties;
    function GetDataBinding: TcxBoldTextEditDataBinding;
    function GetProperties: TcxMemoProperties;
    procedure SetDataBinding(Value: TcxBoldTextEditDataBinding);
    procedure SetProperties(Value: TcxMemoProperties);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure DoChange; override;
    procedure Paint; override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean); override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxMemoProperties read GetActiveProperties;
    procedure DoEnter; override;    
    procedure DoExit; override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DataBinding: TcxBoldTextEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
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
    property Properties: TcxMemoProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
{$IFDEF DELPHI12}
    property TextHint;
{$ENDIF}
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TcxBoldTimeEdit = class(TcxCustomTimeEdit, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxTimeEditProperties;
    function GetDataBinding: TcxBoldTimeEditDataBinding;
    function GetProperties: TcxTimeEditProperties;
    procedure SetDataBinding(Value: TcxBoldTimeEditDataBinding);
    procedure SetProperties(Value: TcxTimeEditProperties);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure DoChange; override;
    function ValidateKeyDown(var Key: Word; Shift: TShiftState): Boolean; override;
    function ValidateKeyPress(var Key: Char): Boolean; override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean); override;
    procedure Paint; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxTimeEditProperties read GetActiveProperties;
    procedure DoEnter; override;    
    procedure DoExit; override;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxBoldTimeEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
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
    property Properties: TcxTimeEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
{$IFDEF DELPHI12}
    property TextHint;
{$ENDIF}
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TcxBoldCurrencyEdit = class(TcxCustomCurrencyEdit, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxCurrencyEditProperties;
    function GetDataBinding: TcxBoldCurrencyEditDataBinding;
    function GetProperties: TcxCurrencyEditProperties;
    procedure SetDataBinding(Value: TcxBoldCurrencyEditDataBinding);
    procedure SetProperties(Value: TcxCurrencyEditProperties);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean); override;
    procedure DoChange; override;
    procedure Paint; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCurrencyEditProperties read GetActiveProperties;
    procedure DoEnter; override;
    procedure DoExit; override;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxBoldCurrencyEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
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
    property Properties: TcxCurrencyEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
{$IFDEF DELPHI12}
    property TextHint;
{$ENDIF}
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditing;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
  end;

  TcxBoldMaskEdit = class(TcxCustomMaskEdit, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxMaskEditProperties;
    function GetDataBinding: TcxBoldTextEditDataBinding;
    function GetProperties: TcxMaskEditProperties;
    procedure SetDataBinding(Value: TcxBoldTextEditDataBinding);
    procedure SetProperties(Value: TcxMaskEditProperties);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    function SupportsSpelling: Boolean; override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean); override;
    procedure Paint; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxMaskEditProperties read GetActiveProperties;
    procedure DoEnter; override;
    procedure DoExit; override;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxBoldTextEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
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
    property Properties: TcxMaskEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
{$IFDEF DELPHI12}
    property TextHint;
{$ENDIF}
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  TcxBoldCheckBox = class(TcxCustomCheckBox, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxCheckBoxProperties;
    function GetDataBinding: TcxBoldCheckBoxEditDataBinding;
    function GetProperties: TcxCheckBoxProperties;
    procedure SetDataBinding(Value: TcxBoldCheckBoxEditDataBinding);
    procedure SetProperties(Value: TcxCheckBoxProperties);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean); override;
    procedure Toggle; override;
    procedure Paint; override;
    procedure Initialize; override;
    procedure DoChange; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCheckBoxProperties read GetActiveProperties;
    procedure DoEnter; override;
    procedure DoExit; override;
    property Checked;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property Caption;
    property Constraints;
    property DataBinding: TcxBoldCheckBoxEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBackground;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxCheckBoxProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
//    property TextHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  TcxBoldComboBox = class(TcxCustomComboBox, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxBoldComboBoxProperties;
    function GetDataBinding: TcxBoldComboBoxEditDataBinding;
    function GetProperties: TcxBoldComboBoxProperties;
    procedure SetDataBinding(Value: TcxBoldComboBoxEditDataBinding);
    procedure SetProperties(Value: TcxBoldComboBoxProperties);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean); override;
    function SupportsSpelling: Boolean; override;
    function CanDropDown: Boolean; override;
    procedure Paint; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxBoldComboBoxProperties read GetActiveProperties;
    procedure DoEnter; override;
    procedure DoExit; override;
    property ItemIndex;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxBoldComboBoxEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
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
    property Properties: TcxBoldComboBoxProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
{$IFDEF DELPHI12}
    property TextHint;
{$ENDIF}
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  // perhaps use TBoldIntegerFollowerController ?
  TcxBoldSpinEdit = class(TcxCustomSpinEdit, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxSpinEditProperties;
    function GetProperties: TcxSpinEditProperties;
    function GetDataBinding: TcxBoldNumericEditDataBinding;
    procedure SetDataBinding(Value: TcxBoldNumericEditDataBinding);
    procedure SetProperties(Value: TcxSpinEditProperties);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean); override;
    procedure Paint; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxSpinEditProperties read GetActiveProperties;
    procedure DoEnter; override;
    procedure DoExit; override;
    property Value;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxBoldNumericEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
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
    property Properties: TcxSpinEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
{$IFDEF DELPHI12}
    property TextHint;
{$ENDIF}
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TcxBoldButtonEdit = class(TcxCustomButtonEdit, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxButtonEditProperties;
    function GetDataBinding: TcxBoldTextEditDataBinding;
    function GetProperties: TcxButtonEditProperties;
    procedure SetDataBinding(Value: TcxBoldTextEditDataBinding);
    procedure SetProperties(Value: TcxButtonEditProperties);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean); override;
    procedure Paint; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxButtonEditProperties read GetActiveProperties;
    procedure DoEnter; override;
    procedure DoExit; override;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxBoldTextEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
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
    property Properties: TcxButtonEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
{$IFDEF DELPHI12}
    property TextHint;
{$ENDIF}
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

  TcxBoldHyperLinkEdit = class(TcxCustomHyperLinkEdit, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxHyperLinkEditProperties;
    function GetDataBinding: TcxBoldTextEditDataBinding;
    function GetProperties: TcxHyperLinkEditProperties;
    procedure SetDataBinding(Value: TcxBoldTextEditDataBinding);
    procedure SetProperties(Value: TcxHyperLinkEditProperties);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean); override;
    procedure Paint; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxHyperLinkEditProperties read GetActiveProperties;
    procedure DoEnter; override;
    procedure DoExit; override;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DataBinding: TcxBoldTextEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
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
    property Properties: TcxHyperLinkEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
{$IFDEF DELPHI12}
    property TextHint;
{$ENDIF}
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditing;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
  end;

  TcxBoldProgressBar = class(TcxCustomProgressBar, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxProgressBarProperties;
    function GetDataBinding: TcxBoldNumericEditDataBinding;
    procedure SetDataBinding(Value: TcxBoldNumericEditDataBinding);
    function GetProperties: TcxProgressBarProperties;
    procedure SetProperties(Value: TcxProgressBarProperties);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure Initialize; override;
    procedure Paint; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxProgressBarProperties read GetActiveProperties;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Constraints;
    property DataBinding: TcxBoldNumericEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxProgressBarProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnContextPopup;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TcxBarBoldEditItem = class(TcxCustomBarEditItem)
  private
    fInternalChange: integer;
    fBoldHandleFollower: TBoldElementHandleFollower;
    fBoldProperties: TBoldVariantFollowerController;
    procedure SetBoldHandle(const Value: TBoldElementHandle);
    function GetFollower: TBoldFollower;
    function GetBoldHandle: TBoldElementHandle;
    procedure SetBoldProperties(const Value: TBoldVariantFollowerController);
  protected
    function GetContextType: TBoldElementTypeInfo;
    procedure _AfterMakeUptoDate(Follower: TBoldFollower);
//    function CanEdit: Boolean; override;
//    procedure DoEditValueChanged(Sender: TObject); override;
    procedure EditValueChanged(Sender: TObject);
    procedure EditExit(Sender: TObject);
    function GetControlClass(AIsVertical: Boolean): TdxBarItemControlClass; override;

    procedure KeyPress(var Key: Char); override;

    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Follower: TBoldFollower read GetFollower;
  published
    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldVariantFollowerController read fBoldProperties write SetBoldProperties;
    property CanSelect;
//    property EditValue;
    property Height;
    property Properties;
    property RepositoryItem;
    property StyleEdit;
//    property TextHint;
  end;

  TcxBarBoldEditItemControl = class(TcxBarEditItemControl)
  private
  protected
//    procedure DoPostEditValue(Sender: TObject); override;
//    procedure DoValidate(Sender: TObject; var DisplayValue: TcxEditValue;
//      var ErrorText: TCaption; var Error: Boolean); override;
    procedure RestoreDisplayValue; override;
    procedure StoreDisplayValue; override;
//    procedure DoPaint(ARect: TRect; PaintType: TdxBarPaintType); override;
  public
  end;

{$IFDEF DevExScheduler}
  TcxBoldDateNavigator = class(TcxCustomDateNavigator, IBoldValidateableComponent)
  private
    fInternalChange: integer;
    fBoldStartHandleFollower: TBoldElementHandleFollower;
    fBoldEndHandleFollower: TBoldElementHandleFollower;
    fBoldStartProperties: TBoldVariantFollowerController;
    fBoldEndProperties: TBoldVariantFollowerController;
//    fValueOrDefinitionInvalid: boolean;
    function GetStartFollower: TBoldFollower;
    function GetEndFollower: TBoldFollower;
    procedure SetBoldStartProperties(const Value: TBoldVariantFollowerController);
    procedure SetBoldEndProperties(const Value: TBoldVariantFollowerController);
    function GetBoldEndHandle: TBoldElementHandle;
    function GetBoldStartHandle: TBoldElementHandle;
    procedure SetBoldEndHandle(const Value: TBoldElementHandle);
    procedure SetBoldStartHandle(const Value: TBoldElementHandle);
    procedure _AfterMakeUptoDate(Follower: TBoldFollower);

    procedure ValidateSelf;

    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;
  protected
    function GetStartContextType: TBoldElementTypeInfo;
    function GetEndContextType: TBoldElementTypeInfo;
//    function CanSelectPeriod: Boolean; // overriden to return false, as we don't support range
    procedure DateNavigatorSelectionChanged; override;
//    procedure DoSelectionChangedEvent; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property StartFollower: TBoldFollower read GetStartFollower;
    property EndFollower: TBoldFollower read GetEndFollower;
  published
    property BoldStartHandle: TBoldElementHandle read GetBoldStartHandle write SetBoldStartHandle;
    property BoldStartProperties: TBoldVariantFollowerController read fBoldStartProperties write SetBoldStartProperties;
    property BoldEndHandle: TBoldElementHandle read GetBoldEndHandle write SetBoldEndHandle;
    property BoldEndProperties: TBoldVariantFollowerController read fBoldEndProperties write SetBoldEndProperties;
    property Align;
    property Anchors;
    property BorderStyle;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FirstWeekOfYear;
    property Font;
    property HolidayColor;
    property LookAndFeel;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Scheduler;
    property SelectPeriod;
    property ShowDatesContainingEventsInBold;
    property ShowDatesContainingHolidaysInColor;
    property ShowWeekNumbers;
    property StartOfWeek;
    property Storage;
    property Styles;
    property TabOrder;
    property TabStop;
    property UnlimitedSelection;
    property Visible;

    property OnClick;
  {$IFDEF DELPHI5}
    property OnContextPopup;
  {$ENDIF}
    property OnCustomDrawBackground;
    property OnCustomDrawContent;
    property OnCustomDrawDayCaption;
    property OnCustomDrawDayNumber;
    property OnCustomDrawHeader;
    property OnPeriodChanged;
    property OnSelectionChanged;
    property OnShowDateHint;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;
{$ENDIF}

  { TcxBoldLabel }

  TcxBoldLabel = class(TcxCustomLabel, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxLabelProperties;
    function GetDataBinding: TcxBoldTextEditDataBinding;
    function GetProperties: TcxLabelProperties;
    procedure SetDataBinding(Value: TcxBoldTextEditDataBinding);
    procedure SetProperties(Value: TcxLabelProperties);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure Initialize; override;
    procedure SetEditAutoSize(Value: Boolean); override;
    procedure Paint; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxLabelProperties read GetActiveProperties;
  published
    property Align;
    property Anchors;
    property AutoSize {default False};
    property Constraints;
    property DataBinding: TcxBoldTextEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
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
    property Properties: TcxLabelProperties read GetProperties
      write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TcxBoldImage = class(TcxCustomImage, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxImageProperties;
    function GetDataBinding: TcxBoldBlobEditDataBinding;
    function GetProperties: TcxImageProperties;
    procedure SetDataBinding(Value: TcxBoldBlobEditDataBinding);
    procedure SetProperties(Value: TcxImageProperties);
//    function GetViewer: TBoldAbstractViewAdapter;
//    procedure SetViewer(Value: TBoldAbstractViewAdapter);
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure Initialize; override;
    procedure Paint; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxImageProperties read GetActiveProperties;
//    property Viewer: TBoldAbstractViewAdapter read GetViewer write SetViewer;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Constraints;
    property DataBinding: TcxBoldBlobEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property PopupMenu;
    property Properties: TcxImageProperties read GetProperties write SetProperties;
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
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetGraphicClass;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TcxBoldRichEdit = class(TcxCustomRichEdit, IBoldValidateableComponent, IBoldOCLComponent)
  private
    function GetActiveProperties: TcxRichEditProperties;
    function GetDataBinding: TcxBoldTextEditDataBinding;
    function GetProperties: TcxRichEditProperties;
    procedure SetDataBinding(Value: TcxBoldTextEditDataBinding);
    procedure SetProperties(Value: TcxRichEditProperties);
  protected
    procedure EditingChanged; override;
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
    procedure DoValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean); override;
    function RealReadOnly: Boolean; override;
    procedure Paint; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxRichEditProperties read GetActiveProperties;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DataBinding: TcxBoldTextEditDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
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
    property Properties: TcxRichEditProperties read GetProperties
      write SetProperties;
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
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
  end;
  
  TcxBoldListBox = class(TcxListBox, IBoldValidateableComponent)
  private
    fInternalUpdate: boolean;
    fListHandleFollower: TBoldListHandleFollower;
    fBoldListProperties: TBoldListAsFollowerListController;
    fBoldRowProperties: TBoldVariantFollowerController;
    function GetBoldListHandle: TBoldAbstractListHandle;
    procedure SetBoldListHandle(const Value: TBoldAbstractListHandle);
    procedure SetBoldListProperties(
      const Value: TBoldListAsFollowerListController);
    procedure SetRowProperties(
      const Value: TBoldVariantFollowerController);
    function GetBoldHandleIndexLock: Boolean;
    procedure SetBoldHandleIndexLock(const Value: Boolean);
    function GetFollower: TBoldFollower;
    function GetMutableList: TBoldList;
    procedure SyncSelection;
    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure DblClick; override;
    procedure Loaded; override;
    procedure _InsertItem(Index: Integer; Follower: TBoldFollower);
    procedure _ReplaceItem(Index: Integer; Follower: TBoldFollower);
    procedure _DeleteItem(Index: Integer; OwningFollower: TBoldFollower);
    procedure _RowAfterMakeUptoDate(Follower: TBoldFollower);
    procedure _BeforeMakeUptoDate(Follower: TBoldFollower);
    procedure _AfterMakeUptoDate(Follower: TBoldFollower);

    function DrawItem(ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect;
      AState: TOwnerDrawState): Boolean; override;
    procedure DefaultSetFontAndColor(Index: integer); virtual;

    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;

    function GetListFollower: TBoldFollower;
    function GetContextForBoldRowProperties: TBoldElementTypeInfo;
    property Follower: TBoldFollower read GetFollower;
    property MutableList: TBoldList read GetMutableList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawItem(Index: integer; aRect: TRect); virtual;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  published
//    property DataBinding: TcxBoldDataBinding read GetDataBinding write SetDataBinding implements IBoldValidateableComponent, IBoldOCLComponent;
    property BoldListHandle: TBoldAbstractListHandle read GetBoldListHandle write SetBoldListHandle;
    property BoldListProperties: TBoldListAsFollowerListController read fBoldListProperties write SetBoldListProperties;
    property BoldRowProperties: TBoldVariantFollowerController read fBoldRowProperties write SetRowProperties;
    property BoldHandleIndexLock: Boolean read GetBoldHandleIndexLock write SetBoldHandleIndexLock default true;
  end;


  TcxBoldCustomCheckListBox = class(TcxCustomCheckListBox, IBoldValidateableComponent)
  private
    fInternalUpdate: boolean;
    fListHandleFollower: TBoldListHandleFollower;
    fBoldListProperties: TBoldAbstractListAsFollowerListController;
    fBoldRowProperties: TBoldVariantFollowerController;
    FUpdateCount: Integer; // for Items
    fBoldRowCheckBoxProperties: TBoldCheckBoxStateFollowerController;
    fControllerList: TBoldControllerList;

    function GetBoldListHandle: TBoldAbstractListHandle;
    procedure SetBoldListHandle(const Value: TBoldAbstractListHandle);
    procedure SetBoldListProperties(
      const Value: TBoldAbstractListAsFollowerListController);
    procedure SetRowProperties(
      const Value: TBoldVariantFollowerController);
    function GetBoldHandleIndexLock: Boolean;
    procedure SetBoldHandleIndexLock(const Value: Boolean);
    function GetFollower: TBoldFollower;
    procedure SetBoldRowCheckBoxProperties(const Value: TBoldCheckBoxStateFollowerController);
    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean; virtual; abstract;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure SyncSelection;
    function GetInnerCheckListBoxClass: TcxCustomInnerCheckListBoxClass; override;

    procedure Loaded; override;

    function GetContextType: TBoldElementTypeInfo;
    procedure _DisplayCheckBox(Follower: TBoldFollower);
    procedure _DisplayString(Follower: TBoldFollower);
    procedure _ListInsertItem(Index: integer; Follower: TBoldFollower);
    procedure _ReplaceItem(Index: Integer; Follower: TBoldFollower);
    procedure _ListDeleteItem(Index: integer; Follower: TBoldFollower);
    procedure _ListBeforeMakeUpToDate(Follower: TBoldFollower);
    procedure _ListAfterMakeUpToDate(Follower: TBoldFollower);
    property BoldRowCheckBoxProperties: TBoldCheckBoxStateFollowerController read fBoldRowCheckBoxProperties write SetBoldRowCheckBoxProperties;
    property Follower: TBoldFollower read GetFollower;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
//    procedure DefaultDrawItem(Index: integer; aRect: TRect); virtual;
//    procedure DragDrop(Source: TObject; X, Y: Integer); override;
  published
    property BoldListHandle: TBoldAbstractListHandle read GetBoldListHandle write SetBoldListHandle;
    property BoldListProperties: TBoldAbstractListAsFollowerListController read fBoldListProperties write SetBoldListProperties;
    property BoldRowProperties: TBoldVariantFollowerController read fBoldRowProperties write SetRowProperties;
    property BoldHandleIndexLock: Boolean read GetBoldHandleIndexLock write SetBoldHandleIndexLock default true;

    property Align;
    property AllowDblClickToggle;
    property AllowGrayed;
    property Anchors;
    property AutoComplete;
    property AutoCompleteDelay;
    property BiDiMode;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditValueFormat;
    property Enabled;
    property Glyph;
    property GlyphCount;
    property Images;
    property ImageLayout;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
//    property Items;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollWidth;
    property ShowChecks;
    property ShowHint;
    property Sorted;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnCheckStatesToEditValue;
    property OnClick;
    property OnClickCheck;
    property OnCompare;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEditValueToCheckStates;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TcxBoldCheckListBox = class(TcxBoldCustomCheckListBox{, IBoldValidateableComponent, IBoldOCLComponent})
  private
    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean; override;
  published
    property BoldRowCheckBoxProperties;
  end;


const
  beSelectionHandleChanged = 400;

type
  TcxBoldSelectionCheckListBox = class(TcxBoldCustomCheckListBox{, IBoldValidateableComponent, IBoldOCLComponent})
  private
    fCheckBoxRenderer: TBoldAsCheckBoxStateRenderer;
    fPublisher: TBoldPublisher;
    fBoldSelectionHandle: TBoldAbstractListHandle;
    function GetAsCheckBoxState(aFollower: TBoldFollower): TCheckBoxState;
    procedure SetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
    procedure OnSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
    procedure SetSelectionHandle(const Value: TBoldAbstractListHandle);
    // IBoldValidateableComponent
    function ValidateComponent(ComponentValidator: TBoldComponentValidator; NamePrefix: string): Boolean; override;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BoldSelectionHandle: TBoldAbstractListHandle read fBoldSelectionHandle write SetSelectionHandle;
  end;

  TcxBoldListView = class(TcxCustomListView{, IBoldValidateableComponent, IBoldOCLComponent})
  private
    fListHandleFollower: TBoldListHandleFollower;
    fBoldProperties: TBoldAbstractListAsFollowerListController;
    fBoldRowProperties: TBoldVariantFollowerController;
    FUpdateCount: Integer;
    function GetContextType: TBoldElementTypeInfo;
    function GetBoldHandle: TBoldAbstractListHandle;
    procedure SetBoldHandle(value: TBoldAbstractListHandle);
    function GetFollower: TBoldFollower;
    procedure SetBoldProperties(Value: TBoldAbstractListAsFollowerListController);
    procedure SetRowProperties(const Value: TBoldVariantFollowerController);
    function GetBoldHandleIndexLock: Boolean;
    procedure SetBoldHandleIndexLock(Value: Boolean);
    function GetBoldList: TBoldList;
    function GetCurrentBoldElement: TBoldElement;
    function GetCurrentBoldObject: TBoldObject;
  protected
    procedure _BeforeMakeUptoDate(Follower: TBoldFollower);
    procedure _AfterMakeUptoDate(Follower: TBoldFollower);
    procedure _InsertItem(Index: Integer; Follower: TBoldFollower);
    procedure _ReplaceItem(Index: Integer; Follower: TBoldFollower);
    procedure _DeleteItem(index: Integer; OwningFollower: TBoldFollower);
    procedure _RowAfterMakeUptoDate(Follower: TBoldFollower);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Follower: TBoldFollower read GetFollower;
    property ListViewCanvas;
    property BoldList: TBoldList read GetBoldList;
    property CurrentBoldObject: TBoldObject read GetCurrentBoldObject;
    property CurrentBoldElement: TBoldElement read GetCurrentBoldElement;
  published
    property BoldHandle: TBoldAbstractListHandle read GetBoldHandle write SetBoldHandle;
    property BoldHandleIndexLock: Boolean read GetBoldHandleIndexLock write SetBoldHandleIndexLock default true;
    property BoldProperties: TBoldAbstractListAsFollowerListController read fBoldProperties write SetBoldProperties;
    property BoldRowProperties: TBoldVariantFollowerController read fBoldRowProperties write SetRowProperties;

    property Align;
    property AllocBy default 0;
    property Anchors;
    property BiDiMode;
    property Checkboxes;
    property ColumnClick default True;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HideSelection default True;
    property HotTrack default False;
    property HoverTime default -1;
    property IconOptions;
  {$IFDEF DELPHI6}
    property ItemIndex;
  {$ENDIF}
//    property Items;
    property LargeImages;
    property MultiSelect default False;
    property OwnerData default False;
    property OwnerDraw default False;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default False;
    property RowSelect default False;
    property ShowColumnHeaders default True;
    property ShowHint;
    property ShowWorkAreas default False;
    property SmallImages;
    property SortType default stNone;
    property StateImages;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property ViewStyle default vsIcon;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnCancelEdit;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
  {$IFDEF DELPHI6}
    property OnCreateItemClass;
  {$ENDIF}
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
  end;

{$IFDEF BoldDevExLog}
  TcxBoldEditorsLogProc = procedure(aMessage: string; aCategory: string = '') of object;
{$ENDIF}

{$IFDEF BoldDevExLog}
var
  cxBoldEditorsLogProc: TcxBoldEditorsLogProc;
{$ENDIF}

procedure InternalComboSetValue(
  aBoldHandle: TBoldElementHandle;
  aFollower: TBoldFollower;
  aSelectedElement: TBoldElement;
  aBoldSelectChangeAction: TBoldComboSelectChangeAction;
  aBoldSetValueExpression: TBoldExpression;
  aListHandle: TBoldAbstractListHandle;
  aValue: Variant);

procedure _ValidateEdit(aEdit: TcxCustomEdit);

implementation

uses
  Types,
{$IFDEF BOLD_DELPHI16_OR_LATER}UiTypes,{$ENDIF}
  BoldAttributes,
  BoldBase,
  Variants,
  Graphics,
  BoldQueue,
  BoldEnvironment,
  cxFilterControlUtils,
  cxBoldEditConsts,
  BoldReferenceHandle,
  BoldValueInterfaces,
  cxDateUtils,
  {$IFDEF DevExScheduler}
  cxSchedulerDateNavigator,
  {$ENDIF}
  BoldLogHandler,
  BoldCoreConsts,
  BoldDomainElement,
  BoldOCL,
  BoldGuard,
  BoldAFP,
  BoldGUI,
  BoldGuiResourceStrings,
  BoldMetaElementList,
  BoldHashIndexes,
  BoldIndex,
  BoldIndexableList,
  Forms, dxMessages;

type
  TcxCustomEditAccess = class(TcxCustomEdit);
  TBoldFollowerControllerAccess = class(TBoldFollowerController);
  TBoldElementHandleAccess = class(TBoldElementHandle);
  TcxEditValidateInfoAccess = class(TcxEditValidateInfo);

{$IFDEF BoldDevExLog}
procedure _Log(aMessage: string; aCategory: string = '');
begin
  if Assigned(cxBoldEditorsLogProc) then
    cxBoldEditorsLogProc(aMessage, aCategory);
end;
{$ENDIF}

procedure _ValidateEdit(aEdit: TcxCustomEdit);
var
  lBoldValidateableComponent: IBoldValidateableComponent;

  procedure InternalValidate;
  var
    lBoldComponentValidator: TBoldComponentValidator;
  begin
    lBoldComponentValidator := TBoldComponentValidator.Create;
    try
      lBoldValidateableComponent.ValidateComponent(lBoldComponentValidator, aEdit.Name);
    finally
      lBoldComponentValidator.free;
    end;
  end;

var
  lcxBoldEditDataBinding: TcxBoldEditDataBinding;
  lValue: Variant;
  lFollower: TBoldFollower;
  s: string;
  lContext: TBoldElementTypeInfo;
  lEvaluator: TBoldOCL;
  lBoldMemberRTInfo: TBoldMemberRTInfo;
  lExpression: string;
begin
  if Supports(aEdit, IBoldValidateableComponent, lBoldValidateableComponent) then
  begin
    InternalValidate;

    lcxBoldEditDataBinding := TcxCustomEditAccess(aEdit).DataBinding as TcxBoldEditDataBinding;
    lContext := lcxBoldEditDataBinding.GetContextType;
    if not Assigned(lcxBoldEditDataBinding.BoldHandle) then
      lValue := '< no handle >'
    else
    if (lContext = nil) then
      lValue := '< no context >'
    else
    if Assigned(lcxBoldEditDataBinding.Follower) then
    begin
      lFollower := lcxBoldEditDataBinding.Follower;
      lEvaluator := lContext.Evaluator as TBoldOCL;
      lExpression := TBoldFollowerControllerAccess(lFollower.Controller).Expression;
      lBoldMemberRTInfo := lEvaluator.RTInfo(lExpression, lContext, false, lFollower.Controller.VariableList);
      if Assigned(lBoldMemberRTInfo) then
        lValue := lBoldMemberRTInfo.AsString
      else
      begin
        lContext := lEvaluator.ExpressionType(lExpression, lContext, false, lFollower.Controller.VariableList);
        if Assigned(lContext) then
          lValue := lContext.AsString;
      end;
    end;
    if VarIsNull(lValue) or (lValue = '') then
      lValue := TBoldFollowerControllerAccess(lcxBoldEditDataBinding.BoldFollowerController).Expression;

//    TcxCustomEditAccess(aEdit).SetInternalEditValue(lValue);
    TcxCustomEditAccess(aEdit).SetInternalDisplayValue(lValue);

    if aEdit.name <> '' then
      s := '_ValidateEdit ' + aEdit.name + ':' + lValue
    else
      s := '_ValidateEdit ' + aEdit.ClassName + ':' + lValue;

    OutPutDebugString(PChar(S));
  end
  else
  if Supports(aEdit.ActiveProperties, IBoldValidateableComponent, lBoldValidateableComponent) then
    InternalValidate;
end;

procedure InternalComboSetValue(
  aBoldHandle: TBoldElementHandle;
  aFollower: TBoldFollower;
  aSelectedElement: TBoldElement;
  aBoldSelectChangeAction: TBoldComboSelectChangeAction;
  aBoldSetValueExpression: TBoldExpression;
  aListHandle: TBoldAbstractListHandle;
  aValue: Variant);

var
  ElementToAssignTo: TBoldElement;
  lValue: Variant;
  lOldValue: IBoldValue;
  lHasOldValue: boolean;

  procedure InternalSetValue();
  begin
    if not ElementToAssignTo.Mutable then
    begin
      SetBoldLastFailureReason(TBoldFailureReason.CreateFmt('Element %s is immutable', [ElementToAssignTo.displayName], ElementToAssignTo));
      exit;
    end
    else
    if (elementToAssignTo is TBoldMember) then
    begin
      if not TBoldMember(ElementToAssignTo).CanModify then
      begin
        exit;
      end;
        if elementToAssignTo is TBoldObjectReference then
        begin
          if not ElementToAssignTo.IsEqual(aSelectedElement) then
          begin
            ElementToAssignTo.Assign(aSelectedElement);
          end;
        end
        else
  //            if elementToAssignTo is TBoldAttribute then
        begin
          lValue := Null;
          if Assigned(aSelectedElement) then
            lValue := aSelectedElement.AsVariant;
          ElementToAssignTo.AsVariant := lValue;
  //              (aFollower.Controller as TBoldVariantFollowerController).MayHaveChanged(lValue, aFollower);
        end;
    end;
  end;

var
  FailureReason: TBoldFailureReason;
  Discard: Boolean;
  Handled: Boolean;
begin
  try
    BoldClearLastFailure;
    try
      case aBoldSelectChangeAction of
        bdcsNone:;
        bdscSetText:
          (aFollower.Controller as TBoldVariantFollowerController).MayHaveChanged(aValue, aFollower);
        bdcsSetReference:
          if Assigned(aBoldHandle) and aBoldHandle.CanSetValue then
            TBoldElementHandleAccess(aBoldHandle).SetValue(aSelectedElement);
        bdcsSetListIndex:
          begin
            aFollower.DiscardChange;
            if assigned(aListHandle) then
              aListHandle.CurrentIndex := aListHandle.List.IndexOf(aSelectedElement);
          end;
        bdcsSetValue:
          begin
            if not (Assigned(aFollower.Element)) then
              exit;
            if trim(aBoldSetValueExpression) <> '' then
              ElementToAssignTo := aFollower.Element.EvaluateExpressionAsDirectElement(aBoldSetValueExpression, TBoldFollowerControllerAccess(aFollower.Controller).VariableList)
            else
            begin
              ElementToAssignTo := aFollower.Value;
            end;
            if assigned(ElementToAssignTo) then
              InternalSetValue();
          end;
      end;
    finally
      FailureReason := GetBoldLastFailureReason;
      if assigned(FailureReason) then
        BoldRaiseLastFailure(nil, '', 'InternalComboSetValue failed.');
    end;
  except
    on E: Exception do
    begin
      Handled := assigned(aFollower.Controller) and TBoldFollowerControllerAccess(aFollower.Controller).DoApplyException(E, ElementToAssignTo, Discard);
      if Discard then
        aFollower.DiscardChange;
      if not Handled then
        raise;
    end;
  end;
end;

{ TcxBoldTextEditProperties }

procedure TcxCustomBoldTextEditProperties._DeleteItem(Index: Integer;
  OwningFollower: TBoldFollower);
begin
  if not LookupItems.Updating then
    LookupItems.BeginUpdate;
  LookupItems.Delete(index);
end;

procedure TcxCustomBoldTextEditProperties._InsertItem(Index: Integer; Follower: TBoldFollower);
begin
  Assert(Assigned(Follower));
  if not LookupItems.Updating then
    LookupItems.BeginUpdate;
  Follower.EnsureDisplayable;
  LookupItems.Insert(Index, VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower)));
end;

procedure TcxCustomBoldTextEditProperties._ReplaceItem(Index: Integer;
  Follower: TBoldFollower);
begin
  if not LookupItems.Updating then
    LookupItems.BeginUpdate;
  Follower.EnsureDisplayable;
  LookupItems.Strings[Index] := VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower));
end;

procedure TcxCustomBoldTextEditProperties._RowAfterMakeUptoDate(
  Follower: TBoldFollower);
var
  index: Integer;
  NewValue: String;
begin
{  OutputDebugString('TcxCustomBoldTextEditProperties._RowAfterMakeUptoDate');
  if (Owner is TcxCustomEdit) and TcxCustomEdit(Owner).IsDesigning then
  begin
    ValidateEdit(TcxCustomEdit(Owner));
  end;
}
  index := Follower.index;
  if (index > -1) and (index < LookupItems.Count) then
  begin
    NewValue := VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower));
    if LookupItems[index] <> NewValue then
      LookupItems[index] := NewValue;
  end;
//  LookupDataChanged(self);
  // forces a redisplay of the edit-area, the windows component might go blank if the active row is removed and then reinserted
//  fBoldHandleFollower.Follower.MarkValueOutOfDate; // do we really need this here ? Danny
end;

function TcxCustomBoldTextEditProperties.GetBoldListHandle: TBoldAbstractListHandle;
begin
  Result := fListHandleFollower.BoldHandle;
end;

function TcxCustomBoldTextEditProperties.GetContextForBoldRowProperties: TBoldElementTypeInfo;
begin
  if assigned(BoldLookupListHandle) then
    result := BoldLookupListHandle.StaticBoldType
  else
    result := nil;
end;

function TcxCustomBoldTextEditProperties.GetListFollower: TBoldFollower;
begin
  Result := fListHandleFollower.Follower;
end;

function TcxCustomBoldTextEditProperties.IsEditValueValid(
  var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean;
begin
  result := inherited IsEditValueValid(EditValue, AEditFocused) or IsNilRepresentation(EditValue);
end;

function TcxCustomBoldTextEditProperties.IsNilRepresentation(
  AValue: Variant): boolean;
begin
  result := UseLookupData and cxEditVarEquals(AValue, BoldRowProperties.NilRepresentation);
end;

procedure TcxCustomBoldTextEditProperties.SetBoldListHandle(
  const Value: TBoldAbstractListHandle);
begin
  fListHandleFollower.BoldHandle := value;
end;

procedure TcxCustomBoldTextEditProperties.SetBoldListProperties(
  const Value: TBoldAbstractListAsFollowerListController);
begin
  fBoldListProperties.Assign(Value);
end;
{
procedure TcxCustomBoldTextEditProperties.SetBoldSelectChangeAction(
  const Value: TBoldComboSelectChangeAction);
begin
  fBoldSelectChangeAction := Value;
end;
}
procedure TcxCustomBoldTextEditProperties.SetRowProperties(
  const Value: TBoldVariantFollowerController);
begin
  fBoldRowProperties.Assign(Value);
end;

constructor TcxCustomBoldTextEditProperties.Create(AOwner: TPersistent);

begin
  inherited;
  fBoldRowProperties := TBoldVariantFollowerController.Create(Owner as TComponent);
  fBoldRowProperties.AfterMakeUptoDate := _RowAfterMakeUptoDate;
  fBoldRowProperties.OnGetContextType := GetContextForBoldRowProperties;
  fBoldListProperties := TBoldAbstractListAsFollowerListController.Create((Owner as TComponent), fBoldRowProperties);
  with fBoldListProperties do
  begin
    OnAfterInsertItem := _InsertItem;
    OnAfterDeleteItem := _DeleteItem;
    OnReplaceitem := _ReplaceItem;
    BeforeMakeUptoDate := _BeforeMakeUptoDate;
    AfterMakeUptoDate := _AfterMakeUptoDate;
  end;
  fListHandleFollower := TBoldListHandleFollower.Create((Owner as TComponent).owner , fBoldListProperties);
  BoldSelectChangeAction := bdcsSetValue;
end;

destructor TcxCustomBoldTextEditProperties.Destroy;
begin
  FreeAndNil(fListHandleFollower);
  FreeAndNil(fBoldListProperties);
  FreeAndNil(fBoldRowProperties);
  inherited;
end;

procedure TcxCustomBoldTextEditProperties._AfterMakeUptoDate(
  Follower: TBoldFollower);
begin
  fBoldRowProperties.AfterMakeUptoDate := _RowAfterMakeUptoDate;
  if LookupItems.Updating then
    LookupItems.EndUpdate;
end;

procedure TcxCustomBoldTextEditProperties._BeforeMakeUptoDate(
  Follower: TBoldFollower);
begin
  fBoldRowProperties.AfterMakeUptoDate := nil;
end;


{ TcxBoldEditDataBinding }

constructor TcxBoldEditDataBinding.Create(AEdit: TcxCustomEdit);
begin
  inherited Create(AEdit);
//  DefaultValuesProvider.fcxBoldEditDataBinding := self;
  fBoldFollowerController:= TBoldVariantFollowerController.Create(AEdit);
//  fBoldProperties := TBoldVariantFollowerController.Create(AEdit);
  BoldFollowerController.AfterMakeUptoDate := _AfterMakeUptoDate;
  BoldFollowerController.OnGetContextType := GetContextType;
  BoldFollowerController.OnApplyException := HandleApplyException;
  fBoldHandleFollower := TBoldElementHandleFollower.Create(AEdit.Owner, BoldFollowerController);

  DefaultValuesProvider.BoldProperties := BoldFollowerController;
  DefaultValuesProvider.BoldHandleFollower := fBoldHandleFollower;
{  if AEdit.InnerControl <> nil then
    FDataLink.Control := AEdit.InnerControl
  else
    FDataLink.Control := AEdit;
}
end;

destructor TcxBoldEditDataBinding.Destroy;
begin
  case BoldFollowerController.ApplyPolicy of
    bapChange, bapExit: try
      Follower.Apply;
    except
      Follower.DiscardChange;
    end;
    bapDemand: Follower.DiscardChange;
  end;
  FreeAndNil(fBrokenConstraints);  
  FreeAndNil(fBoldHandleFollower);
  FreeAndNil(fBoldFollowerController);
  Edit.ViewInfo.OnPaint := nil;
  inherited Destroy;
end;

function TcxBoldEditDataBinding.GetBoldHandle: TBoldElementHandle;
begin
  Result := fBoldHandleFollower.BoldHandle
end;

function TcxBoldEditDataBinding.GetFollower: TBoldFollower;
begin
  Result := fBoldHandleFollower.Follower
end;

function TcxBoldEditDataBinding.GetHasBrokenConstraints: boolean;
begin
  result := Assigned(fBrokenConstraints) and (BrokenConstraints.Count > 0);
end;

function TcxBoldEditDataBinding.GetIsInInternalChange: boolean;
begin
  result := fInternalChange > 0;
end;

procedure TcxBoldEditDataBinding.SetBoldHandle(
  const Value: TBoldElementHandle);
begin
//  if not (Edit.IsLoading) then
  begin
    fBoldHandleFollower.BoldHandle := value;
    DefaultValuesProvider.fBoldHandleFollower := fBoldHandleFollower;
    if (Edit.IsDesigning) and not (Edit.IsLoading) then
    begin
      _ValidateEdit(Edit);
    end;
//    DefaultValuesProvider.BoldHandle := value;
//    DefaultValuesProvider.BoldProperties := BoldProperties;
  end;
end;

procedure TcxBoldEditDataBinding.Assign(Source: TPersistent);
begin
  if Source is TcxBoldEditDataBinding then
  begin
    // TODO: what about HandleFollower ?
    BoldHandle := TcxBoldEditDataBinding(Source).BoldHandle;
    fBoldFollowerController := TcxBoldEditDataBinding(Source).BoldFollowerController;
    DataChanged; // ?
  end;
  inherited Assign(Source);
end;

function TcxBoldEditDataBinding.ImmediatePost: boolean;
begin
  result := BoldFollowerController.ApplyPolicy = bapChange;
end;

function TcxBoldEditDataBinding.CanCheckEditorValue: Boolean;
begin
  result := inherited CanCheckEditorValue;
end;

function TcxBoldEditDataBinding.CanPostEditorValue: Boolean;
begin
  Result := IsDataAvailable and (fInternalChange = 0) and not FEdit.ActiveProperties.ReadOnly and Modified;
//  Result := Editing and Edit.IsFocused; //or (BoldProperties.ApplyPolicy = bapExit);
//  result := false; // inherited CanPostEditorValue;
//  Result := Editing and DataLink.FModified;
end;

procedure TcxBoldEditDataBinding.DataChanged;
begin
  inherited;
end;

procedure TcxBoldEditDataBinding.DataSetChange;
begin
  inherited;
end;

procedure TcxBoldEditDataBinding.DefaultValuesChanged;
begin
  inherited DefaultValuesChanged;
end;

procedure TcxBoldEditDataBinding.EditingChanged;
begin
  TcxCustomEditAccess(Edit).EditingChanged;
end;

function TcxBoldEditDataBinding.ExecuteAction(
  Action: TBasicAction): Boolean;
begin
  result := inherited ExecuteAction(Action);
end;

function TcxBoldEditDataBinding.GetDefaultValuesProvider: TcxCustomBoldEditDefaultValuesProvider;
begin
  Result := TcxCustomBoldEditDefaultValuesProvider(IDefaultValuesProvider.GetInstance);
end;

class function TcxBoldEditDataBinding.GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  result := TcxCustomBoldEditDefaultValuesProvider;
end;

type
  TcxCustomTextEditAccess = class(TcxTextEdit);

function TcxBoldEditDataBinding.GetModified: Boolean;
var
  lcxBoldEditProperties: IcxBoldEditProperties;
  lValue: Variant;
  lEditValue: variant;
//  lElement: TBoldElement;
begin
  if not IsDataAvailable or FEdit.ActiveProperties.ReadOnly then
  begin
    result := false;
  end
  else
  begin
    if Supports(Edit.ActiveProperties, IcxBoldEditProperties, lcxBoldEditProperties) then
    begin
      if Edit is TcxCustomComboBox then
        lEditValue := TcxCustomComboBox(Edit).ILookupData.CurrentKey
      else
        lEditValue := Edit.EditValue;
      lValue := lcxBoldEditProperties.BoldElementToEditValue(Follower, Follower.Element, Edit);
//      result := (Follower.AssertedController.EffectiveRenderer as TBoldAsVariantRenderer).IsChanged(Follower, lValue);
      result := not cxEditVarEquals(lEditValue, lValue);
    end
    else
    begin
      result := (TBoldFollowerControllerAccess(Follower.AssertedController).EffectiveRenderer as TBoldAsVariantRenderer).IsChanged(Follower, Edit.EditValue);
    end;
  end;
end;

function TcxBoldEditDataBinding.GetStoredValue: TcxEditValue;
begin
  Assert(assigned(Follower));
  result := ((Follower.Controller) as TBoldVariantFollowerController).GetAsVariant(Follower);
end;

function TcxBoldEditDataBinding.IsLookupControl: Boolean;
begin
  result := false; // inherited IsLookupControl;
end;

function TcxBoldEditDataBinding.IsRefreshDisabled: Boolean;
begin
  result := false; // inherited IsRefreshDisabled;
end;

procedure TcxBoldEditDataBinding.Reset;
begin
  inc(fInternalChange);
  try
  case BoldFollowerController.ApplyPolicy of
    bapChange :
      begin
//        Follower.UndoChange;
      end;
    bapExit, bapDemand: Follower.DiscardChange;
  end;
  inherited;
//  Follower.Display;
//  TBoldQueueable.DisplayAll;
  finally
    dec(fInternalChange);
    Edit.LockClick(False);
  end;
end;

procedure TcxBoldEditDataBinding.SetModified;
begin
  if (fInternalChange = 0) and Editing then
  begin
    inherited;
  end;
end;

procedure TcxBoldEditDataBinding.SetStoredValue(const Value: TcxEditValue);
var
  lIcxBoldEditProperties: IcxBoldEditProperties;
  lDone: Boolean;
begin
  lDone := false;
  if Supports(Edit.ActiveProperties, IcxBoldEditProperties, lIcxBoldEditProperties) then
  begin
    lIcxBoldEditProperties.SetStoredValue(Value, BoldHandle, Edit, Follower, lDone);
  end;
  if not lDone then
  begin
    InternalSetValue(Value);
  end;
  TBoldQueueable.DisplayAll;
//  Follower.Apply;
  inherited;
end;

function TcxBoldEditDataBinding.UpdateAction(
  Action: TBasicAction): Boolean;
begin
  result := inherited UpdateAction(Action);
end;

procedure TcxBoldEditDataBinding.UpdateData;
begin
  inherited;
end;

procedure TcxBoldEditDataBinding.UpdateDisplayValue;
begin
  Edit.LockClick(True);
  inc(fInternalChange);
  try
    inherited UpdateDisplayValue;
  finally
    dec(fInternalChange);
    Edit.LockClick(False);
  end;
  if Edit.IsDesigning and not Edit.IsLoading then
  begin
    _ValidateEdit(Edit);
  end;
end;

type
  TcxCustomTextEditPropertiesAccess = class(TcxCustomTextEditProperties)
  end;

procedure TcxBoldEditDataBinding._AfterMakeUptoDate(
  Follower: TBoldFollower);
var
  lValue: Variant;
//  lcxBoldComboBoxProperties: TcxBoldComboBoxProperties;
  lElement: TBoldElement;
//  WasModified: boolean;
  ASize: TSize;
  AViewData: TcxCustomEditViewData;
  AEditSizeProperties: TcxEditSizeProperties;
begin
  if fInternalChange > 0 then
  begin
    {$IFDEF BoldDevExLog}
    Assert(assigned(self));
    _Log('TcxBoldEditDataBinding._AfterMakeUptoDate: fInternalChange ' + Edit.Name, className);
    {$ENDIF}
    {$IFDEF Constraints}
     SubscribeToConstraints(Follower.Value);
    {$ENDIF}
    exit;
  end;
//  WasModified := Edit.ModifiedAfterEnter or Edit.EditModified;
  Edit.LockClick(True);
  inc(fInternalChange);
  try
    // this is not really the perfect place for setting ImmediatePost
    Edit.ActiveProperties.ImmediatePost := ImmediatePost;
    lValue := null;
    if Edit.IsDesigning then
    begin
      _ValidateEdit(Edit);
    end
    else
    begin
      TypeMayHaveChanged;
      {$IFDEF Constraints}
      lElement := Follower.Value;
      SubscribeToConstraints(lElement);
      {$ENDIF}
      lValue := InternalGetValue(Follower);
      if Edit.ModifiedAfterEnter and not Edit.IsPosting then
        Edit.Reset;
      if not cxEditVarEquals(Edit.EditValue, lValue) then
      begin
        TcxCustomEditAccess(Edit).SetInternalEditValue(lValue);
      end;
      if ShowHintIfCaptionDoesntFit then
      begin
        AViewData := TcxCustomEditViewData(TcxCustomEditAccess(Edit).CreateViewData);
        try
          TcxCustomEditAccess(Edit).PopulateSizeProperties(AEditSizeProperties);
          ASize := AViewData.GetEditSize(cxScreenCanvas, lValue, AEditSizeProperties, edit.ViewInfo);
          if ASize.cx > Edit.Width then
          begin
            Edit.Hint := VarToStr(lValue);
            Edit.ShowHint := true;
          end
          else
          begin
            Edit.Hint := '';
            Edit.ShowHint := false;
          end;
        finally
          FreeAndNil(AViewData);
        end;
      end;
    end;
  finally
    ValueOrDefinitionInvalid := false;
    try
      if Assigned(Edit.ViewInfo) and Assigned(Edit.ViewInfo.ErrorData) and TcxEditValidateInfoAccess(Edit.ViewInfo.ErrorData).IsError then
      begin
        ValueOrDefinitionInvalid := not Edit.ValidateEdit(false);
      end;
    finally
//      TcxCustomTextEditAccess(Edit).ResetEditValue;
      Edit.LockClick(False);
      dec(fInternalChange);
      if Edit.IsFocused and not Edit.EditModified then
        Edit.SelectAll
    end;
  end;
end;

function TcxBoldEditDataBinding.InternalGetValue(Follower: TBoldFollower): Variant;
var
  lElement: TBoldElement;
  lIcxBoldEditProperties: IcxBoldEditProperties;
begin
  result := Null;
  lElement := Follower.Value;
{    if (Edit.ActiveProperties is TcxBoldComboBoxProperties) then
    begin
      lcxBoldComboBoxProperties := (Edit.ActiveProperties as TcxBoldComboBoxProperties);
      if (Edit is TcxCustomComboBox) and Assigned(TcxCustomComboBox(Edit).ILookupData.ActiveControl) then
      begin
        TcxCustomEditListBox(TcxCustomComboBox(Edit).ILookupData.ActiveControl).ItemIndex := lcxBoldComboBoxProperties.LookupListFollower.CurrentIndex;
      end;
    end;
}
  if Supports(Edit.ActiveProperties, IcxBoldEditProperties, lIcxBoldEditProperties) then
    result := lIcxBoldEditProperties.BoldElementToEditValue(Follower, lElement, Edit)
  else
    result := TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower);
  if VarIsEmpty(result) then
    result := Null;
end;

function TcxBoldEditDataBinding.GetBrokenConstraints: TStringList;
begin
  if not Assigned(fBrokenConstraints) then
    fBrokenConstraints := TStringList.Create;
  result := fBrokenConstraints;
end;

function TcxBoldEditDataBinding.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TcxBoldEditDataBinding.CanModify: Boolean;
begin
  result := inherited CanModify and MayModify;
end;

procedure TcxBoldEditDataBinding.TypeMayHaveChanged;
begin
//  Edit.IsDesigning
// BoldEffectiveEnvironment.RunningInIDE or
  if not Assigned(BoldHandle) or not Assigned(BoldHandle.Value) then
    Exit; // only update at runtime if there are values, avoids update on every UML model change.
  if fCurrentElementType <> BoldHandle.BoldType then
  begin
    fCurrentElementType := BoldHandle.BoldType;
    if Edit is TcxCustomTextEdit then
      TcxCustomTextEditAccess(Edit).LockLookupDataTextChanged;
    try
      TcxCustomEditAccess(Edit).PropertiesChanged(nil);
    finally
      if Edit is TcxCustomTextEdit then
        TcxCustomTextEditAccess(Edit).UnlockLookupDataTextChanged;
    end;
  end;
end;

function TcxBoldEditDataBinding.MayModify: boolean;
var
  lcxBoldEditProperties: IcxBoldEditProperties;
begin
  result := BoldFollowerController.MayModify(Follower);
  if result and Supports(Edit.ActiveProperties, IcxBoldEditProperties, lcxBoldEditProperties) then
  begin
    result := lcxBoldEditProperties.CanEdit(BoldHandle, Follower);
  end;
end;

procedure TcxBoldEditDataBinding.DoChanged;
begin
  if Editing and (fInternalChange = 0) then
  begin
    inc(fInternalChange);
    try
      InternalSetValue(Edit.EditingValue);
      if ImmediatePost  then
        Follower.Apply;
      Follower.EnsureDisplayable;
    finally
      dec(fInternalChange);
    end;
  end;
end;

procedure TcxBoldEditDataBinding.DoEnter;
begin
  if (Follower.State = bfsDirty) then
    Edit.ModifiedAfterEnter := true;
end;

procedure TcxBoldEditDataBinding.DoExit;
begin
  if (Follower.State = bfsDirty) and (Follower.Controller.ApplyPolicy <> bapDemand) then
  try
    Follower.Apply;
  except
    Follower.DiscardChange;
  end;
end;

{$IFDEF Constraints}
procedure TcxBoldEditDataBinding.SubscribeToConstraints(
  aElement: TBoldElement);
var
  s: string;
  lIE: TBoldIndirectElement;
  lConstraintList: TBoldList;
const
  ECM_FIRST = $1500;
  EM_SETCUEBANNER = ECM_FIRST + 1;

  procedure EvaluateConstraints(AElement: TBoldElement);
  var
    i: integer;
  begin
    lIE := TBoldIndirectElement.Create;
    try
      aElement.EvaluateAndSubscribeToExpression('constraints->select(c|not c)', Follower.Subscriber, lIe);
      lConstraintList := lIE.Value as TBoldList;
      for i := 0 to lConstraintList.Count - 1 do
        BrokenConstraints.AddObject(lConstraintList[i].StringRepresentation[11], lConstraintList[i]);
      if HasBrokenConstraints then
        if Assigned(Edit) and Assigned(TcxCustomEditAccess(Edit).InnerEdit) then
        begin
          s := BrokenConstraints.Text;
          SendMessage(TcxCustomEditAccess(Edit).InnerEdit.Control.Handle, EM_SETCUEBANNER, 1, LParam(PWideChar(WideString(s))));
        end;
    finally
      lIE.free;
    end;
  end;

begin
  FreeAndNil(fBrokenConstraints);
  if not Assigned(aElement) then
    exit;
  if ((aElement is TBoldMember) and (Assigned(TBoldMember(aElement).BoldMemberRTinfo)) and
      ((TBoldMember(aElement).BoldMemberRTinfo.ConstraintCount > 0)
//      or (TBoldMember(aElement).OwningObject.BoldClassTypeInfo.ConstraintCount > 0)
      )) or
     (aElement is TBoldObject) and (TBoldObject(aElement).BoldClassTypeInfo.ConstraintCount > 0) or
     (aElement is TBoldSystem) and (TBoldSystem(aElement).BoldSystemTypeInfo.ConstraintCount > 0 ) then
  begin
    EvaluateConstraints(AElement);
  end;
end;
{$ENDIF}

function TcxBoldEditDataBinding.ValidateComponent(
  ComponentValidator: TBoldComponentValidator;
  NamePrefix: string): Boolean;
var
  lContext: TBoldElementTypeInfo;
  lExpressionType: TBoldElementTypeInfo;
  lBoldValidateableComponent: IBoldValidateableComponent;
  s: string;
begin
  lContext := GetContextType;
  result := Assigned(lContext);
  if not result then
  begin
    BoldLog.LogFmt(sNoContext, [Edit.Name])
  end
  else
  begin
    result := ComponentValidator.ValidateExpressionInContext(
      TBoldFollowerControllerAccess(BoldFollowerController).Expression,
      lContext,
      format('%s %s.Expression', [NamePrefix, Edit.Name]), TBoldFollowerControllerAccess(BoldFollowerController).VariableList) and result; // do not localize

    if result then
    begin
      lExpressionType := lContext.Evaluator.ExpressionType(TBoldFollowerControllerAccess(BoldFollowerController).Expression, lContext, false, TBoldFollowerControllerAccess(BoldFollowerController).VariableList);
      if Assigned(lExpressionType) then
      begin
        s := ValidateTypeConforms(lExpressionType);
        if s <> '' then
        begin
          result := false;
          BoldLog.Log('*** ' + s + ' in ' + Edit.Name);
        end;
      end;
    end;
  end;
  if Supports(Edit.ActiveProperties, IBoldValidateableComponent, lBoldValidateableComponent) then
  begin
    result := lBoldValidateableComponent.ValidateComponent(ComponentValidator, NamePrefix) and result;
  end;
  ValueOrDefinitionInvalid := not result;
end;

function TcxBoldEditDataBinding.ValidateTypeConforms(
  aExpressionType: TBoldElementTypeInfo): string;
begin
  result := '';
end;

function TcxBoldEditDataBinding.GetValueOrDefinitionInvalid: boolean;
begin
  result := fValueOrDefinitionInvalid;
end;

procedure TcxBoldEditDataBinding.SetValueOrDefinitionInvalid(
  const Value: boolean);
begin
  if fValueOrDefinitionInvalid <> Value then
  begin
    fValueOrDefinitionInvalid := Value;
//    Edit.Refresh;
//    Edit.Invalidate;
    Edit.Repaint;
  end;
end;

function TcxBoldEditDataBinding.GetBoldProperties: TBoldVariantFollowerController;
begin
  result := BoldFollowerController as TBoldVariantFollowerController;
end;

procedure TcxBoldEditDataBinding.InternalSetValue(
  const aValue: TcxEditValue);
begin
  {$IFDEF BoldDevExLog}
  if Follower.State = bfsSubscriptionOutOfDate then
  begin
    _Log('TcxBoldEditDataBinding.InternalSetValue, Follower.State = bfsSubscriptionOutOfDate', 'Follower debug');
  end;
  {$ENDIF}
  BoldProperties.MayHaveChanged(aValue, Follower);
end;

procedure TcxBoldEditDataBinding.SetBoldProperties(
  const Value: TBoldVariantFollowerController);
begin
  BoldFollowerController.Assign(Value);
end;

function TcxBoldEditDataBinding.GetExpression: TBoldExpression;
begin
  result := BoldProperties.Expression;
end;

function TcxBoldEditDataBinding.GetVariableList: TBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

function TcxBoldEditDataBinding.HandleApplyException(E: Exception;
  Elem: TBoldElement; var Discard: Boolean): Boolean;
begin
  result := false;
  if Edit.IsDestroying then
    exit;
  if Assigned(Edit.ActiveProperties) then
    result := not (evoRaiseException in Edit.ActiveProperties.ValidationOptions);
  if not TcxCustomEditAccess(Edit).IsEditValidated then
    Edit.ValidateEdit(false);
end;

procedure TcxBoldEditDataBinding.SetExpression(const Value: TBoldExpression);
begin
  BoldProperties.Expression := Value;
end;

procedure TcxBoldEditDataBinding.ValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean);

  procedure AddMessage(const s: string);
  begin
    if AErrorText <> '' then
      AErrorText := AErrorText + BOLDCRLF + s
    else
      AErrorText := s;
  end;
begin
  if not AError then
    AErrorText := '';
  BoldClearLastFailure;
  if not BoldProperties.ValidateVariant(ADisplayValue, Follower) then
  begin
    AError := true;
    if (BoldSystem.GetBoldLastFailureReason <> nil) then
      AddMessage(BoldSystem.GetBoldLastFailureReason.Reason);
  end;
  if HasBrokenConstraints then
  begin
    AError := true;
    AddMessage(BrokenConstraints.text);
  end;
  ValueOrDefinitionInvalid := AError;
end;

{ TcxBoldTextEdit }

class function TcxBoldTextEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxBoldTextEditProperties;
end;

function TcxBoldTextEdit.GetActiveProperties: TcxBoldTextEditProperties;
begin
  Result := TcxBoldTextEditProperties(InternalGetActiveProperties);
//  FProperties.ValidateOnEnter := true;
end;

function TcxBoldTextEdit.GetProperties: TcxBoldTextEditProperties;
begin
  Result := TcxBoldTextEditProperties(FProperties);
//  FProperties.ImmediatePost := true;
//  FProperties.ValidateOnEnter := true;
end;

procedure TcxBoldTextEdit.SetProperties(
  Value: TcxBoldTextEditProperties);
begin
  FProperties.Assign(Value);
end;

class function TcxBoldTextEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxBoldTextEditDataBinding;
end;

function TcxBoldTextEdit.GetDataBinding: TcxBoldTextEditDataBinding;
begin
  Result := FDataBinding as TcxBoldTextEditDataBinding;
end;

procedure TcxBoldTextEdit.SetDataBinding(
  Value: TcxBoldTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

function TcxBoldTextEdit.ValidateKeyDown(var Key: Word;
  Shift: TShiftState): Boolean;
begin
  result := DataBinding.IsDataAvailable and inherited ValidateKeyDown(Key, Shift);
end;

function TcxBoldTextEdit.ValidateKeyPress(var Key: Char): Boolean;
begin
  result := DataBinding.MayModify and inherited ValidateKeyPress(Key);
  if not result then
    Key := #0
  else
  begin
    if (Key = #13) and (DataBinding.Follower.Controller.ApplyPolicy <> bapDemand) then
    begin
      DataBinding.Follower.Apply;
      SelectAll;
    end;
  end;
end;

procedure TcxBoldTextEdit.DoOnChange;
begin
  inherited;
  PostEditValue;
end;

procedure TcxBoldTextEdit.DoValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean);
begin
  inherited;
  if Assigned(DataBinding) then
    DataBinding.ValidateDisplayValue(ADisplayValue, AErrorText, AError);
end;

procedure TcxBoldTextEdit.DoChange;
begin
  inherited;
  DataBinding.DoChanged;
end;

procedure TcxBoldTextEdit.DoEnter;
begin
  inherited;
  DataBinding.DoEnter;
end;

procedure TcxBoldTextEdit.DoExit;
begin
  inherited;
  DataBinding.DoExit;
end;
{
function TcxBoldTextEdit.ValidateComponent(
  ComponentValidator: TBoldComponentValidator;
  NamePrefix: string): Boolean;
begin
  result := DataBinding.ValidateComponent(ComponentValidator, NamePrefix);
  result := GetActiveProperties.ValidateComponent(ComponentValidator, NamePrefix) and result;
end;
}
procedure TcxBoldTextEdit.Paint;
begin
  inherited Paint;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

{ TcxCustomBoldEditDefaultValuesProvider }

function TcxCustomBoldEditDefaultValuesProvider.GetBoldElementTypeInfo: TBoldElementTypeInfo;
begin
  if Assigned({DataBinding.}BoldHandle)then
    result := {DataBinding.}BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TcxCustomBoldEditDefaultValuesProvider.IsDataAvailable: Boolean;
begin
  Result := ({DataBinding.}BoldHandle <> nil) {and (DataBinding.BoldHandle.Value <> nil)};
end;

function TcxCustomBoldEditDefaultValuesProvider.DefaultAlignment: TAlignment;
var
  lElement: TBoldElement;
begin
  Result := taLeftJustify;
  if IsDataAvailable then
  begin
    lElement := Follower.Value;
    if (lElement is TBAMoment) or (lElement is TBANumeric) then
      result := taRightJustify;
  end;
end;

function TcxCustomBoldEditDefaultValuesProvider.DefaultBlobKind: TcxBlobKind;
var
  lElement: TBoldElement;
begin
  Result := cxEdit.bkNone;
  lElement := Follower.Value;
  if (lElement is TBABlob) then
  begin
    if (lElement is TBABlobImageBMP) or (lElement is TBABlobImageJPEG) then
      Result := bkGraphic
    else
    if (lElement is TBATypedBlob) then
    begin
      Result := bkBlob;
//    TcxBlobKind = (bkNone, bkBlob, bkGraphic, bkMemo, bkOle);
{
  MIME decode to see what type it is
            (Follower.Value as TBATypedBlob).ContentType
}
    end
    else
  end;
end;

function TcxCustomBoldEditDefaultValuesProvider.DefaultCanModify: Boolean;
begin
// TODO: 'not Assigned(Follower.Element)' is a temp workaround for cases where Value is nil and hence not allowed to be modified as per TBoldRenderer.DefaultMayModify
// Follower here can be list(lookup) follower instead of single editing follower, so we can't use it for MayModify
  Result := not DefaultReadOnly and IsDataAvailable {and (Follower.Controller.MayModify(Follower) or not Assigned(Follower.Element))} {DataBinding.MayModify};
end;

function TcxCustomBoldEditDefaultValuesProvider.DefaultIsFloatValue: Boolean;
var
  lElement: TBoldElement;
begin
  lElement := nil;
  if Assigned(Follower) then
    lElement := Follower.Value;
  result := (lElement is TBAFloat) or (lElement is TBACurrency);
end;

function TcxCustomBoldEditDefaultValuesProvider.DefaultMaxLength: Integer;
var
  lElement: TBoldElement;
begin
  result := 0;
  if not Assigned(fBoldHandleFollower) then
    exit;
  lElement := Follower.Value;
  if (lElement is TBAString) and Assigned(TBAString(lElement).BoldAttributeRTInfo) then
  begin
    result := TBAString(lElement).BoldAttributeRTInfo.Length;
    if result < 1 then
      Result := inherited DefaultMaxLength;
  end
  else
    Result := inherited DefaultMaxLength;
end;

function TcxCustomBoldEditDefaultValuesProvider.IsBlob: Boolean;
var
  lElement: TBoldElement;
begin
  result := IsDataAvailable;
  if result then
  begin
    lElement := Follower.Value;
    result := (lElement is TBABlob);
  end;
end;

function TcxCustomBoldEditDefaultValuesProvider.IsCurrency: Boolean;
var
  lElement: TBoldElement;
begin
  result := IsDataAvailable;
  if result then
  begin
    lElement := Follower.Value;
    result := (lElement is TBACurrency);
  end;
end;

function TcxCustomBoldEditDefaultValuesProvider.IsValidChar(
  AChar: Char): Boolean;
begin
  result := inherited IsValidChar(AChar);
  if result and (BoldProperties is TBoldVariantFollowerController) then
    result := TBoldVariantFollowerController(BoldProperties).ValidateCharacter(AChar, Follower);
end;

{function TcxCustomBoldEditDefaultValuesProvider.GetFollower: TBoldFollower;
begin
  Result := DataBinding.Follower;
end;}

function TcxCustomBoldEditDefaultValuesProvider.GetFollower: TBoldFollower;
begin
  if Assigned(BoldHandleFollower) then
    result := BoldHandleFollower.Follower
  else
    result := nil;
end;
(*
procedure TcxCustomBoldEditDefaultValuesProvider.SetBoldHandle(
  const Value: TBoldElementHandle);
begin
  self.BoldHandleFollower.BoldHandle := Value;
// TODO: Place subscriptions instead of free notifications
{
  if BoldHandle <> Value then
  begin
    if BoldHandle <> nil then
      FFreeNotifier.RemoveSender(BoldHandle);
    BoldHandle := Value;
    if BoldHandle <> nil then
      FFreeNotifier.AddSender(BoldHandle);
  end;
}
end;
*)
procedure TcxCustomBoldEditDefaultValuesProvider.SetBoldProperties(
  const Value: TBoldFollowerController);
begin
  fBoldProperties := Value;
// TODO: Place subscriptions instead of free notifications
{
  if fBoldProperties <> Value then
  begin
    if fBoldProperties <> nil then
      FFreeNotifier.RemoveSender(fBoldProperties);
    fBoldProperties := Value;
    if fBoldProperties <> nil then
      FFreeNotifier.AddSender(fBoldProperties);
  end;
}
end;

procedure TcxCustomBoldEditDefaultValuesProvider.SetHandleFollower(
  const Value: TBoldAbstractHandleFollower);
begin
  fBoldHandleFollower := Value;
// TODO: Place subscriptions instead of free notifications
{
  if fBoldHandleFollower <> Value then
  begin
    if fBoldHandleFollower <> nil then
      FFreeNotifier.RemoveSender(fBoldHandleFollower);
    fBoldHandleFollower := Value;
    if fBoldHandleFollower <> nil then
      FFreeNotifier.AddSender(fBoldHandleFollower);
  end;
}
end;

function TcxCustomBoldEditDefaultValuesProvider.GetBoldHandle: TBoldElementHandle;
begin
  if Assigned(BoldHandleFollower) then
    result := BoldHandleFollower.BoldHandle
  else
    result := nil;
end;

constructor TcxCustomBoldEditDefaultValuesProvider.Create(
  AOwner: TPersistent);
begin
  inherited;

end;

destructor TcxCustomBoldEditDefaultValuesProvider.Destroy;
begin

  inherited;
end;

{ TcxBoldFloatEditDataBinding }

function TcxBoldFloatEditDataBinding.ValidateTypeConforms(
  aExpressionType: TBoldElementTypeInfo): string;
var
  lFloatTypeInfo: TBoldAttributeTypeInfo;
  lCurrencyTypeInfo: TBoldAttributeTypeInfo;
begin
  result := '';
  with (aExpressionType.SystemTypeInfo as TBoldSystemTypeInfo) do
  begin
    lCurrencyTypeInfo := AttributeTypeInfoByExpressionName['Currency']; // do not localize
    lFloatTypeInfo := AttributeTypeInfoByExpressionName['Float']; // do not localize
  end;
  if not aExpressionType.ConformsTo(lCurrencyTypeInfo) and not aExpressionType.ConformsTo(lFloatTypeInfo) then
    result := Format(sPossiblyBadConformance, [aExpressionType.ModelName , lFloatTypeInfo.ModelName]);
end;

{ TcxBoldDateTimeEditDataBinding }

function TcxBoldDateTimeEditDataBinding.ValidateTypeConforms(
  aExpressionType: TBoldElementTypeInfo): string;
var
  lDateTimeTypeInfo: TBoldAttributeTypeInfo;
  lDateTypeInfo: TBoldAttributeTypeInfo;
begin
  result := '';
  with (aExpressionType.SystemTypeInfo as TBoldSystemTypeInfo) do
  begin
    lDateTimeTypeInfo := AttributeTypeInfoByExpressionName['DateTime']; // do not localize
    lDateTypeInfo := AttributeTypeInfoByExpressionName['Date']; // do not localize
  end;
  if not (aExpressionType.ConformsTo(lDateTimeTypeInfo) or aExpressionType.ConformsTo(lDateTypeInfo)) then
    result := Format(sPossiblyBadConformance, [aExpressionType.ModelName , lDateTimeTypeInfo.ModelName + ' nor ' + lDateTypeInfo.ModelName]);
end;

{ TcxBoldDateEdit }

function TcxBoldDateEdit.CanDropDown: Boolean;
begin
  result := inherited CanDropDown and DataBinding.IsDataAvailable;
end;

procedure TcxBoldDateEdit.DoChange;
begin
  inherited;
  DataBinding.DoChanged;
end;

procedure TcxBoldDateEdit.DoEnter;
begin
  inherited;
  DataBinding.DoEnter;
end;

procedure TcxBoldDateEdit.DoExit;
begin
  inherited;
  DataBinding.DoExit;
end;

procedure TcxBoldDateEdit.DoOnChange;
begin
  inherited;
  if not DataBinding.IsInInternalChange then
    PostEditValue;
end;

procedure TcxBoldDateEdit.DoValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean);
begin
  inherited;
  if Assigned(DataBinding) then
    DataBinding.ValidateDisplayValue(ADisplayValue, AErrorText, AError);
end;

{ TcxBoldDateEdit }

function TcxBoldDateEdit.GetActiveProperties: TcxDateEditProperties;
begin
  Result := TcxDateEditProperties(InternalGetActiveProperties);
end;

function TcxBoldDateEdit.GetDataBinding: TcxBoldDateTimeEditDataBinding;
begin
  Result := TcxBoldDateTimeEditDataBinding(FDataBinding);
end;

class function TcxBoldDateEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxBoldDateTimeEditDataBinding;
end;

function TcxBoldDateEdit.GetProperties: TcxDateEditProperties;
begin
  Result := TcxDateEditProperties(FProperties);
end;

class function TcxBoldDateEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxDateEditProperties;
end;

procedure TcxBoldDateEdit.HidePopup(Sender: TcxControl;
  AReason: TcxEditCloseUpReason);
begin
  inherited;
// this will post the value when the calednar popup is closed with ok or enter
// the idea being to post the value even if using bapExit  
  if AReason = crEnter then
  begin
    DataBinding.InternalSetValue(EditingValue);
    if (DataBinding.BoldFollowerController.ApplyPolicy <> bapDemand) and (DataBinding.Follower.State = bfsDirty) then
      DataBinding.Follower.Apply;
  end;
end;

procedure TcxBoldDateEdit.Paint;
begin
  inherited Paint;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

procedure TcxBoldDateEdit.SetDataBinding(
  Value: TcxBoldDateTimeEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldDateEdit.SetProperties(
  Value: TcxDateEditProperties);
begin
  FProperties.Assign(Value);
end;

function TcxBoldDateEdit.ValidateKeyDown(var Key: Word;
  Shift: TShiftState): Boolean;
begin
  result := DataBinding.IsDataAvailable and inherited ValidateKeyDown(Key, Shift);
end;

function TcxBoldDateEdit.ValidateKeyPress(var Key: Char): Boolean;
begin
  result := DataBinding.MayModify and inherited ValidateKeyPress(Key);
  if not result then
    Key := #0
  else
  begin
    if (Key = #13) and (DataBinding.Follower.Controller.ApplyPolicy <> bapDemand) then
    begin
      DataBinding.Follower.Apply;
      SelectAll;
    end;
  end;
end;

{ TcxBoldMemo }

procedure TcxBoldMemo.DoChange;
begin
  inherited;
  DataBinding.DoChanged;
end;

procedure TcxBoldMemo.DoEnter;
begin
  inherited;
  DataBinding.DoEnter;
end;

procedure TcxBoldMemo.DoExit;
begin
  inherited;
  DataBinding.DoExit;
end;

procedure TcxBoldMemo.DoValidateDisplayValue(var ADisplayValue: TcxEditValue;
  var AErrorText: TCaption; var AError: Boolean);
begin
  inherited;
  if Assigned(DataBinding) then    
    DataBinding.ValidateDisplayValue(ADisplayValue, AErrorText, AError);
end;

function TcxBoldMemo.GetActiveProperties: TcxMemoProperties;
begin
  Result := TcxMemoProperties(InternalGetActiveProperties);
end;

function TcxBoldMemo.GetDataBinding: TcxBoldTextEditDataBinding;
begin
  Result := TcxBoldTextEditDataBinding(FDataBinding);
end;

class function TcxBoldMemo.GetDataBindingClass: TcxEditDataBindingClass;
begin
  result := TcxBoldTextEditDataBinding;
end;

function TcxBoldMemo.GetProperties: TcxMemoProperties;
begin
  Result := TcxMemoProperties(FProperties);
end;

class function TcxBoldMemo.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMemoProperties;
end;

procedure TcxBoldMemo.Paint;
begin
  inherited Paint;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

procedure TcxBoldMemo.SetDataBinding(Value: TcxBoldTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldMemo.SetProperties(Value: TcxMemoProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxBoldTimeEdit }

procedure TcxBoldTimeEdit.DoChange;
begin
  inherited;
  DataBinding.DoChanged;
end;

procedure TcxBoldTimeEdit.DoEnter;
begin
  inherited;
  DataBinding.DoEnter;
end;

procedure TcxBoldTimeEdit.DoExit;
begin
  inherited;
  DataBinding.DoExit;
end;

procedure TcxBoldTimeEdit.DoValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean);
begin
  inherited;
  if Assigned(DataBinding) then
    DataBinding.ValidateDisplayValue(ADisplayValue, AErrorText, AError);
end;

function TcxBoldTimeEdit.GetActiveProperties: TcxTimeEditProperties;
begin
  Result := TcxTimeEditProperties(InternalGetActiveProperties);
end;

function TcxBoldTimeEdit.GetDataBinding: TcxBoldTimeEditDataBinding;
begin
  Result := TcxBoldTimeEditDataBinding(FDataBinding);
end;

class function TcxBoldTimeEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  result := TcxBoldTimeEditDataBinding;
end;

function TcxBoldTimeEdit.GetProperties: TcxTimeEditProperties;
begin
  Result := TcxTimeEditProperties(FProperties);
end;

class function TcxBoldTimeEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxTimeEditProperties;
end;

procedure TcxBoldTimeEdit.Paint;
begin
  inherited Paint;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

procedure TcxBoldTimeEdit.SetDataBinding(
  Value: TcxBoldTimeEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldTimeEdit.SetProperties(Value: TcxTimeEditProperties);
begin
  FProperties.Assign(Value);
end;

function TcxBoldTimeEdit.ValidateKeyDown(var Key: Word;
  Shift: TShiftState): Boolean;
begin
  result := DataBinding.IsDataAvailable and inherited ValidateKeyDown(Key, Shift);
end;

function TcxBoldTimeEdit.ValidateKeyPress(var Key: Char): Boolean;
begin
  result := DataBinding.MayModify and inherited ValidateKeyPress(Key);
  if not result then
    Key := #0
  else
  begin
    if (Key = #13) and (DataBinding.Follower.Controller.ApplyPolicy <> bapDemand) then
    begin
      DataBinding.Follower.Apply;
      SelectAll;
    end;
  end;
end;

{ TcxBoldCurrencyEdit }

procedure TcxBoldCurrencyEdit.DoEnter;
begin
  inherited;
  DataBinding.DoEnter;
end;

procedure TcxBoldCurrencyEdit.DoExit;
begin
  inherited;
  DataBinding.DoExit;
end;

procedure TcxBoldCurrencyEdit.DoValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean);
begin
  inherited;
  if Assigned(DataBinding) then
    DataBinding.ValidateDisplayValue(ADisplayValue, AErrorText, AError);
end;

function TcxBoldCurrencyEdit.GetActiveProperties: TcxCurrencyEditProperties;
begin
  Result := TcxCurrencyEditProperties(InternalGetActiveProperties);
end;

function TcxBoldCurrencyEdit.GetDataBinding: TcxBoldCurrencyEditDataBinding;
begin
  Result := TcxBoldCurrencyEditDataBinding(FDataBinding);
end;

class function TcxBoldCurrencyEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxBoldCurrencyEditDataBinding;
end;

function TcxBoldCurrencyEdit.GetProperties: TcxCurrencyEditProperties;
begin
  Result := TcxCurrencyEditProperties(FProperties);
end;

class function TcxBoldCurrencyEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCurrencyEditProperties;
end;

procedure TcxBoldCurrencyEdit.Paint;
begin
  inherited Paint;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

procedure TcxBoldCurrencyEdit.SetDataBinding(
  Value: TcxBoldCurrencyEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldCurrencyEdit.SetProperties(
  Value: TcxCurrencyEditProperties);
begin
  FProperties.Assign(Value);
end;

procedure TcxBoldCurrencyEdit.DoChange;
begin
  inherited;
  DataBinding.DoChanged;
end;

{ TcxBoldTextEditDataBinding }
{
procedure TcxBoldTextEditDataBinding.InternalSetValue(
  const aValue: TcxEditValue);
begin
  if VarIsNull(aValue) then
    BoldProperties.MayHaveChanged('', Follower)
  else
    BoldProperties.MayHaveChanged(aValue, Follower);
end;
}
{ TcxBoldMaskEdit }

procedure TcxBoldMaskEdit.DoEnter;
begin
  inherited;
  DataBinding.DoEnter;
end;

procedure TcxBoldMaskEdit.DoExit;
begin
  inherited;
  DataBinding.DoExit;
end;

procedure TcxBoldMaskEdit.DoValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean);
begin
  inherited;
  if Assigned(DataBinding) then
    DataBinding.ValidateDisplayValue(ADisplayValue, AErrorText, AError);
end;

function TcxBoldMaskEdit.GetActiveProperties: TcxMaskEditProperties;
begin
  Result := TcxMaskEditProperties(InternalGetActiveProperties);
end;

function TcxBoldMaskEdit.GetDataBinding: TcxBoldTextEditDataBinding;
begin
  Result := TcxBoldTextEditDataBinding(FDataBinding);
end;

class function TcxBoldMaskEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxBoldTextEditDataBinding;
end;

function TcxBoldMaskEdit.GetProperties: TcxMaskEditProperties;
begin
  Result := TcxMaskEditProperties(FProperties);
end;

class function TcxBoldMaskEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMaskEditProperties;
end;

procedure TcxBoldMaskEdit.Paint;
begin
  inherited Paint;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

procedure TcxBoldMaskEdit.SetDataBinding(
  Value: TcxBoldTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldMaskEdit.SetProperties(Value: TcxMaskEditProperties);
begin
  FProperties.Assign(Value);
end;

function TcxBoldMaskEdit.SupportsSpelling: Boolean;
begin
  Result := IsTextInputMode;
end;

{ TcxBoldCheckBoxEditDataBinding }


function TcxBoldCheckBoxEditDataBinding.MayModify: boolean;
begin
  result := inherited MayModify; //and (fCurrentElementType is TBoldAttributeTypeInfo) and TBoldAttributeTypeInfo(fCurrentElementType).AttributeClass.InheritsFrom(TBABoolean);
end;


function TcxBoldCheckBoxEditDataBinding.ValidateTypeConforms(
  aExpressionType: TBoldElementTypeInfo): string;
var
  lBooleanTypeInfo: TBoldAttributeTypeInfo;
begin
  result := '';
  with (aExpressionType.SystemTypeInfo as TBoldSystemTypeInfo) do
  begin
    lBooleanTypeInfo := AttributeTypeInfoByExpressionName['Boolean']; // do not localize
  end;
  if not aExpressionType.ConformsTo(lBooleanTypeInfo) then
    result := Format(sPossiblyBadConformance, [aExpressionType.ModelName , lBooleanTypeInfo.ModelName]);
end;

function TcxBoldCheckBoxEditDataBinding.ImmediatePost: boolean;
begin
  result := true;
end;

{ TcxBoldCheckBox }

procedure TcxBoldCheckBox.DoChange;
begin
  inherited;
  DataBinding.DoChanged;
end;

procedure TcxBoldCheckBox.DoEnter;
begin
  inherited;
  DataBinding.DoEnter;
end;

procedure TcxBoldCheckBox.DoExit;
begin
  inherited;
  DataBinding.DoExit;
end;

procedure TcxBoldCheckBox.DoValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean);
begin
  inherited;
  ADisplayValue := EditingValue;
  if Assigned(DataBinding) then    
    DataBinding.ValidateDisplayValue(ADisplayValue, AErrorText, AError);
end;

function TcxBoldCheckBox.GetActiveProperties: TcxCheckBoxProperties;
begin
  Result := TcxCheckBoxProperties(InternalGetActiveProperties);
end;

function TcxBoldCheckBox.GetDataBinding: TcxBoldCheckBoxEditDataBinding;
begin
  Result := FDataBinding as TcxBoldCheckBoxEditDataBinding;
end;

class function TcxBoldCheckBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  result := TcxBoldCheckBoxEditDataBinding;
end;

function TcxBoldCheckBox.GetProperties: TcxCheckBoxProperties;
begin
  Result := TcxCheckBoxProperties(FProperties);
end;

class function TcxBoldCheckBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckBoxProperties;
end;

procedure TcxBoldCheckBox.Initialize;
begin
  inherited;
  if IsDesigning and not IsLoading then
  begin
    _ValidateEdit(self);
  end;
end;

procedure TcxBoldCheckBox.Paint;
begin
  inherited;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 3);
end;

procedure TcxBoldCheckBox.SetDataBinding(
  Value: TcxBoldCheckBoxEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldCheckBox.SetProperties(Value: TcxCheckBoxProperties);
begin
  FProperties.Assign(Value);
end;

procedure TcxBoldCheckBox.Toggle;
begin
  // this is a bit hacky, would be better if we can set it somewhere once
  FProperties.ImmediatePost := DataBinding.ImmediatePost;
  if CanModify and (DataBinding.MayModify) and Assigned(DataBinding.Follower.Element) then
  begin
    inherited Toggle;
    DataBinding.Follower.Apply;
  end;
end;

{ TcxBoldComboBoxProperties }

procedure TcxBoldComboBoxProperties._DeleteItem(Index: Integer;
  OwningFollower: TBoldFollower);
begin
  if not Items.Updating then
    Items.BeginUpdate;
  Items.Delete(index);
end;

procedure TcxBoldComboBoxProperties._InsertItem(Index: Integer; Follower: TBoldFollower);
begin
  if not Items.Updating then
    Items.BeginUpdate;
  if Assigned(Follower) then
  begin
    Follower.EnsureDisplayable;
    Items.Insert(Follower.Index, VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower)));
  end
  else
  begin
    Items.Insert(Index, VarToStr(BoldRowProperties.NilRepresentation));
  end;
end;

procedure TcxBoldComboBoxProperties._ReplaceItem(Index: Integer;
  Follower: TBoldFollower);
begin
  if not Items.Updating then
    Items.BeginUpdate;
  Follower.EnsureDisplayable;
  Items.Strings[Index] := VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower));
end;

procedure TcxBoldComboBoxProperties._RowAfterMakeUptoDate(
  Follower: TBoldFollower);
var
  index: Integer;
  NewValue: String;
begin
  index := Follower.index;
  if (index > -1) and (index < Items.Count) then
  begin
    NewValue := VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower));
    if NewValue <> Items[index] then
      Items[index] := NewValue;
  end;
//  LookupDataChanged(self);
  // forces a redisplay of the edit-area, the windows component might go blank if the active row is removed and then reinserted
//  fBoldHandleFollower.Follower.MarkValueOutOfDate; // do we really need this here ? Danny
end;

procedure TcxBoldComboBoxProperties._AfterMakeUptoDate(
  Follower: TBoldFollower);
begin
  fBoldRowProperties.AfterMakeUptoDate := _RowAfterMakeUptoDate;
  if Items.Updating then
    Items.EndUpdate;
end;

procedure TcxBoldComboBoxProperties._BeforeMakeUptoDate(
  Follower: TBoldFollower);
begin
  fBoldRowProperties.AfterMakeUptoDate := nil;
end;

constructor TcxBoldComboBoxProperties.Create(AOwner: TPersistent);
var
  lMatchObject: TComponent;
//  lBoldAwareViewItem: IBoldAwareViewItem;
begin
  inherited;
  if aOwner is TComponent then
    lMatchObject := aOwner as TComponent
  else
    lMatchObject := nil;
  fBoldRowProperties := TBoldVariantFollowerController.Create(lMatchObject);
  fBoldRowProperties.AfterMakeUptoDate := _RowAfterMakeUptoDate;
  fBoldRowProperties.OnGetContextType := GetContextForBoldRowProperties;
  fBoldListProperties := TBoldComboListController.Create(lMatchObject, fBoldRowProperties);
  with fBoldListProperties do
  begin
    OnAfterInsertItem := _InsertItem;
    OnAfterDeleteItem := _DeleteItem;
    OnReplaceitem := _ReplaceItem;
    BeforeMakeUptoDate := _BeforeMakeUptoDate;
    AfterMakeUptoDate := _AfterMakeUptoDate;
  end;
  fListHandleFollower := TBoldListHandleFollower.Create(Owner, fBoldListProperties);
  ImmediatePost := false;
  BoldSelectChangeAction := bdcsSetValue;
//  if aOwner.GetInterface(IBoldAwareViewItem, lBoldAwareViewItem) then
//    BoldSetValueExpression := lBoldAwareViewItem.DataBinding.BoldProperties.Expression;
end;

destructor TcxBoldComboBoxProperties.Destroy;
begin
  FreeAndNil(fListHandleFollower);
  FreeAndNil(fBoldListProperties);
  FreeAndNil(fBoldRowProperties);
  inherited;
end;

function TcxBoldComboBoxProperties.GetBoldListHandle: TBoldAbstractListHandle;
begin
  Result := fListHandleFollower.BoldHandle;
end;

function TcxBoldComboBoxProperties.GetContextForBoldRowProperties: TBoldElementTypeInfo;
begin
  if assigned(BoldLookupListHandle) then
    result := BoldLookupListHandle.StaticBoldType
  else
    result := nil;
end;

function TcxBoldComboBoxProperties.GetListFollower: TBoldFollower;
begin
  Result := fListHandleFollower.Follower;
end;

function TcxBoldComboBoxProperties.IsDisplayValueValid(
  var DisplayValue: TcxEditValue; AEditFocused: Boolean): Boolean;
begin
//  result := cxEditVarEquals(DisplayValue, BoldRowProperties.NilRepresentation) or inherited IsDisplayValueValid(DisplayValue, AEditFocused)
  if not VarIsNull(BoldRowProperties.NilRepresentation) and cxEditVarEquals(DisplayValue, BoldRowProperties.NilRepresentation) then
    result := true
  else
    result := inherited IsDisplayValueValid(DisplayValue, AEditFocused);
end;

function TcxBoldComboBoxProperties.IsEditValueValid(var EditValue: TcxEditValue;
  AEditFocused: Boolean): Boolean;
begin
//  result := inherited IsEditValueValid(EditValue, AEditFocused) or cxEditVarEquals(EditValue, BoldRowProperties.NilRepresentation);
  result := inherited IsEditValueValid(EditValue, AEditFocused);
  if not result then
    result := cxEditVarEquals(EditValue, BoldRowProperties.NilRepresentation);
end;

procedure TcxBoldComboBoxProperties.SetBoldListHandle(
  const Value: TBoldAbstractListHandle);
begin
  fListHandleFollower.BoldHandle := value;
end;

procedure TcxBoldComboBoxProperties.SetBoldListProperties(
  const Value: TBoldComboListController);
begin
  fBoldListProperties.Assign(Value);
end;

procedure TcxBoldComboBoxProperties.SetRowProperties(
  const Value: TBoldVariantFollowerController);
begin
  fBoldRowProperties.Assign(Value);
end;

procedure TcxBoldComboBoxProperties.SetBoldSelectChangeAction(
  Value: TBoldComboSelectChangeAction);
begin
//  if (Value = bdcsSetReference) and assigned(BoldHandle) and not BoldHandle.CanSetValue then
//    raise EBold.Create(sChangeActionCannotBeSetReference);
  fBoldSelectChangeAction := Value;
end;

procedure TcxBoldComboBoxProperties.SetStoredValue(
  aValue: Variant;
  aBoldHandle: TBoldElementHandle;
  aEdit: TcxCustomEdit;
  aFollower: TBoldFollower;
  var aDone: boolean);
var
  LocalSelectedElement: TBoldElement;
  lItemIndex: Integer;
begin
  Assert(aEdit is TcxCustomComboBox);
  lItemIndex := (aEdit as TcxCustomComboBox).ItemIndex;
  if lItemIndex = -1 then
  begin
    // if DropDownListStyle = lsEditList then we might want to let default handling make modifications
    // on other cases we set aDone := true as we're sure that with a fixed list item that isn't in the list won't make changes.
    if DropDownListStyle <> lsEditList then
      aDone := true;
    exit;
  end
  else
  begin
    if ((lItemIndex = LookupListFollower.SubFollowerCount-1) and (BoldLookupListProperties.NilElementMode = neAddLast))
    or ((lItemIndex = 0) and (BoldLookupListProperties.NilElementMode = neInsertFirst)) then
    begin
      LocalSelectedElement := nil
    end
    else
    begin
      if (BoldLookupListProperties.NilElementMode = neInsertFirst) then
        dec(lItemIndex);
      LocalSelectedElement := BoldLookupListHandle.List[lItemIndex];
    end;
  end;
  InternalComboSetValue(aBoldHandle, aFollower, LocalSelectedElement, BoldSelectChangeAction, BoldSetValueExpression, BoldLookupListHandle, aValue);
  aDone := true;
end;

class function TcxBoldComboBoxProperties.GetContainerClass: TcxContainerClass;
begin
  result := inherited GetContainerClass;
//  result := TcxBoldComboBox;
end;

function TcxBoldComboBoxProperties.GetAlwaysPostEditValue: Boolean;
begin
  result := false;
end;

function TcxBoldComboBoxProperties.ValidateComponent(
  ComponentValidator: TBoldComponentValidator;
  NamePrefix: string): Boolean;
var
  lContext: TBoldElementTypeInfo;
  lName: string;
  lcxBoldEditDataBinding: TcxBoldEditDataBinding;
begin
  result := true;
  if (Owner is TComponent) and (TComponent(Owner).Name <> '') then
    lName := TComponent(Owner).Name
  else
  if Assigned(Owner) then
    lName := Owner.ClassName
  else
    lName := ClassName;

//  OutPutDebugString(PChar(lName));

  lContext := GetContextForBoldRowProperties;
  if assigned(lContext) then
  begin
    result := ComponentValidator.ValidateExpressionInContext(
      BoldRowProperties.Expression,
      lContext,
      format('%s %s.BoldRowProperties.Expression', [NamePrefix, lName]), BoldRowProperties.VariableList) and result; // do not localize

    if (BoldSelectChangeAction = bdcsSetValue) and (Owner is TcxCustomEdit) then
    begin
      lcxBoldEditDataBinding := TcxCustomEditAccess(TcxCustomEdit(Owner)).DataBinding as TcxBoldEditDataBinding;
      lContext := lcxBoldEditDataBinding.GetContextType;
      result := ComponentValidator.ValidateExpressionInContext(
        BoldSetValueExpression,
        lContext,
        format('%s %s.BoldSetValueExpression', [NamePrefix, lName]),
        lcxBoldEditDataBinding.BoldProperties.VariableList) and result; // do not localize
    end;
  end;
end;

function TcxBoldComboBoxProperties.BoldElementToEditValue(
  aFollower: TBoldFollower; aElement: TBoldElement; aEdit: TcxCustomEdit): variant;
begin
  result := BoldRowProperties.GetAsVariant(aFollower);
end;

procedure TcxBoldComboBoxProperties.Assign(Source: TPersistent);
begin
  if Source is TcxBoldComboBoxProperties then
  begin
    BeginUpdate;
    try
      BoldLookupListHandle := TcxBoldComboBoxProperties(Source).BoldLookupListHandle;
      BoldLookupListProperties := TcxBoldComboBoxProperties(Source).BoldLookupListProperties;
      BoldRowProperties := TcxBoldComboBoxProperties(Source).BoldRowProperties;
      BoldSetValueExpression := TcxBoldComboBoxProperties(Source).BoldSetValueExpression;
      BoldSelectChangeAction := TcxBoldComboBoxProperties(Source).BoldSelectChangeAction;
      inherited Assign(Source);


//      (FIDefaultValuesProvider.GetInstance as TcxCustomBoldEditDefaultValuesProvider).BoldHandleFollower := (TcxBoldComboBoxProperties(Source).FIDefaultValuesProvider.GetInstance as TcxCustomBoldEditDefaultValuesProvider).BoldHandleFollower;
//      (FIDefaultValuesProvider.GetInstance as TcxCustomBoldEditDefaultValuesProvider).BoldProperties  := (TcxBoldComboBoxProperties(Source).FIDefaultValuesProvider.GetInstance as TcxCustomBoldEditDefaultValuesProvider).BoldProperties;

      TBoldQueueable.DisplayAll;
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

function TcxBoldComboBoxProperties.CanEdit(aBoldHandle: TBoldElementHandle;
  aFollower: TBoldFollower): boolean;
begin
  result := (LookupListFollower.SubFollowerCount > 0) or (DropDownListStyle = lsEditList);
end;

procedure TcxBoldComboBoxProperties.SetBoldSetValueExpression(
  const Value: TBoldExpression);
begin
  fBoldSetValueExpression := Value;
  if Owner is TcxCustomEdit and (TcxCustomEdit(Owner).IsDesigning) and not (TcxCustomEdit(Owner).IsLoading) then
  begin
    _ValidateEdit(TcxCustomEdit(Owner));
  end;  
end;

{ TcxBoldComboBox }

function TcxBoldComboBox.CanDropDown: Boolean;
var
  lElementToAssignTo: TBoldElement;
begin
  result := inherited CanDropDown and DataBinding.IsDataAvailable;
  if result then
  begin
    case ActiveProperties.BoldSelectChangeAction of
      bdcsSetValue:
        begin
          if (Assigned(DataBinding.Follower.Element)) then
          begin
            if trim(ActiveProperties.BoldSetValueExpression) <> '' then
            begin
              lElementToAssignTo := DataBinding.Follower.Element.EvaluateExpressionAsDirectElement(ActiveProperties.BoldSetValueExpression, TBoldFollowerControllerAccess(DataBinding.Follower.Controller).VariableList);
              result := assigned(lElementToAssignTo) and lElementToAssignTo.Mutable;
            end;
          end
          else
            result := false;
        end;
    end;
  end;
end;

procedure TcxBoldComboBox.DoEnter;
begin
  inherited;
  DataBinding.DoEnter;
end;

procedure TcxBoldComboBox.DoExit;
begin
  inherited;
  DataBinding.DoExit;
end;

procedure TcxBoldComboBox.DoValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean);
begin
  inherited;
  if Assigned(DataBinding) then  
    DataBinding.ValidateDisplayValue(ADisplayValue, AErrorText, AError);
end;

function TcxBoldComboBox.GetActiveProperties: TcxBoldComboBoxProperties;
begin
  Result := TcxBoldComboBoxProperties(InternalGetActiveProperties);
end;

function TcxBoldComboBox.GetDataBinding: TcxBoldComboBoxEditDataBinding;
begin
  Result := TcxBoldComboBoxEditDataBinding(FDataBinding);
end;

class function TcxBoldComboBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  result := TcxBoldComboBoxEditDataBinding; //TcxBoldTextEditDataBinding;
end;

function TcxBoldComboBox.GetProperties: TcxBoldComboBoxProperties;
begin
  Result := TcxBoldComboBoxProperties(FProperties);
end;

class function TcxBoldComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  result := TcxBoldComboBoxProperties;
end;

procedure TcxBoldComboBox.Paint;
begin
  inherited Paint;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

procedure TcxBoldComboBox.SetDataBinding(
  Value: TcxBoldComboBoxEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldComboBox.SetProperties(Value: TcxBoldComboBoxProperties);
begin
  FProperties.Assign(Value);
end;

function TcxBoldComboBox.SupportsSpelling: Boolean;
begin
  Result := IsTextInputMode;
end;
{
function TcxBoldComboBox.ValidateComponent(
  ComponentValidator: TBoldComponentValidator;
  NamePrefix: string): Boolean;
begin
  result := DataBinding.ValidateComponent(ComponentValidator, NamePrefix);
  result := GetActiveProperties.ValidateComponent(ComponentValidator, NamePrefix) and result;
end;
}
{ TcxBoldSpinEdit }

procedure TcxBoldSpinEdit.DoEnter;
begin
  inherited;
  DataBinding.DoEnter;
end;

procedure TcxBoldSpinEdit.DoExit;
begin
  inherited;
  DataBinding.DoExit;
end;

procedure TcxBoldSpinEdit.DoValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean);
begin
  inherited;
  if Assigned(DataBinding) then
    DataBinding.ValidateDisplayValue(ADisplayValue, AErrorText, AError);
end;

function TcxBoldSpinEdit.GetActiveProperties: TcxSpinEditProperties;
begin
  Result := TcxSpinEditProperties(InternalGetActiveProperties);
end;

function TcxBoldSpinEdit.GetDataBinding: TcxBoldNumericEditDataBinding;
begin
  Result := TcxBoldNumericEditDataBinding(FDataBinding);
end;

class function TcxBoldSpinEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxBoldNumericEditDataBinding;
end;

function TcxBoldSpinEdit.GetProperties: TcxSpinEditProperties;
begin
  Result := TcxSpinEditProperties(FProperties);
end;

class function TcxBoldSpinEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxSpinEditProperties;
end;

procedure TcxBoldSpinEdit.Paint;
begin
  inherited Paint;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

procedure TcxBoldSpinEdit.SetDataBinding(
  Value: TcxBoldNumericEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldSpinEdit.SetProperties(Value: TcxSpinEditProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxBoldButtonEdit }

procedure TcxBoldButtonEdit.DoEnter;
begin
  inherited;
  DataBinding.DoEnter;
end;

procedure TcxBoldButtonEdit.DoExit;
begin
  inherited;
  DataBinding.DoExit;
end;

procedure TcxBoldButtonEdit.DoValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean);
begin
  inherited;
  if Assigned(DataBinding) then    
    DataBinding.ValidateDisplayValue(ADisplayValue, AErrorText, AError);
end;

function TcxBoldButtonEdit.GetActiveProperties: TcxButtonEditProperties;
begin
  Result := TcxButtonEditProperties(InternalGetActiveProperties);
end;

function TcxBoldButtonEdit.GetDataBinding: TcxBoldTextEditDataBinding;
begin
  Result := TcxBoldTextEditDataBinding(FDataBinding);
end;

class function TcxBoldButtonEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxBoldTextEditDataBinding;
end;

function TcxBoldButtonEdit.GetProperties: TcxButtonEditProperties;
begin
  Result := TcxButtonEditProperties(FProperties);
end;

class function TcxBoldButtonEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxButtonEditProperties;
end;

procedure TcxBoldButtonEdit.Paint;
begin
  inherited Paint;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

procedure TcxBoldButtonEdit.SetDataBinding(
  Value: TcxBoldTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldButtonEdit.SetProperties(Value: TcxButtonEditProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxBoldHyperLinkEdit }

procedure TcxBoldHyperLinkEdit.DoEnter;
begin
  inherited;
  DataBinding.DoEnter;
end;

procedure TcxBoldHyperLinkEdit.DoExit;
begin
  inherited;
  DataBinding.DoExit;
end;

procedure TcxBoldHyperLinkEdit.DoValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean);
begin
  inherited;
  if Assigned(DataBinding) then
    DataBinding.ValidateDisplayValue(ADisplayValue, AErrorText, AError);
end;

function TcxBoldHyperLinkEdit.GetActiveProperties: TcxHyperLinkEditProperties;
begin
  Result := TcxHyperLinkEditProperties(InternalGetActiveProperties);
end;

function TcxBoldHyperLinkEdit.GetDataBinding: TcxBoldTextEditDataBinding;
begin
  Result := TcxBoldTextEditDataBinding(FDataBinding);
end;

class function TcxBoldHyperLinkEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxBoldTextEditDataBinding;
end;

function TcxBoldHyperLinkEdit.GetProperties: TcxHyperLinkEditProperties;
begin
  Result := TcxHyperLinkEditProperties(FProperties);
end;

class function TcxBoldHyperLinkEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxHyperLinkEditProperties;
end;

procedure TcxBoldHyperLinkEdit.Paint;
begin
  inherited Paint;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

procedure TcxBoldHyperLinkEdit.SetDataBinding(
  Value: TcxBoldTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldHyperLinkEdit.SetProperties(
  Value: TcxHyperLinkEditProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxBoldProgressBar }

function TcxBoldProgressBar.GetActiveProperties: TcxProgressBarProperties;
begin
  Result := TcxProgressBarProperties(InternalGetActiveProperties);
end;

function TcxBoldProgressBar.GetDataBinding: TcxBoldNumericEditDataBinding;
begin
  Result := TcxBoldNumericEditDataBinding(FDataBinding);
end;

class function TcxBoldProgressBar.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxBoldNumericEditDataBinding;
end;

function TcxBoldProgressBar.GetProperties: TcxProgressBarProperties;
begin
  Result := TcxProgressBarProperties(FProperties);
end;

class function TcxBoldProgressBar.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxProgressBarProperties;
end;

procedure TcxBoldProgressBar.Initialize;
begin
  inherited;
  if IsDesigning and not IsLoading then
  begin
    _ValidateEdit(self);
  end;
end;

procedure TcxBoldProgressBar.Paint;
begin
  inherited Paint;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

procedure TcxBoldProgressBar.SetDataBinding(
  Value: TcxBoldNumericEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldProgressBar.SetProperties(
  Value: TcxProgressBarProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxBarBoldEditItem }

procedure TcxBarBoldEditItem._AfterMakeUptoDate(Follower: TBoldFollower);
var
  lValue: variant;//string;
  lElement: TBoldElement;
  lIcxBoldEditProperties: IcxBoldEditProperties;  
begin
//  lValue := BoldProperties.GetAsVariant(Follower);
  if fInternalChange = 0 then
  begin
    lElement := Follower.Value;
    if Supports(Properties, IcxBoldEditProperties, lIcxBoldEditProperties) then
    begin
//      if Assigned(lElement) then
        lValue := lIcxBoldEditProperties.BoldElementToEditValue(Follower, lElement, nil);
    end
    else
    begin
      lValue := TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower);
      if VarIsEmpty(lValue) then
        lValue := Null;
    end;

    inc(fInternalChange);
    try
      if not cxEditVarEquals(EditValue, lValue) then
        EditValue := lValue;
    finally
      dec(fInternalChange);
    end;
  end;
end;

{
function TcxBarBoldEditItem.CanEdit: Boolean;
begin
  result := inherited CanEdit;
end;
}
{
procedure TcxBarBoldEditItem.DoEditValueChanged(Sender: TObject);
begin
  inherited;

end;
}
constructor TcxBarBoldEditItem.Create(AOwner: TComponent);
begin
  inherited;
  fBoldProperties := TBoldVariantFollowerController.Create(Self);
  fBoldProperties.AfterMakeUptoDate := _AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextType;
  fBoldHandleFollower := TBoldElementHandleFollower.Create(Owner, fBoldProperties);
//  self.OnChange := EditValueChanged;
//  self.OnEnter := EditEnter;
  self.OnExit := EditExit;
end;

destructor TcxBarBoldEditItem.Destroy;
begin
  case BoldProperties.ApplyPolicy of
    bapChange, bapExit: try
      Follower.Apply;
    except
      Follower.DiscardChange;
    end;
    bapDemand: Follower.DiscardChange;
  end;
  FreeAndNil(fBoldHandleFollower);
  FreeAndNil(fBoldProperties);
  inherited;
end;

function TcxBarBoldEditItem.GetBoldHandle: TBoldElementHandle;
begin
  Result := fBoldHandleFollower.BoldHandle;
end;

function TcxBarBoldEditItem.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TcxBarBoldEditItem.GetFollower: TBoldFollower;
begin
  Result := fBoldHandleFollower.Follower;
end;

procedure TcxBarBoldEditItem.SetBoldHandle(
  const Value: TBoldElementHandle);
begin
  fBoldHandleFollower.BoldHandle := value;
end;

procedure TcxBarBoldEditItem.SetBoldProperties(
  const Value: TBoldVariantFollowerController);
begin
  fBoldProperties.Assign(Value);
end;

procedure TcxBarBoldEditItem.EditValueChanged(Sender: TObject);
var
  lIcxBoldEditProperties: IcxBoldEditProperties;
  lDone: Boolean;
  lEdit: TcxCustomEdit;
begin
  if fInternalChange = 0 then
  begin
    lDone := false;
    lEdit := Sender as TcxCustomEdit;
    if Supports(Properties, IcxBoldEditProperties, lIcxBoldEditProperties) then
    begin
      lIcxBoldEditProperties.SetStoredValue(Null, BoldHandle, lEdit, Follower, lDone);
    end;
    if not lDone then
    begin
      if VarIsNull(EditValue) then
        BoldProperties.MayHaveChanged('', Follower)
      else
      BoldProperties.MayHaveChanged(EditValue, Follower);
    end;
    TBoldQueueable.DisplayAll;
  end;
end;

procedure TcxBarBoldEditItem.EditExit(Sender: TObject);
begin
  if (Follower.Controller.ApplyPolicy <> bapDemand) then
    Follower.Apply;
end;

class function TcxCustomBoldTextEditProperties.GetContainerClass: TcxContainerClass;
begin
  result := inherited GetContainerClass;
//  result := TcxBoldTextEdit;
end;

procedure TcxBarBoldEditItem.DoEnter;
begin
  inherited;

end;

procedure TcxBarBoldEditItem.DoExit;
begin
  inherited;
end;

function TcxBarBoldEditItem.GetControlClass(
  AIsVertical: Boolean): TdxBarItemControlClass;
begin
  if AIsVertical then
    Result := inherited GetControlClass(AIsVertical)
  else
    Result := TcxBarBoldEditItemControl;
end;

procedure TcxBarBoldEditItem.KeyPress(var Key: Char);
begin
  inherited;
  if (not Follower.Controller.MayModify(Follower)) or (not BoldProperties.ValidateCharacter(Key, Follower)) then
    Key := #0;
end;

{ TcxBarBoldEditItemControl }
{
procedure TcxBarBoldEditItemControl.DoPostEditValue(Sender: TObject);
begin
  inherited;

end;

procedure TcxBarBoldEditItemControl.DoValidate(Sender: TObject;
  var DisplayValue: TcxEditValue; var ErrorText: TCaption;
  var Error: Boolean);
begin
  inherited;
//  Error := Error or not (Item as TcxBarBoldEditItem).BoldProperties.ValidateString(DisplayValue, (Item as TcxBarBoldEditItem).Follower);
//  if Error then
//    Errortext := '+' + Errortext;
//  (Item as TcxBarBoldEditItem).EditValueChanged(sender as TcxCustomEdit);
end;
}
//type
//  TcxCustomDropDownEditPropertiesAccess = Class(TcxCustomDropDownEditProperties);

procedure TcxBarBoldEditItemControl.RestoreDisplayValue;
begin
  inherited;
//  if Properties is TcxCustomDropDownEditProperties then
//    TcxCustomDropDownEditPropertiesAccess(Properties).AlwaysPostEditValue := true;
  Properties.ImmediatePost := true;
end;

procedure TcxBarBoldEditItemControl.StoreDisplayValue;
begin
  inherited;
  (Item as TcxBarBoldEditItem).EditValueChanged(Edit);
//  if Edit is TcxCustomComboBox then
end;

function TcxCustomBoldTextEditProperties.ValidateComponent(
  ComponentValidator: TBoldComponentValidator;
  NamePrefix: string): Boolean;
var
  lContext: TBoldElementTypeInfo;
  lName: string;
begin
  result := true;
  if (Owner is TComponent) and (TComponent(Owner).Name <> '') then
    lName := TComponent(Owner).Name
  else
    lName := ClassName;
  if Assigned(BoldLookupListHandle) then
  begin
    lContext := GetContextForBoldRowProperties;
    if not Assigned(lContext) then
      BoldLog.LogFmt(sNoContext, [lName])
    else
    begin
      result := ComponentValidator.ValidateExpressionInContext(
        BoldRowProperties.Expression,
        lContext,
        format('%s %s.BoldRowProperties.Expression', [NamePrefix, lName]),
        BoldRowProperties.VariableList ) and result; // do not localize
    end;
  end;
end;

{ TcxBoldDateNavigator }

{$IFDEF DevExScheduler}

procedure TcxBoldDateNavigator._AfterMakeUptoDate(Follower: TBoldFollower);
var
  lValue: variant;//string;
  lElement: TBoldElement;
  i,j: integer;
  lBoldComponentValidator: TBoldComponentValidator;
begin
  if IsDesigning then
  begin
    lBoldComponentValidator := TBoldComponentValidator.Create;
    try
      ValidateComponent(lBoldComponentValidator, Name);
    finally
      lBoldComponentValidator.free;
    end;
    exit;
  end;
  lElement := Follower.Value;
  if Assigned(lElement) then
    lValue := lElement.AsVariant
  else
    lValue := Null;
  InnerDateNavigator.EventOperations.ReadOnly := not Assigned(lElement);
  inc(fInternalChange);
  try
    if Follower = StartFollower then
    begin
      if VarIsNull(lValue) or (lValue = 0) then
      begin
        if (self.Date <> NullDate) then
        begin
          SelectedDays.Clear;
          DateNavigator.Refresh;
        end
      end
      else
      begin
        if lValue <> self.Date then
          self.Date := lValue;
      end;
    end
    else
    begin
      if not VarIsNull(lValue) and (lValue <> 0) and (lValue <> RealLastDate) and (self.date <> NullDate) then
      begin
        j := Trunc(self.date);
        SelectedDays.Clear;
        for i := j to lValue do
          SelectedDays.Add(i);
        DateNavigator.Refresh;
      end;
    end;
  finally
    dec(fInternalChange);
  end;
end;

constructor TcxBoldDateNavigator.Create(AOwner: TComponent);
begin
  inherited;
//  fBoldProperties := TBoldVariantFollowerController.Create(Self);
//  fBoldProperties.AfterMakeUptoDate := _AfterMakeUptoDate;
//  fBoldProperties.OnGetContextType := GetContextType;

  fBoldStartProperties := TBoldVariantFollowerController.Create(Self);
  fBoldStartProperties.AfterMakeUptoDate := _AfterMakeUptoDate;
  fBoldStartProperties.OnGetContextType := GetStartContextType;

  fBoldEndProperties := TBoldVariantFollowerController.Create(Self);
  fBoldEndProperties.AfterMakeUptoDate := _AfterMakeUptoDate;
  fBoldEndProperties.OnGetContextType := GetEndContextType;

  fBoldStartHandleFollower := TBoldElementHandleFollower.Create(AOwner, fBoldStartProperties);
  fBoldEndHandleFollower := TBoldElementHandleFollower.Create(AOwner, fBoldEndProperties);

  if IsDesigning and not isLoading then
    ValidateSelf;
end;

procedure TcxBoldDateNavigator.DateNavigatorSelectionChanged;
begin
  inherited;
  if (fInternalChange = 0) {and Assigned(Follower) and Follower.Controller.MayModify(Follower)} then
  begin
    if Assigned(StartFollower) and StartFollower.Controller.MayModify(StartFollower) then
      BoldStartProperties.MayHaveChanged(self.date, StartFollower);
    if Assigned(EndFollower) and EndFollower.Controller.MayModify(EndFollower) then
      BoldEndProperties.MayHaveChanged(self.RealLastDate, EndFollower);
  end;
end;
{
procedure TcxBoldDateNavigator.DoSelectionChangedEvent;
begin
  inherited;
  BoldProperties.MayHaveChanged(DateToStr(self.date), Follower);
end;
}
destructor TcxBoldDateNavigator.Destroy;
begin
  case BoldStartProperties.ApplyPolicy of
    bapChange, bapExit: try
      StartFollower.Apply;
    except
      StartFollower.DiscardChange;
    end;
    bapDemand: StartFollower.DiscardChange;
  end;
  case BoldEndProperties.ApplyPolicy of
    bapChange, bapExit: try
      EndFollower.Apply;
    except
      EndFollower.DiscardChange;
    end;
    bapDemand: EndFollower.DiscardChange;
  end;
  FreeAndNil(fBoldStartHandleFollower);
  FreeAndNil(fBoldEndHandleFollower);
  FreeAndNil(fBoldStartProperties);
  FreeAndNil(fBoldEndProperties);
  inherited;
end;


function TcxBoldDateNavigator.GetBoldEndHandle: TBoldElementHandle;
begin
  Result := fBoldEndHandleFollower.BoldHandle;
end;

function TcxBoldDateNavigator.GetEndContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldEndHandle) then
    result := BoldEndHandle.StaticBoldType
  else
    result := nil;
end;

function TcxBoldDateNavigator.GetEndFollower: TBoldFollower;
begin
  result := fBoldEndHandleFollower.Follower;
end;

function TcxBoldDateNavigator.GetBoldStartHandle: TBoldElementHandle;
begin
  Result := fBoldStartHandleFollower.BoldHandle;
end;

function TcxBoldDateNavigator.GetStartContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldStartHandle) then
    result := BoldStartHandle.StaticBoldType
  else
    result := nil;
end;

function TcxBoldDateNavigator.GetStartFollower: TBoldFollower;
begin
  result := fBoldStartHandleFollower.Follower;
end;

{procedure TcxBoldDateNavigator.SetBoldProperties(
  const Value: TBoldVariantFollowerController);
begin
  fBoldProperties.Assign(Value);
end;}

procedure TcxBoldDateNavigator.SetBoldEndHandle(
  const Value: TBoldElementHandle);
begin
  fBoldEndHandleFollower.BoldHandle := value;
end;

procedure TcxBoldDateNavigator.SetBoldEndProperties(
  const Value: TBoldVariantFollowerController);
begin
  fBoldEndProperties.Assign(Value);
end;

procedure TcxBoldDateNavigator.SetBoldStartHandle(
  const Value: TBoldElementHandle);
begin
  fBoldStartHandleFollower.BoldHandle := value;
end;

procedure TcxBoldDateNavigator.SetBoldStartProperties(
  const Value: TBoldVariantFollowerController);
begin
  fBoldStartProperties.Assign(Value);
end;

type TcxInnerDateNavigatorAccess = class(TcxInnerDateNavigator);

function TcxBoldDateNavigator.ValidateComponent(
  ComponentValidator: TBoldComponentValidator;
  NamePrefix: string): Boolean;
var
  lContext: TBoldElementTypeInfo;
  lExpressionType: TBoldElementTypeInfo;
  lDateTimeTypeInfo: TBoldAttributeTypeInfo;
  lDateTypeInfo: TBoldAttributeTypeInfo;
//  s: string;
begin
//  OutPutDebugString(PChar('TcxBoldDateNavigator.ValidateComponent start'));
  lContext := GetStartContextType;
  result := Assigned(lContext);
  if not result then
  begin
    BoldLog.LogFmt(sNoContext, [Name])
  end
  else
  begin
    with (lContext.SystemTypeInfo as TBoldSystemTypeInfo) do
    begin
      lDateTimeTypeInfo := AttributeTypeInfoByExpressionName['DateTime']; // do not localize
      lDateTypeInfo := AttributeTypeInfoByExpressionName['Date']; // do not localize
    end;
    result := ComponentValidator.ValidateExpressionInContext(
      TBoldFollowerControllerAccess(BoldStartProperties).Expression,
      lContext,
      format('%s %s.Expression', [NamePrefix, Name]),
      BoldStartProperties.VariableList); // do not localize

    if result then
    begin
      lExpressionType := lContext.Evaluator.ExpressionType(TBoldFollowerControllerAccess(BoldStartProperties).Expression, lContext, false, TBoldFollowerControllerAccess(BoldStartProperties).VariableList);
      if not (lExpressionType.ConformsTo(lDateTimeTypeInfo) or lExpressionType.ConformsTo(lDateTypeInfo)) then
      begin
        result := false;
        BoldLog.LogFmt(sPossiblyBadConformance, [lExpressionType.ModelName , lDateTimeTypeInfo.ModelName + ' nor ' + lDateTypeInfo.ModelName]);
      end;
    end;

    lContext := GetEndContextType;
    if Assigned(lContext) then
    begin
      result := ComponentValidator.ValidateExpressionInContext(
        TBoldFollowerControllerAccess(BoldEndProperties).Expression,
        lContext,
        format('%s %s.Expression', [NamePrefix, Name]),
        BoldEndProperties.VariableList) and result; // do not localize

      if result then
      begin
        lExpressionType := lContext.Evaluator.ExpressionType(TBoldFollowerControllerAccess(BoldEndProperties).Expression, lContext, false, TBoldFollowerControllerAccess(BoldEndProperties).VariableList);
        if not (lExpressionType.ConformsTo(lDateTimeTypeInfo) or lExpressionType.ConformsTo(lDateTypeInfo)) then
        begin
          result := false;
          BoldLog.LogFmt(sPossiblyBadConformance, [lExpressionType.ModelName , lDateTimeTypeInfo.ModelName + ' nor ' + lDateTypeInfo.ModelName]);
        end;
      end;
    end;
  end;
//  fValueOrDefinitionInvalid := not result;
//  s := 'TcxBoldDateNavigator.ValidateComponent:' + BoolToStr(result, true);
//  OutPutDebugString(PChar(S));
end;

procedure TcxBoldDateNavigator.Loaded;
begin
  inherited;
  if IsDesigning then
    ValidateSelf;
end;

procedure TcxBoldDateNavigator.ValidateSelf;
var
  lBoldComponentValidator: TBoldComponentValidator;
begin
  if IsDesigning then
  begin
    lBoldComponentValidator := TBoldComponentValidator.Create;
    try
      ValidateComponent(lBoldComponentValidator, Name);
    finally
      lBoldComponentValidator.free;
    end;
  end;
end;

{$ENDIF}

{ TcxBoldLabel }

function TcxBoldLabel.GetActiveProperties: TcxLabelProperties;
begin
  Result := TcxLabelProperties(InternalGetActiveProperties);
end;

function TcxBoldLabel.GetDataBinding: TcxBoldTextEditDataBinding;
begin
  Result := FDataBinding as TcxBoldTextEditDataBinding;
end;

class function TcxBoldLabel.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxBoldTextEditDataBinding;
end;

function TcxBoldLabel.GetProperties: TcxLabelProperties;
begin
  Result := TcxLabelProperties(FProperties);
end;

class function TcxBoldLabel.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxLabelProperties;
end;

procedure TcxBoldLabel.Initialize;
begin
  inherited Initialize;
//  AutoSize := False;
  if IsDesigning and not IsLoading then
  begin
    _ValidateEdit(self);
  end;
end;

procedure TcxBoldLabel.Paint;
begin
  inherited;
  if DataBinding.ValueOrDefinitionInvalid then
//    ViewInfo.TextColor := clRed;
    Canvas.FrameRect(Bounds, clRed, 3);
end;

procedure TcxBoldLabel.SetDataBinding(Value: TcxBoldTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldLabel.SetEditAutoSize(Value: Boolean);
begin
  inherited;

end;

procedure TcxBoldLabel.SetProperties(Value: TcxLabelProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxBoldImage }

function TcxBoldImage.GetActiveProperties: TcxImageProperties;
begin
  Result := TcxImageProperties(InternalGetActiveProperties);
end;

function TcxBoldImage.GetDataBinding: TcxBoldBlobEditDataBinding;
begin
  Result := FDataBinding as TcxBoldBlobEditDataBinding;
end;

class function TcxBoldImage.GetDataBindingClass: TcxEditDataBindingClass;
begin
  result := TcxBoldBlobEditDataBinding;
end;

function TcxBoldImage.GetProperties: TcxImageProperties;
begin
  Result := TcxImageProperties(FProperties);
end;

class function TcxBoldImage.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxImageProperties;
end;

procedure TcxBoldImage.Initialize;
begin
  inherited;
  if IsDesigning and not IsLoading then
  begin
    _ValidateEdit(self);
  end;
end;

procedure TcxBoldImage.Paint;
begin
  inherited Paint;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

procedure TcxBoldImage.SetDataBinding(Value: TcxBoldBlobEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldImage.SetProperties(Value: TcxImageProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxBoldRichEdit }

procedure TcxBoldRichEdit.DoValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean);
begin
  inherited;
  if Assigned(DataBinding) then
    DataBinding.ValidateDisplayValue(ADisplayValue, AErrorText, AError);
end;

procedure TcxBoldRichEdit.EditingChanged;
begin
  inherited;

end;

function TcxBoldRichEdit.GetActiveProperties: TcxRichEditProperties;
begin
  Result := TcxRichEditProperties(InternalGetActiveProperties);
end;

function TcxBoldRichEdit.GetDataBinding: TcxBoldTextEditDataBinding;
begin
  Result := FDataBinding as TcxBoldTextEditDataBinding;
end;

class function TcxBoldRichEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxBoldTextEditDataBinding;
end;

function TcxBoldRichEdit.GetProperties: TcxRichEditProperties;
begin
  Result := TcxRichEditProperties(FProperties);
end;

class function TcxBoldRichEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxRichEditProperties;
end;

procedure TcxBoldRichEdit.Paint;
begin
  inherited Paint;
  if DataBinding.ValueOrDefinitionInvalid then
    Canvas.FrameRect(Bounds, clRed, 2 - Ord(IsNativeStyle));
end;

function TcxBoldRichEdit.RealReadOnly: Boolean;
begin
  Result := inherited RealReadOnly or not DataBinding.Editing;
end;

procedure TcxBoldRichEdit.SetDataBinding(
  Value: TcxBoldTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxBoldRichEdit.SetProperties(Value: TcxRichEditProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxBoldComboBoxEditDataBinding }

constructor TcxBoldComboBoxEditDataBinding.Create(AEdit: TcxCustomEdit);
begin
  inherited;
  Assert(TcxCustomEditAccess(AEdit).Properties is TcxBoldComboBoxProperties);
  fBoldHandleFollower.PrioritizedQueuable := TcxBoldComboBoxProperties(TcxCustomEditAccess(AEdit).Properties).fListHandleFollower;
  fBoldHandleFollower.StronglyDependedOfPrioritized := true;
end;

function TcxBoldComboBoxEditDataBinding.GetModified: Boolean;
var
//  lcxCustomTextEditAccess: TcxCustomTextEditAccess;
  lItemIndex: integer;
  lCount: integer;
  lOriginalElement, lNewElement: TBoldElement;
  lcxBoldComboBox: TcxBoldComboBox;
begin
  if not IsDataAvailable or FEdit.ActiveProperties.ReadOnly or (TcxCustomTextEditAccess(Edit).ILookupData.ActiveControl = nil) then
  begin
    result := false;
    exit;
  end;
  result := not cxEditVarEquals(Edit.EditValue, StoredValue); //not ((VarType(Edit.EditValue) = VarType(StoredValue)) and VarSameValue(StoredValue, Edit.EditValue));
  if result and ((VarIsNull(Edit.EditValue) and VarIsStr(StoredValue) and (StoredValue = ''))
    or (VarIsNull(StoredValue) and VarIsStr(Edit.EditValue) and (Edit.EditValue = ''))) then
    result := false;
//  if not result then
  begin
    lcxBoldComboBox := Edit as TcxBoldComboBox;
    lOriginalElement := lcxBoldComboBox.DataBinding.Follower.Value;
    if lOriginalElement is TBoldObjectReference then
      lOriginalElement := TBoldObjectReference(lOriginalElement).BoldObject;
//    lItemIndex := TcxCustomTextEditAccess(Edit).ItemIndex;
    lCount := lcxBoldComboBox.ActiveProperties.LookupListFollower.SubFollowerCount-1;
    lItemIndex := TcxCustomEditListBox(TcxCustomTextEditAccess(Edit).ILookupData.ActiveControl).itemIndex;

    if ((lItemIndex = lCount) and (lcxBoldComboBox.ActiveProperties.BoldLookupListProperties.NilElementMode = neAddLast))
    or ((lItemIndex = 0) and (lcxBoldComboBox.ActiveProperties.BoldLookupListProperties.NilElementMode = neInsertFirst))
    then
    begin
      Result := lOriginalElement <> nil;
      exit;
    end;
    if lItemIndex <> -1 then
    begin
      //  lcxBoldComboBox.ActiveProperties.BoldLookupListProperties.ListIndexToIndex(lItemIndex);
      if (lcxBoldComboBox.ActiveProperties.BoldLookupListProperties.NilElementMode = neInsertFirst) then
        Dec(lItemIndex);
      lNewElement := lcxBoldComboBox.ActiveProperties.BoldLookupListHandle.List[lItemIndex];
      Result := lOriginalElement <> lNewElement;
    end
    else
    begin
      if not ((lcxBoldComboBox.Properties.BoldSelectChangeAction in [bdcsSetValue,bdscSetText]) and (lOriginalElement is TBoldAttribute)) then
      result := false;
    end;
  end;
end;

function TcxBoldComboBoxEditDataBinding.ImmediatePost: boolean;
begin
  result := true;
end;

{ TcxBoldIntegerEditDataBinding }

function TcxBoldNumericEditDataBinding.ValidateTypeConforms(
  aExpressionType: TBoldElementTypeInfo): string;
var
  lNumericTypeInfo: TBoldAttributeTypeInfo;
begin
  result := '';
  with (aExpressionType.SystemTypeInfo as TBoldSystemTypeInfo) do
  begin
    lNumericTypeInfo := AttributeTypeInfoByExpressionName['Numeric']; // do not localize
  end;
  if not aExpressionType.ConformsTo(lNumericTypeInfo) then
    result := Format(sPossiblyBadConformance, [aExpressionType.ModelName , lNumericTypeInfo.ModelName]);
end;

{ TcxBoldBlobEditDataBinding }

function TcxBoldBlobEditDataBinding.ValidateTypeConforms(
  aExpressionType: TBoldElementTypeInfo): string;
var
  lBlobTypeInfo: TBoldAttributeTypeInfo;
begin
  result := '';
  with (aExpressionType.SystemTypeInfo as TBoldSystemTypeInfo) do
  begin
    lBlobTypeInfo := AttributeTypeInfoByExpressionName['Blob']; // do not localize
  end;
  if not aExpressionType.ConformsTo(lBlobTypeInfo) then
    result := Format(sPossiblyBadConformance, [aExpressionType.ModelName , lBlobTypeInfo.ModelName]);
end;

{ TcxBoldTimeEditDataBinding }

function TcxBoldTimeEditDataBinding.ValidateTypeConforms(
  aExpressionType: TBoldElementTypeInfo): string;
var
  lDateTimeTypeInfo: TBoldAttributeTypeInfo;
  lTimeTypeInfo: TBoldAttributeTypeInfo;
begin
  result := '';
  with (aExpressionType.SystemTypeInfo as TBoldSystemTypeInfo) do
  begin
    lDateTimeTypeInfo := AttributeTypeInfoByExpressionName['DateTime']; // do not localize
    lTimeTypeInfo := AttributeTypeInfoByExpressionName['Time']; // do not localize
  end;
  if not (aExpressionType.ConformsTo(lDateTimeTypeInfo) or aExpressionType.ConformsTo(lTimeTypeInfo)) then
    result := Format(sPossiblyBadConformance, [aExpressionType.ModelName , lDateTimeTypeInfo.ModelName + ' nor ' + lTimeTypeInfo.ModelName]);
end;

function TcxCustomBoldTextEditProperties.BoldElementToEditValue(
  aFollower: TBoldFollower; aElement: TBoldElement;
  aEdit: TcxCustomEdit): variant;
begin
  result := BoldRowProperties.GetAsVariant(aFollower);
end;

procedure TcxCustomBoldTextEditProperties.SetBoldSelectChangeAction(
  Value: TBoldComboSelectChangeAction);
begin
  fBoldSelectChangeAction := Value;
end;

procedure TcxCustomBoldTextEditProperties.SetStoredValue(aValue: Variant;
  aBoldHandle: TBoldElementHandle; aEdit: TcxCustomEdit;
  aFollower: TBoldFollower; var aDone: boolean);
var
  LocalSelectedElement: TBoldElement;
  lItemIndex: Integer;
begin
  lItemIndex := -1;
  if Assigned(TcxTextEdit(aEdit).ILookupData) then
    lItemIndex := TcxTextEdit(aEdit).ILookupData.CurrentKey;
  if lItemIndex = -1 then
  begin
    aDone := false;
    exit;
  end
  else
  begin
{    if ((lItemIndex = LookupListFollower.SubFollowerCount-1) and (BoldLookupListProperties.NilElementMode = neAddLast))
    or ((lItemIndex = 0) and (BoldLookupListProperties.NilElementMode = neInsertFirst)) then
    begin
      LocalSelectedElement := nil
    end
    else
    begin
      if (BoldLookupListProperties.NilElementMode = neInsertFirst) then
        dec(lItemIndex);
}
      LocalSelectedElement := BoldLookupListHandle.List[lItemIndex];
//    end;
  end;
  InternalComboSetValue(aBoldHandle, aFollower, LocalSelectedElement, BoldSelectChangeAction, BoldSetValueExpression, BoldLookupListHandle, aValue);
  aDone := true;
end;

function TcxCustomBoldTextEditProperties.CanEdit(
  aBoldHandle: TBoldElementHandle; aFollower: TBoldFollower): boolean;
begin
  result := true;
end;

procedure TcxCustomBoldTextEditProperties.SetBoldSetValueExpression(
  const Value: TBoldExpression);
begin
  fBoldSetValueExpression := Value;
  if Owner is TcxCustomEdit and (TcxCustomEdit(Owner).IsDesigning) and not (TcxCustomEdit(Owner).IsLoading) then
  begin
    _ValidateEdit(TcxCustomEdit(Owner));
  end;
end;

{ TcxBoldCurrencyEditDataBinding }

function TcxBoldCurrencyEditDataBinding.ValidateTypeConforms(
  aExpressionType: TBoldElementTypeInfo): string;
var
  lCurrencyTypeInfo: TBoldAttributeTypeInfo;
  lFloatTypeInfo: TBoldAttributeTypeInfo;
begin
  result := '';
  with (aExpressionType.SystemTypeInfo as TBoldSystemTypeInfo) do
  begin
    lCurrencyTypeInfo := AttributeTypeInfoByExpressionName['Currency']; // do not localize
    lFloatTypeInfo := AttributeTypeInfoByExpressionName['Float']; // do not localize
  end;
  if not aExpressionType.ConformsTo(lCurrencyTypeInfo) and not aExpressionType.ConformsTo(lFloatTypeInfo) then
    result := Format(sPossiblyBadConformance, [aExpressionType.ModelName , lCurrencyTypeInfo.ModelName]);
end;

{ TcxBoldListBox }

procedure TcxBoldListBox._DeleteItem(Index: Integer;
  OwningFollower: TBoldFollower);
begin
  if not (ListStyle in [lbVirtual, lbVirtualOwnerDraw]) then
  begin
    if not Items.Updating then
      Items.BeginUpdate;
    Items.Delete(index);
  end;
end;

procedure TcxBoldListBox._InsertItem(Index: Integer; Follower: TBoldFollower);
begin
  Assert(Assigned(Follower));
  if not Items.Updating then
    Items.BeginUpdate;
  Follower.EnsureDisplayable;
  if not (ListStyle in [lbVirtual, lbVirtualOwnerDraw]) then
    Items.Insert(Follower.Index, VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower)))
end;

procedure TcxBoldListBox._ReplaceItem(Index: Integer; Follower: TBoldFollower);
begin
  if not Items.Updating then
    Items.BeginUpdate;
  Follower.EnsureDisplayable;
  if not (ListStyle in [lbVirtual, lbVirtualOwnerDraw]) then
    Items.Strings[Index] := VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower));
end;

procedure TcxBoldListBox._RowAfterMakeUptoDate(Follower: TBoldFollower);
var
  index: Integer;
  s: string;
begin
  index := Follower.index;
  if (index > -1) and (index < Items.Count) then
  begin
    s := VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower));
    if s <> Items[index] then
      Items[index] := s;
  end;
end;

procedure TcxBoldListBox._AfterMakeUptoDate(Follower: TBoldFollower);
begin
  fBoldRowProperties.AfterMakeUptoDate := _RowAfterMakeUptoDate;
  if not fInternalUpdate then
  begin
    if ListStyle in [lbVirtual, lbVirtualOwnerDraw] then
      Count := self.Follower.SubFollowerCount;
    ItemIndex := Follower.CurrentIndex;
    if MultiSelect then
      ClearSelection;
    if ItemIndex <> -1 then
      Selected[ItemIndex] := true;
  end;
  SyncSelection;
  if Items.Updating then
    Items.EndUpdate;
end;

procedure TcxBoldListBox._BeforeMakeUptoDate(Follower: TBoldFollower);
begin
  fBoldRowProperties.AfterMakeUptoDate := nil;
  if assigned(BoldListHandle) and assigned(BoldListHandle.list) then
    BoldListHandle.list.EnsureRange(0, BoldListHandle.list.Count-1);
end;

function TcxBoldListBox.GetBoldListHandle: TBoldAbstractListHandle;
begin
  Result := fListHandleFollower.BoldHandle;
end;

procedure TcxBoldListBox.SetBoldListHandle(
  const Value: TBoldAbstractListHandle);
begin
  fListHandleFollower.BoldHandle := value;
end;

procedure TcxBoldListBox.SetBoldListProperties(
  const Value: TBoldListAsFollowerListController);
begin
  fBoldListProperties.Assign(Value);
end;

procedure TcxBoldListBox.SetRowProperties(
  const Value: TBoldVariantFollowerController);
begin
  fBoldRowProperties.Assign(Value);
end;

function TcxBoldListBox.GetListFollower: TBoldFollower;
begin
  Result := fListHandleFollower.Follower;
end;

constructor TcxBoldListBox.Create(AOwner: TComponent);
begin
  inherited;
  fBoldRowProperties := TBoldVariantFollowerController.Create(AOwner);
  fBoldRowProperties.AfterMakeUptoDate := _RowAfterMakeUptoDate;
  fBoldRowProperties.OnGetContextType := GetContextForBoldRowProperties;
  fBoldListProperties := TBoldListAsFollowerListController.Create(AOwner, fBoldRowProperties);
  with fBoldListProperties do
  begin
    OnAfterInsertItem := _InsertItem;
    OnAfterDeleteItem := _DeleteItem;
    OnReplaceitem := _ReplaceItem;    
    BeforeMakeUptoDate := _BeforeMakeUptoDate;
    AfterMakeUptoDate := _AfterMakeUptoDate;
  end;
  fListHandleFollower := TBoldListHandleFollower.Create(Owner, fBoldListProperties);
end;

destructor TcxBoldListBox.Destroy;
begin
  FreeAndNil(fListHandleFollower);
  FreeAndNil(fBoldListProperties);
  FreeAndNil(fBoldRowProperties);
  inherited;
end;

function TcxBoldListBox.GetContextForBoldRowProperties: TBoldElementTypeInfo;
begin
  if assigned(BoldListHandle) then
    result := BoldListHandle.StaticBoldType
  else
    result := nil;
end;

function TcxBoldListBox.GetBoldHandleIndexLock: Boolean;
begin
  Result := fListHandleFollower.HandleIndexLock;
end;

procedure TcxBoldListBox.SetBoldHandleIndexLock(const Value: Boolean);
begin
  fListHandleFollower.HandleIndexLock := Value;
end;

procedure TcxBoldListBox.WndProc(var Message: TMessage);
begin
  inherited;
  if (InnerListBox <> nil) and not IsDestroying and
   ((Message.Msg = WM_CTLCOLORLISTBOX) or (Message.Msg = WM_COMMAND) and (Message.WParamHi = LBN_SELCHANGE)) and (Follower.CurrentIndex <> ItemIndex) then
  begin
    fListHandleFollower.SetFollowerIndex(ItemIndex);
    fInternalUpdate := true;
    try
      TBoldQueueable.DisplayAll;
    finally
      fInternalUpdate := false;
    end;
  end;
end;

procedure TcxBoldListBox.DblClick;
var
  lAutoForm: TForm;
  lElement: TBoldElement;
begin
  inherited;
  if fBoldListProperties.DefaultDblClick and Assigned(Follower.CurrentSubFollower) and Assigned(Follower.CurrentSubFollower.Element) then
  begin
    lElement := Follower.CurrentSubFollower.Element;
    lAutoForm := AutoFormProviderRegistry.FormForElement(lElement);
    if assigned(lAutoForm) then
    begin
      lAutoForm.Show;
    end
  end;
end;

function TcxBoldListBox.GetFollower: TBoldFollower;
begin
  if Assigned(fListHandleFollower) then
    result := fListHandleFollower.Follower
  else
    result := nil;  
end;

procedure TcxBoldListBox.Loaded;
begin
  inherited;
  Items.Clear;
  DragMode := dmAutomatic;
end;

procedure TcxBoldListBox.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  BoldListProperties.EndDrag;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TcxBoldListBox.DoStartDrag(var DragObject: TDragObject);
begin
  SyncSelection;
  BoldListProperties.StartDrag(Follower);
  inherited DoStartDrag(DragObject);
end;

procedure TcxBoldListBox.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Assigned(OnDragOver)
    or (BoldListProperties.DropMode = bdpNone)
    or ((Source = Self) and (not BoldListProperties.InternalDrag)) then
    inherited DragOver(Source, X, Y, State, Accept)
  else
    Accept := BoldListProperties.DragOver(Follower, MutableList, ItemAtPos(Point(X, Y), False));
end;

procedure TcxBoldListBox.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
  begin
    if BoldGuiHandler.ActivateTargetFormOnDrop then
      BoldGUIHandler.TryToFocusHostingForm(self);
    inherited DragDrop(Source, X, Y);
  end
  else
    BoldListProperties.DragDrop(Follower, MutableList, ItemAtPos(Point(X, Y), False));
end;

function TcxBoldListBox.DrawItem(ACanvas: TcxCanvas; AIndex: Integer;
  const ARect: TRect; AState: TOwnerDrawState): Boolean;
begin
  DefaultSetFontAndColor(AIndex);
  Result := inherited DrawItem(ACanvas, AIndex, ARect, AState);
end;

procedure TcxBoldListBox.DefaultDrawItem(Index: integer; aRect: TRect);
begin
  BoldRowProperties.DrawOnCanvas(Follower.SubFollowers[index], Canvas.Canvas, aRect, taLeftJustify, Point(2,0));
end;

procedure TcxBoldListBox.DefaultSetFontAndColor(Index: integer);
var
  ec: tColor;
  SubFollower: TBoldFollower;
begin
  BoldRowProperties.SetFont(InnerListBox.Canvas.Font, InnerListBox.Canvas.Font, Follower.SubFollowers[index]);
  BoldRowProperties.SetColor(ec, InnerListBox.Canvas.Brush.Color, Follower.SubFollowers[index]);
  InnerListBox.Canvas.Brush.Color := ec;
  SubFollower := Follower.SubFollowers[index];
  if assigned(Subfollower) and Subfollower.Selected then
    with InnerListBox.Canvas do
    begin
      Brush.Color := clHighlight;
      Font.Color := clHighlightText;
    end;
end;

function TcxBoldListBox.GetMutableList: TBoldList;
begin
  if assigned(BoldListHandle) then
    result := BoldListHandle.MutableList
  else
    result := nil;
end;

procedure TcxBoldListBox.SyncSelection;
var
  i: integer;
begin
  BoldListProperties.SelectAll(Follower, False);
  if multiselect then
  begin
    if SelCount > 0 then
    begin
      for i := 0 to Count - 1 do
      begin
        BoldListProperties.SetSelected(Follower, i, Selected[i]);
      end;
    end;
  end
  else
  begin
    if ItemIndex <> -1 then
      BoldListProperties.SetSelected(Follower, ItemIndex, true);
  end;
  if Assigned(BoldListHandle) and (BoldListHandle.CurrentIndex <> ItemIndex) then
  begin
    BoldListHandle.CurrentIndex := ItemIndex;
    TBoldQueueable.DisplayAll;
  end;
end;

function TcxBoldListBox.ValidateComponent(
  ComponentValidator: TBoldComponentValidator;
  NamePrefix: string): Boolean;
var
  lContext: TBoldElementTypeInfo;
begin
  result := false;
  lContext := GetContextForBoldRowProperties;
  if assigned(lContext) then
  begin
    result := ComponentValidator.ValidateExpressionInContext(
      BoldRowProperties.Expression,
      lContext,
      format('%s %s.BoldRowProperties.Expression', [NamePrefix, Name]), BoldRowProperties.VariableList); // do not localize
  end;
end;

{ TcxBoldListView }

procedure TcxBoldListView._BeforeMakeUptoDate(Follower: TBoldFollower);
begin
  // Will fetch all
  if assigned(BoldHandle) and assigned(Boldhandle.list) then
    BoldHandle.list.EnsureRange(0, BoldHandle.list.Count-1);
end;

procedure TcxBoldListView._AfterMakeUptoDate(Follower: TBoldFollower);
begin
  ItemIndex := Follower.CurrentIndex;
  if ItemIndex = -1 then
    fBoldProperties.SelectAll(Follower, False)
  else
    ;
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    Items.EndUpdate;
  end;
end;

procedure TcxBoldListView._InsertItem(Index: Integer; Follower: TBoldFollower);
var
  lItem: TListItem;
begin
  Assert(Assigned(Follower));
  if FUpdateCount = 0 then
  begin
    inc(FUpdateCount);
    Items.BeginUpdate;
  end;
  Follower.EnsureDisplayable;
  lItem := Items.Insert(Follower.Index);
  lItem.Caption := VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower));
  lItem.ImageIndex := 0;
end;

procedure TcxBoldListView._DeleteItem(index: Integer;
  OwningFollower: TBoldFollower);
begin
  if FUpdateCount = 0 then
  begin
    inc(FUpdateCount);
    Items.BeginUpdate;
  end;
  Items.Delete(index);
end;

procedure TcxBoldListView._ReplaceItem(Index: Integer; Follower: TBoldFollower);
begin
  if FUpdateCount = 0 then
  begin
    inc(FUpdateCount);
    Items.BeginUpdate;
  end;
  Follower.EnsureDisplayable;
  items[Index].Caption := VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower));
end;

procedure TcxBoldListView._RowAfterMakeUptoDate(Follower: TBoldFollower);
var
  NewValue: string;
begin
  NewValue := VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower));
  if Items[Follower.index].Caption <> NewValue then
    Items[Follower.index].Caption := NewValue;
end;

function TcxBoldListView.GetBoldHandle: TBoldAbstractListHandle;
begin
  Result := fListHandleFollower.BoldHandle;
end;

function TcxBoldListView.GetBoldHandleIndexLock: Boolean;
begin
  Result := fListHandleFollower.HandleIndexLock;
end;

function TcxBoldListView.GetBoldList: TBoldList;
begin
  //CHECKME We may have to remove this because the list is not necessarily equal with the rendered list!!! /FH
  if Assigned(BoldHandle) then
    Result := BoldHandle.List
  else
    Result := nil;
end;

function TcxBoldListView.GetContextType: TBoldElementTypeInfo;
begin
  if assigned(BoldHandle) then
    result := BoldHandle.StaticBoldType
  else
    result := nil;
end;

function TcxBoldListView.GetCurrentBoldElement: TBoldElement;
var
  Subfollower: TBoldFollower;
begin
  Result := nil;
  if ItemIndex <> -1 then
  begin
    SubFollower := Follower.SubFollowers[ItemIndex];
    if assigned(SubFollower) then
      Result := Subfollower.Element;
  end;
end;

function TcxBoldListView.GetCurrentBoldObject: TBoldObject;
begin
  if CurrentBoldElement is TBoldObject then
    Result := CurrentBoldElement as TBoldObject
  else
    Result := nil;
end;

function TcxBoldListView.GetFollower: TBoldFollower;
begin
  Result := fListHandleFollower.Follower;
end;

procedure TcxBoldListView.SetBoldHandle(value: TBoldAbstractListHandle);
begin
  fListHandleFollower.BoldHandle := value;
end;

procedure TcxBoldListView.SetBoldHandleIndexLock(Value: Boolean);
begin
  fListHandleFollower.HandleIndexLock := Value;
end;

procedure TcxBoldListView.SetBoldProperties(
  Value: TBoldAbstractListAsFollowerListController);
begin
  fBoldProperties.Assign(Value);
end;

procedure TcxBoldListView.SetRowProperties(
 const Value: TBoldVariantFollowerController);
begin
  fBoldRowProperties.Assign(Value);
end;

constructor TcxBoldListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBoldRowProperties := TBoldVariantFollowerController.Create(Self);
  fBoldRowProperties.AfterMakeUptoDate := _RowAfterMakeUptoDate;
  fBoldRowProperties.OnGetContextType := GetContextType;
  fBoldProperties := TBoldAbstractListAsFollowerListController.Create(Self, fBoldRowProperties);
  with fBoldProperties do
  begin
    OnAfterInsertItem := _InsertItem;
    OnAfterDeleteItem := _DeleteItem;
    OnReplaceitem := _ReplaceItem;
    BeforeMakeUptoDate := _BeforeMakeUptoDate;
    AfterMakeUptoDate := _AfterMakeUptoDate;
  end;
  fListHandleFollower := TBoldListHandleFollower.Create(Owner, fBoldProperties);
  DragMode := dmAutomatic;
  ViewStyle := vsIcon;
end;

destructor TcxBoldListView.Destroy;
begin
  FreeAndNil(fListHandleFollower);
  FreeAndNil(fBoldProperties);
  FreeAndNil(fBoldRowProperties);
  inherited Destroy;
end;
(*
{ TcxBoldListBox }

function TcxBoldListBox.GetDataBinding: TcxBoldDataBinding;
begin
  Result := TcxBoldDataBinding(FDataBinding);
end;

function TcxBoldListBox.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  result := TcxBoldDataBinding;
end;

procedure TcxBoldListBox.SetDataBinding(Value: TcxBoldDataBinding);
begin
  FDataBinding.Assign(Value);
end;
*)

{ TcxBoldCustomCheckListBox }

constructor TcxBoldCustomCheckListBox.Create(AOwner: TComponent);
begin
  inherited;
  fBoldRowProperties := TBoldVariantFollowerController.Create(self);
  fBoldRowProperties.OnGetContextType := GetContextType;
  fBoldRowCheckBoxProperties := TBoldCheckBoxStateFollowerController.Create(self);
  fBoldRowCheckBoxProperties.OnGetContextType := GetContextType;

  fControllerList := TBoldControllerList.Create(self);
  fControllerList.Add(fBoldRowCheckBoxProperties);
  fControllerLIst.Add(fBoldRowProperties);

  fBoldRowProperties.AfterMakeUptoDate := _DisplayString;
  fBoldRowCheckBoxProperties.AfterMakeUpToDate := _DisplayCheckBox;

  fBoldListProperties := TBoldAbstractListAsFollowerListController.Create(self, fControllerList);
  fBoldListProperties.OnGetContextType := GetContextType;
  fBoldListProperties.AfterMakeUptoDate := _ListAfterMakeUpToDate;
  fBoldListProperties.BeforeMakeUptoDate := _ListBeforeMakeUpToDate;
  fBoldlistProperties.OnAfterInsertItem := _ListInsertItem;
  fBoldListProperties.OnAfterDeleteItem := _ListDeleteItem;
  fBoldListProperties.OnReplaceitem := _ReplaceItem;
//  fBoldListProperties.DefaultDblClick := false;
  fListHandleFollower := TBoldListHandleFollower.Create(AOwner, fBoldListProperties);
end;

destructor TcxBoldCustomCheckListBox.Destroy;
begin
  FreeAndNil(fListHandleFollower);
  FreeAndNil(fBoldListProperties);
  FreeAndNil(fControllerList);
  inherited;
end;

function TcxBoldCustomCheckListBox.GetBoldHandleIndexLock: Boolean;
begin
  Result := fListHandleFollower.HandleIndexLock;
end;

function TcxBoldCustomCheckListBox.GetBoldListHandle: TBoldAbstractListHandle;
begin
  Result := fListHandleFollower.BoldHandle;
end;

function TcxBoldCustomCheckListBox.GetContextType: TBoldElementTypeInfo;
begin
  if Assigned(BoldListHandle) then
    result := BoldListHandle.StaticBoldType
  else
    result := nil;
end;

function TcxBoldCustomCheckListBox.GetFollower: TBoldFollower;
begin
  result := fListHandleFollower.Follower;
end;

type
  TcxBoldCustomInnerCheckListBox = class(TcxCustomInnerCheckListBox)
  protected
    procedure DoClickCheck(const AIndex: Integer; const OldState, NewState: TcxCheckBoxState); override;
  end;

{ TcxBoldCustomInnerCheckListBox }

const
  CHECKBOXFOLLOWER_INDEX = 0;
  STRINGFOLLOWER_INDEX = 1;

procedure TcxBoldCustomInnerCheckListBox.DoClickCheck(
  const AIndex: Integer; const OldState, NewState: TcxCheckBoxState);
var
  CheckBoxFollower: TBoldFollower;
  lOwningCheckListBox: TcxBoldCustomCheckListBox;
begin
  lOwningCheckListBox := (fContainer as TcxBoldCustomCheckListBox);
  if not (csDesigning in ComponentState) and (ItemIndex <> - 1) then
  begin
    CheckBoxFollower := lOwningCheckListBox.fListHandleFollower.Follower.SubFollowers[AIndex].SubFollowers[CHECKBOXFOLLOWER_INDEX];
    TBoldCheckBoxStateFollowerController(CheckBoxFollower.Controller).SetAsCheckBoxState(TCheckBoxState(NewState), CheckBoxFollower);
    lOwningCheckListBox.fListHandleFollower.SetFollowerIndex(AIndex);
  end;
  inherited;
end;

function TcxBoldCustomCheckListBox.GetInnerCheckListBoxClass: TcxCustomInnerCheckListBoxClass;
begin
  result := TcxBoldCustomInnerCheckListBox;
end;

procedure TcxBoldCustomCheckListBox.Loaded;
begin
  inherited;
  Items.Clear;
  DragMode := dmAutomatic;
end;

procedure TcxBoldCustomCheckListBox.SetBoldHandleIndexLock(
  const Value: Boolean);
begin
  fListHandleFollower.HandleIndexLock := Value;
end;

procedure TcxBoldCustomCheckListBox.SetBoldListHandle(
  const Value: TBoldAbstractListHandle);
begin
  fListHandleFollower.BoldHandle := value;
end;

procedure TcxBoldCustomCheckListBox.SetBoldListProperties(
  const Value: TBoldAbstractListAsFollowerListController);
begin
  FBoldListProperties.Assign(Value);
end;

procedure TcxBoldCustomCheckListBox.SetBoldRowCheckBoxProperties(
  const Value: TBoldCheckBoxStateFollowerController);
begin
  fBoldRowCheckBoxProperties.Assign(Value);
end;

procedure TcxBoldCustomCheckListBox.SetRowProperties(
  const Value: TBoldVariantFollowerController);
begin
  fBoldRowProperties.Assign(Value);
end;

procedure TcxBoldCustomCheckListBox.SyncSelection;
var
  i: integer;
begin
  BoldListProperties.SelectAll(Follower, False);
  if InnerCheckListBox.multiselect then
  begin
    if InnerCheckListBox.SelCount > 0 then
    begin
      for i := 0 to Count - 1 do
      begin
        BoldListProperties.SetSelected(Follower, i, Selected[i]);
      end;
    end
  end
  else
  begin
    if ItemIndex <> -1 then
      BoldListProperties.SetSelected(Follower, ItemIndex, true)
  end;
end;

procedure TcxBoldCustomCheckListBox.WndProc(var Message: TMessage);
begin
  inherited;
  if (InnerCheckListBox <> nil) and (Message.Msg = WM_COMMAND) and (Message.WParamHi = LBN_SELCHANGE) then
  begin
    fListHandleFollower.SetFollowerIndex(ItemIndex);
    fInternalUpdate := true;
    try
      TBoldQueueable.DisplayAll;
    finally
      fInternalUpdate := false;
    end;
  end;
end;

procedure TcxBoldCustomCheckListBox._DisplayCheckBox(
  Follower: TBoldFollower);
var
  index: integer;
begin
  index := Follower.OwningFollower.index;
  if (index > -1) and (index < Items.Count) then
  begin
    Items[Index].State := TcxCheckBoxState(TBoldCheckBoxStateFollowerController(Follower.Controller).GetCurrentAsCheckBoxState(Follower));
  end;
end;

procedure TcxBoldCustomCheckListBox._DisplayString(
  Follower: TBoldFollower);
var
  index: integer;
begin
  index := Follower.OwningFollower.index;
  if (index > -1) and (index < Items.Count) then
  begin
    Items[Index].Text := VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower));
  end;
end;

procedure TcxBoldCustomCheckListBox._ListAfterMakeUpToDate(
  Follower: TBoldFollower);
var
  lIndex: integer;
begin
//  fBoldRowProperties.AfterMakeUptoDate := _RowAfterMakeUptoDate;
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    Items.EndUpdate;
  end;
  if not fInternalUpdate then
  begin
    lIndex := Follower.CurrentIndex;
    ItemIndex := lIndex;
    if self.InnerCheckListBox.MultiSelect then
      InnerCheckListBox.ClearSelection;
    if lIndex <> -1 then
      Selected[lIndex] := true;
  end;
  SyncSelection;
end;

procedure TcxBoldCustomCheckListBox._ListBeforeMakeUpToDate(
  Follower: TBoldFollower);
begin
  // will fetch all
  if Assigned(BoldListHandle) and Assigned(BoldListHandle.List) then
    BoldListHandle.List.EnsureRange(0, BoldListHandle.List.Count - 1);
end;

procedure TcxBoldCustomCheckListBox._ListDeleteItem(Index: integer;
  Follower: TBoldFollower);
begin
  if FUpdateCount = 0 then
  begin
    inc(FUpdateCount);
    Items.BeginUpdate;
  end;
  Items.Delete(Index);
end;

procedure TcxBoldCustomCheckListBox._ListInsertItem(Index: Integer; Follower: TBoldFollower);
var
  lCheckListBoxItem: TcxCheckListBoxItem;
begin
  if FUpdateCount = 0 then
  begin
    inc(FUpdateCount);
    Items.BeginUpdate;
  end;
  Assert(Assigned(Follower));
  lCheckListBoxItem := Items.Insert(Follower.Index) as TcxCheckListBoxItem;
  if Assigned(Follower) then
  begin
    Follower.EnsureDisplayable;
    if Assigned(Follower.Value) then
      lCheckListBoxItem.Text := VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower));
  end;
end;

procedure TcxBoldCustomCheckListBox._ReplaceItem(Index: Integer;
  Follower: TBoldFollower);
begin
  if FUpdateCount = 0 then
  begin
    inc(FUpdateCount);
    Items.BeginUpdate;
  end;
  Follower.EnsureDisplayable;
  Items[Index].Text := VarToStr(TBoldVariantFollowerController(Follower.Controller).GetAsVariant(Follower));
end;

{ TcxBoldSelectionCheckListBox }

constructor TcxBoldSelectionCheckListBox.Create(AOwner: TComponent);
begin
  inherited;
  fPublisher := TBoldPublisher.Create;
{
  TBoldGetAsCheckBoxState = function (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): TCheckBoxState of object;
  TBoldSetAsCheckBoxState = procedure (Element: TBoldElement; newValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
}
  fCheckBoxRenderer := TBoldAsCheckBoxStateRenderer.Create(self);
  fCheckBoxRenderer.OnGetAsCheckBoxState := GetAsCheckBoxState;
  fCheckBoxRenderer.OnSetAsCheckBoxState := SetAsCheckBoxState;
  fCheckBoxRenderer.OnSubscribe := OnSubscribe;
  BoldRowCheckBoxProperties.Renderer := fCheckBoxRenderer;
end;

destructor TcxBoldSelectionCheckListBox.Destroy;
begin
  fPublisher.NotifySubscribersAndClearSubscriptions(self);
  FreeAndNil(fPublisher);
  inherited;
end;

function TcxBoldSelectionCheckListBox.GetAsCheckBoxState(
  aFollower: TBoldFollower
  ): TCheckBoxState;
begin
  if Assigned(BoldSelectionHandle) then
  begin
    if (BoldSelectionHandle.List.IndexOf(aFollower.Element) <> -1 ) then
      Result := cbChecked
    else
      Result := cbUnChecked;
   end
   else
     Result := cbGrayed;
end;

procedure TcxBoldSelectionCheckListBox.SetAsCheckBoxState(
  aFollower: TBoldFollower; newValue: TCheckBoxState);
var
  lElement: TBoldElement;
begin
  lElement := aFollower.Element;
  if Assigned(BoldSelectionHandle) then
  begin
    case newValue of
      cbChecked: BoldSelectionHandle.MutableList.Add(lElement);
      cbUnChecked: if (BoldSelectionHandle.List.IndexOf(lElement) <> -1) then BoldSelectionHandle.MutableList.Remove(lElement);
      cbGrayed: ;
    end;
  end;
end;

procedure TcxBoldSelectionCheckListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = BoldSelectionHandle) and (Operation = opRemove) then
    BoldSelectionHandle := nil;
end;

procedure TcxBoldSelectionCheckListBox.OnSubscribe(
  aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
begin
  if Assigned(BoldSelectionHandle) then
  begin
    BoldSelectionHandle.AddSmallSubscription(Subscriber, [beValueIdentityChanged, beDestroying], breReSubscribe);
    if Assigned(BoldSelectionHandle.List) then
      BoldSelectionHandle.List.DefaultSubscribe(Subscriber);
  end;
  fPublisher.AddSubscription(Subscriber, beSelectionHandleChanged, breReSubscribe);
end;

procedure TcxBoldSelectionCheckListBox.SetSelectionHandle(
  const Value: TBoldAbstractListHandle);
begin
  if (fBoldSelectionHandle <> Value) then
  begin
    fBoldSelectionHandle := Value;
    fPublisher.SendExtendedEvent(self, beSelectionHandleChanged, []);
  end;
end;

function TcxBoldSelectionCheckListBox.ValidateComponent(
  ComponentValidator: TBoldComponentValidator;
  NamePrefix: string): Boolean;
var
  lContext: TBoldElementTypeInfo;
begin
  result := false;
  lContext := GetContextType;
  if assigned(lContext) then
  begin
    result := ComponentValidator.ValidateExpressionInContext(
      BoldRowProperties.Expression,
      lContext,
      format('%s %s.BoldRowProperties.Expression', [NamePrefix, Name]), BoldRowProperties.VariableList); // do not localize
  end;
end;

{ TcxBoldCheckListBox }

function TcxBoldCheckListBox.ValidateComponent(
  ComponentValidator: TBoldComponentValidator;
  NamePrefix: string): Boolean;
var
  lContext: TBoldElementTypeInfo;
begin
  result := false;
  lContext := GetContextType;
  if assigned(lContext) then
  begin
    result := ComponentValidator.ValidateExpressionInContext(
      BoldRowProperties.Expression,
      lContext,
      format('%s %s.BoldRowProperties.Expression', [NamePrefix, Name]), BoldRowProperties.VariableList); // do not localize
    result := ComponentValidator.ValidateExpressionInContext(
      BoldRowCheckBoxProperties.Expression,
      lContext,
      format('%s %s.BoldRowProperties.Expression', [NamePrefix, Name]), BoldRowCheckBoxProperties.VariableList) and result; // do not localize
  end;
end;

initialization
  GetRegisteredEditProperties.Register(TcxBoldTextEditProperties, scxSBoldEditRepositoryTextItem);
  GetRegisteredEditProperties.Register(TcxBoldComboBoxProperties, scxSBoldComboBoxRepositoryTextItem);
  FilterEditsController.Register(TcxBoldTextEditProperties, TcxFilterTextEditHelper);
  FilterEditsController.Register(TcxBoldComboBoxProperties, TcxFilterComboBoxHelper);
  dxBarRegisterItem(TcxBarBoldEditItem, TcxBarEditItemControl, True);
//  BarDesignController.RegisterBarControlEditor(TcxItemsEditorEx);


finalization
  dxBarUnregisterItem(TcxBarBoldEditItem);
  FilterEditsController.Unregister(TcxBoldTextEditProperties, TcxFilterTextEditHelper);
  FilterEditsController.Unregister(TcxBoldComboBoxProperties, TcxFilterComboBoxHelper);
  GetRegisteredEditProperties.UnRegister(TcxBoldTextEditProperties);
  GetRegisteredEditProperties.UnRegister(TcxBoldComboBoxProperties);
//  BarDesignController.UnregisterBarControlEditor(TcxItemsEditorEx);

end.
