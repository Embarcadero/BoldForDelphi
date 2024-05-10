
{ Global compiler directives }
{$include bold.inc}
unit RationalRose98_TLB;

{$WARNINGS OFF}
{$WARN SYMBOL_PLATFORM OFF}

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ Version 4.2 }

{ Conversion log:
  Warning: 'Class' is a reserved word. IRoseRole.Class changed to Class_
  Warning: 'Type' is a reserved word. IRoseProperty.Type changed to Type_
  Warning: 'Const' is a reserved word. IRoseParameter.Const changed to Const_
  Warning: 'Type' is a reserved word. IRoseParameter.Type changed to Type_
  Warning: 'Type' is a reserved word. IRoseModule.Type changed to Type_
  Warning: 'Type' is a reserved word. IRoseAttribute.Type changed to Type_
  Warning: 'Class' is a reserved word. Parameter 'Class' in IRoseAssociation.GetCorrespondingRole changed to 'Class_'
  Warning: 'Class' is a reserved word. Parameter 'Class' in IRoseAssociation.GetOtherRole changed to 'Class_'
  Warning: 'Type' is a reserved word. Parameter 'Type' in IRoseCategory.AddScenarioDiagram changed to 'Type_'
 }

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;

const
  LIBID_RationalRose: TGUID = '{860CC660-1C2B-11D0-B1B1-444553540000}';

const

{ RosePersistence }

  rsPersistent = 0;
  rsTransient = 1;

{ RoseSynchronization }

  rsSimple = 0;
  rsSynchronous = 1;
  rsBalking = 2;
  rsTimeout = 3;
  rsAsynchronous = 4;

{ RoseFrequency }

  rsAperiodic = 0;
  rsPeriodic = 1;

const

{ Component class GUIDs }
  Class_RoseProcessorCollection: TGUID = '{BA376ECF-A44E-11D0-BC02-00A024C67143}';
  Class_RoseCategoryCollection: TGUID = '{BA376EE3-A44E-11D0-BC02-00A024C67143}';
  Class_RoseItem: TGUID = '{97B3836E-A4E3-11D0-BFF0-00AA003DEF5B}';
  Class_RoseAddIn: TGUID = '{D5352FC1-346C-11D1-883B-3C8B00C10000}';
  Class_RoseUseCaseCollection: TGUID = '{BA376EDE-A44E-11D0-BC02-00A024C67143}';
  Class_RoseRelation: TGUID = '{97B38370-A4E3-11D0-BFF0-00AA003DEF5B}';
  Class_RoseApplication: TGUID = '{D7BC1B41-8618-11CF-B3D4-00A0241DB1D0}';
  Class_RoseMessageCollection: TGUID = '{BA376ECD-A44E-11D0-BC02-00A024C67143}';
  Class_RoseClassDiagramCollection: TGUID = '{BA376ECB-A44E-11D0-BC02-00A024C67143}';
  Class_RoseScenarioDiagram: TGUID = '{4782FBAF-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseRealizeRelation: TGUID = '{6AC2BA82-454D-11D1-883B-3C8B00C10000}';
  Class_RoseHasRelationship: TGUID = '{4782FBA4-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseClassView: TGUID = '{86652271-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseView_FillColor: TGUID = '{CE5BE564-0380-11D1-BC11-00A024C67143}';
  Class_RoseActionCollection: TGUID = '{BA376EEF-A44E-11D0-BC02-00A024C67143}';
  Class_RoseProcess: TGUID = '{4782FBAE-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseAddInCollection: TGUID = '{C87D2BC0-352A-11D1-883B-3C8B00C10000}';
  Class_RoseControllableUnitCollection: TGUID = '{BA376ED2-A44E-11D0-BC02-00A024C67143}';
  Class_RoseModuleCollection: TGUID = '{BA376ED5-A44E-11D0-BC02-00A024C67143}';
  Class_RoseLinkCollection: TGUID = '{9DE9A9C2-F2D0-11D0-883A-3C8B00C10000}';
  Class_RoseAction: TGUID = '{86652270-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseParameterCollection: TGUID = '{BA376EDA-A44E-11D0-BC02-00A024C67143}';
  Class_RoseAttributeCollection: TGUID = '{BA376ED6-A44E-11D0-BC02-00A024C67143}';
  Class_RoseDevice: TGUID = '{86652275-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseClassDependency: TGUID = '{4ACE189A-6CD3-11D1-BC1E-00A024C67143}';
  Class_RoseRole: TGUID = '{97B38378-A4E3-11D0-BFF0-00AA003DEF5B}';
  Class_RoseClass: TGUID = '{8665226F-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseElement: TGUID = '{97B38396-A4E3-11D0-BFF0-00AA003DEF5B}';
  Class_RoseControllableUnit: TGUID = '{97B38379-A4E3-11D0-BFF0-00AA003DEF5B}';
  Class_RoseModel: TGUID = '{4782FBA6-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseTransition: TGUID = '{4782FBB5-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseSubsystemCollection: TGUID = '{BA376ED4-A44E-11D0-BC02-00A024C67143}';
  Class_RoseProcessor: TGUID = '{4782FBAD-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseCategoryDependencyCollection: TGUID = '{4ACE189E-6CD3-11D1-BC1E-00A024C67143}';
  Class_RoseProperty: TGUID = '{4782FBAB-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseStateDiagram: TGUID = '{4782FBB1-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseEvent: TGUID = '{86652276-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseRichType: TGUID = '{97B38380-A4E3-11D0-BFF0-00AA003DEF5B}';
  Class_RoseScenarioDiagramCollection: TGUID = '{BA376ED1-A44E-11D0-BC02-00A024C67143}';
  Class_RoseParameter: TGUID = '{4782FBAA-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseOperation: TGUID = '{4782FBA8-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseView_LineColor: TGUID = '{CE5BE566-0380-11D1-BC11-00A024C67143}';
  Class_RoseAddInManager: TGUID = '{D5352FC3-346C-11D1-883B-3C8B00C10000}';
  Class_RoseStateDiagramCollection: TGUID = '{BA376EEA-A44E-11D0-BC02-00A024C67143}';
  Class_RoseItemViewCollection: TGUID = '{BA376EE1-A44E-11D0-BC02-00A024C67143}';
  Class_RosePropertyCollection: TGUID = '{BA376ED0-A44E-11D0-BC02-00A024C67143}';
  Class_RoseOperationCollection: TGUID = '{BA376ED7-A44E-11D0-BC02-00A024C67143}';
  Class_RoseDeviceCollection: TGUID = '{BA376ECA-A44E-11D0-BC02-00A024C67143}';
  Class_RoseObject: TGUID = '{97B38389-A4E3-11D0-BFF0-00AA003DEF5B}';
  Class_RoseModuleVisibilityRelationship: TGUID = '{8665227D-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseComponentViewCollection: TGUID = '{9DE9A9C3-F2D0-11D0-883A-3C8B00C10000}';
  Class_RoseHasRelationshipCollection: TGUID = '{BA376ED9-A44E-11D0-BC02-00A024C67143}';
  Class_RoseClassViewCollection: TGUID = '{BA376EC7-A44E-11D0-BC02-00A024C67143}';
  Class_RoseDeploymentDiagram: TGUID = '{86652274-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseInstanceView: TGUID = '{86652279-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseLink: TGUID = '{8665227A-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseObjectInstance: TGUID = '{4782FBA1-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseCategoryDependency: TGUID = '{4ACE189C-6CD3-11D1-BC1E-00A024C67143}';
  Class_RoseInheritRelation: TGUID = '{86652278-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseView_Font: TGUID = '{CE5BE568-0380-11D1-BC11-00A024C67143}';
  Class_RoseStateMachine: TGUID = '{4782FBB2-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseModule: TGUID = '{4782FBA7-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseUseCase: TGUID = '{4782FBB6-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseItemCollection: TGUID = '{0DD9ACF7-D06E-11D0-BC0B-00A024C67143}';
  Class_RoseNoteViewCollection: TGUID = '{BA376EE0-A44E-11D0-BC02-00A024C67143}';
  Class_RoseInheritRelationCollection: TGUID = '{BA376EDC-A44E-11D0-BC02-00A024C67143}';
  Class_RoseDeploymentDiagramCollection: TGUID = '{BA376EC8-A44E-11D0-BC02-00A024C67143}';
  Class_RoseStringCollection: TGUID = '{4782FBB8-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseStateViewCollection: TGUID = '{BA376EEC-A44E-11D0-BC02-00A024C67143}';
  Class_RoseProcessCollection: TGUID = '{BA376EE6-A44E-11D0-BC02-00A024C67143}';
  Class_RoseAssociationCollection: TGUID = '{BA376ED8-A44E-11D0-BC02-00A024C67143}';
  Class_RoseModuleDiagramCollection: TGUID = '{BA376ECC-A44E-11D0-BC02-00A024C67143}';
  Class_RoseDiagram: TGUID = '{97B3838E-A4E3-11D0-BFF0-00AA003DEF5B}';
  Class_RoseRichTypeValuesCollection: TGUID = '{97B38390-A4E3-11D0-BFF0-00AA003DEF5B}';
  Class_RoseSubsystemView: TGUID = '{4782FBB4-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseComponentView: TGUID = '{86652273-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseAttribute: TGUID = '{4782FBA3-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseClassDiagram: TGUID = '{86652272-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseNoteView: TGUID = '{4782FBA0-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RosePackage: TGUID = '{97B38392-A4E3-11D0-BFF0-00AA003DEF5B}';
  Class_RosePathMap: TGUID = '{97B38395-A4E3-11D0-BFF0-00AA003DEF5B}';
  Class_RoseModuleVisibilityRelationshipCollection: TGUID = '{BA376EE2-A44E-11D0-BC02-00A024C67143}';
  Class_RoseClassCollection: TGUID = '{BA376ED3-A44E-11D0-BC02-00A024C67143}';
  Class_RoseMessage: TGUID = '{8665227C-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseClassDependencyCollection: TGUID = '{ED042E50-6CDE-11D1-BC1E-00A024C67143}';
  Class_RoseAssociation: TGUID = '{4782FBA2-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseEventCollection: TGUID = '{BA376EE8-A44E-11D0-BC02-00A024C67143}';
  Class_RoseStateCollection: TGUID = '{BA376EE9-A44E-11D0-BC02-00A024C67143}';
  Class_RoseStateView: TGUID = '{4782FBB3-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseSubsystemViewCollection: TGUID = '{0CEEA5A1-C6F8-11D0-BFF0-00AA003DEF5B}';
  Class_RoseModuleDiagram: TGUID = '{8665227B-EBF7-11D0-BC10-00A024C67143}';
  Class_RoseSubsystem: TGUID = '{4782FBAC-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseExternalDocumentCollection: TGUID = '{BA376EDF-A44E-11D0-BC02-00A024C67143}';
  Class_RoseInstanceViewCollection: TGUID = '{C640C862-F2D3-11D0-883A-3C8B00C10000}';
  Class_RoseItemView: TGUID = '{97B3839B-A4E3-11D0-BFF0-00AA003DEF5B}';
  Class_RoseTransitionCollection: TGUID = '{BA376EEE-A44E-11D0-BC02-00A024C67143}';
  Class_RoseState: TGUID = '{4782FBB0-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseObjectInstanceCollection: TGUID = '{BA376ECE-A44E-11D0-BC02-00A024C67143}';
  Class_RoseRoleCollection: TGUID = '{BA376EDB-A44E-11D0-BC02-00A024C67143}';
  Class_RoseClassRelation: TGUID = '{97B3839E-A4E3-11D0-BFF0-00AA003DEF5B}';
  Class_RoseCategory: TGUID = '{4782FBA9-ECD5-11D0-BFF0-00AA003DEF5B}';
  Class_RoseDefaultModelProperties: TGUID = '{A51B4041-3E99-11D1-883B-3C8B00C10000}';
  Class_RoseRealizeRelationCollection: TGUID = '{67448182-4553-11D1-883B-3C8B00C10000}';
  Class_RosePackageCollection: TGUID = '{BA376EE4-A44E-11D0-BC02-00A024C67143}';
  Class_RoseExternalDocument: TGUID = '{86652277-EBF7-11D0-BC10-00A024C67143}';

type

{ Forward declarations: Interfaces }
  IRoseProcessorCollection = dispinterface;
  IRoseCategoryCollection = dispinterface;
  IRoseItem = dispinterface;
  IRoseAddIn = dispinterface;
  IRoseUseCaseCollection = dispinterface;
  IRoseRelation = dispinterface;
  IRoseApplication = dispinterface;
  IRoseMessageCollection = dispinterface;
  IRoseClassDiagramCollection = dispinterface;
  IRoseScenarioDiagram = dispinterface;
  IRoseRealizeRelation = dispinterface;
  IRoseHasRelationship = dispinterface;
  IRoseClassView = dispinterface;
  IRoseView_FillColor = dispinterface;
  IRoseActionCollection = dispinterface;
  IRoseProcess = dispinterface;
  IRoseAddInCollection = dispinterface;
  IRoseControllableUnitCollection = dispinterface;
  IRoseModuleCollection = dispinterface;
  IRoseLinkCollection = dispinterface;
  IRoseAction = dispinterface;
  IRoseParameterCollection = dispinterface;
  IRoseAttributeCollection = dispinterface;
  IRoseDevice = dispinterface;
  IRoseClassDependency = dispinterface;
  IRoseRole = dispinterface;
  IRoseClass = dispinterface;
  IRoseElement = dispinterface;
  IRoseControllableUnit = dispinterface;
  IRoseModel = dispinterface;
  IRoseTransition = dispinterface;
  IRoseSubsystemCollection = dispinterface;
  IRoseProcessor = dispinterface;
  IRoseCategoryDependencyCollection = dispinterface;
  IRoseProperty = dispinterface;
  IRoseStateDiagram = dispinterface;
  IRoseEvent = dispinterface;
  IRoseRichType = dispinterface;
  IRoseScenarioDiagramCollection = dispinterface;
  IRoseParameter = dispinterface;
  IRoseOperation = dispinterface;
  IRoseView_LineColor = dispinterface;
  IRoseAddInManager = dispinterface;
  IRoseStateDiagramCollection = dispinterface;
  IRoseItemViewCollection = dispinterface;
  IRosePropertyCollection = dispinterface;
  IRoseOperationCollection = dispinterface;
  IRoseDeviceCollection = dispinterface;
  IRoseObject = dispinterface;
  IRoseModuleVisibilityRelationship = dispinterface;
  IRoseComponentViewCollection = dispinterface;
  IRoseHasRelationshipCollection = dispinterface;
  IRoseClassViewCollection = dispinterface;
  IRoseDeploymentDiagram = dispinterface;
  IRoseInstanceView = dispinterface;
  IRoseLink = dispinterface;
  IRoseObjectInstance = dispinterface;
  IRoseCategoryDependency = dispinterface;
  IRoseInheritRelation = dispinterface;
  IRoseView_Font = dispinterface;
  IRoseStateMachine = dispinterface;
  IRoseModule = dispinterface;
  IRoseUseCase = dispinterface;
  IRoseItemCollection = dispinterface;
  IRoseNoteViewCollection = dispinterface;
  IRoseInheritRelationCollection = dispinterface;
  IRoseDeploymentDiagramCollection = dispinterface;
  IRoseStringCollection = dispinterface;
  IRoseStateViewCollection = dispinterface;
  IRoseProcessCollection = dispinterface;
  IRoseAssociationCollection = dispinterface;
  IRoseModuleDiagramCollection = dispinterface;
  IRoseDiagram = dispinterface;
  IRoseRichTypeValuesCollection = dispinterface;
  IRoseSubsystemView = dispinterface;
  IRoseComponentView = dispinterface;
  IRoseAttribute = dispinterface;
  IRoseClassDiagram = dispinterface;
  IRoseNoteView = dispinterface;
  IRosePackage = dispinterface;
  IRosePathMap = dispinterface;
  IRoseModuleVisibilityRelationshipCollection = dispinterface;
  IRoseClassCollection = dispinterface;
  IRoseMessage = dispinterface;
  IRoseClassDependencyCollection = dispinterface;
  IRoseAssociation = dispinterface;
  IRoseEventCollection = dispinterface;
  IRoseStateCollection = dispinterface;
  IRoseStateView = dispinterface;
  IRoseSubsystemViewCollection = dispinterface;
  IRoseModuleDiagram = dispinterface;
  IRoseSubsystem = dispinterface;
  IRoseExternalDocumentCollection = dispinterface;
  IRoseInstanceViewCollection = dispinterface;
  IRoseItemView = dispinterface;
  IRoseTransitionCollection = dispinterface;
  IRoseState = dispinterface;
  IRoseObjectInstanceCollection = dispinterface;
  IRoseRoleCollection = dispinterface;
  IRoseClassRelation = dispinterface;
  IRoseCategory = dispinterface;
  IRoseDefaultModelProperties = dispinterface;
  IRoseRealizeRelationCollection = dispinterface;
  IRosePackageCollection = dispinterface;
  IRoseExternalDocument = dispinterface;

{ Forward declarations: CoClasses }
  RoseProcessorCollection = IRoseProcessorCollection;
  RoseCategoryCollection = IRoseCategoryCollection;
  RoseItem = IRoseItem;
  RoseAddIn = IRoseAddIn;
  RoseUseCaseCollection = IRoseUseCaseCollection;
  RoseRelation = IRoseRelation;
  RoseApplication = IRoseApplication;
  RoseMessageCollection = IRoseMessageCollection;
  RoseClassDiagramCollection = IRoseClassDiagramCollection;
  RoseScenarioDiagram = IRoseScenarioDiagram;
  RoseRealizeRelation = IRoseRealizeRelation;
  RoseHasRelationship = IRoseHasRelationship;
  RoseClassView = IRoseClassView;
  RoseView_FillColor = IRoseView_FillColor;
  RoseActionCollection = IRoseActionCollection;
  RoseProcess = IRoseProcess;
  RoseAddInCollection = IRoseAddInCollection;
  RoseControllableUnitCollection = IRoseControllableUnitCollection;
  RoseModuleCollection = IRoseModuleCollection;
  RoseLinkCollection = IRoseLinkCollection;
  RoseAction = IRoseAction;
  RoseParameterCollection = IRoseParameterCollection;
  RoseAttributeCollection = IRoseAttributeCollection;
  RoseDevice = IRoseDevice;
  RoseClassDependency = IRoseClassDependency;
  RoseRole = IRoseRole;
  RoseClass = IRoseClass;
  RoseElement = IRoseElement;
  RoseControllableUnit = IRoseControllableUnit;
  RoseModel = IRoseModel;
  RoseTransition = IRoseTransition;
  RoseSubsystemCollection = IRoseSubsystemCollection;
  RoseProcessor = IRoseProcessor;
  RoseCategoryDependencyCollection = IRoseCategoryDependencyCollection;
  RoseProperty = IRoseProperty;
  RoseStateDiagram = IRoseStateDiagram;
  RoseEvent = IRoseEvent;
  RoseRichType = IRoseRichType;
  RoseScenarioDiagramCollection = IRoseScenarioDiagramCollection;
  RoseParameter = IRoseParameter;
  RoseOperation = IRoseOperation;
  RoseView_LineColor = IRoseView_LineColor;
  RoseAddInManager = IRoseAddInManager;
  RoseStateDiagramCollection = IRoseStateDiagramCollection;
  RoseItemViewCollection = IRoseItemViewCollection;
  RosePropertyCollection = IRosePropertyCollection;
  RoseOperationCollection = IRoseOperationCollection;
  RoseDeviceCollection = IRoseDeviceCollection;
  RoseObject = IRoseObject;
  RoseModuleVisibilityRelationship = IRoseModuleVisibilityRelationship;
  RoseComponentViewCollection = IRoseComponentViewCollection;
  RoseHasRelationshipCollection = IRoseHasRelationshipCollection;
  RoseClassViewCollection = IRoseClassViewCollection;
  RoseDeploymentDiagram = IRoseDeploymentDiagram;
  RoseInstanceView = IRoseInstanceView;
  RoseLink = IRoseLink;
  RoseObjectInstance = IRoseObjectInstance;
  RoseCategoryDependency = IRoseCategoryDependency;
  RoseInheritRelation = IRoseInheritRelation;
  RoseView_Font = IRoseView_Font;
  RoseStateMachine = IRoseStateMachine;
  RoseModule = IRoseModule;
  RoseUseCase = IRoseUseCase;
  RoseItemCollection = IRoseItemCollection;
  RoseNoteViewCollection = IRoseNoteViewCollection;
  RoseInheritRelationCollection = IRoseInheritRelationCollection;
  RoseDeploymentDiagramCollection = IRoseDeploymentDiagramCollection;
  RoseStringCollection = IRoseStringCollection;
  RoseStateViewCollection = IRoseStateViewCollection;
  RoseProcessCollection = IRoseProcessCollection;
  RoseAssociationCollection = IRoseAssociationCollection;
  RoseModuleDiagramCollection = IRoseModuleDiagramCollection;
  RoseDiagram = IRoseDiagram;
  RoseRichTypeValuesCollection = IRoseRichTypeValuesCollection;
  RoseSubsystemView = IRoseSubsystemView;
  RoseComponentView = IRoseComponentView;
  RoseAttribute = IRoseAttribute;
  RoseClassDiagram = IRoseClassDiagram;
  RoseNoteView = IRoseNoteView;
  RosePackage = IRosePackage;
  RosePathMap = IRosePathMap;
  RoseModuleVisibilityRelationshipCollection = IRoseModuleVisibilityRelationshipCollection;
  RoseClassCollection = IRoseClassCollection;
  RoseMessage = IRoseMessage;
  RoseClassDependencyCollection = IRoseClassDependencyCollection;
  RoseAssociation = IRoseAssociation;
  RoseEventCollection = IRoseEventCollection;
  RoseStateCollection = IRoseStateCollection;
  RoseStateView = IRoseStateView;
  RoseSubsystemViewCollection = IRoseSubsystemViewCollection;
  RoseModuleDiagram = IRoseModuleDiagram;
  RoseSubsystem = IRoseSubsystem;
  RoseExternalDocumentCollection = IRoseExternalDocumentCollection;
  RoseInstanceViewCollection = IRoseInstanceViewCollection;
  RoseItemView = IRoseItemView;
  RoseTransitionCollection = IRoseTransitionCollection;
  RoseState = IRoseState;
  RoseObjectInstanceCollection = IRoseObjectInstanceCollection;
  RoseRoleCollection = IRoseRoleCollection;
  RoseClassRelation = IRoseClassRelation;
  RoseCategory = IRoseCategory;
  RoseDefaultModelProperties = IRoseDefaultModelProperties;
  RoseRealizeRelationCollection = IRoseRealizeRelationCollection;
  RosePackageCollection = IRosePackageCollection;
  RoseExternalDocument = IRoseExternalDocument;

{ Forward declarations: Enums }
  RosePersistence = TOleEnum;
  RoseSynchronization = TOleEnum;
  RoseFrequency = TOleEnum;

  IRoseProcessorCollection = dispinterface
    ['{97B3835C-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseProcessor; dispid 203;
    function Exists(const pObject: IRoseProcessor): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseProcessor): Smallint; dispid 207;
    procedure Add(const theObject: IRoseProcessor); dispid 208;
    procedure AddCollection(const theCollection: IRoseProcessorCollection); dispid 209;
    procedure Remove(const theObject: IRoseProcessor); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseProcessor; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseProcessor; dispid 213;
  end;

  IRoseCategoryCollection = dispinterface
    ['{97B3835B-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseCategory; dispid 203;
    function Exists(const pObject: IRoseCategory): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseCategory): Smallint; dispid 207;
    procedure Add(const theObject: IRoseCategory); dispid 208;
    procedure AddCollection(const theCollection: IRoseCategoryCollection); dispid 209;
    procedure Remove(const theObject: IRoseCategory); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseCategory; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseCategory; dispid 213;
  end;

  IRoseItem = dispinterface
    ['{BC57D1C2-863E-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseAddIn = dispinterface
    ['{D5352FC0-346C-11D1-883B-3C8B00C10000}']
    property EventHandler: IDispatch dispid 12528;
    property Version: WideString dispid 12530;
    property CompanyName: WideString dispid 12531;
    property HelpFilePath: WideString dispid 12532;
    property MenuFilePath: WideString dispid 12533;
    property PropertyFilePath: WideString dispid 12534;
    property InstallDirectory: WideString dispid 12535;
    property RootRegistryPath: WideString dispid 12536;
    property FundamentalTypes: IRoseStringCollection dispid 12537;
    property ToolNames: IRoseStringCollection dispid 12538;
    property ServerName: WideString dispid 12539;
    property Name: WideString dispid 12540;
    property Copyright: WideString dispid 12645;
    function IsLanguageAddIn: WordBool; dispid 12541;
    procedure Deactivate; dispid 12542;
    procedure Activate; dispid 12543;
    function IsActive: WordBool; dispid 12553;
    procedure ExecuteScript(const FileName: WideString); dispid 12595;
    function ReadSetting(const Section, Entry, Default: WideString): WideString; dispid 12596;
    function WriteSetting(const Section, Entry, Value: WideString): WordBool; dispid 12597;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseUseCaseCollection = dispinterface
    ['{97B38356-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseUseCase; dispid 203;
    function Exists(const pObject: IRoseUseCase): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseUseCase): Smallint; dispid 207;
    procedure Add(const theObject: IRoseUseCase); dispid 208;
    procedure AddCollection(const theCollection: IRoseUseCaseCollection); dispid 209;
    procedure Remove(const theObject: IRoseUseCase); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseUseCase; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseUseCase; dispid 213;
  end;

  IRoseRelation = dispinterface
    ['{BA242E02-8961-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property SupplierName: WideString dispid 412;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function GetClient: IRoseItem; dispid 12608;
    function GetSupplier: IRoseItem; dispid 12609;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseApplication = dispinterface
    ['{D7BC1B40-8618-11CF-B3D4-00A0241DB1D0}']
    property Visible: WordBool dispid 202;
    property Top: Smallint dispid 205;
    property Left: Smallint dispid 206;
    property Height: Smallint dispid 207;
    property Width: Smallint dispid 208;
    property CurrentModel: IRoseModel dispid 209;
    property PathMap: IRosePathMap dispid 224;
    property Version: WideString dispid 231;
    property ProductName: WideString dispid 232;
    property ApplicationPath: WideString dispid 233;
    property AddInManager: IRoseAddInManager dispid 12544;
    property CommandLine: WideString dispid 12586;
    function OpenModel(const theModel: WideString): IRoseModel; dispid 210;
    function NewModel: IRoseModel; dispid 211;
    procedure Exit; dispid 212;
    procedure WriteErrorLog(const theMsg: WideString); dispid 213;
    procedure Save(bSaveUnits: WordBool); dispid 214;
    procedure SaveAs(const theFile: WideString; bSaveUnits: WordBool); dispid 215;
    procedure CompileScriptFile(const FileName, BinaryName: WideString; bDebug: WordBool); dispid 218;
    function OpenModelAsTemplate(const szFileName: WideString): IRoseModel; dispid 223;
    procedure OpenScript(const FileName: WideString); dispid 225;
    procedure NewScript; dispid 226;
    function GetLicensedApplication(const theKey: WideString): IRoseApplication; dispid 235;
    procedure ExecuteScript(const pFileName: WideString); dispid 236;
    function OpenURL(const theURL: WideString): WordBool; dispid 12587;
    function OpenExternalDocument(const FileName: WideString): WordBool; dispid 12588;
    function GetProfileString(const Section, Entry, Default: WideString): WideString; dispid 12589;
    function WriteProfileString(const Section, Entry, Value: WideString): WordBool; dispid 12590;
  end;

  IRoseMessageCollection = dispinterface
    ['{97B38359-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseMessage; dispid 203;
    function Exists(const pObject: IRoseMessage): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseMessage): Smallint; dispid 207;
    procedure Add(const theObject: IRoseMessage); dispid 208;
    procedure AddCollection(const theCollection: IRoseMessageCollection); dispid 209;
    procedure Remove(const theObject: IRoseMessage); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseMessage; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseMessage; dispid 213;
  end;

  IRoseClassDiagramCollection = dispinterface
    ['{97B38343-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseClassDiagram; dispid 203;
    function Exists(const pObject: IRoseClassDiagram): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseClassDiagram): Smallint; dispid 207;
    procedure Add(const theObject: IRoseClassDiagram); dispid 208;
    procedure AddCollection(const theCollection: IRoseClassDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseClassDiagram); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseClassDiagram; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseClassDiagram; dispid 213;
  end;

  IRoseScenarioDiagram = dispinterface
    ['{F819833A-FC55-11CF-BBD3-00A024C67143}']
    property Name: WideString dispid 100;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Visible: WordBool dispid 203;
    property Items: IRoseItemCollection dispid 208;
    property InstanceViews: IRoseInstanceViewCollection dispid 423;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property Documentation: WideString dispid 12656;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Invalidate; dispid 205;
    procedure Update; dispid 206;
    function GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function IsActive: WordBool; dispid 209;
    function Exists(const theItem: IRoseItem): WordBool; dispid 210;
    procedure Activate; dispid 211;
    procedure Render(const FileName: WideString); dispid 217;
    function AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function GetNoteViews: IRoseNoteViewCollection; dispid 220;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function GetObjects: IRoseObjectInstanceCollection; dispid 411;
    function GetSelectedObjects: IRoseObjectInstanceCollection; dispid 412;
    function GetMessages: IRoseMessageCollection; dispid 413;
    function GetSelectedMessages: IRoseMessageCollection; dispid 414;
    function CreateMessage(const theName: WideString; const theSender, theReceiver: IRoseObjectInstance; theSequence: Smallint): IRoseMessage; dispid 416;
    function GetSelectedLinks: IRoseLinkCollection; dispid 417;
    function GetDiagramType: Smallint; dispid 418;
    function AddInstanceView(const theInstance: IRoseObjectInstance; AsClassInstance: WordBool): IRoseInstanceView; dispid 419;
    function RemoveInstanceView(const theView: IRoseInstanceView): WordBool; dispid 420;
    function DeleteInstance(const theInstance: IRoseObjectInstance): WordBool; dispid 421;
    function AddInstance(const theName, theClassName: WideString): IRoseObjectInstance; dispid 422;
    function GetSelectedItems: IRoseItemCollection; dispid 12525;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseRealizeRelation = dispinterface
    ['{6AC2BA81-454D-11D1-883B-3C8B00C10000}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property SupplierName: WideString dispid 412;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function GetContextClass: IRoseClass; dispid 12602;
    function GetContextComponent: IRoseModule; dispid 12603;
    function GetSupplierClass: IRoseClass; dispid 12604;
    function GetSupplierComponent: IRoseModule; dispid 12605;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function GetClient: IRoseItem; dispid 12608;
    function GetSupplier: IRoseItem; dispid 12609;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseHasRelationship = dispinterface
    ['{BA242E04-8961-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property SupplierName: WideString dispid 412;
    property ClientCardinality: WideString dispid 817;
    property SupplierCardinality: WideString dispid 818;
    property Static: WordBool dispid 819;
    property ExportControl: IRoseRichType dispid 820;
    property Containment: IRoseRichType dispid 821;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function GetContextClass: IRoseClass; dispid 12600;
    function GetSupplierClass: IRoseClass; dispid 12601;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function GetClient: IRoseItem; dispid 12608;
    function GetSupplier: IRoseItem; dispid 12609;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseClassView = dispinterface
    ['{5F735F36-F9EA-11CF-BBD3-00A024C67143}']
    property Name: WideString dispid 100;
    property YPosition: Smallint dispid 202;
    property XPosition: Smallint dispid 203;
    property Height: Smallint dispid 204;
    property Width: Smallint dispid 205;
    property FillColor: IRoseView_FillColor dispid 206;
    property LineColor: IRoseView_LineColor dispid 208;
    property SubViews: IRoseItemViewCollection dispid 219;
    property ParentView: IRoseItemView dispid 220;
    property Item: IRoseItem dispid 221;
    property ParentDiagram: IRoseDiagram dispid 224;
    property AutomaticResize: WordBool dispid 409;
    property ShowOperationSignature: WordBool dispid 410;
    property ShowAllAttributes: WordBool dispid 411;
    property ShowAllOperations: WordBool dispid 412;
    property SuppressAttributes: WordBool dispid 413;
    property SuppressOperations: WordBool dispid 414;
    property Font: IRoseView_Font dispid 12493;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function SupportsFillColor: WordBool; dispid 210;
    function SupportsLineColor: WordBool; dispid 211;
    function IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function PointInView(x, y: Smallint): WordBool; dispid 214;
    function GetDefaultWidth: Smallint; dispid 215;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetMinHeight: Smallint; dispid 218;
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseView_FillColor = dispinterface
    ['{CE5BE563-0380-11D1-BC11-00A024C67143}']
    property Red: Smallint dispid 12493;
    property Transparent: WordBool dispid 12494;
    property Blue: Smallint dispid 12495;
    property Green: Smallint dispid 12496;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseActionCollection = dispinterface
    ['{97B3835F-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseAction; dispid 203;
    function Exists(const pObject: IRoseAction): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseAction): Smallint; dispid 207;
    procedure Add(const theObject: IRoseAction); dispid 208;
    procedure AddCollection(const theCollection: IRoseActionCollection); dispid 209;
    procedure Remove(const theObject: IRoseAction); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseAction; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseAction; dispid 213;
  end;

  IRoseProcess = dispinterface
    ['{62C43884-DB5A-11CF-B091-00A0241E3F73}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property MyProcessor: IRoseProcessor dispid 412;
    property Priority: WideString dispid 413;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseAddInCollection = dispinterface
    ['{C87D2BC1-352A-11D1-883B-3C8B00C10000}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseAddIn; dispid 203;
    function Exists(const pObject: IRoseAddIn): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseAddIn): Smallint; dispid 207;
    procedure Add(const theObject: IRoseAddIn); dispid 208;
    procedure AddCollection(const theCollection: IRoseAddInCollection); dispid 209;
    procedure Remove(const theObject: IRoseAddIn); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseAddIn; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseAddIn; dispid 213;
  end;

  IRoseControllableUnitCollection = dispinterface
    ['{97B38360-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseControllableUnit; dispid 203;
    function Exists(const pObject: IRoseControllableUnit): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseControllableUnit): Smallint; dispid 207;
    procedure Add(const theObject: IRoseControllableUnit); dispid 208;
    procedure AddCollection(const theCollection: IRoseControllableUnitCollection); dispid 209;
    procedure Remove(const theObject: IRoseControllableUnit); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseControllableUnit; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseControllableUnit; dispid 213;
  end;

  IRoseModuleCollection = dispinterface
    ['{97B3834B-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseModule; dispid 203;
    function Exists(const pObject: IRoseModule): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseModule): Smallint; dispid 207;
    procedure Add(const theObject: IRoseModule); dispid 208;
    procedure AddCollection(const theCollection: IRoseModuleCollection); dispid 209;
    procedure Remove(const theObject: IRoseModule); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseModule; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseModule; dispid 213;
  end;

  IRoseLinkCollection = dispinterface
    ['{9DE9A9C1-F2D0-11D0-883A-3C8B00C10000}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseLink; dispid 203;
    function Exists(const pObject: IRoseLink): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseLink): Smallint; dispid 207;
    procedure Add(const theObject: IRoseLink); dispid 208;
    procedure AddCollection(const theCollection: IRoseLinkCollection); dispid 209;
    procedure Remove(const theObject: IRoseLink); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseLink; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseLink; dispid 213;
  end;

  IRoseAction = dispinterface
    ['{13881143-93C1-11D0-A214-00A024FFFE40}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property Arguments: WideString dispid 12620;
    property Target: WideString dispid 12621;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseParameterCollection = dispinterface
    ['{97B38352-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseParameter; dispid 203;
    function Exists(const pObject: IRoseParameter): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseParameter): Smallint; dispid 207;
    procedure Add(const theObject: IRoseParameter); dispid 208;
    procedure AddCollection(const theCollection: IRoseParameterCollection); dispid 209;
    procedure Remove(const theObject: IRoseParameter); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseParameter; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseParameter; dispid 213;
  end;

  IRoseAttributeCollection = dispinterface
    ['{97B3834C-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseAttribute; dispid 203;
    function Exists(const pObject: IRoseAttribute): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseAttribute): Smallint; dispid 207;
    procedure Add(const theObject: IRoseAttribute); dispid 208;
    procedure AddCollection(const theCollection: IRoseAttributeCollection); dispid 209;
    procedure Remove(const theObject: IRoseAttribute); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseAttribute; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseAttribute; dispid 213;
  end;

  IRoseDevice = dispinterface
    ['{62C43882-DB5A-11CF-B091-00A0241E3F73}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Characteristics: WideString dispid 412;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetConnectedProcessors: IRoseProcessorCollection; dispid 413;
    function GetConnectedDevices: IRoseDeviceCollection; dispid 414;
    function AddProcessorConnection(const theProcessor: IRoseProcessor): WordBool; dispid 415;
    function RemoveProcessorConnection(const theProcessor: IRoseProcessor): WordBool; dispid 416;
    function AddDeviceConnection(const theDevice: IRoseDevice): WordBool; dispid 417;
    function RemoveDeviceConnection(const theDevice: IRoseDevice): WordBool; dispid 418;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseClassDependency = dispinterface
    ['{4ACE1899-6CD3-11D1-BC1E-00A024C67143}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property SupplierName: WideString dispid 412;
    property ClientCardinality: WideString dispid 817;
    property SupplierCardinality: WideString dispid 818;
    property InvolvesFriendship: WordBool dispid 819;
    property ExportControl: IRoseRichType dispid 820;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function GetContextClass: IRoseClass; dispid 12600;
    function GetSupplierClass: IRoseClass; dispid 12601;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function GetClient: IRoseItem; dispid 12608;
    function GetSupplier: IRoseItem; dispid 12609;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseRole = dispinterface
    ['{BA242E00-8961-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property SupplierName: WideString dispid 412;
    property Aggregate: WordBool dispid 614;
    property Static: WordBool dispid 615;
    property Navigable: WordBool dispid 616;
    property Cardinality: WideString dispid 617;
    property ExportControl: IRoseRichType dispid 618;
    property Containment: IRoseRichType dispid 619;
    property Association: IRoseAssociation dispid 620;
    property Class_: IRoseClass dispid 621;
    property Keys: IRoseAttributeCollection dispid 622;
    property AssociateItem: IRoseItem dispid 626;
    property UseCase: IRoseUseCase dispid 627;
    property Constraints: WideString dispid 629;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property Friend: WordBool dispid 12650;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function AddKey(const theName, theType: WideString): IRoseAttribute; dispid 623;
    function DeleteKey(const theAttr: IRoseAttribute): WordBool; dispid 624;
    function GetClassName: WideString; dispid 625;
    function IsAssociateClass: WordBool; dispid 628;
    function GetQualifiedName: WideString; dispid 12555;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function GetClient: IRoseItem; dispid 12608;
    function GetSupplier: IRoseItem; dispid 12609;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseClass = dispinterface
    ['{BC57D1C0-863E-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Abstract: WordBool dispid 412;
    property Cardinality: WideString dispid 413;
    property Persistence: WordBool dispid 414;
    property ParentCategory: IRoseCategory dispid 415;
    property Attributes: IRoseAttributeCollection dispid 416;
    property Operations: IRoseOperationCollection dispid 417;
    property ExportControl: IRoseRichType dispid 418;
    property ClassKind: IRoseRichType dispid 419;
    property Concurrency: IRoseRichType dispid 420;
    property FundamentalType: WordBool dispid 421;
    property Space: WideString dispid 449;
    property StateMachine: IRoseStateMachine dispid 463;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property ParentClass: IRoseClass dispid 12640;
    property Parameters: IRoseParameterCollection dispid 12666;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetHasRelations: IRoseHasRelationshipCollection; dispid 422;
    function GetInheritRelations: IRoseInheritRelationCollection; dispid 423;
    function GetSuperclasses: IRoseClassCollection; dispid 424;
    function GetAssociations: IRoseAssociationCollection; dispid 425;
    function AddOperation(const theName, retType: WideString): IRoseOperation; dispid 427;
    function AddAttribute(const theName, theType, initVal: WideString): IRoseAttribute; dispid 428;
    function AddAssociation(const theSupplierRoleName, theSupplierRoleType: WideString): IRoseAssociation; dispid 429;
    function AddHas(const theSupplierName, theSupplierType: WideString): IRoseHasRelationship; dispid 430;
    function DeleteHas(const theHas: IRoseHasRelationship): WordBool; dispid 432;
    function DeleteAssociation(const theAss: IRoseAssociation): WordBool; dispid 433;
    function DeleteOperation(const theOper: IRoseOperation): WordBool; dispid 434;
    function DeleteAttribute(const theAttr: IRoseAttribute): WordBool; dispid 435;
    function AddInheritRel(const theRelationName, theParentClassName: WideString): IRoseInheritRelation; dispid 437;
    function DeleteInheritRel(const theInheritRel: IRoseInheritRelation): WordBool; dispid 438;
    function IsALinkClass: WordBool; dispid 439;
    function GetLinkAssociation: IRoseAssociation; dispid 440;
    function GetRoles: IRoseRoleCollection; dispid 444;
    function GetAssociateRoles: IRoseRoleCollection; dispid 445;
    function GetNestedClasses: IRoseClassCollection; dispid 446;
    function AddNestedClass(const theName: WideString): IRoseClass; dispid 447;
    function DeleteNestedClass(const theClass: IRoseClass): WordBool; dispid 448;
    function GetAssignedModules: IRoseModuleCollection; dispid 12491;
    procedure AddAssignedModule(const theModule: IRoseModule); dispid 12522;
    procedure RemoveAssignedModule(const theModule: IRoseModule); dispid 12526;
    function GetQualifiedName: WideString; dispid 12555;
    procedure CreateStateMachine; dispid 12598;
    procedure DeleteStateMachine; dispid 12599;
    function AddRealizeRel(const theRelationName, theInterfaceName: WideString): IRoseRealizeRelation; dispid 12610;
    function DeleteRealizeRel(const theRealizeRel: IRoseRealizeRelation): WordBool; dispid 12611;
    function GetRealizeRelations: IRoseRealizeRelationCollection; dispid 12612;
    function GetAssignedLanguage: WideString; dispid 12642;
    function IsNestedClass: WordBool; dispid 12643;
    function GetSubclasses: IRoseClassCollection; dispid 12644;
    function GetClassDependencies: IRoseClassDependencyCollection; dispid 12662;
    function AddClassDependency(const theSupplerName, theSupplierType: WideString): IRoseClassDependency; dispid 12663;
    function DeleteClassDependency(const theClassDependency: IRoseClassDependency): WordBool; dispid 12664;
    function AddParameter(const theName, theType, theDef: WideString; position: Smallint): IRoseParameter; dispid 12667;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseElement = dispinterface
    ['{D067F15F-6987-11D0-BBF0-00A024C67143}']
    property Name: WideString dispid 100;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseControllableUnit = dispinterface
    ['{32C862A7-8AA9-11D0-A70B-0000F803584A}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function IsControlled: WordBool; dispid 12433;
    function Control(const Path: WideString): WordBool; dispid 12434;
    function IsLoaded: WordBool; dispid 12435;
    function Load: WordBool; dispid 12436;
    function IsModifiable: WordBool; dispid 12438;
    function Unload: WordBool; dispid 12439;
    function Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function GetFileName: WideString; dispid 12441;
    function Save: WordBool; dispid 12442;
    function SaveAs(const Path: WideString): WordBool; dispid 12443;
    function GetQualifiedName: WideString; dispid 12555;
    function IsModified: WordBool; dispid 12654;
    function Uncontrol: WordBool; dispid 12655;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseModel = dispinterface
    ['{E38942A0-8621-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property RootCategory: IRoseCategory dispid 417;
    property RootSubsystem: IRoseSubsystem dispid 418;
    property DeploymentDiagram: IRoseDeploymentDiagram dispid 420;
    property UseCases: IRoseUseCaseCollection dispid 421;
    property RootUseCaseCategory: IRoseCategory dispid 422;
    property DefaultProperties: IRoseDefaultModelProperties dispid 12471;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetAllAssociations: IRoseAssociationCollection; dispid 412;
    function AddProcessor(const pName: WideString): IRoseProcessor; dispid 424;
    function DeleteProcessor(const pProcessor: IRoseProcessor): WordBool; dispid 425;
    function AddDevice(const pName: WideString): IRoseDevice; dispid 426;
    function DeleteDevice(const pDevice: IRoseDevice): WordBool; dispid 427;
    function GetSelectedClasses: IRoseClassCollection; dispid 428;
    function GetSelectedCategories: IRoseCategoryCollection; dispid 429;
    function GetSelectedModules: IRoseModuleCollection; dispid 430;
    function GetSelectedSubsystems: IRoseSubsystemCollection; dispid 431;
    function GetAllClasses: IRoseClassCollection; dispid 432;
    function GetAllCategories: IRoseCategoryCollection; dispid 433;
    function GetAllModules: IRoseModuleCollection; dispid 434;
    function GetAllSubsystems: IRoseSubsystemCollection; dispid 435;
    function GetAllProcessors: IRoseProcessorCollection; dispid 436;
    function GetAllDevices: IRoseDeviceCollection; dispid 437;
    function GetSelectedUseCases: IRoseUseCaseCollection; dispid 438;
    function GetAllUseCases: IRoseUseCaseCollection; dispid 439;
    function IsRootPackage: WordBool; dispid 621;
    function IsControlled: WordBool; dispid 12433;
    function Control(const Path: WideString): WordBool; dispid 12434;
    function IsLoaded: WordBool; dispid 12435;
    function Load: WordBool; dispid 12436;
    function IsModifiable: WordBool; dispid 12438;
    function Unload: WordBool; dispid 12439;
    function Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function GetFileName: WideString; dispid 12441;
    function Save: WordBool; dispid 12442;
    function SaveAs(const Path: WideString): WordBool; dispid 12443;
    function FindItems(const ItemName: WideString): IRoseItemCollection; dispid 12472;
    function FindItemWithID(const UniqueID: WideString): IRoseItem; dispid 12473;
    function FindClasses(const ClassName: WideString): IRoseClassCollection; dispid 12474;
    function FindClassWithID(const UniqueID: WideString): IRoseClass; dispid 12475;
    function FindCategories(const CategoryName: WideString): IRoseCategoryCollection; dispid 12476;
    function FindCategoryWithID(const UniqueID: WideString): IRoseCategory; dispid 12477;
    function GetActiveDiagram: IRoseDiagram; dispid 12527;
    function GetQualifiedName: WideString; dispid 12555;
    function IsModified: WordBool; dispid 12654;
    function Uncontrol: WordBool; dispid 12655;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseTransition = dispinterface
    ['{574130A1-93B8-11D0-A214-00A024FFFE40}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property SupplierName: WideString dispid 412;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function RedirectTo(const newTarget: IRoseState): WordBool; dispid 642;
    function GetQualifiedName: WideString; dispid 12555;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function GetClient: IRoseItem; dispid 12608;
    function GetSupplier: IRoseItem; dispid 12609;
    function GetTriggerAction: IRoseAction; dispid 12629;
    function GetSendAction: IRoseAction; dispid 12630;
    function GetTargetState: IRoseState; dispid 12631;
    function GetSourceState: IRoseState; dispid 12632;
    function GetTriggerEvent: IRoseEvent; dispid 12633;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseSubsystemCollection = dispinterface
    ['{97B3834A-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseSubsystem; dispid 203;
    function Exists(const pObject: IRoseSubsystem): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseSubsystem): Smallint; dispid 207;
    procedure Add(const theObject: IRoseSubsystem); dispid 208;
    procedure AddCollection(const theCollection: IRoseSubsystemCollection); dispid 209;
    procedure Remove(const theObject: IRoseSubsystem); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseSubsystem; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseSubsystem; dispid 213;
  end;

  IRoseProcessor = dispinterface
    ['{62C43886-DB5A-11CF-B091-00A0241E3F73}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Processes: IRoseProcessCollection dispid 412;
    property Characteristics: WideString dispid 413;
    property Scheduling: IRoseRichType dispid 414;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetConnectedDevices: IRoseDeviceCollection; dispid 415;
    function GetConnectedProcessors: IRoseProcessorCollection; dispid 416;
    function AddProcess(const Name: WideString): IRoseProcess; dispid 417;
    function DeleteProcess(const theProcess: IRoseProcess): WordBool; dispid 418;
    function AddProcessorConnection(const Processor: IRoseProcessor): WordBool; dispid 419;
    function RemoveProcessorConnection(const theProcessor: IRoseProcessor): WordBool; dispid 420;
    function AddDeviceConnection(const theDevice: IRoseDevice): WordBool; dispid 421;
    function RemoveDeviceConnection(const theDevice: IRoseDevice): WordBool; dispid 422;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseCategoryDependencyCollection = dispinterface
    ['{4ACE189D-6CD3-11D1-BC1E-00A024C67143}']
  end;

  IRoseProperty = dispinterface
    ['{93461A23-8811-11CF-B1B0-D227D5210B2C}']
    property Name: WideString dispid 202;
    property Value: WideString dispid 203;
    property ToolName: WideString dispid 205;
    property Type_: WideString dispid 206;
  end;

  IRoseStateDiagram = dispinterface
    ['{7ADDA701-9B06-11D0-A214-00A024FFFE40}']
    property Name: WideString dispid 100;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Visible: WordBool dispid 203;
    property Items: IRoseItemCollection dispid 208;
    property Parent: IRoseStateMachine dispid 445;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property Documentation: WideString dispid 12656;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Invalidate; dispid 205;
    procedure Update; dispid 206;
    function GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function IsActive: WordBool; dispid 209;
    function Exists(const theItem: IRoseItem): WordBool; dispid 210;
    procedure Activate; dispid 211;
    procedure Render(const FileName: WideString); dispid 217;
    function AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function GetNoteViews: IRoseNoteViewCollection; dispid 220;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function AddStateView(const aState: IRoseState): IRoseStateView; dispid 421;
    function RemoveStateView(const View: IRoseStateView): WordBool; dispid 422;
    function GetSelectedStateViews: IRoseStateViewCollection; dispid 423;
    function GetStateViews: IRoseStateViewCollection; dispid 424;
    function GetSelectedTransitions: IRoseTransitionCollection; dispid 425;
    function GetStateView(const State: IRoseState): IRoseStateView; dispid 426;
    function GetSelectedStates: IRoseStateCollection; dispid 427;
    function GetSelectedItems: IRoseItemCollection; dispid 12525;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseEvent = dispinterface
    ['{A69CAB22-9179-11D0-A214-00A024FFFE40}']
    property Arguments: WideString dispid 215;
    property Name: WideString dispid 216;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property GuardCondition: WideString dispid 12622;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetQualifiedName: WideString; dispid 12555;
    function GetAction: IRoseAction; dispid 12634;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseRichType = dispinterface
    ['{EB7AAB60-939C-11CF-B091-00A0241E3F73}']
    property Value: Smallint dispid 202;
    property Name: WideString dispid 203;
    property Types: IRoseRichTypeValuesCollection dispid 204;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseScenarioDiagramCollection = dispinterface
    ['{97B3835E-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseScenarioDiagram; dispid 203;
    function Exists(const pObject: IRoseScenarioDiagram): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseScenarioDiagram): Smallint; dispid 207;
    procedure Add(const theObject: IRoseScenarioDiagram); dispid 208;
    procedure AddCollection(const theCollection: IRoseScenarioDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseScenarioDiagram); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseScenarioDiagram; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseScenarioDiagram; dispid 213;
  end;

  IRoseParameter = dispinterface
    ['{C78E7028-86E4-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Const_: WordBool dispid 412;
    property Type_: WideString dispid 413;
    property InitValue: WideString dispid 414;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseOperation = dispinterface
    ['{C78E7020-86E4-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property ReturnType: WideString dispid 412;
    property Parameters: IRoseParameterCollection dispid 413;
    property ExportControl: IRoseRichType dispid 414;
    property Concurrency: IRoseRichType dispid 415;
    property Preconditions: WideString dispid 418;
    property Semantics: WideString dispid 419;
    property Postconditions: WideString dispid 420;
    property Protocol: WideString dispid 421;
    property Qualification: WideString dispid 422;
    property Exceptions: WideString dispid 423;
    property Size: WideString dispid 424;
    property Time: WideString dispid 425;
    property Virtual: WordBool dispid 427;
    property ParentClass: IRoseClass dispid 444;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function AddParameter(const theName, theType, theDef: WideString; position: Smallint): IRoseParameter; dispid 416;
    procedure RemoveAllParameters; dispid 417;
    function DeleteParameter(const theParameter: IRoseParameter): WordBool; dispid 426;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseView_LineColor = dispinterface
    ['{CE5BE565-0380-11D1-BC11-00A024C67143}']
    property Blue: Smallint dispid 12502;
    property Green: Smallint dispid 12503;
    property Red: Smallint dispid 12504;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseAddInManager = dispinterface
    ['{D5352FC2-346C-11D1-883B-3C8B00C10000}']
    property AddIns: IRoseAddInCollection dispid 12529;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseStateDiagramCollection = dispinterface
    ['{97B38368-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseStateDiagram; dispid 203;
    function Exists(const pObject: IRoseStateDiagram): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseStateDiagram): Smallint; dispid 207;
    procedure Add(const theObject: IRoseStateDiagram); dispid 208;
    procedure AddCollection(const theCollection: IRoseStateDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseStateDiagram); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseStateDiagram; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseStateDiagram; dispid 213;
  end;

  IRoseItemViewCollection = dispinterface
    ['{97B38362-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseItemView; dispid 203;
    function Exists(const pObject: IRoseItemView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseItemView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseItemView); dispid 208;
    procedure AddCollection(const theCollection: IRoseItemViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseItemView); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseItemView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseItemView; dispid 213;
  end;

  IRosePropertyCollection = dispinterface
    ['{97B3835D-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseProperty; dispid 203;
    function Exists(const pObject: IRoseProperty): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseProperty): Smallint; dispid 207;
    procedure Add(const theObject: IRoseProperty); dispid 208;
    procedure AddCollection(const theCollection: IRosePropertyCollection); dispid 209;
    procedure Remove(const theObject: IRoseProperty); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseProperty; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseProperty; dispid 213;
  end;

  IRoseOperationCollection = dispinterface
    ['{97B3834D-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseOperation; dispid 203;
    function Exists(const pObject: IRoseOperation): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseOperation): Smallint; dispid 207;
    procedure Add(const theObject: IRoseOperation); dispid 208;
    procedure AddCollection(const theCollection: IRoseOperationCollection); dispid 209;
    procedure Remove(const theObject: IRoseOperation); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseOperation; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseOperation; dispid 213;
  end;

  IRoseDeviceCollection = dispinterface
    ['{97B38342-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseDevice; dispid 203;
    function Exists(const pObject: IRoseDevice): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseDevice): Smallint; dispid 207;
    procedure Add(const theObject: IRoseDevice); dispid 208;
    procedure AddCollection(const theCollection: IRoseDeviceCollection); dispid 209;
    procedure Remove(const theObject: IRoseDevice); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseDevice; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseDevice; dispid 213;
  end;

  IRoseObject = dispinterface
    ['{7D8474B2-2C33-11D0-BBDA-00A024C67143}']
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseModuleVisibilityRelationship = dispinterface
    ['{9EF8DDD6-E697-11CF-BBD1-00A024C67143}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property SupplierName: WideString dispid 412;
    property ContextModule: IRoseModule dispid 614;
    property SupplierModule: IRoseModule dispid 615;
    property ContextSubsystem: IRoseSubsystem dispid 617;
    property SupplierSubsystem: IRoseSubsystem dispid 618;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property ContextClass: IRoseClass dispid 12616;
    property SupplierClass: IRoseClass dispid 12617;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function GetClient: IRoseItem; dispid 12608;
    function GetSupplier: IRoseItem; dispid 12609;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseComponentViewCollection = dispinterface
    ['{C640C861-F2D3-11D0-883A-3C8B00C10000}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseComponentView; dispid 203;
    function Exists(const pObject: IRoseComponentView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseComponentView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseComponentView); dispid 208;
    procedure AddCollection(const theCollection: IRoseComponentViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseComponentView); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseComponentView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseComponentView; dispid 213;
  end;

  IRoseHasRelationshipCollection = dispinterface
    ['{97B38351-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseHasRelationship; dispid 203;
    function Exists(const pObject: IRoseHasRelationship): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseHasRelationship): Smallint; dispid 207;
    procedure Add(const theObject: IRoseHasRelationship); dispid 208;
    procedure AddCollection(const theCollection: IRoseHasRelationshipCollection); dispid 209;
    procedure Remove(const theObject: IRoseHasRelationship); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseHasRelationship; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseHasRelationship; dispid 213;
  end;

  IRoseClassViewCollection = dispinterface
    ['{97B38341-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseClassView; dispid 203;
    function Exists(const pObject: IRoseClassView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseClassView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseClassView); dispid 208;
    procedure AddCollection(const theCollection: IRoseClassViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseClassView); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseClassView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseClassView; dispid 213;
  end;

  IRoseDeploymentDiagram = dispinterface
    ['{C2C15EC4-E028-11CF-B091-00A0241E3F73}']
    property Name: WideString dispid 100;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Visible: WordBool dispid 203;
    property Items: IRoseItemCollection dispid 208;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property Documentation: WideString dispid 12656;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Invalidate; dispid 205;
    procedure Update; dispid 206;
    function GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function IsActive: WordBool; dispid 209;
    function Exists(const theItem: IRoseItem): WordBool; dispid 210;
    procedure Activate; dispid 211;
    procedure Render(const FileName: WideString); dispid 217;
    function AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function GetNoteViews: IRoseNoteViewCollection; dispid 220;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function GetProcessors: IRoseProcessorCollection; dispid 411;
    function GetDevices: IRoseDeviceCollection; dispid 412;
    function AddProcessor(const theProcessor: IRoseProcessor; x, y: Smallint): IRoseItemView; dispid 413;
    function AddDevice(const theDevice: IRoseDevice; x, y: Smallint): IRoseItemView; dispid 414;
    function RemoveProcessor(const theProcessor: IRoseProcessor): WordBool; dispid 415;
    function RemoveDevice(const theDevice: IRoseDevice): WordBool; dispid 416;
    function GetSelectedItems: IRoseItemCollection; dispid 12525;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseInstanceView = dispinterface
    ['{348B1AD4-D5C4-11D0-89F8-0020AFD6C181}']
    property Name: WideString dispid 100;
    property YPosition: Smallint dispid 202;
    property XPosition: Smallint dispid 203;
    property Height: Smallint dispid 204;
    property Width: Smallint dispid 205;
    property FillColor: IRoseView_FillColor dispid 206;
    property LineColor: IRoseView_LineColor dispid 208;
    property SubViews: IRoseItemViewCollection dispid 219;
    property ParentView: IRoseItemView dispid 220;
    property Item: IRoseItem dispid 221;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Font: IRoseView_Font dispid 12493;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function SupportsFillColor: WordBool; dispid 210;
    function SupportsLineColor: WordBool; dispid 211;
    function IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function PointInView(x, y: Smallint): WordBool; dispid 214;
    function GetDefaultWidth: Smallint; dispid 215;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetMinHeight: Smallint; dispid 218;
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function GetInstance: IRoseObjectInstance; dispid 409;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseLink = dispinterface
    ['{195D7852-D5B6-11D0-89F8-0020AFD6C181}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property LinkRole1: IRoseObjectInstance dispid 412;
    property LinkRole2: IRoseObjectInstance dispid 413;
    property LinkRole1Shared: WordBool dispid 414;
    property LinkRole2Visibility: IRoseRichType dispid 415;
    property LinkRole1Visibility: IRoseRichType dispid 417;
    property LinkRole2Shared: WordBool dispid 418;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetMessages: IRoseMessageCollection; dispid 416;
    function DeleteMessage(const TheMessage: IRoseMessage): WordBool; dispid 419;
    function AssignAssociation(const TheAssoc: IRoseAssociation): WordBool; dispid 420;
    function UnassignAssociation: WordBool; dispid 421;
    function AddMessageTo(const Name: WideString; const ToInstance: IRoseObjectInstance; SequenceNumber: Smallint): IRoseMessage; dispid 422;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseObjectInstance = dispinterface
    ['{F8198337-FC55-11CF-BBD3-00A024C67143}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property ClassName: WideString dispid 412;
    property MultipleInstances: WordBool dispid 413;
    property Links: IRoseLinkCollection dispid 416;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property Persistence: Smallint dispid 12651;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function IsClass: WordBool; dispid 414;
    function GetClass: IRoseClass; dispid 415;
    function AddLink(const Name: WideString; const ToInstance: IRoseObjectInstance): IRoseLink; dispid 417;
    function DeleteLink(const aLink: IRoseLink): WordBool; dispid 418;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
  end;

  IRoseCategoryDependency = dispinterface
    ['{4ACE189B-6CD3-11D1-BC1E-00A024C67143}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property SupplierName: WideString dispid 412;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function GetClient: IRoseItem; dispid 12608;
    function GetSupplier: IRoseItem; dispid 12609;
    function GetContextCategory: IRoseCategory; dispid 12657;
    function GetSupplierCategory: IRoseCategory; dispid 12658;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseInheritRelation = dispinterface
    ['{00C99560-9200-11CF-B1B0-D227D5210B2C}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property SupplierName: WideString dispid 412;
    property Virtual: WordBool dispid 817;
    property FriendshipRequired: WordBool dispid 818;
    property ExportControl: IRoseRichType dispid 819;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function GetContextClass: IRoseClass; dispid 12600;
    function GetSupplierClass: IRoseClass; dispid 12601;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function GetClient: IRoseItem; dispid 12608;
    function GetSupplier: IRoseItem; dispid 12609;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseView_Font = dispinterface
    ['{CE5BE567-0380-11D1-BC11-00A024C67143}']
    property Size: Smallint dispid 12497;
    property FaceName: WideString dispid 12498;
    property Blue: Smallint dispid 12499;
    property Green: Smallint dispid 12500;
    property Red: Smallint dispid 12501;
    property Bold: WordBool dispid 12521;
    property Italic: WordBool dispid 12567;
    property Underline: WordBool dispid 12568;
    property StrikeThrough: WordBool dispid 12569;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseStateMachine = dispinterface
    ['{A69CAB21-9179-11D0-A214-00A024FFFE40}']
    property Name: WideString dispid 100;
    property Diagram: IRoseStateDiagram dispid 202;
    property ParentClass: IRoseClass dispid 215;
    property States: IRoseStateCollection dispid 222;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function AddState(const Name: WideString): IRoseState; dispid 217;
    function DeleteState(const State: IRoseState): WordBool; dispid 218;
    function RelocateState(const State: IRoseState): WordBool; dispid 219;
    function GetAllStates: IRoseStateCollection; dispid 220;
    function GetAllTransitions: IRoseTransitionCollection; dispid 221;
    function GetQualifiedName: WideString; dispid 12555;
    function GetTransitions: IRoseTransitionCollection; dispid 12665;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseModule = dispinterface
    ['{C78E702A-86E4-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Path: WideString dispid 412;
    property ParentSubsystem: IRoseSubsystem dispid 413;
    property Type_: IRoseRichType dispid 414;
    property Part: IRoseRichType dispid 415;
    property Declarations: WideString dispid 416;
    property OtherPart: IRoseModule dispid 417;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property AssignedLanguage: WideString dispid 12566;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetAssignedClasses: IRoseClassCollection; dispid 418;
    function AddVisibilityRelationship(const theModule: IRoseModule): IRoseModuleVisibilityRelationship; dispid 419;
    function DeleteVisibilityRelationship(const theVisibilityRelationship: IRoseModuleVisibilityRelationship): WordBool; dispid 420;
    function GetDependencies: IRoseModuleVisibilityRelationshipCollection; dispid 421;
    function GetAllDependencies: IRoseModuleVisibilityRelationshipCollection; dispid 422;
    function GetSubsystemDependencies(const theSubsystem: IRoseSubsystem): IRoseModuleVisibilityRelationshipCollection; dispid 423;
    function AddSubsystemVisibilityRelation(const theSubsystem: IRoseSubsystem): IRoseModuleVisibilityRelationship; dispid 428;
    function GetQualifiedName: WideString; dispid 12555;
    function AddRealizeRel(const theRelationName, theInterfaceName: WideString): IRoseRealizeRelation; dispid 12613;
    function DeleteRealizeRel(const theRealizeRel: IRoseRealizeRelation): WordBool; dispid 12614;
    function GetRealizeRelations: IRoseRealizeRelationCollection; dispid 12615;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseUseCase = dispinterface
    ['{7E7F6EE0-16DE-11D0-8976-00A024774419}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Abstract: WordBool dispid 412;
    property ParentCategory: IRoseCategory dispid 413;
    property ClassDiagrams: IRoseClassDiagramCollection dispid 414;
    property ScenarioDiagrams: IRoseScenarioDiagramCollection dispid 415;
    property StateMachine: IRoseStateMachine dispid 420;
    property Rank: WideString dispid 427;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function AddClassDiagram(const szName: WideString): IRoseClassDiagram; dispid 416;
    function DeleteClassDiagram(const pIDispatch: IRoseClassDiagram): WordBool; dispid 417;
    function DeleteScenarioDiagram(const pIDispatch: IRoseScenarioDiagram): WordBool; dispid 418;
    function AddScenarioDiagram(const szName: WideString; iType: Smallint): IRoseScenarioDiagram; dispid 419;
    function AddInheritRel(const szName, szParentName: WideString): IRoseInheritRelation; dispid 421;
    function DeleteInheritRel(const pIDispatchRelation: IRoseInheritRelation): WordBool; dispid 422;
    function GetAssociations: IRoseAssociationCollection; dispid 426;
    function AddAssociation(const szSupplierRoleName, szSupplierRoleType: WideString): IRoseAssociation; dispid 430;
    function DeleteAssociation(const pDispatchAssociation: IRoseAssociation): WordBool; dispid 431;
    function GetSuperUseCases: IRoseUseCaseCollection; dispid 432;
    function GetInheritRelations: IRoseInheritRelationCollection; dispid 433;
    function GetRoles: IRoseRoleCollection; dispid 434;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseItemCollection = dispinterface
    ['{0DD9ACF8-D06E-11D0-BC0B-00A024C67143}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseItem; dispid 203;
    function Exists(const pObject: IRoseItem): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseItem): Smallint; dispid 207;
    procedure Add(const theObject: IRoseItem); dispid 208;
    procedure AddCollection(const theCollection: IRoseItemCollection); dispid 209;
    procedure Remove(const theObject: IRoseItem); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseItem; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseItem; dispid 213;
  end;

  IRoseNoteViewCollection = dispinterface
    ['{97B38358-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseNoteView; dispid 203;
    function Exists(const pObject: IRoseNoteView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseNoteView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseNoteView); dispid 208;
    procedure AddCollection(const theCollection: IRoseNoteViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseNoteView); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseNoteView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseNoteView; dispid 213;
  end;

  IRoseInheritRelationCollection = dispinterface
    ['{97B38354-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseInheritRelation; dispid 203;
    function Exists(const pObject: IRoseInheritRelation): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseInheritRelation): Smallint; dispid 207;
    procedure Add(const theObject: IRoseInheritRelation); dispid 208;
    procedure AddCollection(const theCollection: IRoseInheritRelationCollection); dispid 209;
    procedure Remove(const theObject: IRoseInheritRelation); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseInheritRelation; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseInheritRelation; dispid 213;
  end;

  IRoseDeploymentDiagramCollection = dispinterface
    ['{97B383A1-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseDeploymentDiagram; dispid 203;
    function Exists(const pObject: IRoseDeploymentDiagram): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseDeploymentDiagram): Smallint; dispid 207;
    procedure Add(const theObject: IRoseDeploymentDiagram); dispid 208;
    procedure AddCollection(const theCollection: IRoseDeploymentDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseDeploymentDiagram); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseDeploymentDiagram; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseDeploymentDiagram; dispid 213;
  end;

  IRoseStringCollection = dispinterface
    ['{6A7FC311-C893-11D0-BC0B-00A024C67143}']
    property Count: Smallint dispid 50;
    function GetAt(id: Smallint): WideString; dispid 51;
  end;

  IRoseStateViewCollection = dispinterface
    ['{97B3836A-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseStateView; dispid 203;
    function Exists(const pObject: IRoseStateView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseStateView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseStateView); dispid 208;
    procedure AddCollection(const theCollection: IRoseStateViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseStateView); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseStateView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseStateView; dispid 213;
  end;

  IRoseProcessCollection = dispinterface
    ['{97B38366-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseProcess; dispid 203;
    function Exists(const pObject: IRoseProcess): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseProcess): Smallint; dispid 207;
    procedure Add(const theObject: IRoseProcess); dispid 208;
    procedure AddCollection(const theCollection: IRoseProcessCollection); dispid 209;
    procedure Remove(const theObject: IRoseProcess); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseProcess; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseProcess; dispid 213;
  end;

  IRoseAssociationCollection = dispinterface
    ['{97B3834E-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseAssociation; dispid 203;
    function Exists(const pObject: IRoseAssociation): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseAssociation): Smallint; dispid 207;
    procedure Add(const theObject: IRoseAssociation); dispid 208;
    procedure AddCollection(const theCollection: IRoseAssociationCollection); dispid 209;
    procedure Remove(const theObject: IRoseAssociation); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseAssociation; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseAssociation; dispid 213;
  end;

  IRoseModuleDiagramCollection = dispinterface
    ['{97B38348-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseModuleDiagram; dispid 203;
    function Exists(const pObject: IRoseModuleDiagram): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseModuleDiagram): Smallint; dispid 207;
    procedure Add(const theObject: IRoseModuleDiagram); dispid 208;
    procedure AddCollection(const theCollection: IRoseModuleDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseModuleDiagram); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseModuleDiagram; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseModuleDiagram; dispid 213;
  end;

  IRoseDiagram = dispinterface
    ['{3FD9D000-93B0-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Visible: WordBool dispid 203;
    property Items: IRoseItemCollection dispid 208;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property Documentation: WideString dispid 12656;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Invalidate; dispid 205;
    procedure Update; dispid 206;
    function GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function IsActive: WordBool; dispid 209;
    function Exists(const theItem: IRoseItem): WordBool; dispid 210;
    procedure Activate; dispid 211;
    procedure Render(const FileName: WideString); dispid 217;
    function AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function GetNoteViews: IRoseNoteViewCollection; dispid 220;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function GetSelectedItems: IRoseItemCollection; dispid 12525;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseRichTypeValuesCollection = dispinterface
    ['{BF8C1040-96DD-11CF-B091-00A0241E3F73}']
    property Count: Smallint dispid 202;
    function GetAt(id: Smallint): WideString; dispid 203;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseSubsystemView = dispinterface
    ['{14028C92-C06C-11D0-89F5-0020AFD6C181}']
    property Name: WideString dispid 100;
    property YPosition: Smallint dispid 202;
    property XPosition: Smallint dispid 203;
    property Height: Smallint dispid 204;
    property Width: Smallint dispid 205;
    property FillColor: IRoseView_FillColor dispid 206;
    property LineColor: IRoseView_LineColor dispid 208;
    property SubViews: IRoseItemViewCollection dispid 219;
    property ParentView: IRoseItemView dispid 220;
    property Item: IRoseItem dispid 221;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Font: IRoseView_Font dispid 12493;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function SupportsFillColor: WordBool; dispid 210;
    function SupportsLineColor: WordBool; dispid 211;
    function IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function PointInView(x, y: Smallint): WordBool; dispid 214;
    function GetDefaultWidth: Smallint; dispid 215;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetMinHeight: Smallint; dispid 218;
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function GetQualifiedName: WideString; dispid 12555;
    function GetSubsystem: IRoseSubsystem; dispid 12592;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseComponentView = dispinterface
    ['{14028C94-C06C-11D0-89F5-0020AFD6C181}']
    property Name: WideString dispid 100;
    property YPosition: Smallint dispid 202;
    property XPosition: Smallint dispid 203;
    property Height: Smallint dispid 204;
    property Width: Smallint dispid 205;
    property FillColor: IRoseView_FillColor dispid 206;
    property LineColor: IRoseView_LineColor dispid 208;
    property SubViews: IRoseItemViewCollection dispid 219;
    property ParentView: IRoseItemView dispid 220;
    property Item: IRoseItem dispid 221;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Font: IRoseView_Font dispid 12493;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function SupportsFillColor: WordBool; dispid 210;
    function SupportsLineColor: WordBool; dispid 211;
    function IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function PointInView(x, y: Smallint): WordBool; dispid 214;
    function GetDefaultWidth: Smallint; dispid 215;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetMinHeight: Smallint; dispid 218;
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function GetQualifiedName: WideString; dispid 12555;
    function GetComponent: IRoseModule; dispid 12585;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseAttribute = dispinterface
    ['{C78E7024-86E4-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property InitValue: WideString dispid 412;
    property Type_: WideString dispid 413;
    property Static: WordBool dispid 414;
    property ExportControl: IRoseRichType dispid 415;
    property Containment: IRoseRichType dispid 416;
    property Derived: WordBool dispid 417;
    property ParentClass: IRoseClass dispid 434;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseClassDiagram = dispinterface
    ['{3FD9D002-93B0-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Visible: WordBool dispid 203;
    property Items: IRoseItemCollection dispid 208;
    property ParentCategory: IRoseCategory dispid 411;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property Documentation: WideString dispid 12656;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Invalidate; dispid 205;
    procedure Update; dispid 206;
    function GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function IsActive: WordBool; dispid 209;
    function Exists(const theItem: IRoseItem): WordBool; dispid 210;
    procedure Activate; dispid 211;
    procedure Render(const FileName: WideString); dispid 217;
    function AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function GetNoteViews: IRoseNoteViewCollection; dispid 220;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function AddClass(const theClass: IRoseClass): WordBool; dispid 412;
    function AddCategory(const theCat: IRoseCategory): WordBool; dispid 413;
    function GetSelectedCategories: IRoseCategoryCollection; dispid 414;
    function GetSelectedClasses: IRoseClassCollection; dispid 415;
    function GetClasses: IRoseClassCollection; dispid 416;
    function GetCategories: IRoseCategoryCollection; dispid 417;
    function AddAssociation(const theAssociation: IRoseAssociation): WordBool; dispid 418;
    function RemoveClass(const theClass: IRoseClass): WordBool; dispid 419;
    function RemoveCategory(const theCategory: IRoseCategory): WordBool; dispid 420;
    function RemoveAssociation(const theAssociation: IRoseAssociation): WordBool; dispid 421;
    function GetAssociations: IRoseAssociationCollection; dispid 422;
    function AddUseCase(const theUseCase: IRoseUseCase): WordBool; dispid 423;
    function RemoveUseCase(const theUseCase: IRoseUseCase): WordBool; dispid 424;
    function GetUseCases: IRoseUseCaseCollection; dispid 425;
    function IsUseCaseDiagram: WordBool; dispid 426;
    function GetClassView(const theClass: IRoseClass): IRoseClassView; dispid 427;
    function GetSelectedItems: IRoseItemCollection; dispid 12525;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseNoteView = dispinterface
    ['{015655CA-72DF-11D0-95EB-0000F803584A}']
    property Name: WideString dispid 100;
    property YPosition: Smallint dispid 202;
    property XPosition: Smallint dispid 203;
    property Height: Smallint dispid 204;
    property Width: Smallint dispid 205;
    property FillColor: IRoseView_FillColor dispid 206;
    property LineColor: IRoseView_LineColor dispid 208;
    property SubViews: IRoseItemViewCollection dispid 219;
    property ParentView: IRoseItemView dispid 220;
    property Item: IRoseItem dispid 221;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Text: WideString dispid 423;
    property Font: IRoseView_Font dispid 12493;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function SupportsFillColor: WordBool; dispid 210;
    function SupportsLineColor: WordBool; dispid 211;
    function IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function PointInView(x, y: Smallint): WordBool; dispid 214;
    function GetDefaultWidth: Smallint; dispid 215;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetMinHeight: Smallint; dispid 218;
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function GetNoteViewType: Smallint; dispid 424;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRosePackage = dispinterface
    ['{47D975C1-8A8D-11D0-A214-444553540000}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function IsRootPackage: WordBool; dispid 621;
    function IsControlled: WordBool; dispid 12433;
    function Control(const Path: WideString): WordBool; dispid 12434;
    function IsLoaded: WordBool; dispid 12435;
    function Load: WordBool; dispid 12436;
    function IsModifiable: WordBool; dispid 12438;
    function Unload: WordBool; dispid 12439;
    function Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function GetFileName: WideString; dispid 12441;
    function Save: WordBool; dispid 12442;
    function SaveAs(const Path: WideString): WordBool; dispid 12443;
    function GetQualifiedName: WideString; dispid 12555;
    function IsModified: WordBool; dispid 12654;
    function Uncontrol: WordBool; dispid 12655;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRosePathMap = dispinterface
    ['{4C9E2241-84C5-11D0-A214-444553540000}']
    function DeleteEntry(const Symbol: WideString): WordBool; dispid 50;
    function GetActualPath(const VirtualPath: WideString): WideString; dispid 51;
    function GetVirtualPath(const ActualPath: WideString): WideString; dispid 52;
    function HasEntry(const Symbol: WideString): WordBool; dispid 53;
    function AddEntry(const Symbol, Path, Comment: WideString): WordBool; dispid 54;
  end;

  IRoseModuleVisibilityRelationshipCollection = dispinterface
    ['{97B38363-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseModuleVisibilityRelationship; dispid 203;
    function Exists(const pObject: IRoseModuleVisibilityRelationship): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseModuleVisibilityRelationship): Smallint; dispid 207;
    procedure Add(const theObject: IRoseModuleVisibilityRelationship); dispid 208;
    procedure AddCollection(const theCollection: IRoseModuleVisibilityRelationshipCollection); dispid 209;
    procedure Remove(const theObject: IRoseModuleVisibilityRelationship); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseModuleVisibilityRelationship; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseModuleVisibilityRelationship; dispid 213;
  end;

  IRoseClassCollection = dispinterface
    ['{97B38349-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseClass; dispid 203;
    function Exists(const pObject: IRoseClass): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseClass): Smallint; dispid 207;
    procedure Add(const theObject: IRoseClass); dispid 208;
    procedure AddCollection(const theCollection: IRoseClassCollection); dispid 209;
    procedure Remove(const theObject: IRoseClass); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseClass; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseClass; dispid 213;
  end;

  IRoseMessage = dispinterface
    ['{F819833C-FC55-11CF-BBD3-00A024C67143}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property Synchronization: Smallint dispid 12652;
    property Frequency: Smallint dispid 12653;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetSenderObject: IRoseObjectInstance; dispid 412;
    function GetReceiverObject: IRoseObjectInstance; dispid 413;
    function IsMessageToSelf: WordBool; dispid 414;
    function IsOperation: WordBool; dispid 415;
    function GetOperation: IRoseOperation; dispid 416;
    function GetLink: IRoseLink; dispid 417;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseClassDependencyCollection = dispinterface
    ['{ED042E4F-6CDE-11D1-BC1E-00A024C67143}']
  end;

  IRoseAssociation = dispinterface
    ['{C78E7026-86E4-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Roles: IDispatch dispid 412;
    property Role1: IRoseRole dispid 413;
    property Role2: IRoseRole dispid 414;
    property LinkClass: IRoseClass dispid 415;
    property Derived: WordBool dispid 419;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetCorrespondingRole(const Class_: IRoseClass): IRoseRole; dispid 416;
    function GetOtherRole(const Class_: IRoseClass): IRoseRole; dispid 417;
    procedure SetLinkClassName(const theClassName: WideString); dispid 418;
    function GetQualifiedName: WideString; dispid 12555;
    function NameIsDirectional: WordBool; dispid 12646;
    function GetRoleForNameDirection: IRoseRole; dispid 12647;
    procedure SetRoleForNameDirection(const theRole: IRoseRole); dispid 12648;
    procedure ClearRoleForNameDirection; dispid 12649;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseEventCollection = dispinterface
    ['{97B38361-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseEvent; dispid 203;
    function Exists(const pObject: IRoseEvent): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseEvent): Smallint; dispid 207;
    procedure Add(const theObject: IRoseEvent); dispid 208;
    procedure AddCollection(const theCollection: IRoseEventCollection); dispid 209;
    procedure Remove(const theObject: IRoseEvent); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseEvent; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseEvent; dispid 213;
  end;

  IRoseStateCollection = dispinterface
    ['{97B38367-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseState; dispid 203;
    function Exists(const pObject: IRoseState): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseState): Smallint; dispid 207;
    procedure Add(const theObject: IRoseState); dispid 208;
    procedure AddCollection(const theCollection: IRoseStateCollection); dispid 209;
    procedure Remove(const theObject: IRoseState); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseState; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseState; dispid 213;
  end;

  IRoseStateView = dispinterface
    ['{7BD909E1-9AF9-11D0-A214-00A024FFFE40}']
    property Name: WideString dispid 100;
    property YPosition: Smallint dispid 202;
    property XPosition: Smallint dispid 203;
    property Height: Smallint dispid 204;
    property Width: Smallint dispid 205;
    property FillColor: IRoseView_FillColor dispid 206;
    property LineColor: IRoseView_LineColor dispid 208;
    property SubViews: IRoseItemViewCollection dispid 219;
    property ParentView: IRoseItemView dispid 220;
    property Item: IRoseItem dispid 221;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Font: IRoseView_Font dispid 12493;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function SupportsFillColor: WordBool; dispid 210;
    function SupportsLineColor: WordBool; dispid 211;
    function IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function PointInView(x, y: Smallint): WordBool; dispid 214;
    function GetDefaultWidth: Smallint; dispid 215;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetMinHeight: Smallint; dispid 218;
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function GetState: IRoseState; dispid 415;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseSubsystemViewCollection = dispinterface
    ['{CA3AD902-BFCE-11D0-89F5-0020AFD6C181}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseSubsystemView; dispid 203;
    function Exists(const pObject: IRoseSubsystemView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseSubsystemView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseSubsystemView); dispid 208;
    procedure AddCollection(const theCollection: IRoseSubsystemViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseSubsystemView); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseSubsystemView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseSubsystemView; dispid 213;
  end;

  IRoseModuleDiagram = dispinterface
    ['{3FD9D004-93B0-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Visible: WordBool dispid 203;
    property Items: IRoseItemCollection dispid 208;
    property ParentSubsystem: IRoseSubsystem dispid 411;
    property ComponentViews: IRoseComponentViewCollection dispid 422;
    property SubsystemViews: IRoseSubsystemViewCollection dispid 423;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property Documentation: WideString dispid 12656;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Invalidate; dispid 205;
    procedure Update; dispid 206;
    function GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function IsActive: WordBool; dispid 209;
    function Exists(const theItem: IRoseItem): WordBool; dispid 210;
    procedure Activate; dispid 211;
    procedure Render(const FileName: WideString); dispid 217;
    function AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function GetNoteViews: IRoseNoteViewCollection; dispid 220;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function GetModules: IRoseModuleCollection; dispid 418;
    function GetSubsystems: IRoseSubsystemCollection; dispid 419;
    function GetSelectedModules: IRoseModuleCollection; dispid 420;
    function GetSelectedSubsystems: IRoseSubsystemCollection; dispid 421;
    function AddComponentView(const aModule: IRoseModule): IRoseComponentView; dispid 424;
    function RemoveComponentView(const aComponentView: IRoseComponentView): WordBool; dispid 425;
    function AddSubsystemView(const aSubsystem: IRoseSubsystem): IRoseSubsystemView; dispid 426;
    function RemoveSubsystemView(aSubsystemView: IDispatch): WordBool; dispid 427;
    function GetSelectedItems: IRoseItemCollection; dispid 12525;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseSubsystem = dispinterface
    ['{C78E702C-86E4-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Modules: IRoseModuleCollection dispid 412;
    property Subsystems: IRoseSubsystemCollection dispid 413;
    property ParentSubsystem: IRoseSubsystem dispid 414;
    property ModuleDiagrams: IRoseModuleDiagramCollection dispid 415;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function AddModule(const theName: WideString): IRoseModule; dispid 416;
    function AddModuleDiagram(const Name: WideString): IRoseModuleDiagram; dispid 418;
    function AddSubsystem(const theName: WideString): IRoseSubsystem; dispid 419;
    procedure RelocateModule(const theModule: IRoseModule); dispid 420;
    procedure RelocateSubsystem(const theSubsystem: IRoseSubsystem); dispid 421;
    procedure RelocateModuleDiagram(const theModDiagram: IRoseModuleDiagram); dispid 422;
    function GetAllModules: IRoseModuleCollection; dispid 423;
    function GetAllSubsystems: IRoseSubsystemCollection; dispid 424;
    function GetAssignedCategories: IRoseCategoryCollection; dispid 425;
    function GetAssignedClasses: IRoseClassCollection; dispid 426;
    function GetVisibleSubsystems: IRoseSubsystemCollection; dispid 427;
    function GetSubsystemDependencies(const theSubsystem: IRoseSubsystem): IRoseModuleVisibilityRelationshipCollection; dispid 428;
    function TopLevel: WordBool; dispid 429;
    function AddVisibilityRelationship(const theModule: IRoseModule): IRoseModuleVisibilityRelationship; dispid 430;
    function DeleteVisibilityRelationship(const theVisibilityRelationship: IRoseModuleVisibilityRelationship): WordBool; dispid 433;
    function AddSubsystemVisibilityRelation(theSubsystem: IDispatch): IDispatch; dispid 434;
    function DeleteModule(const pIDispatch: IRoseModule): WordBool; dispid 449;
    function DeleteSubSystem(const pIDispatch: IRoseSubsystem): WordBool; dispid 450;
    function IsRootPackage: WordBool; dispid 621;
    function IsControlled: WordBool; dispid 12433;
    function Control(const Path: WideString): WordBool; dispid 12434;
    function IsLoaded: WordBool; dispid 12435;
    function Load: WordBool; dispid 12436;
    function IsModifiable: WordBool; dispid 12438;
    function Unload: WordBool; dispid 12439;
    function Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function GetFileName: WideString; dispid 12441;
    function Save: WordBool; dispid 12442;
    function SaveAs(const Path: WideString): WordBool; dispid 12443;
    function GetQualifiedName: WideString; dispid 12555;
    function IsModified: WordBool; dispid 12654;
    function Uncontrol: WordBool; dispid 12655;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseExternalDocumentCollection = dispinterface
    ['{97B38357-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseExternalDocument; dispid 203;
    function Exists(const pObject: IRoseExternalDocument): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseExternalDocument): Smallint; dispid 207;
    procedure Add(const theObject: IRoseExternalDocument); dispid 208;
    procedure AddCollection(const theCollection: IRoseExternalDocumentCollection); dispid 209;
    procedure Remove(const theObject: IRoseExternalDocument); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseExternalDocument; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseExternalDocument; dispid 213;
  end;

  IRoseInstanceViewCollection = dispinterface
    ['{C640C864-F2D3-11D0-883A-3C8B00C10000}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseInstanceView; dispid 203;
    function Exists(const pObject: IRoseInstanceView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseInstanceView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseInstanceView); dispid 208;
    procedure AddCollection(const theCollection: IRoseInstanceViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseInstanceView); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseInstanceView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseInstanceView; dispid 213;
  end;

  IRoseItemView = dispinterface
    ['{7DFAFE40-A29D-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property YPosition: Smallint dispid 202;
    property XPosition: Smallint dispid 203;
    property Height: Smallint dispid 204;
    property Width: Smallint dispid 205;
    property FillColor: IRoseView_FillColor dispid 206;
    property LineColor: IRoseView_LineColor dispid 208;
    property SubViews: IRoseItemViewCollection dispid 219;
    property ParentView: IRoseItemView dispid 220;
    property Item: IRoseItem dispid 221;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Font: IRoseView_Font dispid 12493;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function SupportsFillColor: WordBool; dispid 210;
    function SupportsLineColor: WordBool; dispid 211;
    function IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function PointInView(x, y: Smallint): WordBool; dispid 214;
    function GetDefaultWidth: Smallint; dispid 215;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetMinHeight: Smallint; dispid 218;
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseTransitionCollection = dispinterface
    ['{97B3836B-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseTransition; dispid 203;
    function Exists(const pObject: IRoseTransition): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseTransition): Smallint; dispid 207;
    procedure Add(const theObject: IRoseTransition); dispid 208;
    procedure AddCollection(const theCollection: IRoseTransitionCollection); dispid 209;
    procedure Remove(const theObject: IRoseTransition); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseTransition; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseTransition; dispid 213;
  end;

  IRoseState = dispinterface
    ['{A69CAB23-9179-11D0-A214-00A024FFFE40}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property ParentState: IRoseState dispid 412;
    property ParentStateMachine: IRoseStateMachine dispid 413;
    property History: WordBool dispid 421;
    property Transitions: IRoseTransitionCollection dispid 422;
    property StateKind: IDispatch dispid 423;
    property SubStates: IRoseStateCollection dispid 444;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetAllSubStates: IRoseStateCollection; dispid 425;
    function AddTransition(const OnEvent: WideString; const Target: IRoseState): IRoseTransition; dispid 426;
    function DeleteTransition(const Transition: IRoseTransition): WordBool; dispid 427;
    function AddState(const Name: WideString): IRoseState; dispid 428;
    function DeleteState(const State: IRoseState): WordBool; dispid 429;
    function RelocateState(const State: IRoseState): WordBool; dispid 430;
    function DeleteAction(const theAction: IRoseAction): WordBool; dispid 446;
    function GetQualifiedName: WideString; dispid 12555;
    function GetUserDefinedEvents: IRoseEventCollection; dispid 12623;
    function GetEntryActions: IRoseActionCollection; dispid 12625;
    function GetExitActions: IRoseActionCollection; dispid 12626;
    function GetDoActions: IRoseActionCollection; dispid 12627;
    function AddUserDefinedEvent(const EventName, ActionName: WideString): IRoseEvent; dispid 12635;
    function DeleteUserDefinedEvent(const theEvent: IRoseEvent): WordBool; dispid 12636;
    function AddEntryAction(const ActionName: WideString): IRoseAction; dispid 12637;
    function AddExitAction(const ActionName: WideString): IRoseAction; dispid 12638;
    function AddDoAction(const ActionName: WideString): IRoseAction; dispid 12639;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseObjectInstanceCollection = dispinterface
    ['{97B3835A-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseObjectInstance; dispid 203;
    function Exists(const pObject: IRoseObjectInstance): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseObjectInstance): Smallint; dispid 207;
    procedure Add(const theObject: IRoseObjectInstance); dispid 208;
    procedure AddCollection(const theCollection: IRoseObjectInstanceCollection); dispid 209;
    procedure Remove(const theObject: IRoseObjectInstance); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseObjectInstance; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseObjectInstance; dispid 213;
  end;

  IRoseRoleCollection = dispinterface
    ['{97B38353-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRoseRole; dispid 203;
    function Exists(const pObject: IRoseRole): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseRole): Smallint; dispid 207;
    procedure Add(const theObject: IRoseRole); dispid 208;
    procedure AddCollection(const theCollection: IRoseRoleCollection); dispid 209;
    procedure Remove(const theObject: IRoseRole); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRoseRole; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseRole; dispid 213;
  end;

  IRoseClassRelation = dispinterface
    ['{00C99564-9200-11CF-B1B0-D227D5210B2C}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property SupplierName: WideString dispid 412;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function GetContextClass: IRoseClass; dispid 12600;
    function GetSupplierClass: IRoseClass; dispid 12601;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function GetClient: IRoseItem; dispid 12608;
    function GetSupplier: IRoseItem; dispid 12609;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseCategory = dispinterface
    ['{D7BC1B45-8618-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Global: WordBool dispid 412;
    property Classes: IRoseClassCollection dispid 413;
    property Categories: IRoseCategoryCollection dispid 414;
    property ParentCategory: IRoseCategory dispid 415;
    property ClassDiagrams: IRoseClassDiagramCollection dispid 416;
    property Associations: IRoseAssociationCollection dispid 417;
    property ScenarioDiagrams: IRoseScenarioDiagramCollection dispid 418;
    property UseCases: IRoseUseCaseCollection dispid 446;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetAllClasses: IRoseClassCollection; dispid 419;
    function GetAllCategories: IRoseCategoryCollection; dispid 420;
    function AddClass(const theName: WideString): IRoseClass; dispid 421;
    function AddClassDiagram(const Name: WideString): IRoseClassDiagram; dispid 422;
    function AddCategory(const theName: WideString): IRoseCategory; dispid 423;
    procedure RelocateClass(const theClass: IRoseClass); dispid 424;
    procedure RelocateCategory(const theCategory: IRoseCategory); dispid 425;
    procedure RelocateClassDiagram(const theClsDiagram: IRoseClassDiagram); dispid 426;
    function DeleteCategory(const theCategory: IRoseCategory): WordBool; dispid 427;
    function DeleteClass(const theClass: IRoseClass): WordBool; dispid 428;
    function DeleteClassDiagram(const theClassDiagram: IRoseClassDiagram): WordBool; dispid 429;
    function AddScenarioDiagram(const Name: WideString; Type_: Smallint): IRoseScenarioDiagram; dispid 430;
    function DeleteScenarioDiagram(const theScenarioDiagram: IRoseScenarioDiagram): WordBool; dispid 431;
    procedure RelocateScenarioDiagram(const theScenarioDiagram: IRoseScenarioDiagram); dispid 432;
    function GetAssignedSubsystem: IRoseSubsystem; dispid 433;
    procedure SetAssignedSubsystem(const newValue: IRoseSubsystem); dispid 434;
    function HasAssignedSubsystem: WordBool; dispid 435;
    function DeleteUseCase(const theUseCase: IRoseUseCase): WordBool; dispid 436;
    function TopLevel: WordBool; dispid 437;
    function GetAllUseCases: IRoseUseCaseCollection; dispid 447;
    function AddUseCase(const szName: WideString): IRoseUseCase; dispid 448;
    function IsRootPackage: WordBool; dispid 621;
    function IsControlled: WordBool; dispid 12433;
    function Control(const Path: WideString): WordBool; dispid 12434;
    function IsLoaded: WordBool; dispid 12435;
    function Load: WordBool; dispid 12436;
    function IsModifiable: WordBool; dispid 12438;
    function Unload: WordBool; dispid 12439;
    function Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function GetFileName: WideString; dispid 12441;
    function Save: WordBool; dispid 12442;
    function SaveAs(const Path: WideString): WordBool; dispid 12443;
    function GetQualifiedName: WideString; dispid 12555;
    function IsModified: WordBool; dispid 12654;
    function Uncontrol: WordBool; dispid 12655;
    function AddCategoryDependency(const theName, theSupplierCategoryName: WideString): IRoseCategoryDependency; dispid 12659;
    function GetCategoryDependencies: IRoseCategoryDependencyCollection; dispid 12660;
    function DeleteCategoryDependency(const theDependency: IRoseCategoryDependency): WordBool; dispid 12661;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseDefaultModelProperties = dispinterface
    ['{76ACC49D-FA18-11D0-BC11-00A024C67143}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName, thePropName, theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName, thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName, thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName, thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName, thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName, thePropName: WideString): WordBool; dispid 125;
    function CreateProperty(const theToolName, thePropName, theValue, theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName, SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function AddDefaultProperty(const ClassName, ToolName, SetName, PropName, PropType, Value: WideString): WordBool; dispid 440;
    function CloneDefaultPropertySet(const ClassName, ToolName, ExistingSetName, NewSetName: WideString): WordBool; dispid 441;
    function CreateDefaultPropertySet(const ClassName, ToolName, NewSetName: WideString): WordBool; dispid 442;
    function DeleteDefaultPropertySet(const ClassName, ToolName, SetName: WideString): WordBool; dispid 443;
    function GetDefaultPropertySet(const ClassName, ToolName, SetName: WideString): IRosePropertyCollection; dispid 444;
    function FindDefaultProperty(const ClassName, ToolName, SetName, PropName: WideString): IRoseProperty; dispid 445;
    function GetDefaultSetNames(const ClassName, ToolName: WideString): IRoseStringCollection; dispid 447;
    function DeleteDefaultProperty(const ClassName, ToolName, SetName, PropName: WideString): WordBool; dispid 449;
    function IsControlled: WordBool; dispid 12433;
    function Control(const Path: WideString): WordBool; dispid 12434;
    function IsLoaded: WordBool; dispid 12435;
    function Load: WordBool; dispid 12436;
    function IsModifiable: WordBool; dispid 12438;
    function Unload: WordBool; dispid 12439;
    function Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function GetFileName: WideString; dispid 12441;
    function Save: WordBool; dispid 12442;
    function SaveAs(const Path: WideString): WordBool; dispid 12443;
    function GetQualifiedName: WideString; dispid 12555;
    function IsToolVisible(const theToolName: WideString): WordBool; dispid 12593;
    procedure SetToolVisibility(const theToolName: WideString; Visibility: WordBool); dispid 12594;
    function IsModified: WordBool; dispid 12654;
    function Uncontrol: WordBool; dispid 12655;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  IRoseRealizeRelationCollection = dispinterface
    ['{67448181-4553-11D1-883B-3C8B00C10000}']
  end;

  IRosePackageCollection = dispinterface
    ['{97B38364-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function GetAt(Index: Smallint): IRosePackage; dispid 203;
    function Exists(const pObject: IRosePackage): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRosePackage): Smallint; dispid 207;
    procedure Add(const theObject: IRosePackage); dispid 208;
    procedure AddCollection(const theCollection: IRosePackageCollection); dispid 209;
    procedure Remove(const theObject: IRosePackage); dispid 210;
    procedure RemoveAll; dispid 211;
    function GetFirst(const Name: WideString): IRosePackage; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRosePackage; dispid 213;
  end;

  IRoseExternalDocument = dispinterface
    ['{906FF583-276B-11D0-8980-00A024774419}']
    property Path: WideString dispid 1;
    property URL: WideString dispid 2;
    property ParentCategory: IRoseCategory dispid 3;
    function IsURL: WordBool; dispid 4;
    function Open(const szAppPath: WideString): WordBool; dispid 5;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

  CoRoseProcessorCollection = class
    class function Create: IRoseProcessorCollection;
    class function CreateRemote(const MachineName: string): IRoseProcessorCollection;
  end;

  CoRoseCategoryCollection = class
    class function Create: IRoseCategoryCollection;
    class function CreateRemote(const MachineName: string): IRoseCategoryCollection;
  end;

  CoRoseItem = class
    class function Create: IRoseItem;
    class function CreateRemote(const MachineName: string): IRoseItem;
  end;

  CoRoseAddIn = class
    class function Create: IRoseAddIn;
    class function CreateRemote(const MachineName: string): IRoseAddIn;
  end;

  CoRoseUseCaseCollection = class
    class function Create: IRoseUseCaseCollection;
    class function CreateRemote(const MachineName: string): IRoseUseCaseCollection;
  end;

  CoRoseRelation = class
    class function Create: IRoseRelation;
    class function CreateRemote(const MachineName: string): IRoseRelation;
  end;

  CoRoseApplication = class
    class function Create: IRoseApplication;
    class function CreateRemote(const MachineName: string): IRoseApplication;
  end;

  CoRoseMessageCollection = class
    class function Create: IRoseMessageCollection;
    class function CreateRemote(const MachineName: string): IRoseMessageCollection;
  end;

  CoRoseClassDiagramCollection = class
    class function Create: IRoseClassDiagramCollection;
    class function CreateRemote(const MachineName: string): IRoseClassDiagramCollection;
  end;

  CoRoseScenarioDiagram = class
    class function Create: IRoseScenarioDiagram;
    class function CreateRemote(const MachineName: string): IRoseScenarioDiagram;
  end;

  CoRoseRealizeRelation = class
    class function Create: IRoseRealizeRelation;
    class function CreateRemote(const MachineName: string): IRoseRealizeRelation;
  end;

  CoRoseHasRelationship = class
    class function Create: IRoseHasRelationship;
    class function CreateRemote(const MachineName: string): IRoseHasRelationship;
  end;

  CoRoseClassView = class
    class function Create: IRoseClassView;
    class function CreateRemote(const MachineName: string): IRoseClassView;
  end;

  CoRoseView_FillColor = class
    class function Create: IRoseView_FillColor;
    class function CreateRemote(const MachineName: string): IRoseView_FillColor;
  end;

  CoRoseActionCollection = class
    class function Create: IRoseActionCollection;
    class function CreateRemote(const MachineName: string): IRoseActionCollection;
  end;

  CoRoseProcess = class
    class function Create: IRoseProcess;
    class function CreateRemote(const MachineName: string): IRoseProcess;
  end;

  CoRoseAddInCollection = class
    class function Create: IRoseAddInCollection;
    class function CreateRemote(const MachineName: string): IRoseAddInCollection;
  end;

  CoRoseControllableUnitCollection = class
    class function Create: IRoseControllableUnitCollection;
    class function CreateRemote(const MachineName: string): IRoseControllableUnitCollection;
  end;

  CoRoseModuleCollection = class
    class function Create: IRoseModuleCollection;
    class function CreateRemote(const MachineName: string): IRoseModuleCollection;
  end;

  CoRoseLinkCollection = class
    class function Create: IRoseLinkCollection;
    class function CreateRemote(const MachineName: string): IRoseLinkCollection;
  end;

  CoRoseAction = class
    class function Create: IRoseAction;
    class function CreateRemote(const MachineName: string): IRoseAction;
  end;

  CoRoseParameterCollection = class
    class function Create: IRoseParameterCollection;
    class function CreateRemote(const MachineName: string): IRoseParameterCollection;
  end;

  CoRoseAttributeCollection = class
    class function Create: IRoseAttributeCollection;
    class function CreateRemote(const MachineName: string): IRoseAttributeCollection;
  end;

  CoRoseDevice = class
    class function Create: IRoseDevice;
    class function CreateRemote(const MachineName: string): IRoseDevice;
  end;

  CoRoseClassDependency = class
    class function Create: IRoseClassDependency;
    class function CreateRemote(const MachineName: string): IRoseClassDependency;
  end;

  CoRoseRole = class
    class function Create: IRoseRole;
    class function CreateRemote(const MachineName: string): IRoseRole;
  end;

  CoRoseClass = class
    class function Create: IRoseClass;
    class function CreateRemote(const MachineName: string): IRoseClass;
  end;

  CoRoseElement = class
    class function Create: IRoseElement;
    class function CreateRemote(const MachineName: string): IRoseElement;
  end;

  CoRoseControllableUnit = class
    class function Create: IRoseControllableUnit;
    class function CreateRemote(const MachineName: string): IRoseControllableUnit;
  end;

  CoRoseModel = class
    class function Create: IRoseModel;
    class function CreateRemote(const MachineName: string): IRoseModel;
  end;

  CoRoseTransition = class
    class function Create: IRoseTransition;
    class function CreateRemote(const MachineName: string): IRoseTransition;
  end;

  CoRoseSubsystemCollection = class
    class function Create: IRoseSubsystemCollection;
    class function CreateRemote(const MachineName: string): IRoseSubsystemCollection;
  end;

  CoRoseProcessor = class
    class function Create: IRoseProcessor;
    class function CreateRemote(const MachineName: string): IRoseProcessor;
  end;

  CoRoseCategoryDependencyCollection = class
    class function Create: IRoseCategoryDependencyCollection;
    class function CreateRemote(const MachineName: string): IRoseCategoryDependencyCollection;
  end;

  CoRoseProperty = class
    class function Create: IRoseProperty;
    class function CreateRemote(const MachineName: string): IRoseProperty;
  end;

  CoRoseStateDiagram = class
    class function Create: IRoseStateDiagram;
    class function CreateRemote(const MachineName: string): IRoseStateDiagram;
  end;

  CoRoseEvent = class
    class function Create: IRoseEvent;
    class function CreateRemote(const MachineName: string): IRoseEvent;
  end;

  CoRoseRichType = class
    class function Create: IRoseRichType;
    class function CreateRemote(const MachineName: string): IRoseRichType;
  end;

  CoRoseScenarioDiagramCollection = class
    class function Create: IRoseScenarioDiagramCollection;
    class function CreateRemote(const MachineName: string): IRoseScenarioDiagramCollection;
  end;

  CoRoseParameter = class
    class function Create: IRoseParameter;
    class function CreateRemote(const MachineName: string): IRoseParameter;
  end;

  CoRoseOperation = class
    class function Create: IRoseOperation;
    class function CreateRemote(const MachineName: string): IRoseOperation;
  end;

  CoRoseView_LineColor = class
    class function Create: IRoseView_LineColor;
    class function CreateRemote(const MachineName: string): IRoseView_LineColor;
  end;

  CoRoseAddInManager = class
    class function Create: IRoseAddInManager;
    class function CreateRemote(const MachineName: string): IRoseAddInManager;
  end;

  CoRoseStateDiagramCollection = class
    class function Create: IRoseStateDiagramCollection;
    class function CreateRemote(const MachineName: string): IRoseStateDiagramCollection;
  end;

  CoRoseItemViewCollection = class
    class function Create: IRoseItemViewCollection;
    class function CreateRemote(const MachineName: string): IRoseItemViewCollection;
  end;

  CoRosePropertyCollection = class
    class function Create: IRosePropertyCollection;
    class function CreateRemote(const MachineName: string): IRosePropertyCollection;
  end;

  CoRoseOperationCollection = class
    class function Create: IRoseOperationCollection;
    class function CreateRemote(const MachineName: string): IRoseOperationCollection;
  end;

  CoRoseDeviceCollection = class
    class function Create: IRoseDeviceCollection;
    class function CreateRemote(const MachineName: string): IRoseDeviceCollection;
  end;

  CoRoseObject = class
    class function Create: IRoseObject;
    class function CreateRemote(const MachineName: string): IRoseObject;
  end;

  CoRoseModuleVisibilityRelationship = class
    class function Create: IRoseModuleVisibilityRelationship;
    class function CreateRemote(const MachineName: string): IRoseModuleVisibilityRelationship;
  end;

  CoRoseComponentViewCollection = class
    class function Create: IRoseComponentViewCollection;
    class function CreateRemote(const MachineName: string): IRoseComponentViewCollection;
  end;

  CoRoseHasRelationshipCollection = class
    class function Create: IRoseHasRelationshipCollection;
    class function CreateRemote(const MachineName: string): IRoseHasRelationshipCollection;
  end;

  CoRoseClassViewCollection = class
    class function Create: IRoseClassViewCollection;
    class function CreateRemote(const MachineName: string): IRoseClassViewCollection;
  end;

  CoRoseDeploymentDiagram = class
    class function Create: IRoseDeploymentDiagram;
    class function CreateRemote(const MachineName: string): IRoseDeploymentDiagram;
  end;

  CoRoseInstanceView = class
    class function Create: IRoseInstanceView;
    class function CreateRemote(const MachineName: string): IRoseInstanceView;
  end;

  CoRoseLink = class
    class function Create: IRoseLink;
    class function CreateRemote(const MachineName: string): IRoseLink;
  end;

  CoRoseObjectInstance = class
    class function Create: IRoseObjectInstance;
    class function CreateRemote(const MachineName: string): IRoseObjectInstance;
  end;

  CoRoseCategoryDependency = class
    class function Create: IRoseCategoryDependency;
    class function CreateRemote(const MachineName: string): IRoseCategoryDependency;
  end;

  CoRoseInheritRelation = class
    class function Create: IRoseInheritRelation;
    class function CreateRemote(const MachineName: string): IRoseInheritRelation;
  end;

  CoRoseView_Font = class
    class function Create: IRoseView_Font;
    class function CreateRemote(const MachineName: string): IRoseView_Font;
  end;

  CoRoseStateMachine = class
    class function Create: IRoseStateMachine;
    class function CreateRemote(const MachineName: string): IRoseStateMachine;
  end;

  CoRoseModule = class
    class function Create: IRoseModule;
    class function CreateRemote(const MachineName: string): IRoseModule;
  end;

  CoRoseUseCase = class
    class function Create: IRoseUseCase;
    class function CreateRemote(const MachineName: string): IRoseUseCase;
  end;

  CoRoseItemCollection = class
    class function Create: IRoseItemCollection;
    class function CreateRemote(const MachineName: string): IRoseItemCollection;
  end;

  CoRoseNoteViewCollection = class
    class function Create: IRoseNoteViewCollection;
    class function CreateRemote(const MachineName: string): IRoseNoteViewCollection;
  end;

  CoRoseInheritRelationCollection = class
    class function Create: IRoseInheritRelationCollection;
    class function CreateRemote(const MachineName: string): IRoseInheritRelationCollection;
  end;

  CoRoseDeploymentDiagramCollection = class
    class function Create: IRoseDeploymentDiagramCollection;
    class function CreateRemote(const MachineName: string): IRoseDeploymentDiagramCollection;
  end;

  CoRoseStringCollection = class
    class function Create: IRoseStringCollection;
    class function CreateRemote(const MachineName: string): IRoseStringCollection;
  end;

  CoRoseStateViewCollection = class
    class function Create: IRoseStateViewCollection;
    class function CreateRemote(const MachineName: string): IRoseStateViewCollection;
  end;

  CoRoseProcessCollection = class
    class function Create: IRoseProcessCollection;
    class function CreateRemote(const MachineName: string): IRoseProcessCollection;
  end;

  CoRoseAssociationCollection = class
    class function Create: IRoseAssociationCollection;
    class function CreateRemote(const MachineName: string): IRoseAssociationCollection;
  end;

  CoRoseModuleDiagramCollection = class
    class function Create: IRoseModuleDiagramCollection;
    class function CreateRemote(const MachineName: string): IRoseModuleDiagramCollection;
  end;

  CoRoseDiagram = class
    class function Create: IRoseDiagram;
    class function CreateRemote(const MachineName: string): IRoseDiagram;
  end;

  CoRoseRichTypeValuesCollection = class
    class function Create: IRoseRichTypeValuesCollection;
    class function CreateRemote(const MachineName: string): IRoseRichTypeValuesCollection;
  end;

  CoRoseSubsystemView = class
    class function Create: IRoseSubsystemView;
    class function CreateRemote(const MachineName: string): IRoseSubsystemView;
  end;

  CoRoseComponentView = class
    class function Create: IRoseComponentView;
    class function CreateRemote(const MachineName: string): IRoseComponentView;
  end;

  CoRoseAttribute = class
    class function Create: IRoseAttribute;
    class function CreateRemote(const MachineName: string): IRoseAttribute;
  end;

  CoRoseClassDiagram = class
    class function Create: IRoseClassDiagram;
    class function CreateRemote(const MachineName: string): IRoseClassDiagram;
  end;

  CoRoseNoteView = class
    class function Create: IRoseNoteView;
    class function CreateRemote(const MachineName: string): IRoseNoteView;
  end;

  CoRosePackage = class
    class function Create: IRosePackage;
    class function CreateRemote(const MachineName: string): IRosePackage;
  end;

  CoRosePathMap = class
    class function Create: IRosePathMap;
    class function CreateRemote(const MachineName: string): IRosePathMap;
  end;

  CoRoseModuleVisibilityRelationshipCollection = class
    class function Create: IRoseModuleVisibilityRelationshipCollection;
    class function CreateRemote(const MachineName: string): IRoseModuleVisibilityRelationshipCollection;
  end;

  CoRoseClassCollection = class
    class function Create: IRoseClassCollection;
    class function CreateRemote(const MachineName: string): IRoseClassCollection;
  end;

  CoRoseMessage = class
    class function Create: IRoseMessage;
    class function CreateRemote(const MachineName: string): IRoseMessage;
  end;

  CoRoseClassDependencyCollection = class
    class function Create: IRoseClassDependencyCollection;
    class function CreateRemote(const MachineName: string): IRoseClassDependencyCollection;
  end;

  CoRoseAssociation = class
    class function Create: IRoseAssociation;
    class function CreateRemote(const MachineName: string): IRoseAssociation;
  end;

  CoRoseEventCollection = class
    class function Create: IRoseEventCollection;
    class function CreateRemote(const MachineName: string): IRoseEventCollection;
  end;

  CoRoseStateCollection = class
    class function Create: IRoseStateCollection;
    class function CreateRemote(const MachineName: string): IRoseStateCollection;
  end;

  CoRoseStateView = class
    class function Create: IRoseStateView;
    class function CreateRemote(const MachineName: string): IRoseStateView;
  end;

  CoRoseSubsystemViewCollection = class
    class function Create: IRoseSubsystemViewCollection;
    class function CreateRemote(const MachineName: string): IRoseSubsystemViewCollection;
  end;

  CoRoseModuleDiagram = class
    class function Create: IRoseModuleDiagram;
    class function CreateRemote(const MachineName: string): IRoseModuleDiagram;
  end;

  CoRoseSubsystem = class
    class function Create: IRoseSubsystem;
    class function CreateRemote(const MachineName: string): IRoseSubsystem;
  end;

  CoRoseExternalDocumentCollection = class
    class function Create: IRoseExternalDocumentCollection;
    class function CreateRemote(const MachineName: string): IRoseExternalDocumentCollection;
  end;

  CoRoseInstanceViewCollection = class
    class function Create: IRoseInstanceViewCollection;
    class function CreateRemote(const MachineName: string): IRoseInstanceViewCollection;
  end;

  CoRoseItemView = class
    class function Create: IRoseItemView;
    class function CreateRemote(const MachineName: string): IRoseItemView;
  end;

  CoRoseTransitionCollection = class
    class function Create: IRoseTransitionCollection;
    class function CreateRemote(const MachineName: string): IRoseTransitionCollection;
  end;

  CoRoseState = class
    class function Create: IRoseState;
    class function CreateRemote(const MachineName: string): IRoseState;
  end;

  CoRoseObjectInstanceCollection = class
    class function Create: IRoseObjectInstanceCollection;
    class function CreateRemote(const MachineName: string): IRoseObjectInstanceCollection;
  end;

  CoRoseRoleCollection = class
    class function Create: IRoseRoleCollection;
    class function CreateRemote(const MachineName: string): IRoseRoleCollection;
  end;

  CoRoseClassRelation = class
    class function Create: IRoseClassRelation;
    class function CreateRemote(const MachineName: string): IRoseClassRelation;
  end;

  CoRoseCategory = class
    class function Create: IRoseCategory;
    class function CreateRemote(const MachineName: string): IRoseCategory;
  end;

  CoRoseDefaultModelProperties = class
    class function Create: IRoseDefaultModelProperties;
    class function CreateRemote(const MachineName: string): IRoseDefaultModelProperties;
  end;

  CoRoseRealizeRelationCollection = class
    class function Create: IRoseRealizeRelationCollection;
    class function CreateRemote(const MachineName: string): IRoseRealizeRelationCollection;
  end;

  CoRosePackageCollection = class
    class function Create: IRosePackageCollection;
    class function CreateRemote(const MachineName: string): IRosePackageCollection;
  end;

  CoRoseExternalDocument = class
    class function Create: IRoseExternalDocument;
    class function CreateRemote(const MachineName: string): IRoseExternalDocument;
  end;



implementation

uses ComObj;

class function CoRoseProcessorCollection.Create: IRoseProcessorCollection;
begin
  Result := CreateComObject(Class_RoseProcessorCollection) as IRoseProcessorCollection;
end;

class function CoRoseProcessorCollection.CreateRemote(const MachineName: string): IRoseProcessorCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseProcessorCollection) as IRoseProcessorCollection;
end;

class function CoRoseCategoryCollection.Create: IRoseCategoryCollection;
begin
  Result := CreateComObject(Class_RoseCategoryCollection) as IRoseCategoryCollection;
end;

class function CoRoseCategoryCollection.CreateRemote(const MachineName: string): IRoseCategoryCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseCategoryCollection) as IRoseCategoryCollection;
end;

class function CoRoseItem.Create: IRoseItem;
begin
  Result := CreateComObject(Class_RoseItem) as IRoseItem;
end;

class function CoRoseItem.CreateRemote(const MachineName: string): IRoseItem;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseItem) as IRoseItem;
end;

class function CoRoseAddIn.Create: IRoseAddIn;
begin
  Result := CreateComObject(Class_RoseAddIn) as IRoseAddIn;
end;

class function CoRoseAddIn.CreateRemote(const MachineName: string): IRoseAddIn;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseAddIn) as IRoseAddIn;
end;

class function CoRoseUseCaseCollection.Create: IRoseUseCaseCollection;
begin
  Result := CreateComObject(Class_RoseUseCaseCollection) as IRoseUseCaseCollection;
end;

class function CoRoseUseCaseCollection.CreateRemote(const MachineName: string): IRoseUseCaseCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseUseCaseCollection) as IRoseUseCaseCollection;
end;

class function CoRoseRelation.Create: IRoseRelation;
begin
  Result := CreateComObject(Class_RoseRelation) as IRoseRelation;
end;

class function CoRoseRelation.CreateRemote(const MachineName: string): IRoseRelation;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseRelation) as IRoseRelation;
end;

class function CoRoseApplication.Create: IRoseApplication;
begin
  Result := CreateComObject(Class_RoseApplication) as IRoseApplication;
end;

class function CoRoseApplication.CreateRemote(const MachineName: string): IRoseApplication;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseApplication) as IRoseApplication;
end;

class function CoRoseMessageCollection.Create: IRoseMessageCollection;
begin
  Result := CreateComObject(Class_RoseMessageCollection) as IRoseMessageCollection;
end;

class function CoRoseMessageCollection.CreateRemote(const MachineName: string): IRoseMessageCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseMessageCollection) as IRoseMessageCollection;
end;

class function CoRoseClassDiagramCollection.Create: IRoseClassDiagramCollection;
begin
  Result := CreateComObject(Class_RoseClassDiagramCollection) as IRoseClassDiagramCollection;
end;

class function CoRoseClassDiagramCollection.CreateRemote(const MachineName: string): IRoseClassDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseClassDiagramCollection) as IRoseClassDiagramCollection;
end;

class function CoRoseScenarioDiagram.Create: IRoseScenarioDiagram;
begin
  Result := CreateComObject(Class_RoseScenarioDiagram) as IRoseScenarioDiagram;
end;

class function CoRoseScenarioDiagram.CreateRemote(const MachineName: string): IRoseScenarioDiagram;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseScenarioDiagram) as IRoseScenarioDiagram;
end;

class function CoRoseRealizeRelation.Create: IRoseRealizeRelation;
begin
  Result := CreateComObject(Class_RoseRealizeRelation) as IRoseRealizeRelation;
end;

class function CoRoseRealizeRelation.CreateRemote(const MachineName: string): IRoseRealizeRelation;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseRealizeRelation) as IRoseRealizeRelation;
end;

class function CoRoseHasRelationship.Create: IRoseHasRelationship;
begin
  Result := CreateComObject(Class_RoseHasRelationship) as IRoseHasRelationship;
end;

class function CoRoseHasRelationship.CreateRemote(const MachineName: string): IRoseHasRelationship;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseHasRelationship) as IRoseHasRelationship;
end;

class function CoRoseClassView.Create: IRoseClassView;
begin
  Result := CreateComObject(Class_RoseClassView) as IRoseClassView;
end;

class function CoRoseClassView.CreateRemote(const MachineName: string): IRoseClassView;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseClassView) as IRoseClassView;
end;

class function CoRoseView_FillColor.Create: IRoseView_FillColor;
begin
  Result := CreateComObject(Class_RoseView_FillColor) as IRoseView_FillColor;
end;

class function CoRoseView_FillColor.CreateRemote(const MachineName: string): IRoseView_FillColor;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseView_FillColor) as IRoseView_FillColor;
end;

class function CoRoseActionCollection.Create: IRoseActionCollection;
begin
  Result := CreateComObject(Class_RoseActionCollection) as IRoseActionCollection;
end;

class function CoRoseActionCollection.CreateRemote(const MachineName: string): IRoseActionCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseActionCollection) as IRoseActionCollection;
end;

class function CoRoseProcess.Create: IRoseProcess;
begin
  Result := CreateComObject(Class_RoseProcess) as IRoseProcess;
end;

class function CoRoseProcess.CreateRemote(const MachineName: string): IRoseProcess;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseProcess) as IRoseProcess;
end;

class function CoRoseAddInCollection.Create: IRoseAddInCollection;
begin
  Result := CreateComObject(Class_RoseAddInCollection) as IRoseAddInCollection;
end;

class function CoRoseAddInCollection.CreateRemote(const MachineName: string): IRoseAddInCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseAddInCollection) as IRoseAddInCollection;
end;

class function CoRoseControllableUnitCollection.Create: IRoseControllableUnitCollection;
begin
  Result := CreateComObject(Class_RoseControllableUnitCollection) as IRoseControllableUnitCollection;
end;

class function CoRoseControllableUnitCollection.CreateRemote(const MachineName: string): IRoseControllableUnitCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseControllableUnitCollection) as IRoseControllableUnitCollection;
end;

class function CoRoseModuleCollection.Create: IRoseModuleCollection;
begin
  Result := CreateComObject(Class_RoseModuleCollection) as IRoseModuleCollection;
end;

class function CoRoseModuleCollection.CreateRemote(const MachineName: string): IRoseModuleCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseModuleCollection) as IRoseModuleCollection;
end;

class function CoRoseLinkCollection.Create: IRoseLinkCollection;
begin
  Result := CreateComObject(Class_RoseLinkCollection) as IRoseLinkCollection;
end;

class function CoRoseLinkCollection.CreateRemote(const MachineName: string): IRoseLinkCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseLinkCollection) as IRoseLinkCollection;
end;

class function CoRoseAction.Create: IRoseAction;
begin
  Result := CreateComObject(Class_RoseAction) as IRoseAction;
end;

class function CoRoseAction.CreateRemote(const MachineName: string): IRoseAction;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseAction) as IRoseAction;
end;

class function CoRoseParameterCollection.Create: IRoseParameterCollection;
begin
  Result := CreateComObject(Class_RoseParameterCollection) as IRoseParameterCollection;
end;

class function CoRoseParameterCollection.CreateRemote(const MachineName: string): IRoseParameterCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseParameterCollection) as IRoseParameterCollection;
end;

class function CoRoseAttributeCollection.Create: IRoseAttributeCollection;
begin
  Result := CreateComObject(Class_RoseAttributeCollection) as IRoseAttributeCollection;
end;

class function CoRoseAttributeCollection.CreateRemote(const MachineName: string): IRoseAttributeCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseAttributeCollection) as IRoseAttributeCollection;
end;

class function CoRoseDevice.Create: IRoseDevice;
begin
  Result := CreateComObject(Class_RoseDevice) as IRoseDevice;
end;

class function CoRoseDevice.CreateRemote(const MachineName: string): IRoseDevice;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseDevice) as IRoseDevice;
end;

class function CoRoseClassDependency.Create: IRoseClassDependency;
begin
  Result := CreateComObject(Class_RoseClassDependency) as IRoseClassDependency;
end;

class function CoRoseClassDependency.CreateRemote(const MachineName: string): IRoseClassDependency;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseClassDependency) as IRoseClassDependency;
end;

class function CoRoseRole.Create: IRoseRole;
begin
  Result := CreateComObject(Class_RoseRole) as IRoseRole;
end;

class function CoRoseRole.CreateRemote(const MachineName: string): IRoseRole;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseRole) as IRoseRole;
end;

class function CoRoseClass.Create: IRoseClass;
begin
  Result := CreateComObject(Class_RoseClass) as IRoseClass;
end;

class function CoRoseClass.CreateRemote(const MachineName: string): IRoseClass;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseClass) as IRoseClass;
end;

class function CoRoseElement.Create: IRoseElement;
begin
  Result := CreateComObject(Class_RoseElement) as IRoseElement;
end;

class function CoRoseElement.CreateRemote(const MachineName: string): IRoseElement;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseElement) as IRoseElement;
end;

class function CoRoseControllableUnit.Create: IRoseControllableUnit;
begin
  Result := CreateComObject(Class_RoseControllableUnit) as IRoseControllableUnit;
end;

class function CoRoseControllableUnit.CreateRemote(const MachineName: string): IRoseControllableUnit;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseControllableUnit) as IRoseControllableUnit;
end;

class function CoRoseModel.Create: IRoseModel;
begin
  Result := CreateComObject(Class_RoseModel) as IRoseModel;
end;

class function CoRoseModel.CreateRemote(const MachineName: string): IRoseModel;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseModel) as IRoseModel;
end;

class function CoRoseTransition.Create: IRoseTransition;
begin
  Result := CreateComObject(Class_RoseTransition) as IRoseTransition;
end;

class function CoRoseTransition.CreateRemote(const MachineName: string): IRoseTransition;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseTransition) as IRoseTransition;
end;

class function CoRoseSubsystemCollection.Create: IRoseSubsystemCollection;
begin
  Result := CreateComObject(Class_RoseSubsystemCollection) as IRoseSubsystemCollection;
end;

class function CoRoseSubsystemCollection.CreateRemote(const MachineName: string): IRoseSubsystemCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseSubsystemCollection) as IRoseSubsystemCollection;
end;

class function CoRoseProcessor.Create: IRoseProcessor;
begin
  Result := CreateComObject(Class_RoseProcessor) as IRoseProcessor;
end;

class function CoRoseProcessor.CreateRemote(const MachineName: string): IRoseProcessor;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseProcessor) as IRoseProcessor;
end;

class function CoRoseCategoryDependencyCollection.Create: IRoseCategoryDependencyCollection;
begin
  Result := CreateComObject(Class_RoseCategoryDependencyCollection) as IRoseCategoryDependencyCollection;
end;

class function CoRoseCategoryDependencyCollection.CreateRemote(const MachineName: string): IRoseCategoryDependencyCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseCategoryDependencyCollection) as IRoseCategoryDependencyCollection;
end;

class function CoRoseProperty.Create: IRoseProperty;
begin
  Result := CreateComObject(Class_RoseProperty) as IRoseProperty;
end;

class function CoRoseProperty.CreateRemote(const MachineName: string): IRoseProperty;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseProperty) as IRoseProperty;
end;

class function CoRoseStateDiagram.Create: IRoseStateDiagram;
begin
  Result := CreateComObject(Class_RoseStateDiagram) as IRoseStateDiagram;
end;

class function CoRoseStateDiagram.CreateRemote(const MachineName: string): IRoseStateDiagram;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseStateDiagram) as IRoseStateDiagram;
end;

class function CoRoseEvent.Create: IRoseEvent;
begin
  Result := CreateComObject(Class_RoseEvent) as IRoseEvent;
end;

class function CoRoseEvent.CreateRemote(const MachineName: string): IRoseEvent;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseEvent) as IRoseEvent;
end;

class function CoRoseRichType.Create: IRoseRichType;
begin
  Result := CreateComObject(Class_RoseRichType) as IRoseRichType;
end;

class function CoRoseRichType.CreateRemote(const MachineName: string): IRoseRichType;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseRichType) as IRoseRichType;
end;

class function CoRoseScenarioDiagramCollection.Create: IRoseScenarioDiagramCollection;
begin
  Result := CreateComObject(Class_RoseScenarioDiagramCollection) as IRoseScenarioDiagramCollection;
end;

class function CoRoseScenarioDiagramCollection.CreateRemote(const MachineName: string): IRoseScenarioDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseScenarioDiagramCollection) as IRoseScenarioDiagramCollection;
end;

class function CoRoseParameter.Create: IRoseParameter;
begin
  Result := CreateComObject(Class_RoseParameter) as IRoseParameter;
end;

class function CoRoseParameter.CreateRemote(const MachineName: string): IRoseParameter;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseParameter) as IRoseParameter;
end;

class function CoRoseOperation.Create: IRoseOperation;
begin
  Result := CreateComObject(Class_RoseOperation) as IRoseOperation;
end;

class function CoRoseOperation.CreateRemote(const MachineName: string): IRoseOperation;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseOperation) as IRoseOperation;
end;

class function CoRoseView_LineColor.Create: IRoseView_LineColor;
begin
  Result := CreateComObject(Class_RoseView_LineColor) as IRoseView_LineColor;
end;

class function CoRoseView_LineColor.CreateRemote(const MachineName: string): IRoseView_LineColor;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseView_LineColor) as IRoseView_LineColor;
end;

class function CoRoseAddInManager.Create: IRoseAddInManager;
begin
  Result := CreateComObject(Class_RoseAddInManager) as IRoseAddInManager;
end;

class function CoRoseAddInManager.CreateRemote(const MachineName: string): IRoseAddInManager;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseAddInManager) as IRoseAddInManager;
end;

class function CoRoseStateDiagramCollection.Create: IRoseStateDiagramCollection;
begin
  Result := CreateComObject(Class_RoseStateDiagramCollection) as IRoseStateDiagramCollection;
end;

class function CoRoseStateDiagramCollection.CreateRemote(const MachineName: string): IRoseStateDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseStateDiagramCollection) as IRoseStateDiagramCollection;
end;

class function CoRoseItemViewCollection.Create: IRoseItemViewCollection;
begin
  Result := CreateComObject(Class_RoseItemViewCollection) as IRoseItemViewCollection;
end;

class function CoRoseItemViewCollection.CreateRemote(const MachineName: string): IRoseItemViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseItemViewCollection) as IRoseItemViewCollection;
end;

class function CoRosePropertyCollection.Create: IRosePropertyCollection;
begin
  Result := CreateComObject(Class_RosePropertyCollection) as IRosePropertyCollection;
end;

class function CoRosePropertyCollection.CreateRemote(const MachineName: string): IRosePropertyCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RosePropertyCollection) as IRosePropertyCollection;
end;

class function CoRoseOperationCollection.Create: IRoseOperationCollection;
begin
  Result := CreateComObject(Class_RoseOperationCollection) as IRoseOperationCollection;
end;

class function CoRoseOperationCollection.CreateRemote(const MachineName: string): IRoseOperationCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseOperationCollection) as IRoseOperationCollection;
end;

class function CoRoseDeviceCollection.Create: IRoseDeviceCollection;
begin
  Result := CreateComObject(Class_RoseDeviceCollection) as IRoseDeviceCollection;
end;

class function CoRoseDeviceCollection.CreateRemote(const MachineName: string): IRoseDeviceCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseDeviceCollection) as IRoseDeviceCollection;
end;

class function CoRoseObject.Create: IRoseObject;
begin
  Result := CreateComObject(Class_RoseObject) as IRoseObject;
end;

class function CoRoseObject.CreateRemote(const MachineName: string): IRoseObject;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseObject) as IRoseObject;
end;

class function CoRoseModuleVisibilityRelationship.Create: IRoseModuleVisibilityRelationship;
begin
  Result := CreateComObject(Class_RoseModuleVisibilityRelationship) as IRoseModuleVisibilityRelationship;
end;

class function CoRoseModuleVisibilityRelationship.CreateRemote(const MachineName: string): IRoseModuleVisibilityRelationship;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseModuleVisibilityRelationship) as IRoseModuleVisibilityRelationship;
end;

class function CoRoseComponentViewCollection.Create: IRoseComponentViewCollection;
begin
  Result := CreateComObject(Class_RoseComponentViewCollection) as IRoseComponentViewCollection;
end;

class function CoRoseComponentViewCollection.CreateRemote(const MachineName: string): IRoseComponentViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseComponentViewCollection) as IRoseComponentViewCollection;
end;

class function CoRoseHasRelationshipCollection.Create: IRoseHasRelationshipCollection;
begin
  Result := CreateComObject(Class_RoseHasRelationshipCollection) as IRoseHasRelationshipCollection;
end;

class function CoRoseHasRelationshipCollection.CreateRemote(const MachineName: string): IRoseHasRelationshipCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseHasRelationshipCollection) as IRoseHasRelationshipCollection;
end;

class function CoRoseClassViewCollection.Create: IRoseClassViewCollection;
begin
  Result := CreateComObject(Class_RoseClassViewCollection) as IRoseClassViewCollection;
end;

class function CoRoseClassViewCollection.CreateRemote(const MachineName: string): IRoseClassViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseClassViewCollection) as IRoseClassViewCollection;
end;

class function CoRoseDeploymentDiagram.Create: IRoseDeploymentDiagram;
begin
  Result := CreateComObject(Class_RoseDeploymentDiagram) as IRoseDeploymentDiagram;
end;

class function CoRoseDeploymentDiagram.CreateRemote(const MachineName: string): IRoseDeploymentDiagram;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseDeploymentDiagram) as IRoseDeploymentDiagram;
end;

class function CoRoseInstanceView.Create: IRoseInstanceView;
begin
  Result := CreateComObject(Class_RoseInstanceView) as IRoseInstanceView;
end;

class function CoRoseInstanceView.CreateRemote(const MachineName: string): IRoseInstanceView;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseInstanceView) as IRoseInstanceView;
end;

class function CoRoseLink.Create: IRoseLink;
begin
  Result := CreateComObject(Class_RoseLink) as IRoseLink;
end;

class function CoRoseLink.CreateRemote(const MachineName: string): IRoseLink;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseLink) as IRoseLink;
end;

class function CoRoseObjectInstance.Create: IRoseObjectInstance;
begin
  Result := CreateComObject(Class_RoseObjectInstance) as IRoseObjectInstance;
end;

class function CoRoseObjectInstance.CreateRemote(const MachineName: string): IRoseObjectInstance;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseObjectInstance) as IRoseObjectInstance;
end;

class function CoRoseCategoryDependency.Create: IRoseCategoryDependency;
begin
  Result := CreateComObject(Class_RoseCategoryDependency) as IRoseCategoryDependency;
end;

class function CoRoseCategoryDependency.CreateRemote(const MachineName: string): IRoseCategoryDependency;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseCategoryDependency) as IRoseCategoryDependency;
end;

class function CoRoseInheritRelation.Create: IRoseInheritRelation;
begin
  Result := CreateComObject(Class_RoseInheritRelation) as IRoseInheritRelation;
end;

class function CoRoseInheritRelation.CreateRemote(const MachineName: string): IRoseInheritRelation;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseInheritRelation) as IRoseInheritRelation;
end;

class function CoRoseView_Font.Create: IRoseView_Font;
begin
  Result := CreateComObject(Class_RoseView_Font) as IRoseView_Font;
end;

class function CoRoseView_Font.CreateRemote(const MachineName: string): IRoseView_Font;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseView_Font) as IRoseView_Font;
end;

class function CoRoseStateMachine.Create: IRoseStateMachine;
begin
  Result := CreateComObject(Class_RoseStateMachine) as IRoseStateMachine;
end;

class function CoRoseStateMachine.CreateRemote(const MachineName: string): IRoseStateMachine;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseStateMachine) as IRoseStateMachine;
end;

class function CoRoseModule.Create: IRoseModule;
begin
  Result := CreateComObject(Class_RoseModule) as IRoseModule;
end;

class function CoRoseModule.CreateRemote(const MachineName: string): IRoseModule;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseModule) as IRoseModule;
end;

class function CoRoseUseCase.Create: IRoseUseCase;
begin
  Result := CreateComObject(Class_RoseUseCase) as IRoseUseCase;
end;

class function CoRoseUseCase.CreateRemote(const MachineName: string): IRoseUseCase;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseUseCase) as IRoseUseCase;
end;

class function CoRoseItemCollection.Create: IRoseItemCollection;
begin
  Result := CreateComObject(Class_RoseItemCollection) as IRoseItemCollection;
end;

class function CoRoseItemCollection.CreateRemote(const MachineName: string): IRoseItemCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseItemCollection) as IRoseItemCollection;
end;

class function CoRoseNoteViewCollection.Create: IRoseNoteViewCollection;
begin
  Result := CreateComObject(Class_RoseNoteViewCollection) as IRoseNoteViewCollection;
end;

class function CoRoseNoteViewCollection.CreateRemote(const MachineName: string): IRoseNoteViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseNoteViewCollection) as IRoseNoteViewCollection;
end;

class function CoRoseInheritRelationCollection.Create: IRoseInheritRelationCollection;
begin
  Result := CreateComObject(Class_RoseInheritRelationCollection) as IRoseInheritRelationCollection;
end;

class function CoRoseInheritRelationCollection.CreateRemote(const MachineName: string): IRoseInheritRelationCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseInheritRelationCollection) as IRoseInheritRelationCollection;
end;

class function CoRoseDeploymentDiagramCollection.Create: IRoseDeploymentDiagramCollection;
begin
  Result := CreateComObject(Class_RoseDeploymentDiagramCollection) as IRoseDeploymentDiagramCollection;
end;

class function CoRoseDeploymentDiagramCollection.CreateRemote(const MachineName: string): IRoseDeploymentDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseDeploymentDiagramCollection) as IRoseDeploymentDiagramCollection;
end;

class function CoRoseStringCollection.Create: IRoseStringCollection;
begin
  Result := CreateComObject(Class_RoseStringCollection) as IRoseStringCollection;
end;

class function CoRoseStringCollection.CreateRemote(const MachineName: string): IRoseStringCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseStringCollection) as IRoseStringCollection;
end;

class function CoRoseStateViewCollection.Create: IRoseStateViewCollection;
begin
  Result := CreateComObject(Class_RoseStateViewCollection) as IRoseStateViewCollection;
end;

class function CoRoseStateViewCollection.CreateRemote(const MachineName: string): IRoseStateViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseStateViewCollection) as IRoseStateViewCollection;
end;

class function CoRoseProcessCollection.Create: IRoseProcessCollection;
begin
  Result := CreateComObject(Class_RoseProcessCollection) as IRoseProcessCollection;
end;

class function CoRoseProcessCollection.CreateRemote(const MachineName: string): IRoseProcessCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseProcessCollection) as IRoseProcessCollection;
end;

class function CoRoseAssociationCollection.Create: IRoseAssociationCollection;
begin
  Result := CreateComObject(Class_RoseAssociationCollection) as IRoseAssociationCollection;
end;

class function CoRoseAssociationCollection.CreateRemote(const MachineName: string): IRoseAssociationCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseAssociationCollection) as IRoseAssociationCollection;
end;

class function CoRoseModuleDiagramCollection.Create: IRoseModuleDiagramCollection;
begin
  Result := CreateComObject(Class_RoseModuleDiagramCollection) as IRoseModuleDiagramCollection;
end;

class function CoRoseModuleDiagramCollection.CreateRemote(const MachineName: string): IRoseModuleDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseModuleDiagramCollection) as IRoseModuleDiagramCollection;
end;

class function CoRoseDiagram.Create: IRoseDiagram;
begin
  Result := CreateComObject(Class_RoseDiagram) as IRoseDiagram;
end;

class function CoRoseDiagram.CreateRemote(const MachineName: string): IRoseDiagram;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseDiagram) as IRoseDiagram;
end;

class function CoRoseRichTypeValuesCollection.Create: IRoseRichTypeValuesCollection;
begin
  Result := CreateComObject(Class_RoseRichTypeValuesCollection) as IRoseRichTypeValuesCollection;
end;

class function CoRoseRichTypeValuesCollection.CreateRemote(const MachineName: string): IRoseRichTypeValuesCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseRichTypeValuesCollection) as IRoseRichTypeValuesCollection;
end;

class function CoRoseSubsystemView.Create: IRoseSubsystemView;
begin
  Result := CreateComObject(Class_RoseSubsystemView) as IRoseSubsystemView;
end;

class function CoRoseSubsystemView.CreateRemote(const MachineName: string): IRoseSubsystemView;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseSubsystemView) as IRoseSubsystemView;
end;

class function CoRoseComponentView.Create: IRoseComponentView;
begin
  Result := CreateComObject(Class_RoseComponentView) as IRoseComponentView;
end;

class function CoRoseComponentView.CreateRemote(const MachineName: string): IRoseComponentView;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseComponentView) as IRoseComponentView;
end;

class function CoRoseAttribute.Create: IRoseAttribute;
begin
  Result := CreateComObject(Class_RoseAttribute) as IRoseAttribute;
end;

class function CoRoseAttribute.CreateRemote(const MachineName: string): IRoseAttribute;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseAttribute) as IRoseAttribute;
end;

class function CoRoseClassDiagram.Create: IRoseClassDiagram;
begin
  Result := CreateComObject(Class_RoseClassDiagram) as IRoseClassDiagram;
end;

class function CoRoseClassDiagram.CreateRemote(const MachineName: string): IRoseClassDiagram;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseClassDiagram) as IRoseClassDiagram;
end;

class function CoRoseNoteView.Create: IRoseNoteView;
begin
  Result := CreateComObject(Class_RoseNoteView) as IRoseNoteView;
end;

class function CoRoseNoteView.CreateRemote(const MachineName: string): IRoseNoteView;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseNoteView) as IRoseNoteView;
end;

class function CoRosePackage.Create: IRosePackage;
begin
  Result := CreateComObject(Class_RosePackage) as IRosePackage;
end;

class function CoRosePackage.CreateRemote(const MachineName: string): IRosePackage;
begin
  Result := CreateRemoteComObject(MachineName, Class_RosePackage) as IRosePackage;
end;

class function CoRosePathMap.Create: IRosePathMap;
begin
  Result := CreateComObject(Class_RosePathMap) as IRosePathMap;
end;

class function CoRosePathMap.CreateRemote(const MachineName: string): IRosePathMap;
begin
  Result := CreateRemoteComObject(MachineName, Class_RosePathMap) as IRosePathMap;
end;

class function CoRoseModuleVisibilityRelationshipCollection.Create: IRoseModuleVisibilityRelationshipCollection;
begin
  Result := CreateComObject(Class_RoseModuleVisibilityRelationshipCollection) as IRoseModuleVisibilityRelationshipCollection;
end;

class function CoRoseModuleVisibilityRelationshipCollection.CreateRemote(const MachineName: string): IRoseModuleVisibilityRelationshipCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseModuleVisibilityRelationshipCollection) as IRoseModuleVisibilityRelationshipCollection;
end;

class function CoRoseClassCollection.Create: IRoseClassCollection;
begin
  Result := CreateComObject(Class_RoseClassCollection) as IRoseClassCollection;
end;

class function CoRoseClassCollection.CreateRemote(const MachineName: string): IRoseClassCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseClassCollection) as IRoseClassCollection;
end;

class function CoRoseMessage.Create: IRoseMessage;
begin
  Result := CreateComObject(Class_RoseMessage) as IRoseMessage;
end;

class function CoRoseMessage.CreateRemote(const MachineName: string): IRoseMessage;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseMessage) as IRoseMessage;
end;

class function CoRoseClassDependencyCollection.Create: IRoseClassDependencyCollection;
begin
  Result := CreateComObject(Class_RoseClassDependencyCollection) as IRoseClassDependencyCollection;
end;

class function CoRoseClassDependencyCollection.CreateRemote(const MachineName: string): IRoseClassDependencyCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseClassDependencyCollection) as IRoseClassDependencyCollection;
end;

class function CoRoseAssociation.Create: IRoseAssociation;
begin
  Result := CreateComObject(Class_RoseAssociation) as IRoseAssociation;
end;

class function CoRoseAssociation.CreateRemote(const MachineName: string): IRoseAssociation;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseAssociation) as IRoseAssociation;
end;

class function CoRoseEventCollection.Create: IRoseEventCollection;
begin
  Result := CreateComObject(Class_RoseEventCollection) as IRoseEventCollection;
end;

class function CoRoseEventCollection.CreateRemote(const MachineName: string): IRoseEventCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseEventCollection) as IRoseEventCollection;
end;

class function CoRoseStateCollection.Create: IRoseStateCollection;
begin
  Result := CreateComObject(Class_RoseStateCollection) as IRoseStateCollection;
end;

class function CoRoseStateCollection.CreateRemote(const MachineName: string): IRoseStateCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseStateCollection) as IRoseStateCollection;
end;

class function CoRoseStateView.Create: IRoseStateView;
begin
  Result := CreateComObject(Class_RoseStateView) as IRoseStateView;
end;

class function CoRoseStateView.CreateRemote(const MachineName: string): IRoseStateView;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseStateView) as IRoseStateView;
end;

class function CoRoseSubsystemViewCollection.Create: IRoseSubsystemViewCollection;
begin
  Result := CreateComObject(Class_RoseSubsystemViewCollection) as IRoseSubsystemViewCollection;
end;

class function CoRoseSubsystemViewCollection.CreateRemote(const MachineName: string): IRoseSubsystemViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseSubsystemViewCollection) as IRoseSubsystemViewCollection;
end;

class function CoRoseModuleDiagram.Create: IRoseModuleDiagram;
begin
  Result := CreateComObject(Class_RoseModuleDiagram) as IRoseModuleDiagram;
end;

class function CoRoseModuleDiagram.CreateRemote(const MachineName: string): IRoseModuleDiagram;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseModuleDiagram) as IRoseModuleDiagram;
end;

class function CoRoseSubsystem.Create: IRoseSubsystem;
begin
  Result := CreateComObject(Class_RoseSubsystem) as IRoseSubsystem;
end;

class function CoRoseSubsystem.CreateRemote(const MachineName: string): IRoseSubsystem;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseSubsystem) as IRoseSubsystem;
end;

class function CoRoseExternalDocumentCollection.Create: IRoseExternalDocumentCollection;
begin
  Result := CreateComObject(Class_RoseExternalDocumentCollection) as IRoseExternalDocumentCollection;
end;

class function CoRoseExternalDocumentCollection.CreateRemote(const MachineName: string): IRoseExternalDocumentCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseExternalDocumentCollection) as IRoseExternalDocumentCollection;
end;

class function CoRoseInstanceViewCollection.Create: IRoseInstanceViewCollection;
begin
  Result := CreateComObject(Class_RoseInstanceViewCollection) as IRoseInstanceViewCollection;
end;

class function CoRoseInstanceViewCollection.CreateRemote(const MachineName: string): IRoseInstanceViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseInstanceViewCollection) as IRoseInstanceViewCollection;
end;

class function CoRoseItemView.Create: IRoseItemView;
begin
  Result := CreateComObject(Class_RoseItemView) as IRoseItemView;
end;

class function CoRoseItemView.CreateRemote(const MachineName: string): IRoseItemView;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseItemView) as IRoseItemView;
end;

class function CoRoseTransitionCollection.Create: IRoseTransitionCollection;
begin
  Result := CreateComObject(Class_RoseTransitionCollection) as IRoseTransitionCollection;
end;

class function CoRoseTransitionCollection.CreateRemote(const MachineName: string): IRoseTransitionCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseTransitionCollection) as IRoseTransitionCollection;
end;

class function CoRoseState.Create: IRoseState;
begin
  Result := CreateComObject(Class_RoseState) as IRoseState;
end;

class function CoRoseState.CreateRemote(const MachineName: string): IRoseState;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseState) as IRoseState;
end;

class function CoRoseObjectInstanceCollection.Create: IRoseObjectInstanceCollection;
begin
  Result := CreateComObject(Class_RoseObjectInstanceCollection) as IRoseObjectInstanceCollection;
end;

class function CoRoseObjectInstanceCollection.CreateRemote(const MachineName: string): IRoseObjectInstanceCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseObjectInstanceCollection) as IRoseObjectInstanceCollection;
end;

class function CoRoseRoleCollection.Create: IRoseRoleCollection;
begin
  Result := CreateComObject(Class_RoseRoleCollection) as IRoseRoleCollection;
end;

class function CoRoseRoleCollection.CreateRemote(const MachineName: string): IRoseRoleCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseRoleCollection) as IRoseRoleCollection;
end;

class function CoRoseClassRelation.Create: IRoseClassRelation;
begin
  Result := CreateComObject(Class_RoseClassRelation) as IRoseClassRelation;
end;

class function CoRoseClassRelation.CreateRemote(const MachineName: string): IRoseClassRelation;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseClassRelation) as IRoseClassRelation;
end;

class function CoRoseCategory.Create: IRoseCategory;
begin
  Result := CreateComObject(Class_RoseCategory) as IRoseCategory;
end;

class function CoRoseCategory.CreateRemote(const MachineName: string): IRoseCategory;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseCategory) as IRoseCategory;
end;

class function CoRoseDefaultModelProperties.Create: IRoseDefaultModelProperties;
begin
  Result := CreateComObject(Class_RoseDefaultModelProperties) as IRoseDefaultModelProperties;
end;

class function CoRoseDefaultModelProperties.CreateRemote(const MachineName: string): IRoseDefaultModelProperties;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseDefaultModelProperties) as IRoseDefaultModelProperties;
end;

class function CoRoseRealizeRelationCollection.Create: IRoseRealizeRelationCollection;
begin
  Result := CreateComObject(Class_RoseRealizeRelationCollection) as IRoseRealizeRelationCollection;
end;

class function CoRoseRealizeRelationCollection.CreateRemote(const MachineName: string): IRoseRealizeRelationCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseRealizeRelationCollection) as IRoseRealizeRelationCollection;
end;

class function CoRosePackageCollection.Create: IRosePackageCollection;
begin
  Result := CreateComObject(Class_RosePackageCollection) as IRosePackageCollection;
end;

class function CoRosePackageCollection.CreateRemote(const MachineName: string): IRosePackageCollection;
begin
  Result := CreateRemoteComObject(MachineName, Class_RosePackageCollection) as IRosePackageCollection;
end;

class function CoRoseExternalDocument.Create: IRoseExternalDocument;
begin
  Result := CreateComObject(Class_RoseExternalDocument) as IRoseExternalDocument;
end;

class function CoRoseExternalDocument.CreateRemote(const MachineName: string): IRoseExternalDocument;
begin
  Result := CreateRemoteComObject(MachineName, Class_RoseExternalDocument) as IRoseExternalDocument;
end;


end.
