{ Global compiler directives }
{$include bold.inc}
unit RationalRose_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 18/10/2005 08:01:49 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files\Rational\Rose\RationalRose.tlb (1)
// LIBID: {860CC660-1C2B-11D0-B1B1-444553540000}
// LCID: 0
// Helpfile: 
// HelpString: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\STDOLE2.TLB)
// Parent TypeLibrary:
//   (0) v1.0 AttracsRoseAddinServer, (C:\Attracs\Utils\AttracsRoseAddin\AttracsRoseAddinServer.tlb)
// Errors:
//   Hint: Symbol 'ClassName' renamed to '_className'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'ClassName' renamed to '_className'
//   Hint: Member 'Class' of 'IRoseRole' changed to 'Class_'
//   Hint: Parameter 'Class' of IRoseAssociation.GetCorrespondingRole changed to 'Class_'
//   Hint: Parameter 'Class' of IRoseAssociation.GetOtherRole changed to 'Class_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Member 'Const' of 'IRoseParameter' changed to 'Const_'
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Parameter 'Type' of IRoseCategory.AddScenarioDiagram changed to 'Type_'
//   Hint: Symbol 'Type' renamed to 'type_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  RationalRoseMajorVersion = 4;
  RationalRoseMinorVersion = 2;

  LIBID_RationalRose: TGUID = '{860CC660-1C2B-11D0-B1B1-444553540000}';

  DIID_IRoseLineVertex: TGUID = '{B53888D2-3094-11D2-8153-00104B97EBD5}';
  CLASS_RoseLineVertex: TGUID = '{B53888D1-3094-11D2-8153-00104B97EBD5}';
  DIID_IRoseObject: TGUID = '{7D8474B2-2C33-11D0-BBDA-00A024C67143}';
  CLASS_RoseObject: TGUID = '{97B38389-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseStateMachineCollection: TGUID = '{97B38369-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseStateMachineCollection: TGUID = '{BA376EEB-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseStateViewCollection: TGUID = '{97B3836A-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseStateViewCollection: TGUID = '{BA376EEC-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseStateDiagramCollection: TGUID = '{97B38368-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseStateDiagramCollection: TGUID = '{BA376EEA-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseEventCollection: TGUID = '{97B38361-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseEventCollection: TGUID = '{BA376EE8-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseActionCollection: TGUID = '{97B3835F-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseActionCollection: TGUID = '{BA376EEF-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseTransitionCollection: TGUID = '{97B3836B-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseTransitionCollection: TGUID = '{BA376EEE-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseStateCollection: TGUID = '{97B38367-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseStateCollection: TGUID = '{BA376EE9-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseSwimLaneViewCollection: TGUID = '{7FFC5F46-C0C2-11D2-92AA-004005141253}';
  CLASS_RoseSwimLaneViewCollection: TGUID = '{7FFC5F48-C0C2-11D2-92AA-004005141253}';
  DIID_IRoseSwimLaneView: TGUID = '{68F63C21-B047-11D2-92AA-004005141253}';
  CLASS_RoseSwimLaneView: TGUID = '{68F63C23-B047-11D2-92AA-004005141253}';
  DIID_IRoseSyncItemViewCollection: TGUID = '{94CA1891-5D13-11D2-92AA-004005141253}';
  CLASS_RoseSyncItemViewCollection: TGUID = '{94CA1892-5D13-11D2-92AA-004005141253}';
  DIID_IRoseSyncItemView: TGUID = '{94CA1888-5D13-11D2-92AA-004005141253}';
  CLASS_RoseSyncItemView: TGUID = '{94CA1889-5D13-11D2-92AA-004005141253}';
  DIID_IRoseDecisionViewCollection: TGUID = '{BEAED601-578D-11D2-92AA-004005141253}';
  CLASS_RoseDecisionViewCollection: TGUID = '{BEAED602-578D-11D2-92AA-004005141253}';
  DIID_IRoseActivityViewCollection: TGUID = '{BEAED5FE-578D-11D2-92AA-004005141253}';
  CLASS_RoseActivityViewCollection: TGUID = '{BEAED5FF-578D-11D2-92AA-004005141253}';
  DIID_IRoseDecisionView: TGUID = '{BEAED5F9-578D-11D2-92AA-004005141253}';
  CLASS_RoseDecisionView: TGUID = '{BEAED5FA-578D-11D2-92AA-004005141253}';
  DIID_IRoseActivityView: TGUID = '{BEAED5FC-578D-11D2-92AA-004005141253}';
  CLASS_RoseActivityView: TGUID = '{BEAED5FD-578D-11D2-92AA-004005141253}';
  DIID_IRoseStateView: TGUID = '{7BD909E1-9AF9-11D0-A214-00A024FFFE40}';
  CLASS_RoseStateView: TGUID = '{4782FBB3-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseStateDiagram: TGUID = '{7ADDA701-9B06-11D0-A214-00A024FFFE40}';
  CLASS_RoseStateDiagram: TGUID = '{4782FBB1-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseSwimLaneCollection: TGUID = '{7FFC5F42-C0C2-11D2-92AA-004005141253}';
  CLASS_RoseSwimLaneCollection: TGUID = '{7FFC5F44-C0C2-11D2-92AA-004005141253}';
  DIID_IRoseSyncItemCollection: TGUID = '{94CA188F-5D13-11D2-92AA-004005141253}';
  CLASS_RoseSyncItemCollection: TGUID = '{94CA1890-5D13-11D2-92AA-004005141253}';
  DIID_IRoseSyncItem: TGUID = '{94CA188B-5D13-11D2-92AA-004005141253}';
  CLASS_RoseSyncItem: TGUID = '{94CA188D-5D13-11D2-92AA-004005141253}';
  DIID_IRoseStateMachineOwner: TGUID = '{94CA1882-5D13-11D2-92AA-004005141253}';
  CLASS_RoseStateMachineOwner: TGUID = '{94CA1883-5D13-11D2-92AA-004005141253}';
  DIID_IRoseStateVertexCollection: TGUID = '{BEAED5F7-578D-11D2-92AA-004005141253}';
  CLASS_RoseStateVertexCollection: TGUID = '{BB792029-57AA-11D2-92AA-004005141253}';
  DIID_IRoseDecisionCollection: TGUID = '{BEAED5F2-578D-11D2-92AA-004005141253}';
  CLASS_RoseDecisionCollection: TGUID = '{BEAED5F4-578D-11D2-92AA-004005141253}';
  DIID_IRoseActivityCollection: TGUID = '{BEAED5F0-578D-11D2-92AA-004005141253}';
  CLASS_RoseActivityCollection: TGUID = '{BEAED5F1-578D-11D2-92AA-004005141253}';
  DIID_IRoseAbstractStateCollection: TGUID = '{BEAED5EE-578D-11D2-92AA-004005141253}';
  CLASS_RoseAbstractStateCollection: TGUID = '{BB792023-57AA-11D2-92AA-004005141253}';
  DIID_IRoseDecision: TGUID = '{BEAED5E3-578D-11D2-92AA-004005141253}';
  CLASS_RoseDecision: TGUID = '{BEAED5E5-578D-11D2-92AA-004005141253}';
  DIID_IRoseActivity: TGUID = '{BEAED5E7-578D-11D2-92AA-004005141253}';
  CLASS_RoseActivity: TGUID = '{BEAED5E8-578D-11D2-92AA-004005141253}';
  DIID_IRoseAbstractState: TGUID = '{BEAED5EC-578D-11D2-92AA-004005141253}';
  CLASS_RoseAbstractState: TGUID = '{BB792027-57AA-11D2-92AA-004005141253}';
  DIID_IRoseStateVertex: TGUID = '{BEAED5E2-578D-11D2-92AA-004005141253}';
  CLASS_RoseStateVertex: TGUID = '{BB792021-57AA-11D2-92AA-004005141253}';
  DIID_IRoseSwimLane: TGUID = '{BEAED5EA-578D-11D2-92AA-004005141253}';
  CLASS_RoseSwimLane: TGUID = '{BB792025-57AA-11D2-92AA-004005141253}';
  DIID_IRoseStateMachine: TGUID = '{A69CAB21-9179-11D0-A214-00A024FFFE40}';
  CLASS_RoseStateMachine: TGUID = '{4782FBB2-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseEvent: TGUID = '{A69CAB22-9179-11D0-A214-00A024FFFE40}';
  CLASS_RoseEvent: TGUID = '{86652276-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseAction: TGUID = '{13881143-93C1-11D0-A214-00A024FFFE40}';
  CLASS_RoseAction: TGUID = '{86652270-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseTransition: TGUID = '{574130A1-93B8-11D0-A214-00A024FFFE40}';
  CLASS_RoseTransition: TGUID = '{4782FBB5-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseState: TGUID = '{A69CAB23-9179-11D0-A214-00A024FFFE40}';
  CLASS_RoseState: TGUID = '{4782FBB0-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseRichTypeValuesCollection: TGUID = '{BF8C1040-96DD-11CF-B091-00A0241E3F73}';
  CLASS_RoseRichTypeValuesCollection: TGUID = '{97B38390-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseRichType: TGUID = '{EB7AAB60-939C-11CF-B091-00A0241E3F73}';
  CLASS_RoseRichType: TGUID = '{97B38380-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseModuleVisibilityRelationship: TGUID = '{9EF8DDD6-E697-11CF-BBD1-00A024C67143}';
  CLASS_RoseModuleVisibilityRelationship: TGUID = '{8665227D-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseSubsystemViewCollection: TGUID = '{CA3AD902-BFCE-11D0-89F5-0020AFD6C181}';
  CLASS_RoseSubsystemViewCollection: TGUID = '{0CEEA5A1-C6F8-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseComponentViewCollection: TGUID = '{C640C861-F2D3-11D0-883A-3C8B00C10000}';
  CLASS_RoseComponentViewCollection: TGUID = '{9DE9A9C3-F2D0-11D0-883A-3C8B00C10000}';
  DIID_IRoseSubsystemView: TGUID = '{14028C92-C06C-11D0-89F5-0020AFD6C181}';
  CLASS_RoseSubsystemView: TGUID = '{4782FBB4-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseComponentView: TGUID = '{14028C94-C06C-11D0-89F5-0020AFD6C181}';
  CLASS_RoseComponentView: TGUID = '{86652273-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseModuleDiagram: TGUID = '{3FD9D004-93B0-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseModuleDiagram: TGUID = '{8665227B-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseModule: TGUID = '{C78E702A-86E4-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseModule: TGUID = '{4782FBA7-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseSubsystem: TGUID = '{C78E702C-86E4-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseSubsystem: TGUID = '{4782FBAC-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseUseCase: TGUID = '{7E7F6EE0-16DE-11D0-8976-00A024774419}';
  CLASS_RoseUseCase: TGUID = '{4782FBB6-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseDiagramCollection: TGUID = '{38E8FEC2-969A-11D3-92AA-004005141253}';
  CLASS_RoseDiagramCollection: TGUID = '{38E8FEC1-969A-11D3-92AA-004005141253}';
  DIID_IRoseLineVertexCollection: TGUID = '{11A235B2-3095-11D2-8153-00104B97EBD5}';
  CLASS_RoseLineVertexCollection: TGUID = '{11A235B4-3095-11D2-8153-00104B97EBD5}';
  DIID_IRoseInstantiateRelationCollection: TGUID = '{B91D8F06-DDBB-11D1-9FAD-0060975306FE}';
  CLASS_RoseInstantiateRelationCollection: TGUID = '{B91D8F05-DDBB-11D1-9FAD-0060975306FE}';
  DIID_IRoseRealizeRelationCollection: TGUID = '{67448181-4553-11D1-883B-3C8B00C10000}';
  CLASS_RoseRealizeRelationCollection: TGUID = '{67448182-4553-11D1-883B-3C8B00C10000}';
  DIID_IRoseItemCollection: TGUID = '{0DD9ACF8-D06E-11D0-BC0B-00A024C67143}';
  CLASS_RoseItemCollection: TGUID = '{0DD9ACF7-D06E-11D0-BC0B-00A024C67143}';
  DIID_IRoseControllableUnitCollection: TGUID = '{97B38360-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseControllableUnitCollection: TGUID = '{BA376ED2-A44E-11D0-BC02-00A024C67143}';
  DIID_IRosePackageCollection: TGUID = '{97B38364-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RosePackageCollection: TGUID = '{BA376EE4-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseNoteViewCollection: TGUID = '{97B38358-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseNoteViewCollection: TGUID = '{BA376EE0-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseExternalDocumentCollection: TGUID = '{97B38357-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseExternalDocumentCollection: TGUID = '{BA376EDF-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseUseCaseCollection: TGUID = '{97B38356-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseUseCaseCollection: TGUID = '{BA376EDE-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseProcessCollection: TGUID = '{97B38366-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseProcessCollection: TGUID = '{BA376EE6-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseModuleVisibilityRelationshipCollection: TGUID = '{97B38363-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseModuleVisibilityRelationshipCollection: TGUID = '{BA376EE2-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseItemViewCollection: TGUID = '{97B38362-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseItemViewCollection: TGUID = '{BA376EE1-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseScenarioDiagramCollection: TGUID = '{97B3835E-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseScenarioDiagramCollection: TGUID = '{BA376ED1-A44E-11D0-BC02-00A024C67143}';
  DIID_IRosePropertyCollection: TGUID = '{97B3835D-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RosePropertyCollection: TGUID = '{BA376ED0-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseProcessorCollection: TGUID = '{97B3835C-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseProcessorCollection: TGUID = '{BA376ECF-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseObjectInstanceCollection: TGUID = '{97B3835A-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseObjectInstanceCollection: TGUID = '{BA376ECE-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseMessageCollection: TGUID = '{97B38359-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseMessageCollection: TGUID = '{BA376ECD-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseInheritRelationCollection: TGUID = '{97B38354-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseInheritRelationCollection: TGUID = '{BA376EDC-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseRoleCollection: TGUID = '{97B38353-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseRoleCollection: TGUID = '{BA376EDB-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseParameterCollection: TGUID = '{97B38352-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseParameterCollection: TGUID = '{BA376EDA-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseHasRelationshipCollection: TGUID = '{97B38351-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseHasRelationshipCollection: TGUID = '{BA376ED9-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseAssociationCollection: TGUID = '{97B3834E-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseAssociationCollection: TGUID = '{BA376ED8-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseOperationCollection: TGUID = '{97B3834D-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseOperationCollection: TGUID = '{BA376ED7-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseAttributeCollection: TGUID = '{97B3834C-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseAttributeCollection: TGUID = '{BA376ED6-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseModuleCollection: TGUID = '{97B3834B-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseModuleCollection: TGUID = '{BA376ED5-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseSubsystemCollection: TGUID = '{97B3834A-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseSubsystemCollection: TGUID = '{BA376ED4-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseCategoryCollection: TGUID = '{97B3835B-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseCategoryCollection: TGUID = '{BA376EE3-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseClassCollection: TGUID = '{97B38349-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseClassCollection: TGUID = '{BA376ED3-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseModuleDiagramCollection: TGUID = '{97B38348-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseModuleDiagramCollection: TGUID = '{BA376ECC-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseClassDiagramCollection: TGUID = '{97B38343-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseClassDiagramCollection: TGUID = '{BA376ECB-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseDeviceCollection: TGUID = '{97B38342-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseDeviceCollection: TGUID = '{BA376ECA-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseDeploymentDiagramCollection: TGUID = '{97B383A1-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseDeploymentDiagramCollection: TGUID = '{BA376EC8-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseClassViewCollection: TGUID = '{97B38341-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseClassViewCollection: TGUID = '{BA376EC7-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseDeploymentDiagram: TGUID = '{C2C15EC4-E028-11CF-B091-00A0241E3F73}';
  CLASS_RoseDeploymentDiagram: TGUID = '{86652274-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseProcess: TGUID = '{62C43884-DB5A-11CF-B091-00A0241E3F73}';
  CLASS_RoseProcess: TGUID = '{4782FBAE-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseDevice: TGUID = '{62C43882-DB5A-11CF-B091-00A0241E3F73}';
  CLASS_RoseDevice: TGUID = '{86652275-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseProcessor: TGUID = '{62C43886-DB5A-11CF-B091-00A0241E3F73}';
  CLASS_RoseProcessor: TGUID = '{4782FBAD-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseInstanceViewCollection: TGUID = '{C640C864-F2D3-11D0-883A-3C8B00C10000}';
  CLASS_RoseInstanceViewCollection: TGUID = '{C640C862-F2D3-11D0-883A-3C8B00C10000}';
  DIID_IRoseInstanceView: TGUID = '{348B1AD4-D5C4-11D0-89F8-0020AFD6C181}';
  CLASS_RoseInstanceView: TGUID = '{86652279-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseLinkCollection: TGUID = '{9DE9A9C1-F2D0-11D0-883A-3C8B00C10000}';
  CLASS_RoseLinkCollection: TGUID = '{9DE9A9C2-F2D0-11D0-883A-3C8B00C10000}';
  DIID_IRoseLink: TGUID = '{195D7852-D5B6-11D0-89F8-0020AFD6C181}';
  CLASS_RoseLink: TGUID = '{8665227A-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseScenarioDiagram: TGUID = '{F819833A-FC55-11CF-BBD3-00A024C67143}';
  CLASS_RoseScenarioDiagram: TGUID = '{4782FBAF-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseMessage: TGUID = '{F819833C-FC55-11CF-BBD3-00A024C67143}';
  CLASS_RoseMessage: TGUID = '{8665227C-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseObjectInstance: TGUID = '{F8198337-FC55-11CF-BBD3-00A024C67143}';
  CLASS_RoseObjectInstance: TGUID = '{4782FBA1-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseConnectionRelationCollection: TGUID = '{4467F446-F24E-11D2-92AA-004005141253}';
  CLASS_RoseConnectionRelationCollection: TGUID = '{4467F448-F24E-11D2-92AA-004005141253}';
  DIID_IRoseConnectionRelation: TGUID = '{4467F442-F24E-11D2-92AA-004005141253}';
  CLASS_RoseConnectionRelation: TGUID = '{4467F444-F24E-11D2-92AA-004005141253}';
  DIID_IRoseInstantiateRelation: TGUID = '{B91D8F03-DDBB-11D1-9FAD-0060975306FE}';
  CLASS_RoseInstantiateRelation: TGUID = '{B91D8F04-DDBB-11D1-9FAD-0060975306FE}';
  DIID_IRoseClassDependencyCollection: TGUID = '{ED042E4F-6CDE-11D1-BC1E-00A024C67143}';
  CLASS_RoseClassDependencyCollection: TGUID = '{ED042E50-6CDE-11D1-BC1E-00A024C67143}';
  DIID_IRoseCategoryDependencyCollection: TGUID = '{4ACE189D-6CD3-11D1-BC1E-00A024C67143}';
  CLASS_RoseCategoryDependencyCollection: TGUID = '{4ACE189E-6CD3-11D1-BC1E-00A024C67143}';
  DIID_IRoseClassDependency: TGUID = '{4ACE1899-6CD3-11D1-BC1E-00A024C67143}';
  CLASS_RoseClassDependency: TGUID = '{4ACE189A-6CD3-11D1-BC1E-00A024C67143}';
  DIID_IRoseCategoryDependency: TGUID = '{4ACE189B-6CD3-11D1-BC1E-00A024C67143}';
  CLASS_RoseCategoryDependency: TGUID = '{4ACE189C-6CD3-11D1-BC1E-00A024C67143}';
  DIID_IRoseClassRelation: TGUID = '{00C99564-9200-11CF-B1B0-D227D5210B2C}';
  CLASS_RoseClassRelation: TGUID = '{97B3839E-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseRealizeRelation: TGUID = '{6AC2BA81-454D-11D1-883B-3C8B00C10000}';
  CLASS_RoseRealizeRelation: TGUID = '{6AC2BA82-454D-11D1-883B-3C8B00C10000}';
  DIID_IRoseInheritRelation: TGUID = '{00C99560-9200-11CF-B1B0-D227D5210B2C}';
  CLASS_RoseInheritRelation: TGUID = '{86652278-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseHasRelationship: TGUID = '{BA242E04-8961-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseHasRelationship: TGUID = '{4782FBA4-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseRole: TGUID = '{BA242E00-8961-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseRole: TGUID = '{97B38378-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseAssociation: TGUID = '{C78E7026-86E4-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseAssociation: TGUID = '{4782FBA2-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseParameter: TGUID = '{C78E7028-86E4-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseParameter: TGUID = '{4782FBAA-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseOperation: TGUID = '{C78E7020-86E4-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseOperation: TGUID = '{4782FBA8-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseAttribute: TGUID = '{C78E7024-86E4-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseAttribute: TGUID = '{4782FBA3-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseClassView: TGUID = '{5F735F36-F9EA-11CF-BBD3-00A024C67143}';
  CLASS_RoseClassView: TGUID = '{86652271-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseClassDiagram: TGUID = '{3FD9D002-93B0-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseClassDiagram: TGUID = '{86652272-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseClass: TGUID = '{BC57D1C0-863E-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseClass: TGUID = '{8665226F-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseCategory: TGUID = '{D7BC1B45-8618-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseCategory: TGUID = '{4782FBA9-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseView_Font: TGUID = '{CE5BE567-0380-11D1-BC11-00A024C67143}';
  CLASS_RoseView_Font: TGUID = '{CE5BE568-0380-11D1-BC11-00A024C67143}';
  DIID_IRoseView_LineColor: TGUID = '{CE5BE565-0380-11D1-BC11-00A024C67143}';
  CLASS_RoseView_LineColor: TGUID = '{CE5BE566-0380-11D1-BC11-00A024C67143}';
  DIID_IRoseView_FillColor: TGUID = '{CE5BE563-0380-11D1-BC11-00A024C67143}';
  CLASS_RoseView_FillColor: TGUID = '{CE5BE564-0380-11D1-BC11-00A024C67143}';
  DIID_IRoseNoteView: TGUID = '{015655CA-72DF-11D0-95EB-0000F803584A}';
  CLASS_RoseNoteView: TGUID = '{4782FBA0-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseItemView: TGUID = '{7DFAFE40-A29D-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseItemView: TGUID = '{97B3839B-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseDiagram: TGUID = '{3FD9D000-93B0-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseDiagram: TGUID = '{97B3838E-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseStringCollection: TGUID = '{6A7FC311-C893-11D0-BC0B-00A024C67143}';
  CLASS_RoseStringCollection: TGUID = '{4782FBB8-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseObjectFlowCollection: TGUID = '{882D2F8D-BD12-11D3-92AA-004005141253}';
  CLASS_RoseObjectFlowCollection: TGUID = '{882D2F8F-BD12-11D3-92AA-004005141253}';
  DIID_IRoseDependencyRelationCollection: TGUID = '{882D2F89-BD12-11D3-92AA-004005141253}';
  CLASS_RoseDependencyRelationCollection: TGUID = '{882D2F8B-BD12-11D3-92AA-004005141253}';
  DIID_IRoseObjectFlow: TGUID = '{882D2F81-BD12-11D3-92AA-004005141253}';
  CLASS_RoseObjectFlow: TGUID = '{882D2F83-BD12-11D3-92AA-004005141253}';
  DIID_IRoseDependencyRelation: TGUID = '{882D2F85-BD12-11D3-92AA-004005141253}';
  CLASS_RoseDependencyRelation: TGUID = '{882D2F87-BD12-11D3-92AA-004005141253}';
  DIID_IRoseDeploymentUnit: TGUID = '{4335FBE3-F0A0-11D1-9FAD-0060975306FE}';
  CLASS_RoseDeploymentUnit: TGUID = '{4335FBE2-F0A0-11D1-9FAD-0060975306FE}';
  DIID_IRoseRelation: TGUID = '{BA242E02-8961-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseRelation: TGUID = '{97B38370-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseDefaultModelProperties: TGUID = '{76ACC49D-FA18-11D0-BC11-00A024C67143}';
  CLASS_RoseDefaultModelProperties: TGUID = '{A51B4041-3E99-11D1-883B-3C8B00C10000}';
  DIID_IRoseProperty: TGUID = '{93461A23-8811-11CF-B1B0-D227D5210B2C}';
  CLASS_RoseProperty: TGUID = '{4782FBAB-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseElement: TGUID = '{D067F15F-6987-11D0-BBF0-00A024C67143}';
  CLASS_RoseElement: TGUID = '{97B38396-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRosePackage: TGUID = '{47D975C1-8A8D-11D0-A214-444553540000}';
  CLASS_RosePackage: TGUID = '{97B38392-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseControllableUnit: TGUID = '{32C862A7-8AA9-11D0-A70B-0000F803584A}';
  CLASS_RoseControllableUnit: TGUID = '{97B38379-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseExternalDocument: TGUID = '{906FF583-276B-11D0-8980-00A024774419}';
  CLASS_RoseExternalDocument: TGUID = '{86652277-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseItem: TGUID = '{BC57D1C2-863E-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseItem: TGUID = '{97B3836E-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseModel: TGUID = '{E38942A0-8621-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseModel: TGUID = '{4782FBA6-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseContextMenuItemCollection: TGUID = '{EE0B16E2-FF91-11D1-9FAD-0060975306FE}';
  CLASS_RoseContextMenuItemCollection: TGUID = '{EE0B16E4-FF91-11D1-9FAD-0060975306FE}';
  DIID_IRoseContextMenuItem: TGUID = '{EE0B16E0-FF91-11D1-9FAD-0060975306FE}';
  CLASS_RoseContextMenuItem: TGUID = '{EE0B16E1-FF91-11D1-9FAD-0060975306FE}';
  DIID_IRoseAddInCollection: TGUID = '{C87D2BC1-352A-11D1-883B-3C8B00C10000}';
  CLASS_RoseAddInCollection: TGUID = '{C87D2BC0-352A-11D1-883B-3C8B00C10000}';
  DIID_IRoseAddIn: TGUID = '{D5352FC0-346C-11D1-883B-3C8B00C10000}';
  CLASS_RoseAddIn: TGUID = '{D5352FC1-346C-11D1-883B-3C8B00C10000}';
  DIID_IRoseAddInManager: TGUID = '{D5352FC2-346C-11D1-883B-3C8B00C10000}';
  CLASS_RoseAddInManager: TGUID = '{D5352FC3-346C-11D1-883B-3C8B00C10000}';
  DIID_IRosePathMap: TGUID = '{4C9E2241-84C5-11D0-A214-444553540000}';
  CLASS_RosePathMap: TGUID = '{97B38395-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseApplication: TGUID = '{D7BC1B40-8618-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseApplication: TGUID = '{D7BC1B41-8618-11CF-B3D4-00A0241DB1D0}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum RoseFrequency
type
  RoseFrequency = TOleEnum;
const
  rsAperiodic = $00000000;
  rsPeriodic = $00000001;

// Constants for enum RoseSynchronization
type
  RoseSynchronization = TOleEnum;
const
  rsSimple = $00000000;
  rsSynchronous = $00000001;
  rsBalking = $00000002;
  rsTimeout = $00000003;
  rsAsynchronous = $00000004;

// Constants for enum RosePersistence
type
  RosePersistence = TOleEnum;
const
  rsPersistent = $00000000;
  rsTransient = $00000001;

// Constants for enum RoseStereotypeDisplay
type
  RoseStereotypeDisplay = TOleEnum;
const
  rsIconNone = $00000000;
  rsIconName = $00000001;
  rsIconDecoration = $00000002;
  rsIconFull = $00000003;

// Constants for enum RoseAddinEventTypes
type
  RoseAddinEventTypes = TOleEnum;
const
  rsOnNewModel = $00000002;

// Constants for enum RoseNotationTypes
type
  RoseNotationTypes = TOleEnum;
const
  BoochNotation = $00000000;
  OMTNotation = $00000001;
  UMLNotation = $00000002;

// Constants for enum RoseMenuState
type
  RoseMenuState = TOleEnum;
const
  rsDisabled = $00000000;
  rsEnabled = $00000001;
  rsDisabledAndChecked = $00000002;
  rsDisabledAndUnchecked = $00000003;
  rsEnabledAndChecked = $00000004;
  rsEnabledAndUnchecked = $00000005;

// Constants for enum RoseContextMenuItemType
type
  RoseContextMenuItemType = TOleEnum;
const
  rsDefault = $00000000;
  rsDiagram = $00000001;
  rsPackage = $00000002;
  rsUseCase = $00000003;
  rsClass = $00000004;
  rsAttribute = $00000005;
  rsOperation = $00000006;
  rsComponent = $00000007;
  rsRole = $00000008;
  rsProperties = $00000009;
  rsModel = $0000000A;
  rsDeploymentUnit = $0000000B;
  rsExternalDoc = $0000000C;
  rsActivity = $0000000D;
  rsState = $0000000E;
  rsTransition = $0000000F;
  rsSynchronization = $00000010;
  rsDecision = $00000011;
  rsSwimlane = $00000012;

// Constants for enum RoseClientRelType
type
  RoseClientRelType = TOleEnum;
const
  rsTypeAny = $00000000;
  rsTypeHas = $00000001;
  rsTypeInstantiation = $00000002;
  rsTypeInherits = $00000003;
  rsTypeAssociation = $00000004;
  rsTypeDependency = $00000005;
  rsTypeRealizes = $00000006;

// Constants for enum RoseClientRelKind
type
  RoseClientRelKind = TOleEnum;
const
  rsAnyKind = $00000000;
  rsFriend = $00000001;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IRoseLineVertex = dispinterface;
  IRoseObject = dispinterface;
  IRoseStateMachineCollection = dispinterface;
  IRoseStateViewCollection = dispinterface;
  IRoseStateDiagramCollection = dispinterface;
  IRoseEventCollection = dispinterface;
  IRoseActionCollection = dispinterface;
  IRoseTransitionCollection = dispinterface;
  IRoseStateCollection = dispinterface;
  IRoseSwimLaneViewCollection = dispinterface;
  IRoseSwimLaneView = dispinterface;
  IRoseSyncItemViewCollection = dispinterface;
  IRoseSyncItemView = dispinterface;
  IRoseDecisionViewCollection = dispinterface;
  IRoseActivityViewCollection = dispinterface;
  IRoseDecisionView = dispinterface;
  IRoseActivityView = dispinterface;
  IRoseStateView = dispinterface;
  IRoseStateDiagram = dispinterface;
  IRoseSwimLaneCollection = dispinterface;
  IRoseSyncItemCollection = dispinterface;
  IRoseSyncItem = dispinterface;
  IRoseStateMachineOwner = dispinterface;
  IRoseStateVertexCollection = dispinterface;
  IRoseDecisionCollection = dispinterface;
  IRoseActivityCollection = dispinterface;
  IRoseAbstractStateCollection = dispinterface;
  IRoseDecision = dispinterface;
  IRoseActivity = dispinterface;
  IRoseAbstractState = dispinterface;
  IRoseStateVertex = dispinterface;
  IRoseSwimLane = dispinterface;
  IRoseStateMachine = dispinterface;
  IRoseEvent = dispinterface;
  IRoseAction = dispinterface;
  IRoseTransition = dispinterface;
  IRoseState = dispinterface;
  IRoseRichTypeValuesCollection = dispinterface;
  IRoseRichType = dispinterface;
  IRoseModuleVisibilityRelationship = dispinterface;
  IRoseSubsystemViewCollection = dispinterface;
  IRoseComponentViewCollection = dispinterface;
  IRoseSubsystemView = dispinterface;
  IRoseComponentView = dispinterface;
  IRoseModuleDiagram = dispinterface;
  IRoseModule = dispinterface;
  IRoseSubsystem = dispinterface;
  IRoseUseCase = dispinterface;
  IRoseDiagramCollection = dispinterface;
  IRoseLineVertexCollection = dispinterface;
  IRoseInstantiateRelationCollection = dispinterface;
  IRoseRealizeRelationCollection = dispinterface;
  IRoseItemCollection = dispinterface;
  IRoseControllableUnitCollection = dispinterface;
  IRosePackageCollection = dispinterface;
  IRoseNoteViewCollection = dispinterface;
  IRoseExternalDocumentCollection = dispinterface;
  IRoseUseCaseCollection = dispinterface;
  IRoseProcessCollection = dispinterface;
  IRoseModuleVisibilityRelationshipCollection = dispinterface;
  IRoseItemViewCollection = dispinterface;
  IRoseScenarioDiagramCollection = dispinterface;
  IRosePropertyCollection = dispinterface;
  IRoseProcessorCollection = dispinterface;
  IRoseObjectInstanceCollection = dispinterface;
  IRoseMessageCollection = dispinterface;
  IRoseInheritRelationCollection = dispinterface;
  IRoseRoleCollection = dispinterface;
  IRoseParameterCollection = dispinterface;
  IRoseHasRelationshipCollection = dispinterface;
  IRoseAssociationCollection = dispinterface;
  IRoseOperationCollection = dispinterface;
  IRoseAttributeCollection = dispinterface;
  IRoseModuleCollection = dispinterface;
  IRoseSubsystemCollection = dispinterface;
  IRoseCategoryCollection = dispinterface;
  IRoseClassCollection = dispinterface;
  IRoseModuleDiagramCollection = dispinterface;
  IRoseClassDiagramCollection = dispinterface;
  IRoseDeviceCollection = dispinterface;
  IRoseDeploymentDiagramCollection = dispinterface;
  IRoseClassViewCollection = dispinterface;
  IRoseDeploymentDiagram = dispinterface;
  IRoseProcess = dispinterface;
  IRoseDevice = dispinterface;
  IRoseProcessor = dispinterface;
  IRoseInstanceViewCollection = dispinterface;
  IRoseInstanceView = dispinterface;
  IRoseLinkCollection = dispinterface;
  IRoseLink = dispinterface;
  IRoseScenarioDiagram = dispinterface;
  IRoseMessage = dispinterface;
  IRoseObjectInstance = dispinterface;
  IRoseConnectionRelationCollection = dispinterface;
  IRoseConnectionRelation = dispinterface;
  IRoseInstantiateRelation = dispinterface;
  IRoseClassDependencyCollection = dispinterface;
  IRoseCategoryDependencyCollection = dispinterface;
  IRoseClassDependency = dispinterface;
  IRoseCategoryDependency = dispinterface;
  IRoseClassRelation = dispinterface;
  IRoseRealizeRelation = dispinterface;
  IRoseInheritRelation = dispinterface;
  IRoseHasRelationship = dispinterface;
  IRoseRole = dispinterface;
  IRoseAssociation = dispinterface;
  IRoseParameter = dispinterface;
  IRoseOperation = dispinterface;
  IRoseAttribute = dispinterface;
  IRoseClassView = dispinterface;
  IRoseClassDiagram = dispinterface;
  IRoseClass = dispinterface;
  IRoseCategory = dispinterface;
  IRoseView_Font = dispinterface;
  IRoseView_LineColor = dispinterface;
  IRoseView_FillColor = dispinterface;
  IRoseNoteView = dispinterface;
  IRoseItemView = dispinterface;
  IRoseDiagram = dispinterface;
  IRoseStringCollection = dispinterface;
  IRoseObjectFlowCollection = dispinterface;
  IRoseDependencyRelationCollection = dispinterface;
  IRoseObjectFlow = dispinterface;
  IRoseDependencyRelation = dispinterface;
  IRoseDeploymentUnit = dispinterface;
  IRoseRelation = dispinterface;
  IRoseDefaultModelProperties = dispinterface;
  IRoseProperty = dispinterface;
  IRoseElement = dispinterface;
  IRosePackage = dispinterface;
  IRoseControllableUnit = dispinterface;
  IRoseExternalDocument = dispinterface;
  IRoseItem = dispinterface;
  IRoseModel = dispinterface;
  IRoseContextMenuItemCollection = dispinterface;
  IRoseContextMenuItem = dispinterface;
  IRoseAddInCollection = dispinterface;
  IRoseAddIn = dispinterface;
  IRoseAddInManager = dispinterface;
  IRosePathMap = dispinterface;
  IRoseApplication = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  RoseLineVertex = IRoseLineVertex;
  RoseObject = IRoseObject;
  RoseStateMachineCollection = IRoseStateMachineCollection;
  RoseStateViewCollection = IRoseStateViewCollection;
  RoseStateDiagramCollection = IRoseStateDiagramCollection;
  RoseEventCollection = IRoseEventCollection;
  RoseActionCollection = IRoseActionCollection;
  RoseTransitionCollection = IRoseTransitionCollection;
  RoseStateCollection = IRoseStateCollection;
  RoseSwimLaneViewCollection = IRoseSwimLaneViewCollection;
  RoseSwimLaneView = IRoseSwimLaneView;
  RoseSyncItemViewCollection = IRoseSyncItemViewCollection;
  RoseSyncItemView = IRoseSyncItemView;
  RoseDecisionViewCollection = IRoseDecisionViewCollection;
  RoseActivityViewCollection = IRoseActivityViewCollection;
  RoseDecisionView = IRoseDecisionView;
  RoseActivityView = IRoseActivityView;
  RoseStateView = IRoseStateView;
  RoseStateDiagram = IRoseStateDiagram;
  RoseSwimLaneCollection = IRoseSwimLaneCollection;
  RoseSyncItemCollection = IRoseSyncItemCollection;
  RoseSyncItem = IRoseSyncItem;
  RoseStateMachineOwner = IRoseStateMachineOwner;
  RoseStateVertexCollection = IRoseStateVertexCollection;
  RoseDecisionCollection = IRoseDecisionCollection;
  RoseActivityCollection = IRoseActivityCollection;
  RoseAbstractStateCollection = IRoseAbstractStateCollection;
  RoseDecision = IRoseDecision;
  RoseActivity = IRoseActivity;
  RoseAbstractState = IRoseAbstractState;
  RoseStateVertex = IRoseStateVertex;
  RoseSwimLane = IRoseSwimLane;
  RoseStateMachine = IRoseStateMachine;
  RoseEvent = IRoseEvent;
  RoseAction = IRoseAction;
  RoseTransition = IRoseTransition;
  RoseState = IRoseState;
  RoseRichTypeValuesCollection = IRoseRichTypeValuesCollection;
  RoseRichType = IRoseRichType;
  RoseModuleVisibilityRelationship = IRoseModuleVisibilityRelationship;
  RoseSubsystemViewCollection = IRoseSubsystemViewCollection;
  RoseComponentViewCollection = IRoseComponentViewCollection;
  RoseSubsystemView = IRoseSubsystemView;
  RoseComponentView = IRoseComponentView;
  RoseModuleDiagram = IRoseModuleDiagram;
  RoseModule = IRoseModule;
  RoseSubsystem = IRoseSubsystem;
  RoseUseCase = IRoseUseCase;
  RoseDiagramCollection = IRoseDiagramCollection;
  RoseLineVertexCollection = IRoseLineVertexCollection;
  RoseInstantiateRelationCollection = IRoseInstantiateRelationCollection;
  RoseRealizeRelationCollection = IRoseRealizeRelationCollection;
  RoseItemCollection = IRoseItemCollection;
  RoseControllableUnitCollection = IRoseControllableUnitCollection;
  RosePackageCollection = IRosePackageCollection;
  RoseNoteViewCollection = IRoseNoteViewCollection;
  RoseExternalDocumentCollection = IRoseExternalDocumentCollection;
  RoseUseCaseCollection = IRoseUseCaseCollection;
  RoseProcessCollection = IRoseProcessCollection;
  RoseModuleVisibilityRelationshipCollection = IRoseModuleVisibilityRelationshipCollection;
  RoseItemViewCollection = IRoseItemViewCollection;
  RoseScenarioDiagramCollection = IRoseScenarioDiagramCollection;
  RosePropertyCollection = IRosePropertyCollection;
  RoseProcessorCollection = IRoseProcessorCollection;
  RoseObjectInstanceCollection = IRoseObjectInstanceCollection;
  RoseMessageCollection = IRoseMessageCollection;
  RoseInheritRelationCollection = IRoseInheritRelationCollection;
  RoseRoleCollection = IRoseRoleCollection;
  RoseParameterCollection = IRoseParameterCollection;
  RoseHasRelationshipCollection = IRoseHasRelationshipCollection;
  RoseAssociationCollection = IRoseAssociationCollection;
  RoseOperationCollection = IRoseOperationCollection;
  RoseAttributeCollection = IRoseAttributeCollection;
  RoseModuleCollection = IRoseModuleCollection;
  RoseSubsystemCollection = IRoseSubsystemCollection;
  RoseCategoryCollection = IRoseCategoryCollection;
  RoseClassCollection = IRoseClassCollection;
  RoseModuleDiagramCollection = IRoseModuleDiagramCollection;
  RoseClassDiagramCollection = IRoseClassDiagramCollection;
  RoseDeviceCollection = IRoseDeviceCollection;
  RoseDeploymentDiagramCollection = IRoseDeploymentDiagramCollection;
  RoseClassViewCollection = IRoseClassViewCollection;
  RoseDeploymentDiagram = IRoseDeploymentDiagram;
  RoseProcess = IRoseProcess;
  RoseDevice = IRoseDevice;
  RoseProcessor = IRoseProcessor;
  RoseInstanceViewCollection = IRoseInstanceViewCollection;
  RoseInstanceView = IRoseInstanceView;
  RoseLinkCollection = IRoseLinkCollection;
  RoseLink = IRoseLink;
  RoseScenarioDiagram = IRoseScenarioDiagram;
  RoseMessage = IRoseMessage;
  RoseObjectInstance = IRoseObjectInstance;
  RoseConnectionRelationCollection = IRoseConnectionRelationCollection;
  RoseConnectionRelation = IRoseConnectionRelation;
  RoseInstantiateRelation = IRoseInstantiateRelation;
  RoseClassDependencyCollection = IRoseClassDependencyCollection;
  RoseCategoryDependencyCollection = IRoseCategoryDependencyCollection;
  RoseClassDependency = IRoseClassDependency;
  RoseCategoryDependency = IRoseCategoryDependency;
  RoseClassRelation = IRoseClassRelation;
  RoseRealizeRelation = IRoseRealizeRelation;
  RoseInheritRelation = IRoseInheritRelation;
  RoseHasRelationship = IRoseHasRelationship;
  RoseRole = IRoseRole;
  RoseAssociation = IRoseAssociation;
  RoseParameter = IRoseParameter;
  RoseOperation = IRoseOperation;
  RoseAttribute = IRoseAttribute;
  RoseClassView = IRoseClassView;
  RoseClassDiagram = IRoseClassDiagram;
  RoseClass = IRoseClass;
  RoseCategory = IRoseCategory;
  RoseView_Font = IRoseView_Font;
  RoseView_LineColor = IRoseView_LineColor;
  RoseView_FillColor = IRoseView_FillColor;
  RoseNoteView = IRoseNoteView;
  RoseItemView = IRoseItemView;
  RoseDiagram = IRoseDiagram;
  RoseStringCollection = IRoseStringCollection;
  RoseObjectFlowCollection = IRoseObjectFlowCollection;
  RoseDependencyRelationCollection = IRoseDependencyRelationCollection;
  RoseObjectFlow = IRoseObjectFlow;
  RoseDependencyRelation = IRoseDependencyRelation;
  RoseDeploymentUnit = IRoseDeploymentUnit;
  RoseRelation = IRoseRelation;
  RoseDefaultModelProperties = IRoseDefaultModelProperties;
  RoseProperty = IRoseProperty;
  RoseElement = IRoseElement;
  RosePackage = IRosePackage;
  RoseControllableUnit = IRoseControllableUnit;
  RoseExternalDocument = IRoseExternalDocument;
  RoseItem = IRoseItem;
  RoseModel = IRoseModel;
  RoseContextMenuItemCollection = IRoseContextMenuItemCollection;
  RoseContextMenuItem = IRoseContextMenuItem;
  RoseAddInCollection = IRoseAddInCollection;
  RoseAddIn = IRoseAddIn;
  RoseAddInManager = IRoseAddInManager;
  RosePathMap = IRosePathMap;
  RoseApplication = IRoseApplication;


// *********************************************************************//
// DispIntf:  IRoseLineVertex
// Flags:     (4096) Dispatchable
// GUID:      {B53888D2-3094-11D2-8153-00104B97EBD5}
// *********************************************************************//
  IRoseLineVertex = dispinterface
    ['{B53888D2-3094-11D2-8153-00104B97EBD5}']
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetXPosition: Smallint; dispid 12694;
    function GetYPosition: Smallint; dispid 12695;
  end;

// *********************************************************************//
// DispIntf:  IRoseObject
// Flags:     (4096) Dispatchable
// GUID:      {7D8474B2-2C33-11D0-BBDA-00A024C67143}
// *********************************************************************//
  IRoseObject = dispinterface
    ['{7D8474B2-2C33-11D0-BBDA-00A024C67143}']
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;

// *********************************************************************//
// DispIntf:  IRoseStateMachineCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38369-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseStateMachineCollection = dispinterface
    ['{97B38369-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseStateMachine; dispid 203;
    function Exists(const pObject: IRoseStateMachine): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseStateMachine): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseStateMachineCollection); dispid 209;
    procedure Remove(const theObject: IRoseStateMachine); dispid 210;
    procedure Add(const theObject: IRoseStateMachine); dispid 208;
    function GetFirst(const Name: WideString): IRoseStateMachine; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseStateMachine; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseStateViewCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B3836A-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseStateViewCollection = dispinterface
    ['{97B3836A-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseStateView; dispid 203;
    function Exists(const pObject: IRoseStateView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseStateView): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseStateViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseStateView); dispid 210;
    procedure Add(const theObject: IRoseStateView); dispid 208;
    function GetFirst(const Name: WideString): IRoseStateView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseStateView; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseStateDiagramCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38368-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseStateDiagramCollection = dispinterface
    ['{97B38368-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseStateDiagram; dispid 203;
    function Exists(const pObject: IRoseStateDiagram): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseStateDiagram): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseStateDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseStateDiagram); dispid 210;
    procedure Add(const theObject: IRoseStateDiagram); dispid 208;
    function GetFirst(const Name: WideString): IRoseStateDiagram; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseStateDiagram; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseEventCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38361-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseEventCollection = dispinterface
    ['{97B38361-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseEvent; dispid 203;
    function Exists(const pObject: IRoseEvent): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseEvent): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseEventCollection); dispid 209;
    procedure Remove(const theObject: IRoseEvent); dispid 210;
    procedure Add(const theObject: IRoseEvent); dispid 208;
    function GetFirst(const Name: WideString): IRoseEvent; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseEvent; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseActionCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B3835F-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseActionCollection = dispinterface
    ['{97B3835F-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseAction; dispid 203;
    function Exists(const pObject: IRoseAction): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseAction): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseActionCollection); dispid 209;
    procedure Remove(const theObject: IRoseAction); dispid 210;
    procedure Add(const theObject: IRoseAction); dispid 208;
    function GetFirst(const Name: WideString): IRoseAction; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseAction; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseTransitionCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B3836B-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseTransitionCollection = dispinterface
    ['{97B3836B-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseTransition; dispid 203;
    function Exists(const pObject: IRoseTransition): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseTransition): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseTransitionCollection); dispid 209;
    procedure Remove(const theObject: IRoseTransition); dispid 210;
    procedure Add(const theObject: IRoseTransition); dispid 208;
    function GetFirst(const Name: WideString): IRoseTransition; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseTransition; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseStateCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38367-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseStateCollection = dispinterface
    ['{97B38367-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseState; dispid 203;
    function Exists(const pObject: IRoseState): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseState): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseStateCollection); dispid 209;
    procedure Remove(const theObject: IRoseState); dispid 210;
    procedure Add(const theObject: IRoseState); dispid 208;
    function GetFirst(const Name: WideString): IRoseState; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseState; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseSwimLaneViewCollection
// Flags:     (4096) Dispatchable
// GUID:      {7FFC5F46-C0C2-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseSwimLaneViewCollection = dispinterface
    ['{7FFC5F46-C0C2-11D2-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseSwimLaneView; dispid 203;
    function Exists(const pObject: IRoseSwimLaneView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseSwimLaneView): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseSwimLaneViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseSwimLaneView); dispid 210;
    procedure Add(const theObject: IRoseSwimLaneView); dispid 208;
    function GetFirst(const Name: WideString): IRoseSwimLaneView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseSwimLaneView; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseSwimLaneView
// Flags:     (4096) Dispatchable
// GUID:      {68F63C21-B047-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseSwimLaneView = dispinterface
    ['{68F63C21-B047-11D2-92AA-004005141253}']
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetMinHeight: Smallint; dispid 218;
    function GetDefaultWidth: Smallint; dispid 215;
    function GetQualifiedName: WideString; dispid 12555;
    function GetIconIndex: Smallint; dispid 12824;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function IsSelected: WordBool; dispid 212;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function SupportsFillColor: WordBool; dispid 210;
    function SupportsLineColor: WordBool; dispid 211;
    function PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    procedure Invalidate; dispid 207;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetAttachedNotes: IRoseNoteViewCollection; dispid 12829;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    property StereotypeDisplay: Smallint dispid 12837;
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Font: IRoseView_Font dispid 12493;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Item: IRoseItem dispid 221;
    property ParentView: IRoseItemView dispid 220;
    property SubViews: IRoseItemViewCollection dispid 219;
    property LineColor: IRoseView_LineColor dispid 208;
    property FillColor: IRoseView_FillColor dispid 206;
    property Width: Smallint dispid 205;
    property Height: Smallint dispid 204;
    property XPosition: Smallint dispid 203;
    property YPosition: Smallint dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseSyncItemViewCollection
// Flags:     (4096) Dispatchable
// GUID:      {94CA1891-5D13-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseSyncItemViewCollection = dispinterface
    ['{94CA1891-5D13-11D2-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseSyncItemView; dispid 203;
    function Exists(const pObject: IRoseSyncItemView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseSyncItemView): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseSyncItemViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseSyncItemView); dispid 210;
    procedure Add(const theObject: IRoseSyncItemView); dispid 208;
    function GetFirst(const Name: WideString): IRoseSyncItemView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseSyncItemView; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseSyncItemView
// Flags:     (4096) Dispatchable
// GUID:      {94CA1888-5D13-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseSyncItemView = dispinterface
    ['{94CA1888-5D13-11D2-92AA-004005141253}']
    function HasParentView: WordBool; dispid 223;
    function GetQualifiedName: WideString; dispid 12555;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function HasItem: WordBool; dispid 222;
    function GetDefaultHeight: Smallint; dispid 216;
    function IdentifyClass: WideString; dispid 12668;
    function GetIconIndex: Smallint; dispid 12824;
    function GetMinWidth: Smallint; dispid 217;
    function GetMinHeight: Smallint; dispid 218;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function GetAttachedNotes: IRoseNoteViewCollection; dispid 12829;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function IsSelected: WordBool; dispid 212;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function SupportsFillColor: WordBool; dispid 210;
    function SupportsLineColor: WordBool; dispid 211;
    function PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    procedure Invalidate; dispid 207;
    function GetSynchronization: IRoseSyncItem; dispid 12758;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetDefaultWidth: Smallint; dispid 215;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    property StereotypeDisplay: Smallint dispid 12837;
    property Horizontal: WordBool dispid 12789;
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Font: IRoseView_Font dispid 12493;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Item: IRoseItem dispid 221;
    property ParentView: IRoseItemView dispid 220;
    property SubViews: IRoseItemViewCollection dispid 219;
    property LineColor: IRoseView_LineColor dispid 208;
    property FillColor: IRoseView_FillColor dispid 206;
    property Width: Smallint dispid 205;
    property Height: Smallint dispid 204;
    property XPosition: Smallint dispid 203;
    property YPosition: Smallint dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseDecisionViewCollection
// Flags:     (4096) Dispatchable
// GUID:      {BEAED601-578D-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseDecisionViewCollection = dispinterface
    ['{BEAED601-578D-11D2-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseDecisionView; dispid 203;
    function Exists(const pObject: IRoseDecisionView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseDecisionView): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseDecisionViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseDecisionView); dispid 210;
    procedure Add(const theObject: IRoseDecisionView); dispid 208;
    function GetFirst(const Name: WideString): IRoseDecisionView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseDecisionView; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseActivityViewCollection
// Flags:     (4096) Dispatchable
// GUID:      {BEAED5FE-578D-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseActivityViewCollection = dispinterface
    ['{BEAED5FE-578D-11D2-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseActivityView; dispid 203;
    function Exists(const pObject: IRoseActivityView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseActivityView): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseActivityViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseActivityView); dispid 210;
    procedure Add(const theObject: IRoseActivityView); dispid 208;
    function GetFirst(const Name: WideString): IRoseActivityView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseActivityView; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseDecisionView
// Flags:     (4096) Dispatchable
// GUID:      {BEAED5F9-578D-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseDecisionView = dispinterface
    ['{BEAED5F9-578D-11D2-92AA-004005141253}']
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetQualifiedName: WideString; dispid 12555;
    function GetMinHeight: Smallint; dispid 218;
    function IdentifyClass: WideString; dispid 12668;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function GetIconIndex: Smallint; dispid 12824;
    procedure Invalidate; dispid 207;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function SupportsLineColor: WordBool; dispid 211;
    function IsSelected: WordBool; dispid 212;
    function GetDefaultWidth: Smallint; dispid 215;
    function SupportsFillColor: WordBool; dispid 210;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetDecision: IRoseDecision; dispid 12742;
    function GetAttachedNotes: IRoseNoteViewCollection; dispid 12829;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    property StereotypeDisplay: Smallint dispid 12837;
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Font: IRoseView_Font dispid 12493;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Item: IRoseItem dispid 221;
    property ParentView: IRoseItemView dispid 220;
    property SubViews: IRoseItemViewCollection dispid 219;
    property LineColor: IRoseView_LineColor dispid 208;
    property FillColor: IRoseView_FillColor dispid 206;
    property Width: Smallint dispid 205;
    property Height: Smallint dispid 204;
    property XPosition: Smallint dispid 203;
    property YPosition: Smallint dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseActivityView
// Flags:     (4096) Dispatchable
// GUID:      {BEAED5FC-578D-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseActivityView = dispinterface
    ['{BEAED5FC-578D-11D2-92AA-004005141253}']
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetQualifiedName: WideString; dispid 12555;
    function GetMinHeight: Smallint; dispid 218;
    function IdentifyClass: WideString; dispid 12668;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function GetIconIndex: Smallint; dispid 12824;
    procedure Invalidate; dispid 207;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function SupportsLineColor: WordBool; dispid 211;
    function IsSelected: WordBool; dispid 212;
    function GetDefaultWidth: Smallint; dispid 215;
    function SupportsFillColor: WordBool; dispid 210;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetActivity: IRoseActivity; dispid 12741;
    function GetAttachedNotes: IRoseNoteViewCollection; dispid 12829;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    property StereotypeDisplay: Smallint dispid 12837;
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Font: IRoseView_Font dispid 12493;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Item: IRoseItem dispid 221;
    property ParentView: IRoseItemView dispid 220;
    property SubViews: IRoseItemViewCollection dispid 219;
    property LineColor: IRoseView_LineColor dispid 208;
    property FillColor: IRoseView_FillColor dispid 206;
    property Width: Smallint dispid 205;
    property Height: Smallint dispid 204;
    property XPosition: Smallint dispid 203;
    property YPosition: Smallint dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseStateView
// Flags:     (4096) Dispatchable
// GUID:      {7BD909E1-9AF9-11D0-A214-00A024FFFE40}
// *********************************************************************//
  IRoseStateView = dispinterface
    ['{7BD909E1-9AF9-11D0-A214-00A024FFFE40}']
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetState: IRoseState; dispid 415;
    function GetMinHeight: Smallint; dispid 218;
    function GetQualifiedName: WideString; dispid 12555;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function GetIconIndex: Smallint; dispid 12824;
    procedure Invalidate; dispid 207;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function SupportsLineColor: WordBool; dispid 211;
    function IsSelected: WordBool; dispid 212;
    function GetDefaultWidth: Smallint; dispid 215;
    function SupportsFillColor: WordBool; dispid 210;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetAttachedNotes: IRoseNoteViewCollection; dispid 12829;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    property StereotypeDisplay: Smallint dispid 12837;
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Font: IRoseView_Font dispid 12493;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Item: IRoseItem dispid 221;
    property ParentView: IRoseItemView dispid 220;
    property SubViews: IRoseItemViewCollection dispid 219;
    property LineColor: IRoseView_LineColor dispid 208;
    property FillColor: IRoseView_FillColor dispid 206;
    property Width: Smallint dispid 205;
    property Height: Smallint dispid 204;
    property XPosition: Smallint dispid 203;
    property YPosition: Smallint dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseStateDiagram
// Flags:     (4096) Dispatchable
// GUID:      {7ADDA701-9B06-11D0-A214-00A024FFFE40}
// *********************************************************************//
  IRoseStateDiagram = dispinterface
    ['{7ADDA701-9B06-11D0-A214-00A024FFFE40}']
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetDiagramSwimLaneViews: IRoseSwimLaneViewCollection; dispid 12794;
    function GetSwimLaneViews(const aSwimLane: IRoseSwimLane): IRoseSwimLaneViewCollection; dispid 12792;
    function RemoveSwimLaneView(const aSwimLaneView: IRoseSwimLaneView): WordBool; dispid 12795;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetSelectedSwimLaneViews: IRoseSwimLaneViewCollection; dispid 12793;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function AddStateView(const aState: IRoseState): IRoseStateView; dispid 421;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    procedure Layout; dispid 204;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure Update; dispid 206;
    procedure Invalidate; dispid 205;
    function IsActive: WordBool; dispid 209;
    function Exists(const theItem: IRoseItem): WordBool; dispid 210;
    procedure Activate; dispid 211;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetNoteViews: IRoseNoteViewCollection; dispid 220;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    procedure Render(const FileName: WideString); dispid 217;
    function GetStateView(const State: IRoseState): IRoseStateView; dispid 426;
    function GetStateViews: IRoseStateViewCollection; dispid 424;
    function AddSynchronizationView(const theSyncItem: IRoseSyncItem; isHorizontal: WordBool): IRoseSyncItemView; dispid 12749;
    function GetSelectedTransitions: IRoseTransitionCollection; dispid 425;
    function RemoveStateView(const View: IRoseStateView): WordBool; dispid 422;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function AddActivityView(const theActivity: IRoseActivity): IRoseActivityView; dispid 12714;
    function GetSelectedStateViews: IRoseStateViewCollection; dispid 423;
    function GetSelectedStates: IRoseStateCollection; dispid 427;
    function RemoveDecisionView(const theDecisionView: IRoseDecisionView): WordBool; dispid 12725;
    function GetSelectedDecisions: IRoseDecisionCollection; dispid 12718;
    function AddDecisionView(const theDecision: IRoseDecision): IRoseDecisionView; dispid 12715;
    function GetSelectedActivityViews: IRoseActivityViewCollection; dispid 12719;
    function GetSelectedDecisionViews: IRoseDecisionViewCollection; dispid 12720;
    function GetSelectedActivities: IRoseActivityCollection; dispid 12717;
    function GetDiagramStateVertexViews: IRoseItemViewCollection; dispid 12753;
    function GetSelectedSynchronizations: IRoseSyncItemCollection; dispid 12750;
    function RemoveActivityView(const theActivityView: IRoseActivityView): WordBool; dispid 12724;
    function GetSelectedSynchronizationViews: IRoseSyncItemViewCollection; dispid 12751;
    function IdentifyClass: WideString; dispid 12668;
    function CenterDiagramToView(const theView: IRoseItemView): WordBool; dispid 12861;
    function RemoveSynchronizationView(const theSyncItemView: IRoseSyncItemView): WordBool; dispid 12757;
    procedure AutosizeAll; dispid 12862;
    function RemoveObjectInstanceView(const theObjectInstanceView: IRoseInstanceView): WordBool; dispid 12848;
    function AddRelationView(const theRelation: IRoseRelation): WordBool; dispid 12787;
    function GetDiagramDecisionViews: IRoseDecisionViewCollection; dispid 12755;
    function AddSwimLaneView(const aSwimLane: IRoseSwimLane): IRoseSwimLaneView; dispid 12791;
    function GetDiagramSynchronizationViews: IRoseSyncItemViewCollection; dispid 12756;
    function GetDiagramActivityViews: IRoseActivityViewCollection; dispid 12754;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetDiagramObjectInstanceViews: IRoseInstanceViewCollection; dispid 12845;
    function GetObjectInstanceViews(const theObjectInstance: IRoseObjectInstance): IRoseInstanceViewCollection; dispid 12846;
    function GetSelectedItems: IRoseItemCollection; dispid 12525;
    function GetQualifiedName: WideString; dispid 12555;
    function RemoveItemView(const theItemView: IRoseItemView): WordBool; dispid 12832;
    function AddObjectInstanceView(const theObjectInstance: IRoseObjectInstance): IRoseInstanceView; dispid 12847;
    function LayoutSelectedViews(StackClasses: WordBool; ClassesPerRow: Smallint; DistX: Smallint; 
                                 DistY: Smallint; UpperLeftX: Smallint; UpperLeftY: Smallint): WordBool; dispid 12873;
    function GetParentContext: IRoseItem; dispid 12823;
    function GetIconIndex: Smallint; dispid 12824;
    property IsActivityDiagram: WordBool dispid 12761;
    property ZoomFactor: Smallint dispid 12690;
    property Documentation: WideString dispid 12656;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Parent: IRoseStateMachine dispid 445;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Items: IRoseItemCollection dispid 208;
    property Visible: WordBool dispid 203;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseSwimLaneCollection
// Flags:     (4096) Dispatchable
// GUID:      {7FFC5F42-C0C2-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseSwimLaneCollection = dispinterface
    ['{7FFC5F42-C0C2-11D2-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseSwimLane; dispid 203;
    function Exists(const pObject: IRoseSwimLane): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseSwimLane): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseSwimLaneCollection); dispid 209;
    procedure Remove(const theObject: IRoseSwimLane); dispid 210;
    procedure Add(const theObject: IRoseSwimLane); dispid 208;
    function GetFirst(const Name: WideString): IRoseSwimLane; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseSwimLane; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseSyncItemCollection
// Flags:     (4096) Dispatchable
// GUID:      {94CA188F-5D13-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseSyncItemCollection = dispinterface
    ['{94CA188F-5D13-11D2-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseSyncItem; dispid 203;
    function Exists(const pObject: IRoseSyncItem): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseSyncItem): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseSyncItemCollection); dispid 209;
    procedure Remove(const theObject: IRoseSyncItem); dispid 210;
    procedure Add(const theObject: IRoseSyncItem); dispid 208;
    function GetFirst(const Name: WideString): IRoseSyncItem; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseSyncItem; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseSyncItem
// Flags:     (4096) Dispatchable
// GUID:      {94CA188B-5D13-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseSyncItem = dispinterface
    ['{94CA188B-5D13-11D2-92AA-004005141253}']
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function DeleteTransition(const Transition: IRoseTransition): WordBool; dispid 427;
    function GetQualifiedName: WideString; dispid 12555;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function IdentifyClass: WideString; dispid 12668;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetSwimLanes: IRoseSwimLaneCollection; dispid 12748;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function OpenSpecification: WordBool; dispid 216;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function AddTransition(const OnEvent: WideString; const Target: IRoseState): IRoseTransition; dispid 426;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetIconIndex: Smallint; dispid 12824;
    function AddTransitionToVertex(const OnEvent: WideString; const Target: IRoseStateVertex): IRoseTransition; dispid 12814;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetPropertyClassName: WideString; dispid 128;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Parent: IRoseStateVertex dispid 12747;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Transitions: IRoseTransitionCollection dispid 422;
    property ParentStateMachine: IRoseStateMachine dispid 413;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseStateMachineOwner
// Flags:     (4096) Dispatchable
// GUID:      {94CA1882-5D13-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseStateMachineOwner = dispinterface
    ['{94CA1882-5D13-11D2-92AA-004005141253}']
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetUniqueID: WideString; dispid 102;
    function CreateStateMachine(const theStateMachineName: WideString): IRoseStateMachine; dispid 12745;
    function DeleteStateMachine(const theStateMachine: IRoseStateMachine): WordBool; dispid 12746;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetQualifiedName: WideString; dispid 12555;
    function GetPropertyClassName: WideString; dispid 128;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    function GetIconIndex: Smallint; dispid 12824;
    function GetParentItem: IRoseItem; dispid 12830;
    property StateMachines: IRoseStateMachineCollection dispid 12744;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseStateVertexCollection
// Flags:     (4096) Dispatchable
// GUID:      {BEAED5F7-578D-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseStateVertexCollection = dispinterface
    ['{BEAED5F7-578D-11D2-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseStateVertex; dispid 203;
    function Exists(const pObject: IRoseStateVertex): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseStateVertex): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseStateVertexCollection); dispid 209;
    procedure Remove(const theObject: IRoseStateVertex); dispid 210;
    procedure Add(const theObject: IRoseStateVertex); dispid 208;
    function GetFirst(const Name: WideString): IRoseStateVertex; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseStateVertex; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseDecisionCollection
// Flags:     (4096) Dispatchable
// GUID:      {BEAED5F2-578D-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseDecisionCollection = dispinterface
    ['{BEAED5F2-578D-11D2-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseDecision; dispid 203;
    function Exists(const pObject: IRoseDecision): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseDecision): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseDecisionCollection); dispid 209;
    procedure Remove(const theObject: IRoseDecision); dispid 210;
    procedure Add(const theObject: IRoseDecision); dispid 208;
    function GetFirst(const Name: WideString): IRoseDecision; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseDecision; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseActivityCollection
// Flags:     (4096) Dispatchable
// GUID:      {BEAED5F0-578D-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseActivityCollection = dispinterface
    ['{BEAED5F0-578D-11D2-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseActivity; dispid 203;
    function Exists(const pObject: IRoseActivity): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseActivity): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseActivityCollection); dispid 209;
    procedure Remove(const theObject: IRoseActivity); dispid 210;
    procedure Add(const theObject: IRoseActivity); dispid 208;
    function GetFirst(const Name: WideString): IRoseActivity; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseActivity; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseAbstractStateCollection
// Flags:     (4096) Dispatchable
// GUID:      {BEAED5EE-578D-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseAbstractStateCollection = dispinterface
    ['{BEAED5EE-578D-11D2-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseAbstractState; dispid 203;
    function Exists(const pObject: IRoseAbstractState): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseAbstractState): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseAbstractStateCollection); dispid 209;
    procedure Remove(const theObject: IRoseAbstractState); dispid 210;
    procedure Add(const theObject: IRoseAbstractState); dispid 208;
    function GetFirst(const Name: WideString): IRoseAbstractState; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseAbstractState; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseDecision
// Flags:     (4096) Dispatchable
// GUID:      {BEAED5E3-578D-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseDecision = dispinterface
    ['{BEAED5E3-578D-11D2-92AA-004005141253}']
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function DeleteTransition(const Transition: IRoseTransition): WordBool; dispid 427;
    function GetQualifiedName: WideString; dispid 12555;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function IdentifyClass: WideString; dispid 12668;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetSwimLanes: IRoseSwimLaneCollection; dispid 12748;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function OpenSpecification: WordBool; dispid 216;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function AddTransition(const OnEvent: WideString; const Target: IRoseState): IRoseTransition; dispid 426;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetIconIndex: Smallint; dispid 12824;
    function AddTransitionToVertex(const OnEvent: WideString; const Target: IRoseStateVertex): IRoseTransition; dispid 12814;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetPropertyClassName: WideString; dispid 128;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Parent: IRoseStateVertex dispid 12747;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Transitions: IRoseTransitionCollection dispid 422;
    property ParentStateMachine: IRoseStateMachine dispid 413;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseActivity
// Flags:     (4096) Dispatchable
// GUID:      {BEAED5E7-578D-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseActivity = dispinterface
    ['{BEAED5E7-578D-11D2-92AA-004005141253}']
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function DeleteStateMachine(const theStateMachine: IRoseStateMachine): WordBool; dispid 12775;
    function GetSwimLanes: IRoseSwimLaneCollection; dispid 12748;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetStateMachines: IRoseStateMachineCollection; dispid 12768;
    function AddStateMachine(const theName: WideString): IRoseStateMachine; dispid 12774;
    function GetAllSubDecisions: IRoseDecisionCollection; dispid 12806;
    function GetAllSubActivities: IRoseActivityCollection; dispid 12805;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetRoseItem: IRoseItem; dispid 207;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function AddTransition(const OnEvent: WideString; const Target: IRoseState): IRoseTransition; dispid 426;
    function GetExitActions: IRoseActionCollection; dispid 12626;
    function AddDoAction(const ActionName: WideString): IRoseAction; dispid 12639;
    function GetAllSubStates: IRoseStateCollection; dispid 425;
    function OpenSpecification: WordBool; dispid 216;
    function DeleteTransition(const Transition: IRoseTransition): WordBool; dispid 427;
    function AddExitAction(const ActionName: WideString): IRoseAction; dispid 12638;
    function GetDoActions: IRoseActionCollection; dispid 12627;
    function AddEntryAction(const ActionName: WideString): IRoseAction; dispid 12637;
    function DeleteUserDefinedEvent(const theEvent: IRoseEvent): WordBool; dispid 12636;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    function AddUserDefinedEvent(const EventName: WideString; const ActionName: WideString): IRoseEvent; dispid 12635;
    function GetUserDefinedEvents: IRoseEventCollection; dispid 12623;
    function DeleteObjectFlow(const theObjectFlow: IRoseObjectFlow): WordBool; dispid 12840;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetContext: IRoseItem; dispid 12872;
    function GetActions: IRoseActionCollection; dispid 12767;
    function GetAllSubSynchronizations: IRoseSyncItemCollection; dispid 12807;
    function GetObjectFlows: IRoseObjectFlowCollection; dispid 12853;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetQualifiedName: WideString; dispid 12555;
    function DeleteAction(const theAction: IRoseAction): WordBool; dispid 446;
    function GetEntryActions: IRoseActionCollection; dispid 12625;
    function AddTransitionToVertex(const OnEvent: WideString; const Target: IRoseStateVertex): IRoseTransition; dispid 12814;
    function AddObjectFlow(const theObjectInstance: IRoseObjectInstance): IRoseObjectFlow; dispid 12839;
    function GetIconIndex: Smallint; dispid 12824;
    property SubSynchronizations: IRoseSyncItemCollection dispid 12804;
    property SubDecisions: IRoseDecisionCollection dispid 12803;
    property SubActivities: IRoseActivityCollection dispid 12802;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Parent: IRoseStateVertex dispid 12747;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SubStates: IRoseStateCollection dispid 444;
    property Transitions: IRoseTransitionCollection dispid 422;
    property ParentStateMachine: IRoseStateMachine dispid 413;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseAbstractState
// Flags:     (4096) Dispatchable
// GUID:      {BEAED5EC-578D-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseAbstractState = dispinterface
    ['{BEAED5EC-578D-11D2-92AA-004005141253}']
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetStateMachines: IRoseStateMachineCollection; dispid 12768;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    function GetSwimLanes: IRoseSwimLaneCollection; dispid 12748;
    function GetActions: IRoseActionCollection; dispid 12767;
    function DeleteStateMachine(const theStateMachine: IRoseStateMachine): WordBool; dispid 12775;
    function AddStateMachine(const theName: WideString): IRoseStateMachine; dispid 12774;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetPropertyClassName: WideString; dispid 128;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function GetRoseItem: IRoseItem; dispid 207;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function OpenSpecification: WordBool; dispid 216;
    function DeleteUserDefinedEvent(const theEvent: IRoseEvent): WordBool; dispid 12636;
    function AddUserDefinedEvent(const EventName: WideString; const ActionName: WideString): IRoseEvent; dispid 12635;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetAllSubStates: IRoseStateCollection; dispid 425;
    function GetUserDefinedEvents: IRoseEventCollection; dispid 12623;
    function GetDoActions: IRoseActionCollection; dispid 12627;
    function GetExitActions: IRoseActionCollection; dispid 12626;
    function AddExitAction(const ActionName: WideString): IRoseAction; dispid 12638;
    function AddEntryAction(const ActionName: WideString): IRoseAction; dispid 12637;
    function GetEntryActions: IRoseActionCollection; dispid 12625;
    function DeleteAction(const theAction: IRoseAction): WordBool; dispid 446;
    function GetContext: IRoseItem; dispid 12872;
    function GetAllSubDecisions: IRoseDecisionCollection; dispid 12806;
    function GetAllSubActivities: IRoseActivityCollection; dispid 12805;
    function GetIconIndex: Smallint; dispid 12824;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function DeleteTransition(const Transition: IRoseTransition): WordBool; dispid 427;
    function AddTransition(const OnEvent: WideString; const Target: IRoseState): IRoseTransition; dispid 426;
    function GetQualifiedName: WideString; dispid 12555;
    function AddTransitionToVertex(const OnEvent: WideString; const Target: IRoseStateVertex): IRoseTransition; dispid 12814;
    function GetAllSubSynchronizations: IRoseSyncItemCollection; dispid 12807;
    function AddDoAction(const ActionName: WideString): IRoseAction; dispid 12639;
    property SubSynchronizations: IRoseSyncItemCollection dispid 12804;
    property SubDecisions: IRoseDecisionCollection dispid 12803;
    property SubActivities: IRoseActivityCollection dispid 12802;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Parent: IRoseStateVertex dispid 12747;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SubStates: IRoseStateCollection dispid 444;
    property Transitions: IRoseTransitionCollection dispid 422;
    property ParentStateMachine: IRoseStateMachine dispid 413;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseStateVertex
// Flags:     (4096) Dispatchable
// GUID:      {BEAED5E2-578D-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseStateVertex = dispinterface
    ['{BEAED5E2-578D-11D2-92AA-004005141253}']
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function DeleteTransition(const Transition: IRoseTransition): WordBool; dispid 427;
    function GetQualifiedName: WideString; dispid 12555;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function IdentifyClass: WideString; dispid 12668;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetSwimLanes: IRoseSwimLaneCollection; dispid 12748;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function OpenSpecification: WordBool; dispid 216;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function AddTransition(const OnEvent: WideString; const Target: IRoseState): IRoseTransition; dispid 426;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetIconIndex: Smallint; dispid 12824;
    function AddTransitionToVertex(const OnEvent: WideString; const Target: IRoseStateVertex): IRoseTransition; dispid 12814;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetPropertyClassName: WideString; dispid 128;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Parent: IRoseStateVertex dispid 12747;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Transitions: IRoseTransitionCollection dispid 422;
    property ParentStateMachine: IRoseStateMachine dispid 413;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseSwimLane
// Flags:     (4096) Dispatchable
// GUID:      {BEAED5EA-578D-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseSwimLane = dispinterface
    ['{BEAED5EA-578D-11D2-92AA-004005141253}']
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetStateVertices: IRoseStateVertexCollection; dispid 12712;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function DeleteLink(const aLink: IRoseLink): WordBool; dispid 418;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function IsClass: WordBool; dispid 414;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function GetClass: IRoseClass; dispid 415;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function AddLink(const Name: WideString; const ToInstance: IRoseObjectInstance): IRoseLink; dispid 417;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetIconIndex: Smallint; dispid 12824;
    function GetObjectFlows: IRoseObjectFlowCollection; dispid 12854;
    function GetContext: IRoseItem; dispid 12872;
    function AddObjectFlow(const theActivity: IRoseActivity): IRoseObjectFlow; dispid 12843;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function DeleteObjectFlow(const theObjectFlow: IRoseObjectFlow): WordBool; dispid 12844;
    function GetToolNames: IRoseStringCollection; dispid 130;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Persistence: Smallint dispid 12651;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Links: IRoseLinkCollection dispid 416;
    property MultipleInstances: WordBool dispid 413;
    property _className: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseStateMachine
// Flags:     (4096) Dispatchable
// GUID:      {A69CAB21-9179-11D0-A214-00A024FFFE40}
// *********************************************************************//
  IRoseStateMachine = dispinterface
    ['{A69CAB21-9179-11D0-A214-00A024FFFE40}']
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetAllSynchronizations: IRoseSyncItemCollection; dispid 12782;
    function DeleteStateVertex(const aStateVertex: IRoseStateVertex): WordBool; dispid 12781;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function AddDecision(const theName: WideString): IRoseDecision; dispid 12739;
    function DeleteSwimLane(const aSwimLane: IRoseSwimLane): WordBool; dispid 12780;
    function AddSwimLane(const theName: WideString): IRoseSwimLane; dispid 12779;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 12797;
    function AddSynchronization(const aName: WideString): IRoseSyncItem; dispid 12784;
    function GetAllDiagrams: IRoseStateDiagramCollection; dispid 12740;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function AddStateChartDiagram(const theName: WideString): IRoseStateDiagram; dispid 12800;
    function GetTransitions: IRoseTransitionCollection; dispid 12665;
    function GetAllDecisions: IRoseDecisionCollection; dispid 12736;
    function GetAllAbstractStates: IRoseAbstractStateCollection; dispid 12735;
    function OpenSpecification: WordBool; dispid 211;
    function DeleteState(const State: IRoseState): WordBool; dispid 218;
    function AddState(const Name: WideString): IRoseState; dispid 217;
    function IdentifyClass: WideString; dispid 12668;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetAllActivities: IRoseActivityCollection; dispid 12711;
    function AddActivity(const theName: WideString): IRoseActivity; dispid 12738;
    function GetAllStateVertices: IRoseStateVertexCollection; dispid 12737;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteStateDiagram(const theDiagram: IRoseStateDiagram): WordBool; dispid 12867;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function AddActivityDiagram(const theName: WideString): IRoseStateDiagram; dispid 12801;
    function AddObjectInstance(const theName: WideString): IRoseObjectInstance; dispid 12852;
    function DeleteObjectInstance(const theObjectInstance: IRoseObjectInstance): WordBool; dispid 12851;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RelocateState(const State: IRoseState): WordBool; dispid 219;
    function GetQualifiedName: WideString; dispid 12555;
    function GetAllTransitions: IRoseTransitionCollection; dispid 221;
    function GetAllObjectInstances: IRoseObjectInstanceCollection; dispid 12850;
    function GetIconIndex: Smallint; dispid 12824;
    function GetAllStates: IRoseStateCollection; dispid 220;
    property ObjectInstances: IRoseObjectInstanceCollection dispid 12849;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Synchronizations: IRoseSyncItemCollection dispid 12778;
    property Diagrams: IRoseStateDiagramCollection dispid 12734;
    property StateVertices: IRoseStateVertexCollection dispid 12733;
    property Decisions: IRoseDecisionCollection dispid 12732;
    property AbstractStates: IRoseAbstractStateCollection dispid 12731;
    property Activities: IRoseActivityCollection dispid 12730;
    property SwimLanes: IRoseSwimLaneCollection dispid 12729;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property States: IRoseStateCollection dispid 222;
    property ParentClass: IRoseClass dispid 215;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseEvent
// Flags:     (4096) Dispatchable
// GUID:      {A69CAB22-9179-11D0-A214-00A024FFFE40}
// *********************************************************************//
  IRoseEvent = dispinterface
    ['{A69CAB22-9179-11D0-A214-00A024FFFE40}']
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function GetAction: IRoseAction; dispid 12634;
    function IdentifyClass: WideString; dispid 12668;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetPropertyClassName: WideString; dispid 128;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetQualifiedName: WideString; dispid 12555;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetIconIndex: Smallint; dispid 12824;
    property GuardCondition: WideString dispid 12622;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Name: WideString dispid 216;
    property Arguments: WideString dispid 215;
  end;

// *********************************************************************//
// DispIntf:  IRoseAction
// Flags:     (4096) Dispatchable
// GUID:      {13881143-93C1-11D0-A214-00A024FFFE40}
// *********************************************************************//
  IRoseAction = dispinterface
    ['{13881143-93C1-11D0-A214-00A024FFFE40}']
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IdentifyClass: WideString; dispid 12668;
    function GetQualifiedName: WideString; dispid 12555;
    function OpenSpecification: WordBool; dispid 216;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetRoseItem: IRoseItem; dispid 207;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetContext: IRoseItem; dispid 12872;
    function GetIconIndex: Smallint; dispid 12824;
    function GetPropertyClassName: WideString; dispid 128;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Target: WideString dispid 12621;
    property Arguments: WideString dispid 12620;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseTransition
// Flags:     (4096) Dispatchable
// GUID:      {574130A1-93B8-11D0-A214-00A024FFFE40}
// *********************************************************************//
  IRoseTransition = dispinterface
    ['{574130A1-93B8-11D0-A214-00A024FFFE40}']
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetTargetState: IRoseState; dispid 12631;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function IdentifyClass: WideString; dispid 12668;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetPropertyClassName: WideString; dispid 128;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetSourceState: IRoseState; dispid 12632;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetClient: IRoseItem; dispid 12608;
    function GetRoseItem: IRoseItem; dispid 207;
    function RedirectTo(const newTarget: IRoseState): WordBool; dispid 642;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function GetQualifiedName: WideString; dispid 12555;
    function GetSupplier: IRoseItem; dispid 12609;
    function GetTriggerAction: IRoseAction; dispid 12629;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetTargetStateVertex: IRoseStateVertex; dispid 12812;
    function GetContext: IRoseItem; dispid 12872;
    function GetTriggerEvent: IRoseEvent; dispid 12633;
    function GetIconIndex: Smallint; dispid 12824;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetSendAction: IRoseAction; dispid 12630;
    function GetSourceStateVertex: IRoseStateVertex; dispid 12813;
    function RenderIconToClipboard: WordBool; dispid 12820;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseState
// Flags:     (4096) Dispatchable
// GUID:      {A69CAB23-9179-11D0-A214-00A024FFFE40}
// *********************************************************************//
  IRoseState = dispinterface
    ['{A69CAB23-9179-11D0-A214-00A024FFFE40}']
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetActions: IRoseActionCollection; dispid 12767;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetSwimLanes: IRoseSwimLaneCollection; dispid 12748;
    function AddDoAction(const ActionName: WideString): IRoseAction; dispid 12639;
    function AddExitAction(const ActionName: WideString): IRoseAction; dispid 12638;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function AddStateMachine(const theName: WideString): IRoseStateMachine; dispid 12774;
    function GetStateMachines: IRoseStateMachineCollection; dispid 12768;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetRoseItem: IRoseItem; dispid 207;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function IdentifyClass: WideString; dispid 12668;
    function GetAllSubStates: IRoseStateCollection; dispid 425;
    function AddUserDefinedEvent(const EventName: WideString; const ActionName: WideString): IRoseEvent; dispid 12635;
    function GetDoActions: IRoseActionCollection; dispid 12627;
    function OpenSpecification: WordBool; dispid 216;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function AddTransition(const OnEvent: WideString; const Target: IRoseState): IRoseTransition; dispid 426;
    function AddEntryAction(const ActionName: WideString): IRoseAction; dispid 12637;
    function GetQualifiedName: WideString; dispid 12555;
    function GetEntryActions: IRoseActionCollection; dispid 12625;
    function GetAction(const theEvent: IRoseEvent): IRoseAction; dispid 12624;
    function DeleteUserDefinedEvent(const theEvent: IRoseEvent): WordBool; dispid 12636;
    function GetExitActions: IRoseActionCollection; dispid 12626;
    function GetUserDefinedEvents: IRoseEventCollection; dispid 12623;
    function DeleteTransition(const Transition: IRoseTransition): WordBool; dispid 427;
    function GetContext: IRoseItem; dispid 12872;
    function AddTransitionToVertex(const OnEvent: WideString; const Target: IRoseStateVertex): IRoseTransition; dispid 12814;
    function GetAllSubActivities: IRoseActivityCollection; dispid 12805;
    function GetIconIndex: Smallint; dispid 12824;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function DeleteStateMachine(const theStateMachine: IRoseStateMachine): WordBool; dispid 12775;
    function AddState(const Name: WideString): IRoseState; dispid 428;
    function DeleteAction(const theAction: IRoseAction): WordBool; dispid 446;
    function RelocateState(const State: IRoseState): WordBool; dispid 430;
    function GetAllSubSynchronizations: IRoseSyncItemCollection; dispid 12807;
    function GetAllSubDecisions: IRoseDecisionCollection; dispid 12806;
    function DeleteState(const State: IRoseState): WordBool; dispid 429;
    property SubSynchronizations: IRoseSyncItemCollection dispid 12804;
    property SubDecisions: IRoseDecisionCollection dispid 12803;
    property SubActivities: IRoseActivityCollection dispid 12802;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Parent: IRoseStateVertex dispid 12747;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SubStates: IRoseStateCollection dispid 444;
    property StateKind: IRoseRichType dispid 423;
    property Transitions: IRoseTransitionCollection dispid 422;
    property History: WordBool dispid 421;
    property ParentStateMachine: IRoseStateMachine dispid 413;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseRichTypeValuesCollection
// Flags:     (4096) Dispatchable
// GUID:      {BF8C1040-96DD-11CF-B091-00A0241E3F73}
// *********************************************************************//
  IRoseRichTypeValuesCollection = dispinterface
    ['{BF8C1040-96DD-11CF-B091-00A0241E3F73}']
    function GetAt(id: Smallint): WideString; dispid 203;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseRichType
// Flags:     (4096) Dispatchable
// GUID:      {EB7AAB60-939C-11CF-B091-00A0241E3F73}
// *********************************************************************//
  IRoseRichType = dispinterface
    ['{EB7AAB60-939C-11CF-B091-00A0241E3F73}']
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    property Name: WideString dispid 203;
    property Value: Smallint dispid 202;
    property Types: IRoseRichTypeValuesCollection dispid 204;
  end;

// *********************************************************************//
// DispIntf:  IRoseModuleVisibilityRelationship
// Flags:     (4096) Dispatchable
// GUID:      {9EF8DDD6-E697-11CF-BBD1-00A024C67143}
// *********************************************************************//
  IRoseModuleVisibilityRelationship = dispinterface
    ['{9EF8DDD6-E697-11CF-BBD1-00A024C67143}']
    function GetSupplier: IRoseItem; dispid 12609;
    function IdentifyClass: WideString; dispid 12668;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetClient: IRoseItem; dispid 12608;
    function HasSupplier: WordBool; dispid 12607;
    function GetIconIndex: Smallint; dispid 12824;
    function GetQualifiedName: WideString; dispid 12555;
    function HasClient: WordBool; dispid 12606;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetUniqueID: WideString; dispid 102;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetContext: IRoseItem; dispid 12872;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function OpenSpecification: WordBool; dispid 216;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property SupplierClass: IRoseClass dispid 12617;
    property ContextClass: IRoseClass dispid 12616;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SupplierSubsystem: IRoseSubsystem dispid 618;
    property ContextSubsystem: IRoseSubsystem dispid 617;
    property SupplierModule: IRoseModule dispid 615;
    property ContextModule: IRoseModule dispid 614;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseSubsystemViewCollection
// Flags:     (4096) Dispatchable
// GUID:      {CA3AD902-BFCE-11D0-89F5-0020AFD6C181}
// *********************************************************************//
  IRoseSubsystemViewCollection = dispinterface
    ['{CA3AD902-BFCE-11D0-89F5-0020AFD6C181}']
    function GetAt(Index: Smallint): IRoseSubsystemView; dispid 203;
    function Exists(const pObject: IRoseSubsystemView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseSubsystemView): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseSubsystemViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseSubsystemView); dispid 210;
    procedure Add(const theObject: IRoseSubsystemView); dispid 208;
    function GetFirst(const Name: WideString): IRoseSubsystemView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseSubsystemView; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseComponentViewCollection
// Flags:     (4096) Dispatchable
// GUID:      {C640C861-F2D3-11D0-883A-3C8B00C10000}
// *********************************************************************//
  IRoseComponentViewCollection = dispinterface
    ['{C640C861-F2D3-11D0-883A-3C8B00C10000}']
    function GetAt(Index: Smallint): IRoseComponentView; dispid 203;
    function Exists(const pObject: IRoseComponentView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseComponentView): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseComponentViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseComponentView); dispid 210;
    procedure Add(const theObject: IRoseComponentView); dispid 208;
    function GetFirst(const Name: WideString): IRoseComponentView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseComponentView; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseSubsystemView
// Flags:     (4096) Dispatchable
// GUID:      {14028C92-C06C-11D0-89F5-0020AFD6C181}
// *********************************************************************//
  IRoseSubsystemView = dispinterface
    ['{14028C92-C06C-11D0-89F5-0020AFD6C181}']
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetQualifiedName: WideString; dispid 12555;
    function GetMinHeight: Smallint; dispid 218;
    function GetSubsystem: IRoseSubsystem; dispid 12592;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function GetIconIndex: Smallint; dispid 12824;
    procedure Invalidate; dispid 207;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function SupportsLineColor: WordBool; dispid 211;
    function IsSelected: WordBool; dispid 212;
    function GetDefaultWidth: Smallint; dispid 215;
    function SupportsFillColor: WordBool; dispid 210;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetAttachedNotes: IRoseNoteViewCollection; dispid 12829;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    property StereotypeDisplay: Smallint dispid 12837;
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Font: IRoseView_Font dispid 12493;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Item: IRoseItem dispid 221;
    property ParentView: IRoseItemView dispid 220;
    property SubViews: IRoseItemViewCollection dispid 219;
    property LineColor: IRoseView_LineColor dispid 208;
    property FillColor: IRoseView_FillColor dispid 206;
    property Width: Smallint dispid 205;
    property Height: Smallint dispid 204;
    property XPosition: Smallint dispid 203;
    property YPosition: Smallint dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseComponentView
// Flags:     (4096) Dispatchable
// GUID:      {14028C94-C06C-11D0-89F5-0020AFD6C181}
// *********************************************************************//
  IRoseComponentView = dispinterface
    ['{14028C94-C06C-11D0-89F5-0020AFD6C181}']
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetQualifiedName: WideString; dispid 12555;
    function GetMinHeight: Smallint; dispid 218;
    function GetComponent: IRoseModule; dispid 12585;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function GetIconIndex: Smallint; dispid 12824;
    procedure Invalidate; dispid 207;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function SupportsLineColor: WordBool; dispid 211;
    function IsSelected: WordBool; dispid 212;
    function GetDefaultWidth: Smallint; dispid 215;
    function SupportsFillColor: WordBool; dispid 210;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetAttachedNotes: IRoseNoteViewCollection; dispid 12829;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    property StereotypeDisplay: Smallint dispid 12837;
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Font: IRoseView_Font dispid 12493;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Item: IRoseItem dispid 221;
    property ParentView: IRoseItemView dispid 220;
    property SubViews: IRoseItemViewCollection dispid 219;
    property LineColor: IRoseView_LineColor dispid 208;
    property FillColor: IRoseView_FillColor dispid 206;
    property Width: Smallint dispid 205;
    property Height: Smallint dispid 204;
    property XPosition: Smallint dispid 203;
    property YPosition: Smallint dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseModuleDiagram
// Flags:     (4096) Dispatchable
// GUID:      {3FD9D004-93B0-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseModuleDiagram = dispinterface
    ['{3FD9D004-93B0-11CF-B3D4-00A0241DB1D0}']
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function AddSubsystemView(const aSubsystem: IRoseSubsystem): IRoseSubsystemView; dispid 426;
    function RemoveComponentView(const aComponentView: IRoseComponentView): WordBool; dispid 425;
    function IdentifyClass: WideString; dispid 12668;
    function GetQualifiedName: WideString; dispid 12555;
    function AddRelationView(const theRelation: IRoseRelation): WordBool; dispid 12787;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    procedure Update; dispid 206;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetPropertyClassName: WideString; dispid 128;
    function GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    procedure Invalidate; dispid 205;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    procedure Activate; dispid 211;
    function GetSubsystems: IRoseSubsystemCollection; dispid 419;
    function AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function Exists(const theItem: IRoseItem): WordBool; dispid 210;
    function IsActive: WordBool; dispid 209;
    function GetModules: IRoseModuleCollection; dispid 418;
    function GetNoteViews: IRoseNoteViewCollection; dispid 220;
    procedure RenderEnhancedToClipboard; dispid 223;
    procedure RenderToClipboard; dispid 222;
    function GetSelectedSubsystems: IRoseSubsystemCollection; dispid 421;
    function GetSelectedModules: IRoseModuleCollection; dispid 420;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    function RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function CenterDiagramToView(const theView: IRoseItemView): WordBool; dispid 12861;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function LayoutSelectedViews(StackClasses: WordBool; ClassesPerRow: Smallint; DistX: Smallint; 
                                 DistY: Smallint; UpperLeftX: Smallint; UpperLeftY: Smallint): WordBool; dispid 12873;
    function GetSelectedItems: IRoseItemCollection; dispid 12525;
    function RemoveSubsystemView(const aSubsystemView: IDispatch): WordBool; dispid 427;
    procedure AutosizeAll; dispid 12862;
    function GetParentContext: IRoseItem; dispid 12823;
    function AddComponentView(const aModule: IRoseModule): IRoseComponentView; dispid 424;
    procedure Render(const FileName: WideString); dispid 217;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function RemoveItemView(const theItemView: IRoseItemView): WordBool; dispid 12832;
    function GetIconIndex: Smallint; dispid 12824;
    property ZoomFactor: Smallint dispid 12690;
    property Documentation: WideString dispid 12656;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SubsystemViews: IRoseSubsystemViewCollection dispid 423;
    property ComponentViews: IRoseComponentViewCollection dispid 422;
    property ParentSubsystem: IRoseSubsystem dispid 411;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Items: IRoseItemCollection dispid 208;
    property Visible: WordBool dispid 203;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseModule
// Flags:     (4096) Dispatchable
// GUID:      {C78E702A-86E4-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseModule = dispinterface
    ['{C78E702A-86E4-11CF-B3D4-00A0241DB1D0}']
    function AddRealizeRel(const theRelationName: WideString; const theInterfaceName: WideString): IRoseRealizeRelation; dispid 12613;
    function DeleteRealizeRel(const theRealizeRel: IRoseRealizeRelation): WordBool; dispid 12614;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetRealizeRelations: IRoseRealizeRelationCollection; dispid 12615;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function GetSubsystemDependencies(const theSubsystem: IRoseSubsystem): IRoseModuleVisibilityRelationshipCollection; dispid 423;
    function AddSubsystemVisibilityRelation(const theSubsystem: IRoseSubsystem): IRoseModuleVisibilityRelationship; dispid 428;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetIconIndex: Smallint; dispid 12824;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function AddVisibilityRelationship(const theModule: IRoseModule): IRoseModuleVisibilityRelationship; dispid 419;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function DeleteVisibilityRelationship(const theVisibilityRelationship: IRoseModuleVisibilityRelationship): WordBool; dispid 420;
    function OpenSpecification: WordBool; dispid 216;
    function GetAssignedClasses: IRoseClassCollection; dispid 418;
    function GetDependencies: IRoseModuleVisibilityRelationshipCollection; dispid 421;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function GetAllDependencies: IRoseModuleVisibilityRelationshipCollection; dispid 422;
    function GetToolNames: IRoseStringCollection; dispid 130;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property AssignedLanguage: WideString dispid 12566;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property OtherPart: IRoseModule dispid 417;
    property Declarations: WideString dispid 416;
    property Part: IRoseRichType dispid 415;
    property type_: IRoseRichType dispid 414;
    property ParentSubsystem: IRoseSubsystem dispid 413;
    property Path: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseSubsystem
// Flags:     (4096) Dispatchable
// GUID:      {C78E702C-86E4-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseSubsystem = dispinterface
    ['{C78E702C-86E4-11CF-B3D4-00A0241DB1D0}']
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsLocked: WordBool; dispid 12703;
    procedure Refresh; dispid 12701;
    function SaveAs(const Path: WideString): WordBool; dispid 12443;
    function GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function GetAllModules: IRoseModuleCollection; dispid 423;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetToolNames: IRoseStringCollection; dispid 130;
    procedure RelocateModule(const theModule: IRoseModule); dispid 420;
    function OpenSpecification: WordBool; dispid 216;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetRoseItem: IRoseItem; dispid 207;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    procedure RelocateSubsystem(const theSubsystem: IRoseSubsystem); dispid 421;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function AddModule(const theName: WideString): IRoseModule; dispid 416;
    procedure RelocateModuleDiagram(const theModDiagram: IRoseModuleDiagram); dispid 422;
    function AddSubsystem(const theName: WideString): IRoseSubsystem; dispid 419;
    function AddModuleDiagram(const Name: WideString): IRoseModuleDiagram; dispid 418;
    function GetSubsystemDependencies(const theSubsystem: IRoseSubsystem): IRoseModuleVisibilityRelationshipCollection; dispid 428;
    function GetAssignedClasses: IRoseClassCollection; dispid 426;
    function IsModifiable: WordBool; dispid 12438;
    function GetVisibleSubsystems: IRoseSubsystemCollection; dispid 427;
    function AddSubsystemVisibilityRelation(const theSubsystem: IDispatch): IDispatch; dispid 434;
    function DeleteModule(const pIDispatch: IRoseModule): WordBool; dispid 449;
    function GetAllSubsystems: IRoseSubsystemCollection; dispid 424;
    function GetAssignedCategories: IRoseCategoryCollection; dispid 425;
    function Load: WordBool; dispid 12436;
    function IsControlled: WordBool; dispid 12433;
    function DeleteSubSystem(const pIDispatch: IRoseSubsystem): WordBool; dispid 450;
    function Control(const Path: WideString): WordBool; dispid 12434;
    function IsLoaded: WordBool; dispid 12435;
    function GetFileName: WideString; dispid 12441;
    function Unload: WordBool; dispid 12439;
    function IsRootPackage: WordBool; dispid 621;
    function Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetIconIndex: Smallint; dispid 12824;
    function GetQualifiedName: WideString; dispid 12555;
    function Save: WordBool; dispid 12442;
    function IsModified: WordBool; dispid 12654;
    function Uncontrol: WordBool; dispid 12655;
    function GetContext: IRoseItem; dispid 12872;
    function AddVisibilityRelationship(const theModule: IRoseModule): IRoseModuleVisibilityRelationship; dispid 430;
    procedure Lock; dispid 12708;
    function DeleteVisibilityRelationship(const theVisibilityRelationship: IRoseModuleVisibilityRelationship): WordBool; dispid 433;
    function TopLevel: WordBool; dispid 429;
    function NeedsRefreshing: WordBool; dispid 12704;
    function GetVisibilityRelations: IRoseModuleVisibilityRelationshipCollection; dispid 12705;
    procedure Unlock; dispid 12709;
    function GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    property PetalVersion: Integer dispid 12881;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ModuleDiagrams: IRoseModuleDiagramCollection dispid 415;
    property ParentSubsystem: IRoseSubsystem dispid 414;
    property Subsystems: IRoseSubsystemCollection dispid 413;
    property Modules: IRoseModuleCollection dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseUseCase
// Flags:     (4096) Dispatchable
// GUID:      {7E7F6EE0-16DE-11D0-8976-00A024774419}
// *********************************************************************//
  IRoseUseCase = dispinterface
    ['{7E7F6EE0-16DE-11D0-8976-00A024774419}']
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetQualifiedName: WideString; dispid 12555;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function DeleteAssociation(const pDispatchAssociation: IRoseAssociation): WordBool; dispid 431;
    function GetSuperUseCases: IRoseUseCaseCollection; dispid 432;
    function GetRoles: IRoseRoleCollection; dispid 434;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetPropertyClassName: WideString; dispid 128;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetInheritRelations: IRoseInheritRelationCollection; dispid 433;
    function DeleteInheritRel(const pIDispatchRelation: IRoseInheritRelation): WordBool; dispid 422;
    function GetAssociations: IRoseAssociationCollection; dispid 426;
    function GetRoseItem: IRoseItem; dispid 207;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function DeleteScenarioDiagram(const pIDispatch: IRoseScenarioDiagram): WordBool; dispid 418;
    function AddScenarioDiagram(const szName: WideString; iType: Smallint): IRoseScenarioDiagram; dispid 419;
    function DeleteClassDiagram(const pIDispatch: IRoseClassDiagram): WordBool; dispid 417;
    function AddAssociation(const szSupplierRoleName: WideString; 
                            const szSupplierRoleType: WideString): IRoseAssociation; dispid 430;
    function AddInheritRel(const szName: WideString; const szParentName: WideString): IRoseInheritRelation; dispid 421;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function AddUseCaseDiagram(const szName: WideString): IRoseClassDiagram; dispid 12699;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetIconIndex: Smallint; dispid 12824;
    function GetContext: IRoseItem; dispid 12872;
    function OpenSpecification: WordBool; dispid 216;
    function AddClassDiagram(const szName: WideString): IRoseClassDiagram; dispid 416;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    procedure CreateStateMachine; dispid 12700;
    function OpenCustomSpecification: WordBool; dispid 12728;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Rank: WideString dispid 427;
    property StateMachine: IRoseStateMachine dispid 420;
    property ScenarioDiagrams: IRoseScenarioDiagramCollection dispid 415;
    property ClassDiagrams: IRoseClassDiagramCollection dispid 414;
    property ParentCategory: IRoseCategory dispid 413;
    property Abstract: WordBool dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseDiagramCollection
// Flags:     (4096) Dispatchable
// GUID:      {38E8FEC2-969A-11D3-92AA-004005141253}
// *********************************************************************//
  IRoseDiagramCollection = dispinterface
    ['{38E8FEC2-969A-11D3-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseDiagram; dispid 203;
    function Exists(const pObject: IRoseDiagram): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseDiagram): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseDiagram); dispid 210;
    procedure Add(const theObject: IRoseDiagram); dispid 208;
    function GetFirst(const Name: WideString): IRoseDiagram; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseDiagram; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseLineVertexCollection
// Flags:     (4096) Dispatchable
// GUID:      {11A235B2-3095-11D2-8153-00104B97EBD5}
// *********************************************************************//
  IRoseLineVertexCollection = dispinterface
    ['{11A235B2-3095-11D2-8153-00104B97EBD5}']
    function GetAt(Index: Smallint): IRoseLineVertex; dispid 203;
    function Exists(const pObject: IRoseLineVertex): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseLineVertex): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseLineVertexCollection); dispid 209;
    procedure Remove(const theObject: IRoseLineVertex); dispid 210;
    procedure Add(const theObject: IRoseLineVertex); dispid 208;
    function GetFirst(const Name: WideString): IRoseLineVertex; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseLineVertex; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseInstantiateRelationCollection
// Flags:     (4096) Dispatchable
// GUID:      {B91D8F06-DDBB-11D1-9FAD-0060975306FE}
// *********************************************************************//
  IRoseInstantiateRelationCollection = dispinterface
    ['{B91D8F06-DDBB-11D1-9FAD-0060975306FE}']
    function GetAt(Index: Smallint): IRoseInstantiateRelation; dispid 203;
    function Exists(const pObject: IRoseInstantiateRelation): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseInstantiateRelation): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseInstantiateRelationCollection); dispid 209;
    procedure Remove(const theObject: IRoseInstantiateRelation); dispid 210;
    procedure Add(const theObject: IRoseInstantiateRelation); dispid 208;
    function GetFirst(const Name: WideString): IRoseInstantiateRelationCollection; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseInstantiateRelation; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseRealizeRelationCollection
// Flags:     (4096) Dispatchable
// GUID:      {67448181-4553-11D1-883B-3C8B00C10000}
// *********************************************************************//
  IRoseRealizeRelationCollection = dispinterface
    ['{67448181-4553-11D1-883B-3C8B00C10000}']
    function GetAt(Index: Smallint): IRoseRealizeRelation; dispid 203;
    function Exists(const pObject: IRoseRealizeRelation): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseRealizeRelation): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseRealizeRelationCollection); dispid 209;
    procedure Remove(const theObject: IRoseRealizeRelation); dispid 210;
    procedure Add(const theObject: IRoseRealizeRelation); dispid 208;
    function GetFirst(const Name: WideString): IRoseRealizeRelation; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseRealizeRelation; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseItemCollection
// Flags:     (4096) Dispatchable
// GUID:      {0DD9ACF8-D06E-11D0-BC0B-00A024C67143}
// *********************************************************************//
  IRoseItemCollection = dispinterface
    ['{0DD9ACF8-D06E-11D0-BC0B-00A024C67143}']
    function GetAt(Index: Smallint): IRoseItem; dispid 203;
    function Exists(const pObject: IRoseItem): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseItem): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseItemCollection); dispid 209;
    procedure Remove(const theObject: IRoseItem); dispid 210;
    procedure Add(const theObject: IRoseItem); dispid 208;
    function GetFirst(const Name: WideString): IRoseItem; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseItem; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseControllableUnitCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38360-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseControllableUnitCollection = dispinterface
    ['{97B38360-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseControllableUnit; dispid 203;
    function Exists(const pObject: IRoseControllableUnit): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseControllableUnit): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseControllableUnitCollection); dispid 209;
    procedure Remove(const theObject: IRoseControllableUnit); dispid 210;
    procedure Add(const theObject: IRoseControllableUnit); dispid 208;
    function GetFirst(const Name: WideString): IRoseControllableUnit; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseControllableUnit; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRosePackageCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38364-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRosePackageCollection = dispinterface
    ['{97B38364-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRosePackage; dispid 203;
    function Exists(const pObject: IRosePackage): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRosePackage): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRosePackageCollection); dispid 209;
    procedure Remove(const theObject: IRosePackage); dispid 210;
    procedure Add(const theObject: IRosePackage); dispid 208;
    function GetFirst(const Name: WideString): IRosePackage; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRosePackage; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseNoteViewCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38358-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseNoteViewCollection = dispinterface
    ['{97B38358-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseNoteView; dispid 203;
    function Exists(const pObject: IRoseNoteView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseNoteView): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseNoteViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseNoteView); dispid 210;
    procedure Add(const theObject: IRoseNoteView); dispid 208;
    function GetFirst(const Name: WideString): IRoseNoteView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseNoteView; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseExternalDocumentCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38357-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseExternalDocumentCollection = dispinterface
    ['{97B38357-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseExternalDocument; dispid 203;
    function Exists(const pObject: IRoseExternalDocument): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseExternalDocument): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseExternalDocumentCollection); dispid 209;
    procedure Remove(const theObject: IRoseExternalDocument); dispid 210;
    procedure Add(const theObject: IRoseExternalDocument); dispid 208;
    function GetFirst(const Name: WideString): IRoseExternalDocument; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseExternalDocument; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseUseCaseCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38356-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseUseCaseCollection = dispinterface
    ['{97B38356-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseUseCase; dispid 203;
    function Exists(const pObject: IRoseUseCase): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseUseCase): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseUseCaseCollection); dispid 209;
    procedure Remove(const theObject: IRoseUseCase); dispid 210;
    procedure Add(const theObject: IRoseUseCase); dispid 208;
    function GetFirst(const Name: WideString): IRoseUseCase; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseUseCase; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseProcessCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38366-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseProcessCollection = dispinterface
    ['{97B38366-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseProcess; dispid 203;
    function Exists(const pObject: IRoseProcess): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseProcess): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseProcessCollection); dispid 209;
    procedure Remove(const theObject: IRoseProcess); dispid 210;
    procedure Add(const theObject: IRoseProcess); dispid 208;
    function GetFirst(const Name: WideString): IRoseProcess; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseProcess; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseModuleVisibilityRelationshipCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38363-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseModuleVisibilityRelationshipCollection = dispinterface
    ['{97B38363-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseModuleVisibilityRelationship; dispid 203;
    function Exists(const pObject: IRoseModuleVisibilityRelationship): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseModuleVisibilityRelationship): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseModuleVisibilityRelationshipCollection); dispid 209;
    procedure Remove(const theObject: IRoseModuleVisibilityRelationship); dispid 210;
    procedure Add(const theObject: IRoseModuleVisibilityRelationship); dispid 208;
    function GetFirst(const Name: WideString): IRoseModuleVisibilityRelationship; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseModuleVisibilityRelationship; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseItemViewCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38362-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseItemViewCollection = dispinterface
    ['{97B38362-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseItemView; dispid 203;
    function Exists(const pObject: IRoseItemView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseItemView): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseItemViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseItemView); dispid 210;
    procedure Add(const theObject: IRoseItemView); dispid 208;
    function GetFirst(const Name: WideString): IRoseItemView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseItemView; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseScenarioDiagramCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B3835E-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseScenarioDiagramCollection = dispinterface
    ['{97B3835E-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseScenarioDiagram; dispid 203;
    function Exists(const pObject: IRoseScenarioDiagram): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseScenarioDiagram): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseScenarioDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseScenarioDiagram); dispid 210;
    procedure Add(const theObject: IRoseScenarioDiagram); dispid 208;
    function GetFirst(const Name: WideString): IRoseScenarioDiagram; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseScenarioDiagram; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRosePropertyCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B3835D-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRosePropertyCollection = dispinterface
    ['{97B3835D-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseProperty; dispid 203;
    function Exists(const pObject: IRoseProperty): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseProperty): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRosePropertyCollection); dispid 209;
    procedure Remove(const theObject: IRoseProperty); dispid 210;
    procedure Add(const theObject: IRoseProperty); dispid 208;
    function GetFirst(const Name: WideString): IRoseProperty; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseProperty; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseProcessorCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B3835C-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseProcessorCollection = dispinterface
    ['{97B3835C-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseProcessor; dispid 203;
    function Exists(const pObject: IRoseProcessor): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseProcessor): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseProcessorCollection); dispid 209;
    procedure Remove(const theObject: IRoseProcessor); dispid 210;
    procedure Add(const theObject: IRoseProcessor); dispid 208;
    function GetFirst(const Name: WideString): IRoseProcessor; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseProcessor; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseObjectInstanceCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B3835A-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseObjectInstanceCollection = dispinterface
    ['{97B3835A-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseObjectInstance; dispid 203;
    function Exists(const pObject: IRoseObjectInstance): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseObjectInstance): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseObjectInstanceCollection); dispid 209;
    procedure Remove(const theObject: IRoseObjectInstance); dispid 210;
    procedure Add(const theObject: IRoseObjectInstance); dispid 208;
    function GetFirst(const Name: WideString): IRoseObjectInstance; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseObjectInstance; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseMessageCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38359-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseMessageCollection = dispinterface
    ['{97B38359-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseMessage; dispid 203;
    function Exists(const pObject: IRoseMessage): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseMessage): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseMessageCollection); dispid 209;
    procedure Remove(const theObject: IRoseMessage); dispid 210;
    procedure Add(const theObject: IRoseMessage); dispid 208;
    function GetFirst(const Name: WideString): IRoseMessage; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseMessage; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseInheritRelationCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38354-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseInheritRelationCollection = dispinterface
    ['{97B38354-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseInheritRelation; dispid 203;
    function Exists(const pObject: IRoseInheritRelation): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseInheritRelation): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseInheritRelationCollection); dispid 209;
    procedure Remove(const theObject: IRoseInheritRelation); dispid 210;
    procedure Add(const theObject: IRoseInheritRelation); dispid 208;
    function GetFirst(const Name: WideString): IRoseInheritRelation; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseInheritRelation; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseRoleCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38353-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseRoleCollection = dispinterface
    ['{97B38353-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseRole; dispid 203;
    function Exists(const pObject: IRoseRole): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseRole): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseRoleCollection); dispid 209;
    procedure Remove(const theObject: IRoseRole); dispid 210;
    procedure Add(const theObject: IRoseRole); dispid 208;
    function GetFirst(const Name: WideString): IRoseRole; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseRole; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseParameterCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38352-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseParameterCollection = dispinterface
    ['{97B38352-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseParameter; dispid 203;
    function Exists(const pObject: IRoseParameter): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseParameter): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseParameterCollection); dispid 209;
    procedure Remove(const theObject: IRoseParameter); dispid 210;
    procedure Add(const theObject: IRoseParameter); dispid 208;
    function GetFirst(const Name: WideString): IRoseParameter; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseParameter; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseHasRelationshipCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38351-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseHasRelationshipCollection = dispinterface
    ['{97B38351-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseHasRelationship; dispid 203;
    function Exists(const pObject: IRoseHasRelationship): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseHasRelationship): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseHasRelationshipCollection); dispid 209;
    procedure Remove(const theObject: IRoseHasRelationship); dispid 210;
    procedure Add(const theObject: IRoseHasRelationship); dispid 208;
    function GetFirst(const Name: WideString): IRoseHasRelationship; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseHasRelationship; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseAssociationCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B3834E-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseAssociationCollection = dispinterface
    ['{97B3834E-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseAssociation; dispid 203;
    function Exists(const pObject: IRoseAssociation): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseAssociation): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseAssociationCollection); dispid 209;
    procedure Remove(const theObject: IRoseAssociation); dispid 210;
    procedure Add(const theObject: IRoseAssociation); dispid 208;
    function GetFirst(const Name: WideString): IRoseAssociation; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseAssociation; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseOperationCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B3834D-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseOperationCollection = dispinterface
    ['{97B3834D-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseOperation; dispid 203;
    function Exists(const pObject: IRoseOperation): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseOperation): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseOperationCollection); dispid 209;
    procedure Remove(const theObject: IRoseOperation); dispid 210;
    procedure Add(const theObject: IRoseOperation); dispid 208;
    function GetFirst(const Name: WideString): IRoseOperation; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseOperation; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseAttributeCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B3834C-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseAttributeCollection = dispinterface
    ['{97B3834C-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseAttribute; dispid 203;
    function Exists(const pObject: IRoseAttribute): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseAttribute): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseAttributeCollection); dispid 209;
    procedure Remove(const theObject: IRoseAttribute); dispid 210;
    procedure Add(const theObject: IRoseAttribute); dispid 208;
    function GetFirst(const Name: WideString): IRoseAttribute; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseAttribute; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseModuleCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B3834B-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseModuleCollection = dispinterface
    ['{97B3834B-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseModule; dispid 203;
    function Exists(const pObject: IRoseModule): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseModule): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseModuleCollection); dispid 209;
    procedure Remove(const theObject: IRoseModule); dispid 210;
    procedure Add(const theObject: IRoseModule); dispid 208;
    function GetFirst(const Name: WideString): IRoseModule; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseModule; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseSubsystemCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B3834A-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseSubsystemCollection = dispinterface
    ['{97B3834A-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseSubsystem; dispid 203;
    function Exists(const pObject: IRoseSubsystem): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseSubsystem): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseSubsystemCollection); dispid 209;
    procedure Remove(const theObject: IRoseSubsystem); dispid 210;
    procedure Add(const theObject: IRoseSubsystem); dispid 208;
    function GetFirst(const Name: WideString): IRoseSubsystem; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseSubsystem; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseCategoryCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B3835B-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseCategoryCollection = dispinterface
    ['{97B3835B-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseCategory; dispid 203;
    function Exists(const pObject: IRoseCategory): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseCategory): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseCategoryCollection); dispid 209;
    procedure Remove(const theObject: IRoseCategory); dispid 210;
    procedure Add(const theObject: IRoseCategory); dispid 208;
    function GetFirst(const Name: WideString): IRoseCategory; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseCategory; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseClassCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38349-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseClassCollection = dispinterface
    ['{97B38349-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseClass; dispid 203;
    function Exists(const pObject: IRoseClass): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseClass): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseClassCollection); dispid 209;
    procedure Remove(const theObject: IRoseClass); dispid 210;
    procedure Add(const theObject: IRoseClass); dispid 208;
    function GetFirst(const Name: WideString): IRoseClass; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseClass; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseModuleDiagramCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38348-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseModuleDiagramCollection = dispinterface
    ['{97B38348-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseModuleDiagram; dispid 203;
    function Exists(const pObject: IRoseModuleDiagram): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseModuleDiagram): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseModuleDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseModuleDiagram); dispid 210;
    procedure Add(const theObject: IRoseModuleDiagram); dispid 208;
    function GetFirst(const Name: WideString): IRoseModuleDiagram; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseModuleDiagram; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseClassDiagramCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38343-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseClassDiagramCollection = dispinterface
    ['{97B38343-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseClassDiagram; dispid 203;
    function Exists(const pObject: IRoseClassDiagram): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseClassDiagram): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseClassDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseClassDiagram); dispid 210;
    procedure Add(const theObject: IRoseClassDiagram); dispid 208;
    function GetFirst(const Name: WideString): IRoseClassDiagram; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseClassDiagram; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseDeviceCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38342-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseDeviceCollection = dispinterface
    ['{97B38342-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseDevice; dispid 203;
    function Exists(const pObject: IRoseDevice): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseDevice): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseDeviceCollection); dispid 209;
    procedure Remove(const theObject: IRoseDevice); dispid 210;
    procedure Add(const theObject: IRoseDevice); dispid 208;
    function GetFirst(const Name: WideString): IRoseDevice; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseDevice; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseDeploymentDiagramCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B383A1-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseDeploymentDiagramCollection = dispinterface
    ['{97B383A1-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseDeploymentDiagram; dispid 203;
    function Exists(const pObject: IRoseDeploymentDiagram): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseDeploymentDiagram): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseDeploymentDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseDeploymentDiagram); dispid 210;
    procedure Add(const theObject: IRoseDeploymentDiagram); dispid 208;
    function GetFirst(const Name: WideString): IRoseDeploymentDiagram; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseDeploymentDiagram; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseClassViewCollection
// Flags:     (4096) Dispatchable
// GUID:      {97B38341-A4E3-11D0-BFF0-00AA003DEF5B}
// *********************************************************************//
  IRoseClassViewCollection = dispinterface
    ['{97B38341-A4E3-11D0-BFF0-00AA003DEF5B}']
    function GetAt(Index: Smallint): IRoseClassView; dispid 203;
    function Exists(const pObject: IRoseClassView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseClassView): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseClassViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseClassView); dispid 210;
    procedure Add(const theObject: IRoseClassView); dispid 208;
    function GetFirst(const Name: WideString): IRoseClassView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseClassView; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseDeploymentDiagram
// Flags:     (4096) Dispatchable
// GUID:      {C2C15EC4-E028-11CF-B091-00A0241E3F73}
// *********************************************************************//
  IRoseDeploymentDiagram = dispinterface
    ['{C2C15EC4-E028-11CF-B091-00A0241E3F73}']
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function AddRelationView(const theRelation: IRoseRelation): WordBool; dispid 12787;
    function GetQualifiedName: WideString; dispid 12555;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function IsActive: WordBool; dispid 209;
    function IdentifyClass: WideString; dispid 12668;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    procedure Update; dispid 206;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetPropertyClassName: WideString; dispid 128;
    function GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    procedure Invalidate; dispid 205;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Activate; dispid 211;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetNoteViews: IRoseNoteViewCollection; dispid 220;
    function RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function Exists(const theItem: IRoseItem): WordBool; dispid 210;
    function GetDevices: IRoseDeviceCollection; dispid 412;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function AddProcessor(const theProcessor: IRoseProcessor; x: Smallint; y: Smallint): IRoseItemView; dispid 413;
    function AddDevice(const theDevice: IRoseDevice; x: Smallint; y: Smallint): IRoseItemView; dispid 414;
    function GetProcessors: IRoseProcessorCollection; dispid 411;
    function RenderIconToClipboard: WordBool; dispid 12820;
    procedure AutosizeAll; dispid 12862;
    function LayoutSelectedViews(StackClasses: WordBool; ClassesPerRow: Smallint; DistX: Smallint; 
                                 DistY: Smallint; UpperLeftX: Smallint; UpperLeftY: Smallint): WordBool; dispid 12873;
    function RemoveProcessor(const theProcessor: IRoseProcessor): WordBool; dispid 415;
    function RemoveDevice(const theDevice: IRoseDevice): WordBool; dispid 416;
    function GetSelectedItems: IRoseItemCollection; dispid 12525;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RemoveItemView(const theItemView: IRoseItemView): WordBool; dispid 12832;
    procedure Render(const FileName: WideString); dispid 217;
    function AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function CenterDiagramToView(const theView: IRoseItemView): WordBool; dispid 12861;
    function GetParentContext: IRoseItem; dispid 12823;
    function GetIconIndex: Smallint; dispid 12824;
    property ZoomFactor: Smallint dispid 12690;
    property Documentation: WideString dispid 12656;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Items: IRoseItemCollection dispid 208;
    property Visible: WordBool dispid 203;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseProcess
// Flags:     (4096) Dispatchable
// GUID:      {62C43884-DB5A-11CF-B091-00A0241E3F73}
// *********************************************************************//
  IRoseProcess = dispinterface
    ['{62C43884-DB5A-11CF-B091-00A0241E3F73}']
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IdentifyClass: WideString; dispid 12668;
    function GetQualifiedName: WideString; dispid 12555;
    function OpenSpecification: WordBool; dispid 216;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetRoseItem: IRoseItem; dispid 207;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetContext: IRoseItem; dispid 12872;
    function GetIconIndex: Smallint; dispid 12824;
    function GetPropertyClassName: WideString; dispid 128;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Priority: WideString dispid 413;
    property MyProcessor: IRoseProcessor dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseDevice
// Flags:     (4096) Dispatchable
// GUID:      {62C43882-DB5A-11CF-B091-00A0241E3F73}
// *********************************************************************//
  IRoseDevice = dispinterface
    ['{62C43882-DB5A-11CF-B091-00A0241E3F73}']
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function RemoveDeviceConnection(const theDevice: IRoseDevice): WordBool; dispid 418;
    function RemoveProcessorConnection(const theProcessor: IRoseProcessor): WordBool; dispid 416;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function GetUniqueID: WideString; dispid 102;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function AddDeviceConnection(const theDevice: IRoseDevice): WordBool; dispid 417;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetConnectedDevices: IRoseDeviceCollection; dispid 414;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function AddProcessorConnection(const theProcessor: IRoseProcessor): WordBool; dispid 415;
    function GetConnectedProcessors: IRoseProcessorCollection; dispid 413;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetIconIndex: Smallint; dispid 12824;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetContext: IRoseItem; dispid 12872;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function RenderIconToClipboard: WordBool; dispid 12820;
    property Connections: IRoseConnectionRelationCollection dispid 12818;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Characteristics: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseProcessor
// Flags:     (4096) Dispatchable
// GUID:      {62C43886-DB5A-11CF-B091-00A0241E3F73}
// *********************************************************************//
  IRoseProcessor = dispinterface
    ['{62C43886-DB5A-11CF-B091-00A0241E3F73}']
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function RemoveDeviceConnection(const theDevice: IRoseDevice): WordBool; dispid 422;
    function AddProcessorConnection(const Processor: IRoseProcessor): WordBool; dispid 419;
    function RemoveProcessorConnection(const theProcessor: IRoseProcessor): WordBool; dispid 420;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function AddDeviceConnection(const theDevice: IRoseDevice): WordBool; dispid 421;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetConnectedProcessors: IRoseProcessorCollection; dispid 416;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function AddProcess(const Name: WideString): IRoseProcess; dispid 417;
    function OpenSpecification: WordBool; dispid 216;
    function GetConnectedDevices: IRoseDeviceCollection; dispid 415;
    function DeleteProcess(const theProcess: IRoseProcess): WordBool; dispid 418;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetIconIndex: Smallint; dispid 12824;
    function GetContext: IRoseItem; dispid 12872;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    property Connections: IRoseConnectionRelationCollection dispid 12819;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Scheduling: IRoseRichType dispid 414;
    property Characteristics: WideString dispid 413;
    property Processes: IRoseProcessCollection dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseInstanceViewCollection
// Flags:     (4096) Dispatchable
// GUID:      {C640C864-F2D3-11D0-883A-3C8B00C10000}
// *********************************************************************//
  IRoseInstanceViewCollection = dispinterface
    ['{C640C864-F2D3-11D0-883A-3C8B00C10000}']
    function GetAt(Index: Smallint): IRoseInstanceView; dispid 203;
    function Exists(const pObject: IRoseInstanceView): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseInstanceView): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseInstanceViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseInstanceView); dispid 210;
    procedure Add(const theObject: IRoseInstanceView); dispid 208;
    function GetFirst(const Name: WideString): IRoseInstanceView; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseInstanceView; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseInstanceView
// Flags:     (4096) Dispatchable
// GUID:      {348B1AD4-D5C4-11D0-89F8-0020AFD6C181}
// *********************************************************************//
  IRoseInstanceView = dispinterface
    ['{348B1AD4-D5C4-11D0-89F8-0020AFD6C181}']
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetInstance: IRoseObjectInstance; dispid 409;
    function GetMinHeight: Smallint; dispid 218;
    function GetQualifiedName: WideString; dispid 12555;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function GetIconIndex: Smallint; dispid 12824;
    procedure Invalidate; dispid 207;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function SupportsLineColor: WordBool; dispid 211;
    function IsSelected: WordBool; dispid 212;
    function GetDefaultWidth: Smallint; dispid 215;
    function SupportsFillColor: WordBool; dispid 210;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetAttachedNotes: IRoseNoteViewCollection; dispid 12829;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    property StereotypeDisplay: Smallint dispid 12837;
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Font: IRoseView_Font dispid 12493;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Item: IRoseItem dispid 221;
    property ParentView: IRoseItemView dispid 220;
    property SubViews: IRoseItemViewCollection dispid 219;
    property LineColor: IRoseView_LineColor dispid 208;
    property FillColor: IRoseView_FillColor dispid 206;
    property Width: Smallint dispid 205;
    property Height: Smallint dispid 204;
    property XPosition: Smallint dispid 203;
    property YPosition: Smallint dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseLinkCollection
// Flags:     (4096) Dispatchable
// GUID:      {9DE9A9C1-F2D0-11D0-883A-3C8B00C10000}
// *********************************************************************//
  IRoseLinkCollection = dispinterface
    ['{9DE9A9C1-F2D0-11D0-883A-3C8B00C10000}']
    function GetAt(Index: Smallint): IRoseLink; dispid 203;
    function Exists(const pObject: IRoseLink): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseLink): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseLinkCollection); dispid 209;
    procedure Remove(const theObject: IRoseLink); dispid 210;
    procedure Add(const theObject: IRoseLink); dispid 208;
    function GetFirst(const Name: WideString): IRoseLink; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseLink; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseLink
// Flags:     (4096) Dispatchable
// GUID:      {195D7852-D5B6-11D0-89F8-0020AFD6C181}
// *********************************************************************//
  IRoseLink = dispinterface
    ['{195D7852-D5B6-11D0-89F8-0020AFD6C181}']
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function UnassignAssociation: WordBool; dispid 421;
    function AddMessageTo(const Name: WideString; const ToInstance: IRoseObjectInstance; 
                          SequenceNumber: Smallint): IRoseMessage; dispid 422;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function AssignAssociation(const TheAssoc: IRoseAssociation): WordBool; dispid 420;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetUniqueID: WideString; dispid 102;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetIconIndex: Smallint; dispid 12824;
    function OpenSpecification: WordBool; dispid 216;
    function GetMessages: IRoseMessageCollection; dispid 416;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function DeleteMessage(const TheMessage: IRoseMessage): WordBool; dispid 419;
    function GetRoseItem: IRoseItem; dispid 207;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetAssociation: IRoseAssociation; dispid 12776;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property LinkRole2Shared: WordBool dispid 418;
    property LinkRole1Visibility: IRoseRichType dispid 417;
    property LinkRole2Visibility: IRoseRichType dispid 415;
    property LinkRole1Shared: WordBool dispid 414;
    property LinkRole2: IRoseObjectInstance dispid 413;
    property LinkRole1: IRoseObjectInstance dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseScenarioDiagram
// Flags:     (4096) Dispatchable
// GUID:      {F819833A-FC55-11CF-BBD3-00A024C67143}
// *********************************************************************//
  IRoseScenarioDiagram = dispinterface
    ['{F819833A-FC55-11CF-BBD3-00A024C67143}']
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetQualifiedName: WideString; dispid 12555;
    function AddRelationView(const theRelation: IRoseRelation): WordBool; dispid 12787;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    procedure Activate; dispid 211;
    function GetPropertyClassName: WideString; dispid 128;
    procedure Layout; dispid 204;
    function IsActive: WordBool; dispid 209;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    procedure Update; dispid 206;
    function Exists(const theItem: IRoseItem): WordBool; dispid 210;
    function GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    procedure Invalidate; dispid 205;
    function DeleteInstance(const theInstance: IRoseObjectInstance): WordBool; dispid 421;
    function AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    procedure Render(const FileName: WideString); dispid 217;
    function CreateMessage(const theName: WideString; const theSender: IRoseObjectInstance; 
                           const theReceiver: IRoseObjectInstance; theSequence: Smallint): IRoseMessage; dispid 416;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    procedure RenderEnhancedToClipboard; dispid 223;
    function GetMessages: IRoseMessageCollection; dispid 413;
    function GetSelectedObjects: IRoseObjectInstanceCollection; dispid 412;
    function GetObjects: IRoseObjectInstanceCollection; dispid 411;
    function GetSelectedMessages: IRoseMessageCollection; dispid 414;
    function GetDiagramType: Smallint; dispid 418;
    function GetSelectedLinks: IRoseLinkCollection; dispid 417;
    function CenterDiagramToView(const theView: IRoseItemView): WordBool; dispid 12861;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function LayoutSelectedViews(StackClasses: WordBool; ClassesPerRow: Smallint; DistX: Smallint; 
                                 DistY: Smallint; UpperLeftX: Smallint; UpperLeftY: Smallint): WordBool; dispid 12873;
    procedure AutosizeAll; dispid 12862;
    function RemoveInstanceView(const theView: IRoseInstanceView): WordBool; dispid 420;
    function GetSelectedItems: IRoseItemCollection; dispid 12525;
    function AddInstance(const theName: WideString; const theClassName: WideString): IRoseObjectInstance; dispid 422;
    function AddInstanceView(const theInstance: IRoseObjectInstance; AsClassInstance: WordBool): IRoseInstanceView; dispid 419;
    function GetNoteViews: IRoseNoteViewCollection; dispid 220;
    function RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function GetIconIndex: Smallint; dispid 12824;
    function GetParentContext: IRoseItem; dispid 12823;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function RemoveItemView(const theItemView: IRoseItemView): WordBool; dispid 12832;
    property ZoomFactor: Smallint dispid 12690;
    property Documentation: WideString dispid 12656;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property InstanceViews: IRoseInstanceViewCollection dispid 423;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Items: IRoseItemCollection dispid 208;
    property Visible: WordBool dispid 203;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseMessage
// Flags:     (4096) Dispatchable
// GUID:      {F819833C-FC55-11CF-BBD3-00A024C67143}
// *********************************************************************//
  IRoseMessage = dispinterface
    ['{F819833C-FC55-11CF-BBD3-00A024C67143}']
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function IdentifyClass: WideString; dispid 12668;
    function GetOperation: IRoseOperation; dispid 416;
    function GetLink: IRoseLink; dispid 417;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetQualifiedName: WideString; dispid 12555;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetPropertyClassName: WideString; dispid 128;
    function GetReceiverObject: IRoseObjectInstance; dispid 413;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function IsMessageToSelf: WordBool; dispid 414;
    function GetSenderObject: IRoseObjectInstance; dispid 412;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetSequenceInformation: WideString; dispid 12825;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetContext: IRoseItem; dispid 12872;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function GetIconIndex: Smallint; dispid 12824;
    function IsOperation: WordBool; dispid 415;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Frequency: Smallint dispid 12653;
    property Synchronization: Smallint dispid 12652;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseObjectInstance
// Flags:     (4096) Dispatchable
// GUID:      {F8198337-FC55-11CF-BBD3-00A024C67143}
// *********************************************************************//
  IRoseObjectInstance = dispinterface
    ['{F8198337-FC55-11CF-BBD3-00A024C67143}']
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function DeleteLink(const aLink: IRoseLink): WordBool; dispid 418;
    function GetQualifiedName: WideString; dispid 12555;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function IdentifyClass: WideString; dispid 12668;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetIconIndex: Smallint; dispid 12824;
    function GetRoseItem: IRoseItem; dispid 207;
    function IsClass: WordBool; dispid 414;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetClass: IRoseClass; dispid 415;
    function OpenSpecification: WordBool; dispid 216;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetObjectFlows: IRoseObjectFlowCollection; dispid 12854;
    function AddObjectFlow(const theActivity: IRoseActivity): IRoseObjectFlow; dispid 12843;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function DeleteObjectFlow(const theObjectFlow: IRoseObjectFlow): WordBool; dispid 12844;
    function AddLink(const Name: WideString; const ToInstance: IRoseObjectInstance): IRoseLink; dispid 417;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Persistence: Smallint dispid 12651;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Links: IRoseLinkCollection dispid 416;
    property MultipleInstances: WordBool dispid 413;
    property _className: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseConnectionRelationCollection
// Flags:     (4096) Dispatchable
// GUID:      {4467F446-F24E-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseConnectionRelationCollection = dispinterface
    ['{4467F446-F24E-11D2-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseConnectionRelation; dispid 203;
    function Exists(const pObject: IRoseConnectionRelation): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseConnectionRelation): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseConnectionRelationCollection); dispid 209;
    procedure Remove(const theObject: IRoseConnectionRelation); dispid 210;
    procedure Add(const theObject: IRoseConnectionRelation); dispid 208;
    function GetFirst(const Name: WideString): IRoseConnectionRelation; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseConnectionRelation; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseConnectionRelation
// Flags:     (4096) Dispatchable
// GUID:      {4467F442-F24E-11D2-92AA-004005141253}
// *********************************************************************//
  IRoseConnectionRelation = dispinterface
    ['{4467F442-F24E-11D2-92AA-004005141253}']
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetSupplier: IRoseItem; dispid 12609;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function IdentifyClass: WideString; dispid 12668;
    function GetClient: IRoseItem; dispid 12608;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function OpenSpecification: WordBool; dispid 216;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetQualifiedName: WideString; dispid 12555;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetIconIndex: Smallint; dispid 12824;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetPropertyClassName: WideString; dispid 128;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    property SupplierIsDevice: WordBool dispid 12816;
    property Characteristics: WideString dispid 12815;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseInstantiateRelation
// Flags:     (4096) Dispatchable
// GUID:      {B91D8F03-DDBB-11D1-9FAD-0060975306FE}
// *********************************************************************//
  IRoseInstantiateRelation = dispinterface
    ['{B91D8F03-DDBB-11D1-9FAD-0060975306FE}']
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetClient: IRoseItem; dispid 12608;
    function HasClient: WordBool; dispid 12606;
    function GetSupplier: IRoseItem; dispid 12609;
    function IdentifyClass: WideString; dispid 12668;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function HasSupplier: WordBool; dispid 12607;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetContextClass: IRoseClass; dispid 12600;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetSupplierClass: IRoseClass; dispid 12601;
    function GetQualifiedName: WideString; dispid 12555;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetIconIndex: Smallint; dispid 12824;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetContext: IRoseItem; dispid 12872;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function RenderIconToClipboard: WordBool; dispid 12820;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseClassDependencyCollection
// Flags:     (4096) Dispatchable
// GUID:      {ED042E4F-6CDE-11D1-BC1E-00A024C67143}
// *********************************************************************//
  IRoseClassDependencyCollection = dispinterface
    ['{ED042E4F-6CDE-11D1-BC1E-00A024C67143}']
    function GetAt(Index: Smallint): IRoseClassDependency; dispid 203;
    function Exists(const pObject: IRoseClassDependency): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IDispatch): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseClassDependencyCollection); dispid 209;
    procedure Remove(const theObject: IRoseClassDependency); dispid 210;
    procedure Add(const theObject: IRoseClassDependency); dispid 208;
    function GetFirst(const Name: WideString): IRoseClassDependency; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseClassDependency; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseCategoryDependencyCollection
// Flags:     (4096) Dispatchable
// GUID:      {4ACE189D-6CD3-11D1-BC1E-00A024C67143}
// *********************************************************************//
  IRoseCategoryDependencyCollection = dispinterface
    ['{4ACE189D-6CD3-11D1-BC1E-00A024C67143}']
    function GetAt(Index: Smallint): IRoseCategoryDependency; dispid 203;
    function Exists(const pObject: IRoseCategoryDependency): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IDispatch): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseCategoryDependencyCollection); dispid 209;
    procedure Remove(const theObject: IRoseCategoryDependency); dispid 210;
    procedure Add(const theObject: IRoseCategoryDependency); dispid 208;
    function GetFirst(const Name: WideString): IRoseCategoryDependency; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseCategoryDependency; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseClassDependency
// Flags:     (4096) Dispatchable
// GUID:      {4ACE1899-6CD3-11D1-BC1E-00A024C67143}
// *********************************************************************//
  IRoseClassDependency = dispinterface
    ['{4ACE1899-6CD3-11D1-BC1E-00A024C67143}']
    function GetClient: IRoseItem; dispid 12608;
    function GetSupplier: IRoseItem; dispid 12609;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function IdentifyClass: WideString; dispid 12668;
    function GetSupplierClass: IRoseClass; dispid 12601;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetIconIndex: Smallint; dispid 12824;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetContextClass: IRoseClass; dispid 12600;
    function GetRoseItem: IRoseItem; dispid 207;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ExportControl: IRoseRichType dispid 820;
    property InvolvesFriendship: WordBool dispid 819;
    property SupplierCardinality: WideString dispid 818;
    property ClientCardinality: WideString dispid 817;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseCategoryDependency
// Flags:     (4096) Dispatchable
// GUID:      {4ACE189B-6CD3-11D1-BC1E-00A024C67143}
// *********************************************************************//
  IRoseCategoryDependency = dispinterface
    ['{4ACE189B-6CD3-11D1-BC1E-00A024C67143}']
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetContextCategory: IRoseCategory; dispid 12657;
    function GetClient: IRoseItem; dispid 12608;
    function GetSupplierCategory: IRoseCategory; dispid 12658;
    function IdentifyClass: WideString; dispid 12668;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetSupplier: IRoseItem; dispid 12609;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function HasClient: WordBool; dispid 12606;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function HasSupplier: WordBool; dispid 12607;
    function GetQualifiedName: WideString; dispid 12555;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetIconIndex: Smallint; dispid 12824;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetContext: IRoseItem; dispid 12872;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function RenderIconToClipboard: WordBool; dispid 12820;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseClassRelation
// Flags:     (4096) Dispatchable
// GUID:      {00C99564-9200-11CF-B1B0-D227D5210B2C}
// *********************************************************************//
  IRoseClassRelation = dispinterface
    ['{00C99564-9200-11CF-B1B0-D227D5210B2C}']
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetClient: IRoseItem; dispid 12608;
    function HasClient: WordBool; dispid 12606;
    function GetSupplier: IRoseItem; dispid 12609;
    function IdentifyClass: WideString; dispid 12668;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function HasSupplier: WordBool; dispid 12607;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetContextClass: IRoseClass; dispid 12600;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function GetSupplierClass: IRoseClass; dispid 12601;
    function GetQualifiedName: WideString; dispid 12555;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetIconIndex: Smallint; dispid 12824;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetContext: IRoseItem; dispid 12872;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function RenderIconToClipboard: WordBool; dispid 12820;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseRealizeRelation
// Flags:     (4096) Dispatchable
// GUID:      {6AC2BA81-454D-11D1-883B-3C8B00C10000}
// *********************************************************************//
  IRoseRealizeRelation = dispinterface
    ['{6AC2BA81-454D-11D1-883B-3C8B00C10000}']
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetUniqueID: WideString; dispid 102;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetClient: IRoseItem; dispid 12608;
    function HasClient: WordBool; dispid 12606;
    function GetSupplier: IRoseItem; dispid 12609;
    function IdentifyClass: WideString; dispid 12668;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetPropertyClassName: WideString; dispid 128;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function HasSupplier: WordBool; dispid 12607;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetContextComponent: IRoseModule; dispid 12603;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function GetSupplierClass: IRoseClass; dispid 12604;
    function GetContextClass: IRoseClass; dispid 12602;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetIconIndex: Smallint; dispid 12824;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetContext: IRoseItem; dispid 12872;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetSupplierComponent: IRoseModule; dispid 12605;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseInheritRelation
// Flags:     (4096) Dispatchable
// GUID:      {00C99560-9200-11CF-B1B0-D227D5210B2C}
// *********************************************************************//
  IRoseInheritRelation = dispinterface
    ['{00C99560-9200-11CF-B1B0-D227D5210B2C}']
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetSupplier: IRoseItem; dispid 12609;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function IdentifyClass: WideString; dispid 12668;
    function GetClient: IRoseItem; dispid 12608;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetRoseItem: IRoseItem; dispid 207;
    function GetQualifiedName: WideString; dispid 12555;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetContextClass: IRoseClass; dispid 12600;
    function OpenSpecification: WordBool; dispid 216;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetIconIndex: Smallint; dispid 12824;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetSupplierClass: IRoseClass; dispid 12601;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ExportControl: IRoseRichType dispid 819;
    property FriendshipRequired: WordBool dispid 818;
    property Virtual: WordBool dispid 817;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseHasRelationship
// Flags:     (4096) Dispatchable
// GUID:      {BA242E04-8961-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseHasRelationship = dispinterface
    ['{BA242E04-8961-11CF-B3D4-00A0241DB1D0}']
    function GetClient: IRoseItem; dispid 12608;
    function GetSupplier: IRoseItem; dispid 12609;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function HasClient: WordBool; dispid 12606;
    function HasSupplier: WordBool; dispid 12607;
    function IdentifyClass: WideString; dispid 12668;
    function GetSupplierClass: IRoseClass; dispid 12601;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetUniqueID: WideString; dispid 102;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetIconIndex: Smallint; dispid 12824;
    function OpenSpecification: WordBool; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetContextClass: IRoseClass; dispid 12600;
    function GetRoseItem: IRoseItem; dispid 207;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Containment: IRoseRichType dispid 821;
    property ExportControl: IRoseRichType dispid 820;
    property Static: WordBool dispid 819;
    property SupplierCardinality: WideString dispid 818;
    property ClientCardinality: WideString dispid 817;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseRole
// Flags:     (4096) Dispatchable
// GUID:      {BA242E00-8961-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseRole = dispinterface
    ['{BA242E00-8961-11CF-B3D4-00A0241DB1D0}']
    function IsAssociateClass: WordBool; dispid 628;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function HasSupplier: WordBool; dispid 12607;
    function HasClient: WordBool; dispid 12606;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetSpecifier: WideString; dispid 12865;
    function GetSupplier: IRoseItem; dispid 12609;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetClient: IRoseItem; dispid 12608;
    function GetUniqueID: WideString; dispid 102;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetNameBase: WideString; dispid 12864;
    function OpenSpecification: WordBool; dispid 216;
    function AddKey(const theName: WideString; const theType: WideString): IRoseAttribute; dispid 623;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function DeleteKey(const theAttr: IRoseAttribute): WordBool; dispid 624;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetIconIndex: Smallint; dispid 12824;
    function GetClassName: WideString; dispid 625;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    property ShowSpecifier: WordBool dispid 12868;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Friend: WordBool dispid 12650;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Constraints: WideString dispid 629;
    property UseCase: IRoseUseCase dispid 627;
    property AssociateItem: IRoseItem dispid 626;
    property Keys: IRoseAttributeCollection dispid 622;
    property Class_: IRoseClass dispid 621;
    property Association: IRoseAssociation dispid 620;
    property Containment: IRoseRichType dispid 619;
    property ExportControl: IRoseRichType dispid 618;
    property Cardinality: WideString dispid 617;
    property Navigable: WordBool dispid 616;
    property Static: WordBool dispid 615;
    property Aggregate: WordBool dispid 614;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseAssociation
// Flags:     (4096) Dispatchable
// GUID:      {C78E7026-86E4-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseAssociation = dispinterface
    ['{C78E7026-86E4-11CF-B3D4-00A0241DB1D0}']
    procedure SetRoleForNameDirection(const theRole: IRoseRole); dispid 12648;
    procedure ClearRoleForNameDirection; dispid 12649;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function IdentifyClass: WideString; dispid 12668;
    function GetRoleForNameDirection: IRoseRole; dispid 12647;
    function GetIconIndex: Smallint; dispid 12824;
    function GetQualifiedName: WideString; dispid 12555;
    function NameIsDirectional: WordBool; dispid 12646;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function GetPropertyClassName: WideString; dispid 128;
    function OpenSpecification: WordBool; dispid 216;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetCorrespondingRole(const Class_: IRoseClass): IRoseRole; dispid 416;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetOtherRole(const Class_: IRoseClass): IRoseRole; dispid 417;
    function GetRoseItem: IRoseItem; dispid 207;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure SetLinkClassName(const theClassName: WideString); dispid 418;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    property ParentCategory: IRoseCategory dispid 12798;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Constraints: WideString dispid 12706;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Derived: WordBool dispid 419;
    property LinkClass: IRoseClass dispid 415;
    property Role2: IRoseRole dispid 414;
    property Role1: IRoseRole dispid 413;
    property Roles: IDispatch dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseParameter
// Flags:     (4096) Dispatchable
// GUID:      {C78E7028-86E4-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseParameter = dispinterface
    ['{C78E7028-86E4-11CF-B3D4-00A0241DB1D0}']
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetQualifiedName: WideString; dispid 12555;
    function OpenSpecification: WordBool; dispid 216;
    function IdentifyClass: WideString; dispid 12668;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetRoseItem: IRoseItem; dispid 207;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetTypeClass: IRoseClass; dispid 12877;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetPropertyClassName: WideString; dispid 128;
    function GetIconIndex: Smallint; dispid 12824;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property InitValue: WideString dispid 414;
    property type_: WideString dispid 413;
    property Const_: WordBool dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseOperation
// Flags:     (4096) Dispatchable
// GUID:      {C78E7020-86E4-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseOperation = dispinterface
    ['{C78E7020-86E4-11CF-B3D4-00A0241DB1D0}']
    procedure RemoveAllParameters; dispid 417;
    function DeleteParameter(const theParameter: IRoseParameter): WordBool; dispid 426;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function IdentifyClass: WideString; dispid 12668;
    function GetQualifiedName: WideString; dispid 12555;
    function GetDependencies: IRoseDependencyRelationCollection; dispid 12885;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetIconIndex: Smallint; dispid 12824;
    function DeleteDependency(const theDependency: IRoseDependencyRelation): WordBool; dispid 12884;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetUniqueID: WideString; dispid 102;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function AddDependency(const theSupplierName: WideString; const theSupplierType: IRoseAttribute): IRoseDependencyRelation; dispid 12883;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function AddParameter(const theName: WideString; const theType: WideString; 
                          const theDef: WideString; position: Smallint): IRoseParameter; dispid 416;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetExceptionClasses: IRoseClassCollection; dispid 12878;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetContext: IRoseItem; dispid 12872;
    function GetResultClass: IRoseClass; dispid 12876;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetPropertyClassName: WideString; dispid 128;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    property Abstract: WordBool dispid 12863;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ParentClass: IRoseClass dispid 444;
    property Virtual: WordBool dispid 427;
    property Time: WideString dispid 425;
    property Size: WideString dispid 424;
    property Exceptions: WideString dispid 423;
    property Qualification: WideString dispid 422;
    property Protocol: WideString dispid 421;
    property Postconditions: WideString dispid 420;
    property Semantics: WideString dispid 419;
    property Preconditions: WideString dispid 418;
    property Concurrency: IRoseRichType dispid 415;
    property ExportControl: IRoseRichType dispid 414;
    property Parameters: IRoseParameterCollection dispid 413;
    property ReturnType: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseAttribute
// Flags:     (4096) Dispatchable
// GUID:      {C78E7024-86E4-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseAttribute = dispinterface
    ['{C78E7024-86E4-11CF-B3D4-00A0241DB1D0}']
    function GetQualifiedName: WideString; dispid 12555;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetTypeClass: IRoseClass; dispid 12875;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function OpenSpecification: WordBool; dispid 216;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function GetRoseItem: IRoseItem; dispid 207;
    function GetPropertyClassName: WideString; dispid 128;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetIconIndex: Smallint; dispid 12824;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ParentClass: IRoseClass dispid 434;
    property Derived: WordBool dispid 417;
    property Containment: IRoseRichType dispid 416;
    property ExportControl: IRoseRichType dispid 415;
    property Static: WordBool dispid 414;
    property type_: WideString dispid 413;
    property InitValue: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseClassView
// Flags:     (4096) Dispatchable
// GUID:      {5F735F36-F9EA-11CF-BBD3-00A024C67143}
// *********************************************************************//
  IRoseClassView = dispinterface
    ['{5F735F36-F9EA-11CF-BBD3-00A024C67143}']
    function GetDefaultWidth: Smallint; dispid 215;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetQualifiedName: WideString; dispid 12555;
    function GetMinHeight: Smallint; dispid 218;
    function GetMinWidth: Smallint; dispid 217;
    function GetAttachedNotes: IRoseNoteViewCollection; dispid 12829;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function IdentifyClass: WideString; dispid 12668;
    function GetDisplayedOperations: IRoseItemCollection; dispid 12827;
    function HasParentView: WordBool; dispid 223;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetUniqueID: WideString; dispid 102;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function HasItem: WordBool; dispid 222;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    procedure Invalidate; dispid 207;
    function SupportsFillColor: WordBool; dispid 210;
    function SupportsLineColor: WordBool; dispid 211;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetIconIndex: Smallint; dispid 12824;
    function GetDisplayedAttributes: IRoseItemCollection; dispid 12826;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    property StereotypeDisplay: Smallint dispid 12837;
    property Visibility: WordBool dispid 12836;
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Font: IRoseView_Font dispid 12493;
    property SuppressOperations: WordBool dispid 414;
    property SuppressAttributes: WordBool dispid 413;
    property ShowAllOperations: WordBool dispid 412;
    property ShowAllAttributes: WordBool dispid 411;
    property ShowOperationSignature: WordBool dispid 410;
    property AutomaticResize: WordBool dispid 409;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Item: IRoseItem dispid 221;
    property ParentView: IRoseItemView dispid 220;
    property SubViews: IRoseItemViewCollection dispid 219;
    property LineColor: IRoseView_LineColor dispid 208;
    property FillColor: IRoseView_FillColor dispid 206;
    property Width: Smallint dispid 205;
    property Height: Smallint dispid 204;
    property XPosition: Smallint dispid 203;
    property YPosition: Smallint dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseClassDiagram
// Flags:     (4096) Dispatchable
// GUID:      {3FD9D002-93B0-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseClassDiagram = dispinterface
    ['{3FD9D002-93B0-11CF-B3D4-00A0241DB1D0}']
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetQualifiedName: WideString; dispid 12555;
    function AddRelationView(const theRelation: IRoseRelation): WordBool; dispid 12787;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    procedure Render(const FileName: WideString); dispid 217;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    procedure Update; dispid 206;
    function GetPropertyClassName: WideString; dispid 128;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Invalidate; dispid 205;
    procedure Activate; dispid 211;
    function Exists(const theItem: IRoseItem): WordBool; dispid 210;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function IsActive: WordBool; dispid 209;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function GetSelectedItems: IRoseItemCollection; dispid 12525;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    function GetNoteViews: IRoseNoteViewCollection; dispid 220;
    function RemoveAssociation(const theAssociation: IRoseAssociation): WordBool; dispid 421;
    function AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function AddCategory(const theCat: IRoseCategory): WordBool; dispid 413;
    procedure RenderToClipboard; dispid 222;
    function RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function RemoveCategory(const theCategory: IRoseCategory): WordBool; dispid 420;
    function GetSelectedClasses: IRoseClassCollection; dispid 415;
    function AddAssociation(const theAssociation: IRoseAssociation): WordBool; dispid 418;
    function GetCategories: IRoseCategoryCollection; dispid 417;
    function GetClasses: IRoseClassCollection; dispid 416;
    function AddUseCase(const theUseCase: IRoseUseCase): WordBool; dispid 423;
    function GetAssociations: IRoseAssociationCollection; dispid 422;
    function RemoveClass(const theClass: IRoseClass): WordBool; dispid 419;
    function GetSelectedCategories: IRoseCategoryCollection; dispid 414;
    procedure AutosizeAll; dispid 12862;
    function CenterDiagramToView(const theView: IRoseItemView): WordBool; dispid 12861;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function IsUseCaseDiagram: WordBool; dispid 426;
    function GetUseCases: IRoseUseCaseCollection; dispid 425;
    function RemoveUseCase(const theUseCase: IRoseUseCase): WordBool; dispid 424;
    function GetClassView(const theClass: IRoseClass): IRoseClassView; dispid 427;
    function LayoutSelectedViews(StackClasses: WordBool; ClassesPerRow: Smallint; DistX: Smallint; 
                                 DistY: Smallint; UpperLeftX: Smallint; UpperLeftY: Smallint): WordBool; dispid 12873;
    function GetIconIndex: Smallint; dispid 12824;
    function AddClass(const theClass: IRoseClass): WordBool; dispid 412;
    procedure RenderEnhancedToClipboard; dispid 223;
    function RemoveItemView(const theItemView: IRoseItemView): WordBool; dispid 12832;
    function IsDataModelingDiagram: WordBool; dispid 12834;
    function GetParentContext: IRoseItem; dispid 12823;
    function RenderIconToClipboard: WordBool; dispid 12820;
    property ZoomFactor: Smallint dispid 12690;
    property Documentation: WideString dispid 12656;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ParentCategory: IRoseCategory dispid 411;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Items: IRoseItemCollection dispid 208;
    property Visible: WordBool dispid 203;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseClass
// Flags:     (4096) Dispatchable
// GUID:      {BC57D1C0-863E-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseClass = dispinterface
    ['{BC57D1C0-863E-11CF-B3D4-00A0241DB1D0}']
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function AddParameter(const theName: WideString; const theType: WideString; 
                          const theDef: WideString; position: Smallint): IRoseParameter; dispid 12667;
    function IdentifyClass: WideString; dispid 12668;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetRealizeRelations: IRoseRealizeRelationCollection; dispid 12612;
    function GetAssignedLanguage: WideString; dispid 12642;
    function GetClassDependencies: IRoseClassDependencyCollection; dispid 12662;
    function AddClassDependency(const theSupplerName: WideString; const theSupplierType: WideString): IRoseClassDependency; dispid 12663;
    function IsNestedClass: WordBool; dispid 12643;
    function AddInstantiateRel(const theSupplierName: WideString): IRoseInstantiateRelation; dispid 12671;
    function DeleteClassDependency(const theClassDependency: IRoseClassDependency): WordBool; dispid 12664;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetInstantiateRelations: IRoseInstantiateRelationCollection; dispid 12670;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetAssociations: IRoseAssociationCollection; dispid 425;
    function GetSuperclasses: IRoseClassCollection; dispid 424;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetRoseItem: IRoseItem; dispid 207;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetPropertyClassName: WideString; dispid 128;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function OpenSpecification: WordBool; dispid 216;
    function AddOperation(const theName: WideString; const retType: WideString): IRoseOperation; dispid 427;
    function GetInheritRelations: IRoseInheritRelationCollection; dispid 423;
    function GetHasRelations: IRoseHasRelationshipCollection; dispid 422;
    procedure CreateStateMachine; dispid 12598;
    function DeleteHas(const theHas: IRoseHasRelationship): WordBool; dispid 432;
    procedure RemoveAssignedModule(const theModule: IRoseModule); dispid 12526;
    function GetQualifiedName: WideString; dispid 12555;
    function DeleteAssociation(const theAss: IRoseAssociation): WordBool; dispid 433;
    function AddAssociation(const theSupplierRoleName: WideString; 
                            const theSupplierRoleType: WideString): IRoseAssociation; dispid 429;
    function AddHas(const theSupplierName: WideString; const theSupplierType: WideString): IRoseHasRelationship; dispid 430;
    function DeleteOperation(const theOper: IRoseOperation): WordBool; dispid 434;
    function AddAttribute(const theName: WideString; const theType: WideString; 
                          const initVal: WideString): IRoseAttribute; dispid 428;
    procedure AddAssignedModule(const theModule: IRoseModule); dispid 12522;
    function GetAssociateRoles: IRoseRoleCollection; dispid 445;
    function DeleteNestedClass(const theClass: IRoseClass): WordBool; dispid 448;
    function GetAssignedModules: IRoseModuleCollection; dispid 12491;
    function GetNestedClasses: IRoseClassCollection; dispid 446;
    function AddRealizeRel(const theRelationName: WideString; const theInterfaceName: WideString): IRoseRealizeRelation; dispid 12610;
    function DeleteRealizeRel(const theRealizeRel: IRoseRealizeRelation): WordBool; dispid 12611;
    function AddNestedClass(const theName: WideString): IRoseClass; dispid 447;
    procedure DeleteStateMachine; dispid 12599;
    function IsALinkClass: WordBool; dispid 439;
    function RelocateClass(const theClass: IRoseClass): WordBool; dispid 12871;
    function GetContext: IRoseItem; dispid 12872;
    function GetClients(relationKind: Smallint; relationType: Smallint): IRoseClassCollection; dispid 12673;
    function DeleteParameter(const theParameter: IRoseParameter): WordBool; dispid 12678;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function ChangeAttributePosition(const theAttribute: IRoseAttribute; thePosition: Smallint): WordBool; dispid 12870;
    function GetSubclasses: IRoseClassCollection; dispid 12644;
    function GetIconIndex: Smallint; dispid 12824;
    function GetAssignedUnloadedComponents: IRoseStringCollection; dispid 12838;
    function DeleteAttribute(const theAttr: IRoseAttribute): WordBool; dispid 435;
    function AddInheritRel(const theRelationName: WideString; const theParentClassName: WideString): IRoseInheritRelation; dispid 437;
    function GetLinkAssociation: IRoseAssociation; dispid 440;
    function GetRoles: IRoseRoleCollection; dispid 444;
    function DeleteInheritRel(const theInheritRel: IRoseInheritRelation): WordBool; dispid 438;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function DeleteInstantiateRel(const theInstantiateRel: IRoseInstantiateRelation): WordBool; dispid 12672;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetAllNestedClasses: IRoseClassCollection; dispid 12808;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Parameters: IRoseParameterCollection dispid 12666;
    property ParentClass: IRoseClass dispid 12640;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property StateMachine: IRoseStateMachine dispid 463;
    property Space: WideString dispid 449;
    property FundamentalType: WordBool dispid 421;
    property Concurrency: IRoseRichType dispid 420;
    property ClassKind: IRoseRichType dispid 419;
    property ExportControl: IRoseRichType dispid 418;
    property Operations: IRoseOperationCollection dispid 417;
    property Attributes: IRoseAttributeCollection dispid 416;
    property ParentCategory: IRoseCategory dispid 415;
    property Persistence: WordBool dispid 414;
    property Cardinality: WideString dispid 413;
    property Abstract: WordBool dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseCategory
// Flags:     (4096) Dispatchable
// GUID:      {D7BC1B45-8618-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseCategory = dispinterface
    ['{D7BC1B45-8618-11CF-B3D4-00A0241DB1D0}']
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetPropertyClassName: WideString; dispid 128;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function IdentifyClass: WideString; dispid 12668;
    function GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function IsModified: WordBool; dispid 12654;
    function Uncontrol: WordBool; dispid 12655;
    function IsLocked: WordBool; dispid 12703;
    procedure RelocateAssociation(const theAssociation: IRoseAssociation); dispid 12677;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    procedure Refresh; dispid 12701;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetRoseItem: IRoseItem; dispid 207;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure RelocateCategory(const theCategory: IRoseCategory); dispid 425;
    function GetAllCategories: IRoseCategoryCollection; dispid 420;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function OpenSpecification: WordBool; dispid 216;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetAllClasses: IRoseClassCollection; dispid 419;
    procedure RelocateClassDiagram(const theClsDiagram: IRoseClassDiagram); dispid 426;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function AddCategory(const theName: WideString): IRoseCategory; dispid 423;
    procedure RelocateClass(const theClass: IRoseClass); dispid 424;
    function DeleteCategory(const theCategory: IRoseCategory): WordBool; dispid 427;
    function AddClassDiagram(const Name: WideString): IRoseClassDiagram; dispid 422;
    function AddClass(const theName: WideString): IRoseClass; dispid 421;
    function AddCategoryDependency(const theName: WideString; 
                                   const theSupplierCategoryName: WideString): IRoseCategoryDependency; dispid 12659;
    function DeleteScenarioDiagram(const theScenarioDiagram: IRoseScenarioDiagram): WordBool; dispid 431;
    procedure RelocateScenarioDiagram(const theScenarioDiagram: IRoseScenarioDiagram); dispid 432;
    function Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function AddUseCase(const szName: WideString): IRoseUseCase; dispid 448;
    function AddScenarioDiagram(const Name: WideString; Type_: Smallint): IRoseScenarioDiagram; dispid 430;
    function GetAssignedSubsystem: IRoseSubsystem; dispid 433;
    function TopLevel: WordBool; dispid 437;
    function DeleteClass(const theClass: IRoseClass): WordBool; dispid 428;
    function DeleteClassDiagram(const theClassDiagram: IRoseClassDiagram): WordBool; dispid 429;
    function Unload: WordBool; dispid 12439;
    function Control(const Path: WideString): WordBool; dispid 12434;
    function IsRootPackage: WordBool; dispid 621;
    function IsLoaded: WordBool; dispid 12435;
    function Load: WordBool; dispid 12436;
    function IsControlled: WordBool; dispid 12433;
    function SaveAs(const Path: WideString): WordBool; dispid 12443;
    function GetFileName: WideString; dispid 12441;
    function IsModifiable: WordBool; dispid 12438;
    function Save: WordBool; dispid 12442;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function AddUseCaseDiagram(const theName: WideString): IRoseClassDiagram; dispid 12869;
    function GetIconIndex: Smallint; dispid 12824;
    function GetContext: IRoseItem; dispid 12872;
    function AddDataModelDiagram(const Name: WideString): IRoseClassDiagram; dispid 12833;
    function GetCategoryDependencies: IRoseCategoryDependencyCollection; dispid 12660;
    function DeleteCategoryDependency(const theDependency: IRoseCategoryDependency): WordBool; dispid 12661;
    function GetAllClassesEx(Recursive: WordBool; Nested: WordBool): IRoseClassCollection; dispid 12857;
    function NeedsRefreshing: WordBool; dispid 12704;
    procedure Lock; dispid 12708;
    procedure SetAssignedSubsystem(const newValue: IRoseSubsystem); dispid 434;
    function HasAssignedSubsystem: WordBool; dispid 435;
    function GetAllUseCases: IRoseUseCaseCollection; dispid 447;
    function DeleteUseCase(const theUseCase: IRoseUseCase): WordBool; dispid 436;
    function GetQualifiedName: WideString; dispid 12555;
    procedure Unlock; dispid 12709;
    function GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function RenderIconToClipboard: WordBool; dispid 12820;
    property PetalVersion: Integer dispid 12881;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property UseCases: IRoseUseCaseCollection dispid 446;
    property ScenarioDiagrams: IRoseScenarioDiagramCollection dispid 418;
    property Associations: IRoseAssociationCollection dispid 417;
    property ClassDiagrams: IRoseClassDiagramCollection dispid 416;
    property ParentCategory: IRoseCategory dispid 415;
    property Categories: IRoseCategoryCollection dispid 414;
    property Classes: IRoseClassCollection dispid 413;
    property Global: WordBool dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseView_Font
// Flags:     (4096) Dispatchable
// GUID:      {CE5BE567-0380-11D1-BC11-00A024C67143}
// *********************************************************************//
  IRoseView_Font = dispinterface
    ['{CE5BE567-0380-11D1-BC11-00A024C67143}']
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    property Blue: Smallint dispid 12499;
    property Size: Smallint dispid 12497;
    property Green: Smallint dispid 12500;
    property Red: Smallint dispid 12501;
    property FaceName: WideString dispid 12498;
    property Underline: WordBool dispid 12568;
    property Italic: WordBool dispid 12567;
    property Bold: WordBool dispid 12521;
    property StrikeThrough: WordBool dispid 12569;
  end;

// *********************************************************************//
// DispIntf:  IRoseView_LineColor
// Flags:     (4096) Dispatchable
// GUID:      {CE5BE565-0380-11D1-BC11-00A024C67143}
// *********************************************************************//
  IRoseView_LineColor = dispinterface
    ['{CE5BE565-0380-11D1-BC11-00A024C67143}']
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    property Green: Smallint dispid 12503;
    property Blue: Smallint dispid 12502;
    property Red: Smallint dispid 12504;
  end;

// *********************************************************************//
// DispIntf:  IRoseView_FillColor
// Flags:     (4096) Dispatchable
// GUID:      {CE5BE563-0380-11D1-BC11-00A024C67143}
// *********************************************************************//
  IRoseView_FillColor = dispinterface
    ['{CE5BE563-0380-11D1-BC11-00A024C67143}']
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    property Transparent: WordBool dispid 12494;
    property Blue: Smallint dispid 12495;
    property Green: Smallint dispid 12496;
    property Red: Smallint dispid 12493;
  end;

// *********************************************************************//
// DispIntf:  IRoseNoteView
// Flags:     (4096) Dispatchable
// GUID:      {015655CA-72DF-11D0-95EB-0000F803584A}
// *********************************************************************//
  IRoseNoteView = dispinterface
    ['{015655CA-72DF-11D0-95EB-0000F803584A}']
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetQualifiedName: WideString; dispid 12555;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function HasParentView: WordBool; dispid 223;
    function GetAttachedNotes: IRoseNoteViewCollection; dispid 12829;
    function HasItem: WordBool; dispid 222;
    function GetNoteViewType: Smallint; dispid 424;
    function GetMinHeight: Smallint; dispid 218;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetUniqueID: WideString; dispid 102;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetDefaultWidth: Smallint; dispid 215;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function SupportsLineColor: WordBool; dispid 211;
    function GetMinWidth: Smallint; dispid 217;
    function PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetDiagramLink: IRoseDiagram; dispid 12821;
    function GetIconIndex: Smallint; dispid 12824;
    procedure LinkDiagram(const theDiagram: IRoseDiagram); dispid 12855;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function SupportsFillColor: WordBool; dispid 210;
    function GetToolNames: IRoseStringCollection; dispid 130;
    procedure Invalidate; dispid 207;
    function AddAttachmentToView(const theItemView: IRoseItemView; const theLabelText: WideString): IRoseItemView; dispid 12822;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    property StereotypeDisplay: Smallint dispid 12837;
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Font: IRoseView_Font dispid 12493;
    property Text: WideString dispid 423;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Item: IRoseItem dispid 221;
    property ParentView: IRoseItemView dispid 220;
    property SubViews: IRoseItemViewCollection dispid 219;
    property LineColor: IRoseView_LineColor dispid 208;
    property FillColor: IRoseView_FillColor dispid 206;
    property Width: Smallint dispid 205;
    property Height: Smallint dispid 204;
    property XPosition: Smallint dispid 203;
    property YPosition: Smallint dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseItemView
// Flags:     (4096) Dispatchable
// GUID:      {7DFAFE40-A29D-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseItemView = dispinterface
    ['{7DFAFE40-A29D-11CF-B3D4-00A0241DB1D0}']
    function HasItem: WordBool; dispid 222;
    function HasParentView: WordBool; dispid 223;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetMinHeight: Smallint; dispid 218;
    function GetDefaultWidth: Smallint; dispid 215;
    function GetQualifiedName: WideString; dispid 12555;
    function GetIconIndex: Smallint; dispid 12824;
    function GetDefaultHeight: Smallint; dispid 216;
    function GetMinWidth: Smallint; dispid 217;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetUniqueID: WideString; dispid 102;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function IsSelected: WordBool; dispid 212;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function SupportsFillColor: WordBool; dispid 210;
    function SupportsLineColor: WordBool; dispid 211;
    function PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    procedure Invalidate; dispid 207;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetAttachedNotes: IRoseNoteViewCollection; dispid 12829;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    property StereotypeDisplay: Smallint dispid 12837;
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Font: IRoseView_Font dispid 12493;
    property ParentDiagram: IRoseDiagram dispid 224;
    property Item: IRoseItem dispid 221;
    property ParentView: IRoseItemView dispid 220;
    property SubViews: IRoseItemViewCollection dispid 219;
    property LineColor: IRoseView_LineColor dispid 208;
    property FillColor: IRoseView_FillColor dispid 206;
    property Width: Smallint dispid 205;
    property Height: Smallint dispid 204;
    property XPosition: Smallint dispid 203;
    property YPosition: Smallint dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseDiagram
// Flags:     (4096) Dispatchable
// GUID:      {3FD9D000-93B0-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseDiagram = dispinterface
    ['{3FD9D000-93B0-11CF-B3D4-00A0241DB1D0}']
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetSelectedItems: IRoseItemCollection; dispid 12525;
    function AddRelationView(const theRelation: IRoseRelation): WordBool; dispid 12787;
    procedure Invalidate; dispid 205;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function GetUniqueID: WideString; dispid 102;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetPropertyClassName: WideString; dispid 128;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetToolNames: IRoseStringCollection; dispid 130;
    procedure Layout; dispid 204;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function IsActive: WordBool; dispid 209;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    procedure Update; dispid 206;
    function GetNoteViews: IRoseNoteViewCollection; dispid 220;
    function AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    procedure Render(const FileName: WideString); dispid 217;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure Activate; dispid 211;
    procedure AutosizeAll; dispid 12862;
    function LayoutSelectedViews(StackClasses: WordBool; ClassesPerRow: Smallint; DistX: Smallint; 
                                 DistY: Smallint; UpperLeftX: Smallint; UpperLeftY: Smallint): WordBool; dispid 12873;
    function GetParentContext: IRoseItem; dispid 12823;
    function GetQualifiedName: WideString; dispid 12555;
    function IdentifyClass: WideString; dispid 12668;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    procedure RenderEnhancedToClipboard; dispid 223;
    function Exists(const theItem: IRoseItem): WordBool; dispid 210;
    function CenterDiagramToView(const theView: IRoseItemView): WordBool; dispid 12861;
    function GetIconIndex: Smallint; dispid 12824;
    function RemoveItemView(const theItemView: IRoseItemView): WordBool; dispid 12832;
    property ZoomFactor: Smallint dispid 12690;
    property Documentation: WideString dispid 12656;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Items: IRoseItemCollection dispid 208;
    property Visible: WordBool dispid 203;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseStringCollection
// Flags:     (4096) Dispatchable
// GUID:      {6A7FC311-C893-11D0-BC0B-00A024C67143}
// *********************************************************************//
  IRoseStringCollection = dispinterface
    ['{6A7FC311-C893-11D0-BC0B-00A024C67143}']
    function GetAt(id: Smallint): WideString; dispid 51;
    property Count: Smallint dispid 50;
  end;

// *********************************************************************//
// DispIntf:  IRoseObjectFlowCollection
// Flags:     (4096) Dispatchable
// GUID:      {882D2F8D-BD12-11D3-92AA-004005141253}
// *********************************************************************//
  IRoseObjectFlowCollection = dispinterface
    ['{882D2F8D-BD12-11D3-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseObjectFlow; dispid 203;
    function Exists(const pObject: IRoseObjectFlow): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseObjectFlow): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseObjectFlowCollection); dispid 209;
    procedure Remove(const theObject: IRoseObjectFlow); dispid 210;
    procedure Add(const theObject: IRoseObjectFlow); dispid 208;
    function GetFirst(const Name: WideString): IRoseObjectFlow; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseObjectFlow; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseDependencyRelationCollection
// Flags:     (4096) Dispatchable
// GUID:      {882D2F89-BD12-11D3-92AA-004005141253}
// *********************************************************************//
  IRoseDependencyRelationCollection = dispinterface
    ['{882D2F89-BD12-11D3-92AA-004005141253}']
    function GetAt(Index: Smallint): IRoseDependencyRelation; dispid 203;
    function Exists(const pObject: IRoseDependencyRelation): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseDependencyRelation): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseDependencyRelationCollection); dispid 209;
    procedure Remove(const theObject: IRoseDependencyRelation); dispid 210;
    procedure Add(const theObject: IRoseDependencyRelation); dispid 208;
    function GetFirst(const Name: WideString): IRoseDependencyRelation; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseDependencyRelation; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseObjectFlow
// Flags:     (4096) Dispatchable
// GUID:      {882D2F81-BD12-11D3-92AA-004005141253}
// *********************************************************************//
  IRoseObjectFlow = dispinterface
    ['{882D2F81-BD12-11D3-92AA-004005141253}']
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function IdentifyClass: WideString; dispid 12668;
    function GetClient: IRoseItem; dispid 12608;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function GetSupplier: IRoseItem; dispid 12609;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function HasClient: WordBool; dispid 12606;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function OpenSpecification: WordBool; dispid 216;
    function HasSupplier: WordBool; dispid 12607;
    function GetQualifiedName: WideString; dispid 12555;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function SetState(const theState: IRoseState): WordBool; dispid 12842;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetContext: IRoseItem; dispid 12872;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function GetRoseItem: IRoseItem; dispid 207;
    function GetIconIndex: Smallint; dispid 12824;
    function GetState: IRoseState; dispid 12841;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseDependencyRelation
// Flags:     (4096) Dispatchable
// GUID:      {882D2F85-BD12-11D3-92AA-004005141253}
// *********************************************************************//
  IRoseDependencyRelation = dispinterface
    ['{882D2F85-BD12-11D3-92AA-004005141253}']
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetSupplier: IRoseItem; dispid 12609;
    function HasSupplier: WordBool; dispid 12607;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetClient: IRoseItem; dispid 12608;
    function GetPropertyClassName: WideString; dispid 128;
    function OpenSpecification: WordBool; dispid 216;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetQualifiedName: WideString; dispid 12555;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetIconIndex: Smallint; dispid 12824;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function HasClient: WordBool; dispid 12606;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseDeploymentUnit
// Flags:     (4096) Dispatchable
// GUID:      {4335FBE3-F0A0-11D1-9FAD-0060975306FE}
// *********************************************************************//
  IRoseDeploymentUnit = dispinterface
    ['{4335FBE3-F0A0-11D1-9FAD-0060975306FE}']
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function NeedsRefreshing: WordBool; dispid 12704;
    function GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    function GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function IsLocked: WordBool; dispid 12703;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetRoseItem: IRoseItem; dispid 207;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function IsControlled: WordBool; dispid 12433;
    function Control(const Path: WideString): WordBool; dispid 12434;
    function GetQualifiedName: WideString; dispid 12555;
    function Unload: WordBool; dispid 12439;
    function IsLoaded: WordBool; dispid 12435;
    function OpenSpecification: WordBool; dispid 216;
    function Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function GetFileName: WideString; dispid 12441;
    function Save: WordBool; dispid 12442;
    function IsModified: WordBool; dispid 12654;
    function Uncontrol: WordBool; dispid 12655;
    function SaveAs(const Path: WideString): WordBool; dispid 12443;
    function GetIconIndex: Smallint; dispid 12824;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    procedure Refresh; dispid 12701;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function Load: WordBool; dispid 12436;
    function IsModifiable: WordBool; dispid 12438;
    function RenderIconToClipboard: WordBool; dispid 12820;
    procedure Lock; dispid 12708;
    procedure Unlock; dispid 12709;
    property PetalVersion: Integer dispid 12881;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseRelation
// Flags:     (4096) Dispatchable
// GUID:      {BA242E02-8961-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseRelation = dispinterface
    ['{BA242E02-8961-11CF-B3D4-00A0241DB1D0}']
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetSupplier: IRoseItem; dispid 12609;
    function HasSupplier: WordBool; dispid 12607;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetClient: IRoseItem; dispid 12608;
    function GetPropertyClassName: WideString; dispid 128;
    function OpenSpecification: WordBool; dispid 216;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetQualifiedName: WideString; dispid 12555;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetIconIndex: Smallint; dispid 12824;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function HasClient: WordBool; dispid 12606;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property SupplierName: WideString dispid 412;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseDefaultModelProperties
// Flags:     (4096) Dispatchable
// GUID:      {76ACC49D-FA18-11D0-BC11-00A024C67143}
// *********************************************************************//
  IRoseDefaultModelProperties = dispinterface
    ['{76ACC49D-FA18-11D0-BC11-00A024C67143}']
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    procedure Refresh; dispid 12701;
    function NeedsRefreshing: WordBool; dispid 12704;
    function IsLocked: WordBool; dispid 12703;
    function GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function CreateDefaultPropertySet(const ClassName: WideString; const ToolName: WideString; 
                                      const NewSetName: WideString): WordBool; dispid 442;
    function GetPropertyClassName: WideString; dispid 128;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function CloneDefaultPropertySet(const ClassName: WideString; const ToolName: WideString; 
                                     const ExistingSetName: WideString; const NewSetName: WideString): WordBool; dispid 441;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function AddDefaultProperty(const ClassName: WideString; const ToolName: WideString; 
                                const SetName: WideString; const PropName: WideString; 
                                const PropType: WideString; const Value: WideString): WordBool; dispid 440;
    function DeleteDefaultPropertySet(const ClassName: WideString; const ToolName: WideString; 
                                      const SetName: WideString): WordBool; dispid 443;
    function GetDefaultPropertySet(const ClassName: WideString; const ToolName: WideString; 
                                   const SetName: WideString): IRosePropertyCollection; dispid 444;
    function OpenSpecification: WordBool; dispid 216;
    function Uncontrol: WordBool; dispid 12655;
    function DeleteDefaultProperty(const ClassName: WideString; const ToolName: WideString; 
                                   const SetName: WideString; const PropName: WideString): WordBool; dispid 449;
    function GetDefaultSetNames(const ClassName: WideString; const ToolName: WideString): IRoseStringCollection; dispid 447;
    function SaveAs(const Path: WideString): WordBool; dispid 12443;
    function FindDefaultProperty(const ClassName: WideString; const ToolName: WideString; 
                                 const SetName: WideString; const PropName: WideString): IRoseProperty; dispid 445;
    function Load: WordBool; dispid 12436;
    function IsLoaded: WordBool; dispid 12435;
    function GetToolNamesForClass(const Parameter1: WideString): IRoseStringCollection; dispid 446;
    function IsModifiable: WordBool; dispid 12438;
    function GetFileName: WideString; dispid 12441;
    function Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function Unload: WordBool; dispid 12439;
    function Save: WordBool; dispid 12442;
    function IsToolVisible(const theToolName: WideString): WordBool; dispid 12593;
    function GetQualifiedName: WideString; dispid 12555;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetContext: IRoseItem; dispid 12872;
    function GetIconIndex: Smallint; dispid 12824;
    function IsModified: WordBool; dispid 12654;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    procedure SetToolVisibility(const theToolName: WideString; Visibility: WordBool); dispid 12594;
    function Control(const Path: WideString): WordBool; dispid 12434;
    function IsControlled: WordBool; dispid 12433;
    procedure Unlock; dispid 12709;
    procedure Lock; dispid 12708;
    function GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    function OpenCustomSpecification: WordBool; dispid 12728;
    property PetalVersion: Integer dispid 12881;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseProperty
// Flags:     (4096) Dispatchable
// GUID:      {93461A23-8811-11CF-B1B0-D227D5210B2C}
// *********************************************************************//
  IRoseProperty = dispinterface
    ['{93461A23-8811-11CF-B1B0-D227D5210B2C}']
    property Name: WideString dispid 202;
    property Value: WideString dispid 203;
    property ToolName: WideString dispid 205;
    property type_: WideString dispid 206;
  end;

// *********************************************************************//
// DispIntf:  IRoseElement
// Flags:     (4096) Dispatchable
// GUID:      {D067F15F-6987-11D0-BBF0-00A024C67143}
// *********************************************************************//
  IRoseElement = dispinterface
    ['{D067F15F-6987-11D0-BBF0-00A024C67143}']
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetUniqueID: WideString; dispid 102;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function IdentifyClass: WideString; dispid 12668;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetQualifiedName: WideString; dispid 12555;
    function GetIconIndex: Smallint; dispid 12824;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function RenderIconToClipboard: WordBool; dispid 12820;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRosePackage
// Flags:     (4096) Dispatchable
// GUID:      {47D975C1-8A8D-11D0-A214-444553540000}
// *********************************************************************//
  IRosePackage = dispinterface
    ['{47D975C1-8A8D-11D0-A214-444553540000}']
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function NeedsRefreshing: WordBool; dispid 12704;
    function GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    function GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function IsLocked: WordBool; dispid 12703;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetPropertyClassName: WideString; dispid 128;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetRoseItem: IRoseItem; dispid 207;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function IdentifyClass: WideString; dispid 12668;
    function IsRootPackage: WordBool; dispid 621;
    function IsControlled: WordBool; dispid 12433;
    function SaveAs(const Path: WideString): WordBool; dispid 12443;
    function IsModifiable: WordBool; dispid 12438;
    function Control(const Path: WideString): WordBool; dispid 12434;
    function OpenSpecification: WordBool; dispid 216;
    function Unload: WordBool; dispid 12439;
    function Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function GetFileName: WideString; dispid 12441;
    function GetQualifiedName: WideString; dispid 12555;
    function IsModified: WordBool; dispid 12654;
    function Save: WordBool; dispid 12442;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    procedure Refresh; dispid 12701;
    function GetIconIndex: Smallint; dispid 12824;
    function Uncontrol: WordBool; dispid 12655;
    function IsLoaded: WordBool; dispid 12435;
    function Load: WordBool; dispid 12436;
    procedure Lock; dispid 12708;
    procedure Unlock; dispid 12709;
    function OpenCustomSpecification: WordBool; dispid 12728;
    property PetalVersion: Integer dispid 12881;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseControllableUnit
// Flags:     (4096) Dispatchable
// GUID:      {32C862A7-8AA9-11D0-A70B-0000F803584A}
// *********************************************************************//
  IRoseControllableUnit = dispinterface
    ['{32C862A7-8AA9-11D0-A70B-0000F803584A}']
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function NeedsRefreshing: WordBool; dispid 12704;
    function GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    function GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function IsLocked: WordBool; dispid 12703;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetRoseItem: IRoseItem; dispid 207;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function IsControlled: WordBool; dispid 12433;
    function Control(const Path: WideString): WordBool; dispid 12434;
    function GetQualifiedName: WideString; dispid 12555;
    function Unload: WordBool; dispid 12439;
    function IsLoaded: WordBool; dispid 12435;
    function OpenSpecification: WordBool; dispid 216;
    function Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function GetFileName: WideString; dispid 12441;
    function Save: WordBool; dispid 12442;
    function IsModified: WordBool; dispid 12654;
    function Uncontrol: WordBool; dispid 12655;
    function SaveAs(const Path: WideString): WordBool; dispid 12443;
    function GetIconIndex: Smallint; dispid 12824;
    function GetContext: IRoseItem; dispid 12872;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function IdentifyClass: WideString; dispid 12668;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    procedure Refresh; dispid 12701;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function Load: WordBool; dispid 12436;
    function IsModifiable: WordBool; dispid 12438;
    function RenderIconToClipboard: WordBool; dispid 12820;
    procedure Lock; dispid 12708;
    procedure Unlock; dispid 12709;
    property PetalVersion: Integer dispid 12881;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseExternalDocument
// Flags:     (4096) Dispatchable
// GUID:      {906FF583-276B-11D0-8980-00A024774419}
// *********************************************************************//
  IRoseExternalDocument = dispinterface
    ['{906FF583-276B-11D0-8980-00A024774419}']
    function GetIconIndex: Smallint; dispid 12824;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function IsURL: WordBool; dispid 4;
    function IdentifyClass: WideString; dispid 12668;
    function Open(const szAppPath: WideString): WordBool; dispid 5;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    property ParentDiagram: IRoseDiagram dispid 12811;
    property ParentItem: IRoseItem dispid 12810;
    property ParentCategory: IRoseCategory dispid 3;
    property URL: WideString dispid 2;
    property Path: WideString dispid 1;
  end;

// *********************************************************************//
// DispIntf:  IRoseItem
// Flags:     (4096) Dispatchable
// GUID:      {BC57D1C2-863E-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseItem = dispinterface
    ['{BC57D1C2-863E-11CF-B3D4-00A0241DB1D0}']
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function OpenSpecification: WordBool; dispid 216;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function GetUniqueID: WideString; dispid 102;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function GetQualifiedName: WideString; dispid 12555;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetRoseItem: IRoseItem; dispid 207;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function GetContext: IRoseItem; dispid 12872;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetPropertyClassName: WideString; dispid 128;
    function GetIconIndex: Smallint; dispid 12824;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseModel
// Flags:     (4096) Dispatchable
// GUID:      {E38942A0-8621-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseModel = dispinterface
    ['{E38942A0-8621-11CF-B3D4-00A0241DB1D0}']
    function GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function GetToolNames: IRoseStringCollection; dispid 130;
    function IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function GetUniqueID: WideString; dispid 102;
    function OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                              const theValue: WideString): WordBool; dispid 110;
    function CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                            const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function GetPropertyClassName: WideString; dispid 128;
    procedure Lock; dispid 12708;
    procedure Unlock; dispid 12709;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function Uncontrol: WordBool; dispid 12655;
    function IdentifyClass: WideString; dispid 12668;
    function GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    function IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function NeedsRefreshing: WordBool; dispid 12704;
    procedure ResolveReferences; dispid 12710;
    function IsLocked: WordBool; dispid 12703;
    function GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function GetSelectedModules: IRoseModuleCollection; dispid 430;
    function GetSelectedSubsystems: IRoseSubsystemCollection; dispid 431;
    function OpenSpecification: WordBool; dispid 216;
    function GetAllAssociations: IRoseAssociationCollection; dispid 412;
    function DeleteProcessor(const pProcessor: IRoseProcessor): WordBool; dispid 425;
    function GetRoseItem: IRoseItem; dispid 207;
    function AddProcessor(const pName: WideString): IRoseProcessor; dispid 424;
    function GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function GetSelectedClasses: IRoseClassCollection; dispid 428;
    function GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function GetAllProperties: IRosePropertyCollection; dispid 122;
    function GetAllClasses: IRoseClassCollection; dispid 432;
    function GetAllCategories: IRoseCategoryCollection; dispid 433;
    function DeleteDevice(const pDevice: IRoseDevice): WordBool; dispid 427;
    function GetSelectedCategories: IRoseCategoryCollection; dispid 429;
    function AddDevice(const pName: WideString): IRoseDevice; dispid 426;
    function GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function GetAllDevices: IRoseDeviceCollection; dispid 437;
    function GetSelectedUseCases: IRoseUseCaseCollection; dispid 438;
    function Unload: WordBool; dispid 12439;
    function FindClassWithID(const UniqueID: WideString): IRoseClass; dispid 12475;
    function FindCategories(const CategoryName: WideString): IRoseCategoryCollection; dispid 12476;
    function GetAllProcessors: IRoseProcessorCollection; dispid 436;
    function IsLoaded: WordBool; dispid 12435;
    function GetAllSubsystems: IRoseSubsystemCollection; dispid 435;
    function GetAllUseCases: IRoseUseCaseCollection; dispid 439;
    function GetAllModules: IRoseModuleCollection; dispid 434;
    function Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function GetFileName: WideString; dispid 12441;
    function FindItemWithID(const UniqueID: WideString): IRoseItem; dispid 12473;
    function SaveAs(const Path: WideString): WordBool; dispid 12443;
    function FindItems(const ItemName: WideString): IRoseItemCollection; dispid 12472;
    function GetQualifiedName: WideString; dispid 12555;
    function FindClasses(const ClassName: WideString): IRoseClassCollection; dispid 12474;
    function GetActiveDiagram: IRoseDiagram; dispid 12527;
    function Save: WordBool; dispid 12442;
    function FindCategoryWithID(const UniqueID: WideString): IRoseCategory; dispid 12477;
    function Load: WordBool; dispid 12436;
    function GetUserOverriddenProperties(const theToolName: WideString): IRosePropertyCollection; dispid 12886;
    function Import(const theName: WideString): WordBool; dispid 12835;
    function LoadClosure(const sFQName: WideString; depth: Smallint; bReportInfoNotFound: WordBool): Smallint; dispid 12880;
    function RenderIconToClipboard: WordBool; dispid 12820;
    function LoadClassClosure(const theClass: IRoseClass; depth: Smallint; 
                              bReportInfoNotFound: WordBool): Smallint; dispid 12879;
    function GetSelectedItems: IRoseItemCollection; dispid 12681;
    procedure Refresh; dispid 12701;
    function OpenCustomSpecification: WordBool; dispid 12728;
    function GetAllClassesEx(Recursive: WordBool; Nested: WordBool): IRoseClassCollection; dispid 12858;
    function GetContext: IRoseItem; dispid 12872;
    function Control(const Path: WideString): WordBool; dispid 12434;
    function IsModified: WordBool; dispid 12654;
    function IsControlled: WordBool; dispid 12433;
    function IsModifiable: WordBool; dispid 12438;
    function IsRootPackage: WordBool; dispid 621;
    function GetSelectedExternalDocuments: IRoseExternalDocumentCollection; dispid 12788;
    function FindDiagramWithID(const UniqueID: WideString): IRoseDiagram; dispid 12817;
    function GetSelectedDiagrams: IRoseDiagramCollection; dispid 12831;
    function GetIconIndex: Smallint; dispid 12824;
    function LoadControlledUnits(const theControlledUnits: IRoseControllableUnitCollection): WordBool; dispid 12828;
    property MaintainModelForAutoloading: WordBool dispid 12882;
    property PetalVersion: Integer dispid 12881;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Notation: Smallint dispid 12691;
    property DefaultLanguage: WideString dispid 12680;
    property DeploymentUnit: IRoseDeploymentUnit dispid 12676;
    property LocalizedStereotype: WideString dispid 12554;
    property Model: IRoseModel dispid 12524;
    property Application: IDispatch dispid 12523;
    property DefaultProperties: IRoseDefaultModelProperties dispid 12471;
    property RootUseCaseCategory: IRoseCategory dispid 422;
    property UseCases: IRoseUseCaseCollection dispid 421;
    property DeploymentDiagram: IRoseDeploymentDiagram dispid 420;
    property RootSubsystem: IRoseSubsystem dispid 418;
    property RootCategory: IRoseCategory dispid 417;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Stereotype: WideString dispid 212;
    property Documentation: WideString dispid 203;
    property Name: WideString dispid 100;
  end;

// *********************************************************************//
// DispIntf:  IRoseContextMenuItemCollection
// Flags:     (4096) Dispatchable
// GUID:      {EE0B16E2-FF91-11D1-9FAD-0060975306FE}
// *********************************************************************//
  IRoseContextMenuItemCollection = dispinterface
    ['{EE0B16E2-FF91-11D1-9FAD-0060975306FE}']
    function GetAt(Index: Smallint): IRoseContextMenuItem; dispid 203;
    function Exists(const pObject: IRoseContextMenuItem): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseContextMenuItem): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseContextMenuItemCollection); dispid 209;
    procedure Remove(const theObject: IRoseContextMenuItem); dispid 210;
    procedure Add(const theObject: IRoseContextMenuItem); dispid 208;
    function GetFirst(const Name: WideString): IRoseContextMenuItem; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseContextMenuItem; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseContextMenuItem
// Flags:     (4096) Dispatchable
// GUID:      {EE0B16E0-FF91-11D1-9FAD-0060975306FE}
// *********************************************************************//
  IRoseContextMenuItem = dispinterface
    ['{EE0B16E0-FF91-11D1-9FAD-0060975306FE}']
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    property InternalName: WideString dispid 12683;
    property MenuID: Smallint dispid 12686;
    property MenuState: Smallint dispid 12687;
    property Caption: WideString dispid 12682;
  end;

// *********************************************************************//
// DispIntf:  IRoseAddInCollection
// Flags:     (4096) Dispatchable
// GUID:      {C87D2BC1-352A-11D1-883B-3C8B00C10000}
// *********************************************************************//
  IRoseAddInCollection = dispinterface
    ['{C87D2BC1-352A-11D1-883B-3C8B00C10000}']
    function GetAt(Index: Smallint): IRoseAddIn; dispid 203;
    function Exists(const pObject: IRoseAddIn): WordBool; dispid 204;
    function FindFirst(const Name: WideString): Smallint; dispid 205;
    function FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function IndexOf(const theObject: IRoseAddIn): Smallint; dispid 207;
    procedure RemoveAll; dispid 211;
    procedure AddCollection(const theCollection: IRoseAddInCollection); dispid 209;
    procedure Remove(const theObject: IRoseAddIn); dispid 210;
    procedure Add(const theObject: IRoseAddIn); dispid 208;
    function GetFirst(const Name: WideString): IRoseAddIn; dispid 212;
    function GetWithUniqueID(const UniqueID: WideString): IRoseAddIn; dispid 213;
    property Count: Smallint dispid 202;
  end;

// *********************************************************************//
// DispIntf:  IRoseAddIn
// Flags:     (4096) Dispatchable
// GUID:      {D5352FC0-346C-11D1-883B-3C8B00C10000}
// *********************************************************************//
  IRoseAddIn = dispinterface
    ['{D5352FC0-346C-11D1-883B-3C8B00C10000}']
    function LoadFundamentalTypes: WordBool; dispid 12874;
    function GetDisplayName: WideString; dispid 12689;
    function GetContextMenuItems(itemType: Smallint): IRoseContextMenuItemCollection; dispid 12685;
    function AddContextMenuItem(itemType: Smallint; const fullCaption: WideString; 
                                const InternalName: WideString): IRoseContextMenuItem; dispid 12684;
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    function WriteSetting(const Section: WideString; const Entry: WideString; 
                          const Value: WideString): WordBool; dispid 12597;
    function ReadSetting(const Section: WideString; const Entry: WideString; 
                         const Default: WideString): WideString; dispid 12596;
    procedure ExecuteScript(const FileName: WideString); dispid 12595;
    function IsActive: WordBool; dispid 12553;
    procedure Activate; dispid 12543;
    procedure Deactivate; dispid 12542;
    function IsLanguageAddIn: WordBool; dispid 12541;
    property Copyright: WideString dispid 12645;
    property Name: WideString dispid 12540;
    property ServerName: WideString dispid 12539;
    property ToolNames: IRoseStringCollection dispid 12538;
    property FundamentalTypes: IRoseStringCollection dispid 12537;
    property RootRegistryPath: WideString dispid 12536;
    property InstallDirectory: WideString dispid 12535;
    property PropertyFilePath: WideString dispid 12534;
    property MenuFilePath: WideString dispid 12533;
    property HelpFilePath: WideString dispid 12532;
    property CompanyName: WideString dispid 12531;
    property Version: WideString dispid 12530;
    property EventHandler: IDispatch dispid 12528;
  end;

// *********************************************************************//
// DispIntf:  IRoseAddInManager
// Flags:     (4096) Dispatchable
// GUID:      {D5352FC2-346C-11D1-883B-3C8B00C10000}
// *********************************************************************//
  IRoseAddInManager = dispinterface
    ['{D5352FC2-346C-11D1-883B-3C8B00C10000}']
    function IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function IdentifyClass: WideString; dispid 12668;
    function DisableEvents(theEvents: Integer): Integer; dispid 12692;
    function EnableEvents(theEvents: Integer): Integer; dispid 12693;
    property AddIns: IRoseAddInCollection dispid 12529;
  end;

// *********************************************************************//
// DispIntf:  IRosePathMap
// Flags:     (4096) Dispatchable
// GUID:      {4C9E2241-84C5-11D0-A214-444553540000}
// *********************************************************************//
  IRosePathMap = dispinterface
    ['{4C9E2241-84C5-11D0-A214-444553540000}']
    function DeleteEntry(const Symbol: WideString): WordBool; dispid 50;
    function GetActualPath(const VirtualPath: WideString): WideString; dispid 51;
    function GetVirtualPath(const ActualPath: WideString): WideString; dispid 52;
    function HasEntry(const Symbol: WideString): WordBool; dispid 53;
    function AddEntry(const Symbol: WideString; const Path: WideString; const Comment: WideString): WordBool; dispid 54;
    function GetActualPathWithContext(const VirtualPath: WideString; const Context: WideString): WideString; dispid 12674;
    function GetVirtualPathWithContext(const ActualPath: WideString; const Context: WideString): WideString; dispid 12675;
  end;

// *********************************************************************//
// DispIntf:  IRoseApplication
// Flags:     (4096) Dispatchable
// GUID:      {D7BC1B40-8618-11CF-B3D4-00A0241DB1D0}
// *********************************************************************//
  IRoseApplication = dispinterface
    ['{D7BC1B40-8618-11CF-B3D4-00A0241DB1D0}']
    function GetLicensedApplication(const theKey: WideString): IRoseApplication; dispid 235;
    function EditorIsDirty(const FileName: WideString): WordBool; dispid 12864;
    procedure Save(bSaveUnits: WordBool); dispid 214;
    procedure WriteErrorLog(const theMsg: WideString); dispid 213;
    function EditorIsVisible(const FileName: WideString): WordBool; dispid 12865;
    function EditorDomainOf(const FileName: WideString): WideString; dispid 12862;
    function EditorGetOpenFiles: IRoseStringCollection; dispid 12863;
    function EditorRefreshFile(const FileName: WideString): WordBool; dispid 12866;
    function EditorOpenFile(const FileName: WideString; const fileDomain: WideString): WordBool; dispid 12861;
    function OpenModelAsTemplate(const szFileName: WideString): IRoseModel; dispid 223;
    procedure NewScript; dispid 226;
    procedure SelectObjectInBrowser(const theRoseObject: IRoseObject); dispid 221;
    procedure CompileScriptFile(const FileName: WideString; const BinaryName: WideString; 
                                bDebug: WordBool); dispid 218;
    procedure OpenScript(const FileName: WideString); dispid 225;
    function OpenModel(const theModel: WideString): IRoseModel; dispid 210;
    procedure Exit; dispid 212;
    procedure SaveAs(const theFile: WideString; bSaveUnits: WordBool); dispid 215;
    function NewModel: IRoseModel; dispid 211;
    function OpenExternalDocument(const FileName: WideString): WordBool; dispid 12588;
    function SaveModel(bSaveUnits: WordBool): Integer; dispid 12856;
    procedure ExecuteScript(const pFileName: WideString); dispid 236;
    function OpenURL(const theURL: WideString): WordBool; dispid 12587;
    function EnableUserEditOfItem(const theItem: IRoseItem): WordBool; dispid 12859;
    function OpenRoseModel(const theModel: WideString; promptSubUnits: WordBool): IRoseModel; dispid 12697;
    function GetRoseIniPath: WideString; dispid 12698;
    function EnableUserEditOfDiagram(const theDiagram: IRoseDiagram): WordBool; dispid 12860;
    function UpdateBrowserDocOverlayImage(const theDocument: IRoseExternalDocument): WordBool; dispid 12688;
    function LogWriteTab(const tabName: WideString; const domain: WideString; const Text: WideString): WordBool; dispid 12872;
    function EditorDisplayFile(const FileName: WideString): WordBool; dispid 12867;
    function LogClearTab(const tabName: WideString; const domain: WideString): WordBool; dispid 12870;
    function LogSetActiveTab(const tabName: WideString; const domain: WideString): WordBool; dispid 12871;
    function LogCreateTab(const tabName: WideString; const domain: WideString): WordBool; dispid 12868;
    function WriteProfileString(const Section: WideString; const Entry: WideString; 
                                const Value: WideString): WordBool; dispid 12590;
    function UpdateBrowserOverlayImage(const theItem: IRoseItem): WordBool; dispid 12679;
    function LogCloseTab(const tabName: WideString; const domain: WideString): WordBool; dispid 12869;
    function GetProfileString(const Section: WideString; const Entry: WideString; 
                              const Default: WideString): WideString; dispid 12589;
    property IsInitialized: WordBool dispid 12809;
    property CommandLine: WideString dispid 12586;
    property AddInManager: IRoseAddInManager dispid 12544;
    property ApplicationPath: WideString dispid 233;
    property ProductName: WideString dispid 232;
    property Version: WideString dispid 231;
    property PathMap: IRosePathMap dispid 224;
    property CurrentModel: IRoseModel dispid 209;
    property Width: Smallint dispid 208;
    property Height: Smallint dispid 207;
    property Left: Smallint dispid 206;
    property Top: Smallint dispid 205;
    property Visible: WordBool dispid 202;
  end;

// *********************************************************************//
// The Class CoRoseLineVertex provides a Create and CreateRemote method to          
// create instances of the default interface IRoseLineVertex exposed by              
// the CoClass RoseLineVertex. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseLineVertex = class
    class function Create: IRoseLineVertex;
    class function CreateRemote(const MachineName: string): IRoseLineVertex;
  end;

// *********************************************************************//
// The Class CoRoseObject provides a Create and CreateRemote method to          
// create instances of the default interface IRoseObject exposed by              
// the CoClass RoseObject. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseObject = class
    class function Create: IRoseObject;
    class function CreateRemote(const MachineName: string): IRoseObject;
  end;

// *********************************************************************//
// The Class CoRoseStateMachineCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseStateMachineCollection exposed by              
// the CoClass RoseStateMachineCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseStateMachineCollection = class
    class function Create: IRoseStateMachineCollection;
    class function CreateRemote(const MachineName: string): IRoseStateMachineCollection;
  end;

// *********************************************************************//
// The Class CoRoseStateViewCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseStateViewCollection exposed by              
// the CoClass RoseStateViewCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseStateViewCollection = class
    class function Create: IRoseStateViewCollection;
    class function CreateRemote(const MachineName: string): IRoseStateViewCollection;
  end;

// *********************************************************************//
// The Class CoRoseStateDiagramCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseStateDiagramCollection exposed by              
// the CoClass RoseStateDiagramCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseStateDiagramCollection = class
    class function Create: IRoseStateDiagramCollection;
    class function CreateRemote(const MachineName: string): IRoseStateDiagramCollection;
  end;

// *********************************************************************//
// The Class CoRoseEventCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseEventCollection exposed by              
// the CoClass RoseEventCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseEventCollection = class
    class function Create: IRoseEventCollection;
    class function CreateRemote(const MachineName: string): IRoseEventCollection;
  end;

// *********************************************************************//
// The Class CoRoseActionCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseActionCollection exposed by              
// the CoClass RoseActionCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseActionCollection = class
    class function Create: IRoseActionCollection;
    class function CreateRemote(const MachineName: string): IRoseActionCollection;
  end;

// *********************************************************************//
// The Class CoRoseTransitionCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseTransitionCollection exposed by              
// the CoClass RoseTransitionCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseTransitionCollection = class
    class function Create: IRoseTransitionCollection;
    class function CreateRemote(const MachineName: string): IRoseTransitionCollection;
  end;

// *********************************************************************//
// The Class CoRoseStateCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseStateCollection exposed by              
// the CoClass RoseStateCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseStateCollection = class
    class function Create: IRoseStateCollection;
    class function CreateRemote(const MachineName: string): IRoseStateCollection;
  end;

// *********************************************************************//
// The Class CoRoseSwimLaneViewCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseSwimLaneViewCollection exposed by              
// the CoClass RoseSwimLaneViewCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseSwimLaneViewCollection = class
    class function Create: IRoseSwimLaneViewCollection;
    class function CreateRemote(const MachineName: string): IRoseSwimLaneViewCollection;
  end;

// *********************************************************************//
// The Class CoRoseSwimLaneView provides a Create and CreateRemote method to          
// create instances of the default interface IRoseSwimLaneView exposed by              
// the CoClass RoseSwimLaneView. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseSwimLaneView = class
    class function Create: IRoseSwimLaneView;
    class function CreateRemote(const MachineName: string): IRoseSwimLaneView;
  end;

// *********************************************************************//
// The Class CoRoseSyncItemViewCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseSyncItemViewCollection exposed by              
// the CoClass RoseSyncItemViewCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseSyncItemViewCollection = class
    class function Create: IRoseSyncItemViewCollection;
    class function CreateRemote(const MachineName: string): IRoseSyncItemViewCollection;
  end;

// *********************************************************************//
// The Class CoRoseSyncItemView provides a Create and CreateRemote method to          
// create instances of the default interface IRoseSyncItemView exposed by              
// the CoClass RoseSyncItemView. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseSyncItemView = class
    class function Create: IRoseSyncItemView;
    class function CreateRemote(const MachineName: string): IRoseSyncItemView;
  end;

// *********************************************************************//
// The Class CoRoseDecisionViewCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDecisionViewCollection exposed by              
// the CoClass RoseDecisionViewCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDecisionViewCollection = class
    class function Create: IRoseDecisionViewCollection;
    class function CreateRemote(const MachineName: string): IRoseDecisionViewCollection;
  end;

// *********************************************************************//
// The Class CoRoseActivityViewCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseActivityViewCollection exposed by              
// the CoClass RoseActivityViewCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseActivityViewCollection = class
    class function Create: IRoseActivityViewCollection;
    class function CreateRemote(const MachineName: string): IRoseActivityViewCollection;
  end;

// *********************************************************************//
// The Class CoRoseDecisionView provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDecisionView exposed by              
// the CoClass RoseDecisionView. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDecisionView = class
    class function Create: IRoseDecisionView;
    class function CreateRemote(const MachineName: string): IRoseDecisionView;
  end;

// *********************************************************************//
// The Class CoRoseActivityView provides a Create and CreateRemote method to          
// create instances of the default interface IRoseActivityView exposed by              
// the CoClass RoseActivityView. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseActivityView = class
    class function Create: IRoseActivityView;
    class function CreateRemote(const MachineName: string): IRoseActivityView;
  end;

// *********************************************************************//
// The Class CoRoseStateView provides a Create and CreateRemote method to          
// create instances of the default interface IRoseStateView exposed by              
// the CoClass RoseStateView. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseStateView = class
    class function Create: IRoseStateView;
    class function CreateRemote(const MachineName: string): IRoseStateView;
  end;

// *********************************************************************//
// The Class CoRoseStateDiagram provides a Create and CreateRemote method to          
// create instances of the default interface IRoseStateDiagram exposed by              
// the CoClass RoseStateDiagram. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseStateDiagram = class
    class function Create: IRoseStateDiagram;
    class function CreateRemote(const MachineName: string): IRoseStateDiagram;
  end;

// *********************************************************************//
// The Class CoRoseSwimLaneCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseSwimLaneCollection exposed by              
// the CoClass RoseSwimLaneCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseSwimLaneCollection = class
    class function Create: IRoseSwimLaneCollection;
    class function CreateRemote(const MachineName: string): IRoseSwimLaneCollection;
  end;

// *********************************************************************//
// The Class CoRoseSyncItemCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseSyncItemCollection exposed by              
// the CoClass RoseSyncItemCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseSyncItemCollection = class
    class function Create: IRoseSyncItemCollection;
    class function CreateRemote(const MachineName: string): IRoseSyncItemCollection;
  end;

// *********************************************************************//
// The Class CoRoseSyncItem provides a Create and CreateRemote method to          
// create instances of the default interface IRoseSyncItem exposed by              
// the CoClass RoseSyncItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseSyncItem = class
    class function Create: IRoseSyncItem;
    class function CreateRemote(const MachineName: string): IRoseSyncItem;
  end;

// *********************************************************************//
// The Class CoRoseStateMachineOwner provides a Create and CreateRemote method to          
// create instances of the default interface IRoseStateMachineOwner exposed by              
// the CoClass RoseStateMachineOwner. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseStateMachineOwner = class
    class function Create: IRoseStateMachineOwner;
    class function CreateRemote(const MachineName: string): IRoseStateMachineOwner;
  end;

// *********************************************************************//
// The Class CoRoseStateVertexCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseStateVertexCollection exposed by              
// the CoClass RoseStateVertexCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseStateVertexCollection = class
    class function Create: IRoseStateVertexCollection;
    class function CreateRemote(const MachineName: string): IRoseStateVertexCollection;
  end;

// *********************************************************************//
// The Class CoRoseDecisionCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDecisionCollection exposed by              
// the CoClass RoseDecisionCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDecisionCollection = class
    class function Create: IRoseDecisionCollection;
    class function CreateRemote(const MachineName: string): IRoseDecisionCollection;
  end;

// *********************************************************************//
// The Class CoRoseActivityCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseActivityCollection exposed by              
// the CoClass RoseActivityCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseActivityCollection = class
    class function Create: IRoseActivityCollection;
    class function CreateRemote(const MachineName: string): IRoseActivityCollection;
  end;

// *********************************************************************//
// The Class CoRoseAbstractStateCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseAbstractStateCollection exposed by              
// the CoClass RoseAbstractStateCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseAbstractStateCollection = class
    class function Create: IRoseAbstractStateCollection;
    class function CreateRemote(const MachineName: string): IRoseAbstractStateCollection;
  end;

// *********************************************************************//
// The Class CoRoseDecision provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDecision exposed by              
// the CoClass RoseDecision. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDecision = class
    class function Create: IRoseDecision;
    class function CreateRemote(const MachineName: string): IRoseDecision;
  end;

// *********************************************************************//
// The Class CoRoseActivity provides a Create and CreateRemote method to          
// create instances of the default interface IRoseActivity exposed by              
// the CoClass RoseActivity. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseActivity = class
    class function Create: IRoseActivity;
    class function CreateRemote(const MachineName: string): IRoseActivity;
  end;

// *********************************************************************//
// The Class CoRoseAbstractState provides a Create and CreateRemote method to          
// create instances of the default interface IRoseAbstractState exposed by              
// the CoClass RoseAbstractState. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseAbstractState = class
    class function Create: IRoseAbstractState;
    class function CreateRemote(const MachineName: string): IRoseAbstractState;
  end;

// *********************************************************************//
// The Class CoRoseStateVertex provides a Create and CreateRemote method to          
// create instances of the default interface IRoseStateVertex exposed by              
// the CoClass RoseStateVertex. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseStateVertex = class
    class function Create: IRoseStateVertex;
    class function CreateRemote(const MachineName: string): IRoseStateVertex;
  end;

// *********************************************************************//
// The Class CoRoseSwimLane provides a Create and CreateRemote method to          
// create instances of the default interface IRoseSwimLane exposed by              
// the CoClass RoseSwimLane. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseSwimLane = class
    class function Create: IRoseSwimLane;
    class function CreateRemote(const MachineName: string): IRoseSwimLane;
  end;

// *********************************************************************//
// The Class CoRoseStateMachine provides a Create and CreateRemote method to          
// create instances of the default interface IRoseStateMachine exposed by              
// the CoClass RoseStateMachine. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseStateMachine = class
    class function Create: IRoseStateMachine;
    class function CreateRemote(const MachineName: string): IRoseStateMachine;
  end;

// *********************************************************************//
// The Class CoRoseEvent provides a Create and CreateRemote method to          
// create instances of the default interface IRoseEvent exposed by              
// the CoClass RoseEvent. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseEvent = class
    class function Create: IRoseEvent;
    class function CreateRemote(const MachineName: string): IRoseEvent;
  end;

// *********************************************************************//
// The Class CoRoseAction provides a Create and CreateRemote method to          
// create instances of the default interface IRoseAction exposed by              
// the CoClass RoseAction. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseAction = class
    class function Create: IRoseAction;
    class function CreateRemote(const MachineName: string): IRoseAction;
  end;

// *********************************************************************//
// The Class CoRoseTransition provides a Create and CreateRemote method to          
// create instances of the default interface IRoseTransition exposed by              
// the CoClass RoseTransition. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseTransition = class
    class function Create: IRoseTransition;
    class function CreateRemote(const MachineName: string): IRoseTransition;
  end;

// *********************************************************************//
// The Class CoRoseState provides a Create and CreateRemote method to          
// create instances of the default interface IRoseState exposed by              
// the CoClass RoseState. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseState = class
    class function Create: IRoseState;
    class function CreateRemote(const MachineName: string): IRoseState;
  end;

// *********************************************************************//
// The Class CoRoseRichTypeValuesCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseRichTypeValuesCollection exposed by              
// the CoClass RoseRichTypeValuesCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseRichTypeValuesCollection = class
    class function Create: IRoseRichTypeValuesCollection;
    class function CreateRemote(const MachineName: string): IRoseRichTypeValuesCollection;
  end;

// *********************************************************************//
// The Class CoRoseRichType provides a Create and CreateRemote method to          
// create instances of the default interface IRoseRichType exposed by              
// the CoClass RoseRichType. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseRichType = class
    class function Create: IRoseRichType;
    class function CreateRemote(const MachineName: string): IRoseRichType;
  end;

// *********************************************************************//
// The Class CoRoseModuleVisibilityRelationship provides a Create and CreateRemote method to          
// create instances of the default interface IRoseModuleVisibilityRelationship exposed by              
// the CoClass RoseModuleVisibilityRelationship. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseModuleVisibilityRelationship = class
    class function Create: IRoseModuleVisibilityRelationship;
    class function CreateRemote(const MachineName: string): IRoseModuleVisibilityRelationship;
  end;

// *********************************************************************//
// The Class CoRoseSubsystemViewCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseSubsystemViewCollection exposed by              
// the CoClass RoseSubsystemViewCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseSubsystemViewCollection = class
    class function Create: IRoseSubsystemViewCollection;
    class function CreateRemote(const MachineName: string): IRoseSubsystemViewCollection;
  end;

// *********************************************************************//
// The Class CoRoseComponentViewCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseComponentViewCollection exposed by              
// the CoClass RoseComponentViewCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseComponentViewCollection = class
    class function Create: IRoseComponentViewCollection;
    class function CreateRemote(const MachineName: string): IRoseComponentViewCollection;
  end;

// *********************************************************************//
// The Class CoRoseSubsystemView provides a Create and CreateRemote method to          
// create instances of the default interface IRoseSubsystemView exposed by              
// the CoClass RoseSubsystemView. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseSubsystemView = class
    class function Create: IRoseSubsystemView;
    class function CreateRemote(const MachineName: string): IRoseSubsystemView;
  end;

// *********************************************************************//
// The Class CoRoseComponentView provides a Create and CreateRemote method to          
// create instances of the default interface IRoseComponentView exposed by              
// the CoClass RoseComponentView. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseComponentView = class
    class function Create: IRoseComponentView;
    class function CreateRemote(const MachineName: string): IRoseComponentView;
  end;

// *********************************************************************//
// The Class CoRoseModuleDiagram provides a Create and CreateRemote method to          
// create instances of the default interface IRoseModuleDiagram exposed by              
// the CoClass RoseModuleDiagram. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseModuleDiagram = class
    class function Create: IRoseModuleDiagram;
    class function CreateRemote(const MachineName: string): IRoseModuleDiagram;
  end;

// *********************************************************************//
// The Class CoRoseModule provides a Create and CreateRemote method to          
// create instances of the default interface IRoseModule exposed by              
// the CoClass RoseModule. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseModule = class
    class function Create: IRoseModule;
    class function CreateRemote(const MachineName: string): IRoseModule;
  end;

// *********************************************************************//
// The Class CoRoseSubsystem provides a Create and CreateRemote method to          
// create instances of the default interface IRoseSubsystem exposed by              
// the CoClass RoseSubsystem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseSubsystem = class
    class function Create: IRoseSubsystem;
    class function CreateRemote(const MachineName: string): IRoseSubsystem;
  end;

// *********************************************************************//
// The Class CoRoseUseCase provides a Create and CreateRemote method to          
// create instances of the default interface IRoseUseCase exposed by              
// the CoClass RoseUseCase. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseUseCase = class
    class function Create: IRoseUseCase;
    class function CreateRemote(const MachineName: string): IRoseUseCase;
  end;

// *********************************************************************//
// The Class CoRoseDiagramCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDiagramCollection exposed by              
// the CoClass RoseDiagramCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDiagramCollection = class
    class function Create: IRoseDiagramCollection;
    class function CreateRemote(const MachineName: string): IRoseDiagramCollection;
  end;

// *********************************************************************//
// The Class CoRoseLineVertexCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseLineVertexCollection exposed by              
// the CoClass RoseLineVertexCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseLineVertexCollection = class
    class function Create: IRoseLineVertexCollection;
    class function CreateRemote(const MachineName: string): IRoseLineVertexCollection;
  end;

// *********************************************************************//
// The Class CoRoseInstantiateRelationCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseInstantiateRelationCollection exposed by              
// the CoClass RoseInstantiateRelationCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseInstantiateRelationCollection = class
    class function Create: IRoseInstantiateRelationCollection;
    class function CreateRemote(const MachineName: string): IRoseInstantiateRelationCollection;
  end;

// *********************************************************************//
// The Class CoRoseRealizeRelationCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseRealizeRelationCollection exposed by              
// the CoClass RoseRealizeRelationCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseRealizeRelationCollection = class
    class function Create: IRoseRealizeRelationCollection;
    class function CreateRemote(const MachineName: string): IRoseRealizeRelationCollection;
  end;

// *********************************************************************//
// The Class CoRoseItemCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseItemCollection exposed by              
// the CoClass RoseItemCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseItemCollection = class
    class function Create: IRoseItemCollection;
    class function CreateRemote(const MachineName: string): IRoseItemCollection;
  end;

// *********************************************************************//
// The Class CoRoseControllableUnitCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseControllableUnitCollection exposed by              
// the CoClass RoseControllableUnitCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseControllableUnitCollection = class
    class function Create: IRoseControllableUnitCollection;
    class function CreateRemote(const MachineName: string): IRoseControllableUnitCollection;
  end;

// *********************************************************************//
// The Class CoRosePackageCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRosePackageCollection exposed by              
// the CoClass RosePackageCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRosePackageCollection = class
    class function Create: IRosePackageCollection;
    class function CreateRemote(const MachineName: string): IRosePackageCollection;
  end;

// *********************************************************************//
// The Class CoRoseNoteViewCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseNoteViewCollection exposed by              
// the CoClass RoseNoteViewCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseNoteViewCollection = class
    class function Create: IRoseNoteViewCollection;
    class function CreateRemote(const MachineName: string): IRoseNoteViewCollection;
  end;

// *********************************************************************//
// The Class CoRoseExternalDocumentCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseExternalDocumentCollection exposed by              
// the CoClass RoseExternalDocumentCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseExternalDocumentCollection = class
    class function Create: IRoseExternalDocumentCollection;
    class function CreateRemote(const MachineName: string): IRoseExternalDocumentCollection;
  end;

// *********************************************************************//
// The Class CoRoseUseCaseCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseUseCaseCollection exposed by              
// the CoClass RoseUseCaseCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseUseCaseCollection = class
    class function Create: IRoseUseCaseCollection;
    class function CreateRemote(const MachineName: string): IRoseUseCaseCollection;
  end;

// *********************************************************************//
// The Class CoRoseProcessCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseProcessCollection exposed by              
// the CoClass RoseProcessCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseProcessCollection = class
    class function Create: IRoseProcessCollection;
    class function CreateRemote(const MachineName: string): IRoseProcessCollection;
  end;

// *********************************************************************//
// The Class CoRoseModuleVisibilityRelationshipCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseModuleVisibilityRelationshipCollection exposed by              
// the CoClass RoseModuleVisibilityRelationshipCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseModuleVisibilityRelationshipCollection = class
    class function Create: IRoseModuleVisibilityRelationshipCollection;
    class function CreateRemote(const MachineName: string): IRoseModuleVisibilityRelationshipCollection;
  end;

// *********************************************************************//
// The Class CoRoseItemViewCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseItemViewCollection exposed by              
// the CoClass RoseItemViewCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseItemViewCollection = class
    class function Create: IRoseItemViewCollection;
    class function CreateRemote(const MachineName: string): IRoseItemViewCollection;
  end;

// *********************************************************************//
// The Class CoRoseScenarioDiagramCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseScenarioDiagramCollection exposed by              
// the CoClass RoseScenarioDiagramCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseScenarioDiagramCollection = class
    class function Create: IRoseScenarioDiagramCollection;
    class function CreateRemote(const MachineName: string): IRoseScenarioDiagramCollection;
  end;

// *********************************************************************//
// The Class CoRosePropertyCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRosePropertyCollection exposed by              
// the CoClass RosePropertyCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRosePropertyCollection = class
    class function Create: IRosePropertyCollection;
    class function CreateRemote(const MachineName: string): IRosePropertyCollection;
  end;

// *********************************************************************//
// The Class CoRoseProcessorCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseProcessorCollection exposed by              
// the CoClass RoseProcessorCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseProcessorCollection = class
    class function Create: IRoseProcessorCollection;
    class function CreateRemote(const MachineName: string): IRoseProcessorCollection;
  end;

// *********************************************************************//
// The Class CoRoseObjectInstanceCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseObjectInstanceCollection exposed by              
// the CoClass RoseObjectInstanceCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseObjectInstanceCollection = class
    class function Create: IRoseObjectInstanceCollection;
    class function CreateRemote(const MachineName: string): IRoseObjectInstanceCollection;
  end;

// *********************************************************************//
// The Class CoRoseMessageCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseMessageCollection exposed by              
// the CoClass RoseMessageCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseMessageCollection = class
    class function Create: IRoseMessageCollection;
    class function CreateRemote(const MachineName: string): IRoseMessageCollection;
  end;

// *********************************************************************//
// The Class CoRoseInheritRelationCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseInheritRelationCollection exposed by              
// the CoClass RoseInheritRelationCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseInheritRelationCollection = class
    class function Create: IRoseInheritRelationCollection;
    class function CreateRemote(const MachineName: string): IRoseInheritRelationCollection;
  end;

// *********************************************************************//
// The Class CoRoseRoleCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseRoleCollection exposed by              
// the CoClass RoseRoleCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseRoleCollection = class
    class function Create: IRoseRoleCollection;
    class function CreateRemote(const MachineName: string): IRoseRoleCollection;
  end;

// *********************************************************************//
// The Class CoRoseParameterCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseParameterCollection exposed by              
// the CoClass RoseParameterCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseParameterCollection = class
    class function Create: IRoseParameterCollection;
    class function CreateRemote(const MachineName: string): IRoseParameterCollection;
  end;

// *********************************************************************//
// The Class CoRoseHasRelationshipCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseHasRelationshipCollection exposed by              
// the CoClass RoseHasRelationshipCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseHasRelationshipCollection = class
    class function Create: IRoseHasRelationshipCollection;
    class function CreateRemote(const MachineName: string): IRoseHasRelationshipCollection;
  end;

// *********************************************************************//
// The Class CoRoseAssociationCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseAssociationCollection exposed by              
// the CoClass RoseAssociationCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseAssociationCollection = class
    class function Create: IRoseAssociationCollection;
    class function CreateRemote(const MachineName: string): IRoseAssociationCollection;
  end;

// *********************************************************************//
// The Class CoRoseOperationCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseOperationCollection exposed by              
// the CoClass RoseOperationCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseOperationCollection = class
    class function Create: IRoseOperationCollection;
    class function CreateRemote(const MachineName: string): IRoseOperationCollection;
  end;

// *********************************************************************//
// The Class CoRoseAttributeCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseAttributeCollection exposed by              
// the CoClass RoseAttributeCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseAttributeCollection = class
    class function Create: IRoseAttributeCollection;
    class function CreateRemote(const MachineName: string): IRoseAttributeCollection;
  end;

// *********************************************************************//
// The Class CoRoseModuleCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseModuleCollection exposed by              
// the CoClass RoseModuleCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseModuleCollection = class
    class function Create: IRoseModuleCollection;
    class function CreateRemote(const MachineName: string): IRoseModuleCollection;
  end;

// *********************************************************************//
// The Class CoRoseSubsystemCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseSubsystemCollection exposed by              
// the CoClass RoseSubsystemCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseSubsystemCollection = class
    class function Create: IRoseSubsystemCollection;
    class function CreateRemote(const MachineName: string): IRoseSubsystemCollection;
  end;

// *********************************************************************//
// The Class CoRoseCategoryCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseCategoryCollection exposed by              
// the CoClass RoseCategoryCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseCategoryCollection = class
    class function Create: IRoseCategoryCollection;
    class function CreateRemote(const MachineName: string): IRoseCategoryCollection;
  end;

// *********************************************************************//
// The Class CoRoseClassCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseClassCollection exposed by              
// the CoClass RoseClassCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseClassCollection = class
    class function Create: IRoseClassCollection;
    class function CreateRemote(const MachineName: string): IRoseClassCollection;
  end;

// *********************************************************************//
// The Class CoRoseModuleDiagramCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseModuleDiagramCollection exposed by              
// the CoClass RoseModuleDiagramCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseModuleDiagramCollection = class
    class function Create: IRoseModuleDiagramCollection;
    class function CreateRemote(const MachineName: string): IRoseModuleDiagramCollection;
  end;

// *********************************************************************//
// The Class CoRoseClassDiagramCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseClassDiagramCollection exposed by              
// the CoClass RoseClassDiagramCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseClassDiagramCollection = class
    class function Create: IRoseClassDiagramCollection;
    class function CreateRemote(const MachineName: string): IRoseClassDiagramCollection;
  end;

// *********************************************************************//
// The Class CoRoseDeviceCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDeviceCollection exposed by              
// the CoClass RoseDeviceCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDeviceCollection = class
    class function Create: IRoseDeviceCollection;
    class function CreateRemote(const MachineName: string): IRoseDeviceCollection;
  end;

// *********************************************************************//
// The Class CoRoseDeploymentDiagramCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDeploymentDiagramCollection exposed by              
// the CoClass RoseDeploymentDiagramCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDeploymentDiagramCollection = class
    class function Create: IRoseDeploymentDiagramCollection;
    class function CreateRemote(const MachineName: string): IRoseDeploymentDiagramCollection;
  end;

// *********************************************************************//
// The Class CoRoseClassViewCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseClassViewCollection exposed by              
// the CoClass RoseClassViewCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseClassViewCollection = class
    class function Create: IRoseClassViewCollection;
    class function CreateRemote(const MachineName: string): IRoseClassViewCollection;
  end;

// *********************************************************************//
// The Class CoRoseDeploymentDiagram provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDeploymentDiagram exposed by              
// the CoClass RoseDeploymentDiagram. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDeploymentDiagram = class
    class function Create: IRoseDeploymentDiagram;
    class function CreateRemote(const MachineName: string): IRoseDeploymentDiagram;
  end;

// *********************************************************************//
// The Class CoRoseProcess provides a Create and CreateRemote method to          
// create instances of the default interface IRoseProcess exposed by              
// the CoClass RoseProcess. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseProcess = class
    class function Create: IRoseProcess;
    class function CreateRemote(const MachineName: string): IRoseProcess;
  end;

// *********************************************************************//
// The Class CoRoseDevice provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDevice exposed by              
// the CoClass RoseDevice. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDevice = class
    class function Create: IRoseDevice;
    class function CreateRemote(const MachineName: string): IRoseDevice;
  end;

// *********************************************************************//
// The Class CoRoseProcessor provides a Create and CreateRemote method to          
// create instances of the default interface IRoseProcessor exposed by              
// the CoClass RoseProcessor. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseProcessor = class
    class function Create: IRoseProcessor;
    class function CreateRemote(const MachineName: string): IRoseProcessor;
  end;

// *********************************************************************//
// The Class CoRoseInstanceViewCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseInstanceViewCollection exposed by              
// the CoClass RoseInstanceViewCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseInstanceViewCollection = class
    class function Create: IRoseInstanceViewCollection;
    class function CreateRemote(const MachineName: string): IRoseInstanceViewCollection;
  end;

// *********************************************************************//
// The Class CoRoseInstanceView provides a Create and CreateRemote method to          
// create instances of the default interface IRoseInstanceView exposed by              
// the CoClass RoseInstanceView. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseInstanceView = class
    class function Create: IRoseInstanceView;
    class function CreateRemote(const MachineName: string): IRoseInstanceView;
  end;

// *********************************************************************//
// The Class CoRoseLinkCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseLinkCollection exposed by              
// the CoClass RoseLinkCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseLinkCollection = class
    class function Create: IRoseLinkCollection;
    class function CreateRemote(const MachineName: string): IRoseLinkCollection;
  end;

// *********************************************************************//
// The Class CoRoseLink provides a Create and CreateRemote method to          
// create instances of the default interface IRoseLink exposed by              
// the CoClass RoseLink. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseLink = class
    class function Create: IRoseLink;
    class function CreateRemote(const MachineName: string): IRoseLink;
  end;

// *********************************************************************//
// The Class CoRoseScenarioDiagram provides a Create and CreateRemote method to          
// create instances of the default interface IRoseScenarioDiagram exposed by              
// the CoClass RoseScenarioDiagram. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseScenarioDiagram = class
    class function Create: IRoseScenarioDiagram;
    class function CreateRemote(const MachineName: string): IRoseScenarioDiagram;
  end;

// *********************************************************************//
// The Class CoRoseMessage provides a Create and CreateRemote method to          
// create instances of the default interface IRoseMessage exposed by              
// the CoClass RoseMessage. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseMessage = class
    class function Create: IRoseMessage;
    class function CreateRemote(const MachineName: string): IRoseMessage;
  end;

// *********************************************************************//
// The Class CoRoseObjectInstance provides a Create and CreateRemote method to          
// create instances of the default interface IRoseObjectInstance exposed by              
// the CoClass RoseObjectInstance. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseObjectInstance = class
    class function Create: IRoseObjectInstance;
    class function CreateRemote(const MachineName: string): IRoseObjectInstance;
  end;

// *********************************************************************//
// The Class CoRoseConnectionRelationCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseConnectionRelationCollection exposed by              
// the CoClass RoseConnectionRelationCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseConnectionRelationCollection = class
    class function Create: IRoseConnectionRelationCollection;
    class function CreateRemote(const MachineName: string): IRoseConnectionRelationCollection;
  end;

// *********************************************************************//
// The Class CoRoseConnectionRelation provides a Create and CreateRemote method to          
// create instances of the default interface IRoseConnectionRelation exposed by              
// the CoClass RoseConnectionRelation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseConnectionRelation = class
    class function Create: IRoseConnectionRelation;
    class function CreateRemote(const MachineName: string): IRoseConnectionRelation;
  end;

// *********************************************************************//
// The Class CoRoseInstantiateRelation provides a Create and CreateRemote method to          
// create instances of the default interface IRoseInstantiateRelation exposed by              
// the CoClass RoseInstantiateRelation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseInstantiateRelation = class
    class function Create: IRoseInstantiateRelation;
    class function CreateRemote(const MachineName: string): IRoseInstantiateRelation;
  end;

// *********************************************************************//
// The Class CoRoseClassDependencyCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseClassDependencyCollection exposed by              
// the CoClass RoseClassDependencyCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseClassDependencyCollection = class
    class function Create: IRoseClassDependencyCollection;
    class function CreateRemote(const MachineName: string): IRoseClassDependencyCollection;
  end;

// *********************************************************************//
// The Class CoRoseCategoryDependencyCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseCategoryDependencyCollection exposed by              
// the CoClass RoseCategoryDependencyCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseCategoryDependencyCollection = class
    class function Create: IRoseCategoryDependencyCollection;
    class function CreateRemote(const MachineName: string): IRoseCategoryDependencyCollection;
  end;

// *********************************************************************//
// The Class CoRoseClassDependency provides a Create and CreateRemote method to          
// create instances of the default interface IRoseClassDependency exposed by              
// the CoClass RoseClassDependency. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseClassDependency = class
    class function Create: IRoseClassDependency;
    class function CreateRemote(const MachineName: string): IRoseClassDependency;
  end;

// *********************************************************************//
// The Class CoRoseCategoryDependency provides a Create and CreateRemote method to          
// create instances of the default interface IRoseCategoryDependency exposed by              
// the CoClass RoseCategoryDependency. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseCategoryDependency = class
    class function Create: IRoseCategoryDependency;
    class function CreateRemote(const MachineName: string): IRoseCategoryDependency;
  end;

// *********************************************************************//
// The Class CoRoseClassRelation provides a Create and CreateRemote method to          
// create instances of the default interface IRoseClassRelation exposed by              
// the CoClass RoseClassRelation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseClassRelation = class
    class function Create: IRoseClassRelation;
    class function CreateRemote(const MachineName: string): IRoseClassRelation;
  end;

// *********************************************************************//
// The Class CoRoseRealizeRelation provides a Create and CreateRemote method to          
// create instances of the default interface IRoseRealizeRelation exposed by              
// the CoClass RoseRealizeRelation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseRealizeRelation = class
    class function Create: IRoseRealizeRelation;
    class function CreateRemote(const MachineName: string): IRoseRealizeRelation;
  end;

// *********************************************************************//
// The Class CoRoseInheritRelation provides a Create and CreateRemote method to          
// create instances of the default interface IRoseInheritRelation exposed by              
// the CoClass RoseInheritRelation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseInheritRelation = class
    class function Create: IRoseInheritRelation;
    class function CreateRemote(const MachineName: string): IRoseInheritRelation;
  end;

// *********************************************************************//
// The Class CoRoseHasRelationship provides a Create and CreateRemote method to          
// create instances of the default interface IRoseHasRelationship exposed by              
// the CoClass RoseHasRelationship. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseHasRelationship = class
    class function Create: IRoseHasRelationship;
    class function CreateRemote(const MachineName: string): IRoseHasRelationship;
  end;

// *********************************************************************//
// The Class CoRoseRole provides a Create and CreateRemote method to          
// create instances of the default interface IRoseRole exposed by              
// the CoClass RoseRole. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseRole = class
    class function Create: IRoseRole;
    class function CreateRemote(const MachineName: string): IRoseRole;
  end;

// *********************************************************************//
// The Class CoRoseAssociation provides a Create and CreateRemote method to          
// create instances of the default interface IRoseAssociation exposed by              
// the CoClass RoseAssociation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseAssociation = class
    class function Create: IRoseAssociation;
    class function CreateRemote(const MachineName: string): IRoseAssociation;
  end;

// *********************************************************************//
// The Class CoRoseParameter provides a Create and CreateRemote method to          
// create instances of the default interface IRoseParameter exposed by              
// the CoClass RoseParameter. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseParameter = class
    class function Create: IRoseParameter;
    class function CreateRemote(const MachineName: string): IRoseParameter;
  end;

// *********************************************************************//
// The Class CoRoseOperation provides a Create and CreateRemote method to          
// create instances of the default interface IRoseOperation exposed by              
// the CoClass RoseOperation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseOperation = class
    class function Create: IRoseOperation;
    class function CreateRemote(const MachineName: string): IRoseOperation;
  end;

// *********************************************************************//
// The Class CoRoseAttribute provides a Create and CreateRemote method to          
// create instances of the default interface IRoseAttribute exposed by              
// the CoClass RoseAttribute. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseAttribute = class
    class function Create: IRoseAttribute;
    class function CreateRemote(const MachineName: string): IRoseAttribute;
  end;

// *********************************************************************//
// The Class CoRoseClassView provides a Create and CreateRemote method to          
// create instances of the default interface IRoseClassView exposed by              
// the CoClass RoseClassView. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseClassView = class
    class function Create: IRoseClassView;
    class function CreateRemote(const MachineName: string): IRoseClassView;
  end;

// *********************************************************************//
// The Class CoRoseClassDiagram provides a Create and CreateRemote method to          
// create instances of the default interface IRoseClassDiagram exposed by              
// the CoClass RoseClassDiagram. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseClassDiagram = class
    class function Create: IRoseClassDiagram;
    class function CreateRemote(const MachineName: string): IRoseClassDiagram;
  end;

// *********************************************************************//
// The Class CoRoseClass provides a Create and CreateRemote method to          
// create instances of the default interface IRoseClass exposed by              
// the CoClass RoseClass. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseClass = class
    class function Create: IRoseClass;
    class function CreateRemote(const MachineName: string): IRoseClass;
  end;

// *********************************************************************//
// The Class CoRoseCategory provides a Create and CreateRemote method to          
// create instances of the default interface IRoseCategory exposed by              
// the CoClass RoseCategory. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseCategory = class
    class function Create: IRoseCategory;
    class function CreateRemote(const MachineName: string): IRoseCategory;
  end;

// *********************************************************************//
// The Class CoRoseView_Font provides a Create and CreateRemote method to          
// create instances of the default interface IRoseView_Font exposed by              
// the CoClass RoseView_Font. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseView_Font = class
    class function Create: IRoseView_Font;
    class function CreateRemote(const MachineName: string): IRoseView_Font;
  end;

// *********************************************************************//
// The Class CoRoseView_LineColor provides a Create and CreateRemote method to          
// create instances of the default interface IRoseView_LineColor exposed by              
// the CoClass RoseView_LineColor. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseView_LineColor = class
    class function Create: IRoseView_LineColor;
    class function CreateRemote(const MachineName: string): IRoseView_LineColor;
  end;

// *********************************************************************//
// The Class CoRoseView_FillColor provides a Create and CreateRemote method to          
// create instances of the default interface IRoseView_FillColor exposed by              
// the CoClass RoseView_FillColor. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseView_FillColor = class
    class function Create: IRoseView_FillColor;
    class function CreateRemote(const MachineName: string): IRoseView_FillColor;
  end;

// *********************************************************************//
// The Class CoRoseNoteView provides a Create and CreateRemote method to          
// create instances of the default interface IRoseNoteView exposed by              
// the CoClass RoseNoteView. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseNoteView = class
    class function Create: IRoseNoteView;
    class function CreateRemote(const MachineName: string): IRoseNoteView;
  end;

// *********************************************************************//
// The Class CoRoseItemView provides a Create and CreateRemote method to          
// create instances of the default interface IRoseItemView exposed by              
// the CoClass RoseItemView. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseItemView = class
    class function Create: IRoseItemView;
    class function CreateRemote(const MachineName: string): IRoseItemView;
  end;

// *********************************************************************//
// The Class CoRoseDiagram provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDiagram exposed by              
// the CoClass RoseDiagram. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDiagram = class
    class function Create: IRoseDiagram;
    class function CreateRemote(const MachineName: string): IRoseDiagram;
  end;

// *********************************************************************//
// The Class CoRoseStringCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseStringCollection exposed by              
// the CoClass RoseStringCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseStringCollection = class
    class function Create: IRoseStringCollection;
    class function CreateRemote(const MachineName: string): IRoseStringCollection;
  end;

// *********************************************************************//
// The Class CoRoseObjectFlowCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseObjectFlowCollection exposed by              
// the CoClass RoseObjectFlowCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseObjectFlowCollection = class
    class function Create: IRoseObjectFlowCollection;
    class function CreateRemote(const MachineName: string): IRoseObjectFlowCollection;
  end;

// *********************************************************************//
// The Class CoRoseDependencyRelationCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDependencyRelationCollection exposed by              
// the CoClass RoseDependencyRelationCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDependencyRelationCollection = class
    class function Create: IRoseDependencyRelationCollection;
    class function CreateRemote(const MachineName: string): IRoseDependencyRelationCollection;
  end;

// *********************************************************************//
// The Class CoRoseObjectFlow provides a Create and CreateRemote method to          
// create instances of the default interface IRoseObjectFlow exposed by              
// the CoClass RoseObjectFlow. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseObjectFlow = class
    class function Create: IRoseObjectFlow;
    class function CreateRemote(const MachineName: string): IRoseObjectFlow;
  end;

// *********************************************************************//
// The Class CoRoseDependencyRelation provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDependencyRelation exposed by              
// the CoClass RoseDependencyRelation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDependencyRelation = class
    class function Create: IRoseDependencyRelation;
    class function CreateRemote(const MachineName: string): IRoseDependencyRelation;
  end;

// *********************************************************************//
// The Class CoRoseDeploymentUnit provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDeploymentUnit exposed by              
// the CoClass RoseDeploymentUnit. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDeploymentUnit = class
    class function Create: IRoseDeploymentUnit;
    class function CreateRemote(const MachineName: string): IRoseDeploymentUnit;
  end;

// *********************************************************************//
// The Class CoRoseRelation provides a Create and CreateRemote method to          
// create instances of the default interface IRoseRelation exposed by              
// the CoClass RoseRelation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseRelation = class
    class function Create: IRoseRelation;
    class function CreateRemote(const MachineName: string): IRoseRelation;
  end;

// *********************************************************************//
// The Class CoRoseDefaultModelProperties provides a Create and CreateRemote method to          
// create instances of the default interface IRoseDefaultModelProperties exposed by              
// the CoClass RoseDefaultModelProperties. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseDefaultModelProperties = class
    class function Create: IRoseDefaultModelProperties;
    class function CreateRemote(const MachineName: string): IRoseDefaultModelProperties;
  end;

// *********************************************************************//
// The Class CoRoseProperty provides a Create and CreateRemote method to          
// create instances of the default interface IRoseProperty exposed by              
// the CoClass RoseProperty. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseProperty = class
    class function Create: IRoseProperty;
    class function CreateRemote(const MachineName: string): IRoseProperty;
  end;

// *********************************************************************//
// The Class CoRoseElement provides a Create and CreateRemote method to          
// create instances of the default interface IRoseElement exposed by              
// the CoClass RoseElement. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseElement = class
    class function Create: IRoseElement;
    class function CreateRemote(const MachineName: string): IRoseElement;
  end;

// *********************************************************************//
// The Class CoRosePackage provides a Create and CreateRemote method to          
// create instances of the default interface IRosePackage exposed by              
// the CoClass RosePackage. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRosePackage = class
    class function Create: IRosePackage;
    class function CreateRemote(const MachineName: string): IRosePackage;
  end;

// *********************************************************************//
// The Class CoRoseControllableUnit provides a Create and CreateRemote method to          
// create instances of the default interface IRoseControllableUnit exposed by              
// the CoClass RoseControllableUnit. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseControllableUnit = class
    class function Create: IRoseControllableUnit;
    class function CreateRemote(const MachineName: string): IRoseControllableUnit;
  end;

// *********************************************************************//
// The Class CoRoseExternalDocument provides a Create and CreateRemote method to          
// create instances of the default interface IRoseExternalDocument exposed by              
// the CoClass RoseExternalDocument. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseExternalDocument = class
    class function Create: IRoseExternalDocument;
    class function CreateRemote(const MachineName: string): IRoseExternalDocument;
  end;

// *********************************************************************//
// The Class CoRoseItem provides a Create and CreateRemote method to          
// create instances of the default interface IRoseItem exposed by              
// the CoClass RoseItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseItem = class
    class function Create: IRoseItem;
    class function CreateRemote(const MachineName: string): IRoseItem;
  end;

// *********************************************************************//
// The Class CoRoseModel provides a Create and CreateRemote method to          
// create instances of the default interface IRoseModel exposed by              
// the CoClass RoseModel. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseModel = class
    class function Create: IRoseModel;
    class function CreateRemote(const MachineName: string): IRoseModel;
  end;

// *********************************************************************//
// The Class CoRoseContextMenuItemCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseContextMenuItemCollection exposed by              
// the CoClass RoseContextMenuItemCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseContextMenuItemCollection = class
    class function Create: IRoseContextMenuItemCollection;
    class function CreateRemote(const MachineName: string): IRoseContextMenuItemCollection;
  end;

// *********************************************************************//
// The Class CoRoseContextMenuItem provides a Create and CreateRemote method to          
// create instances of the default interface IRoseContextMenuItem exposed by              
// the CoClass RoseContextMenuItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseContextMenuItem = class
    class function Create: IRoseContextMenuItem;
    class function CreateRemote(const MachineName: string): IRoseContextMenuItem;
  end;

// *********************************************************************//
// The Class CoRoseAddInCollection provides a Create and CreateRemote method to          
// create instances of the default interface IRoseAddInCollection exposed by              
// the CoClass RoseAddInCollection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseAddInCollection = class
    class function Create: IRoseAddInCollection;
    class function CreateRemote(const MachineName: string): IRoseAddInCollection;
  end;

// *********************************************************************//
// The Class CoRoseAddIn provides a Create and CreateRemote method to          
// create instances of the default interface IRoseAddIn exposed by              
// the CoClass RoseAddIn. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseAddIn = class
    class function Create: IRoseAddIn;
    class function CreateRemote(const MachineName: string): IRoseAddIn;
  end;

// *********************************************************************//
// The Class CoRoseAddInManager provides a Create and CreateRemote method to          
// create instances of the default interface IRoseAddInManager exposed by              
// the CoClass RoseAddInManager. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseAddInManager = class
    class function Create: IRoseAddInManager;
    class function CreateRemote(const MachineName: string): IRoseAddInManager;
  end;

// *********************************************************************//
// The Class CoRosePathMap provides a Create and CreateRemote method to          
// create instances of the default interface IRosePathMap exposed by              
// the CoClass RosePathMap. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRosePathMap = class
    class function Create: IRosePathMap;
    class function CreateRemote(const MachineName: string): IRosePathMap;
  end;

// *********************************************************************//
// The Class CoRoseApplication provides a Create and CreateRemote method to          
// create instances of the default interface IRoseApplication exposed by              
// the CoClass RoseApplication. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRoseApplication = class
    class function Create: IRoseApplication;
    class function CreateRemote(const MachineName: string): IRoseApplication;
  end;

implementation

uses ComObj;

class function CoRoseLineVertex.Create: IRoseLineVertex;
begin
  Result := CreateComObject(CLASS_RoseLineVertex) as IRoseLineVertex;
end;

class function CoRoseLineVertex.CreateRemote(const MachineName: string): IRoseLineVertex;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseLineVertex) as IRoseLineVertex;
end;

class function CoRoseObject.Create: IRoseObject;
begin
  Result := CreateComObject(CLASS_RoseObject) as IRoseObject;
end;

class function CoRoseObject.CreateRemote(const MachineName: string): IRoseObject;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseObject) as IRoseObject;
end;

class function CoRoseStateMachineCollection.Create: IRoseStateMachineCollection;
begin
  Result := CreateComObject(CLASS_RoseStateMachineCollection) as IRoseStateMachineCollection;
end;

class function CoRoseStateMachineCollection.CreateRemote(const MachineName: string): IRoseStateMachineCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateMachineCollection) as IRoseStateMachineCollection;
end;

class function CoRoseStateViewCollection.Create: IRoseStateViewCollection;
begin
  Result := CreateComObject(CLASS_RoseStateViewCollection) as IRoseStateViewCollection;
end;

class function CoRoseStateViewCollection.CreateRemote(const MachineName: string): IRoseStateViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateViewCollection) as IRoseStateViewCollection;
end;

class function CoRoseStateDiagramCollection.Create: IRoseStateDiagramCollection;
begin
  Result := CreateComObject(CLASS_RoseStateDiagramCollection) as IRoseStateDiagramCollection;
end;

class function CoRoseStateDiagramCollection.CreateRemote(const MachineName: string): IRoseStateDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateDiagramCollection) as IRoseStateDiagramCollection;
end;

class function CoRoseEventCollection.Create: IRoseEventCollection;
begin
  Result := CreateComObject(CLASS_RoseEventCollection) as IRoseEventCollection;
end;

class function CoRoseEventCollection.CreateRemote(const MachineName: string): IRoseEventCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseEventCollection) as IRoseEventCollection;
end;

class function CoRoseActionCollection.Create: IRoseActionCollection;
begin
  Result := CreateComObject(CLASS_RoseActionCollection) as IRoseActionCollection;
end;

class function CoRoseActionCollection.CreateRemote(const MachineName: string): IRoseActionCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseActionCollection) as IRoseActionCollection;
end;

class function CoRoseTransitionCollection.Create: IRoseTransitionCollection;
begin
  Result := CreateComObject(CLASS_RoseTransitionCollection) as IRoseTransitionCollection;
end;

class function CoRoseTransitionCollection.CreateRemote(const MachineName: string): IRoseTransitionCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseTransitionCollection) as IRoseTransitionCollection;
end;

class function CoRoseStateCollection.Create: IRoseStateCollection;
begin
  Result := CreateComObject(CLASS_RoseStateCollection) as IRoseStateCollection;
end;

class function CoRoseStateCollection.CreateRemote(const MachineName: string): IRoseStateCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateCollection) as IRoseStateCollection;
end;

class function CoRoseSwimLaneViewCollection.Create: IRoseSwimLaneViewCollection;
begin
  Result := CreateComObject(CLASS_RoseSwimLaneViewCollection) as IRoseSwimLaneViewCollection;
end;

class function CoRoseSwimLaneViewCollection.CreateRemote(const MachineName: string): IRoseSwimLaneViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSwimLaneViewCollection) as IRoseSwimLaneViewCollection;
end;

class function CoRoseSwimLaneView.Create: IRoseSwimLaneView;
begin
  Result := CreateComObject(CLASS_RoseSwimLaneView) as IRoseSwimLaneView;
end;

class function CoRoseSwimLaneView.CreateRemote(const MachineName: string): IRoseSwimLaneView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSwimLaneView) as IRoseSwimLaneView;
end;

class function CoRoseSyncItemViewCollection.Create: IRoseSyncItemViewCollection;
begin
  Result := CreateComObject(CLASS_RoseSyncItemViewCollection) as IRoseSyncItemViewCollection;
end;

class function CoRoseSyncItemViewCollection.CreateRemote(const MachineName: string): IRoseSyncItemViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSyncItemViewCollection) as IRoseSyncItemViewCollection;
end;

class function CoRoseSyncItemView.Create: IRoseSyncItemView;
begin
  Result := CreateComObject(CLASS_RoseSyncItemView) as IRoseSyncItemView;
end;

class function CoRoseSyncItemView.CreateRemote(const MachineName: string): IRoseSyncItemView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSyncItemView) as IRoseSyncItemView;
end;

class function CoRoseDecisionViewCollection.Create: IRoseDecisionViewCollection;
begin
  Result := CreateComObject(CLASS_RoseDecisionViewCollection) as IRoseDecisionViewCollection;
end;

class function CoRoseDecisionViewCollection.CreateRemote(const MachineName: string): IRoseDecisionViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDecisionViewCollection) as IRoseDecisionViewCollection;
end;

class function CoRoseActivityViewCollection.Create: IRoseActivityViewCollection;
begin
  Result := CreateComObject(CLASS_RoseActivityViewCollection) as IRoseActivityViewCollection;
end;

class function CoRoseActivityViewCollection.CreateRemote(const MachineName: string): IRoseActivityViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseActivityViewCollection) as IRoseActivityViewCollection;
end;

class function CoRoseDecisionView.Create: IRoseDecisionView;
begin
  Result := CreateComObject(CLASS_RoseDecisionView) as IRoseDecisionView;
end;

class function CoRoseDecisionView.CreateRemote(const MachineName: string): IRoseDecisionView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDecisionView) as IRoseDecisionView;
end;

class function CoRoseActivityView.Create: IRoseActivityView;
begin
  Result := CreateComObject(CLASS_RoseActivityView) as IRoseActivityView;
end;

class function CoRoseActivityView.CreateRemote(const MachineName: string): IRoseActivityView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseActivityView) as IRoseActivityView;
end;

class function CoRoseStateView.Create: IRoseStateView;
begin
  Result := CreateComObject(CLASS_RoseStateView) as IRoseStateView;
end;

class function CoRoseStateView.CreateRemote(const MachineName: string): IRoseStateView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateView) as IRoseStateView;
end;

class function CoRoseStateDiagram.Create: IRoseStateDiagram;
begin
  Result := CreateComObject(CLASS_RoseStateDiagram) as IRoseStateDiagram;
end;

class function CoRoseStateDiagram.CreateRemote(const MachineName: string): IRoseStateDiagram;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateDiagram) as IRoseStateDiagram;
end;

class function CoRoseSwimLaneCollection.Create: IRoseSwimLaneCollection;
begin
  Result := CreateComObject(CLASS_RoseSwimLaneCollection) as IRoseSwimLaneCollection;
end;

class function CoRoseSwimLaneCollection.CreateRemote(const MachineName: string): IRoseSwimLaneCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSwimLaneCollection) as IRoseSwimLaneCollection;
end;

class function CoRoseSyncItemCollection.Create: IRoseSyncItemCollection;
begin
  Result := CreateComObject(CLASS_RoseSyncItemCollection) as IRoseSyncItemCollection;
end;

class function CoRoseSyncItemCollection.CreateRemote(const MachineName: string): IRoseSyncItemCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSyncItemCollection) as IRoseSyncItemCollection;
end;

class function CoRoseSyncItem.Create: IRoseSyncItem;
begin
  Result := CreateComObject(CLASS_RoseSyncItem) as IRoseSyncItem;
end;

class function CoRoseSyncItem.CreateRemote(const MachineName: string): IRoseSyncItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSyncItem) as IRoseSyncItem;
end;

class function CoRoseStateMachineOwner.Create: IRoseStateMachineOwner;
begin
  Result := CreateComObject(CLASS_RoseStateMachineOwner) as IRoseStateMachineOwner;
end;

class function CoRoseStateMachineOwner.CreateRemote(const MachineName: string): IRoseStateMachineOwner;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateMachineOwner) as IRoseStateMachineOwner;
end;

class function CoRoseStateVertexCollection.Create: IRoseStateVertexCollection;
begin
  Result := CreateComObject(CLASS_RoseStateVertexCollection) as IRoseStateVertexCollection;
end;

class function CoRoseStateVertexCollection.CreateRemote(const MachineName: string): IRoseStateVertexCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateVertexCollection) as IRoseStateVertexCollection;
end;

class function CoRoseDecisionCollection.Create: IRoseDecisionCollection;
begin
  Result := CreateComObject(CLASS_RoseDecisionCollection) as IRoseDecisionCollection;
end;

class function CoRoseDecisionCollection.CreateRemote(const MachineName: string): IRoseDecisionCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDecisionCollection) as IRoseDecisionCollection;
end;

class function CoRoseActivityCollection.Create: IRoseActivityCollection;
begin
  Result := CreateComObject(CLASS_RoseActivityCollection) as IRoseActivityCollection;
end;

class function CoRoseActivityCollection.CreateRemote(const MachineName: string): IRoseActivityCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseActivityCollection) as IRoseActivityCollection;
end;

class function CoRoseAbstractStateCollection.Create: IRoseAbstractStateCollection;
begin
  Result := CreateComObject(CLASS_RoseAbstractStateCollection) as IRoseAbstractStateCollection;
end;

class function CoRoseAbstractStateCollection.CreateRemote(const MachineName: string): IRoseAbstractStateCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAbstractStateCollection) as IRoseAbstractStateCollection;
end;

class function CoRoseDecision.Create: IRoseDecision;
begin
  Result := CreateComObject(CLASS_RoseDecision) as IRoseDecision;
end;

class function CoRoseDecision.CreateRemote(const MachineName: string): IRoseDecision;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDecision) as IRoseDecision;
end;

class function CoRoseActivity.Create: IRoseActivity;
begin
  Result := CreateComObject(CLASS_RoseActivity) as IRoseActivity;
end;

class function CoRoseActivity.CreateRemote(const MachineName: string): IRoseActivity;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseActivity) as IRoseActivity;
end;

class function CoRoseAbstractState.Create: IRoseAbstractState;
begin
  Result := CreateComObject(CLASS_RoseAbstractState) as IRoseAbstractState;
end;

class function CoRoseAbstractState.CreateRemote(const MachineName: string): IRoseAbstractState;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAbstractState) as IRoseAbstractState;
end;

class function CoRoseStateVertex.Create: IRoseStateVertex;
begin
  Result := CreateComObject(CLASS_RoseStateVertex) as IRoseStateVertex;
end;

class function CoRoseStateVertex.CreateRemote(const MachineName: string): IRoseStateVertex;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateVertex) as IRoseStateVertex;
end;

class function CoRoseSwimLane.Create: IRoseSwimLane;
begin
  Result := CreateComObject(CLASS_RoseSwimLane) as IRoseSwimLane;
end;

class function CoRoseSwimLane.CreateRemote(const MachineName: string): IRoseSwimLane;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSwimLane) as IRoseSwimLane;
end;

class function CoRoseStateMachine.Create: IRoseStateMachine;
begin
  Result := CreateComObject(CLASS_RoseStateMachine) as IRoseStateMachine;
end;

class function CoRoseStateMachine.CreateRemote(const MachineName: string): IRoseStateMachine;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateMachine) as IRoseStateMachine;
end;

class function CoRoseEvent.Create: IRoseEvent;
begin
  Result := CreateComObject(CLASS_RoseEvent) as IRoseEvent;
end;

class function CoRoseEvent.CreateRemote(const MachineName: string): IRoseEvent;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseEvent) as IRoseEvent;
end;

class function CoRoseAction.Create: IRoseAction;
begin
  Result := CreateComObject(CLASS_RoseAction) as IRoseAction;
end;

class function CoRoseAction.CreateRemote(const MachineName: string): IRoseAction;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAction) as IRoseAction;
end;

class function CoRoseTransition.Create: IRoseTransition;
begin
  Result := CreateComObject(CLASS_RoseTransition) as IRoseTransition;
end;

class function CoRoseTransition.CreateRemote(const MachineName: string): IRoseTransition;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseTransition) as IRoseTransition;
end;

class function CoRoseState.Create: IRoseState;
begin
  Result := CreateComObject(CLASS_RoseState) as IRoseState;
end;

class function CoRoseState.CreateRemote(const MachineName: string): IRoseState;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseState) as IRoseState;
end;

class function CoRoseRichTypeValuesCollection.Create: IRoseRichTypeValuesCollection;
begin
  Result := CreateComObject(CLASS_RoseRichTypeValuesCollection) as IRoseRichTypeValuesCollection;
end;

class function CoRoseRichTypeValuesCollection.CreateRemote(const MachineName: string): IRoseRichTypeValuesCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRichTypeValuesCollection) as IRoseRichTypeValuesCollection;
end;

class function CoRoseRichType.Create: IRoseRichType;
begin
  Result := CreateComObject(CLASS_RoseRichType) as IRoseRichType;
end;

class function CoRoseRichType.CreateRemote(const MachineName: string): IRoseRichType;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRichType) as IRoseRichType;
end;

class function CoRoseModuleVisibilityRelationship.Create: IRoseModuleVisibilityRelationship;
begin
  Result := CreateComObject(CLASS_RoseModuleVisibilityRelationship) as IRoseModuleVisibilityRelationship;
end;

class function CoRoseModuleVisibilityRelationship.CreateRemote(const MachineName: string): IRoseModuleVisibilityRelationship;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModuleVisibilityRelationship) as IRoseModuleVisibilityRelationship;
end;

class function CoRoseSubsystemViewCollection.Create: IRoseSubsystemViewCollection;
begin
  Result := CreateComObject(CLASS_RoseSubsystemViewCollection) as IRoseSubsystemViewCollection;
end;

class function CoRoseSubsystemViewCollection.CreateRemote(const MachineName: string): IRoseSubsystemViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSubsystemViewCollection) as IRoseSubsystemViewCollection;
end;

class function CoRoseComponentViewCollection.Create: IRoseComponentViewCollection;
begin
  Result := CreateComObject(CLASS_RoseComponentViewCollection) as IRoseComponentViewCollection;
end;

class function CoRoseComponentViewCollection.CreateRemote(const MachineName: string): IRoseComponentViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseComponentViewCollection) as IRoseComponentViewCollection;
end;

class function CoRoseSubsystemView.Create: IRoseSubsystemView;
begin
  Result := CreateComObject(CLASS_RoseSubsystemView) as IRoseSubsystemView;
end;

class function CoRoseSubsystemView.CreateRemote(const MachineName: string): IRoseSubsystemView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSubsystemView) as IRoseSubsystemView;
end;

class function CoRoseComponentView.Create: IRoseComponentView;
begin
  Result := CreateComObject(CLASS_RoseComponentView) as IRoseComponentView;
end;

class function CoRoseComponentView.CreateRemote(const MachineName: string): IRoseComponentView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseComponentView) as IRoseComponentView;
end;

class function CoRoseModuleDiagram.Create: IRoseModuleDiagram;
begin
  Result := CreateComObject(CLASS_RoseModuleDiagram) as IRoseModuleDiagram;
end;

class function CoRoseModuleDiagram.CreateRemote(const MachineName: string): IRoseModuleDiagram;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModuleDiagram) as IRoseModuleDiagram;
end;

class function CoRoseModule.Create: IRoseModule;
begin
  Result := CreateComObject(CLASS_RoseModule) as IRoseModule;
end;

class function CoRoseModule.CreateRemote(const MachineName: string): IRoseModule;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModule) as IRoseModule;
end;

class function CoRoseSubsystem.Create: IRoseSubsystem;
begin
  Result := CreateComObject(CLASS_RoseSubsystem) as IRoseSubsystem;
end;

class function CoRoseSubsystem.CreateRemote(const MachineName: string): IRoseSubsystem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSubsystem) as IRoseSubsystem;
end;

class function CoRoseUseCase.Create: IRoseUseCase;
begin
  Result := CreateComObject(CLASS_RoseUseCase) as IRoseUseCase;
end;

class function CoRoseUseCase.CreateRemote(const MachineName: string): IRoseUseCase;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseUseCase) as IRoseUseCase;
end;

class function CoRoseDiagramCollection.Create: IRoseDiagramCollection;
begin
  Result := CreateComObject(CLASS_RoseDiagramCollection) as IRoseDiagramCollection;
end;

class function CoRoseDiagramCollection.CreateRemote(const MachineName: string): IRoseDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDiagramCollection) as IRoseDiagramCollection;
end;

class function CoRoseLineVertexCollection.Create: IRoseLineVertexCollection;
begin
  Result := CreateComObject(CLASS_RoseLineVertexCollection) as IRoseLineVertexCollection;
end;

class function CoRoseLineVertexCollection.CreateRemote(const MachineName: string): IRoseLineVertexCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseLineVertexCollection) as IRoseLineVertexCollection;
end;

class function CoRoseInstantiateRelationCollection.Create: IRoseInstantiateRelationCollection;
begin
  Result := CreateComObject(CLASS_RoseInstantiateRelationCollection) as IRoseInstantiateRelationCollection;
end;

class function CoRoseInstantiateRelationCollection.CreateRemote(const MachineName: string): IRoseInstantiateRelationCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseInstantiateRelationCollection) as IRoseInstantiateRelationCollection;
end;

class function CoRoseRealizeRelationCollection.Create: IRoseRealizeRelationCollection;
begin
  Result := CreateComObject(CLASS_RoseRealizeRelationCollection) as IRoseRealizeRelationCollection;
end;

class function CoRoseRealizeRelationCollection.CreateRemote(const MachineName: string): IRoseRealizeRelationCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRealizeRelationCollection) as IRoseRealizeRelationCollection;
end;

class function CoRoseItemCollection.Create: IRoseItemCollection;
begin
  Result := CreateComObject(CLASS_RoseItemCollection) as IRoseItemCollection;
end;

class function CoRoseItemCollection.CreateRemote(const MachineName: string): IRoseItemCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseItemCollection) as IRoseItemCollection;
end;

class function CoRoseControllableUnitCollection.Create: IRoseControllableUnitCollection;
begin
  Result := CreateComObject(CLASS_RoseControllableUnitCollection) as IRoseControllableUnitCollection;
end;

class function CoRoseControllableUnitCollection.CreateRemote(const MachineName: string): IRoseControllableUnitCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseControllableUnitCollection) as IRoseControllableUnitCollection;
end;

class function CoRosePackageCollection.Create: IRosePackageCollection;
begin
  Result := CreateComObject(CLASS_RosePackageCollection) as IRosePackageCollection;
end;

class function CoRosePackageCollection.CreateRemote(const MachineName: string): IRosePackageCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RosePackageCollection) as IRosePackageCollection;
end;

class function CoRoseNoteViewCollection.Create: IRoseNoteViewCollection;
begin
  Result := CreateComObject(CLASS_RoseNoteViewCollection) as IRoseNoteViewCollection;
end;

class function CoRoseNoteViewCollection.CreateRemote(const MachineName: string): IRoseNoteViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseNoteViewCollection) as IRoseNoteViewCollection;
end;

class function CoRoseExternalDocumentCollection.Create: IRoseExternalDocumentCollection;
begin
  Result := CreateComObject(CLASS_RoseExternalDocumentCollection) as IRoseExternalDocumentCollection;
end;

class function CoRoseExternalDocumentCollection.CreateRemote(const MachineName: string): IRoseExternalDocumentCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseExternalDocumentCollection) as IRoseExternalDocumentCollection;
end;

class function CoRoseUseCaseCollection.Create: IRoseUseCaseCollection;
begin
  Result := CreateComObject(CLASS_RoseUseCaseCollection) as IRoseUseCaseCollection;
end;

class function CoRoseUseCaseCollection.CreateRemote(const MachineName: string): IRoseUseCaseCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseUseCaseCollection) as IRoseUseCaseCollection;
end;

class function CoRoseProcessCollection.Create: IRoseProcessCollection;
begin
  Result := CreateComObject(CLASS_RoseProcessCollection) as IRoseProcessCollection;
end;

class function CoRoseProcessCollection.CreateRemote(const MachineName: string): IRoseProcessCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseProcessCollection) as IRoseProcessCollection;
end;

class function CoRoseModuleVisibilityRelationshipCollection.Create: IRoseModuleVisibilityRelationshipCollection;
begin
  Result := CreateComObject(CLASS_RoseModuleVisibilityRelationshipCollection) as IRoseModuleVisibilityRelationshipCollection;
end;

class function CoRoseModuleVisibilityRelationshipCollection.CreateRemote(const MachineName: string): IRoseModuleVisibilityRelationshipCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModuleVisibilityRelationshipCollection) as IRoseModuleVisibilityRelationshipCollection;
end;

class function CoRoseItemViewCollection.Create: IRoseItemViewCollection;
begin
  Result := CreateComObject(CLASS_RoseItemViewCollection) as IRoseItemViewCollection;
end;

class function CoRoseItemViewCollection.CreateRemote(const MachineName: string): IRoseItemViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseItemViewCollection) as IRoseItemViewCollection;
end;

class function CoRoseScenarioDiagramCollection.Create: IRoseScenarioDiagramCollection;
begin
  Result := CreateComObject(CLASS_RoseScenarioDiagramCollection) as IRoseScenarioDiagramCollection;
end;

class function CoRoseScenarioDiagramCollection.CreateRemote(const MachineName: string): IRoseScenarioDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseScenarioDiagramCollection) as IRoseScenarioDiagramCollection;
end;

class function CoRosePropertyCollection.Create: IRosePropertyCollection;
begin
  Result := CreateComObject(CLASS_RosePropertyCollection) as IRosePropertyCollection;
end;

class function CoRosePropertyCollection.CreateRemote(const MachineName: string): IRosePropertyCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RosePropertyCollection) as IRosePropertyCollection;
end;

class function CoRoseProcessorCollection.Create: IRoseProcessorCollection;
begin
  Result := CreateComObject(CLASS_RoseProcessorCollection) as IRoseProcessorCollection;
end;

class function CoRoseProcessorCollection.CreateRemote(const MachineName: string): IRoseProcessorCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseProcessorCollection) as IRoseProcessorCollection;
end;

class function CoRoseObjectInstanceCollection.Create: IRoseObjectInstanceCollection;
begin
  Result := CreateComObject(CLASS_RoseObjectInstanceCollection) as IRoseObjectInstanceCollection;
end;

class function CoRoseObjectInstanceCollection.CreateRemote(const MachineName: string): IRoseObjectInstanceCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseObjectInstanceCollection) as IRoseObjectInstanceCollection;
end;

class function CoRoseMessageCollection.Create: IRoseMessageCollection;
begin
  Result := CreateComObject(CLASS_RoseMessageCollection) as IRoseMessageCollection;
end;

class function CoRoseMessageCollection.CreateRemote(const MachineName: string): IRoseMessageCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseMessageCollection) as IRoseMessageCollection;
end;

class function CoRoseInheritRelationCollection.Create: IRoseInheritRelationCollection;
begin
  Result := CreateComObject(CLASS_RoseInheritRelationCollection) as IRoseInheritRelationCollection;
end;

class function CoRoseInheritRelationCollection.CreateRemote(const MachineName: string): IRoseInheritRelationCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseInheritRelationCollection) as IRoseInheritRelationCollection;
end;

class function CoRoseRoleCollection.Create: IRoseRoleCollection;
begin
  Result := CreateComObject(CLASS_RoseRoleCollection) as IRoseRoleCollection;
end;

class function CoRoseRoleCollection.CreateRemote(const MachineName: string): IRoseRoleCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRoleCollection) as IRoseRoleCollection;
end;

class function CoRoseParameterCollection.Create: IRoseParameterCollection;
begin
  Result := CreateComObject(CLASS_RoseParameterCollection) as IRoseParameterCollection;
end;

class function CoRoseParameterCollection.CreateRemote(const MachineName: string): IRoseParameterCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseParameterCollection) as IRoseParameterCollection;
end;

class function CoRoseHasRelationshipCollection.Create: IRoseHasRelationshipCollection;
begin
  Result := CreateComObject(CLASS_RoseHasRelationshipCollection) as IRoseHasRelationshipCollection;
end;

class function CoRoseHasRelationshipCollection.CreateRemote(const MachineName: string): IRoseHasRelationshipCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseHasRelationshipCollection) as IRoseHasRelationshipCollection;
end;

class function CoRoseAssociationCollection.Create: IRoseAssociationCollection;
begin
  Result := CreateComObject(CLASS_RoseAssociationCollection) as IRoseAssociationCollection;
end;

class function CoRoseAssociationCollection.CreateRemote(const MachineName: string): IRoseAssociationCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAssociationCollection) as IRoseAssociationCollection;
end;

class function CoRoseOperationCollection.Create: IRoseOperationCollection;
begin
  Result := CreateComObject(CLASS_RoseOperationCollection) as IRoseOperationCollection;
end;

class function CoRoseOperationCollection.CreateRemote(const MachineName: string): IRoseOperationCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseOperationCollection) as IRoseOperationCollection;
end;

class function CoRoseAttributeCollection.Create: IRoseAttributeCollection;
begin
  Result := CreateComObject(CLASS_RoseAttributeCollection) as IRoseAttributeCollection;
end;

class function CoRoseAttributeCollection.CreateRemote(const MachineName: string): IRoseAttributeCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAttributeCollection) as IRoseAttributeCollection;
end;

class function CoRoseModuleCollection.Create: IRoseModuleCollection;
begin
  Result := CreateComObject(CLASS_RoseModuleCollection) as IRoseModuleCollection;
end;

class function CoRoseModuleCollection.CreateRemote(const MachineName: string): IRoseModuleCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModuleCollection) as IRoseModuleCollection;
end;

class function CoRoseSubsystemCollection.Create: IRoseSubsystemCollection;
begin
  Result := CreateComObject(CLASS_RoseSubsystemCollection) as IRoseSubsystemCollection;
end;

class function CoRoseSubsystemCollection.CreateRemote(const MachineName: string): IRoseSubsystemCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSubsystemCollection) as IRoseSubsystemCollection;
end;

class function CoRoseCategoryCollection.Create: IRoseCategoryCollection;
begin
  Result := CreateComObject(CLASS_RoseCategoryCollection) as IRoseCategoryCollection;
end;

class function CoRoseCategoryCollection.CreateRemote(const MachineName: string): IRoseCategoryCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseCategoryCollection) as IRoseCategoryCollection;
end;

class function CoRoseClassCollection.Create: IRoseClassCollection;
begin
  Result := CreateComObject(CLASS_RoseClassCollection) as IRoseClassCollection;
end;

class function CoRoseClassCollection.CreateRemote(const MachineName: string): IRoseClassCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassCollection) as IRoseClassCollection;
end;

class function CoRoseModuleDiagramCollection.Create: IRoseModuleDiagramCollection;
begin
  Result := CreateComObject(CLASS_RoseModuleDiagramCollection) as IRoseModuleDiagramCollection;
end;

class function CoRoseModuleDiagramCollection.CreateRemote(const MachineName: string): IRoseModuleDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModuleDiagramCollection) as IRoseModuleDiagramCollection;
end;

class function CoRoseClassDiagramCollection.Create: IRoseClassDiagramCollection;
begin
  Result := CreateComObject(CLASS_RoseClassDiagramCollection) as IRoseClassDiagramCollection;
end;

class function CoRoseClassDiagramCollection.CreateRemote(const MachineName: string): IRoseClassDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassDiagramCollection) as IRoseClassDiagramCollection;
end;

class function CoRoseDeviceCollection.Create: IRoseDeviceCollection;
begin
  Result := CreateComObject(CLASS_RoseDeviceCollection) as IRoseDeviceCollection;
end;

class function CoRoseDeviceCollection.CreateRemote(const MachineName: string): IRoseDeviceCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDeviceCollection) as IRoseDeviceCollection;
end;

class function CoRoseDeploymentDiagramCollection.Create: IRoseDeploymentDiagramCollection;
begin
  Result := CreateComObject(CLASS_RoseDeploymentDiagramCollection) as IRoseDeploymentDiagramCollection;
end;

class function CoRoseDeploymentDiagramCollection.CreateRemote(const MachineName: string): IRoseDeploymentDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDeploymentDiagramCollection) as IRoseDeploymentDiagramCollection;
end;

class function CoRoseClassViewCollection.Create: IRoseClassViewCollection;
begin
  Result := CreateComObject(CLASS_RoseClassViewCollection) as IRoseClassViewCollection;
end;

class function CoRoseClassViewCollection.CreateRemote(const MachineName: string): IRoseClassViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassViewCollection) as IRoseClassViewCollection;
end;

class function CoRoseDeploymentDiagram.Create: IRoseDeploymentDiagram;
begin
  Result := CreateComObject(CLASS_RoseDeploymentDiagram) as IRoseDeploymentDiagram;
end;

class function CoRoseDeploymentDiagram.CreateRemote(const MachineName: string): IRoseDeploymentDiagram;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDeploymentDiagram) as IRoseDeploymentDiagram;
end;

class function CoRoseProcess.Create: IRoseProcess;
begin
  Result := CreateComObject(CLASS_RoseProcess) as IRoseProcess;
end;

class function CoRoseProcess.CreateRemote(const MachineName: string): IRoseProcess;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseProcess) as IRoseProcess;
end;

class function CoRoseDevice.Create: IRoseDevice;
begin
  Result := CreateComObject(CLASS_RoseDevice) as IRoseDevice;
end;

class function CoRoseDevice.CreateRemote(const MachineName: string): IRoseDevice;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDevice) as IRoseDevice;
end;

class function CoRoseProcessor.Create: IRoseProcessor;
begin
  Result := CreateComObject(CLASS_RoseProcessor) as IRoseProcessor;
end;

class function CoRoseProcessor.CreateRemote(const MachineName: string): IRoseProcessor;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseProcessor) as IRoseProcessor;
end;

class function CoRoseInstanceViewCollection.Create: IRoseInstanceViewCollection;
begin
  Result := CreateComObject(CLASS_RoseInstanceViewCollection) as IRoseInstanceViewCollection;
end;

class function CoRoseInstanceViewCollection.CreateRemote(const MachineName: string): IRoseInstanceViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseInstanceViewCollection) as IRoseInstanceViewCollection;
end;

class function CoRoseInstanceView.Create: IRoseInstanceView;
begin
  Result := CreateComObject(CLASS_RoseInstanceView) as IRoseInstanceView;
end;

class function CoRoseInstanceView.CreateRemote(const MachineName: string): IRoseInstanceView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseInstanceView) as IRoseInstanceView;
end;

class function CoRoseLinkCollection.Create: IRoseLinkCollection;
begin
  Result := CreateComObject(CLASS_RoseLinkCollection) as IRoseLinkCollection;
end;

class function CoRoseLinkCollection.CreateRemote(const MachineName: string): IRoseLinkCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseLinkCollection) as IRoseLinkCollection;
end;

class function CoRoseLink.Create: IRoseLink;
begin
  Result := CreateComObject(CLASS_RoseLink) as IRoseLink;
end;

class function CoRoseLink.CreateRemote(const MachineName: string): IRoseLink;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseLink) as IRoseLink;
end;

class function CoRoseScenarioDiagram.Create: IRoseScenarioDiagram;
begin
  Result := CreateComObject(CLASS_RoseScenarioDiagram) as IRoseScenarioDiagram;
end;

class function CoRoseScenarioDiagram.CreateRemote(const MachineName: string): IRoseScenarioDiagram;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseScenarioDiagram) as IRoseScenarioDiagram;
end;

class function CoRoseMessage.Create: IRoseMessage;
begin
  Result := CreateComObject(CLASS_RoseMessage) as IRoseMessage;
end;

class function CoRoseMessage.CreateRemote(const MachineName: string): IRoseMessage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseMessage) as IRoseMessage;
end;

class function CoRoseObjectInstance.Create: IRoseObjectInstance;
begin
  Result := CreateComObject(CLASS_RoseObjectInstance) as IRoseObjectInstance;
end;

class function CoRoseObjectInstance.CreateRemote(const MachineName: string): IRoseObjectInstance;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseObjectInstance) as IRoseObjectInstance;
end;

class function CoRoseConnectionRelationCollection.Create: IRoseConnectionRelationCollection;
begin
  Result := CreateComObject(CLASS_RoseConnectionRelationCollection) as IRoseConnectionRelationCollection;
end;

class function CoRoseConnectionRelationCollection.CreateRemote(const MachineName: string): IRoseConnectionRelationCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseConnectionRelationCollection) as IRoseConnectionRelationCollection;
end;

class function CoRoseConnectionRelation.Create: IRoseConnectionRelation;
begin
  Result := CreateComObject(CLASS_RoseConnectionRelation) as IRoseConnectionRelation;
end;

class function CoRoseConnectionRelation.CreateRemote(const MachineName: string): IRoseConnectionRelation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseConnectionRelation) as IRoseConnectionRelation;
end;

class function CoRoseInstantiateRelation.Create: IRoseInstantiateRelation;
begin
  Result := CreateComObject(CLASS_RoseInstantiateRelation) as IRoseInstantiateRelation;
end;

class function CoRoseInstantiateRelation.CreateRemote(const MachineName: string): IRoseInstantiateRelation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseInstantiateRelation) as IRoseInstantiateRelation;
end;

class function CoRoseClassDependencyCollection.Create: IRoseClassDependencyCollection;
begin
  Result := CreateComObject(CLASS_RoseClassDependencyCollection) as IRoseClassDependencyCollection;
end;

class function CoRoseClassDependencyCollection.CreateRemote(const MachineName: string): IRoseClassDependencyCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassDependencyCollection) as IRoseClassDependencyCollection;
end;

class function CoRoseCategoryDependencyCollection.Create: IRoseCategoryDependencyCollection;
begin
  Result := CreateComObject(CLASS_RoseCategoryDependencyCollection) as IRoseCategoryDependencyCollection;
end;

class function CoRoseCategoryDependencyCollection.CreateRemote(const MachineName: string): IRoseCategoryDependencyCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseCategoryDependencyCollection) as IRoseCategoryDependencyCollection;
end;

class function CoRoseClassDependency.Create: IRoseClassDependency;
begin
  Result := CreateComObject(CLASS_RoseClassDependency) as IRoseClassDependency;
end;

class function CoRoseClassDependency.CreateRemote(const MachineName: string): IRoseClassDependency;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassDependency) as IRoseClassDependency;
end;

class function CoRoseCategoryDependency.Create: IRoseCategoryDependency;
begin
  Result := CreateComObject(CLASS_RoseCategoryDependency) as IRoseCategoryDependency;
end;

class function CoRoseCategoryDependency.CreateRemote(const MachineName: string): IRoseCategoryDependency;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseCategoryDependency) as IRoseCategoryDependency;
end;

class function CoRoseClassRelation.Create: IRoseClassRelation;
begin
  Result := CreateComObject(CLASS_RoseClassRelation) as IRoseClassRelation;
end;

class function CoRoseClassRelation.CreateRemote(const MachineName: string): IRoseClassRelation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassRelation) as IRoseClassRelation;
end;

class function CoRoseRealizeRelation.Create: IRoseRealizeRelation;
begin
  Result := CreateComObject(CLASS_RoseRealizeRelation) as IRoseRealizeRelation;
end;

class function CoRoseRealizeRelation.CreateRemote(const MachineName: string): IRoseRealizeRelation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRealizeRelation) as IRoseRealizeRelation;
end;

class function CoRoseInheritRelation.Create: IRoseInheritRelation;
begin
  Result := CreateComObject(CLASS_RoseInheritRelation) as IRoseInheritRelation;
end;

class function CoRoseInheritRelation.CreateRemote(const MachineName: string): IRoseInheritRelation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseInheritRelation) as IRoseInheritRelation;
end;

class function CoRoseHasRelationship.Create: IRoseHasRelationship;
begin
  Result := CreateComObject(CLASS_RoseHasRelationship) as IRoseHasRelationship;
end;

class function CoRoseHasRelationship.CreateRemote(const MachineName: string): IRoseHasRelationship;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseHasRelationship) as IRoseHasRelationship;
end;

class function CoRoseRole.Create: IRoseRole;
begin
  Result := CreateComObject(CLASS_RoseRole) as IRoseRole;
end;

class function CoRoseRole.CreateRemote(const MachineName: string): IRoseRole;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRole) as IRoseRole;
end;

class function CoRoseAssociation.Create: IRoseAssociation;
begin
  Result := CreateComObject(CLASS_RoseAssociation) as IRoseAssociation;
end;

class function CoRoseAssociation.CreateRemote(const MachineName: string): IRoseAssociation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAssociation) as IRoseAssociation;
end;

class function CoRoseParameter.Create: IRoseParameter;
begin
  Result := CreateComObject(CLASS_RoseParameter) as IRoseParameter;
end;

class function CoRoseParameter.CreateRemote(const MachineName: string): IRoseParameter;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseParameter) as IRoseParameter;
end;

class function CoRoseOperation.Create: IRoseOperation;
begin
  Result := CreateComObject(CLASS_RoseOperation) as IRoseOperation;
end;

class function CoRoseOperation.CreateRemote(const MachineName: string): IRoseOperation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseOperation) as IRoseOperation;
end;

class function CoRoseAttribute.Create: IRoseAttribute;
begin
  Result := CreateComObject(CLASS_RoseAttribute) as IRoseAttribute;
end;

class function CoRoseAttribute.CreateRemote(const MachineName: string): IRoseAttribute;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAttribute) as IRoseAttribute;
end;

class function CoRoseClassView.Create: IRoseClassView;
begin
  Result := CreateComObject(CLASS_RoseClassView) as IRoseClassView;
end;

class function CoRoseClassView.CreateRemote(const MachineName: string): IRoseClassView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassView) as IRoseClassView;
end;

class function CoRoseClassDiagram.Create: IRoseClassDiagram;
begin
  Result := CreateComObject(CLASS_RoseClassDiagram) as IRoseClassDiagram;
end;

class function CoRoseClassDiagram.CreateRemote(const MachineName: string): IRoseClassDiagram;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassDiagram) as IRoseClassDiagram;
end;

class function CoRoseClass.Create: IRoseClass;
begin
  Result := CreateComObject(CLASS_RoseClass) as IRoseClass;
end;

class function CoRoseClass.CreateRemote(const MachineName: string): IRoseClass;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClass) as IRoseClass;
end;

class function CoRoseCategory.Create: IRoseCategory;
begin
  Result := CreateComObject(CLASS_RoseCategory) as IRoseCategory;
end;

class function CoRoseCategory.CreateRemote(const MachineName: string): IRoseCategory;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseCategory) as IRoseCategory;
end;

class function CoRoseView_Font.Create: IRoseView_Font;
begin
  Result := CreateComObject(CLASS_RoseView_Font) as IRoseView_Font;
end;

class function CoRoseView_Font.CreateRemote(const MachineName: string): IRoseView_Font;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseView_Font) as IRoseView_Font;
end;

class function CoRoseView_LineColor.Create: IRoseView_LineColor;
begin
  Result := CreateComObject(CLASS_RoseView_LineColor) as IRoseView_LineColor;
end;

class function CoRoseView_LineColor.CreateRemote(const MachineName: string): IRoseView_LineColor;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseView_LineColor) as IRoseView_LineColor;
end;

class function CoRoseView_FillColor.Create: IRoseView_FillColor;
begin
  Result := CreateComObject(CLASS_RoseView_FillColor) as IRoseView_FillColor;
end;

class function CoRoseView_FillColor.CreateRemote(const MachineName: string): IRoseView_FillColor;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseView_FillColor) as IRoseView_FillColor;
end;

class function CoRoseNoteView.Create: IRoseNoteView;
begin
  Result := CreateComObject(CLASS_RoseNoteView) as IRoseNoteView;
end;

class function CoRoseNoteView.CreateRemote(const MachineName: string): IRoseNoteView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseNoteView) as IRoseNoteView;
end;

class function CoRoseItemView.Create: IRoseItemView;
begin
  Result := CreateComObject(CLASS_RoseItemView) as IRoseItemView;
end;

class function CoRoseItemView.CreateRemote(const MachineName: string): IRoseItemView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseItemView) as IRoseItemView;
end;

class function CoRoseDiagram.Create: IRoseDiagram;
begin
  Result := CreateComObject(CLASS_RoseDiagram) as IRoseDiagram;
end;

class function CoRoseDiagram.CreateRemote(const MachineName: string): IRoseDiagram;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDiagram) as IRoseDiagram;
end;

class function CoRoseStringCollection.Create: IRoseStringCollection;
begin
  Result := CreateComObject(CLASS_RoseStringCollection) as IRoseStringCollection;
end;

class function CoRoseStringCollection.CreateRemote(const MachineName: string): IRoseStringCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStringCollection) as IRoseStringCollection;
end;

class function CoRoseObjectFlowCollection.Create: IRoseObjectFlowCollection;
begin
  Result := CreateComObject(CLASS_RoseObjectFlowCollection) as IRoseObjectFlowCollection;
end;

class function CoRoseObjectFlowCollection.CreateRemote(const MachineName: string): IRoseObjectFlowCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseObjectFlowCollection) as IRoseObjectFlowCollection;
end;

class function CoRoseDependencyRelationCollection.Create: IRoseDependencyRelationCollection;
begin
  Result := CreateComObject(CLASS_RoseDependencyRelationCollection) as IRoseDependencyRelationCollection;
end;

class function CoRoseDependencyRelationCollection.CreateRemote(const MachineName: string): IRoseDependencyRelationCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDependencyRelationCollection) as IRoseDependencyRelationCollection;
end;

class function CoRoseObjectFlow.Create: IRoseObjectFlow;
begin
  Result := CreateComObject(CLASS_RoseObjectFlow) as IRoseObjectFlow;
end;

class function CoRoseObjectFlow.CreateRemote(const MachineName: string): IRoseObjectFlow;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseObjectFlow) as IRoseObjectFlow;
end;

class function CoRoseDependencyRelation.Create: IRoseDependencyRelation;
begin
  Result := CreateComObject(CLASS_RoseDependencyRelation) as IRoseDependencyRelation;
end;

class function CoRoseDependencyRelation.CreateRemote(const MachineName: string): IRoseDependencyRelation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDependencyRelation) as IRoseDependencyRelation;
end;

class function CoRoseDeploymentUnit.Create: IRoseDeploymentUnit;
begin
  Result := CreateComObject(CLASS_RoseDeploymentUnit) as IRoseDeploymentUnit;
end;

class function CoRoseDeploymentUnit.CreateRemote(const MachineName: string): IRoseDeploymentUnit;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDeploymentUnit) as IRoseDeploymentUnit;
end;

class function CoRoseRelation.Create: IRoseRelation;
begin
  Result := CreateComObject(CLASS_RoseRelation) as IRoseRelation;
end;

class function CoRoseRelation.CreateRemote(const MachineName: string): IRoseRelation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRelation) as IRoseRelation;
end;

class function CoRoseDefaultModelProperties.Create: IRoseDefaultModelProperties;
begin
  Result := CreateComObject(CLASS_RoseDefaultModelProperties) as IRoseDefaultModelProperties;
end;

class function CoRoseDefaultModelProperties.CreateRemote(const MachineName: string): IRoseDefaultModelProperties;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDefaultModelProperties) as IRoseDefaultModelProperties;
end;

class function CoRoseProperty.Create: IRoseProperty;
begin
  Result := CreateComObject(CLASS_RoseProperty) as IRoseProperty;
end;

class function CoRoseProperty.CreateRemote(const MachineName: string): IRoseProperty;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseProperty) as IRoseProperty;
end;

class function CoRoseElement.Create: IRoseElement;
begin
  Result := CreateComObject(CLASS_RoseElement) as IRoseElement;
end;

class function CoRoseElement.CreateRemote(const MachineName: string): IRoseElement;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseElement) as IRoseElement;
end;

class function CoRosePackage.Create: IRosePackage;
begin
  Result := CreateComObject(CLASS_RosePackage) as IRosePackage;
end;

class function CoRosePackage.CreateRemote(const MachineName: string): IRosePackage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RosePackage) as IRosePackage;
end;

class function CoRoseControllableUnit.Create: IRoseControllableUnit;
begin
  Result := CreateComObject(CLASS_RoseControllableUnit) as IRoseControllableUnit;
end;

class function CoRoseControllableUnit.CreateRemote(const MachineName: string): IRoseControllableUnit;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseControllableUnit) as IRoseControllableUnit;
end;

class function CoRoseExternalDocument.Create: IRoseExternalDocument;
begin
  Result := CreateComObject(CLASS_RoseExternalDocument) as IRoseExternalDocument;
end;

class function CoRoseExternalDocument.CreateRemote(const MachineName: string): IRoseExternalDocument;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseExternalDocument) as IRoseExternalDocument;
end;

class function CoRoseItem.Create: IRoseItem;
begin
  Result := CreateComObject(CLASS_RoseItem) as IRoseItem;
end;

class function CoRoseItem.CreateRemote(const MachineName: string): IRoseItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseItem) as IRoseItem;
end;

class function CoRoseModel.Create: IRoseModel;
begin
  Result := CreateComObject(CLASS_RoseModel) as IRoseModel;
end;

class function CoRoseModel.CreateRemote(const MachineName: string): IRoseModel;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModel) as IRoseModel;
end;

class function CoRoseContextMenuItemCollection.Create: IRoseContextMenuItemCollection;
begin
  Result := CreateComObject(CLASS_RoseContextMenuItemCollection) as IRoseContextMenuItemCollection;
end;

class function CoRoseContextMenuItemCollection.CreateRemote(const MachineName: string): IRoseContextMenuItemCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseContextMenuItemCollection) as IRoseContextMenuItemCollection;
end;

class function CoRoseContextMenuItem.Create: IRoseContextMenuItem;
begin
  Result := CreateComObject(CLASS_RoseContextMenuItem) as IRoseContextMenuItem;
end;

class function CoRoseContextMenuItem.CreateRemote(const MachineName: string): IRoseContextMenuItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseContextMenuItem) as IRoseContextMenuItem;
end;

class function CoRoseAddInCollection.Create: IRoseAddInCollection;
begin
  Result := CreateComObject(CLASS_RoseAddInCollection) as IRoseAddInCollection;
end;

class function CoRoseAddInCollection.CreateRemote(const MachineName: string): IRoseAddInCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAddInCollection) as IRoseAddInCollection;
end;

class function CoRoseAddIn.Create: IRoseAddIn;
begin
  Result := CreateComObject(CLASS_RoseAddIn) as IRoseAddIn;
end;

class function CoRoseAddIn.CreateRemote(const MachineName: string): IRoseAddIn;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAddIn) as IRoseAddIn;
end;

class function CoRoseAddInManager.Create: IRoseAddInManager;
begin
  Result := CreateComObject(CLASS_RoseAddInManager) as IRoseAddInManager;
end;

class function CoRoseAddInManager.CreateRemote(const MachineName: string): IRoseAddInManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAddInManager) as IRoseAddInManager;
end;

class function CoRosePathMap.Create: IRosePathMap;
begin
  Result := CreateComObject(CLASS_RosePathMap) as IRosePathMap;
end;

class function CoRosePathMap.CreateRemote(const MachineName: string): IRosePathMap;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RosePathMap) as IRosePathMap;
end;

class function CoRoseApplication.Create: IRoseApplication;
begin
  Result := CreateComObject(CLASS_RoseApplication) as IRoseApplication;
end;

class function CoRoseApplication.CreateRemote(const MachineName: string): IRoseApplication;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseApplication) as IRoseApplication;
end;

end.
