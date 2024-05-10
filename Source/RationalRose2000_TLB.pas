
{ Global compiler directives }
{$include bold.inc}
unit RationalRose2000_TLB;
{$INCLUDE Bold.inc}

{$WARNINGS OFF}
{$WARN SYMBOL_PLATFORM OFF}



















{$TYPEDADDRESS OFF}
interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, StdVCL;






const
  RationalRoseMajorVersion = 4;
  RationalRoseMinorVersion = 2;

  LIBID_RationalRose: TGUID = '{860CC660-1C2B-11D0-B1B1-444553540000}';

  DIID_IRoseActivityViewCollection: TGUID = '{BEAED5FE-578D-11D2-92AA-004005141253}';
  CLASS_RoseActivityViewCollection: TGUID = '{BEAED5FF-578D-11D2-92AA-004005141253}';
  DIID_IRoseProcessorCollection: TGUID = '{97B3835C-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseProcessorCollection: TGUID = '{BA376ECF-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseCategoryCollection: TGUID = '{97B3835B-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseCategoryCollection: TGUID = '{BA376EE3-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseDeploymentUnit: TGUID = '{4335FBE3-F0A0-11D1-9FAD-0060975306FE}';
  CLASS_RoseDeploymentUnit: TGUID = '{4335FBE2-F0A0-11D1-9FAD-0060975306FE}';
  DIID_IRoseItem: TGUID = '{BC57D1C2-863E-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseItem: TGUID = '{97B3836E-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseContextMenuItemCollection: TGUID = '{EE0B16E2-FF91-11D1-9FAD-0060975306FE}';
  CLASS_RoseContextMenuItemCollection: TGUID = '{EE0B16E4-FF91-11D1-9FAD-0060975306FE}';
  DIID_IRoseAddIn: TGUID = '{D5352FC0-346C-11D1-883B-3C8B00C10000}';
  CLASS_RoseAddIn: TGUID = '{D5352FC1-346C-11D1-883B-3C8B00C10000}';
  DIID_IRoseDecisionViewCollection: TGUID = '{BEAED601-578D-11D2-92AA-004005141253}';
  CLASS_RoseDecisionViewCollection: TGUID = '{BEAED602-578D-11D2-92AA-004005141253}';
  DIID_IRoseStateVertex: TGUID = '{BEAED5E2-578D-11D2-92AA-004005141253}';
  CLASS_RoseStateVertex: TGUID = '{BB792021-57AA-11D2-92AA-004005141253}';
  DIID_IRoseUseCaseCollection: TGUID = '{97B38356-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseUseCaseCollection: TGUID = '{BA376EDE-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseConnectionRelation: TGUID = '{4467F442-F24E-11D2-92AA-004005141253}';
  CLASS_RoseConnectionRelation: TGUID = '{4467F444-F24E-11D2-92AA-004005141253}';
  DIID_IRoseRelation: TGUID = '{BA242E02-8961-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseRelation: TGUID = '{97B38370-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseApplication: TGUID = '{D7BC1B40-8618-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseApplication: TGUID = '{D7BC1B41-8618-11CF-B3D4-00A0241DB1D0}';
  DIID_IRoseDecisionCollection: TGUID = '{BEAED5F2-578D-11D2-92AA-004005141253}';
  CLASS_RoseDecisionCollection: TGUID = '{BEAED5F4-578D-11D2-92AA-004005141253}';
  DIID_IRoseDecision: TGUID = '{BEAED5E3-578D-11D2-92AA-004005141253}';
  CLASS_RoseDecision: TGUID = '{BEAED5E5-578D-11D2-92AA-004005141253}';
  DIID_IRoseLineVertexCollection: TGUID = '{11A235B2-3095-11D2-8153-00104B97EBD5}';
  CLASS_RoseLineVertexCollection: TGUID = '{11A235B4-3095-11D2-8153-00104B97EBD5}';
  DIID_IRoseInstantiateRelationCollection: TGUID = '{B91D8F06-DDBB-11D1-9FAD-0060975306FE}';
  CLASS_RoseInstantiateRelationCollection: TGUID = '{B91D8F05-DDBB-11D1-9FAD-0060975306FE}';
  DIID_IRoseMessageCollection: TGUID = '{97B38359-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseMessageCollection: TGUID = '{BA376ECD-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseClassDiagramCollection: TGUID = '{97B38343-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseClassDiagramCollection: TGUID = '{BA376ECB-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseScenarioDiagram: TGUID = '{F819833A-FC55-11CF-BBD3-00A024C67143}';
  CLASS_RoseScenarioDiagram: TGUID = '{4782FBAF-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseRealizeRelation: TGUID = '{6AC2BA81-454D-11D1-883B-3C8B00C10000}';
  CLASS_RoseRealizeRelation: TGUID = '{6AC2BA82-454D-11D1-883B-3C8B00C10000}';
  DIID_IRoseHasRelationship: TGUID = '{BA242E04-8961-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseHasRelationship: TGUID = '{4782FBA4-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseClassView: TGUID = '{5F735F36-F9EA-11CF-BBD3-00A024C67143}';
  CLASS_RoseClassView: TGUID = '{86652271-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseView_FillColor: TGUID = '{CE5BE563-0380-11D1-BC11-00A024C67143}';
  CLASS_RoseView_FillColor: TGUID = '{CE5BE564-0380-11D1-BC11-00A024C67143}';
  DIID_IRoseActionCollection: TGUID = '{97B3835F-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseActionCollection: TGUID = '{BA376EEF-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseSyncItemCollection: TGUID = '{94CA188F-5D13-11D2-92AA-004005141253}';
  CLASS_RoseSyncItemCollection: TGUID = '{94CA1890-5D13-11D2-92AA-004005141253}';
  DIID_IRoseActivityCollection: TGUID = '{BEAED5F0-578D-11D2-92AA-004005141253}';
  CLASS_RoseActivityCollection: TGUID = '{BEAED5F1-578D-11D2-92AA-004005141253}';
  DIID_IRoseActivity: TGUID = '{BEAED5E7-578D-11D2-92AA-004005141253}';
  CLASS_RoseActivity: TGUID = '{BEAED5E8-578D-11D2-92AA-004005141253}';
  DIID_IRoseProcess: TGUID = '{62C43884-DB5A-11CF-B091-00A0241E3F73}';
  CLASS_RoseProcess: TGUID = '{4782FBAE-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseAddInCollection: TGUID = '{C87D2BC1-352A-11D1-883B-3C8B00C10000}';
  CLASS_RoseAddInCollection: TGUID = '{C87D2BC0-352A-11D1-883B-3C8B00C10000}';
  DIID_IRoseSwimLaneView: TGUID = '{68F63C21-B047-11D2-92AA-004005141253}';
  CLASS_RoseSwimLaneView: TGUID = '{68F63C23-B047-11D2-92AA-004005141253}';
  DIID_IRoseControllableUnitCollection: TGUID = '{97B38360-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseControllableUnitCollection: TGUID = '{BA376ED2-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseModuleCollection: TGUID = '{97B3834B-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseModuleCollection: TGUID = '{BA376ED5-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseLinkCollection: TGUID = '{9DE9A9C1-F2D0-11D0-883A-3C8B00C10000}';
  CLASS_RoseLinkCollection: TGUID = '{9DE9A9C2-F2D0-11D0-883A-3C8B00C10000}';
  DIID_IRoseAction: TGUID = '{13881143-93C1-11D0-A214-00A024FFFE40}';
  CLASS_RoseAction: TGUID = '{86652270-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseParameterCollection: TGUID = '{97B38352-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseParameterCollection: TGUID = '{BA376EDA-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseAttributeCollection: TGUID = '{97B3834C-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseAttributeCollection: TGUID = '{BA376ED6-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseDevice: TGUID = '{62C43882-DB5A-11CF-B091-00A0241E3F73}';
  CLASS_RoseDevice: TGUID = '{86652275-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseClassDependency: TGUID = '{4ACE1899-6CD3-11D1-BC1E-00A024C67143}';
  CLASS_RoseClassDependency: TGUID = '{4ACE189A-6CD3-11D1-BC1E-00A024C67143}';
  DIID_IRoseRole: TGUID = '{BA242E00-8961-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseRole: TGUID = '{97B38378-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseClass: TGUID = '{BC57D1C0-863E-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseClass: TGUID = '{8665226F-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseElement: TGUID = '{D067F15F-6987-11D0-BBF0-00A024C67143}';
  CLASS_RoseElement: TGUID = '{97B38396-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseControllableUnit: TGUID = '{32C862A7-8AA9-11D0-A70B-0000F803584A}';
  CLASS_RoseControllableUnit: TGUID = '{97B38379-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseModel: TGUID = '{E38942A0-8621-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseModel: TGUID = '{4782FBA6-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseTransition: TGUID = '{574130A1-93B8-11D0-A214-00A024FFFE40}';
  CLASS_RoseTransition: TGUID = '{4782FBB5-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseSubsystemCollection: TGUID = '{97B3834A-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseSubsystemCollection: TGUID = '{BA376ED4-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseProcessor: TGUID = '{62C43886-DB5A-11CF-B091-00A0241E3F73}';
  CLASS_RoseProcessor: TGUID = '{4782FBAD-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseCategoryDependencyCollection: TGUID = '{4ACE189D-6CD3-11D1-BC1E-00A024C67143}';
  CLASS_RoseCategoryDependencyCollection: TGUID = '{4ACE189E-6CD3-11D1-BC1E-00A024C67143}';
  DIID_IRoseProperty: TGUID = '{93461A23-8811-11CF-B1B0-D227D5210B2C}';
  CLASS_RoseProperty: TGUID = '{4782FBAB-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseStateDiagram: TGUID = '{7ADDA701-9B06-11D0-A214-00A024FFFE40}';
  CLASS_RoseStateDiagram: TGUID = '{4782FBB1-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseEvent: TGUID = '{A69CAB22-9179-11D0-A214-00A024FFFE40}';
  CLASS_RoseEvent: TGUID = '{86652276-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseRichType: TGUID = '{EB7AAB60-939C-11CF-B091-00A0241E3F73}';
  CLASS_RoseRichType: TGUID = '{97B38380-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseScenarioDiagramCollection: TGUID = '{97B3835E-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseScenarioDiagramCollection: TGUID = '{BA376ED1-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseParameter: TGUID = '{C78E7028-86E4-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseParameter: TGUID = '{4782FBAA-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseOperation: TGUID = '{C78E7020-86E4-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseOperation: TGUID = '{4782FBA8-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseView_LineColor: TGUID = '{CE5BE565-0380-11D1-BC11-00A024C67143}';
  CLASS_RoseView_LineColor: TGUID = '{CE5BE566-0380-11D1-BC11-00A024C67143}';
  DIID_IRoseAddInManager: TGUID = '{D5352FC2-346C-11D1-883B-3C8B00C10000}';
  CLASS_RoseAddInManager: TGUID = '{D5352FC3-346C-11D1-883B-3C8B00C10000}';
  DIID_IRoseStateDiagramCollection: TGUID = '{97B38368-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseStateDiagramCollection: TGUID = '{BA376EEA-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseSwimLaneViewCollection: TGUID = '{7FFC5F46-C0C2-11D2-92AA-004005141253}';
  CLASS_RoseSwimLaneViewCollection: TGUID = '{7FFC5F48-C0C2-11D2-92AA-004005141253}';
  DIID_IRoseSwimLane: TGUID = '{BEAED5EA-578D-11D2-92AA-004005141253}';
  CLASS_RoseSwimLane: TGUID = '{BB792025-57AA-11D2-92AA-004005141253}';
  DIID_IRoseItemViewCollection: TGUID = '{97B38362-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseItemViewCollection: TGUID = '{BA376EE1-A44E-11D0-BC02-00A024C67143}';
  DIID_IRosePropertyCollection: TGUID = '{97B3835D-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RosePropertyCollection: TGUID = '{BA376ED0-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseOperationCollection: TGUID = '{97B3834D-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseOperationCollection: TGUID = '{BA376ED7-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseDeviceCollection: TGUID = '{97B38342-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseDeviceCollection: TGUID = '{BA376ECA-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseInstantiateRelation: TGUID = '{B91D8F03-DDBB-11D1-9FAD-0060975306FE}';
  CLASS_RoseInstantiateRelation: TGUID = '{B91D8F04-DDBB-11D1-9FAD-0060975306FE}';
  DIID_IRoseContextMenuItem: TGUID = '{EE0B16E0-FF91-11D1-9FAD-0060975306FE}';
  CLASS_RoseContextMenuItem: TGUID = '{EE0B16E1-FF91-11D1-9FAD-0060975306FE}';
  DIID_IRoseLineVertex: TGUID = '{B53888D2-3094-11D2-8153-00104B97EBD5}';
  CLASS_RoseLineVertex: TGUID = '{B53888D1-3094-11D2-8153-00104B97EBD5}';
  DIID_IRoseObject: TGUID = '{7D8474B2-2C33-11D0-BBDA-00A024C67143}';
  CLASS_RoseObject: TGUID = '{97B38389-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseSwimLaneCollection: TGUID = '{7FFC5F42-C0C2-11D2-92AA-004005141253}';
  CLASS_RoseSwimLaneCollection: TGUID = '{7FFC5F44-C0C2-11D2-92AA-004005141253}';
  DIID_IRoseModuleVisibilityRelationship: TGUID = '{9EF8DDD6-E697-11CF-BBD1-00A024C67143}';
  CLASS_RoseModuleVisibilityRelationship: TGUID = '{8665227D-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseComponentViewCollection: TGUID = '{C640C861-F2D3-11D0-883A-3C8B00C10000}';
  CLASS_RoseComponentViewCollection: TGUID = '{9DE9A9C3-F2D0-11D0-883A-3C8B00C10000}';
  DIID_IRoseHasRelationshipCollection: TGUID = '{97B38351-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseHasRelationshipCollection: TGUID = '{BA376ED9-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseClassViewCollection: TGUID = '{97B38341-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseClassViewCollection: TGUID = '{BA376EC7-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseDeploymentDiagram: TGUID = '{C2C15EC4-E028-11CF-B091-00A0241E3F73}';
  CLASS_RoseDeploymentDiagram: TGUID = '{86652274-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseInstanceView: TGUID = '{348B1AD4-D5C4-11D0-89F8-0020AFD6C181}';
  CLASS_RoseInstanceView: TGUID = '{86652279-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseLink: TGUID = '{195D7852-D5B6-11D0-89F8-0020AFD6C181}';
  CLASS_RoseLink: TGUID = '{8665227A-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseObjectInstance: TGUID = '{F8198337-FC55-11CF-BBD3-00A024C67143}';
  CLASS_RoseObjectInstance: TGUID = '{4782FBA1-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseCategoryDependency: TGUID = '{4ACE189B-6CD3-11D1-BC1E-00A024C67143}';
  CLASS_RoseCategoryDependency: TGUID = '{4ACE189C-6CD3-11D1-BC1E-00A024C67143}';
  DIID_IRoseInheritRelation: TGUID = '{00C99560-9200-11CF-B1B0-D227D5210B2C}';
  CLASS_RoseInheritRelation: TGUID = '{86652278-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseView_Font: TGUID = '{CE5BE567-0380-11D1-BC11-00A024C67143}';
  CLASS_RoseView_Font: TGUID = '{CE5BE568-0380-11D1-BC11-00A024C67143}';
  DIID_IRoseStateMachine: TGUID = '{A69CAB21-9179-11D0-A214-00A024FFFE40}';
  CLASS_RoseStateMachine: TGUID = '{4782FBB2-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseModule: TGUID = '{C78E702A-86E4-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseModule: TGUID = '{4782FBA7-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseUseCase: TGUID = '{7E7F6EE0-16DE-11D0-8976-00A024774419}';
  CLASS_RoseUseCase: TGUID = '{4782FBB6-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseItemCollection: TGUID = '{0DD9ACF8-D06E-11D0-BC0B-00A024C67143}';
  CLASS_RoseItemCollection: TGUID = '{0DD9ACF7-D06E-11D0-BC0B-00A024C67143}';
  DIID_IRoseNoteViewCollection: TGUID = '{97B38358-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseNoteViewCollection: TGUID = '{BA376EE0-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseInheritRelationCollection: TGUID = '{97B38354-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseInheritRelationCollection: TGUID = '{BA376EDC-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseDeploymentDiagramCollection: TGUID = '{97B383A1-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseDeploymentDiagramCollection: TGUID = '{BA376EC8-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseStringCollection: TGUID = '{6A7FC311-C893-11D0-BC0B-00A024C67143}';
  CLASS_RoseStringCollection: TGUID = '{4782FBB8-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseStateViewCollection: TGUID = '{97B3836A-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseStateViewCollection: TGUID = '{BA376EEC-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseDecisionView: TGUID = '{BEAED5F9-578D-11D2-92AA-004005141253}';
  CLASS_RoseDecisionView: TGUID = '{BEAED5FA-578D-11D2-92AA-004005141253}';
  DIID_IRoseStateMachineOwner: TGUID = '{94CA1882-5D13-11D2-92AA-004005141253}';
  CLASS_RoseStateMachineOwner: TGUID = '{94CA1883-5D13-11D2-92AA-004005141253}';
  DIID_IRoseProcessCollection: TGUID = '{97B38366-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseProcessCollection: TGUID = '{BA376EE6-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseAssociationCollection: TGUID = '{97B3834E-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseAssociationCollection: TGUID = '{BA376ED8-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseModuleDiagramCollection: TGUID = '{97B38348-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseModuleDiagramCollection: TGUID = '{BA376ECC-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseDiagram: TGUID = '{3FD9D000-93B0-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseDiagram: TGUID = '{97B3838E-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseAbstractStateCollection: TGUID = '{BEAED5EE-578D-11D2-92AA-004005141253}';
  CLASS_RoseAbstractStateCollection: TGUID = '{BB792023-57AA-11D2-92AA-004005141253}';
  DIID_IRoseRichTypeValuesCollection: TGUID = '{BF8C1040-96DD-11CF-B091-00A0241E3F73}';
  CLASS_RoseRichTypeValuesCollection: TGUID = '{97B38390-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseSubsystemView: TGUID = '{14028C92-C06C-11D0-89F5-0020AFD6C181}';
  CLASS_RoseSubsystemView: TGUID = '{4782FBB4-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseComponentView: TGUID = '{14028C94-C06C-11D0-89F5-0020AFD6C181}';
  CLASS_RoseComponentView: TGUID = '{86652273-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseAttribute: TGUID = '{C78E7024-86E4-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseAttribute: TGUID = '{4782FBA3-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseClassDiagram: TGUID = '{3FD9D002-93B0-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseClassDiagram: TGUID = '{86652272-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseNoteView: TGUID = '{015655CA-72DF-11D0-95EB-0000F803584A}';
  CLASS_RoseNoteView: TGUID = '{4782FBA0-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRosePackage: TGUID = '{47D975C1-8A8D-11D0-A214-444553540000}';
  CLASS_RosePackage: TGUID = '{97B38392-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRosePathMap: TGUID = '{4C9E2241-84C5-11D0-A214-444553540000}';
  CLASS_RosePathMap: TGUID = '{97B38395-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseSyncItemViewCollection: TGUID = '{94CA1891-5D13-11D2-92AA-004005141253}';
  CLASS_RoseSyncItemViewCollection: TGUID = '{94CA1892-5D13-11D2-92AA-004005141253}';
  DIID_IRoseAbstractState: TGUID = '{BEAED5EC-578D-11D2-92AA-004005141253}';
  CLASS_RoseAbstractState: TGUID = '{BB792027-57AA-11D2-92AA-004005141253}';
  DIID_IRoseModuleVisibilityRelationshipCollection: TGUID = '{97B38363-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseModuleVisibilityRelationshipCollection: TGUID = '{BA376EE2-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseClassCollection: TGUID = '{97B38349-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseClassCollection: TGUID = '{BA376ED3-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseMessage: TGUID = '{F819833C-FC55-11CF-BBD3-00A024C67143}';
  CLASS_RoseMessage: TGUID = '{8665227C-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseConnectionRelationCollection: TGUID = '{4467F446-F24E-11D2-92AA-004005141253}';
  CLASS_RoseConnectionRelationCollection: TGUID = '{4467F448-F24E-11D2-92AA-004005141253}';
  DIID_IRoseClassDependencyCollection: TGUID = '{ED042E4F-6CDE-11D1-BC1E-00A024C67143}';
  CLASS_RoseClassDependencyCollection: TGUID = '{ED042E50-6CDE-11D1-BC1E-00A024C67143}';
  DIID_IRoseAssociation: TGUID = '{C78E7026-86E4-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseAssociation: TGUID = '{4782FBA2-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseEventCollection: TGUID = '{97B38361-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseEventCollection: TGUID = '{BA376EE8-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseStateCollection: TGUID = '{97B38367-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseStateCollection: TGUID = '{BA376EE9-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseSyncItemView: TGUID = '{94CA1888-5D13-11D2-92AA-004005141253}';
  CLASS_RoseSyncItemView: TGUID = '{94CA1889-5D13-11D2-92AA-004005141253}';
  DIID_IRoseActivityView: TGUID = '{BEAED5FC-578D-11D2-92AA-004005141253}';
  CLASS_RoseActivityView: TGUID = '{BEAED5FD-578D-11D2-92AA-004005141253}';
  DIID_IRoseStateView: TGUID = '{7BD909E1-9AF9-11D0-A214-00A024FFFE40}';
  CLASS_RoseStateView: TGUID = '{4782FBB3-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseStateVertexCollection: TGUID = '{BEAED5F7-578D-11D2-92AA-004005141253}';
  CLASS_RoseStateVertexCollection: TGUID = '{BB792029-57AA-11D2-92AA-004005141253}';
  DIID_IRoseSubsystemViewCollection: TGUID = '{CA3AD902-BFCE-11D0-89F5-0020AFD6C181}';
  CLASS_RoseSubsystemViewCollection: TGUID = '{0CEEA5A1-C6F8-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseModuleDiagram: TGUID = '{3FD9D004-93B0-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseModuleDiagram: TGUID = '{8665227B-EBF7-11D0-BC10-00A024C67143}';
  DIID_IRoseSubsystem: TGUID = '{C78E702C-86E4-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseSubsystem: TGUID = '{4782FBAC-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseExternalDocumentCollection: TGUID = '{97B38357-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseExternalDocumentCollection: TGUID = '{BA376EDF-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseInstanceViewCollection: TGUID = '{C640C864-F2D3-11D0-883A-3C8B00C10000}';
  CLASS_RoseInstanceViewCollection: TGUID = '{C640C862-F2D3-11D0-883A-3C8B00C10000}';
  DIID_IRoseItemView: TGUID = '{7DFAFE40-A29D-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseItemView: TGUID = '{97B3839B-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseTransitionCollection: TGUID = '{97B3836B-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseTransitionCollection: TGUID = '{BA376EEE-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseState: TGUID = '{A69CAB23-9179-11D0-A214-00A024FFFE40}';
  CLASS_RoseState: TGUID = '{4782FBB0-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseObjectInstanceCollection: TGUID = '{97B3835A-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseObjectInstanceCollection: TGUID = '{BA376ECE-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseRoleCollection: TGUID = '{97B38353-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseRoleCollection: TGUID = '{BA376EDB-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseClassRelation: TGUID = '{00C99564-9200-11CF-B1B0-D227D5210B2C}';
  CLASS_RoseClassRelation: TGUID = '{97B3839E-A4E3-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseCategory: TGUID = '{D7BC1B45-8618-11CF-B3D4-00A0241DB1D0}';
  CLASS_RoseCategory: TGUID = '{4782FBA9-ECD5-11D0-BFF0-00AA003DEF5B}';
  DIID_IRoseDefaultModelProperties: TGUID = '{76ACC49D-FA18-11D0-BC11-00A024C67143}';
  CLASS_RoseDefaultModelProperties: TGUID = '{A51B4041-3E99-11D1-883B-3C8B00C10000}';
  DIID_IRoseStateMachineCollection: TGUID = '{97B38369-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RoseStateMachineCollection: TGUID = '{BA376EEB-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseSyncItem: TGUID = '{94CA188B-5D13-11D2-92AA-004005141253}';
  CLASS_RoseSyncItem: TGUID = '{94CA188D-5D13-11D2-92AA-004005141253}';
  DIID_IRoseRealizeRelationCollection: TGUID = '{67448181-4553-11D1-883B-3C8B00C10000}';
  CLASS_RoseRealizeRelationCollection: TGUID = '{67448182-4553-11D1-883B-3C8B00C10000}';
  DIID_IRosePackageCollection: TGUID = '{97B38364-A4E3-11D0-BFF0-00AA003DEF5B}';
  CLASS_RosePackageCollection: TGUID = '{BA376EE4-A44E-11D0-BC02-00A024C67143}';
  DIID_IRoseExternalDocument: TGUID = '{906FF583-276B-11D0-8980-00A024774419}';
  CLASS_RoseExternalDocument: TGUID = '{86652277-EBF7-11D0-BC10-00A024C67143}';



type
  RoseAddinEventTypes = TOleEnum;
const
  rsOnNewModel = $00000002;
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
type
  RoseNotationTypes = TOleEnum;
const
  BoochNotation = $00000000;
  OMTNotation = $00000001;
  UMLNotation = $00000002;
type
  RoseClientRelKind = TOleEnum;
const
  rsAnyKind = $00000000;
  rsFriend = $00000001;
type
  RosePersistence = TOleEnum;
const
  rsPersistent = $00000000;
  rsTransient = $00000001;
type
  RoseSynchronization = TOleEnum;
const
  rsSimple = $00000000;
  rsSynchronous = $00000001;
  rsBalking = $00000002;
  rsTimeout = $00000003;
  rsAsynchronous = $00000004;
type
  RoseFrequency = TOleEnum;
const
  rsAperiodic = $00000000;
  rsPeriodic = $00000001;
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
type
  RoseMenuState = TOleEnum;
const
  rsDisabled = $00000000;
  rsEnabled = $00000001;
  rsDisabledAndChecked = $00000002;
  rsDisabledAndUnchecked = $00000003;
  rsEnabledAndChecked = $00000004;
  rsEnabledAndUnchecked = $00000005;

type


  IRoseActivityViewCollection = dispinterface;
  IRoseProcessorCollection = dispinterface;
  IRoseCategoryCollection = dispinterface;
  IRoseDeploymentUnit = dispinterface;
  IRoseItem = dispinterface;
  IRoseContextMenuItemCollection = dispinterface;
  IRoseAddIn = dispinterface;
  IRoseDecisionViewCollection = dispinterface;
  IRoseStateVertex = dispinterface;
  IRoseUseCaseCollection = dispinterface;
  IRoseConnectionRelation = dispinterface;
  IRoseRelation = dispinterface;
  IRoseApplication = dispinterface;
  IRoseDecisionCollection = dispinterface;
  IRoseDecision = dispinterface;
  IRoseLineVertexCollection = dispinterface;
  IRoseInstantiateRelationCollection = dispinterface;
  IRoseMessageCollection = dispinterface;
  IRoseClassDiagramCollection = dispinterface;
  IRoseScenarioDiagram = dispinterface;
  IRoseRealizeRelation = dispinterface;
  IRoseHasRelationship = dispinterface;
  IRoseClassView = dispinterface;
  IRoseView_FillColor = dispinterface;
  IRoseActionCollection = dispinterface;
  IRoseSyncItemCollection = dispinterface;
  IRoseActivityCollection = dispinterface;
  IRoseActivity = dispinterface;
  IRoseProcess = dispinterface;
  IRoseAddInCollection = dispinterface;
  IRoseSwimLaneView = dispinterface;
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
  IRoseSwimLaneViewCollection = dispinterface;
  IRoseSwimLane = dispinterface;
  IRoseItemViewCollection = dispinterface;
  IRosePropertyCollection = dispinterface;
  IRoseOperationCollection = dispinterface;
  IRoseDeviceCollection = dispinterface;
  IRoseInstantiateRelation = dispinterface;
  IRoseContextMenuItem = dispinterface;
  IRoseLineVertex = dispinterface;
  IRoseObject = dispinterface;
  IRoseSwimLaneCollection = dispinterface;
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
  IRoseDecisionView = dispinterface;
  IRoseStateMachineOwner = dispinterface;
  IRoseProcessCollection = dispinterface;
  IRoseAssociationCollection = dispinterface;
  IRoseModuleDiagramCollection = dispinterface;
  IRoseDiagram = dispinterface;
  IRoseAbstractStateCollection = dispinterface;
  IRoseRichTypeValuesCollection = dispinterface;
  IRoseSubsystemView = dispinterface;
  IRoseComponentView = dispinterface;
  IRoseAttribute = dispinterface;
  IRoseClassDiagram = dispinterface;
  IRoseNoteView = dispinterface;
  IRosePackage = dispinterface;
  IRosePathMap = dispinterface;
  IRoseSyncItemViewCollection = dispinterface;
  IRoseAbstractState = dispinterface;
  IRoseModuleVisibilityRelationshipCollection = dispinterface;
  IRoseClassCollection = dispinterface;
  IRoseMessage = dispinterface;
  IRoseConnectionRelationCollection = dispinterface;
  IRoseClassDependencyCollection = dispinterface;
  IRoseAssociation = dispinterface;
  IRoseEventCollection = dispinterface;
  IRoseStateCollection = dispinterface;
  IRoseSyncItemView = dispinterface;
  IRoseActivityView = dispinterface;
  IRoseStateView = dispinterface;
  IRoseStateVertexCollection = dispinterface;
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
  IRoseStateMachineCollection = dispinterface;
  IRoseSyncItem = dispinterface;
  IRoseRealizeRelationCollection = dispinterface;
  IRosePackageCollection = dispinterface;
  IRoseExternalDocument = dispinterface;



  RoseActivityViewCollection = IRoseActivityViewCollection;
  RoseProcessorCollection = IRoseProcessorCollection;
  RoseCategoryCollection = IRoseCategoryCollection;
  RoseDeploymentUnit = IRoseDeploymentUnit;
  RoseItem = IRoseItem;
  RoseContextMenuItemCollection = IRoseContextMenuItemCollection;
  RoseAddIn = IRoseAddIn;
  RoseDecisionViewCollection = IRoseDecisionViewCollection;
  RoseStateVertex = IRoseStateVertex;
  RoseUseCaseCollection = IRoseUseCaseCollection;
  RoseConnectionRelation = IRoseConnectionRelation;
  RoseRelation = IRoseRelation;
  RoseApplication = IRoseApplication;
  RoseDecisionCollection = IRoseDecisionCollection;
  RoseDecision = IRoseDecision;
  RoseLineVertexCollection = IRoseLineVertexCollection;
  RoseInstantiateRelationCollection = IRoseInstantiateRelationCollection;
  RoseMessageCollection = IRoseMessageCollection;
  RoseClassDiagramCollection = IRoseClassDiagramCollection;
  RoseScenarioDiagram = IRoseScenarioDiagram;
  RoseRealizeRelation = IRoseRealizeRelation;
  RoseHasRelationship = IRoseHasRelationship;
  RoseClassView = IRoseClassView;
  RoseView_FillColor = IRoseView_FillColor;
  RoseActionCollection = IRoseActionCollection;
  RoseSyncItemCollection = IRoseSyncItemCollection;
  RoseActivityCollection = IRoseActivityCollection;
  RoseActivity = IRoseActivity;
  RoseProcess = IRoseProcess;
  RoseAddInCollection = IRoseAddInCollection;
  RoseSwimLaneView = IRoseSwimLaneView;
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
  RoseSwimLaneViewCollection = IRoseSwimLaneViewCollection;
  RoseSwimLane = IRoseSwimLane;
  RoseItemViewCollection = IRoseItemViewCollection;
  RosePropertyCollection = IRosePropertyCollection;
  RoseOperationCollection = IRoseOperationCollection;
  RoseDeviceCollection = IRoseDeviceCollection;
  RoseInstantiateRelation = IRoseInstantiateRelation;
  RoseContextMenuItem = IRoseContextMenuItem;
  RoseLineVertex = IRoseLineVertex;
  RoseObject = IRoseObject;
  RoseSwimLaneCollection = IRoseSwimLaneCollection;
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
  RoseDecisionView = IRoseDecisionView;
  RoseStateMachineOwner = IRoseStateMachineOwner;
  RoseProcessCollection = IRoseProcessCollection;
  RoseAssociationCollection = IRoseAssociationCollection;
  RoseModuleDiagramCollection = IRoseModuleDiagramCollection;
  RoseDiagram = IRoseDiagram;
  RoseAbstractStateCollection = IRoseAbstractStateCollection;
  RoseRichTypeValuesCollection = IRoseRichTypeValuesCollection;
  RoseSubsystemView = IRoseSubsystemView;
  RoseComponentView = IRoseComponentView;
  RoseAttribute = IRoseAttribute;
  RoseClassDiagram = IRoseClassDiagram;
  RoseNoteView = IRoseNoteView;
  RosePackage = IRosePackage;
  RosePathMap = IRosePathMap;
  RoseSyncItemViewCollection = IRoseSyncItemViewCollection;
  RoseAbstractState = IRoseAbstractState;
  RoseModuleVisibilityRelationshipCollection = IRoseModuleVisibilityRelationshipCollection;
  RoseClassCollection = IRoseClassCollection;
  RoseMessage = IRoseMessage;
  RoseConnectionRelationCollection = IRoseConnectionRelationCollection;
  RoseClassDependencyCollection = IRoseClassDependencyCollection;
  RoseAssociation = IRoseAssociation;
  RoseEventCollection = IRoseEventCollection;
  RoseStateCollection = IRoseStateCollection;
  RoseSyncItemView = IRoseSyncItemView;
  RoseActivityView = IRoseActivityView;
  RoseStateView = IRoseStateView;
  RoseStateVertexCollection = IRoseStateVertexCollection;
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
  RoseStateMachineCollection = IRoseStateMachineCollection;
  RoseSyncItem = IRoseSyncItem;
  RoseRealizeRelationCollection = IRoseRealizeRelationCollection;
  RosePackageCollection = IRosePackageCollection;
  RoseExternalDocument = IRoseExternalDocument;




  IRoseActivityViewCollection = dispinterface
    ['{BEAED5FE-578D-11D2-92AA-004005141253}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseActivityView; dispid 203;
    function  Exists(const pObject: IRoseActivityView): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseActivityView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseActivityView); dispid 208;
    procedure AddCollection(const theCollection: IRoseActivityViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseActivityView); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseActivityView; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseActivityView; dispid 213;
  end;




  IRoseProcessorCollection = dispinterface
    ['{97B3835C-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseProcessor; dispid 203;
    function  Exists(const pObject: IRoseProcessor): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseProcessor): Smallint; dispid 207;
    procedure Add(const theObject: IRoseProcessor); dispid 208;
    procedure AddCollection(const theCollection: IRoseProcessorCollection); dispid 209;
    procedure Remove(const theObject: IRoseProcessor); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseProcessor; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseProcessor; dispid 213;
  end;




  IRoseCategoryCollection = dispinterface
    ['{97B3835B-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseCategory; dispid 203;
    function  Exists(const pObject: IRoseCategory): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseCategory): Smallint; dispid 207;
    procedure Add(const theObject: IRoseCategory); dispid 208;
    procedure AddCollection(const theCollection: IRoseCategoryCollection); dispid 209;
    procedure Remove(const theObject: IRoseCategory); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseCategory; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseCategory; dispid 213;
  end;




  IRoseDeploymentUnit = dispinterface
    ['{4335FBE3-F0A0-11D1-9FAD-0060975306FE}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  IsControlled: WordBool; dispid 12433;
    function  Control(const Path: WideString): WordBool; dispid 12434;
    function  IsLoaded: WordBool; dispid 12435;
    function  Load: WordBool; dispid 12436;
    function  IsModifiable: WordBool; dispid 12438;
    function  Unload: WordBool; dispid 12439;
    function  Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function  GetFileName: WideString; dispid 12441;
    function  Save: WordBool; dispid 12442;
    function  SaveAs(const Path: WideString): WordBool; dispid 12443;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IsModified: WordBool; dispid 12654;
    function  Uncontrol: WordBool; dispid 12655;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    procedure Refresh; dispid 12701;
    function  GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function  IsLocked: WordBool; dispid 12703;
    function  NeedsRefreshing: WordBool; dispid 12704;
    function  GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    procedure Lock; dispid 12708;
    procedure Unlock; dispid 12709;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseContextMenuItemCollection = dispinterface
    ['{EE0B16E2-FF91-11D1-9FAD-0060975306FE}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseContextMenuItem; dispid 203;
    function  Exists(const pObject: IRoseContextMenuItem): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseContextMenuItem): Smallint; dispid 207;
    procedure Add(const theObject: IRoseContextMenuItem); dispid 208;
    procedure AddCollection(const theCollection: IRoseContextMenuItemCollection); dispid 209;
    procedure Remove(const theObject: IRoseContextMenuItem); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseContextMenuItem; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseContextMenuItem; dispid 213;
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
    function  IsLanguageAddIn: WordBool; dispid 12541;
    procedure Deactivate; dispid 12542;
    procedure Activate; dispid 12543;
    function  IsActive: WordBool; dispid 12553;
    procedure ExecuteScript(const FileName: WideString); dispid 12595;
    function  ReadSetting(const Section: WideString; const Entry: WideString; 
                          const Default: WideString): WideString; dispid 12596;
    function  WriteSetting(const Section: WideString; const Entry: WideString; 
                           const Value: WideString): WordBool; dispid 12597;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  AddContextMenuItem(itemType: Smallint; const fullCaption: WideString; 
                                 const internalName: WideString): IRoseContextMenuItem; dispid 12684;
    function  GetContextMenuItems(itemType: Smallint): IRoseContextMenuItemCollection; dispid 12685;
    function  GetDisplayName: WideString; dispid 12689;
  end;




  IRoseDecisionViewCollection = dispinterface
    ['{BEAED601-578D-11D2-92AA-004005141253}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseDecisionView; dispid 203;
    function  Exists(const pObject: IRoseDecisionView): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseDecisionView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseDecisionView); dispid 208;
    procedure AddCollection(const theCollection: IRoseDecisionViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseDecisionView); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseDecisionView; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseDecisionView; dispid 213;
  end;




  IRoseStateVertex = dispinterface
    ['{BEAED5E2-578D-11D2-92AA-004005141253}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property ParentStateMachine: IRoseStateMachine dispid 413;
    property Transitions: IRoseTransitionCollection dispid 422;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property Parent: IRoseStateVertex dispid 12747;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  AddTransition(const OnEvent: WideString; const Target: IRoseState): IRoseTransition; dispid 426;
    function  DeleteTransition(const Transition: IRoseTransition): WordBool; dispid 427;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  GetSwimLanes: IRoseSwimLaneCollection; dispid 12748;
    function  AddTransitionToVertex(const OnEvent: WideString; const Target: IRoseStateVertex): IRoseTransition; dispid 12814;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseUseCaseCollection = dispinterface
    ['{97B38356-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseUseCase; dispid 203;
    function  Exists(const pObject: IRoseUseCase): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseUseCase): Smallint; dispid 207;
    procedure Add(const theObject: IRoseUseCase); dispid 208;
    procedure AddCollection(const theCollection: IRoseUseCaseCollection); dispid 209;
    procedure Remove(const theObject: IRoseUseCase); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseUseCase; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseUseCase; dispid 213;
  end;




  IRoseConnectionRelation = dispinterface
    ['{4467F442-F24E-11D2-92AA-004005141253}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property SupplierName: WideString dispid 412;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Characteristics: WideString dispid 12815;
    property SupplierIsDevice: WordBool dispid 12816;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  HasClient: WordBool; dispid 12606;
    function  HasSupplier: WordBool; dispid 12607;
    function  GetClient: IRoseItem; dispid 12608;
    function  GetSupplier: IRoseItem; dispid 12609;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  HasClient: WordBool; dispid 12606;
    function  HasSupplier: WordBool; dispid 12607;
    function  GetClient: IRoseItem; dispid 12608;
    function  GetSupplier: IRoseItem; dispid 12609;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property IsInitialized: WordBool dispid 12809;
    function  OpenModel(const theModel: WideString): IRoseModel; dispid 210;
    function  NewModel: IRoseModel; dispid 211;
    procedure Exit; dispid 212;
    procedure WriteErrorLog(const theMsg: WideString); dispid 213;
    procedure Save(bSaveUnits: WordBool); dispid 214;
    procedure SaveAs(const theFile: WideString; bSaveUnits: WordBool); dispid 215;
    procedure CompileScriptFile(const FileName: WideString; const BinaryName: WideString; 
                                bDebug: WordBool); dispid 218;
    function  OpenModelAsTemplate(const szFileName: WideString): IRoseModel; dispid 223;
    procedure OpenScript(const FileName: WideString); dispid 225;
    procedure NewScript; dispid 226;
    function  GetLicensedApplication(const theKey: WideString): IRoseApplication; dispid 235;
    procedure ExecuteScript(const pFileName: WideString); dispid 236;
    function  OpenURL(const theURL: WideString): WordBool; dispid 12587;
    function  OpenExternalDocument(const FileName: WideString): WordBool; dispid 12588;
    function  GetProfileString(const Section: WideString; const Entry: WideString; 
                               const Default: WideString): WideString; dispid 12589;
    function  WriteProfileString(const Section: WideString; const Entry: WideString; 
                                 const Value: WideString): WordBool; dispid 12590;
    function  UpdateBrowserOverlayImage(const theItem: IRoseItem): WordBool; dispid 12679;
    function  UpdateBrowserDocOverlayImage(const theDocument: IRoseExternalDocument): WordBool; dispid 12688;
    function  OpenRoseModel(const theModel: WideString; promptSubUnits: WordBool): IRoseModel; dispid 12697;
    function  GetRoseIniPath: WideString; dispid 12698;
  end;




  IRoseDecisionCollection = dispinterface
    ['{BEAED5F2-578D-11D2-92AA-004005141253}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseDecision; dispid 203;
    function  Exists(const pObject: IRoseDecision): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseDecision): Smallint; dispid 207;
    procedure Add(const theObject: IRoseDecision); dispid 208;
    procedure AddCollection(const theCollection: IRoseDecisionCollection); dispid 209;
    procedure Remove(const theObject: IRoseDecision); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseDecision; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseDecision; dispid 213;
  end;




  IRoseDecision = dispinterface
    ['{BEAED5E3-578D-11D2-92AA-004005141253}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property ParentStateMachine: IRoseStateMachine dispid 413;
    property Transitions: IRoseTransitionCollection dispid 422;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property Parent: IRoseStateVertex dispid 12747;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  AddTransition(const OnEvent: WideString; const Target: IRoseState): IRoseTransition; dispid 426;
    function  DeleteTransition(const Transition: IRoseTransition): WordBool; dispid 427;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  GetSwimLanes: IRoseSwimLaneCollection; dispid 12748;
    function  AddTransitionToVertex(const OnEvent: WideString; const Target: IRoseStateVertex): IRoseTransition; dispid 12814;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseLineVertexCollection = dispinterface
    ['{11A235B2-3095-11D2-8153-00104B97EBD5}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseLineVertex; dispid 203;
    function  Exists(const pObject: IRoseLineVertex): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseLineVertex): Smallint; dispid 207;
    procedure Add(const theObject: IRoseLineVertex); dispid 208;
    procedure AddCollection(const theCollection: IRoseLineVertexCollection); dispid 209;
    procedure Remove(const theObject: IRoseLineVertex); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseLineVertex; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseLineVertex; dispid 213;
  end;




  IRoseInstantiateRelationCollection = dispinterface
    ['{B91D8F06-DDBB-11D1-9FAD-0060975306FE}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseInstantiateRelation; dispid 203;
    function  Exists(const pObject: IRoseInstantiateRelation): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseInstantiateRelation): Smallint; dispid 207;
    procedure Add(const theObject: IRoseInstantiateRelation); dispid 208;
    procedure AddCollection(const theCollection: IRoseInstantiateRelationCollection); dispid 209;
    procedure Remove(const theObject: IRoseInstantiateRelation); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseInstantiateRelationCollection; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseInstantiateRelation; dispid 213;
  end;




  IRoseMessageCollection = dispinterface
    ['{97B38359-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseMessage; dispid 203;
    function  Exists(const pObject: IRoseMessage): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseMessage): Smallint; dispid 207;
    procedure Add(const theObject: IRoseMessage); dispid 208;
    procedure AddCollection(const theCollection: IRoseMessageCollection); dispid 209;
    procedure Remove(const theObject: IRoseMessage); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseMessage; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseMessage; dispid 213;
  end;




  IRoseClassDiagramCollection = dispinterface
    ['{97B38343-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseClassDiagram; dispid 203;
    function  Exists(const pObject: IRoseClassDiagram): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseClassDiagram): Smallint; dispid 207;
    procedure Add(const theObject: IRoseClassDiagram); dispid 208;
    procedure AddCollection(const theCollection: IRoseClassDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseClassDiagram); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseClassDiagram; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseClassDiagram; dispid 213;
  end;




  IRoseScenarioDiagram = dispinterface
    ['{F819833A-FC55-11CF-BBD3-00A024C67143}']
    property Name: WideString dispid 100;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Visible: WordBool dispid 203;
    property Items: IRoseItemCollection dispid 208;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property InstanceViews: IRoseInstanceViewCollection dispid 423;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property Documentation: WideString dispid 12656;
    property ZoomFactor: Smallint dispid 12690;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Invalidate; dispid 205;
    procedure Update; dispid 206;
    function  GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function  IsActive: WordBool; dispid 209;
    function  Exists(const theItem: IRoseItem): WordBool; dispid 210;
    procedure Activate; dispid 211;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    procedure Render(const FileName: WideString); dispid 217;
    function  AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function  RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function  GetNoteViews: IRoseNoteViewCollection; dispid 220;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function  GetObjects: IRoseObjectInstanceCollection; dispid 411;
    function  GetSelectedObjects: IRoseObjectInstanceCollection; dispid 412;
    function  GetMessages: IRoseMessageCollection; dispid 413;
    function  GetSelectedMessages: IRoseMessageCollection; dispid 414;
    function  CreateMessage(const theName: WideString; const theSender: IRoseObjectInstance; 
                            const theReceiver: IRoseObjectInstance; theSequence: Smallint): IRoseMessage; dispid 416;
    function  GetSelectedLinks: IRoseLinkCollection; dispid 417;
    function  GetDiagramType: Smallint; dispid 418;
    function  AddInstanceView(const theInstance: IRoseObjectInstance; AsClassInstance: WordBool): IRoseInstanceView; dispid 419;
    function  RemoveInstanceView(const theView: IRoseInstanceView): WordBool; dispid 420;
    function  DeleteInstance(const theInstance: IRoseObjectInstance): WordBool; dispid 421;
    function  AddInstance(const theName: WideString; const theClassName: WideString): IRoseObjectInstance; dispid 422;
    function  GetSelectedItems: IRoseItemCollection; dispid 12525;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  AddRelationView(const theRelation: IRoseRelation): WordBool; dispid 12787;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetParentContext: IRoseItem; dispid 12823;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  GetContextClass: IRoseClass; dispid 12602;
    function  GetContextComponent: IRoseModule; dispid 12603;
    function  GetSupplierClass: IRoseClass; dispid 12604;
    function  GetSupplierComponent: IRoseModule; dispid 12605;
    function  HasClient: WordBool; dispid 12606;
    function  HasSupplier: WordBool; dispid 12607;
    function  GetClient: IRoseItem; dispid 12608;
    function  GetSupplier: IRoseItem; dispid 12609;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  GetContextClass: IRoseClass; dispid 12600;
    function  GetSupplierClass: IRoseClass; dispid 12601;
    function  HasClient: WordBool; dispid 12606;
    function  HasSupplier: WordBool; dispid 12607;
    function  GetClient: IRoseItem; dispid 12608;
    function  GetSupplier: IRoseItem; dispid 12609;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function  SupportsFillColor: WordBool; dispid 210;
    function  SupportsLineColor: WordBool; dispid 211;
    function  IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function  PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function  GetDefaultWidth: Smallint; dispid 215;
    function  GetDefaultHeight: Smallint; dispid 216;
    function  GetMinWidth: Smallint; dispid 217;
    function  GetMinHeight: Smallint; dispid 218;
    function  HasItem: WordBool; dispid 222;
    function  HasParentView: WordBool; dispid 223;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
    function  GetDisplayedAttributes: IRoseItemCollection; dispid 12826;
    function  GetDisplayedOperations: IRoseItemCollection; dispid 12827;
  end;




  IRoseView_FillColor = dispinterface
    ['{CE5BE563-0380-11D1-BC11-00A024C67143}']
    property Red: Smallint dispid 12493;
    property Transparent: WordBool dispid 12494;
    property Blue: Smallint dispid 12495;
    property Green: Smallint dispid 12496;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;




  IRoseActionCollection = dispinterface
    ['{97B3835F-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseAction; dispid 203;
    function  Exists(const pObject: IRoseAction): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseAction): Smallint; dispid 207;
    procedure Add(const theObject: IRoseAction); dispid 208;
    procedure AddCollection(const theCollection: IRoseActionCollection); dispid 209;
    procedure Remove(const theObject: IRoseAction); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseAction; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseAction; dispid 213;
  end;




  IRoseSyncItemCollection = dispinterface
    ['{94CA188F-5D13-11D2-92AA-004005141253}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseSyncItem; dispid 203;
    function  Exists(const pObject: IRoseSyncItem): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseSyncItem): Smallint; dispid 207;
    procedure Add(const theObject: IRoseSyncItem); dispid 208;
    procedure AddCollection(const theCollection: IRoseSyncItemCollection); dispid 209;
    procedure Remove(const theObject: IRoseSyncItem); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseSyncItem; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseSyncItem; dispid 213;
  end;




  IRoseActivityCollection = dispinterface
    ['{BEAED5F0-578D-11D2-92AA-004005141253}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseActivity; dispid 203;
    function  Exists(const pObject: IRoseActivity): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseActivity): Smallint; dispid 207;
    procedure Add(const theObject: IRoseActivity); dispid 208;
    procedure AddCollection(const theCollection: IRoseActivityCollection); dispid 209;
    procedure Remove(const theObject: IRoseActivity); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseActivity; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseActivity; dispid 213;
  end;




  IRoseActivity = dispinterface
    ['{BEAED5E7-578D-11D2-92AA-004005141253}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property ParentStateMachine: IRoseStateMachine dispid 413;
    property Transitions: IRoseTransitionCollection dispid 422;
    property SubStates: IRoseStateCollection dispid 444;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property Parent: IRoseStateVertex dispid 12747;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property SubActivities: IRoseActivityCollection dispid 12802;
    property SubDecisions: IRoseDecisionCollection dispid 12803;
    property SubSynchronizations: IRoseSyncItemCollection dispid 12804;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetAllSubStates: IRoseStateCollection; dispid 425;
    function  AddTransition(const OnEvent: WideString; const Target: IRoseState): IRoseTransition; dispid 426;
    function  DeleteTransition(const Transition: IRoseTransition): WordBool; dispid 427;
    function  DeleteAction(const theAction: IRoseAction): WordBool; dispid 446;
    function  GetQualifiedName: WideString; dispid 12555;
    function  GetUserDefinedEvents: IRoseEventCollection; dispid 12623;
    function  GetEntryActions: IRoseActionCollection; dispid 12625;
    function  GetExitActions: IRoseActionCollection; dispid 12626;
    function  GetDoActions: IRoseActionCollection; dispid 12627;
    function  AddUserDefinedEvent(const EventName: WideString; const ActionName: WideString): IRoseEvent; dispid 12635;
    function  DeleteUserDefinedEvent(const theEvent: IRoseEvent): WordBool; dispid 12636;
    function  AddEntryAction(const ActionName: WideString): IRoseAction; dispid 12637;
    function  AddExitAction(const ActionName: WideString): IRoseAction; dispid 12638;
    function  AddDoAction(const ActionName: WideString): IRoseAction; dispid 12639;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  GetSwimLanes: IRoseSwimLaneCollection; dispid 12748;
    function  GetActions: IRoseActionCollection; dispid 12767;
    function  GetStateMachines: IRoseStateMachineCollection; dispid 12768;
    function  AddStateMachine(const theName: WideString): IRoseStateMachine; dispid 12774;
    function  DeleteStateMachine(const theStateMachine: IRoseStateMachine): WordBool; dispid 12775;
    function  GetAllSubActivities: IRoseActivityCollection; dispid 12805;
    function  GetAllSubDecisions: IRoseDecisionCollection; dispid 12806;
    function  GetAllSubSynchronizations: IRoseSyncItemCollection; dispid 12807;
    function  AddTransitionToVertex(const OnEvent: WideString; const Target: IRoseStateVertex): IRoseTransition; dispid 12814;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseAddInCollection = dispinterface
    ['{C87D2BC1-352A-11D1-883B-3C8B00C10000}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseAddIn; dispid 203;
    function  Exists(const pObject: IRoseAddIn): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseAddIn): Smallint; dispid 207;
    procedure Add(const theObject: IRoseAddIn); dispid 208;
    procedure AddCollection(const theCollection: IRoseAddInCollection); dispid 209;
    procedure Remove(const theObject: IRoseAddIn); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseAddIn; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseAddIn; dispid 213;
  end;




  IRoseSwimLaneView = dispinterface
    ['{68F63C21-B047-11D2-92AA-004005141253}']
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
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function  SupportsFillColor: WordBool; dispid 210;
    function  SupportsLineColor: WordBool; dispid 211;
    function  IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function  PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function  GetDefaultWidth: Smallint; dispid 215;
    function  GetDefaultHeight: Smallint; dispid 216;
    function  GetMinWidth: Smallint; dispid 217;
    function  GetMinHeight: Smallint; dispid 218;
    function  HasItem: WordBool; dispid 222;
    function  HasParentView: WordBool; dispid 223;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseControllableUnitCollection = dispinterface
    ['{97B38360-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseControllableUnit; dispid 203;
    function  Exists(const pObject: IRoseControllableUnit): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseControllableUnit): Smallint; dispid 207;
    procedure Add(const theObject: IRoseControllableUnit); dispid 208;
    procedure AddCollection(const theCollection: IRoseControllableUnitCollection); dispid 209;
    procedure Remove(const theObject: IRoseControllableUnit); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseControllableUnit; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseControllableUnit; dispid 213;
  end;




  IRoseModuleCollection = dispinterface
    ['{97B3834B-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseModule; dispid 203;
    function  Exists(const pObject: IRoseModule): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseModule): Smallint; dispid 207;
    procedure Add(const theObject: IRoseModule); dispid 208;
    procedure AddCollection(const theCollection: IRoseModuleCollection); dispid 209;
    procedure Remove(const theObject: IRoseModule); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseModule; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseModule; dispid 213;
  end;




  IRoseLinkCollection = dispinterface
    ['{9DE9A9C1-F2D0-11D0-883A-3C8B00C10000}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseLink; dispid 203;
    function  Exists(const pObject: IRoseLink): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseLink): Smallint; dispid 207;
    procedure Add(const theObject: IRoseLink); dispid 208;
    procedure AddCollection(const theCollection: IRoseLinkCollection); dispid 209;
    procedure Remove(const theObject: IRoseLink); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseLink; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseLink; dispid 213;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseParameterCollection = dispinterface
    ['{97B38352-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseParameter; dispid 203;
    function  Exists(const pObject: IRoseParameter): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseParameter): Smallint; dispid 207;
    procedure Add(const theObject: IRoseParameter); dispid 208;
    procedure AddCollection(const theCollection: IRoseParameterCollection); dispid 209;
    procedure Remove(const theObject: IRoseParameter); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseParameter; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseParameter; dispid 213;
  end;




  IRoseAttributeCollection = dispinterface
    ['{97B3834C-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseAttribute; dispid 203;
    function  Exists(const pObject: IRoseAttribute): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseAttribute): Smallint; dispid 207;
    procedure Add(const theObject: IRoseAttribute); dispid 208;
    procedure AddCollection(const theCollection: IRoseAttributeCollection); dispid 209;
    procedure Remove(const theObject: IRoseAttribute); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseAttribute; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseAttribute; dispid 213;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Connections: IRoseConnectionRelationCollection dispid 12818;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetConnectedProcessors: IRoseProcessorCollection; dispid 413;
    function  GetConnectedDevices: IRoseDeviceCollection; dispid 414;
    function  AddProcessorConnection(const theProcessor: IRoseProcessor): WordBool; dispid 415;
    function  RemoveProcessorConnection(const theProcessor: IRoseProcessor): WordBool; dispid 416;
    function  AddDeviceConnection(const theDevice: IRoseDevice): WordBool; dispid 417;
    function  RemoveDeviceConnection(const theDevice: IRoseDevice): WordBool; dispid 418;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  GetContextClass: IRoseClass; dispid 12600;
    function  GetSupplierClass: IRoseClass; dispid 12601;
    function  HasClient: WordBool; dispid 12606;
    function  HasSupplier: WordBool; dispid 12607;
    function  GetClient: IRoseItem; dispid 12608;
    function  GetSupplier: IRoseItem; dispid 12609;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  AddKey(const theName: WideString; const theType: WideString): IRoseAttribute; dispid 623;
    function  DeleteKey(const theAttr: IRoseAttribute): WordBool; dispid 624;
    function  GetClassName: WideString; dispid 625;
    function  IsAssociateClass: WordBool; dispid 628;
    function  GetQualifiedName: WideString; dispid 12555;
    function  HasClient: WordBool; dispid 12606;
    function  HasSupplier: WordBool; dispid 12607;
    function  GetClient: IRoseItem; dispid 12608;
    function  GetSupplier: IRoseItem; dispid 12609;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetHasRelations: IRoseHasRelationshipCollection; dispid 422;
    function  GetInheritRelations: IRoseInheritRelationCollection; dispid 423;
    function  GetSuperclasses: IRoseClassCollection; dispid 424;
    function  GetAssociations: IRoseAssociationCollection; dispid 425;
    function  AddOperation(const theName: WideString; const retType: WideString): IRoseOperation; dispid 427;
    function  AddAttribute(const theName: WideString; const theType: WideString; 
                           const initVal: WideString): IRoseAttribute; dispid 428;
    function  AddAssociation(const theSupplierRoleName: WideString; 
                             const theSupplierRoleType: WideString): IRoseAssociation; dispid 429;
    function  AddHas(const theSupplierName: WideString; const theSupplierType: WideString): IRoseHasRelationship; dispid 430;
    function  DeleteHas(const theHas: IRoseHasRelationship): WordBool; dispid 432;
    function  DeleteAssociation(const theAss: IRoseAssociation): WordBool; dispid 433;
    function  DeleteOperation(const theOper: IRoseOperation): WordBool; dispid 434;
    function  DeleteAttribute(const theAttr: IRoseAttribute): WordBool; dispid 435;
    function  AddInheritRel(const theRelationName: WideString; const theParentClassName: WideString): IRoseInheritRelation; dispid 437;
    function  DeleteInheritRel(const theInheritRel: IRoseInheritRelation): WordBool; dispid 438;
    function  IsALinkClass: WordBool; dispid 439;
    function  GetLinkAssociation: IRoseAssociation; dispid 440;
    function  GetRoles: IRoseRoleCollection; dispid 444;
    function  GetAssociateRoles: IRoseRoleCollection; dispid 445;
    function  GetNestedClasses: IRoseClassCollection; dispid 446;
    function  AddNestedClass(const theName: WideString): IRoseClass; dispid 447;
    function  DeleteNestedClass(const theClass: IRoseClass): WordBool; dispid 448;
    function  GetAssignedModules: IRoseModuleCollection; dispid 12491;
    procedure AddAssignedModule(const theModule: IRoseModule); dispid 12522;
    procedure RemoveAssignedModule(const theModule: IRoseModule); dispid 12526;
    function  GetQualifiedName: WideString; dispid 12555;
    procedure CreateStateMachine; dispid 12598;
    procedure DeleteStateMachine; dispid 12599;
    function  AddRealizeRel(const theRelationName: WideString; const theInterfaceName: WideString): IRoseRealizeRelation; dispid 12610;
    function  DeleteRealizeRel(const theRealizeRel: IRoseRealizeRelation): WordBool; dispid 12611;
    function  GetRealizeRelations: IRoseRealizeRelationCollection; dispid 12612;
    function  GetAssignedLanguage: WideString; dispid 12642;
    function  IsNestedClass: WordBool; dispid 12643;
    function  GetSubclasses: IRoseClassCollection; dispid 12644;
    function  GetClassDependencies: IRoseClassDependencyCollection; dispid 12662;
    function  AddClassDependency(const theSupplerName: WideString; const theSupplierType: WideString): IRoseClassDependency; dispid 12663;
    function  DeleteClassDependency(const theClassDependency: IRoseClassDependency): WordBool; dispid 12664;
    function  AddParameter(const theName: WideString; const theType: WideString; 
                           const theDef: WideString; position: Smallint): IRoseParameter; dispid 12667;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  GetInstantiateRelations: IRoseInstantiateRelationCollection; dispid 12670;
    function  AddInstantiateRel(const theSupplierName: WideString): IRoseInstantiateRelation; dispid 12671;
    function  DeleteInstantiateRel(const theInstantiateRel: IRoseInstantiateRelation): WordBool; dispid 12672;
    function  GetClients(relationKind: Smallint; relationType: Smallint): IRoseClassCollection; dispid 12673;
    function  DeleteParameter(const theParameter: IRoseParameter): WordBool; dispid 12678;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  GetAllNestedClasses: IRoseClassCollection; dispid 12808;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseElement = dispinterface
    ['{D067F15F-6987-11D0-BBF0-00A024C67143}']
    property Name: WideString dispid 100;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  IsControlled: WordBool; dispid 12433;
    function  Control(const Path: WideString): WordBool; dispid 12434;
    function  IsLoaded: WordBool; dispid 12435;
    function  Load: WordBool; dispid 12436;
    function  IsModifiable: WordBool; dispid 12438;
    function  Unload: WordBool; dispid 12439;
    function  Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function  GetFileName: WideString; dispid 12441;
    function  Save: WordBool; dispid 12442;
    function  SaveAs(const Path: WideString): WordBool; dispid 12443;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IsModified: WordBool; dispid 12654;
    function  Uncontrol: WordBool; dispid 12655;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    procedure Refresh; dispid 12701;
    function  GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function  IsLocked: WordBool; dispid 12703;
    function  NeedsRefreshing: WordBool; dispid 12704;
    function  GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    procedure Lock; dispid 12708;
    procedure Unlock; dispid 12709;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property DeploymentUnit: IRoseDeploymentUnit dispid 12676;
    property DefaultLanguage: WideString dispid 12680;
    property Notation: Smallint dispid 12691;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetAllAssociations: IRoseAssociationCollection; dispid 412;
    function  AddProcessor(const pName: WideString): IRoseProcessor; dispid 424;
    function  DeleteProcessor(const pProcessor: IRoseProcessor): WordBool; dispid 425;
    function  AddDevice(const pName: WideString): IRoseDevice; dispid 426;
    function  DeleteDevice(const pDevice: IRoseDevice): WordBool; dispid 427;
    function  GetSelectedClasses: IRoseClassCollection; dispid 428;
    function  GetSelectedCategories: IRoseCategoryCollection; dispid 429;
    function  GetSelectedModules: IRoseModuleCollection; dispid 430;
    function  GetSelectedSubsystems: IRoseSubsystemCollection; dispid 431;
    function  GetAllClasses: IRoseClassCollection; dispid 432;
    function  GetAllCategories: IRoseCategoryCollection; dispid 433;
    function  GetAllModules: IRoseModuleCollection; dispid 434;
    function  GetAllSubsystems: IRoseSubsystemCollection; dispid 435;
    function  GetAllProcessors: IRoseProcessorCollection; dispid 436;
    function  GetAllDevices: IRoseDeviceCollection; dispid 437;
    function  GetSelectedUseCases: IRoseUseCaseCollection; dispid 438;
    function  GetAllUseCases: IRoseUseCaseCollection; dispid 439;
    function  IsRootPackage: WordBool; dispid 621;
    function  IsControlled: WordBool; dispid 12433;
    function  Control(const Path: WideString): WordBool; dispid 12434;
    function  IsLoaded: WordBool; dispid 12435;
    function  Load: WordBool; dispid 12436;
    function  IsModifiable: WordBool; dispid 12438;
    function  Unload: WordBool; dispid 12439;
    function  Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function  GetFileName: WideString; dispid 12441;
    function  Save: WordBool; dispid 12442;
    function  SaveAs(const Path: WideString): WordBool; dispid 12443;
    function  FindItems(const ItemName: WideString): IRoseItemCollection; dispid 12472;
    function  FindItemWithID(const UniqueID: WideString): IRoseItem; dispid 12473;
    function  FindClasses(const ClassName: WideString): IRoseClassCollection; dispid 12474;
    function  FindClassWithID(const UniqueID: WideString): IRoseClass; dispid 12475;
    function  FindCategories(const CategoryName: WideString): IRoseCategoryCollection; dispid 12476;
    function  FindCategoryWithID(const UniqueID: WideString): IRoseCategory; dispid 12477;
    function  GetActiveDiagram: IRoseDiagram; dispid 12527;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IsModified: WordBool; dispid 12654;
    function  Uncontrol: WordBool; dispid 12655;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  GetSelectedItems: IRoseItemCollection; dispid 12681;
    procedure Refresh; dispid 12701;
    function  GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function  IsLocked: WordBool; dispid 12703;
    function  NeedsRefreshing: WordBool; dispid 12704;
    function  GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    procedure Lock; dispid 12708;
    procedure Unlock; dispid 12709;
    procedure ResolveReferences; dispid 12710;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  GetSelectedExternalDocuments: IRoseExternalDocumentCollection; dispid 12788;
    function  FindDiagramWithID(const UniqueID: WideString): IRoseDiagram; dispid 12817;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  RedirectTo(const newTarget: IRoseState): WordBool; dispid 642;
    function  GetQualifiedName: WideString; dispid 12555;
    function  HasClient: WordBool; dispid 12606;
    function  HasSupplier: WordBool; dispid 12607;
    function  GetClient: IRoseItem; dispid 12608;
    function  GetSupplier: IRoseItem; dispid 12609;
    function  GetTriggerAction: IRoseAction; dispid 12629;
    function  GetSendAction: IRoseAction; dispid 12630;
    function  GetTargetState: IRoseState; dispid 12631;
    function  GetSourceState: IRoseState; dispid 12632;
    function  GetTriggerEvent: IRoseEvent; dispid 12633;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  GetTargetStateVertex: IRoseStateVertex; dispid 12812;
    function  GetSourceStateVertex: IRoseStateVertex; dispid 12813;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseSubsystemCollection = dispinterface
    ['{97B3834A-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseSubsystem; dispid 203;
    function  Exists(const pObject: IRoseSubsystem): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseSubsystem): Smallint; dispid 207;
    procedure Add(const theObject: IRoseSubsystem); dispid 208;
    procedure AddCollection(const theCollection: IRoseSubsystemCollection); dispid 209;
    procedure Remove(const theObject: IRoseSubsystem); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseSubsystem; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseSubsystem; dispid 213;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property Connections: IRoseConnectionRelationCollection dispid 12819;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetConnectedDevices: IRoseDeviceCollection; dispid 415;
    function  GetConnectedProcessors: IRoseProcessorCollection; dispid 416;
    function  AddProcess(const Name: WideString): IRoseProcess; dispid 417;
    function  DeleteProcess(const theProcess: IRoseProcess): WordBool; dispid 418;
    function  AddProcessorConnection(const Processor: IRoseProcessor): WordBool; dispid 419;
    function  RemoveProcessorConnection(const theProcessor: IRoseProcessor): WordBool; dispid 420;
    function  AddDeviceConnection(const theDevice: IRoseDevice): WordBool; dispid 421;
    function  RemoveDeviceConnection(const theDevice: IRoseDevice): WordBool; dispid 422;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseCategoryDependencyCollection = dispinterface
    ['{4ACE189D-6CD3-11D1-BC1E-00A024C67143}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseCategoryDependency; dispid 203;
    function  Exists(const pObject: IRoseCategoryDependency): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IDispatch): Smallint; dispid 207;
    procedure Add(const theObject: IRoseCategoryDependency); dispid 208;
    procedure AddCollection(const theCollection: IRoseCategoryDependencyCollection); dispid 209;
    procedure Remove(const theObject: IRoseCategoryDependency); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseCategoryDependency; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseCategoryDependency; dispid 213;
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
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Parent: IRoseStateMachine dispid 445;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property Documentation: WideString dispid 12656;
    property ZoomFactor: Smallint dispid 12690;
    property IsActivityDiagram: WordBool dispid 12761;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Invalidate; dispid 205;
    procedure Update; dispid 206;
    function  GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function  IsActive: WordBool; dispid 209;
    function  Exists(const theItem: IRoseItem): WordBool; dispid 210;
    procedure Activate; dispid 211;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    procedure Render(const FileName: WideString); dispid 217;
    function  AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function  RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function  GetNoteViews: IRoseNoteViewCollection; dispid 220;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function  AddStateView(const aState: IRoseState): IRoseStateView; dispid 421;
    function  RemoveStateView(const View: IRoseStateView): WordBool; dispid 422;
    function  GetSelectedStateViews: IRoseStateViewCollection; dispid 423;
    function  GetStateViews: IRoseStateViewCollection; dispid 424;
    function  GetSelectedTransitions: IRoseTransitionCollection; dispid 425;
    function  GetStateView(const State: IRoseState): IRoseStateView; dispid 426;
    function  GetSelectedStates: IRoseStateCollection; dispid 427;
    function  GetSelectedItems: IRoseItemCollection; dispid 12525;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  AddActivityView(const theActivity: IRoseActivity): IRoseActivityView; dispid 12714;
    function  AddDecisionView(const theDecision: IRoseDecision): IRoseDecisionView; dispid 12715;
    function  GetSelectedActivities: IRoseActivityCollection; dispid 12717;
    function  GetSelectedDecisions: IRoseDecisionCollection; dispid 12718;
    function  GetSelectedActivityViews: IRoseActivityViewCollection; dispid 12719;
    function  GetSelectedDecisionViews: IRoseDecisionViewCollection; dispid 12720;
    function  RemoveActivityView(const theActivityView: IRoseActivityView): WordBool; dispid 12724;
    function  RemoveDecisionView(const theDecisionView: IRoseDecisionView): WordBool; dispid 12725;
    function  AddSynchronizationView(const theSyncItem: IRoseSyncItem; isHorizontal: WordBool): IRoseSyncItemView; dispid 12749;
    function  GetSelectedSynchronizations: IRoseSyncItemCollection; dispid 12750;
    function  GetSelectedSynchronizationViews: IRoseSyncItemViewCollection; dispid 12751;
    function  GetDiagramStateVertexViews: IRoseItemViewCollection; dispid 12753;
    function  GetDiagramActivityViews: IRoseActivityViewCollection; dispid 12754;
    function  GetDiagramDecisionViews: IRoseDecisionViewCollection; dispid 12755;
    function  GetDiagramSynchronizationViews: IRoseSyncItemViewCollection; dispid 12756;
    function  RemoveSynchronizationView(const theSyncItemView: IRoseSyncItemView): WordBool; dispid 12757;
    function  AddRelationView(const theRelation: IRoseRelation): WordBool; dispid 12787;
    function  AddSwimLaneView(const aSwimLane: IRoseSwimLane): IRoseSwimLaneView; dispid 12791;
    function  GetSwimLaneViews(const aSwimLane: IRoseSwimLane): IRoseSwimLaneViewCollection; dispid 12792;
    function  GetSelectedSwimLaneViews: IRoseSwimLaneViewCollection; dispid 12793;
    function  GetDiagramSwimLaneViews: IRoseSwimLaneViewCollection; dispid 12794;
    function  RemoveSwimLaneView(const aSwimLaneView: IRoseSwimLaneView): WordBool; dispid 12795;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetParentContext: IRoseItem; dispid 12823;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseEvent = dispinterface
    ['{A69CAB22-9179-11D0-A214-00A024FFFE40}']
    property Arguments: WideString dispid 215;
    property Name: WideString dispid 216;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property GuardCondition: WideString dispid 12622;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetQualifiedName: WideString; dispid 12555;
    function  GetAction: IRoseAction; dispid 12634;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseRichType = dispinterface
    ['{EB7AAB60-939C-11CF-B091-00A0241E3F73}']
    property Value: Smallint dispid 202;
    property Name: WideString dispid 203;
    property Types: IRoseRichTypeValuesCollection dispid 204;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;




  IRoseScenarioDiagramCollection = dispinterface
    ['{97B3835E-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseScenarioDiagram; dispid 203;
    function  Exists(const pObject: IRoseScenarioDiagram): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseScenarioDiagram): Smallint; dispid 207;
    procedure Add(const theObject: IRoseScenarioDiagram); dispid 208;
    procedure AddCollection(const theCollection: IRoseScenarioDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseScenarioDiagram); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseScenarioDiagram; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseScenarioDiagram; dispid 213;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  AddParameter(const theName: WideString; const theType: WideString; 
                           const theDef: WideString; position: Smallint): IRoseParameter; dispid 416;
    procedure RemoveAllParameters; dispid 417;
    function  DeleteParameter(const theParameter: IRoseParameter): WordBool; dispid 426;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseView_LineColor = dispinterface
    ['{CE5BE565-0380-11D1-BC11-00A024C67143}']
    property Blue: Smallint dispid 12502;
    property Green: Smallint dispid 12503;
    property Red: Smallint dispid 12504;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;




  IRoseAddInManager = dispinterface
    ['{D5352FC2-346C-11D1-883B-3C8B00C10000}']
    property AddIns: IRoseAddInCollection dispid 12529;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  DisableEvents(theEvents: Integer): Integer; dispid 12692;
    function  EnableEvents(theEvents: Integer): Integer; dispid 12693;
  end;




  IRoseStateDiagramCollection = dispinterface
    ['{97B38368-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseStateDiagram; dispid 203;
    function  Exists(const pObject: IRoseStateDiagram): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseStateDiagram): Smallint; dispid 207;
    procedure Add(const theObject: IRoseStateDiagram); dispid 208;
    procedure AddCollection(const theCollection: IRoseStateDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseStateDiagram); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseStateDiagram; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseStateDiagram; dispid 213;
  end;




  IRoseSwimLaneViewCollection = dispinterface
    ['{7FFC5F46-C0C2-11D2-92AA-004005141253}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseSwimLaneView; dispid 203;
    function  Exists(const pObject: IRoseSwimLaneView): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseSwimLaneView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseSwimLaneView); dispid 208;
    procedure AddCollection(const theCollection: IRoseSwimLaneViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseSwimLaneView); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseSwimLaneView; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseSwimLaneView; dispid 213;
  end;




  IRoseSwimLane = dispinterface
    ['{BEAED5EA-578D-11D2-92AA-004005141253}']
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  IsClass: WordBool; dispid 414;
    function  GetClass: IRoseClass; dispid 415;
    function  AddLink(const Name: WideString; const ToInstance: IRoseObjectInstance): IRoseLink; dispid 417;
    function  DeleteLink(const aLink: IRoseLink): WordBool; dispid 418;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  GetStateVertices: IRoseStateVertexCollection; dispid 12712;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseItemViewCollection = dispinterface
    ['{97B38362-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseItemView; dispid 203;
    function  Exists(const pObject: IRoseItemView): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseItemView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseItemView); dispid 208;
    procedure AddCollection(const theCollection: IRoseItemViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseItemView); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseItemView; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseItemView; dispid 213;
  end;




  IRosePropertyCollection = dispinterface
    ['{97B3835D-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseProperty; dispid 203;
    function  Exists(const pObject: IRoseProperty): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseProperty): Smallint; dispid 207;
    procedure Add(const theObject: IRoseProperty); dispid 208;
    procedure AddCollection(const theCollection: IRosePropertyCollection); dispid 209;
    procedure Remove(const theObject: IRoseProperty); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseProperty; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseProperty; dispid 213;
  end;




  IRoseOperationCollection = dispinterface
    ['{97B3834D-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseOperation; dispid 203;
    function  Exists(const pObject: IRoseOperation): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseOperation): Smallint; dispid 207;
    procedure Add(const theObject: IRoseOperation); dispid 208;
    procedure AddCollection(const theCollection: IRoseOperationCollection); dispid 209;
    procedure Remove(const theObject: IRoseOperation); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseOperation; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseOperation; dispid 213;
  end;




  IRoseDeviceCollection = dispinterface
    ['{97B38342-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseDevice; dispid 203;
    function  Exists(const pObject: IRoseDevice): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseDevice): Smallint; dispid 207;
    procedure Add(const theObject: IRoseDevice); dispid 208;
    procedure AddCollection(const theCollection: IRoseDeviceCollection); dispid 209;
    procedure Remove(const theObject: IRoseDevice); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseDevice; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseDevice; dispid 213;
  end;




  IRoseInstantiateRelation = dispinterface
    ['{B91D8F03-DDBB-11D1-9FAD-0060975306FE}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property SupplierName: WideString dispid 412;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  GetContextClass: IRoseClass; dispid 12600;
    function  GetSupplierClass: IRoseClass; dispid 12601;
    function  HasClient: WordBool; dispid 12606;
    function  HasSupplier: WordBool; dispid 12607;
    function  GetClient: IRoseItem; dispid 12608;
    function  GetSupplier: IRoseItem; dispid 12609;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseContextMenuItem = dispinterface
    ['{EE0B16E0-FF91-11D1-9FAD-0060975306FE}']
    property Caption: WideString dispid 12682;
    property internalName: WideString dispid 12683;
    property MenuID: Smallint dispid 12686;
    property MenuState: Smallint dispid 12687;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;




  IRoseLineVertex = dispinterface
    ['{B53888D2-3094-11D2-8153-00104B97EBD5}']
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  GetXPosition: Smallint; dispid 12694;
    function  GetYPosition: Smallint; dispid 12695;
  end;




  IRoseObject = dispinterface
    ['{7D8474B2-2C33-11D0-BBDA-00A024C67143}']
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;




  IRoseSwimLaneCollection = dispinterface
    ['{7FFC5F42-C0C2-11D2-92AA-004005141253}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseSwimLane; dispid 203;
    function  Exists(const pObject: IRoseSwimLane): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseSwimLane): Smallint; dispid 207;
    procedure Add(const theObject: IRoseSwimLane); dispid 208;
    procedure AddCollection(const theCollection: IRoseSwimLaneCollection); dispid 209;
    procedure Remove(const theObject: IRoseSwimLane); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseSwimLane; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseSwimLane; dispid 213;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  HasClient: WordBool; dispid 12606;
    function  HasSupplier: WordBool; dispid 12607;
    function  GetClient: IRoseItem; dispid 12608;
    function  GetSupplier: IRoseItem; dispid 12609;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseComponentViewCollection = dispinterface
    ['{C640C861-F2D3-11D0-883A-3C8B00C10000}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseComponentView; dispid 203;
    function  Exists(const pObject: IRoseComponentView): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseComponentView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseComponentView); dispid 208;
    procedure AddCollection(const theCollection: IRoseComponentViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseComponentView); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseComponentView; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseComponentView; dispid 213;
  end;




  IRoseHasRelationshipCollection = dispinterface
    ['{97B38351-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseHasRelationship; dispid 203;
    function  Exists(const pObject: IRoseHasRelationship): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseHasRelationship): Smallint; dispid 207;
    procedure Add(const theObject: IRoseHasRelationship); dispid 208;
    procedure AddCollection(const theCollection: IRoseHasRelationshipCollection); dispid 209;
    procedure Remove(const theObject: IRoseHasRelationship); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseHasRelationship; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseHasRelationship; dispid 213;
  end;




  IRoseClassViewCollection = dispinterface
    ['{97B38341-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseClassView; dispid 203;
    function  Exists(const pObject: IRoseClassView): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseClassView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseClassView); dispid 208;
    procedure AddCollection(const theCollection: IRoseClassViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseClassView); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseClassView; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseClassView; dispid 213;
  end;




  IRoseDeploymentDiagram = dispinterface
    ['{C2C15EC4-E028-11CF-B091-00A0241E3F73}']
    property Name: WideString dispid 100;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Visible: WordBool dispid 203;
    property Items: IRoseItemCollection dispid 208;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property Documentation: WideString dispid 12656;
    property ZoomFactor: Smallint dispid 12690;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Invalidate; dispid 205;
    procedure Update; dispid 206;
    function  GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function  IsActive: WordBool; dispid 209;
    function  Exists(const theItem: IRoseItem): WordBool; dispid 210;
    procedure Activate; dispid 211;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    procedure Render(const FileName: WideString); dispid 217;
    function  AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function  RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function  GetNoteViews: IRoseNoteViewCollection; dispid 220;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function  GetProcessors: IRoseProcessorCollection; dispid 411;
    function  GetDevices: IRoseDeviceCollection; dispid 412;
    function  AddProcessor(const theProcessor: IRoseProcessor; x: Smallint; y: Smallint): IRoseItemView; dispid 413;
    function  AddDevice(const theDevice: IRoseDevice; x: Smallint; y: Smallint): IRoseItemView; dispid 414;
    function  RemoveProcessor(const theProcessor: IRoseProcessor): WordBool; dispid 415;
    function  RemoveDevice(const theDevice: IRoseDevice): WordBool; dispid 416;
    function  GetSelectedItems: IRoseItemCollection; dispid 12525;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  AddRelationView(const theRelation: IRoseRelation): WordBool; dispid 12787;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetParentContext: IRoseItem; dispid 12823;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function  SupportsFillColor: WordBool; dispid 210;
    function  SupportsLineColor: WordBool; dispid 211;
    function  IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function  PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function  GetDefaultWidth: Smallint; dispid 215;
    function  GetDefaultHeight: Smallint; dispid 216;
    function  GetMinWidth: Smallint; dispid 217;
    function  GetMinHeight: Smallint; dispid 218;
    function  HasItem: WordBool; dispid 222;
    function  HasParentView: WordBool; dispid 223;
    function  GetInstance: IRoseObjectInstance; dispid 409;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetMessages: IRoseMessageCollection; dispid 416;
    function  DeleteMessage(const TheMessage: IRoseMessage): WordBool; dispid 419;
    function  AssignAssociation(const TheAssoc: IRoseAssociation): WordBool; dispid 420;
    function  UnassignAssociation: WordBool; dispid 421;
    function  AddMessageTo(const Name: WideString; const ToInstance: IRoseObjectInstance; 
                           SequenceNumber: Smallint): IRoseMessage; dispid 422;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  GetAssociation: IRoseAssociation; dispid 12776;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  IsClass: WordBool; dispid 414;
    function  GetClass: IRoseClass; dispid 415;
    function  AddLink(const Name: WideString; const ToInstance: IRoseObjectInstance): IRoseLink; dispid 417;
    function  DeleteLink(const aLink: IRoseLink): WordBool; dispid 418;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  HasClient: WordBool; dispid 12606;
    function  HasSupplier: WordBool; dispid 12607;
    function  GetClient: IRoseItem; dispid 12608;
    function  GetSupplier: IRoseItem; dispid 12609;
    function  GetContextCategory: IRoseCategory; dispid 12657;
    function  GetSupplierCategory: IRoseCategory; dispid 12658;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  GetContextClass: IRoseClass; dispid 12600;
    function  GetSupplierClass: IRoseClass; dispid 12601;
    function  HasClient: WordBool; dispid 12606;
    function  HasSupplier: WordBool; dispid 12607;
    function  GetClient: IRoseItem; dispid 12608;
    function  GetSupplier: IRoseItem; dispid 12609;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
  end;




  IRoseStateMachine = dispinterface
    ['{A69CAB21-9179-11D0-A214-00A024FFFE40}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property ParentClass: IRoseClass dispid 215;
    property States: IRoseStateCollection dispid 222;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property SwimLanes: IRoseSwimLaneCollection dispid 12729;
    property Activities: IRoseActivityCollection dispid 12730;
    property AbstractStates: IRoseAbstractStateCollection dispid 12731;
    property Decisions: IRoseDecisionCollection dispid 12732;
    property StateVertices: IRoseStateVertexCollection dispid 12733;
    property Diagrams: IRoseStateDiagramCollection dispid 12734;
    property Synchronizations: IRoseSyncItemCollection dispid 12778;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  OpenSpecification: WordBool; dispid 211;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  AddState(const Name: WideString): IRoseState; dispid 217;
    function  DeleteState(const State: IRoseState): WordBool; dispid 218;
    function  RelocateState(const State: IRoseState): WordBool; dispid 219;
    function  GetAllStates: IRoseStateCollection; dispid 220;
    function  GetAllTransitions: IRoseTransitionCollection; dispid 221;
    function  GetQualifiedName: WideString; dispid 12555;
    function  GetTransitions: IRoseTransitionCollection; dispid 12665;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  GetAllActivities: IRoseActivityCollection; dispid 12711;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  GetAllAbstractStates: IRoseAbstractStateCollection; dispid 12735;
    function  GetAllDecisions: IRoseDecisionCollection; dispid 12736;
    function  GetAllStateVertices: IRoseStateVertexCollection; dispid 12737;
    function  AddActivity(const theName: WideString): IRoseActivity; dispid 12738;
    function  AddDecision(const theName: WideString): IRoseDecision; dispid 12739;
    function  GetAllDiagrams: IRoseStateDiagramCollection; dispid 12740;
    function  AddSwimLane(const theName: WideString): IRoseSwimLane; dispid 12779;
    function  DeleteSwimLane(const aSwimLane: IRoseSwimLane): WordBool; dispid 12780;
    function  DeleteStateVertex(const aStateVertex: IRoseStateVertex): WordBool; dispid 12781;
    function  GetAllSynchronizations: IRoseSyncItemCollection; dispid 12782;
    function  AddSynchronization(const aName: WideString): IRoseSyncItem; dispid 12784;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 12797;
    function  AddStateChartDiagram(const theName: WideString): IRoseStateDiagram; dispid 12800;
    function  AddActivityDiagram(const theName: WideString): IRoseStateDiagram; dispid 12801;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetAssignedClasses: IRoseClassCollection; dispid 418;
    function  AddVisibilityRelationship(const theModule: IRoseModule): IRoseModuleVisibilityRelationship; dispid 419;
    function  DeleteVisibilityRelationship(const theVisibilityRelationship: IRoseModuleVisibilityRelationship): WordBool; dispid 420;
    function  GetDependencies: IRoseModuleVisibilityRelationshipCollection; dispid 421;
    function  GetAllDependencies: IRoseModuleVisibilityRelationshipCollection; dispid 422;
    function  GetSubsystemDependencies(const theSubsystem: IRoseSubsystem): IRoseModuleVisibilityRelationshipCollection; dispid 423;
    function  AddSubsystemVisibilityRelation(const theSubsystem: IRoseSubsystem): IRoseModuleVisibilityRelationship; dispid 428;
    function  GetQualifiedName: WideString; dispid 12555;
    function  AddRealizeRel(const theRelationName: WideString; const theInterfaceName: WideString): IRoseRealizeRelation; dispid 12613;
    function  DeleteRealizeRel(const theRealizeRel: IRoseRealizeRelation): WordBool; dispid 12614;
    function  GetRealizeRelations: IRoseRealizeRelationCollection; dispid 12615;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  AddClassDiagram(const szName: WideString): IRoseClassDiagram; dispid 416;
    function  DeleteClassDiagram(const pIDispatch: IRoseClassDiagram): WordBool; dispid 417;
    function  DeleteScenarioDiagram(const pIDispatch: IRoseScenarioDiagram): WordBool; dispid 418;
    function  AddScenarioDiagram(const szName: WideString; iType: Smallint): IRoseScenarioDiagram; dispid 419;
    function  AddInheritRel(const szName: WideString; const szParentName: WideString): IRoseInheritRelation; dispid 421;
    function  DeleteInheritRel(const pIDispatchRelation: IRoseInheritRelation): WordBool; dispid 422;
    function  GetAssociations: IRoseAssociationCollection; dispid 426;
    function  AddAssociation(const szSupplierRoleName: WideString; 
                             const szSupplierRoleType: WideString): IRoseAssociation; dispid 430;
    function  DeleteAssociation(const pDispatchAssociation: IRoseAssociation): WordBool; dispid 431;
    function  GetSuperUseCases: IRoseUseCaseCollection; dispid 432;
    function  GetInheritRelations: IRoseInheritRelationCollection; dispid 433;
    function  GetRoles: IRoseRoleCollection; dispid 434;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  AddUseCaseDiagram(const szName: WideString): IRoseClassDiagram; dispid 12699;
    procedure CreateStateMachine; dispid 12700;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseItemCollection = dispinterface
    ['{0DD9ACF8-D06E-11D0-BC0B-00A024C67143}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseItem; dispid 203;
    function  Exists(const pObject: IRoseItem): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseItem): Smallint; dispid 207;
    procedure Add(const theObject: IRoseItem); dispid 208;
    procedure AddCollection(const theCollection: IRoseItemCollection); dispid 209;
    procedure Remove(const theObject: IRoseItem); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseItem; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseItem; dispid 213;
  end;




  IRoseNoteViewCollection = dispinterface
    ['{97B38358-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseNoteView; dispid 203;
    function  Exists(const pObject: IRoseNoteView): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseNoteView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseNoteView); dispid 208;
    procedure AddCollection(const theCollection: IRoseNoteViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseNoteView); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseNoteView; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseNoteView; dispid 213;
  end;




  IRoseInheritRelationCollection = dispinterface
    ['{97B38354-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseInheritRelation; dispid 203;
    function  Exists(const pObject: IRoseInheritRelation): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseInheritRelation): Smallint; dispid 207;
    procedure Add(const theObject: IRoseInheritRelation); dispid 208;
    procedure AddCollection(const theCollection: IRoseInheritRelationCollection); dispid 209;
    procedure Remove(const theObject: IRoseInheritRelation); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseInheritRelation; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseInheritRelation; dispid 213;
  end;




  IRoseDeploymentDiagramCollection = dispinterface
    ['{97B383A1-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseDeploymentDiagram; dispid 203;
    function  Exists(const pObject: IRoseDeploymentDiagram): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseDeploymentDiagram): Smallint; dispid 207;
    procedure Add(const theObject: IRoseDeploymentDiagram); dispid 208;
    procedure AddCollection(const theCollection: IRoseDeploymentDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseDeploymentDiagram); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseDeploymentDiagram; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseDeploymentDiagram; dispid 213;
  end;




  IRoseStringCollection = dispinterface
    ['{6A7FC311-C893-11D0-BC0B-00A024C67143}']
    property Count: Smallint dispid 50;
    function  GetAt(id: Smallint): WideString; dispid 51;
  end;




  IRoseStateViewCollection = dispinterface
    ['{97B3836A-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseStateView; dispid 203;
    function  Exists(const pObject: IRoseStateView): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseStateView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseStateView); dispid 208;
    procedure AddCollection(const theCollection: IRoseStateViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseStateView); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseStateView; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseStateView; dispid 213;
  end;




  IRoseDecisionView = dispinterface
    ['{BEAED5F9-578D-11D2-92AA-004005141253}']
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
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function  SupportsFillColor: WordBool; dispid 210;
    function  SupportsLineColor: WordBool; dispid 211;
    function  IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function  PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function  GetDefaultWidth: Smallint; dispid 215;
    function  GetDefaultHeight: Smallint; dispid 216;
    function  GetMinWidth: Smallint; dispid 217;
    function  GetMinHeight: Smallint; dispid 218;
    function  HasItem: WordBool; dispid 222;
    function  HasParentView: WordBool; dispid 223;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  GetDecision: IRoseDecision; dispid 12742;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseStateMachineOwner = dispinterface
    ['{94CA1882-5D13-11D2-92AA-004005141253}']
    property Name: WideString dispid 100;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property StateMachines: IRoseStateMachineCollection dispid 12744;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  CreateStateMachine(const theStateMachineName: WideString): IRoseStateMachine; dispid 12745;
    function  DeleteStateMachine(const theStateMachine: IRoseStateMachine): WordBool; dispid 12746;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseProcessCollection = dispinterface
    ['{97B38366-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseProcess; dispid 203;
    function  Exists(const pObject: IRoseProcess): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseProcess): Smallint; dispid 207;
    procedure Add(const theObject: IRoseProcess); dispid 208;
    procedure AddCollection(const theCollection: IRoseProcessCollection); dispid 209;
    procedure Remove(const theObject: IRoseProcess); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseProcess; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseProcess; dispid 213;
  end;




  IRoseAssociationCollection = dispinterface
    ['{97B3834E-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseAssociation; dispid 203;
    function  Exists(const pObject: IRoseAssociation): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseAssociation): Smallint; dispid 207;
    procedure Add(const theObject: IRoseAssociation); dispid 208;
    procedure AddCollection(const theCollection: IRoseAssociationCollection); dispid 209;
    procedure Remove(const theObject: IRoseAssociation); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseAssociation; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseAssociation; dispid 213;
  end;




  IRoseModuleDiagramCollection = dispinterface
    ['{97B38348-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseModuleDiagram; dispid 203;
    function  Exists(const pObject: IRoseModuleDiagram): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseModuleDiagram): Smallint; dispid 207;
    procedure Add(const theObject: IRoseModuleDiagram); dispid 208;
    procedure AddCollection(const theCollection: IRoseModuleDiagramCollection); dispid 209;
    procedure Remove(const theObject: IRoseModuleDiagram); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseModuleDiagram; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseModuleDiagram; dispid 213;
  end;




  IRoseDiagram = dispinterface
    ['{3FD9D000-93B0-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Visible: WordBool dispid 203;
    property Items: IRoseItemCollection dispid 208;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property Documentation: WideString dispid 12656;
    property ZoomFactor: Smallint dispid 12690;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Invalidate; dispid 205;
    procedure Update; dispid 206;
    function  GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function  IsActive: WordBool; dispid 209;
    function  Exists(const theItem: IRoseItem): WordBool; dispid 210;
    procedure Activate; dispid 211;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    procedure Render(const FileName: WideString); dispid 217;
    function  AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function  RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function  GetNoteViews: IRoseNoteViewCollection; dispid 220;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function  GetSelectedItems: IRoseItemCollection; dispid 12525;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  AddRelationView(const theRelation: IRoseRelation): WordBool; dispid 12787;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetParentContext: IRoseItem; dispid 12823;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseAbstractStateCollection = dispinterface
    ['{BEAED5EE-578D-11D2-92AA-004005141253}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseAbstractState; dispid 203;
    function  Exists(const pObject: IRoseAbstractState): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseAbstractState): Smallint; dispid 207;
    procedure Add(const theObject: IRoseAbstractState); dispid 208;
    procedure AddCollection(const theCollection: IRoseAbstractStateCollection); dispid 209;
    procedure Remove(const theObject: IRoseAbstractState); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseAbstractState; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseAbstractState; dispid 213;
  end;




  IRoseRichTypeValuesCollection = dispinterface
    ['{BF8C1040-96DD-11CF-B091-00A0241E3F73}']
    property Count: Smallint dispid 202;
    function  GetAt(id: Smallint): WideString; dispid 203;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
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
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function  SupportsFillColor: WordBool; dispid 210;
    function  SupportsLineColor: WordBool; dispid 211;
    function  IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function  PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function  GetDefaultWidth: Smallint; dispid 215;
    function  GetDefaultHeight: Smallint; dispid 216;
    function  GetMinWidth: Smallint; dispid 217;
    function  GetMinHeight: Smallint; dispid 218;
    function  HasItem: WordBool; dispid 222;
    function  HasParentView: WordBool; dispid 223;
    function  GetQualifiedName: WideString; dispid 12555;
    function  GetSubsystem: IRoseSubsystem; dispid 12592;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function  SupportsFillColor: WordBool; dispid 210;
    function  SupportsLineColor: WordBool; dispid 211;
    function  IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function  PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function  GetDefaultWidth: Smallint; dispid 215;
    function  GetDefaultHeight: Smallint; dispid 216;
    function  GetMinWidth: Smallint; dispid 217;
    function  GetMinHeight: Smallint; dispid 218;
    function  HasItem: WordBool; dispid 222;
    function  HasParentView: WordBool; dispid 223;
    function  GetQualifiedName: WideString; dispid 12555;
    function  GetComponent: IRoseModule; dispid 12585;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseClassDiagram = dispinterface
    ['{3FD9D002-93B0-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Visible: WordBool dispid 203;
    property Items: IRoseItemCollection dispid 208;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property ParentCategory: IRoseCategory dispid 411;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property Documentation: WideString dispid 12656;
    property ZoomFactor: Smallint dispid 12690;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Invalidate; dispid 205;
    procedure Update; dispid 206;
    function  GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function  IsActive: WordBool; dispid 209;
    function  Exists(const theItem: IRoseItem): WordBool; dispid 210;
    procedure Activate; dispid 211;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    procedure Render(const FileName: WideString); dispid 217;
    function  AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function  RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function  GetNoteViews: IRoseNoteViewCollection; dispid 220;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function  AddClass(const theClass: IRoseClass): WordBool; dispid 412;
    function  AddCategory(const theCat: IRoseCategory): WordBool; dispid 413;
    function  GetSelectedCategories: IRoseCategoryCollection; dispid 414;
    function  GetSelectedClasses: IRoseClassCollection; dispid 415;
    function  GetClasses: IRoseClassCollection; dispid 416;
    function  GetCategories: IRoseCategoryCollection; dispid 417;
    function  AddAssociation(const theAssociation: IRoseAssociation): WordBool; dispid 418;
    function  RemoveClass(const theClass: IRoseClass): WordBool; dispid 419;
    function  RemoveCategory(const theCategory: IRoseCategory): WordBool; dispid 420;
    function  RemoveAssociation(const theAssociation: IRoseAssociation): WordBool; dispid 421;
    function  GetAssociations: IRoseAssociationCollection; dispid 422;
    function  AddUseCase(const theUseCase: IRoseUseCase): WordBool; dispid 423;
    function  RemoveUseCase(const theUseCase: IRoseUseCase): WordBool; dispid 424;
    function  GetUseCases: IRoseUseCaseCollection; dispid 425;
    function  IsUseCaseDiagram: WordBool; dispid 426;
    function  GetClassView(const theClass: IRoseClass): IRoseClassView; dispid 427;
    function  GetSelectedItems: IRoseItemCollection; dispid 12525;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  AddRelationView(const theRelation: IRoseRelation): WordBool; dispid 12787;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetParentContext: IRoseItem; dispid 12823;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function  SupportsFillColor: WordBool; dispid 210;
    function  SupportsLineColor: WordBool; dispid 211;
    function  IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function  PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function  GetDefaultWidth: Smallint; dispid 215;
    function  GetDefaultHeight: Smallint; dispid 216;
    function  GetMinWidth: Smallint; dispid 217;
    function  GetMinHeight: Smallint; dispid 218;
    function  HasItem: WordBool; dispid 222;
    function  HasParentView: WordBool; dispid 223;
    function  GetNoteViewType: Smallint; dispid 424;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetDiagramLink: IRoseDiagram; dispid 12821;
    function  AddAttachmentToView(const theItemView: IRoseItemView; const theLabelText: WideString): IRoseItemView; dispid 12822;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  IsRootPackage: WordBool; dispid 621;
    function  IsControlled: WordBool; dispid 12433;
    function  Control(const Path: WideString): WordBool; dispid 12434;
    function  IsLoaded: WordBool; dispid 12435;
    function  Load: WordBool; dispid 12436;
    function  IsModifiable: WordBool; dispid 12438;
    function  Unload: WordBool; dispid 12439;
    function  Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function  GetFileName: WideString; dispid 12441;
    function  Save: WordBool; dispid 12442;
    function  SaveAs(const Path: WideString): WordBool; dispid 12443;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IsModified: WordBool; dispid 12654;
    function  Uncontrol: WordBool; dispid 12655;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    procedure Refresh; dispid 12701;
    function  GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function  IsLocked: WordBool; dispid 12703;
    function  NeedsRefreshing: WordBool; dispid 12704;
    function  GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    procedure Lock; dispid 12708;
    procedure Unlock; dispid 12709;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRosePathMap = dispinterface
    ['{4C9E2241-84C5-11D0-A214-444553540000}']
    function  DeleteEntry(const Symbol: WideString): WordBool; dispid 50;
    function  GetActualPath(const VirtualPath: WideString): WideString; dispid 51;
    function  GetVirtualPath(const ActualPath: WideString): WideString; dispid 52;
    function  HasEntry(const Symbol: WideString): WordBool; dispid 53;
    function  AddEntry(const Symbol: WideString; const Path: WideString; const Comment: WideString): WordBool; dispid 54;
    function  GetActualPathWithContext(const VirtualPath: WideString; const Context: WideString): WideString; dispid 12674;
    function  GetVirtualPathWithContext(const ActualPath: WideString; const Context: WideString): WideString; dispid 12675;
  end;




  IRoseSyncItemViewCollection = dispinterface
    ['{94CA1891-5D13-11D2-92AA-004005141253}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseSyncItemView; dispid 203;
    function  Exists(const pObject: IRoseSyncItemView): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseSyncItemView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseSyncItemView); dispid 208;
    procedure AddCollection(const theCollection: IRoseSyncItemViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseSyncItemView); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseSyncItemView; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseSyncItemView; dispid 213;
  end;




  IRoseAbstractState = dispinterface
    ['{BEAED5EC-578D-11D2-92AA-004005141253}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property ParentStateMachine: IRoseStateMachine dispid 413;
    property Transitions: IRoseTransitionCollection dispid 422;
    property SubStates: IRoseStateCollection dispid 444;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property Parent: IRoseStateVertex dispid 12747;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property SubActivities: IRoseActivityCollection dispid 12802;
    property SubDecisions: IRoseDecisionCollection dispid 12803;
    property SubSynchronizations: IRoseSyncItemCollection dispid 12804;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetAllSubStates: IRoseStateCollection; dispid 425;
    function  AddTransition(const OnEvent: WideString; const Target: IRoseState): IRoseTransition; dispid 426;
    function  DeleteTransition(const Transition: IRoseTransition): WordBool; dispid 427;
    function  DeleteAction(const theAction: IRoseAction): WordBool; dispid 446;
    function  GetQualifiedName: WideString; dispid 12555;
    function  GetUserDefinedEvents: IRoseEventCollection; dispid 12623;
    function  GetEntryActions: IRoseActionCollection; dispid 12625;
    function  GetExitActions: IRoseActionCollection; dispid 12626;
    function  GetDoActions: IRoseActionCollection; dispid 12627;
    function  AddUserDefinedEvent(const EventName: WideString; const ActionName: WideString): IRoseEvent; dispid 12635;
    function  DeleteUserDefinedEvent(const theEvent: IRoseEvent): WordBool; dispid 12636;
    function  AddEntryAction(const ActionName: WideString): IRoseAction; dispid 12637;
    function  AddExitAction(const ActionName: WideString): IRoseAction; dispid 12638;
    function  AddDoAction(const ActionName: WideString): IRoseAction; dispid 12639;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  GetSwimLanes: IRoseSwimLaneCollection; dispid 12748;
    function  GetActions: IRoseActionCollection; dispid 12767;
    function  GetStateMachines: IRoseStateMachineCollection; dispid 12768;
    function  AddStateMachine(const theName: WideString): IRoseStateMachine; dispid 12774;
    function  DeleteStateMachine(const theStateMachine: IRoseStateMachine): WordBool; dispid 12775;
    function  GetAllSubActivities: IRoseActivityCollection; dispid 12805;
    function  GetAllSubDecisions: IRoseDecisionCollection; dispid 12806;
    function  GetAllSubSynchronizations: IRoseSyncItemCollection; dispid 12807;
    function  AddTransitionToVertex(const OnEvent: WideString; const Target: IRoseStateVertex): IRoseTransition; dispid 12814;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseModuleVisibilityRelationshipCollection = dispinterface
    ['{97B38363-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseModuleVisibilityRelationship; dispid 203;
    function  Exists(const pObject: IRoseModuleVisibilityRelationship): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseModuleVisibilityRelationship): Smallint; dispid 207;
    procedure Add(const theObject: IRoseModuleVisibilityRelationship); dispid 208;
    procedure AddCollection(const theCollection: IRoseModuleVisibilityRelationshipCollection); dispid 209;
    procedure Remove(const theObject: IRoseModuleVisibilityRelationship); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseModuleVisibilityRelationship; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseModuleVisibilityRelationship; dispid 213;
  end;




  IRoseClassCollection = dispinterface
    ['{97B38349-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseClass; dispid 203;
    function  Exists(const pObject: IRoseClass): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseClass): Smallint; dispid 207;
    procedure Add(const theObject: IRoseClass); dispid 208;
    procedure AddCollection(const theCollection: IRoseClassCollection); dispid 209;
    procedure Remove(const theObject: IRoseClass); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseClass; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseClass; dispid 213;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetSenderObject: IRoseObjectInstance; dispid 412;
    function  GetReceiverObject: IRoseObjectInstance; dispid 413;
    function  IsMessageToSelf: WordBool; dispid 414;
    function  IsOperation: WordBool; dispid 415;
    function  GetOperation: IRoseOperation; dispid 416;
    function  GetLink: IRoseLink; dispid 417;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
    function  GetSequenceInformation: WideString; dispid 12825;
  end;




  IRoseConnectionRelationCollection = dispinterface
    ['{4467F446-F24E-11D2-92AA-004005141253}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseConnectionRelation; dispid 203;
    function  Exists(const pObject: IRoseConnectionRelation): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseConnectionRelation): Smallint; dispid 207;
    procedure Add(const theObject: IRoseConnectionRelation); dispid 208;
    procedure AddCollection(const theCollection: IRoseConnectionRelationCollection); dispid 209;
    procedure Remove(const theObject: IRoseConnectionRelation); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseConnectionRelation; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseConnectionRelation; dispid 213;
  end;




  IRoseClassDependencyCollection = dispinterface
    ['{ED042E4F-6CDE-11D1-BC1E-00A024C67143}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseClassDependency; dispid 203;
    function  Exists(const pObject: IRoseClassDependency): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IDispatch): Smallint; dispid 207;
    procedure Add(const theObject: IRoseClassDependency); dispid 208;
    procedure AddCollection(const theCollection: IRoseClassDependencyCollection); dispid 209;
    procedure Remove(const theObject: IRoseClassDependency); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseClassDependency; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseClassDependency; dispid 213;
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
    property Constraints: WideString dispid 12706;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property ParentCategory: IRoseCategory dispid 12798;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetCorrespondingRole(const Class_: IRoseClass): IRoseRole; dispid 416;
    function  GetOtherRole(const Class_: IRoseClass): IRoseRole; dispid 417;
    procedure SetLinkClassName(const theClassName: WideString); dispid 418;
    function  GetQualifiedName: WideString; dispid 12555;
    function  NameIsDirectional: WordBool; dispid 12646;
    function  GetRoleForNameDirection: IRoseRole; dispid 12647;
    procedure SetRoleForNameDirection(const theRole: IRoseRole); dispid 12648;
    procedure ClearRoleForNameDirection; dispid 12649;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseEventCollection = dispinterface
    ['{97B38361-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseEvent; dispid 203;
    function  Exists(const pObject: IRoseEvent): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseEvent): Smallint; dispid 207;
    procedure Add(const theObject: IRoseEvent); dispid 208;
    procedure AddCollection(const theCollection: IRoseEventCollection); dispid 209;
    procedure Remove(const theObject: IRoseEvent); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseEvent; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseEvent; dispid 213;
  end;




  IRoseStateCollection = dispinterface
    ['{97B38367-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseState; dispid 203;
    function  Exists(const pObject: IRoseState): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseState): Smallint; dispid 207;
    procedure Add(const theObject: IRoseState); dispid 208;
    procedure AddCollection(const theCollection: IRoseStateCollection); dispid 209;
    procedure Remove(const theObject: IRoseState); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseState; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseState; dispid 213;
  end;




  IRoseSyncItemView = dispinterface
    ['{94CA1888-5D13-11D2-92AA-004005141253}']
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
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    property Horizontal: WordBool dispid 12789;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function  SupportsFillColor: WordBool; dispid 210;
    function  SupportsLineColor: WordBool; dispid 211;
    function  IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function  PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function  GetDefaultWidth: Smallint; dispid 215;
    function  GetDefaultHeight: Smallint; dispid 216;
    function  GetMinWidth: Smallint; dispid 217;
    function  GetMinHeight: Smallint; dispid 218;
    function  HasItem: WordBool; dispid 222;
    function  HasParentView: WordBool; dispid 223;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  GetSynchronization: IRoseSyncItem; dispid 12758;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseActivityView = dispinterface
    ['{BEAED5FC-578D-11D2-92AA-004005141253}']
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
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function  SupportsFillColor: WordBool; dispid 210;
    function  SupportsLineColor: WordBool; dispid 211;
    function  IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function  PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function  GetDefaultWidth: Smallint; dispid 215;
    function  GetDefaultHeight: Smallint; dispid 216;
    function  GetMinWidth: Smallint; dispid 217;
    function  GetMinHeight: Smallint; dispid 218;
    function  HasItem: WordBool; dispid 222;
    function  HasParentView: WordBool; dispid 223;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  GetActivity: IRoseActivity; dispid 12741;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function  SupportsFillColor: WordBool; dispid 210;
    function  SupportsLineColor: WordBool; dispid 211;
    function  IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function  PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function  GetDefaultWidth: Smallint; dispid 215;
    function  GetDefaultHeight: Smallint; dispid 216;
    function  GetMinWidth: Smallint; dispid 217;
    function  GetMinHeight: Smallint; dispid 218;
    function  HasItem: WordBool; dispid 222;
    function  HasParentView: WordBool; dispid 223;
    function  GetState: IRoseState; dispid 415;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseStateVertexCollection = dispinterface
    ['{BEAED5F7-578D-11D2-92AA-004005141253}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseStateVertex; dispid 203;
    function  Exists(const pObject: IRoseStateVertex): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseStateVertex): Smallint; dispid 207;
    procedure Add(const theObject: IRoseStateVertex); dispid 208;
    procedure AddCollection(const theCollection: IRoseStateVertexCollection); dispid 209;
    procedure Remove(const theObject: IRoseStateVertex); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseStateVertex; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseStateVertex; dispid 213;
  end;




  IRoseSubsystemViewCollection = dispinterface
    ['{CA3AD902-BFCE-11D0-89F5-0020AFD6C181}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseSubsystemView; dispid 203;
    function  Exists(const pObject: IRoseSubsystemView): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseSubsystemView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseSubsystemView); dispid 208;
    procedure AddCollection(const theCollection: IRoseSubsystemViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseSubsystemView); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseSubsystemView; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseSubsystemView; dispid 213;
  end;




  IRoseModuleDiagram = dispinterface
    ['{3FD9D004-93B0-11CF-B3D4-00A0241DB1D0}']
    property Name: WideString dispid 100;
    property ItemViews: IRoseItemViewCollection dispid 202;
    property Visible: WordBool dispid 203;
    property Items: IRoseItemCollection dispid 208;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property ParentSubsystem: IRoseSubsystem dispid 411;
    property ComponentViews: IRoseComponentViewCollection dispid 422;
    property SubsystemViews: IRoseSubsystemViewCollection dispid 423;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property Documentation: WideString dispid 12656;
    property ZoomFactor: Smallint dispid 12690;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Layout; dispid 204;
    procedure Invalidate; dispid 205;
    procedure Update; dispid 206;
    function  GetViewFrom(const theItem: IRoseItem): IRoseItemView; dispid 207;
    function  IsActive: WordBool; dispid 209;
    function  Exists(const theItem: IRoseItem): WordBool; dispid 210;
    procedure Activate; dispid 211;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    procedure Render(const FileName: WideString); dispid 217;
    function  AddNoteView(const szNoteText: WideString; nType: Smallint): IRoseNoteView; dispid 218;
    function  RemoveNoteView(const pIDispNoteView: IRoseNoteView): WordBool; dispid 219;
    function  GetNoteViews: IRoseNoteViewCollection; dispid 220;
    procedure RenderEnhanced(const FileName: WideString); dispid 221;
    procedure RenderToClipboard; dispid 222;
    procedure RenderEnhancedToClipboard; dispid 223;
    function  GetModules: IRoseModuleCollection; dispid 418;
    function  GetSubsystems: IRoseSubsystemCollection; dispid 419;
    function  GetSelectedModules: IRoseModuleCollection; dispid 420;
    function  GetSelectedSubsystems: IRoseSubsystemCollection; dispid 421;
    function  AddComponentView(const aModule: IRoseModule): IRoseComponentView; dispid 424;
    function  RemoveComponentView(const aComponentView: IRoseComponentView): WordBool; dispid 425;
    function  AddSubsystemView(const aSubsystem: IRoseSubsystem): IRoseSubsystemView; dispid 426;
    function  RemoveSubsystemView(const aSubsystemView: IDispatch): WordBool; dispid 427;
    function  GetSelectedItems: IRoseItemCollection; dispid 12525;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  AddRelationView(const theRelation: IRoseRelation): WordBool; dispid 12787;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetParentContext: IRoseItem; dispid 12823;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  AddModule(const theName: WideString): IRoseModule; dispid 416;
    function  AddModuleDiagram(const Name: WideString): IRoseModuleDiagram; dispid 418;
    function  AddSubsystem(const theName: WideString): IRoseSubsystem; dispid 419;
    procedure RelocateModule(const theModule: IRoseModule); dispid 420;
    procedure RelocateSubsystem(const theSubsystem: IRoseSubsystem); dispid 421;
    procedure RelocateModuleDiagram(const theModDiagram: IRoseModuleDiagram); dispid 422;
    function  GetAllModules: IRoseModuleCollection; dispid 423;
    function  GetAllSubsystems: IRoseSubsystemCollection; dispid 424;
    function  GetAssignedCategories: IRoseCategoryCollection; dispid 425;
    function  GetAssignedClasses: IRoseClassCollection; dispid 426;
    function  GetVisibleSubsystems: IRoseSubsystemCollection; dispid 427;
    function  GetSubsystemDependencies(const theSubsystem: IRoseSubsystem): IRoseModuleVisibilityRelationshipCollection; dispid 428;
    function  TopLevel: WordBool; dispid 429;
    function  AddVisibilityRelationship(const theModule: IRoseModule): IRoseModuleVisibilityRelationship; dispid 430;
    function  DeleteVisibilityRelationship(const theVisibilityRelationship: IRoseModuleVisibilityRelationship): WordBool; dispid 433;
    function  AddSubsystemVisibilityRelation(const theSubsystem: IDispatch): IDispatch; dispid 434;
    function  DeleteModule(const pIDispatch: IRoseModule): WordBool; dispid 449;
    function  DeleteSubSystem(const pIDispatch: IRoseSubsystem): WordBool; dispid 450;
    function  IsRootPackage: WordBool; dispid 621;
    function  IsControlled: WordBool; dispid 12433;
    function  Control(const Path: WideString): WordBool; dispid 12434;
    function  IsLoaded: WordBool; dispid 12435;
    function  Load: WordBool; dispid 12436;
    function  IsModifiable: WordBool; dispid 12438;
    function  Unload: WordBool; dispid 12439;
    function  Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function  GetFileName: WideString; dispid 12441;
    function  Save: WordBool; dispid 12442;
    function  SaveAs(const Path: WideString): WordBool; dispid 12443;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IsModified: WordBool; dispid 12654;
    function  Uncontrol: WordBool; dispid 12655;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    procedure Refresh; dispid 12701;
    function  GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function  IsLocked: WordBool; dispid 12703;
    function  NeedsRefreshing: WordBool; dispid 12704;
    function  GetVisibilityRelations: IRoseModuleVisibilityRelationshipCollection; dispid 12705;
    function  GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    procedure Lock; dispid 12708;
    procedure Unlock; dispid 12709;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseExternalDocumentCollection = dispinterface
    ['{97B38357-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseExternalDocument; dispid 203;
    function  Exists(const pObject: IRoseExternalDocument): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseExternalDocument): Smallint; dispid 207;
    procedure Add(const theObject: IRoseExternalDocument); dispid 208;
    procedure AddCollection(const theCollection: IRoseExternalDocumentCollection); dispid 209;
    procedure Remove(const theObject: IRoseExternalDocument); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseExternalDocument; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseExternalDocument; dispid 213;
  end;




  IRoseInstanceViewCollection = dispinterface
    ['{C640C864-F2D3-11D0-883A-3C8B00C10000}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseInstanceView; dispid 203;
    function  Exists(const pObject: IRoseInstanceView): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseInstanceView): Smallint; dispid 207;
    procedure Add(const theObject: IRoseInstanceView); dispid 208;
    procedure AddCollection(const theCollection: IRoseInstanceViewCollection); dispid 209;
    procedure Remove(const theObject: IRoseInstanceView); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseInstanceView; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseInstanceView; dispid 213;
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
    property LineVertices: IRoseLineVertexCollection dispid 12696;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    procedure Invalidate; dispid 207;
    function  SupportsFillColor: WordBool; dispid 210;
    function  SupportsLineColor: WordBool; dispid 211;
    function  IsSelected: WordBool; dispid 212;
    procedure SetSelected(bSelect: WordBool); dispid 213;
    function  PointInView(x: Smallint; y: Smallint): WordBool; dispid 214;
    function  GetDefaultWidth: Smallint; dispid 215;
    function  GetDefaultHeight: Smallint; dispid 216;
    function  GetMinWidth: Smallint; dispid 217;
    function  GetMinHeight: Smallint; dispid 218;
    function  HasItem: WordBool; dispid 222;
    function  HasParentView: WordBool; dispid 223;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseTransitionCollection = dispinterface
    ['{97B3836B-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseTransition; dispid 203;
    function  Exists(const pObject: IRoseTransition): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseTransition): Smallint; dispid 207;
    procedure Add(const theObject: IRoseTransition); dispid 208;
    procedure AddCollection(const theCollection: IRoseTransitionCollection); dispid 209;
    procedure Remove(const theObject: IRoseTransition); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseTransition; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseTransition; dispid 213;
  end;




  IRoseState = dispinterface
    ['{A69CAB23-9179-11D0-A214-00A024FFFE40}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property ParentStateMachine: IRoseStateMachine dispid 413;
    property History: WordBool dispid 421;
    property Transitions: IRoseTransitionCollection dispid 422;
    property StateKind: IDispatch dispid 423;
    property SubStates: IRoseStateCollection dispid 444;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property Parent: IRoseStateVertex dispid 12747;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    property SubActivities: IRoseActivityCollection dispid 12802;
    property SubDecisions: IRoseDecisionCollection dispid 12803;
    property SubSynchronizations: IRoseSyncItemCollection dispid 12804;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetAllSubStates: IRoseStateCollection; dispid 425;
    function  AddTransition(const OnEvent: WideString; const Target: IRoseState): IRoseTransition; dispid 426;
    function  DeleteTransition(const Transition: IRoseTransition): WordBool; dispid 427;
    function  AddState(const Name: WideString): IRoseState; dispid 428;
    function  DeleteState(const State: IRoseState): WordBool; dispid 429;
    function  RelocateState(const State: IRoseState): WordBool; dispid 430;
    function  DeleteAction(const theAction: IRoseAction): WordBool; dispid 446;
    function  GetQualifiedName: WideString; dispid 12555;
    function  GetUserDefinedEvents: IRoseEventCollection; dispid 12623;
    function  GetAction(const theEvent: IRoseEvent): IRoseAction; dispid 12624;
    function  GetEntryActions: IRoseActionCollection; dispid 12625;
    function  GetExitActions: IRoseActionCollection; dispid 12626;
    function  GetDoActions: IRoseActionCollection; dispid 12627;
    function  AddUserDefinedEvent(const EventName: WideString; const ActionName: WideString): IRoseEvent; dispid 12635;
    function  DeleteUserDefinedEvent(const theEvent: IRoseEvent): WordBool; dispid 12636;
    function  AddEntryAction(const ActionName: WideString): IRoseAction; dispid 12637;
    function  AddExitAction(const ActionName: WideString): IRoseAction; dispid 12638;
    function  AddDoAction(const ActionName: WideString): IRoseAction; dispid 12639;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  GetSwimLanes: IRoseSwimLaneCollection; dispid 12748;
    function  GetActions: IRoseActionCollection; dispid 12767;
    function  GetStateMachines: IRoseStateMachineCollection; dispid 12768;
    function  AddStateMachine(const theName: WideString): IRoseStateMachine; dispid 12774;
    function  DeleteStateMachine(const theStateMachine: IRoseStateMachine): WordBool; dispid 12775;
    function  GetAllSubActivities: IRoseActivityCollection; dispid 12805;
    function  GetAllSubDecisions: IRoseDecisionCollection; dispid 12806;
    function  GetAllSubSynchronizations: IRoseSyncItemCollection; dispid 12807;
    function  AddTransitionToVertex(const OnEvent: WideString; const Target: IRoseStateVertex): IRoseTransition; dispid 12814;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseObjectInstanceCollection = dispinterface
    ['{97B3835A-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseObjectInstance; dispid 203;
    function  Exists(const pObject: IRoseObjectInstance): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseObjectInstance): Smallint; dispid 207;
    procedure Add(const theObject: IRoseObjectInstance); dispid 208;
    procedure AddCollection(const theCollection: IRoseObjectInstanceCollection); dispid 209;
    procedure Remove(const theObject: IRoseObjectInstance); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseObjectInstance; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseObjectInstance; dispid 213;
  end;




  IRoseRoleCollection = dispinterface
    ['{97B38353-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseRole; dispid 203;
    function  Exists(const pObject: IRoseRole): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseRole): Smallint; dispid 207;
    procedure Add(const theObject: IRoseRole); dispid 208;
    procedure AddCollection(const theCollection: IRoseRoleCollection); dispid 209;
    procedure Remove(const theObject: IRoseRole); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseRole; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseRole; dispid 213;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetQualifiedName: WideString; dispid 12555;
    function  GetContextClass: IRoseClass; dispid 12600;
    function  GetSupplierClass: IRoseClass; dispid 12601;
    function  HasClient: WordBool; dispid 12606;
    function  HasSupplier: WordBool; dispid 12607;
    function  GetClient: IRoseItem; dispid 12608;
    function  GetSupplier: IRoseItem; dispid 12609;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  GetAllClasses: IRoseClassCollection; dispid 419;
    function  GetAllCategories: IRoseCategoryCollection; dispid 420;
    function  AddClass(const theName: WideString): IRoseClass; dispid 421;
    function  AddClassDiagram(const Name: WideString): IRoseClassDiagram; dispid 422;
    function  AddCategory(const theName: WideString): IRoseCategory; dispid 423;
    procedure RelocateClass(const theClass: IRoseClass); dispid 424;
    procedure RelocateCategory(const theCategory: IRoseCategory); dispid 425;
    procedure RelocateClassDiagram(const theClsDiagram: IRoseClassDiagram); dispid 426;
    function  DeleteCategory(const theCategory: IRoseCategory): WordBool; dispid 427;
    function  DeleteClass(const theClass: IRoseClass): WordBool; dispid 428;
    function  DeleteClassDiagram(const theClassDiagram: IRoseClassDiagram): WordBool; dispid 429;
    function  AddScenarioDiagram(const Name: WideString; Type_: Smallint): IRoseScenarioDiagram; dispid 430;
    function  DeleteScenarioDiagram(const theScenarioDiagram: IRoseScenarioDiagram): WordBool; dispid 431;
    procedure RelocateScenarioDiagram(const theScenarioDiagram: IRoseScenarioDiagram); dispid 432;
    function  GetAssignedSubsystem: IRoseSubsystem; dispid 433;
    procedure SetAssignedSubsystem(const newValue: IRoseSubsystem); dispid 434;
    function  HasAssignedSubsystem: WordBool; dispid 435;
    function  DeleteUseCase(const theUseCase: IRoseUseCase): WordBool; dispid 436;
    function  TopLevel: WordBool; dispid 437;
    function  GetAllUseCases: IRoseUseCaseCollection; dispid 447;
    function  AddUseCase(const szName: WideString): IRoseUseCase; dispid 448;
    function  IsRootPackage: WordBool; dispid 621;
    function  IsControlled: WordBool; dispid 12433;
    function  Control(const Path: WideString): WordBool; dispid 12434;
    function  IsLoaded: WordBool; dispid 12435;
    function  Load: WordBool; dispid 12436;
    function  IsModifiable: WordBool; dispid 12438;
    function  Unload: WordBool; dispid 12439;
    function  Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function  GetFileName: WideString; dispid 12441;
    function  Save: WordBool; dispid 12442;
    function  SaveAs(const Path: WideString): WordBool; dispid 12443;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IsModified: WordBool; dispid 12654;
    function  Uncontrol: WordBool; dispid 12655;
    function  AddCategoryDependency(const theName: WideString; 
                                    const theSupplierCategoryName: WideString): IRoseCategoryDependency; dispid 12659;
    function  GetCategoryDependencies: IRoseCategoryDependencyCollection; dispid 12660;
    function  DeleteCategoryDependency(const theDependency: IRoseCategoryDependency): WordBool; dispid 12661;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    procedure RelocateAssociation(const theAssociation: IRoseAssociation); dispid 12677;
    procedure Refresh; dispid 12701;
    function  GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function  IsLocked: WordBool; dispid 12703;
    function  NeedsRefreshing: WordBool; dispid 12704;
    function  GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    procedure Lock; dispid 12708;
    procedure Unlock; dispid 12709;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
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
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  AddDefaultProperty(const ClassName: WideString; const ToolName: WideString; 
                                 const SetName: WideString; const PropName: WideString; 
                                 const PropType: WideString; const Value: WideString): WordBool; dispid 440;
    function  CloneDefaultPropertySet(const ClassName: WideString; const ToolName: WideString; 
                                      const ExistingSetName: WideString; 
                                      const NewSetName: WideString): WordBool; dispid 441;
    function  CreateDefaultPropertySet(const ClassName: WideString; const ToolName: WideString; 
                                       const NewSetName: WideString): WordBool; dispid 442;
    function  DeleteDefaultPropertySet(const ClassName: WideString; const ToolName: WideString; 
                                       const SetName: WideString): WordBool; dispid 443;
    function  GetDefaultPropertySet(const ClassName: WideString; const ToolName: WideString; 
                                    const SetName: WideString): IRosePropertyCollection; dispid 444;
    function  FindDefaultProperty(const ClassName: WideString; const ToolName: WideString; 
                                  const SetName: WideString; const PropName: WideString): IRoseProperty; dispid 445;
    function  GetDefaultSetNames(const ClassName: WideString; const ToolName: WideString): IRoseStringCollection; dispid 447;
    function  DeleteDefaultProperty(const ClassName: WideString; const ToolName: WideString; 
                                    const SetName: WideString; const PropName: WideString): WordBool; dispid 449;
    function  IsControlled: WordBool; dispid 12433;
    function  Control(const Path: WideString): WordBool; dispid 12434;
    function  IsLoaded: WordBool; dispid 12435;
    function  Load: WordBool; dispid 12436;
    function  IsModifiable: WordBool; dispid 12438;
    function  Unload: WordBool; dispid 12439;
    function  Modifiable(Modifiable: WordBool): WordBool; dispid 12440;
    function  GetFileName: WideString; dispid 12441;
    function  Save: WordBool; dispid 12442;
    function  SaveAs(const Path: WideString): WordBool; dispid 12443;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IsToolVisible(const theToolName: WideString): WordBool; dispid 12593;
    procedure SetToolVisibility(const theToolName: WideString; Visibility: WordBool); dispid 12594;
    function  IsModified: WordBool; dispid 12654;
    function  Uncontrol: WordBool; dispid 12655;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    procedure Refresh; dispid 12701;
    function  GetSubUnitItems: IRoseControllableUnitCollection; dispid 12702;
    function  IsLocked: WordBool; dispid 12703;
    function  NeedsRefreshing: WordBool; dispid 12704;
    function  GetAllSubUnitItems: IRoseControllableUnitCollection; dispid 12707;
    procedure Lock; dispid 12708;
    procedure Unlock; dispid 12709;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseStateMachineCollection = dispinterface
    ['{97B38369-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseStateMachine; dispid 203;
    function  Exists(const pObject: IRoseStateMachine): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseStateMachine): Smallint; dispid 207;
    procedure Add(const theObject: IRoseStateMachine); dispid 208;
    procedure AddCollection(const theCollection: IRoseStateMachineCollection); dispid 209;
    procedure Remove(const theObject: IRoseStateMachine); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseStateMachine; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseStateMachine; dispid 213;
  end;




  IRoseSyncItem = dispinterface
    ['{94CA188B-5D13-11D2-92AA-004005141253}']
    property Name: WideString dispid 100;
    property Documentation: WideString dispid 203;
    property Stereotype: WideString dispid 212;
    property ExternalDocuments: IRoseExternalDocumentCollection dispid 213;
    property ParentStateMachine: IRoseStateMachine dispid 413;
    property Transitions: IRoseTransitionCollection dispid 422;
    property Application: IDispatch dispid 12523;
    property Model: IRoseModel dispid 12524;
    property LocalizedStereotype: WideString dispid 12554;
    property Parent: IRoseStateVertex dispid 12747;
    property StateMachineOwner: IRoseStateMachineOwner dispid 12790;
    function  GetUniqueID: WideString; dispid 102;
    function  GetCurrentPropertySetName(const ToolName: WideString): WideString; dispid 109;
    function  OverrideProperty(const theToolName: WideString; const thePropName: WideString; 
                               const theValue: WideString): WordBool; dispid 110;
    function  InheritProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 111;
    function  GetPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 119;
    function  GetDefaultPropertyValue(const theToolName: WideString; const thePropName: WideString): WideString; dispid 120;
    function  FindProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 121;
    function  GetAllProperties: IRosePropertyCollection; dispid 122;
    function  GetToolProperties(const theToolName: WideString): IRosePropertyCollection; dispid 123;
    function  IsOverriddenProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 124;
    function  IsDefaultProperty(const theToolName: WideString; const thePropName: WideString): WordBool; dispid 125;
    function  FindDefaultProperty(const theToolName: WideString; const thePropName: WideString): IRoseProperty; dispid 126;
    function  CreateProperty(const theToolName: WideString; const thePropName: WideString; 
                             const theValue: WideString; const theType: WideString): WordBool; dispid 127;
    function  GetPropertyClassName: WideString; dispid 128;
    function  GetDefaultSetNames(const ToolName: WideString): IRoseStringCollection; dispid 129;
    function  GetToolNames: IRoseStringCollection; dispid 130;
    function  SetCurrentPropertySetName(const ToolName: WideString; const SetName: WideString): WordBool; dispid 131;
    function  GetRoseItem: IRoseItem; dispid 207;
    function  AddExternalDocument(const szName: WideString; iType: Smallint): IRoseExternalDocument; dispid 214;
    function  DeleteExternalDocument(const pIDispatch: IRoseExternalDocument): WordBool; dispid 215;
    function  OpenSpecification: WordBool; dispid 216;
    function  AddTransition(const OnEvent: WideString; const Target: IRoseState): IRoseTransition; dispid 426;
    function  DeleteTransition(const Transition: IRoseTransition): WordBool; dispid 427;
    function  GetQualifiedName: WideString; dispid 12555;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  OpenCustomSpecification: WordBool; dispid 12728;
    function  GetSwimLanes: IRoseSwimLaneCollection; dispid 12748;
    function  AddTransitionToVertex(const OnEvent: WideString; const Target: IRoseStateVertex): IRoseTransition; dispid 12814;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;




  IRoseRealizeRelationCollection = dispinterface
    ['{67448181-4553-11D1-883B-3C8B00C10000}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRoseRealizeRelation; dispid 203;
    function  Exists(const pObject: IRoseRealizeRelation): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRoseRealizeRelation): Smallint; dispid 207;
    procedure Add(const theObject: IRoseRealizeRelation); dispid 208;
    procedure AddCollection(const theCollection: IRoseRealizeRelationCollection); dispid 209;
    procedure Remove(const theObject: IRoseRealizeRelation); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRoseRealizeRelation; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRoseRealizeRelation; dispid 213;
  end;




  IRosePackageCollection = dispinterface
    ['{97B38364-A4E3-11D0-BFF0-00AA003DEF5B}']
    property Count: Smallint dispid 202;
    function  GetAt(Index: Smallint): IRosePackage; dispid 203;
    function  Exists(const pObject: IRosePackage): WordBool; dispid 204;
    function  FindFirst(const Name: WideString): Smallint; dispid 205;
    function  FindNext(iCurID: Smallint; const Name: WideString): Smallint; dispid 206;
    function  IndexOf(const theObject: IRosePackage): Smallint; dispid 207;
    procedure Add(const theObject: IRosePackage); dispid 208;
    procedure AddCollection(const theCollection: IRosePackageCollection); dispid 209;
    procedure Remove(const theObject: IRosePackage); dispid 210;
    procedure RemoveAll; dispid 211;
    function  GetFirst(const Name: WideString): IRosePackage; dispid 212;
    function  GetWithUniqueID(const UniqueID: WideString): IRosePackage; dispid 213;
  end;




  IRoseExternalDocument = dispinterface
    ['{906FF583-276B-11D0-8980-00A024774419}']
    property Path: WideString dispid 1;
    property URL: WideString dispid 2;
    property ParentCategory: IRoseCategory dispid 3;
    property ParentItem: IRoseItem dispid 12810;
    property ParentDiagram: IRoseDiagram dispid 12811;
    function  IsURL: WordBool; dispid 4;
    function  Open(const szAppPath: WideString): WordBool; dispid 5;
    function  IdentifyClass: WideString; dispid 12668;
    function  IsClass(const theClassName: WideString): WordBool; dispid 12669;
    function  RenderIconToClipboard: WordBool; dispid 12820;
    function  GetIconIndex: Smallint; dispid 12824;
  end;






  CoRoseActivityViewCollection = class
    class function Create: IRoseActivityViewCollection;
    class function CreateRemote(const MachineName: string): IRoseActivityViewCollection;
  end;






  CoRoseProcessorCollection = class
    class function Create: IRoseProcessorCollection;
    class function CreateRemote(const MachineName: string): IRoseProcessorCollection;
  end;






  CoRoseCategoryCollection = class
    class function Create: IRoseCategoryCollection;
    class function CreateRemote(const MachineName: string): IRoseCategoryCollection;
  end;






  CoRoseDeploymentUnit = class
    class function Create: IRoseDeploymentUnit;
    class function CreateRemote(const MachineName: string): IRoseDeploymentUnit;
  end;






  CoRoseItem = class
    class function Create: IRoseItem;
    class function CreateRemote(const MachineName: string): IRoseItem;
  end;






  CoRoseContextMenuItemCollection = class
    class function Create: IRoseContextMenuItemCollection;
    class function CreateRemote(const MachineName: string): IRoseContextMenuItemCollection;
  end;






  CoRoseAddIn = class
    class function Create: IRoseAddIn;
    class function CreateRemote(const MachineName: string): IRoseAddIn;
  end;






  CoRoseDecisionViewCollection = class
    class function Create: IRoseDecisionViewCollection;
    class function CreateRemote(const MachineName: string): IRoseDecisionViewCollection;
  end;






  CoRoseStateVertex = class
    class function Create: IRoseStateVertex;
    class function CreateRemote(const MachineName: string): IRoseStateVertex;
  end;






  CoRoseUseCaseCollection = class
    class function Create: IRoseUseCaseCollection;
    class function CreateRemote(const MachineName: string): IRoseUseCaseCollection;
  end;






  CoRoseConnectionRelation = class
    class function Create: IRoseConnectionRelation;
    class function CreateRemote(const MachineName: string): IRoseConnectionRelation;
  end;






  CoRoseRelation = class
    class function Create: IRoseRelation;
    class function CreateRemote(const MachineName: string): IRoseRelation;
  end;






  CoRoseApplication = class
    class function Create: IRoseApplication;
    class function CreateRemote(const MachineName: string): IRoseApplication;
  end;






  CoRoseDecisionCollection = class
    class function Create: IRoseDecisionCollection;
    class function CreateRemote(const MachineName: string): IRoseDecisionCollection;
  end;






  CoRoseDecision = class
    class function Create: IRoseDecision;
    class function CreateRemote(const MachineName: string): IRoseDecision;
  end;






  CoRoseLineVertexCollection = class
    class function Create: IRoseLineVertexCollection;
    class function CreateRemote(const MachineName: string): IRoseLineVertexCollection;
  end;






  CoRoseInstantiateRelationCollection = class
    class function Create: IRoseInstantiateRelationCollection;
    class function CreateRemote(const MachineName: string): IRoseInstantiateRelationCollection;
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






  CoRoseSyncItemCollection = class
    class function Create: IRoseSyncItemCollection;
    class function CreateRemote(const MachineName: string): IRoseSyncItemCollection;
  end;






  CoRoseActivityCollection = class
    class function Create: IRoseActivityCollection;
    class function CreateRemote(const MachineName: string): IRoseActivityCollection;
  end;






  CoRoseActivity = class
    class function Create: IRoseActivity;
    class function CreateRemote(const MachineName: string): IRoseActivity;
  end;






  CoRoseProcess = class
    class function Create: IRoseProcess;
    class function CreateRemote(const MachineName: string): IRoseProcess;
  end;






  CoRoseAddInCollection = class
    class function Create: IRoseAddInCollection;
    class function CreateRemote(const MachineName: string): IRoseAddInCollection;
  end;






  CoRoseSwimLaneView = class
    class function Create: IRoseSwimLaneView;
    class function CreateRemote(const MachineName: string): IRoseSwimLaneView;
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






  CoRoseSwimLaneViewCollection = class
    class function Create: IRoseSwimLaneViewCollection;
    class function CreateRemote(const MachineName: string): IRoseSwimLaneViewCollection;
  end;






  CoRoseSwimLane = class
    class function Create: IRoseSwimLane;
    class function CreateRemote(const MachineName: string): IRoseSwimLane;
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






  CoRoseInstantiateRelation = class
    class function Create: IRoseInstantiateRelation;
    class function CreateRemote(const MachineName: string): IRoseInstantiateRelation;
  end;






  CoRoseContextMenuItem = class
    class function Create: IRoseContextMenuItem;
    class function CreateRemote(const MachineName: string): IRoseContextMenuItem;
  end;






  CoRoseLineVertex = class
    class function Create: IRoseLineVertex;
    class function CreateRemote(const MachineName: string): IRoseLineVertex;
  end;






  CoRoseObject = class
    class function Create: IRoseObject;
    class function CreateRemote(const MachineName: string): IRoseObject;
  end;






  CoRoseSwimLaneCollection = class
    class function Create: IRoseSwimLaneCollection;
    class function CreateRemote(const MachineName: string): IRoseSwimLaneCollection;
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






  CoRoseDecisionView = class
    class function Create: IRoseDecisionView;
    class function CreateRemote(const MachineName: string): IRoseDecisionView;
  end;






  CoRoseStateMachineOwner = class
    class function Create: IRoseStateMachineOwner;
    class function CreateRemote(const MachineName: string): IRoseStateMachineOwner;
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






  CoRoseAbstractStateCollection = class
    class function Create: IRoseAbstractStateCollection;
    class function CreateRemote(const MachineName: string): IRoseAbstractStateCollection;
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






  CoRoseSyncItemViewCollection = class
    class function Create: IRoseSyncItemViewCollection;
    class function CreateRemote(const MachineName: string): IRoseSyncItemViewCollection;
  end;






  CoRoseAbstractState = class
    class function Create: IRoseAbstractState;
    class function CreateRemote(const MachineName: string): IRoseAbstractState;
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






  CoRoseConnectionRelationCollection = class
    class function Create: IRoseConnectionRelationCollection;
    class function CreateRemote(const MachineName: string): IRoseConnectionRelationCollection;
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






  CoRoseSyncItemView = class
    class function Create: IRoseSyncItemView;
    class function CreateRemote(const MachineName: string): IRoseSyncItemView;
  end;






  CoRoseActivityView = class
    class function Create: IRoseActivityView;
    class function CreateRemote(const MachineName: string): IRoseActivityView;
  end;






  CoRoseStateView = class
    class function Create: IRoseStateView;
    class function CreateRemote(const MachineName: string): IRoseStateView;
  end;






  CoRoseStateVertexCollection = class
    class function Create: IRoseStateVertexCollection;
    class function CreateRemote(const MachineName: string): IRoseStateVertexCollection;
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






  CoRoseStateMachineCollection = class
    class function Create: IRoseStateMachineCollection;
    class function CreateRemote(const MachineName: string): IRoseStateMachineCollection;
  end;






  CoRoseSyncItem = class
    class function Create: IRoseSyncItem;
    class function CreateRemote(const MachineName: string): IRoseSyncItem;
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

class function CoRoseActivityViewCollection.Create: IRoseActivityViewCollection;
begin
  Result := CreateComObject(CLASS_RoseActivityViewCollection) as IRoseActivityViewCollection;
end;

class function CoRoseActivityViewCollection.CreateRemote(const MachineName: string): IRoseActivityViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseActivityViewCollection) as IRoseActivityViewCollection;
end;

class function CoRoseProcessorCollection.Create: IRoseProcessorCollection;
begin
  Result := CreateComObject(CLASS_RoseProcessorCollection) as IRoseProcessorCollection;
end;

class function CoRoseProcessorCollection.CreateRemote(const MachineName: string): IRoseProcessorCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseProcessorCollection) as IRoseProcessorCollection;
end;

class function CoRoseCategoryCollection.Create: IRoseCategoryCollection;
begin
  Result := CreateComObject(CLASS_RoseCategoryCollection) as IRoseCategoryCollection;
end;

class function CoRoseCategoryCollection.CreateRemote(const MachineName: string): IRoseCategoryCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseCategoryCollection) as IRoseCategoryCollection;
end;

class function CoRoseDeploymentUnit.Create: IRoseDeploymentUnit;
begin
  Result := CreateComObject(CLASS_RoseDeploymentUnit) as IRoseDeploymentUnit;
end;

class function CoRoseDeploymentUnit.CreateRemote(const MachineName: string): IRoseDeploymentUnit;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDeploymentUnit) as IRoseDeploymentUnit;
end;

class function CoRoseItem.Create: IRoseItem;
begin
  Result := CreateComObject(CLASS_RoseItem) as IRoseItem;
end;

class function CoRoseItem.CreateRemote(const MachineName: string): IRoseItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseItem) as IRoseItem;
end;

class function CoRoseContextMenuItemCollection.Create: IRoseContextMenuItemCollection;
begin
  Result := CreateComObject(CLASS_RoseContextMenuItemCollection) as IRoseContextMenuItemCollection;
end;

class function CoRoseContextMenuItemCollection.CreateRemote(const MachineName: string): IRoseContextMenuItemCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseContextMenuItemCollection) as IRoseContextMenuItemCollection;
end;

class function CoRoseAddIn.Create: IRoseAddIn;
begin
  Result := CreateComObject(CLASS_RoseAddIn) as IRoseAddIn;
end;

class function CoRoseAddIn.CreateRemote(const MachineName: string): IRoseAddIn;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAddIn) as IRoseAddIn;
end;

class function CoRoseDecisionViewCollection.Create: IRoseDecisionViewCollection;
begin
  Result := CreateComObject(CLASS_RoseDecisionViewCollection) as IRoseDecisionViewCollection;
end;

class function CoRoseDecisionViewCollection.CreateRemote(const MachineName: string): IRoseDecisionViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDecisionViewCollection) as IRoseDecisionViewCollection;
end;

class function CoRoseStateVertex.Create: IRoseStateVertex;
begin
  Result := CreateComObject(CLASS_RoseStateVertex) as IRoseStateVertex;
end;

class function CoRoseStateVertex.CreateRemote(const MachineName: string): IRoseStateVertex;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateVertex) as IRoseStateVertex;
end;

class function CoRoseUseCaseCollection.Create: IRoseUseCaseCollection;
begin
  Result := CreateComObject(CLASS_RoseUseCaseCollection) as IRoseUseCaseCollection;
end;

class function CoRoseUseCaseCollection.CreateRemote(const MachineName: string): IRoseUseCaseCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseUseCaseCollection) as IRoseUseCaseCollection;
end;

class function CoRoseConnectionRelation.Create: IRoseConnectionRelation;
begin
  Result := CreateComObject(CLASS_RoseConnectionRelation) as IRoseConnectionRelation;
end;

class function CoRoseConnectionRelation.CreateRemote(const MachineName: string): IRoseConnectionRelation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseConnectionRelation) as IRoseConnectionRelation;
end;

class function CoRoseRelation.Create: IRoseRelation;
begin
  Result := CreateComObject(CLASS_RoseRelation) as IRoseRelation;
end;

class function CoRoseRelation.CreateRemote(const MachineName: string): IRoseRelation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRelation) as IRoseRelation;
end;

class function CoRoseApplication.Create: IRoseApplication;
begin
  Result := CreateComObject(CLASS_RoseApplication) as IRoseApplication;
end;

class function CoRoseApplication.CreateRemote(const MachineName: string): IRoseApplication;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseApplication) as IRoseApplication;
end;

class function CoRoseDecisionCollection.Create: IRoseDecisionCollection;
begin
  Result := CreateComObject(CLASS_RoseDecisionCollection) as IRoseDecisionCollection;
end;

class function CoRoseDecisionCollection.CreateRemote(const MachineName: string): IRoseDecisionCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDecisionCollection) as IRoseDecisionCollection;
end;

class function CoRoseDecision.Create: IRoseDecision;
begin
  Result := CreateComObject(CLASS_RoseDecision) as IRoseDecision;
end;

class function CoRoseDecision.CreateRemote(const MachineName: string): IRoseDecision;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDecision) as IRoseDecision;
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

class function CoRoseMessageCollection.Create: IRoseMessageCollection;
begin
  Result := CreateComObject(CLASS_RoseMessageCollection) as IRoseMessageCollection;
end;

class function CoRoseMessageCollection.CreateRemote(const MachineName: string): IRoseMessageCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseMessageCollection) as IRoseMessageCollection;
end;

class function CoRoseClassDiagramCollection.Create: IRoseClassDiagramCollection;
begin
  Result := CreateComObject(CLASS_RoseClassDiagramCollection) as IRoseClassDiagramCollection;
end;

class function CoRoseClassDiagramCollection.CreateRemote(const MachineName: string): IRoseClassDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassDiagramCollection) as IRoseClassDiagramCollection;
end;

class function CoRoseScenarioDiagram.Create: IRoseScenarioDiagram;
begin
  Result := CreateComObject(CLASS_RoseScenarioDiagram) as IRoseScenarioDiagram;
end;

class function CoRoseScenarioDiagram.CreateRemote(const MachineName: string): IRoseScenarioDiagram;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseScenarioDiagram) as IRoseScenarioDiagram;
end;

class function CoRoseRealizeRelation.Create: IRoseRealizeRelation;
begin
  Result := CreateComObject(CLASS_RoseRealizeRelation) as IRoseRealizeRelation;
end;

class function CoRoseRealizeRelation.CreateRemote(const MachineName: string): IRoseRealizeRelation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRealizeRelation) as IRoseRealizeRelation;
end;

class function CoRoseHasRelationship.Create: IRoseHasRelationship;
begin
  Result := CreateComObject(CLASS_RoseHasRelationship) as IRoseHasRelationship;
end;

class function CoRoseHasRelationship.CreateRemote(const MachineName: string): IRoseHasRelationship;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseHasRelationship) as IRoseHasRelationship;
end;

class function CoRoseClassView.Create: IRoseClassView;
begin
  Result := CreateComObject(CLASS_RoseClassView) as IRoseClassView;
end;

class function CoRoseClassView.CreateRemote(const MachineName: string): IRoseClassView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassView) as IRoseClassView;
end;

class function CoRoseView_FillColor.Create: IRoseView_FillColor;
begin
  Result := CreateComObject(CLASS_RoseView_FillColor) as IRoseView_FillColor;
end;

class function CoRoseView_FillColor.CreateRemote(const MachineName: string): IRoseView_FillColor;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseView_FillColor) as IRoseView_FillColor;
end;

class function CoRoseActionCollection.Create: IRoseActionCollection;
begin
  Result := CreateComObject(CLASS_RoseActionCollection) as IRoseActionCollection;
end;

class function CoRoseActionCollection.CreateRemote(const MachineName: string): IRoseActionCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseActionCollection) as IRoseActionCollection;
end;

class function CoRoseSyncItemCollection.Create: IRoseSyncItemCollection;
begin
  Result := CreateComObject(CLASS_RoseSyncItemCollection) as IRoseSyncItemCollection;
end;

class function CoRoseSyncItemCollection.CreateRemote(const MachineName: string): IRoseSyncItemCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSyncItemCollection) as IRoseSyncItemCollection;
end;

class function CoRoseActivityCollection.Create: IRoseActivityCollection;
begin
  Result := CreateComObject(CLASS_RoseActivityCollection) as IRoseActivityCollection;
end;

class function CoRoseActivityCollection.CreateRemote(const MachineName: string): IRoseActivityCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseActivityCollection) as IRoseActivityCollection;
end;

class function CoRoseActivity.Create: IRoseActivity;
begin
  Result := CreateComObject(CLASS_RoseActivity) as IRoseActivity;
end;

class function CoRoseActivity.CreateRemote(const MachineName: string): IRoseActivity;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseActivity) as IRoseActivity;
end;

class function CoRoseProcess.Create: IRoseProcess;
begin
  Result := CreateComObject(CLASS_RoseProcess) as IRoseProcess;
end;

class function CoRoseProcess.CreateRemote(const MachineName: string): IRoseProcess;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseProcess) as IRoseProcess;
end;

class function CoRoseAddInCollection.Create: IRoseAddInCollection;
begin
  Result := CreateComObject(CLASS_RoseAddInCollection) as IRoseAddInCollection;
end;

class function CoRoseAddInCollection.CreateRemote(const MachineName: string): IRoseAddInCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAddInCollection) as IRoseAddInCollection;
end;

class function CoRoseSwimLaneView.Create: IRoseSwimLaneView;
begin
  Result := CreateComObject(CLASS_RoseSwimLaneView) as IRoseSwimLaneView;
end;

class function CoRoseSwimLaneView.CreateRemote(const MachineName: string): IRoseSwimLaneView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSwimLaneView) as IRoseSwimLaneView;
end;

class function CoRoseControllableUnitCollection.Create: IRoseControllableUnitCollection;
begin
  Result := CreateComObject(CLASS_RoseControllableUnitCollection) as IRoseControllableUnitCollection;
end;

class function CoRoseControllableUnitCollection.CreateRemote(const MachineName: string): IRoseControllableUnitCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseControllableUnitCollection) as IRoseControllableUnitCollection;
end;

class function CoRoseModuleCollection.Create: IRoseModuleCollection;
begin
  Result := CreateComObject(CLASS_RoseModuleCollection) as IRoseModuleCollection;
end;

class function CoRoseModuleCollection.CreateRemote(const MachineName: string): IRoseModuleCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModuleCollection) as IRoseModuleCollection;
end;

class function CoRoseLinkCollection.Create: IRoseLinkCollection;
begin
  Result := CreateComObject(CLASS_RoseLinkCollection) as IRoseLinkCollection;
end;

class function CoRoseLinkCollection.CreateRemote(const MachineName: string): IRoseLinkCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseLinkCollection) as IRoseLinkCollection;
end;

class function CoRoseAction.Create: IRoseAction;
begin
  Result := CreateComObject(CLASS_RoseAction) as IRoseAction;
end;

class function CoRoseAction.CreateRemote(const MachineName: string): IRoseAction;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAction) as IRoseAction;
end;

class function CoRoseParameterCollection.Create: IRoseParameterCollection;
begin
  Result := CreateComObject(CLASS_RoseParameterCollection) as IRoseParameterCollection;
end;

class function CoRoseParameterCollection.CreateRemote(const MachineName: string): IRoseParameterCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseParameterCollection) as IRoseParameterCollection;
end;

class function CoRoseAttributeCollection.Create: IRoseAttributeCollection;
begin
  Result := CreateComObject(CLASS_RoseAttributeCollection) as IRoseAttributeCollection;
end;

class function CoRoseAttributeCollection.CreateRemote(const MachineName: string): IRoseAttributeCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAttributeCollection) as IRoseAttributeCollection;
end;

class function CoRoseDevice.Create: IRoseDevice;
begin
  Result := CreateComObject(CLASS_RoseDevice) as IRoseDevice;
end;

class function CoRoseDevice.CreateRemote(const MachineName: string): IRoseDevice;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDevice) as IRoseDevice;
end;

class function CoRoseClassDependency.Create: IRoseClassDependency;
begin
  Result := CreateComObject(CLASS_RoseClassDependency) as IRoseClassDependency;
end;

class function CoRoseClassDependency.CreateRemote(const MachineName: string): IRoseClassDependency;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassDependency) as IRoseClassDependency;
end;

class function CoRoseRole.Create: IRoseRole;
begin
  Result := CreateComObject(CLASS_RoseRole) as IRoseRole;
end;

class function CoRoseRole.CreateRemote(const MachineName: string): IRoseRole;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRole) as IRoseRole;
end;

class function CoRoseClass.Create: IRoseClass;
begin
  Result := CreateComObject(CLASS_RoseClass) as IRoseClass;
end;

class function CoRoseClass.CreateRemote(const MachineName: string): IRoseClass;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClass) as IRoseClass;
end;

class function CoRoseElement.Create: IRoseElement;
begin
  Result := CreateComObject(CLASS_RoseElement) as IRoseElement;
end;

class function CoRoseElement.CreateRemote(const MachineName: string): IRoseElement;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseElement) as IRoseElement;
end;

class function CoRoseControllableUnit.Create: IRoseControllableUnit;
begin
  Result := CreateComObject(CLASS_RoseControllableUnit) as IRoseControllableUnit;
end;

class function CoRoseControllableUnit.CreateRemote(const MachineName: string): IRoseControllableUnit;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseControllableUnit) as IRoseControllableUnit;
end;

class function CoRoseModel.Create: IRoseModel;
begin
  Result := CreateComObject(CLASS_RoseModel) as IRoseModel;
end;

class function CoRoseModel.CreateRemote(const MachineName: string): IRoseModel;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModel) as IRoseModel;
end;

class function CoRoseTransition.Create: IRoseTransition;
begin
  Result := CreateComObject(CLASS_RoseTransition) as IRoseTransition;
end;

class function CoRoseTransition.CreateRemote(const MachineName: string): IRoseTransition;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseTransition) as IRoseTransition;
end;

class function CoRoseSubsystemCollection.Create: IRoseSubsystemCollection;
begin
  Result := CreateComObject(CLASS_RoseSubsystemCollection) as IRoseSubsystemCollection;
end;

class function CoRoseSubsystemCollection.CreateRemote(const MachineName: string): IRoseSubsystemCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSubsystemCollection) as IRoseSubsystemCollection;
end;

class function CoRoseProcessor.Create: IRoseProcessor;
begin
  Result := CreateComObject(CLASS_RoseProcessor) as IRoseProcessor;
end;

class function CoRoseProcessor.CreateRemote(const MachineName: string): IRoseProcessor;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseProcessor) as IRoseProcessor;
end;

class function CoRoseCategoryDependencyCollection.Create: IRoseCategoryDependencyCollection;
begin
  Result := CreateComObject(CLASS_RoseCategoryDependencyCollection) as IRoseCategoryDependencyCollection;
end;

class function CoRoseCategoryDependencyCollection.CreateRemote(const MachineName: string): IRoseCategoryDependencyCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseCategoryDependencyCollection) as IRoseCategoryDependencyCollection;
end;

class function CoRoseProperty.Create: IRoseProperty;
begin
  Result := CreateComObject(CLASS_RoseProperty) as IRoseProperty;
end;

class function CoRoseProperty.CreateRemote(const MachineName: string): IRoseProperty;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseProperty) as IRoseProperty;
end;

class function CoRoseStateDiagram.Create: IRoseStateDiagram;
begin
  Result := CreateComObject(CLASS_RoseStateDiagram) as IRoseStateDiagram;
end;

class function CoRoseStateDiagram.CreateRemote(const MachineName: string): IRoseStateDiagram;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateDiagram) as IRoseStateDiagram;
end;

class function CoRoseEvent.Create: IRoseEvent;
begin
  Result := CreateComObject(CLASS_RoseEvent) as IRoseEvent;
end;

class function CoRoseEvent.CreateRemote(const MachineName: string): IRoseEvent;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseEvent) as IRoseEvent;
end;

class function CoRoseRichType.Create: IRoseRichType;
begin
  Result := CreateComObject(CLASS_RoseRichType) as IRoseRichType;
end;

class function CoRoseRichType.CreateRemote(const MachineName: string): IRoseRichType;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRichType) as IRoseRichType;
end;

class function CoRoseScenarioDiagramCollection.Create: IRoseScenarioDiagramCollection;
begin
  Result := CreateComObject(CLASS_RoseScenarioDiagramCollection) as IRoseScenarioDiagramCollection;
end;

class function CoRoseScenarioDiagramCollection.CreateRemote(const MachineName: string): IRoseScenarioDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseScenarioDiagramCollection) as IRoseScenarioDiagramCollection;
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

class function CoRoseView_LineColor.Create: IRoseView_LineColor;
begin
  Result := CreateComObject(CLASS_RoseView_LineColor) as IRoseView_LineColor;
end;

class function CoRoseView_LineColor.CreateRemote(const MachineName: string): IRoseView_LineColor;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseView_LineColor) as IRoseView_LineColor;
end;

class function CoRoseAddInManager.Create: IRoseAddInManager;
begin
  Result := CreateComObject(CLASS_RoseAddInManager) as IRoseAddInManager;
end;

class function CoRoseAddInManager.CreateRemote(const MachineName: string): IRoseAddInManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAddInManager) as IRoseAddInManager;
end;

class function CoRoseStateDiagramCollection.Create: IRoseStateDiagramCollection;
begin
  Result := CreateComObject(CLASS_RoseStateDiagramCollection) as IRoseStateDiagramCollection;
end;

class function CoRoseStateDiagramCollection.CreateRemote(const MachineName: string): IRoseStateDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateDiagramCollection) as IRoseStateDiagramCollection;
end;

class function CoRoseSwimLaneViewCollection.Create: IRoseSwimLaneViewCollection;
begin
  Result := CreateComObject(CLASS_RoseSwimLaneViewCollection) as IRoseSwimLaneViewCollection;
end;

class function CoRoseSwimLaneViewCollection.CreateRemote(const MachineName: string): IRoseSwimLaneViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSwimLaneViewCollection) as IRoseSwimLaneViewCollection;
end;

class function CoRoseSwimLane.Create: IRoseSwimLane;
begin
  Result := CreateComObject(CLASS_RoseSwimLane) as IRoseSwimLane;
end;

class function CoRoseSwimLane.CreateRemote(const MachineName: string): IRoseSwimLane;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSwimLane) as IRoseSwimLane;
end;

class function CoRoseItemViewCollection.Create: IRoseItemViewCollection;
begin
  Result := CreateComObject(CLASS_RoseItemViewCollection) as IRoseItemViewCollection;
end;

class function CoRoseItemViewCollection.CreateRemote(const MachineName: string): IRoseItemViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseItemViewCollection) as IRoseItemViewCollection;
end;

class function CoRosePropertyCollection.Create: IRosePropertyCollection;
begin
  Result := CreateComObject(CLASS_RosePropertyCollection) as IRosePropertyCollection;
end;

class function CoRosePropertyCollection.CreateRemote(const MachineName: string): IRosePropertyCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RosePropertyCollection) as IRosePropertyCollection;
end;

class function CoRoseOperationCollection.Create: IRoseOperationCollection;
begin
  Result := CreateComObject(CLASS_RoseOperationCollection) as IRoseOperationCollection;
end;

class function CoRoseOperationCollection.CreateRemote(const MachineName: string): IRoseOperationCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseOperationCollection) as IRoseOperationCollection;
end;

class function CoRoseDeviceCollection.Create: IRoseDeviceCollection;
begin
  Result := CreateComObject(CLASS_RoseDeviceCollection) as IRoseDeviceCollection;
end;

class function CoRoseDeviceCollection.CreateRemote(const MachineName: string): IRoseDeviceCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDeviceCollection) as IRoseDeviceCollection;
end;

class function CoRoseInstantiateRelation.Create: IRoseInstantiateRelation;
begin
  Result := CreateComObject(CLASS_RoseInstantiateRelation) as IRoseInstantiateRelation;
end;

class function CoRoseInstantiateRelation.CreateRemote(const MachineName: string): IRoseInstantiateRelation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseInstantiateRelation) as IRoseInstantiateRelation;
end;

class function CoRoseContextMenuItem.Create: IRoseContextMenuItem;
begin
  Result := CreateComObject(CLASS_RoseContextMenuItem) as IRoseContextMenuItem;
end;

class function CoRoseContextMenuItem.CreateRemote(const MachineName: string): IRoseContextMenuItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseContextMenuItem) as IRoseContextMenuItem;
end;

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

class function CoRoseSwimLaneCollection.Create: IRoseSwimLaneCollection;
begin
  Result := CreateComObject(CLASS_RoseSwimLaneCollection) as IRoseSwimLaneCollection;
end;

class function CoRoseSwimLaneCollection.CreateRemote(const MachineName: string): IRoseSwimLaneCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSwimLaneCollection) as IRoseSwimLaneCollection;
end;

class function CoRoseModuleVisibilityRelationship.Create: IRoseModuleVisibilityRelationship;
begin
  Result := CreateComObject(CLASS_RoseModuleVisibilityRelationship) as IRoseModuleVisibilityRelationship;
end;

class function CoRoseModuleVisibilityRelationship.CreateRemote(const MachineName: string): IRoseModuleVisibilityRelationship;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModuleVisibilityRelationship) as IRoseModuleVisibilityRelationship;
end;

class function CoRoseComponentViewCollection.Create: IRoseComponentViewCollection;
begin
  Result := CreateComObject(CLASS_RoseComponentViewCollection) as IRoseComponentViewCollection;
end;

class function CoRoseComponentViewCollection.CreateRemote(const MachineName: string): IRoseComponentViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseComponentViewCollection) as IRoseComponentViewCollection;
end;

class function CoRoseHasRelationshipCollection.Create: IRoseHasRelationshipCollection;
begin
  Result := CreateComObject(CLASS_RoseHasRelationshipCollection) as IRoseHasRelationshipCollection;
end;

class function CoRoseHasRelationshipCollection.CreateRemote(const MachineName: string): IRoseHasRelationshipCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseHasRelationshipCollection) as IRoseHasRelationshipCollection;
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

class function CoRoseInstanceView.Create: IRoseInstanceView;
begin
  Result := CreateComObject(CLASS_RoseInstanceView) as IRoseInstanceView;
end;

class function CoRoseInstanceView.CreateRemote(const MachineName: string): IRoseInstanceView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseInstanceView) as IRoseInstanceView;
end;

class function CoRoseLink.Create: IRoseLink;
begin
  Result := CreateComObject(CLASS_RoseLink) as IRoseLink;
end;

class function CoRoseLink.CreateRemote(const MachineName: string): IRoseLink;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseLink) as IRoseLink;
end;

class function CoRoseObjectInstance.Create: IRoseObjectInstance;
begin
  Result := CreateComObject(CLASS_RoseObjectInstance) as IRoseObjectInstance;
end;

class function CoRoseObjectInstance.CreateRemote(const MachineName: string): IRoseObjectInstance;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseObjectInstance) as IRoseObjectInstance;
end;

class function CoRoseCategoryDependency.Create: IRoseCategoryDependency;
begin
  Result := CreateComObject(CLASS_RoseCategoryDependency) as IRoseCategoryDependency;
end;

class function CoRoseCategoryDependency.CreateRemote(const MachineName: string): IRoseCategoryDependency;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseCategoryDependency) as IRoseCategoryDependency;
end;

class function CoRoseInheritRelation.Create: IRoseInheritRelation;
begin
  Result := CreateComObject(CLASS_RoseInheritRelation) as IRoseInheritRelation;
end;

class function CoRoseInheritRelation.CreateRemote(const MachineName: string): IRoseInheritRelation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseInheritRelation) as IRoseInheritRelation;
end;

class function CoRoseView_Font.Create: IRoseView_Font;
begin
  Result := CreateComObject(CLASS_RoseView_Font) as IRoseView_Font;
end;

class function CoRoseView_Font.CreateRemote(const MachineName: string): IRoseView_Font;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseView_Font) as IRoseView_Font;
end;

class function CoRoseStateMachine.Create: IRoseStateMachine;
begin
  Result := CreateComObject(CLASS_RoseStateMachine) as IRoseStateMachine;
end;

class function CoRoseStateMachine.CreateRemote(const MachineName: string): IRoseStateMachine;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateMachine) as IRoseStateMachine;
end;

class function CoRoseModule.Create: IRoseModule;
begin
  Result := CreateComObject(CLASS_RoseModule) as IRoseModule;
end;

class function CoRoseModule.CreateRemote(const MachineName: string): IRoseModule;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModule) as IRoseModule;
end;

class function CoRoseUseCase.Create: IRoseUseCase;
begin
  Result := CreateComObject(CLASS_RoseUseCase) as IRoseUseCase;
end;

class function CoRoseUseCase.CreateRemote(const MachineName: string): IRoseUseCase;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseUseCase) as IRoseUseCase;
end;

class function CoRoseItemCollection.Create: IRoseItemCollection;
begin
  Result := CreateComObject(CLASS_RoseItemCollection) as IRoseItemCollection;
end;

class function CoRoseItemCollection.CreateRemote(const MachineName: string): IRoseItemCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseItemCollection) as IRoseItemCollection;
end;

class function CoRoseNoteViewCollection.Create: IRoseNoteViewCollection;
begin
  Result := CreateComObject(CLASS_RoseNoteViewCollection) as IRoseNoteViewCollection;
end;

class function CoRoseNoteViewCollection.CreateRemote(const MachineName: string): IRoseNoteViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseNoteViewCollection) as IRoseNoteViewCollection;
end;

class function CoRoseInheritRelationCollection.Create: IRoseInheritRelationCollection;
begin
  Result := CreateComObject(CLASS_RoseInheritRelationCollection) as IRoseInheritRelationCollection;
end;

class function CoRoseInheritRelationCollection.CreateRemote(const MachineName: string): IRoseInheritRelationCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseInheritRelationCollection) as IRoseInheritRelationCollection;
end;

class function CoRoseDeploymentDiagramCollection.Create: IRoseDeploymentDiagramCollection;
begin
  Result := CreateComObject(CLASS_RoseDeploymentDiagramCollection) as IRoseDeploymentDiagramCollection;
end;

class function CoRoseDeploymentDiagramCollection.CreateRemote(const MachineName: string): IRoseDeploymentDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDeploymentDiagramCollection) as IRoseDeploymentDiagramCollection;
end;

class function CoRoseStringCollection.Create: IRoseStringCollection;
begin
  Result := CreateComObject(CLASS_RoseStringCollection) as IRoseStringCollection;
end;

class function CoRoseStringCollection.CreateRemote(const MachineName: string): IRoseStringCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStringCollection) as IRoseStringCollection;
end;

class function CoRoseStateViewCollection.Create: IRoseStateViewCollection;
begin
  Result := CreateComObject(CLASS_RoseStateViewCollection) as IRoseStateViewCollection;
end;

class function CoRoseStateViewCollection.CreateRemote(const MachineName: string): IRoseStateViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateViewCollection) as IRoseStateViewCollection;
end;

class function CoRoseDecisionView.Create: IRoseDecisionView;
begin
  Result := CreateComObject(CLASS_RoseDecisionView) as IRoseDecisionView;
end;

class function CoRoseDecisionView.CreateRemote(const MachineName: string): IRoseDecisionView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDecisionView) as IRoseDecisionView;
end;

class function CoRoseStateMachineOwner.Create: IRoseStateMachineOwner;
begin
  Result := CreateComObject(CLASS_RoseStateMachineOwner) as IRoseStateMachineOwner;
end;

class function CoRoseStateMachineOwner.CreateRemote(const MachineName: string): IRoseStateMachineOwner;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateMachineOwner) as IRoseStateMachineOwner;
end;

class function CoRoseProcessCollection.Create: IRoseProcessCollection;
begin
  Result := CreateComObject(CLASS_RoseProcessCollection) as IRoseProcessCollection;
end;

class function CoRoseProcessCollection.CreateRemote(const MachineName: string): IRoseProcessCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseProcessCollection) as IRoseProcessCollection;
end;

class function CoRoseAssociationCollection.Create: IRoseAssociationCollection;
begin
  Result := CreateComObject(CLASS_RoseAssociationCollection) as IRoseAssociationCollection;
end;

class function CoRoseAssociationCollection.CreateRemote(const MachineName: string): IRoseAssociationCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAssociationCollection) as IRoseAssociationCollection;
end;

class function CoRoseModuleDiagramCollection.Create: IRoseModuleDiagramCollection;
begin
  Result := CreateComObject(CLASS_RoseModuleDiagramCollection) as IRoseModuleDiagramCollection;
end;

class function CoRoseModuleDiagramCollection.CreateRemote(const MachineName: string): IRoseModuleDiagramCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModuleDiagramCollection) as IRoseModuleDiagramCollection;
end;

class function CoRoseDiagram.Create: IRoseDiagram;
begin
  Result := CreateComObject(CLASS_RoseDiagram) as IRoseDiagram;
end;

class function CoRoseDiagram.CreateRemote(const MachineName: string): IRoseDiagram;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDiagram) as IRoseDiagram;
end;

class function CoRoseAbstractStateCollection.Create: IRoseAbstractStateCollection;
begin
  Result := CreateComObject(CLASS_RoseAbstractStateCollection) as IRoseAbstractStateCollection;
end;

class function CoRoseAbstractStateCollection.CreateRemote(const MachineName: string): IRoseAbstractStateCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAbstractStateCollection) as IRoseAbstractStateCollection;
end;

class function CoRoseRichTypeValuesCollection.Create: IRoseRichTypeValuesCollection;
begin
  Result := CreateComObject(CLASS_RoseRichTypeValuesCollection) as IRoseRichTypeValuesCollection;
end;

class function CoRoseRichTypeValuesCollection.CreateRemote(const MachineName: string): IRoseRichTypeValuesCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRichTypeValuesCollection) as IRoseRichTypeValuesCollection;
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

class function CoRoseAttribute.Create: IRoseAttribute;
begin
  Result := CreateComObject(CLASS_RoseAttribute) as IRoseAttribute;
end;

class function CoRoseAttribute.CreateRemote(const MachineName: string): IRoseAttribute;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAttribute) as IRoseAttribute;
end;

class function CoRoseClassDiagram.Create: IRoseClassDiagram;
begin
  Result := CreateComObject(CLASS_RoseClassDiagram) as IRoseClassDiagram;
end;

class function CoRoseClassDiagram.CreateRemote(const MachineName: string): IRoseClassDiagram;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassDiagram) as IRoseClassDiagram;
end;

class function CoRoseNoteView.Create: IRoseNoteView;
begin
  Result := CreateComObject(CLASS_RoseNoteView) as IRoseNoteView;
end;

class function CoRoseNoteView.CreateRemote(const MachineName: string): IRoseNoteView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseNoteView) as IRoseNoteView;
end;

class function CoRosePackage.Create: IRosePackage;
begin
  Result := CreateComObject(CLASS_RosePackage) as IRosePackage;
end;

class function CoRosePackage.CreateRemote(const MachineName: string): IRosePackage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RosePackage) as IRosePackage;
end;

class function CoRosePathMap.Create: IRosePathMap;
begin
  Result := CreateComObject(CLASS_RosePathMap) as IRosePathMap;
end;

class function CoRosePathMap.CreateRemote(const MachineName: string): IRosePathMap;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RosePathMap) as IRosePathMap;
end;

class function CoRoseSyncItemViewCollection.Create: IRoseSyncItemViewCollection;
begin
  Result := CreateComObject(CLASS_RoseSyncItemViewCollection) as IRoseSyncItemViewCollection;
end;

class function CoRoseSyncItemViewCollection.CreateRemote(const MachineName: string): IRoseSyncItemViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSyncItemViewCollection) as IRoseSyncItemViewCollection;
end;

class function CoRoseAbstractState.Create: IRoseAbstractState;
begin
  Result := CreateComObject(CLASS_RoseAbstractState) as IRoseAbstractState;
end;

class function CoRoseAbstractState.CreateRemote(const MachineName: string): IRoseAbstractState;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAbstractState) as IRoseAbstractState;
end;

class function CoRoseModuleVisibilityRelationshipCollection.Create: IRoseModuleVisibilityRelationshipCollection;
begin
  Result := CreateComObject(CLASS_RoseModuleVisibilityRelationshipCollection) as IRoseModuleVisibilityRelationshipCollection;
end;

class function CoRoseModuleVisibilityRelationshipCollection.CreateRemote(const MachineName: string): IRoseModuleVisibilityRelationshipCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModuleVisibilityRelationshipCollection) as IRoseModuleVisibilityRelationshipCollection;
end;

class function CoRoseClassCollection.Create: IRoseClassCollection;
begin
  Result := CreateComObject(CLASS_RoseClassCollection) as IRoseClassCollection;
end;

class function CoRoseClassCollection.CreateRemote(const MachineName: string): IRoseClassCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassCollection) as IRoseClassCollection;
end;

class function CoRoseMessage.Create: IRoseMessage;
begin
  Result := CreateComObject(CLASS_RoseMessage) as IRoseMessage;
end;

class function CoRoseMessage.CreateRemote(const MachineName: string): IRoseMessage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseMessage) as IRoseMessage;
end;

class function CoRoseConnectionRelationCollection.Create: IRoseConnectionRelationCollection;
begin
  Result := CreateComObject(CLASS_RoseConnectionRelationCollection) as IRoseConnectionRelationCollection;
end;

class function CoRoseConnectionRelationCollection.CreateRemote(const MachineName: string): IRoseConnectionRelationCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseConnectionRelationCollection) as IRoseConnectionRelationCollection;
end;

class function CoRoseClassDependencyCollection.Create: IRoseClassDependencyCollection;
begin
  Result := CreateComObject(CLASS_RoseClassDependencyCollection) as IRoseClassDependencyCollection;
end;

class function CoRoseClassDependencyCollection.CreateRemote(const MachineName: string): IRoseClassDependencyCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassDependencyCollection) as IRoseClassDependencyCollection;
end;

class function CoRoseAssociation.Create: IRoseAssociation;
begin
  Result := CreateComObject(CLASS_RoseAssociation) as IRoseAssociation;
end;

class function CoRoseAssociation.CreateRemote(const MachineName: string): IRoseAssociation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseAssociation) as IRoseAssociation;
end;

class function CoRoseEventCollection.Create: IRoseEventCollection;
begin
  Result := CreateComObject(CLASS_RoseEventCollection) as IRoseEventCollection;
end;

class function CoRoseEventCollection.CreateRemote(const MachineName: string): IRoseEventCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseEventCollection) as IRoseEventCollection;
end;

class function CoRoseStateCollection.Create: IRoseStateCollection;
begin
  Result := CreateComObject(CLASS_RoseStateCollection) as IRoseStateCollection;
end;

class function CoRoseStateCollection.CreateRemote(const MachineName: string): IRoseStateCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateCollection) as IRoseStateCollection;
end;

class function CoRoseSyncItemView.Create: IRoseSyncItemView;
begin
  Result := CreateComObject(CLASS_RoseSyncItemView) as IRoseSyncItemView;
end;

class function CoRoseSyncItemView.CreateRemote(const MachineName: string): IRoseSyncItemView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSyncItemView) as IRoseSyncItemView;
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

class function CoRoseStateVertexCollection.Create: IRoseStateVertexCollection;
begin
  Result := CreateComObject(CLASS_RoseStateVertexCollection) as IRoseStateVertexCollection;
end;

class function CoRoseStateVertexCollection.CreateRemote(const MachineName: string): IRoseStateVertexCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateVertexCollection) as IRoseStateVertexCollection;
end;

class function CoRoseSubsystemViewCollection.Create: IRoseSubsystemViewCollection;
begin
  Result := CreateComObject(CLASS_RoseSubsystemViewCollection) as IRoseSubsystemViewCollection;
end;

class function CoRoseSubsystemViewCollection.CreateRemote(const MachineName: string): IRoseSubsystemViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSubsystemViewCollection) as IRoseSubsystemViewCollection;
end;

class function CoRoseModuleDiagram.Create: IRoseModuleDiagram;
begin
  Result := CreateComObject(CLASS_RoseModuleDiagram) as IRoseModuleDiagram;
end;

class function CoRoseModuleDiagram.CreateRemote(const MachineName: string): IRoseModuleDiagram;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseModuleDiagram) as IRoseModuleDiagram;
end;

class function CoRoseSubsystem.Create: IRoseSubsystem;
begin
  Result := CreateComObject(CLASS_RoseSubsystem) as IRoseSubsystem;
end;

class function CoRoseSubsystem.CreateRemote(const MachineName: string): IRoseSubsystem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSubsystem) as IRoseSubsystem;
end;

class function CoRoseExternalDocumentCollection.Create: IRoseExternalDocumentCollection;
begin
  Result := CreateComObject(CLASS_RoseExternalDocumentCollection) as IRoseExternalDocumentCollection;
end;

class function CoRoseExternalDocumentCollection.CreateRemote(const MachineName: string): IRoseExternalDocumentCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseExternalDocumentCollection) as IRoseExternalDocumentCollection;
end;

class function CoRoseInstanceViewCollection.Create: IRoseInstanceViewCollection;
begin
  Result := CreateComObject(CLASS_RoseInstanceViewCollection) as IRoseInstanceViewCollection;
end;

class function CoRoseInstanceViewCollection.CreateRemote(const MachineName: string): IRoseInstanceViewCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseInstanceViewCollection) as IRoseInstanceViewCollection;
end;

class function CoRoseItemView.Create: IRoseItemView;
begin
  Result := CreateComObject(CLASS_RoseItemView) as IRoseItemView;
end;

class function CoRoseItemView.CreateRemote(const MachineName: string): IRoseItemView;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseItemView) as IRoseItemView;
end;

class function CoRoseTransitionCollection.Create: IRoseTransitionCollection;
begin
  Result := CreateComObject(CLASS_RoseTransitionCollection) as IRoseTransitionCollection;
end;

class function CoRoseTransitionCollection.CreateRemote(const MachineName: string): IRoseTransitionCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseTransitionCollection) as IRoseTransitionCollection;
end;

class function CoRoseState.Create: IRoseState;
begin
  Result := CreateComObject(CLASS_RoseState) as IRoseState;
end;

class function CoRoseState.CreateRemote(const MachineName: string): IRoseState;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseState) as IRoseState;
end;

class function CoRoseObjectInstanceCollection.Create: IRoseObjectInstanceCollection;
begin
  Result := CreateComObject(CLASS_RoseObjectInstanceCollection) as IRoseObjectInstanceCollection;
end;

class function CoRoseObjectInstanceCollection.CreateRemote(const MachineName: string): IRoseObjectInstanceCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseObjectInstanceCollection) as IRoseObjectInstanceCollection;
end;

class function CoRoseRoleCollection.Create: IRoseRoleCollection;
begin
  Result := CreateComObject(CLASS_RoseRoleCollection) as IRoseRoleCollection;
end;

class function CoRoseRoleCollection.CreateRemote(const MachineName: string): IRoseRoleCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRoleCollection) as IRoseRoleCollection;
end;

class function CoRoseClassRelation.Create: IRoseClassRelation;
begin
  Result := CreateComObject(CLASS_RoseClassRelation) as IRoseClassRelation;
end;

class function CoRoseClassRelation.CreateRemote(const MachineName: string): IRoseClassRelation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseClassRelation) as IRoseClassRelation;
end;

class function CoRoseCategory.Create: IRoseCategory;
begin
  Result := CreateComObject(CLASS_RoseCategory) as IRoseCategory;
end;

class function CoRoseCategory.CreateRemote(const MachineName: string): IRoseCategory;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseCategory) as IRoseCategory;
end;

class function CoRoseDefaultModelProperties.Create: IRoseDefaultModelProperties;
begin
  Result := CreateComObject(CLASS_RoseDefaultModelProperties) as IRoseDefaultModelProperties;
end;

class function CoRoseDefaultModelProperties.CreateRemote(const MachineName: string): IRoseDefaultModelProperties;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseDefaultModelProperties) as IRoseDefaultModelProperties;
end;

class function CoRoseStateMachineCollection.Create: IRoseStateMachineCollection;
begin
  Result := CreateComObject(CLASS_RoseStateMachineCollection) as IRoseStateMachineCollection;
end;

class function CoRoseStateMachineCollection.CreateRemote(const MachineName: string): IRoseStateMachineCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseStateMachineCollection) as IRoseStateMachineCollection;
end;

class function CoRoseSyncItem.Create: IRoseSyncItem;
begin
  Result := CreateComObject(CLASS_RoseSyncItem) as IRoseSyncItem;
end;

class function CoRoseSyncItem.CreateRemote(const MachineName: string): IRoseSyncItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseSyncItem) as IRoseSyncItem;
end;

class function CoRoseRealizeRelationCollection.Create: IRoseRealizeRelationCollection;
begin
  Result := CreateComObject(CLASS_RoseRealizeRelationCollection) as IRoseRealizeRelationCollection;
end;

class function CoRoseRealizeRelationCollection.CreateRemote(const MachineName: string): IRoseRealizeRelationCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseRealizeRelationCollection) as IRoseRealizeRelationCollection;
end;

class function CoRosePackageCollection.Create: IRosePackageCollection;
begin
  Result := CreateComObject(CLASS_RosePackageCollection) as IRosePackageCollection;
end;

class function CoRosePackageCollection.CreateRemote(const MachineName: string): IRosePackageCollection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RosePackageCollection) as IRosePackageCollection;
end;

class function CoRoseExternalDocument.Create: IRoseExternalDocument;
begin
  Result := CreateComObject(CLASS_RoseExternalDocument) as IRoseExternalDocument;
end;

class function CoRoseExternalDocument.CreateRemote(const MachineName: string): IRoseExternalDocument;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_RoseExternalDocument) as IRoseExternalDocument;
end;

end.
