object dmBoldUMLModelEditorHandles: TdmBoldUMLModelEditorHandles
  OnCreate = DataModuleCreate
  Height = 706
  Width = 1171
  object bchAttributePMapperNames: TBoldCursorHandle
    RootHandle = bdhAttributePMapperNames
    Left = 187
    Top = 361
  end
  object behAllDataTypes: TBoldExpressionHandle
    RootHandle = behModel
    Expression = 'allOwnedElement->filterOnType(UMLDataType)'
    Left = 370
    Top = 171
  end
  object blhClassAssociations: TBoldListHandle
    RootHandle = behClassAssociations
    Left = 804
    Top = 424
  end
  object blhOclExpressions: TBoldListHandle
    RootHandle = behOclExpressions
    Left = 384
    Top = 488
  end
  object blhTypesForAttribute: TBoldListHandle
    RootHandle = behAllDataTypes
    Expression = 'self->orderby(name)'
    Left = 547
    Top = 160
  end
  object bdhTypesForAttribute: TBoldDerivedHandle
    RootHandle = behModel
    OnDeriveAndSubscribe = bdhTypesForAttributeDeriveAndSubscribe
    ValueTypeName = 'Collection(UMLDataType)'
    Left = 547
    Top = 237
  end
  object bchTypesForAttribute: TBoldCursorHandle
    RootHandle = bdhTypesForAttribute
    Left = 547
    Top = 325
  end
  object brhRoot: TBoldReferenceHandle
    StaticValueTypeName = 'UMLModel'
    Left = 28
    Top = 32
  end
  object behBoldified: TBoldExpressionHandle
    RootHandle = behModel
    Expression = 'taggedValue['#39'_Boldify.boldified'#39'].Value='#39'True'#39
    Left = 200
    Top = 32
  end
  object brhCopyCut: TBoldReferenceHandle
    StaticValueTypeName = 'UMLModelElement'
    Left = 160
    Top = 32
  end
  object brhCurrentElement: TBoldReferenceHandle
    StaticValueTypeName = 'UMLElement'
    Left = 124
    Top = 32
  end
  object bvhOptimisticLocking: TBoldVariableHandle
    ValueTypeName = 'Collection(String)'
    InitialValues.Strings = (
      '<Default>'
      'Off'
      'ModifiedMembers'
      'AllMembers'
      'TimeStamp')
    Left = 129
    Top = 64
  end
  object behModel: TBoldExpressionHandle
    RootHandle = brhRoot
    Left = 28
    Top = 64
  end
  object blhModelAssociations: TBoldListHandle
    RootHandle = behModelAssociations
    Left = 96
    Top = 64
  end
  object blhModelClasses: TBoldListHandle
    RootHandle = behModelClasses
    Expression = 'self->orderBy(name)'
    Left = 62
    Top = 64
  end
  object behClass: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 
      'if self.oclIsTypeOf(UMLFeature) then'#13#10'  self.oclasType(UMLFeatur' +
      'e) .owner'#13#10'else'#13#10'  self->filterOnType(UMLClass)->first'#13#10'endif'#13#10#13 +
      #10
    Left = 28
    Top = 96
  end
  object behClassSuperClass: TBoldExpressionHandle
    RootHandle = behClass
    Expression = 'superclass'
    Left = 60
    Top = 96
  end
  object bvhTableMapping: TBoldVariableHandle
    ValueTypeName = 'Collection(String)'
    Left = 95
    Top = 97
  end
  object bcrTableMapping: TBoldCursorHandle
    RootHandle = bvhTableMapping
    AutoFirst = False
    Left = 131
    Top = 97
  end
  object blhClassOperations: TBoldListHandle
    RootHandle = behClassOperations
    Left = 296
    Top = 92
  end
  object blhAllSuperclasses: TBoldListHandle
    RootHandle = behClass
    Expression = 
      'model.allOwnedElement->filterOnType(UMLCLass)->excluding(self)->' +
      'orderby(name)'
    Left = 167
    Top = 97
  end
  object behClassIsRootClass: TBoldExpressionHandle
    RootHandle = behClass
    Expression = 
      '(model.taggedValue['#39'_Boldify.boldified'#39'].Value='#39'True'#39') and (gene' +
      'ralization->isEmpty)'
    Left = 164
    Top = 128
  end
  object bcrAutoCreated: TBoldAsCheckBoxStateRenderer
    OnGetAsCheckBoxState = bcrAutoCreatedGetAsCheckBoxState
    OnSetAsCheckBoxState = bcrAutoCreatedSetAsCheckBoxState
    Left = 132
    Top = 128
  end
  object blhClassAssociationEnds: TBoldListHandle
    RootHandle = behClassAssociationEnds
    Left = 812
    Top = 152
  end
  object blhClassAttributes: TBoldListHandle
    RootHandle = behClassAttributes
    Left = 804
    Top = 296
  end
  object blhClassMembers: TBoldListHandle
    RootHandle = behModel
    Expression = 
      'UMLAttribute.allInstances->union((UMLAssociationEnd.allInstances' +
      '))'#13#10
    Left = 28
    Top = 128
  end
  object behAttribute: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLAttribute)->first'
    Left = 28
    Top = 160
  end
  object blhAllDataTypes: TBoldListHandle
    RootHandle = behAllDataTypes
    Expression = 'self->orderby(name)'
    Left = 64
    Top = 160
  end
  object bvhDelphiProperty: TBoldVariableHandle
    ValueTypeName = 'Collection(String)'
    Left = 95
    Top = 165
  end
  object bchDelphiProperty: TBoldCursorHandle
    RootHandle = bvhDelphiProperty
    AutoFirst = False
    Left = 127
    Top = 165
  end
  object bvhAttributeKind: TBoldVariableHandle
    ValueTypeName = 'Collection(String)'
    Left = 163
    Top = 161
  end
  object bdhAttributePMapperNames: TBoldDerivedHandle
    RootHandle = behAttribute
    OnDeriveAndSubscribe = bdhAttributePMapperNamesDeriveAndSubscribe
    ValueTypeName = 'Collection(String)'
    Left = 300
    Top = 132
  end
  object bchDelphiFunctionType: TBoldCursorHandle
    RootHandle = bvhDelphiFunctionType
    AutoFirst = False
    Left = 203
    Top = 201
  end
  object bvhDelphiFunctionType: TBoldVariableHandle
    ValueTypeName = 'Collection(String)'
    Left = 171
    Top = 201
  end
  object blhAllParameterKind: TBoldListHandle
    RootHandle = behModel
    Expression = 'ParameterDirectionKind.allInstances'
    Left = 135
    Top = 201
  end
  object blhAllOwnerScope: TBoldListHandle
    RootHandle = behModel
    Expression = 'ScopeKind.allInstances'
    Left = 99
    Top = 201
  end
  object blhParameters: TBoldListHandle
    RootHandle = behOperation
    Expression = 'parameter'
    Left = 64
    Top = 200
  end
  object behOperation: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLOperation)->first'
    Left = 32
    Top = 200
  end
  object behAssociation: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLAssociation)->first'
    Left = 32
    Top = 236
  end
  object blhAssociationAssociationEnds: TBoldListHandle
    RootHandle = behAssociation
    Expression = 'connection'
    Left = 67
    Top = 238
  end
  object bchMultiplicityValues: TBoldCursorHandle
    RootHandle = bvhMultiplicityValues
    AutoFirst = False
    Left = 179
    Top = 269
  end
  object bvhMultiplicityValues: TBoldVariableHandle
    ValueTypeName = 'Collection(String)'
    InitialValues.Strings = (
      '0..1'
      '1..1'
      '0..n'
      '1..n')
    Left = 143
    Top = 269
  end
  object blhAssociationEndQualifiers: TBoldListHandle
    RootHandle = behAssociationEnd
    Expression = 'qualifier'
    Left = 107
    Top = 270
  end
  object behAssociationEnd: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLAssociationEnd)->first'
    Left = 32
    Top = 272
  end
  object behAssociationEndType: TBoldExpressionHandle
    RootHandle = behAssociationEnd
    Expression = 'type'
    Left = 67
    Top = 273
  end
  object bvhDeleteActions: TBoldVariableHandle
    ValueTypeName = 'Collection(String)'
    Left = 67
    Top = 305
  end
  object bchDeleteActions: TBoldCursorHandle
    RootHandle = bvhDeleteActions
    AutoFirst = False
    Left = 107
    Top = 305
  end
  object blhAllAggregationKind: TBoldListHandle
    RootHandle = behModel
    Expression = 'AggregationKind.allinstances'
    Left = 143
    Top = 305
  end
  object blhAllChangeabilityKind: TBoldListHandle
    RootHandle = behModel
    Expression = 'ChangeableKind.allinstances'
    Left = 175
    Top = 304
  end
  object behParameter: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLParameter)->first'
    Left = 28
    Top = 356
  end
  object brhPackage: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLPackage)->first'
    Left = 32
    Top = 396
  end
  object blhPackageClasses: TBoldListHandle
    RootHandle = brhPackage
    Expression = 'ownedElement->filterOnType(UMLClass)'
    Left = 67
    Top = 394
  end
  object blhPackageAssociations: TBoldListHandle
    RootHandle = brhPackage
    Expression = 'ownedElement->filterOnType(UMLAssociation)'
    Left = 107
    Top = 390
  end
  object behHighestSeverity: TBoldExpressionHandle
    RootHandle = behModel
    Expression = 'validator.highestSeverity'
    Left = 131
    Top = 437
  end
  object brhDataType: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLDataType)->first'
    Left = 32
    Top = 436
  end
  object blhAllVisibilityKind: TBoldListHandle
    RootHandle = behModel
    Expression = 'VisibilityKind.allinstances'
    Left = 35
    Top = 485
  end
  object blhAllClasses: TBoldListHandle
    RootHandle = brhRoot
    Expression = 'UMLClass.allInstances->orderBy(name)'
    Left = 71
    Top = 486
  end
  object bsrRedOnAutocreated: TBoldAsStringRenderer
    Left = 264
    Top = 620
  end
  object bchAttributeKind: TBoldCursorHandle
    RootHandle = bvhAttributeKind
    AutoFirst = False
    Left = 207
    Top = 157
  end
  object BoldAsVariantRenderer1: TBoldAsVariantRenderer
    OnSetFont = BoldAsVariantRenderer1SetFont
    OnSetColor = BoldAsVariantRenderer1SetColor
    Left = 464
    Top = 632
  end
  object blhPackages: TBoldListHandle
    RootHandle = behPackages
    Left = 536
    Top = 584
  end
  object blhAssociations: TBoldListHandle
    RootHandle = behAssociations
    Left = 536
    Top = 480
  end
  object behModelClasses: TBoldExpressionHandle
    RootHandle = behModel
    Expression = 'classes'
    Left = 624
    Top = 48
  end
  object behModelAssociations: TBoldExpressionHandle
    RootHandle = behModel
    Expression = 'ownedElement->filterOnType(UMLAssociation)'
    Left = 624
    Top = 112
  end
  object behClassFeatures: TBoldExpressionHandle
    RootHandle = behModel
    Expression = 'associations.connection->union(classes.feature)'
    Left = 488
    Top = 312
  end
  object bchClassFeatures: TBoldCursorHandle
    RootHandle = behClassFeatures
    Left = 488
    Top = 384
  end
  object bcrBooleanToCheckBox: TBoldAsCheckBoxStateRenderer
    OnMayModify = bcrBooleanToCheckBoxMayModify
    OnSubscribe = bcrBooleanToCheckBoxSubscribe
    OnGetAsCheckBoxState = bcrBooleanToCheckBoxGetAsCheckBoxState
    OnSetAsCheckBoxState = bcrBooleanToCheckBoxSetAsCheckBoxState
    Left = 47
    Top = 609
  end
  object bchOptimisticLocking: TBoldCursorHandle
    RootHandle = bvhOptimisticLocking
    AutoFirst = False
    Left = 163
    Top = 64
  end
  object behOclExpressions: TBoldExpressionHandle
    RootHandle = blhModelClasses
    Expression = 
      'feature->select(derived)'#13#10'->union(associations.connection->selec' +
      't(derived))'#13#10'->union(constraint)'
    Left = 320
    Top = 480
  end
  object behClassOperations: TBoldExpressionHandle
    RootHandle = blhModelClasses
    Expression = 
      'feature->select(oclIsTypeOf(UMLOperation))->select(qualifyingOwn' +
      'er = self)'
    Left = 240
    Top = 96
  end
  object bsrNiceCRRenderer: TBoldAsStringRenderer
    OnSubscribe = bsrNiceCRRendererSubscribe
    OnGetAsString = bsrNiceCRRendererGetAsString
    Left = 156
    Top = 609
  end
  object rDataTypePMapper: TBoldAsVariantRenderer
    OnGetAsVariant = rDataTypePMapperGetAsVariant
    Left = 372
    Top = 621
  end
  object behClassAssociations: TBoldExpressionHandle
    RootHandle = blhModelClasses
    Expression = 'associationEnd->union(associationEnd.otherEnd)->asSet'
    Left = 800
    Top = 480
  end
  object behClassAssociationEnds: TBoldExpressionHandle
    RootHandle = blhModelClasses
    Expression = 'associationEnd'
    Left = 816
    Top = 224
  end
  object behClassAttributes: TBoldExpressionHandle
    RootHandle = blhModelClasses
    Expression = 'feature->filterOnType(UMLAttribute)'
    Left = 808
    Top = 352
  end
  object bdhSuperClasses: TBoldDerivedHandle
    RootHandle = blhModelClasses
    OnDeriveAndSubscribe = bdhSuperClassesDeriveAndSubscribe
    ValueTypeName = 'Collection(UMLClass)'
    Left = 1076
    Top = 180
  end
  object blhSuperClasses: TBoldListHandle
    RootHandle = bdhSuperClasses
    Expression = 'self->asCommaText'
    Left = 1072
    Top = 112
  end
  object behDataTypes: TBoldExpressionHandle
    RootHandle = bdhTypesForAttribute
    Left = 1032
    Top = 376
  end
  object bchDataTypes: TBoldCursorHandle
    RootHandle = behDataTypes
    Left = 1032
    Top = 304
  end
  object behPackages: TBoldExpressionHandle
    RootHandle = behModel
    Expression = 'UMLPackage.allInstances->reject(u|u.oclIsTypeOf(UMLModel))'
    Left = 616
    Top = 584
  end
  object behAssociations: TBoldExpressionHandle
    RootHandle = behModel
    Expression = 'UMLAssociation.allInstances'
    Left = 536
    Top = 544
  end
  object rDataTypeAttributeType: TBoldAsVariantRenderer
    OnGetAsVariant = rDataTypeAttributeTypeGetAsVariant
    Left = 380
    Top = 565
  end
  object blhConcreteTypesForAttribute: TBoldListHandle
    RootHandle = behAllDataTypes
    Expression = 'self->reject(isAbstract)'
    Left = 656
    Top = 240
  end
end
