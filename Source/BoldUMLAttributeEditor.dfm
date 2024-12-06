object BoldUMLAttributeEditForm: TBoldUMLAttributeEditForm
  Left = 227
  Top = 108
  ActiveControl = tbxAttributeName
  BorderStyle = bsDialog
  Caption = 'Edit Attribute'
  ClientHeight = 500
  ClientWidth = 471
  Color = clBtnFace
  ParentFont = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    471
    500)
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 16
    Top = 16
    Width = 447
    Height = 435
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      DesignSize = (
        439
        405)
      object btAttributeShowDerivExprEditor: TButton
        Left = 327
        Top = 156
        Width = 17
        Height = 21
        Hint = '|Edit the derivation expression for this attribute.'
        HelpContext = 24
        Anchors = [akTop, akRight]
        Caption = #188
        Enabled = False
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Symbol'
        Font.Style = []
        ParentFont = False
        TabOrder = 12
        OnClick = btAttributeShowDerivExprEditorClick
      end
      object gbDelphiFeatures: TcxGroupBox
        Left = 20
        Top = 304
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Attribute kind Delphi features'
        ParentFont = False
        Style.BorderStyle = ebsFlat
        TabOrder = 30
        Visible = False
        DesignSize = (
          332
          85)
        Height = 85
        Width = 332
        object lblDPRead: TcxLabel
          Left = 3
          Top = 36
          Margins.Bottom = 0
          Caption = 'Pr&operty read'
          ParentFont = False
          Style.BorderStyle = ebsNone
          Properties.Alignment.Horz = taRightJustify
          Transparent = True
          AnchorX = 76
        end
        object lblDPWrite: TcxLabel
          Left = 0
          Top = 60
          Margins.Bottom = 0
          Caption = 'Property &write'
          ParentFont = False
          Style.BorderStyle = ebsNone
          Properties.Alignment.Horz = taRightJustify
          Transparent = True
          AnchorX = 76
        end
        object cmbTVDPRead: TcxBoldComboBox
          Left = 80
          Top = 32
          HelpContext = 1220
          Anchors = [akLeft, akTop, akRight]
          DataBinding.BoldHandle = behAttribute
          DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiPropertyRead'#39'].value'
          DataBinding.BoldProperties.ApplyPolicy = bapChange
          DataBinding.BoldProperties.NilRepresentation = ''
          ParentFont = False
          Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.bchDelphiProperty
          Properties.BoldRowProperties.Expression = ''
          Properties.BoldRowProperties.NilRepresentation = ''
          Properties.BoldSelectChangeAction = bdscSetText
          Properties.Alignment.Horz = taLeftJustify
          Properties.DropDownListStyle = lsEditFixedList
          Style.BorderStyle = ebsFlat
          Style.Color = clWindow
          TabOrder = 1
          Width = 241
        end
        object cmbTVDPWrite: TcxBoldComboBox
          Left = 80
          Top = 57
          HelpContext = 1230
          Anchors = [akLeft, akTop, akRight]
          DataBinding.BoldHandle = behAttribute
          DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiPropertyWrite'#39'].value'
          DataBinding.BoldProperties.ApplyPolicy = bapChange
          DataBinding.BoldProperties.NilRepresentation = ''
          ParentFont = False
          Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.bchDelphiProperty
          Properties.BoldRowProperties.Expression = ''
          Properties.BoldRowProperties.NilRepresentation = ''
          Properties.BoldSelectChangeAction = bdscSetText
          Properties.Alignment.Horz = taLeftJustify
          Properties.DropDownListStyle = lsEditFixedList
          Style.BorderStyle = ebsFlat
          Style.Color = clWindow
          TabOrder = 3
          Width = 241
        end
        object bcbDPHasField: TcxBoldCheckBox
          Left = 80
          Top = 15
          Hint = 
            '|Check this to get a private instance variable for the attribute' +
            '.'
          HelpContext = 1210
          Caption = 'Delphi field'
          DataBinding.BoldHandle = behAttribute
          DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiField'#39'].value'
          DataBinding.BoldProperties.ApplyPolicy = bapChange
          ParentFont = False
          Properties.ImmediatePost = True
          Style.BorderStyle = ebsFlat
          Style.TransparentBorder = False
          TabOrder = 0
        end
      end
      object cmbAttributeVisibility: TcxBoldComboBox
        Left = 101
        Top = 108
        HelpContext = 1250
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'visibility'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllVisibilityKind
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownListStyle = lsEditFixedList
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 9
        Width = 241
      end
      object cmbAttributePMapperName: TcxBoldComboBox
        Left = 101
        Top = 254
        HelpContext = 72
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.PMapper'#39'].value'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.bchAttributePMapperNames
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownListStyle = lsEditFixedList
        Properties.OnChange = cmbAttributePMapperNamePropertiesChange
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 24
        Width = 241
      end
      object cmbAttributeBoldType: TcxBoldComboBox
        Left = 101
        Top = 84
        HelpContext = 71
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'type'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhConcreteTypesForAttribute
        Properties.BoldRowProperties.Expression = 'name'
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSetValueExpression = 'type'
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownListStyle = lsEditFixedList
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 8
        Width = 241
      end
      object tbxAttributeStereotype: TcxBoldTextEdit
        Left = 101
        Top = 35
        HelpContext = 1150
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'stereotypeName'
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldRowProperties.Expression = ''
        Properties.Alignment.Horz = taLeftJustify
        Anchors = [akLeft, akTop, akRight]
        ParentFont = False
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 3
        Width = 241
      end
      object tbxAttributeName: TcxBoldTextEdit
        Left = 101
        Top = 11
        HelpContext = 77
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'name'
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldRowProperties.Expression = ''
        Properties.Alignment.Horz = taLeftJustify
        Anchors = [akLeft, akTop, akRight]
        ParentFont = False
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 1
        Width = 241
      end
      object tbxAttributeLength: TcxBoldTextEdit
        Left = 101
        Top = 132
        HelpContext = 70
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.Length'#39'].value'
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldRowProperties.Expression = ''
        Properties.Alignment.Horz = taLeftJustify
        Anchors = [akLeft, akTop, akRight]
        ParentFont = False
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 10
        Width = 241
      end
      object tbxAttributeInitialValue: TcxBoldTextEdit
        Left = 101
        Top = 182
        HelpContext = 1190
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'initialValue'
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldRowProperties.Expression = ''
        Properties.Alignment.Horz = taLeftJustify
        Properties.ValidationOptions = [evoAllowLoseFocus]
        Properties.OnValidate = tbxAttributeInitialValuePropertiesValidate
        Anchors = [akLeft, akTop, akRight]
        ParentFont = False
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 15
        Width = 241
      end
      object tbxAttributeExpressionName: TcxBoldTextEdit
        Left = 101
        Top = 230
        HelpContext = 65
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldRowProperties.Expression = ''
        Properties.Alignment.Horz = taLeftJustify
        Properties.OnChange = tbxAttributeColumnNamePropertiesChange
        Anchors = [akLeft, akTop, akRight]
        ParentFont = False
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 21
        Width = 241
      end
      object tbxAttributeDerivationOCL: TcxBoldTextEdit
        Left = 101
        Top = 156
        HelpContext = 1180
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DerivationOCL'#39'].value'
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldRowProperties.Expression = ''
        Properties.Alignment.Horz = taLeftJustify
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        ParentFont = False
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 11
        Width = 221
      end
      object tbxAttributeDelphiName: TcxBoldTextEdit
        Left = 101
        Top = 206
        HelpContext = 6
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldRowProperties.Expression = ''
        Properties.Alignment.Horz = taLeftJustify
        Properties.OnChange = tbxAttributeColumnNamePropertiesChange
        Anchors = [akLeft, akTop, akRight]
        ParentFont = False
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 18
        Width = 241
      end
      object tbxAttributeColumnName: TcxBoldTextEdit
        Left = 101
        Top = 278
        HelpContext = 66
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.ColumnName'#39'].value'
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldRowProperties.Expression = ''
        Properties.Alignment.Horz = taLeftJustify
        Properties.OnChange = tbxAttributeColumnNamePropertiesChange
        Anchors = [akLeft, akTop, akRight]
        ParentFont = False
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 27
        Width = 241
      end
      object lblAttributeVisibility: TcxLabel
        Left = 50
        Top = 112
        Margins.Bottom = 0
        Caption = 'Visi&bility'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 97
      end
      object lblAttributeStereotype: TcxLabel
        Left = 38
        Top = 39
        Margins.Bottom = 0
        Caption = '&Stereotype'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 97
      end
      object lblAttributePMapperName: TcxLabel
        Left = 16
        Top = 258
        Margins.Bottom = 0
        Caption = '&PMapper name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 97
      end
      object lblAttributeName: TcxLabel
        Left = 64
        Top = 15
        Margins.Bottom = 0
        Caption = '&Name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 97
      end
      object lblAttributeLength: TcxLabel
        Left = 57
        Top = 136
        Margins.Bottom = 0
        Caption = 'Len&gth'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 97
      end
      object lblAttributeInitialValue: TcxLabel
        Left = 34
        Top = 186
        Margins.Bottom = 0
        Caption = 'Initial value'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 97
      end
      object lblAttributeExpressionName: TcxLabel
        Left = 7
        Top = 234
        Margins.Bottom = 0
        Caption = 'E&xpression name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 97
      end
      object lblAttributeDerivationOCL: TcxLabel
        Left = 16
        Top = 160
        Margins.Bottom = 0
        Caption = 'Derivation OC&L'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 97
      end
      object lblAttributeDelphiName: TcxLabel
        Left = 28
        Top = 210
        Margins.Bottom = 0
        Caption = 'Delph&i name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 97
      end
      object lblAttributeColumnName: TcxLabel
        Left = 22
        Top = 282
        Margins.Bottom = 0
        Caption = 'Col&umn name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 97
      end
      object lblAttributeBoldType: TcxLabel
        Left = 70
        Top = 88
        Margins.Bottom = 0
        Caption = 'Type'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 97
      end
      object bcbAttributeReverseDerive: TcxBoldCheckBox
        Left = 349
        Top = 56
        Hint = '|Controls if the attribute is reverse derived.'
        HelpContext = 1240
        TabStop = False
        Anchors = [akTop, akRight]
        Caption = '&Reverse derive'
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.ReverseDerive'#39'].value'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        Enabled = False
        ParentFont = False
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 5
      end
      object bcbAttributePersistent: TcxBoldCheckBox
        Left = 349
        Top = 8
        Hint = 
          '|Controls of this attribute is persistent (stored in the persist' +
          'ent storage)'
        HelpContext = 67
        TabStop = False
        Anchors = [akTop, akRight]
        Caption = 'Persistent'
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'persistent'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.Renderer = brPersistent
        ParentFont = False
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 0
      end
      object bcbAttributeDerived: TcxBoldCheckBox
        Left = 349
        Top = 40
        Hint = '|Controls if the attribute is derived.'
        HelpContext = 69
        TabStop = False
        Anchors = [akTop, akRight]
        Caption = '&Derived'
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'derived'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.Renderer = brDerived
        ParentFont = False
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 4
      end
      object bcbAttributeDelayedFetch: TcxBoldCheckBox
        Left = 349
        Top = 72
        Hint = 
          '|Controls if the attribute is fetched with the rest of the objec' +
          't, or not until it is explicitly requested.'
        HelpContext = 78
        TabStop = False
        Anchors = [akTop, akRight]
        Caption = 'Dela&yed fetch'
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DelayedFetch'#39'].value'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        Enabled = False
        ParentFont = False
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 7
      end
      object bcbAttributeAllowNull: TcxBoldCheckBox
        Left = 349
        Top = 24
        Hint = '|Controls if the attribute can be NULL.'
        HelpContext = 68
        TabStop = False
        Anchors = [akTop, akRight]
        Caption = '&Allow null'
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.AllowNULL'#39'].value'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        ParentFont = False
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 2
      end
      object cmbTVAttributeKind: TcxBoldComboBox
        Left = 101
        Top = 59
        HelpContext = 1200
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAttribute
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.AttributeKind'#39'].value'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.bchAttributeKind
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownListStyle = lsFixedList
        Properties.OnChange = cmbTVAttributeKindPropertiesChange
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 6
        Width = 241
      end
      object lblAttributeKind: TcxLabel
        Left = 21
        Top = 63
        Margins.Bottom = 0
        Caption = 'Attribute &kind'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 97
      end
    end
  end
  object OKBtn: TButton
    Left = 303
    Top = 464
    Width = 75
    Height = 25
    Action = BoldFormSaverOkAction1
    Anchors = [akRight, akBottom]
    Default = True
    ModalResult = 1
    TabOrder = 1
    ExplicitLeft = 299
    ExplicitTop = 463
  end
  object CancelBtn: TButton
    Left = 384
    Top = 464
    Width = 75
    Height = 25
    Action = BoldFormSaverCancelAction1
    Anchors = [akRight, akBottom]
    Cancel = True
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 380
    ExplicitTop = 463
  end
  object behAttribute: TBoldExpressionHandle
    StaticSystemHandle = ServerData.SystemHandle
    RootHandle = brhAttribute
    Left = 100
    Top = 448
  end
  object brhAttribute: TBoldReferenceHandle
    StaticSystemHandle = ServerData.SystemHandle
    StaticValueTypeName = 'UMLAttribute'
    Left = 24
    Top = 448
  end
  object BoldPropertyMapper1: TBoldPropertyMapper
    MappingCollection = <
      item
        BoldHandle = behAttribute
        BoldProperties.Expression = 'derived'
        VCLComponent = tbxAttributeDerivationOCL
        VCLProperty = 'Enabled'
      end
      item
        BoldHandle = behAttribute
        BoldProperties.Expression = 'derived'
        VCLComponent = btAttributeShowDerivExprEditor
        VCLProperty = 'Enabled'
      end
      item
        BoldHandle = behAttribute
        BoldProperties.Expression = 'owner.name + '#39'.'#39' + name'
        VCLComponent = TabSheet1
        VCLProperty = 'Caption'
      end
      item
        BoldHandle = behAttribute
        BoldProperties.Expression = 'taggedValue['#39'Bold.AttributeKind'#39'].value = '#39'Delphi'#39
        VCLComponent = gbDelphiFeatures
        VCLProperty = 'Visible'
      end
      item
        BoldHandle = behAttribute
        BoldProperties.Expression = 'derived or (taggedValue['#39'Bold.ReverseDerive'#39'].value = '#39'True'#39')'
        VCLComponent = bcbAttributeReverseDerive
        VCLProperty = 'Enabled'
      end
      item
        BoldHandle = behAttribute
        BoldProperties.Expression = 'persistent or (taggedValue['#39'Bold.DelayedFetch'#39'].value = '#39'True'#39')'
        VCLComponent = bcbAttributeDelayedFetch
        VCLProperty = 'Enabled'
      end>
    Enabled = True
    Left = 404
    Top = 144
  end
  object brPersistent: TBoldAsVariantRenderer
    OnSetAsVariant = brPersistentSetAsVariant
    Left = 388
    Top = 200
  end
  object brDerived: TBoldAsVariantRenderer
    OnSetAsVariant = brDerivedSetAsVariant
    Left = 388
    Top = 248
  end
  object BoldFormSaver1: TBoldFormSaver
    StaticSystemHandle = ServerData.SystemHandle
    OnlyFirstDirty = False
    Left = 396
    Top = 400
  end
  object ActionList1: TActionList
    Left = 44
    Top = 50
    object BoldFormSaverOkAction1: TBoldFormSaverOkAction
      Category = 'Bold Actions'
      Caption = '&Ok'
      ShortCut = 16474
      BoldFormSaver = BoldFormSaver1
    end
    object BoldFormSaverCancelAction1: TBoldFormSaverCancelAction
      Category = 'Bold Actions'
      Caption = '&Cancel'
      BoldFormSaver = BoldFormSaver1
    end
  end
end
