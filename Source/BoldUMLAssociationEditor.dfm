object BoldUMLAssociationEditForm: TBoldUMLAssociationEditForm
  Left = 0
  Top = 0
  ActiveControl = tbxAssociationName
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Edit Association'
  ClientHeight = 519
  ClientWidth = 481
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  TextHeight = 13
  object PageControl2: TPageControl
    Left = 0
    Top = 0
    Width = 481
    Height = 111
    ActivePage = TabSheet1
    Align = alTop
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Association'
      DesignSize = (
        473
        83)
      object tbxAssociationStereotype: TcxBoldTextEdit
        Left = 72
        Top = 28
        HelpContext = 1150
        DataBinding.BoldHandle = behAssociation
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
        Width = 274
      end
      object cmbAssociationLinkClass: TcxBoldComboBox
        Left = 72
        Top = 52
        HelpContext = 2
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAssociation
        DataBinding.BoldProperties.Expression = 'class'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = '<none>'
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllClasses
        Properties.BoldLookupListProperties.NilElementMode = neInsertFirst
        Properties.BoldRowProperties.Expression = 'name'
        Properties.BoldRowProperties.NilRepresentation = '<none>'
        Properties.BoldSetValueExpression = 'class'
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownListStyle = lsEditFixedList
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 6
        Width = 274
      end
      object tbxAssociationName: TcxBoldTextEdit
        Left = 72
        Top = 3
        HelpContext = 35
        DataBinding.BoldHandle = behAssociation
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
        Width = 274
      end
      object lblAssociationStereotype: TcxLabel
        Left = 9
        Top = 32
        Margins.Bottom = 0
        Caption = '&Stereotype'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 68
      end
      object lblAssociationClass: TcxLabel
        Left = 12
        Top = 56
        Margins.Bottom = 0
        Caption = 'Link C&lass'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 66
      end
      object lblAssociationName: TcxLabel
        Left = 35
        Top = 8
        Margins.Bottom = 0
        Caption = '&Name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 68
      end
      object bcbPersistent: TcxBoldCheckBox
        Left = 352
        Top = 2
        Hint = 
          '|Copntrols if the association is persistent (saved in persistent' +
          ' storage)'
        HelpContext = 1270
        TabStop = False
        Anchors = [akTop, akRight]
        Caption = '&Persistent'
        DataBinding.BoldHandle = behAssociation
        DataBinding.BoldProperties.Expression = 'persistent'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.Renderer = brPersistent
        ParentFont = False
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 0
      end
      object bcbAssociationDerived: TcxBoldCheckBox
        Left = 352
        Top = 18
        Hint = '|Controls if the association is derived.'
        HelpContext = 1270
        TabStop = False
        Anchors = [akTop, akRight]
        Caption = 'Der&ived'
        DataBinding.BoldHandle = behAssociation
        DataBinding.BoldProperties.Expression = 'derived'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.Renderer = brDerived
        ParentFont = False
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 2
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 478
    Width = 481
    Height = 41
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      481
      41)
    object CancelBtn: TButton
      Left = 390
      Top = 7
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object OKBtn: TButton
      Left = 309
      Top = 7
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 111
    Width = 481
    Height = 367
    ActivePage = AssociationEnd1
    Align = alClient
    TabOrder = 1
    object AssociationEnd1: TTabSheet
      Caption = 'AssociationEnd1'
      DesignSize = (
        473
        339)
      object btAssoEndShowDeriExprEditor1: TButton
        Left = 299
        Top = 208
        Width = 17
        Height = 21
        Hint = '|Edit the derivation expression for this role.'
        HelpContext = 24
        Anchors = [akTop, akRight]
        Caption = #188
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Symbol'
        Font.Style = []
        ParentFont = False
        TabOrder = 14
        OnClick = btAssoEndShowDeriExprEditor1Click
      end
      object cmbAssoEndClass: TcxBoldComboBox
        Left = 88
        Top = 64
        Hint = '|Class where the role ends.'
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'type'
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllClasses
        Properties.BoldRowProperties.Expression = ''
        Properties.DropDownListStyle = lsEditFixedList
        TabOrder = 5
        Width = 227
      end
      object tbxAssoEndDerivationOCL: TcxBoldTextEdit
        Left = 88
        Top = 208
        HelpContext = 1180
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DerivationOCL'#39'].value'
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
        TabOrder = 11
        Width = 210
      end
      object cmbAssoEndDeleteAction: TcxBoldComboBox
        Left = 88
        Top = 184
        HelpContext = 1300
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DeleteAction'#39'].value'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.bchDeleteActions
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownListStyle = lsFixedList
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 10
        Width = 227
      end
      object cmbMultiplicity: TcxBoldComboBox
        Left = 88
        Top = 87
        HelpContext = 1280
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'multiplicity'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.bchMultiplicityValues
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownListStyle = lsFixedList
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 6
        Width = 227
      end
      object cmbAssoEndChangeability: TcxBoldComboBox
        Left = 88
        Top = 160
        HelpContext = 1310
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'changeability'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllChangeabilityKind
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownListStyle = lsFixedList
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 9
        Width = 227
      end
      object cmbAssoEndVisibility: TcxBoldComboBox
        Left = 88
        Top = 136
        HelpContext = 1250
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'visibility'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllVisibilityKind
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownListStyle = lsFixedList
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 8
        Width = 227
      end
      object cmbAggregationKind: TcxBoldComboBox
        Left = 88
        Top = 112
        HelpContext = 1290
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'aggregation'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllAggregationKind
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownListStyle = lsFixedList
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 7
        Width = 227
      end
      object tbxAssociationEndStereotype: TcxBoldTextEdit
        Left = 88
        Top = 40
        HelpContext = 1150
        DataBinding.BoldHandle = behAssociationEnd1
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
        Width = 227
      end
      object tbxAssociationEndColumnName: TcxBoldTextEdit
        Left = 88
        Top = 281
        HelpContext = 43
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.ColumnName'#39'].value'
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
        TabOrder = 26
        Width = 227
      end
      object tbxAssociationEndExpressionName: TcxBoldTextEdit
        Left = 88
        Top = 257
        HelpContext = 42
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
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
        TabOrder = 22
        Width = 227
      end
      object tbxAssociationEndDelphiName: TcxBoldTextEdit
        Left = 88
        Top = 233
        HelpContext = 4
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
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
        TabOrder = 18
        Width = 227
      end
      object tbxAssociationEndName: TcxBoldTextEdit
        Left = 88
        Top = 16
        HelpContext = 51
        DataBinding.BoldHandle = behAssociationEnd1
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
        Width = 227
      end
      object lblAssoEndDerivationOCL: TcxLabel
        Left = 3
        Top = 212
        Margins.Bottom = 0
        Caption = 'Derivation OCL'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object lblAssoEndDeleteAction: TcxLabel
        Left = 12
        Top = 188
        Margins.Bottom = 0
        Caption = '&Delete action'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object lbMultiplicity: TcxLabel
        Left = 23
        Top = 92
        Margins.Bottom = 0
        Caption = 'Multi&plicity'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object lblAssoEndChangeability: TcxLabel
        Left = 9
        Top = 164
        Margins.Bottom = 0
        Caption = 'Changeability'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object lblAssoEndVisibility: TcxLabel
        Left = 37
        Top = 140
        Margins.Bottom = 0
        Caption = 'Visibility'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object Label2: TcxLabel
        Left = 15
        Top = 116
        Margins.Bottom = 0
        Caption = 'A&ggregation'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object lblStereotype: TcxLabel
        Left = 25
        Top = 44
        Margins.Bottom = 0
        Caption = '&Stereotype'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object lblAssociationEndClass: TcxLabel
        Left = 54
        Top = 68
        Margins.Bottom = 0
        Caption = 'C&lass'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object lblAssociationEndColumnName: TcxLabel
        Left = 9
        Top = 285
        Margins.Bottom = 0
        Caption = 'C&olumn name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object lblAssociationEndExpressionName: TcxLabel
        Left = -6
        Top = 261
        Margins.Bottom = 0
        Caption = 'E&xpression name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object lblAssociationEndDelphiName: TcxLabel
        Left = 15
        Top = 237
        Margins.Bottom = 0
        Caption = 'Delph&i name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object lblAssociationEndName: TcxLabel
        Left = 51
        Top = 20
        Margins.Bottom = 0
        Caption = '&Name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object bcbAssociationEndEmbed: TcxBoldCheckBox
        Left = 322
        Top = 50
        Hint = '|Controls if the role is embedded in the owning object.'
        HelpContext = 48
        TabStop = False
        Anchors = [akTop, akRight]
        Caption = 'Em&bed'
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.Embed'#39'].value'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        ParentFont = False
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 4
      end
      object bcbAssociationEndOrdered: TcxBoldCheckBox
        Left = 322
        Top = 32
        Hint = '|Controls if order is preserved.'
        HelpContext = 46
        TabStop = False
        Anchors = [akTop, akRight]
        Caption = 'O&rdered'
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'isOrdered'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        ParentFont = False
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 2
      end
      object bcbAssociationEndNavigable: TcxBoldCheckBox
        Left = 322
        Top = 14
        Hint = '|Controls if the role is navigable in this direction.'
        HelpContext = 44
        TabStop = False
        Anchors = [akTop, akRight]
        Caption = 'N&avigable'
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'isNavigable'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        ParentFont = False
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 0
      end
      object cxLabel14: TcxLabel
        Left = 35
        Top = 309
        Margins.Bottom = 0
        Caption = 'Qualifier'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 83
      end
      object cxBoldButtonEdit1: TcxBoldButtonEdit
        Left = 88
        Top = 308
        DataBinding.BoldHandle = behAssociationEnd1
        DataBinding.BoldProperties.Expression = 'qualifier->asCommaText'
        ParentFont = False
        Properties.Buttons = <
          item
            Default = True
            Kind = bkEllipsis
          end>
        Properties.OnButtonClick = cxBoldButtonEdit1PropertiesButtonClick
        TabOrder = 28
        Width = 231
      end
    end
    object AssociationEnd2: TTabSheet
      Caption = 'AssociationEnd2'
      ImageIndex = 1
      DesignSize = (
        473
        339)
      object btAssoEndShowDeriExprEditor2: TButton
        Left = 299
        Top = 208
        Width = 17
        Height = 21
        Hint = '|Edit the derivation expression for this role.'
        HelpContext = 24
        Anchors = [akTop, akRight]
        Caption = #188
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Symbol'
        Font.Style = []
        ParentFont = False
        TabOrder = 14
        OnClick = btAssoEndShowDeriExprEditor2Click
      end
      object cxBoldComboBox1: TcxBoldComboBox
        Left = 88
        Top = 64
        Hint = '|Class where the role ends.'
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'type'
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllClasses
        Properties.BoldRowProperties.Expression = ''
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 5
        Width = 231
      end
      object cxBoldTextEdit1: TcxBoldTextEdit
        Left = 88
        Top = 208
        HelpContext = 1180
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DerivationOCL'#39'].value'
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
        TabOrder = 11
        Width = 210
      end
      object cxBoldComboBox2: TcxBoldComboBox
        Left = 88
        Top = 184
        HelpContext = 1300
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DeleteAction'#39'].value'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.bchDeleteActions
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 10
        Width = 227
      end
      object cxBoldComboBox3: TcxBoldComboBox
        Left = 88
        Top = 87
        HelpContext = 1280
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'multiplicity'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.bchMultiplicityValues
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 6
        Width = 227
      end
      object cxBoldComboBox4: TcxBoldComboBox
        Left = 88
        Top = 160
        HelpContext = 1310
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'changeability'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllChangeabilityKind
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 9
        Width = 227
      end
      object cxBoldComboBox5: TcxBoldComboBox
        Left = 88
        Top = 136
        HelpContext = 1250
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'visibility'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllVisibilityKind
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 8
        Width = 227
      end
      object cxBoldComboBox6: TcxBoldComboBox
        Left = 88
        Top = 112
        HelpContext = 1290
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'aggregation'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        ParentFont = False
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllAggregationKind
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 7
        Width = 227
      end
      object cxBoldTextEdit3: TcxBoldTextEdit
        Left = 88
        Top = 40
        HelpContext = 1150
        DataBinding.BoldHandle = behAssociationEnd2
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
        Width = 227
      end
      object cxBoldTextEdit4: TcxBoldTextEdit
        Left = 88
        Top = 281
        HelpContext = 43
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.ColumnName'#39'].value'
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
        TabOrder = 26
        Width = 227
      end
      object cxBoldTextEdit5: TcxBoldTextEdit
        Left = 88
        Top = 257
        HelpContext = 42
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
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
        TabOrder = 22
        Width = 227
      end
      object cxBoldTextEdit6: TcxBoldTextEdit
        Left = 88
        Top = 233
        HelpContext = 4
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
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
        TabOrder = 18
        Width = 227
      end
      object cxBoldTextEdit7: TcxBoldTextEdit
        Left = 88
        Top = 16
        HelpContext = 51
        DataBinding.BoldHandle = behAssociationEnd2
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
        Width = 227
      end
      object cxLabel1: TcxLabel
        Left = 3
        Top = 212
        Margins.Bottom = 0
        Caption = 'Derivation OCL'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object cxLabel2: TcxLabel
        Left = 12
        Top = 188
        Margins.Bottom = 0
        Caption = '&Delete action'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object cxLabel3: TcxLabel
        Left = 23
        Top = 92
        Margins.Bottom = 0
        Caption = 'Multi&plicity'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object cxLabel5: TcxLabel
        Left = 9
        Top = 164
        Margins.Bottom = 0
        Caption = 'Changeability'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object cxLabel6: TcxLabel
        Left = 37
        Top = 140
        Margins.Bottom = 0
        Caption = 'Visibility'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object cxLabel7: TcxLabel
        Left = 15
        Top = 116
        Margins.Bottom = 0
        Caption = 'A&ggregation'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object cxLabel8: TcxLabel
        Left = 25
        Top = 44
        Margins.Bottom = 0
        Caption = '&Stereotype'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object cxLabel9: TcxLabel
        Left = 54
        Top = 68
        Margins.Bottom = 0
        Caption = 'C&lass'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object cxLabel10: TcxLabel
        Left = 9
        Top = 285
        Margins.Bottom = 0
        Caption = 'C&olumn name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object cxLabel11: TcxLabel
        Left = -6
        Top = 261
        Margins.Bottom = 0
        Caption = 'E&xpression name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object cxLabel12: TcxLabel
        Left = 15
        Top = 237
        Margins.Bottom = 0
        Caption = 'Delph&i name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object cxLabel13: TcxLabel
        Left = 51
        Top = 20
        Margins.Bottom = 0
        Caption = '&Name'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 84
      end
      object BoldCheckBox5: TcxBoldCheckBox
        Left = 322
        Top = 50
        Hint = '|Controls if the role is embedded in the owning object.'
        HelpContext = 48
        TabStop = False
        Anchors = [akTop, akRight]
        Caption = 'Em&bed'
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.Embed'#39'].value'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        ParentFont = False
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 4
      end
      object BoldCheckBox3: TcxBoldCheckBox
        Left = 322
        Top = 32
        Hint = '|Controls if order is preserved.'
        HelpContext = 46
        TabStop = False
        Anchors = [akTop, akRight]
        Caption = 'O&rdered'
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'isOrdered'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        ParentFont = False
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 2
      end
      object BoldCheckBox1: TcxBoldCheckBox
        Left = 322
        Top = 14
        Hint = '|Controls if the role is navigable in this direction.'
        HelpContext = 44
        TabStop = False
        Anchors = [akTop, akRight]
        Caption = 'N&avigable'
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'isNavigable'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        ParentFont = False
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 0
      end
      object cxLabel15: TcxLabel
        Left = 35
        Top = 309
        Margins.Bottom = 0
        Caption = 'Qualifier'
        ParentFont = False
        Style.BorderStyle = ebsNone
        Properties.Alignment.Horz = taRightJustify
        Transparent = True
        AnchorX = 83
      end
      object cxBoldButtonEdit2: TcxBoldButtonEdit
        Left = 88
        Top = 308
        DataBinding.BoldHandle = behAssociationEnd2
        DataBinding.BoldProperties.Expression = 'qualifier->asCommaText'
        ParentFont = False
        Properties.Buttons = <
          item
            Default = True
            Kind = bkEllipsis
          end>
        Properties.ReadOnly = True
        Properties.OnButtonClick = cxBoldButtonEdit1PropertiesButtonClick
        TabOrder = 28
        Width = 231
      end
    end
  end
  object behAssociation: TBoldExpressionHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = brhAssociation
    Left = 376
    Top = 356
  end
  object behAssociationEnd1: TBoldExpressionHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = behAssociation
    Expression = 'connection->first'
    Left = 376
    Top = 253
  end
  object behAssociationEnd2: TBoldExpressionHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = behAssociation
    Expression = 'connection->last'
    Left = 376
    Top = 301
  end
  object brhAssociation: TBoldReferenceHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    StaticValueTypeName = 'UMLAssociation'
    Left = 374
    Top = 411
  end
  object BoldPropertyMapper1: TBoldPropertyMapper
    MappingCollection = <
      item
        BoldHandle = behAssociation
        BoldProperties.Expression = 'derived'
        VCLComponent = cxBoldTextEdit1
        VCLProperty = 'Enabled'
      end
      item
        BoldHandle = behAssociation
        BoldProperties.Expression = 'derived'
        VCLComponent = tbxAssoEndDerivationOCL
        VCLProperty = 'Enabled'
      end
      item
        BoldHandle = behAssociation
        BoldProperties.Expression = 'derived'
        VCLComponent = btAssoEndShowDeriExprEditor1
        VCLProperty = 'Enabled'
      end
      item
        BoldHandle = behAssociation
        BoldProperties.Expression = 'derived'
        VCLComponent = btAssoEndShowDeriExprEditor2
        VCLProperty = 'Enabled'
      end>
    Enabled = True
    Left = 420
    Top = 189
  end
  object brPersistent: TBoldAsVariantRenderer
    OnSetAsVariant = brPersistentSetAsVariant
    Left = 420
    Top = 40
  end
  object brDerived: TBoldAsVariantRenderer
    OnSetAsVariant = brDerivedSetAsVariant
    Left = 420
    Top = 88
  end
  object blhQualifierAttributes1: TBoldListHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = behAssociationEnd1
    Expression = 'type.allFeature->filterOnType(UMLAttribute)'
    Left = 100
    Top = 469
  end
  object blhQualifierAttributes2: TBoldListHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = behAssociationEnd2
    Expression = 'type.allFeature->filterOnType(UMLAttribute)'
    Left = 172
    Top = 469
  end
  object brQualifier: TBoldAsVariantRenderer
    OnSetAsVariant = brQualifierSetAsVariant
    Left = 36
    Top = 469
  end
end
