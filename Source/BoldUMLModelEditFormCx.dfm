object BoldModelEditFrmCx: TBoldModelEditFrmCx
  Left = 352
  Top = 156
  BorderWidth = 2
  Caption = 'Bold Model Editor'
  ClientHeight = 793
  ClientWidth = 1322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF001111
    1111111111111111111111111111111111111111111111111111111111111111
    1111111111111111111111111111111111111111111111111111111111111111
    1111111111111111111111111111111111111111111111111111111111111111
    1111111111111111111111111111111111111111111111111111111111111111
    1111111111111111111111111111111111111111111111111111111111111111
    1111111111111111111111111111111111111111111111111111111111111111
    1111111111111111111111111111111111111111111111111111111111111111
    11111111111111111111111111111111111111111111111111111111111111FF
    FFF11111FFF111FF711FFF1FF71111FFFFFF111FFFFF11FF81FFFFFFF81111FF
    11FFF1FF818FF1FF71FF91FFF71111FF111FF1FF119FF1FF98FF111FF81111FF
    111FF1FF111FF1FF7FF8111FF71111FF111FF1FF111FF1FF9FF8111FF81111FF
    FFFF11FF119FF1FF78FF111FF71111FFFFF111FF81FFF1FF99FF118FF81111FF
    11FF111FFFFF11FF71FFFFFFF71111FF111FF111FFF111FF917FFF1FF81111FF
    111FF111111111FF7111111FF71111FF91FF9111111111FF9111111FF81111FF
    FFFF1111111111FF7111111FF71111FFFF991111111111FF9111111FF8111111
    1111111111111111111111111111111111111111111111111111111111110000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  KeyPreview = True
  Menu = MainMenu1
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  TextHeight = 13
  object Splitter9: TSplitter
    Left = 0
    Top = 588
    Width = 1322
    Height = 6
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 774
    Width = 1322
    Height = 19
    Hint = 'Doubleclick here to validate model'
    Panels = <
      item
        Width = 30
      end
      item
        Width = 50
      end>
  end
  object pcLeft: TPageControl
    Left = 0
    Top = 28
    Width = 1322
    Height = 560
    ActivePage = tsDataTypes2
    Align = alClient
    TabOrder = 1
    object tsModel: TTabSheet
      Caption = 'Model'
      ImageIndex = 1
      object lblModelName: TLabel
        Left = 80
        Top = 20
        Width = 28
        Height = 13
        Alignment = taRightJustify
        Caption = '&Name'
        FocusControl = tbxModelName
      end
      object lblModelDelphiName: TLabel
        Left = 60
        Top = 68
        Width = 48
        Height = 13
        Alignment = taRightJustify
        Caption = '&Unit name'
        FocusControl = tbxModelDelhpiName
      end
      object lblModelExpressionName: TLabel
        Left = 28
        Top = 92
        Width = 80
        Height = 13
        Alignment = taRightJustify
        Caption = 'E&xpression name'
        FocusControl = tbxModelExpressionName
      end
      object lblModelPMapperName: TLabel
        Left = 36
        Top = 284
        Width = 72
        Height = 13
        Alignment = taRightJustify
        Caption = '&PMapper name'
        FocusControl = tbxModelPMapperName
      end
      object lblModelInterfaceUses: TLabel
        Left = 41
        Top = 116
        Width = 67
        Height = 13
        Alignment = taRightJustify
        Caption = '&Interface uses'
        FocusControl = tbxModelInterfaceUses
      end
      object lblModelImplementationUses: TLabel
        Left = 12
        Top = 140
        Width = 96
        Height = 13
        Alignment = taRightJustify
        Caption = 'Imp&lementation uses'
        FocusControl = tbxModelImplementationUses
      end
      object Label1: TLabel
        Left = 31
        Top = 164
        Width = 77
        Height = 13
        Caption = 'Model &root class'
      end
      object lblModelStereotype: TLabel
        Left = 57
        Top = 44
        Width = 51
        Height = 13
        Alignment = taRightJustify
        Caption = 'Stereot&ype'
        FocusControl = tbxModelStereotype
      end
      object lblModelConstraints: TLabel
        Left = 56
        Top = 212
        Width = 52
        Height = 13
        Alignment = taRightJustify
        Caption = 'C&onstraints'
        FocusControl = tbxModelConstraints
      end
      object lblModelGUID: TLabel
        Left = 81
        Top = 236
        Width = 27
        Height = 13
        Alignment = taRightJustify
        Caption = 'GUI&D'
        FocusControl = tbxModelConstraints
      end
      object lblModelTypeLibVersion: TLabel
        Left = 34
        Top = 260
        Width = 74
        Height = 13
        Alignment = taRightJustify
        Caption = 'Type li&b version'
        FocusControl = tbxModelConstraints
      end
      object lblModelOptimisticLocking: TLabel
        Left = 22
        Top = 188
        Width = 86
        Height = 13
        Alignment = taRightJustify
        Caption = 'Optimistic Loc&king'
        FocusControl = tbxModelConstraints
      end
      object tbxModelName: TBoldEdit
        Left = 116
        Top = 16
        Width = 291
        Height = 21
        Hint = '|The name of the model.'
        HelpContext = 33
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'name'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        MaxLength = 0
        TabOrder = 1
      end
      object tbxModelDelhpiName: TBoldEdit
        Left = 116
        Top = 64
        Width = 291
        Height = 21
        Hint = '|Filename used to save model.'
        HelpContext = 1130
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.UnitName'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        MaxLength = 0
        TabOrder = 6
      end
      object tbxModelExpressionName: TBoldEdit
        Left = 116
        Top = 88
        Width = 291
        Height = 21
        Hint = '|The expression name of the model. Can mostly be left unchanged.'
        HelpContext = 11
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        MaxLength = 0
        TabOrder = 8
      end
      object tbxModelPMapperName: TBoldEdit
        Left = 116
        Top = 280
        Width = 291
        Height = 21
        Hint = 
          '|Persistence mapper for the model. There is no reason to change ' +
          'from <Default>.'
        HelpContext = 12
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.PMapper'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        MaxLength = 0
        TabOrder = 20
      end
      object tbxModelInterfaceUses: TBoldEdit
        Left = 116
        Top = 112
        Width = 271
        Height = 21
        Hint = '|The units required by the interface section.'
        HelpContext = 13
        TabStop = False
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.InterfaceUses'#39'].value'
        BoldProperties.Renderer = dmBoldUMLModelEditorHandles.bsrNiceCRRenderer
        ReadOnly = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        MaxLength = 0
        TabOrder = 10
      end
      object tbxModelImplementationUses: TBoldEdit
        Left = 116
        Top = 136
        Width = 271
        Height = 21
        Hint = '|The units required by the implementation section.'
        HelpContext = 14
        TabStop = False
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.ImplementationUses'#39'].value'
        BoldProperties.Renderer = dmBoldUMLModelEditorHandles.bsrNiceCRRenderer
        ReadOnly = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        MaxLength = 0
        TabOrder = 12
      end
      object btInterfaceUses: TButton
        Left = 390
        Top = 112
        Width = 17
        Height = 21
        Hint = '|Edit the interface uses list.'
        HelpContext = 82
        Caption = #188
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Symbol'
        Font.Style = []
        ParentFont = False
        TabOrder = 11
        OnClick = btInterfaceUsesClick
      end
      object btImplementationUses: TButton
        Left = 390
        Top = 136
        Width = 17
        Height = 21
        Hint = '|Edit the implementation uses list.'
        HelpContext = 24
        Caption = #188
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Symbol'
        Font.Style = []
        ParentFont = False
        TabOrder = 13
        OnClick = btImplementationUsesClick
      end
      object bcbModelUseXFiles: TBoldCheckBox
        Left = 414
        Top = 14
        Width = 77
        Height = 17
        Hint = 
          '|If checked, a table for permanent ID recodring will be used. Re' +
          'quired for OLLE.'
        HelpContext = 27
        TabStop = False
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.UseXFiles'#39'].value'
        BoldProperties.ApplyPolicy = bapChange
        BoldProperties.Renderer = dmBoldUMLModelEditorHandles.bcrBooleanToCheckBox
        Caption = 'U&se X files'
        ReadOnly = False
        TabOrder = 0
      end
      object bcbModelUseTimestamp: TBoldCheckBox
        Left = 414
        Top = 30
        Width = 93
        Height = 17
        Hint = 
          '|If checked, a time stamp column will be added to the XFiles tab' +
          'le.'
        HelpContext = 28
        TabStop = False
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.UseTimestamp'#39'].value'
        BoldProperties.ApplyPolicy = bapChange
        BoldProperties.Renderer = dmBoldUMLModelEditorHandles.bcrBooleanToCheckBox
        Caption = 'Use timestamp'
        ReadOnly = False
        TabOrder = 2
      end
      object bcbUseGlobalId: TBoldCheckBox
        Left = 414
        Top = 46
        Width = 93
        Height = 17
        Hint = '|If checked, a GUID column will be added to the XFiles table.'
        HelpContext = 29
        TabStop = False
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.UseGlobalId'#39'].value'
        BoldProperties.ApplyPolicy = bapChange
        BoldProperties.Renderer = dmBoldUMLModelEditorHandles.bcrBooleanToCheckBox
        Caption = 'Use global id'
        ReadOnly = False
        TabOrder = 4
      end
      object bcbUseReadOnly: TBoldCheckBox
        Left = 414
        Top = 62
        Width = 93
        Height = 17
        Hint = 
          '|If checked, a readonly column will be added to the XFiles table' +
          '.'
        HelpContext = 30
        TabStop = False
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.UseReadOnly'#39'].value'
        BoldProperties.ApplyPolicy = bapChange
        BoldProperties.Renderer = dmBoldUMLModelEditorHandles.bcrBooleanToCheckBox
        Caption = 'Use readonly'
        ReadOnly = False
        TabOrder = 5
      end
      object tbxModelStereotype: TBoldEdit
        Left = 116
        Top = 40
        Width = 291
        Height = 21
        Hint = '|Stereotype applied to model.'
        HelpContext = 1150
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'stereotypeName'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        MaxLength = 0
        TabOrder = 3
      end
      object tbxModelConstraints: TBoldEdit
        Left = 116
        Top = 208
        Width = 271
        Height = 21
        Hint = 
          '|Constraints for the model. Note that these are not enforced aut' +
          'omatically.'
        HelpContext = 1160
        TabStop = False
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'constraint'
        BoldProperties.Renderer = dmBoldUMLModelEditorHandles.bsrNiceCRRenderer
        ReadOnly = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        MaxLength = 0
        TabOrder = 16
      end
      object btModelConstraintEditor: TButton
        Left = 390
        Top = 208
        Width = 17
        Height = 21
        Hint = '|Edit the model'#39's constraints'
        HelpContext = 24
        Caption = #188
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Symbol'
        Font.Style = []
        ParentFont = False
        TabOrder = 17
        OnClick = btModelConstraintEditorClick
      end
      object edModelGUID: TBoldEdit
        Left = 116
        Top = 232
        Width = 291
        Height = 21
        Hint = 'GUID used when generating IDL code.'
        HelpContext = 11
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.GUID'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        MaxLength = 0
        TabOrder = 18
      end
      object edModelTypeLibVersion: TBoldEdit
        Left = 116
        Top = 256
        Width = 291
        Height = 21
        Hint = '|Version number for the type lib.'
        HelpContext = 11
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.TypeLibVersion'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        MaxLength = 0
        TabOrder = 19
      end
      object bcbUseClockLog: TBoldCheckBox
        Left = 414
        Top = 78
        Width = 93
        Height = 17
        Hint = 
          '|If checked a table mapping "technical time" to "real time" will' +
          ' be maintained.'
        HelpContext = 30
        TabStop = False
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.UseClockLog'#39'].value'
        BoldProperties.ApplyPolicy = bapChange
        BoldProperties.Renderer = dmBoldUMLModelEditorHandles.bcrBooleanToCheckBox
        Caption = 'Use clock lo&g'
        ReadOnly = False
        TabOrder = 7
      end
      object bcbGenerateMultiplicityConstraints: TBoldCheckBox
        Left = 414
        Top = 94
        Width = 157
        Height = 17
        Hint = 
          '|If checked, constraints for multiplicity will be automatically ' +
          'generated.'
        HelpContext = 30
        TabStop = False
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.GenerateMultiplicityConstraints'#39'].value'
        BoldProperties.ApplyPolicy = bapChange
        BoldProperties.Renderer = dmBoldUMLModelEditorHandles.bcrBooleanToCheckBox
        Caption = 'Use multiplicity constraints'
        ReadOnly = False
        TabOrder = 9
      end
      object cmbModelOptimisticLocking: TBoldComboBox
        Left = 116
        Top = 184
        Width = 291
        Height = 21
        Hint = 
          '|Specifies what kind of optimistic locking mechanism will be use' +
          'd.'
        HelpContext = 1250
        Alignment = taLeftJustify
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldListHandle = dmBoldUMLModelEditorHandles.bchOptimisticLocking
        BoldProperties.Expression = 'taggedValue['#39'Bold.OptimisticLocking'#39'].value'
        BoldProperties.ApplyPolicy = bapChange
        BoldRowProperties.Expression = ''
        BoldSelectChangeAction = bdscSetText
        Style = csDropDownList
        TabOrder = 15
      end
      object tbxModelRootClass: TBoldEdit
        Left = 116
        Top = 160
        Width = 291
        Height = 21
        Hint = '|The root class of the model.'
        HelpContext = 12
        BoldHandle = dmBoldUMLModelEditorHandles.behModel
        BoldProperties.Expression = 'taggedValue['#39'Bold.RootClass'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        MaxLength = 0
        TabOrder = 14
      end
    end
    object tsPackages: TTabSheet
      Caption = 'Packages'
      ImageIndex = 3
      object cxGridPackages: TcxGrid
        Left = 0
        Top = 0
        Width = 1314
        Height = 532
        Align = alClient
        TabOrder = 0
        object tvPackages: TcxGridBoldTableView
          PopupMenu = popTree
          Navigator.Buttons.CustomButtons = <>
          FindPanel.DisplayMode = fpdmAlways
          FindPanel.InfoText = 'Search Packages'
          FindPanel.Layout = fplCompact
          ScrollbarAnnotations.CustomAnnotations = <>
          DataController.BoldProperties.DefaultDblClick = False
          DataController.BoldHandle = dmBoldUMLModelEditorHandles.blhPackages
          DataController.Filter.Options = [fcoSoftNull]
          DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          OptionsData.Deleting = False
          OptionsData.DeletingConfirmation = False
          OptionsData.Editing = False
          OptionsData.Inserting = False
          OptionsSelection.CellSelect = False
          OptionsSelection.InvertSelect = False
          OptionsView.ColumnAutoWidth = True
          OptionsView.GridLines = glNone
          OptionsView.GroupByBox = False
          object tvPackagesname: TcxGridBoldColumn
            Caption = 'name'
            DataBinding.BoldProperties.Expression = 'name'
            PropertiesClassName = 'TcxTextEditProperties'
          end
          object tvPackagesqualifiedName: TcxGridBoldColumn
            Caption = 'qualifiedName'
            DataBinding.BoldProperties.Expression = 'qualifiedName'
            PropertiesClassName = 'TcxTextEditProperties'
          end
          object tvPackagesnamespace: TcxGridBoldColumn
            Caption = 'namespace'
            DataBinding.BoldProperties.Expression = 'namespace'
          end
          object tvPackagesmodel: TcxGridBoldColumn
            Caption = 'model'
            DataBinding.BoldProperties.Expression = 'model'
          end
          object tvPackagesqualifyingOwner: TcxGridBoldColumn
            Caption = 'qualifyingOwner'
            DataBinding.BoldProperties.Expression = 'qualifyingOwner'
          end
        end
        object lvlPackages: TcxGridLevel
          GridView = tvPackages
        end
      end
    end
    object tsClasses: TTabSheet
      Caption = 'Classes'
      ImageIndex = 1
      object Splitter10: TSplitter
        Left = 224
        Top = 0
        Width = 6
        Height = 532
        Beveled = True
      end
      object pnlLeft: TPanel
        Left = 0
        Top = 0
        Width = 224
        Height = 532
        Align = alLeft
        Caption = 'pnlLeft'
        TabOrder = 0
        object cxGridClasses: TcxGrid
          Left = 1
          Top = 1
          Width = 222
          Height = 400
          Align = alClient
          TabOrder = 0
          object tvClasses: TcxGridBoldTableView
            PopupMenu = popTree
            Navigator.Buttons.CustomButtons = <>
            FindPanel.ApplyInputDelay = 600
            FindPanel.DisplayMode = fpdmAlways
            FindPanel.InfoText = 'Search Classes'
            FindPanel.Layout = fplCompact
            ScrollbarAnnotations.CustomAnnotations = <>
            DataController.BoldProperties.DefaultDblClick = False
            DataController.BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
            DataController.Filter.Options = [fcoSoftNull]
            DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <
              item
                Kind = skCount
                Column = tvClassesname
              end>
            DataController.Summary.SummaryGroups = <>
            OptionsBehavior.IncSearch = True
            OptionsCustomize.ColumnFiltering = False
            OptionsCustomize.ColumnGrouping = False
            OptionsCustomize.ColumnMoving = False
            OptionsCustomize.ColumnSorting = False
            OptionsData.Deleting = False
            OptionsData.DeletingConfirmation = False
            OptionsData.Editing = False
            OptionsData.Inserting = False
            OptionsSelection.CellSelect = False
            OptionsView.ColumnAutoWidth = True
            OptionsView.Footer = True
            OptionsView.GridLines = glNone
            OptionsView.GroupByBox = False
            OptionsView.Header = False
            OptionsView.Indicator = True
            object tvClassesConstraints: TcxGridBoldColumn
              Caption = #167
              DataBinding.ValueType = 'Boolean'
              DataBinding.BoldProperties.Expression = 'constraints->select(c|not c)->size = 0'
              BestFitMaxWidth = 16
              Options.Editing = False
              Options.Focusing = False
              Options.IncSearch = False
              Options.HorzSizing = False
              Options.Moving = False
              Width = 20
            end
            object tvClassesname: TcxGridBoldColumn
              Caption = 'name'
              DataBinding.BoldProperties.Expression = 'name'
              PropertiesClassName = 'TcxTextEditProperties'
            end
            object tvClassesisAbstract: TcxGridBoldColumn
              Caption = 'isAbstract'
              DataBinding.ValueType = 'Boolean'
              DataBinding.BoldProperties.Expression = 'isAbstract'
              PropertiesClassName = 'TcxCheckBoxProperties'
              Visible = False
            end
            object tvClassespersistent: TcxGridBoldColumn
              Caption = 'persistent'
              DataBinding.ValueType = 'Boolean'
              DataBinding.BoldProperties.Expression = 'persistent'
              PropertiesClassName = 'TcxCheckBoxProperties'
              Visible = False
            end
            object tvClassessuperclass: TcxGridBoldColumn
              Caption = 'superclass'
              DataBinding.BoldProperties.Expression = 'superclass'
              Visible = False
            end
            object tvClassesisAssociationClass: TcxGridBoldColumn
              Caption = 'isAssociationClass'
              DataBinding.ValueType = 'Boolean'
              DataBinding.BoldProperties.Expression = 'isAssociationClass'
              PropertiesClassName = 'TcxCheckBoxProperties'
              Visible = False
            end
            object tvClassesassociation: TcxGridBoldColumn
              Caption = 'association'
              DataBinding.BoldProperties.Expression = 'association'
              Visible = False
            end
            object tvClassesmodel: TcxGridBoldColumn
              Caption = 'model'
              DataBinding.BoldProperties.Expression = 'model'
              Visible = False
            end
          end
          object cxLevelClasses: TcxGridLevel
            GridView = tvClasses
          end
        end
        object pnlClassFilter: TPanel
          Left = 1
          Top = 401
          Width = 222
          Height = 130
          Align = alBottom
          BorderWidth = 4
          TabOrder = 1
          object cxCheckGroup1: TcxCheckGroup
            Left = 5
            Top = 5
            TabStop = False
            Align = alBottom
            Caption = 'Class Filter'
            ParentFont = False
            Properties.Items = <
              item
                Caption = 'Persistent'
              end
              item
                Caption = 'Transient'
              end
              item
                Caption = 'Abstract'
              end
              item
                Caption = 'Link Class'
              end>
            Properties.OnChange = cxCheckGroup1PropertiesChange
            TabOrder = 0
            Height = 120
            Width = 212
          end
        end
      end
      object Panel2: TPanel
        Left = 230
        Top = 0
        Width = 1084
        Height = 532
        Align = alClient
        TabOrder = 1
        object pgClient: TPageControl
          Left = 1
          Top = 1
          Width = 1082
          Height = 530
          ActivePage = tsClass
          Align = alClient
          TabOrder = 0
          object tsClass: TTabSheet
            Caption = 'Class'
            ImageIndex = 6
            object lblClassName: TLabel
              Left = 84
              Top = 36
              Width = 28
              Height = 13
              Alignment = taRightJustify
              Caption = '&Name'
              FocusControl = tbxClassName
            end
            object lblClassDelphiName: TLabel
              Left = 54
              Top = 332
              Width = 59
              Height = 13
              Alignment = taRightJustify
              Caption = 'Delph&i name'
              FocusControl = tbxClassDelphiName
            end
            object lblClassExpressionName: TLabel
              Left = 33
              Top = 356
              Width = 80
              Height = 13
              Alignment = taRightJustify
              Caption = 'E&xpression name'
              FocusControl = tbxClassExpressionName
            end
            object lblClassPMapperName: TLabel
              Left = 41
              Top = 380
              Width = 72
              Height = 13
              Alignment = taRightJustify
              Caption = '&PMapper name'
              FocusControl = cmbClassPMapperName
            end
            object lblClassTableName: TLabel
              Left = 57
              Top = 404
              Width = 56
              Height = 13
              Alignment = taRightJustify
              Caption = 'T&able name'
              FocusControl = tbxClassTableName
            end
            object lblClassFileName: TLabel
              Left = 33
              Top = 164
              Width = 80
              Height = 13
              Alignment = taRightJustify
              Caption = 'Include file name'
              FocusControl = tbxClassFileName
            end
            object lblClassTableMapping: TLabel
              Left = 43
              Top = 212
              Width = 70
              Height = 13
              Alignment = taRightJustify
              Caption = 'Table mappin&g'
              FocusControl = cmbTableMapping
            end
            object lblClassSuperclass: TLabel
              Left = 60
              Top = 85
              Width = 52
              Height = 13
              Alignment = taRightJustify
              Caption = '&Superclass'
            end
            object blbClassInfo: TBoldLabel
              Left = 120
              Top = 4
              Width = 273
              Height = 24
              AutoSize = False
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'name'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -19
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object Label4: TLabel
              Left = 68
              Top = 4
              Width = 50
              Height = 24
              Caption = 'Class:'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -19
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
            end
            object lblClassStereotype: TLabel
              Left = 61
              Top = 60
              Width = 51
              Height = 13
              Alignment = taRightJustify
              Caption = 'Stereot&ype'
              FocusControl = tbxClassStereotype
            end
            object lblClassConstraint: TLabel
              Left = 61
              Top = 260
              Width = 52
              Height = 13
              Alignment = taRightJustify
              Caption = '&Constraints'
              FocusControl = tbxClassConstraint
            end
            object lblClassDerivationExpressions: TLabel
              Left = 7
              Top = 236
              Width = 106
              Height = 13
              Alignment = taRightJustify
              Caption = 'Derivation expressions'
              FocusControl = tbxClassDerivationExpressions
            end
            object Label12: TLabel
              Left = 65
              Top = 139
              Width = 48
              Height = 13
              Alignment = taRightJustify
              Caption = 'Unit name'
              FocusControl = tbxClassUnitName
            end
            object lblClassDefaultStringRep: TLabel
              Left = 33
              Top = 188
              Width = 80
              Height = 13
              Alignment = taRightJustify
              Caption = 'Default string rep'
              FocusControl = edClassDefaultStringRep
            end
            object lblClassGUID: TLabel
              Left = 87
              Top = 284
              Width = 27
              Height = 13
              Alignment = taRightJustify
              Caption = 'GUID'
              FocusControl = tbxModelConstraints
            end
            object lblClassOptimisticLocking: TLabel
              Left = 27
              Top = 308
              Width = 86
              Height = 13
              Alignment = taRightJustify
              Caption = 'Optimistic &Locking'
              FocusControl = tbxModelConstraints
            end
            object Label2: TLabel
              Left = 50
              Top = 113
              Width = 63
              Height = 13
              Alignment = taRightJustify
              Caption = 'Superclasses'
              FocusControl = BoldEdit1
            end
            object tbxClassName: TBoldEdit
              Left = 120
              Top = 32
              Width = 291
              Height = 21
              Hint = '|Modeled name of the class.'
              HelpContext = 62
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'name'
              ReadOnly = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              Alignment = taLeftJustify
              ButtonStyle = bbsNone
              MaxLength = 0
              TabOrder = 1
            end
            object tbxClassDelphiName: TBoldEdit
              Left = 121
              Top = 328
              Width = 291
              Height = 21
              Hint = '|Template for the name used to access this class in source code.'
              HelpContext = 5
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
              ReadOnly = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              Alignment = taLeftJustify
              ButtonStyle = bbsNone
              MaxLength = 0
              TabOrder = 22
            end
            object tbxClassExpressionName: TBoldEdit
              Left = 121
              Top = 352
              Width = 291
              Height = 21
              Hint = '|Template for the name used to access the class from OCL.'
              HelpContext = 53
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
              ReadOnly = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              Alignment = taLeftJustify
              ButtonStyle = bbsNone
              MaxLength = 0
              TabOrder = 23
            end
            object tbxClassTableName: TBoldEdit
              Left = 121
              Top = 400
              Width = 291
              Height = 21
              Hint = '|Template for the name used for this class'#39' table name.'
              HelpContext = 54
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'taggedValue['#39'Bold.TableName'#39'].value'
              ReadOnly = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              Alignment = taLeftJustify
              ButtonStyle = bbsNone
              MaxLength = 0
              TabOrder = 25
            end
            object tbxClassFileName: TBoldEdit
              Left = 121
              Top = 160
              Width = 291
              Height = 21
              Hint = 
                '|Name of include file into which this class'#39' business method imp' +
                'lementation is generated.'
              HelpContext = 1140
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'taggedValue['#39'Bold.FileName'#39'].value'
              ReadOnly = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              Alignment = taLeftJustify
              ButtonStyle = bbsNone
              MaxLength = 0
              TabOrder = 12
            end
            object bcbClassPersistent: TBoldCheckBox
              Left = 418
              Top = 30
              Width = 69
              Height = 17
              Hint = '|Controls of this class is to be saved in persistent storage.'
              HelpContext = 56
              TabStop = False
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'persistent'
              BoldProperties.ApplyPolicy = bapChange
              Caption = 'Pe&rsistent'
              ReadOnly = False
              TabOrder = 0
            end
            object bcbClassAbstract: TBoldCheckBox
              Left = 418
              Top = 46
              Width = 69
              Height = 17
              Hint = '|Controls if this class can be instantiated.'
              HelpContext = 420
              TabStop = False
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'isAbstract'
              BoldProperties.ApplyPolicy = bapChange
              Caption = 'A&bstract'
              ReadOnly = False
              TabOrder = 2
            end
            object bcbClassImported: TBoldCheckBox
              Left = 418
              Top = 62
              Width = 69
              Height = 17
              HelpContext = 57
              TabStop = False
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'taggedValue['#39'Bold.Imported'#39'].value'
              BoldProperties.ApplyPolicy = bapChange
              BoldProperties.Renderer = dmBoldUMLModelEditorHandles.bcrBooleanToCheckBox
              Caption = 'Importe&d'
              ReadOnly = False
              TabOrder = 4
            end
            object cmbClassPMapperName: TBoldComboBox
              Left = 121
              Top = 376
              Width = 291
              Height = 21
              Hint = 
                '|Persistence mapper for this class. There is no reason to change' +
                ' from <Default>.'
              HelpContext = 58
              Alignment = taLeftJustify
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'taggedValue['#39'Bold.PMapper'#39'].value'
              BoldProperties.ApplyPolicy = bapChange
              BoldRowProperties.Expression = ''
              BoldSelectChangeAction = bdscSetText
              Style = csDropDownList
              TabOrder = 24
            end
            object cmbTableMapping: TBoldComboBox
              Left = 121
              Top = 208
              Width = 291
              Height = 21
              Hint = '|Controls into which table the class persistent data goes.'
              HelpContext = 1000
              Alignment = taLeftJustify
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldListHandle = dmBoldUMLModelEditorHandles.bcrTableMapping
              BoldProperties.Expression = 'taggedValue['#39'Bold.TableMapping'#39'].value'
              BoldProperties.ApplyPolicy = bapChange
              BoldRowProperties.Expression = ''
              BoldSelectChangeAction = bdscSetText
              Style = csDropDownList
              TabOrder = 15
            end
            object tbxClassStereotype: TBoldEdit
              Left = 120
              Top = 56
              Width = 291
              Height = 21
              Hint = '|Stereotype applied on class, if any.'
              HelpContext = 1150
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'stereotypeName'
              ReadOnly = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              Alignment = taLeftJustify
              ButtonStyle = bbsNone
              MaxLength = 0
              TabOrder = 3
            end
            object tbxClassConstraint: TBoldEdit
              Left = 121
              Top = 256
              Width = 272
              Height = 21
              Hint = '|Number of constraints specified for this class.'
              HelpContext = 14
              TabStop = False
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'constraint'
              BoldProperties.Renderer = dmBoldUMLModelEditorHandles.bsrNiceCRRenderer
              ReadOnly = True
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              Alignment = taLeftJustify
              ButtonStyle = bbsNone
              MaxLength = 0
              TabOrder = 18
            end
            object btClassConstraintEditor: TButton
              Left = 395
              Top = 256
              Width = 17
              Height = 21
              Hint = '|Edit the constraints for this class.'
              HelpContext = 24
              Caption = #188
              Font.Charset = SYMBOL_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Symbol'
              Font.Style = []
              ParentFont = False
              TabOrder = 19
              OnClick = btModelConstraintEditorClick
            end
            object tbxClassDerivationExpressions: TBoldEdit
              Left = 121
              Top = 232
              Width = 272
              Height = 21
              Hint = '|Redefinition of inherited derivation expressions.'
              HelpContext = 1170
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'taggedValue['#39'Bold.DerivationExpressions'#39'].value'
              ReadOnly = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              Alignment = taLeftJustify
              ButtonStyle = bbsNone
              MaxLength = 0
              TabOrder = 16
            end
            object btShowDerivationExpressionsEditor: TButton
              Left = 395
              Top = 232
              Width = 17
              Height = 21
              Hint = '|Edit inherited derived expressions.'
              HelpContext = 24
              Caption = #188
              Font.Charset = SYMBOL_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Symbol'
              Font.Style = []
              ParentFont = False
              TabOrder = 17
              OnClick = btShowDerivationExpressionsEditorClick
            end
            object tbxClassUnitName: TBoldEdit
              Left = 121
              Top = 133
              Width = 291
              Height = 21
              Hint = 
                '|The name of the unit this class will be generated into. Blank m' +
                'eans same as superclass.'
              HelpContext = 55
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'taggedValue['#39'Bold.UnitName'#39'].value'
              ReadOnly = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              Alignment = taLeftJustify
              ButtonStyle = bbsNone
              MaxLength = 0
              TabOrder = 11
            end
            object edClassDefaultStringRep: TBoldEdit
              Left = 121
              Top = 184
              Width = 272
              Height = 21
              Hint = 
                '|OCL expression for the string representation of the class objec' +
                'ts.'
              HelpContext = 1140
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'taggedValue['#39'Bold.DefaultStringRepresentation'#39'].value'
              ReadOnly = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              Alignment = taLeftJustify
              ButtonStyle = bbsNone
              MaxLength = 0
              TabOrder = 13
            end
            object btClassDefaultStringRep: TButton
              Left = 395
              Top = 184
              Width = 17
              Height = 21
              Hint = '|Edit the default string representation.'
              HelpContext = 24
              Caption = #188
              Font.Charset = SYMBOL_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Symbol'
              Font.Style = []
              ParentFont = False
              TabOrder = 14
              OnClick = btClassDefaultStringRepClick
            end
            object edClassGUID: TBoldEdit
              Left = 121
              Top = 280
              Width = 291
              Height = 21
              Hint = '|The GUID for this class (required for COM access)'
              HelpContext = 11
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'taggedValue['#39'Bold.GUID'#39'].value'
              ReadOnly = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              Alignment = taLeftJustify
              ButtonStyle = bbsNone
              MaxLength = 0
              TabOrder = 20
            end
            object cmbClassOptimisticLocking: TBoldComboBox
              Left = 121
              Top = 304
              Width = 291
              Height = 21
              Hint = '|Specify the optimistic locking mechanism to use for this class.'
              HelpContext = 1250
              Alignment = taLeftJustify
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldListHandle = dmBoldUMLModelEditorHandles.bchOptimisticLocking
              BoldProperties.Expression = 'taggedValue['#39'Bold.OptimisticLocking'#39'].value'
              BoldProperties.ApplyPolicy = bapChange
              BoldRowProperties.Expression = ''
              BoldSelectChangeAction = bdscSetText
              Style = csDropDownList
              TabOrder = 21
            end
            object bcbRemoveSuperOnUnboldify: TBoldCheckBox
              Left = 418
              Top = 87
              Width = 173
              Height = 17
              HelpContext = 57
              TabStop = False
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'generalization->first'
              BoldProperties.ApplyPolicy = bapChange
              BoldProperties.Renderer = dmBoldUMLModelEditorHandles.bcrAutoCreated
              Caption = 'Remove Superclass on unboldify'
              ReadOnly = False
              TabOrder = 7
            end
            object bcbRemoveOnUnboldify: TBoldCheckBox
              Left = 418
              Top = 104
              Width = 149
              Height = 17
              HelpContext = 57
              TabStop = False
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = ''
              BoldProperties.ApplyPolicy = bapChange
              BoldProperties.Renderer = dmBoldUMLModelEditorHandles.bcrAutoCreated
              Caption = 'Remove class on unboldify'
              ReadOnly = False
              TabOrder = 8
            end
            object bcbIsRootClass: TBoldCheckBox
              Left = 418
              Top = 120
              Width = 97
              Height = 17
              TabStop = False
              BoldHandle = dmBoldUMLModelEditorHandles.behClassIsRootClass
              BoldProperties.Expression = ''
              Caption = 'Is Root Class'
              Enabled = False
              ReadOnly = False
              TabOrder = 10
            end
            object btnGotoSuperClass: TButton
              Left = 395
              Top = 80
              Width = 17
              Height = 21
              Action = actSelectSuperClass
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 6
            end
            object cmbSuperClass: TcxBoldComboBox
              Left = 120
              Top = 80
              HelpContext = 59
              DataBinding.BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              DataBinding.BoldProperties.Expression = 'generalization->first.parent'
              DataBinding.BoldProperties.ApplyPolicy = bapChange
              DataBinding.BoldProperties.NilRepresentation = '<None>'
              ParentFont = False
              Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllClasses
              Properties.BoldRowProperties.Expression = 'name'
              Properties.BoldRowProperties.NilRepresentation = '<None>'
              Properties.Alignment.Horz = taLeftJustify
              Properties.DropDownListStyle = lsEditFixedList
              Style.BorderStyle = ebsFlat
              Style.Color = clWindow
              TabOrder = 5
              Width = 272
            end
            object BoldEdit1: TBoldEdit
              Left = 121
              Top = 106
              Width = 291
              Height = 21
              Hint = 'Superclasses'
              HelpContext = 55
              BoldHandle = dmBoldUMLModelEditorHandles.blhModelClasses
              BoldProperties.Expression = 'superclasses->asCommaText'
              ReadOnly = False
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              Alignment = taLeftJustify
              ButtonStyle = bbsNone
              MaxLength = 0
              TabOrder = 9
            end
          end
          object tabAttributes: TTabSheet
            Caption = 'Attributes'
            DesignSize = (
              1074
              502)
            object cxGridAttributes: TcxGrid
              Left = 0
              Top = 0
              Width = 1074
              Height = 502
              Align = alClient
              TabOrder = 0
              object tvAttributes: TcxGridBoldTableView
                PopupMenu = popTree
                OnDblClick = tvGridDblClick
                OnKeyPress = tvGridViewKeyPress
                Navigator.Buttons.CustomButtons = <>
                FindPanel.DisplayMode = fpdmAlways
                FindPanel.InfoText = 'Search Attributes'
                FindPanel.Layout = fplCompact
                ScrollbarAnnotations.CustomAnnotations = <>
                DataController.BoldProperties.DefaultDblClick = False
                DataController.BoldHandle = dmBoldUMLModelEditorHandles.blhClassAttributes
                DataController.OnAfterLoad = tvDataControllerAfterLoad
                DataController.Filter.Options = [fcoSoftNull]
                DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
                OptionsBehavior.IncSearch = True
                OptionsSelection.CellSelect = False
                OptionsSelection.InvertSelect = False
                OptionsView.ColumnAutoWidth = True
                OptionsView.GridLines = glNone
                OptionsView.GroupByBox = False
                Preview.Column = tvAttributesOCLExpression
                object tvAttributesConstraints: TcxGridBoldColumn
                  Caption = #167
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'constraints->select(c|not c)->size = 0'
                  BestFitMaxWidth = 16
                  Options.Editing = False
                  Options.Focusing = False
                  Options.IncSearch = False
                  Options.HorzSizing = False
                  Options.Moving = False
                  Width = 20
                end
                object tvAttributesqualifiedName: TcxGridBoldColumn
                  Caption = 'qualifiedName'
                  DataBinding.BoldProperties.Expression = 'qualifiedName'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Visible = False
                end
                object tvAttributesqualifyingOwner: TcxGridBoldColumn
                  Caption = 'qualifyingOwner'
                  DataBinding.BoldProperties.Expression = 'qualifyingOwner'
                  Visible = False
                  Width = 100
                end
                object tvAttributesname: TcxGridBoldColumn
                  Caption = 'name'
                  DataBinding.BoldProperties.Expression = 'name'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Styles.Content = cxBoldStyle
                  Width = 285
                end
                object tvAttributestype: TcxGridBoldColumn
                  Caption = 'type'
                  DataBinding.BoldProperties.Expression = 'type'
                  Width = 78
                end
                object tvAttributesstereotype: TcxGridBoldColumn
                  Caption = 'stereotype'
                  DataBinding.BoldProperties.Expression = 'stereotype'
                  Visible = False
                  Width = 49
                end
                object tvAttributespersistent: TcxGridBoldColumn
                  Caption = 'persistent'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'persistent'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 56
                end
                object tvAttributesderived: TcxGridBoldColumn
                  Caption = 'derived'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'derived'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 56
                end
                object tvAttributesReverseDerived: TcxGridBoldColumn
                  Caption = 'reverse derived'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.ReverseDerive'#39'].value = '#39'True'#39
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 56
                end
                object tvAttributesmodel: TcxGridBoldColumn
                  Caption = 'model'
                  DataBinding.BoldProperties.Expression = 'model'
                  Visible = False
                end
                object tvAttributesowner: TcxGridBoldColumn
                  Caption = 'owner'
                  DataBinding.BoldProperties.Expression = 'owner'
                  Visible = False
                end
                object tvAttributesvisibility: TcxGridBoldColumn
                  Caption = 'visibility'
                  DataBinding.BoldProperties.Expression = 'visibility'
                  PropertiesClassName = 'TcxComboBoxProperties'
                  Properties.DropDownListStyle = lsEditFixedList
                  Properties.DropDownRows = 3
                  Properties.Items.Strings = (
                    'private'
                    'protected'
                    'public')
                  Visible = False
                end
                object tvAttributesOCLExpression: TcxGridBoldColumn
                  Caption = 'OCL'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DerivationOCL'#39'].value'
                  PropertiesClassName = 'TcxTextEditProperties'
                end
                object tvAttributesLength: TcxGridBoldColumn
                  Caption = 'Length'
                  DataBinding.ValueType = 'Integer'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.Length'#39'].value.strToInt'
                  PropertiesClassName = 'TcxSpinEditProperties'
                  Properties.Alignment.Horz = taRightJustify
                  Properties.SpinButtons.Visible = False
                  Properties.ValueType = vtInt
                  Properties.ZeroIncrement = True
                  Properties.ZeroLargeIncrement = True
                  Options.AutoWidthSizable = False
                  Width = 64
                end
                object tvAttributesAllowNull: TcxGridBoldColumn
                  Caption = 'AllowNull'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.AllowNULL'#39'].value'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 56
                end
                object tvAttributesinitialValue: TcxGridBoldColumn
                  Caption = 'initialValue'
                  DataBinding.BoldProperties.Expression = 'initialValue'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Width = 78
                end
                object tvAttributesDefaultDBValue: TcxGridBoldColumn
                  Caption = 'Default DB value'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DefaultDBValue'#39'].value'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Width = 63
                end
                object tvAttributesDelayedFetch: TcxGridBoldColumn
                  Caption = 'DelayedFetch'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DelayedFetch'#39'].value'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 74
                end
                object tvAttributesDelphiName: TcxGridBoldColumn
                  Caption = 'Delphi name'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Width = 66
                end
                object tvAttributesExpressionName: TcxGridBoldColumn
                  Caption = 'Expression name'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Width = 64
                end
                object tvAttributesPMapperName: TcxGridBoldColumn
                  Caption = 'PMapper name'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.PMapper'#39'].value'#13
                  PropertiesClassName = 'TcxTextEditProperties'
                  Width = 63
                end
                object tvAttributesColumnName: TcxGridBoldColumn
                  Caption = 'Column name'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.ColumnName'#39'].value'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Width = 65
                end
                object tvAttributesGetMethod: TcxGridBoldColumn
                  Caption = 'Get'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.GetMethod'#39'].value'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Visible = False
                  Width = 49
                end
                object tvAttributesSetMethod: TcxGridBoldColumn
                  Caption = 'Set'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.SetMethod'#39'].value'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Visible = False
                  Width = 50
                end
                object tvAttributesLocalVariable: TcxGridBoldColumn
                  Caption = 'Loc var'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.LocalVariable'#39'].value'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Visible = False
                  Width = 49
                end
              end
              object cxGridAttributesLevel: TcxGridLevel
                GridView = tvAttributes
              end
            end
            object cxCheckBox1: TcxCheckBox
              Left = 840
              Top = 9
              Anchors = [akTop, akRight]
              Caption = 'Show Derivation'
              ParentFont = False
              Style.TransparentBorder = False
              TabOrder = 1
              Transparent = True
              OnClick = CheckBox2Click
            end
            object cxCheckBox2: TcxCheckBox
              Left = 944
              Top = 9
              Anchors = [akTop, akRight]
              Caption = 'Show Inherited'
              ParentFont = False
              Style.TransparentBorder = False
              TabOrder = 2
              Transparent = True
              OnClick = CheckBox1Click
            end
          end
          object tabAssociationEnds: TTabSheet
            Caption = 'AssociationEnds'
            ImageIndex = 1
            DesignSize = (
              1074
              502)
            object cxGridAssociationEnds: TcxGrid
              Left = 0
              Top = 0
              Width = 1074
              Height = 502
              Align = alClient
              TabOrder = 0
              object tvAssociationEnds: TcxGridBoldTableView
                PopupMenu = popTree
                OnDblClick = tvGridDblClick
                OnKeyPress = tvGridViewKeyPress
                Navigator.Buttons.CustomButtons = <>
                FindPanel.DisplayMode = fpdmAlways
                FindPanel.InfoText = 'Search AssociationEnds'
                FindPanel.Layout = fplCompact
                ScrollbarAnnotations.CustomAnnotations = <>
                DataController.BoldProperties.DefaultDblClick = False
                DataController.BoldHandle = dmBoldUMLModelEditorHandles.blhClassAssociationEnds
                DataController.Filter.Options = [fcoSoftNull]
                DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
                OptionsBehavior.IncSearch = True
                OptionsCustomize.ColumnGrouping = False
                OptionsSelection.CellSelect = False
                OptionsSelection.InvertSelect = False
                OptionsView.ColumnAutoWidth = True
                OptionsView.GridLines = glNone
                OptionsView.GroupByBox = False
                Preview.Column = tvAssociationEndsOclExpression
                object tvAssociationEndsConstraints: TcxGridBoldColumn
                  Caption = #167
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'constraints->select(c|not c)->size = 0'
                  BestFitMaxWidth = 16
                  Options.Editing = False
                  Options.Focusing = False
                  Options.IncSearch = False
                  Options.HorzSizing = False
                  Options.Moving = False
                  Width = 20
                end
                object tvAssociationEndsqualifiedName: TcxGridBoldColumn
                  Caption = 'qualifiedName'
                  DataBinding.BoldProperties.Expression = 'qualifiedName'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Visible = False
                end
                object tvAssociationEndstype: TcxGridBoldColumn
                  Caption = 'Other end type'
                  DataBinding.BoldProperties.Expression = 'type'
                  Width = 169
                end
                object tvAssociationEndsname: TcxGridBoldColumn
                  Caption = 'name'
                  DataBinding.BoldProperties.Expression = 'name'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Styles.Content = cxBoldStyle
                  Width = 323
                end
                object tvAssociationEndsqualifyingOwner: TcxGridBoldColumn
                  Caption = 'type'
                  DataBinding.BoldProperties.Expression = 'otherEnd.type'
                  Width = 133
                end
                object tvAssociationEndsvisibility: TcxGridBoldColumn
                  Caption = 'visibility'
                  DataBinding.BoldProperties.Expression = 'visibility'
                  PropertiesClassName = 'TcxComboBoxProperties'
                  Properties.DropDownListStyle = lsEditFixedList
                  Properties.DropDownRows = 3
                  Properties.Items.Strings = (
                    'private'
                    'protected'
                    'public')
                  Visible = False
                  Options.AutoWidthSizable = False
                end
                object tvAssociationEndsaggregation: TcxGridBoldColumn
                  Caption = 'aggregation'
                  DataBinding.BoldProperties.Expression = 'aggregation'
                  PropertiesClassName = 'TcxComboBoxProperties'
                  Properties.DropDownListStyle = lsEditFixedList
                  Properties.DropDownRows = 3
                  Properties.Items.Strings = (
                    'none'
                    'aggregate'
                    'composite')
                  Options.AutoWidthSizable = False
                  Width = 64
                end
                object tvAssociationEndsmultiplicity: TcxGridBoldColumn
                  Caption = 'multiplicity'
                  DataBinding.BoldProperties.Expression = 'multiplicity'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Options.AutoWidthSizable = False
                  Width = 64
                end
                object tvAssociationEndsderived: TcxGridBoldColumn
                  Caption = 'derived'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'association.derived'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 64
                end
                object tvAssociationEndsmodel: TcxGridBoldColumn
                  Caption = 'model'
                  DataBinding.BoldProperties.Expression = 'model'
                  Visible = False
                end
                object tvAssociationEndsmulti: TcxGridBoldColumn
                  Caption = 'multi'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'multi'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Visible = False
                  Options.AutoWidthSizable = False
                end
                object tvAssociationEndsmandatory: TcxGridBoldColumn
                  Caption = 'mandatory'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'mandatory'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Visible = False
                  Options.AutoWidthSizable = False
                  Width = 64
                end
                object tvAssociationEndsColumn1: TcxGridBoldColumn
                  Caption = 'qualified'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'qualifier->notEmpty'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 59
                end
                object tvAssociationEndsisOrdered: TcxGridBoldColumn
                  Caption = 'isOrdered'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'isOrdered'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 64
                end
                object tvAssociationEndsisNavigable: TcxGridBoldColumn
                  Caption = 'isNavigable'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'isNavigable'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 64
                end
                object tvAssociationEndsEmbed: TcxGridBoldColumn
                  Caption = 'Embed'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.Embed'#39'].value'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 64
                end
                object tvAssociationEndsotherEnd: TcxGridBoldColumn
                  Caption = 'otherEnd'
                  DataBinding.BoldProperties.Expression = 'otherEnd'
                  Visible = False
                  Width = 126
                end
                object tvAssociationEndsassociation: TcxGridBoldColumn
                  Caption = 'association'
                  DataBinding.BoldProperties.Expression = 'association'
                  Visible = False
                  Width = 58
                end
                object tvAssociationEndsOclExpression: TcxGridBoldColumn
                  Caption = 'OCL'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DerivationOCL'#39'].value'
                  PropertiesClassName = 'TcxTextEditProperties'
                end
                object tvAssociationEndsstereotype: TcxGridBoldColumn
                  Caption = 'stereotype'
                  DataBinding.BoldProperties.Expression = 'stereotypename'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Visible = False
                  Width = 95
                end
              end
              object cxGridLevelAssociationEnds: TcxGridLevel
                GridView = tvAssociationEnds
              end
            end
            object cxCheckBox3: TcxCheckBox
              Left = 837
              Top = 9
              Anchors = [akTop, akRight]
              Caption = 'Show Derivation'
              ParentFont = False
              Style.TransparentBorder = False
              TabOrder = 1
              Transparent = True
              OnClick = cxCheckBox3Click
            end
            object cxCheckBox4: TcxCheckBox
              Left = 939
              Top = 9
              Anchors = [akTop, akRight]
              Caption = 'Show Inherited'
              ParentFont = False
              Style.TransparentBorder = False
              TabOrder = 2
              Transparent = True
              OnClick = cxCheckBox4Click
            end
          end
          object tabAssociations: TTabSheet
            Caption = 'Associations'
            ImageIndex = 3
            DesignSize = (
              1074
              502)
            object cxGridClassAssociations: TcxGrid
              Left = 0
              Top = 0
              Width = 1074
              Height = 502
              Align = alClient
              TabOrder = 0
              object tvClassAssociations: TcxGridBoldTableView
                PopupMenu = popTree
                OnDblClick = tvGridDblClick
                OnKeyPress = tvGridViewKeyPress
                Navigator.Buttons.CustomButtons = <>
                FindPanel.DisplayMode = fpdmAlways
                FindPanel.InfoText = 'Search Associations'
                FindPanel.Layout = fplCompact
                ScrollbarAnnotations.CustomAnnotations = <>
                DataController.BoldProperties.DefaultDblClick = False
                DataController.BoldHandle = dmBoldUMLModelEditorHandles.blhClassAssociations
                DataController.OnAfterLoad = tvDataControllerAfterLoad
                DataController.Filter.Options = [fcoSoftNull]
                DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
                OptionsBehavior.IncSearch = True
                OptionsCustomize.ColumnGrouping = False
                OptionsSelection.CellSelect = False
                OptionsSelection.InvertSelect = False
                OptionsView.ColumnAutoWidth = True
                OptionsView.Footer = True
                OptionsView.GridLines = glNone
                OptionsView.GroupByBox = False
                OptionsView.Indicator = True
                Preview.Column = tvClassAssociationsOCLExpression
                object tvClassAssociationsConstraints: TcxGridBoldColumn
                  Caption = #167
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'constraints->select(c|not c)->size = 0'
                  BestFitMaxWidth = 16
                  Options.Editing = False
                  Options.Focusing = False
                  Options.IncSearch = False
                  Options.HorzSizing = False
                  Options.Moving = False
                  Width = 20
                end
                object tvClassAssociationsassociation: TcxGridBoldColumn
                  DataBinding.BoldProperties.Expression = 'association'
                  Visible = False
                  GroupIndex = 0
                  IsCaptionAssigned = True
                end
                object tvClassAssociationsqualifyingOwner: TcxGridBoldColumn
                  Caption = 'qualifyingOwner'
                  DataBinding.BoldProperties.Expression = 'qualifyingOwner'
                  Visible = False
                end
                object tvClassAssociationsname: TcxGridBoldColumn
                  Caption = 'name'
                  DataBinding.BoldProperties.Expression = 'name'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Styles.Content = cxBoldStyle
                  Width = 397
                end
                object tvClassAssociationstype: TcxGridBoldColumn
                  Caption = 'type'
                  DataBinding.BoldProperties.Expression = 'type'
                  Width = 291
                end
                object tvClassAssociationsderived: TcxGridBoldColumn
                  Caption = 'derived'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'association.derived'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 46
                end
                object tvClassAssociationsColumn1: TcxGridBoldColumn
                  Caption = 'qualified'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'qualifier->notEmpty'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 59
                end
                object tvClassAssociationsisNavigable: TcxGridBoldColumn
                  Caption = 'isNavigable'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'isNavigable'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 60
                end
                object tvClassAssociationsaggregation: TcxGridBoldColumn
                  Caption = 'aggregation'
                  DataBinding.BoldProperties.Expression = 'aggregation'
                  PropertiesClassName = 'TcxComboBoxProperties'
                  Properties.DropDownListStyle = lsEditFixedList
                  Properties.DropDownRows = 3
                  Properties.Items.Strings = (
                    'none'
                    'aggregate'
                    'composite')
                  Options.AutoWidthSizable = False
                  Width = 68
                end
                object tvClassAssociationsmultiplicity: TcxGridBoldColumn
                  Caption = 'multiplicity'
                  DataBinding.BoldProperties.Expression = 'multiplicity'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Options.AutoWidthSizable = False
                  Width = 60
                end
                object tvClassAssociationsmulti: TcxGridBoldColumn
                  Caption = 'multi'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'multi'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Visible = False
                  Options.AutoWidthSizable = False
                  Width = 32
                end
                object tvClassAssociationsmandatory: TcxGridBoldColumn
                  Caption = 'mandatory'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'mandatory'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Visible = False
                  Options.AutoWidthSizable = False
                  Width = 60
                end
                object tvClassAssociationsisOrdered: TcxGridBoldColumn
                  Caption = 'isOrdered'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'isOrdered'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                  Options.AutoWidthSizable = False
                  Width = 60
                end
                object tvClassAssociationsColumn2: TcxGridBoldColumn
                  Caption = 'isAssociationClass'
                  DataBinding.ValueType = 'Boolean'
                  DataBinding.BoldProperties.Expression = 'association.isAssociationClass'
                  PropertiesClassName = 'TcxCheckBoxProperties'
                end
                object tvClassAssociationsordering: TcxGridBoldColumn
                  Caption = 'ordering'
                  DataBinding.BoldProperties.Expression = 'ordering'
                  PropertiesClassName = 'TcxComboBoxProperties'
                  Properties.DropDownListStyle = lsEditFixedList
                  Properties.DropDownRows = 2
                  Properties.Items.Strings = (
                    'unordered'
                    'ordered')
                  Visible = False
                  Options.AutoWidthSizable = False
                  Width = 60
                end
                object tvClassAssociationsOCLExpression: TcxGridBoldColumn
                  Caption = 'OCL'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DerivationOCL'#39'].value'
                  PropertiesClassName = 'TcxTextEditProperties'
                end
              end
              object cxGridLevelClassAssociations: TcxGridLevel
                GridView = tvClassAssociations
              end
            end
            object cxCheckBox5: TcxCheckBox
              Left = 845
              Top = 17
              Anchors = [akTop, akRight]
              Caption = 'Show Derivation'
              ParentFont = False
              Style.TransparentBorder = False
              TabOrder = 1
              Transparent = True
              OnClick = cxCheckBox5Click
            end
            object cxCheckBox7: TcxCheckBox
              Left = 945
              Top = 17
              Anchors = [akTop, akRight]
              Caption = 'Show Inherited'
              ParentFont = False
              Style.TransparentBorder = False
              TabOrder = 2
              Transparent = True
              OnClick = cxCheckBox7Click
            end
          end
          object tabClientOperations: TTabSheet
            Caption = 'Operations'
            ImageIndex = 2
            DesignSize = (
              1074
              502)
            object cxGridOperations: TcxGrid
              Left = 0
              Top = 0
              Width = 1074
              Height = 502
              Align = alClient
              TabOrder = 0
              object tvOperations: TcxGridBoldTableView
                PopupMenu = popTree
                OnDblClick = tvGridDblClick
                OnKeyPress = tvGridViewKeyPress
                Navigator.Buttons.CustomButtons = <>
                FindPanel.DisplayMode = fpdmAlways
                FindPanel.InfoText = 'Search Operations'
                FindPanel.Layout = fplCompact
                ScrollbarAnnotations.CustomAnnotations = <>
                DataController.BoldProperties.DefaultDblClick = False
                DataController.BoldHandle = dmBoldUMLModelEditorHandles.blhClassOperations
                DataController.OnAfterLoad = tvDataControllerAfterLoad
                DataController.Filter.Options = [fcoSoftNull]
                DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
                OptionsSelection.CellSelect = False
                OptionsSelection.InvertSelect = False
                OptionsView.ColumnAutoWidth = True
                OptionsView.GridLines = glNone
                OptionsView.GroupByBox = False
                object tvOperationsqualifyingOwner: TcxGridBoldColumn
                  Caption = 'qualifyingOwner'
                  DataBinding.BoldProperties.Expression = 'qualifyingOwner'
                  Visible = False
                end
                object tvOperationsname: TcxGridBoldColumn
                  Caption = 'name'
                  DataBinding.BoldProperties.Expression = 'name'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Styles.Content = cxBoldStyle
                end
                object tvOperationsvisibility: TcxGridBoldColumn
                  Caption = 'visibility'
                  DataBinding.BoldProperties.Expression = 'visibility'
                  PropertiesClassName = 'TcxComboBoxProperties'
                  Properties.DropDownListStyle = lsEditFixedList
                  Properties.DropDownRows = 3
                  Properties.Items.Strings = (
                    'private'
                    'protected'
                    'public')
                end
                object tvOperationsisOwnerscope: TcxGridBoldColumn
                  Caption = 'Owner scope'
                  DataBinding.BoldProperties.Expression = 'ownerscope'
                end
                object tvOperationsDelphiFunctionType: TcxGridBoldColumn
                  Caption = 'Delphi function type'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.OperationKind'#39'].value'
                  PropertiesClassName = 'TcxTextEditProperties'
                end
                object tvOperationsDelphiName: TcxGridBoldColumn
                  Caption = 'Delphi name'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
                  PropertiesClassName = 'TcxTextEditProperties'
                end
                object tvOperationsisExpressionName: TcxGridBoldColumn
                  Caption = 'Expression name'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
                end
              end
              object cxLevelOperations: TcxGridLevel
                GridView = tvOperations
              end
            end
            object cxCheckBox6: TcxCheckBox
              Left = 925
              Top = 9
              Anchors = [akTop, akRight]
              Caption = 'Show Inherited'
              ParentFont = False
              Style.TransparentBorder = False
              TabOrder = 1
              Transparent = True
              OnClick = cxCheckBox6Click
            end
          end
          object TabOCL: TTabSheet
            Caption = 'OCL Expressions'
            ImageIndex = 4
            DesignSize = (
              1074
              502)
            object cxGridOclExpressions: TcxGrid
              Left = 0
              Top = 0
              Width = 1074
              Height = 502
              Align = alClient
              TabOrder = 0
              object tvOclExpressions: TcxGridBoldTableView
                PopupMenu = popTree
                OnDblClick = tvOclExpressionsDblClick
                OnKeyPress = tvGridViewKeyPress
                Navigator.Buttons.CustomButtons = <>
                FindPanel.DisplayMode = fpdmAlways
                FindPanel.InfoText = 'Search expressions'
                FindPanel.Layout = fplCompact
                FindPanel.SearchInPreview = True
                ScrollbarAnnotations.CustomAnnotations = <>
                DataController.BoldProperties.DefaultDblClick = False
                DataController.BoldHandle = dmBoldUMLModelEditorHandles.blhOclExpressions
                DataController.Filter.Options = [fcoSoftNull]
                DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
                OptionsData.Deleting = False
                OptionsData.DeletingConfirmation = False
                OptionsData.Editing = False
                OptionsData.Inserting = False
                OptionsSelection.CellSelect = False
                OptionsSelection.InvertSelect = False
                OptionsView.CellAutoHeight = True
                OptionsView.ColumnAutoWidth = True
                OptionsView.GridLines = glNone
                OptionsView.GroupByBox = False
                Preview.Column = tvOclExpressionsExpression
                Preview.Visible = True
                object tvOclExpressionsqualifiedName: TcxGridBoldColumn
                  Caption = 'qualifiedName'
                  DataBinding.BoldProperties.Expression = 'qualifiedName'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Visible = False
                  Width = 155
                end
                object tvOclExpressionsqualifyingOwner: TcxGridBoldColumn
                  Caption = 'Owner'
                  DataBinding.BoldProperties.Expression = 'qualifyingOwner'
                  Visible = False
                end
                object tvOclExpressionsName: TcxGridBoldColumn
                  Caption = 'Name'
                  DataBinding.BoldProperties.Expression = 'name'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Styles.Content = cxBoldStyle
                  Width = 338
                end
                object tvOclExpressionsType: TcxGridBoldColumn
                  Caption = 'Type'
                  DataBinding.BoldProperties.Expression = 'oclType'
                  Options.AutoWidthSizable = False
                  Width = 100
                end
                object tvOclExpressionsExpression: TcxGridBoldColumn
                  Caption = 'OCL Expression'
                  DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DerivationOCL'#39'].value'
                  PropertiesClassName = 'TcxTextEditProperties'
                  Width = 742
                end
              end
              object cxLevelOclExpressions: TcxGridLevel
                GridView = tvOclExpressions
              end
            end
            object cxCheckBox8: TcxCheckBox
              Left = 941
              Top = 9
              Anchors = [akTop, akRight]
              Caption = 'Show Inherited'
              ParentFont = False
              Style.TransparentBorder = False
              TabOrder = 1
              Transparent = True
              OnClick = cxCheckBox8Click
            end
          end
        end
      end
    end
    object tsAssociations: TTabSheet
      Caption = 'Associations'
      ImageIndex = 4
      object gridAssociations: TcxGrid
        Left = 0
        Top = 0
        Width = 1314
        Height = 532
        Align = alClient
        TabOrder = 0
        object tvAssociations: TcxGridBoldTableView
          PopupMenu = popTree
          OnDblClick = tvGridDblClick
          Navigator.Buttons.CustomButtons = <>
          FindPanel.DisplayMode = fpdmAlways
          FindPanel.InfoText = 'Search Associations'
          FindPanel.Layout = fplCompact
          ScrollbarAnnotations.CustomAnnotations = <>
          DataController.BoldProperties.DefaultDblClick = False
          DataController.BoldHandle = dmBoldUMLModelEditorHandles.blhAssociations
          DataController.Filter.Options = [fcoSoftNull]
          DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <
            item
              Kind = skCount
              Column = tvAssociationsname
            end>
          DataController.Summary.SummaryGroups = <>
          OptionsData.Deleting = False
          OptionsData.DeletingConfirmation = False
          OptionsData.Editing = False
          OptionsData.Inserting = False
          OptionsSelection.CellSelect = False
          OptionsSelection.InvertSelect = False
          OptionsView.ColumnAutoWidth = True
          OptionsView.Footer = True
          OptionsView.GridLines = glNone
          OptionsView.GroupByBox = False
          object tvAssociationsConstraints: TcxGridBoldColumn
            Caption = #167
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'constraints->select(c|not c)->size = 0'
            BestFitMaxWidth = 16
            Options.Editing = False
            Options.Focusing = False
            Options.IncSearch = False
            Options.HorzSizing = False
            Options.Moving = False
            Width = 20
          end
          object tvAssociationsname: TcxGridBoldColumn
            Caption = 'name'
            DataBinding.BoldProperties.Expression = 'name'
            PropertiesClassName = 'TcxTextEditProperties'
          end
          object tvAssociationsclass: TcxGridBoldColumn
            Caption = 'class'
            DataBinding.BoldProperties.Expression = 'class'
          end
          object tvAssociationsderived: TcxGridBoldColumn
            Caption = 'derived'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'derived'
            PropertiesClassName = 'TcxCheckBoxProperties'
            Options.AutoWidthSizable = False
            Width = 60
          end
          object tvAssociationsstereotypeName: TcxGridBoldColumn
            Caption = 'stereotypeName'
            DataBinding.BoldProperties.Expression = 'stereotypeName'
            PropertiesClassName = 'TcxTextEditProperties'
            Visible = False
          end
          object tvAssociationspersistent: TcxGridBoldColumn
            Caption = 'persistent'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'persistent'
            PropertiesClassName = 'TcxCheckBoxProperties'
            Options.AutoWidthSizable = False
            Width = 60
          end
          object tvAssociationsisAssociationClass: TcxGridBoldColumn
            Caption = 'isAssociationClass'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'isAssociationClass'
            PropertiesClassName = 'TcxCheckBoxProperties'
            Visible = False
          end
          object tvAssociationsColumn5: TcxGridBoldColumn
            Caption = 'Qualified'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'connection.qualifier->notEmpty'
            PropertiesClassName = 'TcxCheckBoxProperties'
            Options.AutoWidthSizable = False
            Width = 60
          end
          object tvAssociationsColumn6: TcxGridBoldColumn
            Caption = 'isAssociationClass'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'isAssociationClass'
            PropertiesClassName = 'TcxCheckBoxProperties'
            Options.AutoWidthSizable = False
            Width = 100
          end
          object tvAssociationsColumn1: TcxGridBoldColumn
            Caption = 'End A'
            DataBinding.BoldProperties.Expression = 'connection->first'
          end
          object tvAssociationsColumn3: TcxGridBoldColumn
            Caption = 'type A'
            DataBinding.BoldProperties.Expression = 'connection->first.type'
          end
          object tvAssociationsColumn2: TcxGridBoldColumn
            Caption = 'End B'
            DataBinding.BoldProperties.Expression = 'connection->last'
          end
          object tvAssociationsColumn4: TcxGridBoldColumn
            Caption = 'Type B'
            DataBinding.BoldProperties.Expression = 'connection->last.type'
          end
        end
        object lvAssociations: TcxGridLevel
          GridView = tvAssociations
        end
      end
    end
    object tsDataTypes2: TTabSheet
      Caption = 'Data types'
      ImageIndex = 2
      DesignSize = (
        1314
        532)
      object cxGrid2: TcxGrid
        Left = 0
        Top = 0
        Width = 1314
        Height = 532
        Align = alClient
        TabOrder = 0
        object tvDataTypes: TcxGridBoldTableView
          PopupMenu = popTree
          OnDblClick = tvDataTypesDblClick
          Navigator.Buttons.CustomButtons = <>
          FindPanel.DisplayMode = fpdmAlways
          FindPanel.InfoText = 'Search DataTypes'
          FindPanel.Layout = fplCompact
          ScrollbarAnnotations.CustomAnnotations = <>
          DataController.BoldProperties.DefaultDblClick = False
          DataController.BoldHandle = dmBoldUMLModelEditorHandles.bchDataTypes
          DataController.Filter.Options = [fcoSoftNull]
          DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          OptionsData.Deleting = False
          OptionsData.DeletingConfirmation = False
          OptionsData.Editing = False
          OptionsData.Inserting = False
          OptionsSelection.CellSelect = False
          OptionsSelection.InvertSelect = False
          OptionsView.ColumnAutoWidth = True
          OptionsView.GridLines = glNone
          OptionsView.GroupByBox = False
          object cxGridBoldColumn3: TcxGridBoldColumn
            Caption = #167
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'constraints->select(c|not c)->size = 0'
            BestFitMaxWidth = 16
            Options.Editing = False
            Options.Focusing = False
            Options.IncSearch = False
            Options.HorzSizing = False
            Options.Moving = False
            Width = 20
          end
          object cxGridBoldColumn4: TcxGridBoldColumn
            Caption = 'name'
            DataBinding.BoldProperties.Expression = 'name'
            PropertiesClassName = 'TcxTextEditProperties'
          end
          object tvDataTypesColumn2: TcxGridBoldColumn
            Caption = 'isAbstract'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'isAbstract'
            PropertiesClassName = 'TcxCheckBoxProperties'
            Options.AutoWidthSizable = False
          end
          object tvDataTypesColumn1: TcxGridBoldColumn
            Caption = 'Attribute Type'
            DataBinding.BoldProperties.Expression = ''
            DataBinding.BoldProperties.Renderer = dmBoldUMLModelEditorHandles.rDataTypeAttributeType
            PropertiesClassName = 'TcxTextEditProperties'
          end
          object cxGridBoldColumn5: TcxGridBoldColumn
            Caption = 'PMapper name'
            DataBinding.BoldProperties.Expression = ''
            DataBinding.BoldProperties.Renderer = dmBoldUMLModelEditorHandles.rDataTypePMapper
            PropertiesClassName = 'TcxTextEditProperties'
          end
        end
        object cxGridLevel2: TcxGridLevel
          GridView = tvDataTypes
        end
      end
      object cxCheckBox9: TcxCheckBox
        Left = 1171
        Top = 9
        Anchors = [akTop, akRight]
        Caption = 'Show all types'
        ParentFont = False
        Style.TransparentBorder = False
        TabOrder = 1
        Transparent = True
        OnClick = cxCheckBox9Click
      end
    end
    object tabFeatures: TTabSheet
      Caption = 'Features'
      ImageIndex = 6
      object cxGrid5: TcxGrid
        Left = 0
        Top = 0
        Width = 1314
        Height = 532
        Align = alClient
        TabOrder = 0
        object cxGridBoldTableView4: TcxGridBoldTableView
          PopupMenu = popTree
          OnDblClick = tvGridDblClick
          OnKeyPress = tvGridViewKeyPress
          Navigator.Buttons.CustomButtons = <>
          FindPanel.DisplayMode = fpdmAlways
          FindPanel.InfoText = 'Search features'
          FindPanel.Layout = fplCompact
          FindPanel.ShowPreviousButton = False
          ScrollbarAnnotations.CustomAnnotations = <>
          DataController.BoldProperties.DefaultDblClick = False
          DataController.BoldHandle = dmBoldUMLModelEditorHandles.bchClassFeatures
          DataController.OnAfterLoad = tvDataControllerAfterLoad
          DataController.Filter.Options = [fcoSoftNull]
          DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <
            item
              Kind = skCount
              Column = cxGridBoldColumn9
            end>
          DataController.Summary.SummaryGroups = <>
          OptionsBehavior.IncSearch = True
          OptionsData.Editing = False
          OptionsSelection.CellSelect = False
          OptionsSelection.InvertSelect = False
          OptionsView.ColumnAutoWidth = True
          OptionsView.Footer = True
          OptionsView.GridLines = glNone
          Preview.Column = cxGridBoldColumn17
          object cxGridBoldColumn6: TcxGridBoldColumn
            Caption = #167
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'constraints->select(c|not c)->size = 0'
            BestFitMaxWidth = 16
            Options.Editing = False
            Options.Focusing = False
            Options.IncSearch = False
            Options.HorzSizing = False
            Options.Moving = False
            Width = 20
          end
          object cxGridBoldColumn7: TcxGridBoldColumn
            Caption = 'qualifiedName'
            DataBinding.BoldProperties.Expression = 'qualifiedName'
            PropertiesClassName = 'TcxTextEditProperties'
            Visible = False
          end
          object cxGridBoldColumn8: TcxGridBoldColumn
            Caption = 'qualifyingOwner'
            DataBinding.BoldProperties.Expression = 'qualifyingOwner'
            Width = 272
          end
          object cxGridBoldColumn9: TcxGridBoldColumn
            Caption = 'name'
            DataBinding.BoldProperties.Expression = 'name'
            PropertiesClassName = 'TcxTextEditProperties'
            Styles.Content = cxBoldStyle
            Width = 621
          end
          object cxGridBoldTableView4Column3: TcxGridBoldColumn
            Caption = 'Type'
            DataBinding.BoldProperties.Expression = 
              'if self.oclIsTypeOf(UMLAttribute) then'#13#10' self.oclAsType(UMLAttri' +
              'bute).typeName'#13#10'else'#13#10'if self.oclIsTypeOf(UMLAssociationEnd) the' +
              'n'#13#10' self.oclAsType(UMLAssociationEnd).type.name'#13#10'else'#13#10#39#39#13#10'endif' +
              #13#10'endif'
            PropertiesClassName = 'TcxTextEditProperties'
            Width = 166
          end
          object cxGridBoldTableView4Column1: TcxGridBoldColumn
            Caption = 'Element Type'
            DataBinding.BoldProperties.Expression = 'self.oclType'
            Width = 186
          end
          object cxGridBoldColumn14: TcxGridBoldColumn
            Caption = 'model'
            DataBinding.BoldProperties.Expression = 'model'
            Visible = False
          end
          object cxGridBoldColumn15: TcxGridBoldColumn
            Caption = 'owner'
            DataBinding.BoldProperties.Expression = 'owner'
            Visible = False
          end
          object cxGridBoldColumn17: TcxGridBoldColumn
            Caption = 'OCL'
            DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DerivationOCL'#39'].value'
            PropertiesClassName = 'TcxTextEditProperties'
          end
          object cxGridBoldTableView4Column2: TcxGridBoldColumn
            Caption = 'derived'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 
              'if self.oclIsTypeOf(UMLAssociationEnd) then'#13#10'  self.oclAsType(UM' +
              'LAssociationEnd).association.derived'#13#10'else'#13#10'  derived'#13#10'endif'#13#10#13#10 +
              #13#10
            PropertiesClassName = 'TcxCheckBoxProperties'
            Width = 63
          end
          object cxGridBoldTableView4Column4: TcxGridBoldColumn
            Caption = 'Reverse derived'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.ReverseDerive'#39'].value = '#39'True'#39
            PropertiesClassName = 'TcxCheckBoxProperties'
          end
        end
        object cxGridLevel5: TcxGridLevel
          GridView = cxGridBoldTableView4
        end
      end
    end
    object tsClassStats: TTabSheet
      Caption = 'Class stats'
      ImageIndex = 5
      object cxGrid4: TcxGrid
        Left = 0
        Top = 0
        Width = 1314
        Height = 532
        Align = alClient
        TabOrder = 0
        object cxGridBoldTableView3: TcxGridBoldTableView
          PopupMenu = popTree
          Navigator.Buttons.CustomButtons = <>
          FindPanel.DisplayMode = fpdmAlways
          FindPanel.InfoText = 'Search DataTypes'
          FindPanel.Layout = fplCompact
          ScrollbarAnnotations.CustomAnnotations = <>
          DataController.BoldProperties.DefaultDblClick = False
          DataController.BoldHandle = dmBoldUMLModelEditorHandles.blhAllClasses
          DataController.Filter.Options = [fcoSoftNull]
          DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          OptionsData.Deleting = False
          OptionsData.DeletingConfirmation = False
          OptionsData.Editing = False
          OptionsData.Inserting = False
          OptionsSelection.CellSelect = False
          OptionsSelection.InvertSelect = False
          OptionsView.ColumnAutoWidth = True
          OptionsView.GridLines = glNone
          OptionsView.GroupByBox = False
          object cxGridBoldTableView3Constraints: TcxGridBoldColumn
            Caption = #167
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'constraints->select(c|not c)->size = 0'
            BestFitMaxWidth = 16
            Options.Editing = False
            Options.Focusing = False
            Options.IncSearch = False
            Options.HorzSizing = False
            Options.Moving = False
            Width = 20
          end
          object cxGridBoldTableView3name: TcxGridBoldColumn
            Caption = 'name'
            DataBinding.BoldProperties.Expression = 'name'
            PropertiesClassName = 'TcxTextEditProperties'
          end
          object cxGridBoldTableView3Column1: TcxGridBoldColumn
            Caption = 'All features'
            DataBinding.ValueType = 'Integer'
            DataBinding.BoldProperties.Expression = 'allFeature->size'
            PropertiesClassName = 'TcxSpinEditProperties'
            Properties.Alignment.Horz = taRightJustify
            Properties.SpinButtons.Visible = False
            Properties.ValueType = vtInt
            Properties.ZeroIncrement = True
            Properties.ZeroLargeIncrement = True
          end
          object cxGridBoldTableView3Column6: TcxGridBoldColumn
            Caption = 'Own Features'
            DataBinding.ValueType = 'Integer'
            DataBinding.BoldProperties.Expression = 'feature->size'
            PropertiesClassName = 'TcxSpinEditProperties'
            Properties.Alignment.Horz = taRightJustify
            Properties.SpinButtons.Visible = False
            Properties.ValueType = vtInt
            Properties.ZeroIncrement = True
            Properties.ZeroLargeIncrement = True
          end
          object cxGridBoldTableView3Column2: TcxGridBoldColumn
            Caption = 'AssociationEnd'
            DataBinding.ValueType = 'Integer'
            DataBinding.BoldProperties.Expression = 'associationEnd->size'
            PropertiesClassName = 'TcxSpinEditProperties'
            Properties.Alignment.Horz = taRightJustify
            Properties.SpinButtons.Visible = False
            Properties.ValueType = vtInt
            Properties.ZeroIncrement = True
            Properties.ZeroLargeIncrement = True
          end
          object cxGridBoldTableView3Column7: TcxGridBoldColumn
            Caption = 'Superclasses'
            DataBinding.ValueType = 'Integer'
            DataBinding.BoldProperties.Expression = 'superclasses->size'
            PropertiesClassName = 'TcxSpinEditProperties'
            Properties.Alignment.Horz = taRightJustify
            Properties.SpinButtons.Visible = False
            Properties.ValueType = vtInt
            Properties.ZeroIncrement = True
            Properties.ZeroLargeIncrement = True
          end
          object cxGridBoldTableView3Column4: TcxGridBoldColumn
            Caption = 'Subclasses'
            DataBinding.ValueType = 'Integer'
            DataBinding.BoldProperties.Expression = 'subclasses->size'
            PropertiesClassName = 'TcxSpinEditProperties'
            Properties.Alignment.Horz = taRightJustify
            Properties.SpinButtons.Visible = False
            Properties.ValueType = vtInt
            Properties.ZeroIncrement = True
            Properties.ZeroLargeIncrement = True
          end
          object cxGridBoldTableView3Column8: TcxGridBoldColumn
            Caption = 'Constraints'
            DataBinding.ValueType = 'Integer'
            DataBinding.BoldProperties.Expression = 'constraint->size'
            PropertiesClassName = 'TcxSpinEditProperties'
            Properties.Alignment.Horz = taRightJustify
            Properties.SpinButtons.Visible = False
            Properties.ValueType = vtInt
            Properties.ZeroIncrement = True
            Properties.ZeroLargeIncrement = True
          end
          object cxGridBoldTableView3Column3: TcxGridBoldColumn
            Caption = 'All Operations'
            DataBinding.ValueType = 'Integer'
            DataBinding.BoldProperties.Expression = 'allFeature->select(oclIsTypeOf(UMLOperation))->size'
            PropertiesClassName = 'TcxSpinEditProperties'
            Properties.Alignment.Horz = taRightJustify
            Properties.SpinButtons.Visible = False
            Properties.ValueType = vtInt
            Properties.ZeroIncrement = True
            Properties.ZeroLargeIncrement = True
          end
          object cxGridBoldTableView3Column5: TcxGridBoldColumn
            Caption = 'Own Operations'
            DataBinding.ValueType = 'Integer'
            DataBinding.BoldProperties.Expression = 'feature->select(oclIsTypeOf(UMLOperation))->size'
            PropertiesClassName = 'TcxSpinEditProperties'
            Properties.Alignment.Horz = taRightJustify
            Properties.SpinButtons.Visible = False
            Properties.ValueType = vtInt
            Properties.ZeroIncrement = True
            Properties.ZeroLargeIncrement = True
          end
          object cxGridBoldTableView3Column9: TcxGridBoldColumn
            Caption = 'isAbstract'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'isAbstract'
            PropertiesClassName = 'TcxCheckBoxProperties'
          end
        end
        object cxGridLevel4: TcxGridLevel
          GridView = cxGridBoldTableView3
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 594
    Width = 1322
    Height = 180
    Align = alBottom
    TabOrder = 5
    object pcBottom: TPageControl
      Left = 1
      Top = 1
      Width = 1320
      Height = 178
      ActivePage = tsValidation
      Align = alClient
      TabOrder = 0
      object tsValidation: TTabSheet
        Caption = 'Validation'
      end
    end
  end
  object MainMenu1: TMainMenu
    Images = ilMenu
    Left = 280
    Top = 160
    object mnuFile: TMenuItem
      Caption = '&File'
      object Clear1: TMenuItem
        Caption = '&Clear'
        HelpContext = 118
        ImageIndex = 8
        OnClick = Clear1Click
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object OpenFile1: TMenuItem
        Action = FileOpen1
        GroupIndex = 1
      end
      object SaveFileAs1: TMenuItem
        Action = FileSaveAs1
        GroupIndex = 1
      end
      object N4: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object N8: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object Exit1: TMenuItem
        Caption = '&Exit'
        GroupIndex = 1
        HelpContext = 90
        ImageIndex = 14
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Undo1: TMenuItem
        Action = BoldUndoAction1
      end
      object Redo1: TMenuItem
        Action = BoldRedoAction1
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Find1: TMenuItem
        Action = actFind
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object Cut1: TMenuItem
        Caption = 'Cu&t'
        HelpContext = 91
        ImageIndex = 5
        ShortCut = 16472
        OnClick = Cut1Click
      end
      object Copy1: TMenuItem
        Caption = '&Copy'
        HelpContext = 92
        ImageIndex = 6
        ShortCut = 16451
        OnClick = Copy1Click
      end
      object Paste1: TMenuItem
        Caption = '&Paste'
        HelpContext = 93
        ImageIndex = 4
        ShortCut = 16470
        OnClick = Paste1Click
      end
    end
    object Model1: TMenuItem
      Caption = '&Model'
      OnClick = popTreePopup
      object Deletexxx2: TMenuItem
        Caption = '&Delete xxx'
        HelpContext = 102
        ImageIndex = 8
        OnClick = DeleteXxx1Click
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object NewClass2: TMenuItem
        Caption = 'New &Class'
        HelpContext = 103
        OnClick = NewClass1Click
      end
      object AddSubclass2: TMenuItem
        Caption = 'Add Subclass'
        OnClick = AddSubclass1Click
      end
      object InsertSuperclass1: TMenuItem
        Caption = 'Insert Superclass'
        OnClick = InsertSuperclass1Click
      end
      object NewAssociation2: TMenuItem
        Caption = 'New &Association'
        HelpContext = 104
        OnClick = NewAssociation1Click
      end
      object NewDataType2: TMenuItem
        Caption = 'New DataType'
        OnClick = NewDatatype1Click
      end
      object NewAttribute2: TMenuItem
        Caption = 'New A&ttribute'
        HelpContext = 105
        OnClick = NewAttribute1Click
      end
      object NewOperation2: TMenuItem
        Caption = 'New &Operation'
        HelpContext = 106
        OnClick = NewOperation1Click
      end
      object NewParameter2: TMenuItem
        Caption = 'New &Parameter'
        HelpContext = 107
        OnClick = NewParameter1Click
      end
      object NewQualifier2: TMenuItem
        Caption = 'New &Qualifier'
        HelpContext = 108
        OnClick = NewQualifier1Click
      end
    end
    object Tools1: TMenuItem
      Caption = '&Tools'
      OnClick = Tools1Click
      object mnuConsistencycheck: TMenuItem
        Action = actConsistencyCheck
      end
      object N3: TMenuItem
        Action = actTagEdit
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object FetchDatatypes: TMenuItem
        Caption = 'Fetch data types from TypeNameHandle'
        OnClick = EnsureTypesFromTypeNameHandle
      end
      object Flatten: TMenuItem
        Caption = 'Flatten package structure'
        Hint = 
          'Move all classes to topmost package, saving structure in interna' +
          'l tagged values.'
        OnClick = FlattenClick
      end
      object Boldifymodel1: TMenuItem
        Caption = 'xxx'
        OnClick = Boldifymodel1Click
      end
      object N2: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object Loggform1: TMenuItem
        Caption = 'Log form'
        GroupIndex = 1
        ShortCut = 16460
        OnClick = Loggform1Click
      end
    end
  end
  object popTree: TPopupMenu
    OnPopup = popTreePopup
    Left = 36
    Top = 160
    object DeleteXxx1: TMenuItem
      Caption = 'Delete xxx'
      HelpContext = 102
      ShortCut = 46
      OnClick = DeleteXxx1Click
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object OpenOtherEnd: TMenuItem
      Caption = 'Open Other End'
      OnClick = OpenOtherEndClick
    end
    object NewPackage1: TMenuItem
      Caption = 'New Package'
      OnClick = NewPackage1Click
    end
    object NewClass1: TMenuItem
      Caption = 'New Class'
      HelpContext = 103
      OnClick = NewClass1Click
    end
    object NewAssociation1: TMenuItem
      Caption = 'New Association'
      HelpContext = 104
      OnClick = NewAssociation1Click
    end
    object NewDatatype1: TMenuItem
      Caption = 'New Datatype'
      OnClick = NewDatatype1Click
    end
    object AddSubclass1: TMenuItem
      Caption = 'Add Subclass'
      OnClick = AddSubclass1Click
    end
    object InsertSuperclass2: TMenuItem
      Caption = 'Insert Superclass'
      OnClick = InsertSuperclass1Click
    end
    object NewAttribute1: TMenuItem
      Caption = 'New Attribute'
      HelpContext = 105
      OnClick = NewAttribute1Click
    end
    object NewOperation1: TMenuItem
      Caption = 'New Operation'
      HelpContext = 106
      OnClick = NewOperation1Click
    end
    object NewParameter1: TMenuItem
      Caption = 'New Parameter'
      HelpContext = 107
      OnClick = NewParameter1Click
    end
    object NewQualifier1: TMenuItem
      Caption = 'New Qualifier'
      HelpContext = 108
      OnClick = NewQualifier1Click
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object mnuOverrideFrameworkMethods: TMenuItem
      Caption = 'Override Framework Methods'
      HelpContext = 116
      object TMenuItem
      end
    end
    object mnuOverrideModelMethods: TMenuItem
      Caption = 'Override Model methods'
      HelpContext = 1020
      object TMenuItem
      end
    end
    object mnuOverrideInAllSubclasses: TMenuItem
      Caption = 'Override in all subclasses'
      HelpContext = 117
      Visible = False
      OnClick = mnuOverrideInAllSubclassesClick
    end
  end
  object ilTreeView: TImageList
    Left = 100
    Top = 160
    Bitmap = {
      494C010112001300040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000005000000001002000000000000050
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF000000FF000000FF000000FF000000FF000000C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000FF000000FF000000FF000000FF000000FF000000FFFFFF00FFFFFF00FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600FF000000FF000000FF000000C6C6C600C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FF000000FF000000FF000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600FF000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FF000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000840000008400000084
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000840000008400000084
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00C6C6C600C6C6C60000840000008400000084
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00FFFFFF00FFFFFF0000840000008400000084
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00C6C6C600C6C6C60000840000008400000084
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00FFFFFF00FFFFFF0000840000008400000084
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      00000000FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000FF000000FF00
      0000C6C6C600C6C6C600C6C6C600000000000000000000000000000000000000
      0000848484008484840000000000FF000000FF000000FF000000FF000000FF00
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C6C6C600FF000000FF000000FF000000C6C6
      C600C6C6C600C6C6C600C6C6C600000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FF000000FF000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C60000000000C6C6C600C6C6C600FF000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600000000000000000000000000000000000000
      0000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FF000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000FF000000FF00000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600008400000084000000840000000000000000000084848400848484000000
      0000FF000000FF00000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00008400000084000000840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484000000000084848400000000000000000000000000000000000000
      0000C6C6C600FF000000000000000000FF000000FF000000FF00C6C6C600C6C6
      C600008400000084000000840000000000000000000000000000000000000000
      0000FFFFFF00FF000000000000000000FF000000FF000000FF00FFFFFF00FFFF
      FF00008400000084000000840000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00000000008484840000000000C6C6C600C6C6C6000000
      0000C6C6C600C6C6C600000000000000FF000000FF000000FF00C6C6C600C6C6
      C6000084000000840000008400000000000000000000FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00000000000000FF000000FF000000FF00FFFFFF00FFFF
      FF00008400000084000000840000000000000000000084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      84008484840084848400848484000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FF000000FF0000000000
      0000C6C6C600C6C6C600000000000000FF000000FF000000FF00C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C6000000000000000000FF000000FF0000000000
      0000FFFFFF00FFFFFF00000000000000FF000000FF000000FF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00000000000000000000000000C6C6C600FF0000000000
      00000000FF000000FF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FF0000000000
      00000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C6000000
      00000000FF000000FF000000FF00C6C6C600C6C6C60000840000008400000084
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      00000000FF000000FF000000FF00FFFFFF00FFFFFF0000840000008400000084
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C6000000
      00000000FF000000FF000000FF00C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C6000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      00000000FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF00C6C6C600C6C6C60000840000008400000084000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF00FFFFFF00FFFFFF0000840000008400000084000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF00C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000000000848484008484
      8400848484008484840084848400000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840084848400848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000848484008484
      840000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00000000008484840000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000C6C6C600FFFF
      FF00000000008484840000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000008484840000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000C6C6C600FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6C6C600FFFF
      FF00000000008484840000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000C6C6C600FFFF
      FF00000000008484840000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000008484840000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600000000000000FF000000
      FF00000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000084000000
      FF00000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      FF000000000084848400000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000848484000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF00000000000000FF000000
      FF000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000000084000000
      FF000000000084848400000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000848484000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000848484000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF0000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C6000000000000000000FFFFFF00FFFFFF00FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C6000000000000000000FFFFFF00FFFFFF00FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C6000000000000000000FFFFFF00FFFFFF00FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C6000000000000000000FFFFFF00FFFFFF00FFFF
      FF00000000008484840084848400848484008484840084848400848484000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000848484000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C60000000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      0000C6C6C600C6C6C600C6C6C6000000000000000000FFFFFF00FFFFFF00FFFF
      FF0000000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000848484000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C6000000000000000000FFFFFF00FFFFFF00FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C6000000000000000000FFFFFF00FFFFFF00FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C6000000000000000000FFFFFF00FFFFFF00FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      00000000000000000000000000000000000000000000C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C6000000000000000000FFFFFF00FFFFFF00FFFF
      FF00000000008484840000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000500000000100010000000000800200000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      FFFFF00300000000E007E00300000000E007E00300000000E007E00300000000
      E007E00300000000E007E00300000000E007E00300000000E007E00300000000
      E007E00300000000E007E00300000000E007E00700000000FFFFFFFF00000000
      FFFFFFFF00000000FFFFFFFF00000000FFFFFFFFFC00FC00FFFFFFFFFC00FC00
      FFFFFFFFFC00F000FFFFFFFFE000E000FFFFFFFFE000E000EFF7EFF3E0008000
      DFFBC00100000000800180000000000000000000000000008001800100000000
      DFFBDFFB00070003EFF7EFF700070003FFFFFFFF00070007FFFFFFFF003F001F
      FFFFFFFF003F001FFFFFFFFF003F003FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFC01FFFFFC01F803F801F803F801F803F801F803F801F803F801F803F801F
      803F801F803F801F803F801F803F801F803F801F803F8001803F801F80018001
      803F801F803F801F803F801F803F801F803F801F803F801F803F801F803F801F
      803F801F803F801F803F803F803F803FFFFFFFFFFFFFF803FFFFFFFFF007F003
      FFFFC01FF007C003803F801F80078003803F801F80078003803F801F80078003
      803F801F80078003803F801F80078003803F801F80078003803F801F80078003
      803F801F80078003803F801F80078003803F801F80078003803F801F80078007
      803F801F803F801F803F803F803F803FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF83F1FFFFC01F87E183E1803F801F87E183E1803F801F87E183E1803F801F
      87E183E1803F801F87E183E1803F801F87E18001803F801F80018001803F801F
      87E183E1803F801F87E183E1803F801F87E183E1803F801F87E183E1803F801F
      87E183E1803F801F87E187E1803F803F00000000000000000000000000000000
      000000000000}
  end
  object ilMenu: TImageList
    Left = 472
    Top = 160
    Bitmap = {
      494C010111001300040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000005000000001002000000000000050
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400FFFFFF00C6C6
      C60084848400C6C6C600FFFFFF00C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600000000000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      840084848400848484008484840084848400848484008484840084848400FFFF
      FF0084848400FFFFFF0084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600000000000084840000000000000000000000000000000000000000000000
      0000008484000084840000000000000000000000000000000000C6C6C6000000
      0000008484000000000000000000000000008400000084000000840000008400
      00000000000000000000848484008484840084848400FFFFFF00FFFFFF00FFFF
      FF00840000008400000084000000840000000000000000000000008484000084
      8400008484000084840000848400008484000084840000848400008484000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000C6C6C600C6C6
      C600000000000084840000000000000000000000000000000000000000000000
      0000008484000084840000000000000000000000000000000000C6C6C6000000
      0000008484000000000000000000000000000000000000000000000000008400
      0000FF00000084000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00840000000000000000000000000000000000000000FFFF00000000000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000000000000000000000
      0000008484000084840000000000000000000000000000000000000000000000
      0000008484000000000000000000000000000000000000000000000000008400
      000084000000FF0000008400000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF008400000000000000000000000000000000000000FFFFFF0000FFFF000000
      0000008484000084840000848400008484000084840000848400008484000084
      8400008484000000000000000000000000000000000000000000008484000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000000000000000
      0000008484000084840000848400008484000084840000848400008484000084
      8400008484000000000000000000000000000000000000000000000000008400
      0000FF00000084000000FF00000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00840000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000008484000084840000000000000000000000000000000000000000000000
      0000008484000084840000000000000000000000000000000000000000000084
      8400008484000000000000000000000000000000000000000000000000008400
      000084000000FF0000008400000000000000FFFFFF00FFFF0000FFFFFF00FFFF
      00008400000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600000000000084840000000000000000000000000000000000000000000000
      00000084840000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6000000
      0000008484000000000000000000000000000000000000000000000000008400
      0000FF00000084000000FF00000000000000FFFF0000FFFFFF00FFFF0000FFFF
      FF00840000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600000000000084840000000000000000000000000000000000000000000000
      00000084840000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6000000
      0000008484000000000000000000000000000000000000000000000000008400
      000084000000FF0000008400000000000000FFFFFF00FFFF0000FFFFFF00FFFF
      00008400000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600000000000084840000000000000000000000000000000000000000000000
      00000084840000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000FF00000084000000FF00000000000000FFFF0000FFFFFF00FFFF0000FFFF
      FF00840000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600000000000084840000000000000000000000000000000000000000000000
      00000084840000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6000000
      0000C6C6C6000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C60000000000C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000008400000084000000840000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000FF000000008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FF00000000000000000000000000000000FF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840084008400840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF0000000000FF00008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF000000FF00000000000000000000000000000000FF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400840084008400FFFFFF00FFFFFF00C6C6C600848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF00008400000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FF000000000000000000000000FF000000FF000000FF
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      840084008400FFFFFF00FFFFFF000000000000000000C6C6C600C6C6C6008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000FF00000000000000FF
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000848484008400840084008400FFFF
      FF00FFFFFF000000000000000000840084008400840000000000C6C6C600C6C6
      C600848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FF0000000000000000FF000000FF00000000000000FF
      000000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084008400FFFFFF000000
      000000000000840084008400840084008400840084008400840000000000C6C6
      C600C6C6C6008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000FFFFFF000000
      000000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000008400
      840084008400840084000084840000FFFF008400840084008400840084000000
      0000C6C6C600C6C6C60084848400000000000000000000000000000000000000
      000000000000FF000000FF000000008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF000000000000000000000000000000FF000000FF000000FF0000000000
      000000FF000000FF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084008400840084008400
      8400840084008400840084008400008484008400840084008400840084008400
      840000000000C6C6C60000000000000000000000000000000000000000000000
      000000000000FF0000000000FF00008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF000000FF00000000000000FF000000FFFFFF00FFFFFF00FFFF
      FF000000000000FF000000FF0000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084008400FFFFFF008400
      84008400840084008400840084008400840000FFFF0000FFFF00840084008400
      8400840084000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF00008400000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FF00000000000000FF000000FF000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084008400FFFF
      FF0084008400840084008400840084008400840084000084840000FFFF0000FF
      FF00840084008400840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF000000FF00000000000000FF000000FFFFFF00FFFFFF00FFFF
      FF00FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      8400FFFFFF00840084008400840084008400008484008400840000FFFF0000FF
      FF00840084008400840084008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FF00000000000000FF000000FF000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084008400FFFFFF00840084008400840000FFFF0000FFFF0000FFFF008400
      84008400840084008400000000000000000000000000FF000000FF0000000084
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084008400FFFFFF00840084008400840084008400840084008400
      84000000000000000000000000000000000000000000FF0000000000FF000084
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FF000000FF000000FF000000FF000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084008400FFFFFF008400840084008400000000000000
      000000000000000000000000000000000000000000000000FF000000FF000084
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000000
      0000840000000000000000000000840000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF00000000000000000000000000000000000000000084848400008484008484
      8400008484008484840084000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF00840000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00840000000000000000000000000000000000FF000000
      FF000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000848400848484000084
      8400848484000084840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000FFFFFF000000000000000000000000000000
      000000000000FFFFFF008400000000000000000000000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF0000000000000000000000000084848400008484008484
      8400008484008484840084000000FFFFFF00000000000000000000000000FFFF
      FF00840000008400000084000000840000000000000000000000000000000000
      0000000000008400000084000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF008400000000000000000000000000FF00000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      00000000FF000000FF0000000000000000000000000000848400848484000084
      8400848484000084840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084000000FFFFFF0084000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000840000008400
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000FFFFFF000000000000000000000000000000
      000000000000FFFFFF0084000000000000000000FF000000FF00000000000000
      00000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000FF000000FF00000000000000000084848400008484008484
      8400008484008484840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000FF000000FF00000000000000
      0000000000000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000FF000000FF00000000000000000000848400848484000084
      8400848484000084840084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000FFFFFF000000000000000000FFFFFF008400
      0000840000008400000084000000000000000000FF000000FF00000000000000
      000000000000000000000000FF000000FF000000FF0000000000000000000000
      0000000000000000FF000000FF00000000000000000084848400008484008484
      8400008484008484840000848400848484000084840084848400008484008484
      8400008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      0000FFFFFF008400000000000000000000000000FF000000FF00000000000000
      00000000000000000000000000000000FF000000FF000000FF00000000000000
      0000000000000000FF000000FF00000000000000000000848400848484000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      0000840000000000000000000000000000000000FF000000FF00000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      0000000000000000FF000000FF00000000000000000084848400848484000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600000000008484
      8400008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000084000000840000008400000084000000840000008400
      000000000000000000000000000000000000000000000000FF000000FF000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF00000000000000FF0000000000000000000000000000848400848484000084
      84000000000000FFFF00000000000000000000FFFF0000000000848484000084
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF0000FFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000000000848484008484
      8400848484008484840084848400000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FF000000FF
      000000FF000000FF000000FF000000FF000000FF000000000000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000500000000100010000000000800200000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000FFFF000000000000
      FFFF000000000000FFFF000000000000FFFF000000000000FFFF000000000000
      FFF7000000000000C1F7000000000000C3FB000000000000C7FB000000000000
      CBFB000000000000DCF7000000000000FF0F000000000000FFFF000000000000
      FFFF000000000000FFFF000000000000FFFFFF7EFFFFFFFFC001BFFF0000FFFF
      8001F0030000001F8001E0030000000F8001E003E00700078001E003E0070003
      8001E003E007000180012003E00700008001E002E007001F8001E003E007001F
      8001E003E007001F8001E003E0078FF18001E003FFFFFFF98001FFFFF81FFF75
      8001BF7DF81FFF8FFFFF7F7EF81FFFFFFFFFFFFFB07FC1DFFFFFFE3F807FC18F
      EFFDF81FB049C18FC7FFE00FB07F8107C3FB8007B07FC107E3F70003BFFFC003
      F1E70001B07FC103F8CF00008049F701FC1F0001B07FC100FE3F8001B07FC101
      FC1FC001B07FC107F8CFE00007FF8107E1E7F000048FC1DFC3F3F80307FFC01F
      C7FDFC0F07FFC1FFFFFFFE3F07FFFFFFFFFFFFFFFFFFFFFFFC00F9FFFFFFF83F
      8000F6CFFC01E00F0000F6B7FC01CFC70000F6B7FC0187E30000F8B70001A3F3
      0001FE8F000131F90003FE3F000138F90003FF7F00013C790003FE3F00033E39
      0003FEBF00073F190003FC9F000F9F8B0003FDDF00FF8FC38007FDDF01FFC7E7
      F87FFDDF03FFE00FFFFFFFFFFFFFF83FBFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFF
      B049C01FC01FC01F807F801F801F801FB07F801F801F801FB9FF801F801F801F
      BFFF801F801F801FB049801F801F801F807F8001801F801FB07F8001801F801F
      B9FF801F801F801FBFFF801F801F801F048F801F801F801F07FF801F801F801F
      07FF801F801F801F9FFF803F803F803F00000000000000000000000000000000
      000000000000}
  end
  object AppEvents: TApplicationEvents
    OnIdle = AppEventsIdle
    OnHint = AppEventsHint
    Left = 346
    Top = 157
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 274
    Top = 443
    PixelsPerInch = 96
    object cxBoldStyle: TcxStyle
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
    end
  end
  object ActionList1: TActionList
    Images = cxImageList1
    Left = 274
    Top = 323
    object BoldUndoAction1: TBoldUndoAction
      Category = 'Bold Actions'
      Caption = 'Undo'
      Enabled = False
      ImageIndex = 0
      ShortCut = 16474
    end
    object actFind: TAction
      Caption = 'Find'
      ShortCut = 16454
    end
    object BoldRedoAction1: TBoldRedoAction
      Category = 'Bold Actions'
      Caption = 'Redo'
      Enabled = False
      ImageIndex = 1
      ShortCut = 24666
    end
    object actDelete: TBoldAction
      Category = 'Bold Actions'
      BoldHandle = dmBoldUMLModelEditorHandles.brhCurrentElement
      BoldProperties.Expression = ''
      BoldCaption.Expression = #39'Delete '#39' + asString'
      BoldEnabled.Expression = ''
      BoldEnabled.NilRepresentation = True
      ShortCut = 49231
      BoldVisible.Expression = ''
      BoldVisible.NilRepresentation = True
    end
    object actAddSubclass: TBoldAction
      Category = 'Bold Actions'
      BoldProperties.Expression = ''
      BoldCaption.Expression = ''
      BoldEnabled.Expression = ''
      BoldEnabled.NilRepresentation = True
      BoldVisible.Expression = ''
      BoldVisible.NilRepresentation = True
    end
    object actNewOperation: TBoldAction
      Category = 'Bold Actions'
      BoldHandle = dmBoldUMLModelEditorHandles.brhCurrentElement
      BoldProperties.Expression = ''
      BoldCaption.Expression = ''
      BoldEnabled.Expression = ''
      BoldEnabled.NilRepresentation = True
      BoldVisible.Expression = ''
      BoldVisible.NilRepresentation = True
    end
    object actAddDb: TAction
      Caption = '+'
      Hint = 'Add db connection'
      ImageIndex = 7
      OnExecute = actAddDbExecute
    end
    object FileOpen1: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Hint = 'Open|Opens an existing Bold model file'
      ImageIndex = 15
      ShortCut = 16463
      BeforeExecute = FileOpen1BeforeExecute
      OnAccept = FileOpen1Accept
    end
    object FileSaveAs1: TFileSaveAs
      Category = 'File'
      Caption = 'Save &As...'
      Dialog.DefaultExt = 'bld'
      Hint = 'Save As|Saves model to file'
      ImageIndex = 12
      ShortCut = 16467
      BeforeExecute = FileSaveAs1BeforeExecute
      OnAccept = FileSaveAs1Accept
    end
    object actConsistencyCheck: TAction
      Caption = '&Consistency Check'
      Hint = 'Check model consistency'
      ImageIndex = 11
      OnExecute = actConsistencyCheckExecute
    end
    object actTagEdit: TAction
      Caption = '&Edit tagged values'
      Hint = 'Edit tagged values'
      ImageIndex = 0
      ShortCut = 16468
      OnExecute = actTagEditExecute
      OnUpdate = actTagEditUpdate
    end
    object actInsertSuperClass: TBoldAction
      Category = 'Bold Actions'
      BoldProperties.Expression = ''
      BoldCaption.Expression = ''
      BoldEnabled.Expression = ''
      BoldEnabled.NilRepresentation = True
      BoldVisible.Expression = ''
      BoldVisible.NilRepresentation = True
    end
    object actNewClass: TBoldAction
      Category = 'Bold Actions'
      BoldHandle = dmBoldUMLModelEditorHandles.brhCurrentElement
      BoldProperties.Expression = ''
      BoldCaption.Expression = ''
      BoldEnabled.Expression = ''
      BoldEnabled.NilRepresentation = True
      BoldVisible.Expression = ''
      BoldVisible.NilRepresentation = True
    end
    object actNewAssociation: TBoldAction
      Category = 'Bold Actions'
      BoldProperties.Expression = ''
      BoldCaption.Expression = ''
      BoldEnabled.Expression = ''
      BoldEnabled.NilRepresentation = True
      BoldVisible.Expression = ''
      BoldVisible.NilRepresentation = True
    end
    object actNewDataType: TBoldAction
      Category = 'Bold Actions'
      BoldProperties.Expression = ''
      BoldCaption.Expression = ''
      BoldEnabled.Expression = ''
      BoldEnabled.NilRepresentation = True
      BoldVisible.Expression = ''
      BoldVisible.NilRepresentation = True
    end
    object actNewAttribute: TBoldAction
      Category = 'Bold Actions'
      BoldProperties.Expression = ''
      BoldCaption.Expression = ''
      BoldEnabled.Expression = ''
      BoldEnabled.NilRepresentation = True
      BoldVisible.Expression = ''
      BoldVisible.NilRepresentation = True
    end
    object actNewParameter: TBoldAction
      Category = 'Bold Actions'
      BoldProperties.Expression = ''
      BoldCaption.Expression = ''
      BoldEnabled.Expression = ''
      BoldEnabled.NilRepresentation = True
      BoldVisible.Expression = ''
      BoldVisible.NilRepresentation = True
    end
    object actNewQualifier: TBoldAction
      Category = 'Bold Actions'
      BoldProperties.Expression = ''
      BoldCaption.Expression = ''
      BoldEnabled.Expression = ''
      BoldEnabled.NilRepresentation = True
      BoldVisible.Expression = ''
      BoldVisible.NilRepresentation = True
    end
    object actSelectSuperClass: TAction
      Caption = #9650
      OnExecute = actSelectSuperClassExecute
      OnUpdate = actSelectSuperClassUpdate
    end
  end
  object dxBarManager1: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    CanCustomize = False
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    PopupMenuLinks = <>
    UseSystemFont = True
    Left = 278
    Top = 211
    PixelsPerInch = 96
    DockControlHeights = (
      0
      0
      28
      0)
    object dxBarManager1Bar1: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      Caption = 'Custom 1'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 1370
      FloatTop = 2
      FloatClientWidth = 0
      FloatClientHeight = 0
      Images = ilMenu
      IsMainMenu = True
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton4'
        end
        item
          Visible = True
          ItemName = 'dxBarButton5'
        end>
      MultiLine = True
      OneOnRow = False
      Row = 0
      ShowMark = False
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxBarManager1Bar2: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      Caption = 'Database'
      CaptionButtons = <
        item
          Hint = 'bguhb'
        end>
      DockedDockingStyle = dsTop
      DockedLeft = 1068
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 1370
      FloatTop = 2
      FloatClientWidth = 0
      FloatClientHeight = 0
      Images = cxImageList1
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxComboDatabase'
        end
        item
          Visible = True
          ItemName = 'dxBarButton1'
        end>
      OneOnRow = False
      Row = 0
      ShowMark = False
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxBarManager1Bar3: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      Caption = 'Undo'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 118
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 1370
      FloatTop = 2
      FloatClientWidth = 0
      FloatClientHeight = 0
      Images = cxImageList1
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton2'
        end
        item
          Visible = True
          ItemName = 'dxBarButton3'
        end>
      OneOnRow = False
      Row = 0
      ShowMark = False
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxBarManager1Bar4: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      Caption = 'Tools'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 59
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 1370
      FloatTop = 2
      FloatClientWidth = 0
      FloatClientHeight = 0
      Images = ilMenu
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton6'
        end
        item
          Visible = True
          ItemName = 'dxBarButton7'
        end>
      OneOnRow = False
      Row = 0
      ShowMark = False
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxComboDatabase: TdxBarCombo
      Align = iaRight
      Caption = 'Database'
      Category = 0
      Hint = 'Database'
      Visible = ivAlways
      OnChange = dxComboDatabaseChange
      ShowCaption = True
      Width = 160
      ShowEditor = False
      ItemIndex = -1
    end
    object dxBarButton1: TdxBarButton
      Action = actAddDb
      Category = 0
    end
    object dxBarButton2: TdxBarButton
      Action = BoldUndoAction1
      Category = 0
    end
    object dxBarButton3: TdxBarButton
      Action = BoldRedoAction1
      Category = 0
    end
    object dxBarButton4: TdxBarButton
      Action = FileOpen1
      Category = 0
    end
    object dxBarButton5: TdxBarButton
      Action = FileSaveAs1
      Category = 0
    end
    object dxBarButton6: TdxBarButton
      Action = actConsistencyCheck
      Category = 0
    end
    object dxBarButton7: TdxBarButton
      Action = actTagEdit
      Category = 0
    end
  end
  object BoldPropertyMapper1: TBoldPropertyMapper
    MappingCollection = <
      item
        BoldHandle = dmBoldUMLModelEditorHandles.behOclExpressions
        BoldProperties.Expression = #39'OCL expressions ('#39' + self->size.asString + '#39')'#39
        BoldProperties.NilRepresentation = 'OCL expressions'
        VCLComponent = TabOCL
        VCLProperty = 'Caption'
      end
      item
        BoldHandle = dmBoldUMLModelEditorHandles.behClassOperations
        BoldProperties.Expression = #39'Operations ('#39' + self->size.asString + '#39')'#39
        BoldProperties.NilRepresentation = 'Operations'
        VCLComponent = tabClientOperations
        VCLProperty = 'Caption'
      end
      item
        BoldHandle = dmBoldUMLModelEditorHandles.behModelClasses
        BoldProperties.Expression = #39'Classes ('#39' + self->size.asString + '#39')'#39
        BoldProperties.NilRepresentation = 'Classes'
        VCLComponent = tsClasses
        VCLProperty = 'Caption'
      end
      item
        BoldHandle = dmBoldUMLModelEditorHandles.behClassAssociations
        BoldProperties.Expression = #39'Associations ('#39' + self->size.safediv(2).asString + '#39')'#39
        BoldProperties.NilRepresentation = 'Assocations'
        VCLComponent = tabAssociations
        VCLProperty = 'Caption'
      end
      item
        BoldHandle = dmBoldUMLModelEditorHandles.behClassAssociationEnds
        BoldProperties.Expression = #39'Association ends ('#39' + self->size.asString + '#39')'#39
        BoldProperties.NilRepresentation = 'Association ends'
        VCLComponent = tabAssociationEnds
        VCLProperty = 'Caption'
      end
      item
        BoldHandle = dmBoldUMLModelEditorHandles.behClassAttributes
        BoldProperties.Expression = #39'Attributes ('#39' + self->size.asString + '#39')'#39
        BoldProperties.NilRepresentation = 'Attributes'
        VCLComponent = tabAttributes
        VCLProperty = 'Caption'
      end
      item
        BoldHandle = dmBoldUMLModelEditorHandles.behAssociations
        BoldProperties.Expression = #39'Associations ('#39' + self->size.asString + '#39')'#39
        BoldProperties.NilRepresentation = 'Associations'
        VCLComponent = tsAssociations
        VCLProperty = 'Caption'
      end
      item
        BoldHandle = dmBoldUMLModelEditorHandles.behDataTypes
        BoldProperties.Expression = #39'Data types ('#39' + self->size.asString + '#39')'#39
        VCLComponent = tsDataTypes2
        VCLProperty = 'Caption'
      end
      item
        BoldHandle = dmBoldUMLModelEditorHandles.behClassFeatures
        BoldProperties.Expression = #39'Features ('#39' + self->size.asString + '#39')'#39
        VCLComponent = tabFeatures
        VCLProperty = 'Caption'
      end
      item
        BoldHandle = dmBoldUMLModelEditorHandles.behPackages
        BoldProperties.Expression = #39'Packages ('#39' + self->size.asString + '#39')'#39
        VCLComponent = tsPackages
        VCLProperty = 'Caption'
      end>
    Enabled = True
    Left = 278
    Top = 268
  end
  object cxImageList1: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 10551822
    ImageInfo = <
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000016744558745469746C6500556E646F3B4172726F773B456469743BCB
          5CF1270000025C49444154785EB5936F48535F18C7BF77D7692BB1561626134D
          63B319230D86FDF951146868BEB0041B2EFB372367D01FEA45FBC1828CD4CA32
          FF65D60BC3B48282B0863175B6B5325A83A814B360D82B11493252B776EFD3BD
          372FAC17BD920E7C38700E9F2F0FCF730E4344F8574B21C00A307F15E4CBB3F7
          FC20020840A569BD28B0275A7A8A793EBA36F0DEADCDD8B0638A40206204688E
          8814DB1D1FBEF03C44F1E0F9CE55A76E3D7FDCDA3F4296DA5E02B04840295733
          323B0399A3D75FFC5189D2DAE0B4DA3B7C537D81090A703C992A9F5045B367F4
          485D6FCFFEEAAEE359397B97018892832C75EEDF7269E57DBDB5D1EDBDE11AA1
          E199107D0C71F46186A3518EA3B7933FC8F9699CEA9F0E51D9D5FEB1DD673A4A
          01C4889EB9BA4FEA015B56F76CACB8401F9F90A8C66C9891BA1726603AC44B84
          391E6A9560D14F74BB3EE3B56FD8EEB85C720940500C5064159DD3A996689A75
          86B4ADD9DB32A18C66C18BD982480078024004250B68E315E8720C62C0EBDFE7
          B9597E9789189932A3B0C6A25E9150BD29D7189BA2D3C0D1E1C1F4F76F58BC34
          0E49698948D5A740C13058AD2634B478C7075D6D99929D5E784D6E649426DBA2
          D3EEACE9CEB33DA2ED27BB2839BBD4B826DF5E6228AAEDDC7CF876F058E73B6A
          F47F255BFB1B5A676AF85F0A48CDBB288F46AE46A5D972FAD0C6F287130016CE
          755E956434AFCDD875C579A0E9253579026430B7BD92029273AB2459461EABC0
          020136E29C15884B2FA86AB3B60E50FA9EF609460AC8A99294C8474102B1CB93
          C10427E557279D0D3DA8508857DAFC0BF59C6AE57FCC3CFE498CB8CFFB372A30
          CFF50B153D09AADDB2FD980000000049454E44AE426082}
        FileName = 'Images\History\Undo_16x16.png'
        Keywords = 'History;Undo'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000016744558745469746C65005265646F3B4172726F773B456469743BE8
          51AFAC0000027149444154785EA5936B48536118C75FB5D22E441F823EF4C142
          8C42A36610EB43694297052D4C74B92E88612BDD9C6662686A8BC5228D993835
          2F09A5D3722C2AD772B5C63684D11C95D66CD34AADF036237739665B3EBDE79C
          1DFBD087A03DF0E3BCBC3CFFFFC3C3FF3D080042E25F1586598289089EFFAE8A
          4E1B2AEFB0A13212651FBAD46E45A5246D56C43D7765B550619814D6684E62C1
          324C7851AB055DB84D4355096E747803683AB080BE2D0033351CB314B332ABF2
          19343D7780A8CEA03922BAB129781F26ACEFA50D8AB093DD1DA04C4861224FBC
          4620D7E6E5D61A74B975A6519EA41BBEFA17C034340DB8D7C39774E563D9727A
          2D5C790DBD8BBB66CA1E6508EBCDE3CD7A2798865DF07E8680492C1E207E8163
          2E001F3CF350ADB14386F4892539EB5A0265927DD38848F189AB8F8BA5AA7E30
          7EF68165C60F2FBFFBC1E6F6439F3B00167C7E31350FEA1102B46373D0699B80
          940AAD0BEB2211E9925A729F57A67C05FAF19F70779880162766D00BCD98463B
          E69D071ADEBAA1C63A03FC4A3DEC385E6F8E49CA615109251CCE5F7B4CDA33A1
          76CEC22DBB0F14FD6E28C06647F1EEC92225708ABBA1F6F52C9CBF3700ECD3AD
          9E58CEE5022C5CB518ED3EB1B2B054F9064FF041917A101273EEFCD89E2E6F8B
          E34A32A2D9A77626891FC2C18B0F20E690AC671D8B1FC7C419CB9523AAF68ABA
          CC72E31738D362856D69D5DA8DBB059BF17554F001AD600B54AEF57B0AB3C948
          99A9BA111FDAC0B94E1BEC3AAB9ACAC1E2F8D4AA265240BAEBC608A41B255050
          10C564FFF49317693FD2441F90D106ACCC8EA1B894AA4646BC35BD0EC5A79128
          28B6F0DBC9669AFDF49781A948662FF41F15EADF18BAC16F3920CB3E41393A12
          0000000049454E44AE426082}
        FileName = 'Images\History\Redo_16x16.png'
        Keywords = 'History;Redo'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001B744558745469746C65004164643B506C75733B426172733B526962
          626F6E3B9506332F0000036349444154785E35927D6C535518C69F73EE6DEB64
          63A3AEFB60A3A36E33B8C56581E0D8707E21CC1A43A2A22304FE3001512A86C4
          E900132451FF503367420043B244364C483031465C248B4441C0980C45B4D065
          CDBA4ECAE82AAC5DBBDE8FF3E1BD27F1397973DE9C3CBFF7233964226FC2D543
          A53E0280443E3FD752525AB14323FA06685A3381E492F329C6ADF39954E2F8C9
          C3DBA6018858DE940A9C2C5870C1D51BB6FAF61DBB327860F81A1BFE25297FB8
          3127C7EFE4E5D5745E9EBB9991239766E481937FE4DE1818DB0DC0EB322EABBA
          B63FD5EB7D6CCBBE6F1B83FE9E67BA82E084C0E4123697CAE0D109BC94805B0C
          E7AFCC606A66EEECF75FBCBB753AFAEB2201A0BD3E7861B02914D8DBF34408A9
          AC0D2181D3672E23319D81AB950D016CEBED824E809A722FC62E4CE17A343130
          D4DF73507FB9FFAB551E9F6FCF93EB82B879BB088D52504A14FCC9CE4E95F79D
          B80CD396284A8179C7D3DD1144F29FEC5BE1D73E1BA6BEB2C09BEDCD955A7CCE
          44D1744C1687C9045C05EBFC686F0DAADCB08413D2098E89B4E1BC5779965687
          5ED585D03ACBFDA548E7197EFA711C776EDFC5FF12200A7075F4E85975D7D4FA
          F1F4A635A82C5F02A2956CD46D2EEB1D160B455BC19FEE5E0F4A885A45828071
          81137D1B61DB0C1E5D43E4C8CF5858E4D0A1810BBA5CB76DEEBDB768C1E604AE
          EA6B1F40D9121F0A265385BC0E5457530109404A8010E27805EEE60598CDA15B
          8699C8E7CD4784EEC3F2BA00767C340A4AA9327E79300CE1505BDEFF0E9AA681
          5082150DD5604CA26858282E1693D428E42F6666B3909068EF68C5E6171FC7E6
          17BA611A260C93A9029C713CF7FC3A3C1BEE404B5B2398E0989FCBA190FD774C
          CFA46243B11B4B77ADADF67BB236478E10500AA5D2121D5C48354D3A674108A1
          56114C201E4BB1D9F86FA70880FB1EDD3E34B0A229B4E7E1350FC2E22E2011BF
          16C3FCBD050557562DC3CA964608B8B4C4E49F4924A27F1F193F1DD9AF03B0FE
          1AFDE03D113EDC6431B1A96575089212B4AD6D555F581280D902398343308EC9
          EB49DC9A981A75E043000CA46D09005A49457059DB4BC78E77EDFCDAEAFDF892
          DC3B1295EF7C13977D4E444E45E52BCE5BE7AE338555E10FDF0650EE32B30E4B
          D24C0212A8F210EAAED3D01969BB3FD0BCDDE32BEB06D56AD5D09CCDDA66EE62
          EED6EF43A9AB2331008603ABCEFF019D3AAD15CCD8D2E00000000049454E44AE
          426082}
        FileName = 'Images\Actions\Add_16x16.png'
        Keywords = 'Actions;Add'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000019744558745469746C650044656C6574653B52656D6F76653B4D696E
          7573EBA98F410000031A49444154785E7D925F4854591CC7BFE7DE3BA3B9EEBA
          0D3BDBE85A3AA5C16A85586DB4B49459490FBD444B62C63EB450ADECD243D4DA
          4345F4982804D58B0F1541FBB02C0B1995AD44160A8B59499A8330E3486AEA4E
          EBCC9D3F77EE3DE7F43BF78A8F9D3F1CB8E77EBE7C7E3F0E8B9816D4585F5CC0
          00B0B6AE87352B8ABF3CAA33A311BA5ECD20B9E43CEAF07CDFC274ECFACD8B2D
          9300C4B86949179C48E7A1C0FAC6E68253D7063ADB6FBD726E3D8BCB0763F372
          E8BD295FCC99F2E1DB0579FBF9946CBFF93279B2A3F73800BF6214CB145CD770
          D8BFE3D0A9BFD7AD0934EDF97E0D3863B0B8844D5B69F90C06BFC6C0F30EFA06
          A6109D9ABFD773E34CF3E4E86046DDEB273A9F76568583BF36ED0C637AD18690
          804680DAF04421A404E52054E247EFD3284646631DDD679BCEB11FCFDEA90D55
          54BDFAE9E0063D9AC843D73417D469330DCB43081522C0395019F0E3CE9F23F6
          D8F0B32DACF5F2A3EB0DDBD69D280EAD84E500BA0E18068530461BCB830BCF82
          73E19AD98945F4F40E771942E8DB4B02C598331D3CF96708EF6712F8D4089506
          B07BDF667C55F21998BE62AF418D2A2716A9ACEDC2577F6BF04AD0DD52BC2E33
          AF177E9F8E63571E2395E130A093955666D836F77FC8E46173CF7755B0044585
          3EA2D4F202A49AEA945E98CD0512A6806353503E67C54CD3DA288C02947D1344
          CBA57B64A0B93F1A7E03B66543231B5DD7C1C86875C52A388E4436974736938D
          1BB9B4D9BF30BBB87165791075DB6A51E8F31A48CBB3905063D9C07238523907
          FFCF27915EFCAF976D3A70A1BEAC7AEBE0D65DB53EEED54B06DEA9A65A588285
          106E9070045E3F7FEB4406FFDAA2AE0BBF6BEDEE585D15FEE5DBCD6B91E79260
          E95A80B632904B0E028A9698781D476CF4CDD5A1BB6DBFAB00AD2850F945CDFE
          8B7F9486CBF7D5D4872135A6743DCC2D4312CE20487F62248E7791E8FDA1BB3F
          1F0190D4E66C29328958EA4DCFF9E6C9B1C88DFEFBC3F6E4F80C72664EE1AE7B
          266D211E99C1C0A39799C8BF03A7096E01909A2596CD399EE3D73EA6A9722AB6
          B76D2A0A56B7FA0A3EFF019A5EEA4A7367D6B692FDC977C3DDD32F6E8F03C811
          EC3A7E0415947A6BBEDAC8770000000049454E44AE426082}
        FileName = 'Images\Actions\Remove_16x16.png'
        Keywords = 'Actions;Remove'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000023744558745469746C650043616E63656C3B53746F703B457869743B
          426172733B526962626F6E3B4C9696B20000038849444154785E1D906B4C5367
          18C7FF3DBD40CB1A2E32B55C9D598B4CA675D8D13836652E9B0359B67D589665
          3259E644A52571644474CB4CB6ECC23770C4449DD38D2885005E4683AB69C616
          8DA12384264EC8AAAC0C9149A1175ACEE9E939CFDE9EE7E477F2CBFFB924E720
          E6E943CC3B8895D12B00A0FEE3D08167A75A5BBAEEB71D9D081E6B4DA549FBDD
          A3CEEFDD1F3658016818AA98A71FD1915E202DE980A19D741E3EF6E0F8A7FC7F
          673B6979E002C5BC43B4C2581EB8480BE7BA68E6441BEF3B72F03300990C8E1D
          5016554E7B55D6C1ED9543C6C2B5BB739FDF025988838424E4240F10A0D2EAA0
          D26540AD37203CFE17C2C187A3EDBFDE7CF3DAD4748403A06EA8A8E830AC5FB3
          3B7BAB1901B717AE23DFE1CEC5EBEC90A0E0EB71A3CFD981C0B017C6F252180B
          D6BD74BCFA856E003A0CBDFD966DF250532AD4FF038DB734D18557DF21CFB08F
          2E37B5D370ED5E72D7D52BEEF9654CE9F91C1FD392EB0C4D3A0E4BE7F6ECD909
          CFDEFAD381AF4ED0A3D35FD399E272BA3F3D478F971234FD2044BDCE930AF798
          CF2FAED0DF5373CACCFCA92F2970B29DDCAFD7F56B48945E918201C41738945A
          2D581C7461ADA3192AB50AD64F9A010272730CC8D4AA313BE44289D58CF85D3F
          2411504BB28D93845489145E041F9CC1863C09A11BD7E1EFEA86240339463DB2
          B3F59025C0DFD98DD0C83594E6886C360831F408523265D208BC0021B20A35A7
          82B8BC0429C2239A10D812417988007088B14C8A8421EA75A094044A8A48F200
          17E78587629220B370E69F2884EA3750F07E23245946868E43A64EA3B8695F23
          F8EA7A046763EC780AC9640AF155FEB1269AE0BD91AC8CFDF910108E26F15A5B
          33788D1E860CF6CDE7CF225D45FB3F02A0C7CE36076E5CBD84825C3562A20E4B
          097E0CAD051B5FFCA97C9BE4ABAEA05B2FDBE9E6BE0F880F8568FCDB0E1AA9AA
          646C579C654AEF564D15FDB96333FDBCC94A8E751B6A0140DF5168B9E42A7B86
          266AB6D2ED1A1BF559CAC853B58DFCB576F2D7D9D3AE64B777D96862D716EA2F
          2BA76F4CE62B008C1A00C2F9C57F9D8DA2C99212C5E72C85323699F320A77FD2
          72040021DF9885F56BF2204457706F9EC74C4CF2F744169A012430DBF21E00A8
          2B754F98BEC82EEEED7AF2291A306FA451EBD3346633938FF13BF341969D62BD
          CF738AAF6ED6EA4B006882CE77A14ABFD255D2799903606830E4EF28E274070C
          1C67D74255041044C25C9CE43B4149F8B16735F41B8038DB9300E07F6924ECFB
          01D589CC0000000049454E44AE426082}
        FileName = 'Images\Actions\Cancel_16x16.png'
        Keywords = 'Actions;Cancel'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001D744558745469746C6500436C6F73653B457869743B426172733B52
          6962626F6E3B4603B9E8000002AD49444154785E85934B4C546714C77FF73232
          0C041DC2237644C368F109868D98A0290BDA60E2DE9526921856C3C2A08C2E1A
          A336A64DB48D81A8892D6E241A7CA20B1530261849DA4D47596818082F015118
          0698B973DFB7773EB1333BEE97FFCD3927DFF97DE77B1CE9E3AF17293FF3B30C
          48426B7D9939765A1290337AE1DC4B8F2CD73B0E802306C24CEB9BE7ACFA6206
          86650FECFCE55283079025DBAE2F3D5CB79A94B58E206692C5CF1606933DAF7F
          006451BA69986099A8C3C38C3C7846D791D3BCB97413351A151AFCADD38D8589
          DE7F412A3A426A388AC801D92300A689E36A2A1225129923D8D1C9FC9387FCDD
          D587244BC4037BDC581B91AB97716C93F2AA6D989A004802A0AF28382EF19F3B
          FD34F4F7515852C4E2AE20FFFED10140CDC9101B0ABD54B4FFCEAB1F7F62D3F9
          0AB4643203589E9C469F9926B8BF8AF947DD94B5849072246A5A43E040913F9F
          BC75394C3DEE2658BB9BD4F07BE2E3B35915A83AFAA719766CCDE7C3403F43C8
          6C6D6EC65FE8C391C0B660E8FA359203BD546EF7A34E4C60A49C2C8066602514
          B4D94F98F12596150DDB721043DC84CC4A3A168FA14FA75CA085A17D05885B30
          D280548AE8FB39947D8D048E3661D936DE5C99BC5C8FB0BF3BD684527B88D1D1
          18B6AAE22E9A5D8186954C105B50680C87503D3EF2BDEE9E6FFD0940F9F11380
          8FFA700BBD4FEEB2A94842D73D990A74DDC45C49B2A37A336FDB4E5122AB8C5F
          6FE7CBBDDB7C763571A39D328FCEBB702BDF6F2FC352750C4DCFBC0343750189
          243E3381131BE369DD018A4BD71328CE136730D7D3CDD3CEBFD8B2A518AF99C0
          4C030CFEDF82EBE802602C2CE22DC8676F7500633981E2DE0C1694FAFD6C2C29
          419D8FA12515B06C4C43461080822B6595CF7D927410475A3DF9ACA6B0859FDD
          50C2566C7BF0ECD258A3E846200FF066B5AAB4462B3B800628FF0122CC6063F4
          5F96130000000049454E44AE426082}
        FileName = 'Images\Actions\Close_16x16.png'
        Keywords = 'Actions;Close'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001D744558745469746C650044617461626173653B44423B536F757263
          653B53746F72ACB1EA720000029849444154785E6591CB6E5B551486BF73B54F
          E2D83881242D0E992006A1158A50C78C90102378843E04521F821740883760C4
          089870197480A0266949012548CD055F523BCEB1CFDEC7FBCA91155509D9D2BF
          B4F6607D5AFA56ECBD27A8DE379F7FD2B9B3B6FC300E8307C0561470CF7B1779
          07CE393B57F64069DB97A5F9FDE9D1CBAF3EFBE2F161356B630020DADE683C5E
          DFF9A8535FDD265D6A506FAD81156004CECC22797E725F89FC7E311A7E580B7E
          FD14780F10AF00DEB8CE1BEF7E4018BF0601E00C04293ECC08A30AD88E486A23
          965A2BFCF3E497B78198AB0210686DF0F204965382A8859507E8FC296A7A48D9
          D35CFCFD82B896D2DE5EC7280B10DE005865F1DE501500F016BCC6BB2A56E34C
          952858F4466980E006E0F27CD21D1D1DEC36DE6C92AE4454F6F06E015D00CAA9
          C0E505CE49F2F1B40BD8EB80306BB576C32463D23FC39E0C91839F5093BF984F
          7B889EA518CCC896EB34D63649927817486F385042D2DEDA265AD9210C5B38B9
          86CA9F2D1CC833C1E88F43C2C8B37AB78DD97F79DB812A15DE4CC1CD21747835
          C2C933F4EC183551949311719A62551DA36F3BB0F968DA3DAF1C34DFDAA0D64C
          883078A7C0AA8540994B3C822052CC2E451750D70169D66CEC46499DCBC129F6
          6C8C38FD8E72FC1C39E9333BB5E4FF0AB29584D67A46142F1C64C0C52B07F342
          B2DAE910B71F10C6AF63DEC9D09327C88B3F295E48FADD23C2D0B37AA789FB6D
          7CFB8C66AE703A07570206AF8698E2189D1F538E75051A93D412ACAA61B503E0
          3AC088520FF6BEFF796373A746EBEE3D966B8BFBE3AA78AD91A541CCA1783EA1
          28F5C9FF1D886FF7FB1F1FF77E78B814FFF87E12F84E3D0CB69C7578E7AB3884
          B63DA1DC69A1DCB3BD73F92590DFD8E0EBBDE13EF0084881E40A1E5C05C00016
          9803F2EACF7FF1BE83483B8CD56F0000000049454E44AE426082}
        FileName = 'Images\Data\Database_16x16.png'
        Keywords = 'Data;Database'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000030744558745469746C650044423B536F757263653B53746F723B6461
          7461736F757263653B44617461626173653B4164643B4E657722FBFACB000002
          8B49444154785E7DCE5168D56518C7F1EFFB3FEF71E8118732A9664AD344E782
          A4E8C2080BBD58811D8DD0C0180B73154A1122E1A54CD02B9D1769434548BA91
          290C4756A363CB226D93A00BBBB0CCCE6173B5ADE65C9EFDCFFFFF3ECFE3F042
          C641F681EFEDF3FC1C90F9AEE3CD2DD94CB43E72ACC0F429CC3589CAF510B418
          52B95949C2E54DFBBFEE06C4A631D3954FB7156EF4B4DBEDFE33F6EF6FDD3659
          EC351DBF6A776F74D9C8C0312B7EDB6EBF9E79DF7ADB5FFB01F066C6CC3CA61B
          5636BF0726A0092615D029E6D72D21B770112665EA1B1BF9E7E4A99780882A5E
          5221DCFB0B9F6B80081C8AA16814884787A8FC37089561240D008E2A918AA2E9
          04A13C02CC211DBFC264B193D16B1DFC79A19352A19BCA580909F2C8035E5361
          367E5E2DD9BA35D4AD5A0EF4659C7311A00F170409CC66EE138D9C1FF9999EA8
          977DA79FFFFF9313CF1580686FE75AF67CB6964852653696C9726B6298D62DBB
          D8F1C62E422AAF00D1CEB77693C48297205473991CBE7631B9154F1392409208
          A2C270F947924401FCEA856D00E6250D547335CBF8A2BF9B5B778631055523C8
          144948886361C781C629354352FDDEAB08D50CCF1F6343B4B5ECC44C0123D10A
          E974F9ADAB782CF702201C39FAF9CB7EF25EF2CDB5AEB3CDF54D2F52FB6413F3
          6A04E0C1A754624A777FC2503043119C1AC5F13E024625161C903BBCED99ED0B
          6A7C3E1BB1DC67DCCAC7D7ACCB5E9CF8C506E351170278EFC8BFB314C43877AA
          485C56540D09567080033C9005324034238068F3470D23BB3F7E7D7ACD55CE1F
          FF9BAF4E94160009A098597500E43F6CE0E2CD0F00E6BCDAB6CC7A7E7FD78E0D
          3C6B1B5B97185073EEFADB6C68598AE7D14862E1E8C12F01AC52D642477BEF46
          000DEE12A0C70F5D0607F701C8A46B6782959C4D0000000049454E44AE426082}
        FileName = 'Images\Data\AddNewDataSource_16x16.png'
        Keywords = 'Data;AddNewDataSource'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000002F744558745469746C650044423B536F757263653B53746F723B6461
          7461736F757263653B44617461626173653B44656C6574651D144FC8000002C3
          49444154785E85935D4894591C879F737C6D4A5DC96AA54D4BC21594095496BA
          32454476F7C6AE74A1A0A00B5B966097BD2A346865D9201665D90F840AC21B91
          F6C20C86325BC871CD6D6B5D898C4466D459AD1927751C67C671DE734E2FBC08
          C1ECB20FFCEE0ECFEFFF3F87237091633FB69E90D020A0DC187D1823BC4AABE7
          5A993965AB59A5D4C3E60EDF10A08D03EF20277E6A1B99B9D365961EF79937D3
          83263E3F6CF4EA84599FB965227FFE6CE647BACC54DF3973BFEBD351403A02B6
          6301C2696CAAF8B81D8C02BD855169D0290AF695905FB407A3921CA8AA227CED
          FA7140021A1757A0320A3B3187957F18240834068D96369BCB8BA457FE81F42B
          54C60610BC830484561A9D89612723C00E32AB8F88CFF7B2FCA487C0502F0B0F
          0649471750B6FA57013AA3F87F40659F7357B0954D3602CFDE1AF6D6E6A19426
          AFB8106BF25700298490B818CB35EB6CF37B47D0A94202BD83AC4E05A8EE3849
          496D33DF963EAB2DDFB9B37F4BEBBB3796C35FB997682BB2C82962AAFB16078E
          3653F3F525C62F76621D3948B5D7EBABEFBEB27B7174ECDCD695EE02E9BE42F6
          0A5A6BA24EF3FB2505E486FEA2EE9B0B98509CC6AB97767B96A6D87FA8086D68
          90005A654F20A5C47BA18DDBDFFD40623184083EA5EECB33C8B949364221063A
          AFAE4DAEAFB5E600B2A5E6838F92D14885D112838525A36436A6F1141D22BD29
          08DEFD83D2F2627478013B99C0DFFF9B999E099DEF59090F0B40009E5F4E557F
          B62B579EC8810FA5A4B2A2B12D371E8EEBF09D719ACEB64813F81B6D2B044069
          25BEEB43B1A781854F042E02908005C86DE9F7C56593AD97DBCB7297A6D98C6D
          F0F8D19C397AECA0F0E4ED22BDA78C819E8179F763B828276927292749209132
          6A3C786F84B5A528C3F75EC6FCB3AFBF18BE3FB3BEF63A4AD0FF3B29A5262CFE
          1BFBDA6AE4F3CC98B17720EA67EDADD337639127A7B57E11F625FA9C26FFCDF8
          F2F9B7E61E7BE14BB7E8110000000049454E44AE426082}
        FileName = 'Images\Data\DeleteDataSource_16x16.png'
        Keywords = 'Data;DeleteDataSource'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000002D744558745469746C650044423B536F757263653B53746F723B6461
          7461736F757263653B44617461626173653B4564697428F6BA9C000002CB4944
          4154785E85937F48935B1C875F5BD7E59C3F0A6E7F5C1542826B21977185F08F
          CA689933572A3A912B52846228AC5B570B0C6A4C19C950732A0C5774AF190B26
          944DCBED4E3653376992F86366C31F77375CE69C7935AEE8749FCE79D9FEC8B2
          BEF0F9EF3CCFF77CCF97C3900A21E198EB7372FA5592066BA3A4D3AACA19B736
          E4A2AF3E7BDCACCCECFA5B215675CA44B9E4DC6E7A1E00130C2D8EAD29AFC7A9
          97C3FDB215DE8927587519E1FF308815A70E0BF666B84C728CB496C0284FEFA3
          92ED821F6C8DB9C0C63CB03E07ACCDC2FF7112FE9561F897ACD8F2F460735E0F
          DF9C0E46591A2542B70B42FBEBB2E1231DB1F13E20992692096C2EDBF1F18D0E
          5E5B3DBC966B787E434809EE7601F7853213EB0B06F896C788C0830D771BFE73
          48F1CEF41B4695420C57A7C0DD9E0FABFA32257881B7E004DE8FD9635188BF2B
          F0586418EFD2A05B965463BEF52BDAA58714747C2A08335589BE2970FE258577
          E411065B8A31FB341B5BFE69BCD68AD194157B9B15186E9EDE5130A5BD02578F
          0663DA524C3F3EC7C2BE99320C5426A056F823A880F7AC52F855C1E2901CFF9A
          EF62E85E119CBA0C165E7314A1B7FC207A9547512E88A8A382707D45CA1782D5
          99062C4C686127F0E443110BAFD80B61961E80A92A19D244FE1DDA9C15745C3D
          FAB9C063C0F2DB0E0C6B2FC1715FC8C28B16094CA57178762309253FF35484E3
          93EC62B7D05672A47BA0F922662D77B13465C3FAD22B8CF66B6055A6B2B0BBF3
          2C8C2531E8F8E3175C88DFA3264C04850BF6936D062CE1B57989C59A4281FECF
          F302875DD7B45571A50AFA1605FE7F6B83A13806BAB20414C4715B08104D997F
          B4A98C641F8709564860A7BC803D4670EADACCA8AD1B0F6AAEA3FA443CF27F0A
          550761A7E638E3D41C63B2A20806E08B908A8C8E1122627FB22B2CFAB03A3692
          7F3238B3981FC2D0648433CC193EB3A3800E1745B2978E17FCC6E9E47EA23086
          4923A192DFE3B9CC27707A3E75A45EF6F60000000049454E44AE426082}
        FileName = 'Images\Data\EditDataSource_16x16.png'
        Keywords = 'Data;EditDataSource'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C00000018744558745469746C650044617461536F757263653B4F
          7074696F6E7318FCC056000002A749444154785E85915F48935118C69F6F4E6D
          E8226D54282462E63F50872093106B32A61888A05B7FA82C4923BAD09B8ABA30
          24E82E2F52B1F2A284C890C894946C1189A4A685CBF96722EACC744E73EAD86C
          FBBE734EFBD8846A931E38BC1CCEFBFE78CEF372F049D2FFA0AC5802E4714002
          63341E8C4B23948C53C2E689406608211F35B7BB3B0150E615FE9064B0416798
          EEAA634B9F5BD9CF890EE6B0F4326A1F645BD3EDCC36DCC82C863A666CAD62EF
          EA0AFBC47E717EE74801708CD1FC446D25C008403DDEE206E8362215B188888A
          06232EC4A4A460E5714BAE080040E1930F407802C1390F6944BCF80C0E140C14
          5422E0D7EA0FB8D71701F732082F40ECFFCBBE08A08482F29B105C360061E0ED
          0370589AB13A528FD9CE662CBCEF807B6D0144204101A03CC1FF05107F5F8003
          8108D85D1CE4F15990A796E2A8A63CB803C2D3C0B1109937931884EF3F0CD9A1
          2418C678B40C87E2EACD877739AF6AEE3C4575ED137F8842A03569641A363D32
          AC4639B1B44E316076E3824E8DC696379557AE370D6D3B5DC4ED768EFAB710F8
          0571C7AF3FB9C0ED39089B6D1325454AC4C52AA02FC9DD6BE81B6D5744CB3136
          39D7E30B91040FF1D03E0287C389AA722D8CC6299C2AAF85F1DB142E9DD160C3
          BE8135EB4ABF08A0CE6DBE7BA4FD0516478760FF3E230241F9759C504660D23C
          0F8F87C7C097699C56C7598DA639104A31F2758274B5DDBBCFF9530D6F3A9BA1
          97854A8A438023120992E3B20B43C7B714CC8A035CC5B902BCFD608469CA826C
          65225459496878F40A66F3EC2D6E2774FF46A462F5DF433425D52F95E9C99A65
          EB1AB4DA63484F4BC0DCEC229EB5F540BE37122693B9570C2BE81121D9C7CF27
          E4A82F9679EBE582D21B8EE71D7D4CA5AE7064AA745519393A7D6A6651E2AE80
          7F9C85E5155EABCF3F59C33254FA7A00E13B4E7F038A7C6509079C5294000000
          0049454E44AE426082}
        FileName = 'Images\Data\ManageDatasource_16x16.png'
        Keywords = 'Data;ManageDatasource'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000001B744558745469746C65004D6F64656C3B436F70793B43
          6F70794D6F64656C3B892D59CB0000023849444154785E85D04B685D651486E1
          E7DF7BF79CD3D6548B8338CAC8CBBC4E2452E2A514051147223AAE5010AA139D
          8AD116412D360345080A924127B60E6AD28A62EA95A21369AD974A03A618B56A
          20F1E4649FBDFFA59889A1015F58D38F8755450480919191F2C86BF31F14294D
          84842C070111024D5D7FFCEC93FBF7ADACACD450D95C1162E2E0A37B10202080
          C0AB6F7DB917254095FE090552B7DBED8CDD749DBA5EB73668410E522A9455A9
          D72DB54D0B09A04271F8F5CF37D8915CFCF14FDF5CFA5D280422B2D1DD3D0FDD
          779B6E67BBA6DD6220A29D78FAF13B11B0891E989CFA44D33422423DCCD70852
          DB40E86FB0850422D3EDA67FD96D1B72D85290FA83A140FBDF8F47809C0B7F0D
          86EA8D1175DD6C256808720E91109429294B52E2DEF13123DB3BCA22DC75C798
          A9B6DD9652AA10158AB5412D07C3361022A83A85E5F9F7F43F3BE3562C7FC4CF
          9D5FECEE2EF9F0B93D57370BDA8C902374CAA42A19368DFEA767DC72CF38185C
          B9E2E4DCAC47A68E8095C54B4EBD38AD82619B451011CAA270E2EC0F66CFFD44
          F52067D975FDFB76DD709A7D3B7DF5EEF3E0A9D1DB410579980576F62A45B466
          E6CE9B7CE161B070F937C7670E387C68127C7F79C14B6F4C330A5468D7EBFEFC
          81674E4E84F0C0DD3783535FFC0A16BF5B00B317A6C1C5AF97C18963A7ADD6F9
          E50AF5CCB1C7EEC736A4B75FD1DB7FE8F892FFE989D9A51BB15A4544C6DAC691
          525A5FBEFA87B937DF01B0A337307DF45B00F55A3E8A55D4956BABCFCD1CDC81
          1200090081211AF81B46C61F74A3585C150000000049454E44AE426082}
        FileName = 'Images\Data\CopyModelDifferences_16x16.png'
        Keywords = 'Data;CopyModelDifferences'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000027744558745469746C6500526566726573683B5265706561743B4261
          72733B526962626F6E3B52656C6F6164CD4DF6E90000030249444154785EA593
          6D4C926B18C76F018114A40D6D619DE98433AAE3F16D7CB173AC43666FE69C7A
          D4F48825339D4E6136D314EB2C23B46C332B957484A0522E5B6E2C31B537B437
          319BCECC17B2652D672FC715E03194AEEE875A6BB53E796DBFDDCF7EF7FFFFE1
          DAB3DB0500D052C6E5A7FEC73B70F2FDC89A4D48D664FA5A5AF5DB1FE4E8928B
          E2226D9F11FB0F04459A7B3D052A63DE0E71B11B91C10E77FA9D5D94DF70DF59
          4E93EB7EC957DF1B4F935FE009B3EBB7A59FE8704C59E6E185E503DC9D7C0B9A
          5B66D8A7BA33243AD4E087F3A4E9F98F683FD1959EBD8BF0B8E6D418BB1F3C9B
          855DA57A23871BC00C15D755FD99D508E1521DE49EB90E833356B8D4F71C726A
          7A460285F11EC48A92CF5D44D973CC50A8BDF504C6FE7780F2CA3044481AFEF5
          FE3598C1DD24F5E10A73F88109958A8DD94DF69E171638D53E0AA2A36D07885E
          E6E95E8476E6D5090AD57D0B66DB02F45B1661DC6A87B4B2F685E0D8C3422284
          21619605FC5DA148ADE884F303D3905C6630112EF5C40D449AB550D5BDBDA314
          D9C99BC862B52379ED6DF478E215C595B9AAFE4B9918D2CC4857CBE4F834A22D
          672287831C881D4B9B2F74232EE918CFA81283FDDA4B1B241CE9062F7E441076
          0C0C19438B2CD63F8A3AD8017187BB4033F61E88EFA8920E882CD24F92F8D1D5
          F3383467B3D846269EFE877CBC5928707372CAB7FFFFF9F01D890783BE98951E
          8628542ACACDDC88ACAF5E3B4C8656090ACBBD4C846802514DE9DEDAFBD038F4
          06E28F742F083395C7F91B44416BFEDACDA731BD38214995E519AA01A837CF41
          A2A213BC422455B8C744828C56E78E8C956B3921E226739666109A47DE81E4DC
          0024286E424CE935F83DA95A87337E41A92A93ECEA14B043F63F2451591CE78A
          01BB5B50DB14B105725D199C28F0FF47FD24EEF86D28D19B4139340BCAC15908
          2F6807DFF042317BCD9650FF14ED33EA8AB0F5384F65AC2B40889FD48CD1394F
          42D2D9BCD53E5BE515BC98DA21EC017BE0C52A877DB797CBC8547716CE786068
          74AE1411E0F90132C61DC3C67030DE18CF2F8EFC7D78C9CFF9136643522887EA
          10230000000049454E44AE426082}
        FileName = 'Images\Actions\Refresh_16x16.png'
        Keywords = 'Actions;Refresh'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000027744558745469746C6500526566726573683B5265706561743B4261
          72733B526962626F6E3B52656C6F6164CD4DF6E90000037749444154785E6593
          6B4C537718C69F73E885622F4069C9B02DA1ADB60C2133D471DDD451D8257C02
          2661B3719912A3092163898BDBF8B06422DB74612612CDE64CDCE2A698CC2DA2
          0C541C9918645BB701EB3414B99536E08022A597737ADE71927E58C693BCFFBC
          F9E5799E2FFFBC0C364A64FFE794988DE6CF6F55032034567433EF7596AB37E7
          A85AC0302FB22C1C448020D02F3C47BD8F3C4B9F76BC3314FCE44A05AD330844
          585F80B3BDAF00007BB2CBE9BC70C73533E0F998BCF33F5030FA9B38343EFF3D
          F58F1EA7737D0DB347CF943A456F44F06085BF87F7BF28054470A2AB72CFE5C1
          43F4C07F999EC4876989FB891E477B6921DA438BDC6D5A37D398EF22755EDD4B
          CD271DF5626689BB83B74E3920693AB643AF516BCFE7659723439B0E6F6008D7
          6F0F60E46F0F44E5DB6D28DABE1DF9D66750F8F4F3989B0F7C597DC0329026DD
          153878BC80D8CCEC4D2DD9FADC145D8606EE078338FBF5377CFFDDC10FA7C717
          AC33138FAD3F0F0D1FB976EBC798D7EF86C1900AAB292F45A393BE0D800DADC5
          20E1E3F4B22E3D13ABDC1CDC7F8D61666EAEADEFCCEC31001C44011DCA237266
          F88FB18F5277CB91AE27C438FE2500EF46C25C9CE5B9B84DA10462F12798F64D
          63C5C79D17C3CBDC5D4A7C27EBEE5BF86ABD18E3C11E90F2112251CE044071E5
          84572E0947F8C8C44A8F54A94843B24C0685542E88414E0893B3DEACCA2FD6FB
          08B4492661118AFD038A876032EA5587DB75CB828065361CE2A67D8100FC2137
          AC66234C76CD7E00529DDCC9DCBC34115E5E8C746CCBCD455DCD0B880B1C1809
          8FC6FDB5D0E9B4181B9D6D4FB239B45B55AA9462953E0C8D3C0B6BAB5462B029
          42AA34B94FFB540A3B726FFE7E7246AC9E74BF6B92A4045EE0F170E657F47C37
          35E5BEBE788029ADDE6CDE569239BA65475461B66402410BA6BC8B989CF46375
          758DF38E2E952B53E5B6AD858A0BF93BA31075B32B04CFF08ACBDD1DBC240259
          952BA7F1607B01B575DBE9DB87657463B281067CCD7471701FD535E7F603D0D7
          34D9FBDB6ED8A8F5AA899EAD4D1599120083C4935C5663A8AF6BD9E26FEAB4D0
          07D72C74EABE95CEB94BE8707B1155BC96E372546595B85AF3B8CA43199CA558
          510440E278550D54BD99932881D468571BCA6A0D9FED6C308EECDE6BA25DAF9B
          E8B93DC63F2BDFB0B4025057ED339F2EA8569E06A0103385752A301BCF1832B1
          1D4052820B006200E2FF615C82E35FC02B8FD5CBC3AEEB0000000049454E44AE
          426082}
        FileName = 'Images\Actions\Refresh2_16x16.png'
        Keywords = 'Actions;Refresh2'
      end>
  end
  object behViolations: TBoldExpressionHandle
    RootHandle = dmBoldUMLModelEditorHandles.behModel
    Expression = 'validator.Violation'
    Left = 262
    Top = 524
  end
end
