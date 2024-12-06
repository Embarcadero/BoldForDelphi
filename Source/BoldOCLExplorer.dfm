object OclExplorerForm: TOclExplorerForm
  Left = 132
  Top = 107
  Caption = 'OCL Explorer'
  ClientHeight = 465
  ClientWidth = 969
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Splitter5: TSplitter
    Left = 497
    Top = 0
    Width = 4
    Height = 465
    ExplicitHeight = 466
  end
  object Panel1: TPanel
    Left = 501
    Top = 0
    Width = 468
    Height = 465
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 472
    ExplicitHeight = 466
    object Splitter1: TSplitter
      Left = 1
      Top = 137
      Width = 470
      Height = 4
      Cursor = crVSplit
      Align = alTop
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 470
      Height = 136
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 0
      object Panel3: TPanel
        Left = 5
        Top = 5
        Width = 460
        Height = 28
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          456
          28)
        object List2EditOCLButton: TButton
          Left = 0
          Top = 0
          Width = 81
          Height = 25
          Action = EditList2Action
          TabOrder = 0
        end
        object cbEvaluateInPS2: TCheckBox
          Left = 356
          Top = 6
          Width = 97
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'Evaluate in PS'
          TabOrder = 1
          OnClick = cbEvaluateInPS2Click
          ExplicitLeft = 360
        end
      end
      object List2OCLMemo: TMemo
        Left = 5
        Top = 33
        Width = 456
        Height = 98
        Align = alClient
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 1
        ExplicitWidth = 460
      end
    end
    object Panel4: TPanel
      Left = 1
      Top = 141
      Width = 466
      Height = 323
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 470
      ExplicitHeight = 324
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 470
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label2: TLabel
          Left = 4
          Top = 7
          Width = 25
          Height = 13
          Caption = 'List 2'
        end
      end
      object cxGrid2: TcxGrid
        Left = 0
        Top = 25
        Width = 470
        Height = 299
        Align = alClient
        BorderStyle = cxcbsNone
        TabOrder = 1
        LookAndFeel.NativeStyle = True
        object RightView: TcxGridBoldTableView
          DragMode = dmAutomatic
          Navigator.Buttons.CustomButtons = <>
          ScrollbarAnnotations.CustomAnnotations = <>
          DataController.BoldHandle = List2Handle
          DataController.Filter.Options = [fcoSoftNull]
          DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          DateTimeHandling.Grouping = dtgRelativeToToday
          OptionsBehavior.CellHints = True
          OptionsBehavior.IncSearch = True
          OptionsBehavior.ImmediateEditor = False
          OptionsSelection.MultiSelect = True
          OptionsSelection.InvertSelect = False
          OptionsSelection.UnselectFocusedRecordOnExit = False
          OptionsView.ColumnAutoWidth = True
          object RightViewColumn1: TcxGridBoldColumn
            Caption = 'BoldId'
            DataBinding.ValueType = 'Integer'
            DataBinding.BoldProperties.Expression = 'boldId'
          end
          object RightViewColumn2: TcxGridBoldColumn
            Caption = 'Class'
            DataBinding.BoldProperties.Expression = 'oclType'
          end
          object RightViewColumn3: TcxGridBoldColumn
            Caption = 'AsString'
            DataBinding.BoldProperties.Expression = ''
          end
        end
        object cxGridLevel1: TcxGridLevel
          GridView = RightView
        end
      end
    end
  end
  object Panel8: TPanel
    Left = 0
    Top = 0
    Width = 497
    Height = 465
    Align = alLeft
    TabOrder = 1
    ExplicitHeight = 466
    object Splitter3: TSplitter
      Left = 1
      Top = 137
      Width = 495
      Height = 4
      Cursor = crVSplit
      Align = alTop
    end
    object Panel9: TPanel
      Left = 1
      Top = 1
      Width = 495
      Height = 136
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 0
      object Panel10: TPanel
        Left = 5
        Top = 5
        Width = 485
        Height = 28
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          485
          28)
        object List1EditOCLButton: TButton
          Left = 0
          Top = 0
          Width = 81
          Height = 25
          Action = EditList1Action
          TabOrder = 0
        end
        object cbEvaluateInPS1: TCheckBox
          Left = 384
          Top = 6
          Width = 97
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'Evaluate in PS'
          TabOrder = 1
          OnClick = cbEvaluateInPS1Click
        end
      end
      object List1OCLMemo: TMemo
        Left = 5
        Top = 33
        Width = 485
        Height = 98
        Align = alClient
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 1
      end
    end
    object Panel11: TPanel
      Left = 1
      Top = 141
      Width = 495
      Height = 323
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitHeight = 324
      object Panel12: TPanel
        Left = 0
        Top = 0
        Width = 495
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 4
          Top = 7
          Width = 25
          Height = 13
          Caption = 'List 1'
        end
      end
      object cxGrid1: TcxGrid
        Left = 0
        Top = 25
        Width = 495
        Height = 299
        Align = alClient
        BorderStyle = cxcbsNone
        TabOrder = 1
        LookAndFeel.NativeStyle = True
        object LeftView: TcxGridBoldTableView
          DragMode = dmAutomatic
          Navigator.Buttons.CustomButtons = <>
          ScrollbarAnnotations.CustomAnnotations = <>
          DataController.BoldProperties.DefaultDblClick = False
          DataController.BoldHandle = List1Handle
          DataController.Filter.Options = [fcoSoftNull]
          DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          DateTimeHandling.Grouping = dtgRelativeToToday
          OptionsBehavior.CellHints = True
          OptionsBehavior.IncSearch = True
          OptionsBehavior.IncSearchItem = LeftViewClass
          OptionsBehavior.ImmediateEditor = False
          OptionsSelection.MultiSelect = True
          OptionsSelection.InvertSelect = False
          OptionsSelection.UnselectFocusedRecordOnExit = False
          OptionsView.ColumnAutoWidth = True
          OptionsView.Footer = True
          object LeftViewTopSortedIndex: TcxGridBoldColumn
            Caption = 'Index'
            DataBinding.ValueType = 'Integer'
            DataBinding.BoldProperties.Expression = ''
            DataBinding.BoldProperties.Renderer = brIndex
            PropertiesClassName = 'TcxSpinEditProperties'
          end
          object LeftViewClass: TcxGridBoldColumn
            Caption = 'Class'
            DataBinding.BoldProperties.Expression = ''
            Width = 298
          end
          object LeftViewObjectCount: TcxGridBoldColumn
            Caption = 'Objects'
            DataBinding.ValueType = 'Integer'
            DataBinding.BoldProperties.Expression = ''
            DataBinding.BoldProperties.Renderer = brObjects
            PropertiesClassName = 'TcxSpinEditProperties'
            Properties.Alignment.Horz = taRightJustify
            SortIndex = 0
            SortOrder = soDescending
            Width = 80
          end
          object LeftViewIdCount: TcxGridBoldColumn
            Caption = 'Ids'
            DataBinding.ValueType = 'Integer'
            DataBinding.BoldProperties.Expression = ''
            DataBinding.BoldProperties.Renderer = brIds
            PropertiesClassName = 'TcxSpinEditProperties'
            Properties.Alignment.Horz = taRightJustify
            Properties.DisplayFormat = '# ### ###'
          end
          object LeftViewPersistent: TcxGridBoldColumn
            Caption = 'Persistent'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = ''
            DataBinding.BoldProperties.Renderer = bfIsPersistent
            PropertiesClassName = 'TcxCheckBoxProperties'
            Width = 80
          end
          object LeftViewColumn1: TcxGridBoldColumn
            Caption = 'Abstract'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = ''
            DataBinding.BoldProperties.Renderer = brIsAbstract
          end
          object LeftViewColumn2: TcxGridBoldColumn
            Caption = 'LinkClass'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = ''
            DataBinding.BoldProperties.Renderer = brIsLinkClass
          end
          object LeftViewClassState: TcxGridBoldColumn
            Caption = 'Loaded'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = ''
            DataBinding.BoldProperties.Renderer = bfClassState
          end
        end
        object cxGrid1Level1: TcxGridLevel
          GridView = LeftView
        end
      end
    end
  end
  object List1Handle: TBoldListHandle
    StaticSystemHandle = ServerData.SystemHandle
    Left = 16
    Top = 104
  end
  object List2Handle: TBoldListHandle
    StaticSystemHandle = ServerData.SystemHandle
    Variables = BoldVariableDefinition
    Left = 516
    Top = 104
  end
  object BoldVariableDefinition: TBoldOclVariables
    Variables = <
      item
        BoldHandle = List1Handle
        VariableName = 'list'
        UseListElement = True
      end>
    Left = 49
    Top = 105
  end
  object MainMenu1: TMainMenu
    Left = 88
    Top = 104
    object FileMenu: TMenuItem
      Caption = '&File'
      object EditList1OCL1: TMenuItem
        Action = EditList1Action
      end
      object EditList2OCL1: TMenuItem
        Action = EditList2Action
      end
      object UpdateDB1: TMenuItem
        Action = BoldUpdateDBAction1
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Action = CloseApplicationAction
      end
    end
    object ToolsMenu: TMenuItem
      Caption = '&Tools'
      object EditOCL1: TMenuItem
        Action = ShowDebuggerAction
      end
      object ShowOCLSummary1: TMenuItem
        Action = ShowOCLSyntaxSummary
      end
    end
    object HelpMenu: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Action = AboutAction
      end
    end
  end
  object ActionList1: TActionList
    Left = 128
    Top = 104
    object CloseApplicationAction: TAction
      Caption = '&Close'
      OnExecute = CloseApplicationActionExecute
    end
    object AboutAction: TAction
      Caption = '&About'
    end
    object ShowDebuggerAction: TAction
      Caption = 'Show &Debugger'
      OnExecute = ShowDebuggerActionExecute
    end
    object EditList1Action: TAction
      Caption = 'Edit List &1 OCL'
      OnExecute = List1EditOCL
    end
    object EditList2Action: TAction
      Caption = 'Edit List &2 OCL'
      OnExecute = List2EditOCL
    end
    object ShowOCLSyntaxSummary: TAction
      Caption = 'Show &OCL Syntax Summary'
      OnExecute = ShowOCLSyntaxSummaryExecute
    end
    object BoldUpdateDBAction1: TBoldUpdateDBAction
      Category = 'Bold Actions'
      Caption = 'Update DB'
      ShortCut = 16467
    end
  end
  object BoldPlaceableListSubscriber1: TBoldPlaceableListSubscriber
    BoldHandle = List1Handle
    BoldRowProperties.Expression = ''
    OnAfterMakeUptoDate = BoldPlaceableListSubscriber1AfterMakeUptoDate
    Left = 233
    Top = 73
  end
  object BoldPlaceableListSubscriber2: TBoldPlaceableListSubscriber
    BoldHandle = List1Handle
    BoldRowProperties.Expression = ''
    OnAfterMakeUptoDate = BoldPlaceableListSubscriber2AfterMakeUptoDate
    Left = 625
    Top = 73
  end
  object brObjects: TBoldAsVariantRenderer
    OnSubscribe = brClassListSubscribe
    OnGetAsVariant = brObjectsGetAsVariant
    Left = 265
    Top = 237
  end
  object brIndex: TBoldAsVariantRenderer
    OnGetAsVariant = brIndexGetAsVariant
    Left = 17
    Top = 237
  end
  object bfIsPersistent: TBoldAsVariantRenderer
    OnGetAsVariant = bfIsPersistentGetAsVariant
    Left = 377
    Top = 237
  end
  object bfClassState: TBoldAsVariantRenderer
    OnSubscribe = bfClassStateSubscribe
    OnGetAsVariant = bfClassStateGetAsVariant
    Left = 449
    Top = 237
  end
  object cxGridPopupMenu1: TcxGridPopupMenu
    Grid = cxGrid1
    PopupMenus = <>
    Left = 385
    Top = 73
  end
  object cxGridPopupMenu2: TcxGridPopupMenu
    Grid = cxGrid2
    PopupMenus = <>
    Left = 897
    Top = 73
  end
  object brIds: TBoldAsVariantRenderer
    OnSubscribe = brClassListSubscribe
    OnGetAsVariant = brIDsGetAsVariant
    Left = 320
    Top = 240
  end
  object brIsAbstract: TBoldAsVariantRenderer
    OnGetAsVariant = brIsAbstractGetAsVariant
    Left = 360
    Top = 144
  end
  object brIsLinkClass: TBoldAsVariantRenderer
    OnGetAsVariant = brIsLinkClassGetAsVariant
    Left = 296
    Top = 144
  end
end
