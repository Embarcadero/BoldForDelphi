object BoldModelEditFrm: TBoldModelEditFrm
  Left = 352
  Top = 156
  Width = 829
  Height = 634
  Caption = 'Bold UML Model Editor'
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
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 200
    Top = 27
    Width = 6
    Height = 542
    Beveled = True
  end
  object TBoldLabel
    Left = 120
    Top = 4
    Width = 209
    Height = 24
    BoldHandle = behAttribute
    BoldProperties.Expression = 'owner.name + '#39'.'#39' + name'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label15: TLabel
    Left = 45
    Top = 4
    Width = 73
    Height = 24
    Caption = 'Attribute:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label17: TLabel
    Left = 84
    Top = 36
    Width = 28
    Height = 13
    Alignment = taRightJustify
    Caption = '&Name'
  end
  object Label18: TLabel
    Left = 61
    Top = 60
    Width = 51
    Height = 13
    Alignment = taRightJustify
    Caption = '&Stereotype'
  end
  object Label19: TLabel
    Left = 53
    Top = 156
    Width = 59
    Height = 13
    Alignment = taRightJustify
    Caption = '&Delphi name'
  end
  object Label20: TLabel
    Left = 32
    Top = 180
    Width = 80
    Height = 13
    Alignment = taRightJustify
    Caption = 'E&xpression name'
  end
  object Label32: TLabel
    Left = 88
    Top = 84
    Width = 24
    Height = 13
    Alignment = taRightJustify
    Caption = '&Type'
    FocusControl = cmbAttributeBoldType
  end
  object BoldTreeView1: TBoldXCVTreeView
    Left = 0
    Top = 27
    Width = 200
    Height = 542
    HelpContext = 34
    Align = alLeft
    AutoExpandLevels = 0
    BoldHandle = brhTreeRoot
    BoldProperties.Parts = <
      item
        ControllerExpression = 'self.ocltype'
        InterpretAsList = True
      end>
    BoldProperties.NodeDescriptions = <
      item
        Name = 'UMLModel'
        HideNodeWithNoChildren = False
        ListController.Parts = <
          item
            ControllerExpression = #39'LogicalView'#39
            InterpretAsList = False
          end>
        IconController.Expression = '6'
        TextController.Expression = 'name'
      end
      item
        Name = 'Classes'
        ContextTypeName = 'BoldUMLModel'
        HideNodeWithNoChildren = False
        ListController.Parts = <
          item
            ElementExpression = 'classes->orderBy(name)'
            ControllerExpression = 'self.ocltype'
            InterpretAsList = True
          end>
        IconController.Expression = '6'
        TextController.Expression = #39'Classes'#39
      end
      item
        Name = 'Associations'
        ContextTypeName = 'UMLPackage'
        HideNodeWithNoChildren = True
        ListController.Parts = <
          item
            ElementExpression = 'ownedElement->filterOnType(UMLAssociation)->orderby(name)'
            ControllerExpression = 'self.ocltype'
            InterpretAsList = True
          end>
        IconController.Expression = '0'
        TextController.Expression = #39'Associations'#39
      end
      item
        Name = 'DataTypes'
        ContextTypeName = 'UMLPackage'
        HideNodeWithNoChildren = True
        ListController.Parts = <
          item
            ElementExpression = 'ownedElement->filterOnType(UMLDataType)->orderby(name)'
            ControllerExpression = 
              'oclType.asstring->union(oclType.allsupertypes.asString)->union('#39 +
              '<Default>'#39')'
            InterpretAsList = True
          end>
        IconController.Expression = '6'
        TextController.Expression = #39'DataTypes'#39
      end
      item
        Name = 'UMLClass'
        HideNodeWithNoChildren = False
        ListController.Parts = <
          item
            ElementExpression = 'feature->select(oclIsKindOf(UMLAttribute))->orderby(name)'
            ControllerExpression = 'self.ocltype'
            InterpretAsList = True
            Name = 'Attributes'
          end
          item
            ElementExpression = 'associationEnd.otherEnd->select(isNavigable)->orderby(name)'
            ControllerExpression = 'self.ocltype'
            InterpretAsList = True
            Name = 'AssociationEnds'
          end
          item
            ElementExpression = 'feature->select(oclIsKindOf(UMLOperation))->orderby(name)'
            ControllerExpression = 'self.ocltype'
            InterpretAsList = True
            Name = 'Operations'
          end>
        IconController.Expression = '4'
        TextController.Expression = 'name'
      end
      item
        Name = 'UMLAssociation'
        HideNodeWithNoChildren = False
        ListController.Parts = <
          item
            ElementExpression = 'connection'
            ControllerExpression = 'self.ocltype'
            InterpretAsList = True
            Name = 'AssociationEnds'
          end>
        IconController.Expression = '0'
        TextController.Expression = 
          'if name='#39#39' then '#39'('#39' + connection->first.name + '#39':'#39' + connection-' +
          '>first.type.name + '#39')('#39' + connection->last.name + '#39':'#39' + connecti' +
          'on->last.type.name + '#39')'#39' else name endif'
      end
      item
        Name = 'UMLAttribute'
        HideNodeWithNoChildren = False
        ListController.Parts = <>
        IconController.Expression = '2'
        TextController.Expression = 'name'
      end
      item
        Name = 'UMLAssociationEnd'
        HideNodeWithNoChildren = False
        ListController.Parts = <
          item
            ElementExpression = 'qualifier'
            ControllerExpression = 'self.ocltype'
            InterpretAsList = True
          end>
        IconController.Expression = '10'
        TextController.Expression = 'name +  '#39' ('#39' + type.name + '#39')'#39
      end
      item
        Name = 'UMLOperation'
        HideNodeWithNoChildren = False
        ListController.Parts = <
          item
            ElementExpression = 'parameter'
            ControllerExpression = 'self.ocltype'
            InterpretAsList = True
          end>
        IconController.Expression = '8'
        TextController.Expression = 'name'
      end
      item
        Name = 'UMLParameter'
        HideNodeWithNoChildren = False
        ListController.Parts = <>
        IconController.Expression = '12'
        TextController.Expression = 'name'
      end
      item
        Name = 'UMLPackage'
        HideNodeWithNoChildren = False
        ListController.Parts = <
          item
            ElementExpression = 'ownedElement->filterOnType(UMLPackage)->orderBy(name)'
            ControllerExpression = 
              'oclType.asstring->union(oclType.allsupertypes.asString)->union('#39 +
              '<Default>'#39')'
            InterpretAsList = True
          end
          item
            ElementExpression = 'ownedElement->filterOnType(UMLClass)->orderBy(name)'
            ControllerExpression = 
              'oclType.asstring->union(oclType.allsupertypes.asString)->union('#39 +
              '<Default>'#39')'
            InterpretAsList = True
          end
          item
            ElementExpression = 
              'if (ownedElement->filterOnType(UMLAssociation)->size > 0) then '#13 +
              '  Set{self} '#13'else '#13'  Set{self}->excluding(self) '#13'endif'
            ControllerExpression = #39'Associations'#39
            InterpretAsList = True
          end
          item
            ControllerExpression = #39'DataTypes'#39
            InterpretAsList = False
          end>
        IconController.Expression = '6'
        TextController.Expression = 'name '
      end
      item
        Name = 'UMLDataType'
        HideNodeWithNoChildren = False
        ListController.Parts = <>
        IconController.Expression = '4'
        TextController.Expression = 'name'
      end
      item
        Name = 'LogicalView'
        ContextTypeName = 'UMLModel'
        HideNodeWithNoChildren = False
        ListController.Parts = <
          item
            ElementExpression = 'ownedElement->filterOnType(UMLPackage)->orderBy(name)'
            ControllerExpression = 
              'oclType.asstring->union(oclType.allsupertypes.asString)->union('#39 +
              '<Default>'#39')'
            InterpretAsList = True
          end
          item
            ElementExpression = 'ownedElement->filterOnType(UMLClass)->orderBy(name)'
            ControllerExpression = 
              'oclType.asstring->union(oclType.allsupertypes.asString)->union('#39 +
              '<Default>'#39')'
            InterpretAsList = True
          end
          item
            ControllerExpression = #39'Associations'#39
            InterpretAsList = False
          end
          item
            ControllerExpression = #39'DataTypes'#39
            InterpretAsList = False
          end>
        IconController.Expression = '6'
        TextController.Expression = #39'Logical View'#39
      end>
    Constraints.MinWidth = 200
    DragMode = dmAutomatic
    HideSelection = False
    Images = ilTreeView
    Indent = 19
    PopupMenu = popTree
    RightClickSelect = True
    SelectedIndexDelta = 1
    SelectInserted = True
    TabOrder = 2
    OnChange = BoldTreeView1Change
    OnDragDrop = BoldTreeView1DragDrop
    OnDragOver = BoldTreeView1DragOver
    OnEndDrag = BoldTreeView1EndDrag
    OnExpanded = BoldTreeView1Expanded
    OnKeyDown = BoldTreeView1KeyDown
    OnStartDrag = BoldTreeView1StartDrag
    OnCut = BoldTreeView1Cut
    OnCopy = BoldTreeView1Copy
    OnPaste = BoldTreeView1Paste
  end
  object pcModelEdit: TPageControl
    Left = 206
    Top = 27
    Width = 615
    Height = 542
    ActivePage = tabClass
    Align = alClient
    TabHeight = 20
    TabOrder = 0
    OnChange = pcModelEditChange
    object tabModel: TTabSheet
      HelpContext = 9
      Caption = 'Model'
      object Splitter2: TSplitter
        Left = 0
        Top = 329
        Width = 607
        Height = 6
        Cursor = crVSplit
        Align = alTop
        Beveled = True
        OnCanResize = Splitter2CanResize
      end
      object sbModel: TScrollBox
        Left = 0
        Top = 0
        Width = 607
        Height = 329
        HelpContext = 9
        Align = alTop
        BorderStyle = bsNone
        TabOrder = 0
        DesignSize = (
          607
          329)
        object lblModelName: TLabel
          Left = 84
          Top = 36
          Width = 28
          Height = 13
          Alignment = taRightJustify
          Caption = '&Name'
          FocusControl = tbxModelName
        end
        object lblModelDelphiName: TLabel
          Left = 64
          Top = 84
          Width = 48
          Height = 13
          Alignment = taRightJustify
          Caption = '&Unit name'
          FocusControl = tbxModelDelhpiName
        end
        object lblModelExpressionName: TLabel
          Left = 32
          Top = 108
          Width = 80
          Height = 13
          Alignment = taRightJustify
          Caption = 'E&xpression name'
          FocusControl = tbxModelExpressionName
        end
        object lblModelPMapperName: TLabel
          Left = 40
          Top = 300
          Width = 72
          Height = 13
          Alignment = taRightJustify
          Caption = '&PMapper name'
          FocusControl = tbxModelPMapperName
          Visible = False
        end
        object lblModelInterfaceUses: TLabel
          Left = 45
          Top = 132
          Width = 67
          Height = 13
          Alignment = taRightJustify
          Caption = '&Interface uses'
          FocusControl = tbxModelInterfaceUses
        end
        object lblModelImplementationUses: TLabel
          Left = 16
          Top = 156
          Width = 96
          Height = 13
          Alignment = taRightJustify
          Caption = 'Imp&lementation uses'
          FocusControl = tbxModelImplementationUses
        end
        object Label1: TLabel
          Left = 35
          Top = 180
          Width = 77
          Height = 13
          Caption = 'Model &root class'
        end
        object blbModelInfo: TBoldLabel
          Left = 120
          Top = 4
          Width = 269
          Height = 24
          AutoSize = False
          BoldHandle = behModel
          BoldProperties.Expression = 'name'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 60
          Top = 4
          Width = 58
          Height = 24
          Caption = 'Model:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblModelStereotype: TLabel
          Left = 61
          Top = 60
          Width = 51
          Height = 13
          Alignment = taRightJustify
          Caption = 'Stereot&ype'
          FocusControl = tbxModelStereotype
        end
        object lblModelConstraints: TLabel
          Left = 60
          Top = 228
          Width = 52
          Height = 13
          Alignment = taRightJustify
          Caption = 'C&onstraints'
          FocusControl = tbxModelConstraints
          Visible = False
        end
        object lblModelGUID: TLabel
          Left = 85
          Top = 252
          Width = 27
          Height = 13
          Alignment = taRightJustify
          Caption = 'GUI&D'
          FocusControl = tbxModelConstraints
          Visible = False
        end
        object lblModelTypeLibVersion: TLabel
          Left = 38
          Top = 276
          Width = 74
          Height = 13
          Alignment = taRightJustify
          Caption = 'Type li&b version'
          FocusControl = tbxModelConstraints
          Visible = False
        end
        object lblModelOptimisticLocking: TLabel
          Left = 26
          Top = 204
          Width = 86
          Height = 13
          Alignment = taRightJustify
          Caption = 'Optimistic Loc&king'
          FocusControl = tbxModelConstraints
          Visible = False
        end
        object imgWarning: TImage
          Left = 425
          Top = 233
          Width = 16
          Height = 16
          AutoSize = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000076000000280000001000
            000010000000010004000000000080000000C40E0000C40E0000100000000000
            0000000000000000800000800000008080008000000080008000808000008080
            8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00666666666666666666666666666666666999999999999999699BBBBBBBBB
            BB99669BBBBB0BBBBB966699BBBBBBBBB99666699BBB0BBB996666669BBB0BBB
            9666666699BB0BB99666666669BB0BB966666666669BBB96666666666699B996
            666666666669B966666666666669996666666666666696666666666666666666
            6666}
          Transparent = True
        end
        object imgHint: TImage
          Left = 433
          Top = 241
          Width = 16
          Height = 16
          AutoSize = True
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000076000000280000001000
            000010000000010004000000000080000000C40E0000C40E0000100000000000
            0000000000000000800000800000008080008000000080008000808000008080
            8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
            FF00666666666666666666666666666666666000000000000000602AAAAAAAAA
            AA20670AAAAA0AAAAA076602AAAAAAAAA20666602AAA0AAA206666670AAA0AAA
            0766666602AA0AA20666666670AA0AA076666666670AAA07666666666602A206
            666666666670A076666666666660006666666666666606666666666666666666
            6666}
          Transparent = True
        end
        object tbxModelName: TBoldEdit
          Left = 120
          Top = 32
          Width = 291
          Height = 21
          Hint = '|The name of the model.'
          HelpContext = 33
          BoldHandle = behModel
          BoldProperties.Expression = 'name'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 0
        end
        object tbxModelDelhpiName: TBoldEdit
          Left = 120
          Top = 80
          Width = 291
          Height = 21
          Hint = '|Filename used to save model.'
          HelpContext = 1130
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.UnitName'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 2
        end
        object tbxModelExpressionName: TBoldEdit
          Left = 120
          Top = 104
          Width = 291
          Height = 21
          Hint = '|The expression name of the model. Can mostly be left unchanged.'
          HelpContext = 11
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 3
        end
        object tbxModelPMapperName: TBoldEdit
          Left = 120
          Top = 296
          Width = 291
          Height = 21
          Hint = 
            '|Persistence mapper for the model. There is no reason to change ' +
            'from <Default>.'
          HelpContext = 12
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.PMapper'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 15
          Visible = False
        end
        object tbxModelInterfaceUses: TBoldEdit
          Left = 120
          Top = 128
          Width = 275
          Height = 21
          Hint = '|The units required by the interface section.'
          HelpContext = 13
          TabStop = False
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.InterfaceUses'#39'].value'
          BoldProperties.Renderer = bsrNiceCRRenderer
          ReadOnly = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 4
        end
        object tbxModelImplementationUses: TBoldEdit
          Left = 120
          Top = 152
          Width = 275
          Height = 21
          Hint = '|The units required by the implementation section.'
          HelpContext = 14
          TabStop = False
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.ImplementationUses'#39'].value'
          BoldProperties.Renderer = bsrNiceCRRenderer
          ReadOnly = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 6
        end
        object btInterfaceUses: TButton
          Left = 394
          Top = 128
          Width = 17
          Height = 21
          Hint = '|Edit the interface uses list.'
          HelpContext = 82
          Anchors = [akTop, akRight]
          Caption = #188
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Symbol'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
          OnClick = btInterfaceUsesClick
        end
        object btImplementationUses: TButton
          Left = 394
          Top = 152
          Width = 17
          Height = 21
          Hint = '|Edit the implementation uses list.'
          HelpContext = 24
          Anchors = [akTop, akRight]
          Caption = #188
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Symbol'
          Font.Style = []
          ParentFont = False
          TabOrder = 7
          OnClick = btImplementationUsesClick
        end
        object bcbModelUseXFiles: TBoldCheckBox
          Left = 418
          Top = 30
          Width = 77
          Height = 17
          Hint = 
            '|If checked, a table for permanent ID recodring will be used. Re' +
            'quired for OLLE.'
          HelpContext = 27
          Anchors = [akTop, akRight]
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.UseXFiles'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.Renderer = bcrBooleanToCheckBox
          Caption = 'U&se X files'
          ReadOnly = False
          TabOrder = 14
        end
        object bcbModelUseTimestamp: TBoldCheckBox
          Left = 418
          Top = 46
          Width = 93
          Height = 17
          Hint = 
            '|If checked, a time stamp column will be added to the XFiles tab' +
            'le.'
          HelpContext = 28
          Anchors = [akTop, akRight]
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.UseTimestamp'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.Renderer = bcrBooleanToCheckBox
          Caption = 'Use timestamp'
          ReadOnly = False
          TabOrder = 16
        end
        object bcbUseGlobalId: TBoldCheckBox
          Left = 418
          Top = 62
          Width = 93
          Height = 17
          Hint = '|If checked, a GUID column will be added to the XFiles table.'
          HelpContext = 29
          Anchors = [akTop, akRight]
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.UseGlobalId'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.Renderer = bcrBooleanToCheckBox
          Caption = 'Use global id'
          ReadOnly = False
          TabOrder = 17
        end
        object bcbUseReadOnly: TBoldCheckBox
          Left = 418
          Top = 78
          Width = 93
          Height = 17
          Hint = 
            '|If checked, a readonly column will be added to the XFiles table' +
            '.'
          HelpContext = 30
          Anchors = [akTop, akRight]
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.UseReadOnly'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.Renderer = bcrBooleanToCheckBox
          Caption = 'Use readonly'
          ReadOnly = False
          TabOrder = 18
        end
        object tbxModelStereotype: TBoldEdit
          Left = 120
          Top = 56
          Width = 291
          Height = 21
          Hint = '|Stereotype applied to model.'
          HelpContext = 1150
          BoldHandle = behModel
          BoldProperties.Expression = 'stereotypeName'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 1
        end
        object tbxModelConstraints: TBoldEdit
          Left = 120
          Top = 224
          Width = 275
          Height = 21
          Hint = 
            '|Constraints for the model. Note that these are not enforced aut' +
            'omatically.'
          HelpContext = 1160
          TabStop = False
          BoldHandle = behModel
          BoldProperties.Expression = 'constraint'
          BoldProperties.Renderer = bsrNiceCRRenderer
          ReadOnly = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 10
          Visible = False
        end
        object btModelConstraintEditor: TButton
          Left = 394
          Top = 224
          Width = 17
          Height = 21
          Hint = '|Edit the model'#39's constraints'
          HelpContext = 24
          Anchors = [akTop, akRight]
          Caption = #188
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Symbol'
          Font.Style = []
          ParentFont = False
          TabOrder = 11
          Visible = False
          OnClick = btModelConstraintEditorClick
        end
        object edModelGUID: TBoldEdit
          Left = 120
          Top = 248
          Width = 291
          Height = 21
          Hint = 'GUID used when generating IDL code.'
          HelpContext = 11
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.GUID'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 12
          Visible = False
        end
        object edModelTypeLibVersion: TBoldEdit
          Left = 120
          Top = 272
          Width = 291
          Height = 21
          Hint = '|Version number for the type lib.'
          HelpContext = 11
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.TypeLibVersion'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 13
          Visible = False
        end
        object bcbUseClockLog: TBoldCheckBox
          Left = 418
          Top = 94
          Width = 93
          Height = 17
          Hint = 
            '|If checked a table mapping "technical time" to "real time" will' +
            ' be maintained.'
          HelpContext = 30
          Anchors = [akTop, akRight]
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.UseClockLog'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.Renderer = bcrBooleanToCheckBox
          Caption = 'Use clock lo&g'
          ReadOnly = False
          TabOrder = 19
        end
        object bcbGenerateMultiplicityConstraints: TBoldCheckBox
          Left = 418
          Top = 110
          Width = 157
          Height = 17
          Hint = 
            '|If checked, constraints for multiplicity will be automatically ' +
            'generated.'
          HelpContext = 30
          Anchors = [akTop, akRight]
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.GenerateMultiplicityConstraints'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.Renderer = bcrBooleanToCheckBox
          Caption = 'Use multiplicity constraints'
          ReadOnly = False
          TabOrder = 20
        end
        object cmbModelOptimisticLocking: TBoldComboBox
          Left = 120
          Top = 200
          Width = 291
          Height = 21
          Hint = 
            '|Specifies what kind of optimistic locking mechanism will be use' +
            'd.'
          HelpContext = 1250
          Alignment = taLeftJustify
          BoldHandle = behModel
          BoldListHandle = bchOptimisticLocking
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'taggedValue['#39'Bold.OptimisticLocking'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 9
          Visible = False
        end
        object tbxModelRootClass: TBoldEdit
          Left = 120
          Top = 176
          Width = 291
          Height = 21
          Hint = '|The root class of the model.'
          HelpContext = 12
          BoldHandle = behModel
          BoldProperties.Expression = 'taggedValue['#39'Bold.RootClass'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 8
        end
      end
      object pcModelTabs: TPageControl
        Left = 0
        Top = 335
        Width = 607
        Height = 177
        ActivePage = TabSheet2
        Align = alClient
        TabOrder = 1
        object TabSheet1: TTabSheet
          HelpContext = 87
          Caption = '&Classes'
          object BoldGrid1: TBoldGrid
            Left = 0
            Top = 0
            Width = 599
            Height = 149
            HelpContext = 87
            AddNewAtEnd = False
            Align = alClient
            BoldAutoColumns = False
            BoldShowConstraints = False
            BoldHandle = blhModelClasses
            BoldProperties.DefaultDblClick = False
            BoldProperties.InternalDrag = False
            BoldProperties.NilElementMode = neNone
            Columns = <
              item
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
              end
              item
                BoldProperties.Expression = 'name'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Name'
              end
              item
                BoldProperties.Expression = 'superclass.name'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Superclass'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.FileName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'File name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.TableMapping'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Table mapping'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Delphi name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Expression name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.PMapper'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'PMapper name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.TableName'#39'].value'#13
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Table name'
              end
              item
                BoldProperties.Expression = 'persistent'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Persistent'
              end
              item
                BoldProperties.Expression = 'isabstract'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Abstract'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.Imported'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Imported'
              end>
            DefaultRowHeight = 17
            EnableColAdjust = False
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            ColWidths = (
              17
              100
              70
              75
              64
              64
              64
              64
              64
              64
              64
              64)
          end
        end
        object TabSheet2: TTabSheet
          HelpContext = 32
          Caption = '&Associations'
          object BoldGrid2: TBoldGrid
            Left = 0
            Top = 0
            Width = 599
            Height = 149
            HelpContext = 32
            AddNewAtEnd = False
            Align = alClient
            BoldAutoColumns = False
            BoldShowConstraints = False
            BoldHandle = blhModelAssociations
            BoldProperties.DefaultDblClick = False
            BoldProperties.InternalDrag = False
            BoldProperties.NilElementMode = neNone
            Columns = <
              item
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
              end
              item
                BoldProperties.Expression = 'name'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Name'
              end
              item
                BoldProperties.Expression = 'class'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Class'
              end
              item
                BoldProperties.Expression = 
                  'if connection->size > 0 then '#13'  connection->at(1).name'#13' else '#13'  ' +
                  #39'<None>'#39#13'endif'#13
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Connection 1'
              end
              item
                BoldProperties.Expression = 
                  'if connection->size > 1 then '#13'  connection->at(2).name'#13' else '#13'  ' +
                  #39'<None>'#39#13'endif'#13
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Connection 2'
              end>
            DefaultRowHeight = 17
            EnableColAdjust = False
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            ColWidths = (
              17
              64
              64
              64
              64)
            RowHeights = (
              17
              17)
          end
        end
        object TabSheet9: TTabSheet
          HelpContext = 1010
          Caption = 'Packages'
          ImageIndex = 2
          TabVisible = False
        end
      end
    end
    object tabClass: TTabSheet
      HelpContext = 52
      Caption = 'Class'
      object Splitter3: TSplitter
        Left = 0
        Top = 393
        Width = 607
        Height = 6
        Cursor = crVSplit
        Align = alTop
        Beveled = True
      end
      object sbClass: TScrollBox
        Left = 0
        Top = 0
        Width = 607
        Height = 393
        HelpContext = 52
        Align = alTop
        BorderStyle = bsNone
        TabOrder = 0
        DesignSize = (
          607
          393)
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
          Left = 53
          Top = 300
          Width = 59
          Height = 13
          Alignment = taRightJustify
          Caption = 'Delph&i name'
          FocusControl = tbxClassDelphiName
          Visible = False
        end
        object lblClassExpressionName: TLabel
          Left = 32
          Top = 324
          Width = 80
          Height = 13
          Alignment = taRightJustify
          Caption = 'E&xpression name'
          FocusControl = tbxClassExpressionName
          Visible = False
        end
        object lblClassPMapperName: TLabel
          Left = 40
          Top = 348
          Width = 72
          Height = 13
          Alignment = taRightJustify
          Caption = '&PMapper name'
          FocusControl = cmbClassPMapperName
          Visible = False
        end
        object lblClassTableName: TLabel
          Left = 56
          Top = 372
          Width = 56
          Height = 13
          Alignment = taRightJustify
          Caption = 'T&able name'
          FocusControl = tbxClassTableName
          Visible = False
        end
        object lblClassFileName: TLabel
          Left = 32
          Top = 132
          Width = 80
          Height = 13
          Alignment = taRightJustify
          Caption = 'Include file name'
          FocusControl = tbxClassFileName
        end
        object lblClassTableMapping: TLabel
          Left = 42
          Top = 180
          Width = 70
          Height = 13
          Alignment = taRightJustify
          Caption = 'Table mappin&g'
          FocusControl = cmbTableMapping
        end
        object lblClassSuperclass: TLabel
          Left = 60
          Top = 84
          Width = 52
          Height = 13
          Alignment = taRightJustify
          Caption = '&Superclass'
          FocusControl = cmbSuperClass
        end
        object blbClassInfo: TBoldLabel
          Left = 120
          Top = 4
          Width = 273
          Height = 24
          AutoSize = False
          BoldHandle = behClass
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
          Left = 60
          Top = 228
          Width = 52
          Height = 13
          Alignment = taRightJustify
          Caption = '&Constraints'
          FocusControl = tbxClassConstraint
          Visible = False
        end
        object lblClassDerivationExpressions: TLabel
          Left = 6
          Top = 204
          Width = 106
          Height = 13
          Alignment = taRightJustify
          Caption = 'Derivation expressions'
          FocusControl = tbxClassDerivationExpressions
          Visible = False
        end
        object Label12: TLabel
          Left = 64
          Top = 108
          Width = 48
          Height = 13
          Alignment = taRightJustify
          Caption = 'Unit name'
          FocusControl = tbxClassUnitName
        end
        object lblClassDefaultStringRep: TLabel
          Left = 32
          Top = 156
          Width = 80
          Height = 13
          Alignment = taRightJustify
          Caption = 'Default string rep'
          FocusControl = edClassDefaultStringRep
        end
        object lblClassGUID: TLabel
          Left = 86
          Top = 252
          Width = 27
          Height = 13
          Alignment = taRightJustify
          Caption = 'GUID'
          FocusControl = tbxModelConstraints
          Visible = False
        end
        object lblClassOptimisticLocking: TLabel
          Left = 26
          Top = 276
          Width = 86
          Height = 13
          Alignment = taRightJustify
          Caption = 'Optimistic &Locking'
          FocusControl = tbxModelConstraints
          Visible = False
        end
        object tbxClassName: TBoldEdit
          Left = 120
          Top = 32
          Width = 291
          Height = 21
          Hint = '|Modeled name of the class.'
          HelpContext = 62
          BoldHandle = behClass
          BoldProperties.Expression = 'name'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 0
        end
        object tbxClassDelphiName: TBoldEdit
          Left = 120
          Top = 296
          Width = 291
          Height = 21
          Hint = '|Template for the name used to access this class in source code.'
          HelpContext = 5
          BoldHandle = behClass
          BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 14
          Visible = False
        end
        object tbxClassExpressionName: TBoldEdit
          Left = 120
          Top = 320
          Width = 291
          Height = 21
          Hint = '|Template for the name used to access the class from OCL.'
          HelpContext = 53
          BoldHandle = behClass
          BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 15
          Visible = False
        end
        object tbxClassTableName: TBoldEdit
          Left = 120
          Top = 368
          Width = 291
          Height = 21
          Hint = '|Template for the name used for this class'#39' table name.'
          HelpContext = 54
          BoldHandle = behClass
          BoldProperties.Expression = 'taggedValue['#39'Bold.TableName'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 17
          Visible = False
        end
        object tbxClassFileName: TBoldEdit
          Left = 120
          Top = 128
          Width = 291
          Height = 21
          Hint = 
            '|Name of include file into which this class'#39' business method imp' +
            'lementation is generated.'
          HelpContext = 1140
          BoldHandle = behClass
          BoldProperties.Expression = 'taggedValue['#39'Bold.FileName'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 4
        end
        object bcbClassPersistent: TBoldCheckBox
          Left = 418
          Top = 30
          Width = 69
          Height = 17
          Hint = '|Controls of this class is to be saved in persistent storage.'
          HelpContext = 56
          Anchors = [akTop, akRight]
          BoldHandle = behClass
          BoldProperties.Expression = 'persistent'
          BoldProperties.ApplyPolicy = bapChange
          Caption = 'Pe&rsistent'
          ReadOnly = False
          TabOrder = 18
        end
        object bcbClassAbstract: TBoldCheckBox
          Left = 418
          Top = 46
          Width = 69
          Height = 17
          Hint = '|Controls if this class can be instantiated.'
          HelpContext = 420
          Anchors = [akTop, akRight]
          BoldHandle = behClass
          BoldProperties.Expression = 'isAbstract'
          BoldProperties.ApplyPolicy = bapChange
          Caption = 'A&bstract'
          ReadOnly = False
          TabOrder = 19
        end
        object bcbClassImported: TBoldCheckBox
          Left = 418
          Top = 62
          Width = 69
          Height = 17
          HelpContext = 57
          Anchors = [akTop, akRight]
          BoldHandle = behClass
          BoldProperties.Expression = 'taggedValue['#39'Bold.Imported'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.Renderer = bcrBooleanToCheckBox
          Caption = 'Importe&d'
          ReadOnly = False
          TabOrder = 20
        end
        object cmbClassPMapperName: TBoldComboBox
          Left = 120
          Top = 344
          Width = 291
          Height = 21
          Hint = 
            '|Persistence mapper for this class. There is no reason to change' +
            ' from <Default>.'
          HelpContext = 58
          Alignment = taLeftJustify
          BoldHandle = behClass
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'taggedValue['#39'Bold.PMapper'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 16
          Visible = False
        end
        object cmbSuperClass: TBoldComboBox
          Left = 120
          Top = 80
          Width = 291
          Height = 21
          Hint = '|The immediate ancestor of this class.'
          HelpContext = 59
          Alignment = taLeftJustify
          BoldHandle = behClass
          BoldListHandle = blhAllSuperclasses
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'generalization->first.parent.name'
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.NilStringRepresentation = '<None>'
          BoldRowProperties.Expression = 'name'
          BoldRowProperties.NilStringRepresentation = '<None>'
          BoldSelectChangeAction = bdcsNone
          OnSelectChanged = cmbSuperClassSelectChanged
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 2
          Visible = False
        end
        object cmbTableMapping: TBoldComboBox
          Left = 120
          Top = 176
          Width = 291
          Height = 21
          Hint = '|Controls into which table the class persistent data goes.'
          HelpContext = 1000
          Alignment = taLeftJustify
          BoldHandle = behClass
          BoldListHandle = bcrTableMapping
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'taggedValue['#39'Bold.TableMapping'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 7
        end
        object tbxClassStereotype: TBoldEdit
          Left = 120
          Top = 56
          Width = 291
          Height = 21
          Hint = '|Stereotype applied on class, if any.'
          HelpContext = 1150
          BoldHandle = behClass
          BoldProperties.Expression = 'stereotypeName'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 1
        end
        object tbxClassConstraint: TBoldEdit
          Left = 120
          Top = 224
          Width = 275
          Height = 21
          Hint = '|Number of constraints specified for this class.'
          HelpContext = 14
          TabStop = False
          BoldHandle = behClass
          BoldProperties.Expression = 'constraint'
          BoldProperties.Renderer = bsrNiceCRRenderer
          ReadOnly = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 10
          Visible = False
        end
        object btClassConstraintEditor: TButton
          Left = 394
          Top = 224
          Width = 17
          Height = 21
          Hint = '|Edit the constraints for this class.'
          HelpContext = 24
          Anchors = [akTop, akRight]
          Caption = #188
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Symbol'
          Font.Style = []
          ParentFont = False
          TabOrder = 11
          Visible = False
          OnClick = btModelConstraintEditorClick
        end
        object tbxClassDerivationExpressions: TBoldEdit
          Left = 120
          Top = 200
          Width = 275
          Height = 21
          Hint = '|Redefinition of inherited derivation expressions.'
          HelpContext = 1170
          BoldHandle = behClass
          BoldProperties.Expression = 'taggedValue['#39'Bold.DerivationExpressions'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 8
          Visible = False
        end
        object btShowDerivationExpressionsEditor: TButton
          Left = 394
          Top = 200
          Width = 17
          Height = 21
          Hint = '|Edit inherited derived expressions.'
          HelpContext = 24
          Anchors = [akTop, akRight]
          Caption = #188
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Symbol'
          Font.Style = []
          ParentFont = False
          TabOrder = 9
          Visible = False
          OnClick = btShowDerivationExpressionsEditorClick
        end
        object tbxClassUnitName: TBoldEdit
          Left = 120
          Top = 104
          Width = 291
          Height = 21
          Hint = 
            '|The name of the unit this class will be generated into. Blank m' +
            'eans same as superclass.'
          HelpContext = 55
          BoldHandle = behClass
          BoldProperties.Expression = 'taggedValue['#39'Bold.UnitName'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 3
        end
        object edClassDefaultStringRep: TBoldEdit
          Left = 120
          Top = 152
          Width = 275
          Height = 21
          Hint = 
            '|OCL expression for the string representation of the class objec' +
            'ts.'
          HelpContext = 1140
          BoldHandle = behClass
          BoldProperties.Expression = 'taggedValue['#39'Bold.DefaultStringRepresentation'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 5
        end
        object btClassDefaultStringRep: TButton
          Left = 394
          Top = 152
          Width = 17
          Height = 21
          Hint = '|Edit the default string representation.'
          HelpContext = 24
          Anchors = [akTop, akRight]
          Caption = #188
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Symbol'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          OnClick = btClassDefaultStringRepClick
        end
        object edClassGUID: TBoldEdit
          Left = 120
          Top = 248
          Width = 291
          Height = 21
          Hint = '|The GUID for this class (required for COM access)'
          HelpContext = 11
          BoldHandle = behClass
          BoldProperties.Expression = 'taggedValue['#39'Bold.GUID'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 12
          Visible = False
        end
        object cmbClassOptimisticLocking: TBoldComboBox
          Left = 120
          Top = 272
          Width = 291
          Height = 21
          Hint = '|Specify the optimistic locking mechanism to use for this class.'
          HelpContext = 1250
          Alignment = taLeftJustify
          BoldHandle = behClass
          BoldListHandle = bchOptimisticLocking
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'taggedValue['#39'Bold.OptimisticLocking'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 13
          Visible = False
        end
        object bcbRemoveSuperOnUnboldify: TBoldCheckBox
          Left = 418
          Top = 87
          Width = 173
          Height = 17
          HelpContext = 57
          Anchors = [akTop, akRight]
          BoldHandle = behClass
          BoldProperties.Expression = 'generalization->first'
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.Renderer = bcrAutoCreated
          Caption = 'Remove Superclass on unboldify'
          ReadOnly = False
          TabOrder = 21
        end
        object bcbRemoveOnUnboldify: TBoldCheckBox
          Left = 418
          Top = 104
          Width = 149
          Height = 17
          HelpContext = 57
          Anchors = [akTop, akRight]
          BoldHandle = behClass
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.Renderer = bcrAutoCreated
          Caption = 'Remove class on unboldify'
          ReadOnly = False
          TabOrder = 22
        end
        object bcbIsRootClass: TBoldCheckBox
          Left = 418
          Top = 120
          Width = 97
          Height = 17
          Anchors = [akTop, akRight]
          BoldHandle = behClassIsRootClass
          Caption = 'Is Root Class'
          ReadOnly = False
          TabOrder = 23
        end
      end
      object pcClassTabs: TPageControl
        Left = 0
        Top = 399
        Width = 607
        Height = 113
        ActivePage = TabSheet4
        Align = alClient
        TabOrder = 1
        object TabSheet3: TTabSheet
          HelpContext = 64
          Caption = 'Attrib&utes'
          object BoldGrid3: TBoldGrid
            Left = 0
            Top = 0
            Width = 599
            Height = 85
            HelpContext = 64
            AddNewAtEnd = False
            Align = alClient
            BoldAutoColumns = False
            BoldShowConstraints = False
            BoldHandle = blhClassAttributes
            BoldProperties.DefaultDblClick = False
            BoldProperties.InternalDrag = False
            BoldProperties.NilElementMode = neNone
            Columns = <
              item
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
              end
              item
                BoldProperties.Expression = 'name'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Name'
              end
              item
                BoldProperties.Expression = 'typeName'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Type'
              end
              item
                BoldProperties.Expression = 'stereotypeName'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Stereotype'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.Length'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Length'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Delphi name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Expression name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.PMapper'#39'].value'#13
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'PMapper name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.ColumnName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Column name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.Persistent'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Persistent'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.AllowNULL'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Allow null'
              end
              item
                BoldProperties.Expression = 'derived'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Derived'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.DelayedFetch'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Delayed fetch'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.GetMethod'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Get method'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.SetMethod'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Set method'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.LocalVariable'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Local variable'
              end>
            DefaultRowHeight = 17
            EnableColAdjust = False
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            ColWidths = (
              17
              64
              64
              64
              64
              64
              64
              64
              64
              64
              64
              64
              64
              64
              64
              64)
          end
        end
        object TabSheet4: TTabSheet
          HelpContext = 61
          Caption = 'Operati&ons'
          object BoldGrid4: TBoldGrid
            Left = 0
            Top = 0
            Width = 599
            Height = 85
            HelpContext = 61
            AddNewAtEnd = False
            Align = alClient
            BoldAutoColumns = False
            BoldShowConstraints = False
            BoldHandle = blhClassOperations
            BoldProperties.DefaultDblClick = False
            BoldProperties.InternalDrag = False
            BoldProperties.NilElementMode = neNone
            Columns = <
              item
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
              end
              item
                BoldProperties.Expression = 'name'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Name'
              end
              item
                BoldProperties.Expression = 'ownerscope'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Owner scope'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.OperationKind'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Delphi function type'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Delphi name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Expression name'
              end>
            DefaultRowHeight = 17
            EnableColAdjust = False
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            ColWidths = (
              17
              64
              64
              64
              64
              64)
          end
        end
        object TabSheet8: TTabSheet
          Caption = 'Association ends'
          ImageIndex = 2
          object BoldGrid9: TBoldGrid
            Left = 0
            Top = 0
            Width = 599
            Height = 85
            HelpContext = 61
            AddNewAtEnd = False
            Align = alClient
            BoldAutoColumns = False
            BoldShowConstraints = False
            BoldHandle = blhClassAssociationEnds
            BoldProperties.DefaultDblClick = False
            BoldProperties.InternalDrag = False
            BoldProperties.NilElementMode = neNone
            Columns = <
              item
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
              end
              item
                BoldProperties.Expression = 'name'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Name'
              end
              item
                BoldProperties.Expression = 'stereotypeName'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Stereotype'
              end
              item
                BoldProperties.Expression = 'isNavigable'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
              end
              item
                BoldProperties.Expression = 'multi'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Multi'
              end
              item
                BoldProperties.Expression = 'mandatory'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Mandatory'
              end
              item
                BoldProperties.Expression = 'isOrdered'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
              end
              item
                BoldProperties.Expression = 'aggregation'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Aggregation'
              end
              item
                BoldProperties.Expression = 'multiplicity'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Multiplicity'
              end
              item
                BoldProperties.Expression = 'changeability'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Changeability'
              end
              item
                BoldProperties.Expression = 'visibility'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
              end>
            DefaultRowHeight = 17
            EnableColAdjust = False
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            ColWidths = (
              17
              64
              64
              64
              64
              64
              64
              64
              64
              64
              64)
          end
        end
      end
    end
    object tabAttribute: TTabSheet
      HelpContext = 63
      Caption = 'Attribute'
      DesignSize = (
        607
        512)
      object lblAttributeName: TLabel
        Left = 84
        Top = 36
        Width = 28
        Height = 13
        Alignment = taRightJustify
        Caption = '&Name'
        FocusControl = tbxAttributeName
      end
      object lblAttributeDelphiName: TLabel
        Left = 53
        Top = 228
        Width = 59
        Height = 13
        Alignment = taRightJustify
        Caption = 'Delph&i name'
        FocusControl = tbxAttributeDelphiName
        Visible = False
      end
      object lblAttributeExpressionName: TLabel
        Left = 32
        Top = 252
        Width = 80
        Height = 13
        Alignment = taRightJustify
        Caption = 'E&xpression name'
        FocusControl = tbxAttributeExpressionName
        Visible = False
      end
      object lblAttributePMapperName: TLabel
        Left = 40
        Top = 276
        Width = 72
        Height = 13
        Alignment = taRightJustify
        Caption = '&PMapper name'
        FocusControl = cmbAttributePMapperName
        Visible = False
      end
      object lblAttributeColumnName: TLabel
        Left = 48
        Top = 300
        Width = 64
        Height = 13
        Alignment = taRightJustify
        Caption = 'Col&umn name'
        FocusControl = tbxAttributeColumnName
        Visible = False
      end
      object lblAttributeBoldType: TLabel
        Left = 88
        Top = 84
        Width = 24
        Height = 13
        Alignment = taRightJustify
        Caption = 'Type'
        FocusControl = cmbAttributeBoldType
      end
      object lblAttributeLength: TLabel
        Left = 79
        Top = 132
        Width = 33
        Height = 13
        Alignment = taRightJustify
        Caption = 'Len&gth'
        FocusControl = tbxAttributeLength
      end
      object lblAttributeStereotype: TLabel
        Left = 61
        Top = 60
        Width = 51
        Height = 13
        Alignment = taRightJustify
        Caption = '&Stereotype'
        FocusControl = tbxAttributeStereotype
      end
      object blbAttributeInfo: TBoldLabel
        Left = 120
        Top = 4
        Width = 209
        Height = 24
        BoldHandle = behAttribute
        BoldProperties.Expression = 'owner.name + '#39'.'#39' + name'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 45
        Top = 4
        Width = 73
        Height = 24
        Caption = 'Attribute:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblAttributeKind: TLabel
        Left = 50
        Top = 324
        Width = 62
        Height = 13
        Alignment = taRightJustify
        Caption = 'Attribute &kind'
        FocusControl = cmbTVAttributeKind
        Visible = False
      end
      object lblAttributeDerivationOCL: TLabel
        Left = 40
        Top = 156
        Width = 72
        Height = 13
        Alignment = taRightJustify
        Caption = 'Derivation OC&L'
        FocusControl = tbxAttributeDerivationOCL
        Visible = False
      end
      object lblAttributeVisibility: TLabel
        Left = 76
        Top = 108
        Width = 36
        Height = 13
        Alignment = taRightJustify
        Caption = 'Visi&bility'
        FocusControl = cmbAttributeVisibility
      end
      object lblAttributeInitialValue: TLabel
        Left = 59
        Top = 204
        Width = 53
        Height = 13
        Alignment = taRightJustify
        Caption = 'Initial value'
        FocusControl = tbxAttributeInitialValue
        Visible = False
      end
      object lblAttributeConstraint: TLabel
        Left = 60
        Top = 180
        Width = 52
        Height = 13
        Alignment = taRightJustify
        Caption = '&Constraints'
        FocusControl = tbxAttributeConstraint
        Visible = False
      end
      object tbxAttributeName: TBoldEdit
        Left = 120
        Top = 32
        Width = 262
        Height = 21
        Hint = '|The modeled name for this attribute.'
        HelpContext = 77
        BoldHandle = behAttribute
        BoldProperties.Expression = 'name'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 0
      end
      object tbxAttributeDelphiName: TBoldEdit
        Left = 120
        Top = 224
        Width = 262
        Height = 21
        Hint = 
          '|Template for the name used to access this attribute in source c' +
          'ode.'
        HelpContext = 6
        BoldHandle = behAttribute
        BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 10
        Visible = False
      end
      object tbxAttributeExpressionName: TBoldEdit
        Left = 120
        Top = 248
        Width = 262
        Height = 21
        Hint = '|Template for the name used to access this attribute in OCL.'
        HelpContext = 65
        BoldHandle = behAttribute
        BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 11
        Visible = False
      end
      object tbxAttributeColumnName: TBoldEdit
        Left = 120
        Top = 296
        Width = 262
        Height = 21
        Hint = 
          '|Template for the name used for the SQL column of this attribute' +
          '..'
        HelpContext = 66
        BoldHandle = behAttribute
        BoldProperties.Expression = 'taggedValue['#39'Bold.ColumnName'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 13
        Visible = False
      end
      object bcbAttributePersistent: TBoldCheckBox
        Left = 389
        Top = 30
        Width = 97
        Height = 17
        Hint = 
          '|Controls of this attribute is persistent (stored in the persist' +
          'ent storage)'
        HelpContext = 67
        Anchors = [akTop, akRight]
        BoldHandle = behAttribute
        BoldProperties.Expression = 'persistent'
        BoldProperties.ApplyPolicy = bapChange
        Caption = 'Persistent'
        ReadOnly = False
        TabOrder = 16
      end
      object bcbAttributeDelayedFetch: TBoldCheckBox
        Left = 389
        Top = 94
        Width = 97
        Height = 17
        Hint = 
          '|Controls if the attribute is fetched with the rest of the objec' +
          't, or not until it is explicitly requested.'
        HelpContext = 78
        Anchors = [akTop, akRight]
        BoldHandle = behAttribute
        BoldProperties.Expression = 'taggedValue['#39'Bold.DelayedFetch'#39'].value'
        BoldProperties.ApplyPolicy = bapChange
        BoldProperties.Renderer = bcrBooleanToCheckBox
        Caption = 'Dela&yed fetch'
        Enabled = False
        ReadOnly = False
        TabOrder = 20
      end
      object bcbAttributeAllowNull: TBoldCheckBox
        Left = 389
        Top = 46
        Width = 97
        Height = 17
        Hint = '|Controls if the attribute can be NULL.'
        HelpContext = 68
        Anchors = [akTop, akRight]
        BoldHandle = behAttribute
        BoldProperties.Expression = 'taggedValue['#39'Bold.AllowNULL'#39'].value'
        BoldProperties.ApplyPolicy = bapChange
        BoldProperties.Renderer = bcrBooleanToCheckBox
        Caption = '&Allow null'
        ReadOnly = False
        TabOrder = 17
      end
      object bcbAttributeDerived: TBoldCheckBox
        Left = 389
        Top = 62
        Width = 97
        Height = 17
        Hint = '|Controls if the attribute is derived.'
        HelpContext = 69
        Anchors = [akTop, akRight]
        BoldHandle = behAttribute
        BoldProperties.Expression = 'derived'
        BoldProperties.ApplyPolicy = bapChange
        Caption = '&Derived'
        ReadOnly = False
        TabOrder = 18
      end
      object tbxAttributeLength: TBoldEdit
        Left = 120
        Top = 128
        Width = 262
        Height = 21
        Hint = 
          '|The maximum length of the attribute (relevant for strings only)' +
          '.'
        HelpContext = 70
        BoldHandle = behAttribute
        BoldProperties.Expression = 'taggedValue['#39'Bold.Length'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 4
      end
      object cmbAttributeBoldType: TBoldComboBox
        Left = 120
        Top = 80
        Width = 262
        Height = 21
        Hint = '|The type of the attribute.'
        HelpContext = 71
        Alignment = taLeftJustify
        BoldHandle = behAttribute
        BoldListHandle = blhAllDataTypes
        BoldListProperties.DragMode = bdgSelection
        BoldListProperties.DropMode = bdpAppend
        BoldListProperties.NilElementMode = neNone
        BoldProperties.Expression = 'type.Name'
        BoldProperties.ApplyPolicy = bapChange
        BoldRowProperties.Expression = 'name'
        BoldSetValueExpression = 'type'
        BoldSelectChangeAction = bdcsSetValue
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 0
        Sorted = True
        TabOrder = 2
      end
      object cmbAttributePMapperName: TBoldComboBox
        Left = 120
        Top = 272
        Width = 262
        Height = 21
        Hint = '|Persistence mapper to be used for this attribute.'
        HelpContext = 72
        Alignment = taLeftJustify
        BoldHandle = behAttribute
        BoldListHandle = bchAttributePMapperNames
        BoldListProperties.DragMode = bdgSelection
        BoldListProperties.DropMode = bdpAppend
        BoldListProperties.NilElementMode = neNone
        BoldProperties.Expression = 'taggedValue['#39'Bold.PMapper'#39'].value'
        BoldProperties.ApplyPolicy = bapChange
        BoldSelectChangeAction = bdscSetText
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 0
        Sorted = True
        TabOrder = 12
        Visible = False
      end
      object cmbTVAttributeKind: TBoldComboBox
        Left = 120
        Top = 320
        Width = 262
        Height = 21
        Hint = 
          '|Determines if this is a Bold attribute, or a plain Delphi attri' +
          'bute.'
        HelpContext = 1200
        Alignment = taLeftJustify
        BoldHandle = behAttribute
        BoldListHandle = bchAttributeKind
        BoldListProperties.DragMode = bdgSelection
        BoldListProperties.DropMode = bdpAppend
        BoldListProperties.NilElementMode = neNone
        BoldProperties.Expression = 'taggedValue['#39'Bold.AttributeKind'#39'].value'
        BoldProperties.ApplyPolicy = bapChange
        BoldSelectChangeAction = bdscSetText
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 0
        TabOrder = 14
        Visible = False
      end
      object gbDelphiFeatures: TGroupBox
        Left = 40
        Top = 356
        Width = 353
        Height = 83
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Attribute kind Delphi features'
        TabOrder = 15
        Visible = False
        DesignSize = (
          353
          83)
        object lblDPRead: TLabel
          Left = 9
          Top = 36
          Width = 63
          Height = 13
          Alignment = taRightJustify
          Caption = 'Pr&operty read'
          FocusControl = tbxAttributeColumnName
        end
        object lblDPWrite: TLabel
          Left = 8
          Top = 60
          Width = 64
          Height = 13
          Alignment = taRightJustify
          Caption = 'Property &write'
          FocusControl = tbxAttributeColumnName
        end
        object bcbDPHasField: TBoldCheckBox
          Left = 80
          Top = 16
          Width = 97
          Height = 17
          Hint = 
            '|Check this to get a private instance variable for the attribute' +
            '.'
          HelpContext = 1210
          BoldHandle = behAttribute
          BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiField'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.Renderer = bcrBooleanToCheckBox
          Caption = 'Delphi field'
          ReadOnly = False
          TabOrder = 0
        end
        object cmbTVDPRead: TBoldComboBox
          Left = 80
          Top = 32
          Width = 262
          Height = 21
          Hint = '|Controls the read mechanism for the attribute.'
          HelpContext = 1220
          Alignment = taLeftJustify
          BoldHandle = behAttribute
          BoldListHandle = bchDelphiProperty
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiPropertyRead'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 0
          TabOrder = 1
        end
        object cmbTVDPWrite: TBoldComboBox
          Left = 80
          Top = 56
          Width = 262
          Height = 21
          Hint = '|Controls the write mechanism for the attribute.'
          HelpContext = 1230
          Alignment = taLeftJustify
          BoldHandle = behAttribute
          BoldListHandle = bchDelphiProperty
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiPropertyWrite'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 0
          TabOrder = 2
        end
      end
      object tbxAttributeDerivationOCL: TBoldEdit
        Left = 120
        Top = 152
        Width = 246
        Height = 21
        Hint = '|The OCL expression used to derive this attribute.'
        HelpContext = 1180
        BoldHandle = behAttribute
        BoldProperties.Expression = 'taggedValue['#39'Bold.DerivationOCL'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 5
        Visible = False
      end
      object tbxAttributeStereotype: TBoldEdit
        Left = 120
        Top = 56
        Width = 262
        Height = 21
        Hint = '|Stereotype applied to this attribute.'
        HelpContext = 1150
        BoldHandle = behAttribute
        BoldProperties.Expression = 'stereotypeName'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 1
      end
      object cmbAttributeVisibility: TBoldComboBox
        Left = 120
        Top = 104
        Width = 262
        Height = 21
        Hint = 
          '|The visibility scope into which this attribute will be generate' +
          'd.'
        HelpContext = 1250
        Alignment = taLeftJustify
        BoldHandle = behAttribute
        BoldListHandle = blhAllVisibilityKind
        BoldListProperties.DragMode = bdgSelection
        BoldListProperties.DropMode = bdpAppend
        BoldListProperties.NilElementMode = neNone
        BoldProperties.Expression = 'visibility'
        BoldProperties.ApplyPolicy = bapChange
        BoldSelectChangeAction = bdscSetText
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 0
        TabOrder = 3
      end
      object tbxAttributeInitialValue: TBoldEdit
        Left = 120
        Top = 200
        Width = 262
        Height = 21
        Hint = 
          '|Initial value for the attribute. This will be set when the attr' +
          'ibute is first created/accessed.'
        HelpContext = 1190
        BoldHandle = behAttribute
        BoldProperties.Expression = 'initialValue'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 9
        Visible = False
      end
      object tbxAttributeConstraint: TBoldEdit
        Left = 120
        Top = 176
        Width = 246
        Height = 21
        Hint = '|The number of constraints for this attribute.'
        HelpContext = 14
        TabStop = False
        BoldHandle = behAttribute
        BoldProperties.Expression = 'constraint'
        BoldProperties.Renderer = bsrNiceCRRenderer
        ReadOnly = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 7
        Visible = False
      end
      object btAttributeConstraintEditor: TButton
        Left = 365
        Top = 176
        Width = 17
        Height = 21
        Hint = '|Edit the constraints for this attribute.'
        HelpContext = 24
        Anchors = [akTop, akRight]
        Caption = #188
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Symbol'
        Font.Style = []
        ParentFont = False
        TabOrder = 8
        Visible = False
        OnClick = btModelConstraintEditorClick
      end
      object bcbAttributeReverseDerive: TBoldCheckBox
        Left = 389
        Top = 78
        Width = 97
        Height = 17
        Hint = '|Controls if the attribute is reverse derived.'
        HelpContext = 1240
        Anchors = [akTop, akRight]
        BoldHandle = behAttribute
        BoldProperties.Expression = 'taggedValue['#39'Bold.ReverseDerive'#39'].value'
        BoldProperties.ApplyPolicy = bapChange
        BoldProperties.Renderer = bcrBooleanToCheckBox
        Caption = '&Reverse derive'
        Enabled = False
        ReadOnly = False
        TabOrder = 19
      end
      object btAttributeShowDerivExprEditor: TButton
        Left = 365
        Top = 152
        Width = 17
        Height = 21
        Hint = '|Edit the derivation expression for this attribute.'
        HelpContext = 24
        Anchors = [akTop, akRight]
        Caption = #188
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Symbol'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
        Visible = False
        OnClick = btAttributeShowDerivExprEditorClick
      end
    end
    object tabOperation: TTabSheet
      HelpContext = 76
      Caption = 'Operation'
      object Splitter4: TSplitter
        Left = 0
        Top = 229
        Width = 607
        Height = 6
        Cursor = crVSplit
        Align = alTop
        Beveled = True
      end
      object sbOperation: TScrollBox
        Left = 0
        Top = 0
        Width = 607
        Height = 229
        HelpContext = 76
        Align = alTop
        BorderStyle = bsNone
        TabOrder = 0
        DesignSize = (
          607
          229)
        object lblOperationName: TLabel
          Left = 84
          Top = 36
          Width = 28
          Height = 13
          Alignment = taRightJustify
          Caption = '&Name'
          FocusControl = tbxOperationName
        end
        object lblOperationDelphiName: TLabel
          Left = 53
          Top = 180
          Width = 59
          Height = 13
          Alignment = taRightJustify
          Caption = 'Delph&i name'
          FocusControl = tbxOperationDelphiName
          Visible = False
        end
        object lblOperationExpressionName: TLabel
          Left = 32
          Top = 204
          Width = 80
          Height = 13
          Alignment = taRightJustify
          Caption = 'E&xpression name'
          FocusControl = tbxOperationExpressionName
          Visible = False
        end
        object lblOperationOwnerScope: TLabel
          Left = 49
          Top = 108
          Width = 63
          Height = 13
          Alignment = taRightJustify
          Caption = '&Owner scope'
          FocusControl = cmbOwnerScope
        end
        object lblOperationDelphiFunctionType: TLabel
          Left = 43
          Top = 132
          Width = 69
          Height = 13
          Alignment = taRightJustify
          Caption = 'O&peration kind'
          FocusControl = cmbDelphiFunctionType
        end
        object blbOperationInfo: TBoldLabel
          Left = 120
          Top = 4
          Width = 209
          Height = 24
          BoldHandle = behOperation
          BoldProperties.Expression = 'owner.name + '#39'.'#39' + name'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 30
          Top = 4
          Width = 88
          Height = 24
          Caption = 'Operation:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblOperationStereotype: TLabel
          Left = 61
          Top = 60
          Width = 51
          Height = 13
          Alignment = taRightJustify
          Caption = '&Stereotype'
          FocusControl = tbxOperationStereotype
        end
        object lblOperationVisibility: TLabel
          Left = 76
          Top = 84
          Width = 36
          Height = 13
          Alignment = taRightJustify
          Caption = 'Visi&bility'
          FocusControl = cmbOperationVisibility
        end
        object lblOperationConstraint: TLabel
          Left = 60
          Top = 156
          Width = 52
          Height = 13
          Alignment = taRightJustify
          Caption = '&Constraints'
          FocusControl = tbxOperationConstraint
          Visible = False
        end
        object tbxOperationName: TBoldEdit
          Left = 120
          Top = 32
          Width = 262
          Height = 21
          Hint = '|The modeled name of the operation.'
          HelpContext = 83
          BoldHandle = behOperation
          BoldProperties.Expression = 'name'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 0
        end
        object tbxOperationDelphiName: TBoldEdit
          Left = 120
          Top = 176
          Width = 262
          Height = 21
          Hint = 
            '|Template for the name used to access this operation in source c' +
            'ode.'
          HelpContext = 7
          BoldHandle = behOperation
          BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 7
          Visible = False
        end
        object tbxOperationExpressionName: TBoldEdit
          Left = 120
          Top = 200
          Width = 262
          Height = 21
          Hint = '|Template for OCL name (not relevant for operations).'
          HelpContext = 79
          BoldHandle = behOperation
          BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 8
          Visible = False
        end
        object cmbOwnerScope: TBoldComboBox
          Left = 120
          Top = 104
          Width = 262
          Height = 21
          Hint = 
            '|Determines if the operation belongs to the instance or the clas' +
            's.'
          HelpContext = 80
          Alignment = taLeftJustify
          BoldHandle = behOperation
          BoldListHandle = blhAllOwnerScope
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'ownerScope'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 0
          TabOrder = 3
        end
        object cmbDelphiFunctionType: TBoldComboBox
          Left = 120
          Top = 128
          Width = 262
          Height = 21
          Hint = '|Controls the directives of the operation.'
          HelpContext = 81
          Alignment = taLeftJustify
          BoldHandle = behOperation
          BoldListHandle = bchDelphiFunctionType
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'taggedValue['#39'Bold.OperationKind'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 0
          TabOrder = 4
        end
        object tbxOperationStereotype: TBoldEdit
          Left = 120
          Top = 56
          Width = 262
          Height = 21
          Hint = '|The stereotype applied to the operation.'
          HelpContext = 1150
          BoldHandle = behOperation
          BoldProperties.Expression = 'stereotypeName'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 1
        end
        object cmbOperationVisibility: TBoldComboBox
          Left = 120
          Top = 80
          Width = 262
          Height = 21
          Hint = 
            '|The visibility scope into which this operation will be generate' +
            'd'
          HelpContext = 1250
          Alignment = taLeftJustify
          BoldHandle = behOperation
          BoldListHandle = blhAllVisibilityKind
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'visibility'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 0
          TabOrder = 2
        end
        object tbxOperationConstraint: TBoldEdit
          Left = 120
          Top = 152
          Width = 246
          Height = 21
          Hint = '|The number of constraints defined for this operation.'
          HelpContext = 14
          TabStop = False
          BoldHandle = behOperation
          BoldProperties.Expression = 'constraint'
          BoldProperties.Renderer = bsrNiceCRRenderer
          ReadOnly = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 5
          Visible = False
        end
        object btOperationConstraintEditor: TButton
          Left = 365
          Top = 152
          Width = 17
          Height = 21
          Hint = '|Edit the constraints for this operation.'
          HelpContext = 24
          Anchors = [akTop, akRight]
          Caption = #188
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Symbol'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          Visible = False
          OnClick = btModelConstraintEditorClick
        end
        object cbOperationOverrideInAllSubclasses: TBoldCheckBox
          Left = 389
          Top = 30
          Width = 149
          Height = 17
          Hint = 
            '|If checked, the operation will be added to all subclasses at co' +
            'de generation.'
          HelpContext = 1260
          Anchors = [akTop, akRight]
          BoldHandle = behOperation
          BoldProperties.Expression = 'taggedValue['#39'Bold.OverrideInAllSubclasses'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.Renderer = bcrBooleanToCheckBox
          Caption = 'Override in all s&ubclasses'
          ReadOnly = False
          TabOrder = 9
        end
      end
      object pcOperationTabs: TPageControl
        Left = 0
        Top = 235
        Width = 607
        Height = 277
        ActivePage = TabSheet5
        Align = alClient
        TabOrder = 1
        object TabSheet5: TTabSheet
          HelpContext = 84
          Caption = '&Parameters'
          object BoldGrid5: TBoldGrid
            Left = 0
            Top = 0
            Width = 599
            Height = 249
            HelpContext = 84
            AddNewAtEnd = False
            Align = alClient
            BoldAutoColumns = False
            BoldShowConstraints = False
            BoldHandle = blhParameters
            BoldProperties.DefaultDblClick = False
            BoldProperties.InternalDrag = False
            BoldProperties.NilElementMode = neNone
            Columns = <
              item
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
              end
              item
                BoldProperties.Expression = 'name'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Name'
              end
              item
                BoldProperties.Expression = 'kind'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Kind'
              end
              item
                BoldProperties.Expression = 'typeName'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Type'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Expression name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.IsConst'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Is const'
              end>
            DefaultRowHeight = 17
            EnableColAdjust = False
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            ColWidths = (
              17
              64
              64
              64
              64
              64)
          end
        end
      end
    end
    object tabAssociation: TTabSheet
      HelpContext = 20
      Caption = 'Association'
      object Splitter5: TSplitter
        Left = 0
        Top = 128
        Width = 607
        Height = 6
        Cursor = crVSplit
        Align = alTop
        Beveled = True
      end
      object sbAssociation: TScrollBox
        Left = 0
        Top = 0
        Width = 607
        Height = 128
        HelpContext = 20
        Align = alTop
        BorderStyle = bsNone
        TabOrder = 0
        DesignSize = (
          607
          128)
        object lblAssociationName: TLabel
          Left = 84
          Top = 36
          Width = 28
          Height = 13
          Alignment = taRightJustify
          Caption = '&Name'
          FocusControl = tbxAssociationName
        end
        object lblAssociationClass: TLabel
          Left = 87
          Top = 84
          Width = 25
          Height = 13
          Alignment = taRightJustify
          Caption = 'C&lass'
          FocusControl = cmbAssociationLinkClass
        end
        object blbAssociationInfo: TBoldLabel
          Left = 120
          Top = 4
          Width = 48
          Height = 24
          BoldHandle = behAssociation
          BoldProperties.Expression = 'name'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label7: TLabel
          Left = 17
          Top = 4
          Width = 101
          Height = 24
          Caption = 'Association:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblAssociationStereotype: TLabel
          Left = 61
          Top = 60
          Width = 51
          Height = 13
          Alignment = taRightJustify
          Caption = '&Stereotype'
          FocusControl = tbxAssociationStereotype
        end
        object lblAssociationConstraint: TLabel
          Left = 60
          Top = 108
          Width = 52
          Height = 13
          Alignment = taRightJustify
          Caption = '&Constraints'
          FocusControl = tbxAssociationConstraint
          Visible = False
        end
        object tbxAssociationName: TBoldEdit
          Left = 120
          Top = 32
          Width = 291
          Height = 21
          Hint = '|Modeled name of association.'
          HelpContext = 35
          BoldHandle = behAssociation
          BoldProperties.Expression = 'name'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 0
        end
        object cmbAssociationLinkClass: TBoldComboBox
          Left = 120
          Top = 80
          Width = 291
          Height = 21
          Hint = '|Specifies the link class for this association.'
          HelpContext = 2
          Alignment = taLeftJustify
          BoldHandle = behAssociation
          BoldListHandle = blhAllClasses
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neInsertFirst
          BoldProperties.Expression = 'class.name'
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.NilStringRepresentation = '<none>'
          BoldRowProperties.Expression = 'name'
          BoldRowProperties.NilStringRepresentation = '<none>'
          BoldSetValueExpression = 'class'
          BoldSelectChangeAction = bdcsSetValue
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 2
        end
        object tbxAssociationStereotype: TBoldEdit
          Left = 120
          Top = 56
          Width = 291
          Height = 21
          Hint = '|Stereotype applied to acssociation.'
          HelpContext = 1150
          BoldHandle = behAssociation
          BoldProperties.Expression = 'stereotypeName'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 1
        end
        object tbxAssociationConstraint: TBoldEdit
          Left = 120
          Top = 104
          Width = 275
          Height = 21
          Hint = '|Number of constraints specified for the association.'
          HelpContext = 14
          TabStop = False
          BoldHandle = behAssociation
          BoldProperties.Expression = 'constraint'
          BoldProperties.Renderer = bsrNiceCRRenderer
          ReadOnly = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 3
          Visible = False
        end
        object btAssociationConstraintEditor: TButton
          Left = 394
          Top = 104
          Width = 17
          Height = 21
          Hint = '|Edit the constraints for this association.'
          HelpContext = 24
          Anchors = [akTop, akRight]
          Caption = #188
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Symbol'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          Visible = False
          OnClick = btModelConstraintEditorClick
        end
        object bcbAssociationDerived: TBoldCheckBox
          Left = 418
          Top = 46
          Width = 73
          Height = 17
          Hint = '|Controls if the association is derived.'
          HelpContext = 1270
          Anchors = [akTop, akRight]
          BoldHandle = behAssociation
          BoldProperties.Expression = 'derived'
          BoldProperties.ApplyPolicy = bapChange
          Caption = 'Der&ived'
          ReadOnly = False
          TabOrder = 6
        end
        object bcbPersistent: TBoldCheckBox
          Left = 418
          Top = 30
          Width = 73
          Height = 17
          Hint = 
            '|Copntrols if the association is persistent (saved in persistent' +
            ' storage)'
          HelpContext = 1270
          Anchors = [akTop, akRight]
          BoldHandle = behAssociation
          BoldProperties.Expression = 'persistent'
          BoldProperties.ApplyPolicy = bapChange
          Caption = '&Persistent'
          ReadOnly = False
          TabOrder = 5
        end
      end
      object pcAssociationTabs: TPageControl
        Left = 0
        Top = 134
        Width = 607
        Height = 378
        ActivePage = TabSheet6
        Align = alClient
        TabOrder = 1
        object TabSheet6: TTabSheet
          HelpContext = 86
          Caption = '&AssociationEnds'
          object BoldGrid6: TBoldGrid
            Left = 0
            Top = 0
            Width = 599
            Height = 350
            HelpContext = 86
            AddNewAtEnd = False
            Align = alClient
            BoldAutoColumns = False
            BoldShowConstraints = False
            BoldHandle = blhAssociationAssociationEnds
            BoldProperties.DefaultDblClick = False
            BoldProperties.InternalDrag = False
            BoldProperties.NilElementMode = neNone
            Columns = <
              item
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
              end
              item
                BoldProperties.Expression = 'name'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Name'
              end
              item
                BoldProperties.Expression = 'type'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Class'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Delphi name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Expression name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.DBName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Column name'
              end
              item
                BoldProperties.Expression = 'isnavigable'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Navigable'
              end
              item
                BoldProperties.Expression = 'multi'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Multi'
              end
              item
                BoldProperties.Expression = 'isordered'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Ordered'
              end
              item
                BoldProperties.Expression = 'mandatory'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Mandatory'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.Embed'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Embed'
              end>
            DefaultRowHeight = 17
            EnableColAdjust = False
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            ColWidths = (
              17
              64
              64
              64
              64
              64
              64
              64
              64
              64
              64)
          end
        end
      end
    end
    object tabAssociationEnd: TTabSheet
      HelpContext = 40
      Caption = 'AssociationEnd'
      object Splitter6: TSplitter
        Left = 0
        Top = 349
        Width = 607
        Height = 6
        Cursor = crVSplit
        Align = alTop
        Beveled = True
      end
      object sbAssociationEnd: TScrollBox
        Left = 0
        Top = 0
        Width = 607
        Height = 349
        HelpContext = 40
        Align = alTop
        BorderStyle = bsNone
        TabOrder = 0
        DesignSize = (
          607
          349)
        object lblAssociationEndName: TLabel
          Left = 84
          Top = 36
          Width = 28
          Height = 13
          Alignment = taRightJustify
          Caption = '&Name'
          FocusControl = tbxAssociationEndName
        end
        object lblAssociationEndDelphiName: TLabel
          Left = 53
          Top = 276
          Width = 59
          Height = 13
          Alignment = taRightJustify
          Caption = 'Delph&i name'
          FocusControl = tbxAssociationEndDelphiName
          Visible = False
        end
        object lblAssociationEndExpressionName: TLabel
          Left = 32
          Top = 300
          Width = 80
          Height = 13
          Alignment = taRightJustify
          Caption = 'E&xpression name'
          FocusControl = tbxAssociationEndExpressionName
          Visible = False
        end
        object lblAssociationEndColumnName: TLabel
          Left = 48
          Top = 324
          Width = 64
          Height = 13
          Alignment = taRightJustify
          Caption = 'C&olumn name'
          FocusControl = tbxAssociationEndColumnName
          Visible = False
        end
        object lblAssociationEndClass: TLabel
          Left = 87
          Top = 84
          Width = 25
          Height = 13
          Alignment = taRightJustify
          Caption = 'C&lass'
          FocusControl = cmbAssoEndClass
        end
        object blbAssociationEndInfo: TBoldLabel
          Left = 120
          Top = 4
          Width = 439
          Height = 24
          BoldHandle = behAssociationEnd
          BoldProperties.Expression = 'otherEnd.type.name + '#39'.'#39' + name + '#39' -> '#39' + type.name'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label8: TLabel
          Left = 74
          Top = 4
          Width = 44
          Height = 24
          Caption = 'Role:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lblStereotype: TLabel
          Left = 61
          Top = 60
          Width = 51
          Height = 13
          Alignment = taRightJustify
          Caption = '&Stereotype'
          FocusControl = tbxAssociationEndStereotype
        end
        object Label2: TLabel
          Left = 55
          Top = 132
          Width = 57
          Height = 13
          Alignment = taRightJustify
          Caption = 'A&ggregation'
          FocusControl = cmbAggregationKind
        end
        object lblAssoEndVisibility: TLabel
          Left = 76
          Top = 156
          Width = 36
          Height = 13
          Alignment = taRightJustify
          Caption = 'Visibility'
          FocusControl = cmbAssoEndVisibility
        end
        object lblAssoEndChangeability: TLabel
          Left = 49
          Top = 180
          Width = 63
          Height = 13
          Alignment = taRightJustify
          Caption = 'Changeability'
          FocusControl = cmbAssoEndChangeability
        end
        object lblAssociationEndConstraint: TLabel
          Left = 60
          Top = 252
          Width = 52
          Height = 13
          Alignment = taRightJustify
          Caption = '&Constraints'
          FocusControl = tbxAssociationEndConstraint
          Visible = False
        end
        object lbMultiplicity: TLabel
          Left = 64
          Top = 108
          Width = 48
          Height = 13
          Alignment = taRightJustify
          Caption = 'Multi&plicity'
          FocusControl = cmbMultiplicity
        end
        object lblAssoEndDeleteAction: TLabel
          Left = 49
          Top = 204
          Width = 63
          Height = 13
          Alignment = taRightJustify
          Caption = '&Delete action'
          FocusControl = cmbAssoEndDeleteAction
        end
        object lblAssoEndDerivationOCL: TLabel
          Left = 40
          Top = 228
          Width = 72
          Height = 13
          Alignment = taRightJustify
          Caption = 'Derivation OCL'
          FocusControl = tbxAssoEndDerivationOCL
          Visible = False
        end
        object tbxAssociationEndName: TBoldEdit
          Left = 120
          Top = 32
          Width = 291
          Height = 21
          Hint = '|The modeled name of the role.'
          HelpContext = 51
          BoldHandle = behAssociationEnd
          BoldProperties.Expression = 'name'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 0
        end
        object tbxAssociationEndDelphiName: TBoldEdit
          Left = 120
          Top = 272
          Width = 291
          Height = 21
          Hint = '|Template for the name used to access this role in source code.'
          HelpContext = 4
          BoldHandle = behAssociationEnd
          BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 12
          Visible = False
        end
        object tbxAssociationEndExpressionName: TBoldEdit
          Left = 120
          Top = 296
          Width = 291
          Height = 21
          Hint = '|Template for the name used to access this role in OCL.'
          HelpContext = 42
          BoldHandle = behAssociationEnd
          BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 13
          Visible = False
        end
        object tbxAssociationEndColumnName: TBoldEdit
          Left = 120
          Top = 320
          Width = 291
          Height = 21
          Hint = '|Template for SQL column to store this role.'
          HelpContext = 43
          BoldHandle = behAssociationEnd
          BoldProperties.Expression = 'taggedValue['#39'Bold.ColumnName'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 14
          Visible = False
        end
        object bcbAssociationEndNavigable: TBoldCheckBox
          Left = 418
          Top = 30
          Width = 69
          Height = 17
          Hint = '|Controls if the role is navigable in this direction.'
          HelpContext = 44
          Anchors = [akTop, akRight]
          BoldHandle = behAssociationEnd
          BoldProperties.Expression = 'isNavigable'
          BoldProperties.ApplyPolicy = bapChange
          Caption = 'N&avigable'
          ReadOnly = False
          TabOrder = 15
        end
        object bcbAssociationEndMulti: TBoldCheckBox
          Left = 418
          Top = 46
          Width = 53
          Height = 17
          Hint = '|Indicates if the multiplicity if more than 1.'
          HelpContext = 45
          Anchors = [akTop, akRight]
          BoldHandle = behAssociationEnd
          BoldProperties.Expression = 'multi'
          BoldProperties.ApplyPolicy = bapChange
          Caption = 'M&ulti'
          ReadOnly = False
          TabOrder = 16
        end
        object bcbAssociationEndOrdered: TBoldCheckBox
          Left = 418
          Top = 62
          Width = 61
          Height = 17
          Hint = '|Controls if order is preserved.'
          HelpContext = 46
          Anchors = [akTop, akRight]
          BoldHandle = behAssociationEnd
          BoldProperties.Expression = 'isOrdered'
          BoldProperties.ApplyPolicy = bapChange
          Caption = 'O&rdered'
          ReadOnly = False
          TabOrder = 17
        end
        object bcbAssociationEndMandatory: TBoldCheckBox
          Left = 418
          Top = 78
          Width = 77
          Height = 17
          Hint = '|Indicates if the multiplicity is one or more.'
          HelpContext = 47
          Anchors = [akTop, akRight]
          BoldHandle = behAssociationEnd
          BoldProperties.Expression = 'mandatory'
          BoldProperties.ApplyPolicy = bapChange
          Caption = 'Mandator&y'
          ReadOnly = False
          TabOrder = 18
        end
        object bcbAssociationEndEmbed: TBoldCheckBox
          Left = 418
          Top = 94
          Width = 61
          Height = 17
          Hint = '|Controls if the role is embedded in the owning object.'
          HelpContext = 48
          Anchors = [akTop, akRight]
          BoldHandle = behAssociationEnd
          BoldProperties.Expression = 'taggedValue['#39'Bold.Embed'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldProperties.Renderer = bcrBooleanToCheckBox
          Caption = 'Em&bed'
          ReadOnly = False
          TabOrder = 19
        end
        object cmbAssoEndClass: TBoldComboBox
          Left = 120
          Top = 80
          Width = 291
          Height = 21
          Hint = '|Class where the role ends.'
          HelpContext = 49
          Alignment = taLeftJustify
          BoldHandle = behAssociationEndType
          BoldListHandle = blhAllClasses
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'name'
          BoldProperties.ApplyPolicy = bapChange
          BoldRowProperties.Expression = 'name'
          BoldSelectChangeAction = bdcsSetValue
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 0
          TabOrder = 2
        end
        object tbxAssociationEndStereotype: TBoldEdit
          Left = 120
          Top = 56
          Width = 291
          Height = 21
          Hint = '|Stereotype applied to this role.'
          HelpContext = 1150
          BoldHandle = behAssociationEnd
          BoldProperties.Expression = 'stereotypeName'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 1
        end
        object cmbAggregationKind: TBoldComboBox
          Left = 120
          Top = 128
          Width = 291
          Height = 21
          Hint = '|Type of aggregation of the role.'
          HelpContext = 1290
          Alignment = taLeftJustify
          BoldHandle = behAssociationEnd
          BoldListHandle = blhAllAggregationKind
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'aggregation'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 0
          TabOrder = 4
        end
        object cmbAssoEndVisibility: TBoldComboBox
          Left = 120
          Top = 152
          Width = 291
          Height = 21
          Hint = 
            '|The visibility scope into which this role will be code generate' +
            'd.'
          HelpContext = 1250
          Alignment = taLeftJustify
          BoldHandle = behAssociationEnd
          BoldListHandle = blhAllVisibilityKind
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'visibility'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 0
          TabOrder = 5
        end
        object cmbAssoEndChangeability: TBoldComboBox
          Left = 120
          Top = 176
          Width = 291
          Height = 21
          Hint = '|Controls changeability of the role (not enforced).'
          HelpContext = 1310
          Alignment = taLeftJustify
          BoldHandle = behAssociationEnd
          BoldListHandle = blhAllChangeabilityKind
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'changeability'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 0
          TabOrder = 6
        end
        object tbxAssociationEndConstraint: TBoldEdit
          Left = 120
          Top = 248
          Width = 275
          Height = 21
          Hint = '|Number of constraints defined for this role.'
          HelpContext = 14
          TabStop = False
          BoldHandle = behAssociationEnd
          BoldProperties.Expression = 'constraint'
          BoldProperties.Renderer = bsrNiceCRRenderer
          ReadOnly = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 10
          Visible = False
        end
        object btAssociationEndConstraintEditor: TButton
          Left = 394
          Top = 248
          Width = 17
          Height = 21
          Hint = '|Edit the constraints for this role.'
          HelpContext = 24
          Anchors = [akTop, akRight]
          Caption = #188
          Font.Charset = SYMBOL_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Symbol'
          Font.Style = []
          ParentFont = False
          TabOrder = 11
          Visible = False
          OnClick = btModelConstraintEditorClick
        end
        object cmbMultiplicity: TBoldComboBox
          Left = 120
          Top = 104
          Width = 291
          Height = 21
          Hint = 
            '|Multiplicity of the role - how many things can be on the other ' +
            'side.'
          HelpContext = 1280
          Alignment = taLeftJustify
          BoldHandle = behAssociationEnd
          BoldListHandle = bchMultiplicityValues
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'multiplicity'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 3
        end
        object cmbAssoEndDeleteAction: TBoldComboBox
          Left = 120
          Top = 200
          Width = 291
          Height = 21
          Hint = '|Controls how deletion propagates over the role.'
          HelpContext = 1300
          Alignment = taLeftJustify
          BoldHandle = behAssociationEnd
          BoldListHandle = bchDeleteActions
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'taggedValue['#39'Bold.DeleteAction'#39'].value'
          BoldProperties.ApplyPolicy = bapChange
          BoldSelectChangeAction = bdscSetText
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 0
          TabOrder = 7
        end
        object tbxAssoEndDerivationOCL: TBoldEdit
          Left = 120
          Top = 224
          Width = 275
          Height = 21
          Hint = '|Derivation OCL expression for this role.'
          HelpContext = 1180
          BoldHandle = behAssociationEnd
          BoldProperties.Expression = 'taggedValue['#39'Bold.DerivationOCL'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 8
          Visible = False
        end
        object btAssoEndShowDeriExprEditor: TButton
          Left = 394
          Top = 224
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
          TabOrder = 9
          Visible = False
          OnClick = btAssoEndShowDeriExprEditorClick
        end
      end
      object pcAssociationEndTabs: TPageControl
        Left = 0
        Top = 355
        Width = 607
        Height = 157
        ActivePage = TabSheet7
        Align = alClient
        TabOrder = 1
        object TabSheet7: TTabSheet
          HelpContext = 50
          Caption = '&Qualifiers'
          object BoldGrid7: TBoldGrid
            Left = 0
            Top = 0
            Width = 599
            Height = 129
            HelpContext = 50
            AddNewAtEnd = False
            Align = alClient
            BoldAutoColumns = False
            BoldShowConstraints = False
            BoldHandle = blhAssociationEndQualifiers
            BoldProperties.DefaultDblClick = False
            BoldProperties.InternalDrag = False
            BoldProperties.NilElementMode = neNone
            Columns = <
              item
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
              end
              item
                BoldProperties.Expression = 'name'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Name'
              end
              item
                BoldProperties.Expression = 'typeName'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Type'
              end
              item
                BoldProperties.Expression = 'stereotypeName'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Stereotype'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.Length'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Length'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Delphi name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Expression name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.PMapperName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'PMapper name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.DBName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Column name'
              end>
            DefaultRowHeight = 17
            EnableColAdjust = False
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            ColWidths = (
              17
              64
              64
              64
              64
              64
              64
              64
              64)
          end
        end
      end
    end
    object tabParameter: TTabSheet
      HelpContext = 36
      Caption = 'Parameter'
      DesignSize = (
        607
        512)
      object lblParameterName: TLabel
        Left = 84
        Top = 36
        Width = 28
        Height = 13
        Alignment = taRightJustify
        Caption = '&Name'
        FocusControl = tbxParameterName
      end
      object lblParameterType: TLabel
        Left = 88
        Top = 108
        Width = 24
        Height = 13
        Alignment = taRightJustify
        Caption = 'T&ype'
        FocusControl = tbxParameterType
      end
      object lblParameterKind: TLabel
        Left = 91
        Top = 84
        Width = 21
        Height = 13
        Alignment = taRightJustify
        Caption = '&Kind'
        FocusControl = cmbParamKind
      end
      object lblParameterExpressionName: TLabel
        Left = 30
        Top = 156
        Width = 82
        Height = 13
        Alignment = taRightJustify
        Caption = 'E&xpression Name'
        FocusControl = tbxParameterExpressionName
        Visible = False
      end
      object blbParameterInfo: TBoldLabel
        Left = 120
        Top = 4
        Width = 48
        Height = 24
        BoldHandle = behParameter
        BoldProperties.Expression = 'name'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label9: TLabel
        Left = 27
        Top = 4
        Width = 91
        Height = 24
        Caption = 'Parameter:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lblParameterStereotype: TLabel
        Left = 61
        Top = 60
        Width = 51
        Height = 13
        Alignment = taRightJustify
        Caption = '&Stereotype'
        FocusControl = tbxParameterStereotype
      end
      object lblParameterConstraint: TLabel
        Left = 60
        Top = 132
        Width = 52
        Height = 13
        Alignment = taRightJustify
        Caption = '&Constraints'
        FocusControl = tbxParameterConstraint
        Visible = False
      end
      object tbxParameterName: TBoldEdit
        Left = 120
        Top = 32
        Width = 262
        Height = 21
        Hint = '|Modeled name of the parameter.'
        HelpContext = 41
        BoldHandle = behParameter
        BoldProperties.Expression = 'name'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 0
      end
      object tbxParameterType: TBoldEdit
        Left = 120
        Top = 104
        Width = 262
        Height = 21
        Hint = '|Data type of the parameter.'
        HelpContext = 3
        BoldHandle = behParameter
        BoldProperties.Expression = 'typeName'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 3
      end
      object tbxParameterExpressionName: TBoldEdit
        Left = 120
        Top = 152
        Width = 262
        Height = 21
        Hint = 
          '|Template for the name used to access this parameter in OCL. (No' +
          't used!)'
        HelpContext = 37
        BoldHandle = behParameter
        BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 6
        Visible = False
      end
      object bcbIsConst: TBoldCheckBox
        Left = 389
        Top = 30
        Width = 97
        Height = 17
        Hint = '|Determines if the param should be const declared.'
        HelpContext = 38
        Anchors = [akTop, akRight]
        BoldHandle = behParameter
        BoldProperties.Expression = 'taggedValue['#39'Bold.IsConst'#39'].value'
        BoldProperties.ApplyPolicy = bapChange
        BoldProperties.Renderer = bcrBooleanToCheckBox
        Caption = 'is c&onst'
        ReadOnly = False
        TabOrder = 7
      end
      object cmbParamKind: TBoldComboBox
        Left = 120
        Top = 80
        Width = 262
        Height = 21
        Hint = 
          '|Controls the kind of parameter. Return makes the operation a fu' +
          'nction.'
        HelpContext = 39
        Alignment = taLeftJustify
        BoldHandle = behParameter
        BoldListHandle = blhAllParameterKind
        BoldListProperties.DragMode = bdgSelection
        BoldListProperties.DropMode = bdpAppend
        BoldListProperties.NilElementMode = neNone
        BoldProperties.Expression = 'kind'
        BoldProperties.ApplyPolicy = bapChange
        BoldSelectChangeAction = bdscSetText
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 0
        TabOrder = 2
      end
      object tbxParameterStereotype: TBoldEdit
        Left = 120
        Top = 56
        Width = 262
        Height = 21
        Hint = '|Stereotype applied to the parameter.'
        HelpContext = 1150
        BoldHandle = behParameter
        BoldProperties.Expression = 'stereotypeName'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 1
      end
      object tbxParameterConstraint: TBoldEdit
        Left = 120
        Top = 128
        Width = 246
        Height = 21
        Hint = '|Number of constraints on the parameter.'
        HelpContext = 14
        TabStop = False
        BoldHandle = behParameter
        BoldProperties.Expression = 'constraint'
        BoldProperties.Renderer = bsrNiceCRRenderer
        ReadOnly = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 4
        Visible = False
      end
      object btParameterConstraintEditor: TButton
        Left = 365
        Top = 128
        Width = 17
        Height = 21
        Hint = '|Edit the constraints of the parameter.'
        HelpContext = 24
        Anchors = [akTop, akRight]
        Caption = #188
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Symbol'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        Visible = False
        OnClick = btModelConstraintEditorClick
      end
    end
    object tabPackage: TTabSheet
      Caption = 'Package'
      ImageIndex = 7
      object Splitter7: TSplitter
        Left = 0
        Top = 104
        Width = 607
        Height = 6
        Cursor = crVSplit
        Align = alTop
        Beveled = True
        OnCanResize = Splitter2CanResize
      end
      object sbPaCKAGE: TScrollBox
        Left = 0
        Top = 0
        Width = 607
        Height = 104
        HelpContext = 9
        Align = alTop
        BorderStyle = bsNone
        TabOrder = 0
        DesignSize = (
          607
          104)
        object lbPackageName: TLabel
          Left = 84
          Top = 36
          Width = 28
          Height = 13
          Alignment = taRightJustify
          Caption = '&Name'
          FocusControl = tbxPackageName
        end
        object lbPackageExpressionname: TLabel
          Left = 32
          Top = 84
          Width = 80
          Height = 13
          Alignment = taRightJustify
          Caption = 'E&xpression name'
          FocusControl = tbxPackageExpressionName
        end
        object blbPackageInfo: TBoldLabel
          Left = 120
          Top = 4
          Width = 245
          Height = 24
          AutoSize = False
          BoldHandle = brhPackage
          BoldProperties.Expression = 'name'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label22: TLabel
          Left = 40
          Top = 4
          Width = 78
          Height = 24
          Caption = 'Package:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -19
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbPackageStereotyp: TLabel
          Left = 61
          Top = 60
          Width = 51
          Height = 13
          Alignment = taRightJustify
          Caption = '&Stereotype'
          FocusControl = tbxStereotypeName
        end
        object tbxPackageName: TBoldEdit
          Left = 120
          Top = 32
          Width = 262
          Height = 21
          HelpContext = 33
          BoldHandle = brhPackage
          BoldProperties.Expression = 'name'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 0
        end
        object tbxPackageExpressionName: TBoldEdit
          Left = 120
          Top = 80
          Width = 262
          Height = 21
          HelpContext = 11
          BoldHandle = brhPackage
          BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 2
        end
        object tbxStereotypeName: TBoldEdit
          Left = 120
          Top = 56
          Width = 262
          Height = 21
          HelpContext = 1150
          BoldHandle = brhPackage
          BoldProperties.Expression = 'stereotypeName'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          Anchors = [akLeft, akTop, akRight]
          MaxLength = 0
          TabOrder = 1
        end
      end
      object pcPackageTabs: TPageControl
        Left = 0
        Top = 110
        Width = 607
        Height = 402
        ActivePage = TabSheet12
        Align = alClient
        TabOrder = 1
        object TabSheet11: TTabSheet
          HelpContext = 87
          Caption = '&Classes'
          object BoldGrid8: TBoldGrid
            Left = 0
            Top = 0
            Width = 599
            Height = 374
            HelpContext = 87
            AddNewAtEnd = False
            Align = alClient
            BoldAutoColumns = False
            BoldShowConstraints = False
            BoldHandle = blhPackageClasses
            BoldProperties.InternalDrag = False
            BoldProperties.NilElementMode = neNone
            Columns = <
              item
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
              end
              item
                BoldProperties.Expression = 'name'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Name'
              end
              item
                BoldProperties.Expression = 'superclass.name'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Superclass'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.FileName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'File name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.TableMapping'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Table mapping'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Delphi name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Expression name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.PMapper'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'PMapper name'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.TableName'#39'].value'#13
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Table name'
              end
              item
                BoldProperties.Expression = 'persistent'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Persistent'
              end
              item
                BoldProperties.Expression = 'isabstract'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Abstract'
              end
              item
                BoldProperties.Expression = 'taggedValue['#39'Bold.Imported'#39'].value'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Imported'
              end>
            DefaultRowHeight = 17
            EnableColAdjust = False
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            ColWidths = (
              17
              100
              70
              75
              64
              64
              64
              64
              64
              64
              64
              64)
          end
        end
        object TabSheet12: TTabSheet
          HelpContext = 32
          Caption = 'A&ssociations'
          object BoldGrid10: TBoldGrid
            Left = 0
            Top = 0
            Width = 599
            Height = 374
            HelpContext = 32
            AddNewAtEnd = False
            Align = alClient
            BoldAutoColumns = False
            BoldShowConstraints = False
            BoldHandle = blhPackageAssociations
            BoldProperties.DefaultDblClick = False
            BoldProperties.InternalDrag = False
            BoldProperties.NilElementMode = neNone
            Columns = <
              item
                Color = clBtnFace
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
              end
              item
                BoldProperties.Expression = 'name'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Name'
              end
              item
                BoldProperties.Expression = 'class'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Class'
              end
              item
                BoldProperties.Expression = 
                  'if connection->size > 0 then '#13'  connection->at(1).name'#13' else '#13'  ' +
                  #39'<None>'#39#13'endif'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Connection 1'
              end
              item
                BoldProperties.Expression = 
                  'if connection->size > 1 then '#13'  connection->at(2).name'#13' else '#13'  ' +
                  #39'<None>'#39#13'endif'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Title.Caption = 'Connection 2'
              end>
            DefaultRowHeight = 17
            EnableColAdjust = False
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'MS Sans Serif'
            TitleFont.Style = []
            ColWidths = (
              17
              64
              64
              64
              64)
            RowHeights = (
              17
              17)
          end
        end
        object TabSheet13: TTabSheet
          HelpContext = 1010
          Caption = 'Packages'
          ImageIndex = 2
          TabVisible = False
        end
      end
    end
    object tabDataType: TTabSheet
      Caption = 'DataType'
      ImageIndex = 8
      DesignSize = (
        607
        512)
      object BoldLabel2: TBoldLabel
        Left = 120
        Top = 4
        Width = 48
        Height = 24
        BoldHandle = brhDataType
        BoldProperties.Expression = 'name'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label21: TLabel
        Left = 33
        Top = 4
        Width = 85
        Height = 24
        Caption = 'DataType:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label23: TLabel
        Left = 84
        Top = 36
        Width = 28
        Height = 13
        Alignment = taRightJustify
        Caption = '&Name'
        FocusControl = BoldEdit5
      end
      object Label24: TLabel
        Left = 61
        Top = 60
        Width = 51
        Height = 13
        Alignment = taRightJustify
        Caption = '&Stereotype'
        FocusControl = BoldEdit6
        Visible = False
      end
      object Label25: TLabel
        Left = 53
        Top = 84
        Width = 59
        Height = 13
        Alignment = taRightJustify
        Caption = '&Delphi name'
        FocusControl = BoldEdit7
        Visible = False
      end
      object Label26: TLabel
        Left = 32
        Top = 108
        Width = 80
        Height = 13
        Alignment = taRightJustify
        Caption = 'E&xpression name'
        FocusControl = BoldEdit8
        Visible = False
      end
      object BoldEdit5: TBoldEdit
        Left = 120
        Top = 32
        Width = 262
        Height = 21
        HelpContext = 77
        BoldHandle = brhDataType
        BoldProperties.Expression = 'name'
        ReadOnly = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 0
      end
      object BoldEdit6: TBoldEdit
        Left = 120
        Top = 56
        Width = 262
        Height = 21
        HelpContext = 1150
        BoldHandle = brhDataType
        BoldProperties.Expression = 'stereotypeName'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 1
        Visible = False
      end
      object BoldEdit7: TBoldEdit
        Left = 120
        Top = 80
        Width = 262
        Height = 21
        HelpContext = 6
        BoldHandle = brhDataType
        BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 2
        Visible = False
      end
      object BoldEdit8: TBoldEdit
        Left = 120
        Top = 104
        Width = 262
        Height = 21
        HelpContext = 65
        BoldHandle = brhDataType
        BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 3
        Visible = False
      end
    end
    object tabQualifier: TTabSheet
      Caption = 'Qualifier'
      ImageIndex = 9
      DesignSize = (
        607
        512)
      object Label27: TLabel
        Left = 44
        Top = 4
        Width = 74
        Height = 24
        Caption = 'Qualifier:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object BoldLabel1: TBoldLabel
        Left = 120
        Top = 4
        Width = 121
        Height = 24
        BoldHandle = behAttribute
        BoldProperties.Expression = 'qualifiedName'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label28: TLabel
        Left = 84
        Top = 36
        Width = 28
        Height = 13
        Alignment = taRightJustify
        Caption = '&Name'
        FocusControl = tbxAttributeName
      end
      object Label29: TLabel
        Left = 61
        Top = 60
        Width = 51
        Height = 13
        Alignment = taRightJustify
        Caption = '&Stereotype'
        FocusControl = tbxAttributeStereotype
      end
      object Label30: TLabel
        Left = 53
        Top = 108
        Width = 59
        Height = 13
        Alignment = taRightJustify
        Caption = '&Delphi name'
        FocusControl = tbxAttributeDelphiName
      end
      object Label31: TLabel
        Left = 32
        Top = 132
        Width = 80
        Height = 13
        Alignment = taRightJustify
        Caption = 'E&xpression name'
        FocusControl = tbxAttributeExpressionName
      end
      object Label33: TLabel
        Left = 88
        Top = 84
        Width = 24
        Height = 13
        Alignment = taRightJustify
        Caption = '&Type'
        FocusControl = cmbAttributeBoldType
      end
      object BoldEdit1: TBoldEdit
        Left = 120
        Top = 32
        Width = 262
        Height = 21
        Hint = '|Modeled name of the qualifier.'
        HelpContext = 77
        BoldHandle = behAttribute
        BoldProperties.Expression = 'name'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 0
      end
      object BoldEdit3: TBoldEdit
        Left = 120
        Top = 56
        Width = 262
        Height = 21
        Hint = '|Stereotype applied to the qualifier.'
        HelpContext = 1150
        BoldHandle = behAttribute
        BoldProperties.Expression = 'stereotypeName'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 1
      end
      object BoldComboBox1: TBoldComboBox
        Left = 120
        Top = 80
        Width = 262
        Height = 21
        Hint = '|The type of the qualifier.'
        HelpContext = 71
        Alignment = taLeftJustify
        BoldHandle = behAttribute
        BoldListHandle = blhAllDataTypes
        BoldListProperties.DragMode = bdgSelection
        BoldListProperties.DropMode = bdpAppend
        BoldListProperties.NilElementMode = neNone
        BoldProperties.Expression = 'type.Name'
        BoldProperties.ApplyPolicy = bapChange
        BoldRowProperties.Expression = 'name'
        BoldSetValueExpression = 'type'
        BoldSelectChangeAction = bdcsSetValue
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 0
        Sorted = True
        TabOrder = 2
      end
      object BoldEdit4: TBoldEdit
        Left = 120
        Top = 104
        Width = 262
        Height = 21
        Hint = 
          '|Template for the name used to access the qualifier in source co' +
          'de.'
        HelpContext = 6
        BoldHandle = behAttribute
        BoldProperties.Expression = 'taggedValue['#39'Bold.DelphiName'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 3
      end
      object BoldEdit9: TBoldEdit
        Left = 120
        Top = 128
        Width = 262
        Height = 21
        Hint = '|Template for the name used to access the qualifier in OCL.'
        HelpContext = 65
        BoldHandle = behAttribute
        BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
        ReadOnly = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Alignment = taLeftJustify
        ButtonStyle = bbsNone
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 0
        TabOrder = 4
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 569
    Width = 821
    Height = 19
    Hint = 'Doubleclick here to validate model'
    Panels = <
      item
        Width = 30
      end
      item
        Width = 50
      end>
    OnDblClick = mnuConsistencycheckClick
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 821
    Height = 27
    AutoSize = True
    ButtonHeight = 23
    ButtonWidth = 24
    Caption = 'ToolBar1'
    EdgeBorders = [ebTop, ebBottom]
    Flat = True
    Images = ilMenu
    TabOrder = 3
    object Separator1: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'Separator1'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object Separator2: TToolButton
      Left = 8
      Top = 0
      Width = 8
      Caption = 'Separator2'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object tbnFilterAttributes: TToolButton
      Left = 16
      Top = 0
      AllowAllUp = True
      Caption = '&Attributes'
      Down = True
      ImageIndex = 3
      Style = tbsCheck
      OnClick = tbnFilterAttributesClick
    end
    object tbnFilterRoles: TToolButton
      Left = 40
      Top = 0
      Hint = 'Show Roles'
      Caption = 'tbnFilterRoles'
      Down = True
      ImageIndex = 1
      Style = tbsCheck
      OnClick = tbnFilterRolesClick
    end
    object tbnFilterOperations: TToolButton
      Left = 64
      Top = 0
      Hint = 'Show Operations'
      Caption = 'tbnFilterOperations'
      Down = True
      ImageIndex = 2
      Style = tbsCheck
      OnClick = tbnFilterOperationsClick
    end
    object ToolButton1: TToolButton
      Left = 88
      Top = 0
      Hint = ':-)'
      Caption = 'ToolButton1'
      OnClick = ToolButton1Click
    end
  end
  object behModel: TBoldExpressionHandle
    RootHandle = brhTreeRoot
    Expression = 'self->filterOnType(UMLModel)->first'
    Left = 4
    Top = 64
  end
  object behClass: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLClass)->first'
    Left = 4
    Top = 96
  end
  object behAttribute: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLAttribute)->first'
    Left = 4
    Top = 160
  end
  object behOperation: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLOperation)->first'
    Left = 8
    Top = 200
  end
  object behAssociation: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLAssociation)->first'
    Left = 8
    Top = 236
  end
  object behAssociationEnd: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLAssociationEnd)->first'
    Left = 8
    Top = 272
  end
  object behClassSuperClass: TBoldExpressionHandle
    RootHandle = behClass
    RootTypeName = 'UMLClassifier'
    Expression = 'superclass'
    Left = 36
    Top = 96
  end
  object MainMenu1: TMainMenu
    Images = ilMenu
    Left = 272
    object mnuFile: TMenuItem
      Caption = '&File'
      object Clear1: TMenuItem
        Caption = '&Clear'
        HelpContext = 118
        OnClick = Clear1Click
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object N4: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object OpenFile1: TMenuItem
        Caption = '&Open File'
        GroupIndex = 1
        HelpContext = 8
        ImageIndex = 15
        OnClick = OpenFile1Click
      end
      object SaveFileAs1: TMenuItem
        Caption = 'Save File &As...'
        GroupIndex = 1
        HelpContext = 89
        ImageIndex = 13
        OnClick = SaveFileAs1Click
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
      object Undo1: TMenuItem
        Caption = '&Undo'
        HelpContext = 94
        ImageIndex = 16
        ShortCut = 16474
        Visible = False
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object mnuShowAttributes: TMenuItem
        Caption = '&Attributes'
        Checked = True
        HelpContext = 95
        ImageIndex = 3
        OnClick = mnuShowAttributesClick
      end
      object mnuShowRoles: TMenuItem
        Caption = '&Roles'
        Checked = True
        HelpContext = 96
        ImageIndex = 1
        OnClick = mnuShowRolesClick
      end
      object mnuShowOperations: TMenuItem
        Caption = '&Operations'
        Checked = True
        HelpContext = 97
        ImageIndex = 2
        OnClick = mnuShowOperationsClick
      end
      object mnuBreak1: TMenuItem
        Caption = '-'
      end
      object Loggform1: TMenuItem
        Caption = 'Log form'
        ShortCut = 16460
        OnClick = Loggform1Click
      end
      object mnuBreak2: TMenuItem
        Caption = '-'
      end
      object mnuAdvanced: TMenuItem
        Caption = 'A&dvanced'
        HelpContext = 100
        RadioItem = True
        ShortCut = 16449
        OnClick = mnuAdvancedClick
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
        ShortCut = 49219
        OnClick = NewClass1Click
      end
      object NewAssociation2: TMenuItem
        Caption = 'New &Association'
        HelpContext = 104
        ShortCut = 49230
        OnClick = NewAssociation1Click
      end
      object NewDataType2: TMenuItem
        Caption = 'New DataType'
        ShortCut = 49220
        OnClick = NewDatatype1Click
      end
      object AddSubclass2: TMenuItem
        Caption = 'Add Subclass'
        ShortCut = 49235
        OnClick = AddSubclass1Click
      end
      object InsertSuperclass1: TMenuItem
        Caption = 'Insert Superclass'
        OnClick = InsertSuperclass1Click
      end
      object NewAttribute2: TMenuItem
        Caption = 'New A&ttribute'
        HelpContext = 105
        ShortCut = 49217
        OnClick = NewAttribute1Click
      end
      object NewOperation2: TMenuItem
        Caption = 'New &Operation'
        HelpContext = 106
        ShortCut = 49231
        OnClick = NewOperation1Click
      end
      object NewParameter2: TMenuItem
        Caption = 'New &Parameter'
        HelpContext = 107
        ShortCut = 49232
        OnClick = NewParameter1Click
      end
      object NewQualifier2: TMenuItem
        Caption = 'New &Qualifier'
        HelpContext = 108
        ShortCut = 49233
        OnClick = NewQualifier1Click
      end
    end
    object Tools1: TMenuItem
      Caption = '&Tools'
      OnClick = Tools1Click
      object mnuConsistencycheck: TMenuItem
        Caption = '&Consistency Check'
        HelpContext = 110
        ImageIndex = 11
        OnClick = mnuConsistencycheckClick
      end
      object N3: TMenuItem
        Caption = '&Edit tagged values'
        HelpContext = 1320
        ShortCut = 16468
        OnClick = N3Click
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
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object Contents1: TMenuItem
        Caption = '&Contents'
        HelpContext = 111
        ImageIndex = 9
        OnClick = Contents1Click
      end
    end
  end
  object popTree: TPopupMenu
    OnPopup = popTreePopup
    Left = 36
    Top = 32
    object DeleteXxx1: TMenuItem
      Caption = 'Delete xxx'
      HelpContext = 102
      ShortCut = 46
      OnClick = DeleteXxx1Click
    end
    object N10: TMenuItem
      Caption = '-'
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
  object behParameter: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLParameter)->first'
    Left = 4
    Top = 356
  end
  object brhTreeRoot: TBoldReferenceHandle
    StaticValueTypeName = 'UMLModelElement'
    Left = 4
    Top = 32
  end
  object blhModelClasses: TBoldListHandle
    RootHandle = behModel
    Expression = 'ownedElement->filterOnType(UMLClass)->orderBy(name)'
    Left = 38
    Top = 64
  end
  object blhModelAssociations: TBoldListHandle
    RootHandle = behModel
    Expression = 'ownedElement->filterOnType(UMLAssociation)'
    Left = 72
    Top = 64
  end
  object blhClassAttributes: TBoldListHandle
    RootHandle = behClass
    Expression = 'feature->filterOnType(UMLAttribute)'
    Left = 36
    Top = 128
  end
  object blhClassOperations: TBoldListHandle
    RootHandle = behClass
    Expression = 'feature->select(oclIsTypeOf(UMLOperation))'
    Left = 176
    Top = 100
  end
  object blhParameters: TBoldListHandle
    RootHandle = behOperation
    Expression = 'parameter'
    Left = 40
    Top = 200
  end
  object blhAssociationAssociationEnds: TBoldListHandle
    RootHandle = behAssociation
    Expression = 'connection'
    Left = 43
    Top = 238
  end
  object blhAllClasses: TBoldListHandle
    RootHandle = brhTreeRoot
    Expression = 'UMLClass.allInstances->orderBy(name)'
    Left = 47
    Top = 486
  end
  object ilTreeView: TImageList
    Left = 68
    Top = 32
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
    Left = 304
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
  object blhAssociationEndQualifiers: TBoldListHandle
    RootHandle = behAssociationEnd
    Expression = 'qualifier'
    Left = 83
    Top = 270
  end
  object odOpenModel: TOpenDialog
    Left = 240
    Top = 65534
  end
  object sdSaveModel: TSaveDialog
    DefaultExt = 'bld'
    Left = 208
    Top = 1
  end
  object bsrNiceCRRenderer: TBoldAsStringRenderer
    OnSubscribe = bsrNiceCRRendererSubscribe
    OnGetAsString = bsrNiceCRRendererGetAsString
    Left = 52
    Top = 529
  end
  object blhAllParameterKind: TBoldListHandle
    RootTypeName = 'ParameterDirectionKind'
    Expression = 'ParameterDirectionKind.allInstances'
    Left = 111
    Top = 201
  end
  object blhAllOwnerScope: TBoldListHandle
    RootTypeName = 'ScopeKind'
    Expression = 'ScopeKind.allInstances'
    Left = 75
    Top = 201
  end
  object behAssociationEndType: TBoldExpressionHandle
    RootHandle = behAssociationEnd
    RootTypeName = 'UMLAssociationEnd'
    Expression = 'type'
    Left = 43
    Top = 273
  end
  object bcrGetSet: TBoldAsCheckBoxStateRenderer
    OnMayModify = bcrGetSetMayModify
    OnSubscribe = bcrGetSetSubscribe
    OnGetAsCheckBoxState = bcrGetSetGetAsCheckBoxState
    OnSetAsCheckBoxState = bcrGetSetSetAsCheckBoxState
    Left = 15
    Top = 525
  end
  object blhAllSuperclasses: TBoldListHandle
    RootHandle = behClass
    Expression = 
      'model.allOwnedElement->filterOnType(UMLCLass)->excluding(self)->' +
      'orderby(name)'
    Left = 143
    Top = 97
  end
  object bcrBooleanToCheckBox: TBoldAsCheckBoxStateRenderer
    OnMayModify = bcrBooleanToCheckBoxMayModify
    OnSubscribe = bcrBooleanToCheckBoxSubscribe
    OnGetAsCheckBoxState = bcrBooleanToCheckBoxGetAsCheckBoxState
    OnSetAsCheckBoxState = bcrBooleanToCheckBoxSetAsCheckBoxState
    Left = 87
    Top = 529
  end
  object blhAllAggregationKind: TBoldListHandle
    RootTypeName = 'Collection(AggregationKind)'
    Expression = 'AggregationKind.allinstances'
    Left = 119
    Top = 305
  end
  object blhAllVisibilityKind: TBoldListHandle
    Expression = 'VisibilityKind.allinstances'
    Left = 11
    Top = 485
  end
  object blhAllChangeabilityKind: TBoldListHandle
    Expression = 'ChangeableKind.allinstances'
    Left = 151
    Top = 304
  end
  object brhCurrentElement: TBoldReferenceHandle
    StaticValueTypeName = 'UMLElement'
    Left = 100
    Top = 32
  end
  object bvhTableMapping: TBoldVariableHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    ValueTypeName = 'Collection(String)'
    Left = 71
    Top = 97
  end
  object bcrTableMapping: TBoldCursorHandle
    RootHandle = bvhTableMapping
    RootTypeName = 'Collection(String)'
    AutoFirst = False
    Left = 107
    Top = 97
  end
  object bchAttributeKind: TBoldCursorHandle
    RootHandle = bvhAttributeKind
    RootTypeName = 'Collection(String)'
    AutoFirst = False
    Left = 175
    Top = 165
  end
  object bvhAttributeKind: TBoldVariableHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    ValueTypeName = 'Collection(String)'
    Left = 139
    Top = 161
  end
  object bvhDelphiProperty: TBoldVariableHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    ValueTypeName = 'Collection(String)'
    Left = 71
    Top = 165
  end
  object bchDelphiProperty: TBoldCursorHandle
    RootHandle = bvhDelphiProperty
    RootTypeName = 'Collection(String)'
    AutoFirst = False
    Left = 103
    Top = 165
  end
  object bchDelphiFunctionType: TBoldCursorHandle
    RootHandle = bvhDelphiFunctionType
    RootTypeName = 'Collection(String)'
    AutoFirst = False
    Left = 179
    Top = 201
  end
  object bvhDelphiFunctionType: TBoldVariableHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    ValueTypeName = 'Collection(String)'
    Left = 147
    Top = 201
  end
  object bchMultiplicityValues: TBoldCursorHandle
    RootHandle = bvhMultiplicityValues
    RootTypeName = 'Collection(String)'
    AutoFirst = False
    Left = 155
    Top = 269
  end
  object bvhMultiplicityValues: TBoldVariableHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    ValueTypeName = 'Collection(String)'
    InitialValues.Strings = (
      '0..1'
      '1..1'
      '0..*'
      '1..*')
    Left = 119
    Top = 269
  end
  object bchDeleteActions: TBoldCursorHandle
    RootHandle = bvhDeleteActions
    RootTypeName = 'Collection(String)'
    AutoFirst = False
    Left = 83
    Top = 305
  end
  object bvhDeleteActions: TBoldVariableHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    ValueTypeName = 'Collection(String)'
    Left = 43
    Top = 305
  end
  object blhClassAssociationEnds: TBoldListHandle
    RootHandle = behClass
    Expression = 'associationEnd.otherEnd'
    Left = 76
    Top = 128
  end
  object bvhOptimisticLocking: TBoldVariableHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    ValueTypeName = 'Collection(String)'
    InitialValues.Strings = (
      '<Default>'
      'Off'
      'ModifiedMembers'
      'AllMembers'
      'TimeStamp')
    Left = 105
    Top = 64
  end
  object bchOptimisticLocking: TBoldCursorHandle
    RootHandle = bvhOptimisticLocking
    AutoFirst = False
    Left = 139
    Top = 64
  end
  object behHighestSeverity: TBoldExpressionHandle
    RootHandle = behModel
    Expression = 'validator.highestSeverity'
    Left = 107
    Top = 437
  end
  object brhPackage: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLPackage)->first'
    Left = 8
    Top = 396
  end
  object blhPackageAssociations: TBoldListHandle
    RootHandle = brhPackage
    Expression = 'ownedElement->filterOnType(UMLAssociation)'
    Left = 83
    Top = 390
  end
  object blhPackageClasses: TBoldListHandle
    RootHandle = brhPackage
    Expression = 'ownedElement->filterOnType(UMLClass)'
    Left = 43
    Top = 394
  end
  object blhAllDataTypes: TBoldListHandle
    RootHandle = behModel
    Expression = 'allOwnedElement->filterOnType(UMLDataType)->orderby(name)'
    Left = 40
    Top = 160
  end
  object brhDataType: TBoldExpressionHandle
    RootHandle = brhCurrentElement
    Expression = 'self->filterOnType(UMLDataType)->first'
    Left = 8
    Top = 436
  end
  object brhCopyCut: TBoldReferenceHandle
    StaticValueTypeName = 'UMLModelElement'
    Left = 136
    Top = 32
  end
  object behBoldified: TBoldExpressionHandle
    RootHandle = behModel
    RootTypeName = 'Boolean'
    Expression = 'taggedValue['#39'_Boldify.boldified'#39'].Value='#39'True'#39
    Left = 176
    Top = 32
  end
  object bcrAutoCreated: TBoldAsCheckBoxStateRenderer
    OnGetAsCheckBoxState = bcrAutoCreatedGetAsCheckBoxState
    OnSetAsCheckBoxState = bcrAutoCreatedSetAsCheckBoxState
    Left = 108
    Top = 128
  end
  object bsrRedOnAutocreated: TBoldAsStringRenderer
    OnSetColor = bsrRedOnAutocreatedSetColor
    Left = 96
    Top = 484
  end
  object behClassIsRootClass: TBoldExpressionHandle
    RootHandle = behClass
    Expression = 
      '(model.taggedValue['#39'_Boldify.boldified'#39'].Value='#39'True'#39') and (gene' +
      'ralization->isEmpty)'
    Left = 140
    Top = 128
  end
  object bphClassIsRootClass: TBoldPropertiesController
    BoldHandle = behClassIsRootClass
    BoldProperties.Expression = 'not self'
    DrivenProperties = <
      item
        VCLComponent = cmbSuperClass
        PropertyName = 'Visible'
      end>
    Left = 172
    Top = 132
  end
  object BoldPropertiesController1: TBoldPropertiesController
    BoldHandle = behBoldified
    BoldProperties.Expression = 'if self then '#39'neNone'#39'else '#39'neInsertFirst'#39' endif'
    DrivenProperties = <
      item
        VCLComponent = cmbSuperClass
        PropertyName = 'BoldListProperties.NilElementMode'
      end>
    Left = 219
    Top = 93
  end
  object bdhAttributePMapperNames: TBoldDerivedHandle
    RootHandle = behAttribute
    RootTypeName = 'Collection(String)'
    OnDeriveAndSubscribe = bdhAttributePMapperNamesDeriveAndSubscribe
    ValueTypeName = 'Collection(String)'
    Left = 212
    Top = 148
  end
  object bchAttributePMapperNames: TBoldCursorHandle
    RootHandle = bdhAttributePMapperNames
    RootTypeName = 'Collection(String)'
    Left = 163
    Top = 361
  end
  object bvhAdvancedMode: TBoldVariableHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    ValueTypeName = 'Boolean'
    Left = 348
    Top = 2
  end
  object bpcAdvancedView: TBoldPropertiesController
    BoldHandle = bvhAdvancedMode
    DrivenProperties = <
      item
        VCLComponent = lblModelPMapperName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblModelConstraints
        PropertyName = 'Visible'
      end
      item
        VCLComponent = btModelConstraintEditor
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblModelGUID
        PropertyName = 'Visible'
      end
      item
        VCLComponent = edModelGUID
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblModelTypeLibVersion
        PropertyName = 'Visible'
      end
      item
        VCLComponent = edModelTypeLibVersion
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblClassTableName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblClassDelphiName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblClassExpressionName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblClassPMapperName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblClassDerivationExpressions
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblClassConstraint
        PropertyName = 'Visible'
      end
      item
        VCLComponent = btShowDerivationExpressionsEditor
        PropertyName = 'Visible'
      end
      item
        VCLComponent = btClassConstraintEditor
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblClassGUID
        PropertyName = 'Visible'
      end
      item
        VCLComponent = edClassGUID
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblAttributeDelphiName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxAttributeDelphiName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblAttributeExpressionName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxAttributeExpressionName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblAttributePMapperName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = cmbAttributePMapperName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblAttributeColumnName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxAttributeColumnName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblAttributeInitialValue
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxAttributeInitialValue
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblAttributeConstraint
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxAttributeConstraint
        PropertyName = 'Visible'
      end
      item
        VCLComponent = btAttributeConstraintEditor
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblAttributeKind
        PropertyName = 'Visible'
      end
      item
        VCLComponent = cmbTVAttributeKind
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblOperationDelphiName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxOperationDelphiName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblOperationExpressionName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxOperationExpressionName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblOperationConstraint
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxOperationConstraint
        PropertyName = 'Visible'
      end
      item
        VCLComponent = btOperationConstraintEditor
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblAssociationEndDelphiName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxAssociationEndDelphiName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblAssociationEndExpressionName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxAssociationEndExpressionName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblAssociationEndColumnName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxAssociationEndColumnName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxAssociationEndConstraint
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblAssociationEndConstraint
        PropertyName = 'Visible'
      end
      item
        VCLComponent = btAssociationEndConstraintEditor
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblParameterExpressionName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblAssociationConstraint
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxAssociationConstraint
        PropertyName = 'Visible'
      end
      item
        VCLComponent = btAssociationConstraintEditor
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxParameterExpressionName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblParameterConstraint
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxParameterConstraint
        PropertyName = 'Visible'
      end
      item
        VCLComponent = btParameterConstraintEditor
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxClassDelphiName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxClassExpressionName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = cmbClassPMapperName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxClassTableName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxClassDerivationExpressions
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxClassConstraint
        PropertyName = 'Visible'
      end
      item
        VCLComponent = cmbClassOptimisticLocking
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblClassOptimisticLocking
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxModelPMapperName
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxModelConstraints
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblModelOptimisticLocking
        PropertyName = 'Visible'
      end
      item
        VCLComponent = cmbModelOptimisticLocking
        PropertyName = 'Visible'
      end>
    Left = 388
    Top = 2
  end
  object bpcAttributeDerivedVisibility: TBoldPropertiesController
    BoldHandle = behAttribute
    BoldProperties.Expression = 'derived'
    DrivenProperties = <
      item
        VCLComponent = tbxAttributeDerivationOCL
        PropertyName = 'Visible'
      end
      item
        VCLComponent = btAttributeShowDerivExprEditor
        PropertyName = 'Visible'
      end
      item
        VCLComponent = lblAttributeDerivationOCL
        PropertyName = 'Visible'
      end>
    Left = 674
    Top = 173
  end
  object bpcAttributeReverseDerivedEnabled: TBoldPropertiesController
    BoldHandle = behAttribute
    BoldProperties.Expression = 'derived or (taggedValue['#39'Bold.ReverseDerive'#39'].value = '#39'True'#39')'
    DrivenProperties = <
      item
        VCLComponent = bcbAttributeReverseDerive
        PropertyName = 'Enabled'
      end>
    Left = 674
    Top = 221
  end
  object bpcAttributeDelayedFetchEnabled: TBoldPropertiesController
    BoldHandle = behAttribute
    BoldProperties.Expression = 'persistent or (taggedValue['#39'Bold.DelayedFetch'#39'].value = '#39'True'#39')'
    DrivenProperties = <
      item
        VCLComponent = bcbAttributeDelayedFetch
        PropertyName = 'Enabled'
      end>
    Left = 678
    Top = 277
  end
  object bpcAssociationEndDerivationVisible: TBoldPropertiesController
    BoldHandle = behAssociationEnd
    BoldProperties.Expression = 'association.derived'
    DrivenProperties = <
      item
        VCLComponent = lblAssoEndDerivationOCL
        PropertyName = 'Visible'
      end
      item
        VCLComponent = tbxAssoEndDerivationOCL
        PropertyName = 'Visible'
      end
      item
        VCLComponent = btAssoEndShowDeriExprEditor
        PropertyName = 'Visible'
      end>
    Left = 682
    Top = 337
  end
  object apeHintCatcher: TApplicationEvents
    OnHint = apeHintCatcherHint
    Left = 746
    Top = 133
  end
end
