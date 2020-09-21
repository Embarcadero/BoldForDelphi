object Form1: TForm1
  Left = 3
  Top = 4
  Width = 641
  Height = 510
  Caption = 'TreeView Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblMountainbike: TLabel
    Left = 12
    Top = 2
    Width = 72
    Height = 13
    Caption = 'Mountainbikes:'
  end
  object lblFrame: TLabel
    Left = 174
    Top = 2
    Width = 32
    Height = 13
    Caption = 'Frame:'
  end
  object lblComponents: TLabel
    Left = 174
    Top = 42
    Width = 62
    Height = 13
    Caption = 'Components:'
  end
  object BoldGrid1: TBoldGrid
    Left = 8
    Top = 18
    Width = 153
    Height = 110
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhMTB
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
      131)
  end
  object BoldNavigator1: TBoldNavigator
    Left = 55
    Top = 132
    Width = 60
    Height = 25
    BoldHandle = blhMTB
    TabOrder = 1
    VisibleButtons = [nbInsert, nbDelete]
    ImageIndices.nbFirst = -1
    ImageIndices.nbPrior = -1
    ImageIndices.nbNext = -1
    ImageIndices.nbLast = -1
    ImageIndices.nbInsert = -1
    ImageIndices.nbDelete = -1
    ImageIndices.nbMoveUp = -1
    ImageIndices.nbMoveDown = -1
    DeleteQuestion = 'Delete object?'
    UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
    RemoveQuestion = 'Remove "%1:s" from the list?'
  end
  object BoldListBox1: TBoldListBox
    Left = 174
    Top = 58
    Width = 145
    Height = 111
    Alignment = taLeftJustify
    BoldHandle = blhComponents
    BoldProperties.InternalDrag = False
    BoldProperties.NilElementMode = neNone
    BoldRowProperties.Expression = 'model'
    DragMode = dmAutomatic
    ItemHeight = 16
    TabOrder = 2
  end
  object grpFrames: TGroupBox
    Left = 7
    Top = 177
    Width = 153
    Height = 145
    Caption = 'Frames'
    TabOrder = 3
    object BoldGrid2: TBoldGrid
      Left = 8
      Top = 17
      Width = 130
      Height = 93
      AddNewAtEnd = False
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = blhAllFrames
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
        108)
    end
    object BoldNavigator2: TBoldNavigator
      Left = 43
      Top = 114
      Width = 62
      Height = 25
      BoldHandle = blhAllFrames
      TabOrder = 1
      VisibleButtons = [nbInsert, nbDelete]
      ImageIndices.nbFirst = -1
      ImageIndices.nbPrior = -1
      ImageIndices.nbNext = -1
      ImageIndices.nbLast = -1
      ImageIndices.nbInsert = -1
      ImageIndices.nbDelete = -1
      ImageIndices.nbMoveUp = -1
      ImageIndices.nbMoveDown = -1
      DeleteQuestion = 'Delete object?'
      UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
      RemoveQuestion = 'Remove "%1:s" from the list?'
    end
  end
  object grpGears: TGroupBox
    Left = 167
    Top = 177
    Width = 153
    Height = 145
    Caption = 'Gears'
    TabOrder = 4
    object BoldGrid4: TBoldGrid
      Left = 8
      Top = 17
      Width = 130
      Height = 93
      AddNewAtEnd = False
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = blhAllGears
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
          BoldProperties.Expression = 'model'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Title.Caption = 'Model'
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
        108)
    end
    object BoldNavigator4: TBoldNavigator
      Left = 43
      Top = 114
      Width = 62
      Height = 25
      BoldHandle = blhAllGears
      TabOrder = 1
      VisibleButtons = [nbInsert, nbDelete]
      ImageIndices.nbFirst = -1
      ImageIndices.nbPrior = -1
      ImageIndices.nbNext = -1
      ImageIndices.nbLast = -1
      ImageIndices.nbInsert = -1
      ImageIndices.nbDelete = -1
      ImageIndices.nbMoveUp = -1
      ImageIndices.nbMoveDown = -1
      DeleteQuestion = 'Delete object?'
      UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
      RemoveQuestion = 'Remove "%1:s" from the list?'
    end
  end
  object grpBrakes: TGroupBox
    Left = 7
    Top = 328
    Width = 153
    Height = 145
    Caption = 'Brakes'
    TabOrder = 5
    object BoldGrid3: TBoldGrid
      Left = 9
      Top = 18
      Width = 130
      Height = 93
      AddNewAtEnd = False
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = blhAllBrakes
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
          BoldProperties.Expression = 'model'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Title.Caption = 'Model'
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
        108)
    end
    object BoldNavigator3: TBoldNavigator
      Left = 43
      Top = 114
      Width = 62
      Height = 25
      BoldHandle = blhAllBrakes
      TabOrder = 1
      VisibleButtons = [nbInsert, nbDelete]
      ImageIndices.nbFirst = -1
      ImageIndices.nbPrior = -1
      ImageIndices.nbNext = -1
      ImageIndices.nbLast = -1
      ImageIndices.nbInsert = -1
      ImageIndices.nbDelete = -1
      ImageIndices.nbMoveUp = -1
      ImageIndices.nbMoveDown = -1
      DeleteQuestion = 'Delete object?'
      UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
      RemoveQuestion = 'Remove "%1:s" from the list?'
    end
  end
  object GroupBox3: TGroupBox
    Left = 167
    Top = 328
    Width = 153
    Height = 145
    Caption = 'Wheels'
    TabOrder = 6
    object BoldGrid5: TBoldGrid
      Left = 8
      Top = 17
      Width = 130
      Height = 93
      AddNewAtEnd = False
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = blhAllWheels
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
          BoldProperties.Expression = 'model'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Title.Caption = 'Model'
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
        109)
    end
    object BoldNavigator5: TBoldNavigator
      Left = 43
      Top = 114
      Width = 62
      Height = 25
      BoldHandle = blhAllWheels
      TabOrder = 1
      VisibleButtons = [nbInsert, nbDelete]
      ImageIndices.nbFirst = -1
      ImageIndices.nbPrior = -1
      ImageIndices.nbNext = -1
      ImageIndices.nbLast = -1
      ImageIndices.nbInsert = -1
      ImageIndices.nbDelete = -1
      ImageIndices.nbMoveUp = -1
      ImageIndices.nbMoveDown = -1
      DeleteQuestion = 'Delete object?'
      UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
      RemoveQuestion = 'Remove "%1:s" from the list?'
    end
  end
  object btnCurrentAsRoot: TButton
    Left = 344
    Top = 411
    Width = 121
    Height = 25
    Caption = 'Set Current as Root'
    TabOrder = 7
    OnClick = btnCurrentAsRootClick
  end
  object Button1: TButton
    Left = 344
    Top = 444
    Width = 121
    Height = 25
    Action = BoldActivateSystemAction1
    TabOrder = 8
  end
  object Button2: TButton
    Left = 472
    Top = 444
    Width = 121
    Height = 25
    Action = BoldIBDatabaseAction1
    TabOrder = 9
  end
  object btnShowAll: TButton
    Left = 472
    Top = 411
    Width = 121
    Height = 25
    Caption = 'Show All'
    Enabled = False
    TabOrder = 10
    OnClick = btnShowAllClick
  end
  object pgcTreeViews: TPageControl
    Left = 328
    Top = 8
    Width = 297
    Height = 393
    ActivePage = tsEnhancedTree
    MultiLine = True
    TabIndex = 1
    TabOrder = 11
    OnChange = pgcTreeViewsChange
    object tsBasicTree: TTabSheet
      Caption = 'Basic Tree'
      object btrvBasic: TBoldTreeView
        Left = 0
        Top = 0
        Width = 289
        Height = 365
        Align = alClient
        BoldHandle = brhTreeRoot
        BoldProperties.Parts = <
          item
            ControllerExpression = 'self.ocltype'
            InterpretAsList = True
          end>
        BoldProperties.NodeDescriptions = <
          item
            Name = 'MTB'
            ContextTypeName = 'MTB'
            HideNodeWithNoChildren = False
            ListController.Parts = <
              item
                ElementExpression = 'builtAround'
                ControllerExpression = 'self.ocltype'
                InterpretAsList = False
              end
              item
                ElementExpression = 'consistsOf'
                ControllerExpression = 'self.ocltype'
                InterpretAsList = True
              end>
            IconController.Expression = '-1'
            TextController.Expression = 'name'
          end
          item
            Name = 'Brake'
            ContextTypeName = 'Brake'
            HideNodeWithNoChildren = False
            ListController.Parts = <
              item
                ElementExpression = 'partOf'
                ControllerExpression = 'self.ocltype'
                InterpretAsList = True
              end>
            IconController.Expression = '-1'
            TextController.Expression = 'model'
          end
          item
            Name = 'BikeFrame'
            ContextTypeName = 'BikeFrame'
            HideNodeWithNoChildren = False
            ListController.Parts = <
              item
                ElementExpression = 'partOf'
                ControllerExpression = 'self.ocltype'
                InterpretAsList = True
              end>
            IconController.Expression = '-1'
            TextController.Expression = 'name'
          end
          item
            Name = 'Gear'
            ContextTypeName = 'Gear'
            HideNodeWithNoChildren = False
            ListController.Parts = <
              item
                ElementExpression = 'partOf'
                ControllerExpression = 'self.ocltype'
                InterpretAsList = True
              end>
            IconController.Expression = '-1'
            TextController.Expression = 'model'
          end
          item
            Name = 'Wheel'
            ContextTypeName = 'Wheel'
            HideNodeWithNoChildren = False
            ListController.Parts = <
              item
                ElementExpression = 'partOf'
                ControllerExpression = 'self.ocltype'
                InterpretAsList = True
              end>
            IconController.Expression = '-1'
            TextController.Expression = 'model'
          end>
        Indent = 19
        SelectedIndexDelta = 0
        SelectInserted = False
        TabOrder = 0
      end
    end
    object tsEnhancedTree: TTabSheet
      Caption = 'Enhanced Tree'
      ImageIndex = 2
      object btrvEnhanced: TBoldTreeView
        Left = 0
        Top = 33
        Width = 289
        Height = 332
        Align = alClient
        BoldHandle = brhTreeRoot
        BoldProperties.DragMode = bdgSelection
        BoldProperties.Parts = <
          item
            ControllerExpression = 'self.ocltype'
            InterpretAsList = True
          end>
        BoldProperties.NodeDescriptions = <
          item
            Name = 'MTB'
            ContextTypeName = 'MTB'
            HideNodeWithNoChildren = False
            ListController.Parts = <
              item
                ControllerExpression = #39'BuiltAroundFrame'#39
                InterpretAsList = False
              end
              item
                ControllerExpression = #39'ConsistsOfBrakes'#39
                InterpretAsList = False
              end
              item
                ControllerExpression = #39'ConsistsOfWheels'#39
                InterpretAsList = False
              end
              item
                ControllerExpression = #39'ConsistsOfGears'#39
                InterpretAsList = False
              end>
            IconController.Expression = '5'
            TextController.Expression = 'name'
          end
          item
            Name = 'Brake'
            ContextTypeName = 'Brake'
            HideNodeWithNoChildren = False
            ListController.Parts = <
              item
                ControllerExpression = #39'IsPartOf'#39
                InterpretAsList = False
              end>
            IconController.Expression = '3'
            TextController.Expression = 'model'
          end
          item
            Name = 'BikeFrame'
            ContextTypeName = 'BikeFrame'
            HideNodeWithNoChildren = False
            ListController.Parts = <
              item
                ControllerExpression = #39'IsPartOf'#39
                InterpretAsList = False
              end>
            IconController.Expression = '0'
            TextController.Expression = 'name'
          end
          item
            Name = 'Gear'
            ContextTypeName = 'Gear'
            HideNodeWithNoChildren = False
            ListController.Parts = <
              item
                ControllerExpression = #39'IsPartOf'#39
                InterpretAsList = False
              end>
            IconController.Expression = '1'
            TextController.Expression = 'model'
          end
          item
            Name = 'Wheel'
            ContextTypeName = 'Wheel'
            HideNodeWithNoChildren = False
            ListController.Parts = <
              item
                ControllerExpression = #39'IsPartOf'#39
                InterpretAsList = False
              end>
            IconController.Expression = '4'
            TextController.Expression = 'model'
          end
          item
            Name = 'ConsistsOfBrakes'
            ContextTypeName = 'MTB'
            HideNodeWithNoChildren = True
            ListController.Parts = <
              item
                ElementExpression = 'consistsOf->select(oclIsTypeOf(Brake)).oclAsType(Brake)'
                ControllerExpression = 'self.ocltype'
                InterpretAsList = True
              end>
            IconController.Expression = '3'
            TextController.Expression = #39'Brakes'#39
          end
          item
            Name = 'ConsistsOfWheels'
            ContextTypeName = 'MTB'
            HideNodeWithNoChildren = True
            ListController.Parts = <
              item
                ElementExpression = 'consistsOf->select(oclIsTypeOf(Wheel)).oclAsType(Wheel)'
                ControllerExpression = 'self.ocltype'
                InterpretAsList = True
              end>
            IconController.Expression = '4'
            TextController.Expression = #39'Wheels'#39
          end
          item
            Name = 'ConsistsOfGears'
            ContextTypeName = 'MTB'
            HideNodeWithNoChildren = True
            ListController.Parts = <
              item
                ElementExpression = 'consistsOf->select(oclIsTypeOf(Gear)).oclAsType(Gear)'
                ControllerExpression = 'self.ocltype'
                InterpretAsList = True
              end>
            IconController.Expression = '1'
            TextController.Expression = #39'Gears'#39
          end
          item
            Name = 'BuiltAroundFrame'
            ContextTypeName = 'MTB'
            HideNodeWithNoChildren = True
            ListController.Parts = <
              item
                ElementExpression = 'builtAround'
                ControllerExpression = 'self.ocltype'
                InterpretAsList = False
              end>
            IconController.Expression = '0'
            TextController.Expression = #39'BikeFrame'#39
          end
          item
            Name = 'IsPartOf'
            HideNodeWithNoChildren = True
            ListController.Parts = <
              item
                ElementExpression = 'partOf'
                ControllerExpression = 'self.ocltype'
                InterpretAsList = True
              end>
            IconController.Expression = '5'
            TextController.Expression = #39'Is Part Of'#39
          end>
        DragMode = dmAutomatic
        Images = imlTreeNodeIcons
        Indent = 19
        SelectedIndexDelta = 0
        SelectInserted = False
        TabOrder = 0
        OnDragDrop = btrvEnhancedDragDrop
        OnDragOver = btrvEnhancedDragOver
        OnStartDrag = btrvEnhancedStartDrag
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 289
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        Caption = ' '
        TabOrder = 1
        object imgUnlink: TImage
          Left = 8
          Top = 0
          Width = 34
          Height = 31
          Hint = 'Drop here to unlink parts from MTB'
          Center = True
          ParentShowHint = False
          Picture.Data = {
            07544269746D6170F6000000424DF60000000000000076000000280000001000
            0000100000000100040000000000800000000000000000000000100000001000
            0000000000000000800000800000008080008000000080008000808000008080
            8000C0C0C0000000FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFF
            FF00DADADADADADADADAADADADADADADADADDAD0DAD0DAD0DADAADAD0DA0AD0D
            ADADDADADADADADADADAAD0000AD000000ADD08F8F0AD08F8F0A08F00000AD00
            08F00F0ADADADADAD08008F000AD000008F0D08F8F0AD08F8F0AAD000000AD00
            00ADDADADADADADADADAADADA0AD0DA0ADADDADA0ADA0ADA0ADAADADADADADAD
            ADAD}
          ShowHint = True
          Transparent = True
          OnDragDrop = imgUnlinkDragDrop
          OnDragOver = imgUnlinkDragOver
        end
      end
    end
  end
  object BoldEdit1: TBoldEdit
    Left = 174
    Top = 18
    Width = 145
    Height = 21
    BoldHandle = behFrame
    BoldProperties.DropMode = bdpReplace
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
    TabOrder = 12
  end
  object blhMTB: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'MTB.allInstances'
    Left = 48
    Top = 72
  end
  object blhComponents: TBoldListHandle
    RootHandle = blhMTB
    Expression = 'consistsOf'
    Left = 208
    Top = 80
  end
  object behFrame: TBoldExpressionHandle
    RootHandle = blhMTB
    Expression = 'builtAround'
    Left = 216
    Top = 24
  end
  object brhTreeRoot: TBoldReferenceHandle
    StaticSystemHandle = BoldSystemHandle1
    Left = 368
    Top = 72
  end
  object imlTreeNodeIcons: TImageList
    Left = 372
    Top = 128
    Bitmap = {
      494C010106000A00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000948C8400635A4A00948C8400635A4A00948C8400635A4A000000
      000000000000000000000000000000000000000000000000000084848C001010
      08000018000084848C00D6D6D600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000948C
      8400635A4A0000000000ADB5B5000000000000000000847B6B0000000000948C
      8400635A4A000000000000000000000000000000000084848C00293129000000
      0000000000000000000008180800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000635A4A000000
      00000000000000000000847B6B000000000000000000ADB5B500000000000000
      000000000000948C8400000000000000000000000000949C9C00000000000000
      0000000000008C8C84000000000084848C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000948C8400ADB5B5000000
      00000000000000000000ADB5B5000000000000000000847B6B00000000000000
      000000000000847B6B00635A4A00000000000000000084848C00000000008484
      8C006B6B6B000000000000000000101800000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000635A4A0000000000847B
      6B00ADB5B5000000000000000000847B6B00ADB5B5000000000000000000847B
      6B00ADB5B50000000000948C84000000000000000000949C9C00D6D6D6000000
      00009CA5AD0000000000DEE7E700000800000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000948C840000000000000000000000
      000000000000847B6B0000000000ADB5B500847B6B0000000000ADB5B5000000
      0000000000000000000000000000635A4A00000000001818080000000000D6DE
      DE00000000000000000029312900DEDEDE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000635A4A00ADB5B500847B6B00ADB5
      B5000000000000000000ADB5B500847B6B00ADB5B500847B6B00000000000000
      0000847B6B00ADB5B500847B6B00948C84000000000000000000181808008442
      080084848C00A5ADA500D6DEC600949C9C00DEDEDE0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000948C840000000000000000000000
      0000847B6B00ADB5B500847B6B00635A4A00635A4A00ADB5B500847B6B00ADB5
      B500000000000000000000000000635A4A00000000000000000000000000844A
      100000000000000000004A180000000800003142180000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000635A4A0000000000000000000000
      0000ADB5B500847B6B00ADB5B500635A4A00635A4A00847B6B00ADB5B500847B
      6B00000000000000000000000000948C84000000000000000000000000009442
      00007B390000A56B080000000000844208000000000000000000000000001821
      21000018000084848C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000948C8400847B6B00ADB5B500847B
      6B000000000000000000847B6B00847B6B00ADB5B500ADB5B500000000000000
      0000ADB5B500847B6B00ADB5B500635A4A00949C9C0000000000847B7B00A56B
      08000000000000000000000000007B390000000000000000000084848C000000
      0000000000000000000000080000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000635A4A0000000000000000000000
      000000000000ADB5B50000000000ADB5B500847B6B0000000000847B6B000000
      0000000000000000000000000000948C84000000000029212100000000000000
      00007B4A000000000000000000007B3900000000000000080000000000000000
      000000000000000000000000000084848C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000948C840000000000ADB5
      B500847B6B000000000000000000847B6B00ADB5B5000000000000000000ADB5
      B500847B6B0000000000635A4A00000000000000000000000000000000000000
      000000000000A573310000000000A56B08000000000084848C00000000000000
      0000CEC6C6000000000000000000000800000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000635A4A00847B6B000000
      00000000000000000000ADB5B5000000000000000000847B6B00000000000000
      000000000000ADB5B500948C8400000000000000000000000000000000000000
      000000000000A573310000000000A56B080000000000A5ADA50084420800949C
      9C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000948C84000000
      00000000000000000000847B6B000000000000000000ADB5B500000000000000
      000000000000635A4A0000000000000000000000000000000000000000000000
      000000000000000000006B4A0000080800000808000018101000000000000000
      0000DEDEDE00D6D6D600949C9C0084848C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000635A
      4A00948C840000000000ADB5B5000000000000000000847B6B0000000000635A
      4A00948C84000000000000000000000000000000000000000000000000000000
      0000000000000000000031423100000000000000000000000000101818008484
      8C0084848C008C8C8400949C9C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000635A4A00948C8400635A4A00948C8400635A4A00948C84000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000029292100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008C8C8C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005263C600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000424A52000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000187BFF0063636300187B
      FF00217BF700187BFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008C6B63006B525A005A425200000000000000
      000000000000000000000000000000000000000000000000000000000000CEE7
      F700425A8C0000000000294A9400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000018212900ADB5BD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000039424A00000000000000
      00000000000000000000187BFF003184F700187BFF0063636300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008C6B630073525A0084636B008C6B6B00735252000000
      0000000000000000000000000000000000000000000000000000000000000000
      000042529C0000000000B5BDEF00525AA5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000080808006B7B840000000000000000000000000000000000000000000000
      00008C8C8C009CA5AD0000000000000000000000000000000000187BFF000000
      00000000000000000000000000000000000000000000217BF700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008C847B00947373008C6B6B005A394200F7F7F700C6C6D6006B525A000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000042529C0000000000314A63000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003139
      3900C6BDBD000000000000000000000000000000000000000000000000000000
      0000101821009494940000000000000000000000000000000000187BFF000000
      000000000000000000000000000000000000187BFF00187BFF00187BFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000094737B0084636300846363007B5A63000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B84DE00000000009CADE700EFEFFF0000000000000000000000
      000000000000000000000000000000000000000000000000000039DECE00B5B5
      B50031D6BD0039DECE0000000000000000000000000000000000000000000000
      000010101800F7FFFF000000000000000000000000000000000000000000187B
      FF00000000000000000000000000187BFF00639CFF0000000000187BFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B5A630094737300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000314A8C0000000000525A630000000000000000000000
      0000000000000000000000000000000000000000000052B5AD0042D6C600CEBD
      C60042D6BD004A5252001818180000000000000000000818210021CEB50021C6
      AD008C8C8C00C6CECE000000000000000000000000000000000000000000187B
      FF000000000000000000187BFF00639CFF000000000000000000CECECE004A63
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EFDECE006B4A
      4A008C847B000000000000000000000000000000000000000000000000000000
      000000000000000000006B7BD600000000009494E700CEC6F700000000000000
      000000000000000000000000000000000000000000004ACEBD004AE7CE00E7E7
      E7000000000073EFDE0000000000000000000000000018181800212929006BE7
      D600B5B5BD00BDBDB5008CF7E700000000000000000000000000000000000000
      0000187BFF00187BFF00187BFF00187BFF00000000000000000000000000187B
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B5A5A006339
      42007B6B6B000000000000000000000000000000000000000000000000000000
      00000000000000000000D6D6F70029427300F7EFFF0029294A00000000000000
      000000000000000000000000000000000000000000000000000084DED6005263
      63001018210000000000000000000000000000000000000000000000000042A5
      9C00CECECE00A5B5B50042D6C600000000000000000000000000000000000000
      0000639CFF00187BFF0000000000187BFF002973DE000000000000000000187B
      FF00187BFF000000000000000000000000000000000000000000000000006B4A
      52000000000000000000000000000000000052424A008C847B006B4A5A007352
      5A00000000007B5A5A0000000000000000000000000000000000000000000000
      00000000000000000000000000004A52840000000000295AAD00DEE7E7000000
      00000000000000000000000000000000000000000000000000008CEFE7008CF7
      E700000000000000000000000000000000000000000000000000000000008CE7
      D60000000000EFF7FF0000000000000000000000000000000000000000000000
      0000187BFF00000000000000000000000000CECECE00639CFF00000000000000
      0000187BFF000000000000000000000000000000000000000000000000004A31
      4200DECEC600CEB5B50031212100BDB5BD004A29390042293100422939006342
      52005A394200847B6B0000000000000000000000000000000000000000000000
      0000000000000000000000000000C6C6F700080808007B84DE00101010000000
      000000000000000000000000000000000000000000000000000094E7DE0084E7
      D600000000000000000000000000000000000000000000000000000000000000
      0000182929000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000187BFF00639CFF000000
      0000187BFF00187BFF0000000000000000000000000000000000000000000000
      0000B59C9C0031212100311821006B4A5200422931006B4A5A0084636B005239
      420063424A000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D6C6C600EFEFEF00D6C6C6000000
      0000000000000000000000000000000000000000000000000000000000005AAD
      A500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000187BFF000000
      00000000000039424A0000000000000000000000000000000000000000000000
      0000B59C9C00B59C94005A425200C6BDBD00CEC6CE00CEC6C600634A52006B52
      5A0084636B000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004A4A42000000
      000000000000000000000000000000000000000000000000000084EFDE0084EF
      DE000000000000000000000000000000000000000000000000000000000094DE
      D60084E7D6000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000187B
      FF0000000000187BFF0000000000000000000000000000000000000000000000
      0000BDA59C00B59494000000000000000000000000005A394200523139009484
      94008C6B63000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EFEFEF00D6C6
      C6000000000000000000000000000000000000000000000000008CEFDE000000
      00007BE7D60000000000000000000000000000000000000000000000000084CE
      CE0084BDBD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000187BFF00187BFF00187BFF00000000000000000000000000000000000000
      000052394A00F7F7F700FFFFFF00000000000000000000000000846363000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005252
      4A000000000000000000000000000000000000000000000000000000000084EF
      DE000000000094E7DE0084EFDE008CE7DE0094E7E70084EFDE000000000084E7
      D600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006373940000000000000000000000000000000000000000000000
      000000000000736B7B00DED6D600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000EFEF
      EF00D6C6C6000000000000000000000000000000000000000000000000000000
      00007BE7D60084EFDE0063C6BD0084B5B50094E7DE00000000007BDECE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000039424A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005A5A52000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A5EFE7000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000F81FC1FF00000000E5A79DFF00000000
      DDBBBAFF000000009DB9A6FF00000000A66594FF000000007A5EACFF00000000
      0C30C07F00000000700EEC7F00000000700EE2E3000000000C304EDD00000000
      7A5EB6BE00000000A665FAB6000000009DB9FA8E00000000DDBBFC3000000000
      E5A7FDC100000000F81FFDFF00000000BFFFFFFFFDFFF7FF83FFFE3FE5FFF3FF
      BC3FFC1FF4FFF3F3DFBFF01FFAFFE7F3DF1FFE1FFA7FC3F3EE5FFF3FFD7F8183
      ECCFFFC7FD3F8B81F0EFFFC7FC3FC7E1F267EF0BFE9FCFE3F737E003FE1FCFF7
      FF93F007FF1FEFFFFFDBF007FFDFCFE7FFEBF387FFCFD7E7FFF1F1DFFFEFE82F
      FFFBF9FFFFE7F05FFFFBFFFFFFF7FEFF00000000000000000000000000000000
      000000000000}
  end
  object ActionList1: TActionList
    Left = 528
    Top = 136
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = BoldSystemHandle1
      OnSystemOpened = BoldActivateSystemAction1SystemOpened
      OnSystemClosed = BoldActivateSystemAction1SystemClosed
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before exit?'
      SaveOnClose = saAsk
    end
    object BoldIBDatabaseAction1: TBoldIBDatabaseAction
      Category = 'Bold Actions'
      Caption = 'Create DB'
      BoldSystemHandle = BoldSystemHandle1
      Username = 'SYSDBA'
      Password = 'masterkey'
    end
  end
  object BoldModel1: TBoldModel
    UMLModelMode = ummNone
    Boldify.EnforceDefaultUMLCase = False
    Boldify.DefaultNavigableMultiplicity = '0..1'
    Boldify.DefaultNonNavigableMultiplicity = '0..*'
    Left = 520
    Top = 184
    Model = (
      'VERSION 19'
      '(Model'
      #9'"TreeViewExampleClasses"'
      #9'"BusinessClassesRoot"'
      #9'""'
      #9'""'
      
        #9'"_Boldify.boldified=True,_Boldify.RootClass=BusinessClassesRoot' +
        ',_BoldInternal.flattened=True,_BoldInternal.ModelErrors=,Bold.De' +
        'lphiName=<Name>,Bold.UnitName=<Name>,Bold.RootClass=BusinessClas' +
        'sesRoot"'
      #9'(Classes'
      #9#9'(Class'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'"<NONE>"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=Persistent,Bold.TableName=<Prefix>_OBJECT,Bold.V' +
        'ersioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Parts"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Model"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"BikeFrame"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Name"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"FrameSize"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Gear"'
      #9#9#9'"Parts"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"MTB"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Name"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"MTB_Comps"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Brake"'
      #9#9#9'"Parts"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Wheel"'
      #9#9#9'"Parts"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9')'
      #9'(Associations'
      #9#9'(Association'
      #9#9#9'"MTB_Comps"'
      #9#9#9'"MTB_Comps"'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.DelphiName=<Name>"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"ConsistsOf"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"MTB"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..*"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"PartOf"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Parts"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..*"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"MTB_Frame"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.DelphiName=<Name>"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"BuiltAround"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"MTB"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'""'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"PartOf"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"BikeFrame"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..*"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9')'
      ')')
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = False
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldPersistenceHandleDB1
    Left = 520
    Top = 232
  end
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = BoldModel1
    Left = 520
    Top = 280
  end
  object blhAllFrames: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'BikeFrame.allInstances'
    Left = 63
    Top = 233
  end
  object blhAllGears: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Gear.allInstances'
    Left = 223
    Top = 233
  end
  object blhAllBrakes: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Brake.allInstances'
    Left = 63
    Top = 401
  end
  object blhAllWheels: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Wheel.allInstances'
    Left = 223
    Top = 401
  end
  object BoldPersistenceHandleDB1: TBoldPersistenceHandleDB
    BoldModel = BoldModel1
    ClockLogGranularity = '0:0:0.0'
    DatabaseAdapter = BoldDatabaseAdapterIB1
    Left = 464
    Top = 160
  end
  object BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB
    SQLDatabaseConfig.ColumnTypeForDate = 'TIMESTAMP'
    SQLDatabaseConfig.ColumnTypeForTime = 'TIMESTAMP'
    SQLDatabaseConfig.ColumnTypeForDateTime = 'TIMESTAMP'
    SQLDatabaseConfig.ColumnTypeForBlob = 'BLOB'
    SQLDatabaseConfig.ColumnTypeForFloat = 'DOUBLE PRECISION'
    SQLDatabaseConfig.ColumnTypeForCurrency = 'DOUBLE PRECISION'
    SQLDatabaseConfig.ColumnTypeForString = 'VARCHAR(%d)'
    SQLDatabaseConfig.ColumnTypeForInteger = 'INTEGER'
    SQLDatabaseConfig.ColumnTypeForSmallInt = 'SMALLINT'
    SQLDatabaseConfig.DropColumnTemplate = 'ALTER TABLE <TableName> DROP <ColumnName>'
    SQLDatabaseConfig.DropTableTemplate = 'DROP TABLE <TableName>'
    SQLDatabaseConfig.DropIndexTemplate = 'DROP INDEX <IndexName>'
    SQLDatabaseConfig.MaxDbIdentifierLength = 31
    SQLDatabaseConfig.MaxIndexNameLength = 31
    SQLDatabaseConfig.SQLforNotNull = 'NOT NULL'
    SQLDatabaseConfig.QuoteNonStringDefaultValues = False
    SQLDatabaseConfig.SupportsConstraintsInCreateTable = True
    SQLDatabaseConfig.SupportsStringDefaultValues = True
    SQLDatabaseConfig.DBGenerationMode = dbgQuery
    SQLDatabaseConfig.ReservedWords.Strings = (
      'ACTIVE, ADD, ALL, AFTER, ALTER'
      'AND, ANY, AS, ASC, ASCENDING,'
      'AT, AUTO, AUTOINC, AVG, BASE_NAME'
      'BEFORE, BEGIN, BETWEEN, BLOB, BOOLEAN,'
      'BOTH, BY, BYTES, CACHE, CAST, CHAR'
      'CHARACTER, CHECK, CHECK_POINT_LENGTH, COLLATE,'
      'COLUMN, COMMIT, COMMITTED, COMPUTED'
      'CONDITIONAL, CONSTRAINT, CONTAINING, COUNT, CREATE, CSTRING,'
      'CURRENT, CURSOR, DATABASE, DATE, DAY'
      'DEBUG, DEC, DECIMAL, DECLARE, DEFAULT,'
      'DELETE, DESC, DESCENDING, DISTINCT, DO'
      'DOMAIN, DOUBLE, DROP, ELSE, END,'
      'ENTRY_POINT, ESCAPE, EXCEPTION, EXECUTE'
      'EXISTS, EXIT, EXTERNAL, EXTRACT, FILE, FILTER,'
      'FLOAT, FOR, FOREIGN, FROM, FULL, FUNCTION'
      'GDSCODE, GENERATOR, GEN_ID, GRANT,'
      'GROUP, GROUP_COMMIT_WAIT_TIME, HAVING'
      'HOUR, IF, IN, INT, INACTIVE, INDEX, INNER,'
      'INPUT_TYPE, INSERT, INTEGER, INTO'
      'IS, ISOLATION, JOIN, KEY, LONG, LENGTH,'
      'LOGFILE, LOWER, LEADING, LEFT, LEVEL'
      'LIKE, LOG_BUFFER_SIZE, MANUAL, MAX, MAXIMUM_SEGMENT,'
      'MERGE, MESSAGE, MIN, MINUTE, MODULE_NAME'
      'MONEY, MONTH, NAMES, NATIONAL, NATURAL,'
      'NCHAR, NO, NOT, NULL, NUM_LOG_BUFFERS'
      'NUMERIC, OF, ON, ONLY, OPTION,'
      'OR, ORDER, OUTER, OUTPUT_TYPE, OVERFLOW'
      'PAGE_SIZE, PAGE, PAGES, PARAMETER, PASSWORD,'
      'PLAN, POSITION, POST_EVENT, PRECISION'
      
        'PROCEDURE, PROTECTED, PRIMARY, PRIVILEGES, RAW_PARTITIONS, RDB$D' +
        'B_KEY,'
      'READ, REAL, RECORD_VERSION, REFERENCES'
      'RESERV, RESERVING, RETAIN, RETURNING_VALUES, RETURNS, REVOKE,'
      'RIGHT, ROLE, ROLLBACK, SECOND, SEGMENT'
      'SELECT, SET, SHARED, SHADOW, SCHEMA, SINGULAR,'
      'SIZE, SMALLINT, SNAPSHOT, SOME, SORT'
      'SQLCODE, STABILITY, STARTING, STARTS, STATISTICS,'
      'SUB_TYPE, SUBSTRING, SUM, SUSPEND, TABLE'
      'THEN, TIME, TIMESTAMP, TIMEZONE_HOUR, TIMEZONE_MINUTE,'
      'TO, TRAILING, TRANSACTION, TRIGGER, TRIM'
      'UNCOMMITTED, UNION, UNIQUE, UPDATE, UPPER,'
      'USER, VALUE, VALUES, VARCHAR, VARIABLE'
      'VARYING, VIEW, WAIT, WHEN, WHERE,'
      'WHILE, WITH, WORK, WRITE, YEAR')
    SQLDatabaseConfig.StoreEmptyStringsAsNULL = False
    SQLDatabaseConfig.SystemTablePrefix = 'BOLD'
    DataBase = IBDatabase1
    DatabaseEngine = dbeInterbaseSQLDialect3
    Left = 464
    Top = 216
  end
  object IBDatabase1: TIBDatabase
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    IdleTimer = 0
    SQLDialect = 3
    TraceFlags = []
    Left = 464
    Top = 264
  end
end
