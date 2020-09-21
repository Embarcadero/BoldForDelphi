object Form1: TForm1
  Left = -6
  Top = -2
  Width = 522
  Height = 394
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 28
    Height = 13
    Caption = 'Items:'
  end
  object Label2: TLabel
    Left = 248
    Top = 8
    Width = 38
    Height = 13
    Caption = 'Colours:'
  end
  object Label3: TLabel
    Left = 8
    Top = 176
    Width = 34
    Height = 13
    Caption = 'Orders:'
  end
  object Label4: TLabel
    Left = 384
    Top = 48
    Width = 109
    Height = 13
    Caption = 'Server Machine Name:'
  end
  object Label5: TLabel
    Left = 384
    Top = 8
    Width = 56
    Height = 13
    Caption = 'User Name:'
  end
  object BoldGrid1: TBoldGrid
    Left = 8
    Top = 24
    Width = 217
    Height = 120
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhAllItems
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
        BoldProperties.Expression = 'price'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        BoldProperties.Expression = 'colour.name'
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
      64)
  end
  object BoldNavigator1: TBoldNavigator
    Left = 8
    Top = 144
    Width = 56
    Height = 21
    BoldHandle = blhAllItems
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
  object BoldGrid2: TBoldGrid
    Left = 248
    Top = 24
    Width = 89
    Height = 120
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhAllColours
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
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      64)
  end
  object BoldNavigator2: TBoldNavigator
    Left = 248
    Top = 144
    Width = 56
    Height = 21
    BoldHandle = blhAllColours
    TabOrder = 3
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
  object BoldGrid3: TBoldGrid
    Left = 8
    Top = 192
    Width = 89
    Height = 120
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhAllOrders
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
        BoldProperties.Expression = 'orderNo'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'OrderNo'
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      64)
  end
  object BoldGrid4: TBoldGrid
    Left = 104
    Top = 192
    Width = 185
    Height = 120
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhOrderLines
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
        BoldProperties.Expression = 'quantity'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        BoldProperties.Expression = 'item.name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 5
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      64
      97)
  end
  object BoldNavigator3: TBoldNavigator
    Left = 8
    Top = 312
    Width = 56
    Height = 21
    BoldHandle = blhAllOrders
    TabOrder = 6
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
  object BoldNavigator4: TBoldNavigator
    Left = 104
    Top = 312
    Width = 56
    Height = 21
    BoldHandle = blhOrderLines
    TabOrder = 7
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
  object cbPessimisticLocking: TCheckBox
    Left = 384
    Top = 88
    Width = 113
    Height = 17
    Caption = 'PessimisticLocking'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = cbPessimisticLockingClick
  end
  object cbOptimisticLocking: TCheckBox
    Left = 384
    Top = 112
    Width = 113
    Height = 17
    Caption = 'OptimisticLocking'
    Checked = True
    State = cbChecked
    TabOrder = 9
  end
  object cbPropagator: TCheckBox
    Left = 384
    Top = 136
    Width = 113
    Height = 17
    Caption = 'Propagator'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 10
    OnClick = cbPropagatorClick
  end
  object Button4: TButton
    Left = 432
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Start System'
    TabOrder = 11
    OnClick = Button4Click
  end
  object GroupBox1: TGroupBox
    Left = 304
    Top = 200
    Width = 201
    Height = 121
    Caption = 'Selective update and enclosure test'
    TabOrder = 12
    object BoldListBox1: TBoldListBox
      Left = 8
      Top = 16
      Width = 121
      Height = 97
      Hint = 
        'Drop objects here to update selected objects or to test the encl' +
        'osure mechanism (additional objects required to be saved at the ' +
        'same time)'
      Alignment = taLeftJustify
      BoldHandle = BoldCursorHandle1
      BoldProperties.NilElementMode = neNone
      DragMode = dmAutomatic
      ItemHeight = 16
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object Button1: TButton
      Left = 136
      Top = 16
      Width = 57
      Height = 17
      Hint = 'Save the objects in the list to the database'
      Caption = 'Update'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 136
      Top = 40
      Width = 57
      Height = 17
      Hint = 
        'Build the enclosure of objects that has to be updated if the obj' +
        'ects in the list are to be updated'
      Caption = 'Enclosure'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 136
      Top = 64
      Width = 57
      Height = 17
      Hint = 'Clear the list'
      Caption = 'Clear'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = Button3Click
    end
  end
  object edtServerMachineName: TEdit
    Left = 384
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 13
  end
  object edtUserName: TEdit
    Left = 384
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 14
    Text = '<name>'
  end
  object cmdSave: TButton
    Left = 320
    Top = 332
    Width = 75
    Height = 25
    Action = BoldUpdateDBAction1
    TabOrder = 15
  end
  object blhAllItems: TBoldListHandle
    RootHandle = dmMain.bshLocking
    Expression = 'Item.allInstances'
    Left = 16
    Top = 48
  end
  object blhAllColours: TBoldListHandle
    RootHandle = dmMain.bshLocking
    Expression = 'Colour.allInstances'
    Left = 256
    Top = 48
  end
  object blhAllOrders: TBoldListHandle
    RootHandle = dmMain.bshLocking
    Expression = 'PurchaseOrder.allInstances'
    Left = 16
    Top = 224
  end
  object blhOrderLines: TBoldListHandle
    RootHandle = blhAllOrders
    Expression = 'orderLine'
    Left = 120
    Top = 224
  end
  object BoldPlaceableAFP1: TBoldPlaceableAFP
    Left = 168
    Top = 152
  end
  object BoldVariableHandle1: TBoldVariableHandle
    StaticSystemHandle = dmMain.bshLocking
    ValueTypeName = 'Collection(BusinessClassesRoot)'
    Left = 384
    Top = 224
  end
  object BoldCursorHandle1: TBoldCursorHandle
    RootHandle = BoldVariableHandle1
    Left = 352
    Top = 224
  end
  object BoldExceptionHandler1: TBoldExceptionHandler
    OnApplyException = BoldExceptionHandler1ApplyException
    Left = 104
    Top = 152
  end
  object ActionList1: TActionList
    Left = 352
    Top = 136
    object BoldUpdateDBAction1: TBoldUpdateDBAction
      Category = 'Bold Actions'
      Caption = 'Update DB'
      BoldSystemHandle = dmMain.bshLocking
    end
  end
end
