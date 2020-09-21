object MainForm: TMainForm
  Left = -4
  Top = 2
  Width = 697
  Height = 442
  Caption = 'MainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 39
    Height = 13
    Caption = 'Budgets'
  end
  object Label2: TLabel
    Left = 280
    Top = 8
    Width = 40
    Height = 13
    Caption = 'Columns'
  end
  object Label3: TLabel
    Left = 480
    Top = 8
    Width = 27
    Height = 13
    Caption = 'Rows'
  end
  object Label5: TLabel
    Left = 8
    Top = 192
    Width = 56
    Height = 13
    Caption = 'BudgetCells'
  end
  object BoldLabel1: TBoldLabel
    Left = 96
    Top = 8
    Width = 120
    Height = 13
    BoldHandle = dmMain.BoldSystemHandle1
    BoldProperties.Expression = 'Budget.allInstances->size'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object BoldLabel2: TBoldLabel
    Left = 333
    Top = 8
    Width = 101
    Height = 13
    BoldHandle = dmMain.BoldSystemHandle1
    BoldProperties.Expression = 'Col.allInstances->size'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object BoldLabel3: TBoldLabel
    Left = 552
    Top = 8
    Width = 108
    Height = 13
    BoldHandle = dmMain.BoldSystemHandle1
    BoldProperties.Expression = 'Row.allInstances->size'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object Label4: TLabel
    Left = 406
    Top = 156
    Width = 34
    Height = 13
    Caption = 'Budget'
  end
  object bgrBudget: TBoldGrid
    Left = 8
    Top = 30
    Width = 225
    Height = 99
    AddNewAtEnd = False
    BoldAutoColumns = True
    BoldShowConstraints = False
    BoldHandle = blhAllBudget
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
        BoldProperties.Expression = 'aName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        BoldProperties.Expression = 'aNumber'
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
      64)
  end
  object bgrRow: TBoldGrid
    Left = 473
    Top = 31
    Width = 192
    Height = 98
    AddNewAtEnd = False
    BoldAutoColumns = True
    BoldShowConstraints = False
    BoldHandle = blhAllRow
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
        BoldProperties.Expression = 'aNumber'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        BoldProperties.Expression = 'aName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
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
      64
      64)
  end
  object bgrCol: TBoldGrid
    Left = 272
    Top = 30
    Width = 185
    Height = 99
    AddNewAtEnd = False
    BoldAutoColumns = True
    BoldShowConstraints = False
    BoldHandle = blhAllColumn
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
        BoldProperties.Expression = 'aNumber'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        BoldProperties.Expression = 'aName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      64
      64)
  end
  object BoldGrid1: TBoldGrid
    Left = 8
    Top = 216
    Width = 319
    Height = 153
    AddNewAtEnd = False
    BoldAutoColumns = True
    BoldShowConstraints = False
    BoldHandle = blhBudgetCells
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
        BoldProperties.Expression = 'aName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        BoldProperties.Expression = 'aValue'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        BoldProperties.Expression = 'rownum'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        BoldProperties.Expression = 'colnum'
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
      64
      64
      64)
  end
  object btnCreateCells: TButton
    Left = 16
    Top = 137
    Width = 97
    Height = 25
    Caption = 'Create Ce&lls '
    Enabled = False
    TabOrder = 1
    OnClick = btnCreateCellsClick
  end
  object btnClearCells: TButton
    Left = 136
    Top = 137
    Width = 89
    Height = 25
    Caption = '&Clear Cells'
    TabOrder = 2
    OnClick = btnClearCellsClick
  end
  object Button1: TButton
    Left = 588
    Top = 150
    Width = 75
    Height = 25
    Caption = ' Get XML'
    TabOrder = 7
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 448
    Top = 153
    Width = 126
    Height = 21
    TabOrder = 6
  end
  object Memo1: TMemo
    Left = 408
    Top = 184
    Width = 257
    Height = 185
    TabOrder = 8
  end
  object cmdUpdate: TButton
    Left = 100
    Top = 384
    Width = 75
    Height = 25
    Action = BoldUpdateDBAction1
    TabOrder = 10
  end
  object cmdCreateDB: TButton
    Left = 8
    Top = 384
    Width = 75
    Height = 25
    Action = BoldIBDatabaseAction1
    TabOrder = 9
  end
  object cmdOpen: TButton
    Left = 184
    Top = 384
    Width = 75
    Height = 25
    Action = BoldActivateSystemAction1
    TabOrder = 11
  end
  object ActionList1: TActionList
    Left = 288
    Top = 320
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = dmMain.BoldSystemHandle1
      OnSystemOpened = BoldActivateSystemAction1SystemOpened
      OnSystemClosed = BoldActivateSystemAction1SystemClosed
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before closing system?'
      SaveOnClose = saAsk
    end
    object BoldUpdateDBAction1: TBoldUpdateDBAction
      Category = 'Bold Actions'
      Caption = 'Update DB'
      BoldSystemHandle = dmMain.BoldSystemHandle1
    end
    object BoldIBDatabaseAction1: TBoldIBDatabaseAction
      Category = 'Bold Actions'
      Caption = 'Create DB'
      BoldSystemHandle = dmMain.BoldSystemHandle1
      Username = 'SYSDBA'
      Password = 'masterkey'
    end
  end
  object blhAllBudget: TBoldListHandle
    RootHandle = dmMain.BoldSystemHandle1
    Expression = 'Budget.allInstances'
    Left = 56
    Top = 56
  end
  object blhAllRow: TBoldListHandle
    RootHandle = dmMain.BoldSystemHandle1
    BoldComparer = bcRow
    Expression = 'Row.allInstances'
    Left = 496
    Top = 56
  end
  object blhAllColumn: TBoldListHandle
    RootHandle = dmMain.BoldSystemHandle1
    BoldComparer = bcColumn
    Expression = 'Col.allInstances'
    Left = 232
    Top = 56
  end
  object blhAllCell: TBoldListHandle
    RootHandle = dmMain.BoldSystemHandle1
    Expression = 'ACell.allInstances'
    Left = 544
    Top = 16
  end
  object PopupMenu1: TPopupMenu
    Left = 232
    Top = 316
    object New1: TMenuItem
      Caption = '&New'
      OnClick = New1Click
    end
    object Delete1: TMenuItem
      Caption = '&Delete'
      OnClick = Delete1Click
    end
  end
  object blhBudgetCells: TBoldListHandle
    RootHandle = blhAllBudget
    BoldComparer = bcCells
    Expression = 'aCell->orderby(col)'
    Left = 96
    Top = 208
  end
  object bcCells: TBoldComparer
    OnCompare = bcCellsCompare
    Left = 144
    Top = 192
  end
  object bcColumn: TBoldComparer
    OnCompare = bcColumnCompare
    Left = 312
    Top = 64
  end
  object bcRow: TBoldComparer
    OnCompare = bcRowCompare
    Left = 576
    Top = 64
  end
  object BoldReferenceHandle1: TBoldReferenceHandle
    StaticSystemHandle = dmMain.BoldSystemHandle1
    StaticValueTypeName = 'Collection(String)'
    Left = 400
    Top = 56
  end
end
