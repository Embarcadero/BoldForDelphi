object Form1: TForm1
  Left = 34
  Top = 186
  Width = 384
  Height = 248
  Action = BoldIBDatabaseAction1
  Caption = 'Create DB'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 100
    Height = 13
    Caption = 'Number of Accounts:'
  end
  object BoldLabel1: TBoldLabel
    Left = 129
    Top = 16
    Width = 126
    Height = 13
    BoldHandle = DataModule2.BoldSystemHandle1
    BoldProperties.Expression = 'Account.allInstances->size'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object BoldGrid1: TBoldGrid
    Left = 24
    Top = 49
    Width = 241
    Height = 127
    AddNewAtEnd = False
    BoldAutoColumns = True
    BoldShowConstraints = False
    BoldHandle = DataModule2.blhAccounts
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
        BoldProperties.Expression = 'total'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Total'
      end
      item
        BoldProperties.Expression = 'number'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Number'
      end
      item
        BoldProperties.Expression = 'credit'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Credit'
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
    Left = 24
    Top = 189
    Width = 192
    Height = 25
    BoldHandle = DataModule2.blhAccounts
    TabOrder = 1
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
  object cmdSave: TButton
    Left = 292
    Top = 52
    Width = 75
    Height = 25
    Action = BoldUpdateDBAction1
    TabOrder = 2
  end
  object cmdCreateDB: TButton
    Left = 292
    Top = 12
    Width = 75
    Height = 25
    Action = BoldIBDatabaseAction1
    TabOrder = 3
  end
  object cmdOpenSystem: TButton
    Left = 292
    Top = 84
    Width = 75
    Height = 25
    Action = BoldActivateSystemAction1
    TabOrder = 4
  end
  object ActionList1: TActionList
    Left = 276
    Top = 112
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = DataModule2.BoldSystemHandle1
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before closing system?'
      SaveOnClose = saAsk
    end
    object BoldUpdateDBAction1: TBoldUpdateDBAction
      Category = 'Bold Actions'
      Caption = 'Update DB'
      BoldSystemHandle = DataModule2.BoldSystemHandle1
    end
    object BoldIBDatabaseAction1: TBoldIBDatabaseAction
      Category = 'Bold Actions'
      Caption = 'Create DB'
      BoldSystemHandle = DataModule2.BoldSystemHandle1
      Username = 'SYSDBA'
      Password = 'masterkey'
    end
  end
end
