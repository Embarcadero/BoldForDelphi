object frmServerConsole: TfrmServerConsole
  Left = 1
  Top = 3
  Width = 348
  Height = 209
  Caption = 'Server Console'
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
  object btnShutDown: TButton
    Left = 8
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Shut Down'
    TabOrder = 0
    OnClick = btnShutDownClick
  end
  object BoldListBox1: TBoldListBox
    Left = 104
    Top = 24
    Width = 121
    Height = 97
    Alignment = taLeftJustify
    BoldHandle = BoldListHandle1
    BoldProperties.InternalDrag = False
    BoldProperties.NilElementMode = neNone
    DragMode = dmAutomatic
    ItemHeight = 16
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 48
    Width = 75
    Height = 25
    Action = BoldActivateSystemAction1
    TabOrder = 2
  end
  object Button2: TButton
    Left = 8
    Top = 80
    Width = 75
    Height = 25
    Action = BoldIBDatabaseAction1
    TabOrder = 3
  end
  object BoldListHandle1: TBoldListHandle
    RootHandle = DMSystem.SystemHandle
    Left = 136
    Top = 32
  end
  object BoldPlaceableAFP1: TBoldPlaceableAFP
    Left = 272
    Top = 20
  end
  object ActionList1: TActionList
    Left = 32
    Top = 112
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = DMSystem.SystemHandle
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before exit?'
      SaveOnClose = saAsk
    end
    object BoldIBDatabaseAction1: TBoldIBDatabaseAction
      Category = 'Bold Actions'
      Caption = 'Create DB'
      BoldSystemHandle = DMSystem.SystemHandle
      Username = 'SYSDBA'
      Password = 'masterkey'
    end
  end
end
