object frmStart: TfrmStart
  Left = 3
  Top = 5
  Width = 270
  Height = 70
  Caption = 'Start'
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
  object btnCancel: TButton
    Left = 184
    Top = 8
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object cmdCreate: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Action = dmMain.BoldIBDatabaseAction1
    TabOrder = 1
  end
  object cmdOpen: TButton
    Left = 100
    Top = 8
    Width = 75
    Height = 25
    Action = dmMain.BoldActivateSystemAction1
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
