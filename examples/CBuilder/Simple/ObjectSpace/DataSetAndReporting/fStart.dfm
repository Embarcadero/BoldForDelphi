object frmStart: TfrmStart
  Left = 483
  Top = 196
  Width = 270
  Height = 72
  Caption = 'frmStart'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnCreateSchema: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Action = dmMain.BoldIBDatabaseAction1
    TabOrder = 0
  end
  object btnOpenSystem: TButton
    Left = 92
    Top = 8
    Width = 75
    Height = 25
    Action = dmMain.BoldActivateSystemAction1
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 184
    Top = 8
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Start'
    ModalResult = 2
    TabOrder = 2
  end
end
