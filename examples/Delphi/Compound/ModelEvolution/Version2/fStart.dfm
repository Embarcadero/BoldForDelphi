object frmStart: TfrmStart
  Left = 1
  Top = 2
  Width = 349
  Height = 72
  BorderIcons = [biSystemMenu]
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
    Left = 256
    Top = 8
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object btnCreateSchema: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Action = dmSystem.BoldIBDatabaseAction1
    TabOrder = 1
  end
  object btnOpenSystem: TButton
    Left = 92
    Top = 8
    Width = 75
    Height = 25
    Action = dmSystem.BoldActivateSystemAction1
    ModalResult = 1
    TabOrder = 2
  end
  object Button1: TButton
    Left = 176
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Evolve DB'
    TabOrder = 3
    OnClick = Button1Click
  end
end
