object frmStart: TfrmStart
  Left = 404
  Top = 259
  Width = 349
  Height = 72
  Caption = 'Start'
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
    Action = dmSystem.BoldIBAliasAction1
    TabOrder = 0
  end
  object btnOpenSystem: TButton
    Left = 92
    Top = 8
    Width = 75
    Height = 25
    Action = dmSystem.BoldActivateSystemAction1
    ModalResult = 1
    TabOrder = 1
  end
  object Button1: TButton
    Left = 176
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Evolve DB'
    TabOrder = 2
    OnClick = Button1Click
  end
  object btnCancel: TButton
    Left = 256
    Top = 8
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
