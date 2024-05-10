object frmBoldQueryUser: TfrmBoldQueryUser
  Left = 397
  Top = 411
  BorderStyle = bsToolWindow
  Caption = 'User Query'
  ClientHeight = 136
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblQuestion: TLabel
    Left = 24
    Top = 12
    Width = 365
    Height = 77
    AutoSize = False
    Caption = 'lblQuestion'
    WordWrap = True
  end
  object cmdYesAll: TButton
    Left = 120
    Top = 100
    Width = 81
    Height = 25
    Caption = 'Yes to &all'
    ModalResult = 1
    TabOrder = 1
    OnClick = cmdYesAllClick
  end
  object cmdYes: TButton
    Left = 32
    Top = 100
    Width = 81
    Height = 25
    Caption = '&Yes'
    ModalResult = 1
    TabOrder = 0
    OnClick = cmdYesClick
  end
  object cmdNo: TButton
    Left = 208
    Top = 100
    Width = 81
    Height = 25
    Caption = '&No'
    ModalResult = 1
    TabOrder = 2
    OnClick = cmdNoClick
  end
  object cmdNoAll: TButton
    Left = 296
    Top = 100
    Width = 81
    Height = 25
    Caption = 'Cancel'
    ModalResult = 1
    TabOrder = 3
    OnClick = cmdNoAllClick
  end
end
