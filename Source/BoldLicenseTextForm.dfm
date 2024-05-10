object frmLicenseText: TfrmLicenseText
  Left = 418
  Top = 279
  Width = 332
  Height = 426
  Caption = 'BoldSoft License Agreement'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 358
    Width = 324
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object rbAgree: TRadioButton
      Left = 4
      Top = 4
      Width = 113
      Height = 17
      Caption = '&I Agree'
      TabOrder = 0
    end
    object rbNotAgree: TRadioButton
      Left = 4
      Top = 20
      Width = 113
      Height = 17
      Caption = 'I do not agree'
      TabOrder = 1
    end
    object Button1: TButton
      Left = 244
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 2
    end
  end
  object mmoLicenseTextTemplate: TMemo
    Left = 0
    Top = 0
    Width = 324
    Height = 358
    Align = alClient
    TabOrder = 1
  end
end
