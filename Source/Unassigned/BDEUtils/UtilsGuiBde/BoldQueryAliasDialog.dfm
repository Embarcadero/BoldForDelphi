object BoldQueryAliasDialog: TBoldQueryAliasDialog
  Left = 232
  Top = 271
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 83
  ClientWidth = 264
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 264
    Height = 83
    Align = alClient
    Caption = ' Select an alias or folder: '
    TabOrder = 0
    object ComboBoxAlias: TComboBox
      Left = 14
      Top = 20
      Width = 235
      Height = 21
      DropDownCount = 16
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnChange = ComboBoxAliasChange
    end
    object BtnBrowse: TButton
      Left = 16
      Top = 49
      Width = 75
      Height = 24
      Caption = '&Browse...'
      TabOrder = 1
      OnClick = BtnBrowseClick
    end
    object BtnOK: TButton
      Left = 96
      Top = 49
      Width = 75
      Height = 24
      Caption = 'OK'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 2
      OnClick = BtnOKClick
    end
    object BtnCancel: TButton
      Left = 174
      Top = 49
      Width = 75
      Height = 24
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
  end
end
