object BoldAliasDialog: TBoldAliasDialog
  Left = 345
  Top = 127
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 209
  ClientWidth = 279
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 97
    Height = 13
    Caption = 'Create storage using'
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 170
    Width = 279
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object BtnOK: TButton
      Left = 96
      Top = 8
      Width = 81
      Height = 25
      Caption = 'OK'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
      OnClick = BtnOKClick
    end
    object BtnCancel: TButton
      Left = 184
      Top = 8
      Width = 81
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object gbAlias: TGroupBox
    Left = 0
    Top = 122
    Width = 279
    Height = 48
    Align = alBottom
    Caption = 'Select an Alias or folder'
    TabOrder = 1
    object cmbAlias: TComboBox
      Left = 16
      Top = 16
      Width = 185
      Height = 21
      DropDownCount = 16
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnChange = cmbAliasChange
    end
    object BtnBrowse: TButton
      Left = 208
      Top = 16
      Width = 57
      Height = 21
      Caption = '&Browse...'
      TabOrder = 1
      OnClick = BtnBrowseClick
    end
  end
  object gbCreateStyle: TGroupBox
    Left = 0
    Top = 0
    Width = 279
    Height = 49
    Align = alTop
    Caption = 'Create storage using'
    TabOrder = 2
    object cmbCreateStyle: TComboBox
      Left = 16
      Top = 16
      Width = 249
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'BDE (TTable)'
        'BDE without transactions'
        'SQL (TQuery)')
    end
  end
  object gbScriptMode: TGroupBox
    Left = 0
    Top = 49
    Width = 279
    Height = 48
    Align = alTop
    Caption = 'Script mode'
    TabOrder = 3
    object cmbScriptMode: TComboBox
      Left = 16
      Top = 16
      Width = 249
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'Script for this DB only'
        'Generic script for this schema')
    end
  end
end
