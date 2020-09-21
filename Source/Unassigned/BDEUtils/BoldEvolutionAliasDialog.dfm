object BoldEvolutionAliasDialog: TBoldEvolutionAliasDialog
  Left = 48
  Top = 320
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Evolve Database'
  ClientHeight = 136
  ClientWidth = 185
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
  object pnlMode: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 56
      Height = 13
      Caption = 'Script mode'
    end
    object cmbScriptMode: TComboBox
      Left = 8
      Top = 24
      Width = 169
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'Script for this DB only'
        'Generic script for this schema')
    end
  end
  object pnlAlias: TPanel
    Left = 0
    Top = 48
    Width = 185
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 69
      Height = 13
      Caption = 'Select an alias'
    end
    object ComboBoxAlias: TComboBox
      Left = 8
      Top = 24
      Width = 169
      Height = 21
      DropDownCount = 16
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnChange = ComboBoxAliasChange
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 96
    Width = 185
    Height = 40
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object BtnOK: TButton
      Left = 8
      Top = 8
      Width = 81
      Height = 24
      Caption = 'OK'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
      OnClick = BtnOKClick
    end
    object BtnCancel: TButton
      Left = 96
      Top = 8
      Width = 81
      Height = 24
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
