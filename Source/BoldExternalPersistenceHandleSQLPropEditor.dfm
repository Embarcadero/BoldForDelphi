object BoldExternalPersistenceHandleSQLPropEditorForm: TBoldExternalPersistenceHandleSQLPropEditorForm
  Left = 534
  Top = 377
  BorderStyle = bsDialog
  Caption = 'External classes to handle'
  ClientHeight = 266
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object CheckListBox1: TCheckListBox
    Left = 8
    Top = 8
    Width = 233
    Height = 249
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 248
    Top = 8
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 248
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
