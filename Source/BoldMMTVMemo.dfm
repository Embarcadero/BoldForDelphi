object frmMemoEdit: TfrmMemoEdit
  Left = 397
  Top = 213
  Caption = ' - Bold tagged value editor'
  ClientHeight = 196
  ClientWidth = 294
  Color = clBtnFace
  Constraints.MinHeight = 58
  Constraints.MinWidth = 171
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 13
  object memoTheMemo: TMemo
    Left = 0
    Top = 0
    Width = 294
    Height = 171
    Cursor = crIBeam
    Align = alClient
    BevelOuter = bvNone
    Lines.Strings = (
      '')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 171
    Width = 294
    Height = 25
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      294
      25)
    object Button1: TButton
      Left = 224
      Top = 0
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Save'
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 142
      Top = 0
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
