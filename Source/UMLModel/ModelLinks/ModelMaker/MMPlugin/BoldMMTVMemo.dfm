object frmMemoEdit: TfrmMemoEdit
  Left = 397
  Top = 213
  Width = 310
  Height = 235
  Caption = ' - Bold tagged value editor'
  Color = clBtnFace
  Constraints.MinHeight = 58
  Constraints.MinWidth = 171
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object memoTheMemo: TMemo
    Left = 0
    Top = 0
    Width = 302
    Height = 183
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
    Top = 183
    Width = 302
    Height = 25
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      302
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
