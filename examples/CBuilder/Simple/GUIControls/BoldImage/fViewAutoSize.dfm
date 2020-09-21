object frmImageViewer: TfrmImageViewer
  Left = 438
  Top = 155
  Width = 320
  Height = 240
  Caption = 'Image Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 312
    Height = 27
    AutoSize = True
    ButtonHeight = 21
    ButtonWidth = 22
    EdgeBorders = [ebTop, ebBottom]
    ShowCaptions = True
    TabOrder = 0
    object btnScale100: TToolButton
      Left = 0
      Top = 2
      Caption = '1:1'
      ImageIndex = 0
      OnClick = btnScale100Click
    end
    object ToolButton2: TToolButton
      Left = 22
      Top = 2
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object cboScale: TComboBox
      Left = 30
      Top = 2
      Width = 57
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = '100'
      OnChange = cboScaleChange
      Items.Strings = (
        '25'
        '50'
        '100'
        '200'
        '300'
        '400')
    end
    object ToolButton1: TToolButton
      Left = 87
      Top = 2
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object btnAuto: TToolButton
      Left = 95
      Top = 2
      Caption = 'A'
      Grouped = True
      ImageIndex = 2
      Style = tbsCheck
      OnClick = btnAutoClick
    end
    object btnScale: TToolButton
      Left = 117
      Top = 2
      Caption = 'S'
      Down = True
      Grouped = True
      ImageIndex = 3
      Style = tbsCheck
      OnClick = btnScaleClick
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 27
    Width = 312
    Height = 186
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    Align = alClient
    BorderStyle = bsNone
    Color = clBtnFace
    ParentColor = False
    TabOrder = 1
    object BoldImage: TBoldImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      BoldHandle = behImage
      ReadOnly = True
      AutoSize = True
      DrawFocus = False
      StretchMode = bsmStretchToScale
      Center = False
      QuickDraw = False
      BorderStyle = bsNone
      OnResize = BoldImageResize
      ParentColor = True
      TabOrder = 0
    end
  end
  object behImage: TBoldReferenceHandle
    Left = 12
    Top = 35
  end
end
