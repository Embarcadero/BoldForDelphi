object BoldLogFrame: TBoldLogFrame
  Left = 0
  Top = 0
  Width = 443
  Height = 67
  Align = alBottom
  AutoSize = True
  Constraints.MaxHeight = 67
  Constraints.MinHeight = 67
  Constraints.MinWidth = 175
  TabOrder = 0
  Visible = False
  object pgLog: TProgressBar
    Left = 0
    Top = 50
    Width = 443
    Height = 17
    Align = alBottom
    ParentShowHint = False
    Step = 1
    ShowHint = False
    TabOrder = 0
  end
  object WarningPanel: TPanel
    Left = 0
    Top = 0
    Width = 25
    Height = 50
    Align = alLeft
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object WarningIndicator: TShape
      Left = 0
      Top = 2
      Width = 25
      Height = 25
      Hint = 'Click to see full Log'
      Brush.Color = clYellow
      ParentShowHint = False
      Shape = stCircle
      ShowHint = True
    end
  end
  object InfoPanel: TPanel
    Left = 25
    Top = 0
    Width = 218
    Height = 50
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object lblLogText: TLabel
      Left = 2
      Top = 32
      Width = 54
      Height = 15
      Caption = 'lblLogText'
      Color = clBtnFace
      ParentColor = False
    end
    object lblLogMainHeader: TLabel
      Left = 2
      Top = 0
      Width = 98
      Height = 15
      Caption = 'lblLogMainHeader'
    end
    object lblLogHeader: TLabel
      Left = 2
      Top = 16
      Width = 71
      Height = 15
      Caption = 'lblLogHeader'
      Color = clBtnFace
      ParentColor = False
    end
  end
  object Panel1: TPanel
    Left = 243
    Top = 0
    Width = 200
    Height = 50
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      200
      50)
    object lblTimeLeft: TLabel
      Left = 16
      Top = 16
      Width = 94
      Height = 15
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Time left: 00:00:00'
    end
    object lblTotTime: TLabel
      Left = 19
      Top = 0
      Width = 91
      Height = 15
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Tot time: 00:00:00'
    end
    object btnStop: TButton
      Left = 118
      Top = 2
      Width = 81
      Height = 25
      Cursor = crHandPoint
      Caption = 'Stop'
      TabOrder = 0
      OnClick = btnStopClick
    end
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 196
  end
end
