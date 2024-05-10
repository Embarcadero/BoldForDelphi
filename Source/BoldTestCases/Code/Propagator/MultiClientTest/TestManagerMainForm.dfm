object MainForm: TMainForm
  Left = 377
  Top = 199
  Width = 612
  Height = 523
  Caption = 'MultiClient Test Manager'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 604
    Height = 491
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 11
      Width = 123
      Height = 16
      Caption = 'Number of Events'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 18
      Top = 191
      Width = 75
      Height = 16
      Caption = 'Test Results'
    end
    object Label3: TLabel
      Left = 16
      Top = 96
      Width = 106
      Height = 16
      Caption = 'Number of clients '
    end
    object edNumber: TMaskEdit
      Left = 15
      Top = 47
      Width = 129
      Height = 24
      EditMask = '99999999999999999999999999;1; '
      MaxLength = 26
      TabOrder = 0
      Text = '100                       '
    end
    object UpDown1: TUpDown
      Left = 142
      Top = 46
      Width = 19
      Height = 29
      Min = 0
      Position = 0
      TabOrder = 1
      Wrap = False
      OnClick = UpDown1Click
    end
    object LogMemo: TMemo
      Left = 10
      Top = 217
      Width = 560
      Height = 269
      ScrollBars = ssVertical
      TabOrder = 2
    end
    object btnTest: TButton
      Left = 210
      Top = 128
      Width = 93
      Height = 27
      Caption = '&Run Test'
      TabOrder = 3
      OnClick = btnTestClick
    end
    object rgTest: TRadioGroup
      Left = 207
      Top = 20
      Width = 277
      Height = 77
      Caption = 'Tests'
      Items.Strings = (
        'Auto Test')
      TabOrder = 4
    end
    object ednoclients: TEdit
      Left = 16
      Top = 120
      Width = 121
      Height = 24
      TabOrder = 5
      Text = '0'
    end
    object Button1: TButton
      Left = 208
      Top = 168
      Width = 169
      Height = 25
      Caption = '&Close all Test clients'
      TabOrder = 6
      OnClick = Button1Click
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 10
    OnTimer = Timer1Timer
    Left = 264
    Top = 264
  end
  object LoadTestTimer: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = LoadTestTimerTimer
    Left = 384
    Top = 264
  end
end
