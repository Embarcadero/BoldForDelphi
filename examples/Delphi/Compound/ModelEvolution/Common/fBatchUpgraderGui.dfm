object Form1: TForm1
  Left = 375
  Top = 326
  Width = 490
  Height = 307
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 36
    Height = 13
    Caption = 'Status: '
  end
  object lblStatus: TLabel
    Left = 48
    Top = 8
    Width = 46
    Height = 13
    Caption = 'Unknown'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 62
    Height = 13
    Caption = 'ExecuteTime'
  end
  object Label3: TLabel
    Left = 8
    Top = 72
    Width = 72
    Height = 13
    Caption = 'IntervalTime (s)'
  end
  object Label4: TLabel
    Left = 104
    Top = 64
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label5: TLabel
    Left = 320
    Top = 64
    Width = 18
    Height = 13
    Caption = '100'
  end
  object Label6: TLabel
    Left = 8
    Top = 128
    Width = 48
    Height = 13
    Caption = 'BatchSize'
  end
  object Label7: TLabel
    Left = 104
    Top = 128
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label8: TLabel
    Left = 320
    Top = 128
    Width = 18
    Height = 13
    Caption = '100'
  end
  object Label9: TLabel
    Left = 8
    Top = 200
    Width = 64
    Height = 13
    Caption = 'SleepTime (s)'
  end
  object Label10: TLabel
    Left = 104
    Top = 192
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label11: TLabel
    Left = 320
    Top = 192
    Width = 18
    Height = 13
    Caption = '100'
  end
  object lblReport: TLabel
    Left = 152
    Top = 8
    Width = 45
    Height = 13
    Caption = 'Waiting...'
  end
  object BoldLabel1: TBoldLabel
    Left = 192
    Top = 64
    Width = 53
    Height = 13
    BoldHandle = BoldVariableHandle1
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object BoldLabel2: TBoldLabel
    Left = 192
    Top = 128
    Width = 53
    Height = 13
    BoldHandle = BoldVariableHandle2
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object BoldLabel3: TBoldLabel
    Left = 192
    Top = 192
    Width = 53
    Height = 13
    BoldHandle = BoldVariableHandle3
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object dtpExecuteTime: TDateTimePicker
    Left = 104
    Top = 32
    Width = 73
    Height = 21
    CalAlignment = dtaLeft
    Date = 36921.0416666667
    Time = 36921.0416666667
    DateFormat = dfShort
    DateMode = dmComboBox
    Kind = dtkTime
    ParseInput = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 264
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = Button1Click
  end
  object tbIntervalTime: TBoldTrackBar
    Left = 104
    Top = 80
    Width = 241
    Height = 45
    Max = 100
    Orientation = trHorizontal
    Frequency = 10
    SelEnd = 0
    SelStart = 0
    TabOrder = 2
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    BoldHandle = BoldVariableHandle1
    BoldProperties.ApplyPolicy = bapChange
    ReadOnly = False
  end
  object tbBatchSize: TBoldTrackBar
    Left = 104
    Top = 144
    Width = 241
    Height = 45
    Max = 100
    Orientation = trHorizontal
    Frequency = 10
    SelEnd = 0
    SelStart = 0
    TabOrder = 3
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    BoldHandle = BoldVariableHandle2
    BoldProperties.ApplyPolicy = bapChange
    ReadOnly = False
  end
  object tbSleepTime: TBoldTrackBar
    Left = 104
    Top = 208
    Width = 241
    Height = 45
    Max = 100
    Orientation = trHorizontal
    Frequency = 10
    SelEnd = 0
    SelStart = 0
    TabOrder = 4
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    BoldHandle = BoldVariableHandle3
    BoldProperties.ApplyPolicy = bapChange
    ReadOnly = False
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 328
    Top = 8
  end
  object BoldVariableHandle1: TBoldVariableHandle
    StaticSystemHandle = BoldSystemHandle1
    ValueTypeName = 'Integer'
    InitialValues.Strings = (
      '3')
    Left = 280
    Top = 80
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = True
    AutoActivate = True
    SystemTypeInfoHandle = dmSystemTypeInfo.SystemTypeInfo
    Active = False
    Left = 216
    Top = 24
  end
  object BoldVariableHandle2: TBoldVariableHandle
    StaticSystemHandle = BoldSystemHandle1
    ValueTypeName = 'Integer'
    InitialValues.Strings = (
      '10')
    Left = 280
    Top = 144
  end
  object BoldVariableHandle3: TBoldVariableHandle
    StaticSystemHandle = BoldSystemHandle1
    ValueTypeName = 'Integer'
    InitialValues.Strings = (
      '5')
    Left = 280
    Top = 208
  end
end
