object PropagatorMainForm: TPropagatorMainForm
  Left = 428
  Top = 353
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Bold Advanced Propagator'
  ClientHeight = 293
  ClientWidth = 801
  Color = clBtnFace
  Constraints.MinWidth = 520
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF001111
    1111111111111111111111111111111111111111111111111111111111111111
    1111111111111111111111111111111111111111111111111111111111111111
    1111111111111111111111111111111111111111111111111111111111111111
    1111111111111111111111111111111111111111111111111111111111111111
    1111111111111111111111111111111111111111111111111111111111111111
    1111111111111111111111111111111111111111111111111111111111111111
    1111111111111111111111111111111111111111111111111111111111111111
    11111111111111111111111111111111111111111111111111111111111111FF
    FFF11111FFF111FF711FFF1FF71111FFFFFF111FFFFF11FF81FFFFFFF81111FF
    11FFF1FF818FF1FF71FF91FFF71111FF111FF1FF119FF1FF98FF111FF81111FF
    111FF1FF111FF1FF7FF8111FF71111FF111FF1FF111FF1FF9FF8111FF81111FF
    FFFF11FF119FF1FF78FF111FF71111FFFFF111FF81FFF1FF99FF118FF81111FF
    11FF111FFFFF11FF71FFFFFFF71111FF111FF111FFF111FF917FFF1FF81111FF
    111FF111111111FF7111111FF71111FF91FF9111111111FF9111111FF81111FF
    FFFF1111111111FF7111111FF71111FFFF991111111111FF9111111FF8111111
    1111111111111111111111111111111111111111111111111111111111110000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 801
    Height = 274
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 801
      Height = 274
      ActivePage = tsClients
      Align = alClient
      TabOrder = 0
      object tsClients: TTabSheet
        Caption = 'Clients'
        object sgClients: TStringGrid
          Left = 0
          Top = 0
          Width = 793
          Height = 246
          Align = alClient
          ColCount = 12
          DefaultRowHeight = 17
          FixedCols = 0
          RowCount = 2
          TabOrder = 0
          ColWidths = (
            50
            144
            110
            56
            72
            45
            60
            64
            33
            30
            49
            36)
        end
        object rbDequeue: TRadioButton
          Left = 11
          Top = 45
          Width = 38
          Height = 17
          Caption = 'DQ'
          TabOrder = 1
        end
      end
      object tsMemory: TTabSheet
        Caption = 'Memory'
        ImageIndex = 1
        object mmoMemory: TMemo
          Left = 0
          Top = 0
          Width = 377
          Height = 246
          Align = alLeft
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object lbxSystemMemory: TListBox
          Left = 377
          Top = 0
          Width = 416
          Height = 246
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ItemHeight = 14
          ParentFont = False
          TabOrder = 1
        end
      end
      object tsStatisticsHistory: TTabSheet
        Caption = 'Statistics History'
        ImageIndex = 2
        object lbStatisticsHistory: TListBox
          Left = 0
          Top = 0
          Width = 793
          Height = 246
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ItemHeight = 14
          ParentFont = False
          TabOrder = 0
        end
      end
      object TabSheet1: TTabSheet
        Caption = 'DebugInfo'
        ImageIndex = 3
        object mmoDebugInfo: TMemo
          Left = 0
          Top = 0
          Width = 793
          Height = 246
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 274
    Width = 801
    Height = 19
    Panels = <
      item
        Width = 42
      end
      item
        Width = 50
      end
      item
        Width = 28
      end
      item
        Width = 150
      end
      item
        Width = 50
      end>
  end
  object tmrDeadLockChecker: TTimer
    OnTimer = tmrDeadLockCheckerTimer
    Left = 28
    Top = 165
  end
  object MainMenu1: TMainMenu
    Left = 156
    Top = 125
    object File1: TMenuItem
      Caption = 'File'
      object mnuHangPropagatorDEBUG: TMenuItem
        Caption = 'Hang Propagator (DEBUG)'
        Visible = False
        OnClick = mnuLockClick
      end
      object mnuShutdown: TMenuItem
        Caption = 'Disconnect Clients'
        OnClick = mnuDisconnectClientsClick
      end
    end
    object TMenuItem
      Caption = 'View'
      object mnuRefreshGUI: TMenuItem
        Caption = 'Refresh GUI'
        OnClick = mnuRefreshClick
      end
      object mnuCountSubscriptions: TMenuItem
        Caption = 'Count Subscriptions'
        OnClick = mnuCountSubscriptionsClick
      end
      object mnuOnTop: TMenuItem
        Caption = 'StayOnTop'
        OnClick = mnuStayOnTopClick
      end
    end
    object TMenuItem
      Enabled = False
    end
    object N1: TMenuItem
      Caption = '|'
      Enabled = False
    end
    object TMenuItem
      Enabled = False
    end
    object mnuStarted: TMenuItem
      Caption = 'Started:'
    end
    object mnuUptime: TMenuItem
      Caption = 'Uptime:'
    end
  end
end
