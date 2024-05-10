object BoldLEPropagatorMainForm: TBoldLEPropagatorMainForm
  Left = 409
  Top = 289
  BorderStyle = bsSingle
  Caption = 'Bold LowEnd Propagator'
  ClientHeight = 288
  ClientWidth = 482
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 269
    Width = 482
    Height = 19
    Panels = <
      item
        Text = 'Connected Clients: '
        Width = 200
      end
      item
        Style = psOwnerDraw
        Width = 50
      end>
    SimplePanel = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 40
    Width = 482
    Height = 229
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 145
      Top = 0
      Width = 3
      Height = 229
      Cursor = crHSplit
    end
    object ListBox1: TListBox
      Left = 0
      Top = 0
      Width = 145
      Height = 229
      Align = alLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 0
    end
    object Memo1: TMemo
      Left = 148
      Top = 0
      Width = 334
      Height = 229
      Align = alClient
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 482
    Height = 40
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 4
      Top = 24
      Width = 45
      Height = 13
      Caption = 'Client IDs'
    end
    object Label2: TLabel
      Left = 152
      Top = 24
      Width = 33
      Height = 13
      Caption = 'Events'
    end
    object cbLogEvents: TCheckBox
      Left = 256
      Top = 16
      Width = 97
      Height = 17
      Caption = '&Show Events'
      TabOrder = 0
    end
    object btnClearEvents: TButton
      Left = 396
      Top = 8
      Width = 81
      Height = 23
      Caption = '&Clear Events'
      TabOrder = 1
      OnClick = btnClearEventsClick
    end
  end
end
