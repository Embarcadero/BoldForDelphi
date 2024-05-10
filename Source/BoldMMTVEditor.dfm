object frmBoldMMTVEdit: TfrmBoldMMTVEdit
  Left = 363
  Top = 149
  Width = 522
  Height = 419
  Anchors = [akLeft, akTop, akRight]
  Caption = 'Bold Tagged Value Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 145
    Top = 0
    Width = 3
    Height = 354
    Cursor = crHSplit
    Beveled = True
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 354
    Width = 514
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object tvModelElements: TTreeView
    Left = 0
    Top = 0
    Width = 145
    Height = 354
    Align = alLeft
    Indent = 19
    TabOrder = 1
    OnChange = tvModelElementsChange
    OnChanging = tvModelElementsChanging
  end
  object Panel1: TPanel
    Left = 148
    Top = 0
    Width = 366
    Height = 354
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 2
    object tcTaggedValues: TTabControl
      Left = 0
      Top = 0
      Width = 366
      Height = 354
      Align = alClient
      TabOrder = 0
      Tabs.Strings = (
        'Std UML')
      TabIndex = 0
      OnChange = tcTaggedValuesChange
      OnChanging = tcTaggedValuesChanging
      object vleTaggedValues: TValueListEditor
        Left = 4
        Top = 24
        Width = 358
        Height = 326
        Align = alClient
        TabOrder = 0
        TitleCaptions.Strings = (
          'Tag'
          'Value')
        OnEditButtonClick = vleTaggedValuesEditButtonClick
        OnStringsChange = vleTaggedValuesStringsChange
        ColWidths = (
          150
          202)
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 64
    Top = 65520
    object Edit1: TMenuItem
      Caption = 'Edit'
    end
    object About1: TMenuItem
      Caption = 'Help'
      object About2: TMenuItem
        Caption = 'About'
      end
    end
  end
end
