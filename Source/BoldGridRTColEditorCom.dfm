object frmRTColEditorCom: TfrmRTColEditorCom
  Left = 444
  Top = 599
  BorderIcons = [biSystemMenu]
  Caption = 'Runtime Column Editor'
  ClientHeight = 292
  ClientWidth = 360
  Color = clBtnFace
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
  Position = poScreenCenter
  TextHeight = 13
  object lbxColumns: TListBox
    Left = 0
    Top = 0
    Width = 128
    Height = 252
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbxColumnsClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 252
    Width = 360
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 264
    ExplicitWidth = 368
    object Panel2: TPanel
      Left = 280
      Top = 0
      Width = 88
      Height = 40
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object cmdOK: TButton
        Left = 0
        Top = 8
        Width = 81
        Height = 25
        Caption = 'Done'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
    end
    object cmdAddColumn: TButton
      Left = 4
      Top = 8
      Width = 81
      Height = 25
      Caption = '&Add Column'
      TabOrder = 0
      OnClick = cmdAddColumnClick
    end
    object cmdDeleteColumn: TButton
      Left = 92
      Top = 8
      Width = 81
      Height = 25
      Caption = '&Delete Column'
      TabOrder = 1
      OnClick = cmdDeleteColumnClick
    end
  end
  object Panel3: TPanel
    Left = 128
    Top = 0
    Width = 232
    Height = 252
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 136
    ExplicitHeight = 264
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 74
      Height = 13
      Caption = '&Header Caption'
      FocusControl = tbxCaption
    end
    object Label2: TLabel
      Left = 8
      Top = 56
      Width = 51
      Height = 13
      Caption = '&Expression'
      FocusControl = tbxExpression
      WordWrap = True
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 143
      Width = 217
      Height = 121
      Caption = 'Renderer Info'
      Enabled = False
      TabOrder = 4
      object Label3: TLabel
        Left = 24
        Top = 44
        Width = 74
        Height = 13
        Caption = 'String Renderer'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
      end
      object cbxRenderer: TCheckBox
        Left = 8
        Top = 16
        Width = 93
        Height = 17
        AllowGrayed = True
        Caption = '&Has Renderer'
        TabOrder = 0
      end
      object cbxSetFont: TCheckBox
        Left = 24
        Top = 76
        Width = 85
        Height = 17
        Caption = 'Set &Font'
        TabOrder = 2
      end
      object cbxSetColor: TCheckBox
        Left = 24
        Top = 92
        Width = 85
        Height = 17
        Caption = 'Set &Color'
        TabOrder = 3
      end
      object cbxSetText: TCheckBox
        Left = 24
        Top = 60
        Width = 89
        Height = 17
        Caption = '&Get/Set string'
        TabOrder = 1
      end
    end
    object cmdApply: TButton
      Left = 144
      Top = 104
      Width = 81
      Height = 25
      Caption = '&Apply'
      Enabled = False
      TabOrder = 3
      OnClick = cmdApplyClick
    end
    object tbxCaption: TEdit
      Left = 8
      Top = 24
      Width = 217
      Height = 21
      TabOrder = 0
      OnChange = PropertyChanged
      OnKeyPress = PropertyKeyPress
    end
    object tbxExpression: TEdit
      Left = 8
      Top = 72
      Width = 197
      Height = 21
      TabOrder = 1
      OnChange = PropertyChanged
      OnKeyPress = PropertyKeyPress
    end
    object cmdOCLEditor: TButton
      Left = 204
      Top = 72
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 2
      OnClick = cmdOCLEditorClick
    end
  end
end
