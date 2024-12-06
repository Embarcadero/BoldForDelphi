object ValueSetForm1: TValueSetForm1
  Left = 271
  Top = 240
  HelpContext = 30
  Caption = 'ValueSetForm1'
  ClientHeight = 266
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnDestroy = FormDestroy
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 460
    Height = 266
    ActivePage = tsValues
    Align = alClient
    Style = tsButtons
    TabOrder = 0
    object tsClassDef: TTabSheet
      HelpContext = 31
      Caption = 'Definition'
      OnShow = tsClassDefShow
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 460
        Height = 247
        Align = alClient
        Caption = ' Class Definition '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label2: TLabel
          Left = 30
          Top = 43
          Width = 82
          Height = 13
          Caption = 'Expression Name'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label1: TLabel
          Left = 120
          Top = 64
          Width = 79
          Height = 13
          Caption = '( Ex: dayofweek)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 23
          Top = 115
          Width = 89
          Height = 13
          Caption = 'Delphi Class Name'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label5: TLabel
          Left = 120
          Top = 136
          Width = 96
          Height = 13
          Caption = '( Ex: TDayOfWeek )'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label4: TLabel
          Left = 56
          Top = 187
          Width = 56
          Height = 13
          Caption = 'Value Prefix'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 120
          Top = 208
          Width = 119
          Height = 13
          Caption = '( Ex: dw for DayofWeek )'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label10: TLabel
          Left = 266
          Top = 43
          Width = 50
          Height = 13
          Caption = 'Unit Name'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label8: TLabel
          Left = 256
          Top = 115
          Width = 60
          Height = 13
          Caption = 'Model Name'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          Visible = False
        end
        object Label9: TLabel
          Left = 324
          Top = 136
          Width = 79
          Height = 13
          Caption = '( Used in model )'
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          Visible = False
        end
        object edExpressionName: TEdit
          Left = 120
          Top = 40
          Width = 121
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnChange = edExpressionNameChange
        end
        object edDelphiName: TEdit
          Left = 120
          Top = 112
          Width = 121
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnChange = edDelphiNameChange
        end
        object edValuePrefix: TEdit
          Left = 120
          Top = 184
          Width = 121
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
        object edUnitName: TEdit
          Left = 324
          Top = 40
          Width = 121
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnChange = edUnitNameChange
        end
        object edModelName: TEdit
          Left = 324
          Top = 112
          Width = 121
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          Visible = False
        end
      end
    end
    object tsValues: TTabSheet
      HelpContext = 32
      Caption = 'Values'
      ImageIndex = 1
      OnShow = tsValuesShow
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 452
        Height = 235
        Align = alClient
        Caption = '  Define values  '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object ToolBar1: TToolBar
          Left = 2
          Top = 15
          Width = 448
          Height = 29
          ButtonHeight = 25
          Caption = 'ToolBar1'
          EdgeInner = esNone
          EdgeOuter = esNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          TabStop = True
          object ToolButton1: TToolButton
            Left = 0
            Top = 0
            Width = 8
            Caption = 'ToolButton1'
            Style = tbsSeparator
          end
          object bbInsert: TBitBtn
            Left = 8
            Top = 0
            Width = 25
            Height = 25
            Hint = 'New Value (Ctrl + Ins)'
            Glyph.Data = {
              36030000424D3603000000000000360000002800000010000000100000000100
              1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
              0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000
              0000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFF000000000000000000000000000000000000000000000000000000FFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000
              0000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
              0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = ActionInsertExecute
          end
          object bbDelete: TBitBtn
            Left = 33
            Top = 0
            Width = 25
            Height = 25
            Hint = 'Delete Value (Ctrl+Del)'
            Glyph.Data = {
              F6000000424DF600000000000000760000002800000010000000100000000100
              0400000000008000000000000000000000001000000010000000000000000000
              80000080000000808000800000008000800080800000C0C0C000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
              7777777777777777777777777777777777777777777777777777777777777777
              7777777777777777777777777777777777777770000000000777777000000000
              0777777000000000077777777777777777777777777777777777777777777777
              7777777777777777777777777777777777777777777777777777}
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnClick = ActionDeleteExecute
          end
          object bbEdit: TBitBtn
            Left = 58
            Top = 0
            Width = 25
            Height = 25
            Hint = 'Edit Value'
            Default = True
            Glyph.Data = {
              76010000424D7601000000000000760000002800000020000000100000000100
              0400000000000001000000000000000000001000000010000000000000000000
              800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555775777777
              57705557757777775FF7555555555555000755555555555F777F555555555550
              87075555555555F7577F5555555555088805555555555F755F75555555555033
              805555555555F755F75555555555033B05555555555F755F75555555555033B0
              5555555555F755F75555555555033B05555555555F755F75555555555033B055
              55555555F755F75555555555033B05555555555F755F75555555555033B05555
              555555F75FF75555555555030B05555555555F7F7F75555555555000B0555555
              5555F777F7555555555501900555555555557777755555555555099055555555
              5555777755555555555550055555555555555775555555555555}
            NumGlyphs = 2
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            OnClick = ActionEditExecute
          end
          object bbDown: TBitBtn
            Left = 83
            Top = 0
            Width = 25
            Height = 25
            Hint = 'Move Down'
            Glyph.Data = {
              76010000424D7601000000000000760000002800000020000000100000000100
              04000000000000010000120B0000120B00001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
              666666666667F66666666666666006666666666666766F666666666666700766
              6666666666766F66666666666600006666666666676666F66666666667000076
              66666666676666F66666666660000006666666667666666F6666666670000007
              666666667666666F66666666000000006666666766666666F666666700000000
              7666666766666666F66666666660066666666667777666776666666666600666
              6666666666766F6666666666666006666666666666766F666666666666600666
              6666666666766F6666666666666006666666666666766F666666666666600666
              6666666666766F66666666666666666666666666667776666666}
            NumGlyphs = 2
            ParentShowHint = False
            ShowHint = True
            TabOrder = 3
            OnClick = bbDownClick
          end
          object bbUp: TBitBtn
            Left = 108
            Top = 0
            Width = 25
            Height = 25
            Hint = 'Move Up'
            Glyph.Data = {
              76010000424D7601000000000000760000002800000020000000100000000100
              04000000000000010000120B0000120B00001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
              66666666666FFF6666666666666006666666666666766F666666666666600666
              6666666666766F6666666666666006666666666666766F666666666666600666
              6666666666766F6666666666666006666666666666766F666666666666600666
              66666666FF666FFFF6666667000000007666666766666666F666666600000000
              6666666766666666F666666670000007666666667666666F6666666660000006
              666666667666666F666666666700007666666666676666F66666666666000066
              66666666676666F666666666667007666666666666766F666666666666600666
              6666666666766F666666666666666666666666666667F6666666}
            NumGlyphs = 2
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
            OnClick = bbUpClick
          end
        end
        object ListViewValues: TListView
          Left = 2
          Top = 44
          Width = 448
          Height = 189
          Align = alClient
          BorderStyle = bsNone
          BorderWidth = 1
          Columns = <
            item
              Caption = 'Value'
              Width = 75
            end
            item
              Caption = 'Representations'
              Width = 300
            end>
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ReadOnly = True
          RowSelect = True
          ParentFont = False
          TabOrder = 1
          ViewStyle = vsReport
          OnDblClick = ActionEditExecute
        end
      end
    end
  end
  object ActionList1: TActionList
    Left = 329
    Top = 200
    object ActionInsert: TAction
      Caption = 'ActionInsert'
      ShortCut = 16429
      OnExecute = ActionInsertExecute
    end
    object ActionDelete: TAction
      Caption = 'ActionDelete'
      ShortCut = 16430
      OnExecute = ActionDeleteExecute
    end
    object ActionEdit: TAction
      Caption = 'ActionEdit'
      OnExecute = ActionEditExecute
    end
  end
end
