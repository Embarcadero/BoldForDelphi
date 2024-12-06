object CustomAttrForm1: TCustomAttrForm1
  Left = 232
  Top = 237
  HelpContext = 10
  Caption = 'CustomAttrForm1'
  ClientHeight = 275
  ClientWidth = 531
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
    Width = 531
    Height = 275
    ActivePage = tsProperties
    Align = alClient
    Style = tsButtons
    TabOrder = 0
    object tsClassDef: TTabSheet
      HelpContext = 11
      Caption = 'Definition'
      OnShow = tsClassDefShow
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 523
        Height = 244
        Align = alClient
        Caption = ' Class Definition '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label1: TLabel
          Left = 73
          Top = 43
          Width = 31
          Height = 13
          Caption = 'Parent'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 21
          Top = 107
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
        object Label7: TLabel
          Left = 112
          Top = 128
          Width = 115
          Height = 13
          Caption = '( Ex: MyCustomAttribute)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label4: TLabel
          Left = 15
          Top = 179
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
        object Label8: TLabel
          Left = 112
          Top = 200
          Width = 122
          Height = 13
          Caption = '( Ex: TMyCustomAttribute)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 258
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
        object Label2: TLabel
          Left = 248
          Top = 107
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
        object cbParent: TComboBox
          Left = 112
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
          OnChange = cbParentChange
        end
        object edExpressionName: TEdit
          Left = 112
          Top = 104
          Width = 121
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          Text = 'Name'
          OnChange = edExpressionNameChange
        end
        object edDelphiName: TEdit
          Left = 112
          Top = 176
          Width = 121
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnChange = edDelphiNameChange
        end
        object edUnitname: TEdit
          Left = 316
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
          OnChange = edUnitnameChange
        end
        object edModelName: TEdit
          Left = 316
          Top = 104
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
    object tsProperties: TTabSheet
      HelpContext = 12
      Caption = 'Properties'
      ImageIndex = 1
      OnShow = tsPropertiesShow
      object GroupBox4: TGroupBox
        Left = 0
        Top = 0
        Width = 523
        Height = 244
        Align = alClient
        Caption = 'Define new properties'
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
          Width = 519
          Height = 29
          ButtonHeight = 25
          Caption = 'ToolBar1'
          EdgeBorders = [ebRight]
          EdgeInner = esNone
          EdgeOuter = esNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          TabStop = True
          object ToolButton2: TToolButton
            Left = 0
            Top = 0
            Width = 8
            Caption = 'ToolButton2'
            Style = tbsSeparator
          end
          object bbPropertyAdd: TBitBtn
            Left = 8
            Top = 0
            Width = 25
            Height = 25
            Hint = 'Add (Ctrl + Ins)'
            Action = ActionStringGridInsert
            ImageIndex = 0
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
          end
          object bbPropertyDelete: TBitBtn
            Left = 33
            Top = 0
            Width = 25
            Height = 25
            Hint = 'Delete (Ctrl + Del)'
            Action = ActionStringGridDelete
            ImageIndex = 1
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
          end
          object bbPropertyMoveDown: TBitBtn
            Left = 58
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
            TabOrder = 2
            OnClick = bbPropertyMoveDownClick
          end
          object bbPropertyMoveUp: TBitBtn
            Left = 83
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
            TabOrder = 3
            OnClick = bbPropertyMoveUpClick
          end
        end
        object StringGridProperties: TStringGrid
          Left = 2
          Top = 44
          Width = 519
          Height = 198
          Align = alClient
          ColCount = 3
          DefaultColWidth = 100
          DefaultRowHeight = 21
          FixedCols = 0
          RowCount = 2
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
          ParentFont = False
          TabOrder = 1
          OnDrawCell = StringGridPropertiesDrawCell
        end
        object cbPropertyTypes: TComboBox
          Left = 272
          Top = 80
          Width = 121
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          Items.Strings = (
            'integer'
            'real'
            'boolean'
            'extended'
            'cardinal'
            'char'
            'string')
        end
        object cbAccessTypes: TComboBox
          Left = 272
          Top = 120
          Width = 121
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          Items.Strings = (
            'Read/Write'
            'ReadOnly'
            'WriteOnly')
        end
        object edPropertyName: TEdit
          Left = 272
          Top = 160
          Width = 121
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
        end
        object MemoMethodsToOverride: TMemo
          Left = 104
          Top = 104
          Width = 145
          Height = 121
          Enabled = False
          Lines.Strings = (
            'interface'
            ''
            ''
            '  TBoldElement = class( TObject)'
            '  protected'
            
              '   function GetStringRepresentation(Representation: TBoldReprese' +
              'ntation): string; virtual;'
            
              '   procedure SetStringRepresentation(Representation: TBoldRepres' +
              'entation; Value: string); virtual;'
            '  public'
            
              '   procedure DefaultSubscribe(Subscriber : TBoldSubscriber; Requ' +
              'estedEvent : TBoldEvent); virtual; abstract;'
            
              '   function CompareToAs(CompareType:TBoldCompareType; BoldElemen' +
              't:TBoldElement): Integer; virtual;'
            
              '   function IsEqualAs(CompareType:TBoldCompareType; BoldElement:' +
              'TBoldElement): Boolean; virtual;'
            '   procedure Assign(Source:TBoldElement); virtual;'
            
              '   function ValidateCharacter(C:AnsiChar; Representation:TBoldRe' +
              'presentation): Boolean; virtual;'
            
              '   function ValidateString(Value: string;Representation:TBoldRep' +
              'resentation): Boolean;virtual;'
            
              '   procedure SubscribeToStringRepresentation(Representation: TBo' +
              'ldRepresentation;Subscriber: TBoldSubscriber;RequestedEvent: TBo' +
              'ldEvent);virtual;'
            '  end;'
            ''
            '  TBoldDomainElement = class(TObject)'
            '  protected'
            
              '   procedure ReceiveEventFromOwned(Originator: TObject; Original' +
              'Event: TBoldEvent); virtual;'
            
              '   function ReceiveQueryFromOwned(Originator: TObject; OriginalE' +
              'vent: TBoldEvent; const Args: array of const; Subscriber: TBoldS' +
              'ubscriber): Boolean; virtual;'
            '  end;'
            ''
            '  TBoldMember = class(TObject)'
            '  protected'
            '   procedure AssignValue( Source: IBoldValue ); virtual;'
            '   procedure AssignContentValue(Source: IBoldValue); virtual;'
            '   procedure CompleteModify; virtual;'
            
              '   procedure InitializeMember(AOwningElement: TBoldDomainElement' +
              '; ElementTypeInfo: TBoldElementTypeInfo); virtual;'
            '   function MayModify: Boolean; virtual;'
            '   function MayUpdate: Boolean; virtual;'
            '   procedure PrepareModify; virtual;'
            '  end;'
            ''
            '  TBoldAttribute = class(TObject)'
            '  protected'
            '   function GetAsVariant: Variant; virtual;'
            '   procedure SetAsVariant(const Value: Variant); virtual;'
            '  end;'
            ''
            ''
            ' ')
          TabOrder = 5
          Visible = False
          WordWrap = False
        end
      end
    end
    object tsMethods: TTabSheet
      HelpContext = 13
      Caption = 'New Methods'
      ImageIndex = 2
      OnShow = tsMethodsShow
      object Label5: TLabel
        Left = 8
        Top = 176
        Width = 40
        Height = 13
        Caption = 'Override'
      end
      object GroupBox3: TGroupBox
        Left = 0
        Top = 0
        Width = 523
        Height = 244
        Align = alClient
        Caption = 'Define new methods '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object ToolBar2: TToolBar
          Left = 2
          Top = 15
          Width = 519
          Height = 29
          ButtonHeight = 25
          Caption = 'ToolBar2'
          EdgeInner = esNone
          EdgeOuter = esNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object ToolButton1: TToolButton
            Left = 0
            Top = 2
            Width = 8
            Caption = 'ToolButton1'
            Style = tbsSeparator
          end
          object bbNewMethod: TBitBtn
            Left = 8
            Top = 2
            Width = 25
            Height = 25
            Hint = 'Add (Ctrl + Ins)'
            Action = ActionStringGridInsert
            ImageIndex = 0
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
          end
          object bbDeleteMethod: TBitBtn
            Left = 33
            Top = 2
            Width = 25
            Height = 25
            Hint = 'Delete (Ctrl + Del)'
            Action = ActionStringGridInsert
            ImageIndex = 0
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
          end
          object bbMethodMoveDown: TBitBtn
            Left = 58
            Top = 2
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
            TabOrder = 2
            OnClick = bbMethodMoveDownClick
          end
          object bbMethodMoveUp: TBitBtn
            Left = 83
            Top = 2
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
            TabOrder = 3
            OnClick = bbPropertyMoveUpClick
          end
        end
        object StringGridMethods: TStringGrid
          Left = 2
          Top = 44
          Width = 519
          Height = 198
          Align = alClient
          DefaultColWidth = 100
          DefaultRowHeight = 21
          FixedCols = 0
          RowCount = 2
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
          ParentFont = False
          TabOrder = 1
          OnDrawCell = StringGridMethodsDrawCell
          ColWidths = (
            77
            79
            100
            155
            107)
        end
        object cbVisibility: TComboBox
          Left = 96
          Top = 72
          Width = 145
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          Items.Strings = (
            'private'
            'protected'
            'public'
            'published')
        end
        object edMethodName: TEdit
          Left = 112
          Top = 96
          Width = 121
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
        end
        object cbReturnTypes: TComboBox
          Left = 248
          Top = 88
          Width = 145
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          Items.Strings = (
            'boolean'
            'integer'
            'real'
            'extended'
            'char'
            'string'
            '')
        end
        object edMethodSignature: TEdit
          Left = 248
          Top = 120
          Width = 121
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
        end
        object cbMethodTypes: TComboBox
          Left = 232
          Top = 168
          Width = 145
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          Items.Strings = (
            'procedure'
            'function')
        end
      end
    end
    object tsOverride: TTabSheet
      HelpContext = 14
      Caption = 'Methods to Override'
      ImageIndex = 4
      OnShow = tsOverrideShow
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 531
        Height = 256
        Align = alClient
        Caption = 'Select methods to override '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object CheckListBoxOverride: TCheckListBox
          Left = 2
          Top = 15
          Width = 527
          Height = 239
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          ParentShowHint = False
          ShowHint = False
          TabOrder = 0
          OnClickCheck = CheckListBoxOverrideClickCheck
        end
      end
    end
  end
  object ActionList: TActionList
    Left = 28
    Top = 144
    object ActionStringGridInsert: TAction
      ImageIndex = 0
      ShortCut = 16429
      OnExecute = ActionStringGridInsertExecute
    end
    object ActionStringGridDelete: TAction
      ImageIndex = 1
      ShortCut = 16430
      OnExecute = ActionStringGridDeleteExecute
    end
  end
end
