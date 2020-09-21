object PersonAutoForm: TPersonAutoForm
  Left = 317
  Top = 174
  Width = 316
  Height = 295
  Caption = 'Person Detail'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 7
    Width = 50
    Height = 13
    Caption = 'FirstName:'
  end
  object Label2: TLabel
    Left = 160
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Assets:'
  end
  object Label3: TLabel
    Left = 8
    Top = 56
    Width = 51
    Height = 13
    Caption = 'LastName:'
  end
  object Label4: TLabel
    Left = 8
    Top = 120
    Width = 79
    Height = 13
    Caption = 'Owned Buildings'
  end
  object beFirstName: TBoldEdit
    Left = 8
    Top = 23
    Width = 121
    Height = 21
    BoldHandle = brhPerson
    BoldProperties.Expression = 'firstName'
    ReadOnly = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Alignment = taLeftJustify
    ButtonStyle = bbsNone
    MaxLength = 0
    TabOrder = 0
  end
  object BoldEdit2: TBoldEdit
    Left = 160
    Top = 24
    Width = 121
    Height = 21
    BoldHandle = brhPerson
    BoldProperties.Expression = 'assets'
    ReadOnly = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Alignment = taLeftJustify
    ButtonStyle = bbsNone
    MaxLength = 0
    TabOrder = 1
  end
  object BoldEdit3: TBoldEdit
    Left = 8
    Top = 72
    Width = 121
    Height = 21
    BoldHandle = brhPerson
    BoldProperties.Expression = 'lastName'
    ReadOnly = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Alignment = taLeftJustify
    ButtonStyle = bbsNone
    MaxLength = 0
    TabOrder = 2
  end
  object bcbIsMarried: TBoldCheckBox
    Left = 160
    Top = 72
    Width = 97
    Height = 17
    BoldHandle = brhPerson
    BoldProperties.Expression = 'isMarried'
    Caption = 'Is Married'
    ReadOnly = False
    TabOrder = 3
  end
  object bgrOwnedBuildings: TBoldGrid
    Left = 8
    Top = 136
    Width = 233
    Height = 121
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhOwnedBuildings
    BoldProperties.NilElementMode = neNone
    Columns = <
      item
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        BoldProperties.Expression = 'zipCode'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'ZipCode'
      end
      item
        BoldProperties.Expression = 'address'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Address'
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      64
      64
      64)
  end
  object brhPerson: TBoldReferenceHandle
    StaticSystemHandle = DataModule1.BoldSystemHandle1
    StaticValueTypeName = 'Person'
    Left = 80
    Top = 176
  end
  object blhOwnedBuildings: TBoldListHandle
    RootHandle = brhPerson
    Expression = 'ownedBuildings'
    Left = 152
    Top = 176
  end
end
