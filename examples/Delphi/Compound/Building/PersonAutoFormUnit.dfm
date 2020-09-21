object PersonAutoForm: TPersonAutoForm
  Left = 31
  Top = 96
  Width = 323
  Height = 419
  Caption = 'Person Detail'
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
  object Label1: TLabel
    Left = 8
    Top = 7
    Width = 47
    Height = 13
    Caption = 'FirstName'
  end
  object Label2: TLabel
    Left = 8
    Top = 52
    Width = 48
    Height = 13
    Caption = 'LastName'
  end
  object Label4: TLabel
    Left = 136
    Top = 7
    Width = 31
    Height = 13
    Caption = 'Assets'
  end
  object Label3: TLabel
    Left = 9
    Top = 116
    Width = 82
    Height = 13
    Caption = 'Owned Buildings:'
  end
  object Label5: TLabel
    Left = 8
    Top = 272
    Width = 214
    Height = 26
    Caption = 
      'This form is installed by the PlaceableAFP-component on the data' +
      'module'
    WordWrap = True
  end
  object BoldEdit1: TBoldEdit
    Left = 8
    Top = 67
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
    TabOrder = 0
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
    TabOrder = 1
  end
  object BoldEdit7: TBoldEdit
    Left = 136
    Top = 23
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
    TabOrder = 2
  end
  object bcbIsMarried: TBoldCheckBox
    Left = 138
    Top = 67
    Width = 73
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
    Width = 249
    Height = 121
    AddNewAtEnd = False
    BoldAutoColumns = True
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
      end
      item
        BoldProperties.Expression = 'owners->size'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = '# owners'
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
    OnObjectDeleted = brhPersonObjectDeleted
    OnValueDestroyed = brhPersonObjectDeleted
    Left = 16
    Top = 8
  end
  object blhOwnedBuildings: TBoldListHandle
    RootHandle = brhPerson
    Expression = 'ownedBuildings'
    Left = 24
    Top = 168
  end
end
