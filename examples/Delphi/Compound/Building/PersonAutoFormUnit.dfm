object PersonAutoForm: TPersonAutoForm
  Left = 31
  Top = 96
  Caption = 'Person Detail'
  ClientHeight = 559
  ClientWidth = 267
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
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
  object Label5: TLabel
    Left = 8
    Top = 346
    Width = 214
    Height = 26
    Caption = 
      'This form is installed by the PlaceableAFP-component on the data' +
      'module'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 9
    Top = 116
    Width = 82
    Height = 13
    Caption = 'Owned Buildings:'
  end
  object Label6: TLabel
    Left = 8
    Top = 269
    Width = 31
    Height = 13
    Caption = 'Home:'
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
    Columns = <
      item
        BoldProperties.Expression = ''
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'zipCode'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'ZipCode'
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'address'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Address'
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'owners->size'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = '# owners'
        LookUpProperties.Expression = ''
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
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
  object BoldComboBox1: TBoldComboBox
    Left = 8
    Top = 288
    Width = 249
    Height = 21
    Alignment = taLeftJustify
    BoldHandle = brhPerson
    BoldListHandle = blhOwnedResidentialBuildings
    BoldProperties.Expression = 'home'
    BoldRowProperties.Expression = ''
    BoldSetValueExpression = 'home'
    BoldSelectChangeAction = bdcsSetValue
    TabOrder = 5
  end
  object BoldGrid1: TBoldGrid
    Left = 8
    Top = 400
    Width = 249
    Height = 120
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = BoldCursorHandle1
    Columns = <
      item
        BoldProperties.Expression = ''
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = ''
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        LookUpProperties.Expression = ''
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    ColWidths = (
      17
      64)
  end
  object Button1: TButton
    Left = 8
    Top = 526
    Width = 75
    Height = 25
    Action = BoldFormSaverApplyAction1
    TabOrder = 7
  end
  object Button2: TButton
    Left = 94
    Top = 526
    Width = 75
    Height = 25
    Action = BoldFormSaverOkAction1
    Default = True
    TabOrder = 8
  end
  object Button3: TButton
    Left = 182
    Top = 526
    Width = 75
    Height = 25
    Action = BoldFormSaverCancelAction1
    Cancel = True
    TabOrder = 9
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
  object BoldCaptionController1: TBoldCaptionController
    BoldHandle = brhPerson
    BoldProperties.Expression = 'name'
    Left = 160
    Top = 184
  end
  object blhOwnedResidentialBuildings: TBoldListHandle
    RootHandle = brhPerson
    Expression = 'ownedBuildings->filterOnType(Residential_Building)'
    Left = 24
    Top = 240
  end
  object BoldFormSaver1: TBoldFormSaver
    OnlyFirstDirty = False
    Left = 208
    Top = 96
  end
  object BoldCursorHandle1: TBoldCursorHandle
    RootHandle = BoldFormSaver1
    Left = 120
    Top = 456
  end
  object ActionList1: TActionList
    Left = 208
    Top = 432
    object BoldFormSaverApplyAction1: TBoldFormSaverApplyAction
      Category = 'Bold Actions'
      Caption = '&Apply'
      ShortCut = 16474
      BoldFormSaver = BoldFormSaver1
    end
    object BoldFormSaverCancelAction1: TBoldFormSaverCancelAction
      Category = 'Bold Actions'
      Caption = '&Cancel'
      BoldFormSaver = BoldFormSaver1
    end
    object BoldFormSaverOkAction1: TBoldFormSaverOkAction
      Category = 'Bold Actions'
      Caption = '&Ok'
      ShortCut = 16474
      BoldFormSaver = BoldFormSaver1
    end
  end
end
