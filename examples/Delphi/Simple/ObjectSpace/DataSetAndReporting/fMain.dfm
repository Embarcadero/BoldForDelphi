object frmMain: TfrmMain
  Left = 0
  Top = 0
  Width = 480
  Height = 389
  Caption = 'frmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 4
    Top = 4
    Width = 38
    Height = 13
    Caption = 'Persons'
  end
  object Label2: TLabel
    Left = 4
    Top = 172
    Width = 42
    Height = 13
    Caption = 'Buildings'
  end
  object Label3: TLabel
    Left = 328
    Top = 104
    Width = 28
    Height = 13
    Caption = 'Home'
  end
  object Label4: TLabel
    Left = 328
    Top = 192
    Width = 47
    Height = 13
    Caption = 'Residents'
  end
  object BoldGrid1: TBoldGrid
    Left = 4
    Top = 16
    Width = 320
    Height = 120
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhPersons
    BoldProperties.InternalDrag = False
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
        BoldProperties.Expression = 'firstName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'FirstName'
      end
      item
        BoldProperties.Expression = 'lastName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'LastName'
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      101
      194)
  end
  object btnGenerateReport: TButton
    Left = 368
    Top = 312
    Width = 97
    Height = 25
    Caption = 'Generate Report'
    TabOrder = 1
    OnClick = btnGenerateReportClick
  end
  object BoldNavigator1: TBoldNavigator
    Left = 4
    Top = 140
    Width = 144
    Height = 25
    BoldHandle = blhPersons
    TabOrder = 2
    ImageIndices.nbFirst = -1
    ImageIndices.nbPrior = -1
    ImageIndices.nbNext = -1
    ImageIndices.nbLast = -1
    ImageIndices.nbInsert = -1
    ImageIndices.nbDelete = -1
    ImageIndices.nbMoveUp = -1
    ImageIndices.nbMoveDown = -1
    DeleteQuestion = 'Delete object?'
  end
  object BoldGrid2: TBoldGrid
    Left = 4
    Top = 184
    Width = 320
    Height = 120
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhBuildings
    BoldProperties.InternalDrag = False
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
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      64
      232)
  end
  object BoldNavigator2: TBoldNavigator
    Left = 4
    Top = 308
    Width = 144
    Height = 25
    BoldHandle = blhBuildings
    TabOrder = 4
    ImageIndices.nbFirst = -1
    ImageIndices.nbPrior = -1
    ImageIndices.nbNext = -1
    ImageIndices.nbLast = -1
    ImageIndices.nbInsert = -1
    ImageIndices.nbDelete = -1
    ImageIndices.nbMoveUp = -1
    ImageIndices.nbMoveDown = -1
    DeleteQuestion = 'Delete object?'
  end
  object BoldEdit1: TBoldEdit
    Left = 328
    Top = 116
    Width = 137
    Height = 21
    BoldHandle = blhPersonHome
    BoldProperties.DropMode = bdpReplace
    BoldProperties.Expression = 'address'
    ReadOnly = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Alignment = taLeftJustify
    ButtonStyle = bbsNone
    MaxLength = 0
    TabOrder = 5
  end
  object BoldListBox1: TBoldListBox
    Left = 328
    Top = 204
    Width = 137
    Height = 101
    Alignment = taLeftJustify
    BoldHandle = blhResidents
    BoldProperties.InternalDrag = False
    BoldProperties.NilElementMode = neNone
    BoldRowProperties.Expression = 'firstName + '#39' '#39' + lastName'
    DragMode = dmAutomatic
    ItemHeight = 16
    TabOrder = 6
  end
  object MainMenu1: TMainMenu
    Left = 272
    Top = 4
    object File1: TMenuItem
      Caption = 'Open system'
      object UpdateDB1: TMenuItem
        Action = dmMain.BoldUpdateDBAction1
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Debug1: TMenuItem
      Caption = 'Debug'
      object SystemDebugger1: TMenuItem
        Caption = 'SystemDebugger'
        OnClick = SystemDebugger1Click
      end
    end
    object N1: TMenuItem
      Caption = '?'
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object blhPersons: TBoldListHandle
    RootHandle = dmMain.bshMain
    Expression = 'Person.allInstances'
    Left = 124
    Top = 60
  end
  object blhBuildings: TBoldListHandle
    RootHandle = dmMain.bshMain
    Expression = 'Building.allInstances'
    Left = 140
    Top = 224
  end
  object blhPersonHome: TBoldExpressionHandle
    RootHandle = blhPersons
    Expression = 'home'
    Left = 396
    Top = 108
  end
  object blhResidents: TBoldListHandle
    RootHandle = blhBuildings
    Expression = 'residents'
    Left = 368
    Top = 228
  end
end
