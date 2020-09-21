object frmMain: TfrmMain
  Left = 252
  Top = 209
  Width = 932
  Height = 666
  Caption = 'frmMain'
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
    Left = 16
    Top = 8
    Width = 66
    Height = 13
    Caption = 'All Companies'
  end
  object Label2: TLabel
    Left = 424
    Top = 6
    Width = 52
    Height = 13
    Caption = 'All Persons'
  end
  object Label3: TLabel
    Left = 16
    Top = 195
    Width = 80
    Height = 13
    Caption = 'Company Offices'
  end
  object Label10: TLabel
    Left = 428
    Top = 312
    Width = 60
    Height = 13
    Caption = 'Departments'
  end
  object Label11: TLabel
    Left = 424
    Top = 181
    Width = 51
    Height = 13
    Caption = 'Employees'
  end
  object Label12: TLabel
    Left = 253
    Top = 17
    Width = 24
    Height = 13
    Caption = 'Logo'
  end
  object grdCompany: TBoldGrid
    Left = 16
    Top = 27
    Width = 217
    Height = 120
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhAllCompany
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
        BoldProperties.Expression = 'companyName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'CompanyName'
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
      193)
  end
  object grdPerson: TBoldGrid
    Left = 424
    Top = 25
    Width = 241
    Height = 120
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhAllPerson
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
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      64
      64)
  end
  object BoldNavigator1: TBoldNavigator
    Left = 16
    Top = 155
    Width = 192
    Height = 25
    BoldHandle = blhAllCompany
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
    UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
    RemoveQuestion = 'Remove "%1:s" from the list?'
  end
  object BoldNavigator2: TBoldNavigator
    Left = 424
    Top = 153
    Width = 192
    Height = 25
    BoldHandle = blhAllPerson
    TabOrder = 3
    ImageIndices.nbFirst = -1
    ImageIndices.nbPrior = -1
    ImageIndices.nbNext = -1
    ImageIndices.nbLast = -1
    ImageIndices.nbInsert = -1
    ImageIndices.nbDelete = -1
    ImageIndices.nbMoveUp = -1
    ImageIndices.nbMoveDown = -1
    DeleteQuestion = 'Delete object?'
    UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
    RemoveQuestion = 'Remove "%1:s" from the list?'
  end
  object Button1: TButton
    Left = 624
    Top = 602
    Width = 75
    Height = 25
    Action = BoldActivateSystemAction1
    TabOrder = 4
  end
  object Button2: TButton
    Left = 720
    Top = 602
    Width = 75
    Height = 25
    Action = BoldUpdateDBAction1
    TabOrder = 5
  end
  object BoldGrid1: TBoldGrid
    Left = 16
    Top = 219
    Width = 369
    Height = 113
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhCompanyOffices
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
        BoldProperties.Expression = 'name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Name'
      end
      item
        BoldProperties.Expression = 'establishedDate'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'EstablishedDate'
      end
      item
        BoldProperties.Expression = 'phone'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        BoldProperties.Expression = 'fax'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end
      item
        BoldProperties.Expression = 'email'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Email'
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 6
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      64
      84
      64
      64
      64)
  end
  object BoldNavigator3: TBoldNavigator
    Left = 16
    Top = 339
    Width = 192
    Height = 25
    BoldHandle = blhCompanyOffices
    TabOrder = 7
    ImageIndices.nbFirst = -1
    ImageIndices.nbPrior = -1
    ImageIndices.nbNext = -1
    ImageIndices.nbLast = -1
    ImageIndices.nbInsert = -1
    ImageIndices.nbDelete = -1
    ImageIndices.nbMoveUp = -1
    ImageIndices.nbMoveDown = -1
    DeleteQuestion = 'Delete object?'
    UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
    RemoveQuestion = 'Remove "%1:s" from the list?'
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 428
    Width = 393
    Height = 161
    Caption = 'Office Address'
    TabOrder = 8
    object Label4: TLabel
      Left = 8
      Top = 47
      Width = 28
      Height = 13
      Caption = 'Street'
    end
    object Label5: TLabel
      Left = 135
      Top = 47
      Width = 17
      Height = 13
      Caption = 'City'
    end
    object Label6: TLabel
      Left = 8
      Top = 103
      Width = 36
      Height = 13
      Caption = 'Country'
    end
    object Label7: TLabel
      Left = 138
      Top = 104
      Width = 49
      Height = 13
      Caption = 'Post Code'
    end
    object Label8: TLabel
      Left = 261
      Top = 49
      Width = 43
      Height = 13
      Caption = 'Zip Code'
    end
    object Label9: TLabel
      Left = 261
      Top = 106
      Width = 25
      Height = 13
      Caption = 'State'
    end
    object BoldEdit1: TBoldEdit
      Left = 8
      Top = 65
      Width = 121
      Height = 21
      BoldHandle = behAddress
      BoldProperties.Expression = 'street'
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
      Left = 8
      Top = 120
      Width = 121
      Height = 21
      BoldHandle = behAddress
      BoldProperties.Expression = 'country'
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
      Left = 134
      Top = 65
      Width = 121
      Height = 21
      BoldHandle = behAddress
      BoldProperties.Expression = 'city'
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
    object BoldEdit4: TBoldEdit
      Left = 135
      Top = 120
      Width = 121
      Height = 21
      BoldHandle = behAddress
      BoldProperties.Expression = 'postcode'
      ReadOnly = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Alignment = taLeftJustify
      ButtonStyle = bbsNone
      MaxLength = 0
      TabOrder = 3
    end
    object BoldEdit5: TBoldEdit
      Left = 260
      Top = 65
      Width = 121
      Height = 21
      BoldHandle = behAddress
      BoldProperties.Expression = 'zip'
      ReadOnly = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Alignment = taLeftJustify
      ButtonStyle = bbsNone
      MaxLength = 0
      TabOrder = 4
    end
    object BoldEdit6: TBoldEdit
      Left = 260
      Top = 120
      Width = 121
      Height = 21
      BoldHandle = behAddress
      BoldProperties.Expression = 'state'
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
  end
  object btnAddAddress: TButton
    Left = 337
    Top = 594
    Width = 75
    Height = 25
    Action = AddAddressAction
    Caption = 'New Address'
    TabOrder = 9
  end
  object BoldGrid2: TBoldGrid
    Left = 424
    Top = 329
    Width = 225
    Height = 136
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhDepartments
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
        BoldProperties.Expression = 'name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Name'
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 10
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      188)
  end
  object BoldNavigator4: TBoldNavigator
    Left = 424
    Top = 471
    Width = 192
    Height = 25
    BoldHandle = blhDepartments
    TabOrder = 11
    ImageIndices.nbFirst = -1
    ImageIndices.nbPrior = -1
    ImageIndices.nbNext = -1
    ImageIndices.nbLast = -1
    ImageIndices.nbInsert = -1
    ImageIndices.nbDelete = -1
    ImageIndices.nbMoveUp = -1
    ImageIndices.nbMoveDown = -1
    DeleteQuestion = 'Delete object?'
    UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
    RemoveQuestion = 'Remove "%1:s" from the list?'
  end
  object grdEmployees: TBoldGrid
    Left = 424
    Top = 199
    Width = 321
    Height = 110
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhEmployees
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
        BoldProperties.Expression = 'fullName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'FullName'
      end
      item
        BoldProperties.Expression = 'employment->first.title'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Title'
      end
      item
        BoldProperties.Expression = 'employment->first.email'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'email'
      end
      item
        BoldProperties.Expression = 'employment.department->first.name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Department'
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 12
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      64
      64
      64
      87)
  end
  object biLogo: TBoldImage
    Left = 248
    Top = 35
    Width = 121
    Height = 111
    BoldHandle = blhAllCompany
    BoldProperties.Expression = 'logo'
    AutoSize = True
    StretchMode = bsmNoStretch
    Center = False
    QuickDraw = False
    TabOrder = 13
  end
  object Button3: TButton
    Left = 296
    Top = 152
    Width = 75
    Height = 25
    Action = LoadLogFromFile
    TabOrder = 14
  end
  object Button5: TButton
    Left = 817
    Top = 602
    Width = 75
    Height = 25
    Action = BoldIBDatabaseAction1
    TabOrder = 15
  end
  object Button6: TButton
    Left = 280
    Top = 338
    Width = 105
    Height = 25
    Caption = 'Generate Data'
    TabOrder = 16
    OnClick = Button6Click
  end
  object BoldGrid3: TBoldGrid
    Left = 680
    Top = 24
    Width = 225
    Height = 120
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhFiltered
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
        BoldProperties.Expression = 'companyName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'CompanyName'
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 17
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      64)
  end
  object BoldGrid4: TBoldGrid
    Left = 664
    Top = 328
    Width = 249
    Height = 137
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhSearchPerson
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
      end
      item
        BoldProperties.Expression = 'fullName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'FullName'
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 18
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
  object blhAllCompany: TBoldListHandle
    RootHandle = dmMain.OrgChartSystem
    Expression = 'Company.allInstances'
    Left = 168
    Top = 51
  end
  object blhAllPerson: TBoldListHandle
    RootHandle = dmMain.OrgChartSystem
    Expression = 'Person.allInstances'
    Left = 536
    Top = 49
  end
  object ActionList1: TActionList
    Left = 688
    Top = 160
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = dmMain.OrgChartSystem
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before closing system?'
      SaveOnClose = saAsk
    end
    object BoldUpdateDBAction1: TBoldUpdateDBAction
      Category = 'Bold Actions'
      Caption = 'Update DB'
      BoldSystemHandle = dmMain.OrgChartSystem
    end
    object AddAddressAction: TAction
      Category = 'Bold Actions'
      Caption = 'Add Address'
      OnExecute = AddAddressActionExecute
      OnUpdate = AddAddressActionUpdate
    end
    object LoadLogFromFile: TAction
      Category = 'Bold Actions'
      Caption = 'Load from file'
      OnExecute = LoadLogFromFileExecute
    end
    object BoldIBDatabaseAction1: TBoldIBDatabaseAction
      Category = 'Bold Actions'
      Caption = 'Create DB'
      BoldSystemHandle = dmMain.OrgChartSystem
      Username = 'SYSDBA'
      Password = 'masterkey'
    end
  end
  object blhCompanyOffices: TBoldListHandle
    RootHandle = blhAllCompany
    Expression = 'offices'
    Left = 136
    Top = 243
  end
  object behAddress: TBoldExpressionHandle
    RootHandle = blhCompanyOffices
    Expression = 'address'
    Left = 312
    Top = 392
  end
  object blhDepartments: TBoldListHandle
    RootHandle = blhCompanyOffices
    Expression = 'departments'
    Left = 552
    Top = 409
  end
  object blhEmployees: TBoldListHandle
    RootHandle = blhAllCompany
    Expression = 'employees'
    Left = 472
    Top = 263
  end
  object BoldPlaceableAFP1: TBoldPlaceableAFP
    Left = 16
    Top = 376
  end
  object dlgSelectFile: TOpenDialog
    DefaultExt = 'bmp'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a logo'
    Left = 256
    Top = 152
  end
  object blhFiltered: TBoldListHandle
    RootHandle = dmMain.OrgChartSystem
    Expression = 
      'Company.allInstances->select(companyName.sqlLIkeCaseInsensitive(' +
      #39'%BoldSoft%'#39'))'
    Left = 864
    Top = 72
  end
  object blhSearchPerson: TBoldListHandle
    RootHandle = dmMain.OrgChartSystem
    Expression = 
      'Person.allInstances->select(firstName.sqlLIkeCaseInsensitive('#39'%M' +
      'ary%'#39') and lastName.sqlLIkeCaseInsensitive('#39'%Ananian%'#39'))'
    Left = 872
    Top = 424
  end
end
