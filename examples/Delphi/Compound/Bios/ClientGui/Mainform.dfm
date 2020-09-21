object allform: Tallform
  Left = 338
  Top = 113
  Width = 615
  Height = 450
  Caption = 'Buildings and owners'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 65
    Height = 13
    Caption = 'All Persons #:'
  end
  object Label8: TLabel
    Left = 352
    Top = 376
    Width = 250
    Height = 13
    Caption = 'Make sure to create a datbase on the server console'
  end
  object Label10: TLabel
    Left = 352
    Top = 392
    Width = 212
    Height = 13
    Caption = 'and activating the system before connecting.'
  end
  object PersonGroup: TGroupBox
    Left = 208
    Top = 8
    Width = 385
    Height = 161
    Caption = 'Person'
    TabOrder = 0
    object Label2: TLabel
      Left = 24
      Top = 104
      Width = 69
      Height = 13
      Caption = 'Home Address'
    end
    object Label3: TLabel
      Left = 232
      Top = 16
      Width = 79
      Height = 13
      Caption = 'Owned Buildings'
    end
    object FirstName: TLabel
      Left = 24
      Top = 16
      Width = 47
      Height = 13
      Caption = 'FirstName'
    end
    object Label6: TLabel
      Left = 88
      Top = 16
      Width = 48
      Height = 13
      Caption = 'LastName'
    end
    object Label7: TLabel
      Left = 24
      Top = 64
      Width = 31
      Height = 13
      Caption = 'Assets'
    end
    object bedFirstName: TBoldEditCom
      Left = 24
      Top = 32
      Width = 57
      Height = 21
      BoldHandle = blhAllPerson
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
    object bedPersonHome: TBoldEditCom
      Left = 24
      Top = 120
      Width = 137
      Height = 21
      BoldHandle = behHome
      BoldProperties.DropMode = bdpReplace
      BoldProperties.Expression = 'address'
      ReadOnly = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Alignment = taLeftJustify
      ButtonStyle = bbsNone
      MaxLength = 0
      PopupMenu = SingleLinkPopup
      TabOrder = 1
    end
    object blbPersonOwnedBuildings: TBoldListBoxCom
      Left = 232
      Top = 32
      Width = 145
      Height = 113
      Alignment = taLeftJustify
      BoldHandle = blhOwnedBuildingsHandle
      BoldProperties.InternalDrag = False
      BoldProperties.NilElementMode = neNone
      BoldRowProperties.Expression = 'address'
      DragMode = dmAutomatic
      ItemHeight = 13
      PopupMenu = MultiLinkPopup
      TabOrder = 2
    end
    object bedLastName: TBoldEditCom
      Left = 88
      Top = 32
      Width = 121
      Height = 21
      BoldHandle = blhAllPerson
      BoldProperties.Expression = 'lastName'
      BoldProperties.ApplyPolicy = bapChange
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
    object bedAssets: TBoldEditCom
      Left = 24
      Top = 80
      Width = 65
      Height = 21
      BoldHandle = blhAllPerson
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
      TabOrder = 4
    end
    object bcbRich: TBoldCheckBoxCom
      Left = 104
      Top = 80
      Width = 57
      Height = 17
      BoldHandle = blhAllPerson
      BoldProperties.Renderer = DataModule2.IsRichRenderer
      Caption = 'Rich'
      ReadOnly = False
      TabOrder = 5
    end
  end
  object btnUpdateDB: TButton
    Left = 4
    Top = 376
    Width = 89
    Height = 25
    Caption = 'Update Database'
    TabOrder = 1
    OnClick = btnUpdateDBClick
  end
  object PersonCount: TBoldEditCom
    Left = 128
    Top = 16
    Width = 73
    Height = 21
    BoldHandle = blhAllPerson
    BoldProperties.Expression = 'Person.allInstances->size'
    ReadOnly = True
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
  object CheckBox1: TCheckBox
    Left = 8
    Top = 152
    Width = 65
    Height = 17
    Caption = 'Rich only'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 80
    Top = 152
    Width = 97
    Height = 17
    Caption = 'Alphabetically'
    TabOrder = 4
    OnClick = CheckBox2Click
  end
  object bgrPerson: TBoldGridCom
    Left = 8
    Top = 40
    Width = 193
    Height = 97
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhAllPerson
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
        BoldProperties.Expression = 'lastName + '#39', '#39'+firstName'
        BoldProperties.Renderer = DataModule2.FullNameRenderer
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Full Name'
      end
      item
        BoldProperties.Expression = 'assets'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    PopupMenu = PersonPopup
    TabOrder = 5
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      87
      53)
  end
  object pbdbNotification: TProgressBar
    Left = 104
    Top = 384
    Width = 105
    Height = 16
    Min = 0
    Max = 100
    Step = 1
    TabOrder = 6
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 176
    Width = 593
    Height = 193
    ActivePage = tabBuilding
    TabOrder = 7
    object tabBuilding: TTabSheet
      Caption = 'Buildings'
      object bgrBuilding: TBoldGridCom
        Left = 8
        Top = 8
        Width = 269
        Height = 153
        AddNewAtEnd = False
        BoldAutoColumns = False
        BoldShowConstraints = False
        BoldHandle = blhAllBuilding
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
            BoldProperties.Expression = 'address'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
          end>
        DefaultRowHeight = 17
        EnableColAdjust = False
        PopupMenu = BuildingPopup
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        ColWidths = (
          17
          208)
      end
      object GroupBox1: TGroupBox
        Left = 288
        Top = 8
        Width = 289
        Height = 153
        Caption = 'Building'
        TabOrder = 1
        object Label4: TLabel
          Left = 16
          Top = 56
          Width = 36
          Height = 13
          Caption = 'Owners'
        end
        object Label9: TLabel
          Left = 16
          Top = 16
          Width = 38
          Height = 13
          Caption = 'Address'
        end
        object bedAddress: TBoldEditCom
          Left = 16
          Top = 32
          Width = 121
          Height = 21
          BoldHandle = blhAllBuilding
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
          TabOrder = 0
        end
        object blbBuildingOwners: TBoldListBoxCom
          Left = 16
          Top = 72
          Width = 121
          Height = 65
          Alignment = taLeftJustify
          BoldHandle = blhOwners
          BoldProperties.InternalDrag = False
          BoldProperties.NilElementMode = neNone
          DragMode = dmAutomatic
          ItemHeight = 13
          PopupMenu = MultiLinkPopup
          TabOrder = 1
        end
        object rgNameRepresentation: TRadioGroup
          Left = 152
          Top = 64
          Width = 129
          Height = 73
          Caption = 'Name representation'
          ItemIndex = 1
          Items.Strings = (
            'First Name'
            'Last Name'
            'Full Name')
          TabOrder = 2
          OnClick = rgNameRepresentationClick
        end
      end
    end
    object tabResidentialBuilding: TTabSheet
      Caption = 'Residential Buildings'
      ImageIndex = 1
      object bgrResidentialBuilding: TBoldGridCom
        Left = 8
        Top = 8
        Width = 269
        Height = 153
        AddNewAtEnd = False
        BoldAutoColumns = False
        BoldShowConstraints = False
        BoldHandle = blhAllResidentialBuilding
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
            BoldProperties.Expression = 'address'
            BoldProperties.Renderer = DataModule2.bsrAddress
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
          end
          item
            BoldProperties.Renderer = DataModule2.bsrRentPerResident
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Rent/Resident'
          end
          item
            BoldProperties.Expression = 'if ocliskindof(Residential_Building) then totalRent else 0 endif'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Rent'
          end
          item
            BoldProperties.Expression = 'residents.Assets->Sum '
            BoldProperties.Renderer = DataModule2.bsrResidentsTotalAssets
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'tot $'
          end>
        DefaultRowHeight = 17
        EnableColAdjust = False
        PopupMenu = BuildingPopup
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        ColWidths = (
          17
          79
          70
          37
          42)
      end
      object BuildingGroup: TGroupBox
        Left = 288
        Top = 8
        Width = 289
        Height = 153
        Caption = 'Building'
        TabOrder = 1
        object Owners: TLabel
          Left = 16
          Top = 56
          Width = 36
          Height = 13
          Caption = 'Owners'
        end
        object Resi: TLabel
          Left = 152
          Top = 56
          Width = 47
          Height = 13
          Caption = 'Residents'
        end
        object Label5: TLabel
          Left = 16
          Top = 16
          Width = 38
          Height = 13
          Caption = 'Address'
        end
        object bedAddress2: TBoldEditCom
          Left = 16
          Top = 32
          Width = 121
          Height = 21
          BoldHandle = blhAllResidentialBuilding
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
          TabOrder = 0
        end
        object blbBuildingResidents: TBoldListBoxCom
          Left = 152
          Top = 72
          Width = 121
          Height = 65
          Alignment = taLeftJustify
          BoldHandle = blhResidents
          BoldProperties.InternalDrag = False
          BoldProperties.NilElementMode = neNone
          BoldRowProperties.Expression = 'lastName'
          DragMode = dmAutomatic
          ItemHeight = 13
          PopupMenu = MultiLinkPopup
          TabOrder = 1
        end
        object blbBuildingOwners2: TBoldListBoxCom
          Left = 16
          Top = 72
          Width = 121
          Height = 65
          Alignment = taLeftJustify
          BoldHandle = blhOwners2
          BoldProperties.InternalDrag = False
          BoldProperties.NilElementMode = neNone
          DragMode = dmAutomatic
          ItemHeight = 13
          PopupMenu = MultiLinkPopup
          TabOrder = 2
        end
        object btnChargeRent: TButton
          Left = 200
          Top = 32
          Width = 73
          Height = 25
          Caption = 'Charge Rent'
          TabOrder = 3
          OnClick = btnChargeRentClick
        end
      end
    end
  end
  object Button1: TButton
    Left = 272
    Top = 376
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 8
    OnClick = Button1Click
  end
  object BuildingPopup: TPopupMenu
    OnPopup = PopupPopup
    Left = 84
    Top = 324
    object newBuilding: TMenuItem
      Caption = 'New '
      ShortCut = 45
      OnClick = newBuildingClick
    end
    object Delete1: TMenuItem
      Caption = 'Delete'
      ShortCut = 46
      OnClick = DeleteCurrentObject
    end
    object Showinownwindow2: TMenuItem
      Caption = 'Show in own window'
      OnClick = ShowInOwnWindow
    end
  end
  object PersonPopup: TPopupMenu
    OnPopup = PopupPopup
    Left = 136
    Top = 96
    object NewPerson: TMenuItem
      Caption = 'new'
      ShortCut = 45
      OnClick = NewPersonClick
    end
    object DeletePerson: TMenuItem
      Caption = 'Delete'
      ShortCut = 46
      OnClick = DeleteCurrentObject
    end
    object Showinownwindow1: TMenuItem
      Caption = 'Show in own window'
      OnClick = ShowInOwnWindow
    end
  end
  object SingleLinkPopup: TPopupMenu
    OnPopup = PopupPopup
    Left = 400
    Top = 88
    object MenuItem5: TMenuItem
      Caption = 'Delete'
      ShortCut = 46
      OnClick = SingleItemRemove
    end
  end
  object MultiLinkPopup: TPopupMenu
    OnPopup = PopupPopup
    Left = 400
    Top = 128
    object MenuItem1: TMenuItem
      Caption = 'Delete'
      ShortCut = 46
      OnClick = Remove
    end
    object ShowInOwnWindow3: TMenuItem
      Caption = 'Show in own window'
      OnClick = ShowInOwnWindow
    end
  end
  object blhAllPerson: TBoldListHandleCom
    RootHandle = DMClientSystem.SystemHandle
    Expression = 'Person.allInstances'
    EvaluateInPS = False
    Left = 24
    Top = 88
  end
  object blhAllBuilding: TBoldListHandleCom
    RootHandle = DMClientSystem.SystemHandle
    Expression = 'Building.allInstances'
    EvaluateInPS = False
    Left = 208
    Top = 312
  end
  object blhOwnedBuildingsHandle: TBoldListHandleCom
    RootHandle = blhAllPerson
    Expression = 'ownedBuildings'
    EvaluateInPS = False
    Left = 536
    Top = 112
  end
  object blhOwners: TBoldListHandleCom
    RootHandle = blhAllBuilding
    Expression = 'owners'
    EvaluateInPS = False
    Left = 392
    Top = 312
  end
  object behHome: TBoldExpressionHandleCom
    RootHandle = blhAllPerson
    Expression = 'home'
    EvaluateInPS = False
    Left = 368
    Top = 88
  end
  object blhResidents: TBoldListHandleCom
    RootHandle = blhAllResidentialBuilding
    Expression = 'residents'
    EvaluateInPS = False
    Left = 528
    Top = 312
  end
  object blhAllResidentialBuilding: TBoldListHandleCom
    RootHandle = DMClientSystem.SystemHandle
    Expression = 'Residential_Building.allInstances'
    EvaluateInPS = False
    Left = 248
    Top = 320
  end
  object blhOwners2: TBoldListHandleCom
    RootHandle = blhAllResidentialBuilding
    Expression = 'owners'
    EvaluateInPS = False
    Left = 360
    Top = 312
  end
  object BoldPersistenceProgressNotifier1: TBoldPersistenceProgressNotifier
    ProgressBar = pbdbNotification
    AnimationInterval = 100
    MsgFetchObjects = 'Fetching objects'
    MsgRetrieveIds = 'Retrieving object ids'
    MsgUpdateDatabase = 'Updating database'
    Left = 124
    Top = 380
  end
end
