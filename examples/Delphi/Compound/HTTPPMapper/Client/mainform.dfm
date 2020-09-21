object allform: Tallform
  Left = -2
  Top = 51
  Width = 612
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
    object bedFirstName: TBoldEdit
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
    object bedPersonHome: TBoldEdit
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
    object blbPersonOwnedBuildings: TBoldListBox
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
    object bedLastName: TBoldEdit
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
    object bedAssets: TBoldEdit
      Left = 24
      Top = 80
      Width = 65
      Height = 21
      BoldHandle = blhAllPerson
      BoldProperties.Expression = 'assets'
      BoldProperties.Renderer = DataModule1.NegativeRedRenderer
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
    object bcbRich: TBoldCheckBox
      Left = 104
      Top = 80
      Width = 57
      Height = 17
      BoldHandle = blhAllPerson
      BoldProperties.Renderer = DataModule1.IsRichRenderer
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
    Action = BoldUpdateDBAction1
    TabOrder = 1
  end
  object PersonCount: TBoldEdit
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
  object bgrPerson: TBoldGrid
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
    Step = 1
    TabOrder = 6
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 176
    Width = 593
    Height = 193
    ActivePage = tabResidentialBuilding
    TabOrder = 7
    object tabBuilding: TTabSheet
      Caption = 'Buildings'
      object bgrBuilding: TBoldGrid
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
            BoldProperties.Renderer = bsrAddress
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
        object bedAddress: TBoldEdit
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
        object blbBuildingOwners: TBoldListBox
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
      object bgrResidentialBuilding: TBoldGrid
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
            BoldProperties.Renderer = bsrAddress
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
          end
          item
            BoldProperties.Renderer = bsrRentPerResident
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Rent/Resident'
          end
          item
            BoldProperties.Expression = 'if ocliskindof(Residential_Building) then totalRent else 0 endif'
            BoldProperties.Renderer = HighRentRenderer
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Rent'
          end
          item
            BoldProperties.Expression = 'residents.Assets->Sum '
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
        object bedAddress2: TBoldEdit
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
        object blbBuildingResidents: TBoldListBox
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
        object blbBuildingOwners2: TBoldListBox
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
    Left = 464
    Top = 376
    Width = 109
    Height = 25
    Action = BoldActivateSystemAction1
    TabOrder = 8
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
  object blhAllPerson: TBoldListHandle
    RootHandle = DataModule1.BoldSystemHandle1
    Expression = 'Person.allInstances'
    Left = 24
    Top = 88
  end
  object blhAllBuilding: TBoldListHandle
    RootHandle = DataModule1.BoldSystemHandle1
    Expression = 'Building.allInstances'
    Left = 232
    Top = 312
  end
  object blhOwnedBuildingsHandle: TBoldListHandle
    RootHandle = blhAllPerson
    Expression = 'ownedBuildings'
    Left = 536
    Top = 112
  end
  object blhOwners: TBoldListHandle
    RootHandle = blhAllBuilding
    Expression = 'owners'
    Left = 392
    Top = 312
  end
  object behHome: TBoldExpressionHandle
    RootHandle = blhAllPerson
    Expression = 'home'
    Left = 368
    Top = 88
  end
  object bsrRentPerResident: TBoldAsStringRenderer
    OnMayModify = bsrRentPerResidentMayModify
    OnHoldsChangedValue = bsrRentPerResidentHoldsChangedValue
    OnReleaseChangedValue = bsrRentPerResidentReleaseChangedValue
    OnSubscribe = bsrRentPerResidentSubscribe
    OnGetAsString = bsrRentPerResidentGetAsString
    OnSetAsString = bsrRentPerResidentSetAsString
    OnValidateCharacter = bsrRentPerResidentValidateCharacter
    OnValidateString = bsrRentPerResidentValidateString
    Left = 116
    Top = 324
  end
  object bsrResidentsTotalAssets: TBoldAsStringRenderer
    OnSubscribe = bsrResidentsTotalAssetsSubscribe
    OnGetAsString = bsrResidentsTotalAssetsGetAsString
    Left = 148
    Top = 324
  end
  object bsrAddress: TBoldAsStringRenderer
    OnSetFont = bsrAddressSetFont
    OnSetColor = bsrAddressSetColor
    Left = 180
    Top = 324
  end
  object blhResidents: TBoldListHandle
    RootHandle = blhAllResidentialBuilding
    Expression = 'residents'
    Left = 528
    Top = 312
  end
  object blhAllResidentialBuilding: TBoldListHandle
    RootHandle = DataModule1.BoldSystemHandle1
    Expression = 'Residential_Building.allInstances'
    Left = 216
    Top = 256
  end
  object blhOwners2: TBoldListHandle
    RootHandle = blhAllResidentialBuilding
    Expression = 'owners'
    Left = 360
    Top = 312
  end
  object BoldPersistenceProgressNotifier1: TBoldPersistenceProgressNotifier
    ProgressBar = pbdbNotification
    AnimationInterval = 100
    MsgFetchObjects = 'Fetching objects'
    MsgRetrieveIds = 'Retrieving object ids'
    MsgUpdateDatabase = 'Updating database'
    Left = 120
    Top = 376
  end
  object ActionList1: TActionList
    Left = 308
    Top = 376
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = DataModule1.BoldSystemHandle1
      OnSystemOpened = BoldActivateSystemAction1SystemOpened
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before exit?'
      SaveOnClose = saAsk
    end
    object BoldUpdateDBAction1: TBoldUpdateDBAction
      Category = 'Bold Actions'
      Caption = 'Update DB'
      BoldSystemHandle = DataModule1.BoldSystemHandle1
    end
  end
  object HighRentRenderer: TBoldAsStringRenderer
    OnSetFont = HighRentRendererSetFont
    OnSetColor = HighRentRendererSetColor
    Left = 108
    Top = 248
  end
  object BoldPlaceableAFP1: TBoldPlaceableAFP
    Left = 96
    Top = 64
  end
end
