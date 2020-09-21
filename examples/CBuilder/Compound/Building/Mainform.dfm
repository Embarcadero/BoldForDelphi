object allform: Tallform
  Left = 200
  Top = 88
  Width = 612
  Height = 485
  Caption = 'Buildings and owners'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
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
  object PersonCount: TBoldEdit
    Left = 128
    Top = 8
    Width = 73
    Height = 21
    BoldHandle = blhAllPerson
    BoldProperties.Expression = 'Person.allInstances->size'
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
  object bgrPerson: TBoldGrid
    Left = 8
    Top = 40
    Width = 193
    Height = 97
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
  object CheckBox1: TCheckBox
    Left = 8
    Top = 152
    Width = 81
    Height = 17
    Caption = 'Rich Only'
    TabOrder = 2
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 104
    Top = 152
    Width = 89
    Height = 17
    Caption = 'Alphabetically'
    TabOrder = 3
    OnClick = CheckBox2Click
  end
  object PersonGroup: TGroupBox
    Left = 208
    Top = 8
    Width = 385
    Height = 161
    Caption = 'Person'
    TabOrder = 4
    object FirstName: TLabel
      Left = 24
      Top = 16
      Width = 50
      Height = 13
      Caption = 'FirstName:'
    end
    object Label2: TLabel
      Left = 88
      Top = 16
      Width = 51
      Height = 13
      Caption = 'LastName:'
    end
    object Assets: TLabel
      Left = 24
      Top = 64
      Width = 34
      Height = 13
      Caption = 'Assets:'
    end
    object HomeAddress: TLabel
      Left = 24
      Top = 104
      Width = 72
      Height = 13
      Caption = 'Home Address:'
    end
    object OwnedBuildings: TLabel
      Left = 232
      Top = 16
      Width = 82
      Height = 13
      Caption = 'Owned Buildings:'
    end
    object bedLastName: TBoldEdit
      Left = 88
      Top = 32
      Width = 121
      Height = 21
      BoldHandle = blhAllPerson
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
      TabOrder = 1
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
      TabOrder = 2
    end
    object bcbRich: TBoldCheckBox
      Left = 104
      Top = 80
      Width = 97
      Height = 17
      BoldHandle = blhAllPerson
      BoldProperties.Renderer = DataModule1.IsRichRenderer
      Caption = 'Rich'
      ReadOnly = False
      TabOrder = 3
    end
    object bedPersonHome: TBoldEdit
      Left = 24
      Top = 120
      Width = 137
      Height = 21
      BoldHandle = behHome
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
      PopupMenu = SingleLinkPopup
      TabOrder = 4
    end
    object blbPersonOwnedBuildings: TBoldListBox
      Left = 232
      Top = 32
      Width = 145
      Height = 113
      Alignment = taLeftJustify
      BoldHandle = blhOwnedBuildingsHandle
      BoldProperties.NilElementMode = neNone
      BoldRowProperties.Expression = 'address'
      DragMode = dmAutomatic
      ItemHeight = 16
      PopupMenu = MultiLinkPopup
      TabOrder = 5
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 176
    Width = 593
    Height = 193
    ActivePage = tabBuilding
    TabIndex = 0
    TabOrder = 5
    object tabBuilding: TTabSheet
      Caption = 'Building'
      object bgrBuilding: TBoldGrid
        Left = 8
        Top = 8
        Width = 269
        Height = 153
        AddNewAtEnd = False
        BoldAutoColumns = False
        BoldShowConstraints = False
        BoldHandle = blhAllBuilding
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
          64)
      end
      object GroupBox1: TGroupBox
        Left = 280
        Top = 8
        Width = 289
        Height = 153
        Caption = 'GroupBox1'
        TabOrder = 1
        object Label3: TLabel
          Left = 16
          Top = 16
          Width = 41
          Height = 13
          Caption = 'Address:'
        end
        object Label4: TLabel
          Left = 16
          Top = 56
          Width = 36
          Height = 13
          Caption = 'Owners'
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
          BoldProperties.NilElementMode = neNone
          DragMode = dmAutomatic
          ItemHeight = 16
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
      Caption = 'Residential Building'
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
        object Label5: TLabel
          Left = 16
          Top = 16
          Width = 41
          Height = 13
          Caption = 'Address:'
        end
        object Label6: TLabel
          Left = 16
          Top = 56
          Width = 36
          Height = 13
          Caption = 'Owners'
        end
        object Label7: TLabel
          Left = 152
          Top = 56
          Width = 50
          Height = 13
          Caption = 'Residents:'
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
        object blbBuildingOwners2: TBoldListBox
          Left = 16
          Top = 72
          Width = 121
          Height = 65
          Alignment = taLeftJustify
          BoldHandle = blhOwners2
          BoldProperties.NilElementMode = neNone
          DragMode = dmAutomatic
          ItemHeight = 16
          PopupMenu = MultiLinkPopup
          TabOrder = 1
        end
        object blbBuildingResidents: TBoldListBox
          Left = 152
          Top = 72
          Width = 121
          Height = 65
          Alignment = taLeftJustify
          BoldHandle = blhResidents
          BoldProperties.NilElementMode = neNone
          BoldRowProperties.Expression = 'lastName'
          DragMode = dmAutomatic
          ItemHeight = 16
          PopupMenu = MultiLinkPopup
          TabOrder = 2
        end
        object btnChargeRent: TButton
          Left = 200
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Charge Rent'
          TabOrder = 3
          OnClick = btnChargeRentClick
        end
      end
    end
  end
  object btnUpdateDB: TButton
    Left = 8
    Top = 376
    Width = 89
    Height = 25
    Action = BoldUpdateDBAction1
    TabOrder = 6
  end
  object Button1: TButton
    Left = 408
    Top = 376
    Width = 93
    Height = 25
    Action = BoldActivateSystemAction1
    TabOrder = 7
  end
  object Button2: TButton
    Left = 512
    Top = 376
    Width = 85
    Height = 25
    Action = BoldIBDatabaseAction1
    TabOrder = 8
  end
  object pbdbNotification: TProgressBar
    Left = 112
    Top = 384
    Width = 97
    Height = 16
    Min = 0
    Max = 100
    Step = 1
    TabOrder = 9
  end
  object btnCheckpoint: TButton
    Left = 228
    Top = 376
    Width = 75
    Height = 29
    Caption = 'Checkpoint'
    TabOrder = 10
    OnClick = btnCheckpointClick
  end
  object btnUnDo: TButton
    Left = 316
    Top = 372
    Width = 75
    Height = 25
    Caption = 'Undo'
    TabOrder = 11
    OnClick = btnUnDoClick
  end
  object btnRedo: TButton
    Left = 316
    Top = 396
    Width = 75
    Height = 25
    Caption = 'Redo'
    TabOrder = 12
    OnClick = btnRedoClick
  end
  object BoldModel1: TBoldModel
    UMLModelMode = ummNone
    Boldify.EnforceDefaultUMLCase = False
    Boldify.DefaultNavigableMultiplicity = '0..1'
    Boldify.DefaultNonNavigableMultiplicity = '0..*'
    Left = 688
    Top = 32
    Model = (
      'VERSION 19'
      '(Model'
      #9'"New_Model"'
      #9'"New_ModelRoot"'
      #9'""'
      #9'""'
      #9'"Bold.DelphiName=<Name>"'
      #9'(Classes'
      #9#9'(Class'
      #9#9#9'"New_ModelRoot"'
      #9#9#9'"<NONE>"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9')'
      #9'(Associations'
      #9')'
      ')')
  end
  object blhAllPerson: TBoldListHandle
    RootHandle = DataModule1.BoldSystemHandle1
    Expression = 'Person.allInstances'
    Left = 32
    Top = 80
  end
  object PersonPopup: TPopupMenu
    OnPopup = PopupPopup
    Left = 88
    Top = 80
    object NewPerson: TMenuItem
      Caption = 'New'
      ShortCut = 45
      OnClick = NewPersonClick
    end
    object DeletePerson: TMenuItem
      Caption = 'Delete'
      ShortCut = 46
      OnClick = DeleteCurrentObjectClick
    end
    object Showinownwindow1: TMenuItem
      Caption = 'Show in own window'
      OnClick = ShowInOwnWindow
    end
  end
  object BoldPlaceableAFP1: TBoldPlaceableAFP
    OnGetFormClass = BoldPlaceableAFP1GetFormClass
    OnRetrieveHandle = BoldPlaceableAFP1RetrieveHandle
    Left = 136
    Top = 80
  end
  object behHome: TBoldExpressionHandle
    RootHandle = blhAllPerson
    Expression = 'home'
    Left = 360
    Top = 72
  end
  object blhOwnedBuildingsHandle: TBoldListHandle
    RootHandle = blhAllPerson
    Expression = 'ownedBuildings'
    Left = 544
    Top = 112
  end
  object SingleLinkPopup: TPopupMenu
    OnPopup = PopupPopup
    Left = 400
    Top = 72
    object Delete1: TMenuItem
      Caption = 'Delete'
      ShortCut = 46
      OnClick = SingleItemRemove
    end
  end
  object MultiLinkPopup: TPopupMenu
    OnPopup = PopupPopup
    Left = 400
    Top = 104
    object Delete2: TMenuItem
      Caption = 'Delete'
      ShortCut = 46
      OnClick = Remove
    end
    object Showinownwindow2: TMenuItem
      Caption = 'Show in own window'
      OnClick = ShowInOwnWindow
    end
  end
  object blhAllResidentialBuilding: TBoldListHandle
    RootHandle = DataModule1.BoldSystemHandle1
    Expression = 'Residential_Building.allInstances'
    Left = 244
    Top = 232
  end
  object blhAllBuilding: TBoldListHandle
    RootHandle = DataModule1.BoldSystemHandle1
    Expression = 'Building.allInstances'
    Left = 20
    Top = 232
  end
  object BuildingPopup: TPopupMenu
    OnPopup = PopupPopup
    Left = 52
    Top = 312
    object newBuilding: TMenuItem
      Caption = 'New'
      ShortCut = 45
      OnClick = newBuildingClick
    end
    object deleteBuilding: TMenuItem
      Caption = 'Delete'
      ShortCut = 46
      OnClick = DeleteCurrentObjectClick
    end
    object ShowinownwindowBuilding: TMenuItem
      Caption = 'Show in own window'
      OnClick = ShowInOwnWindow
    end
  end
  object HighRentRenderer: TBoldAsStringRenderer
    OnSetFont = HighRentRendererSetFont
    OnSetColor = HighRentRendererSetColor
    Left = 236
    Top = 320
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
    Left = 204
    Top = 320
  end
  object bsrResidentsTotalAssets: TBoldAsStringRenderer
    OnSubscribe = bsrResidentsTotalAssetsSubscribe
    OnGetAsString = bsrResidentsTotalAssetsGetAsString
    Left = 244
    Top = 264
  end
  object bsrAddress: TBoldAsStringRenderer
    OnSetFont = bsrAddressSetFont
    OnSetColor = bsrAddressSetColor
    Left = 52
    Top = 232
  end
  object blhOwners2: TBoldListHandle
    RootHandle = blhAllResidentialBuilding
    Expression = 'owners'
    Left = 324
    Top = 312
  end
  object blhOwners: TBoldListHandle
    RootHandle = blhAllBuilding
    Expression = 'owners'
    Left = 372
    Top = 312
  end
  object blhResidents: TBoldListHandle
    RootHandle = blhAllResidentialBuilding
    Expression = 'residents'
    Left = 532
    Top = 312
  end
  object ActionList1: TActionList
    Left = 448
    Top = 408
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = DataModule1.BoldSystemHandle1
      OnSystemOpened = BoldActivateSystemAction1SystemOpened
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before closing system?'
      SaveOnClose = saAsk
    end
    object BoldUpdateDBAction1: TBoldUpdateDBAction
      Category = 'Bold Actions'
      Caption = 'Update DB'
      BoldSystemHandle = DataModule1.BoldSystemHandle1
    end
    object BoldIBDatabaseAction1: TBoldIBDatabaseAction
      Category = 'Bold Actions'
      Caption = 'Create DB'
      BoldSystemHandle = DataModule1.BoldSystemHandle1
      Username = 'SYSDBA'
      Password = 'masterkey'
    end
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
end
