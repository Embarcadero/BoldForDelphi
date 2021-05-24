object allform: Tallform
  Left = 65
  Top = 67
  ClientHeight = 576
  ClientWidth = 601
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
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
      BoldProperties.DropMode = bdpInsert
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
      BoldProperties.Expression = ''
      BoldProperties.Renderer = DataModule1.IsRichRenderer
      Caption = 'Rich'
      ReadOnly = False
      TabOrder = 5
    end
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
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 152
    Width = 65
    Height = 17
    Caption = 'Rich only'
    TabOrder = 2
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 80
    Top = 152
    Width = 97
    Height = 17
    Caption = 'Alphabetically'
    TabOrder = 3
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
        BoldProperties.Expression = 'lastName + '#39', '#39'+firstName'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Full Name'
        LookUpProperties.Expression = ''
      end
      item
        BoldProperties.Expression = 'assets'
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
    PopupMenu = PersonPopup
    TabOrder = 4
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
  object PageControl1: TPageControl
    Left = 3
    Top = 224
    Width = 593
    Height = 225
    ActivePage = tabResidentialBuilding
    TabOrder = 5
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
            BoldProperties.Expression = 'address'
            BoldProperties.Renderer = bsrAddress
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
          BoldRowProperties.Expression = ''
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
      object BoldNavigator2: TBoldNavigator
        Left = 3
        Top = 167
        Width = 192
        Height = 25
        BoldHandle = blhAllBuilding
        TabOrder = 2
        ImageIndices.nbFirst = -1
        ImageIndices.nbPrior = -1
        ImageIndices.nbNext = -1
        ImageIndices.nbLast = -1
        ImageIndices.nbInsert = -1
        ImageIndices.nbDelete = -1
        ImageIndices.nbMoveUp = -1
        ImageIndices.nbMoveDown = -1
        DeleteQuestion = 'Delete "%1:s"?'
        UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
        RemoveQuestion = 'Remove "%1:s" from the list?'
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
            BoldProperties.Expression = 'address'
            BoldProperties.Renderer = bsrAddress
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            LookUpProperties.Expression = ''
          end
          item
            BoldProperties.Expression = ''
            BoldProperties.Renderer = bsrRentPerResident
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Rent/Resident'
            LookUpProperties.Expression = ''
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
            LookUpProperties.Expression = ''
          end
          item
            BoldProperties.Expression = 'residents.Assets->Sum '
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'tot $'
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
          BoldProperties.DropMode = bdpInsert
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
          BoldProperties.DropMode = bdpInsert
          BoldRowProperties.Expression = ''
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
          Action = actChargeRent
          TabOrder = 3
        end
      end
      object BoldNavigator3: TBoldNavigator
        Left = 3
        Top = 167
        Width = 192
        Height = 25
        BoldHandle = blhAllResidentialBuilding
        TabOrder = 2
        ImageIndices.nbFirst = -1
        ImageIndices.nbPrior = -1
        ImageIndices.nbNext = -1
        ImageIndices.nbLast = -1
        ImageIndices.nbInsert = -1
        ImageIndices.nbDelete = -1
        ImageIndices.nbMoveUp = -1
        ImageIndices.nbMoveDown = -1
        DeleteQuestion = 'Delete "%1:s"?'
        UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
        RemoveQuestion = 'Remove "%1:s" from the list?'
      end
    end
  end
  object GroupBox2: TGroupBox
    Left = 134
    Top = 456
    Width = 96
    Height = 94
    TabOrder = 6
    object btnUnDo: TButton
      Left = 3
      Top = 34
      Width = 88
      Height = 25
      Action = BoldUndoAction1
      TabOrder = 0
    end
    object btnRedo: TButton
      Left = 4
      Top = 65
      Width = 88
      Height = 25
      Action = BoldRedoAction1
      TabOrder = 1
    end
    object btnCheckpoint: TButton
      Left = 3
      Top = 3
      Width = 88
      Height = 29
      Action = BoldSetCheckPointAction1
      TabOrder = 2
    end
  end
  object GroupBox3: TGroupBox
    Left = 504
    Top = 456
    Width = 90
    Height = 95
    TabOrder = 7
    object Button1: TButton
      Left = 3
      Top = 34
      Width = 84
      Height = 25
      Action = BoldActivateSystemAction1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 3
      Top = 3
      Width = 84
      Height = 25
      Action = BoldCreateDatabaseAction1
      TabOrder = 1
    end
  end
  object BoldNavigator1: TBoldNavigator
    Left = 8
    Top = 175
    Width = 192
    Height = 25
    BoldHandle = blhAllPerson
    TabOrder = 8
    ImageIndices.nbFirst = -1
    ImageIndices.nbPrior = -1
    ImageIndices.nbNext = -1
    ImageIndices.nbLast = -1
    ImageIndices.nbInsert = -1
    ImageIndices.nbDelete = -1
    ImageIndices.nbMoveUp = -1
    ImageIndices.nbMoveDown = -1
    DeleteQuestion = 'Delete "%1:s"?'
    UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
    RemoveQuestion = 'Remove "%1:s" from the list?'
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 557
    Width = 601
    Height = 19
    Action = BoldFailureDetectionAction1
    Panels = <>
    SimplePanel = True
  end
  object GroupBox4: TGroupBox
    Left = 392
    Top = 455
    Width = 106
    Height = 96
    TabOrder = 10
    object Label8: TLabel
      Left = 49
      Top = 61
      Width = 3
      Height = 13
    end
    object pbdbNotification: TProgressBar
      Left = 4
      Top = 65
      Width = 98
      Height = 16
      Step = 1
      TabOrder = 2
    end
    object Button3: TButton
      Left = 4
      Top = 34
      Width = 99
      Height = 25
      Action = BoldDiscardChangesAction1
      TabOrder = 0
    end
    object Button5: TButton
      Left = 4
      Top = 3
      Width = 98
      Height = 25
      Action = BoldUpdateDBAction1
      TabOrder = 1
    end
  end
  object GroupBox5: TGroupBox
    Left = 236
    Top = 455
    Width = 133
    Height = 96
    Caption = 'Redo'
    TabOrder = 11
    object lbRedo: TListBox
      Left = 2
      Top = 15
      Width = 129
      Height = 79
      Style = lbOwnerDrawFixed
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnDblClick = lbUndoDblClick
      OnDrawItem = lbUndoDrawItem
    end
  end
  object GroupBox6: TGroupBox
    Left = 8
    Top = 455
    Width = 120
    Height = 96
    Caption = 'Undo'
    TabOrder = 12
    object lbUndo: TListBox
      Left = 2
      Top = 15
      Width = 116
      Height = 79
      Style = lbOwnerDrawFixed
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnDblClick = lbUndoDblClick
      OnDrawItem = lbUndoDrawItem
    end
  end
  object UndoSubscriber: TBoldPlaceableSubscriber
    BoldHandle = DataModule1.BoldSystemHandle1
    OnReceive = UndoSubscriberReceive
    OnSubscribeToElement = UndoSubscriberSubscribeToElement
    Left = 304
    Top = 176
  end
  object BuildingPopup: TPopupMenu
    OnPopup = PopupPopup
    Left = 20
    Top = 276
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
    Left = 16
    Top = 72
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
    Left = 168
    Top = 64
  end
  object blhAllBuilding: TBoldListHandle
    RootHandle = DataModule1.BoldSystemHandle1
    Expression = 'Building.allInstances'
    Left = 240
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
    Top = 352
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
    OnValidateCharacter = bsrRentPerResidentValidateCharacter
    OnGetAsString = bsrRentPerResidentGetAsString
    OnSetAsString = bsrRentPerResidentSetAsString
    OnValidateString = bsrRentPerResidentValidateString
    Left = 32
    Top = 368
  end
  object bsrResidentsTotalAssets: TBoldAsStringRenderer
    OnSubscribe = bsrResidentsTotalAssetsSubscribe
    OnGetAsString = bsrResidentsTotalAssetsGetAsString
    Left = 64
    Top = 368
  end
  object bsrAddress: TBoldAsStringRenderer
    OnSetFont = bsrAddressSetFont
    OnSetColor = bsrAddressSetColor
    Left = 96
    Top = 368
  end
  object blhResidents: TBoldListHandle
    RootHandle = blhAllResidentialBuilding
    Expression = 'residents'
    Left = 528
    Top = 352
  end
  object blhAllResidentialBuilding: TBoldListHandle
    RootHandle = DataModule1.BoldSystemHandle1
    Expression = 'Residential_Building.allInstances'
    Left = 240
    Top = 280
  end
  object blhOwners2: TBoldListHandle
    RootHandle = blhAllResidentialBuilding
    Expression = 'owners'
    Left = 360
    Top = 352
  end
  object BoldPersistenceProgressNotifier1: TBoldPersistenceProgressNotifier
    WinControl = StatusBar1
    ProgressBar = pbdbNotification
    AnimationInterval = 100
    PersistenceHandle = DataModule1.BoldPersistenceHandleDB1
    MsgFetchObjects = 'Fetching objects'
    MsgRetrieveIds = 'Retrieving object ids'
    MsgUpdateDatabase = 'Updating database'
    Left = 112
    Top = 464
  end
  object ActionList1: TActionList
    Left = 16
    Top = 500
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
      ShortCut = 16467
      BoldSystemHandle = DataModule1.BoldSystemHandle1
    end
    object BoldCreateDatabaseAction1: TBoldCreateDatabaseAction
      Category = 'Bold Actions'
      Caption = 'Create DB'
      BoldSystemHandle = DataModule1.BoldSystemHandle1
      IgnoreUnknownTables = True
      DropExisting = True
    end
    object BoldGenerateSchemaAction1: TBoldGenerateSchemaAction
      Category = 'Bold Actions'
      Caption = 'Generate Schema'
      IgnoreUnknownTables = False
      BoldPersistenceHandleDB = DataModule1.BoldPersistenceHandleDB1
    end
    object BoldValidateDBStructureAction1: TBoldValidateDBStructureAction
      Category = 'Bold Actions'
      Caption = 'Validate DB Structure'
      BoldPersistenceHandleDB = DataModule1.BoldPersistenceHandleDB1
    end
    object BoldValidateDBDataAction1: TBoldValidateDBDataAction
      Category = 'Bold Actions'
      Caption = 'Validate DB Data'
      BoldPersistenceHandleDB = DataModule1.BoldPersistenceHandleDB1
    end
    object BoldFailureDetectionAction1: TBoldFailureDetectionAction
      Category = 'Bold Actions'
      Caption = 'BoldFailureDetectionAction1'
    end
    object BoldUndoAction1: TBoldUndoAction
      Category = 'Bold Actions'
      Caption = 'Undo'
      ShortCut = 16474
      BoldSystemHandle = DataModule1.BoldSystemHandle1
    end
    object BoldRedoAction1: TBoldRedoAction
      Category = 'Bold Actions'
      Caption = 'Redo'
      ShortCut = 24666
      BoldSystemHandle = DataModule1.BoldSystemHandle1
    end
    object BoldSystemDebuggerAction1: TBoldSystemDebuggerAction
      Category = 'Bold Actions'
      Caption = 'System debugger'
      ShortCut = 24644
      BoldSystemHandle = DataModule1.BoldSystemHandle1
    end
    object BoldModelEditorAction: TAction
      Category = 'Bold Actions'
      Caption = 'Model Editor'
      OnExecute = BoldModelEditorActionExecute
    end
    object BoldLogOCLAction1: TBoldLogOCLAction
      Category = 'Bold Actions'
      Caption = 'Toggle OCL logs'
    end
    object BoldLogSQLAction1: TBoldLogSQLAction
      Category = 'Bold Actions'
      Caption = 'Toggle SQL logs'
    end
    object BoldLogOSSAction1: TBoldLogOSSAction
      Category = 'Bold Actions'
      Caption = 'Toggle OSS traffic logs'
    end
    object BoldLogPMAction1: TBoldLogPMAction
      Category = 'Bold Actions'
      Caption = 'Toggle PMCalls logs'
    end
    object BoldSetCheckPointAction1: TBoldSetCheckPointAction
      Category = 'Bold Actions'
      Caption = 'Set check point'
      BoldSystemHandle = DataModule1.BoldSystemHandle1
    end
    object actChargeRent: TBoldAction
      Category = 'Bold Actions'
      BoldHandle = blhAllResidentialBuilding
      BoldProperties.Expression = ''
      BoldCaption.Expression = #39'Charge Rent'#39
      BoldEnabled.Expression = 'residents->notEmpty'
      BoldEnabled.NilRepresentation = True
      BoldVisible.Expression = ''
      BoldVisible.NilRepresentation = True
      OnExecute = actChargeRentExecute
    end
    object BoldLogFormAction1: TBoldLogFormAction
      Category = 'Bold Actions'
      Caption = 'Log view'
      ShortCut = 16460
    end
    object BoldDiscardChangesAction1: TBoldDiscardChangesAction
      Category = 'Bold Actions'
      Caption = 'Discard changes'
      BoldSystemHandle = DataModule1.BoldSystemHandle1
    end
    object BoldEvolveDBAction1: TBoldEvolveDBAction
      Category = 'Bold Actions'
      Caption = 'Evolve DB'
      BoldPersistenceHandleDB = DataModule1.BoldPersistenceHandleDB1
      GenerateGenericScript = False
    end
  end
  object HighRentRenderer: TBoldAsStringRenderer
    OnSetFont = HighRentRendererSetFont
    OnSetColor = HighRentRendererSetColor
    Left = 128
    Top = 368
  end
  object MainMenu1: TMainMenu
    Left = 88
    Top = 8
    object Opensystem1: TMenuItem
      Caption = 'Actions'
      object CreateDB1: TMenuItem
        Action = BoldCreateDatabaseAction1
      end
      object EvolveDB1: TMenuItem
        Action = BoldEvolveDBAction1
      end
      object Opensystem2: TMenuItem
        Action = BoldActivateSystemAction1
      end
      object UpdateDB1: TMenuItem
        Action = BoldUpdateDBAction1
      end
      object Discardchanges1: TMenuItem
        Action = BoldDiscardChangesAction1
      end
      object Systemdebugger1: TMenuItem
        Action = BoldSystemDebuggerAction1
      end
      object BoldModelEditorAction1: TMenuItem
        Action = BoldModelEditorAction
      end
      object ValidateDBData1: TMenuItem
        Action = BoldValidateDBDataAction1
      end
      object ValidateDBStructure1: TMenuItem
        Action = BoldValidateDBStructureAction1
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Undo1: TMenuItem
        Action = BoldUndoAction1
      end
      object Redo1: TMenuItem
        Action = BoldRedoAction1
      end
    end
    object Log1: TMenuItem
      Caption = 'Log'
      object ogglelog1: TMenuItem
        Action = BoldLogFormAction1
      end
      object oggleOCLlogs1: TMenuItem
        Action = BoldLogOCLAction1
      end
      object ogglePMCallslogs1: TMenuItem
        Action = BoldLogPMAction1
      end
      object oggleSQLlogs1: TMenuItem
        Action = BoldLogSQLAction1
      end
      object oggleOSStrafficlogs1: TMenuItem
        Action = BoldLogOSSAction1
      end
    end
  end
  object BoldCaptionController1: TBoldCaptionController
    BoldHandle = DataModule1.BoldSystemTypeInfoHandle1
    BoldProperties.Expression = ''
    Left = 448
    Top = 184
  end
end
