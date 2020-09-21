object Form1: TForm1
  Left = 361
  Top = 105
  Width = 541
  Height = 451
  Caption = 'Data Validation Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblMaleStudents: TLabel
    Left = 8
    Top = 8
    Width = 71
    Height = 13
    Caption = 'Male Students:'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 171
    Width = 281
    Height = 2
  end
  object lblFemaleStudents: TLabel
    Left = 152
    Top = 8
    Width = 82
    Height = 13
    Caption = 'Female Students:'
  end
  object lblStudents: TLabel
    Left = 152
    Top = 194
    Width = 45
    Height = 13
    Caption = 'Students:'
  end
  object lblRooms: TLabel
    Left = 8
    Top = 178
    Width = 36
    Height = 13
    Caption = 'Rooms:'
  end
  object Label2: TLabel
    Left = 304
    Top = 328
    Width = 204
    Height = 26
    Caption = 
      'It is not possible to assign more students to a room than the av' +
      'ailable number of beds.'
    Visible = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 304
    Top = 256
    Width = 204
    Height = 65
    Caption = 
      'Try dragging male or female students from their lists and droppi' +
      'ng them into '#39'Students'#39'. The drop action is only available if th' +
      'e new student is typeconformant to those already assigned to the' +
      ' room. '
    Visible = False
    WordWrap = True
  end
  object btnAddMale: TButton
    Left = 8
    Top = 136
    Width = 65
    Height = 25
    Caption = 'Add'
    TabOrder = 0
    OnClick = btnAddMaleClick
  end
  object btnDelMale: TButton
    Left = 78
    Top = 136
    Width = 65
    Height = 25
    Caption = 'Remove'
    TabOrder = 1
    OnClick = btnDelMaleClick
  end
  object bgrdMaleStudents: TBoldGrid
    Left = 8
    Top = 24
    Width = 134
    Height = 105
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhMaleStudents
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
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      112)
  end
  object bgrdFemaleStudents: TBoldGrid
    Left = 152
    Top = 24
    Width = 134
    Height = 105
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhFemaleStudents
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
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      112)
  end
  object btnAddFemale: TButton
    Left = 152
    Top = 136
    Width = 65
    Height = 25
    Caption = 'Add'
    TabOrder = 4
    OnClick = btnAddFemaleClick
  end
  object btnDelFemale: TButton
    Left = 222
    Top = 136
    Width = 65
    Height = 25
    Caption = 'Remove'
    TabOrder = 5
    OnClick = btnDelFemaleClick
  end
  object BoldListBox1: TBoldListBox
    Left = 152
    Top = 210
    Width = 134
    Height = 113
    Alignment = taLeftJustify
    BoldHandle = blhInhabitants
    BoldProperties.InternalDrag = False
    BoldProperties.NilElementMode = neNone
    BoldRowProperties.Expression = 'name'
    DragMode = dmAutomatic
    ItemHeight = 16
    TabOrder = 6
    OnDragOver = BoldListBox1DragOver
  end
  object bgrdRooms: TBoldGrid
    Left = 8
    Top = 194
    Width = 137
    Height = 193
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = blhRooms
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
        BoldProperties.Expression = 'roomNumber'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Room No.'
      end
      item
        BoldProperties.Expression = 'numberOfBeds'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'No. of Beds'
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 7
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      55
      59)
  end
  object btnAddRoom: TButton
    Left = 8
    Top = 394
    Width = 65
    Height = 25
    Caption = 'Add'
    TabOrder = 8
    OnClick = btnAddRoomClick
  end
  object btnDelRoom: TButton
    Left = 78
    Top = 394
    Width = 65
    Height = 25
    Caption = 'Remove'
    TabOrder = 9
    OnClick = btnDelRoomClick
  end
  object btnSave: TButton
    Left = 160
    Top = 394
    Width = 121
    Height = 25
    Caption = 'Save'
    TabOrder = 10
    OnClick = btnSaveClick
  end
  object Button2: TButton
    Left = 160
    Top = 364
    Width = 121
    Height = 25
    Action = BoldIBAliasAction1
    TabOrder = 11
  end
  object Button1: TButton
    Left = 160
    Top = 332
    Width = 121
    Height = 25
    Action = BoldActivateSystemAction1
    TabOrder = 12
  end
  object blhMaleStudents: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'MaleStudent.allInstances'
    Left = 56
    Top = 56
  end
  object blhFemaleStudents: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'FemaleStudent.allInstances'
    Left = 208
    Top = 56
  end
  object ActionList1: TActionList
    Left = 328
    Top = 32
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = BoldSystemHandle1
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before exit?'
      SaveOnClose = saAsk
    end
    object BoldIBAliasAction1: TBoldIBAliasAction
      Category = 'Bold Actions'
      Caption = 'Create DB'
      BoldSystemHandle = BoldSystemHandle1
      Username = 'SYSDBA'
      Password = 'masterkey'
    end
  end
  object BoldModel1: TBoldModel
    UMLModelMode = ummNone
    Boldify.EnforceDefaultUMLCase = False
    Boldify.DefaultNavigableMultiplicity = '0..1'
    Boldify.DefaultNonNavigableMultiplicity = '0..*'
    Left = 416
    Top = 32
    Model = (
      'VERSION 19'
      '(Model'
      #9'"DataValidationExampleClasses"'
      #9'"BusinessClassesRoot"'
      #9'""'
      #9'""'
      
        #9'"_Boldify.boldified=True,_Boldify.RootClass=BusinessClassesRoot' +
        ',_BoldInternal.flattened=True,Bold.RootClass=BusinessClassesRoot' +
        ',Bold.UnitName=<Name>"'
      #9'(Classes'
      #9#9'(Class'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'"<NONE>"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"Bold.TableName=<Prefix>_OBJECT,Bold.Versioned=<Default>,Pers' +
        'istence=Persistent"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Student"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'TRUE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"Bold.Versioned=<Default>,Persistence=Persistent"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Name"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"Bold.Length=100,Persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"FemaleStudent"'
      #9#9#9'"Student"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"Bold.Versioned=<Default>,Persistence=Persistent"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"CompleteCreate"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"Bold.OperationKind=Override"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"MaleStudent"'
      #9#9#9'"Student"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"Bold.Versioned=<Default>,Persistence=Persistent"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"CompleteCreate"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"Bold.OperationKind=Override"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Room"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"Bold.Versioned=<Default>,Persistence=Persistent"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"RoomNumber"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"Persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"NumberOfBeds"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"Persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"ValidateNewInhabitant"'
      #9#9#9#9#9'"NewInhabitant: TStudent"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"boolean"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"CompleteCreate"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"Bold.OperationKind=Override"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9')'
      #9'(Associations'
      #9#9'(Association'
      #9#9#9'"StudentsInRoom"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"Persistence=Persistent"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"LivesIn"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Student"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'""'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Inhabitants"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Room"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..*"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9')'
      ')')
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = False
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldPersistenceHandleBDE1
    Left = 416
    Top = 80
  end
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = BoldModel1
    Left = 416
    Top = 128
  end
  object BoldPersistenceHandleBDE1: TBoldPersistenceHandleBDE
    Username = 'SYSDBA'
    Password = 'masterkey'
    BoldModel = BoldModel1
    SystemTablesPrefix = 'BOLD'
    ClockLogGranularity = '0:0:0.0'
    SQLDataBaseConfig.ColumnTypeForDate = 'DATE'
    SQLDataBaseConfig.ColumnTypeForTime = 'DATE'
    SQLDataBaseConfig.ColumnTypeForDateTime = 'DATE'
    SQLDataBaseConfig.ColumnTypeForBlob = 'BLOB'
    SQLDataBaseConfig.UseSQL92Joins = False
    DatabaseEngine = dbeUnknown
    Left = 416
    Top = 176
  end
  object blhInhabitants: TBoldListHandle
    RootHandle = blhRooms
    Expression = 'inhabitants'
    Left = 200
    Top = 218
  end
  object blhRooms: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Room.allInstances->orderby(roomNumber)'
    Left = 56
    Top = 266
  end
end
