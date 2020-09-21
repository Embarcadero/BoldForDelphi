object Form1: TForm1
  Left = 213
  Top = 165
  Width = 737
  Height = 393
  Caption = 'ComboBox Example'
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
  object lblPerson: TLabel
    Left = 11
    Top = 2
    Width = 36
    Height = 13
    Caption = 'Person:'
  end
  object lblSchool: TLabel
    Left = 180
    Top = 2
    Width = 36
    Height = 13
    Caption = 'School:'
  end
  object lblMajorsIn: TLabel
    Left = 180
    Top = 40
    Width = 46
    Height = 13
    Caption = 'Majors In:'
  end
  object lblPreferredFood: TLabel
    Left = 181
    Top = 79
    Width = 73
    Height = 13
    Caption = 'Preferred Food:'
  end
  object lblFavouriteMusic: TLabel
    Left = 180
    Top = 120
    Width = 78
    Height = 13
    Caption = 'Favourite Music:'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 163
    Width = 327
    Height = 2
  end
  object btxtPerson: TBoldEdit
    Left = 8
    Top = 16
    Width = 163
    Height = 21
    BoldHandle = blhPerson
    BoldProperties.Expression = 'name'
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
  object BoldListBox1: TBoldListBox
    Left = 8
    Top = 40
    Width = 163
    Height = 81
    Alignment = taLeftJustify
    BoldHandle = blhPerson
    BoldProperties.InternalDrag = False
    BoldProperties.NilElementMode = neNone
    BoldRowProperties.Expression = 'name'
    DragMode = dmAutomatic
    ItemHeight = 16
    TabOrder = 1
  end
  object btnAdd: TButton
    Left = 17
    Top = 130
    Width = 68
    Height = 25
    Caption = 'Add'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 94
    Top = 130
    Width = 68
    Height = 25
    Caption = 'Remove'
    TabOrder = 3
    OnClick = btnRemoveClick
  end
  object bcboFavouriteMusic: TBoldComboBox
    Left = 179
    Top = 134
    Width = 159
    Height = 21
    Alignment = taLeftJustify
    BoldHandle = blhPerson
    BoldListHandle = bchMusic
    BoldListProperties.DragMode = bdgSelection
    BoldListProperties.DropMode = bdpAppend
    BoldListProperties.NilElementMode = neNone
    BoldProperties.Expression = 'favouriteMusic'
    BoldSelectChangeAction = bdscSetText
    ItemHeight = 13
    TabOrder = 4
  end
  object bcboPreferredFood: TBoldComboBox
    Left = 180
    Top = 93
    Width = 159
    Height = 21
    Alignment = taLeftJustify
    BoldHandle = blhPerson
    BoldListHandle = blhFood
    BoldListProperties.DragMode = bdgSelection
    BoldListProperties.DropMode = bdpAppend
    BoldListProperties.NilElementMode = neNone
    BoldProperties.Expression = 'name + '#39' likes '#39' + preferredfood.name'
    BoldProperties.ApplyPolicy = bapChange
    BoldRowProperties.Expression = 'name'
    BoldSetValueExpression = 'preferredFood'
    BoldSelectChangeAction = bdcsSetValue
    ItemHeight = 13
    TabOrder = 5
  end
  object bcboMajorsIn: TBoldComboBox
    Left = 179
    Top = 54
    Width = 159
    Height = 21
    Alignment = taLeftJustify
    BoldHandle = blhPerson
    BoldListHandle = bchMajorTopic
    BoldListProperties.DragMode = bdgSelection
    BoldListProperties.DropMode = bdpAppend
    BoldListProperties.NilElementMode = neNone
    BoldProperties.Expression = 'major'
    BoldSelectChangeAction = bdscSetText
    ItemHeight = 13
    TabOrder = 6
  end
  object bcboSchool: TBoldComboBox
    Left = 179
    Top = 16
    Width = 159
    Height = 21
    Alignment = taLeftJustify
    BoldHandle = blhPerson
    BoldListHandle = blhSchool
    BoldListProperties.DragMode = bdgSelection
    BoldListProperties.DropMode = bdpAppend
    BoldListProperties.NilElementMode = neNone
    BoldProperties.Expression = 'attendsSchool'
    BoldRowProperties.Expression = 'name'
    BoldSetValueExpression = 'attendsSchool'
    BoldSelectChangeAction = bdscSetText
    ItemHeight = 13
    TabOrder = 7
  end
  object gboFood: TGroupBox
    Left = 8
    Top = 171
    Width = 161
    Height = 161
    Caption = 'Food'
    TabOrder = 8
    object blstFood: TBoldListBox
      Left = 8
      Top = 40
      Width = 145
      Height = 87
      Alignment = taLeftJustify
      BoldHandle = blhFood
      BoldProperties.InternalDrag = False
      BoldProperties.NilElementMode = neNone
      BoldRowProperties.Expression = 'name'
      DragMode = dmAutomatic
      ItemHeight = 16
      TabOrder = 0
    end
    object btxtFood: TBoldEdit
      Left = 8
      Top = 16
      Width = 145
      Height = 21
      BoldHandle = blhFood
      BoldProperties.Expression = 'name'
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
    object btnDelFood: TButton
      Left = 85
      Top = 131
      Width = 68
      Height = 25
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnDelFoodClick
    end
    object btnAddFood: TButton
      Left = 9
      Top = 131
      Width = 68
      Height = 25
      Caption = 'Add'
      TabOrder = 3
      OnClick = btnAddFoodClick
    end
  end
  object gboSchool: TGroupBox
    Left = 176
    Top = 171
    Width = 161
    Height = 161
    Caption = 'School'
    TabOrder = 9
    object blstSchool: TBoldListBox
      Left = 8
      Top = 40
      Width = 144
      Height = 87
      Alignment = taLeftJustify
      BoldHandle = blhSchool
      BoldProperties.InternalDrag = False
      BoldProperties.NilElementMode = neNone
      BoldRowProperties.Expression = 'name'
      DragMode = dmAutomatic
      ItemHeight = 16
      TabOrder = 0
    end
    object btxtSchool: TBoldEdit
      Left = 8
      Top = 16
      Width = 144
      Height = 21
      BoldHandle = blhSchool
      BoldProperties.Expression = 'name'
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
    object btnDelSchool: TButton
      Left = 84
      Top = 131
      Width = 68
      Height = 25
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnDelSchoolClick
    end
    object btnAddSchool: TButton
      Left = 9
      Top = 131
      Width = 68
      Height = 25
      Caption = 'Add'
      TabOrder = 3
      OnClick = btnAddSchoolClick
    end
  end
  object btnSave: TButton
    Left = 212
    Top = 336
    Width = 97
    Height = 25
    Caption = 'Save'
    TabOrder = 10
    OnClick = btnSaveClick
  end
  object Button2: TButton
    Left = 116
    Top = 336
    Width = 89
    Height = 25
    Action = BoldIBDatabaseAction1
    TabOrder = 11
  end
  object Button1: TButton
    Left = 20
    Top = 336
    Width = 89
    Height = 25
    Action = BoldActivateSystemAction1
    TabOrder = 12
  end
  object blhPerson: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Person.allInstances'
    Left = 56
    Top = 64
  end
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = BoldModel1
    Left = 464
    Top = 312
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = True
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldPersistenceHandleDB1
    Left = 464
    Top = 264
  end
  object BoldModel1: TBoldModel
    UMLModelMode = ummNone
    Boldify.EnforceDefaultUMLCase = False
    Boldify.DefaultNavigableMultiplicity = '0..1'
    Boldify.DefaultNonNavigableMultiplicity = '0..*'
    Left = 464
    Top = 216
    Model = (
      'VERSION 19'
      '(Model'
      #9'"ComboBoxClasses"'
      #9'"BusinessClassesRoot"'
      #9'""'
      #9'""'
      
        #9'"_Boldify.boldified=True,_Boldify.RootClass=BusinessClassesRoot' +
        ',_BoldInternal.flattened=True,Bold.DelphiName=<Name>,Bold.RootCl' +
        'ass=BusinessClassesRoot,Bold.UnitName=<Name>"'
      #9'(Classes'
      #9#9'(Class'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'"<NONE>"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=Persistent,Bold.TableName=<Prefix>_OBJECT,Bold.V' +
        'ersioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Food"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Name"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'"<new>"'
      #9#9#9#9#9'"persistence=Persistent,Bold.Length=50"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Person"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Name"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent,Bold.Length=30"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"AttendsSchool"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Major"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent,Bold.Length=50"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"FavouriteMusic"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
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
      #9#9#9'"School"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Name"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'"<new>"'
      #9#9#9#9#9'"persistence=Persistent,Bold.Length=50"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9')'
      #9'(Associations'
      #9#9'(Association'
      #9#9#9'"NewAssociation"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.DelphiName=<Name>"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"IsFavouredBy"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Food"'
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
      #9#9#9#9'(Role'
      #9#9#9#9#9'"PreferredFood"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Person"'
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
      #9#9#9')'
      #9#9')'
      #9')'
      ')')
  end
  object bchMusic: TBoldCursorHandle
    RootHandle = bvhMusic
    AutoFirst = False
    Left = 464
    Top = 160
  end
  object blhFood: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Food.allInstances'
    Left = 464
    Top = 104
  end
  object bchMajorTopic: TBoldCursorHandle
    RootHandle = bvhMajorTopic
    AutoFirst = False
    Left = 464
    Top = 56
  end
  object blhSchool: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'School.allInstances'
    Left = 464
    Top = 8
  end
  object bvhMajorTopic: TBoldVariableHandle
    StaticSystemHandle = BoldSystemHandle1
    ValueTypeName = 'Collection(String)'
    InitialValues.Strings = (
      'Psychology'
      'History'
      'Litterature'
      'Business Economics'
      'Computer Science')
    Left = 560
    Top = 56
  end
  object bvhMusic: TBoldVariableHandle
    StaticSystemHandle = BoldSystemHandle1
    ValueTypeName = 'Collection(String)'
    InitialValues.Strings = (
      'Pop'
      'Country'
      'Rock'
      'Raggae'
      'Rap')
    Left = 560
    Top = 160
  end
  object ActionList1: TActionList
    Left = 576
    Top = 316
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = BoldSystemHandle1
      OnSystemOpened = BoldActivateSystemAction1SystemOpened
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before exit?'
      SaveOnClose = saAsk
    end
    object BoldIBDatabaseAction1: TBoldIBDatabaseAction
      Category = 'Bold Actions'
      Caption = 'Create DB'
      BoldSystemHandle = BoldSystemHandle1
      Username = 'SYSDBA'
      Password = 'masterkey'
    end
  end
  object BoldPersistenceHandleDB1: TBoldPersistenceHandleDB
    BoldModel = BoldModel1
    ClockLogGranularity = '0:0:0.0'
    DatabaseAdapter = BoldDatabaseAdapterIB1
    Left = 408
    Top = 80
  end
  object BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB
    SQLDatabaseConfig.ColumnTypeForDate = 'TIMESTAMP'
    SQLDatabaseConfig.ColumnTypeForTime = 'TIMESTAMP'
    SQLDatabaseConfig.ColumnTypeForDateTime = 'TIMESTAMP'
    SQLDatabaseConfig.ColumnTypeForBlob = 'BLOB'
    SQLDatabaseConfig.ColumnTypeForFloat = 'DOUBLE PRECISION'
    SQLDatabaseConfig.ColumnTypeForCurrency = 'DOUBLE PRECISION'
    SQLDatabaseConfig.ColumnTypeForString = 'VARCHAR(%d)'
    SQLDatabaseConfig.ColumnTypeForInteger = 'INTEGER'
    SQLDatabaseConfig.ColumnTypeForSmallInt = 'SMALLINT'
    SQLDatabaseConfig.DropColumnTemplate = 'ALTER TABLE <TableName> DROP <ColumnName>'
    SQLDatabaseConfig.DropTableTemplate = 'DROP TABLE <TableName>'
    SQLDatabaseConfig.DropIndexTemplate = 'DROP INDEX <IndexName>'
    SQLDatabaseConfig.MaxDbIdentifierLength = 31
    SQLDatabaseConfig.MaxIndexNameLength = 31
    SQLDatabaseConfig.SQLforNotNull = 'NOT NULL'
    SQLDatabaseConfig.QuoteNonStringDefaultValues = False
    SQLDatabaseConfig.SupportsConstraintsInCreateTable = True
    SQLDatabaseConfig.SupportsStringDefaultValues = True
    SQLDatabaseConfig.DBGenerationMode = dbgQuery
    SQLDatabaseConfig.ReservedWords.Strings = (
      'ACTIVE, ADD, ALL, AFTER, ALTER'
      'AND, ANY, AS, ASC, ASCENDING,'
      'AT, AUTO, AUTOINC, AVG, BASE_NAME'
      'BEFORE, BEGIN, BETWEEN, BLOB, BOOLEAN,'
      'BOTH, BY, BYTES, CACHE, CAST, CHAR'
      'CHARACTER, CHECK, CHECK_POINT_LENGTH, COLLATE,'
      'COLUMN, COMMIT, COMMITTED, COMPUTED'
      'CONDITIONAL, CONSTRAINT, CONTAINING, COUNT, CREATE, CSTRING,'
      'CURRENT, CURSOR, DATABASE, DATE, DAY'
      'DEBUG, DEC, DECIMAL, DECLARE, DEFAULT,'
      'DELETE, DESC, DESCENDING, DISTINCT, DO'
      'DOMAIN, DOUBLE, DROP, ELSE, END,'
      'ENTRY_POINT, ESCAPE, EXCEPTION, EXECUTE'
      'EXISTS, EXIT, EXTERNAL, EXTRACT, FILE, FILTER,'
      'FLOAT, FOR, FOREIGN, FROM, FULL, FUNCTION'
      'GDSCODE, GENERATOR, GEN_ID, GRANT,'
      'GROUP, GROUP_COMMIT_WAIT_TIME, HAVING'
      'HOUR, IF, IN, INT, INACTIVE, INDEX, INNER,'
      'INPUT_TYPE, INSERT, INTEGER, INTO'
      'IS, ISOLATION, JOIN, KEY, LONG, LENGTH,'
      'LOGFILE, LOWER, LEADING, LEFT, LEVEL'
      'LIKE, LOG_BUFFER_SIZE, MANUAL, MAX, MAXIMUM_SEGMENT,'
      'MERGE, MESSAGE, MIN, MINUTE, MODULE_NAME'
      'MONEY, MONTH, NAMES, NATIONAL, NATURAL,'
      'NCHAR, NO, NOT, NULL, NUM_LOG_BUFFERS'
      'NUMERIC, OF, ON, ONLY, OPTION,'
      'OR, ORDER, OUTER, OUTPUT_TYPE, OVERFLOW'
      'PAGE_SIZE, PAGE, PAGES, PARAMETER, PASSWORD,'
      'PLAN, POSITION, POST_EVENT, PRECISION'
      
        'PROCEDURE, PROTECTED, PRIMARY, PRIVILEGES, RAW_PARTITIONS, RDB$D' +
        'B_KEY,'
      'READ, REAL, RECORD_VERSION, REFERENCES'
      'RESERV, RESERVING, RETAIN, RETURNING_VALUES, RETURNS, REVOKE,'
      'RIGHT, ROLE, ROLLBACK, SECOND, SEGMENT'
      'SELECT, SET, SHARED, SHADOW, SCHEMA, SINGULAR,'
      'SIZE, SMALLINT, SNAPSHOT, SOME, SORT'
      'SQLCODE, STABILITY, STARTING, STARTS, STATISTICS,'
      'SUB_TYPE, SUBSTRING, SUM, SUSPEND, TABLE'
      'THEN, TIME, TIMESTAMP, TIMEZONE_HOUR, TIMEZONE_MINUTE,'
      'TO, TRAILING, TRANSACTION, TRIGGER, TRIM'
      'UNCOMMITTED, UNION, UNIQUE, UPDATE, UPPER,'
      'USER, VALUE, VALUES, VARCHAR, VARIABLE'
      'VARYING, VIEW, WAIT, WHEN, WHERE,'
      'WHILE, WITH, WORK, WRITE, YEAR')
    SQLDatabaseConfig.StoreEmptyStringsAsNULL = False
    SQLDatabaseConfig.SystemTablePrefix = 'BOLD'
    DataBase = IBDatabase1
    DatabaseEngine = dbeInterbaseSQLDialect3
    Left = 408
    Top = 136
  end
  object IBDatabase1: TIBDatabase
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    IdleTimer = 0
    SQLDialect = 3
    TraceFlags = []
    Left = 408
    Top = 184
  end
end
