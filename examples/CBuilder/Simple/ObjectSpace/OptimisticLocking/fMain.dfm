object Form1: TForm1
  Left = 0
  Top = -2
  Width = 745
  Height = 485
  Caption = 'Association Class Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 305
    Height = 393
    Caption = 'App 1'
    TabOrder = 0
    object Label2: TLabel
      Left = 8
      Top = 280
      Width = 65
      Height = 13
      Caption = 'Failed objects'
    end
    object cmdOpen1: TButton
      Left = 184
      Top = 56
      Width = 105
      Height = 25
      Action = BoldActivateSystemAction1
      TabOrder = 0
    end
    object BoldGrid1: TBoldGrid
      Left = 8
      Top = 24
      Width = 169
      Height = 120
      AddNewAtEnd = False
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = blhAllPersons1
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
        end
        item
          BoldProperties.Expression = 'hasLicense'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
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
    object BoldGrid2: TBoldGrid
      Left = 8
      Top = 152
      Width = 241
      Height = 120
      AddNewAtEnd = False
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = blhAllCars1
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
          BoldProperties.Expression = 'color'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        item
          BoldProperties.Expression = 'maxSpeed'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        item
          BoldProperties.Expression = 'model'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
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
        64
        64
        64)
    end
    object BoldNavigator1: TBoldNavigator
      Left = 184
      Top = 88
      Width = 56
      Height = 25
      BoldHandle = blhAllPersons1
      TabOrder = 3
      VisibleButtons = [nbInsert, nbDelete]
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
      Left = 184
      Top = 120
      Width = 56
      Height = 25
      BoldHandle = blhAllCars1
      TabOrder = 4
      VisibleButtons = [nbInsert, nbDelete]
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
    object cmdUpdate1: TButton
      Left = 184
      Top = 24
      Width = 105
      Height = 25
      Caption = 'Update Database'
      TabOrder = 5
      OnClick = cmdUpdate1Click
    end
    object BoldListBox1: TBoldListBox
      Left = 8
      Top = 296
      Width = 121
      Height = 81
      Alignment = taLeftJustify
      BoldHandle = bchFailedObjects1
      BoldProperties.InternalDrag = False
      BoldProperties.NilElementMode = neNone
      DragMode = dmAutomatic
      ItemHeight = 16
      TabOrder = 6
    end
    object cmdDiscardApp1: TButton
      Left = 136
      Top = 352
      Width = 75
      Height = 25
      Caption = 'Discard'
      TabOrder = 7
      OnClick = cmdDiscardApp1Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 328
    Top = 8
    Width = 305
    Height = 393
    Caption = 'App 2'
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 280
      Width = 65
      Height = 13
      Caption = 'Failed objects'
    end
    object cmdOpen2: TButton
      Left = 184
      Top = 56
      Width = 105
      Height = 25
      Action = BoldActivateSystemAction2
      TabOrder = 0
    end
    object BoldGrid3: TBoldGrid
      Left = 8
      Top = 24
      Width = 169
      Height = 120
      AddNewAtEnd = False
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = blhAllPersons2
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
        end
        item
          BoldProperties.Expression = 'hasLicense'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
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
      RowHeights = (
        17
        17)
    end
    object BoldGrid4: TBoldGrid
      Left = 8
      Top = 152
      Width = 241
      Height = 120
      AddNewAtEnd = False
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = blhallCars2
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
          BoldProperties.Expression = 'color'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        item
          BoldProperties.Expression = 'maxSpeed'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        item
          BoldProperties.Expression = 'model'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
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
        64
        64
        64)
    end
    object BoldNavigator3: TBoldNavigator
      Left = 184
      Top = 88
      Width = 56
      Height = 25
      BoldHandle = blhAllPersons2
      TabOrder = 3
      VisibleButtons = [nbInsert, nbDelete]
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
    object BoldNavigator4: TBoldNavigator
      Left = 184
      Top = 120
      Width = 56
      Height = 25
      BoldHandle = blhallCars2
      TabOrder = 4
      VisibleButtons = [nbInsert, nbDelete]
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
    object cmdUpdate2: TButton
      Left = 184
      Top = 24
      Width = 105
      Height = 25
      Caption = 'Update Database'
      TabOrder = 5
      OnClick = cmdUpdate2Click
    end
    object BoldListBox2: TBoldListBox
      Left = 8
      Top = 296
      Width = 121
      Height = 81
      Alignment = taLeftJustify
      BoldHandle = bchFailedObjects2
      BoldProperties.InternalDrag = False
      BoldProperties.NilElementMode = neNone
      DragMode = dmAutomatic
      ItemHeight = 16
      TabOrder = 6
    end
    object cmdDiscardApp2: TButton
      Left = 136
      Top = 352
      Width = 75
      Height = 25
      Caption = 'Discard'
      TabOrder = 7
      OnClick = cmdDiscardApp2Click
    end
  end
  object cmdCreateDB: TButton
    Left = 8
    Top = 408
    Width = 75
    Height = 25
    Action = BoldIBDatabaseAction1
    TabOrder = 2
  end
  object BoldModel1: TBoldModel
    UMLModelMode = ummNone
    Boldify.EnforceDefaultUMLCase = False
    Boldify.DefaultNavigableMultiplicity = '0..1'
    Boldify.DefaultNonNavigableMultiplicity = '0..*'
    Left = 672
    Top = 36
    Model = (
      'VERSION 19'
      '(Model'
      #9'"OptimisticLockingExampleClasses"'
      #9'"BusinessClassesRoot"'
      #9'""'
      #9'""'
      
        #9'"_Boldify.boldified=True,_Boldify.RootClass=BusinessClassesRoot' +
        ',_BoldInternal.flattened=True,Bold.DelphiName=<Name>,Bold.Genera' +
        'teMultiplicityConstraints=False,Bold.OptimisticLocking=Member,Bo' +
        'ld.PMapper=<default>,Bold.RootClass=BusinessClassesRoot,Bold.Uni' +
        'tName=<Name>"'
      #9'(Classes'
      #9#9'(Class'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'"<NONE>"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=Persistent,Bold.PMapper=<default>,Bold.TableName' +
        '=BOLD_OBJECT,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Car"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=Persistent,\"Bold.DefaultStringRepresentation=co' +
        'lor + '#39' '#39' + model\",Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"color"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"maxSpeed"'
      #9#9#9#9#9'"i"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"model"'
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
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Person"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=Persistent,\"Bold.DefaultStringRepresentation=na' +
        'me + '#39' '#39'+if haslicense then '#39'with'#39' else '#39'without'#39' endif + '#39' lice' +
        'nse'#39'\",Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"name"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"hasLicense"'
      #9#9#9#9#9'"Boolean"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent"'
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
      #9#9#9#9#9'"driver"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Car"'
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
      #9#9#9#9#9'"cars"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Person"'
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
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = BoldModel1
    Left = 672
    Top = 72
  end
  object ActionList1: TActionList
    Left = 672
    Top = 112
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = BoldSystemHandle1
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before exit?'
      SaveOnClose = saAsk
    end
    object BoldActivateSystemAction2: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = BoldSystemHandle2
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
  object BoldPlaceableAFP1: TBoldPlaceableAFP
    Left = 672
    Top = 152
  end
  object blhAllPersons1: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Person.allInstances'
    Left = 152
    Top = 120
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = False
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldPersistenceHandleDB1
    Left = 264
    Top = 132
  end
  object blhAllCars1: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Car.allInstances'
    Left = 224
    Top = 248
  end
  object bvhFailedObjects1: TBoldVariableHandle
    StaticSystemHandle = BoldSystemHandle1
    ValueTypeName = 'Collection(BusinessClassesRoot)'
    Left = 72
    Top = 352
  end
  object bchFailedObjects1: TBoldCursorHandle
    RootHandle = bvhFailedObjects1
    AutoFirst = False
    Left = 104
    Top = 352
  end
  object blhAllPersons2: TBoldListHandle
    RootHandle = BoldSystemHandle2
    Expression = 'Person.allInstances'
    Left = 472
    Top = 120
  end
  object BoldSystemHandle2: TBoldSystemHandle
    IsDefault = False
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldPersistenceHandleDB2
    Left = 584
    Top = 132
  end
  object blhallCars2: TBoldListHandle
    RootHandle = BoldSystemHandle2
    Expression = 'Car.allInstances'
    Left = 544
    Top = 248
  end
  object bvhFailedObjects2: TBoldVariableHandle
    StaticSystemHandle = BoldSystemHandle2
    ValueTypeName = 'Collection(BusinessClassesRoot)'
    Left = 392
    Top = 352
  end
  object bchFailedObjects2: TBoldCursorHandle
    RootHandle = bvhFailedObjects2
    AutoFirst = False
    Left = 424
    Top = 352
  end
  object BoldPersistenceHandleDB1: TBoldPersistenceHandleDB
    BoldModel = BoldModel1
    ClockLogGranularity = '0:0:0.0'
    DatabaseAdapter = BoldDatabaseAdapterIB1
    Left = 264
    Top = 176
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
    Left = 264
    Top = 232
  end
  object IBDatabase1: TIBDatabase
    DatabaseName = 'OptimisticLocking.gdb'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    IdleTimer = 0
    SQLDialect = 3
    TraceFlags = []
    Left = 264
    Top = 280
  end
  object BoldPersistenceHandleDB2: TBoldPersistenceHandleDB
    BoldModel = BoldModel1
    ClockLogGranularity = '0:0:0.0'
    DatabaseAdapter = BoldDatabaseAdapterIB2
    Left = 584
    Top = 168
  end
  object BoldDatabaseAdapterIB2: TBoldDatabaseAdapterIB
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
    DataBase = IBDatabase2
    DatabaseEngine = dbeInterbaseSQLDialect3
    Left = 584
    Top = 224
  end
  object IBDatabase2: TIBDatabase
    DatabaseName = 'OptimisticLocking.gdb'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    IdleTimer = 0
    SQLDialect = 3
    TraceFlags = []
    Left = 584
    Top = 272
  end
end
