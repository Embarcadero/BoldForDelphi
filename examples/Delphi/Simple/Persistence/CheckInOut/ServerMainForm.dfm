object Form1: TForm1
  Left = 357
  Top = 142
  Width = 496
  Height = 429
  Caption = 'OLLE Server App'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 104
    Top = 176
    Width = 48
    Height = 13
    Caption = 'Links from'
  end
  object Label2: TLabel
    Left = 104
    Top = 272
    Width = 37
    Height = 13
    Caption = 'Links to'
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 323
    Height = 26
    Caption = 
      '1. Alright, you know the drill. Create the database, and add the' +
      ' extra OLLE tables. Press the two buttons below.'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 8
    Top = 72
    Width = 155
    Height = 13
    Caption = '2. Next, activate the Bold system'
  end
  object Label5: TLabel
    Left = 8
    Top = 120
    Width = 217
    Height = 39
    Caption = 
      '3. Create a couple of objects and link them to each other (using' +
      ' the '#39'+'#39' button and drag-drop). Save them to the database.'
    WordWrap = True
  end
  object Label6: TLabel
    Left = 272
    Top = 48
    Width = 51
    Height = 13
    Caption = 'All objects:'
  end
  object Label7: TLabel
    Left = 360
    Top = 48
    Width = 102
    Height = 13
    Caption = 'Objects to check out:'
  end
  object Label8: TLabel
    Left = 248
    Top = 216
    Width = 227
    Height = 52
    Caption = 
      '4. Drag objects from "All objects" to "Objects to check out" to ' +
      'select some objects to transfer to the client app. Press the "Ex' +
      'port && Check out" button. Proceed to the OLLE Client app.'
    WordWrap = True
  end
  object Label9: TLabel
    Left = 248
    Top = 312
    Width = 231
    Height = 52
    Caption = 
      '11. Returning from the Client app, press the "Receive Check-in" ' +
      'button to import back the changes made by the Client app. Deacti' +
      'vate and reactivate the system to see the changes.'
    WordWrap = True
  end
  object Button1: TButton
    Left = 88
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Add OLLE'
    TabOrder = 0
    OnClick = Button1Click
  end
  object BoldGrid1: TBoldGrid
    Left = 8
    Top = 168
    Width = 89
    Height = 193
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = BoldListHandle1
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
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      64)
  end
  object BoldNavigator1: TBoldNavigator
    Left = 8
    Top = 368
    Width = 88
    Height = 25
    BoldHandle = BoldListHandle1
    TabOrder = 2
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
  object BoldListBox1: TBoldListBox
    Left = 104
    Top = 192
    Width = 73
    Height = 73
    Alignment = taLeftJustify
    BoldHandle = BoldListHandle2
    BoldProperties.NilElementMode = neNone
    BoldRowProperties.Expression = 'name'
    DragMode = dmAutomatic
    ItemHeight = 16
    TabOrder = 3
  end
  object BoldListBox2: TBoldListBox
    Left = 104
    Top = 288
    Width = 73
    Height = 73
    Alignment = taLeftJustify
    BoldHandle = BoldListHandle3
    BoldProperties.NilElementMode = neNone
    BoldRowProperties.Expression = 'name'
    DragMode = dmAutomatic
    ItemHeight = 16
    TabOrder = 4
  end
  object BoldListBox3: TBoldListBox
    Left = 248
    Top = 64
    Width = 105
    Height = 145
    Alignment = taLeftJustify
    BoldHandle = BoldListHandle4
    BoldProperties.NilElementMode = neNone
    DragMode = dmAutomatic
    ItemHeight = 16
    TabOrder = 5
  end
  object BoldListBox4: TBoldListBox
    Left = 360
    Top = 64
    Width = 105
    Height = 145
    Alignment = taLeftJustify
    BoldHandle = BoldCursorHandle1
    BoldProperties.NilElementMode = neNone
    DragMode = dmAutomatic
    ItemHeight = 16
    TabOrder = 6
  end
  object Button2: TButton
    Left = 312
    Top = 272
    Width = 105
    Height = 25
    Caption = 'Export && Check out'
    TabOrder = 7
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 312
    Top = 368
    Width = 105
    Height = 25
    Caption = 'Receive Check-in'
    TabOrder = 8
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Action = BoldIBDatabaseAction1
    TabOrder = 10
  end
  object Button5: TButton
    Left = 8
    Top = 96
    Width = 75
    Height = 25
    Action = BoldActivateSystemAction1
    TabOrder = 9
  end
  object Button6: TButton
    Left = 104
    Top = 368
    Width = 75
    Height = 25
    Action = BoldUpdateDBAction1
    TabOrder = 11
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = True
    SystemTypeInfoHandle = DataModule1.BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldPersistenceHandleDB1
    Left = 88
    Top = 88
  end
  object BoldOLLEHandle1: TBoldOLLEHandle
    ApplicationPersistenceHandle = BoldPersistenceHandleDB1
    Left = 168
    Top = 40
  end
  object BoldListHandle1: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Thing.allInstances'
    Left = 32
    Top = 256
  end
  object BoldListHandle2: TBoldListHandle
    RootHandle = BoldListHandle1
    Expression = 'linkFrom'
    Left = 128
    Top = 216
  end
  object BoldListHandle3: TBoldListHandle
    RootHandle = BoldListHandle1
    Expression = 'linkTo'
    Left = 128
    Top = 320
  end
  object BoldListHandle4: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'BusinessClassesRoot.allInstances'
    Left = 256
    Top = 96
  end
  object BoldVariableHandle1: TBoldVariableHandle
    StaticSystemHandle = BoldSystemHandle1
    ValueTypeName = 'Collection(BusinessClassesRoot)'
    Left = 400
    Top = 96
  end
  object BoldCursorHandle1: TBoldCursorHandle
    RootHandle = BoldVariableHandle1
    Left = 400
    Top = 136
  end
  object ActionList1: TActionList
    Left = 208
    Top = 72
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = BoldSystemHandle1
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before closing system?'
      SaveOnClose = saAsk
    end
    object BoldIBDatabaseAction1: TBoldIBDatabaseAction
      Category = 'Bold Actions'
      Caption = 'Create DB'
      BoldSystemHandle = BoldSystemHandle1
      Username = 'SYSDBA'
      Password = 'masterkey'
    end
    object BoldUpdateDBAction1: TBoldUpdateDBAction
      Category = 'Bold Actions'
      Caption = 'Update DB'
      BoldSystemHandle = BoldSystemHandle1
    end
  end
  object BoldPersistenceHandleDB1: TBoldPersistenceHandleDB
    BoldModel = DataModule1.BoldModel1
    ClockLogGranularity = '0:0:0.0'
    DatabaseAdapter = BoldDatabaseAdapterIB1
    Left = 200
    Top = 192
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
    Left = 200
    Top = 248
  end
  object IBDatabase1: TIBDatabase
    DatabaseName = 'CheckInCheckOut.gdb'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    IdleTimer = 0
    SQLDialect = 3
    TraceFlags = []
    Left = 200
    Top = 296
  end
end
