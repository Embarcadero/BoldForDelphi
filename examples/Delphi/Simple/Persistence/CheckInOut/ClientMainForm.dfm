object Form2: TForm2
  Left = 358
  Top = 103
  Width = 520
  Height = 578
  Caption = 'OLLE Client App'
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
    Top = 152
    Width = 48
    Height = 13
    Caption = 'Links from'
  end
  object Label2: TLabel
    Left = 104
    Top = 248
    Width = 37
    Height = 13
    Caption = 'Links to'
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 220
    Height = 26
    Caption = 
      '5. Same routine here, press the two buttons to create the databa' +
      'se and add the OLLE tables.'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 272
    Top = 8
    Width = 107
    Height = 13
    Caption = '6. Activate the system.'
  end
  object Label5: TLabel
    Left = 8
    Top = 72
    Width = 407
    Height = 39
    Caption = 
      '7. Press the "Receive check-out" button to import the objects pr' +
      'eviously checked out from the OLLE Server app. Deactivate and re' +
      'activate the system to see the new objects.'
    WordWrap = True
  end
  object Label6: TLabel
    Left = 8
    Top = 376
    Width = 162
    Height = 26
    Caption = '8. Modify some of the objects and save them to the database'
    WordWrap = True
  end
  object Label7: TLabel
    Left = 248
    Top = 152
    Width = 51
    Height = 13
    Caption = 'All objects:'
  end
  object Label8: TLabel
    Left = 328
    Top = 152
    Width = 95
    Height = 13
    Caption = 'Objects to check in:'
  end
  object Label9: TLabel
    Left = 208
    Top = 328
    Width = 273
    Height = 52
    Caption = 
      '9. Drag objects to "Objects to check in" to select which objects' +
      ' to check in again. Alternately, try pressing the "All checked-o' +
      'ut" and "All modified" buttons to fill the list with all checked' +
      ' out, or all modified objects.'
    WordWrap = True
  end
  object Label10: TLabel
    Left = 208
    Top = 416
    Width = 279
    Height = 26
    Caption = 
      '10 Press the "Check in" button to transfer the objects back to t' +
      'he Server app. Return to the Server app.'
    WordWrap = True
  end
  object Label11: TLabel
    Left = 8
    Top = 456
    Width = 219
    Height = 52
    Caption = 
      '12. After having checked in the objects in the server app, press' +
      ' the "Acknowledge check-in button to tell the client that the ch' +
      'eck-in succeeded. '
    WordWrap = True
  end
  object Label12: TLabel
    Left = 248
    Top = 456
    Width = 221
    Height = 39
    Caption = 
      '12b. Alternately, if the check-in did not reach the server, you ' +
      'can press "Fail check-in" button to make the objects checked-out' +
      ' again. '
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
    Top = 144
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
    Top = 344
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
    Top = 168
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
    Top = 264
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
  object Button2: TButton
    Left = 64
    Top = 112
    Width = 105
    Height = 25
    Caption = 'Receive Check-Out'
    TabOrder = 5
    OnClick = Button2Click
  end
  object BoldListBox3: TBoldListBox
    Left = 224
    Top = 168
    Width = 97
    Height = 153
    Alignment = taLeftJustify
    BoldHandle = BoldListHandle4
    BoldProperties.NilElementMode = neNone
    DragMode = dmAutomatic
    ItemHeight = 16
    TabOrder = 6
  end
  object BoldListBox4: TBoldListBox
    Left = 328
    Top = 168
    Width = 97
    Height = 153
    Alignment = taLeftJustify
    BoldHandle = BoldCursorHandle1
    BoldProperties.NilElementMode = neNone
    BoldRowProperties.Renderer = BoldAsStringRenderer1
    DragMode = dmAutomatic
    ItemHeight = 16
    TabOrder = 7
  end
  object Button3: TButton
    Left = 344
    Top = 384
    Width = 75
    Height = 25
    Caption = 'Check In'
    TabOrder = 8
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 56
    Top = 520
    Width = 121
    Height = 25
    Caption = 'Acknowledge check-in'
    TabOrder = 9
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 320
    Top = 520
    Width = 89
    Height = 25
    Caption = 'Fail check-in'
    TabOrder = 10
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 288
    Top = 120
    Width = 89
    Height = 25
    Caption = 'All checked-out'
    TabOrder = 11
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 376
    Top = 120
    Width = 75
    Height = 25
    Caption = 'All modified'
    TabOrder = 12
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Action = BoldIBDatabaseAction1
    TabOrder = 14
  end
  object Button9: TButton
    Left = 272
    Top = 40
    Width = 75
    Height = 25
    Action = BoldActivateSystemAction1
    TabOrder = 13
  end
  object Button10: TButton
    Left = 104
    Top = 344
    Width = 75
    Height = 25
    Action = BoldUpdateDBAction1
    TabOrder = 15
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = True
    SystemTypeInfoHandle = DataModule1.BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldPersistenceHandleDB1
    Left = 368
    Top = 40
  end
  object BoldOLLEHandle1: TBoldOLLEHandle
    ApplicationPersistenceHandle = BoldPersistenceHandleDB1
    Left = 200
    Top = 40
  end
  object BoldListHandle1: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Thing.allInstances'
    Left = 32
    Top = 232
  end
  object BoldListHandle2: TBoldListHandle
    RootHandle = BoldListHandle1
    Expression = 'linkFrom'
    Left = 128
    Top = 192
  end
  object BoldListHandle3: TBoldListHandle
    RootHandle = BoldListHandle1
    Expression = 'linkTo'
    Left = 128
    Top = 296
  end
  object BoldListHandle4: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'BusinessClassesRoot.allInstances'
    Left = 264
    Top = 192
  end
  object BoldVariableHandle1: TBoldVariableHandle
    StaticSystemHandle = BoldSystemHandle1
    ValueTypeName = 'Collection(BusinessClassesRoot)'
    Left = 344
    Top = 200
  end
  object BoldCursorHandle1: TBoldCursorHandle
    RootHandle = BoldVariableHandle1
    Left = 384
    Top = 200
  end
  object BoldAsStringRenderer1: TBoldAsStringRenderer
    OnSubscribe = BoldAsStringRenderer1Subscribe
    OnGetAsString = BoldAsStringRenderer1GetAsString
    Left = 360
    Top = 256
  end
  object BoldPersistenceHandleDB1: TBoldPersistenceHandleDB
    BoldModel = DataModule1.BoldModel1
    ClockLogGranularity = '0:0:0.0'
    DatabaseAdapter = BoldDatabaseAdapterIB1
    Left = 464
    Top = 24
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
    Left = 464
    Top = 80
  end
  object IBDatabase1: TIBDatabase
    DatabaseName = 'CheckInCheckOutClient.gdb'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    IdleTimer = 0
    SQLDialect = 3
    TraceFlags = []
    Left = 464
    Top = 128
  end
  object ActionList1: TActionList
    Left = 216
    Top = 128
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
end
