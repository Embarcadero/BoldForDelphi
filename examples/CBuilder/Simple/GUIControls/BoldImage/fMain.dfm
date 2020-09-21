object frmMain: TfrmMain
  Left = 264
  Top = 146
  Width = 492
  Height = 558
  Caption = 'Image Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    484
    531)
  PixelsPerInch = 96
  TextHeight = 13
  object lblBoldImage: TLabel
    Left = 8
    Top = 3
    Width = 53
    Height = 13
    Caption = 'BoldImage:'
  end
  object lblDescription: TLabel
    Left = 8
    Top = 400
    Width = 56
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Description:'
  end
  object btxtImageAsString: TBoldEdit
    Left = 8
    Top = 18
    Width = 321
    Height = 21
    BoldHandle = blhImages
    BoldProperties.Representation = 2
    BoldProperties.Expression = 'image'
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
  object gbxProperties: TGroupBox
    Left = 338
    Top = 4
    Width = 141
    Height = 159
    Anchors = [akTop, akRight]
    Caption = 'Properties'
    TabOrder = 1
    object lblDrawFocus: TLabel
      Left = 8
      Top = 15
      Width = 57
      Height = 13
      Caption = 'DrawFocus:'
    end
    object lblContentTypeOnPaste: TLabel
      Left = 8
      Top = 55
      Width = 105
      Height = 13
      Caption = 'ContentTypeOnPaste:'
    end
    object cboDrawFocus: TComboBox
      Left = 8
      Top = 30
      Width = 123
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cboDrawFocusChange
      Items.Strings = (
        'bsNone'
        'bsSingle')
    end
    object edtContentTypeOnPaste: TEdit
      Left = 8
      Top = 70
      Width = 123
      Height = 21
      TabOrder = 1
      OnChange = edtContentTypeOnPasteChange
    end
    object chkEnabled: TCheckBox
      Left = 9
      Top = 97
      Width = 72
      Height = 17
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = chkEnabledClick
    end
    object chkReadOnly: TCheckBox
      Left = 9
      Top = 115
      Width = 74
      Height = 17
      Caption = 'ReadOnly'
      TabOrder = 3
      OnClick = chkReadOnlyClick
    end
    object chkTabStop: TCheckBox
      Left = 9
      Top = 133
      Width = 71
      Height = 17
      Caption = 'TabStop'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = chkTabStopClick
    end
  end
  object gbxClipBoard: TGroupBox
    Left = 338
    Top = 169
    Width = 141
    Height = 140
    Anchors = [akTop, akRight]
    Caption = 'Clipboard'
    TabOrder = 2
    object btnPaste: TButton
      Left = 8
      Top = 77
      Width = 123
      Height = 25
      Caption = 'Paste'
      TabOrder = 0
      OnClick = btnPasteClick
    end
    object btnCut: TButton
      Left = 8
      Top = 18
      Width = 123
      Height = 25
      Caption = 'Cut'
      TabOrder = 1
      OnClick = btnCutClick
    end
    object btnCopy: TButton
      Left = 8
      Top = 48
      Width = 123
      Height = 25
      Caption = 'Copy'
      TabOrder = 2
      OnClick = btnCopyClick
    end
    object btnViewClipboardFmt: TButton
      Left = 9
      Top = 107
      Width = 123
      Height = 25
      Caption = 'View Clipboard Formats'
      TabOrder = 3
      OnClick = btnViewClipboardFmtClick
    end
  end
  object gbxFiles: TGroupBox
    Left = 338
    Top = 315
    Width = 141
    Height = 79
    Anchors = [akTop, akRight]
    Caption = 'Files'
    TabOrder = 3
    object btnLoad: TButton
      Left = 8
      Top = 16
      Width = 123
      Height = 25
      Caption = 'Load from file'
      TabOrder = 0
      OnClick = btnLoadClick
    end
    object btnSave: TButton
      Left = 8
      Top = 46
      Width = 123
      Height = 25
      Caption = 'Save to file'
      TabOrder = 1
      OnClick = btnSaveClick
    end
  end
  object btnView: TButton
    Left = 347
    Top = 401
    Width = 123
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'View'
    TabOrder = 4
    OnClick = btnViewClick
  end
  object Button1: TButton
    Left = 348
    Top = 432
    Width = 125
    Height = 25
    Action = BoldActivateSystemAction1
    Anchors = [akTop, akRight]
    TabOrder = 5
  end
  object Button2: TButton
    Left = 348
    Top = 464
    Width = 125
    Height = 25
    Action = BoldIBDatabaseAction1
    Anchors = [akTop, akRight]
    TabOrder = 6
  end
  object btnUpdateDB: TButton
    Left = 207
    Top = 483
    Width = 123
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save'
    TabOrder = 7
    OnClick = btnUpdateDBClick
  end
  object BoldNavigator1: TBoldNavigator
    Left = 8
    Top = 483
    Width = 144
    Height = 25
    Anchors = [akLeft, akBottom]
    BoldHandle = blhImages
    TabOrder = 8
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
  object bmemDescription: TBoldMemo
    Left = 8
    Top = 419
    Width = 323
    Height = 59
    Alignment = taLeftJustify
    Anchors = [akLeft, akRight, akBottom]
    BoldHandle = blhImages
    BoldProperties.Expression = 'description'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxLength = 0
    ReadOnly = False
    TabOrder = 9
  end
  object pnlImage: TPanel
    Left = 8
    Top = 47
    Width = 319
    Height = 345
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'pnlImage'
    TabOrder = 10
    object BoldImage: TBoldImage
      Left = 0
      Top = 0
      Width = 319
      Height = 345
      BoldHandle = blhImages
      BoldProperties.Expression = 'image'
      ContentTypeOnPaste = 'image/jpeg'
      StretchMode = bsmNoStretch
      Center = False
      QuickDraw = False
      Align = alClient
      TabOrder = 0
    end
  end
  object SavePictureDialog1: TSavePictureDialog
    Left = 264
    Top = 336
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Title = 'Load File'
    Left = 264
    Top = 288
  end
  object ActionList1: TActionList
    Left = 96
    Top = 271
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = BoldSystemHandle1
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
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = BoldModel1
    Left = 96
    Top = 175
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = False
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldPersistenceHandleDB1
    Left = 92
    Top = 123
  end
  object BoldModel1: TBoldModel
    UMLModelMode = ummNone
    Boldify.EnforceDefaultUMLCase = False
    Boldify.DefaultNavigableMultiplicity = '0..1'
    Boldify.DefaultNonNavigableMultiplicity = '0..*'
    Left = 92
    Top = 75
    Model = (
      'VERSION 19'
      '(Model'
      #9'"ImageDemoClasses"'
      #9'"BusinessClassesRoot"'
      #9'""'
      #9'""'
      
        #9'"_Boldify.boldified=True,_Boldify.RootClass=BusinessClassesRoot' +
        ',_BoldInternal.flattened=True,_BoldInternal.ModelErrors=,Bold.De' +
        'lphiName=<Name>,Bold.UnitName=<Name>,Bold.RootClass=BusinessClas' +
        'sesRoot"'
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
      #9#9#9'"ImageClass"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Image"'
      #9#9#9#9#9'"TypedBlob"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent,Bold.AllowNULL=True"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Description"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent,Bold.AllowNULL=True"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9')'
      #9'(Associations'
      #9')'
      ')')
  end
  object blhImages: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'ImageClass.allInstances'
    Left = 220
    Top = 83
  end
  object BoldPersistenceHandleDB1: TBoldPersistenceHandleDB
    BoldModel = BoldModel1
    ClockLogGranularity = '0:0:0.0'
    DatabaseAdapter = BoldDatabaseAdapterIB1
    Left = 40
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
    Left = 40
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
    Left = 40
    Top = 184
  end
end
