object Form1: TForm1
  Left = 3
  Top = 0
  Width = 503
  Height = 393
  Caption = 'Persistence Mapping Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 52
    Height = 13
    Caption = 'Jet aircraft:'
  end
  object Label2: TLabel
    Left = 8
    Top = 152
    Width = 109
    Height = 13
    Caption = 'Propellerdriven Aircraft:'
  end
  object pbcParentOrChild: TPageControl
    Left = 0
    Top = 0
    Width = 495
    Height = 324
    ActivePage = tsParent
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    object tsParent: TTabSheet
      Caption = 'Parent Mapping'
      object lblJet: TLabel
        Left = 8
        Top = 8
        Width = 52
        Height = 13
        Caption = 'Jet aircraft:'
      end
      object lblProp: TLabel
        Left = 8
        Top = 152
        Width = 80
        Height = 13
        Caption = 'Propeller Aircraft:'
      end
      object bgrdJet: TBoldGrid
        Left = 8
        Top = 24
        Width = 204
        Height = 121
        AddNewAtEnd = False
        BoldAutoColumns = False
        BoldShowConstraints = False
        BoldHandle = blhJet
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
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        ColWidths = (
          17
          182)
      end
      object bgrdProp: TBoldGrid
        Left = 8
        Top = 168
        Width = 204
        Height = 121
        AddNewAtEnd = False
        BoldAutoColumns = False
        BoldShowConstraints = False
        BoldHandle = blhProp
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
          end
          item
            BoldProperties.Expression = 'noOfBlades'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'No. of Blades'
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
          104
          74)
      end
      object bnavJet: TBoldNavigator
        Left = 219
        Top = 24
        Width = 50
        Height = 25
        BoldHandle = blhJet
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
      object bnavProp: TBoldNavigator
        Left = 219
        Top = 168
        Width = 50
        Height = 25
        BoldHandle = blhProp
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
      object bchkThrustVector: TBoldCheckBox
        Left = 224
        Top = 72
        Width = 97
        Height = 17
        BoldHandle = blhJet
        BoldProperties.Expression = 'thrustVector'
        BoldProperties.ApplyPolicy = bapChange
        Caption = 'Thrust Vector'
        ReadOnly = False
        TabOrder = 4
      end
    end
    object tsChild: TTabSheet
      Caption = 'Child Mapping'
      ImageIndex = 1
      object lblTruck: TLabel
        Left = 8
        Top = 8
        Width = 31
        Height = 13
        Caption = 'Truck:'
      end
      object lblBus: TLabel
        Left = 8
        Top = 152
        Width = 21
        Height = 13
        Caption = 'Bus:'
      end
      object bgrdTruck: TBoldGrid
        Left = 8
        Top = 24
        Width = 204
        Height = 121
        AddNewAtEnd = False
        BoldAutoColumns = False
        BoldShowConstraints = False
        BoldHandle = blhTruck
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
            BoldProperties.Expression = 'model'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Model'
          end
          item
            BoldProperties.Expression = 'maxLoad'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Max Load'
          end>
        DefaultRowHeight = 17
        EnableColAdjust = False
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        ColWidths = (
          17
          107
          71)
      end
      object bgrdBus: TBoldGrid
        Left = 8
        Top = 168
        Width = 204
        Height = 121
        AddNewAtEnd = False
        BoldAutoColumns = False
        BoldShowConstraints = False
        BoldHandle = blhBus
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
            BoldProperties.Expression = 'model'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'Model'
          end
          item
            BoldProperties.Expression = 'noOfPassengers'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Title.Caption = 'No. of Passengers'
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
          89
          92)
      end
      object bnavBus: TBoldNavigator
        Left = 219
        Top = 168
        Width = 50
        Height = 25
        BoldHandle = blhBus
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
      object bnavTruck: TBoldNavigator
        Left = 219
        Top = 24
        Width = 50
        Height = 25
        BoldHandle = blhTruck
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
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 324
    Width = 495
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 12
      Width = 101
      Height = 25
      Action = BoldActivateSystemAction
      TabOrder = 0
    end
    object Button2: TButton
      Left = 116
      Top = 12
      Width = 101
      Height = 25
      Action = BoldIBDatabaseAction1
      TabOrder = 1
    end
  end
  object BoldModel1: TBoldModel
    UMLModelMode = ummNone
    Boldify.EnforceDefaultUMLCase = False
    Boldify.DefaultNavigableMultiplicity = '0..1'
    Boldify.DefaultNonNavigableMultiplicity = '0..*'
    Left = 376
    Top = 72
    Model = (
      'VERSION 19'
      '(Model'
      #9'"ParentChildMappingClasses"'
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
      #9#9#9'"Aircraft"'
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
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Vehicle"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'TRUE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=Persistent,Bold.TableMapping=Children,Bold.Versi' +
        'oned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Model"'
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
      #9#9#9'"Jet"'
      #9#9#9'"Aircraft"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=Persistent,Bold.TableMapping=Parent,Bold.Version' +
        'ed=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"ThrustVector"'
      #9#9#9#9#9'"Boolean"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent,Bold.AllowNull=True"'
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
      #9#9#9'"Propeller"'
      #9#9#9'"Aircraft"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=Persistent,Bold.TableMapping=Parent,Bold.Version' +
        'ed=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"NoOfBlades"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent,Bold.AllowNull=True"'
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
      #9#9#9'"Bus"'
      #9#9#9'"Vehicle"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"NoOfPassengers"'
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
      #9#9#9'"Truck"'
      #9#9#9'"Vehicle"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.Versioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"MaxLoad"'
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
      #9')'
      #9'(Associations'
      #9')'
      ')')
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = True
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldPersistenceHandleDB1
    Left = 376
    Top = 120
  end
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = BoldModel1
    Left = 376
    Top = 168
  end
  object blhJet: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Jet.allInstances'
    Left = 51
    Top = 88
  end
  object blhProp: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Propeller.allInstances'
    Left = 51
    Top = 232
  end
  object blhTruck: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Truck.allInstances'
    Left = 124
    Top = 88
  end
  object blhBus: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Bus.allInstances'
    Left = 124
    Top = 232
  end
  object ActionList: TActionList
    Left = 268
    Top = 272
    object BoldActivateSystemAction: TBoldActivateSystemAction
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
  object BoldPersistenceHandleDB1: TBoldPersistenceHandleDB
    BoldModel = BoldModel1
    ClockLogGranularity = '0:0:0.0'
    DatabaseAdapter = BoldDatabaseAdapterIB1
    Left = 416
    Top = 72
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
    Left = 416
    Top = 128
  end
  object IBDatabase1: TIBDatabase
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    IdleTimer = 0
    SQLDialect = 3
    TraceFlags = []
    Left = 416
    Top = 176
  end
end
