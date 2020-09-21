object dmPersistence: TdmPersistence
  OldCreateOrder = False
  Left = 265
  Top = 349
  Height = 200
  Width = 400
  object ObjectUpgrader: TBoldObjectUpgraderHandle
    Config = <
      item
        ExpressionName = 'Person'
        UpgradeOlderThanVersion = 2
        OnUpgradeObject = ObjectUpgrader_Person_UpgradeObject
      end
      item
        ExpressionName = 'Order'
        UpgradeOlderThanVersion = 2
        OnUpgradeObject = ObjectUpgrader_Order_UpgradeObject
      end
      item
        ExpressionName = 'Product'
        UpgradeOlderThanVersion = 2
        OnUpgradeObject = ObjectUpgrader_Product_UpgradeObject
      end>
    PersistenceHandle = BoldPersistenceHandleDB1
    SystemTypeInfoHandle = dmSystemTypeInfo.SystemTypeInfo
    Left = 48
    Top = 56
  end
  object BoldPersistenceHandleDB1: TBoldPersistenceHandleDB
    BoldModel = dmModel.Model
    ClockLogGranularity = '0:0:0.0'
    UpgraderHandle = ObjectUpgrader
    DatabaseAdapter = BoldDatabaseAdapterIB1
    Left = 128
    Top = 8
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
    Left = 128
    Top = 64
  end
  object IBDatabase1: TIBDatabase
    DatabaseName = '..\ModelEvolution.gdb'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    IdleTimer = 0
    SQLDialect = 3
    TraceFlags = []
    Left = 128
    Top = 112
  end
end
