object dm: Tdm
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 479
  Width = 741
  object BoldSnooperHandle1: TBoldSnooperHandle
    NextPersistenceHandle = BoldPersistenceHandleDB1
    BoldModel = dm_Model1.BoldModel1
    CheckDatabaseLock = False
    PropagatorHandle = BoldPropagatorHandleCOM1
    UseClassEvents = False
    UseMemberLevelOSS = False
    UseSubscriptions = False
    Left = 432
    Top = 104
  end
  object BoldExternalObjectSpaceEventHandler1: TBoldExternalObjectSpaceEventHandler
    BoldSystemHandle = BoldSystemHandle1
    UseMemberLevelOSS = False
    Left = 432
    Top = 168
  end
  object BoldListenerHandle1: TBoldListenerHandle
    AutoStart = True
    LeaseDuration = 300000
    ExtendLeaseAfter = 80
    PollingInterval = 5000
    AutoExtendLease = False
    Dequeuer = BoldExternalObjectSpaceEventHandler1
    ClientIdentifierString = 'Multi ClientTest'
    PropagatorHandle = BoldPropagatorHandleCOM1
    Left = 432
    Top = 294
  end
  object BoldIdAdderHandle1: TBoldIdAdderHandle
    NextPersistenceHandle = BoldSnooperHandle1
    BoldListener = BoldListenerHandle1
    Left = 256
    Top = 32
  end
  object BoldListHandle1: TBoldListHandle
    StaticSystemHandle = dm.BoldSystemHandle1
    Left = 432
    Top = 228
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = True
    SystemTypeInfoHandle = dm_Model1.BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldIdAdderHandle1
    Left = 56
    Top = 48
  end
  object ActionList1: TActionList
    Left = 216
    Top = 333
  end
  object blhClassA: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'ClassA.allinstances'
    Left = 176
    Top = 120
  end
  object blhThing: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Thing.allInstances'
    Left = 176
    Top = 192
  end
  object DirectBoldHandle: TBoldSystemHandle
    IsDefault = False
    SystemTypeInfoHandle = dm_Model1.BoldSystemTypeInfoHandle1
    Active = False
    Left = 56
    Top = 360
  end
  object blhhitList: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'HitList.allInstances'
    Left = 56
    Top = 120
  end
  object blhSong: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Song.allInstances'
    Left = 56
    Top = 192
  end
  object BoldPropagatorHandleCOM1: TBoldPropagatorHandleCOM
    Active = True
    ConnectionHandle = BoldComConnectionHandle1
    Left = 272
    Top = 192
  end
  object BoldComConnectionHandle1: TBoldComConnectionHandle
    AutoConnect = True
    ServerCLSID = '{11C6E940-CEC6-45BA-873D-27854A82A023}'
    ServerName = 'BoldPropagator.EnterprisePropagator.PropagatorConnection'
    Left = 136
    Top = 240
  end
  object BoldPersistenceHandleDB1: TBoldPersistenceHandleDB
    BoldModel = dm_Model1.BoldModel1
    ClockLogGranularity = '0:0:0.0'
    DatabaseAdapter = BoldDatabaseAdapterUniDAC1
    Left = 632
    Top = 56
  end
  object UniConnection1: TUniConnection
    ProviderName = 'SQL Server'
    Database = 'multiclient'
    Username = 'dba'
    Server = 'localhost'
    Left = 632
    Top = 320
    EncryptedPassword = '92FF9EFF8CFF8BFF9AFF8DFF94FF9AFF86FF'
  end
  object BoldDatabaseAdapterUniDAC1: TBoldDatabaseAdapterUniDAC
    SQLDatabaseConfig.DefaultSystemMapper = '<Default>'
    SQLDatabaseConfig.DefaultObjectMapper = '<Default>'
    SQLDatabaseConfig.IfTemplate = 'IF <Condition> BEGIN <SQLStatement> END'
    SQLDatabaseConfig.ColumnExistsTemplate = 
      'SELECT * FROM SYS.COLUMNS WHERE UPPER(NAME) = UPPER(N'#39'<ColumnNam' +
      'e>'#39') AND OBJECT_ID = OBJECT_ID(UPPER(N'#39'<TableName>'#39'))'
    SQLDatabaseConfig.TableExistsTemplate = 'SELECT * FROM SYS.TABLES WHERE UPPER(NAME)=UPPER('#39'<TableName>'#39')'
    SQLDatabaseConfig.IndexExistsTemplate = 
      'SELECT NAME FROM SYS.INDEXES WHERE UPPER(NAME)=UPPER('#39'<IndexName' +
      '>'#39') AND OBJECT_ID = OBJECT_ID(UPPER(N'#39'<TableName>'#39'))'
    SQLDatabaseConfig.IndexColumnExistsTemplate = 
      'SELECT IND.NAME FROM SYS.INDEXES IND INNER JOIN SYS.INDEX_COLUMN' +
      'S IC ON  IND.OBJECT_ID = IC.OBJECT_ID AND IND.INDEX_ID = IC.INDE' +
      'X_ID INNER JOIN SYS.COLUMNS COL ON IC.OBJECT_ID = COL.OBJECT_ID ' +
      'AND IC.COLUMN_ID = COL.COLUMN_ID WHERE IND.OBJECT_ID = OBJECT_ID' +
      '(UPPER(N'#39'<TableName>'#39')) AND UPPER(COL.NAME) = UPPER('#39'<IndexColum' +
      'nName>'#39')'
    SQLDatabaseConfig.ColumnTypeForDate = 'DATETIME'
    SQLDatabaseConfig.ColumnTypeForTime = 'DATETIME'
    SQLDatabaseConfig.ColumnTypeForDateTime = 'DATETIME'
    SQLDatabaseConfig.ColumnTypeForBlob = 'VARBINARY(MAX)'
    SQLDatabaseConfig.ColumnTypeForFloat = 'DECIMAL (28,10)'
    SQLDatabaseConfig.ColumnTypeForCurrency = 'DECIMAL (28,10)'
    SQLDatabaseConfig.ColumnTypeForString = 'VARCHAR(%d)'
    SQLDatabaseConfig.ColumnTypeForUnicodeString = 'NVARCHAR(%d)'
    SQLDatabaseConfig.ColumnTypeForText = 'VARCHAR(MAX)'
    SQLDatabaseConfig.ColumnTypeForUnicodeText = 'NVARCHAR(MAX)'
    SQLDatabaseConfig.ColumnTypeForInteger = 'INTEGER'
    SQLDatabaseConfig.ColumnTypeForSmallInt = 'SMALLINT'
    SQLDatabaseConfig.ColumnTypeForInt64 = 'BIGINT'
    SQLDatabaseConfig.ColumnTypeForGUID = 'UNIQUEIDENTIFIER'
    SQLDatabaseConfig.DropColumnTemplate = 
      'DECLARE @CONSTRAINTNAME NVARCHAR(200) SELECT @CONSTRAINTNAME=OD.' +
      'NAME   FROM   SYSOBJECTS OT, SYSCOLUMNS C, SYSOBJECTS OD   WHERE' +
      '  UPPER(OT.NAME) = UPPER('#39'<TableName>'#39')   AND  OT.ID          = ' +
      'C.ID   AND  UPPER(C.NAME)  = UPPER('#39'<ColumnName>'#39')   AND  C.CDEF' +
      'AULT     = OD.ID IF @CONSTRAINTNAME IS NOT NULL   EXEC('#39'ALTER TA' +
      'BLE <TableName> DROP CONSTRAINT '#39' + @CONSTRAINTNAME) IF EXISTS (' +
      'SELECT * FROM SYSCOLUMNS WHERE ID=OBJECT_ID('#39'<TableName>'#39') AND U' +
      'PPER(NAME)=UPPER('#39'<ColumnName>'#39')) EXEC('#39'ALTER TABLE <TableName> ' +
      'DROP COLUMN <ColumnName>'#39')'
    SQLDatabaseConfig.DropTableTemplate = 'DROP TABLE <TableName>'
    SQLDatabaseConfig.IndexInfoTemplate = 
      'SELECT IND.NAME INDEXNAME, IND.IS_PRIMARY_KEY ISPRIMARY, IND.IS_' +
      'UNIQUE ISUNIQUE, COL.NAME COLUMNNAME FROM SYS.INDEXES IND INNER ' +
      'JOIN SYS.INDEX_COLUMNS IC ON IND.OBJECT_ID = IC.OBJECT_ID AND IN' +
      'D.INDEX_ID = IC.INDEX_ID INNER JOIN SYS.COLUMNS COL ON IC.OBJECT' +
      '_ID = COL.OBJECT_ID AND IC.COLUMN_ID = COL.COLUMN_ID WHERE UPPER' +
      '(OBJECT_NAME(IND.OBJECT_ID))=UPPER('#39'<TableName>'#39') ORDER BY INDEX' +
      'NAME, INDEX_COLUMN_ID'
    SQLDatabaseConfig.DropIndexTemplate = 'DROP INDEX <TableName>.<IndexName>'
    SQLDatabaseConfig.EvolveDropsUnknownIndexes = True
    SQLDatabaseConfig.MaxBatchQueryLength = 67108864
    SQLDatabaseConfig.MaxBatchQueryParams = 2000
    SQLDatabaseConfig.BatchQuerySeparator = ';'
    SQLDatabaseConfig.MultiRowInsertLimit = 1000
    SQLDatabaseConfig.SQLforNull = 'NULL'
    SQLDatabaseConfig.SQLforNotNull = 'NOT NULL'
    SQLDatabaseConfig.QuoteNonStringDefaultValues = False
    SQLDatabaseConfig.SupportsConstraintsInCreateTable = True
    SQLDatabaseConfig.SupportsStringDefaultValues = True
    SQLDatabaseConfig.DBGenerationMode = dbgQuery
    SQLDatabaseConfig.DatabaseCaseSensitiveTemplate = 'EXECUTE sp_helpsort'
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
    SQLDatabaseConfig.QuoteLeftBracketInLike = False
    SQLDatabaseConfig.SqlScriptTerminator = ';'
    SQLDatabaseConfig.SqlScriptCommentStart = '/* '
    SQLDatabaseConfig.SqlScriptCommentStop = ' */'
    SQLDatabaseConfig.SqlScriptStartTransaction = 'START TRANSACTION'
    SQLDatabaseConfig.SqlScriptCommitTransaction = 'COMMIT'
    SQLDatabaseConfig.SqlScriptRollBackTransaction = 'ROLLBACK'
    CustomIndexes = <>
    Connection = UniConnection1
    DatabaseEngine = dbeSQLServer
    Left = 632
    Top = 264
  end
end
