object DataModule1: TDataModule1
  OldCreateOrder = True
  Height = 394
  Width = 612
  object BoldModel1: TBoldModel
    UMLModelMode = ummNone
    Boldify.EnforceDefaultUMLCase = False
    Boldify.DefaultNavigableMultiplicity = '0..1'
    Boldify.DefaultNonNavigableMultiplicity = '0..*'
    Left = 56
    Top = 8
    Model = (
      'VERSION 19'
      '(Model'
      #9'"BuildingsAndOwners"'
      #9'"BusinessClassesRoot"'
      #9'""'
      #9'""'
      
        #9'"_BoldInternal.toolId=39885BC30059,_BoldInternal.flattened=True' +
        ',_Boldify.boldified=True,_BoldInternal.ModelErrors=,Bold.DelphiN' +
        'ame=<Name>,Bold.ImplementationUses=Dialogs,Bold.UnitName=Buildin' +
        'gClasses,Bold.RootClass=BusinessClassesRoot"'
      #9'(Classes'
      #9#9'(Class'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'"<NONE>"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"_Boldify.autoCreated=True,persistence=persistent,_Boldify.no' +
        'Name=True,Bold.TableName=<Prefix>_OBJECT"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Building"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"_BoldInternal.toolId=39885C170191,persistence=persistent,Bol' +
        'd.FileName=Building.inc"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"ZipCode"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"_BoldInternal.toolId=39885C1702E5,persistence=persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Address"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"_BoldInternal.toolId=39885C1703DF,persistence=persistent"'
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
      
        #9#9#9#9#9'"_BoldInternal.toolId=39885C1800FC,Bold.OperationKind=Overr' +
        'ide"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Ownership"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=persistent,_Boldify.autoCreated=True"'
      #9#9#9'(Attributes'
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
      
        #9#9#9'"_BoldInternal.toolId=39885C180341,persistence=persistent,Bol' +
        'd.FileName=Person.inc"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"FirstName"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"_BoldInternal.toolId=39885C190085,persistence=persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"LastName"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=39885C190189,persistence=persistent,B' +
        'old.Length=25"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Assets"'
      #9#9#9#9#9'"Currency"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"_BoldInternal.toolId=39885C1902CA,persistence=persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"IsMarried"'
      #9#9#9#9#9'"Boolean"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"_BoldInternal.toolId=39885C1903E2,persistence=persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"name"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"derived=True,persistence=transient,\"Bold.DerivationOCL=fi' +
        'rstName + '#39' '#39' + lastname\""'
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
      
        #9#9#9#9#9'"_BoldInternal.toolId=39885C1A0109,Bold.OperationKind=Overr' +
        'ide"'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"BorrowFrom"'
      #9#9#9#9#9'"Lender: TPerson; Amount: Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"_BoldInternal.toolId=39885C1A01DB"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Residential Building"'
      #9#9#9'"Building"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"_BoldInternal.toolId=39885C1A02F3,persistence=persistent,Bol' +
        'd.FileName=ResidentialBuilding.inc"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"TotalRent"'
      #9#9#9#9#9'"Currency"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"_BoldInternal.toolId=39885C1B002E,persistence=persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Capacity"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"derived=False,persistence=persistent"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"ChargeRent"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"_BoldInternal.toolId=39885C1B013C"'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"CompleteCreate"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"_BoldInternal.toolId=39885C1B01C8,Bold.OperationKind=Overr' +
        'ide"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9')'
      #9'(Associations'
      #9#9'(Association'
      #9#9#9'"Residence"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=persistent,_BoldInternal.toolId=39885C1B031D,Bol' +
        'd.DelphiName=<Name>"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Residents"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'"Residential Building"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"*"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      
        #9#9#9#9#9'"_BoldInternal.toolId=39885C1B031E,Bold.Ordered=True,Bold.E' +
        'mbed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Home"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Person"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=39885C1B031F"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"Ownership"'
      #9#9#9'"Ownership"'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=persistent,_BoldInternal.toolId=39885C1C0210,Bol' +
        'd.DelphiName=<Name>"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Owners"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'"Building"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"*"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      
        #9#9#9#9#9'"_BoldInternal.toolId=39885C1C0211,Bold.Ordered=True,Bold.E' +
        'mbed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"OwnedBuildings"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'"Person"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"*"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      
        #9#9#9#9#9'"_BoldInternal.toolId=39885C1C0212,Bold.Ordered=True,Bold.E' +
        'mbed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9')'
      ')')
  end
  object IsRichRenderer: TBoldAsCheckBoxStateRenderer
    OnMayModify = IsRichRendererMayModify
    OnSubscribe = IsRichRendererSubscribe
    OnGetAsCheckBoxState = IsRichRendererGetAsCheckBoxState
    Left = 536
    Top = 16
  end
  object IsRichFilter: TBoldFilter
    OnSubscribe = IsRichFilterSubscribe
    OnFilter = IsRichFilterFilter
    Left = 464
    Top = 80
  end
  object NameComparer: TBoldComparer
    OnSubscribe = NameComparerSubscribe
    OnCompare = NameComparerCompare
    Left = 464
    Top = 16
  end
  object NegativeRedRenderer: TBoldAsStringRenderer
    OnSetFont = NegativeRedRendererSetFont
    Left = 536
    Top = 80
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = True
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldPersistenceHandleDB1
    Left = 56
    Top = 64
  end
  object BoldUMLRoseLink1: TBoldUMLRoseLink
    FileName = 'D:\bold\BfD\examples\Delphi\Compound\Building\Building.mdl'
    BoldModel = BoldModel1
    Left = 52
    Top = 188
  end
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = BoldModel1
    Left = 56
    Top = 128
  end
  object BoldPlaceableAFP1: TBoldPlaceableAFP
    OnGetFormClass = BoldPlaceableAFP1GetFormClass
    OnRetrieveHandle = BoldPlaceableAFP1RetrieveHandle
    Left = 520
    Top = 160
  end
  object BoldPersistenceHandleDB1: TBoldPersistenceHandleDB
    BoldModel = BoldModel1
    ClockLogGranularity = '0:0:0.0'
    DatabaseAdapter = BoldDatabaseAdapterPostgres
    Left = 224
    Top = 8
  end
  object BoldDatabaseAdapterPostgres: TBoldDatabaseAdapterFireDAC
    SQLDatabaseConfig.DefaultSystemMapper = '<Default>'
    SQLDatabaseConfig.DefaultObjectMapper = '<Default>'
    SQLDatabaseConfig.ColumnExistsTemplate = 
      'SELECT column_name FROM information_schema.columns WHERE upper(t' +
      'able_name)=upper('#39'<TableName>'#39') and upper(column_name)=upper('#39'<C' +
      'olumnName>'#39')'
    SQLDatabaseConfig.IndexColumnExistsTemplate = 
      'select indexname from pg_indexes where upper(tablename) = upper(' +
      #39'<TableName>'#39')'
    SQLDatabaseConfig.ColumnTypeForDate = 'DATE'
    SQLDatabaseConfig.ColumnTypeForTime = 'TIME'
    SQLDatabaseConfig.ColumnTypeForDateTime = 'TIMESTAMP'
    SQLDatabaseConfig.ColumnTypeForBlob = 'BYTEA'
    SQLDatabaseConfig.ColumnTypeForFloat = 'NUMERIC'
    SQLDatabaseConfig.ColumnTypeForCurrency = 'NUMERIC'
    SQLDatabaseConfig.ColumnTypeForString = 'VARCHAR(%d)'
    SQLDatabaseConfig.ColumnTypeForUnicodeString = 'NVARCHAR(%d)'
    SQLDatabaseConfig.ColumnTypeForText = 'VARCHAR(MAX)'
    SQLDatabaseConfig.ColumnTypeForUnicodeText = 'NVARCHAR(MAX)'
    SQLDatabaseConfig.ColumnTypeForInteger = 'INTEGER'
    SQLDatabaseConfig.ColumnTypeForSmallInt = 'SMALLINT'
    SQLDatabaseConfig.ColumnTypeForInt64 = 'BIGINT'
    SQLDatabaseConfig.ColumnTypeForGUID = 'UUID'
    SQLDatabaseConfig.CreateDatabaseTemplate = 'CREATE DATABASE <DatabaseName>'
    SQLDatabaseConfig.DropDatabaseTemplate = 'DROP DATABASE <DatabaseName>'
    SQLDatabaseConfig.DatabaseExistsTemplate = 
      'select exists(SELECT datname FROM pg_catalog.pg_database WHERE l' +
      'ower(datname) = lower('#39'<DatabaseName>'#39'));'
    SQLDatabaseConfig.DropColumnTemplate = 'ALTER TABLE <TableName> DROP <ColumnName>'
    SQLDatabaseConfig.DropTableTemplate = 'DROP TABLE <TableName>'
    SQLDatabaseConfig.DropIndexTemplate = 'DROP INDEX <IndexName>'
    SQLDatabaseConfig.EvolveDropsUnknownIndexes = True
    SQLDatabaseConfig.MaxDbIdentifierLength = 63
    SQLDatabaseConfig.MaxIndexNameLength = 63
    SQLDatabaseConfig.BatchQuerySeparator = ';'
    SQLDatabaseConfig.MultiRowInsertLimit = 1000
    SQLDatabaseConfig.SQLforNull = 'NULL'
    SQLDatabaseConfig.SQLforNotNull = 'NOT NULL'
    SQLDatabaseConfig.QuoteNonStringDefaultValues = False
    SQLDatabaseConfig.SupportsConstraintsInCreateTable = True
    SQLDatabaseConfig.SupportsStringDefaultValues = True
    SQLDatabaseConfig.DBGenerationMode = dbgQuery
    SQLDatabaseConfig.ReservedWords.Strings = (
      
        'ALL, ANALYSE, AND, ANY, ARRAY, AS, ASC, ASYMMETRIC, AUTHORIZATIO' +
        'N,'
      
        'BETWEEN, BINARY, BOOLEAN, BOTH, CASE, CAST, CHAR, CHARACTER, CHE' +
        'CK,'
      
        'CMIN, COALESCE, COLLATE, COLUMN, CONSTRAINT, CONVERT, CREATE, CR' +
        'OSS,'
      'CURRENT_DATE, CURRENT_ROLE, CURRENT_TIME, CURRENT_TIMESTAMP,'
      'CURRENT_USER, DEC, DECIMAL, DEFAULT, DEFERRABLE, SEC, ELSE, END,'
      
        'EXCEPT, EXISTS, EXTRACT, FALSE, FLOAT, FOR, FOREIGN, FREEZE, FRO' +
        'M,'
      
        'FULL, GRANT, GREATEST, GROUP, HAVING, ILIKE, IN, INITIALLY, INNE' +
        'R,'
      
        'INOUT, INT, INTEGER, INTERSECT, INTERVAL, INTO, IS, ISNULL, JOIN' +
        ','
      'LEADING, LEAST, LEFT, LIKE, LIMIT, LOCALTIME, LOCALTIMESTAMP,'
      'NATIONAL, NATURAL, NCHAR, NEW, NONE, NOT, NOTNULL, NULL, NULLIF,'
      
        'NUMERIC, OFF, OFFSET, OLD, ON, ONLY, OR, ORDER, OUT, OUTER, OVER' +
        'LAPS,'
      'OVERLAY, PLI, POSITION, PRECISION, PRIMARY, REAL, REFERENCES,'
      'RETURNING, RIGHT, ROW, SELECT, SESSION_USER, SETOF, SIMILAR,'
      
        'SMALLINT, SOME, SUBSTRING, SYMMETRIC, TABLE, THEN, TIME, TIMESTA' +
        'MP,'
      
        'TOP_LEVEL_COUNT, TRAILING, TREAT, TRIM, TRUE, UNION, UNIQUE, USE' +
        'R,'
      'USING, VALUES, VARCHAR, VERBOSE, WHEN, WHERE')
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
    Connection = FDConnectionPostgres
    DatabaseEngine = dbePostgres
    Left = 216
    Top = 208
  end
  object FDConnectionSQLServer: TFDConnection
    Params.Strings = (
      'User_Name=sa'
      'Password=masterkey'
      'Server=DESKTOP-JJPVFK3'
      'OSAuthent=Yes'
      'ApplicationName=Enterprise/Architect/Ultimate'
      'Workstation=DESKTOP-JJPVFK3'
      'MARS=yes'
      'Database=Buildings'
      'DriverID=MSSQL')
    TxOptions.Isolation = xiRepeatableRead
    LoginPrompt = False
    Left = 224
    Top = 128
  end
  object BoldConstraintValidatorOnModify: TBoldConstraintValidator
    StaticSystemHandle = BoldSystemHandle1
    Constraints = <
      item
        Context = 'Residential_Building'
        Expression = 'residents->size <= capacity'
        ErrorMessage = #39'Building capacity limit is '#39' + capacity.asString'
      end>
    ValidationMode = vmOnModify
    Enabled = True
    Left = 488
    Top = 320
  end
  object BoldConstraintValidatorOnUpdate: TBoldConstraintValidator
    StaticSystemHandle = BoldSystemHandle1
    Constraints = <
      item
        Context = 'Person'
        Expression = 
          '(ownedBuildings->filterOnType(Residential_Building)->notEmpty'#13#10')' +
          ' implies (home <> nil)'#13#10
        ErrorMessage = 
          'name + '#39' can not be homeless while owning at least one non full ' +
          'residential building'#39
      end>
    ValidationMode = vmPreUpdate
    Enabled = True
    Left = 488
    Top = 256
  end
  object FDConnectionPostgres: TFDConnection
    Params.Strings = (
      'Password=masterkey'
      'Database=Buildings'
      'User_Name=postgres'
      'DriverID=PG')
    TxOptions.Isolation = xiRepeatableRead
    LoginPrompt = False
    Left = 216
    Top = 256
  end
  object BoldDatabaseAdapterSQLServer: TBoldDatabaseAdapterFireDAC
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
    SQLDatabaseConfig.LongStringLimit = 4000
    SQLDatabaseConfig.ColumnTypeForInteger = 'INTEGER'
    SQLDatabaseConfig.ColumnTypeForSmallInt = 'SMALLINT'
    SQLDatabaseConfig.ColumnTypeForInt64 = 'BIGINT'
    SQLDatabaseConfig.ColumnTypeForGUID = 'UNIQUEIDENTIFIER'
    SQLDatabaseConfig.CreateDatabaseTemplate = 'USE MASTER;GO;CREATE DATABASE <DatabaseName>'
    SQLDatabaseConfig.DropDatabaseTemplate = 'DROP DATABASE <DatabaseName>'
    SQLDatabaseConfig.DatabaseExistsTemplate = 
      'IF EXISTS (SELECT name FROM master.sys.databases WHERE name = N'#39 +
      '<DatabaseName>'#39')'
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
    Connection = FDConnectionSQLServer
    DatabaseEngine = dbeSQLServer
    Left = 224
    Top = 80
  end
end
