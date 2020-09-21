object frmMain: TfrmMain
  Left = -4
  Top = -3
  Width = 872
  Height = 449
  Caption = 'Association Class Example'
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
  object Label3: TLabel
    Left = 8
    Top = 4
    Width = 52
    Height = 13
    Caption = 'All Persons'
  end
  object Label1: TLabel
    Left = 293
    Top = 4
    Width = 48
    Height = 13
    Caption = 'Employers'
  end
  object Label4: TLabel
    Left = 397
    Top = 4
    Width = 142
    Height = 13
    Caption = 'The jobs of the current person'
  end
  object Label5: TLabel
    Left = 8
    Top = 140
    Width = 116
    Height = 13
    Caption = 'All Jobs (the link objects)'
  end
  object Label6: TLabel
    Left = 8
    Top = 244
    Width = 66
    Height = 13
    Caption = 'All Companies'
  end
  object Label2: TLabel
    Left = 296
    Top = 244
    Width = 51
    Height = 13
    Caption = 'Employees'
  end
  object Label7: TLabel
    Left = 400
    Top = 244
    Width = 153
    Height = 13
    Caption = 'The jobs of the current company'
  end
  object AllPersonsGrid: TBoldGrid
    Left = 8
    Top = 20
    Width = 276
    Height = 113
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = AllPersons
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
        BoldProperties.Expression = 'job.salary->Sum'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'TotalSalary'
      end
      item
        BoldProperties.Expression = 'employer->Size'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = '# Jobs'
      end
      item
        BoldProperties.Expression = 'job.Salary->Average->Round'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Average Pay'
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
      73
      64
      42
      72)
  end
  object PersonEmployersListBox: TBoldListBox
    Left = 293
    Top = 20
    Width = 97
    Height = 81
    Hint = 'Drop a company here to create a job'
    Alignment = taLeftJustify
    BoldHandle = PersonEmployers
    BoldProperties.InternalDrag = False
    BoldProperties.NilElementMode = neNone
    BoldRowProperties.Expression = 'name'
    DragMode = dmAutomatic
    ItemHeight = 16
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object PersonNavigator: TBoldNavigator
    Left = 293
    Top = 108
    Width = 96
    Height = 25
    Hint = 'Create / remove Persons'
    BoldHandle = AllPersons
    ParentShowHint = False
    ShowHint = True
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
  object PersonJobGrid: TBoldGrid
    Left = 397
    Top = 20
    Width = 275
    Height = 113
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = PersonJobs
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
        BoldProperties.Expression = 'employer.name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Name of Employer'
      end
      item
        BoldProperties.Expression = 'salary'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Salary'
      end
      item
        BoldProperties.Expression = 'title'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Title'
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
      93
      58
      100)
  end
  object JobGrid: TBoldGrid
    Left = 8
    Top = 156
    Width = 665
    Height = 81
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = AllJobs
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
        BoldProperties.Expression = 'title'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Title'
      end
      item
        BoldProperties.Expression = 'salary'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Salary'
      end
      item
        BoldProperties.Expression = 'employer.name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Name of Employer'
      end
      item
        BoldProperties.Expression = 'employee.name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Name of Employee'
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      264
      59
      153
      164)
  end
  object CompanyGrid: TBoldGrid
    Left = 8
    Top = 260
    Width = 279
    Height = 121
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = AllCompanies
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
        BoldProperties.Expression = 'job.salary->Sum'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'TotalSalary'
      end
      item
        BoldProperties.Expression = 'employee->Size'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = '# Employees'
      end
      item
        BoldProperties.Expression = 'job.salary->Average->Round'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Avg Salary'
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 5
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      64
      58
      68
      64)
  end
  object CompanyEmployeesListBox: TBoldListBox
    Left = 296
    Top = 260
    Width = 97
    Height = 89
    Hint = 'Drop a person here to create a job'
    Alignment = taLeftJustify
    BoldHandle = CompanyEmployees
    BoldProperties.InternalDrag = False
    BoldProperties.NilElementMode = neNone
    BoldRowProperties.Expression = 'name'
    DragMode = dmAutomatic
    ItemHeight = 16
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
  end
  object CompanyNavigator: TBoldNavigator
    Left = 296
    Top = 356
    Width = 96
    Height = 25
    Hint = 'Create / remove Companies'
    BoldHandle = AllCompanies
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
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
  object CompanyJobGrid: TBoldGrid
    Left = 400
    Top = 260
    Width = 271
    Height = 121
    AddNewAtEnd = False
    BoldAutoColumns = False
    BoldShowConstraints = False
    BoldHandle = CompanyJobs
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
        BoldProperties.Expression = 'employee.name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Name of Employee'
      end
      item
        BoldProperties.Expression = 'salary'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Salary'
      end
      item
        BoldProperties.Expression = 'title'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Title.Caption = 'Title'
      end>
    DefaultRowHeight = 17
    EnableColAdjust = False
    TabOrder = 8
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    ColWidths = (
      17
      95
      54
      98)
  end
  object Button1: TButton
    Left = 8
    Top = 388
    Width = 109
    Height = 25
    Action = BoldActivateSystemAction1
    TabOrder = 9
  end
  object Button2: TButton
    Left = 124
    Top = 388
    Width = 109
    Height = 25
    Action = BoldIBDatabaseAction1
    TabOrder = 10
  end
  object AllPersons: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Employee.allInstances'
    Left = 144
    Top = 64
  end
  object PersonEmployers: TBoldListHandle
    RootHandle = AllPersons
    Expression = 'employer'
    Left = 328
    Top = 56
  end
  object PersonJobs: TBoldListHandle
    RootHandle = AllPersons
    Expression = 'job'
    Left = 512
    Top = 56
  end
  object AllJobs: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Job.allInstances'
    Left = 128
    Top = 192
  end
  object AllCompanies: TBoldListHandle
    RootHandle = BoldSystemHandle1
    Expression = 'Company.allInstances'
    Left = 112
    Top = 296
  end
  object CompanyEmployees: TBoldListHandle
    RootHandle = AllCompanies
    Expression = 'employee'
    Left = 336
    Top = 280
  end
  object CompanyJobs: TBoldListHandle
    RootHandle = AllCompanies
    Expression = 'job'
    Left = 520
    Top = 288
  end
  object BoldModel1: TBoldModel
    UMLModelMode = ummNone
    Boldify.EnforceDefaultUMLCase = False
    Boldify.DefaultNavigableMultiplicity = '0..1'
    Boldify.DefaultNonNavigableMultiplicity = '0..*'
    Left = 776
    Top = 36
    Model = (
      'VERSION 19'
      '(Model'
      #9'"AssociationClassExampleClasses"'
      #9'"BusinessClassesRoot"'
      #9'""'
      #9'""'
      
        #9'"_Boldify.boldified=True,_BoldInternal.flattened=True,_BoldInte' +
        'rnal.toolId=35331A920220,Bold.DelphiName=<Name>,Bold.RootClass=B' +
        'usinessClassesRoot,Bold.UnitName=AssociationClassExampleClasses"'
      #9'(Classes'
      #9#9'(Class'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'"<NONE>"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"_Boldify.autoCreated=True,persistence=Persistent,Bold.TableN' +
        'ame=<Prefix>_OBJECT"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Employee"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"_BoldInternal.toolId=35331A9A025C,persistence=Persistent"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"name"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"_BoldInternal.toolId=35331AC20015,persistence=Persistent"'
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
      #9#9#9#9#9'"=False"'
      
        #9#9#9#9#9'"_BoldInternal.toolId=35BB4CD2030F,Bold.OperationKind=Overr' +
        'ide"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Company"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"_BoldInternal.toolId=35331AA0019D,persistence=Persistent"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"name"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"_BoldInternal.toolId=35331B550084,persistence=Persistent"'
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
      #9#9#9#9#9'"=False"'
      
        #9#9#9#9#9'"_BoldInternal.toolId=35BB4CEB0383,Bold.OperationKind=Overr' +
        'ide"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Job"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"_BoldInternal.toolId=35331AAE01B1,persistence=Persistent"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"title"'
      #9#9#9#9#9'"String"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"_BoldInternal.toolId=35331B1D0387,persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"salary"'
      #9#9#9#9#9'"Currency"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"_BoldInternal.toolId=35331B3A018A,persistence=Persistent"'
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
      #9#9#9#9#9'"=False"'
      
        #9#9#9#9#9'"_BoldInternal.toolId=35BB4D0603B4,Bold.OperationKind=Overr' +
        'ide"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9')'
      #9'(Associations'
      #9#9'(Association'
      #9#9#9'"Job"'
      #9#9#9'"Job"'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=Persistent,_BoldInternal.toolId=35331AA601CD,Bol' +
        'd.DelphiName=<Name>"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Employer"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Employee"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..n"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=35331AA701BB,Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Employee"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Company"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..n"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"_BoldInternal.toolId=35331AA701CF,Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9')'
      ')')
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = True
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldPersistenceHandleDB1
    Left = 776
    Top = 84
  end
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = BoldModel1
    Left = 776
    Top = 128
  end
  object ActionList1: TActionList
    Left = 776
    Top = 224
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
  object BoldPersistenceHandleDB1: TBoldPersistenceHandleDB
    BoldModel = BoldModel1
    ClockLogGranularity = '0:0:0.0'
    DatabaseAdapter = BoldDatabaseAdapterIB1
    Left = 488
    Top = 384
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
    Left = 528
    Top = 384
  end
  object IBDatabase1: TIBDatabase
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    IdleTimer = 0
    SQLDialect = 3
    TraceFlags = []
    Left = 568
    Top = 384
  end
  object BoldUMLRoseLink1: TBoldUMLRoseLink
    FileName = 
      'C:\vss\dev\BfD\examples\Simple\ObjectSpace\AssociationClass\Empl' +
      'oyer.mdl'
    BoldModel = BoldModel1
    Left = 624
    Top = 380
  end
end
