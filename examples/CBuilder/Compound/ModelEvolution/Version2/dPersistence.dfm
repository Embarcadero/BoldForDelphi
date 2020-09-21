object dmPersistence: TdmPersistence
  OldCreateOrder = False
  Left = 294
  Top = 350
  Height = 480
  Width = 696
  object PersistenceHandle: TBoldPersistenceHandleBDE
    Username = 'SYSDBA'
    Password = 'masterkey'
    BoldModel = dmModel.Model
    SystemTablesPrefix = 'BOLD'
    ClockLogGranularity = '0:0:0.0'
    UpgraderHandle = ObjectUpgrader
    SQLDataBaseConfig.ColumnTypeForDate = 'DATE'
    SQLDataBaseConfig.ColumnTypeForTime = 'DATE'
    SQLDataBaseConfig.ColumnTypeForDateTime = 'DATE'
    SQLDataBaseConfig.ColumnTypeForBlob = 'BLOB'
    SQLDataBaseConfig.UseSQL92Joins = False
    DatabaseEngine = dbeUnknown
    DatabaseName = 'ModelEvolution'
    Left = 48
    Top = 8
  end
  object ObjectUpgrader: TBoldObjectUpgraderHandle
    Config = <
      item
        ExpressionName = 'Person'
        UpgradeOlderThanVersion = 2
      end
      item
        ExpressionName = 'Order'
        UpgradeOlderThanVersion = 2
      end
      item
        ExpressionName = 'Product'
        UpgradeOlderThanVersion = 2
      end>
    PersistenceHandle = PersistenceHandle
    SystemTypeInfoHandle = dmSystemTypeInfo.SystemTypeInfo
    Left = 48
    Top = 56
  end
end
