object frmDBGen: TfrmDBGen
  Left = 285
  Top = 159
  Width = 313
  Height = 71
  Caption = 'Database Schema Gen for Locking Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BoldIBAliasCreator1: TBoldIBAliasCreator
    Left = 120
    Top = 8
    Width = 75
    Height = 25
    Caption = 'BoldIBAliasCreator1'
    TabOrder = 0
    AliasName = 'LockingDemo'
    ServerName = 'LockingDemo.GDB'
    BoldSystemHandle = BoldSystemHandle1
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = False
    Active = False
    PersistenceHandle = BoldPersistenceHandleBDE1
    Left = 8
    Top = 8
  end
  object BoldPersistenceHandleBDE1: TBoldPersistenceHandleBDE
    Username = 'sysdba'
    Password = 'masterkey'
    BoldModel = dmModel.BoldModel1
    SystemTablesPrefix = 'BOLD'
    ClockLogGranularity = '0:0:0.0'
    SQLDataBaseConfig.ColumnTypeForDate = 'DATE'
    SQLDataBaseConfig.ColumnTypeForTime = 'DATE'
    SQLDataBaseConfig.ColumnTypeForDateTime = 'DATE'
    SQLDataBaseConfig.ColumnTypeForBlob = 'BLOB'
    SQLDataBaseConfig.UseSQL92Joins = False
    DatabaseEngine = dbeUnknown
    DatabaseName = 'LockingDemo'
    Left = 40
    Top = 8
  end
end
