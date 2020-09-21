object dmSystem: TdmSystem
  OldCreateOrder = False
  Left = 321
  Top = 369
  Height = 285
  Width = 489
  object SystemHandle: TBoldSystemHandle
    IsDefault = True
    SystemTypeInfoHandle = dmSystemTypeInfo.SystemTypeInfo
    Active = False
    PersistenceHandle = dmPersistence.PersistenceHandle
    Left = 48
    Top = 8
  end
  object ActionList1: TActionList
    Left = 48
    Top = 64
    object BoldUpdateDBAction1: TBoldUpdateDBAction
      Category = 'Bold Actions'
      Caption = 'Update DB'
      BoldSystemHandle = SystemHandle
    end
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = SystemHandle
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before exit?'
      SaveOnClose = saAsk
    end
    object BoldIBAliasAction1: TBoldIBAliasAction
      Category = 'Bold Actions'
      Caption = 'Create DB'
      BoldSystemHandle = SystemHandle
      Username = 'SYSDBA'
      Password = 'masterkey'
    end
  end
end
