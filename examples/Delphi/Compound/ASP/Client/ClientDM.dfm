object dmClient: TdmClient
  OldCreateOrder = False
  Left = 283
  Top = 116
  Height = 420
  Width = 572
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = dmModel.bmASPDemo
    UseGeneratedCode = False
    Left = 208
    Top = 56
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = False
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldHTTPClientPersistenceHandle1
    Left = 80
    Top = 56
  end
  object BoldHTTPClientPersistenceHandle1: TBoldHTTPClientPersistenceHandle
    BoldModel = dmModel.bmASPDemo
    WebConnection = BoldWebConnection1
    Left = 80
    Top = 104
  end
  object BoldWebConnection1: TBoldWebConnection
    URL = 'http://n87zb/ASPdemo/ASPserver.dll/persistence'
    Agent = 'Bold Application'
    Left = 80
    Top = 152
  end
  object BoldWebConnection2: TBoldWebConnection
    URL = 'http://n87zb/ASPdemo/ASPserver.dll/soapcalls'
    Agent = 'Bold Application'
    Left = 224
    Top = 152
  end
end
