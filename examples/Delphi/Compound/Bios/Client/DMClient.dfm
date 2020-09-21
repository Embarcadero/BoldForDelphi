object DMClientSystem: TDMClientSystem
  OldCreateOrder = False
  Left = 331
  Top = 322
  Height = 479
  Width = 741
  object BoldComConnectionHandle1: TBoldComConnectionHandle
    ServerCLSID = '{D7724C3F-9082-460F-9321-CB47C232CA29}'
    ServerName = 'Server.BoldServer'
    Threaded = True
    Left = 72
    Top = 24
  end
  object SystemHandle: TBoldSystemHandleCom
    ConnectionHandle = BoldComConnectionHandle1
    ObjectName = 'System'
    IsDefault = True
    Left = 68
    Top = 84
  end
end
