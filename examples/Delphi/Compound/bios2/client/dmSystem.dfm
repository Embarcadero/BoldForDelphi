object DM: TDM
  OldCreateOrder = False
  Left = 208
  Top = 215
  Height = 479
  Width = 741
  object BoldSystemHandleCom1: TBoldSystemHandleCom
    ConnectionHandle = BoldComConnectionHandle1
    ObjectName = 'System'
    IsDefault = True
    Left = 304
    Top = 72
  end
  object BoldComConnectionHandle1: TBoldComConnectionHandle
    AutoConnect = True
    ServerCLSID = '{5BC282A9-970A-4DE1-8644-2EB4465000FC}'
    Left = 120
    Top = 72
  end
  object ClientObjectHandle: TBoldComClientObjectHandle
    ConnectionHandle = BoldComConnectionHandle1
    ObjectName = 'AccountValidator'
    Left = 120
    Top = 208
  end
end
