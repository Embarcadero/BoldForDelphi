object dmPropConnection: TdmPropConnection
  OldCreateOrder = False
  Height = 428
  Width = 529
  object BoldComConnectionHandle1: TBoldComConnectionHandle
    ServerCLSID = '{11C6E940-CEC6-45BA-873D-27854A82A023}'
    ServerName = 'BoldPropagator.EnterprisePropagator.PropagatorConnection'
    Left = 136
    Top = 104
  end
  object BoldPropagatorHandleCOM1: TBoldPropagatorHandleCOM
    Active = True
    ConnectionHandle = BoldComConnectionHandle1
    Left = 312
    Top = 104
  end
end
