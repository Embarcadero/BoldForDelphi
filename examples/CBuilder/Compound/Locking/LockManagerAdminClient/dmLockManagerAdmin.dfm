object dmMain: TdmMain
  OldCreateOrder = False
  Left = 382
  Top = 357
  Height = 220
  Width = 444
  object BoldComConnectionHandle1: TBoldComConnectionHandle
    ServerCLSID = '{11C6E940-CEC6-45BA-873D-27854A82A023}'
    ServerName = 'BoldPropagator.EnterprisePropagator.PropagatorConnection'
    Left = 136
    Top = 64
  end
  object BoldLockManagerAdminHandleCom1: TBoldLockManagerAdminHandleCom
    ConnectionHandle = BoldComConnectionHandle1
    Active = False
    Left = 320
    Top = 64
  end
end
