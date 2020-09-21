object dmMain: TdmMain
  OldCreateOrder = False
  Left = 180
  Top = 136
  Height = 480
  Width = 696
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
