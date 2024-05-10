object dmLockManagerHandle: TdmLockManagerHandle
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 113
  Top = 215
  Height = 480
  Width = 696
  object BoldComConnectionHandle1: TBoldComConnectionHandle
    AutoConnect = True
    ServerCLSID = '{11C6E940-CEC6-45BA-873D-27854A82A023}'
    ServerName = 'BoldPropagator.BoldEnterprisePropagator'
    Left = 160
    Top = 48
  end
  object LockManagerHandleCom: TBoldLockManagerHandleCom
    ConnectionHandle = BoldComConnectionHandle1
    Active = True
    Left = 224
    Top = 128
  end
end
