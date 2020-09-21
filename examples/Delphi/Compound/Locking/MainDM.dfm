object dmMain: TdmMain
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 113
  Top = 166
  Height = 479
  Width = 741
  object bshLocking: TBoldSystemHandle
    IsDefault = True
    SystemTypeInfoHandle = bstihLocking
    Active = False
    PersistenceHandle = BoldIdAdderHandle1
    OnPreUpdate = bshLockingPreUpdate
    Left = 72
    Top = 80
  end
  object bstihLocking: TBoldSystemTypeInfoHandle
    BoldModel = dmModel.BoldModel1
    UseGeneratedCode = False
    Left = 72
    Top = 24
  end
  object BoldLockingHandle1: TBoldLockingHandle
    SystemHandle = bshLocking
    Listener = BoldListenerHandle1
    LockManager = BoldLockManagerHandleCom1
    Left = 232
    Top = 24
  end
  object BoldLockManagerHandleCom1: TBoldLockManagerHandleCom
    ConnectionHandle = bcchPropagatorServer
    Active = False
    Left = 232
    Top = 80
  end
  object bcchPropagatorServer: TBoldComConnectionHandle
    ServerCLSID = '{11C6E940-CEC6-45BA-873D-27854A82A023}'
    ServerName = 'BoldPropagator.BoldEnterprisePropagator'
    Left = 240
    Top = 136
  end
  object BoldListenerHandle1: TBoldListenerHandle
    AutoStart = False
    LeaseDuration = 300000
    ExtendLeaseAfter = 80
    PollingInterval = 5000
    AutoExtendLease = True
    Dequeuer = BoldExternalObjectSpaceEventHandler1
    ClientIdentifierString = 'Demo Client'
    PropagatorHandle = BoldPropagatorHandleCOM1
    Left = 264
    Top = 248
  end
  object BoldExternalObjectSpaceEventHandler1: TBoldExternalObjectSpaceEventHandler
    BoldSystemHandle = bshLocking
    Left = 256
    Top = 296
  end
  object BoldIdAdderHandle1: TBoldIdAdderHandle
    NextPersistenceHandle = BoldSOAPClientPersistenceHandle1
    BoldListener = BoldListenerHandle1
    Left = 72
    Top = 144
  end
  object BoldSOAPClientPersistenceHandle1: TBoldSOAPClientPersistenceHandle
    ConnectionHandle = bcchPersistence
    ObjectName = 'SOAPPersistenceHandle'
    BoldModel = dmModel.BoldModel1
    Left = 88
    Top = 200
  end
  object bcchPersistence: TBoldComConnectionHandle
    ServerCLSID = '{8BC8A32E-260A-41EE-A05A-9DB9F62F4B2E}'
    ServerHost = 'locahost'
    ServerName = 'LockingPServer.LockingDemoPSrv'
    Left = 80
    Top = 256
  end
  object BoldPropagatorHandleCOM1: TBoldPropagatorHandleCOM
    Active = False
    ConnectionHandle = bcchPropagatorServer
    Left = 336
    Top = 184
  end
end
