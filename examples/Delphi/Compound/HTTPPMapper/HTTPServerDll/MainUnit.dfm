object WebModule1: TWebModule1
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  Actions = <
    item
      Default = True
      Name = 'PMapper'
      PathInfo = '/BuildingAndOwners'
      OnAction = WebModule1PMapperAction
    end>
  Left = 285
  Top = 161
  Height = 479
  Width = 741
  object httpPMapper: TBoldHTTPServerPersistenceHandlePassthrough
    PersistenceHandle = dmPersistence.BoldPersistenceHandleDB1
    BoldModel = dmCore.BoldModel1
    Left = 128
    Top = 24
  end
end
