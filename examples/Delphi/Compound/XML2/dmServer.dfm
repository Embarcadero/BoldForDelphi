object ServerDataModule: TServerDataModule
  OldCreateOrder = False
  OnDestroy = DataModuleDestroy
  Left = 368
  Top = 288
  Height = 414
  Width = 507
  object XMLDispatcher: TBoldXMLDispatcher
    ObjectName = 'Dispatcher'
    ServerClass = 'OrgChart'
    ServerHandle = BoldComServerHandle1
    Actions = <
      item
        Default = True
        ActionName = 'EvaluateOCL'
        OnAction = BoldXMLDispatcher1ActionsEvaluateOCLAction
      end
      item
        ActionName = 'Update'
        OnAction = BoldXMLDispatcher1ActionsUpdateAction
      end
      item
        ActionName = 'Fetch'
        OnAction = BoldXMLDispatcher1ActionsFetchAction
      end>
    Left = 168
    Top = 56
  end
  object BoldComServerHandle1: TBoldComServerHandle
    Classes = <
      item
        CLSID = '{AE605A6F-9C63-4CA4-90FC-B4D75614D4D7}'
        Description = 'OrgChart Server Application'
        Name = 'OrgChart'
      end>
    Left = 96
    Top = 240
  end
  object BoldManipulator1: TBoldManipulator
    IdStringRepresentation = isrVerbose
    BoldSystemHandle = dmMain.OrgChartSystem
    Mappers = <>
    Left = 376
    Top = 240
  end
  object bxpFetch: TBoldXMLProducer
    BoldManipulator = BoldManipulator1
    Left = 360
    Top = 120
  end
  object ppLastPage: TPageProducer
    HTMLFile = 'LastPage.htm'
    Left = 288
    Top = 184
  end
end
