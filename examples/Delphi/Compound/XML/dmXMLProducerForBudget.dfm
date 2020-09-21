object dmXmlProducer: TdmXmlProducer
  OldCreateOrder = False
  Left = 129
  Top = 224
  Height = 480
  Width = 696
  object BudgetProducer: TBoldXMLProducer
    BoldManipulator = BoldManipulator1
    OnProduce = BudgetProducerProduce
    Left = 248
    Top = 136
  end
  object BoldManipulator1: TBoldManipulator
    IdStringRepresentation = isrVerbose
    BoldSystemHandle = dmMain.BoldSystemHandle1
    Mappers = <
      item
        MappingName = 'CELL'
        OnGet = BoldManipulator1Mappers0Get
        OnSet = BoldManipulator1Mappers0Set
      end>
    Left = 392
    Top = 40
  end
  object BoldXMLDispatcher1: TBoldXMLDispatcher
    ObjectName = 'BudgetDispatcher'
    ServerClass = 'XMLDispatcher'
    ServerHandle = BoldComServerHandle1
    Actions = <
      item
        ActionName = 'GetBudget'
        Producer = BudgetProducer
      end
      item
        ActionName = 'UpdateBudget'
        OnAction = MainDispatcherActions1Action
      end>
    OnGetXMLRequest = BoldXMLDispatcher1GetXMLRequest
    Left = 264
    Top = 40
  end
  object BoldComServerHandle1: TBoldComServerHandle
    Classes = <
      item
        CLSID = '{DCDA95FC-5719-4A65-B04E-89DE2DCC6DE5}'
        Description = 'XMLDispatcher Application'
        Name = 'XMLDispatcher'
      end>
    Left = 112
    Top = 136
  end
end
