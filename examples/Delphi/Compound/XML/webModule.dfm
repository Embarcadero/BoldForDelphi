object WebModule1: TWebModule1
  OldCreateOrder = False
  Actions = <
    item
      Default = True
      Name = 'Budget'
      PathInfo = '/home'
      OnAction = WebModule1BudgetXMLAction
    end>
  Left = 145
  Top = 156
  Height = 480
  Width = 783
  object bohDispatcher: TBoldComClientObjectHandle
    ConnectionHandle = BoldComConnectionHandle1
    ObjectName = 'BudgetDispatcher'
    Left = 144
    Top = 144
  end
  object BoldComConnectionHandle1: TBoldComConnectionHandle
    ServerCLSID = '{DCDA95FC-5719-4A65-B04E-89DE2DCC6DE5}'
    ServerEvents = False
    ServerName = 'XMLDispatcher.XMLDispatcher'
    Left = 272
    Top = 80
  end
end
