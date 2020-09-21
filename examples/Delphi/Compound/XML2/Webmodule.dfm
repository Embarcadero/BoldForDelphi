object WebModule1: TWebModule1
  OldCreateOrder = False
  Actions = <
    item
      Default = True
      Name = 'OrgChart'
      PathInfo = '/home'
      OnAction = WebModule1OrgChartAction
    end>
  Left = 523
  Top = 294
  Height = 241
  Width = 320
  object BoldComConnectionHandle1: TBoldComConnectionHandle
    AutoConnect = True
    ServerCLSID = '{AE605A6F-9C63-4CA4-90FC-B4D75614D4D7}'
    ServerHost = 'localhost'
    ServerName = 'OrgChart.OrgChart'
    Left = 128
    Top = 16
  end
  object bcohDispatcher: TBoldComClientObjectHandle
    ConnectionHandle = BoldComConnectionHandle1
    ObjectName = 'Dispatcher'
    Left = 112
    Top = 112
  end
  object MainPage: TPageProducer
    HTMLFile = 'OrgChart.htm'
    ScriptEngine = 'JScript'
    Left = 232
    Top = 112
  end
end
