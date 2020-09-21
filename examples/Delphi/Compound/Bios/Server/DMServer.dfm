object DataModule3: TDataModule3
  OldCreateOrder = False
  Left = 434
  Top = 322
  Height = 479
  Width = 741
  object BoldComServerHandle1: TBoldComServerHandle
    Classes = <
      item
        CLSID = '{D7724C3F-9082-460F-9321-CB47C232CA29}'
        Description = 'Bold Server Application'
        Name = 'BoldServer'
      end
      item
        CLSID = '{C3733F7A-E56C-4EA3-88E1-73DA4ADE9E68}'
        Name = 'Class1'
      end>
    Left = 88
    Top = 16
  end
  object SystemHandleExporter: TBoldComServerElementHandle
    ObjectName = 'System'
    ServerClass = 'BoldServer'
    ServerHandle = BoldComServerHandle1
    BoldHandle = DMSystem.SystemHandle
    ExportMode = emHandle
    Left = 88
    Top = 80
  end
  object SystemExporter: TBoldComServerElementHandle
    ObjectName = 'BoldSystem'
    ServerClass = 'BoldServer'
    ServerHandle = BoldComServerHandle1
    BoldHandle = DMSystem.SystemHandle
    ExportMode = emValue
    Left = 72
    Top = 136
  end
end
