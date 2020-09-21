object DataModule1: TDataModule1
  OldCreateOrder = True
  Left = 285
  Top = 161
  Height = 479
  Width = 741
  object FullNameRenderer: TBoldAsStringRenderer
    OnSubscribe = FullNameRenderStubscribe
    OnGetAsString = FullNameRendererGetAsString
    Left = 36
    Top = 112
  end
  object IsRichRenderer: TBoldAsCheckBoxStateRenderer
    OnSubscribe = IsRichRendererSubscribe
    OnGetAsCheckBoxState = IsRichRendererGetAsCheckBoxState
    Left = 36
    Top = 12
  end
  object IsRichFilter: TBoldFilter
    OnSubscribe = IsRichFilterSubscribe
    OnFilter = IsRichFilterFilter
    Left = 36
    Top = 60
  end
  object NameComparer: TBoldComparer
    OnSubscribe = NameComparerSubscribe
    OnCompare = NameComparerCompare
    Left = 36
    Top = 224
  end
  object NegativeRedRenderer: TBoldAsStringRenderer
    OnHoldsChangedValue = NegativeRedRendererHoldsChangedValue
    OnSetFont = NegativeRedRendererSetFont
    Left = 44
    Top = 168
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = True
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    PersistenceHandle = BoldHTTPClientPersistenceHandle1
    Left = 160
    Top = 116
  end
  object BoldUMLRoseLink1: TBoldUMLRoseLink
    FileName = 'C:\vss\dev\BfD\examples\Compound\Building\Building.mdl'
    BoldModel = dmCore.BoldModel1
    Left = 164
    Top = 232
  end
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = dmCore.BoldModel1
    Left = 156
    Top = 68
  end
  object BoldHTTPClientPersistenceHandle1: TBoldHTTPClientPersistenceHandle
    BoldModel = dmCore.BoldModel1
    WebConnection = BoldWebConnection1
    Left = 156
    Top = 16
  end
  object BoldWebConnection1: TBoldWebConnection
    URL = 'http://localhost/Bold/BoldHTTPServer.dll/BuildingsAndOwners'
    Agent = 'Bold Application'
    Left = 160
    Top = 172
  end
end
