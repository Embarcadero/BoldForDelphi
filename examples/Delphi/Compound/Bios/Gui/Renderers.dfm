object DataModule2: TDataModule2
  OldCreateOrder = False
  Left = 234
  Top = 527
  Height = 640
  Width = 870
  object NegativeRedRenderer: TBoldAsStringRenderer
    OnHoldsChangedValue = NegativeRedRendererHoldsChangedValue
    OnSetFont = NegativeRedRendererSetFont
    Left = 232
    Top = 16
  end
  object FullNameRenderer: TBoldAsStringRenderer
    OnSubscribe = FullNameRendererSubscribe
    OnGetAsString = FullNameRendererGetAsString
    Left = 112
    Top = 16
  end
  object IsRichRenderer: TBoldAsCheckBoxStateRenderer
    OnSubscribe = IsRichRendererSubscribe
    OnGetAsCheckBoxState = IsRichRendererGetAsCheckBoxState
    Left = 24
    Top = 16
  end
  object bsrAddress: TBoldAsStringRenderer
    OnSetFont = bsrAddressSetFont
    OnSetColor = bsrAddressSetColor
    Left = 28
    Top = 84
  end
  object bsrResidentsTotalAssets: TBoldAsStringRenderer
    OnSubscribe = bsrResidentsTotalAssetsSubscribe
    OnGetAsString = bsrResidentsTotalAssetsGetAsString
    Left = 140
    Top = 92
  end
  object bsrRentPerResident: TBoldAsStringRenderer
    OnMayModify = bsrRentPerResidentMayModify
    OnHoldsChangedValue = bsrRentPerResidentHoldsChangedValue
    OnReleaseChangedValue = bsrRentPerResidentReleaseChangedValue
    OnSubscribe = bsrRentPerResidentSubscribe
    OnGetAsString = bsrRentPerResidentGetAsString
    OnSetAsString = bsrRentPerResidentSetAsString
    OnValidateCharacter = bsrRentPerResidentValidateCharacter
    OnValidateString = bsrRentPerResidentValidateString
    Left = 260
    Top = 84
  end
end
