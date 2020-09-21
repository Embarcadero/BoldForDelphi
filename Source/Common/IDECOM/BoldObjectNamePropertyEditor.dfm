object ObjectNamePropEditFrm: TObjectNamePropEditFrm
  Left = 533
  Top = 204
  Width = 283
  Height = 446
  Caption = 'ObjectNamePropEditFrm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn1: TBitBtn
    Left = 8
    Top = 376
    Width = 75
    Height = 25
    TabOrder = 0
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 96
    Top = 376
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkCancel
  end
  object lvObjects: TListView
    Left = 8
    Top = 16
    Width = 257
    Height = 345
    Columns = <
      item
        AutoSize = True
        Caption = 'ObjectName'
      end
      item
        AutoSize = True
        Caption = 'ClassName'
      end>
    HideSelection = False
    TabOrder = 2
    ViewStyle = vsReport
  end
  object BitBtn3: TBitBtn
    Left = 184
    Top = 376
    Width = 75
    Height = 25
    Action = RefreshAction
    Caption = 'Refresh'
    TabOrder = 3
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000C30E0000C30E00001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
      6666664666644466666666446446644466666644466666644666664444666666
      4666664444466666446666666666666664666666666666666666664666666666
      6666664466666444446666646666664444666664466666644466666644466446
      4466666666444666646666666666666666666666666666666666}
  end
  object ActionList1: TActionList
    Left = 184
    Top = 64
    object RefreshAction: TAction
      Caption = 'Refresh'
      OnExecute = RefreshActionExecute
    end
  end
end
