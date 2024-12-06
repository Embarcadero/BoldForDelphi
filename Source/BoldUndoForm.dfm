object frmBoldUndo: TfrmBoldUndo
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Undo'
  ClientHeight = 162
  ClientWidth = 387
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 13
  object GridUndoList: TcxGrid
    Left = 0
    Top = 0
    Width = 387
    Height = 162
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 395
    ExplicitHeight = 174
    object tvUndoList: TcxGridTableView
      PopupMenu = popUndo
      Navigator.Buttons.CustomButtons = <>
      ScrollbarAnnotations.CustomAnnotations = <>
      OnCellDblClick = tvUndoListCellDblClick
      OnCustomDrawCell = tvUndoListCustomDrawCell
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.ColumnFiltering = False
      OptionsCustomize.ColumnGrouping = False
      OptionsCustomize.ColumnMoving = False
      OptionsCustomize.ColumnSorting = False
      OptionsCustomize.ColumnsQuickCustomizationShowCommands = False
      OptionsData.Editing = False
      OptionsSelection.CellSelect = False
      OptionsSelection.HideFocusRectOnExit = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      OptionsView.Indicator = True
      object colName: TcxGridColumn
        Caption = 'Name'
        Visible = False
      end
      object colCaption: TcxGridColumn
        Caption = 'Undo Block'
        PropertiesClassName = 'TcxMemoProperties'
        Width = 131
      end
      object ColObjectCount: TcxGridColumn
        Caption = 'Objects'
        DataBinding.ValueType = 'Integer'
        Options.AutoWidthSizable = False
        Width = 50
      end
      object colTime: TcxGridColumn
        Caption = 'Time'
        DataBinding.ValueType = 'DateTime'
        PropertiesClassName = 'TcxTimeEditProperties'
        MinWidth = 60
        Options.AutoWidthSizable = False
        Width = 60
      end
    end
    object LevelUndoList: TcxGridLevel
      GridView = tvUndoList
    end
  end
  object UpdateTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = UpdateTimerTimer
    Left = 40
    Top = 136
  end
  object popUndo: TPopupMenu
    Left = 40
    Top = 88
    object miUndo: TMenuItem
      Action = actUndo
    end
    object miRedo: TMenuItem
      Action = actRedo
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miSetcheckpoint: TMenuItem
      Action = actSetCheckPoint
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object actShowChanges1: TMenuItem
      Action = actShowChanges
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Clear1: TMenuItem
      Action = actClear
    end
  end
  object ActionList1: TActionList
    Left = 40
    Top = 32
    object actUndo: TBoldUndoAction
      Category = 'Bold Actions'
      Caption = 'Undo'
      Enabled = False
      ShortCut = 16474
    end
    object actRedo: TBoldRedoAction
      Category = 'Bold Actions'
      Caption = 'Redo'
      Enabled = False
      ShortCut = 24666
    end
    object actSetCheckPoint: TBoldSetCheckPointAction
      Category = 'Bold Actions'
      Caption = 'Set check point'
      Enabled = False
    end
    object actShowChanges: TAction
      Caption = 'ShowChanges'
      OnExecute = actShowChangesExecute
      OnUpdate = actShowChangesUpdate
    end
    object actClear: TBoldClearUndoAction
      Category = 'Bold Actions'
      Caption = 'Clear'
    end
  end
end
