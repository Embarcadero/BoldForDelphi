object frmBoldToCxConverter: TfrmBoldToCxConverter
  Left = 0
  Top = 0
  Caption = 'Select Components'
  ClientHeight = 598
  ClientWidth = 763
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DetectedComponentsGrid: TcxGrid
    Left = 0
    Top = 0
    Width = 763
    Height = 392
    Align = alClient
    TabOrder = 0
    object tv: TcxGridTableView
      Navigator.Buttons.CustomButtons = <>
      ScrollbarAnnotations.CustomAnnotations = <>
      OnSelectionChanged = tvSelectionChanged
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.ColumnFiltering = False
      OptionsCustomize.ColumnGrouping = False
      OptionsCustomize.ColumnHidingOnGrouping = False
      OptionsCustomize.ColumnHorzSizing = False
      OptionsCustomize.ColumnMoving = False
      OptionsCustomize.ColumnSorting = False
      OptionsSelection.CellSelect = False
      OptionsSelection.MultiSelect = True
      OptionsSelection.HideFocusRectOnExit = False
      object tvColumn1: TcxGridColumn
        Caption = 'Component'
        Width = 209
      end
      object tvColumn2: TcxGridColumn
        Caption = 'Type'
        Width = 132
      end
      object tvColumn3: TcxGridColumn
        Caption = 'Ocl Type'
        Width = 211
      end
      object tvColumn4: TcxGridColumn
        Caption = 'Convert to'
        Width = 186
      end
    end
    object DetectedComponentsGridLevel1: TcxGridLevel
      GridView = tv
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 558
    Width = 763
    Height = 40
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      763
      40)
    object cxCancelButton: TcxButton
      Left = 680
      Top = 7
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
      OnClick = cxCancelButtonClick
    end
    object cxConvert: TcxButton
      Left = 562
      Top = 7
      Width = 110
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Convert Selected'
      Enabled = False
      ModalResult = 1
      TabOrder = 1
    end
    object cxRemoveAfterConvertionCheckbox: TcxCheckBox
      Left = 9
      Top = 11
      Anchors = [akLeft, akBottom]
      Caption = 'Remove old components after convertion'
      State = cbsChecked
      TabOrder = 2
    end
  end
  object cxMemoLog: TcxMemo
    Left = 0
    Top = 392
    Align = alBottom
    Properties.ReadOnly = True
    Properties.ScrollBars = ssVertical
    TabOrder = 2
    Height = 166
    Width = 763
  end
end
