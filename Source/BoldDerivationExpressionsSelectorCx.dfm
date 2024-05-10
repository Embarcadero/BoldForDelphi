object frmBoldDerivationExpressionsSelectorCx: TfrmBoldDerivationExpressionsSelectorCx
  Left = 0
  Top = 0
  ActiveControl = cxGrid1
  BorderStyle = bsDialog
  Caption = 'Select member for derivation expression override'
  ClientHeight = 352
  ClientWidth = 638
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 314
    Width = 638
    Height = 38
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 304
    ExplicitWidth = 701
    DesignSize = (
      638
      38)
    object CancelBtn: TButton
      Left = 556
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
      ExplicitLeft = 619
    end
    object OKBtn: TButton
      Left = 475
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
      ExplicitLeft = 538
    end
  end
  object cxGrid1: TcxGrid
    Left = 0
    Top = 0
    Width = 638
    Height = 314
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 96
    ExplicitTop = 176
    ExplicitWidth = 250
    ExplicitHeight = 200
    object cxGrid1BoldTableView1: TcxGridBoldTableView
      OnDblClick = cxGrid1BoldTableView1DblClick
      Navigator.Buttons.CustomButtons = <>
      FindPanel.DisplayMode = fpdmAlways
      FindPanel.Layout = fplCompact
      ScrollbarAnnotations.CustomAnnotations = <>
      DataController.BoldHandle = bchMembers
      DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsSelection.CellSelect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      object cxGrid1BoldTableView1Column1: TcxGridBoldColumn
        Caption = 'Class'
        DataBinding.BoldProperties.Expression = 'owner'
      end
      object cxGrid1BoldTableView1Column2: TcxGridBoldColumn
        Caption = 'Member'
        DataBinding.BoldProperties.Expression = 'name'
        PropertiesClassName = 'TcxTextEditProperties'
      end
      object cxGrid1BoldTableView1Column3: TcxGridBoldColumn
        Caption = 'Expression'
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.DerivationOCL'#39'].value'
        PropertiesClassName = 'TcxTextEditProperties'
      end
    end
    object cxGrid1Level1: TcxGridLevel
      GridView = cxGrid1BoldTableView1
    end
  end
  object bchMembers: TBoldCursorHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = brhMembers
    Left = 48
    Top = 104
  end
  object brhMembers: TBoldReferenceHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    StaticValueTypeName = 'Collection(UMLFeature)'
    Left = 48
    Top = 40
  end
end
