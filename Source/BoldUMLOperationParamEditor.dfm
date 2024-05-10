object BoldUMLOperationParamEditForm: TBoldUMLOperationParamEditForm
  Left = 0
  Top = 0
  ActiveControl = tbxOperationName
  BorderStyle = bsDialog
  Caption = 'Operation parameter'
  ClientHeight = 198
  ClientWidth = 331
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  DesignSize = (
    331
    198)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 305
    Height = 145
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Operation Parameter'
      DesignSize = (
        297
        117)
      object lblOperationName: TLabel
        Left = 19
        Top = 14
        Width = 27
        Height = 13
        Alignment = taRightJustify
        Caption = '&Name'
      end
      object lblOperationOwnerScope: TLabel
        Left = 22
        Top = 88
        Width = 24
        Height = 13
        Alignment = taRightJustify
        Caption = 'Type'
      end
      object lblOperationVisibility: TLabel
        Left = 26
        Top = 61
        Width = 20
        Height = 13
        Alignment = taRightJustify
        Caption = 'Kind'
      end
      object tbxOperationName: TcxBoldTextEdit
        Left = 54
        Top = 10
        HelpContext = 83
        DataBinding.BoldHandle = brhParam
        DataBinding.BoldProperties.Expression = 'name'
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldRowProperties.Expression = ''
        Properties.Alignment.Horz = taLeftJustify
        Anchors = [akLeft, akTop, akRight]
        ParentFont = False
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 0
        Width = 220
      end
      object cmbTypeName: TcxBoldComboBox
        Left = 54
        Top = 84
        HelpContext = 80
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = brhParam
        DataBinding.BoldProperties.Expression = 'typeName'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllDataTypes
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownRows = 12
        Properties.DropDownSizeable = True
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 3
        Width = 220
      end
      object cmbOperationKind: TcxBoldComboBox
        Left = 54
        Top = 57
        HelpContext = 1250
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = brhParam
        DataBinding.BoldProperties.Expression = 'kind'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldLookupListHandle = blhParameterDirectionKind
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 2
        Width = 220
      end
      object cbIsConst: TBoldCheckBox
        Left = 54
        Top = 35
        Width = 97
        Height = 17
        BoldHandle = brhParam
        BoldProperties.Expression = 'taggedValue['#39'Bold.IsConst'#39'].value'
        BoldProperties.Renderer = risConst
        Caption = 'Is Const'
        ReadOnly = False
        TabOrder = 1
      end
    end
  end
  object OKBtn: TButton
    Left = 165
    Top = 167
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 246
    Top = 167
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object brhParam: TBoldReferenceHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    StaticValueTypeName = 'UMLParameter'
    Left = 252
    Top = 40
  end
  object blhParameterDirectionKind: TBoldListHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = brhParam
    Expression = 'ParameterDirectionKind.allInstances'
    Left = 252
    Top = 80
  end
  object risConst: TBoldAsCheckBoxStateRenderer
    OnGetAsCheckBoxState = risConstGetAsCheckBoxState
    OnSetAsCheckBoxState = risConstSetAsCheckBoxState
    Left = 40
    Top = 152
  end
end
