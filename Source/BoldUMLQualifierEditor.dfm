object BoldUMLQualifierEditForm: TBoldUMLQualifierEditForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  BorderWidth = 2
  Caption = 'Select Qualifiers'
  ClientHeight = 350
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 312
    Width = 270
    Height = 38
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      270
      38)
    object CancelBtn: TButton
      Left = 188
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object OKBtn: TButton
      Left = 107
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 270
    Height = 312
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      BorderWidth = 4
      Caption = 'Select Qualifiers'
      object cxBoldSelectionCheckListBox1: TcxBoldSelectionCheckListBox
        Left = 0
        Top = 0
        Width = 254
        Height = 276
        Align = alClient
        BoldListHandle = blhOtherEndAttributes
        BoldRowProperties.Expression = 'name'
        DragMode = dmAutomatic
        ParentColor = False
        TabOrder = 0
        BoldSelectionHandle = blhQualifiers
      end
    end
  end
  object blhOtherEndAttributes: TBoldListHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = brhAssociationEnd
    Expression = 
      'type.allFeature->filterOnType(UMLAttribute)->select(taggedValue[' +
      #39'Bold.AttributeKind'#39'].value='#39'Bold'#39')'#13#10
    Left = 52
    Top = 144
  end
  object brhAssociationEnd: TBoldReferenceHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    StaticValueTypeName = 'UMLAssociationEnd'
    Left = 52
    Top = 47
  end
  object blhQualifiers: TBoldListHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = brhAssociationEnd
    Expression = 'qualifier'
    Left = 52
    Top = 96
  end
end
