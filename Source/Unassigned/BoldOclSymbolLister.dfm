object Form1: TForm1
  Left = 192
  Top = 107
  Width = 448
  Height = 734
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object sgSymbols: TStringGrid
    Left = 0
    Top = 0
    Width = 440
    Height = 707
    Align = alClient
    DefaultColWidth = 80
    DefaultRowHeight = 17
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
    TabOrder = 0
  end
  object BoldModel1: TBoldModel
    UMLModelMode = ummNone
    Boldify.EnforceDefaultUMLCase = False
    Boldify.DefaultNavigableMultiplicity = '0..1'
    Boldify.DefaultNonNavigableMultiplicity = '0..*'
    Left = 16
    Top = 8
    Model = (
      'VERSION 19'
      '(Model'
      #9'"New_Model"'
      #9'"New_ModelRoot"'
      #9'""'
      #9'""'
      #9'"Bold.DelphiName=<Name>"'
      #9'(Classes'
      #9#9'(Class'
      #9#9#9'"New_ModelRoot"'
      #9#9#9'"<NONE>"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9')'
      #9'(Associations'
      #9')'
      ')')
  end
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = BoldModel1
    Left = 80
    Top = 8
  end
end
