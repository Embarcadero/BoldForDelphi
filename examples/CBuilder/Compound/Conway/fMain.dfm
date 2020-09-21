object frmMain: TfrmMain
  Left = 324
  Top = 200
  Width = 520
  Height = 338
  Caption = 'Conway'#39's Game of Life'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    512
    311)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 156
    Width = 63
    Height = 13
    Caption = 'Timer interval'
  end
  object BoldLabel4: TBoldLabel
    Left = 76
    Top = 156
    Width = 45
    Height = 13
    BoldHandle = refGame
    BoldProperties.Expression = 'timerTime'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object BoldLabel3: TBoldLabel
    Left = 76
    Top = 176
    Width = 55
    Height = 13
    BoldHandle = refGame
    BoldProperties.Expression = 'generations'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object BoldLabel1: TBoldLabel
    Left = 76
    Top = 192
    Width = 115
    Height = 13
    BoldHandle = refGame
    BoldProperties.Expression = 'cell->select(active)->size'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object Label2: TLabel
    Left = 0
    Top = 192
    Width = 22
    Height = 13
    Caption = 'Cells'
  end
  object BoldLabel2: TBoldLabel
    Left = 80
    Top = 256
    Width = 38
    Height = 13
    BoldHandle = refGame
    BoldProperties.Expression = 'fontSize'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
  end
  object Label3: TLabel
    Left = 0
    Top = 256
    Width = 42
    Height = 13
    Caption = 'Font size'
  end
  object btnClear: TButton
    Left = 0
    Top = 8
    Width = 93
    Height = 25
    Caption = 'Clear the board'
    TabOrder = 0
    OnClick = btnClearClick
  end
  object btnTick: TButton
    Left = 0
    Top = 36
    Width = 93
    Height = 25
    Caption = 'One generation'
    TabOrder = 1
    OnClick = btnTickClick
  end
  object btnStart: TButton
    Left = 0
    Top = 64
    Width = 93
    Height = 25
    Caption = 'Start ticking'
    TabOrder = 2
    OnClick = btnStartClick
  end
  object BoldTrackBar1: TBoldTrackBar
    Left = 4
    Top = 96
    Width = 149
    Height = 45
    Max = 1000
    Orientation = trHorizontal
    Frequency = 100
    SelEnd = 0
    SelStart = 0
    TabOrder = 3
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    BoldHandle = refGame
    BoldProperties.Expression = 'timerTime'
    BoldProperties.ApplyPolicy = bapChange
    ReadOnly = False
  end
  object bcbCollecting: TBoldCheckBox
    Left = 80
    Top = 132
    Width = 77
    Height = 17
    BoldHandle = refGame
    BoldProperties.Expression = 'collecting'
    Caption = 'Collecting'
    ReadOnly = True
    TabOrder = 4
  end
  object btbFontSize: TBoldTrackBar
    Left = 4
    Top = 208
    Width = 157
    Height = 45
    Max = 24
    Min = 3
    Orientation = trHorizontal
    Frequency = 1
    SelEnd = 0
    SelStart = 0
    TabOrder = 5
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    BoldHandle = refGame
    BoldProperties.Expression = 'fontSize'
    BoldProperties.ApplyPolicy = bapChange
    ReadOnly = False
  end
  object BoldMemo1: TBoldMemo
    Left = 168
    Top = 4
    Width = 338
    Height = 302
    Alignment = taLeftJustify
    Anchors = [akLeft, akTop, akRight, akBottom]
    BoldHandle = refGame
    BoldProperties.Expression = 'board'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = 0
    Font.Name = 'Courier New'
    Font.Style = []
    MaxLength = 0
    ParentFont = False
    ReadOnly = False
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object refGame: TBoldReferenceHandle
    StaticValueTypeName = 'Game'
    Left = 20
    Top = 16
  end
  object bpcTimerInterval: TBoldPropertiesController
    BoldHandle = refGame
    BoldProperties.Expression = 'timerTime'
    BoldProperties.ApplyPolicy = bapChange
    DrivenProperties = <
      item
        VCLComponent = Timer1
        PropertyName = 'Interval'
      end>
    Left = 124
    Top = 96
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 0
    OnTimer = btnTickClick
    Top = 120
  end
  object bpcFontSize: TBoldPropertiesController
    BoldHandle = refGame
    BoldProperties.Expression = 'fontSize'
    BoldProperties.ApplyPolicy = bapChange
    DrivenProperties = <
      item
        VCLComponent = BoldMemo1
        PropertyName = 'Font.Size'
      end>
    Left = 100
    Top = 208
  end
  object BoldSystemHandle1: TBoldSystemHandle
    IsDefault = True
    AutoActivate = True
    SystemTypeInfoHandle = BoldSystemTypeInfoHandle1
    Active = False
    Left = 4
    Top = 276
  end
  object BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
    BoldModel = BoldModel1
    Left = 48
    Top = 276
  end
  object BoldModel1: TBoldModel
    UMLModelMode = ummNone
    Boldify.EnforceDefaultUMLCase = False
    Boldify.DefaultNavigableMultiplicity = '0..1'
    Boldify.DefaultNonNavigableMultiplicity = '0..*'
    Left = 108
    Top = 276
    Model = (
      'VERSION 19'
      '(Model'
      #9'"ConwayClasses"'
      #9'"BusinessClassesRoot"'
      #9'""'
      #9'""'
      
        #9'"_Boldify.boldified=True,_Boldify.RootClass=BusinessClassesRoot' +
        ',_BoldInternal.flattened=True,Bold.DelphiName=<Name>,\"Bold.Impl' +
        'ementationUses=BoldReferenceHandle, \c\lBoldCheckBox, \c\lDialog' +
        's,\c\lForms,\c\lBoldQueue\",Bold.InterfaceUses=Windows,Bold.Root' +
        'Class=BusinessClassesRoot,Bold.UnitName=ConwayClasses"'
      #9'(Classes'
      #9#9'(Class'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'"<NONE>"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=Persistent,Bold.TableName=<Prefix>_OBJECT,Bold.V' +
        'ersioned=<Default>"'
      #9#9#9'(Attributes'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Game"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent"'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"TimerTime"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'"1000"'
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Generations"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'"0"'
      #9#9#9#9#9'"Derived=False,persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"board"'
      #9#9#9#9#9'"Blob"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"Derived=True,persistence=Transient,Bold.ReverseDerive=True' +
        '"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"xMax"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'"0"'
      #9#9#9#9#9'"persistence=Transient"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"xMin"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'"0"'
      #9#9#9#9#9'"persistence=Transient"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"yMax"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'"0"'
      #9#9#9#9#9'"persistence=Transient"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"yMin"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'"0"'
      #9#9#9#9#9'"persistence=Transient"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"InactiveCount"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"persistence=Persistent,Bold.AttributeKind=Delphi,Bold.Delp' +
        'hiField=True,Bold.DelphiPropertyRead=Field,Bold.DelphiPropertyWr' +
        'ite=Field"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"xSize"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"Derived=True,persistence=Transient,\"Bold.DerivationOCL=xM' +
        'ax - xMin + 1\""'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"FontSize"'
      #9#9#9#9#9'"integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'"8"'
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"collecting"'
      #9#9#9#9#9'"Boolean"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"Tick"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"ClearCells"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"UpdateBounds"'
      #9#9#9#9#9'"x: Integer; y: Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"GarbageCollect"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"RefreshBounds"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"GetBounds"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"TRect"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"ResetBounds"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Class'
      #9#9#9'"Cell"'
      #9#9#9'"BusinessClassesRoot"'
      #9#9#9'TRUE'
      #9#9#9'FALSE'
      #9#9#9'""'
      #9#9#9'""'
      
        #9#9#9'"persistence=Persistent,\"Bold.DefaultStringRepresentation=if' +
        ' active then '#39'*'#39' else '#39' '#39' endif\""'
      #9#9#9'(Attributes'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Active"'
      #9#9#9#9#9'"boolean"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent,Bold.ColumnName=Cell<Name>"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"Intermediate"'
      #9#9#9#9#9'"boolean"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"Derived=False,persistence=Transient,Bold.AttributeKind=Del' +
        'phi,Bold.DelphiField=True,Bold.DelphiPropertyRead=Field,Bold.Del' +
        'phiPropertyWrite=Field"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"neighbours"'
      #9#9#9#9#9'"integer"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'1'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"Derived=True,persistence=Transient,\"Bold.DerivationOCL=cD' +
        'own.activeCount +\ccUp.activeCount +\ccLeft.activeCount +\ccRigh' +
        't.activeCount +\ccDownRight.activeCount +\ccDownLeft.activeCount' +
        ' +\ccUpRight.activeCount +\ccUpLeft.activeCount\""'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"x"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"y"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"persistence=Persistent"'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"ActiveCount"'
      #9#9#9#9#9'"Integer"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"Derived=True,persistence=Transient,\"Bold.DerivationOCL=if' +
        ' active then 1 else 0 endif\""'
      #9#9#9#9')'
      #9#9#9#9'(Attribute'
      #9#9#9#9#9'"NeighboursEnsured"'
      #9#9#9#9#9'"Boolean"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'1'
      #9#9#9#9#9'""'
      
        #9#9#9#9#9'"persistence=Transient,Bold.AttributeKind=Delphi,Bold.Delph' +
        'iField=True,Bold.DelphiPropertyRead=Field,Bold.DelphiPropertyWri' +
        'te=Field"'
      #9#9#9#9')'
      #9#9#9')'
      #9#9#9'(Methods'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"CalculateIntermediate"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"UpdateActive"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"SetupCell"'
      #9#9#9#9#9'"x: Integer; y: Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"UnensureNeighbours"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"EnsureCell"'
      #9#9#9#9#9'"aCell: TCell; x: Integer; y: Integer"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"TCell"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"AllowRemove"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"boolean"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"PrepareDelete"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"Bold.OperationKind=Override"'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"NeighboursNotEnsured"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"UnensureCell"'
      #9#9#9#9#9'"aCell: TCell"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9#9'(Method'
      #9#9#9#9#9'"EnsureNeighbours"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9#9'2'
      #9#9#9#9#9'""'
      #9#9#9#9#9'""'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9')'
      #9'(Associations'
      #9#9'(Association'
      #9#9#9'"CellGame"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Bold.DelphiName=<Name>"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Cell"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Game"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..*"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"Game"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Cell"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'""'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"Horizontal"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Transient,Derived=True,Bold.DelphiName=<Name>"'
      #9#9#9'TRUE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"cLeft"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Cell"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"\"Bold.DerivationOCL=game.coord[x-1, y]\""'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"cRight"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Cell"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      
        #9#9#9#9#9'"\"Bold.DerivationOCL=game.coord[x+1, y]\",Bold.Embed=False' +
        '"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"Vertical"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Transient,Derived=True,Bold.DelphiName=<Name>"'
      #9#9#9'TRUE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"cDown"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Cell"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"\"Bold.DerivationOCL=game.coord[x, y+1]\""'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"cUp"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Cell"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      
        #9#9#9#9#9'"\"Bold.DerivationOCL=game.coord[x, y-1]\",Bold.Embed=False' +
        '"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"Diagonal"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Transient,Derived=True,Bold.DelphiName=<Name>"'
      #9#9#9'TRUE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"cDownLeft"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Cell"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"Bold.DerivationOCL=cDown.cLeft"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"cUpRight"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Cell"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"Bold.DerivationOCL=cUp.cRight,Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"Diagonal2"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Transient,Derived=True,Bold.DelphiName=<Name>"'
      #9#9#9'TRUE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"cDownRight"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Cell"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"Bold.DerivationOCL=cDown.cRight"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"cUpLeft"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Cell"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"Bold.DerivationOCL=cUp.cLeft,Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9#9'(Association'
      #9#9#9'"Coords"'
      #9#9#9'"<NONE>"'
      #9#9#9'""'
      #9#9#9'""'
      #9#9#9'"persistence=Persistent,Derived=False,Bold.DelphiName=<Name>"'
      #9#9#9'FALSE'
      #9#9#9'(Roles'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"coord_back"'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Cell"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'""'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9#9'(Role'
      #9#9#9#9#9'"coord"'
      #9#9#9#9#9'TRUE'
      #9#9#9#9#9'FALSE'
      #9#9#9#9#9'"Game"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'"0..1"'
      #9#9#9#9#9'""'
      #9#9#9#9#9'0'
      #9#9#9#9#9'2'
      #9#9#9#9#9'0'
      #9#9#9#9#9'"Bold.Embed=False"'
      #9#9#9#9#9'(Qualifiers'
      #9#9#9#9#9#9'(Qualifier'
      #9#9#9#9#9#9#9'"x"'
      #9#9#9#9#9#9#9'"Integer"'
      
        #9#9#9#9#9#9#9'"persistence=Persistent,Bold.MemberInfoClassName=<Default' +
        '>,Bold.OptimisticLocking=<Default>"'
      #9#9#9#9#9#9')'
      #9#9#9#9#9#9'(Qualifier'
      #9#9#9#9#9#9#9'"y"'
      #9#9#9#9#9#9#9'"Integer"'
      
        #9#9#9#9#9#9#9'"persistence=Persistent,Bold.MemberInfoClassName=<Default' +
        '>,Bold.OptimisticLocking=<Default>"'
      #9#9#9#9#9#9')'
      #9#9#9#9#9')'
      #9#9#9#9')'
      #9#9#9')'
      #9#9')'
      #9')'
      ')')
  end
end
