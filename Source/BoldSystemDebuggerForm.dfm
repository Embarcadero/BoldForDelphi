object BoldSystemDebuggerFrm: TBoldSystemDebuggerFrm
  Left = 455
  Top = 188
  Caption = 'System Debugger'
  ClientHeight = 372
  ClientWidth = 628
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 528
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Splitter2: TSplitter
    Left = 286
    Top = 0
    Width = 4
    Height = 372
    Align = alRight
    AutoSnap = False
  end
  object Splitter1: TSplitter
    Left = 142
    Top = 0
    Width = 4
    Height = 372
    Align = alRight
    AutoSnap = False
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 142
    Height = 372
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      142
      372)
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 50
      Height = 13
      Caption = 'All Classes'
    end
    object blbClasses: TBoldListBox
      Left = 0
      Top = 20
      Width = 138
      Height = 352
      Alignment = taLeftJustify
      Anchors = [akLeft, akTop, akRight, akBottom]
      BoldHandle = blhClasses
      BoldProperties.InternalDrag = False
      BoldRowProperties.Expression = ''
      DragMode = dmAutomatic
      TabOrder = 0
    end
  end
  object Panel4: TPanel
    Left = 146
    Top = 0
    Width = 140
    Height = 372
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      140
      372)
    object Label3: TLabel
      Left = 4
      Top = 4
      Width = 60
      Height = 13
      Caption = 'Dirty Objects'
    end
    object blbDirtyObjects: TBoldListBox
      Left = 0
      Top = 20
      Width = 140
      Height = 312
      Alignment = taLeftJustify
      Anchors = [akLeft, akTop, akRight, akBottom]
      BoldHandle = bchDirtyObjects
      BoldProperties.InternalDrag = False
      BoldRowProperties.Expression = ''
      DragMode = dmAutomatic
      TabOrder = 0
    end
    object btnUpdateDirtyObjects: TButton
      Left = 52
      Top = 341
      Width = 81
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Update'
      Enabled = False
      TabOrder = 1
      OnClick = btnUpdateDirtyObjectsClick
    end
  end
  object Panel1: TPanel
    Left = 290
    Top = 0
    Width = 338
    Height = 372
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      338
      372)
    object lblMemoryInfo: TLabel
      Left = 4
      Top = 4
      Width = 58
      Height = 13
      Caption = 'Memory Info'
    end
    object lblSharedStrings: TLabel
      Left = 4
      Top = 283
      Width = 69
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Shared Strings'
    end
    object Label2: TLabel
      Left = 184
      Top = 4
      Width = 38
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Settings'
    end
    object btnUpdateMemoryInfo: TButton
      Left = 95
      Top = 341
      Width = 81
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Update'
      TabOrder = 2
      OnClick = btnUpdateMemoryInfoClick
    end
    object mmoMemoryInfo: TMemo
      Left = 0
      Top = 20
      Width = 176
      Height = 255
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object mmoSharedStrings: TMemo
      Left = 0
      Top = 299
      Width = 176
      Height = 33
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 1
    end
    object Panel2: TPanel
      Left = 184
      Top = 20
      Width = 149
      Height = 153
      Anchors = [akTop, akRight]
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 3
      object Button1: TButton
        Left = 8
        Top = 64
        Width = 121
        Height = 25
        Caption = 'Show Log Window'
        TabOrder = 3
        OnClick = Button1Click
      end
      object btnClearLog: TButton
        Left = 8
        Top = 96
        Width = 121
        Height = 25
        Caption = 'Clear log'
        TabOrder = 4
        OnClick = btnClearLogClick
      end
      object cbOclDebugger: TCheckBox
        Left = 8
        Top = 128
        Width = 97
        Height = 17
        Caption = 'Ocl debugger'
        TabOrder = 5
        OnClick = cbOclDebuggerClick
      end
      object cbLogSQL: TCheckBox
        Left = 8
        Top = 40
        Width = 121
        Height = 17
        Caption = 'Log SQL statements'
        TabOrder = 2
        OnClick = cbLogSQLClick
      end
      object cbLogPMapper: TCheckBox
        Left = 8
        Top = 24
        Width = 121
        Height = 17
        Caption = 'Log PMapper calls'
        TabOrder = 1
        OnClick = cbLogPMapperClick
      end
      object cbLogOcl: TCheckBox
        Left = 8
        Top = 8
        Width = 97
        Height = 17
        Caption = 'Log Ocl'
        TabOrder = 0
        OnClick = cbLogOclClick
      end
    end
    object Panel5: TPanel
      Left = 184
      Top = 180
      Width = 149
      Height = 152
      Anchors = [akTop, akRight, akBottom]
      BevelInner = bvRaised
      BevelOuter = bvLowered
      Caption = 'Drop object here to inspect'
      TabOrder = 4
      OnDragDrop = Panel2DragDrop
      OnDragOver = Panel2DragOver
    end
    object btnUpdateDatabase: TButton
      Left = 212
      Top = 341
      Width = 121
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Update Database'
      Enabled = False
      TabOrder = 5
      OnClick = btnUpdateDatabaseClick
    end
  end
  object blhClasses: TBoldListHandle
    RootHandle = brhSystem
    Left = 24
    Top = 32
  end
  object bchDirtyObjects: TBoldCursorHandle
    RootHandle = brhDirtyObjects
    AutoFirst = False
    Left = 184
    Top = 32
  end
  object brhSystem: TBoldReferenceHandle
    Left = 24
    Top = 64
  end
  object brhDirtyObjects: TBoldReferenceHandle
    Left = 152
    Top = 32
  end
end
