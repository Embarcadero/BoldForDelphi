object frmMain: TfrmMain
  Left = 2
  Top = 2
  Width = 606
  Height = 577
  Caption = 'frmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 93
    Width = 593
    Height = 201
    Caption = 'Document View: '
    TabOrder = 0
    object Label2: TLabel
      Left = 8
      Top = 24
      Width = 41
      Height = 13
      Caption = 'Projects:'
    end
    object Label3: TLabel
      Left = 136
      Top = 24
      Width = 57
      Height = 13
      Caption = 'Documents:'
    end
    object Label5: TLabel
      Left = 336
      Top = 24
      Width = 35
      Height = 13
      Caption = 'History:'
    end
    object BoldListBox1: TBoldListBox
      Left = 8
      Top = 40
      Width = 121
      Height = 121
      Alignment = taLeftJustify
      BoldHandle = blhAllProjects
      BoldProperties.InternalDrag = False
      BoldProperties.NilElementMode = neNone
      BoldRowProperties.Expression = 'name'
      DragMode = dmAutomatic
      ItemHeight = 16
      TabOrder = 0
    end
    object BoldNavigator1: TBoldNavigator
      Left = 8
      Top = 168
      Width = 56
      Height = 25
      BoldHandle = blhAllProjects
      TabOrder = 1
      VisibleButtons = [nbInsert, nbDelete]
      ImageIndices.nbFirst = -1
      ImageIndices.nbPrior = -1
      ImageIndices.nbNext = -1
      ImageIndices.nbLast = -1
      ImageIndices.nbInsert = -1
      ImageIndices.nbDelete = -1
      ImageIndices.nbMoveUp = -1
      ImageIndices.nbMoveDown = -1
      DeleteQuestion = 'Delete object?'
      UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
      RemoveQuestion = 'Remove "%1:s" from the list?'
    end
    object BoldNavigator2: TBoldNavigator
      Left = 136
      Top = 168
      Width = 56
      Height = 25
      BoldHandle = blhDocs
      TabOrder = 2
      VisibleButtons = [nbInsert, nbDelete]
      ImageIndices.nbFirst = -1
      ImageIndices.nbPrior = -1
      ImageIndices.nbNext = -1
      ImageIndices.nbLast = -1
      ImageIndices.nbInsert = -1
      ImageIndices.nbDelete = -1
      ImageIndices.nbMoveUp = -1
      ImageIndices.nbMoveDown = -1
      DeleteQuestion = 'Delete object?'
      UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
      RemoveQuestion = 'Remove "%1:s" from the list?'
    end
    object Button1: TButton
      Left = 232
      Top = 168
      Width = 97
      Height = 25
      Caption = 'View history'
      TabOrder = 3
      OnClick = Button1Click
    end
    object grdOldVersions: TBoldGrid
      Left = 344
      Top = 40
      Width = 249
      Height = 121
      AddNewAtEnd = False
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = blhDocVersions
      BoldProperties.InternalDrag = False
      BoldProperties.NilElementMode = neNone
      Columns = <
        item
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        item
          BoldProperties.Expression = 'title'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        item
          BoldProperties.Expression = 'self.boldTime.timestampToTime'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        item
          BoldProperties.Expression = 'version->select(time = self.boldTime)->first.name'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end>
      DefaultRowHeight = 17
      EnableColAdjust = False
      TabOrder = 4
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      ColWidths = (
        17
        63
        85
        59)
    end
    object BoldGrid2: TBoldGrid
      Left = 136
      Top = 40
      Width = 193
      Height = 121
      AddNewAtEnd = False
      BoldAutoColumns = False
      BoldShowConstraints = False
      BoldHandle = blhDocuments
      BoldProperties.InternalDrag = False
      BoldProperties.NilElementMode = neNone
      Columns = <
        item
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        item
          BoldProperties.Expression = 'title'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end
        item
          BoldProperties.Expression = 'author.name'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
        end>
      DefaultRowHeight = 17
      EnableColAdjust = False
      TabOrder = 5
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      ColWidths = (
        17
        105
        64)
    end
    object btnPublish: TButton
      Left = 496
      Top = 168
      Width = 91
      Height = 25
      Caption = 'Publish version'
      Enabled = False
      TabOrder = 6
      OnClick = btnPublishClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 496
    Width = 593
    Height = 49
    TabOrder = 1
    object Button3: TButton
      Left = 414
      Top = 16
      Width = 75
      Height = 25
      Action = BoldIBDatabaseAction1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 502
      Top = 16
      Width = 75
      Height = 25
      Action = BoldActivateSystemAction1
      TabOrder = 1
    end
    object Button4: TButton
      Left = 6
      Top = 16
      Width = 75
      Height = 25
      Action = BoldUpdateDBAction1
      TabOrder = 2
    end
  end
  object GroupBox3: TGroupBox
    Left = 184
    Top = 8
    Width = 409
    Height = 76
    Caption = 'New User'
    TabOrder = 2
    object Label8: TLabel
      Left = 146
      Top = 11
      Width = 255
      Height = 16
      Caption = 'Enter a User name then press Create'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label1: TLabel
      Left = 146
      Top = 29
      Width = 144
      Height = 16
      Caption = 'to create a new user.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnNewUser: TButton
      Left = 71
      Top = 46
      Width = 66
      Height = 24
      Caption = '&Create User'
      TabOrder = 0
      OnClick = btnNewUserClick
    end
    object edtNewUserName: TEdit
      Left = 8
      Top = 20
      Width = 129
      Height = 21
      TabOrder = 1
      Text = 'No Name'
    end
  end
  object grbADocument: TGroupBox
    Left = 0
    Top = 301
    Width = 593
    Height = 193
    Caption = 'title + '#39':'#39
    TabOrder = 3
    object Label4: TLabel
      Left = 136
      Top = 16
      Width = 27
      Height = 13
      Caption = 'Parts:'
    end
    object Label6: TLabel
      Left = 264
      Top = 56
      Width = 24
      Height = 13
      Caption = 'Text:'
    end
    object Label7: TLabel
      Left = 264
      Top = 16
      Width = 35
      Height = 13
      Caption = 'Header'
    end
    object BoldListBox3: TBoldListBox
      Left = 136
      Top = 32
      Width = 121
      Height = 121
      Alignment = taLeftJustify
      BoldHandle = blhDocParts
      BoldProperties.InternalDrag = False
      BoldProperties.NilElementMode = neNone
      BoldRowProperties.Expression = 'header'
      DragMode = dmAutomatic
      ItemHeight = 16
      TabOrder = 0
    end
    object BoldNavigator3: TBoldNavigator
      Left = 136
      Top = 160
      Width = 56
      Height = 25
      BoldHandle = blhDocParts
      TabOrder = 1
      VisibleButtons = [nbInsert, nbDelete]
      ImageIndices.nbFirst = -1
      ImageIndices.nbPrior = -1
      ImageIndices.nbNext = -1
      ImageIndices.nbLast = -1
      ImageIndices.nbInsert = -1
      ImageIndices.nbDelete = -1
      ImageIndices.nbMoveUp = -1
      ImageIndices.nbMoveDown = -1
      DeleteQuestion = 'Delete object?'
      UnlinkQuestion = 'Unlink "%1:s" from "%2:s"?'
      RemoveQuestion = 'Remove "%1:s" from the list?'
    end
    object BoldMemo1: TBoldMemo
      Left = 264
      Top = 72
      Width = 185
      Height = 113
      Alignment = taLeftJustify
      BoldHandle = blhDocParts
      BoldProperties.Expression = 'text'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 0
      ReadOnly = False
      TabOrder = 2
    end
    object BoldEdit1: TBoldEdit
      Left = 264
      Top = 32
      Width = 185
      Height = 21
      BoldHandle = blhDocParts
      BoldProperties.Expression = 'header'
      ReadOnly = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Alignment = taLeftJustify
      ButtonStyle = bbsNone
      MaxLength = 0
      TabOrder = 3
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 8
    Width = 161
    Height = 76
    Caption = 'Current User'
    TabOrder = 4
    object BoldComboBox1: TBoldComboBox
      Left = 7
      Top = 22
      Width = 139
      Height = 21
      Hint = 'Select the Current User'
      Alignment = taLeftJustify
      BoldHandle = behCurrentUser
      BoldListHandle = blhAllUsers
      BoldListProperties.DragMode = bdgSelection
      BoldListProperties.DropMode = bdpAppend
      BoldListProperties.NilElementMode = neNone
      BoldProperties.Expression = 'name'
      BoldProperties.NilStringRepresentation = '<No user selected>'
      BoldRowProperties.Expression = 'name'
      BoldSelectChangeAction = bdcsSetValue
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object behCurrentUser: TBoldExpressionHandle
    StaticSystemHandle = dmMain.BoldSystemHandle1
    RootHandle = dmMain.BoldSystemHandle1
    Expression = 'Context.allInstances->first.currentUser'
    Left = 104
    Top = 64
  end
  object blhAllUsers: TBoldListHandle
    StaticSystemHandle = dmMain.BoldSystemHandle1
    RootHandle = dmMain.BoldSystemHandle1
    Expression = 'Person.allInstances'
    Left = 168
    Top = 64
  end
  object blhAllProjects: TBoldListHandle
    RootHandle = dmMain.BoldSystemHandle1
    Expression = 'Project.allInstances'
    Left = 32
    Top = 128
  end
  object blhDocuments: TBoldListHandle
    RootHandle = blhAllProjects
    Expression = 'viewContains'
    Left = 160
    Top = 112
  end
  object blhDocParts: TBoldListHandle
    RootHandle = behDisplayDoc
    Expression = 'documentPart'
    Left = 168
    Top = 352
  end
  object BoldPlaceableAFP1: TBoldPlaceableAFP
    Left = 464
    Top = 88
  end
  object blhDocs: TBoldListHandle
    RootHandle = blhAllProjects
    Expression = 'contains'
    Left = 152
    Top = 200
  end
  object bvhDispOld: TBoldVariableHandle
    StaticSystemHandle = dmMain.BoldSystemHandle1
    ValueTypeName = 'Boolean'
    Left = 216
    Top = 216
  end
  object BoldOclVariables1: TBoldOclVariables
    Variables = <
      item
        BoldHandle = bvhDispOld
        VariableName = 'dispOld'
        UseListElement = False
      end
      item
        BoldHandle = blhDocuments
        VariableName = 'currDoc'
        UseListElement = False
      end
      item
        BoldHandle = blhDocVersions
        VariableName = 'oldDoc'
        UseListElement = False
      end>
    Left = 344
    Top = 216
  end
  object behDisplayDoc: TBoldExpressionHandle
    StaticSystemHandle = dmMain.BoldSystemHandle1
    RootHandle = dmMain.BoldSystemHandle1
    RootTypeName = 'Document'
    Expression = 'if dispOld then oldDoc else currDoc endif'
    Variables = BoldOclVariables1
    Left = 344
    Top = 248
  end
  object bdhDocVersions: TBoldDerivedHandle
    StaticSystemHandle = dmMain.BoldSystemHandle1
    RootHandle = blhDocuments
    OnDeriveAndSubscribe = bdhDocVersionsDeriveAndSubscribe
    ValueTypeName = 'Collection(Document)'
    Left = 352
    Top = 128
  end
  object bdhPartVersions: TBoldDerivedHandle
    StaticSystemHandle = dmMain.BoldSystemHandle1
    RootHandle = behAllHistoricalParts
    OnDeriveAndSubscribe = bdhPartVersionsDeriveAndSubscribe
    ValueTypeName = 'Collection(DocumentPart)'
    Left = 384
    Top = 160
  end
  object bvdPartVersions: TBoldOclVariables
    Variables = <
      item
        BoldHandle = bdhPartVersions
        VariableName = 'partVersions'
        UseListElement = False
      end>
    Left = 424
    Top = 160
  end
  object blhDocVersions: TBoldListHandle
    StaticSystemHandle = dmMain.BoldSystemHandle1
    RootHandle = bdhDocVersions
    Enabled = False
    Expression = 'self->union(partVersions.document)->orderby(boldTime)'
    Variables = bvdPartVersions
    Left = 480
    Top = 136
  end
  object behAllHistoricalParts: TBoldExpressionHandle
    RootHandle = bdhDocVersions
    Expression = 'documentPart.atTime(timestampNow)'
    Left = 352
    Top = 160
  end
  object ActionList1: TActionList
    Left = 336
    Top = 496
    object BoldActivateSystemAction1: TBoldActivateSystemAction
      Category = 'Bold Actions'
      Caption = 'Open system'
      BoldSystemHandle = dmMain.BoldSystemHandle1
      OnSystemOpened = BoldActivateSystemAction1SystemOpened
      OpenCaption = 'Open system'
      CloseCaption = 'Close system'
      SaveQuestion = 'There are dirty objects. Save them before closing system?'
      SaveOnClose = saAsk
    end
    object BoldUpdateDBAction1: TBoldUpdateDBAction
      Category = 'Bold Actions'
      Caption = 'Update DB'
      BoldSystemHandle = dmMain.BoldSystemHandle1
    end
    object BoldIBDatabaseAction1: TBoldIBDatabaseAction
      Category = 'Bold Actions'
      Caption = 'Create DB'
      BoldSystemHandle = dmMain.BoldSystemHandle1
      Username = 'SYSDBA'
      Password = 'masterkey'
    end
  end
  object BoldCaptionController1: TBoldCaptionController
    BoldHandle = behDisplayDoc
    BoldProperties.Expression = 'title + '#39':'#39
    TrackControl = grbADocument
    Left = 80
    Top = 224
  end
end
