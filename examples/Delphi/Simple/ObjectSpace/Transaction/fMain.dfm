object frmMain: TfrmMain
  Left = 2
  Top = -1
  Width = 377
  Height = 407
  Caption = 'frmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 184
    Width = 353
    Height = 169
    Caption = 'Accounts:'
    TabOrder = 0
    object Label1: TLabel
      Left = 137
      Top = 16
      Width = 80
      Height = 13
      Alignment = taRightJustify
      Caption = 'Account Number'
    end
    object Label2: TLabel
      Left = 192
      Top = 40
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = 'Total'
    end
    object Label3: TLabel
      Left = 169
      Top = 64
      Width = 47
      Height = 13
      Alignment = taRightJustify
      Caption = 'Credit limit'
    end
    object BoldListBox1: TBoldListBox
      Left = 8
      Top = 16
      Width = 121
      Height = 113
      Alignment = taLeftJustify
      BoldHandle = blhAllAccounts
      BoldProperties.InternalDrag = False
      BoldProperties.NilElementMode = neNone
      BoldRowProperties.Expression = 'number'
      DragMode = dmAutomatic
      ItemHeight = 16
      TabOrder = 0
    end
    object BoldNavigator1: TBoldNavigator
      Left = 8
      Top = 136
      Width = 120
      Height = 25
      BoldHandle = blhAllAccounts
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
    end
    object BoldEdit1: TBoldEdit
      Left = 224
      Top = 16
      Width = 121
      Height = 21
      BoldHandle = blhAllAccounts
      BoldProperties.Expression = 'number'
      BoldProperties.ApplyPolicy = bapChange
      ReadOnly = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Alignment = taLeftJustify
      ButtonStyle = bbsNone
      MaxLength = 0
      TabOrder = 2
    end
    object BoldEdit2: TBoldEdit
      Left = 224
      Top = 40
      Width = 121
      Height = 21
      BoldHandle = blhAllAccounts
      BoldProperties.Expression = 'total'
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
    object BoldEdit3: TBoldEdit
      Left = 224
      Top = 64
      Width = 121
      Height = 21
      BoldHandle = blhAllAccounts
      BoldProperties.Expression = 'credit'
      ReadOnly = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Alignment = taLeftJustify
      ButtonStyle = bbsNone
      MaxLength = 0
      TabOrder = 4
    end
    object Button1: TButton
      Left = 268
      Top = 140
      Width = 75
      Height = 25
      Action = dmMain.BoldUpdateDBAction1
      TabOrder = 5
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 353
    Height = 169
    Caption = 'Batch of requests:'
    TabOrder = 1
    object Label4: TLabel
      Left = 136
      Top = 96
      Width = 63
      Height = 13
      BiDiMode = bdLeftToRight
      Caption = 'New request:'
      ParentBiDiMode = False
    end
    object Label10: TLabel
      Left = 168
      Top = 32
      Width = 38
      Height = 13
      Caption = 'Label10'
    end
    object BoldListBox2: TBoldListBox
      Left = 8
      Top = 16
      Width = 121
      Height = 113
      Alignment = taLeftJustify
      BoldHandle = blhAllRequests
      BoldProperties.InternalDrag = False
      BoldProperties.NilElementMode = neNone
      DragMode = dmAutomatic
      ItemHeight = 16
      TabOrder = 0
    end
    object btnRun: TButton
      Left = 8
      Top = 136
      Width = 121
      Height = 25
      Caption = 'Run!'
      TabOrder = 1
      OnClick = btnRunClick
    end
    object btnTransfer: TButton
      Left = 136
      Top = 112
      Width = 73
      Height = 17
      Caption = 'Transfer'
      TabOrder = 2
      OnClick = btnTransferClick
    end
    object btnModifyCredit: TButton
      Left = 136
      Top = 128
      Width = 73
      Height = 17
      Caption = 'Modify credit'
      TabOrder = 3
      OnClick = btnModifyCreditClick
    end
    object btnCloseAccount: TButton
      Left = 136
      Top = 144
      Width = 73
      Height = 17
      Caption = 'Close account'
      TabOrder = 4
      OnClick = btnCloseAccountClick
    end
    object BoldPageControl1: TBoldPageControl
      Left = 136
      Top = 8
      Width = 215
      Height = 81
      ActivePage = tsTransfer
      Style = tsFlatButtons
      TabOrder = 5
      BoldHandle = blhAllRequests
      BoldProperties.Expression = #39'ts'#39' + self.oclType.asString'
      object tsTransfer: TTabSheet
        Caption = 'Transfer'
        TabVisible = False
        object Label5: TLabel
          Left = 56
          Top = 0
          Width = 23
          Height = 13
          Alignment = taRightJustify
          BiDiMode = bdLeftToRight
          Caption = 'From'
          ParentBiDiMode = False
        end
        object Label6: TLabel
          Left = 64
          Top = 24
          Width = 13
          Height = 13
          Alignment = taRightJustify
          BiDiMode = bdLeftToRight
          Caption = 'To'
          ParentBiDiMode = False
        end
        object Label7: TLabel
          Left = 40
          Top = 48
          Width = 36
          Height = 13
          Alignment = taRightJustify
          BiDiMode = bdLeftToRight
          Caption = 'Amount'
          ParentBiDiMode = False
        end
        object BoldComboBox1: TBoldComboBox
          Left = 84
          Top = 0
          Width = 121
          Height = 21
          Alignment = taLeftJustify
          BoldHandle = behTransfer
          BoldListHandle = blhAllAccounts
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'source.number'
          BoldSetValueExpression = 'source'
          BoldSelectChangeAction = bdcsSetValue
          ItemHeight = 13
          TabOrder = 0
        end
        object BoldComboBox2: TBoldComboBox
          Left = 84
          Top = 24
          Width = 121
          Height = 21
          Alignment = taLeftJustify
          BoldHandle = behTransfer
          BoldListHandle = blhAllAccounts
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'target.number'
          BoldRowProperties.Expression = 'number'
          BoldSetValueExpression = 'target'
          BoldSelectChangeAction = bdcsSetValue
          ItemHeight = 13
          TabOrder = 1
        end
        object BoldEdit4: TBoldEdit
          Left = 84
          Top = 48
          Width = 121
          Height = 21
          BoldHandle = behTransfer
          BoldProperties.Expression = 'amount'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          MaxLength = 0
          TabOrder = 2
        end
      end
      object tsClose: TTabSheet
        Caption = 'Close'
        ImageIndex = 1
        TabVisible = False
        object Label11: TLabel
          Left = 40
          Top = 0
          Width = 40
          Height = 13
          Alignment = taRightJustify
          Caption = 'Account'
        end
        object BoldComboBox3: TBoldComboBox
          Left = 84
          Top = 0
          Width = 121
          Height = 21
          Alignment = taLeftJustify
          BoldHandle = behClose
          BoldListHandle = blhAllAccounts
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'account.number'
          BoldRowProperties.Expression = 'number'
          BoldSetValueExpression = 'account'
          BoldSelectChangeAction = bdcsSetValue
          ItemHeight = 0
          TabOrder = 0
        end
      end
      object tsModifyCredit: TTabSheet
        Caption = 'ModifyCredit'
        ImageIndex = 2
        TabVisible = False
        object Label8: TLabel
          Left = 40
          Top = 0
          Width = 40
          Height = 13
          Alignment = taRightJustify
          Caption = 'Account'
        end
        object Label9: TLabel
          Left = 9
          Top = 24
          Width = 71
          Height = 13
          Alignment = taRightJustify
          Caption = 'New credit limit'
        end
        object BoldComboBox4: TBoldComboBox
          Left = 84
          Top = 0
          Width = 121
          Height = 21
          Alignment = taLeftJustify
          BoldHandle = behModifyCredit
          BoldListHandle = blhAllAccounts
          BoldListProperties.DragMode = bdgSelection
          BoldListProperties.DropMode = bdpAppend
          BoldListProperties.NilElementMode = neNone
          BoldProperties.Expression = 'account.number'
          BoldRowProperties.Expression = 'number'
          BoldSetValueExpression = 'account'
          BoldSelectChangeAction = bdcsSetValue
          ItemHeight = 0
          TabOrder = 0
        end
        object BoldEdit5: TBoldEdit
          Left = 84
          Top = 24
          Width = 121
          Height = 21
          BoldHandle = behModifyCredit
          BoldProperties.Expression = 'newCredit'
          ReadOnly = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Alignment = taLeftJustify
          ButtonStyle = bbsNone
          MaxLength = 0
          TabOrder = 1
        end
      end
    end
    object btnDelete: TButton
      Left = 272
      Top = 88
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 6
      OnClick = btnDeleteClick
    end
  end
  object blhAllAccounts: TBoldListHandle
    RootHandle = dmMain.bshMain
    Expression = 'Account.allInstances'
    Left = 56
    Top = 288
  end
  object blhAllRequests: TBoldListHandle
    RootHandle = dmMain.bshMain
    Expression = 'Request.allInstances'
    Left = 80
    Top = 80
  end
  object BoldPlaceableAFP1: TBoldPlaceableAFP
    Left = 104
    Top = 32
  end
  object behTransfer: TBoldExpressionHandle
    RootHandle = blhAllRequests
    Expression = 'self->filterOnType(Transfer)->first'
    Left = 152
    Top = 24
  end
  object behClose: TBoldExpressionHandle
    RootHandle = blhAllRequests
    Expression = 'self->filterOnType(Close)->first'
    Left = 152
    Top = 40
  end
  object behModifyCredit: TBoldExpressionHandle
    RootHandle = blhAllRequests
    Expression = 'self->filterOnType(ModifyCredit)->first'
    Left = 152
    Top = 56
  end
  object MainMenu1: TMainMenu
    Left = 320
    Top = 328
    object File1: TMenuItem
      Caption = 'File'
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Debug1: TMenuItem
      Caption = 'Debug'
      object SystemDebugger1: TMenuItem
        Caption = 'SystemDebugger'
        OnClick = SystemDebugger1Click
      end
    end
    object N1: TMenuItem
      Caption = '?'
    end
  end
end
