object ValueSetDlg: TValueSetDlg
  Left = 411
  Top = 201
  BorderStyle = bsToolWindow
  Caption = 'Value'
  ClientHeight = 231
  ClientWidth = 246
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object pnRight: TPanel
    Left = 0
    Top = 0
    Width = 246
    Height = 190
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lbValueName: TLabel
      Left = 8
      Top = 8
      Width = 63
      Height = 15
      Caption = 'Value Name'
      Enabled = False
    end
    object lbRepresentations: TLabel
      Left = 8
      Top = 56
      Width = 84
      Height = 15
      Caption = 'Representations'
      Enabled = False
    end
    object edValueName: TEdit
      Left = 8
      Top = 24
      Width = 160
      Height = 23
      Enabled = False
      TabOrder = 0
      Text = 'Ex: Monday'
      OnChange = edValueNameChange
    end
    object Memo1: TMemo
      Left = 8
      Top = 72
      Width = 237
      Height = 129
      Enabled = False
      Lines.Strings = (
        '')
      TabOrder = 1
      OnChange = edValueNameChange
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 190
    Width = 246
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OkBtn: TButton
      Left = 76
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Ok'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
      OnClick = OkBtnClick
    end
    object CancelBtn: TButton
      Left = 164
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = CancelBtnClick
    end
  end
end
