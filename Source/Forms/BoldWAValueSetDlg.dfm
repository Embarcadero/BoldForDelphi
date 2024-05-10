object ValueSetDlg: TValueSetDlg
  Left = 411
  Top = 201
  BorderStyle = bsToolWindow
  Caption = 'Value'
  ClientHeight = 243
  ClientWidth = 254
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnRight: TPanel
    Left = 0
    Top = 0
    Width = 254
    Height = 202
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lbValueName: TLabel
      Left = 8
      Top = 8
      Width = 58
      Height = 13
      Caption = 'Value Name'
      Enabled = False
    end
    object lbRepresentations: TLabel
      Left = 8
      Top = 56
      Width = 77
      Height = 13
      Caption = 'Representations'
      Enabled = False
    end
    object edValueName: TEdit
      Left = 8
      Top = 24
      Width = 160
      Height = 21
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
    Top = 202
    Width = 254
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
