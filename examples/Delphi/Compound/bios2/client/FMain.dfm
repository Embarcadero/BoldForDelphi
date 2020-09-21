object Form2: TForm2
  Left = 0
  Top = -2
  Width = 527
  Height = 226
  Caption = 'Account server client'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 136
    Top = 24
    Width = 111
    Height = 13
    Caption = 'Enter Account Number:'
  end
  object lbValidate: TLabel
    Left = 136
    Top = 80
    Width = 67
    Height = 16
    Caption = 'Validate!!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnValidate: TButton
    Left = 248
    Top = 120
    Width = 249
    Height = 25
    Caption = 'Validate using account validator service'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = btnValidateClick
  end
  object btnFetchAccounts: TButton
    Left = 32
    Top = 120
    Width = 185
    Height = 25
    Caption = 'Fetch Accounts from server'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btnFetchAccountsClick
  end
  object edAccountNumber: TEdit
    Left = 136
    Top = 48
    Width = 201
    Height = 21
    TabOrder = 2
    OnChange = edAccountNumberChange
  end
  object blhcAccounts: TBoldListHandleCom
    ConnectionHandle = DM.BoldComConnectionHandle1
    RootHandle = DM.BoldSystemHandleCom1
    Expression = 'Account.allInstances'
    EvaluateInPS = False
    Left = 424
    Top = 32
  end
end
