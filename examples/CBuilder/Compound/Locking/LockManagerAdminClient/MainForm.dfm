object frmMain: TfrmMain
  Left = 180
  Top = 173
  Width = 757
  Height = 500
  Caption = 'Lock Manager Administrator'
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
    Left = 24
    Top = 440
    Width = 68
    Height = 13
    Caption = 'Server Name: '
  end
  object PageControl1: TPageControl
    Left = 23
    Top = 8
    Width = 705
    Height = 417
    ActivePage = tsLockManagerStatus
    TabIndex = 0
    TabOrder = 0
    object tsLockManagerStatus: TTabSheet
      Caption = 'Lock Manager'
      OnShow = tsLockManagerStatusShow
      object rgLockManagerStatus: TRadioGroup
        Left = 16
        Top = 24
        Width = 169
        Height = 105
        Caption = 'Status'
        Items.Strings = (
          'Active'
          'Suspended')
        TabOrder = 0
      end
    end
    object tsClients: TTabSheet
      Caption = 'Clients'
      ImageIndex = 1
      OnShow = tsClientsShow
      object Label1: TLabel
        Left = 19
        Top = 5
        Width = 31
        Height = 13
        Caption = 'Clients'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lbClientName: TLabel
        Left = 393
        Top = 24
        Width = 21
        Height = 13
        Caption = '       '
      end
      object lbClient: TLabel
        Left = 352
        Top = 24
        Width = 32
        Height = 13
        Caption = 'Client: '
      end
      object lbClients: TListBox
        Left = 16
        Top = 26
        Width = 289
        Height = 281
        ItemHeight = 13
        PopupMenu = pmClient
        TabOrder = 0
        OnClick = lbClientsClick
      end
      object cbShowLocks: TCheckBox
        Left = 352
        Top = 4
        Width = 137
        Height = 17
        Caption = 'Show held locks'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = cbShowLocksClick
      end
      object lvLocks: TListView
        Left = 352
        Top = 48
        Width = 250
        Height = 257
        Columns = <
          item
            Caption = 'Lock name'
          end
          item
            Caption = 'Time held'
          end>
        TabOrder = 2
        ViewStyle = vsReport
      end
      object rgList: TRadioGroup
        Left = 16
        Top = 314
        Width = 209
        Height = 56
        ItemIndex = 0
        Items.Strings = (
          'List all registered clients'
          'List locking clients')
        TabOrder = 3
        OnClick = rgListClick
      end
    end
    object tsLocks: TTabSheet
      Caption = 'Locks'
      ImageIndex = 2
      object Label2: TLabel
        Left = 16
        Top = 16
        Width = 50
        Height = 13
        Caption = 'Held locks'
      end
      object ListView2: TListView
        Left = 16
        Top = 43
        Width = 250
        Height = 294
        Columns = <
          item
            Caption = 'Lock name'
          end
          item
            Caption = 'Time held'
          end>
        TabOrder = 0
        ViewStyle = vsList
      end
      object CheckBox2: TCheckBox
        Left = 360
        Top = 16
        Width = 145
        Height = 17
        Caption = 'Show clients holding lock'
        TabOrder = 1
      end
      object ListBox2: TListBox
        Left = 352
        Top = 44
        Width = 233
        Height = 293
        ItemHeight = 13
        TabOrder = 2
      end
    end
  end
  object BitBtn2: TBitBtn
    Left = 613
    Top = 428
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkClose
  end
  object edServerName: TEdit
    Left = 96
    Top = 429
    Width = 137
    Height = 21
    TabOrder = 2
  end
  object btnConnect: TButton
    Left = 260
    Top = 431
    Width = 62
    Height = 22
    Caption = '&Connect'
    TabOrder = 3
    OnClick = btnConnectClick
  end
  object pmClient: TPopupMenu
    Left = 236
    Top = 208
    object Kill1: TMenuItem
      Caption = 'Kill'
      OnClick = Kill1Click
    end
    object ShowLocks1: TMenuItem
      Caption = 'Show Locks'
      OnClick = ShowLocks1Click
    end
  end
  object pmLock: TPopupMenu
    Left = 484
    Top = 240
    object Free1: TMenuItem
      Caption = '&Free'
    end
    object Viewclientsholdinglock1: TMenuItem
      Caption = '&View clients holding lock'
    end
  end
end
