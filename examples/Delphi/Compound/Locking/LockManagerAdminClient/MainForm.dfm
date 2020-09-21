object FMain: TFMain
  Left = 105
  Top = 143
  Width = 927
  Height = 643
  Caption = 'Lock Manager Administrator'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Label3: TLabel
    Left = 30
    Top = 561
    Width = 86
    Height = 16
    Caption = 'Server Name: '
  end
  object BitBtn1: TBitBtn
    Left = 522
    Top = 551
    Width = 92
    Height = 31
    Caption = '&Refresh'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object PageControl1: TPageControl
    Left = 30
    Top = 20
    Width = 867
    Height = 513
    ActivePage = tsLockManagerStatus
    TabOrder = 1
    object tsLockManagerStatus: TTabSheet
      Caption = 'Lock Manager'
      OnShow = tsLockManagerStatusShow
      object rgLockManagerStatus: TRadioGroup
        Left = 20
        Top = 30
        Width = 208
        Height = 129
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
        Left = 23
        Top = 6
        Width = 40
        Height = 16
        Caption = 'Clients'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object lbClientName: TLabel
        Left = 484
        Top = 30
        Width = 21
        Height = 16
        Caption = '       '
      end
      object lbClient: TLabel
        Left = 433
        Top = 30
        Width = 39
        Height = 16
        Caption = 'Client: '
      end
      object lbclients: TListBox
        Left = 20
        Top = 32
        Width = 355
        Height = 346
        ItemHeight = 16
        PopupMenu = pmClient
        TabOrder = 0
        OnClick = lbclientsClick
      end
      object cbShowLocks: TCheckBox
        Left = 433
        Top = 5
        Width = 169
        Height = 21
        Caption = 'Show held locks'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = cbShowLocksClick
      end
      object lvLocks: TListView
        Left = 433
        Top = 59
        Width = 308
        Height = 316
        Columns = <
          item
            Caption = 'Lock name'
            Width = 62
          end
          item
            Caption = 'Time held'
            Width = 62
          end>
        TabOrder = 2
        ViewStyle = vsList
      end
      object rgList: TRadioGroup
        Left = 20
        Top = 386
        Width = 257
        Height = 69
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
        Left = 20
        Top = 20
        Width = 64
        Height = 16
        Caption = 'Held locks'
      end
      object ListView2: TListView
        Left = 20
        Top = 53
        Width = 307
        Height = 362
        Columns = <
          item
            Caption = 'Lock name'
            Width = 62
          end
          item
            Caption = 'Time held'
            Width = 62
          end>
        TabOrder = 0
        ViewStyle = vsList
      end
      object CheckBox2: TCheckBox
        Left = 443
        Top = 20
        Width = 179
        Height = 21
        Caption = 'Show clients holding lock'
        TabOrder = 1
      end
      object ListBox2: TListBox
        Left = 433
        Top = 54
        Width = 287
        Height = 361
        ItemHeight = 16
        TabOrder = 2
      end
    end
  end
  object BitBtn2: TBitBtn
    Left = 768
    Top = 551
    Width = 92
    Height = 31
    TabOrder = 2
    Kind = bkClose
  end
  object edServerName: TEdit
    Left = 118
    Top = 556
    Width = 169
    Height = 24
    TabOrder = 3
  end
  object btnConnect: TButton
    Left = 320
    Top = 556
    Width = 76
    Height = 27
    Caption = '&Connect'
    TabOrder = 4
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
