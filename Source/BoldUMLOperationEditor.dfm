object BoldUMLOperationEditForm: TBoldUMLOperationEditForm
  Left = 0
  Top = 0
  ActiveControl = tbxOperationName
  BorderStyle = bsDialog
  Caption = 'Edit Operation'
  ClientHeight = 465
  ClientWidth = 432
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  DesignSize = (
    432
    465)
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 4
    Width = 416
    Height = 415
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      DesignSize = (
        408
        387)
      object lblOperationDelphiFunctionType: TLabel
        Left = 22
        Top = 128
        Width = 70
        Height = 13
        Alignment = taRightJustify
        Caption = 'O&peration kind'
      end
      object lblOperationName: TLabel
        Left = 65
        Top = 14
        Width = 27
        Height = 13
        Alignment = taRightJustify
        Caption = '&Name'
      end
      object lblOperationOwnerScope: TLabel
        Left = 29
        Top = 104
        Width = 63
        Height = 13
        Alignment = taRightJustify
        Caption = '&Owner scope'
      end
      object lblOperationStereotype: TLabel
        Left = 38
        Top = 56
        Width = 54
        Height = 13
        Alignment = taRightJustify
        Caption = '&Stereotype'
      end
      object lblOperationVisibility: TLabel
        Left = 55
        Top = 80
        Width = 37
        Height = 13
        Alignment = taRightJustify
        Caption = 'Visi&bility'
      end
      object tbxOperationStereotype: TcxBoldTextEdit
        Left = 100
        Top = 52
        HelpContext = 1150
        DataBinding.BoldHandle = brhOperation
        DataBinding.BoldProperties.Expression = 'stereotypeName'
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldRowProperties.Expression = ''
        Properties.Alignment.Horz = taLeftJustify
        Anchors = [akLeft, akTop, akRight]
        ParentFont = False
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 2
        Width = 295
      end
      object tbxOperationName: TcxBoldTextEdit
        Left = 100
        Top = 10
        HelpContext = 83
        DataBinding.BoldHandle = brhOperation
        DataBinding.BoldProperties.Expression = 'name'
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldRowProperties.Expression = ''
        Properties.Alignment.Horz = taLeftJustify
        Anchors = [akLeft, akTop, akRight]
        ParentFont = False
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 0
        Width = 295
      end
      object cmbOwnerScope: TcxBoldComboBox
        Left = 100
        Top = 100
        HelpContext = 80
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = brhOperation
        DataBinding.BoldProperties.Expression = 'ownerScope'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllOwnerScope
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownListStyle = lsEditFixedList
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 4
        Width = 295
      end
      object cmbOperationVisibility: TcxBoldComboBox
        Left = 100
        Top = 76
        HelpContext = 1250
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = brhOperation
        DataBinding.BoldProperties.Expression = 'visibility'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllVisibilityKind
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownListStyle = lsEditFixedList
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 3
        Width = 295
      end
      object cmbDelphiFunctionType: TcxBoldComboBox
        Left = 100
        Top = 124
        HelpContext = 81
        Anchors = [akLeft, akTop, akRight]
        DataBinding.BoldHandle = brhOperation
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.OperationKind'#39'].value'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        DataBinding.BoldProperties.NilRepresentation = ''
        Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.bchDelphiFunctionType
        Properties.BoldRowProperties.Expression = ''
        Properties.BoldRowProperties.NilRepresentation = ''
        Properties.BoldSelectChangeAction = bdscSetText
        Properties.Alignment.Horz = taLeftJustify
        Properties.DropDownListStyle = lsEditFixedList
        Style.BorderStyle = ebsFlat
        Style.Color = clWindow
        TabOrder = 5
        Width = 295
      end
      object cbOperationOverrideInAllSubclasses: TcxBoldCheckBox
        Left = 100
        Top = 33
        Hint = 
          '|If checked, the operation will be added to all subclasses at co' +
          'de generation.'
        HelpContext = 1260
        Caption = 'Override in all s&ubclasses'
        DataBinding.BoldHandle = dmBoldUMLModelEditorHandles.behOperation
        DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.OverrideInAllSubclasses'#39'].value'
        DataBinding.BoldProperties.ApplyPolicy = bapChange
        Properties.ImmediatePost = True
        Style.BorderStyle = ebsFlat
        Style.TransparentBorder = False
        TabOrder = 1
      end
    end
  end
  object CancelBtn: TButton
    Left = 345
    Top = 427
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object OKBtn: TButton
    Left = 264
    Top = 427
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object pcOperationTabs: TPageControl
    Left = 21
    Top = 179
    Width = 386
    Height = 228
    ActivePage = TabSheet5
    TabOrder = 1
    object TabSheet5: TTabSheet
      HelpContext = 84
      Caption = '&Parameters'
      object cxBoldGrid5: TcxGrid
        Left = 0
        Top = 30
        Width = 378
        Height = 170
        Align = alClient
        TabOrder = 1
        object cxBoldGrid5DBTableView1: TcxGridDBTableView
          Navigator.Buttons.CustomButtons = <>
          ScrollbarAnnotations.CustomAnnotations = <>
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
        end
        object cxBoldGrid5BoldTableView: TcxGridBoldTableView
          OnDblClick = cxBoldGrid5BoldTableViewDblClick
          Navigator.Buttons.CustomButtons = <>
          ScrollbarAnnotations.CustomAnnotations = <>
          DataController.BoldHandle = blhParameters
          DataController.Filter.Options = [fcoSoftNull]
          DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          NewItemRow.InfoText = 'Click here to add a new param'
          OptionsBehavior.CellHints = True
          OptionsData.Appending = True
          OptionsData.Editing = False
          OptionsSelection.CellSelect = False
          OptionsView.ColumnAutoWidth = True
          OptionsView.GroupByBox = False
          object cxBoldGrid5BoldTableViewConstraints: TcxGridBoldColumn
            Caption = #167
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'constraints->select(c|not c)->size = 0'
            BestFitMaxWidth = 16
            Options.Editing = False
            Options.Focusing = False
            Options.IncSearch = False
            Options.HorzSizing = False
            Options.Moving = False
            Width = 20
          end
          object cxBoldGrid5BoldTableViewName: TcxGridBoldColumn
            Caption = 'Name'
            DataBinding.BoldProperties.Expression = 'name'
            PropertiesClassName = 'TcxTextEditProperties'
            Width = 119
          end
          object cxBoldGrid5BoldTableViewKind: TcxGridBoldColumn
            Caption = 'Kind'
            DataBinding.BoldProperties.Expression = 'kind'
            PropertiesClassName = 'TcxComboBoxProperties'
            Properties.DropDownListStyle = lsEditFixedList
            Properties.DropDownRows = 4
            Properties.Items.Strings = (
              'in'
              'out'
              'inout'
              'return')
            Width = 50
          end
          object cxBoldGrid5BoldTableViewType: TcxGridBoldColumn
            Caption = 'Type'
            DataBinding.BoldProperties.Expression = 'typeName'
            PropertiesClassName = 'TcxBoldLookupComboBoxProperties'
            Properties.BoldLookupListHandle = dmBoldUMLModelEditorHandles.blhAllDataTypes
            Properties.DropDownListStyle = lsEditList
            Properties.ListColumns = <
              item
                BoldProperties.Expression = ''
              end>
            Width = 84
          end
          object cxBoldGrid5BoldTableViewExpressionname: TcxGridBoldColumn
            Caption = 'Expression name'
            DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.ExpressionName'#39'].value'
            PropertiesClassName = 'TcxTextEditProperties'
            Visible = False
          end
          object cxBoldGrid5BoldTableViewIsconst: TcxGridBoldColumn
            Caption = 'Is const'
            DataBinding.ValueType = 'Boolean'
            DataBinding.BoldProperties.Expression = 'taggedValue['#39'Bold.IsConst'#39'].value = '#39'True'#39
            PropertiesClassName = 'TcxCheckBoxProperties'
            Width = 49
          end
        end
        object cxBoldGrid5Level1: TcxGridLevel
          GridView = cxBoldGrid5BoldTableView
        end
      end
      object ToolBar1: TToolBar
        Left = 0
        Top = 0
        Width = 378
        Height = 30
        BorderWidth = 1
        Caption = 'ToolBar1'
        EdgeBorders = [ebTop, ebBottom]
        Images = cxImageList1
        List = True
        TabOrder = 0
        object btAdd: TToolButton
          Left = 0
          Top = 0
          Action = actAdd
        end
        object btRemove: TToolButton
          Left = 23
          Top = 0
          Action = actDelete
        end
        object ToolButton1: TToolButton
          Left = 46
          Top = 0
          Width = 8
          Caption = 'ToolButton1'
          ImageIndex = 2
          Style = tbsSeparator
        end
        object ToolButton2: TToolButton
          Left = 54
          Top = 0
          Action = actMoveUp
        end
        object ToolButton3: TToolButton
          Left = 77
          Top = 0
          Action = actMoveDown
        end
      end
    end
  end
  object brhOperation: TBoldReferenceHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    StaticValueTypeName = 'UMLOperation'
    Left = 192
    Top = 152
  end
  object BoldPropertiesController1: TBoldPropertiesController
    BoldHandle = brhOperation
    BoldProperties.Expression = 'owner.name + '#39'.'#39' + name'
    DrivenProperties = <
      item
        VCLComponent = TabSheet1
        PropertyName = 'Caption'
      end>
    Left = 188
    Top = 104
  end
  object blhParameters: TBoldListHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = brhOperation
    Expression = 'parameter'
    Left = 193
    Top = 267
  end
  object cxImageList1: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 16842812
    ImageInfo = <
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001B744558745469746C65004164643B506C75733B426172733B526962
          626F6E3B9506332F0000036349444154785E35927D6C535518C69F73EE6DEB64
          63A3AEFB60A3A36E33B8C56581E0D8707E21CC1A43A2A22304FE3001512A86C4
          E900132451FF503367420043B244364C483031465C248B4441C0980C45B4D065
          CDBA4ECAE82AAC5DBBDE8FF3E1BD27F1397973DE9C3CBFF7233964226FC2D543
          A53E0280443E3FD752525AB14323FA06685A3381E492F329C6ADF39954E2F8C9
          C3DBA6018858DE940A9C2C5870C1D51BB6FAF61DBB327860F81A1BFE25297FB8
          3127C7EFE4E5D5745E9EBB9991239766E481937FE4DE1818DB0DC0EB322EABBA
          B63FD5EB7D6CCBBE6F1B83FE9E67BA82E084C0E4123697CAE0D109BC94805B0C
          E7AFCC606A66EEECF75FBCBB753AFAEB2201A0BD3E7861B02914D8DBF34408A9
          AC0D2181D3672E23319D81AB950D016CEBED824E809A722FC62E4CE17A343130
          D4DF73507FB9FFAB551E9F6FCF93EB82B879BB088D52504A14FCC9CE4E95F79D
          B80CD396284A8179C7D3DD1144F29FEC5BE1D73E1BA6BEB2C09BEDCD955A7CCE
          44D1744C1687C9045C05EBFC686F0DAADCB08413D2098E89B4E1BC5779965687
          5ED585D03ACBFDA548E7197EFA711C776EDFC5FF12200A7075F4E85975D7D4FA
          F1F4A635A82C5F02A2956CD46D2EEB1D160B455BC19FEE5E0F4A885A45828071
          81137D1B61DB0C1E5D43E4C8CF5858E4D0A1810BBA5CB76DEEBDB768C1E604AE
          EA6B1F40D9121F0A265385BC0E5457530109404A8010E27805EEE60598CDA15B
          8699C8E7CD4784EEC3F2BA00767C340A4AA9327E79300CE1505BDEFF0E9AA681
          5082150DD5604CA26858282E1693D428E42F6666B3909068EF68C5E6171FC7E6
          17BA611A260C93A9029C713CF7FC3A3C1BEE404B5B2398E0989FCBA190FD774C
          CFA46243B11B4B77ADADF67BB236478E10500AA5D2121D5C48354D3A674108A1
          56114C201E4BB1D9F86FA70880FB1EDD3E34B0A229B4E7E1350FC2E22E2011BF
          16C3FCBD050557562DC3CA964608B8B4C4E49F4924A27F1F193F1DD9AF03B0FE
          1AFDE03D113EDC6431B1A96575089212B4AD6D555F581280D902398343308EC9
          EB49DC9A981A75E043000CA46D09005A49457059DB4BC78E77EDFCDAEAFDF892
          DC3B1295EF7C13977D4E444E45E52BCE5BE7AE338555E10FDF0650EE32B30E4B
          D24C0212A8F210EAAED3D01969BB3FD0BCDDE32BEB06D56AD5D09CCDDA66EE62
          EED6EF43A9AB2331008603ABCEFF019D3AAD15CCD8D2E00000000049454E44AE
          426082}
        FileName = 'Images\Actions\Add_16x16.png'
        Keywords = 'Actions;Add'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000019744558745469746C650044656C6574653B52656D6F76653B4D696E
          7573EBA98F410000031A49444154785E7D925F4854591CC7BFE7DE3BA3B9EEBA
          0D3BDBE85A3AA5C16A85586DB4B49459490FBD444B62C63EB450ADECD243D4DA
          4345F4982804D58B0F1541FBB02C0B1995AD44160A8B59499A8330E3486AEA4E
          EBCC9D3F77EE3DE7F43BF78A8F9D3F1CB8E77EBE7C7E3F0E8B9816D4585F5CC0
          00B0B6AE87352B8ABF3CAA33A311BA5ECD20B9E43CEAF07CDFC274ECFACD8B2D
          9300C4B86949179C48E7A1C0FAC6E68253D7063ADB6FBD726E3D8BCB0763F372
          E8BD295FCC99F2E1DB0579FBF9946CBFF93279B2A3F73800BF6214CB145CD770
          D8BFE3D0A9BFD7AD0934EDF97E0D3863B0B8844D5B69F90C06BFC6C0F30EFA06
          A6109D9ABFD773E34CF3E4E86046DDEB273A9F76568583BF36ED0C637AD18690
          804680DAF04421A404E52054E247EFD3284646631DDD679BCEB11FCFDEA90D55
          54BDFAE9E0063D9AC843D73417D469330DCB43081522C0395019F0E3CE9F23F6
          D8F0B32DACF5F2A3EB0DDBD69D280EAD84E500BA0E18068530461BCB830BCF82
          73E19AD98945F4F40E771942E8DB4B02C598331D3CF96708EF6712F8D4089506
          B07BDF667C55F21998BE62AF418D2A2716A9ACEDC2577F6BF04AD0DD52BC2E33
          AF177E9F8E63571E2395E130A093955666D836F77FC8E46173CF7755B0044585
          3EA2D4F202A49AEA945E98CD0512A6806353503E67C54CD3DA288C02947D1344
          CBA57B64A0B93F1A7E03B66543231B5DD7C1C86875C52A388E4436974736938D
          1BB9B4D9BF30BBB87165791075DB6A51E8F31A48CBB3905063D9C07238523907
          FFCF27915EFCAF976D3A70A1BEAC7AEBE0D65DB53EEED54B06DEA9A65A588285
          106E9070045E3F7FEB4406FFDAA2AE0BBF6BEDEE585D15FEE5DBCD6B91E79260
          E95A80B632904B0E028A9698781D476CF4CDD5A1BB6DBFAB00AD2850F945CDFE
          8B7F9486CBF7D5D4872135A6743DCC2D4312CE20487F62248E7791E8FDA1BB3F
          1F0190D4E66C29328958EA4DCFF9E6C9B1C88DFEFBC3F6E4F80C72664EE1AE7B
          266D211E99C1C0A39799C8BF03A7096E01909A2596CD399EE3D73EA6A9722AB6
          B76D2A0A56B7FA0A3EFF019A5EEA4A7367D6B692FDC977C3DDD32F6E8F03C811
          EC3A7E0415947A6BBEDAC8770000000049454E44AE426082}
        FileName = 'Images\Actions\Remove_16x16.png'
        Keywords = 'Actions;Remove'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000011744558745469746C6500446F776E3B4172726F773BBDFC82580000
          029049444154785E65926B48935118C78FD3564A46F4C5BE04E13E6846376390
          45E85022304A717611C9D034BB41855DE665B39517C4795D5898B70CA9B40F5E
          E78DCD255E3237C2FA5239B7D499A2656AA3E5947FE7BC4C51DF033FDE87F3FC
          DF1FCF39EF4B00900DCB8522A0B86DC095EBAD5900C8AA405963240F292C28AF
          1A6897571B9056F5012995039095BDC7CD425D07135DC9D592B8EC0EBEE05E69
          3FF9B9B8CCCA4DB79EF480D6EB8852B4B0A09065A4B246BEE04E492F99B12FB1
          72F38DA26E4CDB97D0396943E70F1B067FD97136B58905B7D07D127EB79E2710
          303B7B99E299A8EAC2D45F079ABECF73F44CD91071BF8105B753DC9D39C15A81
          F05A811E89797A24E476E1728E16569B03755F6751F76516DAD10544A5B7E1BC
          5C83C8540DA4C9CDDC34EB046149AF5BDEF6596099FB87D1F9458EEA4FD3A81E
          9A81C63447F9CDF56AB4DF70E482BA954DB1EE08E2D349BED1690D0B6F8C5328
          78370E759F154F0726F18C52DC3301957E0CC5FA518424542D880262FDD8D75A
          2B6008832E163F5094F542DD3D8EF496118A190AFA54348F205F3786988C56EC
          0F55CA9C7740360A5C285B0363CB062B75263CD29891D668E25052517AED100E
          85A90CEC9229029E4012FF823DDCFC24D78F4624D53A2AFAAD486E18464AFD30
          F23A2D90C4953B768B2F1D63997DA7F2094F703CA67C650A77FFB0EC027694AC
          363332E8243159EDF00D4E2DA43D0F96D97352C51704443F271F276CAC146CF3
          F2F51247AA474A7566C8EB3E636F688ED963876827EB19AC7F88CF896CBE407C
          AE8418699305E8127A075C954A6FBF5A0E89AF58DE753856CAF6588F210ACEE4
          0BFC23D4E460B89A1C3853B472144F9F10658D77A0EC25ABD99E48924944920C
          220A7ACC17F021AECEDFD683ABF98B13FC07806FBA055103A92F000000004945
          4E44AE426082}
        FileName = 'Images\Arrows\MoveDown_16x16.png'
        Keywords = 'Arrows;MoveDown'
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000000F744558745469746C650055703B4172726F773BC85E281400000275
          49444154785E7D527D485351143FEF4DF7219399A9690EA1AF592409936A546C
          5242DA871F907D424C8288502144C706111641909052A0564890454A655F5011
          14EC8FA6188404D11F6111441AA331DBF7EEBBB7732F7BF09CD2E1FDE09EF7FB
          9DDF3DE7BC078C31F84F4802CB87A895D5C43B32059EDB53D07D7312BA86DF8B
          E2CEEBFEC143BE2737F8B9C53301CDDD8FA1B1EB111C3837BED88547CFAD497E
          5673E94CDF1BD7B587338A6FD0AFECEFBC5BCB4D54BEBE636C6907845050A3BE
          EDA23157AF1F6E76AD93DD4DD5B25E9F3754B5E7B449E5A946BBC4409224D96A
          DBE6ABABA9A84C180C90329BA065B7ADB26CFD4E2F723AA155C832068A0212C6
          51EFFDCDA5C5059E9A6A2BCCC752F02B9202E72E1B58CB567AB6B75EA9E21A85
          D0253B50379EEBBEFC3AF0E9FB1FF6EEE75FF6EC5B58E0E58F0536FD658E39DB
          4602A8D10B6DF60E10F2F1F313EDB5F60A47CA648060340DB1041108470924F3
          CDD0E8DAE4D87A78A01DB5BAEC11E4BDA7FAD6141716F6DAB794C387D9300443
          498827148821420B4998FE1A02976B23941615F5DA9C1D6BD5DA9C8C81CE52BA
          61A0B5CE96FF793E011203900885D9DF3141AE5E6104638E0C337309387BC261
          F65D0DF623D18C48A90632C8FA7DFD0F3E02A31428655077D00E84316E26F2A7
          6301A098230D9692550DD91DA4C72F359832B3198F5D781534EA64A00A56E343
          38140A6F878E94201F47505EA3DD016DEA799E608C45314912C2790684221815
          371334C3E09AC80EF7289F4DD1760084885C404131D797E3EC186204EDDFA760
          2E426B904E1368E97921F87864C17FEF8EDF892388DB19EF2415F323471C2747
          D170F91F49FB69F3101644410616F14E708B6BFF01189337125152162A000000
          0049454E44AE426082}
        FileName = 'Images\Arrows\MoveUp_16x16.png'
        Keywords = 'Arrows;MoveUp'
      end>
  end
  object ActionList1: TActionList
    Left = 313
    Top = 267
    object actAdd: TAction
      Caption = 'actAdd'
      ImageIndex = 0
      OnExecute = actAddExecute
    end
    object actDelete: TAction
      Caption = 'actDelete'
      ImageIndex = 1
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
    object actMoveUp: TAction
      Caption = 'Move up'
      ImageIndex = 3
      OnExecute = actMoveUpExecute
    end
    object actMoveDown: TAction
      Caption = 'Move down'
      ImageIndex = 2
      OnExecute = actMoveDownExecute
    end
  end
end
