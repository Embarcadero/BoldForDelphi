object frmBoldDerivationExpressionsEditorCx: TfrmBoldDerivationExpressionsEditorCx
  Left = 0
  Top = 0
  ActiveControl = cxGridDerivationExpressions
  BorderStyle = bsDialog
  Caption = 'Derivation Expression Editor'
  ClientHeight = 357
  ClientWidth = 701
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 319
    Width = 701
    Height = 38
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      701
      38)
    object CancelBtn: TButton
      Left = 619
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object OKBtn: TButton
      Left = 538
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 701
    Height = 319
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Derivation Expressions'
      object ToolBar1: TToolBar
        Left = 0
        Top = 0
        Width = 693
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
      object cxGridDerivationExpressions: TcxGrid
        Left = 0
        Top = 30
        Width = 693
        Height = 261
        Align = alClient
        TabOrder = 1
        object tvDerivationExpressions: TcxGridBoldTableView
          Navigator.Buttons.CustomButtons = <>
          ScrollbarAnnotations.CustomAnnotations = <>
          DataController.BoldHandle = bchTaggedValue
          DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoImmediatePost]
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          OptionsCustomize.ColumnFiltering = False
          OptionsCustomize.ColumnGrouping = False
          OptionsCustomize.ColumnMoving = False
          OptionsCustomize.ColumnSorting = False
          OptionsView.CellAutoHeight = True
          OptionsView.ColumnAutoWidth = True
          OptionsView.GroupByBox = False
          object colElement: TcxGridBoldColumn
            Caption = 'Element'
            DataBinding.BoldProperties.Expression = ''
            DataBinding.BoldProperties.Renderer = rElement
            Width = 153
          end
          object colExpression: TcxGridBoldColumn
            Caption = 'Expression'
            DataBinding.BoldProperties.Expression = ''
            DataBinding.BoldProperties.Renderer = rExpression
            PropertiesClassName = 'TcxButtonEditProperties'
            Properties.Buttons = <
              item
                Default = True
                Kind = bkEllipsis
              end>
            Properties.OnButtonClick = tvDerivationExpressionsColumn2PropertiesButtonClick
            Width = 538
          end
        end
        object lvlDerivationExpressions: TcxGridLevel
          GridView = tvDerivationExpressions
        end
      end
    end
  end
  object brhTaggedValue: TBoldReferenceHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    StaticValueTypeName = 'UMLTaggedValue'
    Left = 40
    Top = 80
  end
  object bdhTaggedValue: TBoldDerivedHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = brhTaggedValue
    OnDeriveAndSubscribe = bdhTaggedValueDeriveAndSubscribe
    Left = 40
    Top = 128
  end
  object bchTaggedValue: TBoldCursorHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = bdhTaggedValue
    Left = 40
    Top = 176
  end
  object rElement: TBoldAsVariantRenderer
    OnGetAsVariant = rElementGetAsVariant
    Left = 128
    Top = 80
  end
  object rExpression: TBoldAsVariantRenderer
    OnSubscribe = rExpressionSubscribe
    OnGetAsVariant = rExpressionGetAsVariant
    Left = 128
    Top = 128
  end
  object cxImageList1: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 4981012
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
    Left = 209
    Top = 76
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
      OnUpdate = actMoveUpUpdate
    end
    object actMoveDown: TAction
      Caption = 'Move down'
      ImageIndex = 2
      OnExecute = actMoveDownExecute
      OnUpdate = actMoveDownUpdate
    end
  end
  object bchElements: TBoldCursorHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = bdhElements
    Left = 380
    Top = 136
  end
  object bdhElements: TBoldDerivedHandle
    StaticSystemHandle = dmModelEdit.bshUMLModel
    RootHandle = brhTaggedValue
    OnDeriveAndSubscribe = bdhElementsDeriveAndSubscribe
    ValueTypeName = 'Collection(UMLFeature)'
    Left = 388
    Top = 80
  end
end