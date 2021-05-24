unit cxBoldEditConsts;

interface

//  v2.03 - 25 Jan 2011  2007-2011 Daniel Mauric

resourcestring
  scxSBoldEditRepositoryTextItem         = 'BoldTextEdit|Represents a Bold aware single line text editor';
  scxSBoldComboBoxRepositoryTextItem     = 'BoldComboBox|Represents a Bold aware combo box editor';
  scxSBoldLookupComboBoxRepositoryTextItem     = 'BoldLookupComboBox|Represents a Bold aware lookup combo box editor';
  scxSBoldExtLookupComboBoxRepositoryTextItem     = 'BoldExtLookupComboBox|Represents a Bold aware lookup combo using grid as its drop down control';

implementation                                     

uses
  dxCore;

procedure AddEditorsResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAdress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAdress);
  end;

begin
  InternalAdd('scxSBoldEditRepositoryTextItem', @scxSBoldEditRepositoryTextItem);
  InternalAdd('scxSBoldComboBoxRepositoryTextItem', @scxSBoldComboBoxRepositoryTextItem);
  InternalAdd('scxSBoldLookupComboBoxRepositoryTextItem', @scxSBoldLookupComboBoxRepositoryTextItem);
  InternalAdd('scxSBoldExtLookupComboBoxRepositoryTextItem', @scxSBoldExtLookupComboBoxRepositoryTextItem);
end;


initialization
  dxResourceStringsRepository.RegisterProduct('Bold ExpressEditors Library', @AddEditorsResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('Bold ExpressEditors Library');

end.
