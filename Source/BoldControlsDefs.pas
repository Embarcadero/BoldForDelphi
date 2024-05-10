
{ Global compiler directives }
{$include bold.inc}
unit BoldControlsDefs;

interface

uses
  TypInfo,
  Grids,
  Types,
  Graphics;

type
  TBoldColumnValue = (
    cvColor, cvWidth, cvFont, cvAlignment, cvReadOnly, cvTitleColor,
    cvTitleCaption, cvTitleAlignment, cvTitleFont, cvImeMode, cvImeName);
  TBoldColumnValues = set of TBoldColumnValue;
  TBoldCWAdjust = (caAllowGrow, caAllowShrink, caIncludeTitle);
  TBoldCWAdjustSet = set of TBoldCWAdjust;

  TBoldDrawCellEvent = procedure (Sender: TObject; Canvas: TCanvas; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState) of object;

  TBoldStretchMode = (bsmNoStretch, bsmStretchProportional, bsmStretchToFit, bsmStretchToScale);

  TBoldNodeExpansionMethod = (neAll, neVisible, neDemand);
  TBoldMouseDirection = (DirMouseUp, DirMouseDown);

  TBoldComboSelectChangeAction = (bdscSetText, bdcsSetValue, bdcsNone, bdcsSetReference, bdcsSetListIndex);

  TBoldEditButtonStyle = (bbsNone, bbsCombo, bbsEllipsis, bbsCustom);
  
const
  BoldPropertiesController_SupportedPropertyTypes =
    [{tkUnknown,} tkInteger, tkChar, tkEnumeration, tkFloat,
     tkString, tkSet, tkClass, {tkMethod,} tkWChar, tkLString, tkWString,
     tkVariant{, tkArray, tkRecord, tkInterface}, tkInt64{, tkDynArray}];

implementation


end.
