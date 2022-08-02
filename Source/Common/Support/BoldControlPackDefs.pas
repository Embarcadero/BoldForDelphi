
{ Global compiler directives }
{$include bold.inc}
unit BoldControlPackDefs;

interface

type
  TBoldApplyPolicy = (bapChange, bapExit, bapDemand);
  TBoldDragMode = (bdgNone, bdgSelection);
  TBoldDropMode = (bdpNone, bdpInsert, bdpAppend, bdpReplace);
  TBoldPopupDeleteType = (bpdNone, bdpModel, bpdDelete, bdpUnlink);

  TBoldFollowerState = (bfsEmpty, bfsCurrent, bfsDirty,
    bfsValueOutOfDate, bfsSubscriptionOutOfDate,
    bfsInactiveValidElement, bfsInactiveInvalidElement, bfsActivating);

  TBoldNilElementMode = (neNone, neInsertFirst, neAddLast);

const
  bfsDisplayable = [bfsCurrent, bfsDirty];
  bfsOutOfDate = [bfsValueOutOfDate, bfsSubscriptionOutOfDate];
  bfsActive = [bfsEmpty, bfsCurrent, bfsDirty,
  bfsValueOutOfDate, bfsSubscriptionOutOfDate];
  bfdNeedResubscribe = [bfsSubscriptionOutOfDate, bfsActivating];

  BoldNodeListIndex = 0;
  BoldNodeIconIndex = 1;
  BoldNodeTextIndex = 2;
  BoldNodeFirstColumnIndex = 3;

  breHandleNil = 42;
  breValueIdentityChanged = 43;
  breListIdentityChanged = 44;
  breHandleIndexChanged = 45;

  beListPartEnabledChanged = 100;

  DefaultBoldDragMode = bdgSelection;
  DefaultBoldDropMode = bdpAppend;

implementation


end.
