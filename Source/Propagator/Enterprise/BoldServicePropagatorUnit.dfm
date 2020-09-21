object BoldPropagatorSvc: TBoldPropagatorSvc
  OldCreateOrder = False
  AllowPause = False
  Dependencies = <
    item
      IsGroup = False
    end>
  DisplayName = 'BoldPropagatorSvc'
  Interactive = True
  StartType = stManual
  AfterInstall = ServiceAfterInstall
  AfterUninstall = ServiceAfterUninstall
  OnStart = ServiceStart
  Left = 121
  Top = 218
  Height = 479
  Width = 741
end
