{ Global compiler directives }
{$include bold.inc}
unit BoldModelSencha;

interface

uses
  BoldSystemRT,
  BoldElements;

type

  TBoldModelSencha = class(TObject)
    class function ClassToSenchaModel(aClass : TBoldClassTypeInfo;NameSpace: String;ProxyUrl: String) : String;
    class function BoldTypeToSencha(aBoldType : TBoldElementTypeInfo) : String;
    class function ModelName2ResourceName(aModelName: String) : String;
  end;

implementation

uses
  BoldUMLTypes;

{ TBoldModelSencha }

const
  PERSIST_FALSE = ',  persist: false';

class function TBoldModelSencha.BoldTypeToSencha(aBoldType: TBoldElementTypeInfo): String;
begin
  Result := '';
  if aBoldType.ExpressionName = 'String' then Result := 'string'
  else if aBoldType.ExpressionName = 'Boolean' then Result := 'boolean'
  else if aBoldType.ExpressionName = 'Float' then Result := 'float'
  else if aBoldType.ExpressionName = 'Integer' then Result := 'int'
  else if aBoldType.ExpressionName = 'DateTime' then Result := 'date'', dateFormat: ''Y-m-d H:i:s' ;
end;

class function TBoldModelSencha.ClassToSenchaModel(aClass: TBoldClassTypeInfo;NameSpace: String;ProxyUrl: String): String;
var
  I: Integer;
  vMember: TBoldMemberRTInfo;
  vMembersAdded : Integer;
  vSep: String;
  vPersist : String;
begin
  Result := 'Ext.define('''+ NameSpace + '.model.' + aClass.ExpressionName + ''', {' +  #13#10 +
    '  extend: ''Ext.data.Model'',' + #13#10 +
    '  fields: [' + #13#10 +
		'    {name: ''id'',       type: ''int''},' + #13#10 +
		'    {name: ''asString'', type: ''string'', persist:false},' +#13#10 ;

    vMembersAdded := 0 ;
    for I := 0 to aClass.AllMembersCount - 1 do
    begin
      vMember := aClass.AllMembers[I];
      vPersist := '';
      if vMember.Visibility = vkPublic then
      begin
        if vMember.IsDerived and not vMember.IsReverseDerived then
          vPersist := PERSIST_FALSE;

        if vMembersAdded > 0 then vSep := ',' +#13#10;
        if vMember.IsAttribute then
        begin
          if vMember.BoldType.ExpressionName = 'DateTime' then
          begin
            Result := Result + vSep + '    {name: ''' + vMember.ExpressionName + ''', type:''' + BoldTypeToSencha(vMember.BoldType) + ''''+ PERSIST_FALSE +'}';
            Result := Result + vSep + '    {name: ''' + vMember.ExpressionName + '_Date'', type:''date'', dateFormat: ''Y-m-d''' + vPersist +'}';
            Result := Result + vSep + '    {name: ''' + vMember.ExpressionName + '_Time'', type:''date'', dateFormat: ''H:i:s''' + vPersist +'}';
            vMembersAdded := vMembersAdded + 3;
          end
          else begin
            Result := Result + vSep + '    {name: ''' + vMember.ExpressionName + ''', type:''' + BoldTypeToSencha(vMember.BoldType) + ''''+ vPersist +'}';
            Inc(vMembersAdded);
          end;

        end;
        if vMember.IsSingleRole then
        begin
          if (vMember.Constraint[''] <> nil) then
            if (vMember.Constraint[''].ConstraintExpression = 'updateable') then
            vPersist := '';
          Result := Result + vSep + '    {name: ''' + vMember.ExpressionName + '_id'', type:''int''' + vPersist + '}';
          Result := Result + vSep + '    {name: ''' + vMember.ExpressionName + '_AsString'', type:''string''' + PERSIST_FALSE + '}';
          vMembersAdded := vMembersAdded + 2;
        end;
      end;
    end;

	//validations: [
	//	{type: ''presence'', name: ''code''},
	//	{type: ''length'',   name: ''code'', min: 2, max: 2},
	//	{type: ''presence'', name: ''name''}
	//],
  Result := Result + #13#10 + '  ],' +#13#10 + #13#10 +


	'  proxy: {' + #13#10 +
  '    type:   ''rest'',' + #13#10 +
  '    url:    '''+ ProxyUrl+ '/' + ModelName2ResourceName(aClass.ExpressionName) + ''',' + #13#10 +
  '    reader: { ' + #13#10 +
  '	     root: ''data'',' + #13#10 +
  '      messageProperty: ''errorMessage''' + #13#10 +
  '    },' + #13#10 +
  '    groupParam: '''',' + #13#10 +
  '    limitParam: '''','+ #13#10 +
  '    pageParam: '''',' +#13#10 +
  '    sortParam : '''',' + #13#10 +
  '    startParam: '''',' +  #13#10 +
  '    writer: { ' + #13#10 +
  '	     writeAllFields: false' + #13#10 +
  '    }, ' + #13#10 + #13#10 +
	'    listeners: { ' + #13#10 +
	'      exception: function(proxy, response, operation) { ' + #13#10 +
	'        Ext.MessageBox.show({ ' + #13#10 +
	'           title:   ''Remote Exception'', ' + #13#10 +
	'           msg:     response.responseText,  ' + #13#10 +
	'           icon:    Ext.MessageBox.ERROR,' + #13#10 +
	'           buttons: Ext.Msg.OK' + #13#10 +
	'        });' + #13#10 +
	'        LOG.console(''proxy/response/operation'');' + #13#10 +
	'        LOG.console(proxy);' + #13#10 +
	'        LOG.console(response);' + #13#10 +
	'        LOG.console(operation);' + #13#10 +
	'      }' + #13#10 +
	'    }' + #13#10 +
	'  }' + #13#10 +
  '});';

end;

class function TBoldModelSencha.ModelName2ResourceName(aModelName: String): String;
var
  vAnEnding : String;
begin
  vAnEnding := '';
  if Length(aModelName) > 1 then vAnEnding := Copy(aModelName,Length(aModelName),1);
  if vAnEnding = 'y' then
    Result := Copy(aModelName,1, Length(aModelName)-1) + 'ies'
  else if vAnEnding = 's' then
    Result := aModelName + 'es'
  else
    Result := aModelName + 's'
end;

end.
