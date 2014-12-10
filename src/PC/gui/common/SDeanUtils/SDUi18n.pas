unit SDUi18n;
 // Description: Internationalization (i18n) Functions
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //

 // This unit implements i18n related code.
 // For now, it largely wraps the dxGetText library atm, but can be used for
 // projects built without dxGetText (in which case, this unit does
 // nothing - for now)

 // Define "_DXGETTEXT" to use dxGetText for translation
 // Leave this undefined for default behaviour


interface

uses
  Classes, ComCtrls, StdCtrls;

const
  ISO639_ALPHA2_ENGLISH = 'en';

//{$IFNDEF _DXGETTEXT}
function _(msg: unicodestring): unicodestring;
//{$ENDIF}
function SDUTranslate(msg: unicodestring): unicodestring;
procedure SDUTranslateComp(var c: TRichEdit); OVERLOAD;
procedure SDUTranslateComp(var c: TLabel); OVERLOAD;
function SDUPluralMsg(n: Integer; singleMsg: WideString; pluralMsg: WideString): WideString;
  OVERLOAD;
function SDUPluralMsg(n: Integer; msgs: array of WideString): WideString; OVERLOAD;
procedure SDUSetLanguage(lang: String);
procedure SDUTranslateComponent(Comp: TComponent);
procedure SDURetranslateComponent(Comp: TComponent);
procedure SDUGetLanguageCodes(langCodes: TStringList);
function SDUGetCurrentLanguageCode(): String;
function SDUGetTranslatorName(): WideString;
function SDUGetTranslatorNameAndEmail(): WideString;

procedure SDUTP_GlobalIgnoreClass(IgnClass: TClass);
procedure SDUTP_GlobalIgnoreClassProperty(IgnClass: TClass; const propertyname: String);

 // Returns TRUE/FALSE if English
 // (e.g. US English, British English)
 // WARNING: If '' supplied, this will return FALSE
function SDUIsLanguageCodeEnglish(code: String): Boolean;

implementation

uses
{$IFDEF _DXGETTEXT}
  gnugettext,
{$ENDIF}
  ActnList, Controls, ExtCtrls, Graphics, SysUtils;

function _(msg: unicodestring): unicodestring;
begin
  Result := SDUTranslate(msg);
end;

function SDUTranslate(msg: unicodestring): unicodestring;
begin
{$IFDEF _DXGETTEXT}
  Result := gnugettext._(msg);
{$ELSE}
  Result := msg;
{$ENDIF}
end;

procedure SDUTranslateComp(var c: TRichEdit); OVERLOAD;
begin
  //tag is used as flag if translated already
  if c.tag = 0 then
    c.Text := SDUTranslate(c.Text);
  c.tag := 1;
end;

procedure SDUTranslateComp(var c: TLabel); OVERLOAD;
begin
  //tag is used as flag if translated already
  if c.tag = 0 then
    c.Caption := SDUTranslate(c.Caption);
  c.tag := 1;
end;

function SDUPluralMsg(n: Integer; singleMsg: WideString; pluralMsg: WideString): WideString;
begin
  Result := SDUPluralMsg(n, [singleMsg, pluralMsg]);
end;

// Note: n must be 1 or greater
function SDUPluralMsg(n: Integer; msgs: array of WideString): WideString; OVERLOAD;
var
  retval: WideString;
begin
  retval := '';

  if (length(msgs) > 0) then begin
    if (n > length(msgs)) then begin
      n := length(msgs);
    end else
    if (n < 1) then begin
      n := 1;
    end;

    // -1 as array indexes from zero
    retval := msgs[n - 1];
  end;

  Result := retval;
end;

procedure SDUTranslateComponent(Comp: TComponent);
begin
{$IFDEF _DXGETTEXT}
  TranslateComponent(comp);
{$ENDIF}
end;

procedure SDURetranslateComponent(Comp: TComponent);
begin
{$IFDEF _DXGETTEXT}
  RetranslateComponent(comp);
{$ENDIF}
end;

procedure SDUSetLanguage(lang: String);
begin
{$IFDEF _DXGETTEXT}
  UseLanguage(lang);
{$ENDIF}
end;

procedure SDUTP_GlobalIgnoreClass(IgnClass: TClass);
begin
{$IFDEF _DXGETTEXT}
  TP_GlobalIgnoreClass(IgnClass);
{$ENDIF}
end;

procedure SDUTP_GlobalIgnoreClassProperty(IgnClass: TClass; const propertyname: String);
begin
{$IFDEF _DXGETTEXT}
  TP_GlobalIgnoreClassProperty(IgnClass, propertyname);
{$ENDIF}
end;

procedure SDUGetLanguageCodes(langCodes: TStringList);
begin
{$IFDEF _DXGETTEXT}
  DefaultInstance.GetListOfLanguages('default', langCodes);
{$ENDIF}
end;

function SDUGetCurrentLanguageCode(): String;
begin
{$IFDEF _DXGETTEXT}
  Result := GetCurrentLanguage();
{$ELSE}
  Result := '';
{$ENDIF}

end;

function SDUGetTranslatorName(): WideString;
var
  retval: WideString;
begin
  retval := SDUGetTranslatorNameAndEmail();

  //aaa := 'aa <b@c.d>';
  //aaa := '<b@c.d> aa';
  //aaa := 'aaa';
  //aaa := '<b@c.d>';
  //aaa := 'b@c.d';

  if ((Pos('<', retval) > 0) and (Pos('@', retval) > Pos('<', retval)) and
    // Sanity, in case of "<Berty>" - not an email addr
    (Pos('>', retval) > Pos('@', retval))     // Sanity, in case of "<Berty>" - not an email addr
    ) then begin
    // Trivial version; only handles stuff like "Fred <bert@domain.com>"
    // Really should be able to handle "<ME!> <myaddr@domain.com"
    retval := copy(retval, 1, (Pos('<', retval) - 1));
    retval := Trim(retval);
  end;

  Result := retval;
end;

function SDUGetTranslatorNameAndEmail(): WideString;
begin
{$IFDEF _DXGETTEXT}
  Result := GetTranslatorNameAndEmail();
{$ELSE}
  Result := '';
{$ENDIF}

end;

function SDUIsLanguageCodeEnglish(code: String): Boolean;
begin
  Result := (Pos(ISO639_ALPHA2_ENGLISH, code) = 1);
end;

initialization
  // This is the list of ignores. The list of ignores has to come before the
  // first call to TranslateComponent().
  // Note: Many of these are commented out; including them all would require
  //       the "uses" clause to include all the units that define the classes
  //       listed
  // VCL, important ones
  SDUTP_GlobalIgnoreClassProperty(TAction, 'Category');
  SDUTP_GlobalIgnoreClassProperty(TControl, 'HelpKeyword');
  SDUTP_GlobalIgnoreClassProperty(TNotebook, 'Pages');
  // VCL, not so important
  SDUTP_GlobalIgnoreClassProperty(TControl, 'ImeName');
  SDUTP_GlobalIgnoreClass(TFont);
  // Database (DB unit)
  //  SDUTP_GlobalIgnoreClassProperty(TField, 'DefaultExpression');
  //  SDUTP_GlobalIgnoreClassProperty(TField, 'FieldName');
  //  SDUTP_GlobalIgnoreClassProperty(TField, 'KeyFields');
  //  SDUTP_GlobalIgnoreClassProperty(TField, 'DisplayName');
  //  SDUTP_GlobalIgnoreClassProperty(TField, 'LookupKeyFields');
  //  SDUTP_GlobalIgnoreClassProperty(TField, 'LookupResultField');
  //  SDUTP_GlobalIgnoreClassProperty(TField, 'Origin');
  //  SDUTP_GlobalIgnoreClass(TParam);
  //  SDUTP_GlobalIgnoreClassProperty(TFieldDef, 'Name');
  // MIDAS/Datasnap
  //  SDUTP_GlobalIgnoreClassProperty(TClientDataset, 'CommandText');
  //  SDUTP_GlobalIgnoreClassProperty(TClientDataset, 'Filename');
  //  SDUTP_GlobalIgnoreClassProperty(TClientDataset, 'Filter');
  //  SDUTP_GlobalIgnoreClassProperty(TClientDataset, 'IndexFieldnames');
  //  SDUTP_GlobalIgnoreClassProperty(TClientDataset, 'IndexName');
  //  SDUTP_GlobalIgnoreClassProperty(TClientDataset, 'MasterFields');
  //  SDUTP_GlobalIgnoreClassProperty(TClientDataset, 'Params');
  //  SDUTP_GlobalIgnoreClassProperty(TClientDataset, 'ProviderName');
  // Database controls
  //  SDUTP_GlobalIgnoreClassProperty(TDBComboBox, 'DataField');
  //  SDUTP_GlobalIgnoreClassProperty(TDBCheckBox, 'DataField');
  //  SDUTP_GlobalIgnoreClassProperty(TDBEdit, 'DataField');
  //  SDUTP_GlobalIgnoreClassProperty(TDBImage, 'DataField');
  //  SDUTP_GlobalIgnoreClassProperty(TDBListBox, 'DataField');
  //  SDUTP_GlobalIgnoreClassProperty(TDBLookupControl, 'DataField');
  //  SDUTP_GlobalIgnoreClassProperty(TDBLookupControl, 'KeyField');
  //  SDUTP_GlobalIgnoreClassProperty(TDBLookupControl, 'ListField');
  //  SDUTP_GlobalIgnoreClassProperty(TDBMemo, 'DataField');
  //  SDUTP_GlobalIgnoreClassProperty(TDBRadioGroup, 'DataField');
  //  SDUTP_GlobalIgnoreClassProperty(TDBRichEdit, 'DataField');
  //  SDUTP_GlobalIgnoreClassProperty(TDBText, 'DataField');
  // Interbase Express (IBX)
  //  SDUTP_GlobalIgnoreClass(TIBDatabase);
  //  SDUTP_GlobalIgnoreClass(TIBDatabase);
  //  SDUTP_GlobalIgnoreClass(TIBTransaction);
  //  SDUTP_GlobalIgnoreClassProperty(TIBSQL, 'UniqueRelationName');
  // Borland Database Engine (BDE)
  //  SDUTP_GlobalIgnoreClass(TSession);
  //  SDUTP_GlobalIgnoreClass(TDatabase);
  // ADO components
  //  SDUTP_GlobalIgnoreClass (TADOConnection);
  //  SDUTP_GlobalIgnoreClassProperty(TADOQuery, 'CommandText');
  //  SDUTP_GlobalIgnoreClassProperty(TADOQuery, 'ConnectionString');
  //  SDUTP_GlobalIgnoreClassProperty(TADOQuery, 'DatasetField');
  //  SDUTP_GlobalIgnoreClassProperty(TADOQuery, 'Filter');
  //  SDUTP_GlobalIgnoreClassProperty(TADOQuery, 'IndexFieldNames');
  //  SDUTP_GlobalIgnoreClassProperty(TADOQuery, 'IndexName');
  //  SDUTP_GlobalIgnoreClassProperty(TADOQuery, 'MasterFields');
  //  SDUTP_GlobalIgnoreClassProperty(TADOTable, 'IndexFieldNames');
  //  SDUTP_GlobalIgnoreClassProperty(TADOTable, 'IndexName');
  //  SDUTP_GlobalIgnoreClassProperty(TADOTable, 'MasterFields');
  //  SDUTP_GlobalIgnoreClassProperty(TADOTable, 'TableName');
  //  SDUTP_GlobalIgnoreClassProperty(TADODataset, 'CommandText');
  //  SDUTP_GlobalIgnoreClassProperty(TADODataset, 'ConnectionString');
  //  SDUTP_GlobalIgnoreClassProperty(TADODataset, 'DatasetField');
  //  SDUTP_GlobalIgnoreClassProperty(TADODataset, 'Filter');
  //  SDUTP_GlobalIgnoreClassProperty(TADODataset, 'IndexFieldNames');
  //  SDUTP_GlobalIgnoreClassProperty(TADODataset, 'IndexName');
  //  SDUTP_GlobalIgnoreClassProperty(TADODataset, 'MasterFields');
  // ActiveX stuff
  //  SDUTP_GlobalIgnoreClass(TWebBrowser);

end.
