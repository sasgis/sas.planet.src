{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: GTLanguageList.PAS, released on 2003-05-13.

The Initial Developer of the Original Code is Olivier Sannier
[obones@altern.org]
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): none to date.

You may retrieve the latest version of this file at the Connection Manager
home page, located at http://cnxmanager.sourceforge.net

Known Issues: none to date.
-----------------------------------------------------------------------------}
// $Id$
unit GTLanguageList;

interface

uses Classes;

type
  TGTLanguageList = class
    protected
      fNames : TStringList;
      fCodes : TStringList;
      fInstalledOnly : boolean;
      procedure setInstalledOnly(nInstalledOnly : boolean);
      function  getName(const code : string) : string;
      function  getCode(const name : string) : string;
      procedure add(Name, code : string);
    public
      constructor Create; virtual;
      destructor Destroy; override;

      procedure LoadLanguages;

      property Code[const name : string] : string read getCode;
      property Codes : TStringList read fCodes;
      property InstalledOnly : boolean read FInstalledOnly write setInstalledOnly;
      property Name[const code : string] : string read getName;
      property Names : TStringList read fNames;

  end;

var LanguageList : TGTLanguageList;

implementation

uses gnugettext, Windows, SysUtils, Forms, GTLanguagesEx;

{ TGLLanguageList }

// this to prevent NameFromLocaleID to trigger a warning
// because it is platform specific
{$WARNINGS OFF}
procedure TGTLanguageList.LoadLanguages;
var
  installedLanguages : TStringList;
  lang : integer;
  englishFound : boolean;
  id : LCID;
begin
  fCodes.Clear;
  fNames.Clear;

  // if asked to display only installed languages
  if fInstalledOnly then
  begin
    // at first, english is not found
    englishFound := false;
    // then get the installed languages
    installedLanguages := TStringList.Create;

    // get languages as a list of codes
    DefaultInstance.GetListOfLanguages('default', installedLanguages);
    // add them into the list
    for lang := 0 to installedLanguages.Count - 1 do
    begin
      englishFound := englishFound or (installedLanguages[lang] = 'en');
      id := LanguagesEx.GNUGetTextID[installedLanguages[lang]];
      add(dgettext('languages', LanguagesEx.NameFromLocaleID[id]), installedLanguages[lang]);
    end;

    // and always add english, if not already there, this is the default
    if not englishFound then
    begin
      id := LanguagesEx.GNUGetTextID['en'];
      add(dgettext('languages', LanguagesEx.NameFromLocaleID[id]), 'en');
    end;

    installedLanguages.Free;
  end
  else
  begin
    // else, add a set in language code order
    add(dgettext('languages', 'Danish'),    'da');
    add(dgettext('languages', 'German'),    'de');
    add(dgettext('languages', 'English'),   'en');
    add(dgettext('languages', 'Spanish'),   'es');
    add(dgettext('languages', 'French'),    'fr');
    add(dgettext('languages', 'Italian'),   'it');
    add(dgettext('languages', 'Dutch'),     'nl');
    add(dgettext('languages', 'Norwegian'), 'no');
    add(dgettext('languages', 'Russian'),   'ru');
    add(dgettext('languages', 'Swedish'),   'sv');
  end;
end;
{$WARNINGS OFF}

constructor TGTLanguageList.Create;
begin
  inherited;
  fCodes := TStringList.Create;
  fNames := TStringList.Create;
end;

destructor TGTLanguageList.Destroy;
begin
  fNames.Free;
  fCodes.Free;
  inherited;
end;

procedure TGTLanguageList.add(Name, code: string);
begin
  fNames.Add(name);
  fCodes.Add(code);
end;

function TGTLanguageList.getCode(const name: string): string;
var nameIndex : integer;
begin
  nameIndex := fNames.IndexOf(name);
  if nameIndex > -1 then
  begin
    Result := fCodes[nameIndex];
  end
  else
  begin
    Result := '';
  end;
end;

function TGTLanguageList.getName(const code: string): string;
var codeIndex : integer;
begin
  // by default, result is empty
  Result := '';
  // try to find code in the list
  codeIndex := fCodes.IndexOf(code);
  if codeIndex > -1 then
  begin
    // if directly found return the associated name
    Result := fNames[codeIndex];
  end
  else
  begin
    // else, if length is longer than 2 (hence, includes country info)
    if length(code) > 2 then
    begin
      // then try to find the language code alone
      codeIndex := fCodes.IndexOf(Copy(code, 1, 2));
      if codeIndex > -1 then
      begin
        // if found, return it
        Result := fNames[codeIndex];
      end;
    end;
  end;
end;

procedure TGTLanguageList.setInstalledOnly(nInstalledOnly: boolean);
begin
  if fInstalledOnly <> nInstalledOnly then
  begin
    fInstalledOnly := nInstalledOnly;
    LoadLanguages;
  end;
end;

initialization
  // create variable
  LanguageList := TGTLanguageList.Create;

  // refresh content
  LanguageList.LoadLanguages;

finalization
  LanguageList.Free;

end.
