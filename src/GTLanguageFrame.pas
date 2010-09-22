{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: GTLanguageFrame.PAS, released on 2003-05-13.

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
unit GTLanguageFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TOnGTLanguageChanged = procedure (Sender : TObject; Code : string) of object;
  TOnGTBeforeLanguageChange = procedure (Sender : TObject; Code : string) of object;

  TGTfraLanguage = class(TFrame)
    cmbLanguage: TComboBox;
    lblLanguage: TLabel;
    procedure cmbLanguageSelect(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    function getInstalledOnly: boolean;
    procedure setInstalledOnly(const Value: boolean);
    { Private declarations }
  protected
    eOnGTLanguageChanged : TOnGTLanguageChanged;
    eOnGTBeforeLanguageChange : TOnGTBeforeLanguageChange;

    procedure populateComboBox;
    function  getName : string; virtual;
    procedure setGTName(nName : string); virtual;
    function  getCode : string; virtual;
    procedure setCode(nCode : string); virtual;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    property Name : string read getName write setGTName;
    property Code : string read getCode write setCode;
  published
    property OnGTLanguageChanged : TOnGTLanguageChanged read eOnGTLanguageChanged write eOnGTLanguageChanged;
    property OnGTBeforeLanguageChange : TOnGTBeforeLanguageChange read eOnGTBeforeLanguageChange write eOnGTBeforeLanguageChange;
    property InstalledOnly : boolean read getInstalledOnly write setInstalledOnly;
  end;

implementation

{$R *.dfm}

uses gnugettext, GTLanguageList, GTForm;

procedure TGTfraLanguage.cmbLanguageSelect(Sender: TObject);
var currCompo : integer;
begin
  // inform user if needed
  if assigned(eOnGTBeforeLanguageChange) then
  begin
    eOnGTBeforeLanguageChange(Self, GetCurrentLanguage);
  end;

  // change language
  UseLanguage(LanguageList.Code[cmbLanguage.Items[cmbLanguage.ItemIndex]]);

  // force reloading forms with new selection
  for currCompo := 0 to application.ComponentCount - 1 do
  begin
    if application.Components[currCompo] is TGTForm then begin
      TGTForm(application.Components[currCompo]).RefreshTranslation;
    end else if application.Components[currCompo] is TGTFrame then begin
      TGTFrame(application.Components[currCompo]).RefreshTranslation;
    end;
  end;

  // force reloading language list with new languages
  LanguageList.LoadLanguages;
  populateComboBox;
  cmbLanguage.ItemIndex := cmbLanguage.Items.IndexOf(LanguageList.Name[GetCurrentLanguage]);

  // inform user if needed
  if assigned(eOnGTLanguageChanged) then
  begin
    eOnGTLanguageChanged(Self, GetCurrentLanguage);
  end;
end;

constructor TGTfraLanguage.Create(AOwner: TComponent);
begin
  inherited;
  LanguageList.LoadLanguages;
  populateComboBox;
end;

function TGTfraLanguage.getCode: string;
begin
  if cmbLanguage.ItemIndex > -1 then
  begin
    Result := LanguageList.Code[cmbLanguage.Items[cmbLanguage.ItemIndex]];
  end
  else
  begin
    Result := '';
  end;
end;

procedure TGTfraLanguage.setCode(nCode: string);
var listCodeIndex : integer;
begin
  // tries to find the indicated code in the language list
  listCodeIndex := LanguageList.Codes.IndexOf(nCode);
  if listCodeIndex <> -1 then
  begin
    // if found, select it in the combo box
    cmbLanguage.ItemIndex := cmbLanguage.Items.IndexOf(LanguageList.Name[nCode]);
  end
  else
  begin
    // else, select current language
    cmbLanguage.ItemIndex := cmbLanguage.Items.IndexOf(LanguageList.Name[GetCurrentLanguage]);
  end;
  // in any case, force selection of new language
  cmbLanguageSelect(self);
end;

function TGTfraLanguage.getName: string;
begin
  if cmbLanguage.ItemIndex > -1 then
  begin
    Result := cmbLanguage.Items[cmbLanguage.ItemIndex];
  end
  else
  begin
    Result := '';
  end;
end;

procedure TGTfraLanguage.setGTName(nName: string);
var listNameIndex : integer;
begin
  // tries to find the indicated code in the language list
  listNameIndex := LanguageList.Names.IndexOf(nName);
  if listNameIndex <> -1 then
  begin
    // if found, select it
    cmbLanguage.ItemIndex := cmbLanguage.Items.IndexOf(nName);
  end
  else
  begin
    // else, select current language
    cmbLanguage.ItemIndex := cmbLanguage.Items.IndexOf(LanguageList.Name[GetCurrentLanguage]);
  end;
  // in any case, force selection of new language
  cmbLanguageSelect(self);
end;

procedure TGTfraLanguage.FrameResize(Sender: TObject);
begin
  cmbLanguage.ItemIndex := cmbLanguage.Items.IndexOf(LanguageList.Name[GetCurrentLanguage]);
end;

procedure TGTfraLanguage.populateComboBox;
begin
  // load list of languages
  cmbLanguage.Items.Assign(LanguageList.Names);
end;

function TGTfraLanguage.getInstalledOnly: boolean;
begin
  Result := LanguageList.InstalledOnly;
end;

procedure TGTfraLanguage.setInstalledOnly(const Value: boolean);
begin
  LanguageList.InstalledOnly := Value;
  populateComboBox;
  cmbLanguage.ItemIndex := cmbLanguage.Items.IndexOf(LanguageList.Name[GetCurrentLanguage]);
end;

end.
