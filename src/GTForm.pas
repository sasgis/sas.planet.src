{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: GTForm.PAS, released on 2003-05-13.

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
unit GTForm;

interface

uses gnugettext, Forms, Controls, Classes;

type
  TGTForm = class (TForm)
    protected
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      procedure RefreshTranslation; virtual;
  end;

  TGTFrame = class (TFrame)
    protected
    public
      constructor Create(AOwner : TComponent); override;
      procedure RefreshTranslation; virtual;
  end;


implementation

{ TGTForm }
uses ActnList, Graphics;

constructor TGTForm.Create(AOwner: TComponent);
begin
  inherited;

  TranslateComponent(self);
end;

destructor TGTForm.Destroy;
begin
  inherited;
end;

procedure TGTForm.RefreshTranslation;
begin
  ReTranslateComponent(self);
end;

{ TGTFrame }

constructor TGTFrame.Create(AOwner: TComponent);
begin
  inherited;
  if (Owner = Application) or (Owner = nil) then begin
    TranslateComponent(self);
  end;
end;

procedure TGTFrame.RefreshTranslation;
begin
  if (Owner = Application) or (Owner = nil) then begin
    ReTranslateComponent(self);
  end;
end;

initialization
  // Use delphi6rtl.mo for runtime library translations, if it is there
  AddDomainForResourceString('delphi6rtl');

  TP_GlobalIgnoreClassProperty(TAction,'Category');
  TP_GlobalIgnoreClassProperty(TControl,'HelpKeyword');
  TP_GlobalIgnoreClassProperty(TWinControl,'ImeName');
  TP_GlobalIgnoreClass(TFont);
finalization
end.
