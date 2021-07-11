{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit fr_GeoCoderApiKey;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  i_LanguageManager,
  i_GeoCoderList,
  i_GeoCoderConfig,
  u_CommonFormAndFrameParents, ExtCtrls;

type
  TfrGeoCoderApiKey = class(TFrame)
    lblGeoCoderApiKey: TLabel;
    edtApiKey: TEdit;
    chkShowKey: TCheckBox;
    pnlKey: TPanel;
    procedure chkShowKeyClick(Sender: TObject);
  private
    FGeoCoderName: string;
    procedure UpdateLabelCaption;

    function GetValue: string;
    procedure SetValue(const AValue: string);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGeoCoderName: string
    ); reintroduce;

    procedure RefreshTranslation; override;

    property Value: string read GetValue write SetValue;
  end;

implementation

uses
  gnugettext;

resourcestring
  rsGeoCoderApiKeyCaptionFmt = '%s API Key:';

{$R *.dfm}

{ TfrGeoCoderApiKey }

constructor TfrGeoCoderApiKey.Create(
  const ALanguageManager: ILanguageManager;
  const AGeoCoderName: string
);
begin
  inherited Create(ALanguageManager);

  FGeoCoderName := AGeoCoderName;
  UpdateLabelCaption;
end;

function TfrGeoCoderApiKey.GetValue: string;
begin
  Result := edtApiKey.Text;
end;

procedure TfrGeoCoderApiKey.SetValue(const AValue: string);
begin
  edtApiKey.Text := AValue;
end;

procedure TfrGeoCoderApiKey.UpdateLabelCaption;
begin
  lblGeoCoderApiKey.Caption := Format(
    rsGeoCoderApiKeyCaptionFmt, [_(FGeoCoderName)]
  );
end;

procedure TfrGeoCoderApiKey.chkShowKeyClick(Sender: TObject);
begin
  if chkShowKey.Checked then begin
    edtApiKey.PasswordChar := #0;
  end else begin
    edtApiKey.PasswordChar := '*';
  end;
end;

procedure TfrGeoCoderApiKey.RefreshTranslation;
begin
  inherited RefreshTranslation;

  UpdateLabelCaption;
end;

end.
