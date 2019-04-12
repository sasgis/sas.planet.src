{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit frm_InvisibleBrowser;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  OleCtrls,
  i_LanguageManager,
  i_InetConfig,
  u_InternalBrowserImplByIE,
  u_CommonFormAndFrameParents;

type
  TfrmInvisibleBrowser = class(TFormWitghLanguageManager)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCS: IReadWriteSync;
    FBrowser: TInternalBrowserImplByIE;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AInetConfig: IInetConfig
    ); reintroduce;
    procedure NavigateAndWait(const AUrl: string);
  end;

implementation

uses
  u_Synchronizer;

{$R *.dfm}

{ TfrmInvisibleBrowser }

constructor TfrmInvisibleBrowser.Create(
  const ALanguageManager: ILanguageManager;
  const AInetConfig: IInetConfig
);
begin
  inherited Create(ALanguageManager);
  FCS := GSync.SyncBig.Make(Self.ClassName);

  FBrowser :=
    TInternalBrowserImplByIE.Create(
      Self,
      True,
      AInetConfig.ProxyConfig,
      nil,
      AInetConfig.UserAgentString
    );
end;

procedure TfrmInvisibleBrowser.FormCreate(Sender: TObject);
begin
  FBrowser.AssignEmptyDocument;
end;

procedure TfrmInvisibleBrowser.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBrowser);
end;

procedure TfrmInvisibleBrowser.NavigateAndWait(const AUrl: string);
begin
  FCS.BeginWrite;
  try
    FBrowser.NavigateWait(AUrl, 10000);
  finally
    FCS.EndWrite;
  end;
end;

end.
