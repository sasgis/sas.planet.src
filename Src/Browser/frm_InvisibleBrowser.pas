{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit frm_InvisibleBrowser;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  i_LanguageManager,
  i_InternalBrowserFactory,
  u_InternalBrowserImpl,
  u_InternalBrowserByImpl,
  u_CommonFormAndFrameParents;

type
  TfrmInvisibleBrowser = class(TFormWitghLanguageManager)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLock: IReadWriteSync;
    FBrowser: TInternalBrowserByImpl;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AInternalBrowserFactory: IInternalBrowserFactory
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
  const AInternalBrowserFactory: IInternalBrowserFactory
);
begin
  inherited Create(ALanguageManager);
  FLock := GSync.SyncBig.Make(Self.ClassName);
  FBrowser := TInternalBrowserByImpl.Create(Self, True, nil, nil, AInternalBrowserFactory);
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
  FLock.BeginWrite;
  try
    FBrowser.NavigateWait(AUrl, 10000);
  finally
    FLock.EndWrite;
  end;
end;

end.
