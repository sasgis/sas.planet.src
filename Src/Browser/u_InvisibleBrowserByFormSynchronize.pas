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

unit u_InvisibleBrowserByFormSynchronize;

interface

uses
  SysUtils,
  i_InvisibleBrowser,
  i_LanguageManager,
  i_InternalBrowserFactory,
  u_BaseInterfacedObject,
  frm_InvisibleBrowser;

type
  TInvisibleBrowserByFormSynchronize = class(TBaseInterfacedObject, IInvisibleBrowser)
  private
    FCS: IReadWriteSync;
    FInternalBrowserFactory: IInternalBrowserFactory;
    FLanguageManager: ILanguageManager;
    FfrmInvisibleBrowser: TfrmInvisibleBrowser;
  private
    procedure NavigateAndWait(const AUrl: string);
    procedure InternalCreateBrowser;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AInternalBrowserFactory: IInternalBrowserFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  u_Synchronizer;

type
  TSyncNavigate = class
  private
    FUrl: string;
    FfrmInvisibleBrowser: TfrmInvisibleBrowser;
    procedure SyncNavigate;
  public
    constructor Create(
      const AUrl: string;
      AfrmInvisibleBrowser: TfrmInvisibleBrowser
    );
    procedure Navigate;
  end;

{ TSyncNavigate }

constructor TSyncNavigate.Create(
  const AUrl: string;
  AfrmInvisibleBrowser: TfrmInvisibleBrowser
);
begin
  inherited Create;
  FUrl := AUrl;
  FfrmInvisibleBrowser := AfrmInvisibleBrowser;
end;

procedure TSyncNavigate.Navigate;
begin
  TThread.Synchronize(nil, SyncNavigate);
end;

procedure TSyncNavigate.SyncNavigate;
begin
  FfrmInvisibleBrowser.NavigateAndWait(FUrl);
end;

{ TInvisibleBrowserByFormSynchronize }

constructor TInvisibleBrowserByFormSynchronize.Create(
  const ALanguageManager: ILanguageManager;
  const AInternalBrowserFactory: IInternalBrowserFactory
);
begin
  inherited Create;
  FCS := GSync.SyncVariable.Make(Self.ClassName);
  FInternalBrowserFactory := AInternalBrowserFactory;
  FLanguageManager := ALanguageManager;
end;

destructor TInvisibleBrowserByFormSynchronize.Destroy;
begin
  FreeAndNil(FfrmInvisibleBrowser);
  FCS := nil;
  inherited;
end;

procedure TInvisibleBrowserByFormSynchronize.InternalCreateBrowser;
begin
  FfrmInvisibleBrowser := TfrmInvisibleBrowser.Create(FLanguageManager, FInternalBrowserFactory);
end;

procedure TInvisibleBrowserByFormSynchronize.NavigateAndWait(const AUrl: string);
var
  VSyncNav: TSyncNavigate;
begin
  FCS.BeginWrite;
  try
    if FfrmInvisibleBrowser = nil then begin
      TThread.Synchronize(nil, InternalCreateBrowser);
    end;
  finally
    FCS.EndWrite;
  end;

  VSyncNav := TSyncNavigate.Create(AUrl, FfrmInvisibleBrowser);
  try
    VSyncNav.Navigate;
  finally
    VSyncNav.Free;
  end;
end;

end.
