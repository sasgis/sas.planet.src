{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_TileDownloadSubsystemState;

interface

uses
  i_TileDownloaderState,
  i_MapAbilitiesConfig,
  u_ConfigDataElementComplexBase;

type
  TTileDownloadSubsystemState = class(TConfigDataElementComplexWithStaticBase, ITileDownloaderStateChangeble)
  private
    FZmpDownloadEnabled: Boolean;
    FSaverState: ITileDownloaderStateChangeble;
    FRequestBuilderState: ITileDownloaderStateChangeble;
    FMapAbilitiesConfig: IMapAbilitiesConfig;
  protected
    function CreateStatic: IInterface; override;
  private
    function GetStatic: ITileDownloaderStateStatic;
  public
    constructor Create(
      AZmpDownloadEnabled: Boolean;
      const ARequestBuilderState: ITileDownloaderStateChangeble;
      const ASaverState: ITileDownloaderStateChangeble;
      const AMapAbilitiesConfig: IMapAbilitiesConfig
    );
  end;

implementation

uses
  gnugettext,
  u_TileDownloaderStateStatic;

{ TTileDownloadSubsystemState }

constructor TTileDownloadSubsystemState.Create(
  AZmpDownloadEnabled: Boolean;
  const ARequestBuilderState: ITileDownloaderStateChangeble;
  const ASaverState: ITileDownloaderStateChangeble;
  const AMapAbilitiesConfig: IMapAbilitiesConfig
);
begin
  inherited Create;
  FZmpDownloadEnabled := AZmpDownloadEnabled;
  if FZmpDownloadEnabled then begin
    FSaverState := ASaverState;
    Add(FSaverState);

    FRequestBuilderState := ARequestBuilderState;
    Add(FRequestBuilderState);

    FMapAbilitiesConfig := AMapAbilitiesConfig;
    Add(FMapAbilitiesConfig);
  end;
end;

function TTileDownloadSubsystemState.CreateStatic: IInterface;
var
  VStatic: ITileDownloaderStateStatic;
  VState: ITileDownloaderStateStatic;
begin
  VStatic := ITileDownloaderStateStatic(GetStaticInternal);
  if not FZmpDownloadEnabled then begin
    if VStatic = nil then begin
      VStatic := TTileDownloaderStateStatic.Create(False, gettext_NoOp('Disabled by Zmp'));
    end;
  end else begin
    if not FMapAbilitiesConfig.UseDownload then begin
      VStatic := TTileDownloaderStateStatic.Create(False, gettext_NoOp('Disabled by map params'));
    end else begin
      VState := FRequestBuilderState.GetStatic;
      if not VState.Enabled then begin
        VStatic := TTileDownloaderStateStatic.Create(False, VState.DisableReason);
      end else begin
        VState := FSaverState.GetStatic;
        if not VState.Enabled then begin
          VStatic := TTileDownloaderStateStatic.Create(False, VState.DisableReason);
        end else begin
          VStatic := TTileDownloaderStateStatic.Create(True, '');
        end;
      end;
    end;
  end;
  Result := VStatic;
end;

function TTileDownloadSubsystemState.GetStatic: ITileDownloaderStateStatic;
begin
  Result := ITileDownloaderStateStatic(GetStaticInternal);
end;

end.
