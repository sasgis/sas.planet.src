{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_BitmapTileProviderWithRecolor;

interface

uses
  Types,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Projection,
  i_BitmapTileProvider,
  i_BitmapPostProcessing,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderWithRecolor = class(TBaseInterfacedObject, IBitmapTileProvider)
  private
    FRecolorConfig: IBitmapPostProcessing;
    FSourceProvider: IBitmapTileProvider;
  private
    function GetProjection: IProjection;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const ARecolorConfig: IBitmapPostProcessing;
      const ASourceProvider: IBitmapTileProvider
    );
  end;

implementation

{ TBitmapTileProviderWithRecolor }

constructor TBitmapTileProviderWithRecolor.Create(
  const ARecolorConfig: IBitmapPostProcessing;
  const ASourceProvider: IBitmapTileProvider
);
begin
  Assert(Assigned(ARecolorConfig));
  Assert(Assigned(ASourceProvider));
  inherited Create;
  FSourceProvider := ASourceProvider;
  FRecolorConfig := ARecolorConfig;
end;

function TBitmapTileProviderWithRecolor.GetProjection: IProjection;
begin
  Result := FSourceProvider.Projection;
end;

function TBitmapTileProviderWithRecolor.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IBitmap32Static;
begin
  Result := FSourceProvider.GetTile(AOperationID, ACancelNotifier, ATile);
  if Result <> nil then begin
    Result := FRecolorConfig.Process(Result);
  end;
end;

end.
