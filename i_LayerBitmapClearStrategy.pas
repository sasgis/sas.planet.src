{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit i_LayerBitmapClearStrategy;

interface

uses
  GR32,
  i_LocalCoordConverter;

type
  ILayerBitmapClearStrategy =  interface
    ['{F2E51CCC-D584-4D88-98E7-0057F3825F63}']
    procedure Clear(ABitmap: TCustomBitmap32);
  end;

  ILayerBitmapClearStrategyFactory = interface
    ['{9F14B47C-2D9C-4974-B78E-E3E3E6B74725}']
    function GetStrategy(
      const ASourceConverter, ATargetConverter: ILocalCoordConverter;
      ASourceBitmap: TCustomBitmap32;
      const APrevStrategy: ILayerBitmapClearStrategy
    ): ILayerBitmapClearStrategy;
  end;

implementation

end.
