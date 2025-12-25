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

unit i_BitmapTileMatrixChangeable;

interface

uses
  i_Changeable,
  i_BitmapTileMatrix;

type
  TBitmapTileMatrixPrepareState = (psNone, psBusy, psComplete, psWaiting, psCancelled);

  IBitmapTileMatrixStateChangeable = interface(IChangeable)
    ['{573DCE49-46EA-4A5C-B8D6-2F834878C976}']
    function GetState: TBitmapTileMatrixPrepareState;
    property State: TBitmapTileMatrixPrepareState read GetState;
  end;

  IBitmapTileMatrixStateChangeableInternal = interface(IChangeable)
    ['{8358BC40-734B-40E1-8399-BE70BA25BBFB}']
    function GetState: TBitmapTileMatrixPrepareState;
    procedure SetState(AState: TBitmapTileMatrixPrepareState);
    property State: TBitmapTileMatrixPrepareState read GetState write SetState;
  end;

  IBitmapTileMatrixChangeable = interface(IChangeable)
    ['{2FDE6E8A-E4D3-4DA0-AF89-99DF831BAB6B}']
    function GetPrepareStateChangeable: IBitmapTileMatrixStateChangeable;
    property PrepareStateChangeable: IBitmapTileMatrixStateChangeable read GetPrepareStateChangeable;

    function GetStatic: IBitmapTileMatrix;
  end;

implementation

end.
