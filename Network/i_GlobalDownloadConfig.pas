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

unit i_GlobalDownloadConfig;

interface

uses
  i_ConfigDataElement;

type
  IGlobalDownloadConfig = interface(IConfigDataElement)
    ['{66442801-51C5-43DF-AB59-747E058A3567}']
    // Переходить к следующему тайлу если произошла ошибка закачки
    function GetIsGoNextTileIfDownloadError: Boolean;
    procedure SetIsGoNextTileIfDownloadError(AValue: Boolean);
    property IsGoNextTileIfDownloadError: Boolean read GetIsGoNextTileIfDownloadError write SetIsGoNextTileIfDownloadError;

    //Начать сохраненную сессию загрузки с последнего удачно загруженного тайла
    function GetIsUseSessionLastSuccess: Boolean;
    procedure SetIsUseSessionLastSuccess(AValue: Boolean);
    property IsUseSessionLastSuccess: Boolean read GetIsUseSessionLastSuccess write SetIsUseSessionLastSuccess;

    //Записывать информацию о тайлах отсутствующих на сервере
    function GetIsSaveTileNotExists: Boolean;
    procedure SetIsSaveTileNotExists(AValue: Boolean);
    property IsSaveTileNotExists: Boolean read GetIsSaveTileNotExists write SetIsSaveTileNotExists;
  end;

implementation

end.
