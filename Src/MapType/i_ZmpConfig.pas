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

unit i_ZmpConfig;

interface

uses
  i_ConfigDataElement;

type
  IZmpConfig = interface(IConfigDataElement)
    ['{57185941-5902-4843-9E24-54CBEA4A67FD}']
    function GetMaxConnectToServerCount: Cardinal;
    procedure SetMaxConnectToServerCount(const AValue: Cardinal);
    property MaxConnectToServerCount: Cardinal read GetMaxConnectToServerCount write SetMaxConnectToServerCount;

    function GetUseMemCache: Boolean;
    procedure SetUseMemCache(const AValue: Boolean);
    property UseMemCache: Boolean read GetUseMemCache write SetUseMemCache;

    function GetMemCacheCapacity: Integer;
    procedure SetMemCacheCapacity(const AValue: Integer);
    property MemCacheCapacity: Integer read GetMemCacheCapacity write SetMemCacheCapacity;

    function GetMemCacheTTL: Cardinal;
    procedure SetMemCacheTTL(const AValue: Cardinal);
    property MemCacheTTL: Cardinal read GetMemCacheTTL write SetMemCacheTTL;

    function GetMemCacheClearStrategy: Integer;
    procedure SetMemCacheClearStrategy(const AValue: Integer);
    property MemCacheClearStrategy: Integer read GetMemCacheClearStrategy write SetMemCacheClearStrategy;
  end;

implementation

end.
