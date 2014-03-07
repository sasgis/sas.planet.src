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

unit i_TileRequestResult;

interface

uses
  i_TileRequest,
  i_TileDownloadRequest,
  i_DownloadResult;

type
  ITileRequestResult = interface
    ['{EE795F1F-AE10-42A4-99F4-2923B9D9F7FA}']
    function GetRequest: ITileRequest;
    property Request: ITileRequest read GetRequest;
  end;

  ITileRequestResultCanceled = interface(ITileRequestResult)
    ['{2A22DD2C-6D70-4F27-AC7F-FB5ADB66B5A6}']
  end;

  ITileRequestResultOk = interface(ITileRequestResult)
    ['{609F98D5-72DB-4D6A-A0A9-8E3BDFCF1325}']
  end;

  ITileRequestResultError = interface(ITileRequestResult)
    ['{38404B81-9EB3-48AB-87C2-4345EAC5F971}']
    function GetErrorText: string;
    property ErrorText: string read GetErrorText;
  end;

  ITileRequestResultStorageError = interface(ITileRequestResultError)
    ['{6B5D1AA4-E81B-4C3D-8EEE-AB0B371D29CC}']
  end;

  ITileRequestResultWithDownloadRequest = interface
    ['{296E1F57-68EA-4C78-AFC8-7A62BB2D2F78}']
    function GetDownloadRequest: ITileDownloadRequest;
    property DownloadRequest: ITileDownloadRequest read GetDownloadRequest;
  end;

  ITileRequestResultWithDownloadResult = interface
    ['{474646B2-7F30-4CB1-8BB6-174FE61FC92D}']
    function GetDownloadResult: IDownloadResult;
    property DownloadResult: IDownloadResult read GetDownloadResult;
  end;

implementation

end.
