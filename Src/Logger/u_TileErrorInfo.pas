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

unit u_TileErrorInfo;

interface

uses
  Types,
  i_TileError,
  i_TileRequestResult,
  i_DownloadResult,
  u_BaseInterfacedObject;

type
  TTileErrorInfoBase = class(TBaseInterfacedObject, ITileErrorInfo)
  private
    FMapTypeGUID: TGUID;
    FZoom: Byte;
    FTile: TPoint;
  private
    function GetMapTypeGUID: TGUID;
    function GetZoom: Byte;
    function GetTile: TPoint;
    function GetErrorText: string; virtual; abstract;
  public
    constructor Create(
      const AMapTypeGUID: TGUID;
      const AZoom: Byte;
      const ATile: TPoint
    );
  end;

  TTileErrorInfo = class(TTileErrorInfoBase)
  private
    FErrorText: string;
  private
    function GetErrorText: string; override;
  public
    constructor Create(
      const AMapTypeGUID: TGUID;
      const AZoom: Byte;
      const ATile: TPoint;
      const AErrorText: string
    );
  end;

  TTileErrorInfoByDataNotExists = class(TTileErrorInfoBase)
  private
    FResult: IDownloadResultDataNotExists;
  private
    function GetErrorText: string; override;
  public
    constructor Create(
      const AMapTypeGUID: TGUID;
      const AZoom: Byte;
      const ATile: TPoint;
      const AResult: IDownloadResultDataNotExists
    );
  end;

  TTileErrorInfoByDownloadResultError = class(TTileErrorInfoBase)
  private
    FResult: IDownloadResultError;
  private
    function GetErrorText: string; override;
  public
    constructor Create(
      const AMapTypeGUID: TGUID;
      const AZoom: Byte;
      const ATile: TPoint;
      const AResult: IDownloadResultError
    );
  end;

  TTileErrorInfoByNotNecessary = class(TTileErrorInfoBase)
  private
    FResult: IDownloadResultNotNecessary;
  private
    function GetErrorText: string; override;
  public
    constructor Create(
      const AMapTypeGUID: TGUID;
      const AZoom: Byte;
      const ATile: TPoint;
      const AResult: IDownloadResultNotNecessary
    );
  end;

  TTileErrorInfoByTileRequestResult = class(TTileErrorInfoBase)
  private
    FResult: ITileRequestResultError;
  private
    function GetErrorText: string; override;
  public
    constructor Create(
      const AMapTypeGUID: TGUID;
      const AResult: ITileRequestResultError
    );
  end;

implementation

{ TTileErrorInfoBase }

constructor TTileErrorInfoBase.Create(
  const AMapTypeGUID: TGUID;
  const AZoom: Byte;
  const ATile: TPoint
);
begin
  inherited Create;
  FMapTypeGUID := AMapTypeGUID;
  FZoom := AZoom;
  FTile := ATile;
end;

function TTileErrorInfoBase.GetMapTypeGUID: TGUID;
begin
  Result := FMapTypeGUID;
end;

function TTileErrorInfoBase.GetTile: TPoint;
begin
  Result := FTile;
end;

function TTileErrorInfoBase.GetZoom: Byte;
begin
  Result := FZoom;
end;

{ TTileErrorInfo }

constructor TTileErrorInfo.Create(
  const AMapTypeGUID: TGUID;
  const AZoom: Byte;
  const ATile: TPoint;
  const AErrorText: string
);
begin
  inherited Create(AMapTypeGUID, AZoom, ATile);
  FErrorText := AErrorText;
end;

function TTileErrorInfo.GetErrorText: string;
begin
  Result := FErrorText;
end;

{ TTileErrorInfoByTileRequestResult }

constructor TTileErrorInfoByTileRequestResult.Create(
  const AMapTypeGUID: TGUID;
  const AResult: ITileRequestResultError
);
begin
  Assert(AResult <> nil);
  inherited Create(AMapTypeGUID, AResult.Request.Zoom, AResult.Request.Tile);
  FResult := AResult;
end;

function TTileErrorInfoByTileRequestResult.GetErrorText: string;
begin
  Result := FResult.ErrorText;
end;

{ TTileErrorInfoByDataNotExists }

constructor TTileErrorInfoByDataNotExists.Create(
  const AMapTypeGUID: TGUID;
  const AZoom: Byte;
  const ATile: TPoint;
  const AResult: IDownloadResultDataNotExists
);
begin
  inherited Create(AMapTypeGUID, AZoom, ATile);
  FResult := AResult;
end;

function TTileErrorInfoByDataNotExists.GetErrorText: string;
begin
  Result := FResult.ReasonText;
end;

{ TTileErrorInfoByDownloadResultError }

constructor TTileErrorInfoByDownloadResultError.Create(
  const AMapTypeGUID: TGUID; const AZoom: Byte; const ATile: TPoint;
  const AResult: IDownloadResultError);
begin
  inherited Create(AMapTypeGUID, AZoom, ATile);
  FResult := AResult;
end;

function TTileErrorInfoByDownloadResultError.GetErrorText: string;
begin
  Result := FResult.ErrorText;
end;

{ TTileErrorInfoByNotNecessary }

constructor TTileErrorInfoByNotNecessary.Create(const AMapTypeGUID: TGUID;
  const AZoom: Byte; const ATile: TPoint;
  const AResult: IDownloadResultNotNecessary);
begin
  inherited Create(AMapTypeGUID, AZoom, ATile);
  FResult := AResult;
end;

function TTileErrorInfoByNotNecessary.GetErrorText: string;
begin
  Result := FResult.ReasonText;
end;

end.
