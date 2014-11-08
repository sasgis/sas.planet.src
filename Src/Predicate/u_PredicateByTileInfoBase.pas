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

unit u_PredicateByTileInfoBase;

interface

uses
  Types,
  i_TileInfoBasic,
  i_PredicateByBinaryData,
  i_PredicateByTileInfo,
  u_BaseInterfacedObject;

type
  TPredicateByTileInfoAbstract = class(TBaseInterfacedObject, IPredicateByTileInfo)
  protected
    function Check(
      const ATileInfo: ITileInfoBasic;
      AZoom: Byte;
      const ATile: TPoint
    ): Boolean; overload;
    function Check(const ATileInfo: TTileInfo): Boolean; overload; virtual; abstract;
  end;

  TPredicateByTileInfoExistsTile = class(TPredicateByTileInfoAbstract)
  protected
    function Check(const ATileInfo: TTileInfo): Boolean; override;
  end;

  TPredicateByTileInfoExistsTileCheckData = class(TPredicateByTileInfoAbstract)
  private
    FDataCheck: IPredicateByBinaryData;
  protected
    function Check(const ATileInfo: TTileInfo): Boolean; override;
  public
    constructor Create(ADataCheck: IPredicateByBinaryData);
  end;

  TPredicateByTileInfoNotExistsTile = class(TPredicateByTileInfoAbstract)
  protected
    function Check(const ATileInfo: TTileInfo): Boolean; override;
  end;

  TPredicateByTileInfoExistsTNE = class(TPredicateByTileInfoAbstract)
  protected
    function Check(const ATileInfo: TTileInfo): Boolean; override;
  end;

  TPredicateByTileInfoNotExistsTNE = class(TPredicateByTileInfoAbstract)
  protected
    function Check(const ATileInfo: TTileInfo): Boolean; override;
  end;

  TPredicateByTileInfoExistsTileOrTNE = class(TPredicateByTileInfoAbstract)
  protected
    function Check(const ATileInfo: TTileInfo): Boolean; override;
  end;

  TPredicateByTileInfoNotExistsTileOrTNE = class(TPredicateByTileInfoAbstract)
  protected
    function Check(const ATileInfo: TTileInfo): Boolean; override;
  end;

  TPredicateByTileInfoEqualSize = class(TPredicateByTileInfoAbstract)
  private
    FSize: Cardinal;
    FDeleteTNE: Boolean;
  protected
    function Check(const ATileInfo: TTileInfo): Boolean; override;
  public
    constructor Create(
      ADeleteTNE: Boolean;
      ASize: Cardinal
    );
  end;

  TPredicateByTileInfoNotExistOrBeforDate = class(TPredicateByTileInfoAbstract)
  private
    FDate: TDateTime;
    FIgnoreTNE: Boolean;
  protected
    function Check(const ATileInfo: TTileInfo): Boolean; override;
  public
    constructor Create(
      AIgnoreTNE: Boolean;
      const ADate: TDateTime
    );
  end;

implementation

uses
  SysUtils;

{ TPredicateByTileInfoAbstract }

function TPredicateByTileInfoAbstract.Check(
  const ATileInfo: ITileInfoBasic;
  AZoom: Byte;
  const ATile: TPoint
): Boolean;
var
  VTileInfo: TTileInfo;
  VTileWithData: ITileInfoWithData;
begin
  VTileInfo.FTile := ATile;
  VTileInfo.FZoom := AZoom;
  if ATileInfo = nil then begin
    VTileInfo.FInfoType := titNotExists;
    VTileInfo.FData := nil;
  end else if ATileInfo.IsExists then begin
    VTileInfo.FInfoType := titExists;
    if Supports(ATileInfo, ITileInfoWithData, VTileWithData) then begin
      VTileInfo.FData := VTileWithData.TileData;
    end else begin
      VTileInfo.FData := nil;
    end;
  end else if ATileInfo.IsExistsTNE then begin
    VTileInfo.FInfoType := titTneExists;
    VTileInfo.FData := nil;
  end else begin
    VTileInfo.FInfoType := titNotExists;
    VTileInfo.FData := nil;
  end;
  VTileInfo.FLoadDate := ATileInfo.LoadDate;
  VTileInfo.FVersionInfo := ATileInfo.VersionInfo;
  VTileInfo.FContentType := ATileInfo.ContentType;
  VTileInfo.FSize := ATileInfo.Size;
  Result := Check(VTileInfo);
end;

{ TPredicateByTileInfoEqualSize }

constructor TPredicateByTileInfoEqualSize.Create(
  ADeleteTNE: Boolean;
  ASize: Cardinal
);
begin
  inherited Create;
  FSize := ASize;
  FDeleteTNE := ADeleteTNE;
end;

function TPredicateByTileInfoEqualSize.Check(
  const ATileInfo: TTileInfo): Boolean;
begin
  Result := False;
  if ATileInfo.FInfoType = titExists then begin
    Result := ATileInfo.FSize = FSize;
  end else if ATileInfo.FInfoType = titTneExists then begin
    Result := FDeleteTNE;
  end;
end;

{ TPredicateByTileInfoExistsTile }

function TPredicateByTileInfoExistsTile.Check(
  const ATileInfo: TTileInfo): Boolean;
begin
  Result := ATileInfo.FInfoType = titExists;
end;

{ TPredicateByTileInfoExistsTNE }

function TPredicateByTileInfoExistsTNE.Check(
  const ATileInfo: TTileInfo): Boolean;
begin
  Result := ATileInfo.FInfoType = titTneExists;
end;

{ TPredicateByTileInfoExistsTileOrTNE }

function TPredicateByTileInfoExistsTileOrTNE.Check(
  const ATileInfo: TTileInfo): Boolean;
begin
  Result := (ATileInfo.FInfoType = titTneExists) or (ATileInfo.FInfoType = titExists);
end;

{ TPredicateByTileInfoNotExistsTile }

function TPredicateByTileInfoNotExistsTile.Check(
  const ATileInfo: TTileInfo): Boolean;
begin
  Result := ATileInfo.FInfoType <> titExists;
end;

{ TPredicateByTileInfoNotExistsTNE }

function TPredicateByTileInfoNotExistsTNE.Check(
  const ATileInfo: TTileInfo): Boolean;
begin
  Result := ATileInfo.FInfoType <> titTneExists;
end;

{ TPredicateByTileInfoNotExistsTileOrTNE }

function TPredicateByTileInfoNotExistsTileOrTNE.Check(
  const ATileInfo: TTileInfo): Boolean;
begin
  Result := (ATileInfo.FInfoType <> titTneExists) and (ATileInfo.FInfoType <> titExists);
end;

{ TPredicateByTileInfoBeforDate }

constructor TPredicateByTileInfoNotExistOrBeforDate.Create(
  AIgnoreTNE: Boolean;
  const ADate: TDateTime
);
begin
  inherited Create;
  FDate := ADate;
  FIgnoreTNE := AIgnoreTNE;
end;

function TPredicateByTileInfoNotExistOrBeforDate.Check(
  const ATileInfo: TTileInfo
): Boolean;
begin
  if ATileInfo.FInfoType = titNotExists then begin
    Result := True;
  end else if (ATileInfo.FInfoType = titTneExists) and FIgnoreTNE then begin
    Result := True;
  end else if (ATileInfo.FInfoType = titExists) and (ATileInfo.FLoadDate < FDate) then begin
    Result := True;
  end else begin
    Result := False;
  end;
end;

{ TPredicateByTileInfoExistsTileCheckData }

constructor TPredicateByTileInfoExistsTileCheckData.Create(
  ADataCheck: IPredicateByBinaryData
);
begin
  inherited Create;
  FDataCheck := ADataCheck;
end;

function TPredicateByTileInfoExistsTileCheckData.Check(
  const ATileInfo: TTileInfo
): Boolean;
begin
  if ATileInfo.FInfoType = titExists then begin
    Result := FDataCheck.Check(ATileInfo.FData);
  end else begin
    Result := False;
  end;
end;

end.
