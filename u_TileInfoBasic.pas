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

unit u_TileInfoBasic;

interface

uses
  i_BinaryData,
  i_ContentTypeInfo,
  i_MapVersionInfo,
  i_TileInfoBasic;

type
  TTileInfoBasicBase = class(TInterfacedObject, ITileInfoBasic)
  private
    FDate: TDateTime;
    FVersionInfo: IMapVersionInfo;
  protected
    function GetIsExists: Boolean; virtual; abstract;
    function GetIsExistsTNE: Boolean; virtual; abstract;
    function GetLoadDate: TDateTime; 
    function GetTileData: IBinaryData; virtual;
    function GetSize: Cardinal; virtual; abstract;
    function GetVersionInfo: IMapVersionInfo; 
    function GetContentType: IContentTypeInfoBasic; virtual; abstract;
  public
    constructor Create(
      const ADate: TDateTime;
      const AVersionInfo: IMapVersionInfo
    );
  end;

  TTileInfoBasicNotExists = class(TTileInfoBasicBase)
  protected
    function GetIsExists: Boolean; override;
    function GetIsExistsTNE: Boolean; override;
    function GetSize: Cardinal; override;
    function GetContentType: IContentTypeInfoBasic; override;
  end;

  TTileInfoBasicTNE = class(TTileInfoBasicBase)
  protected
    function GetIsExists: Boolean; override;
    function GetIsExistsTNE: Boolean; override;
    function GetSize: Cardinal; override;
    function GetContentType: IContentTypeInfoBasic; override;
  end;

  TTileInfoBasicExists = class(TTileInfoBasicBase)
  private
    FSize: Cardinal;
    FContentType: IContentTypeInfoBasic;
  protected
    function GetIsExists: Boolean; override;
    function GetIsExistsTNE: Boolean; override;
    function GetSize: Cardinal; override;
    function GetContentType: IContentTypeInfoBasic; override;
  public
    constructor Create(
      const ADate: TDateTime;
      ASize: Cardinal;
      const AVersionInfo: IMapVersionInfo;
      const AContentType: IContentTypeInfoBasic
    );
  end;

  TTileInfoBasicExistsWithTile = class(TTileInfoBasicExists)
  private
    FTileData: IBinaryData;
  protected
    function GetTileData: IBinaryData; override;
  public
    constructor Create(
      const ADate: TDateTime;
      const ATileData: IBinaryData;
      ASize: Cardinal;
      const AVersionInfo: IMapVersionInfo;
      const AContentType: IContentTypeInfoBasic
    );
    destructor Destroy; override;
  end;

implementation

{ TTileInfoBasicBase }

constructor TTileInfoBasicBase.Create(
  const ADate: TDateTime;
  const AVersionInfo: IMapVersionInfo
);
begin
  FDate := ADate;
  FVersionInfo := AVersionInfo;
end;

function TTileInfoBasicBase.GetLoadDate: TDateTime;
begin
  Result := FDate;
end;

function TTileInfoBasicBase.GetVersionInfo: IMapVersionInfo;
begin
  Result := FVersionInfo;
end;

function TTileInfoBasicBase.GetTileData: IBinaryData;
begin
  Result := nil;
end;

{ TTileInfoBasicTNE }

function TTileInfoBasicTNE.GetContentType: IContentTypeInfoBasic;
begin
  Result := nil;
end;

function TTileInfoBasicTNE.GetIsExists: Boolean;
begin
  Result := False;
end;

function TTileInfoBasicTNE.GetIsExistsTNE: Boolean;
begin
  Result := True;
end;

function TTileInfoBasicTNE.GetSize: Cardinal;
begin
  Result := 0;
end;

{ TTileInfoBasicExists }

constructor TTileInfoBasicExists.Create(
  const ADate: TDateTime;
  ASize: Cardinal;
  const AVersionInfo: IMapVersionInfo;
  const AContentType: IContentTypeInfoBasic
);
begin
  inherited Create(ADate, AVersionInfo);
  FSize := ASize;
  FContentType := AContentType;
end;

function TTileInfoBasicExists.GetContentType: IContentTypeInfoBasic;
begin
  Result := FContentType;
end;

function TTileInfoBasicExists.GetIsExists: Boolean;
begin
  Result := True;
end;

function TTileInfoBasicExists.GetIsExistsTNE: Boolean;
begin
  Result := False;
end;

function TTileInfoBasicExists.GetSize: Cardinal;
begin
  Result := FSize;
end;

{ TTileInfoBasicExistsWithTile }

constructor TTileInfoBasicExistsWithTile.Create(
  const ADate: TDateTime;
  const ATileData: IBinaryData;
  ASize: Cardinal;
  const AVersionInfo: IMapVersionInfo;
  const AContentType: IContentTypeInfoBasic
);
begin
  inherited Create(ADate, ASize, AVersionInfo, AContentType);
  FTileData := ATileData;
end;

destructor TTileInfoBasicExistsWithTile.Destroy;
begin
  FTileData := nil;
  inherited;
end;

function TTileInfoBasicExistsWithTile.GetTileData: IBinaryData;
begin
  Result := FTileData;
end;

{ TTileInfoBasicNotExists }

function TTileInfoBasicNotExists.GetContentType: IContentTypeInfoBasic;
begin
  Result := nil;
end;

function TTileInfoBasicNotExists.GetIsExists: Boolean;
begin
  Result := False;
end;

function TTileInfoBasicNotExists.GetIsExistsTNE: Boolean;
begin
  Result := False;
end;

function TTileInfoBasicNotExists.GetSize: Cardinal;
begin
  Result := 0;
end;

end.
