{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_ContentTypeInfo;

interface

uses
  i_BitmapTileSaveLoad,
  i_VectorDataLoader,
  i_ContentTypeInfo;

type
  TContentTypeInfoBase = class(TInterfacedObject, IContentTypeInfoBasic)
  private
    FContentType: WideString;
    FDefaultExt: WideString;
  protected
    function GetContentType: WideString;
    function GetDefaultExt: WideString;
  public
    constructor Create(
      const AContentType: WideString;
      const ADefaultExt: WideString
    );
  end;

  TContentTypeInfoBitmap = class(TContentTypeInfoBase, IContentTypeInfoBitmap)
  private
    FLoader: IBitmapTileLoader;
    FSaver: IBitmapTileSaver;
  protected
    function GetLoader: IBitmapTileLoader;
    function GetSaver: IBitmapTileSaver;
  public
    constructor Create(
      const AContentType: WideString;
      const ADefaultExt: WideString;
      const ALoader: IBitmapTileLoader;
      const ASaver: IBitmapTileSaver
    );
    destructor Destroy; override;
  end;

  TContentTypeInfoKml = class(TContentTypeInfoBase, IContentTypeInfoVectorData)
  private
    FLoader: IVectorDataLoader;
  protected
    function GetLoader: IVectorDataLoader;
  public
    constructor Create(
      const AContentType: WideString;
      const ADefaultExt: WideString;
      const ALoader: IVectorDataLoader
    );
    destructor Destroy; override;
  end;

implementation

{ TContentTypeInfoBase }

constructor TContentTypeInfoBase.Create(const AContentType, ADefaultExt: WideString);
begin
  FContentType := AContentType;
  FDefaultExt := ADefaultExt;
end;

function TContentTypeInfoBase.GetContentType: WideString;
begin
  Result := FContentType;
end;

function TContentTypeInfoBase.GetDefaultExt: WideString;
begin
  Result := FDefaultExt;
end;

{ TContentTypeInfoBitmap }

constructor TContentTypeInfoBitmap.Create(
  const AContentType, ADefaultExt: WideString;
  const ALoader: IBitmapTileLoader;
  const ASaver: IBitmapTileSaver
);
begin
  inherited Create(AContentType, ADefaultExt);
  FLoader := ALoader;
  FSaver := ASaver;
end;

destructor TContentTypeInfoBitmap.Destroy;
begin
  FLoader := nil;
  FSaver := nil;
  inherited;
end;

function TContentTypeInfoBitmap.GetLoader: IBitmapTileLoader;
begin
  Result := FLoader;
end;

function TContentTypeInfoBitmap.GetSaver: IBitmapTileSaver;
begin
  Result := FSaver;
end;

{ TContentTypeInfoKml }

constructor TContentTypeInfoKml.Create(
  const AContentType, ADefaultExt: WideString;
  const ALoader: IVectorDataLoader
);
begin
  inherited Create(AContentType, ADefaultExt);
  FLoader := ALoader;
end;

destructor TContentTypeInfoKml.Destroy;
begin
  FLoader := nil;
  inherited;
end;

function TContentTypeInfoKml.GetLoader: IVectorDataLoader;
begin
  Result := FLoader;
end;

end.
