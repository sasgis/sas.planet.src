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

unit u_ContentTypeInfo;

interface

uses
  i_BitmapTileSaveLoad,
  i_VectorDataLoader,
  i_ContentTypeInfo,
  u_BaseInterfacedObject;

type
  TContentTypeInfoBase = class(TBaseInterfacedObject, IContentTypeInfoBasic)
  private
    FContentType: AnsiString;
    FDefaultExt: AnsiString;
  private
    function GetContentType: AnsiString;
    function GetDefaultExt: AnsiString;
  protected
    function CheckOtherForSaveCompatible(const AContentType: IContentTypeInfoBasic): Boolean; virtual; abstract;
  public
    constructor Create(
      const AContentType: AnsiString;
      const ADefaultExt: AnsiString
    );
  end;

  TContentTypeInfoBitmap = class(TContentTypeInfoBase, IContentTypeInfoBitmap)
  private
    FLoader: IBitmapTileLoader;
    FSaver: IBitmapTileSaver;
  protected
    function CheckOtherForSaveCompatible(const AContentType: IContentTypeInfoBasic): Boolean; override;
  private
    function GetLoader: IBitmapTileLoader;
    function GetSaver: IBitmapTileSaver;
  public
    constructor Create(
      const AContentType: AnsiString;
      const ADefaultExt: AnsiString;
      const ALoader: IBitmapTileLoader;
      const ASaver: IBitmapTileSaver
    );
  end;

  TContentTypeInfoVector = class(TContentTypeInfoBase, IContentTypeInfoVectorData)
  private
    FLoader: IVectorDataLoader;
  private
    function GetLoader: IVectorDataLoader;
  protected
    function CheckOtherForSaveCompatible(const AContentType: IContentTypeInfoBasic): Boolean; override;
  public
    constructor Create(
      const AContentType: AnsiString;
      const ADefaultExt: AnsiString;
      const ALoader: IVectorDataLoader
    );
  end;

implementation

uses
  SysUtils;

{ TContentTypeInfoBase }

constructor TContentTypeInfoBase.Create(
  const AContentType: AnsiString;
  const ADefaultExt: AnsiString
);
begin
  inherited Create;
  FContentType := AContentType;
  FDefaultExt := ADefaultExt;
end;

function TContentTypeInfoBase.GetContentType: AnsiString;
begin
  Result := FContentType;
end;

function TContentTypeInfoBase.GetDefaultExt: AnsiString;
begin
  Result := FDefaultExt;
end;

{ TContentTypeInfoBitmap }

constructor TContentTypeInfoBitmap.Create(
  const AContentType: AnsiString;
  const ADefaultExt: AnsiString;
  const ALoader: IBitmapTileLoader;
  const ASaver: IBitmapTileSaver
);
begin
  inherited Create(AContentType, ADefaultExt);
  FLoader := ALoader;
  FSaver := ASaver;
end;

function TContentTypeInfoBitmap.CheckOtherForSaveCompatible(
  const AContentType: IContentTypeInfoBasic): Boolean;
var
  VBitmapType: IContentTypeInfoBitmap;
begin
  if Supports(AContentType, IContentTypeInfoBitmap, VBitmapType) then begin
    Result := FDefaultExt = VBitmapType.GetDefaultExt;
  end else begin
    Result := False;
  end;
end;

function TContentTypeInfoBitmap.GetLoader: IBitmapTileLoader;
begin
  Result := FLoader;
end;

function TContentTypeInfoBitmap.GetSaver: IBitmapTileSaver;
begin
  Result := FSaver;
end;

{ TContentTypeInfoVector }

function TContentTypeInfoVector.CheckOtherForSaveCompatible(
  const AContentType: IContentTypeInfoBasic): Boolean;
var
  VVectorType: IContentTypeInfoVectorData;
begin
  if Supports(AContentType, IContentTypeInfoVectorData, VVectorType) then begin
    Result := FDefaultExt = VVectorType.GetDefaultExt;
  end else begin
    Result := False;
  end;
end;

constructor TContentTypeInfoVector.Create(
  const AContentType: AnsiString;
  const ADefaultExt: AnsiString;
  const ALoader: IVectorDataLoader
);
begin
  inherited Create(AContentType, ADefaultExt);
  FLoader := ALoader;
end;

function TContentTypeInfoVector.GetLoader: IVectorDataLoader;
begin
  Result := FLoader;
end;

end.
