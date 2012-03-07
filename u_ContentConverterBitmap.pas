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

unit u_ContentConverterBitmap;

interface

uses
  Classes,
  i_BinaryData,
  i_ContentTypeInfo,
  i_BitmapTileSaveLoad,
  u_ContentConverterBase;

type
  TContentConverterBitmap = class(TContentConverterBase)
  private
    FSourceLoader: IBitmapTileLoader;
    FTargetSaver: IBitmapTileSaver;
  protected
    procedure ConvertStream(ASource, ATarget: TStream); override;
    function Convert(AData: IBinaryData): IBinaryData; override;
  public
    constructor Create(
      ASource: IContentTypeInfoBasic;
      ATarget: IContentTypeInfoBasic
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32,
  u_ResStrings;

{ TContentConverterBitmap }

constructor TContentConverterBitmap.Create(ASource,
  ATarget: IContentTypeInfoBasic);
var
  VSource: IContentTypeInfoBitmap;
  VTarget: IContentTypeInfoBitmap;
begin
  inherited;
  VSource := GetSource as IContentTypeInfoBitmap;
  FSourceLoader := VSource.GetLoader;
  if FSourceLoader = nil then begin
    raise Exception.Create(SAS_ERR_CantLoadBitmapFromSourceType);
  end;

  VTarget := GetTarget as IContentTypeInfoBitmap;
  FTargetSaver := VTarget.GetSaver;
  if FTargetSaver  = nil then begin
    raise Exception.Create(SAS_ERR_CantSaveBitmapToTargetType);
  end;
end;

destructor TContentConverterBitmap.Destroy;
begin
  FSourceLoader := nil;
  FTargetSaver := nil;
  inherited;
end;

function TContentConverterBitmap.Convert(AData: IBinaryData): IBinaryData;
begin
  Result := FTargetSaver.Save(FSourceLoader.Load(AData));
end;

procedure TContentConverterBitmap.ConvertStream(ASource, ATarget: TStream);
var
  VBitmap: TCustomBitmap32;
begin
  inherited;
  VBitmap := TCustomBitmap32.Create;
  try
    FSourceLoader.LoadFromStream(ASource, VBitmap);
    FTargetSaver.SaveToStream(VBitmap, ATarget);
  finally
    VBitmap.Free;
  end;
end;

end.
