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

unit u_MarkPictureSimple;

interface

uses
  Windows,
  SyncObjs,
  Classes,
  GR32,
  i_BinaryData,
  i_BitmapMarker,
  i_BitmapTileSaveLoad,
  i_MarkPicture;

type
  TMarkPictureSimple = class(TInterfacedObject, IMarkPicture)
    FFullFileName: string;
    FName: string;
    FLoader: IBitmapTileLoader;

    FCS: TCriticalSection;
    FSimpleMarkerProvider: IBitmapMarkerProvider;
    FSource: IBinaryData;

    FInited: Integer;
    procedure InitPic;
  protected
    function GetMarker: IBitmapMarker;
    function GetMarkerBySize(ASize: Integer): IBitmapMarker;
  protected
    function GetName: string;
    function GetSource: IBinaryData;

    function GetTextAlignment: TAlignment;
    function GetTextVerticalAlignment: TVerticalAlignment;
  public
    constructor Create(AFullFileName: string; AName: string; ALoader: IBitmapTileLoader);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  i_Bitmap32Static,
  u_BitmapMarker,
  u_BinaryDataByMemStream,
  u_BitmapMarkerProviderStaticFromDataProvider;

{ TMarkPictureSimple }
constructor TMarkPictureSimple.Create(AFullFileName: string; AName: string; ALoader: IBitmapTileLoader);
begin
  FFullFileName := AFullFileName;
  FName := AName;
  FLoader := ALoader;

  FCS := TCriticalSection.Create;
  FInited := 0;
end;

destructor TMarkPictureSimple.Destroy;
begin
  FreeAndNil(FCS);
  inherited;
end;

function TMarkPictureSimple.GetTextAlignment: TAlignment;
begin
  Result := taRightJustify;
end;

function TMarkPictureSimple.GetTextVerticalAlignment: TVerticalAlignment;
begin
  Result := taVerticalCenter;
end;

procedure TMarkPictureSimple.InitPic;
var
  VMemStream: TMemoryStream;
  VBitmap:  IBitmap32Static;
  VAnchor: TDoublePoint;
  VBaseMarker: IBitmapMarker;
begin
  if InterlockedCompareExchange(FInited, 0, 0) = 0 then begin
    FCS.Acquire;
    try
      if InterlockedCompareExchange(FInited, 0, 0) = 0 then begin
        VMemStream := TMemoryStream.Create;
        try
          VMemStream.LoadFromFile(FFullFileName);
        except
          VMemStream.Free;
          raise;
        end;
        FSource := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
        VBitmap := FLoader.Load(FSource);

        VAnchor.X := VBitmap.Bitmap.Width / 2;
        VAnchor.Y := VBitmap.Bitmap.Height;
        VBaseMarker := TBitmapMarker.Create(VBitmap.Bitmap, VAnchor);
        FSimpleMarkerProvider := TBitmapMarkerProviderStatic.Create(VBaseMarker);
        InterlockedIncrement(FInited);
      end;
    finally
      FCS.Release;
    end;
  end;
end;

function TMarkPictureSimple.GetMarker: IBitmapMarker;
begin
  InitPic;
  Result := FSimpleMarkerProvider.GetMarker;
end;

function TMarkPictureSimple.GetMarkerBySize(ASize: Integer): IBitmapMarker;
begin
  InitPic;
  Result := FSimpleMarkerProvider.GetMarkerBySize(ASize);
end;

function TMarkPictureSimple.GetName: string;
begin
  Result := FName;
end;

function TMarkPictureSimple.GetSource: IBinaryData;
begin
  InitPic;
  Result := FSource;
end;

end.
