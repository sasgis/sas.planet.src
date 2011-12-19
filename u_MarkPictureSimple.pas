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
  Types,
  SyncObjs,
  Classes,
  GR32,
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

    FInited: Integer;
    procedure InitPic;
  protected
    function GetMarker: IBitmapMarker;
    function GetMarkerBySize(ASize: Integer): IBitmapMarker;
  protected
    function GetName: string;
    procedure ExportToStream(AStream: TStream);

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
  u_BitmapMarker,
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

procedure TMarkPictureSimple.ExportToStream(AStream: TStream);
var
  VMemStream: TMemoryStream;
  VOwnStream: Boolean;
begin
  InitPic;
  if AStream is TMemoryStream then begin
    VMemStream := TMemoryStream(AStream);
    VOwnStream := False;
  end else begin
    VMemStream := TMemoryStream.Create;
    VOwnStream := True;
  end;
  try
    VMemStream.LoadFromFile(FFullFileName);
    if VOwnStream then begin
      VMemStream.SaveToStream(AStream);
    end;
  finally
    if VOwnStream then begin
      VMemStream.Free;
    end;
  end;
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
  VBitmap: TCustomBitmap32;
  VAnchor: TDoublePoint;
  VBaseMarker: IBitmapMarker;
begin
  if InterlockedCompareExchange(FInited, 0, 0) = 0 then begin
    FCS.Acquire;
    try
      if InterlockedCompareExchange(FInited, 0, 0) = 0 then begin
        VBitmap := TCustomBitmap32.Create;
        try
          VMemStream := TMemoryStream.Create;
          try
            VMemStream.LoadFromFile(FFullFileName);
            FLoader.LoadFromStream(VMemStream, VBitmap);
          finally
            VMemStream.Free;
          end;
        except
          VBitmap.SetSize(0, 0);
        end;
        VAnchor.X := VBitmap.Width / 2;
        VAnchor.Y := VBitmap.Height;
        VBaseMarker := TBitmapMarker.CreateTakeBitmapOwn(VBitmap, VAnchor);
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

end.
