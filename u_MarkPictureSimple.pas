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

unit u_MarkPictureSimple;

interface

uses
  SysUtils,
  Classes,
  i_SimpleFlag,
  i_BinaryData,
  i_BitmapMarker,
  i_BitmapTileSaveLoad,
  i_MarkPicture,
  u_BaseInterfacedObject;

type
  TMarkPictureSimple = class(TBaseInterfacedObject, IMarkPicture)
  private
    FFullFileName: string;
    FName: string;
    FLoader: IBitmapTileLoader;

    FCS: IReadWriteSync;
    FBitmapMarker: IBitmapMarker;
    FSource: IBinaryData;

    FInitedFlag: ISimpleFlag;
    procedure InitPic;
  private
    function GetMarker: IBitmapMarker;
  private
    function GetName: string;
    function GetSource: IBinaryData;

    function GetTextAlignment: TAlignment;
    function GetTextVerticalAlignment: TVerticalAlignment;
  public
    constructor Create(
      const AFullFileName: string;
      const AName: string;
      const ALoader: IBitmapTileLoader
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  t_GeoTypes,
  i_Bitmap32Static,
  u_BitmapMarker,
  u_SimpleFlagWithInterlock,
  u_BinaryDataByMemStream;

{ TMarkPictureSimple }
constructor TMarkPictureSimple.Create(
  const AFullFileName: string;
  const AName: string;
  const ALoader: IBitmapTileLoader
);
begin
  inherited Create;
  FFullFileName := AFullFileName;
  FName := AName;
  FLoader := ALoader;

  FCS := MakeSyncRW_Sym(Self, False);
  FInitedFlag := TSimpleFlagWithInterlock.Create;
end;

destructor TMarkPictureSimple.Destroy;
begin
  FCS := nil;
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
  VBitmap: IBitmap32Static;
  VAnchor: TDoublePoint;
begin
  if not FInitedFlag.CheckFlag then begin
    FCS.BeginWrite;
    try
      if not FInitedFlag.CheckFlag then begin
        VMemStream := TMemoryStream.Create;
        try
          VMemStream.LoadFromFile(FFullFileName);
          FSource := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
          VMemStream := nil;
        finally
          VMemStream.Free;
        end;
        VBitmap := FLoader.Load(FSource);

        VAnchor.X := VBitmap.Size.X / 2;
        VAnchor.Y := VBitmap.Size.Y;
        FBitmapMarker := TBitmapMarker.Create(VBitmap, VAnchor);
        FInitedFlag.SetFlag;
      end;
    finally
      FCS.EndWrite;
    end;
  end;
end;

function TMarkPictureSimple.GetMarker: IBitmapMarker;
begin
  InitPic;
  Result := FBitmapMarker;
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
