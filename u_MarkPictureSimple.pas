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
  Types,
  Classes,
  GR32,
  i_BitmapTileSaveLoad,
  i_MarkPicture;

type
  TMarkPictureSimple = class(TInterfacedObject, IMarkPicture)
    FFullFileName: string;
    FName: string;
    FBitmap: TCustomBitmap32;
    FBitmapSize: TPoint;
  protected
    function GetName: string;
    procedure ExportToStream(AStream: TStream);
    procedure LoadBitmap(ABmp: TCustomBitmap32);
    function GetPointInPicture: TPoint;
    function GetTextAlignment: TAlignment;
    function GetTextVerticalAlignment: TVerticalAlignment;
    function GetBitmapSize: TPoint;
  public
    constructor Create(AFullFileName: string; AName: string; ALoader: IBitmapTileLoader);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_LowLevel;

{ TMarkPictureSimple }
constructor TMarkPictureSimple.Create(AFullFileName: string; AName: string; ALoader: IBitmapTileLoader);
begin
  FFullFileName := AFullFileName;
  FName := AName;
  FBitmap := TCustomBitmap32.Create;
  ALoader.LoadFromFile(FFullFileName, FBitmap);
  FBitmapSize := Point(FBitmap.Width, FBitmap.Height);
end;

destructor TMarkPictureSimple.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TMarkPictureSimple.ExportToStream(AStream: TStream);
var
  VMemStream: TMemoryStream;
  VOwnStream: Boolean;
begin
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

function TMarkPictureSimple.GetPointInPicture: TPoint;
begin
  Result.X := FBitmapSize.X div 2;
  Result.Y := FBitmapSize.Y;
end;

function TMarkPictureSimple.GetTextAlignment: TAlignment;
begin
  Result := taRightJustify;
end;

function TMarkPictureSimple.GetTextVerticalAlignment: TVerticalAlignment;
begin
  Result := taVerticalCenter;
end;

procedure TMarkPictureSimple.LoadBitmap(ABmp: TCustomBitmap32);
begin
  ABmp.SetSize(FBitmapSize.X, FBitmapSize.Y);
  if not FBitmap.Empty then
    MoveLongword(FBitmap.Bits[0], ABmp.Bits[0], FBitmapSize.X * FBitmapSize.Y);
end;

function TMarkPictureSimple.GetName: string;
begin
  Result := FName;
end;

function TMarkPictureSimple.GetBitmapSize: TPoint;
begin
  result:=FBitmapSize;
end;


end.
