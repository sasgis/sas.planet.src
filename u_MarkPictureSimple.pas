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
  i_BitmapTileSaveLoad,
  i_MarkPicture;

type
  TMarkPictureSimple = class(TInterfacedObject, IMarkPicture)
    FFullFileName: string;
    FName: string;
    FLoader: IBitmapTileLoader;

    FCS: TCriticalSection;
    FBitmap: TCustomBitmap32;

    FInited: Integer;
    FBitmapSize: TPoint;
    procedure InitPic;
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
  FLoader := ALoader;

  FBitmap := TCustomBitmap32.Create;
  FCS := TCriticalSection.Create;
  FInited := 0;
end;

destructor TMarkPictureSimple.Destroy;
begin
  FreeAndNil(FBitmap);
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

function TMarkPictureSimple.GetPointInPicture: TPoint;
begin
  InitPic;
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

procedure TMarkPictureSimple.InitPic;
var
  VMemStream: TMemoryStream;
begin
  if InterlockedCompareExchange(FInited, 0, 0) = 0 then begin
    FCS.Acquire;
    try
      if InterlockedCompareExchange(FInited, 0, 0) = 0 then begin
        try
          VMemStream := TMemoryStream.Create;
          try
            VMemStream.LoadFromFile(FFullFileName);
            FLoader.LoadFromStream(VMemStream, FBitmap);
          finally
            VMemStream.Free;
          end;
        except
          FBitmap.SetSize(0, 0);
        end;
        FBitmapSize := Point(FBitmap.Width, FBitmap.Height);
        InterlockedIncrement(FInited);
      end;
    finally
      FCS.Release;
    end;
  end;
end;

procedure TMarkPictureSimple.LoadBitmap(ABmp: TCustomBitmap32);
begin
  InitPic;
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
  InitPic;
  Result := FBitmapSize;
end;


end.
