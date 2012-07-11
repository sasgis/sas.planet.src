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

unit u_BitmapTileJpegLoadSave;

interface

uses
  Classes,
  GR32,
  i_BinaryData,
  i_Bitmap32Static,
  i_BitmapTileSaveLoad,
  i_InternalPerformanceCounter;

type
  TBitmapTileJpegLoadSave = class(
    TInterfacedObject,
    IBitmapTileLoader,
    IBitmapTileSaver
  )
  private
    FLoader: IBitmapTileLoader;
    FSaver: IBitmapTileSaver;
    function Load(const AData: IBinaryData): IBitmap32Static;
    function Save(const ABitmap: IBitmap32Static): IBinaryData;
    procedure SaveToStream(
      ABtm: TCustomBitmap32;
      AStream: TStream
    );
  public
    constructor Create(
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ); overload;
    constructor Create(
      const ACompressionQuality: Byte;
      const APerfCounterList: IInternalPerformanceCounterList = nil
    ); overload;
    destructor Destroy; override;
  end;

implementation

{$DEFINE NATIVE_LIBJPEG}

uses
  u_InternalPerformanceCounterList,
  {$IFDEF NATIVE_LIBJPEG}
  u_BitmapTileLibJpeg;
  {$ELSE}
  u_BitmapTileVampyreLoader,
  u_BitmapTileVampyreSaver;
  {$ENDIF}

{ TBitmapTileJpegLoadSave }

constructor TBitmapTileJpegLoadSave.Create(
  const APerfCounterList: IInternalPerformanceCounterList = nil
);
var
  VPerfCounterList: IInternalPerformanceCounterList;
begin
  inherited Create;
  VPerfCounterList := APerfCounterList;
  if not Assigned(VPerfCounterList) then begin
    VPerfCounterList := TInternalPerformanceCounterFake.Create;
  end;
  {$IFDEF NATIVE_LIBJPEG}
  FLoader := TLibJpegTileLoader.Create(VPerfCounterList);
  FSaver := nil;
  {$ELSE}
  FLoader := TVampyreBasicBitmapTileLoaderJPEG.Create(VPerfCounterList);
  FSaver := nil
  {$ENDIF}
end;

constructor TBitmapTileJpegLoadSave.Create(
  const ACompressionQuality: Byte;
  const APerfCounterList: IInternalPerformanceCounterList = nil
);
var
  VPerfCounterList: IInternalPerformanceCounterList;
begin
  inherited Create;
  VPerfCounterList := APerfCounterList;
  if not Assigned(VPerfCounterList) then begin
    VPerfCounterList := TInternalPerformanceCounterFake.Create;
  end;
  {$IFDEF NATIVE_LIBJPEG}
  FLoader :=nil;
  FSaver := TLibJpegTileSaver.Create(ACompressionQuality, VPerfCounterList);
  {$ELSE}
  FLoader := nil;
  FSaver := TVampyreBasicBitmapTileSaverJPG.Create(ACompressionQuality, VPerfCounterList);
  {$ENDIF}
end;

destructor TBitmapTileJpegLoadSave.Destroy;
begin
  FLoader := nil;
  FSaver := nil;
  inherited Destroy;
end;

function TBitmapTileJpegLoadSave.Load(const AData: IBinaryData): IBitmap32Static;
begin
  Result := FLoader.Load(AData);
end;

function TBitmapTileJpegLoadSave.Save(const ABitmap: IBitmap32Static): IBinaryData;
begin
  Result := FSaver.Save(ABitmap);
end;

procedure TBitmapTileJpegLoadSave.SaveToStream(
  ABtm: TCustomBitmap32;
  AStream: TStream
);
begin
  FSaver.SaveToStream(ABtm, AStream);
end;

end.

