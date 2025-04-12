{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_FreeImageFormatIdProvider;

interface

uses
  SyncObjs,
  FreeImage,
  i_FreeImageFormatIdProvider,
  u_BaseInterfacedObject;

type
  TFreeImageFormatIdProvider = class(TBaseInterfacedObject, IFreeImageFormatIdProvider)
  private
    FLock: TCriticalSection;
    FIsInitialized: Boolean;
    procedure LazyInit;
  private
    FBmpFormatId: FREE_IMAGE_FORMAT;
    FIcoFormatId: FREE_IMAGE_FORMAT;
    FGifFormatId: FREE_IMAGE_FORMAT;
    FPngFormatId: FREE_IMAGE_FORMAT;
    FTiffFormatId: FREE_IMAGE_FORMAT;
    FWebpFormatId: FREE_IMAGE_FORMAT;
  private
    { IFreeImageFormatIdProvider }
    function GetBmpFormatId: FREE_IMAGE_FORMAT;
    function GetIcoFormatId: FREE_IMAGE_FORMAT;
    function GetGifFormatId: FREE_IMAGE_FORMAT;
    function GetPngFormatId: FREE_IMAGE_FORMAT;
    function GetTiffFormatId: FREE_IMAGE_FORMAT;
    function GetWebpFormatId: FREE_IMAGE_FORMAT;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_GlobalDllName;

{ TFreeImageFormatIdProvider }

constructor TFreeImageFormatIdProvider.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FIsInitialized := False;
end;

destructor TFreeImageFormatIdProvider.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TFreeImageFormatIdProvider.LazyInit;

  function _GetFormatId(const AFormat: PAnsiChar): FREE_IMAGE_FORMAT;
  begin
    Result := FreeImage_GetFIFFromFormat(AFormat);
    Assert(Result <> FIF_UNKNOWN, '[FreeImage] Unsupported format: ' + string(AFormat));
  end;

begin
  FLock.Acquire;
  try
    if FIsInitialized then begin
      Exit;
    end;

    InitFreeImageLib(GDllName.FreeImage);

    // The predefined FREE_IMAGE_FORMAT constants (e.g., FIF_BMP, FIF_ICO, etc.) from the FreeImage.pas unit
    // should not be used as fixed values.

    // In scenarios where the FreeImage DLL is built with certain formats disabled - particularly when
    // the format exclusion is not backward-compatible (as observed in the MSYS2 build) - using these constants
    // may result in incorrect format selection. See issue: https://www.sasgis.org/mantis/view.php?id=3911

    // To avoid this, the IDs of the required formats are determined dynamically at runtime.

    FBmpFormatId  := _GetFormatId('BMP');
    FIcoFormatId  := _GetFormatId('ICO');
    FGifFormatId  := _GetFormatId('GIF');
    FPngFormatId  := _GetFormatId('PNG');
    FTiffFormatId := _GetFormatId('TIFF');
    FWebpFormatId := _GetFormatId('WEBP');

    FIsInitialized := True;
  finally
    FLock.Release;
  end;
end;

function TFreeImageFormatIdProvider.GetBmpFormatId: FREE_IMAGE_FORMAT;
begin
  if not FIsInitialized then begin
    LazyInit;
  end;
  Result := FBmpFormatId;
end;

function TFreeImageFormatIdProvider.GetGifFormatId: FREE_IMAGE_FORMAT;
begin
  if not FIsInitialized then begin
    LazyInit;
  end;
  Result := FGifFormatId;
end;

function TFreeImageFormatIdProvider.GetIcoFormatId: FREE_IMAGE_FORMAT;
begin
  if not FIsInitialized then begin
    LazyInit;
  end;
  Result := FIcoFormatId;
end;

function TFreeImageFormatIdProvider.GetPngFormatId: FREE_IMAGE_FORMAT;
begin
  if not FIsInitialized then begin
    LazyInit;
  end;
  Result := FPngFormatId;
end;

function TFreeImageFormatIdProvider.GetTiffFormatId: FREE_IMAGE_FORMAT;
begin
  if not FIsInitialized then begin
    LazyInit;
  end;
  Result := FTiffFormatId;
end;

function TFreeImageFormatIdProvider.GetWebpFormatId: FREE_IMAGE_FORMAT;
begin
  if not FIsInitialized then begin
    LazyInit;
  end;
  Result := FWebpFormatId;
end;

end.
