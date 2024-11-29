{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_ExternalTerrainFile;

interface

uses
  Windows,
  Types,
  Math,
  u_ByteSwapFunc,
  u_ElevationValue,
  u_ElevationReader,
  u_ElevationReaderRAW,
  u_ElevationReaderTIFF;

type
  TExternalTerrainFile = class
  private
    FFileName: string;
    FFileHandle: THandle;

    FRowsCount: Integer;
    FColsCount: Integer;

    FByteOrder: Integer;
    FVoidValue: Integer;

    FIsFileOk: Boolean;

    FTiffReader: TElevationReaderTIFF;
    FRawReader: TElevationReaderRAW;

    FElevationReader: TElevationReader;

    procedure InternalClose;
  public
    function Open(
      const AFileName: string;
      const ARowsCount: Integer;
      const AColsCount: Integer
    ): Boolean;

    function FindElevation(
      ARow, ACol: Integer;
      out AElevation: Single
    ): Boolean; inline;
  public
    constructor Create(
      const AByteOrder: Integer;
      const AVoidValue: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  StrUtils,
  SysUtils,
  NTFiles;

{ TExternalTerrainFile }

constructor TExternalTerrainFile.Create(
  const AByteOrder: Integer;
  const AVoidValue: Integer
);
begin
  inherited Create;

  FByteOrder := AByteOrder;
  FVoidValue := AVoidValue;

  FFileHandle := 0;
  FFileName := '';
  FIsFileOk := False;

  FTiffReader := nil;
  FRawReader := nil;
  FElevationReader := nil;
end;

destructor TExternalTerrainFile.Destroy;
begin
  InternalClose;
  FreeAndNil(FTiffReader);
  FreeAndNil(FRawReader);
  inherited Destroy;
end;

function TExternalTerrainFile.Open(
  const AFileName: string;
  const ARowsCount: Integer;
  const AColsCount: Integer
): Boolean;
var
  VParams: TElevationReaderParams;
begin
  // don't try reopen same file on failue
  if not FIsFileOk and (FFileName = AFileName) then begin
    Result := False;
    Exit;
  end;

  // if file not opened or opened another file - open this
  if (FFileHandle = 0) or (FFileName <> AFileName) then begin
    InternalClose;

    FFileName := AFileName;
    FRowsCount := ARowsCount;
    FColsCount := AColsCount;

    // open file
    FFileHandle := CreateFile(PChar(FFileName), GENERIC_READ, FILE_SHARE_READ,
      nil, OPEN_EXISTING, 0, 0);

    FIsFileOk := FFileHandle <> INVALID_HANDLE_VALUE;
    if not FIsFileOk then begin
      FFileHandle := 0;
      Result := False;
      Exit;
    end;

    if MatchText(ExtractFileExt(FFileName), ['.tif', '.tiff']) then begin
      if FTiffReader = nil then begin
        FTiffReader := TElevationReaderTIFF.Create;
      end;
      FElevationReader := FTiffReader;
    end else begin
      if FRawReader = nil then begin
        FRawReader := TElevationReaderRAW.Create;
      end;
      FElevationReader := FRawReader;
    end;

    with VParams do begin
      FileName := FFileName;
      FileHandle := FFileHandle;
      RowsCount := FRowsCount;
      ColsCount := FColsCount;
    end;

    FIsFileOk := FElevationReader.Open(VParams);
  end;

  Result := FIsFileOk;
end;

procedure TExternalTerrainFile.InternalClose;
begin
  FFileName := '';
  FIsFileOk := False;

  if FFileHandle <> 0 then begin
    CloseHandle(FFileHandle);
    FFileHandle := 0;
  end;
end;

function TExternalTerrainFile.FindElevation(
  ARow, ACol: Integer;
  out AElevation: Single
): Boolean;
var
  VElevationValue: TElevationValue;
begin
  Assert(ARow >= 0);
  Assert(ARow < FRowsCount);

  Assert(ACol >= 0);
  Assert(ACol < FColsCount);

  //ARow := Max(0, ARow);
  //ARow := Min(ARow, FRowsCount-1);

  //ACol := Max(0, ACol);
  //ACol := Min(ACol, FColsCount-1);

  // rows stored from top to bottom
  ARow := (FRowsCount - 1) - ARow;

  Result := FElevationReader.ReadElevationValue(ARow, ACol, VElevationValue);

  if not Result then begin
    // fatal error
    FIsFileOk := False;
    Exit;
  end;

  // check byte inversion
  if FByteOrder <> 0 then begin
    VElevationValue.SwapIntByteOrder;
  end;

  // check voids
  if VElevationValue.IsVoid(FVoidValue) then begin
    Result := False;
    Exit;
  end;

  // ok
  AElevation := VElevationValue.ToSingle;
end;

end.
