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

unit u_BerkeleyDBValue;

interface

uses
  i_ContentTypeInfo,
  i_MapVersionInfo,
  i_BerkeleyDBKeyValue,
  u_BaseInterfacedObject;

type
  TBerkeleyDBValueBase = class(TBaseInterfacedObject, IBerkeleyDBKeyValueBase)
  protected
    FData: PByte;
    FSize: Integer;
    FOwnMem: Boolean;
  private
    { IBerkeleyDBKeyValueBase }
    function GetData: Pointer;
    function GetSize: Integer;
    procedure Assign(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TBerkeleyDBMetaValue = class(TBerkeleyDBValueBase, IBerkeleyDBMetaValue)
  private
    type
      TMetaValue = record
        MetaMagic: array [0..3] of AnsiChar;
        MetaCRC32: Cardinal;
        StorageEPSG: Integer;
      end;
      PMetaValue = ^TMetaValue;
  private
    FMetaValue: PMetaValue;
    procedure DataToMetaValue(const AStorageEPSG: Integer);
    procedure Assign(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ); override;
  private
    { IBerkeleyDBMetaValue }
    function GetStorageEPSG: Integer;
  public
    constructor Create(const AStorageEPSG: Integer);
    destructor Destroy; override;
  end;

  TBerkeleyDBValue = class(TBerkeleyDBValueBase, IBerkeleyDBValue)
  private
    type
      TValue = record
        RecMagic: array [0..3] of AnsiChar;
        RecCRC32: Cardinal;
        TileSize: Cardinal;
        TileDate: TDateTime;
        TileVersion: WideString;
        TileContentType: WideString;
        TileBody: PByte;
      end;
      PValue = ^TValue;
  private
    FValue: PValue;
    procedure TileToValue(
      const ATileBody: Pointer;
      const ATileSize: Integer;
      const ATileDate: TDateTime;
      const ATileVersionInfo: IMapVersionInfo;
      const ATileContentType: IContentTypeInfoBasic
    );
    procedure Assign(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ); override;
  private
    { IBerkeleyDBValue }
    function GetTileBody: Pointer;
    function GetTileSize: Integer;
    function GetTileDate: TDateTime;
    function GetTileVersionInfo: WideString;
    function GetTileContentType: WideString;
  public
    constructor Create(
      const ATileBody: Pointer;
      const ATileSize: Integer;
      const ATileDate: TDateTime;
      const ATileVersionInfo: IMapVersionInfo;
      const ATileContentType: IContentTypeInfoBasic
    ); overload;
    constructor Create(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ); overload;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  CRC32;

const
  cWideCharEndLine: WideChar = #0000;

  cMetaValueVersion = #01;
  cMetaValueMagic: array [0..3] of AnsiChar = ('M', 'I', 'D', cMetaValueVersion); // Meta Info Data

  cValueVersion = #03;
  cValueMagic: array [0..3] of AnsiChar = ('T', 'L', 'D', cValueVersion);         // TiLe Data

{ TBerkeleyDBValueBase }

constructor TBerkeleyDBValueBase.Create;
begin
  inherited Create;
  FData := nil;
  FSize := 0;
  FOwnMem := False;
end;

destructor TBerkeleyDBValueBase.Destroy;
begin
  if FOwnMem and Assigned(FData) then begin
    FreeMemory(FData);
  end;
  inherited Destroy;
end;

function TBerkeleyDBValueBase.GetData: Pointer;
begin
  Result := FData;
end;

function TBerkeleyDBValueBase.GetSize: Integer;
begin
  Result := FSize;
end;

{ TBerkeleyDBMetaValue }

constructor TBerkeleyDBMetaValue.Create(const AStorageEPSG: Integer);
begin
  inherited Create;
  FMetaValue := nil;
  DataToMetaValue(AStorageEPSG);
  FOwnMem := True;
end;

destructor TBerkeleyDBMetaValue.Destroy;
begin
  if Assigned(FMetaValue) then begin
    Dispose(FMetaValue);
  end;
  inherited Destroy;
end;

procedure TBerkeleyDBMetaValue.DataToMetaValue(const AStorageEPSG: Integer);
begin
  if Assigned(FMetaValue) then begin
    Dispose(FMetaValue);
  end;

  New(FMetaValue);

  FSize :=
    Length(FMetaValue.MetaMagic) +
    SizeOf(FMetaValue.MetaCRC32) +
    SizeOf(FMetaValue.StorageEPSG);

  Move(cMetaValueMagic[0], FMetaValue.MetaMagic[0], Length(FMetaValue.MetaMagic));
  FMetaValue.MetaCRC32 := 0;
  FMetaValue.StorageEPSG := AStorageEPSG;
  FMetaValue.MetaCRC32 := CRC32Buf(Pointer(FMetaValue), FSize);

  if FOwnMem and Assigned(FData) then begin
    FreeMemory(FData);
  end;
  FData := GetMemory(FSize);
  Move(FMetaValue^, FData^, FSize);
end;

procedure TBerkeleyDBMetaValue.Assign(
  const AData: Pointer;
  const ASize: Integer;
  const AOwnMem: Boolean
);
var
  VPtr: PByte;
  VCRC32Ptr: PByte;
begin
  if Assigned(FMetaValue) then begin
    Dispose(FMetaValue);
  end;

  if FOwnMem and Assigned(FData) then begin
    FreeMemory(FData);
  end;

  FData := AData;
  FSize := ASize;
  FOwnMem := AOwnMem;

  New(FMetaValue);

  VPtr := AData;

  Move(VPtr^, FMetaValue.MetaMagic[0], Length(FMetaValue.MetaMagic));
  Inc(VPtr, Length(FMetaValue.MetaMagic));
  if FMetaValue.MetaMagic = cMetaValueMagic then begin
    VCRC32Ptr := VPtr;
    FMetaValue.MetaCRC32 := 0;
    Inc(VPtr, SizeOf(FMetaValue.MetaCRC32));
    FMetaValue.StorageEPSG := PInteger(VPtr)^;
    FMetaValue.MetaCRC32 := CRC32Buf(Pointer(FMetaValue), ASize);
    if PCardinal(VCRC32Ptr)^ <> FMetaValue.MetaCRC32 then begin
      raise Exception.Create(
        'Error [BerkeleyDB MetaValue]: Bad CRC32 value: 0x' + IntToHex(FMetaValue.MetaCRC32, 8)
      );
    end;
  end;
end;

function TBerkeleyDBMetaValue.GetStorageEPSG: Integer;
begin
  if Assigned(FMetaValue) then begin
    Result := FMetaValue.StorageEPSG;
  end else begin
    Result := -1;
  end;
end;

{ TBerkeleyDBValue }

constructor TBerkeleyDBValue.Create(
  const ATileBody: Pointer;
  const ATileSize: Integer;
  const ATileDate: TDateTime;
  const ATileVersionInfo: IMapVersionInfo;
  const ATileContentType: IContentTypeInfoBasic
);
begin
  inherited Create;
  FValue := nil;
  TileToValue(
    ATileBody,
    ATileSize,
    ATileDate,
    ATileVersionInfo,
    ATileContentType
  );
  FOwnMem := True;
end;

constructor TBerkeleyDBValue.Create(
  const AData: Pointer;
  const ASize: Integer;
  const AOwnMem: Boolean
);
begin
  inherited Create;
  FValue := nil;
  Assign(AData, ASize, AOwnMem);
end;

destructor TBerkeleyDBValue.Destroy;
begin
  if Assigned(FValue) then begin
    Dispose(FValue);
  end;
  inherited Destroy;
end;

procedure TBerkeleyDBValue.TileToValue(
  const ATileBody: Pointer;
  const ATileSize: Integer;
  const ATileDate: TDateTime;
  const ATileVersionInfo: IMapVersionInfo;
  const ATileContentType: IContentTypeInfoBasic
);
var
  VPtr: PByte;
  VLen: Integer;
  VCRC32Ptr: PByte;
begin
  if Assigned(FValue) then begin
    Dispose(FValue);
  end;

  New(FValue);

  Move(cValueMagic[0], FValue.RecMagic[0], Length(FValue.RecMagic));
  FValue.RecCRC32 := 0;
  FValue.TileSize := ATileSize;
  FValue.TileDate := ATileDate;
  FValue.TileBody := ATileBody;

  if Assigned(ATileVersionInfo) then begin
    FValue.TileVersion := ATileVersionInfo.StoreString;
  end else begin
    FValue.TileVersion := '';
  end;

  if Assigned(ATileContentType) then begin
    FValue.TileContentType := ATileContentType.GetContentType;
  end else begin
    FValue.TileContentType := '';
  end;

  FSize :=
    Length(FValue.RecMagic) +
    SizeOf(FValue.RecCRC32) +
    SizeOf(FValue.TileSize) +
    SizeOf(FValue.TileDate) +
    (Length(FValue.TileVersion) + Length(cWideCharEndLine)) * SizeOf(WideChar) +
    (Length(FValue.TileContentType) + Length(cWideCharEndLine)) * SizeOf(WideChar) +
    ATileSize;

  if FOwnMem and Assigned(FData) then begin
    FreeMemory(FData);
  end;
  FData := GetMemory(FSize);
  VPtr := FData;

  // magic
  VLen := Length(FValue.RecMagic);
  Move(FValue.RecMagic[0], VPtr^, VLen);
  Inc(VPtr, VLen);

  // init crc32 with zero value
  VCRC32Ptr := VPtr;
  VLen := SizeOf(FValue.RecCRC32);
  PInteger(VCRC32Ptr)^ := FValue.RecCRC32;
  Inc(VPtr, VLen);

  // tile size
  VLen := SizeOf(FValue.TileSize);
  PInteger(VPtr)^ := FValue.TileSize;
  Inc(VPtr, VLen);

  // tile date
  VLen := SizeOf(FValue.TileDate);
  PDateTime(VPtr)^ := FValue.TileDate;
  Inc(VPtr, VLen);

  // tile version
  VLen := Length(FValue.TileVersion) * SizeOf(WideChar);
  if VLen > 0 then begin
    Move(PWideChar(FValue.TileVersion)^, VPtr^, VLen);
    Inc(VPtr, VLen);
  end;
  VLen := Length(cWideCharEndLine) * SizeOf(WideChar);
  Move(cWideCharEndLine, VPtr^, VLen);
  Inc(VPtr, VLen);

  // tile content-type
  VLen := Length(FValue.TileContentType) * SizeOf(WideChar);
  if VLen > 0 then begin
    Move(PWideChar(FValue.TileContentType)^, VPtr^, VLen);
    Inc(VPtr, VLen);
  end;
  VLen := Length(cWideCharEndLine) * SizeOf(WideChar);
  Move(cWideCharEndLine, VPtr^, VLen);
  Inc(VPtr, VLen);

  // tile body
  if (ATileSize > 0) and (ATileBody <> nil) then begin
    Move(ATileBody^, VPtr^, ATileSize);
  end;

  // calc and save CRC32
  FValue.RecCRC32 := CRC32Buf(Pointer(FData), FSize);
  PInteger(VCRC32Ptr)^ := FValue.RecCRC32;
end;

procedure TBerkeleyDBValue.Assign(
  const AData: Pointer;
  const ASize: Integer;
  const AOwnMem: Boolean
);
var
  VPtr: PByte;
  VCRC32: Cardinal;
begin
  if Assigned(FValue) then begin
    Dispose(FValue);
  end;

  if FOwnMem and Assigned(FData) then begin
    FreeMemory(FData);
  end;

  FData := AData;
  FSize := ASize;
  FOwnMem := AOwnMem;

  New(FValue);

  VPtr := AData;

  Move(VPtr^, FValue.RecMagic[0], Length(FValue.RecMagic));
  Inc(VPtr, Length(FValue.RecMagic));

  if FValue.RecMagic = cValueMagic then begin
    FValue.RecCRC32 := PCardinal(VPtr)^;
    PCardinal(VPtr)^ := 0;

    VCRC32 := CRC32Buf(AData, ASize);

    PCardinal(VPtr)^ := FValue.RecCRC32;
    if VCRC32 = FValue.RecCRC32 then begin
      Inc(VPtr, SizeOf(FValue.RecCRC32));

      FValue.TileSize := PCardinal(VPtr)^;
      Inc(VPtr, SizeOf(FValue.TileSize));

      FValue.TileDate := PDateTime(VPtr)^;
      Inc(VPtr, SizeOf(FValue.TileDate));

      FValue.TileVersion := PWideChar(VPtr);
      Inc(VPtr, (Length(FValue.TileVersion) + 1) * SizeOf(WideChar));

      FValue.TileContentType := PWideChar(VPtr);
      Inc(VPtr, (Length(FValue.TileContentType) + 1) * SizeOf(WideChar));

      if FValue.TileSize > 0 then begin
        FValue.TileBody := VPtr;
      end else begin
        FValue.TileBody := nil;
      end;
    end else begin
      raise Exception.Create(
        'Error [BerkeleyDB Value]: Bad CRC32 value: 0x' + IntToHex(FValue.RecCRC32, 8)
      );
    end;
  end else begin
    raise Exception.Create(
      AnsiString('Error [BerkeleyDB Value]: Bad magic value (') +
      FValue.RecMagic[0] +
      FValue.RecMagic[1] +
      FValue.RecMagic[2] +
      FValue.RecMagic[3] +
      ')'
    );
  end;
end;

function TBerkeleyDBValue.GetTileBody: Pointer;
begin
  if Assigned(FValue) then begin
    Result := FValue.TileBody;
  end else begin
    Result := nil;
  end;
end;

function TBerkeleyDBValue.GetTileSize: Integer;
begin
  if Assigned(FValue) then begin
    Result := FValue.TileSize;
  end else begin
    Result := 0;
  end;
end;

function TBerkeleyDBValue.GetTileDate: TDateTime;
begin
  if Assigned(FValue) then begin
    Result := FValue.TileDate;
  end else begin
    Result := 0;
  end;
end;

function TBerkeleyDBValue.GetTileVersionInfo: WideString;
begin
  if Assigned(FValue) then begin
    Result := FValue.TileVersion;
  end else begin
    Result := '';
  end;
end;

function TBerkeleyDBValue.GetTileContentType: WideString;
begin
  if Assigned(FValue) then begin
    Result := FValue.TileContentType;
  end else begin
    Result := '';
  end;
end;

end.
