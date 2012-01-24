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

unit u_TileStorageBerkeleyDBHelper;

interface

uses
  Types,
  Classes,
  u_BerkeleyDB,
  u_BerkeleyDBEnv,
  u_BerkeleyDBPool;

type
  PBDBKey = ^TBDBKey;
  TBDBKey = packed record
    TileX : Cardinal;
    TileY : Cardinal;
  end;

  PBDBData = ^TBDBData;
  TBDBData = record
    RecMagic  : array [0..3] of AnsiChar;
    RecCRC32  : Cardinal;
    TileSize  : Cardinal;
    TileDate  : TDateTime;
    TileVer   : PWideChar;
    TileMIME  : PWideChar;
    TileBody  : PByte;
  end;

  PBDBStorageMetaInfo = ^TBDBStorageMetaInfo;
  TBDBStorageMetaInfo = record
    MetaMagic     : array [0..3] of AnsiChar;
    MetaCRC32     : Cardinal;
    StorageEPSG   : Integer;
  end;

  TTileStorageBerkeleyDBHelper = class(TObject)
  private
    FEnv: TBerkeleyDBEnv;
    FPool: TBerkeleyDBPool;
    FStorageEPSG: Integer;
    function OnBDBObjCreate(const AFileName: string): TBerkeleyDB;
    procedure OnBDBFileCreate(Sender: TObject);
    procedure OnBDBFileFirstOpen(Sender: TObject);
  public
    constructor Create(
      const AStorageRootPath: string;
      AStorageEPSG: Integer
    );
    destructor Destroy; override;
    class procedure OnBDBClose(Sender: TObject);
    property Environment: TBerkeleyDBEnv read FEnv write FEnv;
    property Pool: TBerkeleyDBPool read FPool write FPool;
  end;

function PointToKey(APoint: TPoint): TBDBKey;

function PBDBDataToMemStream(AData: PBDBData; out AStream: TMemoryStream): Boolean;
function PRawDataToPBDBData(ARawData: PByte; ARawDataSize: Cardinal; AData: PBDBData): Boolean;

function PBDBMetaInfoToMemStream(AMeta: PBDBStorageMetaInfo; out AStream: TMemoryStream): Boolean;
function PRawMetaToPBDBMetaInfo(ARawData: PByte; ARawDataSize: Cardinal; AMeta: PBDBStorageMetaInfo): Boolean;

implementation

uses
  SysUtils,
  CRC32;

const
  CBDBMetaVersion = #01;
  CBDBMetaMagic: array [0..3] of AnsiChar = ('M', 'I', 'D', CBDBMetaVersion);    //Meta Info Data

  CBDBMetaKeyX : Cardinal = $FFFFFFFF;
  CBDBMetaKeyY : Cardinal = $FFFFFFFF;

  CBDBRecVersion = #03;
  CBDBRecMagic: array [0..3] of AnsiChar = ('T', 'L', 'D', CBDBRecVersion);      //TiLe Data

  CPageSize = 1024; // 1k
  CCacheSize = BDB_DEF_CACHE_SIZE; //256k

function PointToKey(APoint: TPoint): TBDBKey;

  procedure SetBit(var ADest: Cardinal; ABit: Integer); inline;
  begin
    ADest := ADest or (Cardinal(1) shl ABit);
  end;

  function Swap32(Value: Cardinal): Cardinal; assembler;
  asm
    bswap eax
  end;

var
  I: Integer;
  VSetX, VSetY: Boolean;
begin
  Result.TileX := 0;
  Result.TileY := 0;
  for I := 0 to 31 do begin
    VSetX := ((APoint.X shr I) and 1) = 1;
    VSetY := ((APoint.Y shr I) and 1) = 1;
    if I <= 15 then begin
      if VSetX then begin
        SetBit(Result.TileY, I*2);
      end;
      if VSetY then begin
        SetBit(Result.TileY, I*2+1);
      end;
    end else begin
      if VSetX then begin
        SetBit(Result.TileX, (I-16)*2);
      end;
      if VSetY then begin
        SetBit(Result.TileX, (I-16)*2+1);
      end;
    end;
  end;
  Result.TileX := Swap32(Result.TileX);
  Result.TileY := Swap32(Result.TileY);
end;

procedure WideCharToStream(APWideChar: PWideChar; AStream: TMemoryStream);
const
  CEndLine: WideChar = #0000;
begin
  if (APWideChar <> nil) and (Length(APWideChar) > 0) then begin
    AStream.WriteBuffer( APWideChar^, Length(APWideChar)*SizeOf(WideChar) );
  end;
  AStream.WriteBuffer( CEndLine , SizeOf(CEndLine) );
end;

function PBDBDataToMemStream(
  AData: PBDBData;
  out AStream: TMemoryStream
): Boolean;
begin
  Result := False;
  if (AData <> nil) and Assigned(AStream) then begin
    AStream.Clear;
    AStream.Position := 0;
    AData.RecCRC32 := 0;
    Move(CBDBRecMagic[0], AData.RecMagic[0], Length(AData.RecMagic));
    AStream.WriteBuffer(AData.RecMagic[0], Length(AData.RecMagic));
    AStream.WriteBuffer(AData.RecCRC32, SizeOf(AData.RecCRC32));
    AStream.WriteBuffer(AData.TileSize, SizeOf(AData.TileSize));
    AStream.WriteBuffer(AData.TileDate, SizeOf(AData.TileDate));
    WideCharToStream(AData.TileVer, AStream);
    WideCharToStream(AData.TileMIME, AStream);
    if (AData.TileSize > 0) and (AData.TileBody <> nil) then begin
      AStream.WriteBuffer(AData.TileBody^, AData.TileSize);
    end;
    AStream.Position := 0;
    AData.RecCRC32 := CRC32Buf(AStream.Memory, AStream.Size);
    AStream.Position := Length(AData.RecMagic);
    AStream.WriteBuffer(AData.RecCRC32, SizeOf(AData.RecCRC32));
    AStream.Position := 0;
    Result := True;
  end;
end;

function PRawDataToPBDBData(
  ARawData: PByte;
  ARawDataSize: Cardinal;
  AData: PBDBData
): Boolean;
var
  PRawData: PByte;
  VCRC32: Cardinal;
begin
  Result := False;
  if (AData <> nil) and (ARawData <> nil) then begin
    FillChar(AData^, SizeOf(TBDBData), 0);
    PRawData := ARawData;

    Move(PRawData^, AData.RecMagic[0], Length(AData.RecMagic));
    Inc(PRawData, Length(AData.RecMagic));

    if AData.RecMagic = CBDBRecMagic then begin
      AData.RecCRC32 := PCardinal(PRawData)^;
      PCardinal(PRawData)^ := 0;

      VCRC32 := CRC32Buf(ARawData, ARawDataSize);

      PCardinal(PRawData)^ := AData.RecCRC32;
      if VCRC32 = AData.RecCRC32 then begin
        Inc(PRawData, SizeOf(AData.RecCRC32));

        AData.TileSize := PCardinal(PRawData)^;
        Inc(PRawData, SizeOf(AData.TileSize));

        AData.TileDate := PDateTime(PRawData)^;
        Inc(PRawData, SizeOf(AData.TileDate));

        AData.TileVer := PWideChar(PRawData);
        Inc(PRawData, (Length(AData.TileVer) + 1) * SizeOf(WideChar));

        AData.TileMIME := PWideChar(PRawData);
        Inc(PRawData, (Length(AData.TileMIME) + 1) * SizeOf(WideChar));

        if AData.TileSize > 0 then begin
          AData.TileBody := PRawData;
        end;

        Result := True;
      end;
    end else begin
      raise EBerkeleyDBExeption.Create(
        'Error [BerkeleyDB]: Bad magic value (' +
        AData.RecMagic[0] +
        AData.RecMagic[1] +
        AData.RecMagic[2] +
        AData.RecMagic[3] +
        ')'
      );
    end;
  end;
end;

function PBDBMetaInfoToMemStream(
  AMeta: PBDBStorageMetaInfo;
  out AStream: TMemoryStream
): Boolean;
begin
  Move(CBDBMetaMagic[0], AMeta.MetaMagic[0], Length(AMeta.MetaMagic));
  AMeta.MetaCRC32 := 0;
  AStream.Clear;
  AStream.Position := 0;
  AStream.WriteBuffer(AMeta.MetaMagic[0], Length(AMeta.MetaMagic));
  AStream.WriteBuffer(AMeta.MetaCRC32, SizeOf(AMeta.MetaCRC32));
  AStream.WriteBuffer(AMeta.StorageEPSG, SizeOf(AMeta.StorageEPSG));
  AStream.Position := 0;
  AMeta.MetaCRC32 := CRC32Buf(AStream.Memory, AStream.Size);
  AStream.Position := Length(AMeta.MetaMagic);
  AStream.WriteBuffer(AMeta.MetaCRC32, SizeOf(AMeta.MetaCRC32));
  AStream.Position := 0;
  Result := True;
end;

function PRawMetaToPBDBMetaInfo(
  ARawData: PByte;
  ARawDataSize: Cardinal;
  AMeta: PBDBStorageMetaInfo
): Boolean;
var
  PRawData: PByte;
  VCRC32: Cardinal;
begin
  Result := False;
  if (AMeta <> nil) and (ARawData <> nil) then begin
    FillChar(AMeta^, SizeOf(TBDBStorageMetaInfo), 0);
    PRawData := ARawData;
    Move(PRawData^, AMeta.MetaMagic[0], Length(AMeta.MetaMagic));
    Inc(PRawData, Length(AMeta.MetaMagic));
    if AMeta.MetaMagic = CBDBMetaMagic then begin
      AMeta.MetaCRC32 := PCardinal(PRawData)^;
      PCardinal(PRawData)^ := 0;
      VCRC32 := CRC32Buf(ARawData, ARawDataSize);
      PCardinal(PRawData)^ := AMeta.MetaCRC32;
      if VCRC32 = AMeta.MetaCRC32 then begin
        Inc(PRawData, SizeOf(AMeta.MetaCRC32));
        AMeta.StorageEPSG := PInteger(PRawData)^;
        Result := True;
      end;
    end;
  end;
end;

{ TTileStorageBerkeleyDBHelper }

constructor TTileStorageBerkeleyDBHelper.Create(
  const AStorageRootPath: string;
  AStorageEPSG: Integer
);
begin
  inherited Create;
  FStorageEPSG := AStorageEPSG;
  FEnv := GlobalAllocateEnvironment(AStorageRootPath);
  FPool := FEnv.Pool;
  FPool.OnObjCreate := Self.OnBDBObjCreate;
end;

destructor TTileStorageBerkeleyDBHelper.Destroy;
begin
  FPool := nil;
  inherited Destroy;
end;

function TTileStorageBerkeleyDBHelper.OnBDBObjCreate(
  const AFileName: string
): TBerkeleyDB;
var
  VBDB: TBerkeleyDB;
  VMeta: PBDBStorageMetaInfo;
begin
  New(VMeta);
  try
    VBDB := TBerkeleyDB.Create;
    VBDB.OnCreate := Self.OnBDBFileCreate;
    VBDB.OnOpen := Self.OnBDBFileFirstOpen;
    VBDB.OnCheckPoint := FEnv.CheckPoint;
    VBDB.OnClose := TTileStorageBerkeleyDBHelper.OnBDBClose;

    FillChar(VMeta^, SizeOf(TBDBStorageMetaInfo), 0);
    VBDB.AppData := VMeta;

    if VBDB.Open(FEnv.EnvPtr, AFileName, CPageSize, CCacheSize) then begin
      Result := VBDB;
    end else begin
      raise EBerkeleyDBExeption.Create(
        'Error [BerkeleyDB]: Can''t open file: ' + AFileName
      );
    end;
  except
    Dispose(VMeta);
    FreeAndNil(VBDB);
    raise;
  end;
end;

procedure TTileStorageBerkeleyDBHelper.OnBDBFileCreate(Sender: TObject);
var
  VKey: TBDBKey;
  VMeta: TBDBStorageMetaInfo;
  VMemStream: TMemoryStream;
  VBDB: TBerkeleyDB;
begin
  if Sender is TBerkeleyDB then begin
    VBDB := Sender as TBerkeleyDB;
    if Assigned(VBDB) then begin
      VMemStream := TMemoryStream.Create;
      try
        VKey := PointToKey(Point(CBDBMetaKeyX, CBDBMetaKeyY));
        VMeta.StorageEPSG := FStorageEPSG;
        if PBDBMetaInfoToMemStream(@VMeta, VMemStream) then begin
          VMemStream.Position := 0;
          VBDB.Write(@VKey, SizeOf(TBDBKey), VMemStream.Memory, VMemStream.Size);
        end;
      finally
        VMemStream.Free;
      end;
    end;
  end;
end;

procedure TTileStorageBerkeleyDBHelper.OnBDBFileFirstOpen(Sender: TObject);
var
  VKey: TBDBKey;
  VBDB: TBerkeleyDB;
  VRawMetaData: Pointer;
  VRawMetaDataSize: Cardinal;
  VMeta: PBDBStorageMetaInfo;
begin
  if Sender is TBerkeleyDB then begin
    VBDB := Sender as TBerkeleyDB;
    if Assigned(VBDB) then begin
      VKey := PointToKey(Point(CBDBMetaKeyX, CBDBMetaKeyY));
      if VBDB.Read(@VKey, SizeOf(TBDBKey), VRawMetaData, VRawMetaDataSize) then
      try
        VMeta := VBDB.AppData;
        if VMeta <> nil then begin
          PRawMetaToPBDBMetaInfo(VRawMetaData, VRawMetaDataSize, VMeta);
        end;
      finally
        if VRawMetaData <> nil then begin
          FreeMem(VRawMetaData);
        end;
      end;
    end;
  end;
end;

class procedure TTileStorageBerkeleyDBHelper.OnBDBClose(Sender: TObject);
var
  VBDB: TBerkeleyDB;
  VMeta: PBDBStorageMetaInfo;
begin
  if Sender is TBerkeleyDB then begin
    VBDB := Sender as TBerkeleyDB;
    if Assigned(VBDB) then begin
      VMeta := VBDB.AppData;
      if VMeta <> nil then begin
        Dispose(VMeta);
        VBDB.AppData := nil;
      end;
    end;
  end;
end;

end.
