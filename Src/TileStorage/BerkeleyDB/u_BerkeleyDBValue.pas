{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_BerkeleyDBValue;

interface

uses
  SysUtils,
  i_BinaryData,
  i_ContentTypeInfo,
  i_MapVersionInfo,
  i_BerkeleyDBKeyValue,
  u_BaseInterfacedObject;

type
  EBerkeleyDBBadValue = class(Exception);

  TBerkeleyDBValueBase = class(TBaseInterfacedObject, IBerkeleyDBKeyValueBase, IBinaryData)
  protected
    FData: PByte;
    FSize: Integer;
    FOwnMem: Boolean;
  private
    { IBerkeleyDBKeyValueBase }
    function GetData: Pointer;
    function GetSize: Integer;
    function Assign(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ): Boolean; virtual; abstract;
    { IBinaryData }
    function IBinaryData.GetBuffer = GetData;
    function IBinaryData.GetSize = GetSize;
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
    function Assign(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ): Boolean; override;
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
    FBinData: IBinaryData;
    procedure TileToValue(
      const ATileBody: Pointer;
      const ATileSize: Integer;
      const ATileDate: TDateTime;
      const ATileVersionInfo: IMapVersionInfo;
      const ATileContentType: IContentTypeInfoBasic
    );
    function Assign(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ): Boolean; override;
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
    constructor Create(const AData: IBinaryData); overload;
    destructor Destroy; override;
  end;

  TBerkeleyDBVersionedMetaValueElement = class(TBerkeleyDBValueBase, IBerkeleyDBVersionedMetaValueElement)
  private
    type
      TVersionedMetaValueElement = record
        VersionID: Word;
        TileZOrder: Word;
        TileSize: Integer;
        TileDate: TDateTime;
        TileCRC: Cardinal;
        TileVersionInfo: WideString;
        TileContentType: WideString;
      end;
      PVersionedMetaValueElement = ^TVersionedMetaValueElement;
  private
    FValue: PVersionedMetaValueElement;
    procedure ValueToData;
  private
    { IBerkeleyDBVersionedMetaValueElement }
    function GetVersionID: Word;
    function GetTileZOrder: Word;
    function GetTileSize: Integer;
    function GetTileDate: TDateTime;
    function GetTileCRC: Cardinal;
    function GetTileVersionInfo: WideString;
    function GetTileContentType: WideString;
    { IBerkeleyDBKeyValueBase }
    function Assign(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ): Boolean; override;
  public
    constructor Create(
      const AVersionID: Word;
      const ATileZOrder: Word;
      const ATileSize: Integer;
      const ATileDate: TDateTime;
      const ATileCRC: Cardinal;
      const ATileVersionInfo: IMapVersionInfo;
      const ATileContentType: IContentTypeInfoBasic
    ); overload;
    constructor Create(
      const AData: Pointer
    ); overload;
    destructor Destroy; override;
  end;

  TBerkeleyDBVersionedMetaValue = class(TBerkeleyDBValueBase, IBerkeleyDBVersionedMetaValue)
  private
    type
      TVersionedMetaValue = record
        RecMagic: array [0..3] of AnsiChar;
        RecCRC32: Cardinal;
        ItemsCount: Word;
        ItemsArray: array of IBerkeleyDBVersionedMetaValueElement;
      end;
      PVersionedMetaValue = ^TVersionedMetaValue;
  private
    FValue: PVersionedMetaValue;
    FBinData: IBinaryData;
    procedure Alloc;
    procedure Clear;
    procedure ValueToData;
  private
    { IBerkeleyDBKeyValueBase }
    function Assign(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ): Boolean; override;
    { IBerkeleyDBVersionedMetaValue }
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): IBerkeleyDBVersionedMetaValueElement;
    function Add(const AItem: IBerkeleyDBVersionedMetaValueElement): Integer;
    procedure Replace(const AIndex: Integer; const AItem: IBerkeleyDBVersionedMetaValueElement);
    procedure Del(const AIndex: Integer);
  public
    constructor Create; overload;
    constructor Create(
      const AData: Pointer;
      const ASize: Integer;
      const AOwnMem: Boolean
    ); overload;
    constructor Create(
      const AData: IBinaryData
    ); overload;
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  CRC32,
  u_BerkeleyDBValueZlib;

const
  cWideCharEndLine: WideChar = #0000;

  cMetaValueVersion = #01;
  cMetaValueMagic: array [0..3] of AnsiChar = ('M', 'I', 'D', cMetaValueVersion); // Meta Info Data

  cVersionedMetaValueVersion = #01;
  cVersionedMetaValueMagic: array [0..3] of AnsiChar = ('M', 'V', 'I', cVersionedMetaValueVersion); // Meta Version Info

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
  inherited;
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
  inherited;
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

function TBerkeleyDBMetaValue.Assign(
  const AData: Pointer;
  const ASize: Integer;
  const AOwnMem: Boolean
): Boolean;
var
  VPtr: PByte;
  VCRC32Ptr: PByte;
begin
  Result := False;

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
      raise EBerkeleyDBBadValue.Create(
        'Read meta-value error - bad checksumm: 0x' + IntToHex(FMetaValue.MetaCRC32, 8)
      );
    end;
    Result := True;
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
  FBinData := nil;
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
  FBinData := nil;
  Assign(AData, ASize, AOwnMem);
end;

constructor TBerkeleyDBValue.Create(
  const AData: IBinaryData
);
begin
  inherited Create;
  Assign(AData.Buffer, AData.Size, False);
  FBinData := AData;
end;

destructor TBerkeleyDBValue.Destroy;
begin
  if Assigned(FValue) then begin
    Dispose(FValue);
  end;
  FBinData := nil;
  inherited;
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
    FValue.TileContentType := WideString(ATileContentType.GetContentType);
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
  FValue.RecCRC32 := 0;
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

function TBerkeleyDBValue.Assign(
  const AData: Pointer;
  const ASize: Integer;
  const AOwnMem: Boolean
): Boolean;
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

      if Integer(FValue.TileSize) < 0 then begin
        raise EBerkeleyDBBadValue.Create(
          'Read value error - bad TileSize: ' + IntToStr(FValue.TileSize)
        );
      end;

      FValue.TileDate := PDateTime(VPtr)^;
      Inc(VPtr, SizeOf(FValue.TileDate));

      if IsNan(FValue.TileDate) then begin
        raise EBerkeleyDBBadValue.Create(
          'Read value error - bad TileDate: NaN'
        );
      end;

      FValue.TileVersion := PWideChar(VPtr);
      Inc(VPtr, (Length(FValue.TileVersion) + 1) * SizeOf(WideChar));

      FValue.TileContentType := PWideChar(VPtr);
      Inc(VPtr, (Length(FValue.TileContentType) + 1) * SizeOf(WideChar));

      if FValue.TileSize > 0 then begin
        FValue.TileBody := VPtr;
      end else begin
        FValue.TileBody := nil;
      end;

      Result := True;
    end else begin
      raise EBerkeleyDBBadValue.Create(
        'Read value error - bad checksumm: 0x' + IntToHex(FValue.RecCRC32, 8)
      );
    end;
  end else begin
    raise EBerkeleyDBBadValue.Create(
      'Read value error - bad magic: 0x' + IntToHex(PCardinal(@FValue.RecMagic[0])^, 8)
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

{ TBerkeleyDBVersionedMetaValueElement }

constructor TBerkeleyDBVersionedMetaValueElement.Create(
  const AVersionID: Word;
  const ATileZOrder: Word;
  const ATileSize: Integer;
  const ATileDate: TDateTime;
  const ATileCRC: Cardinal;
  const ATileVersionInfo: IMapVersionInfo;
  const ATileContentType: IContentTypeInfoBasic
);
begin
  inherited Create;
  New(FValue);
  FValue.VersionID := AVersionID;
  FValue.TileZOrder := ATileZOrder;
  FValue.TileSize := ATileSize;
  FValue.TileDate := ATileDate;
  FValue.TileCRC := ATileCRC;

  if Assigned(ATileVersionInfo) then begin
    FValue.TileVersionInfo := ATileVersionInfo.StoreString;
  end else begin
    FValue.TileVersionInfo := '';
  end;

  if Assigned(ATileContentType) then begin
    FValue.TileContentType := WideString(ATileContentType.GetContentType);
  end else begin
    FValue.TileContentType := '';
  end;

  ValueToData;
end;

constructor TBerkeleyDBVersionedMetaValueElement.Create(const AData: Pointer);
begin
  inherited Create;
  New(FValue);
  if not Assign(AData, -1, False) then begin
    Dispose(FValue);
    FValue := nil;
  end;
end;

destructor TBerkeleyDBVersionedMetaValueElement.Destroy;
begin
  Dispose(FValue);
  inherited;
end;

function TBerkeleyDBVersionedMetaValueElement.Assign(
  const AData: Pointer;
  const ASize: Integer;
  const AOwnMem: Boolean
): Boolean;
var
  VPtr: PByte;
begin
  if FOwnMem and Assigned(FData) then begin
    FreeMemory(FData);
  end;

  FData := AData;
  FSize := ASize;
  FOwnMem := AOwnMem;

  VPtr := AData;

  FValue.VersionID := PWord(VPtr)^;
  Inc(VPtr, SizeOf(FValue.VersionID));

  FValue.TileZOrder := PWord(VPtr)^;
  Inc(VPtr, SizeOf(FValue.TileZOrder));

  FValue.TileSize := PInteger(VPtr)^;
  Inc(VPtr, SizeOf(FValue.TileSize));

  if Integer(FValue.TileSize) < 0 then begin
    raise EBerkeleyDBBadValue.Create(
      'Read meta-value element error - bad TileSize: ' + IntToStr(FValue.TileSize)
    );
  end;

  FValue.TileDate := PDateTime(VPtr)^;
  Inc(VPtr, SizeOf(FValue.TileDate));

  if IsNan(FValue.TileDate) then begin
    raise EBerkeleyDBBadValue.Create(
      'Read meta-value element error - bad TileDate: NaN'
    );
  end;

  FValue.TileCRC := PCardinal(VPtr)^;
  Inc(VPtr, SizeOf(FValue.TileCRC));

  FValue.TileVersionInfo := PWideChar(VPtr);
  Inc(VPtr, (Length(FValue.TileVersionInfo) + 1) * SizeOf(WideChar));

  FValue.TileContentType := PWideChar(VPtr);
  Inc(VPtr, (Length(FValue.TileContentType) + 1) * SizeOf(WideChar));

  if FSize <= 0 then begin
    FSize := Cardinal(VPtr) - Cardinal(AData);
    FOwnMem := False;
  end;

  Result := True;
end;

procedure TBerkeleyDBVersionedMetaValueElement.ValueToData;
var
  VPtr: PByte;
  VLen: Integer;
begin
  if FOwnMem and Assigned(FData) then begin
    FreeMemory(FData);
  end;

  FSize :=
    SizeOf(FValue.VersionID) +
    SizeOf(FValue.TileZOrder) +
    SizeOf(FValue.TileSize) +
    SizeOf(FValue.TileDate) +
    SizeOf(FValue.TileCRC) +
    (Length(FValue.TileVersionInfo) + Length(cWideCharEndLine)) * SizeOf(WideChar) +
    (Length(FValue.TileContentType) + Length(cWideCharEndLine)) * SizeOf(WideChar);

  FData := GetMemory(FSize);
  FOwnMem := True;
  VPtr := FData;

   // version id
  VLen := SizeOf(FValue.VersionID);
  PWord(VPtr)^ := FValue.VersionID;
  Inc(VPtr, VLen);

   // tile Z-order
  VLen := SizeOf(FValue.TileZOrder);
  PWord(VPtr)^ := FValue.TileZOrder;
  Inc(VPtr, VLen);

   // tile size
  VLen := SizeOf(FValue.TileSize);
  PInteger(VPtr)^ := FValue.TileSize;
  Inc(VPtr, VLen);

   // tile date
  VLen := SizeOf(FValue.TileDate);
  PDateTime(VPtr)^ := FValue.TileDate;
  Inc(VPtr, VLen);

  // tile CRC
  VLen := SizeOf(FValue.TileCRC);
  PCardinal(VPtr)^ := FValue.TileCRC;
  Inc(VPtr, VLen);

  // tile version
  VLen := Length(FValue.TileVersionInfo) * SizeOf(WideChar);
  if VLen > 0 then begin
    Move(PWideChar(FValue.TileVersionInfo)^, VPtr^, VLen);
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
end;

function TBerkeleyDBVersionedMetaValueElement.GetVersionID: Word;
begin
  if Assigned(FValue) then begin
    Result := FValue.VersionID;
  end else begin
    Result := 0;
  end;
end;

function TBerkeleyDBVersionedMetaValueElement.GetTileZOrder: Word;
begin
  if Assigned(FValue) then begin
    Result := FValue.TileZOrder;
  end else begin
    Result := 0;
  end;
end;

function TBerkeleyDBVersionedMetaValueElement.GetTileSize: Integer;
begin
  if Assigned(FValue) then begin
    Result := FValue.TileSize;
  end else begin
    Result := -1;
  end;
end;

function TBerkeleyDBVersionedMetaValueElement.GetTileDate: TDateTime;
begin
  if Assigned(FValue) then begin
    Result := FValue.TileDate;
  end else begin
    Result := 0;
  end;
end;

function TBerkeleyDBVersionedMetaValueElement.GetTileCRC: Cardinal;
begin
  if Assigned(FValue) then begin
    Result := FValue.TileCRC;
  end else begin
    Result := 0;
  end;
end;

function TBerkeleyDBVersionedMetaValueElement.GetTileVersionInfo: WideString;
begin
  if Assigned(FValue) then begin
    Result := FValue.TileVersionInfo;
  end else begin
    Result := '';
  end;
end;

function TBerkeleyDBVersionedMetaValueElement.GetTileContentType: WideString;
begin
  if Assigned(FValue) then begin
    Result := FValue.TileContentType;
  end else begin
    Result := '';
  end;
end;

{ TBerkeleyDBVersionedMetaValue }

constructor TBerkeleyDBVersionedMetaValue.Create;
begin
  inherited Create;
  FValue := nil;
  FBinData := nil;
  Alloc;
  ValueToData;
end;

constructor TBerkeleyDBVersionedMetaValue.Create(
  const AData: Pointer;
  const ASize: Integer;
  const AOwnMem: Boolean
);
begin
  inherited Create;
  FValue := nil;
  FBinData := nil;
  Assign(AData, ASize, AOwnMem);
end;

constructor TBerkeleyDBVersionedMetaValue.Create(
  const AData: IBinaryData
);
begin
  Assert(AData <> nil);
  inherited Create;
  Assign(AData.Buffer, AData.Size, False);
  FBinData := AData;
end;

destructor TBerkeleyDBVersionedMetaValue.Destroy;
begin
  Clear;
  inherited;
end;

function TBerkeleyDBVersionedMetaValue.Assign(
  const AData: Pointer;
  const ASize: Integer;
  const AOwnMem: Boolean
): Boolean;
var
  I: Integer;
  VPtr: PByte;
  VCRC32: Cardinal;
begin
  Clear;

  if FOwnMem and Assigned(FData) then begin
    FreeMemory(FData);
  end;

  if ZlibDecompress(AData, ASize, Pointer(FData), FSize) then begin
    FOwnMem := True;
    if AOwnMem then begin
      FreeMemory(AData);
    end;
  end else begin
    FData := AData;
    FSize := ASize;
    FOwnMem := AOwnMem;
  end;

  VPtr := FData;

  New(FValue);

  Move(VPtr^, FValue.RecMagic[0], Length(FValue.RecMagic));
  Inc(VPtr, Length(FValue.RecMagic));

  if FValue.RecMagic = cVersionedMetaValueMagic then begin
    FValue.RecCRC32 := PCardinal(VPtr)^;
    PCardinal(VPtr)^ := 0;

    VCRC32 := CRC32Buf(FData, FSize);

    PCardinal(VPtr)^ := FValue.RecCRC32;
    if VCRC32 = FValue.RecCRC32 then begin
      Inc(VPtr, SizeOf(FValue.RecCRC32));

      FValue.ItemsCount := PWord(VPtr)^;
      Inc(VPtr, SizeOf(FValue.ItemsCount));

      SetLength(FValue.ItemsArray, FValue.ItemsCount);

      for I := 0 to FValue.ItemsCount - 1 do begin
        FValue.ItemsArray[I] := TBerkeleyDBVersionedMetaValueElement.Create(VPtr);
        Inc(VPtr, FValue.ItemsArray[I].Size);
      end;

      Result := True;
    end else begin
      raise EBerkeleyDBBadValue.Create(
        'Read versioned meta-value error - bad checksumm: 0x' + IntToHex(FValue.RecCRC32, 8)
      );
    end;
  end else begin
    raise EBerkeleyDBBadValue.Create(
      'Read versioned meta-value error - bad magic: 0x' + IntToHex(PCardinal(@FValue.RecMagic[0])^, 8)
    );
  end;
end;

procedure TBerkeleyDBVersionedMetaValue.Alloc;
begin
  New(FValue);
  Move(cVersionedMetaValueMagic[0], FValue.RecMagic[0], Length(FValue.RecMagic));
  FValue.RecCRC32 := 0;
  FValue.ItemsCount := 0;
  SetLength(FValue.ItemsArray, 0);
end;

procedure TBerkeleyDBVersionedMetaValue.Clear;
begin
  if Assigned(FValue) then begin
    SetLength(FValue.ItemsArray, 0);
    Dispose(FValue);
  end;
  FBinData := nil;
end;

procedure TBerkeleyDBVersionedMetaValue.ValueToData;
var
  I: Integer;
  VPtr: PByte;
  VLen: Integer;
  VCRC32Ptr: PByte;
begin
  if FOwnMem and Assigned(FData) then begin
    FreeMemory(FData);
  end;

  FSize :=
    Length(FValue.RecMagic) +
    SizeOf(FValue.RecCRC32) +
    SizeOf(FValue.ItemsCount);

  for I := 0 to Length(FValue.ItemsArray) - 1 do begin
    Inc(FSize, FValue.ItemsArray[I].Size);
  end;

  FData := GetMemory(FSize);
  FOwnMem := True;
  VPtr := FData;

   // magic
  VLen := Length(FValue.RecMagic);
  Move(FValue.RecMagic[0], VPtr^, VLen);
  Inc(VPtr, VLen);

  // init crc32 with zero value
  VCRC32Ptr := VPtr;
  VLen := SizeOf(FValue.RecCRC32);
  FValue.RecCRC32 := 0;
  PInteger(VCRC32Ptr)^ := FValue.RecCRC32;
  Inc(VPtr, VLen);

  // items count
  VLen := SizeOf(FValue.ItemsCount);
  PWord(VPtr)^ := FValue.ItemsCount;
  Inc(VPtr, VLen);

  // items values
  for I := 0 to Length(FValue.ItemsArray) - 1 do begin
    VLen := FValue.ItemsArray[I].Size;
    Move(FValue.ItemsArray[I].Data^, VPtr^, VLen);
    Inc(VPtr, VLen);
  end;

  // calc and save CRC32
  FValue.RecCRC32 := CRC32Buf(Pointer(FData), FSize);
  PInteger(VCRC32Ptr)^ := FValue.RecCRC32;

  // make archive
  VPtr := nil;
  VLen := 0;
  ZlibCompress(FData, FSize, Pointer(VPtr), VLen);
  FreeMemory(FData);
  FData := VPtr;
  FSize := VLen;
end;

function TBerkeleyDBVersionedMetaValue.GetCount: Integer;
begin
  Result := FValue.ItemsCount;
end;

function TBerkeleyDBVersionedMetaValue.GetItem(const AIndex: Integer): IBerkeleyDBVersionedMetaValueElement;
begin
  Result := FValue.ItemsArray[AIndex];
end;

function TBerkeleyDBVersionedMetaValue.Add(const AItem: IBerkeleyDBVersionedMetaValueElement): Integer;
var
  I: Integer;
begin
  I := Length(FValue.ItemsArray);
  FValue.ItemsCount := I + 1;
  SetLength(FValue.ItemsArray, FValue.ItemsCount);
  FValue.ItemsArray[I] := AItem;
  ValueToData;
  Result := FValue.ItemsCount;
end;

procedure TBerkeleyDBVersionedMetaValue.Replace(
  const AIndex: Integer;
  const AItem: IBerkeleyDBVersionedMetaValueElement
);
begin
  FValue.ItemsArray[AIndex] := AItem;
  ValueToData;
end;

procedure TBerkeleyDBVersionedMetaValue.Del(const AIndex: Integer);
var
  I: Integer;
begin
  I := Length(FValue.ItemsArray) - 1;
  if AIndex <> I then begin
    FValue.ItemsArray[AIndex] := FValue.ItemsArray[I];
  end;
  FValue.ItemsCount := I;
  SetLength(FValue.ItemsArray, FValue.ItemsCount);
  ValueToData;
end;

end.
