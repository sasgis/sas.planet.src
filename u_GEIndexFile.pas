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

unit u_GEIndexFile;

interface

uses
  Types,
  SysUtils,
  Classes,
  i_JclNotify,
  i_StorageStateInternal,
  i_MapVersionInfo,
  u_MapTypeCacheConfig;

type
  TIndexRec = packed record
    Magic  : LongWord;  // число-идентификатор =  D5 BF 93 75
    Ver    : Word;      // версия тайла
    TileID : Byte;      // тип тайла
    Res1   : Byte;      // индекс снимка?
    Zoom   : Byte;      // уровень зума
    Res2   : Byte;      // ?
    Layer  : Word;      // номер слоя (только для слоя, иначе = 0)
    NameLo : LongWord;  // первая часть имени
    NameHi : LongWord;  // вторая часть имени
    ServID : Word;      // номер сервера из списка в dbCache.dat
    Unk    : Word;      // ? наличие поля зависит от ОС (в Win - есть, в Linux - нет)
    Offset : LongWord;  // позиция тайла в кэше dbCache.dat
    Size   : LongWord;  // размер тайла
  end;

  TGEIndexFile = class
  private
    FSync: TMultiReadExclusiveWriteSynchronizer;
    FCacheConfig: TMapTypeCacheConfigGE;
    FStorageStateInternal: IStorageStateInternal;
    FIndexFileName: string;
    FIndexInfo: array of TIndexRec;
    FServerID: Word;
    FConfigChangeListener: IJclListener;
    FFileInited: Boolean;
    procedure GEXYZtoHexTileName(APoint: TPoint; AZoom: Byte; out ANameHi, ANameLo: LongWord);
    procedure _UpdateIndexInfo;
    procedure OnConfigChange;
    function getServID:word;
  protected
  public
    constructor Create(
      AStorageStateInternal: IStorageStateInternal;
      ACacheConfig: TMapTypeCacheConfigGE
    );
    destructor Destroy; override;
    function FindTileInfo(
      APoint: TPoint;
      AZoom: Byte;
      var AVersionInfo: IMapVersionInfo;
      out AOffset: Integer;
      out ASize: Integer;
      AVersions: TStrings
    ): Boolean;
  end;

implementation

uses
  Variants,
  t_CommonTypes,
  u_NotifyEventListener,
  u_MapVersionInfo;

{ TGEIndexFile }

constructor TGEIndexFile.Create(
  AStorageStateInternal: IStorageStateInternal;
  ACacheConfig: TMapTypeCacheConfigGE
);
begin
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  FCacheConfig := ACacheConfig;
  FStorageStateInternal := AStorageStateInternal;
  FFileInited := False;
  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FCacheConfig.ConfigChangeNotifier.Add(FConfigChangeListener);
end;

destructor TGEIndexFile.Destroy;
begin
  FCacheConfig.ConfigChangeNotifier.Remove(FConfigChangeListener);
  FConfigChangeListener := nil;
  FreeAndNil(FSync);
  FIndexInfo := nil;
  inherited;
end;

procedure TGEIndexFile.GEXYZtoHexTileName(APoint: TPoint; AZoom: Byte; out ANameHi, ANameLo: LongWord);
var
  VMask: Integer;
  i: byte;
  VValue: Byte;
begin
  ANameHi := 0;
  ANameLo := 0;
  if AZoom > 0 then begin
    VMask := 1 shl (AZoom - 1);
    for i := 1 to AZoom do begin
      if (APoint.X and VMask) > 0 then begin
        if (APoint.y and VMask) > 0 then begin
          VValue := 1;
        end else begin
          VValue := 2;
        end;
      end else begin
        if (APoint.y and VMask) > 0 then begin
          VValue := 0;
        end else begin
          VValue := 3;
        end;
      end;
      if i <= 16 then begin
        ANameLo := ANameLo or (LongWord(VValue) shl (32 - i * 2));
      end else begin
        ANameHi := ANameHi or (LongWord(VValue) shl (32 - (i - 16) * 2));
      end;
      VMask := VMask shr 1;
    end;
  end;
end;

function TGEIndexFile.FindTileInfo(
  APoint: TPoint;
  AZoom: Byte;
  var AVersionInfo: IMapVersionInfo;
  out AOffset, ASize: Integer;
  AVersions: TStrings
): Boolean;
var
  VNameLo: LongWord;
  VNameHi: LongWord;
  i: Integer;
  VProcessed: Boolean;
  VVersion: Word;
  VAskVersion: Byte;
  VText: String;
begin
  Result := False;
  AOffset := 0;
  ASize := 0;
  
  VAskVersion:=0;
  try
    if Assigned(AVersionInfo) then begin
      VText:=VarToStrDef(AVersionInfo.Version,'');
      if (0<Length(VText)) then begin
        if TryStrToInt(Trim(VText), i) then
          if (i>0) and (i<=$FF) then
            VAskVersion := i;
      end;
    end;
  except
  end;

  AVersionInfo := nil;
  VProcessed := False;
  while not VProcessed do begin
    if not FFileInited then begin
      FSync.BeginWrite;
      try
        if not FFileInited then begin
          _UpdateIndexInfo;
        end;
      finally
        FSync.EndWrite;
      end;
    end;
    FSync.BeginRead;
    try
      if FFileInited then begin
        if Length(FIndexInfo) > 0 then begin
          GEXYZtoHexTileName(APoint, AZoom, VNameHi, VNameLo);
          for i := Length(FIndexInfo) - 1 downto 0 do begin
            if FIndexInfo[i].Magic = $7593BFD5 then begin
              if FIndexInfo[i].TileID = 130 then begin
                if FIndexInfo[i].ServID = FServerID then begin
                  if FIndexInfo[i].Zoom = AZoom then begin
                    if (FIndexInfo[i].NameLo = VNameLo) and (FIndexInfo[i].NameHi = VNameHi) then begin
                      // found
                      if (not Result) and ((0=VAskVersion) or (VAskVersion=FIndexInfo[i].Res1)) then begin
                        // second entrance will fail because of Result
                        AOffset := FIndexInfo[i].Offset;
                        ASize := FIndexInfo[i].Size;
                        VVersion := FIndexInfo[i].Res1; // FIndexInfo[i].Ver;
                        AVersionInfo := TMapVersionInfo.Create(VVersion);
                        Inc(Result);
                        //Break;
                      end;
                      // ok without collecting versions
                      if Result and (nil=AVersions) then
                        break;
                      // collecting versions (SORTED and DUP IGNORED!)
                      if (nil<>AVersions) then
                        AVersions.Add(IntToStr(FIndexInfo[i].Res1));
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        VProcessed := True;
      end;
    finally
      FSync.EndRead;
    end;
  end;
end;

procedure TGEIndexFile.OnConfigChange;
begin
  FSync.BeginWrite;
  try
    FFileInited := False;
  finally
    FSync.EndWrite;
  end;
end;

function TGEIndexFile.getServID:word;
var
  VCode:  Integer;
  VNameInCache: string;
begin
  VNameInCache := FCacheConfig.GetNameInCache;
  if VNameInCache = '' then begin
    Result := 0;
  end else if VNameInCache = '1' then begin
    Result := 1;
  end else if VNameInCache = '2' then begin
    Result := 2;
  end else if VNameInCache = '3' then begin
    Result := 3;
  end else begin
    if TryStrToInt(VNameInCache, VCode) then begin
      Result := VCode;
    end else begin
      Result := 0;
    end;
  end;
end;

procedure TGEIndexFile._UpdateIndexInfo;
var
  VFileName: string;
  VFileStream: TFileStream;
  VCount: Cardinal;
begin
  VFileName := FCacheConfig.GetIndexFileName;
  if VFileName <> FIndexFileName then begin
    FIndexInfo := nil;
    FIndexFileName := VFileName;
    if FileExists(VFileName) then begin
      VFileStream := TFileStream.Create(VFileName, fmOpenRead);
      try
        VCount := VFileStream.Size div SizeOf(FIndexInfo[0]);
        SetLength(FIndexInfo, VCount );
        VFileStream.ReadBuffer(FIndexInfo[0], VCount * SizeOf(FIndexInfo[0]));
        FServerID := getServID;
        FStorageStateInternal.ReadAccess := asEnabled;
      finally
        FreeAndNil(VFileStream);
      end;
    end else begin
      FStorageStateInternal.ReadAccess := asDisabled;
    end;
  end;
  FFileInited := True;
end;

end.
