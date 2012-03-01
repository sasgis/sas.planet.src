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

unit u_TileStorageGE;

interface

uses
  Types,
  Windows,
  Classes,
  i_SimpleTileStorageConfig,
  u_MapVersionFactoryGE,
  i_TileObjCache,
  i_ContentTypeInfo,
  i_MapVersionInfo,
  i_TileInfoBasic,
  i_ContentTypeManager,
  u_MapTypeCacheConfig,
  u_GlobalCahceConfig,
  u_GEIndexFile,
  u_TileStorageAbstract;

type
  TTileStorageGE = class(TTileStorageAbstract)
  private
    FCacheConfig: TMapTypeCacheConfigGE;
    FMapVersionFactoryGE: IMapVersionFactoryGEInternal;
    FIndex: TGEIndexFile;
    FMainContentType: IContentTypeInfoBasic;
  private
    function InternalCreateGEStream(out AGEStream: TFileStream): Boolean;
    function InternalExtractFromGEStream(const AGEStream: TFileStream;
                                         const AOffset, ASize: LongWord;
                                         AResultStream: TMemoryStream): Boolean;
    function InternalProcessGEOffsets(
      const AGEStream: TFileStream;
      const AListOfOffsets: TList
    ): IInterfaceList;
    function InternalCreateAndProcessGEOffsets(
      const AListOfOffsets: TList
    ): IInterfaceList;
  public
    constructor Create(
      AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      AContentTypeManager: IContentTypeManager
    );
    destructor Destroy; override;

    function GetMainContentType: IContentTypeInfoBasic; override;
    function GetAllowDifferentContentTypes: Boolean; override;

    function GetCacheConfig: TMapTypeCacheConfigAbstract; override;

    function GetTileFileName(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): string; override;

    function GetTileInfo(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): ITileInfoBasic; override;

    function LoadTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo;
      AStream: TStream;
      out ATileInfo: ITileInfoBasic
    ): Boolean; override;

    function DeleteTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): Boolean; override;
    function DeleteTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    procedure SaveTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo;
      AStream: TStream
    ); override;
    procedure SaveTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ); override;

    function GetListOfTileVersions(
      const AXY: TPoint;
      const Azoom: byte
    ): IMapVersionListStatic; override;
  end;

implementation

uses
  SysUtils,
  t_CommonTypes,
  i_MapVersionInfoGE,
  u_MapVersionListStatic,
  u_AvailPicsNMC,
  u_TileInfoBasic,
  u_TileStorageTypeAbilities,
  u_GECrypt;

{ TTileStorageGEStuped }

constructor TTileStorageGE.Create(
  AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  AContentTypeManager: IContentTypeManager
);
begin
  FMapVersionFactoryGE := TMapVersionFactoryGE.Create;
  inherited Create(TTileStorageTypeAbilitiesGE.Create, FMapVersionFactoryGE, AConfig);
  FCacheConfig := TMapTypeCacheConfigGE.Create(AConfig, AGlobalCacheConfig);
  FIndex := TGEIndexFile.Create(StorageStateInternal, FCacheConfig);
  FMainContentType := AContentTypeManager.GetInfo('application/vnd.google-earth.tile-image');
end;

destructor TTileStorageGE.Destroy;
begin
  FreeAndNil(FIndex);
  FreeAndNil(FCacheConfig);
  inherited;
end;

function TTileStorageGE.DeleteTile(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): Boolean;
begin
  Result := False;
end;

function TTileStorageGE.DeleteTNE(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): Boolean;
begin
  Result := False;
end;

function TTileStorageGE.GetAllowDifferentContentTypes: Boolean;
begin
  Result := True;
end;

function TTileStorageGE.GetListOfTileVersions(
  const AXY: TPoint;
  const Azoom: byte
): IMapVersionListStatic;
var
  VListOfOffsets: TList;
  VRec: TIndexRec;
  VList: IInterfaceList;
begin
  VListOfOffsets := TList.Create;
  try
    VList := nil;
    // do not check result!
    FIndex.FindTileInfo(AXY, Azoom, 0, 0, VRec, VListOfOffsets);
    if (VListOfOffsets.Count > 0) then begin
      VList := InternalCreateAndProcessGEOffsets(VListOfOffsets);
    end;
    Result := TMapVersionListStatic.Create(VList);
  finally
    VListOfOffsets.Free;
  end;
end;

function TTileStorageGE.GetCacheConfig: TMapTypeCacheConfigAbstract;
begin
  Result := FCacheConfig;
end;

function TTileStorageGE.GetMainContentType: IContentTypeInfoBasic;
begin
  Result := FMainContentType;
end;

function TTileStorageGE.GetTileFileName(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): string;
begin
  Abort;
end;

function TTileStorageGE.GetTileInfo(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): ITileInfoBasic;
begin
  LoadTile(AXY, Azoom, AVersionInfo, nil, Result);
end;

function TTileStorageGE.InternalCreateAndProcessGEOffsets(
  const AListOfOffsets: TList
): IInterfaceList;
var
  VFileStream: TFileStream;
begin
  Result := nil;
  if (AListOfOffsets <> nil) and (AListOfOffsets.Count > 0) then begin
    // no stream - create it
    if InternalCreateGEStream(VFileStream) then
    try
      Result := InternalProcessGEOffsets(VFileStream, AListOfOffsets);
    finally
      VFileStream.Free;
    end;
  end;
end;

function TTileStorageGE.InternalCreateGEStream(out AGEStream: TFileStream): Boolean;
var
  VFileName: string;
begin
  AGEStream := nil;
  VFileName := FCacheConfig.GetDataFileName;
  Result := FileExists(VFileName);
  if Result then
    AGEStream := TFileStream.Create(VFileName, fmOpenRead + fmShareDenyNone);
end;

function TTileStorageGE.InternalExtractFromGEStream(const AGEStream: TFileStream;
                                                    const AOffset, ASize: LongWord;
                                                    AResultStream: TMemoryStream): Boolean;
var
  VTileStart: LongWord;
begin
  Result := FALSE;
  
  // copy part to result atream
  AGEStream.Position := AOffset + 36;
  AResultStream.CopyFrom(AGEStream, ASize);

  // check tag
  AResultStream.Position := 0;
  AResultStream.ReadBuffer(VTileStart, SizeOf(VTileStart));

  // switch by tag
  case VTileStart of
    CRYPTED_JPEG: begin
      GEcrypt(AResultStream.Memory, AResultStream.Size);
      Result := TRUE;
    end;
    DECRYPTED_JPEG: begin
      Result := TRUE;
    end;
    CRYPTED_DXT1: begin
      GEcrypt(AResultStream.Memory, AResultStream.Size);
      Result := TRUE;
    end;
    DECRYPTED_DXT1: begin
      Result := TRUE;
    end;
  end;
end;

function TTileStorageGE.InternalProcessGEOffsets(
  const AGEStream: TFileStream;
  const AListOfOffsets: TList
): IInterfaceList;

  function _ExtractDate(var ADate: String): Boolean;
  var
    p: Integer;
  begin
    // *#G0#*0*AD*2010:03:13*#0G#*
    // get between * from start - date has at least 8 chars (yyyymmdd)
    Result:=FALSE;
    while (0<Length(ADate)) do begin
      p := System.Pos('*',ADate);
      if (p>0) then begin
        // found
        if (p<=8) then begin
          // too short - remove it
          System.Delete(ADate, 1, p);
        end else begin
          // ok - set length and exit
          SetLength(ADate, (p-1));
          Inc(Result);
          Exit;
        end;
      end else begin
        // not found - exit
        Exit;
      end;
    end;
  end;

var
  i: Integer;
  VRec: TIndexRec;
  VMemStream: TMemoryStream;
  VExifOffset: PByte;
  VExifSize: DWORD;
  VExifValue: String;
  VVersion: IMapVersionInfo;
begin
  Result := TInterfaceList.Create;
  VMemStream:=TMemoryStream.Create;
  try
    for i := 0 to AListOfOffsets.Count-1 do
    if FIndex.GetIndexRecByIndex(Integer(AListOfOffsets[i]), VRec) then begin
      // prepare
      VMemStream.Position:=0;
      VMemStream.Size:=0;

      VExifValue := '';
      // process single index item
      if InternalExtractFromGEStream(AGEStream, VRec.Offset, VRec.Size, VMemStream) then begin
        // get exif from streamed image
        if FindExifInJpeg(VMemStream, TRUE, $0000, VExifOffset, VExifSize) then begin
          SetString(VExifValue, PChar(VExifOffset), VExifSize);
          SetLength(VExifValue, StrLen(PChar(VExifValue)));
          VExifValue:=Trim(VExifValue);
          if not _ExtractDate(VExifValue) then
            VExifValue:='';
        end;
      end;
      VVersion := FMapVersionFactoryGE.CreateByGE(VRec.Ver, VRec.Res1, VExifValue);
      Result.Add(VVersion);
    end;
  finally
    VMemStream.Free;
  end;
end;

function TTileStorageGE.LoadTile(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo;
  AStream: TStream;
  out ATileInfo: ITileInfoBasic
): Boolean;
var
  VFileStream: TFileStream;
  VMemStream: TMemoryStream;
  VVersionInfo: IMapVersionInfoGE;
  VAskVer: Word;
  VAskRes1: Byte;
  VRec: TIndexRec;
begin
  Result := False;
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    VAskVer := 0;
    VAskRes1 := 0;
    if Supports(AVersionInfo, IMapVersionInfoGE, VVersionInfo) then begin
      VAskVer := VVersionInfo.Ver;
      VAskRes1 := VVersionInfo.Res1;
    end;
    // do it
    if FIndex.FindTileInfo(AXY, Azoom, VAskVer, VAskRes1, VRec, nil) then begin
      if InternalCreateGEStream(VFileStream) then
        try
          VMemStream := TMemoryStream.Create;
          try
            // extract tile from GE
            Result := InternalExtractFromGEStream(VFileStream, VRec.Offset, VRec.Size, VMemStream);

            if Result then begin
              VMemStream.SaveToStream(AStream);
            end;
            ATileInfo := TTileInfoBasicExists.Create(
              0,
              VRec.Size,
              FMapVersionFactoryGE.CreateByGE(VRec.Ver, VRec.Res1, ''),
              FMainContentType
            );
          finally
            VMemStream.Free;
          end;
        finally
          VFileStream.Free;
        end;
    end else begin
      // no tile
      ATileInfo := TTileInfoBasicNotExists.Create(0, AVersionInfo);
    end;
  end;
end;

procedure TTileStorageGE.SaveTile(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo;
  AStream: TStream
);
begin
  Abort;
end;

procedure TTileStorageGE.SaveTNE(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
);
begin
  Abort;
end;

end.
