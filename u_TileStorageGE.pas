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
    FIndex: TGEIndexFile;
    FMainContentType: IContentTypeInfoBasic;
    FTileVersionsCacher: ITileObjCachePersistent;
  private
    function ListOfVersions_Make: TStrings;
    function ListOfVersions_NeedToCollect(const AXY: TPoint; const AZoom: Byte): Boolean;
    procedure ListOfVersions_SaveToCache(const AXY: TPoint; const AZoom: Byte;
                                         var AListOfVersions: TStrings);
  private
    function InternalCreateGEStream(out AGEStream: TFileStream): Boolean;
    function InternalExtractFromGEStream(const AGEStream: TFileStream;
                                         const AOffset, ASize: LongWord;
                                         AResultStream: TMemoryStream): Boolean;
    function InternalProcessGEOffsets(const AGEStream: TFileStream;
                                      const AListOfOffsets: TStrings;
                                      AListOfVersions: TStrings): Byte;
    function InternalCreateAndProcessGEOffsets(const AListOfOffsets: TStrings;
                                               AListOfVersions: TStrings): Byte;
  public
    constructor Create(
      AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      AContentTypeManager: IContentTypeManager;
      ATileVersionsCacher: ITileObjCachePersistent
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

    function GetListOfTileVersions(const AXY: TPoint; const Azoom: byte;
                                   const AAllowFromCache: Boolean;
                                   AListOfVersions: TStrings): Boolean; override;
  end;

implementation

uses
  SysUtils,
  t_CommonTypes,
  u_AvailPicsNMC,
  u_TileInfoBasic,
  u_TileStorageTypeAbilities,
  u_GECrypt;

{ TTileStorageGEStuped }

constructor TTileStorageGE.Create(
  AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  AContentTypeManager: IContentTypeManager;
  ATileVersionsCacher: ITileObjCachePersistent
);
begin
  inherited Create(TTileStorageTypeAbilitiesGE.Create, AConfig);
  FTileVersionsCacher := ATileVersionsCacher;
  FCacheConfig := TMapTypeCacheConfigGE.Create(AConfig, AGlobalCacheConfig);
  FIndex := TGEIndexFile.Create(StorageStateInternal, FCacheConfig);
  FMainContentType := AContentTypeManager.GetInfo('application/vnd.google-earth.tile-image');
end;

destructor TTileStorageGE.Destroy;
begin
  FTileVersionsCacher:=nil;
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

function TTileStorageGE.GetListOfTileVersions(const AXY: TPoint; const Azoom: byte;
                                              const AAllowFromCache: Boolean;
                                              AListOfVersions: TStrings): Boolean;
var
  VVersionInfo: IMapVersionInfo;
  VOffset, VSize: LongWord;
  VListOfOffsets: TStrings;
begin
  Result:=FALSE;

  if AAllowFromCache and Assigned(FTileVersionsCacher) then begin
    // allow from cache
    if FTileVersionsCacher.TryLoadTileFromCache(AListOfVersions, AXY, Azoom, nil) then begin
      Inc(Result);
      Exit;
    end;
  end;

  // from storage
  VListOfOffsets:=ListOfVersions_Make;
  try
    if FIndex.FindTileInfo(AXY, Azoom, VVersionInfo, VOffset, VSize, VListOfOffsets) then begin
      // parse
      if (InternalCreateAndProcessGEOffsets(VListOfOffsets, AListOfVersions) > 0) then begin
        // to cache
        if Assigned(FTileVersionsCacher) then begin
          VListOfOffsets.Assign(AListOfVersions);
          ListOfVersions_SaveToCache(AXY, Azoom, VListOfOffsets);
        end;
        // done 
        Inc(Result);
      end;
    end;
  finally
    FreeAndNil(VListOfOffsets);
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

function TTileStorageGE.InternalCreateAndProcessGEOffsets(const AListOfOffsets: TStrings;
                                                          AListOfVersions: TStrings): Byte;
var
  VFileStream: TFileStream;
begin
  Result := 0;
  if (nil<>AListOfOffsets) and (0<AListOfOffsets.Count) then begin
    // no stream - create it
    if InternalCreateGEStream(VFileStream) then
    try
      Result := InternalProcessGEOffsets(VFileStream, AListOfOffsets, AListOfVersions);
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

function TTileStorageGE.InternalProcessGEOffsets(const AGEStream: TFileStream;
                                                 const AListOfOffsets: TStrings;
                                                 AListOfVersions: TStrings): Byte;

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
  VIndex: Integer;
  VRec: TIndexRec;
  VMemStream: TMemoryStream;
  VExifOffset: PByte;
  VExifSize: DWORD;
  VExifValue: String;
begin
  Result:=0;
  if (nil=AListOfOffsets) then
    Exit;
  if (0=AListOfOffsets.Count) then
    Exit;

  VMemStream:=TMemoryStream.Create;
  try
    for i := 0 to AListOfOffsets.Count-1 do
    if TryStrToInt(AListOfOffsets[i], VIndex) then
    if FIndex.GetIndexRecByIndex(VIndex, VRec) then begin
      // prepare
      VMemStream.Position:=0;
      VMemStream.Size:=0;

      // process single index item
      if InternalExtractFromGEStream(AGEStream, VRec.Offset, VRec.Size, VMemStream) then begin
        // get exif from streamed image
        if FindExifInJpeg(VMemStream, TRUE, $0000, VExifOffset, VExifSize) then begin
          SetString(VExifValue, PChar(VExifOffset), VExifSize);
          SetLength(VExifValue, StrLen(PChar(VExifValue)));
          VExifValue:=Trim(VExifValue);
          if (0<Length(VExifValue)) then
          if _ExtractDate(VExifValue) then begin
            // date in VExifValue (as yyyy:mm:dd)
            VExifValue := VExifValue + '=' + IntToStr(VRec.Res1)+'\'+IntToStr(VRec.Ver);
            AListOfVersions.Add(VExifValue);
            Inc(Result);
          end;
        end;
      end;
    end;
  finally
    VMemStream.Free;
  end;
end;

function TTileStorageGE.ListOfVersions_Make: TStrings;
begin
  Result := TStringList.Create;
  with TStringList(Result) do begin
    Sorted:=TRUE;
    Duplicates:=dupIgnore;
  end;
end;

function TTileStorageGE.ListOfVersions_NeedToCollect(const AXY: TPoint; const AZoom: Byte): Boolean;
begin
  Result := FALSE; // not need to cache
  if Assigned(FTileVersionsCacher) then begin
    // versioninfo always NIL
    if not FTileVersionsCacher.TryLoadTileFromCache(nil, AXY, AZoom, nil) then
      Inc(Result); // not in cache and need to cache
  end;
end;

procedure TTileStorageGE.ListOfVersions_SaveToCache(const AXY: TPoint;
                                                    const AZoom: Byte;
                                                    var AListOfVersions: TStrings);
begin
  if Assigned(FTileVersionsCacher) then
    FTileVersionsCacher.AddTileToCache(TPersistent(AListOfVersions), AXY, AZoom, nil);
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
  VOffset, VSize: LongWord;
  VMemStream: TMemoryStream;
  VVersionInfo: IMapVersionInfo;
  VListOfVersions, VListOfOffsets: TStrings;
begin
  Result := False;
  VListOfOffsets := nil;
  VListOfVersions := nil;
  if StorageStateStatic.ReadAccess <> asDisabled then
  try
    VVersionInfo := AVersionInfo;

    // if need to collect versions
    if ListOfVersions_NeedToCollect(AXY, Azoom) then begin
      VListOfOffsets:=ListOfVersions_Make;
      VListOfVersions:=ListOfVersions_Make;
    end;
    
    // do it
    if FIndex.FindTileInfo(AXY, Azoom, VVersionInfo, VOffset, VSize, VListOfOffsets) then begin
      if InternalCreateGEStream(VFileStream) then
        try
          VMemStream := TMemoryStream.Create;
          try
            // extract tile from GE
            Result := InternalExtractFromGEStream(VFileStream, VOffset, VSize, VMemStream);
            
            if Result then begin
              VMemStream.SaveToStream(AStream);
            end;
            
            ATileInfo := TTileInfoBasicExists.Create(
              0,
              VSize,
              VVersionInfo,
              FMainContentType
            );

            // additional info from collected tiles
            if (nil<>VListOfOffsets) then begin
              // stream created - just process
              InternalProcessGEOffsets(VFileStream, VListOfOffsets, VListOfVersions);
            end;
          finally
            VMemStream.Free;
          end;
        finally
          VFileStream.Free;
        end;
    end else begin
      // no tile
      ATileInfo := TTileInfoBasicNotExists.Create(0, VVersionInfo);

      // check additional info
      InternalCreateAndProcessGEOffsets(VListOfOffsets, VListOfVersions);
    end;

    // cache versions
    if (nil<>VListOfVersions) then
      ListOfVersions_SaveToCache(AXY, Azoom, VListOfVersions);
  finally
    FreeAndNil(VListOfVersions);
    FreeAndNil(VListOfOffsets);
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
