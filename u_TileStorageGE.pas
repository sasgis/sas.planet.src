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
  i_BinaryData,
  i_SimpleTileStorageConfig,
  u_MapVersionFactoryGE,
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
    FUserDefinedGEPath: String;
    FUserDefinedGEServer: String;
    FServerID: Word;
    FMainContentType: IContentTypeInfoBasic;
  private
    function InternalGetServerID(const AUserDefinedGEServer: String): Word;
    function InternalCreateGEStream(out AGEStream: TFileStream): Boolean;
    function InternalExtractFromGEStream(const AGEStream: TFileStream;
                                         const AOffset, ASize: LongWord;
                                         AResultStream: TMemoryStream;
                                         out ATileDateStr: String): Boolean;
    function InternalProcessGEOffsets(
      const AGEStream: TFileStream;
      const AListOfOffsets: TList;
      const AGEServer: String
    ): IInterfaceList;
    function InternalCreateAndProcessGEOffsets(
      const AListOfOffsets: TList;
      const AGEServer: String
    ): IInterfaceList;

    procedure DoOnMapSettingsEdit(Sender: TObject);
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
      AData: IBinaryData
    ); override;
    procedure SaveTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ); override;

    function GetListOfTileVersions(
      const AXY: TPoint;
      const Azoom: byte;
      AVersionInfo: IMapVersionInfo
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
  // for caching ServerID
  FServerID := 0;
  FUserDefinedGEPath := '';
  FUserDefinedGEServer := '';
  FCacheConfig := TMapTypeCacheConfigGE.Create(AConfig, AGlobalCacheConfig, Self.DoOnMapSettingsEdit);
  FIndex := TGEIndexFile.Create(StorageStateInternal, FCacheConfig);
  FMainContentType := AContentTypeManager.GetInfo('application/vnd.google-earth.tile-image');
end;

destructor TTileStorageGE.Destroy;
begin
  FreeAndNil(FIndex);
  FreeAndNil(FCacheConfig);
  inherited;
end;

procedure TTileStorageGE.DoOnMapSettingsEdit(Sender: TObject);
begin
  if Assigned(FIndex) then
    FIndex.OnConfigChange(Sender);
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
  const Azoom: byte;
  AVersionInfo: IMapVersionInfo
): IMapVersionListStatic;
var
  VListOfOffsets: TList;
  VRec: TIndexRec;
  VVersionInfo: IMapVersionInfoGE;
  VUserDefinedGEServer: String;
  VList: IInterfaceList;
begin
  VListOfOffsets := TList.Create;
  try
    // get ServerID from version (and skip other params)
    if Supports(AVersionInfo, IMapVersionInfoGE, VVersionInfo) then
      VUserDefinedGEServer := VVersionInfo.GEServer
    else
      VUserDefinedGEServer := '';
    
    // do not check result!
    FIndex.FindTileInfo(AXY, Azoom, (0<Length(VUserDefinedGEServer)), InternalGetServerID(VUserDefinedGEServer), 0, '', VRec, VListOfOffsets);

    // make list with original(!) GEServer
    // if user asks about [tm] or [sky] - do not replace it with 2 or 3
    VList := nil;
    if (VListOfOffsets.Count > 0) then begin
      VList := InternalCreateAndProcessGEOffsets(VListOfOffsets, VUserDefinedGEServer);
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
  const AListOfOffsets: TList;
  const AGEServer: String
): IInterfaceList;
var
  VFileStream: TFileStream;
begin
  Result := nil;
  if (AListOfOffsets <> nil) and (AListOfOffsets.Count > 0) then begin
    // no stream - create it
    if InternalCreateGEStream(VFileStream) then
    try
      Result := InternalProcessGEOffsets(VFileStream, AListOfOffsets, AGEServer);
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
                                                    AResultStream: TMemoryStream;
                                                    out ATileDateStr: String): Boolean;
var
  VTileStart: LongWord;
  VTileRec: TTileRec;
begin
  Result := FALSE;

  // get tile info
  AGEStream.Position := AOffset;
  AGEStream.ReadBuffer(VTileRec, SizeOf(VTileRec));
  // date
  with VTileRec do begin
    ATileDateStr := MakeGEDateToStr(RX01, Layer);
  end;

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

function TTileStorageGE.InternalGetServerID(const AUserDefinedGEServer: String): Word;
var
  VCode: Integer;
  VFileStream: TFileStream;
  VCache_Head: TCache_Head;
  VServerRec: TServerRec;
  VServerName: String;
  VUserDefinedGEPath: String;
begin
  // allow to use some values without checking the cache
  if (0=Length(AUserDefinedGEServer)) then
    Result := 0
  else if TryStrToInt(AUserDefinedGEServer, VCode) then
    Result := VCode
  else begin
    VUserDefinedGEPath := FCacheConfig.GetNameInCache;
    if SameText(AUserDefinedGEServer, FUserDefinedGEServer) and AnsiSameText(VUserDefinedGEPath, FUserDefinedGEPath) then begin
      // cached ok
      Result := FServerID;
    end else begin
      // should check cache header
      Result := 0;
      if InternalCreateGEStream(VFileStream) then
      try
        // try to get server id from file
        VFileStream.Position := 0;
        VFileStream.ReadBuffer(VCache_Head, SizeOf(VCache_Head));
        // loop
        if (VCache_Head.SCount <= $FF) then
        for VCode := 0 to VCache_Head.SCount - 1 do begin
          // read single item
          VFileStream.ReadBuffer(VServerRec, SizeOf(VServerRec));
          // get server name
          VServerName:='';
          while VServerRec.Name<>#0 do begin
            VServerName:=VServerName+VServerRec.Name;
            VFileStream.ReadBuffer(VServerRec.Name, SizeOf(VServerRec.Name));
          end;
          // check server name
          if System.Pos(AUserDefinedGEServer, VServerName) > 0 then begin
            // found
            Result := VCode;
            FServerID := VCode;
            FUserDefinedGEServer := AUserDefinedGEServer;
            FUserDefinedGEPath := VUserDefinedGEPath;
            Exit;
          end;
        end;
      finally
        VFileStream.Free;
      end;
    end;
  end;
end;

function TTileStorageGE.InternalProcessGEOffsets(
  const AGEStream: TFileStream;
  const AListOfOffsets: TList;
  const AGEServer: String
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
  VGEServer: String;
  VTileDate: String;
  VVersion: IMapVersionInfo;
begin
  VGEServer := AGEServer;
  Result := TInterfaceList.Create;
  VMemStream:=TMemoryStream.Create;
  try
    for i := 0 to AListOfOffsets.Count-1 do
    if FIndex.GetIndexRecByIndex(Integer(AListOfOffsets[i]), VRec) then begin
      // try to make date from index
      with VRec do begin
        VTileDate := MakeGEDateToStr(VRec.RX01, VRec.Layer);
      end;

      // if no date - get it from tile
      if (0=Length(VTileDate)) then begin
        // prepare
        VMemStream.Position:=0;
        VMemStream.Size:=0;

        // process single index item
        if InternalExtractFromGEStream(AGEStream, VRec.Offset, VRec.Size, VMemStream, VTileDate) then begin
          // may be tile found without date? so get exif from streamed image
          if (0=Length(VTileDate)) then
          if FindExifInJpeg(VMemStream, TRUE, $0000, VExifOffset, VExifSize) then begin
            SetString(VTileDate, PChar(VExifOffset), VExifSize);
            SetLength(VTileDate, StrLen(PChar(VTileDate)));
            VTileDate:=Trim(VTileDate);
            if not _ExtractDate(VTileDate) then
              VTileDate:='';
          end;
        end;
      end;

      // try to keep user-defined GEServer
      if (0=Length(AGEServer)) then
        VGEServer := IntToStr(VRec.ServID);

      // make version
      VVersion := FMapVersionFactoryGE.CreateByGE(VRec.Ver, VGEServer, VTileDate);
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
  VAskGEServer: String;
  VAskTileDate: String;
  VRec: TIndexRec;

  function _GetUserGEServer: String;
  begin
    // keep user defined GEServer
    Result := VAskGEServer;
    if (0=Length(Result)) then
      Result := IntToStr(VRec.ServID);
  end;
begin
  Result := False;
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    // get filter
    VAskVer := 0;
    VAskTileDate := '';
    VAskGEServer := '';
    if Supports(AVersionInfo, IMapVersionInfoGE, VVersionInfo) then begin
      VAskVer := VVersionInfo.Ver;
      VAskGEServer := VVersionInfo.GEServer;
      VAskTileDate := VVersionInfo.TileDate;
    end;
    
    // do it (on load tiles do not mix different ServerIDs)
    if FIndex.FindTileInfo(AXY, Azoom, TRUE, InternalGetServerID(VAskGEServer), VAskVer, VAskTileDate, VRec, nil) then begin
      if InternalCreateGEStream(VFileStream) then
        try
          VMemStream := TMemoryStream.Create;
          try
            // extract tile from GE
            Result := InternalExtractFromGEStream(VFileStream, VRec.Offset, VRec.Size, VMemStream, VAskTileDate);

            if Result then begin
              VMemStream.SaveToStream(AStream);
            end;
            ATileInfo := TTileInfoBasicExists.Create(
              0,
              VRec.Size,
              FMapVersionFactoryGE.CreateByGE(VRec.Ver, _GetUserGEServer, VAskTileDate),
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
  AData: IBinaryData
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
