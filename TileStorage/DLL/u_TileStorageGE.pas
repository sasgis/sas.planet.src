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

unit u_TileStorageGE deprecated;

interface

uses
  Windows,
  SysUtils,
  Classes,
  i_BinaryData,
  t_CommonTypes,
  t_DLLCache,
  i_ContentTypeInfo,
  i_MapVersionInfo,
  i_MapVersionFactory,
  i_MapVersionListStatic,
  i_MapVersionRequest,
  i_CoordConverter,
  i_TileInfoBasic,
  i_TileStorageAbilities,
  i_TileStorage,
  i_ContentTypeManager,
  u_TileStorageAbstract;

type
  TTileStorageDLL = class(TTileStorageAbstract)
  protected
    FMainContentType: IContentTypeInfoBasic;
    FTileNotExistsTileInfo: ITileInfoBasic;
    // access
    FDLLSync: IReadWriteSync;
    FDLLHandle: THandle;
    FDLLCacheHandle: TDLLCacheHandle;
    // routines
    FDLLCache_EnumTileVersions: Pointer;
    FDLLCache_QueryTile: Pointer;
    FDLLCache_ConvertImage: Pointer;
    FDLLCache_QueryFillingMap: Pointer;
    FDLLCache_QueryTileRectInfo: Pointer;
  protected
    // Lib routines
    function InternalLib_CleanupProc: Boolean; virtual;
    function InternalLib_Initialize: Boolean; virtual;
    function InternalLib_CheckInitialized: Boolean; virtual;
    function InternalLib_Unload: Boolean; virtual;
    function InternalLib_NotifyStateChanged(const AEnabled: Boolean): Boolean;
    function InternalLib_SetPath(const APath: PAnsiChar): Boolean;
    function InternalLib_GetTileVersions(const AEnumInfo: PEnumTileVersionsInfo): Boolean;
    function InternalLib_QueryTile(const ATileInfo: PQueryTileInfo): Boolean;
    function InternalLib_GetTileRectInfo(const ATileRectInfo: PQueryTileRectInfo): Boolean;
    function InternalLib_ConvertImage(
      const AConvertImage_Context: Pointer;
      const ABuffer: Pointer;
      const ASize: Cardinal
    ): Boolean;
  protected
    function QueryTileInternal(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AIsNeedData: Boolean
    ): ITileInfoBasic;
  protected
    // common tile storage interface
    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic; override;
    function GetTileInfoEx(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic; override;

    function GetTileRectInfo(
      const ARect: TRect;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest
    ): ITileRectInfo; override;

    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): string; override;

    function DeleteTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    function SaveTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const ALoadDate: TDateTime;
      const AContentType: IContentTypeInfoBasic;
      const AData: IBinaryData;
      const AIsOverwrite: Boolean
    ): Boolean; override;

    function GetListOfTileVersions(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest
    ): IMapVersionListStatic; override;
  public
    constructor Create(
      const AStorageTypeAbilities: ITileStorageTypeAbilities;
      const AStorageForceAbilities: ITileStorageAbilities;
      const AGeoConverter: ICoordConverter;
      const AStoragePath: string;
      const AMapVersionFactory: IMapVersionFactory;
      const AContentTypeManager: IContentTypeManager
    );
    destructor Destroy; override;
  end;

  TTileStorageGC = class(TTileStorageDLL)
  protected
    function InternalLib_Initialize: Boolean; override;
    function InternalLib_CheckInitialized: Boolean; override;
  end;

implementation

uses
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_BinaryDataByMemStream,
  u_MapVersionListStatic,
  u_AvailPicsNMC,
  u_Synchronizer,
  u_TileInfoBasic,
  u_TileRectInfoShort;

function DLLCache_ConvertImage_Callback(
  const AConvertImage_Context: Pointer;
  const AFormatOut: LongWord;
  const AOutputBuffer: Pointer;
  const AOutputSize: LongWord
): Boolean; stdcall;
begin
  Result := FALSE;
  // called from DLLCache_QueryTile_Callback - AConvertImage_Context is ATileInfo: PQueryTileInfo
  if (DLLCACHE_IMG_PRIMARY = AFormatOut) and (AConvertImage_Context <> nil) and (AOutputBuffer <> nil) and (AOutputSize > 0) then begin
    try
      with TMemoryStream(PQueryTileInfo(AConvertImage_Context)^.TileStream) do begin
        WriteBuffer(AOutputBuffer^, AOutputSize);
        Position := 0;
      end;
      Inc(Result);
    except
    end;
  end;
end;

function DLLCache_EnumTileVersions_Callback(
  const AContext: Pointer;
  const AEnumInfo: PEnumTileVersionsInfo;
  const AVersionString: PAnsiChar
): Boolean; stdcall;
var
  VVersionString: AnsiString;
begin
  Result := FALSE;
  // if AVersionString is NULL - it means NO VERSION aka CLEAR - do not enum it
  if (nil <> AEnumInfo) and (nil <> AVersionString) then begin
    try
      // make list
      if (nil = AEnumInfo^.ListOfVersions) then begin
        AEnumInfo^.ListOfVersions := TStringList.Create;
        with TStringList(AEnumInfo^.ListOfVersions) do begin
          Sorted := TRUE;
          Duplicates := dupIgnore;
        end;
      end;
      // make version string
      SetString(VVersionString, AVersionString, StrLen(AVersionString));
      // add if not found
      TStringList(AEnumInfo^.ListOfVersions).Add(VVersionString);
      Inc(Result);
    except
    end;
  end;
end;

function InternalCreate_TileInfoType(const ATileRectInfo: PQueryTileRectInfo): TTileInfoType;
begin
  if ((ATileRectInfo^.Common.FlagsOut and DLLCACHE_QTO_TNE_EXISTS) <> 0) then begin
    Result := titTneExists;
  end else if (ATileRectInfo^.TileSize > 0) then begin
    Result := titExists;
  end else begin
    Result := titNotExists;
  end
end;

type
  TGetTileRectFullInfo = packed record
    Base: TQueryTileRectInfo;
    InfoArray: TArrayOfTileInfoShortInternal;
  end;
  PGetTileRectFullInfo = ^TGetTileRectFullInfo;

function DLLCache_QueryTileRectInfo_Callback(
  const AContext: Pointer;
  const ATileRectInfo: PQueryTileRectInfo
): Boolean; stdcall;
var
  VIndex: Integer;
begin
  // add info from tile
  VIndex := TTileRectInfoShort.TileInRectToIndex(ATileRectInfo^.Common.XY, ATileRectInfo^.TileRect);
  // write info to TTileInfoShortInternal
  if (VIndex>=0) then begin
    with PGetTileRectFullInfo(ATileRectInfo)^.InfoArray[VIndex] do
    if (titUnknown=FInfoType) then begin
      // base fields
      FLoadDate := ATileRectInfo^.DateOut;
      FSize     := ATileRectInfo^.TileSize;
      // derived field
      FInfoType := InternalCreate_TileInfoType(ATileRectInfo);
      Result := TRUE;
    end else begin
      // duplicated value
      Result := FALSE;
    end;
  end else begin
    // invalid tile position
    Result := FALSE;
  end;
end;

function DLLCache_QueryTile_Callback(
  const AContext: Pointer;
  const ATileInfo: PQueryTileInfo;
  const ATileBuffer: Pointer;
  const AVersionString: PAnsiChar
): Boolean; stdcall;
var
  VVersionStoreString: AnsiString;
begin
  Result := FALSE;
  if (nil <> ATileInfo) then begin
    try
      // tile body
      if (nil <> ATileBuffer) and (ATileInfo^.TileSize > 0) and (nil <> ATileInfo^.TileStream) then begin
        if (ATileInfo^.Common.Size >= SizeOf(TQueryTileInfo_V2)) then begin
          // MULTIPLE TYPES! check image type
          case PQueryTileInfo_V2(ATileInfo)^.FormatOut of
            DLLCACHE_IMG_PRIMARY: begin
              // JPEG
              with TMemoryStream(ATileInfo^.TileStream) do begin
                WriteBuffer(ATileBuffer^, ATileInfo^.TileSize);
                Position := 0;
              end;
              Result := TRUE;
            end;
            DLLCACHE_IMG_SEC_DXT1: begin
              // call DLL to CONVERT to JPEG
              Result := TTileStorageDLL(AContext).InternalLib_ConvertImage(ATileInfo, ATileBuffer, ATileInfo^.TileSize);
            end;
          end;
        end else begin
          // ONLY PRIMARY! always convert to primary image format at DLL
          with TMemoryStream(ATileInfo^.TileStream) do begin
            WriteBuffer(ATileBuffer^, ATileInfo^.TileSize);
            Position := 0;
          end;
          // do smth
          Result := TRUE;
        end;
      end;

      // tile version
      if (0 <> (ATileInfo^.Common.FlagsOut and DLLCACHE_QTO_SAME_VERSION)) then begin
        // ok
        Result := TRUE;
      end else if (nil <> AVersionString) and (nil <> AContext) then begin
        // make as string
        SetString(VVersionStoreString, AVersionString, StrLen(AVersionString));
        // make and set version
        IMapVersionInfo(ATileInfo^.VersionOut) := TTileStorageDLL(AContext).MapVersionFactory.CreateByStoreString(VVersionStoreString);
        // do smth
        Result := TRUE;
      end;
    except
    end;
  end;
end;

function HostExifReaderProc(
  const AContext: Pointer;
  const ABuffer: Pointer;
  const ASize: LongWord;
  const AExifBufPtr: PPointer;
  const AExifSizPtr: PLongWord
): Boolean; stdcall;
var
  VExifOffset: PByte;
  VExifSize: Cardinal;
begin
  Result := FALSE;
  try
    if FindExifInJpeg(ABuffer, ASize, TRUE, $0000, VExifOffset, VExifSize) then begin
      AExifBufPtr^ := VExifOffset;
      AExifSizPtr^ := VExifSize;
      Inc(Result);
    end;
  except
  end;
end;

function HostStateChangedProc(
  const AContext: Pointer;
  const AEnabled: Boolean
): Boolean; stdcall;
begin
  Result := FALSE;
  if (nil <> AContext) then begin
    try
      if TTileStorageDLL(AContext).InternalLib_NotifyStateChanged(AEnabled) then begin
        Inc(Result);
      end;
    except
    end;
  end;
end;

{ TTileStorageDLL }

constructor TTileStorageDLL.Create(
  const AStorageTypeAbilities: ITileStorageTypeAbilities;
  const AStorageForceAbilities: ITileStorageAbilities;
  const AGeoConverter: ICoordConverter;
  const AStoragePath: string;
  const AMapVersionFactory: IMapVersionFactory;
  const AContentTypeManager: IContentTypeManager
);
var
  VStoragePath: AnsiString;
begin
  inherited Create(
    AStorageTypeAbilities,
    AStorageForceAbilities,
    AMapVersionFactory,
    AGeoConverter,
    AStoragePath
  );
  FDLLSync := MakeSyncRW_Big(Self);
  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);
  FDLLHandle := 0;
  FDLLCacheHandle := nil;
  InternalLib_CleanupProc;
  FMainContentType := AContentTypeManager.GetInfo('image/jpeg');
  VStoragePath := AnsiString(StoragePath);
  if not InternalLib_SetPath(PAnsiChar(VStoragePath)) then begin
    StorageStateInternal.ReadAccess := asEnabled;
  end;
end;

function TTileStorageDLL.DeleteTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
begin
  Result := FALSE;
end;

destructor TTileStorageDLL.Destroy;
begin
  if Assigned(StorageStateInternal) then begin
    StorageStateInternal.ReadAccess := asDisabled;
  end;

  if Assigned(FDLLSync) then begin
    FDLLSync.BeginWrite;
    try
      InternalLib_Unload;
    finally
      FDLLSync.EndWrite;
    end;
  end;

  FTileNotExistsTileInfo := nil;
  FDLLSync := nil;

  inherited;
end;

function TTileStorageDLL.GetListOfTileVersions(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionRequest
): IMapVersionListStatic;
var
  VEnumInfo: TEnumTileVersionsInfo;
  VVersionStoreString: AnsiString;
  VList: IInterfaceListSimple;
  VVersion: IMapVersionInfo;
  i: Integer;
begin
  VList := nil;

  FDLLSync.BeginRead;
  try
    if StorageStateInternal.ReadAccess <> asDisabled then begin
      VVersionStoreString := '';
      if Assigned(AVersionInfo) then begin
        VVersionStoreString := AVersionInfo.BaseVersion.StoreString;
      end;
      // init
      FillChar(VEnumInfo, sizeof(VEnumInfo), #0);
      VEnumInfo.Common.Size := SizeOf(VEnumInfo);
      VEnumInfo.Common.Zoom := AZoom;
      VEnumInfo.Common.XY := AXY;
      VEnumInfo.Common.VersionInp := PAnsiChar(VVersionStoreString);
      // call
      if InternalLib_GetTileVersions(@VEnumInfo) then begin
        if (nil <> VEnumInfo.ListOfVersions) then begin
          try
            // make version for each item
            if (TStringList(VEnumInfo.ListOfVersions).Count > 0) then begin
              VList := TInterfaceListSimple.Create;
              for i := 0 to TStringList(VEnumInfo.ListOfVersions).Count - 1 do begin
                VVersion := MapVersionFactory.CreateByStoreString(TStringList(VEnumInfo.ListOfVersions).Strings[i]);
                VList.Add(VVersion);
              end;
            end;
          finally
            FreeAndNil(VEnumInfo.ListOfVersions);
          end;
        end;
      end;
    end;
  finally
    FDLLSync.EndRead;
  end;

  Result := TMapVersionListStatic.Create(VList.MakeStaticAndClear);
end;

function TTileStorageDLL.GetTileFileName(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): string;
begin
  Result := StoragePath;
end;

function TTileStorageDLL.GetTileInfo(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
  procedure _LogToFile;
  const c_fillingmapcalls = 'C:\_gettilecalls.txt';
  var
    sl: TStringList;
    vline: String;
  begin
    sl := TStringList.Create;
    try
      if FileExists(c_fillingmapcalls) then
        sl.LoadFromFile(c_fillingmapcalls);
      vline := Format('%d - Zoom=%d, X=%d, Y = %d', [sl.Count, AZoom, AXY.X, AXY.Y]);
      sl.Add(vline);
      sl.SaveToFile(c_fillingmapcalls);
    finally
      sl.Free;
    end;
  end;
begin
  //_LogToFile;
  Result := QueryTileInternal(AXY, AZoom, AVersionInfo, AMode = gtimWithData);
end;

function TTileStorageDLL.GetTileInfoEx(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionRequest;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
var
  VVersion: IMapVersionInfo;
begin
  VVersion := nil;
  if Assigned(AVersionInfo) then begin
    VVersion := AVersionInfo.BaseVersion;
  end;
  Result := QueryTileInternal(AXY, AZoom, VVersion, AMode = gtimWithData);
end;

function TTileStorageDLL.GetTileRectInfo(
  const ARect: TRect;
  const AZoom: byte;
  const AVersionInfo: IMapVersionRequest
): ITileRectInfo;
var
  VObj: TGetTileRectFullInfo;
  VVersionStoreString: AnsiString;
  VTileCount: TPoint;
  VInfoCount: Integer;

  procedure _LogToFile;
  const c_fillingmapcalls = 'C:\_fillingmapcalls.txt';
  var
    sl: TStringList;
    vline: String;
  begin
    sl := TStringList.Create;
    try
      if FileExists(c_fillingmapcalls) then
        sl.LoadFromFile(c_fillingmapcalls);
      vline := Format('%d - Zoom=%d, Left=%d, Top=%d, Bottom = %d, Right = %d', [sl.Count, AZoom, ARect.Left, ARect.Top, ARect.Bottom, ARect.Right]);
      sl.Add(vline);
      sl.SaveToFile(c_fillingmapcalls);
    finally
      sl.Free;
    end;
  end;
var
  VVersion: IMapVersionInfo;
begin
  Result := nil;
  //Exit;

//x39 y 16
//x47 y 20
  //_LogToFile;


  if (nil=FDLLCache_QueryTileRectInfo) then
    Exit;

  FDLLSync.BeginRead;
  try
    if StorageStateInternal.ReadAccess <> asDisabled then begin
      VVersionStoreString := '';
      VVersion := nil;
      if Assigned(AVersionInfo) then begin
        VVersion := AVersionInfo.BaseVersion;
        VVersionStoreString := VVersion.StoreString;
      end;
      // init
      FillChar(VObj, sizeof(VObj), #0);
      VObj.Base.Common.Size := SizeOf(VObj);

      with VObj.Base do begin
        Common.Zoom := AZoom;
        TileRect := ARect;
        Common.VersionInp := PAnsiChar(VVersionStoreString);
      end;

      VTileCount.X := ARect.Right - ARect.Left;
      VTileCount.Y := ARect.Bottom - ARect.Top;
      VInfoCount := VTileCount.X * VTileCount.Y;
      SetLength(VObj.InfoArray, VInfoCount);

      // call
      if not InternalLib_GetTileRectInfo(@VObj) then begin
        VObj.InfoArray := nil;
        Exit;
      end;

      // make result
      Result := TTileRectInfoShort.CreateWithOwn(
        ARect,
        AZoom,
        VVersion,
        FMainContentType,
        VObj.InfoArray
      );
      VObj.InfoArray := nil;
    end;
  finally
    FDLLSync.EndRead;
  end;
end;

function TTileStorageDLL.InternalLib_CheckInitialized: Boolean;
begin
  Result := (0 <> FDLLHandle) and
    (nil <> FDLLCacheHandle) and
    (nil <> FDLLCache_EnumTileVersions) and
    (nil <> FDLLCache_QueryTile);
  // FDLLCache_ConvertImage and FDLLCache_QueryFillingMap and FDLLCache_QueryTileRectInfo can be NULL
end;

function TTileStorageDLL.InternalLib_CleanupProc: Boolean;
begin
  Result := FALSE;
  FDLLCache_EnumTileVersions := nil;
  FDLLCache_QueryTile := nil;
  FDLLCache_ConvertImage := nil;
  FDLLCache_QueryFillingMap := nil;
  FDLLCache_QueryTileRectInfo := nil;
end;

function TTileStorageDLL.InternalLib_ConvertImage(
  const AConvertImage_Context: Pointer;
  const ABuffer: Pointer;
  const ASize: Cardinal
): Boolean;
begin
  Result := FALSE;
  if (nil <> FDLLCache_ConvertImage) then begin
    Result := TDLLCache_ConvertImage(FDLLCache_ConvertImage)(AConvertImage_Context, ABuffer, ASize,
      DLLCACHE_IMG_SEC_DXT1,
      DLLCACHE_IMG_PRIMARY,
      DLLCache_ConvertImage_Callback);
  end;
end;

function TTileStorageDLL.InternalLib_GetTileRectInfo(const ATileRectInfo: PQueryTileRectInfo): Boolean;
begin
  Result := FALSE;
  if (nil <> FDLLCache_QueryTileRectInfo) then begin
    Result := TDLLCache_QueryTileRectInfo(FDLLCache_QueryTileRectInfo)(@FDLLCacheHandle, ATileRectInfo, DLLCache_QueryTileRectInfo_Callback);
  end;
end;

function TTileStorageDLL.InternalLib_GetTileVersions(const AEnumInfo: PEnumTileVersionsInfo): Boolean;
begin
  Result := FALSE;
  if (nil <> FDLLCache_EnumTileVersions) then begin
    Result := TDLLCache_EnumTileVersions(FDLLCache_EnumTileVersions)(@FDLLCacheHandle, AEnumInfo, DLLCache_EnumTileVersions_Callback);
  end;
end;

function TTileStorageDLL.InternalLib_Initialize: Boolean;
var
  p: Pointer;
begin
  Result := FALSE;
  if (0 <> FDLLHandle) then begin
    // get init proc
    p := GetProcAddress(FDLLHandle, 'DLLCache_Init');
    if (nil <> p) then begin
      Result := TDLLCache_Init(p)(@FDLLCacheHandle, 0, Self);
    end;

    if Result then begin
      // set exif reader
      p := GetProcAddress(FDLLHandle, 'DLLCache_SetInformation');
      if (nil <> p) then begin
        TDLLCache_SetInformation(p)(@FDLLCacheHandle, DLLCACHE_SIC_STATE_CHANGED, 0, @HostStateChangedProc);
        TDLLCache_SetInformation(p)(@FDLLCacheHandle, DLLCACHE_SIC_EXIF_READER, 0, @HostExifReaderProc);
      end;

      // initialized - get other functions
      FDLLCache_EnumTileVersions := GetProcAddress(FDLLHandle, 'DLLCache_EnumTileVersions');
      FDLLCache_QueryTile := GetProcAddress(FDLLHandle, 'DLLCache_QueryTile');
      FDLLCache_ConvertImage := GetProcAddress(FDLLHandle, 'DLLCache_ConvertImage');
      FDLLCache_QueryFillingMap := GetProcAddress(FDLLHandle, 'DLLCache_QueryFillingMap');
      FDLLCache_QueryTileRectInfo := GetProcAddress(FDLLHandle, 'DLLCache_QueryTileRectInfo');
    end;
  end;
end;

function TTileStorageDLL.InternalLib_NotifyStateChanged(const AEnabled: Boolean): Boolean;
var
  VReadAccess: TAccesState;
begin
  Result := FALSE;

  if AEnabled then begin
    VReadAccess := asEnabled;
  end else begin
    VReadAccess := asDisabled;
  end;

  StorageStateInternal.ReadAccess := VReadAccess;
end;

function TTileStorageDLL.InternalLib_QueryTile(const ATileInfo: PQueryTileInfo): Boolean;
begin
  Result := FALSE;
  if (nil <> FDLLCache_QueryTile) then begin
    try
      Result := TDLLCache_QueryTile(FDLLCache_QueryTile)(@FDLLCacheHandle, ATileInfo, DLLCache_QueryTile_Callback);
    except
    end;
  end;
end;

function TTileStorageDLL.InternalLib_SetPath(const APath: PAnsiChar): Boolean;
var
  p: Pointer;
begin
  Result := FALSE;
  try
    if (0 = FDLLHandle) then begin
      InternalLib_Initialize;
    end;
    if InternalLib_CheckInitialized then begin
      p := GetProcAddress(FDLLHandle, 'DLLCache_SetPath');
      if (nil <> p) then begin
        Result := TDLLCache_SetPath(p)(@FDLLCacheHandle, APath);
      end;
    end;
  finally
    InternalLib_NotifyStateChanged(Result);
  end;
end;

function TTileStorageDLL.InternalLib_Unload: Boolean;
var
  p: Pointer;
begin
  Result := FALSE;
  if (0 <> FDLLHandle) then begin
    // uninit
    p := GetProcAddress(FDLLHandle, 'DLLCache_Uninit');
    if (nil <> p) then begin
      TDLLCache_Uninit(p)(@FDLLCacheHandle);
    end;

    // finishing
    Inc(Result);
    FreeLibrary(FDLLHandle);
    FDLLHandle := 0;
    InternalLib_CleanupProc;
    InternalLib_NotifyStateChanged(FALSE);
  end;
end;

function TTileStorageDLL.QueryTileInternal(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AIsNeedData: Boolean
): ITileInfoBasic;
var
  VVersionInfo: IMapVersionInfo;
  VVersionStoreString: AnsiString;
  VQTInfo: TQueryTileInfo;
begin
  Result := nil;
  FDLLSync.BeginRead;
  try
    if StorageStateInternal.ReadAccess <> asDisabled then begin
      VVersionInfo := AVersionInfo;
      VVersionStoreString := VVersionInfo.StoreString;
      // init
      FillChar(VQTInfo, SizeOf(VQTInfo), #0);
      VQTInfo.Common.Size := SizeOf(VQTInfo);
      VQTInfo.Common.Zoom := AZoom;
      VQTInfo.Common.XY := AXY;
      VQTInfo.Common.VersionInp := PAnsiChar(VVersionStoreString);

      // load tile body or not
      if AIsNeedData then begin
        VQTInfo.Common.FlagsInp := DLLCACHE_QTI_LOAD_TILE;
        VQTInfo.TileStream := TMemoryStream.Create;
      end;

      try
        // call
        if InternalLib_QueryTile(@VQTInfo) then begin
          // check version
          if (nil = VQTInfo.VersionOut) then begin
            // no output version - may be _the_same_ version
            if (0 <> (VQTInfo.Common.FlagsOut and DLLCACHE_QTO_SAME_VERSION)) then begin
              IMapVersionInfo(VQTInfo.VersionOut) := AVersionInfo;
            end;
          end;

          // check size
          if (VQTInfo.TileSize > 0) then begin
            // tile exists
            if AIsNeedData then begin
              Result := TTileInfoBasicExistsWithTile.Create(
                VQTInfo.DateOut,
                TBinaryDataByMemStream.CreateWithOwn(TMemoryStream(VQTInfo.TileStream)),
                IMapVersionInfo(VQTInfo.VersionOut),
                FMainContentType
              );
              VQTInfo.TileStream := nil;
            end else begin
              Result := TTileInfoBasicExists.Create(
                VQTInfo.DateOut,
                VQTInfo.TileSize,
                IMapVersionInfo(VQTInfo.VersionOut),
                FMainContentType
              );
            end;
          end else if (0 <> (VQTInfo.Common.FlagsOut and DLLCACHE_QTO_TNE_EXISTS)) then begin
            // tne found
            Result := TTileInfoBasicTNE.Create(VQTInfo.DateOut, IMapVersionInfo(VQTInfo.VersionOut));
          end else begin
            // nothing
            Result := FTileNotExistsTileInfo;
          end;
        end else begin
          // nothing
          Result := FTileNotExistsTileInfo;
        end;
      finally
        IMapVersionInfo(VQTInfo.VersionOut) := nil;
        TMemoryStream(VQTInfo.TileStream).Free;
      end;
    end;
  finally
    FDLLSync.EndRead;
  end;
end;

function TTileStorageDLL.SaveTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const ALoadDate: TDateTime;
  const AContentType: IContentTypeInfoBasic;
  const AData: IBinaryData;
  const AIsOverwrite: Boolean
): Boolean;
begin
  Result := False;
  Abort;
end;

{ TTileStorageGC }

function TTileStorageGC.InternalLib_CheckInitialized: Boolean;
begin
  // common checks
  Result := inherited InternalLib_CheckInitialized;
  (*
  if Result then begin
    // special checks
  end;
  *)
end;

function TTileStorageGC.InternalLib_Initialize: Boolean;
begin
  if (0 = FDLLHandle) then begin
    FDLLHandle := LoadLibrary('TileStorage_GC.dll');
  end;

  // common routines
  Result := inherited InternalLib_Initialize;
  (*
  if Result then begin
    // special routines
  end;
  *)
end;

end.
