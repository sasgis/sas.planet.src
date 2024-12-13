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

unit u_TileStorageSQLiteHelper;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  t_TileStorageSQLite,
  t_NotifierOperationRec,
  i_NotifierTime,
  i_NotifierOperation,
  i_ListenerTime,
  i_MapVersionInfo,
  i_MapVersionRequest,
  i_MapVersionListStatic,
  i_SimpleFlag,
  i_TileFileNameGenerator,
  i_TileStorage,
  i_TileStorageSQLiteHelper,
  i_TileStorageSQLiteHandler,
  i_TileStorageSQLiteHolder,
  u_TileStorageSQLiteList,
  u_BaseInterfacedObject;

type
  TTileStorageSQLiteHelper = class(TBaseInterfacedObject, ITileStorageSQLiteHelper)
  private
    FStoragePath: string;
    FDBSingleList: TSQLiteSingleList; // databases
    FTileStorageSQLiteHolder: ITileStorageSQLiteHolder;
    FFileNameGenerator: ITileFileNameGenerator;
    FUseVersion: Boolean;
    FIsReadOnly: Boolean;
    FShutdown: Boolean;

    FGCNotifier: INotifierTime;
    FSyncCallListener: IListenerTimeWithUsedFlag;
    FIsNeedCleanup: ISimpleFlag;

    procedure Sync;

    function InternalGetHandler(
      const AOper: PNotifierOperationRec;
      const AZoom: Byte;
      const AXY: TPoint;
      const AVersionInfo: IMapVersionInfo;
      const AForceMakeDB: Boolean
    ): ITileStorageSQLiteHandler; inline;

    function InternalHandlerFactory(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AForceMakeDB: Boolean;
      var ADBNotFound: Boolean
    ): ITileStorageSQLiteHandler;
  private
    { ITileStorageSQLiteHelper }
    function GetTileInfo(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode;
      const AUseOtherVersions: Boolean;
      const AResult: PGetTileResult
    ): Boolean;

    // load tile rect info
    function GetTileRectInfo(
      const AOper: PNotifierOperationRec;
      const AUseOtherVersions: Boolean;
      const AEnumData: TTileInfoShortEnumData
    ): Boolean;

    // delete tile
    function DeleteTile(
      const AOper: PNotifierOperationRec;
      const ADeleteTileAllData: PDeleteTileAllData
    ): Boolean;

    // save tile or tne to storage
    function SaveTile(
      const AOper: PNotifierOperationRec;
      const ASaveTileAllData: PSaveTileAllData
    ): Boolean;

    // get list of versions
    function GetListOfTileVersions(
      const AOper: PNotifierOperationRec;
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionRequest
    ): IMapVersionListStatic;
  public
    constructor Create(
      const AGCNotifier: INotifierTime;
      const AStoragePath: string;
      const ATileStorageSQLiteHolder: ITileStorageSQLiteHolder;
      const AFileNameGenerator: ITileFileNameGenerator;
      const AUseVersion: Boolean;
      const AIsReadOnly: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  c_TileStorageSQLite,
  u_ListenerTime,
  u_SimpleFlagWithInterlock,
  u_TileStorageSQLiteHandler;

function TileInfoModeToSQLiteMode(const AMode: TGetTileInfoMode): TGetTileInfoModeSQLite; inline;
begin
  if AMode = gtimWithData then begin
    Result := [gtiiLoadDate, gtiiSize, gtiiBody, gtiiContentType];
  end else begin
    // gtimWithoutData, gtimAsIs
    Result := [gtiiLoadDate, gtiiSize, gtiiContentType];
  end;
end;

{ TTileStorageSQLiteHelper }

constructor TTileStorageSQLiteHelper.Create(
  const AGCNotifier: INotifierTime;
  const AStoragePath: string;
  const ATileStorageSQLiteHolder: ITileStorageSQLiteHolder;
  const AFileNameGenerator: ITileFileNameGenerator;
  const AUseVersion: Boolean;
  const AIsReadOnly: Boolean
);
begin
  Assert(AGCNotifier <> nil);

  inherited Create;

  FGCNotifier := AGCNotifier;
  FStoragePath := IncludeTrailingPathDelimiter(AStoragePath);
  FTileStorageSQLiteHolder := ATileStorageSQLiteHolder;
  FFileNameGenerator := AFileNameGenerator;
  FUseVersion := AUseVersion;
  FIsReadOnly := AIsReadOnly;

  FShutdown := False;
  FDBSingleList.Init(Self.InternalHandlerFactory);

  FSyncCallListener :=
    TListenerTTLCheck.Create(
      Self.Sync,
      cStorageSyncInterval
    );
  FGCNotifier.Add(FSyncCallListener);

  FIsNeedCleanup := TSimpleFlagWithInterlock.Create;
end;

destructor TTileStorageSQLiteHelper.Destroy;
begin
  if Assigned(FGCNotifier) and Assigned(FSyncCallListener) then begin
    FGCNotifier.Remove(FSyncCallListener);
    FGCNotifier := nil;
  end;
  FSyncCallListener := nil;

  FShutdown := True;
  FDBSingleList.Uninit;

  FTileStorageSQLiteHolder := nil;
  FFileNameGenerator := nil;

  inherited Destroy;
end;

procedure TTileStorageSQLiteHelper.Sync;
begin
  FDBSingleList.Sync;

  if FIsNeedCleanup.CheckFlagAndReset then begin
    if FSyncCallListener <> nil then begin
      FSyncCallListener.CheckUseTimeUpdated;
    end;
  end;
end;

function TTileStorageSQLiteHelper.InternalGetHandler(
  const AOper: PNotifierOperationRec;
  const AZoom: Byte;
  const AXY: TPoint;
  const AVersionInfo: IMapVersionInfo;
  const AForceMakeDB: Boolean
): ITileStorageSQLiteHandler;
begin
  Result := FDBSingleList.GetHandler(AOper, AZoom, AXY, AVersionInfo, AForceMakeDB);
  FSyncCallListener.CheckUseTimeUpdated;
  FIsNeedCleanup.SetFlag;
end;

function TTileStorageSQLiteHelper.InternalHandlerFactory(
  const AOper: PNotifierOperationRec;
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AForceMakeDB: Boolean;
  var ADBNotFound: Boolean
): ITileStorageSQLiteHandler;
var
  VDBPath: String;
begin
  // get path
  VDBPath :=
    FStoragePath +
    FFileNameGenerator.GetTileFileName(AXY, AZoom) +
    cSQLiteDBFileExt;

  ADBNotFound := not FileExists(VDBPath);
  if ADBNotFound then begin
    // no database
    if FIsReadOnly or not AForceMakeDB then begin
      Result := nil;
      Exit;
    end;
    // forced
    ForceDirectories(ExtractFilePath(VDBPath));
  end;

  if AOper.IsOperationCancelled then begin
    Result := nil;
    Exit;
  end;

  // make new database or open existing
  Result :=
    TTileStorageSQLiteHandlerComplex.Create(
      FTileStorageSQLiteHolder,
      VDBPath,
      AVersionInfo,
      FUseVersion,
      FIsReadOnly
    );

  // check if opened
  if not Result.Opened then begin
    Result := nil;
  end;
end;

function TTileStorageSQLiteHelper.DeleteTile(
  const AOper: PNotifierOperationRec;
  const ADeleteTileAllData: PDeleteTileAllData
): Boolean;
var
  VHandler: ITileStorageSQLiteHandler;
begin
  if FShutdown then begin
    Result := False;
    Exit;
  end;

  // get database by xyz
  with ADeleteTileAllData^ do begin
    VHandler :=
      InternalGetHandler(
        AOper,
        DZoom,
        DXY,
        DVersionInfo,
        False
      );
  end;

  if AOper.IsOperationCancelled then begin
    Result := False;
    Exit;
  end;

  // check if database exists
  if VHandler = nil then begin
    // no database - no tile
    Result := True;
  end else begin
    // delete tile
    Result := VHandler.DeleteTile(ADeleteTileAllData);
  end;
end;

function TTileStorageSQLiteHelper.GetListOfTileVersions(
  const AOper: PNotifierOperationRec;
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionRequest
): IMapVersionListStatic;
var
  VHandler: ITileStorageSQLiteHandler;
  VVersionInfo: IMapVersionInfo;
begin
  if FShutdown or not FUseVersion then begin
    Result := nil;
    Exit;
  end;

  if Assigned(AVersion) then begin
    VVersionInfo := AVersion.BaseVersion;
  end else begin
    VVersionInfo := nil;
  end;

  // get database by xyz
  VHandler :=
    InternalGetHandler(
      AOper,
      AZoom,
      AXY,
      VVersionInfo,
      False
    );

  // check if database exists
  if (VHandler = nil) or AOper.IsOperationCancelled then begin
    // no database - no versions
    Result := nil;
  end else begin
    // delete tile
    Result :=
      VHandler.GetListOfTileVersions(
        AOper,
        AXY,
        AZoom,
        VVersionInfo
      );
  end;
end;

function TTileStorageSQLiteHelper.GetTileInfo(
  const AOper: PNotifierOperationRec;
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode;
  const AUseOtherVersions: Boolean;
  const AResult: PGetTileResult
): Boolean;
var
  VHandler: ITileStorageSQLiteHandler;
begin
  if FShutdown then begin
    Result := False;
    Exit;
  end;

  // get database by xyz
  VHandler :=
    InternalGetHandler(
      AOper,
      AZoom,
      AXY,
      AVersionInfo,
      False
    );

  // check if database exists
  if VHandler = nil then begin
    // no database - no tile
    Result := False;
  end else begin
    // obtain tile or tne or info only
    Result :=
      VHandler.GetTileInfo(
        AOper,
        AXY,
        AZoom,
        AVersionInfo,
        TileInfoModeToSQLiteMode(AMode),
        AUseOtherVersions,
        AResult
      );
  end;
end;

function TTileStorageSQLiteHelper.GetTileRectInfo(
  const AOper: PNotifierOperationRec;
  const AUseOtherVersions: Boolean;
  const AEnumData: TTileInfoShortEnumData
): Boolean;
var
  I, J: Integer;
  VXY: TPoint;
  VDBRect: TRect;
  VResult: Boolean;
  VHandler: ITileStorageSQLiteHandler;
begin
  Result := False;

  if FShutdown then begin
    Exit;
  end;

  // check zoom
  if AEnumData.DestZoom <= c_Max_Single_Zoom then begin
    // single database request without shiftings
    VHandler :=
      InternalGetHandler(
        AOper,
        AEnumData.DestZoom,
        FDBSingleList.Zero,
        AEnumData.RectVersionInfo,
        False
      );
    if VHandler <> nil then begin
      // obtain information from single database
      Result :=
        VHandler.GetTileRectInfo(
          AOper,
          AUseOtherVersions,
          AEnumData
        );
    end;
  end else begin
    // covered databases
    VDBRect.Top := AEnumData.DestRect.Top shr 8;
    VDBRect.Left := AEnumData.DestRect.Left shr 8;
    VDBRect.Bottom := (AEnumData.DestRect.Bottom - 1) shr 8;
    VDBRect.Right := (AEnumData.DestRect.Right - 1) shr 8;
    // iterate by shifted values
    for I := VDBRect.Left to VDBRect.Right do begin
      if FShutdown or AOper.IsOperationCancelled then begin
        Result := False;
        Exit;
      end;
      for J := VDBRect.Top to VDBRect.Bottom do begin
        // check
        if FShutdown or AOper.IsOperationCancelled then begin
          Result := False;
          Exit;
        end;
        // restore value
        VXY.X := I shl 8;
        VXY.Y := J shl 8;
        // get handler
        VHandler :=
          InternalGetHandler(
            AOper,
            AEnumData.DestZoom,
            VXY,
            AEnumData.RectVersionInfo,
            False
          );
        if VHandler <> nil then begin
          // obtain information from covered database
          VResult :=
            VHandler.GetTileRectInfo(
              AOper,
              AUseOtherVersions,
              AEnumData
            );
          Result := Result or VResult;
        end;
      end;
    end;
  end;
end;

function TTileStorageSQLiteHelper.SaveTile(
  const AOper: PNotifierOperationRec;
  const ASaveTileAllData: PSaveTileAllData
): Boolean;
var
  VHandler: ITileStorageSQLiteHandler;
begin
  if FShutdown then begin
    Result := False;
    Exit;
  end;

  // get database by xyz
  with ASaveTileAllData^ do begin
    VHandler :=
      InternalGetHandler(
        AOper,
        SZoom,
        SXY,
        SVersionInfo,
        True
      );
  end;

  if AOper.IsOperationCancelled then begin
    Result := False;
    Exit;
  end;

  // check if database exists
  if VHandler = nil then begin
    // no database - error
    Result := False;
  end else begin
    // save tile
    Result := VHandler.SaveTile(ASaveTileAllData);
  end;
end;

end.
