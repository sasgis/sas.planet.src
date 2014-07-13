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

unit u_RegionProcessProgressInfoDownload;

interface

uses
  Types,
  SysUtils,
  i_LogSimple,
  i_LogSimpleProvider,
  i_GeometryLonLat,
  i_MapVersionInfo,
  i_MapVersionRequest,
  i_ConfigDataWriteProvider,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoDownload,
  u_BaseInterfacedObject;

type
  TRegionProcessProgressInfoDownload = class(TBaseInterfacedObject, IProgressInfoBase, IRegionProcessProgressInfoDownload, IRegionProcessProgressInfoDownloadInternal)
  private
    FGUID: TGUID;
    FZoom: Byte;
    FPolygon: IGeometryLonLatPolygon;
    FVersionForCheck: IMapVersionRequest;
    FVersionForDownload: IMapVersionInfo;

    FSecondLoadTNE: boolean;
    FReplaceExistTiles: boolean;
    FCheckExistTileSize: boolean;
    FCheckExistTileDate: boolean;
    FCheckTileDate: TDateTime;

    FCS: IReadWriteSync;
    FLog: ILogSimple;
    FLogProvider: ILogSimpleProvider;
    FProcessedRatio: Double;
    FFinished: Boolean;
    FNeedPause: Boolean;
    FPaused: Boolean;
    FElapsedTime: TDateTime;
    FStartTime: TDateTime;
    FTotalInRegion: Int64;
    FProcessed: Int64;
    FDownloadedSize: UInt64;
    FDownloadedCount: Int64;
    FLastProcessedPoint: TPoint;
    FLastSuccessfulPoint: TPoint;
  private
    function GetProcessedRatio: Double;
    function GetFinished: Boolean;
  private
    function GetTotalToProcess: Int64;
    function GetDownloaded: Int64;
    function GetProcessed: Int64;
    function GetDownloadSize: UInt64;
    function GetElapsedTime: TDateTime;
    function GetZoom: Byte;
    function GetLogProvider: ILogSimpleProvider;

    function GetIsPaused: Boolean;
    procedure Pause;
    procedure Resume;
    procedure SaveState(const ASLSSection: IConfigDataWriteProvider);
  private
    function GetNeedPause: Boolean;
    procedure SetNeedPause(AValue: Boolean);
    procedure Finish;
    procedure SetPaused;
    procedure SetStarted;
    procedure AddManyProcessedTile(const ALastTile: TPoint; const ACnt: Cardinal);
    procedure AddProcessedTile(const ATile: TPoint);
    procedure AddDownloadedTile(const ATile: TPoint; const ASize: Cardinal);
    procedure AddNotNecessaryTile(const ATile: TPoint);
    procedure SetTotalToProcess(AValue: Int64);
    function GetLog: ILogSimple;
  public
    constructor Create(
      const ALog: ILogSimple;
      const ALogProvider: ILogSimpleProvider;
      const AGUID: TGUID;
      const AVersionForCheck: IMapVersionRequest;
      const AVersionForDownload: IMapVersionInfo;
      AZoom: Byte;
      const APolygon: IGeometryLonLatPolygon;
      ASecondLoadTNE: boolean;
      AReplaceExistTiles: boolean;
      ACheckExistTileSize: boolean;
      ACheckExistTileDate: boolean;
      ACheckTileDate: TDateTime;
      APaused: Boolean;
      const ADownloadedSize: UInt64;
      const ADownloadedCount: Int64;
      const ALastProcessedPoint: TPoint;
      const AElapsedTime: TDateTime
    );
  end;

implementation

uses
  u_ConfigProviderHelpers,
  u_Synchronizer;

{ TRegionProcessProgressInfoDownload }

constructor TRegionProcessProgressInfoDownload.Create(
  const ALog: ILogSimple;
  const ALogProvider: ILogSimpleProvider;
  const AGUID: TGUID;
  const AVersionForCheck: IMapVersionRequest;
  const AVersionForDownload: IMapVersionInfo;
  AZoom: Byte;
  const APolygon: IGeometryLonLatPolygon;
  ASecondLoadTNE: boolean;
  AReplaceExistTiles: boolean;
  ACheckExistTileSize: boolean;
  ACheckExistTileDate: boolean;
  ACheckTileDate: TDateTime;
  APaused: Boolean;
  const ADownloadedSize: UInt64;
  const ADownloadedCount: Int64;
  const ALastProcessedPoint: TPoint;
  const AElapsedTime: TDateTime
);
begin
  inherited Create;
  FGUID := AGUID;
  FVersionForCheck := AVersionForCheck;
  FVersionForDownload := AVersionForDownload;
  FZoom := AZoom;
  FPolygon := APolygon;
  FSecondLoadTNE := ASecondLoadTNE;
  FReplaceExistTiles := AReplaceExistTiles;
  FCheckExistTileSize := ACheckExistTileSize;
  FCheckExistTileDate := ACheckExistTileDate;
  FCheckTileDate := ACheckTileDate;

  FCS := GSync.SyncVariable.Make(Self.ClassName);

  FLog := ALog;
  FLogProvider := ALogProvider;

  FPaused := APaused;
  FNeedPause := APaused;
  FProcessedRatio := 0;
  FFinished := False;
  FElapsedTime := FElapsedTime;
  FStartTime := Now;
  FTotalInRegion := 0;
  FProcessed := 0;
  FDownloadedSize := ADownloadedSize;
  FDownloadedCount := ADownloadedCount;
  FLastProcessedPoint := ALastProcessedPoint;
  FLastSuccessfulPoint := Point(-1, -1);
end;

procedure TRegionProcessProgressInfoDownload.AddDownloadedTile(
  const ATile: TPoint;
  const ASize: Cardinal
);
begin
  FCS.BeginWrite;
  try
    FLastSuccessfulPoint := ATile;
    Inc(FDownloadedSize, ASize);
    Inc(FDownloadedCount);
  finally
    FCS.EndWrite;
  end;
end;

procedure TRegionProcessProgressInfoDownload.AddManyProcessedTile(
  const ALastTile: TPoint; const ACnt: Cardinal);
begin
  FCS.BeginWrite;
  try
    FLastProcessedPoint := ALastTile;
    Inc(FProcessed, ACnt);
  finally
    FCS.EndWrite;
  end;
end;

procedure TRegionProcessProgressInfoDownload.AddNotNecessaryTile(
  const ATile: TPoint);
begin
  FCS.BeginWrite;
  try
    FLastSuccessfulPoint := ATile;
  finally
    FCS.EndWrite;
  end;
end;

procedure TRegionProcessProgressInfoDownload.AddProcessedTile(
  const ATile: TPoint);
begin
  FCS.BeginWrite;
  try
    FLastProcessedPoint := ATile;
    Inc(FProcessed);
  finally
    FCS.EndWrite;
  end;
end;

procedure TRegionProcessProgressInfoDownload.Finish;
begin
  FCS.BeginWrite;
  try
    FFinished := True;
  finally
    FCS.EndWrite;
  end;
end;

function TRegionProcessProgressInfoDownload.GetDownloaded: Int64;
begin
  FCS.BeginRead;
  try
    Result := FDownloadedCount
  finally
    FCS.EndRead;
  end;
end;

function TRegionProcessProgressInfoDownload.GetDownloadSize: UInt64;
begin
  FCS.BeginRead;
  try
    Result := FDownloadedSize
  finally
    FCS.EndRead;
  end;
end;

function TRegionProcessProgressInfoDownload.GetElapsedTime: TDateTime;
begin
  FCS.BeginRead;
  try
    if FFinished or FPaused then begin
      Result := FElapsedTime;
    end else begin
      Result := FElapsedTime + (Now - FStartTime);
    end;
  finally
    FCS.EndRead;
  end;
end;

function TRegionProcessProgressInfoDownload.GetFinished: Boolean;
begin
  FCS.BeginRead;
  try
    Result := FFinished;
  finally
    FCS.EndRead;
  end;
end;

function TRegionProcessProgressInfoDownload.GetIsPaused: Boolean;
begin
  FCS.BeginRead;
  try
    Result := FNeedPause;
  finally
    FCS.EndRead;
  end;
end;

function TRegionProcessProgressInfoDownload.GetLog: ILogSimple;
begin
  Result := FLog;
end;

function TRegionProcessProgressInfoDownload.GetLogProvider: ILogSimpleProvider;
begin
  Result := FLogProvider;
end;

function TRegionProcessProgressInfoDownload.GetNeedPause: Boolean;
begin
  FCS.BeginRead;
  try
    Result := FNeedPause;
  finally
    FCS.EndRead;
  end;
end;

function TRegionProcessProgressInfoDownload.GetProcessed: Int64;
begin
  FCS.BeginRead;
  try
    Result := FProcessed
  finally
    FCS.EndRead;
  end;
end;

function TRegionProcessProgressInfoDownload.GetProcessedRatio: Double;
begin
  FCS.BeginRead;
  try
    if FFinished then begin
      Result := 1;
    end else if FTotalInRegion = 0 then begin
      Result := 0;
    end else if FProcessed > FTotalInRegion then begin
      Result := 1;
    end else begin
      Result := FProcessed / FTotalInRegion;
    end;
  finally
    FCS.EndRead;
  end;
end;

function TRegionProcessProgressInfoDownload.GetTotalToProcess: Int64;
begin
  FCS.BeginRead;
  try
    Result := FTotalInRegion
  finally
    FCS.EndRead;
  end;
end;

function TRegionProcessProgressInfoDownload.GetZoom: Byte;
begin
  Result := FZoom;
end;

procedure TRegionProcessProgressInfoDownload.Pause;
begin
  FCS.BeginWrite;
  try
    FNeedPause := True;
  finally
    FCS.EndWrite;
  end;
end;

procedure TRegionProcessProgressInfoDownload.Resume;
begin
  FCS.BeginWrite;
  try
    FNeedPause := False;
  finally
    FCS.EndWrite;
  end;
end;

procedure TRegionProcessProgressInfoDownload.SaveState(
  const ASLSSection: IConfigDataWriteProvider);
var
  VElapsedTime: TDateTime;
  VVersionForCheck: string;
begin
  VVersionForCheck := '';
  if Assigned(FVersionForCheck.BaseVersion) then begin
    VVersionForCheck := FVersionForCheck.BaseVersion.StoreString;
  end;
  ASLSSection.WriteString('MapGUID', GUIDToString(FGUID));
  ASLSSection.WriteString('VersionDownload', FVersionForDownload.StoreString);
  ASLSSection.WriteString('VersionCheck', VVersionForCheck);
  ASLSSection.WriteBool('VersionCheckPrev', FVersionForCheck.ShowPrevVersion);
  ASLSSection.WriteInteger('Zoom', FZoom + 1);
  ASLSSection.WriteBool('ReplaceExistTiles', FReplaceExistTiles);
  ASLSSection.WriteBool('CheckExistTileSize', FCheckExistTileSize);
  ASLSSection.WriteBool('CheckExistTileDate', FCheckExistTileDate);
  ASLSSection.WriteDate('CheckTileDate', FCheckTileDate);
  ASLSSection.WriteBool('SecondLoadTNE', FSecondLoadTNE);
  FCS.BeginRead;
  try
    ASLSSection.WriteInteger('ProcessedTileCount', FDownloadedCount);
    ASLSSection.WriteInteger('Processed', FProcessed);
    ASLSSection.WriteFloat('ProcessedSize', FDownloadedSize / 1024);
    ASLSSection.WriteInteger('StartX', FLastProcessedPoint.X);
    ASLSSection.WriteInteger('StartY', FLastProcessedPoint.Y);
    ASLSSection.WriteInteger('LastSuccessfulStartX', FLastSuccessfulPoint.X);
    ASLSSection.WriteInteger('LastSuccessfulStartY', FLastSuccessfulPoint.Y);
    WritePolygon(ASLSSection, FPolygon);
    if FNeedPause then begin
      VElapsedTime := FElapsedTime;
    end else begin
      VElapsedTime := FElapsedTime + (Now - FStartTime);
    end;
    ASLSSection.WriteFloat('ElapsedTime', VElapsedTime);
  finally
    FCS.EndRead;
  end;
end;

procedure TRegionProcessProgressInfoDownload.SetNeedPause(AValue: Boolean);
begin
  FCS.BeginWrite;
  try
    FNeedPause := AValue;
  finally
    FCS.EndWrite;
  end;
end;

procedure TRegionProcessProgressInfoDownload.SetPaused;
begin
  FCS.BeginWrite;
  try
    FPaused := True;
    FElapsedTime := FElapsedTime + (Now - FStartTime);
  finally
    FCS.EndWrite;
  end;
end;

procedure TRegionProcessProgressInfoDownload.SetStarted;
begin
  FCS.BeginWrite;
  try
    FPaused := False;
    FStartTime := Now;
  finally
    FCS.EndWrite;
  end;
end;

procedure TRegionProcessProgressInfoDownload.SetTotalToProcess(AValue: Int64);
begin
  FCS.BeginWrite;
  try
    FTotalInRegion := AValue;
  finally
    FCS.EndWrite;
  end;
end;

end.
