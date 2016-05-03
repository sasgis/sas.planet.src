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
  i_DownloadSession,
  i_MapTypeSet,
  i_ConfigDataWriteProvider,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoDownload,
  u_BaseInterfacedObject;

type
  TRegionProcessProgressInfoDownload = class(
    TBaseInterfacedObject,
    IProgressInfoBase,
    IRegionProcessProgressInfoDownload,
    IRegionProcessProgressInfoDownloadInternal
  )
  private
    FSession: IDownloadSession;
    FCS: IReadWriteSync;
    FLog: ILogSimple;
    FLogProvider: ILogSimpleProvider;
    FFinished: Boolean;
    FNeedPause: Boolean;
    FPaused: Boolean;
    FStartTime: TDateTime;
    FTotalInRegion: Int64;
    FLastSuccessfulSize: Integer;
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
    procedure SetZoom(const AValue: Byte);
    function GetZoomArray: TByteDynArray;
    function GetLogProvider: ILogSimpleProvider;

    procedure GetLastTileInfo(
      out AZoom: Byte;
      out APoint: TPoint;
      out ASize: Integer
    );

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
    procedure AddManyProcessedTile(
      const ALastTile: TPoint;
      const ACnt: Cardinal
    );
    procedure AddProcessedTile(const ATile: TPoint);
    procedure AddDownloadedTile(
      const ATile: TPoint;
      const ASize: Cardinal
    );
    procedure AddNotNecessaryTile(const ATile: TPoint);
    procedure SetTotalToProcess(AValue: Int64);
    function GetLog: ILogSimple;
  public
    constructor Create(
      const ALog: ILogSimple;
      const ALogProvider: ILogSimpleProvider;
      const ASession: IDownloadSession;
      const APaused: Boolean
    );

  end;

implementation

uses
  Math,
  u_GeoFunc,
  u_Synchronizer;

{ TRegionProcessProgressInfoDownload }

constructor TRegionProcessProgressInfoDownload.Create(
  const ALog: ILogSimple;
  const ALogProvider: ILogSimpleProvider;
  const ASession: IDownloadSession;
  const APaused: Boolean
);
begin
  inherited Create;
  FCS := GSync.SyncVariable.Make(Self.ClassName);
  FLog := ALog;
  FLogProvider := ALogProvider;
  FSession := ASession;
  FPaused := APaused;
  FNeedPause := APaused;
  FFinished := False;
  FStartTime := Now;
  FLastSuccessfulSize := -1;
end;

procedure TRegionProcessProgressInfoDownload.AddDownloadedTile(
  const ATile: TPoint;
  const ASize: Cardinal
);
begin
  FCS.BeginWrite;
  try
    FLastSuccessfulSize := ASize;
    FSession.LastSuccessfulPoint := ATile;
    FSession.DownloadedSize := FSession.DownloadedSize + ASize;
    FSession.DownloadedCount := FSession.DownloadedCount + 1;
  finally
    FCS.EndWrite;
  end;
end;

procedure TRegionProcessProgressInfoDownload.AddManyProcessedTile(
  const ALastTile: TPoint;
  const ACnt: Cardinal
);
begin
  FCS.BeginWrite;
  try
    FSession.LastProcessedPoint := ALastTile;
    FSession.Processed := FSession.Processed + ACnt;
  finally
    FCS.EndWrite;
  end;
end;

procedure TRegionProcessProgressInfoDownload.AddNotNecessaryTile(
  const ATile: TPoint
);
begin
  FCS.BeginWrite;
  try
    FSession.LastSuccessfulPoint := ATile;
    FLastSuccessfulSize := -1;
  finally
    FCS.EndWrite;
  end;
end;

procedure TRegionProcessProgressInfoDownload.AddProcessedTile(
  const ATile: TPoint
);
begin
  FCS.BeginWrite;
  try
    FSession.LastProcessedPoint := ATile;
    FSession.Processed := FSession.Processed + 1;
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
    Result := FSession.DownloadedCount;
  finally
    FCS.EndRead;
  end;
end;

function TRegionProcessProgressInfoDownload.GetDownloadSize: UInt64;
begin
  FCS.BeginRead;
  try
    Result := FSession.DownloadedSize;
  finally
    FCS.EndRead;
  end;
end;

function TRegionProcessProgressInfoDownload.GetElapsedTime: TDateTime;
begin
  FCS.BeginRead;
  try
    if FFinished or FPaused then begin
      Result := FSession.ElapsedTime;
    end else begin
      Result := FSession.ElapsedTime + (Now - FStartTime);
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
    Result := FSession.Processed;
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
    end else if FSession.Processed > FTotalInRegion then begin
      Result := 1;
    end else begin
      Result := FSession.Processed / FTotalInRegion;
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

procedure TRegionProcessProgressInfoDownload.SetTotalToProcess(AValue: Int64);
begin
  FCS.BeginWrite;
  try
    FTotalInRegion := AValue;
  finally
    FCS.EndWrite;
  end;
end;

function TRegionProcessProgressInfoDownload.GetZoom: Byte;
begin
  FCS.BeginRead;
  try
    Result := FSession.Zoom;
  finally
    FCS.EndRead;
  end;
end;

procedure TRegionProcessProgressInfoDownload.SetZoom(const AValue: Byte);
begin
  FCS.BeginWrite;
  try
    FSession.Zoom := AValue;
  finally
    FCS.EndWrite;
  end;
end;

function TRegionProcessProgressInfoDownload.GetZoomArray: TByteDynArray;
begin
  FCS.BeginRead;
  try
    Result := FSession.ZoomArr;
  finally
    FCS.EndRead;
  end;
end;

procedure TRegionProcessProgressInfoDownload.GetLastTileInfo(
  out AZoom: Byte;
  out APoint: TPoint;
  out ASize: Integer
);
begin
  FCS.BeginRead;
  try
    AZoom := FSession.Zoom;
    APoint := FSession.LastProcessedPoint;
    if IsPointsEqual(FSession.LastSuccessfulPoint, FSession.LastProcessedPoint) then begin
      ASize := FLastSuccessfulSize;
    end else begin
      ASize := 0;
    end;
  finally
    FCS.EndRead;
  end;
end;

procedure TRegionProcessProgressInfoDownload.SaveState(
  const ASLSSection: IConfigDataWriteProvider
);
begin
  FCS.BeginWrite;
  try
    if not FNeedPause then begin
      FSession.ElapsedTime := FSession.ElapsedTime + (Now - FStartTime);
    end;
    FSession.Save(ASLSSection);
  finally
    FCS.EndWrite;
  end;
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
    FSession.ElapsedTime := FSession.ElapsedTime + (Now - FStartTime);
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

end.
