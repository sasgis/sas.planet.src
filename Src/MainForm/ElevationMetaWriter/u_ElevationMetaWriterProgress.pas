{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_ElevationMetaWriterProgress;

interface

uses
  SyncObjs,
  i_ElevationMetaWriterProgress,
  u_BaseInterfacedObject;

type
  TElevationMetaWriterProgress = class(TBaseInterfacedObject, IElevationMetaWriterProgress)
  private
    FInfo: TElevationMetaWriterProgressInfo;
    FStatus: TElevationMetaWriterProgressStatus;
    FLock: TCriticalSection;
  private
    { IElevationMetaWriterProgress }
    procedure Reset;

    function GetInfo: TElevationMetaWriterProgressInfo;
    procedure SetInfo(const AValue: TElevationMetaWriterProgressInfo);

    function GetStatus: TElevationMetaWriterProgressStatus;
    procedure SetStatus(const AValue: TElevationMetaWriterProgressStatus);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

const
  CEmptyProgressInfo: TElevationMetaWriterProgressInfo = (
    TotalCount: 0;
    ReadyCount: 0;
  );

{ TElevationMetaWriterProgress }

constructor TElevationMetaWriterProgress.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  Reset;
end;

destructor TElevationMetaWriterProgress.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TElevationMetaWriterProgress.Reset;
begin
  FLock.Acquire;
  try
    FInfo := CEmptyProgressInfo;
    FStatus := emwIdle;
  finally
    FLock.Release;
  end;
end;

function TElevationMetaWriterProgress.GetInfo: TElevationMetaWriterProgressInfo;
begin
  FLock.Acquire;
  try
    Result := FInfo;
  finally
    FLock.Release;
  end;
end;

procedure TElevationMetaWriterProgress.SetInfo(const AValue: TElevationMetaWriterProgressInfo);
begin
  FLock.Acquire;
  try
    FInfo := AValue;
  finally
    FLock.Release;
  end;
end;

function TElevationMetaWriterProgress.GetStatus: TElevationMetaWriterProgressStatus;
begin
  Result := FStatus;
end;

procedure TElevationMetaWriterProgress.SetStatus(const AValue: TElevationMetaWriterProgressStatus);
begin
  FStatus := AValue;
end;

end.
