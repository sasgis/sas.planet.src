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

unit u_MergePolygonsProgress;

interface

uses
  SyncObjs,
  i_VectorDataItemSimple,
  i_MergePolygonsProgress,
  u_BaseInterfacedObject;

type
  TMergePolygonsProgress = class(TBaseInterfacedObject, IMergePolygonsProgress)
  private
    FPolyCount: Integer;
    FHolesCount: Integer;
    FTime: Double;
    FVectorItem: IVectorDataItem;
    FIsFinished: Boolean;
    FStartedAt: TDateTime;
    FLock: TCriticalSection;
  public
    procedure ResetProgress;
    procedure GetProgress(
      out APolyCount: Integer;
      out AHolesCount: Integer;
      out ATime: Double;
      out AVectorItem: IVectorDataItem
    );
    procedure SetProgress(
      const APolyCount: Integer;
      const AHolesCount: Integer;
      const ATime: Double;
      const AVectorItem: IVectorDataItem
    );
    function GetFinished: Boolean;
    procedure SetFinished(const AValue: Boolean);
    function GetStartedAt: TDateTime;
    procedure SetStartedAt(const AValue: TDateTime);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TMergePolygonsProgress }

constructor TMergePolygonsProgress.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  ResetProgress;
end;

destructor TMergePolygonsProgress.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TMergePolygonsProgress.ResetProgress;
begin
  FLock.Acquire;
  try
    FPolyCount := 0;
    FHolesCount := 0;
    FTime := 0;
    FVectorItem := nil;
    FIsFinished := False;
    FStartedAt := Now;
  finally
    FLock.Release;
  end;
end;

procedure TMergePolygonsProgress.GetProgress(
  out APolyCount: Integer;
  out AHolesCount: Integer;
  out ATime: Double;
  out AVectorItem: IVectorDataItem
);
begin
  FLock.Acquire;
  try
    APolyCount := FPolyCount;
    AHolesCount := FHolesCount;
    ATime := FTime;
    AVectorItem := FVectorItem;
  finally
    FLock.Release;
  end;
end;

procedure TMergePolygonsProgress.SetProgress(
  const APolyCount: Integer;
  const AHolesCount: Integer;
  const ATime: Double;
  const AVectorItem: IVectorDataItem
);
begin
  FLock.Acquire;
  try
    FPolyCount := APolyCount;
    FHolesCount := AHolesCount;
    FTime := ATime;
    FVectorItem := AVectorItem;
  finally
    FLock.Release;
  end;
end;

function TMergePolygonsProgress.GetFinished: Boolean;
begin
  FLock.Acquire;
  try
    Result := FIsFinished;
  finally
    FLock.Release;
  end;
end;

procedure TMergePolygonsProgress.SetFinished(const AValue: Boolean);
begin
  FLock.Acquire;
  try
    FIsFinished := AValue;
  finally
    FLock.Release;
  end;
end;

function TMergePolygonsProgress.GetStartedAt: TDateTime;
begin
  FLock.Acquire;
  try
    Result := FStartedAt;
  finally
    FLock.Release;
  end;
end;

procedure TMergePolygonsProgress.SetStartedAt(const AValue: TDateTime);
begin
  FLock.Acquire;
  try
    FStartedAt := AValue;
  finally
    FLock.Release;
  end;
end;

end.
