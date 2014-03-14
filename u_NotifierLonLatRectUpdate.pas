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

unit u_NotifierLonLatRectUpdate;

interface

uses
  SysUtils,
  t_GeoTypes,
  i_Listener,
  i_LonLatRect,
  i_NotifierLonLatRectUpdate,
  u_BaseInterfacedObject;

type
  TListenerRecord = record
    Listener: IListener;
    Rect: ILonLatRect;
  end;

  TNotifierLonLatRectUpdate = class(TBaseInterfacedObject, INotifierLonLatRectUpdate, INotifierLonLatRectUpdateInternal)
  private
    FSynchronizer: IReadWriteSync;
    FCount: Integer;
    FList: array of TListenerRecord;
    function CalcGrowSize(AOldSize: Integer): Integer;
  private
    procedure Add(
      const AListener: IListener;
      const ARect: ILonLatRect
    ); stdcall;
    procedure Remove(const AListener: IListener); stdcall;
  private
    procedure RectUpdateNotify(const ARect: ILonLatRect); overload; stdcall;
    procedure RectUpdateNotify(const ARect: TDoubleRect); overload; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  u_LonLatRect,
  u_Synchronizer;

{ TNotifierLonLatRectUpdate }

constructor TNotifierLonLatRectUpdate.Create;
begin
  inherited Create;
  FSynchronizer := MakeSyncRW_Big(Self, False);
end;

destructor TNotifierLonLatRectUpdate.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do begin
    FList[i].Listener := nil;
  end;
  FList := nil;
  inherited;
end;

procedure TNotifierLonLatRectUpdate.Add(
  const AListener: IListener;
  const ARect: ILonLatRect
);
var
  i: Integer;
  VIndex: Integer;
begin
  Assert(AListener <> nil);
  FSynchronizer.BeginWrite;
  try
    VIndex := -1;
    for i := 0 to FCount - 1 do begin
      if FList[i].Listener = AListener then begin
        VIndex := i;
        Break;
      end;
    end;
    if VIndex < 0 then begin
      if FCount >= Length(FList) then begin
        SetLength(FList, CalcGrowSize(Length(FList)));
      end;
      VIndex := FCount;
      Inc(FCount);
    end;
    FList[VIndex].Listener := AListener;
    FList[VIndex].Rect := ARect;
  finally
    FSynchronizer.EndWrite;
  end;
end;

function TNotifierLonLatRectUpdate.CalcGrowSize(AOldSize: Integer): Integer;
begin
  if AOldSize < 8 then begin
    Result := 8;
  end else begin
    Result := AOldSize * 2;
  end;
end;

procedure TNotifierLonLatRectUpdate.RectUpdateNotify(const ARect: TDoubleRect);
var
  i: Integer;
  VRect: ILonLatRect;
  VList: TList;
  VListener: IListener;
begin
  VList := TList.Create;
  try
    VList.Capacity := 8;
    FSynchronizer.BeginRead;
    try
      for i := 0 to FCount - 1 do begin
        if (FList[i].Rect = nil) or FList[i].Rect.IsIntersecWithRect(ARect) then begin
          FList[i].Listener._AddRef;
          VList.Add(Pointer(FList[i].Listener));
        end;
      end;
    finally
      FSynchronizer.EndRead;
    end;
    if VList.Count > 0 then begin
      VRect := TLonLatRect.Create(ARect);
      for i := 0 to VList.Count - 1 do begin
        VListener := IListener(VList[i]);
        VListener.Notification(VRect);
        VListener._Release;
        VListener := nil;
      end;
    end;
  finally
    VList.Free;
  end;
end;

procedure TNotifierLonLatRectUpdate.RectUpdateNotify(const ARect: ILonLatRect);
var
  i: Integer;
  VList: TList;
  VListener: IListener;
begin
  VList := TList.Create;
  try
    VList.Capacity := 10;
    FSynchronizer.BeginRead;
    try
      for i := 0 to FCount - 1 do begin
        if (FList[i].Rect = nil) or FList[i].Rect.IsIntersecWithRect(ARect) then begin
          FList[i].Listener._AddRef;
          VList.Add(Pointer(FList[i].Listener));
        end;
      end;
    finally
      FSynchronizer.EndRead;
    end;
    if VList.Count > 0 then begin
      for i := 0 to VList.Count - 1 do begin
        VListener := IListener(VList[i]);
        VListener.Notification(ARect);
        VListener._Release;
        VListener := nil;
      end;
    end;
  finally
    VList.Free;
  end;
end;

procedure TNotifierLonLatRectUpdate.Remove(const AListener: IListener);
var
  i: Integer;
  VIndex: Integer;
begin
  Assert(AListener <> nil);
  FSynchronizer.BeginWrite;
  try
    VIndex := -1;
    for i := 0 to FCount - 1 do begin
      if FList[i].Listener = AListener then begin
        VIndex := i;
        Break;
      end;
    end;
    if VIndex >= 0 then begin
      FList[VIndex].Listener := nil;
      FList[VIndex].Rect := nil;
      Dec(FCount);
      if VIndex < FCount then begin
        Pointer(FList[VIndex].Listener) := Pointer(FList[FCount].Listener);
        Pointer(FList[VIndex].Rect) := Pointer(FList[FCount].Rect);
        Pointer(FList[FCount].Listener) := nil;
        Pointer(FList[FCount].Rect) := nil;
      end;
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

end.
