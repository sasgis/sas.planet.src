{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_LastSelectionInfo;

interface

uses
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LastSelectionInfo,
  u_ConfigDataElementBase;

type
  TLastSelectionInfo = class(TConfigDataElementBase, ILastSelectionInfo)
  private
    // Полигон последнего выделения при операциях с областью.
    FPolygon: TArrayOfDoublePoint;
    // Масштаб, на котором было последнее выделение
    FZoom: Byte;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetZoom: Byte;
    function GetPolygon: TArrayOfDoublePoint;
    procedure SetPolygon(ALonLatPolygon: TArrayOfDoublePoint; AZoom: Byte);
  public
    constructor Create();
  end;

implementation

uses
  SysUtils;

{ TLastSelectionInfo }

constructor TLastSelectionInfo.Create;
begin
  inherited;

  FPolygon := nil;
  FZoom := 0;
end;

procedure TLastSelectionInfo.DoReadConfig(AConfigData: IConfigDataProvider);
var
  i: Integer;
  VPoint: TDoublePoint;
  VValidPoint: Boolean;
  VPolygon: TArrayOfDoublePoint;
  VZoom: Byte;
begin
  inherited;
  if AConfigData <> nil then begin
    i:=1;
    repeat
      VPoint.X := AConfigData.ReadFloat('PointX_'+inttostr(i), 1000000);
      VPoint.Y := AConfigData.ReadFloat('PointY_'+inttostr(i), 1000000);
      VValidPoint := (Abs(VPoint.X) < 360) and (Abs(VPoint.Y) < 360);
      if VValidPoint then begin
        SetLength(VPolygon, i);
        VPolygon[i - 1] := VPoint;
        inc(i);
      end;
    until not VValidPoint;
    if length(VPolygon)>0 then begin
      VZoom := AConfigData.Readinteger('Zoom', FZoom);
    end;
    SetPolygon(VPolygon, VZoom);
  end;
end;

procedure TLastSelectionInfo.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
var
  i: Integer;
begin
  inherited;
  AConfigData.DeleteValues;
  if Length(FPolygon)>0 then begin
    AConfigData.WriteInteger('Zoom', FZoom);
    for i := 0 to length(FPolygon) - 1 do begin
      AConfigData.WriteFloat('PointX_'+inttostr(i+1), FPolygon[i].x);
      AConfigData.WriteFloat('PointY_'+inttostr(i+1), FPolygon[i].y);
    end;
  end;
end;

function TLastSelectionInfo.GetPolygon: TArrayOfDoublePoint;
begin
  LockRead;
  try
    Result := Copy(FPolygon);
  finally
    UnlockRead;
  end;
end;

function TLastSelectionInfo.GetZoom: Byte;
begin
  LockRead;
  try
    Result := FZoom;
  finally
    UnlockRead;
  end;
end;

procedure TLastSelectionInfo.SetPolygon(ALonLatPolygon: TArrayOfDoublePoint;
  AZoom: Byte);
begin
  LockWrite;
  try
    FPolygon := copy(ALonLatPolygon);
    FZoom := AZoom;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
