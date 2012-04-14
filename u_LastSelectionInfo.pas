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

unit u_LastSelectionInfo;

interface

uses
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LastSelectionInfo,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  u_ConfigDataElementBase;

type
  TLastSelectionInfo = class(TConfigDataElementBase, ILastSelectionInfo)
  private
    FVectorItmesFactory: IVectorItmesFactory;
    // Полигон последнего выделения при операциях с областью.
    FPolygon: ILonLatPolygon;
    // Масштаб, на котором было последнее выделение
    FZoom: Byte;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetZoom: Byte;
    function GetPolygon: ILonLatPolygon;
    procedure SetPolygon(const ALonLatPolygon: ILonLatPolygon; AZoom: Byte);
  public
    constructor Create(const AVectorItmesFactory: IVectorItmesFactory);
  end;

implementation

uses
  SysUtils,
  i_DoublePointsAggregator,
  i_EnumDoublePoint,
  u_DoublePointsAggregator;

{ TLastSelectionInfo }

constructor TLastSelectionInfo.Create(const AVectorItmesFactory: IVectorItmesFactory);
begin
  inherited Create;
  FVectorItmesFactory := AVectorItmesFactory;
  FPolygon := AVectorItmesFactory.CreateLonLatPolygon(nil, 0);
  FZoom := 0;
end;

procedure TLastSelectionInfo.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  i: Integer;
  VPoint: TDoublePoint;
  VValidPoint: Boolean;
  VPolygon: IDoublePointsAggregator;
  VZoom: Byte;
begin
  inherited;
  if AConfigData <> nil then begin
    VPolygon := TDoublePointsAggregator.Create;
    i:=1;
    repeat
      VPoint.X := AConfigData.ReadFloat('PointX_'+inttostr(i), 1000000);
      VPoint.Y := AConfigData.ReadFloat('PointY_'+inttostr(i), 1000000);
      VValidPoint := (Abs(VPoint.X) < 360) and (Abs(VPoint.Y) < 360);
      if VValidPoint then begin
        VPolygon.Add(VPoint);
        inc(i);
      end;
    until not VValidPoint;
    if VPolygon.Count > 0 then begin
      VZoom := AConfigData.Readinteger('Zoom', FZoom);
      SetPolygon(FVectorItmesFactory.CreateLonLatPolygon(VPolygon.Points, VPolygon.Count), VZoom);
    end;
  end;
end;

procedure TLastSelectionInfo.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  i: Integer;
  VEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  inherited;
  AConfigData.DeleteValues;
  if FPolygon.Count > 0 then begin
    AConfigData.WriteInteger('Zoom', FZoom);
    VEnum := FPolygon.GetEnum;
    i := 1;
    while VEnum.Next(VPoint) do begin
      AConfigData.WriteFloat('PointX_'+inttostr(i), VPoint.X);
      AConfigData.WriteFloat('PointY_'+inttostr(i), VPoint.Y);
      Inc(i);
    end;
  end;
end;

function TLastSelectionInfo.GetPolygon: ILonLatPolygon;
begin
  LockRead;
  try
    Result := FPolygon;
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

procedure TLastSelectionInfo.SetPolygon(
  const ALonLatPolygon: ILonLatPolygon;
  AZoom: Byte
);
begin
  LockWrite;
  try
    FPolygon := ALonLatPolygon;
    FZoom := AZoom;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
