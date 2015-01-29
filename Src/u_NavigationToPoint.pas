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

unit u_NavigationToPoint;

interface

uses
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_NavigationToPoint,
  u_ConfigDataElementBase;

type
  TNavigationToPoint = class(TConfigDataElementBase, INavigationToPoint)
  private
    FIsActive: Boolean;
    FMarkId: string;
    FLonLat: TDoublePoint;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetIsActive: Boolean;
    function GetMarkId: string;
    function GetLonLat: TDoublePoint;

    procedure StartNavToMark(
      const AMarkId: string;
      const APointLonLat: TDoublePoint
    );
    procedure StartNavLonLat(const APointLonLat: TDoublePoint);
    procedure StopNav;
  public
    constructor Create;
  end;

implementation

uses
  u_GeoFunc;

{ TNavigationToPoint }

constructor TNavigationToPoint.Create;
begin
  inherited Create;
  FIsActive := False;
  FMarkId := '';
end;

procedure TNavigationToPoint.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FIsActive := AConfigData.ReadBool('Active', FIsActive);
    FMarkId := AConfigData.ReadString('ID', FMarkId);
    FLonLat.X := AConfigData.ReadFloat('X', FLonLat.X);
    FLonLat.Y := AConfigData.ReadFloat('Y', FLonLat.Y);
    if PointIsEmpty(FLonLat) then begin
      FIsActive := False;
    end;
    SetChanged;
  end;
end;

procedure TNavigationToPoint.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('Active', FIsActive);
  AConfigData.WriteString('ID', FMarkId);
  AConfigData.WriteFloat('X', FLonLat.X);
  AConfigData.WriteFloat('Y', FLonLat.Y);
end;

function TNavigationToPoint.GetMarkId: string;
begin
  LockRead;
  try
    Result := FMarkId;
  finally
    UnlockRead;
  end;
end;

function TNavigationToPoint.GetIsActive: Boolean;
begin
  LockRead;
  try
    Result := FIsActive;
  finally
    UnlockRead;
  end;
end;

function TNavigationToPoint.GetLonLat: TDoublePoint;
begin
  LockRead;
  try
    Result := FLonLat;
  finally
    UnlockRead;
  end;
end;

procedure TNavigationToPoint.StartNavLonLat(const APointLonLat: TDoublePoint);
begin
  if PointIsEmpty(APointLonLat) then begin
    StopNav;
  end else begin
    LockWrite;
    try
      FIsActive := True;
      FMarkId := '';
      FLonLat := APointLonLat;
      SetChanged;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TNavigationToPoint.StartNavToMark(
  const AMarkId: string;
  const APointLonLat: TDoublePoint
);
begin
  if PointIsEmpty(APointLonLat) then begin
    StopNav;
  end else begin
    LockWrite;
    try
      FIsActive := True;
      FMarkId := AMarkId;
      FLonLat := APointLonLat;
      SetChanged;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TNavigationToPoint.StopNav;
begin
  LockWrite;
  try
    FIsActive := False;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
