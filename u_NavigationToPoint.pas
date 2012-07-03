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

unit u_NavigationToPoint;

interface

uses
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarksSimple,
  i_NavigationToPoint,
  u_ConfigDataElementBase;

type
  TNavigationToPoint = class(TConfigDataElementBase, INavigationToPoint)
  private
    FIsActive: Boolean;
    FMarkId: IMarkID;
    FLonLat: TDoublePoint;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetIsActive: Boolean;
    function GetMarkId: IMarkID;
    function GetLonLat: TDoublePoint;

    procedure StartNavToMark(
      const AMarkId: IMarkID;
      const APointLonLat: TDoublePoint
    );
    procedure StartNavLonLat(const APointLonLat: TDoublePoint);
    procedure StopNav;
  public
    constructor Create;
  end;

implementation

{ TNavigationToPoint }

constructor TNavigationToPoint.Create;
begin
  inherited;
  FIsActive := False;
  FMarkId := nil;
end;

procedure TNavigationToPoint.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FIsActive := AConfigData.ReadBool('Active', FIsActive);
    { TODO -odemidov -c :  06.04.2011 10:54:49 Переделать чтение метки из параметров}
    //    FId := AConfigData.ReadInteger('ID', FId);
    FLonLat.X := AConfigData.ReadFloat('X', FLonLat.X);
    FLonLat.Y := AConfigData.ReadFloat('Y', FLonLat.Y);
    SetChanged;
  end;
end;

procedure TNavigationToPoint.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('Active', FIsActive);
  { TODO -odemidov -c :  06.04.2011 10:54:49 Переделать чтение метки из параметров}
  //  AConfigData.WriteInteger('ID', FId);
  AConfigData.WriteFloat('X', FLonLat.X);
  AConfigData.WriteFloat('Y', FLonLat.Y);
end;

function TNavigationToPoint.GetMarkId: IMarkID;
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
  LockWrite;
  try
    FIsActive := True;
    FMarkId := nil;
    FLonLat := APointLonLat;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TNavigationToPoint.StartNavToMark(
  const AMarkId: IMarkID;
  const APointLonLat: TDoublePoint
);
begin
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
