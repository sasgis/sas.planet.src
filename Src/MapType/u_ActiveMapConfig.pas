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

unit u_ActiveMapConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ActiveMapsConfig,
  u_ConfigDataElementBase;

type
  TActiveMapConfig = class(TConfigDataElementBase, IActiveMapConfig)
  private
    FIsAllowZeroGUID: Boolean;
    FMainMapGUID: TGUID;
  protected
    function GetMainMapGUID: TGUID;
    procedure SetMainMapGUID(const AMapGUID: TGUID);
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create(
      const AIsAllowZeroGUID: Boolean;
      const AMainMapGUID: TGUID
    );
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID;

const
  CKeyNameMap = 'Map';

{ TActiveMapConfig }

constructor TActiveMapConfig.Create(
  const AIsAllowZeroGUID: Boolean;
  const AMainMapGUID: TGUID
);
begin
  Assert(AIsAllowZeroGUID or not IsEqualGUID(AMainMapGUID, CGUID_Zero));
  inherited Create;
  FIsAllowZeroGUID := AIsAllowZeroGUID;
  FMainMapGUID := AMainMapGUID;
end;

procedure TActiveMapConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  VGUIDString: string;
  VGUID: TGUID;
begin
  inherited;
  VGUID := CGUID_Zero;
  if AConfigData <> nil then begin
    VGUIDString := AConfigData.ReadString(CKeyNameMap, '');
    if VGUIDString <> '' then begin
      try
        VGUID := StringToGUID(VGUIDString);
      except
        VGUID := CGUID_Zero;
      end;
    end;
  end;
  if IsEqualGUID(VGUID, CGUID_Zero) then begin
    if FIsAllowZeroGUID then begin
      SetMainMapGUID(VGUID);
    end;
  end else begin
    SetMainMapGUID(VGUID);
  end;
end;

procedure TActiveMapConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
var
  VGUIDString: string;
begin
  inherited;
  VGUIDString := GUIDToString(FMainMapGUID);
  AConfigData.WriteString(CKeyNameMap, VGUIDString);
end;

function TActiveMapConfig.GetMainMapGUID: TGUID;
begin
  LockRead;
  try
    Result := FMainMapGUID;
  finally
    UnlockRead;
  end;
end;

procedure TActiveMapConfig.SetMainMapGUID(const AMapGUID: TGUID);
begin
  if IsEqualGUID(AMapGUID, CGUID_Zero) then begin
    if FIsAllowZeroGUID then begin
      LockWrite;
      try
        if not IsEqualGUID(FMainMapGUID, AMapGUID) then begin
          FMainMapGUID := AMapGUID;
          SetChanged;
        end;
      finally
        UnlockWrite;
      end;
    end else begin
      Assert(not IsEqualGUID(AMapGUID, CGUID_Zero));
    end;
  end else begin
    LockWrite;
    try
      if not IsEqualGUID(FMainMapGUID, AMapGUID) then begin
        FMainMapGUID := AMapGUID;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

end.
