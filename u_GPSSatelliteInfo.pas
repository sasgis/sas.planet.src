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

unit u_GPSSatelliteInfo;

interface

uses
  i_GPS;

type
  TGPSSatelliteInfo = class(TInterfacedObject, IGPSSatelliteInfo)
  private
    FPseudoRandomCode: Integer;
    FElevation: Integer;
    FAzimuth: Integer;
    FSignalToNoiseRatio: Integer;
    FIsFix: Boolean;
  protected
    function GetPseudoRandomCode: Integer; stdcall;
    function GetElevation: Integer; stdcall;
    function GetAzimuth: Integer; stdcall;
    function GetSignalToNoiseRatio: Integer; stdcall;
    function GetIsFix: Boolean; stdcall;
  public
    constructor Create(
      APseudoRandomCode: Integer;
      AElevation: Integer;
      AAzimuth: Integer;
      ASignalToNoiseRatio: Integer;
      AIsFix: Boolean
    );
    destructor Destroy; override;
  end;


implementation

{ TGPSSatelliteInfo }

constructor TGPSSatelliteInfo.Create(APseudoRandomCode, AElevation, AAzimuth,
  ASignalToNoiseRatio: Integer; AIsFix: Boolean);
begin
  FPseudoRandomCode := APseudoRandomCode;
  FElevation := AElevation;
  FAzimuth := AAzimuth;
  FSignalToNoiseRatio := ASignalToNoiseRatio;
  FIsFix := AIsFix;
end;

destructor TGPSSatelliteInfo.Destroy;
begin
  FPseudoRandomCode := 0;
  inherited;
end;

function TGPSSatelliteInfo.GetAzimuth: Integer;
begin
  Result := FAzimuth;
end;

function TGPSSatelliteInfo.GetElevation: Integer;
begin
  Result := FElevation;
end;

function TGPSSatelliteInfo.GetIsFix: Boolean;
begin
  Result := FIsFix;
end;

function TGPSSatelliteInfo.GetPseudoRandomCode: Integer;
begin
  Result := FPseudoRandomCode;
end;

function TGPSSatelliteInfo.GetSignalToNoiseRatio: Integer;
begin
  Result := FSignalToNoiseRatio;
end;

end.
