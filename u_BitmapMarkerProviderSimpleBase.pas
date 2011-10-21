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

unit u_BitmapMarkerProviderSimpleBase;

interface

uses
  Types,
  i_JclNotify,
  i_BitmapMarker,
  i_BitmapMarkerProviderSimpleConfig;

type
  TBitmapMarkerProviderSimpleBase = class(TInterfacedObject, IBitmapMarkerProvider)
  private
    FConfig: IBitmapMarkerProviderSimpleConfigStatic;
    FUseDirection: Boolean;
    FDefaultDirection: Double;
    FMarker: IBitmapMarker;
  protected
    property Config: IBitmapMarkerProviderSimpleConfigStatic read FConfig;
    function CreateMarker(ASize: Integer; ADirection: Double): IBitmapMarker; virtual; abstract;
  protected
    function GetUseDirection: Boolean;

    function GetMarker: IBitmapMarker;
    function GetMarkerBySize(ASize: Integer): IBitmapMarker;
    function GetMarkerWithRotation(AAngle: Double): IBitmapMarker;
    function GetMarkerWithRotationBySize(AAngle: Double;  ASize: Integer): IBitmapMarker;
  public
    constructor CreateProvider(AConfig: IBitmapMarkerProviderSimpleConfigStatic); virtual; abstract;
    constructor Create(
      ADefaultDirection: Double;
      AConfig: IBitmapMarkerProviderSimpleConfigStatic
    );
  end;

  TBitmapMarkerProviderSimpleBaseClass = class of TBitmapMarkerProviderSimpleBase;

  TBitmapMarkerProviderChangeableWithConfig = class(TInterfacedObject, IBitmapMarkerProviderChangeable)
  private
    FConfig: IBitmapMarkerProviderSimpleConfig;
    FProviderClass: TBitmapMarkerProviderSimpleBaseClass;
    FProviderStatic: IBitmapMarkerProvider;

    FConfigChangeListener: IJclListener;
    FChangeNotifier: IJclNotifier;
    procedure OnConfigChange(Sender: TObject);
  protected
    function GetStatic: IBitmapMarkerProvider;
    function GetChangeNotifier: IJclNotifier;
  public
    constructor Create(
      AProviderClass: TBitmapMarkerProviderSimpleBaseClass;
      AConfig: IBitmapMarkerProviderSimpleConfig
    );
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils,
  u_JclNotify,
  u_NotifyEventListener,
  u_GeoFun;

const
  CAngleDelta = 1.0;

{ TBitmapMarkerProviderSimpleBase }

constructor TBitmapMarkerProviderSimpleBase.Create(
  ADefaultDirection: Double;
  AConfig: IBitmapMarkerProviderSimpleConfigStatic
);
var
  VMarkerWithDirection: IBitmapMarkerWithDirection;
begin
  FConfig := AConfig;

  FMarker := CreateMarker(FConfig.MarkerSize, ADefaultDirection);
  if Supports(FMarker, IBitmapMarkerWithDirection, VMarkerWithDirection) then begin
    FUseDirection := True;
    FDefaultDirection := VMarkerWithDirection.Direction;
  end else begin
    FUseDirection := False;
    FDefaultDirection := 0;
  end;
end;

function TBitmapMarkerProviderSimpleBase.GetMarker: IBitmapMarker;
begin
  Result := FMarker;
end;

function TBitmapMarkerProviderSimpleBase.GetMarkerBySize(
  ASize: Integer): IBitmapMarker;
begin
  if ASize = FConfig.MarkerSize then begin
    Result := FMarker;
  end else begin
    Result := CreateMarker(ASize, FDefaultDirection);
  end;
end;

function TBitmapMarkerProviderSimpleBase.GetMarkerWithRotation(
  AAngle: Double): IBitmapMarker;
begin
  if (not FUseDirection) or (Abs(CalcAngleDelta(AAngle, FDefaultDirection)) < CAngleDelta) then begin
    Result := FMarker;
  end else begin
    Result := CreateMarker(FConfig.MarkerSize, AAngle);
  end;
end;

function TBitmapMarkerProviderSimpleBase.GetMarkerWithRotationBySize(
  AAngle: Double; ASize: Integer): IBitmapMarker;
begin
  if (not FUseDirection) or (Abs(CalcAngleDelta(AAngle, FDefaultDirection)) < CAngleDelta) then begin
    Result := GetMarkerBySize(ASize);
  end else begin
    Result := CreateMarker(ASize, AAngle);
  end;
end;

function TBitmapMarkerProviderSimpleBase.GetUseDirection: Boolean;
begin
  Result := FUseDirection;
end;

{ TBitmapMarkerProviderChangeableWithConfig }

constructor TBitmapMarkerProviderChangeableWithConfig.Create(
  AProviderClass: TBitmapMarkerProviderSimpleBaseClass;
  AConfig: IBitmapMarkerProviderSimpleConfig);
begin
  FProviderClass := AProviderClass;
  FConfig := AConfig;

  FConfigChangeListener := TNotifyEventListener.Create(Self.OnConfigChange);
  FConfig.GetChangeNotifier.Add(FConfigChangeListener);

  FChangeNotifier := TJclBaseNotifier.Create;
  OnConfigChange(nil);
end;

destructor TBitmapMarkerProviderChangeableWithConfig.Destroy;
begin
  FConfig.GetChangeNotifier.Remove(FConfigChangeListener);
  FConfigChangeListener := nil;

  inherited;
end;

function TBitmapMarkerProviderChangeableWithConfig.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapMarkerProviderChangeableWithConfig.GetStatic: IBitmapMarkerProvider;
begin
  Result := FProviderStatic;
end;

procedure TBitmapMarkerProviderChangeableWithConfig.OnConfigChange(
  Sender: TObject);
begin
  FProviderStatic := FProviderClass.CreateProvider(FConfig.GetStatic);
  FChangeNotifier.Notify(nil);
end;

end.

