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

unit u_BitmapMarkerProviderSimpleBase;

interface

uses
  SysUtils,
  i_Notify,
  i_BitmapMarker,
  i_BitmapMarkerProviderSimpleConfig;

type
  TBitmapMarkerProviderSimpleAbstract = class(TInterfacedObject, IBitmapMarkerProvider)
  private
    FConfig: IBitmapMarkerProviderSimpleConfigStatic;
  protected
    property Config: IBitmapMarkerProviderSimpleConfigStatic read FConfig;
  protected
    function GetMarker: IBitmapMarker; virtual; abstract;
    function GetMarkerBySize(ASize: Integer): IBitmapMarker; virtual; abstract;
  public
    constructor CreateProvider(const AConfig: IBitmapMarkerProviderSimpleConfigStatic); virtual;
  end;

  TBitmapMarkerProviderSimpleBase = class(TBitmapMarkerProviderSimpleAbstract)
  private
    FMarker: IBitmapMarker;
  protected
    function CreateMarker(ASize: Integer): IBitmapMarker; virtual; abstract;
  protected
    function GetMarker: IBitmapMarker; override;
    function GetMarkerBySize(ASize: Integer): IBitmapMarker; override;
  public
    constructor CreateProvider(const AConfig: IBitmapMarkerProviderSimpleConfigStatic); override;
  end;

  TBitmapMarkerWithDirectionProviderSimpleBase = class(TBitmapMarkerProviderSimpleAbstract, IBitmapMarkerWithDirectionProvider)
  private
    FMarker: IBitmapMarkerWithDirection;
  protected
    function CreateMarker(
      ASize: Integer;
      ADirection: Double
    ): IBitmapMarkerWithDirection; virtual; abstract;
  protected
    function GetMarker: IBitmapMarker; override;
    function GetMarkerBySize(ASize: Integer): IBitmapMarker; override;
    function GetMarkerWithRotation(const AAngle: Double): IBitmapMarkerWithDirection;
    function GetMarkerWithRotationBySize(
      const AAngle: Double;
      ASize: Integer
    ): IBitmapMarkerWithDirection;
  public
    constructor CreateProvider(const AConfig: IBitmapMarkerProviderSimpleConfigStatic); override;
  end;

  TBitmapMarkerProviderSimpleClass = class of TBitmapMarkerProviderSimpleAbstract;

  TBitmapMarkerProviderChangeableWithConfig = class(TInterfacedObject, IBitmapMarkerProviderChangeable)
  private
    FConfig: IBitmapMarkerProviderSimpleConfig;
    FProviderClass: TBitmapMarkerProviderSimpleClass;
    FProviderStatic: IBitmapMarkerProvider;

    FConfigChangeListener: IListener;
    FChangeNotifier: INotifier;
    FCS: IReadWriteSync;
    procedure OnConfigChange;
  protected
    function GetStatic: IBitmapMarkerProvider;
    function GetChangeNotifier: INotifier;
  public
    constructor Create(
      AProviderClass: TBitmapMarkerProviderSimpleClass;
      const AConfig: IBitmapMarkerProviderSimpleConfig
    );
    destructor Destroy; override;
  end;


implementation

uses
  u_Synchronizer,
  u_Notifier,
  u_NotifyEventListener,
  u_GeoFun;

const
  CAngleDelta = 1.0;

{ TBitmapMarkerProviderSimpleAbstract }

constructor TBitmapMarkerProviderSimpleAbstract.CreateProvider(
  const AConfig: IBitmapMarkerProviderSimpleConfigStatic
);
begin
  inherited Create;
  FConfig := AConfig;
end;

{ TBitmapMarkerProviderSimpleBase }

constructor TBitmapMarkerProviderSimpleBase.CreateProvider(
  const AConfig: IBitmapMarkerProviderSimpleConfigStatic
);
begin
  inherited;
  FMarker := CreateMarker(Config.MarkerSize);
end;

function TBitmapMarkerProviderSimpleBase.GetMarker: IBitmapMarker;
begin
  Result := FMarker;
end;

function TBitmapMarkerProviderSimpleBase.GetMarkerBySize(
  ASize: Integer): IBitmapMarker;
begin
  if ASize = Config.MarkerSize then begin
    Result := FMarker;
  end else begin
    Result := CreateMarker(ASize);
  end;
end;

{ TBitmapMarkerWithDirectionProviderSimpleBase }

constructor TBitmapMarkerWithDirectionProviderSimpleBase.CreateProvider(
  const AConfig: IBitmapMarkerProviderSimpleConfigStatic
);
begin
  inherited;
  FMarker := CreateMarker(Config.MarkerSize, 0);
end;

function TBitmapMarkerWithDirectionProviderSimpleBase.GetMarker: IBitmapMarker;
begin
  Result := FMarker;
end;

function TBitmapMarkerWithDirectionProviderSimpleBase.GetMarkerBySize(
  ASize: Integer): IBitmapMarker;
begin
  if ASize = Config.MarkerSize then begin
    Result := FMarker;
  end else begin
    Result := CreateMarker(ASize, FMarker.Direction);
  end;
end;

function TBitmapMarkerWithDirectionProviderSimpleBase.GetMarkerWithRotation(
  const AAngle: Double
): IBitmapMarkerWithDirection;
begin
  if (Abs(CalcAngleDelta(AAngle, FMarker.Direction)) < CAngleDelta) then begin
    Result := FMarker;
  end else begin
    Result := CreateMarker(Config.MarkerSize, AAngle);
  end;
end;

function TBitmapMarkerWithDirectionProviderSimpleBase.GetMarkerWithRotationBySize(
  const AAngle: Double;
  ASize: Integer
): IBitmapMarkerWithDirection;
begin
  if (Abs(CalcAngleDelta(AAngle, FMarker.Direction)) < CAngleDelta) and (ASize = Config.MarkerSize) then begin
    Result := FMarker;
  end else begin
    Result := CreateMarker(ASize, AAngle);
  end;
end;

{ TBitmapMarkerProviderChangeableWithConfig }

constructor TBitmapMarkerProviderChangeableWithConfig.Create(
  AProviderClass: TBitmapMarkerProviderSimpleClass;
  const AConfig: IBitmapMarkerProviderSimpleConfig
);
begin
  inherited Create;
  FProviderClass := AProviderClass;
  FConfig := AConfig;

  FCS := MakeSyncRW_Var(Self);
  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.GetChangeNotifier.Add(FConfigChangeListener);

  FChangeNotifier := TNotifierBase.Create;
  OnConfigChange;
end;

destructor TBitmapMarkerProviderChangeableWithConfig.Destroy;
begin
  FConfig.GetChangeNotifier.Remove(FConfigChangeListener);
  FConfigChangeListener := nil;

  FCS := nil;
  inherited;
end;

function TBitmapMarkerProviderChangeableWithConfig.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapMarkerProviderChangeableWithConfig.GetStatic: IBitmapMarkerProvider;
begin
  FCS.BeginRead;
  try
    Result := FProviderStatic;
  finally
    FCS.EndRead;
  end;
end;

procedure TBitmapMarkerProviderChangeableWithConfig.OnConfigChange;
var
  VProviderStatic: IBitmapMarkerProvider;
begin
  VProviderStatic := FProviderClass.CreateProvider(FConfig.GetStatic);
  FCS.BeginWrite;
  try
    FProviderStatic := VProviderStatic;
  finally
    FCS.EndWrite;
  end;
  FChangeNotifier.Notify(nil);
end;

end.



