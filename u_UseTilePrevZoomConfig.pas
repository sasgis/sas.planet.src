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

unit u_UseTilePrevZoomConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_UseTilePrevZoomConfig,
  u_ConfigDataElementBase;

type
  TUseTilePrevZoomConfig = class(TConfigDataElementWithStaticBase, IUseTilePrevZoomConfig)
  private
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(const AValue: Boolean);

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(const AValue: Boolean);

    function GetStatic: IUseTilePrevZoomTileConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  u_BaseInterfacedObject;

type
  TUseTilePrevZoomTileConfigStatic = class(TBaseInterfacedObject, IUseTilePrevZoomTileConfigStatic)
  private
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
  private
    function GetUsePrevZoomAtMap: Boolean;
    function GetUsePrevZoomAtLayer: Boolean;
  public
    constructor Create(
      AUsePrevZoomAtMap: Boolean;
      AUsePrevZoomAtLayer: Boolean
    );
  end;

{ TUseTilePrevZoomTileConfigStatic }

constructor TUseTilePrevZoomTileConfigStatic.Create(AUsePrevZoomAtMap,
  AUsePrevZoomAtLayer: Boolean);
begin
  inherited Create;
  FUsePrevZoomAtMap := AUsePrevZoomAtMap;
  FUsePrevZoomAtLayer := AUsePrevZoomAtLayer;
end;

function TUseTilePrevZoomTileConfigStatic.GetUsePrevZoomAtLayer: Boolean;
begin
  Result := FUsePrevZoomAtLayer;
end;

function TUseTilePrevZoomTileConfigStatic.GetUsePrevZoomAtMap: Boolean;
begin
  Result := FUsePrevZoomAtMap;
end;

{ TUseTilePrevZoomConfig }

constructor TUseTilePrevZoomConfig.Create;
begin
  inherited Create;
  FUsePrevZoomAtMap := True;
  FUsePrevZoomAtLayer := True;
end;

function TUseTilePrevZoomConfig.CreateStatic: IInterface;
var
  VStatic: IUseTilePrevZoomTileConfigStatic;
begin
  VStatic :=
    TUseTilePrevZoomTileConfigStatic.Create(
      FUsePrevZoomAtMap,
      FUsePrevZoomAtLayer
    );
  Result := VStatic;
end;

procedure TUseTilePrevZoomConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetUsePrevZoomAtMap(AConfigData.ReadBool('UsePrevZoomAtMap', FUsePrevZoomAtMap));
    SetUsePrevZoomAtLayer(AConfigData.ReadBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer));
  end;
end;

procedure TUseTilePrevZoomConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UsePrevZoomAtMap', FUsePrevZoomAtMap);
  AConfigData.WriteBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer);
end;

function TUseTilePrevZoomConfig.GetStatic: IUseTilePrevZoomTileConfigStatic;
begin
  Result := IUseTilePrevZoomTileConfigStatic(GetStaticInternal);
end;

function TUseTilePrevZoomConfig.GetUsePrevZoomAtLayer: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtLayer;
  finally
    UnlockRead;
  end;
end;

function TUseTilePrevZoomConfig.GetUsePrevZoomAtMap: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtMap;
  finally
    UnlockRead;
  end;
end;

procedure TUseTilePrevZoomConfig.SetUsePrevZoomAtLayer(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUsePrevZoomAtLayer <> AValue then begin
      FUsePrevZoomAtLayer := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TUseTilePrevZoomConfig.SetUsePrevZoomAtMap(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUsePrevZoomAtMap <> AValue then begin
      FUsePrevZoomAtMap := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
