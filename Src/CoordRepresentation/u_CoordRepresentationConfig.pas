{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_CoordRepresentationConfig;

interface

uses
  t_CoordRepresentation,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_CoordRepresentationConfig,
  u_ConfigDataElementBase,
  u_BaseInterfacedObject;

type
  TCoordRepresentationConfigStatic = class(TBaseInterfacedObject, ICoordRepresentationConfigStatic)
  private
    FIsLatitudeFirst: Boolean;
    FDegrShowFormat: TDegrShowFormat;
    FCoordSysType: TCoordSysType;
  private
    function GetIsLatitudeFirst: Boolean;
    function GetDegrShowFormat: TDegrShowFormat;
    function GetCoordSysType: TCoordSysType;
  public
    constructor Create(
      const AIsLatitudeFirst: Boolean;
      const ADegrShowFormat: TDegrShowFormat;
      const ACoordSysType: TCoordSysType
    );
  end;

  TCoordRepresentationConfig = class(TConfigDataElementWithStaticBase, ICoordRepresentationConfig)
  private
    FIsLatitudeFirst: Boolean;
    FDegrShowFormat: TDegrShowFormat;
    FCoordSysType: TCoordSysType;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetIsLatitudeFirst: Boolean;
    procedure SetIsLatitudeFirst(const AValue: Boolean);

    function GetDegrShowFormat: TDegrShowFormat;
    procedure SetDegrShowFormat(const AValue: TDegrShowFormat);

    function GetCoordSysType: TCoordSysType;
    procedure SetCoordSysType(const AValue: TCoordSysType);

    function GetStatic: ICoordRepresentationConfigStatic;
  public
    constructor Create;
  end;

implementation

{ TCoordRepresentationConfigConfig }

constructor TCoordRepresentationConfig.Create;
begin
  inherited Create;
  FIsLatitudeFirst := True;
  FDegrShowFormat := dshCharDegrMinSec;
  FCoordSysType := cstWGS84;
end;

function TCoordRepresentationConfig.CreateStatic: IInterface;
var
  VStatic: ICoordRepresentationConfigStatic;
begin
  VStatic :=
    TCoordRepresentationConfigStatic.Create(
      FIsLatitudeFirst,
      FDegrShowFormat,
      FCoordSysType
    );
  Result := VStatic;
end;

procedure TCoordRepresentationConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    FIsLatitudeFirst := AConfigData.ReadBool('FirstLat', FIsLatitudeFirst);
    FDegrShowFormat := TDegrShowFormat(AConfigData.ReadInteger('DegrisShowFormat', Integer(FDegrShowFormat)));
    FCoordSysType := TCoordSysType(AConfigData.ReadInteger('CoordSysType', Integer(FCoordSysType)));
    SetChanged;
  end;
end;

procedure TCoordRepresentationConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('FirstLat', FIsLatitudeFirst);
  AConfigData.WriteInteger('DegrisShowFormat', Integer(FDegrShowFormat));
  AConfigData.WriteInteger('AreaShowFormat', Integer(FCoordSysType));
end;

function TCoordRepresentationConfig.GetCoordSysType: TCoordSysType;
begin
  LockRead;
  try
    Result := FCoordSysType;
  finally
    UnlockRead;
  end;
end;

function TCoordRepresentationConfig.GetDegrShowFormat: TDegrShowFormat;
begin
  LockRead;
  try
    Result := FDegrShowFormat;
  finally
    UnlockRead;
  end;
end;

function TCoordRepresentationConfig.GetIsLatitudeFirst: Boolean;
begin
  LockRead;
  try
    Result := FIsLatitudeFirst;
  finally
    UnlockRead;
  end;
end;

function TCoordRepresentationConfig.GetStatic: ICoordRepresentationConfigStatic;
begin
  Result := ICoordRepresentationConfigStatic(GetStaticInternal);
end;

procedure TCoordRepresentationConfig.SetCoordSysType(
  const AValue: TCoordSysType
);
begin
  LockWrite;
  try
    if FCoordSysType <> AValue then begin
      FCoordSysType := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCoordRepresentationConfig.SetDegrShowFormat(
  const AValue: TDegrShowFormat
);
begin
  LockWrite;
  try
    if FDegrShowFormat <> AValue then begin
      FDegrShowFormat := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCoordRepresentationConfig.SetIsLatitudeFirst(
  const AValue: Boolean
);
begin
  LockWrite;
  try
    if FIsLatitudeFirst <> AValue then begin
      FIsLatitudeFirst := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{ TCoordRepresentationConfigStatic }

constructor TCoordRepresentationConfigStatic.Create(
  const AIsLatitudeFirst: Boolean;
  const ADegrShowFormat: TDegrShowFormat;
  const ACoordSysType: TCoordSysType
);
begin
  inherited Create;
  FIsLatitudeFirst := AIsLatitudeFirst;
  FDegrShowFormat := ADegrShowFormat;
  FCoordSysType := ACoordSysType;
end;

function TCoordRepresentationConfigStatic.GetCoordSysType: TCoordSysType;
begin
  Result := FCoordSysType;
end;

function TCoordRepresentationConfigStatic.GetDegrShowFormat: TDegrShowFormat;
begin
  Result := FDegrShowFormat;
end;

function TCoordRepresentationConfigStatic.GetIsLatitudeFirst: Boolean;
begin
  Result := FIsLatitudeFirst;
end;

end.
