{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
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
    FGeogCoordShowFormat: TGeogCoordShowFormat;
    FProjCoordShowFormat: TProjCoordShowFormat;
    FCoordSysType: TCoordSysType;
    FCoordSysInfoType: TCoordSysInfoType;
  private
    function GetIsLatitudeFirst: Boolean;
    function GetGeogCoordShowFormat: TGeogCoordShowFormat;
    function GetProjCoordShowFormat: TProjCoordShowFormat;
    function GetCoordSysType: TCoordSysType;
    function GetCoordSysInfoType: TCoordSysInfoType;
  public
    constructor Create(
      const AIsLatitudeFirst: Boolean;
      const AGeogCoordShowFormat: TGeogCoordShowFormat;
      const AProjCoordShowFormat: TProjCoordShowFormat;
      const ACoordSysType: TCoordSysType;
      const ACoordSysInfoType: TCoordSysInfoType
    );
  end;

  TCoordRepresentationConfig = class(TConfigDataElementWithStaticBase, ICoordRepresentationConfig)
  private
    FIsLatitudeFirst: Boolean;
    FGeogCoordShowFormat: TGeogCoordShowFormat;
    FProjCoordShowFormat: TProjCoordShowFormat;
    FCoordSysType: TCoordSysType;
    FCoordSysInfoType: TCoordSysInfoType;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { ICoordRepresentationConfig }
    function GetIsLatitudeFirst: Boolean;
    procedure SetIsLatitudeFirst(const AValue: Boolean);

    function GetGeogCoordShowFormat: TGeogCoordShowFormat;
    procedure SetGeogCoordShowFormat(const AValue: TGeogCoordShowFormat);

    function GetProjCoordShowFormat: TProjCoordShowFormat;
    procedure SetProjCoordShowFormat(const AValue: TProjCoordShowFormat);

    function GetCoordSysType: TCoordSysType;
    procedure SetCoordSysType(const AValue: TCoordSysType);

    function GetCoordSysInfoType: TCoordSysInfoType;
    procedure SetCoordSysInfoType(const AValue: TCoordSysInfoType);

    function GetStatic: ICoordRepresentationConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  u_CoordRepresentation;

{ TCoordRepresentationConfigConfig }

constructor TCoordRepresentationConfig.Create;
begin
  inherited Create;
  FIsLatitudeFirst := True;
  FGeogCoordShowFormat := dshCharDegrMinSec;
  FProjCoordShowFormat := csfWhole;
  FCoordSysType := cstWGS84;
  FCoordSysInfoType := csitShowExceptWGS84;
end;

function TCoordRepresentationConfig.CreateStatic: IInterface;
var
  VStatic: ICoordRepresentationConfigStatic;
begin
  VStatic :=
    TCoordRepresentationConfigStatic.Create(
      FIsLatitudeFirst,
      FGeogCoordShowFormat,
      FProjCoordShowFormat,
      FCoordSysType,
      FCoordSysInfoType
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
    FGeogCoordShowFormat := IntegerToGeogCoordShowFormat(AConfigData.ReadInteger('DegrisShowFormat', GeogCoordShowFormatToInteger(FGeogCoordShowFormat)));
    FProjCoordShowFormat := IntegerToProjCoordShowFormat(AConfigData.ReadInteger('ProjCoordShowFormat', ProjCoordShowFormatToInteger(FProjCoordShowFormat)));
    FCoordSysType := TCoordSysType(AConfigData.ReadInteger('CoordSysType', Integer(FCoordSysType)));
    FCoordSysInfoType := TCoordSysInfoType(AConfigData.ReadInteger('CoordSysInfoType', Integer(FCoordSysInfoType)));
    SetChanged;
  end;
end;

procedure TCoordRepresentationConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('FirstLat', FIsLatitudeFirst);
  AConfigData.WriteInteger('DegrisShowFormat', GeogCoordShowFormatToInteger(FGeogCoordShowFormat));
  AConfigData.WriteInteger('ProjCoordShowFormat', ProjCoordShowFormatToInteger(FProjCoordShowFormat));
  AConfigData.WriteInteger('CoordSysType', Integer(FCoordSysType));
  AConfigData.WriteInteger('CoordSysInfoType', Integer(FCoordSysInfoType));
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

function TCoordRepresentationConfig.GetCoordSysInfoType: TCoordSysInfoType;
begin
  LockRead;
  try
    Result := FCoordSysInfoType;
  finally
    UnlockRead;
  end;
end;

function TCoordRepresentationConfig.GetGeogCoordShowFormat: TGeogCoordShowFormat;
begin
  LockRead;
  try
    Result := FGeogCoordShowFormat;
  finally
    UnlockRead;
  end;
end;

function TCoordRepresentationConfig.GetProjCoordShowFormat: TProjCoordShowFormat;
begin
  LockRead;
  try
    Result := FProjCoordShowFormat;
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

procedure TCoordRepresentationConfig.SetCoordSysInfoType(
  const AValue: TCoordSysInfoType
);
begin
  LockWrite;
  try
    if FCoordSysInfoType <> AValue then begin
      FCoordSysInfoType := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCoordRepresentationConfig.SetGeogCoordShowFormat(
  const AValue: TGeogCoordShowFormat
);
begin
  LockWrite;
  try
    if FGeogCoordShowFormat <> AValue then begin
      FGeogCoordShowFormat := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCoordRepresentationConfig.SetProjCoordShowFormat(const AValue: TProjCoordShowFormat);
begin
  LockWrite;
  try
    if FProjCoordShowFormat <> AValue then begin
      FProjCoordShowFormat := AValue;
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
  const AGeogCoordShowFormat: TGeogCoordShowFormat;
  const AProjCoordShowFormat: TProjCoordShowFormat;
  const ACoordSysType: TCoordSysType;
  const ACoordSysInfoType: TCoordSysInfoType
);
begin
  inherited Create;
  FIsLatitudeFirst := AIsLatitudeFirst;
  FGeogCoordShowFormat := AGeogCoordShowFormat;
  FProjCoordShowFormat := AProjCoordShowFormat;
  FCoordSysType := ACoordSysType;
  FCoordSysInfoType := ACoordSysInfoType;
end;

function TCoordRepresentationConfigStatic.GetCoordSysType: TCoordSysType;
begin
  Result := FCoordSysType;
end;

function TCoordRepresentationConfigStatic.GetCoordSysInfoType: TCoordSysInfoType;
begin
  Result := FCoordSysInfoType;
end;

function TCoordRepresentationConfigStatic.GetGeogCoordShowFormat: TGeogCoordShowFormat;
begin
  Result := FGeogCoordShowFormat;
end;

function TCoordRepresentationConfigStatic.GetProjCoordShowFormat: TProjCoordShowFormat;
begin
  Result := FProjCoordShowFormat;
end;

function TCoordRepresentationConfigStatic.GetIsLatitudeFirst: Boolean;
begin
  Result := FIsLatitudeFirst;
end;

end.
