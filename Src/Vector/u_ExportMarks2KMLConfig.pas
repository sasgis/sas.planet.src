{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2018, SAS.Planet development team.                      *}
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

unit u_ExportMarks2KMLConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ExportMarks2KMLConfig,
  u_ConfigDataElementBase,
  u_BaseInterfacedObject;

type
  TExportMarks2KMLConfigStatic = class(TBaseInterfacedObject, IExportMarks2KMLConfigStatic)
  private
    FUseCoordFormatting: Boolean;
    FCoordPrecision: Integer;
    FSortingType: TKmlSortingType;
    FUseAbsPathToIcon: Boolean;
    FAbsPathToIcon: string;
  private
    { IExportMarks2KMLConfigStatic }
    function GetUseCoordFormatting: Boolean;
    function GetCoordPrecision: Integer;
    function GetSortingType: TKmlSortingType;
    function GetUseAbsPathToIcon: Boolean;
    function GetAbsPathToIcon: string;
  public
    constructor Create(
      const AUseCoordFormatting: Boolean;
      const ACoordPrecision: Integer;
      const ASortingType: TKmlSortingType;
      const AUseAbsPathToIcon: Boolean;
      const AAbsPathToIcon: string
    );
  end;

  TExportMarks2KMLConfig = class(TConfigDataElementWithStaticBase, IExportMarks2KMLConfig)
  private
    FUseCoordFormatting: Boolean;
    FCoordPrecision: Integer;
    FSortingType: TKmlSortingType;
    FUseAbsPathToIcon: Boolean;
    FAbsPathToIcon: string;
  private
    { IExportMarks2KMLConfig }
    function GetUseCoordFormatting: Boolean;
    procedure SetUseCoordFormatting(const AValue: Boolean);

    function GetCoordPrecision: Integer;
    procedure SetCoordPrecision(const AValue: Integer);

    function GetSortingType: TKmlSortingType;
    procedure SetSortingType(const AValue: TKmlSortingType);

    function GetUseAbsPathToIcon: Boolean;
    procedure SetUseAbsPathToIcon(const AValue: Boolean);

    function GetAbsPathToIcon: string;
    procedure SetAbsPathToIcon(const AValue: string);

    function GetStatic: IExportMarks2KMLConfigStatic;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create;
  end;

implementation

const
  cDefaultPrecision = 6;
  cDefaultSortingType = kstNone;

{ TExportMarks2KMLConfig }

constructor TExportMarks2KMLConfig.Create;
begin
  inherited Create;
  FUseCoordFormatting := False;
  FCoordPrecision := cDefaultPrecision;
  FSortingType := cDefaultSortingType;
  FUseAbsPathToIcon := False;
  FAbsPathToIcon := '';
end;

function TExportMarks2KMLConfig.CreateStatic: IInterface;
var
  VStatic: IExportMarks2KMLConfigStatic;
begin
  VStatic :=
    TExportMarks2KMLConfigStatic.Create(
      FUseCoordFormatting,
      FCoordPrecision,
      FSortingType,
      FUseAbsPathToIcon,
      FAbsPathToIcon
    );
  Result := VStatic;
end;

procedure TExportMarks2KMLConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
var
  VSortingType: Integer;
begin
  inherited;
  if AConfigData <> nil then begin
    FUseCoordFormatting := AConfigData.ReadBool('UseCoordFormatting', FUseCoordFormatting);
    FCoordPrecision := AConfigData.ReadInteger('CoordPrecision', FCoordPrecision);
    if (FCoordPrecision > 12) or (FCoordPrecision < 4) then begin
      FCoordPrecision := cDefaultPrecision;
    end;
    VSortingType := AConfigData.ReadInteger('SortingType', Integer(FSortingType));
    if (VSortingType >= Ord(Low(TKmlSortingType))) and (VSortingType <= Ord(High(TKmlSortingType))) then begin
      FSortingType := TKmlSortingType(VSortingType);
    end;
    FUseAbsPathToIcon := AConfigData.ReadBool('UseAbsPathToIcon', FUseAbsPathToIcon);
    FAbsPathToIcon := AConfigData.ReadString('AbsPathToIcon', FAbsPathToIcon);
    SetChanged;
  end;
end;

procedure TExportMarks2KMLConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('UseCoordFormatting', FUseCoordFormatting);
  AConfigData.WriteInteger('CoordPrecision', FCoordPrecision);
  AConfigData.WriteInteger('SortingType', Integer(FSortingType));
  AConfigData.WriteBool('UseAbsPathToIcon', FUseAbsPathToIcon);
  AConfigData.WriteString('AbsPathToIcon', FAbsPathToIcon);
end;

function TExportMarks2KMLConfig.GetAbsPathToIcon: string;
begin
  LockRead;
  try
    Result := FAbsPathToIcon;
  finally
    UnlockRead;
  end;
end;

function TExportMarks2KMLConfig.GetCoordPrecision: Integer;
begin
  LockRead;
  try
    Result := FCoordPrecision;
  finally
    UnlockRead;
  end;
end;

function TExportMarks2KMLConfig.GetSortingType: TKmlSortingType;
begin
  LockRead;
  try
    Result := FSortingType;
  finally
    UnlockRead;
  end;
end;

function TExportMarks2KMLConfig.GetStatic: IExportMarks2KMLConfigStatic;
begin
  Result := Self.GetStaticInternal as IExportMarks2KMLConfigStatic;
end;

function TExportMarks2KMLConfig.GetUseAbsPathToIcon: Boolean;
begin
  LockRead;
  try
    Result := FUseAbsPathToIcon;
  finally
    UnlockRead;
  end;
end;

function TExportMarks2KMLConfig.GetUseCoordFormatting: Boolean;
begin
  LockRead;
  try
    Result := FUseCoordFormatting;
  finally
    UnlockRead;
  end;
end;

procedure TExportMarks2KMLConfig.SetAbsPathToIcon(const AValue: string);
begin
  LockWrite;
  try
    if AValue <> FAbsPathToIcon then begin
      FAbsPathToIcon := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TExportMarks2KMLConfig.SetCoordPrecision(const AValue: Integer);
begin
  LockWrite;
  try
    if AValue <> FCoordPrecision then begin
      FCoordPrecision := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TExportMarks2KMLConfig.SetSortingType(const AValue: TKmlSortingType);
begin
  LockWrite;
  try
    if AValue <> FSortingType then begin
      FSortingType := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TExportMarks2KMLConfig.SetUseAbsPathToIcon(const AValue: Boolean);
begin
  LockWrite;
  try
    if AValue <> FUseAbsPathToIcon then begin
      FUseAbsPathToIcon := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TExportMarks2KMLConfig.SetUseCoordFormatting(const AValue: Boolean);
begin
  LockWrite;
  try
    if AValue <> FUseCoordFormatting then begin
      FUseCoordFormatting := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{ TExportMarks2KMLConfigStatic }

constructor TExportMarks2KMLConfigStatic.Create(
  const AUseCoordFormatting: Boolean;
  const ACoordPrecision: Integer;
  const ASortingType: TKmlSortingType;
  const AUseAbsPathToIcon: Boolean;
  const AAbsPathToIcon: string
);
begin
  inherited Create;
  FUseCoordFormatting := AUseCoordFormatting;
  FCoordPrecision := ACoordPrecision;
  FSortingType := ASortingType;
  FUseAbsPathToIcon := AUseAbsPathToIcon;
  FAbsPathToIcon := AAbsPathToIcon;
end;

function TExportMarks2KMLConfigStatic.GetAbsPathToIcon: string;
begin
  Result := FAbsPathToIcon;
end;

function TExportMarks2KMLConfigStatic.GetCoordPrecision: Integer;
begin
  Result := FCoordPrecision;
end;

function TExportMarks2KMLConfigStatic.GetSortingType: TKmlSortingType;
begin
  Result := FSortingType;
end;

function TExportMarks2KMLConfigStatic.GetUseAbsPathToIcon: Boolean;
begin
  Result := FUseAbsPathToIcon;
end;

function TExportMarks2KMLConfigStatic.GetUseCoordFormatting: Boolean;
begin
  Result := FUseCoordFormatting;
end;

end.
