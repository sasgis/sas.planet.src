{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_RegionProcessConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_StringListStatic,
  i_RegionProcessConfig,
  u_ConfigDataElementBase;

type
  TRegionProcessConfig = class(TConfigDataElementBase, IRegionProcessConfig)
  private
    FCopyBboxTmpl: IStringListStatic;
    FCopyBboxTmplActiveIndex: Integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { IRegionProcessConfig }
    function GetCopyBboxTemplates: IStringListStatic;
    procedure SetCopyBboxTemplates(const AValue: IStringListStatic);

    function GetCopyBboxTemplateActiveIndex: Integer;
    procedure SetCopyBboxTemplateActiveIndex(const AValue: Integer);
  public
    constructor Create;
  end;

implementation

uses
  u_StringListStatic,
  u_ConfigProviderHelpers;

{ TRegionProcessConfig }

constructor TRegionProcessConfig.Create;
begin
  inherited Create;

  FCopyBboxTmpl := TStringListStatic.CreateByStringDynArray(['{bbox}', '*[bbox={bbox}]']);
  FCopyBboxTmplActiveIndex := 0;
end;

procedure TRegionProcessConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;

  if AConfigData = nil then begin
    Exit;
  end;

  FCopyBboxTmplActiveIndex := AConfigData.ReadInteger('CopyBboxTmplActiveIndex', FCopyBboxTmplActiveIndex);
  FCopyBboxTmpl := ReadStringListStatic(AConfigData, 'CopyBboxTmpl', FCopyBboxTmpl);

  SetChanged;
end;

procedure TRegionProcessConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('CopyBboxTmplActiveIndex', FCopyBboxTmplActiveIndex);
  WriteStringListStatic(AConfigData, 'CopyBboxTmpl', FCopyBboxTmpl);
end;

function TRegionProcessConfig.GetCopyBboxTemplateActiveIndex: Integer;
begin
  LockRead;
  try
    Result := FCopyBboxTmplActiveIndex;
  finally
    UnlockRead;
  end;
end;

function TRegionProcessConfig.GetCopyBboxTemplates: IStringListStatic;
begin
  LockRead;
  try
    Result := FCopyBboxTmpl;
  finally
    UnlockRead;
  end;
end;

procedure TRegionProcessConfig.SetCopyBboxTemplateActiveIndex(const AValue: Integer);
begin
  LockWrite;
  try
    if FCopyBboxTmplActiveIndex <> AValue then begin
      FCopyBboxTmplActiveIndex := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TRegionProcessConfig.SetCopyBboxTemplates(const AValue: IStringListStatic);
begin
  LockWrite;
  try
    FCopyBboxTmpl := AValue;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
