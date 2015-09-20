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

unit u_MarksExplorerConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase,
  i_MarksExplorerConfig,
  i_WindowPositionConfig;

type
  TMarksExplorerConfig = class(TConfigDataElementBase, IMarksExplorerConfig)
  private
    FCategoriesWidth: Integer;
    FWindowPositionConfig: IWindowPositionConfig;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { IMarksExplorerConfig }
    function GetCategoriesWidth: Integer;
    procedure SetCategoriesWidth(const AValue: Integer);
    function GetWindowPositionConfig: IWindowPositionConfig;
  public
    constructor Create;
  end;

implementation

uses
  u_WindowPositionConfig;

{ TMarksExplorerConfig }

constructor TMarksExplorerConfig.Create;
begin
  inherited Create;
  FCategoriesWidth := 0;
  FWindowPositionConfig := TWindowPositionConfig.Create;
end;

procedure TMarksExplorerConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    FWindowPositionConfig.ReadConfig(AConfigData);
    FCategoriesWidth := AConfigData.ReadInteger('CategoriesWidth', 0);
    SetChanged;
  end;
end;

procedure TMarksExplorerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    FWindowPositionConfig.WriteConfig(AConfigData);
    AConfigData.WriteInteger('CategoriesWidth', FCategoriesWidth);
  end;
end;

function TMarksExplorerConfig.GetWindowPositionConfig: IWindowPositionConfig;
begin
  LockRead;
  try
    Result := FWindowPositionConfig;
  finally
    UnlockRead;
  end;
end;

function TMarksExplorerConfig.GetCategoriesWidth: Integer;
begin
  LockRead;
  try
    Result := FCategoriesWidth;
  finally
    UnlockRead;
  end;
end;

procedure TMarksExplorerConfig.SetCategoriesWidth(const AValue: Integer);
begin
  LockWrite;
  try
    if FCategoriesWidth <> AValue then begin
      FCategoriesWidth := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
