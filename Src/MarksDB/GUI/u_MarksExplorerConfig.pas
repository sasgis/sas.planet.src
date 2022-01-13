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
    FSelectedCategory: string;
    FExpandedCategories: string;
    FWindowPositionConfig: IWindowPositionConfig;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { IMarksExplorerConfig }
    function GetCategoriesWidth: Integer;
    procedure SetCategoriesWidth(const AValue: Integer);
    function GetExpandedCategories: string;
    procedure SetExpandedCategories(const AValue: string);
    function GetSelectedCategory: string;
    procedure SetSelectedCategory(const AValue: string);
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
  FSelectedCategory := '';
  FExpandedCategories := '';
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
    FExpandedCategories := AConfigData.ReadString('ExpandedCategories', '');
    FSelectedCategory := AConfigData.ReadString('SelectedCategory', '');
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
    AConfigData.WriteString('ExpandedCategories', FExpandedCategories);
    AConfigData.WriteString('SelectedCategory', FSelectedCategory);
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

function TMarksExplorerConfig.GetExpandedCategories: string;
begin
  LockRead;
  try
    Result := FExpandedCategories;
  finally
    UnlockRead;
  end;
end;

procedure TMarksExplorerConfig.SetExpandedCategories(const AValue: string);
begin
  LockWrite;
  try
    if FExpandedCategories <> AValue then begin
      FExpandedCategories := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TMarksExplorerConfig.GetSelectedCategory: string;
begin
  LockRead;
  try
    Result := FSelectedCategory;
  finally
    UnlockRead;
  end;
end;

procedure TMarksExplorerConfig.SetSelectedCategory(const AValue: string);
begin
  LockWrite;
  try
    if FSelectedCategory <> AValue then begin
      FSelectedCategory := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
