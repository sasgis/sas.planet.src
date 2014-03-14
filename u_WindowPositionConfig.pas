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

unit u_WindowPositionConfig;

interface

uses
  Types,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase,
  i_WindowPositionConfig;

type
  TWindowPositionConfig = class(TConfigDataElementBase, IWindowPositionConfig)
  private
    FBoundsRect: TRect;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetBoundsRect: TRect;
    procedure SetWindowPosition(const ARect: TRect);
  public
    constructor Create;
  end;


implementation

{ TWindowPositionConfig }

constructor TWindowPositionConfig.Create;
begin
  inherited Create;
  FBoundsRect := Rect(0, 0, 0, 0);
end;

procedure TWindowPositionConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FBoundsRect := Bounds(
      AConfigData.ReadInteger('Left', FBoundsRect.Left),
      AConfigData.ReadInteger('Top', FBoundsRect.Top),
      AConfigData.ReadInteger('Width', FBoundsRect.Right - FBoundsRect.Top),
      AConfigData.ReadInteger('Height', FBoundsRect.Bottom - FBoundsRect.Top)
    );
    SetChanged;
  end;
end;

procedure TWindowPositionConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('Left', FBoundsRect.Left);
  AConfigData.WriteInteger('Top', FBoundsRect.Top);
  AConfigData.WriteInteger('Width', FBoundsRect.Right - FBoundsRect.Left);
  AConfigData.WriteInteger('Height', FBoundsRect.Bottom - FBoundsRect.Top);
end;

function TWindowPositionConfig.GetBoundsRect: TRect;
begin
  LockRead;
  try
    Result := FBoundsRect;
  finally
    UnlockRead;
  end;
end;

procedure TWindowPositionConfig.SetWindowPosition(const ARect: TRect);
begin
  LockWrite;
  try
    if not EqualRect(FBoundsRect, ARect) then begin
      FBoundsRect := ARect;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
