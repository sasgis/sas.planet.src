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

unit u_DebugInfoWindow;

interface

uses
  i_DebugInfoWindow,
  i_DebugInfoSubSystem,
  i_InternalDebugConfig,
  u_BaseInterfacedObject,
  frm_DebugInfo;

type
  TDebugInfoWindow = class(TBaseInterfacedObject, IDebugInfoWindow)
  private
    FDebugInfoSubSystem: IDebugInfoSubSystem;
    FInternalDebugConfig: IInternalDebugConfig;
    FfrmDebugInfo: TfrmDebugInfo;
  private
    procedure Show;
  public
    constructor Create(
      const AInternalDebugConfig: IInternalDebugConfig;
      const ADebugInfoSubSystem: IDebugInfoSubSystem
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TDebugInfoWindow }

constructor TDebugInfoWindow.Create(
  const AInternalDebugConfig: IInternalDebugConfig;
  const ADebugInfoSubSystem: IDebugInfoSubSystem
);
begin
  inherited Create;
  FInternalDebugConfig := AInternalDebugConfig;
  FDebugInfoSubSystem := ADebugInfoSubSystem;
end;

destructor TDebugInfoWindow.Destroy;
begin
  if FfrmDebugInfo <> nil then begin
    FreeAndNil(FfrmDebugInfo);
  end;
  inherited;
end;

procedure TDebugInfoWindow.Show;
begin
  if FfrmDebugInfo = nil then begin
    if FInternalDebugConfig.IsShowDebugInfo then begin
      FfrmDebugInfo := TfrmDebugInfo.Create(nil, FDebugInfoSubSystem);
    end;
  end;
  if FfrmDebugInfo <> nil then begin
    FfrmDebugInfo.Show;
  end;
end;

end.
