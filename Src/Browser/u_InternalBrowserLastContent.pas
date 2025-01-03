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

unit u_InternalBrowserLastContent;

interface

uses
  SysUtils,
  i_InternalBrowserLastContent,
  u_ChangeableBase;

type
  TInternalBrowserLastContent = class(TChangeableWithSimpleLockBase, IInternalBrowserLastContent)
  private
    FContent: string;
  private
    function GetContent: string;
    procedure SetContent(const AValue: string);
  public
    constructor Create;
  end;

implementation

{ IInternalBrowserLastContent }

constructor TInternalBrowserLastContent.Create;
begin
  inherited Create;
end;

function TInternalBrowserLastContent.GetContent: string;
begin
  CS.BeginRead;
  try
    Result := FContent;
  finally
    CS.EndRead;
  end;
end;

procedure TInternalBrowserLastContent.SetContent(const AValue: string);
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if FContent <> AValue then begin
      FContent := AValue;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

end.
