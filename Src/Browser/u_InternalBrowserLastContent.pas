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

unit u_InternalBrowserLastContent;

interface

uses
  i_InternalBrowserLastContent,
  u_ConfigDataElementBase;

type
  TInternalBrowserLastContent = class(TConfigDataElementBaseEmptySaveLoad, IInternalBrowserLastContent)
  private
    FContent: string;
  private
    function GetContent: string;
    procedure SetContent(const AValue: string);
  end;

implementation

{ IInternalBrowserLastContent }

function TInternalBrowserLastContent.GetContent: string;
begin
  LockRead;
  try
    Result := FContent;
  finally
    UnlockRead;
  end;
end;

procedure TInternalBrowserLastContent.SetContent(const AValue: string);
begin
  LockWrite;
  try
    if FContent <> AValue then begin
      FContent := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
