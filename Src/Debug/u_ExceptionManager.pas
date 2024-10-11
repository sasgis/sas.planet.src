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

unit u_ExceptionManager;

interface

type
  TExceptionManager = record
    class procedure ShowExceptionInfo; static;
  end;

implementation

uses
  {$IF DEFINED(EUREKALOG)}
  Classes,
  EBase;
  {$ELSEIF DEFINED(MADEXCEPT)}
  madExcept;
  {$ELSE}
  SysUtils;
  {$IFEND}

{ TExceptionManager }

class procedure TExceptionManager.ShowExceptionInfo;
begin
  {$IF DEFINED(EUREKALOG)}
  var E := System.AcquireExceptionObject;
  try
    TThread.Synchronize(nil, procedure
    begin
      EBase.HandleException(E);
    end);
  finally
    E.Free;
  end;
  {$ELSEIF DEFINED(MADEXCEPT)}
  madExcept.HandleException;
  {$ELSE}
  SysUtils.ShowException(ExceptObject, ExceptAddr);
  {$IFEND}
end;

end.
