{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_InternalDomainUrlHandlerList;

interface

uses
  i_PathConfig,
  u_InterfaceListStatic;

type
  TInternalDomainUrlHandlerList = class(TInterfaceListStatic)
  public
    constructor Create(
      const AMediaDataPath: IPathConfig
    );
  end;

implementation

uses
  Classes,
  i_InternalDomainUrlHandler,
  u_InternalDomainPhotoUrlHandler;

{ TInternalDomainUrlHandlerList }

constructor TInternalDomainUrlHandlerList.Create(
  const AMediaDataPath: IPathConfig
);
var
  VList: TList;
  VItem: IInternalDomainUrlHandler;
begin
  VList := TList.Create;
  try
    VItem := TInternalDomainPhotoUrlHandler.Create(AMediaDataPath);
    VItem._AddRef;
    VList.Add(Pointer(VItem));

    inherited CreateWithOwn(VList);

    VList := nil;
  finally
    VList.Free;
  end;
end;

end.
