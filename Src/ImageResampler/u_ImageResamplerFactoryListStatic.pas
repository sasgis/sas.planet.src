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

unit u_ImageResamplerFactoryListStatic;

interface

uses
  i_InterfaceListStatic,
  i_ImageResamplerFactory,
  u_BaseInterfacedObject;

type
  TImageResamplerFactoryListStatic = class(TBaseInterfacedObject, IImageResamplerFactoryList)
  private
    FList: IInterfaceListStatic;
  private
    function GetCount: Integer;
    function Get(AIndex: Integer): IImageResamplerFactory;
    function GetCaption(AIndex: Integer): string;
    function GetGUID(AIndex: Integer): TGUID;
    function GetIndexByGUID(const AGUID: TGUID): Integer;
  public
    constructor Create(const AList: IInterfaceListStatic);
  end;

  TImageResamplerFactoryListStaticEntry = class(TBaseInterfacedObject, IImageResamplerFactoryListEntry)
  private
    FFactory: IImageResamplerFactory;
    FCaption: string;
    FGUID: TGUID;
  private
    function GetFactory: IImageResamplerFactory;
    function GetCaption: string;
    function GetGUID: TGUID;
  public
    constructor Create(
      const AFactory: IImageResamplerFactory;
      const ACaption: string;
      const AGUID: TGUID
    );
  end;

implementation

uses
  SysUtils;

{ TImageResamplerFactoryListStatic }

constructor TImageResamplerFactoryListStatic.Create(const AList: IInterfaceListStatic);
begin
  Assert(Assigned(AList));
  Assert(AList.Count > 0);
  inherited Create;
  FList := AList;
end;

function TImageResamplerFactoryListStatic.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TImageResamplerFactoryListStatic.GetGUID(AIndex: Integer): TGUID;
begin
  Result := IImageResamplerFactoryListEntry(FList.Items[AIndex]).GUID;
end;

function TImageResamplerFactoryListStatic.GetIndexByGUID(
  const AGUID: TGUID
): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FList.Count - 1 do begin
    if IsEqualGUID(IImageResamplerFactoryListEntry(FList.Items[i]).GUID, AGUID) then begin
      Result := i;
      Break;
    end;
  end;
end;

function TImageResamplerFactoryListStatic.Get(AIndex: Integer): IImageResamplerFactory;
begin
  Result := IImageResamplerFactoryListEntry(FList.Items[AIndex]).Factory;
end;

function TImageResamplerFactoryListStatic.GetCaption(AIndex: Integer): string;
begin
  Result := IImageResamplerFactoryListEntry(FList.Items[AIndex]).Caption;
end;

{ TImageResamplerFactoryListStaticEntry }

constructor TImageResamplerFactoryListStaticEntry.Create(
  const AFactory: IImageResamplerFactory;
  const ACaption: string;
  const AGUID: TGUID
);
begin
  Assert(Assigned(AFactory));
  inherited Create;
  FFactory := AFactory;
  FCaption := ACaption;
  FGUID := AGUID;
end;

function TImageResamplerFactoryListStaticEntry.GetCaption: string;
begin
  Result := FCaption;
end;

function TImageResamplerFactoryListStaticEntry.GetFactory: IImageResamplerFactory;
begin
  Result := FFactory;
end;

function TImageResamplerFactoryListStaticEntry.GetGUID: TGUID;
begin
  Result := FGUID;
end;

end.
