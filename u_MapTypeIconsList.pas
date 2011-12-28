{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MapTypeIconsList;

interface

uses
  Windows,
  
  ActiveX,
  Graphics,
  ImgList,
  i_GUIDSet,
  i_MapTypeIconsList;

type
  TMapTypeIconsList = class(TInterfacedObject, IMapTypeIconsList)
  private
    FList: IGUIDObjectSet;
    FImageList: TCustomImageList;
    function GetImageList: TCustomImageList;
    function GetIconIndexByGUID(AGUID: TGUID): Integer;
    function GetIterator: IEnumGUID;
  public
    procedure Add(AGUID: TGUID; ABmp: TBitmap);
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_GUIDObjectSet;

{ TMapTypeIconsList }

procedure TMapTypeIconsList.Add(AGUID: TGUID; ABmp: TBitmap);
var
  VIndex: Integer;
begin
  VIndex := GetIconIndexByGUID(AGUID);
  if VIndex < 0 then begin
    VIndex := FImageList.AddMasked(Abmp, RGB(255, 0, 255));
    FList.Add(AGUID, Pointer(VIndex + 1));
  end else begin
    FImageList.ReplaceMasked(VIndex, ABmp, RGB(255, 0, 255));
  end;
end;

constructor TMapTypeIconsList.Create(AWidth, AHeight: Integer);
begin
  FImageList := TCustomImageList.Create(nil);
  FImageList.Height := AHeight;
  FImageList.Width := AWidth;
  FList := TGUIDObjectSet.Create(True);
end;

destructor TMapTypeIconsList.Destroy;
begin
  FreeAndNil(FImageList);
  FList := nil;
  inherited;
end;

function TMapTypeIconsList.GetImageList: TCustomImageList;
begin
  Result := FImageList;
end;

function TMapTypeIconsList.GetIterator: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

function TMapTypeIconsList.GetIconIndexByGUID(AGUID: TGUID): Integer;
begin
  Result := Integer(Flist.GetByGUID(AGUID)) - 1;
end;

end.
