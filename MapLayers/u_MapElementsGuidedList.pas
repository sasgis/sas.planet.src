{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_MapElementsGuidedList;

interface

uses
  Classes,
  i_MapElementsGuidedList,
  u_GUIDInterfaceSet;

type
  TMapElementsGuidedList = class(TGUIDInterfaceSet, IMapElementsGuidedList) // TInterfacedObject, IInterfaceList
  private
    FListWithoutGUID: IInterfaceList;
  protected
    { IMapElementsGuidedList }
    function GetMapElementsWithGUID(const AGUID: TGUID): IInterfaceList;
    function GetMapElementsWithoutGUID: IInterfaceList;
    procedure CopyMapElementsToList(
      const AWithoutGUID, AWithGUID: Boolean;
      ADstList: IInterfaceList
    );
    procedure ClearMapElements;
  public
    constructor Create;
    destructor Destroy; override;
  end;

procedure CopyMapElements(
    const ASrcList: IInterfaceList;
    ADstList: IInterfaceList
  );

implementation

uses
  ActiveX;

procedure CopyMapElements(
  const ASrcList: IInterfaceList;
  ADstList: IInterfaceList
);
var
  i, k: Integer;
begin
  if not Assigned(ASrcList) then begin
    Exit;
  end;
  ASrcList.Lock;
  try
    k := ASrcList.Count;
    if (0 < k) then begin
      for i := 0 to k - 1 do begin
        ADstList.Add(ASrcList[i]);
      end;
    end;
  finally
    ASrcList.Unlock;
  end;
end;

{ TMapElementsGuidedList }

procedure TMapElementsGuidedList.ClearMapElements;
begin
  Clear;
  if Assigned(FListWithoutGUID) then begin
    FListWithoutGUID.Clear;
  end;
end;

procedure TMapElementsGuidedList.CopyMapElementsToList(
  const AWithoutGUID, AWithGUID: Boolean;
  ADstList: IInterfaceList
);
var
  VEnum: IEnumGUID;
  VGUID: TGUID;
  celtFetched: Cardinal;
begin
  if AWithoutGUID then begin
    // from list without guid
    CopyMapElements(FListWithoutGUID, ADstList);
  end;

  if AWithGUID then begin
    // from every list with guid
    VEnum := GetGUIDEnum;
    if Assigned(VEnum) then begin
      while (S_OK = VEnum.Next(1, VGUID, celtFetched)) do begin
        CopyMapElements(IInterfaceList(Pointer(GetByGUID(VGUID))), ADstList);
      end;
    end;
  end;
end;

constructor TMapElementsGuidedList.Create;
begin
  inherited Create;
  FListWithoutGUID := nil;
end;

destructor TMapElementsGuidedList.Destroy;
begin
  FListWithoutGUID := nil;
  inherited;
end;

function TMapElementsGuidedList.GetMapElementsWithGUID(const AGUID: TGUID): IInterfaceList;
begin
  // get existing
  Result := IInterfaceList(Pointer(GetByGUID(AGUID)));
  if not Assigned(Result) then begin
    // make new
    Result := TInterfaceList.Create;
    Add(AGUID, Result);
  end;
end;

function TMapElementsGuidedList.GetMapElementsWithoutGUID: IInterfaceList;
begin
  if not Assigned(FListWithoutGUID) then begin
    FListWithoutGUID := TInterfaceList.Create;
  end;
  Result := FListWithoutGUID;
end;

end.