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

unit u_GeoCodeResult;

interface

uses
  ActiveX,
  t_Hash,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_GeoCoder,
  u_BaseInterfacedObject;

type
  TGeoCodeResult = class(TBaseInterfacedObject, IVectorItemSubset, IGeoCodeResult)
  private
    FSearchText: string;
    FMessage: string;
    FResultCode: Integer;
    FList: IVectorItemSubset;
  private
    function GetSearchText: string;
    function GetResultCode: Integer;
    function GetMessage: string;
  private
    function GetEnum: IEnumUnknown;
    function IsEmpty: Boolean;
    function IsEqual(const ASubset: IVectorItemSubset): Boolean;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IVectorDataItem;
    function GetHash: THashValue;
  public
    constructor Create(
      const ASearchText: string;
      AResultCode: integer;
      const AMessage: string;
      const AList: IVectorItemSubset
    );
  end;

implementation

{ TGeoCodeResult }

constructor TGeoCodeResult.Create(
  const ASearchText: string;
  AResultCode: integer;
  const AMessage: string;
  const AList: IVectorItemSubset
);
begin
  inherited Create;
  FSearchText := ASearchText;
  FList := AList;
  FMessage := AMessage;
  FResultCode := AResultCode;
end;

function TGeoCodeResult.GetCount: Integer;
begin
  if Assigned(FList) then begin
    Result := FList.Count;
  end else begin
    Result := 0;
  end;
end;

function TGeoCodeResult.GetEnum: IEnumUnknown;
begin
  if Assigned(FList) then begin
    Result := FList.GetEnum;
  end else begin
    Result := nil;
  end;
end;

function TGeoCodeResult.GetHash: THashValue;
begin
  if Assigned(FList) then begin
    Result := FList.Hash;
  end else begin
    Result := 0;
  end;
end;

function TGeoCodeResult.GetItem(AIndex: Integer): IVectorDataItem;
begin
  if Assigned(FList) then begin
    Result := FList.Items[AIndex];
  end else begin
    Result := nil;
  end;
end;

function TGeoCodeResult.GetMessage: string;
begin
  Result := FMessage;
end;

function TGeoCodeResult.GetResultCode: Integer;
begin
  Result := FResultCode;
end;

function TGeoCodeResult.GetSearchText: string;
begin
  Result := FSearchText;
end;

function TGeoCodeResult.IsEmpty: Boolean;
begin
  if Assigned(FList) then begin
    Result := FList.IsEmpty;
  end else begin
    Result := True;
  end;
end;

function TGeoCodeResult.IsEqual(const ASubset: IVectorItemSubset): Boolean;
begin
  if Assigned(FList) then begin
    Result := FList.IsEqual(ASubset);
  end else begin
    Result := False;
  end;
end;

end.
