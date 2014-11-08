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

unit u_VectorDataFactoryForMap;

interface

uses
  Windows,
  t_Hash,
  i_HashFunction,
  i_StringProvider,
  i_HtmlToHintTextConverter,
  i_VectorDataItemSimple,
  i_VectorDataFactory,
  u_BaseInterfacedObject;

type
  PIdData = ^TIdData;

  TIdData = record
    UrlPrefix: IStringProvider;
    NextIndex: Integer;
  end;

  TVectorDataItemMainInfoFactoryForMap = class(TBaseInterfacedObject, IVectorDataItemMainInfoFactory)
  private
    FHashFunction: IHashFunction;
    FHintConverter: IHtmlToHintTextConverter;
  private
    function BuildMainInfo(
      const AIdData: Pointer;
      const AName: string;
      const ADesc: string
    ): IVectorDataItemMainInfo;
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const AHintConverter: IHtmlToHintTextConverter
    );
  end;

implementation

uses
  SysUtils;

type
  TVectorDataItemMainInfoOfMap = class(TBaseInterfacedObject, IVectorDataItemMainInfo)
  private
    FHash: THashValue;
    FHintConverter: IHtmlToHintTextConverter;
    FUrlPrefix: IStringProvider;
    FIndex: Integer;
    FName: string;
    FDesc: string;
  private
    function GetHash: THashValue;
    function GetName: string;
    function GetDesc: string;
    function IsEqual(const AItem: IVectorDataItemMainInfo): Boolean;
    function GetHintText: string;
    function GetInfoUrl: string;
    function GetInfoCaption: string;
    function GetInfoHTML: string;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AUrlPrefix: IStringProvider;
      const AIndex: Integer;
      const AName: string;
      const ADesc: string
    );
  end;

{ TVectorDataItemMainInfoOfMap }

constructor TVectorDataItemMainInfoOfMap.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AUrlPrefix: IStringProvider;
  const AIndex: Integer;
  const AName, ADesc: string
);
begin
  inherited Create;
  FHintConverter := AHintConverter;
  FHash := AHash;
  FUrlPrefix := AUrlPrefix;
  FIndex := AIndex;
  FName := AName;
  FDesc := ADesc;
end;

function TVectorDataItemMainInfoOfMap.GetDesc: string;
begin
  Result := FDesc;
end;

function TVectorDataItemMainInfoOfMap.GetHash: THashValue;
begin
  Result := FHash;
end;

function TVectorDataItemMainInfoOfMap.GetHintText: string;
begin
  Result := FHintConverter.Convert(FName, '');
  if Result = '' then begin
    Result := FHintConverter.Convert(FName, FDesc);
  end;
end;

function TVectorDataItemMainInfoOfMap.GetInfoCaption: string;
begin
  Result := FName;
end;

function TVectorDataItemMainInfoOfMap.GetInfoHTML: string;
begin
  Result := '';
  if FDesc <> '' then begin
    Result := '<HTML><BODY>';
    Result := Result + FDesc;
    Result := Result + '</BODY></HTML>';
  end;
end;

function TVectorDataItemMainInfoOfMap.GetInfoUrl: string;
begin
  Result := FUrlPrefix.GetValue + IntToStr(FIndex) + '/';
end;

function TVectorDataItemMainInfoOfMap.GetName: string;
begin
  Result := FName;
end;

function TVectorDataItemMainInfoOfMap.IsEqual(
  const AItem: IVectorDataItemMainInfo
): Boolean;
begin
  if not Assigned(AItem) then begin
    Result := False;
    Exit;
  end;
  if AItem = IVectorDataItemMainInfo(Self) then begin
    Result := True;
    Exit;
  end;
  if (AItem.Hash <> 0) and (FHash <> 0) and (AItem.Hash <> FHash) then begin
    Result := False;
    Exit;
  end;
  if FName <> AItem.Name then begin
    Result := False;
    Exit;
  end;
  if FDesc <> AItem.Desc then begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

{ TVectorDataItemMainInfoFactory }

constructor TVectorDataItemMainInfoFactoryForMap.Create(
  const AHashFunction: IHashFunction;
  const AHintConverter: IHtmlToHintTextConverter
);
begin
  Assert(Assigned(AHashFunction));
  Assert(Assigned(AHintConverter));
  inherited Create;
  FHashFunction := AHashFunction;
  FHintConverter := AHintConverter;
end;

function TVectorDataItemMainInfoFactoryForMap.BuildMainInfo(
  const AIdData: Pointer;
  const AName, ADesc: string
): IVectorDataItemMainInfo;
var
  VIndex: Integer;
  VHash: THashValue;
begin
  Assert(AIdData <> nil);
  Result := nil;
  if AIdData <> nil then begin
    VIndex := InterlockedIncrement(PIdData(AIdData).NextIndex) - 1;
    VHash := FHashFunction.CalcHashByString(AName);
    FHashFunction.UpdateHashByString(VHash, ADesc);
    Result :=
      TVectorDataItemMainInfoOfMap.Create(
        VHash,
        FHintConverter,
        PIdData(AIdData).UrlPrefix,
        VIndex,
        AName,
        ADesc
      );
  end;
end;

end.
