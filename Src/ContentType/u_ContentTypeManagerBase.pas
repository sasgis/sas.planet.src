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

unit u_ContentTypeManagerBase;

interface

uses
  Classes,
  i_BitmapTileSaveLoad,
  i_ContentTypeInfo,
  i_ContentConverter,
  i_ContentTypeManager,
  i_StringListStatic,
  u_ContentTypeListByKey,
  u_ContentConverterMatrix,
  u_BaseInterfacedObject;

type
  TContentTypeManagerBase = class(
    TBaseInterfacedObject,
    IContentTypeManager,
    IContentTypeManagerBitmap
  )
  private
    FExtList: TContentTypeListByKey;
    FTypeList: TContentTypeListByKey;
    FBitmapExtList: TContentTypeListByKey;
    FBitmapTypeList: TContentTypeListByKey;
    FVectorExtList: TContentTypeListByKey;
    FVectorTypeList: TContentTypeListByKey;
    FConverterMatrix: TContentConverterMatrix;
  protected
    procedure AddByType(
      const AInfo: IContentTypeInfoBasic;
      const AType: AnsiString
    );
    procedure AddByExt(
      const AInfo: IContentTypeInfoBasic;
      const AExt: AnsiString
    );
    property ExtList: TContentTypeListByKey read FExtList;
    property TypeList: TContentTypeListByKey read FTypeList;
    property BitmapExtList: TContentTypeListByKey read FBitmapExtList;
    property BitmapTypeList: TContentTypeListByKey read FBitmapTypeList;
    property VectorExtList: TContentTypeListByKey read FVectorExtList;
    property VectorTypeList: TContentTypeListByKey read FVectorTypeList;
    property ConverterMatrix: TContentConverterMatrix read FConverterMatrix;
  private
    function GetInfo(const AType: AnsiString): IContentTypeInfoBasic;
    function GetInfoByExt(const AExt: AnsiString): IContentTypeInfoBasic;
    function GetIsBitmapType(const AType: AnsiString): Boolean;
    function GetIsBitmapExt(const AExt: AnsiString): Boolean;
    function GetBitmapLoaderByFileName(const AFileName: string): IBitmapTileLoader;
    function GetIsVectorType(const AType: AnsiString): Boolean;
    function GetIsVectorExt(const AExt: AnsiString): Boolean;
    function GetConverter(const ATypeSource, ATypeTarget: AnsiString): IContentConverter;
    function GetKnownExtList: IStringListStatic;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Alcinoe.StringList,
  u_StringListStatic,
  u_AnsiStr;

procedure TContentTypeManagerBase.AddByExt(
  const AInfo: IContentTypeInfoBasic;
  const AExt: AnsiString
);
begin
  Assert(IsAscii(AExt));
  FExtList.Add(AExt, AInfo);
  if Supports(AInfo, IContentTypeInfoBitmap) then begin
    FBitmapExtList.Add(AExt, AInfo);
  end else if Supports(AInfo, IContentTypeInfoVectorData) then begin
    FVectorExtList.Add(AExt, AInfo);
  end;
end;

procedure TContentTypeManagerBase.AddByType(
  const AInfo: IContentTypeInfoBasic;
  const AType: AnsiString
);
begin
  Assert(IsAscii(AType));
  FTypeList.Add(AType, AInfo);
  if Supports(AInfo, IContentTypeInfoBitmap) then begin
    FBitmapTypeList.Add(AType, AInfo);
  end else if Supports(AInfo, IContentTypeInfoVectorData) then begin
    FVectorTypeList.Add(AType, AInfo);
  end;
end;

constructor TContentTypeManagerBase.Create;
begin
  inherited Create;
  FExtList := TContentTypeListByKey.Create;
  FTypeList := TContentTypeListByKey.Create;
  FBitmapExtList := TContentTypeListByKey.Create;
  FBitmapTypeList := TContentTypeListByKey.Create;
  FVectorExtList := TContentTypeListByKey.Create;
  FVectorTypeList := TContentTypeListByKey.Create;
  FConverterMatrix := TContentConverterMatrix.Create;
end;

destructor TContentTypeManagerBase.Destroy;
begin
  FreeAndNil(FExtList);
  FreeAndNil(FTypeList);
  FreeAndNil(FBitmapExtList);
  FreeAndNil(FBitmapTypeList);
  FreeAndNil(FVectorExtList);
  FreeAndNil(FVectorTypeList);
  FreeAndNil(FConverterMatrix);
  inherited;
end;

function TContentTypeManagerBase.GetKnownExtList: IStringListStatic;
var
  VList: TStringList;
  VEnum: TStringsEnumeratorA;
begin
  VEnum := FExtList.GetEnumerator;
  try
    VList := TStringList.Create;
    try
      while VEnum.MoveNext do begin
        VList.Add(VEnum.Current);
      end;
    finally
      Result := TStringListStatic.CreateWithOwn(VList);
    end;
  finally
    VEnum.Free;
  end;
end;

function TContentTypeManagerBase.GetBitmapLoaderByFileName(
  const AFileName: string
): IBitmapTileLoader;
var
  VExt: string;
  VContentType: IContentTypeInfoBasic;
  VContentTypeBitmap: IContentTypeInfoBitmap;
begin
  Result := nil;
  VExt := LowerCase(ExtractFileExt(AFileName));
  if IsAscii(VExt) then begin
    VContentType := GetInfoByExt(AnsiString(VExt));
    if Assigned(VContentType) then begin
      if Supports(VContentType, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
        Result := VContentTypeBitmap.GetLoader;
      end;
    end;
  end;
end;

function TContentTypeManagerBase.GetConverter(
  const ATypeSource, ATypeTarget: AnsiString): IContentConverter;
begin
  Assert(IsAscii(ATypeSource));
  Assert(IsAscii(ATypeTarget));
  Result := FConverterMatrix.Get(ATypeSource, ATypeTarget);
end;

function TContentTypeManagerBase.GetInfo(
  const AType: AnsiString): IContentTypeInfoBasic;
begin
  Assert(IsAscii(AType));
  Result := FTypeList.Get(AType);
end;

function TContentTypeManagerBase.GetInfoByExt(
  const AExt: AnsiString
): IContentTypeInfoBasic;
begin
  Assert(IsAscii(AExt));
  Result := FExtList.Get(AExt);
end;

function TContentTypeManagerBase.GetIsBitmapExt(const AExt: AnsiString): Boolean;
begin
  Assert(IsAscii(AExt));
  Result := FBitmapExtList.Get(AExt) <> nil;
end;

function TContentTypeManagerBase.GetIsBitmapType(const AType: AnsiString): Boolean;
begin
  Assert(IsAscii(AType));
  Result := FBitmapTypeList.Get(AType) <> nil;
end;

function TContentTypeManagerBase.GetIsVectorExt(const AExt: AnsiString): Boolean;
begin
  Assert(IsAscii(AExt));
  Result := FVectorExtList.Get(AExt) <> nil;
end;

function TContentTypeManagerBase.GetIsVectorType(const AType: AnsiString): Boolean;
begin
  Assert(IsAscii(AType));
  Result := FVectorTypeList.Get(AType) <> nil;
end;

end.
