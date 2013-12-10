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

unit u_MarkSystemSml;

interface

uses
  i_HashFunction,
  i_VectorItemsFactory,
  i_VectorItemSubsetBuilder,
  i_InternalPerformanceCounter,
  i_AppearanceOfMarkFactory,
  i_ReadWriteState,
  i_VectorDataItemSimple,
  i_MarkPicture,
  i_HtmlToHintTextConverter,
  i_MarkCategory,
  i_MarkFactory,
  i_MarkDbImpl,
  i_MarkCategoryDBImpl,
  i_MarkSystemImpl,
  i_MarkCategoryDBSmlInternal,
  i_MarkDbSmlInternal,
  i_MarkFactorySmlInternal,
  u_BaseInterfacedObject;

type
  TMarkSystemSml = class(TBaseInterfacedObject, IMarkSystemImpl)
  private
    FState: IReadWriteStateChangeble;
    FDbId: Integer;

    FMarkDbImpl: IMarkDbImpl;
    FMarkDbInternal: IMarkDbSmlInternal;
    FCategoryDBImpl: IMarkCategoryDBImpl;
    FCategoryDBInternal: IMarkCategoryDBSmlInternal;
    FFactoryDbInternal: IMarkFactorySmlInternal;
  private
    function GetMarkDb: IMarkDbImpl;
    function GetCategoryDB: IMarkCategoryDBImpl;
    function GetState: IReadWriteStateChangeble;

    function GetStringIdByMark(const AMark: IVectorDataItemSimple): string;
    function GetMarkByStringId(const AId: string): IVectorDataItemSimple;
    function GetMarkCategoryByStringId(const AId: string): IMarkCategory;
  public
    constructor Create(
      const ABasePath: string;
      const AMarkPictureList: IMarkPictureList;
      const AHashFunction: IHashFunction;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AMarkFactory: IMarkFactory;
      const ALoadDbCounter: IInternalPerformanceCounter;
      const ASaveDbCounter: IInternalPerformanceCounter;
      const AHintConverter: IHtmlToHintTextConverter
    );
  end;


implementation

uses
  SysUtils,
  u_ReadWriteStateInternal,
  u_MarkFactorySmlDbInternal,
  u_MarkDbSml,
  u_MarkCategoryDBSml;

{ TMarkSystemSml }

constructor TMarkSystemSml.Create(
  const ABasePath: string;
  const AMarkPictureList: IMarkPictureList;
  const AHashFunction: IHashFunction;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AMarkFactory: IMarkFactory;
  const ALoadDbCounter: IInternalPerformanceCounter;
  const ASaveDbCounter: IInternalPerformanceCounter;
  const AHintConverter: IHtmlToHintTextConverter
);
var
  VCategoryDb: TMarkCategoryDBSml;
  VMarkDb: TMarkDbSml;
  VState: TReadWriteStateInternal;
begin
  inherited Create;
  FDbId := Integer(Self);
  VState := TReadWriteStateInternal.Create;
  FState := VState;
  VCategoryDb :=
    TMarkCategoryDBSml.Create(
      FDbId,
      VState,
      IncludeTrailingPathDelimiter(ABasePath) + 'Categorymarks.sml'
    );
  FCategoryDBImpl := VCategoryDb;
  FCategoryDBInternal := VCategoryDb;
  FFactoryDbInternal :=
    TMarkFactorySmlDbInternal.Create(
      FDbId,
      AMarkPictureList,
      AVectorGeometryLonLatFactory,
      AAppearanceOfMarkFactory,
      AMarkFactory,
      AHashFunction,
      AHintConverter,
      FCategoryDBInternal
    );
  VMarkDb :=
    TMarkDbSml.Create(
      FDbId,
      VState,
      IncludeTrailingPathDelimiter(ABasePath) + 'marks.sml',
      AVectorItemSubsetBuilderFactory,
      FFactoryDbInternal,
      ALoadDbCounter,
      ASaveDbCounter
    );
  FMarkDbImpl := VMarkDb;
  FMarkDbInternal := VMarkDb;
end;

function TMarkSystemSml.GetCategoryDB: IMarkCategoryDBImpl;
begin
  Result := FCategoryDBImpl;
end;

function TMarkSystemSml.GetMarkByStringId(const AId: string): IVectorDataItemSimple;
var
  VId: Integer;
begin
  Result := nil;
  if AId <> '' then begin
    if TryStrToInt(AId, VId) then begin
      if not Supports(FMarkDbInternal.GetById(VId), IVectorDataItemSimple, Result) then begin
        Result := nil;
      end;
    end;
  end;
end;

function TMarkSystemSml.GetMarkCategoryByStringId(
  const AId: string): IMarkCategory;
var
  VId: Integer;
begin
  Result := nil;
  if AId <> '' then begin
    if TryStrToInt(AId, VId) then begin
      if not Supports(FCategoryDBInternal.GetCategoryByID(VId), IMarkCategory, Result) then begin
        Result := nil;
      end;
    end;
  end;
end;

function TMarkSystemSml.GetMarkDb: IMarkDbImpl;
begin
  Result := FMarkDbImpl;
end;

function TMarkSystemSml.GetState: IReadWriteStateChangeble;
begin
  Result := FState;
end;

function TMarkSystemSml.GetStringIdByMark(const AMark: IVectorDataItemSimple): string;
var
  VMark: IMarkSMLInternal;
begin
  Result := '';
  if Supports(AMark, IMarkSMLInternal, VMark) then begin
    Result := IntToStr(VMark.Id);
  end;
end;

end.
