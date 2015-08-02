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

unit u_MarkSystemImplFactoryChangeable;

interface

uses
  ActiveX,
  i_HashFunction,
  i_AppearanceOfMarkFactory,
  i_VectorItemSubsetBuilder,
  i_MarkPicture,
  i_GeometryLonLatFactory,
  i_InternalPerformanceCounter,
  i_HtmlToHintTextConverter,
  i_MarkFactory,
  i_GUIDSet,
  i_MarkSystemImplFactory,
  u_BaseInterfacedObject;

type
  TMarkSystemImplFactoryListStatic = class(TBaseInterfacedObject, IMarkSystemImplFactoryListStatic)
  private
    FList: IGUIDInterfaceSet;
  private
    function GetGUIDEnum: IEnumGUID;
    function Get(const AGUID: TGUID): IMarkSystemImplFactoryListElement;
  public
    constructor Create(
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
  c_MarkSystem,
  i_MarkSystemImplORMClientProvider,
  u_GUIDInterfaceSet,
  u_MarkSystemImplFactorySML,
  u_MarkSystemImplFactoryORM;

type
  TMarkSystemImplFactoryListElement = class(TBaseInterfacedObject, IMarkSystemImplFactoryListElement)
  private
    FGUID: TGUID;
    FCaption: string;
    FFactory: IMarkSystemImplFactory;
  private
    function GetGUID: TGUID;
    function GetCaption: string;
    function GetFactory: IMarkSystemImplFactory;
  public
    constructor Create(
      const AGUID: TGUID;
      const ACaption: string;
      const AFactory: IMarkSystemImplFactory
    );
  end;

{ TMarkSystemImplFactoryListElement }

constructor TMarkSystemImplFactoryListElement.Create(
  const AGUID: TGUID;
  const ACaption: string;
  const AFactory: IMarkSystemImplFactory
);
begin
  inherited Create;
  FGUID := AGUID;
  FCaption := ACaption;
  FFactory := AFactory;
end;

function TMarkSystemImplFactoryListElement.GetCaption: string;
begin
  Result := FCaption;
end;

function TMarkSystemImplFactoryListElement.GetFactory: IMarkSystemImplFactory;
begin
  Result := FFactory;
end;

function TMarkSystemImplFactoryListElement.GetGUID: TGUID;
begin
  Result := FGUID;
end;

{ TMarkSystemImplFactoryListStatic }

constructor TMarkSystemImplFactoryListStatic.Create(
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
  VItem: IMarkSystemImplFactoryListElement;
  VFactory: IMarkSystemImplFactory;
begin
  inherited Create;

  FList := TGUIDInterfaceSet.Create(False);

  // SML
  VFactory :=
    TMarkSystemImplFactorySML.Create(
      AMarkPictureList,
      AHashFunction,
      AAppearanceOfMarkFactory,
      AVectorGeometryLonLatFactory,
      AVectorItemSubsetBuilderFactory,
      AMarkFactory,
      ALoadDbCounter,
      ASaveDbCounter,
      AHintConverter
    );
  VItem :=
    TMarkSystemImplFactoryListElement.Create(
      cSMLMarksDbGUID,
      rsSMLMarksDbName,
      VFactory
    );
  FList.Add(VItem.GUID, VItem);

  // SQLite3
  VFactory :=
    TMarkSystemImplFactoryORM.Create(
      ctSQLite3,
      AMarkPictureList,
      AHashFunction,
      AAppearanceOfMarkFactory,
      AVectorGeometryLonLatFactory,
      AVectorItemSubsetBuilderFactory,
      AMarkFactory,
      ALoadDbCounter,
      ASaveDbCounter,
      AHintConverter
    );
  VItem :=
    TMarkSystemImplFactoryListElement.Create(
      cORMSQLiteMarksDbGUID,
      rsORMSQLiteMarksDbName,
      VFactory
    );
  FList.Add(VItem.GUID, VItem);

  // MongoDB
  VFactory :=
    TMarkSystemImplFactoryORM.Create(
      ctMongoDB,
      AMarkPictureList,
      AHashFunction,
      AAppearanceOfMarkFactory,
      AVectorGeometryLonLatFactory,
      AVectorItemSubsetBuilderFactory,
      AMarkFactory,
      ALoadDbCounter,
      ASaveDbCounter,
      AHintConverter
    );
  VItem :=
    TMarkSystemImplFactoryListElement.Create(
      cORMMongoDbMarksDbGUID,
      rsORMMongoDbMarksDbName,
      VFactory
    );
  FList.Add(VItem.GUID, VItem);

  // DBMS (ODBC)
  VFactory :=
    TMarkSystemImplFactoryORM.Create(
      ctODBC,
      AMarkPictureList,
      AHashFunction,
      AAppearanceOfMarkFactory,
      AVectorGeometryLonLatFactory,
      AVectorItemSubsetBuilderFactory,
      AMarkFactory,
      ALoadDbCounter,
      ASaveDbCounter,
      AHintConverter
    );
  VItem :=
    TMarkSystemImplFactoryListElement.Create(
      cORMODBCMarksDbGUID,
      rsORMODBCMarksDbName,
      VFactory
    );
  FList.Add(VItem.GUID, VItem);

  Exit;

  // DBMS (ZeosLib)
  VFactory :=
    TMarkSystemImplFactoryORM.Create(
      ctZDBC,
      AMarkPictureList,
      AHashFunction,
      AAppearanceOfMarkFactory,
      AVectorGeometryLonLatFactory,
      AVectorItemSubsetBuilderFactory,
      AMarkFactory,
      ALoadDbCounter,
      ASaveDbCounter,
      AHintConverter
    );
  VItem :=
    TMarkSystemImplFactoryListElement.Create(
      cORMZDBCMarksDbGUID,
      rsORMZDBCMarksDbName,
      VFactory
    );
  FList.Add(VItem.GUID, VItem);
end;

function TMarkSystemImplFactoryListStatic.Get(const AGUID: TGUID): IMarkSystemImplFactoryListElement;
begin
  Result := IMarkSystemImplFactoryListElement(FList.GetByGUID(AGUID));
end;

function TMarkSystemImplFactoryListStatic.GetGUIDEnum: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

end.
