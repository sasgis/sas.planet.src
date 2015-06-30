{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_MarkSystemImplFactoryORM;

interface

uses
  i_NotifierOperation,
  i_HashFunction,
  i_GeometryLonLatFactory,
  i_VectorItemSubsetBuilder,
  i_InternalPerformanceCounter,
  i_AppearanceOfMarkFactory,
  i_MarkPicture,
  i_HtmlToHintTextConverter,
  i_MarkFactory,
  i_MarkSystemImpl,
  i_MarkSystemImplFactory,
  u_BaseInterfacedObject;

type
  TMarkSystemImplFactoryORM = class(TBaseInterfacedObject, IMarkSystemImplFactory)
  private
    FMarkPictureList: IMarkPictureList;
    FHashFunction: IHashFunction;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FMarkFactory: IMarkFactory;
    FLoadDbCounter: IInternalPerformanceCounter;
    FSaveDbCounter: IInternalPerformanceCounter;
    FHintConverter: IHtmlToHintTextConverter;
  private
    function GetIsInitializationRequired: Boolean;
    function Build(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ABasePath: string;
      const AReadOnly: Boolean = False
    ): IMarkSystemImpl;
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
  u_MarkSystemImplORM;

{ TMarkSystemImplFactoryORM }

constructor TMarkSystemImplFactoryORM.Create(
  const AMarkPictureList: IMarkPictureList;
  const AHashFunction: IHashFunction;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AMarkFactory: IMarkFactory;
  const ALoadDbCounter, ASaveDbCounter: IInternalPerformanceCounter;
  const AHintConverter: IHtmlToHintTextConverter
);
begin
  Assert(Assigned(AMarkPictureList));
  Assert(Assigned(AHashFunction));
  Assert(Assigned(AAppearanceOfMarkFactory));
  Assert(Assigned(AVectorGeometryLonLatFactory));
  Assert(Assigned(AVectorItemSubsetBuilderFactory));
  Assert(Assigned(AMarkFactory));
  Assert(Assigned(AHintConverter));
  inherited Create;
  FMarkPictureList := AMarkPictureList;
  FHashFunction := AHashFunction;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FMarkFactory := AMarkFactory;
  FLoadDbCounter := ALoadDbCounter;
  FSaveDbCounter := ASaveDbCounter;
  FHintConverter := AHintConverter;
end;

function TMarkSystemImplFactoryORM.GetIsInitializationRequired: Boolean;
begin
  Result := True;
end;

function TMarkSystemImplFactoryORM.Build(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ABasePath: string;
  const AReadOnly: Boolean
): IMarkSystemImpl;
begin
  Result :=
    TMarkSystemImplORM.Create(
      AOperationID,
      ACancelNotifier,
      ABasePath,
      FMarkPictureList,
      FHashFunction,
      FAppearanceOfMarkFactory,
      FVectorGeometryLonLatFactory,
      FVectorItemSubsetBuilderFactory,
      FMarkFactory,
      FLoadDbCounter,
      FSaveDbCounter,
      FHintConverter,
      AReadOnly
    );
end;

end.
