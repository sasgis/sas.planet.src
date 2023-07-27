{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_MarkSystemORMModel;

interface

uses
  SysUtils,
  StrUtils,
  mORMot,
  mORMotSQLite3,
  SynCommons;

type
  // Список пользователей
  TSQLUser = class(TSQLRecord)
  public
    FName: RawUTF8;
  published
    property uName: RawUTF8 read FName write FName;
  end;

  // Категории
  TSQLCategory = class(TSQLRecord)
  public
    FName: RawUTF8;
  published
    property cName: RawUTF8 read FName write FName;
  end;

  // Настройка видимости категорий по пользователям
  TSQLCategoryView = class(TSQLRecord)
  public
    FUser: TID;
    FCategory: TID;
    FVisible: Boolean;
    FMinZoom: Byte;
    FMaxZoom: Byte;
  published
    property cvUser: TID read FUser write FUser;
    property cvCategory: TID read FCategory write FCategory;
    property cvVisible: Boolean read FVisible write FVisible;
    property cvMinZoom: Byte read FMinZoom write FMinZoom;
    property cvMaxZoom: Byte read FMaxZoom write FMaxZoom;
  end;

  // Типы геометрий для меток
  TSQLGeoType = (gtUndef=0, gtPoint, gtLine, gtPoly);

  // Пути к картинкам для меток
  TSQLMarkImage = class(TSQLRecord)
  public
    FName: RawUTF8;
  published
    property miName: RawUTF8 read FName write FName;
  end;

  TSQLMarkAppearance = class(TSQLRecord)
  public
    FColor1: Cardinal;
    FColor2: Cardinal;
    FScale1: Integer;
    FScale2: Integer;
  published
    property maColor1: Cardinal read FColor1 write FColor1;
    property maColor2: Cardinal read FColor2 write FColor2;
    property maScale1: Integer read FScale1 write FScale1;
    property maScale2: Integer read FScale2 write FScale2;
  end;

  // Метки
  TSQLMark = class(TSQLRecord)
  public
    FCategory: TID;
    FImage: TID;
    FAppearance: TID;
    FName: RawUTF8;
    FDesc: RawUTF8;
    FGeoType: TSQLGeoType;
    FGeoCount: Cardinal;
    FGeoLonSize: Cardinal;
    FGeoLatSize: Cardinal;
    FGeoWKB: TSQLRawBlob;
  published
    property mCategory: TID read FCategory write FCategory;
    property mImage: TID read FImage write FImage;
    property mAppearance: TID read FAppearance write FAppearance;
    property mName: RawUTF8 read FName write FName;
    property mDesc: RawUTF8 read FDesc write FDesc;
    property mGeoType: TSQLGeoType read FGeoType write FGeoType;
    property mGeoCount: Cardinal read FGeoCount write FGeoCount;
    property mGeoLonSize: Cardinal read FGeoLonSize write FGeoLonSize;
    property mGeoLatSize: Cardinal read FGeoLatSize write FGeoLatSize;
    property mGeoWKB: TSQLRawBlob read FGeoWKB write FGeoWKB;
  end;

  TSQLMarkClass = class of TSQLMark;

  TSQLMarkDBMS = class(TSQLMark)
  public
    FLeft, FRight, FBottom, FTop: Integer;
  published
    property mLeft: Integer read FLeft write FLeft;
    property mRight: Integer read FRight write FRight;
    property mBottom: Integer read FBottom write FBottom;
    property mTop: Integer read FTop write FTop;
  end;

  TSQLMarkMongoDB = class(TSQLMarkDBMS);

  // Настройка видимости меток по пользователям
  TSQLMarkView = class(TSQLRecord)
  public
    FUser: TID;
    FMark: TID;
    FCategory: TID;
    FVisible: Boolean;
  published
    property mvUser: TID read FUser write FUser;
    property mvMark: TID read FMark write FMark;
    property mvCategory: TID read FCategory write FCategory;
    property mvVisible: Boolean read FVisible write FVisible;
  end;

  // Индекс по ограничивающему прямоугольнику, для быстрого поиска геометрий
  TSQLMarkRTree = class(TSQLRecordRTree)
  public
    FLeft, FRight, FBottom, FTop: Double;
  published
    // X or Longitude coordinates in range [-180..180]
    property mLeft: Double read FLeft write FLeft;         // min_dimension1
    property mRight: Double read FRight write FRight;      // max_dimension1
    // Y or Latitude coordinates in range [-90..90]
    property mBottom: Double read FBottom write FBottom;   // min_dimension2
    property mTop: Double read FTop write FTop;            // max_dimension2
  end;

  // Индекс по имени и описания меток, для быстрого текстового поиска
  // - для нелатинских символов чувствителен к регистру, поэтому пишем сюда
  //   всё в AnsiLowerCase
  TSQLMarkFTS = class(TSQLRecordFTS4)
  public
    FName: RawUTF8;
    FDesc: RawUTF8;
  published
    property mName: RawUTF8 read FName write FName; // имя метки в AnsiLowerCase
    property mDesc: RawUTF8 read FDesc write FDesc; // описание мекти в AnsiLowerCase
  end;

  TSQLMarkMeta = class(TSQLRecord)
  public
    FMark: TID;
    FMeta: TSQLRawBlob;
  published
    property mMark: TID read FMark write FMark stored AS_UNIQUE;
    property mMeta: TSQLRawBlob read FMeta write FMeta;
  end;

function CreateModelSQLite3: TSQLModel;
function CreateModelDBMS: TSQLModel;
function CreateModelMongoDB: TSQLModel;

procedure CreateMissingIndexesSQLite3(const AServer: TSQLRestServerDB);
procedure CreateMissingIndexesDBMS(const AServer: TSQLRestServerDB);
procedure CreateMissingIndexesMongoDB(const AServer: TSQLRestServerDB);

implementation

function CreateModelSQLite3: TSQLModel;
begin
  Result :=
    TSQLModel.Create(
      [
        TSQLUser,
        TSQLCategory,
        TSQLCategoryView,
        TSQLMarkImage,
        TSQLMark,
        TSQLMarkView,
        TSQLMarkAppearance,
        TSQLMarkFTS,
        TSQLMarkRTree,
        TSQLMarkMeta
      ]
    );
end;

function CreateModelDBMS: TSQLModel;
begin
  Result :=
    TSQLModel.Create(
      [
        TSQLUser,
        TSQLCategory,
        TSQLCategoryView,
        TSQLMarkImage,
        TSQLMarkDBMS,
        TSQLMarkView,
        TSQLMarkAppearance,
        TSQLMarkFTS,
        TSQLMarkMeta
      ]
    );
end;

function CreateModelMongoDB: TSQLModel;
begin
  Result :=
    TSQLModel.Create(
      [
        TSQLUser,
        TSQLCategory,
        TSQLCategoryView,
        TSQLMarkImage,
        TSQLMarkMongoDB,
        TSQLMarkView,
        TSQLMarkAppearance,
        TSQLMarkFTS,
        TSQLMarkMeta
      ]
    );
end;

procedure CreateMissingIndexesSQLite3(const AServer: TSQLRestServerDB);

  function _IsIndexExists(const AName: RawUTF8; const AExisting: TRawUTF8DynArray): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Length(AExisting) - 1 do begin
      if EndsStr(AName, AExisting[I]) then begin
        Result := True;
        Exit;
      end;
    end;
  end;

var
  VIdxName: RawUTF8;
  VCount: Integer;
  VExisting: TRawUTF8DynArray;
begin
  VCount := AServer.DB.Execute(
    'SELECT name FROM sqlite_master WHERE type=''index'' AND name NOT LIKE ''sqlite_%'';',
    VExisting
  );
  SetLength(VExisting, VCount);

  // --------------- User -----------------------------------------------------
  VIdxName := '_uName';
  if not _IsIndexExists(VIdxName, VExisting) then begin
    AServer.CreateSQLIndex(TSQLUser, 'uName', True, VIdxName); // unique
  end;

  // --------------- Category -------------------------------------------------
  VIdxName := '_cName';
  if not _IsIndexExists(VIdxName, VExisting) then begin
    AServer.CreateSQLIndex(TSQLCategory, 'cName', False, VIdxName); // not unique for backward compatibility
  end;

  // --------------- CategoryView ---------------------------------------------
  VIdxName := '_cvUser';
  if not _IsIndexExists(VIdxName, VExisting) then begin
    AServer.CreateSQLIndex(TSQLCategoryView, 'cvUser', False, VIdxName);
  end;

  VIdxName := '_cvCategoryUser';
  if not _IsIndexExists(VIdxName, VExisting) then begin
    AServer.CreateSQLMultiIndex(TSQLCategoryView, ['cvCategory', 'cvUser'], True, VIdxName); // unique
  end;

  // --------------- MarkImage ------------------------------------------------
  VIdxName := '_miName';
  if not _IsIndexExists(VIdxName, VExisting) then begin
    AServer.CreateSQLIndex(TSQLMarkImage, 'miName', True, VIdxName); // unique
  end;

  // --------------- MarkAppearance -------------------------------------------
  VIdxName := '_maC1C2S1S2';
  if not _IsIndexExists(VIdxName, VExisting) then begin
    AServer.CreateSQLMultiIndex(TSQLMarkAppearance, ['maColor1', 'maColor2', 'maScale1', 'maScale2'],
      True, VIdxName); // unique
  end;

  // --------------- Mark -----------------------------------------------------
  VIdxName := '_mCategory';
  if not _IsIndexExists(VIdxName, VExisting) then begin
    AServer.CreateSQLIndex(TSQLMark, 'mCategory', False, VIdxName);
  end;

  // --------------- MarkView -------------------------------------------------
  VIdxName := '_mvUser';
  if not _IsIndexExists(VIdxName, VExisting) then begin
    AServer.CreateSQLIndex(TSQLMarkView, 'mvUser', False, VIdxName);
  end;

  VIdxName := '_mvCategoryUser';
  if not _IsIndexExists(VIdxName, VExisting) then begin
    AServer.CreateSQLMultiIndex(TSQLMarkView, ['mvCategory', 'mvUser'], False, VIdxName);
  end;

  VIdxName := '_mvMarkUser';
  if not _IsIndexExists(VIdxName, VExisting) then begin
    AServer.CreateSQLMultiIndex(TSQLMarkView, ['mvMark', 'mvUser'], True, VIdxName); // unique
  end;
end;

procedure CreateMissingIndexesDBMS(const AServer: TSQLRestServerDB);
begin
  // ToDo
end;

procedure CreateMissingIndexesMongoDB(const AServer: TSQLRestServerDB);
begin
  // ToDo
end;

end.
