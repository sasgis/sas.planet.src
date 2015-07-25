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

unit u_MarkSystemORMModel;

interface

uses
  mORMot,
  SynCommons;

type
  // Список пользователей
  TSQLUser = class(TSQLRecord)
  private
    FName: RawUTF8;
  published
    property Name: RawUTF8 read FName write FName stored AS_UNIQUE;
  end;

  // Категории
  TSQLCategory = class(TSQLRecord)
  private
    FName: RawUTF8;
  published
    property Name: RawUTF8 read FName write FName;
  end;

  // Настройка видимости категорий по пользователям
  TSQLCategoryView = class(TSQLRecord)
  private
    FUser: TSQLUser;
    FCategory: TSQLCategory;
    FVisible: Boolean;
    FMinZoom: Byte;
    FMaxZoom: Byte;
  published
    property User: TSQLUser read FUser write FUser;
    property Category: TSQLCategory read FCategory write FCategory;
    property Visible: Boolean read FVisible write FVisible;
    property MinZoom: Byte read FMinZoom write FMinZoom;
    property MaxZoom: Byte read FMaxZoom write FMaxZoom;
  end;

  // Типы геометрий для меток
  TSQLGeoType = (gtUndef=0, gtPoint, gtLine, gtPoly);

  // Пути к картинкам для меток
  TSQLMarkImage = class(TSQLRecord)
  private
    FName: RawUTF8;
  published
    property Name: RawUTF8 read FName write FName stored AS_UNIQUE;
  end;

  TSQLMarkAppearance = class(TSQLRecord)
  private
    FColor1: Cardinal;
    FColor2: Cardinal;
    FScale1: Integer;
    FScale2: Integer;
  published
    property Color1: Cardinal read FColor1 write FColor1;
    property Color2: Cardinal read FColor2 write FColor2;
    property Scale1: Integer read FScale1 write FScale1;
    property Scale2: Integer read FScale2 write FScale2;
  end;

  // Метки
  TSQLMark = class(TSQLRecord)
  private
    FCategory: TSQLCategory;
    FImage: TSQLMarkImage;
    FAppearance: TSQLMarkAppearance;
    FName: RawUTF8;
    FDesc: RawUTF8;
    FGeoType: TSQLGeoType;
    FGeoCount: Cardinal;
    FGeoLonSize: Cardinal;
    FGeoLatSize: Cardinal;
    FGeoJsonIdx: Variant; // simple GeoJSON geometry for MongoDB spatial index
    FGeoWKB: TSQLRawBlob;
  published
    property Category: TSQLCategory read FCategory write FCategory;
    property Image: TSQLMarkImage read FImage write FImage;
    property Appearance: TSQLMarkAppearance read FAppearance write FAppearance;
    property Name: RawUTF8 read FName write FName;
    property Desc: RawUTF8 read FDesc write FDesc;
    property GeoType: TSQLGeoType read FGeoType write FGeoType;
    property GeoCount: Cardinal read FGeoCount write FGeoCount;
    property GeoLonSize: Cardinal read FGeoLonSize write FGeoLonSize;
    property GeoLatSize: Cardinal read FGeoLatSize write FGeoLatSize;
    property GeoJsonIdx: Variant read FGeoJsonIdx write FGeoJsonIdx;
    property GeoWKB: TSQLRawBlob read FGeoWKB write FGeoWKB;
  end;

  // Настройка видимости меток по пользователям
  TSQLMarkView = class(TSQLRecord)
  private
    FUser: TSQLUser;
    FMark: TSQLMark;
    FVisible: Boolean;
  published
    property User: TSQLUser read FUser write FUser;
    property Mark: TSQLMark read FMark write FMark;
    property Visible: Boolean read FVisible write FVisible;
  end;

  // Индекс по ограничивающему прямоугольнику, для быстрого поиска геометрий
  TSQLMarkRTree = class(TSQLRecordRTree)
  private
    FLeft, FRight, FBottom, FTop: Double;
  published
    // X or Longitude coordinates in range [-180..180]
    property Left: Double read FLeft write FLeft;         // min_dimension1
    property Right: Double read FRight write FRight;      // max_dimension1
    // Y or Latitude coordinates in range [-90..90]
    property Bottom: Double read FBottom write FBottom;   // min_dimension2
    property Top: Double read FTop write FTop;            // max_dimension2
  end;

  // Индекс по имени и описания меток, для быстрого текстового поиска
  // - для нелатинских символов чувствителен к регистру, поэтому пишем сюда
  //   всё в LowerCase
  TSQLMarkFTS = class(TSQLRecordFTS4)
  private
    FName: RawUTF8;
    FDesc: RawUTF8;
  published
    property Name: RawUTF8 read FName write FName; // имя метки в AnsiLowerCase
    property Desc: RawUTF8 read FDesc write FDesc; // описание мекти в AnsiLowerCase
  end;

function CreateModel: TSQLModel;

implementation

function CreateModel: TSQLModel;
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
        TSQLMarkRTree
      ]
    );
end;

end.
