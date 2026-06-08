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

unit u_MarkSystemORMModel;

interface

uses
  mormot.core.base,
  mormot.core.unicode,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.server,
  mormot.orm.rest,
  mormot.rest.server,
  mormot.rest.sqlite3;

type
  // List of Users
  TOrmUser = class(TOrm)
  public
    FName: RawUtf8;
  published
    property uName: RawUtf8 read FName write FName;
  end;

  // Categories
  TOrmCategory = class(TOrm)
  public
    FName: RawUtf8;
  published
    property cName: RawUtf8 read FName write FName;
  end;

  // Category visibility settings per User
  TOrmCategoryView = class(TOrm)
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

  // Geometry types for Marks
  TOrmGeoType = (gtUndef=0, gtPoint, gtLine, gtPoly);

  // Image paths for Marks
  TOrmMarkImage = class(TOrm)
  public
    FName: RawUtf8;
  published
    property miName: RawUtf8 read FName write FName;
  end;

  TOrmMarkAppearance = class(TOrm)
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

  // Marks
  TOrmMark = class(TOrm)
  public
    FCategory: TID;
    FImage: TID;
    FAppearance: TID;
    FName: RawUtf8;
    FDesc: RawUtf8;
    FGeoType: TOrmGeoType;
    FGeoCount: Cardinal;
    FGeoLonSize: Cardinal;
    FGeoLatSize: Cardinal;
    FGeoWKB: RawBlob;
  published
    property mCategory: TID read FCategory write FCategory;
    property mImage: TID read FImage write FImage;
    property mAppearance: TID read FAppearance write FAppearance;
    property mName: RawUtf8 read FName write FName;
    property mDesc: RawUtf8 read FDesc write FDesc;
    property mGeoType: TOrmGeoType read FGeoType write FGeoType;
    property mGeoCount: Cardinal read FGeoCount write FGeoCount;
    property mGeoLonSize: Cardinal read FGeoLonSize write FGeoLonSize;
    property mGeoLatSize: Cardinal read FGeoLatSize write FGeoLatSize;
    property mGeoWKB: RawBlob read FGeoWKB write FGeoWKB;
  end;

  TOrmMarkClass = class of TOrmMark;

  TOrmMarkDBMS = class(TOrmMark)
  public
    FLeft, FRight, FBottom, FTop: Integer;
  published
    property mLeft: Integer read FLeft write FLeft;
    property mRight: Integer read FRight write FRight;
    property mBottom: Integer read FBottom write FBottom;
    property mTop: Integer read FTop write FTop;
  end;

  TOrmMarkMongoDB = class(TOrmMarkDBMS);

  // Mark visibility settings per User
  TOrmMarkView = class(TOrm)
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

  // Bounding box index for fast geometry lookup
  TOrmMarkRTree = class(TOrmRTree)
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

  // Index on Mark name and description for fast full-text search
  // Case-sensitive for non-Latin characters, so everything is stored here in AnsiLowerCase
  TOrmMarkFTS = class(TOrmFts4)
  public
    FName: RawUtf8;
    FDesc: RawUtf8;
  published
    property mName: RawUtf8 read FName write FName; // Mark name in AnsiLowerCase
    property mDesc: RawUtf8 read FDesc write FDesc; // Mark description in AnsiLowerCase
  end;

  TOrmMarkMeta = class(TOrm)
  public
    FMark: TID;
    FMeta: RawBlob;
  published
    property mMark: TID read FMark write FMark stored AS_UNIQUE;
    property mMeta: RawBlob read FMeta write FMeta;
  end;

function CreateModelSQLite3: TOrmModel;
function CreateModelDBMS: TOrmModel;
function CreateModelMongoDB: TOrmModel;

procedure CreateMissingIndexesSQLite3(const AServer: TRestServerDB);
procedure CreateMissingIndexesDBMS(const AServer: TRestServerDB);
procedure CreateMissingIndexesMongoDB(const AServer: TRestServerDB);

implementation

function CreateModelSQLite3: TOrmModel;
begin
  Result :=
    TOrmModel.Create(
      [
        TOrmUser,
        TOrmCategory,
        TOrmCategoryView,
        TOrmMarkImage,
        TOrmMark,
        TOrmMarkView,
        TOrmMarkAppearance,
        TOrmMarkFTS,
        TOrmMarkRTree,
        TOrmMarkMeta
      ]
    );
end;

function CreateModelDBMS: TOrmModel;
begin
  Result :=
    TOrmModel.Create(
      [
        TOrmUser,
        TOrmCategory,
        TOrmCategoryView,
        TOrmMarkImage,
        TOrmMarkDBMS,
        TOrmMarkView,
        TOrmMarkAppearance,
        TOrmMarkFTS,
        TOrmMarkMeta
      ]
    );
end;

function CreateModelMongoDB: TOrmModel;
begin
  Result :=
    TOrmModel.Create(
      [
        TOrmUser,
        TOrmCategory,
        TOrmCategoryView,
        TOrmMarkImage,
        TOrmMarkMongoDB,
        TOrmMarkView,
        TOrmMarkAppearance,
        TOrmMarkFTS,
        TOrmMarkMeta
      ]
    );
end;

procedure _CreateMissingIndexes(
  const AServer: TRestOrmServer;
  const AExisting: TRawUtf8DynArray;
  const AMarksTableClass: TOrmMarkClass
);

  function _IsIndexExists(const AName: RawUtf8): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Length(AExisting) - 1 do begin
      if EndWith(AName, AExisting[I]) then begin
        Result := True;
        Exit;
      end;
    end;
  end;

var
  VIdxName: RawUtf8;
begin
  // --------------- User -----------------------------------------------------
  VIdxName := '_uName';
  if not _IsIndexExists(VIdxName) then begin
    AServer.CreateSQLIndex(TOrmUser, 'uName', True, VIdxName); // unique
  end;

  // --------------- Category -------------------------------------------------
  VIdxName := '_cName';
  if not _IsIndexExists(VIdxName) then begin
    AServer.CreateSQLIndex(TOrmCategory, 'cName', False, VIdxName); // not unique for backward compatibility
  end;

  // --------------- CategoryView ---------------------------------------------
  VIdxName := '_cvUser';
  if not _IsIndexExists(VIdxName) then begin
    AServer.CreateSQLIndex(TOrmCategoryView, 'cvUser', False, VIdxName);
  end;

  VIdxName := '_cvCategoryUser';
  if not _IsIndexExists(VIdxName) then begin
    AServer.CreateSQLMultiIndex(TOrmCategoryView, ['cvCategory', 'cvUser'], True, VIdxName); // unique
  end;

  // --------------- MarkImage ------------------------------------------------
  VIdxName := '_miName';
  if not _IsIndexExists(VIdxName) then begin
    AServer.CreateSQLIndex(TOrmMarkImage, 'miName', True, VIdxName); // unique
  end;

  // --------------- MarkAppearance -------------------------------------------
  VIdxName := '_maC1C2S1S2';
  if not _IsIndexExists(VIdxName) then begin
    AServer.CreateSQLMultiIndex(TOrmMarkAppearance, ['maColor1', 'maColor2', 'maScale1', 'maScale2'],
      True, VIdxName); // unique
  end;

  // --------------- Mark -----------------------------------------------------
  VIdxName := '_mCategory';
  if not _IsIndexExists(VIdxName) then begin
    AServer.CreateSQLIndex(AMarksTableClass, 'mCategory', False, VIdxName);
  end;

  // --------------- MarkView -------------------------------------------------
  VIdxName := '_mvUser';
  if not _IsIndexExists(VIdxName) then begin
    AServer.CreateSQLIndex(TOrmMarkView, 'mvUser', False, VIdxName);
  end;

  VIdxName := '_mvCategoryUser';
  if not _IsIndexExists(VIdxName) then begin
    AServer.CreateSQLMultiIndex(TOrmMarkView, ['mvCategory', 'mvUser'], False, VIdxName);
  end;

  VIdxName := '_mvMarkUser';
  if not _IsIndexExists(VIdxName) then begin
    AServer.CreateSQLMultiIndex(TOrmMarkView, ['mvMark', 'mvUser'], True, VIdxName); // unique
  end;
end;

procedure CreateMissingIndexesSQLite3(const AServer: TRestServerDB);
var
  I: Integer;
  VCount: Integer;
  VExisting: TRawUtf8DynArray;
begin
  VCount := AServer.DB.Execute(
    'SELECT name FROM sqlite_master WHERE type=''index'' AND name NOT LIKE ''sqlite_%'';',
    VExisting
  );
  SetLength(VExisting, VCount);

  for I := 0 to High(VExisting) do begin
    VExisting[I] := UpperCase(VExisting[I]); // for case-insensitive search
  end;

  _CreateMissingIndexes(AServer.OrmInstance as TRestOrmServer, VExisting, TOrmMark);
end;

procedure CreateMissingIndexesDBMS(const AServer: TRestServerDB);
begin
  _CreateMissingIndexes(AServer.OrmInstance as TRestOrmServer, nil, TOrmMarkDBMS);
end;

procedure CreateMissingIndexesMongoDB(const AServer: TRestServerDB);
begin
  _CreateMissingIndexes(AServer.OrmInstance as TRestOrmServer, nil, TOrmMarkMongoDB);
end;

end.
