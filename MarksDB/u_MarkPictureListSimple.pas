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

unit u_MarkPictureListSimple;

interface

uses
  Classes,
  i_PathConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ContentTypeManager,
  i_MarkPicture,
  u_ConfigDataElementBase;

type
  TMarkPictureListSimple = class(TConfigDataElementBase, IMarkPictureList)
  private
    FList: TStringList;
    FBasePath: IPathConfig;
    FContentTypeManager: IContentTypeManager;
    procedure Clear;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetCount: Integer;

    function Get(AIndex: Integer): IMarkPicture;
    function GetName(AIndex: Integer): string;
    function GetIndexByName(const AValue: string): Integer;

    function GetDefaultPicture: IMarkPicture;
    function FindByNameOrDefault(const AValue: string): IMarkPicture;
  public
    constructor Create(
      const ABasePath: IPathConfig;
      const AContentTypeManager: IContentTypeManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_BitmapTileSaveLoad,
  i_ContentTypeInfo,
  u_MarkPictureSimple;

{ TMarkPictureListSimple }

constructor TMarkPictureListSimple.Create(
  const ABasePath: IPathConfig;
  const AContentTypeManager: IContentTypeManager
);
begin
  inherited Create;
  FBasePath := ABasePath;
  FContentTypeManager := AContentTypeManager;
  FList := TStringList.Create;
end;

destructor TMarkPictureListSimple.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TMarkPictureListSimple.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    IInterface(Pointer(FList.Objects[i]))._Release;
  end;
  FList.Clear;
end;

procedure TMarkPictureListSimple.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  SearchRec: TSearchRec;
  VLoader: IBitmapTileLoader;
  VPicture: IMarkPicture;
  VFullName: string;
  VContentType: IContentTypeInfoBasic;
  VContentTypeBitmap: IContentTypeInfoBitmap;
  VPath: string;
begin
  inherited;
  Clear;
  VContentType := FContentTypeManager.GetInfoByExt('.png');
  if VContentType <> nil then begin
    if Supports(VContentType, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
      VLoader := VContentTypeBitmap.GetLoader;
    end;
  end;
  VPath := IncludeTrailingPathDelimiter(FBasePath.FullPath);
  if FindFirst(VPath + '*.png', faAnyFile, SearchRec) = 0 then begin
    try
      repeat
        if (SearchRec.Attr and faDirectory) <> faDirectory then begin
          VFullName := VPath + SearchRec.Name;
          VPicture := TMarkPictureSimple.Create(VFullName, SearchRec.Name, VLoader);
          VPicture._AddRef;
          FList.AddObject(SearchRec.Name, TObject(Pointer(VPicture)));
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

procedure TMarkPictureListSimple.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
end;

function TMarkPictureListSimple.FindByNameOrDefault(
  const AValue: string): IMarkPicture;
var
  VIndex: Integer;
begin
  Result := nil;
  LockRead;
  try
    if FList.Find(AValue, VIndex) then begin
      Result := Get(VIndex);
    end else begin
      if GetCount > 0 then begin
        Result := Get(0);
      end;
    end;
  finally
    UnlockRead;
  end;
end;

function TMarkPictureListSimple.Get(AIndex: Integer): IMarkPicture;
begin
  LockRead;
  try
    Result := IMarkPicture(Pointer(FList.Objects[AIndex]));
  finally
    UnlockRead;
  end;
end;

function TMarkPictureListSimple.GetCount: Integer;
begin
  LockRead;
  try
    Result := FList.Count;
  finally
    UnlockRead;
  end;
end;

function TMarkPictureListSimple.GetDefaultPicture: IMarkPicture;
begin
  Result := nil;
  LockRead;
  try
    if GetCount > 0 then begin
      Result := Get(0);
    end;
  finally
    UnlockRead;
  end;
end;

function TMarkPictureListSimple.GetIndexByName(const AValue: string): Integer;
begin
  LockRead;
  try
    Result := FList.IndexOf(AValue);
  finally
    UnlockRead;
  end;
end;

function TMarkPictureListSimple.GetName(AIndex: Integer): string;
begin
  LockRead;
  try
    Result := FList.Strings[AIndex];
  finally
    UnlockRead;
  end;
end;

end.
