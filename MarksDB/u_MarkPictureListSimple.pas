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

unit u_MarkPictureListSimple;

interface

uses
  Classes,
  i_HashFunction,
  i_PathConfig,
  i_ContentTypeManager,
  i_MarkPicture,
  u_ConfigDataElementBase;

type
  TMarkPictureListSimple = class(TConfigDataElementBaseEmptySaveLoad, IMarkPictureList)
  private
    FHashFunction: IHashFunction;
    FList: TStringList;
    FBasePath: IPathConfig;
    FContentTypeManager: IContentTypeManager;
    procedure Clear;
  private
    procedure LoadList;
    function GetCount: Integer;

    function Get(AIndex: Integer): IMarkPicture;
    function GetName(AIndex: Integer): string;
    function GetIndexByName(const AValue: string): Integer;

    function GetDefaultPicture: IMarkPicture;
    function FindByNameOrDefault(const AValue: string): IMarkPicture;
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const ABasePath: IPathConfig;
      const AContentTypeManager: IContentTypeManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  t_Hash,
  i_BitmapTileSaveLoad,
  i_ContentTypeInfo,
  u_MarkPictureSimple;

procedure GetFilesList(const APath, AMask: string; var AList: TStringList);
const
  cFileMask = '*.*';
var
  VRec: TSearchRec;
  VPath: string;
begin
  VPath := IncludeTrailingPathDelimiter(APath);
  if FindFirst(VPath + cFileMask, faAnyFile - faDirectory, VRec) = 0 then
  try
    repeat
      if AnsiPos(ExtractFileExt(VRec.Name), AMask) > 0 then
        AList.Add(VPath + VRec.Name);
    until FindNext(VRec) <> 0;
  finally
    FindClose(VRec);
  end;
  if FindFirst(VPath + cFileMask, faDirectory, VRec) = 0 then
  try
    repeat
      if ((VRec.Attr and faDirectory) <> 0) and (VRec.Name <> '.') and (VRec.Name <> '..') then
        GetFilesList(VPath + VRec.Name, AMask, AList); // recursion
    until FindNext(VRec) <> 0;
  finally
    FindClose(VRec);
  end;
end;

{ TMarkPictureListSimple }

constructor TMarkPictureListSimple.Create(
  const AHashFunction: IHashFunction;
  const ABasePath: IPathConfig;
  const AContentTypeManager: IContentTypeManager
);
begin
  inherited Create;
  FHashFunction := AHashFunction;
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
  if Assigned(FList) then begin
    for i := 0 to FList.Count - 1 do begin
      IInterface(Pointer(FList.Objects[i]))._Release;
    end;
    FList.Clear;
  end;
end;

procedure TMarkPictureListSimple.LoadList;
const
  cExtList: array [0..2] of string = ('.png', '.jpg', '.jpeg');

  function _GetLoaderByExt(const AExt: string): IBitmapTileLoader;
  var
    VContentType: IContentTypeInfoBasic;
    VContentTypeBitmap: IContentTypeInfoBitmap;
  begin
    VContentType := FContentTypeManager.GetInfoByExt(AExt);
    if VContentType <> nil then begin
      if Supports(VContentType, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
        Result := VContentTypeBitmap.GetLoader;
      end;
    end;
  end;

var
  I, J: Integer;
  VFilesList: TStringList;
  VLoader: IBitmapTileLoader;
  VPngLoader, VJpegLoader: IBitmapTileLoader;
  VPicture: IMarkPicture;
  VFullName: string;
  VShortName: string;
  VExtention: string;
  VPath: string;
  VHash: THashValue;
begin
  inherited;
  Clear;

  VPngLoader := _GetLoaderByExt('.png');
  VJpegLoader := _GetLoaderByExt('.jpg');

  VPath := IncludeTrailingPathDelimiter(FBasePath.FullPath);
  VFilesList := TStringList.Create;
  try
    GetFilesList(VPath, '.png;.jpg;.jpeg', VFilesList);

    for I := 0 to VFilesList.Count - 1 do begin
      VLoader := nil;

      VFullName := VFilesList.Strings[I];
      VShortName := StringReplace(VFullName, VPath, '', [rfIgnoreCase]);
      VExtention := ExtractFileExt(VFullName);

      for J := 0 to Length(cExtList) - 1 do begin
        if SameText(cExtList[J], VExtention) then begin
          if J = 0 then begin
            VLoader := VPngLoader;
          end else begin
            VLoader := VJpegLoader;
          end;
        end;
      end;

      if not Assigned(VLoader) then begin
        Continue;
      end;

      VHash := FHashFunction.CalcHashByString(VFullName);
      VPicture := TMarkPictureSimple.Create(VHash, VFullName, VShortName, VLoader);
      VPicture._AddRef;
      FList.AddObject(VShortName, TObject(Pointer(VPicture)));
    end;
  finally
    VFilesList.Free;
  end;
end;

function TMarkPictureListSimple.FindByNameOrDefault(
  const AValue: string): IMarkPicture;
var
  VIndex: Integer;
begin
  Result := nil;
  LockRead;
  try
    if (AValue <> '') and FList.Find(AValue, VIndex) then begin
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
