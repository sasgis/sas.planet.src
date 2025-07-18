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

unit u_ZmpInfoSet;

interface

uses
  ActiveX,
  i_GUIDSet,
  i_ZmpInfo,
  i_ProjectionSetFactory,
  i_ArchiveReadWriteFactory,
  i_LanguageManager,
  i_Bitmap32BufferFactory,
  i_ContentTypeManager,
  i_AppearanceOfMarkFactory,
  i_MarkPicture,
  i_FileNameIterator,
  i_ZmpConfig,
  i_ZmpInfoSet,
  u_BaseInterfacedObject;

type
  TZmpInfoSet = class(TBaseInterfacedObject, IZmpInfoSet)
  private
    FList: IGUIDInterfaceSet;
  private
    function GetZmpByGUID(const AGUID: TGUID): IZmpInfo;
    function GetIterator: IEnumGUID;
  public
    constructor Create(
      const AZmpConfig: IZmpConfig;
      const AProjectionSetFactory: IProjectionSetFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AContentTypeManager: IContentTypeManager;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkPictureList: IMarkPictureList;
      const ABufferFactory: IBitmap32BufferFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ALanguageManager: ILanguageManager;
      const AFilesIterator: IFileNameIterator
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  ExplorerSort,
  i_ConfigDataProvider,
  u_Dialogs,
  u_GUIDInterfaceSetOrdered,
  u_ConfigDataProviderByFolder,
  u_ConfigDataProviderByZip,
  u_ZmpInfo,
  u_ZmpInfoProxy,
  u_ResStrings;

const
  CMaxReservedZmpCount = 32; // todo: get this value from config

function IsHiddenZmp(const AName: string): Boolean;
var
  VFileName: string;
begin
  Result := False;
  VFileName := ExtractFileName(AName);
  if VFileName <> '' then begin
    Result := VFileName[1] = '.';
  end;
end;

{ TZmpInfoSet }

constructor TZmpInfoSet.Create(
  const AZmpConfig: IZmpConfig;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AContentTypeManager: IContentTypeManager;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkPictureList: IMarkPictureList;
  const ABufferFactory: IBitmap32BufferFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ALanguageManager: ILanguageManager;
  const AFilesIterator: IFileNameIterator
);
var
  I: Integer;
  VIsLayer: Boolean;
  VIsBitmapTiles: Boolean;
  VFileName: string;
  VRootFolder: string;
  VFullFileName: string;
  VZmp: IZmpInfo;
  VZmpExist: IZmpInfo;
  VZmpMapConfig: IConfigDataProvider;
  VMapTypeCount: integer;
  VStream: TStream;
  VFileNameList: TStringList;
begin
  inherited Create;

  VMapTypeCount := 0;
  FList := TGUIDInterfaceSetOrdered.Create;

  VFileNameList := TStringList.Create;
  try
    VRootFolder := AFilesIterator.GetRootFolderName;

    while AFilesIterator.Next(VFileName) do begin
      if not IsHiddenZmp(VFileName) then begin
        VFileNameList.Add(VFileName);
      end;
    end;
    VFileNameList.CustomSort(StringListCompare);

    for I := 0 to VFileNameList.Count - 1 do begin
      VFileName := VFileNameList.Strings[I];
      VFullFileName := VRootFolder + VFileName;
      try
        if FileExists(VFullFileName) then begin
          VStream := TMemoryStream.Create;
          try
            TMemoryStream(VStream).LoadFromFile(VFullFileName);
            VZmpMapConfig := TConfigDataProviderByArchive.Create(
              VFullFileName,
              AArchiveReadWriteFactory.Zip.ReaderFactory.BuildByStreamWithOwn(VStream)
            );
            VStream := nil;
          finally
            FreeAndNil(VStream);
          end;
        end else begin
          VZmpMapConfig := TConfigDataProviderByFolder.Create(VFullFileName);
        end;
        try
          VZmp := TZmpInfo.Create(
            AZmpConfig,
            ALanguageManager,
            AProjectionSetFactory,
            AContentTypeManager,
            AAppearanceOfMarkFactory,
            AMarkPictureList,
            ABitmapFactory,
            VFileName,
            VZmpMapConfig,
            VMapTypeCount
          );
        except
          on E: EZmpError do begin
            raise Exception.CreateResFmt(@SAS_ERR_MapGUIDError, [VFileName, E.Message]);
          end;
        end;
        VZmpExist := IZmpInfo(FList.GetByGUID(VZmp.GUID));
        if VZmpExist <> nil then begin
          raise Exception.CreateFmt(SAS_ERR_MapGUIDDuplicate, [VZmpExist.FileName, VFullFileName]);
        end;
      except
        if ExceptObject <> nil then begin
          ShowErrorMessage((ExceptObject as Exception).Message);
        end;
        VZmp := nil;
      end;
      if VZmp <> nil then begin
        FList.Add(VZmp.GUID, VZmp);
        Inc(VMapTypeCount);
      end;
    end;

    for I := 0 to CMaxReservedZmpCount - 1 do begin
      VIsLayer := I > (CMaxReservedZmpCount div 2);
      VIsBitmapTiles := not VIsLayer or (VIsLayer and (I mod 2 = 0));
      VZmp := TZmpInfoProxy.Create(
        AZmpConfig,
        ALanguageManager,
        AProjectionSetFactory,
        AContentTypeManager,
        AAppearanceOfMarkFactory,
        AMarkPictureList,
        ABitmapFactory,
        VMapTypeCount,
        VIsBitmapTiles,
        VIsLayer
      ) as IZmpInfo;
      FList.Add(VZmp.GUID, VZmp);
      Inc(VMapTypeCount);
    end;
  finally
    VFileNameList.Free;
  end;
end;

function TZmpInfoSet.GetIterator: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

function TZmpInfoSet.GetZmpByGUID(const AGUID: TGUID): IZmpInfo;
begin
  Result := IZmpInfo(FList.GetByGUID(AGUID));
end;

end.
