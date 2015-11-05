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
      const ABufferFactory: IBitmap32BufferFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ALanguageManager: ILanguageManager;
      const AFilesIterator: IFileNameIterator
    );
  end;

implementation

uses
  SysUtils,
  Dialogs,
  i_ConfigDataProvider,
  u_GUIDInterfaceSet,
  u_ConfigDataProviderByFolder,
  u_ConfigDataProviderByZip,
  u_ZmpInfo,
  u_ResStrings;

{ TZmpInfoSet }

constructor TZmpInfoSet.Create(
  const AZmpConfig: IZmpConfig;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AContentTypeManager: IContentTypeManager;
  const ABufferFactory: IBitmap32BufferFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ALanguageManager: ILanguageManager;
  const AFilesIterator: IFileNameIterator
);
var
  VFileName: string;
  VFullFileName: string;
  VZmp: IZmpInfo;
  VZmpExist: IZmpInfo;
  VZmpMapConfig: IConfigDataProvider;
  VMapTypeCount: integer;
begin
  inherited Create;
  FList := TGUIDInterfaceSet.Create;
  VMapTypeCount := 0;
  while AFilesIterator.Next(VFileName) do begin
    VFullFileName := AFilesIterator.GetRootFolderName + VFileName;
    try
      if FileExists(VFullFileName) then begin
        VZmpMapConfig := TConfigDataProviderByArchive.Create(
          VFullFileName,
          AArchiveReadWriteFactory.Zip.ReaderFactory.BuildByFileName(VFullFileName)
        );
      end else begin
        VZmpMapConfig := TConfigDataProviderByFolder.Create(VFullFileName);
      end;
      try
        VZmp := TZmpInfo.Create(
          AZmpConfig,
          ALanguageManager,
          AProjectionSetFactory,
          AContentTypeManager,
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
        ShowMessage((ExceptObject as Exception).Message);
      end;
      VZmp := nil;
    end;
    if VZmp <> nil then begin
      FList.Add(VZmp.GUID, VZmp);
      inc(VMapTypeCount);
    end;
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
