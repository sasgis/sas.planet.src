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

unit u_MarkSystemSml;

interface

uses
  Windows,
  Classes,
  i_HashFunction,
  i_GeometryLonLatFactory,
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
  i_ReadWriteStateInternal,
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
    function PrepareStream(
      const AFileName: string;
      const AState: IReadWriteStateInternal
    ): TStream;
    procedure MakeBackUp(const AFileName: string);
  private
    function GetMarkDb: IMarkDbImpl;
    function GetCategoryDB: IMarkCategoryDBImpl;
    function GetState: IReadWriteStateChangeble;

    function GetStringIdByMark(const AMark: IVectorDataItem): string;
    function GetMarkByStringId(const AId: string): IVectorDataItem;
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
      const AHintConverter: IHtmlToHintTextConverter;
      const AReadOnly: Boolean = False
    );
  end;


implementation

uses
  SysUtils,
  t_CommonTypes,
  i_GeometryToStream,
  i_GeometryFromStream,
  u_ReadWriteStateInternal,
  u_MarkFactorySmlDbInternal,
  u_GeometryToStreamSML,
  u_GeometryFromStreamSML,
  u_MarkDbSml,
  u_MarkCategoryDBSml;

function GetDiskFree(const ADrive: Char): Int64;
var
  lpFreeBytesAvailableToCaller,
  lpTotalNumberOfBytes,
  lpTotalNumberOfFreeBytes : TLargeInteger;
begin
  if
    GetDiskFreeSpaceEx(
      PChar(ADrive + ':\'),
      lpFreeBytesAvailableToCaller,
      lpTotalNumberOfBytes,
      @lpTotalNumberOfFreeBytes
    )
  then
    Result := lpTotalNumberOfFreeBytes
  else
    Result := -1;
end;

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
  const AHintConverter: IHtmlToHintTextConverter;
  const AReadOnly: Boolean
);
var
  VCategoryDb: TMarkCategoryDBSml;
  VMarkDb: TMarkDbSml;
  VState: TReadWriteStateInternal;
  VStateInternal: IReadWriteStateInternal;
  VCategoryFileName: string;
  VCategoryStream: TStream;
  VMarkFileName: string;
  VMarkStream: TStream;
  VGeometryReader: IGeometryFromStream;
  VGeometryWriter: IGeometryToStream;
  VFreeDiskSpace: Int64;
  VBackUpRequaredDiskSize: Int64;
  VUseUnicodeSchema: Boolean;
  VStoreInBinaryFormat: Boolean;
begin
  inherited Create;
  FDbId := Integer(Self);
  VState := TReadWriteStateInternal.Create;
  FState := VState;
  VStateInternal := VState;

  VUseUnicodeSchema := False; // ToDo
  VStoreInBinaryFormat := False; // ToDo

  if AReadOnly then begin
    VStateInternal.WriteAccess := asDisabled;
  end;

  VCategoryFileName := IncludeTrailingPathDelimiter(ABasePath) + 'Categorymarks.sml';
  VCategoryStream := PrepareStream(VCategoryFileName, VState);

  VMarkFileName := IncludeTrailingPathDelimiter(ABasePath) + 'marks.sml';
  VMarkStream := PrepareStream(VMarkFileName, VState);

  if VStateInternal.WriteAccess <> asDisabled then begin
    // ensure that we have some free space on current drive
    VBackUpRequaredDiskSize := (VCategoryStream.Size + VMarkStream.Size) * 2;
    VFreeDiskSpace := GetDiskFree(ExtractFileDrive(ABasePath)[1]);
    if (VFreeDiskSpace >= 0) and (VBackUpRequaredDiskSize > VFreeDiskSpace) then begin
      VStateInternal.WriteAccess := asDisabled;
    end;
  end;

  VCategoryDb :=
    TMarkCategoryDBSml.Create(
      FDbId,
      VState,
      VCategoryStream,
      VUseUnicodeSchema,
      VStoreInBinaryFormat
    );

  FCategoryDBImpl := VCategoryDb;
  FCategoryDBInternal := VCategoryDb;
  FFactoryDbInternal :=
    TMarkFactorySmlDbInternal.Create(
      FDbId,
      AMarkPictureList,
      AAppearanceOfMarkFactory,
      AMarkFactory,
      AHashFunction,
      AHintConverter,
      FCategoryDBInternal
    );

  VGeometryReader := TGeometryFromStreamSML.Create(AVectorGeometryLonLatFactory);
  VGeometryWriter := TGeometryToStreamSML.Create;
  VMarkDb :=
    TMarkDbSml.Create(
      FDbId,
      VState,
      VMarkStream,
      AVectorItemSubsetBuilderFactory,
      VGeometryReader,
      VGeometryWriter,
      FFactoryDbInternal,
      ALoadDbCounter,
      ASaveDbCounter,
      VUseUnicodeSchema,
      VStoreInBinaryFormat
    );

  if FState.GetStatic.WriteAccess = asEnabled then begin
    if VCategoryStream.Size > 0 then begin
      MakeBackUp(VCategoryFileName);
    end;
    if VMarkStream.Size > 0 then begin
      MakeBackUp(VMarkFileName);
    end;
  end;

  FMarkDbImpl := VMarkDb;
  FMarkDbInternal := VMarkDb;
end;

function TMarkSystemSml.PrepareStream(
  const AFileName: string;
  const AState: IReadWriteStateInternal
): TStream;
begin
  Result := nil;
  AState.LockWrite;
  try
    if AState.ReadAccess <> asDisabled then begin
      if FileExists(AFileName) then begin
        if AState.WriteAccess <> asDisabled then begin
          try
            Result := TFileStream.Create(AFileName, fmOpenReadWrite + fmShareDenyWrite);
            AState.WriteAccess := asEnabled;
          except
            Result := nil;
            AState.WriteAccess := asDisabled;
          end;
        end;
        if Result = nil then begin
          try
            Result := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
            AState.ReadAccess := asEnabled;
          except
            AState.ReadAccess := asDisabled;
            Result := nil;
          end;
        end;
      end else begin
        if AState.WriteAccess <> asDisabled then begin
          try
            Result := TFileStream.Create(AFileName, fmCreate);
            Result.Free;
            Result := nil;
          except
            AState.WriteAccess := asDisabled;
            Result := nil;
          end;
          if AState.WriteAccess <> asDisabled then begin
            try
              Result := TFileStream.Create(AFileName, fmOpenReadWrite + fmShareDenyWrite);
              AState.WriteAccess := asEnabled;
            except
              Result := nil;
              AState.WriteAccess := asDisabled;
            end;
          end;
        end;
      end;
    end;
  finally
    AState.UnlockWrite;
  end;
end;

function TMarkSystemSml.GetCategoryDB: IMarkCategoryDBImpl;
begin
  Result := FCategoryDBImpl;
end;

function TMarkSystemSml.GetMarkByStringId(const AId: string): IVectorDataItem;
var
  VId: Integer;
begin
  Result := nil;
  if AId <> '' then begin
    if TryStrToInt(AId, VId) then begin
      if not Supports(FMarkDbInternal.GetById(VId), IVectorDataItem, Result) then begin
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

function TMarkSystemSml.GetStringIdByMark(const AMark: IVectorDataItem): string;
var
  VMark: IMarkSMLInternal;
begin
  Result := '';
  if Assigned(AMark) and Supports(AMark.MainInfo, IMarkSMLInternal, VMark) then begin
    Result := IntToStr(VMark.Id);
  end;
end;

procedure TMarkSystemSml.MakeBackUp(const AFileName: string);
var
  VNewFileName: string;
begin
  VNewFileName := ChangeFileExt(AFileName, '.~sml');
  CopyFile(PChar(AFileName), PChar(VNewFileName), false);
end;

end.
