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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_ArchiveReadWrite7Zip;

interface

uses
  Classes,
  i_ArchiveReadWrite,
  i_ArchiveReadWriteFactory,
  u_BaseInterfacedObject;

type
  TArchiveType = (atTar = 0, atZip = 1, atRar = 2, at7Zip = 3);

type
  TArchiveWriterFactory7Zip = class(TBaseInterfacedObject, IArchiveWriterFactory)
  private
    FType: TArchiveType;
  private
    function BuildByFileName(const AFileName: string): IArchiveWriter;
    function BuildByStream(const AStream: TStream): IArchiveWriter;
  public
    constructor Create(AType: TArchiveType);
  end;

type
  TArchiveReaderFactory7Zip = class(TBaseInterfacedObject, IArchiveReaderFactory)
  private
    FType: TArchiveType;
  private
    function BuildByFileName(const AFileName: string): IArchiveReader;
    function BuildByStream(const AStream: TStream): IArchiveReader;
  public
    constructor Create(AType: TArchiveType);
  end;


implementation

uses
  Windows,
  SysUtils,
  SevenZip,
  i_BinaryData,
  u_BinaryDataByMemStream,
  u_StreamReadOnlyByBinaryData;

type
  EArchiveWriteBy7Zip = class(Exception);

{ TArchiveReadBy7Zip }

type
  TArchiveReadBy7Zip = class(TBaseInterfacedObject, IArchiveReader)
  private
    FArch: I7zInArchive;
    FOwnStream: Boolean;
    FStream: TStream;
  private
    function CreateArchive(const AArchiveType: TArchiveType): I7zInArchive;
    // IArchiveReader
    function GetItemsCount: Integer;
    function GetItemByName(const AItemName: string): IBinaryData;
    function GetItemNameByIndex(const AItemIndex: Integer): string;
    function GetItemByIndex(
      const AItemIndex: Integer;
      out AItemName: string
    ): IBinaryData;
  public
    constructor Create(
      const AFileName: string;
      const AArchiveType: TArchiveType
    ); overload;
    constructor Create(
      const AStream: TStream;
      const AArchiveType: TArchiveType
    ); overload;
    destructor Destroy; override;
  end;

constructor TArchiveReadBy7Zip.Create(
  const AFileName: string;
  const AArchiveType: TArchiveType
);
begin
  inherited Create;
  FOwnStream := True;
  FStream := TFileStream.Create(AFileName, fmOpenRead);
  FArch := CreateArchive(AArchiveType);
end;

constructor TArchiveReadBy7Zip.Create(
  const AStream: TStream;
  const AArchiveType: TArchiveType
);
begin
  inherited Create;
  FOwnStream := False;
  FStream := AStream;
  FArch := CreateArchive(AArchiveType);
end;

destructor TArchiveReadBy7Zip.Destroy;
begin
  if Assigned(FArch) then begin
    FArch.Close;
  end;
  if FOwnStream then begin
    FreeAndNil(FStream);
  end;
  inherited;
end;

function TArchiveReadBy7Zip.CreateArchive(
  const AArchiveType: TArchiveType
): I7zInArchive;
begin
  case AArchiveType of
    atTar: begin
      Result := CreateInArchive(CLSID_CFormatTar);
    end;
    atZip: begin
      Result := CreateInArchive(CLSID_CFormatZip);
    end;
    at7Zip: begin
      Result := CreateInArchive(CLSID_CFormat7z);
    end;
  else // atRar
  begin
    Result := CreateInArchive(CLSID_CFormatRar);
  end;
  end;
  if Result <> nil then begin
    Result.OpenStream(T7zStream.Create(FStream, soReference));
  end;
end;

function TArchiveReadBy7Zip.GetItemsCount: Integer;
begin
  Result := FArch.NumberOfItems;
end;

function TArchiveReadBy7Zip.GetItemByName(const AItemName: string): IBinaryData;
var
  I: Integer;
  VItemName: string;
begin
  for I := 0 to FArch.NumberOfItems - 1 do begin
    VItemName := FArch.ItemPath[I];
    if AItemName = VItemName then begin
      Result := GetItemByIndex(I, VItemName);
      Break;
    end;
  end;
end;

function TArchiveReadBy7Zip.GetItemNameByIndex(const AItemIndex: Integer): string;
begin
  Result := FArch.ItemPath[AItemIndex];
end;

function TArchiveReadBy7Zip.GetItemByIndex(
  const AItemIndex: Integer;
  out AItemName: string
): IBinaryData;
var
  VStream: TMemoryStream;
begin
  if not FArch.ItemIsFolder[AItemIndex] then begin
    AItemName := FArch.ItemPath[AItemIndex];
    if AItemName <> '' then begin
      VStream := TMemoryStream.Create;
      try
        FArch.ExtractItem(AItemIndex, VStream, False);
        VStream.Position := 0;
        Result := TBinaryDataByMemStream.CreateWithOwn(VStream);
        VStream := nil;
      finally
        VStream.Free;
      end;
    end;
  end else begin
    AItemName := '';
  end;
end;

{ TArchiveWriteBy7Zip }

type
  TArchiveWriteBy7Zip = class(TBaseInterfacedObject, IArchiveWriter)
  private
    FArch: I7zOutArchive;
    FOwnStream: Boolean;
    FStream: TStream;
    FFilesCount: Integer;
  private
    function CreateArchive(const AArchiveType: TArchiveType): I7zOutArchive;
    // IArchiveWriter
    function AddFile(
      const AFileData: IBinaryData;
      const AFileNameInArchive: string;
      const AFileDate: TDateTime
    ): Integer;
  public
    constructor Create(
      const AFileName: string;
      const AArchiveType: TArchiveType
    ); overload;
    constructor Create(
      const AStream: TStream;
      const AArchiveType: TArchiveType
    ); overload;
    destructor Destroy; override;
  end;

constructor TArchiveWriteBy7Zip.Create(
  const AFileName: string;
  const AArchiveType: TArchiveType
);
begin
  inherited Create;
  FFilesCount := 0;
  FOwnStream := True;
  FStream := TFileStream.Create(AFileName, fmCreate);
  FArch := CreateArchive(AArchiveType);
end;

constructor TArchiveWriteBy7Zip.Create(
  const AStream: TStream;
  const AArchiveType: TArchiveType
);
begin
  inherited Create;
  FFilesCount := 0;
  FOwnStream := False;
  FStream := AStream;
  FArch := CreateArchive(AArchiveType);
end;

destructor TArchiveWriteBy7Zip.Destroy;
begin
  if Assigned(FArch) and Assigned(FStream) then begin
    FArch.SaveToStream(FStream);
  end;
  if FOwnStream then begin
    FreeAndNil(FStream);
  end;
  inherited;
end;

function TArchiveWriteBy7Zip.CreateArchive(
  const AArchiveType: TArchiveType
): I7zOutArchive;
begin
  case AArchiveType of
    atTar: begin
      Result := CreateOutArchive(CLSID_CFormatTar);
    end;
    atZip:
    begin
      Result := CreateOutArchive(CLSID_CFormatZip);
      SetCompressionMethod(Result, mzDeflate);
    end;
    at7Zip: begin
      Result := CreateOutArchive(CLSID_CFormat7z);
    end;
  else // atRar
  begin
    raise EArchiveWriteBy7Zip.Create('Unsupport open RAR in write mode!');
  end;
  end;
end;

function TArchiveWriteBy7Zip.AddFile(
  const AFileData: IBinaryData;
  const AFileNameInArchive: string;
  const AFileDate: TDateTime
): Integer;
var
  VStream: TStream;
  VFileTime: TFileTime;
begin
  VFileTime := DateTimeToFileTime(AFileDate);
  VStream := TStreamReadOnlyByBinaryData.Create(AFileData);
  try
    {$WARN SYMBOL_PLATFORM OFF}
    FArch.AddStream(
      VStream,
      soOwned,
      faArchive, // (!) platform
      VFileTime,
      VFileTime,
      AFileNameInArchive,
      False,
      False
    );
    {$WARN SYMBOL_PLATFORM ON}
    VStream := nil;
    Inc(FFilesCount);
    Result := FFilesCount;
  finally
    VStream.Free;
  end;
end;

{ TArchiveReaderFactory7Zip }

constructor TArchiveReaderFactory7Zip.Create(AType: TArchiveType);
begin
  inherited Create;
  FType := AType;
end;

function TArchiveReaderFactory7Zip.BuildByFileName(
  const AFileName: string): IArchiveReader;
begin
  Result := TArchiveReadBy7Zip.Create(AFileName, FType);
end;

function TArchiveReaderFactory7Zip.BuildByStream(
  const AStream: TStream): IArchiveReader;
begin
  Result := TArchiveReadBy7Zip.Create(AStream, FType);
end;

{ TArchiveWriterFactory7Zip }

constructor TArchiveWriterFactory7Zip.Create(AType: TArchiveType);
begin
  inherited Create;
  FType := AType;
end;

function TArchiveWriterFactory7Zip.BuildByFileName(
  const AFileName: string): IArchiveWriter;
begin
  Result := TArchiveWriteBy7Zip.Create(AFileName, FType);
end;

function TArchiveWriterFactory7Zip.BuildByStream(
  const AStream: TStream): IArchiveWriter;
begin
  Result := TArchiveWriteBy7Zip.Create(AStream, FType);
end;

end.
