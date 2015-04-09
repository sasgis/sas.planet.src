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

unit u_ArchiveReadWriteKaZip;

interface

uses
  Classes,
  i_ArchiveReadWrite,
  i_ArchiveReadWriteFactory,
  u_BaseInterfacedObject;

type
  TArchiveReaderFactoryKaZip = class(TBaseInterfacedObject, IArchiveReaderFactory)
  private
    function BuildByFileName(const AFileName: string): IArchiveReader;
    function BuildByStream(const AStream: TStream): IArchiveReader;
  public
    constructor Create;
  end;

type
  TArchiveWriterFactoryKaZip = class(TBaseInterfacedObject, IArchiveWriterFactory)
  private
    function BuildByFileName(const AFileName: string): IArchiveWriter;
    function BuildByStream(const AStream: TStream): IArchiveWriter;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  KAZip,
  i_BinaryData,
  u_BinaryDataByMemStream,
  u_StreamReadOnlyByBinaryData;

{ TArchiveReadByKaZip }

type
  TArchiveReadByKaZip = class(TBaseInterfacedObject, IArchiveReader)
  private
    FZip: TKAZip;
  private
    function GetItemsCount: Integer;
    function GetItemByName(const AItemName: string): IBinaryData;
    function GetItemNameByIndex(const AItemIndex: Integer): string;
    function GetItemByIndex(
      const AItemIndex: Integer;
      out AItemName: string
    ): IBinaryData;
  public
    constructor Create(const AFileName: string); overload;
    constructor Create(const AStream: TStream); overload;
    destructor Destroy; override;
  end;

constructor TArchiveReadByKaZip.Create(const AFileName: string);
begin
  inherited Create;
  FZip := TKAZip.Create(nil);
  FZip.Open(AFileName);
end;

constructor TArchiveReadByKaZip.Create(const AStream: TStream);
begin
  inherited Create;
  FZip := TKAZip.Create(nil);
  FZip.Open(AStream);
end;

destructor TArchiveReadByKaZip.Destroy;
begin
  FreeAndNil(FZip);
  inherited;
end;

function TArchiveReadByKaZip.GetItemsCount: Integer;
begin
  Result := FZip.Entries.Count;
end;

function TArchiveReadByKaZip.GetItemByName(const AItemName: string): IBinaryData;
var
  VMemStream: TMemoryStream;
  VItemIndex: Integer;
begin
  Result := nil;
  VItemIndex := FZip.Entries.IndexOf(AnsiString(AItemName));
  if VItemIndex >= 0 then begin
    VMemStream := TMemoryStream.Create;
    try
      FZip.Entries.Items[VItemIndex].ExtractToStream(VMemStream);
      VMemStream.Position := 0;
      Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
      VMemStream := nil;
    finally
      VMemStream.Free;
    end;
  end;
end;

function TArchiveReadByKaZip.GetItemNameByIndex(const AItemIndex: Integer): string;
begin
  if FZip.Entries.Count >= AItemIndex then begin
    Result := string(ToDosName(FZip.Entries.Items[AItemIndex].FileName));
  end else begin
    Result := '';
  end;
end;

function TArchiveReadByKaZip.GetItemByIndex(
  const AItemIndex: Integer;
  out AItemName: string
): IBinaryData;
begin
  Result := nil;
  AItemName := GetItemNameByIndex(AItemIndex);
  if AItemName <> '' then begin
    Result := GetItemByName(AItemName);
  end;
end;

{ TArchiveWriteByKaZip }

type
  TArchiveWriteByKaZip = class(TBaseInterfacedObject, IArchiveWriter)
  private
    FZip: TKAZip;
    FIsFromFileName: Boolean;
  private
    function AddFile(
      const AFileData: IBinaryData;
      const AFileNameInArchive: string;
      const AFileDate: TDateTime
    ): Integer;
  public
    constructor Create(
      const AFileName: string;
      const AAllowOpenExisting: Boolean
    ); overload;
    constructor Create(const AStream: TStream); overload;
    destructor Destroy; override;
  end;

constructor TArchiveWriteByKaZip.Create(
  const AFileName: string;
  const AAllowOpenExisting: Boolean
);
begin
  inherited Create;
  FIsFromFileName := True;
  FZip := TKAZip.Create(nil);
  FZip.FileName := AFileName;

  if AAllowOpenExisting and FileExists(AFileName) then begin
    FZip.Open(AFileName);
  end else begin
    FZip.CreateZip(AFileName);
    FZip.CompressionType := ctFast;
  end;

  FZip.Active := True;
end;

constructor TArchiveWriteByKaZip.Create(const AStream: TStream);
begin
  inherited Create;
  FIsFromFileName := False;
  FZip := TKAZip.Create(nil);
  FZip.CreateZip(AStream);
  FZip.CompressionType := ctFast;
  FZip.Open(AStream);
end;

destructor TArchiveWriteByKaZip.Destroy;
begin
  if FIsFromFileName and Assigned(FZip) then begin
    FZip.Active := False;
    FZip.Close;
  end;
  FreeAndNil(FZip);
  inherited;
end;

function TArchiveWriteByKaZip.AddFile(
  const AFileData: IBinaryData;
  const AFileNameInArchive: string;
  const AFileDate: TDateTime
): Integer;
var
  VDataStream: TStream;
  VEntry: TKAZipEntriesEntry;
begin
  VDataStream := TStreamReadOnlyByBinaryData.Create(AFileData);
  try
    {$WARN SYMBOL_PLATFORM OFF}
    VEntry := FZip.AddStream(
      AnsiString(AFileNameInArchive),
      faArchive, // (!) platform
      AFileDate,
      VDataStream
    );
    {$WARN SYMBOL_PLATFORM ON}
    Result := VEntry.Index;
  finally
    VDataStream.Free;
  end;
end;

{ TArchiveReaderFactoryKaZip }

constructor TArchiveReaderFactoryKaZip.Create;
begin
  inherited Create;
end;

function TArchiveReaderFactoryKaZip.BuildByFileName(
  const AFileName: string): IArchiveReader;
begin
  Result := TArchiveReadByKaZip.Create(AFileName);
end;

function TArchiveReaderFactoryKaZip.BuildByStream(
  const AStream: TStream): IArchiveReader;
begin
  Result := TArchiveReadByKaZip.Create(AStream);
end;

{ TArchiveWriterFactoryKaZip }

constructor TArchiveWriterFactoryKaZip.Create;
begin
  inherited Create;
end;

function TArchiveWriterFactoryKaZip.BuildByFileName(
  const AFileName: string): IArchiveWriter;
begin
  Result := TArchiveWriteByKaZip.Create(AFileName, False);
end;

function TArchiveWriterFactoryKaZip.BuildByStream(
  const AStream: TStream): IArchiveWriter;
begin
  Result := TArchiveWriteByKaZip.Create(AStream);
end;

end.
