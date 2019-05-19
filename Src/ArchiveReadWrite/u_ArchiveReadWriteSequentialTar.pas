{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_ArchiveReadWriteSequentialTar;

interface

uses
  i_ArchiveReadWrite,
  i_ArchiveReadWriteFactory,
  u_BaseInterfacedObject;

type
  TArchiveReaderSequentialFactoryTar = class(TBaseInterfacedObject, IArchiveReaderSequentialFactory)
  private
    { IArchiveReaderSequentialFactory }
    function Build(
      const AFileName: string;
      const AOptions: Pointer = nil
    ): IArchiveReaderSequential;
  end;

  TArchiveWriterSequentialFactoryTar = class(TBaseInterfacedObject, IArchiveWriterSequentialFactory)
  private
    { IArchiveWriterSequentialFactory }
    function Build(
      const AFileName: string;
      const AOptions: Pointer = nil
    ): IArchiveWriterSequential;
  end;

implementation

uses
  Classes,
  SysUtils,
  libtar,
  i_BinaryData,
  u_BinaryDataByMemStream,
  u_StreamReadOnlyByBinaryData,
  u_StrFunc;

type
  TArchiveReaderSequentialTar = class(TBaseInterfacedObject, IArchiveReaderSequential)
  private
    FTar: TTarArchive;
  private
    procedure Reset;
    function Next(
      out AFileData: IBinaryData;
      out AFileNameInArchive: string;
      out AFileDate: TDateTime
    ): Boolean;
  public
    constructor Create(
      const AFileName: string;
      const AOptions: Pointer = nil
    );
    destructor Destroy; override;
  end;

  TArchiveWriterSequentialTar = class(TBaseInterfacedObject, IArchiveWriterSequential)
  private
    FTar: TTarWriter;
  private
    procedure Add(
      const AFileData: IBinaryData;
      const AFileNameInArchive: string;
      const AFileDate: TDateTime
    );
  public
    constructor Create(
      const AFileName: string;
      const AOptions: Pointer = nil
    );
    destructor Destroy; override;
  end;

{ TArchiveReaderSequentialFactoryTar }

function TArchiveReaderSequentialFactoryTar.Build(
  const AFileName: string;
  const AOptions: Pointer
): IArchiveReaderSequential;
begin
  Result := TArchiveReaderSequentialTar.Create(AFileName, AOptions);
end;

{ TArchiveWriterSequentialFactoryTar }

function TArchiveWriterSequentialFactoryTar.Build(
  const AFileName: string;
  const AOptions: Pointer
): IArchiveWriterSequential;
begin
  Result := TArchiveWriterSequentialTar.Create(AFileName, AOptions);
end;

{ TArchiveWriteSequentialTar }

constructor TArchiveWriterSequentialTar.Create(
  const AFileName: string;
  const AOptions: Pointer
);
begin
  inherited Create;
  FTar := TTarWriter.Create(AFileName);
end;

destructor TArchiveWriterSequentialTar.Destroy;
begin
  FreeAndNil(FTar);
  inherited;
end;

procedure TArchiveWriterSequentialTar.Add(
  const AFileData: IBinaryData;
  const AFileNameInArchive: string;
  const AFileDate: TDateTime
);
var
  VStream: TStream;
  VFileName: AnsiString;
begin
  if IsAscii(AFileNameInArchive) then begin
    VFileName := AnsiString(AFileNameInArchive);
  end else begin
    VFileName := UTF8Encode(AFileNameInArchive);
  end;

  VStream := TStreamReadOnlyByBinaryData.Create(AFileData);
  try
    FTar.AddStream(VStream, VFileName, AFileDate);
  finally
    VStream.Free;
  end;
end;

{ TArchiveReaderSequentialTar }

constructor TArchiveReaderSequentialTar.Create(
  const AFileName: string;
  const AOptions: Pointer
);
begin
  inherited Create;
  FTar := TTarArchive.Create(AFileName);
end;

destructor TArchiveReaderSequentialTar.Destroy;
begin
  FreeAndNil(FTar);
  inherited;
end;

procedure TArchiveReaderSequentialTar.Reset;
begin
  FTar.Reset;
end;

function TArchiveReaderSequentialTar.Next(
  out AFileData: IBinaryData;
  out AFileNameInArchive: string;
  out AFileDate: TDateTime
): Boolean;
var
  VDirRec: TTarDirRec;
  VStream: TMemoryStream;
begin
  Result := False;
  while FTar.FindNext(VDirRec) do begin
    if VDirRec.FileType = ftNormal then begin // regular file

      AFileNameInArchive := {$IF CompilerVersion >= 33}UTF8String{$ELSE}UTF8Decode{$IFEND}(VDirRec.Name);
      if PathDelim <> '/' then begin
        AFileNameInArchive := StringReplace(AFileNameInArchive, '/', PathDelim, [rfReplaceAll]);
      end;

      AFileDate := VDirRec.DateTime;

      if VDirRec.Size > 0 then begin
        VStream := TMemoryStream.Create;
        try
          FTar.ReadFile(VStream);
          Assert(VStream.Size = VDirRec.Size);
          VStream.Position := 0;
          AFileData := TBinaryDataByMemStream.CreateWithOwn(VStream);
          VStream := nil;
        finally
          VStream.Free;
        end;
      end else begin
        AFileData := nil;
      end;

      Result := True;
      Break;
    end;
  end;
end;

end.
