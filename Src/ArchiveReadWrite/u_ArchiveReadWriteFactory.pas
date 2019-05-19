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

unit u_ArchiveReadWriteFactory;

interface

uses
  i_ArchiveReadWriteFactory,
  u_BaseInterfacedObject;

type
  TArchiveReadWriteFactory = class(TBaseInterfacedObject, IArchiveReadWriteFactory)
  private
    FZip: IArchiveType;
    FZipSequential: IArchiveTypeSequential;

    FTar: IArchiveType;
    FTarSequential: IArchiveTypeSequential;

    FSevenZip: IArchiveType;
    FRar: IArchiveType;
  private
    { IArchiveReadWriteFactory }
    function GetZip: IArchiveType;
    function GetZipSequential: IArchiveTypeSequential;

    function GetTar: IArchiveType;
    function GetTarSequential: IArchiveTypeSequential;

    function GetSevenZip: IArchiveType;

    function GetRar: IArchiveType;
  public
    constructor Create;
  end;

implementation

uses
  u_ArchiveReadWrite7Zip,
  u_ArchiveReadWriteSequentialTar,
  u_ArchiveReadWriteSequentialZip;

type
  TArchiveType = class(TBaseInterfacedObject, IArchiveType)
  private
    FReaderFactory: IArchiveReaderFactory;
    FWriterFactory: IArchiveWriterFactory;
  private
    { IArchiveType }
    function GetReaderFactory: IArchiveReaderFactory;
    function GetWriterFactory: IArchiveWriterFactory;
  public
    constructor Create(
      const AReaderFactory: IArchiveReaderFactory;
      const AWriterFactory: IArchiveWriterFactory
    );
  end;

  TArchiveTypeSequential = class(TBaseInterfacedObject, IArchiveTypeSequential)
  private
    FReaderFactory: IArchiveReaderSequentialFactory;
    FWriterFactory: IArchiveWriterSequentialFactory;
  private
    { IArchiveTypeSequential }
    function GetReaderFactory: IArchiveReaderSequentialFactory;
    function GetWriterFactory: IArchiveWriterSequentialFactory;
  public
    constructor Create(
      const AReaderFactory: IArchiveReaderSequentialFactory;
      const AWriterFactory: IArchiveWriterSequentialFactory
    );
  end;

{ TArchiveType }

constructor TArchiveType.Create(
  const AReaderFactory: IArchiveReaderFactory;
  const AWriterFactory: IArchiveWriterFactory
);
begin
  inherited Create;
  FReaderFactory := AReaderFactory;
  FWriterFactory := AWriterFactory;
end;

function TArchiveType.GetReaderFactory: IArchiveReaderFactory;
begin
  Result := FReaderFactory;
end;

function TArchiveType.GetWriterFactory: IArchiveWriterFactory;
begin
  Result := FWriterFactory;
end;

{ TArchiveTypeSequential }

constructor TArchiveTypeSequential.Create(
  const AReaderFactory: IArchiveReaderSequentialFactory;
  const AWriterFactory: IArchiveWriterSequentialFactory
);
begin
  inherited Create;
  FReaderFactory := AReaderFactory;
  FWriterFactory := AWriterFactory;
end;

function TArchiveTypeSequential.GetReaderFactory: IArchiveReaderSequentialFactory;
begin
  Result := FReaderFactory;
end;

function TArchiveTypeSequential.GetWriterFactory: IArchiveWriterSequentialFactory;
begin
  Result := FWriterFactory;
end;

{ TArchiveReadWriteFactory }

constructor TArchiveReadWriteFactory.Create;
begin
  inherited Create;

  // *.zip r/w
  FZip :=
    TArchiveType.Create(
      TArchiveReaderFactory7Zip.Create(atZip),
      TArchiveWriterFactory7Zip.Create(atZip)
    );
  FZipSequential :=
    TArchiveTypeSequential.Create(
      TArchiveReaderSequentialFactoryZip.Create,
      TArchiveWriterSequentialFactoryZip.Create
    );

  // *.tar r/w
  FTar :=
    TArchiveType.Create(
      TArchiveReaderFactory7Zip.Create(atTar),
      TArchiveWriterFactory7Zip.Create(atTar)
    );
  FTarSequential :=
    TArchiveTypeSequential.Create(
      TArchiveReaderSequentialFactoryTar.Create,
      TArchiveWriterSequentialFactoryTar.Create
    );

  // *.7z r/w
  FSevenZip :=
    TArchiveType.Create(
      TArchiveReaderFactory7Zip.Create(at7Zip),
      TArchiveWriterFactory7Zip.Create(at7Zip)
    );

  // *.rar r/o
  FRar :=
    TArchiveType.Create(
      TArchiveReaderFactory7Zip.Create(atRar),
      nil
    );
end;

function TArchiveReadWriteFactory.GetSevenZip: IArchiveType;
begin
  Result := FSevenZip;
end;

function TArchiveReadWriteFactory.GetRar: IArchiveType;
begin
  Result := FRar;
end;

function TArchiveReadWriteFactory.GetTar: IArchiveType;
begin
  Result := FTar;
end;

function TArchiveReadWriteFactory.GetTarSequential: IArchiveTypeSequential;
begin
  Result := FTarSequential;
end;

function TArchiveReadWriteFactory.GetZip: IArchiveType;
begin
  Result := FZip;
end;

function TArchiveReadWriteFactory.GetZipSequential: IArchiveTypeSequential;
begin
  Result := FZipSequential;
end;

end.
