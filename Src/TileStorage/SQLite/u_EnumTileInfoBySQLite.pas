unit u_EnumTileInfoBySQLite;

interface

uses
  i_TileInfoBasic,
  i_FileNameIterator,
  i_TileFileNameParser,
  i_TileStorageSQLiteHolder,
  i_TileStorageSQLiteFetcher,
  u_BaseInterfacedObject;

type
  TTileEnumState = (
    tes_Start,
    tes_Finish,
    tes_Fetched
  );
  
  TEnumTileInfoBySQLite = class(TBaseInterfacedObject, IEnumTileInfo)
  private
    FStoragePath: string;
    FFilesIterator: IFileNameIterator;
    FTileFileNameParser: ITileFileNameParser;
    FHolder: ITileStorageSQLiteHolder;
    FFetcher: ITileStorageSQLiteFetcher;
    FZoom: Byte;
    FState: TTileEnumState;
    FUseVersionFieldInDB: Boolean;
    FIgnoreTNE: Boolean;
    FIgnoreMultiVersionTiles: Boolean;
  private
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const AStoragePath: string;
      const AFilesIterator: IFileNameIterator;
      const ATileFileNameParser: ITileFileNameParser;
      const AHolder: ITileStorageSQLiteHolder;
      const AUseVersionFieldInDB: Boolean;
      const AIgnoreTNE: Boolean;
      const AIgnoreMultiVersionTiles: Boolean
    );
  end;

implementation

uses
  Types,
  SysUtils,
  u_StrFunc,
  u_TileStorageSQLiteFetcher;

{ TEnumTileInfoBySQLite }

constructor TEnumTileInfoBySQLite.Create(
  const AStoragePath: string;
  const AFilesIterator: IFileNameIterator;
  const ATileFileNameParser: ITileFileNameParser;
  const AHolder: ITileStorageSQLiteHolder;
  const AUseVersionFieldInDB: Boolean;
  const AIgnoreTNE: Boolean;
  const AIgnoreMultiVersionTiles: Boolean
);
begin
  inherited Create;
  FStoragePath := AStoragePath;
  FFilesIterator := AFilesIterator;
  FTileFileNameParser := ATileFileNameParser;
  FHolder := AHolder;
  FUseVersionFieldInDB := AUseVersionFieldInDB;
  FIgnoreTNE := AIgnoreTNE;
  FIgnoreMultiVersionTiles := AIgnoreMultiVersionTiles; // (!) not used
  FFetcher := nil;
  FState := tes_Start;
  FZoom := 0;
end;

function TEnumTileInfoBySQLite.Next(var ATileInfo: TTileInfo): Boolean;
var
  VDummy: TPoint;
  VDatabaseFilename: string;
  VDatabaseFilenameA: AnsiString;
begin
  if (FState = tes_Finish) or (FFilesIterator = nil) then begin
    // finished
    Result := False;
    Exit;
  end;

  repeat
    // start new database
    while FState = tes_Start do begin
      if FFilesIterator.Next(VDatabaseFilename) then begin
        VDatabaseFilenameA := '';
        if IsAscii(VDatabaseFilename) then begin
          VDatabaseFilenameA := StringToAsciiSafe(VDatabaseFilename);
        end;
        if VDatabaseFilenameA = '' then begin
          // failed - goto next database
          FFetcher := nil;
          FState := tes_Start;
          Continue;
        end;
        // parse filename
        if FTileFileNameParser.GetTilePoint(VDatabaseFilenameA, VDummy, FZoom) then begin
          FFetcher :=
            TTileStorageSQLiteFetcherComplex.Create(
              FHolder,
              FStoragePath + VDatabaseFilename,
              nil,
              FUseVersionFieldInDB,
              FIgnoreTNE
            );
          // check opened
          if (FFetcher <> nil) and FFetcher.Opened then begin
            // created - fetch row
            if FFetcher.Fetch(ATileInfo) then begin
              // done
              ATileInfo.FZoom := FZoom;
              FState := tes_Fetched;
              Result := True;
              Exit;
            end else begin
              // cannot fetch row - close and get next
              FFetcher := nil;
            end;
          end else begin
            // failed to open - close and get next
            FFetcher := nil;
          end;
        end else begin
          // failed to open - close and get next
          FFetcher := nil;
        end;
      end else begin
        // no more databases
        FFilesIterator := nil;
        FFetcher := nil;
        FState := tes_Finish;
        Result := False;
        Exit;
      end;
    end;

    // get rows
    if FState = tes_Fetched then begin
      // was fetched - try to select another
      if FFetcher.Fetch(ATileInfo) then begin
        // successfully
        ATileInfo.FZoom := FZoom;
        Result := True;
        Exit;
      end else begin
        // failed - goto next database
        FFetcher := nil;
        FState := tes_Start;
      end;
    end;
  until False;
end;

end.
