unit u_TileStorageSQLiteFunc;

interface

uses
  i_BinaryData,
  i_MapVersionInfo,
  t_TileStorageSQLiteHandler;

function CreateTileBinaryData(
  const AOrigSize: Integer;
  const ABlobSize: Integer;
  const ABlobData: Pointer
): IBinaryData;

procedure ParseSQLiteDBVersion(
  const AUseVersionFieldInDB: Boolean;
  const ATBColInfoModeV: TVersionColMode;
  const ARequestedVersionInfo: IMapVersionInfo;
  out AInfo: TSelectTileInfoComplex
);

function VersionFieldIsEqual(
  const ARequestedVersionIsInt: Boolean;
  const ATBColInfoModeV: TVersionColMode;
  const AStrVersionToDB: AnsiString
): AnsiString;

implementation

uses
  ALString,
  SysUtils,
  c_TileStorageSQLite,
  u_BinaryData;

function CreateTileBinaryData(
  const AOrigSize: Integer;
  const ABlobSize: Integer;
  const ABlobData: Pointer
): IBinaryData;
begin
  Assert(ABlobData <> nil);
  Assert(AOrigSize = ABlobSize);
  Result := TBinaryData.Create(ABlobSize, ABlobData);
end;

function LocalTryStrToInt64(const S: AnsiString; out Value: Int64): Boolean;
begin
  // X701 is string
  // $123 is string
  Result := not (S[1] in ['x','X','$']) and ALTryStrToInt64(S, Value);
end;

function GetDefaultDBVersion(const AVersionColMode: TVersionColMode): AnsiString; inline;
begin
  if AVersionColMode = vcm_Int then begin
    Result := ALIntToStr(cDefaultVersionAsIntValue);
  end else begin
    Result := ALQuotedStr(cDefaultVersionAsStrValue);
  end;
end;

function VersionStoreStringToAnsi(const AVersionString: String): AnsiString; inline;
begin
  Result := AnsiString(AVersionString);
end;

function VersionInfoToAnsi(const AVersionInfo: IMapVersionInfo): AnsiString; inline;
begin
  Result := VersionStoreStringToAnsi(AVersionInfo.StoreString);
end;

procedure ParseSQLiteDBVersion(
  const AUseVersionFieldInDB: Boolean;
  const ATBColInfoModeV: TVersionColMode;
  const ARequestedVersionInfo: IMapVersionInfo;
  out AInfo: TSelectTileInfoComplex
);
begin
  FillChar(AInfo, SizeOf(AInfo), 0);

  if AUseVersionFieldInDB and Assigned(ARequestedVersionInfo) then begin
    AInfo.RequestedVersionToDB := VersionInfoToAnsi(ARequestedVersionInfo);
    if Length(AInfo.RequestedVersionToDB) = 0 then begin
      // default version
      AInfo.RequestedVersionToDB := GetDefaultDBVersion(ATBColInfoModeV);
      AInfo.RequestedVersionIsInt := True;
    end else if LocalTryStrToInt64(AInfo.RequestedVersionToDB, AInfo.RequestedVersionAsInt) then begin
      // as int - result ok
      if ATBColInfoModeV = vcm_Text then begin
        // версия в БД текстовая
        AInfo.RequestedVersionToDB := ALQuotedStr(AInfo.RequestedVersionToDB);
      end;
      AInfo.RequestedVersionIsInt := True;
      AInfo.RequestedVersionIsSet := True;
    end else begin
      // as text - quote result
      AInfo.RequestedVersionToDB := ALQuotedStr(AInfo.RequestedVersionToDB);
      AInfo.RequestedVersionIsInt := False;
      AInfo.RequestedVersionIsSet := True;
    end;
  end else begin
    // default version
    AInfo.RequestedVersionToDB := GetDefaultDBVersion(ATBColInfoModeV);
    AInfo.RequestedVersionIsInt := True;
  end;
end;

function VersionFieldIsEqual(
  const ARequestedVersionIsInt: Boolean;
  const ATBColInfoModeV: TVersionColMode;
  const AStrVersionToDB: AnsiString
): AnsiString;
begin
  // формируем кусок вида v = VERSION
  case ATBColInfoModeV of
    vcm_Int: begin
      // версия в БД целочисленная
      if ARequestedVersionIsInt then begin
        Result := 'v'; // если оба целые - значит так и будет
      end else begin
        Result := 'cast(v as TEXT)'; // разные - значит всё конвертируем к строке
      end;
    end;
    vcm_Text: begin
      // версия в БД текстовая
      if not ARequestedVersionIsInt then begin
        Result := 'v'; // если оба строки - значит так и будет
      end else begin
        Result := 'cast(v as TEXT)'; // разные - значит всё конвертируем к строке
      end;
    end;
  else
    begin
      Assert(False, IntToStr(Ord(ATBColInfoModeV)));
    end;
  end;
  // формируем условие сравнения версий
  Result := Result + '=' + AStrVersionToDB;
end;

end.
