unit u_MarkDbImplHelperORM;

interface

uses
  Windows,
  Classes,
  SysUtils,
  mORMot,
  SynCommons,
  t_MarkSystemORM,
  u_MarkSystemORMModel,
  i_GeometryLonLat,
  i_GeometryToStream,
  i_GeometryFromStream;

procedure DeleteMarkSQL(
  const AMarkID: TID;
  const AClient: TSQLRestClient
);

procedure InsertMarkSQL(
  var AMarkRec: TSQLMarkRec;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const AGeometryWriter: IGeometryToStream
);

procedure UpdateMarkSQL(
  const AOldMarkRec: TSQLMarkRec;
  var ANewMarkRec: TSQLMarkRec;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const AGeometryWriter: IGeometryToStream
);

function GetSQLMarkRec(
  var AMarkRec: TSQLMarkRec;
  const AUserID: TID;
  const ASQLMark: TSQLMark;
  const AClient: TSQLRestClient;
  const AOptions: TSQLMarkRecOptions;
  const AGeometryReader: IGeometryFromStream;
  const AIsJoined: Boolean
): Boolean;

function SetMarkVisibleSQL(
  const AMarkID: TID;
  const AUserID: TID;
  const AVisible: Boolean;
  const AClient: TSQLRestClient;
  const AUseTransaction: Boolean
): Boolean;

implementation

uses
  t_GeoTypes,
  u_MarkSystemORMTools;

function _GeomertryFromBlob(
  const ABlob: TSQLRawBlob;
  const AGeometryReader: IGeometryFromStream
): IGeometryLonLat; forward;

function _GeomertryToBlob(
  const AGeometry: IGeometryLonLat;
  const AGeometryWriter: IGeometryToStream
): TSQLRawBlob; forward;

function _AddMarkImage(
  const AClient: TSQLRestClient;
  const APicName: string
): TID;
var
  VPicName: RawUTF8;
  VSQLMarkImage: TSQLMarkImage;
begin
  Result := 0;
  VPicName := StringToUTF8(APicName);
  VSQLMarkImage := TSQLMarkImage.Create(AClient, 'Name=?', [VPicName]);
  try
    if VSQLMarkImage.ID = 0 then begin
      VSQLMarkImage.Name := VPicName;
      CheckID( AClient.Add(VSQLMarkImage, True) );
    end;
    Result := VSQLMarkImage.ID;
  finally
    VSQLMarkImage.Free;
  end;
end;

function _AddMarkAppearance(
  const AClient: TSQLRestClient;
  const AColor1, AColor2: Cardinal;
  const AScale1, AScale2: Integer
): TID;
var
  VSQLMarkAppearance: TSQLMarkAppearance;
begin
  Result := 0;
  VSQLMarkAppearance := TSQLMarkAppearance.Create(
    AClient, 'Color1=? AND Color2=? AND Scale1=? AND Scale2=?',
    [AColor1, AColor2, AScale1, AScale2]
  );
  try
    if VSQLMarkAppearance.ID = 0 then begin
      VSQLMarkAppearance.Color1 := AColor1;
      VSQLMarkAppearance.Color2 := AColor2;
      VSQLMarkAppearance.Scale1 := AScale1;
      VSQLMarkAppearance.Scale2 := AScale2;
      CheckID( AClient.Add(VSQLMarkAppearance, True) );
    end;
    Result := VSQLMarkAppearance.ID;
  finally
    VSQLMarkAppearance.Free;
  end;
end;

procedure DeleteMarkSQL(
  const AMarkID: TID;
  const AClient: TSQLRestClient
);
var
  VTransaction: TTransactionRec;
begin
  Assert(AMarkID > 0);
  Assert(AClient <> nil);

  StartTransaction(AClient, VTransaction, TSQLMark);
  try
    // delete view for all Users if exists
    AClient.Delete(TSQLMarkView, FormatUTF8('Mark=?', [], [AMarkID]));

    // delete rect
    CheckDeleteResult( AClient.Delete(TSQLMarkRTree, AMarkID) );

    // delete name and desc
    CheckDeleteResult( AClient.Delete(TSQLMarkFTS, AMarkID) );

    // delete mark
    CheckDeleteResult( AClient.Delete(TSQLMark, AMarkID) );

    // pic name and appearance are never deleted...

    CommitTransaction(AClient, VTransaction);
  except
    RollBackTransaction(AClient, VTransaction);
    raise;
  end;
end;

procedure InsertMarkSQL(
  var AMarkRec: TSQLMarkRec;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const AGeometryWriter: IGeometryToStream
);
var
  VRect: TDoubleRect;
  VGeometryBlob: TSQLRawBlob;
  VSQLMark: TSQLMark;
  VSQLMarkView: TSQLMarkView;
  VSQLMarkFTS: TSQLMarkFTS;
  VSQLMarkRTree: TSQLMarkRTree;
  VTransaction: TTransactionRec;
begin
  Assert(AClient <> nil);
  Assert(AGeometryWriter <> nil);

  VRect := AMarkRec.FGeometry.Bounds.Rect;
  VGeometryBlob := _GeomertryToBlob(AMarkRec.FGeometry, AGeometryWriter);
  CalcGeometrySize(VRect, AMarkRec.FGeoLonSize, AMarkRec.FGeoLatSize);

  StartTransaction(AClient, VTransaction, TSQLMark);
  try
    if AMarkRec.FPicName <> '' then begin
      AMarkRec.FPicId := _AddMarkImage(AClient, AMarkRec.FPicName);
    end else begin
      AMarkRec.FPicId := 0;
    end;

    AMarkRec.FAppearanceId := _AddMarkAppearance(
      AClient, AMarkRec.FColor1, AMarkRec.FColor2, AMarkRec.FScale1, AMarkRec.FScale2
    );

    VSQLMark := TSQLMark.Create;
    try
      VSQLMark.Category := Pointer(AMarkRec.FCategoryId);
      VSQLMark.Image := Pointer(AMarkRec.FPicId);
      VSQLMark.Appearance := Pointer(AMarkRec.FAppearanceId);

      VSQLMark.Name := StringToUTF8(AMarkRec.FName);
      VSQLMark.Desc := StringToUTF8(AMarkRec.FDesc);

      VSQLMark.GeoLonSize := AMarkRec.FGeoLonSize;
      VSQLMark.GeoLatSize := AMarkRec.FGeoLatSize;
      VSQLMark.GeoType := AMarkRec.FGeoType;
      VSQLMark.GeoCount := AMarkRec.FGeoCount;

      // add mark
      CheckID( AClient.Add(VSQLMark, True) );
      AMarkRec.FMarkId := VSQLMark.ID;
      // add geometry blob
      CheckUpdateResult( AClient.UpdateBlob(TSQLMark, AMarkRec.FMarkId, 'GeoWKB', VGeometryBlob) );
    finally
      VSQLMark.Free;
    end;

    VSQLMarkFTS := TSQLMarkFTS.Create;
    try
      VSQLMarkFTS.DocID := AMarkRec.FMarkId;
      VSQLMarkFTS.Name := StringToUTF8(SysUtils.AnsiLowerCase(AMarkRec.FName));
      VSQLMarkFTS.Desc := StringToUTF8(SysUtils.AnsiLowerCase(AMarkRec.FDesc));
      // add name and desc
      CheckID( AClient.Add(VSQLMarkFTS, True) );
    finally
      VSQLMarkFTS.Free;
    end;

    VSQLMarkRTree := TSQLMarkRTree.Create;
    try
      VSQLMarkRTree.IDValue := AMarkRec.FMarkId;
      VSQLMarkRTree.Left := VRect.Left;
      VSQLMarkRTree.Right := VRect.Right;
      VSQLMarkRTree.Top := VRect.Top;
      VSQLMarkRTree.Bottom := VRect.Bottom;
      // add rect
      CheckID( AClient.Add(VSQLMarkRTree, True) );
    finally
      VSQLMarkRTree.Free;
    end;

    if not AMarkRec.FVisible then begin
      VSQLMarkView := TSQLMarkView.Create;
      try
        VSQLMarkView.User := Pointer(AUserID);
        VSQLMarkView.Mark := Pointer(AMarkRec.FMarkId);
        VSQLMarkView.Visible := AMarkRec.FVisible;
        // add view
        CheckID( AClient.Add(VSQLMarkView, True) );
      finally
        VSQLMarkView.Free;
      end;
    end;

    CommitTransaction(AClient, VTransaction);
  except
    RollBackTransaction(AClient, VTransaction);
    raise;
  end;
end;

procedure UpdateMarkSQL(
  const AOldMarkRec: TSQLMarkRec;
  var ANewMarkRec: TSQLMarkRec;
  const AUserID: TID;
  const AClient: TSQLRestClient;
  const AGeometryWriter: IGeometryToStream
);
var
  VFieldsCount: Integer;
  VFieldsNames: array of RawUTF8;

  procedure _ClearFields;
  begin
    VFieldsCount := 0;
    SetLength(VFieldsNames, VFieldsCount);
  end;

  procedure _AddField(const AName: string);
  begin
    SetLength(VFieldsNames, VFieldsCount + 1);
    VFieldsNames[VFieldsCount] := StringToUTF8(AName);
    Inc(VFieldsCount);
  end;

  function _FieldsNamesStr: RawUTF8;
  var
    I: Integer;
    VSep: RawUTF8;
  begin
    VSep := '';
    Result := '';
    for I := 0 to Length(VFieldsNames) - 1 do begin
      if I = 1 then begin
        VSep := RawUTF8(',');
      end;
      Result := Result + VSep + VFieldsNames[I];
    end;
  end;

var
  VRect: TDoubleRect;
  VGeometryBlob: TSQLRawBlob;
  VSQLMark: TSQLMark;
  VSQLMarkFTS: TSQLMarkFTS;
  VSQLMarkRTree: TSQLMarkRTree;
  VTransaction: TTransactionRec;
  VUpdatePic: Boolean;
  VUpdateGeo: Boolean;
  VUpdateName: Boolean;
  VUpdateDesc: Boolean;
  VUpdateAppearance: Boolean;
begin
  Assert(AClient <> nil);

  CheckID(AOldMarkRec.FMarkId);

  ANewMarkRec.FMarkId := AOldMarkRec.FMarkId;

  VUpdatePic := (AOldMarkRec.FPicName <> ANewMarkRec.FPicName);
  VUpdateGeo := not ANewMarkRec.FGeometry.IsSameGeometry(AOldMarkRec.FGeometry);
  VUpdateName := (AOldMarkRec.FName <> ANewMarkRec.FName);
  VUpdateDesc := (AOldMarkRec.FDesc <> ANewMarkRec.FDesc);

  VUpdateAppearance :=
    (AOldMarkRec.FColor1 <> ANewMarkRec.FColor1) or
    (AOldMarkRec.FColor2 <> ANewMarkRec.FColor2) or
    (AOldMarkRec.FScale1 <> ANewMarkRec.FScale1) or
    (AOldMarkRec.FScale2 <> ANewMarkRec.FScale2);

  if VUpdateGeo then begin
    VRect := ANewMarkRec.FGeometry.Bounds.Rect;
    VGeometryBlob := _GeomertryToBlob(ANewMarkRec.FGeometry, AGeometryWriter);
    CalcGeometrySize(VRect, ANewMarkRec.FGeoLonSize, ANewMarkRec.FGeoLatSize);
  end;

  StartTransaction(AClient, VTransaction, TSQLMark);
  try
    if VUpdatePic then begin
      if ANewMarkRec.FPicName <> '' then begin
        ANewMarkRec.FPicId := _AddMarkImage(AClient, ANewMarkRec.FPicName);
      end else begin
        ANewMarkRec.FPicId := 0;
      end;
    end;

    if VUpdateAppearance then begin
      ANewMarkRec.FAppearanceId := _AddMarkAppearance(
        AClient,
        ANewMarkRec.FColor1, ANewMarkRec.FColor2,
        ANewMarkRec.FScale1, ANewMarkRec.FScale2
      );
    end;

    VSQLMark := TSQLMark.Create;
    try
      _ClearFields;

      VSQLMark.IDValue := ANewMarkRec.FMarkId;

      if AOldMarkRec.FCategoryId <> ANewMarkRec.FCategoryId then begin
        _AddField('Category');
        VSQLMark.Category := Pointer(ANewMarkRec.FCategoryId);
      end;
      if VUpdatePic then begin
        _AddField('Image');
        VSQLMark.Image := Pointer(ANewMarkRec.FPicId);
      end;
      if VUpdateAppearance then begin
        _AddField('Appearance');
        VSQLMark.Appearance := Pointer(ANewMarkRec.FAppearanceId);
      end;
      if VUpdateName then begin
        _AddField('Name');
        VSQLMark.Name := StringToUTF8(ANewMarkRec.FName);
      end;
      if VUpdateDesc then begin
        _AddField('Desc');
        VSQLMark.Desc := StringToUTF8(ANewMarkRec.FDesc);
      end;

      if VUpdateGeo then begin
        _AddField('GeoLonSize');
        _AddField('GeoLatSize');
        _AddField('GeoType');
        _AddField('GeoCount');
        VSQLMark.GeoLonSize := ANewMarkRec.FGeoLonSize;
        VSQLMark.GeoLatSize := ANewMarkRec.FGeoLatSize;
        VSQLMark.GeoType := ANewMarkRec.FGeoType;
        VSQLMark.GeoCount := ANewMarkRec.FGeoCount;
      end;

      if VFieldsCount > 0 then begin
        // update mark
        CheckUpdateResult( AClient.Update(VSQLMark, _FieldsNamesStr) );
      end;

      if VUpdateGeo then begin
        // update geometry blob
        CheckUpdateResult(
          AClient.UpdateBlob(TSQLMark, VSQLMark.ID, 'GeoWKB', VGeometryBlob)
        );
      end;
    finally
      VSQLMark.Free;
    end;

    VSQLMarkFTS := TSQLMarkFTS.Create;
    try
      _ClearFields;

      VSQLMarkFTS.DocID := ANewMarkRec.FMarkId;

      if VUpdateName then begin
        _AddField('Name');
        VSQLMarkFTS.Name := StringToUTF8(SysUtils.AnsiLowerCase(ANewMarkRec.FName));
      end;
      if VUpdateDesc then begin
        _AddField('Desc');
        VSQLMarkFTS.Desc := StringToUTF8(SysUtils.AnsiLowerCase(ANewMarkRec.FDesc));
      end;

      if VFieldsCount > 0 then begin
        // update name / desc
        CheckUpdateResult( AClient.Update(VSQLMarkFTS, _FieldsNamesStr) );
      end;
    finally
      VSQLMarkFTS.Free;
    end;

    if VUpdateGeo then begin
      VSQLMarkRTree := TSQLMarkRTree.Create;
      try
        VSQLMarkRTree.IDValue := ANewMarkRec.FMarkId;
        VSQLMarkRTree.Left := VRect.Left;
        VSQLMarkRTree.Right := VRect.Right;
        VSQLMarkRTree.Top := VRect.Top;
        VSQLMarkRTree.Bottom := VRect.Bottom;
        // update rect
        CheckUpdateResult( AClient.Update(VSQLMarkRTree) );
      finally
        VSQLMarkRTree.Free;
      end;
    end;

    if AOldMarkRec.FVisible <> ANewMarkRec.FVisible then begin
      // update view
      SetMarkVisibleSQL(
        ANewMarkRec.FMarkId, AUserID, ANewMarkRec.FVisible, AClient, False
      );
    end;

    CommitTransaction(AClient, VTransaction);
  except
    RollBackTransaction(AClient, VTransaction);
    raise;
  end;
end;

function GetSQLMarkRec(
  var AMarkRec: TSQLMarkRec;
  const AUserID: TID;
  const ASQLMark: TSQLMark;
  const AClient: TSQLRestClient;
  const AOptions: TSQLMarkRecOptions;
  const AGeometryReader: IGeometryFromStream;
  const AIsJoined: Boolean
): Boolean;
var
  VSQLMarkView: TSQLMarkView;
  VSQLMarkImage: TSQLMarkImage;
  VSQLMarkAppearance: TSQLMarkAppearance;
  VSQLBlobData: TSQLRawBlob;
begin
  Result := False;

  AMarkRec := cEmptySQLMarkRec;

  AMarkRec.FMarkId := ASQLMark.ID;

  if AMarkRec.FMarkId > 0 then begin
    AMarkRec.FCategoryId := TID(ASQLMark.Category.AsTSQLRecord);
    CheckID(AMarkRec.FCategoryId);

    AMarkRec.FPicId := TID(ASQLMark.Image.AsTSQLRecord); // = 0 is OK

    AMarkRec.FAppearanceId := TID(ASQLMark.Appearance.AsTSQLRecord);
    CheckID(AMarkRec.FAppearanceId);

    AMarkRec.FName := UTF8ToString(ASQLMark.Name);
    AMarkRec.FDesc := UTF8ToString(ASQLMark.Desc);

    AMarkRec.FGeoLonSize := ASQLMark.GeoLonSize;
    AMarkRec.FGeoLatSize := ASQLMark.GeoLatSize;
    AMarkRec.FGeoType := ASQLMark.GeoType;
    AMarkRec.FGeoCount := ASQLMark.GeoCount;
  end else begin
    Exit;
  end;

  // read geometry blob
  if (mrAll in AOptions) or (mrGeometry in AOptions) then begin
    CheckRetrieveResult( AClient.RetrieveBlob(TSQLMark, ASQLMark.ID, 'GeoWKB', VSQLBlobData) );
    AMarkRec.FGeometry := _GeomertryFromBlob(VSQLBlobData, AGeometryReader);
  end;

  // read view
  if (mrAll in AOptions) or (mrView in AOptions) then begin
    VSQLMarkView := TSQLMarkView.Create(AClient, 'Mark=? AND User=?', [ASQLMark.ID, AUserID]);
    try
      if VSQLMarkView.ID > 0 then begin
        AMarkRec.FVisible := VSQLMarkView.Visible;
      end else begin
        AMarkRec.FVisible := True;
      end;
    finally
      VSQLMarkView.Free;
    end;
  end;

  // read pic name
  if (AMarkRec.FGeoType = gtPoint) and ( (mrAll in AOptions) or (mrPic in AOptions) ) then begin
    if AMarkRec.FPicId > 0 then begin
      if AIsJoined then begin
        VSQLMarkImage := ASQLMark.Image;
      end else begin
        VSQLMarkImage := TSQLMarkImage.Create(AClient, AMarkRec.FPicId);
      end;
      try
        CheckID(VSQLMarkImage.ID);
        AMarkRec.FPicName := UTF8ToString(VSQLMarkImage.Name);
      finally
        if not AIsJoined then begin
          VSQLMarkImage.Free;
        end;
      end;
    end else begin
      AMarkRec.FPicName := '';
    end;
  end;

  // read appearance
  if (mrAll in AOptions) or (mrAppearance in AOptions) then begin
    if AIsJoined then begin
      VSQLMarkAppearance := ASQLMark.Appearance;
    end else begin
      VSQLMarkAppearance := TSQLMarkAppearance.Create(AClient, AMarkRec.FAppearanceId);
    end;
    try
      CheckID(VSQLMarkAppearance.ID);
      AMarkRec.FColor1 := VSQLMarkAppearance.Color1;
      AMarkRec.FColor2 := VSQLMarkAppearance.Color2;
      AMarkRec.FScale1 := VSQLMarkAppearance.Scale1;
      AMarkRec.FScale2 := VSQLMarkAppearance.Scale2;
    finally
      if not AIsJoined then begin
        VSQLMarkAppearance.Free;
      end;
    end;
  end;

  Result := True;
end;

function SetMarkVisibleSQL(
  const AMarkID: TID;
  const AUserID: TID;
  const AVisible: Boolean;
  const AClient: TSQLRestClient;
  const AUseTransaction: Boolean
): Boolean;
var
  VSQLMarkView: TSQLMarkView;
  VTransaction: TTransactionRec;
begin
  Assert(AMarkID > 0);

  Result := False;

  if AUseTransaction then begin
    StartTransaction(AClient, VTransaction, TSQLMarkView);
  end;
  try
    VSQLMarkView := TSQLMarkView.Create(AClient, 'Mark=? AND User=?', [AMarkID, AUserID]);
    try
      if VSQLMarkView.ID > 0 then begin
        if VSQLMarkView.Visible <> AVisible then begin
          Result := AClient.UpdateField(TSQLMarkView, VSQLMarkView.ID, 'Visible', [AVisible]);
          CheckUpdateResult(Result);
        end;
      end else if not AVisible then begin
        VSQLMarkView.User := Pointer(AUserID);
        VSQLMarkView.Mark := Pointer(AMarkID);
        VSQLMarkView.Visible := AVisible;
        CheckID(AClient.Add(VSQLMarkView, True));
        Result := True;
      end;
    finally
      VSQLMarkView.Free;
    end;
    if AUseTransaction then begin
      CommitTransaction(AClient, VTransaction);
    end;
  except
    if AUseTransaction then begin
      RollBackTransaction(AClient, VTransaction);
    end;
    raise;
  end;
end;

function _GeomertryFromBlob(
  const ABlob: TSQLRawBlob;
  const AGeometryReader: IGeometryFromStream
): IGeometryLonLat;
var
  VStream: TRawByteStringStream;
begin
  Assert(ABlob <> '');
  Assert(AGeometryReader <> nil);

  VStream := TRawByteStringStream.Create(ABlob);
  try
    Result := AGeometryReader.Parse(VStream);
  finally
    VStream.Free;
  end;
end;

function _GeomertryToBlob(
  const AGeometry: IGeometryLonLat;
  const AGeometryWriter: IGeometryToStream
): TSQLRawBlob;
var
  VStream: TRawByteStringStream;
begin
  Assert(AGeometry <> nil);
  Assert(AGeometryWriter <> nil);

  VStream := TRawByteStringStream.Create;
  try
    AGeometryWriter.Save(AGeometry, VStream);
    Result := VStream.DataString;
  finally
    VStream.Free;
  end;
end;

end.
