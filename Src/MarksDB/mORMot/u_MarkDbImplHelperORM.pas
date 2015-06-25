unit u_MarkDbImplHelperORM;

interface

uses
  Windows,
  Classes,
  SysUtils,
  mORMot,
  SynCommons,
  t_MarkSystemORM,
  t_MarkSystemModelORM,
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
  const AGeometryReader: IGeometryFromStream
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

procedure DeleteMarkSQL(
  const AMarkID: TID;
  const AClient: TSQLRestClient
);
var
  VSQLWhere: RawUTF8;
  VTransaction: TTransactionRec;
begin
  Assert(AMarkID > 0);
  Assert(AClient <> nil);

  StartTransaction(AClient, VTransaction, TSQLMark);
  try
    // delete view
    VSQLWhere := FormatUTF8('Mark=?', [], [AMarkID]); // for all Users
    CheckDeleteResult( AClient.Delete(TSQLMarkView, VSQLWhere) );

    // delete rect
    CheckDeleteResult( AClient.Delete(TSQLMarkGeometryRect, AMarkID) );

    // delete name and desc
    CheckDeleteResult( AClient.Delete(TSQLMarkTextInfo, AMarkID) );

    // delete mark
    CheckDeleteResult( AClient.Delete(TSQLMark, AMarkID) );

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
  VPicName: RawUTF8;
  VGeometryBlob: TSQLRawBlob;
  VSQLMark: TSQLMark;
  VSQLMarkView: TSQLMarkView;
  VSQLMarkImage: TSQLMarkImage;
  VSQLMarkTextInfo: TSQLMarkTextInfo;
  VSQLMarkGeometryRect: TSQLMarkGeometryRect;
  VTransaction: TTransactionRec;
begin
  Assert(AClient <> nil);
  Assert(AGeometryWriter <> nil);

  VRect := AMarkRec.FGeometry.Bounds.Rect;

  StartTransaction(AClient, VTransaction, TSQLMark);
  try
    if AMarkRec.FPicName <> '' then begin
      VPicName := StringToUTF8(AMarkRec.FPicName);
      VSQLMarkImage := TSQLMarkImage.Create(AClient, 'Name=?', [VPicName]);
      try
        if VSQLMarkImage.ID = 0 then begin
          VSQLMarkImage.Name := VPicName;
          // add pic
          CheckID( AClient.Add(VSQLMarkImage, True) );
        end;
        AMarkRec.FPicId := VSQLMarkImage.ID;
      finally
        VSQLMarkImage.Free;
      end;
    end else begin
      AMarkRec.FPicId := 0;
    end;

    VSQLMark := TSQLMark.Create;
    try
      VGeometryBlob := _GeomertryToBlob(AMarkRec.FGeometry, AGeometryWriter);
      VSQLMark.Category := Pointer(AMarkRec.FCategoryId);
      VSQLMark.Image := Pointer(AMarkRec.FPicId);

      VSQLMark.Color1 := AMarkRec.FColor1;
      VSQLMark.Color2 := AMarkRec.FColor2;

      VSQLMark.Scale1 := AMarkRec.FScale1;
      VSQLMark.Scale2 := AMarkRec.FScale2;

      VSQLMark.Name := StringToUTF8(AMarkRec.FName);
      VSQLMark.Desc := StringToUTF8(AMarkRec.FDesc);

      VSQLMark.GeoLon := AMarkRec.FGeoLon;
      VSQLMark.GeoLat := AMarkRec.FGeoLat;
      VSQLMark.GeoType := AMarkRec.FGeoType;
      VSQLMark.GeoCount := AMarkRec.FGeoCount;

      // add mark
      CheckID( AClient.Add(VSQLMark, True) );
      AMarkRec.FMarkId := VSQLMark.ID;
      // add geometry blob
      CheckUpdateResult( AClient.UpdateBlob(TSQLMark, AMarkRec.FMarkId, 'Geometry', VGeometryBlob) );
    finally
      VSQLMark.Free;
    end;

    VSQLMarkTextInfo := TSQLMarkTextInfo.Create;
    try
      VSQLMarkTextInfo.DocID := AMarkRec.FMarkId;
      VSQLMarkTextInfo.Name := StringToUTF8(SysUtils.AnsiLowerCase(AMarkRec.FName));
      VSQLMarkTextInfo.Desc := StringToUTF8(SysUtils.AnsiLowerCase(AMarkRec.FDesc));
      // add name and desc
      CheckID( AClient.Add(VSQLMarkTextInfo, True) );
    finally
      VSQLMarkTextInfo.Free;
    end;

    VSQLMarkGeometryRect := TSQLMarkGeometryRect.Create;
    try
      VSQLMarkGeometryRect.IDValue := AMarkRec.FMarkId;
      VSQLMarkGeometryRect.Left := VRect.Left;
      VSQLMarkGeometryRect.Right := VRect.Right;
      VSQLMarkGeometryRect.Top := VRect.Top;
      VSQLMarkGeometryRect.Bottom := VRect.Bottom;
      // add rect
      CheckID( AClient.Add(VSQLMarkGeometryRect, True) );
    finally
      VSQLMarkGeometryRect.Free;
    end;

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
  VPicName: RawUTF8;
  VGeometryBlob: TSQLRawBlob;
  VSQLMark: TSQLMark;
  VSQLMarkImage: TSQLMarkImage;
  VSQLMarkTextInfo: TSQLMarkTextInfo;
  VSQLMarkGeometryRect: TSQLMarkGeometryRect;
  VTransaction: TTransactionRec;
  VUpdatePic: Boolean;
  VUpdateGeo: Boolean;
  VUpdateName: Boolean;
  VUpdateDesc: Boolean;
begin
  Assert(AClient <> nil);

  CheckID(AOldMarkRec.FMarkId);

  ANewMarkRec.FMarkId := AOldMarkRec.FMarkId;

  VUpdatePic := (AOldMarkRec.FPicName <> ANewMarkRec.FPicName);
  VUpdateGeo := not ANewMarkRec.FGeometry.IsSameGeometry(AOldMarkRec.FGeometry);
  VUpdateName := (AOldMarkRec.FName <> ANewMarkRec.FName);
  VUpdateDesc := (AOldMarkRec.FDesc <> ANewMarkRec.FDesc);

  StartTransaction(AClient, VTransaction, TSQLMark);
  try
    if VUpdatePic then begin
      if ANewMarkRec.FPicName <> '' then begin
        VPicName := StringToUTF8(ANewMarkRec.FPicName);
        VSQLMarkImage := TSQLMarkImage.Create(AClient, 'Name=?', [VPicName]);
        try
          if VSQLMarkImage.ID = 0 then begin
            VSQLMarkImage.Name := VPicName;
            // add pic
            CheckID( AClient.Add(VSQLMarkImage, True) );
          end;
          ANewMarkRec.FPicId := VSQLMarkImage.ID;
        finally
          VSQLMarkImage.Free;
        end;
      end else begin
        ANewMarkRec.FPicId := 0;
      end;
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
      if AOldMarkRec.FColor1 <> ANewMarkRec.FColor1 then begin
        _AddField('Color1');
        VSQLMark.Color1 := ANewMarkRec.FColor1;
      end;
      if AOldMarkRec.FColor2 <> ANewMarkRec.FColor2 then begin
        _AddField('Color2');
        VSQLMark.Color2 := ANewMarkRec.FColor2;
      end;
      if AOldMarkRec.FScale1 <> ANewMarkRec.FScale1 then begin
        _AddField('Scale1');
        VSQLMark.Scale1 := ANewMarkRec.FScale1;
      end;
      if AOldMarkRec.FScale2 <> ANewMarkRec.FScale2 then begin
        _AddField('Scale2');
        VSQLMark.Scale2 := ANewMarkRec.FScale2;
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
        _AddField('GeoLon');
        _AddField('GeoLat');
        _AddField('GeoType');
        _AddField('GeoCount');
        VSQLMark.GeoLon := ANewMarkRec.FGeoLon;
        VSQLMark.GeoLat := ANewMarkRec.FGeoLat;
        VSQLMark.GeoType := ANewMarkRec.FGeoType;
        VSQLMark.GeoCount := ANewMarkRec.FGeoCount;
      end;

      if VFieldsCount > 0 then begin
        // update mark
        CheckUpdateResult( AClient.Update(VSQLMark, _FieldsNamesStr) );
      end;

      if VUpdateGeo then begin
        // update geometry blob
        VGeometryBlob := _GeomertryToBlob(ANewMarkRec.FGeometry, AGeometryWriter);
        CheckUpdateResult(
          AClient.UpdateBlob(TSQLMark, VSQLMark.ID, 'Geometry', VGeometryBlob)
        );
      end;
    finally
      VSQLMark.Free;
    end;

    VSQLMarkTextInfo := TSQLMarkTextInfo.Create;
    try
      _ClearFields;

      VSQLMarkTextInfo.DocID := ANewMarkRec.FMarkId;

      if VUpdateName then begin
        _AddField('name');
        VSQLMarkTextInfo.name := StringToUTF8(SysUtils.AnsiLowerCase(ANewMarkRec.FName));
      end;
      if VUpdateDesc then begin
        _AddField('desc');
        VSQLMarkTextInfo.desc := StringToUTF8(SysUtils.AnsiLowerCase(ANewMarkRec.FDesc));
      end;

      if VFieldsCount > 0 then begin
        // update name / desc
        CheckUpdateResult( AClient.Update(VSQLMarkTextInfo, _FieldsNamesStr) );
      end;
    finally
      VSQLMarkTextInfo.Free;
    end;

    if VUpdateGeo then begin
      VSQLMarkGeometryRect := TSQLMarkGeometryRect.Create;
      try
        VRect := ANewMarkRec.FGeometry.Bounds.Rect;
        VSQLMarkGeometryRect.IDValue := ANewMarkRec.FMarkId;
        VSQLMarkGeometryRect.Left := VRect.Left;
        VSQLMarkGeometryRect.Right := VRect.Right;
        VSQLMarkGeometryRect.Top := VRect.Top;
        VSQLMarkGeometryRect.Bottom := VRect.Bottom;
        // update rect
        CheckUpdateResult( AClient.Update(VSQLMarkGeometryRect) );
      finally
        VSQLMarkGeometryRect.Free;
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
  const AGeometryReader: IGeometryFromStream
): Boolean;
var
  VSQLMarkView: TSQLMarkView;
  VSQLMarkImage: TSQLMarkImage;
  VSQLBlobData: TSQLRawBlob;
begin
  Result := False;

  AMarkRec.FMarkId := ASQLMark.ID;

  if AMarkRec.FMarkId > 0 then begin
    AMarkRec.FCategoryId := TID(ASQLMark.Category);
    AMarkRec.FPicId := TID(ASQLMark.Image);

    AMarkRec.FColor1 := ASQLMark.Color1;
    AMarkRec.FColor2 := ASQLMark.Color2;

    AMarkRec.FScale1 := ASQLMark.Scale1;
    AMarkRec.FScale2 := ASQLMark.Scale2;

    AMarkRec.FName := UTF8ToString(ASQLMark.Name);
    AMarkRec.FDesc := UTF8ToString(ASQLMark.Desc);

    AMarkRec.FGeoLon := ASQLMark.GeoLon;
    AMarkRec.FGeoLat := ASQLMark.GeoLat;
    AMarkRec.FGeoType := ASQLMark.GeoType;
    AMarkRec.FGeoCount := ASQLMark.GeoCount;
  end else begin
    Exit;
  end;

  // read geometry blob
  if (mrAll in AOptions) or (mrGeometry in AOptions) then begin
    CheckRetrieveResult( AClient.RetrieveBlob(TSQLMark, ASQLMark.ID, 'Geometry', VSQLBlobData) );
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
      VSQLMarkImage := TSQLMarkImage.Create(AClient, AMarkRec.FPicId);
      try
        CheckID(VSQLMarkImage.ID);
        AMarkRec.FPicName := UTF8ToString(VSQLMarkImage.Name);
      finally
        VSQLMarkImage.Free;
      end;
    end else begin
      AMarkRec.FPicName := '';
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
  VStream: TStringStream;
begin
  Assert(ABlob <> '');
  Assert(AGeometryReader <> nil);

  VStream := TStringStream.Create(ABlob);
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
  VSize: Int64;
  VStream: TMemoryStream;
begin
  Assert(AGeometry <> nil);
  Assert(AGeometryWriter <> nil);

  VStream := TMemoryStream.Create;
  try
    AGeometryWriter.Save(AGeometry, VStream);
    VSize := VStream.Size;
    SetLength(Result, VSize);
    VStream.Position := 0;
    VStream.ReadBuffer(Result[1], VSize);
  finally
    VStream.Free;
  end;
end;

end.
