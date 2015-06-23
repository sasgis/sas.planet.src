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

function GetSQLMarkRec(
  var AMarkRec: TSQLMarkRec;
  const AUserID: TID;
  const ASQLMark: TSQLMark;
  const AClient: TSQLRestClient;
  const AOptions: TSQLMarkRecOptions;
  const AGeometryReader: IGeometryFromStream
): Boolean;

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
