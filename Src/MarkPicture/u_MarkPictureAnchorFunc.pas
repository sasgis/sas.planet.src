unit u_MarkPictureAnchorFunc;

interface

uses
  Types,
  Classes,
  t_GeoTypes,
  c_MarkPictureAnchor;

function GetAnchorFromName(const AName: string): TDoublePoint;
function IsKnownAnchor(const AAnchor: TDoublePoint; out AName: string): Boolean;

procedure AddAnchorNamesToList(const AList: TStrings);

function AnchorRelativeToAbsolute(const ARelative: TDoublePoint; const APicSize: TPoint): TDoublePoint;
function AnchorAbsoluteToRelative(const AAbsolute: TDoublePoint; const APicSize: TPoint): TDoublePoint;

function AnchorAbsoluteToPoint(const AAbsolute: TDoublePoint; const APicSize: TPoint): TPoint;
function AnchorRelativeToPoint(const ARelative: TDoublePoint; const APicSize: TPoint): TPoint;


implementation

uses
  Math,
  SysUtils,
  u_GeoFunc;

type
  TAnchorItemRec = record
    ReadableName: string;
    Value: TDoublePoint;
  end;

var
  GAnchorItemsArray: array [0..8] of TAnchorItemRec;

function GetAnchorFromName(const AName: string): TDoublePoint;
var
  I: Integer;
begin
  Result := CEmptyDoublePoint;
  if AName <> '' then begin
    for I := 0 to Length(GAnchorItemsArray) - 1 do begin
      if SameText(AName, GAnchorItemsArray[I].ReadableName) then begin
        Result := GAnchorItemsArray[I].Value;
        Break;
      end;
    end;
  end;
end;

function IsKnownAnchor(const AAnchor: TDoublePoint; out AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(GAnchorItemsArray) - 1 do begin
    if DoublePointsEqual(AAnchor, GAnchorItemsArray[I].Value) then begin
      AName := GAnchorItemsArray[I].ReadableName;
      Result := True;
      Break;
    end;
  end;
end;

procedure AddAnchorNamesToList(const AList: TStrings);
var
  I: Integer;
begin
  for I := 0 to Length(GAnchorItemsArray) - 1 do begin
    AList.Add(GAnchorItemsArray[I].ReadableName);
  end;
end;

function AnchorRelativeToAbsolute(
  const ARelative: TDoublePoint;
  const APicSize: TPoint
): TDoublePoint;
begin
  if (ARelative.X < 0) or (ARelative.X > 1) or
     (ARelative.Y < 0) or (ARelative.Y > 1)
  then begin
    raise Exception.CreateFmt(
      'Anchor relative value out of range: %.2f; %.2f',
      [ARelative.X, ARelative.Y]
    );
  end;
  Result.X := (APicSize.X - 1) * ARelative.X + 1;
  Result.Y := (APicSize.Y - 1) * ARelative.Y + 1;
end;

function AnchorAbsoluteToRelative(
  const AAbsolute: TDoublePoint;
  const APicSize: TPoint
): TDoublePoint;
begin
  if (AAbsolute.X < 1) or (AAbsolute.X > APicSize.X) or
     (AAbsolute.Y < 1) or (AAbsolute.Y > APicSize.Y)
  then begin
    raise Exception.CreateFmt(
      'Anchor absolute value out of range: %.2f; %.2f (pic size: %dx%d)',
      [AAbsolute.X, AAbsolute.Y, APicSize.X, APicSize.Y]
    );
  end;
  Result.X := (AAbsolute.X - 1) / (APicSize.X - 1);
  Result.Y := (AAbsolute.Y - 1) / (APicSize.Y - 1);
end;

function AnchorAbsoluteToPoint(
  const AAbsolute: TDoublePoint;
  const APicSize: TPoint
): TPoint;
begin
  if (AAbsolute.X < 1) or (AAbsolute.X > APicSize.X) or
     (AAbsolute.Y < 1) or (AAbsolute.Y > APicSize.Y)
  then begin
    raise Exception.CreateFmt(
      'Anchor absolute value out of range: %.2f; %.2f (pic size: %dx%d)',
      [AAbsolute.X, AAbsolute.Y, APicSize.X, APicSize.Y]
    );
  end;
  Result := PointFromDoublePoint(AAbsolute, prToTopLeft);
end;

function AnchorRelativeToPoint(
  const ARelative: TDoublePoint;
  const APicSize: TPoint
): TPoint;
var
  VAbsolute: TDoublePoint;
begin
  VAbsolute := AnchorRelativeToAbsolute(ARelative, APicSize);
  Result := AnchorAbsoluteToPoint(VAbsolute, APicSize);
end;

procedure InitAnchorItemsArray;
begin
  GAnchorItemsArray[0].ReadableName := cPicAnchorTopLeftName;
  GAnchorItemsArray[0].Value := cPicAnchorTopLeft;

  GAnchorItemsArray[1].ReadableName := cPicAnchorTopName;
  GAnchorItemsArray[1].Value := cPicAnchorTop;

  GAnchorItemsArray[2].ReadableName := cPicAnchorTopRightName;
  GAnchorItemsArray[2].Value := cPicAnchorTopRight;

  GAnchorItemsArray[3].ReadableName := cPicAnchorLeftName;
  GAnchorItemsArray[3].Value := cPicAnchorLeft;

  GAnchorItemsArray[4].ReadableName := cPicAnchorCenterName;
  GAnchorItemsArray[4].Value := cPicAnchorCenter;

  GAnchorItemsArray[5].ReadableName := cPicAnchorRightName;
  GAnchorItemsArray[5].Value := cPicAnchorRight;

  GAnchorItemsArray[6].ReadableName := cPicAnchorBottomLeftName;
  GAnchorItemsArray[6].Value := cPicAnchorBottomLeft;

  GAnchorItemsArray[7].ReadableName := cPicAnchorBottomName;
  GAnchorItemsArray[7].Value := cPicAnchorBottom;

  GAnchorItemsArray[8].ReadableName := cPicAnchorBottomRightName;
  GAnchorItemsArray[8].Value := cPicAnchorBottomRight;
end;

initialization
  InitAnchorItemsArray;

end.
