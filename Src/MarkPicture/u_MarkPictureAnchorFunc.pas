unit u_MarkPictureAnchorFunc;

interface

uses
  t_GeoTypes,
  c_MarkPictureAnchor;

function GetAnchorFromName(const AName: string): TDoublePoint;
function IsKnownAnchor(const AAnchor: TDoublePoint; out AName: string): Boolean;

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
