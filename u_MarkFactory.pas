unit u_MarkFactory;

interface

uses
  GR32,
  t_GeoTypes,
  i_IMarkPicture,
  i_MarksSimple;

type
  TMarkFactory =  class
  private
    function GetLLRectFromPoints(APoints: TDoublePointArray): TDoubleRect;
    function GetLLRectFromPoint(APoint: TDoublePoint): TDoubleRect;
    function GetArrayFromPoint(APoint: TDoublePoint): TDoublePointArray;
  public
    function CreatePoint(
      AName: string;
      AVisible: Boolean;
      APicName: string;
      APic: IMarkPicture;
      ACategoryId: Integer;
      ADesc: string;
      APoint: TDoublePoint;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ): IMarkFull;
    function CreateLine(
      AName: string;
      AVisible: Boolean;
      ACategoryId: Integer;
      ADesc: string;
      APoints: TDoublePointArray;
      AColor1: TColor32;
      AScale1: Integer
    ): IMarkFull;
    function CreatePoly(
      AName: string;
      AVisible: Boolean;
      ACategoryId: Integer;
      ADesc: string;
      APoints: TDoublePointArray;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer
    ): IMarkFull;
  end;
implementation

uses
  u_MarksSimpleNew;

{ TMarkFactory }

function TMarkFactory.CreateLine(
  AName: string;
  AVisible: Boolean;
  ACategoryId: Integer;
  ADesc: string;
  APoints: TDoublePointArray;
  AColor1: TColor32;
  AScale1: Integer
): IMarkFull;
var
  VLLRect: TDoubleRect;
  VPoints: TDoublePointArray;
begin
  Result := TMarkFull.Create(
    AName,
    -1,
    AVisible,
    '',
    nil,
    ACategoryId,
    ADesc,
    VLLRect,
    VPoints,
    AColor1,
    0,
    AScale1,
    0
  );
end;

function TMarkFactory.CreatePoint(
  AName: string;
  AVisible: Boolean;
  APicName: string;
  APic: IMarkPicture;
  ACategoryId: Integer;
  ADesc: string;
  APoint: TDoublePoint;
  AColor1, AColor2: TColor32;
  AScale1,
  AScale2: Integer
): IMarkFull;
begin

end;

function TMarkFactory.CreatePoly(
  AName: string;
  AVisible: Boolean;
  ACategoryId: Integer;
  ADesc: string;
  APoints: TDoublePointArray;
  AColor1, AColor2: TColor32;
  AScale1: Integer
): IMarkFull;
begin

end;

function TMarkFactory.GetArrayFromPoint(
  APoint: TDoublePoint): TDoublePointArray;
begin
  SetLength(Result, 1);
  Result[0] := APoint;
end;

function TMarkFactory.GetLLRectFromPoint(APoint: TDoublePoint): TDoubleRect;
begin
  Result.TopLeft := APoint;
  Result.BottomRight := APoint;
end;

function TMarkFactory.GetLLRectFromPoints(
  APoints: TDoublePointArray): TDoubleRect;
var
  VCount: Integer;
  i: Integer;
begin
  VCount := Length(APoints);
  if VCount > 0 then begin
    Result.TopLeft := APoints[0];
    Result.BottomRight := APoints[0];
    for i := 1 to VCount - 1 do begin
      if Result.Left > APoints[i].X then begin
        Result.Left := APoints[i].X;
      end;
      if Result.Top < APoints[i].Y then begin
        Result.Top := APoints[i].Y;
      end;
      if Result.Right < APoints[i].X then begin
        Result.Right := APoints[i].X;
      end;
      if Result.Bottom > APoints[i].Y then begin
        Result.Bottom := APoints[i].Y;
      end;
    end;
  end else begin
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := 0;
    Result.Bottom := 0;
  end;
end;

end.
