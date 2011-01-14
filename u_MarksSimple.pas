unit u_MarksSimple;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarksSimple,
  i_IMarkPicture;

type
  TCategoryId = class
    id: integer;
    name: string;
    AfterScale: Integer;
    BeforeScale: Integer;
    visible: boolean;
  end;

  TMarksIteratorBase = class
  protected
    FCurrentMark: IMarkFull;
  public
    constructor Create;
    destructor Destroy; override;
    function Next: Boolean; virtual; abstract;
    property Current: IMarkFull read FCurrentMark;
  end;

implementation

uses
  SysUtils;

{ TMarkFull }

//procedure TMarkFull.ClosePoly;
//var
//  VPointCount: Integer;
//begin
//  VPointCount := Length(Points);
//  if VPointCount > 1 then begin
//    if (Points[0].X <> Points[VPointCount - 1].X) or
//      (Points[0].Y <> Points[VPointCount - 1].Y) then begin
//      SetLength(Points, VPointCount + 1);
//      Points[VPointCount] := Points[0];
//    end;
//  end;
//end;
//

//
//procedure TMarkFull.SetPic(const APic: IMarkPicture; const APicName: string);
//begin
//  FPic := APic;
//  FPicName := APicName;
//end;

{ TMarksIteratorBase }

constructor TMarksIteratorBase.Create;
begin
//  FCurrentMark := TMarkFull.Create;
end;

destructor TMarksIteratorBase.Destroy;
begin
//  FreeAndNil(FCurrentMark);
  inherited;
end;

end.
