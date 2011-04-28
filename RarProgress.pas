unit RarProgress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls;

type
  TRarProgress = class(TGraphicControl)
  private
    FMin,FMax,FProgress1,FProgress2: cardinal;
    FDouble: boolean;
    FPercent1,FPercent2: byte;

    FLightColor1,FDarkColor1,FLightColor2,FFrameColor1,FFrameColor2,
    FFillColor1,FFillColor2,FBackFrameColor1,FBackFrameColor2,
    FBackFillColor,FShadowColor: TColor;

    TopX,TopY,SizeX,SizeY: cardinal;

    procedure Draw;
    procedure SetMin(Value: cardinal);
    procedure SetMax(Value: cardinal);
    procedure SetProgress1(Value: cardinal);
    procedure SetProgress2(Value: cardinal);
    procedure SetDouble(Value: boolean);

  protected
    procedure Paint; override;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property Min: cardinal read FMin write SetMin;
    property Max: cardinal read FMax write SetMax;
    property Progress1: cardinal read FProgress1 write SetProgress1;
    property Progress2: cardinal read FProgress2 write SetProgress2;
    property Double: boolean read FDouble write SetDouble;
    property Percent1: byte read FPercent1;
    property Percent2: byte read FPercent2;

    property LightColor1: TColor read FLightColor1 write FLightColor1;
    property DarkColor1: TColor read FDarkColor1 write FDarkColor1;
    property LightColor2: TColor read FLightColor2 write FLightColor2;
    property FrameColor1: TColor read FFrameColor1 write FFrameColor1;
    property FrameColor2: TColor read FFrameColor2 write FFrameColor2;
    property FillColor1: TColor read FFillColor1 write FFillColor1;
    property FillColor2: TColor read FFillColor2 write FFillColor2;
    property BackFrameColor1: TColor read FBackFrameColor1 write FBackFrameColor1;
    property BackFrameColor2: TColor read FBackFrameColor2 write FBackFrameColor2;
    property BackFillColor: TColor read FBackFillColor write FBackFillColor;
    property ShadowColor: TColor read FShadowColor write FShadowColor;
    property Color;

    procedure Add1(Delta: cardinal);
    procedure Add2(Delta: cardinal);
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TRarProgress]);
end;

constructor TRarProgress.Create(AOwner: TComponent);
begin
  inherited;
  FMin:=0;
  FMax:=100;
  FDouble:=False;
  FProgress1:=0;
  FProgress2:=0;
  FPercent1:=0;
  FPercent2:=0;

  FLightColor1:=clWhite;
  FDarkColor1:=$606060;
  FLightColor2:=$C0FFFF;
  FFrameColor1:=$EEE8E8;
  FFrameColor2:=$B4D4E4;
  FFillColor1:=$DCD6D6;
  FFillColor2:=$A0C0D0;
  FBackFrameColor1:=$9494B4;
  FBackFrameColor2:=$80809E;
  FBackFillColor:=$6E6E94;
  FShadowColor:=$464040;

  Width:=204;
  Height:=18;
end;

procedure TRarProgress.SetMin(Value: cardinal);
begin
  if Value>FMax then Value:=FMax;
  FMin:=Value;
  Draw;
end;

procedure TRarProgress.SetMax(Value: cardinal);
begin
  if Value<FMin then Value:=FMin;
  FMax:=Value;
  Draw;
end;

procedure TRarProgress.SetProgress1(Value: cardinal);
begin
  if FDouble then if Value<FProgress2 then Value:=FProgress2;
  try
  FProgress1:=Value;
  except
  end;
  Draw;
end;

procedure TRarProgress.SetProgress2(Value: cardinal);
begin
  if FDouble then if Value>FProgress1 then Value:=FProgress1;
  try
  FProgress2:=Value;
  except
  end;
  Draw;
end;

procedure TRarProgress.SetDouble(Value: boolean);
begin
  FDouble:=Value;
  Draw;
end;

procedure TRarProgress.Paint;
begin
  inherited;
  TopX:=2;
  TopY:=2;
  SizeX:=Width-int64(TopX)-2;
  SizeY:=Height-int64(TopY)-4;
  Draw;
end;

procedure TRarProgress.Draw;
var TMP: TBitmap;
    R: real;
    Prog: cardinal;
begin
  if (SizeX=0) or (SizeY=0) or (FMax-FMin=0) then Exit;
  TMP:=TBitmap.Create;
  TMP.Width:=Width;
  TMP.Height:=Height;

///////////////////////////////////////////////////////////////////////////////
//              Draw main
///////////////////////////////////////////////////////////////////////////////

  with TMP.Canvas do
    begin
      Brush.Style:=bsSolid;
      Brush.Color:=Color;
      FillRect(Bounds(0,0,TMP.Width,TMP.Height));
      Brush.Color:=FShadowColor;
      FillRect(Bounds(TopX+1,TopY+2,SizeX,SizeY));
      Brush.Color:=FBackFillColor;
      FillRect(Bounds(TopX,TopY,SizeX,SizeY+1));
      Brush.Color:=FDarkColor1;
      FrameRect(Bounds(TopX,TopY,SizeX,SizeY+1));
      Brush.Color:=FBackFrameColor1;
      FrameRect(Bounds(TopX,TopY,SizeX,SizeY));
      Brush.Color:=FBackFrameColor2;
      FrameRect(Bounds(TopX+1,TopY+1,SizeX-2,SizeY-2));
    end;

///////////////////////////////////////////////////////////////////////////////
//              Draw first indicator
///////////////////////////////////////////////////////////////////////////////

  R:=(FProgress1-FMin)/((FMax-FMin)/SizeX);
  Prog:=Round(R);
  FPercent1:=Byte(Round(R/(SizeX/100)));
  if Prog<>0 then
    begin
      with TMP.Canvas do
        begin
          Brush.Color:=FLightColor1;
          FillRect(Bounds(TopX,TopY,TopX+Prog-2,TopY+SizeY-2));
          if Prog>1 then
            begin
              Brush.Color:=FFillColor1;
              FillRect(Bounds(TopX+1,TopY+1,TopX+Prog-3,TopY+SizeY-3));
              Brush.Color:=FFrameColor1;
              FrameRect(Bounds(TopX+1,TopY+1,TopX+Prog-3,TopY+SizeY-3));
            end;
          Brush.Color:=FDarkColor1;
          FillRect(Bounds(TopX+Prog,TopY,1,TopY+SizeY-1));
          if Prog<SizeX-1 then
            begin
              Brush.Color:=FBackFillColor;
              FillRect(Bounds(TopX+Prog+1,TopY,SizeX-Prog-1,SizeY));
              Brush.Color:=FBackFrameColor1;
              FrameRect(Bounds(TopX+Prog+1,TopY,SizeX-Prog-1,SizeY));
              Brush.Color:=FBackFrameColor2;
              FrameRect(Bounds(TopX+Prog+1,TopY+1,SizeX-Prog-2,SizeY-2));
            end;
        end;
    end;

///////////////////////////////////////////////////////////////////////////////
//              Draw second indicator
///////////////////////////////////////////////////////////////////////////////

  R:=(FProgress2-FMin)/((FMax-FMin)/SizeX);
  Prog:=Round(R);
  FPercent2:=Byte(Round(R/(SizeX/100)));
  if FDouble then
    begin
      if Prog<>0 then
        begin
          with TMP.Canvas do
            begin
              Brush.Color:=FLightColor2;
              FillRect(Bounds(TopX,TopY,TopX+Prog-2,TopY+SizeY-2));
              if Prog>1 then
                begin
                  Brush.Color:=FFillColor2;
                  FillRect(Bounds(TopX+1,TopY+1,TopX+Prog-3,TopY+SizeY-3));
                  Brush.Color:=FFrameColor2;
                  FrameRect(Bounds(TopX+1,TopY+1,TopX+Prog-3,TopY+SizeY-3));
                end;
            end;
        end;
    end;
  try
  Canvas.Lock;
  Canvas.Draw(0,0,TMP);
  Canvas.Unlock;
  except
  end;
  TMP.Free;
end;

procedure TRarProgress.Add1(Delta: cardinal);
begin
  Inc(FProgress1,Delta);
  Draw;
end;

procedure TRarProgress.Add2(Delta: cardinal);
begin
  Inc(FProgress2,Delta);
  Draw;
end;

end.

