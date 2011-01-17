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
    FMarkPictureList: IMarkPictureList;
    FTemplateNewPoint: IMarkFull;
    FTemplateNewLine: IMarkFull;
    FTemplateNewPoly: IMarkFull;
    function GetLLRectFromPoints(APoints: TDoublePointArray): TDoubleRect;
    function GetLLRectFromPoint(APoint: TDoublePoint): TDoubleRect;
    function GetArrayFromPoint(APoint: TDoublePoint): TDoublePointArray;
    procedure ClosePolyPoints(APoints: TDoublePointArray);
  public
    function CreateNewPoint(
      APoint: TDoublePoint;
      AName: string;
      ADesc: string;
      ATemplate:IMarkFull = nil
    ): IMarkFull;
    function CreateNewLine(
      APoints: TDoublePointArray;
      AName: string;
      ADesc: string;
      ATemplate:IMarkFull = nil
    ): IMarkFull;
    function CreateNewPoly(
      APoints: TDoublePointArray;
      AName: string;
      ADesc: string;
      ATemplate:IMarkFull = nil
    ): IMarkFull;

    function CreatePointTemplate(
      AVisible: Boolean;
      APicName: string;
      APic: IMarkPicture;
      ACategoryId: Integer;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ): IMarkFull; overload;
    function CreatePointTemplate(
      ASource: IMarkFull
    ): IMarkFull; overload;

    function CreateLineTemplate(
      AVisible: Boolean;
      ACategoryId: Integer;
      AColor1: TColor32;
      AScale1: Integer
    ): IMarkFull; overload;
    function CreateLineTemplate(
      ASource: IMarkFull
    ): IMarkFull; overload;

    function CreatePolyTemplate(
      AVisible: Boolean;
      ACategoryId: Integer;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer
    ): IMarkFull; overload;
    function CreatePolyTemplate(
      ASource: IMarkFull
    ): IMarkFull; overload;

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
      AScale2: Integer;
      ASource: IMarkFull
    ): IMarkFull;
    function CreateLine(
      AName: string;
      AVisible: Boolean;
      ACategoryId: Integer;
      ADesc: string;
      APoints: TDoublePointArray;
      AColor1: TColor32;
      AScale1: Integer;
      ASource: IMarkFull
    ): IMarkFull;
    function CreatePoly(
      AName: string;
      AVisible: Boolean;
      ACategoryId: Integer;
      ADesc: string;
      APoints: TDoublePointArray;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      ASource: IMarkFull
    ): IMarkFull;

    function CreateModifedLine(
      APoints: TDoublePointArray;
      ADesc: string;
      ASource: IMarkFull
    ): IMarkFull;
    function CreateModifedPoly(
      APoints: TDoublePointArray;
      ASource: IMarkFull
    ): IMarkFull;


    property TemplateNewPoint: IMarkFull read FTemplateNewPoint write FTemplateNewPoint;
    property TemplateNewLine: IMarkFull read FTemplateNewLine write FTemplateNewLine;
    property TemplateNewPoly: IMarkFull read FTemplateNewPoly write FTemplateNewPoly;

  public
    constructor Create(AMarkPictureList: IMarkPictureList);
  end;

implementation

uses
  Graphics,
  UResStrings,
  Ugeofun,
  u_MarksSimpleNew;

{ TMarkFactory }

constructor TMarkFactory.Create(AMarkPictureList: IMarkPictureList);
var
  VPicName: string;
  VPic: IMarkPicture;
begin
  FMarkPictureList := AMarkPictureList;

  if FMarkPictureList.Count > 0 then begin
    VPicName := FMarkPictureList.GetName(0);
    VPic := FMarkPictureList.Get(0);
  end else begin
    VPicName := '';
    VPic := nil;
  end;
  FTemplateNewPoint := CreatePointTemplate(
    True,
    VPicName,
    VPic,
    -1,
    SetAlpha(Color32(clYellow), 166),
    SetAlpha(Color32(clBlack), 166),
    11,
    32
  );
  FTemplateNewLine := CreateLineTemplate(
    True,
    -1,
    SetAlpha(Color32(clRed), 166),
    2
  );
  FTemplateNewPoly := CreatePolyTemplate(
    True,
    -1,
    SetAlpha(Color32(clBlack), 166),
    SetAlpha(Color32(clWhite), 51),
    2
  );
end;

function TMarkFactory.CreateLineTemplate(ASource: IMarkFull): IMarkFull;
var
  VVisible: Boolean;
  VMarkVisible: IMarkVisible;
begin
  VVisible := True;
  if ASource.QueryInterface(IMarkVisible, VMarkVisible) = S_OK then begin
    VVisible := VMarkVisible.Visible;
  end;
  Result := CreateLineTemplate(
    VVisible,
    ASource.CategoryId,
    ASource.Color1,
    ASource.Scale1
  );
end;

function TMarkFactory.CreateLineTemplate(AVisible: Boolean;
  ACategoryId: Integer; AColor1: TColor32; AScale1: Integer): IMarkFull;
begin
  Result := TMarkFull.Create(
    '',
    -1,
    AVisible,
    '',
    nil,
    ACategoryId,
    '',
    DoubleRect(0, 0, 0, 0),
    nil,
    AColor1,
    clBlack32,
    AScale1,
    0
  );
end;

function TMarkFactory.CreatePointTemplate(ASource: IMarkFull): IMarkFull;
var
  VVisible: Boolean;
  VMarkVisible: IMarkVisible;
begin
  VVisible := True;
  if ASource.QueryInterface(IMarkVisible, VMarkVisible) = S_OK then begin
    VVisible := VMarkVisible.Visible;
  end;
  Result := CreatePointTemplate(
    VVisible,
    ASource.PicName,
    ASource.Pic,
    ASource.CategoryId,
    ASource.Color1,
    ASource.Color2,
    ASource.Scale1,
    ASource.Scale2
  );
end;

function TMarkFactory.CreatePointTemplate(AVisible: Boolean; APicName: string;
  APic: IMarkPicture; ACategoryId: Integer; AColor1, AColor2: TColor32; AScale1,
  AScale2: Integer): IMarkFull;
begin
  Result := TMarkFull.Create(
    '',
    -1,
    AVisible,
    APicName,
    APic,
    ACategoryId,
    '',
    DoubleRect(0, 0, 0, 0),
    nil,
    AColor1,
    AColor2,
    AScale1,
    AScale2
  );
end;

function TMarkFactory.CreatePolyTemplate(ASource: IMarkFull): IMarkFull;
var
  VVisible: Boolean;
  VMarkVisible: IMarkVisible;
begin
  VVisible := True;
  if ASource.QueryInterface(IMarkVisible, VMarkVisible) = S_OK then begin
    VVisible := VMarkVisible.Visible;
  end;
  Result := CreatePolyTemplate(
    VVisible,
    ASource.CategoryId,
    ASource.Color1,
    ASource.Color2,
    ASource.Scale1
  );
end;

function TMarkFactory.CreatePolyTemplate(AVisible: Boolean;
  ACategoryId: Integer; AColor1, AColor2: TColor32;
  AScale1: Integer): IMarkFull;
begin
  Result := TMarkFull.Create(
    '',
    -1,
    AVisible,
    '',
    nil,
    ACategoryId,
    '',
    DoubleRect(0, 0, 0, 0),
    nil,
    AColor1,
    AColor2,
    AScale1,
    0
  );
end;

function TMarkFactory.CreateNewLine(APoints: TDoublePointArray; AName,
  ADesc: string; ATemplate: IMarkFull): IMarkFull;
var
  VTemplate: IMarkFull;
  VVisible: Boolean;
  VMarkVisible: IMarkVisible;
  VName: string;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FTemplateNewLine;
  end;

  VVisible := True;
  if VTemplate.QueryInterface(IMarkVisible, VMarkVisible) = S_OK then begin
    VVisible := VMarkVisible.Visible;
  end;

  VName := AName;
  if VName = '' then begin
    VName := SAS_STR_NewPath;
  end;

  Result := CreateLine(
    VName,
    VVisible,
    VTemplate.CategoryId,
    ADesc,
    APoints,
    VTemplate.Color1,
    VTemplate.Scale1,
    nil
  );
end;

function TMarkFactory.CreateNewPoint(APoint: TDoublePoint; AName, ADesc: string;
  ATemplate: IMarkFull): IMarkFull;
var
  VTemplate: IMarkFull;
  VVisible: Boolean;
  VMarkVisible: IMarkVisible;
  VName: string;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FTemplateNewPoint;
  end;

  VVisible := True;
  if VTemplate.QueryInterface(IMarkVisible, VMarkVisible) = S_OK then begin
    VVisible := VMarkVisible.Visible;
  end;

  VName := AName;
  if VName = '' then begin
    VName := SAS_STR_NewMark;
  end;

  Result := CreatePoint(
    VName,
    VVisible,
    VTemplate.PicName,
    VTemplate.Pic,
    VTemplate.CategoryId,
    ADesc,
    APoint,
    VTemplate.Color1,
    VTemplate.Color2,
    VTemplate.Scale1,
    VTemplate.Scale2,
    nil
  );
end;

function TMarkFactory.CreateNewPoly(APoints: TDoublePointArray; AName,
  ADesc: string; ATemplate: IMarkFull): IMarkFull;
var
  VTemplate: IMarkFull;
  VVisible: Boolean;
  VMarkVisible: IMarkVisible;
  VName: string;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FTemplateNewPoly;
  end;

  VVisible := True;
  if VTemplate.QueryInterface(IMarkVisible, VMarkVisible) = S_OK then begin
    VVisible := VMarkVisible.Visible;
  end;

  VName := AName;
  if VName = '' then begin
    VName := SAS_STR_NewPoly;
  end;

  Result := CreatePoly(
    VName,
    VVisible,
    VTemplate.CategoryId,
    ADesc,
    APoints,
    VTemplate.Color1,
    VTemplate.Color2,
    VTemplate.Scale1,
    nil
  );
end;

function TMarkFactory.CreateModifedLine(APoints: TDoublePointArray;
  ADesc: string; ASource: IMarkFull): IMarkFull;
var
  VDesc: string;
  VVisible: Boolean;
  VMarkVisible: IMarkVisible;
begin
  VVisible := True;
  if ASource.QueryInterface(IMarkVisible, VMarkVisible) = S_OK then begin
    VVisible := VMarkVisible.Visible;
  end;
  VDesc := ADesc;
  if ADesc = '' then begin
    VDesc := ASource.Desc;
  end;
  Result := CreateLine(
    ASource.Name,
    VVisible,
    ASource.CategoryId,
    VDesc,
    APoints,
    ASource.Color1,
    ASource.Scale1,
    ASource
  );
end;

function TMarkFactory.CreateModifedPoly(APoints: TDoublePointArray;
  ASource: IMarkFull): IMarkFull;
var
  VVisible: Boolean;
  VMarkVisible: IMarkVisible;
begin
  VVisible := True;
  if ASource.QueryInterface(IMarkVisible, VMarkVisible) = S_OK then begin
    VVisible := VMarkVisible.Visible;
  end;
  Result := CreatePoly(
    ASource.Name,
    VVisible,
    ASource.CategoryId,
    ASource.Desc,
    APoints,
    ASource.Color1,
    ASource.Color2,
    ASource.Scale1,
    ASource
  );
end;

function TMarkFactory.CreateLine(
  AName: string;
  AVisible: Boolean;
  ACategoryId: Integer;
  ADesc: string;
  APoints: TDoublePointArray;
  AColor1: TColor32;
  AScale1: Integer;
  ASource: IMarkFull
): IMarkFull;
var
  VID: Integer;
  VPoints: TDoublePointArray;
begin
  if ASource <> nil then begin
    VID := ASource.Id;
  end else begin
    VID := -1;
  end;
  VPoints := Copy(APoints);
  Result := TMarkFull.Create(
    AName,
    VId,
    AVisible,
    '',
    nil,
    ACategoryId,
    ADesc,
    GetLLRectFromPoints(APoints),
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
  AColor1: TColor32;
  AColor2: TColor32;
  AScale1: Integer;
  AScale2: Integer;
  ASource: IMarkFull
): IMarkFull;
var
  VID: Integer;
begin
  if ASource <> nil then begin
    VID := ASource.Id;
  end else begin
    VID := -1;
  end;
  Result := TMarkFull.Create(
    AName,
    VID,
    AVisible,
    APicName,
    APic,
    ACategoryId,
    ADesc,
    GetLLRectFromPoint(APoint),
    GetArrayFromPoint(APoint),
    AColor1,
    AColor2,
    AScale1,
    AScale2
  );
end;

function TMarkFactory.CreatePoly(
  AName: string;
  AVisible: Boolean;
  ACategoryId: Integer;
  ADesc: string;
  APoints: TDoublePointArray;
  AColor1: TColor32;
  AColor2: TColor32;
  AScale1: Integer;
  ASource: IMarkFull
): IMarkFull;
var
  VID: Integer;
  VPoints: TDoublePointArray;
begin
  if ASource <> nil then begin
    VID := ASource.Id;
  end else begin
    VID := -1;
  end;
  VPoints := Copy(APoints);
  ClosePolyPoints(VPoints);
  Result := TMarkFull.Create(
    AName,
    VID,
    AVisible,
    '',
    nil,
    ACategoryId,
    ADesc,
    GetLLRectFromPoints(APoints),
    VPoints,
    AColor1,
    AColor2,
    AScale1,
    0
  );
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

procedure TMarkFactory.ClosePolyPoints(APoints: TDoublePointArray);
var
  VCount: Integer;
begin
  VCount := Length(APoints);
  Assert(VCount > 2, 'В полигоне должно быть хотя бы 3 точки');
  if VCount > 0 then begin
    if not compare2EP(APoints[0], APoints[VCount - 1]) then begin
      SetLength(APoints, VCount + 1);
      APoints[VCount] := APoints[0];
    end;
  end;
end;

end.
