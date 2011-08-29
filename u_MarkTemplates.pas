unit u_MarkTemplates;

interface

uses
  GR32,
  i_MarkTemplate,
  i_MarkCategory,
  i_MarkPicture,
  i_MarksDbSmlInternal,
  i_MarkCategoryDBSmlInternal,
  i_MarkNameGenerator;

type
  FMarkTemplateBase = class(TInterfacedObject, IMarkTemplate, IMarkTemplateSMLInternal)
  private
    FCategoryDb: IMarkCategoryDBSmlInternal;
    FNameGenerator: IMarkNameGenerator;
    FCategoryId: Integer;
    function IsSameInternal(ATemplate: IMarkTemplate): Boolean;
  protected
    function GetNewName: string;
    function GetCategory: IMarkCategory;
    function GetCategoryId: Integer;
  public
    constructor Create(
      ACategoryDb: IMarkCategoryDBSmlInternal;
      ANameGenerator: IMarkNameGenerator;
      ACategoryId: Integer
    );
  end;

  TMarkTemplatePoint = class(FMarkTemplateBase, IMarkTemplatePoint)
  private
    FTextColor: TColor32;
    FTextBgColor: TColor32;
    FFontSize: Integer;
    FMarkerSize: Integer;
    FPic: IMarkPicture;
  protected
    function GetTextColor: TColor32;
    function GetTextBgColor: TColor32;
    function GetFontSize: Integer;
    function GetMarkerSize: Integer;
    function GetPic: IMarkPicture;
    function IsSame(ATemplate: IMarkTemplatePoint): Boolean;
  public
    constructor Create(
      ACategoryDb: IMarkCategoryDBSmlInternal;
      ANameGenerator: IMarkNameGenerator;
      ACategoryId: Integer;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      AFontSize: Integer;
      AMarkerSize: Integer;
      APic: IMarkPicture
    );
  end;

  TMarkTemplateLine = class(FMarkTemplateBase, IMarkTemplateLine)
  private
    FColor: TColor32;
    FLineWeight: Integer;
  protected
    function GetColor: TColor32;
    function GetLineWeight: Integer;
    function IsSame(ATemplate: IMarkTemplateLine): Boolean;
  public
    constructor Create(
      ACategoryDb: IMarkCategoryDBSmlInternal;
      ANameGenerator: IMarkNameGenerator;
      ACategoryId: Integer;
      AColor: TColor32;
      ALineWeight: Integer
    );
  end;

  TMarkTemplatePoly = class(FMarkTemplateBase, IMarkTemplatePoly)
  private
    FColor1: TColor32;
    FFillColor: TColor32;
    FScale1: Integer;
  protected
    function GetColor1: TColor32;
    function GetFillColor: TColor32;
    function GetScale1: Integer;
    function IsSame(ATemplate: IMarkTemplatePoly): Boolean;
  public
    constructor Create(
      ACategoryDb: IMarkCategoryDBSmlInternal;
      ANameGenerator: IMarkNameGenerator;
      ACategoryId: Integer;
      AColor1: TColor32;
      AFillColor: TColor32;
      AScale1: Integer
    );
  end;

implementation

uses
  SysUtils;

{ FMarkTemplateBase }

constructor FMarkTemplateBase.Create(
  ACategoryDb: IMarkCategoryDBSmlInternal;
  ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer
);
begin
  inherited Create;
  FNameGenerator := ANameGenerator;
  FCategoryDb := ACategoryDb;
  FCategoryId := ACategoryId;
end;

function FMarkTemplateBase.GetCategory: IMarkCategory;
begin
  Result := FCategoryDb.GetCategoryByID(FCategoryId);
end;

function FMarkTemplateBase.GetCategoryId: Integer;
begin
  Result := FCategoryId;
end;

function FMarkTemplateBase.GetNewName: string;
begin
  Result := FNameGenerator.GetNewName;
end;

function FMarkTemplateBase.IsSameInternal(
  ATemplate: IMarkTemplate): Boolean;
var
  VTemplateInternal: IMarkTemplateSMLInternal;
begin
  Result := False;
  if ATemplate <> nil then begin
    if Supports(ATemplate, IMarkTemplateSMLInternal, VTemplateInternal) then begin
      Result := VTemplateInternal.CategoryId = FCategoryId;
    end;
  end;
end;

{ TMarkTemplatePoint }

constructor TMarkTemplatePoint.Create(
  ACategoryDb: IMarkCategoryDBSmlInternal;
  ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer;
  ATextColor, ATextBgColor: TColor32;
  AFontSize, AMarkerSize: Integer;
  APic: IMarkPicture
);
begin
  inherited Create(ACategoryDb, ANameGenerator, ACategoryId);
  FTextColor := ATextColor;
  FTextBgColor := ATextBgColor;
  FFontSize := AFontSize;
  FMarkerSize := AMarkerSize;
  FPic := APic;
end;

function TMarkTemplatePoint.GetTextColor: TColor32;
begin
  Result := FTextColor;
end;

function TMarkTemplatePoint.GetTextBgColor: TColor32;
begin
  Result := FTextBgColor;
end;

function TMarkTemplatePoint.GetPic: IMarkPicture;
begin
  Result := FPic;
end;

function TMarkTemplatePoint.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TMarkTemplatePoint.GetMarkerSize: Integer;
begin
  Result := FMarkerSize;
end;

function TMarkTemplatePoint.IsSame(ATemplate: IMarkTemplatePoint): Boolean;
begin
  Result := IsSameInternal(ATemplate);
  if Result then begin
    Result :=
      (FTextColor = ATemplate.TextColor) and
      (FTextBgColor = ATemplate.TextBgColor) and
      (FFontSize = ATemplate.FontSize) and
      (FMarkerSize = ATemplate.MarkerSize) and
      (FPic = ATemplate.Pic);
  end;
end;

{ TMarkTemplateLine }

constructor TMarkTemplateLine.Create(
  ACategoryDb: IMarkCategoryDBSmlInternal;
  ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer;
  AColor: TColor32;
  ALineWeight: Integer);
begin
  inherited Create(ACategoryDb, ANameGenerator, ACategoryId);
  FColor := AColor;
  FLineWeight := ALineWeight;
end;

function TMarkTemplateLine.GetColor: TColor32;
begin
  Result := FColor;
end;

function TMarkTemplateLine.GetLineWeight: Integer;
begin
  Result := FLineWeight;
end;

function TMarkTemplateLine.IsSame(ATemplate: IMarkTemplateLine): Boolean;
begin
  Result := IsSameInternal(ATemplate);
  if Result then begin
    Result :=
      (FColor = ATemplate.Color) and
      (FLineWeight = ATemplate.LineWeight);
  end;
end;

{ TMarkTemplatePoly }

constructor TMarkTemplatePoly.Create(
  ACategoryDb: IMarkCategoryDBSmlInternal;
  ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer; AColor1,
  AFillColor: TColor32; AScale1: Integer);
begin
  inherited Create(ACategoryDb, ANameGenerator, ACategoryId);
  FColor1 := AColor1;
  FFillColor := AFillColor;
  FScale1 := AScale1;
end;

function TMarkTemplatePoly.GetColor1: TColor32;
begin
  Result := FColor1;
end;

function TMarkTemplatePoly.GetFillColor: TColor32;
begin
  Result := FFillColor;
end;

function TMarkTemplatePoly.GetScale1: Integer;
begin
  Result := FScale1;
end;

function TMarkTemplatePoly.IsSame(ATemplate: IMarkTemplatePoly): Boolean;
begin
  Result := IsSameInternal(ATemplate);
  if Result then begin
    Result :=
      (FColor1 = ATemplate.Color1) and
      (FFillColor = ATemplate.FillColor) and
      (FScale1 = ATemplate.Scale1);
  end;
end;

end.
