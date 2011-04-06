unit u_MarkTemplates;

interface

uses
  GR32,
  i_MarkTemplate,
  i_MarkCategory,
  i_MarkPicture,
  i_MarksDbSmlInternal,
  i_MarkCategoryDBSmlInternal,
  i_MarkNameGenerator,
  i_MarksSimple;

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
    FColor1: TColor32;
    FColor2: TColor32;
    FScale1: Integer;
    FScale2: Integer;
    FPicName: string;
    FPic: IMarkPicture;
  protected
    function GetColor1: TColor32;
    function GetColor2: TColor32;
    function GetScale1: Integer;
    function GetScale2: Integer;
    function GetPicName: string;
    function GetPic: IMarkPicture;
    function IsSame(ATemplate: IMarkTemplatePoint): Boolean;
  public
    constructor Create(
      ACategoryDb: IMarkCategoryDBSmlInternal;
      ANameGenerator: IMarkNameGenerator;
      ACategoryId: Integer;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer;
      APicName: string;
      APic: IMarkPicture
    );
  end;

  TMarkTemplateLine = class(FMarkTemplateBase, IMarkTemplateLine)
  private
    FColor1: TColor32;
    FScale1: Integer;
  protected
    function GetColor1: TColor32;
    function GetScale1: Integer;
    function IsSame(ATemplate: IMarkTemplateLine): Boolean;
  public
    constructor Create(
      ACategoryDb: IMarkCategoryDBSmlInternal;
      ANameGenerator: IMarkNameGenerator;
      ACategoryId: Integer;
      AColor1: TColor32;
      AScale1: Integer
    );
  end;

  TMarkTemplatePoly = class(FMarkTemplateBase, IMarkTemplatePoly)
  private
    FColor1: TColor32;
    FColor2: TColor32;
    FScale1: Integer;
  protected
    function GetColor1: TColor32;
    function GetColor2: TColor32;
    function GetScale1: Integer;
    function IsSame(ATemplate: IMarkTemplatePoly): Boolean;
  public
    constructor Create(
      ACategoryDb: IMarkCategoryDBSmlInternal;
      ANameGenerator: IMarkNameGenerator;
      ACategoryId: Integer;
      AColor1: TColor32;
      AColor2: TColor32;
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
  ACategoryId: Integer; AColor1,
  AColor2: TColor32; AScale1, AScale2: Integer; APicName: string;
  APic: IMarkPicture);
begin
  inherited Create(ACategoryDb, ANameGenerator, ACategoryId);
  FColor1 := AColor1;
  FColor2 := AColor2;
  FScale1 := AScale1;
  FScale2 := AScale2;
  FPicName := APicName;
  FPic := APic;
end;

function TMarkTemplatePoint.GetColor1: TColor32;
begin
  Result := FColor1;
end;

function TMarkTemplatePoint.GetColor2: TColor32;
begin
  Result := FColor2;
end;

function TMarkTemplatePoint.GetPic: IMarkPicture;
begin
  Result := FPic;
end;

function TMarkTemplatePoint.GetPicName: string;
begin
  Result := FPicName;
end;

function TMarkTemplatePoint.GetScale1: Integer;
begin
  Result := FScale1;
end;

function TMarkTemplatePoint.GetScale2: Integer;
begin
  Result := FScale2;
end;

function TMarkTemplatePoint.IsSame(ATemplate: IMarkTemplatePoint): Boolean;
begin
  Result := IsSameInternal(ATemplate);
  if Result then begin
    Result :=
      (FColor1 = ATemplate.Color1) and
      (FColor2 = ATemplate.Color2) and
      (FScale1 = ATemplate.Scale1) and
      (FScale2 = ATemplate.Scale2) and
      (FPic = ATemplate.Pic);
  end;
end;

{ TMarkTemplateLine }

constructor TMarkTemplateLine.Create(
  ACategoryDb: IMarkCategoryDBSmlInternal;
  ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer; AColor1: TColor32;
  AScale1: Integer);
begin
  inherited Create(ACategoryDb, ANameGenerator, ACategoryId);
  FColor1 := AColor1;
  FScale1 := AScale1;
end;

function TMarkTemplateLine.GetColor1: TColor32;
begin
  Result := FColor1;
end;

function TMarkTemplateLine.GetScale1: Integer;
begin
  Result := FScale1;
end;

function TMarkTemplateLine.IsSame(ATemplate: IMarkTemplateLine): Boolean;
begin
  Result := IsSameInternal(ATemplate);
  if Result then begin
    Result :=
      (FColor1 = ATemplate.Color1) and
      (FScale1 = ATemplate.Scale1);
  end;
end;

{ TMarkTemplatePoly }

constructor TMarkTemplatePoly.Create(
  ACategoryDb: IMarkCategoryDBSmlInternal;
  ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer; AColor1,
  AColor2: TColor32; AScale1: Integer);
begin
  inherited Create(ACategoryDb, ANameGenerator, ACategoryId);
  FColor1 := AColor1;
  FColor2 := AColor2;
  FScale1 := AScale1;
end;

function TMarkTemplatePoly.GetColor1: TColor32;
begin
  Result := FColor1;
end;

function TMarkTemplatePoly.GetColor2: TColor32;
begin
  Result := FColor2;
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
      (FColor2 = ATemplate.Color2) and
      (FScale1 = ATemplate.Scale1);
  end;
end;

end.
