unit u_MarkTemplates;

interface

uses
  GR32,
  i_IMarkPicture,
  i_IMarkNameGenerator,
  i_MarksSimple;

type
  FMarkTemplateBase = class(TInterfacedObject)
  private
    FNameGenerator: IMarkNameGenerator;
    FCategoryId: Integer;
  protected
    function GetNewName: string;
    function GetCategoryId: Integer;
  public
    constructor Create(
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
  public
    constructor Create(
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
  public
    constructor Create(
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
  public
    constructor Create(
      ANameGenerator: IMarkNameGenerator;
      ACategoryId: Integer;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer
    );
  end;

implementation

{ FMarkTemplateBase }

constructor FMarkTemplateBase.Create(ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer);
begin
  inherited Create;
  FNameGenerator := ANameGenerator;
  FCategoryId := ACategoryId;
end;

function FMarkTemplateBase.GetCategoryId: Integer;
begin
  Result := FCategoryId;
end;

function FMarkTemplateBase.GetNewName: string;
begin
  Result := FNameGenerator.GetNewName;
end;

{ TMarkTemplatePoint }

constructor TMarkTemplatePoint.Create(
  ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer; AColor1,
  AColor2: TColor32; AScale1, AScale2: Integer; APicName: string;
  APic: IMarkPicture);
begin
  inherited Create(ANameGenerator, ACategoryId);
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

{ TMarkTemplateLine }

constructor TMarkTemplateLine.Create(
  ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer; AColor1: TColor32;
  AScale1: Integer);
begin
  inherited Create(ANameGenerator, ACategoryId);
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

{ TMarkTemplatePoly }

constructor TMarkTemplatePoly.Create(
  ANameGenerator: IMarkNameGenerator;
  ACategoryId: Integer; AColor1,
  AColor2: TColor32; AScale1: Integer);
begin
  inherited Create(ANameGenerator, ACategoryId);
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

end.
