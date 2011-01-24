unit u_MarkTemplates;

interface

uses
  GR32,
  i_IMarkPicture,
  i_MarksSimple;

type
  TMarkTemplatePoint = class(TInterfacedObject, IMarkTemplatePoint)
  private
    FCategoryId: Integer;
    FColor1: TColor32;
    FColor2: TColor32;
    FScale1: Integer;
    FScale2: Integer;
    FPicName: string;
    FPic: IMarkPicture;
  protected
    function GetCategoryId: Integer;
    function GetColor1: TColor32;
    function GetColor2: TColor32;
    function GetScale1: Integer;
    function GetScale2: Integer;
    function GetPicName: string;
    function GetPic: IMarkPicture;
  public
    constructor Create(
      ACategoryId: Integer;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer;
      APicName: string;
      APic: IMarkPicture
    );
  end;

  TMarkTemplateLine = class(TInterfacedObject, IMarkTemplateLine)
  private
    FCategoryId: Integer;
    FColor1: TColor32;
    FScale1: Integer;
  protected
    function GetCategoryId: Integer;
    function GetColor1: TColor32;
    function GetScale1: Integer;
  public
    constructor Create(
      ACategoryId: Integer;
      AColor1: TColor32;
      AScale1: Integer
    );
  end;

  TMarkTemplatePoly = class(TInterfacedObject, IMarkTemplatePoly)
  private
    FCategoryId: Integer;
    FColor1: TColor32;
    FColor2: TColor32;
    FScale1: Integer;
  protected
    function GetCategoryId: Integer;
    function GetColor1: TColor32;
    function GetColor2: TColor32;
    function GetScale1: Integer;
  public
    constructor Create(
      ACategoryId: Integer;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer
    );
  end;

implementation

{ TMarkTemplatePoint }

constructor TMarkTemplatePoint.Create(ACategoryId: Integer; AColor1,
  AColor2: TColor32; AScale1, AScale2: Integer; APicName: string;
  APic: IMarkPicture);
begin
  FCategoryId := ACategoryId;
  FColor1 := AColor1;
  FColor2 := AColor2;
  FScale1 := AScale1;
  FScale2 := AScale2;
  FPicName := APicName;
  FPic := APic;
end;

function TMarkTemplatePoint.GetCategoryId: Integer;
begin
  Result := FCategoryId;
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

constructor TMarkTemplateLine.Create(ACategoryId: Integer; AColor1: TColor32;
  AScale1: Integer);
begin
  FCategoryId := ACategoryId;
  FColor1 := AColor1;
  FScale1 := AScale1;
end;

function TMarkTemplateLine.GetCategoryId: Integer;
begin
  Result := FCategoryId;
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

constructor TMarkTemplatePoly.Create(ACategoryId: Integer; AColor1,
  AColor2: TColor32; AScale1: Integer);
begin
  FCategoryId := ACategoryId;
  FColor1 := AColor1;
  FColor2 := AColor2;
  FScale1 := AScale1;
end;

function TMarkTemplatePoly.GetCategoryId: Integer;
begin
  Result := FCategoryId;
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
