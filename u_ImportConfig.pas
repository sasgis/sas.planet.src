unit u_ImportConfig;

interface

uses
  i_MarkTemplate,
  i_ImportConfig,
  u_MarksDb;

type
  TImportConfig = class(TInterfacedObject, IImportConfig)
  private
    FMarkDB: TMarksDb;
    FTemplateNewPoint: IMarkTemplatePoint;
    FTemplateNewLine: IMarkTemplateLine;
    FTemplateNewPoly: IMarkTemplatePoly;
  protected
    function GetMarkDB: TMarksDb;
    function GetTemplateNewPoint: IMarkTemplatePoint;
    function GetTemplateNewLine: IMarkTemplateLine;
    function GetTemplateNewPoly: IMarkTemplatePoly;
  public
    constructor Create(
      AMarkDB: TMarksDb;
      ATemplateNewPoint: IMarkTemplatePoint;
      ATemplateNewLine: IMarkTemplateLine;
      ATemplateNewPoly: IMarkTemplatePoly
    );
  end;

implementation

{ TImportConfig }

constructor TImportConfig.Create(
  AMarkDB: TMarksDb;
  ATemplateNewPoint: IMarkTemplatePoint;
  ATemplateNewLine: IMarkTemplateLine;
  ATemplateNewPoly: IMarkTemplatePoly
);
begin
  FMarkDB := AMarkDB;
  FTemplateNewPoint := ATemplateNewPoint;
  FTemplateNewLine := ATemplateNewLine;
  FTemplateNewPoly := ATemplateNewPoly;
end;

function TImportConfig.GetMarkDB: TMarksDb;
begin
  Result := FMarkDB;
end;

function TImportConfig.GetTemplateNewLine: IMarkTemplateLine;
begin
  Result := FTemplateNewLine;
end;

function TImportConfig.GetTemplateNewPoint: IMarkTemplatePoint;
begin
  Result := FTemplateNewPoint;
end;

function TImportConfig.GetTemplateNewPoly: IMarkTemplatePoly;
begin
  Result := FTemplateNewPoly;
end;

end.
