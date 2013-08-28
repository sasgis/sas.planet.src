unit i_AppearanceOfMarkFactory;

interface

uses
  GR32,
  i_MarkPicture,
  i_Appearance;

type
  IAppearanceOfMarkFactory = interface
    ['{8AE2CA8B-59EC-43CE-A9FB-D5D081876369}']
    function CreatePointAppearance(
      const ATextColor: TColor32;
      const ATextBgColor: TColor32;
      const AFontSize: Integer;
      const APicName: string;
      const APic: IMarkPicture;
      const AMarkerSize: Integer
    ): IAppearance;
    function CreateLineAppearance(
      const ALineColor: TColor32;
      const ALineWidth: Integer
    ): IAppearance;
    function CreatePolygonAppearance(
      const ALineColor: TColor32;
      const ALineWidth: Integer;
      const AFillColor: TColor32
    ): IAppearance;
  end;
  
implementation

end.
