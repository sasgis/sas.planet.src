{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit i_SunCalcConfig;

interface

uses
  t_SunCalcConfig,
  i_ConfigDataElement;

type
  TSunCalcDataProviderType = (scdpSun, scdpMoon);

  ISunCalcColorSchemaStatic = interface
    ['{59740592-7425-4E8C-BE19-DF7121E1ACC0}']
    function GetSchemaName: string;
    property SchemaName: string read GetSchemaName;

    function GetTimeLineColors: TSunCalcTimeLineColors;
    property TimeLineColors: TSunCalcTimeLineColors read GetTimeLineColors;

    function GetTimeLineFont: TSunCalcFontInfo;
    property TimeLineFont: TSunCalcFontInfo read GetTimeLineFont;

    function GetTimeLineHintFont: TSunCalcFontInfo;
    property TimeLineHintFont: TSunCalcFontInfo read GetTimeLineHintFont;

    function GetDetailsPanelFont: TSunCalcFontInfo;
    property DetailsPanelFont: TSunCalcFontInfo read GetDetailsPanelFont;

    function GetDetailsPanelColors: TSunCalcDetailsPanelColors;
    property DetailsPanelColors: TSunCalcDetailsPanelColors read GetDetailsPanelColors;

    function GetShapesColors: TSunCalcShapesColors;
    property ShapesColors: TSunCalcShapesColors read GetShapesColors;
  end;

  ISunCalcColorSchema = interface(IConfigDataElement)
    ['{5BD5A3AC-1475-4A1D-8263-F62E5C9CB0EA}']
    function GetSchemaName: string;
    procedure SetSchemaName(const AValue: string);
    property SchemaName: string read GetSchemaName write SetSchemaName;

    function GetTimeLineColors: TSunCalcTimeLineColors;
    procedure SetTimeLineColors(const AValue: TSunCalcTimeLineColors);
    property TimeLineColors: TSunCalcTimeLineColors read GetTimeLineColors write SetTimeLineColors;

    function GetTimeLineFont: TSunCalcFontInfo;
    procedure SetTimeLineFont(const AValue: TSunCalcFontInfo);
    property TimeLineFont: TSunCalcFontInfo read GetTimeLineFont write SetTimeLineFont;

    function GetTimeLineHintFont: TSunCalcFontInfo;
    procedure SetTimeLineHintFont(const AValue: TSunCalcFontInfo);
    property TimeLineHintFont: TSunCalcFontInfo read GetTimeLineHintFont write SetTimeLineHintFont;

    function GetDetailsPanelFont: TSunCalcFontInfo;
    procedure SetDetailsPanelFont(const AValue: TSunCalcFontInfo);
    property DetailsPanelFont: TSunCalcFontInfo read GetDetailsPanelFont write SetDetailsPanelFont;

    function GetDetailsPanelColors: TSunCalcDetailsPanelColors;
    procedure SetDetailsPanelColors(const AValue: TSunCalcDetailsPanelColors);
    property DetailsPanelColors: TSunCalcDetailsPanelColors read GetDetailsPanelColors write SetDetailsPanelColors;

    function GetShapesColors: TSunCalcShapesColors;
    procedure SetShapesColors(const AValue: TSunCalcShapesColors);
    property ShapesColors: TSunCalcShapesColors read GetShapesColors write SetShapesColors;

    function GetStatic: ISunCalcColorSchemaStatic;
  end;

  ISunCalcColorSchemaList = interface(IConfigDataElement)
    ['{8A6DD225-4AB0-4DA1-AD0D-52CDAA452E99}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetActiveSchemaIndex: Integer;
    procedure SetActiveSchemaIndex(const AValue: Integer);
    property ActiveSchemaIndex: Integer read GetActiveSchemaIndex write SetActiveSchemaIndex;

    function GetColorSchemaByIndex(const AIndex: Integer): ISunCalcColorSchema;

    function GetActiveColorSchema: ISunCalcColorSchema;
  end;

  ISunCalcConfig = interface(IConfigDataElement)
    ['{22F184A5-68D5-44BB-B98A-A5C760BAEBF7}']
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetCircleRadius: Integer;
    procedure SetCircleRadius(const AValue: Integer);
    property CircleRadius: Integer read GetCircleRadius write SetCircleRadius;

    function GetYearTimeLineHight: Integer;
    procedure SetYearTimeLineHight(const AValue: Integer);
    property YearTimeLineHight: Integer read GetYearTimeLineHight write SetYearTimeLineHight;

    function GetDayTimeLineHight: Integer;
    procedure SetDayTimeLineHight(const AValue: Integer);
    property DayTimeLineHight: Integer read GetDayTimeLineHight write SetDayTimeLineHight;

    function GetDetailsPanelRowHight: Integer;
    procedure SetDetailsPanelRowHight(const AValue: Integer);
    property DetailsPanelRowHight: Integer read GetDetailsPanelRowHight write SetDetailsPanelRowHight;

    function GetDetailsPanelColsWidth: TSunCalcDetailsPanelColsWidth;
    procedure SetDetailsPanelColsWidth(const AValue: TSunCalcDetailsPanelColsWidth);
    property DetailsPanelColsWidth: TSunCalcDetailsPanelColsWidth read GetDetailsPanelColsWidth write SetDetailsPanelColsWidth;

    function GetIsDetailedView: Boolean;
    procedure SetIsDetailedView(const AValue: Boolean);
    property IsDetailedView: Boolean read GetIsDetailedView write SetIsDetailedView;

    function GetShowCaptionNearSun: Boolean;
    procedure SetShowCaptionNearSun(const AValue: Boolean);
    property ShowCaptionNearSun: Boolean read GetShowCaptionNearSun write SetShowCaptionNearSun;

    function GetShowDayInfoPanel: Boolean;
    procedure SetShowDayInfoPanel(const AValue: Boolean);
    property ShowDayInfoPanel: Boolean read GetShowDayInfoPanel write SetShowDayInfoPanel;

    function GetIsRealTime: Boolean;
    procedure SetIsRealTime(const AValue: Boolean);
    property IsRealTime: Boolean read GetIsRealTime write SetIsRealTime;

    function GetDataProviderType: TSunCalcDataProviderType;
    procedure SetDataProviderType(const AValue: TSunCalcDataProviderType);
    property DataProviderType: TSunCalcDataProviderType read GetDataProviderType write SetDataProviderType;

    function GetColorSchemaList: ISunCalcColorSchemaList;
    property ColorSchemaList: ISunCalcColorSchemaList read GetColorSchemaList;
  end;

implementation

end.
