
{*******************************************************}
{                                                       }
{               MiTeC XP Menu                           }
{           version 1.0 for Delphi 5,6                  }
{                                                       }
{          Copyright © 2002 Michal Mutl                 }
{                                                       }
{*******************************************************}


unit MXPMenu;

interface

uses
  Windows, Graphics, Controls, Classes;

type
  TMXPMenu = class(TComponent)
  private
    FOwner: TComponent;
    FActive: Boolean;
    FSelectedBackColor: TColor;
    FIconBackColor: TColor;
    FSeparatorColor: TColor;
    FBackColor: TColor;
    FSelectedFontColor: TColor;
    FFontColor: TColor;
    FCheckSignColor: TColor;
    FCheckedColor: TColor;
    FDisabledFontColor: TColor;
    procedure SetActive(Value: boolean);
  protected
    procedure DrawMenuItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    procedure MeasureMenuItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HookDrawItemEventHandlers;
  published
    property Active: boolean read FActive Write SetActive;
    property BackColor: TColor read FBackColor Write FBackColor;
    property IconBackColor: TColor read FIconBackColor Write FIconBackColor;
    property SelectedBackColor: TColor read FSelectedBackColor Write FSelectedBackColor;
    property FontColor: TColor read FFontColor Write FFontColor;
    property SelectedFontColor: TColor read FSelectedFontColor Write FSelectedFontColor;
    property DisabledFontColor: TColor read FDisabledFontColor Write FDisabledFontColor;
    property SeparatorColor: TColor read FSeparatorColor Write FSeparatorColor;
    property CheckedColor: TColor read FCheckedColor Write FCheckedColor;
    property CheckedSignColor: TColor read FCheckSignColor Write FCheckSignColor;
  end;

procedure Register;

implementation

uses
  Menus;

procedure Register;
begin
  RegisterComponents('MiTeC', [TMXPMenu]);
end;

constructor TMXPMenu.Create;
begin
  inherited;
  FOwner:=AOwner;
  FBackColor:=$00E1E1E1;
  FIconBackColor:=$00D1D1D1;
  FSelectedBackColor:=$00DCCFC7;
  FFontColor:=clBlack;
  FSelectedFontColor:=clNavy;
  FDisabledFontColor:=clGray;
  FSeparatorColor:=$00D1D1D1;
  FCheckedColor:=clGray;
  FCheckSignColor:=$00A56D39;
  Active:=True;
end;

procedure TMXPMenu.SetActive(Value: boolean);
var
  i,j: integer;
begin
  if (FActive<>Value) then begin
    FActive:=Value;
    with FOwner do begin
      HookDrawItemEventHandlers;
      for i:=0 to ComponentCount-1 do begin
        if (Components[i] is TMenu) then
          TMenu(Components[i]).OwnerDraw:=FActive;
        if (Components[i] is TMainMenu) then begin
          with TMainMenu(Components[i]) do begin
            for j:=0 to Items.Count-1 do begin
              if Items[j].Visible then begin
                Items[j].Visible:=False;
                Items[j].Visible:=True;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TMXPMenu.HookDrawItemEventHandlers;

  procedure Hook(MenuItem: TMenuItem);
  var
    i: integer;
  begin
    if FActive then begin
      MenuItem.OnAdvancedDrawItem:=DrawMenuItem;
      MenuItem.OnMeasureItem:=MeasureMenuItem;
    end else begin
      MenuItem.OnAdvancedDrawItem:=nil;
      MenuItem.OnMeasureItem:=nil;
    end;
    for i:=0 to MenuItem.Count-1 do
      Hook(MenuItem.Items[i]);
  end;

var
  i,j: integer;
  Menu: TMenu;
begin
  with FOwner do begin
    for i:=0 to ComponentCount-1 do begin
      if (Components[i] is TMenu) then begin
        Menu:=TMenu(Components[i]);
        for j:=0 to Menu.Items.Count-1 do
          Hook(Menu.Items[j]);
      end;
    end;
  end;
end;

procedure TMXPMenu.DrawMenuItem;
var
  Text: string;
  Bitmap: TBitmap;
  IconRect,
  TextRect,
  CheckRect: TRect;
  i,x1,x2,TextFormat: integer;
  MenuItem: TMenuItem;
  Menu: TMenu;
begin
  MenuItem:=TMenuItem(Sender);
  Menu:=MenuItem.Parent.GetParentMenu;
  if Menu.IsRightToLeft then begin
    x1:=ARect.Right-23;
    x2:=ARect.Right;
  end else begin
    x1:=ARect.Left;
    x2:=ARect.Left+23;
  end;
  IconRect:=Rect(x1,ARect.Top,x2,ARect.Bottom);
  TextRect:=ARect;
  Text:=#32+MenuItem.Caption;
  Bitmap:=TBitmap.Create;
  Bitmap.Width := 23{ARect.Right-ARect.Left};
  Bitmap.Height := ARect.Bottom-ARect.Top;
  Bitmap.Canvas.Brush.Color := clWhite; // ACanvas.Brush.Color;
  Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
  Bitmap.Transparent:=True;
  Bitmap.TransparentMode:=tmAuto;
  if Assigned(MenuItem.Parent.GetParentMenu.Images) or Assigned(MenuItem.Parent.SubMenuImages) then begin
    if (MenuItem.ImageIndex<>-1) then begin
      if Assigned(MenuItem.Parent.SubMenuImages) then
        MenuItem.Parent.SubMenuImages.GetBitmap(MenuItem.ImageIndex,Bitmap)
      else
        MenuItem.Parent.GetParentMenu.Images.GetBitmap(MenuItem.ImageIndex,Bitmap)
    end;
  end;
  if Menu.IsRightToLeft then begin
    x1:=ARect.Left;
    x2:=ARect.Right-23;
  end else begin
    x1:=ARect.Left+23;
    x2:=ARect.Right;
  end;
  TextRect:=Rect(x1,ARect.Top,x2,ARect.Bottom);
  ACanvas.Brush.Color:=FBackColor;
  ACanvas.FillRect(TextRect);
  if (Menu is TMainMenu) then begin
    for i:=0 to MenuItem.GetParentMenu.Items.Count-1 do begin
      if (MenuItem.GetParentMenu.Items[i]=MenuItem) then begin
        ACanvas.Brush.Color:=FIconBackColor;
        ACanvas.FillRect(ARect);
        if (MenuItem.ImageIndex=-1) and (MenuItem.Bitmap.Width=0) then begin
          TextRect:=ARect;
          Break;
        end;
      end;
    end;
  end;
  ACanvas.Brush.Color:=FIconBackColor;
  ACanvas.FillRect(IconRect);
  if MenuItem.Enabled then
    ACanvas.Font.Color:=FFontColor
  else
    ACanvas.Font.Color:=FDisabledFontColor;
  if (odSelected in State) or (odHotLight in State) then begin
    ACanvas.Brush.Style:=bsSolid;
    ACanvas.Brush.Color:=FSelectedBackColor;
    ACanvas.FillRect(TextRect);
    ACanvas.Pen.Color:=FSelectedFontColor;
    ACanvas.Brush.Style:=bsClear;
    ACanvas.Rectangle(TextRect.Left,TextRect.Top,TextRect.Right,TextRect.Bottom);
    if MenuItem.Enabled then
      ACanvas.Font.Color:=FSelectedFontColor;
  end;
  x1:=IconRect.Left+2;
  if MenuItem.Checked then begin
    ACanvas.Pen.Color:=FCheckedColor;
    ACanvas.Brush.Style:=bsClear;
    {if (Bitmap.Width=0) then begin
      if
      ACanvas.RoundRect(IconRect.Left+5,IconRect.Top+5,IconRect.Right-5,IconRect.Bottom-5,3,3);
      CopyRect(CheckRect,IconRect);
      InflateRect(CheckRect,-7,-7);
      ACanvas.Brush.Color:=FCheckSignColor;
      ACanvas.FillRect(CheckRect);
    end else begin    }
      ACanvas.Brush.Color:=FCheckSignColor;
     if MenuItem.ImageIndex>-1 then ACanvas.RoundRect(IconRect.Left,IconRect.Top,IconRect.Right,IconRect.Bottom,3,3)
                               else with IconRect do
                                     begin
                                      if MenuItem.RadioItem
                                       then ACanvas.Ellipse(Left+(Right-Left)div 2-4,Top+(Bottom-Top) div 2-4,Left+(Right-Left)div 2+4,Top+(Bottom-Top) div 2 +4)
                                       else ACanvas.RoundRect(Left+(Right-Left)div 2-4,Top+(Bottom-Top) div 2-4,Left+(Right-Left)div 2+4,Top+(Bottom-Top) div 2 +4,2,2);
                                     end;
    //end;
  end;
  if Assigned(Bitmap) then
    ACanvas.Draw(x1,IconRect.Top+1,Bitmap);
  if not MenuItem.IsLine then begin
    SetBkMode(ACanvas.Handle,TRANSPARENT);
    ACanvas.Font.Name:='Tahoma';
    ACanvas.Font.Style:=[];
    if Menu.IsRightToLeft then
      ACanvas.Font.Charset:=ARABIC_CHARSET;
    if Menu.IsRightToLeft then
      TextFormat:=DT_RIGHT+DT_RTLREADING
    else
      TextFormat:=0;
    if MenuItem.Default then
      ACanvas.Font.Style:=ACanvas.Font.Style+[fsBold];
    inc(TextRect.Left,2);
    inc(TextRect.Top,3);
    DrawTextEx(ACanvas.Handle,PChar(Text),Length(Text),TextRect,TextFormat,nil);
    Text:=ShortCutToText(MenuItem.ShortCut)+' ';
    if Menu.IsRightToLeft then
      TextFormat:=DT_LEFT
    else
      TextFormat:=DT_RIGHT;
    DrawTextEx(ACanvas.Handle,PChar(Text),Length(Text),TextRect,TextFormat,nil);
  end else begin
    ACanvas.Pen.Color:=FSeparatorColor;
    ACanvas.MoveTo(ARect.Left+26,TextRect.Top+Round((TextRect.Bottom-TextRect.Top)/2));
    ACanvas.LineTo(ARect.Right-2,TextRect.Top+Round((TextRect.Bottom-TextRect.Top)/2));
  end;
  Bitmap.Free;
end;

destructor TMXPMenu.Destroy;
begin
  Active:=False;
  inherited;
end;

procedure TMXPMenu.MeasureMenuItem(Sender: TObject; ACanvas: TCanvas;
  var Width, Height: Integer);
begin
end;

end.

