unit UKMLExplorer;

interface                    

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, CheckLst, Buttons, Ugeofun, UResStrings;
                 
type
  TFKMLExplorer = class(TForm)
    OpenDialog: TOpenDialog;
    GroupBox1: TGroupBox;
    ObjListBox: TCheckListBox;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    GroupBox2: TGroupBox;
    FileListBox: TCheckListBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure FileListBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ObjListBoxClickCheck(Sender: TObject);
    procedure FileListBoxClickCheck(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure ObjListBoxDblClick(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FKMLExplorer: TFKMLExplorer;
  function GetfullKMLPath:string;

implementation
uses UKMLMap, unit1, USaveas;
{$R *.dfm}

function GetfullKMLPath:string;
begin
 if (copy(KML_Path,2,2)<>':\')and(copy(KML_Path,1,2)<>'\\')
  then result:=ExtractFilePath(ParamStr(0))+KML_Path
  else result:=KML_Path
end;

procedure TFKMLExplorer.FormShow(Sender: TObject);
var i:integer;
begin
 FileListBox.Items.Clear;
 For i:=0 to length(KMLFile)-1 do
  begin
    FileListBox.AddItem(KMLFile[i].FileName,KMLFile[i]);
    FileListBox.Checked[i]:=KMLFile[i].visible;
  end;
 ShowKML;
end;

procedure TFKMLExplorer.FileListBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i:integer;
begin
 if (Button=mbLeft)and(FileListBox.ItemIndex>=0) then
  begin
   ObjListBox.Items.Clear;
   For i:=0 to length(KMLFile[FileListBox.ItemIndex].KMLLayer)-1 do
    begin
      ObjListBox.AddItem(KMLFile[FileListBox.ItemIndex].KMLLayer[i].name_blok,KMLFile[FileListBox.ItemIndex].KMLLayer[i]);
      ObjListBox.Checked[i]:=KMLFile[FileListBox.ItemIndex].KMLLayer[i].visiblelayer;
    end;
  end;
end;

procedure TFKMLExplorer.ObjListBoxClickCheck(Sender: TObject);
begin
 TKMLLayer(ObjListBox.items.Objects[ObjListBox.ItemIndex]).visiblelayer:=ObjListBox.Checked[ObjListBox.ItemIndex];
 TKMLLayer(ObjListBox.items.Objects[ObjListBox.ItemIndex]).SaveToIni;
 Fmain.generate_im(nilLastLoad,'');
end;

procedure TFKMLExplorer.FileListBoxClickCheck(Sender: TObject);
begin
 TKMLFILE(FileListBox.items.Objects[FileListBox.ItemIndex]).visible:=FileListBox.Checked[FileListBox.ItemIndex];
 TKMLFILE(FileListBox.items.Objects[FileListBox.ItemIndex]).SaveToIni;
 Fmain.generate_im(nilLastLoad,'');
end;

procedure TFKMLExplorer.SpeedButton1Click(Sender: TObject);
begin
 if (OpenDialog.Execute)and(OpenDialog.FileName<>'') then
  begin
   CopyFile(PChar(OpenDialog.FileName),PChar(GetfullKMLPath+ExtractFileName(OpenDialog.FileName)),true);
   loadKML;
   Fmain.generate_im(nilLastLoad,'');
   FormShow(Fmain);
  end;
end;

procedure TFKMLExplorer.SpeedButton2Click(Sender: TObject);
begin
 if (FileListBox.ItemIndex>=0) then
  begin
   DeleteFile(PChar(GetfullKMLPath+TKMLFILE(FileListBox.items.Objects[FileListBox.ItemIndex]).FileName));
   loadKML;
   Fmain.generate_im(nilLastLoad,'');
   FormShow(Fmain);
  end;
end;

procedure TFKMLExplorer.ObjListBoxDblClick(Sender: TObject);
begin
 if (ObjListBox.ItemIndex>=0)
  then With TKMLLayer(ObjListBox.items.Objects[ObjListBox.ItemIndex]) do
       Fmain.topos(coordinatesLT.Y+(coordinatesRD.Y-coordinatesLT.Y)/2,
                   coordinatesLT.X+(coordinatesRD.X-coordinatesLT.X)/2,Zoom_size,true)
  else ShowMessage(SAS_MSG_needselect);
end;

procedure TFKMLExplorer.SpeedButton4Click(Sender: TObject);
var Reg_arr:array of TExtendedPoint;
    i:integer;
begin
 if (ObjListBox.ItemIndex>=0)
  then with TKMLLayer(ObjListBox.items.Objects[ObjListBox.ItemIndex]) do
        begin
         SetLength(Reg_arr,length(coordinates));
         for i:=0 to length(Reg_Arr)-1 do
          begin
           Reg_arr[i].x:=coordinates[i].x;
           Reg_arr[i].y:=coordinates[i].y;
          end;
         if (length(coordinates)<2)or
            ((coordinates[0].x<>coordinates[length(coordinates)-1].x)or(coordinates[0].y<>coordinates[length(coordinates)-1].y))
          then fsaveas.Show_(zoom_size,[ExtPoint(coordinatesLT.X,coordinatesLT.Y),ExtPoint(coordinatesRD.X,coordinatesRD.Y)])
          else fsaveas.Show_(zoom_size,Reg_arr)
        end
  else ShowMessage(SAS_MSG_needselect);

end;

end.
