
function ReadFile( fso, fileName){
	var textFile = fso.OpenTextFile(fileName, 1, false);
	var text = "";
	if (!textFile.AtEndOfStream){
		text = textFile.ReadAll();
	};
	textFile.Close();
	return text;
}

var fso = WScript.CreateObject("Scripting.FileSystemObject");

var dpr = ReadFile(fso, "SASPlanet.dpr");
dpr = dpr.replace('  u_EurekaLog,\r\n', "");
var dprFile = fso.OpenTextFile("SASPlanet.dpr", 2, false);
dprFile.write(dpr);

var dproj = ReadFile(fso, "SASPlanet.dproj");
dproj = dproj.replace(';EUREKALOG_VER6', "");
dproj = dproj.replace(';EUREKALOG_VER7', "");
dproj = dproj.replace('EUREKALOG_VER7', "");
dproj = dproj.replace(';EUREKALOG', "");
var dprojFile = fso.OpenTextFile("SASPlanet.dproj", 2, false);
dprojFile.write(dproj);