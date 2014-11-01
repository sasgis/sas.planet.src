
set wmcd="..\..\.bin\TestWMCopyData.exe"
set log=TestWMCopyData.log

set loc=(27.5,53.9)
set nav=(30.99,60)
set map="{F6574B06-E632-4D5F-BC75-C8FA658B57DF}"

%wmcd% > "1_"%log%
%wmcd% -h > "2_"%log%
%wmcd% --help > "3_"%log%

%wmcd% --map=%map% --zoom=12 --move=%loc% --show-placemarks=0 > "4_"%log%
%wmcd% --show-placemarks=1 --insert-placemark="Test Name";%loc%;"Test Description" > "5_"%log%
%wmcd% --navigate=%nav% --zoom=10 > "6_"%log%
%wmcd% --move=%nav%

rem test errors
%wmcd% --move=(600,10) --zoom=0 --show-placemarks=-1 > "7_"%log%
%wmcd% --undef-option > "8_"%log%
%wmcd% --insert-placemark > "9_"%log%
