@echo off
rem  Script to convert from Nordic to qml with filename as only option
set infile=%1
IF "%infile%"=="" (goto nofile)
@echo on
java -jar  %SEISAN_TOP%/PRO/nor2qml.jar --input=%1
@echo off
echo Output qml file is quakeml.xml
goto end
:nofile
echo You must give an input Nordic file
:end 
