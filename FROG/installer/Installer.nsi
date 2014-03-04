;NSIS Modern User Interface
;Start Menu Folder Selection Example Script
;Written by Joost Verburg

;--------------------------------
;Include Modern UI

  !include "MUI2.nsh"

;--------------------------------
;General

  !define VERSION 3.2.5-dev


  ; Comment out this next line (with a ";") to get the demo installer
  !define Production
  
  ; PickOne
  !define BITS32
  ;!define BITS64
  
  ; Pick one
  ;!define FrogType F
  ;!define FrogType FT
  ;!define FrogType FX
  !define FrogType FTX
  ;!define FrogType Demo

  ;General
  !ifdef Production
    Name "FROG ${VERSION}"
    !ifdef BITS32
        OutFile "FrogInstaller-${FrogType}-${VERSION}-32bits.exe"
    !endif
    !ifdef BITS64
        OutFile "FrogInstaller-${FrogType}-${VERSION}-64bits.exe"
    !endif
  !else
    Name "FROG ${VERSION} Demo"
    OutFile "FROGDemoInstaller-${VERSION}.exe"
  !endif

  ;Folder selection page
  !ifdef BITS32
    InstallDir "$PROGRAMFILES\Femtosoft Technologies\FROG"
  !endif
  !ifdef BITS64
    InstallDir "$PROGRAMFILES64\Femtosoft Technologies\FROG"
  !endif
  
  !define FEMTOSOFT_REG_KEY "SOFTWARE\Femtosoft Technologies"
  !define FROG_REG_KEY "${FEMTOSOFT_REG_KEY}\FROG"
  
  ;Get install folder from registry if available
  InstallDirRegKey HKLM "${FROG_REG_KEY}" "Install_Dir"

  ;Request application privileges for Windows Vista
  RequestExecutionLevel admin

;--------------------------------
;Variables

  Var StartMenuFolder

;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_LICENSE "LICENSE.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  
  ;Start Menu Folder Page Configuration
  !insertmacro MUI_DEFAULT MUI_STARTMENUPAGE_DEFAULTFOLDER "Femtosoft Technologies\FROG"
  !define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKCU" 
  !define MUI_STARTMENUPAGE_REGISTRY_KEY "${FROG_REG_KEY}" 
  !define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "StartMenu"
  
  !insertmacro MUI_PAGE_STARTMENU Application $StartMenuFolder
  
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "FROG" SecFROG

  !ifdef BITS64
    SetRegView 64
  !endif

  ; Add all the files
  SetOutPath "$INSTDIR"
  File "..\Frog3.exe"
  File "frog.ico"
  File "LICENSE.TXT"
  
  SetOutPath "$INSTDIR\doc"
  File "..\doc\FROG.chm"

  SetOutPath "$INSTDIR\Samples"
  File "..\examples\*.*"
  
  SetOutPath "$INSTDIR\Tutorial"
  File "..\doc\*.pdf"

  ; Need to do this so the help file root dir is set correctly
  SetOutPath "$INSTDIR"
  
  ; Create Log Dirs
  CreateDirectory "$PROFILE\Femtosoft\FROG"
      
  ;Store installation folder
  WriteRegStr HKLM "${FROG_REG_KEY}" "Install_Dir" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\UninstallFROG.exe"
  
  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
    
    ;Create shortcuts
    CreateDirectory "$SMPROGRAMS\$StartMenuFolder"
    CreateShortCut "$SMPROGRAMS\$StartMenuFolder\UninstallFROG.lnk" "$INSTDIR\UninstallFROG.exe"
  
    CreateShortCut "$SMPROGRAMS\$StartMenuFolder\FROG.lnk" "$INSTDIR\FROG3.exe" "" "$INSTDIR\frog.ico" 0
    CreateShortCut "$SMPROGRAMS\$StartMenuFolder\FROGFastGuideTutorial.lnk" "$INSTDIR\Tutorial\FROGFastGuideTutorial.pdf" "" "$INSTDIR\frog.ico" 0
  
  !insertmacro MUI_STARTMENU_WRITE_END

SectionEnd

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecFROG ${LANG_ENGLISH} "Install the FROG program and example files."

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${SecFROG} $(DESC_SecFROG)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section

Section "Uninstall"

  !ifdef BITS64
    SetRegView 64
  !endif

  ;ADD YOUR OWN FILES HERE...
  Delete $INSTDIR\*.*

  Delete "$INSTDIR\UninstallFROG.exe"

  RMDir /r "$INSTDIR"

  !insertmacro MUI_STARTMENU_GETFOLDER Application $StartMenuFolder
    
  Delete "$SMPROGRAMS\$StartMenuFolder\UninstallFROG.lnk"
  Delete "$SMPROGRAMS\$StartMenuFolder\FROG.lnk"
  Delete "$SMPROGRAMS\$StartMenuFolder\FROGFastGuideTutorial.lnk"
  RMDir /r "$SMPROGRAMS\$StartMenuFolder"
  ; RMDir should only remove the dir if it's completely empty, so if QuickFrog is also installed this should not break 
  RMDir "$SMPROGRAMS\Femtosoft Technologies"
  
  ; Delete install dir reg key
  DeleteRegKey HKLM "${FROG_REG_KEY}"
  DeleteRegKey /ifempty HKLM "${FEMTOSOFT_REG_KEY}"
  DeleteRegKey HKCU "${FROG_REG_KEY}"

SectionEnd