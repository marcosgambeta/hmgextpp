//
// MINIGUI - Harbour Win32 GUI library source code
//
// Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
// http://harbourminigui.googlepages.com/
//
// FOLDER form source code
// (C)2009 Janusz Pora <januszpora@onet.eu>
//

// $BEGIN_LICENSE$
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// this software; see the file COPYING. If not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
// visit the web site http://www.gnu.org/).
//
// As a special exception, you have permission for additional uses of the text
// contained in this release of Harbour Minigui.
//
// The exception is that, if you link the Harbour Minigui library with other
// files to produce an executable, this does not by itself cause the resulting
// executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of linking the
// Harbour-Minigui library code into it.
// $END_LICENSE$

// Parts of this project are based upon:
//
// "Harbour GUI framework for Win32"
// Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
// Copyright 2001 Antonio Linares <alinares@fivetech.com>
// www - https://harbour.github.io/
//
// "Harbour Project"
// Copyright 1999-2022, https://harbour.github.io/
//
// "WHAT32"
// Copyright 2002 AJ Wos <andrwos@aust1.net>
//
// "HWGUI"
// Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

#include "minigui.ch"
#include "i_winuser.ch"

#define FLBTN_OK            1
#define FLBTN_APPLY         2
#define FLBTN_CANCEL        3
#define FLBTN_HELP          4

#define DS_SETFONT          64
#define DS_CONTROL          1024       // 0x0400L

#define FLD_RC              0
#define FLD_INDIRECT        1

#define FLN_FIRST           -200

#define FLN_SETACTIVE       (FLN_FIRST-0)
#define FLN_KILLACTIVE      (FLN_FIRST-1)
#define FLN_APPLY           (FLN_FIRST-2)
#define FLN_RESET           (FLN_FIRST-3)
#define FLN_HELP            (FLN_FIRST-5)
#define FLN_QUERYCANCEL     (FLN_FIRST-6)
#define FLN_FINISH          (FLN_FIRST-7)

#define FLD_AFH             1          // _HMG_ActiveFolderHandle
#define FLD_MOD             2          // _HMG_ActiveFolderModal
#define FLD_INM             3          // _HMG_FolderInMemory
#define FLD_PGT             4
#define FLD_FLT             5          // _HMG_aFolderTemplate
#define FLD_FPG             6          // _HMG_aFolderPages
#define FLD_FIT             7          // _HMG_aFolderItems
#define FLD_HFP             8          //  aHwndFolderPages

STATIC aHwndFolderPages := {}

/*
_BeginFolder(...) --> NIL
*/
FUNCTION _BeginFolder(name, parent, lRes, x, y, w, h, caption, fontname, fontsize, folderproc, cancelproc, initproc, helpproc, modal, apply, bold, italic, underline, strikeout, buttons, flat, hottrack, vertical, bottom, multiline)

   IF _HMG_BeginDialogActive
      MsgMiniGuiError("DEFINE FOLDER Structures can't be nested.")
   ENDIF

   IF _HMG_BeginWindowActive
      __defaultNIL(@FontName, _HMG_ActiveFontName)
      __defaultNIL(@FontSize, _HMG_ActiveFontSize)
   ENDIF

   IF parent == NIL
      parent := _HMG_ActiveFormName
   ENDIF

   _DefineFolder(name, parent, lRes, x, y, w, h, caption, fontname, fontsize, folderproc, cancelproc, initproc, helpproc, modal, apply, bold, italic, underline, strikeout, buttons, flat, hottrack, vertical, bottom, multiline)

RETURN NIL

/*
_DefineFolder(...) --> 0
*/
FUNCTION _DefineFolder(FormName, ParentForm, lRes, x, y, w, h, caption, fontname, fontsize, folderProcedure, CancelProcedure, InitProcedure, HelpProcedure, modal, apply, bold, italic, underline, strikeout, buttons, flat, hottrack, vertical, bottom, multiline)

   LOCAL i
   LOCAL mVar
   LOCAL k
   LOCAL ParentHandle
   LOCAL style
   LOCAL styleEx
   LOCAL lOkBtn := .F.
   LOCAL lApplyBtn := .F.
   LOCAL lCancelBtn := .F.
   LOCAL lHelpBtn
   LOCAL FontHandle

   DEFAULT x TO 100
   DEFAULT y TO 100
   DEFAULT w TO 300
   DEFAULT h TO 250

   AAdd(_HMG_aFolderInfo, {0, .F., .F., {}, {}, {}, {}, {}})
   _HMG_FldID := Len(_HMG_aFolderInfo)
   _HMG_aFolderInfo[_HMG_FldID,FLD_MOD] := modal
   _HMG_aFolderInfo[_HMG_FldID,FLD_INM] := !lRes
   _HMG_aFolderInfo[_HMG_FldID,FLD_HFP] := {}
   _HMG_aFolderInfo[_HMG_FldID,FLD_FLT] := {}
   _HMG_aFolderInfo[_HMG_FldID,FLD_FPG] := {}
   _HMG_aFolderInfo[_HMG_FldID,FLD_FIT] := {}

   _HMG_aDialogTemplate := {}
   _HMG_DialogInMemory  := .F.

   aHwndFolderPages := {}

   IF (FontHandle := GetFontHandle(FontName)) != HMG_NULLHANDLE
      GetFontParamByRef(FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout)
   ENDIF

   IF FormName == NIL
      FormName := _HMG_TempWindowName
   ENDIF

   FormName := AllTrim(FormName)

   i := AScan(_HMG_aFormType, "A")
   IF i <= 0
      MsgMiniGuiError("Main Window Not Defined.")
   ENDIF

   IF _IsWindowDefined(FormName)
      MsgMiniGuiError("Folder: " + FormName + " already defined.")
   ENDIF

   style := BS_NOTIFY + WS_CHILD + BS_PUSHBUTTON + WS_VISIBLE
   IF hb_IsBlock(FolderProcedure)
      lOkBtn := .T.
      AAdd(_HMG_aFolderInfo[_HMG_FldID,FLD_FIT], {FLBTN_OK, 0, "button", style, 0, 0, 0, 70, 25, _HMG_MESSAGE[6], 0, "", FontName, FontSize, bold, italic, underline, strikeout, , _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})
      IF apply
         lApplyBtn := .T.
         AAdd(_HMG_aFolderInfo[_HMG_FldID,FLD_FIT], {FLBTN_APPLY, 0, "button", style, 0, 0, 0, 70, 25, _HMG_MESSAGE[8], 0, "", FontName, FontSize, bold, italic, underline, strikeout, , _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})
      ENDIF
   ENDIF
   IF hb_IsBlock(CancelProcedure)
      lCancelBtn := .T.
      AAdd(_HMG_aFolderInfo[_HMG_FldID,FLD_FIT], {FLBTN_CANCEL, 0, "button", style, 0, 0, 0, 70, 25, _HMG_MESSAGE[7], 0, "", FontName, FontSize, bold, italic, underline, strikeout, , _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})
   ENDIF
   IF hb_IsBlock(HelpProcedure)
      lHelpBtn := .T.
      AAdd(_HMG_aFolderInfo[_HMG_FldID,FLD_FIT], {FLBTN_HELP, 0, "button", style, 0, 0, 0, 70, 25, "Help", 0, "", FontName, FontSize, bold, italic, underline, strikeout, , _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})
   ENDIF

   mVar := "_" + FormName
   ParentHandle := GetFormHandle(ParentForm)
   style := DS_SETFONT + WS_POPUP + WS_CAPTION + WS_VISIBLE + WS_SYSMENU + WS_THICKFRAME// +WS_MAXIMIZEBOX +WS_MINIMIZEBOX
   styleEx := 0
   _HMG_aFolderInfo[_HMG_FldID,FLD_FLT] := {0, ParentHandle, modal, style, styleEx, x, y, w, h, caption, fontname, fontsize, bold, Italic, lOkBtn, lApplyBtn, lCancelBtn, lHelpBtn, buttons, flat, hottrack, vertical, bottom, multiline}
   _HMG_aFolderInfo[_HMG_FldID,FLD_PGT] := {}
   _HMG_aFolderInfo[_HMG_FldID,FLD_AFH] := 0

   _HMG_ActiveFontName := hb_defaultValue(FontName, _HMG_DefaultFontName)

   _HMG_ActiveFontSize := hb_defaultValue(FontSize, _HMG_DefaultFontSize)

   k := _GetFormFree()

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aFormNames                     [k] := FormName
   _HMG_aFormHandles                   [k] := 0
   _HMG_aFormActive                    [k] := .T.
   _HMG_aFormType                      [k] := "F"     // Windows type Folder
   _HMG_aFormParentHandle              [k] := ParentHandle
   _HMG_aFormReleaseProcedure          [k] := HelpProcedure
   _HMG_aFormInitProcedure             [k] := InitProcedure
   _HMG_aFormToolTipHandle             [k] := 0
   _HMG_aFormContextMenuHandle         [k] := 0
   _HMG_aFormMouseDragProcedure        [k] := ""
   _HMG_aFormSizeProcedure             [k] := ""
   _HMG_aFormClickProcedure            [k] := FolderProcedure
   _HMG_aFormMouseMoveProcedure        [k] := ""
   _HMG_aFormMoveProcedure             [k] := ""
   _HMG_aFormDropProcedure             [k] := ""
   _HMG_aFormDeleted                   [k] := .F.
   _HMG_aFormBkColor                   [k] := {-1, -1, -1}
   _HMG_aFormPaintProcedure            [k] := ""
   _HMG_aFormNoShow                    [k] := .F.
   _HMG_aFormNotifyIconName            [k] := ""
   _HMG_aFormNotifyIconToolTip         [k] := ""
   _HMG_aFormNotifyIconLeftClick       [k] := ""
   _HMG_aFormNotifyIconDblClick        [k] := ""
   _HMG_aFormGotFocusProcedure         [k] := ""
   _HMG_aFormLostFocusProcedure        [k] := ""
   _HMG_aFormReBarHandle               [k] := FontHandle
   _HMG_aFormNotifyMenuHandle          [k] := 0
   _HMG_aFormBrowseList                [k] := {}
   _HMG_aFormSplitChildList            [k] := {}
   _HMG_aFormVirtualHeight             [k] := 0
   _HMG_aFormVirtualWidth              [k] := 0
   _HMG_aFormFocused                   [k] := .F.
   _HMG_aFormScrollUp                  [k] := ""
   _HMG_aFormScrollDown                [k] := ""
   _HMG_aFormScrollLeft                [k] := ""
   _HMG_aFormScrollRight               [k] := ""
   _HMG_aFormHScrollBox                [k] := ""
   _HMG_aFormVScrollBox                [k] := ""
   _HMG_aFormBrushHandle               [k] := 0
   _HMG_aFormFocusedControl            [k] := 0
   _HMG_aFormGraphTasks                [k] := {}
   _HMG_aFormMaximizeProcedure         [k] := NIL
   _HMG_aFormMinimizeProcedure         [k] := NIL
   _HMG_aFormRestoreProcedure          [k] := ""
   _HMG_aFormAutoRelease               [k] := .F.
   _HMG_aFormInteractiveCloseProcedure [k] := CancelProcedure
   _HMG_aFormMinMaxInfo                [k] := {}
   _HMG_aFormActivateId                [k] := 0
   _HMG_aFormMiscData1                 [k] := {}
   _HMG_aFormMiscData2                 [k] := ""
#ifdef _HMG_COMPAT_
   _HMG_StopWindowEventProcedure       [k] := .F.
#endif

   _SetThisFormInfo(k)
/*
   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnFormInit, k, mVar)
   ENDIF
*/
   IF Len(_HMG_aFolderInfo[_HMG_FldID,FLD_FLT]) > 0
#ifdef _NAMES_LIST_
      _HMG_aFolderInfo[_HMG_FldID,FLD_FLT, 1] := _GetNameList(mVar)
#else
      _HMG_aFolderInfo[_HMG_FldID,FLD_FLT, 1] := &mVar.
#endif
   ENDIF

RETURN 0

/*
_DefineFolderPage(FolderName, Id, cTitle, cImageName) --> NIL
*/
FUNCTION _DefineFolderPage(FolderName, Id, cTitle, cImageName)

   LOCAL style := WS_POPUP + WS_CHILD + DS_CONTROL

   DEFAULT Id := 0
   IF Id == 0
      MsgMiniGuiError("FOLDER Page Structures only from Resourses.")
   ENDIF
   IF _IsWindowDefined(FolderName)
      IF _HMG_aFolderInfo[_HMG_FldID,FLD_AFH] == GetFormHandle(FolderName)
         MsgMiniGuiError("Folder Page: " + FolderName + " already defined.")
      ENDIF
   ENDIF

   _HMG_ActiveDialogHandle    := 0
   _HMG_ActiveDialogName      := FolderName
   _HMG_BeginDialogActive     := .T.
   _HMG_DialogInMemory        := .F.

   _HMG_aFolderInfo[_HMG_FldID,FLD_PGT] := {cTitle, Id, Style, cImageName}
   _HMG_aFolderInfo[_HMG_FldID,FLD_AFH] := hmg_CreateFolderPage(_HMG_aFolderInfo[_HMG_FldID,FLD_PGT])
   AAdd(aHwndFolderPages, _HMG_aFolderInfo[_HMG_FldID,FLD_AFH])
   AAdd(_HMG_aFolderInfo[_HMG_FldID,FLD_HFP], 0)
   AAdd(_HMG_aFolderInfo[_HMG_FldID,FLD_FPG], {FolderName, Id, _HMG_aFolderInfo[_HMG_FldID,FLD_AFH], 0, _HMG_DialogInMemory})

   _HMG_ActiveDialogHandle    := 0
   _HMG_ActiveDialogName      := ""
   _HMG_BeginDialogActive     := .F.

RETURN NIL

/*
_BeginFolderPage(FolderName, Id, cTitle, cImageName) --> NIL
*/
FUNCTION _BeginFolderPage(FolderName, Id, cTitle, cImageName)

   LOCAL Style := WS_POPUP + WS_CHILD + DS_CONTROL

   DEFAULT Id := 0
   _HMG_ActiveDialogHandle    := 0
   _HMG_ActiveDialogName      := FolderName
   _HMG_BeginDialogActive     := .T.
   _HMG_DialogInMemory        := .T.
   _HMG_aDialogItems          := {}
   _HMG_aDialogTemplate       := _HMG_aFolderInfo[_HMG_FldID,FLD_FLT]
   IF _IsWindowDefined(FolderName)
      IF _HMG_aFolderInfo[_HMG_FldID,FLD_AFH] == GetFormHandle(FolderName)
         MsgMiniGuiError("Folder Page: " + FolderName + " already defined.")
      ENDIF
   ENDIF

   AAdd(_HMG_aFolderInfo[_HMG_FldID,FLD_FPG], {FolderName, Id, 0, 0, _HMG_DialogInMemory, cImageName})

   _HMG_aFolderInfo[_HMG_FldID,FLD_PGT] := {cTitle, Id, Style, cImageName}

RETURN NIL

/*
_EndFolderPage() --> NIL
*/
FUNCTION _EndFolderPage()

   LOCAL aFldPageTemp
   LOCAL nPos

   IF _HMG_BeginDialogActive
      nPos := Len(_HMG_aFolderInfo[_HMG_FldID,FLD_FPG])
      aFldPageTemp := AClone(_HMG_aFolderInfo[_HMG_FldID,FLD_FLT])
      aFldPageTemp[4] := _HMG_aFolderInfo[_HMG_FldID,FLD_PGT, 3]
      aFldPageTemp[5] := WS_EX_CONTROLPARENT
      _HMG_aFolderInfo[_HMG_FldID,FLD_AFH] := hmg_CreateFolderPageIndirect(_HMG_aFolderInfo[_HMG_FldID,FLD_PGT], aFldPageTemp, _HMG_aDialogItems)

      AAdd(aHwndFolderPages, _HMG_aFolderInfo[_HMG_FldID,FLD_AFH])
      AAdd(_HMG_aFolderInfo[_HMG_FldID,FLD_HFP], 0)

      _HMG_aFolderInfo[_HMG_FldID,FLD_FPG, nPos, 2] := _HMG_aFolderInfo[_HMG_FldID,FLD_PGT, 2]
      _HMG_aFolderInfo[_HMG_FldID,FLD_FPG, nPos, 3] := _HMG_ActiveDialogHandle
      _HMG_aFolderInfo[_HMG_FldID,FLD_FPG, nPos, 4] := _HMG_aDialogItems
   ENDIF

   _HMG_ActiveDialogHandle    := 0
   _HMG_ActiveDialogName      := ""
   _HMG_BeginDialogActive     := .F.
   _HMG_DialogInMemory        := .F.

RETURN NIL

/*
_EndFolder() --> NIL
*/
FUNCTION _EndFolder()

   LOCAL Formhandle
   LOCAL k
   LOCAL ModalFolderReturn

   _PopEventInfo()

   _HMG_aFolderInfo[_HMG_FldID,FLD_AFH] := 0

   IF _HMG_aFolderInfo[_HMG_FldID, FLD_FLT, 3]
      ModalFolderReturn := hmg_CreateDlgFolder(_HMG_FldID, _HMG_aFolderInfo[_HMG_FldID,FLD_AFH], aHwndFolderPages, _HMG_aFolderInfo[_HMG_FldID,FLD_FLT], _HMG_aFolderInfo[_HMG_FldID,FLD_FIT], _HMG_aFolderInfo[_HMG_FldID,FLD_INM])
      IF ModalFolderReturn != 0
         MsgMiniGuiError("MODAL FOLDER from " + iif(_HMG_aFolderInfo[_HMG_FldID,FLD_INM], "memory", "resouces") + " can't be created with success.")
      ENDIF
      _HMG_InitDialogProcedure := ""
      _HMG_ModalDialogProcedure := ""
      _HMG_aDialogItems := {}
      RETURN NIL
   ELSE
      Formhandle := hmg_CreateDlgFolder(_HMG_FldID, _HMG_aFolderInfo[_HMG_FldID,FLD_AFH], aHwndFolderPages, _HMG_aFolderInfo[_HMG_FldID,FLD_FLT], _HMG_aFolderInfo[_HMG_FldID,FLD_FIT], _HMG_aFolderInfo[_HMG_FldID,FLD_INM])
      IF _HMG_aFolderInfo[_HMG_FldID, FLD_FLT, 1] > 0
         _HMG_aFormHandles[_HMG_aFolderInfo[_HMG_FldID, FLD_FLT, 1]] := FormHandle
      ENDIF
   ENDIF

   IF !_HMG_aFolderInfo[_HMG_FldID,FLD_MOD] .AND. Formhandle > 0
      k := _HMG_aFolderInfo[_HMG_FldID, FLD_FLT, 1]
      _HMG_aFolderInfo[_HMG_FldID,FLD_AFH] := Formhandle
      _HMG_aFormHandles[k]      := FormHandle
      _HMG_aFormToolTipHandle[k] := hmg_InitToolTip(FormHandle, hmg_SetToolTipBalloon())
      hmg__SetFont(hmg_Folder_GetTabHandle(FormHandle), _HMG_aFolderInfo[_HMG_FldID,FLD_FLT, 11], _HMG_aFolderInfo[_HMG_FldID,FLD_FLT, 12], _HMG_aFolderInfo[_HMG_FldID,FLD_FLT, 13], _HMG_aFolderInfo[_HMG_FldID,FLD_FLT, 14], .F., .F.)   //,bold,italic,underline,strikeout)
   ENDIF

RETURN NIL

/*
_DefineFolderDialog(FormName, FormHandle, hWndParent) --> NIL
*/
FUNCTION _DefineFolderDialog(FormName, FormHandle, hWndParent)

   LOCAL mVar
   LOCAL k

   mVar := "_" + FormName
   IF _HMG_DialogInMemory
      _HMG_aDialogItems := {}
   ENDIF

   k := _GetFormFree()

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aFormNames                     [k] := FormName
   _HMG_aFormHandles                   [k] := FormHandle
   _HMG_aFormActive                    [k] := .T.
   _HMG_aFormType                      [k] := "D"     // Windows type Dialog
   _HMG_aFormParentHandle              [k] := hWndParent
   _HMG_aFormReleaseProcedure          [k] := ""
   _HMG_aFormInitProcedure             [k] := ""
   _HMG_aFormToolTipHandle             [k] := 0
   _HMG_aFormContextMenuHandle         [k] := 0
   _HMG_aFormMouseDragProcedure        [k] := ""
   _HMG_aFormSizeProcedure             [k] := ""
   _HMG_aFormClickProcedure            [k] := ""
   _HMG_aFormMouseMoveProcedure        [k] := ""
   _HMG_aFormMoveProcedure             [k] := ""
   _HMG_aFormDropProcedure             [k] := ""
   _HMG_aFormDeleted                   [k] := .F.
   _HMG_aFormBkColor                   [k] := {-1, -1, -1}
   _HMG_aFormPaintProcedure            [k] := ""
   _HMG_aFormNoShow                    [k] := .F.
   _HMG_aFormNotifyIconName            [k] := ""
   _HMG_aFormNotifyIconToolTip         [k] := ""
   _HMG_aFormNotifyIconLeftClick       [k] := ""
   _HMG_aFormNotifyIconDblClick        [k] := ""
   _HMG_aFormGotFocusProcedure         [k] := ""
   _HMG_aFormLostFocusProcedure        [k] := ""
   _HMG_aFormReBarHandle               [k] := 0
   _HMG_aFormNotifyMenuHandle          [k] := 0
   _HMG_aFormBrowseList                [k] := {}
   _HMG_aFormSplitChildList            [k] := {}
   _HMG_aFormVirtualHeight             [k] := 0
   _HMG_aFormVirtualWidth              [k] := 0
   _HMG_aFormFocused                   [k] := .F.
   _HMG_aFormScrollUp                  [k] := ""
   _HMG_aFormScrollDown                [k] := ""
   _HMG_aFormScrollLeft                [k] := ""
   _HMG_aFormScrollRight               [k] := ""
   _HMG_aFormHScrollBox                [k] := ""
   _HMG_aFormVScrollBox                [k] := ""
   _HMG_aFormBrushHandle               [k] := 0
   _HMG_aFormFocusedControl            [k] := 0
   _HMG_aFormGraphTasks                [k] := {}
   _HMG_aFormMaximizeProcedure         [k] := NIL
   _HMG_aFormMinimizeProcedure         [k] := NIL
   _HMG_aFormRestoreProcedure          [k] := NIL
   _HMG_aFormAutoRelease               [k] := .F.
   _HMG_aFormInteractiveCloseProcedure [k] := ""
   _HMG_aFormMinMaxInfo                [k] := {}
   _HMG_aFormActivateId                [k] := 0
   _HMG_aFormMiscData1                 [k] := {}
   _HMG_aFormMiscData2                 [k] := ""
#ifdef _HMG_COMPAT_
   _HMG_StopWindowEventProcedure       [k] := .F.
#endif

   _SetThisFormInfo(k)
/*
   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnFormInit, k, mVar)
   ENDIF
*/
RETURN NIL

/*
InitPageFldProc(hWndParent, hwndDlg, idDlg) --> NIL
*/
FUNCTION InitPageFldProc(hWndParent, hwndDlg, idDlg)

   LOCAL i
   LOCAL n
   LOCAL aDialogItems
   LOCAL k_old
   LOCAL nId
   LOCAL k
   LOCAL blInit
   LOCAL FontHandle
   LOCAL ControlHandle

   _HMG_ActiveDialogHandle    := hwndDlg
   _HMG_BeginDialogActive     := .T.
   _HMG_ActiveDlgProcHandle   := hwndDlg
   _HMG_ActiveDlgProcId       := idDlg
   _HMG_FldID                 := hmg_Folder_GetIdFld(hWndParent, _HMG_FldID)

   i := AScan(_HMG_aFolderInfo[_HMG_FldID,FLD_FPG], {|x|x[2] == idDlg})
   IF i > 0
      _HMG_ActiveDialogName   := _HMG_aFolderInfo[_HMG_FldID,FLD_FPG, i, 1]
      aDialogItems            := _HMG_aFolderInfo[_HMG_FldID,FLD_FPG, i, 4]
      _HMG_aFolderInfo[_HMG_FldID,FLD_HFP, i] :=  hwndDlg
   ENDIF
   IF AScan(_HMG_aFormHandles, hwndDlg) == 0
      _DefineFolderDialog(_HMG_ActiveDialogName, hwndDlg, hWndParent)
   ENDIF
   IF _HMG_aFolderInfo[_HMG_FldID,FLD_INM] .AND. hb_IsArray(aDialogItems)
      k_old := 0
      FOR n := 1 TO Len(aDialogItems)

         nId     := aDialogItems[n, 1]
         k       := aDialogItems[n, 2]
         blInit  := aDialogItems[n, 19]
         ControlHandle := hmg_GetDialogItemHandle(hwndDlg, nId)
         FontHandle := GetFontHandle(aDialogItems[n, 13])
         IF !empty(FontHandle)
            hmg__SetFontHandle(ControlHandle, FontHandle)
         ELSE
            IF aDialogItems[n, 13] != NIL .AND. aDialogItems[n, 14] != NIL
               FontHandle := hmg__SetFont(ControlHandle, aDialogItems[n, 13], aDialogItems[n, 14], aDialogItems[n, 15], aDialogItems[n, 16], aDialogItems[n, 17], aDialogItems[n, 18])
            ELSE
               FontHandle := hmg__SetFont(ControlHandle, _HMG_DefaultFontName, _HMG_DefaultFontSize, aDialogItems[n, 15], aDialogItems[n, 16], aDialogItems[n, 17], aDialogItems[n, 18])
            ENDIF
            IF k > 0
               _HMG_aControlFontHandle[k] :=  FontHandle
            ENDIF
         ENDIF

         IF Len(aDialogItems[n]) >= 20
            IF hb_IsLogical(aDialogItems[n, 20]) .AND. aDialogItems[n, 20]
               AAdd(_HMG_ActiveTabCurrentPageMap, Controlhandle)
               IF aDialogItems[n, 21]
                  DO WHILE Len(_HMG_ActiveTabFullPageMap) + 1 < aDialogItems[n, 22]
                     AAdd(_HMG_ActiveTabFullPageMap, {})
                  ENDDO
                  AAdd(_HMG_ActiveTabFullPageMap, _HMG_ActiveTabCurrentPageMap)
                  _HMG_ActiveTabCurrentPageMap := {}
               ENDIF
            ENDIF
         ENDIF
         IF aDialogItems[n, 12] != NIL .AND. hmg_IsWindowHandle(GetFormToolTipHandle(_HMG_ActiveDialogName))
            hmg_SetToolTip(ControlHandle, aDialogItems[n, 12], GetFormToolTipHandle(_HMG_ActiveDialogName))
         ENDIF
         IF k > 0
            IF !hb_isArray(_HMG_aControlHandles[k])
               _HMG_aControlHandles[k] :=  ControlHandle
            ELSE
               IF k_old != k
                  _HMG_aControlHandles[k] :=  {ControlHandle}
               ELSE
                  AAdd(_HMG_aControlHandles[k], ControlHandle)
               ENDIF
            ENDIF
            _HMG_aControlParentHandles[k] := hwndDlg
            k_old := k
         ENDIF
         IF hb_IsBlock(blInit) .AND. !_HMG_aControlDeleted[k]
            Eval(blInit, _HMG_ActiveDialogName, ControlHandle, k)
         ENDIF
      NEXT
   ENDIF

   _HMG_ActiveDialogHandle     := 0
   _HMG_BeginDialogActive      := .F.
   _HMG_ActiveDlgProcHandle    := 0
   _HMG_ActiveDlgProcId        := 0

RETURN NIL

/*
FolderProc(hwndDlg, nMsg, wParam, lParam) --> .T.|.F.
*/
FUNCTION FolderProc(hwndDlg, nMsg, wParam, lParam)

   LOCAL ret := .F.
   LOCAL i
   LOCAL ControlHandle

   _HMG_ActiveDlgProcHandle    := hwndDlg
   _HMG_ActiveDlgProcMsg       := nMsg
   _HMG_ActiveDlgProcId        := wParam
   _HMG_ActiveDlgProcNotify    := lParam
   _HMG_ActiveDlgProcModal     := .F.

   SWITCH nMsg
   CASE WM_INITDIALOG
      _HMG_FldID  := hmg_Folder_GetIdFld(hwndDlg, _HMG_FldID)
      IF _HMG_aFolderInfo[_HMG_FldID,FLD_FLT, 1] > 0
         _HMG_aFolderInfo[_HMG_FldID,FLD_FLT, 2] :=  hwndDlg
         _HMG_aFormHandles[_HMG_aFolderInfo[_HMG_FldID,FLD_FLT, 1]] := hwndDlg
      ENDIF
      i := AScan(_HMG_aFormhandles, hwndDlg)
      IF i > 0
         IF hb_IsBlock(_HMG_aFormInitProcedure[i]) .AND. _HMG_aFormType[i] == "F"
            Eval(_HMG_aFormInitProcedure[i], hwndDlg)
            ret := .T.
         ENDIF
      ENDIF
      EXIT
   CASE WM_CLOSE
      _HMG_FldID  := hmg_Folder_GetIdFld(hwndDlg, _HMG_FldID)
      ret := EraseFolder(hwndDlg, _HMG_aFolderInfo[_HMG_FldID,FLD_MOD])
      EXIT
   CASE WM_COMMAND
      i := AScan(_HMG_aFormhandles, hwndDlg)  // find DialogProcedure
      IF i > 0
         IF hb_IsBlock(_HMG_aFormClickProcedure[i]) .AND. _HMG_aFormType[i] == "F"
            ret :=  RetValue(Eval(_HMG_aFormClickProcedure[i], nMsg, hmg_LOWORD(wParam), hmg_HIWORD(wParam)), .F.)
         ELSE
            ControlHandle := hmg_GetDialogITemHandle(hwndDlg, hmg_LOWORD(wParam))
            Events(hwndDlg, nMsg, wParam, ControlHandle)
            ret := .T.
         ENDIF
      ENDIF
      IF !ret
         ControlHandle := hmg_GetDialogITemHandle(hwndDlg, hmg_LOWORD(wParam))
         Events(hwndDlg, nMsg, wParam, ControlHandle)
         ret := .T.
      ENDIF
      EXIT
   OTHERWISE
      Events(hwndDlg, nMsg, wParam, lParam)
      ret := .F.
   ENDSWITCH

   _HMG_ActiveDlgProcHandle    := 0
   _HMG_ActiveDlgProcMsg       := 0
   _HMG_ActiveDlgProcId        := 0
   _HMG_ActiveDlgProcNotify    := 0

RETURN ret

/*
PageFldProc(hWndDlg, nMsg, wParam, lParam) --> .T.|.F.
*/
FUNCTION PageFldProc(hWndDlg, nMsg, wParam, lParam)

   LOCAL lRet := .F.
   LOCAL i
   LOCAL ControlHandle
   LOCAL x
   LOCAL hwndFolder
   LOCAL nFldID

   _HMG_ActiveDlgProcHandle    := hwndDlg
   _HMG_ActiveDlgProcMsg       := nMsg
   _HMG_ActiveDlgProcId        := wParam
   _HMG_ActiveDlgProcNotify    := lParam
   _HMG_ActiveDlgProcModal     := .F.

   hwndFolder := GetFolderHandle(hwndDlg)
   nFldID     := hmg_Folder_GetIdFld(hwndFolder, _HMG_FldID)

   SWITCH nMsg

   CASE WM_COMMAND
      i := AScan(_HMG_aControlhandles, hmg_numbertohandle(lParam))
      IF _HMG_aFolderInfo[nFldID, FLD_FLT, 2] > 0
         IF i > 0
            IF hmg_HIWORD(wParam) == BN_CLICKED .OR. hmg_HIWORD(wParam) == EN_CHANGE .OR. ;
                  hmg_HIWORD(wParam) == CBN_SELCHANGE .OR. hmg_HIWORD(wParam) == LBN_SELCHANGE .OR. ;
                  hmg_HIWORD(wParam) == DTN_DATETIMECHANGE
               hmg_Folder_Changed(hwndFolder, hWndDlg)
            ENDIF
         ELSE
            IF _HMG_aFolderInfo[nFldID,FLD_INM]
               IF hmg_HIWORD(wParam) == BN_CLICKED
                  FOR i := 1 TO Len(_HMG_aControlHandles)
                     IF hb_IsArray(_HMG_aControlHandles[i]) .AND. _HMG_aControlParentHandles[i] == hwndDlg
                        FOR x := 1 TO Len(_HMG_aControlHandles[i])
                           IF _HMG_aControlHandles[i][x] == lParam
                              hmg_Folder_Changed(hwndFolder, hWndDlg)
                           ENDIF
                        NEXT
                     ENDIF
                  NEXT
               ENDIF
            ELSE
               IF lParam == hmg_GetDialogITemHandle(hwndDlg, hmg_LOWORD(wParam))
                  IF hmg_HIWORD(wParam) == BN_CLICKED .OR. hmg_HIWORD(wParam) == EN_CHANGE .OR. ;
                        hmg_HIWORD(wParam) == CBN_SELCHANGE .OR. hmg_HIWORD(wParam) == LBN_SELCHANGE .OR. ;
                        hmg_HIWORD(wParam) == DTN_DATETIMECHANGE
                     hmg_Folder_Changed(hwndFolder, hWndDlg)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF !lRet
         IF hmg_GetDialogITemHandle(hwndDlg, hmg_LOWORD(wParam)) != 0
            Events(hwndDlg, nMsg, wParam, lParam)
            lRet := .T.
         ENDIF
      ENDIF
      EXIT

   CASE WM_NOTIFY

      SWITCH hmg_GetNotifyCode(lParam)

      CASE FLN_APPLY
         hmg_Folder_UnChanged(hwndFolder, hWndDlg)
         i := AScan(_HMG_aFormhandles, hwndFolder)  // find FolderProcedure
         IF i > 0
            IF hb_IsBlock(_HMG_aFormClickProcedure[i]) .AND. _HMG_aFormType[i] == "F"
               lRet :=  RetValue(Eval(_HMG_aFormClickProcedure[i], nMsg, hmg_LOWORD(wParam), hmg_HIWORD(wParam)), .F.)
            ELSE
               ControlHandle := hmg_GetDialogITemHandle(hwndDlg, hmg_LOWORD(wParam))
               Events(hwndDlg, nMsg, wParam, ControlHandle)
               lRet := .T.
            ENDIF
         ENDIF
         IF !lret
            ControlHandle := hmg_GetDialogITemHandle(hwndDlg, hmg_LOWORD(wParam))
            IF ControlHandle != 0
               Events(hwndDlg, nMsg, wParam, ControlHandle)
            ENDIF
         ENDIF
         EXIT

      CASE FLN_RESET
         i := AScan(_HMG_aFolderInfo[nFldID,FLD_HFP], hwndDlg)
         IF i > 0
            _HMG_aFolderInfo[nFldID,FLD_HFP, i] := 0
            i := AScan(_HMG_aFormhandles, hwndFolder)  // find FolderProcedure
            IF i > 0
               IF hb_IsBlock(_HMG_aFormInteractiveCloseProcedure[i]) .AND. _HMG_aFormType[i] == "F"
                  lRet := RetValue(Eval(_HMG_aFormInteractiveCloseProcedure[i], nMsg, hmg_LOWORD(wParam), hmg_HIWORD(wParam)), .F.)
               ENDIF
            ENDIF
            IF !lret
               ControlHandle := hmg_GetDialogITemHandle(hwndDlg, hmg_LOWORD(wParam))
               IF  ControlHandle != 0
                  Events(hwndDlg, nMsg, wParam, ControlHandle)
               ENDIF
            ENDIF
         ENDIF
         EXIT

      CASE FLN_FINISH
         _ReleaseFolder(hwndFolder)
         hmg_Folder_CleanUp(hwndFolder)
         EXIT

      CASE FLN_HELP
         i := AScan(_HMG_aFormhandles, hwndFolder)  // find FolderProcedure
         IF i > 0
            IF hb_IsBlock(_HMG_aFormReleaseProcedure[i]) .AND. _HMG_aFormType[i] == "F"
               lRet :=  RetValue(Eval(_HMG_aFormReleaseProcedure[i], nMsg, hmg_LOWORD(wParam), hmg_HIWORD(wParam)), .F.)
            ENDIF
         ENDIF
         EXIT

      CASE FLN_SETACTIVE
         EXIT

      CASE FLN_KILLACTIVE
         EXIT

      OTHERWISE
         IF hmg_GetDialogITemHandle(hwndDlg, hmg_LOWORD(wParam)) != 0
            Events(hwndDlg, nMsg, wParam, lParam)
         ENDIF

      ENDSWITCH

      IF !lRet
         IF hmg_GetDialogITemHandle(hwndDlg, hmg_LOWORD(wParam)) != 0
            Events(hwndDlg, nMsg, wParam, lParam)
            lRet := .T.
         ENDIF
      ENDIF
      // TODO: EXIT ?

   OTHERWISE
      IF hmg_GetDialogITemHandle(hwndDlg, hmg_LOWORD(wParam)) != 0
         Events(hwndDlg, nMsg, wParam, lParam)
      ENDIF

   ENDSWITCH

   _HMG_ActiveDlgProcHandle    := 0
   _HMG_ActiveDlgProcMsg       := 0
   _HMG_ActiveDlgProcId        := 0
   _HMG_ActiveDlgProcNotify    := 0

RETURN lRet

/*
GetFolderHandle(hwndDlg) --> handle
*/
FUNCTION GetFolderHandle(hwndDlg)

   LOCAL i := AScan(_HMG_aFormHandles, hwndDlg)

RETURN iif(i > 0, _HMG_aFormParentHandle[i], 0)

/*
EraseFolder(hwndDlg, lModal) --> .T.
*/
FUNCTION EraseFolder(hwndDlg, lModal)

   LOCAL i
   LOCAL x
   LOCAL ControlCount
   LOCAL mVar

   i := AScan(_HMG_aFormhandles, hwndDlg)
   IF i > 0
      ControlCount := Len(_HMG_aControlHandles)
      FOR x := 1 TO ControlCount
         IF _HMG_aControlParentHandles[x] == hwndDlg
            mVar := "_" + _HMG_aFormNames[i] + "_" + _HMG_aControlNames[x]
            IF __mvExist(mVar)
               __mvPut(mVar, 0)
            ENDIF
            _EraseControl(x, i)
         ENDIF
      NEXT x
      mVar := "_" + _HMG_aFormNames[i]
      IF __mvExist(mVar)
         __mvPut(mVar, 0)
      ENDIF
      _HMG_aFormDeleted                  [i] := .T.
      _HMG_aFormhandles                  [i] := 0
      _HMG_aFormNames                    [i] := ""
      _HMG_aFormActive                   [i] := .F.
      _HMG_aFormType                     [i] := ""
      _HMG_aFormParenthandle             [i] := 0
      _HMG_aFormInitProcedure            [i] := ""
      _HMG_aFormReleaseProcedure         [i] := ""
      _HMG_aFormToolTipHandle            [i] := 0
      _HMG_aFormContextMenuHandle        [i] := 0
      _HMG_aFormMouseDragProcedure       [i] := ""
      _HMG_aFormSizeProcedure            [i] := ""
      _HMG_aFormClickProcedure           [i] := ""
      _HMG_aFormMouseMoveProcedure       [i] := ""
      _HMG_aFormMoveProcedure            [i] := ""
      _HMG_aFormDropProcedure            [i] := ""
      _HMG_aFormBkColor                  [i] := NIL
      _HMG_aFormPaintProcedure           [i] := ""
      _HMG_aFormNoShow                   [i] := .F.
      _HMG_aFormNotifyIconName           [i] := ""
      _HMG_aFormNotifyIconToolTip        [i] := ""
      _HMG_aFormNotifyIconLeftClick      [i] := ""
      _HMG_aFormNotifyIconDblClick       [i] := ""
      _HMG_aFormReBarHandle              [i] := 0
      _HMG_aFormNotifyMenuHandle         [i] := 0
      _HMG_aFormBrowseList               [i] := {}
      _HMG_aFormSplitChildList           [i] := {}
      _HMG_aFormVirtualHeight            [i] := 0
      _HMG_aFormGotFocusProcedure        [i] := ""
      _HMG_aFormLostFocusProcedure       [i] := ""
      _HMG_aFormVirtualWidth             [i] := 0
      _HMG_aFormFocused                  [i] := .F.
      _HMG_aFormScrollUp                 [i] := ""
      _HMG_aFormScrollDown               [i] := ""
      _HMG_aFormScrollLeft               [i] := ""
      _HMG_aFormScrollRight              [i] := ""
      _HMG_aFormHScrollBox               [i] := ""
      _HMG_aFormVScrollBox               [i] := ""
      _HMG_aFormBrushHandle              [i] := 0
      _HMG_aFormFocusedControl           [i] := 0
      _HMG_aFormGraphTasks               [i] := {}
      _HMG_aFormMaximizeProcedure        [i] := NIL
      _HMG_aFormMinimizeProcedure        [i] := NIL
      _HMG_aFormRestoreProcedure         [i] := ""
      _HMG_aFormAutoRelease              [i] := .F.
      _HMG_aFormInteractiveCloseProcedure[i] := ""
      _HMG_aFormMinMaxInfo               [i] := {}
      _HMG_aFormActivateId               [i] := 0
      _HMG_aFormMiscData1                [i] := {}
      _HMG_aFormMiscData2                [i] := ""

      IF lModal
         hmg_EndDialog(hwndDlg, 0)
      ELSE
         hmg_DestroyWindow(hwndDlg)
      ENDIF
   ENDIF

RETURN .T.

/*
_ReleaseFolder(hwndFolder) --> NIL
*/
FUNCTION _ReleaseFolder(hwndFolder)

   LOCAL n
   LOCAL nFldID

   nFldID := hmg_Folder_GetIdFld(hwndFolder, _HMG_FldID)
   IF hwndFolder != 0
      FOR n := 1 TO Len(_HMG_aFormParentHandle)
         IF _HMG_aFormParentHandle[n] == hwndFolder
            EraseFolder(_HMG_aFormHandles[n], .F.)
         ENDIF
      NEXT
      EraseFolder(hwndFolder, _HMG_aFolderInfo[nFldID, FLD_MOD])
   ENDIF

RETURN NIL

/*
RetValue(lRet, def) --> .T.|.F.
*/
STATIC FUNCTION RetValue(lRet, def)

   IF lRet == NIL .OR. !hb_isLogical(lRet)
      IF hb_IsNumeric(lRet)
         lRet := iif(lRet == 0, .F., .T.)
      ELSE
         lRet := def
      ENDIF
   ENDIF

RETURN lRet
