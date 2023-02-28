/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this software; see the file COPYING. If not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
 * visit the web site http://www.gnu.org/).
 *
 * As a special exception, you have permission for additional uses of the text
 * contained in this release of Harbour Minigui.
 *
 * The exception is that, if you link the Harbour Minigui library with other
 * files to produce an executable, this does not by itself cause the resulting
 * executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of linking the
 * Harbour-Minigui library code into it.
 *
 * Parts of this project are based upon:
 *
 * "Harbour GUI framework for Win32"
 * Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * www - https://harbour.github.io/
 *
 * "Harbour Project"
 * Copyright 1999-2022, https://harbour.github.io/
 *
 * "WHAT32"
 * Copyright 2002 AJ Wos <andrwos@aust1.net>
 *
 * "HWGUI"
 * Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>
 */

#include "minigui.ch"
#include "i_winuser.ch"

FUNCTION _DefineChkListbox(ControlName, ParentFormName, x, y, w, h, arows, value, ;
      fontname, fontsize, tooltip, changeprocedure, dblclick, gotfocus, lostfocus, break, HelpId, ;
      invisible, notabstop, sort, bold, italic, underline, strikeout, backcolor, fontcolor, ;
      multiselect, aCheck, nItemHeight, nId)

   LOCAL ParentFormHandle
   LOCAL blInit
   LOCAL mVar
   LOCAL ControlHandle
   LOCAL Style
   LOCAL FontHandle
   LOCAL rows := {}
   LOCAL i
   LOCAL k
   LOCAL aChkItem := {}
   LOCAL nPos
   LOCAL lDialogInMemory

   DEFAULT w               TO 120
   DEFAULT h               TO 120
   DEFAULT gotfocus        TO ""
   DEFAULT lostfocus       TO ""
   DEFAULT value           TO 0
   DEFAULT changeprocedure TO ""
   DEFAULT dblclick        TO ""
   DEFAULT invisible       TO .F.
   DEFAULT notabstop       TO .F.
   DEFAULT sort            TO .F.
   DEFAULT aCheck          TO {}
   DEFAULT nItemHeight     TO 16

   IF (FontHandle := GetFontHandle(FontName)) != 0
      GetFontParamByRef(FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout)
   ENDIF

   IF !ISARRAY(arows)
      arows := {}
   ENDIF
   IF Len(arows) > 0
      IF !ISARRAY(arows[1])
         rows := AClone(arows)
         AEval(arows, {|x, y|HB_SYMBOL_UNUSED(x), nPos := y, AAdd(aChkItem, iif(AScan(aCheck, {|z|z == nPos}) > 0, 2, 1))})
      ELSE
         AEval(arows, {|x|AAdd(rows, x[1])})
         AEval(arows, {|x, y|nPos := y, AAdd(aChkItem, iif(HB_ISLOGICAL(x[2]) .AND. x[2] .OR. AScan(aCheck, {|z|z == nPos}) > 0, 2, 1))})
      ENDIF
   ENDIF

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      ParentFormName := iif(_HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName)
      __defaultNIL(@FontName, _HMG_ActiveFontName)
      __defaultNIL(@FontSize, _HMG_ActiveFontSize)
   ENDIF

   IF _HMG_FrameLevel > 0 .AND. !_HMG_ParentWindowActive
      x += _HMG_ActiveFrameCol[_HMG_FrameLevel]
      y += _HMG_ActiveFrameRow[_HMG_FrameLevel]
      ParentFormName := _HMG_ActiveFrameParentFormName[_HMG_FrameLevel]
   ENDIF
   lDialogInMemory := _HMG_DialogInMemory

   IF !_IsWindowDefined(ParentFormName) .AND. !lDialogInMemory
      MsgMiniGuiError("Window: " + IFNIL(ParentFormName, "Parent", ParentFormName) + " is not defined.")
   ENDIF

   IF _IsControlDefined(ControlName, ParentFormName) .AND. !lDialogInMemory
      MsgMiniGuiError("Control: " + ControlName + " Of " + ParentFormName + " Already defined.")
   ENDIF

   mVar := "_" + ParentFormName + "_" + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      Style := WS_BORDER + WS_CHILD + WS_VSCROLL + LBS_DISABLENOSCROLL + LBS_NOTIFY + LBS_NOINTEGRALHEIGHT

      IF multiselect
         Style += LBS_MULTIPLESEL
      ENDIF

      IF !notabstop
         Style += WS_TABSTOP
      ENDIF

      IF !invisible
         Style += WS_VISIBLE
      ENDIF

      IF sort
         Style += LBS_SORT
      ENDIF


      IF lDialogInMemory         //Dialog Template

         //          {{"ID",k/hwnd,class,Style,ExStyle,x,y,w,h,caption,HelpId,tooltip,font,size, bold, italic, underline, strikeout}}  --->_HMG_aDialogItems
         blInit := {|x, y, z| InitDialogChkListBox(x, y, z) }
         AAdd(_HMG_aDialogItems, {nId, k, "LISTBOX", style, 0, x, y, w, h, "", HelpId, tooltip, FontName, FontSize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage})

      ELSE

         ControlHandle := GetDialogItemHandle(ParentFormHandle, nId)

         x := GetWindowCol(Controlhandle)
         y := GetWindowRow(Controlhandle)
         w := GetWindowWidth(Controlhandle)
         h := GetWindowHeight(Controlhandle)

         SetWindowStyle(ControlHandle, Style, .T.)

      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle(ParentFormName)

      IF x == NIL .OR. y == NIL

         IF _HMG_SplitLastControl == "TOOLBAR"
            Break := .T.
         ENDIF

         i := GetFormIndex(ParentFormName)

         IF i > 0

            IF multiselect
               ControlHandle := InitMultiChkListBox(_HMG_aFormReBarHandle[i], 0, x, y, w, h, fontname, fontsize, invisible, notabstop, sort, nItemHeight)
            ELSE
               ControlHandle := InitChkListBox(_HMG_aFormReBarHandle[i], 0, 0, 0, w, h, "", 0, invisible, notabstop, sort, nItemHeight)
            ENDIF

            AddSplitBoxItem(Controlhandle, _HMG_aFormReBarHandle[i], w, break, , , , _HMG_ActiveSplitBoxInverted)

            _HMG_SplitLastControl := "LISTBOX"

         ENDIF

      ELSE

         IF multiselect
            ControlHandle := InitMultiChkListBox(ParentFormHandle, 0, x, y, w, h, fontname, fontsize, invisible, notabstop, sort, nItemHeight)
         ELSE
            ControlHandle := InitChkListBox(ParentFormHandle, 0, x, y, w, h, "", 0, invisible, notabstop, sort, nItemHeight)
         ENDIF

      ENDIF

   ENDIF

   IF !lDialogInMemory

      IF FontHandle != 0
         _SetFontHandle(ControlHandle, FontHandle)
      ELSE
         __defaultNIL(@FontName, _HMG_DefaultFontName)
         __defaultNIL(@FontSize, _HMG_DefaultFontSize)
         IF IsWindowHandle(ControlHandle)
            FontHandle := _SetFont(ControlHandle, FontName, FontSize, bold, italic, underline, strikeout)
         ENDIF
      ENDIF

      IF _HMG_BeginTabActive
         AAdd(_HMG_ActiveTabCurrentPageMap, ControlHandle)
      ENDIF

      IF tooltip != NIL
         SetToolTip(ControlHandle, tooltip, GetFormToolTipHandle(ParentFormName))
      ENDIF

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList(mVar, k)
#else
   Public &mVar. := k
#endif

   _HMG_aControlType               [k] := iif(multiselect, CONTROL_TYPE_MULTICHKLIST, CONTROL_TYPE_CHKLIST)
   _HMG_aControlNames              [k] := ControlName
   _HMG_aControlHandles            [k] := ControlHandle
   _HMG_aControlParenthandles      [k] := ParentFormHandle
   _HMG_aControlIds                [k] := nId
   _HMG_aControlProcedures         [k] := ""
   _HMG_aControlPageMap            [k] := {}
   _HMG_aControlValue              [k] := Value
   _HMG_aControlInputMask          [k] := ""
   _HMG_aControllostFocusProcedure [k] := lostfocus
   _HMG_aControlGotFocusProcedure  [k] := gotfocus
   _HMG_aControlChangeProcedure    [k] := ChangeProcedure
   _HMG_aControlDeleted            [k] := .F.
   _HMG_aControlBkColor            [k] := backcolor
   _HMG_aControlFontColor          [k] := fontcolor
   _HMG_aControlDblClick           [k] := dblclick
   _HMG_aControlHeadClick          [k] := {}
   _HMG_aControlRow                [k] := y
   _HMG_aControlCol                [k] := x
   _HMG_aControlWidth              [k] := w
   _HMG_aControlHeight             [k] := h
   _HMG_aControlSpacing            [k] := nItemHeight
   _HMG_aControlContainerRow       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1)
   _HMG_aControlContainerCol       [k] := iif(_HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1)
   _HMG_aControlPicture            [k] := ""
   _HMG_aControlContainerHandle    [k] := 0
   _HMG_aControlFontName           [k] := fontname
   _HMG_aControlFontSize           [k] := fontsize
   _HMG_aControlFontAttributes     [k] := {bold, italic, underline, strikeout}
   _HMG_aControlToolTip            [k] := tooltip
   _HMG_aControlRangeMin           [k] := rows
   _HMG_aControlRangeMax           [k] := aCheck
   _HMG_aControlCaption            [k] := ""
   _HMG_aControlVisible            [k] := iif(invisible, .F., .T.)
   _HMG_aControlHelpId             [k] := HelpId
   _HMG_aControlFontHandle         [k] := FontHandle
   _HMG_aControlBrushHandle        [k] := 0
   _HMG_aControlEnabled            [k] := .T.
   _HMG_aControlMiscData1          [k] := ""
   _HMG_aControlMiscData2          [k] := ""

   IF _HMG_lOOPEnabled
      Eval(_HMG_bOnControlInit, k, mVar)
   ENDIF

   IF Len(_HMG_aDialogTemplate) == 0     //Dialog Template
      IF Len(aRows) > 0
         AEval(Rows, {|r, n|ChkListboxAddItem(ControlHandle, r, aChkItem[n], nItemHeight)})
      ENDIF

      IF FontSize != _HMG_DefaultFontSize .AND. Len(Rows) > 0
         SetChkLBItemHeight(ControlHandle, FontHandle)
      ENDIF

      IF multiselect
         IF ISARRAY(value)
            LISTBOXSETMULTISEL(ControlHandle, Value)
         ENDIF
      ELSE
         IF ISNUMBER(value) .AND. value != 0
            ListboxSetCurSel(ControlHandle, Value)
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

FUNCTION InitDialogChkListBox(ParentName, ControlHandle, k)

   LOCAL Rows
   LOCAL Value
   LOCAL FontSize
   LOCAL FontHandle

   HB_SYMBOL_UNUSED(ParentName)

   Rows        := _HMG_aControlRangeMin[k]
   Value       := _HMG_aControlValue[k]
   FontSize    := _HMG_aControlFontSize[k]
   FontHandle  := _HMG_aControlFontHandle[k]

   IF Len(Rows) > 0
      AEval(Rows, {|r|ListboxAddString(ControlHandle, r)})
   ENDIF

   IF FontSize != _HMG_DefaultFontSize .AND. Len(Rows) > 0
      SetChkLBItemHeight(ControlHandle, FontHandle)
   ENDIF

   IF _HMG_aControlType[k] == CONTROL_TYPE_MULTICHKLIST
      IF ISARRAY(value)
         LISTBOXSETMULTISEL(ControlHandle, Value)
      ENDIF
   ELSE
      IF ISNUMBER(value) .AND. value != 0
         ListboxSetCurSel(ControlHandle, Value)
      ENDIF
   ENDIF
// JP 62
   IF Len(_HMG_aDialogTemplate) != 0 .AND. _HMG_aDialogTemplate[3]  // Modal
      _HMG_aControlDeleted[k] := .T.
   ENDIF

RETURN NIL

#pragma BEGINDUMP

#include "mgdefs.hpp"
#include <commctrl.h>
#include <hbwinuni.h>

#ifndef WC_LISTBOX
#define WC_LISTBOX "ListBox"
#endif

#define BUFFER MAX_PATH

HINSTANCE GetInstance(void);

static int m_nHeightItem = 16;

HB_FUNC_STATIC( INITCHKLISTBOX )
{
   HWND hwnd;
   HWND hbutton;
   int style = WS_CHILD | WS_VSCROLL | LBS_DISABLENOSCROLL | LBS_NOTIFY | LBS_NOINTEGRALHEIGHT | LBS_OWNERDRAWFIXED | LBS_HASSTRINGS | LBS_WANTKEYBOARDINPUT;

   hwnd = hmg_par_HWND(1);
   m_nHeightItem = 16;

   if( !hb_parl(9) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(10) )
   {
      style |= WS_TABSTOP;
   }

   if( hb_parl(11) )
   {
      style |= LBS_SORT;
   }

   if( hb_parni(12) )
   {
      m_nHeightItem = hb_parni(12);
   }

   hbutton = CreateWindowEx(WS_EX_CLIENTEDGE,
                            WC_LISTBOX,
                            "",
                            style,
                            hmg_par_int(3),
                            hmg_par_int(4),
                            hmg_par_int(5),
                            hmg_par_int(6),
                            hwnd,
                            hmg_par_HMENU(2),
                            GetInstance(),
                            nullptr);

   hmg_ret_HANDLE(hbutton);
}

HB_FUNC_STATIC( INITMULTICHKLISTBOX )
{
   HWND hwnd;
   HWND hbutton;
   int style = LBS_EXTENDEDSEL | WS_CHILD | WS_VSCROLL | LBS_DISABLENOSCROLL | LBS_NOTIFY | LBS_MULTIPLESEL | LBS_NOINTEGRALHEIGHT | LBS_OWNERDRAWFIXED | LBS_HASSTRINGS;

   hwnd = hmg_par_HWND(1);
   m_nHeightItem = 16;

   if( !hb_parl(9) )
   {
      style |= WS_VISIBLE;
   }

   if( !hb_parl(10) )
   {
      style |= WS_TABSTOP;
   }

   if( hb_parl(11) )
   {
      style |= LBS_SORT;
   }

   if( hb_parni(12) )
   {
      m_nHeightItem = hb_parni(12);
   }

   hbutton = CreateWindowEx(WS_EX_CLIENTEDGE,
                            WC_LISTBOX,
                            "",
                            style,
                            hmg_par_int(3),
                            hmg_par_int(4),
                            hmg_par_int(5),
                            hmg_par_int(6),
                            hwnd,
                            hmg_par_HMENU(2),
                            GetInstance(),
                            nullptr);

   hmg_ret_HANDLE(hbutton);
}

HB_FUNC( CHKLISTBOXINSERTITEM )
{
   HWND hwnd = hmg_par_HWND(1);
   void * String;
   LPCTSTR lpString = HB_PARSTR(2, &String, nullptr);
   int lbItem = hb_parni(3) - 1;
   int bChecked = hb_parni(4);

   SendMessage(hwnd, LB_INSERTSTRING, static_cast<WPARAM>(lbItem), reinterpret_cast<LPARAM>(lpString));
   SendMessage(hwnd, LB_SETITEMDATA, static_cast<WPARAM>(static_cast<int>(lbItem)), static_cast<LPARAM>(bChecked));

   hb_strfree(String);
}

HB_FUNC( CHKLISTBOXADDITEM )
{
   HWND hwnd = hmg_par_HWND(1);
   void * String;
   LPCTSTR lpString = HB_PARSTR(2, &String, nullptr);
   int bChecked = hb_parni(3);
   int lbItem;

   m_nHeightItem = hb_parni(4);
   lbItem = static_cast<int>(SendMessage(hwnd, LB_ADDSTRING, 0, reinterpret_cast<LPARAM>(lpString)));
   SendMessage(hwnd, LB_SETITEMDATA, static_cast<WPARAM>(static_cast<int>(lbItem)), static_cast<LPARAM>(bChecked));

   hb_strfree(String);
}

HB_FUNC( SETCHKLBITEMHEIGHT ) // set the height of a string in pixels
{
   TCHAR achBuffer[BUFFER];
   HWND hwnd = hmg_par_HWND(1);
   HDC hdc = GetDC(hwnd);
   HFONT hFont = hmg_par_HFONT(2);
   HFONT hOldFont = nullptr;
   SIZE sz;

   if( !hdc )
   {
      hwnd = GetActiveWindow();
      hdc = GetDC(hwnd);
   }
   SendMessage(hwnd, LB_GETTEXT, 0, reinterpret_cast<LPARAM>(achBuffer));

   if( hFont )
   {
      hOldFont = static_cast<HFONT>(SelectObject(hdc, hFont));
   }

   GetTextExtentPoint32(hdc, achBuffer, static_cast<int>(HB_STRLEN(achBuffer)), &sz);

   if( sz.cy > m_nHeightItem )
   {
      m_nHeightItem = sz.cy;

      SendMessage(hwnd, LB_SETITEMHEIGHT, 0, MAKELPARAM(m_nHeightItem, 0));
   }

   if( hFont )
   {
      SelectObject(hdc, hOldFont);
   }

   ReleaseDC(hwnd, hdc);
}

HB_FUNC( CHKLIST_SETCHECKBOX )
{
   HWND hwnd = hmg_par_HWND(1);
   int lbItem = hb_parni(2) - 1;
   int bChecked = hb_parni(3);
   TCHAR cString[1024] = {""};

   SendMessage(hwnd, LB_GETTEXT, static_cast<WPARAM>(lbItem), reinterpret_cast<LPARAM>(cString));
   SendMessage(hwnd, LB_DELETESTRING, static_cast<WPARAM>(lbItem), 0);
   SendMessage(hwnd, LB_INSERTSTRING, static_cast<WPARAM>(lbItem), reinterpret_cast<LPARAM>(cString));
   SendMessage(hwnd, LB_SETITEMDATA, static_cast<WPARAM>(lbItem), static_cast<LPARAM>(bChecked));
}

HB_FUNC( CHKLIST_GETCHECKBOX )
{
   HWND hwnd = hmg_par_HWND(1);
   int lbItem = hb_parni(2);
   int iCheck = static_cast<int>(SendMessage(hwnd, LB_GETITEMDATA, static_cast<WPARAM>(lbItem) - 1, 0));

   hb_retl(iCheck - 1);
}

HB_FUNC( _ONMEASURELISTBOXITEM )
{
   LPMEASUREITEMSTRUCT lpmis;

   lpmis = reinterpret_cast<LPMEASUREITEMSTRUCT>(HB_PARNL(1));

   // Set the height of the list box items.
   lpmis->itemHeight = m_nHeightItem;
}

HB_FUNC( _ONDRAWLISTBOXITEM )
{
   PDRAWITEMSTRUCT pdis;
   TCHAR achBuffer[BUFFER];
   int cch;
   int yPos, iCheck, style = 0;
   TEXTMETRIC tm;
   RECT rcCheck;
   HBRUSH hBackBrush;

   pdis = reinterpret_cast<PDRAWITEMSTRUCT>(HB_PARNL(1));

   // If there are no list box items, skip this message.
   if( static_cast<int>(pdis->itemID) > -1 )
   {
      // Draw the bitmap and text for the list box item. Draw a
      // rectangle around the bitmap if it is selected.

      switch( pdis->itemAction )
      {
         case ODA_SELECT:
         case ODA_DRAWENTIRE:

            iCheck = static_cast<int>(SendMessage(pdis->hwndItem, LB_GETITEMDATA, pdis->itemID, 0));

            if( pdis->itemState & ODS_SELECTED )
            {
               SetTextColor(pdis->hDC, GetSysColor(COLOR_HIGHLIGHTTEXT));
               SetBkColor(pdis->hDC, GetSysColor(COLOR_HIGHLIGHT));
               hBackBrush = CreateSolidBrush(GetSysColor(COLOR_HIGHLIGHT));

            }
            else
            {
               SetTextColor(pdis->hDC, GetSysColor(COLOR_WINDOWTEXT));
               SetBkColor(pdis->hDC, GetSysColor(COLOR_WINDOW));
               hBackBrush = CreateSolidBrush(GetSysColor(COLOR_WINDOW));
            }
            FillRect(pdis->hDC, &pdis->rcItem, hBackBrush);
            DeleteObject(hBackBrush);
            rcCheck = pdis->rcItem;
            if( iCheck )
            {
               rcCheck.left += 4;
               rcCheck.top += 2;
               rcCheck.right = rcCheck.left + (pdis->rcItem.bottom - pdis->rcItem.top);
               rcCheck.bottom -= 2;

               if( iCheck == 1 )
               {
                  style = DFCS_BUTTONCHECK;
               }
               else if( iCheck == 2 )
               {
                  style = DFCS_BUTTONCHECK | DFCS_CHECKED;
               }
               DrawFrameControl(pdis->hDC, &rcCheck, DFC_BUTTON, style);

            }

            // Draw the string associated with the item.
            //
            // Get the item string from the list box.
            SendMessage(pdis->hwndItem, LB_GETTEXT, pdis->itemID, reinterpret_cast<LPARAM>(achBuffer));

            // Get the metrics for the current font.
            GetTextMetrics(pdis->hDC, &tm);

            // Calculate the vertical position for the item string
            // so that the string will be vertically centered in the
            // item rectangle.
            yPos = (pdis->rcItem.bottom + pdis->rcItem.top - tm.tmHeight) / 2;
            // Get the character length of the item string.
            cch = static_cast<int>(HB_STRLEN(achBuffer));
            // Draw the string in the item rectangle, leaving a six
            // pixel gap between the item bitmap and the string.
            TextOut(pdis->hDC, rcCheck.right + 6, yPos, achBuffer, cch);

            break;

         case ODA_FOCUS:
            DrawFocusRect(pdis->hDC, &pdis->rcItem);
            break;
      }
   }
}

/*
   Function GETMISCTLTYPE return value of CtlType MEASUREITEMSTRUCT member
 */
HB_FUNC( GETMISCTLTYPE )
{
   LPMEASUREITEMSTRUCT pmis = reinterpret_cast<LPMEASUREITEMSTRUCT>(HB_PARNL(1));

   if( pmis )
   {
      hb_retni(static_cast<UINT>(pmis->CtlType));
   }
}

#pragma ENDDUMP
