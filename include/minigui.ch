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

#ifndef __HMG__

#ifdef HMG_USE_POINTERS
   #define HMG_NULLHANDLE   NIL
#else
   #define HMG_NULLHANDLE   0
#endif

#ifdef HMG_USE_POINTERS
   #define hmg_numbertohandle(x)   win_n2p(x)
#else
   #define hmg_numbertohandle(x)   x
#endif

#ifdef HMG_USE_POINTERS
   #define NTOP(v)   (v)
#else
   #define NTOP(v)   waNToP(v)
#endif

#include "mgver.h"

/* ***********************************************************************
 * Enable support for legacy functions
 *
 * By default this is turned OFF.
 */
#ifndef HMG_LEGACY_ON
  #define HMG_LEGACY_OFF
#endif

/* ***********************************************************************
 * Enable multilingual support in HMG
 *
 * Note that if you turn this off, the English will be set as the default language.
 *
 * By default this is turned ON.
 */
#define _MULTILINGUAL_

/* ***********************************************************************
 * Enable support for TSBrowse library in HMG
 *
 * By default this is turned ON.
 */
#define _TSBROWSE_

/* ***********************************************************************
 * Enable support for PropGrid library in HMG
 *
 * By default this is turned ON.
 */
#define _PROPGRID_

/* ***********************************************************************
 * Enable this option if you want that the internal Public variables were
 * stored in the global hash instead of the many Public variables.
 *
 * By default this is turned OFF.
 */
//#define _NAMES_LIST_

/* ***********************************************************************
 * Disable this option if you want that the internal Public variables to be
 * equate to zero at windows and controls destroying (similar to Official HMG).
 *
 * By default this is turned ON.
 */
#define _PUBLIC_RELEASE_

/* ***********************************************************************
 * Enable support for standard Browse control in HMG
 *
 * By default this is turned ON.
 */
#define _DBFBROWSE_

/* ***********************************************************************
 * Enable support for Panel windows in HMG
 *
 * By default this is turned ON.
 */
#define _PANEL_

/* ***********************************************************************
 * Enable support for Pager toolbar in HMG
 *
 * By default this is turned ON.
 */
#define _PAGER_

/* ***********************************************************************
 * Enable support for compatibility with Official HMG code
 *
 * By default this is turned ON.
 */
#define _HMG_COMPAT_

/* ***********************************************************************
 * Enable support for simple debug logging
 *
 * By default this is turned OFF to preserve the proper Harbour functionality.

#ifndef _HMG_OUTLOG
  #define _HMG_OUTLOG
#endif
 */

/* ***********************************************************************
 * Enable support for the internal OOP classes
 *
 * By default this is turned ON.
 */
#define _OBJECT_

/* ***********************************************************************
 * Enable support for User Components in HMG
 *
 * By default this is turned ON.
 */
#define _USERINIT_

#ifdef _USERINIT_
  #include "i_UsrInit.ch"
  #include "i_UsrSOOP.ch"
#else
  #xcommand DECLARE CUSTOM COMPONENTS <Window> ;
  =>;
  #define SOOP_DUMMY ;;
  #undef SOOP_DUMMY
#endif

#include "i_var.ch"
#include "i_error.ch"
#include "i_media.ch"
#include "i_pseudofunc.ch"
#include "i_exec.ch"
#include "i_comm.ch"
#include "i_keybd.ch"
#include "i_keybd_ext.ch"
#include "i_checkbox.ch"
#include "i_menu.ch"
#include "i_misc.ch"
#include "i_timer.ch"
#include "i_frame.ch"
#include "i_slider.ch"
#include "i_progressbar.ch"
#include "i_window.ch"
#include "i_window_ext.ch"
#include "i_button.ch"
#include "i_image.ch"
#include "i_imagelist.ch"
#include "i_radiogroup.ch"
#include "i_label.ch"
#include "i_combobox.ch"
#include "i_datepicker.ch"
#include "i_listbox.ch"
#include "i_spinner.ch"
#include "i_textbox.ch"
#include "i_editbox.ch"
#include "i_grid.ch"
#include "i_tab.ch"
#include "i_controlmisc.ch"
#include "i_color.ch"
#include "i_toolbar.ch"
#include "i_splitbox.ch"
#include "i_tree.ch"
#include "i_status.ch"
#include "i_ini.ch"
#include "i_encrypt.ch"
#include "i_Help.ch"
#include "i_monthcal.ch"
#include "i_region.ch"
#include "i_socket.ch"
#include "i_ipaddress.ch"
#include "i_altsyntax.ch"
#include "i_ScrSaver.ch"
#include "i_registry.ch"
#include "i_edit.ch"
#include "i_report.ch"
#include "i_lang.ch"
#include "i_this.ch"
#include "i_hyperlink.ch"
#include "i_zip.ch"
#include "i_graph.ch"
#include "i_richeditbox.ch"
#include "i_browse.ch"
#include "i_dll.ch"
#include "i_tooltip.ch"
#include "i_dialog.ch"
#include "i_font.ch"
#include "i_winprop.ch"
#include "i_appevents.ch"
#include "i_getbox.ch"
#include "i_btntextbox.ch"
#include "i_hotkeybox.ch"
#include "i_brush.ch"
#include "i_folder.ch"
#include "i_pager.ch"
#include "i_chklabel.ch"
#include "i_progresswheel.ch"
#include "i_rating.ch"

#ifdef _MIXEDMODE_
  ANNOUNCE HB_GTSYS
  REQUEST HB_GT_WIN_DEFAULT
#endif

// control types
/* TODO: usar numeros no lugar de strings */
#if 1
#define CONTROL_TYPE_ACTIVEX       "ACTIVEX"
#define CONTROL_TYPE_ANIGIF        "ANIGIF"
#define CONTROL_TYPE_ANIMATEBOX    "ANIMATEBOX"
#define CONTROL_TYPE_ANIMATERES    "ANIMATERES"
#define CONTROL_TYPE_BROWSE        "BROWSE"
#define CONTROL_TYPE_BTNNUMTEXT    "BTNNUMTEXT"
#define CONTROL_TYPE_BTNTEXT       "BTNTEXT"
#define CONTROL_TYPE_BUTTON        "BUTTON"
#define CONTROL_TYPE_CHARMASKTEXT  "CHARMASKTEXT"
#define CONTROL_TYPE_CHECKBOX      "CHECKBOX"
#define CONTROL_TYPE_CHECKLABEL    "CHECKLABEL"
#define CONTROL_TYPE_CHKLIST       "CHKLIST"
#define CONTROL_TYPE_CLBUTTON      "CLBUTTON"
#define CONTROL_TYPE_COMBO         "COMBO"
#define CONTROL_TYPE_DATEPICK      "DATEPICK"
#define CONTROL_TYPE_EDIT          "EDIT"
#define CONTROL_TYPE_FONT          "FONT"
#define CONTROL_TYPE_FRAME         "FRAME"
#define CONTROL_TYPE_GETBOX        "GETBOX"
#define CONTROL_TYPE_GRID          "GRID"
#define CONTROL_TYPE_HOTKEY        "HOTKEY"
#define CONTROL_TYPE_HOTKEYBOX     "HOTKEYBOX"
#define CONTROL_TYPE_HYPERLINK     "HYPERLINK"
#define CONTROL_TYPE_IMAGE         "IMAGE"
#define CONTROL_TYPE_IMAGELIST     "IMAGELIST"
#define CONTROL_TYPE_IPADDRESS     "IPADDRESS"
#define CONTROL_TYPE_ITEMMESSAGE   "ITEMMESSAGE"
#define CONTROL_TYPE_LABEL         "LABEL"
#define CONTROL_TYPE_LIST          "LIST"
#define CONTROL_TYPE_LISTBOX       "LISTBOX"
#define CONTROL_TYPE_MASKEDTEXT    "MASKEDTEXT"
#define CONTROL_TYPE_MENU          "MENU"
#define CONTROL_TYPE_MESSAGEBAR    "MESSAGEBAR"
#define CONTROL_TYPE_MONTHCAL      "MONTHCAL"
#define CONTROL_TYPE_MULTICHKLIST  "MULTICHKLIST"
#define CONTROL_TYPE_MULTIGRID     "MULTIGRID"
#define CONTROL_TYPE_MULTILIST     "MULTILIST"
#define CONTROL_TYPE_NUMTEXT       "NUMTEXT"
#define CONTROL_TYPE_OBUTTON       "OBUTTON"
#define CONTROL_TYPE_PAGER         "PAGER"
#define CONTROL_TYPE_PLAYER        "PLAYER"
#define CONTROL_TYPE_POPUP         "POPUP"
#define CONTROL_TYPE_PROGRESSBAR   "PROGRESSBAR"
#define CONTROL_TYPE_PROGRESSWHEEL "PROGRESSWHEEL"
#define CONTROL_TYPE_PROPGRID      "PROPGRID"
#define CONTROL_TYPE_RADIOGROUP    "RADIOGROUP"
#define CONTROL_TYPE_RATING        "RATING"
#define CONTROL_TYPE_RICHEDIT      "RICHEDIT"
#define CONTROL_TYPE_SLIDER        "SLIDER"
#define CONTROL_TYPE_SPBUTTON      "SPBUTTON"
#define CONTROL_TYPE_SPINNER       "SPINNER"
#define CONTROL_TYPE_TAB           "TAB"
#define CONTROL_TYPE_TBROWSE       "TBROWSE"
#define CONTROL_TYPE_TEXT          "TEXT"
#define CONTROL_TYPE_TIMEPICK      "TIMEPICK"
#define CONTROL_TYPE_TIMER         "TIMER"
#define CONTROL_TYPE_TOOLBAR       "TOOLBAR"
#define CONTROL_TYPE_TOOLBUTTON    "TOOLBUTTON"
#define CONTROL_TYPE_TREE          "TREE"
#define CONTROL_TYPE_WEBCAM        "WEBCAM"
#else
#define CONTROL_TYPE_ACTIVEX       1
#define CONTROL_TYPE_ANIGIF        2
#define CONTROL_TYPE_ANIMATEBOX    3
#define CONTROL_TYPE_ANIMATERES    4
#define CONTROL_TYPE_BROWSE        5
#define CONTROL_TYPE_BTNNUMTEXT    6
#define CONTROL_TYPE_BTNTEXT       7
#define CONTROL_TYPE_BUTTON        8
#define CONTROL_TYPE_CHARMASKTEXT  9
#define CONTROL_TYPE_CHECKBOX      10
#define CONTROL_TYPE_CHECKLABEL    11
#define CONTROL_TYPE_CHKLIST       12
#define CONTROL_TYPE_CLBUTTON      13
#define CONTROL_TYPE_COMBO         14
#define CONTROL_TYPE_DATEPICK      15
#define CONTROL_TYPE_EDIT          16
#define CONTROL_TYPE_FONT          17
#define CONTROL_TYPE_FRAME         18
#define CONTROL_TYPE_GETBOX        19
#define CONTROL_TYPE_GRID          20
#define CONTROL_TYPE_HOTKEY        21
#define CONTROL_TYPE_HOTKEYBOX     22
#define CONTROL_TYPE_HYPERLINK     23
#define CONTROL_TYPE_IMAGE         24
#define CONTROL_TYPE_IMAGELIST     25
#define CONTROL_TYPE_IPADDRESS     26
#define CONTROL_TYPE_ITEMMESSAGE   27
#define CONTROL_TYPE_LABEL         28
#define CONTROL_TYPE_LIST          29
#define CONTROL_TYPE_LISTBOX       30
#define CONTROL_TYPE_MASKEDTEXT    31
#define CONTROL_TYPE_MENU          32
#define CONTROL_TYPE_MESSAGEBAR    33
#define CONTROL_TYPE_MONTHCAL      34
#define CONTROL_TYPE_MULTICHKLIST  35
#define CONTROL_TYPE_MULTIGRID     36
#define CONTROL_TYPE_MULTILIST     37
#define CONTROL_TYPE_NUMTEXT       38
#define CONTROL_TYPE_OBUTTON       39
#define CONTROL_TYPE_PAGER         40
#define CONTROL_TYPE_PLAYER        41
#define CONTROL_TYPE_POPUP         42
#define CONTROL_TYPE_PROGRESSBAR   43
#define CONTROL_TYPE_PROGRESSWHEEL 44
#define CONTROL_TYPE_PROPGRID      45
#define CONTROL_TYPE_RADIOGROUP    46
#define CONTROL_TYPE_RATING        47
#define CONTROL_TYPE_RICHEDIT      48
#define CONTROL_TYPE_SLIDER        49
#define CONTROL_TYPE_SPBUTTON      50
#define CONTROL_TYPE_SPINNER       51
#define CONTROL_TYPE_TAB           52
#define CONTROL_TYPE_TBROWSE       53
#define CONTROL_TYPE_TEXT          54
#define CONTROL_TYPE_TIMEPICK      55
#define CONTROL_TYPE_TIMER         56
#define CONTROL_TYPE_TOOLBAR       57
#define CONTROL_TYPE_TOOLBUTTON    58
#define CONTROL_TYPE_TREE          59
#define CONTROL_TYPE_WEBCAM        60
#endif

#endif /* __HMG__ */
