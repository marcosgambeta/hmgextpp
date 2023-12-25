/*
 * MINIGUI - Harbour Win32 GUI library source code
 * 
 * Copyright 2017 P.Chornyj <myorg63@mail.ru>
 */

#xcommand ON APPEVENT [ID] <nId> ACTION <bAction> OF <window> [<noactive: NOACTIVE>] [<once: ONCE>] ;
=> ;
hmg_AppEvents( <window>, <nId>, <bAction>, !<.noactive.>, <.once.> )

#xcommand ON APPEVENT [ID] <nId> ACTION <bAction> OF <window> [<noactive: NOACTIVE>] [<once: ONCE>] [RESULT] TO <lResult> ;
=> ;
<lResult> := hmg_AppEvents( <window>, <nId>, <bAction>, !<.noactive.>, <.once.> )


#xtranslate EMIT [EVENT] [ID] <nId> OF <window> ;
=> ;
hmg_SendMessage( <window>, <nId>, 0, 0 )


#xcommand REMOVE APPEVENT [ID] [<nId>] OF <window> [<once: ONCE>] ;
=> ;
hmg_AppEventsRemove( <window>, <nId>, <.once.> )

#xcommand REMOVE APPEVENT [ID] [<nId>] OF <window> [<once: ONCE>] [RESULT] TO <lResult> ;
=> ;
<lResult> := hmg_AppEventsRemove( <window>, <nId>, <.once.> )

#xcommand REMOVE APPEVENT ALL OF <window> [<once: ONCE>] ;
=> ;
hmg_AppEventsRemove( <window>, 0, <.once.> )

#xcommand REMOVE APPEVENT ALL OF <window> [<once: ONCE>] [RESULT] TO <lResult> ;
=> ;
<lResult> := hmg_AppEventsRemove( <window>, 0, <.once.> )


#xcommand UPDATE APPEVENT [ID] <nId> [ACTION <bAction>] OF <window> [<noactive: NOACTIVE>] [<once: ONCE>] ;
=> ;
hmg_AppEventsUpdate( <window>, <nId>, <bAction>, !<.noactive.>, <.once.> )

#xcommand UPDATE APPEVENT [ID] <nId> [ACTION <bAction>] OF <window> [<noactive: NOACTIVE>] [<once: ONCE>] [RESULT] TO <lResult> ;
=> ;
<lResult> := hmg_AppEventsUpdate( <window>, <nId>, <bAction>, !<.noactive.>, <.once.> )


#xcommand [DEFINE] [WINDOW] MESSAGEONLY <window> [EVENTS [FUNC] <efunc>] [RESULT] TO <lResult> ; 
=> ; 
<lResult> := hmg_InitMessageOnlyWindow( <"window">, <"efunc"> )


#xcommand ON WINEVENT [ID] <nId> ACTION <bAction> OF <window> [<noactive: NOACTIVE>] [<once: ONCE>];
=> ;
hmg_WinEvents( <window>, <nId>, <bAction>, !<.noactive.>, <.once.> )

#xcommand ON WINEVENT [ID] <nId> ACTION <bAction> OF <window> [<noactive: NOACTIVE>] [<once: ONCE>] [RESULT] TO <lResult>;
=> ;
<lResult> := hmg_WinEvents( <window>, <nId>, <bAction>, !<.noactive.>, <.once.> )


#xcommand REMOVE WINEVENT [ID] [<nId>] OF <window> [<once: ONCE>];
=> ;
hmg_WinEventsRemove( <window>, <nId>, <.once.> )

#xcommand REMOVE WINEVENT [ID] [<nId>] OF <window> [<once: ONCE>] [RESULT] TO <lResult>;
=> ;
<lResult> := hmg_WinEventsRemove( <window>, <nId>, <.once.> )

#xcommand REMOVE WINEVENT ALL OF <window> [<once: ONCE>];
=> ;
hmg_WinEventsRemove( <window>, 0, <.once.> )

#xcommand REMOVE WINEVENT ALL OF <window> [<once: ONCE>] [RESULT] TO <lResult>;
=> ;
<lResult> := hmg_WinEventsRemove( <window>, 0, <.once.> )


#xcommand UPDATE WINEVENT [ID] <nId> [ACTION <bAction>] OF <window> [<noactive: NOACTIVE>] [<once: ONCE>];
=> ;
hmg_WinEventsUpdate( <window>, <nId>, <bAction>, !<.noactive.>, <.once.> )

#xcommand UPDATE WINEVENT [ID] <nId> [ACTION <bAction>] OF <window> [<noactive: NOACTIVE>] [<once: ONCE>] [RESULT] TO <lResult>;
=> ;
<lResult> := hmg_WinEventsUpdate( <window>, <nId>, <bAction>, !<.noactive.>, <.once.> )
