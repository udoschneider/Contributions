| package |
package := Package name: 'Appbar'.
package paxVersion: 1;
	basicComment: 'Appbar
Copyright (c) Jerry Bell, 2002
Licensed under OpenBSD license: http://www.openbsd.org/policy.html
'.

package basicPackageVersion: '0.001'.


package classNames
	add: #APPBARDATA;
	add: #AppbarShell;
	add: #AppbarView;
	yourself.

package methodNames
	add: #ShellLibrary -> #SHAppBarMessage:appbarData:;
	yourself.

package globalNames
	add: #AppbarConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!

Win32Structure subclass: #APPBARDATA
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #AppbarShell
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ShellView subclass: #AppbarView
	instanceVariableNames: 'edge isAutohide isHidden isRegistered isMoving'
	classVariableNames: ''
	poolDictionaries: 'AppbarConstants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!ShellLibrary methodsFor!

SHAppBarMessage: aMessage appbarData: anAppBarMessage

	"UINT SHAppBarMessage(DWORD DdwMessage, PAPPBARDATApabd);"

	<stdcall: sdword SHAppBarMessage dword APPBARDATA*>
	^self invalidCall
! !
!ShellLibrary categoriesFor: #SHAppBarMessage:appbarData:!public! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #AppbarConstants put: (PoolConstantsDictionary named: #AppbarConstants)!
AppbarConstants at: 'ABE_BOTTOM' put: 16r3!
AppbarConstants at: 'ABE_FLOAT' put: 16r5!
AppbarConstants at: 'ABE_LEFT' put: 16r0!
AppbarConstants at: 'ABE_RIGHT' put: 16r2!
AppbarConstants at: 'ABE_TOP' put: 16r1!
AppbarConstants at: 'ABE_UNKNOWN' put: 16r4!
AppbarConstants at: 'ABF_ALLOWBOTTOM' put: 16r8!
AppbarConstants at: 'ABF_ALLOWFLOAT' put: 16r10!
AppbarConstants at: 'ABF_ALLOWLEFT' put: 16r1!
AppbarConstants at: 'ABF_ALLOWRIGHT' put: 16r2!
AppbarConstants at: 'ABF_ALLOWTOP' put: 16r4!
AppbarConstants at: 'ABM_ACTIVATE' put: 16r6!
AppbarConstants at: 'ABM_GETAUTOHIDEBAR' put: 16r7!
AppbarConstants at: 'ABM_GETSTATE' put: 16r4!
AppbarConstants at: 'ABM_GETTASKBARPOS' put: 16r5!
AppbarConstants at: 'ABM_NEW' put: 16r0!
AppbarConstants at: 'ABM_QUERYPOS' put: 16r2!
AppbarConstants at: 'ABM_REMOVE' put: 16r1!
AppbarConstants at: 'ABM_SETAUTOHIDEBAR' put: 16r8!
AppbarConstants at: 'ABM_SETPOS' put: 16r3!
AppbarConstants at: 'ABM_WINDOWPOSCHANGED' put: 16r9!
AppbarConstants at: 'ABN_FULLSCREENAPP' put: 16r2!
AppbarConstants at: 'ABN_POSCHANGED' put: 16r1!
AppbarConstants at: 'ABN_STATECHANGE' put: 16r0!
AppbarConstants at: 'ABN_WINDOWARRANGE' put: 16r3!
AppbarConstants shrink!

"Classes"!

APPBARDATA guid: (GUID fromString: '{132662E3-2528-11D5-8DDA-0090278D2010}')!
APPBARDATA comment: ''!
!APPBARDATA categoriesForClass!Unclassified! !
!APPBARDATA class methodsFor!

defineFields

	"struct _AppBarData {
		DWORD cbSize;
		HWND hWnd;
		UINT	uCallbackMessage;
		UINT uEdge;
		RECT rc;
		LPARAM lParam;
		} APPBARDATE, *PAPPBARDATA"

	self 
		defineField: #cbSize type: DWORDField new;
		defineField: #hWnd type: DWORDField new;
		defineField: #uCallbackMessage type: DWORDField new;
		defineField: #uEdge type: DWORDField new;
		defineField: #rc type: (StructureField type: RECT);
		defineField: #lParam type: SDWORDField new
	! !
!APPBARDATA class categoriesFor: #defineFields!public! !

AppbarShell guid: (GUID fromString: '{132662EC-2528-11D5-8DDA-0090278D2010}')!
AppbarShell comment: ''!
!AppbarShell categoriesForClass!Unclassified! !
!AppbarShell methodsFor!

beAutohide

	self view beAutohide!

beDockBottom

	self view beDockBottom!

beDockFloat 

	self view beDockFloat!

beDockLeft

	self view beDockLeft!

beDockRight

	self view beDockRight!

beDockTop
	self view beDockTop!

beNotAutohide

	self view beNotAutohide! !
!AppbarShell categoriesFor: #beAutohide!public! !
!AppbarShell categoriesFor: #beDockBottom!public! !
!AppbarShell categoriesFor: #beDockFloat!public! !
!AppbarShell categoriesFor: #beDockLeft!public! !
!AppbarShell categoriesFor: #beDockRight!public! !
!AppbarShell categoriesFor: #beDockTop!public! !
!AppbarShell categoriesFor: #beNotAutohide!public! !

!AppbarShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.AppbarView)  98 32 0 0 98 2 27131905 131329 416 0 196934 1 ##(Smalltalk.RGB)  25264513 0 517 265030 4 ##(Smalltalk.Menu)  0 16 98 8 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #beDockTop 8 'Dock on Top' 1 1 0 0 0 562 1 594 8 #beDockBottom 8 'Dock on Bottom' 1 1 0 0 0 562 1 594 8 #beDockLeft 8 'Dock on Left' 1 1 0 0 0 562 1 594 8 #beDockRight 8 'Dock on Right' 1 1 0 0 0 562 1 594 8 #beDockFloat 8 'Float' 1 1 0 0 0 983366 1 ##(Smalltalk.DividerMenuItem)  4097 562 1 594 8 #beAutohide 8 'Autohide' 1 1 0 0 0 562 1 594 8 #beNotAutohide 8 'Stop Autohide' 1 1 0 0 0 8 '' 0 1 0 0 0 0 0 0 0 416 0 234 256 98 0 590342 ##(Smalltalk.Rectangle)  328198 ##(Smalltalk.Point)  1 1 1154 1 1 0 0 0 0 1 0 0 0 0 1 0 0 11 32 32 32 16 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 1154 369 369 1154 2521 1483 416 1266 8 #contextMenu: 98 1 528 416 1266 8 #updateMenuBar 1104 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 184 0 0 0 184 0 0 0 164 5 0 0 157 3 0 0] 98 0 1154 193 193 0 27 )! !
!AppbarShell class categoriesFor: #resource_Default_view!public!resources-views! !

AppbarView guid: (GUID fromString: '{132662E2-2528-11D5-8DDA-0090278D2010}')!
AppbarView comment: ''!
!AppbarView categoriesForClass!Unclassified! !
!AppbarView methodsFor!

beAutohide

	| appbarData result |

	(self isRegistered)
		ifFalse:[^false].
	appbarData := self getNewAppbarData.
	appbarData 
		hWnd: (self asParameter);
		uEdge: (self edgeToDockOn);
		lParam: (true asParameter).
	result := (ShellLibrary default SHAppBarMessage: ABM_SETAUTOHIDEBAR appbarData: appbarData) asBoolean.
	result
		ifTrue:[isAutohide := true.
			self registerAsZeroSize.
			self hideAppbar]
		ifFalse:[MessageBox notify: 'There is already an autohide window on this edge, and only one per edge is allowed.'].
	^result
	!

beDockBottom

	self register.
	edge := ABE_BOTTOM.
	self positionAppbar!

beDockFloat

	self unRegister.
	edge := ABE_FLOAT.
	!

beDockLeft

	self register.
	edge := ABE_LEFT.
	self positionAppbar!

beDockRight

	self register.
	edge := ABE_RIGHT.
	self positionAppbar!

beDockTop

	self register.
	edge := ABE_TOP.
	self positionAppbar!

beNotAutohide

	| appbarData result |

	(self isRegistered)
		ifFalse:[^false].
	appbarData := self getNewAppbarData.
	appbarData 
		hWnd: (self asParameter);
		uEdge: (self edgeToDockOn);
		lParam: (false asParameter).
	(ShellLibrary default SHAppBarMessage: ABM_SETAUTOHIDEBAR appbarData: appbarData) asBoolean.
	isAutohide := false.
	isHidden := false.
	self positionAppbar.

	!

correctRectangle: aRectangle

	| theEdge rect |

	
	theEdge := self edgeToDockOn.
	rect := aRectangle deepCopy.
	(theEdge = ABE_LEFT)
		ifTrue:[rect right: (aRectangle left + (self width))].
	(theEdge = ABE_TOP)
		ifTrue:[rect bottom: (aRectangle top + (self height))].
	(theEdge = ABE_RIGHT)
		ifTrue:[rect left: (aRectangle right - (self width))].
	(theEdge = ABE_BOTTOM)
		ifTrue:[rect top: (aRectangle bottom - (self height))].
	^rect!

defaultWindowExStyle
	"Private - Answer the default extended window creation style.
	We use the WS_EX_TOOLWINDOW style to prevent splashes from appearing in the
	desktop tray."

	^super defaultWindowExStyle bitOr: WS_EX_TOOLWINDOW
!

dispatchUser: userId wParam: wParam lParam: lParam

	super dispatchUser: userId wParam: wParam lParam: lParam.
	(userId = 100)
		ifTrue:[self wmAppbarNotified: userId wParam: wParam lParam: lParam ]
		ifFalse:[super dispatchUser: userId wParam: wParam lParam: lParam]!

edgeToDockOn

	^edge
	
	!

getNewAppbarData

	| appbarData |

	appbarData := APPBARDATA new.
	appbarData cbSize: (appbarData size).
	^appbarData
!

getScreenRectangle

	^Rectangle
		left: 0
		top: 0
		right: (UserLibrary default getSystemMetrics: 0)
		bottom: (UserLibrary default getSystemMetrics: 1).
!

getValidRectangle

	"Private - Find the largest area our appbar can cover"

	| appbarData  |

	appbarData := self getNewAppbarData.
	appbarData 
		hWnd: (self asParameter);
		uEdge: (self edgeToDockOn);
		rc: (self getScreenRectangle asParameter).
	ShellLibrary default SHAppBarMessage: ABM_QUERYPOS appbarData: appbarData.
	^appbarData rc asRectangle
	

		
					
	!

hideAppbar

	((self isNotHidden) and: [self isAutohide])
		ifTrue:[isHidden := true.
			self slide]!

initialize

	super initialize.
	isHidden := false.
	isAutohide := false.
	isRegistered := false.
	edge := ABE_FLOAT.
	self hasSysMenu: true.	
	isMoving := false.
	
	
	!

isAutohide

	^isAutohide!

isCloseToBottomOfScreen

	^false!

isCloseToLeftOfScreen

	^false!

isCloseToRightOfScreen

	^false!

isCloseToTopOfScreen

	^false!

isFloating

	^edge = ABE_FLOAT !

isHidden
	
	^isHidden!

isNotAutohide

	^isAutohide not!

isNotHidden
	
	^isHidden not!

isRegistered

	^isRegistered!

notifyActivated

	| appbarData |

	appbarData := self getNewAppbarData.
	appbarData hWnd: (self asParameter).

	ShellLibrary default SHAppBarMessage: ABM_ACTIVATE appbarData: appbarData

	
	!

notifyPositionChanged

	| appbarData |

	(self isRegistered)
		ifFalse:[^false].
	appbarData := self getNewAppbarData.
	appbarData hWnd: (self asParameter).

	ShellLibrary default SHAppBarMessage: ABM_WINDOWPOSCHANGED appbarData: appbarData
!

onBegin: dragOp drag: dragee

	super onBegin: dragOp drag: dragee.
	self halt.
	((self isAutohide) and: [self isNotHidden])
		ifTrue:[self beNotAutohide.
			  self beDockFloat]!

onDestroyed

	super onDestroyed.
	(self isRegistered)
		ifTrue: [self unregisterWithShell]!

onKillFocus

	super onKillFocus.
	(self isAutohide)
		ifTrue:[self hideAppbar]
!

onMouseMoved: aMouseEvent

	super onMouseMoved: aMouseEvent.
	((self isAutohide) and: [self isHidden])
		ifTrue:[self unHideAppbar].!

onPositionChanged: aPositionEvent

	super onPositionChanged: aPositionEvent.
	(self isAutohide)
		ifFalse:[self notifyPositionChanged.
			   	((aPositionEvent isMove) and: [isMoving = false])
					ifTrue:[self beDockFloat]
					ifFalse:[self positionAppbar]].
	(self isFloating)
		ifTrue:[self tryToDock].
!

onPositionChanging: anEvent

	super onPositionChanging: anEvent.
	((((self isAutohide) and: [self isNotHidden]) and: [isMoving = false]) and: [anEvent isMove])
		ifTrue:[self beNotAutohide.
			  self beDockFloat]!

onViewActivated: anEvent

	super onViewActivated: anEvent.
	self isRegistered
		ifTrue:[self notifyActivated].
	(self isHidden)
		ifTrue:[self unHideAppbar]
	
	!

onViewDeactivated: anEvent

	super onViewDeactivated: anEvent.
	(self isAutohide)
		ifTrue:[self hideAppbar]!

positionAppbar

	| appbarData rectangle |

	isMoving := true.
	((self isRegistered) and: [self isNotAutohide])
		ifFalse:[^self].
	rectangle := self correctRectangle: (self getValidRectangle).
	appbarData := self getNewAppbarData.
	appbarData "Private - tell Windows where I want to be"
		hWnd: (self asParameter);
		uEdge: (self edgeToDockOn);
		rc: (rectangle asParameter).
	ShellLibrary default SHAppBarMessage: ABM_SETPOS appbarData: appbarData.
	self rectangle: rectangle.
	isMoving := false
	!

register

	(self isRegistered)
		ifFalse:[self registerWithShell.
				self isToolWindow: true.
				isRegistered := true.
				self hasSysMenu: true]
	
!

registerAsZeroSize

	| appbarData |

	appbarData := self getNewAppbarData.
	appbarData hWnd: (self asParameter).
	appbarData uEdge: (self edgeToDockOn).
	appbarData rc: ((Rectangle origin: 0@0 corner: 0@0) asParameter).
	appbarData lParam: (false asParameter).
	ShellLibrary default SHAppBarMessage: ABM_SETPOS appbarData: appbarData.
!

registerWithShell

	| appbarData |

	appbarData := self getNewAppbarData.
	appbarData hWnd: (self asParameter).
	appbarData uCallbackMessage: (WM_USER + 100).
	ShellLibrary default SHAppBarMessage: ABM_NEW appbarData: appbarData!

repositionAppbar

	| rect |

	isMoving := true.
	rect := self correctRectangle: (self getValidRectangle).
	self rectangle: rect.
	isMoving := false
	!

slide

	isMoving := true.
	(self edgeToDockOn = ABE_LEFT)
		ifTrue:[self slideLeft].
	(self edgeToDockOn = ABE_RIGHT)
		ifTrue:[self slideRight].
	(self edgeToDockOn = ABE_TOP)
		ifTrue:[self slideUp].
	(self edgeToDockOn = ABE_BOTTOM)
		ifTrue:[self slideDown].
	isMoving := false.!

slideDown

	| pos |

	pos := self position.
	pos := (pos + (0@(self height - 5))).
	self slideToPoint: pos!

slideLeft

	| pos |

	pos := self position.
	pos := (pos - ((self width - 5)@0)).
	self slideToPoint: pos!

slideRight

	| pos |

	pos := self position.
	pos := (pos + ((self width - 5)@0)).
	self slideToPoint: pos!

slideToPoint: aPoint

	self position: aPoint!

slideUp

	| pos |

	pos := self position.
	pos := (pos - (0@(self height - 5))).
	self slideToPoint: pos!

tryToDock

	(self isCloseToTopOfScreen)
		ifTrue:[self beDockTop].
	(self isCloseToBottomOfScreen)
		ifTrue:[self beDockBottom].
	(self isCloseToLeftOfScreen)
		ifTrue:[self beDockLeft].
	(self isCloseToRightOfScreen)
		ifTrue:[self beDockRight]

	!

unHideAppbar

	isHidden
		ifTrue:[isHidden := false.
			self repositionAppbar.
			self beTopMost.
			self beForeground]!

unRegister

	(self isRegistered)
		ifTrue: [self unregisterWithShell].
	isRegistered := false.
	self hasSysMenu: true.!

unregisterWithShell

	| appbarData |

	appbarData := self getNewAppbarData.
	appbarData hWnd: (self asParameter).
	ShellLibrary default SHAppBarMessage: ABM_REMOVE appbarData: appbarData.
	!

wmAppbarNotified: userId wParam: wParam lParam: lParam

	(wParam = ABN_FULLSCREENAPP)
		ifTrue:[].
	(wParam = ABN_POSCHANGED)
		ifTrue:[(self isHidden) ifFalse:[self positionAppbar]].
	(wParam = ABN_STATECHANGE)
		ifTrue:[].
	(wParam = ABN_WINDOWARRANGE)
		ifTrue:[].!

xdefaultWindowExStyle
	"Private - Answer the default extended window creation style.
	We use the WS_EX_TOOLWINDOW style to prevent splashes from appearing in the
	desktop tray."

	^super defaultWindowExStyle bitOr: WS_EX_TOOLWINDOW
!

xdefaultWindowStyle
	"Private - Answer the default basic window creation style"

	^##(WS_POPUP | WS_DLGFRAME)
! !
!AppbarView categoriesFor: #beAutohide!public! !
!AppbarView categoriesFor: #beDockBottom!public! !
!AppbarView categoriesFor: #beDockFloat!public! !
!AppbarView categoriesFor: #beDockLeft!public! !
!AppbarView categoriesFor: #beDockRight!public! !
!AppbarView categoriesFor: #beDockTop!public! !
!AppbarView categoriesFor: #beNotAutohide!public! !
!AppbarView categoriesFor: #correctRectangle:!private! !
!AppbarView categoriesFor: #defaultWindowExStyle!constants!private! !
!AppbarView categoriesFor: #dispatchUser:wParam:lParam:!private! !
!AppbarView categoriesFor: #edgeToDockOn!private! !
!AppbarView categoriesFor: #getNewAppbarData!private! !
!AppbarView categoriesFor: #getScreenRectangle!private! !
!AppbarView categoriesFor: #getValidRectangle!private! !
!AppbarView categoriesFor: #hideAppbar!private! !
!AppbarView categoriesFor: #initialize!private! !
!AppbarView categoriesFor: #isAutohide!private! !
!AppbarView categoriesFor: #isCloseToBottomOfScreen!public! !
!AppbarView categoriesFor: #isCloseToLeftOfScreen!public! !
!AppbarView categoriesFor: #isCloseToRightOfScreen!public! !
!AppbarView categoriesFor: #isCloseToTopOfScreen!public! !
!AppbarView categoriesFor: #isFloating!private! !
!AppbarView categoriesFor: #isHidden!private! !
!AppbarView categoriesFor: #isNotAutohide!private! !
!AppbarView categoriesFor: #isNotHidden!private! !
!AppbarView categoriesFor: #isRegistered!private! !
!AppbarView categoriesFor: #notifyActivated!private! !
!AppbarView categoriesFor: #notifyPositionChanged!private! !
!AppbarView categoriesFor: #onBegin:drag:!private! !
!AppbarView categoriesFor: #onDestroyed!private! !
!AppbarView categoriesFor: #onKillFocus!private! !
!AppbarView categoriesFor: #onMouseMoved:!private! !
!AppbarView categoriesFor: #onPositionChanged:!private! !
!AppbarView categoriesFor: #onPositionChanging:!private! !
!AppbarView categoriesFor: #onViewActivated:!private! !
!AppbarView categoriesFor: #onViewDeactivated:!private! !
!AppbarView categoriesFor: #positionAppbar!private! !
!AppbarView categoriesFor: #register!private! !
!AppbarView categoriesFor: #registerAsZeroSize!private! !
!AppbarView categoriesFor: #registerWithShell!private! !
!AppbarView categoriesFor: #repositionAppbar!private! !
!AppbarView categoriesFor: #slide!private! !
!AppbarView categoriesFor: #slideDown!private! !
!AppbarView categoriesFor: #slideLeft!private! !
!AppbarView categoriesFor: #slideRight!private! !
!AppbarView categoriesFor: #slideToPoint:!private! !
!AppbarView categoriesFor: #slideUp!private! !
!AppbarView categoriesFor: #tryToDock!public! !
!AppbarView categoriesFor: #unHideAppbar!private! !
!AppbarView categoriesFor: #unRegister!private! !
!AppbarView categoriesFor: #unregisterWithShell!private! !
!AppbarView categoriesFor: #wmAppbarNotified:wParam:lParam:!private! !
!AppbarView categoriesFor: #xdefaultWindowExStyle!constants!private! !
!AppbarView categoriesFor: #xdefaultWindowStyle!constants!private! !

"Binary Globals"!

