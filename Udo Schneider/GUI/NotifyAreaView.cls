"Filed out from Dolphin Smalltalk 7"!

View subclass: #NotifyAreaView
	instanceVariableNames: 'icons version'
	classVariableNames: 'DefaultInstance NotifyMessageMap WM_NOTIFYAREACALLBACK WM_TASKBARCREATED'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NotifyAreaView guid: (GUID fromString: '{aea859af-b752-4704-93c1-3d891c9e0f67}')!
NotifyAreaView comment: ''!
!NotifyAreaView categoriesForClass!Unclassified! !
!NotifyAreaView methodsFor!

add: aNotifyAreaIcon 
	aNotifyAreaIcon identifier: self nextIconIdentifier.
	icons add: aNotifyAreaIcon.
	self basicAdd: aNotifyAreaIcon!

basicAdd: aNotifyAreaIcon
	| notifyStruct |
	notifyStruct := NOTIFYICONDATA2 new
				hWnd: self handle;
				uID: aNotifyAreaIcon identifier;
				message: WM_NOTIFYAREACALLBACK;
				tipText: aNotifyAreaIcon message asUtf8String;
				icon: aNotifyAreaIcon icon;
				yourself.
	ShellLibrary default shell_NotifyIcon: NIM_ADD pnid: notifyStruct!

basicHide: aNotifyAreaIcon 
	| notifyStruct |
	notifyStruct := (NOTIFYICONDATA2 new)
				hWnd: self handle;
				uID: aNotifyAreaIcon identifier;
				yourself.
	ShellLibrary default shell_NotifyIcon: NIM_DELETE pnid: notifyStruct!

basicReAddIcons
	icons do: [:each | each ifNotNil: [:value | self basicAdd: value]]!

basicRemoveIcons
	icons do: [:each | each ifNotNil: [:value | self basicHide: value]]!

basicUpdate: aNotifyAreaIcon
	| notifyStruct |
	notifyStruct := NOTIFYICONDATA2 new
				hWnd: self handle;
				uID: aNotifyAreaIcon identifier;
				message: WM_NOTIFYAREACALLBACK;
				tipText: aNotifyAreaIcon message asUtf8String ;
				icon: aNotifyAreaIcon icon;
				yourself.
	ShellLibrary default shell_NotifyIcon: NIM_MODIFY pnid: notifyStruct!

dispatchRegistered: registeredId wParam: wParam lParam: lParam 
	"Private - Dispatch the window message"

	registeredId = WM_NOTIFYAREACALLBACK 
		ifTrue: 
			[(NotifyMessageMap includesKey: lParam) 
				ifTrue: 
					["Transcript
						show: 'Known Message ' , (NotifyMessageMap at: lParam);
						cr."
					^self perform: (NotifyMessageMap at: lParam) with: (self iconFromIdentifier: wParam)]
				ifFalse: 
					["Transcript
						show: 'Unknown Message ' , lParam displayString;
						cr"]].
	registeredId = WM_TASKBARCREATED ifTrue: [self onTaskbarCreated].
	^super 
		dispatchRegistered: registeredId
		wParam: wParam
		lParam: lParam!

hide: aNotifyAreaIcon 
	self basicHide: aNotifyAreaIcon.
	icons remove: aNotifyAreaIcon ifAbsent: [].
	aNotifyAreaIcon identifier: nil!

iconFromIdentifier: identifier 
	
	^icons detect: [:each | each identifier = identifier] ifNone: [nil]!

initialize
	super initialize.
	icons := WeakSet new.
	self useWin2000Behavior.
!

nextIconIdentifier
	| iconIds iconId |
	iconIds := icons collect: [:each | each identifier ].
	iconId := 0.
	[iconIds includes: iconId] whileTrue: [iconId  := iconId  +1].
	^iconId!

onBalloonClick: aNotifyAreaIcon 
	aNotifyAreaIcon trigger: #ballonClick!

onBalloonHide
!

onBalloonShow: aNotifyAreaIcon 
	aNotifyAreaIcon trigger: #ballonShow!

onBalloonTimeout: aNotifyAreaIcon 
aNotifyAreaIcon trigger: #ballonTimeout!

onKeySelect: anUndefinedObject 
	!

onLeftButtonDoubleClicked: aNotifiyAreaIcon 
aNotifiyAreaIcon trigger: #leftButtonDoubleClicked!

onLeftButtonPressed: aNotifiyAreaIcon 
	aNotifiyAreaIcon trigger: #leftButtonPressed!

onLeftButtonReleased: aNotifiyAreaIcon 
aNotifiyAreaIcon trigger: #leftButtonReleased!

onMouseMoved: aNotifyAreaIcon 
	aNotifyAreaIcon trigger: #mouseMoved!

onRightButtonDoubleClicked: aNotifiyAreaIcon 
	aNotifiyAreaIcon trigger: #rightButtonDoubleClicked!

onRightButtonPressed: aNotifiyAreaIcon 
	aNotifiyAreaIcon trigger: #rightButtonPressed!

onRightButtonReleased: aNotifiyAreaIcon 
	aNotifiyAreaIcon trigger: #rightButtonReleased!

onSelect: anUndefinedObject 
!

onShowContextMenu: aNotifiyAreaIcon 
	aNotifiyAreaIcon trigger: #showContextMenu!

onStartup
	self
		parentView: View desktop;
		create.
		self setVersion: version.
	self basicReAddIcons!

onTaskbarCreated
	self basicReAddIcons!

onViewClosed
	self basicRemoveIcons.
	!

setVersion: anInteger 
	| notifyStruct |
	version := anInteger.
	notifyStruct := (NOTIFYICONDATA2 new)
				hWnd: self handle;
				uVersion: version;
				yourself.
	ShellLibrary default shell_NotifyIcon: NIM_SETVERSION pnid: notifyStruct!

showBalloon: aNotifyAreaIcon icon: anIcon title: title message: message flags: dwInfoFlags timeout: anInteger playSound: aBoolean 
	| notifyStruct |
	notifyStruct := (NOTIFYICONDATA2 new)
				hWnd: self handle;
				balloonTitle: title;
				balloonMessage: message;
				balloonTimeout: anInteger;
				balloonFlags: (dwInfoFlags bitOr: (aBoolean ifFalse: [NIIF_NOSOUND] ifTrue: [0]));
				yourself.
	(dwInfoFlags anyMask: NIIF_USER) ifTrue: [notifyStruct hIcon: anIcon asParameter].
	ShellLibrary default shell_NotifyIcon: NIM_MODIFY pnid: notifyStruct!

useWin2000Behavior
	"Use the Windows 2000 behavior. Use this value for applications designed for Windows 2000 and later."

	self setVersion: NOTIFYICON_VERSION
	

!

useWin95Behavior
	"Use the Windows 95 behavior. Use this value for applications designed for Windows versions prior to Windows 2000."

	self setVersion: 0!

useWinVistaBehavior
	"Use the Windows Vista behavior. Use this value for applications designed for Windows Vista and later."
Error notYetImplemented.
#USToDo. "reimplement #iconFromIdentifier"
	self setVersion: NOTIFYICON_VERSION4! !
!NotifyAreaView categoriesFor: #add:!public! !
!NotifyAreaView categoriesFor: #basicAdd:!helpers!private! !
!NotifyAreaView categoriesFor: #basicHide:!helpers!private! !
!NotifyAreaView categoriesFor: #basicReAddIcons!helpers!private! !
!NotifyAreaView categoriesFor: #basicRemoveIcons!helpers!private! !
!NotifyAreaView categoriesFor: #basicUpdate:!public! !
!NotifyAreaView categoriesFor: #dispatchRegistered:wParam:lParam:!dispatching!private! !
!NotifyAreaView categoriesFor: #hide:!public! !
!NotifyAreaView categoriesFor: #iconFromIdentifier:!helpers!private! !
!NotifyAreaView categoriesFor: #initialize!initializing!private! !
!NotifyAreaView categoriesFor: #nextIconIdentifier!helpers!private! !
!NotifyAreaView categoriesFor: #onBalloonClick:!public! !
!NotifyAreaView categoriesFor: #onBalloonHide!public! !
!NotifyAreaView categoriesFor: #onBalloonShow:!public! !
!NotifyAreaView categoriesFor: #onBalloonTimeout:!public! !
!NotifyAreaView categoriesFor: #onKeySelect:!public! !
!NotifyAreaView categoriesFor: #onLeftButtonDoubleClicked:!event handling!public! !
!NotifyAreaView categoriesFor: #onLeftButtonPressed:!event handling!public! !
!NotifyAreaView categoriesFor: #onLeftButtonReleased:!event handling!public! !
!NotifyAreaView categoriesFor: #onMouseMoved:!event handling!public! !
!NotifyAreaView categoriesFor: #onRightButtonDoubleClicked:!event handling!public! !
!NotifyAreaView categoriesFor: #onRightButtonPressed:!event handling!public! !
!NotifyAreaView categoriesFor: #onRightButtonReleased:!event handling!public! !
!NotifyAreaView categoriesFor: #onSelect:!public! !
!NotifyAreaView categoriesFor: #onShowContextMenu:!event handling!public! !
!NotifyAreaView categoriesFor: #onStartup!event handling!public! !
!NotifyAreaView categoriesFor: #onTaskbarCreated!event handling!public! !
!NotifyAreaView categoriesFor: #onViewClosed!event handling!public! !
!NotifyAreaView categoriesFor: #setVersion:!public! !
!NotifyAreaView categoriesFor: #showBalloon:icon:title:message:flags:timeout:playSound:!helpers!private! !
!NotifyAreaView categoriesFor: #useWin2000Behavior!public! !
!NotifyAreaView categoriesFor: #useWin95Behavior!public! !
!NotifyAreaView categoriesFor: #useWinVistaBehavior!public! !

!NotifyAreaView class methodsFor!

default
	"Answers the singleton instance."

	DefaultInstance isNil 
		ifTrue: 
			[DefaultInstance := (self new)
						parentView: View desktop;
						create;
						yourself].
	^DefaultInstance!

initialize
	self
		initializeNotificationMap;
		registerMessages.
		SessionManager current 
		when: #sessionStopped
		send: #onExit
		to: self.
	^super initialize!

initializeNotificationMap
	"Private - Initialise the map of  notification codes to selectors.
	N.B. This method must not be stripped in order to ensure that the notification event handler
	methods (which are looked up in the table) are preserved.
	
	self initializeNotificationMap
	"

	NotifyMessageMap := (IdentityDictionary new)
				at: WM_LBUTTONDOWN put: #onLeftButtonPressed:;
				at: WM_LBUTTONUP put: #onLeftButtonReleased:;
				at: WM_RBUTTONDOWN put: #onRightButtonPressed:;
				at: WM_RBUTTONUP put: #onRightButtonReleased:;
				at: WM_LBUTTONDBLCLK put: #onLeftButtonDoubleClicked:;
				at: WM_RBUTTONDBLCLK put: #onRightButtonDoubleClicked:;
				at: WM_MOUSEMOVE put: #onMouseMoved:;
				at: WM_CONTEXTMENU put: #onShowContextMenu:;
				at: NIN_BALLOONSHOW put: #onBalloonShow:;
				at: NIN_BALLOONHIDE put: #onBalloonHide;
				at: NIN_BALLOONTIMEOUT put: #onBalloonTimeout:;
				at: NIN_BALLOONUSERCLICK put: #onBalloonClick:;
				at: NIN_SELECT put: #onSelect:;
				at: NIN_KEYSELECT put: #onKeySelect:;
				shrink;
				yourself!

onExit
	DefaultInstance ifNotNil: [:value | value basicRemoveIcons ]!

onStartup
	"Private - Perform any startup operations."

	self registerMessages.
	!

registerMessages
	WM_NOTIFYAREACALLBACK := self registerMessage: 'NotifyAreaCallback'.
	WM_TASKBARCREATED := self registerMessage: 'TaskbarCreated'!

uninitialize
	"Private - Uninitialize the receiver as it is about to be removed from the system."

	DefaultInstance notNil 
		ifTrue: 
			[DefaultInstance destroy.
			DefaultInstance := nil]! !
!NotifyAreaView class categoriesFor: #default!accessing!public! !
!NotifyAreaView class categoriesFor: #initialize!initializing!private! !
!NotifyAreaView class categoriesFor: #initializeNotificationMap!initializing!must not strip!private! !
!NotifyAreaView class categoriesFor: #onExit!initializing!private! !
!NotifyAreaView class categoriesFor: #onStartup!initializing!private! !
!NotifyAreaView class categoriesFor: #registerMessages!initializing!private! !
!NotifyAreaView class categoriesFor: #uninitialize!class hierarchy-removing!private! !

