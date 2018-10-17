"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #NotifyShellExample
	instanceVariableNames: 'notifyIcon'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NotifyShellExample guid: (GUID fromString: '{8e0528a6-54f0-4368-8d83-83cc78af605d}')!
NotifyShellExample comment: 'This NotifyShell exmple demonstrates how to react to the different events triggered by the traybar icon.'!
!NotifyShellExample categoriesForClass!Unclassified! !
!NotifyShellExample methodsFor!

animateTrayIcon
self logMessage: 'animateTrayIcon'.
	(Class allBehaviors collect: [:each | each icon]) asSet do: [:eachIcon | notifyIcon icon: eachIcon].
	notifyIcon icon: self icon!

createComponents
	"Create the presenters contained by the receiver"

	super createComponents.
	self
		add: TextPresenter new name: 'log'
	!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	self 
		when: #viewMinimized
		send: #minimizeToTray
		to: self.
	notifyIcon
		when: #leftButtonDoubleClicked
			send: #restoreFromTray
			to: self;
		when: #rightButtonPressed
			send: #showTrayMenu
			to: self;
		when: #leftButtonPressed
			send: #animateTrayIcon
			to: self!

logMessage: aString 
	| logModel |
	logModel := self presenterNamed: 'log'.
	logModel value: logModel value , '
' , aString!

minimizeToTray
	self logMessage:  'minimizeToTray'.
	self hide.
	notifyIcon show!

onViewDestroyed
self logMessage: 'onViewDestroyed'.
	notifyIcon hide.
	notifyIcon := nil.
	super onViewDestroyed!

onViewOpened

	notifyIcon := NotifyAreaIcon icon: self icon message: self view caption.
	self logMessage: 'onViewOpened: Registered NotifyIcon'.
	super onViewOpened!

restoreFromTray
self logMessage: 'restoreFromTray'.
	notifyIcon hide.
	self show!

showTrayMenu
self logMessage: 'showTrayMenu'.
	self view contextMenu showIn: self view position: Cursor position.

	"Allow the command selected from the menu to be dispatched before returning"
	SessionManager inputState pumpMessages! !
!NotifyShellExample categoriesFor: #animateTrayIcon!public! !
!NotifyShellExample categoriesFor: #createComponents!initializing!public! !
!NotifyShellExample categoriesFor: #createSchematicWiring!public! !
!NotifyShellExample categoriesFor: #logMessage:!helpers!private! !
!NotifyShellExample categoriesFor: #minimizeToTray!public! !
!NotifyShellExample categoriesFor: #onViewDestroyed!public! !
!NotifyShellExample categoriesFor: #onViewOpened!public! !
!NotifyShellExample categoriesFor: #restoreFromTray!public! !
!NotifyShellExample categoriesFor: #showTrayMenu!public! !

!NotifyShellExample class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 0 551 265030 4 ##(Smalltalk.Menu)  0 16 98 3 984134 2 ##(Smalltalk.CommandMenuItem)  1 1180998 4 ##(Smalltalk.CommandDescription)  8 #noCommand 8 'Menu Item1' 1 1 0 0 0 530 0 16 98 1 578 1 610 640 8 'Sub Menu Item' 1 1 0 0 0 8 'SubMenu' 0 134217729 0 0 0 0 0 578 1 610 8 #restoreFromTray 8 'Restore from Tray' 1 1 0 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 416 788230 ##(Smalltalk.BorderLayout)  1 1 0 0 0 0 410 8 ##(Smalltalk.MultilineTextEdit)  98 16 0 416 98 2 8 1143017796 1025 880 0 482 8 4278190080 0 7 0 0 0 880 0 8 4294903091 852486 ##(Smalltalk.NullConverter)  0 0 9 983302 ##(Smalltalk.MessageSequence)  202 208 98 3 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  1 1 1170 1185 693 880 1106 8 #selectionRange: 98 1 525062 ##(Smalltalk.Interval)  3 1 3 880 1106 8 #isTextModified: 98 1 32 880 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 80 2 0 0 90 1 0 0] 98 0 1170 193 193 0 27 234 256 98 2 880 8 'log' 0 461638 4 ##(Smalltalk.MenuBar)  0 16 98 1 530 0 16 98 1 578 1 610 8 #exit 8 'Close' 17639 1 0 0 0 8 'File' 0 134217729 0 0 14507 0 0 8 '' 0 134217729 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1042 202 208 98 4 1106 1136 98 2 1170 3839 21 1170 1201 801 416 1106 8 #contextMenu: 98 1 544 416 1106 8 #text: 98 1 8 'NotifyShell Demo' 416 1106 8 #updateMenuBar 98 0 416 1346 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 215 9 0 0 154 1 0 0] 98 1 880 1408 0 27 )! !
!NotifyShellExample class categoriesFor: #resource_Default_view!public!resources-views! !

