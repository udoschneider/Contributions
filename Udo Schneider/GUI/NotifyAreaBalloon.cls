"Filed out from Dolphin Smalltalk 7"!

MessageBoxAbstract subclass: #NotifyAreaBalloon
	instanceVariableNames: 'iconId icon timeout'
	classVariableNames: 'IconIds'
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
NotifyAreaBalloon guid: (GUID fromString: '{ac0eb94f-5b9e-4f18-8e9e-b2819d779d5c}')!
NotifyAreaBalloon comment: ''!
!NotifyAreaBalloon categoriesForClass!Kernel-Objects! !
!NotifyAreaBalloon methodsFor!

basicOpen
	owner 
		showBalloonIcon:  icon
		title: self caption
		message: self text
		flags: self iconStyleFlag
		timeout: self timeout
		playSound: false!

icon: anIcon 
	"Set the receiver's icon to be the argument, anIcon. The icon MUST have been loaded from
	resources since the message box does not use the icon directly, but rather it uses its ID
	and module handle to load it again.. Also the icon only seems to be displayed if the
	resource id is an integer."

	icon := anIcon.
	self iconStyle: (icon isNil ifTrue: [#notify] ifFalse: [#user])!

iconStyle
	"Answer the receiver's symbolic icon style name."

	^IconIds keyAtValue: self iconStyleFlag!

iconStyle: aSymbol 
	"Set the receiver's icon style to that named by the argument."

	
	self iconStyleFlag: (IconIds at: aSymbol)!

iconStyleFlag
	"Private - Answer the icon style bits from the receiver's style mask."


	^IconIds at: iconId ifAbsent: [0]!

iconStyleFlag: anInteger 
	"Private - Set the receiver's icon to be that named by the argument."

	iconId := IconIds keyAtValue: anInteger ifAbsent: [0]!

initialize
super initialize.
	iconId := 0.

	timeout := 0.
!

open
	self basicOpen.
	^#ok!

timeout
	"Answer the number of milliseconds for which the bubble will be displayed.
	If <= 0, then the bubble is displayed until clicked or otherwise closed explicitly."

	^timeout!

timeout: anInteger 
	timeout := anInteger! !
!NotifyAreaBalloon categoriesFor: #basicOpen!private! !
!NotifyAreaBalloon categoriesFor: #icon:!accessing!public! !
!NotifyAreaBalloon categoriesFor: #iconStyle!accessing-styles!public! !
!NotifyAreaBalloon categoriesFor: #iconStyle:!accessing-styles!public! !
!NotifyAreaBalloon categoriesFor: #iconStyleFlag!accessing-styles!private! !
!NotifyAreaBalloon categoriesFor: #iconStyleFlag:!accessing-styles!private! !
!NotifyAreaBalloon categoriesFor: #initialize!initializing!private! !
!NotifyAreaBalloon categoriesFor: #open!public!realizing/unrealizing! !
!NotifyAreaBalloon categoriesFor: #timeout!accessing!public! !
!NotifyAreaBalloon categoriesFor: #timeout:!accessing!public! !

!NotifyAreaBalloon class methodsFor!

icon
	^Tooltip icon!

initialize
	"Private - Initialize the class variables of the receiver:
		self initialize
	"

	IconIds := (IdentityDictionary new)
				at: #none put: NIIF_NONE ;
				at: #error put: NIIF_ERROR ;
				
				at: #warning put: NIIF_WARNING ;
			
				at: #notify put: NIIF_INFO ;
				at: #user put: NIIF_USER ;
				shrink;
				yourself! !
!NotifyAreaBalloon class categoriesFor: #icon!constants!development!public! !
!NotifyAreaBalloon class categoriesFor: #initialize!private! !

