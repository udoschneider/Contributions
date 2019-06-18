"Filed out from Dolphin Smalltalk 7"!

OFCChartDecoration subclass: #OFCStyledText
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCStyledText guid: (GUID fromString: '{18590008-9393-4c91-b8ae-785175839105}')!
OFCStyledText comment: ''!
!OFCStyledText categoriesForClass!Unclassified! !
!OFCStyledText methodsFor!

style: aCssString 
	self 
		propertiesAt: #style
		put: aCssString!

text: aString 
	self 
		propertiesAt: #text
		put: aString! !
!OFCStyledText categoriesFor: #style:!accessing!public! !
!OFCStyledText categoriesFor: #text:!accessing!public! !

!OFCStyledText class methodsFor!

icon
^String icon! !
!OFCStyledText class categoriesFor: #icon!public! !

