"Filed out from Dolphin Smalltalk 7"!

AbstractToTextConverter subclass: #GenericToText
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GenericToText guid: (GUID fromString: '{c0349179-4c0e-4065-94f3-edd66636b362}')!
GenericToText comment: ''!
!GenericToText categoriesForClass!TypeConverter-Text! !
!GenericToText methodsFor!

leftToRight: anObject 
	^anObject displayString!

rightToLeft: aString 
	^Object fromString: aString! !
!GenericToText categoriesFor: #leftToRight:!private! !
!GenericToText categoriesFor: #rightToLeft:!private! !

