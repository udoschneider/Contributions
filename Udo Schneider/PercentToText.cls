"Filed out from Dolphin Smalltalk 7"!

NumberToText subclass: #PercentToText
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PercentToText guid: (GUID fromString: '{ad11b882-626d-4b05-879c-5b951e799fcf}')!
PercentToText comment: ''!
!PercentToText categoriesForClass!TypeConverter-Text! !
!PercentToText methodsFor!

leftToRight: aNumber 
	^(super leftToRight: aNumber * 100) , '%'!

rightToLeft: aString 
	^(aString includes: $%) 
		ifTrue: [(super rightToLeft: (aString reject: [:each | each = $%])) / 100]
		ifFalse: [super rightToLeft: aString]! !
!PercentToText categoriesFor: #leftToRight:!private! !
!PercentToText categoriesFor: #rightToLeft:!private! !

