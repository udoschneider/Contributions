"Filed out from Dolphin Smalltalk 7"!

NumberToText subclass: #FloatToText
	instanceVariableNames: 'decimalPlaces'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FloatToText guid: (GUID fromString: '{9dff7317-c813-4845-b150-f7ce87fd3aad}')!
FloatToText comment: ''!
!FloatToText categoriesForClass!TypeConverter-Text! !
!FloatToText methodsFor!

decimalPlaces: anInteger
	decimalPlaces := anInteger!

initialize
	super initialize.
	decimalPlaces := 6!

leftToRight: aFloat 
	^super leftToRight: (self toDecimalPlaces: aFloat)!

rightToLeft: aString 
	^self toDecimalPlaces: (super rightToLeft: aString)!

toDecimalPlaces: aNumber 
	^ ((aNumber asFloat
		* (10 raisedToInteger: decimalPlaces)) rounded
		/ (10 raisedToInteger: decimalPlaces)) asFloat! !
!FloatToText categoriesFor: #decimalPlaces:!public! !
!FloatToText categoriesFor: #initialize!private! !
!FloatToText categoriesFor: #leftToRight:!private! !
!FloatToText categoriesFor: #rightToLeft:!private! !
!FloatToText categoriesFor: #toDecimalPlaces:!public! !

!FloatToText class methodsFor!

decimalPlaces: anInteger 
	^ self new decimalPlaces: anInteger;
		 yourself! !
!FloatToText class categoriesFor: #decimalPlaces:!public! !

