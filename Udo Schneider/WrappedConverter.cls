"Filed out from Dolphin Smalltalk 7"!

TypeConverter subclass: #WrappedConverter
	instanceVariableNames: 'wrapper wrappee'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
WrappedConverter guid: (GUID fromString: '{53a2ed45-1e0a-4f98-8429-075f3a907c00}')!
WrappedConverter comment: ''!
!WrappedConverter categoriesForClass!TypeConverter-Base! !
!WrappedConverter methodsFor!

leftToRight: anObject 
	^wrappee convertFromLeftToRight: (wrapper convertFromLeftToRight: anObject)!

rightToLeft: anObject 
	^wrapper rightToLeft: (wrappee rightToLeft: anObject)!

setWrapper: aWrapper wrappee: aWrappee 
	wrapper := aWrapper.
	wrappee := aWrappee! !
!WrappedConverter categoriesFor: #leftToRight:!private! !
!WrappedConverter categoriesFor: #rightToLeft:!private! !
!WrappedConverter categoriesFor: #setWrapper:wrappee:!private! !

!WrappedConverter class methodsFor!

wrapper: aWrapper wrappee: aWrappee 
	^ self new
		setWrapper: aWrapper
			wrappee: aWrappee;
		yourself! !
!WrappedConverter class categoriesFor: #wrapper:wrappee:!public! !

