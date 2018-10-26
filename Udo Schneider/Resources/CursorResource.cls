"Filed out from Dolphin Smalltalk 7"!

IconResource subclass: #CursorResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CursorResource guid: (GUID fromString: '{bf354619-caf3-4052-a0d8-846c176ec041}')!
CursorResource comment: ''!
!CursorResource categoriesForClass!Unclassified! !
!CursorResource methodsFor!

setIdentifier: aResourceId locale: aLocale bytes: aByteArray 

	^super 
		setIdentifier: aResourceId
		locale: aLocale
		bytes: aByteArray! !
!CursorResource categoriesFor: #setIdentifier:locale:bytes:!*-in class package!private! !

!CursorResource class methodsFor!

resourceType
	^RT_CURSOR! !
!CursorResource class categoriesFor: #resourceType!*-in class package!private! !

