"Filed out from Dolphin Smalltalk 7"!

FlatResource subclass: #BitmapResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BitmapResource guid: (GUID fromString: '{1ef56875-d021-457d-8930-7dc09c1028bc}')!
BitmapResource comment: ''!
!BitmapResource categoriesForClass!Unclassified! !
!BitmapResource class methodsFor!

fromResourceFileBytes: bytes in: aFilename 
	^self new 
		setIdentifier: (File splitFilenameFrom: aFilename)
		locale: nil
		bytes: (bytes copyFrom: 15)!

resourceType
	^RT_BITMAP! !
!BitmapResource class categoriesFor: #fromResourceFileBytes:in:!private! !
!BitmapResource class categoriesFor: #resourceType!private! !

