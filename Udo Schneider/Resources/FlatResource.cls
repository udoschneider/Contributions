"Filed out from Dolphin Smalltalk 7"!

AbstractWindowsResource subclass: #FlatResource
	instanceVariableNames: 'bytes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FlatResource guid: (GUID fromString: '{1b940b5e-37f7-4227-aab2-ff7f92ea75f9}')!
FlatResource comment: 'Flat resources which exist as a ByteStream. The bytes that got in are the bytes to bw written.'!
!FlatResource categoriesForClass!Unclassified! !
!FlatResource methodsFor!

packedResourceBytes
	^bytes!

setIdentifier: aResourceId locale: aLocale bytes: aByteArray 
	identifier := aResourceId.
	locale := aLocale.
	bytes := aByteArray! !
!FlatResource categoriesFor: #packedResourceBytes!*-in class package!private! !
!FlatResource categoriesFor: #setIdentifier:locale:bytes:!*-in class package!private! !

!FlatResource class methodsFor!

fromResourceBytes: bytes id: aResourceId locale: aLocale in: anExternalLibraryOrHandle 
	^(self new)
		setIdentifier: aResourceId
			locale: aLocale
			bytes: bytes;
		yourself!

fromResourceFileBytes: bytes in: aFilename 
	^self new 
		setIdentifier: (File splitFilenameFrom: aFilename)
		locale: nil
		bytes: bytes; yourself! !
!FlatResource class categoriesFor: #fromResourceBytes:id:locale:in:!*-in class package!private! !
!FlatResource class categoriesFor: #fromResourceFileBytes:in:!*-in class package!private! !

