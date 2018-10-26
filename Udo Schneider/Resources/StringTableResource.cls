"Filed out from Dolphin Smalltalk 7"!

AbstractWindowsResource variableSubclass: #StringTableResource
	instanceVariableNames: 'entries'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
StringTableResource guid: (GUID fromString: '{64af8626-a821-4524-9079-030f90362fbd}')!
StringTableResource comment: ''!
!StringTableResource categoriesForClass!Unclassified! !
!StringTableResource methodsFor!

at: anInteger 
	^entries at: anInteger + 1!

at: anInteger put: aString 
	^entries at: anInteger + 1 put: aString!

packedResourceBytes
	| resourceBytesStream |
	resourceBytesStream := ReadWriteStream on: ByteArray new.
	entries do: 
			[:entry |
			| bytes |
			bytes := entry asUtf16String asByteArray.
			resourceBytesStream
				nextPutAll: ((ByteArray new: 2)
							wordAtOffset: 0 put: bytes size // 2;
							yourself);
				nextPutAll: bytes	"The ByteArray construct above could be replaced with #nextDWORDPut:"].
	^resourceBytesStream contents!

setIdentifier: aResourceId locale: aLocale entries: anArray 
	identifier := aResourceId.
	locale := aLocale.
	entries := anArray! !
!StringTableResource categoriesFor: #at:!public! !
!StringTableResource categoriesFor: #at:put:!*-in class package!public! !
!StringTableResource categoriesFor: #packedResourceBytes!*-in class package!private! !
!StringTableResource categoriesFor: #setIdentifier:locale:entries:!private! !

!StringTableResource class methodsFor!

empty
	^self new 
		setIdentifier: nil
		locale: nil
		entries: (Array new: 16 withAll: '')!

fromResourceBytes: bytes id: aResourceId locale: aLocale in: anExternalLibraryOrHandle 
	| entryBytesStream resourceByteStream entries |
	entryBytesStream := ReadWriteStream on: Array new.
	resourceByteStream := ReadStream on: bytes.
	[resourceByteStream atEnd] 
		whileFalse: [entryBytesStream nextPut: (resourceByteStream next: resourceByteStream nextWORD * 2)].
	entries := entryBytesStream contents 
				collect: [:each | (UnicodeString fromAddress: each yourAddress length: each size / 2) asString].
	^self new 
		setIdentifier: aResourceId
		locale: aLocale
		entries: entries !

fromResourceFileBytes: bytes in: aFilename 
	self shouldNotImplement!

resourceType
	^RT_STRING! !
!StringTableResource class categoriesFor: #empty!*-in class package!public! !
!StringTableResource class categoriesFor: #fromResourceBytes:id:locale:in:!*-in class package!private! !
!StringTableResource class categoriesFor: #fromResourceFileBytes:in:!private! !
!StringTableResource class categoriesFor: #resourceType!private! !

