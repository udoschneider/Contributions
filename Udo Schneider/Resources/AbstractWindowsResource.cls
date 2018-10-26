"Filed out from Dolphin Smalltalk 7"!

Object subclass: #AbstractWindowsResource
	instanceVariableNames: 'identifier locale'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
AbstractWindowsResource guid: (GUID fromString: '{b0524098-1dd4-47cd-8cba-ca51c72c94bd}')!
AbstractWindowsResource comment: ''!
!AbstractWindowsResource categoriesForClass!Unclassified! !
!AbstractWindowsResource methodsFor!

identifier
	^identifier!

identifier: aResourceId 
	identifier := aResourceId!

initialize
^self!

locale
	locale isNil ifTrue: [locale  := Locale dolphinResourceDefault].
^locale!

locale: aLocale
locale := aLocale!

packedResourceBytes
	self subclassResponsibility!

resourceType ^ self class resourceType!

updateResource: hUpdate 
	| resourceBytes |
	
	resourceBytes := self packedResourceBytes.
	(KernelLibrary default 
		updateResource: hUpdate
		lpType: self resourceType asResourceType asParameter
		lpName: (self identifier isString ifTrue: [self identifier asUppercase] ifFalse: [self identifier]) asResourceId asParameter
		wLanguage: self locale asParameter
		lpData: resourceBytes asParameter
		cbData: resourceBytes size) ifFalse: [KernelLibrary default systemError]! !
!AbstractWindowsResource categoriesFor: #identifier!accessing!public! !
!AbstractWindowsResource categoriesFor: #identifier:!*-in class package!accessing!public! !
!AbstractWindowsResource categoriesFor: #initialize!public! !
!AbstractWindowsResource categoriesFor: #locale!accessing!public! !
!AbstractWindowsResource categoriesFor: #locale:!accessing!public! !
!AbstractWindowsResource categoriesFor: #packedResourceBytes!private! !
!AbstractWindowsResource categoriesFor: #resourceType!public! !
!AbstractWindowsResource categoriesFor: #updateResource:!*-in class package!public! !

!AbstractWindowsResource class methodsFor!

extractResourceBytesType: aResourceType id: aResourceId locale: aLocale in: anExternalLibraryOrHandle 
	^aLocale isNil 
		ifTrue: 
			[ByteArray 
				fromId: aResourceId
				in: anExternalLibraryOrHandle
				type: aResourceType]
		ifFalse: 
			[ByteArray 
				fromId: aResourceId
				locale: aLocale
				in: anExternalLibraryOrHandle
				type: aResourceType]!

fromId: aResourceId in: anExternalLibraryOrHandle 
	^self 
		fromId: aResourceId locale: nil in: anExternalLibraryOrHandle!

fromId: aResourceId locale: aLocale in: anExternalLibraryOrHandle 
	^self 
		fromResourceBytes: (self 
				extractResourceBytesType: self resourceType
				id: aResourceId
				locale: aLocale
				in: anExternalLibraryOrHandle)
		id: aResourceId
		locale: aLocale
		in: anExternalLibraryOrHandle!

fromImage: anImage 
	^anImage instanceHandle isNil 
		ifTrue: 
			[anImage identifier notNil 
				ifTrue: 
					["File"
					self fromResourceFile: (anImage fileLocator localFileSpecFor: anImage identifier)]
				ifFalse: 
					["Memory"
					self fromResourceBytes: anImage resourceBytes id: anImage identifier locale: nil in: anImage instanceHandle
				]]
		ifFalse: 
			["Resource"
			self fromId: anImage identifier in: anImage instanceHandle]!

fromResourceBytes: bytes id: aResourceId locale: aLocale in: anExternalLibraryOrHandle 
	self subclassResponsibility!

fromResourceFile: aFilename 
	| stream bytes |
	stream := FileStream read: aFilename type: #binary.
	bytes := stream contents.
	stream close.
	^self fromResourceFileBytes: bytes in: aFilename!

fromResourceFileBytes: bytes in: aFilename 
	self subclassResponsibility!

id: aResourceId locale: aLocale 
	^(self new)
		identifier: aResourceId;
		locale: aLocale;
		yourself!

new
^super new initialize!

resourceType
^self subclassResponsibility! !
!AbstractWindowsResource class categoriesFor: #extractResourceBytesType:id:locale:in:!private! !
!AbstractWindowsResource class categoriesFor: #fromId:in:!public! !
!AbstractWindowsResource class categoriesFor: #fromId:locale:in:!public! !
!AbstractWindowsResource class categoriesFor: #fromImage:!public! !
!AbstractWindowsResource class categoriesFor: #fromResourceBytes:id:locale:in:!private! !
!AbstractWindowsResource class categoriesFor: #fromResourceFile:!public! !
!AbstractWindowsResource class categoriesFor: #fromResourceFileBytes:in:!private! !
!AbstractWindowsResource class categoriesFor: #id:locale:!public! !
!AbstractWindowsResource class categoriesFor: #new!public! !
!AbstractWindowsResource class categoriesFor: #resourceType!private! !

