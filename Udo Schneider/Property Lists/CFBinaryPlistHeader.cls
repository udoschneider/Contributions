"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #CFBinaryPlistHeader
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CFBinaryPlistHeader guid: (GUID fromString: '{5765f8e2-1898-4061-aa2c-057b519b6157}')!
CFBinaryPlistHeader comment: ''!
!CFBinaryPlistHeader categoriesForClass!Unclassified! !
!CFBinaryPlistHeader methodsFor!

magic
	"Answer the receiver's magic field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress) length: 6!

version
	"Answer the receiver's version field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress + 6) length: 2! !
!CFBinaryPlistHeader categoriesFor: #magic!**compiled accessors**!public! !
!CFBinaryPlistHeader categoriesFor: #version!**compiled accessors**!public! !

!CFBinaryPlistHeader class methodsFor!

defineFields
	"Define CFBinaryPlistHeader structure
		CFBinaryPlistHeader compileDefinition

	typedef struct {
		uint8_t	_magic[6];
		uint8_t	_version[2];
	} CFBinaryPlistHeader;"

	self
		defineField: #magic type: (ArrayField type: String length: 6) beReadOnly;
		defineField: #version type:  (ArrayField type: String length: 2) beReadOnly! !
!CFBinaryPlistHeader class categoriesFor: #defineFields!public! !

