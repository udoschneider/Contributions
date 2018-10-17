"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #CFBinaryPlistTrailer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CFBinaryPlistTrailer guid: (GUID fromString: '{5b3e39e6-b4a0-4a7e-bc01-b43cffb058bf}')!
CFBinaryPlistTrailer comment: ''!
!CFBinaryPlistTrailer categoriesForClass!Unclassified! !
!CFBinaryPlistTrailer methodsFor!

numObjects
	"Answer the receiver's numObjects field as a Smalltalk object."

	^(bytes bigEndianQwordAtOffset: 8)!

numObjects: anObject
	"Set the receiver's numObjects field to the value of anObject."

	bytes bigEndianQwordAtOffset: 8 put: anObject!

objectRefSize
	"Answer the receiver's objectRefSize field as a Smalltalk object."

	^(bytes byteAtOffset: 7)!

objectRefSize: anObject
	"Set the receiver's objectRefSize field to the value of anObject."

	bytes byteAtOffset: 7 put: anObject!

offsetIntSize
	"Answer the receiver's offsetIntSize field as a Smalltalk object."

	^(bytes byteAtOffset: 6)!

offsetIntSize: anObject
	"Set the receiver's offsetIntSize field to the value of anObject."

	bytes byteAtOffset: 6 put: anObject!

offsetTableOffset
	"Answer the receiver's offsetTableOffset field as a Smalltalk object."

	^(bytes bigEndianQwordAtOffset: 24)!

offsetTableOffset: anObject
	"Set the receiver's offsetTableOffset field to the value of anObject."

	bytes bigEndianQwordAtOffset: 24 put: anObject!

topObject
	"Answer the receiver's topObject field as a Smalltalk object."

	^(bytes bigEndianQwordAtOffset: 16)!

topObject: anObject
	"Set the receiver's topObject field to the value of anObject."

	bytes bigEndianQwordAtOffset: 16 put: anObject! !
!CFBinaryPlistTrailer categoriesFor: #numObjects!**compiled accessors**!public! !
!CFBinaryPlistTrailer categoriesFor: #numObjects:!**compiled accessors**!*-unreferenced selectors!public! !
!CFBinaryPlistTrailer categoriesFor: #objectRefSize!**compiled accessors**!public! !
!CFBinaryPlistTrailer categoriesFor: #objectRefSize:!**compiled accessors**!*-unreferenced selectors!public! !
!CFBinaryPlistTrailer categoriesFor: #offsetIntSize!**compiled accessors**!public! !
!CFBinaryPlistTrailer categoriesFor: #offsetIntSize:!**compiled accessors**!*-unreferenced selectors!public! !
!CFBinaryPlistTrailer categoriesFor: #offsetTableOffset!**compiled accessors**!public! !
!CFBinaryPlistTrailer categoriesFor: #offsetTableOffset:!**compiled accessors**!*-unreferenced selectors!public! !
!CFBinaryPlistTrailer categoriesFor: #topObject!**compiled accessors**!public! !
!CFBinaryPlistTrailer categoriesFor: #topObject:!**compiled accessors**!*-unreferenced selectors!public! !

!CFBinaryPlistTrailer class methodsFor!

defineFields
	"Define CFBinaryPlistTrailer structure
		CFBinaryPlistTrailer compileDefinition

	typedef struct {
		uint8_t	_unused[6];
		uint8_t	_offsetIntSize;
		uint8_t	_objectRefSize;
		uint64_t	_numObjects;
		uint64_t	_topObject;
		uint64_t	_offsetTableOffset;
	} CFBinaryPlistTrailer;"

	self
		defineField: #unused type: (FillerField byteSize: 6);
		defineField: #offsetIntSize type: BYTEField new;
		defineField: #objectRefSize type: BYTEField new;
		defineField: #numObjects type: BigEndianQWORDField new;
		defineField: #topObject type: BigEndianQWORDField new;
		defineField: #offsetTableOffset type: BigEndianQWORDField new! !
!CFBinaryPlistTrailer class categoriesFor: #defineFields!public! !

