"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #TT_TABLE_DIRECTORY
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TT_TABLE_DIRECTORY guid: (GUID fromString: '{4a277749-634a-4910-96c1-d619fa9a55fa}')!
TT_TABLE_DIRECTORY comment: ''!
!TT_TABLE_DIRECTORY categoriesForClass!Unclassified! !
!TT_TABLE_DIRECTORY methodsFor!

szTag
	"Answer the receiver's szTag field as a Smalltalk object."

	^String fromAddress: (bytes yourAddress) length: 4!

szTag: anObject
	"Set the receiver's szTag field to the value of anObject."

	| size |
	size := anObject byteSize - 1 min: (4 * 1).
	anObject replaceBytesOf: bytes from: 1 to: size startingAt: 1!

uCheckSum
	"Answer the receiver's uCheckSum field as a Smalltalk object."

	^(bytes bigEndianDwordAtOffset: 4)!

uCheckSum: anObject
	"Set the receiver's uCheckSum field to the value of anObject."

	bytes bigEndianDwordAtOffset: 4 put: anObject!

uLength
	"Answer the receiver's uLength field as a Smalltalk object."

	^(bytes bigEndianDwordAtOffset: 12)!

uLength: anObject
	"Set the receiver's uLength field to the value of anObject."

	bytes bigEndianDwordAtOffset: 12 put: anObject!

uOffset
	"Answer the receiver's uOffset field as a Smalltalk object."

	^(bytes bigEndianDwordAtOffset: 8)!

uOffset: anObject
	"Set the receiver's uOffset field to the value of anObject."

	bytes bigEndianDwordAtOffset: 8 put: anObject! !
!TT_TABLE_DIRECTORY categoriesFor: #szTag!public! !
!TT_TABLE_DIRECTORY categoriesFor: #szTag:!public! !
!TT_TABLE_DIRECTORY categoriesFor: #uCheckSum!public! !
!TT_TABLE_DIRECTORY categoriesFor: #uCheckSum:!public! !
!TT_TABLE_DIRECTORY categoriesFor: #uLength!public! !
!TT_TABLE_DIRECTORY categoriesFor: #uLength:!public! !
!TT_TABLE_DIRECTORY categoriesFor: #uOffset!public! !
!TT_TABLE_DIRECTORY categoriesFor: #uOffset:!public! !

!TT_TABLE_DIRECTORY class methodsFor!

defineFields
	"Define the fields of the TT_TABLE_DIRECTORY structure.
		TT_TABLE_DIRECTORY compileDefinition
	
typedef struct _tagTT_TABLE_DIRECTORY{
    char szTag[4]; //table name

    ULONG uCheckSum; //Check sum

    ULONG uOffset; //Offset from beginning of file

    ULONG uLength; //length of the table in bytes

}TT_TABLE_DIRECTORY;
"

	self
		defineField: #szTag type: (ArrayField type: String length: 4);
		defineField: #uCheckSum type: BigEndianDWORDField new;
		defineField: #uOffset type: BigEndianDWORDField new;
		defineField: #uLength type: BigEndianDWORDField new! !
!TT_TABLE_DIRECTORY class categoriesFor: #defineFields!public! !

