"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #TT_NAME_TABLE_HEADER
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TT_NAME_TABLE_HEADER guid: (GUID fromString: '{1fcf03b7-ebc9-4532-9cfc-cdb4370d4de3}')!
TT_NAME_TABLE_HEADER comment: ''!
!TT_NAME_TABLE_HEADER categoriesForClass!Unclassified! !
!TT_NAME_TABLE_HEADER methodsFor!

uFSelector
	"Answer the receiver's uFSelector field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 0)!

uFSelector: anObject
	"Set the receiver's uFSelector field to the value of anObject."

	bytes bigEndianWordAtOffset: 0 put: anObject!

uNRCount
	"Answer the receiver's uNRCount field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 2)!

uNRCount: anObject
	"Set the receiver's uNRCount field to the value of anObject."

	bytes bigEndianWordAtOffset: 2 put: anObject!

uStorageOffset
	"Answer the receiver's uStorageOffset field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 4)!

uStorageOffset: anObject
	"Set the receiver's uStorageOffset field to the value of anObject."

	bytes bigEndianWordAtOffset: 4 put: anObject! !
!TT_NAME_TABLE_HEADER categoriesFor: #uFSelector!public! !
!TT_NAME_TABLE_HEADER categoriesFor: #uFSelector:!public! !
!TT_NAME_TABLE_HEADER categoriesFor: #uNRCount!public! !
!TT_NAME_TABLE_HEADER categoriesFor: #uNRCount:!public! !
!TT_NAME_TABLE_HEADER categoriesFor: #uStorageOffset!public! !
!TT_NAME_TABLE_HEADER categoriesFor: #uStorageOffset:!public! !

!TT_NAME_TABLE_HEADER class methodsFor!

defineFields
	"Define the fields of the TT_NAME_TABLE_HEADER structure.
		TT_NAME_TABLE_HEADER compileDefinition
	
typedef struct _tagTT_NAME_TABLE_HEADER{
    USHORT uFSelector; //format selector. Always 0

    USHORT uNRCount; //Name Records count

    USHORT uStorageOffset; //Offset for strings storage, 

                           //from start of the table

}TT_NAME_TABLE_HEADER;
"

	self
		defineField: #uFSelector type: BigEndianWORDField new;
		defineField: #uNRCount type: BigEndianWORDField new;
		defineField: #uStorageOffset type: BigEndianWORDField new! !
!TT_NAME_TABLE_HEADER class categoriesFor: #defineFields!public! !

