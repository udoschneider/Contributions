"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #TT_NAME_RECORD
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TT_NAME_RECORD guid: (GUID fromString: '{e6aa3468-5148-46f1-898d-407ce2408c7a}')!
TT_NAME_RECORD comment: ''!
!TT_NAME_RECORD categoriesForClass!Unclassified! !
!TT_NAME_RECORD methodsFor!

uEncodingID
	"Answer the receiver's uEncodingID field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 2)!

uEncodingID: anObject
	"Set the receiver's uEncodingID field to the value of anObject."

	bytes bigEndianWordAtOffset: 2 put: anObject!

uLanguageID
	"Answer the receiver's uLanguageID field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 4)!

uLanguageID: anObject
	"Set the receiver's uLanguageID field to the value of anObject."

	bytes bigEndianWordAtOffset: 4 put: anObject!

uNameID
	"Answer the receiver's uNameID field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 6)!

uNameID: anObject
	"Set the receiver's uNameID field to the value of anObject."

	bytes bigEndianWordAtOffset: 6 put: anObject!

uPlatformID
	"Answer the receiver's uPlatformID field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 0)!

uPlatformID: anObject
	"Set the receiver's uPlatformID field to the value of anObject."

	bytes bigEndianWordAtOffset: 0 put: anObject!

uStringLength
	"Answer the receiver's uStringLength field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 8)!

uStringLength: anObject
	"Set the receiver's uStringLength field to the value of anObject."

	bytes bigEndianWordAtOffset: 8 put: anObject!

uStringOffset
	"Answer the receiver's uStringOffset field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 10)!

uStringOffset: anObject
	"Set the receiver's uStringOffset field to the value of anObject."

	bytes bigEndianWordAtOffset: 10 put: anObject! !
!TT_NAME_RECORD categoriesFor: #uEncodingID!public! !
!TT_NAME_RECORD categoriesFor: #uEncodingID:!public! !
!TT_NAME_RECORD categoriesFor: #uLanguageID!public! !
!TT_NAME_RECORD categoriesFor: #uLanguageID:!public! !
!TT_NAME_RECORD categoriesFor: #uNameID!public! !
!TT_NAME_RECORD categoriesFor: #uNameID:!public! !
!TT_NAME_RECORD categoriesFor: #uPlatformID!public! !
!TT_NAME_RECORD categoriesFor: #uPlatformID:!public! !
!TT_NAME_RECORD categoriesFor: #uStringLength!public! !
!TT_NAME_RECORD categoriesFor: #uStringLength:!public! !
!TT_NAME_RECORD categoriesFor: #uStringOffset!public! !
!TT_NAME_RECORD categoriesFor: #uStringOffset:!public! !

!TT_NAME_RECORD class methodsFor!

defineFields
	"Define the fields of the TT_NAME_RECORD structure.
		TT_NAME_RECORD compileDefinition
	
		typedef struct _tagTT_NAME_RECORD{
    USHORT uPlatformID;
    USHORT uEncodingID;
    USHORT uLanguageID;
    USHORT uNameID;
    USHORT uStringLength;
    USHORT uStringOffset; //from start of storage area

}TT_NAME_RECORD;
"

	self
		defineField: #uPlatformID type: BigEndianWORDField new;
		defineField: #uEncodingID type: BigEndianWORDField new;
		defineField: #uLanguageID type: BigEndianWORDField new;
		defineField: #uNameID type: BigEndianWORDField new;
		defineField: #uStringLength type: BigEndianWORDField new;
		defineField: #uStringOffset type: BigEndianWORDField new! !
!TT_NAME_RECORD class categoriesFor: #defineFields!public! !

