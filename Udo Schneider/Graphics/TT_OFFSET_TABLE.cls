"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #TT_OFFSET_TABLE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TT_OFFSET_TABLE guid: (GUID fromString: '{cc003cbb-20e1-46c6-8191-2bb134185a1f}')!
TT_OFFSET_TABLE comment: ''!
!TT_OFFSET_TABLE categoriesForClass!Unclassified! !
!TT_OFFSET_TABLE methodsFor!

uEntrySelector
	"Answer the receiver's uEntrySelector field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 8)!

uEntrySelector: anObject
	"Set the receiver's uEntrySelector field to the value of anObject."

	bytes bigEndianWordAtOffset: 8 put: anObject!

uMajorVersion
	"Answer the receiver's uMajorVersion field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 0)!

uMajorVersion: anObject
	"Set the receiver's uMajorVersion field to the value of anObject."

	bytes bigEndianWordAtOffset: 0 put: anObject!

uMinorVersion
	"Answer the receiver's uMinorVersion field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 2)!

uMinorVersion: anObject
	"Set the receiver's uMinorVersion field to the value of anObject."

	bytes bigEndianWordAtOffset: 2 put: anObject!

uNumOfTables
	"Answer the receiver's uNumOfTables field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 4)!

uNumOfTables: anObject
	"Set the receiver's uNumOfTables field to the value of anObject."

	bytes bigEndianWordAtOffset: 4 put: anObject!

uRangeShift
	"Answer the receiver's uRangeShift field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 10)!

uRangeShift: anObject
	"Set the receiver's uRangeShift field to the value of anObject."

	bytes bigEndianWordAtOffset: 10 put: anObject!

uSearchRange
	"Answer the receiver's uSearchRange field as a Smalltalk object."

	^(bytes bigEndianWordAtOffset: 6)!

uSearchRange: anObject
	"Set the receiver's uSearchRange field to the value of anObject."

	bytes bigEndianWordAtOffset: 6 put: anObject! !
!TT_OFFSET_TABLE categoriesFor: #uEntrySelector!public! !
!TT_OFFSET_TABLE categoriesFor: #uEntrySelector:!public! !
!TT_OFFSET_TABLE categoriesFor: #uMajorVersion!public! !
!TT_OFFSET_TABLE categoriesFor: #uMajorVersion:!public! !
!TT_OFFSET_TABLE categoriesFor: #uMinorVersion!public! !
!TT_OFFSET_TABLE categoriesFor: #uMinorVersion:!public! !
!TT_OFFSET_TABLE categoriesFor: #uNumOfTables!public! !
!TT_OFFSET_TABLE categoriesFor: #uNumOfTables:!public! !
!TT_OFFSET_TABLE categoriesFor: #uRangeShift!public! !
!TT_OFFSET_TABLE categoriesFor: #uRangeShift:!public! !
!TT_OFFSET_TABLE categoriesFor: #uSearchRange!public! !
!TT_OFFSET_TABLE categoriesFor: #uSearchRange:!public! !

!TT_OFFSET_TABLE class methodsFor!

defineFields
	"Define the fields of the TT_OFFSET_TABLE structure.
		TT_OFFSET_TABLE compileDefinition
	
typedef struct _tagTT_OFFSET_TABLE{
    USHORT uMajorVersion;
    USHORT uMinorVersion;
    USHORT uNumOfTables;
    USHORT uSearchRange;
    USHORT uEntrySelector;
    USHORT uRangeShift;
}TT_OFFSET_TABLE;
"

	self
		defineField: #uMajorVersion type: BigEndianWORDField new;
		defineField: #uMinorVersion type: BigEndianWORDField new;
		defineField: #uNumOfTables type: BigEndianWORDField new;
		defineField: #uSearchRange type: BigEndianWORDField new;
		defineField: #uEntrySelector type: BigEndianWORDField new;
		defineField: #uRangeShift type: BigEndianWORDField new! !
!TT_OFFSET_TABLE class categoriesFor: #defineFields!public! !

