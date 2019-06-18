"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #NEWHEADER
	instanceVariableNames: ''
	classVariableNames: '_OffsetOf_ResCount _OffsetOf_Reserved _OffsetOf_ResType'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
NEWHEADER guid: (GUID fromString: '{7f441582-bec8-4358-bccb-3626975e75be}')!
NEWHEADER addClassConstant: '_OffsetOf_ResCount' value: 16r4!
NEWHEADER addClassConstant: '_OffsetOf_Reserved' value: 16r0!
NEWHEADER addClassConstant: '_OffsetOf_ResType' value: 16r2!
NEWHEADER comment: ''!
!NEWHEADER categoriesForClass!Unclassified! !
!NEWHEADER methodsFor!

binresdirEntries
^self ResEntries: BINRESDIR!

fileresdirEntries
	^self ResEntries: FILERESDIR!

ResCount
	"Answer the receiver's ResCount field as a Smalltalk object."

	^(bytes wordAtOffset: 4)!

ResCount: anObject
	"Set the receiver's ResCount field to the value of anObject."

	bytes wordAtOffset: 4 put: anObject!

ResEntries: anEntryClass
^(StructureArray 
		fromAddress: self bytes yourAddress + self byteSize
		length: self ResCount
		elementClass: anEntryClass)!

resEntryBytes: aFILERESDIR 
	^ByteArray fromAddress: self bytes yourAddress + aFILERESDIR IconCursorOffset
		length: aFILERESDIR BytesInRes!

ResType
	"Answer the receiver's ResType field as a Smalltalk object."

	^(bytes wordAtOffset: 2)!

ResType: anObject
	"Set the receiver's ResType field to the value of anObject."

	bytes wordAtOffset: 2 put: anObject! !
!NEWHEADER categoriesFor: #binresdirEntries!public! !
!NEWHEADER categoriesFor: #fileresdirEntries!public! !
!NEWHEADER categoriesFor: #ResCount!**compiled accessors**!public! !
!NEWHEADER categoriesFor: #ResCount:!**compiled accessors**!public! !
!NEWHEADER categoriesFor: #ResEntries:!public! !
!NEWHEADER categoriesFor: #resEntryBytes:!public! !
!NEWHEADER categoriesFor: #ResType!**compiled accessors**!public! !
!NEWHEADER categoriesFor: #ResType:!**compiled accessors**!public! !

!NEWHEADER class methodsFor!

defineFields
	"Define the fields of the NEWHEADER structure (see http://msdn2.microsoft.com/en-us/library/ms648026(VS.85).aspx).
	
	NEWHEADER compileDefinition

	struct NEWHEADER { 
		WORD Reserved;
		WORD ResType;
		WORD ResCount;
	} NEWHEADER, *PNEWHEADER;
	"

	self
		defineField: #Reserved type: (FillerField byteSize: 2);
		defineField: #ResType type: WORDField new;
		defineField: #ResCount type: WORDField new! !
!NEWHEADER class categoriesFor: #defineFields!constants!public! !

