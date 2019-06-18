"Filed out from Dolphin Smalltalk 7"!

RESDIR subclass: #BINRESDIR
	instanceVariableNames: ''
	classVariableNames: '_OffsetOf_IconCursorId'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BINRESDIR guid: (GUID fromString: '{216201b6-b087-43c6-9286-44fa9c25fd1c}')!
BINRESDIR addClassConstant: '_OffsetOf_IconCursorId' value: 16rC!
BINRESDIR comment: ''!
!BINRESDIR categoriesForClass!Unclassified! !
!BINRESDIR methodsFor!

asBINRESDIR
	^self!

asFILERESDIR
	^FILERESDIR fromBytes: self bytes , #[0 0]!

IconCursorId
	"Answer the receiver's IconCursorId field as a Smalltalk object."

	^(bytes wordAtOffset: 12)!

IconCursorId: anObject
	"Set the receiver's IconCursorId field to the value of anObject."

	bytes wordAtOffset: 12 put: anObject! !
!BINRESDIR categoriesFor: #asBINRESDIR!public! !
!BINRESDIR categoriesFor: #asFILERESDIR!public! !
!BINRESDIR categoriesFor: #IconCursorId!**compiled accessors**!public! !
!BINRESDIR categoriesFor: #IconCursorId:!**compiled accessors**!public! !

!BINRESDIR class methodsFor!

defineFields
	"Define the fields of the BINRESDIR structure (see http://msdn2.microsoft.com/en-us/library/ms648026(VS.85).aspx).
	
	BINRESDIR compileDefinition

	typedef struct tagBINRESDIR {
		union {
			ICONRESDIR   Icon;
			CURSORDIR    Cursor;
		} ResInfo;
		WORD    Planes;
		WORD    BitCount;
		DWORD   BytesInRes;
		WORD  IconCursorId;
	} BINRESDIR;
	"

	super defineFields.
	self 
		defineField: #IconCursorId
		type: WORDField new
		offset: 12! !
!BINRESDIR class categoriesFor: #defineFields!constants!public! !

