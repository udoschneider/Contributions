"Filed out from Dolphin Smalltalk 7"!

RESDIR subclass: #FILERESDIR
	instanceVariableNames: ''
	classVariableNames: '_OffsetOf_IconCursorOffset'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FILERESDIR guid: (GUID fromString: '{191ff8b3-bf24-45b0-b789-38f672c7fb9b}')!
FILERESDIR addClassConstant: '_OffsetOf_IconCursorOffset' value: 16rC!
FILERESDIR comment: ''!
!FILERESDIR categoriesForClass!Unclassified! !
!FILERESDIR methodsFor!

asBINRESDIR
	^BINRESDIR fromBytes: self bytes!

asFILERESDIR
^self!

IconCursorOffset
	"Answer the receiver's IconCursorOffset field as a Smalltalk object."

	^(bytes dwordAtOffset: 12)!

IconCursorOffset: anObject
	"Set the receiver's IconCursorOffset field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject! !
!FILERESDIR categoriesFor: #asBINRESDIR!public! !
!FILERESDIR categoriesFor: #asFILERESDIR!public! !
!FILERESDIR categoriesFor: #IconCursorOffset!**compiled accessors**!public! !
!FILERESDIR categoriesFor: #IconCursorOffset:!**compiled accessors**!public! !

!FILERESDIR class methodsFor!

defineFields
	"Define the fields of the FILERESDIR structure.
	
	FILERESDIR compileDefinition

	typedef struct tagFILERESDIR {
		union {
			ICONRESDIR   Icon;
			CURSORDIR    Cursor;
		} ResInfo;
		WORD    Planes;
		WORD    BitCount;
		DWORD   BytesInRes;
		DWORD  IconCursorOffset;
	} FILERESDIR;
	"

	super defineFields.
	self 
		defineField: #IconCursorOffset
		type: DWORDField new
		offset: 12! !
!FILERESDIR class categoriesFor: #defineFields!constants!public! !

