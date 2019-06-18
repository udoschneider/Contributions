"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #CURSORDIR
	instanceVariableNames: ''
	classVariableNames: '_OffsetOf_Height _OffsetOf_Width'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CURSORDIR guid: (GUID fromString: '{7940710a-59d7-48d0-a46c-6b8e09836c90}')!
CURSORDIR addClassConstant: '_OffsetOf_Height' value: 16r2!
CURSORDIR addClassConstant: '_OffsetOf_Width' value: 16r0!
CURSORDIR comment: ''!
!CURSORDIR categoriesForClass!Unclassified! !
!CURSORDIR methodsFor!

extent
^self Width@self Height!

extent: aPoint

	self Width: aPoint x; Height: aPoint y.
	^aPoint!

Height
	"Answer the receiver's Height field as a Smalltalk object."

	^(bytes wordAtOffset: 2)!

Height: anObject
	"Set the receiver's Height field to the value of anObject."

	bytes wordAtOffset: 2 put: anObject!

Width
	"Answer the receiver's Width field as a Smalltalk object."

	^(bytes wordAtOffset: 0)!

Width: anObject
	"Set the receiver's Width field to the value of anObject."

	bytes wordAtOffset: 0 put: anObject! !
!CURSORDIR categoriesFor: #extent!public! !
!CURSORDIR categoriesFor: #extent:!public! !
!CURSORDIR categoriesFor: #Height!**compiled accessors**!public! !
!CURSORDIR categoriesFor: #Height:!**compiled accessors**!public! !
!CURSORDIR categoriesFor: #Width!**compiled accessors**!public! !
!CURSORDIR categoriesFor: #Width:!**compiled accessors**!public! !

!CURSORDIR class methodsFor!

defineFields
	"Define the fields of the CURSORDIR structure (see http://msdn2.microsoft.com/en-us/library/ms648011(VS.85).aspx).
	
	CURSORDIR compileDefinition

	struct CURSORDIR {
		WORD Width;
		WORD Height;
	} CURSORDIR;
	"

	self
		defineField: #Width type: WORDField new;
		defineField: #Height type: WORDField new.
	! !
!CURSORDIR class categoriesFor: #defineFields!constants!public! !

