"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #ICONRESDIR
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ICONRESDIR guid: (GUID fromString: '{f3f3ae91-4e08-4535-b553-06b5bf11be53}')!
ICONRESDIR comment: ''!
!ICONRESDIR categoriesForClass!Unclassified! !
!ICONRESDIR methodsFor!

ColorCount
	"Answer the receiver's ColorCount field as a Smalltalk object."

	^(bytes byteAtOffset: 2)!

ColorCount: anObject
	"Set the receiver's ColorCount field to the value of anObject."

	bytes byteAtOffset: 2 put: anObject!

extent
^self Width@self Height!

extent: aPoint

	self Width: aPoint x; Height: aPoint y.
	^aPoint!

Height
	"Answer the receiver's Height field as a Smalltalk object."

	^(bytes byteAtOffset: 1)!

Height: anObject
	"Set the receiver's Height field to the value of anObject."

	bytes byteAtOffset: 1 put: anObject!

Width
	"Answer the receiver's Width field as a Smalltalk object."

	^(bytes byteAtOffset: 0)!

Width: anObject
	"Set the receiver's Width field to the value of anObject."

	bytes byteAtOffset: 0 put: anObject! !
!ICONRESDIR categoriesFor: #ColorCount!**compiled accessors**!public! !
!ICONRESDIR categoriesFor: #ColorCount:!**compiled accessors**!public! !
!ICONRESDIR categoriesFor: #extent!public! !
!ICONRESDIR categoriesFor: #extent:!public! !
!ICONRESDIR categoriesFor: #Height!**compiled accessors**!public! !
!ICONRESDIR categoriesFor: #Height:!**compiled accessors**!public! !
!ICONRESDIR categoriesFor: #Width!**compiled accessors**!public! !
!ICONRESDIR categoriesFor: #Width:!**compiled accessors**!public! !

!ICONRESDIR class methodsFor!

defineFields
	"Define the fields of the ICONRESDIR structure (see http://msdn2.microsoft.com/en-us/library/ms648016(VS.85).aspx).
	
	ICONRESDIR compileDefinition

	struct ICONRESDIR { 
		BYTE Width;
		BYTE Height;
		BYTE ColorCount;
		BYTE reserved;
	} ICONRESDIR; 
	"

	self
		defineField: #Width type: BYTEField new;
		defineField: #Height type: BYTEField new;
		defineField: #ColorCount type: BYTEField new;
		defineField: #reserved type: (FillerField byteSize: 1)! !
!ICONRESDIR class categoriesFor: #defineFields!constants!public! !

