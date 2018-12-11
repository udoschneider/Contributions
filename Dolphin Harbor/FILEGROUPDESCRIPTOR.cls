"Filed out from Dolphin Smalltalk 7"!

OLEStructure subclass: #FILEGROUPDESCRIPTOR
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FILEGROUPDESCRIPTOR guid: (GUID fromString: '{7faedab6-a88c-4dd5-990c-3f9bc8b7d1d0}')!
FILEGROUPDESCRIPTOR comment: 'FILEGROUPDESCRIPTOR is an <ExternalStructure> that wraps the Windows FILEGROUPDESCRIPTOR structure and defines the CF_FILEGROUPDESCRIPTOR clipboard format. 

- cItems, the number of elements in fgd. 
- fgd, an array of FILEDESCRIPTOR structures that contain the file information. 
'!
!FILEGROUPDESCRIPTOR categoriesForClass!Unclassified! !
!FILEGROUPDESCRIPTOR methodsFor!

cItems
	"Answer the receiver's cItems field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

cItems: anObject
	"Set the receiver's cItems field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

fgd
	"Answer the receiver's fgd field as a Smalltalk object."

	^StructureArray fromAddress: (bytes yourAddress + 4) length: self cItems elementClass: FILEDESCRIPTOR!

fgd: anObject
	"Set the receiver's fgd field to the value of anObject."

	| size |
	size := anObject byteSize min: (self cItems * 0).
	anObject replaceBytesOf: bytes from: 5 to: 4 + size startingAt: 1! !
!FILEGROUPDESCRIPTOR categoriesFor: #cItems!**compiled accessors**!public! !
!FILEGROUPDESCRIPTOR categoriesFor: #cItems:!**compiled accessors**!public! !
!FILEGROUPDESCRIPTOR categoriesFor: #fgd!**compiled accessors**!public! !
!FILEGROUPDESCRIPTOR categoriesFor: #fgd:!**compiled accessors**!public! !

!FILEGROUPDESCRIPTOR class methodsFor!

defineFields
	"Define the Win32 FILEGROUPDESCRIPTOR structure
		FILEGROUPDESCRIPTOR compileDefinition

	typedef struct _FILEGROUPDESCRIPTOR {
	    UINT cItems; 
	    FILEDESCRIPTOR fgd[1]; 
	} FILEGROUPDESCRIPTOR, * LPFILEGROUPDESCRIPTOR; 


	N.B. #fgd is really a variable length array 
	"

	self
		defineField: #cItems type: DWORDField new;
		defineField: #fgd type: (VariableStructureArrayField type: FILEDESCRIPTOR length: #cItems)! !
!FILEGROUPDESCRIPTOR class categoriesFor: #defineFields!initializing!public! !

