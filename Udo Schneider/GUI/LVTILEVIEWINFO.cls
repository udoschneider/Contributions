"Filed out from Dolphin Smalltalk 7"!

Win32Structure subclass: #LVTILEVIEWINFO
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LVTILEVIEWINFO guid: (GUID fromString: '{dfbb203d-1891-4e18-9b9d-1f8f94c6f941}')!
LVTILEVIEWINFO comment: ''!
!LVTILEVIEWINFO categoriesForClass!Unclassified! !
!LVTILEVIEWINFO methodsFor!

cbSize
	"Answer the receiver's cbSize field as a Smalltalk object."

	^(bytes dwordAtOffset: 0)!

cbSize: anObject
	"Set the receiver's cbSize field to the value of anObject."

	bytes dwordAtOffset: 0 put: anObject!

cLines
	"Answer the receiver's cLines field as a Smalltalk object."

	^(bytes sdwordAtOffset: 20)!

cLines: anObject
	"Set the receiver's cLines field to the value of anObject."

	bytes sdwordAtOffset: 20 put: anObject!

dwFlags
	"Answer the receiver's dwFlags field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

dwFlags: anObject
	"Set the receiver's dwFlags field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

dwMask
	"Answer the receiver's dwMask field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

dwMask: anObject
	"Set the receiver's dwMask field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

rcLabelMargin
	"Answer the receiver's rcLabelMargin field as a Smalltalk object."

	^RECT fromAddress: (bytes yourAddress + 24)!

rcLabelMargin: anObject
	"Set the receiver's rcLabelMargin field to the value of anObject."

	anObject replaceBytesOf: bytes from: 25 to: 40 startingAt: 1!

sizeTile
	"Answer the receiver's sizeTile field as a Smalltalk object."

	^SIZE fromAddress: (bytes yourAddress + 12)!

sizeTile: anObject
	"Set the receiver's sizeTile field to the value of anObject."

	anObject replaceBytesOf: bytes from: 13 to: 20 startingAt: 1! !
!LVTILEVIEWINFO categoriesFor: #cbSize!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #cbSize:!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #cLines!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #cLines:!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #dwFlags!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #dwFlags:!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #dwMask!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #dwMask:!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #rcLabelMargin!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #rcLabelMargin:!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #sizeTile!**compiled accessors**!public! !
!LVTILEVIEWINFO categoriesFor: #sizeTile:!**compiled accessors**!public! !

!LVTILEVIEWINFO class methodsFor!

defineFields
	"Define the fields of the Win32 LVTILEVIEWINFO structure
		self compileDefinition
		
		typedef struct LVTILEVIEWINFO {
    UINT cbSize;
    DWORD dwMask;
    DWORD dwFlags;
    SIZE sizeTile;
    int cLines;
    RECT rcLabelMargin;
} LVTILEVIEWINFO, *PLVTILEVIEWINFO;
	"

	self
		defineField: #cbSize type: DWORDField new;
		defineField: #dwMask type: DWORDField new;
		defineField: #dwFlags type: DWORDField new;
		defineField: #sizeTile type: (StructureField type: SIZE);
		defineField: #cLines type: SDWORDField new;
		defineField: #rcLabelMargin type: (StructureField type: RECT)! !
!LVTILEVIEWINFO class categoriesFor: #defineFields!initializing!public! !

