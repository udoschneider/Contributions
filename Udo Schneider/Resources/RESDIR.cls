"Filed out from Dolphin Smalltalk 7"!

ExternalStructure subclass: #RESDIR
	instanceVariableNames: ''
	classVariableNames: '_OffsetOf_BitCount _OffsetOf_BytesInRes _OffsetOf_Cursor _OffsetOf_Icon _OffsetOf_Planes _OffsetOf_xHotSpot _OffsetOf_yHotSpot'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RESDIR guid: (GUID fromString: '{e41f6b74-0391-4600-b8a1-6895390ba0fe}')!
RESDIR addClassConstant: '_OffsetOf_BitCount' value: 16r6!
RESDIR addClassConstant: '_OffsetOf_BytesInRes' value: 16r8!
RESDIR addClassConstant: '_OffsetOf_Cursor' value: 16r0!
RESDIR addClassConstant: '_OffsetOf_Icon' value: 16r0!
RESDIR addClassConstant: '_OffsetOf_Planes' value: 16r4!
RESDIR addClassConstant: '_OffsetOf_xHotSpot' value: 16r4!
RESDIR addClassConstant: '_OffsetOf_yHotSpot' value: 16r6!
RESDIR comment: ''!
!RESDIR categoriesForClass!Unclassified! !
!RESDIR methodsFor!

<= aRESDIR 
self BitCount = aRESDIR BitCount ifFalse: [^self BitCount <= aRESDIR BitCount].
	^self Icon Width > aRESDIR Icon Width
	!

asBINRESDIR
	self subclassResponsibility!

asFILERESDIR
	self subclassResponsibility!

BitCount
	"Answer the receiver's BitCount field as a Smalltalk object."

	^(bytes wordAtOffset: 6)!

BitCount: anObject
	"Set the receiver's BitCount field to the value of anObject."

	bytes wordAtOffset: 6 put: anObject!

BytesInRes
	"Answer the receiver's BytesInRes field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

BytesInRes: anObject
	"Set the receiver's BytesInRes field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

Cursor
	"Answer the receiver's Cursor field as a Smalltalk object."

	^CURSORDIR fromAddress: (bytes yourAddress)!

Cursor: anObject
	"Set the receiver's Cursor field to the value of anObject."

	anObject replaceBytesOf: bytes from: 1 to: 4 startingAt: 1!

hotspot
^self xHotSpot @ self yHotSpot!

hotspot: aPoint
self xHotSpot:  aPoint x; yHotSpot: aPoint y.
^aPoint!

Icon
	"Answer the receiver's Icon field as a Smalltalk object."

	^ICONRESDIR fromAddress: (bytes yourAddress)!

Icon: anObject
	"Set the receiver's Icon field to the value of anObject."

	anObject replaceBytesOf: bytes from: 1 to: 4 startingAt: 1!

Planes
	"Answer the receiver's Planes field as a Smalltalk object."

	^(bytes wordAtOffset: 4)!

Planes: anObject
	"Set the receiver's Planes field to the value of anObject."

	bytes wordAtOffset: 4 put: anObject!

xHotSpot
	"Answer the receiver's xHotSpot field as a Smalltalk object."

	^(bytes wordAtOffset: 4)!

xHotSpot: anObject
	"Set the receiver's xHotSpot field to the value of anObject."

	bytes wordAtOffset: 4 put: anObject!

yHotSpot
	"Answer the receiver's yHotSpot field as a Smalltalk object."

	^(bytes wordAtOffset: 6)!

yHotSpot: anObject
	"Set the receiver's yHotSpot field to the value of anObject."

	bytes wordAtOffset: 6 put: anObject! !
!RESDIR categoriesFor: #<=!public! !
!RESDIR categoriesFor: #asBINRESDIR!public! !
!RESDIR categoriesFor: #asFILERESDIR!public! !
!RESDIR categoriesFor: #BitCount!**compiled accessors**!public! !
!RESDIR categoriesFor: #BitCount:!**compiled accessors**!public! !
!RESDIR categoriesFor: #BytesInRes!**compiled accessors**!public! !
!RESDIR categoriesFor: #BytesInRes:!**compiled accessors**!public! !
!RESDIR categoriesFor: #Cursor!**compiled accessors**!public! !
!RESDIR categoriesFor: #Cursor:!**compiled accessors**!public! !
!RESDIR categoriesFor: #hotspot!public! !
!RESDIR categoriesFor: #hotspot:!public! !
!RESDIR categoriesFor: #Icon!**compiled accessors**!public! !
!RESDIR categoriesFor: #Icon:!**compiled accessors**!public! !
!RESDIR categoriesFor: #Planes!**compiled accessors**!public! !
!RESDIR categoriesFor: #Planes:!**compiled accessors**!public! !
!RESDIR categoriesFor: #xHotSpot!**compiled accessors**!public! !
!RESDIR categoriesFor: #xHotSpot:!**compiled accessors**!public! !
!RESDIR categoriesFor: #yHotSpot!**compiled accessors**!public! !
!RESDIR categoriesFor: #yHotSpot:!**compiled accessors**!public! !

!RESDIR class methodsFor!

alignment
	"Answer the natural alignment for instances of the receiver when 
	embedded in arrays or other structures. This is the natural alignment 
	of the largest field in the receiver. The actual alignment used may
	be different if the structure packing overrides it."

^2!

defineFields
	"Define the fields of the RESDIR structure.
	
	RESDIR compileDefinition
	"

	self
		defineField: #Icon
			type: (StructureField type: ICONRESDIR)
			offset: 0;
		defineField: #Cursor
			type: (StructureField type: CURSORDIR)
			offset: 0;
		defineField: #Planes
			type: WORDField new
			offset: 4; defineField: #xHotSpot
			type: WORDField new
			offset: 4;
		defineField: #BitCount
			type: WORDField new
			offset: 6; 	defineField: #yHotSpot
			type: WORDField new
			offset: 6;
		defineField: #BytesInRes
			type: DWORDField new
			offset: 8!

packing
	"Answer the default packing for instances of the receiver."

	^2! !
!RESDIR class categoriesFor: #alignment!constants!public! !
!RESDIR class categoriesFor: #defineFields!constants!public! !
!RESDIR class categoriesFor: #packing!public! !

