"Filed out from Dolphin Smalltalk 7"!

DWORD subclass: #BigEndianDWORD
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BigEndianDWORD guid: (GUID fromString: '{8dc2d999-f973-49fe-95ad-bc9f265be609}')!
BigEndianDWORD comment: ''!
!BigEndianDWORD categoriesForClass!Unclassified! !
!BigEndianDWORD methodsFor!

asSignedInteger
	"Answer the signed 16-bit Integer value of the receiver."

	^bytes bigEndianSdwordAtOffset: 0!

value
	"Answer the receiver's value field as a Smalltalk object.
	Automatically generated get method - do not modify"

	^bytes bigEndianDwordAtOffset: 0!

value: anObject 
	"Set the receiver's value field to the value of anObject.
	Automatically generated set method - do not modify"

	bytes bigEndianDwordAtOffset: 0 put: anObject! !
!BigEndianDWORD categoriesFor: #asSignedInteger!converting!public! !
!BigEndianDWORD categoriesFor: #value!**compiled accessors**!public! !
!BigEndianDWORD categoriesFor: #value:!**compiled accessors**!public! !

!BigEndianDWORD class methodsFor!

arrayType
	"Private - Answer the class of object (typically an <ExternalArray> subclass) to be used to 
	represent arrays of the receiver's field type in other structures."

	"^WORDArray"
^self error: 'Implement if needed'!

defineFields
	"Define the fields of the WORD 'structure'. ExternalInteger subclasses
	have a single value.
		BigEndianDWORD compileDefinition
	"

	self defineField: #value type: BigEndianDWORDField new!

fieldType
	"Private - Answer the <ExternalField> type to be used to represent the receiver's field type in 
	other structures."

	^BigEndianDWORDField! !
!BigEndianDWORD class categoriesFor: #arrayType!constants!development!private! !
!BigEndianDWORD class categoriesFor: #defineFields!initializing!public! !
!BigEndianDWORD class categoriesFor: #fieldType!constants!development!private! !

