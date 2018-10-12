"Filed out from Dolphin Smalltalk 7"!

QWORD subclass: #BigEndianQWORD
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BigEndianQWORD guid: (GUID fromString: '{b93fff0e-fb3d-4201-a323-ed6af12f9c0b}')!
BigEndianQWORD comment: ''!
!BigEndianQWORD categoriesForClass!Unclassified! !
!BigEndianQWORD methodsFor!

asSignedInteger
	"Answer the signed 16-bit Integer value of the receiver."

	^bytes bigEndianSqwordAtOffset: 0!

value
	"Answer the receiver's value field as a Smalltalk object.
	Automatically generated get method - do not modify"

	^bytes bigEndianQwordAtOffset: 0!

value: anObject 
	"Set the receiver's value field to the value of anObject.
	Automatically generated set method - do not modify"

	bytes bigEndianQwordAtOffset: 0 put: anObject! !
!BigEndianQWORD categoriesFor: #asSignedInteger!converting!public! !
!BigEndianQWORD categoriesFor: #value!**compiled accessors**!public! !
!BigEndianQWORD categoriesFor: #value:!**compiled accessors**!public! !

!BigEndianQWORD class methodsFor!

arrayType
	"Private - Answer the class of object (typically an <ExternalArray> subclass) to be used to 
	represent arrays of the receiver's field type in other structures."

	"^QWORDArray"

	^self error: 'Implement if needed'!

defineFields
	"Define the fields of the WORD 'structure'. ExternalInteger subclasses
	have a single value.
		BigEndianDWORD compileDefinition
	"

	self defineField: #value type: BigEndianQWORDField new!

fieldType
	"Private - Answer the <ExternalField> type to be used to represent the receiver's field type in 
	other structures."

	^BigEndianQWORDField! !
!BigEndianQWORD class categoriesFor: #arrayType!constants!development!private! !
!BigEndianQWORD class categoriesFor: #defineFields!initializing!public! !
!BigEndianQWORD class categoriesFor: #fieldType!constants!development!private! !

