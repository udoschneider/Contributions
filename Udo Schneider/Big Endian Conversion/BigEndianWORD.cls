"Filed out from Dolphin Smalltalk 7"!

WORD subclass: #BigEndianWORD
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BigEndianWORD guid: (GUID fromString: '{58d59b89-860f-4708-a5a2-eb1abc35af9f}')!
BigEndianWORD comment: ''!
!BigEndianWORD categoriesForClass!Unclassified! !
!BigEndianWORD methodsFor!

asSignedInteger
	"Answer the signed 16-bit Integer value of the receiver."

	^bytes bigEndianSwordAtOffset: 0!

value
	"Answer the receiver's value field as a Smalltalk object.
	Automatically generated get method - do not modify"

	^bytes bigEndianWordAtOffset: 0!

value: anObject 
	"Set the receiver's value field to the value of anObject.
	Automatically generated set method - do not modify"

	bytes bigEndianWordAtOffset: 0 put: anObject! !
!BigEndianWORD categoriesFor: #asSignedInteger!converting!public! !
!BigEndianWORD categoriesFor: #value!**compiled accessors**!public! !
!BigEndianWORD categoriesFor: #value:!**compiled accessors**!public! !

!BigEndianWORD class methodsFor!

arrayType
	"Private - Answer the class of object (typically an <ExternalArray> subclass) to be used to 
	represent arrays of the receiver's field type in other structures."

	"^WORDArray"
^self error: 'Implement if needed'!

defineFields
	"Define the fields of the WORD 'structure'. ExternalInteger subclasses
	have a single value.
		BigEndianWORD compileDefinition
	"

	self defineField: #value type: BigEndianWORDField new!

fieldType
	"Private - Answer the <ExternalField> type to be used to represent the receiver's field type in 
	other structures."

	^BigEndianWORDField! !
!BigEndianWORD class categoriesFor: #arrayType!constants!development!private! !
!BigEndianWORD class categoriesFor: #defineFields!initializing!public! !
!BigEndianWORD class categoriesFor: #fieldType!constants!development!private! !

