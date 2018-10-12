"Filed out from Dolphin Smalltalk 7"!

ArrayField subclass: #VariableArrayField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
VariableArrayField guid: (GUID fromString: '{601d61fb-1ceb-4058-ba79-4d291cf0c4af}')!
VariableArrayField comment: ''!
!VariableArrayField categoriesForClass!Unclassified! !
!VariableArrayField methodsFor!

byteSize
	"Private - Answer the byte size of the embedded array the receiver represents"

	^self length * self elementSize!

elementClass
	"Private - Answer the class of <ExternalStructure> used to represent elements of the receiver."

	^fieldClass!

length
	"Answer the length of the array at the field described
	by the receiver. We don't know until run time what the length
	is, so claim there is one element."

	^1!

lengthString
	"Private - Answer the length string to be inserted in the accessor."

	^'self ', length!

readAccessorMethodText: contentsAccessString 
	"Private - Answer suitable method text for compiling a read-accessor method
	to substitute for the receiver (does not include the selector)."

	^(super readAccessorMethodText: contentsAccessString) !

readFrom: struct
	"Private -  Answer a <StructureArray> which references the array
	of structures embedded in the <ExternalStructure>, struct, in the
	field described by the receiver.  The result may be sent #at: and #at:put:
	messages to get/set the actual elements in-place."

	^self arrayClass
		fromAddress: struct yourAddress + offset
		length: (struct perform: length)
		elementClass: self elementClass! !
!VariableArrayField categoriesFor: #byteSize!accessing!private! !
!VariableArrayField categoriesFor: #elementClass!constants!private! !
!VariableArrayField categoriesFor: #length!accessing!public! !
!VariableArrayField categoriesFor: #lengthString!constants!private! !
!VariableArrayField categoriesFor: #readAccessorMethodText:!automatic generation!private! !
!VariableArrayField categoriesFor: #readFrom:!accessing!private! !

