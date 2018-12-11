"Filed out from Dolphin Smalltalk 7"!

IEnumXXXX subclass: #IEnumFORMATETC
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
IEnumFORMATETC guid: (IID fromString: '{7DD75F87-9857-41E6-8100-9094972ABE93}')!
IEnumFORMATETC comment: 'IEnumFORMATETC is a wrapper class for the IEnumFORMATETC COM interface, and is used by IDataObject (and thus by ShellDragDropSession).  An IEnumFORMATETC instance is a pointer to a COM enumeration object.  It is created by IDataObject>>enumFormatEtc:, which may be accessed through methods in ShellDragDropSession.  Once an IEnumFORMATETC is created, #nextAvailable can be used to enumerate the FORMATETCs of the object.'!
!IEnumFORMATETC categoriesForClass!Unclassified! !
!IEnumFORMATETC class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'Iobjidl.IEnumFORMATETC'
		FORMATETC defineTemplate"

	"This method only present to prevent auto-generation from a type library"

	!

elementClass
	"Answer the class of element enumerated by the receiver."

	^FORMATETC! !
!IEnumFORMATETC class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !
!IEnumFORMATETC class categoriesFor: #elementClass!constants!public! !

