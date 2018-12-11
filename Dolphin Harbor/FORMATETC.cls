"Filed out from Dolphin Smalltalk 7"!

OLEStructure subclass: #FORMATETC
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
FORMATETC guid: (GUID fromString: '{4f9b5d7f-2c30-4fdd-9639-e389a9dedcbf}')!
FORMATETC comment: 'FORMATETC is an <ExternalStructure> that wraps the Windows FORMATETC structure.

[Edit the following which is almost verbatim from MSDN]

The FORMATETC structure is a generalized Clipboard format. It is enhanced to encompass a target device, the aspect or view of the data, and a storage medium indicator. Where one might expect to find a Clipboard format, OLE uses a FORMATETC data structure instead. This structure is used as a parameter in OLE functions and methods that require data format information.

- cfFormat, a particular clipboard format of interest. The three types of formats recognized by OLE are 1) Standard interchange formats, such as CF_TEXT; 2) Private application formats understood only by the application offering the format, or by other applications offering similar features; and 3) OLE formats, which are used to create linked or embedded objects. 

- ptd, a pointer to a DVTARGETDEVICE structure containing information about the target device for which the data is being composed. A NULL value is used whenever the specified data format is independent of the target device or when the caller doesn''t care what device is used. In the latter case, if the data requires a target device, the object should pick an appropriate default device (often the display for visual components). Data obtained from an object with a NULL target device, such as most metafiles, is independent of the target device. The resulting data is usually the same as it would be if the user chose the Save As command from the File menu and selected an interchange format. 

- dwAspect, one (not one or more) of the DVASPECT enumeration constants that indicate how much detail should be contained in the rendering. A single clipboard format can support multiple aspects or views of the object. Most data and presentation transfer and caching methods pass aspect information. For example, a caller might request an object''s iconic picture, using the metafile clipboard format to retrieve it.

- lindex, part of the aspect when the data must be split across page boundaries. The most common value is -1, which identifies all of the data. For the aspects DVASPECT_THUMBNAIL and DVASPECT_ICON, lindex is ignored. 

- tymed, one of the TYMED enumeration constants which indicate the type of storage medium used to transfer the object''s data. Data can be transferred using whatever medium makes sense for the object. For example, data can be passed using global memory, a disk file, or structured storage objects.'!
!FORMATETC categoriesForClass!Unclassified! !
!FORMATETC methodsFor!

cfFormat
	"Answer the receiver's cfFormat field as a Smalltalk object."

	^(bytes wordAtOffset: 0)!

cfFormat: anObject
	"Set the receiver's cfFormat field to the value of anObject."

	bytes wordAtOffset: 0 put: anObject!

dwAspect
	"Answer the receiver's dwAspect field as a Smalltalk object."

	^(bytes dwordAtOffset: 8)!

dwAspect: anObject
	"Set the receiver's dwAspect field to the value of anObject."

	bytes dwordAtOffset: 8 put: anObject!

lindex
	"Answer the receiver's lindex field as a Smalltalk object."

	^(bytes sdwordAtOffset: 12)!

lindex: anObject
	"Set the receiver's lindex field to the value of anObject."

	bytes sdwordAtOffset: 12 put: anObject!

ptd
	"Answer the receiver's ptd field as a Smalltalk object."

	^(bytes dwordAtOffset: 4)!

ptd: anObject
	"Set the receiver's ptd field to the value of anObject."

	bytes dwordAtOffset: 4 put: anObject!

tymed
	"Answer the receiver's tymed field as a Smalltalk object."

	^(bytes dwordAtOffset: 16)!

tymed: anObject
	"Set the receiver's tymed field to the value of anObject."

	bytes dwordAtOffset: 16 put: anObject! !
!FORMATETC categoriesFor: #cfFormat!**compiled accessors**!public! !
!FORMATETC categoriesFor: #cfFormat:!**compiled accessors**!public! !
!FORMATETC categoriesFor: #dwAspect!**compiled accessors**!public! !
!FORMATETC categoriesFor: #dwAspect:!**compiled accessors**!public! !
!FORMATETC categoriesFor: #lindex!**compiled accessors**!public! !
!FORMATETC categoriesFor: #lindex:!**compiled accessors**!public! !
!FORMATETC categoriesFor: #ptd!**compiled accessors**!public! !
!FORMATETC categoriesFor: #ptd:!**compiled accessors**!public! !
!FORMATETC categoriesFor: #tymed!**compiled accessors**!public! !
!FORMATETC categoriesFor: #tymed:!**compiled accessors**!public! !

!FORMATETC class methodsFor!

defineFields
	"Define the fields of the Win32 FORMATETC structure.

		self compileDefinition

		typedef struct tagFORMATETC 
{ 
    CLIPFORMAT      cfFormat; 
    DVTARGETDEVICE  *ptd; 
    DWORD           dwAspect; 
    LONG            lindex; 
    DWORD           tymed; 
}FORMATETC, *LPFORMATETC;
 
	"

	self
		defineField: #cfFormat type: WORDField new;
		defineField: #ptd type: DWORDField new;
		defineField: #dwAspect type: DWORDField new;
		defineField: #lindex type: SDWORDField new;
		defineField: #tymed type: DWORDField new! !
!FORMATETC class categoriesFor: #defineFields!initializing!public! !

